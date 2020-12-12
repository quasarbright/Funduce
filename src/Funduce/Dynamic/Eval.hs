{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Funduce.Dynamic.Eval(interpretExpr, interpretDecl, interpretProgram, interpretEnv, Store, emptyStore, Env, emptyEnv, getInitialState, Value) where

import Funduce.Syntax.Lit
import Funduce.Syntax.Prim
import Funduce.Syntax.Core
import Funduce.Dynamic.DynamicError
import Funduce.Dynamic.WiredIns
import Funduce.Dynamic.Type

import Control.Monad.RWS.Strict
import Control.Monad.Except

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.Identity (Identity)
import Data.Functor.Foldable
import Control.Arrow

data Cell a = CInt Integer
            | CBool Bool
            | CChar Char
            | CPointer Address
            deriving(Eq, Ord, Show)

-- TODO if you do garbage collection, you'll need to reify the enclosed environment. (maybe just do that instead of using a function)
data Boxed a = Closure (Maybe String) (Cell a -> Interpreter a (Cell a))
             | Object [(String, Cell a)] Type

data Value a = VInt Integer
             | VBool Bool
             | VChar Char
             | VClosure (Maybe String) (Cell a -> (Either DynamicError (Value a), Store a)) -- Might need revision
             | VObject [(String, Value a)] Type

instance Show (Value a) where
    show = \case
        VInt n -> show n
        VBool True -> "#true"
        VBool False -> "#false"
        VChar c -> "#\\"++[c]
        VClosure (Just name) _ -> "<function "++name++">"
        VClosure Nothing _ -> "<function>"
        VObject props (TCon  name _) -> name ++ show props
        VObject _ t -> error ("object with non-con type: "++show t)

type Address = Integer

-- Heap
type Store a = Map Address (Boxed a)

emptyStore :: Store a
emptyStore = mempty

-- Stack
type Env a = Map String (Cell a)

emptyEnv :: Env a
emptyEnv = mempty

-- TODO maintain call stack for errors
-- TODO maintain current location
newtype Interpreter a r = Interpreter {runInterpreter
 :: ExceptT DynamicError (RWST (Env a) () (Store a) Identity) r
 } deriving( Functor
           , Applicative
           , Monad
           , MonadError DynamicError
           , MonadReader (Env a)
           , MonadState (Store a))

extend :: (Ord k, MonadReader (Map k v) m) => k -> v -> m a -> m a
extend x v = local (Map.insert x v)

lookupVar :: String -> Interpreter a (Cell a)
lookupVar x = do
    mV <- asks (Map.lookup x)
    maybe (throwError . InternalError $ "unbound variable "++x) return mV

freshAddress :: Interpreter a Address
freshAddress = gets $ Map.keys >>> fmap Just >>> foldr max Nothing >>> maybe 0 succ

readAddress :: Address -> Interpreter a (Boxed a)
readAddress address = do
    mBoxed <- gets (Map.lookup address)
    maybe (throwError . InternalError $ "unbound pointer") return mBoxed

writeAddress :: (MonadState (Map k a) m, Ord k) => k -> a -> m ()
writeAddress address boxed = modify (Map.insert address boxed)

makeBoxed :: Boxed a -> Interpreter a Address
makeBoxed boxed = do
    address <- freshAddress
    writeAddress address boxed
    return address

inferCell :: Cell a -> Interpreter a Type
inferCell = \case
    CInt{} -> return TInt
    CBool{} -> return TBool
    CChar{} -> return TChar
    CPointer address -> do
        boxed <- readAddress address
        case boxed of
            Closure{} -> return TFun
            Object _ t -> return t

checkCell :: Cell a -> Type -> Interpreter a ()
checkCell cell expected = do
    actual <- inferCell cell
    unless (expected == actual) (throwError (TypeMismatch expected actual))

wrapArith :: (Integer -> Integer -> Integer) -> Cell a -> Cell a -> Interpreter a (Cell a)
wrapArith op a b = do
    checkCell a TInt
    checkCell b TInt
    case (a,b) of
       (CInt a', CInt b') -> return (CInt (a' `op` b'))
       _ -> throwError (InternalError "type checker should have caught this")

wrapCmp :: (Cell a1 -> Cell a1 -> Bool) -> Cell a1 -> Cell a1 -> Interpreter a1 (Cell a2)
wrapCmp cmp a b = do
    ta <- inferCell a
    checkCell b ta
    return (CBool (cmp a b)) -- TODO actual cmp for structs

wrapLogic :: (Bool -> Bool -> Bool) -> Cell a -> Cell a -> Interpreter a (Cell a)
wrapLogic op a b = do
    checkCell a TBool
    checkCell b TBool
    case (a,b) of
        (CBool a', CBool b') -> return (CBool (op a' b'))
        _ -> throwError (InternalError "type checker should have caught this")

prim2Help :: Prim2 -> Cell a -> Cell a -> Interpreter a (Cell a)
prim2Help p a b = case p of
    Plus -> wrapArith (+) a b
    Minus -> wrapArith (-) a b
    Times -> wrapArith (*) a b
    Divide | b == CInt 0 -> throwError DivideByZero
           | otherwise -> wrapArith div a b
    Eq -> wrapCmp (==) a b
    Neq -> wrapCmp (/=) a b
    Lt -> wrapCmp (<) a b
    Gt -> wrapCmp (>) a b
    Lte -> wrapCmp (<=) a b
    Gte -> wrapCmp (>=) a b
    And -> wrapLogic (&&) a b
    Or -> wrapLogic (||) a b

evalExpr :: Expr a -> Interpreter a (Cell a)
evalExpr = cata $ \case
    VarF x _ -> lookupVar x
    LitF l _ -> case l of
        LInt n _ -> return $ CInt n
        LBool b _ -> return $ CBool b
        LChar c _ -> return $ CChar c
    Prim2F p a b -> join (prim2Help p <$> a <*> b)
    LetF decl body _ -> do
        env <- runBinding id decl
        local (const env) body
    LambdaF x body _ -> do
        env <- ask
        let fun cell = local (const $ Map.insert x cell env) body
        CPointer <$> makeBoxed (Closure Nothing fun)
    AppF f_ x _ -> do
        f <- f_
        case f of
            CPointer fAddress -> do
                fBoxed <- readAddress fAddress
                case fBoxed of
                    Closure _ fun -> do
                        fun =<< x
                    _ -> throwError AppliedNonFunction
            _ -> throwError AppliedNonFunction
    IfF cnd thn els _ -> do
        cnd' <- cnd
        checkCell cnd' TBool
        case cnd' of
            CBool b -> if b then thn else els
            _ -> throwError (InternalError "should be impossible")
    TypeTestF e t _ -> do
        e' <- e
        t' <- inferCell e'
        return (CBool (t == t'))
    MakeObjF name idx vals _ -> do
        vals' <- mapM (\(field, e) -> do
            c <- e
            return (field, c)) vals
        CPointer <$> makeBoxed (Object vals' (TCon name idx))
    AccessObjF name idx field obj _ -> do
        obj' <- obj
        case obj' of
            CPointer addr -> do
                boxed <- readAddress addr
                case boxed of
                    Object vals (TCon actualName actualIdx)
                        | (actualName, actualIdx) /= (name, idx) -> throwError (TypeMismatch (TCon name idx) (TCon actualName actualIdx))
                        | otherwise ->
                            maybe (throwError (InternalError "bad field access")) return (lookup field vals)
                    _ -> throwError (InternalError "addr led to non object")
            _ -> do
                t <- inferCell obj'
                throwError (TypeMismatch (TCon name idx) t)
        
        


runBinding :: (b -> Interpreter a (Cell a)) -> Binding a b -> Interpreter a (Env a)
runBinding evalRHS (NonRec x rhs _) = do
    rhs' <- evalRHS rhs
    extend x rhs' ask
runBinding evalRHS (Rec bindings _) = do
    -- allocate heap space for bound values (all bound values should be heap-things)
    addresses <- mapM (const freshAddress) bindings
    let (xs,rhss) = unzip $ fmap (\(a,b,_) -> (a,b)) bindings
    let extensions = zipWith (curry $ second CPointer) xs addresses
    let extend' exts m = foldr (uncurry extend) m exts
    rhss' <- mapM (extend' extensions. evalRHS) rhss
    -- fills in holes in the heap and returns the new bindings
    let go (x,(address,CPointer address')) = do
             boxed <- readAddress address'
             writeAddress address boxed
             return [(x,CPointer address)]
        go _ = throwError . InternalError $ "unboxed rhs in letrec"
    finalExtensions <- concat <$> mapM go (zip xs (zip addresses rhss'))
    extend' finalExtensions ask

runBinding _ (DefineStruct name fields a) = foldr go ask decls
    where
        arity = length fields
        tObj = TCon name arity -- TODO unique id
        decls =
            [ wireTypeTest (name++"?") tObj a
            , NonRec ("make-"++name) makeFun a
            ] ++ accessors
        makeFun = foldr (\field body -> Lambda field body a) makeBody fields
        makeBody = MakeObj name arity [(field, Var field a) | field <- fields] a
        accessors = [NonRec (name++"-"++field) (accessor field) a | field <- fields]
        accessor field = Lambda field (AccessObj name arity field (Var field a) a) a
        go decl m = do
            env' <- runDecl decl
            local (const env') m

----runDecl (Rec [(x,rhs,_)] _) = do
----    address <- freshAddress
----    rhs' <- extend x (CPointer address) (evalExpr rhs)
----    case rhs' of
----        CPointer address' -> do
----            boxed <- readAddress address'
----            writeAddress address boxed
----            extend x (CPointer address) ask
----        _ -> ask -- this really shouldn't happen

runDecl :: Binding a (Expr a) -> Interpreter a (Env a)
runDecl = runBinding evalExpr

runProgram :: Program a -> Interpreter a (Env a)
runProgram (Program decls) = foldr go ask decls
    where go decl m = do
              env' <- runDecl decl
              local (const env') m

getInitialState :: a -> (Env a, Store a)
getInitialState a = case executeInterpreter emptyEnv emptyStore $ runProgram (Program (wiredIns a)) of
    (Left _,_) -> error "prelude failed?"
    (Right env,store) -> (env,store) 

evalCell :: Cell a -> Interpreter a (Value a)
evalCell = \case
    CInt n -> return $ VInt n
    CBool b -> return $ VBool b
    CChar c -> return $ VChar c
    CPointer address -> do
        boxed <- readAddress address
        case boxed of
            Closure mName fun -> do
                env <- ask
                store <- get
                return $ VClosure mName (\cell -> executeInterpreter env store (evalCell =<< fun cell))
            Object props t -> VObject <$> mapM (\(n,c) -> (,) n <$> evalCell c) props <*> pure t
            

executeInterpreter :: Env a -> Store a -> Interpreter a r -> (Either DynamicError r, Store a)
executeInterpreter env store = runInterpreter >>> runExceptT >>> (\ m -> runRWS m env store) >>> (\(a,b,_) -> (a,b))

interpretExpr :: Env a -> Store a -> Expr a -> (Either DynamicError (Value a), Store a)
interpretExpr env store = executeInterpreter env store . (evalExpr >=> evalCell)

interpretDecl :: Env a -> Store a -> Binding a (Expr a) -> (Either DynamicError (Env a), Store a)
interpretDecl env store = executeInterpreter env store . runDecl

interpretProgram :: Env a -> Store a -> Program a -> (Either DynamicError (Env a), Store a)
interpretProgram env store = executeInterpreter env store . runProgram

interpretEnv :: Env a -> Store a -> (Either DynamicError (Map String (Value a)), Store a)
interpretEnv env store = executeInterpreter env store (ask >>= mapM evalCell)
