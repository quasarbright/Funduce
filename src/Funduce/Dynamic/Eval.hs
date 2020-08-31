{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Funduce.Dynamic.Eval(interpretExpr, interpretDecl, interpretProgram, interpretEnv, Store, emptyStore, Env, emptyEnv, Value) where

import Funduce.Syntax.Lit
import Funduce.Syntax.Core
import Funduce.Dynamic.DynamicError

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
            deriving(Show)

-- TODO if you do garbage collection, you'll need to reify the enclosed environment
data Boxed a = Closure (Maybe String) (Cell a -> Interpreter a (Cell a))

data Value a = VInt Integer
             | VBool Bool
             | VChar Char
             | VClosure (Maybe String) (Cell a -> (Either DynamicError (Value a), Store a)) -- Might need revision

instance Show (Value a) where
    show = \case
        VInt n -> show n
        VBool True -> "#true"
        VBool False -> "#false"
        VChar c -> "#\\"++[c]
        VClosure (Just name) _ -> "<function "++name++">"
        VClosure Nothing _ -> "<function>"

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

evalExpr :: Expr a -> Interpreter a (Cell a)
evalExpr = cata $ \case
    VarF x _ -> lookupVar x
    LitF l _ -> case l of
        LInt n _ -> return $ CInt n
        LBool b _ -> return $ CBool b
        LChar c _ -> return $ CChar c
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

executeInterpreter :: Env a -> Store a -> Interpreter a r -> (Either DynamicError r, Store a)
executeInterpreter env store = runInterpreter >>> runExceptT >>> (\ m -> runRWS m env store) >>> (\(a,b,_) -> (a,b))

interpretExpr :: Env a -> Store a -> Expr a -> (Either DynamicError (Value a), Store a)
interpretExpr env store = executeInterpreter env store . (evalExpr >=> evalCell)

interpretDecl :: Env a -> Store a -> Binding a (Expr a) -> (Either DynamicError (Env a), Store a)
interpretDecl env store = executeInterpreter env store . runDecl

interpretProgram :: Env a -> Store a -> Program a -> (Either DynamicError (Env a), Store a)
interpretProgram env store = executeInterpreter env store . fmap mconcat . mapM runDecl . getProgram

interpretEnv :: Env a -> Store a -> (Either DynamicError (Map String (Value a)), Store a)
interpretEnv env store = executeInterpreter env store (ask >>= mapM evalCell)
