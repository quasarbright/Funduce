{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Funduce.Parsing.ConvertSexpr(convertDecl, convertProgram, convertExpr) where

import Funduce.Parsing.ParseSexpr
import Funduce.Syntax.AST
import Funduce.Syntax.Lit

import Data.Functor.Foldable
import Control.Monad.Except

data ConversionError a = Msg String a deriving(Show)
--                       | Generic a

type Converter a r = Except (ConversionError a) r

convertVars :: a -> [Sexpr a] -> Converter a [String]
convertVars _ [] = return []
convertVars a (SVar x _:rest) = (:) x <$> convertVars a rest
convertVars a _ = throwError (Msg "expected identifiers" a)

convertExpr :: Sexpr a -> Converter a (Expr a)
convertExpr = para $ \case
    SIntF n a -> return $ Lit (LInt n a) a
    SBoolF b a -> return $ Lit (LBool b a) a
    SCharF c a -> return $ Lit (LChar c a) a
    SVarF x a -> return $ Var x a
    SParenF [(SVar "fun" _, _), (SParen vars va, _), (_, body)] a ->
        Lambda <$> convertVars va vars <*> body <*> pure a
    SParenF ((SVar "fun" _,_):_) a -> throwError (Msg "invalid function expression" a)
    SParenF [(SVar "local" _,_), (SBracket decls _,_), (_,body)] a ->
        Local <$> mapM convertDecl decls <*> body <*> pure a
    SParenF ((SVar "local" _,_):_) a -> throwError (Msg "invalid local expression" a)
    SParenF [(SVar "if" _,_), (_,cnd), (_,thn), (_,els)] a ->
        If <$> cnd <*> thn <*> els <*> pure a
    SParenF ((SVar "if" _,_):_) a -> throwError (Msg "invalid if expression" a)
    SParenF [(_,e)] _ -> e
    SParenF ((_,f):args) a -> App <$> f <*> sequence (snd <$> args) <*> pure a
    SParenF [] a -> throwError (Msg "unexpected empty parens" a)
    SBracketF _ a -> throwError (Msg "invalid expression" a)

convertDecl :: Sexpr a -> Converter a (Decl a (Expr a))
convertDecl = let invalid a = throwError (Msg "expected a definition like (define x 1) or (define-struct pos [x y])" a) in \case
    SParen [SVar "define" _, SVar x _, rhs] a -> Define x [] <$> convertExpr rhs <*> pure a
    SParen [SVar "define" _, SParen (SVar f _:vars) va, rhs] a ->
        Define f <$> convertVars va vars <*> convertExpr rhs <*> pure a
    SParen [SVar "define-struct" _, SVar name _, SBracket vars va] a -> DefineStruct name <$> convertVars va vars <*> pure a
    SParen _ a -> invalid a
    SInt _ a -> invalid a
    SChar _ a -> invalid a
    SBool _ a -> invalid a
    SBracket _ a -> invalid a
    SVar _ a -> invalid a

convertProgram :: [Sexpr a] -> Converter a (Program a)
convertProgram sexprs = Program <$> mapM convertDecl sexprs
