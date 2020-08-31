{-# LANGUAGE LambdaCase #-}

module Funduce.Static.Desugar where

import qualified Funduce.Syntax.Core as C
import qualified Funduce.Syntax.AST as A

import Data.Functor.Foldable
import Control.Arrow ((>>>))


curryHelp :: Foldable t => t String -> C.Expr a -> a -> C.Expr a
curryHelp args body a = foldr go body args
    where go x body' = C.Lambda x body' a

desugarExpr :: A.Expr a -> C.Expr a
desugarExpr = cata $ \case
    A.VarF x a -> C.Var x a
    A.LitF l a -> C.Lit l a
    A.LocalF decls body a' -> -- TODO scc dependency analysis for mutual recursion
        foldr go body decls
        where go decl body' =
                  C.Let (desugarDeclWith id decl) body' a'
    A.LambdaF args body a -> curryHelp args body a
    A.AppF fun args a -> foldr go fun args
        where go f x = C.App f x a

desugarDecl :: A.Decl a (A.Expr a) -> C.Decl a
desugarDecl = desugarDeclWith desugarExpr

-- | No dependency analysis. Only makes function definitions self-recursive, no mutual recursion
desugarDeclWith :: (e -> C.Expr a) -> A.Decl a e -> C.Decl a
desugarDeclWith convert (A.Define x [] rhs a) = C.NonRec x (convert rhs) a
desugarDeclWith convert (A.Define f args rhs a) = C.Rec [(f,curryHelp args (convert rhs) a, a)] a

desugarProgram :: A.Program a -> C.Program a
desugarProgram = A.getProgram >>> fmap desugarDecl >>> C.Program
