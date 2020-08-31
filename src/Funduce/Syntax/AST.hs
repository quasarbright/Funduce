{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module Funduce.Syntax.AST where

import Funduce.Syntax.Lit
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Functor.Foldable
import Data.Text.Prettyprint.Doc


data Expr a = Var String a
            | Lit (Lit a) a
            | Local [Decl a (Expr a)] (Expr a) a
            | Lambda [String] (Expr a) a
            | App (Expr a) [Expr a] a
            deriving(Eq, Ord, Show)

data Decl a b = Define String [String] b a deriving(Eq, Ord, Show, Functor, Foldable, Traversable)

makeBaseFunctor ''Expr

newtype Program a = Program {getProgram :: [Decl a (Expr a)]} deriving(Eq,Ord)

instance Pretty b => Pretty (Decl a b) where
    pretty (Define x [] rhs _) = nest 4 . vsep $ [hsep [pretty "(define", pretty x], pretty rhs <> pretty ")"]
    pretty (Define f args rhs _) = nest 4 . vsep $ [hsep [pretty "(define", tupled . fmap pretty $ (f:args)], pretty rhs <> pretty ")"]
    
    prettyList = vsep . fmap ((<> line) .pretty)

instance Pretty (Expr a) where
    pretty = para $ \case
        VarF x _ -> pretty x
        LitF l _ -> pretty $ show l
        LocalF decls body _ -> nest 4 . sep $ [ pretty "(local [" 
                                                        <+> align (vsep [pretty . fmap (fmap fst) $ decls])
                                                        <> pretty "]"
                                                      , snd body <> pretty ")"
                                                      ]
        LambdaF args body _ -> nest 4 . sep $ [ pretty "(fun (" <> hsep (pretty <$> args) <> pretty ")"
                                              , snd body <> pretty ")"
                                              ]
        AppF f [] _ -> snd f
        AppF f args _ -> parens . nest 4 . sep $ snd <$> (f:args)

instance Pretty (Program a) where
    pretty = pretty . getProgram

getTag :: ExprF p r -> p
getTag = \case
    VarF _ a -> a
    LitF _ a -> a
    LocalF _ _ a -> a
    LambdaF _ _ a -> a
    AppF _ _ a -> a

combineExprTags :: Semigroup a => ExprF a r1 -> ExprF a r2 -> a
combineExprTags a b = getTag a <> getTag b