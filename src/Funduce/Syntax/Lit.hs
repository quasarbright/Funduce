{-# LANGUAGE LambdaCase #-}

module Funduce.Syntax.Lit where

data Lit a = LInt Int a
           | LBool Bool a
           | LChar Char a
           deriving (Eq, Ord)

instance Show (Lit a) where
    show = \case
        LInt n _ -> show n
        LBool b _ -> show b
        LChar c _ -> show c