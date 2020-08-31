{-# LANGUAGE LambdaCase #-}

module Funduce.Syntax.Lit where

data Lit a = LInt Int a
           | LBool Bool a
           | LChar Char a
           deriving (Eq, Ord)

instance Show (Lit a) where
    show = \case
        LInt n _ -> show n
        LBool True _ -> "#true"
        LBool False _ -> "#false"
        LChar c _ -> show c