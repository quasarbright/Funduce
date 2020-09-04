module Funduce.Dynamic.Type where

data Type = TInt | TBool | TChar | TFun | TCon String Int deriving(Eq, Ord, Show)