module Funduce.Dynamic.DynamicError where

data DynamicError = InternalError String
                  | AppliedNonFunction
                  | DivideByZero
                  | TypeMismatch Type Type
                  deriving(Show)

data Type = TInt | TBool | TChar | TFun deriving(Eq, Ord, Show)