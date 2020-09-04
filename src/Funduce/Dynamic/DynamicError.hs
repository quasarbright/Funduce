module Funduce.Dynamic.DynamicError where

import Funduce.Dynamic.Type

data DynamicError = InternalError String
                  | AppliedNonFunction
                  | DivideByZero
                  | TypeMismatch Type Type
                  deriving(Show)