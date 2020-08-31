module Funduce.Dynamic.DynamicError where

data DynamicError = InternalError String
                  | AppliedNonFunction
                  deriving(Show)