module Funduce.Syntax.Prim where

data Prim2 = Plus
           | Minus
           | Times
           | Divide
           | Eq
           | Neq
           | Lt
           | Gt
           | Lte
           | Gte
           | And
           | Or
          deriving(Eq, Ord, Show)