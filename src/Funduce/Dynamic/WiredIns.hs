module Funduce.Dynamic.WiredIns where

import Funduce.Syntax.Core
import Funduce.Syntax.Prim
import Funduce.Dynamic.Type

wirePrim2 :: String -> Prim2 -> a -> Binding a (Expr a)
wirePrim2 name prim2 a = NonRec name (Lambda "a" (Lambda "b" (Prim2 prim2 (Var "a" a) (Var "b" a)) a) a) a 

wireTypeTest :: String -> Type -> a -> Binding a (Expr a)
wireTypeTest name t a = NonRec name (Lambda "x" (TypeTest (Var "x" a) t a) a) a

wiredIns :: a -> [Decl a]
wiredIns a = [wirePrim2 name prim2 a | (name,prim2) <- primTable] ++ [wireTypeTest name t a | (name,t) <- typeTable]
    where primTable =
            [ ("+", Plus)
            , ("-", Minus)
            , ("*", Times)
            , ("/", Divide)
            , ("eq?", Eq)
            , ("neq?", Neq)
            , ("<", Lt)
            , (">", Gt)
            , ("<=", Lte)
            , (">=", Gte)
            , ("and", And)
            , ("or", Or)
            ]
          typeTable =
            [ ("int?", TInt)
            , ("bool?", TBool)
            , ("char?", TChar)
            ]
