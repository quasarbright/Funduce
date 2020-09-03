{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Funduce.Syntax.Core where

import Funduce.Syntax.Lit
import Funduce.Syntax.Prim

import Data.Functor.Foldable
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Data.List (intercalate)
import Control.Arrow ((>>>))
import Data.Functor.Foldable.TH (makeBaseFunctor)

data Expr a = Var String a
            | Lit (Lit a) a
            | Prim2 Prim2 (Expr a) (Expr a)
            | Let (Binding a (Expr a)) (Expr a) a
            | Lambda String (Expr a) a
            | App (Expr a) (Expr a) a
            | If (Expr a) (Expr a) (Expr a) a
            deriving (Eq, Ord)

data Binding a b = NonRec String b a
                 | Rec [(String, b, a)] a
                 deriving(Eq, Ord, Functor, Foldable, Traversable)

type Decl a = Binding a (Expr a)

makeBaseFunctor ''Expr

newtype Program a = Program {getProgram :: [Decl a]}

instance Show (Binding a String) where
    show = \case
        NonRec x rhs _ -> unwords[x,"=",rhs]
        Rec bindings _ -> intercalate " and " [unwords [x,"=", rhs] | (x,rhs,_) <- bindings]

instance Show (Expr a) where
    show = cata $ \case
        VarF x _ -> x
        LitF l _ -> show l
        Prim2F p a b -> concat["(PRIM ",show p," ",a," ",b,")"]
        LetF binding body _ -> concat["(let ",show binding," in ",body,")"]
        LambdaF x body _ -> concat["(fun ",x," -> ",body,")"]
        AppF f x _ -> concat ["(",f," ",x,")"]
        IfF cnd thn els _ -> concat["(if ",cnd," ",thn," ",els,")"]

instance Pretty b => Pretty (Binding a b) where
    pretty = \case
        NonRec x rhs _ -> hsep [pretty x, pretty "=", pretty rhs]
        Rec bindings _ -> align . encloseSep mempty mempty (softline <> pretty "and ") 
                          $ [hsep[pretty x,pretty "=",pretty rhs] | (x,rhs,_) <- bindings]
    prettyList decls = encloseSep mempty mempty line (pretty <$> decls)

instance Pretty (Expr a) where
    pretty = para $ \case
        VarF x _ -> pretty x
        LitF l _ -> pretty (show l)
        Prim2F p (_,a) (_,b) -> parens (nest 4 . sep $ [hsep [pretty "PRIM", pretty (show p)], a, b]) 
        LetF binding (_,body) _ -> align . sep $ [hsep [pretty "(let",pretty (fst <$> binding),pretty "in"], body <> pretty ")"]
        LambdaF x (_,body) _ -> nest 4 . sep $ [hsep [pretty "(fun",pretty x,pretty "->"],body <> pretty ")"] 
        AppF (_,f) (_,x) _ -> parens (nest 4 . sep $ [f,x])
        IfF (_,cnd) (_,thn) (_,els) _ -> parens . nest 4 . sep $ [pretty "if", cnd, thn, els]

instance Pretty (Program a) where
    pretty = pretty . getProgram

instance Show (Decl a) where
    show = renderPretty

instance Show (Program a) where
    show = renderPretty

renderPretty :: Pretty a => a -> String
renderPretty = pretty >>> layoutPretty defaultLayoutOptions >>> renderString

renderExpr :: Expr a -> String
renderExpr = renderPretty

renderDecls :: [Decl a] -> String
renderDecls = renderPretty

renderProg :: Program a -> String
renderProg = renderPretty