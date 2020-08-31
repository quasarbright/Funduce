{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module Funduce.Parsing.ParseSexpr where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Funduce.Parsing.ParseUtils
import Data.Functor (($>))

import Data.Functor.Foldable.TH
import Data.Functor.Foldable (cata)


data Sexpr a = SInt Integer a
             | SBool Bool a
             | SChar Char a
             | SVar String a
             | SParen [Sexpr a] a
             | SBracket [Sexpr a] a
             deriving(Eq, Ord)

makeBaseFunctor ''Sexpr

instance Show (Sexpr a) where
    show = cata $ \case
        SIntF n _ -> show n
        SCharF c _ -> "#\\"++[c]
        SBoolF True _ -> "#true"
        SBoolF False _ -> "#false"
        SVarF x _ -> x
        SParenF exprs _ -> "("++unwords exprs++")"
        SBracketF exprs _ -> "["++unwords exprs++"]"
        

parseNat :: Parser Integer
parseNat = lexeme L.decimal

parseBool :: Parser Bool
parseBool = choice [try (symbol s $> b) | (s,b) <- table]
    where
        table =
            [ ("#true", True)
            , ("#T", True)
            , ("#t", True)
            , ("#false", False)
            , ("#F", False)
            , ("#f", False)
            ]

parseChar :: Parser Char
parseChar = lexeme $ string "#\\" *> L.charLiteral

parseVar :: Parser String
parseVar = identifier

parseAtom :: Parser (Sexpr SS)
parseAtom = choice [ wrapSSApp (SChar <$> parseChar)
                   , wrapSSApp (SInt <$> parseNat)
                   , wrapSSApp (SBool <$> parseBool)
                   , wrapSSApp (SVar <$> parseVar)
                   ]

parseParens :: Parser (Sexpr SS)
parseParens = wrapSSApp $ SParen <$> parens (some pSexpr)

parseBrackets :: Parser (Sexpr SS)
parseBrackets = wrapSSApp $ SBracket <$> brackets (some pSexpr)

pSexpr :: Parser (Sexpr SS)
pSexpr = choice [parseAtom, parseParens, parseBrackets]

parseSexpr :: String -> String -> Either (ParseErrorBundle String Void) (Sexpr SS)
parseSexpr = runParser (scn *> pSexpr <* eof)

parseSexprs :: String -> String -> Either (ParseErrorBundle String Void) [Sexpr SS]
parseSexprs = runParser (scn *> many pSexpr <* eof)