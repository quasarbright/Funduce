{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}

module Funduce.Parsing.ParseSexpr where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Funduce.Parsing.ParseUtils
import Data.Functor (($>))

import Data.Functor.Foldable.TH


data Sexpr a = SInt Integer a
             | SBool Bool a
             | SChar Char a
             | SVar String a
             | SParen [Sexpr a] a
             | SBracket [Sexpr a] a
             deriving(Eq, Ord, Show)

makeBaseFunctor ''Sexpr

parseNat :: Parser Integer
parseNat = lexeme L.decimal

parseBool :: Parser Bool
parseBool = choice [symbol s $> b | (s,b) <- table]
    where
        table =
            [ ("#t", True)
            , ("#T", True)
            , ("#true", True)
            , ("#f", False)
            , ("#F", False)
            , ("#false", False)
            ]

parseChar :: Parser Char
parseChar = lexeme $ string "$\\" *> L.charLiteral

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
parseSexpr = runParser pSexpr