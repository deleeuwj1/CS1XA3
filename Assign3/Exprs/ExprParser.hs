module ExprParser (parseExprFloat, parseExprDouble) where

import Text.Parsec
import Text.Parsec.String

import ExprType

{-|
Module: ExprParser
Description: Contains parsers for doubles and floats.
Copyright: (c) [2018] [Jessica de Leeuw]
Licence MIT Licence
Maintainer:

 -}


{-
- We only will want to show the top level function "parseExprDouble", "parseExprFloat", etc.
- to do this we put this (parseExprFloat, , etc.) in the module declaration
-}

parseExprDouble :: String -- ^ the string to be parsed
                -> Expr Double -- ^ the resulting expression
parseExprDouble ss = case parse exprD "" ss of
                        Left err -> error $ "Parse Error " ++ show err
                        Right val -> val

exprD :: Parser (Expr Double)
exprD = error "TODO!"

parseExprFloat :: String -> Expr Float
parseExprFloat ss = case parse exprF "" ss of
                      Left err -> error $ "Parse Error " ++ show err
                      Right val -> val

exprF :: Parser (Expr Float)
exprF = error "TODO!"
