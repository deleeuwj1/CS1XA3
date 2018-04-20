
{-|
Module      : ExprTest
Description : Contains methods that check the functionality and accuracy of the library
Copyright   : (c) Jessica de Leeuw @2018
License     : MIT
Maintainer  : deleeuwj@mcmaster.ca
Stability   : experimental
Portability : POSIX
-}

module ExprTest where

import Data.Map as Map

import ExprType
import ExprDiff
import ExprEval
import ExprParser
import ExprPretty

import Test.QuickCheck

exprProp xs = let
                todo = error "#TODO"
              in
                todo

listToExpr :: [Double] -> Expr Double
listToExpr xs = error "#TODO"


sampleExpr1 = "x+y*2"
