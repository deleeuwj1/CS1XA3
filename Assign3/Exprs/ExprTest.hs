module ExprTest where

import Data.Map as Map

import ExprType
import ExprDiff
import ExprEval
import ExprParser
import ExprShow

import Test.QuickCheck

exprProp xs = let
                todo = error "#TODO"
              in
                todo

listToExpr :: [Double] -> Expr Double
listToExpr xs = error "#TODO"


sampleExpr1 = (var "x") !+ (var "y") !* (val 2.0)
