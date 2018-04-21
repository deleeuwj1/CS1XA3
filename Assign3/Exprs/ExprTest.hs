{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}

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
  import Generic.Random.Generic
  import GHC.Generics

  instance Arbitrary (Expr Double) where
    arbitrary = genericArbitraryRec uniform `withBaseCase` return (Const 1)

  {-
  Simplify is idempotent
  simplify e == simplify (simplify e)
  -}
  -- | Simplifying an expression once should be the same result as simplifying it multiple times
  simplifyProp1 :: Expr Double -> Bool
  simplifyProp1 e = simplify vrs e == simplify vrs (simplify vrs e)
    where vrs = Map.fromList []

  -- | If the variable is not in the list of vars, it should be returned as a variable
  simplifyProp2 :: Double -> Double -> String -> Bool
  simplifyProp2 a b s = simplify (Map.fromList [("x", a),("y", b)]) (Var s) == Var s

  -- | An expression that is of the form Add (Const a) (Add (Const b) (Add (etc.) (etc.))) should simplify to a number
  simplifyProp3 :: [Integer] -> Bool
  simplifyProp3 [] = True
  simplifyProp3 xs = simplify (Map.fromList []) (listToExpr xs) == Const (sum xs)

  listToExpr :: [Integer] -> Expr Integer
  listToExpr [x]    = Const x
  listToExpr (x:xs) = Add (Const x) (listToExpr xs)
  listToExpr []     = error "Cannot create an expression from an empty list"

  sampleExpr1 = "x+y*2"
