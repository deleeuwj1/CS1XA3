{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}

{-|
Module      : ExprDiff
Description : Contains methods for differentiable expressions
Copyright   : (c) Jessica de Leeuw @2018
License     : MIT
Maintainer  : deleeuwj@mcmaster.ca
Stability   : experimental
Portability : POSIX
-}

module ExprDiff where

  import ExprType
  import ExprEval
  import ExprPretty
  import Data.Map as Map

  -- * Class Declaration
  -- | ExprDiff has the method `partDiff`, which symbolically partially differentiates an expression
  class ExprDiff a where
    partDiff :: String -> Expr a -> Expr a

  instance ExprDiff Double where
    partDiff v (Add e1 e2)  = (partDiff v e1) !+ (partDiff v e2)
    partDiff v (Sub e1 e2)  = (partDiff v e1) !- (partDiff v e2)
    partDiff v (Mult e1 e2) = let -- uses product rule to differentiate
      pd1 = partDiff v e1
      pd2 = partDiff v e2
      in (pd1 !* e2) !+ (e1 !* pd2)

    partDiff v (Div (Const 1) e) = ((val (-1)) !* (partDiff v e)) !* ((val 1) !/ (e !^ (val 2))) -- simpler form of quotient rule
    partDiff v (Div e1 e2)       = let -- uses quotient rule to differentiate
      pd1 = partDiff v e1
      pd2 = partDiff v e2
      top = (pd1 !* e2) !- (e1 !* pd2)
      bottom = (e2) !^ (val 2)
      in (top !/ bottom)

    partDiff v (E e)        = (nPow(e)) !* (partDiff v e) -- uses chain rule to differentiate
    partDiff v (Log a e)    = ((val 1) !/ (ln (val a))) !* (partDiff v e) -- uses derivative of logarithm rules

    partDiff v (Ln e)       = (partDiff v e) !* ((val 1) !/ e) -- uses derivative of ln rules

    partDiff v (Cos e)      = (partDiff v e) !* ((val (-1)) !* (sine e)) -- uses derivative of trigonometric functions rules
    partDiff v (Sin e)      = (partDiff v e) !* (cosine e)
    partDiff v (Pow e1 e2)  = partDiff v (nPow (e2 !* (ln e1))) -- uses chain rule to differentiate

    partDiff _ (Const a)    = val 0
    partDiff v (Var b)      = if b == v then (val 1) else (val 0)
