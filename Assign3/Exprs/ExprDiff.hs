{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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

  class ExprDiff a where
    partDiff :: String -> Expr a -> Expr a

  instance ExprDiff Double where
    partDiff v (Add e1 e2)  = Add (partDiff v e1) (partDiff v e2)
    partDiff v (Sub e1 e2)  = Sub (partDiff v e1) (partDiff v e2)
    partDiff v (Mult e1 e2) = let
      pd1 = partDiff v e1
      pd2 = partDiff v e2
      in Add (Mult pd1 e2) (Mult e1 pd2)

    partDiff v (Div (Const 1) e) = Mult (Mult (Const (-1)) (partDiff v e)) (Div (Const 1) (Pow e (Const 2)))
    partDiff v (Div e1 e2)       = let
      pd1 = partDiff v e1
      pd2 = partDiff v e2
      top = Sub (Mult pd1 e2) (Mult e1 pd2)
      bottom = Pow (e2) (Const 2)
      in (Div top bottom)

    partDiff v (E e)        = Mult (E e) (partDiff v e)
    partDiff v (Log a e)    = Mult (Div (Const 1) (Ln (Const a))) (partDiff v e)

    partDiff v (Ln e)       = Mult (partDiff v e) (Div (Const 1) e)

    partDiff v (Cos e)      = Mult (partDiff v e) (Mult (Const (-1)) (Sin e))
    partDiff v (Sin e)      = Mult (partDiff v e) (Cos e)
    partDiff v (Pow e1 e2)  = partDiff v (E (Mult e2 (Ln e1)))

    partDiff _ (Const a)    = Const 0
    partDiff v (Var b)      = if b == v then (Const 1) else (Const 0)
