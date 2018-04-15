{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ExprDiff where

  import ExprType
  import ExprEval
  import Data.Map as Map

  class ExprDiff a where
    partDiff :: String -> Expr a -> Expr a

  instance ExprDiff Double where
    partDiff v (Add e1 e2)     = Add (partDiff v e1) (partDiff v e2)
    partDiff v (Sub e1 e2)     = Sub (partDiff v e1) (partDiff v e2)
    partDiff v (Mult e1 e2)     = Mult (partDiff v e1) (partDiff v e2)

    partDiff v (Div e1 e2)  = Div (partDiff v e1) (partDiff v e2)
    partDiff v (E e)        = E (partDiff v e)
    partDiff v (Log a e)    = Log a (partDiff v e)
    partDiff v (Ln (Var x)) = if x == v then (Div (Const 1) (Var x)) else (Const 0)
    partDiff v (Ln e)       = Mult (Div (Const 1) e) (partDiff v e)

    partDiff v (Cos e)      = Cos (partDiff v e)
    partDiff v (Sin e)      = Sin (partDiff v e)
    partDiff v (Pow e1 e2)  = Pow (partDiff v e1) (partDiff v e2)
    partDiff _ (Const x)    = Const 0
    partDiff v (Var b)      = if b == v then (Const 1) else (Const 0)


  {-
  class DiffExpr repr where
    eval :: (Num a) => Map.Map String a -> repr a -> a
    simplify :: (Num a) => Map.Map String a -> repr a -> repr a
    partDiff :: (Num a) => String -> repr a -> repr a

    (!+) :: (Num a) => repr a -> repr a -> repr a
    (!-) :: (Num a) => repr a -> repr a -> repr a
    (!*) :: (Num a) => repr a -> repr a -> repr a
    (!/) :: (Num a) => repr a -> repr a -> repr a
  -}
