{-|
Module      : ExprPretty
Description : Contains methods that take `Expr` expressions and print them in a aesthetic format
Copyright   : (c) Jessica de Leeuw @2018
License     : MIT
Maintainer  : deleeuwj@mcmaster.ca
Stability   : experimental
Portability : POSIX
-}

module ExprPretty where

  import ExprType

  -- | Wraps the expressions in parenthesis
  parenthesis :: String -> String
  parenthesis ss = "(" ++ ss ++ ")"

  -- | Formats each operation by printing the operator associated with it
  instance Show a => Show (Expr a) where
    show (Add e1 e2)  = parenthesis $ (show e1) ++ " + " ++ (show e2)
    show (Sub e1 e2)  = parenthesis $ (show e1) ++ " - " ++ (show e2)
    show (Mult e1 e2) = parenthesis $ (show e1) ++ " * " ++ (show e2)
    show (Div e1 e2)  = parenthesis $ (show e1) ++ " / " ++ (show e2)
    show (E e)        = "e^" ++ parenthesis (show e)
    show (Log a e)    = "log base " ++ (show a) ++ " " ++ parenthesis (show e)
    show (Ln e)       = "ln" ++ parenthesis (show e)
    show (Cos e)      = "cos" ++ parenthesis (show e)
    show (Sin e)      = "sin" ++ parenthesis (show e)
    show (Pow e1 e2)  = parenthesis $ (show e1) ++ " ^ " ++ (show e2)
    show (Var x)      = x
    show (Const x)    = show x
