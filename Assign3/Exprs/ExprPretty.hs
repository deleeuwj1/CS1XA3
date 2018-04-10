module ExprPretty where

import ExprType

parens :: String -> String
parens ss = "(" ++ ss ++ ")"

instance Show a => Show (Expr a) where
  show (Add e1 e2) = parens (show e1) ++ " !+ " ++ parens (show e2)
  show (Sub e1 e2) = parens (show e1) ++ " !- " ++ parens (show e2)
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)
  show (Div e1 e2) = parens (show e1) ++ " !/ " ++ parens (show e2)
  show (Var x) = parens $ "var " ++ "\"" ++ x ++ "\""
  show (Const x) = parens $ "value " ++ show x
