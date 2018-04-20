module ExprPretty where

  import ExprType

  parens :: String -> String
  parens ss = "(" ++ ss ++ ")"

  instance Show a => Show (Expr a) where
    show (Add e1 e2)  = parens (show e1) ++ " + " ++ parens (show e2)
    show (Sub e1 e2)  = parens (show e1) ++ " - " ++ parens (show e2)
    show (Mult e1 e2) = parens (show e1) ++ " * " ++ parens (show e2)
    show (Div e1 e2)  = parens (show e1) ++ " / " ++ parens (show e2)
    show (E e)        = " e^" ++ parens (show e)
    show (Log a e)    = " log base " ++ (show a) ++ " " ++ parens (show e)
    show (Ln e)       = " ln" ++ parens (show e)
    show (Cos e)      = " cos" ++ parens (show e)
    show (Sin e)      = " sin" ++ parens (show e)
    show (Pow e1 e2)  = parens (show e1) ++ " ^ " ++ parens (show e2)
    show (Var x)      = x
    show (Const x)    = show x
