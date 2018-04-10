{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ExprEval where

  import ExprType
  import Data.Map as Map

  data EvalResult a = EvalError String | Result a
    deriving Show

  instance Functor EvalResult where
    fmap f (Result x) = Result (f x)
    fmap f (EvalError err) = EvalError err

  instance Applicative EvalResult where
    pure x = Result x
    (Result f) <*> x = fmap f x
    (EvalError err) <*> _ = EvalError err

  {- Class ExprEval
   - Differentiable Expressions
   - ------------------------------
   - Methods
     - eval- given a dictionary of variable identifiers and an Expr, will evaluate the Expr to a value
     - simplify- given a possibly incomplete dictionary of variable identifiers and an Expr, will
                evalute the Expr as much as possible and reduce
     - partDiff- takes a variable identifier and an Expr and differentiates in terms of the identifier
   - Default Methods
     - !+, !-, !*, !/ val, var: corresponed to optimized type wrappers
   - Example DSL use
     - (var "x") !+ (val 0.0 !* var "y")
       - this should become (var "x")
  -}

  class ExprEval a where
     eval :: Map.Map String a -> Expr a -> EvalResult a
     simplify :: Map.Map String a -> Expr a -> Expr a

     {- Default Methods -}
     (!+) :: Expr a -> Expr a -> Expr a
     e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
     (!-) :: Expr a -> Expr a -> Expr a
     e1 !- e2 =  simplify (Map.fromList []) $ Sub e1 e2
     (!*) :: Expr a -> Expr a -> Expr a
     e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2
     (!/) :: Expr a -> Expr a -> Expr a
     e1 !/ e2 = simplify (Map.fromList []) $ Div e1 e2
     e :: Expr a -> Expr a
     e e = simplify (Map.fromList []) $ E e
     log' :: a -> Expr a -> Expr a
     log' a e = simplify (Map.fromList []) $ Log a e
     ln :: Expr a -> Expr a
     ln e = simplify (Map.fromList []) $ Ln e
     sine :: Expr a -> Expr a
     sine e = simplify (Map.fromList []) $ Sin e
     cosine :: Expr a -> Expr a
     cosine e = simplify (Map.fromList []) $ Cos e
     (!^) :: Expr a -> Expr a -> Expr a
     e1 !^ e2 = simplify (Map.fromList []) $ Pow e1 e2
     val :: a -> Expr a
     val = Const
     var :: String -> Expr a
     var = Var
     -- this replaces eval, but is much slower than having an eval defined on it's own
     --eval vrs expr = case simplify vrs expr of
      --                 Const x -> x
        --               _       -> error "Failed to fully eval expr"

  instance ExprEval Double where -- (Num a, Fractional a) =>
   eval vrs (Add e1 e2)  = (+) <$> (eval vrs e1) <*> (eval vrs e2)
   eval vrs (Sub e1 e2)  = (-) <$> (eval vrs e1) <*> (eval vrs e2)
   eval vrs (Mult e1 e2) = (*) <$> (eval vrs e1) <*> (eval vrs e2)
   eval vrs (Div e1 e2)  = case (eval vrs e1, eval vrs e2) of
                             EvalError err -> err
                             (Result r1, Result r2) ->
                               let res = (eval vrs e1) / (eval vrs e2)
                                 in if (isNaN res) then EvalError "Zero Division Error" else  res
   eval vrs (E e)        = exp (eval vrs e)
   eval vrs (Log a e)    = case (eval vrs e) of
                            EvalError err -> EvalError err
                            Result r -> if a > 0 then Result (logBase a r) else EvalError "Base must be greater than 0"
   eval vrs (Ln e)       = case (eval vrs e) of
                            EvalError err -> EvalError err
                            Result r -> log (eval vrs e)
   eval vrs (Cos e)      = cos (eval vrs e)
   eval vrs (Sin e)      = sin (eval vrs e)
   eval vrs (Pow e1 e2)  = case (eval vrs e1, eval vrs e2) of
                             (Result r1, Result r2) ->
                                let res = r1 ** r2
                                  in if (isInfinity res) then EvalError "Invalid operands" else Result res
                             (_, EvalError err) -> EvalError err
   eval vrs (Const x) = Result x
   eval vrs (Var v) = case Map.lookup v vrs of
                        Just x -> Result x
                        Nothing -> error "Lookup failed in eval"
   simplify _ e = e -- #TODO

  isInfinity ::  Double -> Bool
  isInfinity e = let inf = show e
                   in ((inf == "Infinity") || (inf == "-Infinity"))
