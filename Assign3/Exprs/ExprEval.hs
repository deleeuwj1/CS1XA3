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

  instance ExprEval Double where -- (Num a, Fractional a) =>
   eval vrs (Add e1 e2)  = (+) <$> (eval vrs e1) <*> (eval vrs e2)
   eval vrs (Sub e1 e2)  = (-) <$> (eval vrs e1) <*> (eval vrs e2)
   eval vrs (Mult e1 e2) = (*) <$> (eval vrs e1) <*> (eval vrs e2)
   eval vrs (Div e1 e2)  = case (eval vrs e1, eval vrs e2) of
                             (_, EvalError err) -> EvalError err
                             (Result r1, Result r2) ->
                               let res = r1 / r2
                                 in if (isNaN res) then EvalError "Zero Division Error" else Result res
   eval vrs (E e)        = case (eval vrs e) of
                             EvalError err -> EvalError err
                             Result r -> Result (exp r)
   eval vrs (Log a e)    = case (eval vrs e) of
                            EvalError err -> EvalError err
                            Result r -> if a > 0 then Result (logBase a r) else EvalError "Base must be greater than 0"
   eval vrs (Ln e)       = case (eval vrs e) of
                            EvalError err -> EvalError err
                            Result r -> Result (log r)
   eval vrs (Cos e)      = case (eval vrs e) of
                            EvalError err -> EvalError err
                            Result r -> Result (cos r)
   eval vrs (Sin e)      = case (eval vrs e) of
                            EvalError err -> EvalError err
                            Result r -> Result (sin r)
   eval vrs (Pow e1 e2)  = case (eval vrs e1, eval vrs e2) of
                             (Result r1, Result r2) ->
                                let res = r1 ** r2
                                  in if (isInfinity res) then EvalError "Invalid operands" else Result res
                             (_, EvalError err) -> EvalError err
   eval vrs (Const x)    = Result x
   eval vrs (Var v)      = case Map.lookup v vrs of
                             Just x -> Result x
                             Nothing -> error "Lookup failed in eval"

   {- simplify expressions -}
   --addition
   simplify vrs (Add e (Var x)) = case Map.lookup x vrs of
                                    Just v  -> (Add (simplify vrs e) (Const v))
                                    Nothing -> (Add (simplify vrs e) (Var x))
   simplify vrs (Add (Var x) e) = simplify vrs (Add e (Var x))
   simplify vrs (Add e1 e2)     = Add (simplify vrs e1) (simplify vrs e2)

   --subtraction
   simplify vrs (Sub e (Var x)) = case Map.lookup x vrs of
                                    Just v  -> (Sub (simplify vrs e) (Const v))
                                    Nothing -> (Sub (simplify vrs e) (Var x))
   simplify vrs (Sub (Var x) e) = case Map.lookup x vrs of
                                    Just v  -> (Sub (Const v) (simplify vrs e))
                                    Nothing -> (Sub (Var x) (simplify vrs e))
   simplify vrs (Sub e1 e2)     = Sub (simplify vrs e1) (simplify vrs e2)

   -- multiplication
   simplify vrs (Mult e (Var x)) = case Map.lookup x vrs of
                                    Just v  -> (Mult (simplify vrs e) (Const v))
                                    Nothing -> (Mult (simplify vrs e) (Var x))
   simplify vrs (Mult (Var x) e) = simplify vrs (Mult e (Var x))
   simplify vrs (Mult e1 e2)     = Mult (simplify vrs e1) (simplify vrs e2)

   -- division
   simplify vrs (Div e (Var x))       = case Map.lookup x vrs of
                                          Just v  -> (Div (simplify vrs e) (Const v))
                                          Nothing -> (Div (simplify vrs e) (Var x))
   simplify vrs (Div (Var x) e)       = case Map.lookup x vrs of
                                          Just v  -> (Div (Const v) (simplify vrs e))
                                          Nothing -> (Div (Var x) (simplify vrs e))
   {-
   simplify vrs (Div (Var a) (Var b)) = case (Map.lookup a vrs, Map.lookup b vrs) of
                                          (Just x, Just y)   -> (Div (Const x) (Const y))
                                          (Just x, _)        -> (Div (Const x) (Var b))
                                          (_, Just y)        -> (Div (Var a) (Const y))
                                          (Nothing, Nothing) -> if a == b
                                                                then (Const 1)
                                                                else (Div (Var a) (Var b)) -}
   simplify vrs (Div e1 e2)           = Div (simplify vrs e1) (simplify vrs e2)

   -- natural exponent
   simplify vrs (E (Var x)) = case Map.lookup x vrs of
                                Just v  -> (E (Const v))
                                Nothing -> (E (Var x))
   simplify vrs (E (Ln e))  = simplify vrs e
   simplify vrs (E e)       = E (simplify vrs e)


   -- logarithm of any base
   simplify vrs (Log a (Var x)) = case Map.lookup x vrs of
                                    Just v  -> (Log a (Const v))
                                    Nothing -> (Log a (Var x))
   simplify vrs (Log a e)       = Log a (simplify vrs e)

   -- natural logarithm
   simplify vrs (Ln (Var x)) = case Map.lookup x vrs of
                                Just v  -> (Ln (Const v))
                                Nothing -> (Ln (Var x))
   simplify vrs (Ln (E e))   = simplify vrs e
   simplify vrs (Ln e)       = Ln (simplify vrs e)

   -- cosine
   simplify vrs (Cos (Var x)) = case Map.lookup x vrs of
                                  Just v  -> (Cos (Const v))
                                  Nothing -> (Cos (Var x))
   simplify vrs (Cos e)       = Cos (simplify vrs e)

   --sine
   simplify vrs (Sin (Var x)) = case Map.lookup x vrs of
                                  Just v  -> (Sin (Const v))
                                  Nothing -> (Sin (Var x))
   simplify vrs (Sin e)       = Sin (simplify vrs e)

   -- exponent
   simplify vrs (Pow (Var x) e) = case Map.lookup x vrs of
                                    Just v  -> (Pow (Const v) (simplify vrs e))
                                    Nothing -> (Pow (Var x) (simplify vrs e))
   simplify vrs (Pow e (Var x)) = case Map.lookup x vrs of
                                    Just v  -> (Pow (simplify vrs e) (Const v))
                                    Nothing -> (Pow (simplify vrs e) (Var x))
   simplify vrs (Pow e1 e2)     = Pow (simplify vrs e1) (simplify vrs e2)

   -- constants and variables
   simplify vrs (Const x)    = Const x
   simplify vrs (Var v)      = Var v

  isInfinity :: Double -> Bool
  isInfinity e = let inf = show e
                   in ((inf == "Infinity") || (inf == "-Infinity"))
