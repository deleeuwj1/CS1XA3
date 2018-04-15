{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

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

  {- Helper function for eval
   - Determines if an expression evalutes to either negative or positive Infinity-}
  isInfinity :: Double -> Bool -- (AllNums a) => a -> Bool
  isInfinity e = let inf = show e
                     in ((inf == "Infinity") || (inf == "-Infinity"))

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

  instance ExprEval Double where -- (AllNums a) => ExprEval a
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
                              Result r -> if a > 0
                                          then
                                            if r /= 0
                                            then Result (logBase a r)
                                            else EvalError "Expression cannot be equal to 0"
                                          else EvalError "Base must be greater than 0"
     eval vrs (Ln e)       = case (eval vrs e) of
                              EvalError err -> EvalError err
                              Result r -> if r /= 0
                                          then Result (log r)
                                          else EvalError "Expression cannot be equal to 0"
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
     -- addition
     simplify vrs (Add e (Var x))           = Add (simplify vrs e) (Var x)
     simplify vrs (Add (Var x) e)           = Add (Var x) (simplify vrs e)
     simplify vrs (Add (Const 0) e)         = simplify vrs e
     simplify vrs (Add e (Const 0))         = simplify vrs e
     simplify vrs (Add (Const a) (Const b)) = Const (a + b)
     simplify vrs (Add e1 e2)               = Add (simplify vrs e1) (simplify vrs e2)

     -- subtraction
     simplify vrs (Sub e (Var x))           = Sub (simplify vrs e) (Var x)
     simplify vrs (Sub (Var x) e)           = Sub (Var x) (simplify vrs e)
     simplify vrs (Sub e (Const 0))         = simplify vrs e
     simplify vrs (Sub (Const a) (Const b)) = Const (a - b)
     simplify vrs (Sub e1 e2)               = Sub (simplify vrs e1) (simplify vrs e2)

     -- multiplication
     simplify vrs (Mult e (Var x))           = Mult (simplify vrs e) (Var x)
     simplify vrs (Mult (Var x) e)           = Mult (Var x) (simplify vrs e)
     simplify vrs (Mult (Const 0) e)         = Const 0
     simplify vrs (Mult e (Const 0))         = Const 0
     simplify vrs (Mult (Const 1) e)         = simplify vrs e
     simplify vrs (Mult e (Const 1))         = simplify vrs e
     simplify vrs (Mult (Const a) (Const b)) = Const (a * b)
     simplify vrs (Mult e1 e2)               = Mult (simplify vrs e1) (simplify vrs e2)

     -- division
     simplify vrs (Div (Var x) (Var y))    = if x == y
                                             then (Const 1)
                                             else (Div (Var x) (Var y))
     simplify vrs (Div e (Var x))           = Div (simplify vrs e) (Var x)
     simplify vrs (Div (Var x) e)           = Div (Var x) (simplify vrs e)
     simplify vrs (Div e (Const 1))         = simplify vrs e
     simplify vrs (Div (Const 0) _)         = Const 0
     simplify vrs (Div (Const a) (Const b)) = Const (numDiv a b)
     simplify vrs (Div e1 e2)               = Div (simplify vrs e1) (simplify vrs e2)

     -- natural exponent
     simplify vrs (E (Var x))   = E (Var x)
     simplify vrs (E (Const 0)) = Const 1
     simplify vrs (E (Ln e))    = simplify vrs e
     simplify vrs (E e)         = E (simplify vrs e)

     -- logarithm of any base
     simplify vrs (Log a (Var x))   = Log a (Var x)
     simplify vrs (Log a (Const 1)) = Const 0
     simplify vrs (Log a (Const x)) = Const (numLog a x)
     simplify vrs (Log a e)         = Log a (simplify vrs e)

     -- natural logarithm
     simplify vrs (Ln (Var x))   = Ln (Var x)
     simplify vrs (Ln (E e))     = simplify vrs e
     simplify vrs (Ln (Const 1)) = Const 0
     simplify vrs (Ln (Const a)) = Const (numLn a)
     simplify vrs (Ln e)         = Ln (simplify vrs e)

     -- cosine
     simplify vrs (Cos (Var x))   = Cos (Var x)
     simplify vrs (Cos (Const a)) = Const (numCos a)
     simplify vrs (Cos e)         = Cos (simplify vrs e)

     -- sine
     simplify vrs (Sin (Var x))   = Sin (Var x)
     simplify vrs (Sin (Const a)) = Const (numSin a)
     simplify vrs (Sin e)         = Sin (simplify vrs e)

     -- exponent
     simplify vrs (Pow (Var x) e)           = Pow (Var x) (simplify vrs e)
     simplify vrs (Pow e (Var x))           = Pow (simplify vrs e) (Var x)
     simplify vrs (Pow e (Const 0))         = Const 1
     simplify vrs (Pow e (Const 1))         = simplify vrs e
     simplify vrs (Pow (Const a) (Const b)) = Const (numPow a b)
     simplify vrs (Pow e1 e2)               = Pow (simplify vrs e1) (simplify vrs e2)

     -- constants and variables
     simplify vrs (Const x) = Const x
     simplify vrs (Var v)   = Var v

  class (Num a) => AllNums a where
    numDiv :: a -> a -> a
    numE :: a -> a
    numLog :: a -> a -> a
    numLn :: a -> a
    numCos :: a -> a
    numSin :: a -> a
    numPow :: a -> a -> a

  instance AllNums Float where
    numDiv a b = a / b
    numE x     = exp x
    numLog b x = logBase b x
    numLn x    = log x
    numCos x   = cos x
    numSin x   = sin x
    numPow b x = b ** x

  instance AllNums Double where
    numDiv a b = a / b
    numE x     = exp x
    numLog b x = logBase b x
    numLn x    = log x
    numCos x   = cos x
    numSin x   = sin x
    numPow b x = b ** x

  instance AllNums Int where
    numDiv a b = round $ (fromIntegral a) / (fromIntegral b)
    numE x     = round $ exp (fromIntegral x)
    numLog b x = round $ logBase (fromIntegral b) (fromIntegral x)
    numLn x    = round $ log (fromIntegral x)
    numCos x   = round $ cos (fromIntegral x)
    numSin x   = round $ sin (fromIntegral x)
    numPow b x = round $ (fromIntegral b) ** (fromIntegral x)

  instance AllNums Integer where
    numDiv a b = round $ (fromInteger a) / (fromInteger b)
    numE x     = round $ exp (fromInteger x)
    numLog b x = round $ logBase (fromInteger b) (fromInteger x)
    numLn x    = round $ log (fromInteger x)
    numCos x   = round $ cos (fromInteger x)
    numSin x   = round $ sin (fromInteger x)
    numPow b x = round $ (fromInteger b) ** (fromInteger x)
