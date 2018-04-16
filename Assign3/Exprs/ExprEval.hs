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
                               Result r -> if (isInfinity r)
                                           then EvalError "Invalid Operands"
                                           else Result (exp r)
     eval vrs (Log a e)    = case (eval vrs e) of
                              EvalError err -> EvalError err
                              Result r -> if a > 0
                                          then
                                            if r > 0
                                            then Result (logBase a r)
                                            else EvalError "Expression cannot be less than or equal to 0"
                                          else EvalError "Base must be greater than 0"
     eval vrs (Ln e)       = case (eval vrs e) of
                              EvalError err -> EvalError err
                              Result r -> if r > 0
                                          then Result (log r)
                                          else EvalError "Expression cannot be less than or equal to 0"
     eval vrs (Cos e)      = (cos) <$> (eval vrs e)
     eval vrs (Sin e)      = (sin) <$> (eval vrs e)
     eval vrs (Pow e1 e2)  = case (eval vrs e1, eval vrs e2) of
                               (Result r1, Result r2) ->
                                  let res = r1 ** r2
                                    in if (isInfinity res)
                                       then EvalError "Invalid operands"
                                       else
                                         if r1 == 0 && r2 == 0
                                         then EvalError "Zero to power of zero is undefined"
                                         else Result res
                               (_, EvalError err) -> EvalError err
     eval vrs (Const x)    = Result x
     eval vrs (Var v)      = case Map.lookup v vrs of
                               Just x -> Result x
                               Nothing -> error "Lookup failed in eval"


     {- simplify expressions -}
     -- addition
     simplify vrs (Add (Const 0) e)           = simplify vrs e
     simplify vrs (Add e (Const 0))           = simplify vrs e
     simplify vrs (Add e (Var x))             = Add (simplify vrs e) (Var x)
     simplify vrs (Add (Var x) e)             = Add (Var x) (simplify vrs e)
     simplify vrs (Add (Const a) (Const b))   = case (eval vrs (Add (Const a) (Const b))) of
                                                  Result r      -> Const r
                                                  EvalError err -> error err
     simplify vrs (Add (Ln e1) (Ln e2))       = Ln (Mult (simplify vrs e1) (simplify vrs e2))
     simplify vrs (Add (Log a e1) (Log b e2)) = if a ==b
                                                then Log a (Mult (simplify vrs e1) (simplify vrs e2))
                                                else (Add (Log a (simplify vrs e1)) (Log b (simplify vrs e1)))
     simplify vrs (Add e1 e2)                 = Add (simplify vrs e1) (simplify vrs e2)

     -- subtraction
     simplify vrs (Sub e (Const 0))           = simplify vrs e
     simplify vrs (Sub e (Var x))             = Sub (simplify vrs e) (Var x)
     simplify vrs (Sub (Var x) e) = Sub (Var x) (simplify vrs e)
     simplify vrs (Sub (Const a) (Const b))   = case (eval vrs (Sub (Const a) (Const b))) of
                                                  Result r      -> Const r
                                                  EvalError err -> error err
     simplify vrs (Sub (Ln e1) (Ln e2))       = Ln (Div (simplify vrs e1) (simplify vrs e2))
     simplify vrs (Sub (Log a e1) (Log b e2)) = if a == b
                                                then Log a (Div (simplify vrs e1) (simplify vrs e1))
                                                else (Sub (Log a (simplify vrs e1)) (Log b (simplify vrs e2)))
     simplify vrs (Sub e1 e2)                 = Sub (simplify vrs e1) (simplify vrs e2)

     -- multiplication
     simplify vrs (Mult (Const 0) _)             = Const 0
     simplify vrs (Mult _ (Const 0))             = Const 0
     simplify vrs (Mult e (Var x))               = Mult (simplify vrs e) (Var x)
     simplify vrs (Mult (Var x) e) = Mult (Var x) (simplify vrs e)
     simplify vrs (Mult (Const 1) e)             = simplify vrs e
     simplify vrs (Mult e (Const 1))             = simplify vrs e
     simplify vrs (Mult (Const a) (Const b))     = case (eval vrs (Mult (Const a) (Const b))) of
                                                      Result r      -> Const r
                                                      EvalError err -> error err
     simplify vrs (Mult (Pow e1 e2) (Pow x1 x2)) = if (simplify vrs e1) == (simplify vrs x1)
                                                   then Pow (simplify vrs e1) (Add (simplify vrs e2) (simplify vrs x2))
                                                   else (Mult (Pow (simplify vrs e1) (simplify vrs e2)) (Pow (simplify vrs x1) (simplify vrs x2)))
     simplify vrs (Mult e1 e2)                   = Mult (simplify vrs e1) (simplify vrs e2)

     -- division
     simplify vrs (Div e (Const 1))         = simplify vrs e
     simplify vrs (Div (Const 0) _)         = Const 0
     simplify vrs (Div (Var x) (Var y))     = if x == y
                                              then (Const 1)
                                              else (Div (Var x) (Var y))
     simplify vrs (Div e (Var x))           = Div (simplify vrs e) (Var x)
     simplify vrs (Div (Var x) e)           = Div (Var x) (simplify vrs e)
     simplify vrs (Div (Const a) (Const b)) = case (eval vrs (Div (Const a) (Const b))) of
                                                Result r      -> Const r
                                                EvalError err -> error err
     simplify vrs (Div e1 e2)               = Div (simplify vrs e1) (simplify vrs e2)

     -- natural exponent
     simplify vrs (E (Const 0)) = Const 1
     simplify vrs (E (Var x))   = E (Var x)
     simplify vrs (E (Const a)) = case (eval vrs (E (Const a))) of
                                    Result r      -> Const r
                                    EvalError err -> error err
     simplify vrs (E (Ln e))    = simplify vrs e
     simplify vrs (E e)         = E (simplify vrs e)

     -- logarithm of any base
     simplify vrs (Log a (Const 1))   = Const 0
     simplify vrs (Log a (Var x))     = Log a (Var x)
     simplify vrs (Log a (Const b))   = case (eval vrs (Log a (Const b))) of
                                          Result r      -> Const r
                                          EvalError err -> error err
     simplify vrs (Log a (Pow e1 e2)) = Mult (simplify vrs e2) (Log a (simplify vrs e1))
     simplify vrs (Log a e)           = Log a (simplify vrs e)

     -- natural logarithm
     simplify vrs (Ln (E e))       = simplify vrs e
     simplify vrs (Ln (Const 1))   = Const 0
     simplify vrs (Ln (Var x))     = Ln (Var x)
     simplify vrs (Ln (Const a))   = case (eval vrs (Ln (Const a))) of
                                       Result r      -> Const r
                                       EvalError err -> error err
     simplify vrs (Ln (Pow e1 e2)) = Mult (simplify vrs e2) (Ln (simplify vrs e1))
     simplify vrs (Ln e)           = Ln (simplify vrs e)

     -- cosine
     simplify vrs (Cos (Var x))   = Cos (Var x)
     simplify vrs (Cos (Const a)) = case (eval vrs (Cos (Const a))) of
                                      Result r      -> Const r
                                      EvalError err -> error err
     simplify vrs (Cos e)         = Cos (simplify vrs e)

     -- sine
     simplify vrs (Sin (Var x))   = Sin (Var x)
     simplify vrs (Sin (Const a)) = case (eval vrs (Sin (Const a))) of
                                      Result r      -> Const r
                                      EvalError err -> error err
     simplify vrs (Sin e)         = Sin (simplify vrs e)

     -- exponent
     simplify vrs (Pow e (Const 0))         = Const 1
     simplify vrs (Pow e (Const 1))         = simplify vrs e
     simplify vrs (Pow (Var x) e)           = Pow (Var x) (simplify vrs e)
     simplify vrs (Pow e (Var x))           = Pow (simplify vrs e) (Var x)
     simplify vrs (Pow (Const a) (Const b)) = case (eval vrs (Pow (Const a) (Const b))) of
                                                 Result r      -> Const r
                                                 EvalError err -> error err
     simplify vrs (Pow (Pow e1 e2) e)       = Pow (simplify vrs e1) (Mult (simplify vrs e2) (simplify vrs e))
     simplify vrs (Pow e1 e2)               = Pow (simplify vrs e1) (simplify vrs e2)

     -- constants and variables
     simplify vrs (Const x) = Const x
     simplify vrs (Var v)   = case Map.lookup v vrs of
                                Just value -> Const value
                                Nothing    -> Var v

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
