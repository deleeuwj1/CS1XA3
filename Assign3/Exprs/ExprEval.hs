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
     simplify vrs (Add e1 e2) = let
       s1 = simplify vrs e1
       s2 = simplify vrs e2
       in case (s1, s2) of
            (Const a, Const b)         -> Const (a + b)
            (Const 0, e)               -> s2
            (e, Const 0)               -> s1
            (Const a, Add (Const b) e) -> simplify vrs $ Add (Const (a + b)) (simplify vrs e)
            (Const a, e)               -> Add (Const a) s2
            (e, Const a)               -> Add (Const a) s1
            (e, Var x)                 -> Add s1 (Var x)
            (Var x, e)                 -> Add s2 (Var x)
            (x1, Mult (Const (-1)) x2) -> simplify vrs (Sub x1 x2)
            (Ln e3, Ln e4)             -> simplify vrs $ Ln (Mult (simplify vrs e3) (simplify vrs e4))
            (Log a e3, Log b e4)       -> if a == b
                                          then simplify vrs $ Log a (Mult (simplify vrs e3) (simplify vrs e4))
                                          else simplify vrs (Add (Log a (simplify vrs e3)) (Log b (simplify vrs e4)))
            (x1, x2)                   -> Add x1 x2

     -- subtraction
     {-
     simplify vrs (Sub e1 e2) = let
       s1 = simplify vrs e1
       s2 = simplify vrs e2
       in case (s1, s2) of
           (Const a, Const b)   -> Const (a - b)
           (e, Const 0)         -> s1
           (Const a, e)         -> Sub s2 (Const a)
           (e, Const a)         -> Sub s1 (Const a)
           (e, Var x)           -> Sub (Var x) s1
           (Var x, e)           -> Sub (Var x) s2
           (Ln e3, Ln e4)       -> simplify vrs $ Ln (Mult (simplify vrs e3) (simplify vrs e4))
           (Log a e3, Log b e4) -> if a == b
                                   then simplify vrs $ Log a (Div (simplify vrs e3) (simplify vrs e4))
                                   else simplify vrs (Sub (Log a (simplify vrs e3)) (Log b (simplify vrs e4)))
-}
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
     simplify vrs (Mult e1 e2) = let
       s1 = simplify vrs e1
       s2 = simplify vrs e2
       in case (s1, s2) of
         (Const 0, _)                -> Const 0
         (_, Const 0)                -> Const 0
         (Const 1, e)                -> s2
         (e, Const 1)                -> s1
         (Const (-1), Const (-1))    -> Const 1
         (Const (-1), Mult (Const (-1)) e) -> e
         (Const a, Const b)          -> Const (a * b)
         (Const a, Mult (Const b) e) -> Mult (Const (a * b)) e
         (Const a, e)                -> Mult (Const a) s2
         (e, Const a)                -> Mult (Const a) s1
         (Var x, Var y)              -> if x <= y
                                        then Mult (Var x) (Var y)
                                        else Mult (Var y) (Var x)
         (Var x, (Mult (Var y) e))   -> if x <= y
                                        then Mult (Var x) (Mult (Var y) e)
                                        else simplify vrs $ Mult (Var y) (Mult (Var x) e)
         (Var x, e)                  -> Mult s2 (Var x)
         (e, Var x)                  -> Mult s1 (Var x)
         (Pow e1 e2, Pow x1 x2)      -> if e1 == x1
                                        then Pow e1 (Add e2 x2)
                                        else Mult (Pow e1 e2) (Pow x1 x2)
         (x1, x2)                     -> Mult x1 x2

     -- division
     simplify vrs (Div e1 e2) = let
       s1 = simplify vrs e1
       s2 = simplify vrs e2
       in case (s1, s2) of
            (e, Const 1)               -> s1
            (Const 0, _)               -> Const 0
            (Const a, Const b)         -> Const (a / b)
            (Const a, Div (Const b) e) -> Div (Mult (Const a) e) (Const b)
            (Var x, Var y)             -> if x == y
                                          then Const 1
                                          else Div (Var x) (Var y)
            (Var x, e)                 -> Div (Var x) s2
            (e, Var x)                 -> Div s1 (Var x)
            (Const a, Ln e)            -> Mult (Const a) (Ln e)
            (x1, Ln x2)                -> simplify vrs $ Mult (Ln x2) x1
            (Pow e1 e2, Pow x1 x2)     -> if e1 == x1
                                          then Pow e1 (Sub e2 x2)
                                          else Div (Pow e1 e2) (Pow x1 x2)
            (x1, x2)                   -> Div x1 x2

     -- natural exponent
     simplify vrs (E e) = let
       s = simplify vrs e
       in case s of
            (Const 0)         -> Const 1
            (Const a)         -> case (eval vrs (E (Const a))) of
                                   Result r      -> Const r
                                   EvalError err -> E (Const a)
            (Var x)           -> E (Var x)
            (Ln x)            -> x
            (Mult x1 (Ln x2)) -> Pow x2 x1
            x                 -> x

     -- logarithm of any base
     simplify vrs (Log b e) = let
       s = simplify vrs e
       in case s of
         (Const 1)   -> Const 0
         (Const a)   -> case (eval vrs (Log b (Const a))) of
                          Result r      -> Const r
                          EvalError err -> Log b (Const a)
         (Var x)     -> Log b (Var x)
         (Pow e1 e2) -> simplify vrs $ Mult e2 (Log b e1)

     -- natural logarithm
     simplify vrs (Ln e) = let
       s = simplify vrs e
       in case s of
            (Const 1)   -> Const 0
            (Const a)   -> case (eval vrs (Ln (Const a))) of
                             Result r      -> Const r
                             EvalError err -> Ln (Const a)
            (Var x)     -> Ln (Var x)
            (E x)       -> x
            (Pow e1 e2) -> simplify vrs $ Mult e2 (Ln e1)
            x           -> Ln x

     -- cosine
     simplify vrs (Cos e) = let
       s = simplify vrs e
       in case s of
            (Const a) -> case (eval vrs (Cos (Const a))) of
                          Result r      -> Const r
                          EvalError err -> Cos (Const a)
            (Var x)   -> Cos (Var x)
            x         -> x

     -- sine
     simplify vrs (Sin e) = let
       s = simplify vrs e
       in case s of
            (Const a) -> case (eval vrs (Sin (Const a))) of
                          Result r      -> Const r
                          EvalError err -> Sin (Const a)
            (Var x)   -> Sin (Var x)
            x         -> x

     -- exponent
     simplify vrs (Pow e1 e2) = let
       s1 = simplify vrs e1
       s2 = simplify vrs e2
       in case (s1, s2) of
            (Const 0, e)       -> Const 0
            (e, Const 0)       -> Const 1
            (e, Const 1)       -> e
            (Const a, Const b) -> case (eval vrs (Pow (Const a) (Const b))) of
                                    Result r      -> Const r
                                    EvalError err -> Pow (Const a) (Const b)
            (Var x, e)         -> Pow (Var x) e
            (e, Var x)         -> Pow e (Var x)
            (Pow x1 x2, e)     -> simplify vrs $ Pow x1 (Mult x2 e)
            (x1, x2)           -> Pow x1 x2

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
