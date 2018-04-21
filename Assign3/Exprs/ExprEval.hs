{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts, ConstrainedClassMethods #-}

{-|
Module      : ExprEval
Description : Contains a type class and instances for evaluating and simplifying expressions
Copyright   : (c) Jessica de Leeuw @2018
License     : MIT
Maintainer  : deleeuwj@mcmaster.ca
Stability   : experimental
Portability : POSIX
-}

module ExprEval where

  import ExprType
  import ExprPretty
  import Data.Map as Map

  -- | `EvalResult` is a type that holds the result of the evaluation of an expression
  data EvalResult a = EvalError String -- ^ Wraps an error string if there is an error during computation
                    | Result a         -- ^ Wraps the result of a successful computation
    deriving Show

  instance Functor EvalResult where
    fmap f (Result x) = Result (f x)
    fmap f (EvalError err) = EvalError err

  instance Applicative EvalResult where
    pure x = Result x
    (Result f) <*> x = fmap f x
    (EvalError err) <*> _ = EvalError err

  {- Helper function for eval:
   - Determines if an expression evaluates to either negative or positive Infinity -}
  -- | `isInfinity` takes a Double, Float, Int or Integer and returns a Boolean result
  isInfinity :: (AllNums a) => a -> Bool
  isInfinity e = let inf = show e
                     in ((inf == "Infinity") || (inf == "-Infinity"))

  -- * Class Declaration
  {-  Class ExprEval
   -  ------------------------------
   -  Methods
     -  eval- given a dictionary of variable identifiers and an Expr, will evaluate the Expr to a value
     -  simplify- given a possibly incomplete dictionary of variable identifiers and an Expr, will evalute the Expr as much as possible and reduce
   -  Default Methods
    -  !+, !-, !*, !/, e, log a, ln, cos, sin, pow, val, var: corresponed to optimized type wrappers
   - Example DSL use:
     -  "x + y * 0" should become "x" -}


  class ExprEval a where
     eval :: Map.Map String a -> Expr a -> EvalResult a
     simplify :: Map.Map String a -> Expr a -> Expr a

     {- Default Methods -}
     (!+) :: (AllNums a) => Expr a -> Expr a -> Expr a
     e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
     (!-) :: (AllNums a) => Expr a -> Expr a -> Expr a
     e1 !- e2 =  simplify (Map.fromList []) $ Sub e1 e2
     (!*) :: (AllNums a) => Expr a -> Expr a -> Expr a
     e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2
     (!/) :: (AllNums a) => Expr a -> Expr a -> Expr a
     e1 !/ e2 = simplify (Map.fromList []) $ Div e1 e2
     nPow :: (AllNums a) => Expr a -> Expr a
     nPow e = simplify (Map.fromList []) $ E e
     log' :: (AllNums a) => a -> Expr a -> Expr a
     log' a e = simplify (Map.fromList []) $ Log a e
     ln :: (AllNums a) => Expr a -> Expr a
     ln e = simplify (Map.fromList []) $ Ln e
     sine :: (AllNums a) => Expr a -> Expr a
     sine e = simplify (Map.fromList []) $ Sin e
     cosine :: (AllNums a) => Expr a -> Expr a
     cosine e = simplify (Map.fromList []) $ Cos e
     (!^) :: (AllNums a) => Expr a -> Expr a -> Expr a
     e1 !^ e2 = simplify (Map.fromList []) $ Pow e1 e2
     val :: (AllNums a) => a -> Expr a
     val = Const
     var :: (AllNums a) => String -> Expr a
     var = Var

  -- Evaluates and simplifies an expression as much as possible
  instance (AllNums a) => ExprEval a where
     eval vrs (Add e1 e2)  = (+) <$> (eval vrs e1) <*> (eval vrs e2)
     eval vrs (Sub e1 e2)  = (-) <$> (eval vrs e1) <*> (eval vrs e2)
     eval vrs (Mult e1 e2) = (*) <$> (eval vrs e1) <*> (eval vrs e2)
     eval vrs (Div e1 e2)  = case (eval vrs e1, eval vrs e2) of
                               (_, EvalError err) -> EvalError err
                               (Result r1, Result r2) ->
                                 let res = numDiv r1 r2
                                   in if r2 == 0 then EvalError "Zero Division Error" else Result res -- cannot divide something by 0
     eval vrs (E e)        = case (eval vrs e) of
                               EvalError err -> EvalError err
                               Result r -> if (isInfinity r)
                                           then EvalError "Invalid Operands" -- this throws an error if the expression evaluates to a large number
                                           else Result (numE r)
     eval vrs (Log a e)    = case (eval vrs e) of
                              EvalError err -> EvalError err
                              Result r -> if a > 0
                                          then
                                            if r > 0
                                            then Result (numLog a r)
                                            else EvalError "Expression cannot be less than or equal to 0" -- log of negative numbers or 0 doesn't exist
                                          else EvalError "Base must be greater than 0" --cannot have a base of 0 in log
     eval vrs (Ln e)       = case (eval vrs e) of
                              EvalError err -> EvalError err
                              Result r -> if r > 0
                                          then Result (numLn r)
                                          else EvalError "Expression cannot be less than or equal to 0" -- ln of negative numbers or 0 doesn't exist
     eval vrs (Cos e)      = (numCos) <$> (eval vrs e) -- applies cos to the expression
     eval vrs (Sin e)      = (numSin) <$> (eval vrs e) -- applies sin to the expression
     eval vrs (Pow e1 e2)  = case (eval vrs e1, eval vrs e2) of
                               (Result r1, Result r2) ->
                                  let res = numPow r1 r2
                                    in if (isInfinity res)
                                       then EvalError "Invalid operands" -- this throws an error if the expression evaluates to a large number
                                       else
                                         if r1 == 0 && r2 == 0
                                         then EvalError "Zero to power of zero is undefined" -- 0 to the 0 is undefined
                                         else Result res
                               (_, EvalError err) -> EvalError err
     eval vrs (Const x)    = Result x
     eval vrs (Var v)      = case Map.lookup v vrs of
                               Just x -> Result x
                               Nothing -> error "Lookup failed in eval" -- if the variable is not in the list of variables and values

     -- | Simplification of an expression

     -- Simplification of addition
     simplify vrs (Add e1 e2) = let
       s1 = simplify vrs e1
       s2 = simplify vrs e2
       in case (s1, s2) of
            (Const a, Const b)         -> Const (a + b) -- simplifies the addition of two constants into just one constant
            (Const 0, e)               -> s2            -- 0 plus anything is always 0
            (e, Const 0)               -> s1
            (Const a, Add (Const b) e) -> simplify vrs $ Add (Const (a + b)) (simplify vrs e) -- addtion of nested constants into one consant
            (Const a, e)               -> Add (Const a) s2 -- brings the constant to the front of the simplified expression
            (e, Const a)               -> Add (Const a) s1
            (Var x, Var y)             -> if x == y
                                          then simplify vrs $ Mult (Const 2) (Var x)
                                          else Add s1 s2
            (Var x, Add (Var y) e)     -> if x == y
                                          then simplify vrs $ Add e (Mult (Const 2) (Var x))
                                          else Add s1 s2
            (e, Var x)                 -> Add s1 (Var x)  -- brings the variable to the end of the simplified expression
            (Var x, e)                 -> Add s2 (Var x)
            (x1, Mult (Const (-1)) x2) -> simplify vrs (Sub x1 x2) -- turns addition of an expression and a negative expression into subtraction
            (Ln e3, Ln e4)             -> simplify vrs $ Ln (Mult e3 e4) -- ln rules of addition
            (Log a e3, Log b e4)       -> if a == b -- if the bases are equal, the logarithm rules of addition apply
                                          then simplify vrs $ Log a (Mult e3 e4)
                                          else simplify vrs (Add (Log a e3) (Log b e4)) --if not, the expression remains the same
            (Mult (Const a) x1, Mult (Const b) x2)
                                       -> if x1 == x2 -- if the variables are the same, their coefficients can be added
                                          then Mult (Const (a + b)) s1
                                          else Add (Mult (Const a) x1) (Mult (Const b) x2) -- otherwise the expression is left as is
            (Mult (Const a) x1, x2)    -> if x1 == x2
                                          then Mult (Const (a + 1)) s2
                                          else Add (Mult (Const a) x1) x2
            (x1, Mult (Const a) x2)    -> if x1 == x2
                                          then Mult (Const (a + 1)) s1
                                          else Add (Mult (Const a) x2) x1
            (x1, x2)                   -> if x1 == x2
                                          then Mult (Const 2) x1
                                          else Add x1 x2 -- the expression remains the same if it does not fit any of the cases

     -- Simplification of subtraction
     simplify vrs (Sub e1 e2) = let
       s1 = simplify vrs e1
       s2 = simplify vrs e2
       in case (s1, s2) of
           (e, Const 0)               -> s1 -- an expression minos 0 is itself
           (Const 0, e)               -> Mult (Const (-1)) s2
           (Const a, Const b)         -> Const (a - b) -- simplifies the subtraction of two constants into just one constant
           (Const a, e)               -> Sub (Const a) s2
           (e, Const a)               -> Sub s1 (Const a)
           (x1, Mult (Const (-1)) x2) -> simplify vrs $ Add x1 x2 -- subtraction of a negative number is just addition
           (e, Var x)                 -> Sub s2 (Var x)
           (Var x, e)                 -> Sub (Var x) s2
           (Ln e3, Ln e4)             -> simplify vrs $ Ln (Div e3 e4) --ln rules of subtraction
           (Log a e3, Log b e4)       -> if a == b  --logarithm rules of subtraction
                                         then simplify vrs $ Log a (Div e3 e4)
                                         else Sub s1 s2
           (x1, x2)                   -> if x1 == x2
                                         then Const 0
                                         else Sub s1 s2

     -- Simplification of multiplication
     simplify vrs (Mult e1 e2) = let
       s1 = simplify vrs e1
       s2 = simplify vrs e2
       in case (s1, s2) of
         (Const 0, _)                     -> Const 0 -- 0 multiplied by anything is 0
         (_, Const 0)                     -> Const 0
         (Const 1, e)                     -> s2 -- anything multiplied by 1 is itself
         (e, Const 1)                     -> s1
         (Const (-1), Const (-1))         -> Const 1
         (Const (-1),Mult (Const (-1)) e) -> e -- two negative make a positive
         (Const a, Const b)               -> Const (a * b) -- simplifies the multiplication of two constants into just one constant
         (Const a, Mult (Const b) e)      -> Mult (Const (a * b)) e
         (Mult (Const a) e, Const b)      -> Mult (Const (a * b)) e
         (Const a, e)                     -> Mult (Const a) s2
         (e, Const a)                     -> Mult (Const a) s1
         (Mult (Const a) x1, x2)          -> simplify vrs $ Mult (Const a) (Mult x1 x2)
         (x1, Mult (Const a) x2)          -> simplify vrs $ Mult (Const a) (Mult x1 x2)
         (Var x, Var y)                   -> if x < y -- puts them in alphabetic order for standardization
                                             then Mult s1 s2
                                             else
                                               if x == y
                                               then Pow (Var x) (Const 2)
                                               else Mult (Var y) (Var x)
         (Var x, (Mult (Var y) e))        -> if x < y -- puts them in alphabetic order for standardization
                                             then Mult s1 s2
                                             else simplify vrs $ Mult (Var y) (Mult (Var x) e)
         (Pow (Var x) (Const a), Var y)   -> if x == y
                                             then Pow (Var x) (Const (a + 1))
                                             else Mult s1 s2
         (Pow (Var x) (Const a), Mult (Var y) e)
                                          -> if x == y
                                             then simplify vrs $ Mult (Pow (Var x) (Const (a + 1))) e
                                             else Mult s1 s2
         (Var x, e)                       -> Mult s2 (Var x) -- brings the variable to the end of the simplified expression
         (e, Var x)                       -> Mult s1 (Var x)
         (Pow e1 e2, Pow x1 x2)           -> if e1 == x1 -- power rule of multiplication of two bases that are the same
                                             then Pow e1 (Add e2 x2)
                                             else Mult s1 s2
         (x1, x2)                         -> Mult x1 x2 -- the expression remains the same if it does not fit any of the cases

     -- Simplification of division
     simplify vrs (Div e1 e2) = let
       s1 = simplify vrs e1
       s2 = simplify vrs e2
       in case (s1, s2) of
            (e, Const 1)               -> s1 -- anything divided by 1 is itself
            (Const 0, _)               -> Const 0 -- 0 divided by anything is 0
            (Const a, Const b)         -> case (eval vrs (Div (Const a) (Const b))) of
                                            Result r      -> Const r -- simplifies the power of a constant to a constant into just a constant
                                            EvalError err -> Div (Const a) (Const b)
            (Const a, Div (Const b) e) -> simplify vrs $ Mult (Div (Const a) (Const b)) e -- simplifes multiple divisions into numerator / denominator
            (Mult (Const a) x1, x2)    -> if x1 == x2
                                          then Const a
                                          else simplify vrs $ Mult (Const a) (Div x1 x2)
            (Mult x1 x2, x3)           -> if x1 == x3 --removes expressions that appear in both the top and bottom
                                          then x2
                                          else if x2 == x3
                                               then x1
                                               else if s1 == x3
                                                    then Const 1
                                                    else Div (Mult x1 x2) x3
            (x1, Mult x2 x3)           -> if x1 == x3 --removes expressions that appear in both the top and bottom
                                          then simplify vrs $ Div (Const 1) x2
                                          else if x1 == x2
                                               then simplify vrs $ Div (Const 1) x3
                                               else if x1 == s2
                                                    then Const 1
                                                    else Div x1 (Mult x2 x3)
            (Var x, Var y)             -> if x == y -- if the variables on the top and bottom are the same, it becoems q1
                                          then Const 1
                                          else Div s1 s2
            (Var x, e)                 -> Div (Var x) s2 -- the expression remains the same
            (e, Var x)                 -> Div s1 (Var x)
            (Pow e1 e2, Pow x1 x2)     -> if e1 == x1
                                          then simplify vrs $ Pow e1 (Sub e2 x2)
                                          else Div s1 s2
            (x1, x2)                   -> if x1 == x2
                                          then Const 1
                                          else Div x1 x2 -- the expression remains the same if it does not fit any of the cases

     -- Simplification of the natural exponent
     simplify vrs (E e) = let
       s = simplify vrs e
       in case s of
            (Const 0)         -> Const 1
            (Const a)         -> case (eval vrs (E (Const a))) of
                                   Result r      -> Const r -- simplifies the natural exponent of a constant into just a constant
                                   EvalError err -> E (Const a)
            (Var x)           -> E s
            (Ln x)            -> x
            (Mult x1 (Ln x2)) -> Pow x2 x1
            x                 -> E x -- the expression remains the same if it does not fit any of the cases

     -- Simplification of a logarithm of any base
     simplify vrs (Log b e) = let
       s = simplify vrs e
       in case s of
         (Const 1)         -> Const 0 -- log of 0 is 1
         (Const a)         -> if a == b
                              then Const 1
                              else case (eval vrs (Log b (Const a))) of
                                    Result r      -> Const r -- simplifies the logarithm of a constant into just a constant
                                    EvalError err -> Log b (Const a)
         (Var x)           -> Log b (Var x)
         (Div (Const a) x) -> simplify vrs $ Mult (Mult (Const (-1)) (Const a)) (Log b x) -- logarithm power rule
         (Pow e1 e2)       -> simplify vrs $ Mult e2 (Log b e1)
         x                 -> Log b x -- the expression remains the same if it does not fit any of the cases

     -- Simplification of the natural logarithm
     simplify vrs (Ln e) = let
       s = simplify vrs e
       in case s of
            (Const 1)        -> Const 0
            (Const a)        -> case (eval vrs (Ln (Const a))) of
                                  Result r      -> Const r -- simplifies the natural logarithm of a constant into just a constant
                                  EvalError err -> Ln (Const a)
            (Var x)           -> Ln (Var x)
            (Div (Const a) x) -> simplify vrs $ Mult (Mult (Const (-1)) (Const a)) (Ln x) -- logarithm power rule
            (E x)             -> x -- ln of e to an expression becomes just the expression
            (Pow e1 e2)       -> simplify vrs $ Mult e2 (Ln e1) -- logarithm power rule
            x                 -> Ln x -- the expression remains the same if it does not fit any of the cases

     -- Simplification of cosine
     simplify vrs (Cos e) = let
       s = simplify vrs e
       in case s of
            (Const a) -> case (eval vrs (Cos (Const a))) of
                          Result r      -> Const r -- simplifies the cosine of a constant into just a constant
                          EvalError err -> Cos (Const a)
            x         -> Cos x -- the expression remains the same if it does not fit any of the cases

     -- Simplification of sine
     simplify vrs (Sin e) = let
       s = simplify vrs e
       in case s of
            (Const a) -> case (eval vrs (Sin (Const a))) of
                          Result r      -> Const r -- simplifies the cosine of a constant into just a constant
                          EvalError err -> Sin (Const a)
            x         -> Sin x -- the expression remains the same if it does not fit any of the cases

     -- Simplification of exponents
     simplify vrs (Pow e1 e2) = let
       s1 = simplify vrs e1
       s2 = simplify vrs e2
       in case (s1, s2) of
            (Const 0, e)       -> Const 0 -- 0 to any power is 0
            (e, Const 0)       -> Const 1 -- anything to exponent 0 becomes 1
            (e, Const 1)       -> e
            (e, Const (-1))    -> simplify vrs $ Div (Const 1) e
            (Const a, Const b) -> case (eval vrs (Pow (Const a) (Const b))) of
                                    Result r      -> Const r -- simplifies the power of a constant to a constant into just a constant
                                    EvalError err -> Pow (Const a) (Const b)
            (Var x, e)         -> Pow (Var x) e
            (e, Var x)         -> Pow e (Var x)
            (Pow x1 x2, e)     -> simplify vrs $ Pow x1 (Mult x2 e)
            (x1, x2)           -> Pow x1 x2

     -- Simplification of variables and constants
     simplify vrs (Const x) = Const x
     simplify vrs (Var v)   = case Map.lookup v vrs of
                                Just value -> Const value
                                Nothing    -> Var v
  -- | Allows `eval` and `simplify` to be used with Double, Float, Integer and Int types by creating instances for each type and defining mathematical operators in them
  class (Num a, Eq a, Show a, Ord a) => AllNums a where
    numDiv :: a -> a -> a
    numE :: a -> a
    numLog :: a -> a -> a
    numLn :: a -> a
    numCos :: a -> a
    numSin :: a -> a
    numPow :: a -> a -> a

  -- | Deals with values of type float
  instance AllNums Float where
    numDiv a b = a / b
    numE x     = exp x
    numLog b x = logBase b x
    numLn x    = log x
    numCos x   = cos x
    numSin x   = sin x
    numPow b x = b ** x

  -- | Deals with values of type double
  instance AllNums Double where
    numDiv a b = a / b
    numE x     = exp x
    numLog b x = logBase b x
    numLn x    = log x
    numCos x   = cos x
    numSin x   = sin x
    numPow b x = b ** x

  -- | Deals with values of type Int (it turns number into Ints to do this)
  instance AllNums Int where
    numDiv a b = round $ (fromIntegral a) / (fromIntegral b)
    numE x     = round $ exp (fromIntegral x)
    numLog b x = round $ logBase (fromIntegral b) (fromIntegral x)
    numLn x    = round $ log (fromIntegral x)
    numCos x   = round $ cos (fromIntegral x)
    numSin x   = round $ sin (fromIntegral x)
    numPow b x = round $ (fromIntegral b) ** (fromIntegral x)

  -- | Deals with values of type Integer (it turns number into Integers to do this)
  instance AllNums Integer where
    numDiv a b = round $ (fromInteger a) / (fromInteger b)
    numE x     = round $ exp (fromInteger x)
    numLog b x = round $ logBase (fromInteger b) (fromInteger x)
    numLn x    = round $ log (fromInteger x)
    numCos x   = round $ cos (fromInteger x)
    numSin x   = round $ sin (fromInteger x)
    numPow b x = round $ (fromInteger b) ** (fromInteger x)
