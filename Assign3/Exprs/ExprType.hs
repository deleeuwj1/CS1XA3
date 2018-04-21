{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}

{-|
Module      : ExprType
Description : Contains the datatype declaration of `Expr`
Copyright   : (c) Jessica de Leeuw @2018
License     : MIT
Maintainer  : deleeuwj@mcmaster.ca
Stability   : experimental
Portability : POSIX
-}

module ExprType where

  import Data.List

  import Generic.Random.Generic
  import GHC.Generics

  -- * Datatype declaration

  {- |  Datatype for Numerical Expressions
   - Supports the following operations: -}

  data Expr a = Add (Expr a) (Expr a)  -- ^ Add - binary addition
              | Sub (Expr a) (Expr a)  -- ^ Sub - binary subtraction
              | Mult (Expr a) (Expr a) -- ^ Mult - binary multiplication
              | Div (Expr a) (Expr a)  -- ^ Div - binary division
              | E (Expr a)             -- ^ E - natural exponent
              | Log a (Expr a)         -- ^ Log - log of base a
              | Ln (Expr a)            -- ^ Ln - natural logarithm
              | Cos (Expr a)           -- ^ Cos - binary multiplication
              | Sin (Expr a)           -- ^ Sin - binary multiplication
              | Pow (Expr a) (Expr a)  -- ^ Pow - some expression to some exponent
              | Const a                -- ^ Const - wraps a constant value
              | Var String             -- ^ Var - wraps a variable identifier
    deriving (Eq, Generic)

  {- `getVars`
   - | Given an expression, retrieves a list of all variable identifiers that are present in the expression
  -}

  getVars :: Expr a -> [String] -- ^ takes an expression and returns a list of variables as Strings
  getVars (Add e1 e2)  = getVars e1 ++ getVars e2
  getVars (Sub e1 e2)  = getVars e1 ++ getVars e2
  getVars (Mult e1 e2) = getVars e1 ++ getVars e2
  getVars (Div e1 e2)  = getVars e1 ++ getVars e2
  getVars (E e)        = getVars e
  getVars (Log b e)    = getVars e
  getVars (Ln e)       = getVars e
  getVars (Cos e)      = getVars e
  getVars (Sin e)      = getVars e
  getVars (Pow e1 e2)  = getVars e1 ++ getVars e2
  getVars (Const _)    = []
  getVars (Var x)      = [x]
