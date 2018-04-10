{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ExprDiff where

import ExprType
import Data.Map as Map


{-
class DiffExpr repr where
  eval :: (Num a) => Map.Map String a -> repr a -> a
  simplify :: (Num a) => Map.Map String a -> repr a -> repr a
  partDiff :: (Num a) => String -> repr a -> repr a

  (!+) :: (Num a) => repr a -> repr a -> repr a
  (!-) :: (Num a) => repr a -> repr a -> repr a
  (!*) :: (Num a) => repr a -> repr a -> repr a
  (!/) :: (Num a) => repr a -> repr a -> repr a
-}
