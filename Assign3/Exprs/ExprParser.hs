{-|
Module      : ExprParser
Description : Contains functions that parse strings of a certain format into an `Expr` type
Copyright   : (c) Jessica de Leeuw @2018
License     : MIT
Maintainer  : deleeuwj@mcmaster.ca
Stability   : experimental
Portability : POSIX
-}

module ExprParser (parseExprD, parseExprF, parseExprI) where

  import ExprType
  import ExprEval
  import ExprPretty

  import Text.Parsec
  import Text.Parsec.String

  {- parsing doubles -}

  -- | Parses an expression that contains constants of type Double
  parseExprD :: String -> Expr Double
  parseExprD ss = case parse exprD "" ss of
                    Left err   -> error $ show err
                    Right expr -> expr -- only parses if the expression is valid

  -- | Parses a constant of type Double
  parseConstD :: Parser (Expr Double)
  parseConstD = do { d <- decimalNum;
                     return $ Const (read d) }

  -- | Parses in order of BEDMAS and mathematical expressions
  exprD :: Parser (Expr Double)
  exprD = let -- parses in order: first, second, third so that it follows the order of BEDMAS
    first = (parens exprD) <|> otherOps (parens exprD) <|> logOpD (parens exprD) <|> (parseConstD <|> parseVar)
    negFirst = do { char '-'; --in case the expression has a negative in front of it
                     f <- first;
                     return $ Mult (Const (-1)) f }
    second = (first <|> negFirst) `chainl1` powOp -- power is the next highest for order of parsing
    third = second `chainl1` mulOp -- multiplication and division are next
    in third `chainl1` addOp -- adiition and subtraction are last

  -- | Parses logarithms of type Double
  logOpD :: Parser (Expr Double) -> Parser (Expr Double)
  logOpD e1 = do { symbol "log"; b <- digits; spaces; p <- e1; return $ log' (read b) p }

  {- parsing floats -}

  -- | Parses an expression that contains constants of type Float
  parseExprF :: String -> Expr Float
  parseExprF ss = case parse exprF "" ss of
                    Left err   -> error $ show err
                    Right expr -> expr -- only parses if the expression is valid

  -- | Parses a constant of type Float
  parseConstF :: Parser (Expr Float)
  parseConstF = do { d <- decimalNum;
                     return (Const (read d)) }

  -- | Parses in order of BEDMAS and mathematical expressions
  exprF :: Parser (Expr Float)
  exprF = let -- parses in order: first, second, third so that it follows the order of BEDMAS
    first = (parens exprF) <|> otherOps (parens exprF) <|> logOpF (parens exprF) <|> (parseConstF <|> parseVar)
    negFirst = do { char '-'; --in case the expression has a negative in front of it
                     f <- first;
                     return $ Mult (Const (-1)) f }
    second = (first <|> negFirst) `chainl1` powOp -- power is the next highest for order of parsing
    third = second `chainl1` mulOp -- multiplication and division are next
    in third `chainl1` addOp -- adiition and subtraction are last

  -- | Parses logarithms of type Float
  logOpF :: Parser (Expr Float) -> Parser (Expr Float)
  logOpF e1 = do { symbol "log"; b <- digits; spaces; p <- e1; return $ log' (read b) p }

  {- parsing integer -}

  -- | Parses an expression that contains constants of type Integer
  parseExprI :: String -> Expr Integer
  parseExprI ss = case parse exprI "" ss of
                    Left err   -> error $ show err
                    Right expr -> expr -- only parses if the expression is valid

  -- | Parses a constant of type Integer
  parseConstI :: Parser (Expr Integer)
  parseConstI = do { d <- digits;
                     return (Const (read d)) }

  -- | Parses in order of BEDMAS and mathematical expressions
  exprI :: Parser (Expr Integer)
  exprI = let -- parses in order: first, second, third so that it follows the order of BEDMAS
    first = (parens exprI) <|> otherOps (parens exprI) <|> logOpI (parens exprI) <|> (parseConstI <|> parseVar)
    negFirst = do { char '-'; --in case the expression has a negative in front of it
                     f <- first;
                     return $ Mult (Const (-1)) f }
    second = (first <|> negFirst) `chainl1` powOp -- power is the next highest for order of parsing
    third = second `chainl1` mulOp -- multiplication and division are next
    in third `chainl1` addOp -- addition and subtraction are last

  -- | Parses logarithms of type Integer
  logOpI :: Parser (Expr Integer) -> Parser (Expr Integer)
  logOpI e1 = do { symbol "log"; b <- digits; spaces; p <- e1; return $ log' (read b) p }

  {- parsing ints -}

  -- | Parses an expression that contains constants of type Int
  parseExprInt :: String -> Expr Int
  parseExprInt ss = case parse exprInt "" ss of
                    Left err   -> error $ show err
                    Right expr -> expr -- only parses if the expression is valid

  -- | Parses a constant of type Int
  parseConstInt :: Parser (Expr Int)
  parseConstInt = do { d <- digits;
                     return (Const (read d)) }

  -- | Parses in order of BEDMAS and mathematical expressions
  exprInt :: Parser (Expr Int)
  exprInt = let -- parses in order: first, second, third so that it follows the order of BEDMAS
    first = (parens exprInt) <|> otherOps (parens exprInt) <|> logOpInt (parens exprInt) <|> (parseConstInt <|> parseVar)
    negFirst = do { char '-'; --in case the expression has a negative in front of it
                     f <- first;
                     return $ Mult (Const (-1)) f }
    second = (first <|> negFirst) `chainl1` powOp -- power is the next highest for order of parsing
    third = second `chainl1` mulOp -- multiplication and division are next
    in third `chainl1` addOp -- adiition and subtraction are last

  -- | Parses logarithms of type Int
  logOpInt :: Parser (Expr Int) -> Parser (Expr Int)
  logOpInt e1 = do { symbol "log"; b <- digits; spaces; p <- e1; return $ log' (read b) p }

  {- General parsers -}

  -- | Parses a variable
  parseVar :: Parser (Expr a)
  parseVar = do { v <- many1 alphaNum;
                  return (Var v) }
  -- | Parses the exponent symbol
  powOp :: (AllNums a) => Parser (Expr a -> Expr a -> Expr a)
  powOp = do { symbol "^"; return (!^) }

  -- | Parses the multiplication or division symbols
  mulOp :: (AllNums a) => Parser (Expr a -> Expr a -> Expr a)
  mulOp = do { symbol "*"; return (!*) }
      <|> do { symbol "/"; return (!/) }

  -- | Parses the addition or subtraction symbols
  addOp :: (AllNums a) => Parser (Expr a -> Expr a -> Expr a)
  addOp = do { symbol "+"; return (!+) }
      <|> do { symbol "-"; return (!-) }

  -- | Parses mathematical expressions that are at the same level in BEDMAS
  otherOps :: (AllNums a) => Parser (Expr a) -> Parser (Expr a)
  otherOps e1 = do { symbol "exp"; p <- e1; return $ nPow p }
            <|> do { symbol "ln"; p <- e1; return $ ln p }
            <|> do { symbol "sin"; p <- e1; return $ sine p }
            <|> do { symbol "cos"; p <- e1; return $ cosine p }

  {- Helper functions -}

  -- *  Utility Parsers

  digits :: Parser String
  digits = many1 digit

  -- for negative numbers
  negDigits :: Parser String
  negDigits = do { symbol "-";
                   ds <- digits;
                   return ('-':ds) }

  decimalDigits :: Parser String
  decimalDigits = do { d <- symbol "." <|> string "";
                       n <- if d == "." then digits else string "";
                       return (d ++ n) }

  decimalNum :: Parser String
  decimalNum = do { w <- try negDigits <|> digits;
                    d <- try decimalDigits <|> return "";
                    return (w ++ d) }

  manyStrings :: String -> Parser [String]
  manyStrings ss = many (string ss)

  symbol :: String -> Parser String
  symbol ss = let
    symbol' :: Parser String
    symbol' = do { spaces;
                   ss' <- string ss;
                   spaces;
                   return ss' }
    in try symbol'

  parens :: Parser a -> Parser a
  parens p = do { symbol "(";
                  cs <- p;
                  symbol ")";
                  return cs }
