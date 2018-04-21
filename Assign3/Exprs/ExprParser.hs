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
  parseExprD :: String -> Expr Double
  parseExprD ss = case parse exprD "" ss of
                    Left err   -> error $ show err
                    Right expr -> expr -- only parses if the expression is valid

  parseConstD :: Parser (Expr Double)
  parseConstD = do { d <- decimalNum;
                     return $ Const (read d) }

  exprD :: Parser (Expr Double)
  exprD = let -- parses in order: first, second, third so that it follows the order of BEDMAS
    first = (parens exprD) <|> otherOps (parens exprD) <|> logOpD (parens exprD) <|> (parseConstD <|> parseVar)
    negFirst = do { char '-'; --in case the expression has a negative in front of it
                     f <- first;
                     return $ Mult (Const (-1)) f }
    second = (first <|> negFirst) `chainl1` powOp
    third = second `chainl1` mulOp
    in third `chainl1` addOp

  logOpD :: Parser (Expr Double) -> Parser (Expr Double)
  logOpD e1 = do { symbol "log"; b <- digits; spaces; p <- e1; return $ log' (read b) p }

  {- parsing floats -}
  parseExprF :: String -> Expr Float
  parseExprF ss = case parse exprF "" ss of
                    Left err   -> error $ show err
                    Right expr -> expr -- only parses if the expression is valid

  parseConstF :: Parser (Expr Float)
  parseConstF = do { d <- decimalNum;
                     return (Const (read d)) }

  exprF :: Parser (Expr Float)
  exprF = let -- parses in order: first, second, third so that it follows the order of BEDMAS
    first = (parens exprF) <|> otherOps (parens exprF) <|> logOpF (parens exprF) <|> (parseConstF <|> parseVar)
    negFirst = do { char '-';
                     f <- first;
                     return $ Mult (Const (-1)) f }
    second = (first <|> negFirst) `chainl1` powOp
    third = second `chainl1` mulOp
    in third `chainl1` addOp

  logOpF :: Parser (Expr Float) -> Parser (Expr Float)
  logOpF e1 = do { symbol "log"; b <- digits; spaces; p <- e1; return $ log' (read b) p }

  {- parsing integer -}
  parseExprI :: String -> Expr Integer
  parseExprI ss = case parse exprI "" ss of
                    Left err   -> error $ show err
                    Right expr -> expr -- only parses if the expression is valid

  parseConstI :: Parser (Expr Integer)
  parseConstI = do { d <- digits;
                     return (Const (read d)) }

  exprI :: Parser (Expr Integer)
  exprI = let -- parses in order: first, second, third so that it follows the order of BEDMAS
    first = (parens exprI) <|> otherOps (parens exprI) <|> logOpI (parens exprI) <|> (parseConstI <|> parseVar)
    negFirst = do { char '-';
                     f <- first;
                     return $ Mult (Const (-1)) f }
    second = (first <|> negFirst) `chainl1` powOp
    third = second `chainl1` mulOp
    in third `chainl1` addOp

  logOpI :: Parser (Expr Integer) -> Parser (Expr Integer)
  logOpI e1 = do { symbol "log"; b <- digits; spaces; p <- e1; return $ log' (read b) p }

  {- parsing ints -}
  parseExprInt :: String -> Expr Integer
  parseExprInt ss = case parse exprI "" ss of
                    Left err   -> error $ show err
                    Right expr -> expr -- only parses if the expression is valid

  parseConstInt :: Parser (Expr Integer)
  parseConstInt = do { d <- digits;
                     return (Const (read d)) }

  exprInt :: Parser (Expr Integer)
  exprInt = let -- parses in order: first, second, third so that it follows the order of BEDMAS
    first = (parens exprI) <|> otherOps (parens exprI) <|> logOpI (parens exprI) <|> (parseConstI <|> parseVar)
    negFirst = do { char '-';
                     f <- first;
                     return $ Mult (Const (-1)) f }
    second = (first <|> negFirst) `chainl1` powOp
    third = second `chainl1` mulOp
    in third `chainl1` addOp

  logOpInt :: Parser (Expr Integer) -> Parser (Expr Integer)
  logOpInt e1 = do { symbol "log"; b <- digits; spaces; p <- e1; return $ log' (read b) p }

  {- General parsers -}

  parseVar :: Parser (Expr a)
  parseVar = do { v <- many1 alphaNum;
                  return (Var v) }

  powOp :: (AllNums a) => Parser (Expr a -> Expr a -> Expr a)
  powOp = do { symbol "^"; return (!^) }

  mulOp :: (AllNums a) => Parser (Expr a -> Expr a -> Expr a)
  mulOp = do { symbol "*"; return (!*) }
      <|> do { symbol "/"; return (!/) }

  addOp :: (AllNums a) => Parser (Expr a -> Expr a -> Expr a)
  addOp = do { symbol "+"; return (!+) }
      <|> do { symbol "-"; return (!-) }

  otherOps :: (AllNums a) => Parser (Expr a) -> Parser (Expr a)
  otherOps e1 = do { symbol "exp"; p <- e1; return $ e p }
            <|> do { symbol "ln"; p <- e1; return $ ln p }
            <|> do { symbol "sin"; p <- e1; return $ sine p }
            <|> do { symbol "cos"; p <- e1; return $ cosine p }

  {- Functions for all types -}

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
