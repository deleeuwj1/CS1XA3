module ExprParser (parseExprD,parseExprF) where

  import ExprType

  import Text.Parsec
  import Text.Parsec.String

  {- parsing doubles -}
  parseExprD :: String -> Expr Double
  parseExprD ss = case parse exprD "" ss of
                    Left err   -> error $ show err
                    Right expr -> expr

  parseConstD :: Parser (Expr Double)
  parseConstD = do { d <- decimalNum;
                     return $ Const (read d) }

  exprD :: Parser (Expr Double)
  exprD = let
    first = (parens exprD) <|> otherOps (parens exprD) <|> logOp <|> (parseVar <|> parseConstD)
    negFirst = do { char '-';
                     f <- first;
                     return $ Mult (Const (-1)) first }
    second = (first <|> negFirst) `chainl` powOp
    third = second `chainl` mulOp
    in third `chainl` addOp

  {- parsing floats -}
  parseExprF :: String -> Expr Float
  parseExprF ss = case parse exprF "" ss of
                    Left err   -> error $ show err
                    Right expr -> expr

  parseConstF :: Parser (Expr Float)
  parseConstF = do { d <- decimalNum;
                     return (Const (read d)) }

  exprF :: Parser (Expr Float)
  exprF = let
    first = (parens exprF) <|> otherOps (parens exprF) <|> logOp <|> (parseVar <|> parseConstF)
    negFirst = do { char '-';
                     f <- first;
                     return $ Mult (Const (-1)) first }
    second = (first <|> negFirst) `chainl` powOp
    third = second `chainl` mulOp
    in third `chainl` addOp

  {- parsing integer -}
  parseExprI :: String -> Expr Integer
  parseExprI ss = case parse exprI "" ss of
                    Left err   -> error $ show err
                    Right expr -> expr

  parseConstI :: Parser (Expr Integer)
  parseConstI = do { d <- digits;
                     return (Const (read d)) }

  exprI :: Parser (Expr Integer)
  exprI = let
    first = (parens exprI) <|> otherOps (parens exprI) <|> logOp <|> (parseVar <|> parseConstI)
    negFirst = do { char '-';
                     f <- first;
                     return $ Mult (Const (-1)) first }
    second = (first <|> negFirst) `chainl` powOp
    third = second `chainl` mulOp
    in third `chainl` addOp

  {- General parsers -}

  parseVar :: Parser (Expr a)
  parseVar = do { v <- many1 alphaNum;
                  return (Var v) }

  powOp :: Parser (Expr a -> Expr a -> Expr a)
  powOp = do { symbol "^"; return (!^) }

  mulOp :: Parser (Expr a -> Expr a -> Expr a)
  mulOp = do { symbol "*"; return (!*) }
      <|> do { symbol "/"; return (!/) }

  addOp :: Parser (Expr a -> Expr a -> Expr a)
  addOp = do { symbol "+"; return (!+) }
      <|> do { symbol "-"; return (!-) }

  otherOps :: Parser (Expr a) -> Parser (Expr a)
  otherOps e1 = do { symbol "e"; p <- e1; return $ E p }
            <|> do { symbol "ln"; p <- e1; return $ Ln p }
            <|> do { symbol "sin"; p <- e1; return $ Sin p }
            <|> do { symbol "cos"; p <- e1; return $ Cos p }

  logOp :: Parser (Expr a) -> Parser (Expr a)
  logOp e1 = do { symbol "log"; b <- digits; spaces; p <- e1; return $ Log b p }

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
