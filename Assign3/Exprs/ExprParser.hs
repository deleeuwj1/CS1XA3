{-# LANGUAGE FlexibleContexts #-}

module ExprParser where --(parseExprFloat, parseExprDouble, parseExprInt, parseExprInteger) where

  import Text.Parsec
  import Text.Parsec.String

  import ExprType

  {-|
  Module: ExprParser
  Description: Contains parsers for doubles and floats.
  Copyright: (c) [2018] [Jessica de Leeuw]
  Licence MIT Licence
  Maintainer: -}


  {-
  - We only will want to show the top level functions "parseExprDouble", "parseExprFloat", etc.
  - to do this we put this (parseExprFloat, , etc.) in the module declaration
  -}
  {- This section defines all required functions to parse expressions containing doubles -}

  parseExprDouble :: String -> Expr Double -- ^ the string to be parsed and a resulting expression
  parseExprDouble ss = case parse exprD "" ss of
                          Left err -> error $ "Parse Error " ++ show err
                          Right val -> val

  exprD :: Parser (Expr Double)
  exprD = undefined -- "TODO!"

  {-
   parseConstD :: Parser (Expr Double)
   parseConstD = do { symbol "val";
                   w <- try negDigits <|> digits;
                   symbol "." <|> symbol "";
                   d <- digits;
                   return (Const (w ++ d)) }
  -}

   {- This section defines all required functions to parse expressions containing Floats -}
   parseExprFloat :: String -> Expr Float
   parseExprFloat ss = case parse exprF "" ss of
                         Left err -> error $ "Parse Error " ++ show err
                         Right val -> val

   exprF :: Parser (Expr Float)
   exprF = undefined -- "TODO!"

   {- This section defines all required functions to parse expressions containing Integers -}
   parseExprInteger :: String -- ^ the string to be parsed
                  -> Expr Integer -- ^ the resulting expression
   parseExprInteger ss = case parse exprI "" ss of
                          Left err -> error $ "Parse Error " ++ show err
                          Right val -> val

   exprI :: Parser (Expr Integer)
   exprI = undefined -- "TODO!"

   parseConstI :: Parser (Expr Integer)
   parseConstI = do { symbol "val";
                     i <- try negDigits <|> digits;
                     return (Const i) }

   {- This section defines all required functions to parse expressions containing Ints -}
   parseExprInt:: String -- ^ the string to be parsed
                   -> Expr Int -- ^ the resulting expression
   parseExprInt ss = case parse exprD "" ss of
                          Left err -> error $ "Parse Error " ++ show err
                          Right val -> val

   exprInt :: Parser (Expr Int)
   exprInt = error "TODO!"

   parseConstInt :: Parser (Expr Int)
   parseConstInt = do { symbol "val";
                       i <- try negDigits <|> digits;
                       return (Const i) }

   {- General parsers -}
   parseVar :: Parser (Expr a)
   parseVar = do { symbol "var";
                  v <- manyStrings;
                  return (Var v)}

   basicOps :: Parser (Expr a -> Expr a -> Expr a)
   basicOps = do { symbol "!+"; return Add }
          <|> do { symbol "!-"; return Sub }
          <|> do { symbol "!*"; return Mult }
          <|> do { symbol "!/"; return Div }
          <|> do { symbol "!^"; return Pow }

   otherOps :: Parser (Expr a -> Expr a)
   otherOps = do { symbol "e"; return E }
          <|> do { symbol "ln"; return Ln }
          <|> do { symbol "sine"; return Sin }
          <|> do { symbol "cosine"; return Cos }

 {- Functions for all types -}

   digits :: Parser String
   digits = many1 digit

   -- for negative numbers
   negDigits :: Parser String
   negDigits = do { symbol "-";
                   ds <- digits;
                   return ('-':ds) }


   decimalDigits :: Parser String
   decimalDigits = do { d <- symbol ".";
                        return d }

   doubles :: Parser String
   doubles = do { w <- try negDigits <|> digits;
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
