# Study Sheet on Haskell

### Datatypes
- Enumeration Types
  - Method of representing different states with unique names
  - Ex. 
    ```haskell
       data TypeName = E1 | E2 | E3
         deriving (Show, Eq)
    ```

- Product Types
  - Wraps values of **predefined** types with a tag/value constructor to form a new type
  - Ex.
    ```haskell
       data TypeName = Tag P1 P2
         deriving Show
    ```

- Sum / Union Types
  - Uses the `|` operator to define multiple tagged values
  - Ex.
    ```haskell
       data TypeName = Tag T1 | Tag T2 
         deriving Show
    ```

- Parametric Types
  - Datatypes that have **type parameters** (arguments like in functions)
  - Ex.
    ```haskell
       data TypeName a b c = Tag a b c
         deriving Show
    ```

- Record Syntax
  - This provides a cleaner way of defining **getters**
     - Getter functions retrieve specific values
  - Ex.
    ```haskell
       data Thing = { property1 :: type, property2 :: type}
    ```

### Functors and Applicatives
- Functors
  - A class of **Mappable** datatypes
    - The `map` function can be defined over a functor
  - It is used to prevent having to unwrap and re-wrap values 
  - Ex.
  ```haskell
     instance Functor Maybe where
       fmap f (Just a) = Just (f a)
       fmap f Nothing  = Nothing
  ```
  - Functors have to obey ceratin laws
    - `fmap id = id`
    - `fmap (f . g) = fmap f . fmap g`
    - These aren't automatically enforced by Haskell

- Applicatives 
  - This class defines two functions over a datatype F
    - F **must** also be a Functor
  - Defined as
    ```haskell
       class Functor f => Applicative f where
         pure :: a -> f a
         (<*>) :: f (a -> b) -> f a -> f b
    ```
  - Ex.
    ```haskell
       instance Applicative Maybe where
         pure            = Just
         Nothing <*> _   = Nothing
         (Just f) <*>  x = fmap f x    
    ```
  - Applicatives also have to follow laws
    - `pure id <*> v = v`
    - `pure f <*> pure x = pure (f x)`
    - `u <*> pure y = pure ($ y) <*> u`
    - `pre (.) <*> u <*> v <*> w = u <*> (v <*> w)`

### Monoids and Monads    
- Monoids
  - This typeclass has associative binary operators with an identity
  - Ex. List are monoids
    ```haskell
       class Monoid [a] where
         mempty = []
         mappend = (++)
    ```
- Monads
  - Monads can be used to chain functions together, without haveing to repeatedly wrap and unwrap the values
  - Ex.
    ```haskell
       class Applicative => Monad m where
         (>>=) :: m a -> (a -> m b) -> m b
         (>>) :: m a -> m b -> m b
         return :: a -> m a
         fail :: String -> m a
  - IO values are only accessible through Functors and Monads because they contain **side-effects**

### Parsing
- Processing a String into a desired data structure
- Because Parsing may fail, the result is wrapped in a `Maybe` type
  - Monads are used to write parsers because they deal with the wrapping and unwrapping
  - Ex. 
    ```haskell
       parseTwoDigs :: String -> Maybe (Int, Int, String)
       parseTwoDigs ss = do { (d1, ss')  <- parseDigit ss
                              (d2, ss'') <- parseDigit ss'
                              return (d1, d2, ss'') }
    ```
- A better way to define a parser would be by wrapping it:
```haskell
   data Parser a = Parser (String -> Maybe (a, String))
```
  - To unwrap the function without doing it every time, a helper function can be written
  ```haskell
     parse :: Parser a -> String
     parse (Parse p) ss = p ss
  ```
- More complicated parsers are built out of smaller parsers, known as **parser combinators**
- Using Functors and Applicatives on Parsers are okay, but using Monads is best
```haskell
   instance MOnad Parser where
     p >>= f = Parser (\ss -> case parse p ss of
                               Just (a, ss') -> parse (f a) ss'
                               Nothing       -> Nothing )  
```
- Another useful class is called **Alternative**
```haskell
   instance Alternative Parser where 
    empty = zero
    p <|> a = Parser (\ss -> case parse p ss of
                               Nothing -> parse q ss
                               res     -> res )
```
- The Parsec Library
  - The definition of Parsec is a lot more complicated, but using it is much easier
  - Some important combinators:
     - `(<|>)` executes the second command only if the first fails without consuming any input
     - `try` allows you to execute a parser and pretend no input has been consumed
        - Used with `(<|>)`
     - `many` applies the parser zero or more times
     - `sepBy` applies the first parser separated by the second parser
     - `spaces` skips zero or more spaces
     - `char` parses the specified character or fails
     - `anyChar` parses any character or fails
     - `string` parses the specified string or fails
  - Some combinators that Parsec shoudl include but doesn't
    ```haskell
       symbol :: String -> Parser String
       symbol ss = do { spaces ; 
                        ss' <- string ss;
                        spaces; return ss'}
       
       parens :: Parser a -> Parser a
       parens p = do { char '(';
                       cs <- p;
                       char ')';
                       return cs }
      
       digits :: Parser Integer
       digits = many1 digit
     
       negDigits :: Parser String
       negDigits = do { neg <- symbol "-";
                        dig <- digits;
                        return (neg ++ dig) }
    
       integer :: Parser Integer
       integer = fmap read $ try negDigits <|> digits
    ``` 

### Expression Trees
- Binary Trees
  - Like a **Linked List** with two links at each node instead of one
  ```haskell
     data BinTree a = Node (BinTree a) (BinTree a)
                    | Leaf a 
  ```
- Multi-way Trees (Rose Trees)
  - These allow for an arbitrary number of branches at each node
  ```haskell
     data RoseTree a = RoseTree a [RoseTree a]
  ```
- Data Maps
  - These associate **key-value** pairs
  - A map can be constructed from a list of tuples
  - An efficient one is provided by `Data.Map.Strict`

- Expression Trees
  - These have generalized nodes (they're indistinguishable from each other)
  ```haskell
     data Expr a = Op1 (Expr a) (Expr a)
                 | Op2 (Expr a) (Expr a)
                 | Const a
  ```
  - These are what we used in Assignment 3

### Domain Specific Languages (DSLs)
- These are small languages that are focused on a particular aspect of a software system
   - Assignment 3 is a simple example

Created by Jessica de Leeuw
