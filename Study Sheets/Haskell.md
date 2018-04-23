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
#skell
       data TypeName = E1 | E2 | E3
         deriving (Show, Eq)
    ```
# Functors and Applicatives
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
    








