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
## Functors and Applicatives

