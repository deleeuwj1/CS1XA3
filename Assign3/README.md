# Assignment 3: Custom Haskell Math Library

Due Date: April 20th, 2018.

# Description
A Math library written in Haskell that is built to parse, simplify, evaluate and differentiate functions.
## Documentation
To view documentation, visit [this site](https://deleeuwj1.github.io/docs/).

## Functionalities
Accepts the following mathematical expressions:
```haskell
Const (num)                    -- inputted simply as a number, and is recognized as a Double, Float, Integer or Int
Var (string)                   -- any string not recognized as a mathematical expression is a variable  
Add (expression) (expression)  -- inputted as (expression) + (expression) 
Sub (expression) (expression)  -- inputted as (expression) - (expression)
Mult (expression) (expression) -- inputted as (expression) * (expression)
Div (expression) (expression)  -- inputted as (expression) / (expression)
E (expression)                 -- inputted as exp(expression)
Ln (expression)                -- inputted as ln(expression)
Cos (expression)               -- inputted as cos(expression)
Sin (expression)               -- inputted as sin(expression)
Pow (expression) (expression)  -- inputted as (expression) ^ (expression)
```
Example DSL use: 
`"x + (cos(-x) ^ 3)"` becomes: Add (Var "x") (Pow (Cos (Mult (Const (-1)) (Var "x"))) (Const 3))
`"ln (y / (4*5)) - exp(32 + z)"`bcomes Sub (Ln (Div (Var "y") (Mult (Const 4) (Const 5)))) (E (Add (Const 32) (Var "z")))

## References
  - Used [Allen Chen's](https://github.com/chenc118/CS1XA3/blob/master/Assign3/ExprDiff.hs) method of generalizing the `eval` and `simplify` functions, both located in `ExprEval.hs`. This allows `ExprEval` to work with numerical types such as `Double`, `Float`, `Integer` and `Int`.  

### License

This project is protected under the MIT License. For more information, see [LICENSE.md](https://github.com/deleeuwj1/CS1XA3/blob/master/Assign3/LICENSE.md).

