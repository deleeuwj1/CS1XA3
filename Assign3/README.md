# Assignment 3: Custom Haskell Math Library

Due Date: April 20th, 2018.

# Description


## Functionalities
Accepts the following mathematical expressions:
```haskell
Const (num)                    -- inputted simply as a number, and is recognized as a Double, Float, Integer or an Int
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
```haskell
"x + (cos(-x) ^ 3)"
"ln (y / (4*5)) - exp(32 + z)"
```

## References
  - Modified [Allen Chen's](https://github.com/chenc118/CS1XA3/blob/master/Assign3/ExprDiff.hs) method of generalizing the `eval` and `simplify` functions, both located in `ExprEval.hs`. This will allows `ExprEval` to work with numerical types such as `Double`, `Float`, `Integer` and `Int`.  
 

### License

This project is protected under the MIT License. For more information, see [LICENSE.md](https://github.com/deleeuwj1/CS1XA3/blob/master/Assign3/LICENSE.md).

### Doumentation
To view documentation, visit [this site](https://deleeuwj1.github.io/docs/).
