# Assignment 3: Custom Haskell Math Library

Due Date: April 20th, 2018.

## Description
A Math library written in Haskell that is built to parse, simplify, evaluate and differentiate functions.
It includes the modules: 
- `ExprType` which contains the datatype definition
- `ExprEval` which contains functions that simplify and evaluate expressions
- `ExprDiff` which contains a function that partially differentiates a given expression
- `ExprParser` which parses certain strings into the `Expr` type
- `ExprPretty` which outputs the expressions in a readable format
- `ExprTest` which is used to test the accuracy and functionality of the library

## Documentation and Installation
To use `ExprTest.hs`, installation of the `generic-random` package is required. This is done by running 
`cabal install generic-random`
in the folder that contains the `Expr` modules.

To view thorough Haddock documentation, visit [this site](https://deleeuwj1.github.io/docs/).

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
Log (num) (expression)         -- inputted as log(base) (expression), where base is a number
Cos (expression)               -- inputted as cos(expression)
Sin (expression)               -- inputted as sin(expression)
Pow (expression) (expression)  -- inputted as (expression) ^ (expression)
```
See section ExprParser for more detail and for examples.
## Usage of the Modules

### ExprEval
- `eval`
    - Evaluates expressions by determining if there are exceptions or errors in the expression. If there aren't, `eval` matches a variable to it's value and evaluates the expression for the given values.
    - It takes a list of tuples that contain a variable and it's corresponding value.
       - Ex. `[("x", 12), ("y", 13.5), ("z", 0)]`

- `simplify`
   - Partially or fully simplifies an expression.
   - Ex. `(x + x) * 3` becomes `6 * x`
   - Ex. `ln(1 / x)` becomes `(-1) * (ln(x))`

### ExprParser
- `exprParseD`, `exprParseF`, `exprParseI` and `exprParseInt` each parse in an identical manner, but cover different numerical types
   - These parse expressions into the `Expr` type.
   - Ex. `"x + (cos(-x) ^ 3)"` becomes: `Add (Var "x") (Pow (Cos (Mult (Const (-1)) (Var "x"))) (Const 3))`
   - Ex. `"ln (y / (4*5)) - exp(32 + z)"` becomes: `Sub (Ln (Div (Var "y") (Mult (Const 4) (Const 5)))) (E (Add (Const 32) (Var "z")))`
   - Ex. `log24(sin(x+3))` is the same as: `Log 24 (Sin (Add (Var "x") (Const 3))`
      - Note that the input for the base is attached to the word log. To input log of base 10, it must be written as `log10(x)`.

### ExprDiff
- partDiff partially differentiates a variable based on the variable the user chooses to differentiate by.
   - Ex. `partDiff "x" (parseExprD "2 * x")` becomes: `2`
   - Ex. `partDiff "y" (parseExprD "sin(2 * x * y)")` becomes `2*x*(cos(2*x*y))`

## Extra Features
- Subtraction (Sub)
- Division (Div)
- Exponents (Pow)
- Logarithm of any base (Log a)

## References
  - Used [Allen Chen's](https://github.com/chenc118/CS1XA3/blob/master/Assign3/ExprDiff.hs) method of generalizing the `eval` and `simplify` functions, both located in `ExprEval.hs`. This allows `ExprEval` to work with numerical types such as `Double`, `Float`, `Integer` and `Int`.  

### License
This project is protected under the MIT License. For more information, see [LICENSE.md](https://github.com/deleeuwj1/CS1XA3/blob/master/Assign3/LICENSE.md).

