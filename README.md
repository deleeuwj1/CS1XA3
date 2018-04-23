# Jessica's COMPSCI 1XA3 Repository
Course: COMPSCI 1XA3

Instructor: Curtis D'Alves

Session: Winter 2018

## Assign1
 - This directory contains ProjectAnalyze.sh and a README.md file describing how it is used.
 - ProjectAnalyze.sh is a script that performs the requirements of the assignment:
    1. Checks if your remote and local repositories are up to date.
    2. Puts all uncommitted changes in a file **changes.log**.
    3. Puts all lines containing the tag `#TODO` into a file **todo.log**.
    4. Checks all Haskell files for errors and puts the output into a file **error.log**.
    5. Performs a variety of other functionalities.

## Assign2
  - This directory contains code for a simple game created in ELM.
     - You are a circle in a rectangular world. Try your best to escape!
     - It was created to demonstrate my knowledge of the ELM architure that was taught in class. 
  - It also contains the HTML code for my resume.
     - The README.md file contains a link to my resume. Feel free to check it out. 
     - It is a modified version of an existing template, credits are given in the README.md file.

## Assign3
  - This directory contains the modules that form a simple math library written in Haskell
  - It contains: `ExprType.hs`, `ExprEval.hs`, `ExprDiff.hs`, `ExprParser.hs`, `ExprPretty.hs` and `ExprTest.hs`
     - It accepts a variety of expressions, including:
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
     - For more information on usage, see the [README.md](https://github.com/deleeuwj1/CS1XA3/blob/master/Assign3/README.md) in the Assign3 folder.
  - If you would like to view documentation for the library, visit [this site](https://deleeuwj1.github.io/docs/).

## Labs
  - This directory contains subdirectories for each of the marked labs.
     - These contain encrypted files with answers for the in-class labs.

## Study Sheet
  - This directory contains three study sheets, one for each of the topics covered in CS 1XA3
     - `Bash.md`
     - `Elm.md`
     - `Haskell.md`
