# Study Sheet on ELM

### Web Development
- Front End
  - Run by a browser
  - Scripting is done in **JavaScript**, and MarkUp in **HTML** and **CSS**
- Back End
  - Run by a server
  - Scripting is done in **PHP**, **Python**, etc., and Databases are in **SQL**, **MongoDB**, etc.
- ELM in Web Dev
  - Compiles to JavaScript, HTML and CSS, meaning it provides a unifed language
     - Complies to HTML by executing: `elm-make Main.elm --output Main.hmtl`
  - Built to resemble Haskell, be simple, and have great performance

### Basics
- Modules 
  - To define a program in Elm, you define a module
      ```elm
          module ModName exposing (..) 
       ``` 
       or
       ```elm 
          module ModName exposing (function)
       ```
   - Modules can also be imported in a similar manner
      ```elm 
         import ModName as MN
      ```
      - It is best practice to use the `as` keyword because of Elm's absence of **type classes**

- Differences between Elm and Haskell
  - No type classes
  - No `where` clause, only `let-in`
  - `data` becomes `type`
  - `type` becomes `type alias`
  - No guards or pattern matching, other than **case statements** 

## The Elm Architecture
- Init
  - Initialized the program
  - Provides information describing the initial state of the program
 
- Model
  - Definition of a data type that represents the state of the program

- View
  - Takes the **Model** and generates HTMl 

- Update
  - A function that takes the **Module** and generates a new, updated Model

- Main
  - There are many types of programs, for instance:
    ```elm
       main : Program Never Model Msg
       main = Html.beginnerProgram
              { model = init
              , view = view
              , update = update }
    ```
  - Other definitions add more functionality:
    ```elm
       main : Program Never Model Msg
       main = Html.program
              { init = init
              , view = view
              , update = update
              , subscriptions = subscriptions }
    ```


