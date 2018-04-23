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
  - Defined as:
    ```elm
       init : Model
       init = ...
    ```
- Model
  - Definition of a data type that represents the state of the program
  - Defined as:
    ```elm
       type alias Model =
    ```
- View
  - Takes the **Model** and generates HTMl 
  - Defined as:
    ```elm
       view : Model -> Html Msg
       view model = ...
    ```
- Update
  - A function that takes the **Module** and generates a new, updated Model
  - Defined as:
    ```elm
       update : Msg -> Model -> Model
       update msg model = ...
    ```
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
## Elm HTML
- Elm and HTML are very similar
```elm
   div [] [ 
           h1 [style [("color", "blue")] ] [ text "Hello World" ]
          ]
```
versus
```html
   <div>
     <h1 style="color:blue;">Hello World</h1>
   <div>
```
- Elm HTML Tags
  - `div` is just a generic container
  - `p` groups text into a paragraph
  - `pre` groups text and leaves it formatted as is
  - `b` is for bold, `sub` for subscript, `sup` for superscript 
  - `br` for a line break
  - `ol` for an ordered list, `ul` for an unordered list and `li` for an enumerated list
  - `img` for embedding an image, and `a` for creating a hyperlink
    - Ex.
      ```elm
         main = div [] [ 
                         h1 [] [text "Drunk Rick"] 
                       , img [src "www.somelink.com/drunkrick"] []
                       ]
      ```
  - **CSS** can also be used in Elm 
    - Ex.
      ```elm
         myStyle = [ ("backgroundColor", "red"), ("height", "90px") ] 
      ```
 

