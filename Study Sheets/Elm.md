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
- Messages
  - These define things that happen in the application that the components respond to
  - Defined as:
    ```elm
       type Msg = Something 
                | SomethingElse
                | MaybeAnotherThing
    ```

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
  - Takes the **Model** and generates HTML 
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
- Subscriptions
  - These allow the program to listen for **external output** such as Keyboard and Mouse events
  - Defined as:
    ```elm
       subscriptions : Model -> Sub Msg
       subscriptions model = ...
    ```
  - Subscriptions can be **batched**, or combined
    - This means that they can listen for multiple events in one subscription
    - Ex.
      ```elm
         subscriptions model = Sub.batch
             [ Mouse.clicks MouseMsg
             , Keyboard.downs KeyMsg ]
      ``` 
- Commands
   - These are used to execute things that involve **side effects**
   - Ex. Random Number Generation, HTTP requests, saving to local storage

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
- Elm Buttons
  - Buttons should include an event from `Html.Event`
  - They are defined like this:
    ```elm
       main = div [] [ button [onClick Msg] [text "Label"] ]  
    ```

## SVG Graphics
- Record Types
  - This is a unique structure to Elm
  - Ex. 
    ```elm
       type alias Pos = { x : Int, y : Int }
       pos = { x = 5, y = 3 }
    ```  
- SVG Graphics
  - This library contains a large variety of shapes, as well as other functionalities
  - Can be used like so: 
    ```elm
       view model = svg [ width "600", height "400" ] 
                        [ circle [cx "300", cy "300", r "20", fill "blue" ] [] ]
    ```
Created by Jessica de Leeuw


