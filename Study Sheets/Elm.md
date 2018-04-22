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
     - '''elm
          module ModName exposing (..) 
       ''' 

