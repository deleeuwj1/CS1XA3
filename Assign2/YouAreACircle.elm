module YouAreACircle exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Char exposing (..)
import Keyboard exposing (..)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame as Anim
import Mouse exposing (..)
import List as List

----------------------------------------------------------------------

type Outcome = Playing | Lose
type alias Model =  {x : Int, y : Int, p : Outcome, l : Int}
type Msg = KeyMsg Int | ResetMsg
type alias Rect = {x : Int, y : Int, w : Int, h : Int, i : Bool }

----------------------------------------------------------------------

screenHeight = 635
screenWidth = 1200

menuStyle = Attr.style [ ("color", "#1C82F7"),
                         ("font-size","100px"),
                         ("text-align", "center")]

descriptionStyle = Attr.style [ ("color", "black"),
                                ("font-size","18px"),
                                ("text-align", "center")]


mainMenu : Model -> Html.Html Msg
mainMenu model = div [menuStyle]
                     [ p [menuStyle] [Html.text "Welcome!"]
                     , p [descriptionStyle] [Html.text "You are a circle in a rectangular world. Try your best to escape!"]
                     , p [descriptionStyle] [Html.text "Pro tip: the arrow keys help you run."]
                     , button [onClick ResetMsg] [Html.text "Let's Play"] ]

borderStyle = Attr.style [("border-style", "solid solid solid solid"), ("border-color", "#1C82F7")]

loseScreen : Model -> Html.Html Msg
loseScreen model =  div [menuStyle]
                        [ p [menuStyle] [Html.text "YOU LOST!"]
                        , p [descriptionStyle] [Html.text "Sucks to suck. The rectangles have eaten you alive."]
                        , button [onClick ResetMsg] [Html.text "Play Again"] ]

level1LoseScreen : Model -> Html.Html Msg
level1LoseScreen model =  div [menuStyle]
                              [ p [menuStyle] [Html.text "YOU LOST!"]
                              , p [descriptionStyle] [Html.text "Level 1? Really? I thought better of you!"]
                              , button [onClick ResetMsg] [Html.text "Play Again"] ]

level2LoseScreen : Model -> Html.Html Msg
level2LoseScreen model =  div [menuStyle]
                              [ p [menuStyle] [Html.text "YOU LOST!"]
                              , p [descriptionStyle] [Html.text "You were so close! (Yes, I'm just saying that to be nice.)"]
                              , button [onClick ResetMsg] [Html.text "Play Again"] ]

level3LoseScreen : Model -> Html.Html Msg
level3LoseScreen model =  div [menuStyle]
                              [ p [menuStyle] [Html.text "YOU LOST!"]
                              , p [descriptionStyle] [Html.text "Sucks to suck. The rectangles have eaten you alive."]
                              , button [onClick ResetMsg] [Html.text "Play Again"] ]

level4LoseScreen : Model -> Html.Html Msg
level4LoseScreen model =  div [menuStyle]
                              [ p [menuStyle] [Html.text "YOU LOST!"]
                              , p [descriptionStyle] [Html.text "You were so close! You can do this, I believe in you!"]
                              , button [onClick ResetMsg] [Html.text "Play Again"] ]

winScreen : Model -> Html.Html Msg
winScreen model = div [menuStyle]
                     [ p [menuStyle] [Html.text "YOU WON!"]
                     , p [descriptionStyle] [Html.text "You sigh with relief and hope you don't have to go through it again."]
                     , button [onClick ResetMsg] [Html.text "Play Again"]]

level1View : Model -> Html.Html Msg
level1View model = let
       posX = toString model.x
       posY = toString model.y
    in
     div [Attr.align "center"]
             [svg [width (toString screenWidth), height (toString screenHeight), Attr.align "center", borderStyle]
                 [Svg.circle [cx posX, cy posY, r "30", fill "blue"] []
                , Svg.rect [x "475", y "200", rx "5", ry "5", width "300", height "200", fill "red"] [] ]
            ]

level2View : Model -> Html.Html Msg
level2View model = let
       posX = toString model.x
       posY = toString model.y
    in
       div [Attr.align "center"]
           [ svg [width (toString screenWidth), height (toString screenHeight), Attr.align "center", borderStyle]
                 [ Svg.circle [cx posX, cy posY, r "30", fill "blue"] []
                 , rect [x "350", y "300", rx "5", ry "5", width "200", height "150", fill "green"] []
                 , rect [x "670", y "300", rx "5", ry "5", width "200", height "150", fill "green"] []
                 , rect [x "475", y "130", rx "5", ry "5", width "200", height "80", fill "green"] []
                 ]
           ]

level3View : Model -> Html.Html Msg
level3View model = let
      posX = toString model.x
      posY = toString model.y
  in
    div [Attr.align "center"]
        [ svg [width (toString screenWidth), height (toString screenHeight), Attr.align "center", borderStyle]
              [ Svg.circle [cx posX, cy posY, r "30", fill "blue"] []
              , rect [x "350", y "300", rx "5", ry "5", width "200", height "150", fill "blue"] []
              , rect [x "670", y "300", rx "5", ry "5", width "200", height "150", fill "blue"] []
              , rect [x "675", y "530", rx "5", ry "5", width "200", height "80", fill "blue"] []
              , rect [x "40", y "0", rx "5", ry "5", width "200", height "80", fill "blue"] []
              , rect [x "275", y "130", rx "5", ry "5", width "200", height "100", fill "blue"] []
              , rect [x "150", y "530", rx "5", ry "5", width "200", height "90", fill "blue"] []
              , rect [x "550", y "90", rx "5", ry "5", width "200", height "120", fill "blue"] []
              ]
        ]

level4View : Model -> Html.Html Msg
level4View model = let
    posX = toString model.x
    posY = toString model.y
  in
    div [Attr.align "center"]
        [ svg [width (toString screenWidth), height (toString screenHeight), borderStyle]
              [  Svg.circle [cx posX, cy posY, r "30", fill "blue"] []
              , rect [x "10", y "450", rx "5", ry "5", width "200", height "150", fill "red"] []
              , rect [x "170", y "220", rx "5", ry "5", width "200", height "150", fill "purple"] []
              , rect [x "305", y "470", rx "5", ry "5", width "200", height "100", fill "green"] []
              , rect [x "415", y "0", rx "5", ry "5", width "200", height "80", fill "blue"] []
              , rect [x "250", y "100", rx "5", ry "5", width "200", height "100", fill "yellow"] []
              , rect [x "670", y "530", rx "5", ry "5", width "200", height "90", fill "black"] []
              , rect [x "620", y "40", rx "5", ry "5", width "200", height "120", fill "orange"] []
              , rect [x "850", y "90", rx "5", ry "5", width "200", height "120", fill "pink"] []
              , rect [x "1000", y "290", rx "5", ry "5", width "200", height "120", fill "gray"] []
              , rect [x "600", y "360", rx "5", ry "5", width "300", height "120", fill "cyan"] []
              , rect [x "550", y "200", rx "5", ry "5", width "200", height "120", fill "magenta"] []
              ]
         ]

levelRects : Model -> List Rect
levelRects model =
  case model.l of
    1 -> [ { x = 475, y = 200, w = 300, h = 200, i = False } ]

    2 -> [ { x = 350, y = 300, w = 200, h = 150, i = False }
         , { x = 670, y = 300, w = 200, h = 150, i = False }
         , { x = 475, y = 130, w = 200, h = 80, i = False }
         ]

    3 -> [ { x = 350, y = 300, w = 200, h = 150, i = False }
         , { x = 670, y = 300, w = 200, h = 150, i = False }
         , { x = 675, y = 530, w = 200, h = 80, i = False }
         , { x = 40, y = 0, w = 200, h = 80, i = False }
         , { x = 275, y = 130, w = 200, h = 100, i = False }
         , { x = 150, y = 530, w = 200, h = 90, i = False }
         , { x = 550, y = 90, w = 200, h = 120, i = False }
         ]

    4 -> [ { x = 10, y = 450, w = 200, h = 150, i = False }
         , { x = 170, y = 220, w = 200, h = 150, i = False }
         , { x = 305, y = 470, w = 200, h = 100, i = False }
         , { x = 415, y = 0, w = 200, h = 80, i = False }
         , { x = 250, y = 100, w = 200, h = 100, i = False }
         , { x = 670, y = 530, w = 200, h = 90, i = False }
         , { x = 620, y = 40, w = 200, h = 120, i = False }
         , { x = 850, y = 90, w = 200, h = 120, i = False }
         , { x = 1000, y = 290, w = 200, h = 120, i = False }
         , { x = 600, y = 360, w = 300, h = 120, i = False }
         , { x = 550, y = 200, w = 200, h = 120, i = False }
         ]

    _ -> []


----------------------------------------------------------------------

--screenW = screen.availWidth
init = ({x = 610, y = 550, p = Playing, l = 0},Cmd.none)

----------------------------------------------------------------------

view : Model -> Html.Html Msg
view model =
      if model.p == Playing
      then
        case model.l of
          0 -> mainMenu model
          1 -> level1View model
          2 -> level2View model
          3 -> level3View model
          4 -> level4View model
          5 -> winScreen model
          _ -> mainMenu model
      else
        if model.p == Lose
        then
          case model.l of
            1 -> level1LoseScreen model
            2 -> level2LoseScreen model
            3 -> level3LoseScreen model
            4 -> level4LoseScreen model
            _ -> loseScreen model
        else mainMenu model
----------------------------------------------------------------------

elem = True

valInList : List a -> a -> Int -> Bool
valInList l elem pos =
  case l of
    []      -> False
    x :: xs ->
      if x == elem then True
      else valInList xs elem (pos + 1)

inRectsArea : (List Rect) -> Model -> Bool
inRectsArea l model = (valInList (List.map (inRect model) l ) elem 0)

inRect : Model -> Rect -> Bool
inRect model rec = (model.x >= (rec.x - 30)) && (model.x <= (rec.x + rec.w) + 30) && (model.y <= (rec.y + rec.h) + 30) && (model.y >= rec.y - 30)

----------------------------------------------------------------------

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    (KeyMsg i) -> keyMsgUpdate i model
    ResetMsg  -> ({ x = 625, y = 550, p = Playing, l = 1}, Cmd.none)

keyMsgUpdate : Int -> Model -> (Model, Cmd Msg)
keyMsgUpdate keyCode model =
  if model.y < -18
  then
    ({ x = 610, y = 550, p = Playing, l = (model.l + 1)},Cmd.none)
  else
    if model.y == (screenHeight - 25)
    then
      ({ x = model.x, y = model.y - 10, p = Playing, l = model.l},Cmd.none)
    else
      if model.x <= 28
      then
        ({ x = model.x + 10, y = model.y, p = Playing, l = model.l},Cmd.none)
      else
        if model.x >= (screenWidth - 20)
        then
          ({ x = model.x - 10, y = model.y, p = Playing, l = model.l},Cmd.none)
        else
          if inRectsArea (levelRects model) model
          then
            ({ x = model.x, y = model.y, p = Lose, l = model.l},Cmd.none)
          else
            case keyCode of
              40 -> ({ x = model.x, y = model.y + 10, p = Playing, l = model.l},Cmd.none) -- up arrow
              38 -> ({ x = model.x, y = model.y - 10, p = Playing, l = model.l},Cmd.none) -- down arrow
              39 -> ({ x = model.x + 10, y = model.y, p = Playing, l = model.l},Cmd.none) -- left arrow
              37 -> ({ x = model.x - 10, y = model.y, p = Playing, l = model.l},Cmd.none) -- right arrow
              _  -> (model, Cmd.none)

----------------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model = Keyboard.downs KeyMsg

----------------------------------------------------------------------

main = Html.program
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions }
