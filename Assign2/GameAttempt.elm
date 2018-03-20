module GameAttempt exposing (..)

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


type Outcome = Win | Playing | Lose
type alias Model =  {x : Int, y : Int, p : Outcome}
type Msg = KeyMsg Int | ResetMsg
type alias Rect = {x : Int, y : Int, w : Int, h : Int}

outcomeStyle = Attr.style [ ("color", "#1C82F7"),
                            ("font-size","100px"),
                            ("text-align", "center"),
                            ("padding-top", "200px") ]

inArea : Model -> Rect -> Bool
inArea model rec = if model.x > rec.x && model.x < (rec.x + rec.w) && model.y < rec.y && model.y > (rec.y + rec.h)
                   then
                    True
                   else
                    False

init = ({x = 625, y = 550, p = Playing},Cmd.none)

view : Model -> Html.Html Msg
view model = let
       posX = toString model.x
       posY = toString model.y
       play = model.p
    in
      if model.p == Playing
      then
        div []
            [ svg [width "1250", height "640"]
                  [circle [cx posX, cy posY, r "30", fill "blue"] []
                  ,Svg.rect [x "475", y "200", rx "20", ry "20", width "300", height "200", fill "red"] [] ]
            ]
      else
        if model.p == Win
        then
          div [outcomeStyle] [Html.text "YOU WON"
                    ,        br [] []
                    , button [onClick ResetMsg] [Html.text "Play Again"]]
        else
          div [outcomeStyle] [Html.text "YOU LOST"
                    ,        br [] []
                    , button [onClick ResetMsg] [Html.text "Play Again"] ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    (KeyMsg i) -> keyMsgUpdate i model
    ResetMsg  -> ({ x = 625, y = 550, p = Playing}, Cmd.none)

keyMsgUpdate : Int -> Model -> (Model, Cmd Msg)
keyMsgUpdate keyCode model =
  if model.y < -18
  then
    ({ x = model.x, y = model.y, p = Win},Cmd.none)
  else
    if model.y == 640
    then
      ({ x = model.x, y = model.y - 10, p = Playing},Cmd.none)
    else
      if model.x >= 455 && model.x <= 790 && model.y <= 429 && model.y >= 175
      then
        ({ x = model.x, y = model.y, p = Lose},Cmd.none)
      else
        case keyCode of
          40 -> ({ x = model.x, y = model.y + 10, p = Playing},Cmd.none) -- up arrow
          38 -> ({ x = model.x, y = model.y - 10, p = Playing},Cmd.none) -- down arrow
          39 -> ({ x = model.x + 10, y = model.y, p = Playing},Cmd.none) -- left arrow
          37 -> ({ x = model.x - 10, y = model.y, p = Playing},Cmd.none) -- right arrow
          _  -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Keyboard.presses KeyMsg

-- main : Program Never Model Msg
main = Html.program
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions }
