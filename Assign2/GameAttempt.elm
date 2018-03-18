module GameAttempt exposing (..)

import Html exposing (..)
import Char exposing (..)
import Keyboard exposing (..)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame as Anim
import Mouse exposing (..)

type alias Model =  {x : Int, y : Int}
type Msg = KeyMsg Int
type alias Rect = {x : Int, y : Int, w : Int, h : Int}

inArea : Model -> Rect -> Bool
inArea model rec = if model.x > rec.x && model.x < (rec.x + rec.w) && model.y < rec.y && model.y > (rec.y + rec.h)
                   then
                    True
                   else
                    False

rect = { x = 475, y = 200, w = 300, h = 200}

init = ({x = 625, y = 550},Cmd.none)

view : Model -> Html.Html Msg
view model = let
       posX = toString model.x
       posY = toString model.y
    in div []
      [ svg [width "1250", height "640"]
            [circle [cx posX, cy posY, r "30", fill "blue"] []
            ,Svg.rect [x "475", y "200", width "300", height "200", fill "red"] [] ]
      ]

update : Msg -> Model -> (Model,Cmd Msg)
update (KeyMsg keyCode) model =
  if model.y < -20 || model.y == 640
  then
    (model, Cmd.none)
  else
    if model.x >= 475 && model.x <= 775 && model.y <= 400 && model.y >= 200
    then
      (model, Cmd.none)
    else
      case keyCode of
        40 -> ({ x = model.x, y = model.y + 10},Cmd.none) -- up arrow
        38 -> ({ x = model.x, y = model.y - 10},Cmd.none) -- down arrow
        39 -> ({ x = model.x + 10, y = model.y},Cmd.none) -- left arrow
        37 -> ({ x = model.x - 10, y = model.y},Cmd.none) -- right arrow
        _  -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Keyboard.presses KeyMsg

-- main : Program Never Model Msg
main = Html.program
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions }
