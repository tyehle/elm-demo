import Color exposing (..)
import Html exposing (Html, text)
import Html.App as App
import Collage exposing (Form, collage, circle, move, filled, rect)
import Element exposing (Element, toHtml)
import AnimationFrame
import Time exposing (Time)
import Window



-- MODEL --

type alias Ball = { position : (Float, Float)
                   , velocity : (Float, Float)
                   , size : Float
                   , color : Color
                   }

type alias Model = { balls: List Ball
                   , windowSize: Window.Size
                   }

start : Model
start = { balls = [ { position = (-50,0), velocity = (0.2,-0.1), size = 20, color = darkPurple }
                  , { position = (50,0),  velocity = (0.2,-0.4), size = 10, color = charcoal }
                  ]
        , windowSize = { width = 800, height = 800 }
        }

init : (Model, Cmd msg)
init = (start, Cmd.none)



-- UPDATE --

update : Msg -> Model -> (Model, Cmd Msg)
update msg state = case msg of
  Animate t -> 
    ( { state | balls = List.map (moveBall state.windowSize t) state.balls }
    , Cmd.none
    )
  
  Resize size ->
    ( { state | windowSize = size }
    , Cmd.none
    )

moveBall : Window.Size -> Time -> Ball -> Ball
moveBall {width, height} t s =
  let
    (newX, newVX) = updatePosition t (fst s.position, fst s.velocity) ((toFloat width)/2 - s.size)
    (newY, newVY) = updatePosition t (snd s.position, snd s.velocity) ((toFloat height)/2 - s.size)
  in
    { s | position = (newX, newY)
        , velocity = (newVX, newVY)
    }

updatePosition : Time -> (Float, Float) -> Float -> (Float, Float)
updatePosition t (x, v) bound =
  let
    projected = x + t*v
    extra     = (abs projected) - bound
    newV      = if extra <= 0 then v else -v
    newX      = if extra <= 0 then projected else projected + 2*newV*extra
  in
    (newX, newV)



-- SUBSCRIPTIONS --

type Msg = Animate Time | Resize Window.Size

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [AnimationFrame.diffs Animate, Window.resizes Resize]



-- VIEW --

view : Model -> Html msg
view {balls, windowSize} =
  let
    {width, height} = windowSize
    background = filled darkGrey (rect (toFloat width) (toFloat height))
    combine = toHtml << collage width height
  in
    combine (background :: (List.map drawCircle balls))

drawCircle : Ball -> Form
drawCircle ball =
  filled ball.color (circle ball.size)
    |> move (fst ball.position, snd ball.position)



-- MAIN --

main : Program Never
main = App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
