import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)

type alias State = { position : (Float, Float)
                   , velocity : (Float, Float)
                   }

width = 800
height = 800
size = 20

main : Signal Element
main =
  let
    start = {position = (-50,0), velocity = (0.2,-0.1)}
    total = Signal.foldp bounce start (fps 60)
  in
    Signal.map ball total


ball : State -> Element
ball s =
  collage width height
    [ filled charcoal (circle size)
        |> move (fst s.position, snd s.position)
    ]


bounce : Time -> State -> State
bounce t s =
  let
    (newX, newVX) = updatePosition t (fst s.position, fst s.velocity) (width/2 - size)
    (newY, newVY) = updatePosition t (snd s.position, snd s.velocity) (height/2 - size)
  in
    { position = (newX, newY)
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
