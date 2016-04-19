import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)


main : Signal Element
main =
  Signal.map clock (every (second / 60))


clock : Float -> Element
clock t =
  collage 800 800
    [ filled lightGrey (ngon 64 220)
    , outlined (solid grey) (ngon 64 220)
    , hand orange 200 t
    , hand charcoal 200 (t/60)
    , hand charcoal 120 (t/720)
    ]


hand : Color -> Float -> Float -> Form
hand clr len time =
  let
    angle = degrees (90 - 6 * inSeconds time)
  in
    segment (0,0) (fromPolar (len,angle))
      |> traced (solid clr)