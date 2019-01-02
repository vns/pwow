module Main exposing (main)

import Browser
import Html
import Html.Attributes
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color exposing (..)
import List exposing (map, range, concat)


main =
    Browser.sandbox
        { init = \_ -> {}
        , update = \_ msg model -> {}
        , view = view
        }


gap : Collage msg
gap =
    spacer 20 10


rect : Color -> Float -> Collage msg
rect color len =
    rectangle len 10 |> filled (uniform color)


cantor : Float -> Int -> Collage msg
cantor len depth =
    case depth of
        0 ->
            rect blue len

        _ ->
            [ cantor (len / 3) (depth - 1)
            , spacer (len / 3) 0
            , cantor (len / 3) (depth - 1)
            ]
                |> horizontal
                |> center


view model =
    let
        row =
            \n ->
                [ gap
                , cantor 700 n
                ]
    in
        range 0 5
            |> map row
            |> concat
            |> vertical
            |> svg
