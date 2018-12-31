module Main exposing (main)

import Browser
import Html
import Html.Attributes
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color exposing (..)


main =
    Browser.sandbox
        { init = \_ -> {}
        , update = \_ msg model -> {}
        , view = view
        }


gap : Collage msg
gap =
    spacer 20 10


hline : Color -> Float -> Collage msg
hline color len =
    rectangle len 10 |> filled (uniform blue)


cantor : Float -> Int -> Collage msg
cantor len depth =
    case depth of
        0 ->
            hline black len

        _ ->
            let
                aLine =
                    cantor (len / 3) (depth - 1)
            in
                [ aLine
                , spacer (len / 3) 0
                , aLine
                ]
                    |> horizontal
                    |> center


view model =
    let
        cantorRow =
            \n ->
                [ gap
                , cantor 700 n
                ]
    in
        List.range 0 5
            |> List.map cantorRow
            |> List.concat
            |> vertical
            |> svg
