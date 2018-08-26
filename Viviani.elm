module Viviani exposing (main)

import Browser
import Color.Palette exposing (darkYellow, purple, white)
import Html exposing (Html, div, h1, span)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Animation as Anim
import List exposing (map, range)
import Debug


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { foo : Anim.State
    }


type Msg
    = Animate Anim.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( { foo = Anim.style [ Anim.fill white ] }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Anim.subscription Animate
        [ model.foo ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate animMsg ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ Attr.style "margin" "200px auto"
        , Attr.style "width" "700px"
        , Attr.style "height" "750px"
        ]
        [ h1 [ Attr.style "cursor" "pointer" ]
            []
        , mainCanvas model
        ]


type alias Point =
    { x : Float
    , y : Float
    }


type alias Vector =
    Point


dot : Vector -> Vector -> Float
dot v1 v2 =
    v1.x * v2.x + v1.y * v2.y


vector : Point -> Point -> Vector
vector e1 e2 =
    { x = e1.x - e2.x, y = e1.y - e2.y }


norm : Vector -> Vector
norm v =
    { x = v.x / (sqrt (dot v v)), y = v.y / (sqrt (dot v v)) }


proj : Point -> Point -> Point -> Point
proj e1 e2 p =
    let
        v =
            vector e1 e2

        x =
            vector p e2

        c =
            (dot x v) / (dot v v)
    in
        { x = e2.x + c * v.x, y = e2.y + c * v.y }


dist : Point -> Point -> Float
dist p1 p2 =
    let
        dx =
            p2.x - p1.x

        dy =
            p2.y - p1.y
    in
        sqrt (dx * dx + dy * dy)


equilateral : Point -> Point -> Point -> List Point
equilateral p e1 e2 =
    let
        pr =
            proj e1 e2 p

        halfSide =
            dist pr p
                |> (*) (tan (degrees 30))

        u =
            vector pr e2
                |> norm
    in
        [ p
        , { x = pr.x + u.x * halfSide, y = pr.y + u.y * halfSide }
        , { x = pr.x - u.x * halfSide, y = pr.y - u.y * halfSide }
        ]


mainCanvas : Model -> Html Msg
mainCanvas model =
    let
        aPoint =
            { x = 40, y = 70 }

        prRight =
            proj { x = 50, y = 15 } { x = 100, y = 100 } aPoint

        prDown =
            proj { x = 0, y = 100 } { x = 100, y = 100 } aPoint

        v1 =
            { x = 50, y = 15 }

        v2 =
            { x = 0, y = 100 }

        equi =
            equilateral aPoint v1 v2
                |> map (\p -> String.fromFloat (p.x) ++ "," ++ String.fromFloat (p.y))
                |> String.join " "

        prLeft =
            proj v1 v2 aPoint
    in
        div []
            [ svg
                [ version "1.1", width "700", height "700", viewBox "0 0 150 150" ]
                [ g [ transform "translate(25,25)" ]
                    [ polygon [ points "50,15 100,100 0,100", fill "none", stroke "black", strokeWidth "0.2" ] []
                    , line [ x1 "50", y1 "15", x2 "50", y2 "100", stroke "blue", strokeWidth "0.1" ] []
                    , circle [ cx "40", cy "70", r "0.5", fill "black" ] []
                    , circle [ cx (String.fromFloat prLeft.x), cy (String.fromFloat prLeft.y), r "0.5", fill "black" ] []
                    , circle [ cx (String.fromFloat prRight.x), cy (String.fromFloat prRight.y), r "0.5", fill "black" ] []
                    , circle [ cx (String.fromFloat prDown.x), cy (String.fromFloat prDown.y), r "0.5", fill "black" ] []
                    , polygon [ points equi, fill "none", strokeWidth "0.2", stroke "black" ] []
                    ]
                ]
            ]
