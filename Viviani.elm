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
    Sub.batch
        [ Anim.subscription Animate
            [ model.foo ]
        ]


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


type alias Line =
    { start : Point
    , end : Point
    }


type alias Equilateral =
    { a : Point
    , b : Point
    , c : Point
    }


dot : Vector -> Vector -> Float
dot v1 v2 =
    v1.x * v2.x + v1.y * v2.y


vector : Point -> Point -> Vector
vector e1 e2 =
    { x = e1.x - e2.x, y = e1.y - e2.y }


norm : Vector -> Vector
norm v =
    { x = v.x / (sqrt (dot v v)), y = v.y / (sqrt (dot v v)) }


projection : Line -> Point -> Point
projection line point =
    let
        v =
            vector line.start line.end

        x =
            vector point line.end

        c =
            (dot x v) / (dot v v)
    in
        { x = line.end.x + c * v.x, y = line.end.y + c * v.y }


dist : Point -> Point -> Float
dist p1 p2 =
    let
        dx =
            p2.x - p1.x

        dy =
            p2.y - p1.y
    in
        sqrt (dx * dx + dy * dy)


equilateral : Line -> Point -> Equilateral
equilateral line point =
    let
        proj =
            projection line point

        halfSide =
            dist proj point
                |> (*) (tan (degrees 30))

        u =
            vector proj line.end
                |> norm
    in
        { c = point
        , b = { x = proj.x + u.x * halfSide, y = proj.y + u.y * halfSide }
        , a = { x = proj.x - u.x * halfSide, y = proj.y - u.y * halfSide }
        }


equilateralFromLine : Line -> Equilateral
equilateralFromLine { start, end } =
    let
        normal =
            vector start end
                |> norm
                |> Debug.log "norm"

        side =
            dist start end
                |> Debug.log "side"

        height =
            (sqrt (side * side - (side * side) / 4))
                |> Debug.log "height"

        centerX =
            (if start.x > end.x then
                end.x + (start.x - end.x) / 2
             else if start.x < end.x then
                start.x + (end.x - start.x) / 2
             else
                start.x
            )
                |> Debug.log "centerX"

        centerY =
            (if start.y > end.y then
                end.y + (start.y - end.y) / 2
             else if start.y < end.y then
                start.y + (end.y - start.y) / 2
             else
                start.y
            )
                |> Debug.log "centerY"

        apex =
            { x = centerX - height * normal.y
            , y = centerY + height * normal.x
            }
                |> Debug.log "apex"
    in
        { a = start, b = end, c = apex }


equilateralAsString : Equilateral -> String
equilateralAsString eq =
    String.fromFloat (eq.a.x)
        ++ ","
        ++ String.fromFloat (eq.a.y)
        ++ " "
        ++ String.fromFloat (eq.b.x)
        ++ ","
        ++ String.fromFloat (eq.b.y)
        ++ " "
        ++ String.fromFloat (eq.c.x)
        ++ ","
        ++ String.fromFloat (eq.c.y)
        ++ " "


mainCanvas : Model -> Html Msg
mainCanvas model =
    let
        aPoint =
            { x = 300, y = 400 }

        v1 =
            { x = 10, y = 740 }

        v2 =
            { x = 740, y = 740 }

        eq =
            equilateralFromLine { start = v1, end = v2 }

        eqBC =
            equilateral { start = eq.b, end = eq.c } aPoint
                |> Debug.log "BC"

        eqAC =
            equilateral { start = eq.a, end = eq.c } aPoint
                |> Debug.log "AC"

        eqUpper =
            equilateral { start = eqAC.b, end = eqBC.b } eq.c

        -- Test things
        half =
            (eqBC.b.x - eqAC.b.x) / 2

        x =
            half * (tan (degrees 30))

        centerX =
            eqAC.b.x + half

        centerY =
            eqAC.b.y

        normal =
            vector eqAC.b eqBC.b |> norm

        origin =
            { x = centerX - x * normal.y
            , y = centerY + x * normal.x
            }

        originAsString =
            "transform-origin: " ++ String.fromFloat (origin.x) ++ "px " ++ String.fromFloat (origin.y) ++ "px"

        eqAB =
            equilateral { start = eq.a, end = eq.b } aPoint

        prLeft =
            projection { start = eq.a, end = eq.c } aPoint
    in
        div []
            [ svg
                [ version "1.1", width "750", height "750", viewBox "0 0 750 750" ]
                [ g []
                    [ circle
                        [ cx (String.fromFloat aPoint.x)
                        , cy (String.fromFloat aPoint.y)
                        , r "2"
                        , fill "black"
                        ]
                        []
                    , circle
                        [ cx (String.fromFloat origin.x)
                        , cy (String.fromFloat origin.y)
                        , r "2"
                        , fill "black"
                        ]
                        []
                    , polygon
                        [ points (equilateralAsString eq)
                        , fill "none"
                        , strokeWidth "1"
                        , stroke "black"
                        ]
                        []
                    , polygon
                        [ points (equilateralAsString (equilateral { start = eq.a, end = eq.b } aPoint))
                        , fill "none"
                        , strokeWidth "1"
                        , stroke "black"
                        ]
                        []
                    , g []
                        [ g [ Svg.Attributes.style originAsString, transform "rotate(-120)" ]
                            [ polygon
                                [ points (equilateralAsString (equilateral { start = eq.a, end = eq.c } aPoint))
                                , fill "none"
                                , strokeWidth "1"
                                , stroke "black"
                                ]
                                []
                            , polygon
                                [ points (equilateralAsString (equilateral { start = eq.b, end = eq.c } aPoint))
                                , fill "none"
                                , strokeWidth "1"
                                , stroke "black"
                                ]
                                []
                            , polygon
                                [ points (equilateralAsString eqUpper)
                                , fill "none"
                                , strokeWidth "1"
                                , stroke "black"
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            ]
