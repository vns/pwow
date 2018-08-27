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


vectorFromLine : Line -> Vector
vectorFromLine { end, start } =
    { x = end.x - start.x, y = end.y - start.y }


norm : Vector -> Vector
norm v =
    { x = v.x / (sqrt (dot v v)), y = v.y / (sqrt (dot v v)) }


projection : Line -> Point -> Point
projection line point =
    let
        v =
            vectorFromLine line

        x =
            vectorFromLine { end = point, start = line.start }

        c =
            (dot x v) / (dot v v)
    in
        { x = line.start.x + c * v.x, y = line.start.y + c * v.y }


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
            vectorFromLine { end = proj, start = line.start }
                |> norm
    in
        { c = point
        , b = { x = proj.x + u.x * halfSide, y = proj.y + u.y * halfSide }
        , a = { x = proj.x - u.x * halfSide, y = proj.y - u.y * halfSide }
        }


lineCenter : Line -> Point
lineCenter { start, end } =
    let
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
    in
        { x = centerX, y = centerY }



-- Apex is left of line direction


equilateralFromLine : Line -> Equilateral
equilateralFromLine line =
    let
        normal =
            vectorFromLine line
                -- normalize
                |> norm
                -- rotate 90 degrees counter-clockwise
                |> \{ x, y } -> { x = y, y = -x }

        side =
            dist line.start line.end
                |> Debug.log "side"

        height =
            (sqrt (side * side - (side * side) / 4))

        center =
            lineCenter line

        apex =
            { x = center.x + height * normal.x
            , y = center.y + height * normal.y
            }
    in
        { a = line.start, b = line.end, c = apex }


equilateralCenter : Equilateral -> Point
equilateralCenter eq =
    let
        half =
            (dist eq.a eq.b) / 2

        proj =
            projection { start = eq.a, end = eq.b } eq.c

        offset =
            half * (tan (degrees 30))

        normal =
            vectorFromLine { start = proj, end = eq.c } |> norm
    in
        { x = proj.x + offset * normal.x, y = proj.y + offset * normal.y }


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

        centerBC =
            equilateralCenter eqBC

        eqAC =
            equilateral { start = eq.a, end = eq.c } aPoint
                |> Debug.log "AC"

        eqUpper =
            equilateral { start = eqAC.a, end = eqBC.a } eq.c

        origin =
            equilateralCenter eqUpper

        originAsString =
            "transform-origin: " ++ String.fromFloat (origin.x) ++ "px " ++ String.fromFloat (origin.y) ++ "px"

        eqAB =
            equilateral { start = eq.a, end = eq.b } aPoint

        prAB =
            projection { start = eq.a, end = eq.b } aPoint
                |> Debug.log "prAB"
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
                    , circle
                        [ cx (String.fromFloat centerBC.x)
                        , cy (String.fromFloat centerBC.y)
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
                    , g
                        []
                        [ polygon
                            [ points (equilateralAsString eqAB)
                            , fill "none"
                            , strokeWidth "1"
                            , stroke "black"
                            ]
                            []
                        , line
                            [ x1 (String.fromFloat aPoint.x)
                            , y1 (String.fromFloat aPoint.y)
                            , x2 (String.fromFloat prAB.x)
                            , y2 (String.fromFloat prAB.y)
                            , strokeWidth "1"
                            , stroke "blue"
                            ]
                            []
                        ]
                    , g []
                        [ g
                            -- [ Svg.Attributes.style originAsString, transform "rotate(-120)" ]
                            []
                            [ polygon
                                [ points (equilateralAsString eqAC)
                                , fill "none"
                                , strokeWidth "1"
                                , stroke "black"
                                ]
                                []
                            , polygon
                                [ points (equilateralAsString eqBC)
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
