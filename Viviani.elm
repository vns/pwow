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
import Json.Decode as Decode
import Debug


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { eq : Equilateral
    , aPoint : Point
    , scene : Anim.State
    , rotation1 : Anim.State
    , rotation2 : Anim.State
    }


type Msg
    = Step1
    | Step2
    | Step3
    | MouseClick Point
    | Animate Anim.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    let
        aPoint =
            { x = 300, y = 400 }

        eq =
            initEquilateral
    in
        ( { eq = eq
          , aPoint = aPoint
          , scene = Anim.style [ Anim.opacity 0 ]
          , rotation1 = Anim.style [ Anim.rotate (Anim.deg 0) ]
          , rotation2 = Anim.style [ Anim.rotate (Anim.deg 0) ]
          }
        , Cmd.none
        )


initEquilateral : Equilateral
initEquilateral =
    equilateralFromLine
        { start = { x = 10, y = 740 }
        , end = { x = 740, y = 740 }
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Anim.subscription Animate
        [ model.scene
        , model.rotation1
        , model.rotation2
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step1 ->
            ( { model
                | scene = Anim.interrupt [ Anim.to [ Anim.opacity 1 ] ] model.scene
              }
            , Cmd.none
            )

        Step2 ->
            let
                eqAC =
                    equilateral { start = model.eq.a, end = model.eq.c }
                        model.aPoint

                center =
                    equilateralCenter eqAC
                        |> Debug.log "center"
            in
                ( { model
                    | rotation1 =
                        Anim.interrupt
                            [ Anim.set
                                [ Anim.transformOrigin
                                    (Anim.px center.x)
                                    (Anim.px center.y)
                                    (Anim.px 0.0)
                                ]
                            , Anim.to
                                [ Anim.rotate (Anim.deg 120)
                                ]

                            -- , Anim.set [ Anim.rotate (Anim.deg 12) ]
                            ]
                            model.rotation1
                  }
                , Cmd.none
                )

        Step3 ->
            let
                eqAC =
                    equilateral { start = model.eq.a, end = model.eq.c } model.aPoint

                eqBC =
                    equilateral { start = model.eq.b, end = model.eq.c } model.aPoint

                eqUpper =
                    equilateral { start = eqAC.a, end = eqBC.a } model.eq.c

                center =
                    equilateralCenter eqUpper
            in
                ( { model
                    | rotation2 =
                        Anim.interrupt
                            [ Anim.set
                                [ Anim.transformOrigin
                                    (Anim.px center.x)
                                    (Anim.px center.y)
                                    (Anim.px 0.0)
                                ]
                            , Anim.to
                                [ Anim.rotate (Anim.deg 120)
                                ]
                            ]
                            model.rotation2
                  }
                , Cmd.none
                )

        MouseClick clickedPoint ->
            let
                _ =
                    clickedPoint |> Debug.log "mouseClick"
            in
                ( { model | aPoint = clickedPoint }
                , Cmd.none
                )

        Animate animMsg ->
            ( { model
                | scene = Anim.update animMsg model.scene
                , rotation1 = Anim.update animMsg model.rotation1
                , rotation2 = Anim.update animMsg model.rotation2
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        [ Attr.style "margin" "200px auto"
        , Attr.style "width" "700px"
        , Attr.style "height" "750px"
        ]
        [ h1 [ Attr.style "cursor" "pointer" ]
            [ span [ onClick Step1, Attr.style "margin-right" "30px" ] [ text "Step 1" ]
            , span [ onClick Step2, Attr.style "margin-right" "30px" ] [ text "Step 2" ]
            , span [ onClick Step3, Attr.style "margin-right" "30px" ] [ text "Step 3" ]
            ]
        , div
            [ Attr.style "cursor" "crosshair" ]
            [ mainCanvas model ]
        ]


onClickWithOffset : (Point -> msg) -> Attribute msg
onClickWithOffset message =
    Html.Events.on "click"
        (Decode.map
            message
            pointDecoder
        )


pointDecoder : Decode.Decoder Point
pointDecoder =
    Decode.map2 Point
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)


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
            model.aPoint

        eq =
            model.eq

        centerBC =
            equilateralCenter eqBC

        eqUpper =
            equilateral { start = eqAC.a, end = eqBC.a } eq.c

        eqAB =
            equilateral { start = eq.a, end = eq.b } aPoint

        prAB =
            projection { start = eq.a, end = eq.b } aPoint

        eqAC =
            equilateral { start = eq.a, end = eq.c } aPoint

        prAC =
            projection { start = eq.a, end = eq.c } aPoint

        eqBC =
            equilateral { start = eq.b, end = eq.c } aPoint

        prBC =
            projection { start = eq.b, end = eq.c } aPoint
    in
        div []
            [ svg
                [ version "1.1", width "750", height "750", viewBox "0 0 750 750", onClickWithOffset MouseClick ]
                [ g []
                    [ circle
                        [ cx (String.fromFloat aPoint.x)
                        , cy (String.fromFloat aPoint.y)
                        , r "2"
                        , fill "black"
                        ]
                        []
                    , polygon
                        [ points (equilateralAsString model.eq)
                        , fill "none"
                        , strokeWidth "1"
                        , stroke "black"
                        ]
                        []
                    , g
                        (Anim.render model.scene)
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
                            ((Anim.render model.scene)
                                ++ (Anim.render model.rotation2)
                            )
                            [ g (Anim.render model.rotation1)
                                [ polygon
                                    [ points (equilateralAsString eqAC)
                                    , fill "none"
                                    , strokeWidth "1"
                                    , stroke "black"
                                    ]
                                    []
                                , line
                                    [ x1 (String.fromFloat aPoint.x)
                                    , y1 (String.fromFloat aPoint.y)
                                    , x2 (String.fromFloat prAC.x)
                                    , y2 (String.fromFloat prAC.y)
                                    , stroke "red"
                                    ]
                                    []
                                ]
                            , g []
                                [ polygon
                                    [ points (equilateralAsString eqBC)
                                    , fill "none"
                                    , strokeWidth "1"
                                    , stroke "black"
                                    ]
                                    []
                                , line
                                    [ x1 (String.fromFloat aPoint.x)
                                    , y1 (String.fromFloat aPoint.y)
                                    , x2 (String.fromFloat prBC.x)
                                    , y2 (String.fromFloat prBC.y)
                                    , stroke "orange"
                                    ]
                                    []
                                ]
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
