module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, span)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Animation as Anim
import List exposing (map, range)
import Json.Decode as Decode


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
    , step : Step
    }


type Step
    = Step0
    | Step1
    | Step2


type Msg
    = Step Step
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
          , scene = Anim.style [ Anim.opacity 1 ]
          , rotation1 = Anim.style [ Anim.rotate (Anim.deg 0) ]
          , rotation2 = Anim.style [ Anim.rotate (Anim.deg 0) ]
          , step = Step0
          }
        , Cmd.none
        )


step0 : Model -> Model
step0 model =
    let
        eqAC =
            equilateral { start = model.eq.a, end = model.eq.c }
                model.aPoint

        eqBC =
            equilateral { start = model.eq.b, end = model.eq.c } model.aPoint

        eqUpper =
            equilateral { start = eqAC.a, end = eqBC.a } model.eq.c

        center1 =
            equilateralCenter eqAC

        center2 =
            equilateralCenter eqUpper
    in
        { model
            | step = Step0
            , scene =
                Anim.interrupt [ Anim.to [ Anim.opacity 1 ] ] model.scene
            , rotation1 =
                Anim.interrupt
                    [ Anim.set
                        [ Anim.transformOrigin
                            (Anim.px center1.x)
                            (Anim.px center1.y)
                            (Anim.px 0.0)
                        ]
                    , Anim.to
                        [ Anim.rotate (Anim.deg 0)
                        ]
                    ]
                    model.rotation1
            , rotation2 =
                Anim.interrupt
                    [ Anim.set
                        [ Anim.transformOrigin
                            (Anim.px center2.x)
                            (Anim.px center2.y)
                            (Anim.px 0.0)
                        ]
                    , Anim.to
                        [ Anim.rotate (Anim.deg 0)
                        ]
                    ]
                    model.rotation2
        }


step1 : Model -> Model
step1 model =
    let
        eqAC =
            equilateral { start = model.eq.a, end = model.eq.c }
                model.aPoint

        eqBC =
            equilateral { start = model.eq.b, end = model.eq.c } model.aPoint

        eqUpper =
            equilateral { start = eqAC.a, end = eqBC.a } model.eq.c

        center1 =
            equilateralCenter eqAC

        center2 =
            equilateralCenter eqUpper
    in
        { model
            | step = Step1
            , rotation1 =
                Anim.interrupt
                    [ Anim.set
                        [ Anim.transformOrigin
                            (Anim.px center1.x)
                            (Anim.px center1.y)
                            (Anim.px 0.0)
                        ]
                    , Anim.to
                        [ Anim.rotate (Anim.deg 120)
                        ]
                    ]
                    model.rotation1
            , rotation2 =
                Anim.interrupt
                    [ Anim.set
                        [ Anim.transformOrigin
                            (Anim.px center2.x)
                            (Anim.px center2.y)
                            (Anim.px 0.0)
                        ]
                    , Anim.to
                        [ Anim.rotate (Anim.deg 0)
                        ]
                    ]
                    model.rotation2
        }


step2 : Model -> Model
step2 model =
    let
        eqAC =
            equilateral { start = model.eq.a, end = model.eq.c }
                model.aPoint

        eqBC =
            equilateral { start = model.eq.b, end = model.eq.c } model.aPoint

        eqUpper =
            equilateral { start = eqAC.a, end = eqBC.a } model.eq.c

        center1 =
            equilateralCenter eqAC

        center2 =
            equilateralCenter eqUpper
    in
        { model
            | step = Step2
            , rotation1 =
                Anim.interrupt
                    [ Anim.set
                        [ Anim.transformOrigin
                            (Anim.px center1.x)
                            (Anim.px center1.y)
                            (Anim.px 0.0)
                        ]
                    , Anim.to
                        [ Anim.rotate (Anim.deg 120)
                        ]
                    ]
                    model.rotation1
            , rotation2 =
                Anim.interrupt
                    [ Anim.set
                        [ Anim.transformOrigin
                            (Anim.px center2.x)
                            (Anim.px center2.y)
                            (Anim.px 0.0)
                        ]
                    , Anim.to
                        [ Anim.rotate (Anim.deg 120)
                        ]
                    ]
                    model.rotation2
        }


makeStepFn : Step -> (Model -> Model)
makeStepFn step =
    case step of
        Step0 ->
            step0

        Step1 ->
            step1

        Step2 ->
            step2


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
        Step step ->
            ( model |> makeStepFn step
            , Cmd.none
            )

        MouseClick clickedPoint ->
            let
                inside =
                    pointInEquilateral clickedPoint model.eq

                newPoint =
                    if
                        (pointInEquilateral clickedPoint model.eq)
                            && (model.step == Step0)
                    then
                        clickedPoint
                    else
                        model.aPoint
            in
                ( { model | aPoint = newPoint }
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
        [ Attr.class "presentation" ]
        [ div [ Attr.class "steps" ]
            [ stepView Step0 model.step
            , stepView Step1 model.step
            , stepView Step2 model.step
            ]
        , div
            [ Attr.class "canvas-container" ]
            [ mainCanvas model ]
        ]


stepView : Step -> Step -> Html.Html Msg
stepView aStep currentStep =
    let
        stepClass =
            "step"
                ++ if aStep == currentStep then
                    " active"
                   else
                    ""
    in
        span [ onClick (Step aStep), Attr.class stepClass ] []


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

        centerY =
            (if start.y > end.y then
                end.y + (start.y - end.y) / 2
             else if start.y < end.y then
                start.y + (end.y - start.y) / 2
             else
                start.y
            )
    in
        { x = centerX, y = centerY }



-- rotate 90 degrees counter-clockwise


rotate90 : Vector -> Vector
rotate90 { x, y } =
    { x = y, y = -x }


leftOf : Point -> Line -> Bool
leftOf point line =
    let
        v1 =
            vectorFromLine { start = line.start, end = point }

        v2 =
            vectorFromLine { start = line.start, end = line.end }
                |> rotate90
    in
        dot v1 v2 > 0.0


pointInEquilateral : Point -> Equilateral -> Bool
pointInEquilateral pt eq =
    leftOf pt { start = eq.a, end = eq.b }
        && leftOf pt { start = eq.c, end = eq.a }
        && leftOf pt { start = eq.b, end = eq.c }



-- Apex is left of line direction


equilateralFromLine : Line -> Equilateral
equilateralFromLine line =
    let
        normal =
            vectorFromLine line
                -- normalize
                |> norm
                -- rotate 90 degrees counter-clockwise
                |> rotate90

        side =
            dist line.start line.end

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
        div
            [ Attr.style "margin-top" "-90px"
            ]
            [ svg
                [ version "1.1", width "750", height "750", viewBox "0 0 750 750", onClickWithOffset MouseClick ]
                [ g []
                    [ polygon
                        [ points (equilateralAsString model.eq)
                        , fill "none"
                        , strokeWidth "0"
                        , fill "#eee"
                        ]
                        []
                    , circle
                        [ cx (String.fromFloat aPoint.x)
                        , cy (String.fromFloat aPoint.y)
                        , r "3"
                        , fill "#888"
                        ]
                        []
                    , g
                        (Anim.render model.scene)
                        [ polygon
                            [ points (equilateralAsString eqAB)
                            , strokeWidth "0"
                            , fill "#4A90E2"
                            ]
                            []
                        , line
                            [ x1 (String.fromFloat aPoint.x)
                            , y1 (String.fromFloat (aPoint.y + 4))
                            , x2 (String.fromFloat prAB.x)
                            , y2 (String.fromFloat (prAB.y - 2))
                            , strokeWidth "1"
                            , stroke "#eee"
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
                                    , strokeWidth "0"
                                    , fill "#B6D6CC"
                                    ]
                                    []
                                , line
                                    [ x1 (String.fromFloat aPoint.x)
                                    , y1 (String.fromFloat aPoint.y)
                                    , x2 (String.fromFloat prAC.x)
                                    , y2 (String.fromFloat prAC.y)
                                    , stroke "#eee"
                                    , strokeWidth "1"
                                    , strokeLinecap "butt"
                                    ]
                                    []
                                ]
                            , g []
                                [ polygon
                                    [ points (equilateralAsString eqBC)
                                    , fill "none"
                                    , strokeWidth "0"
                                    , fill "#9BC53D"
                                    ]
                                    []
                                , line
                                    [ x1 (String.fromFloat aPoint.x)
                                    , y1 (String.fromFloat aPoint.y)
                                    , x2 (String.fromFloat prBC.x)
                                    , y2 (String.fromFloat prBC.y)
                                    , stroke "#eee"
                                    , strokeWidth "1"
                                    , strokeLinecap "butt"
                                    ]
                                    []
                                ]
                            , polygon
                                [ points (equilateralAsString eqUpper)
                                , fill "none"
                                , strokeWidth "0"
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            ]
