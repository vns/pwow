module Main exposing (main)

import Browser
import Color.Palette exposing (red, green, white)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Animation as Anim
import List exposing (map, range)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { tria1 : Anim.State
    , tria2 : Anim.State
    , tria3 : Anim.State
    , tria4 : Anim.State
    , step : Step
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tria1 = Anim.style [ Anim.fill red, Anim.points tria1 ]
      , tria2 = Anim.style [ Anim.fill white, Anim.points <| translate ( 700, 0 ) <| tria2 ]
      , tria3 = Anim.style [ Anim.fill white, Anim.points <| translate ( 700, 700 ) <| tria3 ]
      , tria4 = Anim.style [ Anim.fill white, Anim.points <| translate ( 0, 700 ) <| tria4 ]
      , step = Step0
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Anim.subscription Animate
        [ model.tria1
        , model.tria2
        , model.tria3
        , model.tria4
        ]


type Msg
    = Animate Anim.Msg
    | Step Step


type Step
    = Step0
    | Step1
    | Step2
    | Step3


type alias Vector =
    ( Float, Float )


type alias Triangle =
    List Vector


type Rotation
    = CW90
    | CW180
    | CW270


{-| Rotate a triangle clockwise
-}
rotate : Rotation -> Triangle -> Triangle
rotate rot triangle =
    let
        ptRot =
            \( x, y ) ->
                case rot of
                    CW90 ->
                        ( -y, x )

                    CW180 ->
                        ( -x, -y )

                    CW270 ->
                        ( y, -x )
    in
        triangle |> map ptRot


{-| Translate a triangle
-}
translate : Vector -> Triangle -> Triangle
translate vec triangle =
    let
        ptTrans =
            \( px, py ) ( x, y ) ->
                ( x + px, y + py )
    in
        triangle |> map (ptTrans vec)


baseTri : Triangle
baseTri =
    [ ( 0, 0 ), ( 500, 0 ), ( 0, 200 ) ]


tria1 =
    baseTri


tria2 =
    baseTri |> rotate CW90


tria3 =
    baseTri |> rotate CW180


tria4 =
    baseTri |> rotate CW270


step0 : Model -> Model
step0 model =
    let
        newTria1Style =
            [ Anim.points <| tria1, Anim.fill red ]

        newTria2Style =
            [ Anim.points <| translate ( 700, 0 ) <| tria2, Anim.fill white ]

        newTria3Style =
            [ Anim.points <| translate ( 700, 700 ) <| tria3, Anim.fill white ]

        newTria4Style =
            [ Anim.points <| translate ( 0, 700 ) <| tria4, Anim.fill white ]
    in
        { model
            | tria1 = Anim.interrupt [ Anim.to newTria1Style ] model.tria1
            , tria2 = Anim.interrupt [ Anim.to newTria2Style ] model.tria2
            , tria3 = Anim.interrupt [ Anim.to newTria3Style ] model.tria3
            , tria4 = Anim.interrupt [ Anim.to newTria4Style ] model.tria4
        }


step1 : Model -> Model
step1 model =
    let
        newTria1Style =
            [ Anim.points tria1, Anim.fill red ]

        newTria2Style =
            [ Anim.points <| translate ( 700, 0 ) <| tria2, Anim.fill green ]

        newTria3Style =
            [ Anim.points <| translate ( 700, 700 ) <| tria3, Anim.fill red ]

        newTria4Style =
            [ Anim.points <| translate ( 0, 700 ) <| tria4, Anim.fill green ]
    in
        { model
            | tria1 = Anim.interrupt [ Anim.to newTria1Style ] model.tria1
            , tria2 = Anim.interrupt [ Anim.to newTria2Style ] model.tria2
            , tria3 = Anim.interrupt [ Anim.to newTria3Style ] model.tria3
            , tria4 = Anim.interrupt [ Anim.to newTria4Style ] model.tria4
        }


step2 : Model -> Model
step2 model =
    let
        newTria1Style =
            [ Anim.points <| translate ( 200, 500 ) <| tria1, Anim.fill red ]

        newTria2Style =
            [ Anim.points <| translate ( 700, 0 ) <| tria2, Anim.fill green ]

        newTria3Style =
            [ Anim.points <| translate ( 700, 700 ) <| tria3, Anim.fill red ]

        newTria4Style =
            [ Anim.points <| translate ( 500, 500 ) <| tria4, Anim.fill green ]
    in
        { model
            | tria1 = Anim.interrupt [ Anim.to newTria1Style ] model.tria1
            , tria2 = Anim.interrupt [ Anim.to newTria2Style ] model.tria2
            , tria3 = Anim.interrupt [ Anim.to newTria3Style ] model.tria3
            , tria4 = Anim.interrupt [ Anim.to newTria4Style ] model.tria4
        }


step3 : Model -> Model
step3 model =
    let
        newTria1Style =
            [ Anim.points <| translate ( 0, 500 ) <| tria1, Anim.fill red ]

        newTria2Style =
            [ Anim.points <| translate ( 700, 0 ) <| tria2, Anim.fill green ]

        newTria3Style =
            [ Anim.points <| translate ( 500, 700 ) <| tria3, Anim.fill red ]

        newTria4Style =
            [ Anim.points <| translate ( 500, 500 ) <| tria4, Anim.fill green ]
    in
        { model
            | tria1 = Anim.interrupt [ Anim.to newTria1Style ] model.tria1
            , tria2 = Anim.interrupt [ Anim.to newTria2Style ] model.tria2
            , tria3 = Anim.interrupt [ Anim.to newTria3Style ] model.tria3
            , tria4 = Anim.interrupt [ Anim.to newTria4Style ] model.tria4
        }


updateWith : Step -> (Model -> Model)
updateWith step =
    case step of
        Step0 ->
            step0

        Step1 ->
            step1

        Step2 ->
            step2

        Step3 ->
            step3


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step step ->
            ( { model | step = step } |> updateWith step, Cmd.none )

        Animate time ->
            ( { model
                | tria1 = Anim.update time model.tria1
                , tria2 = Anim.update time model.tria2
                , tria3 = Anim.update time model.tria3
                , tria4 = Anim.update time model.tria4
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
            , stepView Step3 model.step
            ]
        , div
            [ Attr.class "canvas-container"
            ]
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
        span [ onClick (Step aStep), Attr.class stepClass ] [ Html.text " " ]


mainCanvas : Model -> Html.Html msg
mainCanvas model =
    div []
        [ svg
            [ version "1.1", width "500", height "500", viewBox "0 0 700 700", stroke "#444" ]
            [ g []
                [ rect [ fill "#eee", strokeWidth "0", x "0", y "0", width "700", height "700" ] []
                , g [ strokeWidth "0" ]
                    [ polygon (Anim.render model.tria1) []
                    , polygon (Anim.render model.tria2) []
                    , polygon (Anim.render model.tria3) []
                    , polygon (Anim.render model.tria4) []
                    ]
                ]
            ]
        ]
