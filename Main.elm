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


type alias Labels =
    { csq : Anim.State
    , asq : Anim.State
    , bsq : Anim.State
    }


type alias Model =
    { tria1 : Anim.State
    , tria2 : Anim.State
    , tria3 : Anim.State
    , tria4 : Anim.State
    , labels : Labels
    , step : Step

    -- , rect1 : Anim.State
    -- , rect2 : Anim.State
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        tria1 =
            initialTria

        tria2 =
            initialTria
                |> map rotate90
                |> map (translate ( 700, 0 ))

        tria3 =
            initialTria
                |> map rotate90
                |> map rotate90
                |> map (translate ( 700, 700 ))

        tria4 =
            initialTria
                |> map rotate90
                |> map rotate90
                |> map rotate90
                |> map (translate ( 0, 700 ))
    in
        ( { tria1 = Anim.style [ Anim.fill red, Anim.points tria1 ]
          , tria2 = Anim.style [ Anim.fill white, Anim.points tria2 ]
          , tria3 = Anim.style [ Anim.fill white, Anim.points tria3 ]
          , tria4 = Anim.style [ Anim.fill white, Anim.points tria4 ]
          , labels =
                { csq = Anim.style [ Anim.display Anim.none ]
                , asq = Anim.style [ Anim.display Anim.none ]
                , bsq = Anim.style [ Anim.display Anim.none ]
                }
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
        , model.labels.csq
        , model.labels.asq
        , model.labels.bsq
        ]


type Msg
    = Animate Anim.Msg
    | Step Step


type Step
    = Step0
    | Step1
    | Step2
    | Step3


rotate90 ( x, y ) =
    ( -y, x )


translate ( px, py ) ( x, y ) =
    ( x + px, y + py )


initialTria =
    [ ( 0, 0 ), ( 500, 0 ), ( 0, 200 ) ]


triaAnim color points =
    [ Anim.points points
    , Anim.fill color
    ]


step0 : Model -> Model
step0 model =
    let
        tria1 =
            initialTria

        tria2 =
            initialTria
                |> map rotate90
                |> map (translate ( 700, 0 ))

        tria3 =
            initialTria
                |> map rotate90
                |> map rotate90
                |> map (translate ( 700, 700 ))

        tria4 =
            initialTria
                |> map rotate90
                |> map rotate90
                |> map rotate90
                |> map (translate ( 0, 700 ))

        newTria1Style =
            [ Anim.points tria1, Anim.fill red ]

        newTria2Style =
            [ Anim.points tria2, Anim.fill white ]

        newTria3Style =
            [ Anim.points tria3, Anim.fill white ]

        newTria4Style =
            [ Anim.points tria4, Anim.fill white ]

        oldLabels =
            model.labels

        newLabels =
            { oldLabels
                | csq = Anim.interrupt [ Anim.set [ Anim.display Anim.block ] ] oldLabels.csq
                , asq = Anim.interrupt [ Anim.set [ Anim.display Anim.none ] ] oldLabels.asq
                , bsq = Anim.interrupt [ Anim.set [ Anim.display Anim.none ] ] oldLabels.bsq
            }
    in
        { model
            | tria1 =
                Anim.interrupt
                    [ Anim.to newTria1Style
                    ]
                    model.tria1
            , tria2 =
                Anim.interrupt
                    [ Anim.to newTria2Style
                    ]
                    model.tria2
            , tria3 =
                Anim.interrupt
                    [ Anim.to newTria3Style
                    ]
                    model.tria3
            , tria4 =
                Anim.interrupt
                    [ Anim.to newTria4Style
                    ]
                    model.tria4
            , labels = newLabels
        }


step1 : Model -> Model
step1 model =
    let
        tria1 =
            initialTria

        tria2 =
            initialTria
                |> map rotate90
                |> map (translate ( 700, 0 ))

        tria3 =
            initialTria
                |> map rotate90
                |> map rotate90
                |> map (translate ( 700, 700 ))

        tria4 =
            initialTria
                |> map rotate90
                |> map rotate90
                |> map rotate90
                |> map (translate ( 0, 700 ))

        newTria1Style =
            [ Anim.points tria1, Anim.fill red ]

        newTria2Style =
            [ Anim.points tria2, Anim.fill green ]

        newTria3Style =
            [ Anim.points tria3, Anim.fill red ]

        newTria4Style =
            [ Anim.points tria4, Anim.fill green ]

        oldLabels =
            model.labels

        newLabels =
            { oldLabels
                | csq = Anim.interrupt [ Anim.set [ Anim.display Anim.block ] ] oldLabels.csq
                , asq = Anim.interrupt [ Anim.set [ Anim.display Anim.none ] ] oldLabels.asq
                , bsq = Anim.interrupt [ Anim.set [ Anim.display Anim.none ] ] oldLabels.bsq
            }
    in
        { model
            | tria1 =
                Anim.interrupt
                    [ Anim.to newTria1Style
                    ]
                    model.tria1
            , tria2 =
                Anim.interrupt
                    [ Anim.to newTria2Style
                    ]
                    model.tria2
            , tria3 =
                Anim.interrupt
                    [ Anim.to newTria3Style
                    ]
                    model.tria3
            , tria4 =
                Anim.interrupt
                    [ Anim.to newTria4Style
                    ]
                    model.tria4
            , labels = newLabels
        }


step2 : Model -> Model
step2 model =
    let
        tria1 =
            initialTria
                |> map (translate ( 200, 500 ))

        tria2 =
            initialTria
                |> map rotate90
                |> map (translate ( 700, 0 ))

        tria3 =
            initialTria
                |> map rotate90
                |> map rotate90
                |> map (translate ( 700, 700 ))

        tria4 =
            initialTria
                |> map rotate90
                |> map rotate90
                |> map rotate90
                |> map (translate ( 500, 500 ))

        newTria1Style =
            [ Anim.points tria1
            ]

        newTria2Style =
            [ Anim.points tria2
            ]

        newTria3Style =
            [ Anim.points tria3
            ]

        newTria4Style =
            [ Anim.points tria4
            ]

        oldLabels =
            model.labels

        newLabels =
            { oldLabels
                | csq = Anim.interrupt [ Anim.set [ Anim.display Anim.none ] ] oldLabels.csq
                , asq = Anim.interrupt [ Anim.set [ Anim.display Anim.none ] ] oldLabels.asq
                , bsq = Anim.interrupt [ Anim.set [ Anim.display Anim.none ] ] oldLabels.bsq
            }
    in
        { model
            | tria1 = Anim.interrupt [ Anim.to newTria1Style ] model.tria1
            , tria2 = Anim.interrupt [ Anim.to newTria2Style ] model.tria2
            , tria3 = Anim.interrupt [ Anim.to newTria3Style ] model.tria3
            , tria4 = Anim.interrupt [ Anim.to newTria4Style ] model.tria4
            , labels = newLabels
        }


step3 : Model -> Model
step3 model =
    let
        tria1 =
            initialTria
                |> map (translate ( 0, 500 ))

        tria2 =
            initialTria
                |> map rotate90
                |> map (translate ( 700, 0 ))

        tria3 =
            initialTria
                |> map rotate90
                |> map rotate90
                |> map (translate ( 500, 700 ))

        tria4 =
            initialTria
                |> map rotate90
                |> map rotate90
                |> map rotate90
                |> map (translate ( 500, 500 ))

        newTria1Style =
            [ Anim.points tria1
            , Anim.fill red
            ]

        newTria2Style =
            [ Anim.points tria2
            , Anim.fill green
            ]

        newTria3Style =
            [ Anim.points tria3
            , Anim.fill red
            ]

        newTria4Style =
            [ Anim.points tria4
            , Anim.fill green
            ]

        oldLabels =
            model.labels

        newLabels =
            { oldLabels
                | csq = Anim.interrupt [ Anim.set [ Anim.display Anim.none ] ] oldLabels.csq
                , asq = Anim.interrupt [ Anim.set [ Anim.display Anim.block ] ] oldLabels.asq
                , bsq = Anim.interrupt [ Anim.set [ Anim.display Anim.block ] ] oldLabels.bsq
            }
    in
        { model
            | tria1 = Anim.interrupt [ Anim.to newTria1Style ] model.tria1
            , tria2 = Anim.interrupt [ Anim.to newTria2Style ] model.tria2
            , tria3 = Anim.interrupt [ Anim.to newTria3Style ] model.tria3
            , tria4 = Anim.interrupt [ Anim.to newTria4Style ] model.tria4
            , labels = newLabels
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

        Step3 ->
            step3


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step step ->
            ( { model | step = step } |> makeStepFn step, Cmd.none )

        Animate time ->
            let
                oldLabels =
                    model.labels

                newLabels =
                    { oldLabels
                        | csq = Anim.update time oldLabels.csq
                        , asq = Anim.update time oldLabels.asq
                        , bsq = Anim.update time oldLabels.bsq
                    }
            in
                ( { model
                    | tria1 = Anim.update time model.tria1
                    , tria2 = Anim.update time model.tria2
                    , tria3 = Anim.update time model.tria3
                    , tria4 = Anim.update time model.tria4
                    , labels = newLabels
                  }
                , Cmd.none
                )


tria : Float -> Float -> Color.Palette.Color -> List Anim.Property
tria base height color =
    [ Anim.points [ ( 0, 0 ), ( base, 0 ), ( 0, height ) ]
    , Anim.fill color
    ]


theoremDef =
    """
from Greek theorema "spectacle, sight," in Euclid "proposition to be proved," literally "that which is looked at," from theorein "to look at, behold"
"""


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
        span [ onClick (Step aStep), Attr.class stepClass ] []


mainCanvas : Model -> Html.Html msg
mainCanvas model =
    div []
        [ svg
            [ version "1.1", width "500", height "500", viewBox "0 0 700 700", stroke "#444" ]
            [ g []
                [ rect [ fill "#eee", strokeWidth "0", x "0", y "0", width "700", height "700" ] []
                , g [ strokeWidth "0" ]
                    [ polygon (Anim.render model.tria2) []
                    , polygon (Anim.render model.tria3) []
                    , polygon (Anim.render model.tria4) []
                    , polygon (Anim.render model.tria1) []
                    ]

                -- , squared
                --     ((Anim.render model.labels.csq)
                --         ++ [ x "342", y "355", color "darkGray" ]
                --     )
                --     "c"
                -- , squared
                --     ((Anim.render model.labels.asq)
                --         ++ [ x "242", y "255", color "darkGray" ]
                --     )
                --     "a"
                -- , squared
                --     ((Anim.render model.labels.asq)
                --         ++ [ x "592", y "605", color "darkGray" ]
                --     )
                --     "b"
                ]

            -- [ g [] (List.map (\poly -> polygon (Anim.render poly) []) model.styles)
            ]
        ]


squared attrList letter =
    text_ attrList
        [ tspan
            [ fontSize "35", stroke "darkGray" ]
            [ Svg.text letter
            , tspan [ dy "-10", fontSize "20" ] [ Svg.text "2" ]
            ]
        ]
