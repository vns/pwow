module Main exposing (main)

import Browser
import Color.Palette exposing (darkYellow, purple)
import Html exposing (Html, div, h1, span)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Animation
import Debug


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Labels =
    { csq : Animation.State
    , asq : Animation.State
    , bsq : Animation.State
    }


type alias Model =
    { tria1 : Animation.State
    , tria2 : Animation.State
    , tria3 : Animation.State
    , tria4 : Animation.State
    , labels : Labels

    -- , rect1 : Animation.State
    -- , rect2 : Animation.State
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tria1 = Animation.style <| tria 500 200 darkYellow
      , tria2 = Animation.style <| tria 500 200 darkYellow
      , tria3 = Animation.style <| tria 500 200 darkYellow
      , tria4 = Animation.style <| tria 500 200 darkYellow
      , labels =
            { csq = Animation.style [ Animation.display Animation.none ]
            , asq = Animation.style [ Animation.display Animation.none ]
            , bsq = Animation.style [ Animation.display Animation.none ]
            }
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate
        [ model.tria1
        , model.tria2
        , model.tria3
        , model.tria4
        , model.labels.csq
        , model.labels.asq
        , model.labels.bsq
        ]


type Msg
    = Animate Animation.Msg
    | Step1
    | Step2
    | Step3


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step1 ->
            let
                newTria1Style =
                    tria 500 200 darkYellow

                newTria2Style =
                    [ Animation.points [ ( 500, 0 ), ( 700, 0 ), ( 700, 500 ) ]
                    , Animation.fill purple
                    ]

                newTria3Style =
                    [ Animation.points [ ( 700, 500 ), ( 700, 700 ), ( 200, 700 ) ]
                    , Animation.fill darkYellow
                    ]

                newTria4Style =
                    [ Animation.points [ ( 200, 700 ), ( 0, 700 ), ( 0, 200 ) ]
                    , Animation.fill purple
                    ]

                oldLabels =
                    model.labels

                newLabels =
                    { oldLabels
                        | csq = Animation.interrupt [ Animation.set [ Animation.display Animation.block ] ] oldLabels.csq
                    }
            in
                ( { model
                    | tria1 = Animation.interrupt [ Animation.to newTria1Style ] model.tria1
                    , tria2 = Animation.interrupt [ Animation.to newTria2Style ] model.tria2
                    , tria3 = Animation.interrupt [ Animation.to newTria3Style ] model.tria3
                    , tria4 = Animation.interrupt [ Animation.to newTria4Style ] model.tria4
                    , labels = newLabels
                  }
                , Cmd.none
                )

        Step2 ->
            let
                newTria1Style =
                    [ Animation.points [ ( 500, 0 ), ( 500, 500 ), ( 700, 500 ) ]
                    , Animation.fill purple
                    ]

                newTria4Style =
                    [ Animation.points [ ( 200, 500 ), ( 700, 500 ), ( 200, 700 ) ]
                    , Animation.fill darkYellow
                    ]
            in
                ( { model
                    | tria1 = Animation.interrupt [ Animation.to newTria1Style ] model.tria1
                    , tria4 = Animation.interrupt [ Animation.to newTria4Style ] model.tria4
                  }
                , Cmd.none
                )

        Step3 ->
            ( model, Cmd.none )

        Animate time ->
            let
                oldLabels =
                    model.labels

                newLabels =
                    { oldLabels
                        | csq = Animation.update time oldLabels.csq
                        , asq = Animation.update time oldLabels.asq
                        , bsq = Animation.update time oldLabels.bsq
                    }
            in
                ( { model
                    | tria1 = Animation.update time model.tria1
                    , tria2 = Animation.update time model.tria2
                    , tria3 = Animation.update time model.tria3
                    , tria4 = Animation.update time model.tria4
                    , labels = newLabels
                  }
                , Cmd.none
                )


tria : Float -> Float -> Color.Palette.Color -> List Animation.Property
tria base height color =
    [ Animation.points [ ( 0, 0 ), ( base, 0 ), ( 0, height ) ]
    , Animation.fill color
    ]


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
        , mainCanvas model
        ]


mainCanvas : Model -> Html.Html msg
mainCanvas model =
    div []
        [ svg
            [ version "1.1", width "700", height "750", viewBox "0 0 700 750", stroke "black" ]
            [ g []
                [ rect [ stroke "black", fill "white", x "0", y "0", width "700", height "700" ] []
                , rect [ stroke "black", fill "white", x "0", y "0", width "500", height "500", display "none" ] []
                , rect [ stroke "black", fill "white", x "0", y "500", width "200", height "200", display "none" ] []
                , polygon (Animation.render model.tria1) []
                , polygon (Animation.render model.tria2) []
                , polygon (Animation.render model.tria3) []
                , polygon (Animation.render model.tria4) []
                , squared
                    ((Animation.render model.labels.csq)
                        ++ [ x "350", y "350", color "black" ]
                    )
                    "c"
                ]

            -- [ g [] (List.map (\poly -> polygon (Animation.render poly) []) model.styles)
            ]
        , svg
            [ version "1.1", width "700", height "750", transform "scale(0.5)translate(200,0)", display "none", viewBox "0 0 700 750" ]
            [ polygon (Animation.render model.tria1) []
            , polygon (Animation.render model.tria2) []
            , polygon (Animation.render model.tria3) []
            , polygon (Animation.render model.tria4) []

            -- [ g [] (List.map (\poly -> polygon (Animation.render poly) []) model.styles)
            ]
        ]


squared attrList letter =
    text_ attrList
        [ tspan
            [ fontSize "25" ]
            [ text letter
            , tspan [ dy "-10", fontSize "15" ] [ text "2" ]
            ]
        ]
