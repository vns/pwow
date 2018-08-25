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


type alias Model =
    { styles : List Animation.State
    , tria1 : Animation.State
    , tria2 : Animation.State
    , tria3 : Animation.State
    , tria4 : Animation.State
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { styles =
            List.map Animation.style polygons
      , tria1 = Animation.style <| tria 500 200 darkYellow
      , tria2 = Animation.style <| tria 500 200 darkYellow
      , tria3 = Animation.style <| tria 500 200 darkYellow
      , tria4 = Animation.style <| tria 500 200 darkYellow
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.tria1, model.tria2, model.tria3, model.tria4 ]


type Msg
    = Animate Animation.Msg
    | ChangeColors
    | MorphTria


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorphTria ->
            let
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
            in
                ( { model
                    | tria2 = Animation.interrupt [ Animation.to newTria2Style ] model.tria2
                    , tria3 = Animation.interrupt [ Animation.to newTria3Style ] model.tria3
                    , tria4 = Animation.interrupt [ Animation.to newTria4Style ] model.tria4
                  }
                , Cmd.none
                )

        ChangeColors ->
            ( { model
                | styles =
                    List.map2
                        (\style newStyle ->
                            Animation.interrupt
                                [ Animation.to newStyle ]
                                style
                        )
                        model.styles
                        polygons2
              }
            , Cmd.none
            )

        Animate time ->
            ( { model
                | tria1 = Animation.update time model.tria1
                , tria2 = Animation.update time model.tria2
                , tria3 = Animation.update time model.tria3
                , tria4 = Animation.update time model.tria4
              }
            , Cmd.none
            )


tria : Float -> Float -> Color.Palette.Color -> List Animation.Property
tria base height color =
    [ Animation.points [ ( 0, 0 ), ( base, 0 ), ( 0, height ) ]
    , Animation.fill color
    ]


polygons : List (List Animation.Property)
polygons =
    [ [ Animation.points
            [ ( 0, 0 ), ( 400, 0 ), ( 0, 300 ) ]
      , Animation.fill darkYellow
      ]
    , [ Animation.points
            [ ( 400, 0 ), ( 700, 0 ), ( 700, 400 ) ]
      , Animation.fill purple
      ]
    , [ Animation.points
            [ ( 700, 400 ), ( 700, 700 ), ( 300, 700 ) ]
      , Animation.fill darkYellow
      ]
    , [ Animation.points
            [ ( 300, 700 ), ( 0, 700 ), ( 0, 300 ) ]
      , Animation.fill purple
      ]
    ]


polygons2 : List (List Animation.Property)
polygons2 =
    [ [ Animation.points
            [ ( 0, 0 ), ( 400, 0 ), ( 0, 300 ) ]
      , Animation.fill purple
      ]
    , [ Animation.points
            [ ( 400, 0 ), ( 700, 0 ), ( 700, 400 ) ]
      , Animation.fill darkYellow
      ]
    , [ Animation.points
            [ ( 700, 400 ), ( 700, 700 ), ( 300, 700 ) ]
      , Animation.fill darkYellow
      ]
    , [ Animation.points
            [ ( 300, 700 ), ( 0, 700 ), ( 0, 300 ) ]
      , Animation.fill purple
      ]
    ]


view : Model -> Html Msg
view model =
    div
        [ Attr.style "margin" "200px auto"
        , Attr.style "width" "700px"
        , Attr.style "height" "750px"
        ]
        [ h1 [ Attr.style "cursor" "pointer" ]
            [ span [ onClick MorphTria, Attr.style "margin-right" "30px" ] [ text "Step 1" ]
            , span [ onClick ChangeColors ] [ text "Step 2" ]
            ]
        , mainCanvas model
        ]


mainCanvas : Model -> Html.Html msg
mainCanvas model =
    svg
        [ version "1.1", width "700", height "750", viewBox "0 0 700 750" ]
        [ polygon (Animation.render model.tria1) []
        , polygon (Animation.render model.tria2) []
        , polygon (Animation.render model.tria3) []
        , polygon (Animation.render model.tria4) []

        -- [ g [] (List.map (\poly -> polygon (Animation.render poly) []) model.styles)
        ]


squared attrList letter =
    text_ attrList
        [ tspan
            [ fontSize "25" ]
            [ text letter
            , tspan [ dy "-10", fontSize "15" ] [ text "2" ]
            ]
        ]
