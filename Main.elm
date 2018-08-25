module Main exposing (main)

import Browser
import Color.Palette exposing (darkYellow, purple)
import Html exposing (Html, div, h1)
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
    , tria : Animation.State
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { styles =
            List.map Animation.style polygons
      , tria = Animation.style <| tria 300 400 darkYellow
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.tria ]


type Msg
    = Animate Animation.Msg
    | ChangeColors
    | MorphTria


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorphTria ->
            let
                newStyle =
                    tria 200 500 purple
            in
                ( { model | tria = Animation.interrupt [ Animation.to newStyle ] model.tria }
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
            ( { model | tria = Animation.update time model.tria }
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
        [ onClick MorphTria
        , Attr.style "margin" "200px auto"
        , Attr.style "width" "700px"
        , Attr.style "height" "750px"
        , Attr.style "cursor" "pointer"
        ]
        [ h1 [] [ text "Click to begin!" ]
        , mainCanvas model
        ]


mainCanvas : Model -> Html.Html msg
mainCanvas model =
    svg
        [ version "1.1", width "700", height "750", viewBox "0 0 700 750" ]
        [ polygon (Animation.render model.tria) []

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
