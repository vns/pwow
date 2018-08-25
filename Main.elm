module Main exposing (main)

import Browser
import Color.Palette exposing (darkYellow, purple)
import Html exposing (Html, div, h1)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Animation


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { styles : List Animation.State
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { styles =
            List.map Animation.style polygons
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate model.styles


type Msg
    = Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate time ->
            ( { model | styles = List.map (Animation.update time) model.styles }
            , Cmd.none
            )


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


view : Model -> Html Msg
view model =
    div
        [ Attr.style "margin" "200px auto"
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
        [ g [] (List.map (\poly -> polygon (Animation.render poly) []) model.styles)
        ]


squared attrList letter =
    text_ attrList
        [ tspan
            [ fontSize "25" ]
            [ text letter
            , tspan [ dy "-10", fontSize "15" ] [ text "2" ]
            ]
        ]
