module Main exposing (main)

import Browser
import Html exposing (Html, div, math)


--import Html.Attributes as Attr

import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { style : String
    }


type Msg
    = A


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model ""
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        A ->
            ( { style = "" }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ mainCanvas
        ]


mainCanvas : Html.Html msg
mainCanvas =
    svg
        [ version "1.1", width "700", height "750", viewBox "0 0 700 750" ]
        [ polygon [ points "0,0 400,0 0,300", fill "red" ] []
        , polygon [ points "400,0 700,0 700,400", fill "blue" ] []
        , polygon [ points "700,400 700,700 300,700", fill "red" ] []
        , polygon [ points "300,700 0,700 0,300", fill "blue" ] []
        , text_ [ x "25", y "725", fill "green" ]
            [ tspan [ fontSize "25" ]
                [ text "a"
                , tspan [ dy "-10", fontSize "15" ] [ text "2" ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
