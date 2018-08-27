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


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    Int


type Msg
    = Msg1


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( 0, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        []
