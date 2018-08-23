module Main exposing (main)

import Browser
import Html exposing (Html, div, math)
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
    { style : Animation.State
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { style =
            Animation.style [ Animation.opacity 1.0 ]
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.style ]


type Msg
    = FadeInFadeOut
    | Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FadeInFadeOut ->
            ( { model
                | style =
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.opacity 0
                            ]
                        , Animation.to
                            [ Animation.opacity 1
                            ]
                        ]
                        model.style
              }
            , Cmd.none
            )

        Animate animMsg ->
            ( { model | style = Animation.update animMsg model.style }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        (Animation.render model.style
            ++ [ onClick FadeInFadeOut
               , Attr.style "position" "relative"
               , Attr.style "margin" "100px auto"
               , Attr.style "padding" "25px"
               , Attr.style "width" "200px"
               , Attr.style "height" "200px"
               , Attr.style "background-color" "#268bd2"
               , Attr.style "color" "white"
               ]
        )
        [ text "Click to animate!" ]


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
