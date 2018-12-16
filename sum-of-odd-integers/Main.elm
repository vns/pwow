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
    { numberOfRows : Int
    , isTransformed : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { numberOfRows = 6
      , isTransformed = False
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = DoTransform
    | Less
    | More


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Less ->
            let
                n =
                    case model.numberOfRows of
                        1 ->
                            1

                        _ ->
                            model.numberOfRows - 1
            in
                ( { model
                    | numberOfRows = n
                  }
                , Cmd.none
                )

        More ->
            let
                n =
                    case model.numberOfRows of
                        9 ->
                            9

                        _ ->
                            model.numberOfRows + 1
            in
                ( { model
                    | numberOfRows = n
                  }
                , Cmd.none
                )

        DoTransform ->
            ( { model
                | isTransformed = not model.isTransformed
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        [ Attr.class "presentation" ]
        [ div
            [ Attr.class "canvas-container"
            , Attr.style "margin" "0"
            ]
            [ mainCanvas model ]
        ]


square : Int -> Int -> Svg msg
square posX posY =
    let
        attrX =
            posX + 5 |> String.fromInt

        attrY =
            posY + 5 |> String.fromInt
    in
        rect
            [ x attrX
            , y attrY
            , height "40"
            , width "40"
            , strokeWidth "0"
            ]
            []


rowOfSquares n =
    List.range 1 n
        |> List.map (\i -> square ((i - 1) * 50) ((n - 1) * 50))


transforms model =
    [ animateTransform
        [ attributeName "transform"
        , type_ "translate"
        , from "0 0"
        , to "-50 50"
        , dur "0.4s"
        , repeatCount "1"
        , fill "freeze"
        , additive "sum"
        , begin "rot1.end"
        ]
        []
    , animateTransform
        [ attributeName "transform"
        , type_ "rotate"
        , from "0"
        , to "90"
        , dur "1s"
        , repeatCount "1"
        , fill "freeze"
        , additive "replace"
        , begin "fwd.click"
        , id "rot1"
        ]
        []
    , animateTransform
        [ attributeName "transform"
        , type_ "rotate"
        , from "90"
        , to "0"
        , dur "1s"
        , repeatCount "1"
        , fill "freeze"
        , additive "replace"
        , begin "trans2.end"
        ]
        []
    , animateTransform
        [ attributeName "transform"
        , type_ "translate"
        , from "0 0"
        , to "50 -50"
        , dur "0.4s"
        , repeatCount "1"
        , fill "freeze"
        , additive "sum"
        , begin "bwd.click"
        , id "trans2"
        ]
        []
    ]


btn model =
    let
        ( color, attrId ) =
            if model.isTransformed == True then
                ( "#4A90E2", "bwd" )
            else
                ( "#ddd", "fwd" )
    in
        rect
            [ onClick DoTransform
            , id attrId
            , x "6"
            , y "106"
            , width "38"
            , height "38"
            , strokeWidth "0"
            , fill color
            ]
            [ set
                [ begin "click" ]
                []
            ]


mainCanvas : Model -> Html.Html Msg
mainCanvas model =
    div []
        [ svg
            [ version "1.1", width "500", height "500", viewBox "0 0 1000 1000", stroke "#444" ]
            [ g [ width "50", height "500", x "0", y "0", fill "#ddd" ]
                [ polygon [ onClick Less, id "less", points "45,45 5,45 25,10", fill "#ddd", strokeWidth "0" ] []
                , polygon [ onClick More, id "more", points "45,45 5,45 25,10", transform "translate(0, 255), scale(1, -1)", fill "#ddd", strokeWidth "0" ] []
                , btn model
                ]
            , g
                ([ width "500", height "500", transform "translate(500, 50) scale(-1, 1)" ])
                [ List.range 1 (model.numberOfRows)
                    |> List.map rowOfSquares
                    |> List.concat
                    |> List.append (transforms model)
                    |> g [ fill "#FDE74C" ]
                ]
            , g [ width "500", height "500", transform "translate(500, 0)" ]
                [ List.range 1 (model.numberOfRows + 1)
                    |> List.map rowOfSquares
                    |> List.concat
                    |> g [ fill "#FA7921" ]
                ]
            ]
        ]
