module Main exposing (main)

import Browser exposing (Document)
import Color.Palette exposing (darkRed, darkGreen, white)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Animation as Anim
import List exposing (map, range)


main =
    Browser.document
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

    -- , rect1 : Anim.State
    -- , rect2 : Anim.State
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tria1 = Anim.style [ Anim.fill darkRed, Anim.points initialTria ]
      , tria2 = Anim.style [ Anim.fill white, Anim.points initialTria ]
      , tria3 = Anim.style [ Anim.fill white, Anim.points initialTria ]
      , tria4 = Anim.style [ Anim.fill white, Anim.points initialTria ]
      , labels =
            { csq = Anim.style [ Anim.display Anim.none ]
            , asq = Anim.style [ Anim.display Anim.none ]
            , bsq = Anim.style [ Anim.display Anim.none ]
            }
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
    | Step0
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step0 ->
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
                    [ Anim.points tria1, Anim.fill darkRed ]

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
                ( { model
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
                , Cmd.none
                )

        Step1 ->
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
                    [ Anim.points tria1, Anim.fill darkRed ]

                newTria2Style =
                    [ Anim.points tria2, Anim.fill darkGreen ]

                newTria3Style =
                    [ Anim.points tria3, Anim.fill darkRed ]

                newTria4Style =
                    [ Anim.points tria4, Anim.fill darkGreen ]

                oldLabels =
                    model.labels

                newLabels =
                    { oldLabels
                        | csq = Anim.interrupt [ Anim.set [ Anim.display Anim.block ] ] oldLabels.csq
                        , asq = Anim.interrupt [ Anim.set [ Anim.display Anim.none ] ] oldLabels.asq
                        , bsq = Anim.interrupt [ Anim.set [ Anim.display Anim.none ] ] oldLabels.bsq
                    }
            in
                ( { model
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
                , Cmd.none
                )

        Step2 ->
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
                ( { model
                    | tria1 = Anim.interrupt [ Anim.to newTria1Style ] model.tria1
                    , tria2 = Anim.interrupt [ Anim.to newTria2Style ] model.tria2
                    , tria3 = Anim.interrupt [ Anim.to newTria3Style ] model.tria3
                    , tria4 = Anim.interrupt [ Anim.to newTria4Style ] model.tria4
                    , labels = newLabels
                  }
                , Cmd.none
                )

        Step3 ->
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
                    , Anim.fill darkRed
                    ]

                newTria2Style =
                    [ Anim.points tria2
                    , Anim.fill darkGreen
                    ]

                newTria3Style =
                    [ Anim.points tria3
                    , Anim.fill darkRed
                    ]

                newTria4Style =
                    [ Anim.points tria4
                    , Anim.fill darkGreen
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
                ( { model
                    | tria1 = Anim.interrupt [ Anim.to newTria1Style ] model.tria1
                    , tria2 = Anim.interrupt [ Anim.to newTria2Style ] model.tria2
                    , tria3 = Anim.interrupt [ Anim.to newTria3Style ] model.tria3
                    , tria4 = Anim.interrupt [ Anim.to newTria4Style ] model.tria4
                    , labels = newLabels
                  }
                , Cmd.none
                )

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


view : Model -> Document Msg
view model =
    { title = "Pythagorean Theorem"
    , body =
        [ div [ Attr.class "header" ]
            [ h1 [] [ Html.text "proofs without words" ]
            ]
        , div [ Attr.class "content" ]
            [ div [ Attr.class "sidebar" ]
                [ ul []
                    [ li [] [ Html.a [ Attr.href "#", Attr.class "active" ] [ Html.text "pythagorean theorem" ] ]
                    , li [] [ Html.a [ Attr.href "#" ] [ Html.text "viviani's theorem" ] ]
                    , li [] [ Html.a [ Attr.href "#" ] [ Html.text "infinite series" ] ]
                    ]
                ]
            , div [ Attr.class "steps" ]
                [ span [ onClick Step0, Attr.class "step active" ] []
                , span [ onClick Step1, Attr.class "step" ] []
                , span [ onClick Step2, Attr.class "step" ] []
                , span [ onClick Step3, Attr.class "step" ] []
                ]
            , div [ Attr.class "main" ]
                [ div
                    [ Attr.class "canvas-container"
                    , Attr.style "width" "500px"
                    , Attr.style "height" "500px"
                    ]
                    [ mainCanvas model ]
                ]
            ]
        ]
    }


mainCanvas : Model -> Html.Html msg
mainCanvas model =
    div []
        [ svg
            [ version "1.1", width "500", height "500", viewBox "0 0 700 700", stroke "#999" ]
            [ g []
                [ rect [ fill "white", stroke "1", x "0", y "0", width "700", height "700" ] []
                , polygon (Anim.render model.tria2) []
                , polygon (Anim.render model.tria3) []
                , polygon (Anim.render model.tria4) []
                , polygon (Anim.render model.tria1) []

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
