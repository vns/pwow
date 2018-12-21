module Wireframe exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Time
import WebGL exposing (Mesh, Shader)
import List exposing (..)
import List.Extra exposing (zip, last)
import Maybe


type Msg
    = Tick Float


type alias Model =
    { time : Float
    , angle : Float
    }


type alias Vertex =
    { position : Vec3
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0
      , angle = 0
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta ((\dt -> dt / 1000) >> Tick)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | time = dt + model.time
                , angle = model.angle + pi / 180
              }
            , Cmd.none
            )


camera : Float -> Mat4
camera ratio =
    let
        eye =
            vec3 0 3 10

        center =
            vec3 0 0 0
    in
        (Mat4.makeLookAt eye center Vec3.j)
            |> Mat4.mul (Mat4.makePerspective 45 ratio 0.01 100)


light : Vec3
light =
    vec3 -1 1 10 |> Vec3.normalize


mesh : Mesh Vertex
mesh =
    let
        vertices =
            cylinder

        sideLines =
            vertices
                |> makeTuples zip
                |> concatMap identity

        bottomLines =
            case last vertices of
                Just bottom ->
                    makeTuples (\x y -> ( x, y )) bottom

                Nothing ->
                    []
    in
        append sideLines bottomLines
            |> WebGL.lines



-- WebGL.lines
--     [ ( Vertex (vec3 0 0 0)
--       , Vertex (vec3 1 1 0)
--       )
--     , ( Vertex (vec3 1 1 0)
--       , Vertex (vec3 1 -1 0)
--       )
--     , ( Vertex (vec3 1 -1 0)
--       , Vertex (vec3 0 0 0)
--       )
--     , ( Vertex (vec3 1 -1 0)
--       , Vertex (vec3 0 0 2)
--       )
--     ]


cylinder =
    let
        heightSegments =
            1

        radialSegments =
            15

        height =
            3

        halfHeight =
            height / 2

        radiusBottom =
            1.5

        radiusTop =
            0

        slope =
            (radiusBottom - radiusTop) / height

        thetaLength =
            2 * pi
    in
        range 0 heightSegments
            |> map
                (\y ->
                    let
                        v =
                            toFloat y / heightSegments

                        radius =
                            v * (radiusBottom - radiusTop) + radiusTop
                    in
                        range 0 radialSegments
                            |> map
                                (\x ->
                                    let
                                        u =
                                            (toFloat x / radialSegments)

                                        theta =
                                            (u * thetaLength - 0.00001)

                                        sinTheta =
                                            sin theta

                                        cosTheta =
                                            cos theta

                                        p0 =
                                            vec3 (radius * sinTheta) (-v * height + halfHeight) (radius * cosTheta)
                                    in
                                        Vertex p0
                                )
                )


makeTuples fn list =
    case list of
        a :: b :: rest ->
            fn a b :: (makeTuples fn <| b :: rest)

        _ ->
            []


view : Model -> Html msg
view model =
    WebGL.toHtml
        [ width 700
        , height 700
        , style "display" "block"
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh
            (Uniforms
                (camera 1)
            )
        ]


type alias Uniforms =
    { perspective : Mat4 }


vertexShader : Shader Vertex Uniforms {}
vertexShader =
    [glsl|

        attribute vec3 position;
        uniform mat4 perspective;

        void main () {
            gl_Position = perspective * vec4(position, 1.0);
        }

    |]


fragmentShader : Shader {} Uniforms {}
fragmentShader =
    [glsl|

        precision mediump float;

        void main () {
            gl_FragColor = vec4(vec3(0.3, 0.3, 0.3), 1.0);
        }

    |]
