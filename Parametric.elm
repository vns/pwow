module Parametric exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import List exposing (..)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Time
import WebGL exposing (Mesh, Shader)


type Msg
    = Tick Float


type alias Model =
    { time : Float
    , angle : Float
    }


type alias Attributes =
    { position : Vec3
    , normal : Vec3
    }


type alias Uniforms =
    { color : Vec3
    , camera : Mat4
    , rotation : Mat4
    , light : Vec3
    }


type alias Varying =
    { vlighting : Float
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
        [ onAnimationFrameDelta ((\dt -> dt / 1000) >> Tick) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | time = dt + model.time
                , angle = model.angle + model.time / 5000
              }
            , Cmd.none
            )


view : Model -> Html msg
view model =
    WebGL.toHtml
        [ width 1000
        , height 1000
        , style "display" "block"
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh
            (Uniforms
                (vec3 0.2 0.2 1)
                (camera 1)
                (Mat4.makeRotate model.angle (vec3 0 1 0))
                light
            )
        ]


attributes : Vec3 -> Attributes
attributes vec =
    Attributes vec (vec |> Vec3.normalize)


sphereFn phi theta =
    vec3 (sin theta * cos phi) (sin theta * sin phi) (cos theta)


mesh =
    let
        ( attr, indices ) =
            triangles sphereFn
    in
        WebGL.indexedTriangles attr indices


triangles fn =
    let
        phlen =
            2 * pi

        thlen =
            pi

        width =
            25

        height =
            25
    in
        range 0 height
            |> map
                (\i ->
                    range 0 width
                        |> map
                            (\j ->
                                let
                                    phi =
                                        ((toFloat j / width) * phlen)

                                    theta =
                                        ((toFloat i / height) * thlen)
                                in
                                    ( attributes (fn phi theta), indexes width height i j )
                            )
                )
            |> concatMap identity
            |> foldl (\( v, i ) ( av, ai ) -> ( v :: av, i ++ ai )) ( [], [] )


indexes width height i j =
    let
        w =
            width + 1

        h =
            height + 1
    in
        if (i >= height) || (j >= height) then
            []
        else
            [ ( i * w + j
              , i * w + j + 1
              , (i + 1) * w + j
              )
            , ( i * w + j + 1
              , (i + 1) * w + j
              , (i + 1) * w + j + 1
              )
            ]


camera : Float -> Mat4
camera ratio =
    let
        c =
            0

        eye =
            vec3 0 0 3

        center =
            vec3 0 0 0
    in
        (Mat4.makeLookAt eye center Vec3.j)
            |> Mat4.mul (Mat4.makePerspective 45 ratio 0.01 100)


light : Vec3
light =
    vec3 -1 1 3 |> Vec3.normalize


vertexShader : Shader Attributes Uniforms Varying
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 normal;
        uniform mat4 camera;
        uniform mat4 rotation;
        uniform vec3 light;
        varying highp float vlighting;

        void main () {
            highp float ambientLight = 0.5;
            highp float directionalLight = 0.5;
            gl_Position = camera * rotation * vec4(position, 1.0);
            vlighting = ambientLight + max(dot((rotation * vec4(normal, 1.0)).xyz, light), 0.0) * directionalLight;
        }


    |]


fragmentShader : Shader {} Uniforms Varying
fragmentShader =
    [glsl|

        precision mediump float;
        varying highp float vlighting;
        uniform vec3 color;

        void main () {
            gl_FragColor = vec4(color * vlighting, 1.0);
        }
    |]
