module Main exposing (main)

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
    , mesh : Mesh Attributes
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
      , mesh = mesh
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
                , angle = model.angle + pi / 360
              }
            , Cmd.none
            )


camera : Float -> Mat4
camera ratio =
    let
        eye =
            vec3 0 0 10

        center =
            vec3 0 0 0
    in
        (Mat4.makeLookAt eye center Vec3.j)
            |> Mat4.mul (Mat4.makePerspective 45 ratio 0.01 100)


light : Vec3
light =
    vec3 -1 1 10 |> Vec3.normalize


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
            model.mesh
            (Uniforms
                -- (vec3 (0x9B / 0xFF) (0xC5 / 0xFF) (0x3D / 0xFF))
                (vec3 (0xE5 / 0xFF) (0x59 / 0xFF) (0x34 / 0xFF))
                (camera 1)
                (Mat4.makeRotate model.angle (vec3 0 1 0))
                light
            )
        ]


sphere u v =
    let
        maxTheta =
            pi

        maxPhi =
            2 * pi

        theta =
            u * maxTheta

        phi =
            v * maxPhi
    in
        vec3 (sin theta * cos phi) (sin theta * sin phi) (cos theta)


mobius u v =
    let
        phi =
            u - 0.5

        theta =
            2 * pi * v

        a =
            2
    in
        vec3
            ((cos theta) * (a + phi * cos (theta / 2)))
            ((sin theta) * (a + phi * cos (theta / 2)))
            (phi * sin (theta / 2))


mobius3d u0 v0 =
    let
        u =
            u0 * pi * 2

        phi =
            u / 2

        v =
            v0 * pi * 2

        major =
            2.25

        a =
            0.125

        b =
            0.65

        x =
            (a * (cos v) * (cos phi) - b * (sin v) * (sin phi))

        z =
            (a * (cos v) * (sin phi) + b * (sin v) * (cos phi))
    in
        vec3
            ((major + x) * (cos u))
            ((major + x) * (sin u))
            z


mesh =
    let
        ( attr, indices ) =
            triangles mobius3d
    in
        WebGL.indexedTriangles attr indices


triangles fn =
    let
        slices =
            60

        stacks =
            60

        eps =
            0.0001
    in
        range 0 stacks
            |> map
                (\i ->
                    range 0 slices
                        |> map
                            (\j ->
                                let
                                    u =
                                        toFloat j / slices

                                    v =
                                        toFloat i / stacks

                                    p0 =
                                        fn u v

                                    pu =
                                        if (u - eps >= 0) then
                                            Vec3.sub p0 (fn (u - eps) v)
                                        else
                                            Vec3.sub (fn (u + eps) v) p0

                                    pv =
                                        if (v - eps >= 0) then
                                            Vec3.sub p0 (fn u (v - eps))
                                        else
                                            Vec3.sub (fn u (v + eps)) p0

                                    normal =
                                        Vec3.cross pu pv |> Vec3.normalize
                                in
                                    ( Attributes p0 normal, indexes slices stacks i j )
                            )
                )
            |> concatMap identity
            |> foldl (\( v, i ) ( av, ai ) -> ( v :: av, i ++ ai )) ( [], [] )


indexes slices stacks i j =
    let
        sliceCount =
            slices + 1

        a =
            i * sliceCount + j

        b =
            i * sliceCount + j + 1

        c =
            (i + 1) * sliceCount + j + 1

        d =
            (i + 1) * sliceCount + j
    in
        if (i >= stacks) || (j >= slices) then
            []
        else
            [ ( a, b, d )
            , ( b, c, d )
            ]


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
            gl_FragColor = vec4(color * vlighting, 1);
        }
    |]
