module Wireframe exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Time
import WebGL exposing (Mesh, Shader)
import WebGL.Settings exposing (back, front)
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest
import List exposing (..)
import List.Extra exposing (zip, last)
import Maybe
import Geometry.Line as Line exposing (Line)
import Geometry.Plane as Plane exposing (Plane)
import Geometry.Cone as Cone exposing (Cone)
import Geometry.Ellipse as Ellipse exposing (Ellipse)
import Geometry.Sphere as Sphere exposing (Sphere)
import Dandelin.Mesh


type Msg
    = Tick Float


type alias Model =
    { time : Float
    , angle : Float
    , cone : Mesh Vertex
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
      , cone =
            Dandelin.Mesh.cone aCone
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
            vec3 4 3 10

        center =
            vec3 0 0 0
    in
        (Mat4.makeLookAt eye center Vec3.j)
            |> Mat4.mul (Mat4.makePerspective 45 ratio 0.01 100)


light : Vec3
light =
    vec3 -1 1 3 |> Vec3.normalize


aPlane : Plane
aPlane =
    Plane (Vec3.normalize (vec3 -1.0 2.5 1.0)) (vec3 0.0 0.0 0.0)


aLine : Line
aLine =
    Line
        (Vec3.add (vec3 -0.37 -0.29 0.37) (Vec3.scale 1.85 (vec3 -0.61 -0.49 0.61)))
        (vec3 0.0 1.0 0.0)


aCone : Cone
aCone =
    { vertex = vec3 0.0 2.5 0.0
    , axis = vec3 0.0 -1.0 0.0 |> Vec3.normalize
    , height = 5.0
    , angle = pi / 6
    }


aSphere : Sphere
aSphere =
    (Sphere 0.77 (vec3 0.0 0.9 0.0))


view : Model -> Html msg
view model =
    WebGL.toHtmlWith
        [ WebGL.alpha True
        , WebGL.antialias
        , WebGL.depth 1
        ]
        [ width 700
        , height 700
        , style "display" "block"
        ]
        [ -- [ WebGL.entity
          --     vertexShader
          --     fragmentShader
          --     (Dandelin.Mesh.line aLine)
          --     (Uniforms
          --         (camera 1)
          --         (vec4 0.8 0.0 0.0 1.0)
          --     )
          -- WebGL.entity
          --   vertexShader
          --   fragmentShader
          --   (Dandelin.Mesh.coordinateAxes)
          --   (Uniforms
          --       (camera 1)
          --       (vec4 0.7 0.7 0.7 1.0)
          --   )
          WebGL.entityWith
            [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
            , DepthTest.less { write = True, near = 0.0, far = 1.0 }
            ]
            vertexShader
            fragmentShader
            (Dandelin.Mesh.plane aPlane)
            (Uniforms
                (camera 1)
                (vec4 (0x69 / 0xFF) (0x69 / 0xFF) (0x69 / 0xFF) 1)
            )
        , WebGL.entity
            vertexShader
            fragmentShader
            (Dandelin.Mesh.sphere aSphere)
            (Uniforms
                (camera 1)
                (vec4 0.6 0.6 0.6 1.0)
            )
        , WebGL.entityWith
            [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
            , DepthTest.less { write = False, near = 0.0, far = 1.0 }

            -- , WebGL.Settings.cullFace back
            ]
            vertexShader
            fragmentShader
            model.cone
            (Uniforms
                (camera 1)
                (vec4 (0x69 / 0xFF) (0x69 / 0xFF) (0x69 / 0xFF) 0.6)
            )

        -- , WebGL.entity
        --     vertexShader
        --     fragmentShader
        --     (normalMesh aPlane)
        --     (Uniforms
        --         (camera 1)
        --         (vec4 0.9 0.0 0.0 1.0)
        --     )
        , WebGL.entity
            vertexShader
            fragmentShader
            (Dandelin.Mesh.ellipse aCone aPlane)
            (Uniforms
                (camera 1)
                (vec4 0.6 0.6 0.6 1.0)
            )
        ]


type alias Uniforms =
    { perspective : Mat4
    , color : Vec4
    }


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
        uniform vec4 color;

        void main () {
            gl_FragColor = vec4(color);
        }

    |]
