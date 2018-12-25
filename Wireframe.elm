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
import Plane exposing (Plane)
import Cone exposing (Cone)
import Ellipse exposing (Ellipse)
import Sphere exposing (Sphere)


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
            (coneMesh aCone)
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


aLine : Plane.Line
aLine =
    Plane.Line
        (Vec3.add (vec3 -0.37 -0.29 0.37) (Vec3.scale 1.85 (vec3 -0.61 -0.49 0.61)))
        (vec3 0.0 1.0 0.0)


aCone : Cone
aCone =
    { vertex = vec3 0.0 2.5 0.0
    , axis = vec3 0.0 -1.0 0.0 |> Vec3.normalize
    , height = 5.0
    , angle = pi / 6
    }


lineMesh : Plane.Line -> Mesh Vertex
lineMesh line =
    let
        offset =
            Vec3.scale 3.0 line.direction
    in
        WebGL.lines
            [ ( Vertex (Vec3.add line.origin offset)
              , Vertex (Vec3.sub line.origin offset)
              )
            ]


planeMesh : Plane -> Mesh Vertex
planeMesh plane =
    let
        line =
            aLine

        intersect =
            Plane.intersectLine plane line

        vertices =
            Plane.toMesh plane
                |> map Vertex

        -- indices =
        --     [ ( 0, 1, 2 )
        --     , ( 0, 3, 2 )
        --     ]
        indices =
            [ ( 1, 2, 3 )
            , ( 0, 1, 3 )
            ]
    in
        WebGL.indexedTriangles vertices indices


normalMesh : Plane -> Mesh Vertex
normalMesh plane =
    let
        transform =
            Plane.makeTransform plane
    in
        [ ( vec3 0.0 0.0 0.0 |> transform |> Vertex
          , vec3 0.0 1.0 0.0 |> transform |> Vertex
          )
        ]
            |> WebGL.lines


coordinateMesh : Mesh Vertex
coordinateMesh =
    WebGL.lines
        [ ( Vertex (vec3 -10 0 0), Vertex (vec3 10 0 0) )
        , ( Vertex (vec3 0 -10 0), Vertex (vec3 0 10 0) )
        , ( Vertex (vec3 0 0 -10), Vertex (vec3 0 0 10) )
        ]


coneMesh : Cone -> Mesh Vertex
coneMesh cone =
    let
        ( vertices, indexes ) =
            Cone.toMesh cone
    in
        WebGL.indexedTriangles (vertices |> map Vertex) indexes


sphereMesh =
    let
        ( vertices, indexes ) =
            Sphere.toMesh (Sphere 0.77 (vec3 0.0 0.9 0.0))
    in
        WebGL.indexedTriangles (vertices |> map Vertex) indexes


ellipseMesh : Mesh Vertex
ellipseMesh =
    let
        ellipse =
            Cone.intersectPlane aCone aPlane
    in
        Ellipse.toMesh ellipse
            |> map Vertex
            |> makeTuples (\x y -> ( x, y ))
            |> WebGL.lines


makeTuples fn list =
    case list of
        a :: b :: rest ->
            fn a b :: (makeTuples fn <| b :: rest)

        _ ->
            []


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
          --     (lineMesh aLine)
          --     (Uniforms
          --         (camera 1)
          --         (vec4 0.8 0.0 0.0 1.0)
          --     )
          -- , WebGL.entity
          --     vertexShader
          --     fragmentShader
          --     coordinateMesh
          --     (Uniforms
          --         (camera 1)
          --         (vec4 0.7 0.7 0.7 1.0)
          --     )
          -- WebGL.entityWith
          --   [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
          --   , DepthTest.less { write = True, near = 0.0, far = 1.0 }
          --   ]
          --   vertexShader
          --   fragmentShader
          --   (planeMesh aPlane)
          --   (Uniforms
          --       (camera 1)
          --       (vec4 (0x69 / 0xFF) (0x69 / 0xFF) (0x69 / 0xFF) 1)
          --   )
          WebGL.entity
            vertexShader
            fragmentShader
            (sphereMesh)
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
            (ellipseMesh)
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
