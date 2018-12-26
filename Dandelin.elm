module Wireframe exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Time
import WebGL
import WebGL.Settings exposing (Setting, FaceMode)
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
import Dandelin.Mesh as Mesh
import Dandelin.Shader as Shader exposing (Attributes, Uniforms)


type Msg
    = Tick Float


type alias Model =
    { time : Float
    , angle : Float
    , cone : WebGL.Mesh Attributes
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
            Mesh.cone aCone
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [-- onAnimationFrameDelta ((\dt -> dt / 1000) >> Tick)
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
            vec3 4 1 10

        center =
            vec3 0 -1 0
    in
        (Mat4.makeLookAt eye center Vec3.j)
            |> Mat4.mul (Mat4.makePerspective 45 ratio 0.01 100)


light : Vec3
light =
    vec3 -1 1 3 |> Vec3.normalize


aPlane : Plane
aPlane =
    Plane (Vec3.normalize (vec3 -1.0 2.5 1.0)) (vec3 0.0 0.0 0.0)


anotherPlane : Plane
anotherPlane =
    Plane (Vec3.normalize (vec3 -2.0 0.0 1.0)) (vec3 0.0 0.0 0.0)


aTangentPoint0 =
    let
        plane =
            anotherPlane

        cone =
            aCone

        sphere0 =
            Cone.sphere0 aCone aPlane

        direction =
            Mat4.transform (Mat4.makeRotate cone.angle plane.normal) <|
                Vec3.normalize (Vec3.cross plane.normal cone.axis)

        -- coord =
        --     Vec3.sub sphere0.center (Vec3.scale sphere0.radius direction)
    in
        Vec3.sub sphere0.center (Vec3.scale sphere0.radius direction)


aTangentPoint1 =
    let
        plane =
            anotherPlane

        cone =
            aCone

        sphere1 =
            Cone.sphere1 aCone aPlane

        direction =
            Mat4.transform (Mat4.makeRotate cone.angle plane.normal) <|
                Vec3.normalize (Vec3.cross plane.normal cone.axis)

        -- coord =
        --     Vec3.sub sphere0.center (Vec3.scale sphere0.radius direction)
    in
        Vec3.add sphere1.center (Vec3.scale sphere1.radius direction)


aPointOnTheEllipse =
    let
        line =
            Line aTangentPoint1 (Vec3.sub aTangentPoint1 aTangentPoint0)

        point =
            Maybe.withDefault (vec3 0 0 0) (Plane.intersectLine aPlane line)
    in
        point


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
    (Sphere 0.75 (vec3 0.0 0.9 0.0))


anEllipse : Ellipse
anEllipse =
    Cone.intersectPlane aCone aPlane


aPoint : Vec3 -> Sphere
aPoint center =
    (Sphere 0.025 center)


objectWith : List Setting -> WebGL.Mesh Attributes -> Vec4 -> WebGL.Entity
objectWith settings mesh color =
    WebGL.entityWith
        settings
        Shader.vertex
        Shader.fragment
        mesh
        (Shader.Uniforms
            (camera 1)
            (color)
        )


object : WebGL.Mesh Attributes -> Vec4 -> WebGL.Entity
object mesh color =
    objectWith [] mesh color


colors :
    { black : Vec4
    , sphereGray : Vec4
    , ellipseGray : Vec4
    , coneGray : Vec4
    , blue : Vec4
    , green : Vec4
    }
colors =
    { black = vec4 0.0 0.0 0.0 1
    , sphereGray = vec4 0.7 0.7 0.7 0.6
    , ellipseGray = vec4 0.3 0.3 0.3 0.5
    , coneGray = vec4 0.41 0.41 0.41 0.6
    , blue = vec4 0.0 0.0 1.0 1
    , green = vec4 0.0 1.0 1.0 1
    }


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
        [ -- object
          --     Mesh.coordinateAxes
          --     colors.black
          -- SPHERE CENTERS
          object
            (Mesh.point <| .center <| Cone.sphere0 aCone aPlane)
            colors.black
        , object
            (Mesh.point <| .center <| Cone.sphere1 aCone aPlane)
            colors.black

        -- LOWER SPHERE
        , objectWith
            [ Blend.add Blend.srcAlpha Blend.one ]
            (Mesh.sphere (Cone.sphere0 aCone aPlane))
            colors.sphereGray

        -- ELLIPSE
        , objectWith
            [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha ]
            (Mesh.ellipse anEllipse)
            colors.ellipseGray

        -- UPPER SPHERE
        , objectWith
            [ Blend.add Blend.srcAlpha Blend.one ]
            (Mesh.sphere (Cone.sphere1 aCone aPlane))
            colors.sphereGray

        -- FOCI
        , object
            (Mesh.point <| .focus0 anEllipse)
            colors.black
        , object
            (Mesh.point <| .focus1 anEllipse)
            colors.black

        -- LINE SEGMENTS ON THE CONE
        , object
            (Mesh.lineSegment aPointOnTheEllipse (.focus0 anEllipse))
            colors.blue
        , object
            (Mesh.lineSegment aPointOnTheEllipse (.focus1 anEllipse))
            colors.green

        -- THE CONE
        , objectWith
            [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
            , DepthTest.less { write = False, near = 0.0, far = 1.0 }
            , WebGL.Settings.cullFace WebGL.Settings.back
            ]
            (Mesh.cone aCone)
            colors.coneGray

        -- INTERSECTING PLANE
        -- , objectWith
        --     [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
        --     , DepthTest.less { write = True, near = 0.0, far = 1.0 }
        --     ]
        --     (Mesh.plane aPlane)
        --     colors.ellipseGray
        -- TANGENT POINTS
        , object
            (Mesh.point aTangentPoint0)
            colors.black
        , object
            (Mesh.point aTangentPoint1)
            colors.black
        , object
            (Mesh.point aPointOnTheEllipse)
            colors.black

        -- LINE SEGMENTS IN THE CONE
        , object
            (Mesh.lineSegment aPointOnTheEllipse aTangentPoint0)
            colors.blue
        , object
            (Mesh.lineSegment aPointOnTheEllipse aTangentPoint1)
            colors.green
        ]
