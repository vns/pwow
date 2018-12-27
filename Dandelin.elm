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
import Animation exposing (..)


type Msg
    = Tick Float


type alias Model =
    { clock : Float
    , h0 : Animation
    , h1 : Animation
    , clockEnabled : Bool
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
    let
        h0max =
            Vec3.sub aCone.vertex (.center <| Cone.sphere0 aCone aPlane)
                |> Vec3.length

        h1max =
            Vec3.sub aCone.vertex (.center <| Cone.sphere1 aCone aPlane)
                |> Vec3.length
    in
        ( { clock = 0
          , clockEnabled = True
          , h0 = animation 0 |> from (h0max + 1) |> to h0max |> duration 4000
          , h1 = animation 0 |> from 0.1 |> to h1max |> duration 4000
          }
        , Cmd.none
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.clockEnabled then
        Sub.batch
            [ onAnimationFrameDelta Tick
            ]
    else
        Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | clock = model.clock + dt
                , clockEnabled = not (isDone model.clock model.h0 && isDone model.clock model.h1)
              }
            , Cmd.none
            )


camera : Float -> Mat4
camera ratio =
    let
        eye =
            vec3 0 0 11

        center =
            vec3 0 -1.5 0
    in
        (Mat4.makeLookAt eye center Vec3.j)
            |> Mat4.mul (Mat4.makePerspective 45 ratio 0.01 100)


light : Vec3
light =
    vec3 -1 1 3 |> Vec3.normalize


aPlane : Plane
aPlane =
    Plane (Vec3.normalize (vec3 -1.0 3.0 1.0)) (vec3 0.0 0.0 0.0)


anotherPlane : Plane
anotherPlane =
    Plane (Vec3.normalize (vec3 -2.0 0.0 1.0)) (vec3 0.0 0.0 0.0)


aCone : Cone
aCone =
    { vertex = vec3 0.0 2.5 0.0
    , axis = vec3 0.0 -1.0 0.0 |> Vec3.normalize
    , height = 5.0
    , angle = pi / 6
    }


anEllipse : Ellipse
anEllipse =
    Cone.intersectPlane aCone aPlane


animatedSphere : Sphere -> Float -> Sphere
animatedSphere sphere h =
    let
        cone =
            aCone
    in
        { sphere
            | center = Vec3.add cone.vertex (Vec3.scale h cone.axis)
            , radius = h * sin cone.angle
        }


aTangentPoint0 : Vec3
aTangentPoint0 =
    aTangentPoint (Cone.sphere0 aCone aPlane)


aTangentPoint1 : Vec3
aTangentPoint1 =
    aTangentPoint (Cone.sphere1 aCone aPlane)


aTangentPoint : Sphere -> Vec3
aTangentPoint sphere =
    let
        direction =
            Mat4.transform (Mat4.makeRotate aCone.angle anotherPlane.normal) <|
                Vec3.normalize (Vec3.cross anotherPlane.normal aCone.axis)
    in
        Vec3.add sphere.center (Vec3.scale (abs sphere.radius) direction)


aPointOnTheEllipse : Vec3
aPointOnTheEllipse =
    let
        line =
            Line aTangentPoint1 (Vec3.sub aTangentPoint1 aTangentPoint0)

        point =
            Maybe.withDefault (vec3 0 0 0) (Plane.intersectLine aPlane line)
    in
        point


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
    , planeGray : Vec4
    , coneGray : Vec4
    , bottomGray : Vec4
    , strokeBlue : Vec4
    , strokeGreen : Vec4
    , strokeRed : Vec4
    , fillBlue : Vec4
    , fillGreen : Vec4
    , fillRed : Vec4
    }
colors =
    { black = vec4 0.0 0.0 0.0 1
    , sphereGray = vec4 0.7 0.7 0.7 0.6
    , planeGray = vec4 0.3 0.3 0.3 0.5
    , coneGray = vec4 0.41 0.41 0.41 0.6
    , bottomGray = vec4 0.3 0.3 0.3 0.2
    , strokeBlue = vec4 0.0 0.0 1.0 0.8
    , strokeGreen = vec4 0.0 1.0 0.0 0.8
    , strokeRed = vec4 1.0 0.0 0.0 0.8
    , fillBlue = vec4 0.0 0.0 0.5 0.3
    , fillGreen = vec4 0.0 0.5 0.0 0.3
    , fillRed = vec4 0.3 0.0 0.0 0.5
    }


view : Model -> Html msg
view model =
    let
        h1 =
            animate model.clock model.h1

        h0 =
            animate model.clock model.h0
                |> Debug.log "h0"
    in
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
                (Mesh.bigSphere (animatedSphere (Cone.sphere0 aCone aPlane) h0))
                colors.sphereGray

            -- INTERSECTING PLANE
            , objectWith
                [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
                ]
                (Mesh.plane aPlane)
                colors.planeGray

            -- ELLIPSE
            , objectWith
                [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha ]
                (Mesh.ellipse anEllipse |> Mesh.fill)
                colors.fillRed
            , objectWith
                [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha ]
                (Mesh.ellipse anEllipse |> Mesh.stroke)
                colors.strokeRed

            -- , object
            --     (Mesh.line <| Line anEllipse.center anEllipse.majorAxis)
            --     colors.black
            -- , object
            --     (Mesh.line <| Line anEllipse.center anEllipse.minorAxis)
            --     colors.black
            -- UPPER SPHERE
            , objectWith
                [ Blend.add Blend.srcAlpha Blend.one
                , DepthTest.less { write = False, near = 0.0, far = 1.0 }
                ]
                -- (Mesh.sphere (Cone.sphere1 aCone aPlane))
                (Mesh.sphere (animatedSphere (Cone.sphere1 aCone aPlane) h1))
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
                colors.strokeBlue
            , object
                (Mesh.lineSegment aPointOnTheEllipse (.focus1 anEllipse))
                colors.strokeGreen

            -- THE CONE
            , objectWith
                [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
                , DepthTest.less { write = False, near = 0.0, far = 1.0 }
                , WebGL.Settings.cullFace WebGL.Settings.back
                ]
                (Mesh.cone aCone)
                colors.coneGray
            , objectWith
                [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
                ]
                (Mesh.circle (Cone.bottomSection aCone) |> Mesh.fill)
                colors.bottomGray

            -- TANGENT POINTS
            , object
                (Mesh.point aTangentPoint0)
                colors.black
            , object
                (Mesh.point aTangentPoint1)
                colors.black

            -- CIRCLES
            , objectWith
                [ Blend.add Blend.srcAlpha Blend.one ]
                (Mesh.circle (Cone.circleSection aCone aTangentPoint0) |> Mesh.fill)
                colors.fillBlue
            , objectWith
                [ Blend.add Blend.srcAlpha Blend.one ]
                (Mesh.circle (Cone.circleSection aCone aTangentPoint0) |> Mesh.stroke)
                colors.strokeBlue
            , objectWith
                [ Blend.add Blend.srcAlpha Blend.one ]
                (Mesh.circle (Cone.circleSection aCone aTangentPoint1) |> Mesh.fill)
                colors.fillGreen
            , objectWith
                [ Blend.add Blend.srcAlpha Blend.one ]
                (Mesh.circle (Cone.circleSection aCone aTangentPoint1) |> Mesh.stroke)
                colors.strokeGreen
            , object
                (Mesh.point aPointOnTheEllipse)
                colors.black

            -- LINE SEGMENTS IN THE CONE
            , object
                (Mesh.lineSegment aPointOnTheEllipse aTangentPoint0)
                colors.strokeBlue
            , object
                (Mesh.lineSegment aPointOnTheEllipse aTangentPoint1)
                colors.strokeGreen
            ]
