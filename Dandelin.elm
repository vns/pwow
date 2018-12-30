module Dandelin exposing (..)

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
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type Msg
    = Tick Float
    | Step Int


type alias Model =
    { clock : Float
    , h0 : Animation
    , h1 : Animation
    , theta : Animation
    , clockEnabled : Bool
    , step : Int
    , rotatingPlane : Plane
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
    ( { clock = 0
      , clockEnabled = False
      , step = 0
      , h0 = static 0
      , h1 = static 0
      , theta = static 0
      , rotatingPlane = anotherPlane
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
                , clockEnabled =
                    not ((isDone model.clock model.h0 && isDone model.clock model.h1))
                        || not (isDone model.clock model.theta)
              }
            , Cmd.none
            )

        Step step ->
            let
                h0max =
                    Vec3.sub aCone.vertex (.center <| Cone.sphere0 aCone aPlane)
                        |> Vec3.length

                h1max =
                    Vec3.sub aCone.vertex (.center <| Cone.sphere1 aCone aPlane)
                        |> Vec3.length

                newModel =
                    case step of
                        1 ->
                            { model
                                | h0 = animation model.clock |> from (h0max + 1) |> to h0max |> duration 4000
                                , h1 = animation model.clock |> from 0.1 |> to h1max |> duration 4000
                                , clockEnabled = True
                            }

                        2 ->
                            { model
                                | theta = animation model.clock |> from 0 |> to (2 * pi) |> duration 10000
                                , clockEnabled = True
                            }

                        _ ->
                            model
            in
                ( { newModel | step = step }, Cmd.none )


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
    Plane (Vec3.normalize (vec3 -1.0 3.5 1.0)) (vec3 0.0 0.0 0.0)


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


aTangentPoint0 : Plane -> Vec3
aTangentPoint0 plane =
    aTangentPoint plane (Cone.sphere0 aCone aPlane)


aTangentPoint1 : Plane -> Vec3
aTangentPoint1 plane =
    aTangentPoint plane (Cone.sphere1 aCone aPlane)


aTangentPoint : Plane -> Sphere -> Vec3
aTangentPoint plane sphere =
    let
        direction =
            Mat4.transform (Mat4.makeRotate aCone.angle plane.normal) <|
                Vec3.normalize (Vec3.cross plane.normal aCone.axis)
    in
        Vec3.add sphere.center (Vec3.scale (abs sphere.radius) direction)


aPointOnTheEllipse : Plane -> Vec3
aPointOnTheEllipse plane =
    let
        line =
            Line (aTangentPoint1 plane) (Vec3.sub (aTangentPoint1 plane) (aTangentPoint0 plane))

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


type alias SceneObject =
    { step : Int
    , object : WebGL.Entity
    }


view : Model -> Html Msg
view model =
    let
        h1 =
            animate model.clock model.h1

        h0 =
            animate model.clock model.h0

        theta =
            animate model.clock model.theta

        oldPlane =
            model.rotatingPlane

        rotatingPlane =
            { oldPlane
                | normal = Mat4.transform (Mat4.makeRotate -theta Vec3.j) model.rotatingPlane.normal
            }
    in
        div []
            [ ul []
                [ li [] [ a [ onClick (Step 0) ] [ text "step 0" ] ]
                , li [] [ a [ onClick (Step 1) ] [ text "step 1" ] ]
                , li [] [ a [ onClick (Step 2) ] [ text "step 2" ] ]
                ]
            , WebGL.toHtmlWith
                [ WebGL.alpha True
                , WebGL.antialias
                , WebGL.depth 1
                ]
                [ width 700
                , height 700
                , style "display" "block"
                ]
              <|
                List.map .object <|
                    filter (\x -> x.step <= model.step) <|
                        [ -- object
                          --     Mesh.coordinateAxes
                          --     colors.black
                          -- SPHERE CENTERS
                          SceneObject 0 <|
                            object
                                (Mesh.point <| .center <| Cone.sphere0 aCone aPlane)
                                colors.black
                        , SceneObject 0 <|
                            object
                                (Mesh.point <| .center <| Cone.sphere1 aCone aPlane)
                                colors.black

                        -- LOWER SPHERE
                        , SceneObject 1 <|
                            objectWith
                                [ Blend.add Blend.srcAlpha Blend.one ]
                                (Mesh.bigSphere (animatedSphere (Cone.sphere0 aCone aPlane) h0))
                                colors.sphereGray

                        -- INTERSECTING PLANE
                        , SceneObject 0 <|
                            objectWith
                                [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
                                ]
                                (Mesh.plane aPlane)
                                colors.planeGray

                        -- ELLIPSE
                        , SceneObject 0 <|
                            objectWith
                                [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha ]
                                (Mesh.ellipse anEllipse |> Mesh.fill)
                                colors.fillRed
                        , SceneObject 0 <|
                            objectWith
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
                        , SceneObject 1 <|
                            objectWith
                                [ Blend.add Blend.srcAlpha Blend.one
                                , DepthTest.less { write = False, near = 0.0, far = 1.0 }
                                ]
                                -- (Mesh.sphere (Cone.sphere1 aCone aPlane))
                                (Mesh.sphere (animatedSphere (Cone.sphere1 aCone aPlane) h1))
                                colors.sphereGray

                        -- FOCI
                        , SceneObject 1 <|
                            object
                                (Mesh.point <| .focus0 anEllipse)
                                colors.black
                        , SceneObject 1 <|
                            object
                                (Mesh.point <| .focus1 anEllipse)
                                colors.black

                        -- LINE SEGMENTS ON THE CONE
                        , SceneObject 2 <|
                            object
                                (Mesh.lineSegment (aPointOnTheEllipse rotatingPlane) (.focus0 anEllipse))
                                colors.strokeBlue
                        , SceneObject 2 <|
                            object
                                (Mesh.lineSegment (aPointOnTheEllipse rotatingPlane) (.focus1 anEllipse))
                                colors.strokeGreen

                        -- THE CONE
                        , SceneObject 0 <|
                            objectWith
                                [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
                                , DepthTest.less { write = False, near = 0.0, far = 1.0 }
                                , WebGL.Settings.cullFace WebGL.Settings.back
                                ]
                                (Mesh.cone aCone)
                                colors.coneGray
                        , SceneObject 0 <|
                            objectWith
                                [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
                                ]
                                (Mesh.circle (Cone.bottomSection aCone) |> Mesh.fill)
                                colors.bottomGray

                        -- TANGENT POINTS
                        , SceneObject 2 <|
                            object
                                (Mesh.point (aTangentPoint0 rotatingPlane))
                                colors.black
                        , SceneObject 2 <|
                            object
                                (Mesh.point (aTangentPoint1 rotatingPlane))
                                colors.black

                        -- CIRCLES
                        , SceneObject 2 <|
                            objectWith
                                [ Blend.add Blend.srcAlpha Blend.one ]
                                (Mesh.circle (Cone.circleSection aCone (aTangentPoint0 rotatingPlane)) |> Mesh.fill)
                                colors.fillBlue
                        , SceneObject 2 <|
                            objectWith
                                [ Blend.add Blend.srcAlpha Blend.one ]
                                (Mesh.circle (Cone.circleSection aCone (aTangentPoint0 rotatingPlane)) |> Mesh.stroke)
                                colors.strokeBlue
                        , SceneObject 2 <|
                            objectWith
                                [ Blend.add Blend.srcAlpha Blend.one ]
                                (Mesh.circle (Cone.circleSection aCone (aTangentPoint1 rotatingPlane)) |> Mesh.fill)
                                colors.fillGreen
                        , SceneObject 2 <|
                            objectWith
                                [ Blend.add Blend.srcAlpha Blend.one ]
                                (Mesh.circle (Cone.circleSection aCone (aTangentPoint1 rotatingPlane)) |> Mesh.stroke)
                                colors.strokeGreen
                        , SceneObject 2 <|
                            object
                                (Mesh.point (aPointOnTheEllipse rotatingPlane))
                                colors.black

                        -- LINE SEGMENTS IN THE CONE
                        , SceneObject 2 <|
                            object
                                (Mesh.lineSegment (aPointOnTheEllipse rotatingPlane) (aTangentPoint0 rotatingPlane))
                                colors.strokeBlue
                        , SceneObject 2 <|
                            object
                                (Mesh.lineSegment (aPointOnTheEllipse rotatingPlane) (aTangentPoint1 rotatingPlane))
                                colors.strokeGreen
                        ]
            ]
