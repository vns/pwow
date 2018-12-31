module Main exposing (main)

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
    , pq1len : Animation
    , pf1len : Animation
    , pq0len : Animation
    , pf0len : Animation
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

        ( pq1, pf1 ) =
            upperTangents anotherPlane
    in
        ( { clock = 0
          , clockEnabled = False
          , step = 0
          , h0 = animations.h0 0
          , h1 = animations.h1 0
          , theta = animations.theta 0
          , pq1len = animations.pq1len 0
          , pf1len = animations.pf1len 0
          , pq0len = animations.pq0len 0
          , pf0len = animations.pf0len 0
          }
        , Cmd.none
        )


animations :
    { h0 : Clock -> Animation
    , h1 : Clock -> Animation
    , theta : Clock -> Animation
    , pq1len : Clock -> Animation
    , pf1len : Clock -> Animation
    , pq0len : Clock -> Animation
    , pf0len : Clock -> Animation
    }
animations =
    let
        h0max =
            Vec3.sub aCone.vertex (.center <| Cone.sphere0 aCone aPlane)
                |> Vec3.length

        h1max =
            Vec3.sub aCone.vertex (.center <| Cone.sphere1 aCone aPlane)
                |> Vec3.length

        ( pq1, pf1 ) =
            upperTangents anotherPlane

        ( pq0, pf0 ) =
            lowerTangents anotherPlane
    in
        { h0 = \clock -> animation clock |> from (h0max + 1) |> to h0max |> duration 3000
        , h1 = \clock -> animation clock |> from 0.1 |> to h1max |> duration 3000
        , theta = \clock -> animation clock |> from 0 |> to (2 * pi) |> duration 10000
        , pq1len = \clock -> animation clock |> from 0 |> to (Vec3.length pq1) |> duration 2000
        , pf1len = \clock -> animation clock |> from 0 |> to (Vec3.length pf1) |> duration 2000
        , pq0len = \clock -> animation clock |> from 0 |> to (Vec3.length pq0) |> duration 2000
        , pf0len = \clock -> animation clock |> from 0 |> to (Vec3.length pf0) |> duration 2000
        }


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
                    not (isDone model.clock model.h0)
                        || not (isDone model.clock model.h1)
                        || not (isDone model.clock model.theta)
                        || not
                            (isDone model.clock model.pq1len
                                && isDone model.clock model.pf1len
                                && isDone model.clock model.pq0len
                                && isDone model.clock model.pf0len
                            )
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

                ( pq1, pf1 ) =
                    upperTangents anotherPlane

                ( pq0, pf0 ) =
                    lowerTangents anotherPlane

                newModel =
                    case step of
                        1 ->
                            { model
                                | h1 = animations.h1 model.clock
                            }

                        2 ->
                            { model
                                | h0 = animations.h0 model.clock
                            }

                        3 ->
                            { model
                                | pq1len = animations.pq1len model.clock
                                , pf1len = animations.pf1len model.clock
                                , pq0len = animations.pq0len model.clock
                                , pf0len = animations.pf0len model.clock
                            }

                        4 ->
                            { model
                                | theta = animations.theta model.clock
                            }

                        _ ->
                            model
            in
                ( { newModel | step = step, clockEnabled = True }, Cmd.none )


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


animatedSphere : Float -> Sphere -> Sphere
animatedSphere h sphere =
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


upperTangents rotatingPlane =
    let
        p =
            aPointOnTheEllipse rotatingPlane

        q1 =
            aTangentPoint1 rotatingPlane

        pq1 =
            Vec3.sub q1 p

        f1 =
            .focus1 anEllipse

        pf1 =
            Vec3.sub f1 p
    in
        ( pq1, pf1 )


lowerTangents rotatingPlane =
    let
        p =
            aPointOnTheEllipse rotatingPlane

        q0 =
            aTangentPoint0 rotatingPlane

        pq0 =
            Vec3.sub q0 p

        f0 =
            .focus0 anEllipse

        pf0 =
            Vec3.sub f0 p
    in
        ( pq0, pf0 )


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
    , transparent : Vec4
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
    , transparent = vec4 1.0 1.0 1.0 0.0
    }


type alias SceneObject =
    { step : Int
    , object : WebGL.Entity
    }


stepView : Int -> Int -> Html Msg
stepView aStep currentStep =
    let
        stepClass =
            "step"
                ++ if aStep == currentStep then
                    " active"
                   else
                    ""
    in
        span [ onClick (Step aStep), class stepClass ] []


view : Model -> Html Msg
view model =
    div
        [ class "presentation" ]
        [ div [ class "steps" ]
            [ stepView 0 model.step
            , stepView 1 model.step
            , stepView 2 model.step
            , stepView 3 model.step
            , stepView 4 model.step
            ]
        , div
            [ class "canvas-container"
            ]
            [ mainCanvas model ]
        ]


mainCanvas : Model -> Html Msg
mainCanvas model =
    let
        h1 =
            (if model.step == 1 then
                animate model.clock model.h1
             else
                getTo model.h1
            )

        h0 =
            (if model.step == 2 then
                animate model.clock model.h0
             else
                getTo model.h0
            )

        rotatingPlane =
            { anotherPlane
                | normal = Mat4.transform (Mat4.makeRotate -theta Vec3.j) anotherPlane.normal
            }

        ( pq1len, pf1len ) =
            if model.step == 3 then
                ( animate model.clock model.pq1len
                , animate model.clock model.pf1len
                )
            else
                upperTangents rotatingPlane
                    |> \( pq1, pf1 ) -> ( Vec3.length pq1, Vec3.length pf1 )

        ( pq1unit, pf1unit ) =
            upperTangents rotatingPlane
                |> \( pq1, pf1 ) -> ( Vec3.normalize pq1, Vec3.normalize pf1 )

        ( pq0len, pf0len ) =
            if model.step == 3 then
                ( animate model.clock model.pq0len
                , animate model.clock model.pf0len
                )
            else
                lowerTangents rotatingPlane
                    |> \( pq0, pf0 ) -> ( Vec3.length pq0, Vec3.length pf0 )

        ( pq0unit, pf0unit ) =
            lowerTangents rotatingPlane
                |> \( pq0, pf0 ) -> ( Vec3.normalize pq0, Vec3.normalize pf0 )

        theta =
            if model.step == 4 then
                animate model.clock model.theta
            else
                getTo model.theta
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
        <|
            List.map .object <|
                filter (\x -> x.step <= model.step) <|
                    [ -- object
                      --     Mesh.coordinateAxes
                      --     colors.black
                      -- SPHERE CENTERS
                      SceneObject 2 <|
                        object
                            (Mesh.point <| .center <| animatedSphere h0 <| Cone.sphere0 aCone aPlane)
                            colors.black
                    , SceneObject 1 <|
                        object
                            (Mesh.point <| .center <| animatedSphere h1 <| Cone.sphere1 aCone aPlane)
                            colors.black

                    -- LOWER SPHERE
                    , SceneObject 2 <|
                        objectWith
                            [ Blend.add Blend.srcAlpha Blend.one ]
                            (Mesh.bigSphere <| animatedSphere h0 <| Cone.sphere0 aCone aPlane)
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
                            (Mesh.sphere <| animatedSphere h1 <| Cone.sphere1 aCone aPlane)
                            colors.sphereGray

                    -- FOCI
                    , SceneObject 1 <|
                        objectWith
                            [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha ]
                            (Mesh.point <| .focus1 anEllipse)
                            (if h1 == getTo model.h1 then
                                colors.black
                             else
                                colors.transparent
                            )
                    , SceneObject 2 <|
                        objectWith
                            [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha ]
                            (Mesh.point <| .focus0 anEllipse)
                            (if h0 == getTo model.h0 then
                                colors.black
                             else
                                colors.transparent
                            )

                    -- LINE SEGMENTS IN THE CONE
                    , SceneObject 3 <|
                        object
                            (Mesh.lineSegment
                                (aPointOnTheEllipse rotatingPlane)
                                (Vec3.add (aPointOnTheEllipse rotatingPlane) <| Vec3.scale pf0len pf0unit)
                            )
                            colors.strokeBlue
                    , SceneObject 3 <|
                        object
                            (Mesh.lineSegment
                                (aPointOnTheEllipse rotatingPlane)
                                -- (.focus1 anEllipse)
                                (Vec3.add (aPointOnTheEllipse rotatingPlane) <| Vec3.scale pf1len pf1unit)
                            )
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
                    , SceneObject 3 <|
                        objectWith
                            [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha ]
                            (Mesh.point (aTangentPoint0 rotatingPlane))
                            (if pq0len == getTo model.pq0len then
                                colors.black
                             else if not (model.step == 3) then
                                colors.black
                             else
                                colors.transparent
                            )
                    , SceneObject 3 <|
                        objectWith
                            [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha ]
                            (Mesh.point (aTangentPoint1 rotatingPlane))
                            (if pq1len == getTo model.pq1len then
                                colors.black
                             else if not (model.step == 3) then
                                colors.black
                             else
                                colors.transparent
                            )

                    -- CIRCLES
                    , SceneObject 2 <|
                        objectWith
                            [ Blend.add Blend.srcAlpha Blend.one ]
                            (Mesh.circle (Cone.circleSection aCone (aTangentPoint0 rotatingPlane)) |> Mesh.fill)
                            (if h0 == getTo model.h0 then
                                colors.fillBlue
                             else
                                colors.transparent
                            )
                    , SceneObject 2 <|
                        objectWith
                            [ Blend.add Blend.srcAlpha Blend.one ]
                            (Mesh.circle (Cone.circleSection aCone (aTangentPoint0 rotatingPlane)) |> Mesh.stroke)
                            (if h0 == getTo model.h0 then
                                colors.strokeBlue
                             else
                                colors.transparent
                            )
                    , SceneObject 1 <|
                        objectWith
                            [ Blend.add Blend.srcAlpha Blend.one ]
                            (Mesh.circle (Cone.circleSection aCone (aTangentPoint1 rotatingPlane)) |> Mesh.fill)
                            (if h1 == getTo model.h1 then
                                colors.fillGreen
                             else
                                colors.transparent
                            )
                    , SceneObject 1 <|
                        objectWith
                            [ Blend.add Blend.srcAlpha Blend.one ]
                            (Mesh.circle (Cone.circleSection aCone (aTangentPoint1 rotatingPlane)) |> Mesh.stroke)
                            (if h1 == getTo model.h1 then
                                colors.strokeGreen
                             else
                                colors.transparent
                            )
                    , SceneObject 3 <|
                        object
                            (Mesh.point (aPointOnTheEllipse rotatingPlane))
                            colors.black

                    -- LINE SEGMENTS ON THE CONE
                    , SceneObject 3 <|
                        object
                            (Mesh.lineSegment
                                (aPointOnTheEllipse rotatingPlane)
                                (Vec3.add (aPointOnTheEllipse rotatingPlane) <| Vec3.scale pq0len pq0unit)
                            )
                            colors.strokeBlue
                    , SceneObject 3 <|
                        object
                            (Mesh.lineSegment
                                (aPointOnTheEllipse rotatingPlane)
                                -- (aTangentPoint1 rotatingPlane)
                                (Vec3.add (aPointOnTheEllipse rotatingPlane) <| Vec3.scale pq1len pq1unit)
                            )
                            colors.strokeGreen
                    ]
