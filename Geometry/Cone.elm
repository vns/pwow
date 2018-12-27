module Geometry.Cone exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4
import List exposing (..)
import List.Extra exposing (zip, last)
import Tuple
import Geometry.Plane as Plane exposing (Plane)
import Geometry.Ellipse as Ellipse exposing (Ellipse)
import Geometry.Circle as Circle exposing (Circle)
import Geometry.Sphere as Sphere exposing (Sphere)
import Geometry exposing (epsilon)


type alias Cone =
    { vertex : Vec3
    , axis : Vec3
    , angle : Float
    , height : Float
    }


toMesh : Cone -> ( List Vec3, List Vec3, List ( Int, Int, Int ) )
toMesh cone =
    let
        heightSegments =
            1

        radialSegments =
            40

        halfHeight =
            cone.height / 2

        slope =
            tan cone.angle

        radiusBottom =
            cone.height * slope

        thetaLength =
            2 * pi

        angle =
            acos (Vec3.dot (Vec3.normalize cone.axis) (Vec3.normalize Vec3.j))

        axis =
            if abs (angle - pi) <= epsilon then
                Vec3.i
            else
                Vec3.normalize (Vec3.cross Vec3.j cone.axis)

        rotate =
            if abs angle <= epsilon then
                identity
            else
                Mat4.transform (Mat4.makeRotate angle axis)
    in
        range 0 heightSegments
            |> map
                (\y ->
                    let
                        v =
                            toFloat y / heightSegments

                        radius =
                            v * radiusBottom
                    in
                        range 0 radialSegments
                            |> map
                                (\x ->
                                    let
                                        u =
                                            (toFloat x / radialSegments)

                                        theta =
                                            (u * thetaLength - epsilon)

                                        sinTheta =
                                            sin theta

                                        cosTheta =
                                            cos theta

                                        p0 =
                                            vec3 (radius * sinTheta) (v * cone.height) (radius * cosTheta)

                                        normal =
                                            vec3 sinTheta slope cosTheta |> Vec3.normalize
                                    in
                                        ( Vec3.add (rotate p0) cone.vertex
                                        , Vec3.add (rotate normal) cone.vertex
                                        , Geometry.indexes radialSegments heightSegments y x
                                        )
                                )
                )
            |> concatMap identity
            |> foldl (\( v, n, i ) ( av, an, ai ) -> ( v :: av, n :: an, i ++ ai )) ( [], [], [] )


sphere0 : Cone -> Plane -> Sphere
sphere0 cone plane =
    let
        r0 =
            (Vec3.dot (Vec3.sub cone.vertex plane.point) (Vec3.scale (sin cone.angle) plane.normal))
                / ((sin cone.angle) + Vec3.dot cone.axis plane.normal)

        center0 =
            Vec3.sub cone.vertex <| Vec3.scale (r0 / sin cone.angle) cone.axis
    in
        Sphere r0 center0


sphere1 : Cone -> Plane -> Sphere
sphere1 cone plane =
    let
        r1 =
            (Vec3.dot (Vec3.sub cone.vertex plane.point) (Vec3.scale (sin cone.angle) plane.normal))
                / ((sin cone.angle) - Vec3.dot cone.axis plane.normal)

        center1 =
            Vec3.add cone.vertex <| Vec3.scale (r1 / sin cone.angle) cone.axis
    in
        Sphere r1 center1


focus0 : Cone -> Plane -> Vec3
focus0 cone plane =
    let
        sph =
            sphere0 cone plane
    in
        Vec3.sub cone.vertex <|
            Vec3.add
                (Vec3.scale (sph.radius / (sin cone.angle)) cone.axis)
                (Vec3.scale sph.radius plane.normal)


focus1 : Cone -> Plane -> Vec3
focus1 cone plane =
    let
        sph =
            sphere1 cone plane
    in
        Vec3.add cone.vertex <|
            Vec3.sub
                (Vec3.scale (sph.radius / (sin cone.angle)) cone.axis)
                (Vec3.scale sph.radius plane.normal)


intersectPlane : Cone -> Plane -> Ellipse
intersectPlane cone plane =
    let
        cosTheta =
            Vec3.dot cone.axis plane.normal

        sinAlpha =
            sin cone.angle

        cosAlpha =
            sqrt (1 - sinAlpha * sinAlpha)

        t =
            Vec3.dot (Vec3.sub plane.point cone.vertex) plane.normal

        b =
            (cosTheta * cosTheta - sinAlpha * sinAlpha)

        h =
            t / b

        center =
            Vec3.add cone.vertex
                (Vec3.sub
                    (Vec3.scale (h * cosTheta) cone.axis)
                    (Vec3.scale (h * sinAlpha * sinAlpha) plane.normal)
                )

        majorAxis =
            Vec3.normalize (Vec3.sub cone.axis (Vec3.scale cosTheta plane.normal))

        minorAxis =
            Vec3.cross plane.normal majorAxis

        majorRadius =
            abs h * sinAlpha * cosAlpha

        minorRadius =
            t * sinAlpha / (sqrt (abs b))

        r1 =
            (Vec3.dot (Vec3.sub cone.vertex plane.point) (Vec3.scale (sin cone.angle) plane.normal))
                / ((sin cone.angle) - Vec3.dot cone.axis plane.normal)
    in
        { center = center
        , majorAxis = majorAxis
        , minorAxis = minorAxis
        , majorRadius = majorRadius
        , minorRadius = minorRadius
        , focus0 = focus0 cone plane
        , focus1 = focus1 cone plane
        , normal = plane.normal
        }


{-| Create a circle as a section of a plane perpendicular to the axis
going through a point on the cone
-}
circleSection : Cone -> Vec3 -> Circle
circleSection cone point =
    { normal = Vec3.j
    , center = Vec3.add cone.vertex (Vec3.scale (Vec3.dot cone.axis (Vec3.sub point cone.vertex)) cone.axis)
    , radius = sin cone.angle * (Vec3.length <| Vec3.sub point cone.vertex)
    }


bottomSection : Cone -> Circle
bottomSection cone =
    { normal = Vec3.j
    , center = Vec3.add cone.vertex (Vec3.scale cone.height cone.axis)
    , radius = cone.height * tan cone.angle
    }
