module Cone exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4
import List exposing (..)
import List.Extra exposing (zip, last)
import Plane exposing (Plane)
import Ellipse exposing (Ellipse)


type alias Cone =
    { vertex : Vec3
    , axis : Vec3
    , angle : Float
    , height : Float
    }


makeTuples fn list =
    case list of
        a :: b :: rest ->
            fn a b :: (makeTuples fn <| b :: rest)

        _ ->
            []


toMesh : Cone -> List ( Vec3, Vec3 )
toMesh cone =
    let
        vertices =
            toVertices cone

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


toVertices : Cone -> List (List Vec3)
toVertices cone =
    let
        heightSegments =
            1

        radialSegments =
            25

        halfHeight =
            cone.height / 2

        radiusBottom =
            cone.height * (tan cone.angle)

        thetaLength =
            2 * pi

        angle =
            acos (Vec3.dot (Vec3.normalize cone.axis) (Vec3.normalize Vec3.j))

        axis =
            if abs (angle - pi) <= 0.00001 then
                Vec3.i
            else
                Vec3.normalize (Vec3.cross Vec3.j cone.axis)

        rotate =
            if abs angle <= 0.00001 then
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
                                            (u * thetaLength - 0.00001)

                                        sinTheta =
                                            sin theta

                                        cosTheta =
                                            cos theta

                                        p0 =
                                            vec3 (radius * sinTheta) (v * cone.height) (radius * cosTheta)
                                    in
                                        Vec3.add (rotate p0) cone.vertex
                                )
                )


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
    in
        { center = center
        , majorAxis = majorAxis
        , minorAxis = minorAxis
        , majorRadius = majorRadius
        , minorRadius = minorRadius
        }
