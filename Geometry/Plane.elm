module Geometry.Plane exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4
import Geometry exposing (epsilon)
import Geometry.Line exposing (Line)
import List exposing (..)


type alias Plane =
    { normal : Vec3
    , point : Vec3
    }


withPoints : Vec3 -> Vec3 -> Vec3 -> Plane
withPoints p q r =
    let
        normal =
            Vec3.cross (Vec3.sub q p) (Vec3.sub r p) |> Vec3.normalize

        d =
            Vec3.dot normal p
    in
        Plane normal p


intersectLine : Plane -> Line -> Maybe Vec3
intersectLine plane line =
    let
        dotOrigin =
            Vec3.dot plane.normal line.origin

        dotDir =
            Vec3.dot plane.normal line.direction

        dist =
            (Vec3.dot (Vec3.normalize plane.normal) plane.point)
                / Vec3.length plane.normal
    in
        if (abs dotDir < epsilon) then
            Nothing
        else
            Just
                (Vec3.scale ((dist - dotOrigin) / dotDir) line.direction
                    |> Vec3.add line.origin
                )


makeTransform : Plane -> Vec3 -> Vec3
makeTransform plane =
    let
        dist =
            (Vec3.dot (Vec3.normalize plane.normal) plane.point)
                / Vec3.length plane.normal

        angle =
            acos (Vec3.dot (Vec3.normalize plane.normal) Vec3.j)

        axis =
            if abs (angle - pi) <= epsilon then
                Vec3.i
            else
                Vec3.normalize (Vec3.cross Vec3.j plane.normal)

        rot =
            if abs angle <= epsilon then
                Mat4.identity
            else
                Mat4.makeRotate angle axis

        trans =
            Mat4.makeTranslate (Vec3.scale dist plane.normal)
    in
        Mat4.transform (Mat4.mul trans rot)


toMesh : Plane -> ( List Vec3, List Vec3 )
toMesh plane =
    let
        scaleFactor =
            3.5

        vertices =
            [ vec3 -1.0 0.0 -1.0
            , vec3 -1.0 0.0 1.0
            , vec3 1.0 0.0 1.0
            , vec3 1.0 0.0 -1.0
            ]
                |> map (Vec3.scale scaleFactor)
                |> map (makeTransform plane)

        normals =
            repeat 4 ((makeTransform plane) <| plane.normal)
    in
        ( vertices, normals )
