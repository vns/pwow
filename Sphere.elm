module Sphere exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Geometry


type alias Sphere =
    { radius : Float
    , center : Vec3
    }


sphereFn sphere u v =
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
        Vec3.add sphere.center <|
            vec3 (sphere.radius * sin theta * cos phi) (sphere.radius * sin theta * sin phi) (sphere.radius * cos theta)


toMesh sphere =
    Geometry.triangles (sphereFn sphere)
