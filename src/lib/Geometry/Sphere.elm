module Geometry.Sphere exposing (Sphere, toMesh, toMeshWith)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Geometry exposing (Config)


type alias Sphere =
    { radius : Float
    , center : Vec3
    }


parametrize sphere u v =
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


normalize sphere u v =
    parametrize sphere u v |> Vec3.normalize


toMeshWith config sphere =
    Geometry.trianglesWith config (parametrize sphere) (normalize sphere)


toMesh sphere =
    Geometry.triangles (parametrize sphere) (normalize sphere)
