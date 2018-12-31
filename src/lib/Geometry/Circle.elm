module Geometry.Circle exposing (..)

import List exposing (..)
import Geometry exposing (epsilon)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type alias Circle =
    { center : Vec3
    , normal : Vec3
    , radius : Float
    }


toMesh : Circle -> ( List Vec3, List Vec3 )
toMesh circle =
    let
        segmentCount =
            30

        thetaLength =
            2 * pi
    in
        range 0 segmentCount
            |> map
                (\i ->
                    let
                        x =
                            toFloat i / segmentCount

                        theta =
                            x * thetaLength - epsilon

                        p0 =
                            Vec3.add circle.center <|
                                Vec3.add
                                    (Vec3.scale (cos theta * circle.radius) Vec3.i)
                                    (Vec3.scale (sin theta * circle.radius) Vec3.k)
                    in
                        ( p0, circle.normal )
                )
            |> foldl (\( v, n ) ( av, an ) -> ( v :: av, n :: an )) ( [], [] )
