module Geometry.Ellipse exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4
import Geometry exposing (epsilon)
import List exposing (..)


type alias Ellipse =
    { center : Vec3
    , majorAxis : Vec3
    , minorAxis : Vec3
    , majorRadius : Float
    , minorRadius : Float
    }


toMesh : Ellipse -> List Vec3
toMesh ellipse =
    let
        a =
            ellipse.majorRadius

        b =
            ellipse.minorRadius

        segmentCount =
            50

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
                            Vec3.add ellipse.center <|
                                Vec3.add
                                    (Vec3.scale (cos theta * a) ellipse.majorAxis)
                                    (Vec3.scale (sin theta * b) ellipse.minorAxis)
                    in
                        p0
                )
