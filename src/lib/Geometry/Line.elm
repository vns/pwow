module Geometry.Line exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type alias Line =
    { origin : Vec3
    , direction : Vec3
    }
