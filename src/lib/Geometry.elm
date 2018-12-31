module Geometry exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import List exposing (..)


type alias Config =
    { sliceCount : Int
    , stackCount : Int
    }


epsilon =
    0.00001


indexes slices stacks i j =
    let
        sliceCount =
            slices + 1

        a =
            i * sliceCount + j

        b =
            i * sliceCount + j + 1

        c =
            (i + 1) * sliceCount + j + 1

        d =
            (i + 1) * sliceCount + j
    in
        if (i >= stacks) || (j >= slices) then
            []
        else
            [ ( a, b, d )
            , ( b, c, d )
            ]


eachTuple fn list =
    case list of
        a :: b :: rest ->
            fn a b :: (eachTuple fn <| b :: rest)

        _ ->
            []


trianglesWith : Config -> (Float -> Float -> Vec3) -> (Float -> Float -> Vec3) -> ( List Vec3, List Vec3, List ( Int, Int, Int ) )
trianglesWith config parametricFn normalizeFn =
    range 0 config.stackCount
        |> map
            (\i ->
                range 0 config.sliceCount
                    |> map
                        (\j ->
                            let
                                u =
                                    toFloat j / toFloat config.sliceCount

                                v =
                                    toFloat i / toFloat config.stackCount

                                p0 =
                                    parametricFn u v

                                normal =
                                    normalizeFn u v
                            in
                                ( p0, normal, indexes config.sliceCount config.stackCount i j )
                        )
            )
        |> concatMap identity
        |> foldl (\( v, n, i ) ( av, an, ai ) -> ( v :: av, n :: an, i ++ ai )) ( [], [], [] )


triangles : (Float -> Float -> Vec3) -> (Float -> Float -> Vec3) -> ( List Vec3, List Vec3, List ( Int, Int, Int ) )
triangles parametricFn normalizeFn =
    trianglesWith { sliceCount = 25, stackCount = 25 } parametricFn normalizeFn


normalizeFDiffs fn u v =
    let
        p0 =
            fn u v

        eps =
            0.00001

        pu =
            if (u - eps >= 0) then
                Vec3.sub p0 (fn (u - eps) v)
            else
                Vec3.sub (fn (u + eps) v) p0

        pv =
            if (v - eps >= 0) then
                Vec3.sub p0 (fn u (v - eps))
            else
                Vec3.sub (fn u (v + eps)) p0
    in
        Vec3.cross pu pv |> Vec3.normalize
