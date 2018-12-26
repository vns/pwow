module Geometry exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import List exposing (..)


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


triangles parametricFn normalizeFn =
    let
        slices =
            50

        stacks =
            50
    in
        range 0 stacks
            |> map
                (\i ->
                    range 0 slices
                        |> map
                            (\j ->
                                let
                                    u =
                                        toFloat j / slices

                                    v =
                                        toFloat i / stacks

                                    p0 =
                                        parametricFn u v

                                    normal =
                                        normalizeFn u v
                                in
                                    ( p0, normal, indexes slices stacks i j )
                            )
                )
            |> concatMap identity
            |> foldl (\( v, n, i ) ( av, an, ai ) -> ( v :: av, n :: an, i ++ ai )) ( [], [], [] )


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
