module Geometry exposing (..)

import List exposing (..)


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



-- TODO: trianglesWithNormals


triangles fn =
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
                                        fn u v

                                    -- normal =
                                    --     normalize fn u v
                                in
                                    ( p0, indexes slices stacks i j )
                            )
                )
            |> concatMap identity
            |> foldl (\( v, i ) ( av, ai ) -> ( v :: av, i ++ ai )) ( [], [] )
