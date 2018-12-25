module Dandelin.Mesh exposing (..)

import List exposing (..)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Plane exposing (Plane, Line)
import Cone exposing (Cone)
import Ellipse exposing (Ellipse)
import Sphere exposing (Sphere)
import WebGL exposing (Mesh)
import Dandelin.Shader exposing (Vertex)


{-| Create a mesh for a cone
-}
cone : Cone -> Mesh Vertex
cone aCone =
    let
        ( vertices, indices ) =
            Cone.toMesh aCone
    in
        WebGL.indexedTriangles (vertices |> map Vertex) indices


{-| Create a mesh for a given line
-}
line : Line -> Mesh Vertex
line aLine =
    let
        offset =
            Vec3.scale 3.0 aLine.direction
    in
        WebGL.lines
            [ ( Vertex (Vec3.add aLine.origin offset)
              , Vertex (Vec3.sub aLine.origin offset)
              )
            ]


{-| Create a mesh for a given plane
-}
plane : Plane -> Mesh Vertex
plane aPlane =
    let
        vertices =
            Plane.toMesh aPlane
                |> map Vertex

        indices =
            [ ( 1, 2, 3 )
            , ( 0, 1, 3 )
            ]
    in
        WebGL.indexedTriangles vertices indices


{-| Create a line mesh for the normal of a given plane
-}
planeNormal : Plane -> Mesh Vertex
planeNormal aPlane =
    let
        transform =
            Plane.makeTransform aPlane
    in
        [ ( vec3 0.0 0.0 0.0 |> transform |> Vertex
          , vec3 0.0 1.0 0.0 |> transform |> Vertex
          )
        ]
            |> WebGL.lines


{-| Create a mesh for the coordinate axes
-}
coordinateAxes : Mesh Vertex
coordinateAxes =
    WebGL.lines
        [ ( Vertex (vec3 -10 0 0), Vertex (vec3 10 0 0) )
        , ( Vertex (vec3 0 -10 0), Vertex (vec3 0 10 0) )
        , ( Vertex (vec3 0 0 -10), Vertex (vec3 0 0 10) )
        ]


{-| Create a mesh of the conic section produced by a given plane
-}
ellipse : Cone -> Plane -> Mesh Vertex
ellipse aCone aPlane =
    let
        anEllipse =
            Cone.intersectPlane aCone aPlane
    in
        Ellipse.toMesh anEllipse
            |> map Vertex
            |> WebGL.lineLoop


sphere : Sphere -> Mesh Vertex
sphere aSphere =
    let
        ( vertices, indices ) =
            Sphere.toMesh aSphere
    in
        WebGL.indexedTriangles (vertices |> map Vertex) indices
