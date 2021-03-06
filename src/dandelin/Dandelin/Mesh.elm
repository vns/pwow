module Dandelin.Mesh exposing (..)

import List exposing (..)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Geometry.Line as Line exposing (Line)
import Geometry.Plane as Plane exposing (Plane)
import Geometry.Cone as Cone exposing (Cone)
import Geometry.Ellipse as Ellipse exposing (Ellipse)
import Geometry.Circle as Circle exposing (Circle)
import Geometry.Sphere as Sphere exposing (Sphere)
import WebGL exposing (Mesh)
import Dandelin.Shader exposing (Vertex, Attributes)


{-| Create a mesh for a cone
-}
cone : Cone -> Mesh Attributes
cone aCone =
    let
        ( vertices, normals, indices ) =
            Cone.toMesh aCone
    in
        WebGL.indexedTriangles (map2 Attributes vertices normals) indices


{-| Create a mesh for a given line
-}
line : Line -> Mesh Attributes
line aLine =
    let
        offset =
            Vec3.scale 3.0 aLine.direction
    in
        WebGL.lines
            [ ( Attributes (Vec3.add aLine.origin offset) (vec3 0 0 0)
              , Attributes (Vec3.sub aLine.origin offset) (vec3 0 0 0)
              )
            ]


lineSegment : Vec3 -> Vec3 -> Mesh Attributes
lineSegment p1 p2 =
    let
        normal =
            vec3 0 0 0
    in
        WebGL.lines
            [ ( Attributes p1 normal
              , Attributes p2 normal
              )
            ]


{-| Create a mesh for a given plane
-}
plane : Plane -> Mesh Attributes
plane aPlane =
    let
        ( vertices, normals ) =
            Plane.toMesh aPlane

        indices =
            [ ( 1, 2, 3 )
            , ( 0, 1, 3 )
            ]
    in
        WebGL.indexedTriangles (map2 Attributes vertices normals) indices


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
coordinateAxes : Mesh Attributes
coordinateAxes =
    WebGL.lines
        [ ( Attributes (vec3 -10 0 0) (vec3 0 0 0), Attributes (vec3 10 0 0) (vec3 0 0 0) )
        , ( Attributes (vec3 0 -10 0) (vec3 0 0 0), Attributes (vec3 0 10 0) (vec3 0 0 0) )
        , ( Attributes (vec3 0 0 -10) (vec3 0 0 0), Attributes (vec3 0 0 10) (vec3 0 0 0) )
        ]


{-| Create a mesh of an ellipse
-}
ellipse : Ellipse -> List Attributes
ellipse anEllipse =
    let
        ( vertices, normals ) =
            Ellipse.toMesh anEllipse
    in
        map2 Attributes vertices normals


circle : Circle -> List Attributes
circle aCircle =
    let
        ( vertices, normals ) =
            Circle.toMesh aCircle
    in
        map2 Attributes vertices normals


stroke : List Attributes -> Mesh Attributes
stroke attributes =
    attributes
        |> WebGL.lineLoop


fill : List Attributes -> Mesh Attributes
fill attributes =
    attributes
        |> WebGL.triangleFan


{-| Create a mesh for a sphere
-}
sphere : Sphere -> Mesh Attributes
sphere aSphere =
    let
        ( vertices, normals, indices ) =
            Sphere.toMesh aSphere
    in
        WebGL.indexedTriangles (map2 Attributes vertices normals) indices


bigSphere : Sphere -> Mesh Attributes
bigSphere aSphere =
    let
        ( vertices, normals, indices ) =
            Sphere.toMeshWith { stackCount = 50, sliceCount = 50 } aSphere
    in
        WebGL.indexedTriangles (map2 Attributes vertices normals) indices


{-| Create a mesh for a point
-}
point : Vec3 -> Mesh Attributes
point p =
    let
        ( vertices, normals, indices ) =
            Sphere.toMeshWith { stackCount = 5, sliceCount = 5 } (Sphere 0.025 p)
    in
        WebGL.indexedTriangles (map2 Attributes vertices normals) indices
