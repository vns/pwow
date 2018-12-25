module Dandelin.Shader exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Math.Matrix4 as Mat4 exposing (Mat4)
import WebGL


type alias Vertex =
    { position : Vec3
    }


type alias Uniforms =
    { perspective : Mat4
    , color : Vec4
    }


vertex : WebGL.Shader Vertex Uniforms {}
vertex =
    [glsl|

        attribute vec3 position;
        uniform mat4 perspective;

        void main () {
            gl_Position = perspective * vec4(position, 1.0);
        }

    |]


fragment : WebGL.Shader {} Uniforms {}
fragment =
    [glsl|

        precision mediump float;
        uniform vec4 color;

        void main () {
            gl_FragColor = vec4(color);
        }

    |]
