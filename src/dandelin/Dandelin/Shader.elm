module Dandelin.Shader exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Math.Matrix4 as Mat4 exposing (Mat4)
import WebGL


type alias Attributes =
    { position : Vec3
    , normal : Vec3
    }


type alias Vertex =
    { position : Vec3
    }


type alias Varying =
    { vlighting : Float
    }


type alias Uniforms =
    { perspective : Mat4
    , color : Vec4
    }


simpleVertex : WebGL.Shader Vertex Uniforms {}
simpleVertex =
    [glsl|

        attribute vec3 position;
        uniform mat4 perspective;
        void main () {
            gl_Position = perspective * vec4(position, 1.0);
        }
    |]


simpleFragment : WebGL.Shader {} Uniforms {}
simpleFragment =
    [glsl|

        precision mediump float;
        uniform vec4 color;
        void main () {
            gl_FragColor = vec4(color);
        }

    |]


vertex : WebGL.Shader Attributes Uniforms Varying
vertex =
    [glsl|

        attribute vec3 position;
        attribute vec3 normal;
        uniform mat4 perspective;
        varying highp float vlighting;
        highp float ambientLight = 0.4;
        highp float directionalLight = 0.5;
        highp vec3 directionalVector = vec3(0, 0, 3);
        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            highp vec4 transformedNormal = normalize(vec4(normal, 1.0));
            highp float directional = max(dot(transformedNormal.xyz, directionalVector), 0.0);
            vlighting = ambientLight + directional * directionalLight;
        }

    |]


fragment : WebGL.Shader {} Uniforms Varying
fragment =
    [glsl|

        precision mediump float;
        varying highp float vlighting;
        uniform vec4 color;

        void main () {
            gl_FragColor = vec4(color.rgb * vlighting, color.a);
        }

    |]
