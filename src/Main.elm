module Main exposing (main)

{-
   Rotating triangle, that is a "hello world" of the WebGL
-}

import AnimationFrame
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Time exposing (Time)
import WebGL exposing (Mesh, Shader)


main : Program Never Time Time
main =
    Html.program
        { init = ( 0, Cmd.none )
        , view = view
        , subscriptions = (\model -> AnimationFrame.diffs Basics.identity)
        , update = (\elapsed currentTime -> ( elapsed + currentTime, Cmd.none ))
        }


view : Float -> Html msg
view t =
    WebGL.toHtml
        [ width 400
        , height 400
        , style [ ( "display", "block" ) ]
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh
            { perspective = perspective
            , camera = camera
            , t = t
            }
        ]


perspective : Mat4
perspective =
    Mat4.makePerspective 45 1 0.01 1000


camera : Mat4
camera =
    Mat4.makeLookAt (vec3 0 10 10) (vec3 0 0 0) (vec3 0 1 0)



-- Mesh


type alias Triangle =
    ( Vertex, Vertex, Vertex )


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


mesh : Mesh Vertex
mesh =
    let
        vs =
            square 0 0

        vs2 =
            square 0 1

        vs3 =
            square 1 0

        vs4 =
            square 1 1
    in
        WebGL.triangles
            (vs ++ vs2 ++ vs3 ++ vs4)


square : Float -> Float -> List Triangle
square x z =
    [ ( Vertex (vec3 (-1 + x * 2) 0 (-1 + z * 2)) (vec3 1 0 0)
      , Vertex (vec3 (-1 + x * 2) 0 (1 + z * 2)) (vec3 0 1 0)
      , Vertex (vec3 (1 + x * 2) 0 (1 + z * 2)) (vec3 0 0 1)
      )
    , ( Vertex (vec3 (1 + x * 2) 0 (1 + z * 2)) (vec3 1 0 0)
      , Vertex (vec3 (1 + x * 2) 0 (-1 + z * 2)) (vec3 0 1 0)
      , Vertex (vec3 (-1 + x * 2) 0 (-1 + z * 2)) (vec3 0 0 1)
      )
    ]



-- Shaders


type alias Uniforms =
    { perspective : Mat4
    , camera : Mat4
    , t : Float
    }





vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        varying vec3 vcolor;

        void main () {
            gl_Position = perspective * camera * vec4(position, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;

        varying vec3 vcolor;

        void main() {
          vec3 terrainColor = vcolor;
          gl_FragColor = vec4(terrainColor, 1.0);
        }

    |]
