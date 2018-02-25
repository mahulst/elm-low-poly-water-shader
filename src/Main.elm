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
        [ width 1000
        , height 1000
        , style [ ( "display", "block" ) ]
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh
            { perspective = perspective
            , camera = camera
            , t = t / 1000
            }
        ]


perspective : Mat4
perspective =
    Mat4.makePerspective 45 1 0.01 1000


camera : Mat4
camera =
    Mat4.makeLookAt (vec3 15 50 75) (vec3 15 0 0) (vec3 0 1 0)



-- Mesh


type alias Triangle =
    ( Vertex, Vertex, Vertex )


type alias Vertex =
    { position : Vec2
    , vert1 : Vec2
    , vert2 : Vec2
    , vert3 : Vec2
    , color : Vec3
    }


mesh : Mesh Vertex
mesh =
    let
        size =
            16

        list =
            List.range 0 size

        fn x =
            rowOfSquares size (toFloat x)

        rows =
            List.concatMap fn list
    in
        WebGL.triangles
            rows


rowOfSquares : Int -> Float -> List Triangle
rowOfSquares size z =
    let
        list =
            List.range 0 size

        fn x =
            square (toFloat x) z
    in
        List.concatMap fn list


square : Float -> Float -> List Triangle
square x z =
    let
        no =
            (vec2 (1 + x * 2) (-1 + z * 2))

        nw =
            (vec2 (-1 + x * 2) (-1 + z * 2))

        so =
            (vec2 (-1 + x * 2) (1 + z * 2))

        sw =
            (vec2 (1 + x * 2) (1 + z * 2))
    in
        [ ( Vertex nw nw so sw (vec3 1 0 0)
          , Vertex so nw so sw (vec3 0 1 0)
          , Vertex sw nw so sw (vec3 0 0 1)
          )
        , ( Vertex sw sw no nw (vec3 1 0 0)
          , Vertex no sw no nw (vec3 0 1 0)
          , Vertex nw sw no nw (vec3 0 0 1)
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
        attribute vec2 position;
        attribute vec2 vert1;
        attribute vec2 vert2;
        attribute vec2 vert3;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform float t;
        varying vec3 vcolor;

        float calculateSurface(float x, float z) {
            float scale = 20.0;
            float y = 0.0;
            y += (sin(x * 1.0 / scale + t * 1.0) + sin(x * 2.3 / scale + t * 1.5) + sin(x * 3.3 / scale + t * 0.4)) / 3.0;
            y += (sin(z * 0.2 / scale + t * 1.8) + sin(z * 1.8 / scale + t * 1.8) + sin(z * 2.8 / scale + t * 0.8)) / 3.0;
            y += cos(x * 1.25 - t) * sin(z * 0.75 + t) * 0.5;

            return y;
        }



        void main () {
            float y = calculateSurface(position.x, position.y);

            vec3 p1 = vec3(vert1.x, calculateSurface(vert1.x, vert1.y), vert1.y);
            vec3 p2 = vec3(vert2.x, calculateSurface(vert2.x, vert2.y), vert2.y);
            vec3 p3 = vec3(vert3.x, calculateSurface(vert3.x, vert3.y), vert3.y);
            vec3 normal = normalize(cross(p2 - p3, p1 - p2));

            gl_Position = perspective * camera * vec4(vec3(position.x, y, position.y), 1.0);
            vcolor = normal;
        }
    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;

        varying vec3 vcolor;

        void main() {

            vec3 terrainColor = vcolor;
            terrainColor.x = 0.0;
            terrainColor.y = 0.1;
            terrainColor.z += 0.6;

            gl_FragColor = vec4(terrainColor, 1.0);
        }
    |]
