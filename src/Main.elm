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
            waterVertexShader
            waterFragmentShader
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
    Mat4.makeLookAt (vec3 60 150 250) (vec3 60 0 0) (vec3 0 1 0)



-- Mesh


type alias Triangle =
    ( Vertex, Vertex, Vertex )


type alias Vertex =
    { position : Vec2
    , vert1 : Vec2
    , vert2 : Vec2
    , vert3 : Vec2
    }


mesh : Mesh Vertex
mesh =
    let
        size =
            64

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
        [ ( Vertex nw nw so sw
          , Vertex so nw so sw
          , Vertex sw nw so sw
          )
        , ( Vertex sw sw no nw
          , Vertex no sw no nw
          , Vertex nw sw no nw
          )
        ]


type alias Uniforms =
    { perspective : Mat4
    , camera : Mat4
    , t : Float
    }


waterVertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
waterVertexShader =
    [glsl|
        attribute vec2 position;
        attribute vec2 vert1;
        attribute vec2 vert2;
        attribute vec2 vert3;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform float t;
        varying vec3 vcolor;

        vec3 permute(vec3 x) { return mod(((x*34.0)+1.0)*x, 289.0); }

        float snoise(vec2 v){
          const vec4 C = vec4(0.211324865405187, 0.366025403784439,
                   -0.577350269189626, 0.024390243902439);
          vec2 i  = floor(v + dot(v, C.yy) );
          vec2 x0 = v -   i + dot(i, C.xx);
          vec2 i1;
          i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
          vec4 x12 = x0.xyxy + C.xxzz;
          x12.xy -= i1;
          i = mod(i, 289.0);
          vec3 p = permute( permute( i.y + vec3(0.0, i1.y, 1.0 ))
          + i.x + vec3(0.0, i1.x, 1.0 ));
          vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy),
            dot(x12.zw,x12.zw)), 0.0);
          m = m*m ;
          m = m*m ;
          vec3 x = 2.0 * fract(p * C.www) - 1.0;
          vec3 h = abs(x) - 0.5;
          vec3 ox = floor(x + 0.5);
          vec3 a0 = x - ox;
          m *= 1.79284291400159 - 0.85373472095314 * ( a0*a0 + h*h );
          vec3 g;
          g.x  = a0.x  * x0.x  + h.x  * x0.y;
          g.yz = a0.yz * x12.xz + h.yz * x12.yw;
          return 130.0 * dot(m, g);
        }

        float calculateSurface(float x, float z) {
            float scale = 40.0;
            float y = snoise(vec2(x + t / 3.0,z + t / 3.0)) / 2.0;
            y += (sin(x * 1.0 / scale + t * 1.0) + sin(x * 2.3 / scale + t * 1.5) + sin(x * 3.3 / scale + t * 0.4)) / 3.0;
            y += (sin(z * 0.2 / scale + t * 1.8) + sin(z * 1.8 / scale + t * 1.8) + sin(z * 2.8 / scale + t * 0.8)) / 3.0;

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


waterFragmentShader : Shader {} Uniforms { vcolor : Vec3 }
waterFragmentShader =
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
