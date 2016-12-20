module Island.Shaders exposing (..)

import Math.Vector3 exposing (vec3, Vec3)
import Math.Vector3 as V3
import Math.Matrix4 as M4 exposing (Mat4)
import WebGL exposing (render, Renderable, Shader)
import Island.Types exposing (..)


type alias Varyings =
    { fColor : Vec3, intensity : Float, fNormal : Vec3 }


type alias Uniforms u =
    { u | perspective : Mat4, light : Vec3 }


colorVertexShader : Shader Vertex (Uniforms u) Varyings
colorVertexShader =
    [glsl|

precision mediump float;
attribute vec3 position;
attribute vec3 color;
attribute vec3 normal;

uniform mat4 perspective;
uniform vec3 light;

varying vec3 fColor;
varying float intensity;
varying vec3 fNormal;

void main () {
  gl_Position = perspective * vec4(position, 1.0);
  float lightcoeff = dot(normalize(light - position), normalize(normal));
  float falloff = 1.0 / pow(length(light - position) / 10.0, 2.0);
  intensity = lightcoeff * clamp(falloff, 0.0, 1.0);
  fColor = color;
  fNormal = normal;
}

|]


colorFragmentShader : Shader {} (Uniforms u) Varyings
colorFragmentShader =
    [glsl|

precision mediump float;
varying vec3 fColor;
varying float intensity;
varying vec3 fNormal;

vec3 rgb2hsv(vec3 c)
{
    vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
    vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
    vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));

    float d = q.x - min(q.w, q.y);
    float e = 1.0e-10;
    return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

vec3 hsv2rgb(vec3 c) {
  c = vec3(c.x, clamp(c.yz, 0.0, 1.0));
  vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
  vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
  return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

void main () {
  vec3 hsvColor = rgb2hsv(fColor);
  hsvColor[2] = floor(hsvColor[2] * intensity * 6.0) / 6.0;
  gl_FragColor = vec4(hsv2rgb(hsvColor), 1.0);
}

|]
