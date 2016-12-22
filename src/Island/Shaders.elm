module Island.Shaders exposing (..)

import Math.Vector3 exposing (vec3, Vec3)
import Math.Vector3 as V3
import Math.Matrix4 as M4 exposing (Mat4)
import WebGL exposing (render, Renderable, Shader, Texture)
import Island.Types exposing (..)


type alias Edged a =
    { a | n1 : Vec3, n2 : Vec3, n3 : Vec3, n4 : Vec3, n5 : Vec3, n6 : Vec3 }


type alias EdgedVertex =
    Edged Vertex


type alias Varyings =
    { phongI : Vec3
    , phongL : Vec3
    , phongN : Vec3
    , phongV : Vec3
    , sources :
        Vec3
        -- , fDistance : Float
    }


type alias Uniforms u =
    { u | perspective : Mat4, light : Vec3, viewer : Vec3 }


type alias TextureUniforms u =
    { u | tex0 : Texture, tex1 : Texture }


colorVertexShader : Shader Vertex (Uniforms u) Varyings
colorVertexShader =
    [glsl|

precision mediump float;
attribute vec3 position;
attribute vec3 color;
attribute vec3 normal;

uniform mat4 perspective;
uniform vec3 light;
uniform vec3 viewer;

varying vec3 phongL;
varying vec3 phongN;
varying vec3 phongV;
varying vec3 phongI;
//varying float fDistance;
varying vec3 sources[2];

void main () {
  gl_Position = perspective * vec4(position, 1.0);
  phongL = normalize(light - position);
  phongN = normalize(normal);
  phongV = normalize(viewer - position);

  phongI = color;
  // fDistance = length(light - position);
  sources[0] = light;
  sources[1] = position;
}

|]


edgeVertexShader : Shader EdgedVertex (Uniforms u) Varyings
edgeVertexShader =
    [glsl|

precision mediump float;
attribute vec3 position;
attribute vec3 color;
attribute vec3 n1;
attribute vec3 n2;
attribute vec3 n3;
attribute vec3 n4;
attribute vec3 n5;
attribute vec3 n6;

uniform mat4 perspective;
uniform vec3 light;
uniform vec3 viewer;

varying vec3 phongL;
varying vec3 phongN;
varying vec3 phongV;
varying vec3 phongI;
//varying float fDistance;
varying vec3 sources[2];

// acceptable at faking diffuse lighting, quite poor for specular lighting
vec3 estimateNormal(vec3 p, vec3 n1, vec3 n2, vec3 n3, vec3 n4, vec3 n5, vec3 n6) {
  vec3 estimate = -0.3333 * (normalize(cross(p - n1, p - n2)) + normalize(cross(p - n3, p - n4)) + normalize(cross(p - n5, p - n6)));
  return normalize(estimate);
}


void main () {
  gl_Position = perspective * vec4(position, 1.0);
  phongL = normalize(light - position);
  phongN = estimateNormal(position,n1,n2,n3,n4,n5,n6);
  phongV = normalize(viewer - position);

  phongI = color;
  // fDistance = length(light - position);
  sources[0] = light;
  sources[1] = position;
}

|]


oceanVertexShader : Shader EdgedVertex (Uniforms { texture : Texture }) { vcoord : Vec3 }
oceanVertexShader =
    [glsl|

precision mediump float;
attribute vec3 position;
attribute vec3 color;
attribute vec3 n1;
attribute vec3 n2;
attribute vec3 n3;
attribute vec3 n4;
attribute vec3 n5;
attribute vec3 n6;

uniform mat4 perspective;
uniform vec3 light;
uniform vec3 viewer;
uniform sampler2D texture; // displacement map

varying vec3 vcoord;


// acceptable at faking diffuse lighting, quite poor for specular lighting
vec3 estimateNormal(vec3 p, vec3 n1, vec3 n2, vec3 n3, vec3 n4, vec3 n5, vec3 n6) {
  vec3 estimate = -0.3333 * (normalize(cross(p - n1, p - n2)) + normalize(cross(p - n3, p - n4)) + normalize(cross(p - n5, p - n6)));
  return normalize(estimate);
}


void main () {
  // vec3 position = a_position + texture2D(u_displacementMap, a_coordinates).rgb * (u_geometrySize / u_size);
  vec3 displacedPosition = position;
  displacedPosition.z = position.z + texture2D(texture, position.xy / 10.0).b * 2.0;
  displacedPosition.xy = displacedPosition.xy * 10.0;

  gl_Position = perspective * vec4(displacedPosition, 1.0);
  vcoord.yz = position.xy;

}

|]



-- FRAGMENT


colorFragmentShader : Shader {} (Uniforms u) Varyings
colorFragmentShader =
    [glsl|
precision mediump float;

uniform mat4 perspective;
uniform vec3 light;
uniform vec3 viewer;

varying vec3 phongL;
varying vec3 phongN;
varying vec3 phongV;
varying vec3 phongI;
//varying float fDistance;
varying vec3 sources[2];

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

  float distance = length(sources[1] - sources[0]);
  float dropoff = (1.0 / pow(distance, 2.0)) * 40.0;
  dropoff = clamp(dropoff, 0.5, 1.0);

  // these can be color-dependent
  float kd = 0.5;
  float ks = 0.5;

  vec3 L = normalize(phongL);
  vec3 N = normalize(phongN);
  vec3 V = normalize(phongV);
  vec3 R = normalize(2.0 * dot(L, N) * N - L);
  // can raise dot(R,V) to power alpha to emulate size of source
  float alpha = 40.0;
  float dotRV = clamp(dot(R, V), 0.0, 1.0);
  // normalizing specular but not diffuse makes the interpolation look right, for some reason
  float intensity = kd * clamp(dot(phongL, phongN), 0.0, 1.0) + ks * pow(dotRV, alpha);

  // can use different intensities for dot(L,N) and dot(R,V) terms
  vec3 phongColor = phongI * intensity * dropoff;

  vec3 hsvColor = rgb2hsv(phongColor);

  // hsvColor[2] = floor(hsvColor[2] * 12.0) / 12.0;
  gl_FragColor = vec4(hsv2rgb(hsvColor), 1.0);
}

|]



-- regular texture shaders


textureVertexShader : Shader Vertex (Uniforms { texture : Texture }) { vcoord : Vec3 }
textureVertexShader =
    [glsl|

attribute vec3 position;
attribute vec3 coords;
uniform mat4 perspective;
varying vec3 vcoord;

void main () {
  gl_Position = perspective * vec4(position, 1.0);
  vcoord = coords;
}

|]


textureFragmentShader : Shader {} (Uniforms { texture : Texture }) { vcoord : Vec3 }
textureFragmentShader =
    [glsl|

precision mediump float;
uniform sampler2D texture;
varying vec3 vcoord;

void main () {
  gl_FragColor = texture2D(texture, vcoord.yz);
}

|]



-- dummy shaders


dummyVertexShader : Shader {} (TextureUniforms u) {}
dummyVertexShader =
    [glsl|

precision mediump float;

// uniform sampler2D tex0;
// uniform sampler2D tex1;


void main () {
}

|]


dummyFragmentShader : Shader {} (TextureUniforms u) {}
dummyFragmentShader =
    [glsl|

precision mediump float;
void main () {
}

|]
