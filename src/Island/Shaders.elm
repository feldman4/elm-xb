module Island.Shaders exposing (..)

import WebGL exposing (render, Renderable, Shader, Texture)
import Island.Types exposing (..)


colorVertexShader : Shader Attribute (UniformColor (Uniforms u)) Varyings
colorVertexShader =
    [glsl|

precision mediump float;
attribute vec3 position;
attribute vec3 normal;


uniform mat4 perspective;
uniform mat4 transform;
uniform mat4 normalMatrix;
uniform vec3 light;
uniform vec3 viewer;
uniform vec4 color;

varying vec3 phongL;
varying vec3 phongN;
varying vec3 phongV;
varying vec3 phongI;
varying vec3 sources[2];

void main () {
  vec4 worldPosition = transform * vec4(position, 1.0);
  vec4 worldNormal = normalMatrix * vec4(normal, 1.0);

  gl_Position = perspective * worldPosition;
  phongL = normalize(light - worldPosition.xyz);
  phongN = normalize(worldNormal.xyz);
  phongV = normalize(viewer - worldPosition.xyz);
  phongI = color.rgb;
  sources[0] = light;
  sources[1] = worldPosition.xyz;
}

|]



-- oceanVertexShader : Shader EdgedVertex (Uniforms { texture : Texture }) { vcoord : Vec3 }


oceanVertexShader =
    [glsl|

precision mediump float;
attribute vec3 position;

uniform mat4 perspective;
uniform mat4 transform;
uniform vec3 light;
uniform vec3 viewer;
uniform vec4 color;

uniform sampler2D displacement; // displacement map
uniform sampler2D normals; // normal map

varying vec3 phongL;
varying vec3 phongN;
varying vec3 phongV;
varying vec3 phongI;
varying vec3 sources[2];

void main () {
  // vec3 position = a_position + texture2D(u_displacementMap, a_coordinates).rgb * (u_geometrySize / u_size);
  vec3 displacedPosition = position;
  // WHICH COMPONENTS OF DISPLACEMENT TO USE?
  // displacedPosition.z = position.z + length(texture2D(displacement, position.xy ));
  displacedPosition.z = position.z + texture2D(displacement, position.xy ).y;

  vec4 worldPosition = transform * vec4(displacedPosition, 1.0);
  vec4 worldNormal = transform * vec4(texture2D(normals, position.xy).xyz, 1.0);
  worldNormal = worldNormal.xzyw; // WRONG, NEED TO FIGURE OUT
  gl_Position = perspective * worldPosition;
  phongL = normalize(light - worldPosition.xyz);
  phongN = normalize(worldNormal.xyz);
  phongV = normalize(viewer - worldPosition.xyz);
  phongI = color.rgb;
  sources[0] = light;
  sources[1] = worldPosition.xyz;

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
  float dropoff = (1.0 / pow(distance, 2.0)) * 400.0;
  dropoff = clamp(dropoff, 0.4, 1.0);

  // these can be color-dependent
  float kd = 1.0;
  float ks = 0.0;

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
-- textureVertexShader : Shader Attribute (Uniforms { texture : Texture }) { vcoord : Vec3 }


textureVertexShader =
    [glsl|

attribute vec3 position;
attribute vec3 coords;
uniform mat4 perspective;
uniform mat4 transform;
varying vec3 vcoord;


void main () {
  gl_Position = perspective * transform * vec4(position, 1.0);
  vcoord = coords;
}

|]



-- textureFragmentShader : Shader {} (Uniforms { texture : Texture }) { vcoord : Vec3 }


textureFragmentShader =
    [glsl|

precision mediump float;
uniform sampler2D texture;
varying vec3 vcoord;

void main () {
  gl_FragColor = texture2D(texture, vcoord.xy );
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


edgeVertexShader : Shader EdgedVertex (Uniforms u) Varyings
edgeVertexShader =
    [glsl|

precision mediump float;
attribute vec3 position;
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
// NEEDS TO TRANSFORM NEIGHBORS TO GET THE RIGHT NORMAL
vec3 estimateNormal(vec3 p, vec3 n1, vec3 n2, vec3 n3, vec3 n4, vec3 n5, vec3 n6) {
  vec3 estimate = -0.3333 * (normalize(cross(p - n1, p - n2)) + normalize(cross(p - n3, p - n4)) + normalize(cross(p - n5, p - n6)));
  return normalize(estimate);
}


void main () {
  gl_Position = perspective * vec4(position, 1.0);
  phongL = normalize(light - position);
  phongN = estimateNormal(position,n1,n2,n3,n4,n5,n6);
  phongV = normalize(viewer - position);

  vec3 color = vec3(0.6, 0.7, 0.8);
  phongI = color;
  // fDistance = length(light - position);
  sources[0] = light;
  sources[1] = position;
}

|]
