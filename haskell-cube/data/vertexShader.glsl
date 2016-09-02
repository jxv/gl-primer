#version 330

uniform mat4 pvmMatrix;
uniform mat4 viewModelMatrix;
uniform mat3 normalMatrix;

in vec3 position;
in vec3 colour;
in vec3 normal;
in vec2 uv;

out vec3 calculatedNormal;
out vec4 calculatedEye;
out vec4 rgba;
out vec2 fragmentUV;

void main ()
{
  vec4 position4 = vec4(position, 1.0);
  gl_Position = pvmMatrix * position4;

  calculatedNormal = normalize(normalMatrix * normal);
  calculatedEye = -(viewModelMatrix * position4);

  rgba = vec4(colour, 1.0);
  fragmentUV = uv;
}
