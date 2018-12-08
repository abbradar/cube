#version 330 core

smooth in vec3 onorm;
smooth in vec2 ftexcoord;
uniform sampler2D tex;

out vec4 color;

struct DirectionalLight {
  vec3 color;
  vec3 direction;
  float ambient;
};

uniform DirectionalLight sunLight;

void main()
{
  float diffuseint = max(0.0, dot(normalize(onorm),  -sunLight.direction));
  color = vec4(vec3(0.0f, 1.0f, 0.0f)*((sunLight.ambient + diffuseint)*sunLight.color), 1.0);
}
