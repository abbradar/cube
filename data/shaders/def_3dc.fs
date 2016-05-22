#version 330 core

smooth in vec3 onorm;

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
  color = vec4(vec3(0.0, 1.0, 0.0)*(sunLight.ambient*sunLight.color + diffuseint), 1.0);
}
