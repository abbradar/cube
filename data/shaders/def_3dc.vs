#version 330 core

uniform mat4 projectionMat;
uniform mat4 modelViewMat;

layout(location = 0) in vec3 vpos;
layout(location = 1) in vec3 inorm;

smooth out vec3 onorm;

void main()
{
  onorm = inorm;
  gl_Position = projectionMat * modelViewMat * vec4(vpos, 1.0);
}
