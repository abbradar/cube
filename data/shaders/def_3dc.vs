#version 330 core

uniform mat4 projectionMat;
uniform mat4 modelViewMat;

layout(location = 0) in vec3 vpos;
layout(location = 1) in vec3 inorm;
layout(location = 2) in vec2 texcoord;

smooth out vec3 onorm;
smooth out vec2 ftexcoord;

void main()
{
  onorm = inorm;
  ftexcoord = texcoord;
  gl_Position = projectionMat * modelViewMat * vec4(vpos, 1.0);
}
