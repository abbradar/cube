#version 330 core

uniform mat4 projectionMat;
uniform mat4 mVMat;
#define MAX_BONES 4

uniform mat4 bonesMat[32];
uniform mat4 offsetMat[4];

layout(location = 0) in vec3 vpos;
layout(location = 1) in vec3 inorm;
layout(location = 2) in vec2 texcoord;
layout(location = 3) in ivec4 index;
layout(location = 4) in vec4 weight;

smooth out vec3 onorm;
smooth out vec2 ftexcoord;

vec4 pos;
vec4 vpos1;
vec4 norm;
int ind;

void main()
{
  ftexcoord = texcoord;
  pos = vec4(0.0f, 0.0f, 0.0f, 0.0f);
  norm = vec4(0.0f, 0.0f, 0.0f, 0.0f);
  vpos1 = vec4(vpos, 1.0f);


  for(int i = 0; i < MAX_BONES; i++) {
    ind = int(index[i]);
    pos += weight[ind] * bonesMat[ind] * offsetMat[ind] * vpos1;
    norm += weight[ind] * bonesMat[ind] * offsetMat[ind] * vec4(inorm, 0.0f);
  }

  gl_Position = projectionMat * mVMat * pos;
  onorm = (norm).xyz;
}
