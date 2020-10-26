#version 330

in vec3 position;

uniform mat4 viewProjectionMatrix;
uniform mat4 modelMatrix;
uniform mat4 normalMatrix;

void main()
{
    vec4 pos = vec4(position, 1.0);
    gl_Position = viewProjectionMatrix * modelMatrix * pos;
}
