#version 330 core

#include "tonemapping.glsl"

out vec4 fragColor;

in vec3 vertPosition;

uniform vec3 uniCamera;

#if TEX_COORDS_COUNT >= 1
in vec2 vertTexCoord_0;
#if TEX_COORDS_COUNT >= 2
in vec2 vertTexCoord_1;
#endif
#endif

#ifdef HAS_NORMALS
#ifdef HAS_TANGENTS
in mat3 vertTBN;
#else
in vec3 vertNormal;
#endif
#endif

#if COLOR_0_COMPONENTS == 3
in vec3 vertColor;
#elif COLOR_0_COMPONENTS == 4
in vec4 vertColor;
#endif

uniform vec4 uniBaseColorFactor;

#ifdef BASE_COLOR_TEXTURE_IDX
#define MAKE_BASE_COLOR_TEXTURE_COORD(idx) vertTexCoord_##idx
#define BASE_COLOR_TEXTURE_COORD MAKE_BASE_COLOR_TEXTURE_COORD(BASE_COLOR_TEXTURE_IDX)

uniform sampler2D uniBaseColorTexture;
#endif

#ifdef HAS_ALPHA_CUTOFF
uniform float uniAlphaCutoff;
#endif

void main()
{
    vec4 color = uniBaseColorFactor;

#ifdef BASE_COLOR_TEXTURE_IDX
    color *= sRGBToLinear(texture(uniBaseColorTexture, BASE_COLOR_TEXTURE_COORD));
#endif

#ifdef HAS_ALPHA_CUTOFF
    // Late discard to avaoid samplig artifacts. See https://github.com/KhronosGroup/glTF-Sample-Viewer/issues/267
    if (color.a < uniAlphaCutoff)
        discard;
#endif

    fragColor = color;
}
