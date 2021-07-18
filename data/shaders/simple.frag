#version 330 core

out vec4 fragColor;

in vec3 vertPosition;

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
#define BASE_COLOR_TEXTURE_COORD (vertTexCoord_ ## BASE_COLOR_TEXTURE_IDX)

uniform sampler2D uniBaseColorTexture;
#endif

void main()
{
#if COLOR_0_COMPONENTS == 3
    fragColor = vec4(vertColor, 1.0);
#elif COLOR_0_COMPONENTS == 4
    fragColor = vertColor;
#else
    fragColor = vec4(1.0, 1.0, 1.0, 1.0);
#endif

#ifdef BASE_COLOR_TEXTURE_IDX
    //fragColor = vec4(vertTexCoord_0.x, 0, 0, 1);
    fragColor = vec4(texture(uniBaseColorTexture, vertTexCoord_0).rgb, uniBaseColorFactor.a);
#endif
}
