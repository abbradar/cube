#version 330 core

out vec3 vertPosition;

in vec3 attrPosition;

#ifdef HAS_NORMALS
in vec3 attrNormal;
#endif

#ifdef HAS_TANGENTS
in vec4 attrTangent;
#endif

#if TEX_COORDS_COUNT >= 1
in vec2 attrTexCoord_0;
out vec2 vertTexCoord_0;
#if TEX_COORDS_COUNT >= 2
in vec2 attrTexCoord_1;
out vec2 vertTexCoord_1;
#endif
#endif

#ifdef HAS_NORMALS
#ifdef HAS_TANGENTS
out mat3 vertTBN;
#else
out vec3 vertNormal;
#endif
#endif

#if COLOR_0_COMPONENTS == 3
in vec3 attrColor;
out vec3 vertColor;
#elif COLOR_0_COMPONENTS == 4
in vec4 attrColor;
out vec4 vertColor;
#endif

uniform mat4 uniViewProjectionMatrix;
uniform mat4 uniModelMatrix;

#ifdef HAS_NORMALS
uniform mat4 uniNormalMatrix;
#endif


#if WEIGHTS_COUNT >= 1
#define MAX_BONES 4
#define MAX_BONES_PER_MESH 32
uniform mat4 uniBoneMatrices[MAX_BONES_PER_MESH];
uniform mat4 uniOffsetMatrices[MAX_BONES_PER_MESH];
in ivec4 attrJoint;
in vec4 attrWeight;
mat4 blendMatrix = mat4(0.0, 0.0, 0.0, 0.0,
                        0.0, 0.0, 0.0, 0.0,
                        0.0, 0.0, 0.0, 0.0,
                        0.0, 0.0, 0.0, 0.0);
#ifdef HAS_NORMALS
#ifdef HAS_TANGENTS
mat4 blendTangent = vec4(0.0, 0.0, 0.0, 0.0);
#else
vec4 blendNormal = vec4(0.0, 0.0, 0.0, 0.0);
#endif
#endif
#endif //weights

void main()
{
    vec4 pos = vec4(attrPosition, 1.0);
    vertPosition = vec3(pos.xyz) / pos.w;


#if TEX_COORDS_COUNT >= 1
    vertTexCoord_0 = attrTexCoord_0;
#if TEX_COORDS_COUNT >= 2
    vertTexCoord_1 = vertTexCoord_1;
#endif
#endif

#if COLORS_COUNT >= 1
    vertColor = attrColor;
#endif
#if WEIGHTS_COUNT >=1
    for(int i = 0; i < MAX_BONES; i++) {
        int ind = attrJoint[i];
        blendMatrix += attrWeight[i] * uniBoneMatrices[ind] * uniOffsetMatrices[ind];
    }
#ifdef HAS_NORMALS
#ifdef HAS_TANGENTS
    vec3 normalW = normalize(vec3(blendMatrix * vec4(attrNormal, 0.0)));
    vec3 tangentW = normalize(vec3(blendMatrix * vec4(attrTangent.xyz, 0.0)));
    vec3 bitangentW = cross(normalW, tangentW) * a_Tangent.w
    vertTBN = mat3(tangentW, bitangentW, normalW;
#else // !HAS_TANGENTS
    vertNormal = normalize(vec3(blendNormal));
#endif
#endif //!HAS_NORMALS
    gl_Position = uniViewProjectionMatrix * blendMatrix * pos;
#else //!HAS_WEIGHTS
#ifdef HAS_NORMALS
#ifdef HAS_TANGENTS
        vec3 normalW = normalize(vec3(uniNormalMatrix * vec4(attrNormal, 0.0)));
        vec3 tangentW = normalize(vec3(uniModelMatrix * vec4(attrTangent.xyz, 0.0)));
        vec3 bitangentW = cross(normalW, tangentW) * a_Tangent.w;
        vertTBN = mat3(tangentW, bitangentW, normalW);
#else // !HAS_TANGENTS
        vertNormal = normalize(vec3(uniNormalMatrix * vec4(attrNormal, 0.0)));
#endif
#endif // !HAS_NORMALS
    gl_Position = uniViewProjectionMatrix * uniModelMatrix * pos;
#endif


}
