xof 0303txt 0032

template Frame
{
    < 3D82AB46-62DA-11CF-AB39-0020AF71E433 >
    [...]
}

template MeshFace
{
    < 3D82AB5F-62DA-11cf-AB39-0020AF71E433 >
    dword nFaceVertexIndices;
    array dword faceVertexIndices[nFaceVertexIndices];
}

template Vector
{
    < 3D82AB5E-62DA-11cf-AB39-0020AF71E433 >
    float x;
    float y;
    float z;
}

template Mesh
{
    <3D82AB44-62DA-11CF-AB39-0020AF71E433>
    dword nVertices;
    array Vector vertices[nVertices];
    dword nFaces;
    array MeshFace faces[nFaces];
    [...]
}

template Matrix4x4
{
    < F6F23F45-7686-11cf-8F52-0040333594A3 >
    array float matrix[16];
}

template FrameTransformMatrix
{
    < F6F23F41-7686-11cf-8F52-0040333594A3 >
    Matrix4x4 frameMatrix;
}

template MeshNormals
{
    < F6F23F43-7686-11cf-8F52-0040333594A3 >
    dword nNormals;
    array Vector normals[nNormals];
    dword nFaceNormals;
    array MeshFace faceNormals[nFaceNormals];
}

template ColorRGBA
{
    < 35FF44E0-6C7C-11cf-8F52-0040333594A3 >
    float red;
    float green;
    float blue;
    float alpha;
}

template ColorRGB
{
    < D3E16E81-7835-11cf-8F52-0040333594A3 >
    float red;
    float green;
    float blue;
}

template Material
{
    < 3D82AB4D-62DA-11CF-AB39-0020AF71E433 >
    ColorRGBA faceColor;
    float power;
    ColorRGB specularColor;
    ColorRGB emissiveColor;
    [...]
}

template MeshMaterialList
{
    < F6F23F42-7686-11CF-8F52-0040333594A3 >
    dword nMaterials;
    dword nFaceIndexes;
    array dword faceIndexes[nFaceIndexes];
    [Material <3D82AB4D-62DA-11CF-AB39-0020AF71E433>]
}

template TextureFilename 
{ 
    < A42790E1-7810-11cf-8F52-0040333594A3 > 
    string filename; 
}

template Coords2d
{
    < F6F23F44-7686-11cf-8F52-0040333594A3 >
    float u;
    float v;
}

template MeshTextureCoords
{
    < F6F23F40-7686-11cf-8F52-0040333594A3 >
    dword nTextureCoords;
    array Coords2d textureCoords[nTextureCoords];
}
