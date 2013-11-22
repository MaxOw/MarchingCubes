out vec3 texCoord;

void main() {
    texCoord = gl_MultiTexCoord0.xyz;
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}

