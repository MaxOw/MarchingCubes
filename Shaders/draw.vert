uniform float time;
uniform float invSize;

out vec3 Position;
out vec3 Vertex;
out vec3 Normal;
out vec3 Tangent;
out vec3 Bitangent;
out float AmbientOcc;
out vec3 LightPosition;

void main()
{
    gl_Position = gl_ModelViewProjectionMatrix * vec4(gl_Vertex.xyz, 1);
    Position = (gl_ModelViewMatrixInverse * gl_Position).xyz;
    Vertex = (gl_Vertex.xyz * invSize) * 2 - 1;
    Normal = gl_Normal;
    Tangent = normalize(cross(Normal, Normal.zxy));
    Bitangent = normalize(cross(Normal, Tangent));
    AmbientOcc = gl_Vertex.w;
    
    LightPosition.x += (sin(time)-0.5)*15;
    LightPosition.z += (cos(time)-0.5)*15;
    LightPosition = vec3(1, 25, 1);
    LightPosition = (gl_ModelViewMatrixInverse * vec4(LightPosition, 1)).xyz;
}

