uniform sampler2D triTexture;
uniform sampler2D triTextureNormal;
uniform sampler2D terTexture;
uniform sampler2D terTextureNormal;

in vec3 Position;
in vec3 Vertex;
in vec3 Normal;
in vec3 Tangent;
in vec3 Bitangent;
in float AmbientOcc;
in vec3 LightPosition;

vec3 LightColor = vec3(1.0, 1.0, 1.0);
vec3 GlobalAmbient = vec3(0.7, 0.7, 0.7);

vec3 Ke = vec3(0.0, 0.0, 0.0);
vec3 Ka = vec3(0.6, 0.6, 0.6);
vec3 Kd = vec3(0.4, 0.4, 0.4);
vec3 Ks = vec3(0.0, 0.0, 0.0);
float Shininess = 0.0;

void main() {
    vec3 FP = clamp((Vertex + 1)*0.5, 0, 1);
    vec3 Color = vec3(0);
    vec3 bw = abs(Normal);
    bw = (bw - 0.2) * 0.7;
    bw = max(bw, 0);
    bw /= (bw.x + bw.y + bw.z);
    
    vec2 UVX = -FP.zy;
    vec2 UVY = FP.xz;
    vec2 UVZ = vec2(FP.x, -FP.y);
    
    Color += texture2D(triTexture, UVX).rgb * bw.x;
    Color += texture2D(terTexture, UVY).rgb * bw.y;
    Color += texture2D(triTexture, UVZ).rgb * bw.z;
    
    vec3 NMapX = normalize(texture2D(triTextureNormal, UVX).rgb * 2 - 1);
    vec3 NMapY = normalize(texture2D(terTextureNormal, UVY).rgb * 2 - 1);
    vec3 NMapZ = normalize(texture2D(triTextureNormal, UVZ).rgb * 2 - 1);
    
    NMapX = Tangent*NMapX.x + Bitangent*NMapX.y + Normal*NMapX.z;
    NMapY = Tangent*NMapY.x + Bitangent*NMapY.y + Normal*NMapY.z;
    NMapZ = Tangent*NMapZ.x + Bitangent*NMapZ.y + Normal*NMapZ.z;
    
    vec3 tct = gl_FragCoord.xyz * 0.01;
    
    vec3 P = Position;
    vec3 N = normalize(NMapX*bw.x + NMapY*bw.y + NMapZ*bw.z);
    vec3 L = normalize(LightPosition - P);
    vec3 V = normalize(-P);
    vec3 H = normalize(L + V);
    
    vec3 Emissive = Ke;
    vec3 Ambient = Ka * Color * GlobalAmbient;
    
    float d = max(dot(Normal, L), 0);
    float diff = max(dot(N, L), 0);
    vec3 Diffuse = Kd * Color * LightColor * d;
    
    vec3 FragColor = Emissive + Ambient + Diffuse;
         FragColor *= 1-AmbientOcc*2;
    gl_FragColor.rgb = FragColor;
    gl_FragColor.a = 1;
}
