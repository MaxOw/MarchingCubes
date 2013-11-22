in vec3 texCoord;
uniform sampler2D gradpermTexture;
uniform vec3 cubeLocation;
uniform float margin;

const float ONE = 1.0/256.0;
const float ONEHALF = ONE/2.0;
const vec3 st = vec3(ONE, 0, 1);

vec4 gradperm(vec2 x) {
    vec4 p = texture2D(gradpermTexture, x);
    return vec4(p.xyz*4 - 1, p.w);
}

vec3  fade(vec3  x) { return x*x*x*(x*(x*6 - 15) + 10); }

float noise(vec3 P) {
    vec3 P0 = floor(P)*ONE + ONEHALF;
    vec3 d = fract(P);
    float perm;
    vec4 gp;
    
    perm = gradperm(P0.xy        ).w;
    float N000 = dot(gradperm(vec2(perm, P0.z      )).xyz, d         );
    float N001 = dot(gradperm(vec2(perm, P0.z + ONE)).xyz, d - st.yyz);
    
    perm = gradperm(P0.xy + st.yx).w;
    float N010 = dot(gradperm(vec2(perm, P0.z      )).xyz, d - st.yzy);
    float N011 = dot(gradperm(vec2(perm, P0.z + ONE)).xyz, d - st.yzz);
    
    perm = gradperm(P0.xy + st.xy).w;
    float N100 = dot(gradperm(vec2(perm, P0.z      )).xyz, d - st.zyy);
    float N101 = dot(gradperm(vec2(perm, P0.z + ONE)).xyz, d - st.zyz);
    
    perm = gradperm(P0.xy + st.xx).w;
    float N110 = dot(gradperm(vec2(perm, P0.z      )).xyz, d - st.zzy);
    float N111 = dot(gradperm(vec2(perm, P0.z + ONE)).xyz, d - st.zzz);
    
    d = fade(d);
    vec4 m1 = mix(vec4(N000, N001, N010, N011), 
                  vec4(N100, N101, N110, N111), d.x);
    vec2 m2 = mix(m1.xy, m1.zw, d.y);
    return (mix(m2.x, m2.y, d.z) + 1)*0.5;
}

void main() {
    vec3 c = texCoord + cubeLocation*margin;
    c -= 0.5;
    float d; 
    d = c.y-0.8;
    
    d = c.y-1.4;
    d += noise(c*128.0923)*0.0078125;
    d += noise(c*64.32107)*0.015625;
    d += noise(c*32.24718)*0.03125;
    d += noise(c*15.89272)*0.0625;
    d += noise(c*8.132338)*0.125;
    d += noise(c*3.895309)*0.25;
    d += noise(c*2.012824)*0.5;
    d += noise(c*0.992315);
    d += clamp((c.y-1.0)*3, 0, 1)*40;
    gl_FragColor = vec4(d, 0, 0, 1);
}

