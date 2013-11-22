uniform sampler2D gradpermTexture;
in vec3 texCoord;

const float ONE = 1.0/256.0;
const float ONEHALF = ONE/2.0;

vec4 gradperm(vec2 x) {
    vec4 p = texture2D(gradpermTexture, x);
    return vec4(p.xyz*4 - 1, p.w);
}

vec4 gradperm(float x) {
    return gradperm(vec2(x, ONEHALF));
}

float fade(float x) { return x*x*x*(x*(x*6 - 15) + 10); }
vec2  fade(vec2  x) { return x*x*x*(x*(x*6 - 15) + 10); }
vec3  fade(vec3  x) { return x*x*x*(x*(x*6 - 15) + 10); }

const vec3 st = vec3(ONE, 0, 1);

float noise(float P) {
    float P0 = floor(P)*ONE + ONEHALF;
    float d = fract(P);
    vec4 gp;
    
    gp = gradperm(P0);
    float N0 = dot(gp.x, d);
    
    gp = gradperm(P0 + st.x);
    float N1 = dot(gp.x, d - st.z);
    
    d = fade(d);
    return mix(N0, N1, d);
}

float noise(vec2 P) {
    vec2 P0 = floor(P)*ONE + ONEHALF;
    vec2 d = fract(P);
    vec4 gp;
    
    gp = gradperm(P0);
    float N00 = dot(gp.xy, d);
    
    gp = gradperm(P0 + st.xy);
    float N10 = dot(gp.xy, d - st.zy);
    
    gp = gradperm(P0 + st.yx);
    float N01 = dot(gp.xy, d - st.yz);
    
    gp = gradperm(P0 + st.xx);
    float N11 = dot(gp.xy, d - st.zz);
    
    d = fade(d);
    vec2 m1 = mix(vec2(N00, N01), vec2(N10, N11), d.x);
    return mix(m1.x, m1.y, d.y);
}

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
    return mix(m2.x, m2.y, d.z);
}

float loopNoise(vec3 tc, float v) {
    float n = noise(vec3(    tc.x,     tc.y,     tc.z))*(tc.x - v)*(tc.y - v)*(tc.z - v) + 
              noise(vec3(v - tc.x,     tc.y,     tc.z))*(tc.x    )*(tc.y - v)*(tc.z - v) + 
              noise(vec3(    tc.x, v - tc.y,     tc.z))*(tc.x - v)*(tc.y    )*(tc.z - v) + 
              noise(vec3(v - tc.x, v - tc.y,     tc.z))*(tc.x    )*(tc.y    )*(tc.z - v) + 
              noise(vec3(    tc.x,     tc.y, v - tc.z))*(tc.x - v)*(tc.y - v)*(tc.z    ) + 
              noise(vec3(v - tc.x,     tc.y, v - tc.z))*(tc.x    )*(tc.y - v)*(tc.z    ) + 
              noise(vec3(    tc.x, v - tc.y, v - tc.z))*(tc.x - v)*(tc.y    )*(tc.z    ) + 
              noise(vec3(v - tc.x, v - tc.y, v - tc.z))*(tc.x    )*(tc.y    )*(tc.z    );
    return (n + 1) * 0.5;
}

void main() {
    vec3 tc = texCoord;
    
    vec3 n = vec3(loopNoise(tc, 1), loopNoise(tc, -1), loopNoise(tc, -4));
    vec3 nn = vec3(noise(tc+1), noise(-tc+0.1), noise(tc+0.2));
    gl_FragColor = vec4(nn, 1);
}
