out int mc_case;
uniform sampler3D densityTexture;
uniform float invSize;
uniform float margin;

float density(vec3 p) {
    return texture3D(densityTexture, p).x*2 - 1;
}

void main() {
    vec2 one = vec2(invSize, 0);
    vec3 vp = (gl_Vertex.xyz + 0.5 + margin)*invSize;
    vec4 d0123, d4567;
    d0123.x = density(vp);
    d0123.y = density(vp + one.xyy);
    d0123.z = density(vp + one.xyx);
    d0123.w = density(vp + one.yyx);

    d4567.x = density(vp + one.yxy);
    d4567.y = density(vp + one.xxy);
    d4567.z = density(vp + one.xxx);
    d4567.w = density(vp + one.yxx);

    ivec4 n0123 = ivec4(clamp(sign(d0123), 0, 1));
    ivec4 n4567 = ivec4(clamp(sign(d4567), 0, 1));
    mc_case = (n0123.x     ) | (n0123.y << 1) | (n0123.z << 2) | (n0123.w << 3) |
              (n4567.x << 4) | (n4567.y << 5) | (n4567.z << 6) | (n4567.w << 7);

    gl_Position = gl_Vertex;
}

