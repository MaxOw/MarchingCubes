layout(points) in;
layout(triangle_strip, max_vertices = 15) out;

in vec4[] d0123;
in vec4[] d4567;
in int[] mc_case;
in vec4[] vpos;
out vec4 norm;
uniform sampler1D caseTexture;
uniform sampler3D densityTexture;
uniform float invSize;
uniform float margin;

const float inv_caseSize = 1. / (256 * 5);

const vec4 cMask[4] = vec4[4](
    vec4(1, 0, 0, 0),
    vec4(0, 1, 0, 0),
    vec4(0, 0, 1, 0),
    vec4(0, 0, 0, 1)
);

const vec3 cVertSt[12] = vec3[12](
    vec3(0, 0, 0),
    vec3(1, 0, 0),
    vec3(1, 0, 1),
    vec3(0, 0, 1),

    vec3(0, 1, 0),
    vec3(1, 1, 0),
    vec3(1, 1, 1),
    vec3(0, 1, 1),

    vec3(0, 0, 0),
    vec3(1, 0, 0),
    vec3(1, 0, 1),
    vec3(0, 0, 1)
);

const vec3 cVertDir[12] = vec3[12](
    vec3( 1, 0, 0),
    vec3( 0, 0, 1),
    vec3(-1, 0, 0),
    vec3( 0, 0,-1),
    
    vec3( 1, 0, 0),
    vec3( 0, 0, 1),
    vec3(-1, 0, 0),
    vec3( 0, 0,-1),
    
    vec3(0, 1, 0),
    vec3(0, 1, 0),
    vec3(0, 1, 0),
    vec3(0, 1, 0)
);

const vec3 RaySpharePoisson[32] = vec3[32](
    vec3( 0.286582,  0.257763, -0.922729),
    vec3(-0.171812, -0.888079,  0.426375),
    vec3( 0.440764, -0.502089, -0.744066),
    vec3(-0.841007, -0.428818, -0.329882),
    vec3(-0.380213, -0.588038, -0.713898),
    vec3(-0.055393, -0.207160, -0.976738),
    vec3(-0.901510, -0.077811,  0.425706),
    vec3(-0.974593,  0.123830, -0.186643),
    vec3( 0.208042, -0.524280,  0.825741),
    vec3( 0.258429, -0.898570, -0.354663),
    vec3(-0.262118,  0.574475, -0.775418),
    vec3( 0.735212,  0.551820,  0.393646),
    vec3( 0.828700, -0.523923, -0.196877),
    vec3( 0.788742,  0.005727, -0.614698),
    vec3(-0.696885,  0.649338, -0.304486),
    vec3(-0.625313,  0.082413, -0.776010),
    vec3( 0.358696,  0.928723,  0.093864),
    vec3( 0.188264,  0.628978,  0.754283),
    vec3(-0.495193,  0.294596,  0.817311),
    vec3( 0.818889,  0.508670, -0.265851),
    vec3( 0.027189,  0.057757,  0.997960),
    vec3(-0.188421,  0.961802, -0.198582),
    vec3( 0.995439,  0.019982,  0.093282),
    vec3(-0.315254, -0.925345, -0.210596),
    vec3( 0.411992, -0.877706,  0.244733),
    vec3( 0.625857,  0.080059,  0.775818),
    vec3(-0.243839,  0.866185,  0.436194),
    vec3(-0.725464, -0.643645,  0.243768),
    vec3( 0.766785, -0.430702,  0.475959),
    vec3(-0.446376, -0.391664,  0.804580),
    vec3(-0.761557,  0.562508,  0.321895),
    vec3( 0.344460,  0.753223, -0.560359)
);

float density(vec3 p) {
    return texture3D(densityTexture, (p + 0.5 + margin)*invSize).x;
}

const vec2 celld = vec2(1.00, 0);

void emitVert(int edge, vec3 p) {
    int en = edge / 4;
    int eA = int(mod(edge, 4));
    int eB = int(mod(edge+1, 4));
    float s0, s1;
    if (en == 0) {
        s0 = dot(d0123[0], cMask[eA]); 
        s1 = dot(d0123[0], cMask[eB]); 
    } else if (en == 1) {
        s0 = dot(d4567[0], cMask[eA]); 
        s1 = dot(d4567[0], cMask[eB]); 
    } else if (en == 2) {
        s0 = dot(d0123[0], cMask[eA]); 
        s1 = dot(d4567[0], cMask[eA]); 
    }

    float t = clamp(s0/(s0-s1), 0, 1);
    vec3 pos_in_cell = cVertSt[edge] + t*cVertDir[edge];
    vec3 pos = p + pos_in_cell;

    vec3 n;
    n.x = density(pos + celld.xyy) - density(pos - celld.xyy);
    n.y = density(pos + celld.yxy) - density(pos - celld.yxy);
    n.z = density(pos + celld.yyx) - density(pos - celld.yyx);

    float visibility = 0;
    for(int i=0; i<32; i++) {
        vec3 ray = RaySpharePoisson[i];
        float crv = 1;
        for(int rs = 1; rs<2; rs++) {
            float d = 1;-density(pos + ray*rs);
            crv *= clamp(d*8, 0, 1);
        }
        visibility += crv;
    }

    gl_Position = vec4(pos, 1 - visibility/32.0);
    norm = vec4(normalize(n), 1);
    EmitVertex();
}

void main() {
    if(mc_case[0] == 0) return;
    if(mc_case[0] == 255) return;
    for(int i = 0; i < 5; ++i) {
        vec4 tri = texture1D(caseTexture, (mc_case[0]*5 + i)*inv_caseSize)*255;
        if (tri.w == 0) break;
        emitVert(int(tri.x), vpos[0].xyz);
        emitVert(int(tri.y), vpos[0].xyz);
        emitVert(int(tri.z), vpos[0].xyz);
        EndPrimitive();
    }
}

