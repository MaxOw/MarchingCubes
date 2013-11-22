layout(points) in;
layout(points, max_vertices = 1) out;
in int[] mc_case;
out vec4 norm;

void main() {
    if(mc_case[0] == 0) return;
    if(mc_case[0] == 255) return;
    gl_Position = gl_in[0].gl_Position;
    norm = vec4(0, 0, 0, 0);
    EmitVertex();
    EndPrimitive();
}
