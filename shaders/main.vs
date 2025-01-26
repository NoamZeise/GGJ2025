#version 330

layout (location = 0) in vec2 pos;
layout (location = 1) in vec2 uv;

uniform mat4 model;
uniform mat4 viewproj;
uniform mat4 uv_mat;
uniform mat4 uv_model;
uniform int correct_uv;
uniform float uv_speed;

out vec2 fpos;
out vec2 fuv;

void main() {
  vec4 world = model * vec4(pos, 0, 1);
  fpos = vec2(world);
  if(correct_uv > 0) {
    vec4 tuv = //vec4(uv.x, uv.y, 0, 0) +
      uv_model * uv_mat * model * vec4(uv.x, uv.y, 0, 1); 
    fuv = tuv.xy;
  } else {
    fuv = uv;
  }
  gl_Position = viewproj * world;
}
