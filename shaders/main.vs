#version 330

layout (location = 0) in vec2 pos;
layout (location = 1) in vec2 uv;

uniform mat4 model;
uniform mat4 viewproj;

out vec2 fpos;
out vec2 fuv;

void main() {
  vec4 world = model * vec4(pos, 0, 1);
  fpos = vec2(world);
  fuv = uv;
  gl_Position = viewproj * world;
}
