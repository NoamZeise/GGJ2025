#version 330

in vec2 fpos;
in vec2 fuv;

out vec4 colour;

void main() {
  colour = vec4(fuv.x, fuv.y, 1, 1);
}
