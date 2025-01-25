#version 330

in vec2 fpos;
in vec2 fuv;

out vec4 colour;

uniform sampler2D tex;
uniform vec4 tint;

void main() {
  vec4 tex_col = texture(tex, fuv);
  if(tex_col.a == 0)
    discard;
  colour = tex_col * tint;
}
