#version 330

in vec2 fpos;
in vec2 fuv;

layout (location = 0) out vec4 colour;
layout (location = 1) out vec4 noise;

uniform sampler2D tex;
uniform sampler2D noise_tex;
uniform int output_noise;
uniform float time;
uniform float speed;
uniform vec4 tint;

void main() {
  vec4 tex_col = texture(tex, fuv);
  if(tex_col.a == 0)
    discard;
  if(output_noise > 0) {
    noise = texture(noise_tex, 0.2*fuv + (sin(time*speed)+pow(time*speed, 2))*vec2(1));
  } else {
    noise = vec4(0.5, 0.5, 0.5, 1);
  }
  colour = tex_col * tint;  
}
