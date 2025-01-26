#version 330

in vec2 uv;
out vec4 colour;

uniform sampler2D screen;
uniform sampler2D noise;

void main() { 
  if(uv.x > 1 || uv.y > 1 || uv.x < 0 || uv.y < 0) discard;
  vec4 ns = texture(noise, uv);
  colour = texture(screen, uv + (((ns*2)-vec4(1))/16).xy);
}
