#version 330

in vec2 fpos;
in vec2 fuv;

layout (location = 0) out vec4 colour;
layout (location = 1) out vec4 noise;

uniform sampler2D tex;
uniform sampler2D noise_tex;
uniform int output_noise;
uniform int scene_noise;
uniform float time;
uniform float speed;
uniform float obj_size;
uniform vec4 tint;

void main() {
  vec4 tex_col = texture(tex, fuv);
  if(tex_col.a == 0)
    discard;
  if(output_noise > 0 || scene_noise > 0) {
    noise = texture(noise_tex, (obj_size)*0.0005*fuv + (sin(time*speed)+pow(time*speed, 2))*vec2(1));
  } else {
    noise = vec4(0.5, 0.5, 0.5, 1);
  }

  vec4 tn = tint;
  if(scene_noise > 0) {
    vec4 e = vec4(0);
    e.r += sin(fpos.x*0.1);
    e.g += sin(fpos.y*0.1);
    e.b += tan(fpos.x*0.1 + fpos.y*0.1);
    tn += e*sin(time*0.2)*0.2;
    noise += e*sin(time)*0.02;
  }
  
  colour = tex_col * tn;  
}
