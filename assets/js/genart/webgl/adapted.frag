// https://www.shadertoy.com/view/MsByDw#

precision mediump float;

// the texCoords passed in from the vertex shader.
varying vec2 v_texCoord;

uniform float u_frame;

void main()
{
    float frame = 12.3 + u_frame / 60.0;
    vec2 uv = v_texCoord * 2.0 - 1.0;
    uv = uv.yx;
    uv.x /= 2.0;
    uv.y *= 2.0;
    float freqs[4];
    freqs[0] = 0.5;
    freqs[1] = 0.15;
    freqs[2] = 0.4;
    freqs[3] = 0.1;

    uv.x += sin(uv.y * uv.y + frame * 1.5) * freqs[3];

    float d = 0.0;

    for (float i = 0.0; i < 13.0; i++) {
        uv.x += sin(i * cos(frame * 0.1) * 20.0 + frame * 0.5 + uv.y * 4.0) * freqs[1] * 0.1;
        d += abs(1.0 / uv.x) * freqs[3] * 0.01;
    }

    vec3 colour = vec3(freqs[0], freqs[1], freqs[2] * 2.0) * d;

    gl_FragColor = vec4(colour, 1.0);
}
