#define PI 3.141592653589793
#define HPI 1.5707963267948966

#define sinn(t) sin((t) * HPI)
#define asinn(t) (asin(t) / HPI)

#define bias(t) (((t)+1.0)/2.0)
#define norm(t) (((t)*2.0)-1.0)

#define ease(t, s) (bias(sinn(norm((t) / (s)))) * (s))

#define bounce(t) ((t) - abs((t) - mod(frame, 2.0 * (t))))
#define sbounce(t) ease(bounce(t), t)


precision mediump float;

// the texCoords passed in from the vertex shader.
varying vec2 v_texCoord;

uniform float u_frame;

void main()
{
    float frame = u_frame / 2.0;
    vec2 uv = norm(v_texCoord);
    uv = uv.yx;
    uv.x *= 2.0;
    float freqs[4];
    freqs[0] = 0.5;
    freqs[1] = 0.15;
    freqs[2] = 0.4;
    freqs[3] = 0.1;

    float d = 0.0;

    float scale = 1.0 / 1500.0 / 2.0;
    float x = bias(uv.y);
    float k = 0.091 * PI;
    float width = 1.0;
    float a = 50.0;
    float x0 = 0.0;
    float pct = 0.8;
    vec3 colour = vec3(0.0, 0.0, 0.0);
    vec3 white = vec3(255.0, 255.0, 255.0);

    for (float j = 0.0; j < 5.0; j += 1.0) {
        float pct = 0.8;
        float phi = x;
        phi = norm(phi/width);
        phi = asinn(phi * sinn(pct))/pct;
        phi = bias(phi) * width;

        float expected = -25.0 * scale * sin((phi - x0)*(150.0/width) * k + 0.1*sbounce(52.0+j)) * (a * 0.75 + a * 1.75 * sin((phi + 3.0*sbounce(17.0)*scale) * PI / width));

        d = (5.0 - j*0.6) / pow(abs(uv.x - expected), 1.5);

        k *= 1.1236 - j/200.0;
        a -= 2.0 + j;
        x0 -= 10.0*scale;
        gl_FragColor += vec4(vec3(freqs[0], freqs[1], freqs[2] * 2.0) * d * freqs[3] * 0.01, (max(5.0 - j, 0.0)/5.0) * min(d / 255.0 / max(3.0 - j * 0.5, 1.0), 1.0));
    }
}
