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
    freqs[1] = 0.07;
    freqs[2] = 0.4;
    freqs[3] = 0.1;

    freqs[0] = 0.004;
    freqs[1] = 0.25;
    freqs[2] = 0.02;
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

//    gl_FragColor = vec4(0.30196078431372547, 0.0196078431372549, 0.21176470588235294, 1.0);
    vec4 start = vec4(0.30196078431372547, 0.0196078431372549, 0.21176470588235294, 0.9411764705882353);
    vec4 end = vec4(0.24313725490196078, 0.01568627450980392, 0.17254901960784313, 0.8156862745098039);
    gl_FragColor = end + length(v_texCoord) * (start - end) / length(vec2(1, 1));

    float arcs[2];
    arcs[0] = 100.0;
    arcs[1] = -120.0;
    float ks[6];
    ks[0] = 173.0;
    ks[1] = -177.0;
    ks[2] = -135.0;
    ks[3] = 133.0;
    ks[4] = 151.0;
    ks[5] = -105.0;

    for (float j = 0.0; j < 12.0; j += 1.0) {
        float arc = arcs[int(mod(j, 2.0))];
        float k = ks[int(j / 2.0)];

        float height = 1.0;
        float expected = height/2.0;
        for (float i = 1.0; i < 10.0; i += 1.0) {
            float x = i/10.0;
            float pct = 5.0 * max(0.0, 1.0 - abs(v_texCoord.x - x)*10.0);
            // scale * (45 + 20 * Math.sin(x * Math.PI / width)) * Math.sin((arc*k > 0 ? width - x : x) / scale / k * Math.PI) + scale * arc * Math.sin((x + scale * Math.sign(arc*k) * bounce(3*(13+i))*3) * Math.PI / width)
            expected += pct * scale * (45.0 + 20.0 * sin(x * PI)) * sin(PI * (arc*k > 0.0 ? 1.0 - x : x) / scale / k);
            expected += pct * scale * arc * sin(PI * (x + sign(arc*k) * 2.0 * scale * bounce(3.0*(13.0+i))*3.0));
        }

        gl_FragColor += vec4(0.9490196078431372, 0.42745098039215684, 0.7176470588235294, 1.0) * 0.08 * max(0.0, 1.0 - abs(v_texCoord.y - expected)*500.0/15.0);
        gl_FragColor += vec4(1, 0.8784313725490196, 0.9490196078431372, 1.0) * 0.25 * max(0.0, 1.0 - abs(v_texCoord.y - expected)*500.0);
    }

    for (float j = 0.0; j < 5.0; j += 1.0) {
        //freqs[1] = 0.07 - j * 0.01;
        freqs[0] -= 0.000;
        float pct = 0.7 + j * 0.03;
        float phi = x;
        phi = norm(phi/width);
        phi = asinn(phi * sinn(pct))/pct;
        phi = bias(phi) * width;

        float expected = -24.0 * scale * sin((phi - x0)*(150.0/width) * k + 0.1*sbounce(52.0+j)) * (a * 0.75 + a * 1.75 * sin((phi + 3.0*sbounce(17.0)*scale) * PI / width));

        d = 5.0 * (5.0 - j*0.6) / pow(abs(uv.x - expected), 1.5);

        k *= 1.1236 - j/200.0;
        a -= 3.0 + j*1.2;
        x0 -= 10.0*scale;

        float alpha = (max(4.0 - j*1.0/4.0, 0.0)/5.0) * min(pow(d / 255.0 / 3.0, 3.0) / max(5.0 + j, 1.0), (5.0 - j*2.0/4.0)/5.0);
        gl_FragColor += vec4(min(vec3(1.0, 1.0, 1.0), vec3(freqs[0], freqs[1], freqs[2] * 2.0) * max(d * freqs[3] * 0.01, 1.0))*alpha, alpha);
    }
}
