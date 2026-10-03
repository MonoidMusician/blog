---
title: Where does dissonance come from?
subtitle: Physics and perception
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

<!-- ʼ -->

In musical practice, we are taught that small ratios are the most consonant: 2:3 for the perfect fifth, 3:4 for the perfect fourth, and of course the golden 1:2 for the octave, the purest non-unison interval.
Stepping outside the “perfect” intervals, there is 4:5 for the major third (arising from harmonics), which derives 5:6 for the minor third, but this is really getting questionable …

The theory of small ratios is not wrong!

It makes sense from two perspectives^[it makes for nice periodic curves, and it accords with the natural harmonics of string and wind instruments and human vocal chords], but it doesnʼt explain why one can be _close enough_ to being in tune, while things that are a little bit more out of tune are the most grating.

Human perception is of limited precision.
*Any* kind of measurement that is based on finite samples of finite precision is ultimately of limited precision.
So itʼs not surprising that consonances can be approximate, good enough.

Is there a *continuous* perception of dissonance that can explain what is happening?^[Continuous functions are a natural fit for dealing with approximate measurements, in a way that is kind of hard to explain but pretty important to some explanations of topology.]

Small integer ratios will appear as emergent properties of continuous dissonance for string timbres:
they are local extrema.

There’s a lot of ingredients we can integrate into this story.
Some of the ingredients come straight from physics and mathematics.
Others are straight up quirks of human perception that need to be calculated through studies.
And of course there are aspects of music theory that have pervaded culture to be inescapable.

- Physical phænomena: frequency spectrum, waveform, acoustic power, interference, …
- Perceptual phænomena: dissonance curve and beating, frequency loudness, pitch-vs-tempo, …
- Musical phænomena: semitones and intervals and scales, temperaments, chords and harmonies, …

<!--
dissonance curve for sine waves
wave equation, timbre, timpani, beam stiffness
integer overtones
beats, frequency, tempo
ringing, beating, vibrato, tremolo
tritones and emergent tones?

hertz, cents, logarithm
major/minor/perfect
-->

## Background

### Pitch

Pitch perception is logarithmic.
Although frequency is measured in Hertz^[famously 440Hz is the modern pitch reference for A<sup>4</sup> for modern music, though most musicians I know tune to 441Hz, and some professional orchestras tune to 442 or even 443Hz], wave cycles per second, an absolute/linear difference in Hz is not meaningful.
Instead, the interval of an octave is a *ratio* of frequencies 1:2, meaning that the octave of a note is the floored base-2 logarithm of its frequency (divided by some reference frequency).

The reason for this is quite simple: the shape of a waveform is determined by the ratio of frequencies that comprise it^[and their relative amplitudes and phases: see timbre], so that 220Hz and 440Hz, an octave apart, have an analogous shape to 440Hz with 880Hz, also an octave apart, not 440Hz with 660Hz (that would be described as a perfect fifth instead).
Making this discrete observation into a continuous phænomenon, we arrive at the fact that perceived pitch is the logarithm of frequency.

### Octave, semitones, intervals

An octave is the fundamental interval: it is the most similar that two notes can be, theoretically speaking.
Notes that are apart by an octave generally behave the same in music theory^[although the bass note of a chord/harmony is usually distinguished], and two instruments playing the same line separated by an octave is perceived as normal^[[e.g.]{t=} violoncello and double bass].

An octave is conventionally divided into 12 equal divisions: semitones.
A diatonic scale is made out of half steps (semitones) and whole steps (two semitones): seven notes, with the eighth finishing out the octave, hence the name.

Inversion around the octave is also a pervasive feature of music theory: the fourth and the fifth combine to an octave, a third and a sixth, a second and a seventh.^[Inclusive counting!]
The quality of interval is also preserved: fourths and fifths are the perfect intervals (a perfect fourth and a perfect fifth make a perfect octave), while the rest come in major and minor pairs, alternating in quality to make up the octave: a minor third with a major sixth, a major second with a minor seventh.
(That is: a minor sixth and a major sixth differ by a semitone, and minor always refers to the smaller interval of the pair, major to the greater.)
These intervals can be diminished or augmented, once again alternating in quality: an augmented fourth with a diminished fifth make an octave, an augmented second with a diminished seventh, and so on.

Since pitch perception is logarithmic, this means that in Equal Temperament (12 TET or 12 EDO), each semitone has a frequency ratio of \(2^{1/12}\): 12 steps, multiplied together, make up an octave.

The standardization of 12 TET at A440 is a modern development in music: before that, pitch references were highly regional (often tuned to the local church organ), and temperaments were used that placed some keys and intervals into more harmonious ratios, while other intervals and keys.
<!-- [_The Well-Tempered Clavier_](https://en.wikipedia.org/wiki/The_Well-Tempered_Clavier), a collection of preludes and fugues by Bach in all 24 major and minor keys, is a . -->

#### Cents

An (equal-temperament) semitone is further divided into 100 cents for the measurement of pitch: 1 cent represents a frequency ratio of \(2^{1/1200}\).

This is a scientific tool for analyzing pitch and frequencies: a cent is always defined as this ratio, even in contexts where a temperament other than 12 TET is used (or where the twelve-tone scale is not used!).

### Amplitude, loudness

Amplitude is the height of a sine wave, in linear units.
This is the actual width that a string is vibrating, or the pressure difference propagating through the room, and so on.

Interestingly, loudness perception is *also* logarithmic like pitch, measured in decibels (dB) with a funny formula (ten increments of dB correspond to a factor of ten change in amplitude, so one increment corresponds to \(10^{1/10}\)), but it is much more difficult to come up with an absolute reference for loudness, and different frequencies are perceived as different loudness (hence [dB(A)](https://en.wikipedia.org/wiki/A-weighting#Environmental_and_other_noise_measurements), a frequency weighting, but more on that later!).

(Presumably this is due to the same phænomenon of similarity? That 220Hz at amplitude 1 with 440Hz at amplitude 0.5 is similar to 220Hz at amplitude 2 with 440Hz at amplitude 1? I am not sure.)

### Timbre, Fourier transforms

[Timbre](https://en.wiktionary.org/wiki/timbre) (prounced like “tamber”) is the quality of a sound: the shape of its wave, or the relative frequencies it possesses with their relative amplitudes.
This is what makes instruments sound different even when playing the same sustained note.

The basic way to analyze timbre is to decompose into sine waves with a Fourier transform.
Fourier transform analyzes a periodic wave^[having the wave be periodic is not exactly essential to analysis, but is what Fourier transforms model] as am infinite sum of paired sine and cosine waves, with frequency increasing harmonically: for period \(p\), the basis waves are \(\sin(2\pi x/p)\), \(\sin(4\pi x/p)\), \(\sin(6\pi x/p)\), \(\sin(8\pi x/p)\), ..., \(\sin(2n\pi x/p)\), ..., plus cosines, to infinity ~~and beyond~~.

(Note that a sine and cosine wave of the same frequency combine to make a shifted sine wave, thus you can determine amplitude and phase from comparing the sine and cosine amplitudes.)

<details class="Details">
<summary id="digression-about-vector-spaces">Digression about vector spaces</summary>
Okay the cool thing about Fourier transform is thinking about it in terms of vector spaces.
(I learned this from my math advisor, thanks John!)

Choose a period \(p\).
The vector space we are considering is the space of continuous functions with period \(p\).
(Functions from reals to reals with the property that \(f(x + p) = f(x)\) for all \(x\).^[Topologically these are functions from the circle \(S^1\) to the real number line.])

You can take linear combinations of functions \(n\cdot f + m\cdot g\), and the result is still periodic, so this forms a vector space.
The multiplication and addition distribute over evaluation, so \((n\cdot f + m\cdot g)(x)\) means \(n\cdot f(x) + m\cdot g(x)\).

Now for a new trick: we can define an inner (dot) product on this vector space, notated \(\langle f, g\rangle\).
This measures the similarity of the two periodic functions by integrating them together over the period:

\[\langle f, g\rangle = \frac{2}{p} \int_0^{p} f(x)g(x)\, dx.\]

(This is a scaled \(L^2\) norm: [Wikipedia](https://en.wikipedia.org/wiki/Lp_space#Special_cases).)

Now hold your breath for a magic trick.

Our sines and cosines can form an *orthonormal basis* for continuous periodic functions.

\[\left\{ \frac{1}{\sqrt{2}},\; \sin\left(2\pi\, \frac{x}{p}\right)\!,\; \cos\left(2\pi\, \frac{x}{p}\right)\!,\; \sin\left(4\pi n\, \frac{x}{p}\right)\!,\; \cos\left(4\pi n\, \frac{x}{p}\right)\!,\; \dots,\; \sin\left(2\pi n\, \frac{x}{p}\right)\!,\; \cos\left(2\pi n\, \frac{x}{p}\right)\!,\; \dots \right\}\]

(\(\sin(0x)\) is not included, since it is the zero function and never part of any basis, but \(\frac{1}{\sqrt{2}}\cos(0x) = \frac{1}{\sqrt{2}}\) is included to model [DC offset](https://en.wikipedia.org/wiki/DC_bias).)

Naturally it is an infinite basis, but it is remarkable that it is countable and composed of familiar functions.
Like, why is the integral of sines and cosines of different periods zero??
For more details on orthonormality, here is a unit on exactly this: [Differential Equations, Section 8.3: Periodic Functions & Orthogonal Functions](https://tutorial.math.lamar.edu/classes/de/periodicorthogonal.aspx).

Being a basis means that we can model any continuous function with period \(p\) as a linear combination of these basis functions: one coefficient for each sine and each cosine.
But being an *orthonormal* basis means we can find these coefficients really easily: we use the inner product to measure how much it needs to contribute.

\[
  \begin{align*}
  &\text{Let\ } & s_n &= \left\langle f, \sin\left(2\pi n\, \frac{x}{p}\right) \right\rangle, \\
  &\text{and\ } & c_n &= \left\langle f, \cos\left(2\pi n\, \frac{x}{p}\right) \right\rangle, \\
  &\text{so\ }  & f(x) &= \sum_{n=0}^{\infty} s_n \sin\left(2\pi n\, \frac{x}{p}\right) + c_n \cos\left(2\pi n\, \frac{x}{p}\right).
  \end{align*}
\]

In general, for an orthonormal basis \(B\), a vector \(v\) can be reconstructed as the linear combination
\[v = \sum_{b \in B} \langle v, b \rangle \cdot b,\]
because for \(d \in B\), the inner product picks out the right coefficient
\[\left\langle d, \sum_{b \in B} c_b \cdot b \right\rangle = c_d,\]
by distributivity (linearity) and the fact that \(\langle d, b \rangle = \delta_{d=b}\): one if equal basis vectors, zero if different.

However, you need some evidence that this sum converges if it is infinite and that distributivity still applies, which I will not go over here.

:::Bonus
This is actually a story about the complex exponential function, which shows up literally everywhere in the solutions to partial differential equations.
:::

</details>

The discrete version, the *fast* Fourier transform (FFT), is an indispensable tool for digital signal processing (DSP), sound synthesis, statistics, and so much more.


## Sine waves

Sine waves are fundamental to the physics of sound: we can talk more about solutions to the wave equation later, but the fundamental modes of vibration always oscillate as sine waves with respect to time.

(Square, sawtooth, and triangular waves are also common building blocks for sound synthesis, but they do not have the same status as sine waves in the physics: they are particularly high energy modes of vibration, and square and sawtooth waves in particular are not physically realizable with their discontinuities.)

Furthermore, on a string, the standing modes of oscillation are sine waves across the length of the string (so the physical shape and their amplitude are both sine waves – again, of the fundamental modes of vibration).
This is important, but [for later](#integer-ratio-harmonics). Not right now.

Pure sine waves behave differently than more complex timbres in some ways, but the hope is that we can analyze what happens for pure sine waves, and then combine this analysis for realistic timbres.

I think it is accurate to say that humans perceive characteristics of the waveform and the frequency spectrum.
Essentially our perception is operating like a Fourier transform, picking out individual frequencies (and their phases?), but we also experience the pressure waves directly in an inescapable way.

### Loudness perception

For a given sound, humans perceive its loudness in terms of decibels (dB), not in terms of raw (linear) amplitude.
That is, volume sliders need to change amplitude according to a formula like \(10^s\).^[which has the problem of never reaching zero, so I like to adjust it to something like \(\min(s^4, 10^s)\).]

The relationship between perceived loudness and amplitude also varies by frequency.
Generally speaking, low frequencies are perceived as quieter for the same amplitude.

Noise.


### Dissonance curve (sine waves)

The fundamental research by Plomp and Levelt exposed (non musically trained) listeners to sine waves of different frequencies and asked them to rate how dissonant they are, essentially.

If you havenʼt seen it before, it will probably be surprising to you!

<details>
<summary>See the curve</summary>
</details>

Their conclusion was that dissonance starts at zero, for equal frequencies of course, and *rapidly* rises to a rounded peak, before falling off almost as sharply and gradually returning to zero for high differences.
Thatʼs it, thatʼs the whole phænomenon.
No integer ratios in sight.— but remember that is for pure sine waves.

Pure sine waves have no features to key into: beating is really noticeable when they have slightly different frequencies, but being near-but-not-exactly an octave is not noticeable.
Or is it?

More on that later.

<!-- https://sethares.engr.wisc.edu/consemi.html -->

## Overtones & timbres (the wave equation)



### Integer-ratio overtones

Back to standing waves on a string.

The fundamental modes of waves on a string are sine waves.

The overtones: .

This is true for a lot of situations, but is not universal!

One remarkable thing about having integer-ratio overtones is that playing the pitch versus selecting the harmonic produces the same frequencies, just at different amplitudes, subtly changing the sound.

That is, not only do A440 and A880 (an octave higher) have a high degree of similarity because A880 is in the overtone series for A440, but the other overtones of A880 are also present in A440: \(2*880 = 4*440\), \(3*880 = 4*440\), and so on.

Periodic waves.

### Near-integer overtones

A lot of things have overtones near integer ratios, but not exactly.




### Non-integer timbres

Timpani and gamelan.

## Dissonance curve (complex timbres)

