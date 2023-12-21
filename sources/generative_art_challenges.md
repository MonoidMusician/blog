It seems like we should have a primary data language and then primitives for drawing and audio.

- Configuration, parameters from user-space. Great if it could be a UI!
- Different unit systems. [E.g.]{t=} `px` and `%` in CSS.
- Unit truncation. [E.g.]{t=} pixel-perfect alignment.
- Different coordinate systems. [E.g.]{t=} rectangles in terms of center and size or top-left and size or opposite corners.
  A description in one coordinate system should be able to be converted into any other equally good coordinate system.
- Segmentation, [e.g.]{t=} dividing time up into measures, notes.
  Maybe it reduces to flexbox lol.

---

- Interior and exterior coordinate systems
- Key points of shapes, linear algebra or custom functions to solve
- Yeah, first-class linear algebra would be great
- Partial isomorphisms, maybe compile time verified for literals?
- Circle from three points -> circle with infinite radius = line
- I guess there's always an `mempty` lurking somewhere for soft failures.
  (Something goes wrong? Don't draw anything!)

Shaders??

Examples to build:

- Mandelbrot set
- Patterns, [e.g.]{t=} interlocking circles
- Perlin noise
- Animations
- Synths
- Coffee stain
