---
title: Impossible Bézier Calligraphy
subtitle: Approximating cubic nibs drawn along cubic strokes
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

The story of how I implemented calligraphy for [cubic Bézier curves](https://www.jasondavies.com/animated-bezier/) (the kind widely used in graphics programs, especially the common [Scalable Vector Graphics (SVG) format](https://en.wikipedia.org/wiki/Scalable_Vector_Graphics)).

The problem statement:

::: {.Key_Idea box-name="Challenge"}
Given a shape of a pen nib, what composite shape is produced when the pen is drawn along any particular path?

If the inputs are cubic Bézier curves, is the output as well?

:::: {.Bonus box-name="Jargonized"}
Is the [Minkowski sum](https://en.wikipedia.org/wiki/Minkowski_addition) of two piecewise cubic Bézier hulls a [piecewise cubic Bézier hull](https://en.wikipedia.org/wiki/Composite_B%C3%A9zier_curve)?
::::
:::

The catch?
Itʼs mathematically impossible to model the output using cubic Bézier curves, as I determined after a bit of math.
In fact, it fails already for _quadratic_ curves (the simpler companion to cubic curves, which would have simpler, more tractable solutions).

The cubic in “cubic Bézier” refers to the fact that they are parametric curves modeled by _polynomials_ (one polynomial for the \(x\) coordinate and one polynomial for the \(y\) coordinate, in terms of a shared time variable \(t\)).
Simply put, the solution for how the curves of the pen and the curves of the path interact means that the solution wonʼt be a polynomial anymore, it would at least be a [rational function](https://en.wikipedia.org/wiki/Rational_function), [i.e.]{.foreign lang=la} a polynomials divided by another polynomial.

However, that doesnʼt prevent us from getting pretty darn close.
Let me show you how it works out.

::: Note
Cubic polynomials have nothing to do with Cubism.
At least, not that I know of.
:::

## Backstory

You want to know how I got here?
The story begins with a car trip.
My customary activity while riding in the car is to invent new writing systems, drawing on my tablet to try different calligraphic curves to start establishing what shapes I want to represent various sounds.
(I tend to develop [featural writing systems](https://en.wikipedia.org/wiki/Featural_writing_system) based on sounds represented by the [International Phonetic Alphabet](https://en.wikipedia.org/wiki/International_Phonetic_Alphabet).)

Of course, doing this in the car is hard mode already!
The bumps of the car mean I have to use the tablets undo feature for more strokes than not.
Plus, not only are there the mistakes of my hand being jostled by the car going over bumps, thereʼs also the mistakes of me just being mediocre at calligraphy, _plus_ the fact that I have to teach myself the script as Iʼm inventing it!
(I do love the creative interaction of drawing random shapes, seeing what feels good, and refining it both intentionally and through the natural iterations.)

Iʼve done this for many years, since before I could drive.
As long as Iʼve done that, Iʼve also wanted to digitize the shapes, maybe to make them into a computer font so I donʼt have to manually write things out when I want to see how full sentences look.
(ʼTwould also be great to explore ligatures and open type features to really make a natural flowing calligraphic font …)

As I mentioned above, the precisely stated mathematical problem says the curves we are looking for arenʼt the type of curves supported by graphics programs today.
But why let the mathematical impossibilities get in the way of actually quite good enough?
It took me until now to have the skills/insight/motivation to finally realize my dream, and I want to share the result and the process with you.

## Demo

But first, always start with the demo!

<script src="https://cdn.jsdelivr.net/npm/path-data-polyfill@1.0.3/path-data-polyfill.min.js"></script>
<style>
  #superpath path:nth-child(2n) {
    stroke: blue;
  }
  .annot path:hover {
    stroke: blue;
    fill: #0059;
    pointer-events: stroke;
  }
</style>
<div id="main-calligraphy-demo"></div>
<script src="assets/js/quartic.js"></script>
<script src="assets/js/calligraphy.js"></script>
<script src="assets/js/minkowski.js"></script>

<br/>

::: {.Details box-name="Legend"}
- Black – covered by the basic algorithm.
- Red – the ideal output, approximated.
- Green – improved algorithm.
:::

## Big picture

We can take apart the pen nib and pen path into a bunch of segments and compute the composite of each segment with each segment (Cartesian product).
Each composite of individual segments produces a section of the result, resulting in a patchwork of sections that form the whole composite shape.
Having a lot of overlapping sections is OK, since [e.g.]{.foreign lang=la} Inkscapeʼs builtin Boolean operations will simplify the shapes for us.^[It should be possible to only compute the outer segments and skip that step, but for now itʼs better to overapproximate it and optimize later.]

In fact, we will end up subdividing the original segments a bunch to produce accurate results:

- We donʼt want any self-intersecting segments [not implemented yet].
- We also donʼt want any segments that stop (their derivative is zero).
- The pen nib needs to be split up at inflection points, so its slope is monotonic along the segment.
  - This is because the slope of the pen path needs to be mapped onto the pen nib, and we want a unique solution.
- The pen nib also needs to be split up so that it doesnʼt loop around [not implemented yet].
  - More specifically, each segment needs to be split at the tangent of an endpoint.
- The pen paths need to be split at the tangents of the endpoints of the pen nib segment it is being combined with.
  <!-- TODO: explain better -->
  - This ensures that each segment either traces out the obvious thing or has a composite.
  - Basically we want that either the tangents are disjoint or the pen nibʼs are contained in the pen pathʼs tangents.

Then the task is to come up with the sections that form the patchwork:

- The original segments form corners (especially if they are disjoint)
  - With only this, you essentially get stamps at the endpoints, connected by rakes from the points; see the [smooth sailing] section.
- Finally the special composite that we will spend a lot of time discussing (if the tangents are a subset)

But thatʼs the end result, letʼs see how I got to this algorithm.

### Smooth sailing
Rake

The simplest approach is to paste each path on each segment, something like this:
```javascript
(Ps,Qs) => Ps.flatMap(P => Qs.flatMap(Q => [shift(Q, P[0]), shift(P, Q[0])]))
```
Mathematically we would say weʼre taking the Cartesian product of the segments.

I was able to do this much in Inkscape directly: copy some segments, align them.
You can even make linked clones so it updates altogether.

But there were problems: when the paths crossed over it got way too thin.
Basically if the pen nib wasnʼt perfectly straight it would be missing stuff.

Essentially is simulating what you would get by stamping the pen nib in certain points, and then drawing a rake through the path to connect them with curves.
(The rake only touching at the segmentation points of the pen nib.)

It appeared that anything more complex would require algorithmic help, and oh was I right …
There were more issues lurking with curved segments.

### Point of no return
Where tangents go wrong.

I sat down and tried to analyze where this occurred.
My first thought was what I said above: itʼs where the crossovers happen, right?
Right??

However, I realized that canʼt possibly be right:
the curves going wrong is a _local_ property, and itʼs already happened by the time the curves cross over.

Looking at it more closely (literally) I realized it occurs when the pen path matches the tangent of one of the segments of the pen nib.
(The sharp angles.)

Adding this .

Little did I know this was only the start of unraveling a long thread … it was not enough!

### Dead reckoning
Find matching tangents, but these.

#### Death
The solution is a rational function (ratio of two polynomials).
Bézier curves are polynomials, not rational, so the result will not be a Bézier curve.

#### Reckoning
But since it is cubic, we have another parameter to control at each place, in addition to the tangent.

Youʼd think this parameter would be curvature.
Youʼd **think**!!

### Why curvature?

The obvious first parameter to control is the tangent angles.
If we were approximating with quadratic curves, this would be all there is to it: two points and two tangents.

Working with cubic curves, however, we expect an extra degree of freedom: more control over how it matches our curve.
The obvious place to look is curvature.

Curvature (often denoted \(\kappa\)) is a geometric quantity that captures second-order behavior of a curve, regardless of parametrization.
That is, regardless of how \(t\) maps to actual \(\langle x, y \rangle\) points on the curve, if the curve looks the same, it will have the same curvature.

However, it is not so simple to map curvature onto the Bézier curve parameters, as weʼll see next.

For one thing, the formula involves a complex mix of components:
\[\kappa = \frac{x'y'' - x''y'}{(x'^2 + y'^2)^{3/2}}.\]

And then add in the complexities of the Bézier parameterization and you have a fun problem that yields non-unique solutions.

::: {.Bonus box-name="Aside"}
Itʼs funny: in order to display curvature in a geometrically meaningful way, you want it to be in units of distance, which means youʼd take its inverse \(1/\kappa\).
This inverse is the radius of the _osculating circle_ that just barely touches the curve in the most graceful way.
(Perhaps you know that you can form a circle passing through any three points, possibly a circle with infinite radius if the points are [along the same line](https://en.wikipedia.org/wiki/Collinearity).
This is why it is a second-order property.)

However, despite being in the right units, the radius of the osculating circle is poorly behaved because it can blow up to infinity when the curvature is near zero!
([E.g.]{.foreign lang="la"} near inflection points.)

So people often resort to displaying curvature as a kind of vector field associated with the curve, with some implicit conversion of units from inverse distance to real distance.

There is a third-order analogue of curvature called [aberrancy](https://www.jstor.org/stable/2690245).
It is related to the _osculating parabola_, since parabolas rotated in space can be fit to four points.
:::

## Details. Oh so many details

### Background and notation

If you want to dig into the details youʼll want some familiarity with vectors, calculus, and parametric curves.

Bézier curves are parametric curves based on some control points.
Weʼll only be dealing with 2D cubic Bézier curves.
Weʼll put the control points in boldface like \(\mathbf{P}\), \(\mathbf{Q}\), and give the 2D vectors arrows over top like \(\vec{u}\), \(\vec{v}\).

\[\mathbf{P} = [\langle \mathbf{P}_{0x}, \mathbf{P}_{0y}\rangle, \langle \mathbf{P}_{1x}, \mathbf{P}_{1y}\rangle, \langle \mathbf{P}_{2x}, \mathbf{P}_{2y}\rangle, \langle \mathbf{P}_{3x}, \mathbf{P}_{3y}\rangle].\]
\[\mathbf{P} = [\mathbf{P}_{0}, \mathbf{P}_{1}, \mathbf{P}_{2}, \mathbf{P}_{3}].\]
\[\mathbf{P}_ {x} = [\mathbf{P}_{0x}, \mathbf{P}_{1x}, \mathbf{P}_{2x}, \mathbf{P}_{3x}].\]

We need the formula for the Bézier polynomial that results from the control points, and weʼll also need its first and second derivatives:

\[
\begin{aligned}
\displaystyle \mathbf{B}_\mathbf{P}(t)
  &= (1-t)^3 \mathbf{P}_0 + 3t(1-t)^2 \mathbf{P}_1 + 3t^2(1-t) \mathbf{P}_2 + t^3 \mathbf{P}_3\\
  &= \mathbf{P}_0 + 3(\mathbf{P}_1 - \mathbf{P}_0)t + 3(\mathbf{P}_2 - 2\mathbf{P}_1 + \mathbf{P}_0)t^2 + (\mathbf{P}_3 - 3\mathbf{P}_2 + 3\mathbf{P}_1 - \mathbf{P}_0)t^3.\\
\mathbf{B}'_\mathbf{P}(t)
  &= 3(1-t)^{2}(\mathbf{P} _{1}-\mathbf{P} _{0})+6(1-t)t(\mathbf{P} _{2}-\mathbf{P} _{1})+3t^{2}(\mathbf{P} _{3}-\mathbf{P} _{2})\\
  &= 3(\mathbf{P}_1 - \mathbf{P}_0) + 6(\mathbf{P}_2 - 2\mathbf{P}_1 + \mathbf{P}_0)t + 3(\mathbf{P}_3 - 3\mathbf{P}_2 + 3\mathbf{P}_1 - \mathbf{P}_0)t^2.\\
\mathbf{B}''_\mathbf{P}(t)
  &= 6(1-t)(\mathbf{P} _{2}-2\mathbf{P} _{1}+\mathbf{P} _{0})+6t(\mathbf{P} _{3}-2\mathbf{P} _{2}+\mathbf{P} _{1})\\
  &= 6(\mathbf{P}_2 - 2\mathbf{P}_1 + \mathbf{P}_0) + 6(\mathbf{P}_3 - 3\mathbf{P}_2 + 3\mathbf{P}_1 - \mathbf{P}_0)t.\\
\end{aligned}
\]

Naturally the first derivative of a cubic Bézier is quadratic, and the second derivative is linear.

#### [2D]{style="font-variant-numeric: lining-nums"} cross product
Normally we think of it in 3D space, because the cross product of two 3D vectors is another 3D vector.
But it also works in 2D space, it just produces a scalar (1D vector) instead!
And it turns out to be a useful abstraction for a lot of our calculations.

\[\vec{u} \times \vec{v} = u_x v_y - u_y v_x.\]

### Matching the curves up

The two curves are controlled by their own $t$ parameters that are independent of each otherʼs!
We need to match them up somehow, and as discussed above, thereʼs a particular way to do that for our application:
We need to find the times when they have _parallel tangent lines_, since that will tell us what is the furthest point of the pen nib (\(\mathbf{Q}\)) (locally) along any given point of the pen path (\(\mathbf{P}\)).

The slope of the tangent line at time \(t\) is given by the ratio of the \(y\) and \(x\) components of the first derivative of the curve.
\[\frac{\mathbf{B}'_\mathbf{P}(t)_y}{\mathbf{B}'_\mathbf{P}(t)_x}.\]

Weʼll start using \(p = t_\mathbf{P}\) to refer to the time along curve \(\mathbf{P}\) and \(q = t_\mathbf{Q}\) to refer to the time along curve \(\mathbf{Q}\).
Weʼll think of it as solving for \(q\) in terms of \(p\); \(p\) is the input and \(q\) the output.
Our goal is to match them up, so the curves have the same slope at corresponding times!

\[\frac{\mathbf{B}'_\mathbf{Q}(q)_y}{\mathbf{B}'_\mathbf{Q}(q)_x} = \frac{\mathbf{B}'_\mathbf{P}(p)_y}{\mathbf{B}'_\mathbf{P}(p)_x}.\]

**Cross** multiply
\[{\mathbf{B}'_\mathbf{Q}(q)_y} {\mathbf{B}'_\mathbf{P}(p)_x} = {\mathbf{B}'_\mathbf{Q}(q)_x} {\mathbf{B}'_\mathbf{P}(p)_y}.\]

Or use the **cross** product:
\[\mathbf{B}'_\mathbf{Q}(q) \times \mathbf{B}'_\mathbf{P}(p) = 0.\]

(Recall that the cross product is a measure of how _perpendicular_ two vectors are, so they are _parallel_ exactly when their cross product is zero.
This is true in 2D just like in 3D, itʼs just that the cross product is now a scalar quantity, not a vector quantity.)

What does this get us?
Well, we can think of it either way, but letʼs assume that weʼre given \(p\), so we plug it in and obtain an equation to solve for \(q\).
Since \(\mathbf{B}'_ \mathbf{Q}\) is quadratic, we get a quadratic equation to solve, with some nasty scalar coefficients \(a\), \(b\), and \(c\) coming from the control points of our curves \(\mathbf{P}\) and \(\mathbf{Q}\), evaluated at \(p\):^[Even though weʼre putting a lot of vector information in from the control points of the curves, the coefficients are just scalars, and in fact are even quadratic polynomials in terms of \(p\)! Knowing that they are quadratic polynomials does not really help things at all.]
\[a(p)q^2 + b(p)q + c(p) = 0.\]

Obviously it gets tedious to write all of that, so we omit the \(p\) parameter and simply write:
\[aq^2 + bq + c = 0.\]

#### Issues

Thereʼs a few issues we run into.

The first is that the solution doesnʼt necessarily lie on the actual Bézier segment drawn out by \(0 \le q \le 1\).

Second there might be two solutions, since weʼre solving a quadratic!

The solution to both is to split things up!
We need to split up the _pen path_ so it indexes the tangents at the end of the Bézier segments of the pen nib, after first splitting the _pen nib_ at its inflection points.

Splitting at inflection points ensures that the tangent slope is always increasing or decreasing along the segment, making there only be a single solution.
Actually this requires also knowing that the Bézier segment doesnʼt rotate 180&deg;, so we need to split it if it reaches its original tangents again.

Solving these issues means we can think of the equation above as giving us a function for \(q\) in terms of \(p\):
\[q(p) = \frac{-b(p) \pm \sqrt{b(p)^2 - 4a(p)c(p)}}{2a(p)}.\]

This puts the functions in lock-step in terms of their tangents, giving us what we need to calculate the outside of their sweep.

#### Derivative of this

Weʼll need the derive of this equation soon, so letʼs calculate it while weʼre here.

My first thought was great, we have a quadratic equation, so we know the formula and can just take the derivative of it!

This was … naïve, oh so naïve.
Letʼs see why.

We have our solution here:

\[
q = q(p) = \frac{-b\pm\sqrt{b^2 - 4ac}}{2a}.
\]

So we can take its derivative \(q' = q'(p)\), using the chain rule, quotient rule, product rule … oh Iʼll spare you the gory details.

\[
\begin{aligned}
q'
  &= \frac{(-b'\pm\frac{2bb' - 4a' c - 4ac'}{\sqrt{b^2 - 4ac}})2a + 2a'(-b\pm\sqrt{b^2 - 4ac})}{4a^2}\\
  &= \frac{-b'\pm\frac{2bb' - 4a' c - 4ac'}{\sqrt{b^2 - 4ac}}}{2a} + \frac{a'(-b\pm\sqrt{b^2 - 4ac})}{2a^2}
\end{aligned}
\]

(Recall that \(a\), \(b\), and \(c\) are functions of \(p\), so they have derivatives \(a'\), \(b'\), and \(c'\) in terms of that variable.)

Notice any problems?

Well, first off, itʼs an ugly, messy formula!
And thatʼs even with hiding the definitions of the coefficients \(a\), \(b\), and \(c\).

The biggest problem, though, is that everything is divided by \(2a\) or \(4a^2\), which means it doesnʼt work when \(a = 0\).
That shouldnʼt be too surprising, given that the quadratic formula also fails in that case.
(Itʼs the _quadratic_ formula after all, not the _quadratic-or-linear_ formula!)

I mean, we could solve the linear case separately:

\[
bq + c = 0
\]
\[
q = \frac{-c}{b}
\]
\[
q' = \frac{-c' b + cb'}{b^2} = \frac{-c'}{b} + \frac{cb'}{b^2}
\]

But that also doesnʼt work; it omits the contribution of \(a'\), which does in fact influence the result of the rate of change of the quadratic formula, even when \(a = 0\).^[I first verified this numerically, but it makes a lot of sense if you think about it.]

So I took a deep breath, started texting my math advisor, and I [rubber ducked](https://en.wikipedia.org/wiki/Rubber_duck_debugging) myself into a much _much_ better solution.

You see, the quadratic formula is a lie.
How did we define \(q\)?
Certainly not as a complicated quadratic formula solution.
It is **really** defined as the implicit solution to an equation (an equation which happens to be quadratic):
\[aq^2 + bq + c = 0.\]

Look, we can just take the derivative of that whole equation, even before we attempt to solve it (only takes the product rule this time!):

\[
a'q^2 + 2aqq' + b'q + bq' + c' = 0.
\]

And **this**, now _this_ has a nicer solution:
\[
q' = \frac{a' q^2 + b' q + c'}{2aq + b}.
\]

I think itʼs cute how the numerator is another quadratic polynomial with the derivatives of the coefficients of the original polynomial.
Itʼs also convenient how we have no square roots or plusminus signs anymore – instead we write the derivative in terms of the original solution \(q\).

We still have a denominator that can be zero, but this is for deeper reasons:
\[2aq + b = -b \pm \sqrt{b^2 - 4ac} + b = \pm \sqrt{b^2 - 4ac} = 0.\]

Obviously this is zero exactly when \(b^2 - 4ac = 0\).
This quantity is called the discriminant of the quadratic, and controls much of its behavior: the basic property of how many real-valued solutions it has, as well as deeper number-theoretic properties studied in [Galois theory]().

::: {.Bonus box-name="Aside"}
I was looking at this and seeing \(q^2\) made me think that it could be rewritten a bit, since we can solve for \(q^2\) in the defining equation:

\[
q^2 = \frac{-c-bq}{a}.
\]

With some work that gives us this formula:
\[
q' = \frac{a' q^2 + b' q + c'}{2aq + b} = \frac{(ab' - a' b)q + c' a - ca'}{a(2aq + b)},
\]
which is nice and symmetric (it is patterned a little like the cross product in the numerator) but not what I ended up going for, I think I was worried about floating-point precision but idk.
:::

::: Bonus
For fun we can see what the second derivative is, though we wonʼt end up using it!
(I was scared we would need it at one point but that was caused by me misreading my code.)

\[
(2aq+b)q' = a' q^2 + b' q + c'
\]
\[
(2at+b)q'' + (\cancel{2a'q} + 2aq' + \cancel{b'})q' = a'' q^2 + \cancel{2a'qq'} + b'' q + \cancel{b' q'} + c''
\]
\[
q'' = \frac{- 2aq'^2 + a'' q^2 + b'' q + c''}{2aq+b}
\]
:::

#### Mystery coefficients

In which I attempt to write out what \(a\), \(b\), and \(c\) actually are.
Wish me luck.

We take the standard quadratic form of the tangent of \(\mathbf{Q}\):

\[
\mathbf{B}'_\mathbf{Q}(q) =
3(\mathbf{Q}_1 - \mathbf{Q}_0) + 6(\mathbf{Q}_2 - 2\mathbf{Q}_1 + \mathbf{Q}_0)q + 3(\mathbf{Q}_3 - 3\mathbf{Q}_2 + 3\mathbf{Q}_1 - \mathbf{Q}_0)q^2
\]

(Notice how there is a constant term, a term multiplied by \(q\), and a term with \(q^2\).)

We want to wrangle this cross product of that with \(\mathbf{B}'_\mathbf{P}(p)\) into a quadratic equation of \(q\):
\[
\mathbf{B}'_\mathbf{Q}(q) \times \mathbf{B}'_\mathbf{P}(p)
  = a(p)q^2 + b(p)q + c(p)
\]

So by distributivity, each coefficient we saw above is cross-producted with \(\mathbf{B}'_\mathbf{P}(p)\) to obtain our mystery coefficients:
\[
\begin{aligned}
a(p) &= 3(\mathbf{Q}_3 - 3\mathbf{Q}_2 + 3\mathbf{Q}_1 - \mathbf{Q}_0) \times \mathbf{B}'_\mathbf{P}(p),\\
b(p) &= 6(\mathbf{Q}_2 - 2\mathbf{Q}_1 + \mathbf{Q}_0) \times \mathbf{B}'_\mathbf{P}(p),\\
c(p) &= 3(\mathbf{Q}_1 - \mathbf{Q}_0) \times \mathbf{B}'_\mathbf{P}(p).\\
\end{aligned}
\]

You _could_  expand it out more into the individual components, but it would be painful, not very insightful, and waste ink.

Note that this vector cross-product *cannot* be cancelled out as a common term, because the \(\mathbf{Q}\)-vector coefficients control how the separate components of \(\mathbf{B}'_\mathbf{P}(p)\) are mixed together to create the coefficients of \(a\), \(b\), and \(c\).
However, it could be divided by its norm without a problem (that is, only the direction of \(\mathbf{B}'_\mathbf{P}(p)\) matters, not is magnitude – and this is by design.)

### Dead reckoning revisited

Now that we have a formula for \(q\) in terms of \(p\), we can just plug it in and get our whole curve.

\[\mathbf{C}_{\mathbf{P},\mathbf{Q}}(p) = \mathbf{B}_\mathbf{P}(p) + \mathbf{B}_\mathbf{Q}(q(p)).\]

Now for the main question: is this a Bézier curve?
Nope!

Even if \(\mathbf{Q}\) was a quadratic Bézier curve, the solution \(q(p)\) would still be a [rational function](https://en.wikipedia.org/wiki/Rational_function), which is not compatible with the polynomial structure of Bézier curves.

That means we canʼt just stick the curve into an SVG file or similar graphics format, its true form is not natively supported by any graphics libraries.
(And for good reason, because itʼs kind of a beast!)

However, we know a lot of information about the curve, and we can use it to reconstruct a decent approximation of its behavior, meaning all is not lost.

#### Computing compound curvature

We now know an exact formula for the idealized \(\mathbf{C}_{\mathbf{P}, \mathbf{Q}}\).
We can use this to get some key bits of information that will allow us to construct a good approximation to its behavior.

In particular, we want to know the slope at the endpoints and also the curvature at the endpoints.
The curvature is the complicated part.

Itʼs going to get verbose very quickly, so letʼs trim down the notation a bit by leaving \(p\) implicit, focusing on \(t = q(p)\), and remove the extraneous parts of the Bézier notation:

\[\mathbf{C} = \mathbf{P} + \mathbf{Q}(q).\]

By construction, the slope at the endpoints is just the slope of \(\mathbf{P}\) at the endpoints:
\[\mathbf{C}' = \mathbf{P}' + \mathbf{Q}'(q)q' \parallel \mathbf{P}',\]
since those vectors are parallel by the construction of \(q(p)\):
\[\mathbf{P}' \parallel \mathbf{Q}'(q).\]

The curvature is a bit complicated, but we can work through it and just requires applying the formulas, starting with this formula for curvature of a parametric curve:

\[\mathbf{C}^\kappa = \frac{\mathbf{C}' \times \mathbf{C}''}{\|\mathbf{C}'\|^3}.\]

We already computed \(\mathbf{C}'\) above, so we just need to compute \(\mathbf{C}''\) and compute the cross product.
\[\mathbf{C}'' = \mathbf{P}'' + \mathbf{Q}''(q)q'^2 + \mathbf{Q}'(q)q''.\]

However, I promised that we wouldnʼt need the second derivative \(q''\), so letʼs see how it cancels out in the cross product.
With some distributivity we can expand it:

\[
\begin{aligned}
\mathbf{C}' \times \mathbf{C}''
  && = &\ (\mathbf{P}' + \mathbf{Q}'(q)q')\\
  && \times &\ (\mathbf{P}'' + \mathbf{Q}''(q)q'^2 + \mathbf{Q}'(q)q'')\\\\
  && = &\ (\mathbf{P}' + \mathbf{Q}'(q)q') \times \mathbf{P}''\\
  && + &\ (\mathbf{P}' + \mathbf{Q}'(q)q') \times \mathbf{Q}''(q)q'^2\\
  && + &\ \cancel{(\mathbf{P}' \times \mathbf{Q}'(q))}q''\\
  && + &\ \cancel{(\mathbf{Q}'(q) \times \mathbf{Q}'(q))}q'q''\\\\
  && = &\ \mathbf{C}' \times (\mathbf{P}'' + \mathbf{Q}''(q)q'^2).\\
\end{aligned}
\]

Obviously \(\mathbf{Q}'(q) \times \mathbf{Q}'(q) = 0\), since those are parallel, being the same vector.
But \(\mathbf{P}' \times \mathbf{Q}'(q) = 0\) as well, since those are parallel by construction of \(q = q(p)\).
That means we do not need to deal with the second derivative \(q''\).

#### Reconstructive surgery

Now we can get down to business.
How do we find the best Bézier curve to fill in for the much more complicated curve \(\mathbf{C}_{\mathbf{P},\mathbf{Q}}\) that combines the two curves \(\mathbf{P}\) and \(\mathbf{Q}\)?

Weʼll take six (6) pieces of data from \(\mathbf{C}_{\mathbf{P},\mathbf{Q}}\):

1. The endpoints: \(f_0\), \(f_1\) (x2),
2. The tangents at the endpoints: \(d_0\), \(d_1\) (x2), and
3. The curvature at the endpoints: \(\kappa_0\), \(\kappa_1\) (x2).

This should be enough to pin down a Bézier curve, and indeed there is a way to find cubic Bézier curves that match these parameters.

We will be following the paper [High accuracy geometric Hermite interpolation](https://minds.wisconsin.edu/bitstream/handle/1793/58822/TR692.pdf;jsessionid=E008B26966FD35F59178ECBD7500CB56?sequence=1) by Carl de Boor, Klaus Höllig, and Malcolm Sabin to answer this question.
The basic sketch of the math is pretty straightforward, but the authors have done the work to come up with the right parameterizations to make it easy to compute and reason about.

The bad news is we end up with a quartic (degree 4) equation, to solve the system of two quadratic equations.
So we see that there can be up to 4 solutions.
But we can narrow them down a bunch, like if there are solutions with loops (self-intersections) we can rule them out, or other outlandish solutions with far-flung control points.

For example, one can take these same datapoints from a real cubic Bézier curve and reconstruct its control points from those six pieces of information.
In our case, we are hoping that the curves we come across, although not technically being of that form, are very close and will still produce a similar curve to the perfect idealized solution.^[Note that we arenʼt doing other methods of error reduction, like [minimizing area](https://raphlinus.github.io/curves/2021/03/11/bezier-fitting.html#signed-area).]

::: Bonus
In fact, one cool thing about this implementation is that we can use it to find the closest Bézier curve _without a loop_ to one _with a loop_.
(And the reverse, though I have not implemented that.)
:::

##### Steps to a solution

Basically a lot of shuffling variables around.

We will be solving for \(\delta_0\) and \(\delta_1\), which scale the control handles along the predefined tangents, giving these Bézier control points:
\[
\begin{aligned}
b_0 = f_0,\ &\ b_1 = b_0 + \delta_0 d_0,\\
b_3 = f_1,\ &\ b_2 = b_3 - \delta_1 d_1.\\
\end{aligned}
\]

Now we compute the curvature at the endpoints for this Bézier curve:
\[
\begin{aligned}
\kappa_0 &= 2d_0 \times (b_2 - b_1)/(3\delta_0^2),\\
\kappa_1 &= 2d_1 \times (b_1 - b_2)/(3\delta_1^2).\\
\end{aligned}
\]

And with these substitutions,
\[
\begin{aligned}
f_0 - f_1 &=: a,\\
b_2 - b_1 &=: a - \delta_0 d_0 - \delta_1 d_1,\\
\end{aligned}
\]
we get a system of two quadratic equations for \(\delta_0\) and \(\delta_1\):
\[
\begin{aligned}
(d_0 \times d_1)\delta_0 = (a \times d_1) - (3/2)\kappa_1 \delta_1^2,\\
(d_0 \times d_1)\delta_1 = (d_0 \times a) - (3/2)\kappa_0 \delta_0^2.\\
\end{aligned}
\]

Itʼs easy to deal with the case when \(d_0 \times d_1 = 0\) (that is, when the starting and ending tangents are parallel).
For the nonzero case, we reparameterize again according to:

\[
\begin{aligned}
\delta_0 &=: \rho_0 \frac{a \times d_1}{d_0 \times d_1},\\
\delta_1 &=: \rho_1 \frac{d_0 \times a}{d_0 \times d_1}.\\
\end{aligned}
\]
\[
\begin{aligned}
R_0 &:= \frac{3}{2}\frac{\kappa_0 (a \times d_1)}{(d_0 \times a)(d_0 \times d_1)^2},\\
R_1 &:= \frac{3}{2}\frac{\kappa_1 (d_0 \times a)}{(a \times d_1)(d_0 \times d_1)^2}.\\
\end{aligned}
\]

Thus we end up with the very pretty system of quadratics:
\[
\begin{aligned}
\rho_0 = 1 - R_1 \rho_1^2,\\
\rho_1 = 1 - R_0 \rho_0^2.\\
\end{aligned}
\]

We can solve for one of these variables, by substituting from the other equation,
\[
\begin{aligned}
\rho_0
  &= 1 - R_1 (1 - R_0 \rho_0^2)^2\\
  &= 1 - R_1 + 2R_0R_1\rho_0^2 - R_0^2 R_1 \rho_0^4.\\
\end{aligned}
\]

This is a depressed quartic equation, with coefficients \([1 - R_1,\ -1,\ 2R_0R_1,\ 0,\ -R_0^2R_1]\).

##### Pruning solutions

We want solutions with \(\delta_0, \delta_1 \ge 0\) – that is, with the control handles pointing the correct way.
We also want to generally minimize those variables too, otherwise there are outlandish solutions with huge coefficients (particularly ones with loops).
Finally I also added a check that ensures we are getting the correct curvature out of them – for some reason I was getting solutions with flipped curvature.

::: Bonus
Itʼs actually really pretty to see solutions with all signs of curvatures together:
:::

## Implementation

## References

My primary sources/inspiration:

- The wonderful, fabulous, extraordinary [Primer on Bézier Curves](https://pomax.github.io/bezierinfo/) by Pomax.
- [Fitting cubic Bézier curves](https://raphlinus.github.io/curves/2021/03/11/bezier-fitting.html), [How long is that Bézier?](https://raphlinus.github.io/curves/2018/12/28/bezier-arclength.html), and other posts by the excellent Raph Levien.
- Linked from the above, my main reference that convinced me it was possible and made my life easier (except for the hour I spent chasing down a minus sign I forgot): [High accuracy geometric Hermite interpolation](https://minds.wisconsin.edu/bitstream/handle/1793/58822/TR692.pdf;jsessionid=E008B26966FD35F59178ECBD7500CB56?sequence=1) by Carl de Boor, Klaus Höllig, and Malcolm Sabin.

Other miscellanea on Bézier curves:

- [The Uniqueness of the Rational Bézier Polygon is Unique](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4049692) by Javier Sánchez-Reyes
- [The problem with cubic bezier curvature](https://sealedabstract.com/posts/bezier-curvature/) and [related StackExchange post](https://math.stackexchange.com/questions/3833973/smoothing-asymptotic-behavior-in-the-curvature-of-a-cubic-bezier)
- Special cases to [arc length reparameterization of a cubic Bézier](https://math.stackexchange.com/questions/3024630/arc-length-reparameterization-of-a-cubic-bezier-in-parts)
- [the term for the third-derivative analog of curvature for curves is “aberrancy”](https://math.stackexchange.com/questions/3294/how-to-approximate-connect-two-continuous-cubic-b%C3%A9zier-curves-with-to-a-single-o#comment8479_3983)
- [Bézier curvature extrema](https://math.stackexchange.com/questions/1954845/bezier-curvature-extrema/1956264#1956264)
