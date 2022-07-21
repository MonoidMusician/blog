---
title: Eudoxus Real Numbers as Slopes of Pixelated Graphs
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

If you have a little background in classical constructions of the real numbers, you might have noticed that all of the usual constructions start with the rational numbers as a base, and then build up to real numbers.
For example, Cauchy real numbers are constructed as sequences _of rational numbers_ satisfying the Cauchy condition on the usual rational metric.
Dedekind real numbers are constructed as sets _of rational numbers_ satisfying the conditions to make them Dedekind cuts with the usual ordering.

This is a post on constructing real numbers _without_ constructing rational numbers.
Along the way the rationals will sort of be constructed, or at least heavily implied, but they donʼt directly figure into the definition!
Instead all we need is functions from integers to integers.
<!-- (I also want to see if we can get by merely with natural numbers, since they are the simplest infinite mathematical object.) -->

The construction weʼll be talking about is my favorite esoteric construction of real numbers: the _[Eudoxus real numbers](https://ncatlab.org/nlab/show/Eudoxus+real+number)_.

<canvas data-graph="[x => Math.round(2*Math.round(Math.round(x/2) + 2)), x => 3 + Math.round(x/Math.PI) - Math.round(x/Math.E), x => Math.round(-24*x/43)]" class="pixelated" width="1000" height="500" style="width: 100%;"></canvas>

I want to explore _why_ this definition works, not merely recite its definition and properties.
So letʼs go on a journey together through various properties of linear and almost linear functions, and learn how to leverage “wrong” definitions of slope to create the real numbers!

::: Note
_If you donʼt have any background in real analysis or constructions of the real numbers, thatʼs okay.
Bring along your intuitions nonetheless, they will aid you greatly!
We will go over the necessary details as we get to them, although you may miss some of the depth of the discussion when I reference those other things.
Just know that thereʼs a lot of strange and marvelous things going on that make constructing the real numbers an essential part of mathematics.
Are these marvelous properties of the real numbers all coincidences? Surely not_ 😉.
:::

## What makes a good approximation?

Like all constructions of fundamental objects, we need to begin with our existing intuition about what real numbers are in order to construct them out of more basic building blocks.
Just like the integers are built from natural numbers, and rational numbers are built from integers, real numbers need to be constructed from natural numbers, integers, and/or rationals.
In the case of real numbers, we will need two ingredients: approximations, and a lot of them – infinitely many, in fact.

::: Key_Idea
The key property of approximations is that they need to be _easier to grasp_ than the thing we are approximating, otherwise it doesnʼt buy us any leverage!
In our circumstance in particular, each individual approximation should contain a _finite amount of data_, and then we stitch together an _infinite number of approximations_ to represent the complicated object we are constructing: a real number.


::: Details
A convenient way to say that something has a finite amount of data is to say it is countable.
In programmer terms, we could say we want it to be serializable into a string – itʼs equivalent.
Why? Well each natural number or string has a finite amount of data, it is easy to understand or describe all at once.
So if some set we are studying is countable/serializable, meaning it can be understood as if it was a set of natural numbers, then it can serve as an approximation for our purposes.
:::

:::

Thereʼs much more to say about this kind of process, indeed I hope to write a whole nother blog post about it, but for now know that there are two main questions to ask:

(1) When are approximations consistent enough so they ought to represent _a thing_ (e.g. “when does a sequence of rationals converge?”), and
(2) When do two approximations approximate the _same_ thing (e.g. “when do two convergent sequences converge to the same point?”).
<!-- https://mathoverflow.net/questions/239921/concept-associated-to-the-eudoxus-reals -->

::: Example
For example, letʼs think about approximation generators, functions that generate approximations by taking in an error bound and returning a rational number that lies within that error bound of the real number we are trying to approximate.^[Rational numbers are countable, so they qualify as a finite approximation for us!]
Because we can ask for smaller and smaller error bounds, we in fact get an infinite number of approximations that probe closer and closer to the real number. This is good: when approximating an irrational number, no single rational approximation will suffice to capture it!
But not all functions will be well-behaved approximation generators, and even of those that are, there will be many ways of approximating the same number.
Thatʼs why we ask those two questions: when do the approximations work, and when do they agree with each other.
:::

Answering these two questions will let us invent real numbers.
However, the process to get there wonʼt quite be _linear_: the notion of approximation will not be as clear as it was in the example above!

<!--
### Piecing together approximations

There are two main methods of piecing together an infinite number of approximations to represent an object: functions and sets.
Cauchy sequences use functions inventing convergence points, Dedekind cuts use sets inventing extrema.

Thereʼs often a step of determining when two approximations approximate the same value, even if they do so in different ways.

Subsets and quotients.

All of this work lets us represent real numbers by choosing approximations that _should_ represent real numbers to be actual representatives of those real numbers.
-->

## Intuition: What is slope?

Letʼs dive in!

The idea of Eudoxus real numbers is that they represent a real number indirectly, via a functionʼs slope.
We will cover _why_ this works later, but first letʼs agree on _what slope is_ so we can proceed with the construction and then analyze its properties that make it all work.

### Step 1: What functions have slope for sure?

#### Linear functions

Letʼs say we have a function that takes in numbers and outputs numbers.
(We donʼt need to be formal about what kind of numbers yet.)

What **definitely** has a slope?
Well our favorite function from school mathematics does: $f(x) = m*x + b$.
Weʼve memorized that this has slope $m$!

::: Details
Why do we say it has slope $m$? What is slope?
Well, weʼve also memorized that slope is “Rise Over Run”.
If we take two points at $x_1$ and $x_2$, the distance we **run** from the first to the second point is $x_2 - x_1$, and the distance we had to **rise** to keep up with the function is $f(x_2) - f(x_1)$.
Slope as “rise over run” is therefore
$$\frac{f(x_2) - f(x_1)}{x_2 - x_1}.$$

The beautiful thing about $f(x) = m*x + b$ is that functions of this form have _constant slope_, no matter what $x_1$ and $x_2$ are!
$$\begin{align*}\frac{f(x_2) - f(x_1)}{x_2 - x_1} &= \frac{(m*x_2 + b) - (m*x_1 + b)}{x_2 - x_1} \\&= \frac{(m*x_2 - m*x_1) + \cancel{(b - b)}}{x_2 - x_1} \\&= \frac{m*\cancel{(x_2 - x_1)}}{\cancel{x_2 - x_1}} \\&= m.\end{align*}$$

The inputs \(x_1\) and \(x_2\) disappear, meaning it traces out a line with a constant slope – itʼs why we call it a linear function.
So weʼre pretty happy saying that this function has slope $m$.
Tada! 🎉
:::

#### What slopes does this give us?

This is where we have to ask what kind of numbers we are using, because that determines what $m$ can be.
If the function takes in real numbers and outputs real numbers, $m$ can surely be any real number too.
But tough luck – we canʼt use that to _construct_ real numbers, no circular definitions allowed!

Maybe $m$ can be any rational number.
Thatʼs fine – slope works in the same way.
But we run into another roadblock: the function still only has one well-defined slope, and itʼs stuck being a rational number.
Thereʼs no magic of infinity happening.

What if we say that $f$ is an integer function?
Here we have to be careful: integer division isnʼt necessarily well-defined, **but** if we know $m$ is an integer, then it happens to work out in this case: the denominator will always evenly divide the numerator, and out pops $m$.
This seems like an even worse situation, getting stuck with integers!
But wait …

We need a little wiggle room.
Having a constant slope is too restrictive!
Can we loosen up and find something that is still like slope, but only approximately?

### Step 2: Graphing calculators approximate slope

Having hit these roadblocks, we need some inspiration.
Letʼs revisit the idea of integer functions but in a new way.

Hereʼs the thing: consider a graphing calculator, or a graphing app on a computer.
What happens when we ask it to graph $f(x) = (1/\pi) * x$?
(Pick your favorite irrational-but-[computable](https://en.wikipedia.org/wiki/Computable_number) real number there.)

It does some math, and shows us a line on a screen …
but wait, that line isnʼt a perfect mathematical function from real numbers to real numbers, itʼs a pixelated entity:
_itʼs an **integer** function that approximates the **real** function_.^[Okay, thereʼs some slight details here, like [antialiasing](https://en.wikipedia.org/wiki/Spatial_anti-aliasing) and the fact it could draw several pixels stacked vertically.
But for the sake of simplification letʼs assume that it only renders one pixel in each column, making it an honest-to-goodness integer function.]

<canvas data-graph="[x => Math.round(x/Math.PI)]" class="pixelated" width="1000" height="500" style="width: 100%;"></canvas>

#### Sampling rational slope

What happens as we keep scrolling the screen?
We could keep sampling snapshots of the screen at each moment, and see how they vary.

If we had picked an integer slope, every snapshot would appear to have the same slope.
For a slope of $0$, there is never any change.
For a slope of $1$, it steadily moves up pixel by pixel.
For a slope of $2$, it skips a pixel as it goes.
Et cetera.
This isnʼt so interesting, yet!

<canvas data-graph="[x => 3, x => x - 4, x => 2*x]" class="pixelated" width="1000" height="500" style="width: 100%;"></canvas>

But if we had picked a rational slope, we start to see something interesting happen:
It doesnʼt always step by the same amount, sometimes it jumps more than it does other times.

For instance, a slope of $1/7$ would only jump up a single pixel every $7$ pixels.
A slope of $4/3$ would jump up $2$ pixels one time, then jump up a single pixel twice more, making visible groups of three.

<canvas data-graph="[x => Math.round(x/7 + 3), x => Math.round(4*x/3), x => Math.round(2*(x+3)/9) - 4]" class="pixelated" width="1000" height="500" style="width: 100%;"></canvas>

More complicated fractions will look more complicated and repeat over longer distances.
Can you figure out what the third line graphed in this figure is?
(Hint: how long does it take to repeat?)

#### Sampling irrational slope

However, this same phenomenon occurs for irrational slopes too: how do we tell the difference now??

The key is not how the jumps repeat in individual snapshots, but how they repeat across the whole domain of the function.

Do the snapshots recur in a nice pattern?
If so, that must be graphing a line with rational slope!
They will recur when the window has shifted by the denominator of the slope.

::: Key_Idea
*If the snapshots recur irregularly, without fitting a nice pattern, it is graphing an irrational number.*
:::

(Since there are a finite number of snapshots that will be generated for a particular line, they must recur – thatʼs not the point.
The point is whether they recur on a fixed schedule.)

This is where we cross over from computation to mathematics:
We canʼt hope to decide whether this is the case from a finite number of snapshots!
Instead we leave it to the realm of mathematical proof to definitely establish whether a slope is rational or irrational.
(We might not know!)

### Step 3: What weirder functions might still have slope?

Okay, we are ready to take the knowledge we learned from graphing calculators and translate it back into mathematics.

We agreed that linear functions of all types have slope, but only linear functions using real numbers contained enough data to represent real numbers.
However, we saw that calculators approximate real functions with integer functions – this is our hope of salvation, if we can connect the two ideas.

What, then, will tell us whether some **integer** function approximates a **real _linear_** function?

#### Integer functions with rational slope

We said that integer-function approximations to lines with rational slopes had a key property:
they recur in a consistent pattern!

If we call the period of recurrence $p$, this property is that
$f(x + p) = f(x) + d$, for some $d$.

What is this mysterious $d$ though?
Itʼs the **rise** for the **run** $p$.
Weʼre saying that the function repeats every time we shift by $p$ steps of the $x$-axis, but it has shifted by $d$ in the $y$-axis in the meantime.

Thatʼs right, weʼve circled back to good olʼ slope, but in a new way:
instead of saying that we want the function to be linear by having rise over run being constant for all $x_1, x_2$ we choose, we just want it to be constant when $x_2 - x_1$ is a known period.

This is much less restrictive: in particular, it lets us get rational slopes back out of integer functions!
If we define the slope to be $d/p$, we see that for integer approximations it matches the slope of the original rational linear function, which we said would recur with period of the denominator, and of course it must travel vertically by the numerator in that time.

<canvas data-graph="[x => Math.round(x/2 + Math.sin(x*Math.PI/2)), x => Math.round(-x/2) + Math.round(3*Math.cos(0.5 + x*Math.PI/4))]" class="pixelated" width="1000" height="500" style="width: 100%;"></canvas>

With how they wave around, these two functions donʼt look linear anymore, but they do follow a nice recurrence.
Their shapes keep repeating every $4$ or $8$ pixels, while they climb or fall by half that each time.
That is, they have slope $+1/2$ and $-1/2$ in our new system, even though they are not linear.

#### Irrational “slopes”

::: Error
We hit a roadblock with irrational slopes, though.
If we donʼt have a period to work with, how can we get our hands on a slope from an integer function??
:::

The answer is … [drumroll] … **we donʼt!!!!**

### Step 4: Maybe we donʼt need slopes

> Huh?!?! 🤨😧😱

What. Donʼt give me that look. 😄😌😇

Look here: we said linear functions were boooring.
Linear functions were just their slope!
pluuus a second term that merely shifts it vertically, as if that matters.
Yawn 😴😪.

However, now weʼre going to use this correspondence as leverage to do something _truly_ exciting:
represent slopes **as the functions weʼre given!**

We donʼt need an independent notion of slope if we can isolate the properties that _arenʼt_ slope in order to distill the functions down into their slopiest essence. 🍾✨

#### Whatʼs _not_ slope, in fact?

As we just said, shifting by a constant amount vertically will not impact slope.
However, we can do infinitely better than this.

Recall that weʼre looking at integer functions through their whole domain, not just at a single slice anymore.

Say the calculator made an error and calculated a pixel incorrectly.
(The horror!
Quick – letʼs blame it on cosmic radiation.)

Should this _single error_ affect the slope?
Nope!
Itʼll quickly get washed away in the _infinite other data_ that informs us what the slope is.

Should another error affect the slope?
Nahhh.
Should any finite amount of errors affect the slope?
Not even!

Thus we can say that shifting vertically doesnʼt influence the slope and any finite number of errors wonʼt influence it either.

Putting these together, we can wrap them up as a single property, and with a little extra sprinkle on top:
_any two functions that differ by a **bounded amount** represent the same slope!_^[This accounts for the two above properties as well as infinite differences that are not merely a shift but are still bounded.
In fact, this allows us to get rid of horizontal shifts, as we will see [later][Horizontal shifting].]

<canvas data-graph="[x => Math.round(x/2 + 4 + 2*Math.random()), x => Math.round(x/2 - 4 - 2*Math.random())]" class="pixelated" width="1000" height="500" style="width: 100%;"></canvas>

#### What _is_ slope, really?

Finally we get to identify what slope actually is.
Took us long enough!

::: Key_Idea
Hereʼs the key property:
we want to ask that the quantity $d_f(x_1, x_2) = f(x_1) + f(x_2) - f(x_1 + x_2)$ is _bounded_ for the integer functions we are considering.

The [n-lab](https://ncatlab.org/nlab/show/Eudoxus+real+number) calls this property “almost linear”.
Letʼs also call $d_f(x_1, x_2)$ the “wiggle” of the function $f$ between $x_1$ and $x_2$.
:::

As a first check, letʼs see that this happens for actually linear functions $f(x) = m*x + b$:
$$\begin{align*}&\ f(x_1) + f(x_2) - f(x_1 + x_2) \\=&\ (m*x_1 + b) + (m*x_2 + b) - f(x_1 + x_2) \\=&\ (m*(x_1+x_2) + 2*b) - f(x_1 + x_2) \\=&\ (\cancel{m*(x_1+x_2)} + \cancel{2}*b) - (\cancel{m*(x_1 + x_2)} + \cancel{b}) \\=&\ b.\end{align*}$$

Whoa, itʼs not only bounded, itʼs actually constant!
And the constant is the vertical shift we said we didnʼt care about, interesting.

What about for integer approximations of linear functions?

##### Redux: Linearly-periodic approximations

First we can look at the linearly-periodic approximations of rational linear functions:
saying the period is $p$, we mean that $f(x + p) = f(x) + d$ for all $x$.
So as one example, if we pick $x_2 = p$, then for any $x_1$, the quantity weʼre looking at $f(x_1) + f(p) - f(x_1 + p)$ is just $f(p) = f(0)$.
Constant, thus bounded.
(Note how nicely this matches up with the perfectly linear case, since $f(0) = b$ then!)

We can play this game again: $f(x_1) + f(2*p) - f(x_1 + 2*p) = f(2*p) = f(0)$ as well.

But what about other values of $x_2$ that donʼt fit into the period?
The key is that it $f(x_1) + f(x_2) - f(x_1 + x_2)$ is not just linearly-periodic, but actually periodic in both $x_1$ and $x_2$ now, having the same period $p$ of course:

$$\begin{align*}&\ f(x_1) + f(x_2 + p) - f(x_1 + (x_2 + p)) \\=&\ f(x_1) + (f(x_2) + d) - (f(x_1 + x_2) + d) \\=&\ f(x_1) + f(x_2) - f(x_1 + x_2).\end{align*}$$

The $d$ cancels out from the two terms that changed.

The upshot is that we donʼt need to worry about _all_ values of $x_2$, just one period worth of it, from $1$ to $p$.
Explicitly, the wiggle of the function is bounded by the quantity $$\max(|f(1)|, |f(2)|, \dots, |f(p)|).$$

##### Irrational approximations

We didnʼt identify a property for abstractly looking at approximations of linear functions with irrational slopes, like we did with approximations of rational slopes.
However, if we look at a typical approximation function, we know that, with whatever integer rounding it is using, its integer value will be within $\pm 1$ of the actual real value at any given point.
So when you look at the three terms $f(x_1)$, $f(x_2)$, and $f(x_1 + x_2)$, it is clear that the wiggle will be within $\pm 3$ of the value of the wiggle of the underlying linear function, which we know is constantly $b$.
So the wiggle is clearly bounded.

## Just tell me: Definition

Having satisfied our curiosity that we have identified the necessary ingredients, letʼs finally define the actual mathematical object and the operations on it.

The **Eudoxus real numbers** are **almost linear functions** but where two functions are considered the same when they have a **bounded difference**.

- **Almost linear functions** have bounded wiggle, which is our name for the quantity $d_f(x_1, x_2) = f(x_1) + f(x_2) - f(x_1 + x_2)$.
  This property should make it possible to say this function has a slope!
- **Bounded difference** just means that $f(x) - g(x)$ is bounded.
  This property eliminates what data each function carries that is extraneous to our goal of defining slopes.

### Arithmetic

#### Addition

We can add slopes in the obvious way, by adding the functions together (pointwise!): $(f+g)(x) = f(x) + g(x)$.
And we can negate pointwise $(-f)(x) = -f(x)$, and the zero for this is of course the constantly zero function: $0(x) = 0$.
For example, we can add slope $1/6$ to slope $-1/2$ to get slope $-1/3$:

<canvas data-graph="[x => Math.round(x/6), x => Math.round(-x/2), x => Math.round(-x/2) + Math.round(x/6)]" class="pixelated" width="1000" height="500" style="width: 100%;"></canvas>

Huh, that middle line doesnʼt look like the graph of the line with slope $-1/3$!
Itʼs very flakey, like it almost canʼt decide whether it wants to go up or down.
Still, it looks like it does have the right trend overall, a general slope of $-1/3$, and we will prove this later: addition works out just fine, within the bounded wiggle we expect!

#### Multiplication

Now we get to my favorite part: how to multiply slopes.

Multiplying the functions pointwise is the wrong move: that would produce something like quadratic functions, or worse.

<canvas data-graph="[x => 15 + Math.round(-x/4) * Math.round(x/7)]" class="pixelated" width="1000" height="500" style="width: 100%;"></canvas>

That is a sad looking parabola indeed ☹️.

Hmm … oh wait! What about _composing_ functions?

If you compose two functions, you multiply their slopes!
So we have $(f*g)(x) = f(g(x))$.
This suggests that the identity function acts as the identity for this multiplication, $1(x) = x$ (of course it has slope $1$!).

<canvas data-graph="[x => Math.round(x/6), x => Math.round(-x/2), x => Math.round(-Math.round(x/6)/2)]" class="pixelated" width="1000" height="500" style="width: 100%;"></canvas>

### Ordering

The goal is to come up with an ordering on almost-linear functions that is consistent with the arithmetic and the equivalence relation.
So we can take a shortcut: we only need to define what it means to be positive, then $f > g$ is defined by the difference $f - g$ being positive.

Before we ask what it means to positive or negative, what does it mean to be zero?
We said that zero was the constant function – but wait, itʼs actually the _equivalence class_ of the constant function, which consists of all functions that are bounded (since that means they have bounded difference from zero).

Positive and negative, then, are going to be unbounded functions.
How do we distinguish them?

The basic idea is that positive will grow unboundedly _positive_ to the right and negative will grow unboundedly _negative_ to the right.
In formal terms, we say that $f$ is positive if, upon picking some arbitrary height $C$, it is eventually always above $C$ as it goes towards the right: i.e. there is some $N$ such that for all $m > N$, $f(m) > C$.

There are some properties to verify.

First off, why is it sufficient to just consider the behavior of the function to the right (i.e. for $m > 0$)?
We would need to know that growing unboundedly positive to the right is the same as growing unboundedly negative to the left.
This conforms with our intuition of how almost linear functions have slope, but it requires a proof.

<!--
Secondly, the [n-lab](https://ncatlab.org/nlab/show/Eudoxus+real+number) defines it as having infinitely many points on the right above an arbitrary height $C$, and we need to show that it is equivalent to what we said above: having _all_ points to the right of some point be above the height.

::: Details
We want some assurance that they express the same property of almost linear functions.
Why does knowing that _some_ infinite number of points are higher than $C$ tell us that _all_ points beyond some point are higher than $C$?

If it was false, that would mean there was some $C$ for which there are infinitely many points _below_ $C$, and yet it is still the case that for all $C^\prime$ there are infinitely many points _above_ $C^\prime$, _and_ it is still almost-linear.

Infinitely below $C$, and infinitely above $C^\prime > C$.
$f(m+n)-f(n)-f(m)$
:::
-->

The other properties of how the ordering interacts with arithmetic are straightforward, however:

1. Negation works as expected: $-f > -g$ holds exactly when $g > f$ holds, since the first means $(-f) - (-g)$ is positive, and the second means $g - f$ is positive.
2. Addition works as expected: $f_1 + f_2 > g_1 + g_2$ if $f_1 > g_1$ and $f_2 > g_2$, since $(f_1 + f_2) - (g_1 + g_2) = (f_1 - g_1) + (f_2 - g_2)$, and obviously the sum of two positive functions is still positive.
3. Multiplying by a positive factor works as expected: $f * h > g * h$ if $f > g$ and $h > 0$ holds by distributivity $f * h - g * h = (f - g) * h$ and multiplication of positives is obvious too.

## Does it work? Properties

### Slope

The million dollar question: does it really have a slope?

To determine this, weʼll basically use Cauchy real numbers.
Recall that those are equivalence classes of sequences of rational numbers.
Can we construct a Cauchy real number from _our_ definition of real numbers, to capture the slope of our almost-linear functions?

Recall our definition of slope as rise over run.
We should be able to pick two arbitrary points and compute their slope, right?
It will be a rational number, since weʼre dealing with integer functions.
And then presumably as those points get further and further away from each other, the approximation will get better and better.

We might as well pick $0$ as our basepoint and move out to positive numbers, like so:
$$\lim_{n \to \infty} \frac{f(n) - f(0)}{n - 0}.$$

Except notice that $f(0)$ is a constant in the numerator, and since the denominator grows to $\infty$, some basic properties of limits tell us that it is irrelevant, so we can instead use this limit:
$$\lim_{n \to \infty} f(n)/n.$$

In fact this realization is the key step that informs us that the limit is well-defined on equivalence classes: any bounded difference between two functions will not affect this limit.
So we could also assume that $f(0) = 0$ without loss of generality, since that will be in the same equivalence class.

Now we just need to show that it satisfies the Cauchy condition, assuming $f$ is almost linear:

$$\forall \epsilon > 0, \exists N > 0, \forall n, m > N, \\ \left|f(n)/n - f(m)/m\right| < \epsilon.$$

We will go through what this means including a proof later, since it requires more machinery.

But weʼve half-answered our question already: we have done what we can to isolate slope from the other data the functions carry, and it just remains to confirm that we can in fact define slope from it.

###  Horizontal shifting

Thereʼs another obvious property that we need slopes to be invariant under:

Weʼve covered vertical shifting, but what about horizontal shifting?
Does bounded (vertical) differences suffice to cover bounded (horizontal) differences?
Obviously not for arbitrary functions, but hopefully for the almost-linear functions we are covering?

It turns out yes, being almost-linear is exactly what it means that horizontal shifts of the function require just bounded vertical shifts to compensate:
we need $f(n) - f(n+c)$ bounded (for fixed $c$), but being almost linear means that $f(c) + f(n) - f(n+c)$ is bounded which is different just by the constant $f(c)$.

For example, these two functions with slope $3/7$ are shifted by $6$ relative to each other, so their different forms a pretty checkerboard-like pattern, which is clearly bounded, only taking the values $2$ and $3$ as the functions grow in close proportion.

<canvas data-graph="[x => Math.round(3*(x+3)/7), x => Math.round(3*(x-3)/7), x => Math.round(3*(x+3)/7) - Math.round(3*(x-3)/7)]" class="pixelated" width="1000" height="500" style="width: 100%;"></canvas>

### Monotonicity

Thereʼs one property we have not mentioned yet: monotonicity.

All of the linear approximation functions we would come up with are monotonic: either for all $m \le n$, $f(m) \le f(n)$ (weakly monotonically increasing), or for all $m \le n$, $f(m) \ge f(m)$ (weakly monotonically decreasing).
But weʼve never mentioned this.
Why not include it as a property we care about having or respecting?

The first clue is what we saw above with addition: when we added two monotonic representative functions (with slope $1/6$ and $-1/2$), the result wasnʼt monotonic anymore, it went up and down and up and down, although in a nicely repeating pattern.
So our naïve definition of addition did not preserve monotonicity.^[Note that the plain definition of multiplication _does_ preserve monotonicity!]

However, you can in fact take any representative function and make it monotonic by only a bounded difference – but with one big catch: you have to know up front whether the slope is zero, positive, or negative,— and that property is [undecidable](https://en.wikipedia.org/wiki/Computable_set).

So it just seems more fussy and complicated to require the representative functions be monotonic, even though it could be done without resulting in a different theory.

## Proofs

### Addition

All of the lemmas involving addition are straightforward, so I wonʼt go through them here, but feel free to ask me if you have questions!

### Multiplication distributes over addition

This is my **favorite** theorem, and the reason I like this construction so much.

Distributivity is what lets you pull out common factors, like so: $(f+g)*h = f*h + g*h$.

Watch what happens when we use our definitions of these operators (recall that multiplication is function composition):

$$((f+g)*h)(x) = (f+g)(h(x)) = f(h(x)) + g(h(x)).$$

Thatʼs right, it falls out for free based on how we defined multiplication and addition, just because of how functions work!
Isnʼt that so cool?

But … thereʼs a catch.

Proving that multiplication _on the left_ distributes over addition is not so straightforward:
$$(h*(f+g))(x) = h((f+g)(x)) = h(f(x) + g(x)) =\ ??$$

There doesnʼt seem to be a direct way to attack this.
However, once we prove that [multiplication is commutative], it doesnʼt matter anymore.
(In fact, I suspect that proving left-distributivity directly requires similar arguments to the commutativity of multiplication anyways.)

### Multiplication is well-defined

Thereʼs two aspects to show that the multiplication is well-defined: since weʼre dealing with subquotients, we need to show that the result satisfies the necessary property, and also that it respects the quotienting relation.

#### Multiplication is almost-linear

I think this first aspect requires one of the more tricky proofs:
$f*g$ is almost linear if $f$ and $g$ are.
(Remember we defined multiplication as the composition of those functions, not pointwise multiplication!)

Assume $f(m+n) - f(m) - f(n)$ and $g(m+n) - g(m) - g(n)$ are bounded, then show that $f(g(m+n)) - f(g(m)) - f(g(n))$ is bounded as well.

For the set-up, we observe that $f(a(m,n)+e(m,n)) - f(a(m,n)) - f(e(m,n))$ is bounded (by assumption), and we choose strategic functions $a(m)$ and $e(n)$.

In particular, if $e(m,n)$ is bounded by $b$, then $f(e(m,n))$ is clearly also bounded, thus so is $f(a(m,n)+e(m,n)) - f(a(m,n))$ – namely, by the maximum of $f$ on the interval $[-b,b]$.

What quantity do we know is bounded?
Letʼs choose $e(m,n) = g(m)+g(n)-g(m+n)$.

For the other function we choose $a(m,n) = g(m+n)$, which makes their sum $a(m,n)+e(m,n) = g(m)+g(n)$.

Plugging these in, we see that $f(g(m)+g(n)) - f(g(m+n))$ is bounded.
But $f(g(m)+g(n)) - f(g(m)) - f(g(n))$ is also bounded by the first assumption.
Thus their difference $f(g(m+n)) - f(g(m)) - f(g(n))$ is bounded.
[Q.E.D.](https://en.wikipedia.org/wiki/Q.E.D.)

#### Multiplication preserves bounded-differences

We also need that if $f_1 \sim f_2$ and $g_1 \sim g_2$ then $f_1*g_1 \sim f_2*g_2$.
We can decompose this into two steps, varying one side at a time: $f_1 \sim f_2 \implies f_1*g \sim f_2*g$ and $g_1 \sim g_2 \implies f*g_1 \sim f*g_2$.

The first is trivial: if $f_1(x) - f_2(x)$ is bounded, of course $f_1(g(x)) - f_2(g(x))$ is also bounded, just by properties of functions!

The second step also makes sense, but is trickier to formalize: if $g_1(x) - g_2(x)$ is bounded, then $f$ being almost linear should preserve this bounded difference.
Itʼs not like $f$ can stray too far away so as to make an unbounded difference on closely bounded inputs.

So how do we knead the almost-linear condition into a form that tells us about $f(g_1(x)) - f(g_2(x))$?
Well, what it looks like now is:
$$f(m+n) - f(m) - f(n)\ \text{bounded},$$
and to ask about the _difference_ of $f$ at certain input, we want to pick $m+n = g_1(x)$ and $m = g_2(x)$, which makes $n = g_1(x) - g_2(x)$, giving us:
$$f(g_1(x)) - f(g_2(x)) - f(g_2(x) - g_1(x))\ \text{bounded}.$$
But weʼre done now, since $g_2(x) - g_1(x)$ is bounded, making its image under $f$ bounded, so using the above fact, $f(g_1(x)) - f(g_2(x))$ is really bounded as we wanted.

### Machinery

Following [Arthanʼs exposition](http://arxiv.org/abs/math/0405454), we will need some more machinery before we tackle the rest of the proofs.
Using the tools of [mathematical analysis](https://en.wikipedia.org/wiki/Mathematical_analysis) we will establish further properties that capture the almost-linearity of functions, using the existing property of bounded wiggle.

Let $C$ be a bound for the wiggle of the function; i.e. $d_f(p, q) < C$ for all $p$ and $q$.

The first lemma^[Part of lemma 7 in Arthan] we want to establish is the following^[Exercise: Why is this related to $f$ being an almost linear function? What if $f$ was actually a linear function?]:
$$|f(pq) − pf(q)| < (|p| + 1)C.$$

We need to use induction on $p$.
This wasnʼt obvious to me at first, but it makes sense in retrospect!

::: Details
For the base case $p = 0$, it is easy:
$$|f(0) - 0| = |f(0)| = |f(0 + 0) - f(0) - f(0)| = |d_f(0, 0)| < C.$$
However, letʼs rewrite it using the point $q$, to make the next steps of induction clearer:
$$|f(0)| = |f(0 + q) - f(0) - f(q)| = |d_f(0, q)| < C.$$

Now we need to establish that if it holds for $p$, then it holds for $p+1$:
$$|f(pq) - pf(q)| < (|p| + 1)C \implies\\ |f(pq + q) - pf(q) - f(q)| < (|p| + 2)C.$$

How do we get from $f(pq) - pf(q)$ to $f((p+1)q) - (p+1)f(q)$?
The difference is $f((p+1)q) - f(pq) - f(q)$: but this is actually $d_f(pq, q)$, so it is also less than $C$ in absolute value.

So this means that each step we take changes it by less than $C$, and weʼre done by induction^[Technically we also need to cover the cases where $p$ is negative, but thatʼs easy.].
We can write it all out in one step like this:
$$|d_f(0, q) + d_f(q, q) + d_f(2q, q) + \cdots + d_f(pq, q)| < (|p|+1)C.$$
:::

Using that lemma, we get to the next lemma^[Lemma 7 in Arthan] in a very slick manner:
$$|pf(q) − qf(p)| < (|p| + |q| + 2)C.$$

Our task here is to compare $pf(q)$ and $qf(p)$.
The magic of the above lemma is that it compared these values to a _common_ value: $f(pq) = f(qp)$.
So we have what we need now:
$$|pf(q) - qf(p)| \le |pf(q) - f(pq)| + |f(qp) - qf(p)| < (|p| + 1 + |q| + 1)C.$$

While weʼre here, we need one more lemma^[Lemma 8 in Arthan] that certifies that $f$ acts like a linear function, approximately.
We show that there are some constants $A$ and $B$ such that the following holds for all $p$:
$$|f(p)| < A|p| + B.$$

Notice that this also says that $f$ is behaves like a linear: it cannot grow outside the confines of the linear function $A|p| + B$ (although those $A$ and $B$ may be large numbers, thus providing only loose bounds on $f$).

It comes immediately from the above lemma: take $q = 1$ to get
$$|pf(1) - f(p)| < (|p| + 3)C$$
so with some rearranging we get $f(p)$ by itself and a coefficient for $|p|$:
$$|f(p)| < (f(1) + C)|p| + 3C.$$

Thus we can take $A = (f(1) + C)$ and $B = 3C$ to accomplish the task.


### Slope converges

Now we _finally_ have the tools we need to prove that we can get a slope out of these almost linear functions!
Recall that we were going to define slope as
$$\lim_{n \to \infty} \frac{f(n) - f(0)}{n - 0} = \lim_{n \to \infty} f(n)/n.$$

And to show that it converges, we show that it is a Cauchy sequence, meaning that terms in the sequence get arbitrarily close beyond a certain point:
$$\forall \epsilon > 0, \exists N > 0, \forall n, m > N, \\ \left|f(n)/n - f(m)/m\right| < \epsilon.$$

Well, the lemma above gives us exactly what we need, just pretend that $n$ and $m$ stand for $p$ and $q$:
$$\left\vert\frac{f(n)}{n} - \frac{f(m)}{m}\right\vert = \frac{mf(n) - nf(m)}{mn} < \frac{(m+n+2)C}{mn}.$$

Notice how the numerator is like $m+n$ and the denominator is like $m*n$, clearly the denominator will win as $m$ and $n$ get large!

::: Details
To give more details, notice how we can bound the fraction like so
$$\frac{(m+n+2)C}{mn} = \frac{C}{n} + \frac{C}{m} + \frac{2C}{mn} < \frac{2C}{N} + \frac{2C}{N^2} < \frac{4C}{N^2},$$
since $m, n > N$ and $N^2 > N$.

So now to get $4C/N^2$ to be less than $\epsilon$, we need $N^2 > 4C/\epsilon$, i.e. $N > \sqrt{4C/\epsilon}$.
So as $\epsilon$ gets really tiny and close to $0$, $N$ has to grow very large in response.
But this is fine: it exists by the [Archimedean property](https://en.wikipedia.org/wiki/Archimedean_property), completing our proof.
:::

### Multiplication is commutative

Let $f$, $g$ be two almost-linear functions, we need to show that $f*g = g*f$ by showing that $w(p) = f(g(p)) - g(f(p))$ is bounded.

We will use the same trick as above, comparing $f(g(p))$ and $g(f(p))$ to a common term.
In fact itʼs a titch more subtle still: we will add extra factors of $p$ first, comparing $pf(g(p))$ and $pg(f(p))$ to the common term $g(p)f(p)$.
Do you see where this is going?
We get to use our lemmas from above!

Take $q = f(p)$ and $q = g(p)$ in our favorite lemma, to give us these two inequalities:
$$|pf(g(p)) − g(p)f(p)| < (|p| + |g(p)| + 2)C$$
and
$$|g(p)f(p) − pg(f(p)| < (|p| + |f(p)| + 2)C.$$

Now to get $|pf(g(p)) - pg(f(p))|$ we take the sum, getting a bound of $$(2|p| + |f(p)| + |g(p)| + 2)C.$$

That kind of looks awful, Iʼm not going to lie, **but** weʼre sooo close.
Remember the other lemma that told us that $|f(p)|$ and $|g(p)|$ behaved roughly like $|p|$?
We can use that here to say that it _all_ behaves like $|p|$:

$$(2|p| + |f(p)| + |g(p)| + 2)C < (2+A+A)C|p| + (B + B + 2)C.$$

We can squash all that nonsense behind two new constants $D$ and $E$, reaching our next conclusion:
$$|pf(g(p)) - pg(f(p))| = |p|w(p) < D|p| + E.$$

Take a breath … One last step.

Both sides now behave like $|p|$.
If you look really closely, this actually implies that $w(p)$ is bounded:
it must sort of behave like $D$ (the slope of $|p|$) on the other side.
(It certainly canʼt behave like $|p|$ or $|p|^2$ or anything else that grows faster!)

::: Details
More specifically, if we have $|p| w(p) < D|p| + E$, we can divide through by $|p|$ to get
$$w(p) < D + \frac{E}{|p|}.$$

This is only valid when $p \ne 0$, however!
Since $|p|$ is an integer, it would then have to be at least $1$, so $w(p) < D + E$ in that case.

And dealing with the case $p = 0$ on its own poses no problem (it bounds itself), so overall we have
$$w(p) \le \max(D + E, w(0)).$$
:::

This establishes that $w(p) = f(g(p)) − g(f(p))$ is bounded, and thus multiplication commutes!

## Reflection & Take-aways

Whew! Hopefully you enjoyed our journey through almost-linear integer functions to get to Eudoxus real numbers.

Here are some things I did my best to convey in this article:

- We started with the intuition of how graphing calculators display pixelated lines in order to find a notion of slope that we could derive from integer functions.
- My saw my favorite parts of Eudoxus real numbers: looking at the slope of lines means [function composition _creates_ multiplication][multiplication], and we get [distributivity][multiplication distributes over addition] for free that way.
- Perhaps you got a glimpse of the non-local nature of dealing with infinity: only the loooong term behavior matters when defining slope, because finite differences and even bounded differences simply [disappear in the limit][slope].
- And if you stuck around for the [proofs] (no shame if not!), you also got to peek at the methods of [mathematical analysis](https://en.wikipedia.org/wiki/Mathematical_analysis), where techniques for dealing with limits and bounding abstract quantities are studied and put to use.
  (This is often covered in a “Real Analysis” course in colleges and universities.)

Thereʼs a lot still to talk about – like “what does it mean to _construct_ a mathematical entity and what tools do we use to do so?”, and we didnʼt even prove that multiplicative inverses exist (what would that look like visually? I bet you have the intuition!) or that the Eudoxus reals are [sequentially complete](https://en.wikipedia.org/wiki/Complete_metric_space).
But this is more than enough for now – the intuition is more important than the details.

::: Bonus
For more on Eudoxus reals, including a cool construction of $\pi$ as a Eudoxus real number and references to further connections made with the construction, I would recommend [Matt Baker's Math Blog](https://mattbaker.blog/2021/12/15/the-eudoxus-reals/).
And of course see the [n-lab](https://ncatlab.org/nlab/show/Eudoxus+real+number), where I originally learned about this construction and which I already referenced several times, and [Arthanʼs exposition](http://arxiv.org/abs/math/0405454), which helped me fill in the tougher proofs.
:::
<br>
