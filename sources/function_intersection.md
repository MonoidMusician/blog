What is the intersection of function types?

It is surprisingly hard to find a correct answer online!
And no, it isnʼt a matter of interpretation.

## Setup

Letʼs consider it as functions from sets to sets.
Or as types equipped with set-like operations: intersection, union, and (importantly!) difference.

The contract of a function type is that you can give it an argument of the input type, and it will return a value of the output type.
You can consider this to be implemented with function pointers, closures, functional lambda code – any sort of implementation is okay, as long as it does not preclude assigning various compatible types to a function.

For instance, you can loosen the output from `I -> O1` to `I -> (O1 | O2)`, or restrict the input, `I1 -> O` to `(I1 & I2) -> O`.

### Intersections and unions

If you have a value of type `T1 & T2`, you can use it as a value of both types, at your discretion.

Conversely, the untagged union `T1 | T2` says you have to be able to handle a value of either type: you donʼt get to choose.

These are duals of each other.
Intersections `T1 & T2` come with projections `(T1 & T2) -> T1` and `(T1 & T2) -> T2` meaning the receiver can choose how to interpret it, while unions `T1 | T2` come with injections `T1 -> (T1 | T2)` and `T2 -> (T1 | T2)`, meaning the producer chooses what to put in it.

These are coercions, so the result of computation should never depend on these ascriptions.
And ideally typechecking shouldnʼt, either.

Unions appear more commonly, so they are typically seen as more intuitive.

### Union of function types

If you have an untagged union of function types `(I1 -> O1) | (I2 -> O2)`, the answer is simple: `(I1 & I2) -> (O1 | O2)`.

You cannot inspect the type of the function you receive, so you only know that you can provide it an input that works for both cases: an input in `(I1 & I2)`.
And once you provide it that input, youʼre stuck with an output that could be either in `O1` or `O2` (or, of course, their overlap – it just isnʼt guaranteed).

This makes sense, not least since functions are contravariant in their input (transforming the union of functions into an intersection of input types, and keeping the union in the outputs).

This reasoning doesnʼt work for intersection though.

## Partial answers

There are some easy cases, but they are somewhat misleading towards the full solution.

### Same output type

For `(I1 -> O) & (I2 -> O)`, we know we can use it at the type `(I1 -> O)`, provide an input in `I1`, get an output in `O`, and we can also use it at the type `I2` and get an output also in `O`.
So overall we can provide an input in the union `(I1 | I2)`: that is, `(I1 -> O) & (I2 -> O) = ((I1 | I2) -> O)`.

### Same input type

For `(I -> O1) & (I -> O2)`, we know the input needs to be in the common input type `I`.
What do we get from applying it?
Well we can choose to get `O1` or `O2`, which means it is really `O1 & O2`.

It is easy to see if it is a pure function: `(o : O1) = (f : I -> O1)(i) = f(i) = (f : I -> O2)(i) = (o : O2)` by equational reasoning with the irrelevance of coercions.
Applying `f` both at the type `I -> O1` and at the type `I -> O2` yield the same output `o = f(i)`, so `o` must be in `O1 & O2`.
But the same should also hold if `f` is not a pure function, as long as coercions do not affect evaluation.

## Wrong answers

- MLstruct: chooses `(I1 | I2) -> (O1 & O2)`: https://lptk.github.io/files/%5Bv8.0%5D%20mlstruct.pdf
- Erlang Dialyzer: `(I1 & I2) -> (O1 & O2)`, apparently


### Contravariance/dualization

Dualize the answer for unions of function types.
This suggests that `(I1 -> O1) & (I2 -> O2)` should be `(I1 | I2) -> (O1 & O2)`.

This answer is acceptable, but not correct.
It misses out on important examples.

That is, there is a coercion from `(I1 | I2) -> (O1 & O2)` to the actual intersection `(I1 -> O1) & (I2 -> O2)`.

### Anything goes

On the opposite side, the type `(I1 | I2) -> (O1 | O2)` is too large.
It allows anything to happen on the output side.

However, this type would be useful for implementing function intersections.

### A kernel of truth

`(I1 & I2) -> (O1 & O2)` is part of the answer.

### Lattice

`(I1 | I2) -> (O1 & O2) <: (I1 -> O1) & (I2 -> O2) <: (I1 | I2) -> (O1 | O2) <: (I1 & I2) -> (O1 | O2)`

`(I1 | I2) -> (O1 & O2)              <: (I1 & I2) -> (O1 & O2)               <: (I1 & I2) -> (O1 | O2)`

where `X <: Y` denotes that `X` is stronger/more restrictive than `Y`, so there is a coercion `X -> Y` that weakens `X` to `Y`.

## Correct answers

It turns out that, even though we know that `(I1 -> O1) & (I2 -> O2)` accepts inputs in `(I1 | I2)`, there is no `X` such that it is `(I1 | I2) -> X`, outside of the special cases earlier.

Instead, we need to look at how each part of the input space can act.

Not just `(I1 & I2)`, but also `(I1 | I2) \ (I1 & I2)`: the region outside of the intersection.
This is split into two parts: `I1 \ I2` and `I2 \ I1` (left and right sides, if you will).^[You can equivalently write these as `I1 \ (I1 & I2)` and `I2 \ (I1 & I2)`.]

The answer from here is simple:

- `(I1 & I2)` maps to `(O1 & O2)`
- `(I1 \ I2)` maps to all of `O1`
- `(I2 \ I1)` maps to all of `O2`

So we have a “piecewise function type” consisting of `(I1 & I2) -> (O1 & O2)`, `(I1 \ I2) -> O1`, and `(I2 \ I1) -> O2`.

What connective do we use to join these pieces?
Intersection, because all three are usable in isolation, and union would result in a trivial domain.

So, the intersection of the function types `(I1 -> O1) & (I2 -> O2)` is equal to `((I1 & I2) -> (O1 & O2)) & ((I1 \ I2) -> O1) & ((I2 \ I1) -> O2)`.

This does not seem like a simplification!

### Independent data (products not pullbacks)

`f1 : (I1 -> O1)` and `f2 : (I2 -> O2)` such that they agree on their intersection:

- `i in (I1 & I2)` implies `f1(i) = f2(i) in O1 & O2`

or, `c0 : (I1 & I2) -> (O1 & O2)`, `c1 : (I1 \ I2) -> O1`, and `c2 : (I2 \ I1) -> O2`, subject to no conditions at all

you just stitch back `f1 = c0 ||| c1` and `f2 = c0 ||| c2`

the fact that we encoded the exact restrictions in the types, not in a pullback condition, means that we did make progress

it may not *look* simpler, but we have improved our understanding of what the intersection of function types *is*

### Partitioned cases

My takeaway is that we shouldnʼt work with intersections of function types, we should work with functions by cases.

```
{
  (I1 & I2) => (O1 & O2),
  (I1 \ I2) => O1,
  (I2 \ I1) => O2,
}
```

The only thing that distinguishes this from an intersection is the fact that the input sets are disjoint.^[And non empty, by discarding cases that are empty.]
And the intention is clear: it is a normalized form with all of the restrictions baked into the type.

You can now distribute cases: for each pair of cases

```
{
  I1 => O1,
} & {
  I2 => Q1,
}
```

you compute the partition

```
{
  (I1 & I2) => (O1 & O2),
  (I1 \ I2) => O1,
  (I2 \ I1) => O2,
}
```

and discard the empty cases

### Set view

The set of functions `f : (I1 | I2) -> (O1 | O2)` such that, for all `i in I1 | I2`:

- if `i in I1 & I2`, `f(i) in O1 & O2`
- if `i in I1 \ I2`, `f(i) in O1`
- if `i in I2 \ I1`, `f(i) in O2`
