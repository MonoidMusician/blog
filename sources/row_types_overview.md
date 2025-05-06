---
title: Overview of row types
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2025/04/16
---

the important detail about row types is that you need to be very careful about what “type” labels are

and then, in return, your rows are allowed to be unordered: the order of different labels does not matter [this is carefully phrased :3]

with variables, it's pretty easy to enforce well-behaved labels, since variables are already static identifiers within your syntax, so you don't have to worry about that much

in PureScript, they use the `Symbol`{.ps} kind, the type-level version of `String`{.ps}, and everything basically goes through the constraint system: `IsSymbol (s :: Symbol)`{.ps} allows reflecting a symbol to a string, for FFI, and

`Row.Cons (s :: Symbol) (t :: k) (without :: Row k) (with :: Row k)`{.ps} is the one-at-a-time constraint, and it compute when `s`{.ps} is a literal symbol: `Row.Cons "someLabel" t row ( someLabel :: t | row )`{.ps}

(this constraint computation is [janky](https://github.com/purescript/purescript/issues/3243) in PureScript since it doesn't have proper type equality constraints :neobot_woozy:)

`Row.Cons`{.ps} and the builtin syntax for rows is enough to give types to record gets `_.field :: Record ( field :: t | r ) -> t`{.ps} and updates `_ { field = _ } :: Record ( field :: _prev | r ) -> t -> Record ( field :: t | r )`{.ps}

and there are library functions like `Record.get (Proxy :: Proxy s)`{.ps} as a library function that works with arbitrary symbols, using an `IsSymbol s`{.ps} constraint

notice how this already relies on rows being unordered, so that you can pull *any* field to the front to make it unify with `( field :: t | r )`{.ps}

there is also `Row.Lacks (s :: Symbol) (r :: Row k)`{.ps}, which would be used with record deletes and (optionally) record inserts: these operations are not primitives in PureScript, they need to be provided by FFI (and use `IsSymbol s`{.ps} to reflect the symbol for the FFI)

there is also `Row.Union left right unioned`{.ps} whose name is a little misleading: PureScript made the choice to allow records to have duplicate labels, so if `left`{.ps} and `right`{.ps} have duplicate labels, the types for that label from `left`{.ps} stay on the left, and `right`{.ps} on the right, but distinct labels still can commute around the row type, so you can pull them out

`Row.Union ( x :: X1, y :: Y ) ( x :: X2, z :: Z ) unioned`{.ps} gives you `unioned = ( x :: X1, x :: X2, y :: Y, z :: Z )`{.ps}, which is not equal to `( x :: X2, x :: X1, y :: Y, z :: Z )`{.ps} for example, but *is* equal to `( z :: Z, x :: X1, y :: Y, x :: X2 )`{.ps}

using `Row.Nub withDuplicates withoutDuplicates`{.ps}, a left-biased record merge then has type

```purescript
Record.merge ::
  forall left right withDuplicates merged.
    Row.Union left right withDuplicates =>
    Row.Nub withDuplicates merged =>
  Record left  -> Record right -> Record merged
```

important to note that this is where subtyping breaks down! if you allow your records to be drop fields via subtyping, then this record merge is no longer well-defined, since you could argue that any overlapping fields (that are supposed to come from the left record) actually didn't contain any of those fields, via weakening

we really want the JS implementation of the merge function to just be `Object.assign({}, right, left)`{.javascript}, but this relies on the fact that records contain exactly their stated labels and no more fields, which subtyping cannot do (without a lot of work)

for example, you cannot implement a `pick`{.ps} operation on records without reflecting the *list of labels* you expect (or the list of labels you want to drop):

```purescript
Record.pick ::
  forall input dropped output.
    -- double check that there are no duplicates in the row
    -- (this means the union is commutative, among other things)
    Row.Nub input input =>
    -- the input has more fields, we drop some, and should end up with the output row
    Row.Union output dropped input =>
    -- but Row.Union has no evidence that the runtime can use
    -- to perform this `pick` operation and filter out labels
    -- so we need a new constraint, on either the input or output
    -- to reflect the labels via RowToList and IsSymbol
    ReflectLabels output =>
    -- you could also use this, but it is less direct
    ReflectLabels dropped =>
  Record input -> Record output
```

and I'm *pretty* sure that the reason that row types allow duplicate labels is so that `Row.Union`{.ps} can compute when `left`{.ps} is fully known (a “closed” row) while `right`{.ps} is only partially known (an “open” row)

(it also works in reverse: you can compute `right`{.ps} from `unioned`{.ps} also when `left`{.ps} is a closed row)

if duplicate labels were not allowed, `Row.Union`{.ps} would either have to:
- always block until the full row is known, which is super annoying (and could be implemented with `RowToList`{.ps} at that rate)
- insert a `Lacks`{.ps} constraint on the tail of `right`{.ps} for each field in `left`{.ps}, which is ugly as fuck
- insert a `Nub`{.ps} constraint prior to the result, which isn't as terrible as the other options (since `Nub`{.ps} can make progress on open rows), but it means that you can no longer compute `right`{.ps} from `left`{.ps} and `unioned`{.ps} (because `right`{.ps} could be anything from `unioned`{.ps} minus `left`{.ps} to `unioned`{.ps} itself), so it breaks inference in other ways

so to be the most useful, flexible, and inferable, they decided to allow duplicates in row types and keep the functionality of `Union`{.ps} and `Nub`{.ps} as separate builtins that you can combine when it makes sense

https://pursuit.purescript.org/builtins/docs/Prim.Row

that's kind of all of the rough edges and design considerations of row types in PureScript

---

it is interesting to consider dependent type theory next, because PureScript uses `Symbol`{.ps} for labels, so dependent type theory could just use its basic string type and not have to deal with reflection, right? just use `Record.get s`{.ps} instead of `Record.get (Proxy :: Proxy s)`{.ps}

well it actually doesn't work that way. at least, not well.

even simple stuff like `Row.Lacks label row`{.ps} breaks down when the type checker is confronted with what would now be arbitrary expressions `label`{.ps} and `row`{.ps}: how could it possibly decide whether the label belongs to the row now?

instead, you need a new kind of type, of labels that are guaranteed to either be variables or string constants. that is, the type now induces a separate category of syntax that does not allow arbitrary expressions.

well, i'm a little fuzzy on the exact details of what a dedicated `Label`{.ps} type would mean and how it would be integrated into `Row`{.ps}, because i haven't thought about it for a long while and as far as i know, nobody has implemented it in a dependent type theory (sigh… it's such a good idea).

anyways, the point is that you need to strike a new balance between decidability and abstraction, because the type checker *needs* to be able to grok row types at compile time, otherwise unification falls apart and you don't get the properties of row types like commutativity that you *need* for basic things like `Record.get`{.ps}.

this is what implementations of Haskell “row types” miss out on, is the commutativity of rows and other things.
