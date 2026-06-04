---
title: Selective Applicative Functors
subtitle: Applications and Examples
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---


[Parsers](https://hackage.haskell.org/package/parsley-2.0.0.1/docs/Parsley-Selective.html).
Bidirectional codecs, pattern matching.
Build systems.
Compilers.
Batched data fetching (Haxl).

Selective applicatives and nuanced control flow make these applications *all* better!
Why arenʼt we all using them?


## Parsers

The original thing that put me on to this was a simple idea:

Why are context-free parsers, for human readable languages, so different than parsers for binary formats?

Context-free grammars require a lot more effort to parse (disambiguation, pushdown context, waiting for closing brackets) than binary formats, which are nominally context-sensitive but also easier to parse in practice.
Length-prefixed data allows allocating buffers ahead of time and quick scanning.
It makes the grammar more compositional, even: do not need to avoid certain tokens in the grammar.

On the other hand, there isnʼt a standard format for binary formats: by resigning ourselves to thinking of them as “context-sensitive grammars”, we have lost the ability to statically document them.
To document the code paths, the binary shapes that underly the grammar.
And *most* importantly: this way we can represent the binary reader and writer in the same structure.

Iʼm not saying that bidirectional printer–parsers are perfect or will prevent all bugs.
But keeping the logic for printing and parsing close to each other has got to count for something, and keeping the *structure* unified is exactly how it should be.

### Selective Applicative Parsers

The idea of selective applicative parsers is simple:

#.  First we capture the grammar: the terminals (tokens) that the grammar recognizes, organized into nonterminals when it needs recursion.
    This only requires the applicative structure, though it can be inflected by the selective structure, especially for prettier printing.

#.  Next we analyze the grammar to build a way to parse it: an automaton (LL or LR or something).
    Ideally this is staged to happen during compilation, not at runtime.

#.  Then we parse input into a concrete syntax tree ([CST]{t=}), consisting of all of the tokens and all of the tags of what branch of the grammar they fit into.
    If there was selective control flow, it can be used to guide the parsing from the automaton.
    That means it needs to be interleaved with the next step.
    (The alternative is to parse *all* possibilities and only prune them at the end, but that negates the whole efficiency we want from this representation!)

#.  The same applicative – this time with the selective information – also built up a codec, which parses from the [CST]{t=} into the user data types.
    Integers, parsed strings, lists, [AST]{t=}s, and so on.
    You could parse directly to an interpreter if you wanted.
    Thatʼs part of the magic of parser combinators.


It takes a lot of work to implement it, especially with the generic types that I wanted.
The staging is tricky, since it needs to be shuffled through JSON and isnʼt validated to match with the parser.
The parser machine itself is not optimized yet, still reeling from refactors for whitespace among other things.
The selective part is not functional yet, either, though it is threaded through to places it needs to go.


```purescript
arithmetic :: Comber Number
arithmetic = ws *> namedRec "arithmetic" \expr -> choices
  [ number <* ws
  , token '(' *> ws *> expr <* token ')' <* ws
  , ado
      x <- expr
      op <- choices
        [ (+) <$ tokenPrecL '+' zero
        , (-) <$ tokenPrecL '-' zero
        , (*) <$ tokenPrecL '*' one
        , (/) <$ tokenPrecL '/' one
        , Num.pow <$ tokenPrecR '^' (one + one)
        , Num.min <$ tokenPrecL "/\\" (negate one)
        , Num.max <$ tokenPrecL "\\/" (negate one + negate one)
        ] <* ws
      y <- expr
      in op x y
  ]
```

Note the pattern of consuming whitespace up front and after every combinator: this is the typical pattern that makes it easy to handle “significant” whitespace, without any affordances for proper whitespace handling that would make it compositional.
It should be possible to have each combinator declare its whitespace compatibility, and have it get resolved more dynamically.


### Staged Monadic Parsers

[Gist](https://gist.github.com/MonoidMusician/b8ce75f012a4008f7904cde941a7fcba)

:::Note
This is a monadic interface, not selective.
But it stages effects in a way that is intensely relevant.
:::

Completely separate from the above, I wanted to create a data type that represented Parsec-style monadic parser combinators more accurately.

It is staged so at each step, where the possible tokens are gathered first, they each come with a continuation to call, and then it recurses to see what happens at the next token.

:::Details
A monadic parser encoded as an infinite lazy tree, which branches on a token classification `ix`{.haskell}, receives token values of type `tok`{.haskell} along the way, and ultimately returns a result `r`{.haskell} or fails.

The basic idea going on here: at each state (position) in the parse machine we can accept a token from the map (where tokens are indexed by `ix`{.haskell}: the category of token you want to accept, before you receive the actual data via `tok`{.haskell}).

This does not include any backtracking: there is only one possible walk through the `ProtoParseMachine`{.haskell} for a given input, and it is a _linear_ walk, with strictly increasing depth. The real `ParseMachine`{.haskell} below adds backtracking (which requires a bit more complexity than you would expect), `Coyoneda`{.haskell} (to handle maps/binds more efficiently), and fallbacks for when `ix`{.haskell} fails to match the current token: there is no reason for the `Map`{.haskell} to be exhaustive, it just makes it more efficient, theoretically.

```haskell
data ProtoParseMachine ix tok r = ProtoParseMachine
  { acceptP :: Map.Map ix (tok -> ProtoParseMachine ix tok r)
  , finishP :: Maybe r -- only called on EOF in this proto parser, unlike
                       -- below where it is used for stopping early for staging
  } deriving (Functor)
```
:::

This is missing a few ingredients.

One of the key (and somewhat confusing!) parts of Parsec is that parsers can either commit to their parse (after consuming some input) _or_ they can keep allowing backtracking.
We can encode this in the types, too!
Instead of `M r`{.haskell}, we can have `M (M r)`{.haskell}, where the first stage tries it out and the second has committed.
Because it is a monad, you can always squash it, then decide which stage it goes into: fully backtracking or fully committed.

The next part to add is that we want to not have to enumerate tokens, but rather match any token as a fallback.

This necessitates more elaborate `<|>`{.haskell} behavior, a kind of staging of the fallbacks, so specific tokens can come after catchalls.

:::Details

The full parser, with backtracking and a catchall case after the `Map`{.haskell}.

It proceeds by gathering indexed tokens into a `Map ix`{.haskell}, one layer of backtracking at a time, and then matching it on the stream. If it fails, backtracking proceeds at the next `MatchTrack`{.haskell}. Finally, when the stream is exhausted, the `Maybe r`{.haskell} at the end of the tunnel is returned.

Interestingly this statically asserts part of the backtracking behavior of `Parsec`{.haskell}: for a parser with singleton `MatchTrack`{.haskell}s, there is no backtracking since each map only has one behavior for the `ix`{.haskell} that it sees.

I think using the `Coyoneda`{.haskell} trick to factor out the implicit state should allow finite applicative parsers (for context-free grammars) to be finite data structures in memory. Otherwise it is just lazy for absolutely no reason, and repeated usages of the same parser instance would leak memory.

```haskell
data ParseMachine ix tok r = ParseStep
  { accept ::
    -- `MatchTrack` gives backtracking behavior by allowing several `Map`s`
    -- to propose the tokens that should match (without backtracking, the
    -- first result for a token wins, via left-biased `Map.union`)
   !( MatchTrack
      -- We include a catchall, to match any single token
      ( WithCatchall
        -- The `Map`s index what _kind_ of tokens to accept and contain
        -- continuations which receive the particular _value_ of the token
        (Coyoneda (Map.Map ix))
        -- The continuation is staged so that the first stage can include
        -- backtracking to the next `Map` in the `MatchTrack`, while the second
        -- stage commits locally to the parse (notwithstanding `try`s further back)
        (tok -> ParseMachine ix tok (ParseMachine ix tok r))
    ) )
  , finish ::
    -- Finally there is a pure value: either a result for EOF, hopefully, or,
    -- for intermediate steps, a fallback that is used to call the next continuation
    Maybe r
  }
  deriving (Functor)

-- A monoid (with suggestive names) that overlaps elements by one when appending
data MatchTrack m = NoMatch | MatchTrack m (MatchTrack m)

type ParseMatchTrack ix tok r = MatchTrack
  ( WithCatchall
    (Coyoneda (Map.Map ix))
    (tok -> ParseMachine ix tok (ParseMachine ix tok r))
  )

data WithCatchall f a = WithCatchall !(f a) (Maybe a)

data Coyoneda f a = forall b. Coyoneda (b -> a) !(f b)
```
:::

*However*, with all that said.
If you want to stick to the applicative combinators strictly, and adhere to strict discipline about only using implicit fixpoints.
You could implement a grammar printer by identifying sharing at the pointer level, using `StableName`{.haskell}s, like [Parsley](https://hackage.haskell.org/package/parsley) does.
(But any parameterized parser, [e.g.]{t=} in a monadic bind or nontrivial fixpoint, would mean that this would not terminate.)



### Bidirectional Parser–Printers

¡printer–parser–pretty-printers! (that is em-dash, em-dash, hyphen)

In the large, the idea is that parsing will be of the shape `Parser AST`{.purescript} for some [AST]{t=} type, providing some `String -> Maybe AST`{.purescript}.
Pretty printing is the inverse: `AST -> String`{.purescript} (really, it goes to a compositional pretty printer type).
Unless the AST embeds information about the concrete syntax, `String -> String`{.purescript} concrete printing allows for doing interesting transformations on relevant AST nodes while doing more exact printing for untouched nodes (though you may need to alter indentation, for example).

That is one big reason to fuse printing and parsing: to capture information about the exact formatting in the parser that is otherwise discarded in the [AST]{t=}.
This is nice for, say, automated refactoring tools.
Because it is done in the parser itself, on the raw CST, it sees all the information it wants without cluttering the AST.


It requires a lot of abstraction to handle whitespace and precedence correctly.
Those concepts donʼt naïvely correspond between printing and parsing.
You canʼt abstract whitespace without doing a lot of work to treat it specifically in the parser.
I havenʼt _fully_ worked things out yet, but I am hopeful.

Of course, there is nothing guaranteeing that printing and parsing are inverses.
Especially when `<|>`{.purescript} is involved, which lets two parsers run but chooses the pretty printer from the left.


More concretely, a printer–parser–pretty-printer `PrinterParser i o`{.purescript} stores a pretty-printer of the rough shape `i -> Doc`{.purescript} (but made more compositional) and a parser of the shape `Parser { cst :: Doc, ast :: Lazy o }`{.purescript}.
The CST and AST parser need to be one, otherwise you donʼt necessarily have a unified grammar and would have to pay the cost of creating the parsing automaton twice.
But you can choose whether you lay it out into a CST or grab the input type – or both!
After all, if it has been validated to parse, why shouldnʼt you be able to grab both out of it?

The details of making it compositional get tiresome.

```purescript
-- A pretty-printer, from an abstract type to a document layout
type PrinterMade i =
  -- Parenthesize the output for precedence, or do whatever is necessary
  -- (passed as reader context down from the parent)
  (O.Doc Ann -> O.Doc Ann) ->
  -- The abstract input type, to invent syntax for
  i ->
  -- The precedence from the parent, if necessary
  Maybe (Tuple Rational Associativity) ->
  -- Formatting options requested by the user, e.g. “prefer unicode”
  Opts ->
  -- The pretty printer document, with whitespace handling and extra annotations
  -- (when two WSDocs are juxtaposed, they may insert whitespace)
  WSDoc Ann
```

The printer needs a parenthesizer, an idea of precedence for the current context.
Some formatting options set by the user, [e.g.]{t=} “prefer unicode”.
The documents are wrapped in additional whitespace-handling structure beyond what the pretty printer provides itself.
Then the pretty printer also has space for annotations, for colorful ANSI output, HTML classes – lots of interesting things are possible.

The parser type is just about as complicated.

```purescript
type ParserMade o = WithBoundaryF InnerParser o
newtype InnerParser o = InnerParser (Compose Comber Parsed o)
newtype Parsed o = Parsed
  { usedOpts :: Lazy Opts
  , cst :: Opts -> O.Doc Ann
  , ast :: Lazy o
  }
```

There is whitespace handling in `WithBoundaryF`{.purescript}: again, when two parsers are juxtaposed, a whitespace token may need to be inserted in between them.
The `Comber`{.purescript} functor is the raw parser combinator type, with [LR(1)]{t=} support and specialized whitespace handling and other good stuff.^[The downside is that it does not allow plugging in your own tokenizer.]
Finally, as said above, the parser returns an AST and CST together.
It states which options it actually encountered in the source, provides a printer for the CST with the ability to request different options, and a lazy AST result `o`{.purescript}.


The basics of how to compose these types is surprisingly not too difficult.
But a lot more research needs to be done to work out the fine examples, especially mixing concrete printing and abstract.


Indentation is going to be super complicated and I have not even attempted that yet.



## Bidirectional Codecs

Switching gears a bit, letʼs think about codecs for data types like JSON (or even CBOR?).

First we make a codec type that handles single values.

```purescript
-- | A bidirectional codec with a single target (e.g. `JSON`), meaning that it
-- | supports disjunction but not conjunction.
data OneCodec ctx err reg s t i o = OneCodec
  (Star (RegisterM reg) i t)
  (Star (VI ctx err) (Registered reg s) o)

-- | Applicative validation, with extra context for errors.
data VI i e a = VE (i -> ErrorTree e) | VA a
```

This is a souped-up version of just a pair of basic decoding functions.

```purescript
{ encode :: i -> t
, decode :: s -> Either err o
}
```

(note: `Star m i o`{.purescript} is also called `Kleisli m i o`{.purescript}: `i -> m o`{.purescript}.)

Because there is only a singular focus, this type supports disjunction, but not conjunction.

It can support decoding sum types, by seeing which is the first decoder that succeeds, and choosing the right encoder on the other end.
This uses the `\!/`{.purescript} combinator: `disjuxt2 :: forall @p u v x y. Disjuxt p => p u x -> p v y -> p (u \/ v) (x \/ y)`{.purescript}.

There are multiple ways to encode sum types in JSON, so here are two:

```purescript
-- { type: "Left", data: L } | { type: "Right", data: R }
eitherO :: forall reg i1 o1 i2 o2. Ord reg => JRCodec' reg i1 o1 -> JRCodec' reg i2 o2 -> JRCodec' reg (i1 \/ i2) (o1 \/ o2)
-- ["L", L] | ["R", R]
eitherA :: forall reg i1 o1 i2 o2. Ord reg => JRCodec' reg i1 o1 -> JRCodec' reg i2 o2 -> JRCodec' reg (i1 \/ i2) (o1 \/ o2)
```

On the other hand, the `<|>`{.purescript} combinator also tries multiple decoders, but it always takes the left encoder, since they come from the same type.
This encodes a preference – particularly notable for pretty printers.


### Array Codecs

To support conjunction – to parse multiple things at a time, like a text or binary parser does – we need to operate over compound datatypes, arrays, maps, objects.

An array codec supports multiple targets.
Specifically it encodes to multiple targets (which will get wrapped in a JSON array) and decodes multiple targets, based on their length and the individual items that get slotted in.

Conjunction takes two array codecs and juxtaposes them, putting the items of one before the items of the other.
That is, an array codec item does not live at a fixed position in the array: it shifts around based on context.
This is the distinction between array-like codecs and keyed codecs.

If we did not support disjunction at this layer of codecs, the decoder could be a simple pair `Tuple Int (Array s -> Either err o)`{.purescript} of the expected length combined with the bit to decode the segment it was given: that is, the slice `Array s`{.purescript} is always expected to be of the length specified by the `Int`{.purescript}.

Supporting disjunction means we need to handle multiple lengths that may show up, resulting in a `Map Int (Array s -> Either err o)`{.purescript} to keep track of all of them.
The codec works by distributing conjunction over disjunction, which sounds inefficient, but it actually just shifts the cost from decoding time to codec creation time in a nice manner.

This is basically what pattern matching against an array is.
Decodecs are pattern matchers!

```purescript
-- | A bidirectional codec with multiple outputs, meaning that it supports
-- | conjunction in addition to disjunction.
data ArrCodec ctx err reg s t i o = ArrCodec
  (Star (RegisterM reg) i (RoseArray t))
  (ArrDecodec ctx err reg s o)
-- | A decodec with multiple outputs, meaning that it supports disjunction
-- | and conjunction.
newtype ArrDecodec ctx err reg s o = ArrDecodec
  (Map Int {- length -} (Star (VI ctx err) (Registered reg (Tuple Int {- offset -} (Array s))) o))

gitem :: forall ctx' ctx err reg s t i o.
  (Int -> ctx -> ctx') ->
  OneCodec ctx' err reg s t i o ->
  ArrCodec ctx err reg s t i o
gitem contextualize (OneCodec f g) = ArrCodec (map pure f) $ ArrDecodec $
  Map.singleton 1 $ mkStar case _ of
    Registered reg (Tuple idx items)
      | Just s <- items Array.!! idx ->
        rectx (contextualize idx) $ unStar g (Registered reg s)
    _ -> empty

-- | A codec for a single item.
item :: forall reg i o. JRCodec' reg i o -> JRACodec' reg i o
item = gitem JIndex
```

### Keyed Codecs

Keyed codecs operate on specific fields.
They support conjunction by taking the union of fields, which means that field can be decoded more than once!, and if it is encoded more than once, the leftmost one wins, like for `<|>`{.purescript}.

The type is simpler than for.

```purescript
-- | A bidirectional codec with keyed outputs, meaning that it supports
-- | conjunction in addition to disjunction.
newtype MapCodec k ctx err reg s t i o =
  MapCodec (OneCodec ctx err reg (Map k s) (Map k t) i o)

gfield :: forall k ctx' ctx err reg s t i o. Ord k =>
  (k -> ctx -> ctx') ->
  k -> OneCodec ctx' err reg s t i o ->
  MapCodec k ctx err reg s t i o
gfield contextualize k (OneCodec f g) =
  MapCodec $ OneCodec (map (Map.singleton k) f) $
    mkStar case _ of
      Registered reg jsonFields
        | Just s <- Map.lookup k jsonFields ->
          rectx (contextualize k) $ unStar g (Registered reg s)
      _ -> empty

-- | A single field, with its static key and a codec for its value.
field :: forall reg i o. String -> JRCodec' reg i o -> JROCodec' reg i o
field = gfield JField
```


### Other Shapes

You can envision other shapes of codecs.

You could envision a keyed codec shaped more like the array codecs, `Map (Set k) `{.purescript}, that handles specific sets of keys that could occur.
With or without the ability to ignore excess keys – although that gets less compositional.
More difficult to version such encoding schemes for backwards compatibility.

This open-endedness could be useful for arrays, too.
Right now this basic array codec type does not support proper parsing and pretty printing that the previous section covered.

Imagine fusing a bidirectional parser–printer for JSON with bidirectional codecs: you could make codecs that print directly to JSONʼs string representation, and parse directly when possible, without worrying about bugs that would come with doing it manually.
Especially with the right compiler optimizations, it would be so cool.

## Compilers

All of this stuff about static analysis is great for compilers too.
(Whatever a compiler may mean to you: primarily something that takes in code-shaped data in some form and outputs other code or runs it as an interpreter.)

Essentially the static analysis of arrows brings together compile-time optimizations into runtime, with staging.

## Build Systems

The ability to dry run something is an important property of build systems.
Conditional execution also wants to skip recompiling outputs when it can determine they are up to date already, based on file modification times and/or hashes.^[The approach that the PureScript compiler takes, which is a good one, is to check for exact equality of modification times, and then compare file hashes. Otherwise you run into issues with source tooling that may revert/backdate modification times.]

This makes build systems a perfect target for the control flow weʼve been talking about.
You want to be able to know – or at least stage – what effects are going to be run; what files are going to be read, created; what commands are going to be executed.


```purescript
traverseStaged ::
  forall t f i o.
    TraverseFlow f =>
    Traversable t =>
  f (t i) -> (i -> f o) -> t o
traverseStaged gather run = do
  twoStage do
    traverseF gather (pure run)

compileFiles inputDirectory outputDirectory = do
  void $ traverseStaged
    (GatheringStage $ searchDirectory inputDirectory)
    \source -> twoStage $ PlanStage ado
      let target = makeTarget inputDirectory outputDirectory source
      -- This records that it will read `source` and write `target`
      -- before running any effects. It can skip running if it
      -- determines that source is newer than target, for example,
      -- or if it is in dry run mode.
      transform <- willTransformFile source target IfNewer
      in transform
        -- If it does run, it will provide the contents,
        -- then take the result and write it out
        \contents -> do
          result <- process contents
          pure result
```


## Haxl: Batched Data Fetching

Honestly I think my previous post said most of what I wanted to say about Haxl?
In a sense, it ends up being the most straightforward conceptually of the applications.

The implementation is complicated by concurrency, memoization, error handling – the usual.


