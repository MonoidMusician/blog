---
title: Parsers Upon Parsers
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2025/08/–
---

It began as a vague idea to unify binary parsers and text parsers.
Textual languages for humans are often _context-free_ languages, which is touted as an easy/tractable category of grammars for computer parsing.
So why are binary formats often _more_ convenient for computers to parse, despite being wildly context-sensitive??
Is there any reliable way to describe their grammars too?
*Bidirectionally?!*

I thought that the answer must lie in [selective applicative functors](selective-applicatives-theoretical-basis.html), augmented with a [few new types of control flow](selective-applicatives-theory.html#nuanced-control-flow).
I think this was the right approach.

This selective applicative structure introduces a controlled flow of information, not only within the grammar, but all the way up to the user level and back.
Within the static paths of a typical context-free grammar and [LR(1)]{t=} or GLR/GLL/Earley parsing tables, a selective applicative parser is allowed to make choices about pruning possibilities given all the tokens it has seen so far.

Restricting the interface of the parser, from full monadic parsing to a selective applicative combinators, allows knowing the grammar statically as the combinators build up the parser.^[A monadic parser combinator type precludes any such static analysis: every rule is dynamic.]
This allows compiling the grammar to a parse table with any common method ([LR(1)]{t=}, GLR, Earley, and so on) or even just parsing it on the fly.
Running it against an input will then build a [CST]{t=} of the exact tokens matched, with each branch denoting a particular rule of the grammar that matched.
From this [CST]{t=}, the codecs built by the functor are used to parse it into user data types – and in theory this can be fused if the CST is not demanded.^[The CST cannot be reconstructed from the user data.]

Having the [CST]{t=} available, along with selective application, enables **recursive parsing**: parse with a simpler grammar on first pass, and then apply a second parser on that result to refine the match to a new data structure or reject it with a dedicated error message.
This helps keep grammars compositional, efficient; it reduces noise in the grammar (the “string” combinator is separated from the details of escape patterns), it helps you provide better error messages, and it models how some languages actually parse things.
For staged parsers, it needs to be integrated so that compiling the grammar can happen ahead of time, incorporating the new entrypoint into the overall parser.^[Why donʼt more parsing frameworks incorporate multiple entrypoints?? It is really necessary for highlighting code snippets.]

:::{.Example box-name="Examples"}
For example, [CSS]{t=} specifies an [`<any-value>` rule](https://www.w3.org/TR/css-syntax-3/#any-value) to ensure forwards compatibility and error recovery, and my [tmTTmt](tmttmt.html) likewise constrains the allowable syntax for [values, types, and comments](tmttmt-syntax.html) so parsers do not have to implement the full specification at once.

Even Haskell and PureScript parse user operators generically first, with fixed associativity and no precedence, before rewriting the tree using a custom parser to incorporate their resolved fixities^[precedence and associativity, via the `infix`{.purescript} declaration].
:::

In addition, to make defining grammars much simpler and solve a common source of mistakes, I added a technique for handling whitespace and assertions without introducing ambiguity into the LR(1) grammar.
This is tricky because whitespace exists *between* other tokens, in a way that means that sequential whitespace assertions^[whether they occur literally next to each other in the grammar, or through other, potentially nonlocal interactions] become aggregated together as one whitespace match.
This needs to be integrated with the grammar generator and the tokenizer, and it also needs to be integrated with the CST and codecs.
This is because it does not act like terminals, which are the checkpoints that shuffle data from the tokenizer to the user level.
Whitespace is in fact not available directly to the data codecs, due to the ambiguity of where it come from – you do not want to double-count it – but it still must be present in the assembled [CST]{t=} of composite elements somehow.
When this is done, though, it resolves a major source of bugs and ambiguity in grammars, allowing for simple specifications that do the right thing.

In addition, all of this is overly generic: though we provide a parser specialized to strings and regexes (and maybe another parser specialzed to bit and bytestrings), it can be applied to any data types, including pre-tokenized input.
However, pre-tokenized input loses out on one last capability lurking in the parsing framework: tokens can be discriminated based on the specific tokens allowed by the grammar, for the current state in the parsing automaton.
(I guess this is sometimes called a pseudo-scanner?)

*Finally*, the next step in the journey is making better bidirectional printer–parsers.
I believe the difficulties of whitespace on the parsing side are one major obstacle to bidirectional parsers, and that is solved.
The selective applicative structure does not pose any particular challenges for bidirectionality.^[There is no magic bullet for monadic parsers, I believe. If you really need a monadic parser, you are going to have to put your own work into ensuring it can be bidirectional. The combinators just donʼt make sense if there isnʼt at least a static grammar, like with selective applicatives.]
Integrating into a parser combinator framework allows capturing all of the data from the CST when necessary, but not polluting the AST with it.

Let me be clear.
Itʼs not easy.
This is an ambitious project to make the best parser theory imaginable.
Itʼs a highly elaborate and interleaved process, staged from a static step to assemble the grammar and parse table, and the dynamic interleaved steps of handling the input and transforming it into a parse result.
But it is highly flexible and has the possibility of being efficient in theory, especially when combined with the right compilers (… which do not exist yet).

<!--
[TODO]{t=}:

- Finish refactoring whitespace
- Add GLR(1). LALR(1)? [IELR(1)](https://malloy.people.clemson.edu/publications/papers/sac08/paper.pdf)? [XLR](https://arxiv.org/abs/2209.08383)?
- Selective recursion for length-prefixed data?
- Compile to WASM???
-->

## Features

To recap:

- Parsers expressed in terms of combinators with staging, not a dedicated syntax
  - Using combinators ensures that features like highlighting and editor integration work well
  - It also allows for more abstraction: if you want to have extra information from a `ReaderT`{.purescript} while building up the parser, for example
  - And if you want to make parser dynamically at runtime, without staging, you can do that too
- No monadic combinators, just selective applicative combinators plus some extra control flow in the spirit of them.
  Arrows shaped like `f (i -> o)`{.purescript} not `i -> f o`{.purescript}.
- Static grammar, built up by the combinators.
  - A “pretty” version, preserving the tree structure of the combinators
  - An expanded version, suitable for LR parse table generation, for example^[Possibly there is some way to take advantage of the sharing/deduplication of the pretty version? But I have not seen that done before, it may be open research…]
- Multiple entry points and staged parsing
  - Multiple entry points are easy to generate, extra `<entrypoint> TOP` starting points mean you basically just end up adding table entries with a few more rules having `TOP` in their lookahead sets
- Ability to plug in any parsing framework that generates a CST
  - Any framework is possible, as long as it can return the CST annotated with the matched grammar rules
  - Could even use monadic parsing frameworks! at the expense of losing out on the nice LR(1) features
    - But you could design a way to factor our some forms of left recursion at least …
  - The CST has cases for terminals, whitespace, and productions (particular rules that matched, with their children):

    ```purescript
    data ICST air rule tok
      = ILeaf tok
      | IAir air
      | IBranch rule (Array (ICST air rule tok))
    ```
  - To reconstruct the input, concatenate all of the leaf nodes

- Currently: LR(1) table generation + parsing
  - … with interleaved lexing, so that only the expected tokens are parsed^[when a lexing error happens, all tokens will be parsed]
  - … and selective pruning^[[WIP]{t=} to incorporate the selective applicative structure to guide the parsing and prune ambiguity]
- Codecs to interpret the CST into user types ([i.e.]{t=} the functor part of it)
  - These codecs mirror the grammar structure (they are responsible for recognizing it, after all!), but they are distinct, because the same grammar structures may come to be associated with different codecs at different points in the parse.
  - The codecs form a lazy tree which is unfolded as tokens are parsed and fit into constructs
- Compositional whitespace: `{ allowed_newline :: Conj Boolean, allowed_space :: Conj Boolean, required :: Disj Boolean }`{.purescript}
  - Whitespace needs to be handled specially by the parser, but the benefit is that it is now compositional and does not require carefully designed grammars
    - Abstracted whitespace really helps with bidirectionality
    - Rendering whitespace has the options `NoWS | Space | SoftBreak | SpaceBreak | Break`{.purescript}, and they are combined with the parsing whitespace
  - The combinators also need to be careful to not expose the matched whitespace token except when it is guarded by tokens
  - The easy cases, collapsing whitespace between terminals in the grammar, are taken care of ahead of time
  - The difficult cases are when a nonterminal may start or end with various whitespace assertions, which then appear in the grammar in unrelated places
  - The tokenizer parses the maximum allowed whitespace for any step, and then rules are pruned based on whether they match what was parsed.
- Precedence, in the traditional LR style with some upgrades
  - Left associativity corresponds to choosing shifts, right to reductions^[[TODO]{t=} double check]
  - Precedence is modeled by rational numbers, so new levels can always be inserted in between any existing levels
- [WIP]{t=} Bidirectional printer–parser–pretty-printers, allowing compositional description of languages with both a parser and pretty-printer (between strings and user types), as well as verbatim re-printing when used in that mode.


### Combinators

Let `P`{.purescript} stand for a parser type.

Specific combinators that this parsing framework has:

- Primitive:
  - `pure :: x -> P x`{.purescript}, `lift2 :: (x -> y -> z) -> P x -> P y -> P z`{.purescript} via `Applicative`{.purescript}
  - `empty`{.purescript}, `<|>`{.purescript} via `Plus`{.purescript}
  - `filterMap :: (x -> Maybe y) -> P x -> P y`{.purescript} via `Witherable`, for selective rejection
  - `namedRec :: String -> (P x -> P x) -> P x`{.purescript} constructs a nonterminal, thus allowing recursion
  - `branch :: P (Either x y) -> P (x -> r) -> P (y -> r) -> P r`{.purescript} and so on, from selective applicatives
  - deterministic repetition `Traversable t => P (t x) -> P (x -> y) -> P (t y)`{.purescript}, nuanced control flow
  - unbounded repetition `P x -> P (x -> Either y x) -> P y`{.purescript}, nuanced control flow
  - `tokenPrecA :: tok -> Associativity -> Rational -> P tok`{.purescript} and `rulePrec :: Rational -> P x -> P x`{.purescript} for precedence
  - `withTokensSourceOf :: P x -> P (Array tok /\ x)`{.purescript}
- Derived:
  - Many combinators for repetition! Note that they require names for their recursive portions
    - `many :: String -> P t -> P (Array t)`{.purescript}
    - `many1 :: String -> P t -> P (NonEmptyArray t)`{.purescript}
    - `manySepBy :: String -> P h -> P t -> P (Array t)`{.purescript}
    - `manyBaseSepBy :: String -> P nil -> P h -> P t -> P (Array t)`{.purescript}
    - `many1SepBy :: String -> P h -> P t -> P (NonEmptyArray t)`{.purescript}

And for the bidirectional printer–parser–pretty-printers, call them `PP`{.purescript}:

- `conjuxt0 :: PP Unit Unit`{.purescript} and `conjuxt2 = (/!\) :: PP u x -> PP v y -> PP (u /\ v) (x /\ y)`{.purescript}
- `disjuxt0 :: PP Void Void`{.purescript} and `disjuxt2 = (\!/) :: PP u x -> PP v y -> PP (u \/ v) (x \/ y)`{.purescript}
- `namedRec :: String -> (PP i o -> PP i o) -> PP i o`{.purescript}
- `many :: String -> PP i o -> PP (Array i) (Array o)`{.purescript} and the other “nondeterministic” repetition operators (`some`{.purescript}, [etc.]{t=})
- deterministic branching, [e.g.]{t=} for tagged data in binary formats

  ```purescript
  branchP ::
    p (i \/ j) (w \/ z) ->
    p u x ->
    p v y ->
    p
      ((i /\ u) \/ (j /\ v))
      ((w /\ x) \/ (z /\ y))
  ```
- deterministic iteration, for length-prefixed data in binary formats
  `lengthPrefixed :: String -> PP Int Int -> PP i o -> PP (Array i) (Array o)`{.purescript} (nuanced control flow)

## Codecs

A “data codec” for a parser is essentially a Functor `CST -> Maybe t`{.purescript}, for a functor parameter type `t`{.purescript} which ultimately becomes the result of the parser combinator.
Itʼs the part that maps the literal items that the parser matches to the abstract data result that a parser combinator library offers.

The use of `Maybe`{.purescript} means that codec conversion may fail: not only is this used to build the selective application / selective rejection in the parser, but also it is just a reflection of the impreciseness of the type: `CST`{.purescript} is too vague to guarantee a match.
A properly functioning parser should not fail the parse in this way, but the types are not strong enough to guarantee this.

## Whitespace

Whitespace is mainly found in between other elements, and it is very slippery: whitespace merges together like blobs of liquid.
This poses some problems for the API of parser combinators.

You are certainly allowed to use whitespace as an assertion: must have a space, cannot have a newline, [etc.]{t=}.
This is the easiest usage, but it isnʼt always sufficient.^[In fact, not even sufficient for constructing an exact CST.]

You can look at the exact value of the whitespace if you guarantee there is some token on either side: the adjacent parser combinators cannot match empty, otherwise the parser could coalesce two whitespace assertions into one, and the data of the exact whitespace would have to be duplicated between them, or arbitrarily assigned to one or the other: not great.

There are exactly two other situations that whitespace can occur: at the top-level, either as the *only* element (*no* tokens), or on either side of the top-level non-empty parser combinator.

This breaks a small bit of the organization of the parser framework: the top-level isnʼt supposed to be special like that, and you ideally would not have to care about whether a parser can match empty strings or not.
Sequences of terminals and non-terminals are supposed to exist anywhere, equally, and be concatenable and alternatable.

Basically it is a leaky abstraction.
But matching empty is a fundamental property of parsers that you cannot run away from: it pops up time and time again.
*And!* it is in service of another abstraction.

It is nice for ease of writing parsers, being freer to build abstractions.
(The most successful convention for avoiding ambiguity in a parser is to have each nonempty parser consume trailing whitespace, but this is tedious and really should be unnecessary.)

More importantly, factoring out whitespace is crucially important for bidirectional printer–parsers, so that it can shared between printer and parser.
I am convinced that truly compositional bidirectional printer–parsers are not possible without solving whitespace like this.

Eventually it would be nice to “solve” indentation-sensitivity in this way too, but I have not though about that much yet. One step at a time.

### Datatypes for Whitespace

Printing, parsing, and shared.

On the printing side, the datatype is rather simple.
There is an option for a force line break, and options for printing a space or not, with the possibility of the line wrapping as needed.
(`SpaceBreak`{.purescript} means a space or line wrap, while `SoftBreak`{.purescript} prints nothing if the line does not wrap there.)

```purescript
data RenderWS = NoWS | Space | SoftBreak | SpaceBreak | Break
```

On the parsing side, spaces and/or newlines can be allowed, and some whitespace may be required – or not.

```purescript
newtype ParseWS = ParseWS
  { allowed_newline :: Conj Boolean
  , allowed_space :: Conj Boolean
  , required :: Disj Boolean
  }
```

Putting them together, we end up with a lot of possibilities.

```purescript
data WS
  = Failed
  | Forbidden
  | AllowedSpace
  | AllowedNewline
  | Allowed
  | SpacePreferred Boolean
  | SoftBreakPreferred
  | SpaceBreakPreferred
  | BreakPreferred Boolean
  | RequireSpace Boolean
  | RequireSpaceBreak
  | RequireNewline Boolean
```


Whitespace can be:

1. failed (if you try to concatenate forbidden with required, for example … it's cleanest to just include it in the semilattice)
1. forbidden
1. space allowed, no newlines
1. newline allowed, not just a space
1. whitespace allowed
1. space preferred, no newlines
1. space preferred, newlines allowed
1. soft break preferred (print nothing or newline, depending on layout)
1. space break preferred
1. prefer newline, spaces allowed
1. … I don't think prefer newline, spaces not allowed makes sense?? idk
1. require space, don't allow newline
1. require space, allow newline
1. required space break
1. print newline, require space or newline
1. require newline


The goal is that these combine as semilattices, before they get to the printer or parser.
This allows several things:

- It allows modularly combining instructions to come up with the right behavior.
  Ideally there are just a couple common combinators for printing and parsing.
- It coalesces the print instructions before they get to the pretty-printer (which would not coalesce spaces).
- It allows the parser to treat whitespace as assertions, influencing how the lexer operates and allowing it to show up in the CST in predictable ways.
  For example, if the same token shows up twice on different branches, one with whitespace required and one with whitespace disallowed, then the lexer will look for optional whitespace, and whether it sees whitespace will determine which branch is taken.


### Structures for Whitespace

The pretty-printing side is easier to explain.

The data structure is either whitespace on its own or a document wrapped in whitespace.

```purescript
data WSDoc ann
  = JustWS RenderWS
  | WSDoc
    { before :: RenderWS
    , doc :: Dodo.Doc ann
    , after :: RenderWS
    }
```

Appending these operates as you would expect: it inherits the beginning and end whitespace chunks, and incorporates the combined whitespace squished in the middle.

```purescript
instance semigroupWSDoc :: Semigroup (WSDoc ann) where
  append (JustWS w1) (JustWS w2) = JustWS (w1 <> w2)
  append (JustWS w1) (WSDoc d2) = WSDoc d2 { before = w1 <> d2.before }
  append (WSDoc d1) (JustWS w2) = WSDoc d1 { after = d1.after <> w2 }
  append (WSDoc d1) (WSDoc d2) = WSDoc
    { before: d1.before
    , doc: d1.doc <> toDoc (d1.after <> d2.before) <> d2.doc
    , after: d2.after
    }
```

The parser side is more difficult, due to branching control flow.
Most of the difficulty is just if you want the parser to look pretty …



### Combinators for Whitespace



