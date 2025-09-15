---
title: Parsers Upon Parsers
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2025/08/–
---

It began as a vague idea to unify parsers and parsers: the binary formats that are convenient for computers to parse, but not captured by context-free parsers, the other “easy” category of grammars but intended for humans more than machines nonetheless.

Selective applicative structure introduces a controlled flow of information, not only within the grammar, but up to the user level and back.
Within the static paths of a typical context-free grammar and LR(1) or GLR parsing tables, a selective applicative parser is allowed to make choices about pruning possibilities with all tokens so far.

Restricting the capability of the functor for the parser, from full monadic parsing to a selective applicative functor, allows knowing the grammar statically from traversing the functor structure.
This is then used by any common mechanism (LR(1), GLR, and so on) to build a [CST]{t=} of the exact tokens matched.
From this CST, the codecs built by the functor are used to parse it into user data types – and this can be fused if the CST is not demanded.^[The CST cannot be reconstructed from the user data.]

Having the CST available, along with selective application, enables **recursive parsing**: parse with a simpler grammar on first pass, and then apply a second parser on that result to refine the match to a new data structure or reject it.^[]

In addition, to make defining grammars much simpler and solve a common source of mistakes, a technique for handling whitespace and assertions without introducing ambiguity into the LR(1) grammar was added.
This is tricky because whitespace exists *between* other tokens, in a way that means that sequential whitespace assertions^[whether they occur literally next to each other in the grammar, or through other, potentially nonlocal interactions] become aggregated together as one whitespace match.
Thus it is not available directly to the data codecs due to the ambiguity, but still must be present in the assembled [CST]{t=} of composite elements – somehow.

In addition, all of this is overly generic: though we provide a parser specialized to strings and regexes (and maybe another parser specialzed to bit and bytestrings), it can be applied to any data types, including pre-tokenized input.
However, pre-tokenized input loses out on one last capability lurking in the parsing framework: tokens can be discriminated based on the tokens possible at that position in the input.

Itʼs not easy. Itʼs a highly elaborate and interleaved process, staged from a static step to assemble the grammar and parse table, and the dynamic interleaved steps of handling the input and transforming it into a parse result.
But it is highly flexible and has the possibility of being efficient.

[TODO]{t=}:

- Finish refactoring whitespace
- Add GLR(1). LALR(1)? [IELR(1)](https://malloy.people.clemson.edu/publications/papers/sac08/paper.pdf)?
- Selective recursion for length-prefixed data?
- Compile to WASM???

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

Mainly just for ease of writing parsers, being freer to build abstractions.
It is also important to do this in service of bidirectional printer–parsers, where whitespace needs to be factored out before it can be shared between printer and parser.

### Data Structures for Whitespace

Printing, parsing, and shared.

### Combinators for Whitespace



