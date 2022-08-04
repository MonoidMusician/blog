---
title: Uses of Grammars
subtitle: "[Interactive Parser Explanations](parser.html)"
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

Thereʼs a couple distinct ways to use grammars, and this is why some of the explanations and terminology are confusing.

The differences manifest on both sides:

- On the grammar side/as a user of parsing tools, the kinds of grammars you write for efficient recognition are not the kinds of grammars you would write for unambiguous parsing.
- On the algorithm side/as a designer of parsing tools, parsing algorithms often are more complex than recognition algorithms, sometimes in surprising ways (c.f. [Earley parsers](https://en.wikipedia.org/wiki/Earley_parser#Constructing_the_parse_forest)).

## List of Uses

Weʼll mainly be talking about formal uses of grammars that require algorithms.
Using grammars as a kind of informal communication, like in semiformal language/data specifications, are very common but not discussed here.

The two main formal uses are recognition and parsing, as mentioned above, but there are some more ways to actually make use of grammars.

In rough order of difficulty the distinct uses of grammars are:

1. Generators: Nondeterministically generate strings in the grammar by following the rules as state transitions.

    This task is really simple because thereʼs basically no algorithmic difficulty, just following some rules until you reach an end state: a concrete string that belongs to the grammar.
2. Recognition: Recognize which strings belong to the grammar and which do not.

    Simply put, it is to construct a function `String -> Boolean` for a grammar.
3. Syntax highlighting: Uhh?

    This one is kind of cursed because it is not very well-defined.
    Obviously it requires a bit more than mere recognition, since you want more substantial data back from it, but
    what do you actually need out of syntax highlighting?
    And more importantly: how do you deal with errors and partial inputs?
    Weʼll get into that later …
4. Parsing: Find an unambiguous parse tree for all inputs that belong to the grammar, and errors for inputs that do not belong to the grammar.

    Weʼre talking about parsing it to the level that you can run a compiler or interpreter.
    It would look like a function `String -> Either Error AST`.
    This is the most difficult, since it incorporates recognition but needs to be unambiguous and actually produce a parse tree.

## Generating from a grammar

### The generating game

The rules of the game are simple:

1. Start with the nonterminal you want to generate an input for (usually the top nonterminal in the grammar).
2. Replace any nonterminal you see with any of its production rules.
3. Repeat step 2 as necessary.
4. You are done when there are only terminals left.
  That is, youʼve produced a concrete string that belongs to the grammar.

Pretty simple, no?
However, thereʼs one catch, as I learned when implementing it!

### Determining what is producible.

You can think of this as the question: is the game winnable?

There are two modes of failure:


01. No rules: A nonterminal has no production rules associated with it.

    In the game, this means no action could be taken to advance a nonterminal, so it would stay and never be converted to any sequence of terminals.

    ::: Note
    In my example framework this isnʼt an issue, since the list of nonterminals is collected from the production rules.
    So nothing is parsed as a nonterminal that doesnʼt have a production rule.
    Or, restated without the double negative:
    Everything that is parsed as a nonterminal has a production rule.
    :::

02. Cyclic dependency: Every production rule for a nonterminal depends on first producing that nonterminal.

    In the game this means you would always be forced into an infinite loop, being able to apply a rule to the problematic nonterminal but always ending up back at it after one or more steps.

    ::: Example
    For example, if a nonterminal has _only a single rule_ [EXPR]{.non-terminal} [:]{.meta} [(]{.terminal}[EXPR]{.non-terminal}[)]{.terminal}, then [EXPR]{.non-terminal} has a cyclic dependency on itself.
    It must produce itself before it can produce itself!
    Therefore it is stuck and can never be produced.

    Similarly, there can be more complex cyclic dependencies through multiple nonterminals with multiple alternative rules.
    As long as _all_ rules for a nonterminal _must_ depend on the same nonterminal, there is a cyclic dependency.
    :::

It gets a little tricky thinking of it that way, but we can write a simple algorithm to compute which rules are produceable.
Note that this PureScript implementation keeps track of all producible rules by giving an example input for them.

```haskell
type Produced nt r tok =
  { production :: GrammarRule nt r tok
  , produced :: Array tok
  }

producible
  :: forall nt r tok
   . Eq nt
  => Eq r
  => Eq tok
  => Grammar nt r tok
  -> Array (Produced nt r tok)
producible (MkGrammar initialRules) = produceAll initiallyProduced
  where


```

1.  Seed: Start by saying that all nonterminals with a production rule that _only consists of terminals_ is producible.

    ::: {.Key_Idea box-name="Key Point"}
    If there are no production rules that are all terminals, then nothing in the grammar can be produced!
    :::

    ```haskell
    initiallyProduced :: Array (Produced nt r tok)
    initiallyProduced = initialRules # filterMap \rule ->
      rule.rule # traverse unTerminal # map \prod ->
        { production: rule
        , produced: prod
        }

    unTerminal :: forall nt tok. Part nt tok -> Maybe tok
    unTerminal (Terminal t) = Just t
    unTerminal _ = Nothing
    ```
2. Closure: For all producible production rules, [i.e.]{.foreign lang=la} where all of its nonterminals have already been marked as producible, mark its nonterminal as producible as well.

    ```haskell
    produceOne :: Array (Produced nt r tok) -> Part nt tok -> Maybe (Array tok)
    produceOne produced (NonTerminal nt) =
      Array.find (_.production >>> _.pName >>> eq nt) produced <#> _.produced
    produceOne _ (Terminal tok) = pure [ tok ]

    produceMore :: Array (Produced nt r tok) -> Array (Produced nt r tok)
    produceMore produced =
      let
        rejected = initialRules `Array.difference` map _.production produced
        more = rejected # filterMap \rule ->
          rule.rule # traverse (produceOne produced) # map \prod ->
            { production: rule
            , produced: join prod
            }
      in
        produced <> more
    ```
3. Fixpoint: Stop when the set of producible nonterminals has not grown from the last iteration.

    ```haskell
    produceAll rules =
      let
        rules' = produceMore rules
      in
        if rules' == rules then rules else produceAll rules'
    ```

::: {.Bonus box-name="Aside"}
I realized when implementing this algorithm that in fact the seed can literally be the empty set.
The logic for `initiallyProduced`{.haskell} is just the logic for `produceMore []`{.haskell}!
:::

This is the only algorithmic difficulty, however.
With that out of the way, all that is left is to determine how you want to represent nondeterminism.

### Nondeterminism

I want to take this opportunity to address what “nondeterminism” means, since it is notoriously tricky to pin down and often confusing.
Might as well talk about it while we have a nice context to discuss it.
Feel free to skip this section if you want.

Is “nondeterminism” a buzzword?
Not really …

You can make an arbitrary choice (random or pseudorandom or otherwise), or you can keep a (potentially infinite) list of all possible choices that you could have made.
(Of course you can later pick an arbitrary item from the list.)

### Alternate rules of the game

Hereʼs another way to phrase the game, in the spirit of what I actually implemented:

## Recognizing strings in the grammar

## Syntax highlighting

## Unambiguous parsing
