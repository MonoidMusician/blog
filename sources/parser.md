---
title: Interactive Parser Explanations
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

I have been building this framework for explaining, analyzing, and teaching about LR(1) grammars for a couple months now.
I hope to turn it into a series of interactive blog posts to explain what parsing is and some approaches we can take to it, most notably [LR(1)](https://en.wikipedia.org/wiki/Canonical_LR_parser) parsing.
Many notable parser generators like Haskellʼs [Happy](https://www.haskell.org/happy/) and the mainstream [Yacc](https://en.wikipedia.org/wiki/Yacc), [Bison](https://en.wikipedia.org/wiki/GNU_Bison), and [Tree-sitter](https://tree-sitter.github.io/tree-sitter/) use variants of LR(1), modified for efficiency by reducing the number of generated states.^[Specifically LALR(1) is the most common alternative, and it cuts down on states by merging ones that are the same except for their lookaheads. We will not focus on any of those details in this series, most likely.]

The interactive widgets here will allow you to build and verify your intuition by clicking through examples, because I believe that once you are armed with the basic ideas and the right intuition, you can figure out the rest of details for yourself.

Alternatively, it can serve as a playground to test out hypotheses about grammars, see exactly where things go wrong when conflicts occur, and what to do to fix those errors.^[Have you ever had to debug a Happy grammar by opening up the [`.info`](https://www.haskell.org/happy/doc/html/sec-invoking.html) file? Itʼs overwhelming!]

My real goal is to show you how intuitive LR(1) parsing can be!
Thatʼs right, thereʼs nothing too advanced or magical going on, just some intuitive ideas carried to their logical conclusions.
I used to be scared of parser generators, but once I was introduced to them I followed this approach to understand them for myself and I hope to share that with you.

As (functional) programmers, weʼre used to learning topics in terms of the appropriate datatypes and operations for the job, and thatʼs what I will go through for you here.
Hint: weʼll use a lot of monoids!

```haskell
data ShiftReduce s r
  = Shift s
  | Reduces (NonEmptyArray r)
  | ShiftReduces s (NonEmptyArray r)

instance Semigroup (ShiftReduce s r) where
  -- We do not expect to see two shifts, so arbitrarily prefer the first one
  append (Shift s) (Shift _) = Shift s
  append (Shift s) (ShiftReduces _ rs) = ShiftReduces s rs
  append (ShiftReduces s rs) (Shift _) = ShiftReduces s rs
  append (ShiftReduces s rs) (ShiftReduces _ rs') = ShiftReduces s (rs <> rs')
  append (Shift s) (Reduces rs) = ShiftReduces s rs
  append (Reduces rs) (Shift s) = ShiftReduces s rs
  append (Reduces rs) (Reduces rs') = Reduces (rs <> rs')
  append (ShiftReduces s rs) (Reduces rs') = ShiftReduces s (rs <> rs')
  append (Reduces rs) (ShiftReduces s rs') = ShiftReduces s (rs <> rs')


```

Thatʼs my big complaint: thereʼs too many numbers floating around in traditional explanations of LR(1) tables, without any indication of what they mean or how they tie together.
So I have used semantic formatting to indicate what they all mean: numbered states [0]{.state}, [1]{.state}, [2]{.state} are highlighted differently from named rules [0]{.rule}, [1]{.rule}, [2]{.rule} and differently from number tokens [0]{.terminal}, [1]{.terminal}, [2]{.terminal}.
Hopefully the pretty colors will keep your attention!

All of the mechanical steps in generating LR(1) parsers will be broken down and their motivation explained.
What does it mean to “close” a state?
How do you know what states are next, and when are you done?
Why are “reduce–reduce” conflicts bad?
Stay tuned to find out!

Skip to [Widgets] below to start using! Or click this button to see them on their own:

::: {widget="Widget.Control" widget-datakey="main"}
:::

## Blog Posts

Right now I donʼt have much helpful content below, but I will slowly add more posts.
I am finally ready to start writing explanatory content now, after working a lot on the behind-the-scenes code to animate it.

Along the way Iʼll also blog about the [technologies](technology.html) Iʼve used to create it.
Big shoutout to [@mikesol](https://github.com/mikesol) for creating the framework Iʼm using, [purescript-deku](https://github.com/mikesol/purescript-deku).

### Topics

- TODO: [Using this tool by example](parser_by_example.html)
- TODO: [Terminology reference](parser_terminology)
- WIP: [Basics: What are grammars](parser_basics.html) (BNF, RegExp)
  1. Nonterminals and terminals
  1. Sequencing and alternation (regexes)
- WIP: [Uses of grammars](parser_applications.html):
  1. Generators: nondeterministically generate strings in the grammar by following the rules as state transitions
  1. Recognition: recognize which strings belong to the grammar and which do not
  1. Syntax highlighting: cursed.
  1. Parsing: find an unambiguous parse tree for inputs that belong to the grammar
- Basics of LR(1) parsing
  1. States
  1. State transitions
  1. Closure of states
  1. Lookahead
- Precedence
  1. Refresher on operator precedence
  1. Operator precedence mapped to LR(1) table parsing
  1. Conflict resolution using precedence operators à la Happy.
- Grammars as datatypes
  1. AST/CSTs as ADTs
  1. Data associated with tokens
  1. Finding perfect representations, e.g. no leading zeroes, if you want it to encode data exactly
  1. Common practice of using grammars this way (e.g. in type theory papers)

### Ideas & Questions

- State exploration: current state, next states, previous states
- Emulate Happy, especially precedence operators
- Can Happy precedence operators be explained in terms of alternative grammars? Can the conflict resolutions always be “pulled back” to a grammar that would generate the same table, or a larger table that represents the same abstract grammar? Does it require quotiented grammars to explain?
- Generate example inputs for each state, especially to diagnose conflicts^[currently it only generates example inputs for nonterminals/productions]
- Explain [Earley parsing](https://en.wikipedia.org/wiki/Earley_parser) using a similar approach
- Better errors!

## Widgets

::: {widget="Widget.Control" widget-datakey="main"}
:::

::: {.widget widget="Widget.Query" widget-empty="true" widget-datakey="default" widget-data-keys="grammar" widget-loading="true"}
:::

### Enter a grammar
Craft a grammar out of a set of rules. Each rule consists of a nonterminal name, then a colon followed by a sequence of nonterminals and terminals. Each rule must also have a unique name, which will be used to refer to it during parsing and when displaying parse trees. If ommitted, an automatic rule name will be supplied based on the nonterminal name.

The top rule is controlled by the upper input boxes (LR(1) grammars often require a top rule that contains a unique terminal to terminate each input), while the lower input boxes are for adding a new rule. The nonterminals are automatically parsed out of the entered rule, which is otherwise assumed to consist of terminals.

Click “Use grammar” to see the current set of rules in action! It may take a few seconds, depending on the size of the grammar and how many states it produces.

:::: widget-group
::: {widget="Widget.Control" widget-datakey="main"}
:::
::: {.widget widget="Parser.Grammar" widget-datakey="default" widget-loading="true"}
:::
::::

### Generate random matching inputs

This will randomly generate some inputs that conform to the grammar. Click on one to send it to be tested down below!

:::: widget-group
::: {widget="Widget.Control" widget-datakey="main"}
:::
::: {.widget widget="Parser.Random" widget-datakey="default" widget-loading="true"}
:::
::::

### Explore building trees in the grammar
Each rule can be read as a transition: “this nonterminal may be replaced with this sequence of terminals and nonterminals”. Build a tree by following these state transitions, and when it consists of only terminals, send it off to be parsed below!

:::: widget-group
::: {widget="Widget.Control" widget-datakey="main"}
:::
::: {.widget widget="Parser.Explorer" widget-datakey="default" widget-loading="true"}
:::
::::

### See parsing step-by-step on custom input
Text entered here (which may also be generated by other widgets below) will be parsed step-by-step, and the final parse tree displayed if the parse succeeded. (Note that the closing terminal is automatically appended, if necessary.) Check the state tables above to see what state the current input ends up in, and the valid next terminals will be highlighted for entry.

:::: widget-group
::: {widget="Widget.Control" widget-datakey="main"}
:::
::: {.widget widget="Parser.Input" widget-datakey="default" widget-loading="true"}
:::
::::

### List of parsing states
To construct the LR(1) parse table, the possible states are enumerated. Each state represents partial progress of some rules in the grammar. The center dot “•” represents the dividing line between already-parsed and about-to-be-parsed.

Each state starts from a few seed rules, which are then closed by adding all nonterminals that could be parsed next. Then new states are explored by advancing on terminals or nonterminals, each of which generates some new seed items. That is, if multiple rules will advance on the same (non)terminal, they will collectively form the seed items for a state. (This state may have been recorded already, in which case nothing further is done.)

When a full rule is parsed, it is eligible to be reduced, but this is only done when one of its lookaheads come next (highlighted in red).

:::: widget-group
::: {widget="Widget.Control" widget-datakey="main"}
:::
::: {.widget widget="Parser.StateTable" widget-datakey="default" widget-loading="true"}
:::
::::

### Table of parse actions for each state
Once the states are enumerated, the table of parse actions can be read off:

Terminals can be “shifted” onto the stack, transitioning to a new state seeded by pushing through that terminal in all applicable rules in the current state.

Completely parsed rules will be “reduced” when their lookahead appears, popping the values matching the rule off of the stack and replacing it with the corresponding nonterminal, which then is received by the last state not involved in the rule.

Nonterminals received from another state trigger “gotos” to indicate the next state.

Two types of conflicts may occur: if a terminal indicates both a shift and reduce actions (shift–reduce conflict) or multiple reduce actions (reduce–reduce conflict). Note that there cannot be multiple shift actions at once, so most implementations (including this one) choose to do the shift action in the case of shift–reduce conflict.

:::: widget-group
::: {widget="Widget.Control" widget-datakey="main"}
:::
::: {.widget widget="Parser.ParseTable" widget-datakey="default" widget-loading="true"}
:::
::::

::: {widget="Widget.Control" widget-datakey="main" widget-parent="#TOC"}
:::
