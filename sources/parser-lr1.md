---
title: "Basics of LR(1) Parsing"
subtitle: "[Interactive Parser Explanations](parser.html)"
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

Here we will explore how LR(1) parsing works: its concepts, its algorithms, and most importantly the intuitions behind these things.

Weʼll start with states, skip to parsing actions, then go back and fill in the details.

## Overview

The heart of LR(1) parsing is how it forms states, and how it transitions between states.
These are pretty straightforward.
The rest is just an exercise in accounting for all the information produced.

To keep track of all of this information, we organize it in a **graph**, a **table**, and a **stack**.

A LR(1) parser generator starts by looking at a grammar and creating a **graph** from it.
Specifically it is a finite [Directed Acyclic Graph (DAG)](https://en.wikipedia.org/wiki/Directed_acyclic_graph):
its nodes are states of the parser and its edges represent ways to transition between the states.

Then it creates a **table** from the graph.
The rows are states (the nodes from the graph), and the columns are the terminals and nonterminals in the grammar.

Finally, when parsing an input, it uses a **stack** to keep track of states.
Using the information kept on the stack, it will simply follow the corresponding instructions in the table based on the symbols in the input.

The first step -- generating the graph -- is the most important.
The other steps are very mechanical, although it is important to understand how transitions are represented in different ways in the graph versus the table.


automaton?


### LR(1) specifically

Those steps are actually common to several types of parsing.
What makes LR(1).

## States

Letʼs talk about what a state means in LR(1) concepts and how to transition between states.

I will assume you know the [terminology of grammars](parser_terminology.html) in general, like nonterminals, terminals, production rules, etc.

A state consists of a bunch of state items.


### State items

An item in a state is very similar to a production rule, except it is a production rule _in progress_ and _in context_.

```haskell
type GrammarRule nt r tok =
  -- nonterminal / production rule name
  { pName :: nt
  -- each rule has a unique name
  , rName :: r
  -- sequence of nonterminals and terminals that make up the rule
  , rule :: Fragment nt tok
  }

type StateItem nt r tok =
  { rName :: r
  , pName :: nt
  , rule :: Zipper nt tok
  , lookahead :: Lookahead tok
  }
```

#### In progress

By **in progress** I mean that some of the rule has been parsed already and some of it is yet to be parsed.

```haskell
data Zipper nt tok = Zipper (Fragment nt tok) (Fragment nt tok)
```

::: {.Details box-name="History"}
The term “zipper” is sort of a generic term for data structures representing traversals in progress, see [Wikipedia](https://en.wikipedia.org/wiki/Zipper_(data_structure)).
:::

#### In context

By **in context** I mean that the rule also comes with lookahead symbols that indicate what we expect to see after the rule is fully parsed.

```haskell
type Lookahead tok = Array tok
```

::: {.Bonus box-name="Preview"}
As we will see later, the lookahead symbols are important to avoid conflicts in the grammar.
If there are multiple rules that are all fully matched, they better have different lookaheads so we know which one to complete parsing.
(This is called a **reduction**.)
Thereʼs an additional way conflicts could occur that will be covered later.
:::

### State transitions

The way to move from one state to another is to push past a symbol, either a nonterminal symbol or a terminal symbol.
You donʼt just do this in one rule, you do it for all the rules that have the specific symbol next.
Advancing these rules produces the **seed items** for the next state.

This is how the parser makes progress.

### Closure

But wait: how does a parser advance on a _nonterminal_?
The input only consists of terminals&nbsp;…

There are two components that work in tandem to achieve what we need:

1. Closure: For each nonterminal we can parse next, we incorporate all production rules for that nonterminal.
  This also requires calculating the lookaheads for the nonterminal, which we use for the added rules.
  But the key part is that it allows dealing with the ambiguity of .....
2. Reductions: When a rule is fully parsed, we are able to reduce it.
  This sends the information that a nonterminal has been parsed up the stack, to the rule that needs to accept it.

#### Computing lookaheads

## Parsing actions

Now weʼll skip to the end of the story: how does the parser actually run?
From there we can fill in the details.

### Stack

## Generation

Finally we get to fill in the bridge: how to generate the graph and the table.
