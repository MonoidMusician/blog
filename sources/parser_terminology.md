---
title: Terminology Reference
subtitle: "[Interactive Parser Explanations](parser.html)"
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

Just a reference list of terminology, as a compact little walkthrough.

A grammar is a bunch of production rules.
Each production rule has a nonterminal on the left and on the right they have a sequence of terminals and nonterminals.
Sequencing is represented directly in the rules.
Choice (a.k.a. alternation) is encoded by multiple rules for the same nonterminal.

Nonterminals name parts of the grammar, are associated with multiple defining rules (encoding choice), and may be referenced in other rules and recursively in their own rules.

A terminal in a production rule denotes a token, either a literal token from the input like `+`, or a category of token like `digit` (matching `0`, `1`, &c.).

A LR(1) state consists of LR(1) state items, starting with a set of _seed items_ that are then _closed_ by adding more items.
Closure is the process of adding the rules for all nonterminals that may be parsed next.

A LR(1) state item is a production rule in progress with lookahead.
That is, the rule value has a cursor separating already-parsed from to-be-parsed.

A lookahead is a set of strings to expect after parsing the current rule fully; in LR(1) grammars it is a single token.
Lookaheads are used to selectively apply reduction rules.

A reduction rule is a fully-parsed rule, which will trigger a reduction action in the parser when the lookahead is seen.

A reduction action pops the … off the parsing stack and adds them back headed by the nonterminal the rule belongs to.
This then triggers a goto for the nonterminal .
(The goto is to nonterminals as shifts are to tokens; that is, they are both methods of advancing a production rule past the next part it needs to parse.)

A shift action simply takes a terminal from the input and adds it to the stack, advancing to the next state (whose seed rules are given by advancing that terminal in all applicable rules).
