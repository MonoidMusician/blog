---
title: "Basics: What Are Grammars"
subtitle: "[Interactive Parser Explanations](parser.html)"
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

Letʼs look at .

## The structure of a grammar

## Regular expressions
Regular expressions are a restricted type of grammar.
But wait, regexes look like `/^([a-zA-Z0-9_\-\.]+)@([a-zA-Z0-9_\-\.]+)\.([a-zA-Z]{2,5})$/`{.awk}, and thatʼs not at all like what weʼve been considering here.

Thereʼs two things going on here:

1. Regular expressions that we write have a lot of fancy features, like character classes `/[0-9]/`{.awk}, that are really just syntax for simpler features, like `/0|1|2|3|4|5|6|7|8|9/`{.awk} in that example.
2. The simpler features of the formal definition of regular expressions can be converted into our grammars without too much work.

Regular expressions, considered as formal types of grammars, have only a few operations:

- Alternating: `/a|b/`{.awk}.
  Choose between different alternatives.
- Repetition: `/a*/`{.awk} and `/a+/`{.awk}.
  Repeat the same pattern.
- Sequencing: `/ab/`{.awk}.
  Match one pattern then match the next pattern.
  (This is the trickiest since it isnʼt written with an operator and so itʼs often forgotten about.)


The trick is that we can encode these operations into many rules.
