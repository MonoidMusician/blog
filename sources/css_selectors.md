---
title: Algebra of CSS Selectors
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

A basic demo of the algebra of CSS selectors.
Enter many selectors (they can be complex selectors), and it will compute their *conjunction*.
(Disjunction is already given by the comma separator in CSS.)

The parser takes 30s–1m to load, and this will hang your browser tab, sorry.
I will optimize it later, and I will add better support for selectors and negation!
Probably buggy too.

::: {.widget widget="Parser.Main.CSS" widget-datakey="default" widget-loading="true"}
:::

See if you can figure out the rules for how it works!
