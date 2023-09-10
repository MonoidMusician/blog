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

Some examples to get you started:

- `a + b`{.css}, `a + b`{.css} []{.widget widget="Parser.Main.CSS.Example" widget-datakey="default" widget-data-example="[&quot;a + b&quot;,&quot;a + b&quot;]"}
- `a + b`{.css}, `a + c`{.css} []{.widget widget="Parser.Main.CSS.Example" widget-datakey="default" widget-data-example="[&quot;a + b&quot;,&quot;a + c&quot;]"}
- `a + .b`{.css}, `a + .c`{.css} []{.widget widget="Parser.Main.CSS.Example" widget-datakey="default" widget-data-example="[&quot;a + .b&quot;,&quot;a + .c&quot;]"}
- `a + .b`{.css}, `a ~ .c`{.css} []{.widget widget="Parser.Main.CSS.Example" widget-datakey="default" widget-data-example="[&quot;a + .b&quot;,&quot;a ~ .c&quot;]"}
- `.x + .b`{.css}, `.y ~ .c`{.css} []{.widget widget="Parser.Main.CSS.Example" widget-datakey="default" widget-data-example="[&quot;.x + .b&quot;,&quot;.y ~ .c&quot;]"}
- `.x ~ .b`{.css}, `.y ~ .c`{.css} []{.widget widget="Parser.Main.CSS.Example" widget-datakey="default" widget-data-example="[&quot;.x ~ .b&quot;,&quot;.y ~ .c&quot;]"}
- `a ~ c`{.css}, `a + b + c`{.css} []{.widget widget="Parser.Main.CSS.Example" widget-datakey="default" widget-data-example="[&quot;a ~ c&quot;,&quot;a + b + c&quot;]"}
  <br/>
  (actually not optimal, oops …)
- `.a ~ .c`{.css}, `.a + .b + .c`{.css} []{.widget widget="Parser.Main.CSS.Example" widget-datakey="default" widget-data-example="[&quot;.a ~ .c&quot;,&quot;.a + .b + .c&quot;]"}
  <br/>
  (what’s the difference? why? but this is also not optimal)
- `.a ~ .b + .c ~ .d`{.css}, `.w + .x ~ .y ~ .z`{.css} []{.widget widget="Parser.Main.CSS.Example" widget-datakey="default" widget-data-example="[&quot;.a ~ .b + .c ~ .d&quot;,&quot;.w + .x ~ .y ~ .z&quot;]"}
  <br/>
  (okay, you’re not guessing this one!)

Can you guess what the result will be?
See if you can figure out the rules for how it works!
