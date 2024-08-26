---
title: Algebra of CSS Selectors
subtitle: How do you zip two nested selectors together?
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

A basic demo of the algebra of CSS selectors.
Enter many selectors (they can be complex selectors), and it will compute their *conjunction*.
(Disjunction is already given by the comma separator in CSS.)

I need to add better support for selectors and negation.
The output can be optimized too (I know how, I just need to finish implementing it).
The part I was not able to do originally but probably can do now is to implement negation of complex selectors.

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
  (whatʼs the difference? why? but this is also not optimal)
- `.a ~ .b + .c ~ .d`{.css}, `.w + .x ~ .y ~ .z`{.css} []{.widget widget="Parser.Main.CSS.Example" widget-datakey="default" widget-data-example="[&quot;.a ~ .b + .c ~ .d&quot;,&quot;.w + .x ~ .y ~ .z&quot;]"}
  <br/>
  (okay, youʼre not guessing this one!)

Can you guess what the result will be?
See if you can figure out the rules for how it works!


[CSS selectors as Boolean Algebra](https://github.com/MonoidMusician/purescript-free-boolean/blob/slides/scanned/Slides.pdf) (warning: 35MB PDF)
