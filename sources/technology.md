---
title: Blog Technology
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

The source can be found at [MonoidMusician/blog](https://github.com/MonoidMusician/blog) on GitHub.

## Fonts
<!-- TODO link other regions of fonts -->
<!-- TODO fix links -->

[Cormorant](https://github.com/CatharsisFonts/Cormorant){style='font-family: Cormorant'}
  : I fell in love with this font upon seeing its [Bēhance gallery page](https://www.behance.net/gallery/28579883/Cormorant-an-open-source-display-font-family).
    Turns out, my friend and I had also already settled on it for their website!
    Itʼs a modern, more angular/precise cut of the classic Garamond family.
    Itʼs a bit risky at body font sizes, but its Garamond OpenType font features assist in keeping it legible, and I went for a more generous font size and weight.

    I adore the Upright style, an [unslanted italic]{style='font-family: Cormorant Upright'} that has served great in places where I need a more cursive vibe without the semantic implications of a _slanted italic_.

    I had some trouble finding the right source for the files.
    The latest commit in the GitHub repo seems broken: at least on Mac, the [even-odd](https://en.wikipedia.org/wiki/Even%E2%80%93odd_rule) rule was broken, making the cross-braces on “A” and “e” cut into the figures, which not only looked funny at high resolutions but also destroyed the hinting at low resolutions, almost making the cross-braces disappear.
    I think I ended up going with the version downloaded from Google Fonts, which contains all of the [OpenType features](https://fonts.google.com/knowledge/introducing_type/open_type_features_in_practice) though the font served on their CDN does not.

    ::: Warning
    > At the time of writing, non-essential OpenType features aren’t included in web fonts served via Google Fonts’ API; however, the OpenType features are still present in the downloadable fonts, which can then be [self-hosted](https://fonts.google.com/knowledge/using_type/self_hosting_web_fonts).
    :::

[EB Garamond ([Ελληνικά]{style='font-family: EB Garamond GR'})](https://fonts.google.com/specimen/EB+Garamond?subset=greek){style='font-family: EB Garamond'}
  : I used this purely to supplement Cormorant with Greek characters, which otherwise only contains Latin and Cyrillic.

    <details class="Details" data-box-name="CSS">
    <summary>CSS Details</summary>
    I do this by making it the default font, coming before Cormorant, but only loading the Greek section of it by chopping down the default Google Font loader.
    This is necessary because Comorant actually does contain a _few_ Greek characters, like [&pi;]{style='font-family: Cormorant'} (compare [&pi;]{style='font-family: EB Garamond'} in EB Garamond), so we need EB Garamond to override those for the sake of consistency.

    ```css
    body {
      font-family: "EB Garamond GR", "Cormorant", "Apple Color Emoji";
    }

    /* greek-ext */
    @font-face {
      font-family: "EB Garamond GR";
      font-style: italic;
      font-weight: 400;
      font-display: swap;
      src: url(https://fonts.gstatic.com/s/ebgaramond/v25/SlGFmQSNjdsmc35JDF1K5GRwUjcdlttVFm-rI7e8QL9xU661hGFJRvzr2Q.woff) format("woff");
      unicode-range: U+1F00-1FFF;
    }
    /* greek */
    @font-face {
      font-family: "EB Garamond GR";
      font-style: italic;
      font-weight: 400;
      font-display: swap;
      src: url(https://fonts.gstatic.com/s/ebgaramond/v25/SlGFmQSNjdsmc35JDF1K5GRwUjcdlttVFm-rI7e8QL9-U661hGFJRvzr2Q.woff) format("woff");
      unicode-range: U+0370-03FF;
    }
    /* greek-ext */
    @font-face {
      font-family: "EB Garamond GR";
      font-style: normal;
      font-weight: 400;
      font-display: swap;
      src: url(https://fonts.gstatic.com/s/ebgaramond/v25/SlGDmQSNjdsmc35JDF1K5E55YMjF_7DPuGi-6_RkCI95WamXgHlIbvw.woff) format("woff");
      unicode-range: U+1F00-1FFF;
    }
    /* greek */
    @font-face {
      font-family: "EB Garamond GR";
      font-style: normal;
      font-weight: 400;
      font-display: swap;
      src: url(https://fonts.gstatic.com/s/ebgaramond/v25/SlGDmQSNjdsmc35JDF1K5E55YMjF_7DPuGi-6_RkB495WamXgHlIbvw.woff) format("woff");
      unicode-range: U+0370-03FF;
    }
    ```

    (Notice how the `greek` and `greek-ext` URLs only differ by 1 or 2 characters – ask me how I learned that^[{-} (The hard way)].)
    </details>

[Mulish](https://fonts.google.com/specimen/Mulish){style='font-family: Mulish'}
  : I donʼt have a favorite sans-serif font.
    I do like Libertinus Sans, but it isnʼt appropriate for most situations.
    But this seems like a nice modern, legible sans-serif font.

[Fira Sans ([Ελληνικά]{style='font-family: Fira Sans GR'})](https://fonts.google.com/specimen/Fira+Sans?subset=greek){style='font-family: Fira Sans'}
  : Same story as the other Greek supplement font.
    Same trick.
    <!--
    Mulish actually does contain a few Greek characters, like [&pi;]{style='font-family: Mulish'} (compare [&pi;]{style='font-family: Fira Sans'} in Fira Sans)
    -->

[Fira Code](https://github.com/tonsky/FiraCode){style='font-family: Fira Code'}
  : Iʼve been using [Hasklig](https://github.com/i-tu/Hasklig) for a couple years for my personal coding, but as I was looking for a good font for this blog I found Fira Code on Google Fonts.
    I started using it for my personal coding too – after all, if Iʼm going to use it for my blog I should find it good enough to use myself!^[And I have been satisfied with the experience.]

    At first I thought I would be able to use the Google Fonts hosted version, but due to the same reason as above that did not work out due to the lack of OpenType features.
    I needed finer control over the ligatures Fira Code was using.
    In particular, PureScript uses the backwards fat arrow `<=`{style='font-feature-settings: "cv20" 1'} for class declarations, which Fira Code can render like that but by default renders as a less-than-or-equals sign `<=`{style='font-feature-settings: "cv20" 0'}.
    This is controlled with stylistic set `cv20`, so I set `font-feature-settings: "cv20" 1` on this blog, since I think `<=`{style='font-feature-settings: "cv20" 1'} is less confusing than `<=`{style='font-feature-settings: "cv20" 0'} – but of course a context-sensitive approach would be better.

    I am also tempted to make my own version of the font with visible spaces, but I donʼt yet have enough experience with the font building tools to do that yet.

[Oswald](https://fonts.google.com/specimen/Oswald){style='font-family: Oswald'}
  : Used for nonterminals and rule names in grammars.

    I needed something with a lot of contrast from Fira Code, which basically meant tall and skinny.
    It doesnʼt have a lot of characters though, just Latin, Cyrillic, and ASCII symbols (no arrows unfortunately).

[Amaranth](https://fonts.google.com/specimen/Amaranth){style='font-family: Amaranth'}
  : I spent a while trying to find a nice sans-serif font that had character to use for UI accents, like the info boxes I sprinkled into the [Eudoxus real numbers post](Eudoxus.html) and the input boxes on the [parsing posts](parser.html).
    I really love Amaranth because it meets these goals: almost all characters have a little extra flair and curve to them, so it remains distinctive and legible.
    I just wish it was available as a [variable font](https://fonts.google.com/knowledge/introducing_type/introducing_variable_fonts), itʼs a bit heavy for my liking.
    Of course, it doesnʼt help that Cormorant is so feather-light …

## Pandoc
[Pandoc](https://pandoc.org/) is cool because itʼs written in Haskell and does what I need.
It supports lots of nice Markdown features, seems to have a sane data model, and is pretty easily extensible.
I knew that optimizing for SSR static content that doesnʼt require JS was one of my design goals, so thatʼs why itʼs great.

### Lua
Pandoc includes a builtin Lua scripting language for extensions that transform the content, as opposed to slow external scrips that need to serialize the whole document back and forth.

I tried using some third party scripts, but I ended up rewriting them in Lua:

[pandoc-static-katex](https://github.com/Zaharid/pandoc_static_katex) → [static-katex.lua](https://github.com/MonoidMusician/blog/blob/main/pandoc/lua/static-katex.lua)
  : Ported the script that used python to call node! Ugh that was unnecessary indirection.

    I added caching and trimmed the trailing space from the KaTeX output, which otherwise would appear as unwanted space between KaTeX-rendered numbers and their suffixes or whatnot in inline math output.
    Running it with `npx` was slow, so I cached the binary location from that, but that didnʼt work on the server which immediately deleted the temporary installation or something, even though I thought I had a non-temporary installation.
    Who knows.

[pandoc-sidenote](https://github.com/jez/pandoc-sidenote) → [sidenotes.lua](https://github.com/MonoidMusician/blog/blob/main/pandoc/lua/sidenotes.lua)
  : This is a pretty direct port as well, with slightly different parsing of the variants of notes, and I removed the generated symbol for toggleable sidenotes (for mobile layouts).
    I was having trouble with the binary versions compatibility, so Lua is clearly the easier option here for compatibility.

[anchor-links.lua](https://github.com/jgm/pandoc-website/blob/master/tools/anchor-links.lua)
  : Just copied.

I also implemented SSR for some of my extensions.

[canvas.lua](https://github.com/MonoidMusician/blog/blob/main/pandoc/lua/canvas.lua)
  : Renders the dynamic canvas content Iʼve added to my post on [Eudoxus real numbers](Eudoxus.html) to static PNGs that are loaded with `<noscript>`{.html} tags.
    Uses [parse5](https://parse5.js.org/) to parse the [raw HTML](https://pandoc.org/lua-filters.html#type-rawblock) embedded in the Pandoc and the node [canvas](https://www.npmjs.com/package/canvas) library (based on [Cairo](https://www.cairographics.org/)!) to run the canvas.
    It renders to a default size based on the attributes, e.g. `<canvas data-graph="[x => x]" class="pixelated" width="1000" height="500" style="width: 100%;"></canvas>`{.html}.

## Widgets

I wanted Pandoc/Markdown content to be primary, but I also wanted to make interactive blog posts with embedded widgets written in PureScript.

### Embedding into posts

The widgets are embedded with Pandocʼs [fenced divs](https://pandoc.org/MANUAL.html#extension-fenced_divs) (which embed unknown attributes as data attributes) and then they are instantiated with a PS helper that scans for the selector `[data-widget]`{.css}.
Widgets are wired up to shared datasources based on the provided `data-widget-datakey`{.er}, making it nice and modular across a whole page:

```markdown
:::: widget-group
::: {widget="Widget.Control" widget-datakey="main"}
:::
::: {.widget widget="Parser.Grammar" widget-datakey="default" widget-loading="true"}
:::
::::
```

The dashboard mode is implemented all in CSS, and is the first time Iʼve actually used `{display: contents}`{.css} to reach inside elements.
In particular, we need to peek through `<section>`{.html} tags and `.widgets`{.css} and `.widget-group`{.css} classes and gather their children to throw directly into the top-level `{display: grid}`{.css} container (usually the `<body class="focus-mode">`{.html} element).

## CSS
I like [Sass](https://sass-lang.com/), most significantly for the [color manipulation functions](https://sass-lang.com/documentation/modules/color) and also the syntax.
Of course the nested selectors in Sass/SCSS is a huge convenience for DRY, plus I find the braceless syntax of Sass a little nicer to work with quickly, but syntax support in editors seems abysmal for some reason I donʼt understand.
Maybe SCSS support is better just because the syntax definitions could be based on CSS and extended.

### SCSS as a language

SCSS^[And by extension Sass – since SCSS includes certain novel features and Sass merely includes a new syntax, I should mention the new features by the name SCSS for brevity.] really needs antiquotation for selectors, attributes, and other core parts of syntax.
That is, since SCSS is 90% a glorified string concatenation library – mainly running on manipulating strings – one of its key usefulnesses is in providing feedback (e.g. in the form of syntax highlighting) on whether the user has entered sensible strings.
So it would be great to have helpers like `$sel: syntax.selector(details > :not(summary))`{.sass} to quote that selector expression into `$sel: "details > :not(summary)"`{.sass} so it can be used in metaprogramming (i.e. passed to the rest of the utility of SCSS) while maintaining the syntactic discipline and feedback of the selector class specifically.

As a specific example, SCSS docs themselves recommend [this helper function](https://sass-lang.com/documentation/style-rules/parent-selector#advanced-nesting) (to paste over other limitations of their implementation of selector extension with `&`{.sass}).
But in doing so you lose the semantic intention that `$child: "input"`{.sass} means a selector specifically, so if you misspell it you will not have any quick feedback to correct the issue.
Even notice how `&`{.sass} is not highlighted as an operator inside `selector.unify`{.sass}, since it is just treated as an unquoted string literal, not as a selector specifically.

```sass
@use "sass:selector"

@mixin unify-parent($child)
  @at-root #{selector.unify(&, $child)}
    @content

.wrapper .field
  @include unify-parent("input")
    /* ... */

  @include unify-parent("select")
    /* ... */
```

In general thereʼs a deeper discussion to be had here about how parts of a syntax definition correspond to types in their interpretation.

### Design

Steps:

1. Try stuff
2. Record the things that work
3. Articulate how they work, what doesnʼt work
4. Find a philosophy behind it
5. Generalize, abstract
6. Win

### Quirks

Some weird quirks and anti-quirks Iʼve run into while implementing stuff:

1. The most annoying one I do not have a workaround for:
  I implemented floating vertical _and_ horizontal headers for one of the [parsing tables](parser.html#table-of-parse-actions-for-each-state).
  It works great in dashboard mode, but in the blogpost mode I have to set `{overflow: auto hidden}`{.css} so that it scrolls horizontally but not vertically in the body of the post.
  Unfortunately, due to how scroll containers work in CSS, this makes only the horizontal scrolling move the headers, not the vertical scrolling, which is kind of annoying 😒.
  I donʼt know what to do about it.
1. Tiny thing: Only Chrome seems to implement `border-image`{.kw} tweening at the moment.
  Itʼs a long story why Iʼm using it in the first place (for info boxes), but I like it both with and without.
1. Good news: Iʼm so happy that `:focus-visible`{.css} is actually supported now.
  I swear the last time I was doing this stuff I was going to have to implement it myself.
1. Bugbear: I like using the `<label><input></label>`{.html} pattern, to avoid having to generate an `id`{.er} for the `<input>`{.html}, but that doesnʼt work when the `<input>`{.html} needs to control the visibility of elements with its `:checked`{.css} status.
  Nobody else seems to like doing this anyways …
1. Workaround: Safari on iOS wanted to open the background image^[In light theme, I have a subtle background texture.] on long-press^[Erm, I think itʼs actually called 3D Touch].
  It was very annoying when scrolling and pausing, so I had to have a fake `{background-image: linear-gradient(transparent, transparent), …}`{.css} appear before the actual background image, since iOS does not want to open up a `linear-gradient`{.fu} image.

## KaTeX

### Quirks

1. https://github.com/KaTeX/KaTeX/issues/3168

## LilyPond

Coming soon??

<!--
## Makefile
Of course, what good project doesnʼt need a Makefile?
-->
