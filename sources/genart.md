---
title: Gen Art Scratchpad
subtitle: A really simple editor for generative art in JS
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

## Editor

<style>
  nav {
    display: none;
  }
  section#editor {
    margin: 0 -20vw;
    padding: 0 !important;
    max-width: initial !important;
  }
  @media (max-width: 760px) {
    section#editor {
      margin: 0 -5vw;
    }
  }
  section#editor > *, #studio > *, #panes > * {
    margin: 0 !important;
    padding: 0 !important;
    max-width: initial !important;
  }
  #panes {
    display: flex;
    flex-direction: row;
  }
  section#editor > #studio > #panes > * {
    width: 50%;
  }
  section#editor > #studio > #panes > * > :first-child {
    border: 1px solid gray;
  }
  textarea {
    background: inherit;
    color: inherit;
  }
  textarea:focus, section#editor > #studio > #panes > * > textarea:focus {
    outline: none;
    border-color: #b859a7;
  }
  #bar > p {
    margin: 0;
  }
</style>
:::::: {#studio}
::::: {#panes}
:::: {}
  <textarea id="textarea" style="display: block; width: 100%; font-family: monospace; font-size: 14px; height: calc(100vh - 140px); background: inherit; resize: none;box-sizing: border-box;"></textarea>
::::

:::: {}
  <canvas id="canvas" style="display:block;margin:auto;max-width:100%" width="1500px" height="500px"></canvas>
::::
:::::

::::: {#bar}
<input id="active"></input>
<input id="color" type="color"></input>
:::::
::::::
<script src="assets/js/genart.js"></script>

## Instructions

Live code on the left, see the result on the right.
Changes apply `onChange`{.js} or ctrl/cmd+enter.

You have access to the `ctx`{.js} for the canvas, that is it.
(But of course `ctx.canvas`{.js} will give you the canvas element itself.)

The bottom text input component is a label for the work of art, used to store it in `localStorage`{.js}.
Changing it will load whatever is at the label, or save to it if it is blank (ctrl/cmd+enter will force saving to it).

The color input component live replaces whatever selection you have in the text area with the chosen hex color of the form `#RRGGBB`.

The canvas size is currently fixed to 500x500.
(TODO: make configurable.)

TODO: make more interactive components.

TODO: autocomplete for names

TODO: proper JS editor???
