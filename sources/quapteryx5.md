---
title: "Quapteryx Takes Flight!"
subtitle: "Quapteryx Part V"
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
# date: 2025/05/11
---

<style>#TOC {display: none;}</style>

<details open style="margin-bottom: 1em" class="Example">
<summary>Example expressions</summary>
Basics:
<button data-example="00 K 3 1">First</button>
<button data-example="00 0KI 3 1">Second</button>
<button data-example="000 S 3 2 1">Share</button>
<br/>
Simple arithmetic:
<button data-example="`$asNat ``+ #3 #2">3 + 2</button>
<button data-example="`$asNat ``* #3 #2">3 * 2</button>
<br/>
Factorial function:
<button data-example="`$asNat `$factorial #4">4 factorial</button>
<button data-example="`$asNat `$factorial #6">6 factorial (slow)</button>
<br/>
Infinity and beyond:
<button data-example="` M M">Self-reducing</button>
<button data-example="` Y Y">Infinitely growing</button>
<button data-example="0000030333333">Smallest infinitely growing program</button>
<button data-example="K 2 `YY">Lazy evaluation</button>
<br/>
</details>

<div class="sourceCode side-label" data-lang="In"><pre><code><textarea style="height: 100px" id="quapteryx_input" autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false"></textarea></code></pre></div>
<br/>
<div id="quapteryx_calculating" style="display: none">Reducing… <button id="quapteryx_cancel" class="delete">Stop</button></div>
<div id="quapteryx_output_wrapper" style="display: none" class="sourceCode side-label" data-lang="Out"><pre class="wrap"><code style="max-height: 80svh; overflow-y: auto"><span id="quapteryx_output"></span></code></pre></div>
<div id="quapteryx_error" style="display: none"></div>

<script type="module">
import { quapteryx } from "../assets/js/quapteryx.mjs";
import { sugar, toatomic } from "../assets/js/combinators.mjs";
const { ById } = Ve; // verity.js
{
  let input = ById.quapteryx_input;
  let output = ById.quapteryx_output;
  let last = null;
  input.onchange = () => {
    if (last) { try { last.cancel(); } catch {} last = null }
    if (input.value) {
      var evaluating = quapteryx(toatomic(sugar(input.value)));
      if (typeof evaluating === 'string') {
        output.textContent = evaluating;
        ById.quapteryx_calculating.style.display = 'none';
        ById.quapteryx_output_wrapper.style.display = '';
        ById.quapteryx_error.style.display = 'none';
        last = null;
      } else {
        output.textContent = "Loading...";
        ById.quapteryx_calculating.style.display = '';
        ById.quapteryx_output_wrapper.style.display = 'none';
        ById.quapteryx_error.style.display = 'none';
        (last = evaluating).then(
          evaluated => {
            output.textContent = evaluated;
            ById.quapteryx_calculating.style.display = 'none';
            ById.quapteryx_output_wrapper.style.display = '';
            ById.quapteryx_error.style.display = 'none';
          },
          err => {
            ById.quapteryx_calculating.style.display = 'none';
            ById.quapteryx_output_wrapper.style.display = 'none';
            ById.quapteryx_error.style.display = '';
            console.error(err);
            ById.quapteryx_error.textContent = err instanceof Error ? `${err.name}: ${err.message}` : err;

            last = null;
          },
        );
      }
    } else {
      ById.quapteryx_calculating.style.display = 'none';
      ById.quapteryx_output_wrapper.style.display = 'none';
      ById.quapteryx_error.style.display = 'none';
    }
  };
  ById.quapteryx_cancel.onclick = () => {
    let evaluating;
    if (last) {
      try {
        evaluating = last.cancel();
      } catch(err) {
        ById.quapteryx_calculating.style.display = 'none';
        ById.quapteryx_output_wrapper.style.display = 'none';
        ById.quapteryx_error.style.display = '';
        console.error(err);
        ById.quapteryx_error.textContent = err instanceof Error ? `${err.name}: ${err.message}` : err;

        last = null;
        return;
      }
      last = null;
    }
    if (evaluating) {
      output.textContent = evaluating;
      ById.quapteryx_calculating.style.display = 'none';
      ById.quapteryx_output_wrapper.style.display = '';
      ById.quapteryx_error.style.display = 'none';
      last = null;
    } else {
      ById.quapteryx_calculating.style.display = 'none';
      ById.quapteryx_output_wrapper.style.display = 'none';
      ById.quapteryx_error.style.display = 'none';
    }
  };
  document.onclick = (e) => {
    if (e.target.dataset["example"]) {
      input.value = e.target.dataset["example"];
      input.onchange();
    }
  }
};
</script>

## Combinators

:::centered
*Based on https://www.angelfire.com/tx4/cus/combinator/birds.html with help from https://crypto.stanford.edu/~blynn/lambda/sk.html as well*
:::

<div class="full-width h-scroll" style="padding: 1em">
<table>
  <thead><tr>
    <th>Symbol</th><th>Name</th><th>Definition</th><th>Lambda Term</th><th style="text-align: left">Quapteryx String / <br/> SKI Term</th>
  </tr></thead>
  <tbody id="combinator_table">
  </tbody>
  <tfoot><tr>
    <th>Symbol</th><th>Name</th><th>Definition</th><th>Lambda Term</th><th style="text-align: left">Quapteryx String / <br/> SKI Term</th>
  </tr></tfoot>
</table>
</div>

<script type="module">
import { combinatorTable, combinators, sugar, toCombinators } from "../assets/js/combinators.mjs";
import * as c from "../assets/js/combinators.mjs";
globalThis.combinators = c; // export it to the browser console

const { HTML, ById } = Ve; // verity.js

let table = ById.combinator_table;

for (const [symbol, sk] of Object.entries(combinatorTable)) {
  if (typeof sk === 'string') {
    table.appendChild(HTML.tr(HTML.td({
      colSpan: 5,
      style: { 'text-align': 'left', 'font-style': 'italic' },
    }, sk)));
  } else {
    let lambda;
    table.appendChild(HTML.tr(
      HTML.th(HTML.span.code(symbol)),
      HTML.td((sk.name || '') + (sk.name && sk.bird ? ' / ' : '') + (sk.bird || '')),
      ...(sk.value === "0"
        ? [
          HTML.td({ colSpan: 2 }, HTML.em("Just corresponds to syntax in combinator calculus, not to a combinator per se")),
          HTML.td(HTML.span.code(sk.value)),
        ]
        : [
          HTML.td(HTML.span.code(sk.def)),
          HTML.td(HTML.span({ class: 'code nowrap' }, sk.lambda)),
          HTML.td(
            HTML.span.code(sk.value),
            HTML.br,
            HTML.span.code(toCombinators(sk.value)),
          ),
        ]
      ),
    ));
  }
}
</script>
