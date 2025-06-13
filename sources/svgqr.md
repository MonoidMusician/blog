---
title: Perfect Vector Graphics for a QR code
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2025/01/29
---

You would think that SVG would be a perfect medium for rendering QR codes (or other 2D matrix codes): it is perfectly scalable so the renderer can decide how best to render it at each resolution, thereʼs no need to mess around with hints like [`image-rendering: pixelated;`{.css}](https://developer.mozilla.org/en-US/docs/Web/CSS/image-rendering), and so there should be no compromises, right? Right??

Well the quality of SVG renderers really varies, and iOS was having trouble rendering the SVGs for the complex, large QR codes I was using (nearly max size).
Specifically, it was failing to render adjacent black modules^[A module is a pixel unit of the QR code, as opposed to the pixels used when rendering it to a screen.] of the QR code as contiguous blocks of rendered pixels: in fact, entire rows and columns “between” the modules were consistently white all the way across, but only a single screen pixel in width.
(Some form of this problem appeared at every zoom level I tried.)

Presumably this is due to some rounding assumptions the renderer made when dealing with many objects that had fill but no stroke.
After all, I had chosen the simplest method of *generating* the SVG: each module became a `1x1`{.dimension} filled rectangle in the SVG, which was then sized to fit on the screen.
Again, one would naïvely think that having it on integer coordinates in the SVG coordinate space may help a renderer realize they were adjacent, certainly there were no floating point problems before scaling, but for whatever reason, this did not in fact help.

So in the end, maybe it wasnʼt so surprising that we are obligated to help the renderer along.

How best to do this?

Well, the answer (and about the only thing we can try) is to teach the renderer that there are no gaps between adjacent modules.
We need to render adjacent modules as a single SVG path, and modules adjacent to those, and so on: the [*connected components*](https://en.wikipedia.org/wiki/Connected_component_(graph_theory)) of the QR code.

So there are two core steps: finding the connected components of the matrix, and rendering each component into the SVG as a single path.

As one example, the recognition and locating features of the QR code are nested squares, so each inner square and each outer square will now be a single path each, forcing the renderer to render all of their modules contiguously, helping ensure there are no glitchy gaps.
Other patterns are essentially random, and in fact are meant to be somewhat unpredictable^[(look distinct from other features of the QR code)].

<div>
<noscript><img src="assets/images/qr_demo1.svg"></noscript>
<div id="demo_qr" data-qr="https://webrtc-over-qr.veritates.love" class="fit"></div>
<div class="scriptful">
<label class="input-wrapper" style="width: 100%">
  <span>QR text</span>
  <input id="demo_input" class="code" style="width: 100%; box-sizing: border-box; text-overflow: ellipsis" value="https://webrtc-over-qr.veritates.love" />
</label>
<label class="input-wrapper">
  <span>Margin</span>
  <input id="demo_margin" class="code" style="width: 50px" type="number" min="0" max="4" value="1" />
</label>
<label>
  <input id="demo_debug" type="checkbox" checked />
  <span>Debug</span>
</label>
<label>
  <input name="demo_ecc" type="radio" checked value="L" />
  <span>L</span>
</label>
<label>
  <input name="demo_ecc" type="radio" value="M" />
  <span>M</span>
</label>
<label>
  <input name="demo_ecc" type="radio" value="Q" />
  <span>Q</span>
</label>
<label>
  <input name="demo_ecc" type="radio" value="H" />
  <span>H</span>
</label>
<div style="display: flex; justify-content: space-around; margin-top: 1em">
  <div>Components: <span data-output="components.length">57</span></div>
  <div>Version: <span data-output="version.versionNumber">3</span></div>
  <div>Codewords: <span data-output="version.totalCodewords">70</span></div>
  <div>Mask pattern: <span data-output="maskPattern">7</span></div>
  <div>[<a id="demo_download" download="QR.svg">Download</a>]</div>
</div>
</div>
</div>

<script src="assets/js/zxing.min.js"></script>
<script src="assets/js/moreqr.js"></script>
<script>
  "use strict";
  window.addEventListener('DOMContentLoaded', function() {
    Ve.forQuery('[data-qr]', (e) => {
      MoreQR.debug = true;
      const matrix = ZXing.QRCodeEncoder.encode(e.dataset.qr, ZXing.QRCodeDecoderErrorCorrectionLevel.L).getMatrix();
      const components = MoreQR.matrix2components(matrix);
      const paths = components.map(MoreQR.component2path);
      const margin = 4;
      const viewBox = `${-margin} ${-margin} ${matrix.getWidth()+2*margin} ${matrix.getHeight()+2*margin}`;
      const svg = Ve.SVG.svg.qrcode(
        { attrs: { viewBox } },
        paths,
      );
      if (e.dataset.viewbox) svg.attrs.viewBox = e.dataset.viewbox;
      e.clearChildren(); // from verity.js <3
      e.appendChild(svg);
    });
    let debounce = 0, generation;
    function demo() {
      const now = Date.now();
      if (generation || now < debounce+200) {
        generation && clearTimeout(generation);
        generation = setTimeout(() => { generation = undefined; demo() }, 250);
        debounce = now;
        return;
      } else { debounce = now; generation = undefined; }
      const ecc = document.querySelector('[name=demo_ecc]:checked')?.value ?? 'L';
      MoreQR.debug = Ve.ById.demo_debug?.checked ?? true;
      const encoded = ZXing.QRCodeEncoder.encode(Ve.ById.demo_input.value, ZXing.QRCodeDecoderErrorCorrectionLevel[ecc]);
      const matrix = encoded.getMatrix();
      const components = MoreQR.matrix2components(matrix);
      const paths = components.map(MoreQR.component2path);
      console.log(Object.assign(encoded, { components }));
      const margin = Ve.ById.demo_margin?.value ?? 1;
      const viewBox = `${-margin} ${-margin} ${matrix.getWidth()+2*margin} ${matrix.getHeight()+2*margin}`;
      const svg = Ve.SVG.svg.qrcode(
        { attrs: { viewBox } },
        paths,
      );
      if (!MoreQR.debug) svg.style.background = 'white';
      Ve.ById.demo_qr.clearChildren();
      Ve.ById.demo_qr.appendChild(svg);
      // Download link
      const xml = new XMLSerializer().serializeToString(svg);
      const href = URL.createObjectURL(new Blob([xml], { type: 'image/svg+xml' }));
      Ve.ById.demo_download.href = href;
      // Outputs
      Ve.forQuery("[data-output]", e => {
        let data = encoded;
        for (const key of e.dataset.output.split(".")) data = data[key];
        e.textContent = String(data);
      });
    }
    Ve.ById.demo_input.oninput = demo;
    Ve.ById.demo_debug.oninput = demo;
    Ve.ById.demo_margin.oninput = demo;
    Ve.forQuery('[name=demo_ecc]', (e) => e.oninput = demo);
    demo();
  });
</script>
<style>
  div[data-qr] > svg {
    display: block;
    margin: auto;
    max-width: 100%;
    max-height: 100%;
  }
  div.fit > svg {
    max-width: min(100%, 100svw);
    max-height: min(100%, 100svh);
    max-width: min(100%, 100svw);
    max-height: min(100%, 100svh);
  }
  div.smol > svg {
    max-width: 100px;
    max-height: 100px;
  }
  div.medium > svg {
    max-width: 300px;
    max-height: 300px;
  }
  div[data-qr][data-viewbox] {
    margin: 20px;
  }
</style>
<noscript><style>.scriptful { display: none }</style></noscript>

## Calculate connected components

Calculating the connected components is fairly straightforward, so I spent some time trying to optimize it.

Instead of operating cell-by-cell in the matrix, we process each row by detecting a run of black modules, which we will patch together to form the components.
A component maps which row this takes place on with all of the runs in the component, and links back to the accumulated component.

To match up entries from each row, we scan across the previous row and look for columns that overlap with the current one: if `[j_0, k_0]`{.js} is a run from the previous row, and `[j_1, k_1]`{.js} is the run we are currently looking at, then they overlap when `k_0 > j_1 && j_0 < k_1`{.js}.

When we find an overlap, we add this new run into the connected component.
However, it may also overlap with additional runs: if they do not already belong to the same connected component, we need to merge the connected components.

```javascript
MoreQR.matrix2components = function matrix2components(matrix) {
  const comps = []; // rows of horizontally connected components
  for (let i=0; i<matrix.getWidth(); i++) {
    comps.push([]);
    let l = 0;
    for (let j=0, k; j<matrix.getHeight(); j=k) {
      if (!matrix.get(i, j)) { k = j+1; continue }
      for (k=j; k<matrix.getHeight() && matrix.get(i, k); k++) {}
      const c = comps.at(-2); // previous row of horiz-connected components
      const len = c?.length;
      let f = {}; // found component, `f = { [i]: [[j,k,f], ...], ... }`
      // scan while the upper bound is less than the current lower bound
      while (l < len && c[l][1] <= j) l++;
      if (l < len && c[l][0] < k) {
        // grab the existing connected component
        f = c[l][2];
        // fuse it with other components that are also connected to `[j,k]`
        while (l+1 < len && c[l+1][0] < k) {
          l++;
          const f2 = c[l][2];
          if (f2 === f) continue;
          // Copy everything over and update the references
          for (const row in f2) {
            (f[row] = f[row] || []).push(...f2[row]);
            for (const h of f2[row]) h[2] = f;
          }
        }
      }
      const t = [j, k, f]; // lower, upper (half-open), reference to its connected component
      (f[i] = f[i] || []).push(t); // add it to the connected component
      comps.at(-1).push(t);
    }
  }
  // Gather all the unique components!
  const components = [...new Set(comps.flatMap(row => row.map(t => t[2])))];
  return components;
}
```

## Draw them

Once we have the modules of each component grouped together, we need to render it to an SVG element, using `<path d="..."/>`{.svg} and four of the [SVG path commands](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d#path_commands):

- `M` for move, to set the initial position
- `h` for relative horizontal movements
- `v` for relative vertical movements
- `Z` to close the path

Like the naïve approach, we will use SVG elements with fill and no stroke, and very conveniently, SVG provides a way to cut out the interiors of connected components^[so that we do not have to render white on top of the black modules to cancel them out – that just feels conceptually ugly]: the default [`fill-rule="nonzero"`{.svg}](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fill-rule) is sufficient to allow this cookie cutter approach.

For both horizontal and vertical movements, we *could* specify them as a bunch of single-module movements&nbsp;… but we might as well put in the effort to coalesce them, and this will generate a much smaller SVG source as well.

### Outer Path

The basic idea is that we will start at some point along the outside of the component, might as well be the leftmost module of the first row of the component.
And then we will walk around the outside of the shape, tracing the outline and recording the steps to draw it along the way: the first step is horizontal to the right, then we will turn right so that we are moving down, and proceed apace.

You can think of it as operating like those cute little line-following robots, that zip around a path, making decisions based upon whether they bump into the left or right of a line.
In this case, we want to be following a boundary between black and white modules, instead of a thin line.

A little bit of mathematical intuition (topology?) tells us that we will end up right back where we began, exactly when we finish the loop.
That is, connected components have nice perimeters, and operating in discrete space makes it really easy for us.

So thereʼs a little bit of logic to implement it correctly, but the idea is very simple.

:::Bonus
It turns out that the SVG representation of paths works very conveniently for this algorithm.

Sometimes there are weird edge cases and such when dealing with SVG paths.
Like sometimes you just have odd code to handle the first or last item in the array.
And *reversing* a path is a pain: you canʼt just reverse the commands, you have to know where they start and end to patch them (and deal with relative coordinates).

But here, since we are describing our algorithm as walking a path around the outside of the component, it corresponds quite nicely with the SVG draw commands which operate in that sense too.
:::

#### Walking variables

These variables track where we are (and which direction we are facing) as we walk along the perimeter.

```javascript
// `i` for the first index: row (a.k.a. `y` in the SVG)
let i = +Object.keys(component)[0];
// `j` for the second index: column (a.k.a. `x` in the SVG)
let j = component[i][0][0];
// `d` for direction: what direction are we moving in (+i, -i, +j, -j)
let d = [0,1];
```

Then for each step, we calculate some helper variables.

```javascript
// vertical or horizontal movement?
const v = +!!d[0];
// relative directions: `l` for left, `r` for right
const l = [-d[1],d[0]];
const r = [d[1],-d[0]];
```

#### Center vs perimeter

The fussiest detail is that we consider modules as an integer grid based on their top left coordinate, but we want to trace around their perimeter.
That is, the first module at `0,0` has a perimeter between `0,0` and `1,1`: as an SVG path, that is `<path d="M0,0 h1 v1 h-1 Z">`{.svg}.
When we are tracing along the bottom, from `1,1` back to `0,1`, we want to make sure we are still inspecting the module at `0,0` in determining where to go next (in this case, move `h-1`{.svg} and then turn the corner).

```javascript
// We want to inspect *this* point: because we are walking along the
// left edge of the outline, the center of the pixel we are
// inspecting is half a forward and half a right move away,
// and then we shift it by `-[0.5,0.5]` back to the integer grid
const c = [i+d[0]/2+r[0]/2-0.5, j+d[1]/2+r[1]/2-0.5];
```

#### Movement patterns

```javascript
// If the current cell is blank, we wrap around to the right to
// get back on track.
if (!grid[c]) {
  // Buffered line drawing
  if (i !== i0 || j !== j0) path += `hv`[v]+([j,i][v] - last);
  last = [i,j][v];
  // Turn to the right
  d = r;
// Otherwise we might want to swing out to the left, if there is a
// filled cell there.
} else if (grid[[c[0]+l[0], c[1]+l[1]]]) {
  // Buffered line drawing
  if (i !== i0 || j !== j0) path += `hv`[v]+([j,i][v] - last);
  last = [i,j][v];
  // Turn to the left
  d = l;
// Finally we proceed ahead. This is where we draw lines conceptually,
// although we buffer it so we are not stepping pixel by pixel.
} else {
  if (visited[[i,j,d]]) continue OUTER;
  visited[[i,j,d]] = true;
  const C = [i+d[0]/2+r[0]/2, j+d[1]/2+r[1]/2];
  i += d[0];
  j += d[1];
}
```

<div data-qr="demo" data-viewbox="0 0 7 7" class="smol"></div>

<div data-qr="https://webrtc-over-qr.veritates.love" data-viewbox="0 17 4 4" class="smol"></div>

### Inner Paths

This is where we get very lucky: we can reuse the same algorithm, and it even runs in the correct direction to make the default `fill-rule="nonzero"`{.svg} work! (Otherwise we would have to use `fill-rule="evenodd"`{.svg}, or reverse the algorithm for the inner paths.)

A sprinkle more of topological knowledge tells us that the connected component may have a series of holes punched out of it, but it is not infinitely nested: any further black modules inside will be considered a separate connected component.

So yeah, all that we really need to do is to find where holes start and trace around them like normal.

#### Detect holes

It is pretty simple to come up with spots that are missing from the interior of the connected component.

These arenʼt exactly the holes, though: several of them might belong to the same hole.

However!, since we are already walking the shape in order to turn it into a path, we just need to toss out duplicates: if we reach the same spot and direction along the way, weʼve already traced this hole and do not need to continue.

In the debug mode, we mark the candidate holes with circles.
You can see that most holes end up along outside edges, and in this example, the three holes that are on the inside edge are redundant with each other.

<div data-qr="https://webrtc-over-qr.veritates.love" data-viewbox="19 11 10 18" class="medium"></div>

```javascript
// We initialize the grid, indexed with `grid[[i,j]]`.
const grid = {};
// We also keep track of possible holes inside the connected component,
// which we need to punch out of the SVG path. This records places
// we should start looking for a hole, but these start points are
// likely to result in the same holes, or may even be on the exterior,
// where we were never going to draw anyways.
const holes = [];
// So to prune holes, we throw out a hole if we end up revisiting
// the same exact edge of the cell (i.e. same cell, same direction),
// since that is just retracing our steps at that point.
const visited = {};

// The connected component is of the form `{ [i]: [[j,k], ...], ... }`,
// where `i` is the row, `j` is the first column of a contiguous run,
// and `k` is past the last column of the run: that is, the cell of
// `grid[[i,j]]` is filled while `grid[[i,k]]` is blank.
//
// We also assume that its keys are sorted, since they are visited
// in order by the above algorithm at least.
const entries = Object.entries(component).map(([i,row]) => ([+i,row]));
for (const [i, row] of entries) {
  for (const run of row) {
    // Fill in the cells of the run, `run[0] <= j < run[1]`
    for (let j = run[0]; j < run[1]; j++) {
      grid[[i,j]] = true;
    }
    // Look for potential holes: this is easy, because it just means
    // that we have more than one run in the row.
    if (run !== row.at(-1)) {
      // Trim off the first and last rows for efficiency, since we
      // know they are exposed to the air, so to speak.
      if (row !== entries.at(0)[1] && row !== entries.at(-1)[1]) {
        // This is a location where the hole may start: ideally it is
        // the leftmost blank cell of the first row of the hole,
        // otherwise it will end up being a revisit of the same hole,
        // but we cannot really know that until we trace its contour.
        // And then `[1,0]` is the direction to move in.
        holes.push([i, run[1], [1,0]]);
      }
    }
  }
}

```

#### Draw holes

As mentioned, we can reuse the same drawing code for holes as for the outside contour.
So the changes to support holes is mostly structural in the code: wrap it in another loop, abort the loop if we encounter a visited path, and collate all of the paths together.

```javascript
const paths = [];
OUTER: for (let startPoint of [[i,j,d], ...holes]) {
  [i,j,d] = startPoint;
  let path = `M`+[j,i];
  // walk along the contour until we are back at the start
  do {
    const v, l, r, c; // ... described previously
    if (!grid[c]) {
      if (i !== i0 || j !== j0) path += `hv`[v]+([j,i][v] - last);
      last = [i,j][v];
      d = r;
    } else if (grid[[c[0]+l[0], c[1]+l[1]]]) {
      if (i !== i0 || j !== j0) path += `hv`[v]+([j,i][v] - last);
      last = [i,j][v];
      d = l;
    } else {
      if (visited[[i,j,d]]) continue OUTER;
      visited[[i,j,d]] = true;
      i += d[0];
      j += d[1];
    }
  } while (i !== i0 || j !== j0);
  // If we did not revisit an edge and thus call `continue OUTER`,
  // then this path is valid and we will append it to the paths.
  paths.push(path+`Z`);
}
return Ve.SVG.path({ attrs: { d: paths.join('') }});
```

## Finishing touches

To finish it out, we can compose it all together, using ZXing-JS to assemble the matrix that represents the QR code, turning it into a list of connected components, turning them into SVG paths, and plopping them in an SVG with a margin around it (“quiet zone”).

I additionally extract the XML, save it to a `blob:` URL, and render it as an `<img>`{.html} tag.
I think this makes it more efficient, but I donʼt really have any data to back that up, just a hope and a wish.
But it keeps some data and nodes out of the DOM and hopefully allows the browser to render it more statically, not interactive at all.

```javascript
// Better SVG rendering! On some systems, especially iOS, the SVG rendering is
// not particularly great, and so the QR code can end up glitchy, with white
// pixels rendered in between contiguous modules. So we fix this by rendering
// an entire connected component as a single path, so that the rendering engine
// does not dare put white pixels in between them. Furthermore, we stuff it
// into an `<img>` tag instead of the `<svg>` to optimize the DOM, rendering?
function MoreQR(data, ...params) {
  // Generate the QR matrix as normal
  const matrix = ZXing.QRCodeEncoder.encode(data, ...params).getMatrix();

  // Get a list of the connected components in the QR code.
  const components = MoreQR.matrix2components(matrix);
  // Now we transform each connected component into an SVG path node
  const paths = components.map(MoreQR.component2path);

  // SVG parameters
  // Account for the margin (quiet zone)
  const margin = 4;
  const viewBox = `${-margin} ${-margin} ${matrix.getWidth()+2*margin} ${matrix.getHeight()+2*margin}`;

  // Stick it into an `<svg>` node
  const svg = Ve.SVG.svg.qrcode(
    { attrs: { viewBox } },
    paths,
  );
  // Render it to XML
  const xml = new XMLSerializer().serializeToString(svg);
  // Turn it into a `blob:` URL (to be nice to the DOM, hopefully)
  const src = URL.createObjectURL(new Blob([xml], { type: 'image/svg+xml' }));

  // And render it as an `<img>` tag!
  return Ve.HTML.img.qrcode({ src });
}
```
