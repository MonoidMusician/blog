// More QR utilities
"use strict";

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

MoreQR.component2path = function component2path(component) {
  let debug = MoreQR.debug;
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
  // `i` for the first index: row (a.k.a. `y` in the SVG)
  let i = +Object.keys(component)[0];
  // `j` for the second index: column (a.k.a. `x` in the SVG)
  let j = component[i][0][0];
  // `d` for direction: what direction are we moving in (+i, -i, +j, -j)
  let d = [0,1];
  // relative directions: `l` for left, `r` for right
  // and `c` marks the center of the current move
  let l, r, c;
  const paths = [];
  const debugPaths = [];
  OUTER: for (let startPoint of [[i,j,d], ...holes]) {
    [i,j,d] = startPoint;
    let path = `M`+[j,i];
    const i0 = i, j0 = j;
    // last coordinate along the direction we are moving
    let last = d[0] ? i : j;
    // walk along the contour until we are back at the start
    do {
      const v = +!!d[0]; // vertical or horizontal movement?
      l = [-d[1],d[0]];
      r = [d[1],-d[0]];
      // We want to inspect *this* point: because we are walking along the
      // left edge of the outline, the center of the pixel we are
      // inspecting is half a forward and half a right move away,
      // and then we shift it by `-[0.5,0.5]` back to the integer grid
      c = [i+d[0]/2+r[0]/2-0.5, j+d[1]/2+r[1]/2-0.5];
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
        debug && debugPaths.push(
          startPoint[2][0]
          ? `M${[j,i]} Q${[j+d[1]/2+r[1]/2, i+d[0]/2+r[0]/2].map(x=>x.toFixed(2))} ${[j+d[1],i+d[0]]} Z`
          : `M${[j,i]} L${[j+d[1]/2+r[1]/3, i+d[0]/2+r[0]/3].map(x=>x.toFixed(2))} L${[j+d[1],i+d[0]]} Z`
        );
        i += d[0];
        j += d[1];
      }
      // An alternate scheme would be to just check for
      //     grid[[c[0]+l[0], c[1]+l[1]]] ? l : grid[c] ? d : r
      // But that has a different notion of boundary: it includes diagonal
      // jumps, so to speak (8-connected not 4-connected).
      //
      // > First we detect if we should swing out to the left, if there is
      // > a pixel there, so we can stay along the left edge of the outline.
      // >
      // > Then we detect if we are still on a pixel, so we can move forward
      // > to its next corner. This is where we draw lines conceptually,
      // > although we buffer it so we are not stepping pixel by pixel.
      // >
      // > Otherwise we stay on the same pixel (`c` does not change) but
      // > pivot to the right, to catch the contour closing in on itself
      // > or meandering around.
    } while (i !== i0 || j !== j0);
    // If we did not revisit an edge and thus call `continue OUTER`,
    // then this path is valid and we will append it to the paths.
    paths.push(path+`Z`);
  }
  const rand = Math.floor(Math.random()*310);
  const debugColor = `lch(60 80 ${rand+50*(rand>60)})`;
  const result = [
    Ve.SVG.path({ attrs: { d: paths.join('') }}),
    debugPaths.map(d => Ve.SVG.path({ attrs: { d }, style: { fill: debugColor } })),
    !debug ? [] : holes.map(([i,j]) => Ve.SVG.circle({ attrs: { cx: j+0.5, cy: i+0.5, r: 0.25 }, style: { fill: debugColor }})),
  ];
  if (debug) return result;
  return result[0];
}

MoreQR.ECC32 = function ECC32(f) {
  if (f instanceof Uint8Array || f instanceof Array)
    f = Array.from(f, x=>Number(x).toString(16).padStart(2,'0')).join(':');
  let s,d,a,z,i,r,u,k,Z,t
  const {abs:B,max:X} = Math
  t=(x,y=x)=>X(B(x-i),B(y-j))
  s='420'+f.replace(/:/g,'')+'0'
  d=[]
  a=[u=1];z=[1/0]
  for(i=0;d[i/2]="0x"+s[i]+s[++i],i<511;)
    z[a[i]=u=2*u^(u>127)*285]??=i
  r=[...d]
  for(k=35;--k;)
    for(Z=+r[i=0];i<34;)
      r[i]=a['\xFBC.=vF@^ -'.charCodeAt(i)+z[Z]]^r[++i]
  return r.slice(0,10);
}
