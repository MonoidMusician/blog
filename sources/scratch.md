---
title: Scratch
---


```
main = number .
number = digit | number digit .
digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" .
```

```{.no-visible-space}
main:
    │├── number ──┤│

number:
    │├──╮─────── digit ───────╭──┤│
        │                     │
        ╰── number ── digit ──╯

digit:
    │├──╮── "0" ──╭──┤│
        │         │
        │    :    │
        │         │
        ╰── "9" ──╯
```

<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' width='320' height='420' class="kgt">
  <style>
    rect, line, path { stroke-width: 1.5px; stroke: black; fill: transparent; }
    rect, line, path { stroke-linecap: square; stroke-linejoin: rounded; }
    path { fill: transparent; }
    text.literal { font-family: monospace; }
    line.ellipsis { stroke-dasharray: 1 3.5; }
    tspan.hex { font-family: monospace; font-size: 90%; }
    path.arrow { fill: black; }
  </style>
  <g transform='translate(40 50)'>
    <text x='-30' y='-10'>main:</text>
    <path d='M5.5 14 v12 m 4 0 v-12' class='station'/>
    <path d='M120.5 14 v12 m 4 0 v-12' class='station'/>
    <rect x='30' y='10' height='20' width='70' rx='0' ry='0' class='rule'/>
    <text x='65' y='25' text-anchor='middle' class='rule'>number</text>
    <path d='M100 20 h20'/>
    <path d='M10 20 h20'/>
  </g>
  <g transform='translate(40 130)'>
    <text x='-30' y='-10'>number:</text>
    <path d='M5.5 14 v12 m 4 0 v-12' class='station'/>
    <path d='M250.5 14 v12 m 4 0 v-12' class='station'/>
    <rect x='105' y='10' height='20' width='50' rx='0' ry='0' class='rule'/>
    <text x='130' y='25' text-anchor='middle' class='rule'>digit</text>
    <rect x='60' y='40' height='20' width='70' rx='0' ry='0' class='rule'/>
    <text x='95' y='55' text-anchor='middle' class='rule'>number</text>
    <rect x='150' y='40' height='20' width='50' rx='0' ry='0' class='rule'/>
    <text x='175' y='55' text-anchor='middle' class='rule'>digit</text>
    <path d='M220 30 v10'/>
    <path d='M200 50 h10 q10 0 10 -10'/>
    <path d='M130 50 h20'/>
    <path d='M40 40 q0 10 10 10 h10'/>
    <path d='M220 30 q0 -10 10 -10'/>
    <path d='M155 20 h95'/>
    <path d='M30 20 q10 0 10 10 v10'/>
    <path d='M10 20 h95'/>
  </g>
  <g transform='translate(40 240)'>
    <text x='-30' y='-10'>digit:</text>
    <path d='M5.5 14 v12 m 4 0 v-12' class='station'/>
    <path d='M130.5 14 v12 m 4 0 v-12' class='station'/>
    <rect x='60' y='10' height='20' width='20' rx='8' ry='8' class='literal'/>
    <text x='70' y='25' text-anchor='middle' class='literal'>0</text>
    <line x1='70' y1='35' x2='70' y2='45' class='ellipsis'/>    <rect x='60' y='50' height='20' width='20' rx='8' ry='8' class='literal'/>
    <text x='70' y='65' text-anchor='middle' class='literal'>9</text>
    <path d='M100 30 v20'/>
    <path d='M80 60 h10 q10 0 10 -10'/>
    <path d='M40 50 q0 10 10 10 h10'/>
    <path d='M100 30 q0 -10 10 -10'/>
    <path d='M80 20 h50'/>
    <path d='M30 20 q10 0 10 10 v20'/>
    <path d='M10 20 h50'/>
  </g>
</svg>
