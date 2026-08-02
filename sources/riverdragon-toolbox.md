---
title: Riverdragonʼs Toolbox
subtitle: Riverdragon FRP Documentation (2)
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2025/02/22
---

An overview and orientation of the toolbox (types, combinators, and functions) that Riverdragon gives you.

`Riverdragon.River`{.ps} is, of course, the [FRP]{t=} `Stream`{.ps} implementation.

`Riverdragon.Dragon`{.ps} is the [VDOM]{t=} renderer.
It technically is a Virtual [DOM]{t=}: `Dragon`{.ps} is just an [ADT]{t=} with some options for what to render.
But it is pretty lightweight, it has no diffing.
(Technically you could implement some diffing logic? But it just shouldnʼt be necessary.)

`Riverdragon.Roar`{.ps} is the interface to Web Audio, for making synthesizers and such.
It also includes some [MIDI]{t=} stuff.

`Parser.Comb.Comber`{.ps} is a complete solution to selective applicative [LR(1)]{t=} parser combinators in PureScript.

`import Dodo as T`{.ps} is a pretty printer that may be of use. (Eventually it will all be integrated with the parser to make bidirectional parser-printers in `Parser.Printer`{.ps}.)
