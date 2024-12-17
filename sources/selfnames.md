---
title: Self-names (paths)
subtitle: The other hardest problem in programming?
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2024/12/16
---

> [There are 2 hard problems in computer science: cache invalidation, naming things, and off-by-1 errors.](https://martinfowler.com/bliki/TwoHardThings.html)
> <!-- https://twitter.com/secretGeek/status/7269997868 -->

You know, Iʼve always heard this and been like, “Yeah, naming functions and variables is difficult, language and communication are difficult, I guess Iʼll just call it `Create​Puppeteer​And​Chromium​Instance​Or​Testing​Stub​Factory` and call it a day”^[Fictional example, no Iʼve never done that.].

However, I think the stronger problem is that we expect to be able to reuse names (*paths) in contexts other than they were made in. We need to!

## Selfnames in webdev with reverse proxies

Reverse proxies (like nginx) are a particularly compelling example of this that I keep running into: they can change the public URL of something, in a way that the code often needs to know about, and there isnʼt always a great way to tell the frontend code about.

For work Iʼve been working on packaging our multiple layers of products into a single deployment, thus deployed on a single HTTPS port but with different root paths for each software. Getting it to work through the reverse proxies was a bit of a pain (and is still not perfectly finished: some paths have to redirect like `/B/*` -> `/A/B/*`).

For my blog, Iʼve kept the directory structure very predictable so far so it doesnʼt matter where it is hosted: there are no absolute references, even to `assets/` and so on.
But if I want to have articles that are nested under directories, either I need to add a `../` to all of these relative URLs, or I need to commit to having it hosted at the root of the domain.

There are some ways to work around this … unfortunately [`<base>`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/base) is a particularly bad one, since it cannot leave relative URLs unaffected, for example.
(It would be great if it was able to just rename absolute URLs that start with a slash, for example.)

Idk, you basically just have to commit to caring about it in *some* place, passing along information to handle it where you can (and hopefully not break down too many abstraction boundaries).

## Selfnames in programming languages (modules?)

When developing [verity.js](https://blog.veritates.love/assets/js/verity.js) I decided that I prefer the code to be *copyable*, not convenient to write.
So I committed to always writing out the `Ve.`{.js} prefix so that I was able to copy the source code out and run it from outside the module, .

But to some extent this *should* be a tooling problem, *were* we to live in the alternate reality where we treated programs as structured syntax, not plain text.

Like, in theory, if you were to copy code out of one Haskell or PureScript (not JavaScript) module into another, it would be able to re-qualify all of the identifiers and add imports for you, possibly throwing an error *only* if the code was using local identifiers that were not exported (though it could prompt you with a button to add exports for them).

But the hegemonic tools we are forced to use do not understand this fine distinction.
