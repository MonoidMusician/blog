---
title: Weather the Storm
subtitle: What code can you write that assumes nothing about the state of the VM?
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

Take a runtime execution environment, like a JavaScript VM or Python or whatever.
The key is that it is a janky language!

Now imagine you have a malicious actor who gets to run code in your execution environment and do whatever they want:
mutate global variables off of `window`{.js}, delete or replace methods on global object prototypes, install event handlers designed to mess you up, and so on.

(They could lock up the VM by going into an infinite loop, but that isnʼt interesting.)

What code can you write that is resistant to this?
What functions are safe?

------

The purpose of this thought experiment is to reveal how much power of a lanugage (that you use every day!) comes from unstated assumptions made about the runtime – assupmtions that cannot be made by, [e.g.]{t=}, optimizers.

I guess it is kind of like reverse mathematics?

## JavaScript

Pretty much all of the globals accessible from `window`{.js} are out.
Some are read-only, but not transitively.
See this note from [crypto global property](https://developer.mozilla.org/en-US/docs/Web/API/crypto_property):

> Although the property itself is read-only, all of its methods (and the methods of its child object, `SubtleCrypto`) are not read-only, and therefore vulnerable to attack by polyfill.
>
> Although `crypto` is available on all windows, the returned `Crypto` object only has one usable feature in insecure contexts: the `getRandomValues()` method. In general, you should use this API only in secure contexts.

You canʼt assume you get access to `Proxy`{.js}.
You canʼt even assume you get access to any of the nice APIs from `Object`{.js}.

So whatʼs safe?

- Arithmetic on numbers should be safe.
- Is string indexing safe? Probably.
- Properties that you know you added to objects via builtin syntax is probably okay.
- Idk, Iʼm not going to think too hard about this right now.

## Python

?

## Erlang

Just because Iʼve been thinking a lot about Erlang recently:

Erlang BIFs (built-in functions) are okay to use.
I think this extends to everything in the `erlang:`{.erlang} module.

Same-module functions are okay (if they are not qualified!), but other modules are a no-go.
Other modules can be reloaded at any time.

In general, you get access to a lot more of the language safely in Erlang, especially since data is purely functional and well-specified.
