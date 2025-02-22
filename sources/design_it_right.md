---
title: Design your programming languages right!
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2024/12/16
---

I just think we should have some ground rules for new programming languages and their configuration.
Yʼknow?

Just some rambly thoughts on features I think you should include, or at least think about and not block yourself *out of* including.
And other topics I am thinking about with no real answers yet.

## Solved problems / non-issues

### Indented multiline strings!

Itʼs so ugly when you go to embed a multiline string into your code, and you realize that you're going to have to close your eyes and pretend the current indentation level does not exist, as the raw contents take over the left gutter.

This is a solved problem.
[Dhall](https://github.com/dhall-lang/dhall-lang/blob/master/standard/multiline.md) did a pretty good job of it (although I am not too fond of the escaping rules there), and [Nix/Lix](https://docs.lix.systems/manual/lix/stable/language/values.html#type-string) have similar rules.

I implemented similar logic for JavaScript template literals in [Ve.dedent](https://blog.veritates.love/assets/js/verity.js), I am very happy with it.
And Iʼve tried my best for PureScript (and Python?), but not having the distinction between source and escaped/interpolated texts makes it much harder, and more of a tool that works for some cases than a systematic approach that it could be if it was baked in.

Indented multiline strings are just a thing that you can put a few extra hours of effort into fixing early, before it becomes a problem for your users and then you canʼt change without backwards compatibility issues.

### Relative file paths

File paths should be relative to the file theyʼre in, or to a well defined project root.
This mostly applies to configuration, less so running programs (for which a working directory is usually fine), but like&nbsp;… imports should be predictable.

Notably, configuration files for both [Docker Compose](https://docs.docker.com/compose/how-tos/multiple-compose-files/extends/#understanding-multiple-compose-files) and [Rustʼs Cargo](https://doc.rust-lang.org/cargo/reference/config.html#config-relative-paths) get this wrong.

Again, you have to get it right up-front, or figure out solid plans for versioning.
(But of course, programming languages that donʼt have the thought put in to get it right up front, usually donʼt have good enough versioning policies to deal with this either.)

### File extensions

Pick an extension or two, and stick to it!
Remember that if youʼre a new language, youʼre a guest entering existing ecosystems by their good grace and should work on their terms.

Syntax highlighting, in IDEs and online code viewers/git forges, is mainly based on the file extension.
And if you donʼt have a recognizable file extension, your users are going to have to work harder.

As one example, Bazel uses Starlark in files named `WORKSPACE` and `BUILD` and `.bzl` or `.bazel`.
Starlark just has Python syntax highlighting, which is nice that you can use that.

But itʼs hard to convince your IDE to highlight them if thereʼs different extensions and extensionless file names.
(Itʼs even harder if these names are configurable, but I donʼt think `WORKSPACE` and `BUILD` are?)
Itʼs harder to `grep` through the files if you have to include all the forms that it can occur.
And so on.

### Commas

Another solved problem: allow trailing commas! (And possibly leading commas, if you feel like that&nbsp;… or actual bullet points.)

### Extensible syntax

A lot of languages have made mistakes around numeric literals (wanting to add more prefixes or suffixes, but the path forward is blocked by backwards compatibility), or string/regex escapes (use delimited escapes like `"\u{XXXX}"`{.js}!), or keywords.

Itʼs more my personal opinion, but I dislike bare keywords. I think keywords should come with a sigil to clearly denote them as a keyword and allow extending the language with more keywords without breaking old code that used them as identifiers.
Do you *really* think youʼll get the perfect set of keywords right the first time?
… can you point to a programming language that did?


## (Mostly) unsolved, nuanced problems

### Developer experience vs Released code

iterating on code as youʼre developing it, versus having a nice clean, pristine released version&nbsp;… theyʼre quite different things, and I think these workflows should be respected!

e.g. when developing, we often put web servers on different ports and/or LAN hosts, and use HTTP or self-signed HTTPS (or even `file:///` in the few cases we can get away with it!), whereas in production it might be behind a reverse proxy under some specific route, and so on.

### Versioning

Versioning means a lot of things.

One of the problems that I havenʼt really seen *solved* before, is how to deal with code *across* versions.

Like, letʼs say that I want to have a migration script between versions of my released software.
Well, I guess the code for it has to live in the later version – itʼs not like the previous version knew what was coming up.
But how do you test it? You kind of need to check out the previous version of your code from source control, and have them both in parallel.
(This is also something where the “development” and “release” modes are pretty separate!)

And in general, if youʼre expecting people to write libraries in your programming language, give them a sensible way to talk about versions.
One of the cooler tools in this area are the ones that check a libraryʼs API (especially if it is typed!) to determine whether it should be a patch/minor/major release.

*However*, I must stress: version numbers are always going to be subject to judgment calls.
They are an imperfect tool of communication, not a mathematically precise abstraction.

<hr/>

Oh another hobby horse:
Please always include dates with your release version numbers!
It doesnʼt have to be *in* the version number, but it should be easily accessible, displayed beside it or in a tooltip or on a canonical webpage.
Itʼs nearly impossible to compare versions *across* software (languages, runtimes, packages, dependencies) without having a sense of release dates of it all.

### File watching

This is part of a larger discussion around build systems and the like, but.

Iʼve never found it possible to retrofit detailed file watching onto build systems that didnʼt have it.
Without, like, writing a whole Python program to go and parse files, reconstruct their dependency tree, make sure I know how each file is getting used and if it is relevant at all, reload the whole watcher if the configuration changes, and so on.

### Integration with tools

Like `find`/`grep`: itʼs much nicer if you can ask your build tool to tell you all the files to search through, and then you can ask if you want to include dependencies or not.
(Also this listing of files/globs gives you a first step towards filewatching too, although (again) you really need proper integration to be able to account for added files, and changing configuration, and so on!)

Itʼs even better if you want to add proper code search, being able to distinguish your search terms in identifiers versus strings, or in types versus terms, and so on. Yeah \^.\^
