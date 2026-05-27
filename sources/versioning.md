---
title: Thoughts about versioning,
subtitle: \& config, \& builds,
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2026/01/12
---

I think versioning is one of the most important problems to deal with.

Not “how do you slap version numbers on things and where do you put/distribute them”, but what do you actually *do* with that information.
How do you deal with *change*?

Weʼve gotten better at dealing with change within a running system (as one example, [FRP]{t=}), but dealing with change over larger timescales with less coherent “systems” is still enormously underaddressed.

Whatever we can do to work in programming languages with sophisticated type systems and static analysis is going to be a boon for versioning systems.

And I think fixing versioning will reveal insight and lead to fixing other things, increasing the efficiency of systems (build systems and user installations), removing edge cases from upgrades, and decreasing frustration with broken software.

:::centered
*these thoughts provided without warranty of any kind*
:::

## Library/package versions

### Intentional breaking changes

To start off, I want to comment on a pattern I see in some ecosystems: libraries will mark breaking changes with type/name changes, so that code relying on old behavior will fail to compile after a breaking change.

That is, behavioral changes (which are otherwise silent) get reified as more substantial changes that the compiler can detect, to make users explicitly handle the behavior.
Of course this relies on users seeing the note about behavior while they are investigating why their code broke, which is not a guarantee.

It would also pair well with automatic code upgrading tools!
But alas, those are all too uncommon.

Unfortunately it introduces other issues too, by reducing compatibility between versions, coupling upstreams to consumers more closely, but thatʼs kind of unavoidable.
The question is what do you do to the ecosystem to make that palatable.
Commit to maintaining accurate version bounds.
Commit to being maintainable.

### Maybe libraries should have version bounds out of source

This brings me to an idea that Iʼve long had, that for packages that become unmaintained – for stable packages that donʼt *need* maintenance to their code – maybe their version bounds should live out of source.

## Practicalities of versions

### Reading version-specific info straight out of git

One of the key things I want to see happen is integration with version control systems (VCS) like git.
Like, one option is to keep all of the data about versions in the source tree, and track that in the VCS.
But all of that versioned data should already there in the history – the VCS already keeps track of it!
So it should be at least possible to look up versions in git, using [e.g.]{t=} `git show $ref:$file`{.sh}.

One example that I would like to track like this is schemas (of any form) – especially schemas that allow automatic migrations.
If you need to write out migrations manually, that will require a different story for source tracking.
But at least grabbing information out of git should be a good starting point.

### Levels of versions

But this raises the question of what are versions you care about.
Release versions are the most obvious, and prerelease versions.
They have official release processes.

What about every commit in the repository?
Well, every commit that affects what you are versioning at least.

And then you have the trickiest, which is ephemeral versions, the “working tree” as youʼre developing – and presumably creating and fixing bugs all the while.

Are your tools good enough to handle all of that chaos?
Youʼll need static checking (syntax checking, type checking, maybe semantics!), fine-grained flexibility, and automation to keep on top of it – after all, you donʼt want to be writing database migrations every time you change a line of SQL in your code.

### Who versions the versioner

Finally, as the last major practicality, itʼs worth thinking about how there are layers of versioners too.
Think about bootstrapping compilers or build tools for programming languages.
They often can be built with their previous version, but a version from a year ago? Less likely.

Similarly, for build workspaces if you upgrade your tools (or even just your Makefile), you may have problems with build tools that do not detect that things have changed and need rebuilding because nothing in the source changed, only things in the environment.

Or you might introduce a discontinuity that leaves side effects when you run, say, `make clean`{.sh} with your _new_ tools on a workspace created by the old tools.

This may be as benign as leaking disk space, if directories do not get cleaned up, but it can be more serious:
if the management of that directory changed ([e.g.]{t=} from being generated to being versioned), it could result in a build or merge error until it is manually cleaned up, or reverted, cleaned, and fast forwarded again.

One option for this would be to have build commands write the cleanup command in a more stable format, like its own Makefile that lives in a gitignored file, or even a shell script or executable.

```makefile
.PHONY : build-thingy
build-thingy : .Makefile.clean-thingy
	# Normal build steps
	echo "Do your things here"
# But first we copy the current build file
# to a gitignored location, so that it
# persists across git checkouts
.Makefile.clean-thingy : Makefile
	@cp Makefile .Makefile.clean-thingy

.PHONY : clean-thingy clean-thingy-impl
clean-thingy :
	# Reference the *old* Makefile to run
	# cleanup for the old build!
	@(test -f .Makefile.clean-thingy && make -f .Makefile.clean-thingy clean-thingy-impl)
	# You could also run the current
	# clean-thingy-impl, to catch
	# cleanup steps you add, I guess
	@(not diff -q Makefile .Makefile.clean-thingy >/dev/null && make clean-thingy-impl)
# The actual cleanup implementation
clean-thingy-impl :
	echo "Remove your build files here"
```

This seems like it would reduce edge cases, but still not eliminate them.

## Examples

### [API]{t=} surface of programs

For example, if we werenʼt using *JavaScript* (derisive) and were actually able to analyze their exact [API]{t=} surface, Electron apps probably would have less reason to be tied to particular Electron versions, besides bugfixes and stuff.
This could allow apps to share Electron versions and reduce their size, ideally.

Thereʼs no reason that the [API]{t=} surface between programs and their runtime framework shouldnʼt be analyzable, itʼs just that dynamic languages like JavaScript are a terrible tool for that.
[WASM]{t=} has a chance at being better for this, because it was developed from a security-first sandbox.
But it is constrained by lots of legacy formats on the flipside, so, letʼs just say that we are not there yet in any meaningful capacity.

### Dhall is too strict, *but*

Dhall has a really strict type system.
Even though this strictness and explicitness is great for pinning down semantics (and for ensuring that programs terminate),
Dhall suffers from a lack of abstraction to make typing flexible enough to really be usable.

Most notably it lacks row types to abstract over shapes of records, which I tried to address at one point.
As an example, there are many record types `t`{.dhall} such that a value `v : t`{.dhall} is well-typed in the program `v.field : Integer`{.dhall}.
But there is no way to write a function `f`{.dhall} to modify those values `v`{.dhall} that will work for all of their possible types `t`{.dhall}, in the sense of allowing `(f v).field : Integer`{.dhall} to always typecheck when `v.field : Integer`{.dhall} did.

Notably this is less of a problem for the host language you embed it into.
It is no problem to accept extra-wide records and extract just the fields of interest, or accept extra-narrow unions and expand to include the variants you expected.

So you can write your config in Dhall, type check it, and likely plug it into a program that consumes the config.
But you cannot write meaningful processing on this config type *in Dhall*, without committing to a very concrete type for the configuration and giving up the ability to have the few kinds of soft migrations that Dhall would allow.
(Namely, adding variants to unions could be a minor version bump, and dually, _removing_ fields from records (not adding them! there would be no way to add defaults to fields without extra support in the host language).)

(Btw: the point isnʼt really about Dhall here, it is just a particularly strict example in a way that JSON is not, even with schemas and TypeScript, because virtually all of those tools will silently ignore “extraneous” record fields, and missing variants are literally undetectable. But as a stricter example, Lean has started using Lean as a configuration language for build systems – how do they handle versioning like this, across types??)

#### Nicely typed migrations

Back in, say, 2018, I really thought about addressing this, but I never had enough reason/motivation to really try it out.

But thereʼs something that Dhall gets right in being so strict, that provides a good model for **good** config for other domains and applications, languages and type systems.

------

Alright, so. Letʼs imagine that you did commit to a strict configuration type per version, what would you need to make it tolerable?

Well, you would want to have explicit migrations between the config types of each version, and then you need a way to apply them as necessary to actually.
Each migration would just be a Dhall function from the old type to the new type,
possibly taking in additional data just for the migration, data that it could prompt the user for.

You could let the tool automatically apply migrations that follow the expected rules of “coercions”: dropping a record field or adding a union variant.
You could even use your knowledge of variance in category theory: these automatic migrations could apply in reverse contravariantly (which is something that naïve host integration would not handle very well!), and they could apply transparently through covariant functors like lists, of course.
Things like that!
It could even fill in new `Optional`{.dhall} fields with `None`{.dhall} by default, list-typed fields with empty lists.

:::Bonus
This is part of why I want tmTTmt to have the type system that it does: to support all of these coercions as first-class concepts, and not require weird workarounds.
:::

When that fails, you could always write your own migration functions.
Filling in _new_ fields with explicit defaults ([e.g.]{t=} if you make the port configurable now, youʼll want to include `port = 8080`{.dhall} by default in older configs).
And because you have access to the whole config at once, you can potentially fill in defaults based on other fields of the config – [e.g.]{t=} making sure it is an unused port number, or defaulting a file location based upon its default location relative to other directories in the config, or stuff like that.

#### Category theoretic versioning

Finally, we need to ask ourselves how these migrations should work together: how should they compose, and how should they cohere?

Well, it is pretty obvious that we want explicit migrations to take precedence over coercions-as-implicit-migrations.

Then we want to be able to chain migrations – ahem, *compose* migrations to form a category.
If the config can migrate from v1.0.0 to v1.0.1 and v1.0.1 to v1.1.1, then you can form the migration from v1.0.0 to v1.1.1 via composition.^[If these migrations take in side data, it will need to take in the side data for both.]

Next, you want to prefer the largest migrations – say that v1.0.1 → v1.1.1 removed a field, but v2.0.0 reintroduced that field again with a default value in the migration.
You could build a migration from v1.0.1 → v2.0.0 that preserves the field instead of inserting the default value.
So it should prefer that larger migration instead of composing the smaller migrations.

Finally, we want these migrations to cohere, mostly.
No matter which path we take to migrate between versions, it should hopefully result in the same config.
This is called a [_thin category_](https://ncatlab.org/nlab/show/thin+category) or _posetal category_, as it resembles a poset: the endpoints determine the morphism, all paths between those points have equal compositions.
As versions already form a poset, migrations will be minimally surprising when they form a poset too.

(This is not to say that the morphisms do not contain information: the migrations are important! But the particular choice of how to migrate when there are several paths available would not matter.)

Unfortunately the real world is probably not this clean, and having additional side-data during the migrations makes things harder to state/prove^[I guess it would be like, “there exists some side data that makes both paths equal”? “most side data can result in an equal config”?], but thatʼs how we should strive to make it coherent.

:::Bonus
And to tie it in with earlier ideas, these schemas could live in a `ConfigType.dhall` file and be implicitly tracked in the version history of the Git repository.
However, the migrations would need to be explicitly enumerated – maybe the set of supported migrations would be maintained actively in the tree, and old migrations dropped when they are no longer supported/available by default.
:::

#### Digressions about config

Layers of interpreting:

- User intent (the most important part, but ineffable, alas)
- Writing it in a config file
- Resolving the config file syntax to semantic config
- Implementing the config to choose codepaths or numbers or such

Composition of config (layers of intent):

- Program defaults
- Global defaults
- User defaults
- Repository config (in git)
- Local overrides (not in git)
- Temporary/situation-specific overrides

### Database migrations

I donʼt really use databases, SQLʼs data model is awful, a far cry from the cleanness of Dhallʼs immutable pure typed data.

But yeah, you would want to think of database migrations in this kind of model too.

One thing that pure config formats do not handle (well) is object identity.
You usually need to insert (U)UIDs.
Database primary keys.
So on.

I guess a tool could rewrite the config file to add UIDs when they are instantiated (think how harddrive partitions are sometimes referenced by UUIDs instead of names or partition numbers), or maintain a lockfile to map names to UIDs (which is messy in its own way).
But you need _something_ stable to key data over time, maintain object identity.

On the other hand, using UUIDs makes diffing hard, for tests and repeatability.

Just like you wouldnʼt want raw pointers to show up when you are [pickling](pickling.html) data: you want to serialize to a (more) deterministic type like integer backrefs into the structure ([e.g.]{t=} implementations of the JavaScript structured clone algorithm), when then do become rehydrated pointers in a new runtime context when deserialized.

### Front end, back end

One of the problems of front end back end is that the client may be running a different version than the server.

Dhall-style category theoretic versioning may help out a little bit, but it doesnʼt come for free.
Either the client needs to be preloaded with a mechanism (and awareness) to download version migrations from a specific place, or the server needs to rewrite requests and responses from the client to mitigate versions.
Additionally, since data flows both ways, the migrations are much more complicated then – round tripping may even require looking at the original data on the other side of the request/response flow.
([E.g.]{t=} if you change IDs from a number to a string, you could generate numbers for the strings, but need to look at how to map them back. Or other additional data along for the ride like that.)

Worst case you need to run two versions of the backend at once, and maybe that really means youʼve ended up with three versions: the v1 backend (operating on v1 database) migrating to the v2 database with its updated v2 backend for the v2 client and the updated v1-backend-on-v2-database for the v1 client.

### Build systems

Ingredients of building software:

- Version control (donʼt leave home without it)
	- Local tweaks
	- Work in progress
- Environmental dependencies (build tools, [etc.]{t=})
- Package manager for dependencies on libraries
- Build orchestrator
- Code generators and compilers

Directory and file layout.
In source vs out of source builds.
Temporary build directories.
Generated source files.
Hygienic builds.
Incrementalization.
Cache.

Itʼs not enough to have this information.
You have to know how it changes over time – mutably and immutably –, who is responsible for it, what goes into it, how it relates to everything around it.

### Live-reloading frontend dev

Ah yes, the mythical live-reloading front-end development experience.

Letʼs start with a simplifying assumption: state is either stored in one central place (à la React Redux) or in the DOM (or the state of the browser, more generally).

This state should be typed, otherwise it is unknown if the old state is compatible with the new state, or how to migrate (automatically or manually).

But if it is typed, we can know statically at compile time whether it is unchanged.
If it has changed,

- We can make a migration codec that 
- Writing this as a function from the old data to new data is problematic.

The point is, that we can make assumptions (regarding names and types) to make hot reload happen in some cases.
The need for rapid feedback cycles mean that 
