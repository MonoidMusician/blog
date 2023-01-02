---
title: We Were Lied to About Computer Technology
subtitle: Programming is not “the future now” – at least, not yet
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

Computer technology seems like the future – from the semiconductor revolution to the birth of the modern smartphone, technology has raced ahead, quickly transforming the science fiction into the science reality.

**And yet**, it is hardly revolutionary at all. Instead mired in reactionary and regressive norms.
(Okayyy, maybe it is the teeniest tiniest bit revolutionary, but overall itʼs a sad state of affairs.)

I think there are a couple main reasons why we got to this point:

1. Capitalism
2. Corporate and startup culture stifle open development, collaboration, and interoperation, especially with intellectual property rights/patent abuse (aka capitalism)
3. Computer science is a young field, which means that, for example, teaching programming is not a smooth process
4. Ironically, programming practices are also stuck in the past, loaded with historical baggage and actively ignoring solutions to solved problems
5. where are the hackers when you need them
6. Lack of community (also capitalism)

A lot of these relate to broader social issues that I am not going to comment in any comprehensive manner.

Instead, Iʼd like to share my perspective as a programmer, since my perspective lets me comment on three main areas:

1. My amazement^[In a literal sense of the word.] at society, as a programmer
2. Why coding is so painful, even for someone as deep into software development as I am
3. Why todayʼs consumer software is a sham and why you are impacted by this every day and not getting the software you deserve
4. Also AI is bad and not doing what you think it is doing

Basically, why the computer technology ecosystem is so lackluster and 2-dimensional.
(Of course, thereʼs shiny consumerism slapped on top of every technology product ever produced, but once you take off the 3D holographic glasses you see its rotting foundations.)

## How I came to write this

Iʼd like to start by sharing the story of what led me to write this article.

It starts with amazement.
Iʼve always been amazed at the world.
The size of the world.
The bewildering variety and uncanny homogeneity of the world.
So many people going about their lives, and all the stuff it takes.
Not even just the stuff, the physical stuff you interact with, but the systems for producing and coordinating that stuff.
Logistics, supply chains (rip), production and consumption.

Like, I just think about roads – how much work it takes to lay new roads, pave over and repair existing roads, to design and specify the shapes, materials, timings, and behaviors of the roads and traffic control devices.
How we travel on them every day, without thinking.^[Especially in the U.S., which has next to no public transportation infrastructure and thus makes surviving without a car next to impossible.]
How you can spend days travelling along the same road, driving from sun up to sun down, and through the night.
Through wind, rain, construction work, up hills and around curves, chasing the horizon as far as you can.

For a while, Iʼve attributed this amazement to being young, naïve.
Just beginning to learn how the world works.
To being autistic and thinking too deeply about things I shouldnʼt^[Just kidding, thinking deeply about things is literally the point of life.].
Mistakenly thinking everything must be solved from first principles.

In particular, Iʼve never thought about it as an amazement caused by being a programmer.
If anything, being a programmer should help me believe in the world, help me make better sense of it, no?
It should not overwhelm me more, it should not, yeah?

Iʼm a programmer, Iʼm totally used to immersing myself in challenging problems, complicated systems, and intricate tradeoffs and using all my skills and intellect to figure out comprehensive solutions!
I know how to _engineer_ all manner of things, why would I be amazed at all the things that exist outside of computer technology.
Computer technology is the most advanced field after all, isnʼt it?
_Isnʼt it?!?_

Well, it turns out that was wrong.
As Iʼve been reflecting on recently (particularly as I start working in tech), the tech industry is deeply broken .

Like, there are certainly a lot of things to say about worker exploitation, the marginalization of publically-recognized-yet-systematically-ignored “essential” workers, outsourcing and global trade disparities and racism and colonialism.
These are part of the reason why the world outside of tech is horrible and awful, and they are reflected in and exacerbated in tech (and exacerbated _by_ tech, with the negative impacts of tech reflecting back $n$-fold).

But the computer technology industry was somehow supposed to rise above this, to point the way to the future and provide a way out of our current circumstances.

It has failed spectacularly.
Irredeemably? Not quite irredeemably.
But we need to put a lot of honest, genuine, insightful work in before we can have something we are proud of.

## Being a developer is hard (and it really doesnʼt need to be?? we can do so much better!)

Developer experience basically sucks across the board.

Shells/terminals suck.
The very idea of shells mostly sucks and gets in the way.
Terrible to teach/learn.

Editors suck.
The basic idea of editing text is flawed, but workable.
Implementations, however, are disastrous.

Languages suck.
Granted – this one Iʼm going to say is a very hard problem.
(Of course I say that, I am a programming language designer.)
But thereʼs a few issues:

1. We arenʼt making it any easier on ourselves
2. Mainstream languages love to ignore everything weʼve learned in computer science
3. Uhh Iʼm sure I had a third point, I mustʼve …

Code collaboration tools suck.
Git CLI sucks.
Textual merges suck.
GitHub sucks?
Microsoft monopoly on GitHub AND VSCODE/CLOUD EDITORS SUCKS

Diffing tools
Structured logging tools
Structured any-kind-of-communication-and-recording-and-interpretation tools

Write the same algorithm in N different languages.

teaching sucks.

STUCK WRITING BABY PROBLEMS
can figure anything out from first principles
no true genius programmers
(okay maybe a couple but still)

ugh.
:((

### Turing completeness is a sham

Just because your tool can do it, does not mean it is the right tool.

See, the lie of Turing completeness is that “any language can do anything”.
As if any tool that has the bare minimum needed to express everything is equally good.
Mathematically speaking, thatʼs true.

But programmers get so caught up in the first thing that works.
We never circle back around and improve our tools, improve our processes, improve the ergonomics and the language of composing the bits and pieces that do what we need to accomplish at the end of the day.

Because thatʼs what it should be about at the end of the day – composing parts and processes.
But because weʼre locked into a landscape of proprietary secret ingredients, of incompatible runtimes and languages and standards and data descriptions and process descriptions and algorithm descriptions.

In particular, all of our algorithm descriptions are either unexecutable pseudocode or tied to the particular details of a programming language.
Even if they are literally just integer/array algorithms, or floating point.
(Arguably floating point isnʼt portable, but uhh it is fairly standardized and incredibly convenient.)

I will talk more about consumer-facing UIs below, but the general problem is they arenʼt configurable and arenʼt abstractable.
That is, even if the things to accomplish what you want are technically possible in the UI, they require a lot of different actions, and sometimes they involve reading the UI to know what actions to take!
(That is, you couldnʼt just script them solely by keypresses/mouse clicks. [Example needed].)
UIs usually arenʼt even hackable in the hackiest of hacky ways without serious amounts of work (platform-dependent, like, simulating keypresses, and application-dependent, like dedicated browser extensions).

### You have to google anything and everything

I want to make a separate blog post about this.
But like above, where I was complaining about how algorithms are implicitly entangled in code (which almost always has competing concerns), as opposed to distilled down to a portable essence, I will complain about how other forms of programmer knowledge are like this.

Think about this – a lot of questions have a short one-sentence summaries, and then a very concrete piece of advice to follow for their answer.
It might be a bash command (or a recipe for a bash command).
It might be a way to reorganize your code to fix it.
Importantly, there might be very canonical answers for these things.
The main problem is that we have no central locations for these bits of knowledge.
And then you run into outdated answers, because there is no reason to update those non-canonical source of knowledge.
E.g. `git clone --depth 1`{.bash}.

So what do we do instead?
We google.
We google a *lot*.
We google anything and everything, any nugget of knowledge that would bring us closer to the knowledge we seek.
Piecing together bits of answers from StackOverflow, issue trackers^[Okay, unrelated, but I love reading issues filed against compilers, they are fascinating sources of edge cases, design decisions, plain olʼ bugs/oversights, and April foolsʼ jokes.], and wherever else you can find desperate pleas for help and halfhearted replies^[ahem, Quora. What an absolute shithole of a site, from conception to “design” and “execution”.].
Have to spend time testing, verifying, adapting it to our use cases.
Seeing if it is outdated – if thereʼs better answers out there (although sometimes old questions are closed and never updated ugh), if it simply does not work anymore.


Another complaint is that we very rarely archive the clues we gathered along the way to know what to do in the end, focusing on the last symptom as opposed to the telltale clue that will tip us off to the situation in the future, and the false branches we explored that were not in fact the answer.
(E.g. confusing error messages that really have some simple solution unrelated to what they seem to be saying.)

Anyways, that all requires a new post.
I call it my spaghetti model of knowledge via experience, where I conceptualize finding information about something as a path of questioning, and knowledge is the answer or value associated with it.
I call it spaghetti because it slithers around searching for knowledge and it often leaves a messy, winding trail of pasta sauce in its wake – and the pasta sauce is cleaned up as a “mess”, being judged to be extraneous and of no value, but I think it is very important to keep track of, to expedite the paths of others along the same journey.
Kind of like how if you have water or oil pooled on a surface, it will travel much faster where it has already been.

If you found the answer once, you can probably find it again, and with enough repetition (and/or if it is annoying or memorable enough), you will get to know the answer by heart.
But that locks knowledge into implicit institutional knowledge among the few who have those abilities, skills, and related knowledge to find what they are seeking.
Basically the antithesis of what I strive to be – a teacher and sharer of knowledge.

## You, as ordinary consumers, arenʼt getting the software you deserve

Iʼm sorry, I really really wish I could give you the software you deserve, but weʼve fucked up.

### My vision for software

Interop.
Collaboration.
Configurability.
Hackability.

Open protocols, no more proprietary nonsense.
Collaboration.

#### Basic configurability

Basically have to be a hacker to know that you can configure your software.
Ideally you could configure your software to actually work for you, and work well for you.
But most stuff doesnʼt actually get there, and you have to hack it in yourself.
[Othersʼ observation: Ironic that kids these days grow up with more technology than ever, but everything happens in commercial, self-contained apps and they donʼt need to hack around anymore, so they have even less computer knowledge.]

Most apps arenʼt that configurable.
Only work for a few use-cases, and thereʼs no way to achieve app synthesis by mixing functionality together across apps.

The reason this matters is that it is the prerequisite for the rest of the things I write about in this section.
If users canʼt configure one app on one device, how could they configure multiple apps to coördinate across multiple devices?

[Sidenote: itʼs weird that browsers somehow _canʼt_ be made configurable for shortcuts and such, because JavaScript are used to intercepting certain key/mouse patterns [which is already very fragile], and so that would fail if the browser let users remap their shortcuts.]

#### Why is AirDrop closed-source and not a ubiquitous feature of devices, platforms, apps?

I think one of the biggest letdowns of the technology revolution is that every platform has been developed independently, every device has been developed independently, and there is next to no proper peer-to-peer (P2P) interop across platforms, across devices.
AirDrop is a neat protocol, but it only exists on Apple devices and it only lets you do a few limited things.
Even so, the ability to just transfer files between iPhones, iPads, and Mac computers with just bluetooth and wifi is a very nice feature.

Why arenʼt there more open protocols for adhoc P2P connections??
It would be great to have a world where devices worked as a unified, cohesive whole, instead of separate little islands that get in each otherʼs way.

As one example, I want to be able to control my laptop from my phone, since I sometimes want to be running media on it while Iʼm going to bed or something.
I had to set up a custom web server application on the laptop to let me do the most basic things, such as controlling volume and screen brightness.
I also embedded a very basic [VLC](https://www.videolan.org/) control interface, but now that YouTube support is broken yet again I canʼt really use it as I would like to.
(I can only use it for the bit of media, and I would need to make a better interface to find those files … sigh.)
I might just have to develop my own Chromium puppeteer interface to the YouTube website itself.

Related to this, itʼs just so hard to remote control computers (I donʼt think this is just a Mac problem – maybe it is).
I had to download a commandline tool to control screen brightness – thatʼs not so bad honestly.
For preventing screen sleep, there is a builtin Mac command [`caffeinate`](https://ss64.com/osx/caffeinate.html), but it doesnʼt help with the screen saver!
I had to resort to simulating shift keypresses via AppleScript to prevent the screen saver from activating, since it does not appear to be configurable in any substantial capacity.
(It is easy enough to call the application to start the screen saver, though. But not to reset its timeout. Disappointing.)
Even remapping the keyboard is hard.
Idk, software is hard, it shouldnʼt have to be so difficult to accomplish basic things.

#### Image editors

As one example, I want to expand on how image editors are broken, under-performing.

For fun, I do a lot of image editing on my phone with Snapseed, an app developed by a startup and bought by Google.
Itʼs a really cool app, it has a slick UI that is perfect for the phone and easy to use, and it has a bunch of nice ways to edit things.

These edits range from the more autofilter-like (e.g. sepia filter, automatic white balance) to the more fine-grained tools (color curves editor, healing tool).
Additionally, you can see how these edits stack up in history and selectively remove them.
While applying each edit, you can hold the screen to flip between the previous edit and current edit of the image, and afterwards you can compare to the original loaded image.
One of the best features is that each edit admits masking, so you can paint it on to certain parts of the image.

Despite these nice features thereʼs a lot missing.
Like thereʼs basic things that just werenʼt implemented – it would be nice to have a little more control like rearranging edits and duplicating selected ones, instead of just duplicating the whole stack and deleting single items.
It does mean that you have to rerender the whole stack of further edits, but deleting an edit already forces that to happen anyways.

But Iʼm also talking about the meta game here: it only lets you compare edits _vertically_, but as an artist, I often want to compare edits _horizontally_, applying two different filters (or the same filter with different settings) and seeing which I prefer, especially after further edits stacked on top.
I donʼt know of a single image editing app that implements this!
Not GIMP, not Inkscape, not … uh … Adobe Fresco … and uh I donʼt really use other apps (but I donʼt think itʼs a FOSS problem, pretty sure MS paint doesnʼt do what Iʼm talking about either).
Please tell me if Iʼm wrong, but Iʼve never heard of it being implemented, even though it is an obvious thing that artists and designers want to do!
[Related: devs often want to work on tests and code somewhat separately, not in lockstep, and git does not make this a pleasant workflow since it isnʼt convenient to modify just tests or just code, especially when they are syntactically tied together due to like renaming functions or whatnot, but semantically the same tests could apply in theory.]
We are forced to workaround the lack of meta-editing by playing weird games with the editor history, which is a risky business since it is linear and may delete our edits if we mess up.
Also would be great if apps gave the option of saving some or all of history, or even editing the history.
Make history a first-class aspect of editors!

## Problems with “AI” (ahem, Machine Learning)

AI running before we can walk, skipped all the foundation, the interesting journey along the way, discovering new degrees of freedom

Also, uh, blatant copying and license violations and questionable training data and.

And lack of invariants and any sense of reliability and correctness checks.

I mean, in general my refrain is that AI doesnʼt solve problems I care about, and Iʼm sticking by that.
(Except for like TTS or STT, that seems very reasonable to use machine learning approaches for, even though they still donʼt have the kind of linguistic depth and insight I would wish for.)
