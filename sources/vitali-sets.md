---
title: Vitali Sets
subtitle: Why \(\R/\Q\) is strange
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2025/01/12
---

The “construction” of Vitali sets rests on the quotient \(\R/\Q\), which is a strange beast: not entirely cursed, just strange.

(is it still a construction if it uses the axiom of choice haha)

the simpler quotient \(\R/\Z\) is stilll a bit weird, because just, why are you doing that, but it ends up looking like \(\halfOpen{0}{1}\): you can tile this to cover \(\R\) exactly

this tiling is related to the translation-invariance of Lebesgue measure, btw … since we are looking at the additive structure of \(\R\) and quotienting it

uhh so why is \(\R/\Q\) strange?

it is not related to the cardinality of \(\Q\): it has the same cardinality as \(\Z\)

it's not even related to the cardinality of \(\R/\Q\): it is no larger than \(\R/\Z\), since we are quotienting by a larger subgroup

i think it's mostly because of the topology of \(\Q\) as a subspace of \(\R\) …

so, \(\R/\Z \sim \halfOpen{0}{1}\) tells us that we can measure the gaps in \(\Z\) as having measure \(1\) in \(R\)

(again, relating to translation invariance and stuff)

but \(\Q\) doesn't have gaps like that, its topology/group structure doesn't support a “next number” function

instead of imagining a tiling of \(\halfOpen{0}{1}\) covering \(\R\), we are instead asked to imagine a {patchwork, admixture, cursed recombination} of a (cursed) Vitali set covering \(\R\)

there simply is no way to imagine it in any visual sense, especially since Vitali sets don't exist in any explicit sense: they rely on the axiom of choice

and yeah, we can talk about why the axiom of choice appears from the quotienting and “equivalence classes” and canonical representatives and such …

------

well, as I said earlier, \(\R/\Q\) is smaller than \(\R/\Z\), so since we already fit \(\R/\Z\) in \(\halfOpen{0}{1}\) we can do it with \(\R/\Q\) too, in theory

but type theory (especially constructive type theory) says it is isn't so easy to move between viewing objects as quotients and viewing them as subsets: you have to have some nice structure that makes this constructive, much less computable

\(\Z\) had that nice structure in relation to \(\R\), and \(\Q\) does not

_rerereading the wiki article_

right, and \(\Q\) is threading the needle here: we do still need it to be countable to apply countable additivity

and then we ask how much measure out of \(\halfOpen{0}{1}\) it is and we get the answer that it doesn't

large enough to be eldritch, small enough to be amenable to the Tools still

------

i sort of have a intuition of sets that goes like

the nice measurable sets start with intervals

and then we can take countable unions of them

(which is great because then it means it doesn't even matter what type of intervals we started with, closed or open or half open)

and then there's some things like the Cantor set (which has measure zero)

but its still built on the idea of intervals as our unit of measurement: if your over and under approximation by intervals converge to An Answer, that is The Measure

and intervals are lovely as topological objects (open balls are great building blocks for topology) and as arithmetical ones too (they have a length, and that length is translation invariant)

and yeah, it's a little remarkable that we can do the countable thing and still preserve these nice properties of intervals, but maybe not too surprising

so what's remarkable about \(\R/\Q\) and Vitali sets is that they really are purpose built to destroy all of the arithmetical and topological niceness of these intervals!

running out of steam on that train of thought …

------

back to the logical side

quotients take a set and chop it down by an equivalence relation: you are unable to distinguish elements that you were previously able to

and that's all they say, really. asking for more than that is relying either on the niceness of the relation (like \(\R/\Z\)), or appealing to the axiom of choice

because subsets also take a set and chop it down, by specifying a property to include (or exclude) elements

both reduce cardinality, but they just don't have similar internal structures to their being at all

but to the set theorist, especially the non-constructive set theorist,

everything looks like a (sub)set!

so when they see a quotient by a relation, they want to ask, “what subset of the original set does this correspond to?”, and they don't like the answer when we warn them, “uhh idk, nothing in particular? unless it happens to be very special”

\(\R/\Z\) is very special, and we got back to “it looks like \(\halfOpen{0}{1}\)” by saying “well, it would be better if the real numbers in it sit close to each other, if we can corral it into a familiar topology and alignment”

but from the perspective of cold, hard logic, there's nothing special about \(\halfOpen{0}{1}\) as a representative, we just like those numbers, but we could offset it by any integer, or we could split it up into more intervals that are offset by their own integers, and so on

so when we wag our finger at the set theorists and tell them to stop thinking of quotients this way

they sigh and acquiesce, scratch their heads

and this is where “equivalence classes” come from

(for some examples, the size distinction of equivalence classes instead of equivalence sets matters, but I don't think that's the case here …)

… uhh need a refresher on the setup here, brb

------

uhh correction

each individual “equivalence class” is a subset (e.g. \([0]\sim\)), but the collection of them is canonically a class, I think?

and then set theorists have a motive to prove it really is just a set, not a class, especially when we were supposed to be constructing a smaller, nicer object, not a larger, weirder one!

but trying to work with these weird subsets is gnarly: why would you want to work with the equality of subsets, asks the type theorist? and put algebraic structure back on them??

so yeah, there's pressure to think of canonical representatives of each equivalence class, so that you can work with those instead of vague amorphous subsets,

and this process (“pick canonical representatives”) is the same as what I said the original issue was (“view a quotient construction as a subset construction”), and you can more directly see how it involves the axiom of choice in the bad cases

and yeah, idk, in general these quotients kind of have weird topological behavior sometimes?

like even when you aren't talking about topology topology, computation still has its own topology

it's not that subsets can't have their own weird behavior sometimes … but trying to view quotients as subsets is a particularly weird thing to do, constructively speaking

idk, i feel like i'm going in circles now

------

well, like,

roughly speaking, an algorithm is something that takes a *thing* (the input)

and it inspects it in order to produce an output (and it may embed the input in the output too)

and so working with subsets isn't so weird: as an input, you just get the element of the set and the guarantee that it lies in the subset

and as an output, you just need a reason that the element of the set that you decided on lies in the subset: maybe the property is computable and you can actually check it algorithmically, or maybe you just know it by how you constructed the algorithm or what inputs you received

but quotients are weird: as an output, you can technically just produce whatever element of the underlying set you want to,

but if you get one as an input, you suddenly need to prove that your whole algorithm is well-defined (yeah, this is the only place where that comes from):

you need to certify that as your algorithm inspects the input, it only does so in allowed ways with respect to the equivalence relation, or it happens to end up with a corresponding result:

that the information it embeds into the result respects the equivalence relation still (maybe it goes into an even looser equivalence relation)

and now you've gotten tangled up with what does equality *really* mean, which is again where the topology starts sneaking in

but it's really really good to think about, it's a powerful tool, just requires some care
