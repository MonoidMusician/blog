# What is this?
This is a tool for explaining, analyzer, and teaching about LR1 gramars.

# Why?
The Monoid Musician was frustrated by the lack of intuitive explanations of LR1 grammars, and decided to build a cool tool with pretty colors to keep your attention.

# What does it do?
You can input an arbitrary grammar and then test against inputs and see what's happening under the hood.

Once you have entered a grammar, you can generate random inputs that match that grammar.

# What are those fields on the page?
The four fields at the top of the page are the `TOP` rule that tells the parser where to start.

The three fields below the list of rules are where you would add a new rule. See the next heading for a quick demonstration.

# Getting started
Let's do a grammar for arithmetic.

1. Delete all the production rules, but keep the TOP rule the same.
2. Add the rules below
   (For example, the first E rule below would be `E F E1` in the three fields above the `Generate grammar` button)

Here's the rules we want to add:
```
TOP	:	E␄	#TOP
E	:	F	#E1
E	:	E+F	#E2
F	:	G	#F2
G	:	N	#G1
G	:	(E)	#G2
N	:	0	#N0
N	:	1	#N1
```

Now click the `Generate grammar` button to make sure everything was typed in correctly.
Assuming there aren't any errors, click the `Random` button to see some legal values!

Let's say you want to add `*` to the parsing rules, can you figure out how to do it?

<spoiler>
Add a new rule with `F F*G F3`
</spoiler>
# How does it work?
MAGIC, or see the Exposition heading below.

# Exposition
# Vocabulary
- Terminals
- NonTerminals
- State
- Lookahead
- mechanics of how the parsing machine actually works
