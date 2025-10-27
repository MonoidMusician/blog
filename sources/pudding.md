think a basic setup isnʼt so hard, but a bit difficult to give room to expand and get niceties:

- parser
  - S-exps?
  - or something more structured
    - `%pragma`{.dtt}, `$constructor`{.dtt}, indentation
- Pi types, Sigma types, universes (auto levels?), inductive types, (dependent?) record types
  - casing, pattern matching...
- elaboration from surface to core
  - implicit arguments...
  - implicit proofs? typeclasses??
  - named arguments
- normalization by evaluation please

----

then pick a couple places to go from there:

- staging ([2LTT](https://andraskovacs.github.io/pdfs/2ltt.pdf)) is kind of essential for advanced features
  - any benefit to multiple levels of staging??
- [Observational Type Theory](https://people.cs.nott.ac.uk/psztxa/publ/obseqnow.pdf)
  - https://pigworker.wordpress.com/2015/01/06/observational-type-theory-the-motivation/
- nominal types? to hang typeclasses on
  - versus macros for general types?
  - nominal types would force evaluation of lambdas i guess, while named functors would be blocked from further evaluation?
  - maybe a modality
- [distfix](https://dl.acm.org/doi/pdf/10.1145/5657.5659)!
  - `prefix:`{.dtt}, `:infix:`{.dtt}, `:postfix`{.dtt}, and they can be chained like mixfix (but without quite so much parsing ambiguity, and thus more flexibility)
  - [e.g.]{t=} `if:then:else: : Π{r : Type}, Bool -> r -> r -> r`{.dtt} (like Haskell)
    - `if: cond :then: truth :else: falseth`{.dtt}
  - *and* `:if:else: : Π{r : Type}, r -> Bool -> r -> r`{.dtt} (like Python)
    - `truth :if: cond :else: falseth`{.dtt}
- module system
  - modules are basically modeled by dependent functions into dependent record types
  - but the insides are soupier, just like the top level of a file
    - you can define types, inductives
    - mutual recursion
    - all the good stuff
- try to (theoretically?) unbox stuff... bit layout? :3
  - [Bit Stealing Made Legal: Compilation for Custom Memory Representations of Algebraic Data Types](https://dl.acm.org/doi/abs/10.1145/3607858)
  - [Double-Ended Bit-Stealing for Algebraic Data Types](https://elsman.com/pdf/icfp24main-p22-final.pdf) (64bit pointer hacking)
  - Sixten/sixty
- row types! ***¿staging?***
  - so i guess: \(\text{Symbol} : U_0\)
  - how about: constructors are of the form \(.sym : \text{Symbol} : U_0\), but there are no destructors, and you only get the isomorphism \((({\Uparrow} \text{Symbol}) \simeq \text{String}_1 : U_1)\) at the _meta_ level if you want to manipulate it
  - \(\text{Row}(t : U_0) : U_0\) acts like \(\text{Map}(\text{Symbol})(t)\) or \(\text{List}(\text{Symbol}\times t)\)
    - has constructors of its own, which are treated specially for unification, to quotient by order (can we make that an actual quotient in the meta level?):
      - \(()_t : \text{Row}(t)\) a.k.a. \(\text{RNil}\)
      - \((\ sym :: \_\ |\ \_\ )\) magic unordered constructor
      - \(\text{RCons}(s, t, r)\) exposes order? only in meta level? idk
        - probably just expose a fold and a guarded constructor at the meta level
        - how does it deal with open rows?? idk
          - *mumble* difference lists…
    - hang on these constructors via unification, to avoid proofs
      - but probably still need proofs...
  - \(\text{Record} : \text{Row}(U_0) \to U_0\)
  - \(\_.sym_{t,r} : \text{Record}(\ sym :: t\ |\ r\ ) \to t\)

  - `{ x :: Int, y :: Nat }` `{ y :: Nat, x :: Int }`
  - `{ x :: Int, y :: Nat | r }` `{ y :: Nat, x :: Int | r }`
  - `RCons "x" (Ident "Int") (RCons "y" (Ident "Nat") (Ident "r"))`
- unification
  - [controlled unfolding](https://arxiv.org/abs/2210.05420) :-/
- tactics?
- closed terms, e.g. efficient values for Ints, Sets, and so on, borrowing for Haskell representations?
  - maybe there are closed and open modalities, so that `Map` can have closed keys and open terms?
  - thatʼs almost row types right there haha, but it doesnʼt account for open rows…
- sized types? eh...
- staging of syntax? type-resolved syntax regions

## [2LTT](https://andraskovacs.github.io/pdfs/2ltt.pdf)

[extracted from said paper]

### 2.1.1 Universes

We have universes \(U_{i,j}\), where \(i \in \{0, 1\}\), and \(j \in \N\). The \(i\) index denotes stages, where \(0\) is the runtime (object-level) stage, and \(1\) is the compile time (meta-level) stage. The \(j\) index denotes universe sizes in the usual sense of type theory. We assume Russell-style universes, with \(U_{i,j} : U_{i,j+1}\). However, for the sake of brevity we will usually omit the \(j\) indices in this section, as sizing is orthogonal to our use-cases and examples.

- \(U_0\) can be viewed as the universe of object-level or runtime types. Each closed type \(A : U_0\) can be staged to an actual type in the object language (the language of the staging output).
- \(U_1\) can be viewed as the universe of meta-level or static types. If we have \(A : U_1\), then \(A\) is guaranteed to be only present at compile time, and will be staged away. Elements of \(A\) are likewise computed away.

-----

- *Lifting*: for \(A : U_0\), we have \({\Uparrow} A : U_1\). From the staging point of view, \({\Uparrow} A\) is the type of
metaprograms which compute runtime expressions of type \(A\).
- *Quoting*: for \(A : U_0\) and \(t : A\), we have \(\langle 𝑡\rangle : {\Uparrow} A\). A quoted term \(\langle 𝑡\rangle\) represents the metaprogram which immediately yields \(t\).
- *Splicing*: for \(A : U_0\) and \(t : {\Uparrow} A\), we have \({\sim} t : A\). During staging, the metaprogram in the splice is executed, and the resulting expression is inserted into the output.

## Builtins

(You want to program with this thing?)

- `Array`{.dtt}
- `CharString`{.dtt}, `ByteString`{.dtt}, `BitString`{.dtt}: `Array _`{.dtt}
- `Char`{.dtt} unicode code point
- `Nat`{.dtt} `n`{.dtt}, `Int`{.dtt} `+z`{.dtt} `-z`{.dtt}, `Bool`{.dtt}
- `Fin : Nat -> U`{.dtt}, sized ints and uints
- enums, variants, records, ...
- `Float32`{.dtt}, `Float64`{.dtt} (probably would not come with laws, unless you really really wanted some) (although i think commutativity is safe? for finites?)
- `IO`{.dtt}, `IORef`{.dtt}, and so on come without laws
  - although uh, bind would be nice to remember properties about what was bound, if possible (without wrapping in a subtype and unwrapping)

```xml {.skylighting}
<?xml version="1.0" encoding="UTF-8"?>
<!-- ```{.xml .skylighting}``` -->
<!-- https://docs.kde.org/stable5/en/kate/katepart/highlight.html -->
<language name="dtt" casesensitive="1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="language.xsd">
  <highlighting>
    <contexts>
      <context name="Root" attribute="Normal Text" lineEndContext="#stay">
        <RegExpr attribute="Keyword" context="#stay" String="%[-a-zA-Z_][-a-zA-Z0-9_]*"/>
        <RegExpr attribute="Variable" context="#stay" String="\$[-a-zA-Z_][-a-zA-Z0-9_]*"/>
        <RegExpr attribute="Annotation" context="#stay" String="@[-a-zA-Z_][-a-zA-Z0-9_]*:?"/>
        <RegExpr attribute="Keyword" context="#stay" String="\s:\s|-&gt;|Π|Σ"/>
        <RegExpr attribute="Meta" context="#stay" String="…|&lt;[^][(){}&lt;&gt;]+&gt;"/>
        <IncludeRules context="FindOperators"/>
        <IncludeRules context="FindComments"/>
        <IncludeRules context="FindStrings"/>
        <RegExpr attribute="Data Type" context="#stay" String="[A-Z][-a-zA-Z0-9_]*"/>
      </context>

      <context name="FindOperators" attribute="Normal Text" lineEndContext="#stay">
        <RegExpr attribute="Operator" context="#stay" String="([-a-zA-Z_][-a-zA-Z0-9_]*:|:[-a-zA-Z_][-a-zA-Z0-9_]*:?)"/>
        <!-- <RegExpr attribute="Operator" context="#stay" String="\(:|:\)|[=|+*?!\\:;]"/> -->
      </context>

      <context name="FindComments" attribute="Comment">
        <StringDetect attribute="Comment" context="LineComment" String="##"/>
        <StringDetect attribute="Comment" context="LineComment" String="#!"/>
        <StringDetect attribute="Comment" context="BlockComment" String="{#"/>
        <StringDetect attribute="Comment" context="ParenComment" String="(#"/>
      </context>

      <context name="LineComment" attribute="Comment" lineEndContext="#pop">
      </context>
      <context name="BlockComment" attribute="Comment" lineEndContext="#stay">
        <StringDetect attribute="Comment" context="BlockComment" String="{#"/>
        <StringDetect attribute="Comment" context="ParenComment" String="(#"/>
        <StringDetect attribute="Comment" context="#pop" String="#}"/>
      </context>
      <context name="ParenComment" attribute="Comment" lineEndContext="#stay">
        <StringDetect attribute="Comment" context="BlockComment" String="{#"/>
        <StringDetect attribute="Comment" context="ParenComment" String="(#"/>
        <StringDetect attribute="Comment" context="#pop" String="#)"/>
      </context>

      <!-- FindStrings looks for single and double quoted strings -->
      <context name="FindStrings" attribute="Normal Text" lineEndContext="#stay">
        <DetectChar context="StringSQ" attribute="String SingleQ" char="'"/>
        <DetectChar context="StringDQ" attribute="String DoubleQ" char="&quot;"/>
      </context>

      <!-- StringSQ consumes anything till ' -->
      <context name="StringSQ" attribute="String SingleQ" lineEndContext="#stay">
        <!--no line continuation here-->
        <Detect2Chars attribute="Escape" char="\" char1="'"/>
        <Detect2Chars attribute="Escape" char="\" char1="\"/>
        <DetectChar attribute="String SingleQ" context="#pop" char="'"/>
      </context>

      <!-- StringDQ consumes anything till ", substitutes vars and expressions -->
      <context name="StringDQ" attribute="String DoubleQ" lineEndContext="#stay">
        <LineContinue attribute="Escape"/>
        <Detect2Chars attribute="Escape" char="\" char1="&quot;"/>
        <Detect2Chars attribute="Escape" char="\" char1="$"/>
        <Detect2Chars attribute="Escape" char="\" char1="\"/>
        <DetectChar attribute="String DoubleQ" context="#pop" char="&quot;"/>
      </context>
    </contexts>

    <itemDatas>
      <itemData name="Normal Text"    defStyleNum="dsNormal"/>
      <itemData name="Keychar"        defStyleNum="dsSpecialChar"/>
      <itemData name="Variable"       defStyleNum="dsVariable"/>
      <itemData name="Operator"       defStyleNum="dsOperator"/>
      <itemData name="Builtin"        defStyleNum="dsBuiltIn"/>
      <itemData name="Index"          defStyleNum="dsAttribute"/>
      <itemData name="Import"         defStyleNum="dsImport"/>
      <itemData name="Function"       defStyleNum="dsFunction"/>
      <itemData name="Data Type"      defStyleNum="dsDataType"/>
      <itemData name="Keyword"        defStyleNum="dsKeyword"/>
      <itemData name="Annotation"     defStyleNum="dsAnnotation"/>

      <itemData name="String SingleQ" defStyleNum="dsString"/>
      <itemData name="String DoubleQ" defStyleNum="dsString"/>
      <itemData name="Escape"         defStyleNum="dsSpecialChar"/>
      <itemData name="Meta"           defStyleNum="dsPreprocessor"/>

      <itemData name="Regular Expression" defStyleNum="dsSpecialString" spellChecking="false"/>

      <itemData name="Comment"        defStyleNum="dsComment"/>

      <itemData name="Error"          defStyleNum="dsError"/>
    </itemDatas>
  </highlighting>
</language>

```
