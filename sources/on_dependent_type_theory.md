dependent type theory is a foundational language that unifies frameworks for mathematics, logic, and computer science by taking ideas, language, and practices from set theory, algebra, category theory, topology, programming, and formal logic

by unifying these into a single foundational language, it conflates notions in ways that are sometimes unhelpful, but often illuminating as well

for example, universal quantification can be identified with dependent function types, but existential quantification and dependent pair types should *not* be identified: the former is the propositional truncation of the latter

the generality of dependent type theory also poses challenges for implementation and explanation, both in informal descriptions (where the seeming “level-mixing” of dependent types feels strange to practitioners of stratified foundations) and in its formal semantics in category theory and set theory

unlike classical foundations of mathematics (namely set theory), intensional dependent type theory prioritizes computation, for several reasons:

- computation is closely related to consistency, with proofs of false manifesting as infinitely reducing terms in an otherwise well-behaved theory
- computation is beneficial for elucidating meaning: the formal judgments of a type theory are understood to be decidable and self-evident, and so beta reduction in the theory ought to be computable
- computation is necessary for using dependent type theory for programming and is beautiful in its own right, with deep connections to topology that manifest in types and functions

the nature of equality is important to the character of a type theory, with judgmental equality (definitional equality) being the computable notion necessary for checking the consistency of a program or proof syntax (thus influencing the usability of the type theory), while typal equality (propositional or homotopical equality) reifies provable semantic equality as a type within the type theory

there are still gaps and unanswered questions between the implementation of front-ends for interactive theorem provers and what has been proven and formally specified: even the core calculus of a theorem prover (meant as a final check on its consistency, after tactics and elaboration) may not be as-specified, or it may include compromises on properties of the type theory (like transitivity of definitional equality)

besides their use as combined logical, mathematical, and programming foundations, dependent type theories are also rich starting points for modeling objects *synthetically*, where types represent non-Set notions directly via additional type formers and operations. if HoTT (Homotopy Type Theory) is the internal language of (∞, 1)-toposes, then one can imagine dependent type theories for other notions, such as (∞, n)-toposes (directed homotopy type theories), or more geometric notions rather than topological–logical ones
