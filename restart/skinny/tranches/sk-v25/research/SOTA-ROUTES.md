# SK-V25 state-of-the-art route bank

Research informs the portfolio; it does not decide the carrier. Each route below
names the local prototype that must earn adoption.

## Parser execution and staging

### Staged selective combinators

[Staged Selective Parser Combinators](https://mpickering.github.io/papers/parsley-icfp.pdf)
compiles a declarative combinator representation rather than accepting permanent
closure-dispatch overhead. Its useful lesson here is sequencing: first give parser
definitions a stable inspectable algebra, then compare a compiled face with the
direct interpreter. It does not justify staging the current mutable closures.

Local falsifier: after PT-E semantics are sealed, compile the same parser graph to
a compact program and compare semantic fixtures, allocations, code size, cold start,
and throughput. Retire staging if it fails to beat the direct carrier on a named
workload.

### Fused lexing and parsing

[flap: A Deterministic Parser with Fused Lexing](https://arxiv.org/abs/2304.05276)
shows that separately specified lexers and parsers can be fused through a normalized
deterministic form and reports two-to-six-fold speedups over an established
generator. The load-bearing idea is a semantics-preserving normal form, not a
handwritten token cursor.

Local falsifier: Value's CSS experiment compares direct string parsing, materialized
tokens, and generated fused scanning from one grammar and corpus. The token or fused
route survives only with equal diagnostics and a measured end-to-end win.

### Grammar structure and contextual lexing

[Tree-sitter's grammar guide](https://tree-sitter.github.io/tree-sitter/creating-parsers/3-writing-the-grammar.html)
ties grammar symbols to useful tree structure, requires explicit ambiguity, and uses
context-aware lexing. Its generator fails on unresolved conflicts and emits
structured grammar and node metadata
([generation contract](https://tree-sitter.github.io/tree-sitter/cli/generate.html)).

Local consequence: BBNF/regex bootstrap gates must compare language, structure,
conflicts, diagnostics, and generated provenance. Byte-identical output alone cannot
prove a good grammar; silently resolved ambiguity is forbidden.

## Regex execution tiers

The official [`regex-automata` meta engine](https://docs.rs/regex-automata/latest/regex_automata/meta/struct.Regex.html)
selects among literal acceleration, one-pass DFA, lazy/hybrid DFA, and PikeVM-style
routes under explicit configuration and size limits. The architecture supports a
small semantic HIR with several execution tiers; it does not support treating every
construct as DFA-compatible.

Local falsifier: classify anchors, captures, laziness, Unicode, and unsupported
constructs before tier selection. Compare a direct selector with any CSP selector
and retain CSP only if a corpus contains a genuine coupled choice it solves better.
Compare table minimization with any e-graph minimizer on identical automata.

The 2025 Martynova–Okhotin bound for
[regular-expression to DFA conversion](https://arxiv.org/abs/2504.20555) sharpens
the worst-case state requirement to
`2^(n/2 + sqrt(n) log(n)^Theta(1))` by alphabetic width and supplies a nearly
matching lower bound. This is a direct rejection of unconditional determinization:
even a better construction retains exponential families. A sealed regex plan must
therefore carry an explicit state/transition budget and select Thompson/Pike,
one-pass, lazy DFA, or bounded full DFA before materializing the chosen machine.
“DFA where possible” is not a plan unless the possibility predicate includes the
resource proof.

## Type inference

### Stratified constraint inference

[OutsideIn(X)](https://www.microsoft.com/en-us/research/?p=162562) separates a
common inference engine from a parameterized constraint domain and insists on
principal-type conditions. It cautions against unconstrained local generalization
when richer assumptions destroy principality.

Local consequence: `bbnf-types` owns semantic rules; `bbnf-solve` owns scheduling.
Generalization occurs only at declared reusable grammar boundaries. Grammar layout
and backend choice are not smuggled into equality unification.

### Algebraic subtyping

[The Simple Essence of Algebraic Subtyping](https://infoscience.epfl.ch/entities/publication/106da598-3385-4029-892b-27ea85194046)
presents a compact principal inference algorithm and validates it against MLsub on a
large generated corpus. It is an orthogonal candidate when union/intersection and
subtyping are real grammar consumers.

Local falsifier: first implement equality, rows, variants, functions, and explicit
recursion. Add algebraic subtyping only if Value/grammar examples require relations
that row unification cannot express, then compare inferred precision, diagnostic
quality, implementation size, and solve time with the simpler kernel.

### Row-polymorphic effects

[Koka's row-polymorphic effect system](https://arxiv.org/abs/1406.2061) demonstrates
inferred extensible effect rows and state encapsulation. For this campaign, rows are
useful first for record/variant output; parser effects such as recovery, commitment,
allocation, or context remain a research family until they improve a concrete
static guarantee.

Local falsifier: encode one host transform and one recovery-capable grammar with an
effect row. Retire effect inference if explicit capability types are smaller and
equally precise.

## Fixed points and equality

[egglog](https://arxiv.org/abs/2304.04332) combines incremental Datalog-like
lattice reasoning with congruence closure and extraction. It is relevant because
bbnf-lang currently rebuilds CSP state inside equality-saturation iterations.
It does not excuse semantic rewrites or incomplete extraction.

Local falsifier: implement a direct deterministic worklist for one analysis and a
small translation-validated equality island for one rewrite family. Compare direct,
current e-graph, and egglog-style formulations on semantics, incrementality, memory,
and code size. The simplest winner owns the organ.

Two 2025 routes raise the bar without changing that ruling. The
[eqsat MLIR dialect](https://arxiv.org/abs/2505.09363) represents e-graphs inside a
compiler IR so constructive passes can preserve non-destructive alternatives and
ordinary compiler machinery can operate on them. The ongoing
[relational contextual equality-saturation](https://arxiv.org/abs/2507.11897)
route seeks context-dependent rewrites in egglog. Both make the boundary more
explicit, not less: a retained BBNF equality island must have a typed translation,
context relation, bounded saturation report, and checked extraction back into the
sealed stage value. They do not justify carrying one ambient whole-program e-graph
through unrelated analyses, and the latter remains research rather than established
production evidence.

Local falsifier: prototype one recognizer/rewrite family three ways—direct
normalization, external bounded e-graph, and an IR-resident non-destructive carrier.
Measure translation cost, retained alternatives, extraction totality, stage-value
size, and incremental invalidation. Delete both e-graph routes unless one wins on a
real corpus while the equivalence witness remains complete.

## Novelty route

The campaign's novel route is a composed proof, not a new name:

```text
BBNF-defined regex grammar
  -> reproducible generated syntax parser
  -> checked canonical regex HIR
  -> semantics-selected execution tier
  -> parse-that invocation/result algebra
  -> immutable typed BBNF program
  -> sealed lowering plan
  -> packed Value/demo consumer
```

It is novel only when the chain runs from empty generated directories, improves a
real consumer, and every stage is replaceable and independently falsifiable.
