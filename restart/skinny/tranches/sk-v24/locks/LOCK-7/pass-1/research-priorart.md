# LOCK-7 — PRIOR-ART RESEARCH (pass 1) — THE GRAMMAR TOTALITY & BOOTSTRAP

Prior-art researcher, pass 1. No family allegiance; the four families (7A staged-bootstrap,
7B total-self-host, 7C host-fn-lattice, 7D meta-circular-oracle) are judged only against what
the field and our own evidence base already prove or refute. Citations: web + `path:line`.

---

## 0. THE PROBLEM CLASS, NAMED

LOCK-7 is constitution clause (i) as a design problem: *everything grammar-driven — here and
there — save primitives and combinators; the regex grammar IS bootstrapped and bbnf-defined*
(PORTFOLIO.md:20-22). Concretely: (a) bbnf-in-bbnf self-hosting depth; (b) the regex-grammar
bootstrap (staging, fixpoint, committed stage-0); (c) which planes beyond surface syntax are
grammar-specified (IR? emitter templates? oracle tables?); (d) typed host functions for
context-dependence (hybrid-grammar-host law); (e) what "primitive" lawfully means; (f)
xtask/regen as the bootstrap engine. Standing law: self-hosting uses language features —
augment BBNF if features are missing, never work around (memory `no-workarounds-self-hosting`).

This is the metacompiler / self-hosting-parser-generator problem class — a 60-year literature
with several settled adjudications.

---

## 1. THE FIELD: WHAT EXISTS, WHAT WON, WHAT LOST

### 1.1 The self-hosting metacompiler lineage (the fixpoint proof is old and settled)

- **META II** (Schorre, 1964) — first practical self-hosting metacompiler: written in its own
  language, compiles its own source after a hand bootstrap. Its correctness argument IS the
  fixpoint: compile the meta-grammar with the freshly-generated compiler; if output equals the
  prior output, the generated compiler is proven consistent
  ([Grokipedia: Compiler-compiler](https://grokipedia.com/page/Compiler-compiler);
  [History of compiler construction](https://en.wikipedia.org/wiki/History_of_compiler_construction)).
  **This is exactly SPEC 7A's `regen∘regen = regen, byte-identical` gate.** The idea is not
  novel; the discipline is. The lawful novum is not "we self-host" but "the fixpoint is a
  *committed CI gate* over deterministic codegen."

- **GCC three-stage bootstrap** — stage1 (host cc) → stage2 (stage1) → stage3 (stage2), with
  stage2≡stage3 byte-identical as the gate. **Lesson for 7A: the fixpoint needs ≥2
  self-applications, not one** — a single regen only proves the *hand* bootstrap produced *a*
  parser, not a *fixed* one.

### 1.2 PEG bootstrap practice (the production wins + the minimal-subset pattern)

- **CPython `pegen` / PEG meta-grammar** (van Rossum, PEP 617) — a hand bootstrap parser reads
  the meta-grammar; the generator emits a meta-parser that parses *its own* meta-grammar;
  regenerating yields identical output, proving correctness
  ([A Meta-Grammar for PEG Parsers](https://medium.com/@gvanrossum_83706/a-meta-grammar-for-peg-parsers-3d3d502ea332)).
  **The strongest production win for THIS class** — CPython replaced its LL(1) grammar wholesale
  and ran the *old* and the *new generated* parser side-by-side (differential) for a full release
  cycle before promotion. **Self-host as ORACLE first, production second — 7D's exact thesis,
  field-validated.**

- **Minimal-bootstrap-subset pattern** — Peguin, kragen/`peg-bootstrap`, Peggy/PEG.js share one
  shape: the meta-grammar uses *only simple features*, a small hand bootstrap metaparser produces
  the *full* metaparser, which then parses and regenerates itself
  ([Peguin](https://github.com/jndean/Peguin);
  [peg-bootstrap](https://github.com/kragen/peg-bootstrap/blob/master/peg.md);
  [Peggy](https://github.com/peggyjs/peggy)).
  **This IS 7A's "minimal bootstrap surface, maximal auditability" — a won, standard technique.**
  It forecloses the trap of a bootstrap parser that needs the *full* language to start. bbnf's
  sources already respect it: `bbnf.bbnf` (89 LOC), `expressions.bbnf` (37), `types.bbnf` (7).

### 1.3 The trusting-trust / auditable-stage-0 constraint (7A's dark side)

**Diverse Double-Compiling (DDC)** (Wheeler) + **bootstrappable builds / Mes** — a committed
stage-0 binary is a *trust* liability (Thompson): a wrong/compromised stage-0 propagates
silently into every self-hosted descendant. Guix + Nix + Debian produced a *bit-for-bit
identical* GNU Mes from three different GCCs, demonstrating the counter
([DDC](https://dwheeler.com/trusting-trust/);
[Mes reproducible bootstrap](https://reproducible-builds.org/news/2019/12/21/reproducible-bootstrap-of-mes-c-compiler/);
[Bootstrappable builds, LWN](https://lwn.net/Articles/841797/)).
**Constraint on 7A:** committed stage-0 must be *minimal + auditable*; the byte-identical
fixpoint is the local DDC analogue; the hand parser is the "diverse second compiler." This is
why 7D (oracle-first) is not a nicety — a generated parser no independent artifact cross-checks
is a trusting-trust surface. 7A and 7D are complementary, not rival.

### 1.4 Grammar-totality traps: where semantic actions killed the grammar (7B/7C)

- **Imperative semantic actions — the anti-pattern that LOST for totality.** yacc/bison, ANTLR
  actions, PEG.js/Peggy actions embed arbitrary host code in productions. Totality is *lost* the
  moment logic escapes into opaque host blocks: no analysis, no isomorphism, no cross-backend
  portability. Exactly the failure 7C's *typed host-fn lattice* + memory `hybrid-grammar-host`
  must beat — host behavior behind **declared signatures**, never inline code.

- **Attribute grammars — the principled alternative.** Synthesized/inherited attributes are
  declarative and side-effect-free: "an attribute grammar decorates a chosen tree" vs imperative
  models that "use a mutable context in which later decisions depend on earlier mutations"
  ([Synthesis of Semantic Actions in Attribute Grammars](https://arxiv.org/pdf/2208.06916)).
  **bbnf's `->` projections ARE synthesized attributes** (`int_lit = /.../ -> i64`,
  `expressions.bbnf:6-9`). Known strain point: *inherited* attributes (top-down context) — which
  is precisely the "context-dependent / recursive → host function" boundary the
  hybrid-grammar-host law draws. Theory says this boundary is real and principled, not a cop-out.

- **Nez (open grammar language)** ([arxiv 1511.08307](https://arxiv.org/pdf/1511.08307)) — tried
  to make grammars *fully declarative/portable* by moving AST construction into grammar
  operators, to eliminate host semantic actions. Direct prior art for 7B's "the generator
  becomes mostly data." Lesson: a large fraction *can* go behind the grammar, but Nez still
  needed a host boundary for the genuinely context-sensitive — corroborating 7C's "totality is a
  measured percentage with a named floor."

- **tree-sitter grammar DSL + external scanners** — grammar is a JS DSL, but an *external C
  scanner* is the escape hatch "to handle lexical rules that cannot be described by regular
  expressions"
  ([The Grammar DSL](https://tree-sitter.github.io/tree-sitter/creating-parsers/2-the-grammar-dsl.html)).
  **The escape hatch is necessary AND is the un-analyzable seam.** 7C's contribution over
  tree-sitter is *typing* that seam (declared signatures), so the host boundary is auditable, not
  an opaque C blob. GN-5's TOML datetime/multiline-string leaf (R2 risk, GN-5.md:512-524) is this
  seam: it must be grammar-expressed or a *typed* host-fn — "a host function that hard-codes TOML
  datetime parsing is a name-gate in disguise."

---

## 2. WHAT OUR OWN EVIDENCE BASE ALREADY IMPOSES

### 2.1 The regex-grammar bootstrap is ALREADY LIVE (constitution i, partially delivered)

parse-that ships a **self-hosted regex parser generated from a `.bbnf` grammar**:
`parse_that::regex::generated::parse_generated` — front `rust/bootstrap/src/lib.rs:22`, tests
green for bounded reps / quantifiers / comprehensive patterns (`rust/bootstrap/src/lib.rs:28-61`).
Its header records the bootstrap history: pre-B2 a `#[derive(bbnf_derive::Parser)]` marker was
`cargo expand`-ed into `regex/generated.rs`; post-B2 (bbnf-lang `6142387f`) the proc-macro crate
was **retired** and "the IR pipeline runs through bbnf-lang's `cargo xtask regen`"
(`rust/bootstrap/src/lib.rs:1-20`). **A concrete running artifact, not SPEC-ONLY** — the
strongest evidence clause (i)'s regex-bootstrap is achievable, and it names a preferred
bootstrap *engine*: xtask regen, not a compile-time derive (auditability win, §1.2). Caveat: the
regex `.bbnf` grammar and its regen target are **not visible in skinny xtask**
(`main.rs:40-48` lists json/css/sheets/real-typed only) — the regex bootstrap seam lives on the
parse-that side; where its regen fixpoint is gated is an open cross-repo question for the gestalt.

### 2.2 bbnf-in-bbnf self-hosting is DORMANT SUBSTRATE — a Y.13 violation at HEAD

A bbnf self-host existed in the **legacy** tree (pre-B2, via the retired `bbnf_derive::Parser`)
but was **not carried into skinny** — GN-0 archives legacy, so skinny HEAD has only dormant
sources + a hand front-end. The self-host sources are *rich*: `grammar/bbnf/bbnf.bbnf` (89 LOC)
with `@import` modularity (`bbnf.bbnf:4-5`), a `@host id : type_name` typed directive
(`bbnf.bbnf:73`), `->` projections, grammar-function closures `|params| rhs` (`bbnf.bbnf:51`),
and `@pretty/@ws/@token/@debug/@recover` directives; plus `expressions.bbnf` (37), `types.bbnf`
(7). **But nothing consumes them.** xtask has regen targets for json/css/sheets/real-typed ONLY
— **no `regen-bbnf`, no self-host target** (`skinny/xtask/src/main.rs:40-47`; R1 §5). The bbnf
front-end `parse_grammar` is entirely **hand-written** (`skinny/crates/grammar/src/lib.rs:29`,
~2142 LOC in one `lib.rs`), NOT generated from `bbnf.bbnf`. The `crates/bootstrap` the parse-that
header claims mirrors ("one `pub use ::bbnf::grammar::generated::BbnfBootstrap;` line",
`rust/bootstrap/src/lib.rs:18-20`) **does not exist in skinny** — no `crates/bootstrap`, no
`grammar::generated`.

**Adjudication this imposes:** by clause viii (Y.13, consumer-less substrate forbidden, "in
designs as in code", PORTFOLIO.md:32), the dormant sources are a live violation unless a design
gives them a consumer. **7D (meta-circular ORACLE) is the minimal lawful consumer:** the
bbnf-defined bbnf grammar becomes a differential oracle against the hand `parse_grammar`, gated
for promotion on measured parity — mirroring CPython's oracle-first migration (§1.2). Under 7D
the dormant sources become verification substrate today and a promotion candidate tomorrow;
under 7B alone they stay aspirational until the whole front-end is regenerated (higher risk, no
incremental consumer). Evidence tilts toward **7D as entry ramp, 7B as destination** — not
either/or.

### 2.3 The typed host-fn boundary is DECLARED but UNSUPPORTED at runtime

`@host` is *parsed* — the front-end has `RuntimeHostCapture` /
`RuntimeConstructKind::HostCapture` (`grammar/src/lib.rs:170,251,421`) — but the lowering stamps
it **`"BBNF-UNSUPPORTED-HOST-CAPTURE"`** (`grammar/src/lib.rs:222`). So 7C's typed-host-fn
interface exists as *syntax and IR node* with **no backend**. This is the exact "declared,
un-lowered" seam 7C must close, and it is the hybrid-grammar-host law's home
(`bbnf.bbnf:73` `host_directive = "@host" ?w , identifier ?w , (":" ?w , type_name)?`). The type
plane at `type_name` (`types.bbnf:3-7`: primitives + arbitrary identifier for backend
resolution) already speaks the backend-agnostic-types law — so the *signature* language is
present; only the *dispatch* is missing.

### 2.4 SPEC §0 laws that constrain any LOCK-7 design

- **Fixpoint requires deterministic codegen.** `regen∘regen = regen, byte-identical` (7A) is
  impossible under nondeterministic emission (HashMap order, unstable sort). Memory laws
  `changed-flag-convergence` and `clean-regen-discipline` (generated files always fresh-regen,
  never hand-patched) are the preconditions. xtask already enforces a *per-grammar* stale-fail
  gate (`check_target` fails if stale, R1 §5; `main.rs:224,237`) — the fixpoint mechanism exists;
  it is not yet pointed at the bbnf grammar itself, nor does it do ≥2 self-applications.
- **No perf theater on promotion.** 7D's promotion to production parser is gated on *measured
  parity + performance, subsystem by subsystem* — warm benches are disinformation (SPEC §0), cold
  paired PMU per standing law. A correct-but-slower self-host is an oracle, not a replacement —
  and that is fine (CPython shipped the oracle for a full cycle first).
- **No god modules.** The 2142-LOC single-file `grammar/src/lib.rs` is itself a no-god-modules
  smell (memory `no-god-modules`, `directory-module-structure`). If 7B/7D regenerate the
  front-end, the generated home must be a directory-module, not one dumped file.
- **Augment BBNF, never work around** (`no-workarounds-self-hosting`). Every gap — `@host`
  lowering, TOML datetime leaf, inherited-attribute context — is resolved by *adding a language
  feature or a typed host signature*, never a grammar-name shim (which W3's `check-name-gates`,
  GN-5.md:276-341, would catch as a name-gate in disguise).

---

## 3. THE TRAPS, NAMED (from comparable systems)

1. **Two-parser drift.** Hand and generated parsers silently diverge (CPython mitigated with a
   full differential cycle). *Counter:* 7D makes the generated grammar an ORACLE with a
   differential corpus BEFORE promotion; drift surfaces as an oracle failure, not a prod bug.
   bbnf has the corpus infra (`grammar/tests/`, `check-conformance` `main.rs:48`).
2. **The un-auditable stage-0** (trusting-trust, §1.3). *Counter:* minimal auditable stage-0 +
   byte-identical fixpoint (7A) as local DDC; the hand parser is the diverse second compiler.
3. **Semantic-action escape hatch swallows the grammar** (yacc/PEG.js). Totality erodes as logic
   migrates into host blocks. *Counter:* 7C's *typed* host-fn lattice with a **named floor per
   subsystem** — totality is a measured percentage (honest metric), not a binary claim; `@host :
   type` is the only host seam, and it is typed.
4. **The fixpoint that isn't byte-identical.** Nondeterministic codegen breaks regen∘regen
   (§2.4). *Counter:* deterministic ordering as a hard precondition; the gate is born-RED until
   two self-applications match byte-for-byte.
5. **Left-recursion in the bootstrap** (OMeta's hard PEG problem). *Non-issue for bbnf:* the
   value/expression grammars use explicit precedence-climbing ladders
   (`value_mul→value_add→value_cmp→value_and→value_or`, `expressions.bbnf:26-30`), sidestepping
   left-recursion by construction — a design choice the field validates.
6. **Totality overreach (the Nez lesson).** Pushing *everything* behind the grammar (7B maximal)
   hits genuinely context-sensitive constructs needing host help; denying the host boundary
   re-implements a Turing tarpit in grammar operators. *Counter:* 7C's measured floor bounds 7B —
   the families compose (7B destination, 7C boundary), they do not compete.
7. **Grammar-specified planes with no consumer** (master pathology, precept Y.13). Grammar-
   specifying IR / emitter templates / oracle tables (7B's reach beyond surface syntax) is dead
   substrate unless each plane has a same-wave consumer. *Counter:* every plane 7B proposes to
   grammar-specify names its generator/oracle consumer in-wave — dormant `bbnf.bbnf` today is the
   negative example (§2.2).

---

## 4. SYNTHESIS FOR THE FAMILIES (no allegiance — where the evidence points)

- **7A STAGED-BOOTSTRAP** is the *floor everyone builds on*, not a rival: fixpoint gate + minimal
  committed stage-0 is settled, standard, half-built (xtask stale-fail exists; determinism laws
  exist). Open work: point the existing gate at the bbnf grammar itself + add the ≥2-self-
  application byte-identical check. Low novelty, high necessity.
- **7D META-CIRCULAR-ORACLE** is the *lawful entry ramp*: gives the dormant `.bbnf` sources a
  consumer TODAY (cures the Y.13 violation §2.2), matches the field's strongest production
  precedent (CPython oracle-first §1.2), gates promotion on measured parity+perf per SPEC §0. The
  family the evidence most directly demands.
- **7C HOST-FN-LATTICE** is *forced* by the field's totality traps (§1.4, §3.3) and by our own
  un-lowered `@host` seam (§2.3). The signature language exists (`types.bbnf`, backend-agnostic);
  the dispatch is the gap. Totality-as-measured-percentage-with-a-floor is the honest framing
  attribute-grammar/Nez theory supports.
- **7B TOTAL-SELF-HOST** is the *destination, bounded by 7C and gated by Y.13* (§3.6-3.7):
  achievable in reach (Nez, tree-sitter push large fractions behind grammar), but every
  grammar-specified plane must ship its consumer in-wave or it is the master pathology reincarnate.
  Highest ambition, highest discipline cost.

**Not four routes to pick one of — they compose as a DAG:** 7A (deterministic fixpoint floor) →
7D (oracle gives dormant sources a consumer, cures Y.13) → 7C (type the host seam both need) → 7B
(grow totality behind the now-typed boundary, consumer-per-plane). Open adjudications for
synthesis, both *measured* not asserted (clause vii): *how far 7B reaches* (surface only? +IR?
+emitter templates?) and *where 7C's floor sits per subsystem*.

---

## 5. CITATIONS

Web: META II / metacompiler fixpoint — https://grokipedia.com/page/Compiler-compiler ,
https://en.wikipedia.org/wiki/History_of_compiler_construction ;
CPython PEG meta-grammar — https://medium.com/@gvanrossum_83706/a-meta-grammar-for-peg-parsers-3d3d502ea332 ;
minimal-bootstrap-subset — https://github.com/jndean/Peguin ,
https://github.com/kragen/peg-bootstrap/blob/master/peg.md , https://github.com/peggyjs/peggy ;
trusting-trust / DDC / bootstrappable — https://dwheeler.com/trusting-trust/ ,
https://reproducible-builds.org/news/2019/12/21/reproducible-bootstrap-of-mes-c-compiler/ ,
https://lwn.net/Articles/841797/ ;
attribute grammars — https://arxiv.org/pdf/2208.06916 ;
Nez open grammar language — https://arxiv.org/pdf/1511.08307 ;
tree-sitter grammar DSL + external scanner —
https://tree-sitter.github.io/tree-sitter/creating-parsers/2-the-grammar-dsl.html .

Codebase (`path:line`): live regex bootstrap `parse-that/rust/bootstrap/src/lib.rs:1-61`;
dormant bbnf self-host `grammar/bbnf/{bbnf.bbnf,expressions.bbnf,types.bbnf}`; hand front-end +
unsupported host capture `skinny/crates/grammar/src/lib.rs:{29,170,222,251,421}`; xtask regen
targets (no regen-bbnf) `skinny/xtask/src/main.rs:{40-48,224,237}`; regen/fixpoint mechanics +
grammar source map R1-skinny-cartography.md §5,§6; TOML host-seam risk GN-5.md:512-524; name-gate
CI gate GN-5.md:276-341; constitution clauses (i),(vii),(viii) PORTFOLIO.md:20-32; memory laws
`no-workarounds-self-hosting`, `hybrid-grammar-host`, `clean-regen-discipline`,
`changed-flag-convergence`, `no-god-modules`, `directory-module-structure`.
