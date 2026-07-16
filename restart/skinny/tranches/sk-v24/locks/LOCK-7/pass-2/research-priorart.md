# LOCK-7 pass-2 — PRIOR-ART RESEARCH (grammar totality & bootstrap)

Sweep of what exists here, what won/lost elsewhere for this problem class, the constraints our
evidence base already imposes, and the traps named in comparable systems. Options and evidence,
never conclusions. Every codebase claim is `path:line`; external claims are hyperlinked.

---

## 0. The measurement substrate as it actually stands (codebase truth)

> **Re-verified at skinny HEAD 2026-07-16 (XL-H):** every `path:line` in this section was re-grepped
> this pass. `xtask` dispatch still carries only `regen-{json,sheets,css,real-typed}` (no
> `regen-bbnf`, no `fixpoint`); `general_descent/emit.rs:1` still reads "recursive-descent
> **recognizer** emitter" with the DirectBuild no-op at `:385` and the `cursor: &mut usize` alias
> at `:448`; `resolve_refs`/`add_expr`/`validate` still at `ir/src/lib.rs:{84,51,110}`;
> `!has_rule("value")` shape-dispatch still at `runtime_generator.rs:98`; `parse_generated` still a
> PRODUCER at `parse-that/rust/parse_that/src/regex/mod.rs:34` (`host::lower` at `:57`, over the
> `rust/bootstrap/` crate — the seed crate is `rust/bootstrap`, NOT `rust/regex-bootstrap`). **The
> concrete self-host subject exists in-tree:** `grammar/bbnf/bbnf.bbnf` (+ `expressions.bbnf`,
> `types.bbnf`) and `grammar/ebnf/ebnf.bbnf` are present — bbnf-in-bbnf is a real file to route
> through the emitter, not a hypothetical. Note `@{…}` has *partial* handling already
> (`grammar/src/lib.rs:420` `rest.starts_with("@{")` + tests at `:1737,:1985,:2024`), so the "three
> parser gaps" are the DIRECTIVE-given floor, one of which (`@{}` atom) is only partly open.

- **No `regen-bbnf`, no fixpoint gate exists in skinny.** The xtask dispatch
  (`skinny/xtask/src/main.rs:39-52`) carries only `regen-{json,css,sheets,real-typed}`; a
  grep for `fixpoint|regen-bbnf|byte_identical` over `skinny/xtask` returns *nothing* (only two
  incidental `byte-identical` comments in `regen_css.rs:35,83` about the 7-replica CSS collapse).
  The "~88s→0.5s" self-host datum and the `regen-bbnf` command are **legacy transplants** —
  they live only in prior-tranche docs (sk-v14/15/20/21 research) and the legacy `runtime/bbnf/`
  tree, never in skinny HEAD. **Confirms DIRECTIVE 7A: the datum must be re-derived on skinny or
  demoted; do not carry it.**
- **The legacy self-host was OVERFIT, and that is a settled adjudication.** The skinny-impl
  overfit audit (`restart/audit/skinny-impl-overfit/V1/AGENT-3-pattern-h-runtime.md:201`) records
  legacy `regen-bbnf` as "8 files → `regen-bbnf` + projection (89 LOC) + NEW `emit_typed_bbnf_*`
  cohort (9 fns, 1026 LOC) | **NO — bespoke per-grammar emitter**". A re-authored bbnf bootstrap
  that resurrects a bespoke emitter cohort **repeats a named refutation**; the only lawful route
  is through the grammar-NEUTRAL general emitter (§2 below).
- **Today's fixpoint SUBJECT is generated `.rs` bytes, via `check_dir`.** `regen.rs:37-49`
  `check_target` calls `emitted.check_dir(...)` — a byte comparison of emitted output vs on-disk.
  `validate_inputs` (`regen.rs:153-166`) blake3-hashes the *grammar inputs* and prints the digest;
  it does NOT relate input structure to output. So the extant determinism evidence is
  output-byte-identity under a *fixed* generator — exactly the **vacuous-fixpoint** the DIRECTIVE
  flags: a fixed-const emit path stamps identical bytes for any input, so triple-identity
  certifies nothing about self-application.
- **The general emitter emits RECOGNIZERS, not GrammarIr builders.** `general_descent/emit.rs:1`
  ("recursive-descent *recognizer* emitter"); each rule → `fn parse_<rule>(bytes, cursor, tape:
  &mut TapeBuilder) -> Result<(), ParseError>` (`emit.rs:259,269`), leaves `mark(tape,...)`
  (`emit.rs:251`) and Span-project. Line 385 discloses "A DirectBuild records ... Tape
  bookkeeping / no-op for the recognizer." So whether a `-> Span`/`-> T` **builder** augment over
  this recognizer round-trips into GrammarIr-equivalent structure is genuinely **OPEN/unseeded** —
  it cannot be presumed from the legacy `emit_typed_bbnf_*` cohort (which is the refuted route).
- **The host build-mechanism census is concrete and small.** `add_expr` is an arena append
  (`ir/src/lib.rs:51`); `resolve_refs` (`ir/src/lib.rs:84-108`) is a pure name→rule_id link over a
  `HashMap` — deterministic, no external context, no `&mut Ctx` beyond the grammar itself;
  `validate` (`ir/src/lib.rs:110`) runs `validate_resolved_refs` + a nullable-repeat check. These
  are **shared semantic passes** runnable on hand-parser output AND generated-parser output — the
  natural 7D pivot.
- **The regex self-host already exists and WORKS in parse-that.** `rust/bootstrap/` is the
  `regex_bootstrap` crate (`Cargo.toml`, `regex.bbnf`, `src/lib.rs`, a `regex_parse` bench). Its
  `regex.bbnf` is present. `parse_generated` (`parse_that/src/regex/mod.rs:34`) is a **PRODUCER**:
  generated parser → typed AST → `host::lower` → `Hir`, "Available for benchmarking and
  verification against the primary `parse()`." **Confirms DIRECTIVE 7D: `parse_generated` is a
  producer, not a recognizer** — and the meta-circular oracle is not hypothetical, it is deployed.

---

## 1. STAGED-BOOTSTRAP (7A) — prior art, constraints, traps

**What won elsewhere (the seed-and-fixpoint canon).** Every mature self-hoster gates on a
**stage2 = stage3** byte/hash fixpoint, NOT stage1 = stage2:
[rustc](https://rustc-dev-guide.rust-lang.org/building/bootstrapping/what-bootstrapping-does.html)
(stage0 std → stage1 → stage2; stage1 differs because it is built with stage0 std — the *uplift*
subtlety), [kefir](https://sr.ht/~jprotopopov/kefir/) (stage1/stage2 SHA256 of `kefir` +
`libkefir.so` must match), [shecc](https://github.com/sysprog21/shecc) (stage1/stage2 byte-wise
identical), and thagore (Stage2 reproduces an identical Stage2'). The recurring law: **the
fixpoint is over the SELF-APPLIED transform, and only the second self-application certifies** —
the first stage inherits the seed's toolchain and is expected to differ.

**The on-the-nose precedent is a parser GENERATOR, not a compiler.** CPython's
[pegen](https://we-like-parsers.github.io/pegen/) (PEP 617, the production parser generator) is
exactly bbnf's problem class: a PEG meta-grammar `metagrammar.gram` with actions generates the
grammar-file parser, "meaning there are no manually-written parsers"
([van Rossum, A Meta-Grammar for PEG Parsers](https://medium.com/@gvanrossum_83706/a-meta-grammar-for-peg-parsers-3d3d502ea332)).
Its fixpoint is stated verbatim: the bootstrap meta-parser turns the meta-grammar into a new
meta-parser; **the new meta-parser reparses the same meta-grammar and the emitted output is the
same** — that identity is the correctness proof. This is (a) the *bytes-of-generated-parser*
fixpoint subject (matches skinny's `check_dir`), and (b) a working demonstration that a
seed-parser + actions is sufficient to bootstrap — the seed need not be a full hand parser. It is
a closer analogue than rustc (a whole compiler) and its fixpoint subject is precisely the DIRECTIVE
7A choice-(a).

**Constraint our evidence imposes.** The extant `check_dir` byte-gate (§0) is the *right subject
form* (generated `.rs` bytes) but is applied to a fixed generator, not to bbnf-generating-bbnf.
For a non-vacuous 7A the fixpoint must be: `emit(bbnf.bbnf)` produces a parser P; P re-parses
`bbnf.bbnf` into GrammarIr G'; `emit(G')` must equal `emit(bbnf.bbnf)` **byte-for-byte** — the
loop must route grammar *content* through the generated parser, else it certifies nothing
(the DIRECTIVE's exact caution).

**Options for the fixpoint subject:**
- (a) **generated `.rs` bytes** (matches the whole external canon and the extant `check_dir`
  machinery; cheap; but only meaningful once the loop passes grammar content through P);
- (b) **GrammarIr equivalence** (`G ≡ G'` under a named relation — see 7D §4) — stronger, immune
  to cosmetic-emit drift, but needs the relation pinned first.
  Evidence leans (a)-as-gate + (b)-as-oracle: bytes are the CI artifact, IR-equivalence is the
  semantic witness that the bytes aren't vacuous.

**Traps named in comparable systems.**
- **Trusting-trust / committed-seed provenance.** [Ken Thompson's "Reflections on Trusting
  Trust"](https://www.cs.cmu.edu/~rdriley/487/papers/Thompson_1984_ReflectionsonTrustingTrust.pdf)
  and the [Bootstrappable Builds](https://bootstrappable.org/) / GNU Mes
  [stage0](https://www.gnu.org/software/mes/manual/html_node/Stage0.html) program: a self-hoster
  needs a **minimal, auditable, committed stage-0** — a large *building* seed inflates the trusted
  surface toward the hand-parser's own LOC, dissolving the auditability the bootstrap is for. The
  `seed_LOC` headline must be re-derived on skinny and traded against a total self-host: if the
  seed grows to emit builders it approaches the 2142-LOC hand grammar frontend (`R1 §1`), and the
  win evaporates. Concrete proof the byte-identical fixpoint is *achievable across divergent seed
  toolchains*: at the 2023 Reproducible Builds Summit, GNU Guix, Nix, and Debian each built the
  initial Mes compiler with a **different major GCC version** yet produced a bit-for-bit identical
  Mes binary
  ([reproducible-builds.org](https://reproducible-builds.org/news/2022/05/18/jan-nieuwenhuizen-on-bootrappable-builds-gnu-mes-and-gnu-guix/)) —
  the [diverse double-compilation](https://reproducible-builds.org/news/2019/12/21/reproducible-bootstrap-of-mes-c-compiler/)
  technique that operationalizes Thompson's trusting-trust countermeasure. The lesson for 7A: the
  fixpoint's *value* is that it holds independent of the seed that produced the first stage — so the
  gate must route grammar content through the generated parser (not a fixed emit path) for that
  independence to mean anything.
- **The uplift illusion.** Do not gate on stage1=stage2 (P built by the *hand* generator vs P
  built by P) — rustc proves these legitimately differ; only stage2=stage3 is the fixpoint.
- **Vacuous determinism.** §0's fixed-const trap: any triple-identical claim over an emit path
  that ignores its input is disinformation (aligns with SPEC §0 "warm benches are disinformation"
  discipline — the gate must be able to *fail*).
- **Born-RED for the parser augment.** The three bounded parser gaps (`?w`→`->` attach in
  `parse_seq`; `@{}` atom in `parse_atom`, seam at `grammar/src/lib.rs:1495`; `|params|` closures)
  are what block routing `bbnf.bbnf` through the emitter at all — the born-RED gate is
  "`bbnf.bbnf` parses + the augmented emitter reparses the corpus" before any fixpoint talk.

---

## 2. TOTAL-SELF-HOST (7B) — prior art, constraints, traps

**What exists.** The one generator has THREE lowering paths (`json_sink_direct`,
`css_scan_direct`, `general_descent`); the shape-dispatch to the general path is
`!has_rule("value")` (`runtime_generator.rs:98`) — shape-based, not name-based, and `bbnf.bbnf`
(no `value` rule) shape-dispatches there. The general path is grammar-NEUTRAL by construction
(`general_descent/mod.rs:11-14`). This is the lawful spine a total self-host must ride — NOT a
bespoke `emit_typed_bbnf_*` cohort (the refuted legacy, §0).

**The recognizer-then-builder split has a validated architectural precedent: Ohm.**
[Ohm](https://ohmjs.org/docs/syntax-reference) (successor to
[OMeta](https://en.wikipedia.org/wiki/OMeta), Warth) *separates* the grammar — "purely about
recognition, no semantic actions" — from the action layer bolted on afterward. This is exactly
the general emitter's shape: a `parse_<rule>` **recognizer** (recognition-only, §0) that a `-> T`
**builder** augment would decorate. Ohm demonstrates the two layers *can* be authored and evolved
independently over one grammar — but it does NOT settle the skinny-specific open question (does the
builder augment reparse the corpus into GrammarIr-equivalent structure while `regen∘regen` holds
byte-identical?): Ohm builds an in-memory CST via generic semantics, whereas skinny's builder must
emit `add_expr`/`resolve_refs` calls that reconstruct the *same arena IR* the hand parser builds.
The precedent legitimizes the split as an architecture; the round-trip remains RED-unknown (§5).

**Scope axis (the LOCK-3 join).** SPEC LOCK-3C ("GRAMMAR-HOMOIOUSIA: the IR is itself
grammar-specified ... transpositions are grammar-to-grammar maps; the generator eats itself —
bootstrap alignment with LOCK-7") is the declared meeting point. If LOCK-3 adopts it, an `ir.bbnf`
becomes a same-wave deliverable whose consumer is the lowering tower (Y.13: consumer-less
substrate forbidden); its node values must be `->`-expressible. If LOCK-3 does not, 7B's totality
stops at the grammar+regex planes and the IR/emitter-templates remain hand code — name what
remains rather than assert total self-host. **This is a scope question for synthesis, not a route
to seed.**

**Structure-scoping (the borrow-checker escape question).** Prior art on "the generator becomes
mostly data": [PetitParser / meta-grammar](https://github.com/petitparser) and PEG bootstraps
express *structure* (rule towers) as grammar data, but control-flow / tape-assembly / cursor
mutation resist grammar-plane expression — they are the **kernel escape**. The measurable question
(DIRECTIVE 7B): on ONE emitter blob (e.g. `general_descent/emit.rs` render_rule), the
data-vs-escape ratio = {literal/first-set/dispatch tables derivable from grammar nodes} vs {the
`&mut TapeBuilder` cursor plumbing, `mark()`, loop-progress guards}. **Named kernel-escape
signature:** any construct requiring `&mut TapeBuilder`/`&mut usize cursor` aliasing or
borrow-split (`emit.rs:161,251,448` "recognizers take `cursor: &mut usize`; here we bind a local
alias") is escape — it cannot be grammar-plane data without a runtime interpreter, which SPEC §0
forecloses (fold-over-cursor dead; interpretive automata is the *anti*-differentiator, §1). A
research answer: report the ratio as a measured fact per blob, not a totality route.

**Traps.** (i) Resurrecting a bespoke emitter to hit a totality % is the §0 refutation. (ii) A
"grammar-as-data" control layer that becomes a runtime interpreter regresses the whole
performance thesis (SPEC §1: "LR/PEG generators emit interpretive automata" is precisely what
bbnf beats). (iii) LOC-budget: SPEC/`generated-size-budget` memory — a self-host that inflates
generated LOC past budget blocks the wave.

---

## 3. HOST-FUNCTION-LATTICE (7C) — prior art, constraints, traps

**The controlling theory is attribute grammars.** The lattice split maps exactly onto
[S-attributed vs L-attributed grammars](https://en.wikipedia.org/wiki/Attribute_grammar)
(Lewis–Rosenkrantz–Stearns 1974): **synthesized attributes** (leaf-up, one-pass) are what `->`
projections already express (`grammar/src/lib.rs:1481` `parse_projection` → `ExprKind::Project`);
**inherited attributes** (context passed *down/laterally* — scopes, table-paths, cross-references)
are the context-dependent semantics that need a host function. Wirth's Pascal semantic phase and
the [Silver/MELT](https://melt.cs.umn.edu/silver/tutorial/4_attribute_grammars/) and
[Tahr](https://arxiv.org/html/2512.01872v1) frameworks are the prior art: they bound "how much is
grammar" by exactly this synthesized/inherited frontier. **This gives 7C a principled floor: a
semantic is grammar-expressible iff it is synthesized; inherited semantics are the lawful host
surface** (aligns with the `hybrid-grammar-host` memory law).

**The frontier is a PERFORMANCE boundary, not an expressibility one — a load-bearing refinement.**
The classical result (Knuth 1968; restated in the
[attribute-grammar](https://en.wikipedia.org/wiki/Attribute_grammar) literature) is that
**synthesized attributes are semantically complete: any inherited attribute can be re-expressed as
a set of synthesized attributes reproducing the same information.** So the host-fn boundary at the
inherited frontier is NOT a hard limit on totality — inherited context (table paths, scopes) can in
principle be lifted into synthesized form, pushing totality % arbitrarily high. What forecloses the
lift is COST: the standard construction pays *additional tree passes* (or thunked closures), which
SPEC §0's single-pass, fold-over-cursor-dead, zero-runtime-cost discipline forbids on the hot path.
This sharpens 7C's counter (§below): the denominator's "inherited" leaves are host-bound *by the
performance law, not by expressibility* — and the totality floor-gate must be justified as "grammar
+ synthesized-only within one left-to-right pass", the exact frontier tree-sitter's external
scanner draws for the same reason (state beyond one pass ⇒ host C).

**The deployed reference implementation of a host-fn boundary is tree-sitter's external
scanner.** tree-sitter draws the totality line at *lexical* rules a regex cannot express —
Python's indentation, heredocs, contextual keywords — and hands them to hand-written C via a
declared 5-function contract:
[`create`, `destroy`, `serialize`, `deserialize`, `scan`](https://tree-sitter.github.io/tree-sitter/creating-parsers/4-external-scanners.html),
with the external token set named in the grammar's `externals` array (the typed signature at the
grammar/host seam — the `@host` analogue). Two lessons bind 7C: (i) the boundary is drawn at
*regex-inexpressibility*, matching the synthesized/inherited frontier — a leaf that needs state
beyond one left-to-right pass crosses to a host fn; (ii) **the scanner's `serialize`/`deserialize`
must capture its ENTIRE state** — tree-sitter's own warning — which is the concrete form of the
CONTEXT-tier zero-cost/serialization trap: an inherited-context host fn (TOML `push_path`,
GN-5.W2) carries mutable state whose cost and round-trip must be *measured*, not assumed
zero (§0 "type plane costs zero at runtime" is a claim the CONTEXT tier must re-earn, not
inherit).

**Independence split (from the census).** LOCK-free items: kill the `passes/src/lib.rs`
heuristic (`has_parse_time_host_decode` at `:798`, a `name.contains("host") &&
value.contains("decode"|"parse")` string-sniff — a name-gate-shaped heuristic, GN-5.W3
`check-name-gates` territory); lower `@{}` at the atom seam; `resolve_refs` is the reference
example of a *pure* host mechanism (deterministic, context-free-of-external-state — §0). LOCK-1-
bound items: a typed host-fn signature threaded into inference, and any tier keyed off `&mut Ctx`,
wait on LOCK-1's inherited-attribute verdict.

**Totality % counter (a GN-5 `check-name-gates` sibling).** Specify it as **leaves × branches**:
denominator = every grammar leaf (terminal/regex) + every alternation branch reachable from entry;
numerator = those whose semantics resolve to a `->` projection or a *synthesized*-only host fn (no
inherited context). A per-subsystem **floor gate** (e.g. "≥X% of bbnf/regex leaves grammar-bound")
sits beside `check-name-gates` in xtask. **Falsification (precept-19, must be able to fail):**
plant a leaf that routes to an inherited-context host shim and assert the counter *drops* below
the floor — a counter that cannot register a breach certifies nothing (the §0 discipline).

**CONTEXT-tier zero-cost testability.** Before scheduling the inherited tier, confirm the
zero-cost invariant is *measurable*: `resolve_refs` round-trip (grammar→IR→resolve→pretty→reparse
is identity) and the TOML `push_path` inherited-context case (GN-5.W2 authors TOML; its table-path
nesting is the canonical inherited-attribute test) must show zero runtime residue — SPEC §0 "type
plane costs zero at runtime (verified invariant)".

**Traps.** (i) A host fn that hard-codes grammar semantics (GN-5.W2 R2: a TOML-datetime host shim)
is a **name-gate in disguise** — the counter/gate must audit host fns, or the datetime leaf stays
grammar-expressed (`no-workarounds-self-hosting`). (ii) L-attributed evaluation assumes one-pass
left-to-right; a semantic needing multi-pass/circular attributes exceeds the lattice and must be
named, not silently shimmed.

---

## 4. META-CIRCULAR-ORACLE (7D) — prior art, constraints, traps

**What exists and works.** parse-that's `parse_generated` (§0) IS a deployed meta-circular oracle:
the bbnf-defined `regex.bbnf` generates a parser whose output is `host::lower`ed to `Hir` and
verified against the hand `parse()`. The record is corrected: it is a **producer**, and the
regex-grammar self-host is not speculative.

**Prior art on the oracle discipline.** The differential-testing literature
([Grammar Mutation for Testing Input Parsers, TOSEM 2025](https://dl.acm.org/doi/10.1145/3708517);
[AsFuzzer](https://www.researchgate.net/publication/383963161_AsFuzzer_Differential_Testing_of_Assemblers_with_Error-Driven_Grammar_Inference))
teaches two load-bearing rules for a generated-vs-hand oracle: (1) **golden-anchor, not
majority-vote** — when two implementations disagree, a third (the committed hand parser as the
golden reference) breaks the tie; a majority vote among near-clones hides shared bugs; (2)
**planted-mutation falsification** — inject a known divergence and confirm the oracle catches it,
else the oracle's silence is meaningless (the SPEC §0 "gate must be able to fail" discipline,
mirrored in GN-5.W3's falsification test).

**The equivalence relation must be pinned before any parity %.** The DIRECTIVE names the
canonical normalization pitfall: `Empty ≡ Concat([])` (a generated parser may emit a singleton
`Seq` where the hand parser emits the bare node — structurally distinct, semantically equal). The
relation must (i) normalize such trivial-wrapper forms, and (ii) run the **shared semantic passes
on both sides or neither** — `resolve_refs` + `validate` (`ir/src/lib.rs:84,110`) applied to both
generated-IR and hand-IR, so the comparison is post-canonicalization. **Pin a relation-id
(e.g. `bbnf-ir-eq-v1`) and quote parity % only against it.**

**Regex divergences: host obligations vs oracle verdicts (settled substrate, XL-C).** On the
settled two-type regex substrate (grammar-tier `Regex(StringId)` opaque, engine-tier rich `Hir`,
bridged by `simplify_hir`), a divergence between `parse_generated` and `parse()` is an
**engine-tier `host.rs` lowering obligation** (the `host::lower` fold, `regex/mod.rs:34`) when it
is a lowering choice (e.g. class-set simplification order), and an **oracle verdict** (a real
parser bug) only when the pre-lowering ASTs differ. The oracle must compare at the AST layer
*before* `host::lower` to separate the two.

**Promotion-ledger schema (draft).** A subsystem-by-subsystem table gating promotion of a
generated parser to production:

| subsystem | parity% (rel-id `bbnf-ir-eq-v1`) | totality% (7C counter) | perf vs hand (cyc/B, cold paired) | planted-mutation caught? | golden-anchor | disposition |
|---|---|---|---|---|---|---|
| regex | … | … | … | Y/N | hand `parse()` | oracle-only / promote |
| bbnf  | … | … | … | Y/N | hand `parse_grammar` | oracle-only / promote |

Promotion is gated on **measured parity + perf**, not on the self-host existing — SPEC §0's
implementation-majority + the LOCK-7D charter ("promotion gated on measured parity + performance,
subsystem by subsystem"). The totality% column sources 7C's counter (§3); the golden anchor is
always the committed hand parser, never a vote.

**Traps.** (i) Quoting parity % before pinning the relation-id (the `Empty ≡ Concat([])` family
inflates false-divergence). (ii) Asymmetric semantic passes (running `resolve_refs` on one side
only) manufactures spurious mismatches. (iii) Majority-vote oracles over near-clones (shared-bug
blindness). (iv) Treating a `host::lower` normalization difference as a parser bug (mis-routing a
host obligation to an oracle verdict).

---

## 5. Cross-cutting constraints from the evidence base (carry, do not re-derive)

- Regex substrate SETTLED (XL-C): two domain-fit types; the vendored parse-that-regex in skinny is
  the measurement substrate; the top-level duplicate is legacy (GN-0 archive).
- `parse_generated` is a producer (corrected here and in §0/§4).
- The general emitter is recognizer-only; a builder augment is OPEN/unseeded — do not presume
  round-trip from the legacy emitter or the emitter's mere existence (the shared open empirical
  question, still RED-unknown).
- x86 dead; no digit-scan SIMD; no block-scheduling; fold-over-cursor dead; warm benches are
  disinformation; type plane costs zero at runtime (SPEC §0 laws — bind 7B's structure-scoping and
  7C's zero-cost tier).
- The legacy bespoke `emit_typed_bbnf_*` self-host is a NAMED refutation (overfit audit) — any
  re-author routes through the grammar-neutral general emitter.

---

## Sources

Codebase (`path:line`): `skinny/xtask/src/{main.rs,regen.rs,regen_css.rs}`;
`skinny/crates/codegen/src/{runtime_generator.rs, general_descent/mod.rs, general_descent/emit.rs}`;
`skinny/crates/ir/src/lib.rs`; `skinny/crates/grammar/src/lib.rs`;
`parse-that/rust/bootstrap/{Cargo.toml,regex.bbnf,src/lib.rs}`;
`parse-that/rust/parse_that/src/regex/mod.rs`;
`restart/audit/skinny-impl-overfit/V1/AGENT-3-pattern-h-runtime.md`.

Web: [rustc bootstrapping](https://rustc-dev-guide.rust-lang.org/building/bootstrapping/what-bootstrapping-does.html) ·
[kefir](https://sr.ht/~jprotopopov/kefir/) ·
[shecc](https://github.com/sysprog21/shecc) ·
[pegen (PEP 617)](https://we-like-parsers.github.io/pegen/) ·
[PEP 617 — New PEG parser for CPython](https://peps.python.org/pep-0617/) ·
[van Rossum, A Meta-Grammar for PEG Parsers](https://medium.com/@gvanrossum_83706/a-meta-grammar-for-peg-parsers-3d3d502ea332) ·
[GNU Mes stage0](https://www.gnu.org/software/mes/manual/html_node/Stage0.html) ·
[Mes reproducible bootstrap / diverse double-compilation](https://reproducible-builds.org/news/2019/12/21/reproducible-bootstrap-of-mes-c-compiler/) ·
[Nieuwenhuizen on bootstrappable builds (3-distro bit-identical Mes)](https://reproducible-builds.org/news/2022/05/18/jan-nieuwenhuizen-on-bootrappable-builds-gnu-mes-and-gnu-guix/) ·
[Bootstrappable Builds](https://bootstrappable.org/) ·
[Thompson, Reflections on Trusting Trust](https://www.cs.cmu.edu/~rdriley/487/papers/Thompson_1984_ReflectionsonTrustingTrust.pdf) ·
[Ohm syntax reference](https://ohmjs.org/docs/syntax-reference) ·
[OMeta (Wikipedia)](https://en.wikipedia.org/wiki/OMeta) ·
[tree-sitter external scanners](https://tree-sitter.github.io/tree-sitter/creating-parsers/4-external-scanners.html) ·
[Attribute grammar (Wikipedia)](https://en.wikipedia.org/wiki/Attribute_grammar) ·
[Silver/MELT attribute grammars](https://melt.cs.umn.edu/silver/tutorial/4_attribute_grammars/) ·
[Tahr generative attribute grammar](https://arxiv.org/html/2512.01872v1) ·
[Grammar Mutation for Testing Input Parsers (TOSEM 2025)](https://dl.acm.org/doi/10.1145/3708517) ·
[AsFuzzer differential testing](https://www.researchgate.net/publication/383963161_AsFuzzer_Differential_Testing_of_Assemblers_with_Error-Driven_Grammar_Inference).
