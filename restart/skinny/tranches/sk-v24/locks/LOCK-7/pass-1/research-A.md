# LOCK-7 / Family 7A — STAGED-BOOTSTRAP (research, pass 1)

Charter: the classic two-stage bootstrap for grammar totality — committed stage-0 artifacts,
a fixpoint gate (`regen∘regen = regen`, byte-identical), a **minimal** bootstrap surface, and
**maximal** auditability. Constitution clause (i): everything is grammar-driven, here and
there, save primitives and combinators — the regex grammar IS bootstrapped and bbnf-defined.
This family's thesis: totality is not a percentage to chase (that is 7C's lattice) nor an
oracle to promote (7D) — it is a **closure property, proven by self-application and enforced
byte-for-byte in CI**, over a trusted seed small enough to read.

## 1. The ground truth — what self-hosting already exists, and where it regressed

Self-hosting is **not greenfield**; it is built, then abandoned in the tree the greenfield
inherits. Two facts must be held together:

**(a) The legacy `crates/core` tree self-hosts fully.** `grammar/bbnf/bbnf.bbnf` (89 LOC, with
`@import` of `expressions.bbnf` + `types.bbnf`) is compiled by `cargo xtask regen` into
`crates/core/src/grammar/generated/bbnf.rs` — **21,557 LOC / 1.09 MB of committed generated
parser**. The whole grammar fleet is bootstrapped the same way: `bbnf.rs, bnf.rs, ebnf.rs,
css_l4.rs (6.1 MB), css_pretty.rs, csv.rs, google_sheets.rs, json.rs, math.rs`, each with a
`<name>.registry.json` companion. The self-host entry is a single function:
`bbnf::grammar::generated::BbnfBootstrap::parse` (re-exported thinly by
`crates/bootstrap/src/lib.rs`, one `pub use` line). bbnf parses its own grammar-of-grammars
with a bbnf-generated parser **today** — the closure exists, it merely lives in the retired tree.

**(b) The greenfield seed (`skinny/`) has REGRESSED to a hand parser.** The skinny `grammar`
crate is `crates/grammar/src/lib.rs:29 pub fn parse_grammar(name, source) -> GrammarIr` —
**2142 hand-written LOC** (R1 §1), no generated bbnf parser, and the xtask regen targets are
`json / sheets / css / real-typed` ONLY (`xtask/src/main.rs:39-52`) — **there is no
bbnf-parses-bbnf target and no regex-parses-regex target in skinny.** Because SPEC §1 retires
skinny "as a tree" and the GN-series ports its organs forward, **the greenfield is born
WITHOUT self-hosting unless 7A re-lands it.** Constitution clause (i) is therefore currently
UNMET for the two grammars that matter most to it — bbnf and regex.

**(c) parse-that is the isomorphic mirror, and it too self-hosts.** `rust/bootstrap/regex.bbnf`
(72 LOC, full L4 regex) → xtask regen → `parse_that::regex::generated` (`generated.rs`, **2293
LOC committed**), surfaced by the thin `regex-bootstrap` crate (`pub use
parse_that::regex::generated::{RegexParser, parse_generated}`). Its header records the exact
shape 7A must preserve: the proc-macro path (`bbnf_derive::Parser` + `cargo expand` +
`scripts/bootstrap-regex.sh` python post-processing) was **retired** (bbnf-lang commit
`6142387f`); the generated parser is now committed source emitted by `cargo xtask regen`, and
the bbnf-side mirror (`BbnfBootstrap`) is called out as the analogue. One `xtask fixpoint`
gate can therefore cover BOTH repos' clause-(i) grammars — the bootstrap is physically
isomorphic (LOCK-6's bbnf/parse-that face by construction).

## 2. The mechanism made concrete — the fixpoint the tree does not yet have

What exists is a **staleness / idempotency gate**, not a fixpoint gate. Two mechanisms live in
`skinny/xtask/src/regen.rs`:

- `check_target` (`regen.rs:37`) → `EmittedSource::check_dir`: recompute `emit(source)` and
  fail if it differs from the committed bytes. This is **one application** — it proves the
  committed artifact is fresh w.r.t. the *current* generator. It does **not** run the
  generated parser at all.
- The legacy idempotent write-skip (`crates/bootstrap/src/lib.rs` header, "Post-B6.W0"):
  `regen` skips the on-disk write when emitted bytes equal existing bytes, dropping cold-wall
  regen ~88 s → ~0.5 s. This is a **build-cache** optimization on generator determinism, and
  it is dangerous to 7A precisely because it can *mask* a fixpoint failure: it never forces the
  grammar source to be re-parsed by the generated parser.

Neither is `regen∘regen = regen` in the rustc sense. The staged bootstrap 7A must add:

- **Stage 0** — the committed generated parser artifacts (`grammars/bbnf/*.rs`,
  `grammars/regex/*.rs`), the auditable checkpoint.
- **Seed (trusted base)** — the hand `parse_grammar` (2142 LOC) is the stage-(−1) compiler that
  bootstraps stage 0 from source. It is the *only* unauditable-by-regen code and must be
  minimized and quarantined (§4).
- **Stage 1** — seed parses `bbnf.bbnf` → passes → codegen → emit parser **P1**.
- **Stage 2** — **P1 (the generated parser) parses `bbnf.bbnf`** → passes → codegen → emit **P2**.
- **The gate**: `blake3(P1) == blake3(P2) == blake3(stage-0 committed)`, byte-identical, in CI,
  born-RED. Only stage 2 running the grammar source *through the generated parser* proves the
  generated parser accepts the grammar that defines it — the self-application closure. This is
  exactly rustc's stage1≡stage2 invariant and Hexa-lang's gen3≡gen4 native fixpoint.

**Concrete xtask surface** (mirrors the existing `RuntimeTarget` machinery verbatim):

1. Add `RuntimeTarget { grammar_name:"bbnf", entry_rule:"grammar", source_inputs:
   ["grammar/bbnf/bbnf.bbnf","grammar/bbnf/expressions.bbnf","grammar/bbnf/types.bbnf"],
   output_dir:"crates/<runtime>/src/grammars/bbnf", ... }` and the twin `regex` target
   (`grammar/misc/regex.bbnf`, entry `regex`) — structurally identical to `JSON_TARGET`
   (`xtask/src/main.rs:177`) and `SHEETS_TARGET` (`:200`). Wire `regen-bbnf-self` /
   `check-bbnf-self` / `regen-regex-self` / `check-regex-self` into the dispatch (`main.rs:39-52`).
2. Add `xtask fixpoint`: run stage 1 via the seed; **rebind** the grammar-source front-end to
   the freshly generated parser (a codegen entry that consumes a `dyn GrammarFrontend` rather
   than calling `parse_grammar` directly — the one seam that must be introduced); run stage 2;
   compare in memory (no write). The `validate_inputs` blake3 hasher already present
   (`regen.rs:154-166`) is the substrate — extend it from *input* hashing to *emitted-byte*
   hashing so stage1/stage2/stage0 digests are one comparison. `check_dir` already gives the
   stage-0 half free.
3. Budget the seed like code (`lint-loc`, `xtask/src/main.rs:259-311` is the template): a
   `trusted-seed` LOC budget on `crates/grammar` that only ever ratchets DOWN — the Guix
   reduced-binary-seed ethos made a number.

## 3. The substrate — what already carries the design

- **Determinism substrate**: regen already sorts via `BTreeSet` (`regen.rs:89-101`
  `validate_unique_targets`) and hashes inputs with blake3 — byte-identity is within reach, but
  the whole `codegen` emit path (10201 LOC, R1 §2) must be audited for HashMap-iteration /
  timestamp non-determinism (rustc's canonical fixpoint-breaker). The `.registry.json`
  companions are already deterministic sidecars.
- **The re-export seam is proven**: `crates/bootstrap/src/lib.rs` and `rust/bootstrap` show the
  exact "generated parser lives in the runtime crate, thin crate re-exports the canonical type"
  pattern for both repos. 7A does not invent a shape — it re-lands a retired one into the
  greenfield and adds the missing gate.
- **The check-dir gate is the enforcement rail**: `EmittedSource::check_dir` + the
  `check-<grammar>` commands already fail CI on staleness; `check-fixpoint` slots beside them.
- **Grammar totality is already broad**: `bbnf.bbnf` is expressed in bbnf (imports, `->`
  projections, `@pretty` directives), `regex.bbnf` in bbnf, `ebnf.bbnf`/`bnf.bbnf` in bbnf —
  the corpus for the differential soundness guard (§4) already exists in `grammar/`.

## 4. The trusted seed — minimality and auditability, made rigorous

"Minimal bootstrap surface, maximal auditability" is the family's discriminating clause, and it
has a precise reading from the bootstrappable-builds literature (Guix Reduced Binary Seed; GNU
Mes; Thompson's *Reflections on Trusting Trust*): the trusted computing base is exactly what
cannot be re-derived by a byte-checkable regen. Here that base is `{ parse_grammar seed +
codegen backend }`. 7A's honest floor:

- **Bootstrap the front-end, not the whole generator.** The `codegen` emit backend (10201 LOC)
  is a *primitive* under clause (i) ("save primitives and combinators") — 7A does NOT attempt
  to grammar-specify it (that is 7B's ambition). 7A drives the seed→fixpoint to closure on the
  **parser** and names the backend as trusted primitive. This is what keeps 7A *minimal*.
- **Stage the language so the seed shrinks.** The seed need only parse the sub-language in which
  `bbnf.bbnf` itself is written — core EBNF (`= ; | , () [] {} * + ? ?w` and `/regex/`,
  `"literal"`, `@import`). It does **not** need `->` value-expressions, `@host`, `@pretty`,
  closures, or `<<`/`>>`/`-` binary operators to *parse the grammar-of-grammars* (those are
  needed to parse rich grammars, and the GENERATED parser supplies them). Define **bbnf-core**
  = the seed's accepted language; the seed's LOC budget is measured against bbnf-core, and every
  feature not in bbnf-core is proof the seed is smaller than the language. Target: the trusted
  seed is a strict, readable subset — Mes's "≈5000 lines of simple C" is the spiritual bound.
- **Soundness guard against false fixpoints** (the named counterexample below): a fixpoint on
  `bbnf.bbnf` alone can pass while seed and generated parser accept *different languages*. 7A
  therefore carries a bounded differential: parse the whole `grammar/` corpus
  (`json/css_l4/ebnf/bnf/csv/google_sheets/regex`) with BOTH seed and generated parser and
  assert equal `GrammarIr`. This is a *bounded* version of 7D's oracle — 7A needs it only as the
  fixpoint's guard, not as a promotion mechanism.

## 5. Prior art (citations)

- **rustc stage0/1/2**: stage0 precompiled → builds stage1 → stage1 builds stage2; **if stage1
  and stage2 binaries are byte-identical the compiler is a fixpoint**. The exact gate 7A adopts.
  (rustc-dev-guide, "What Bootstrapping Does".)
- **Hexa-lang**: self-hosting native compiler advertising a **byte-identical gen3 ≡ gen4
  fixpoint** with citation-enforced stages — a second independent instance of the double-regen
  gate as the correctness proof.
- **Guix Reduced Binary Seed / Full-Source Bootstrap / GNU Mes**: reducing the trusted binary
  seed (250 MB → ~60 MiB → 25%) toward a fully source-auditable path; the discipline of
  *minimizing the unauditable seed* is precisely 7A's §4. Rooted in **Ken Thompson, "Reflections
  on Trusting Trust"** — the WHY of a small readable seed.
- **T-diagram bootstrapping / metacircular front-ends**: the classical frame; 7A is the
  front-end-only, byte-checked instance.

Sources: [rustc-dev-guide bootstrapping](https://rustc-dev-guide.rust-lang.org/building/bootstrapping/what-bootstrapping-does.html) ·
[Hexa-lang](https://github.com/dancinlab/hexa-lang) ·
[Guix Reduced Binary Seed](https://guix.gnu.org/manual/en/html_node/Reduced-Binary-Seed-Bootstrap.html) ·
[Guix Full-Source Bootstrap](https://guix.gnu.org/en/blog/2023/the-full-source-bootstrap-building-from-source-all-the-way-down/) ·
[GNU Mes / bootstrappable](https://www.gnu.org/software/mes/manual/html_node/Bootstrappable-Builds.html)

## 6. Named risks and counterexamples

1. **Two-parser divergence — HAS HAPPENED.** The legacy tree already suffered two
   `BbnfBootstrap` types (proc-macro-relocated vs raw-expansion) that DIVERGED and had to be
   unified (`crates/bootstrap/src/lib.rs` header, "per W0-per-grammar-boundary §3.3"). Risk:
   seed and generated parser accept different languages → fixpoint on `bbnf.bbnf` passes
   vacuously. Mitigation = the §4 differential guard. This is the single most likely way 7A
   ships a *green but wrong* gate.
2. **Emit non-determinism → false RED.** HashMap iteration order, embedded paths/timestamps, or
   float formatting in codegen break byte-identity and fail the fixpoint spuriously (rustc's
   perennial reproducibility tax). Must audit the full `codegen` path; the existing BTreeSet
   discipline is necessary but not sufficient.
3. **Idempotent write-skip masks staleness.** The Post-B6.W0 mtime-preserving write-skip must be
   *disabled* inside `xtask fixpoint` (compare in memory), or a real stage1≠stage2 drift hides
   behind an unchanged file. Speed optimization and correctness gate must be separate code paths.
4. **Seed creep.** Every new bbnf feature tempts growing the seed; that inflates the trusted
   base and violates minimality. Counter: bbnf-core is FROZEN as the seed's language; new
   features land only in the generated parser. If a feature is genuinely needed to *parse
   bbnf.bbnf*, that is a deliberate, reviewed seed-budget increase — never incidental.
5. **Double-regen build cost.** A fixpoint run is two full regens (bbnf 21.5k LOC, css_l4 6 MB);
   at ~88 s cold-wall each, CI cost is real. Prototype must measure whether the double pass is
   affordable or must be gated to a nightly / pre-merge lane while `check-<grammar>` staleness
   runs per-commit.
6. **Generator-is-not-bootstrapped is an honest boundary, not a hole.** 7A deliberately leaves
   `codegen` as trusted primitive. A critic will call this "incomplete totality"; the answer is
   that 7A's value proposition IS the minimal, auditable boundary — pushing the boundary into
   the backend is 7B/7C's trade, measured against build-time and debuggability, not 7A's.

## 7. What a prototype must test to move this family

1. **Wire `bbnf-self` regen in skinny** (a `RuntimeTarget` clone) and confirm the emitted bbnf
   parser (a) COMPILES and (b) parses `bbnf.bbnf` to the same `GrammarIr` the hand
   `parse_grammar` produces. Proves the front-end is regenerable in the greenfield seed at all.
2. **Implement `xtask fixpoint` and run it on the legacy `bbnf.rs`/`regex generated.rs`
   artifacts**: is `regen∘regen` byte-identical TODAY, or does it drift? A measured yes de-risks
   the whole family; a measured no localizes the non-determinism (risk 2) before GN.
3. **Measure the trusted seed**: strip `parse_grammar` to bbnf-core; report the LOC of the
   minimal seed that still parses `bbnf.bbnf` + `regex.bbnf`. This number is the family's headline.
4. **Determinism audit**: two clean regens, `diff` the bytes across the whole `grammars/` output.
   Any diff is a named non-determinism site with a fix.
5. **Differential soundness**: parse the full `grammar/` corpus with seed vs generated parser,
   assert `GrammarIr` equality — the guard against risk 1.
6. **Cost**: wall-time of the double-regen fixpoint vs single staleness check; decide the CI lane.

## 8. The integration seams (path:line)

- `skinny/xtask/src/main.rs:39-52` — dispatch; add `regen/check-bbnf-self`, `regen/check-regex-self`, `fixpoint`.
- `skinny/xtask/src/main.rs:177` `JSON_TARGET`, `:200` `SHEETS_TARGET` — the `RuntimeTarget` template to clone.
- `skinny/xtask/src/regen.rs:20 write_targets`, `:37 check_target`, `:154 validate_inputs` (blake3) — extend to emitted-byte hashing; the fixpoint comparator.
- `skinny/xtask/src/main.rs:259-311 lint_loc` — the template for the `trusted-seed` LOC budget.
- `skinny/crates/grammar/src/lib.rs:29 parse_grammar` — the trusted seed to minimize to bbnf-core.
- `skinny/crates/codegen/src/lib.rs:101 emit_from_source` + `runtime_generator.rs` — the front-end seam (`dyn GrammarFrontend`) and the determinism-audit surface.
- `crates/bootstrap/src/lib.rs` + `crates/core/src/grammar/generated/bbnf.rs` (legacy, 21,557 LOC) — the proven self-host artifact + re-export shape to re-land.
- parse-that `rust/bootstrap/regex.bbnf`, `parse_that::regex::generated` (2293 LOC), `regex-bootstrap` crate — the isomorphic regex bootstrap the same `fixpoint` gate covers.

## 9. Family verdict

Self-hosting to a byte-identical fixpoint is **the correct, minimal reading of clause (i)** and
it is ~80% built — but built in the tree being retired, and never gated for closure. 7A's
deliverable is small and hard-edged: re-land the bbnf and regex bootstraps into the greenfield
as committed stage-0 artifacts, add the double-regen fixpoint gate (stage1≡stage2≡stage0) with
the differential soundness guard, minimize and budget the trusted seed to bbnf-core, and name
the codegen backend as trusted primitive. It does not chase a totality percentage (7C) or a
promotion oracle (7D); it makes totality a *proven closure over a readable seed*. The
falsifiable prototype is `xtask fixpoint` returning byte-identical on `bbnf.rs` — run it and
the family either converges or hands GN a named non-determinism list.
