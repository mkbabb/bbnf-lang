# LOCK-7 · Family 7A — STAGED-BOOTSTRAP · research (pass 1)

The classic two-stage bootstrap: a committed stage-0 parser artifact for bbnf and for regex,
a byte-identical fixpoint gate (`regen∘regen = regen`), a minimal irreducible seed, and a
provenance ledger that makes the whole chain auditable in one command. This family does NOT
grammar-specify the IR, the emitter, or the oracle tables — its center is the parser
ladder and its reproducibility law. Everything above the parser stays hand-written primitive:
combinators, the codegen kernel, the semantic fold. The wager is that the smallest thing you
can self-host — the grammar front-end itself — bought with a rigorous fixpoint, beats a
sprawling total self-host on auditability and build cost, at the price of a fixed seed.

## 1. The ground truth at HEAD (what exists, what is missing)

**bbnf is self-hosting-CAPABLE but NOT self-hosted.** The skinny grammar front-end is a
hand-written recursive-descent parser: `skinny/crates/grammar/src/lib.rs:29`
`parse_grammar(name, source) -> GrammarIr` (`Parser::new` + `parser.parse_rules()` +
`grammar.resolve_refs()` + `grammar.validate()`), 2142 LOC, imperative, no combinators, no
generated artifact consumed. The self-hosting grammar `grammar/bbnf/bbnf.bbnf` (89 LOC +
`expressions.bbnf` 37 + `types.bbnf` 7) EXISTS as source of truth but is wired to nothing at
skinny HEAD — no `regen-bbnf` target, no committed generated bbnf parser. The only grammar in
`skinny/Cargo.toml [workspace.metadata.bbnf]` is `json`; the xtask dispatch
(`skinny/xtask/src/main.rs:40-47`) registers regen/check for json, css, sheets, real-typed —
never bbnf, never regex.

**regex IS self-hosted, but only as a verification lane.** parse-that carries a committed
stage-0: `parse_that/src/regex/generated.rs` (2293 LOC, header
`//! AUTO-GENERATED from bootstrap/regex.bbnf — do not edit manually`), embedding its own
grammar as `const GRAMMAR_RegexParser: [&str;1]`, consumed by
`parse_that/src/regex/mod.rs:33` `parse_generated(pattern) -> Hir` →
`parse_that/src/regex/host.rs:1` `lower` (524 LOC semantic fold, `RegexParserEnum` AST →
`Hir`). But `mod.rs` states this path is "available for benchmarking and verification against
the primary `parse()`" — the production regex parser is the hand-written
`bbnf_regex` engine (`parse_that/rust/regex/src/path_lexer.rs` 186 LOC + `hir/`). So the
generated regex parser is a consumer-less-in-production oracle today (a Y.13 liability this
family must resolve: promote it to the parser of record, or the substrate is banked).

**The stage-0 minting chain was proc-macro, now xtask.** `parse-that/rust/bootstrap/Cargo.toml`
records the migration: pre-B2, `#[derive(bbnf_derive::Parser)]` on a `RegexParser` marker was
`cargo expand`-ed by `scripts/bootstrap-regex.sh` into `regex/generated.rs`; the derive crate
was retired (bbnf-lang commit `6142387f`), and "the IR pipeline runs through bbnf-lang's
`cargo xtask regen`." `bootstrap/src/lib.rs` is now a one-line diagnostic re-export
(`pub use parse_that::regex::generated::{RegexParser, parse_generated}`). Its comment names the
mirror: bbnf-lang's `crates/bootstrap/src/lib.rs` was
`pub use ::bbnf::grammar::generated::BbnfBootstrap;` — that crate is ABSENT from skinny's
member list (`bbnf, bbnf-bench, bbnf-regex, bbnf-simd, codegen, grammar, ir, parse-that-regex,
passes, runtime, simd-scan, test-fixtures`). The skinny bbnf bootstrap crate was dropped in the
skinny excision.

**The fixpoint precedent is real and byte-exact was already done.** The pre-greenfield tree
carries `crates/core/src/grammar/generated/bbnf.rs` (21557 LOC, header
`//! AUTO-GENERATED from [workspace.metadata.bbnf.grammars]` / `Regenerate: cargo xtask regen
--grammar bbnf`), structured as `struct BbnfBootstrap` with the grammar embedded via
`include_str!(concat!(CARGO_MANIFEST_DIR, "/../../grammar/bbnf/bbnf.bbnf"))` and a baked
`__GRAMMAR_STRUCTURAL_ALPHABET`. A full generated bbnf-in-bbnf parser existed and regenerated
through xtask. This is the T-diagram proof-of-existence 7A rebuilds under a fixpoint law.

**The reproducibility infrastructure already exists — it is just not pointed at the self
grammars.** `skinny/xtask/src/regen.rs:37` `check_target` re-emits from source and calls
`emitted.check_dir(...)` — a byte-exact staleness assertion (`generated ... is stale; run
cargo xtask regen-*`). `regen.rs:153` `validate_inputs` computes a `blake3` digest over each
source + metadata input (path-tagged, NUL-separated, empty-input-rejected) and prints
`inputs: <profile> <hex>`. These two are, respectively, the fixpoint comparator and the
determinism anchor 7A needs — already load-bearing for json/css/sheets, never registered for
bbnf/regex.

**The extant GN codex omits the bootstrap entirely.** `docs/tranches/sk-v24/INDEX.md` has zero
bootstrap/self-host/fixpoint mentions; SPEC GN-1 ports xtask as "one regen host" with a CSV
target but no bbnf/regex self-host target. 7A is therefore a **named extension** of GN-1 (a
new `regen-bbnf`/`regen-regex` target pair + a fixpoint gate), not a re-cut of existing waves.

## 2. The mechanism, made concrete

Three artifacts and one law.

### 2.1 The seed floor (S₋₁ — the irreducible primitive surface)

Under constitution clause (i) primitives and combinators are exempt from grammar-drivenness.
7A draws the primitive line precisely at the parser, and no higher:

- **The combinator/runtime primitives** — parse-that combinators (regex stage-0 runtime) and
  the skinny tape runtime (bbnf stage-0 runtime). Hand-written, exempt.
- **The codegen kernel** — `skinny/crates/codegen/src/lib.rs:101` `emit_from_source(name,
  source) -> EmittedSource` and its lowering (`lower/`, `general_descent/`). Grammar IR → Rust.
  A primitive by fiat: the generator is not self-generated in 7A (that is a sibling family's
  wager, deliberately not taken here — the generator kernel stays hand code so the bootstrap
  surface is small and one-directional).
- **The semantic fold** — `regex/host.rs` `lower` (AST → Hir). A hand-written host kernel, not
  grammar. (7A treats folds as primitive; the typed-host-fn generalization is a sibling axis.)
- **The bbnf seed parser** — `grammar/src/lib.rs` `parse_grammar` (2142 LOC). This is the
  bootstrap SEED. Its disposition is the central 7A decision (§4, R4): frozen-forever vs
  retired-after-fixpoint.

The seed number is the family's headline metric: seed_LOC = grammar-hand-parser (2142) +
regex-host-fold (524) + the codegen kernel it invokes. 7A minimizes and *measures* this against
a total-self-host surface — the trade is auditability and single-direction dataflow bought with
a permanent hand parser.

### 2.2 Stage-0 (S₀ — committed generated artifacts)

Two checked-in `@generated` parsers, byte-frozen, each self-describing:

- `skinny/crates/runtime/src/grammars/bbnf/` (new) — the generated bbnf-in-bbnf parser, minted
  by `emit_from_source("bbnf", read(grammar/bbnf/bbnf.bbnf))`, mirroring the legacy
  `BbnfBootstrap` shape (embed the grammar via `include_str!`, bake the structural alphabet).
- `parse_that/src/regex/generated.rs` — the regex stage-0 (extant; adopted, not re-minted).

Each header carries a **provenance triple**: `{source path, blake3(source), blake3(emitted)}`,
committed to a `bootstrap.lock` manifest (the audit ledger). `validate_inputs`
(`regen.rs:153`) already computes the source digest; 7A adds the emitted digest and commits
both. This is the reproducible-build receipt: any auditor re-runs regen and diffs the lock.

### 2.3 The fixpoint gate (the heart of the family)

New xtask `RuntimeTarget`s registered beside `JSON_TARGET` (`main.rs:177`) and `SHEETS_TARGET`
(`main.rs:200`): `BBNF_TARGET` (sources `grammar/bbnf/{bbnf,expressions,types}.bbnf`, entry
`grammar`) and `REGEX_TARGET` (source `grammar/misc/regex.bbnf` or the parse-that
`bootstrap/regex.bbnf`, entry `regex`), with dispatch arms `regen-bbnf`/`check-bbnf`/
`regen-regex`/`check-regex` at `main.rs:40-47`. The gate has two rungs:

- **Rung 1 — staleness (extant mechanism).** `check-bbnf` re-emits and `check_dir`-compares
  against committed S₀. This is `check_target` (`regen.rs:37`) verbatim, newly pointed at the
  self grammar. GREEN ⟺ the committed S₀ is what the current generator + current grammar emit.
- **Rung 2 — the stage ladder (the 7A novum).** The load-bearing claim: the parser that PARSES
  bbnf.bbnf and the parser that IS regenerated are the same generated parser. Concretely:
  build S₀ into the regen host; use the *generated* bbnf parser (not the hand parser) to parse
  `grammar/bbnf/bbnf.bbnf`; emit S₁; assert `sha256(S₁) == sha256(S₀)`. Then emit S₂ from S₁;
  assert `S₁ == S₂` (no 2-cycle — a true fixed point, not an oscillation). This is
  `regen∘regen = regen`, the reproducing-bootstrap law, expressed as one CI target
  (`cargo xtask fixpoint-bbnf`).

The distinction from Rung 1 is exactly the distinction between "generated output is
committed" (a codegen-freshness check, which json/css already have) and "the generator's own
front-end reproduces itself" (self-hosting). Only Rung 2 proves self-hosting.

## 3. Integration seams (path:line)

- `skinny/xtask/src/main.rs:40-47` — regen/check dispatch; add four arms + `fixpoint-bbnf`/
  `fixpoint-regex`.
- `skinny/xtask/src/main.rs:177` `JSON_TARGET`, `:200` `SHEETS_TARGET` — the `RuntimeTarget`
  templates to mirror for `BBNF_TARGET`/`REGEX_TARGET`.
- `skinny/xtask/src/regen.rs:37` `check_target` — the byte-identity comparator (Rung 1 verbatim).
- `skinny/xtask/src/regen.rs:153` `validate_inputs` (blake3 over inputs) — extend to emit the
  emitted-digest → `bootstrap.lock`.
- `skinny/crates/grammar/src/lib.rs:29` `parse_grammar` — the seed parser (freeze/retire; §4).
- `skinny/crates/codegen/src/lib.rs:101` `emit_from_source` — the generator entry that mints S₀.
- `parse_that/src/regex/generated.rs` (2293) — regex S₀ (adopt).
- `parse_that/src/regex/mod.rs:33` `parse_generated`, `host.rs:1` `lower` (524) — the regex
  self-host consumer + fold; the promotion target (§4 R3).
- `parse-that/rust/bootstrap/{Cargo.toml, src/lib.rs}` — the retired-derive record (proc-macro
  → xtask migration, commit `6142387f`) and the diagnostic re-export shape both parsers mirror.
- Legacy fixpoint precedent: `crates/core/src/grammar/generated/bbnf.rs` header (`Regenerate:
  cargo xtask regen --grammar bbnf`, `include_str!(bbnf.bbnf)`, baked structural alphabet).

## 4. Named risks and counterexamples

- **R1 — the fixpoint may 2-cycle rather than converge.** If codegen output order depends on
  parse-produced AST ordering, `regen∘regen` could yield S₀→S₁→S₂ with S₂==S₀ but S₁≠S₀. The
  gate must assert `S₁==S₂` (a fixed point), not merely `S₂==S₀`. Prior art: rustc requires
  stage1==stage2, not stage2==stage0. Counterexample to plant: perturb rule emission order and
  confirm the gate trips.
- **R2 — self-application closure (the load-bearing feasibility question).** bbnf.bbnf uses the
  full bbnf surface to describe itself: `@import`, `?w`, grammar-function `call_arg`, `closure`
  (`|params| rhs`), `->` projections, `@pretty`/`@host`/`@ws` directives — 34 uses of
  `?w`/`@import`/`closure`/`->` in 89 lines. The generated parser must handle EVERY construct
  bbnf.bbnf exercises or self-host is impossible. At skinny HEAD the emitter (`general_descent`,
  `json_sink_direct`, `css_scan`) is shaped for json/css/sheets; the recursive-alternation +
  closure + import tower of bbnf may exceed it. The legacy 21557-LOC `bbnf.rs` argues it was
  once feasible, but on the OLD emitter. This is the first thing a prototype must falsify.
- **R3 — regex stage-0 is a verification lane, not the parser of record.** Promoting
  `parse_generated` to production retires `bbnf_regex::path_lexer` (the hand engine); leaving it
  as an oracle makes it consumer-less substrate (Y.13). 7A's answer: promote or delete. Promotion
  gates on (a) parse-equivalence vs the hand parser on the full suite, (b) a perf non-regression
  (the hand `path_lexer` is tuned). If perf blocks promotion, the generated parser is banked and
  7A's regex leg is a differential oracle only (which is a sibling family's center, not 7A's).
- **R4 — the seed-retirement paradox.** True two-stage retires the hand parser once S₀ exists —
  but then S₀ can only be regenerated from S₀, and a grammar change S₀ cannot parse bricks the
  chain (the "can't add syntax you don't yet parse" trap). Mitigations, both prior-art-backed:
  (i) keep the hand parser as a FROZEN, never-growing seed (the conservative 7A default —
  auditability via a small fixed seed, à la Guix's reduced binary seed); (ii) stage grammar
  changes across the seed boundary with a `cfg(bootstrap)`-style two-commit dance (rustc's
  mechanism for language changes that cross the stage0 line). 7A must PICK; the default is (i).
- **R5 — twin ladders across repos.** regex S₀ lives in parse-that (combinator runtime); bbnf S₀
  in skinny (tape runtime). Two generated-parser runtimes ⇒ two bootstrap ladders ⇒ the "minimal
  surface" doubles, pressuring constitution (ii)/(iii) isomorphism. 7A must state whether the two
  ladders share a fixpoint harness (one `fixpoint` xtask over both) or stay independent.
- **R6 — determinism holes.** Byte-identity requires HashMap-free emission order, no timestamps,
  no absolute paths in generated headers (the legacy `include_str!(concat!(CARGO_MANIFEST_DIR,
  ...))` bakes a build-relative path — a reproducibility hazard). The `bootstrap.lock` digest
  catches this; the emitter must use ordered maps (BTreeMap — `regen.rs` already uses
  `BTreeSet`).

## 5. Prior art (citations)

- **rustc bootstrap ladder** — stage0 (pre-built downloaded compiler) → stage1 (built by stage0)
  → stage2; `x.py`; the stage1==stage2 reproducibility invariant is exactly Rung 2. The
  downloaded stage0 = "minimal trusted surface." `cfg(bootstrap)` stages language changes across
  the seed line (R4-ii).
- **GHC stage1/stage2** — the Haskell self-host; stage2 rebuild reproducibility; "booting from
  a bootstrap compiler."
- **Bootstrappable Builds / Guix (`stage0`, `mes`, hex0)** — the reduced-binary-seed movement;
  the telos "minimal bootstrap surface, maximal auditability" verbatim. Directly motivates 7A's
  frozen-seed default (R4-i).
- **Diverse Double-Compilation (David A. Wheeler, 2005/2009)** — defends against Thompson's
  "Reflecting on Trusting Trust": bootstrap via two independent seeds, require byte-identical
  output. The audit story for a committed binary/source stage0 (the strongest form of 7A's
  provenance ledger).
- **Reproducible Builds (SOURCE_DATE_EPOCH, bit-for-bit determinism)** — the byte-identity gate
  is a reproducible-build check; `check_target`'s `check_dir` is the local comparator.
- **tree-sitter** — `tree-sitter generate` commits `src/parser.c` from `grammar.js`; the
  generator itself is not self-hosted. The exact pragmatic split 7A takes: generator kernel
  primitive, grammar parser generated + committed.
- **lalrpop / rust-peg / pest** — the "commit the generated parser" vs "regenerate in build.rs"
  axis; 7A's committed-stage-0 thesis is the former (auditable, no build-time codegen dependency).
- **Wirth, *Compiler Construction* (Oberon/Pascal-P P-code bootstrap)** — the classical
  T-diagram self-compilation fixpoint this family formalizes.

## 6. What a prototype MUST test to move this family

1. **Feasibility (falsifies R2 first).** Does `emit_from_source("bbnf", bbnf.bbnf)` produce a
   parser that parses `grammar/bbnf/bbnf.bbnf` to a `GrammarIr` structurally equal to
   `parse_grammar`'s? Corpus: all 30 `grammar/**/*.bbnf` sources. PASS ⟺ generated ≡ hand on
   every source. This is the go/no-go for the whole family.
2. **Single-step fixpoint (the core claim).** Emit S₀ via the hand seed; emit S₁ via S₀'s
   generated parser; assert `sha256(S₁)==sha256(S₀)`; emit S₂ via S₁; assert `S₁==S₂`. Report
   which rung, if any, diverges and by how many bytes.
3. **Determinism.** Two `regen-bbnf` runs (reordered inputs, second machine) → byte-identical
   S₀; `bootstrap.lock` digests stable. Catches R6.
4. **Seed-surface measurement.** Report seed_LOC (hand parser + fold + the codegen kernel S₀
   touches) — the family's headline number, to be traded against a total-self-host surface.
5. **Regex promotion gate (resolves R3).** `parse_generated` vs `bbnf_regex::parse` on the full
   regex suite + every regex literal embedded in the 30 grammars; report parse-equivalence and
   cyc/B delta. Promotion is licensed only if equivalence holds and perf non-regresses.
6. **Build-time budget.** Wall-time of the two-stage `fixpoint-bbnf` (it compiles S₀ into the
   regen host); report the delta over a one-stage `regen-json`. The auditability/build-cost
   trade that distinguishes 7A from a heavier total self-host.
