# SK-V23 W5 — the parse-that FEATURE brainstorm (novel · QoL · performance, both trees)

Date: 2026-07-09. READ-ONLY on `~/Programming/parse-that` (non-clobber law, PASS-ALPHA §"the
non-clobber law"). Grounds forward from SK-V22 research/05 (Rust SOTA: de-nightly, arena the
tree, fold the engine beneath the recognizer, hot-loop `find_at`) and 06 (TS/V8: shape
normalization, PT-E1 scoped diagnostics, PT-E2 inference tightening). Those two are the *uplift*
wave lists (RU-1..9, TW-PT-TS.1..5) — repairs to what exists. This pass is the *feature* frontier:
what parse-that should GROW that neither list proposes, plus the crate's 1.0/1.1 product shape.

Every item carries: **value/cost class**, **backbone semver impact** (value.js / latex-paper /
keyframes — which consume the **TS package only**, so no Rust item can red a backbone; SK-V22
research/05 §2 consumer table), and **owning PT tranche or NOT-NOW**. Tranche lettering uses the
PT-R\* (Rust) / PT-T\* (TS) track family (PASS-ALPHA §non-clobber) to avoid collision with the
native A/B/Q/S series.

Repo facts anchoring this pass (file:line): TS tree 2650 LOC under `typescript/src/parse/`, public
barrel `typescript/src/parse/index.ts:1-16` (9-identifier consumed surface + `./diagnostics`
subpath, `typescript/package.json` exports). Rust: `parse_that` 0.4.0 combinators
(`rust/parse_that/Cargo.toml`), `bbnf-regex` 0.1.0 — the 6,742-LOC HIR→NFA→DFA engine
(`rust/regex/src/lib.rs:14-45` public re-exports). The self-hosted `rust/bootstrap/regex.bbnf`
(72 lines) generates `rust/parse_that/src/regex/generated.rs` (2,293 LOC), re-exported through the
bootstrap crate's `parse_generated` (`rust/bootstrap/src/lib.rs:24`).

---

## (a) RUST FRONTIER

### PTR-F1 — arena-first combinator API as an authoring surface (not just an internal swap)
SK-V22 RU-2 arenas the parser tree into the existing `BumpSlab` (`rust/parse_that/src/bump_slab.rs`,
`alloc<T>`/`alloc_slice_copy`) to kill smallbox. Go beyond the *internal* transposition: expose the
arena as the **ergonomic surface** — a `Grammar<'arena>` builder (or `grammar! { … }` macro) that
lifetime-binds every node so `p.then(q)` returns `&'arena dyn ParserFn` with zero per-node heap
alloc, and reuse of a built grammar across N parses is free. Today each combinator closes over its
children into one `SmallBox<dyn…,S32>` that *spills* on the hot two-child shape (research/05 §1) —
an arena-first API makes the zero-alloc path the DEFAULT authoring shape, not a hidden win.
- **Value/cost**: HIGH ergonomics + zero-alloc / MED-HIGH (lifetime plumbing across the builder).
- **Backbone**: none — Rust combinators have no surviving backbone consumer (research/05 §2:
  backbones consume the TS package; skinny uses codegen, not combinators).
- **Owner**: NOT-NOW, **conditional on open-question-A** (does the graduated tree keep a general
  Rust combinator surface at all, e.g. to host the BBNF bootstrap in combinators?). If yes → PT-R
  combinator tranche, folds RU-2. If no, the crate archives and this is moot.

### PTR-F2 — a stable 1.0 `Regex` façade (the crate's product shape)
`bbnf-regex` today exposes raw internals — `Dfa`, `DfaOptions`, `RegexInfo`, `EngineSet`,
`classify_regex`, `parse` (`rust/regex/src/lib.rs:29-45`) — but no ergonomic top-level `Regex` type.
Every match is anchored `find_at(bytes, offset)` (research/05 §3). A 1.0 product needs a documented,
semver-frozen façade: `Regex::new(pat) -> Result<Regex, ParseError>`, `is_match`, `find`,
`find_iter`, plus explicit `find_at`/`find_anchored` for the grammar-hot anchored path. The
unanchored `find` is powered by the EXISTING but bbnf-unused start prefilter — `literal_prefix.rs`
+ `first.rs` first-char sets (research/05 §3 called the start-prefilter "rightly absent" for bbnf;
it is *required* for a general crate product). This is the "stable 1.0 shape for the Rust crates"
ask (W5 charter, Rust bullet).
- **Value/cost**: HIGH (turns an internal engine into a shippable crate) / MED.
- **Backbone**: none. Consumers are `bbnf-lang/crates/ir` + `bbnf-path` (research/05 §2), both
  legacy-archiving — gate is internal facts-parity, not external semver.
- **Owner**: PT-R engine-1.0 tranche (co-lands with RU-8 workspace cut).

### PTR-F3 — lazy/hybrid DFA tier as the 4096-bail *degrade* (SOTA, not a slow-path panic)
RU-6 replaces the `state_limit:4096` panic (`rust/regex/src/automata/dfa.rs:24,36`;
`scanners.rs:76`) with an NFA-walk fallback. Go beyond a slow PikeVM walk: make the degrade a
**lazy DFA** that builds states on demand and caches them (regex-automata's hybrid tier) — reusing
the eager DFA's subset-construction builder incrementally rather than a separate interpreter. It
dodges exponential *compile* on pathological grammar regexes while keeping DFA match speed on the
states actually visited. This is the "lazy-DFA tier" W5 explicitly names.
- **Value/cost**: MED (robustness + no compile blowup; most grammar regexes never trip it) / MED-HIGH.
- **Backbone**: none (internal engine).
- **Owner**: PT-R engine tranche, supersedes RU-6's plain-NFA degrade.

### PTR-F4 — SIMD prefilter tiers (Teddy / shufti / truffle) as a pluggable `EngineSet` tier
Charter axis 2 (PASS-ALPHA) mandates Hyperscan-lineage prefilters on parse-that's regex engine.
The substrate exists: `EngineSet` bitset (`rust/regex/src/info/mod.rs:61`), literal-prefix
extraction, first-char sets, and NEON self-loop accel already matching the simdjson two-VPSHUFB
nibble technique (`automata/accel.rs`, research/05 §3). The feature: wire a **Teddy** multi-literal
prefilter (alternation of literals) and a **shufti/truffle** byte-class prefilter as new
`EngineSet` tiers the recognizer-CSP selects — pluggable per the pluggable-components precept, not
hardcoded branches. Aligns the engine with the W2 prefilter-tier plane.
- **Value/cost**: HIGH (perf on the `Unknown` fallback + multi-alternation grammars) / HIGH.
- **Backbone**: none.
- **Owner**: PT-R SIMD tranche (the charter axis-2 kernel work; NEON via the `bbnf-simd` façade
  RU-1 adopts).

### PTR-F5 — opt-in capture-slot tier on the one-pass DFA (genuinely new capability)
The engine is capture-free by design (research/05 §3: "No PikeVM / bounded-backtracker /
capture-group tier — rightly rejected" for bbnf). But `info/one_pass.rs` already detects one-pass
eligibility. A **bounded capture-slot array on the one-pass DFA** (no PikeVM, no backtracking) adds
span extraction for the one-pass subset at near-DFA speed — a real new capability for a general
regex-crate product, without the tier bbnf rejects.
- **Value/cost**: LOW for bbnf / MED for a 1.0 crate product; cost MED.
- **Backbone**: none.
- **Owner**: NOT-NOW (product-shaped; bbnf's DFA-only surface never needs it — defer past the 1.0
  façade unless an external consumer asks).

### PTR-F6 — Rust error/diagnostic surface parity with the TS side
The Rust combinator crate mints labels via `Box::leak(format!(…))` under the `diagnostics` feature
(research/05 §1, `leaf.rs` multiple sites) — an unbounded leak. The TS side carries a structured
`Diagnostic` (offset, furthest, expected-set, suggestions — `typescript/src/parse/utils.ts`). The
feature: a structured `ParseError { offset, furthest, expected: &[&'static str] }` backed by a
`&'static` interner (fixes the leak *and* reaches TS-parity), mirroring the cross-tree Diagnostic
schema (see PTX-F3).
- **Value/cost**: MED / MED.
- **Backbone**: none (Rust-internal; but the SCHEMA must match PTT-F1 for cross-parity).
- **Owner**: NOT-NOW unless combinators graduate (open-question-A); the interner-leak fix alone can
  ride RU-9 regardless.

---

## (b) TYPESCRIPT FRONTIER

Discipline: every TS item stays **additive MINOR → 1.1.0** against the 9-identifier consumed surface
(research/06 §0), or it is NOT-NOW. The one tripwire to 2.0.0 is mutating a consumed `ParserState`
method — none below do.

### PTT-F1 — structured diagnostics as a PRODUCT feature (expected-sets + span + a renderer)
Elevate PT-E1 / TW-PT-TS.2 past the scoped-return: the `Diagnostic` (`utils.ts:84-93`, already
built by `collectDiagnostic`) becomes a first-class product surface — a deduped/ranked **expected-set**,
a source **span**, and a `renderDiagnostic(src): string` caret+ANSI formatter reusing the extant
`ansi.ts` (954 B). value.js authors labels today that vanish into a global-gated `console.error`
(research/06 §2.1); this hands consumers pretty, structured errors for free — the highest-leverage
QoL win for the value.js / keyframes error path (VALUEJS-PT-E asked diagnostics HIGH).
- **Value/cost**: HIGH / MED.
- **Backbone**: additive MINOR — new `parseWithDiagnostics` + `renderDiagnostic`; the consumed
  `.parse():T` / `.parseState()` are untouched. value.js gets its labels lit with ZERO value.js
  change (research/06 §2.1).
- **Owner**: PT-T diagnostics tranche (folds TW-PT-TS.2; the scoped latch reuses `PACKRAT_ARMED`,
  research/06 §1.5).

### PTT-F2 — benchmark harness modernization (build-infra-first, lands FIRST)
The bench dir is half-modern: `json-comprehensive.bench.ts` + `pt-b3-fusion.bench.ts` use vitest
`bench()`, but `arcsecond.ts` / `nearley.ts` / `parsimmon.ts` / `ohm.ts` / `peggy.ts` are raw
comparison scripts (`typescript/test/benchmarks/`). Consolidate all to vitest `bench()` (vitest-bench
precept), cold-only per no-warm-benches, one harness. Dev-iteration infra → lands first in its
tranche (build-infra-first precept).
- **Value/cost**: MED (dev velocity) / LOW.
- **Backbone**: none (test-only, unpublished).
- **Owner**: PT-T infra wave (W0 of whichever TS tranche opens).

### PTT-F3 — worker-offload pattern (a shape constraint + doc, not new runtime)
keyframes/value.js parse on the main thread. The feature is a **transferable-safe result shape**
(the Diagnostic + AST carry no closures / no `Parser` refs, so they survive `structuredClone`) plus
a documented `parseInWorker` pattern. Mostly a discipline gate on PTT-F1's Diagnostic shape + a doc,
not a runtime subsystem — QoL for animation-heavy keyframes consumers offloading large parses.
- **Value/cost**: MED / LOW.
- **Backbone**: additive (a shape guarantee; no surface change).
- **Owner**: PT-T (rides PTT-F1's Diagnostic shape; doc-gen adjacent).

### PTT-F4 — grammar trace / devtools integration
`debug.ts` (383 LOC) prints a static grammar tree. The feature: a `trace()` mode emitting nested
console.group events (or Chrome-trace-format spans) for a live parse — the dispatch-LUT arm taken,
packrat memo hit/miss (`packrat.ts` `getCijKey`), furthest-offset advances — so grammar authors see
WHY a parse failed where it did. QoL for grammar authoring; zero cost when the trace scope is closed
(reuse the `PACKRAT_ARMED` latch discipline).
- **Value/cost**: MED / MED.
- **Backbone**: additive (new opt-in method).
- **Owner**: PT-T diagnostics tranche (co-lands with PTT-F1; shares the scoped-latch).

### PTT-F5 — streaming / chunked input
A recorded 1.0.0 non-goal (research/06 §4 non-goals: token streams / incremental / Pratt dormant).
Genuinely hard on a backtracking combinator — needs suspendable `ParserState` across chunk
boundaries. Named for completeness.
- **Value/cost**: MED / HIGH.
- **Backbone**: would be additive but is a rewrite of the state thread.
- **Owner**: **NOT-NOW** (non-goal; gated behind a bbnf incremental-consumer GO, mirroring the WDM/LR
  PROVISIONAL disposition, research/06 §4).

---

## (c) CROSS-TREE

### PTX-F1 — regex.bbnf as a MAINTAINED self-hosting verification artifact (a live gate)
`rust/bootstrap/regex.bbnf` (72 lines) is the self-hosted L4 regex grammar; it generates the
`parse_generated` recognizer whose only current check is a hand-listed pattern array
(`rust/bootstrap/src/lib.rs:36-58`). The feature: a **differential CI gate** — every pattern in the
engine's own HIR-parser test corpus (`rust/regex/src/hir/parser/`) must ALSO parse under
`parse_generated`, and vice versa. This makes the self-hosted grammar catch dialect drift the
instant the hand-written `hir/parser` and the generated bootstrap diverge — the grammar becomes a
maintained truth artifact, not a frozen demo (W5 cross bullet: "the self-hosted regex.bbnf bootstrap
as a maintained verification artifact").
- **Value/cost**: HIGH (correctness / anti-drift) / MED.
- **Backbone**: none (Rust-internal gate).
- **Owner**: PT-R engine tranche (rides RU-3's recognizer/engine fold; the differential corpus IS the
  fold's parity gate).

### PTX-F2 — one shared Diagnostic schema across Rust + TS (backend-agnostic errors)
Unify PTR-F6 + PTT-F1: a single `{ offset, furthest, expected: string[], found, span }` diagnostic
SCHEMA mirrored in both trees, so a BBNF grammar's error surface reads identically whichever backend
(Rust codegen vs TS runtime) emits it — the backend-agnostic-types precept applied to the error
plane. This is the design bbnf's own Value-API error surface should copy (research/06 §2.1: "THIS
scoped-return Diagnostic shape is the design to copy").
- **Value/cost**: MED-HIGH / MED.
- **Backbone**: TS half additive MINOR; Rust half internal.
- **Owner**: cross PT-R+PT-T (schema frozen once, consumed both sides).

### PTX-F3 — doc generation from combinator types
Generate API docs from the tightened combinator signatures — Rust rustdoc on the PTR-F2 façade + TS
from the PT-E2-tightened (`Parser<unknown>`, `const` params, `NoInfer`) types (research/06 §2.2).
Low-leverage; named for the W5 "doc generation from combinator types" bullet.
- **Value/cost**: LOW-MED / MED.
- **Backbone**: none.
- **Owner**: NOT-NOW (low priority; rides whichever 1.0/1.1 tranche freezes the public types).

---

## Disposition summary (what to fold into the PT tranche codex)

**Own now (into PT-R / PT-T tranches):** PTR-F2 (1.0 `Regex` façade), PTR-F3 (lazy-DFA degrade),
PTR-F4 (SIMD prefilter tiers — the charter axis-2 kernel), PTX-F1 (regex.bbnf live gate), PTT-F1
(diagnostics-as-product), PTT-F2 (bench harness, infra-first), PTT-F4 (grammar trace), PTX-F2
(shared Diagnostic schema), PTT-F3 (worker-safe shape).

**Conditional (open-question-A — does a Rust combinator surface graduate?):** PTR-F1 (arena-first
API), PTR-F6 (Rust error parity).

**NOT-NOW (product-shaped or recorded non-goal):** PTR-F5 (capture slots), PTT-F5 (streaming),
PTX-F3 (doc-gen).

**No backbone tripwire anywhere** — every Rust item is internal (backbones are TS-only, research/05
§2); every TS item is additive MINOR → 1.1.0 (research/06 §0). The one thing that would force 2.0.0
— mutating a consumed `ParserState` method — appears in no proposal.
