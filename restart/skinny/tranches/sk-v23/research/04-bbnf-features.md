# SK-V23 research/04 — the bbnf feature brainstorm (novel / QoL / performance, ruthlessly triaged)

Date: 2026-07-09. Read-only pass. Governing charter: `sk-v23/PASS-ALPHA.md` (three axes: x86
abrogation, SIMD modernization, greenfield-from-skinny). Prior plans of record: sk-v22 SPEC
(graduation GW0–GW6 + PT-R/PT-T + the "future greenfield shopwindow wave" for the dormant
constellation, SPEC.md:99) and sk-v21 SPEC (perf adjudications). This pass enumerates the
feature surface the codex could fold, and — the load-bearing act — says which to fold, which to
defer to the shopwindow wave, and which to explicitly NOT-NOW.

## §0 — The triage lens (why most of this list must NOT be folded)

Three constraints from the corpus bound every proposal below, and they do most of the cutting:

1. **The novelty is one thing, and it is already owned.** The four-part conjunction — declarative
   grammar → block-wide SIMD structural classification → backend-agnostic IR → typed value plane
   over one tape — is the *entire* differentiator, and the seam is confirmed empty against the
   2025–26 record (`26-sota-parser-generation.md:19,55,280-283`). Every "novel feature" below is
   adjacent to that core, not a second novelty. A feature that does not either (a) harden the core
   or (b) become the *shopwindow* that shows the core off is scope-creep, and the charter's
   no-overfitting line guards against exactly this in both directions (`24-sota-css.md:25`).

2. **The constellation that hosts most "novel features" is already dispositioned:
   KEEP-DORMANT-IN-ARCHIVE.** The `lsp` + `ir/vm` + `@debug` + `gorgeous` constellation (LIVE at
   HEAD — `41-feature-surface.md:53-57,82-87`, 1772 LOC VM, DAP adapter at `dap/adapter.rs:150`)
   rides `archive/legacy-crates` and re-binds only at the **future greenfield shopwindow wave**,
   which must NAME the re-bind at its opening (sk-v22 SPEC.md:99, ADDENDUM §1:30-31). So the
   debugger, LSP linting, and pretty-printer are *not* NOT-NOW — they are OWNED-BY-SHOPWINDOW.
   The brainstorm's job is to say which get *elevated* from dormant-carry to first-class showcase.

3. **The lightningcss depth-asymmetry caps the CSS ambition.** Beating lightningcss is scoped to
   parse + typed materialization; minify/browserslist/nesting parity is a separate
   knowledge-engineering project with no generalization angle and must never be folded silently
   (`24-sota-css.md:19,25,41`). Any CSS feature that reaches past parsing is NOT-NOW by charter.

Value classes: **DIFFERENTIATOR** (extends/showcases the novelty), **QoL** (adoption friction),
**PERF** (kernel/throughput), **HYGIENE**. Cost: S / M / L / XL. Owner: a GW wave, the SHOPWINDOW
wave, the PT-R/PT-T tracks, or NOT-NOW-with-reason.

---

## §1 — (a) NOVEL features

### N1. Grammar-level debugging / DAP as a *shipped* generator feature — ELEVATE to SHOWCASE
- **Value: DIFFERENTIATOR (second-strongest after the SIMD core). Cost: M** (activation, not build —
  the engine exists).
- **Evidence.** The bytecode VM (`crates/ir/src/vm/`, 1772 LOC), `SourceMapEntry`
  (`bytecode.rs:203`), `DebugBreak` (`vm/debug.rs`), the `@debug` directive
  (`crates/analysis/src/directives/debug.rs`), and a working DAP adapter driving the VM
  (`dap/adapter.rs:150`) all SURVIVE at HEAD and are UNOWNED by any wave
  (`41-feature-surface.md:53-66,150-158` — the load-bearing census gap). No competing generator
  ships this: tree-sitter, pest, chumsky, winnow, ANTLR emit a parser, not a source-mapped,
  breakpoint-capable debugger over the grammar's own execution (`26:85-127`). "Set a breakpoint on
  a grammar rule, step the parse, inspect the value plane" is a genuine, defensible first.
- **Disposition: OWNED-BY-SHOPWINDOW, but the codex must ELEVATE it from dormant-carry to a named
  showcase deliverable.** The greenfield friction: the VM is the *pretty/debug* engine, orthogonal
  to the hot SIMD parse path (`41:60-63`); the debugger debugs the *interpreted* semantics, not the
  emitted NEON kernel. Honest framing required — it debugs the grammar, not the generated
  classifier. Recommend a distinct shopwindow sub-wave that re-binds `@debug`+DAP over the
  graduated VM and states this boundary at its opening.

### N2. Error-recovery / diagnostic-quality parse trees (tree-sitter-class recovery) — NOT-NOW (scope), MIRROR later
- **Value: DIFFERENTIATOR-if-shipped. Cost: XL.**
- **Evidence.** bbnf's hot path is fail-fast RD (the AW fn-per-rule prototype, `41:59`); tree-sitter
  gets recovery from run-time GLR (`26:90-95`), and CSS's never-fail tokenization is a *spec*
  mandate bbnf must already reproduce for CSS fidelity (CSS Syntax L3, `24:11`). Real recovery
  (error nodes, resync, partial trees) is an architectural axis bbnf does not have and that
  fights the fail-fast-is-fast thesis.
- **Disposition: NOT-NOW.** Reason: recovery is orthogonal to the SIMD-throughput novelty and
  costs an XL redesign of the descent. The *cheap* subset — a scoped `parseWithDiagnostics()`
  returning first-error + expected-set — is the PT-E1 mirror (see Q3), not full recovery. Full
  tree-sitter-class recovery is a post-graduation research question, not an SK-V23 fold.

### N3. Streaming / incremental parse surface — NOT-NOW (no consumer), FLAG for shopwindow
- **Value: QoL/DIFFERENTIATOR. Cost: L.**
- **Evidence.** The tape architecture (AoS `Vec<TapeRec>` + `sib_skip`, NEGATIVE-RESULTS §1) is a
  whole-buffer, two-pass classify→materialize model (`24:11` shape; sk-v22 03:187). Incremental
  reparse (tree-sitter's headline) needs edit-diff + subtree reuse; streaming needs a chunked
  classifier that survives block boundaries (escape-carry across 64-byte blocks — the exact
  cross-block state simdjson/Lemire handle by hand, `26:146-155`). The playground is the only
  interactive consumer and it is archived (`39-playground-census.md:82-85`).
- **Disposition: NOT-NOW.** Reason: zero surviving consumer post-playground-archival; the value
  plane (GW4) must land first. Flag as a shopwindow candidate *only if* an editor surface returns.

### N4. W3C/MDN-derived typed CSS catalog — the genuine lightningcss NOVELTY — FOLD (scoped), own at GW4+
- **Value: DIFFERENTIATOR (this is the one CSS angle lightningcss itself does NOT have). Cost: L.**
- **Evidence.** lightningcss's ~100+ `Property` variants are *hand-authored* Rust from a
  hand-maintained list — it does NOT derive its catalog from any machine-readable spec
  (`24:19`). bbnf's current catalog is 27 hand-curated `CssLengthUnit` values / ~594-line TOML
  (`24:19`). Generating the typed property catalog from the W3C/MDN machine-readable CSS data
  (mdn/data `css/properties.json`, csstree's patched syntax set) is "a real point of novelty over
  lightningcss rather than parity with it" (`24:19`) — a *generator* feeding the *grammar
  generator*, which is exactly on-thesis (one generator, N grammars, catalog-from-data not
  hand-curation).
- **Mandatory constraint.** The catalog MUST carry the `var()` escape hatch — an
  opaque/deferred-token variant matching lightningcss `Property::Unparsed`/`Custom`
  (`24:17`); forcing every leaf into a closed union rejects real var()-heavy corpora (tailwind,
  bootstrap already in `data/css/`). This is a build-time spec-data pipeline, not a parser-speed
  item.
- **Disposition: FOLD, own at GW4+ (rides the value plane / catalog authoring).** Scope hard:
  catalog breadth + var() hatch ONLY. NOT minify/browserslist/nesting (§0.3).

### N5. Grammar-derived fuzzers / property tests — FOLD-CHEAP (test infra), own at GW0/GW2a
- **Value: HYGIENE→DIFFERENTIATOR. Cost: S–M.**
- **Evidence.** Grammar-based fuzzing is active 2025 SOTA but every tool *consumes* a grammar as a
  model (Grammarinator; tree-crasher, which splices tree-sitter ASTs and found dozens of bugs in
  Clang/rustc; "Grammar Mutation for Testing Input Parsers", TOSEM 2025). bbnf already OWNS its
  grammars and round-trips them (self-host parity, `crates/core/tests/bbnf_parity.rs:21-35`,
  `41:45`). A generator that emits, from each grammar, a *generator* of valid inputs (derivation)
  + a *mutator* — turning the grammar into its own differential fuzzer + property-test corpus
  against the SOTA oracle (serde_json / lightningcss, `ORACLES.md`, sk-v22 03:176) — is nearly
  free relative to the parity harness already present, and it strengthens the honesty ledger
  (fuzz-found divergences become NEGATIVE-RESULTS entries).
- **Disposition: FOLD-CHEAP as test infrastructure at GW0/GW2a** (the regen host's conformance
  feature already exists — `skinny/xtask ... features conformance`, sk-v22 03:131). This is
  build-first infra (memory `build-infra-first`), not a shipped user feature. A grammar-derived
  fuzzer *as a documented product surface* is a shopwindow line if it earns one; the internal
  differential fuzzer earns its keep on day one.

### N6. Const-eval / build-time grammar compilation ergonomics — ALREADY-THE-ARCHITECTURE, harden not add
- **Value: PERF/QoL. Cost: S** (it is the status quo; the work is hygiene).
- **Evidence.** bbnf is already build-time codegen out of proc-macro (B2's cure — sk-v22 03:130),
  with `regen --check` byte-equality (N1), `generated_loc_budget` (N6 — `css_l4.rs` was 108,406
  LOC / 7× budget, the "relocation is not reduction" lesson, sk-v22 03:143), and N25
  idempotency-on-disk. logos, by contrast, is the *fastest* Rust emitter and still leaves ≥20% on
  the table by deferring to the autovectorizer (`26:105-110`) — the const-eval/build-time story is
  bbnf's structural win, not a feature to invent.
- **Disposition: NO NEW FEATURE. Fold the three gates into the graduated regen host at GW2a**
  (already the SEAM-XTASK plan). The generated-size budget is a per-tranche wall (memory
  `generated-size-budget`); enforce, don't extend.

---

## §2 — (b) QoL features

### Q1. Derive-macro direct-to-struct UX — FOLD, own at GW4 (rides the value plane)
- **Value: QoL (the #1 adoption lever for the Rust audience). Cost: M.**
- **Evidence.** The fused eager sink exists (`json_sink_direct/`, `41:94`) and the serde
  `Deserialize` bridge is a GW4 lane on the cursor (`41:97`; sk-v22 SPEC §4). No 2025–26 Rust crate
  pairs derive-macro direct-to-struct with a grammar-generated SIMD front end (web: syn/combine
  cover proc-macro parsing and struct-building separately, not this union). The competitor is
  sonic-rs's direct-to-struct (rejecting the two-stage tape, `26:171`) — bbnf can offer the same
  ergonomics *from an arbitrary grammar*, not JSON-only.
- **Disposition: FOLD at GW4.** The WART-C name-gate fold (`json_sink_direct/mod.rs:98` →
  `general_descent`, GW2a) is the prerequisite: direct-to-struct must be grammar-agnostic before it
  is a UX surface. Guard: no facet runtime reflection (explicitly rejected, sk-v22 SPEC §4).

### Q2. Grammar linting in the LSP — OWNED-BY-SHOPWINDOW (elevate)
- **Value: QoL. Cost: M** (diagnostics crate exists).
- **Evidence.** `crates/analysis` (5,241 LOC, hover/completion/diagnostics) + `crates/lsp` are LIVE
  and UNOWNED (`40-prune-census.md:114`, `41:82-85`). Lints bbnf can uniquely offer: unreachable
  alternations, left-recursion (already eliminated at compile time, `passes/lr.rs`, `41:118`) as a
  *warning*, ambiguous `->` projections, non-materialized arrows (the
  `typed_materialization_invariant`), un-consumed `@host` params (the unifier's dropped `FnCall`
  args, `41:42`), off-manifest grammars (`regex.bbnf`, `41:48`).
- **Disposition: OWNED-BY-SHOPWINDOW.** Same re-bind wave as N1 (DAP). The grammar-specific lints
  (materialization-invariant, projection ambiguity) are the differentiated subset worth naming;
  generic LSP hygiene is table stakes.

### Q3. Error-message quality — the PT-E1 mirror — FOLD-DEFERRED to GW4+ (as SEAM records)
- **Value: QoL. Cost: S** (copy a settled shape).
- **Evidence.** PT-E1 is EXECUTED on the parse-that side (PT-T.2 ships scoped diagnostics via the
  PACKRAT_ARMED latch; sk-v22 SPEC §3 SEAM-PT-E1, 03:104-107). bbnf's obligation is to *copy the
  scoped-return `Diagnostic` shape when its Value API grows an error surface at GW4+* — a mirror,
  not original design. The anti-pattern to avoid: process-global toggles; the pattern is a scoped
  latch (`parseWithDiagnostics()` returning `expected`).
- **Disposition: FOLD-DEFERRED — the SEAM already schedules it at GW4+.** No new work item; ensure
  the GW4 value-plane wave carries the `Diagnostic`-shape mirror as an acceptance line.

### Q4. cargo-generate project templates — NOT-NOW (premature)
- **Value: QoL. Cost: S.**
- **Evidence.** No published, stable bbnf Rust crate exists (`bbnf-path-ts` deferred; the TS
  package `@mkbabb/bbnf-lang ^0.1.1` is a latex-paper edge only, sk-v22 03:94). Templates presuppose
  a stable public API and a first user; the graduation has neither until post-GW6.
- **Disposition: NOT-NOW.** Reason: templates for an API that is still being greenfielded generate
  churn, not adoption. Reconsider only after the value plane (GW4) and a published crate settle.

### Q5. Docs-from-grammar — FOLD-CHEAP at GW2a (rides the one regen host)
- **Value: QoL. Cost: S.**
- **Evidence.** The grammar IS the spec; railroad-diagram / rule-reference generation from the same
  parsed AST the codegen consumes is a small emitter over an existing artifact (the 9-grammar
  manifest: bbnf/json/css_l4/css_pretty/google_sheets/ebnf/bnf/csv/math, `41:48`). Self-host means
  `bbnf.bbnf` documents bbnf (`41:45`). gorgeous already pretty-prints grammars
  (`40:31`) — docs are a formatting target, not new machinery.
- **Disposition: FOLD-CHEAP at GW2a** as an emitter mode on the one regen host. Guard: obey the
  generated-loc-budget wall (§N6) — docs output counts against it.

---

## §3 — (c) PERFORMANCE features

### P1. Bulk unescape kernel (the remaining SOTA-gap) — FOLD, own at the SIMD kernel wave
- **Value: PERF (a real, named gap). Cost: M.**
- **Evidence.** simdjson 4.x carries a hand-written **NEON unescape kernel** (`26:146`); bulk
  unescape of quoted-string bodies is a distinct hot kernel from structural classification. This is
  precisely the charter's Axis-2 SOTA-NEON mandate (vshrn family, table-lookup classification,
  Hyperscan-lineage prefilters, PASS-ALPHA:14-18). It is NOT novel (Langdale–Lemire, mainstream —
  `26:317-318`); the win is *generating* it grammar-side (per `->` string leaf), on-thesis.
- **Disposition: FOLD at the SIMD-modernization kernel wave** (the charter's Axis-2 home). Gate:
  cold-only PMU cyc/byte, paired interleaving (BENCH-PROTOCOL). Guard against the NEGATIVE-RESULTS
  trap: a dead pre-scanner cost 30–170% more to reclaim 3% (sk-v22 03:209) — the unescape kernel
  must show a production grammar exercising it under `cargo expand` as an OPENING gate (the
  twice-exhumed-bitmap lesson, NEGATIVE-RESULTS §3).

### P2. Tokenize-once CSS bodies (CSS-specific SIMD structural index) — FOLD-AMBITIOUS, own at the CSS/kernel wave
- **Value: PERF/DIFFERENTIATOR (a confirmed-empty lacuna). Cost: L, HIGH-RISK.**
- **Evidence.** "Nobody in the public record has shipped a CSS analogue of simdjson's find-all-
  structurals-with-SIMD approach" (`24:35-37`) — a real first-mover position. But CSS's tokenizer
  has messier local state than JSON: comment-inside-anything, `url()`'s raw-token sub-grammar,
  backslash escapes, nested `{}()[]` depth (`24:37`). "Speculate" (Mozilla multi-thread
  speculative CSS tokenization) is the only prior attempt and it shelved (`24:29`).
- **Disposition: FOLD-AMBITIOUS but budget the risk explicitly.** This is where the SIMD-generator
  thesis earns its most *novel* CSS win. Gate hard: the comment/url()/escape correctness cases have
  no solved precedent to copy (`24:37,41`) — treat as a research sub-wave with a falsifier corpus,
  not a checklist item. Do NOT let it balloon past tokenization into minify (§0.3).

### P3. PGO hooks — FOLD-CHEAP as bench infra, NOT a shipped profile
- **Value: PERF. Cost: S.**
- **Evidence.** The codegen path is monolithic and hot-loop-shaped; PGO (and the nightly toolchain
  already pinned, sk-v22 03:55-58) can wring branch-layout wins the autovectorizer misses — the
  same class of gap logos leaves open (`26:105-110`). But PGO is a *build-configuration* lever, not
  an architecture; the profiling standing law already mandates real PMU measurement (memory
  `actual-profiling`, `iter-profile-always`).
- **Disposition: FOLD-CHEAP as an optional bench/release profile hook.** NOT-NOW as a shipped
  default (PGO training-corpus maintenance is unbounded). Wire it into BENCH-PROTOCOL as an
  available lever the kernel waves may use, gated on cold-only re-measurement.

### P4. no_std / embedded profile — NOT-NOW (no user, real cost)
- **Value: PERF/reach. Cost: L.**
- **Evidence.** The stack pulls `mimalloc`, `blake3`, `serde_json`, arena allocation as the
  singular strategy (memory `no-orthogonal-codepaths`), and NEON intrinsics are the kernel plane.
  no_std would fork the allocation strategy (forbidden — arena must stay singular) and strip the
  serde/mimalloc surface. Zero embedded consumer exists in the constellation (value.js, latex-paper,
  keyframes.js are all web/Node, sk-v22 03:74-91).
- **Disposition: NOT-NOW.** Reason: no consumer, and it directly fights the singular-arena and
  fused-sink adjudications. Revisit only if an embedded consumer materializes; even then, a feature
  gate, never a code-path fork.

---

## §4 — The triage table (fold / defer / not-now)

| # | Feature | Value | Cost | Owner |
|---|---|---|---|---|
| N1 | Grammar DAP / step-debug | DIFFERENTIATOR | M | **SHOWCASE — elevate at SHOPWINDOW wave** |
| N4 | W3C/MDN typed CSS catalog (+var() hatch) | DIFFERENTIATOR | L | **FOLD — GW4+** |
| N5 | Grammar-derived differential fuzzer | HYGIENE→DIFF | S–M | **FOLD-CHEAP — GW0/GW2a** |
| N6 | Build-time compilation gates | PERF/QoL | S | **HARDEN (no new feature) — GW2a** |
| Q1 | Derive-macro direct-to-struct | QoL | M | **FOLD — GW4** |
| Q2 | Grammar linting (LSP) | QoL | M | **OWNED-BY-SHOPWINDOW** |
| Q3 | Error-msg quality (PT-E1 mirror) | QoL | S | **FOLD-DEFERRED — GW4+ (SEAM)** |
| Q5 | Docs-from-grammar | QoL | S | **FOLD-CHEAP — GW2a** |
| P1 | Bulk unescape kernel | PERF | M | **FOLD — SIMD kernel wave** |
| P2 | Tokenize-once CSS index | PERF/DIFF | L (risk) | **FOLD-AMBITIOUS — CSS/kernel sub-wave** |
| P3 | PGO hooks | PERF | S | **FOLD-CHEAP as bench lever** |
| N2 | Tree-sitter-class error recovery | DIFF | XL | **NOT-NOW (scope; fights fail-fast)** |
| N3 | Streaming / incremental parse | QoL/DIFF | L | **NOT-NOW (no consumer)** |
| Q4 | cargo-generate templates | QoL | S | **NOT-NOW (no stable API/user)** |
| P4 | no_std / embedded | PERF | L | **NOT-NOW (no consumer; fights arena)** |

## §5 — The ruthless cut (what actually enters the near-term GW plan)

A brainstorm that folds everything is a plan for nothing. The **near-term GW folds** are exactly
five, each riding a wave that already exists:

1. **P1 bulk unescape** + **P2 CSS tokenize-once index** — the charter's Axis-2 SIMD mandate; these
   ARE the SIMD-modernization tranche's payload, not additions to it.
2. **N4 W3C/MDN CSS catalog + Q1 direct-to-struct + Q3 diagnostics mirror** — all ride GW4 (value
   plane), which sk-v22 already names as "the largest genuine forward workload" (SPEC:82).
3. **N5 fuzzer + Q5 docs-from-grammar + N6 gate-hardening** — cheap emitters/infra on the one regen
   host at GW0/GW2a; build-infra-first, so they land early and pay for themselves in the parity gate.

The **SHOWCASE elevation** (N1 DAP, Q2 LSP lints) is the shopwindow wave's job — the codex's
contribution is to convert their status from silent dormant-carry (the census gap, `41:150-158`)
into a *named* shopwindow deliverable with the honesty boundary stated up front (debugs the
grammar semantics, not the emitted kernel).

The **NOT-NOW four** (N2, N3, Q4, P4) are cut for concrete reasons — no consumer, fights a settled
adjudication (fail-fast, singular arena), or presupposes a stable API the greenfield has not
produced — not for lack of merit. Each is revisitable behind a named precondition, recorded so a
later pass does not re-litigate from zero.
