# Tranche AZ-I — Direct-to-Struct on JSON, CSS L4, and Sheets

AZ-I is the first of two tranches that retire the generic tape
substrate in favour of grammar-derived native Rust struct shapes.
AZ-I covers the three primary data grammars — JSON, CSS L4, and
Sheets — moving each one to a `project_types`-derived struct graph
and landing the parse output directly into that graph. `crates/tape/`
remains on disk at AZ-I close, but only one consumer continues to
use it: BBNF's own bootstrap, which AZ-II migrates and deletes.

AZ-I is not "step one of two of tape dissolution across the fleet".
AZ-I is a complete tranche in its own right: three grammars close
their `StructRegistry` coverage, three grammars flow through a single
struct-emitting codegen path, and the three-grammar 17-entry matrix
slice meets AU-baseline parity with no regression. The tape crate
survives as the BBNF-only substrate at close, scoped to one
consumer, with AZ-II opening on that scope.

## Thesis

Grammar-derived direct-to-struct is the single materialised form of
a parse on the three primary data grammars. A typed `->` in a JSON,
CSS L4, or Sheets rule is the declaration of a field (or a compound
shape) in the emitted struct; the IR type-inference pass composes
these into a closed struct graph; the emitter writes straight into
the graph without traversing an intermediate tape record stream.

The `crates/tape/` substrate, at AZ-I close, has exactly one
remaining consumer: BBNF's bootstrap parser, produced by
`bbnf-derive` from `grammars/bbnf/bbnf.bbnf`. This consumer is
explicitly out of AZ-I scope and is migrated in AZ-II. AZ-I's
discipline is to leave that one consumer compiling and correct
while every other grammar sheds the substrate.

The three-grammar direct-to-struct migration takes the form:

1. **`StructRegistry` closure.** Every `Named` rule in JSON, CSS L4,
   and Sheets has a registered `StructLayout` produced by
   `project_types` running to fixed point.
2. **Emitter rewire.** The emission path for these three grammars
   writes directly into struct builders whose shapes come from
   `StructRegistry`. No tape record is materialised on the parse of
   JSON, CSS L4, or Sheets input.
3. **View recode.** The view layer reads struct fields directly.
   Any path that consulted tape offsets for these three grammars is
   rewritten to consult struct field indices.
4. **Parity harness recode.** The sonic-rs, lightningcss, simdjson
   OnDemand, cssparser, and serde_json parity harnesses compare
   struct-vs-external-native, not tape-vs-external.

BBNF's tape-backed path is not touched in AZ-I. `crates/tape/`
compiles and links at AZ-I close, scoped to BBNF's bootstrap only.

## Invariants

1. **Typed materialisation on the three data grammars.** Every `->`
   in JSON, CSS L4, and Sheets reaches a struct field or a
   compound-opening call on the struct builder. An IR audit pass
   enforces 100% coverage on these grammars and fails the build on
   miss. BBNF is exempt from the audit pass in AZ-I (AZ-II enforces
   it there).
2. **Single codegen path on the three data grammars.** One emission
   path for JSON, CSS L4, and Sheets: struct builder calls. No
   combinator fallback. No tape fallback. No "conditional
   struct-or-tape" branch in the emitter for these three grammars.
   `feedback_no-orthogonal-codepaths` is load-bearing.
3. **Rich AST preserved.** Struct shapes carry the full
   lightningcss-level richness. Flattening a typed grammar rule for
   speed is not permitted — `feedback_preserve-rich-ast` is in
   force. CSS L4 `Length`, `Color`, `Dimension`, `Time`,
   `Resolution`, `Percentage`, `Angle` each get a typed Rust enum
   whose variants match the grammar's alternation structure and
   whose fidelity meets or exceeds lightningcss on its own fixtures.
4. **BBNF continues on tape.** BBNF's grammar is not migrated in
   AZ-I. `bbnf-derive` and the BBNF bootstrap parser read and write
   tape records as before. AZ-II cuts this over.
5. **`crates/tape/` remains compilable.** At AZ-I close, the tape
   crate exists on disk, compiles, and is linked by exactly one
   consumer: BBNF's bootstrap. `cargo build -p bbnf` succeeds with
   `crates/tape/` present.
6. **Measurement gates substrate.** Every wave's substrate lands at
   the same commit as a runtime call site that reads the new
   structure, plus a same-commit bench delta on the three-grammar
   slice of the 17-entry matrix. AX invariant 13 is in force.
7. **No deferrals.** Every optimisation needed to hit an AZ-I gate
   lands in the wave that owns that gate; no carry-forward to a
   later wave of AZ-I or to AZ-II. `feedback_no-deferrals`.

## Hard gates (AU-baseline anchored)

AZ-I's close matrix is the three-data-grammar slice of the AU-
baseline 17-entry matrix from `AU/FINAL.md`, evaluated on the
struct-only parse path for those three grammars. BBNF's matrix
entries are evaluated on the existing tape-backed path (must not
regress from AU baseline; AZ-I does not re-tune BBNF).

**Throughput gates (three-data-grammar slice):**

| Grammar / fixture | AU-baseline | AZ-I floor | AZ-I target |
|---|---:|---:|---:|
| JSON canada | 1231 MB/s | 1231 | 1500 |
| JSON citm | 2438 MB/s | 2438 | 2700 |
| JSON twitter | 1967 MB/s | 1967 | 2200 |
| CSS normalize | 735 MB/s | 735 | 850 |
| CSS bootstrap | 454 MB/s | 600 | 700 |
| CSS tailwind | 496 MB/s | 500 | 600 |
| Sheets parse_simple | 95 MB/s | 95 | 110 |

**Struct-only path gates (three-data-grammar slice):**

- The emitter for JSON, CSS L4, and Sheets writes only into struct
  builders. Grep check: `rg 'TapeBuilder|push_rec' crates/core/src/backend/emitter.rs`
  returns no hits along the JSON/CSS/Sheets dispatch paths.
- `NodeView` accessors for these three grammars read struct fields;
  no tape cursor is instantiated on their parse path.
- Parity harnesses for sonic-rs (JSON), lightningcss (CSS L4),
  simdjson OnDemand (JSON), cssparser (CSS L4), and serde_json
  (JSON) compare struct-vs-native, green across the fixture corpus.

**Tape-remains gate (AZ-II handoff):**

- `crates/tape/` exists on disk and compiles.
- The live tape-symbol scan
  (`::bbnf::runtime::tape|bbnf::runtime::tape|use tape::|\btape::|\bTape(Rec|Builder|Cursor|Offset|Kind)\b|\bColumns\b|\bFinaliser\b|\bDTA\b|\bPSI\b|Fused(Build|Output)`)
  returns hits exclusively in the BBNF-scoped consumer set and
  historical docs. No hit is permitted in JSON, CSS L4, or Sheets
  runtime paths.
- `cargo build -p bbnf` succeeds.

**Coverage gates (structural):**

- IR audit pass reports 100% `->` coverage across JSON, CSS L4, and
  Sheets.
- `StructRegistry` non-empty for every `Named` rule in these three
  grammars.
- lightningcss typed-value parity: every `<length>`, `<color>`,
  `<angle>`, `<time>`, `<resolution>`, `<percentage>` rule in CSS L4
  returns a typed value byte-equivalent to lightningcss's through
  the parity harness on its own fixture corpus.

**Workspace gates:**

- Pass count ≥ 967, fail count ≤ 33, ignored count ≤ 30 on
  `cargo nextest run --workspace --profile ax-iter`.
- Parity harnesses green on every wave boundary.

## Wave structure

Four waves plus FINAL. Each wave has a same-commit runtime call
site, a same-commit bench delta on the three-grammar slice, and a
same-commit samply capture.

| Wave | Headline | Opens after | Status |
|---|---|---|---|
| **W0** | Research + classifier-unification study + audit baseline | AZ-I open | closed (2026-04-27) |
| **W1** | `StructRegistry` + `project_types` closure — JSON + CSS L4 + Sheets | W0 | closed (2026-04-27) |
| **W2** | Direct-to-struct emission — substrate (StructBuilder + JSON runtime + EmitStrategy + 9 per-shape struct-direct emitters) | W1 | closed substrate-only (2026-04-28); activation rolls into **W2-act** |
| **W2-act** | GESTALT-ACTIVATE — resolver flip activates JSON + Sheets + CSS L4 simultaneously; per-grammar runtime view-API; parity harness recoding; 17-entry bench gate; EmitStrategy hoist to `bbnf-ir`; dead-substrate sweep; W4 FINAL absorbed (per `audit/W2-CLOSE-AUDIT.md`) | W2 substrate | in progress |

**Wave plan refined 2026-04-28** per six-agent audit synthesis at
`docs/tranches/AZ-I/audit/W2-CLOSE-AUDIT.md`. The original W2.B / W3 /
W4 waves collapse into W2-act because the W2 substrate is grammar-
general; activation is one resolver match-arm extension per grammar,
not a per-grammar wave. AZ-II's three waves likewise collapse into a
single `AZ-II.cutover` (BBNF cutover + tape deletion), and BB scaffold
opens in parallel. Total declared waves to BB.close: 5 (down from the
17 the trajectory previously held).

### W0 — Research and measurement surface

W0 establishes AZ-I's baseline measurement surface and resolves
every open question that would otherwise drive a reactive sub-wave
mid-tranche. The pre-B2 derive-cache relocation + Watt items are
T3-superseded — B2 retired the proc-macro IR-pipeline contract
entirely; there is no proc-macro to relocate the cache for, no
proc-macro to wrap with Watt; the substrate they presupposed
ceases to exist at B2.W2. The retained items below are the
load-bearing core.

Landed artefacts:

- `docs/tranches/AZ-I/CLASSIFIER-UNIFICATION.md` — research note on
  whether the regex-HIR classifier, the structural-alphabet
  classifier, and the payload-kind classifier can unify on a single
  decision surface for the three data grammars. The note either
  produces a unified design or declares unification intractable and
  locks the three classifiers into their current scoping across
  these grammars.
- IR audit pass at `crates/ir/src/passes/audit/payload_coverage.rs`
  enumerating every typed `->` in JSON, CSS L4, and Sheets and
  reporting emitter coverage. Gates the build on these grammars;
  initially red for every grammar that has not yet migrated.
- Baseline bench capture on the AZ-I branch: three-grammar slice of
  the 17-entry cold-parse matrix, samply fleet, parity harness
  summary.

Runtime call site: the audit pass runs on every `cargo check` via
build.rs and emits `docs/benchmarks/AZ-I/W0/audit-coverage.json`.

No payload path change yet; W0 proves the measurement surface and
lifts the infrastructure ceiling so W1–W3 iterate quickly.

### W1 — `StructRegistry` and `project_types` closure

`project_types` runs to fixed-point across JSON, CSS L4, and Sheets
and populates `StructRegistry` for every Named rule in these three
grammars. Hard-fail-and-block semantics: a grammar that fails to
close through `project_types` halts the build with a diagnostic
naming the unclosed rule and type edge.

Per-grammar gate: `project_types` close on JSON, CSS L4, and Sheets.
Each grammar's closure produces a registered `StructLayout` per
Named rule. BBNF's existing `StructRegistry` entries (if any; they
may remain empty in AZ-I) are not modified.

Runtime call site: the emitter reads `StructRegistry` for every
compound emission on these three grammars. `compute_payload_layouts`
returns a non-empty map for all three. The tape continues to accept
emissions in parallel during W1 — it is the fallback-free bridge
until W2/W3 sever the tape path for these grammars.

Bench delta gate: no regression on any AU-baseline entry; W1's
substrate is populate-only, so no throughput gain is expected, but
no regression is tolerated.

### W2 — Direct-to-struct emission (JSON + Sheets)

The emitter stops writing to the tape for JSON and Sheets. Instead
it writes directly into struct builders whose shapes come from
`StructRegistry`. `push_leaf_with_f64`, `push_leaf_with_i64`,
`push_leaf_with_bool`, `push_leaf_with_span` become field writes on
the owning struct. `begin_compound` / `end_compound` allocate and
close the child struct.

Runtime call site: `NodeView::<Number>::as_f64()` and Sheet cell
accessors read struct fields directly, bypassing any tape cursor
for these two grammars. Bench runs on the struct path.

Bench delta gate: JSON twitter ≥ 1967 MB/s, JSON canada ≥ 1231
MB/s, JSON citm ≥ 2438 MB/s, Sheets parse_simple ≥ 95 MB/s, all on
the struct-only path. Miss ≥ 20% on any entry reverts the wave's
substrate.

### W3 — Direct-to-struct emission (CSS L4)

CSS L4 aggregate and Named emissions move to struct builders.
Typed values (`Length`, `Color`, `Dimension`, `Time`, `Resolution`,
`Percentage`, `Angle`) become enums whose variants match the
grammar's alternation structure. The lightningcss parity harness
passes node-for-node on the normalize, bootstrap, and tailwind
fixtures.

Runtime call site: `css::StyleSheet::rules()` returns
`CssDeclaration { property, value: TypedValue }` from the struct
graph directly; the tape path for CSS L4 is severed at this commit.
Every `<length>` rule returns a `Length` convertible to
`lightningcss::values::length::Length` without loss.

Bench delta gate: CSS normalize ≥ 735 MB/s, bootstrap ≥ 600 MB/s,
tailwind ≥ 500 MB/s, all on the struct-only path. Parity harness
green on the full CSS L4 corpus.

### W4 — FINAL

AZ-I's close matrix:

- Three-grammar slice of the 17-entry AU-baseline matrix at or
  above AU floor on every entry, on the struct-only path, with
  samply captures under `docs/benchmarks/profiles/AZ-I/W4/`.
- Tape-scoped-to-BBNF verification: a CI job that greps for live
  tape-symbol references (`tape::`, `TapeBuilder`, `TapeCursor`,
  `TapeRec`, `push_rec`, `output.tape()`, `payload_bytes`) on the
  JSON, CSS L4, and Sheets parse paths and fails on hit; references
  on the BBNF bootstrap path are permitted.
- Parity harnesses (sonic-rs, lightningcss, simdjson OnDemand,
  cssparser, serde_json) green across the full fixture corpus.
- IR audit pass reports 100% `->` coverage across JSON, CSS L4,
  and Sheets.
- `crates/tape/` compiles and links; BBNF bootstrap test suite
  green on the tape path.

`FINAL.md` records deltas, reversals taken, and hands off to AZ-II
under the contract in §Handoff contract to AZ-II.

### AZ-I.W4 absorbs durable AY-III gates

AY-III.W0 + W1 durable gates fold into the AZ-I.W4 close ceremony
as grammar-general infrastructure. The admission-totality test is
parameterized over the eight-grammar corpus (JSON, CSS L4, Sheets,
BBNF, BNF, EBNF, CSV, math) — each grammar's `Named`-rule axis
verifies admission count ≡ materialiser count ≡ consumer count,
executed via a single macro-driven test harness. The competitor-
keyed close gates (sonic-rs / simd-json for JSON, lightningcss /
cssparser for CSS, where applicable) ride on the post-struct samply
capture; no internal ratio gates. The fused-pipeline wire contract
(parse-root invocation count == `to_value()` invocation count)
generalises to: parse-root invocation count == grammar-derived view
materialisation count, parameterized per grammar.

## Reversal criteria

AZ-I inherits AQ.5's reversal discipline (~32 commits, the cleanest
precedent in project history: the structural pre-scan was deleted at
`2f7c1bd` once the substrate failed to activate its consumer). AZ-I
explicitly rejects AW-IV's anti-precedent (92 commits of declared-
exceedance with zero entries actually exceeding post-AU).

Reversal rules:

1. **Wave-local 20% rule.** A wave that misses its own declared
   throughput gate on the three-grammar slice by more than 20%
   reverts its substrate at wave close. No accumulation of
   unreverted debt across waves.
2. **No-regression rule.** A wave that regresses an already-passing
   AU-baseline entry reverts the responsible substrate immediately,
   regardless of whether its own gate passed. BBNF entries are
   included in this rule — AZ-I may not regress BBNF from AU even
   though AZ-I does not migrate BBNF.
3. **No hedging forward.** A wave does not route its miss to a
   later wave of AZ-I or to AZ-II. `feedback_no-deferrals` is
   enforced.
4. **Substrate-without-activation is a re-plan trigger.** A wave
   that ships substrate without a same-commit runtime call site is
   a hard re-plan signal, not a missed detail. AX invariant 13 is
   non-negotiable.
5. **Reversals are a health signal.** Reversals are first-class
   outcomes. AZ-I budgets an explicit reversal/re-plan lane per wave
   so a miss narrows the architecture instead of softening it. The
   Era V anti-pattern — substrate first, consumer later, reversal
   never — is what AZ-I exists to preclude.

A final reversal budget: if W3 (CSS L4) does not complete the full
lightningcss parity surface in one wave, AZ-I may close with a
CSS-semantic partial only if the CSS parser is still struct-only.
Partial means typed coverage gap (for example selectors or calc
families routed to TODO parity rows), never a tape-backed CSS path.
Any need to re-introduce tape on CSS L4 is a wave revert + re-plan
trigger, not an AZ-I close floor.

## Critical files

AZ-I touches these subsystems. The table below is indicative, not
exhaustive; `git log --stat` on AZ-I's branch at close produces the
authoritative list.

| Path | Role | Wave |
|---|---|---|
| `crates/ir/src/passes/types/` | `project_types` closure over JSON, CSS L4, Sheets | W1 |
| `crates/ir/src/registry/struct.rs` (new) | `StructRegistry` + `StructLayout` | W1 |
| `crates/ir/src/passes/audit/payload_coverage.rs` (new) | IR audit pass, `->` coverage on three data grammars | W0 |
| `crates/core/src/runtime/json/` | JSON runtime — struct-only | W2 |
| `crates/core/src/runtime/sheets/` | Sheets runtime — struct-only | W2 |
| `crates/core/src/runtime/css_l4/` | CSS L4 runtime — struct-only | W3 |
| `crates/core/src/backend/emitter.rs` | Emission path — struct builder calls on three data grammars | W2–W3 |
| `crates/core/src/pipeline/compile.rs` | Compile pipeline — struct path selection per grammar | W2–W3 |
| `grammar/json/`, `grammar/css-l4/`, `grammar/sheets/` | Typed-leaf authoring pass | W1–W3 |
| `tests/*_parity.rs` | Struct-vs-external parity harnesses for three data grammars | W2–W4 |
| `docs/benchmarks/profiles/AZ-I/W<n>/` | Samply fleet per wave | W0–W4 |

Unchanged (BBNF scope, moves in AZ-II):
`crates/tape/`, `grammar/bbnf/`, `crates/core/src/runtime/bbnf/`,
`xtask/src/regen.rs` BBNF entry (the `bbnf_derive` proc-macro
referenced here in earlier drafts retired at B2.W2; the BBNF
self-host parser builds from `crates/core/src/grammar/generated/
bbnf.rs`, refreshed by `cargo xtask regen`).

## Open questions absorbed

The following open questions have dispositions at AZ-I open; W0
research finalises any that remain operational for the three-
grammar slice.

1. **Q1 — backward-pointer form.** The prior answer under a tape-
   retention assumption was "sidecar column, measured at W3". Under
   tape abrogation this dissolves: there is no tape column to widen
   or sidecar on the three data grammars. Struct-tree navigation
   uses parent pointers (`&'arena Parent`) or root-traversal, and
   the decision between the two is a BA-opens problem, not AZ-I's.
   AZ-I.W0 records the dissolution and flags BA as the owner. The
   research note `RESEARCH.md` sketches the parent-pointer-vs-
   root-traversal tradeoff to hand off to BA cleanly.
2. **Q2 — `StructRegistry` partial-close semantics.** Hard-fail-
   and-block on JSON, CSS L4, and Sheets. A grammar whose
   `project_types` does not close produces a build-stop diagnostic
   naming the unclosed rule and type edge. BBNF is exempt in AZ-I.
   AZ-I.W1 owns enforcement.
3. **Q7 — derive-cache key.** Dissolved at B2.W2: the proc-macro
   IR-pipeline contract retired and `crates/derive/` deleted
   outright; there is no proc-macro cache to key, lift, or
   invalidate. Drift detection now lives in standard "diff after
   regen" hygiene — `cargo xtask regen --check` regenerates to a
   tempdir and exits non-zero on divergence; CI invokes it before
   `iter-check` and pre-commit invokes it when grammar files or the
   regen entrypoint change. AZ-I.W0 records the dissolution.
4. **Q9 — classifier collision.** Front-loaded research in AZ-I.W0;
   `CLASSIFIER-UNIFICATION.md` either specifies a unified decision
   surface across regex-HIR, structural-alphabet, and payload-kind
   classifiers for the three data grammars, or declares unification
   intractable and locks the existing three-classifier split.
   Either outcome is acceptable; the unacceptable outcome is an
   unresolved question driving a reactive W3' sub-wave mid-tranche.

BBNF-specific cutover questions (bootstrap reproducibility, tape
deletion, drift-source mitigations in BOOTSTRAP-CUTOVER.md) route
to AZ-II. No partial-closure floor is pre-declared for AZ-II; full
tape abrogation is the close gate on that tranche as well.

## Defensible floor

Non-negotiable at AZ-I close:

1. **JSON twitter ≥ 1967 MB/s on the struct-only path.** First-
   order recovery gate.
2. **CSS bootstrap ≥ 600 MB/s on the struct-only path.**
3. **Sheets parse_simple ≥ 95 MB/s on the struct-only path.**
4. **`StructRegistry` non-empty for every Named rule in JSON, CSS
   L4, and Sheets.**
5. **IR audit pass reports 100% `->` coverage on JSON, CSS L4, and
   Sheets.**
6. **CSS L4 at minimum `<length>` typed and lightningcss-parity-
   green on its own corpus.** Aggregate CSS (colors, calc, lists)
   may remain semantically partial only as typed struct surfaces with
   named TODO parity rows. They may not remain tape-backed.
7. **`crates/tape/` compiles and links; BBNF bootstrap path
   unchanged.**

Partial-close escape clause: if classifier unification (W0) is
declared intractable AND CSS L4 aggregate struct-emission (W3)
fails parity on the lightningcss corpus, AZ-I may close with
"direct-to-struct on JSON and Sheets; CSS L4 partial (length/color
typed, selectors/calc/colors incomplete against named lightningcss
rows); tape scoped to BBNF only". The follow-on wave or tranche
(AZ-I') picks up CSS semantic parity completion before AZ-II opens.
AZ-I' is not permitted to carry a CSS tape bridge.

Anything less than the partial-close floor is Era V recurring:
substrate without activation, consumer without substrate, or tape-
and-struct side-by-side across primary data grammars. The plan does
not accept that outcome.

## Executed preflight truth from 2026-04-24

The 2026-04-24 probability-lift pass found three missing live
substrates that AZ-I must not assume:

- `docs/tranches/AZ-I/CLASSIFIER-UNIFICATION.md` is required by W0
  but absent at the pass.
- `crates/ir/src/passes/audit/payload_coverage.rs` is required by
  W0/W1 but absent at the pass.
- `StructRegistry` / `StructLayout` are not code-real yet; current
  IR payload docs still describe the old registry as deleted.

Before W1 opens, AZ-I must prove at least one vertical slice per
data-grammar family:

1. JSON scalar direct-to-struct with no `TapeBuilder`, `TapeCursor`,
   `output.tape()`, or `payload_bytes` in the expanded slice.
2. Sheets `parse_simple` struct output for numeric/bool/error/ref
   cases.
3. CSS declaration + `Length` struct output against lightningcss,
   with no tape fallback.
4. CSS `Color` scalar-packed output and one `CursorChild`
   materialization case before W3 aggregate rollout.

These slices are not overfitting fixtures; they are grammar-derived
admission tests for the generic direct-to-struct machinery.

## Handoff contract to AZ-II

At AZ-I close, AZ-II opens on the following guaranteed state:

1. **Three data grammars running direct-to-struct.** JSON, CSS L4,
   and Sheets parse into `StructRegistry`-backed structs through a
   single codegen path. No tape materialisation on their hot paths.
2. **`StructRegistry` closed on those three.** Every Named rule in
   JSON, CSS L4, and Sheets has a registered `StructLayout`. The
   IR audit pass reports 100% `->` coverage on these three.
3. **Tape crate compiles.** `crates/tape/` exists on disk. Its
   symbols (`TapeRec`, `TapeBuilder`, `TapeCursor`, `Columns`,
   `Visitor`, `Finaliser`, `DTA`, `PSI`, `Stage1`, `StructuralScan`,
   `Packed`, `Decoder`, `Dedup`, `Kind`, `Driver`, `Profile`) are
   all present and link. The crate is not shrunk in AZ-I; AZ-II
   owns the full deletion.
4. **BBNF grammar unchanged.** `grammar/bbnf/bbnf.bbnf` is not
   edited in AZ-I. The BBNF self-host parser regenerates via
   `cargo xtask regen --grammar bbnf` to
   `crates/core/src/grammar/generated/bbnf.rs` (the proc-macro
   IR-pipeline contract retired at B2.W2; the on-disk per-grammar
   source is the substrate every consumer reads). The BBNF
   bootstrap test suite is green on the tape path.
5. **17-entry matrix at AU parity.** The three-data-grammar slice
   on the struct path; the BBNF-slice on the tape path. AZ-II
   carries both forward as its opening budget.
6. **Classifier scoping resolved.** The W0 research note declares
   either a unified classifier surface or the locked three-
   classifier split. AZ-II inherits that answer and extends it to
   BBNF-specific patterns.
7. **Research artefacts cited.** `AZ-I/RESEARCH.md` is the authoritative
   external-grounding document; AZ-II's BBNF-bootstrap-cutover
   research builds on its parent-pointer sketch (§3) and its
   simdjson/sonic-rs/lightningcss technique survey (§5).

AZ-II's opening gate is a clean read of this contract: if any of
1–6 fails at AZ-I's declared close commit, AZ-II does not open and
AZ-I re-plans to close the gap.
