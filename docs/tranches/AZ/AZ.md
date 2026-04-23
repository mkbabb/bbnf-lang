# Tranche AZ — Direct-to-Struct Across the Fleet + Tape Dissolution

AZ is the transformational tranche. Every production grammar — JSON,
CSS L4, Sheets, and BBNF itself — derives its own native Rust struct
shape from `project_types` feeding `StructRegistry`, and parse output
lands directly into those structs. `crates/tape/` dissolves entirely:
its roles either vanish, fold into the struct tree, or migrate to
single-purpose test artefacts on the parity harness side.

AZ is not *tape activation followed by a later dissolution*. AZ is
activation-by-dissolution: the native struct replaces the tape on
every grammar in the same tranche, leaving no orthogonal substrate
behind.

## Thesis

Grammar-derived direct-to-struct is the single materialised form of a
parse. A typed `->` in a grammar rule is the declaration of a field
(or a compound shape) in the emitted struct; the IR type-inference
pass composes these into a closed struct graph; the emitter writes
straight into the graph without traversing an intermediate tape. No
role-5 fallback for "undeclared struct targets" — every grammar,
including BBNF's own, flows through `project_types` and lands in a
derived struct.

The `crates/tape/` substrate, in its five historical roles, dissolves
as follows:

1. **Intermediate during parse** — disappears. The emitter writes
   struct fields directly; there is no intermediate record stream.
2. **Materialised view source** — disappears. Views are direct
   projections of the struct graph; accessors are struct field reads.
3. **Debug substrate** — folds into a diagnostic serialiser on the
   struct graph. The `@debug` directive lowers to a struct-tree
   walker, not a tape replay.
4. **Parity oracle substrate** — migrates to parity harnesses only.
   Harnesses that compare bbnf output against sonic-rs, lightningcss,
   simdjson, cssparser, and serde_json compare struct-vs-struct (or
   struct-vs-harness-native), never tape-vs-struct.
5. **Undeclared-grammar fallback** — no such role exists after AZ.
   Every grammar has a `project_types` result; emptiness is a hard
   failure, not a soft fallback.

The struct IS the materialised form. There is no second
representation that the parser or the backend can route to.

## What this replaces

AZ subsumes the scope of the prior "grammar-derived tape activation"
plan (archived as `OLD-BA-absorbed.md` in this directory). That plan
activated `project_types` and `StructRegistry` across the fleet but
preserved the tape substrate as the materialised form, with views
layered on top. AZ keeps the activation scope intact but rejects the
preservation.

Concrete differences:

- Activation-only plan: populate `StructRegistry`, emit into tape,
  view layer projects tape records into typed accessors. Struct
  shape is a *view-time concept*; the tape remains canonical.
- AZ: populate `StructRegistry`, emit into struct fields, view layer
  reads struct fields directly. Struct shape is the *parse-time
  canonical form*; tape is deleted.

The AU-baseline throughput gates carry over unchanged (JSON twitter
≥ 1967 MB/s, CSS bootstrap ≥ 600 MB/s, full 17-entry matrix parity
with AU-baseline). AZ inherits these as its close conditions, not as
its opening budget. The opening budget is AY-II's close state —
whatever AY-II lands is the ceiling AZ cannot regress beneath.

## Invariants

1. **Typed materialisation across the fleet.** Every `->` in every
   production grammar reaches a struct field or a compound-opening
   call on the struct builder. An IR audit pass enforces 100%
   coverage and fails the build on miss. No grammar opts out.
2. **Single codegen path.** One emission path: struct builder calls.
   No combinator fallback. No tape fallback. No "conditional
   struct-or-tape" branch in the emitter. `feedback_no-orthogonal-codepaths`
   is load-bearing.
3. **Rich AST preserved.** Struct shapes carry the full
   lightningcss-level richness. Flattening a typed grammar rule for
   speed is not permitted — `feedback_preserve-rich-ast` is in force.
   CSS L4 `Length`, `Color`, `Dimension`, `Time`, `Resolution`,
   `Percentage`, `Angle` each get a typed Rust enum whose variants
   match the grammar's alternation structure and whose fidelity meets
   or exceeds lightningcss on its own fixtures.
4. **BBNF self-hosts on a derived struct.** BBNF's own grammar
   (`grammars/bbnf/bbnf.bbnf`) feeds `project_types` and produces a
   `BbnfAst` struct (or similarly-named root type) through the same
   pipeline every other grammar uses. The bootstrap does not special-
   case BBNF into a tape path.
5. **`crates/tape/` deleted at close.** By AZ's close commit, the
   `crates/tape/` directory does not exist in the tree.
   `cargo build -p bbnf --no-default-features` does not depend on a
   `bbnf-tape` crate. No downstream crate imports `bbnf_tape::*`.
6. **Measurement gates substrate.** Every wave's substrate lands at
   the same commit as a runtime call site that reads the new
   structure, plus a same-commit bench delta on the 17-entry matrix.
   AX invariant 13 is in force.
7. **No deferrals.** Every optimisation needed to hit an AZ gate
   lands in the wave that owns that gate; no carry-forward to a later
   wave or to the next tranche. `feedback_no-deferrals`.

## Hard gates (AU-baseline anchored)

AZ's close matrix is the AU-baseline 17-entry matrix from
`AU/FINAL.md`, evaluated on the struct-only parse path. AY-II's
close state is AZ's opening ceiling; AZ close must meet or exceed
AY-II close on every entry AND complete tape deletion.

**Throughput gates (parity recovery first, then exceedance):**

| Grammar / fixture | AU-baseline | AZ floor | AZ target |
|---|---:|---:|---:|
| JSON canada | 1231 MB/s | 1231 | 1500 |
| JSON citm | 2438 MB/s | 2438 | 2700 |
| JSON twitter | 1967 MB/s | 1967 | 2200 |
| CSS normalize | 735 MB/s | 735 | 850 |
| CSS bootstrap | 454 MB/s | 600 | 700 |
| CSS tailwind | 496 MB/s | 500 | 600 |
| Sheets parse_simple | 95 MB/s | 95 | 110 |

**Struct-only path gates:**

- `cargo build -p bbnf --no-default-features` succeeds without
  `crates/tape/` existing on disk.
- `rg --files crates/ | rg '^crates/tape/'` returns zero matches at
  AZ close.
- `rg 'use bbnf_tape' crates/` returns zero matches at AZ close.
- No `TapeRec`, `TapeBuilder`, `TapeCursor`, or `Columns` symbol is
  referenced by non-test code at AZ close.

**Coverage gates (structural):**

- IR audit pass reports 100% `->` coverage across all grammars.
- `StructRegistry` non-empty for every `Named` rule in JSON, CSS L4,
  Sheets, and BBNF.
- lightningcss typed-value parity: every `<length>`, `<color>`,
  `<angle>`, `<time>`, `<resolution>`, `<percentage>` rule in CSS L4
  returns a typed value byte-equivalent to lightningcss's through the
  parity harness on its own fixture corpus.

**Workspace gates:**

- Pass count ≥ 967, fail count ≤ 33, ignored count ≤ 30 on
  `cargo nextest run --workspace --profile ax-iter`.
- Parity harnesses (sonic-rs, lightningcss, simdjson OnDemand,
  cssparser, serde_json) green on every wave boundary.

## Wave structure

Seven waves. Each wave has a same-commit runtime call site, a same-
commit 17-entry bench delta, and a same-commit samply capture.

| Wave | Headline | Opens after | Status |
|---|---|---|---|
| **W0** | Research + classifier-unification study + audit baseline | AZ open | planned |
| **W1** | `StructRegistry` + `project_types` closure across all grammars | W0 | planned |
| **W2** | Direct-to-struct emission — JSON + Sheets | W1 | planned |
| **W3** | Direct-to-struct emission — CSS L4 aggregate / Named | W2 | planned |
| **W4** | Direct-to-struct emission — BBNF self-hosting + struct-vs-struct parity harness | W3 | planned |
| **W5** | `crates/tape/` deletion + runtime alias-shim retirement + view recode | W4 | planned |
| **W6** | FINAL — 17-entry AU-baseline close + tape-crate-deleted verification | W5 | planned |

### W0 — Research and measurement surface

W0 establishes AZ's baseline measurement surface and resolves every
open question that would otherwise drive a reactive sub-wave mid-
tranche.

Landed artefacts:

- `docs/tranches/AZ/CLASSIFIER-UNIFICATION.md` — research note on
  whether the regex-HIR classifier, the structural-alphabet
  classifier, and the payload-kind classifier can unify on a single
  decision surface (open question Q9). The note either produces a
  unified design or declares unification intractable and locks the
  three classifiers into their current scoping.
- Derive-cache lift to `$XDG_CACHE_HOME/bbnf-derive/` with composite
  fingerprint `(grammar-sha, derive-version, rustc-sha, codegen-flags)`.
  Cache invalidation test suite under
  `crates/derive/tests/cache_invalidation/`. (Open question Q7.)
- IR audit pass at `crates/ir/src/passes/audit/payload_coverage.rs`
  enumerating every typed `->` in every grammar and reporting
  emitter coverage. Gates the build; initially red for every
  grammar that has not yet migrated.
- Baseline bench capture on the AZ branch: 17-entry cold-parse
  matrix, samply fleet, parity harness summary.

Runtime call site: the audit pass runs on every `cargo check` via
build.rs and emits `docs/benchmarks/AZ/W0/audit-coverage.json`.

No payload path change yet; W0 proves the measurement surface and
lifts the infrastructure ceiling so W1–W4 iterate quickly.

### W1 — `StructRegistry` and `project_types` closure

`project_types` runs to fixed-point across all production grammars
and populates `StructRegistry` for every Named rule. Q2's hard-fail-
and-block semantics: a grammar that fails to close through
`project_types` halts the build with a diagnostic naming the
unclosed rule and type edge.

Per-grammar gate: `project_types` close on JSON, CSS L4, Sheets, and
BBNF. Each grammar's closure produces a registered `StructLayout`
per Named rule.

Runtime call site: the emitter reads `StructRegistry` for every
compound emission. `compute_payload_layouts` returns a non-empty map
for all four grammars. The tape continues to accept emissions in
parallel during W1 — it is the fallback-free bridge until W2.

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
accessors read struct fields directly, bypassing any tape cursor.
Bench runs on the struct path.

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
lightningcss's `lightningcss::values::length::Length` without loss.

Bench delta gate: CSS normalize ≥ 735 MB/s, bootstrap ≥ 600 MB/s,
tailwind ≥ 500 MB/s, all on the struct-only path. Parity harness
green on the full CSS L4 corpus.

### W4 — BBNF self-hosting on derived struct

BBNF's own grammar migrates to the struct path. `project_types`
applied to `grammars/bbnf/bbnf.bbnf` yields a root struct whose
shape mirrors the in-memory IR surface the compiler already uses
internally (a `BbnfAst` with `rules: Vec<Rule>`, `imports:
Vec<Import>`, `directives: Vec<Directive>`, and so on). The bbnf-
derive proc-macro's embedded parser — the piece that reads a
grammar file during build — stops using the tape cursor and reads
from the derived struct instead.

Parity harness recoded: the bootstrap self-host test compares
struct-vs-struct against the hand-maintained IR surface, not tape-
vs-tape. Bootstrap reproducibility test (rebuild BBNF from BBNF-
on-derived-struct and confirm byte-equal output with the previous
bootstrap) is the load-bearing close gate.

Runtime call site: `grammars/bbnf/bbnf.bbnf` parsed via the derived
struct path produces the same IR graph as the prior tape path, byte-
equal on every test fixture.

Bench delta gate: BBNF grammar parse throughput (currently an
internal build-time cost, not user-facing) does not regress more
than 10% relative to W3 close; if it does, the W4 substrate is
reviewed for the cause and either fixed or reverted.

### W5 — Tape dissolution

`crates/tape/` is deleted. Its symbols (`TapeRec`, `TapeBuilder`,
`TapeCursor`, `Columns`, `Visitor`, `Finaliser`, `DTA`, `PSI`,
`Stage1`, `StructuralScan`, `Packed`, `Decoder`, `Dedup`, `Kind`,
`Driver`, `Profile`) are either deleted outright or relocated to
single-purpose test artefacts inside parity harnesses.

`crates/core/src/runtime/mod.rs` alias shims (any re-exports that
preserved tape compatibility during W2–W4) are retired. The view
layer (`crates/core/src/backend/driver/*`) is rewritten to target
struct shapes directly; any path that consulted tape offsets is
rewritten to consult struct field indices.

Runtime call site: the entire production parse path routes through
struct builders. The tape crate is unreachable from any
`#[cfg(not(test))]` code at this commit.

Gate: `rg '^crates/tape/'` on the tree returns zero matches.
`cargo build -p bbnf --no-default-features` succeeds. No test
imports `bbnf_tape::*` outside parity harnesses (and those imports
migrate to inline fixture structs by W6 open).

### W6 — FINAL

AZ's close matrix:

- 17-entry AU-baseline matrix at or above AU floor on every entry,
  on the struct-only path, with samply captures under
  `docs/benchmarks/profiles/AZ/W6/`.
- Tape-crate-deleted verification: a CI job that grep-walks the tree
  for any `bbnf_tape` / `crates/tape/` reference and fails on hit.
- Parity harnesses (sonic-rs, lightningcss, simdjson OnDemand,
  cssparser, serde_json) green across the full fixture corpus.
- IR audit pass reports 100% `->` coverage across JSON, CSS L4,
  Sheets, and BBNF.
- BBNF self-host reproduces byte-equal across a clean rebuild from
  the derived struct path.

`FINAL.md` records deltas, reversals taken, and any follow-on work
routed to BA (pointer queries on the struct tree) or BB (egraph
rules over the stable IR).

## Reversal criteria

AZ inherits AQ.5's reversal discipline (~32 commits, the cleanest
precedent in project history: the structural pre-scan was deleted at
`2f7c1bd` once the substrate failed to activate its consumer). AZ
explicitly rejects AW-IV's anti-precedent (92 commits of declared-
exceedance with zero entries actually exceeding post-AU).

Reversal rules:

1. **Wave-local 20% rule.** A wave that misses its own declared
   throughput gate by more than 20% reverts its substrate at wave
   close. No accumulation of unreverted debt across waves.
2. **No-regression rule.** A wave that regresses an already-passing
   AU-baseline entry reverts the responsible substrate immediately,
   regardless of whether its own gate passed.
3. **No hedging forward.** A wave does not route its miss to a
   later wave of AZ or to BA or BB. `feedback_no-deferrals` is
   enforced.
4. **Substrate-without-activation is a re-plan trigger.** A wave
   that ships substrate without a same-commit runtime call site is
   a hard re-plan signal, not a missed detail. AX invariant 13 is
   non-negotiable.
5. **Reversals are a health signal.** Reversals are first-class
   outcomes. AZ budgets for at least one reversal per wave as the
   expected case. The Era V anti-pattern — substrate first, consumer
   later, reversal never — is what AZ exists to preclude.

A final reversal budget: if W5 (tape dissolution) cannot complete
without re-introducing a tape path in any grammar, AZ invokes its
defensible floor (see §Defensible floor) and closes as a partial
tape-retention tranche.

## Critical files

AZ touches these subsystems. The table below is indicative, not
exhaustive; `git log --stat` on AZ's branch at close produces the
authoritative list.

| Path | Role | Wave |
|---|---|---|
| `crates/ir/src/passes/types/` | `project_types` closure over every grammar | W1 |
| `crates/ir/src/registry/struct.rs` (new) | `StructRegistry` + `StructLayout` | W1 |
| `crates/ir/src/passes/audit/payload_coverage.rs` (new) | IR audit pass, `->` coverage | W0 |
| `crates/core/src/runtime/mod.rs` | Runtime entry; tape alias shims retired | W5 |
| `crates/core/src/backend/driver/*` | View layer rewritten to struct shapes | W5 |
| `crates/core/src/backend/emitter.rs` | Emission path — struct builder calls only | W2–W5 |
| `crates/core/src/pipeline/compile.rs` | Compile pipeline — struct-only | W2–W5 |
| `crates/derive/` | Derive-cache lift; BBNF bootstrap cutover | W0, W4 |
| `crates/derive/tests/cache_invalidation/` (new) | Cache fingerprint tests | W0 |
| `crates/tape/` | Deleted at W5 | W5 |
| `grammars/bbnf/bbnf.bbnf` | Source for BBNF's derived struct | W4 |
| `tests/*_parity.rs` | Struct-vs-external parity harnesses | W2–W6 |
| `docs/benchmarks/profiles/AZ/W<n>/` | Samply fleet per wave | W0–W6 |

The `crates/tape/` deletion (W5) is the single largest LOC delta in
AZ. The current tape crate contains `lib.rs`, `builder/`, `columns`,
`cursor`, `decoders`, `dedup`, `driver`, `dta`, `finaliser`, `kind`,
`packed`, `profile`, `psi`, `stage1`, `structural_scan`, `tape`, and
`visitor` modules; every one of those symbols leaves the tree at W5.
That scale of deletion is the surface on which AZ's simplification
shows up.

## Open questions absorbed

The following open questions have dispositions at AZ open; W0
research finalises any that remain operational.

1. **Q1 — backward-pointer form.** The prior answer under a tape-
   retention assumption was "sidecar column, measured at W3". Under
   tape abrogation this dissolves: there is no tape column to widen
   or sidecar. Struct-tree navigation uses parent pointers
   (`&'arena Parent`) or root-traversal, and the decision between
   the two is a BA-opens problem, not AZ's. AZ.W0 records the
   dissolution and flags BA as the owner. The research note
   `RESEARCH.md` §3 sketches the parent-pointer-vs-root-traversal
   tradeoff to hand off to BA cleanly.
2. **Q2 — `StructRegistry` partial-close semantics.** Hard-fail-
   and-block. A grammar whose `project_types` does not close
   produces a build-stop diagnostic naming the unclosed rule and
   type edge. No "empty registry is acceptable" fallback. AZ.W1
   owns enforcement.
3. **Q7 — derive-cache key.** Composite fingerprint
   `(grammar-sha, derive-version, rustc-sha, codegen-flags)` with a
   test suite under `crates/derive/tests/cache_invalidation/`
   validating every component. Lift target
   `$XDG_CACHE_HOME/bbnf-derive/`. AZ.W0 owns.
4. **Q9 — classifier collision.** Front-loaded research in AZ.W0;
   `CLASSIFIER-UNIFICATION.md` either specifies a unified decision
   surface across regex-HIR, structural-alphabet, and payload-kind
   classifiers, or declares unification intractable and locks the
   existing three-classifier split. Either outcome is acceptable;
   the unacceptable outcome is an unresolved question driving a
   reactive W3' or W4' sub-wave mid-tranche.

No other open questions bear on AZ. Questions that concern BA's
pointer-query design (laziness, skip masks, path compilation) or
BB's egraph rule inference (cost models, rewrite confluence) are
out of scope and surface in those tranches.

## BBNF self-hosting

BBNF's grammar describes BBNF itself; `project_types` applied to
`grammars/bbnf/bbnf.bbnf` therefore produces a struct graph whose
shape is the BBNF AST. The derived root type is `BbnfAst`, with
fields drawn from the grammar's top-level productions:

- `imports: Vec<Import>` — from the `import` production.
- `directives: Vec<Directive>` — from the `directive` production.
- `rules: Vec<Rule>` — from the `rule` production.
- `comments: Vec<Comment>` — from the `comment` production.

Each field's element type is itself a derived struct. `Rule`
decomposes into `name: Ident`, `params: Vec<Param>`, `return_type:
Option<TypeExpr>`, `body: Expr`. `Expr` is a derived enum with a
variant per alternation in the grammar's `expr` production. The
derivation is mechanical: `project_types` produces the graph; the
emitter materialises a Rust type per registered `StructLayout`.

The circular-bootstrap concern is load-bearing. BBNF's parser is
produced by bbnf-derive, which is itself built by expanding BBNF's
grammar. Pre-AZ the bootstrap walks a tape during grammar reads;
post-AZ it must walk a derived struct. The cutover is the W4 work.

Cutover mechanism:

1. **Two-stage bootstrap.** Pre-W4 compiler (tape-based) builds
   the W4 compiler (struct-based). The W4 compiler then rebuilds
   itself from its own source using only the struct path.
2. **Byte-equal reproducibility check.** The self-build output
   (tape-less compiler built by tape-less compiler) must be byte-
   equal to the previous self-build output (tape-less compiler built
   by tape-based compiler) on every grammar in the corpus. This is
   the W4 close gate.
3. **Tape path remains present until W4 close, then is pulled.**
   The W4 opening state has both paths wired; the W4 closing state
   has only the struct path wired in BBNF's own bootstrap. W5 then
   deletes the tape crate proper.
4. **No special case in `project_types`.** The pass treats
   BBNF's grammar exactly like any other. The only BBNF-specific
   work is the bootstrap harness that verifies reproducibility.

An escape hatch for W4: if byte-equal reproducibility fails
non-trivially (e.g., the struct path introduces a deterministic but
different AST ordering), W4 reverts the BBNF migration and AZ
closes with tape retained for BBNF specifically — the defensible
floor (§Defensible floor) covers this case.

## Cross-tranche contract

AZ closes after AY-II. AZ's close gate is a hard prerequisite for
BA open.

**AZ → BA.** BA operates on the struct tree that AZ produces.
Pointer queries, the subject of BA, are defined on struct paths, not
tape offsets. BA's design (laziness, path compilation, skip masks) is
independent of whether AZ's struct tree is populated eagerly or
lazily — AZ populates eagerly; BA introduces laziness on the
already-activated substrate. The parent-pointer question that AZ
defers lands in BA's scope, specifically BA.W0.

**AZ → BB.** BB (egraph rewrite rule inference) does not depend on
AZ's struct shape specifically. BB depends on the IR being stable,
and AZ does not rewire IR — `project_types` closes tighter and
`StructRegistry` populates, but the IR's edge structure and pass
ordering do not change. BB opens on AZ close regardless of BA's
progress.

**AZ does not open on AY-II.1 or earlier partial states.** AY-II
must close fully before AZ opens; any AY-II reversal pushes AZ's
open date. This is the explicit cross-tranche interlock that
prevents Era V's substrate-stacking failure mode.

## Defensible floor

Non-negotiable at AZ close:

1. **JSON twitter ≥ 1967 MB/s on the struct-only path.** First-
   order recovery gate.
2. **CSS bootstrap ≥ 600 MB/s on the struct-only path.**
3. **`StructRegistry` non-empty for every Named rule in at least
   three of four production grammars** (JSON, CSS L4, Sheets).
4. **IR audit pass reports 100% `->` coverage for at least three
   of four grammars.**
5. **`crates/tape/` deleted for at least three of four grammars'
   worth of parse paths.**

Partial-close escape clause: if classifier unification (W0) is
declared intractable AND BBNF self-hosting (W4) fails byte-equal
reproducibility, AZ may close with "direct-to-struct on JSON, CSS
L4, Sheets; tape retained for BBNF only". The tape crate in that
scenario shrinks to the BBNF-bootstrap path only — `bbnf-tape-mini`
rather than full `bbnf-tape` — and tape deletion is deferred to a
targeted follow-on tranche. This is the escape hatch of last
resort, not a planned outcome; AZ's success case is full tape
dissolution including BBNF.

Anything less than the partial-close floor is Era V recurring:
substrate without activation, consumer without substrate, or tape-
and-struct side-by-side. The plan does not accept that outcome.
