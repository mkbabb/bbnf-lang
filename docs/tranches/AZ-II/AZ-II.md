# Tranche AZ-II — BBNF Self-Hosting Direct-to-Struct + Tape Dissolution

AZ-II is the tranche that closes the direct-to-struct migration.
BBNF's own grammar — `grammar/bbnf/bbnf.bbnf`, the grammar that
describes BBNF itself — moves to the `project_types`-derived
struct path via a two-stage bootstrap cutover. Once BBNF parses
into a derived struct, the tape crate has no remaining consumers.
AZ-II deletes `crates/tape/` and recodes the view layer, parity
harnesses, and debug substrate to the struct-tree shape.

AZ-II opens on AZ-I's close contract: three data grammars
(JSON, CSS L4, Sheets) already running direct-to-struct,
`StructRegistry` closed on those three, and `crates/tape/`
compilable but scoped to BBNF's bootstrap only. AZ-II's work is
the last consumer migration + the substrate deletion.

## Thesis

BBNF self-hosts on a grammar-derived struct. `project_types`
applied to `grammar/bbnf/bbnf.bbnf` produces a `BbnfAst` struct
graph whose shape mirrors the compiler's in-memory IR surface. The
`bbnf-derive` proc-macro's embedded parser — the piece that reads a
grammar file during build — stops using the tape cursor and reads
from the derived struct instead.

The cutover is byte-equal reproducibility across two stages:

1. **Stage A.** The pre-AZ-II compiler (tape-based, inherited from
   AZ-I close) builds the AZ-II-candidate compiler (struct-based).
   The candidate's `bbnf-derive` emits struct-writing parsers; the
   candidate itself was built from a tape-writing parser.
2. **Stage B.** The AZ-II-candidate compiler rebuilds itself from
   its own source. The final compiler is built from a struct-
   writing parser and produces struct-writing parsers. The tape
   is unwired in both directions.
3. **Close gate.** Stage B output is byte-equal to Stage A output
   on the full BBNF grammar corpus (every `.bbnf` fixture in the
   tree, including `grammar/bbnf/bbnf.bbnf` itself). Any divergence
   reverts the W2 substrate and invokes the escape clause.

Once Stage B is green, `crates/tape/` has no remaining consumers.
AZ-II W3 deletes the crate and recodes every downstream reference.

## Dependency on AZ-I

AZ-II opens on AZ-I's seven-point handoff contract (AZ-I.md
§Handoff contract to AZ-II):

1. JSON, CSS L4, Sheets direct-to-struct on a single codegen path.
2. `StructRegistry` closed on those three.
3. `crates/tape/` compilable on disk.
4. `grammar/bbnf/bbnf.bbnf` unchanged; `bbnf-derive` produces
   tape-writing parsers; BBNF bootstrap test suite green.
5. 17-entry matrix at AU parity (three-data-slice on struct,
   BBNF-slice on tape).
6. Classifier scoping disposition (unified or locked-split) landed.
7. Research artefacts cited (parent-pointer study, struct-shape
   sketches, external-parser technique survey).

If any point fails at AZ-I close, AZ-II does not open. This is
an explicit interlock, not a soft dependency.

## Invariants

1. **BBNF self-parse byte-identical pre-and-post cutover.** Stage A
   compiler output = Stage B compiler output on every BBNF fixture.
   This is the load-bearing invariant; violation triggers the
   escape clause (see §Reversal criteria).
2. **Tape crate deleted at close.** `crates/tape/` does not exist
   on disk at AZ-II close. `cargo build -p bbnf` succeeds without
   it. The full 17-entry matrix evaluates against the struct-only
   codegen path.
3. **No orthogonal substrate remains.** Exactly one materialised
   form in the tree: the grammar-derived struct graph.
   `feedback_no-orthogonal-codepaths` is load-bearing on the final
   commit.
4. **IR audit pass fleet-wide 100%.** The W0-AZ-I audit pass is
   extended to cover BBNF; at AZ-II close it reports 100% `->`
   coverage on JSON, CSS L4, Sheets, and BBNF.
5. **Rich AST preserved on BBNF.** The `BbnfAst` struct carries the
   full BBNF grammar richness — every alternation in `expr`, every
   directive kind, every rule-parameter shape, every regex-HIR
   reference — with no flattening for parse speed.
   `feedback_preserve-rich-ast` in force.
6. **Measurement gates substrate.** Every wave's substrate lands
   at the same commit as a runtime call site + bench delta + samply
   capture. AX invariant 13.
7. **No deferrals.** Every optimisation needed to hit an AZ-II gate
   lands in the wave that owns the gate. `feedback_no-deferrals`.

## Hard gates

AZ-II's close matrix is the full 17-entry AU-baseline matrix
evaluated on the struct-only parse path for all four grammars. No
entry may regress from AZ-I close.

**Throughput gates (full 17-entry matrix, struct-only path):**

| Grammar / fixture | AU-baseline | AZ-II floor | AZ-II target |
|---|---:|---:|---:|
| JSON canada | 1231 MB/s | 1231 | 1500 |
| JSON citm | 2438 MB/s | 2438 | 2700 |
| JSON twitter | 1967 MB/s | 1967 | 2200 |
| CSS normalize | 735 MB/s | 735 | 850 |
| CSS bootstrap | 454 MB/s | 600 | 700 |
| CSS tailwind | 496 MB/s | 500 | 600 |
| Sheets parse_simple | 95 MB/s | 95 | 110 |
| BBNF self-parse | AU-baseline | ≥ AU | 10% better |

(The BBNF self-parse throughput entry is the internal build-time
grammar-read step; it is measured but lightly weighted — regression
≤ 10% is acceptable per §Reversal criteria, not under the wave
20% rule.)

**Bootstrap reproducibility gate:**

- Stage A output (pre-AZ-II compiler builds candidate) is captured
  at W1 close.
- Stage B output (candidate rebuilds itself) is captured at W2
  close.
- `diff -r <stage-A-output> <stage-B-output>` returns zero byte
  differences across the entire BBNF fixture corpus.

**Tape-deleted gates:**

- `rg '^crates/tape/'` on the tree at AZ-II close returns zero
  matches.
- The live tape-symbol scan
  (`::bbnf::runtime::tape|bbnf::runtime::tape|use tape::|\btape::|\bTape(Rec|Builder|Cursor|Offset|Kind)\b|\bColumns\b|\bFinaliser\b|\bDTA\b|\bPSI\b|Fused(Build|Output)`)
  returns zero matches outside historical docs.
- `rg '\bTapeRec\b|\bTapeBuilder\b|\bTapeCursor\b|\bColumns\b|\bFinaliser\b|\bDTA\b|\bPSI\b' crates/ --type rust`
  returns zero matches outside historical artefacts under
  `docs/tranches/AZ-I/old-BA-artifacts/`.
- `cargo build -p bbnf --no-default-features` succeeds without
  `crates/tape/` existing.

**Coverage gates (structural):**

- IR audit pass reports 100% `->` coverage fleet-wide (JSON,
  CSS L4, Sheets, BBNF).
- `StructRegistry` non-empty for every Named rule in the four
  grammars, including BBNF.
- Parity harnesses recoded to struct-vs-external-native on all
  four grammars; no tape-vs-struct comparison remains.

**Workspace gates:**

- Pass count ≥ 967, fail count ≤ 33, ignored count ≤ 30 on
  `cargo nextest run --workspace --profile ax-iter`.

## Wave structure

Three waves plus FINAL. Each wave has a same-commit runtime call
site, a same-commit bench delta, and a same-commit samply capture.

| Wave | Headline | Opens after | Status |
|---|---|---|---|
| **W0** | BBNF bootstrap research + cutover design + classifier extension | AZ-II open | planned |
| **W1** | Stage A — tape-compiler builds struct-compiler candidate | W0 | planned |
| **W2** | Stage B — candidate rebuilds itself + byte-equal close gate | W1 | planned |
| **W3** | FINAL — `crates/tape/` deletion + parity recode + BA handoff | W2 | planned |

### W0 — Research + cutover design + classifier extension

W0 authors the cutover design doc, extends the classifier scoping
decision from AZ-I.W0 to BBNF-specific patterns, and captures the
AZ-II baseline.

Landed artefacts:

- `docs/tranches/AZ-II/BOOTSTRAP-CUTOVER.md` — research note
  (forthcoming, W0 deliverable) that specifies Stage A / Stage B
  mechanics, the byte-equal reproducibility harness, and the
  drift-source enumeration (AST ordering, trivia handling, numeric
  formatting). Drift mitigation lands in the cutover design, not
  in a pre-declared partial-closure floor.
- Classifier extension for BBNF's patterns: applying AZ-I.W0's
  disposition (unified surface or locked split) to the BBNF
  grammar's regex-HIR + structural-alphabet references. Lands as
  edits to the classifier module (if unified) or as a scoped BBNF
  classifier plug-in (if locked split).
- Baseline bench capture on AZ-II branch: full 17-entry matrix
  against AZ-I close. Emits
  `docs/benchmarks/AZ-II/W0/baseline.json`.
- IR audit pass extended to cover `grammar/bbnf/bbnf.bbnf`.
  Initially red (BBNF not yet migrated); W1/W2 drive it to 100%.

Runtime call site: classifier extension runs on every `cargo
check`; baseline bench captures the AZ-II opening matrix.

### W1 — Stage A (tape-compiler builds struct-compiler candidate)

`project_types` extended to close on `grammar/bbnf/bbnf.bbnf`.
`StructRegistry` populates `BbnfAst`, `Rule`, `Expr`, `Ident`,
`Param`, `TypeExpr`, `Import`, `Directive`, `Comment`, and the
`RegexPattern` / regex-HIR variants per BBNF's grammar.

`bbnf-derive` gains a struct-writing emission mode, wired to
`StructRegistry`. The tape-writing mode remains present — Stage A
is the bridge stage where both emission targets are wired.

Stage A output: the pre-AZ-II compiler (tape-based, inherited
from AZ-I close) runs `cargo build -p bbnf_derive` with the
struct-emission mode enabled. The resulting `bbnf-derive` proc-
macro produces struct-writing parsers for BBNF grammars.

Corpus bench: run `bbnf-derive` on every `.bbnf` fixture in the
tree (`grammar/*/*.bbnf`, `tests/fixtures/*.bbnf`); collect
per-fixture parsed output to
`docs/benchmarks/AZ-II/W1/stage-a-output/`.

Runtime call site: every `cargo build` re-runs `bbnf-derive`; the
struct-emission path is exercised on every grammar file read.

Bench delta gate: BBNF self-parse throughput ≥ AU baseline minus
10% (this entry is not on the 20% wave rule because it is
internal build-time, not user-facing). No regression on JSON,
CSS L4, Sheets (they are on the AZ-I close struct path; any
regression here is a W1 fault).

### W2 — Stage B (candidate rebuilds itself + byte-equal close gate)

The W1 candidate compiler rebuilds itself. `cargo clean && cargo
build -p bbnf` executed with the W1-candidate as the bootstrap
compiler. The resulting compiler is built from a struct-writing
parser and produces struct-writing parsers.

Stage B output: the same `bbnf-derive` invocations on the same
`.bbnf` fixture corpus, captured to
`docs/benchmarks/AZ-II/W2/stage-b-output/`.

Byte-equal close gate: `diff -r` between
`docs/benchmarks/AZ-II/W1/stage-a-output/` and
`docs/benchmarks/AZ-II/W2/stage-b-output/` returns zero byte
differences. Any divergence at wave close triggers the reversal
path.

Runtime call site: the W2-final compiler is the new default.
`cargo build -p bbnf` produces a tape-free binary (the tape crate
is still present on disk but no production code path imports it).

Bench delta gate: full 17-entry matrix at or above AZ-I close.
Any regression reverts the W2 substrate.

### W3 — FINAL (tape deletion + parity recode + BA handoff)

`crates/tape/` is deleted. Its symbols (`TapeRec`, `TapeBuilder`,
`TapeCursor`, `Columns`, `Visitor`, `Finaliser`, `DTA`, `PSI`,
`Stage1`, `StructuralScan`, `Packed`, `Decoder`, `Dedup`, `Kind`,
`Driver`, `Profile`) leave the tree entirely; any non-test
reference is rewritten to consult the struct graph.

`crates/core/src/runtime/mod.rs` alias shims from AZ-I W2/W3 are
retired. The view layer for BBNF is rewritten to target struct
shapes directly; the `@debug` directive's tape-replay backend
migrates to a struct-tree walker.

Parity harnesses recoded: all four grammars' harnesses compare
struct-vs-external-native. No tape-vs-struct comparison remains.

Runtime call site: the entire production parse path routes through
struct builders. The tape crate is unreachable from any
`#[cfg(not(test))]` code. Any `#[cfg(test)]` reference migrates to
an inline fixture struct or is deleted.

`FINAL.md` records deltas, reversals taken, and hands off to BA
under BA's opening contract (struct tree present fleet-wide; BA's
pointer-path query surface consumes it).

## Reversal criteria

AZ-II inherits AZ-I's reversal discipline (wave-local 20% rule,
no-regression rule, no hedging forward, substrate-without-
activation is a re-plan trigger, reversals are a health signal)
and adds two AZ-II-specific reversal paths.

**BBNF self-parse 10% rule.** Unlike user-facing data-grammar
throughput, BBNF self-parse is an internal build-time cost. A
regression of ≤ 10% on BBNF self-parse relative to AU baseline is
acceptable and does not trigger reversal. A regression > 10%
triggers substrate reversal.

**Byte-equal failure → re-plan trigger.** If Stage B byte-equals
fails on W2 close, AZ-II does not partial-close. The wave reverts
its substrate and AZ-II re-plans from the observed drift sources.
Full tape abrogation is binding repo policy; there is no planned
"shrunken-tape" floor. If drift proves intractable under a genuine
attempt, the failure is recorded as evidence, a re-plan brief is
authored against that evidence, and the next attempt opens against
the refined cutover design rather than against a pre-declared
partial-closure state. `feedback_no-workarounds-arch` applies: a
shrunken-tape-retained-for-BBNF floor would be exactly the
two-decision-surface pathology `feedback_no-orthogonal-codepaths`
prohibits, and the discipline refuses it even under W2 pressure.

## Risk register

1. **AST ordering drift.** The derived struct may order fields
   differently than the tape's cursor traversal. Mitigation: the
   derivation follows the grammar's declaration order, which is
   stable; W0 `BOOTSTRAP-CUTOVER.md` specifies the ordering
   contract.
2. **Trivia handling drift.** The tape preserved certain trivia
   via the `Columns` side-channel; the struct may or may not
   preserve the same trivia. Mitigation: the derived struct's
   comment / trivia fields are explicit per grammar, so the
   preservation contract is visible at the type level; the W0
   research enumerates every trivia class in `grammar/bbnf/bbnf.bbnf`
   and specifies which fields preserve each.
3. **Numeric formatting drift.** f64 precision roundtrip. Mitigation:
   the struct carries the source span for every numeric leaf, so
   exact-byte recovery is available if the display form drifts.
4. **Tape deletion breaks view codegen elsewhere.** A downstream
   crate (e.g., `crates/pprint`, the `@debug` directive lowerer,
   a tooling binary) may import or re-export the live tape surface in
   a path that W3 misses. Mitigation: the W3 live-symbol scan
   (`::bbnf::runtime::tape|bbnf::runtime::tape|use tape::|\btape::|\bTape(Rec|Builder|Cursor|Offset|Kind)\b|\bColumns\b|\bFinaliser\b|\bDTA\b|\bPSI\b|Fused(Build|Output)`)
   returns zero matches before W3 closes; any hit gates the close.
5. **Cross-crate lifetime threading.** The derived `BbnfAst`
   struct's `'a` lifetime must thread through every API boundary
   that currently accepts a tape cursor. Mitigation: W1 includes
   a lifetime-threading pass over the consuming crates
   (`crates/pprint`, `crates/core/src/backend/`, tooling binaries);
   type errors surface at `cargo check` boundary, not runtime.
6. **Stage A / Stage B compilation divergence on rustc / codegen
   version drift.** If the compiler version used to build Stage A
   differs from the one used to build Stage B, the produced output
   may drift for non-semantic reasons. Mitigation: Stage A and
   Stage B use the same `rust-toolchain.toml`-pinned compiler; the
   byte-equal gate is evaluated on the compiled parser output, not
   the compiler binary.

## Critical files

AZ-II touches these subsystems. The table below is indicative;
`git log --stat` on AZ-II's branch at close produces the
authoritative list.

| Path | Role | Wave |
|---|---|---|
| `docs/tranches/AZ-II/BOOTSTRAP-CUTOVER.md` (new) | Stage A/B mechanics, drift enumeration, escape design | W0 |
| `crates/ir/src/passes/types/` | `project_types` extended to close on BBNF | W1 |
| `crates/ir/src/registry/struct.rs` | `StructRegistry` extended with BBNF entries | W1 |
| `crates/ir/src/passes/audit/payload_coverage.rs` | Audit pass extended to cover BBNF | W0 |
| `crates/bbnf_derive/src/emitter.rs` | Struct-writing emission mode alongside tape mode (W1) → struct-only (W2) | W1, W2 |
| `crates/bbnf_derive/src/loader.rs` | Grammar-file reader routes through struct path | W1 |
| `crates/core/src/runtime/bbnf/` | Runtime BBNF loader reads `BbnfAst` directly | W1 |
| `crates/core/src/runtime/mod.rs` | Alias shims retired at W3 | W3 |
| `crates/core/src/backend/driver/*` | View layer for BBNF rewritten; `@debug` lowerer migrated | W3 |
| `crates/tape/` | Deleted at W3 | W3 |
| `grammar/bbnf/bbnf.bbnf` | Source for BBNF's derived struct; may gain typed-leaf annotations | W1 |
| `tests/bbnf_*_parity.rs` | Struct-vs-hand-maintained-IR-surface parity harness | W1, W2 |
| `docs/benchmarks/AZ-II/W<n>/` | Stage A/B output archives + bench + samply | W0–W3 |

The `crates/tape/` deletion (W3) is the single largest LOC delta
in AZ-II. The current tape crate contains `lib.rs`, `builder/`,
`columns`, `cursor`, `decoders`, `dedup`, `driver`, `dta`,
`finaliser`, `kind`, `packed`, `profile`, `psi`, `stage1`,
`structural_scan`, `tape`, and `visitor` modules; every one of
those symbols leaves the tree at W3.

## Handoff contract to BA

At AZ-II close, BA opens on the following guaranteed state:

1. **All four grammars running direct-to-struct.** JSON, CSS L4,
   Sheets, BBNF — one codegen path, one materialised form.
2. **`crates/tape/` deleted.** The tape crate does not exist on
   disk. No non-test crate imports or re-exports the live tape
   surface.
3. **`StructRegistry` closed fleet-wide.** Every Named rule in
   every production grammar has a registered `StructLayout`.
4. **Parity harnesses rewired to struct comparisons.** All four
   grammars' parity harnesses compare struct-vs-external-native.
5. **Full 17-entry matrix at AU parity on struct-only path.**
6. **BBNF self-parse byte-reproducible.** Stage B = Stage A on
   the full BBNF fixture corpus; this is the permanent invariant
   encoded in a CI check on every commit post-W3.
7. **Parent-pointer decision surface open.** AZ-I's RESEARCH.md
   §3 (parent-pointer vs root-traversal) is BA.W0's opening
   question; BA measures on the fleet-wide struct tree and
   decides.

BA's opening gate is a clean read of this contract: if any of
1–6 fails at AZ-II's declared close commit, BA does not open and
AZ-II re-plans to close the gap.
