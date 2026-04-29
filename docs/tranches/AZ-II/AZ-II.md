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

## 2026-04-29 Hardening Amendment

AZ-II remains open. The implemented progress read-of-record is
[`PROGRESS-SNAPSHOT-2026-04-29.md`](PROGRESS-SNAPSHOT-2026-04-29.md):
cutover.A through cutover.M landed, cutover.N was dispatched and
halted at organizational usage limit, and no cutover.N code commits
landed. The current state is an interim manifest: direct-to-struct is live
for 9/9 grammars after O2, while `Parsed<R>`, `TapeDirect`,
generated tape-view residue, and `crates/tape` remain terminal
blockers.

The active AZ-II wave is `cutover.O`. It is not a workaround wave and
not a reduced tape floor. O0 tooling preflight, O1 grammar-general
StructDirect builder transactions, and O2 EBNF direct projection have
landed. The active resume point is O3a: close the post-O2
failure-baseline cohorts and child-wave ownership before O3 source
redress resumes. O3 then purges generated tape-view residue from
StructDirect output, O4 deletes `Parsed<R>` and `TapeDirect`, O5 deletes
the standalone tape crate, O6 refreshes semantic parity/performance
truth, and O7 converts the interim manifest to terminal close.
Each O substage is now specified as a dispatchable wave under
`waves/cutover/O0.md` through `waves/cutover/O7.md`, with up to 10
parallel sibling worktree agents and explicit file bounds.
`cutover.O3a.md` is the inserted failure-baseline and triumvirate
redress prelude before O3 implementation continues. Its child specs
(`O3a-J1`, `O3a-C1`, `O3a-S1`, `O3a-P1`, `O3a-A1`) own all 84
post-O2 failed tests and the failed JSON bench timeout.

AZ-III opens only if the EBNF blocker is proven to require new
grammar-general inference/layout machinery spanning node facts,
CSP/egraph typing, and projection emission. AZ-III must not carry
forward tape deletion, `Parsed<R>` deletion, stale benches, or parity
gaps as deferred work.

## Thesis

BBNF self-hosts on a grammar-derived struct. `project_types`
applied to `grammar/bbnf/bbnf.bbnf` produces a `BbnfAst` struct
graph whose shape mirrors the compiler's in-memory IR surface.
The xtask regen pipeline's embedded parser (orchestrated by
`xtask/src/regen.rs`, emitted via `crates/core/src/backend/rust/
emitter/`) — the piece that reads a grammar file at IR-pipeline
time — stops using the tape cursor and reads from the derived
struct instead.

The cutover is byte-equal reproducibility across two stages, both
landed atomically inside W1:

1. **Stage A.** The pre-AZ-II compiler (tape-based, inherited from
   AZ-I close) builds the AZ-II-candidate compiler (struct-based).
   The candidate's xtask regen pipeline emits struct-writing
   parsers; the candidate itself was built from a tape-writing
   parser.
2. **Stage B.** The AZ-II-candidate compiler rebuilds itself from
   its own source. The final compiler is built from a struct-
   writing parser and produces struct-writing parsers. The tape
   is unwired in both directions.
3. **Close gate.** Stage B output is byte-equal to Stage A output
   on the full BBNF grammar corpus (every `.bbnf` fixture in the
   tree, including `grammar/bbnf/bbnf.bbnf` itself). Any divergence
   reverts the W1 substrate and invokes the escape clause.

Stage A and Stage B are folded into one wave because the
intermediate state — Stage A produced but Stage B not yet
verified — leaves the tree with two emission targets wired side by
side and no proof that either is reproducible. Splitting the
cutover across wave boundaries would force master to carry that
unworkability between waves; merging them makes the unworkability
window honest and bounded to the wave's own internal stages.

Once W1 is green, `crates/tape/` has no remaining consumers. AZ-II
W2 (FINAL) deletes the crate and recodes every downstream
reference.

## Dependency on AZ-I

AZ-II opens on AZ-I's seven-point handoff contract (AZ-I.md
§Handoff contract to AZ-II):

1. JSON, CSS L4, Sheets direct-to-struct on a single codegen path.
2. `StructRegistry` closed on those three.
3. `crates/tape/` compilable on disk.
4. `grammar/bbnf/bbnf.bbnf` unchanged; `cargo xtask regen`
   produces tape-writing parsers; BBNF bootstrap test suite green.
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

- Stage A output (pre-AZ-II compiler builds candidate) and Stage B
  output (candidate rebuilds itself) are both captured at W1 close.
- `diff -r <stage-A-output> <stage-B-output>` returns zero byte
  differences across the entire BBNF fixture corpus.

**Tape-deleted gates:**

- `rg '^crates/tape/'` on the tree at AZ-II close returns zero
  matches.
- The live tape-symbol scan
  (`::bbnf::runtime::tape|bbnf::runtime::tape|use tape::|\btape::|\bTape(Rec|Builder|Cursor|Offset|Kind)\b|\bColumns\b|\bFinaliser\b|\bDTA\b|\bPSI\b|Fused(Build|Output)`)
  returns zero matches outside historical docs.
- `rg '\bTapeRec\b|\bTapeBuilder\b|\bTapeCursor\b|\bColumns\b|\bFinaliser\b|\bDTA\b|\bPSI\b' crates/ --type rust`
  returns zero matches.
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

### cutover.O absorbs durable AY-III gates

AY-III.W2's BBNF self-host identity gate folds into AZ-II cutover.O as
the post-cutover regen check. `cargo xtask regen --check` exit 0 across
the full 9-grammar corpus remains the byte-equal close ceremony. The
grammar-general totality test (parameterized per primary grammar)
verifies admission ≡ materialiser ≡ consumer for the post-tape
struct-only path; this generalises AY-III.W2's BBNF-only assertion to
the primary grammars on the struct substrate.

## Wave structure

**Wave plan refined 2026-04-28** per audit synthesis at
`docs/tranches/AZ-I/audit/W2-CLOSE-AUDIT.md` §9. The original W0 / W1 /
W2 three-wave plan collapses into a single **AZ-II.cutover** wave —
the W2-act activation pattern is reusable for BBNF without further
substrate work; Stage A / Stage B is two regen invocations; tape
deletion is mechanical post-byte-equal-green. The W0 / W1 / W2 wave
docs at `waves/{W0,W1,W2}.md` carry supersede notices and remain on
disk as historical record.

| Wave | Headline | Opens after | Status |
|---|---|---|---|
| **cutover** | BBNF self-host + tape deletion ([waves/cutover/README.md](waves/cutover/README.md)) — original three-stage plan expanded under contact into 14 sub-stages cutover.A through cutover.N, then O0-O7 plus O3a child specs | AZ-I.W2-act close (AZ-I FINAL.md committed; seven-point handoff verified) | interim manifest (cutover.A→M LANDED; cutover.N halted at usage limit; [O0](waves/cutover/O0.md)/[O1](waves/cutover/O1.md)/[O2](waves/cutover/O2.md) LANDED; terminal hardening active at [O3a failure-baseline triage](waves/cutover/O3a.md); [O3 generated view purge](waves/cutover/O3.md) blocked until O3a close) |

The cutover wave's actual trajectory across 14 sub-stages is recorded
in `docs/tranches/AZ-II/PROGRESS-SNAPSHOT-2026-04-29.md` (per-substage
commit SHAs, hard-gate readout, BA handoff verification, agent dispatch
history). Per-substage scope-reveal reports archived under
`docs/tranches/AZ-II/audit/cutover.{C,E,F,G,I}-PARTIAL.md`.
Historical agency specs for the substages live beside the active O
specs under `waves/cutover/`: `A.md` through `O.md`, plus `O0.md`
through `O7.md`, inserted `O3a.md`, and the deployable O3a child specs
`O3a-J1.md`, `O3a-C1.md`, `O3a-S1.md`, `O3a-P1.md`, and
`O3a-A1.md`.
cutover.J is a halted no-code record; its mapped-factor diagnosis
resumed and landed in K rather than under a retroactive J label.

| Sub-stage | Headline | Status |
|---|---|---|
| cutover.A | BBNF substrate + resolver-arm + decay sweep | LANDED |
| cutover.B | Stage A/B byte-equal + permanent CI gate | LANDED |
| cutover.C | scope-reveal — 700 BbnfBootstrapNodeView refs surface | DIAGNOSTIC |
| cutover.D | 4 parallel agents migrate BBNF consumers | LANDED |
| cutover.E | non-BBNF substrates authored; Discovery 1 emitter regression surfaces | SUBSTRATE LANDED |
| cutover.F | 3 emitter bug classes diagnosed + fixed | LANDED |
| cutover.G | hand-crafted bootstrap_parser breaks chicken-and-egg | LANDED |
| cutover.H | BBNF resolver-arm re-flip + transparent emitter fix + PARTIAL FINAL.md | LANDED |
| cutover.I.5 | `BbnfBootstrap::serialize_compact_doc` + bbnf_rule un-ignore | LANDED |
| cutover.J | halted blocker-fix attempt; zero code commits; mapped-factor diagnosis routes to K | complete_with_misses |
| cutover.K | mapped_factor wrapper + typed-leaf source recovery + per-shape Err frame cleanup | LANDED |
| cutover.L | keyword-shape Alt-of-Ref handler | LANDED |
| cutover.M | non-BBNF resolver arms (CSV/Math/BNF/CSS Pretty); AltDispatch struct_direct surgery | LANDED |
| cutover.N | EBNF activation + Phases 4/5/6 close | dispatched + halted at usage limit; routed to cutover.O; O0/O1/O2 now landed, O3a is active, and O3 generated view purge is blocked until O3a close |

| O spec | Headline | Status |
|---|---|---|
| [cutover.O0](waves/cutover/O0.md) | Tooling preflight | LANDED |
| [cutover.O1](waves/cutover/O1.md) | StructDirect builder transactions | LANDED |
| [cutover.O2](waves/cutover/O2.md) | EBNF direct projection | LANDED |
| [cutover.O3a](waves/cutover/O3a.md) | Failure baseline + triumvirate redress | in_progress |
| [cutover.O3a-J1](waves/cutover/O3a-J1.md) | JSON materialization/parity/throughput cohort | planned |
| [cutover.O3a-C1](waves/cutover/O3a-C1.md) | CSS admission/payload/parity cohort | planned |
| [cutover.O3a-S1](waves/cutover/O3a-S1.md) | Sheets payload/serialization cohort | planned |
| [cutover.O3a-P1](waves/cutover/O3a-P1.md) | Projection totality/generated-view cohort | planned |
| [cutover.O3a-A1](waves/cutover/O3a-A1.md) | Analysis/LSP/prototype/bootstrap disposition cohort | planned |
| [cutover.O3](waves/cutover/O3.md) | Generated view purge | blocked |
| [cutover.O4](waves/cutover/O4.md) | `Parsed<R>` / `TapeDirect` deletion | planned |
| [cutover.O5](waves/cutover/O5.md) | `crates/tape` deletion | planned |
| [cutover.O6](waves/cutover/O6.md) | Semantic/performance close | planned |
| [cutover.O7](waves/cutover/O7.md) | FINAL conversion | planned |

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
  Initially red (BBNF not yet migrated); W1 drives it to 100%.

Runtime call site: classifier extension runs on every `cargo
check`; baseline bench captures the AZ-II opening matrix.

### W1 — Stage A + Stage B atomic byte-equal cutover

W1 lands the cutover end-to-end. `project_types` extends to close
on `grammar/bbnf/bbnf.bbnf`; `StructRegistry` populates `BbnfAst`,
`Rule`, `Expr`, `Ident`, `Param`, `TypeExpr`, `Import`,
`Directive`, `Comment`, and the `RegexPattern` / regex-HIR
variants per BBNF's grammar; the xtask regen pipeline
(`xtask/src/regen.rs` orchestration + the rust emitter under
`crates/core/src/backend/rust/emitter/`) gains a struct-writing
emission mode wired to `StructRegistry`.

**Stage A.** The pre-AZ-II compiler (tape-based, inherited from
AZ-I close) runs `cargo xtask regen --emit-mode struct` with the
struct-emission mode enabled. The tape-writing mode remains
present in this stage — Stage A is the bridge state where both
emission targets are wired. The resulting per-grammar output
under `crates/core/src/grammar/generated/<ident>.rs` holds
struct-writing parsers for BBNF grammars. Corpus output is
captured to `docs/benchmarks/AZ-II/W1/stage-a-output/` for every
`.bbnf` fixture in the tree (`grammar/*/*.bbnf`,
`tests/fixtures/*.bbnf`, `grammar/bbnf/bbnf.bbnf` itself).

**Stage B.** The Stage A candidate compiler rebuilds itself.
`cargo clean && cargo build -p bbnf` runs with the candidate as
the bootstrap toolchain; the resulting xtask regen pipeline is
itself built from a struct-writing parser and produces
struct-writing parsers. Tape is unwired in both directions for
BBNF's bootstrap. The same `cargo xtask regen --emit-mode struct`
invocations on the same fixture corpus are captured to
`docs/benchmarks/AZ-II/W1/stage-b-output/`.

**Byte-equal close gate.** `diff -r
docs/benchmarks/AZ-II/W1/stage-a-output/
docs/benchmarks/AZ-II/W1/stage-b-output/` returns zero byte
differences across the entire BBNF fixture corpus. Any divergence
at wave close triggers the reversal path; the W1 substrate
reverts and AZ-II re-plans against the observed drift.

The two stages must land in the same wave because Stage A's
intermediate state — both emission targets wired, no
reproducibility proof — is unworkable as a master-green
checkpoint. Merging the stages into one wave keeps the
unworkability bounded to the wave's interior, where the close
gate is the sole arbiter.

Runtime call site: `cargo xtask regen` is invoked at IR-pipeline
time (CI + pre-commit gate via `cargo xtask regen --check`); the
struct-emission path is exercised on every grammar file read.
Post-W1, the W1-final compiler is the new default. `cargo build
-p bbnf` produces a tape-free binary (the tape crate is still
present on disk but no production code path imports it).

`tests/bbnf_bootstrap_reproducibility.rs` lands at W1 close as a
permanent CI gate: it encodes the Stage A / Stage B diff as a
repeatable test that runs on every commit post-W1.

Bench delta gate: BBNF self-parse throughput ≥ AU baseline minus
10% (this entry is not on the 20% wave rule because it is
internal build-time, not user-facing); full 17-entry matrix at or
above AZ-I close on JSON, CSS L4, Sheets (any regression here is
a W1 fault).

### W2 — FINAL (tape deletion + parity recode + BA handoff)

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
| `crates/core/src/backend/rust/emitter/**` | Struct-writing emission mode alongside tape mode (W1) → struct-only (W2) | W1, W2 |
| `xtask/src/regen.rs` | Grammar-file orchestration routes through struct path | W1 |
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
