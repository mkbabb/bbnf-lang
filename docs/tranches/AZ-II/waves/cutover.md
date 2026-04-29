# AZ-II.cutover — BBNF self-host cutover + `crates/tape/` deletion

**Opens after**: AZ-I.W2-act close (AZ-I FINAL.md committed; AZ-II
seven-point handoff contract verified).
**Agents (planned)**: 3 sequential. **Agents (actual)**: 14 sequential
sub-stages (cutover.A through cutover.N) over multiple sessions; 13
LANDED at master, cutover.N halted at organizational usage limit.
**Hard gate**: BBNF self-parses through a `project_types`-derived
`BbnfDocument` struct graph; Stage A / Stage B byte-equal across the
full BBNF fixture corpus; `crates/tape/` deleted; `cargo build -p bbnf
--no-default-features` succeeds without `crates/tape/`; AZ-II FINAL.md
+ `docs/benchmarks/post-AZ-II.json` archived.
**Status**: PARTIAL CLOSE — substrate canonical; 8/9 grammars
StructDirect; BBNF self-parity 56/56; reproducibility CI gate green;
cutover.O.0 tooling preflight and O1 builder transactions landed;
remaining work resumes at O2 EBNF activation before tape deletion,
bench refresh, and FINAL CLOSE conversion.

**2026-04-29 hardening amendment**: cutover.O must begin with the
grammar-general StructDirect builder transaction gap. EBNF activation
is blocked not only by alternate layout depth but also by the fact that
speculative StructDirect branches can mutate builder state without a
matching rollback.

**2026-04-29 O1 addendum**: O0 and O1 are landed. The active resume
point is O2 EBNF direct projection, now with grammar-general
StructDirect checkpoint/rollback/commit support available to every
speculative emitter path.

**Trajectory snapshot**: see [`../PROGRESS-SNAPSHOT-2026-04-29.md`](../PROGRESS-SNAPSHOT-2026-04-29.md)
for per-substage commit-by-commit detail across cutover.A through
cutover.N, agent dispatch history with caps + outcomes, hard-gate
readout, BA handoff verification, and trajectory progress estimate.

**Per-substage scope-reveal reports**: archived under
[`../audit/cutover.{C,E,F,G,I}-PARTIAL.md`](../audit/) — the cutover
wave's scope expanded under contact from 3 sub-stages to 14, with
each scope-reveal report documenting the discovered work, blocker
diagnosis, and follow-on substage routing.

AZ-II.cutover collapses the original AZ-II.W0 / W1 / W2 three-wave
plan into one cutover wave per `docs/tranches/AZ-I/audit/W2-CLOSE-AUDIT.md`
§9. The W2 substrate established the per-shape struct-direct pattern;
AZ-II reuses it for BBNF unchanged. The actual 14-substage trajectory
documents the contact-discovered work below the original 3-stage
scope.

## Scope

1. Hoist `tape::dta` (4 types: `DtaStateId`, `DtaRuleId`,
   `DtaAssociativity`, `DtaPrecedenceEntry`) from `crates/tape/src/dta.rs`
   to `crates/ir/src/dta/mod.rs` per `audit/AUDIT-6-ARCHITECTURE.md`
   §8.2 (the IR layer is the natural owner; tape consumes them as
   `bbnf_ir::dta::*`). Inverts the workaround crate-edge documented at
   `tape/src/dta.rs:39-43`.
2. Delete the `crates/tape/src/visitor.rs` family (7 traits +
   ~746 LOC) per `audit/AUDIT-6-ARCHITECTURE.md` §8.3 — struct-direct
   grammars never invoke them; per-grammar concrete builders inline
   their dispatch.
3. Delete tape driver dead helpers (`emit_leaf`,
   `emit_reducer_compound`, `lookup_precedence`, `trim_ascii_ws`,
   `trim_with_pattern`, `first_ws_pattern`, `saturating_u16`,
   `emit_leaf_with_payload`, `close_compound`) per
   `audit/AUDIT-3-DECAY-INVENTORY.md` §6 — zero non-doc consumers
   verified by grep across the workspace.
4. Author typed-leaf annotations on `grammar/bbnf/bbnf.bbnf` — every
   `Named` rule (`bbnf_ast`, `rule`, `expr`, `ident`, `param`,
   `type_expr`, `import`, `directive`, `comment`,
   `regex_pattern`, regex-HIR variants) gets `-> T` annotations or
   inferred layouts so `populate_struct_registry` closes BBNF the
   same way it closes JSON / CSS L4 / Sheets.
5. Author `crates/core/src/runtime/bbnf/` runtime: `BbnfValue<'p>`
   sum (matches the existing `bbnf::ast` shape but lifetime-borrowed
   instead of owned), `BbnfArena<'p>`, `BbnfStructBuilder<'p>`,
   `BbnfDocument<'p>` accessors mirroring JSON / Sheets / CSS L4.
6. Resolver-arm extension: `EmitStrategy::for_grammar("BbnfBootstrap"
   | "BbnfParser", true) => StructDirect { rust: SubstrateBinding {
   builder_path: "::bbnf::runtime::bbnf::BbnfStructBuilder",
   document_path: "::bbnf::runtime::bbnf::BbnfDocument" }, … }`.
7. Stage A: regen BBNF's per-grammar source via `cargo xtask regen
   --grammar bbnf` against the post-W2-act compiler. Stage A produces
   a struct-writing BBNF parser; capture
   `crates/core/src/grammar/generated/bbnf.rs` to
   `docs/benchmarks/AZ-II/cutover/stage-a-bbnf.rs`.
8. Stage B: rebuild from candidate source. Run `cargo clean -p bbnf
   && cargo build -p bbnf --profile ax-iter` against the candidate;
   re-run `cargo xtask regen --grammar bbnf` from the candidate
   bootstrap; capture stage-b-bbnf.rs. Byte-equal diff against
   stage-a-bbnf.rs across the full BBNF fixture corpus
   (`grammar/*/*.bbnf`, `tests/fixtures/*.bbnf`,
   `grammar/bbnf/bbnf.bbnf` itself).
9. `crates/tape/` directory deletion. Cross-crate severance:
   `crates/core/Cargo.toml` removes `tape =` dep; the `pub use
   simd_scan as scan` alias retires; the
   `crates/core/src/runtime/mod.rs:56` `pub use ::bbnf::runtime::tape`
   retires; any `crates/pprint/` / `crates/gorgeous/` /
   `crates/analysis/` / `crates/lsp/` reference rewires to the struct
   runtime or deletes outright.
10. View / pprint / @debug recode: `crates/gorgeous/`'s @debug
    directive lowerer migrates from tape-replay to a struct-tree
    walker. The `bbnf::runtime::path::Path<'_>` query infrastructure
    targets struct fields directly.
11. Permanent CI gate at `crates/core/tests/bbnf_bootstrap_reproducibility.rs`
    encodes the Stage A / Stage B diff as a repeatable test.
12. Decay deletions absorbed (per `audit/W2-CLOSE-AUDIT.md` §4):
    `crates/ir/src/passes/recognizers/dta.rs` ~900 LOC amputation
    (retire `DtaState`, `DtaTable`, `DtaBuilder`, `DtaProfile`,
    `summarise`; expose `collect_pattern_set(ir) → Vec<PatternRef>` for
    DfaCodegen); IR `dta.rs` sentinel hooks (3 sentinel fns) deletion;
    `pattern_alphabet.rs::bitmaps_disjoint` deletion;
    `tape/src/psi/stream.rs` rayon cfg-gate flatten;
    `pub use crate::backend::rust as codegen` alias retire.
13. Close ceremony: AZ-II FINAL.md, `docs/benchmarks/post-AZ-II.json`
    archive, full 17-entry matrix on the post-tape struct-only path.

## File bounds

| File | Access | Owner |
|---|---|---|
| `crates/ir/src/dta/mod.rs` | create | cutover.A |
| `crates/ir/src/dta/types.rs` | create | cutover.A |
| `crates/ir/src/lib.rs` | modify | cutover.A |
| `crates/tape/src/dta.rs` | delete | cutover.A |
| `crates/tape/src/lib.rs` | modify-carve | cutover.A (re-export retirement) → delete (cutover.C) |
| `crates/tape/src/visitor.rs` | delete | cutover.A |
| `crates/tape/src/driver.rs` | modify-carve | cutover.A (delete dead helpers) |
| `crates/tape/Cargo.toml` | modify | cutover.A |
| `grammar/bbnf/bbnf.bbnf` | modify | cutover.A |
| `crates/core/src/runtime/bbnf/value.rs` | create | cutover.A |
| `crates/core/src/runtime/bbnf/arena.rs` | create | cutover.A |
| `crates/core/src/runtime/bbnf/builder.rs` | create | cutover.A |
| `crates/core/src/runtime/bbnf/document.rs` | create | cutover.A |
| `crates/core/src/runtime/bbnf/mod.rs` | create | cutover.A |
| `crates/core/src/runtime/mod.rs` | modify | cutover.A + cutover.C |
| `crates/ir/src/registry/strategy.rs` | modify | cutover.A |
| `crates/ir/src/passes/audit/payload_coverage.rs` | modify | cutover.A (extend coverage to BBNF) |
| `crates/ir/src/passes/types/mod.rs` | modify | cutover.A |
| `crates/ir/src/passes/recognizers/dta.rs` | modify-carve | cutover.A |
| `crates/ir/src/passes/recognizers/pattern_alphabet.rs` | modify-carve | cutover.A |
| `crates/core/src/grammar/generated/bbnf.rs` | regen (Stage A → Stage B) | cutover.B |
| `docs/benchmarks/AZ-II/cutover/stage-{a,b}-bbnf.rs` | create | cutover.B |
| `crates/core/tests/bbnf_bootstrap_reproducibility.rs` | create | cutover.B |
| `crates/tape/` | delete | cutover.C |
| `crates/core/Cargo.toml` | modify | cutover.C |
| `Cargo.toml` (workspace) | modify | cutover.C |
| `crates/gorgeous/src/**` | modify | cutover.C |
| `crates/lsp/src/**` | modify | cutover.C |
| `crates/analysis/src/**` | modify | cutover.C |
| `crates/core/src/backend/rust/emitter/grammar.rs` | modify | cutover.C |
| `crates/simd-scan/src/**` | modify-carve | cutover.C (retire tape dep if any) |
| `crates/core/tests/bbnf_*_parity.rs` | modify | cutover.C |
| `crates/core/tests/bbnf_self_parity.rs` | modify | cutover.C |
| `crates/core/tests/bbnf_ast_parity.rs` | modify | cutover.C |
| `docs/benchmarks/post-AZ-II.json` | create | cutover.C |
| `docs/benchmarks/profiles/AZ-II/cutover/**` | create | cutover.C |
| `docs/tranches/AZ-II/FINAL.md` | create | cutover.C |
| `docs/tranches/AZ-II/PROGRESS.md` | modify | each agent |

**Do NOT touch**: AZ-I-completed grammars (JSON / Sheets / CSS L4
runtime stays as W2-act landed it); pprint / parse-that source
(no AZ-II edits — pprint may need to drop a tape dep but the work is
in `crates/core/src/runtime/mod.rs`'s re-export retirement, not in
sibling repo source); BB scaffold (BB.W0 owns its own files).

## Phase sub-items

### AZ-II.cutover.O — Terminal hardening

Sequential with fan-out only where file ownership is disjoint. This is
the terminal AZ-II wave, not an AZ-III deferral surface.

Required order:

1. **O0 tooling preflight** — LANDED: repair or explicitly de-canonicalize stale
   bench/profiling/IAI command surfaces before they are used as close
   evidence.
2. **O1 transactional builder ABI** — LANDED: grammar-general
   checkpoint/rollback/commit support exists on `StructBuilder` and is
   wired through every speculative StructDirect emitter path.
3. **O2 EBNF direct projection** — model large literal alternates
   through shared layout/type facts and require `EbnfParser::parse ->
   EbnfDocument`.
4. **O3 generated view purge** — remove tape-backed `TapeCursor`,
   node-view, and `ValueRoot` residue from StructDirect generated output
   unless it is consumed through a document API.
5. **O4 Parsed/TapeDirect deletion** — delete `Parsed<R>` as a
   production parser result and remove `TapeDirect` fallback semantics.
6. **O5 tape crate deletion** — delete `crates/tape` after relocating
   only genuinely non-tape scan/index primitives to their natural owner.
7. **O6 semantic/perf close** — refresh JSON `sonic-rs` parity, CSS
   `lightningcss` typed parity, and the 17-entry close matrix.
8. **O7 final conversion** — convert AZ-II FINAL from PARTIAL CLOSE to
   terminal close after the gates above pass.

Hard gates:

- `EbnfParser::parse` returns `EbnfDocument`.
- `rg '\bParsed\b|\bTapeDirect\b|\bTapeCursor\b|\bTapeRec\b|\bTapeOffset\b|runtime::tape|crates/tape' crates/ --type rust`
  has no production hits except deliberately relocated non-tape
  primitives.
- `cargo xtask regen --check` passes.
- `cargo nextest run -p bbnf --test bbnf_bootstrap_reproducibility --profile ax-iter`
  passes.
- JSON and CSS parity tests pass against `sonic-rs` and `lightningcss`
  respectively on the post-tape path.
- `docs/benchmarks/post-AZ-II.json` is regenerated with no placeholder
  entries.

### AZ-II.cutover.A — Substrate hoist + BBNF runtime + resolver-arm

Sequential. Cap **120 min** (the largest single dispatch — substrate
moves + new grammar runtime + decay deletions).

Mechanism:
- Hoist `tape::dta` 4 types to `bbnf_ir::dta::types::{DtaStateId,
  DtaRuleId, DtaAssociativity, DtaPrecedenceEntry}`. `bbnf-ir`
  already has no edge to `tape`; tape gains an edge to `bbnf-ir`
  (which it has via dev-deps already; promote to lib dep). Update
  re-exports across crates that consume the types via
  `bbnf::runtime::tape::DtaPrecedenceEntry` to read from the new IR
  location.
- Delete `crates/tape/src/visitor.rs` (746 LOC), the 7 trait family
  + 12 re-exports at `crates/tape/src/lib.rs:72-103`. Generated code
  consumers (every per-grammar generated parser that takes
  `V: ObjectVisitor + …` bounds) collapse to direct-call shapes;
  W2-act establishes the pattern for struct-direct grammars,
  cutover.A applies it to the few remaining tape-direct visitor
  consumers (BBNF) by emitting concrete dispatch in place of the
  generic.
- Delete tape driver dead helpers: 9 `pub fn` items at
  `crates/tape/src/driver.rs:79-281` (`trim_ascii_ws`,
  `trim_with_pattern`, `first_ws_pattern`, `saturating_u16`,
  `emit_leaf`, `emit_leaf_with_payload`, `emit_reducer_compound`,
  `lookup_precedence`, `close_compound`). Verified zero non-doc
  consumers by `audit/AUDIT-3-DECAY-INVENTORY.md` §6.
- Author `grammar/bbnf/bbnf.bbnf` typed-leaf annotations: every
  `Named` rule receives `-> T` markers so `populate_struct_registry`
  produces a `StructLayout` for BBNF the same way it does for the
  data grammars.
- Author `crates/core/src/runtime/bbnf/` runtime: same shape as
  `crates/core/src/runtime/json/` (value.rs / arena.rs / builder.rs /
  document.rs).
- Extend the resolver in `crates/ir/src/registry/strategy.rs::EmitStrategy::for_grammar`
  with the `BbnfBootstrap` / `BbnfParser` arm.
- Decay sweep: `crates/ir/src/passes/recognizers/dta.rs` amputation
  per `audit/AUDIT-3` §1 + `audit/AUDIT-6` §8.4 — retain only
  `collect_pattern_set(ir) → Vec<PatternRef>` for DfaCodegen
  consumption; delete `DtaState` (167-311), `DtaTable` (376-435),
  `DtaBuilder`, `DtaProfile` (1592-1612), `summarise` (1557-1591),
  3 sentinel hooks (1618-1626). ~900 LOC reclaim.

Sub-gate (cutover.A close):
1. `bbnf_ir::dta::*` types resolve; `crates/tape/src/dta.rs` does
   not exist. Verification: `rg 'pub.*struct DtaPrecedenceEntry'
   crates/` returns one hit at `crates/ir/src/dta/types.rs`.
2. `crates/tape/src/visitor.rs` does not exist; the trait family is
   un-imported. Verification: `rg 'GrammarVisitor|ObjectVisitor|ArrayVisitor|StringVisitor|NumberVisitor|KeywordVisitor|PrattVisitor'
   crates/tape/` returns zero matches.
3. Tape driver dead-helper items absent. Verification: `rg
   '\bpub fn (trim_ascii_ws|trim_with_pattern|first_ws_pattern|saturating_u16|emit_leaf|emit_reducer_compound|lookup_precedence|close_compound)\b'
   crates/tape/` returns zero matches.
4. `populate_struct_registry` returns ≥ 1 layout for `BbnfBootstrap`
   on `cargo nextest run -p bbnf-ir --test struct_registry --profile
   ax-iter` (extend the test).
5. `crates/core/src/runtime/bbnf/` exists; `BbnfStructBuilder`
   implements `StructBuilder`.
6. `cargo nextest run --workspace --profile ax-iter --no-fail-fast`
   does not regress. (Workspace test count may DROP if `crates/tape/`
   tests retire alongside the visitor surface; that's acceptable.)
7. `crates/ir/src/passes/recognizers/dta.rs` ≤ ~720 LOC (≥ 900 LOC
   amputated from the 1625-LOC original).

### AZ-II.cutover.B — Stage A + Stage B byte-equal cycle

Sequential after A. Cap **60 min**.

Mechanism:
- Stage A capture:
  ```bash
  cargo xtask regen --grammar bbnf
  cp crates/core/src/grammar/generated/bbnf.rs \
     docs/benchmarks/AZ-II/cutover/stage-a-bbnf.rs
  ```
  The xtask runs against the post-W2-act compiler, which carries the
  resolver-arm extension landed in cutover.A. The candidate
  generated bbnf.rs is a struct-writing parser (no
  `TapeBuilder` / `TapeCursor` references in the body).
- Stage B regen:
  ```bash
  cargo clean -p bbnf
  cargo build -p bbnf --profile ax-iter
  cargo xtask regen --grammar bbnf
  cp crates/core/src/grammar/generated/bbnf.rs \
     docs/benchmarks/AZ-II/cutover/stage-b-bbnf.rs
  ```
  The compiler now bootstraps from a struct-writing BBNF parser;
  re-running regen produces stage-b output. The two outputs MUST
  byte-match.
- Byte-equal verification:
  ```bash
  diff -u docs/benchmarks/AZ-II/cutover/stage-{a,b}-bbnf.rs
  ```
  Returns zero diff.
- Permanent CI gate at `crates/core/tests/bbnf_bootstrap_reproducibility.rs`
  encodes the Stage A / Stage B diff as a repeatable test that runs
  on every commit post-cutover.B.

Sub-gate (cutover.B close):
1. `docs/benchmarks/AZ-II/cutover/stage-a-bbnf.rs` and `stage-b-bbnf.rs`
   exist; `diff -u` returns empty.
2. `cargo nextest run -p bbnf --test bbnf_bootstrap_reproducibility
   --profile ax-iter` 1/1 green.
3. `cargo xtask regen --check` returns zero (idempotent regen).
4. The post-cutover.B `crates/core/src/grammar/generated/bbnf.rs`
   contains zero `TapeBuilder` / `TapeCursor` / `TapeRec` /
   `push_rec` references. Verification: `rg
   'TapeBuilder|TapeCursor|TapeRec|push_rec' crates/core/src/grammar/generated/bbnf.rs`
   returns zero matches.

### AZ-II.cutover.C — `crates/tape/` deletion + recode + FINAL

Sequential after B. Cap **120 min**.

Mechanism:
- Delete `crates/tape/` directory wholesale. Update
  `Cargo.toml`'s `[workspace.members]` to remove `crates/tape`.
  Update `.cargo/config.toml`'s `[patch.crates-io]` if `tape =`
  appears; remove.
- Cross-crate severance: `crates/core/Cargo.toml` removes `tape`
  dep; `crates/simd-scan/Cargo.toml` removes if present; check every
  workspace member.
- Retire `crates/core/src/runtime/mod.rs` re-exports of `tape`
  (line 56 `pub use ::bbnf::runtime::tape` and any sibling). The
  `pub use simd_scan as scan` alias may also retire if unreferenced
  post-cutover.
- Recode view / pprint / @debug:
  - `crates/gorgeous/src/`: the @debug directive lowerer was
    tape-replay-shaped; replace with a struct-tree walker against
    `BbnfDocument` (or whichever grammar's runtime carries the
    debug body).
  - `crates/lsp/src/`: any tape-shaped code paths (semantic-token
    extraction, hover) re-target struct fields.
  - `crates/analysis/src/`: same.
- Recode parity harnesses:
  - `crates/core/tests/bbnf_*_parity.rs` (bbnf_self_parity,
    bbnf_ast_parity, bbnf_parity) — compare struct-vs-existing-
    `bbnf::ast` reference output, not tape-vs-anything.
  - Any remaining `tape_to_value` test helpers (e.g.
    `json_value_parity.rs:259-278`) retire — the struct-direct path
    has no tape to read from.
- Run the full 17-entry matrix on the post-tape struct-only path.
  Cold per-parse, sequential. Archive bench JSON at
  `docs/benchmarks/post-AZ-II.json`.
- Capture samply fleet under
  `docs/benchmarks/profiles/AZ-II/cutover/`.
- Author `docs/tranches/AZ-II/FINAL.md` per
  `docs/instructions/README.md` §Tranche completion. Cap 350 LOC.
- Workspace nextest: `cargo nextest run --workspace --profile
  ax-iter --no-fail-fast` returns 0 failures.
- Update `docs/tranches/AZ-II/PROGRESS.md` close entry with master
  HEAD.
- Verify BA handoff contract from AZ-II.md §Handoff contract to BA
  (seven points): all four grammars on direct-to-struct;
  `crates/tape/` deleted; `StructRegistry` closed fleet-wide;
  parity harnesses on struct comparisons; 17-entry matrix at AU
  parity; BBNF self-parse byte-reproducible; parent-pointer
  decision surface open for BA.W0.

Sub-gate (cutover.C close, AZ-II-final):
1. `crates/tape/` does not exist on disk. Verification: `find
   crates/tape -type d 2>/dev/null` returns empty.
2. `cargo build -p bbnf --no-default-features` succeeds.
3. Live tape-symbol scan
   (`::bbnf::runtime::tape|bbnf::runtime::tape|use tape::|\btape::|\bTape(Rec|Builder|Cursor|Offset|Kind)\b|\bColumns\b|\bFinaliser\b|\bDTA\b|\bPSI\b|Fused(Build|Output)`)
   returns zero matches in `crates/` outside historical docs and
   the moved-to-IR `Dta*` types.
4. `docs/benchmarks/post-AZ-II.json` exists and covers the close
   matrix.
5. AZ-II FINAL.md committed; AZ-II PROGRESS.md close entry committed
   with master HEAD.
6. `cargo nextest run --workspace --profile ax-iter --no-fail-fast`
   returns 0 failures.
7. 17-entry matrix at AZ-I close baseline on every entry; BBNF
   self-parse within ±10% of AU baseline.

## Hard gate

1. `crates/tape/` deleted; `cargo build -p bbnf --no-default-features`
   green.
2. Stage A / Stage B byte-equal across the BBNF fixture corpus;
   `bbnf_bootstrap_reproducibility.rs` permanent CI gate green.
3. IR audit pass reports 100% `->` coverage fleet-wide (JSON, CSS L4,
   Sheets, BBNF).
4. `StructRegistry` non-empty for every Named rule in the four
   grammars including BBNF.
5. Parity harnesses recoded to struct-vs-external on all four
   grammars; no tape-vs-struct comparison.
6. 17-entry matrix at AU floor on every entry; BBNF self-parse within
   ±10% of AU baseline.
7. AZ-II FINAL.md + `docs/benchmarks/post-AZ-II.json` exist on master.
8. Decay sweep: `crates/ir/src/passes/recognizers/dta.rs` ≤ ~720 LOC;
   `tape::dta` and `tape::visitor` surfaces gone; tape driver dead
   helpers gone; `crates/json-prototype/` retired (if not already
   in W2-act); pattern_alphabet decay items gone.

## Verification artefacts

- `docs/benchmarks/post-AZ-II.json` — 17-entry close matrix.
- `docs/benchmarks/profiles/AZ-II/cutover/{json,css_l4,google_sheets,bbnf,compile_pipeline}/`
  — samply captures.
- `docs/benchmarks/AZ-II/cutover/stage-{a,b}-bbnf.rs` — byte-equal
  proof artefacts.
- `crates/core/tests/bbnf_bootstrap_reproducibility.rs` — permanent
  CI gate.
- `docs/tranches/AZ-II/FINAL.md` — close ceremony.
- `docs/tranches/AZ-II/PROGRESS.md` — close entry + master HEAD.
- Commit hashes for each milestone in PROGRESS.md.

## Dependencies

- **Depends on**: AZ-I.W2-act close (master HEAD post-W2-act-C);
  AZ-I FINAL.md committed; the seven-point AZ-II opening contract
  verified.
- **Blocks**: BA.W0 (path IR over the closed struct tree fleet-wide).

## Reversal posture

Per `audit/W2-CLOSE-AUDIT.md` §10:
- Byte-equal miss reverts the tape deletion + re-plans through
  cutover.A. The dta hoist + visitor deletion + driver dead-helper
  deletion stay (independent retirements; the tape crate may shrink
  to a smaller surface that survives until drift sources resolve).
- Per AZ-II.md §Reversal: full tape abrogation is binding repo policy;
  no shrunken-tape-retained-for-BBNF floor is permitted under
  `feedback_no-orthogonal-codepaths`.
- BBNF self-parse regression > 10% triggers substrate reversal of the
  responsible commit.

## Archaeology

AZ-II.cutover supersedes the original `docs/tranches/AZ-II/waves/{W0,W1,W2}.md`
wave docs (carrying supersede notices per `6f78c1ef`). The pre-AZ-II
trajectory had W0 research + W1 Stage A/B + W2 deletion across three
waves; the audit synthesis observes that the W2-act activation pattern
is reusable for BBNF without further substrate work, that Stage A/B is
two regen invocations rather than a wave's worth of ceremony, and that
deletion is mechanical post-byte-equal-green.

The cutover wave inherits AZ-II's load-bearing invariants: BBNF
self-parse byte-identical pre-and-post; tape crate deleted at close;
no orthogonal substrate; IR audit pass fleet-wide 100%; rich AST
preserved on BBNF; measurement gates substrate; no deferrals.
