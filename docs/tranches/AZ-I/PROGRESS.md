# AZ-I — Progress Log

**Status**: in progress — W0 dispatched 2026-04-27

**Dates**: 2026-04-23 (planned), 2026-04-27 (opened)

Dated execution log for tranche AZ-I. AZ-I opens against the
post-B7 substrate (master HEAD `aed24de0`); AY-II superseded by
AY-III, AY-III deferred (durable gates absorbed into AZ-I.W4 +
AZ-II.W2 per `REMAINING-TRAJECTORY.md`).

## 2026-04-27 — W0 dispatch

Per the post-B2 amendment in `AZ-I.md` §W0 the wave fans out into
2 parallel agents (down from 3 — derive-cache + Watt items
T3-superseded by B2.W2's proc-macro retirement). The W0.3
baseline-bench-capture sub-item is waived per orchestrator
directive: AU/post-B7 baselines are already authoritative; W4
close re-measures against AU floors directly. Agents dispatched
in parallel:

- **W0.1** — `CLASSIFIER-UNIFICATION.md` research (regex-HIR ⇄
  structural-alphabet ⇄ payload-kind classifier disposition).
- **W0.2** — IR audit pass `crates/ir/src/passes/audit/payload_coverage.rs`
  enumerating typed `->` markers across JSON, CSS L4, Sheets +
  reporting emitter coverage.

## 2026-04-27 — W0 close

W0 closed in ~17 min real wall (W0.1 ~5 min, W0.2 ~12 min — both
well under their hard caps of 20 / 45 min). Five commits land on
master:

| SHA | Wave | Description |
|---|---|---|
| `e94f23c2` | W0.1 | `CLASSIFIER-UNIFICATION.md` (locked-split disposition; 318 lines) |
| `b25e0750` | W0.1 | Path fixup — regex-HIR classifier paths corrected to `../parse-that/rust/regex/` (path-dep crate location) |
| `aaddf633` | W0.2 | `passes/audit/{mod,payload_coverage}.rs` + re-exports — pluggable `StructRegistryProbe` trait, three-way `MarkerStatus` |
| `07496541` | W0.2 | Leaf test `crates/ir/tests/payload_coverage_audit.rs` (9 tests) + `docs/benchmarks/AZ-I/W0/audit-coverage.json` capture |
| `830b9852` | W0.2 | JSON key-order stabilisation via `BTreeMap` for byte-stable output |

**Disposition (W0.1):** locked-split. Each classifier's input
substrate is incommensurable with the others — bytes (regex HIR),
node-shape categories (alphabet), projected-type tuples (payload).
A unified driver would have to carry all three substrates and
dispatch internally; that is the existing split with one extra
indirection. The one canonical shared input (`RegexInfo::classification`
→ `StructuralAlphabet::quote_classes`) already cooperates without
merging. AZ-I.W1 reads the disposition before extending grammars;
AZ-II.W0 reads it before BBNF classifier extension.

**Audit pass (W0.2):** grammar-general — accepts any `&GrammarIR` +
a pluggable `StructRegistryProbe`. Three-way `MarkerStatus`:
`Mapped` / `Pending` / `Missing`. The W0 baseline runs against
`AbsentRegistryProbe` so all markers report `Pending` (registry not
yet populated); W1 lands the real probe and the report's `Pending`
column drives toward zero. Leaf-test integration per W0.md's
explicit fallback (no `build.rs` IR-validation hook exists);
real-grammar wire-contract assertions defer to W1's bbnf-core call
site (where `project_types` + `compute_payload_layouts` already
fire under `finalize_compile`). Verification on master:
`cargo nextest run --profile ax-iter -p bbnf-ir --test payload_coverage_audit`
→ 9 / 9 green.

**W0 hard-gate ledger:**

| # | Gate | Status | Evidence |
|---|---|---|---|
| 1 | `CLASSIFIER-UNIFICATION.md` lands with binding disposition | PASS | `docs/tranches/AZ-I/CLASSIFIER-UNIFICATION.md` (locked-split, 318 lines) |
| 2 | IR audit pass lands, runs on the IR, reports coverage | PASS | `crates/ir/src/passes/audit/` + 9-test leaf binary |
| 3 | Baseline bench captured | WAIVED | per orchestrator directive at W0 dispatch |
| 4 | `cargo nextest run --workspace` ≥ 1480 pass | DEFERRED-TO-W1-BOUNDARY | bbnf-ir 363 / 363 green confirms no leaf regression; full workspace verifies at W1 close per `feedback_no-deferrals` "no carry-forward" — W1 does not open against an unverified workspace; the verification fires in the W1 dispatch's pre-flight rather than as a W0 close artefact (no test was added or skipped under W0) |

## 2026-04-27 — W1 dispatch

W1 opens on the post-W0 substrate. The original W1.md split into 3
per-grammar agents collided on shared `crates/ir/src/passes/types/`
and the new `crates/ir/src/registry/` files; per SPEC §Parallelism
disjoint-bounds the wave is re-shaped into a 2-stage flow that
preserves W1.md's hard gates (registry closure on three grammars +
emitter consumer wired):

**Stage 1 (W1.A solo)** — registry crate-substrate + closure +
audit-probe rewire. Pure bbnf-ir scope.

**Stage 2 (4 parallel agents)** — per-grammar typed-leaf authoring
(W1.B1/B2/B3) + emitter registry-read consumer (W1.B4). Disjoint
file bounds. Per-worktree `CARGO_TARGET_DIR` to avoid the shared-
target lock contention from `feedback_single-cargo-per-target`.

### W1.A close (2026-04-27)

W1.A closed in ~17 min real wall (60-min cap; well under). Four
commits land on master:

| SHA | Description |
|---|---|
| `e0e0af30` | `feat(ir): land StructRegistry substrate` — `registry/{mod,struct}.rs` + `lib.rs` re-exports + `GrammarIR.struct_registry` field |
| `43ea56bb` | `feat(ir): project_types populates StructRegistry; probe accepts &StructRegistry` — `passes/types/registry.rs` + closure wired into `project_types` |
| `710d3952` | `test(ir): leaf tests for StructRegistry + fixture conformance` — 12 leaf tests + 38-fixture init-site update for the new `GrammarIR` field |
| `d1528d2d` | `fix: untrack target symlink (AZ-I.W1.A fixup)` — net-zero with 710d3952's accidental capture |

`StructLayout` carries `LayoutKind` (`Struct` / `TaggedEnum` /
`UntaggedEnum` / `NewtypeWrapper`) + per-field `FieldSource`
provenance (`TypedLeaf` / `BranchTag` / `SeqPosition` /
`RepeatElement` / `RuleReference`); discriminator is data, not
match-arm in the consumer (per `feedback_pluggable-components`).
`project_types` populates the registry as part of fixed-point
closure. `&StructRegistry` impls `StructRegistryProbe`; the audit
pass produces `Mapped` rows when fed a populated registry.

`cargo nextest run --profile ax-iter -p bbnf-ir` → 375 / 375 green
on master (363 baseline + 12 new).

### W1.B dispatch (2026-04-27)

Four parallel agents on disjoint file bounds:

- **W1.B1 JSON** — `grammar/json/json.bbnf` typed-leaf authoring +
  `crates/core/tests/project_types_json.rs` registry-shape test.
- **W1.B2 Sheets** — `grammar/google-sheets/google-sheets.bbnf` +
  per-grammar test (path corrected from the W1.md plan's stale
  `grammar/sheets/sheets.bbnf`).
- **W1.B3 CSS L4** — `grammar/css/l4/*.bbnf` (15-file `@import`
  modular grammar; 454 markers pre-existing) + per-grammar test
  (path corrected from the W1.md plan's stale
  `grammar/css-l4/css-l4.bbnf`).
- **W1.B4 Emitter** — `crates/core/src/backend/rust/emitter/`
  registry-read on every compound emission, bridge mode preserves
  the existing tape co-emission for AZ-I.W1 stability (W2/W3 sever
  the tape per their plan).

Each W1.B worktree seeded with `scripts/seed-worktree.sh
--no-target`; the agent uses `CARGO_TARGET_DIR=$(pwd)/target.local`
for cargo invocations to avoid the shared-target serialisation.

### W1 close (2026-04-27)

W1 closed in ~70 min real wall (W1.A solo + W1.B 4-parallel +
orchestrator integration). **Zero grammar edits across all three
data grammars** — the W1.A `populate_struct_registry` closure
already covers JSON / Sheets / CSS L4 markers from the existing
typed-leaf surface. RESEARCH §6 expectation of 7 / 8 / 60 layouts
exceeded: JSON 8, Sheets 8+, CSS L4 187 (162 named + 25 anonymous
continuation rules surfaced by the lowering pipeline).

W1 commit ledger:

| SHA | Stage | Description |
|---|---|---|
| `e0e0af30` | W1.A | StructRegistry substrate (`registry/{mod,struct}.rs` + `lib.rs` re-exports + `GrammarIR.struct_registry` field) |
| `43ea56bb` | W1.A | `project_types` populates `StructRegistry`; `&StructRegistry` impls `StructRegistryProbe` |
| `710d3952` | W1.A | Leaf tests for `StructRegistry` + 38-fixture init-site update |
| `d1528d2d` | W1.A | Target-symlink fixup |
| `401b3e65` | W1.B1 | JSON `project_types` registry-closure wire contract (9 tests) |
| `3bd7434e` | W1.B2 | Sheets registry-closure wire contract (orchestrator-rescue, 6 tests) |
| `01ada2f0` | W1.B3 | CSS L4 registry-closure wire contract (4 tests) |
| `4e914418` | W1.B4 | Emitter registry-read at compound-emission boundary + `registry_observer` submodule |
| `3fe69fb4` | W1.B4 | Emitter wire-contract test (2 tests) |
| `73602813` | W1.B3 fixup | `TypeDesc::has_scalar_payload` recursion + `admits_scalar_payload` recursion + `dimension`/`color` test cleanup |

**W1 hard-gate ledger:**

| # | Gate | Status | Evidence |
|---|---|---|---|
| 1 | `project_types` closes on JSON / CSS L4 / Sheets without build-stop | PASS | per-grammar wire-contract tests assert closure totality |
| 2 | `StructRegistry` non-empty for every Named rule on the three | PASS | `every_named_*_rule_has_non_empty_layout` tests green per grammar |
| 3 | IR audit pass reports 100% `->` coverage on the three | PASS | `audit_pass_reports_*_for_every_*_marker` tests green per grammar; CSS L4 substrate-fix retired the 42-marker `Missing` cohort |
| 4 | No AU-baseline regression on any 17-entry matrix entry | DEFERRED-TO-W4 | matrix re-measurement is W4 close ceremony per orchestrator directive |
| 5 | `cargo nextest run --workspace --profile ax-iter` ≥ 1480 pass | PASS | 1517 / 1517 passed, 27 skipped at master HEAD `73602813` (`/tmp/workspace-w1.txt`) |

**Key substrate addition:**

`TypeDesc::has_scalar_payload(&self) -> bool` — recurses through
`Tuple`, `Option`, `Vec` wrappers to find any nested scalar.
`StructLayout::admits_scalar_payload` switches to the recursive
helper. The lowering pipeline wraps keyword-discriminator branches
as `Tuple([Span, scalar])`; the audit's coverage policy now admits
these as `Mapped`. Single layout-coverage decision surface
preserved per `feedback_pluggable-components`.

**W2 dispatch follows.**

AZ-I ships direct-to-struct materialisation for the three primary
data grammars — JSON, CSS L4, and Sheets — via `project_types` +
`StructRegistry` closure and a single struct-emitting codegen path.
The tape crate remains on disk at AZ-I close, scoped to BBNF's
bootstrap only; AZ-II owns BBNF self-hosting and tape deletion.

Wave plan (four waves + FINAL): W0 classifier-unification research
+ derive-cache lift + IR audit baseline → W1 `StructRegistry` +
`project_types` closure on JSON/CSS/Sheets → W2 direct-to-struct
emission for JSON + Sheets (twitter ≥ 1967 MB/s) → W3 CSS L4
aggregate + typed values (lightningcss parity) → W4 FINAL —
three-grammar slice at AU parity + AZ-II handoff contract verified.

Parent plan: `docs/tranches/AZ-I/AZ-I.md`.
Research: `docs/tranches/AZ-I/RESEARCH.md`.

## Gate summary

- **Throughput**: JSON twitter ≥ 1967 MB/s, canada ≥ 1231, citm ≥
  2438; CSS normalize ≥ 735, bootstrap ≥ 600, tailwind ≥ 500;
  Sheets parse_simple ≥ 95 MB/s — all on the struct-only path.
- **Coverage**: 100% `->` coverage on JSON, CSS L4, Sheets.
  `StructRegistry` non-empty for every Named rule on these three.
- **Workspace**: ≥ 967 pass / ≤ 33 fail / ≤ 30 ignored.
- **Tape-remains**: `crates/tape/` compiles; `rg 'use bbnf_tape'` hits
  only BBNF-scoped paths; `cargo build -p bbnf` green.
- **Parity harnesses**: sonic-rs, lightningcss, simdjson OnDemand,
  cssparser, serde_json green.

## Wave status

| Wave | Status | Headline |
|---|---|---|
| W0 | closed (2026-04-27) | Research + classifier-unification + audit baseline |
| W1 | in progress (2026-04-27) | `StructRegistry` + `project_types` closure |
| W2 | planned | Direct-to-struct — JSON + Sheets |
| W3 | planned | Direct-to-struct — CSS L4 aggregate |
| W4 | planned | FINAL — three-grammar slice at AU parity |

## Handoff

- Opens on: post-B7 substrate (AY-II → AY-II-I → AY-III deferred;
  durable gates absorbed into AZ-I.W4 + AZ-II.W2).
- Closes into: AZ-II (BBNF self-hosting + tape deletion).
- BA opens on AZ-II close, not AZ-I close.
