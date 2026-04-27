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

W1 opens on the post-W0 substrate. Per `AZ-I.md` §W1 the wave fans
out into 3 parallel agents (one per data grammar). Hard-fail-and-
block on any unclosed `project_types` edge.

- **W1.1** — JSON `project_types` closure + `StructRegistry` entries
  for `value`, `array`, `object`, `pair` (~7 layouts expected).
- **W1.2** — Sheets `project_types` closure + entries for `sheet`,
  `row`, `cell`, `formula`, `reference`, value sub-shapes (~8 layouts).
- **W1.3** — CSS L4 `project_types` closure + entries for
  `stylesheet`, at-rule kinds, selector kinds, `declaration`, every
  typed-value enum (~60 layouts).
- **Orchestrator** owns `crates/core/src/backend/emitter.rs` registry-
  read wiring (W1.4) after the three closures land — the emitter
  edit composes across all three grammars and is consolidator-shaped.

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
