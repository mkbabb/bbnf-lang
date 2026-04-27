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
| W0 | in progress | Research + classifier-unification + audit baseline |
| W1 | planned | `StructRegistry` + `project_types` closure |
| W2 | planned | Direct-to-struct — JSON + Sheets |
| W3 | planned | Direct-to-struct — CSS L4 aggregate |
| W4 | planned | FINAL — three-grammar slice at AU parity |

## Handoff

- Opens on: post-B7 substrate (AY-II → AY-II-I → AY-III deferred;
  durable gates absorbed into AZ-I.W4 + AZ-II.W2).
- Closes into: AZ-II (BBNF self-hosting + tape deletion).
- BA opens on AZ-II close, not AZ-I close.
