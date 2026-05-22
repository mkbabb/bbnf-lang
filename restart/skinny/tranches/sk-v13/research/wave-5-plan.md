# SK-V13 W5 Plan - Regex Analysis Extraction + Decision Gate

Cycle: W5 Plan. Scope: SPEC Section 8.

## Selected Intervention

Create a small `skinny/crates/bbnf-regex` analysis crate and consume its facts
from `ir` and `passes` decision logic. Do not move runtime scanner APIs out of
`parse-that-regex`.

The intervention is admitted only if the extracted facts are consumed by
IR/passes and by a gate-visible generated selection path in the same redress. If
the generated JSON/CSS selection path cannot consume those facts, W5 records the
measured architectural block
`JSON-W5-REGEX-FACTS-NOT-CONSUMED-BY-GENERATED-DISPATCH` rather than claiming a
support-only admit.

## Owner Paths

Source owner paths:

- `skinny/Cargo.toml`.
- `skinny/crates/bbnf-regex/`.
- `skinny/crates/ir/src/`.
- `skinny/crates/ir/Cargo.toml`.
- `skinny/crates/passes/src/`.
- `skinny/crates/passes/Cargo.toml`.
- `skinny/crates/codegen/src/` only for generated-selection evidence/report
  tests named below.
- `skinny/crates/bbnf-bench/src/report.rs`.
- `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`.
- `skinny/xtask/src/main.rs`.

Evidence/document owner paths:

- `restart/skinny/tranches/sk-v13/research/w5/`.
- `skinny/REDRESS.md`.
- `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` only if a row
  moves or admits.

Any runtime JSON/CSS parser change is out of scope for W5 unless CHALLENGE
revises this plan.

## Implementation Shape

`bbnf-regex` exposes repo-owned facts:

- `analyze(pattern: &str) -> RegexFacts`.
- `RegexFacts { nullable, first, byte_classes, hir, string }`.
- `FirstSet::Exact(ByteSet256)` or `FirstSet::Unknown`.
- explicit `RegexKind`/HIR facts for whitespace, quoted string, JSON-like
  number, and unknown regexes, without JSON-specific names in the public API.

Consumers:

- `ir::nullability` uses `RegexFacts::nullable`.
- `passes::layout::types::regex_type` uses facts rather than exact pattern
  strings.
- `passes::recognizers::first_bytes` uses `FirstSet`; unknown first sets are
  recorded and fail closed for dispatch-disjoint optimization.
- `passes::extract::span_kind` no longer defaults every unknown regex to
  `Number`; unknown must produce a diagnostic/block path.
- `CostFacts` or diagnostics carry compact decision evidence: analyzed regex
  count, unknown count, fact consumer paths, generated-selection path, and
  cascade fallback status.

W5 does not hide the separate JSON role-inference problem in
`derive_materialization_roles`; that remains routed to W8/W9 unless the redress
needs a tiny status diagnostic.

## Gate And Report

Add a companion report:

- schema: `sk-v13-decision-regex-v1`.
- flag: `--skv13-decision-regex-report`.
- gate print: `G-W5-DECISION-REGEX`.

Required fields:

- `regex_fact_source`, `regex_fact_artifact_path`, `regex_fact_sha256`.
- `regex_fact_consumer_path` naming `ir` and `passes` call sites.
- `generated_selection_path`.
- `hardcoded_regex_scan_status`.
- `feature_gate_status`.
- `cascade_fallback_status`.
- `row_move_toward_sota_status`.

`row_move_toward_sota_status` must be `pass`, `admitted`, or
`measured_architectural_block`. `support_only`, `gate_only`, empty generated
selection, stale hashes, and silent cascade fallback reject.

## Falsifiability Gate

Primary gate: `G-W5-DECISION-REGEX`.

Pass conditions:

1. `bbnf-regex` unit tests prove nullable, first-set, byte-class, `\d`, range,
   quoted-string, whitespace, and unknown-regex behavior.
2. `ir` and `passes` no longer contain W5-blocked exact JSON regex pattern
   decisions for nullability, first bytes, and span kind.
3. Unknown regex facts fail closed or emit explicit diagnostics; no unknown
   first-set branch is treated as disjoint/no-overlap.
4. `gate-json --skv13-decision-regex-report` consumes the report.
5. JSON and admitted CSS guards maintain under `--check-results --advisory`.
6. At least one generated selection path consumes the facts and records either
   row movement by P3-C or a measured architectural block.

Target row family for movement, if reachable: `canada`, `mesh`,
`github_events`, or `update_center` `direct_to_struct`, limited to generated
FIRST/follow dispatch selection for object/array envelopes. W5 must not select
string, number, unicode, digest, or source-hook routes.

## Preblocked Routes

Binding preblocks:

- REDRESS 84, 87, 114, 115, and 121.
- REDRESS 119/120 cannot close W5 or any direct row.
- No support-only extraction.
- No JSON-specific generic branch under neutral names.
- No fused hidden solver and no old P1-P8 cascade fallback admission.
- No new directive, BIR variant, `BackendShape`, public substrate API, sidecar
  stream, or parser-owned cursor/list.

## Revert Protocol

On fail:

1. Revert `bbnf-regex`, `ir`, `passes`, `codegen`, report/gate, Lock 14, and
   evidence files as one slice.
2. Save rejected source patch at `/tmp/skv13-waveW5-rejected.patch`.
3. Record REDRESS with the failed condition: unsupported facts, JSON/CSS guard
   regression, no generated selection consumer, or row movement miss.

## Verification Commands

- `cargo test -p bbnf-regex`.
- `cargo test -p ir regex`.
- `cargo test -p passes regex`.
- `cargo test -p codegen cost_facts`.
- `cargo test -p bbnf-bench --bin gate skv13_decision_regex_report`.
- `cargo test -p xtask gate_json_passthrough_accepts_skv13_decision_regex_report_flag`.
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-decision-regex-report ../restart/skinny/tranches/sk-v13/research/w5/skv13-W5-decision-regex.json`.

## CHALLENGE Questions

- CH1: Can `bbnf-regex` analyze the current JSON regexes without changing
  nullability or first-set behavior accidentally?
- CH2: Does the API avoid JSON role names and generic grammar leaks?
- CH3: Are REDRESS 84/87/114/115/119/120/121 still blocked?
- CH4: Is the source/test/report LOC small enough for W5 redress?
- CH5: Does any generated selection path actually consume the facts, or is this
  support-only?
- CH6: Does the report reject paper-close states such as `support_only`,
  `gate_only`, empty generated selection, or silent cascade fallback?
