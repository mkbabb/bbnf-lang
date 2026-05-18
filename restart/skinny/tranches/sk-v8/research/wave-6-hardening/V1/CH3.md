# CH3 Accepted-Source Proof Review

Date: 2026-05-18.

Verdict: ACCEPT.
Confidence: 96%.

Scope: adversarial review of accepted-source proof for W0, W1, W2, and W5:
same-wave consumer proof, profile/row threshold where required, Lock 14 proof,
and admitted/rejected distinction.

## Evidence

- W0 has an executable telemetry/report/gate consumer, not a behavior admission.
  V12 says `gate-json` consumes CostFacts/redress/Track 2 sentinels, substrate
  tuples, run id, profile artifact, hot leaf, sample cost, build/host/feature
  metadata, sidecar freshness, and comparator evidence
  (`wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:42-44`). The row audit
  reported 38 manifest rows, 38 `gate_only` rows, 38 `SK-V8-open` rows, and 38
  frozen-run-id rows (`:57-60`). W0 is explicitly admitted as the baseline
  profile and telemetry lock (`:77-84`), while W0 `gate_only` telemetry is not a
  structural-projection production consumer (`:72-73`).
- W1 has same-wave gate consumption. HANDOFF records commit `c6345e4d` binding
  `gate-json --with-cost-facts` to `sk-v8-costfacts-v1` with 15 materialized
  JSON rules, SK-V8-W1 ids, evidence sources, REDRESS refs, zero gate-level
  missing-evidence diagnostics, and strict native comparator id binding that
  rejects lossy, sidecar, and unknown ids (`HANDOFF.md:166-175`). The W1 plan
  required the command to compose the normal report gate and validate the W1
  manifest before success (`skv8-W1-plan.md:37-39`, `:95-112`, `:126-130`),
  and states CostFacts are not a performance claim or row-threshold shortcut
  (`:45-48`, `:74-75`). Commit `c6345e4d` touched only
  `skinny/crates/bbnf-bench/src/gate.rs` and `skinny/xtask/src/main.rs`.
- W2 preserves the admitted/rejected split. The plan admits exactly two typed
  fixtures and restricts edits to the typed schema/generator path
  (`skv8-W2-plan.md:9-25`), with parity against serde_json Track 2/oracle and
  the sonic-rs strict lane, plus existing typed rows kept green (`:41-50`).
  REDRESS 91 admits Apache/CITM source/product rows, rejects
  `canada/real_typed_struct`, and rejects benchmark row-table admission because
  `skinny/RESULTS.md` remains unchanged (`skinny/REDRESS.md:2622-2659`). W2 V5
  confirms the report-gate fold derives metadata requirements from measured W0
  baseline rows, not source fixtures, so Apache/CITM remain source/product rows
  and unadmitted Criterion rows are not required
  (`wave-2-hardening/V5/HARDENING-W2-V5-CONSOLIDATED.md:18-27`, `:31-40`).
- W5 has Lock 14 proof and no row/performance overclaim. The plan scopes W5 to a
  named provider-boundary cleanup, keeps generated output and `skinny/RESULTS.md`
  out of scope, and requires no performance or row-table refresh
  (`skv8-W5-plan.md:30-36`, `:81-86`). The research fold moves JSON provider
  material out of the generic `codegen/src/lib.rs` surface, adds the
  `per_grammar_provider` allowlist, and confines provider residency by scan
  (`skv8-W5-lock14-audit-research.md:140-156`). Closure admits only that named
  cleanup, with the audit gate as same-wave consumer and no generated output or
  `RESULTS.md` edit (`:196-212`). V5 hardening records 148 source/test
  insertions under the <=150 cap, `lock14_baseline` passing 11/11, generation
  and conformance checks, zero-drift diffs, and provider-residency scans
  (`wave-5-hardening/V5/HARDENING-W5-V5-CONSOLIDATED.md:24-57`).
- Cross-ledger evidence is aligned. W6 research and the close artifact assign
  W0/W1/W2/W5 the same accepted effects (`skv8-W6-close-reconciliation-research.md:15-20`;
  `skv8-W6-close-and-alpha-feedback.md:27-32`). W6 makes missing profile/row
  evidence, Lock 14 proof, or same-wave consumer proof a close falsifier
  (`skv8-W6-close-reconciliation-research.md:75-86`). Local checks found
  `skinny/RESULTS.md` and `skinny/REDRESS.md` unchanged versus HEAD, with
  `manifest_rows=38` and `real_typed_rows=4`.

## Required Fold

None. Preserve the current close wording: W0 telemetry/gate only; W1 gate
binding only; W2 source/product parity admitted with row-table admission
rejected; W5 named Lock 14 provider-boundary cleanup only.
