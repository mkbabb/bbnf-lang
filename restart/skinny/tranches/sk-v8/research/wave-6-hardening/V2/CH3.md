# CH3 Accepted-Source Proof Review V2

Date: 2026-05-18.

Verdict: ACCEPT.
Confidence: 96%.

Scope: unchanged re-challenge of committed target `e500ad00` for accepted-source
proof on W0, W1, W2, and W5. REVISE threshold was a concrete blocker in
same-wave consumer proof, Lock 14 proof, profile/row threshold where required,
admitted/rejected distinction, or drift from V1.

## Evidence

- No V1 drift found. `e500ad00` is the current W6 close packet target, and the
  V1 consolidated packet already records CH3 ACCEPT for the same split: W0
  telemetry/gate only, W1 CostFacts gate binding only, W2 source/product parity
  with row-table admission rejected, and W5 named Lock 14 provider-boundary
  cleanup only
  (`wave-6-hardening/V1/HARDENING-W6-V1-CONSOLIDATED.md:33-35`).
- W0 has accepted gate/profile/row proof, not behavior-source admission. V12
  states `gate-json` consumes CostFacts/redress/Track 2 sentinels, substrate
  tuples, run id, profile artifact, hot leaf, sample cost, build/host/feature
  metadata, sidecar freshness, and comparator evidence
  (`wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:42-44`). Its audit
  records 38 manifest rows, 38 `gate_only` rows, 38 `SK-V8-open` rows, and 38
  frozen-run-id rows (`:57-60`), and admits W0 only as the baseline profile and
  telemetry lock (`:77-83`).
- W1 has same-wave gate consumption. HANDOFF records commit `c6345e4d` binding
  `gate-json --with-cost-facts` to `sk-v8-costfacts-v1`, 15 materialized JSON
  rules, SK-V8-W1 ids, evidence sources, REDRESS refs, zero gate-level missing
  evidence diagnostics, and strict comparator id rejection for lossy, sidecar,
  and unknown ids (`HANDOFF.md:166-175`). The W1 plan requires the command to
  compose the normal report gate and validate the W1 manifest before success
  (`skv8-W1-plan.md:33-48`, `:88-112`, `:124-130`), with no `RESULTS.md`
  update planned (`:74-75`). Commit `c6345e4d` touched only
  `skinny/crates/bbnf-bench/src/gate.rs` and `skinny/xtask/src/main.rs`.
- W2 keeps admitted source/product rows separate from rejected measured rows.
  The plan admits exactly `apache_builds` and `citm_catalog` through the typed
  schema/generator path and requires parity with serde_json Track 2/oracle plus
  the sonic-rs strict lane (`skv8-W2-plan.md:9-25`, `:39-50`). REDRESS 91 admits
  those two source/product rows, rejects `canada/real_typed_struct`, and rejects
  benchmark row-table admission because `skinny/RESULTS.md` is unchanged
  (`skinny/REDRESS.md:2622-2659`). W2 V5 confirms Apache/CITM do not require
  unadmitted Criterion rows and closes with benchmark row-table admission
  rejected (`wave-2-hardening/V5/HARDENING-W2-V5-CONSOLIDATED.md:18-27`).
- W5 has Lock 14 proof and no row/performance admission. The plan scopes W5 to a
  named provider-boundary cleanup, keeps generated output and `skinny/RESULTS.md`
  out of scope, and requires no performance claim or row-table refresh
  (`skv8-W5-plan.md:30-36`, `:81-86`). The research fold moves JSON provider
  material from generic `codegen/src/lib.rs` into `json_provider`, adds the
  `per_grammar_provider` allowlist, confines provider residency by scan, and
  names the audit gate as same-wave consumer (`skv8-W5-lock14-audit-research.md:140-156`,
  `:202-212`). W5 V5 records 148 source/test insertions under the <=150 cap,
  Lock 14 baseline 11/11, no performance claim, no row refresh, zero-drift
  diffs, and clean provider scans
  (`wave-5-hardening/V5/HARDENING-W5-V5-CONSOLIDATED.md:24-57`).
- W6 close wording matches those limits: W0 telemetry/report gate, W1 gate
  binding with no `RESULTS.md` change, W2 source/product-only with Apache/CITM
  not measured rows, W5 no generated output/row/performance movement, and W6 no
  source/generated/RESULTS/REDRESS change unless a mismatch is found
  (`skv8-W6-close-and-alpha-feedback.md:13-20`, `:27-33`, `:37-55`, `:95-106`).

## Required Fold

None. Preserve the current close packet.
