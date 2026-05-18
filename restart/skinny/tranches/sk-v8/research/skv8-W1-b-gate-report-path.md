# SK-V8 W1 Research B: Gate JSON / Report Integration Path

Date: 2026-05-18.
Scope: W1 integration path for CostFacts evidence in `gate-json --with-cost-facts`
and report validation, preserving W0 `gate-json --check-results` behavior and
forbidding parser behavior movement.
Output: `restart/skinny/tranches/sk-v8/research/skv8-W1-b-gate-report-path.md`.

## §1 - Findings

1. W1 is a gate/report wave, not a behavior wave. SPEC Section 4 gives W1 a
   zero parser/generated-behavior budget, names `skinny/xtask/src/`,
   `skinny/crates/bbnf-bench/`, CostFacts producers, `skinny/RESULTS.md`, and
   `skinny/REDRESS.md` as owner paths, and requires `gate-json --with-cost-facts`
   to reject missing CostFacts evidence after W1
   (`restart/skinny/tranches/sk-v8/SPEC.md:376`,
   `restart/skinny/tranches/sk-v8/SPEC.md:384`,
   `restart/skinny/tranches/sk-v8/SPEC.md:398`,
   `restart/skinny/tranches/sk-v8/SPEC.md:405`,
   `restart/skinny/tranches/sk-v8/SPEC.md:409`,
   `restart/skinny/tranches/sk-v8/SPEC.md:416`). HANDOFF repeats the same entry
   gate: W1 must leave generated JSON output and parser behavior unchanged and
   make `gate-json --with-cost-facts` reject missing evidence
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:174`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:181`).

2. W0 is intentionally stable on `none:pre-W1` sentinels. The W0 producer emits
   `costfacts_rule_id`, `costfacts_chosen_shape`, and
   `costfacts_rejected_alternative_ids` as `none:pre-W1`, plus `redress_entry =
   none` (`skinny/crates/bbnf-bench/src/bin/gate.rs:474`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:488`). The W0 report validator
   rejects any non-sentinel CostFacts/redress value
   (`skinny/crates/bbnf-bench/src/report.rs:1007`,
   `skinny/crates/bbnf-bench/src/report.rs:1019`), and the full-baseline test
   asserts those mutations fail (`skinny/crates/bbnf-bench/src/report.rs:2053`,
   `skinny/crates/bbnf-bench/src/report.rs:2060`). Current `RESULTS.md` mirrors
   that state in the telemetry manifest (`skinny/RESULTS.md:44`,
   `skinny/RESULTS.md:48`).

3. `gate-json --with-cost-facts` currently bypasses the report gate. `xtask`
   diverts any invocation containing `--with-cost-facts` into
   `gate_json_cost_facts()` (`skinny/xtask/src/main.rs:240`,
   `skinny/xtask/src/main.rs:243`). That path accepts only `--advisory`, reads
   `grammars/json.bbnf`, builds a `codegen::cost_facts_from_source()` snapshot,
   prints JSON schema `sk-v7-costfacts-v1`, and returns `Ok(())`
   (`skinny/xtask/src/main.rs:274`, `skinny/xtask/src/main.rs:304`). It does not
   call `bbnf-bench`'s report generator, does not compare `skinny/RESULTS.md`,
   and does not fail on missing CostFacts diagnostics. The normal W0 path does
   call `bbnf-bench --bin gate`, which validates schema/W0 telemetry and fails if
   the rendered report differs from checked-in `RESULTS.md`
   (`skinny/xtask/src/main.rs:244`, `skinny/xtask/src/main.rs:266`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:338`).

4. The CostFacts substrate already has evidence fields, but current JSON
   CostFacts still expose missing evidence as non-fatal diagnostics. `CostFacts`
   stores chosen shape, rejected alternatives, priority, optional capacity, and
   rejected-alternative evidence (`skinny/crates/ir/src/cost.rs:5`,
   `skinny/crates/ir/src/cost.rs:13`, `skinny/crates/ir/src/cost.rs:81`,
   `skinny/crates/ir/src/cost.rs:109`). `passes::compile()` installs CostFacts
   into `LayoutFacts` (`skinny/crates/passes/src/lib.rs:44`,
   `skinny/crates/passes/src/lib.rs:55`), but the recognizer planner emits
   `BBNF-COSTFACTS-MISSING-EVIDENCE` when a facts row lacks measurement-backed
   evidence (`skinny/crates/passes/src/lib.rs:420`,
   `skinny/crates/passes/src/lib.rs:424`). Existing tests currently expect that
   diagnostic to be present (`skinny/crates/passes/src/lib.rs:1496`,
   `skinny/crates/passes/src/lib.rs:1501`,
   `skinny/crates/passes/src/lib.rs:1553`,
   `skinny/crates/passes/src/lib.rs:1560`). W1 therefore has a real pre-block:
   the gate can be wired first, but it should reject until the facts are
   backfilled or the wave records a REDRESS rejection.

5. Strict comparator refusal is partly factored and should remain separate from
   W0 sentinel validation. `bbnf_bench::gate::validate_strict_admission()` rejects
   non-GO outcomes, deferred/view-boundary validation, plane mismatch, stale or
   historical freshness, absent evidence, and `sidecar-same-run` without a
   structured manifest (`skinny/crates/bbnf-bench/src/gate.rs:135`,
   `skinny/crates/bbnf-bench/src/gate.rs:175`). Its focused tests cover the
   rejection shapes (`skinny/crates/bbnf-bench/src/gate.rs:451`,
   `skinny/crates/bbnf-bench/src/gate.rs:518`). W1 should consume this predicate
   from report validation for strict-admission rows instead of duplicating it in
   prose.

## §2 - Recommendations

1. Keep W0 stable by leaving `Report::validate_sk_v8_w0()` and the unflagged /
   `--check-results` `gate-json` path behaviorally unchanged. Do not relax the
   W0 sentinel rule in place. Add a separate W1 validator, for example
   `Report::validate_sk_v8_w1_costfacts(&CostFactsGateReport)`, that first calls
   the existing W0 validation for row identity, run id, throughput, comparator
   baseline, and full-table maintain, then validates the W1 CostFacts manifest.
   This preserves the V12 closure basis that W0 consumes CostFacts/redress
   sentinels and leaves W1 to replace or supplement them
   (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:42`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:44`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:67`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:68`).

2. Change `xtask gate-json --with-cost-facts` from a JSON-only producer into a
   composed gate:

   - Run the normal bench report gate first, forwarding `--advisory`,
     `--check-results`, and `--update-results` according to existing W0 semantics.
   - Build the `codegen::cost_facts_from_source("json", source)` snapshot in
     `xtask`, where the codegen dependency already exists
     (`skinny/xtask/Cargo.toml:12`, `skinny/xtask/src/main.rs:284`,
     `skinny/xtask/src/main.rs:285`).
   - Validate the CostFacts snapshot and the gate-consumed report/JSON payload.
     Any `BBNF-COSTFACTS-MISSING-EVIDENCE` diagnostic, empty rejected-alternative
     evidence vector, empty `source_ref`, missing rejected alternative, missing
     wave id, or missing REDRESS reference should exit non-zero after W1.

   This avoids adding `codegen` to `bbnf-bench` while still making
   `gate-json --with-cost-facts` the same-wave consumer.

3. Represent W1 CostFacts as a rule-level manifest, not as fake per-row data.
   Current report rows are 38 corpus/workload rows, while the JSON CostFacts
   snapshot contains materialized grammar rules (`skinny/crates/codegen/src/lib.rs:222`,
   `skinny/crates/codegen/src/lib.rs:248`; current REDRESS verification observed
   15 entries at `skinny/REDRESS.md:2491`, `skinny/REDRESS.md:2495`). Add either:

   - a `## SK-V8 W1 CostFacts Manifest` section in `RESULTS.md`, rendered and
     validated by `report.rs`, with one row per rule; or
   - a gate-consumed JSON payload whose digest is rendered in `RESULTS.md`.

   The manifest row should include at minimum: grammar, rule id, chosen shape,
   rationale, priority step, rejected shapes, evidence source(s), source refs,
   W1 wave id, and REDRESS reference. The existing W0 row CostFacts cell should
   not be overloaded with all 15 rule decisions; at most it should carry a digest
   or manifest reference if W1 chooses to replace the sentinel.

4. Promote missing-evidence diagnostics to hard gate failures after W1. The
   current W9 substrate deliberately made `BBNF-COSTFACTS-MISSING-EVIDENCE`
   visible but non-fatal (`skinny/REDRESS.md:2487`,
   `skinny/REDRESS.md:2490`). W1's job is the policy flip: the same signal must
   fail `gate-json --with-cost-facts` after the facts are expected to be
   gate-consumed. If the facts cannot be fully backed by W1, reject W1 with a
   REDRESS entry rather than paper-closing the diagnostics.

5. For strict comparator integration, add a report-side adapter that converts
   each candidate strict-admission row and its selected comparator evidence into
   `StrictAdmissionEvidence` and calls
   `gate::validate_strict_admission()`. W0 rows should still fail this adapter
   because they are deferred/view-boundary by design; W1 should only require the
   adapter when a row is being claimed as strict admission. This keeps current W0
   `A / GO` typed/direct rows from being reinterpreted as strict SOTA proof.

## §3 - Owner Files And Tests

Owner files:

- `skinny/xtask/src/main.rs`: primary CLI composition. Accept
  `--with-cost-facts --check-results`, run the normal report gate, build the
  CostFacts snapshot, and exit non-zero on missing evidence.
- `skinny/crates/bbnf-bench/src/report.rs`: add W1 CostFacts manifest structs,
  rendering, and validation, without weakening `validate_sk_v8_w0()`.
- `skinny/crates/bbnf-bench/src/gate.rs`: reuse `StrictAdmissionEvidence` for
  report-side strict-admission refusal.
- `skinny/crates/passes/src/lib.rs`: backfill evidence so JSON CostFacts no
  longer emit `BBNF-COSTFACTS-MISSING-EVIDENCE`, or leave the diagnostic and let
  W1 reject.
- `skinny/crates/ir/src/cost.rs`: use existing `RejectedAlternative.evidence`,
  `Measurement.source`, and `Measurement.source_ref` before adding fields.
  Extend only if the report cannot express wave id / REDRESS reference outside
  generic IR.
- `skinny/RESULTS.md`: add the W1 manifest or digest only after the gate consumes
  it. Keep generated JSON output and parser behavior unchanged.
- `skinny/REDRESS.md`: update only if W1 rejects or records the W1 admission
  evidence route.

Recommended tests:

- `cargo test -p bbnf-bench w0_report_accepts_exact_opening_baseline` must keep
  passing unchanged to prove W0 stability.
- New `bbnf-bench` report tests:
  `w1_costfacts_rejects_pre_w1_sentinel`,
  `w1_costfacts_rejects_missing_rejected_alternative_evidence`,
  `w1_costfacts_rejects_missing_wave_or_redress_ref`, and
  `w1_costfacts_accepts_rule_manifest_without_relaxing_w0`.
- New `xtask` unit tests around the pure validation helper:
  absent `BBNF-COSTFACTS-MISSING-EVIDENCE` diagnostics pass, present diagnostics
  fail, empty `source_ref` fails, and `--with-cost-facts --check-results` is
  accepted as a flag shape.
- `cargo test -p passes cost_facts` should be updated so the positive JSON path
  no longer expects `BBNF-COSTFACTS-MISSING-EVIDENCE`; keep a negative fixture
  that proves the diagnostic still exists for deliberately unevidenced facts.
- `cargo test -p codegen cost_facts` or an added snapshot test should assert the
  JSON CostFacts snapshot has the expected rule count, all non-chosen backend
  shapes in `rejected`, and nonempty evidence/source refs.
- W1 verification commands: `cargo xtask gate-json --advisory --check-results`,
  `cargo xtask gate-json --with-cost-facts --advisory --check-results`,
  `cargo xtask check-json`, `cargo xtask check-real-typed`, and
  `cargo xtask check-conformance`. Add a generated-output diff audit because
  SPEC W1 forbids generated JSON/parser behavior movement
  (`restart/skinny/tranches/sk-v8/SPEC.md:403`,
  `restart/skinny/tranches/sk-v8/SPEC.md:405`,
  `restart/skinny/tranches/sk-v8/SPEC.md:415`).

## §4 - Risks / Pre-Blocks

- Do not turn `validate_sk_v8_w0()` into a mixed W0/W1 validator. It currently
  pins W0 row count, row ids, outcomes, verdicts, throughput deltas, run id, and
  sentinels (`skinny/crates/bbnf-bench/src/report.rs:494`,
  `skinny/crates/bbnf-bench/src/report.rs:532`,
  `skinny/crates/bbnf-bench/src/report.rs:655`). Weakening it would destabilize
  `gate-json --check-results`.
- Do not let `--with-cost-facts` remain an early-return side channel. If it does
  not run or compose the normal report check, W1 can pass while `RESULTS.md` is
  stale or throughput drifted.
- Do not hide current missing CostFacts evidence by filtering diagnostics out of
  the JSON report. W1 either backfills evidence or rejects; the existing
  diagnostic is the pre-block signal.
- Do not encode JSON grammar policy in generic CostFacts/passes/codegen paths.
  SPEC Section 2.1 requires public API, grammar-branch, primitive/table, role
  boundary, template/provider, and non-JSON proof checks for generic CostFacts
  edits (`restart/skinny/tranches/sk-v8/SPEC.md:261`,
  `restart/skinny/tranches/sk-v8/SPEC.md:282`).
- Do not treat CostFacts as performance proof. SPEC pre-blocks
  CostFacts-as-performance claims and producer-only telemetry
  (`restart/skinny/tranches/sk-v8/SPEC.md:421`,
  `restart/skinny/tranches/sk-v8/SPEC.md:423`).

## §5 - Sources

- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md`
- `skinny/xtask/src/main.rs`
- `skinny/xtask/Cargo.toml`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/gate.rs`
- `skinny/crates/ir/src/cost.rs`
- `skinny/crates/passes/src/lib.rs`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
