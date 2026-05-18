# SK-V8 W1 Plan: CostFacts And Comparator Gate Binding

Date: 2026-05-18.
Status: plan phase. No implementation authority beyond the W1 redress slice
named here.

Authority:

- `restart/skinny/tranches/sk-v8/SPEC.md` Section 4.
- `restart/skinny/tranches/sk-v8/HANDOFF.md` W1 entry gate.
- `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md`.
- `restart/skinny/tranches/sk-v8/research/skv8-W1-a-costfacts-producer.md`.
- `restart/skinny/tranches/sk-v8/research/skv8-W1-b-gate-report-path.md`.
- `restart/skinny/tranches/sk-v8/research/skv8-W1-c-comparator-admission.md`.
- `restart/skinny/tranches/sk-v8/research/skv8-W1-d-lock14-nonjson.md`.
- `restart/skinny/tranches/sk-v8/research/skv8-W1-e-preblock-rollback.md`.
- `restart/skinny/tranches/sk-v8/research/skv8-W1-f-verification-matrix.md`.
- `skinny/REDRESS.md` item 87.

## 1. Entry Decision

W1 is dispatchable. W0 closed through V11 and V12 challenge convergence, every
current main row carries `SK-V8-open` telemetry, and the remaining W0 residual
is explicitly the `none:pre-W1` CostFacts sentinel plus producer-only
`gate-json --with-cost-facts` path.

The W1 implementation must not change parser behavior, generated JSON output,
typed product behavior, direct digest behavior, Track 2, SIMD primitives, BIR,
directives, substrate surfaces, or `BackendShape`.

## 2. Selected Redress

W1 will admit only if the following redress lands and verifies:

1. Complete grammar-neutral CostFacts evidence for every materialized rejected
   alternative.
2. Make `cargo xtask gate-json --with-cost-facts` compose the normal report gate
   and then validate a SK-V8 W1 CostFacts manifest before success.
3. Add strict comparator admission id binding so only admitted native strict
   comparator ids can satisfy the existing plane, strictness, freshness, and
   measured-path checks.
4. Preserve W0 `validate_sk_v8_w0()` sentinel semantics for the unflagged
   baseline gate.

This is a load-bearing gate redress, not a performance claim. CostFacts can
explain selected shapes and rejected alternatives; they cannot reopen a blocked
route without a later wave plan, row thresholds, same-wave production consumer,
and challenge acceptance.

## 3. Owner Paths

Implementation owner paths:

- `skinny/crates/passes/src/lib.rs`
- `skinny/crates/bbnf-bench/src/gate.rs`
- `skinny/xtask/src/main.rs`

Verification-only/generated-frozen paths:

- `skinny/crates/runtime/src/grammars/json/`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/track2/`
- `skinny/crates/bbnf-bench/src/parity.rs`
- `skinny/crates/bbnf-bench/src/scan.rs`
- `skinny/crates/bbnf-bench/src/materialization.rs`

Docs/status owner paths:

- `restart/skinny/tranches/sk-v8/research/skv8-W1-plan.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md` after redress disposition.
- `skinny/REDRESS.md` only if W1 rejects.

No `RESULTS.md` update is planned. W1 will emit a gate-consumed JSON manifest
from `gate-json --with-cost-facts` rather than overloading the 38-row W0 table.

## 4. Exact Intervention

`passes/src/lib.rs`:

- Keep the existing backend-shape choice order unchanged.
- Keep all rejected alternatives complete: every non-chosen `BackendShape` must
  remain present.
- Preserve REDRESS-72 `RedressBackfill` measurements for the existing
  string/SinkOnly regression.
- Add grammar-neutral `StaticAnalysis` evidence to every otherwise-empty
  rejected alternative. The evidence source ref is a generic CostFacts predicate
  id, not a JSON/corpus/workload policy.
- Tighten the missing-evidence diagnostic to require evidence on every rejected
  alternative. After W1, JSON CostFacts must emit no
  `BBNF-COSTFACTS-MISSING-EVIDENCE`.

`xtask/src/main.rs`:

- Accept `--with-cost-facts` with the normal gate flags
  `--advisory`, `--check-results`, `--update-results`, `--write-results`, and
  `--include-volatile-probes`.
- Run the normal `bbnf-bench --bin gate` path first with `--with-cost-facts`
  removed, suppressing the markdown stdout so the CostFacts command remains
  machine-readable JSON.
- Build the existing `codegen::cost_facts_from_source("json", source)` snapshot.
- Validate a W1 manifest before success:
  - schema `sk-v8-costfacts-v1`
  - wave id `SK-V8-W1`
  - one manifest row for each materialized JSON rule
  - rule key matches `CostFacts.rule_id`
  - chosen shape is nonempty
  - at least four rejected alternatives exist
  - every rejected alternative has nonempty evidence source and source ref
  - every rule reports at least one REDRESS reference, using REDRESS-72 where
    present and REDRESS-87 for CostFacts substrate/static evidence
  - no `BBNF-COSTFACTS-MISSING-EVIDENCE` diagnostic remains
- Print the validated JSON manifest. Any validation failure exits non-zero even
  in advisory mode.

`bbnf-bench/src/gate.rs`:

- Add `comparator_id` to `StrictAdmissionEvidence`.
- Reject strict admission unless the selected comparator id is one of the
  admitted native strict anchors: `sonic_rs_strict` or `serde_json`.
- Keep existing outcome, strictness, plane, freshness, and measured-validation
  checks unchanged.

## 5. Falsifiability Gates

W1 admits only if every gate below passes:

1. `W1-costfacts-complete`: `gate-json --with-cost-facts --advisory
   --check-results` exits 0 and emits schema `sk-v8-costfacts-v1`, 15 rule
   manifest rows, no `none:pre-W1`, no missing rejected-alternative evidence,
   and no `BBNF-COSTFACTS-MISSING-EVIDENCE`.
2. `W1-costfacts-negative`: focused tests reject missing rule id, missing chosen
   shape, missing rejected alternatives, missing evidence source/ref, missing
   REDRESS reference, missing wave id, and producer-only rendering.
3. `W1-strict-comparator-id`: strict admission accepts native strict ids only
   and rejects `sonic_rs_lossy`, sidecar ids, unknown ids, stale sidecars, plane
   mismatch, non-strict comparators, and view-boundary validation.
4. `W1-w0-stability`: `cargo xtask gate-json --advisory --check-results`
   remains valid against the W0 report path.
5. `W1-generated-freeze`: `cargo xtask check-json`, `cargo xtask
   check-real-typed`, and generated/product diff audits show no parser,
   generated, typed product, direct, Track 2, parity, scan, or materialization
   drift.
6. `W1-Lock14`: generic scans show no new JSON policy in `ir`, `passes`,
   `codegen`, runtime, SIMD, or parser templates. Because W1 edits generic
   `passes`, root non-JSON smoke tests must also pass for CSS L4, Sheets, and
   BBNF-self projection/accessor coverage.

## 6. Verification Commands

Focused:

```sh
cd skinny
CARGO_TARGET_DIR=/tmp/skv8-w1-target cargo test -p passes cost_facts -- --nocapture
CARGO_TARGET_DIR=/tmp/skv8-w1-target cargo test -p bbnf-bench strict -- --nocapture
CARGO_TARGET_DIR=/tmp/skv8-w1-target cargo test -p xtask w1_costfacts -- --nocapture
CARGO_TARGET_DIR=/tmp/skv8-w1-target RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --with-cost-facts --advisory --check-results >/tmp/skv8-w1-costfacts.json
jq -e '.schema == "sk-v8-costfacts-v1" and .wave_id == "SK-V8-W1" and (.manifest | length) == 15 and ([.diagnostics[]? | select(.code == "BBNF-COSTFACTS-MISSING-EVIDENCE")] | length) == 0' /tmp/skv8-w1-costfacts.json
```

No-behavior drift:

```sh
cd skinny
CARGO_TARGET_DIR=/tmp/skv8-w1-target RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --advisory --check-results
cargo xtask check-json
cargo xtask check-real-typed
cargo xtask check-conformance
git diff --exit-code -- crates/runtime/src/grammars/json crates/bbnf-bench/src/generated_real_typed.rs crates/bbnf-bench/src/direct_struct.rs crates/bbnf-bench/src/real_typed_struct.rs crates/bbnf-bench/src/track2 crates/bbnf-bench/src/parity.rs crates/bbnf-bench/src/scan.rs crates/bbnf-bench/src/materialization.rs
```

Lock 14 and non-JSON proof:

```sh
cd skinny
rg -n '\b(Json|json|JSON|serde_json|object|array|pair|field|StrictJson|skip_json|match_json|unescape_json|StructuralAlphabet::json)\b' crates/ir/src/cost.rs crates/codegen/src/lower -S
sed -n '390,626p' crates/passes/src/lib.rs | rg -n '\b(Json|json|JSON|serde_json|object|array|pair|field|StrictJson|skip_json|match_json|unescape_json|StructuralAlphabet::json)\b' -S
rg -n '(StrictJson|skip_json|match_json|unescape_json|StructuralAlphabet::json|Json[A-Za-z_]*Cost|costfacts_.*json)' crates/ir/src/cost.rs crates/passes/src/lib.rs crates/codegen/src/lower crates/codegen/src/lib.rs crates/bbnf-bench/src/report.rs crates/bbnf-bench/src/bin/gate.rs xtask/src/main.rs -S
cd ..
cargo test -p bbnf --test projection_totality struct_direct_documents_have_concrete_roots -- --nocapture
cargo test -p bbnf --test typed_accessor_surface struct_direct_document_projection_surface_per_grammar -- --nocapture
```

The known pre-existing `codegen::ensure_runtime_profile()` `json` guard is not
a W1 CostFacts policy change. Any new hit in generic CostFacts paths blocks W1
unless it is proven to be a per-grammar/template boundary.

## 7. Revert Protocol

If W1 fails after source edits:

1. Save the rejected source diff to
   `restart/skinny/tranches/sk-v8/research/skv8-W1-rejected.patch`.
2. Revert `passes`, `xtask`, and `bbnf-bench` changes as one slice.
3. Keep the research and plan artifacts.
4. Add `skinny/REDRESS.md` entry stating W1 rejected, naming the missing or
   non-neutral evidence class, the failed command, generated-output status, and
   that W2-W6 behavior waves remain blocked.

If W1 admits, commit the redress as a separate implementation commit and update
`HANDOFF.md` to mark W1 closed and W2 dispatchable.

## 8. Pre-Blocked Routes

W1 does not reopen:

- behavior changes;
- CostFacts-as-performance claims;
- global route policy that ignores rejected alternatives;
- generic JSON policy under neutral names;
- generated output drift;
- producer-only CostFacts/telemetry;
- sidecar, lossy, permissive, stale, or view-boundary strict admission;
- new directives, BIR variants, substrate surfaces, `BackendShape`, `UnionTape`,
  public substrate APIs, parser-owned facts, or sidecar substrates;
- REDRESS 28+33, 50-72, 74-79, 81, 87 outside its admitted CostFacts
  evidence-substrate boundary, 88-90, or Tier B string-boundary/parity work.

## 9. Downstream

If W1 admits, W2 becomes the next dispatchable wave. W3 remains blocked until
W1 closure plus a fresh W3 plan and required challenge acceptance. If W1
rejects, W2-W6 behavior waves remain blocked.
