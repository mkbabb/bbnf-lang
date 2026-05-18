# SK-V8 W5 Plan - Grammar-Neutral Audit And Lock 14 Preservation

Date: 2026-05-18.

Status: planned as no-source audit close.

## Entry Gate

W5 enters after W1-W4 are admitted, rejected, or routed:

- W1 is closed by CostFacts gate binding.
- W2 is closed by V4+V5 challenge convergence.
- W3 is rejected/routed by V1 challenge on the pre-redress fit gate.
- W4 is rejected/routed by V3+V4 challenge convergence.

## Scope

Owner paths:

- `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md`
- `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md`
- W5 hardening artifacts under
  `restart/skinny/tranches/sk-v8/research/wave-5-hardening/`
- `restart/skinny/tranches/sk-v8/HANDOFF.md` only after challenge accepts the
  no-source close.

Source, generated output, and `skinny/RESULTS.md` owner paths are explicitly
out of scope. W5 research found no named Lock 14 drift to fix, so the source
LOC budget is 0.

## Falsifiability Gate

W5 passes only if all of the following hold:

1. No forbidden renamed JSON policy appears in generic crates. The focused scan
   covers old public JSON helpers and renamed policy surfaces:

   ```text
   StrictJson
   StrictJsonTrustedUtf8
   JsonStringMatch
   JsonNumberMatch
   skip_json
   match_json
   unescape_json
   shapes_for_json
   nominate_json
   materialization_for_rule
   descriptor_for_rule
   rule_by_name("json")
   MissingEntry("json")
   StructuralAlphabet::json
   UnionTape
   union_tape
   BackendShape::Union
   BackendShape::Json
   ```

2. Allowed JSON surfaces remain confined to grammar inputs, generated JSON
   output, per-grammar templates/providers, tests, and host/API schema facts.
3. REDRESS 36-38 remain reconciled by REDRESS 85 and REDRESS 86; the
   W7/W8-era Lock 14 unit suites still pass.
4. `skinny/RESULTS.md`, generated JSON output, generated typed output, direct
   guard source, and generic crate surfaces have zero diff from HEAD.
5. Root `cargo xtask regen --check` remains clean for non-JSON grammars,
   including CSS L4, Google Sheets, and BBNF-self generated output.
6. W5 makes no performance claim and performs no row-table refresh.

## Verification Commands

Run before hardening and again before W5 close if any document fold changes
the audit claim:

```text
cargo test -p bbnf-bench lock14_baseline -- --nocapture
cargo xtask check-json
cargo xtask check-real-typed
cargo xtask check-conformance
cargo test -p parse-that-regex -p passes -p codegen -p ir
cargo xtask regen --check
git diff --exit-code HEAD -- skinny/RESULTS.md skinny/crates/runtime/src/grammars/json skinny/crates/codegen/src/json_templates skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/ir/src skinny/crates/codegen/src skinny/crates/passes/src skinny/crates/parse-that-regex/src skinny/crates/bbnf-simd/src skinny/crates/runtime/src skinny/crates/bbnf/src skinny/xtask/src
rg -n '\b(StrictJson|StrictJsonTrustedUtf8|JsonStringMatch|JsonNumberMatch|skip_json|match_json|unescape_json|shapes_for_json|nominate_json|materialization_for_rule|descriptor_for_rule|rule_by_name\("json"\)|MissingEntry\("json"\)|StructuralAlphabet::json|UnionTape|union_tape|BackendShape::Union|BackendShape::Json)\b' skinny/crates/parse-that-regex/src skinny/crates/passes/src skinny/crates/codegen/src skinny/crates/ir/src skinny/crates/bbnf-simd/src skinny/crates/runtime/src skinny/crates/bbnf/src skinny/xtask/src --glob '!skinny/crates/runtime/src/grammars/json/**' --glob '!skinny/crates/codegen/src/json_templates/**'
```

The `rg` command is expected to return no matches with exit code 1.

## Hardening Plan

Run a W5 CH1-CH6 challenge against this no-source audit close:

- CH1: citations, command evidence, and zero-drift claims resolve.
- CH2: Lock 14 and grammar-neutrality hold; allowed JSON surfaces are not
  confused with generic JSON policy.
- CH3: REDRESS 36-38, 85, and 86 remain closed; no pre-blocked route reopens.
- CH4: 0 source LOC, 90-minute cap, and audit-gate same-wave consumer are
  realistic.
- CH5: no hidden substrate, sidecar, `UnionTape`, `BackendShape`, or
  Track 1/Track 2 coupling is introduced.
- CH6: no paper close; W5 closes on live command evidence, not assertion.

If the first hardening cycle returns a qualifying ACCEPT, re-challenge the
unchanged packet once more. W5 may close only after two consecutive qualifying
ACCEPT cycles or a specific orchestrator-approved narrower rule for this
no-source audit wave.

## Revert And Redress

No source revert is expected. If hardening finds live Lock 14 drift, stop W5
close and either:

- write a new W5 plan naming the exact source cleanup within <=150 LOC and the
  same-wave consumer tests; or
- route the mismatch in HANDOFF with exact owner paths if the fix exceeds W5's
  cap.

Do not update `skinny/RESULTS.md` or claim throughput movement in W5.
