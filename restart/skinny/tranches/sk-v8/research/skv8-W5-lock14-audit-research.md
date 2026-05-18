# SK-V8 W5 Research - Grammar-Neutral Audit And Lock 14 Preservation

Date: 2026-05-18.

Status: research complete; no source drift found.

## Entry State

W5 is active after W4 closed as rejected/routed by V3+V4 hardening
convergence. The W5 SPEC entry gate is satisfied: W1-W4 have admitted,
rejected, or been explicitly routed. W5's default source budget is 0 LOC; it
may use <=150 source LOC only if a W5 plan names a small Lock 14 cleanup.

The W5 research question is therefore narrow: is there a named Lock 14 drift
that must be fixed before W6 close, or can W5 close as a no-source audit gate?

## Audit Scope

Required W5 surfaces from SPEC Section 8 and P3-C:

- no JSON policy in generic crates;
- allowed JSON surfaces remain grammar inputs, generated JSON output,
  per-grammar templates/providers, tests, and host/API schema facts;
- renamed JSON policy is audited, not only old symbol names;
- REDRESS 36, 37, and 38 residue clusters remain neutralized by REDRESS 85
  and 86;
- generated JSON output and `skinny/RESULTS.md` have zero behavior drift by
  default;
- CSS L4, Sheets, and BBNF-self proof is required for generic edits. W5 has no
  generic edit, so this is checked as unchanged-output coverage rather than a
  new source proof.

## Evidence

Repository state:

- `git status --short` returned clean before W5 research edits.
- `git diff --exit-code HEAD -- skinny/RESULTS.md
  skinny/crates/runtime/src/grammars/json
  skinny/crates/codegen/src/json_templates
  skinny/crates/bbnf-bench/src/generated_real_typed.rs
  skinny/crates/bbnf-bench/src/direct_struct.rs
  skinny/crates/ir/src
  skinny/crates/codegen/src
  skinny/crates/passes/src
  skinny/crates/parse-that-regex/src
  skinny/crates/bbnf-simd/src
  skinny/crates/runtime/src
  skinny/crates/bbnf/src
  skinny/xtask/src` returned clean.

Lock 14 executable baseline:

- `cargo test -p bbnf-bench lock14_baseline -- --nocapture` passed 10/10.
- The baseline validates the current allowlist, frozen root cleanliness,
  parent-diff authorization, and the five-variant `BackendShape` surface; it
  rejects `UnionTape` in the IR surface.

Renamed JSON policy scan:

```text
rg -n '\b(StrictJson|StrictJsonTrustedUtf8|JsonStringMatch|JsonNumberMatch|skip_json|match_json|unescape_json|shapes_for_json|nominate_json|materialization_for_rule|descriptor_for_rule|rule_by_name\("json"\)|MissingEntry\("json"\)|StructuralAlphabet::json|UnionTape|union_tape|BackendShape::Union|BackendShape::Json)\b' \
  skinny/crates/parse-that-regex/src \
  skinny/crates/passes/src \
  skinny/crates/codegen/src \
  skinny/crates/ir/src \
  skinny/crates/bbnf-simd/src \
  skinny/crates/runtime/src \
  skinny/crates/bbnf/src \
  skinny/xtask/src \
  --glob '!skinny/crates/runtime/src/grammars/json/**' \
  --glob '!skinny/crates/codegen/src/json_templates/**'
```

Result: no matches. The excluded paths are the generated JSON output and
per-grammar JSON template/provider surfaces that W5 allows.

Generation and conformance:

- `cargo xtask check-json` passed.
- `cargo xtask check-real-typed` passed.
- `cargo xtask check-conformance` passed: 21 valid fixtures accepted and 7
  invalid fixtures rejected.
- Root `cargo xtask regen --check` passed with `clean (9 of 9 grammars
  matched)`, covering BBNF-self, JSON, CSS L4, CSS pretty, Google Sheets, EBNF,
  BNF, CSV, and math generated output.

W7/W8 Lock 14 residue suites:

- `cargo test -p parse-that-regex -p passes -p codegen -p ir` passed:
  codegen 6 tests, ir 3 tests, parse-that-regex 22 tests, passes 8 tests, and
  doc-tests all green.
- These tests cover the previously admitted neutralizations from REDRESS 85 and
  86: grammar-neutral string/number matchers, structure-derived
  materialization, generated direct build roles under renamed rule tests,
  `StructuralAlphabet::json()` removal, and generic nullability handling.

## Findings

1. No W5 source cleanup is warranted. The audit found no active forbidden old
   JSON helper names, no `StructuralAlphabet::json()`, no `UnionTape`, no
   `BackendShape::Union`/`BackendShape::Json`, and no source or generated
   output drift.
2. REDRESS 36-38 remain historical violation records, not live blockers:
   REDRESS 85 and REDRESS 86 record the admitted neutralization work and the
   current tests still pass.
3. The non-JSON proof burden is satisfied for a no-source W5 close by unchanged
   output over all nine root grammars. No W5 generic edit exists that would
   require a new CSS L4 / Sheets / BBNF-self source proof.
4. W5 should plan a no-source audit close. It must not claim performance
   movement, must not update `skinny/RESULTS.md`, and must not add a REDRESS
   item unless the plan or challenge discovers a concrete mismatch.

## Recommended Plan

Proceed to a W5 plan with source LOC budget 0:

- same-wave consumer: the W5 audit gate itself;
- no source, generated output, or `RESULTS.md` edit;
- verification commands: the scans and checks listed above;
- hardening challenge: review the no-source audit close for Lock 14,
  REDRESS 36-38/85/86 consistency, no hidden generic JSON policy, and no
  paper close.
