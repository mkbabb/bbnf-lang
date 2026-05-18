# SK-V8 W5 Research - Grammar-Neutral Audit And Lock 14 Preservation

Date: 2026-05-18.

Status: closed by V4+V5 hardening convergence; named provider-boundary cleanup
admitted.

## Entry State

W5 is active after W4 closed as rejected/routed by V3+V4 hardening
convergence. The W5 SPEC entry gate is satisfied: W1-W4 have admitted,
rejected, or been explicitly routed. W5's default source budget is 0 LOC; it
may use <=150 source LOC only if a W5 plan names a small Lock 14 cleanup.

The W5 research question is therefore narrow: is there a named Lock 14 drift
that must be fixed before W6 close, or can W5 close from an audit gate with a
bounded named cleanup?

## Audit Scope

Required W5 surfaces from SPEC Section 8 and P3-C:

- no JSON policy in generic crates;
- allowed JSON surfaces remain grammar inputs, generated JSON output,
  per-grammar templates/providers, tests, and host/API schema facts;
- renamed JSON policy is audited, not only old symbol names;
- REDRESS 36, 37, and 38 residue clusters
  (`skinny/REDRESS.md:460-515`) remain neutralized by REDRESS 85
  (`skinny/REDRESS.md:2399-2427`) and REDRESS 86
  (`skinny/REDRESS.md:2431-2464`);
- generated JSON output and `skinny/RESULTS.md` have zero behavior drift by
  default;
- CSS L4, Sheets, and BBNF-self proof is required for generic edits. W5 touches
  the generic `codegen/src/lib.rs` surface only to delegate provider material to
  `json_provider`, so this is checked as unchanged-output coverage plus the
  provider-boundary scans rather than a new behavior proof.

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
- These tests cover the previously admitted neutralizations from REDRESS 85
  (`skinny/REDRESS.md:2399-2427`) and REDRESS 86
  (`skinny/REDRESS.md:2431-2464`): grammar-neutral string/number matchers,
  structure-derived
  materialization, generated direct build roles under renamed rule tests,
  `StructuralAlphabet::json()` removal, and generic nullability handling.

## Findings

1. The initial W5 audit found no active forbidden old JSON helper names, no
   `StructuralAlphabet::json()`, no `UnionTape`, no
   `BackendShape::Union`/`BackendShape::Json`, and no generated output or
   `RESULTS.md` drift.
2. V1 hardening found one named Lock 14 provider-boundary cleanup: the JSON
   profile guard and JSON template/runtime includes belonged in a per-grammar
   provider surface, not in the generic `codegen/src/lib.rs` surface.
3. REDRESS 36-38 (`skinny/REDRESS.md:460-515`) remain historical violation
   records, not live blockers. REDRESS 85 (`skinny/REDRESS.md:2399-2427`) and
   REDRESS 86 (`skinny/REDRESS.md:2431-2464`) record the admitted
   neutralization work. The current tests still pass.
4. The non-JSON proof burden remains satisfied by unchanged output over all
   nine root grammars. The W5 cleanup is a provider-boundary/allowlist change
   and does not alter generated output.
5. W5 must not claim performance movement, must not update
   `skinny/RESULTS.md`, and must not add a REDRESS item unless a later
   challenge discovers a concrete mismatch.

## V1 Hardening Fold

W5 V1 hardening returned CH1 and CH2 REVISE.

CH1 accepted the measured audit but required reproducible command context and
current anchors:

- `bbnf-bench`, skinny `xtask check-*`, and skinny package tests must be run
  from `skinny/`.
- Root `cargo xtask regen --check`, repo-path `git diff`, and repo-path `rg`
  must be run from the repository root.
- W5 uses current `skinny/RESULTS.md` anchors: W0 manifest rows at
  `skinny/RESULTS.md:46-85` and report/Track 2 authority at
  `skinny/RESULTS.md:138-141`.

CH2 found one live provider-boundary residue: `skinny/crates/codegen/src/lib.rs`
contained the JSON runtime-profile guard and JSON template/runtime includes
while also being classified as `generic_surface` by the Lock 14 baseline. The
fold is a named W5 Lock 14 cleanup, not a performance change:

- `skinny/crates/codegen/src/lib.rs` now delegates JSON provider material to
  `json_provider`.
- `skinny/crates/codegen/src/json_provider.rs` owns the JSON profile guard and
  template/runtime includes as a per-grammar provider surface.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs` adds
  `per_grammar_provider` and authorizes only W5 parent diffs for
  `crates/codegen/src/lib.rs` plus `crates/codegen/src/json_provider.rs`.
- The generic codegen scan for `grammar_name == "json"`, `backend.grammar_name`,
  `include_str!("json_templates`, and `runtime/src/grammars/json`, excluding
  the provider/template files, returns no matches.
- Provider residency remains explicit: `runtime/src/grammars/json` references
  are confined to the new provider module and xtask generated-output tooling.

Post-fold checks:

- `cargo test -p parse-that-regex -p passes -p codegen -p ir` passed:
  codegen 6 tests, ir 3 tests, parse-that-regex 22 tests, passes 8 tests, and
  doc-tests all green.
- `cargo xtask check-json`, `cargo xtask check-real-typed`, and
  `cargo xtask check-conformance` passed; conformance again accepted 21 valid
  fixtures and rejected 7 invalid fixtures.
- Root `cargo xtask regen --check` passed with `clean (9 of 9 grammars
  matched)`.
- Zero-drift diff over `RESULTS.md`, generated JSON/typed output, direct guard
  source, IR, passes, parse-that-regex, SIMD, runtime, skinny bbnf, and xtask
  owner paths returned clean.

## V2 Hardening Fold

W5 V2 hardening returned CH1 REVISE and five ACCEPT results. CH1 found no code
blocker, but required the documentation to stop carrying stale no-source and
no-generic-edit language after the named provider-boundary cleanup.

The fold keeps the code unchanged and clarifies that:

- the source/test insertion count remains 148, below W5's <=150 named Lock 14
  cleanup cap;
- `skinny/crates/codegen/src/lib.rs` is a generic surface touched only to
  delegate provider material to `skinny/crates/codegen/src/json_provider.rs`;
- same-wave consumer evidence is the audit gate plus existing codegen/runtime
  checks;
- REDRESS reconciliation anchors resolve to `skinny/REDRESS.md:460-515`,
  `skinny/REDRESS.md:2399-2427`, and `skinny/REDRESS.md:2431-2464`.

## V3 Hardening Fold

W5 V3 hardening returned CH1 REVISE and five ACCEPT results. CH1 found that
the audit-scope REDRESS assertion still named the REDRESS 36-38/85/86
reconciliation without inline spans. The fold keeps source unchanged and adds
the exact anchors directly to every active W5 REDRESS reconciliation assertion.

## V4/V5 Closure

W5 V4 accepted 6/6 with minimum confidence 95 as the first qualifying cycle
after V3. W5 V5 re-challenged the unchanged packet and accepted 6/6 with
minimum confidence 95. W5 is closed.

The closure admits the named Lock 14 cleanup only:

- same-wave consumer: the W5 audit gate itself;
- source owner paths:
  `skinny/crates/codegen/src/lib.rs`,
  `skinny/crates/codegen/src/json_provider.rs`, and
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs`;
- no generated output or `RESULTS.md` edit;
- verification commands: the scans and checks listed above;
- hardening authority:
  `restart/skinny/tranches/sk-v8/research/wave-5-hardening/V5/HARDENING-W5-V5-CONSOLIDATED.md`.
