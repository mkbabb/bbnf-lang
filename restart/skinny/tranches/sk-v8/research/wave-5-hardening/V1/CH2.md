# SK-V8 W5 Hardening V1 CH2 - Generality

Verdict: REVISE.

Confidence: 92%.

## Findings

1. The W5 renamed-policy scan is too narrow to prove the SPEC Section 2.1
   grammar-branch gate. SPEC Section 2.1 requires public API, grammar-branch,
   primitive/table, role/fact, template/provider, and non-JSON proof scans.
   W5 repeats that no JSON policy may enter generic crates, allowed JSON
   surfaces must remain bounded, and renamed JSON policy must be covered.
2. The V1 focused scan covers old helper names and several renamed policy
   symbols, and the rerun returned no matches. That proves the old-name/
   renamed-helper subset, but it does not cover grammar-name branches or
   template-provider residency.
3. Current generic code still has a JSON grammar-profile branch in
   `skinny/crates/codegen/src/lib.rs`: `emit_from_source(grammar_name, ...)`
   flows through `ensure_runtime_profile`, which accepts only
   `backend.grammar_name == "json"` and errors otherwise. The same generic file
   embeds JSON module exports and JSON runtime/template includes.
   `lock14_baseline` classifies `crates/codegen/src/lib.rs` as
   `generic_surface`, not as a per-grammar template file.
4. This may be an allowed skinny per-grammar provider boundary, but W5 V1 does
   not explicitly classify it that way and the scan does not test it.

## Passing Evidence

- Working tree remained clean before the source fold.
- W5 zero-drift check passed before the source fold.
- `cargo test -p bbnf-bench lock14_baseline -- --nocapture` passed 10/10 from
  `skinny/` before the source fold.
- `cargo xtask check-json`, `cargo xtask check-real-typed`, and
  `cargo xtask check-conformance` passed from `skinny/`.
- `cargo test -p parse-that-regex -p passes -p codegen -p ir` passed from
  `skinny/`.
- Repo-root `cargo xtask regen --check` passed clean with 9 of 9 grammars
  matched.

## Required Folds

1. Extend W5's scan/audit to cover grammar-name branches and provider
   residency, not only old helper names. At minimum, scan for
   `grammar_name == "json"`, `backend.grammar_name`, `emit_from_source("json")`,
   `include_str!("json_templates`, and `runtime/src/grammars/json` references
   outside tests, grammar inputs, generated output, host/API schema facts, and
   explicitly allowed per-grammar provider/template files.
2. Explicitly classify the live `skinny/crates/codegen/src/lib.rs` JSON profile
   guard and JSON template includes as an allowed per-grammar provider boundary,
   or revise the W5 plan to move that residue behind a named provider/template
   boundary within the <=150 LOC cleanup cap.
3. Re-run the same zero-drift, Lock 14, W7/W8 residue, and root regen checks.
