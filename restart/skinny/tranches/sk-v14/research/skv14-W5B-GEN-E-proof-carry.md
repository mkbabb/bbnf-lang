# SK-V14 W5B-GEN-E: W5A Proof Carry

Date: 2026-05-26.
Scope: Read-only audit of W5A JSON, Sheets, and BBNF-self proof carry into W5B-GEN.
Output: this file.

## §1 — Findings

The JSON carry proof is valid but must be preserved after W5B-GEN. `skinny/xtask/src/main.rs:166`
constructs a `RuntimeGenerationRequest` for `check-json`, and the W5A codegen
test at `skinny/crates/codegen/src/lib.rs:862` compares request output to
`emit_from_source`.

The Sheets/BBNF carry proof remains fail-closed rather than generated-role.
`skinny/crates/codegen/src/grammar_provider.rs:31` routes all runtime requests
through source fact validation, and the W5A tests at
`skinny/crates/codegen/src/lib.rs:886` and
`skinny/crates/grammar/src/lib.rs:689` assert named unsupported construct
errors with source location and hash.

The handwritten `sheets_witness` module is not W5B-GEN proof. W5A-E explicitly
states that it is proof-only and may not serve as the generated-role witness at
`restart/skinny/tranches/sk-v14/research/skv14-W5A-E-sheets-bbnf-witness.md:127`.

Fresh W5A carry verification passed at V6 HEAD:

```sh
cargo test -p codegen w5a_json_request_matches_emit_from_source -- --exact --nocapture
cargo test -p codegen w5a_sheets_bbnf_fail_closed_through_runtime_contract -- --exact --nocapture
cargo test -p grammar w5a_named_unsupported_constructs_are_source_located -- --exact --nocapture
cargo xtask check-json
cargo xtask gate-json --check-results --skv14-existing-results-capture
```

## §2 — Recommendations

W5B-GEN must rerun the three W5A carry tests, `cargo xtask check-json`,
`cargo xtask gate-json --check-results --skv14-existing-results-capture`, and
an explicit JSON generated-directory diff. W5A-D warned that the roster check
does not cover adjacent JSON-owned files at
`restart/skinny/tranches/sk-v14/research/skv14-W5A-D-json-unchanged-output.md:81`.

## §3 — Risks

If W5B-GEN removes provider dispatch but changes JSON generated output, the
wave re-opens W5A-D. If it claims Sheets/BBNF success without a generated-role
path or fail-closed proof, it re-opens W5A-E.

## §4 — Sources

- `restart/skinny/tranches/sk-v14/SPEC.md:728` through `SPEC.md:743`
- `skinny/xtask/src/main.rs:166`
- `skinny/crates/codegen/src/lib.rs:862`
- `skinny/crates/codegen/src/lib.rs:886`
- `skinny/crates/codegen/src/grammar_provider.rs:31`
- `skinny/crates/grammar/src/lib.rs:689`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-D-json-unchanged-output.md:81`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-E-sheets-bbnf-witness.md:127`
