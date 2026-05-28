# SK-V15 Wave W10 Plan: FNV Strict-Product Quarantine

Inputs: `skv15-W10-research.md`, W9 admission `a21573cdf`, SPEC Section 13,
and DISPATCH-PROMPT W10.

Intervention: add a bench-only FNV quarantine witness and a gate-consumed W10
report. The witness rejects hash-equal typed-semantic mismatch and rejects a
sidecar declared to share the same closed enum. The xtask report consumes the
production scan instead of allowing producer-only documentation.

Owner paths:

- `skinny/crates/bbnf-bench/src/fnv_quarantine.rs`
- `skinny/crates/bbnf-bench/src/lib.rs`
- `skinny/xtask/src/main.rs`
- `restart/skinny/tranches/sk-v15/research/w10/*`
- `skinny/REDRESS.md`

Falsifiability gate:

- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench fnv_quarantine_rejects_matching_hash_with_mismatched_typed_semantics -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench fnv_quarantine_rejects_shared_closed_enum_sidecar -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench fnv_quarantine_report_accepts_bench_only_metadata -- --exact`
- `cargo xtask gate-json --check-results --skv15-fnv-quarantine-report restart/skinny/tranches/sk-v15/research/w10/skv15-W10-fnv-quarantine-report.json`
- `rg -n "fnv|FNV" crates/core/src/runtime crates/core/src/backend crates/core/src/generate skinny/crates/runtime/src skinny/crates/codegen/src`

Report fields:

- schema and wave identity: `schema_version`, `wave_id`, `dep_row`.
- quarantine status: bench/diagnostic-only FNV scope, production migration
  blocked, runtime selector absent, production arbiter absent, production hash
  correctness proof absent.
- closed-enum rows: exact W11L/W11N/W11O typed/direct row set.
- adversarial fixtures: matching-hash typed-semantic mismatch and shared
  closed-enum sidecar.
- production scan: command, non-absent hit groups, classification, and routed
  disposition.
- commands: exact tests and report gate.
- disposition: `ADMIT-W10`.

Same-wave consumer: `bbnf-bench::fnv_quarantine` enforces semantic equality
independently from hash metadata; `gate-json` validates the report and rejects
missing production-scan routing.

Pre-blocked routes: production FNV arbiter, production hash correctness proof,
runtime selector role, production migration of closed-enum evidence,
hash-equality-only strict product, shared closed-enum sidecar acceptance,
docs-only close, and false clean-scan claims.

Expected result: W10 admits if exact adversarial tests pass, the report gate
consumes the quarantine metadata, production FNV hits are classified/routed, and
`DEP-W10-FNV-QUARANTINE` is consumed without changing unrelated dirty generated
CSS files.
