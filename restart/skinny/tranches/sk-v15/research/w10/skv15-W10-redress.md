# SK-V15 Wave W10 Redress: FNV Quarantine

Status: ADMIT-W10.

Implementation:

- Added `bbnf-bench::fnv_quarantine`, a bench-only witness surface for the
  W11L/W11N/W11O closed-enum rows.
- Added adversarial failures for equal FNV metadata with mismatched typed
  semantics and for shared closed-enum sidecar coupling.
- Added `cargo xtask gate-json --check-results --skv15-fnv-quarantine-report`
  so the quarantine report is an executable consumer, not producer-only
  metadata.
- Recorded non-absent production FNV scan hits and routed them under REDRESS
  252 as codegen-internal or old CSS diagnostic metadata. No production FNV hit
  is admitted as runtime selector, production arbiter, or correctness proof.

Evidence:

- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench fnv_quarantine::tests::fnv_quarantine_rejects_matching_hash_with_mismatched_typed_semantics -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench fnv_quarantine::tests::fnv_quarantine_rejects_shared_closed_enum_sidecar -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench fnv_quarantine::tests::fnv_quarantine_report_accepts_bench_only_metadata -- --exact`
- `cargo xtask gate-json --check-results --skv15-fnv-quarantine-report restart/skinny/tranches/sk-v15/research/w10/skv15-W10-fnv-quarantine-report.json`
- `rg -n "fnv|FNV" crates/core/src/runtime crates/core/src/backend crates/core/src/generate skinny/crates/runtime/src skinny/crates/codegen/src`
- `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` returned `16`.
- `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`
  returned `67`.

Disposition:

- `DEP-W10-FNV-QUARANTINE` is consumed for quarantine. Production migration
  remains `no:block`.
- W11 may proceed after W1-W10 reconciliation.
- The unrelated dirty generated CSS files remain unstaged and outside W10's
  owned slice.
