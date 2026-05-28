# SK-V16 S-P0 A5 - Decision-Engine Fold Integrity

Date: 2026-05-28.
HEAD: `fc16919d4`.
Axis: A5 decision-engine fold integrity.
Disposition: ACCEPT WITH PRUNE CANDIDATES.

## Finding Summary

Current Decision Engine proof remains executable and not scaffold-only:

- CSP + egraph + cost facts drive the current emission path.
- `codegen::emit_from_source` routes through pass-produced layout facts.
- `lower_to_rust` rejects missing/non-SAT CSP and policy/union mismatches before
  selecting a lowerer.
- All five BackendShape lowerers have concrete output tests.
- W7/W8/W9 dependency proofs remain executable tests and gate-consumed reports,
  not row SOTA movement.

## Evidence

Commands:

```sh
cargo test --manifest-path skinny/Cargo.toml -p passes decision_ -- --nocapture
# 3 passed

cargo test --manifest-path skinny/Cargo.toml -p codegen lower_ -- --nocapture
# 5 passed

cargo xtask gate-json --check-results \
  --skv15-backend-lowerers-report \
  ../restart/skinny/tranches/sk-v15/research/w9/skv15-W9-backend-lowerers-report.json
# previously consumed by W11; retained as W9 gate evidence
```

Local rerun in this S-P0 slice passed the `decision_` and `lower_` test
families. The backend-lowerer report remains the W9/W11 gate-consumed artifact.

## Prune Candidates

1. Add an adversarial CSP fixture where egraph-preferred selection violates a
   constraint while another canonical shape is valid. Admit only if resolver
   intentionally reselects or documents fail-closed behavior.
2. Require non-SinkOnly lowerers to carry policy/union facts into emitted
   runtime-plan artifacts before future row or runtime admission depends on
   them.
3. Keep broad `cargo test -p codegen` and generated-file staging out of
   admission evidence until dirty CSS generated files are resolved.
