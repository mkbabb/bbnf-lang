# SK-V8 W2 Hardening V2 Consolidated

Date: 2026-05-18.
Target: `8ce03af4` (`fix(sk-v8-wave2-gate): fold typed hardening disposition`).

## Verdict

ACCEPT, 6/6.

Minimum confidence: 93%.

## Disposition

V2 accepts the V1 fold. Lock 14's W2 parent-diff allowance is scoped to
`sk-v8-wave2` commits and the three real typed owner paths. The schema identity
is `sk-v8-real-typed-w2`; Apache host/API facts include root `mode`, root
`nodeName`, and job string fields. The implementation and documents describe
serde_json as the Track 2/oracle path with sonic-rs as a separate strict parity
lane.

REDRESS 91, HANDOFF, research, and plan surfaces agree that W2 admits
`apache_builds/real_typed_struct` and `citm_catalog/real_typed_struct` as
source/product parity rows only. They are not measured rows in the current W0
`skinny/RESULTS.md` manifest. `canada/real_typed_struct` remains rejected on
full-fixture DirectBuild-versus-serde checksum mismatch.

## Verification Cited By Challenge

- `cargo test -p bbnf-bench lock14_baseline -- --nocapture`
- `cargo xtask check-real-typed`
- `cargo test -p bbnf-bench real_typed -- --nocapture`
- `cargo test -p codegen typed_direct -- --nocapture`
- `cargo xtask check-json`
- `cargo xtask check-conformance`
- `git diff --exit-code HEAD^ HEAD -- skinny/RESULTS.md`
- Targeted off-scope diff over runtime/parser/substrate/direct/Track 2/parity/scan/materialization/RESULTS paths

## Required Folds

None in V2. V3 was still required because the unchanged target received an
additional adversarial checked-report challenge.
