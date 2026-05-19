# SK-V10 W6 CHALLENGE - Consolidated

Disposition: ACCEPT WITH REQUIRED PLAN REVISION.

## CH1 Correctness

ACCEPT. `github_events` is a top-level array and directly consumes the W5
`Vec<T>` root proof. The proposed typed product uses existing schema classes:
borrowed strings, `u64`, `bool`, nested structs, `Option<T>`, and `Vec<T>`.
The plan does not rely on direct digest evidence or parse-only throughput.

Required redress detail: full-fixture parity must run over
`skinny/test_data/github_events.json` through generated Track 1, independent
Track 2, serde_json typed, and sonic-rs typed sidecars. A reduced sample test is
not sufficient for row movement.

## CH2 Generality / Lock 14

REVISE BEFORE REDRESS. The initial owner path list is the SPEC Section 9 table,
but redress will touch frozen typed owner paths. The Lock 14 validator checks
the parent frozen diff by commit subject and exact path set, and currently only
authorizes SK-V8 W2 and SK-V10 W5 typed/root changes. Without a W6 allowance,
the W6 source commit cannot pass Lock 14 even if the typed row is otherwise
valid.

Required plan revision: add
`skinny/crates/bbnf-bench/src/lock14_baseline.rs` as a W6 gate-validation owner
path and require an exact SK-V10 W6 owner-path allowance. The revision must not
authorize generic runtime, grammar, or parser substrate paths.

The W6 schema itself does not edit generic codegen behavior if it only adds the
`github_events` schema root and fixture product types to the W5 root model. The
W5 CSS L4 / Sheets / BBNF-self proof remains sufficient for the already-landed
root-model behavior; W6 must not add new generic root semantics.

## CH3 Regression / REDRESS

ACCEPT. The gate preserves W4's lesson: generated Track 1 alone is not enough.
Track 2/oracle must clear `ceil(same-run sonic_typed / 1.10)`. If Track 2
misses, the row-moving source slice is reverted and REDRESS records a measured
reject with `/tmp/skv10-waveW6-rejected.patch`.

The plan also correctly keeps existing typed maintain rows in scope. Redress
must run the W6 `gate-json` contract after measurement so row-count expansion
and maintain floors are consumed in the same wave.

## CH4 Cost

ACCEPT. `github_events` is 65 KB and 30 root entries, so it is the only W6
candidate likely to fit the redress cap. `gsoc-2018` is a 3.3 MB map root and
would combine a larger schema surface with a longer measurement slice. Do not
reverse the target order.

## CH5 Hidden Coupling

REVISE BEFORE REDRESS. Besides Lock 14, `Report::validate_sk_v8_w0` currently
requires exactly the W0 baseline row count and rejects unknown row ids. W6 must
add a W6-specific typed-row exception for
`json/github_events/real_typed_struct/main`; the exception must preserve exact
row-count discipline for all non-W6 rows.

`gate-json` also currently expects real-typed metadata only for rows already in
the W0 baseline. W6 must extend the metadata expectation only for
`github_events`, not for all fixtures with source-side typed schemas.

## CH6 Anti-Paper-Close

ACCEPT. A W6 PASS requires four same-run Criterion typed rows, full-fixture
checksum parity, Track 1 and Track 2 floors, `RESULTS.md` movement, and
`gate-json` validation. A proof-only close is not available here because W5
already closed the root proof. If throughput fails, W6 is a measured REDRESS
reject.

## Required Revision

Before source redress, commit a plan revision that adds the W6 Lock 14
gate-validation owner path and states the exact W6 parent-diff allowance. After
that revision, redress may edit only the revised owner set.
