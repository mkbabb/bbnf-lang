# SK-V8 W4 Hardening V1 CH4

Date: 2026-05-18.

Verdict: REVISE.

Confidence: 94%.

## Findings

1. The planned commands cannot prove the W4 gates as written. SPEC requires W4
   direct guard tests, Track 1/Track 2 independence, full-table maintain, and
   one full gate refresh. The plan benchmarks only the three selected rows plus
   the three existing direct GO rows; it does not remeasure the remaining
   non-target direct rows even though the hand Track 2 parser changes globally.
2. The checked report path is still W0-shaped. `gate-json --check-results`
   validates exact `SK-V8-open` row identity, run id, outcomes, verdicts, and
   baseline deltas. A successful W4 direct target would move selected rows from
   `N-direct / NO-GO` toward `A / GO`, which the current W0 validator is
   designed to reject. W4 needs a W4-aware checked gate before row-table
   admission.
3. Lock 14 currently blocks the planned W4 owner path. Live review runs showed
   `cargo test -p bbnf-bench direct_struct -- --nocapture` passes, while
   `cargo test -p bbnf-bench lock14_baseline -- --nocapture` and
   `cargo xtask gate-json --advisory --check-results` fail when
   `direct_struct.rs` is dirty. Even after commit, the parent-diff allowance
   only recognizes `sk-v8-wave2` typed owner paths, not `sk-v8-wave4` direct
   owner paths.
4. The RESULTS refresh/run-id drift caveat is acceptable only as a fail-closed
   routing rule, not as admission evidence. If the checked report refuses
   refresh on the known W0 run-id drift, W4 must not claim row-table admission.
   The current caveat is too loose because the live failure is Lock 14, and a
   future W4 success would also need a non-W0 validator.
5. Track 2 independence is under-verified. Direct parity proves equality, not
   independence. W4 needs a static or executable guard that the hand Track 2
   path does not call generated SinkOnly, generated typed helpers, generated
   Track 1, or a shared benchmark-private parser.
6. The command block should state `skinny/` as the working directory or use
   `--manifest-path skinny/Cargo.toml`; the W4 research already records that
   root-level cargo commands fail for this workspace.

## Required Folds

1. Add a W4-aware checked gate/report mode before row-table admission. It must
   allow only selected W4 direct rows to change status, enforce their Track 1
   and Track 2 floors, require same-run strict direct anchors and measured
   validation paths, enforce non-target direct rows no worse than -2.0% versus
   `SK-V8-open`, and preserve existing direct GO plus real-typed GO rows.
2. Replace the focused-only benchmark proof with the SPEC-required full W4 gate
   refresh, or split/reject if it cannot fit the cap. Stale W0 Criterion rows
   cannot prove full-table maintain for a global Track 2 parser change.
3. Fold Lock 14 for W4 explicitly if admitting source: allow exactly
   `crates/bbnf-bench/src/direct_struct.rs` under a `sk-v8-wave4` parent-diff
   scope, keep dirty frozen roots rejected, and add tests rejecting off-owner
   runtime/codegen/generic movement.
4. Add owner-path and independence guards: diff limited to W4 docs/redress plus
   `direct_struct.rs`, LOC <=300, no generated/runtime/codegen/BIR/direct
   Track 1 movement, and no Track 2 coupling calls inside the hand parser.
5. Tighten the run-id caveat: if checked W4 report admission does not pass, W4
   may only reject/route with `RESULTS.md` unchanged; it cannot admit W4 row
   status or source behavior on stale RESULTS.
