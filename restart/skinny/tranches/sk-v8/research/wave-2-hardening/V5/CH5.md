# SK-V8 W2 Hardening V5 CH5 Review

Date: 2026-05-18.
Reviewer: CH5.
Target reviewed: `bf2f073d`
(`docs(sk-v8-wave2-hardening): record V4 accept cycle`).
Underlying folded implementation target: `74fe4e1b`
(`fix(sk-v8-wave2-gate): bind real typed metadata expectations to measured W0 rows`).
Lens: convergence mechanics, challenge archive discipline, consolidated records,
and unresolved V3/V4 fold detection.

## Verdict

Verdict: ACCEPT.

Confidence: 94%.

## Findings

1. V4 is correctly recorded as the first post-reset ACCEPT cycle. V3
   consolidated as `REVISE, 5/6 ACCEPT and 1/6 REVISE`, with the sole blocker
   in CH3: the standard checked report gate derived required
   `real_typed_struct` Criterion metadata from the source fixture map, causing
   Apache/CITM source-only typed fixtures to require unadmitted benchmark
   metadata. V4 consolidated as `ACCEPT, 6/6` on `74fe4e1b` and explicitly says
   it is qualifying ACCEPT cycle 1 after the V3 reset. V2 is therefore not
   being counted through the V3 reset.

2. HEAD preserves the V4-folded target rather than changing the implementation.
   `bf2f073d` adds only the six V4 challenge records plus
   `HARDENING-W2-V4-CONSOLIDATED.md`. A targeted diff from `74fe4e1b` to
   `bf2f073d` is empty for `skinny/RESULTS.md`, `skinny/REDRESS.md`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md`, `gate.rs`, and
   `real_typed_struct.rs`. The V5 re-challenge is therefore against the
   unchanged implementation fold accepted by V4.

3. The V3 blocker remains folded in the executable gate. Current `gate.rs`
   passes `w0_real_typed_metadata_expected(&fixture.name)` into W0 metadata
   validation, and that helper keys off
   `sk_v8_open_baseline("json/{fixture}/real_typed_struct/main")`. The
   regression test asserts that W0 measured rows such as `twitter` and
   `update_center` require real typed metadata while W2 source-only
   `apache_builds` and `citm_catalog` do not. That matches the V3 required
   fold and avoids admitting Apache/CITM benchmark rows by metadata side effect.

4. The row-table boundary and W0 validator remain coherent. `report.rs` still
   carries the strict `SK_V8_OPEN_RUN_ID` validator and exact
   `SK_V8_OPEN_BASELINE` row-count checks. The baseline and `skinny/RESULTS.md`
   contain exactly four measured `real_typed_struct` rows: `twitter`,
   `update_center`, `mesh`, and `marine_ik`. There are no measured
   `apache_builds/real_typed_struct`, `citm_catalog/real_typed_struct`, or
   `canada/real_typed_struct` rows. Apache/CITM remain source/product parity
   only, and Canada remains routed out.

5. Commit-per-cycle discipline is now acceptable for convergence. The earlier
   V2/V3 archive merger in `74fe4e1b` was visible but not usable as two
   qualifying ACCEPT cycles because V3 reset the packet. V4 then landed as its
   own docs-only commit, `bf2f073d`, with a body that records it as the first
   post-fold ACCEPT cycle and states that V5 is still required. This satisfies
   the relevant cycle separation for the current convergence close. The V5
   archive should likewise land as its own docs cycle before final closure, but
   that is ordinary archive discipline, not a blocker in the reviewed HEAD.

6. Consolidated records are present and internally aligned. V2, V3, and V4 each
   have six `CH*.md` records plus one consolidated file. V3's consolidated
   record names the checked-report metadata mismatch and required fold. V4's
   consolidated record names the fold, cites the expected verification evidence,
   records `Required Folds: None`, and marks V4 as qualifying ACCEPT cycle 1.
   I found no unresolved V3 or V4 required fold in the records reviewed.

7. For CH5's convergence lens, this V5 re-challenge can serve as the second
   post-reset ACCEPT signal for the mechanics it covers. Final W2 closure should
   still wait for the V5 set and V5 consolidated record to agree, but CH5 finds
   no remaining blocker that would force a REVISE.

## Verification

- `git rev-parse --short HEAD`: `bf2f073d`.
- `git status --short --untracked-files=all` before writing this file: clean.
- `git show --stat --oneline --decorate --no-renames HEAD`: confirmed HEAD
  adds only V4 challenge records and the V4 consolidated record.
- `git diff --name-status 74fe4e1b..bf2f073d`: confirmed the V4 archive is the
  only diff from the implementation fold to HEAD.
- `git diff --exit-code 74fe4e1b..bf2f073d -- skinny/RESULTS.md skinny/REDRESS.md restart/skinny/tranches/sk-v8/HANDOFF.md skinny/crates/bbnf-bench/src/bin/gate.rs skinny/crates/bbnf-bench/src/real_typed_struct.rs`:
  passed.
- `git diff --check 74fe4e1b..bf2f073d --`: passed.
- `rg` over V3/V4 hardening records: confirmed V3 has the only REVISE, V4 has
  six ACCEPT records, and V4 consolidated has no required folds.
- `rg` over `gate.rs`, `report.rs`, `skinny/RESULTS.md`, `skinny/REDRESS.md`,
  and `HANDOFF.md`: confirmed measured-baseline metadata binding, strict W0
  run-id validation, four measured W0 `real_typed_struct` rows, Apache/CITM
  source/product-only disposition, and Canada route-out text.

I did not rerun cargo tests because this assignment restricts me to writing one
owned file and cargo would write build artifacts. V4 already cites the relevant
test evidence for the folded implementation target.

## Required Folds

None.
