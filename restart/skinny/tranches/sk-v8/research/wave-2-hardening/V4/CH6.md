# SK-V8 W2 Hardening V4 - CH6

Role: CH6 adversarial anti-paper-close audit.
Target reviewed: commit `74fe4e1b`
(`fix(sk-v8-wave2-gate): bind real typed metadata expectations to measured W0 rows`).

## Verdict

ACCEPT.

Confidence: 96%.

## Findings

1. The V3 checked-report blocker is folded in the executable gate. The previous
   failure came from deriving required `real_typed_struct` Criterion metadata
   from the source fixture map. In `gate.rs`, the metadata expectation is now
   passed as `w0_real_typed_metadata_expected(&fixture.name)`, and that helper
   resolves only rows present in `sk_v8_open_baseline("json/{fixture}/real_typed_struct/main")`.
   Required real-typed metadata specs are appended only when that boolean is
   true. The regression test proves `twitter` and `update_center` are expected
   while `apache_builds` and `citm_catalog` are not
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:57-63`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1115-1117`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1299-1343`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1718-1724`). This directly matches
   the V3 required fold.

2. W2 no longer claims benchmark row-table admission, and the rejection is not
   hidden as a deferred success. HANDOFF states the wave disposition as
   source/product parity admitted with benchmark row-table admission rejected
   for this wave, then makes W3 the next move
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:5-11`). The dispatch table repeats
   that W2 source/product parity is admitted by `12aff1e4` while benchmark
   row-table admission is rejected/routed in REDRESS 91
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:133-135`). REDRESS 91 says
   Apache/CITM are admitted source/product rows, are not present as measured
   rows in the current W0 manifest, and W2 does not claim six measured
   `real_typed_struct A / GO` rows (`skinny/REDRESS.md:2622-2625`,
   `skinny/REDRESS.md:2648-2652`). This is an explicit route-out, not paper
   close language.

3. SPEC Section 5 remains compatible with this split because W2 has a recorded
   disposition rather than an admitted benchmark table. Section 5 requires the
   plan to name exact typed rows, host/API facts, owner paths, Track 1,
   Track 2/oracle path, and rollback boundaries
   (`restart/skinny/tranches/sk-v8/SPEC.md:449-456`). It also states the full
   measured exit shape: new typed rows pass same-plane gates, existing typed
   and direct GO rows maintain, and non-target rows avoid regression
   (`restart/skinny/tranches/sk-v8/SPEC.md:467-478`). The W2 plan then defines
   the failure route for a rejected benchmark refresh: keep `RESULTS.md`
   unchanged, reject benchmark row-table admission, and record source/product
   parity without weakening W0 validation
   (`restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:39-50`). That is
   exactly the state recorded in REDRESS and HANDOFF, so there is no remaining
   contradiction that would force W2 benchmark rows to be admitted before W3.

4. The source/product typed slice is real and still isolated from benchmark
   admission. The schema source and generated file add Apache and CITM roots
   under `schema_hash: sk-v8-real-typed-w2`
   (`skinny/xtask/src/real_typed_schema.rs:7-31`,
   `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1-4`,
   `skinny/crates/bbnf-bench/src/generated_real_typed.rs:53-70`). The generated
   Apache parser consumes `mode`, `nodeName`, and `jobs`; the generated CITM
   parser consumes `events`, `id`, `name`, `subTopicIds`, and `topicIds`
   (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:169-340`). The source
   typed layer maps Apache/CITM through generated Track 1, serde_json-backed
   Track 2/oracle, and sonic-rs parity, then asserts checksum equality on both
   minimal and full fixtures (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:33-65`,
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:225-323`,
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:596-618`).

5. The W0 row-table guard remains strict. `Report::validate_sk_v8_w0` still
   requires the exact `SK_V8_OPEN_BASELINE` row count, rejects unknown row ids,
   checks outcome/verdict stability, and validates Track 1/Track 2 deltas
   (`skinny/crates/bbnf-bench/src/report.rs:494-532`). The baseline includes
   only four `real_typed_struct` rows: `twitter`, `update_center`, `mesh`, and
   `marine_ik` (`skinny/crates/bbnf-bench/src/report.rs:678-684`,
   `skinny/crates/bbnf-bench/src/report.rs:755-761`,
   `skinny/crates/bbnf-bench/src/report.rs:770-776`,
   `skinny/crates/bbnf-bench/src/report.rs:813-819`). Tests still reject both
   single-row and uniform W0 run-id drift
   (`skinny/crates/bbnf-bench/src/report.rs:2031-2039`). If local Apache/CITM
   Criterion real-typed estimates appear, `gate.rs` can render those rows, but
   the W0 validator will reject the enlarged/unknown row table rather than
   admitting it by accident (`skinny/crates/bbnf-bench/src/bin/gate.rs:206-250`,
   `skinny/crates/bbnf-bench/src/report.rs:494-511`).

6. `skinny/RESULTS.md` stayed unchanged in `74fe4e1b`, and the live manifest
   still has no measured Apache/CITM/Canada `real_typed_struct` rows. The
   current top table has measured real-typed rows only for `twitter`,
   `update_center`, `mesh`, and `marine_ik`, while Apache/CITM appear only as
   parse/direct rows (`skinny/RESULTS.md:5-28`). The metadata manifest mirrors
   the same four real-typed row ids and includes Apache/CITM only as parse/direct
   W0 rows (`skinny/RESULTS.md:50-71`). `git diff --exit-code 74fe4e1b^
   74fe4e1b -- skinny/RESULTS.md` produced no diff.

## Verification

- `git show --stat --oneline --decorate --no-renames 74fe4e1b`
- `git show --no-ext-diff --unified=80 --no-renames 74fe4e1b -- restart/skinny/tranches/sk-v8/HANDOFF.md skinny/REDRESS.md skinny/crates/bbnf-bench/src/bin/gate.rs`
- `rg -n "W2|source/product|benchmark|row-table|real_typed_struct|apache_builds|citm_catalog|canada" restart/skinny/tranches/sk-v8/SPEC.md restart/skinny/tranches/sk-v8/HANDOFF.md skinny/REDRESS.md skinny/RESULTS.md skinny/crates/bbnf-bench/src/bin/gate.rs skinny/crates/bbnf-bench/src/real_typed_struct.rs skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/xtask/src/real_typed_schema.rs`
- `git diff --exit-code 74fe4e1b^ 74fe4e1b -- skinny/RESULTS.md`
- `git diff --name-only --no-renames 74fe4e1b^ 74fe4e1b`
- `git diff --check 74fe4e1b^ 74fe4e1b`

I did not run cargo tests during this audit to avoid writing build artifacts
while the assignment allowed only the owned CH6 review file to be written. The
commit message records the focused test evidence for the folded gate.

## Required Folds

None.
