# SK-V8 W2 Hardening V5 - CH6

Role: CH6 adversarial close-read for source/product parity versus benchmark row-table admission.
Target reviewed: HEAD `bf2f073d` (`docs(sk-v8-wave2-hardening): record V4 accept cycle`), re-challenging the unchanged V4-folded target after `74fe4e1b`.

## Verdict

ACCEPT.

Confidence: 96%.

## Findings

1. HEAD is an unchanged implementation target for this question. `bf2f073d` adds only V4 hardening review files; the live SPEC, HANDOFF, REDRESS, RESULTS, gate code, report code, and typed source files have no diff from the folded target at `74fe4e1b`. That makes this a valid second challenge cycle rather than a new source review.

2. SPEC Section 5 can be closed only with the split now recorded: source/product parity admitted, benchmark row-table admission rejected/routed. Section 5 requires exact W2 rows, host/API facts, Track 1, Track 2/oracle, rollback boundaries, generated typed product rows, preservation of existing typed/direct GO rows, non-target row maintain, and Track 2 independence (`restart/skinny/tranches/sk-v8/SPEC.md:449-481`). The W2 plan explicitly defines the admissible failure route if a benchmark refresh hits unrelated W0 run-id or throughput drift: keep `skinny/RESULTS.md` unchanged, reject W2 benchmark row-table admission, and record source/product parity without weakening W0 validation (`restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:39-50`). The current state follows that route. It is not a benchmark-table close.

3. HANDOFF and REDRESS 91 now agree on the boundary. HANDOFF states W2 has source/product parity admitted and benchmark row-table admission rejected, then makes W3 the next move (`restart/skinny/tranches/sk-v8/HANDOFF.md:5-11`). Its disposition table repeats that W2 is source/product-admitted by `12aff1e4` while row-table admission is rejected/routed in REDRESS 91 (`restart/skinny/tranches/sk-v8/HANDOFF.md:131-135`). REDRESS 91 names only `apache_builds/real_typed_struct` and `citm_catalog/real_typed_struct` as admitted source/product rows, says they are absent from the W0 measured manifest, rejects Canada on full-fixture checksum mismatch, and says W2 does not claim six measured `real_typed_struct A / GO` rows (`skinny/REDRESS.md:2622-2659`).

4. The V3 report-gate blocker is folded executably. `gate.rs` now calls `w0_real_typed_metadata_expected(&fixture.name)` during W0 metadata validation, and that helper checks only `sk_v8_open_baseline("json/{fixture}/real_typed_struct/main")` (`skinny/crates/bbnf-bench/src/bin/gate.rs:57-64`, `skinny/crates/bbnf-bench/src/bin/gate.rs:1115-1117`). Real typed Criterion metadata specs are appended only when that measured-baseline predicate is true (`skinny/crates/bbnf-bench/src/bin/gate.rs:1299-1343`), and the regression test asserts `twitter` and `update_center` true while `apache_builds` and `citm_catalog` are false (`skinny/crates/bbnf-bench/src/bin/gate.rs:1718-1724`). This keeps source-only Apache/CITM fixtures from becoming unadmitted W0 metadata requirements.

5. The row-table guard remains strict rather than silently admitting Apache/CITM. `Report::validate_sk_v8_w0` still requires the exact W0 baseline row count, rejects duplicate or unknown row ids, checks outcome/verdict stability, and validates Track 1/Track 2 deltas (`skinny/crates/bbnf-bench/src/report.rs:494-532`). The baseline includes four measured real-typed rows only: `twitter`, `update_center`, `mesh`, and `marine_ik` (`skinny/crates/bbnf-bench/src/report.rs:678-684`, `skinny/crates/bbnf-bench/src/report.rs:755-776`, `skinny/crates/bbnf-bench/src/report.rs:813-819`). If local Apache/CITM real-typed Criterion data appears, the gate can render rows from it, but the W0 validator rejects the enlarged/unknown table instead of admitting it (`skinny/crates/bbnf-bench/src/bin/gate.rs:206-260`, `skinny/crates/bbnf-bench/src/report.rs:494-511`).

6. The typed source/product slice is real and still narrow. The schema adds `parse_apache_builds` and `parse_citm_catalog` under `schema_hash: sk-v8-real-typed-w2`, with Apache consuming `mode`, `nodeName`, and `jobs` fields and CITM consuming keyed `events` fields (`skinny/xtask/src/real_typed_schema.rs:7-99`). The generated parser has Apache/CITM root functions and generated bodies for those same fields (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1-4`, `skinny/crates/bbnf-bench/src/generated_real_typed.rs:53-70`, `skinny/crates/bbnf-bench/src/generated_real_typed.rs:169-340`). The bench typed layer maps Apache/CITM through generated Track 1, serde_json-backed Track 2/oracle, and a separate sonic-rs lane, then checksum-compares all lanes on minimal and full fixtures (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:182-190`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:225-323`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:596-618`). Canada is not reopened in the typed fixture map.

7. `skinny/RESULTS.md` has no row-table overclaim. The current measured table has real-typed rows only for `twitter`, `update_center`, `mesh`, and `marine_ik`; Apache/CITM and Canada appear only as parse/direct W0 rows (`skinny/RESULTS.md:7-28`, `skinny/RESULTS.md:50-71`). That matches REDRESS 91 and prevents W2 from being read as six measured `real_typed_struct A / GO` rows.

## Verification

- `git rev-parse --short HEAD`: `bf2f073d`.
- `git status --short`: clean before writing this file.
- `git log --oneline --decorate --max-count=12`: confirmed `bf2f073d` sits on top of `74fe4e1b` and records only V4 review files.
- `git diff --name-only --no-renames 74fe4e1b..HEAD`: only `restart/skinny/tranches/sk-v8/research/wave-2-hardening/V4/` review artifacts.
- `git diff --exit-code 74fe4e1b..HEAD -- skinny/RESULTS.md skinny/REDRESS.md restart/skinny/tranches/sk-v8/SPEC.md restart/skinny/tranches/sk-v8/HANDOFF.md skinny/crates/bbnf-bench/src/bin/gate.rs skinny/crates/bbnf-bench/src/gate.rs skinny/crates/bbnf-bench/src/report.rs skinny/xtask/src/real_typed_schema.rs skinny/crates/bbnf-bench/src/real_typed_struct.rs skinny/crates/bbnf-bench/src/generated_real_typed.rs`: no diff.
- Static close-read with `rg` and `nl` over SPEC Section 5, HANDOFF W2 disposition, REDRESS 91, `gate.rs`, `report.rs`, `RESULTS.md`, and the typed source/generated files named above.

I did not run cargo tests during V5 because the assignment says to write exactly one file. V4's recorded test evidence remains applicable because HEAD did not change the code or source artifacts under review.

## Required Folds

None.

Carry-forward constraint: W2 may close only as Apache/CITM source/product parity admitted with benchmark row-table admission rejected/routed. Do not treat Apache/CITM or Canada as measured `real_typed_struct` rows unless a later accepted benchmark row-table wave updates `skinny/RESULTS.md` and passes the row gates.
