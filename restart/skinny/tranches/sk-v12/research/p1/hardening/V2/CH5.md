# SK-V12 S-P1 Hardening V2 CH5 Hidden Coupling

Pass: S-P1 Profile. Cycle: V2.
Date: 2026-05-20.
Lens: CH5 hidden coupling.
Output: this file only.

## Findings

1. PASS - JSON direct/typed guard evidence remains fenced from generated
   non-JSON baseline planning. The CH5 contract requires Track 1 generated
   runtime and structurally independent Track 2 paths to stay distinct
   (`restart/prompts/skinny/PASS-1-PROFILE.md:148-153`). SK-V12 then makes a
   generated non-JSON direct or typed baseline the first material target, with
   generated Track 1, independent Track 2/oracle, strict equality, telemetry,
   and no JSON policy leak (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:38-49`);
   admitted JSON direct/typed rows are guards, parse-only is diagnostic, and
   JSON direct residual reopen requires fresh material evidence beyond REDRESS
   114-119 (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:57-69`). P1-B follows
   that boundary: direct rows are JSON digest-plane rows, typed rows are JSON
   typed guard rows, and neither is a non-JSON baseline row
   (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:137-143`,
   `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:197-204`,
   `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:248-252`).
   P1-E likewise treats JSON hot families as planning
   facts only and keeps the generated non-JSON blocker as the primary target
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:275-322`).

2. PASS - The non-JSON blocker is source-inventory evidence, not hidden report
   authority. Current codegen still accepts only the JSON runtime profile:
   `ensure_runtime_profile` returns `Ok(())` only for `grammar_name == "json"`
   and errors otherwise (`skinny/crates/codegen/src/json_provider.rs:4-12`),
   and both direct and typed emission call that guard
   (`skinny/crates/codegen/src/lib.rs:102-108`,
   `skinny/crates/codegen/src/lib.rs:139-146`). P1-F records that
   the only admitted non-JSON surface is the REDRESS 111 companion report lane,
   not a generated baseline or row movement
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:183-193`).
   The W1a validator rejects admission coupling by requiring `S / NO-GO`,
   `schema-only`, `non_json_gate_schema_only`, independent Track 2, and
   nonproducer diagnostic status (`skinny/crates/bbnf-bench/src/report.rs:1814-1825`).
   The gate path also exits after validating `--w1a-non-json-report`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:37-44`) and refuses to combine
   that flag with JSON result update or volatile probe flags
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:404-416`).

3. PASS - No source mutation masquerades as profile evidence. P1-A states that
   the behavior-source diff from the W0 source anchor is limited to gate/report
   files and that `50bd1648..HEAD` is empty under `skinny/crates`,
   `skinny/Cargo.toml`, and `skinny/Cargo.lock`
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:24-29`).
   P1-C says it did source-read/artifact extraction only and did not edit
   behavior source, `skinny/RESULTS.md`, stage, or commit
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:35-38`).
   P1-D marks PMU values as profile evidence only, with no `skinny/RESULTS.md`
   row movement or direct/typed admission
   (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:81-89`).
   P1-F records no SK-V11-close diff in `skinny/RESULTS.md` or
   `skinny/REDRESS.md`, and narrows source delta to result/redress plus
   gate/report consumer code, with no parser-source changes
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:66-68`,
   `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:197-205`).
   I re-ran the source filters after `d1e6938a`: `git diff
   --name-status 50bd1648..HEAD -- skinny/crates skinny/Cargo.toml
   skinny/Cargo.lock skinny/RESULTS.md skinny/REDRESS.md` and `git diff
   --name-status 3ce75df4..HEAD -- skinny/crates/codegen skinny/crates/runtime
   skinny/crates/bbnf-simd skinny/crates/parse-that skinny/crates/parse-that-regex
   skinny/Cargo.toml skinny/Cargo.lock` both returned no paths.

4. PASS - Benchmark/report metadata does not leak into admission authority.
   `skinny/RESULTS.md` states Track 1 is `runtime::generated_json::parse`,
   Track 2 is independent, and C++ sidecars are historical or absent, never
   strict W0 anchors (`skinny/RESULTS.md:143-146`). P1-F keeps the live
   `SK-V9-open` Criterion run id stale-by-name and says fresh `/tmp/skv12-p1`
   captures are not consumed by `skinny/RESULTS.md` as row movement, hot-leaf
   symbol resolution, or direct/typed admission evidence
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:21-33`,
   `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:208-228`).
   The report validator requires the JSON W0 diagnostic status
   `structural_scan+masking_probes+pmu+cycles:nonproducer`
   (`skinny/crates/bbnf-bench/src/report.rs:396-398`), the strict admission
   validator rejects stale/historical/absent comparator or sidecar freshness
   (`skinny/crates/bbnf-bench/src/gate.rs:170-180`), and the W1a non-JSON
   validator requires a same-plane strict internal oracle with `sidecar_freshness
   == "n/a"` (`skinny/crates/bbnf-bench/src/report.rs:1865-1879`). That is
   fail-closed metadata handling, not hidden coupling.

5. PASS - `/tmp` dependencies are declared profile evidence and fenced from
   behavior authority. The S-P1 prompt permits profile artifacts outside the doc
   tree under `/tmp/skv{N}-p1/` while committing only the P1 artifacts
   (`restart/prompts/skinny/PASS-1-PROFILE.md:204-206`). The capture manifest
   declares `/tmp/skv12-p1` and `/tmp/skv12-profile-target-50bd1648`, states that
   result authority remains `skinny/RESULTS.md`, and records no row movement
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:11-16`).
   It then declares primary capture status, CWD/alias correction, xctrace export
   policy, product-v2 recapture, derived self-time tables, PMU aggregate limits,
   and the Mode III boundary
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:38-68`,
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:70-116`,
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:118-168`).
   P1-C also declares the prior
   `/tmp/skv11-open-criterion-3ce75df` root as W0 Criterion diagnostic evidence,
   not fresh Mode III call-stack authority, and explicitly records 0/17 fresh
   Mode III samply/probe coverage under `/tmp/skv12-p1`
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:13-19`,
   `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:21-29`,
   `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:54-63`,
   `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:145-161`).
   I spot-checked the declared artifacts after
   the fold: summary/details TSVs are 82/410 data rows plus headers, product-v2
   exports are 48 files, parse exports are 34 files, and samply has 82 profile
   files plus 82 symbol sidecars. No `/tmp/skv12-p1/samply/probes`,
   `json_probes_*`, or structural capture path was present.

## Verdict

ACCEPT.

The V2 packet preserves CH5 separation after the V1 fold. JSON direct/typed
guard profiling is diagnostic and guard-maintenance evidence only; it is not
used as generated non-JSON baseline authority. Source inventory remains
read-only under behavior paths, benchmark/report metadata stays fail-closed, and
all non-repo `/tmp` dependencies used by the packet are declared as profile or
diagnostic evidence rather than implementation authority.

## Materials Read

- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/V1/CH5.md`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/V1/FOLD-REVISIONS.md`
- Relevant code inventory under `skinny/crates/codegen`,
  `skinny/crates/runtime/src/grammars`, and `skinny/crates/bbnf-bench`.
