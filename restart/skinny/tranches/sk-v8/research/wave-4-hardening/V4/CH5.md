# SK-V8 W4 Hardening V4 CH5

Verdict: ACCEPT.

Confidence: 97%.

## Findings

1. No premature close coupling found. `HANDOFF.md` still says W4 is a proposed
   rejection/routing disposition pending hardening convergence and W5-W6 remain
   conditional (`restart/skinny/tranches/sk-v8/HANDOFF.md:5-13`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:135-141`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:316-321`). This matches Section
   3Z: V3 was only the first qualifying accept cycle, and V4 is the required
   unchanged second challenge before close wording can be folded
   (`restart/prompts/ORCHESTRATOR.md:104-121`,
   `restart/skinny/tranches/sk-v8/research/wave-4-hardening/V3/HARDENING-W4-V3-CONSOLIDATED.md:18-38`).
2. No parallel substrate or sidecar producer is admitted. SPEC Section 10 keeps
   new substrate surfaces, sidecar substrate, parallel substrate, sidecar
   evidence, Track 1/Track 2 coupling, and benchmark-private parsers globally
   blocked (`restart/skinny/tranches/sk-v8/SPEC.md:756-773`). W4's only
   attempted owner path was `skinny/crates/bbnf-bench/src/direct_struct.rs`,
   with runtime, codegen, BIR, directives, substrate, generated Track 1, and
   generic crates explicitly unchanged
   (`restart/skinny/tranches/sk-v8/research/skv8-W4-plan.md:21-40`,
   `skinny/REDRESS.md:2700-2706`).
3. Track 1/Track 2 independence remains honest for the rejected W4 candidate.
   SPEC requires Track 2 not to call generated SinkOnly, generated typed
   helpers, generated Track 1, or a shared benchmark-private parser
   (`restart/skinny/tranches/sk-v8/SPEC.md:626-638`). The live direct bench
   calls `direct_struct::track1_digest` and `direct_struct::track2_digest`
   separately (`skinny/crates/bbnf-bench/benches/json_parity.rs:181-205`);
   source has Track 1 calling `runtime::generated_json::parse_direct` while
   Track 2 calls `hand::sink_digest` with its own `HandParser`
   (`skinny/crates/bbnf-bench/src/direct_struct.rs:401-409`,
   `skinny/crates/bbnf-bench/src/direct_struct.rs:440-459`).
4. The rejected scalar-parent fold is not present in source. Current object/
   array hand Track 2 code still folds child digests through
   `digest.fold_child(self.value()?)`
   (`skinny/crates/bbnf-bench/src/direct_struct.rs:483-529`). A source search
   found no active `value_into_object`, `value_into_array`, or
   `fold_number_raw_known_scalar` helper in `direct_struct.rs`.
5. No benchmark-private parser admission or digest-as-product upgrade is made.
   `skinny/RESULTS.md` still records the selected W4 direct rows as
   `N-direct / NO-GO` (`skinny/RESULTS.md:13`, `skinny/RESULTS.md:23`,
   `skinny/RESULTS.md:32`) and the overall report remains `N-direct / NoGo`
   (`skinny/RESULTS.md:138`). W4 routes residual direct digest misses and keeps
   digest evidence guard-plane only
   (`restart/skinny/tranches/sk-v8/research/skv8-W4-plan.md:84-112`,
   `skinny/REDRESS.md:2723-2729`).
6. Rejected patch path is consistent and external to the admitted tree. The
   plan, HANDOFF, and REDRESS all name
   `/tmp/skv8-wave4-track2-scalar-fold-rejected.patch`
   (`restart/skinny/tranches/sk-v8/research/skv8-W4-plan.md:105-110`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:216-227`,
   `skinny/REDRESS.md:2723-2729`). The patch exists, `git apply --check`
   succeeds, and `git apply --numstat` reports `108  5` touching only
   `skinny/crates/bbnf-bench/src/direct_struct.rs`.
7. No source/RESULTS drift found. `git status --short`, `git diff --stat`, and
   `git diff --exit-code -- skinny/RESULTS.md
   skinny/crates/bbnf-bench/src/direct_struct.rs` returned clean. HEAD's W4 V3
   commit contains only V3 hardening artifacts, not source or
   `skinny/RESULTS.md` changes.

## Required Folds

None. CH5 should not require W4 close wording before V4 acceptance; that fold
belongs after the V4 consolidation accepts the unchanged state.
