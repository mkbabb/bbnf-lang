# SK-V8 W4 Hardening V4 CH1

Verdict: ACCEPT.

Confidence: 97%.

## Findings

1. No correctness blocker. The W4 disposition is a permitted fail-closed
   outcome: parse/direct waves may reject with REDRESS evidence, and W4's
   revert protocol keeps a triage report plus REDRESS for failed behavior
   attempts (`restart/skinny/tranches/sk-v8/SPEC.md:55`,
   `restart/skinny/tranches/sk-v8/SPEC.md:646`,
   `skinny/REDRESS.md:2694`).
2. The selected-row floor math matches the strict same-run digest gate.
   `apache_builds/direct_to_struct` uses Track 1 8306, Track 2 7796, sonic
   8852, so `ceil(8852 / 1.10) = 8048`: Track 1 passes and Track 2 misses
   (`skinny/RESULTS.md:13`, `skinny/RESULTS.md:56`).
   `numbers/direct_to_struct` uses 9773 / 6966 / 7953, so
   `ceil(7953 / 1.10) = 7230`: Track 1 passes and Track 2 misses
   (`skinny/RESULTS.md:32`, `skinny/RESULTS.md:75`).
   `random/direct_to_struct` uses 7751 / 6952 / 8141, so
   `ceil(8141 / 1.10) = 7401`: Track 1 passes and Track 2 misses
   (`skinny/RESULTS.md:23`, `skinny/RESULTS.md:66`). Each cited telemetry row
   records the comparator as `plane=digest`, `strictness=strict`, and
   `freshness=same-run-native`.
3. The rejected scalar-parent fold evidence supports rejection/routing.
   Apache passed the time gate because `95.347us <= 92.643us * 1.10 =
   101.907us`, but `random` failed because `569.57us > 463.26us * 1.10 =
   509.586us`, and `numbers` failed because `106.43us > 93.211us * 1.10 =
   102.532us` while regressing +6.3287% Track 2 time
   (`restart/skinny/tranches/sk-v8/research/wave-4-hardening/V1/HARDENING-W4-V1-CONSOLIDATED.md:51`,
   `skinny/REDRESS.md:2711`). SPEC and the W4 plan require every selected row
   to meet its floors or the patch is reverted and redressed
   (`restart/skinny/tranches/sk-v8/SPEC.md:628`,
   `restart/skinny/tranches/sk-v8/research/skv8-W4-plan.md:86`).
4. Source state matches the fail-closed claim. Current Track 1 still calls
   generated `runtime::generated_json::parse_direct`, while Track 2 still
   calls the independent hand parser
   (`skinny/crates/bbnf-bench/src/direct_struct.rs:401`,
   `skinny/crates/bbnf-bench/src/direct_struct.rs:408`). The current hand
   parser still builds child digests with `digest.fold_child(self.value()?)`
   for object and array values
   (`skinny/crates/bbnf-bench/src/direct_struct.rs:502`,
   `skinny/crates/bbnf-bench/src/direct_struct.rs:529`). The rejected patch
   would have replaced those sites with `value_into_object` /
   `value_into_array` and touched only `direct_struct.rs`
   (`/tmp/skv8-wave4-track2-scalar-fold-rejected.patch:39`,
   `/tmp/skv8-wave4-track2-scalar-fold-rejected.patch:50`,
   `/tmp/skv8-wave4-track2-scalar-fold-rejected.patch:58`).
5. No closure or W5 activation overclaim is present. HANDOFF says W4 has only
   a proposed rejection/routing disposition pending hardening convergence and
   that W5-W6 require prior wave dispositions and their own gates
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:8`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:10`). V3 explicitly made V4 the
   required unchanged challenge before W4 may close
   (`restart/skinny/tranches/sk-v8/research/wave-4-hardening/V3/HARDENING-W4-V3-CONSOLIDATED.md:35`).
   The pending pre-V4 HANDOFF posture is therefore not a CH1 reject.
6. The live artifacts agree on no admission: `RESULTS.md` still shows the
   selected rows as `N-direct / NO-GO` and the overall direct report as
   `N-direct / NoGo` (`skinny/RESULTS.md:13`, `skinny/RESULTS.md:23`,
   `skinny/RESULTS.md:32`, `skinny/RESULTS.md:138`); REDRESS 93 records no
   admitted source patch, no Lock 14 allowance, and unchanged `RESULTS.md`
   (`skinny/REDRESS.md:2717`).

## Required Folds

None. This CH1 ACCEPT does not by itself close W4; V4 still requires panel
consolidation under the two-consecutive-ACCEPT rule
(`restart/prompts/ORCHESTRATOR.md:118`).
