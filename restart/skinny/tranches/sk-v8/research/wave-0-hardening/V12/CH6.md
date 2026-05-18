# SK-V8 W0 Hardening V12 CH6 - Anti-Paper-Close

Verdict: ACCEPT.

Confidence: 97%.

Target reviewed: `61d5cc3b4312883e026060174e876a0c18b34703`
(`fix(sk-v8-wave0): fold hardening V10 cost and metadata blockers`).

Current HEAD is `b34dbeb8`, but the W0 implementation/report surface is
unchanged from the target: `git diff --exit-code
61d5cc3b4312883e026060174e876a0c18b34703..HEAD -- skinny/RESULTS.md
skinny/crates/bbnf-bench/src/report.rs skinny/crates/bbnf-bench/src/gate.rs
skinny/crates/bbnf-bench/src/bin/gate.rs
skinny/crates/bbnf-bench/src/lock14_baseline.rs skinny/xtask/src/main.rs`
exited 0. The only post-target commit in scope is the V11 hardening archive.

## Basis

CH6 rejects self-reported "complete", "wired", or "verified" claims unless
they are backed by live evidence; it also rejects deferral as closure
(`restart/prompts/ORCHESTRATOR.md:74`-`restart/prompts/ORCHESTRATOR.md:88`).
The orchestrator requires folded hardening and two consecutive qualifying
ACCEPT cycles before advancement
(`restart/prompts/ORCHESTRATOR.md:112`-`restart/prompts/ORCHESTRATOR.md:121`).
V11 is only the first qualifying ACCEPT cycle after the V10 reset
(`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V11/HARDENING-W0-V11-CONSOLIDATED.md:14`-`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V11/HARDENING-W0-V11-CONSOLIDATED.md:17`).
Therefore this CH6 acceptance does not by itself close W0. W0 can close only if
all six V12 lanes accept and the V12 consolidation records zero critical
defects and no unresolved REVISE.

## Evidence

1. V10's paper-close blockers were concrete and V11 folded them in code. V10
   CH4 rejected the prior fold for exceeding the live post-V6 W0 cap and for
   allowing empty `arch`, `cpu`, `os`, and `simd` metadata
   (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:45`-`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:70`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:92`-`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:118`).
   V10 consolidation required a footprint fold, non-empty metadata validation,
   focused negatives, and preservation of the accepted evidence
   (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:29`-`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:45`).
   The target diff is report-local and under the accepted cap:
   `git diff --numstat 00c3485a..61d5cc3b4312883e026060174e876a0c18b34703
   -- skinny/crates/bbnf-bench/src/report.rs` printed `118 13`, and
   `git show --stat --oneline 61d5cc3b4312883e026060174e876a0c18b34703 --
   skinny/crates/bbnf-bench/src/report.rs` reported one touched file with
   `58 insertions(+)` and `109 deletions(-)`.

2. The live W0 validator consumes the telemetry instead of trusting prose. It
   rejects missing required W0 fields, pins row identity and run id, validates
   profile artifact/hot leaf shape, and requires `gate_only`
   (`skinny/crates/bbnf-bench/src/report.rs:275`-`skinny/crates/bbnf-bench/src/report.rs:356`).
   It consumes CostFacts/redress/Track 2 sentinels, exact build flags,
   non-empty host `arch`/`cpu`, non-empty feature `arch`/`os`/`simd`, and exact
   W0 substrate tuples
   (`skinny/crates/bbnf-bench/src/report.rs:1007`-`skinny/crates/bbnf-bench/src/report.rs:1070`).
   Focused tests include the formerly open empty metadata cases and run-id drift
   cases (`skinny/crates/bbnf-bench/src/report.rs:1960`-`skinny/crates/bbnf-bench/src/report.rs:2070`).

3. The gate path is an executable same-wave consumer. `cargo xtask gate-json`
   shells to the bench gate and rejects failed gate status
   (`skinny/xtask/src/main.rs:240`-`skinny/xtask/src/main.rs:270`). The gate
   reads the Criterion root, validates Lock 14, builds run facts, validates W0,
   and compares rendered output to `RESULTS.md`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:37`-`skinny/crates/bbnf-bench/src/bin/gate.rs:43`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:319`-`skinny/crates/bbnf-bench/src/bin/gate.rs:338`).
   Run id is derived from Criterion inputs
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:383`-`skinny/crates/bbnf-bench/src/bin/gate.rs:393`),
   and the fingerprint filter admits only W0 baseline inputs while excluding
   probes/future groups
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:673`-`skinny/crates/bbnf-bench/src/bin/gate.rs:743`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1769`-`skinny/crates/bbnf-bench/src/bin/gate.rs:1805`).

4. Current `skinny/RESULTS.md` has row-bound W0 telemetry. The manifest header
   and first row carry `SK-V8-open`, frozen run id, build/host/feature metadata,
   CostFacts/redress sentinels, substrate tuple, `gate_only`, and comparator
   evidence (`skinny/RESULTS.md:44`-`skinny/RESULTS.md:48`). The report note
   states that `gate-json` consumes the manifest and that C++ sidecars are
   historical or absent, not W0 strict anchors (`skinny/RESULTS.md:141`).
   Live row audit:
   `awk 'BEGIN{rows=0;gate=0;run=0;sk=0} /^\| json\//{rows++; if ($0 ~ /\|
   gate_only \|/) gate++; if ($0 ~ /sk-v8-open:criterion-fnv64-9a37562ed3d0383a/)
   run++; if ($0 ~ /\| SK-V8-open \|/) sk++} END{printf
   "manifest_rows=%d gate_only=%d sk_v8_open=%d frozen_run_id=%d\n", rows,
   gate, sk, run}' skinny/RESULTS.md` printed `manifest_rows=38 gate_only=38
   sk_v8_open=38 frozen_run_id=38`.

5. Focused and full tests passed from the skinny workspace:
   `env CARGO_TARGET_DIR=/tmp/skv8-ch6-v12-test-target cargo test -p
   bbnf-bench w0_ -- --nocapture` passed 12 report W0 tests and 8 gate-bin W0
   tests; `env CARGO_TARGET_DIR=/tmp/skv8-ch6-v12-test-target cargo test -p
   bbnf-bench strict -- --nocapture` passed 5 strict tests; `env
   CARGO_TARGET_DIR=/tmp/skv8-ch6-v12-test-target cargo test -p bbnf-bench
   sidecar_same_run -- --nocapture` passed 1 sidecar same-run test; and `env
   CARGO_TARGET_DIR=/tmp/skv8-ch6-v12-test-target cargo test -p bbnf-bench`
   passed 52 library tests, 8 gate-bin tests, and doc tests.

6. Gate replay and xtask checks passed. `env CARGO_TARGET_DIR=/tmp/skv8-w0-target
   RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory
   --check-results` exited 0 against the captured W0 Criterion root. `env
   CARGO_TARGET_DIR=/tmp/skv8-ch6-v12-test-target cargo xtask check-json` exited
   0; `env CARGO_TARGET_DIR=/tmp/skv8-ch6-v12-test-target cargo xtask
   check-real-typed` exited 0; `env
   CARGO_TARGET_DIR=/tmp/skv8-ch6-v12-test-target cargo xtask check-conformance`
   exited 0 and printed `conformance: 21 valid fixtures accepted; 7 invalid
   fixtures rejected`. These xtask checks are the generated JSON, generated
   real-typed, and conformance gates
   (`skinny/xtask/src/main.rs:127`-`skinny/xtask/src/main.rs:149`).

7. Dynamic Criterion probes were live. On a copied Criterion root, injecting
   `json_unvalidated_future/track1_generated/new/estimates.json` and
   `json_probes_twitter/host_call_dispatch_overhead/new/estimates.json`, then
   running `cargo xtask gate-json --advisory --check-results`, exited 0 and
   printed `PASS non-W0 injected Criterion groups ignored; manifest_rows=38
   run_id_mentions=38`. On a fresh copy, appending a newline to admitted W0 file
   `json_twitter/track1_generated/new/estimates.json` caused
   `cargo xtask gate-json --advisory --check-results` to fail as expected with
   exit code 1 and `Schema/W0 validation failure:
   json/twitter/parse_only/main run_id moved from SK-V8-open baseline
   sk-v8-open:criterion-fnv64-9a37562ed3d0383a to
   sk-v8-open:criterion-fnv64-a5417170e7ed57aa.`

8. Frozen behavior-surface and diff checks are live. Lock 14 names frozen roots
   and validates both dirty frozen status and frozen diffs
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:405`);
   its tests reject dirty frozen roots and cover directive/asm surfaces
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:562`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:580`).
   `git diff --exit-code 0bd16f6d..HEAD -- <Lock14 frozen roots>` exited 0,
   and `git diff --exit-code HEAD^..HEAD -- <Lock14 frozen roots>` exited 0.
   `git diff --check
   3a9fa326..61d5cc3b4312883e026060174e876a0c18b34703 --
   skinny/crates/bbnf-bench/src/report.rs`, `git diff --check
   61d5cc3b4312883e026060174e876a0c18b34703..HEAD --`, and `git diff --check`
   all exited 0.

## Blockers

None for CH6. I found no V12 close claim backed only by self-report, and no
future-phase deferral is being used to close the V10 CH4 blockers.

## Required Fold If Rejecting

Not applicable.

## Residual Risk

This is CH6 lane acceptance only. W0 can close only if CH1-CH6 all ACCEPT in
V12 and the consolidation records the second consecutive qualifying cycle after
V11. I did not run a fresh full Criterion benchmark capture; CH6 reverified the
existing W0 Criterion root, gate replay, dynamic mutation behavior, tests,
xtask checks, row counts, and frozen behavior-surface diffs required for the
anti-paper-close lens.
