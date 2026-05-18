# SK-V8 W0 Hardening V11 CH6 - Anti-Paper-Close

Verdict: ACCEPT.

Confidence: 96%.

Target reviewed: `61d5cc3b4312883e026060174e876a0c18b34703`
(`fix(sk-v8-wave0): fold hardening V10 cost and metadata blockers`).

This is a CH6 anti-paper-close acceptance for the V11 fold, not W0 closure.
The orchestrator requires CH6 to reject self-reported "complete" or "verified"
claims without live evidence, and forbids deferral as closure
(`restart/prompts/ORCHESTRATOR.md:74`-`restart/prompts/ORCHESTRATOR.md:88`).
It also says hardening that is not folded is paper-hardening and requires two
consecutive qualifying ACCEPT cycles before advancement
(`restart/prompts/ORCHESTRATOR.md:112`-`restart/prompts/ORCHESTRATOR.md:121`).

## Evidence

1. V10 CH4 blockers were concrete and V11 addressed them with code, not prose.
   V10 rejected the fold for exceeding the live `<=120` post-V6 fold cap and
   for accepting empty `arch`, `cpu`, `os`, and `simd` metadata
   (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:45`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:53`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:94`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:97`).
   The V10 consolidation required a V11 fold for the footprint, non-empty
   metadata validation, focused negatives, and preservation of the accepted
   V10 evidence
   (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:29`-`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:45`).

2. The V11 cost claim has live diff evidence. `git show --stat --oneline
   61d5cc3b` reports one touched file,
   `skinny/crates/bbnf-bench/src/report.rs`, with `58 insertions(+)` and
   `109 deletions(-)`. `git diff --numstat 00c3485a..61d5cc3b --
   skinny/crates/bbnf-bench/src/report.rs` reports `118 13`, putting the
   V10 telemetry-consumption footprint under the `<=120` insertion cap. The
   target diff is report-local: `git diff --name-only HEAD^..HEAD` printed only
   `skinny/crates/bbnf-bench/src/report.rs`.

3. Empty metadata no longer has a paper-close path. The W0 validator rejects
   empty required text before semantic validation
   (`skinny/crates/bbnf-bench/src/report.rs:275`-`skinny/crates/bbnf-bench/src/report.rs:321`),
   binds every row to the frozen W0 run id
   (`skinny/crates/bbnf-bench/src/report.rs:330`-`skinny/crates/bbnf-bench/src/report.rs:341`),
   and now requires non-empty `host_triple` host, `arch`, and `cpu` plus
   non-empty feature `arch`, `os`, and `simd`
   (`skinny/crates/bbnf-bench/src/report.rs:1020`-`skinny/crates/bbnf-bench/src/report.rs:1062`).
   The focused baseline test mutates both formerly-open empty cases:
   `host_triple = "aarch64-apple-darwin;arch=;cpu="` and
   `feature_mask = "arch=;os=;simd=;target_cpu=native"`
   (`skinny/crates/bbnf-bench/src/report.rs:1960`-`skinny/crates/bbnf-bench/src/report.rs:2070`).

4. W0 telemetry consumption is executable and row-bound. The current
   `skinny/RESULTS.md` has the SK-V8 W0 manifest and first W0 row carrying the
   frozen run id, structured build/host/feature metadata, `gate_only`, and
   comparator evidence (`skinny/RESULTS.md:44`, `skinny/RESULTS.md:46`,
   `skinny/RESULTS.md:48`). The note says `gate-json` consumes the manifest and
   that C++ sidecars are historical or absent, not W0 strict anchors
   (`skinny/RESULTS.md:141`). Live row audit:
   `awk 'BEGIN{rows=0;gate=0;run=0} /^\| json\//{rows++; if ($0 ~ /\| gate_only \|/) gate++; if ($0 ~ /sk-v8-open:criterion-fnv64-9a37562ed3d0383a/) run++} END{printf "manifest_rows=%d gate_only=%d frozen_run_id=%d\n", rows, gate, run}' skinny/RESULTS.md`
   printed `manifest_rows=38 gate_only=38 frozen_run_id=38`.

5. Focused and full tests passed from the skinny workspace:
   `env CARGO_TARGET_DIR=/tmp/skv8-ch6-v11-test-target cargo test -p bbnf-bench w0_ -- --nocapture`
   passed 12 report W0 tests and 8 gate-bin W0 tests.
   `env CARGO_TARGET_DIR=/tmp/skv8-ch6-v11-test-target cargo test -p bbnf-bench strict -- --nocapture`
   passed 5 strict tests.
   `env CARGO_TARGET_DIR=/tmp/skv8-ch6-v11-test-target cargo test -p bbnf-bench sidecar_same_run -- --nocapture`
   passed 1 sidecar same-run test.
   `env CARGO_TARGET_DIR=/tmp/skv8-ch6-v11-test-target cargo test -p bbnf-bench`
   passed 52 library tests, 8 gate-bin tests, and doc tests.

6. The live gate and xtask checks passed. `env CARGO_TARGET_DIR=/tmp/skv8-w0-target
   RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
   exited 0 against the existing W0 Criterion root. `env
   CARGO_TARGET_DIR=/tmp/skv8-ch6-v11-test-target cargo xtask check-json` exited
   0. `env CARGO_TARGET_DIR=/tmp/skv8-ch6-v11-test-target cargo xtask
   check-real-typed` exited 0. `env
   CARGO_TARGET_DIR=/tmp/skv8-ch6-v11-test-target cargo xtask check-conformance`
   exited 0 and printed `conformance: 21 valid fixtures accepted; 7 invalid
   fixtures rejected`.

7. Dynamic Criterion probes were live, not self-report. On a temp copy of
   `/tmp/skv8-w0-target/criterion`, injecting non-W0 groups
   `json_unvalidated_future/track1_generated/new/estimates.json` and
   `json_probes_twitter/host_call_dispatch_overhead/new/estimates.json`, then
   running `gate-json --advisory --check-results`, exited 0 and printed
   `PASS non-W0 injected Criterion groups ignored; manifest_rows=38
   run_id_mentions=38`. On a fresh temp copy, mutating the admitted W0 file
   `json_twitter/track1_generated/new/estimates.json` caused
   `gate-json --advisory --check-results` to fail as expected with
   `Schema/W0 validation failure: json/twitter/parse_only/main run_id moved
   from SK-V8-open baseline sk-v8-open:criterion-fnv64-9a37562ed3d0383a to
   sk-v8-open:criterion-fnv64-6679789d31eeb5c1.`

8. Frozen behavior-surface and diff checks are live. Lock 14 validates the
   frozen roots and rejects dirty frozen surfaces
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:336`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:406`).
   `git diff --exit-code 0bd16f6d..HEAD -- <frozen W0 behavior roots>` exited
   0, and `git diff --exit-code HEAD^..HEAD -- <frozen W0 behavior roots>`
   exited 0. `git diff --check 3a9fa326..61d5cc3b --
   skinny/crates/bbnf-bench/src/report.rs` exited 0, and `git diff --check`
   also exited 0.

## Blockers

None for CH6. I found no V11 close claim that depends only on self-report, and
no deferral is being used to close the V10 CH4 blockers.

## Required Fold If Rejecting

Not applicable.

## Residual Risk

This acceptance does not admit W0 or dispatch W1-W6. V10 reset the consecutive
ACCEPT counter and the V10 consolidation explicitly keeps W1-W6 blocked until
two qualifying challenge cycles complete
(`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:64`-`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:67`).
I did not run a fresh full Criterion benchmark capture; CH6 verified the frozen
W0 root, gate replay, mutation behavior, tests, xtask checks, and frozen
behavior-surface diffs required for the anti-paper-close lens.
