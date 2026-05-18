# CH6 W0 V3 Hardening Challenge

Date: 2026-05-18.

Lens: CH6 - anti-paper-close. Focus: end-to-end gate evidence,
reproducibility, stale/incoherent evidence rejection, and dispatch/convergence
discipline after `61d5d30407d96ed176cc59e410f7884e30ed30ba`.

## Verdict

ACCEPT.

Confidence: 96%.

The W0 V3 fold is not a CH6 paper close. The live gate path now rejects stale
or incoherent evidence before report mutation, the committed W0 evidence root
replays, malformed comparator/sidecar/metadata cases are covered by executable
negative tests, and the prompt packet still blocks W1-W6 until W0 convergence
and later per-wave dispatch gates. This CH6 ACCEPT does not itself close W0;
the full V3 cohort and the two-consecutive-cycle convergence rule still apply.

## Commands

| Command | Result |
|---|---|
| `cargo test -p bbnf-bench` | PASS: 49 library tests, 6 gate-bin tests, doc tests. |
| `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results` | PASS: committed W0 evidence root re-rendered and matched `skinny/RESULTS.md`. |
| `cargo xtask gate-json --advisory` | Expected FAIL on my default local Criterion root: `Schema/W0 validation failure: json/twitter/parse_only/main Track 1 moved 63.03% from SK-V8-open baseline.` |

The failed default-root run is positive CH6 evidence: stale or different
Criterion artifacts cannot silently promote or paper-close W0. The exact
evidence root named by the reviewed commit body still passes.

## Findings

1. Gate mutation remains explicit and stale evidence fails closed. The bench gate
   only writes on `--update-results` / `--write-results`, rejects volatile probes
   combined with writes, validates W0 before writing/comparing, and exits invalid
   when the rendered report diverges from `skinny/RESULTS.md`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:20`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:29`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:308`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:319`). The xtask wrapper bounds the
   accepted gate flags (`skinny/xtask/src/main.rs:240`).

2. The W0 baseline is gate-consumed, not merely rendered. The report validator
   requires the full `SK_V8_OPEN_BASELINE` row count, unique known row ids, and
   +/-1.0% Track 1/Track 2 agreement with the opening baseline
   (`skinny/crates/bbnf-bench/src/report.rs:493`,
   `skinny/crates/bbnf-bench/src/report.rs:501`,
   `skinny/crates/bbnf-bench/src/report.rs:508`,
   `skinny/crates/bbnf-bench/src/report.rs:511`,
   `skinny/crates/bbnf-bench/src/report.rs:848`). That is the mechanism that
   rejected my stale default target.

3. V2's row-identity blocker is folded. W0 now parses `sk_v8.row_id` and binds
   it to the rendered corpus/workload (`skinny/crates/bbnf-bench/src/report.rs:962`),
   with a negative test for rendered identity mismatch
   (`skinny/crates/bbnf-bench/src/report.rs:1531`).

4. Native and sidecar comparator evidence is no longer producer-only prose.
   Strict admission consumes `parse_utf8`, escape completion, output plane,
   strictness, freshness, and measured-path evidence and rejects stale,
   historical, or absent freshness for strict claims
   (`skinny/crates/bbnf-bench/src/gate.rs:135`,
   `skinny/crates/bbnf-bench/src/gate.rs:151`,
   `skinny/crates/bbnf-bench/src/gate.rs:163`). Native comparators must have the
   workload-specific source, expected plane, strict freshness, no sidecar
   freshness, and finite Mbps (`skinny/crates/bbnf-bench/src/report.rs:1109`).
   Sidecars require DOM/strict/freshness-source coherence and reject
   `sidecar-same-run` without a structured manifest
   (`skinny/crates/bbnf-bench/src/report.rs:1059`,
   `skinny/crates/bbnf-bench/src/report.rs:1083`). Negative tests cover these
   mutations (`skinny/crates/bbnf-bench/src/report.rs:1605`,
   `skinny/crates/bbnf-bench/src/report.rs:1644`).

5. Criterion capture coherence is validated before report update. `gate-json`
   now reads metadata as fallible input, rejects missing/malformed rows, binds
   fixture hash/bytes and required benchmark specs, and rejects mixed
   host/profile/rustflags/target/sample captures before any write path can run
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:51`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1013`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1091`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1302`). Negative tests cover
   fixture mismatch, mixed capture, and missing required benches
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:1591`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1602`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1613`).

6. Lock 14/no-behavior-change evidence is live enough for CH6. The gate calls
   Lock 14 validation before report generation (`skinny/crates/bbnf-bench/src/bin/gate.rs:41`);
   frozen roots now include grammar/directive, runtime/parser/codegen/IR/pass,
   SIMD build/asm, and bench behavior surfaces
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`), and the freeze checks
   use git status/diff including the parent diff
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:399`). Tests cover dirty
   frozen roots and directive/asm coverage
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:562`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:571`).

7. W1-W6 dispatch remains blocked before convergence. ORCHESTRATOR requires two
   consecutive >=95% ACCEPT challenge cycles with zero critical/unresolved REVISE
   before advancement (`restart/prompts/ORCHESTRATOR.md:118`), and says the next
   pass does not dispatch until convergence (`restart/prompts/ORCHESTRATOR.md:123`).
   SK-V8 SPEC says current authority covers W0 only and W1-W6 remain blocked
   until W0 closes plus exact owner/row/challenge/orchestrator gates
   (`restart/skinny/tranches/sk-v8/SPEC.md:31`,
   `restart/skinny/tranches/sk-v8/SPEC.md:36`). DISPATCH-PROMPT repeats that
   W1-W6 cannot dispatch from the prompt alone and lists the conditional gates
   (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:90`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:94`). HANDOFF grants W0
   only and keeps W1-W6 blocked
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:225`).

## Residual Risk

No CH6 blocker. The only caution is operational: agents must use the committed
W0 evidence root or refresh under the W0 rules. A random local `target/criterion`
capture can and should fail, as observed, because the baseline delta gate treats
that as stale/incoherent evidence rather than retry room.

## Mandatory Fold Items

None for CH6.
