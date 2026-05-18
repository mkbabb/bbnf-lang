# SK-V8 W0 Hardening V5 CH6

Date: 2026-05-18.

## Verdict

ACCEPT.

Confidence: 96%.

W0 at `0c49fabd6d6facd136e1e69b8482aa4f239561ae`
(`fix(sk-v8-wave0): fold hardening V4 gate blockers`) is not a CH6 paper close.
The V4 blockers were folded into executable report/gate semantics, the committed
`SK-V8-open` report replays from the canonical and copied target roots, stale or
incoherent evidence fails closed, and the live packet does not let this CH6
acceptance mass-unlock W1-W6.

This is only the CH6 disposition for V5. V4 was rejected, so W0 still needs the
full V5 cohort result and another consecutive qualifying accept cycle before W0
can close under ORCHESTRATOR Section 3Z.

## Scope

Lens: end-to-end anti-paper-close review of W0 after the V4 rejection fold. I
reviewed ORCHESTRATOR Section 3W/3Z, V4 consolidation, live SPEC/SYNTHESIS/
HANDOFF/DISPATCH-PROMPT, `skinny/RESULTS.md`, the W0 report/gate implementation,
and the executable evidence around clean target roots, copied target roots,
stale evidence, sidecar contract wording, deferred validation semantics, and
downstream W1-W6 dispatch gating.

Out of scope: source remediation. This artifact is the only file edited.

## Evidence

- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v5-test-target RUSTFLAGS='-C target-cpu=native' cargo test -p bbnf-bench w0_ -- --nocapture`: PASS, including 12 W0 report tests and 8 gate-bin tests.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v5-test-target RUSTFLAGS='-C target-cpu=native' cargo test -p bbnf-bench`: PASS, 52 library tests plus 8 gate-bin tests.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`: PASS against committed `skinny/RESULTS.md`.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v5-test-target RUSTFLAGS='-C target-cpu=native' cargo xtask check-json`: PASS.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v5-test-target RUSTFLAGS='-C target-cpu=native' cargo xtask check-real-typed`: PASS.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v5-test-target RUSTFLAGS='-C target-cpu=native' cargo xtask check-conformance`: PASS, 21 valid fixtures accepted and 7 invalid fixtures rejected.
- Copied `/tmp/skv8-w0-target/criterion` into `/tmp/skv8-ch6-v5-copy.PAPtsU/criterion`, then ran `CARGO_TARGET_DIR=/tmp/skv8-ch6-v5-copy.PAPtsU RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`: PASS. This proves the W0 check path is not bound to the original target directory.
- Added `/tmp/skv8-ch6-v5-copy.PAPtsU/criterion/json_unvalidated_future/track1_generated/new/estimates.json`, then reran the copied-root gate: PASS. The unrelated W0-shaped Criterion group no longer perturbs the committed run id.
- Mutated `/tmp/skv8-ch6-v5-copy.PAPtsU/criterion/simd_structural_scan/canada_simd/metadata.toml` to replace the scalar parity hash with `deadbeef`, then reran the copied-root gate: expected FAIL with `canada SIMD metadata invalid: SIMD metadata parity hash does not match scalar scan`.
- `git diff --check`: PASS.
- `git status --short`: clean before writing this artifact.

## Findings

1. No blocker: V4's deferred validation hole is folded and tested.

   V4 rejected because a deferred W0 row could change `parse_utf8` to `none` and
   `escape_complete` to `n/a` while preserving `strictness=deferred` and
   `measured_validation_path=view-boundary`. The current validator now rejects
   non-strict rows unless `strictness=deferred`,
   `measured_validation_path=view-boundary`, `parse_utf8=view-boundary`, and
   `escape_complete=yes` all hold
   (`skinny/crates/bbnf-bench/src/report.rs:920`,
   `skinny/crates/bbnf-bench/src/report.rs:930`,
   `skinny/crates/bbnf-bench/src/report.rs:936`,
   `skinny/crates/bbnf-bench/src/report.rs:942`). The focused negative test
   mutates both fields and now expects rejection
   (`skinny/crates/bbnf-bench/src/report.rs:1640`).

2. No blocker: V4's run-id scope hole is folded and executable on a copied root.

   `RunFacts::probe` derives `run_id` from `criterion_fingerprint` using the
   loaded fixture names (`skinny/crates/bbnf-bench/src/bin/gate.rs:384`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:390`). The fingerprint includes
   only fixture-bound JSON groups and admitted W0 bench names, plus fixture-bound
   SIMD metadata (`skinny/crates/bbnf-bench/src/bin/gate.rs:673`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:735`). The unit test proves both
   `json_probes_*` and `json_unvalidated_future` estimates are excluded while a
   real fixture estimate changes the fingerprint
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:1766`). I reproduced the
   `json_unvalidated_future` case dynamically on `/tmp/skv8-ch6-v5-copy.PAPtsU`.

3. No blocker: the W0 report is gate-consumed, row-complete, and replayable.

   `Report::validate_sk_v8_w0()` enforces the exact
   `SK_V8_OPEN_BASELINE` row count, validates every row, rejects duplicate or
   unknown row ids, rejects missing baseline rows, and enforces Track 1/Track 2
   movement against the opening baseline (`skinny/crates/bbnf-bench/src/report.rs:493`,
   `skinny/crates/bbnf-bench/src/report.rs:501`,
   `skinny/crates/bbnf-bench/src/report.rs:508`,
   `skinny/crates/bbnf-bench/src/report.rs:514`). The gate validates schema and
   W0 semantics before writing or comparing `RESULTS.md`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:319`). The canonical
   `/tmp/skv8-w0-target` gate and copied-root gate both passed.

4. No blocker: stale and incoherent evidence rejects before acceptance.

   The gate validates Criterion metadata before row construction
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:45`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:57`). SIMD metadata is mandatory,
   fixture-bound, capture-bound, policy-bound, and parity-hash checked against a
   fresh scalar scan (`skinny/crates/bbnf-bench/src/bin/gate.rs:69`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1381`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1392`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1413`). The copied-root parity-hash
   mutation failed with the expected error.

5. No blocker: sidecar manifest contract wording now matches the implementation.

   The live SPEC says W0 has no structured same-run sidecar manifest and any
   `sidecar-same-run` claim rejects until a later accepted wave adds a parser and
   gate (`restart/skinny/tranches/sk-v8/SPEC.md:73`,
   `restart/skinny/tranches/sk-v8/SPEC.md:77`). The W0 exit gate repeats that
   populated sidecars are historical non-manifest planning signals and
   `sidecar-same-run` rejects without a structured manifest
   (`restart/skinny/tranches/sk-v8/SPEC.md:328`,
   `restart/skinny/tranches/sk-v8/SPEC.md:331`). The validator rejects
   sidecar-same-run comparators before strict admission
   (`skinny/crates/bbnf-bench/src/report.rs:1119`,
   `skinny/crates/bbnf-bench/src/report.rs:1143`), while native strict admission
   accepts only same-run native anchors with measured-row validation
   (`skinny/crates/bbnf-bench/src/gate.rs:151`,
   `skinny/crates/bbnf-bench/src/gate.rs:170`).

6. No blocker: no-behavior-change evidence is executable enough for CH6.

   W0 forbids parser, scanner, SIMD, asm, codegen behavior, product-plane
   behavior, and generated parser output changes (`restart/skinny/tranches/sk-v8/SPEC.md:333`).
   The W0 gate invokes Lock 14 before report generation
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:42`). Lock 14 freezes grammar,
   fixture, runtime, IR, passes, codegen, SIMD, generated-output, Track 2,
   parity, scan, materialization, and schema roots
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`) and checks dirty state
   plus the parent diff for those roots (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:399`).
   The full bbnf-bench suite, `check-json`, `check-real-typed`, `check-conformance`,
   and gate replay all passed.

7. No blocker: W0 closure cannot paper-unlock W1-W6.

   ORCHESTRATOR Section 3Z requires two consecutive cycles at at least 95%
   ACCEPT, zero open critical defects, and no orphan unresolved REVISE before a
   pass advances (`restart/prompts/ORCHESTRATOR.md:118`). V4 consolidation
   explicitly reset the consecutive-ACCEPT counter and kept W1-W6 blocked
   (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V4/HARDENING-W0-V4-CONSOLIDATED.md:61`).
   The live packet grants authority for W0 only and says W1-W6 remain blocked
   until W0 closes plus exact owner paths, row gates, challenge acceptance, entry
   gates, and dispatch authority (`restart/skinny/tranches/sk-v8/SPEC.md:31`,
   `restart/skinny/tranches/sk-v8/SPEC.md:36`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:92`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:226`). After a second future
   qualifying accept cycle, W0 can legitimately become the prerequisite for later
   waves; it still does not batch-dispatch W1-W6.

## Required Disposition If Rejected

Not applicable. I found no material CH6 blocker at `0c49fabd`.

## Residual Risks

- `restart/skinny/tranches/sk-v8/research/wave-0-plan.md` is a pre-redress plan
  artifact and still contains stale planning wording: `run_id=sk-v8-open:<git-sha>:<criterion-root>`
  and `parse_only` substrate-guard non-admission as `K`. I do not treat this as a
  CH6 blocker because the live SPEC/SYNTHESIS/HANDOFF/DISPATCH-PROMPT,
  `skinny/RESULTS.md`, and executable gate all use the current `criterion-fnv64`
  run id and `S`/`L` parse posture. A later docs-only cleanup should avoid
  confusing future agents.
- The Criterion capture metadata still records `bbnf_commit=0bd16f6d` while the
  folded target is `0c49fabd`. CH6 accepts this because all intervening folded
  changes are report/gate/hardening changes and Lock 14 freezes the behavior
  roots before gate acceptance. A future capture would be stronger if the manifest
  recorded the measured behavior commit and folded gate commit separately.
- This CH6 ACCEPT does not close W0. V5 must consolidate, and a second
  consecutive qualifying accept cycle is still required before W0 closure can
  become a legitimate entry predicate for later waves.
