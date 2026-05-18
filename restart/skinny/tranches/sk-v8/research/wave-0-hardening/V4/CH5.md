# CH5 W0 V4 Hardening Challenge

## Verdict

ACCEPT.

Confidence: 96%.

## Scope

Reviewed target: `077aadad8aacf95e3250ec157f30ba6ab873bf6b`
(`fix(sk-v8-wave0): fold hardening V3 gate blockers`).

Lens: CH5 hidden coupling, with adversarial focus on live
`SYNTHESIS.md` / `SPEC.md` / `HANDOFF.md` / `DISPATCH-PROMPT.md`
consistency, W1-W6 blocking until W0 close, row counts and floors versus
`skinny/RESULTS.md`, retained candidate lists versus archived S-P3 numeric
seeds, profile/hot-leaf routing, sidecar freshness, row identity, and substrate
surface/cardinality.

## Evidence

- ORCHESTRATOR CH5 requires no hidden parallel substrate, sidecar producer,
  renamed-scanner Lock 1 violation, or Track 1/Track 2 dishonesty
  (`restart/prompts/ORCHESTRATOR.md:83`-`restart/prompts/ORCHESTRATOR.md:88`).
  ORCHESTRATOR convergence still requires two consecutive >=95% ACCEPT cycles
  with no open critical defects before the next pass/wave advances
  (`restart/prompts/ORCHESTRATOR.md:104`-`restart/prompts/ORCHESTRATOR.md:123`).
- The live packet consistently grants W0 only and keeps W1-W6 conditional:
  `SPEC.md` says current dispatch authority covers W0 only and W1-W6 remain
  blocked until W0 close plus exact owner paths, row gates, challenge, and
  orchestrator/user dispatch (`restart/skinny/tranches/sk-v8/SPEC.md:31`-
  `restart/skinny/tranches/sk-v8/SPEC.md:38`); `HANDOFF.md` grants W0 only and
  keeps W1-W6 blocked (`restart/skinny/tranches/sk-v8/HANDOFF.md:5`-
  `restart/skinny/tranches/sk-v8/HANDOFF.md:7`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:226`-
  `restart/skinny/tranches/sk-v8/HANDOFF.md:229`); `DISPATCH-PROMPT.md` repeats
  that W1-W6 cannot dispatch from the prompt alone
  (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:6`-
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:9`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:90`-
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:106`).
- Row state matches across live packet and RESULTS: 38 main rows; 16
  `parse_only` `S / NO-GO`, 1 `parse_only` `L / NO-GO`, 14 direct
  `N-direct / NO-GO`, 3 direct `A / GO`, and 4 real typed `A / GO`
  (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:28`-
  `restart/skinny/tranches/sk-v8/SYNTHESIS.md:45`,
  `restart/skinny/tranches/sk-v8/SPEC.md:150`-
  `restart/skinny/tranches/sk-v8/SPEC.md:189`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:31`-
  `restart/skinny/tranches/sk-v8/HANDOFF.md:48`,
  `skinny/RESULTS.md:3`-`skinny/RESULTS.md:42`).
- The W0 manifest is present and gate-shaped for every current row
  (`skinny/RESULTS.md:44`-`skinny/RESULTS.md:85`). The report validator requires
  exactly the W0 baseline row count, rejects duplicate/unknown row ids, validates
  Track 1 and Track 2 against `SK-V8-open`, and requires every baseline row
  (`skinny/crates/bbnf-bench/src/report.rs:493`-
  `skinny/crates/bbnf-bench/src/report.rs:519`).
- Profile and hot-leaf routing are row-bound rather than placeholder prose: W0
  rejects non-`criterion-slope-profile` artifact shapes and requires hot leaf to
  equal the row's profile artifact plus row id
  (`skinny/crates/bbnf-bench/src/report.rs:881`-
  `skinny/crates/bbnf-bench/src/report.rs:906`).
- The V3 run-id blocker is folded: the fingerprint now includes only W0
  Criterion inputs accepted by `is_w0_criterion_input`, excluding de-rendered
  volatile probe groups and unrelated Criterion files
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:668`-
  `skinny/crates/bbnf-bench/src/bin/gate.rs:738`). The regression test proves a
  probe estimate does not perturb the fingerprint while a main W0 estimate does
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:1748`-
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1771`).
- Sidecars are now historical or absent W0 planning signals, not hidden same-run
  anchors. The live SPEC says W0 has no structured sidecar same-run manifest and
  rejects `sidecar-same-run` until a later accepted manifest parser/gate exists
  (`restart/skinny/tranches/sk-v8/SPEC.md:73`-
  `restart/skinny/tranches/sk-v8/SPEC.md:81`). The validator rejects
  `sidecar-same-run` without a structured manifest and requires exact
  historical/absence source shapes (`skinny/crates/bbnf-bench/src/report.rs:1101`-
  `skinny/crates/bbnf-bench/src/report.rs:1149`).
- Candidate-list versus seed-floor debt is routed. The live SPEC archives the
  remaining W2/W3/W4 numeric seed tables as pre-W0 planning data, keeps only the
  retained candidate row lists, and requires later plans to recompute thresholds
  from current `SK-V8-open` telemetry before challenge or redress
  (`restart/skinny/tranches/sk-v8/SPEC.md:179`-
  `restart/skinny/tranches/sk-v8/SPEC.md:189`).
- Executed evidence:
  - `cargo test -p bbnf-bench`: PASS, 51 library tests and 8 gate-bin tests.
  - `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`: PASS; committed W0 evidence root re-rendered and matched `skinny/RESULTS.md`.
  - `cargo xtask check-json`: PASS.
  - `cargo xtask check-real-typed`: PASS.
  - `cargo xtask check-conformance`: PASS, 21 valid fixtures accepted and 7 invalid fixtures rejected.
  - `git diff --check`: PASS for tracked workspace diffs;
    `git diff --no-index --check /dev/null restart/skinny/tranches/sk-v8/research/wave-0-hardening/V4/CH5.md`
    emitted no whitespace errors for this artifact.
  - A direct parse of `skinny/RESULTS.md` returned 38 main rows with counts
    matching the live packet and current floors for the four typed GO rows plus
    three direct GO guard rows.

## Findings

No material CH5 blocker found.

1. Accepted: W0 does not smuggle a second substrate or sidecar producer into the
   opening packet. Current rows render `borrowed_view_over_offset_tape`,
   `sink_only_digest`, or `typed_direct_projection` with cardinality `one` or
   `zero_or_inert` in the manifest (`skinny/RESULTS.md:48`-
   `skinny/RESULTS.md:85`), while the live SPEC keeps W3 constrained to one
   retained `Tape` and blocks sidecar/parallel substrate routes
   (`restart/skinny/tranches/sk-v8/SPEC.md:191`-
   `restart/skinny/tranches/sk-v8/SPEC.md:212`,
   `restart/skinny/tranches/sk-v8/SPEC.md:531`-
   `restart/skinny/tranches/sk-v8/SPEC.md:553`).
2. Accepted: row counts, GO floors, and direct guard floors are packet-consistent
   with the current W0 RESULTS state. The live SPEC now lists the four current
   real-typed GO floors and only the three current direct GO guard floors
   (`restart/skinny/tranches/sk-v8/SPEC.md:162`-
   `restart/skinny/tranches/sk-v8/SPEC.md:177`), matching
   `skinny/RESULTS.md:7`, `skinny/RESULTS.md:9`,
   `skinny/RESULTS.md:18`, `skinny/RESULTS.md:21`,
   `skinny/RESULTS.md:27`, `skinny/RESULTS.md:28`, and
   `skinny/RESULTS.md:38`.
3. Accepted: later-wave dispatch remains blocked. This CH5 ACCEPT is only one
   V4 challenge result; it does not close W0 or authorize W1-W6 under
   ORCHESTRATOR convergence (`restart/prompts/ORCHESTRATOR.md:118`-
   `restart/prompts/ORCHESTRATOR.md:123`) or the packet gates cited above.

## Required Disposition If Rejected

Not rejected. No CH5 fold is required. The orchestrator should count this as one
V4 CH5 ACCEPT only, then wait for the full V4 cohort and consolidation. W1-W6
must remain blocked until W0 obtains the required challenge convergence and any
later wave receives its own fresh plan/challenge/dispatch authority.

## Residual Risks

- The default local `skinny/target` Criterion cache failed closed for me because
  its SIMD metadata was from an unsupported stale capture policy. This is not a
  packet blocker because the committed W0 evidence root replays with the
  documented `/tmp/skv8-w0-target` and native RUSTFLAGS command, but future
  operators must use the committed evidence root or regenerate a coherent W0
  capture.
- P3-A/P3-B/P3-C still contain stale pre-W0 numeric seed prose. The live packet
  routes those numeric seeds as archived planning data and requires recomputation
  from `SK-V8-open`, so this is a residual citation risk rather than a W0
  blocker.
- The W0 "hot leaf" is a row-bound criterion-slope routing token, not a samply
  symbol-path proof. That is acceptable for W0 gate telemetry, but W3 must not
  treat it as sufficient production hot-leaf attribution; W3 still needs fresh
  measured-path profile evidence and challenge acceptance.
