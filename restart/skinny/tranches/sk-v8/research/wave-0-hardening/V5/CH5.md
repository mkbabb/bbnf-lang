# CH5 W0 V5 Hardening Challenge

## Verdict

ACCEPT.

Confidence: 95%.

## Scope

Reviewed target: `0c49fabd6d6facd136e1e69b8482aa4f239561ae`
(`fix(sk-v8-wave0): fold hardening V4 gate blockers`).

Lens: CH5 hidden coupling, with adversarial focus on packet/RESULTS
consistency, W1-W6 blocking until W0 close, row counts and floors versus
`skinny/RESULTS.md`, candidate lists versus archived S-P3 numeric seeds,
LOC/cap posture after the V4 fold, sidecar freshness, profile/hot-leaf routing,
substrate/cardinality, and Track 1/Track 2 independence.

## Evidence

- ORCHESTRATOR CH5 rejects hidden parallel substrates, sidecar producers,
  renamed-scanner Lock 1 violations, and Track 1 / Track 2 dishonesty
  (`restart/prompts/ORCHESTRATOR.md:74`-`restart/prompts/ORCHESTRATOR.md:88`).
  ORCHESTRATOR Section 3Z still requires two consecutive challenge cycles at
  >=95% ACCEPT, no open critical defects, and no orphan unresolved REVISE before
  the pass/wave advances (`restart/prompts/ORCHESTRATOR.md:118`-
  `restart/prompts/ORCHESTRATOR.md:127`).
- V4 consolidated as REJECT because CH1 and CH4 found material W0 blockers:
  deferred-row `parse_utf8` / `escape_complete` drift was not consumed, and
  `run_id` still fingerprinted unvalidated W0-shaped Criterion groups
  (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V4/HARDENING-W0-V4-CONSOLIDATED.md:20`-
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V4/HARDENING-W0-V4-CONSOLIDATED.md:47`).
- The current fold target is exactly `0c49fabd`. `git show --stat 0c49fabd`
  reports only `skinny/crates/bbnf-bench/src/bin/gate.rs` and
  `skinny/crates/bbnf-bench/src/report.rs`, with 83 insertions and 15 deletions.
  That stays inside W0 report/gate owner scope and below the <=350
  report/gate/schema/test/doc LOC cap in SPEC Section 3
  (`restart/skinny/tranches/sk-v8/SPEC.md:288`-
  `restart/skinny/tranches/sk-v8/SPEC.md:347`).
- The CH1 fold is executable: non-strict W0 rows now require
  `strictness=deferred`, `measured_validation_path=view-boundary`,
  `parse_utf8=view-boundary`, and `escape_complete=yes` before returning OK
  (`skinny/crates/bbnf-bench/src/report.rs:920`-
  `skinny/crates/bbnf-bench/src/report.rs:944`). The negative test
  `w0_rejects_deferred_validation_semantic_drift` mutates `parse_utf8=none` and
  `escape_complete=n/a` and expects rejection
  (`skinny/crates/bbnf-bench/src/report.rs:1639`-
  `skinny/crates/bbnf-bench/src/report.rs:1663`).
- The CH4 fold is executable: `RunFacts::probe` now passes the loaded fixture
  set into `criterion_fingerprint`, and `is_w0_criterion_input` accepts only
  fixture-backed `json_<corpus>` groups plus the Canada SIMD row
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:383`-
  `skinny/crates/bbnf-bench/src/bin/gate.rs:393`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:673`-
  `skinny/crates/bbnf-bench/src/bin/gate.rs:752`). The regression test proves
  both a volatile probe and `json_unvalidated_future` leave the fingerprint
  unchanged while a real W0 estimate changes it
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:1765`-
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1795`).
- Live packet dispatch posture is consistent: SPEC grants current dispatch
  authority for W0 only and keeps W1-W6 conditional on W0 close plus exact
  owner paths, row gates, challenge where required, and orchestrator/user
  dispatch (`restart/skinny/tranches/sk-v8/SPEC.md:31`-
  `restart/skinny/tranches/sk-v8/SPEC.md:38`,
  `restart/skinny/tranches/sk-v8/SPEC.md:778`-
  `restart/skinny/tranches/sk-v8/SPEC.md:785`). HANDOFF and DISPATCH-PROMPT
  repeat the same lock (`restart/skinny/tranches/sk-v8/HANDOFF.md:5`-
  `restart/skinny/tranches/sk-v8/HANDOFF.md:7`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:226`-
  `restart/skinny/tranches/sk-v8/HANDOFF.md:229`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:6`-
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:9`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:90`-
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:106`).
- Row state matches across live packet and RESULTS. The live packet says the
  W0-rendered table has 16 `parse_only` `S / NO-GO`, 1 `parse_only` `L /
  NO-GO`, 3 direct `A / GO`, 14 direct `N-direct / NO-GO`, and 4
  `real_typed_struct` `A / GO` rows (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:28`-
  `restart/skinny/tranches/sk-v8/SYNTHESIS.md:45`,
  `restart/skinny/tranches/sk-v8/SPEC.md:148`-
  `restart/skinny/tranches/sk-v8/SPEC.md:189`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:31`-
  `restart/skinny/tranches/sk-v8/HANDOFF.md:48`). A direct `awk` parse of
  `skinny/RESULTS.md` returned 38 main rows with exactly those family/outcome
  counts.
- W2 current GO floors and direct guard floors match the current RESULTS surface
  at material gate level: the four real typed GO rows are
  `twitter/update_center/mesh/marine_ik` (`skinny/RESULTS.md:7`,
  `skinny/RESULTS.md:18`, `skinny/RESULTS.md:21`,
  `skinny/RESULTS.md:28`), and the three direct GO guards are
  `citm_catalog/marine_ik/unicode_basic` (`skinny/RESULTS.md:9`,
  `skinny/RESULTS.md:27`, `skinny/RESULTS.md:38`). SPEC lists those same four
  typed rows and three direct guards (`restart/skinny/tranches/sk-v8/SPEC.md:162`-
  `restart/skinny/tranches/sk-v8/SPEC.md:177`).
- Candidate-list debt is routed, not live floor authority. SPEC explicitly
  archives the remaining W2/W3/W4 numeric seed tables as pre-W0 planning data,
  retains only the candidate row lists, and requires later plans to recompute
  thresholds from current `SK-V8-open` telemetry before challenge or redress
  (`restart/skinny/tranches/sk-v8/SPEC.md:179`-
  `restart/skinny/tranches/sk-v8/SPEC.md:189`). DISPATCH-PROMPT limits W2
  candidates to that retained list unless a later accepted S-P3 revision expands
  it (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:117`-
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:123`).
- W0 profile/hot-leaf routing is row-bound and gate-consumed. The validator
  rejects non-`criterion-slope-profile` artifact shapes and requires hot leaf to
  equal the row profile artifact plus row id
  (`skinny/crates/bbnf-bench/src/report.rs:881`-
  `skinny/crates/bbnf-bench/src/report.rs:906`). A direct table parse confirmed
  every main row hot leaf has the row-bound `criterion-slope-profile` shape, and
  the manifest has 38 rows with `gate_only` consumer, `baseline` delta,
  `independent_verified` Track 2, and no retained side substrate.
- Executed evidence:
  - `cargo test -p bbnf-bench`: PASS, 52 library tests and 8 gate-bin tests.
  - `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`: PASS; committed W0 evidence root re-rendered and matched `skinny/RESULTS.md`.
  - `cargo xtask check-json`: PASS.
  - `cargo xtask check-real-typed`: PASS.
  - `cargo xtask check-conformance`: PASS, 21 valid fixtures accepted and 7
    invalid fixtures rejected.
  - `git diff --check`: PASS.
  - Frozen behavior roots checked with `git diff --name-only 0c49fabd -- ...`:
    no parser, runtime/tape, SIMD, codegen, generated/product, Track 2, parity,
    scan, or materialization path diffs.

## Findings

1. No material CH5 blocker found. The V4 fold closes the two V4 gate-honesty
   blockers with executable negative tests and keeps the implementation slice
   inside W0 report/gate owner paths and cap.

2. Accepted: W0 does not smuggle a second substrate, sidecar producer, or
   `tape_vs_tape` production consumer. The manifest keeps current parse rows on
   `borrowed_view_over_offset_tape` with cardinality `one`, direct/typed rows on
   `sink_only_digest` or `typed_direct_projection` with `zero_or_inert`, and
   `same_wave_consumer_class=gate_only`. The live SPEC continues to block
   sidecar/parallel substrate surfaces, `UnionTape`, parser-owned cursors/facts,
   and telemetry-only rows as W3 production consumers
   (`restart/skinny/tranches/sk-v8/SPEC.md:191`-
   `restart/skinny/tranches/sk-v8/SPEC.md:212`,
   `restart/skinny/tranches/sk-v8/SPEC.md:525`-
   `restart/skinny/tranches/sk-v8/SPEC.md:558`,
   `restart/skinny/tranches/sk-v8/SPEC.md:731`-
   `restart/skinny/tranches/sk-v8/SPEC.md:776`).

3. Accepted: W1-W6 remain blocked. This CH5 ACCEPT is one V5 lens result only;
   it does not close W0, does not satisfy ORCHESTRATOR convergence by itself, and
   does not dispatch any later wave.

4. Accepted: stale P3 numeric seeds and stale pre-W0 planning prose are not live
   dispatch floors. SPEC's retained candidate lists are current; later W2/W3/W4
   plans must recompute thresholds from `SK-V8-open` before any redress.

## Required Disposition If Rejected

Not rejected. No CH5 fold is required.

The orchestrator should count this as a V5 CH5 ACCEPT only. W1-W6 must remain
blocked until W0 obtains the required challenge convergence and each later wave
has fresh plan/challenge/dispatch authority.

## Residual Risks

- P3-A/P3-C and `wave-0-plan.md` still contain stale pre-W0 wording, including
  old `K` parse-row language and old numeric seed floors. The live
  SPEC/SYNTHESIS/HANDOFF/DISPATCH-PROMPT/RESULTS packet supersedes those
  historical planning seeds for dispatch, but future doc cleanup should reduce
  citation ambiguity.
- The `marine_ik/real_typed_struct` displayed Sonic GO floor in SPEC is 1 Mbps
  lower than `ceil(6951 / 1.10)` from rounded RESULTS cells. This is not a W0
  blocker because the stronger no-regression floor dominates that row and W2
  remains blocked/recomputed, but the next W2 plan should either cite unrounded
  source values or normalize the displayed integer formula.
- W0 hot leaf evidence is a row-bound Criterion slope profile token, not a
  samply symbol-path proof. That is acceptable for W0 gate telemetry only. W3
  must not use it as production hot-leaf attribution without fresh measured-path
  profile evidence and challenge acceptance.
