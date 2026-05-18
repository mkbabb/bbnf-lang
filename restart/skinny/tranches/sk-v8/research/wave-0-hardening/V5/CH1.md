# CH1 W0 V5 Hardening Challenge

## Verdict

REJECT.

Confidence: 98%.

## Scope

Correctness challenge of `0c49fabd6d6facd136e1e69b8482aa4f239561ae`
(`fix(sk-v8-wave0): fold hardening V4 gate blockers`). Lens: strict
outcome/admission row identity, `parse_utf8` / `escape_complete` invariants
including deferred rows, strict-vs-strict comparator discipline, and no new
directive/BIR/substrate. This review edited only this artifact.

## Evidence

- ORCHESTRATOR CH1 requires resolving evidence and strictness-plane discipline
  (`restart/prompts/ORCHESTRATOR.md:83`); the global non-negotiables forbid new
  directive/BIR/substrate routes and keep permissive rows out of strict admission
  (`restart/prompts/ORCHESTRATOR.md:201`,
  `restart/prompts/ORCHESTRATOR.md:202`,
  `restart/prompts/ORCHESTRATOR.md:203`,
  `restart/prompts/ORCHESTRATOR.md:208`).
- W0 must create a checked `SK-V8-open` baseline
  (`restart/skinny/tranches/sk-v8/SPEC.md:46`), reject unsupported outcomes and
  strict-admission failures (`restart/skinny/tranches/sk-v8/SPEC.md:142`-
  `restart/skinny/tranches/sk-v8/SPEC.md:146`,
  `restart/skinny/tranches/sk-v8/SPEC.md:318`-
  `restart/skinny/tranches/sk-v8/SPEC.md:319`), and preserve the current opening
  row families: 16 parse `S / NO-GO`, 1 parse `L / NO-GO`, 3 direct `A / GO`, 14
  direct `N-direct / NO-GO`, and 4 real typed `A / GO`
  (`restart/skinny/tranches/sk-v8/SPEC.md:150`-
  `restart/skinny/tranches/sk-v8/SPEC.md:160`;
  `restart/skinny/tranches/sk-v8/HANDOFF.md:31`-
  `restart/skinny/tranches/sk-v8/HANDOFF.md:48`).
- Current rendered rows demonstrate the exact states the gate must bind: e.g.
  `twitter/parse_only` is `S / NO-GO`, `twitter/direct_to_struct` is
  `N-direct / NO-GO`, and `twitter/real_typed_struct` is `A / GO`
  (`skinny/RESULTS.md:5`-`skinny/RESULTS.md:7`).
- The V4 deferred-row blocker is materially folded. Non-strict rows now require
  `Strictness=deferred`, `measured_validation_path=view-boundary`,
  `parse_utf8=view-boundary`, and `escape_complete=yes` before returning OK
  (`skinny/crates/bbnf-bench/src/report.rs:920`-
  `skinny/crates/bbnf-bench/src/report.rs:948`), and the focused test passes
  (`skinny/crates/bbnf-bench/src/report.rs:1640`-
  `skinny/crates/bbnf-bench/src/report.rs:1662`).
- The V4 run-id fingerprint blocker is materially folded. `criterion_fingerprint`
  now receives the loaded fixture set and filters Criterion inputs by fixture name
  plus admitted W0 bench names (`skinny/crates/bbnf-bench/src/bin/gate.rs:673`-
  `skinny/crates/bbnf-bench/src/bin/gate.rs:755`); its focused test proves
  `json_unvalidated_future/track1_generated/new/estimates.json` is excluded while
  a real W0 main estimate changes the hash
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:1766`-
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1794`).
- Executed evidence:
  - `cargo test -p bbnf-bench`: PASS, 52 library tests and 8 gate-bin tests.
  - `cargo test -p bbnf-bench report::tests::w0_report_accepts_exact_opening_baseline -- --nocapture`: PASS; this is the reproducer below.
  - `cargo test -p bbnf-bench w0_rejects_deferred_validation_semantic_drift -- --nocapture`: PASS.
  - `cargo test -p bbnf-bench w0_criterion_fingerprint_excludes_derendered_probe_estimates -- --nocapture`: PASS.
  - `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`: PASS against committed `skinny/RESULTS.md`.

## Findings

1. BLOCKER: `Report::validate_sk_v8_w0()` does not bind exact opening
   outcome/verdict identity. `SkV8OpenBaseline` stores only `row_id`,
   `track1_mbps`, and `track2_mbps` (`skinny/crates/bbnf-bench/src/report.rs:634`-
   `skinny/crates/bbnf-bench/src/report.rs:638`). Report-level W0 validation checks
   exact row count, duplicate/unknown/missing row ids, and Track 1/Track 2 drift,
   but it never compares each row's outcome or verdict to the opening row state
   (`skinny/crates/bbnf-bench/src/report.rs:493`-
   `skinny/crates/bbnf-bench/src/report.rs:519`). Row-level validation only checks
   that the outcome is in a broad W0 allowlist
   (`skinny/crates/bbnf-bench/src/report.rs:868`-
   `skinny/crates/bbnf-bench/src/report.rs:878`) and that parse rows are one of
   `I/J/K/L/M/S` (`skinny/crates/bbnf-bench/src/report.rs:360`-
   `skinny/crates/bbnf-bench/src/report.rs:368`).

   The existing test named `w0_report_accepts_exact_opening_baseline` proves the
   false-accept. It constructs every parse row as `Outcome::KSimdParityHashFail`
   (`skinny/crates/bbnf-bench/src/report.rs:1830`-
   `skinny/crates/bbnf-bench/src/report.rs:1839`) and every non-parse row with
   `outcome=None`, which renders `A / GO`
   (`skinny/crates/bbnf-bench/src/report.rs:147`-
   `skinny/crates/bbnf-bench/src/report.rs:153`,
   `skinny/crates/bbnf-bench/src/report.rs:1840`-
   `skinny/crates/bbnf-bench/src/report.rs:1854`), then asserts
   `report.validate_sk_v8_w0().is_ok()`
   (`skinny/crates/bbnf-bench/src/report.rs:1855`-
   `skinny/crates/bbnf-bench/src/report.rs:1859`). That accepts impossible opening
   identities such as `twitter/direct_to_struct = A / GO` even though the live row
   is `N-direct / NO-GO` (`skinny/RESULTS.md:6`) and current packet state says
   direct rows are 3 `A / GO` plus 14 `N-direct / NO-GO`
   (`restart/skinny/tranches/sk-v8/SPEC.md:155`-
   `restart/skinny/tranches/sk-v8/SPEC.md:157`).

   Minimal reproduction from repo root:

   ```sh
   cd skinny
   cargo test -p bbnf-bench report::tests::w0_report_accepts_exact_opening_baseline -- --nocapture
   ```

   This should fail if W0 enforces exact opening outcome/verdict identity; it
   passes.

2. No CH1 blocker remains in deferred-row validation semantics. The V5 target
   rejects `parse_utf8=none` and `escape_complete=n/a` while keeping the row
   deferred/view-boundary, and the focused test covers both mutations
   (`skinny/crates/bbnf-bench/src/report.rs:1640`-
   `skinny/crates/bbnf-bench/src/report.rs:1662`).

3. No CH1 blocker remains in the V4 `run_id` unvalidated Criterion-group path.
   Fixture-name filtering excludes unrelated W0-shaped `json_*` Criterion groups,
   and the focused fingerprint test passed
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:735`-
   `skinny/crates/bbnf-bench/src/bin/gate.rs:755`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1766`-
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1794`).

4. No new directive/BIR/substrate blocker found in the V5 fold. The fold from
   `077aadad` to `0c49fabd` edits only
   `skinny/crates/bbnf-bench/src/bin/gate.rs` and
   `skinny/crates/bbnf-bench/src/report.rs`, preserving the W0 owner boundary.

## Required Disposition If Rejected

- Extend the W0 baseline model to store expected `outcome_id` and `verdict` per
  row, sourced from the live `SK-V8-open` table.
- Make `validate_sk_v8_w0()` reject any row whose `row_id`, `outcome_id`,
  `verdict`, Track 1, or Track 2 differs from the W0 baseline beyond the existing
  numeric tolerance. Do not rely on a broad enum allowlist for current-row
  identity.
- Replace or split `w0_report_accepts_exact_opening_baseline` so its accepted
  fixture uses the actual current W0 outcome/verdict tuple for each row, then add
  negative tests mutating `twitter/direct_to_struct` from `N-direct / NO-GO` to
  `A / GO` and `twitter/parse_only` from `S / NO-GO` to `K / NO-GO`.
- Rerun:
  - `cargo test -p bbnf-bench`
  - `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`

## Residual Risks

- The committed `skinny/RESULTS.md` currently has packet-consistent row states; the
  blocker is that the W0 validator would accept a bad regenerated report if row
  outcomes/verdicts drift while row ids and throughput cells stay in range.
- The live W0 plan/research artifacts still contain older `K` wording for parse
  substrate-guard rows (`restart/skinny/tranches/sk-v8/research/wave-0-plan.md:126`-
  `restart/skinny/tranches/sk-v8/research/wave-0-plan.md:130`). The fix must bind
  the live `SPEC.md` / `RESULTS.md` state, where parse rows are `S` except the
  canada hard failure `L`.
- W1-W6 remain blocked under ORCHESTRATOR convergence because this V5 CH1
  rejection is a material W0 gate-honesty defect.
