# SK-V8 W0 Hardening V4 CH4 Review

## Verdict

REJECT.

Confidence: 96%.

## Scope

Reviewed target: `077aadad8aacf95e3250ec157f30ba6ab873bf6b`
(`fix(sk-v8-wave0): fold hardening V3 gate blockers`).

Lens: CH4 cost/reproducibility pressure on W0 telemetry: `run_id`
validated-input scope, volatile probe exclusion, W0 Criterion fingerprint,
SIMD metadata coherence, SIMD parity source, and check/update semantics.
This review edits only this CH4 artifact.

## Evidence

- ORCHESTRATOR CH4 owns realistic cost/wave alignment and same-wave consumers
  (`restart/prompts/ORCHESTRATOR.md:86`), and §3Z keeps W0 open until challenge
  convergence has no critical defects (`restart/prompts/ORCHESTRATOR.md:118`,
  `restart/prompts/ORCHESTRATOR.md:120`, `restart/prompts/ORCHESTRATOR.md:123`).
- W0 requires every emitted telemetry field to be consumed by `gate-json` in the
  same wave and rejects producer-only telemetry, behavior drift, malformed
  sidecar evidence, and stale sidecar claims (`restart/skinny/tranches/sk-v8/SPEC.md:103`,
  `restart/skinny/tranches/sk-v8/SPEC.md:142`, `restart/skinny/tranches/sk-v8/SPEC.md:146`,
  `restart/skinny/tranches/sk-v8/SPEC.md:322`, `restart/skinny/tranches/sk-v8/SPEC.md:337`).
- V3 specifically required `run_id` to derive from validated W0 row inputs only,
  excluding de-rendered volatile probes, unrelated Criterion groups, and later
  wave files (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V3/HARDENING-W0-V3-CONSOLIDATED.md:50`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V3/HARDENING-W0-V3-CONSOLIDATED.md:53`).
- Positive checks run from `skinny/`:
  `cargo test -p bbnf-bench w0_ -- --nocapture` passed 11 report tests and 8
  gate-bin tests; `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native'
  cargo xtask gate-json --advisory --check-results` exited 0.
- Focused negative checks:
  mutating only a de-rendered `json_probes_twitter` estimate exited 0 and kept
  `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`; corrupting
  `simd_structural_scan/canada_simd/metadata.toml` exited 1 with
  `canada SIMD metadata invalid: SIMD metadata is from a different capture`;
  the same corrupted SIMD metadata with `--update-results` exited 1 and left
  `skinny/RESULTS.md` byte-unchanged.

## Findings

1. BLOCKER: `run_id` is still scoped to W0-shaped Criterion paths, not the
   validated W0 row set.

   `RunFacts::probe` builds every manifest `run_id` from
   `criterion_fingerprint(criterion_root)` (`skinny/crates/bbnf-bench/src/bin/gate.rs:379`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:388`). The fingerprint recursively
   walks the entire Criterion root and hashes files accepted by
   `is_w0_criterion_input` (`skinny/crates/bbnf-bench/src/bin/gate.rs:668`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:681`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:684`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:701`). That predicate rejects probe
   groups, but it accepts any `group.starts_with("json_")` with a W0 bench name
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:725`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:735`). It does not bind `group` to
   `test_fixtures::load_available_bench_fixtures()` or to the 38
   `SK_V8_OPEN_BASELINE` row ids. A stray or later-wave
   `json_unvalidated_future/track1_generated/new/estimates.json` therefore
   changes the committed run id even though no validated W0 row, metadata row, or
   rendered benchmark row changes. This is the V3 blocker in narrower form:
   volatile probes are excluded, but unrelated W0-shaped Criterion groups are not.

   Minimal reproduction from `skinny/`:

   ```sh
   tmp=$(mktemp -d /tmp/skv8-ch4-extra.XXXXXX)
   cp -R /tmp/skv8-w0-target/criterion "$tmp/criterion"
   mkdir -p "$tmp/criterion/json_unvalidated_future/track1_generated/new"
   printf '{"slope":{"point_estimate":12345.0}}' > \
     "$tmp/criterion/json_unvalidated_future/track1_generated/new/estimates.json"
   CARGO_TARGET_DIR="$tmp" RUSTFLAGS='-C target-cpu=native' \
     cargo xtask gate-json --advisory --check-results
   ```

   Observed result: exit status 1 with
   `RESULTS.md is stale; rerun cargo xtask gate-json --update-results --advisory`.
   Since the only changed file is outside the validated fixture/row set, this is
   not acceptable W0 evidence stability.

2. Accepted: volatile probe exclusion is materially fixed for the de-rendered
   probe class. The write path rejects `--include-volatile-probes` combined with
   `--update-results` or `--write-results`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:20`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:33`), and the fingerprint unit test
   proves `json_probes_*` estimates do not perturb the fingerprint
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:1748`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1771`). My temp-root mutation
   reproduced the same behavior dynamically.

3. Accepted: SIMD metadata promotion is now fail-closed before report update.
   The gate reads SIMD metadata before classification/report validation
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:60`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:75`), fails read/TOML errors
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:1353`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1361`), validates fixture hash/bytes,
   bench semantics, capture identity, capture policy, and parity hash
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:1364`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1422`), and the update path validates
   before writing (`skinny/crates/bbnf-bench/src/bin/gate.rs:314`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:327`).

4. Accepted: SIMD parity source is coherent enough for W0. The SIMD benchmark
   computes scalar and SIMD structural offsets from the same fixture bytes and
   asserts hash equality before writing metadata
   (`skinny/crates/bbnf-bench/benches/simd_scan.rs:16`,
   `skinny/crates/bbnf-bench/benches/simd_scan.rs:27`). The metadata constructor
   stores the scalar parity hash in a fixture-bound slot
   (`skinny/crates/bbnf-bench/src/metadata.rs:248`,
   `skinny/crates/bbnf-bench/src/metadata.rs:261`), and the gate compares that
   slot to a freshly recomputed scalar hash before report generation
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:60`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:69`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1419`).

5. Accepted: check-only/update semantics are now safe for the tested W0 failure
   class. The gate validates schema and W0 manifest before rendering/writing
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:314`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:323`), writes only under
   `--update-results` / `--write-results`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:21`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:24`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:325`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:327`), and a corrupted SIMD update
   attempt left `RESULTS.md` unchanged.

## Required Disposition If Rejected

Fold the W0 fingerprint one more time. `criterion_fingerprint` must be driven by
the validated W0 fixture/row manifest, not by a free recursive path predicate.
Acceptable fixes:

- Pass the loaded fixture list and expected W0 bench/workload set into the
  fingerprint builder, and reject or ignore any `json_*` Criterion group whose
  corpus is not in the fixture list.
- Include only files that correspond to validated required metadata specs,
  expected estimates for rendered rows/comparators, and the admitted SIMD
  metadata/Canada scan estimate.
- Add a negative test or scripted check for an unrelated W0-shaped
  `json_unvalidated_future/track1_generated/new/estimates.json` proving the
  committed `run_id` remains unchanged, or that the gate fails with an explicit
  "unvalidated Criterion group" error before any update path.

W1-W6 remain blocked until this is folded and W0 receives the required challenge
acceptance cycle.

## Residual Risks

- The current `run_id` is a single capture-level hash for all rows. That is
  acceptable for W0 once the input set is validated, but later waves that add row
  families should consider row-local capture ids to reduce unrelated churn.
- SIMD parity metadata is strict for the W0 bench shape, but only Canada SIMD
  throughput is rendered as a gate note. That is packet-consistent for W0; later
  SIMD behavior waves need row-specific scalar/checkasm gates before wiring.
- I did not rerun the full benchmark capture; this review used the committed
  `/tmp/skv8-w0-target` Criterion root and focused negative mutations.
