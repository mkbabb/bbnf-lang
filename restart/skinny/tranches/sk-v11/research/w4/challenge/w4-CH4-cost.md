# SK-V11 W4 CHALLENGE CH4 Cost

Date: 2026-05-20.
Lens: CH4 cost / measurement sufficiency.
Scope: W4 `container_tail_next` plan for `random/direct_to_struct`.
Output: this file.
Disposition: REVISE.

## Authorities Read

- `restart/skinny/tranches/sk-v11/SPEC.md` Sections 0.4, 0.5, 2.1, and 8.
- `restart/skinny/tranches/sk-v11/HANDOFF.md`.
- `restart/skinny/tranches/sk-v11/research/w4/w4-plan-container-tail-direct.md`.
- `restart/skinny/tranches/sk-v11/research/w4/w4-R1-generated-dispatch-lowering.md`.
- `restart/skinny/tranches/sk-v11/research/w4/w4-R2-json-generated-runtime.md`.
- `restart/skinny/tranches/sk-v11/research/w4/w4-R3-direct-oracles.md`.
- `restart/skinny/tranches/sk-v11/research/w4/w4-R4-gate-report-consumption.md`.
- `restart/skinny/tranches/sk-v11/research/w4/w4-R5-row-floors.md`.
- `restart/skinny/tranches/sk-v11/research/w4/w4-R6-preblocked-ledger.md`.
- `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md`.
- `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md`.
- `restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md`.
- `skinny/RESULTS.md`.
- `skinny/crates/codegen/src/sink_direct.rs`.
- `skinny/crates/runtime/src/grammars/json/generated.rs`.
- `skinny/crates/bbnf-bench/src/direct_struct.rs`.
- `skinny/crates/bbnf-bench/benches/json_parity.rs`.
- `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- `skinny/crates/bbnf-bench/src/report.rs`.

## Verdict

REVISE. The D1 scalar container-tail helper is an authorized W4 shape, and the
one-row target keeps source and measurement cost bounded. The plan still cannot
proceed to row admission as written because the required cost story is weak on
the binding side: `random/direct_to_struct` needs only +185 Mbps Track 1
movement, but it needs +929 Mbps Track 2 movement, from 6949 to 7878 Mbps.
That is a 13.4% throughput lift, or roughly 0.136 ns/byte removed from Track 2.

The current code already performs the tail as an inlined whitespace skip plus
one comma-or-close byte check. Generated Track 1 does this in
`parse_object_direct` / `parse_array_direct` and `take_direct`
(`sink_direct.rs:247-313`, generated as `generated.rs:548-606`,
`generated.rs:800-821`). Track 2 has the same local shape in
`HandParser::object`, `HandParser::array`, and `HandParser::take`
(`direct_struct.rs:483-539`, `direct_struct.rs:609-616`). A scalar helper may
clean up shape and may plausibly fund the small Track 1 gap, but CH4 cannot
assume it removes enough work to fund a 13.4% Track 2 lift without a
pre-admission micro/probe.

## Cost Likelihood

Likelihood is low without new evidence.

`random` is not primarily a container-tail row in the accepted P1 attribution.
P1-B reports direct `random` hot leaves as Track 1 `tiny_string` 23.8%,
`ws` 17.9%, and `option_copied` 6.6%; Track 2 is `hand_tiny` 20.2%, `ws`
16.9%, and `u64_add` 8.5%. P1-E likewise classifies direct `random` as a
near-floor string/digest residual, not a number/container row. D1 touches a
cross-cutting delimiter site, but the measured dominant leaves are string,
whitespace, and digest folding.

A fixture count reinforces the cost problem. `skinny/test_data/random.json` is
510,476 bytes with about 24,004 member/element values and 20,002 commas. Moving
Track 2 from 6949 to 7878 Mbps requires about 69 us per full parse, or roughly
2.9 ns saved per member/element tail if all of the gain comes from D1. The
current tail is already one skip plus one byte check on the fast path, so that
per-tail saving is not credible enough for row admission until measured.

Guard risk is also real because W4 touches all generated and hand direct
object/array loops. The direct guard margins are narrow: `citm_catalog` has
about 2% headroom on both tracks, `apache_builds` about 2%, `marine_ik` Track 1
about 2%, and `unicode_basic` about 2%. A helper that loses a small amount of
inlining or branch predictability can erase a guard while still improving
`random`.

## Required Evidence Before Row Admit

Before `random/direct_to_struct` can move to `A / GO`, W4 needs the following
evidence in the redress artifact or attached probe output:

1. Same-host old-vs-new `profile_direct` probe for `random` Track 1 and Track
   2, with run id, host triple, flags, iteration count, and repeated samples.
   The Track 2 probe must show enough margin above 7878 Mbps to survive
   Criterion noise; a one-sample scrape at or barely above floor is not enough.
2. Tail-specific cost probe or instrumentation showing the D1 helper actually
   reduces work at the post-value container tail, not just total row noise. For
   `random`, the proof must explain the required Track 2 time removal against
   the observed member/element count.
3. Same probe on the direct guard rows
   `citm_catalog`, `apache_builds`, `marine_ik`, and `unicode_basic`, proving
   no guard-shaped regression before binding Criterion.
4. Track 2 independence proof after the edit: the hand helper must remain local
   to `direct_struct.rs` and must not call generated Track 1, generated
   SinkOnly helpers, generated typed helpers, or hidden shared parser code.
5. Binding Criterion in a fresh `CRITERION_HOME` for `random`, all four direct
   guards, and same-run `sonic_rs_direct_to_struct` / `serde_json_direct_to_struct`
   comparator rows, followed by `gate-json --with-cost-facts --check-results`.
6. W4 gate/report tests proving selected-row floors are shared between
   producer and validator, unselected W4 candidate rows remain clamped,
   below-floor rows reject, missing W4 provenance rejects, and direct guard
   floors are consumed.
7. Typed guard measurement if W4 claims all SPEC Section 0.5 guards hold or if
   report-wide validation requires refreshed typed rows:
   `twitter`, `citm_catalog`, `apache_builds`, `github_events`,
   `update_center`, `mesh`, and `marine_ik` `real_typed_struct` tracks.

## Measurement Command Review

The plan's commands are necessary but not sufficient.

What is good:

- It includes `check-json`, codegen tests, direct/Track 2 tests, native
  Criterion for the selected direct row and direct guards, and
  `gate-json --with-cost-facts --check-results`.
- It measures both generated Track 1 and independent Track 2 for `random`,
  plus sonic and serde direct comparators.

Required revisions:

- Add a pre-admission micro/probe command set. R3 already names the right
  starting point: build `profile_direct` and run same-host sweeps. The W4 plan
  should require old-vs-new repeated probes for `random` and the direct guards
  before treating a full Criterion row as admission evidence.
- Add `cargo run -p xtask -- regen-json` before `check-json` when
  `sink_direct.rs` is edited. `check-json` is a stale-output guard, not a
  regeneration command.
- Split the `bbnf-bench` unit test filters or make them explicit. The plan's
  `cargo test -p bbnf-bench direct_struct track2::json -- --nocapture` is not
  a clear durable contract for both test families; use separate commands or a
  named W4 test filter.
- Add report-side direct-contract tests, not only `--bin gate w4`. R4 requires
  W4 validation in `report.rs` and a shared W4 floor authority because the
  current `sk_v10_direct_floor` table disagrees with SPEC Section 0.4 for W4
  candidates, including `random`.
- Add direct guard floor consumption tests. R4 notes the current validator
  enforces typed maintain floors but does not separately enforce unchanged
  direct `A / GO` guard floors.
- Add typed guard Criterion rows or explicitly revise the plan so CHALLENGE
  states typed guards are not refreshed and explains why that still satisfies
  SPEC Section 0.5. As written, the plan says typed guards are conditional, but
  W4 exit says guard floors in Section 0.5 hold.

## Required Plan Changes

1. Keep D1 as an allowed candidate, but downgrade its likelihood for
   `random` to "probe first, low confidence until Track 2 moves."
2. Add a hard pre-redress or pre-admission rule: no `RESULTS.md` row movement
   unless `random` Track 2 shows a repeated same-host probe lift of at least the
   required 13.4% plus noise margin and direct guards stay above maintain
   floors.
3. Add a tail-density/cost explanation to REDRESS 115 if the route admits. If
   the measured win comes from whitespace/string/digest side effects instead
   of the container-tail helper, do not claim D1 closed the row.
4. Add `regen-json`, `profile_direct` old-vs-new probes, report validator
   tests, direct guard consumption tests, and typed guard measurement or an
   explicit CHALLENGE waiver before redress.
5. Preserve the existing owner correction for `sink_direct.rs`; without that
   correction, generated direct source edits remain outside the Section 8 owner
   table.

DISPOSITION: REVISE
