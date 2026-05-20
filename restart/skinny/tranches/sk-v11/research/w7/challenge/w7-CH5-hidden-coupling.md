# SK-V11 W7 CH5 - Hidden Coupling

Pass: W7 CHALLENGE lens CH5.
Date: 2026-05-20.
Scope: hidden coupling review for W7 output digest/hash host sink, covering
Track 1 / Track 2 independence, gate/report schema coupling,
digest-as-typed-proof risk, and raw/decoded boundary parity.
Source edits: none.

## Verdict

ACCEPT the W7 entry block.

`w7-plan-output-digest-entry-block.md` is correct to stop before source
redress. SPEC Section 11 admits only C8 output digest/hash oracle or
per-product host sink work, and its entry gate requires CHALLENGE acceptance of
a bounded row subset whose fresh post-W6 profile still names
`output_digest_hash` as the limiting hot leaf, plus exact scalar fold/mix
source, output plane, and independent oracle. The W7 packet does not supply
that accepted subset. More importantly for CH5, the only obvious escaped-string
source-method route is already coupled to W6 / REDRESS 117 and replays the
REDRESS 54 sink-local decoded hash family.

This is not permission to close W7 by prose. It is permission to route W7 to a
measured REDRESS/block record with no source patch. Any future attempt to turn
W7 into an admitting source/report/gate packet is REVISE unless it satisfies the
requirements below in the named owner paths.

## Findings

1. Track 1 / Track 2 independence is not strong enough for a changed digest
contract.

The current split is real but narrow: Track 1 enters generated JSON direct
parsing through `track1_digest`, while Track 2 enters the local hand parser
through `track2_digest` in `skinny/crates/bbnf-bench/src/direct_struct.rs`.
Both tracks then share `JsonDirectDigest`, `hash_bytes`, `mix`, and fold helper
semantics. Existing exact Track 1 == Track 2 equality catches many parser
differences, but if W7 changes `hash_bytes`, `mix`, key fold, string fold,
container fold, or raw/decoded fold order in one shared helper, parity can
become self-confirming.

The W7 block avoids this hidden coupling by making no source change. A W7
admission packet would need either an independently written Track 2 reference
fold for the changed operation or a separately named canonical digest oracle
consumed by the gate. Track 2 cannot call generated direct parser symbols,
`JsonDigestSink` Track 1-only methods, report/gate code, or the changed Track 1
production helper as its only proof.

2. Gate/report schema is still a coupling point, not just a rendering detail.

R6 identifies a live mismatch: current direct row validation consumes
`sk_v10_direct_floor` constants in `skinny/crates/bbnf-bench/src/report.rs`,
while W7 must use the SK-V11 Section 0.4 floor table and W7-specific hot-leaf
proof. Existing validation also rejects `gate_only` consumers and stale open
wave ids, but a generic non-`gate_only` class is too weak for W7 unless the
same wave consumes a W7-specific host-sink consumer, profile artifact, hot leaf,
REDRESS entry, and wave id.

The block plan correctly avoids updating `skinny/RESULTS.md` or emitting a
producer-only W7 field. If W7 later emits a new column, manifest field,
consumer-class value, sidecar field, non-JSON field, or outcome variant, the
same commit must add fail-closed gate consumption and negative fixtures.
Producer-only report evidence cannot close `G-W7-DIGEST-SINK`.

3. Direct digest is not typed proof.

R1 and R5 both preserve the plane separation: direct rows live on the `digest`
output plane; generated typed rows live on `typed direct` and use typed host
contracts. A W7 direct-digest optimization cannot maintain or admit typed rows
by claiming the same checksum, same decoded strings, or same host hash shape.
If W7 touches `skinny/crates/bbnf-bench/src/generated_real_typed.rs` or typed
report/gate paths, it must supply typed guard proof on the typed output plane.
Direct digest evidence remains insufficient.

The block plan stays clear of this coupling because it opens no typed source or
gate path. A future W7 redress that tries to use direct digest parity as typed
guard evidence is REVISE before measurement.

4. Raw/decoded boundary parity is load-bearing.

SPEC Section 11 requires bit-exact digest equivalence after raw and decoded
segment boundaries. The tempting route, overriding `JsonDigestSink::*_source`
to fold decoded bytes from raw escaped slices, is already blocked by W6
challenge and REDRESS 117 as a REDRESS 54 replay. Renaming that work from W6
escaped segments to W7 host sink does not remove the coupling: it is still the
same decoded-source fold over the same direct digest fields.

If W7 ever selects a legal hash-core-only change below the decoded `&str`
boundary, it must prove all four string sites independently: root string,
object key, array string value, and object string value. The fixture set must
cover plain raw strings, all JSON single-character escapes, `\u00XX`,
surrogate pairs, mixed raw plus escaped segments, empty/short/long strings, and
repeated-key/order cases. Negative fixtures must cover invalid escape letters,
short Unicode escapes, invalid hex, lone/reversed surrogates, unescaped control
bytes, missing close quote, and trailing input, with Track 1, Track 2,
serde_json, and sonic-rs agreeing on accept/reject.

5. Non-JSON host sink remains unmeasurable for W7.

The plan correctly rejects non-JSON W7 closure because W1b did not leave an
accepted generated non-JSON Track 1 baseline with strict output equality and
gate-consumed report authority. CH5 does not accept a hidden non-JSON sidecar,
Lock 14 prose, or independent oracle alone as a substitute for generated Track
1 plus gate consumption.

## Required Fixes If The Block Is Overturned

Any REVISE path must name and implement these exact fixes before source redress
can be accepted:

1. In `skinny/crates/bbnf-bench/src/direct_struct.rs`, keep Track 1 as
generated direct product work and Track 2 as an independent hand/oracle path.
If a digest/fold helper changes, add a separately written Track 2 reference
fold or a separately named canonical digest oracle; do not let both measured
tracks share the changed production helper as their only proof.

2. In `skinny/crates/bbnf-bench/benches/json_parity.rs`, add raw/decoded parity
and negative fixtures for all four string boundary sites: root string, object
key, array string value, and object string value. The tests must catch silent
fallback to trait defaults, context swaps between key and value folds, and
Track 1 / Track 2 shared-helper bugs.

3. In `skinny/crates/bbnf-bench/src/report.rs`, replace or version the direct
floor consumer for W7 so admitted W7 direct rows use SK-V11 Section 0.4 floors,
not `sk_v10_direct_floor`. Add fail-closed W7 checks for `wave_id`,
`redress_entry`, W7 host-sink consumer class, `output_plane=digest`,
`track2_independence_status=independent_verified`, fresh post-W6 hot leaf
showing `output_digest_hash`, and same-run strict comparator evidence.

4. In `skinny/crates/bbnf-bench/src/bin/gate.rs`, emit only telemetry that
`report.rs` consumes in the same wave. Add negative gate fixtures for stale
SK-V9/SK-V10/SK-V11-W5/W6 provenance, `gate_only`, missing comparator, wrong
output plane, coupled Track 2, missing hot-leaf proof, floor miss, and
producer-only W7 fields.

5. In `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, make no W7 edit
unless the plan explicitly opens typed output. If opened, preserve typed guard
rows with typed-plane evidence; direct digest parity cannot count as typed
proof.

6. In selected non-JSON oracle/report owner files, proceed only if there is an
accepted generated non-JSON Track 1 baseline, independent oracle/Track 2,
strict output equality, and same-wave gate-consumed grammar/domain/workload
schema. Without that baseline, leave the route blocked.

7. In `skinny/RESULTS.md` and `skinny/REDRESS.md`, move results only after the
gate packet passes. If any W7 requirement fails, record the measured or
entry-gate REDRESS block and leave `RESULTS.md` unchanged.

## Disposition

DISPOSITION: ACCEPT W7 BLOCK.

CH5 finds no hidden-coupling objection to the no-source block. CH5 would reject
any W7 PASS that relies on Track 1 == Track 2 shared digest helpers, stale
SK-V10 direct gate floors, producer-only report fields, direct digest as typed
proof, raw/decoded source-method replay of REDRESS 54/117, or hidden non-JSON
sidecar authority.
