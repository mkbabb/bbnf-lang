# SK-V11 W7 R5: Track 2 / Oracle Independence And Parity

Pass: SK-V11 W7 Phase 1 research.
Agent: R5.
Date: 2026-05-20.
Scope: define independence and parity requirements for `G-W7-DIGEST-SINK`.
Output: this file only.

## Read Set

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 11.
- `restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md`.
- `restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md`.
- `skinny/crates/bbnf-bench/src/direct_struct.rs`.
- `skinny/crates/bbnf-bench/benches/json_parity.rs`.
- `skinny/crates/bbnf-bench/src/gate.rs`.
- `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- `skinny/crates/bbnf-bench/src/report.rs`.

## Current Boundary

SPEC Section 11 limits W7 to the output digest/host sink. A W7 admit is valid
only when a fresh post-W6 profile still names `output_digest_hash` as the
limiting leaf for the selected row subset, the intervention stays out of
generic parser semantics, and selected rows clear the direct floors on both
generated Track 1 and independent Track 2/oracle.

The existing direct path is:

- Track 1: `track1_digest` instantiates `JsonDigestSink` and calls
  `runtime::generated_json::parse_direct`.
- Track 2: `track2_digest` calls the local hand parser in `direct_struct.rs`.
- Native comparators: `serde_digest` and `sonic_digest` deserialize directly
  into `JsonDirectDigest`.
- Bench parity: `json_parity.rs` runs `assert_direct_struct_parity` before
  benchmarking every fixture.
- Gate parity: `gate.rs` rejects direct projection on correctness failure, and
  `bin/gate.rs` derives direct rows from `track1_direct_to_struct`,
  `track2_direct_to_struct`, `sonic_rs_direct_to_struct`, and
  `serde_json_direct_to_struct`.

This boundary is strong enough for W0/W10 direct correctness, but not enough
by itself for W7 host-sink movement. Today `assert_direct_struct_parity`
requires exact Track 1 == Track 2 equality, but only `same_shape_as` equality
against serde and sonic. It also lets Track 2 share the same digest type,
`hash_bytes`, `mix`, string materialization helper, and number materializers as
Track 1. W7 must tighten those seams if it changes hash/fold behavior.

## Independence Requirements

Track 1 requirements:

- Track 1 must remain the generated direct parser driving a product sink:
  `runtime::generated_json::parse_direct(input, &mut JsonDigestSink)`.
- Any W7 fast path must be reachable from that generated product path, not from
  `gate-json`, report rendering, a probe bench, or a parse-only path.
- Track 1 may use the optimized W7 sink/fold implementation, but it must not
  read Track 2 state, serde/sonic state, cached comparator output, or hidden
  semantic side tables.

Track 2 / oracle requirements:

- Track 2 must parse the input through the hand direct path and must not call
  `runtime::generated_json::parse_direct`, `JsonSink` default source methods,
  Track 1-only W7 sink methods, generated parser helpers, or report/gate code.
- Track 2 may share the public `JsonDirectDigest` shape only as the output
  contract. If W7 changes `hash_bytes`, `mix`, raw/decoded fold order, key fold,
  string fold, or container fold semantics, Track 2 must use an independently
  written reference fold for the changed operation or W7 must add a second
  independent oracle. A shared changed helper cannot be the only oracle.
- Track 2 must keep its own accept/reject path for malformed strings and
  malformed numbers. A Track 1 bug that accepts an invalid escape, control
  byte, surrogate pair, trailing input, or invalid number must fail parity
  before throughput is considered.
- `track2_independence_status` may remain `independent_verified` only when a
  same-wave test proves these call-boundaries. If the proof is source-only, it
  should grep or otherwise assert that Track 2 does not reference generated
  direct parser symbols or W7 Track 1-only sink symbols.

Serde / sonic comparator requirements:

- W7 cannot close on Track 1 == Track 2 alone. The selected row must carry
  same-run native `sonic_rs_direct_to_struct` and `serde_json_direct_to_struct`
  comparator evidence on the `digest` output plane.
- For W7 digest/hash movement, serde and sonic must be exact digest oracles for
  every field that W7 changes. Shape-only comparator parity is insufficient for
  an output hash/fingerprint intervention.
- If serde or sonic materialization changes object iteration order and exact
  fingerprint equality is therefore not a valid comparator, W7 must add a
  separately named canonical digest oracle and gate consumer before redress.
  The canonical oracle must compare object, array, member, element, scalar
  counts, decoded string byte counts, and the selected hash/fingerprint
  contract. A row with only `same_shape_as` comparator evidence remains
  `N-direct / NO-GO`.
- The report must continue to emit same-run native comparator artifacts for
  both strict native comparators. Historical sidecars, absent sidecars, PMU,
  Criterion slope, or probe-only values cannot substitute for serde/sonic
  digest equality.

Raw / decoded boundary parity requirements:

- W7 must prove bit-exact equality across all four direct string boundary
  sites: object key, root string, array string, and object string value.
- The parity fixture set must include raw plain ASCII, escaped quote, escaped
  backslash, slash, backspace, form feed, newline, carriage return, tab,
  `\u00XX`, non-BMP surrogate pair, mixed raw plus escaped segments, empty
  string, short/tiny string, long string, and repeated-key/object-order cases.
- The negative fixture set must include invalid escape letters, short Unicode
  escapes, invalid hex, lone high surrogate, lone low surrogate, reversed
  surrogate, unescaped control byte before close, missing close quote, and
  trailing input. Track 1, Track 2, serde, and sonic must agree on accept versus
  reject for these fixtures.
- W7 cannot use the already-consuming escape path as proof of a new product
  sink. If the selected change is only a faster hash/fold over decoded bytes,
  the evidence must isolate the hash/fold delta from escape validation and
  decode correctness.

Typed guard separation requirements:

- Direct digest evidence cannot admit or maintain typed product rows. The
  `direct_digest_as_typed` rejection in P3-D remains binding.
- If W7 edits only `direct_struct.rs`, report/gate direct rows, or direct
  parity, it must still run and preserve the seven typed maintain rows because
  report/gate changes can misclassify existing typed GO rows.
- If W7 touches `generated_real_typed.rs`, `parse-that-regex`, `bbnf-simd`,
  generic runtime/codegen, or any shared string/number helper used by typed
  output, typed guard proof must be independent: generated typed Track 1,
  typed Track 2/oracle, serde/sonic typed comparator where applicable, and
  `typed_checksum` parity. A direct digest checksum is not a typed oracle.
- The W7 report row must keep direct rows on output plane `digest` and typed
  rows on output plane `typed direct`; no row may mix the two planes in
  comparator evidence.

Sampled same-wave consumer proof requirements:

- W7 must capture a same-wave sampled profile for each admitted selected row.
  The sampled stack must show the generated direct benchmark entering
  `track1_direct_to_struct`, then `track1_digest`, then generated
  `parse_direct`, then the W7 digest/host-sink consumer.
- The new W7 consumer must appear in the affected row's sampled hot-path
  evidence. A symbol visible only in unit tests, probes, Track 2, serde/sonic,
  or `gate-json` is not a same-wave product consumer.
- `same_wave_consumer_class` must be a non-`gate_only` direct contract class
  specific enough to distinguish W7 output sink work, for example
  `gate_json_direct_contract` plus a signal string naming `SK-V11-W7` and the
  W7 sink function. If gate/report introduce a narrower value, `gate-json`
  must consume it in the same commit.
- The `profile_artifact` and `hot_leaf` cells must remain gate-consumed and
  must point to the selected direct benchmark, not to parse-only, a probe, or a
  comparator benchmark.
- RESULTS may move only after all of these are true in one same-run capture:
  selected row floors pass on Track 1 and Track 2/oracle, serde/sonic or
  canonical digest equality passes, guard floors pass, W7 consumer is sampled
  on the generated direct hot path, and report/gate consume the W7 wave id,
  REDRESS entry, Track 2 independence status, comparator evidence, and
  same-wave consumer class.

## Gate / Report Implications

The current gate/report layer already enforces several necessary pieces:

- Track 1 and Track 2 Mbps are required for direct movement.
- Direct row movement requires output plane `digest`, strict rows,
  measured-row validation, `escape_complete = yes`, `independent_verified`,
  a non-`gate_only` consumer, REDRESS provenance, profile artifact, hot leaf,
  and comparator evidence.
- Native strict comparator evidence must include same-run sonic and serde
  direct comparator rows on the `digest` plane.

W7 still needs a same-wave gate/report extension or an explicit plan showing
the existing direct-contract branch already admits the selected W7 row. The
extension must reject:

- `same_shape_as`-only serde/sonic parity for a changed hash/fingerprint route.
- Track 2 sharing the changed W7 fold/hash implementation as its only oracle.
- Missing sampled W7 consumer evidence.
- Direct digest rows counted as typed guard evidence.
- Any new telemetry field or consumer-class value not read by `gate-json`.

## R5 Verdict

RESEARCH COMPLETE.

The viable W7 independence contract is narrower than "four-way parity green":
Track 1 must be generated direct product work; Track 2 must be an independent
hand/oracle path for the changed fold/hash operation; serde/sonic must provide
same-run digest-plane equality for the changed output contract or W7 must add a
canonical digest oracle consumed by the gate; raw/decoded boundary fixtures
must cover all direct string boundary sites; typed guards must remain separate;
and a sampled W7 consumer must appear on the generated direct hot path before
any row moves.
