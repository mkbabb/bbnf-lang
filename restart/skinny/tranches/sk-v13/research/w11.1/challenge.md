# SK-V13 W11.1 CHALLENGE — Numbers Direct Numeric-Array Dispatch

Date: 2026-05-21
Wave: W11.1
Disposition: ACCEPT WITH CONSTRAINTS

## Scope

W11.1 reopens `json/numbers/direct_to_struct/main` under USER PIN D4 with a
generated direct numeric-array dispatch route. The selected intervention is a
generic JSON direct fast path inside `parse_array_direct`: after whitespace is
skipped and before generic array-element dispatch, numeric-leading elements call
the existing `parse_number_array_direct(input, bytes, cursor, sink, byte)`.

The accepted plan does not add a new number parser, substrate, directive, BIR
variant, `BackendShape`, source hook, digest shortcut, SIMD primitive, or row
private branch.

## CH1 Correctness

ACCEPT with constraints.

- Preserve the existing array sequence: consume `[`, call
  `sink.begin_array()`, skip whitespace, check empty `]`, then enter the loop.
- After every comma, skip whitespace before peeking the next byte.
- Numeric-leading array elements (`-` or `0..=9`) must call only the existing
  `parse_number_array_direct` path.
- Non-number bytes keep the existing `parse_array_element_at_direct` behavior
  and error offsets, including EOF and invalid bytes.
- `[1,]` must remain `ExpectedValue` at `]`; malformed numeric-leading tokens
  such as `[-]` must remain `InvalidNumber` at the number start.
- Numeric array elements continue through `sink.array_i64/u64/f64`; no root or
  object callbacks and no double-counted elements.
- Redress tests must cover empty array, numeric-only array, mixed scalars,
  nested array/object, whitespace after commas, `[1,]`, `[-]`,
  `[1e999999999999999999999]`, and direct parity against Track 2 / serde /
  sonic for `numbers`.

## CH2 Generality / Lock 14

ACCEPT with constraints.

- Add explicit W11.1 Lock 14 owner-path allowance in
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs`.
- Edit runtime output and generator together:
  `skinny/crates/runtime/src/grammars/json/generated.rs` and
  `skinny/crates/codegen/src/json_sink_direct.rs`.
- The fast path must be generic JSON direct array behavior, not a
  `numbers`-specific fixture or corpus branch.
- Do not edit `bbnf-simd`, `parse-that-regex`, `JsonDirectDigest`
  hashing/folding, generic substrate, directives, BIR, or `BackendShape`.

## CH3 Regression / REDRESS

ACCEPT with constraints.

- Refresh guards for existing JSON direct admits, typed admits, and admitted CSS
  rows. Minimum direct guard set: `citm_catalog`, `apache_builds`,
  `marine_ik`, `instruments`, `numbers`, and `unicode_basic`.
- No silent demotion. A prior admit that loses correctness or materially drops
  below its guard requires revert or explicit measured demotion.
- If the row does not clear the pinned sonic strict direct threshold, revert the
  behavior/generated patch together, save `/tmp/skv13-waveW11.1-rejected.patch`,
  append REDRESS evidence, and leave RESULTS / rolling delta unchanged unless
  disposition changes.

## CH4 Cost / Noise

ACCEPT with binding measurement constraints.

- Runtime + renderer behavior delta target is <=40 net LOC, hard cap <=80 net
  LOC. Report, gate, REDRESS, and docs are outside that cap.
- No fixture/private branch, digest/hash/source hook, SIMD, parser cursor, or
  materializer edit.
- Redress must capture clean baseline and post-patch repeated A/B
  measurements: at least five runs each for Track 1, Track 2, sonic, and serde,
  recording median and min/max.
- Criterion is binding for admission. `track1_direct_to_struct` must exceed
  `sonic_rs_direct_to_struct + 1 Mbps`; if a lower confidence bound is
  available, it must clear the bar, otherwise the confidence gap is stated in
  REDRESS.

## CH5 Hidden Coupling

ACCEPT with constraints.

- Implement only the direct array-byte fast path in `parse_array_direct`; do not
  create a second substrate or row-private branch.
- Preserve parse_only and typed-plane code. Do not touch `JsonSink` trait
  methods or `JsonDirectDigest` internals.
- Runtime output and generator must remain reproducible through the codegen
  reproducibility test.
- The companion report and gate must require `--check-results`, consume
  Criterion lanes, prove strict equality across Track 1 / Track 2 / serde /
  sonic, and reject report-only closure.

## CH6 Anti-Paper-Close

ACCEPT with constraints.

- W11.1 admits only if `json/numbers/direct_to_struct/main` Track 1 clears the
  same-run sonic-rs strict direct threshold by at least 1 Mbps. The current
  pinned threshold is 12599 Mbps.
- Strict equality must pass across Track 1, Track 2, serde, and sonic. Track 2
  remains an independent oracle path.
- The companion report is consumed by `gate-json` via
  `--skv13-json-direct-reopen-report`; support-only closure is forbidden.
- If Track 1 improves but remains below or equal to sonic + 1, record a
  measured REDRESS rejection and do not call the wave admitted.

## Redress Authority

Redress may proceed on the accepted owner paths only. The implementation must
wire the generated numeric-array dispatch to the live direct consumer in the same
commit, add the W11.1 report / gate / Lock 14 owner allowance, and measure the
row plus JSON/CSS guards before disposition.
