# SK-V8 Alpha-D: SK-V7 Validated / Invalidated Ledger

Date: 2026-05-16.

Scope: Pass Alpha SK-V7 -> SK-V8 alpha-D. Sources read: SK-V7
`SYNTHESIS.md`, `SPEC.md`, `HANDOFF.md`, `skinny/RESULTS.md`,
`skinny/REDRESS.md`, and git log through `56e66ef5`.

This ledger records what SK-V7 actually validated, what it invalidated, what
must be demoted from "close target" to "substrate or evidence only", and what
remains open for SK-V8. It is intentionally stricter than the original SK-V7
forecast: any claim that did not survive `RESULTS.md` or `REDRESS.md`
measurement is treated as invalidated or demoted.

## 1. Current Measured Authority

- HEAD for this ledger: `56e66ef5` (`feat(sk-v7-wave10c): admit B6
  stack-canary Stage 1`).
- Current `skinny/RESULTS.md` overall state remains `N-direct / NoGo`.
- Parse rows: 17 of 17 are still `NO-GO` in the current schema-v3 table.
- Direct-to-struct rows: 6 of 17 are `GO` (`citm_catalog`, `apache_builds`,
  `mesh`, `marine_ik`, `numbers`, `unicode_basic`); 11 remain `NO-GO`.
- Real typed rows: 4 of 4 present rows are `GO` (`twitter`, `update_center`,
  `mesh`, `marine_ik`), but the output plane is typed direct and every row
  still records `Strictness=deferred` / `parse_utf8=view-boundary`.
- The table has schema-v3 columns and same-run sonic strict/lossy provenance,
  but `Delta vs SK-V6` is explicitly `n/a` because W0b had no
  machine-readable SK-V6 baseline binding.

## 2. Validated / Load-Bearing Wins

### V1. Strict sonic-rs comparator repair landed

- Commits: research `a5cf0969`, plan `df8beb58`, admit `ed923615`.
- Redress item: 77.
- Result: `sonic-rs` is no longer built with the global `utf8_lossy` feature.
  The bench dependency keeps only `sort_keys`; lossy sonic rows are now
  explicit flaw-probe provenance, not strict anchors.
- Validation: the comparator-plane flaw identified by SK-V6/SK-V7 was real
  and was repaired. Reverting this would restore a known invalid S anchor.
- Boundary: the W0 row-flip forecast did not hold; see I1.

### V2. Schema v3 telemetry landed and is now the gate surface

- Commits: research `9ddae991`, plan `7a3e4126`, admit `0d2fab3f`.
- Redress item: 78.
- Result: `skinny/RESULTS.md` now renders the PASS-ALPHA schema-v3 columns:
  strictness, UTF-8 boundary, flaw-probe notes, output plane, Track 1/2,
  strict and lossy sonic, sidecar competitors, deltas, hot leaf, and signal.
- Validation: same-run sonic strict/lossy provenance is explicit, and lossy
  rows are marked as flaw probes.
- Boundary: this is a reporting and provenance win, not a throughput win.
  It did not reclassify `instruments` or `unicode_basic`.

### V3. Descriptor-preserving neutral TapeKind rename landed

- Commits: research `980469b1`, plan `a2403144`, admit `89f29768`.
- Redress item: 79.
- Result: generic IR no longer exposes the seven old JSON-shaped `TapeKind`
  variant names. `DirectBuildDecode::{JsonString,JsonNumber}` became
  `DirectBuildDecode::{EscapedString,NumberScalar}`.
- Validation: grep checks found no old `TapeKind` / `DirectBuildDecode`
  spellings and no `materialization_for_rule` helper; workspace tests passed.
- Boundary: this was intentionally non-behavioral and made no performance
  claim.

### V4. Generated typed DirectBuild expansion validated the numeric Vec route

- Commits: research `0f3e0579`, plan `3c2f9854`, admit `41ecf187`.
- Redress item: 81.
- Result: `DirectTypeRef::Vec` gained `capacity_hint`, helper names include
  the hint, Vec helpers allocate with `Vec::with_capacity`, and generated
  real typed rows were added for `mesh` and `marine_ik`.
- Validation: W3 closed its typed gate:
  - `mesh real_typed_struct`: Track 1 9466 Mbps, Track 2 8089 Mbps,
    sonic strict 8696 Mbps, `GO`.
  - `marine_ik real_typed_struct`: Track 1 12020 Mbps, Track 2 9630 Mbps,
    sonic strict 8750 Mbps, `GO`.
  - `mesh direct_to_struct` and `twitter real_typed_struct` guard rows stayed
    `GO`.
- Boundary: this validates generated typed output-schema work, not a broad
  retained parse close and not a general direct-to-struct close.

### V5. Lock 14 cleanup landed in two load-bearing phases

- Phase A+B commits: research `4be402b6`, plan `36bb9df5`, admit `f786e597`.
- Phase C+D commits: research `7c5e8ad6`, plan `ddab18a8`, admit `7c6837b8`.
- Redress items: 85 and 86.
- Result: public JSON-prefixed matcher names were removed from
  `parse-that-regex`, JSON binding helpers were removed from `passes`, generic
  codegen module/API names were neutralized, `StructuralAlphabet::json()` was
  removed, and schema-direct/sink-direct lowering now validates grammar-derived
  facts rather than JSON allowlists.
- Validation: audit greps passed, generated JSON output and `skinny/RESULTS.md`
  were unchanged, root regen checks passed, and workspace tests passed.
- Boundary: per-grammar JSON names remain only where expected: JSON grammar
  inputs and emitted JSON parser output. Lock 14 cleanup did not claim
  throughput.

### V6. CostFacts substrate projection landed

- Commits: research `ae063b43`, plan `457bc7a8`, admit `51d8c8be`.
- Redress item: 87.
- Result: grammar-neutral `ir::cost` exists with `CostFacts`, shape rationale,
  rejected alternatives, measurement evidence, capacity policy, and priority
  steps. `LayoutFacts.backend_shape` is now a projection of `CostFacts.chosen`.
- Validation: `xtask gate-json --with-cost-facts --advisory` emitted parseable
  schema `sk-v7-costfacts-v1` with 15 CostFacts entries, at least four
  rejected alternatives per entry, REDRESS 72 backfill evidence, and diagnostics
  `BBNF-DOMINATED-ALTERNATIVE` plus `BBNF-COSTFACTS-MISSING-EVIDENCE`.
- Boundary: this is substrate/evidence plumbing. Generated JSON outputs and
  `RESULTS.md` stayed unchanged.

### V7. B6 canary hardening landed as Stage 1

- Commits: research `b99cf338`, plan `0fc24d1a`, admit `56e66ef5`.
- Redress item: 90.
- Result: the fixed `0xDE` volatile-probe checkasm canary was replaced by a
  shared randomized XOR-fold helper with byte-exact backstop diagnostics.
- Validation: representative negative canary controls failed closed for
  bitmap-next-bit, bulk-emit, byte-class, and parity wrappers; static audits
  found no old fixed canary pattern in `checkasm_*.rs`.
- Boundary: W10c is test-harness hardening only. It has zero production bitmap,
  runtime, generated JSON, or `RESULTS.md` diff.

## 3. Invalidated Candidates

### I1. W0 row-flip forecast was invalidated

- Commit: admit `ed923615`; redress item 77.
- Original forecast: strict sonic rebuild would slow sonic uniformly by 3-8%
  and flip `instruments` and `unicode_basic`.
- Measurement: sonic parse deltas ranged from -14.6% to +18.5% rather than a
  uniform slowdown. `instruments` stayed `G/NO-GO` and `unicode_basic` stayed
  `G/NO-GO`.
- Current authority: `instruments parse_only` is still `NO-GO`; `unicode_basic
  parse_only` is still `NO-GO`.

### I2. W2 zero-fallback mantissa-widen route was rejected

- Commits: research `c3d6e102`, plan `3d8bb04a`, redress `78d83497`.
- Redress item: 80.
- Original forecast: canada direct would close by eliminating f64 fallback.
- Measurement: fresh canada attribution found 111126 numbers, 111080 f64
  candidates, zero mantissa overflows, zero ambiguous Eisel-Lemire returns, and
  zero `str::parse::<f64>()` fallbacks.
- Outcome: no same-wave consumer existed; no source patch was attempted.
  Canada direct remained `N-direct / NO-GO` at Track 1 10773 Mbps, Track 2
  10296 Mbps, sonic strict 12421 Mbps.

### I3. W4 single-quartet Unicode escape classifier was rejected

- Commits: research `3f1828e6`, plan `25fc2b79`, redress `17bd39b1`.
- Redress item: 82.
- Original forecast: `unicode_escapes` and `y_string_unicode` would lift via a
  per-quartet TBL-backed decoder.
- Measurement: correctness and checkasm passed, but the falsifiability gate
  failed. `unicode_escapes parse_only` reached only 82.1% of sonic against a
  95% threshold; `y_string_unicode parse_only` reached only 49.9% against a
  70% threshold; direct rows stayed far below threshold and one Track 2 guard
  regressed.
- Outcome: do not reopen this per-quartet materializer helper as the SK-V8
  close.

### I4. W5 generated-retained StringBlock16 tiny probe was rejected

- Commits: research `893ce6fb`, plan `21e6c66f`, redress `db761873`.
- Redress item: 83.
- Original forecast: a 16-byte plain-string block probe would lift 4 of 6
  string-bound parse rows.
- Measurement: zero of six rows crossed threshold and every named Track 1
  parse row regressed more than the allowed 3% guard.
- Affected rows: `twitter`, `update_center`, `unicode_basic`, `random`,
  `unicode_mixed`, and `distinct_values`.
- Outcome: the existing AArch64 `string_block` movemask shape is too expensive
  for the already-tiny generated retained quote-pair probe.

### I5. W6 object-pair value-byte control compaction was rejected

- Commits: research `bfa2f9e3`, plan `e045e008`, redress `58479e29`.
- Redress item: 84.
- Original forecast: citm Track 2 and instruments parse/direct would close
  through value-byte dispatch compaction.
- Measurement: `citm_catalog parse_only` Track 2 stayed below 90% sonic,
  `instruments parse_only` stayed below 100% sonic, `instruments
  direct_to_struct` stayed `N-direct / NO-GO`, and `citm_catalog` Track 1
  violated the no-regression guard.
- Outcome: returning the post-colon value byte is too small and too
  layout-sensitive to close the W6 rows.

### I6. W10 PMULL prefix-XOR body was rejected

- Commits: research `814118fd`, plan `ae6ebd79`, redress `db913136`.
- Redress item: 88.
- Original forecast: admit PMULL prefix-XOR and CSSC/CTZ next-bit bodies with
  a same-wave consumer plus B6 canary fold.
- Measurement: checkasm and asm proof passed, but JSON parse rows regressed
  before `RESULTS.md` could be admitted: notable drops included instruments,
  numbers, and unicode_escapes Track 1/2 rows.
- Outcome: PMULL as the default hot `bitmap_prefix_xor_64` body is not
  admissible for escape-heavy and narrow parse-only JSON rows on this host.

### I7. W10b CTZ/bulk consumer was rejected

- Commits: research `508dfd16`, plan `7f3200df`, redress `0cd00886`.
- Redress item: 89.
- Original forecast: keep prefix-XOR scalar, admit CTZ next-bit through
  `bulk_emit_positions_64_neon`, and retain B6 canary fold.
- Measurement: checkasm, static audit, negative canary controls, and explicit
  `ctz` asm proof passed; however the refreshed `RESULTS.md` comparison showed
  more than 2% drops on six Track 1/2 rows, including canada, citm_catalog,
  instruments, marine_ik, mesh, and numbers.
- Outcome: the production-path `bitmap_next_set_bit` / bulk consumer change
  violates the W10b maintain invariant. CTZ/bulk remains rejected for SK-V7.

## 4. Demoted Claims

### D1. Real typed wins are valid typed-output wins, not SOTA parse proof

- Relevant commits: SK-V6 typed DirectBuild `ab06ff11`; SK-V7 Vec expansion
  `41ecf187`.
- The current real typed rows are useful and measured (`twitter`,
  `update_center`, `mesh`, `marine_ik` are `GO`), but they consume a typed
  output plane with a structural oracle and `Strictness=deferred`.
- The SK-V7 C3 diagnosis still applies: `twitter real_typed_struct` is largely
  skip-work over dropped fields, not proof that the retained parse path beats
  DOM-class parsers.

### D2. Generated direct work remains correctness-green but not broadly closed

- Relevant prior commit: SK-V5 generated SinkOnly lowerer `d37f1cc2`; SK-V7
  current authority from schema-v3 `RESULTS.md`.
- The current direct-to-struct plane is generated Track 1 SinkOnly vs an
  independent hand Track 2 SinkOnly and remains a valid gate surface.
- Demotion: the original SK-V7 direct forecast of 10-12 PASS did not hold.
  Current direct rows are 6 of 17 `GO`; 11 remain `NO-GO`.

### D3. B6 hardening is admitted only as canary Stage 1

- Relevant commits: W10 rejection `db913136`, W10b rejection `0cd00886`, W10c
  admit `56e66ef5`.
- The B6 canary work is a real harness-integrity improvement, but the original
  W10 exit gate required bitmap primitives plus consumers. That gate did not
  go green.
- Demotion: SK-V8 must not treat B6 as having admitted PMULL or CTZ/bulk.

### D4. Strict comparator repair did not close the strictness story

- Relevant commits: `ed923615`, `0d2fab3f`.
- The comparator side is repaired, but current bbnf rows still record
  `Strictness=deferred` and `parse_utf8=view-boundary`.
- Demotion: the strict-vs-strict SOTA posture is not fully closed until the
  bbnf side has scan-boundary strictness or the deferred boundary is otherwise
  resolved in the gate semantics.

### D5. Lock 14 cleanup is validated as neutrality cleanup, not performance

- Relevant commits: `89f29768`, `f786e597`, `7c6837b8`.
- The cleanup is load-bearing for architecture and future substrate work.
- Demotion: because generated outputs and `RESULTS.md` stayed unchanged, it
  does not provide a runtime or throughput close by itself.

## 5. Predicted SK-V7 Close Targets That Did Not Hold

- Overall close: not held. Current gate remains `N-direct / NoGo`.
- Parse close: not held. SK-V7 predicted 9-11 parse PASS and HANDOFF required
  at least 6 parse rows PASS. Current schema-v3 table has 0 parse `GO` rows.
- Direct close: not held. SK-V7 predicted 10-12 direct PASS and HANDOFF
  required at least 10. Current table has 6 direct `GO` rows.
- Typed close: held numerically but with output-plane caveat. SK-V7 predicted
  3-5 typed PASS; current table has 4 real typed `GO` rows, all on typed
  direct output with deferred strictness.
- W0 row flips: not held for `instruments parse_only` or `unicode_basic
  parse_only`.
- W2 canada direct close: not held.
- W4 unicode escape close: not held for `unicode_escapes` or
  `y_string_unicode`.
- W5 string-bound close: not held for `twitter`, `update_center`,
  `unicode_basic`, `random`, `unicode_mixed`, or `distinct_values`.
- W6 control/key close: not held for `citm_catalog` Track 2 or `instruments`
  parse/direct.
- W10 primitive admission: not held. PMULL and CTZ/bulk both remain rejected.
- Strict-vs-strict closure: not held. The comparator is repaired, but bbnf rows
  remain view-boundary/deferred.

## 6. Still Open for SK-V8

- Twitter parse and the yyjson gap remain the largest hard residual. Current
  `twitter parse_only` is Track 1 15752 Mbps vs sonic strict 21020 Mbps,
  simdjson DOM 24522 Mbps, and yyjson 30931 Mbps.
- Parse-plane close remains open across all 17 fixtures. Several parse rows are
  fast in raw Mbps, but the current classifier still records every parse row as
  `NO-GO`.
- Direct-to-struct close remains open for 11 rows: `twitter`, `canada`,
  `github_events`, `update_center`, `random`, `gsoc-2018`, `instruments`,
  `unicode_mixed`, `unicode_escapes`, `distinct_values`, and
  `y_string_unicode`.
- The SK-V7 rejected hot-leaf routes are pre-blocked unless SK-V8 brings fresh
  PC-level evidence and a structurally different same-row consumer:
  mantissa-widen fallback elimination, per-quartet Unicode materializer helper,
  StringBlock16 tiny wrapper, object-pair value-byte dispatch, PMULL default
  prefix-XOR, and CTZ/bulk production consumer.
- The bbnf strictness boundary remains open: current rows disclose
  `view-boundary`, not scan-boundary strictness.
- `Delta vs SK-V6` remains unbound in schema-v3 `RESULTS.md` until a
  machine-readable SK-V6 baseline is made available or the delta field is
  redefined for forward-only tranches.
- PMULL and CTZ/bulk should route to SK-V8 only as narrowly gated primitive
  candidates with same-row non-regression proof. They are not SK-V7 wins.
- B6 canary hardening can be carried forward as infrastructure, but not as a
  bitmap primitive admission.

## 7. SHA Index

Validated:

- `ed923615` - strict comparator repair.
- `0d2fab3f` - schema-v3 telemetry row builder.
- `89f29768` - descriptor-preserving TapeKind rename.
- `41ecf187` - capacity-hinted numeric Vec real typed expansion.
- `f786e597` - Lock 14 Phase A+B neutralization.
- `7c6837b8` - Lock 14 Phase C+D codegen shell neutralization.
- `51d8c8be` - CostFacts substrate projection.
- `56e66ef5` - B6 stack-canary Stage 1.

Rejected or demoted:

- `78d83497` - W2 zero-fallback mantissa-widen redress rejected.
- `17bd39b1` - W4 single-quartet Unicode escape classifier rejected.
- `db761873` - W5 generated-retained StringBlock16 tiny probe rejected.
- `58479e29` - W6 object-pair value-byte control compaction rejected.
- `db913136` - W10 consumed aarch64 bitmap bodies and B6 canary fold rejected.
- `0cd00886` - W10b CTZ bulk consumer and B6 canary fold rejected.

Research/plan context:

- `8312261b` - original SK-V7 synthesis/spec/handoff packet.
- `a5cf0969`, `df8beb58` - W0 comparator-plane research and plan.
- `9ddae991`, `7a3e4126` - W0b schema-v3 research and plan.
- `980469b1`, `a2403144` - W1 TapeKind research and plan.
- `c3d6e102`, `3d8bb04a` - W2 mantissa research and plan.
- `0f3e0579`, `3c2f9854` - W3 DirectBuild Vec research and plan.
- `3f1828e6`, `25fc2b79` - W4 Unicode escape research and plan.
- `893ce6fb`, `21e6c66f` - W5 plain-string scan research and plan.
- `bfa2f9e3`, `e045e008` - W6 control/key research and plan.
- `4be402b6`, `36bb9df5` - W7 Lock 14 Phase A+B research and plan.
- `7c5e8ad6`, `ddab18a8` - W8 Lock 14 Phase C+D research and plan.
- `ae063b43`, `457bc7a8` - W9 CostFacts research and plan.
- `814118fd`, `ae6ebd79` - W10 asm body research and plan.
- `508dfd16`, `7f3200df` - W10b CTZ/bulk research and plan.
- `b99cf338`, `0fc24d1a` - W10c B6-only research and plan.
