# SK-V6 A4 local history synthesis

Date: 2026-05-15.
Workspace read-only: `/Users/mkbabb/Programming/bbnf-lang`.

Sources read: `skinny/REDRESS.md`, `skinny/RESULTS.md`,
`restart/HANDOFF.md`, `restart/skinny/tranches/sk-v5/HANDOFF.md`,
`restart/skinny/tranches/sk-v5/SYNTHESIS.md`, and all reports under
`restart/skinny/tranches/sk-v6/research/`.

## Current binding gate

Binding authority is `skinny/RESULTS.md`, as reaffirmed by REDRESS 58 and the
SK-V5/SK-V6 handoffs.

- Overall: `N-direct / NoGo`.
- Retained parse matrix: 17 rows total; 4 A / GO rows (`canada`, `mesh`,
  `marine_ik`, `numbers`) and 13 G / NO-GO rows (`twitter`, `citm_catalog`,
  `apache_builds`, `github_events`, `update_center`, `random`, `gsoc-2018`,
  `instruments`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`,
  `distinct_values`, `y_string_unicode`).
- Retained structural scan: Canada is green at 69075 Mbps against the 40000
  Mbps NEON floor. This is admitted by REDRESS 56.
- Direct `semantic_full_digest_stressor`: 17 rows total; correctness PASS on
  all rows; 4 throughput PASS rows (`citm_catalog`, `apache_builds`,
  `github_events`, `instruments`) and 13 NO-GO rows (`twitter`, `canada`,
  `update_center`, `mesh`, `random`, `gsoc-2018`, `marine_ik`, `numbers`,
  `unicode_mixed`, `unicode_escapes`, `unicode_basic`, `distinct_values`,
  `y_string_unicode`).
- Representative `real_typed_struct`: 2 rows total; `twitter` and
  `update_center` both PASS under the host/API output-schema plane admitted by
  REDRESS 71.
- Strictness/output plane: bbnf rows are disclosed as `deferred /
  view-boundary / yes`; sidecar strictness/output-plane columns are present.

Current retained parse row values, Mbps:

| Row | Outcome | T1 | T2 | S anchor | T1/S | T2/S |
|---|---|---:|---:|---:|---:|---:|
| twitter | G / NO-GO | 15597 | 12128 | sonic-rs 21184 | 73.6% | 57.2% |
| citm_catalog | G / NO-GO | 32459 | 20792 | sonic-rs 24910 | 130.3% | 83.5% |
| canada | A / GO | 18775 | 17133 | sonic-rs 12658 | 148.3% | 135.4% |
| apache_builds | G / NO-GO | 12638 | 12227 | sonic-rs 16206 | 78.0% | 75.4% |
| github_events | G / NO-GO | 15268 | 13034 | sonic-rs 22182 | 68.8% | 58.8% |
| update_center | G / NO-GO | 11912 | 9226 | sonic-rs 19983 | 59.6% | 46.2% |
| mesh | A / GO | 14330 | 13173 | sonic-rs 11837 | 121.1% | 111.3% |
| random | G / NO-GO | 10071 | 7800 | sonic-rs 15370 | 65.5% | 50.7% |
| gsoc-2018 | G / NO-GO | 23161 | 21870 | sonic-rs 43207 | 53.6% | 50.6% |
| marine_ik | A / GO | 13688 | 12801 | sonic-rs 10064 | 136.0% | 127.2% |
| instruments | G / NO-GO | 18163 | 11826 | sonic-rs 19737 | 92.0% | 59.9% |
| numbers | A / GO | 20085 | 18671 | sonic-rs 13567 | 148.0% | 137.6% |
| unicode_mixed | G / NO-GO | 8914 | 8940 | sonic-rs 15892 | 56.1% | 56.3% |
| unicode_escapes | G / NO-GO | 12905 | 12931 | sonic-rs 16048 | 80.4% | 80.6% |
| unicode_basic | G / NO-GO | 12193 | 10782 | sonic-rs 13304 | 91.7% | 81.0% |
| distinct_values | G / NO-GO | 9783 | 6100 | sonic-rs 16259 | 60.2% | 37.5% |
| y_string_unicode | G / NO-GO | 6290 | 6034 | sonic-rs 13673 | 46.0% | 44.1% |

## Major implemented landings

- Report truthfulness and reproducibility landed: Mbps reporting, S-anchor
  disclosure, strictness/output-plane columns, masking probes, materialization
  stats, peak RSS/report metadata, `CARGO_TARGET_DIR` routing, advisory bench
  mode, and executable gate behavior. REDRESS 1, 8, 9, 10, 26, 32, 41, 47.
- Parser/substrate cleanup landed: structural scan and parser parse-index are
  split, Track 1 and Track 2 share the one-buffer `TapeBuilder`, whitespace
  ownership is caller-boundary correct, direct views project over sealed tape,
  payload arena stays cold, generated JSON API definitions own their files,
  close-token elision is canonical, parser-grade structural byte vectors are
  removed, lazy-offset tape is implemented, sparse flags and spare-capacity
  offset writes landed. REDRESS 2, 3, 4, 5, 6, 11, 13, 14, 20, 23.
- Hot-path parser wins landed without changing the substrate: cold errors,
  SWAR digit runs, SWAR plain strings, delimiter fusion, newline-indent space
  skipper, `parse_value_at` split, short plain-string fast path, and Track 2
  inline parity. REDRESS 12, 24, 28, 42, 44.
- Primitive/runtime SIMD path was made auditable: `bbnf-simd` replaced the old
  scanner surface, Lock 15 release/fusion discipline was enforced, two-layer
  primitive vocabulary was recorded, consumed primitives were admitted only
  with same-wave consumers, and Canada scan-floor repair admitted
  `BULK_EMIT_POSITIONS_64` plus the structural+terminator classifier. REDRESS
  21, 22, 28, 29, 56.
- Codegen/direct authority landed: BIR carries tape/direct markers,
  `BackendShape`, `LayoutFacts.backend_shape`, `derive_backend_shape`, and
  `codegen/src/lower/` exist; generated SinkOnly now comes from a
  BIR-derived `SinkOnlyProgram`; Track 1 direct calls generated
  `parse_direct`; Track 2 remains structurally independent. REDRESS 7, 34,
  35, 40, 48.
- Number materialization was moved from research gap to implementation
  substrate: Eisel-Lemire and integer materializers were vendored/wired into
  `parse-that-regex::number`, used by generated and hand SinkOnly. This
  validated the number lever as necessary while current `RESULTS.md` keeps
  several direct numeric rows red due the full dual-track gate. REDRESS 31, 39,
  46.
- Direct string-source hooks and receiver/source-shape redress landed: generated
  direct preserves raw string spans to `JsonSink::*_source`, receiver hot leaves
  were inlined, and generated direct tiny-plain-string routing was admitted as
  direct-only improvement. REDRESS 49, 57.
- Retained parser-control array continuation landed: generated retained arrays
  now use `ContainerNext` / next-byte carry into `dispatch_value` without
  re-entering `parse_value_at`; it improved container-transition attribution
  but did not close parse-G. REDRESS 63.
- Generated-retained-only cap-16 landed: native rerun admitted widening
  `match_tiny_plain_string` from 8 to 16 only for generated retained
  `OffsetTape`. Generated direct, hand retained Track 2, and hand direct Track
  2 remain cap 8. REDRESS 72.
- Host/API schema-source typed DirectBuild landed for representative rows:
  typed `DirectBuild` now accepts host/API output schema facts without a BBNF
  directive or new BIR variant, and `real_typed_struct` passes for `twitter`
  and `update_center`. REDRESS 70 rejected the hand-authored proof; REDRESS 71
  admits the generated schema-source shape.

## Measured wins and retained value

- Historical skinny triad remains useful substrate evidence, but not the close
  condition. The expanded gate is binding. REDRESS 20, 26, 58.
- Canada structural-only scan recovered from stale L / NO-GO to 69075 Mbps vs
  40000 Mbps floor. The remaining misses are not scanner-floor misses.
  REDRESS 56.
- Lazy tape materialization is real and measured: offsets plus sparse flags,
  zero payload bytes, and per-node counts are reported. Example current notes:
  twitter 29573 offsets / 133632 allocated tape bytes; citm_catalog 85035 /
  524312; canada 223236 / 1048576. REDRESS 9, 20, 23.
- Payload arena stays cold on JSON: Track 1 and Track 2 publish 0/0
  writes/allocations across corpus notes. REDRESS 6, 8.
- Direct correctness is now honest and green: exact generated Track 1 / hand
  Track 2 digest equality plus sonic-rs and serde_json shape parity. REDRESS
  30, 34, 40, 48, 58.
- Direct digest pass count improved to four current rows after generated direct
  and receiver/source-shape work: `citm_catalog`, `apache_builds`,
  `github_events`, `instruments`. REDRESS 57; current authority
  `skinny/RESULTS.md`.
- Candidate 4 `ContainerNext` measured broad focused retained wins before
  admission: side-by-side medians included `mesh` +7.34%, `random` +6.46%,
  `unicode_mixed` +7.51%, `unicode_escapes` +5.84%, plus PC attribution
  collapse of old boundary share on `citm_catalog` and `canada`. REDRESS 63.
- Candidate 13 cap-16 generated-retained split measured native Track 1 wins:
  `twitter` +27.5%, `citm_catalog` +49.2%, `github_events` +16.9%,
  `update_center` +27.4%, `random` +21.8%, `instruments` +44.9%,
  `distinct_values` +57.5%, `unicode_basic` +9.9%. REDRESS 72.
- Representative typed DirectBuild rows pass under the right output plane:
  `twitter.real_typed_struct` Track 1 278.67 us vs sonic-rs 422.12 us, and
  `update_center.real_typed_struct` Track 1 354.15 us vs sonic-rs 351.23 us
  within 1.10x slack. REDRESS 71.

## Regressions, failed candidates, and no-go routes

Older substrate perturbations rejected:

- Pair-token fusion reduced token count but regressed key rows. REDRESS 16.
- Function-pointer/dispatch-table route was invalid: first probe duplicated
  Track 1; real table regressed and was reverted. REDRESS 17.
- Skipless 12-byte tape token reduced logical bytes but did not cleanly improve
  throughput. REDRESS 18.
- Structural-index typed parser prepass, NEON no-escape matcher, separator
  elision, generic SWAR whitespace skipper, 12-byte/width churn, and dispatch
  alternates remain rejected. REDRESS 25.
- Capacity Plan A sampled heuristic, Plan B exact prescan, Plan C oneshot SIMD
  prescan are rejected; Plan D grow-only is the production capacity default.
  REDRESS 27, 28.

SK-V5 refutations:

- Active Class A `match_tiny_plain_string` NEON wiring is invalidated as a
  parse-G fix. It had been wired previously and regressed twitter about 25%.
  REDRESS 28, 33.
- Post-escape skip / validation-batch route regressed `unicode_escapes` and
  was redressed. REDRESS 43.
- Direct digit-prefix / local digit scan did not close the numeric direct rows
  and was rejected while shared number materialization remained necessary.
  REDRESS 46.
- Direct no-allocation decoded visitor was admitted only as source-hook seam
  evidence and rejected as a no-allocation decoded visitor route. REDRESS 49.
- Retained parse-time aux side tables were rejected: they improved view probes
  but regressed retained parse. REDRESS 50.
- Byte-class whitespace `EventCursor` wrapper was rejected. REDRESS 51.
- Parser-local structural-mask cursor was rejected, even with O(1) pending
  state and live emit mask consumption, because it was still a second scanner
  beside source-byte recursive descent. REDRESS 53.
- Exact decoded-string stats sink and quote-source fused string materializer
  were both rejected; both lost to the default allocate-then-contiguous-hash
  baseline on escaped/string rows. REDRESS 54, 55.
- The broad SK-V5 Wave 3 "UTF-8 fusion closes parse-G/direct strings" claim is
  refuted as a close route by REDRESS 50, 51, 53, 54, 55 and recorded as
  dispatch hygiene in REDRESS 59.

SK-V6 retained parse candidates:

- Candidate 1 deleted the retained tiny-string probe and regressed every
  measured row (`twitter` -20.5%, `random` -39.9%, `unicode_basic` -46.9%,
  etc.). The tiny scalar early-out is load-bearing. REDRESS 60.
- Candidate 2 long-string trusted 64-byte scanner had row-local wins but failed
  the full matrix; `canada` -9.8%, `instruments` -7.5%, only `gsoc-2018` hit
  the 10% full-matrix lift target. REDRESS 61.
- Candidate 3 delayed-wide trusted scanner still regressed sentinel rows
  (`distinct_values` -8.49%, `twitter` -7.46%, `update_center` -5.28%) and was
  reverted. REDRESS 62.
- Candidate 4 array `ContainerNext` is admitted but not a close. REDRESS 63.
- Candidate 5 retained Unicode-escape run validator improved
  `unicode_escapes` +31.82% but failed companion rows, including
  `y_string_unicode` -3.72%; rejected. REDRESS 64.
- Candidate 6 object next-key carry had sub-1% gains and missed required row
  lifts; rejected. REDRESS 65.
- Candidate 14 Track 2 array next-byte parity repair helped `citm_catalog` but
  regressed guard `apache_builds` from about 10475 to about 7490 Mbps; rejected.
  REDRESS 73.

SK-V6 direct candidates:

- Candidate 7 direct source-hook field-layout receiver shortcut moved target
  rows only about 0-2%; rejected. REDRESS 66.
- Candidate 8 parser-owned decoded scratch regressed `unicode_escapes` -44.03%,
  `unicode_mixed` -4.91%, and `y_string_unicode` -16.76%; rejected. REDRESS 67.
- Candidate 9 byte-output `unescape_json_string` regressed primary
  `unicode_escapes` -4.00%; rejected. REDRESS 68.
- Candidate 10 semantic string facts for the current digest stressor regressed
  `unicode_escapes` about -15%; rejected. REDRESS 69.
- Candidate 11 hand-authored typed sink proved a measurement surface but not
  grammar-general DirectBuild authority and missed the SOTA close; rejected as
  proof. REDRESS 70.
- Candidate 12 schema-source typed DirectBuild is admitted for representative
  typed rows, but it explicitly does not erase the existing
  `semantic_full_digest_stressor` guard or the 13 current digest misses.
  REDRESS 71.

Primitive/workflow no-go routes:

- Orphan primitives remain blocked: `BULK_EMIT_COMPRESSED`,
  `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`, and `FSM_DISPATCH_THREADED`
  cannot be admitted without same-wave generated/runtime consumers. REDRESS
  Wave 5 primitive admission section.
- Register-clobber sentinels around arbitrary Rust closures were found
  unsound; stack canaries stay for Rust candidate calls, raw register sentinels
  are reserved for explicit FFI/ASM call boundaries. REDRESS Wave 5 primitive
  admission section.
- Sidecar target churn does not explain current parse-G. R5c classifies the
  remaining parse-G state as parser-owned; current S anchor in `RESULTS.md` is
  sonic-rs for every retained row.
- i-cache / branch-capacity explanation is not supported. Lock 15 holds; fused
  hot function sizes are below the 20 KiB budget; PMU was unavailable, and
  proxy evidence points to string leaves, not front-end capacity. R6/R6c.

## Validated assumptions

- Expanded 17-row matrix, not the historical triad, is the close condition.
  REDRESS 20, 26, 58.
- Strictness disclosure is load-bearing. Deferred UTF-8 validation at parse
  boundary must be disclosed and cannot be read as strict-vs-strict against
  permissive or scan-boundary sidecars. REDRESS 47; SK-V5 B3; R5/R5c.
- Track 1 direct attribution is now honest: generated `parse_direct` reaches
  the stack on current profiles, replacing the old bench-private SinkParser.
  REDRESS 34, 40, 48; R3/R4c.
- Track 2 independence matters: many retained rows remain G because Track 2 is
  below the S anchor even where generated Track 1 improved after cap-16.
  REDRESS 72, 73.
- The tape/structural projection union remains the right substrate, but
  parse-time aux side tables and cursor sidecars are not the union. REDRESS 50,
  51, 53; SK-V5 A4.
- Tiny string scalar early-out is load-bearing in retained parse. Removing it
  was broadly negative; widening it is only admitted for generated retained
  Track 1 under native build. REDRESS 60, 72.
- String boundary remains the coarse retained parse blocker, but not the old
  raw UTF-8 validator leaf. R1/R2/R4 show current generated retained parse is
  dominated by `match_tiny_plain_string` and `match_string_at_quote` over a
  trusted `&str` input path.
- Direct digest misses are a representation/output-plane issue, not merely a
  missing local decoder trick. The digest stressor forces every semantic
  key/string/number byte through full semantic digest work. R2f/R2g.
- Host/API output schema is the correct place for typed DirectBuild facts; not
  BBNF syntax, not a new top-level BIR variant, and not a hidden
  benchmark-private parser. REDRESS 70, 71; schema A/B/C.
- Number/Eisel-Lemire work was necessary, but not sufficient for the full
  direct close. REDRESS 31, 39, 46; current `RESULTS.md`.
- Lock 15 holds by code size; no i-cache split is justified without new
  same-row evidence. R6/R6c.
- Lock 16 consumed-primitive discipline is correct: primitive parity is not
  sufficient without same-wave hot consumers. REDRESS 28, 29, 56.

## Invalidated assumptions

- "Canada structural floor is the blocker" is invalidated. The floor is green;
  Canada retained parse is A / GO. REDRESS 56.
- "Original triad pass means close" is invalidated. Full gate remains
  `N-direct / NoGo` with 13 retained G rows and 13 direct digest misses.
  REDRESS 20, 26, 58.
- "Class A tiny-string NEON is the parse-G fix" is invalidated. REDRESS 28,
  33, 60.
- "Fold raw UTF-8 validation into the NEON string body and close SK-V5" is
  invalidated for the current generated baseline. REDRESS 59; R1/R2/R4.
- "Long string threshold/wide scanner solves retained parse" is invalidated by
  Candidates 2 and 3 and post-C4 string-distribution analysis. REDRESS 61, 62;
  R2c/R4b.
- "Deleting the tiny probe removes duplicate scan cost" is invalidated.
  REDRESS 60.
- "Object next-key carry is the remaining parser-control close" is invalidated
  as too small. REDRESS 65.
- "Generated helper shape transfers monotonically to hand Track 2" is
  invalidated by Candidate 14. REDRESS 73.
- "Receiver/source-hook overhead is the direct string bottleneck" is
  invalidated. REDRESS 66.
- "Allocator reuse/parser-owned scratch closes direct escaped strings" is
  invalidated. REDRESS 67.
- "Manual byte-output inside the existing `Cow<str>` API is faster" is
  invalidated. REDRESS 68.
- "Semantic string fact hashing closes the current digest workload" is
  invalidated. REDRESS 69.
- "Hand-authored typed sink proves grammar-general DirectBuild" is invalidated.
  REDRESS 70.
- "Sidecar anchor movement explains parse-G" is invalidated. R5c.
- "i-cache/front-end capacity is the current retained blocker" is invalidated
  on available evidence. R6/R6c.

## Cohort synthesis by theme

- R1/R2/R4 parse attribution: current generated retained parse is string
  boundary dominated on many red rows, with `match_tiny_plain_string` and
  `match_string_at_quote` replacing the old SK-V5 `validate_utf8_codepoint`
  leaf. Structural/container churn remains visible on `citm_catalog`; number
  plus container remains visible on `marine_ik`, which is current GO.
- R1b/R1c/R2c/R4b string-route analysis: keep the tiny probe; no defensible
  retained threshold remains after Candidates 1-3. Escape-heavy rows are not
  solved by raw length alone, and corpus distribution is not an admissible
  lowering fact.
- R2b/R3c parser-control analysis: array `ContainerNext` was the one admitted
  parser-control win; after it, value re-entry is no longer the dominant
  retained issue. Offset/tape emission is real but single-digit on most rows;
  object next-key carry was the only narrow non-string follow-up and failed.
- R3/R4c/R2f direct attribution: generated Track 1 direct now reaches
  `parse_direct`; red rows split into string scan/materialization, receiver
  fold, number materialization, and generated object/control cost. The common
  cause is late semantic `&str` event representation for a digest stressor, not
  one local function.
- R2g/R1g/schema A/B/C: split representative typed DirectBuild from maximal
  semantic digest stressor. Typed output needs an explicit host/API schema
  source that feeds DirectBuild field facts; digest remains a guard.
- R5/R5c sidecars: current strict in-tree sidecars are sonic-rs, simd-json
  borrowed/owned, sonic-rs direct, and serde_json direct. Stale C++/yyjson
  profiles are useful for eventual SOTA claims but do not change current
  `RESULTS.md` gate accounting.
- R6/R6b/R6c measurement discipline: use production `profile-lazy` all-row
  smoke, parse-attribution only on target/sentinel rows, c/B and PC-bin deltas,
  then full `bench-json --advisory`. Do not close on focused one-row toggles or
  aggregate wrapper percentages.

## REDRESS item map

- 1: Mbps report units landed.
- 2: Structural-only scan split from parser parse-index.
- 3: Track 1 and Track 2 share one-buffer `TapeBuilder`.
- 4: Parser whitespace ownership corrected.
- 5: Direct/tape projection kept as one substrate.
- 6: JSON payload arena remains cold.
- 7: BIR materialization events landed.
- 8: Bench metadata counters are observed, not hardcoded.
- 9: Tape materialization report artifact landed.
- 10: Masking probes report artifact landed.
- 11: Generated runtime files own JSON API definitions.
- 12: JSON number/whitespace scanners tightened.
- 13: Close-token elision canonicalized.
- 14: Parser-grade structural byte vector removed.
- 15: Tape sealing semantics recorded; lazy mode uses offset/flag tape.
- 16: Pair-token fusion rejected.
- 17: Dispatch-table alternate invalidated/rejected.
- 18: Skipless 12-byte token rejected.
- 19: Host-call overhead split from eager decode; eager decode is MASKING.
- 20: Lazy-offset tape-union migration implemented and measured.
- 21: Lock 15 release-profile discipline enforced.
- 22: `bbnf-simd` replaced runtime scanner dependency surface.
- 23: Sparse flags and spare-capacity offset writes landed.
- 24: Parser hot-path wins landed without substrate change.
- 25: Measured alternates remain rejected.
- 26: Bench auditability gates landed.
- 27: SK-V3 reprofile split blockers by mechanism.
- 28: SIMD parity/Plan D/admitted host primitives; active 16-byte tiny dispatch rejected.
- 29: Two-layer primitive vocabulary recorded.
- 30: Direct-to-struct is a throughput gate.
- 31: Direct sink profiling moved blocker to materialization leaves.
- 32: Gate status and budget-cliff behavior executable.
- 33: Class A retained tiny-string NEON invalidated as parse-G fix.
- 34: Bench-private SinkParser dishonesty identified and closed.
- 35: Codegen lowerer scaffolding and SinkOnly authority closed, with residual.
- 36: JSON-hardcoded scalar references in `bbnf-simd` identified.
- 37: `bbnf-simd/src/lib.rs` JSON god-module status identified.
- 38: `crates/simd-scan/` fossil identified for deletion.
- 39: Eisel-Lemire and integer materialization wired, with residual.
- 40: Generated `SinkOnly` is Track 1 direct workload; gate remains NoGo.
- 41: `CARGO_TARGET_DIR` gate/metadata routing corrected.
- 42: Trusted-UTF8 string boundary matching validated as necessary, insufficient.
- 43: Post-escape skip/validation-batch route invalidated.
- 44: Direct Track 2 false strict-string penalty removed.
- 45: Wave 3 correctness/primitive gates green, exit gate did not fire.
- 46: Direct-number/context-sink improved numeric rows, gate remains NoGo.
- 47: Advisory bench mode and output-plane disclosure corrected.
- 48: SinkOnly lowerer consumes BIR; no throughput claim.
- 49: Direct string source hooks admitted; no-allocation decoded visitor rejected.
- 50: Retained projection aux side tables rejected.
- 51: Byte-class whitespace cursor rejected.
- 52: Baseline reassay after event-cursor rejection.
- 53: Parser-local structural-mask cursor rejected.
- 54: Exact decoded-string stats sink rejected.
- 55: Quote-source fused string materializer rejected.
- 56: Structural scan floor redress admitted; Canada scan green.
- 57: Direct receiver inlining and generated direct tiny-string route admitted as direct-only.
- 58: SK-V6 dispatch framing recorded; no row closed.
- 59: SK-V5 UTF-8 fusion close class refuted.
- 60: Retained trusted-string boundary collapse rejected.
- 61: Retained long-string trusted 64-byte scan rejected as tested.
- 62: Delayed-wide retained trusted string scan rejected.
- 63: Array `ContainerNext` / next-byte carry admitted.
- 64: Retained Unicode-escape run validator rejected.
- 65: Object next-key carry rejected.
- 66: Direct source-hook field-layout materializer rejected.
- 67: Parser-owned decoded scratch rejected.
- 68: Byte-output `unescape_json_string` materialization rejected.
- 69: DirectBuild semantic string field facts for digest rejected.
- 70: Hand-authored `real_typed_struct` implementation rejected as DirectBuild proof.
- 71: Host/API schema-source generated typed DirectBuild admitted for representative rows.
- 72: Generated-retained-only cap-16 tiny-string probe admitted; global/direct/Track 2 cap-16 rejected.
- 73: Track 2 array next-byte dispatch parity repair rejected.

## Bottom line

The work done hitherto is substantial but not closed. SK-V5/SK-V6 converted a
large amount of substrate, reporting, codegen, SIMD, and direct-workload
authority from prose into measured implementation. The current result is honest:
the scanner floor is green, generated direct attribution is real, representative
typed DirectBuild has a valid schema-source path, and several local throughput
wins are admitted. The close remains blocked by 13 retained G rows and 13
direct digest misses. The no-go ledger is now tight enough that future work must
either consume the structural/event stream as the actual retained parser
substrate, explain the generated Track 1 vs hand Track 2 cap-16 split, or move
direct closure through explicit host/API DirectBuild field facts while keeping
the semantic digest stressor visible as a guard.
