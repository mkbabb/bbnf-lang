# SK-V12 P3-A: Candidate Shortlist

Pass: S-P3 Synthesis-Plan. Cycle: V4.
Date: 2026-05-20.
Scope: distil the converged S-P2 survivor pool into the SK-V12 implementation-candidate shortlist.
Output: this file.
Pass Alpha goalset: admit exactly one generated non-JSON direct or typed baseline first, then admit one measured grammar-generalized intervention on that same row at >= `ceil(baseline_mbps * 1.01)`, while preserving the 4 direct and 7 typed JSON guard rows.
Candidate pool: research/p2/ post-CHALLENGE survivors.

## §1 - Synthesis

SK-V12 is not a JSON-direct retry. The accepted goalset says generated
non-JSON baseline work comes first, the measured grammar-generalized
intervention comes second, JSON product guards remain binding, and JSON direct
residual rows are pre-blocked by REDRESS 119 unless a later packet records
fresh material evidence beyond REDRESS 114-119
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:38`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:50`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:58`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:65`). S-P1 converged with the
same boundary: JSON-only profile telemetry may nominate primitive families, but
does not prove CSS L4, Sheets, or BBNF-self behavior
(`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:60`).

The S-P2 survivor pool is therefore narrower than the raw primitive inventory.
P2-A's seven comparator-shaped candidates survive only as grammar-generated,
same-wave-consumed mechanics (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:31`,
`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:139`);
P2-B supplies the scalar-oracle-first/checkasm process and marks digest/hash as
oracle/report support, not a parser primitive
(`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:49`);
P2-C leaves six current AArch64 candidates but demotes LD4 and SHA3 ternary
folding to ISA inventory (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:44`,
`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:153`);
P2-D contributes no selectable substrate candidate and keeps
`structural_class_lane_union` rejected (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:69`,
`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:79`);
P2-E contributes five parse-that gaps with scalar sketches and same-wave
consumer requirements (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:50`,
`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:95`,
`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:153`,
`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:222`,
`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:284`);
and P2-F maps the pool to six conditional parser/support families, with F7
oracle-only and F8 accounting-only/ineligible as parser candidates
(`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:23`,
`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:52`,
`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:53`).

The shortlist below keeps the SK-V12 priority order. C1-C3 are mutually
exclusive generated-baseline candidates in the Pass Alpha preferred order. C4-C8
are conditional measured-intervention candidates; each is dispatchable only
after one of C1-C3 admits a generated non-JSON row and records the baseline
Mbps used by the intervention gate.

## §2 - Deliverable

### C1 - Generated CSS L4 Declaration-Values Baseline

- Owner paths: `skinny/crates/codegen/src/lib.rs`,
  `skinny/crates/codegen/src/json_provider.rs`, a generated
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/` runtime
  module, `skinny/crates/runtime/src/lib.rs`, `skinny/crates/bbnf-bench/src/report.rs`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs`, and a CSS declaration-value
  benchmark/oracle fixture lane.
- Scalar-reference status: baseline scalar parser must be generated from the
  CSS L4 grammar; no existing generated CSS runtime is admitted. Current codegen
  still gates runtime emission through JSON provider policy
  (`skinny/crates/codegen/src/lib.rs:108`,
  `skinny/crates/codegen/src/json_provider.rs:4`).
- Checkasm/parity status: checkasm N/A for the baseline itself; required parity
  is compile/equality smoke against an independent same-plane CSS oracle plus
  gate consumption. Any SIMD helper used inside the baseline inherits its own
  scalar/checkasm requirement.
- Same-wave consumer:
  `css_l4/declaration_values/{direct_to_struct|real_typed_struct}/main`
  generated Track 1, consumed by the non-JSON report/gate in the same wave.
- Falsifiability gate: selected CSS row generated Track 1 >= 1 Mbps, independent
  oracle/Track 2 >= 1 Mbps, strict output equality PASS, sample count >= 30,
  run/build/sample telemetry present, and the gate consumes the row. Maintain the direct guards
  `citm_catalog` >= 18191/17431, `apache_builds` >= 11028/9996,
  `marine_ik` >= 8759/9248, `unicode_basic` >= 2253/2182, and typed guards
  `twitter` >= 17385/15593, `citm_catalog` >= 29928/17321,
  `apache_builds` >= 8308/6754, `github_events` >= 11633/12029,
  `update_center` >= 11613/10150, `mesh` >= 9214/7739,
  `marine_ik` >= 11552/9894 (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:127`,
  `restart/skinny/tranches/sk-v12/SYNTHESIS.md:139`).
- Grammar-neutral verdict: direct proof of grammar generalization if emitted
  from the normal codegen template and no CSS policy leaks into generic crates.
- Pre-block notes: clears only the REDRESS 112 blocker if a generated CSS
  runtime actually exists; it must not treat the REDRESS 111 report lane as a
  parser baseline, reopen REDRESS 113's blocked intervention, or use REDRESS
  70/71 typed-output shortcuts: no hand-authored typed sink, direct digest
  proof, hidden directive/BIR extension, hidden host schema, or
  benchmark-private Track 1 parser.

### C2 - Generated Sheets Formula Baseline

- Owner paths: `skinny/crates/codegen/src/lib.rs`,
  `skinny/crates/codegen/src/json_provider.rs`, a generated
  `skinny/crates/runtime/src/grammars/sheets/` runtime module,
  `skinny/crates/runtime/src/lib.rs`, `skinny/crates/bbnf-bench/src/report.rs`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs`, and a Sheets formula
  benchmark/oracle fixture lane.
- Scalar-reference status: baseline scalar parser must be generated from
  `grammar/google-sheets/google-sheets.bbnf`; the existing `sheets_witness`
  module is a witness/test surface, not an admitted generated runtime
  (`skinny/crates/runtime/src/lib.rs:11`).
- Checkasm/parity status: checkasm N/A for the baseline itself; required parity
  is compile/equality smoke against an independent same-plane Sheets oracle plus
  gate consumption.
- Same-wave consumer:
  `sheets/formula/{direct_to_struct|real_typed_struct}/main` generated Track 1,
  consumed by the non-JSON report/gate in the same wave.
- Falsifiability gate: selected Sheets row generated Track 1 >= 1 Mbps,
  independent oracle/Track 2 >= 1 Mbps, strict output equality PASS, sample
  count >= 30, required telemetry present, and the same JSON direct/typed guard
  floors from C1 maintain.
- Grammar-neutral verdict: direct proof if Sheets grammar metadata owns doubled
  quote strings, numbers, operators, functions, and array delimiters; generic
  crates may expose templates only (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:19`).
- Pre-block notes: fallback only if C1's CSS owner/preflight blocks. It cannot
  claim that the hand-only witness module satisfies the generated baseline gate,
  and it must not use REDRESS 70/71 typed-output shortcuts: no hand-authored
  typed sink, direct digest proof, hidden directive/BIR extension, hidden host
  schema, or benchmark-private Track 1 parser.

### C3 - Generated BBNF-Self Grammar Baseline

- Owner paths: `skinny/crates/codegen/src/lib.rs`,
  `skinny/crates/codegen/src/json_provider.rs`, a generated
  `skinny/crates/runtime/src/grammars/bbnf_self/` runtime module,
  `skinny/crates/runtime/src/lib.rs`, `skinny/crates/bbnf-bench/src/report.rs`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs`, and a BBNF grammar
  benchmark/oracle fixture lane.
- Scalar-reference status: baseline scalar parser must be generated from the
  BBNF grammar sources; no current generated BBNF-self runtime is admitted.
- Checkasm/parity status: checkasm N/A for the baseline itself; required parity
  is compile/equality smoke against an independent same-plane BBNF oracle plus
  gate consumption.
- Same-wave consumer:
  `bbnf_self/grammar/{direct_to_struct|real_typed_struct}/main` generated Track
  1, consumed by the non-JSON report/gate in the same wave.
- Falsifiability gate: selected BBNF-self row generated Track 1 >= 1 Mbps,
  independent oracle/Track 2 >= 1 Mbps, strict output equality PASS, sample
  count >= 30, required telemetry present, and the same JSON direct/typed guard
  floors from C1 maintain.
- Grammar-neutral verdict: direct proof if identifiers, literals, regex spans,
  comments, directives, and value-expression policy remain generated metadata or
  host declarations (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:19`).
- Pre-block notes: fallback after CSS and Sheets. It must not add a directive,
  BIR variant, public substrate API, hand-written per-grammar generic policy, or
  REDRESS 70/71 typed-output shortcut: no hand-authored typed sink, direct
  digest proof, hidden directive/BIR extension, hidden host schema, or
  benchmark-private Track 1 parser.

### C4 - Selected-Baseline Generated FIRST/Prefix Dispatch Template

- Owner paths: `skinny/crates/codegen/src/lib.rs`,
  `skinny/crates/codegen/src/lower/`, selected generated runtime module under
  `skinny/crates/runtime/src/grammars/`, `skinny/crates/bbnf-bench/src/report.rs`,
  and `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- Scalar-reference status: generated scalar FIRST/follow/prefix dispatch
  required for the selected grammar. Existing JSON dispatch is demand evidence,
  not the neutral oracle (`skinny/crates/runtime/src/grammars/json/generated.rs:47`).
- Checkasm/parity status: N/A unless it consumes a SIMD classifier; generated
  branch parity against the independent oracle is mandatory.
- Same-wave consumer: the admitted C1/C2/C3 row's generated parser dispatch in
  the same wave.
- Falsifiability gate: same selected non-JSON row as the admitted baseline,
  intervention Track 1 >= `ceil(baseline_mbps * 1.01)`, oracle/Track 2
  >= 1 Mbps, strict equality PASS, and all C1 JSON guard floors maintain.
- Grammar-neutral verdict: passes only with generated tables and no generic
  grammar-name branch (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:50`).
- Pre-block notes: may not repackage JSON container-tail/object carry; REDRESS
  115 remains a block for that route.

### C5 - Selected-Baseline Byte-Set Run Skip / Transient Class Mask

- Owner paths: `skinny/crates/parse-that-regex/src/lib.rs`,
  `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs`,
  `skinny/crates/bbnf-simd/src/scalar/byte_class_from_table_64.rs`,
  `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs`,
  `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_table_64.rs`,
  `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`,
  `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_table_64.rs`,
  selected generated runtime module, report, and gate.
- Scalar-reference status: Layer 0 scalar table/eq-set references exist; the
  parse-that `ByteSet` run-skip scalar reference must be promoted before this is
  a product primitive (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:50`).
- Checkasm/parity status: existing eq-set/table checkasm coverage is reusable,
  but the selected kernel must run strict parity for tails, alignment, high-bit,
  duplicate-set, and corpus windows before production wiring
  (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:23`).
- Same-wave consumer: selected-baseline layout/trivia skip, delimiter dispatch,
  string-interesting scan, or FIRST-set branch.
- Falsifiability gate: selected non-JSON row intervention Track 1 >=
  `ceil(baseline_mbps * 1.01)`, oracle/Track 2 >= 1 Mbps, strict equality
  PASS, and all C1 JSON guard floors maintain.
- Grammar-neutral verdict: passes only if byte sets/tables are generated
  metadata and masks are transient (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:44`).
- Pre-block notes: no retained class stream, sidecar cursor, structural vector,
  or second scan; REDRESS 50/51/53/96/97/98/119/120 remain blocking.

### C6 - Selected-Baseline Bounded String Span / Special-Byte Scan

- Owner paths: `skinny/crates/parse-that-regex/src/lib.rs`,
  `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`,
  `skinny/crates/bbnf-simd/src/aarch64/movemask.rs`,
  `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`, selected generated
  runtime module, report, and gate.
- Scalar-reference status: a generic bounded string scalar reference is missing;
  existing JSON and 16-byte string-block helpers are partial evidence only
  (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:95`).
- Checkasm/parity status: required if NEON/SWAR support is routed; scalar tests
  must cover terminator, escape, control, non-ASCII, cap, tail, and caller start
  modes.
- Same-wave consumer: selected-baseline generated string/literal/key scanner
  replacement.
- Falsifiability gate: selected non-JSON row intervention Track 1 >=
  `ceil(baseline_mbps * 1.01)`, oracle/Track 2 >= 1 Mbps, strict equality
  PASS, and all C1 JSON guard floors maintain.
- Grammar-neutral verdict: conditional PASS only when delimiter, escape,
  control, UTF-8, and cap policy are generated/caller-owned
  (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:46`).
- Pre-block notes: must not retry JSON tiny-string, StringBlock16 proof-only,
  retained decoded-byte, or eager materialization routes; REDRESS 28/33 active
  TBL/NEON `match_tiny_plain_string` dispatch, REDRESS 54/55/60-69, 72, 82, 83,
  106, 116, 117, and 119 remain blocking. REDRESS 72 scalar cap widening does
  not authorize the REDRESS 28/33 active-dispatch kernel.

### C7 - Selected-Baseline Digit-Run Span / Accumulate

- Owner paths: `skinny/crates/parse-that-regex/src/number/mod.rs`,
  `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs`,
  `skinny/crates/bbnf-simd/tests/aarch64_primitives.rs`,
  `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`, selected generated
  runtime module, report, and gate.
- Scalar-reference status: private parse-that digit-run anchors exist, but a
  public grammar-neutral digit-run span/accumulate oracle is missing
  (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:153`).
- Checkasm/parity status: required for UDOT/native helpers; tests must cover
  zero-length runs, tails, non-digit boundaries, truncation/overflow, sign/radix
  boundaries, and unaligned loads.
- Same-wave consumer: selected-baseline generated number/literal parser or typed
  numeric field.
- Falsifiability gate: selected non-JSON row intervention Track 1 >=
  `ceil(baseline_mbps * 1.01)`, oracle/Track 2 >= 1 Mbps, strict equality
  PASS, and all C1 JSON guard floors maintain.
- Grammar-neutral verdict: conditional PASS because it scans ASCII digits only;
  sign, radix, exponent, suffix/unit, and materialization policy remain
  generated (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:48`).
- Pre-block notes: cannot recreate JSON numeric slot/direct closure; REDRESS 80
  and REDRESS 114 remain blocking.

### C8 - Selected-Baseline Escape Segment / Hex-Quad Decode

- Owner paths: `skinny/crates/parse-that-regex/src/lib.rs`,
  `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`,
  `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs`,
  `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`, selected generated
  runtime module, report, and gate.
- Scalar-reference status: local JSON scalar hex/unescape helpers exist, but
  the grammar-neutral `hex_quad_decode` and segment visitor references must be
  promoted before product routing
  (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:222`,
  `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:284`).
- Checkasm/parity status: required for x4/TBL support; must cover invalid lanes,
  first-failing offsets, mixed validity, short tails, surrogate-policy handoff,
  and end-to-end segment-stream parity.
- Same-wave consumer: selected-baseline generated escape decoder or
  string/literal materialization consumer; no allocation-forcing generic
  materializer.
- Falsifiability gate: selected non-JSON row intervention Track 1 >=
  `ceil(baseline_mbps * 1.01)`, oracle/Track 2 >= 1 Mbps, strict equality
  PASS, and all C1 JSON guard floors maintain.
- Grammar-neutral verdict: conditional PASS if escape tables, hex width,
  surrogate/scalar validity, and output sink are caller-owned
  (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:47`).
- Pre-block notes: cannot reopen single-quartet proof-only, decoded-string
  stats, source-fold, or host-sink routes; REDRESS 54/55/60-69/72/82/83/107/108,
  116/117/118/119 remain blocking.

### Dropped / Ineligible From S-P2

- P2-D `offset_tape_capacity_policy`, `sparse_flag_lookup_policy`, and
  `retained_cursor_skip_projection`: diagnostic/ineligible under current S-P1;
  no current hot leaf names the exact movement locus.
- P2-D `structural_class_lane_union`: rejected under REDRESS 96/97/98 and Lock
  1.
- P2-C `a64_ld4_interleaved_classifier64x4` and
  `a64_sha3_ternary_bool_fold`: ISA inventory only; S-P1 names no interleaved
  stream or hot three-input boolean expression.
- P2-B `OUTPUT_DIGEST_HASH_ORACLE_GATE` and P2-F F7: oracle/report support
  only, parser-candidate-ineligible.
- P2-F F8 tape/direct accounting: required guardrail, not a parser primitive.
- JSON direct residual work: pre-blocked by REDRESS 119/120 until non-JSON
  priority succeeds or explicitly blocks and a later packet supplies fresh
  material evidence beyond REDRESS 114-119.

## §3 - Falsifiability binding

Baseline candidates C1-C3 use concrete finite-throughput gates because no
generated non-JSON row currently exists. The admitted baseline row must report
generated Track 1 >= 1 Mbps, independent oracle/Track 2 >= 1 Mbps, strict
output equality PASS, sample count >= 30, and same-wave gate consumption on
exactly one row:
`css_l4/declaration_values/{direct_to_struct|real_typed_struct}/main` for C1;
`sheets/formula/{direct_to_struct|real_typed_struct}/main` for C2;
`bbnf_self/grammar/{direct_to_struct|real_typed_struct}/main` for C3.

Intervention candidates C4-C8 use the admitted baseline row as their named
target. Their threshold is `ceil(baseline_mbps * 1.01)` on the same row,
with the same independent oracle/Track 2 >= 1 Mbps and strict equality PASS.
The numeric floor becomes concrete in the wave packet after C1, C2, or C3
records the baseline Mbps; a wave may not dispatch an intervention while that
number is undefined.

Every candidate maintains the SK-V12 guard rows:

| Guard row | Track 1 floor | Track 2/oracle floor |
| --- | ---: | ---: |
| `citm_catalog/direct_to_struct` | 18191 | 17431 |
| `apache_builds/direct_to_struct` | 11028 | 9996 |
| `marine_ik/direct_to_struct` | 8759 | 9248 |
| `unicode_basic/direct_to_struct` | 2253 | 2182 |
| `twitter/real_typed_struct` | 17385 | 15593 |
| `citm_catalog/real_typed_struct` | 29928 | 17321 |
| `apache_builds/real_typed_struct` | 8308 | 6754 |
| `github_events/real_typed_struct` | 11633 | 12029 |
| `update_center/real_typed_struct` | 11613 | 10150 |
| `mesh/real_typed_struct` | 9214 | 7739 |
| `marine_ik/real_typed_struct` | 11552 | 9894 |

Any guard miss falsifies the wave unless the same wave records a measured
demotion disposition. `parse_only` rows remain diagnostic and cannot close any
SK-V12 candidate.

## §4 - Pre-blocked routes

The binding pre-block surface is:

- W3 union/event/class-column/streaming-cursor/class-lane/sidecar substrate,
  including `UnionTape`, retained structural vectors, parser-owned projections,
  and W4-through-W3 cascade.
- Parse-only SOTA close or parse-only row admission.
- JSON direct residual row movement without fresh material evidence beyond
  REDRESS 114-119 and without the non-JSON priority succeeding or explicitly
  blocking.
- W0-clamped direct admission by docs-only accounting.
- Direct digest evidence as typed product proof, and output digest/host-sink as
  parser primitive.
- Replays of `number_span_emit_slot`, `container_tail_next`, bounded string
  span, decoded-byte source folds, output-digest host sink, PMULL prefix-XOR,
  CTZ bulk consumer, or tiny-string/TBL proof-only routes without material
  differential and same-row evidence.
- PMU, cycles, structural-scan, masking probes, Criterion slope, sidecar
  freshness, or parser inventory as behavior producers.
- JSON policy in generic crates or runtime outside generated per-grammar code.
- New directive, BIR variant, `BackendShape`, public substrate API,
  parser-owned sidecar/fact slot, second retained substrate, or x86
  implementation work.

## §5 - Sources

- `restart/prompts/ORCHESTRATOR.md`
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
