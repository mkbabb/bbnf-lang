# SK-V11 P3-A: Candidate Shortlist

Pass: S-P3 Synthesis-Plan. Cycle: V3.
Date: 2026-05-20.
Scope: distil the converged S-P2 candidate pool into row-gated SK-V11 candidate interventions.
Output: this file.
Pass Alpha goalset: close the 13 residual `direct_to_struct N-direct / NO-GO`
rows or record measured uncloseable proofs; preserve the 7 typed and 4 direct
admissions; land at least one benchmarked non-JSON generated direct or typed
parser intervention; keep parse-only and the SK-V9 W3 substrate family closed.
Candidate pool: research/p2/ post-CHALLENGE survivors.

## §1 — Synthesis (concrete; cites P1 row, P2 candidate, REDRESS entry, or goalset line)

S-P2 converged with two consecutive six-of-six ACCEPT cycles and leaves this
packet a narrow pool: C1-C7 are parser primitives; C8 is benchmark/oracle or a
per-product host sink only; C9 is Lock-1/output-plane accounting only; and
`HEX_QUARTET_X4_PROOF`, PRFM/STNP/cache hints, PMULL/CTZ, EOR3/BCAX, and
movemask-only work are support or proof surfaces, not standalone row movers
(`restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:7-29`).

The SK-V11-open target surface is direct product closure. The direct gate floor
is `ceil(sonic-rs direct Mbps / 1.10)`, and both generated Track 1 and the
independent Track 2/oracle must meet the floor. The 13 concrete direct floors
from the opening goalset are:

| Row | Direct floor Mbps |
|---|---:|
| `twitter/direct_to_struct` | 13740 |
| `canada/direct_to_struct` | 10637 |
| `github_events/direct_to_struct` | 13403 |
| `update_center/direct_to_struct` | 10059 |
| `mesh/direct_to_struct` | 8675 |
| `random/direct_to_struct` | 7878 |
| `gsoc-2018/direct_to_struct` | 3737 |
| `instruments/direct_to_struct` | 8969 |
| `numbers/direct_to_struct` | 2425 |
| `unicode_mixed/direct_to_struct` | 2588 |
| `unicode_escapes/direct_to_struct` | 3441 |
| `distinct_values/direct_to_struct` | 2658 |
| `y_string_unicode/direct_to_struct` | 3950 |

Existing direct `A / GO` rows must not be silently lost. The maintain floor is
the P3-C/SPEC authority: `max(ceil(sonic direct / 1.10),
floor(SK-V11-open track Mbps * 0.98))` per track.

| Direct guard row | Track 1 maintain | Track 2 maintain |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 18191 | 17431 |
| `apache_builds/direct_to_struct` | 11028 | 9996 |
| `marine_ik/direct_to_struct` | 8759 | 9248 |
| `unicode_basic/direct_to_struct` | 2253 | 2182 |

Existing typed `A / GO` rows are guarded on generated Track 1 by
`max(ceil(sonic typed / 1.10), floor(SK-V11-open Track 1 * 0.98))`; Track 2 is
an independent oracle guard at `floor(SK-V11-open Track 2 * 0.98)` when
measured. Typed output parity remains required.

| Typed guard row | Track 1 maintain | Track 2 oracle guard |
|---|---:|---:|
| `twitter/real_typed_struct` | 17385 | 15593 |
| `citm_catalog/real_typed_struct` | 29928 | 17321 |
| `apache_builds/real_typed_struct` | 8308 | 6754 |
| `github_events/real_typed_struct` | 11633 | 12029 |
| `update_center/real_typed_struct` | 11613 | 10150 |
| `mesh/real_typed_struct` | 9214 | 7739 |
| `marine_ik/real_typed_struct` | 11552 | 9894 |

Every candidate below is therefore an intervention packet, not a primitive
wish. It names owner paths, scalar-reference state, checkasm/parity state,
micro-prove-first state, same-wave consumer, output planes, and concrete row
floors. A candidate admits rows per row: if a selected row misses its floor, it
is withheld or the wave rejects according to the later P3-C gate; no row moves
by analogy.

Dropped as standalone candidates:

- C7 `movemask` / bitmap support. It may be consumed by C1/C2/C5/C6 but has no
  standalone row gate.
- C8 `output_digest_hash_oracle`. It is a parity/oracle sink, not parser
  semantics or a generic SIMD candidate.
- C9 Lock-1/output-plane accounting. It is mandatory bookkeeping, not a hot
  leaf intervention.
- `HEX_QUARTET_X4_PROOF`. It can support candidate P3A-C3 only after a new
  source delta; REDRESS 107/108 block proof-to-production reuse.
- PRFM/STNP/cache hints, PMULL/CTZ, EOR3/BCAX, x86 work, parse-only movement,
  W3 union/event/class-column/streaming-cursor repairs, and JSON policy in
  generic crates.

## §2 — Deliverable (the shortlist / sequence / gate set / schema / ledger / SPEC section)

### P3A-C1: `direct_slot_dispatch_and_container_tail_next`

- **S-P2 survivor source:** P2-F C6, P2-D D1/D2, P2-A C4
  `separator_pair_probe16_direct`, and P2-B `CONTAINER_DISPATCH_CLASSIFIER`.
- **P1 antecedents:** `container_dispatch`, `ascii_whitespace_skip`,
  `bounded_plain_string_scan`, `number_digit_span`, `output_digest_hash`.
- **Owner paths:** `skinny/crates/runtime/src/grammars/json/generated.rs`;
  `skinny/crates/codegen/src/lower/sink_only.rs`;
  `skinny/crates/bbnf-bench/src/direct_struct.rs`;
  `skinny/crates/bbnf-bench/src/track2/json.rs`;
  `skinny/crates/bbnf-bench/src/bin/gate.rs`;
  `skinny/crates/bbnf-bench/src/gate.rs`.
- **Shape:** factor root/object/array direct value dispatch and container-tail
  handling into one generated scalar slot dispatcher over the existing cursor.
  It may use local separator/close pair probes, but it may not retain next-byte
  state, object-key carry, value-byte compaction, a class column, or sidecar.
- **Scalar-reference status:** present as current generated direct root/object/
  array functions, retained `consume_container_next`, and independent Track 2
  hand parser. The redress wave must first factor an explicit scalar helper and
  differential it against current output.
- **Checkasm/parity status:** scalar/product parity only unless a C1/C7 mask is
  consumed in the same loop. Any SIMD classifier then requires strict
  `BBNF_SIMD_STRICT=1` parity before row evidence counts.
- **Micro-prove-first status:** open. Required microbench: old generated
  direct shape versus factored slot shape on `github_events`, `update_center`,
  `random`, `canada`, `mesh`, and `instruments`; no source change dispatches
  without showing at least one selected row can plausibly cross its floor and
  no direct guard loses more than the maintain table.
- **Same-wave consumer:** generated `SinkOnly` direct Track 1 and independent
  Track 2/oracle direct digest for the same selected rows.
- **Output plane:** JSON direct digest. Typed rows are guards unless a separate
  typed consumer is named. Non-JSON output is eligible only through P3A-C6's
  generated FIRST/prefix proof.
- **Falsifiability gate:** selected direct rows must meet both-track floors:
  `github_events >= 13403`, `update_center >= 10059`, `random >= 7878`,
  `canada >= 10637`, `mesh >= 8675`, `instruments >= 8969`. Direct and typed
  guard floors from §1 hold.
- **Reject boundary:** reject on output mismatch, Track 2 dependency on Track 1,
  new directive/BIR/backend variant, hidden sidecar, retained structural
  projection, parse-only evidence, object/key/value-byte carry, or no selected
  direct row crossing its floor.

### P3A-C2: `bounded_plain_string_span_direct`

- **S-P2 survivor source:** P2-F C2, P2-E `pt_bounded_plain_string_end`,
  P2-D D3 `borrowed_string_span`, P2-C widened string-special block as
  support-only until re-proven, and P2-B `STRING_SPECIAL_BLOCK_CALLER_MICROPROOF`.
- **P1 antecedents:** `bounded_plain_string_scan`, `string_escape_decode`,
  `simd_movemask`, `output_digest_hash`.
- **Owner paths:** `skinny/crates/parse-that-regex/src/lib.rs`;
  `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`;
  `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`;
  `skinny/crates/runtime/src/grammars/json/generated.rs`;
  `skinny/crates/bbnf-bench/src/direct_struct.rs`;
  `skinny/crates/bbnf-bench/src/generated_real_typed.rs`;
  `skinny/crates/bbnf-bench/src/track2/json.rs`;
  `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- **Shape:** grammar-parameterized string-span helper returning
  `{content_start, content_end, raw_end, needs_decode}` or first interesting
  byte. It does not allocate decoded scratch, retain string facts, or create a
  `StringBlock16` retained wrapper.
- **Scalar-reference status:** present in generated JSON tiny scans,
  direct_struct tiny/plain scans, typed generated string scans, and
  parse-that trusted string helpers. A wave must expose the scalar oracle as the
  comparison point for any factored helper.
- **Checkasm/parity status:** current 16-byte string block has scalar/checkasm
  coverage, but REDRESS 106 falsified the broad caller proof. Any 64-byte or
  new AArch64 body needs a new scalar oracle and strict parity over quote
  offsets, caps, alignments, escapes, controls, non-ASCII, and tails.
- **Micro-prove-first status:** open. Required caller microbench on the exact
  selected string/key loops before production. Primitive parity alone is
  insufficient.
- **Same-wave consumer:** generated direct string/key paths and, if selected,
  generated typed string fields. Independent Track 2/oracle must remain
  independent on the same output plane.
- **Output plane:** JSON direct digest and optional typed string product. Non-
  JSON proof target: CSS L4 quoted strings, Sheets doubled-quote strings, or
  BBNF literal/regex spans through a generated direct/typed parser.
- **Falsifiability gate:** selected JSON direct rows must meet both-track
  floors: `twitter >= 13740`, `github_events >= 13403`,
  `update_center >= 10059`, `random >= 7878`, `gsoc-2018 >= 3737`,
  `distinct_values >= 2658`. If any unicode string row is refreshed, it remains
  a floor-bearing residual unless already admitted; it must meet its own direct
  floor:
  `unicode_escapes >= 3441`, `unicode_mixed >= 2588`,
  `y_string_unicode >= 3950`.
- **Reject boundary:** reject on REDRESS 61/62/83/106 route reuse, retained
  wide-string facts, decoded side tables, primitive-only proof, no selected row
  floor, or any guard-row maintain miss.

### P3A-C3: `escaped_string_segments_hex_run`

- **S-P2 survivor source:** P2-F C3, P2-E `pt_escaped_string_segments`, P2-D D3,
  P2-C x4 escape hex decode as proof-gated support, and P2-B
  `HEX_QUARTET_X4_PROOF`.
- **P1 antecedents:** `string_escape_decode`, `unicode_escape_hex_decode`,
  `bounded_plain_string_scan`, `output_digest_hash`.
- **Owner paths:** `skinny/crates/parse-that-regex/src/lib.rs`;
  `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`;
  `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs`;
  `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`;
  `skinny/crates/runtime/src/grammars/json/generated.rs`;
  `skinny/crates/bbnf-bench/src/direct_struct.rs`;
  `skinny/crates/bbnf-bench/src/generated_real_typed.rs`;
  `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- **Shape:** segment visitor over raw spans, simple escapes, and decoded scalar
  values, with hex-nibble/hex-run decode as a neutral core. JSON surrogate
  policy, CSS variable-width escapes, and BBNF literal policy stay in generated
  or host caller code.
- **Scalar-reference status:** x1 scalar references exist in
  `unescape_uxxxx_scalar`, `decode_unicode_escape`, `read_hex_unit_scalar`, and
  `unescape_string`. Production requires a new scalar segment-stream oracle;
  x4 requires a scalar x4 oracle that preserves valid/invalid/mixed-lane
  semantics.
- **Checkasm/parity status:** current x4 evidence is proof/smoke only. A
  production route requires strict x4 checkasm over valid, invalid,
  mixed-validity, alignment 0..63, surrogate, unpaired-surrogate, and
  boundary/tail cases.
- **Micro-prove-first status:** open. REDRESS 107 is proof-only and REDRESS 108
  rejects production reuse of the already-wired `unescape_string` caller. This
  candidate must name a real source delta and a new product consumer.
- **Same-wave consumer:** direct/typed escaped-segment product consumer or a
  non-JSON host/parser path, not a wrapper around the existing JSON
  `unescape_string` call site.
- **Output plane:** JSON direct digest and optional typed decoded string field.
  Non-JSON proof target: CSS L4 escaped strings or `hexColor`, then BBNF
  literals/regexes. Sheets doubled quotes are not a C3 proof surface.
- **Falsifiability gate:** selected unicode direct rows must meet both-track
  floors: `unicode_escapes >= 3441`, `unicode_mixed >= 2588`,
  `y_string_unicode >= 3950`; `gsoc-2018 >= 3737` may be selected as a
  string-heavy companion. Direct and typed guard floors from §1 hold.
- **Reject boundary:** reject if the only production consumer is existing
  `unescape_string`, if JSON surrogate policy enters generic crates, if decoded
  scratch/stats/hash side channels appear, if strict checkasm fails, or if no
  selected row reaches its floor.

### P3A-C4: `digit_run_span_accumulate_and_number_slot`

- **S-P2 survivor source:** P2-F C4, P2-E `pt_digit_run_span_accumulate`,
  P2-D D4 `number_span_emit_slot`, P2-C UDOT digit-span, and P2-B
  `DIGIT_SPAN_UDOT`.
- **P1 antecedents:** `number_digit_span`, `container_dispatch`,
  `output_digest_hash`.
- **Owner paths:** `skinny/crates/parse-that-regex/src/number/mod.rs`;
  `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs`;
  `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`;
  `skinny/crates/runtime/src/grammars/json/generated.rs`;
  `skinny/crates/bbnf-bench/src/direct_struct.rs`;
  `skinny/crates/bbnf-bench/src/generated_real_typed.rs`;
  `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- **Shape:** scan and optionally accumulate ASCII digit runs into
  `DigitRun`/`NumberSpan`, then emit through slot-specific direct or typed
  numeric sinks. Sign, fraction, exponent, suffix/unit, range, and conversion
  policy remain caller-owned.
- **Scalar-reference status:** present in parse-that digit scan and current
  generated direct/typed numeric wrappers. The wave must define the scalar
  `DigitRun`/`NumberSpan` oracle before any DotProd body.
- **Checkasm/parity status:** scalar/product parity required for all paths.
  AArch64 DotProd/UDOT is optional and needs strict parity for lengths 0..long
  runs, offsets, alignments, digit/non-digit boundaries, overflow/truncation,
  and random bytes.
- **Micro-prove-first status:** open. Required caller microbench must prove the
  factored span or DotProd chunk changes a real numeric direct/typed consumer,
  not only a 4-digit helper.
- **Same-wave consumer:** generated direct numeric sinks, independent Track 2
  numeric digest, and optional generated typed numeric fields.
- **Output plane:** JSON direct digest and optional typed numeric product.
  Non-JSON proof target: CSS L4 dimensions/percentages first, Sheets numeric
  formulas second.
- **Falsifiability gate:** selected numeric rows must meet both-track direct
  floors: `canada >= 10637`, `mesh >= 8675`, `numbers >= 2425`,
  `instruments >= 8969`. If typed numeric guards are refreshed, typed Track 1
  maintain floors include `mesh >= 9214` and `marine_ik >= 11552`; Track 2
  oracle guards follow §1 when measured.
- **Reject boundary:** reject if number conversion semantics change, f64
  fallback or mantissa widening reopens REDRESS 80, Track 2/oracle output
  diverges, DotProd lacks scalar fallback, or no selected row reaches its
  floor.

### P3A-C5: `byte_set_layout_skip_with_transient_masks`

- **S-P2 survivor source:** P2-F C1/C5/C7, P2-E `pt_byte_set_run_skip`, P2-C
  whitespace byte-set skip and TBL byte-set/classifier, and P2-B
  `WHITESPACE_BYTE_SET_SKIP` plus `BYTE_CLASS_TBL_CLASSIFIER`.
- **P1 antecedents:** `ascii_whitespace_skip`, `container_dispatch`,
  `bounded_plain_string_scan`, `simd_movemask`.
- **Owner paths:** `skinny/crates/parse-that-regex/src/lib.rs`;
  `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs`;
  `skinny/crates/bbnf-simd/src/scalar/byte_class_from_table_64.rs`;
  `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs`;
  `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs`;
  `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`;
  `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_table_64.rs`;
  `skinny/crates/runtime/src/grammars/json/generated.rs`;
  `skinny/crates/bbnf-bench/src/direct_struct.rs`;
  `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- **Shape:** grammar-supplied byte-set run skip using scalar/SWAR first, with
  optional AArch64 TBL/TBX/compare masks consumed in the same loop. It returns
  only the first non-member offset; comments and multi-token trivia remain
  generated grammar policy.
- **Scalar-reference status:** current JSON whitespace/space helpers exist;
  generic promotion requires a new `skip_byte_set_ref(input, offset, set)`
  oracle. Byte-class scalar refs already exist for table and eq-set masks.
- **Checkasm/parity status:** existing byte-class checkasm is usable as a base,
  but the specific candidate needs strict parity over JSON, CSS, Sheets, and
  BBNF byte sets, alignments, tails, empty/short/long runs, all-member and
  no-member blocks, and first-non-member extraction.
- **Micro-prove-first status:** open. Required caller microbench must attach to
  a generated direct whitespace/layout site or a non-JSON layout site. Standalone
  classifier speed is support evidence only.
- **Same-wave consumer:** generated direct whitespace/value entry points and a
  generated non-JSON layout consumer if generic crates are touched.
- **Output plane:** JSON direct digest; non-JSON generated direct/typed parser
  for Sheets `?w`, CSS/BBNF layout with comments above the byte-set primitive.
  Typed JSON rows are guards unless a typed consumer is named.
- **Falsifiability gate:** selected direct rows must meet both-track floors:
  `twitter >= 13740`, `random >= 7878`, `distinct_values >= 2658`,
  `instruments >= 8969`, and optionally `update_center >= 10059` if the value
  entry path is touched. Direct and typed guard floors from §1 hold.
- **Reject boundary:** reject on whitespace bitmap/cursor/class column,
  retained structural-position vector, second scanner, JSON-only policy in
  generic crates, comment-aware logic inside `bbnf-simd` or parse-that generic
  byte-set code, or no selected row floor.

### P3A-C6: `generated_first_prefix_non_json_dispatch`

- **S-P2 survivor source:** P2-F C6 with C1/C2/C4/C5 support, P2-D D1/D2
  consumer-shape constraints, and P2-A comparator pressure around generated
  direct dispatch.
- **P1 antecedents:** `container_dispatch`, `ascii_whitespace_skip`,
  `bounded_plain_string_scan`, `number_digit_span`, `simd_movemask`.
- **Owner paths:** `skinny/crates/codegen/src/lib.rs`;
  `skinny/crates/codegen/src/lower/sink_only.rs`;
  `skinny/crates/runtime/src/grammars/json/generated.rs`;
  `skinny/crates/bbnf-bench/src/direct_struct.rs`;
  `skinny/crates/bbnf-bench/src/bin/gate.rs`;
  `grammar/css/l4/values.bbnf`;
  `grammar/css/l4/color.bbnf`;
  `grammar/google-sheets/google-sheets.bbnf`;
  `grammar/bbnf/bbnf.bbnf`.
- **Shape:** generated FIRST-set, prefix-trie, or lookahead dispatch template
  over grammar metadata, not JSON object/array role code. This is the carrier
  for the mandatory non-JSON proof: CSS L4 declaration values first, Sheets
  formulas second, BBNF-self third.
- **Scalar-reference status:** open per grammar. The wave must generate a scalar
  dispatch oracle and differential it against the existing grammar parser or
  independently declared output oracle.
- **Checkasm/parity status:** N/A for scalar-only generated dispatch. If it
  consumes C1/C7 masks, strict mask/checkasm parity is mandatory before product
  rows count.
- **Micro-prove-first status:** open and blocking. Current codegen still routes
  through `json_provider`; P2-F makes that a Lock 14 gate before any
  CSS/Sheets/BBNF-self generated-parser admission.
- **Same-wave consumer:** a generated non-JSON direct or typed parser benchmark
  row with independent oracle, and optionally a JSON direct dispatch row if the
  same template also touches JSON.
- **Output plane:** non-JSON generated direct/typed parser is mandatory. JSON
  direct rows are optional companions. Typed JSON rows are guards unless a typed
  consumer is selected.
- **Falsifiability gate:** JSON companion rows, if selected, must meet the
  direct floors `github_events >= 13403`, `update_center >= 10059`,
  `random >= 7878`, or `instruments >= 8969`. The non-JSON row is named
  `css_l4/declaration_values/{direct,typed}` in V3, with fallback
  `google_sheets/formula/{direct,typed}`. W1a creates the gate/report lane and
  W1b creates the baseline row; W2 admits only if the intervention reaches
  `ceil(W1b_css_baseline_mbps * 1.01)` with strict oracle equality.
- **Reject boundary:** reject on JSON-provider policy in generic codegen, new
  directive/BIR/backend variant, generic-crate grammar names, hidden sidecar,
  Track 1/Track 2 shared implementation, non-JSON proof by prose, or absence of
  a W1b concrete non-JSON Mbps baseline before redress.

### P3A-C7: `typed_direct_guard_extension`

- **S-P2 survivor source:** P2-D D3/D4 consumer-shape constraints and P2-F
  C2/C4 generated typed surfaces. This is retained because typed guard rows are
  the existing SOTA-bearing plane, but it is lower priority than direct closure.
- **P1 antecedents:** `bounded_plain_string_scan`, `string_escape_decode`,
  `number_digit_span`, `output_digest_hash`.
- **Owner paths:** `skinny/crates/bbnf-bench/src/generated_real_typed.rs`;
  `skinny/crates/bbnf-bench/src/real_typed_struct.rs`;
  `skinny/crates/parse-that-regex/src/lib.rs`;
  `skinny/crates/parse-that-regex/src/number/mod.rs`;
  `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- **Shape:** extend a direct/typed string or numeric consumer only when it also
  preserves typed SOTA rows or supplies a typed non-JSON consumer. It cannot use
  direct digest evidence to admit typed rows.
- **Scalar-reference status:** present in generated typed string/number helpers
  and serde/oracle sidecars. Any new typed root or field path needs an explicit
  scalar or serde oracle.
- **Checkasm/parity status:** product parity by default; strict checkasm only if
  an AArch64 string/digit primitive is routed.
- **Micro-prove-first status:** open. Required caller microbench must show typed
  product movement or guard preservation before any typed behavior wave.
- **Same-wave consumer:** generated typed parser plus independent serde/oracle
  for the same row.
- **Output plane:** JSON typed product and optional non-JSON typed parser. It is
  not a direct digest candidate.
- **Falsifiability gate:** typed guards must maintain Track 1 floors:
  `twitter >= 17385`, `citm_catalog >= 29928`,
  `apache_builds >= 8308`, `github_events >= 11633`,
  `update_center >= 11613`, `mesh >= 9214`, `marine_ik >= 11552`.
  Track 2 oracle guards from §1 hold when measured. If this candidate admits a
  new typed row, P3-C must assign that row's strict typed floor from same-run
  sonic typed evidence before redress.
- **Reject boundary:** reject on direct-digest-as-typed-proof, missing
  independent typed oracle, no typed row/guard movement, hidden schema source,
  or JSON-only generic policy.

## §3 — Falsifiability binding (named corpus rows + Mbps thresholds)

All JSON direct admissions use the same concrete target floors:

| Candidate | Candidate row set | Mbps floor rule |
|---|---|---|
| P3A-C1 `direct_slot_dispatch_and_container_tail_next` | `github_events`, `update_center`, `random`, `canada`, `mesh`, `instruments` | Both Track 1 and Track 2/oracle meet 13403 / 10059 / 7878 / 10637 / 8675 / 8969 respectively. |
| P3A-C2 `bounded_plain_string_span_direct` | `twitter`, `github_events`, `update_center`, `random`, `gsoc-2018`, `distinct_values`; unicode residual rows as floor-bearing selected rows | Both tracks meet 13740 / 13403 / 10059 / 7878 / 3737 / 2658 respectively; if selected, `unicode_escapes >= 3441`, `unicode_mixed >= 2588`, `y_string_unicode >= 3950`. |
| P3A-C3 `escaped_string_segments_hex_run` | `unicode_escapes`, `unicode_mixed`, `y_string_unicode`, optional `gsoc-2018` | Both tracks meet 3441 / 2588 / 3950 / 3737 respectively. |
| P3A-C4 `digit_run_span_accumulate_and_number_slot` | `canada`, `mesh`, `numbers`, `instruments` | Both tracks meet 10637 / 8675 / 2425 / 8969 respectively. |
| P3A-C5 `byte_set_layout_skip_with_transient_masks` | `twitter`, `random`, `distinct_values`, `instruments`, optional `update_center` | Both tracks meet 13740 / 7878 / 2658 / 8969 / 10059 respectively. |
| P3A-C6 `generated_first_prefix_non_json_dispatch` | JSON companions `github_events`, `update_center`, `random`, `instruments`; non-JSON `css_l4/declaration_values/{direct,typed}` or fallback `google_sheets/formula/{direct,typed}` | JSON companions meet 13403 / 10059 / 7878 / 8969 respectively. W1b creates the concrete non-JSON baseline Mbps and independent oracle; W2 admits only at `ceil(W1b_css_baseline_mbps * 1.01)` with strict equality. P3-D binds fields but does not own the performance floor. |
| P3A-C7 `typed_direct_guard_extension` | Existing typed guards, plus any P3-C selected new typed row | Existing typed guards maintain 17385 / 29928 / 8308 / 11633 / 11613 / 9214 / 11552 on Track 1, with §1 Track 2 oracle guards when measured. New typed row floor must be computed from same-run sonic typed strict before redress. |

Full-table maintain is inherited by every candidate. At minimum, a direct-output
refresh preserves the direct guard Track 1/Track 2 maintain floors in §1, and a
typed-output refresh preserves the typed guard Track 1 floors in §1. If a wave
touches generic parser crates, `bbnf-simd`, `parse-that-regex`, generated
runtime templates, or report/gate schema, P3-C should promote these floors into
the wave's exit gate rather than leaving them as prose.

Micro-prove-first is binding for all candidates:

- Candidate-specific scalar oracle exists or lands in the same wave before
  native/SIMD evidence is trusted.
- Strict checkasm/parity passes for every AArch64 body used by a product row.
- Caller microbench on representative slices precedes production wiring.
- Same-wave consumer appears in generated direct, generated typed, or generated
  non-JSON product path.
- `gate-json` or the named non-JSON gate consumes every emitted field; no
  producer-only telemetry.

## §4 — Pre-blocked routes (REDRESS entries each wave must NOT re-open)

- REDRESS 50, 51, and 53 block parse-time aux columns, event cursors,
  whitespace bitmaps, structural cursors, parser-local second scanners, and
  structural-position sidecars.
- REDRESS 54, 55, 60, 61, 62, 64, 67, 68, 69, 72, 82, 83, 106, 107, and 108
  block decoded-string stats sinks, source-hook fused materializers, retained
  wide string scans, delayed-wide trusted scans, retained Unicode validators,
  parser-owned decoded scratch, byte-output materialization, semantic string
  facts, primitive-parity-only string production, proof-only x4 promotion, and
  production reuse of an already-wired caller.
- REDRESS 63 permits only narrow generated JSON array carry and does not
  authorize object next-key carry, value-byte compaction, generic JSON pair
  policy, or persistent local carry.
- REDRESS 65 and 84 keep object next-key/value-byte carry blocked.
- REDRESS 80 blocks generic numeric fallback, mantissa-widen, and f64 fallback
  rewrites without same-wave product movement.
- REDRESS 88 and 89 block PMULL prefix-XOR and CSSC CTZ/bulk rewires as default
  hot production paths.
- REDRESS 96, 97, 98, and 102 close the W3 union/event/class-column/
  streaming-cursor/class-lane/sidecar substrate family and keep parse-only
  movement proof-only.
- REDRESS 103-105 preserve the typed-plane distinction: typed rows require
  generated typed Track 1 plus independent typed oracle evidence; direct digest
  evidence cannot admit typed rows.
- SK-V11 handoff refusal conditions additionally block x86 implementation work,
  parse-only SOTA admission, JSON policy in generic crates, new directive/BIR/
  backend variants, missing micro-proofs, missing non-JSON benchmark proof, and
  any telemetry field not consumed by a same-wave gate.

## §5 — Sources (every upstream artefact cited)

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/prompts/ORCHESTRATOR.md:74-132`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v11/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v10/SPEC.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/locks/LOCKS.md`
