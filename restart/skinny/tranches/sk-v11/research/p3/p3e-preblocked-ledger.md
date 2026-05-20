# SK-V11 P3-E: Pre-Blocked Route Ledger

Pass: S-P3 Synthesis-Plan. Cycle: V2.
Date: 2026-05-20.
Scope: enumerate SK-V11 per-wave pre-blocked routes from REDRESS, S-P2 V3, and the SK-V11 goalset.
Output: this file.
Pass Alpha goalset: direct plane closure or measured direct fixpoint across the 13 residual `direct_to_struct` rows; preserve the 7 typed and 4 direct admitted rows; keep `parse_only` diagnostic; admit at least one benchmarked non-JSON generated direct/typed intervention; require aarch64 micro-prove-first for SIMD/ASM.
Candidate pool: research/p2/ post-CHALLENGE survivors: C1-C7 parser primitives, C8 oracle/host sink only, C9 Lock-1/output-plane accounting only; `HEX_QUARTET_X4_PROOF`, PRFM/STNP/cache hints, PMULL/CTZ, and EOR3/BCAX remain proof/support/inventory only until a later wave supplies a full source delta, scalar oracle, strict parity/checkasm, same-wave consumer, and row gate.

## §1 — Synthesis (concrete; cites P1 row, P2 candidate, REDRESS entry, or goalset line)

S-P2 V3 gives S-P3 a narrow implementation vocabulary. C1-C7 are the only parser primitive pool: byte-set/class-table masking, bounded special-byte scanning, neutral escape/hex segment decode, digit-span/accumulate, byte-set layout skip, generated FIRST/prefix/lookahead dispatch, and movemask/bitmap support only when consumed by C1/C2/C6 in the same wave. C8 is not parser vocabulary; it is benchmark oracle or host output sink only. C9 is Lock-1/output-plane accounting only.

The SK-V11 goalset makes direct product rows the JSON close surface. The current residual rows are `twitter`, `canada`, `github_events`, `update_center`, `mesh`, `random`, `gsoc-2018`, `instruments`, `numbers`, `unicode_mixed`, `unicode_escapes`, `distinct_values`, and `y_string_unicode` under `direct_to_struct`. `parse_only` is a closed concession and cannot count as SOTA movement. Any wave that attempts row movement must name generated Track 1, independent Track 2/oracle, strict same-run comparator, output plane, REDRESS id, and same-wave gate consumption.

The W3 family is retired, not waiting for a smaller patch. REDRESS 96 and 97 implemented the class-column and streaming-cursor union-substrate hypotheses with correctness/parity green and then missed every W3 target plus every W10b maintain row. REDRESS 98 retires `G-W3-UNION-SUBSTRATE`; REDRESS 102 makes parse-only movement proof-only and forbids W3 as a W4 consumer/substrate dependency. Therefore no SK-V11 wave may reopen union/event substrate, retained class column, class-lane-only repair, parser-owned structural projection, `UnionTape`, streaming cursor, structural-position vector, or W4-through-W3 cascade lock.

The REDRESS ledger splits into four SK-V11 route classes:

1. Hard blocks: routes no SK-V11 wave may reopen under current authority.
2. Material-differential-only routes: routes that can be considered only if the wave names a materially different source delta, same-wave product consumer, and row gate before redress.
3. Proof-only / inventory-only surfaces: useful research or harness surfaces that cannot move rows by themselves.
4. Per-wave pre-blocks: the concrete REDRESS entries each wave family must carry into P3-B/P3-C/P3-F and later CH3 review.

## §2 — Deliverable (the shortlist / sequence / gate set / schema / ledger / SPEC section)

### §2.1 — Wave-Facing Ownership Map

P3-B may renumber waves. If it does, the pre-block list moves with the candidate family; the block does not relax.

| Wave family for P3-B/P3-F | Candidate surface | Routes it can consider | Routes it must pre-block |
|---|---|---|---|
| W0 baseline / telemetry lock | SK-V11-open run id, full `RESULTS.md`, gate-json schema, strict comparator provenance | Evidence only. It can identify hot leaves and row floors. | All behavior source changes, row relabeling, stale sidecars, PMU-only or parse-only admission, producer-only telemetry, and any source edit not required by telemetry/gate validation. |
| W1a non-JSON gate/report schema lane | C9 accounting, grammar id/output-plane/Track 2 telemetry gate | Harness-only. It creates the grammar-neutral gate lane and rejects missing or producer-only non-JSON fields. | Behavior row movement, generated baseline authority, hand-only non-JSON proof, JSON-provider emission as generality proof, hidden directives/BIR variants, or producer-only non-JSON telemetry. |
| W1b generated non-JSON baseline/oracle lane | C9 accounting, generated CSS/Sheets/BBNF-self baseline row plus independent oracle/Track 2 | Baseline-only. It creates exactly one generated non-JSON baseline row with strict equality and gate consumption. | Behavior intervention, row admission, JSON-provider emission as generality proof, coupled oracle, or generic JSON policy. |
| W2 CSS L4 generated intervention proof | C1/C2/C4/C5/C6 with C7 support on generated CSS direct/typed parser | One admitted benchmarked CSS L4 intervention with output oracle/comparator and same-wave gate, consuming the W1b baseline. | REDRESS 36-38, 85-87, 100-102. No JSON policy in generic crates, no renamed JSON helper as generality proof, no CostFacts-as-performance, no prose-only Lock 14 proof, no parse-only SOTA movement, and no first-baseline creation in W2. |
| W3 numeric direct closure | C4; D4 `number_span_emit_slot`; P2-E `pt_digit_run_span_accumulate`; optional UDOT support | Direct/typed numeric consumer with scalar span/accumulate oracle and unchanged number semantics. | REDRESS 31, 39, 46, 80. No raw `parse::<f64>()`, mantissa/table widening without fallback pool, f64 fallback policy change, number side table, or digit work without same-wave product row movement. |
| W4 generated dispatch / byte-set control | C1, C5, C6, C7 support; D1 `container_tail_next`; D2 `direct_slot_dispatch` | Same-loop direct/typed container, whitespace, byte-class, or generated dispatch cleanup with scalar reference and product-row gate. | REDRESS 16/17/18/25, 50/51/53, 63/65/84, 92, 96/97/98/102. No pair-token fusion, function-pointer table, token-width churn, EventCursor, parser-local structural cursor, object next-key/value-byte carry, sidecar, class column, or parse-only close. |
| W5 bounded string span / special-byte scan | C2, C7 support; D3 `borrowed_string_span`; P2-E `pt_bounded_plain_string_end` | Direct/typed string/key consumer with scalar oracle; SIMD only after strict checkasm and caller microproof. | REDRESS 28+33, 49, 54/55, 59-62, 66-69, 72, 83, 106. No retained tiny-string parse fix, retained string widening, decoded scratch/stats/hash side channels, StringBlock16 wrapper, or primitive-parity-only production. |
| W6 escaped segment / hex decode | C3, C2/D3 support; P2-E `pt_escaped_string_segments`; `HEX_QUARTET_X4_PROOF` proof-only unless source delta exists | Direct/typed escaped-string consumer with scalar oracle; SIMD only after strict checkasm and caller microproof. | REDRESS 64, 82, 107, 108. No x4 proof-to-production promotion through the already-wired `unescape_string` caller, no JSON surrogate policy in generic code, and no single-quartet materializer. |
| W7 output digest/hash host sink | C8 per-product host sink/oracle only; C9 accounting | Product output sink only after fresh post-parser profile still names digest/hash as limiting. | Digest/hash as parser semantics, semantic string facts or hash side tables, cache hints/prefetch without fresh output-sink hot-leaf evidence. |
| W8 direct residual fixpoint / row reclamation | Remaining measured C1-C8 residuals; docs/gate by default | Close rows by strict measurement or record per-row uncloseable proof naming exhausted candidates. | Paper-close by routed residual, W0-clamped admission without behavior provenance, direct/typed guard demotion, and hidden telemetry fields not consumed by gate-json. |
| W9 close / Alpha feedback | Docs, RESULTS/REDRESS/SYNTHESIS/HANDOFF/SPEC/DISPATCH reconciliation | Close if direct rows are GO or each residual has measured uncloseable proof, and a non-JSON generated intervention admitted. | Close document drift, future-phase promises, G-Alpha presentation while any W1a-W8 wave lacks admitted/rejected/measured status. |

### §2.2 — Candidate-To-REDRESS Ledger

| Candidate or surface | Blocked REDRESS routes | Required avoidance rule |
|---|---|---|
| C1 byte-set / class-table masking | 50, 51, 53, 88, 89, 96, 97, 98, 102 | Produce transient masks or first offsets only. No retained positions, structural sidecars, class columns, streaming cursors, PMULL default body, CTZ bulk rewire, or parse-only admission. Same-loop direct/typed/non-JSON consumer must move or guard a row. |
| C2 bounded special-byte string/body scan | 28, 33, 60, 61, 62, 72, 83, 106 | Keep scalar span/end oracle first. Do not reopen active tiny NEON parser wiring, retained boundary collapse, retained 64-byte widening, generated-retained `StringBlock16`, or full-string microproof production without new caller and row gate. |
| C3 escape segment / hex decode | 49, 54, 55, 64, 66, 67, 68, 69, 82, 107, 108 | Generic primitive is escaped segment or hex-run decode. JSON `\uXXXX`/surrogate policy stays generated. x4 stays proof-only unless a new source delta beyond current `unescape_string` consumes it and direct/typed/non-JSON rows are measured. |
| C4 digit span / base-10 accumulation | 31, 39, 46, 80 | Scan/accumulate digits only. Do not alter f64 fallback, exponent/sign/leading-zero policy, raw parse shortcuts, or mantissa table coverage without fresh nonzero fallback-pool evidence and product consumer. |
| C5 byte-set layout skip | 51, 53, 59, 63, 65, 84, 102 | Generic helper is byte membership run skip. CSS/BBNF comments and grammar trivia remain generated policy. No whitespace cursor, sidecar bitmap, object key carry, value-byte return, or parse-only row claim. |
| C6 generated FIRST/prefix/lookahead dispatch | 16, 17, 18, 25, 63, 65, 84 | Generated dispatch may use grammar tables, but cannot reland pair-token fusion, function-pointer dispatch, token-width churn, separator elision, object next-key carry, or object-pair value-byte compaction under new names. |
| C7 movemask / bitmap support | 28, 33, 82, 83, 88, 89, 90 | Support only under C1/C2/C6. No standalone row movement, no retained bitmap column, no PMULL/CTZ production rewire, no B6 canary hardening as performance admission. |
| C8 output digest/hash oracle | 34, 35, 48, 54, 55, 69, 93, 100, 101, 109 | Use only as output oracle or product host sink. It cannot become parser vocabulary, generic hash fact, hidden sidecar, or Track 1 == Track 2 shared parser. Direct row movement follows REDRESS 100/101/109 contract only. |
| C9 output-plane / Lock-1 accounting | 34, 35, 48, 50, 51, 53, 92, 96, 97, 98, 102 | Accounting only. Every packet declares retained tape versus direct `SinkOnly`, Track 1/Track 2 independence, no hidden sidecar, and same-output proof. No row closes from tape bytes, PMU, lazy materialization, masking, or parse-only diagnostics. |
| D1 `container_tail_next` | 51, 53, 63, 65, 84, 96-98, 102 | May factor scalar delimiter/container tail only at current-pointer generated direct/typed sites. It cannot carry object-key/value-byte facts, parser-owned structural cursors, retained class lanes, or parse-only evidence. |
| D2 `direct_slot_dispatch` | 17, 34, 35, 48, 63, 65, 84, 100, 101 | Generated direct code shape only. It must preserve existing `BackendShape::SinkOnly`/`DirectBuild`, add no directive/BIR variant, and keep independent Track 2 separate from generated Track 1. |
| D3 `borrowed_string_span` | 49, 54, 55, 60-62, 66-69, 72, 82, 83, 106-108 | Return spans and `needs_decode` only. No decoded scratch, output hash shortcut, retained wide string fact, semantic string side channel, or already-wired x4 production close. |
| D4 `number_span_emit_slot` | 31, 39, 46, 80 | One number span may feed root/object/array/typed slots. Numeric materialization semantics remain current parse-that/generated policy; retained parse is not admission. |
| D5 `tape_sparse_flag_delta_lane` | 50, 51, 53, 92, 96-98, 102 | Only internal encoding of existing sparse flags while preserving `flags_at(cursor)`. No new facts, aux density table, direct row claim, parser-owned projection, retained class lane, or row movement from materialization counters. |

### §2.3 — Hard Blocks: No SK-V11 Wave May Reopen

These routes require a future Alpha/Omega governance change, not an SK-V11 wave plan:

- W3 union/event substrate and variants: REDRESS 92, 96, 97, 98, and 102 block retained class columns, move-consumed structural vectors, streaming cursors, class-lane-only repairs, `UnionTape`, W4-through-W3 cascade lock, parser-owned structural projections, and parse-only SOTA movement.
- New public substrate surfaces: new directive, BIR variant, `BackendShape`, public substrate API, sidecar event vector, retained position vector, parser-owned cursor/fact slot, or alternate retained tape.
- Track 1 == Track 2 or benchmark-private parser dishonesty: REDRESS 34, 35, and 48 block shared hidden parser proof; REDRESS 100/101/109 show the required direct movement contract.
- Parser-owned aux/sidecar projection: REDRESS 50, 51, and 53 block dense/sparse aux columns, EventCursor whitespace cursors, parser-local structural mask cursors, and second scanners.
- Generic-crate JSON policy leakage: REDRESS 36-38 and 85-87 block JSON-named generic helpers, JSON structural alphabets, and CostFacts-as-performance. Lock 14 requires generated per-grammar policy plus non-JSON proof.
- x86 implementation work. SK-V11 is aarch64 Apple Silicon only.

### §2.4 — Material-Differential-Only Routes

These may appear only when the wave explicitly names the material differential from the rejected route, before redress:

| REDRESS route | Existing rejection | Minimum material differential before SK-V11 redress |
|---|---|---|
| 28 + 33 tiny-string NEON/TBL | Active tiny-string dispatch regressed twitter and targeted the wrong boundary. | Different current hot boundary, scalar/checkasm, generated direct/typed or non-JSON consumer, and row gate. No parse-only close. |
| 49, 54, 55 decoded string materialization | Visitor, exact stats, and fused quote-source hashing lost to allocate-then-contiguous-hash baseline. | New product representation or generated typed/direct field materializer that beats current baseline through same-wave row gates. |
| 60-62 retained string scan | Boundary collapse and wide/delayed-wide retained scanners regressed guards or failed full gate. | Non-retained or generated product-plane string span with fresh P1 owner and no retained sidecar/widening. |
| 64, 82 unicode escape classifiers | Dense x4 validator and single-quartet path helped too narrow a shape and missed row gates. | Neutral escaped segment/hex-run primitive with new caller delta, scalar x4 oracle, strict invalid/mixed/alignment/surrogate parity, and product row gate. |
| 65, 84 object/key/value-byte carry | Object next-key and value-byte compaction were too small or regressed layout. | Generated FIRST/prefix/lookahead dispatch over grammar tables, not object carry, with direct/typed/non-JSON consumer. |
| 80 numeric fallback widening | Fresh attribution found zero fallback pool. | Fresh S-P1 evidence of nonzero fallback/hot digit pool and product consumer that does not change numeric semantics. |
| 88, 89 PMULL/CTZ production rewires | Correct primitives regressed JSON parse maintain rows. | Narrow non-default consumer with scalar/checkasm, feature/fallback, and full maintain block; PMULL/CTZ remain blocked as default hot bodies. |
| 93 Track 2 scalar parent fold | Track 2-only digest arithmetic missed selected direct floors. | W4/V11-aware direct contract, independent Track 2 backstop, full-table maintain, and a materially different direct-output/control path. |
| 103 instruments typed | Track 1 passed but independent Track 2 missed typed floor. | Different typed root/schema/oracle shape proving Track 2/oracle clears floor; no weakening to length/digest-only proof. |
| 106-108 string/escape microproof-to-production | Full string caller proof failed; x4 proof passed but existing production already consumed it; row floors failed. | New source delta and new consumer, not cosmetic wrapper or feature re-gate, plus direct/typed/non-JSON rows. |

### §2.5 — Proof-Only / Inventory-Only Surfaces

- `HEX_QUARTET_X4_PROOF` and REDRESS 107 remain proof-only. REDRESS 108 blocks claiming production from the already-wired `unescape_string` path.
- PMULL, CSSC CTZ, SHA3 EOR3/BCAX, PRFM/STNP/cache hints, movemask idiom swaps, `EXT` chunk context, `ADDV`/`CNT`, and canary hardening are inventory/support until an S-P3 wave names scalar reference, strict parity/checkasm, feature/fallback, caller microbench, same-wave consumer, and row gate.
- PMU counters, cycles, structural scan speed, masking probes, lazy tape byte counts, `tape_vs_tape`, and parse-only rows are diagnostics. REDRESS 102 and the SK-V11 goalset forbid admitting product rows from those signals.
- CostFacts and telemetry schemas are evidence consumers, not performance improvements. REDRESS 87 and SK-V11 telemetry binding require producer and consumer to land in the same wave.

## §3 — Falsifiability binding (named corpus rows + Mbps thresholds)

P3-E does not set final gate thresholds for every wave; P3-C owns the final threshold table. This ledger binds the minimum measurable surfaces that a route must carry before P3-C can accept it.

### Direct Residual Floors

Every JSON direct-row movement candidate must compare against the SK-V11-open strict same-run direct floor `ceil(sonic-rs direct / 1.10)` on both generated Track 1 and independent Track 2/oracle:

| Row | Seed floor Mbps |
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

`instruments`, `numbers`, and `unicode_mixed` are W0-clamped planning rows. Even if an opening run appears above floor, admission requires a behavior wave with provenance, REDRESS id, `gate-json` consumption, and unchanged strict contract.

### Guard And Generality Floors

- Existing direct admits `citm_catalog`, `apache_builds`, `marine_ik`, and `unicode_basic` remain guard rows. A wave touching direct/gate/report output must set maintain floors from SK-V11-open and cannot silently demote them.
- Existing typed admits `twitter`, `citm_catalog`, `apache_builds`, `github_events`, `update_center`, `mesh`, and `marine_ik` remain guard rows. A wave touching typed/gate/report output must set maintain floors from SK-V11-open.
- At least one non-JSON generated direct/typed benchmark must admit an SK-V11 intervention. Preferred surfaces are CSS L4 declaration values, then Sheets formulas, then BBNF-self. A prose Lock 14 proof does not close the axis.
- SIMD/ASM routes require scalar reference, strict parity/checkasm where applicable, same-host caller microbench, feature/fallback behavior, and a same-wave product consumer. Isolated primitive speed is not a row gate.

### Reopen Package

Any material-differential route must write this package into the wave plan:

| Requirement | Binding |
|---|---|
| REDRESS citation | Name the rejected entries and the material differential. |
| Fresh antecedent | Cite S-P1/S-P2 hot leaf and row family; no stale SK-V7/SK-V8/SK-V9 transfer. |
| Same-wave consumer | Direct, typed, or generated non-JSON hot path consumes the primitive/source delta in the same commit. |
| Scalar/parity | Scalar reference for every primitive; strict checkasm/differential parity for SIMD/ASM; product parity for scalar-only output-shape changes. |
| Row thresholds | Named corpus rows and Mbps thresholds, plus guard/maintain rows. |
| Lock 1 / Lock 14 proof | No sidecar substrate, hidden Track 2 coupling, directive/BIR variant, or generic JSON policy. Generic edits need non-JSON generated parser proof. |
| Revert protocol | Failed threshold reverts source, preserves rejected patch, and records REDRESS with Track 1, Track 2/oracle, comparator, floor, and reason. |

## §4 — Pre-blocked routes (REDRESS entries each wave must NOT re-open)

### W0 Baseline / Telemetry Lock

- Pre-block: all behavior source edits, generated parser changes, row-close claims from schema completion, stale sidecars as anchors, PMU/cycles-only claims, parse-only admission, and telemetry fields not consumed by gate-json.
- REDRESS: 78 and 87 for telemetry/CostFacts-as-evidence-only; 99 for explicit Criterion authority; 100 for direct row movement predicate.
- Avoidance rule: W0 may only bind evidence. It cannot move rows or reopen any rejected implementation route.

### W1a Non-JSON Gate / Report Schema Lane

- Pre-block: JSON-provider generated parser claims without non-JSON benchmark, old hand non-JSON struct-direct runtimes as proof, hidden directives/BIR variants, stale row-table reclamation, gate-only consumers, and producer-only telemetry.
- REDRESS: 34, 35, 36, 37, 38, 48, 85, 86, 87, 100, 101, 109; also SK-V11 goalset clauses 7 and 10.
- Avoidance rule: W1a creates the grammar-neutral gate/report lane only. It cannot create baseline authority or admit a row. Track 1, Track 2/oracle, grammar id, output plane, run id, feature mask, and same-wave consumer fields must be independently consumable by the gate.

### W1b Generated Non-JSON Baseline / Oracle Lane

- Pre-block: JSON-provider generated parser claims without non-JSON benchmark, old hand non-JSON struct-direct runtimes as proof, hidden directives/BIR variants, behavior intervention, coupled oracle, and row admission from baseline creation.
- REDRESS: 34, 35, 36, 37, 38, 48, 85, 86, 87, 100, 101, 109; also SK-V11 goalset clauses 7 and 10.
- Avoidance rule: W1b creates exactly one generated CSS/Sheets/BBNF-self baseline row and independent oracle/Track 2. It cannot land an intervention, claim admission, or leave W2 to invent the first baseline.

### W2 CSS L4 Generated Direct/Typed Intervention Proof

- Pre-block: prose-only Lock 14 proof, JSON-named generic helpers, `bbnf-simd` JSON god-module residue, fossil scanner routes, public JSON generic APIs, renamed JSON policy, CostFacts-as-performance, and non-JSON benchmark without generated Track 1 plus independent oracle.
- REDRESS: 36, 37, 38, 85, 86, 87; string/numeric/control REDRESS entries apply if the selected CSS proof uses those primitives.
- Avoidance rule: generic crate edits need the same-wave CSS generated direct/typed parser proof over the W1b baseline. CSS policy is generated per grammar; no generic helper may encode JSON roles. The row admits only with before/after Mbps and strict semantic equality. W2 may not create the first non-JSON baseline and intervention in one redress.

### W3 Numeric Direct Closure Slice

- Pre-block: raw `parse::<f64>()`, mantissa table widening, fallback elimination without fallback pool, table-only numeric work, number side table, digit microkernel without same-wave product consumer, and direct admission from parse-only or materialization counters.
- REDRESS: 31, 39, 46, 80.
- Avoidance rule: numeric waves can factor digit span or number-slot emission only if output remains bit/shape equivalent and a direct/typed/non-JSON numeric row moves. Full number grammar and conversion semantics stay in generated/parser policy.

### W4 Generated Dispatch And Byte-Set Control

- Pre-block: pair-token fusion, function-pointer dispatch table, 12-byte token churn, separator elision, generic SWAR whitespace transfer, EventCursor/sidecar, parser-local structural cursor, W3 union substrate, object next-key carry, object-pair value-byte compaction, retained position storage, and class lanes.
- REDRESS: 16, 17, 18, 25, 50, 51, 53, 59, 63, 65, 84, 92, 96, 97, 98, 102.
- Avoidance rule: W4 must stay inside generated direct/typed local cursor and same-output sink shape. A byte mask or dispatch table is transient and must be consumed in the same loop.

### W5 Bounded String Span / Special-Byte Scan

- Pre-block: active tiny-string NEON/TBL parse close, no-allocation decoded visitor, exact decoded stats sink, quote-source streaming hash, retained boundary collapse, retained wide/delayed-wide scanner, direct source-hook folding, parser-owned decoded scratch, byte-output unescape, semantic string facts, cap16/global string policy, generated-retained StringBlock16, and failed full-string microproof.
- REDRESS: 28, 33, 49, 54, 55, 60, 61, 62, 66, 67, 68, 69, 72, 83, 106.
- Avoidance rule: a string wave must name a new direct/typed/non-JSON product consumer and beat the current scalar/allocate baseline. Scalar/checkasm parity is necessary but insufficient; primitive-only paths are paper-close.

### W6 Escaped Segment / Hex Decode

- Pre-block: retained unicode x4 validator, single-quartet Unicode materializer, proof-only x4 promotion, JSON surrogate policy in generic crates, and already-wired `unescape_string` production as a new admit.
- REDRESS: 64, 82, 107, 108; also 54, 67, 68, and 69 for decoded scratch/materialization traps.
- Avoidance rule: W6 must name a new escaped-segment source delta and direct/typed/non-JSON product consumer. `HEX_QUARTET_X4_PROOF` remains proof-only unless strict scalar x4 oracle, checkasm, caller microbench, and row gate all land same-wave.

### W7 Output Digest/Hash Host Sink

- Pre-block: digest/hash as parser semantics, semantic string/hash side tables, cache hints/prefetch as performance proof, hidden Track 2 coupling, and output sink work without fresh limiting profile evidence.
- REDRESS: 54, 67, 68, 69, 78, 87, 93; cache-hint inventory remains non-admission unless a later plan provides a measured product-sink route.
- Avoidance rule: C8 can only move rows as a product output sink with strict Track 1/Track 2-or-oracle parity and a same-wave consumer. It must not enter generic parser crates as semantics.

### W8 Direct Residual Fixpoint / Row Reclamation

- Pre-block: paper close, route "wired" without row threshold, W0-clamped row admission without behavior provenance, direct fixpoint without per-row proof, missing REDRESS for failed waves, and silent guard-row demotion.
- REDRESS: 98, 100, 101, 102, 103, 106, 108, 110.
- Avoidance rule: every direct residual row is either `A / GO` by fixed floor or has a per-row uncloseable proof naming the failed intervention, measurements, comparator, floor, and exhausted route.

### W9 Close / Alpha Feedback

- Pre-block: future-phase promise, close document drift, G-Alpha presentation while any W1a-W8 wave lacks admitted/rejected/measured status, non-JSON axis closed by prose, unconsumed telemetry, and silent guard-row demotion.
- REDRESS: all unresolved REDRESS entries cited by W1a-W8 plus REDRESS 110.
- Avoidance rule: close requires every planned wave to admit or reject with measurement, direct rows either GO or per-row uncloseable proof, and at least one non-JSON generated parser intervention admitted.

### Routes No SK-V11 Wave May Reopen

1. W3 union/class-column/streaming-cursor/class-lane/sidecar substrate, including parse-only SOTA movement and W4-through-W3 cascade lock: REDRESS 92, 96, 97, 98, 102.
2. New directive, BIR variant, `BackendShape`, `UnionTape`, public substrate API, parser-owned fact slot, retained sidecar, structural-position vector, or alternate retained tape.
3. Track 1 == Track 2, benchmark-private parser, hidden hand sink, or shared parser evidence: REDRESS 34, 35, 48.
4. Direct row admission without strict same-run direct comparator, both tracks above floor, REDRESS provenance, and gate-json consumption: REDRESS 100, 101, 109 set the only admissible pattern.
5. Generic JSON policy in `parse-that-regex`, `bbnf-simd`, IR, codegen, or runtime outside generated grammar-local code: REDRESS 36-38, 85, 86.
6. PMULL prefix-XOR or CSSC CTZ bulk production as default hot path: REDRESS 88, 89.
7. x86 implementation work in SK-V11.

## §5 — Sources (every upstream artefact cited)

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v11/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md`
- `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
- `restart/skinny/tranches/sk-v9/research/p3/hardening/HARDENING-S-P3-CONVERGED.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` entries 16-18, 25, 28, 31, 33-39, 46, 48-55, 59-72, 76, 78, 80-90, 92-110.
