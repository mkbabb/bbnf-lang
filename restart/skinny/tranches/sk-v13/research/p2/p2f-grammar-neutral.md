# SK-V13 P2-F: Grammar-Neutral Abstraction

Pass: S-P2 Research. Cycle: V3.
Date: 2026-05-21.
Scope: grammar-neutral abstraction verdict for SK-V13 candidate primitives surfaced by S-P1, scoping, and sibling P2-A/B/C/D/E artefacts, with the V2 CH1 CSS row-scope fold applied.
Output: this file.
P1 hot-leaf antecedents: generated JSON parse/direct envelopes, `parse_that_regex::unescape_string`, `read_hex_unit_scalar`, mode-III structural scalar/SIMD scan, CSS declaration-values fact-sink profile, seven JSON typed leaves, ten missing typed surfaces.
Lock surface: Lock 14 primary; Lock 1 and Lock 16 secondary.

## §1 - Findings

P2-F V2 cross-read is complete against the sibling P2-A/B/C/D/E artefacts present
under `restart/skinny/tranches/sk-v13/research/p2/` at update time. P2-B adds
the FFmpeg/VideoLAN checkasm process gate and B1-B5 SIMD admission candidates; P2-C adds
C-P2C-1..7 host aarch64 candidates; P2-D adds D1-D5 substrate/tape candidates;
P2-E adds P2E-1..8 parse-that vocabulary gaps; P2-A adds C1-C8 comparator-led
candidate primitives. The table below folds those
names into one Lock-14 verdict matrix.

The S-P1 authority is intentionally non-admissive. The canonical ledger says
JSON parse/direct wrappers are envelopes, JSON unicode/string leaves are
JSON-confirmed only, typed leaves are JSON typed-only, CSS profiling is
timer/fact-sink dominated, and all rows are
`profile_signal_not_gate_admission`
(`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:10`-`23`).
P2-F therefore treats every primitive below as a research candidate, not an
admission.

The Lock 14 shape is strict. Generic crates carry no grammar arms, grammar-named
modules, grammar-specific public types, or grammar-specific feature flags; every
per-grammar runtime module must be generated from a single grammar-agnostic
template
(`restart/locks/LOCKS.md:78`). P2-F's verdict vocabulary is:

- `ADMISSIBLE-GRAMMAR-NEUTRAL`: expressible as a byte-set, classifier, regex,
  tape/fact, cost, or generated-template operation with no generic-crate grammar
  branch.
- `CONDITIONAL-GRAMMAR-NEUTRAL`: valid only after a named policy surface or
  same-wave non-JSON consumer is added.
- `JSON-OVERFIT`: currently depends on JSON-specific callbacks, FIRST sets,
  quote/escape policy, object/pair shape, or `JsonSink`.
- `INVENTORY-ONLY`: no current P1 antecedent or same-wave consumer; S-P3 should
  not shortlist without a fresh P2-B/C/D/E material differential.
- `CSS-ROW-SCOPE-CONDITIONAL`: a generated CSS parity row/fact-stream scope, not
  a primitive admission. S-P3 may plan it only with a fresh narrow CSS parser
  profile or same-wave strict lightningcss/cssparser row movement; do not read
  the grammar-neutral row template as a CH1 primitive hot-leaf proof.

CSS L4 is the binding non-JSON target. Sheets and BBNF-self remain fallback
history unless CSS L4 redress attempts are measured blocked, but P2-F still
uses them as generality probes: a grammar-neutral primitive should be
expressible for CSS L4 first, and should have a plausible Sheets and BBNF-self
mapping unless the production family is CSS-specific
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:375`-`390`).

The remaining Lock-14 leaks are implementation blockers for many candidates:
dispatch remains inline JSON FIRST bytes, string/escape policy is JSON quote
and backslash, number policy is JSON-shaped, `parse_key_colon()` is JSON-owned,
`OffsetFlags` names JSON semantics, and `JsonSink` is still the only sink trait
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:63`-`117`).
The legal SK-V13 route is generated `pub(crate)` per-grammar config and sink
surfaces, not a public `GrammarConfig` trait
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:216`-`238`).

## §2 - Candidate primitives

| Candidate | Shape | P1 antecedent | Scalar ref / checkasm / same-wave consumer | Grammar-neutrality verdict |
|---|---|---|---|---|
| CSS stylesheet + selector facts | Generated `stylesheet`, `ruleList`, `selectorList`, `qualifiedRule` fact stream | CSS row admitted only for declaration values; selectors missing in scoping matrix; no CSS parser hot leaf isolated. | Scalar oracle: lightningcss rule AST plus cssparser selector token check. Checkasm: n/a unless SIMD scanner added. Consumer: `css_l4/stylesheet_and_selectors/direct_to_struct/main`; S-P3 must require fresh narrow CSS parser profile or same-wave strict row movement. | `CSS-ROW-SCOPE-CONDITIONAL`, not primitive eligibility. The row template is grammar-neutral, but CSS facts are row-production scope until measured. |
| CSS declaration-value extension | Generated var/calc/color/url dispatch and fact stream | CSS declaration-values equality exists, parser hot leaf unresolved; feature gap lists var/calc/color missing. | Scalar oracle: lightningcss property value AST plus cssparser tokens. Checkasm: n/a for scalar codegen. Consumer: `css_l4/declaration_values_extended/direct_to_struct/main`; S-P3 must require fresh narrow CSS parser profile or same-wave strict row movement. | `CSS-ROW-SCOPE-CONDITIONAL`, not primitive eligibility. It becomes `JSON-OVERFIT` if it reuses JSON number/string rules unchanged. |
| CSS visual functions | Generated gradient, transform, filter, easing fact stream | CSS feature gap; no P1 parser hot leaf. | Scalar oracle: lightningcss AST plus cssparser token cross-check. Checkasm: n/a unless numeric SIMD is added. Consumer: `css_l4/visual_functions/direct_to_struct/main`; S-P3 must require fresh narrow CSS parser profile or same-wave strict row movement. | `CSS-ROW-SCOPE-CONDITIONAL`, not primitive eligibility. Formulas map conceptually to Sheets functions and BBNF calls, but CSS semantics stay in CSS metadata. |
| CSS at-rules and media queries | Generated `mediaRule`, `keyframesRule`, `mediaQueryList`, at-rule taxonomy | CSS feature gap; W1b fixture has only shallow `@media`; no P1 parser hot leaf. | Scalar oracle: lightningcss rule/media AST. Checkasm: n/a. Consumer: `css_l4/at_rules_and_media/direct_to_struct/main`; S-P3 must require fresh narrow CSS parser profile or same-wave strict row movement. | `CSS-ROW-SCOPE-CONDITIONAL`, not primitive eligibility. No generic at-rule branch may enter runtime or IR. |
| CSS nesting | Generated nested `ruleItem` and bounded recursion facts | CSS feature gap; no P1 hot leaf. | Scalar oracle: lightningcss nested `StyleRule::rules`. Checkasm: n/a. Consumer: `css_l4/nested_rules_and_queries/direct_to_struct/main`; S-P3 must require fresh narrow CSS parser profile or same-wave strict row movement. | `CSS-ROW-SCOPE-CONDITIONAL`, not primitive eligibility. Recursion/depth policy must be grammar metadata; container/scope query status needs S-P3 OUT_OF_SCOPE reconciliation. |
| Vendor/custom at-rule taxonomy | Generated `dashIdent` and custom at-rule fact categories | CSS feature gap; no P1 parser hot leaf. | Scalar oracle: lightningcss property normalization and cssparser at-rule scan. Checkasm: n/a. Consumer: `css_l4/vendor_and_custom_atrules/direct_to_struct/main`; S-P3 must require fresh narrow CSS parser profile or same-wave strict row movement. | `CSS-ROW-SCOPE-CONDITIONAL`, not primitive eligibility. Valid only if taxonomy is emitted from grammar metadata and not hardcoded in generic code. |
| Source/comment/whitespace facts | Offset/comment/whitespace fact emission | CSS profile top leaves are fact sink/timer overhead | Scalar oracle: cssparser token offsets and lightningcss source positions. Checkasm: n/a. Consumer: diagnostic row only. | `INVENTORY-ONLY` for SK-V13 admission because scoping marks source mapping/comments/whitespace low-priority or out of scope. |
| Per-grammar dispatch table | Generated FIRST-byte/action table per grammar | JSON direct and parse envelopes dominate P1; CSS needs different FIRST set | Scalar ref: byte-loop dispatch using generated table. Checkasm: n/a. Consumer: JSON guard + CSS stylesheet/declaration rows. | `ADMISSIBLE-GRAMMAR-NEUTRAL` if table comes from grammar metadata. This is the required refactor for JSON dispatch envelopes. |
| Per-grammar whitespace/comment skipper | `skip_ws_and_comments(input, cursor)` generated from layout policy | CSS delimiter/skip route; CSS feature gap for comments | Scalar ref: byte walk with grammar comment rules. Checkasm: optional if SIMD set-run-skip added. Consumer: CSS scanner rows. | `ADMISSIBLE-GRAMMAR-NEUTRAL`; applies to CSS comments, BBNF comments, Sheets whitespace. |
| P2E-1 `StringPolicy` / per-grammar string policy | Generated quote/escape/control policy and string span matcher | `match_tiny_plain_string*`, `unescape_string`, `read_hex_unit_scalar`; Lock-14 leak #3 | Scalar ref: policy-driven byte walk. Checkasm: required for SIMD/string masks. Consumer: JSON string rows plus CSS strings/escaped identifiers. | `CONDITIONAL-GRAMMAR-NEUTRAL`; current JSON quote/backslash hardcoding is `JSON-OVERFIT` until policy is generated. |
| P2E-4 `NumberPolicy` / per-grammar number policy | Generated number grammar and numeric token span | JSON direct envelopes; numbers row; CSS number features | Scalar ref: policy-driven number span with units/exponents/signs. Checkasm: required for UDOT digit kernels. Consumer: JSON number rows and CSS number/dimension tokens. | `CONDITIONAL-GRAMMAR-NEUTRAL`; current `match_number_span_from_first` is JSON-shaped. |
| Per-grammar `DirectSink`/view emission | Generated `NodeKind`, `ValueRef` wrappers, direct sink trait per grammar | Seven JSON typed-only rows and ten missing typed surfaces | Scalar ref: generated sink parity against oracle. Checkasm: n/a unless sink hashing SIMD added. Consumer: CSS direct row, future typed rows. | `ADMISSIBLE-GRAMMAR-NEUTRAL` only if generated from one template; hand-written `JsonSink` remains `JSON-OVERFIT`. |
| Per-grammar flag policy | Generated flag interpretation over storage bits | `OffsetFlags` JSON semantic leak | Scalar ref: bit decode/encode table per grammar. Checkasm: n/a. Consumer: string/escape/tape rows. | `CONDITIONAL-GRAMMAR-NEUTRAL`; current `HAS_ESC` / `HAS_CONTROL` names are JSON-overfit. |
| C-P2C-1 / P2E-6 / B2 `a64_ascii_set_run_skip` / `ByteSetRunSkip64` | `find_ascii_set_member64(bytes, cursor, end, set)` byte-set run skip | SK-V12 W4 microbench; CSS scan-block route | Scalar ref exists from W4; checkasm parity passed; same-wave consumer still required in CSS scan-block. | `ADMISSIBLE-GRAMMAR-NEUTRAL`: pure byte-set primitive. CSS delimiters, JSON structural sets, Sheets separators, and BBNF punctuation all fit. |
| Byte-set classifier (`byte_class_from_eq_set_64`, TBL/TBX family) | Parametric byte-class membership/mask | Mode-III structural scan SIMD; Lock 16 allowlist | Scalar byte-set classifier; checkasm required; consumer must be grammar-generated classifier. | `ADMISSIBLE-GRAMMAR-NEUTRAL` if alphabet is grammar-supplied. JSON `STRUCTURAL_BYTES` hardcoding is not enough. |
| `escape_mask_64` | Prefix mask for escape runs with carry-in/out | Escape correctness prerequisite; unicode/string candidates | Scalar byte-walk escape parity; direct checkasm still required for caller windows; consumer: string policy rows. | `CONDITIONAL-GRAMMAR-NEUTRAL`: valid for backslash-escape grammars; Sheets doubled-quote needs a different policy. |
| P2E-2 / C-P2C-5 64-byte string-special scan | Quote/escape/control byte-set scan across chunk boundaries | `match_tiny_plain_string*`, unicode/string rows, mode-III structural signal | Scalar 4x16 or byte-walk oracle; checkasm for alignment/tails/cross-chunk; consumer: JSON string or CSS escaped identifier. | `CONDITIONAL-GRAMMAR-NEUTRAL` if quote, escape, control, and terminator sets are generated. Current JSON constants are overfit. |
| B1 / C-P2C-4 / P2E-3 unicode and escape decoder | `unescape_string` / `read_hex_unit_scalar` generalized to escape productions | `unicode_escapes` rank-1 direct primitive; `y_string_unicode` parse leaf | Scalar decode table per grammar; checkasm for all nibble/tail/error cases if SIMD added; consumer: JSON and CSS escaped identifiers/strings. | `CONDITIONAL-GRAMMAR-NEUTRAL`; JSON `\uXXXX` and CSS hex escapes differ, so grammar policy is mandatory. |
| C-P2C-3 / P2E-5 UDOT digit-run / digit block decode | Dot-product digit accumulation and span validation | numbers/canada/mesh direct gaps; scoping UDOT candidate | Scalar digit-span oracle; checkasm for valid/invalid/mixed/overflow lanes; consumer: JSON number projection or CSS number token. | `ADMISSIBLE-GRAMMAR-NEUTRAL` in abstract: JSON numbers, CSS numbers/dimensions, Sheets formulas, and BBNF numeric literals all carry digit runs. |
| C-P2C-2 / P2E-8 PMULL + CSSC CTZ `GrammarStructuralScan` | SIMD structural-position matrix plus rank-order bit extraction | Mode-III structural SIMD beats scalar; union C scoping | Scalar consume-structural oracle; checkasm for density/alignment/string-boundary cases; consumer: JSON structural lane or CSS structural row. | `CONDITIONAL-GRAMMAR-NEUTRAL`; byte classes are grammar-supplied, but REDRESS 88/89/96/97/98 risks are high. |
| D1 lazy tape capacity policy | Grammar-neutral capacity/growth policy selected from observed density and generated grammar metadata | High allocated/logical tape ratios in `y_string_unicode`, `mesh`, `marine_ik`, `update_center`, `canada`, `numbers`; mode-III capacity signal | Scalar ref: current `CapacityPlan::GrowOnly` and `TapeBuilder`; parity: offset/flag traversal equality; consumer: retained parse rows or CSS retained row. | `NOT-S-P3-ELIGIBLE` as a standalone V2 behavior wave; legal and metadata-driven, but current P1 hot leaves are not payload/tape writers. Carry only as a measurement question or as part of a row-moving consumer after micro-proof. |
| `bitmap_prefix_xor_64` | Prefix-XOR/string mask bit operation | Orphan demoted; PMULL blocked history | Scalar bit oracle; checkasm exists/required; no current consumer. | `INVENTORY-ONLY` unless paired with union C or string scan consumer in same wave. |
| `bitmap_next_set_bit` / CSSC CTZ | Next-bit extraction from bitmap | Orphan demoted; CTZ blocked history | Scalar `trailing_zeros` oracle; checkasm required; no current hot consumer. | `INVENTORY-ONLY` unless consumed by PMULL+CTZ union or a measured bitset row. |
| `bulk_emit_positions_64` | Mask-to-offset compaction | Mode-III `bulk_emit_positions_64_neon` sidecar; orphan demoted | Scalar mask walk oracle; checkasm required; consumer must be active tape/fact emitter. | `CONDITIONAL-GRAMMAR-NEUTRAL` as a mask operation, but currently `INVENTORY-ONLY` for S-P3. |
| `byte_context` / `vextq_u8` | Cross-chunk byte-context propagation | String-special candidate; orphan demoted | Scalar neighboring-window oracle; checkasm for chunk edges; consumer: string/comment/identifier scanner. | `NOT-S-P3-ELIGIBLE` standalone; grammar-neutral only if consumed by a chunk-spanning token row or deleted/demoted with evidence. |
| `cache_hints` | PRFM/STNP memory hinting for tape/fact writes | Orphan demoted; no P1 hot leaf | Scalar n/a; microbench only; consumer must be a measured writer path. | `INVENTORY-ONLY`; grammar-neutral in theory, but no P1 antecedent or same-wave consumer. |
| EOR3/BCAX ternary boolean | Three-input boolean fold for masks | Scoping says no current three-input P1 expression | Scalar boolean oracle; checkasm if selected; consumer absent. | `NOT-S-P3-ELIGIBLE` for V2 S-P2; do not shortlist without a new hot expression. |
| LD4/TBX/SMIN/SMAX refinements | Load/deinterleave, out-of-range table, compare reductions | Scoping marks no measured consumer or no hot leaf | Scalar oracle varies; checkasm required if selected. | `INVENTORY-ONLY`; not eligible until P2-C/E names a measured consumer. |
| Fact-stream digest u64x2 | Vectorized digest fold over emitted facts | JSON direct envelopes and prior digest routes | Scalar fact-stream digest oracle; SIMD parity; consumer: JSON/CSS fact-stream rows. | `CONDITIONAL-GRAMMAR-NEUTRAL`; valid only if it hashes grammar-neutral fact streams, not `JsonDigestSink` internals. |
| Generated dispatch unroll | Template-level FIRST/action unroll and tail specialization | JSON direct envelopes (`parse_object_value_at_direct`, `parse_array_element_at_direct`) | Scalar generated dispatch table; checkasm n/a; consumer: JSON direct plus CSS generated dispatch row. | `ADMISSIBLE-GRAMMAR-NEUTRAL` if emitted from grammar facts; `JSON-OVERFIT` if it targets object/array/key colon directly. |
| P2E-7 regex analysis extraction | Regex HIR, nullability, first-set, char-class analysis | Decision-engine scoping; hardcoded regex predicates in IR | Scalar analysis functions; checkasm n/a; consumer: e-graph/CSP resolver and grammar recognizer mining. | `ADMISSIBLE-GRAMMAR-NEUTRAL` if named `parse-that-regex`/regex family per Lock 11 and no JSON pattern strings remain in IR. |
| E-graph language and rewrites | BackendExpr language, shape, regex, SIMD rewrites | Decision-engine scoping | Scalar/proof oracle: rewrite equivalence tests; checkasm n/a. Consumer: resolver-selected generated rows. | `CONDITIONAL-GRAMMAR-NEUTRAL`; rewrites must be over IR facts, not grammar names, and must obey Lock 4 output-piping. |
| Active cost function | CostFacts as extractor cost over measurements/code size | Decision-engine scoping; passive ledger today | Scalar/proof oracle: deterministic ranking tests. Consumer: resolver selection. | `ADMISSIBLE-GRAMMAR-NEUTRAL` if costs are keyed to rule facts and measured rows; stale JSON-only costs are overfit. |
| CSP resolver | Constraint problem over shape, recognizer, sink, capacity | Decision-engine scoping | Scalar/proof oracle: UNSAT/SAT test matrix; checkasm n/a. Consumer: generated backend selection. | `CONDITIONAL-GRAMMAR-NEUTRAL`; egraph and CSP must compose by output-piping, not a fused solver. |
| P1-P8 cascade deletion | Remove hardcoded priority cascade | Decision-engine scoping names JSON-specific recognizer/cascade | Regression oracle: JSON equality vs SK-V12 baseline and CSS equality. | `ADMISSIBLE-GRAMMAR-NEUTRAL`; deletion removes a hardcoded route, but only with resolver replacement and equality gates. |
| Union C1 per-rule policy | Codegen-time per-rule tape/event selection in config | Union scoping, GrammarConfig legality | Scalar fixed-shape tape baseline; parity across rule boundaries; consumer: CSS row and JSON guard. | `CONDITIONAL-GRAMMAR-NEUTRAL`; legal only if codegen-private, no `UnionTape`, no new directive/BIR/BackendShape/API. |
| Union C2 e-graph selected shape | E-graph equivalence class selects tape/event shape | Union + decision-engine scoping | Scalar C1/fixed baseline; equivalence proof; consumer: measured JSON/CSS row. | `CONDITIONAL-GRAMMAR-NEUTRAL`; high CH5 coupling risk, but not JSON-overfit if shapes are IR-derived. |
| Union C3 SIMD lane index | PMULL/CSSC/EOR3 vector index drives union selection | Mode-III scan signal, union C scoping | Scalar structural oracle; checkasm for vector lanes; consumer: JSON/CSS structural row. | `CONDITIONAL-GRAMMAR-NEUTRAL`; grammar byte-set parameterization required; current route is high-risk historical REDRESS adjacency. |

### P2-A C1-C8 literal mapping

| P2-A candidate | P2-F mapped verdict | Mapping boundary |
|---|---|---|
| C1 `class_mask64_transient` | `ADMISSIBLE-GRAMMAR-NEUTRAL` | Maps to the byte-set classifier family when classes are generated grammar tables and the mask is transient. Reject retained class sidecars. |
| C2 `bounded_special_string_end` | `CONDITIONAL-GRAMMAR-NEUTRAL` | Maps to P2E-1/P2E-2 string policy and 64-byte special scan. Valid only after quote/escape/control/terminator policy is generated and a JSON or CSS string row consumes it. |
| C3 `escape_segment_hex_decode` | `CONDITIONAL-GRAMMAR-NEUTRAL` | Maps to P2E-3 / B1 / C-P2C-4 escape decode. JSON `\uXXXX` and CSS variable-width escapes require separate grammar policy; the nibble core is neutral. |
| C4 `digit_run_accumulate` | `ADMISSIBLE-GRAMMAR-NEUTRAL` | Maps to P2E-4/P2E-5 and C-P2C-3 digit-run work. JSON, CSS, Sheets, and BBNF can share digit spans if sign/suffix/materialization policy stays generated. |
| C5 `generated_first_follow_probe` | `ADMISSIBLE-GRAMMAR-NEUTRAL` | Maps to per-grammar dispatch tables and generated dispatch unroll. Reject JSON object/array/key-colon branches in generic code. |
| C6 `same_loop_structural_mask_consume` | `CONDITIONAL-GRAMMAR-NEUTRAL` | Maps to P2E-8 / D2-D3 / C-P2C-2 union routes. It must consume transient masks into the single tape or sink-only projection and carry REDRESS 96/97/98 material differential. |
| C7 `ascii_set_member_find64_css` | `CONDITIONAL-GRAMMAR-NEUTRAL` route-production | Maps to C-P2C-1 / P2E-6 / B2 `ByteSetRunSkip64`. V2 treats it as conditional because CSS P1 is not yet a parser-hot-leaf proof; S-P3 must require a fresh narrow CSS profile or same-wave scan-block row movement. |
| C8 `output_digest_fold_u64x2_sink` | `CONDITIONAL-GRAMMAR-NEUTRAL` only over fact streams | Maps to fact-stream digest u64x2. It is JSON-output-plane scoped unless a grammar-neutral fact stream is the row output; reject parser-speed claims or `JsonDigestSink` internals. |

## §3 - Grammar-neutrality

CSS L4 row scopes are admissible only as row-production work: generated fact
streams with strict equality against lightningcss and cssparser plus either a
fresh narrow CSS parser profile or same-wave strict row movement. They are not
generic runtime primitives by themselves and they are not CH1 primitive
hot-leaf evidence. The grammar-neutral property is that the same template and
metadata mechanism could emit Sheets or BBNF-self without adding grammar arms to
generic crates. The scoping matrix confirms the SK-V12 row is only declaration
values and that selectors, stylesheet root, variables, calc, visual functions,
media, nested rules, and several at-rule families remain missing or partial
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:96`-`132`).

Sheets and BBNF-self are not SK-V13 admission targets unless CSS is blocked,
but they expose the overfit test. The following mappings are the minimum P2-F
expects S-P3 to carry:

- Byte-set classifiers and run-skip map to Sheets separators/quotes and BBNF
  punctuation just as they map to CSS delimiters and JSON structural bytes.
- String and escape primitives need a policy object: JSON backslash, CSS
  escaped identifiers/strings, Sheets doubled quotes, and BBNF strings do not
  share one hardcoded escape model.
- Number primitives map broadly: JSON numbers, CSS dimensions/percentages,
  Sheets numeric formulas, and BBNF numeric literals.
- Regex/recognizer rewrites are grammar-neutral only if regex facts come from
  grammar IR or `parse-that-regex`, not from hardcoded pattern strings.
- Union variants are grammar-neutral only as codegen-private tape/fact routing
  over one substrate; a retained side vector, parser-owned cursor/list, or
  public substrate API violates Lock 1 and Lock 14.

S-P3 carry-forward classification:

- Eligible primitive/refactor families: per-grammar dispatch/whitespace/string/number/sink
  refactors, `a64_ascii_set_run_skip` production wiring, generic byte-set
  classifier, UDOT digit primitive, fact-stream digest iff generalized, regex
  extraction, e-graph/cost/CSP resolver surfaces, P1-P8 cascade deletion, and
  union C1/C2/C3 under the stated caveats.
- Conditional row-production scopes, not primitive eligibility: CSS rows 1-6.
  S-P3 may plan them only with fresh narrow CSS parser profiling or same-wave
  strict lightningcss/cssparser row movement, and any SIMD primitive inside the
  row inherits its own scalar/checkasm/consumer gate.
- `NOT-S-P3-ELIGIBLE` without new P2 evidence and same-wave consumer:
  EOR3/BCAX, TBX/LD4/SMIN/SMAX, `cache_hints`, standalone
  `bitmap_prefix_xor_64`, standalone `bitmap_next_set_bit`, standalone
  `bulk_emit_positions_64`, standalone `byte_context`, and standalone D1 lazy
  tape capacity policy.
- Must be rejected as written: JSON object/array/key-specific dispatch rewrites,
  `JsonSink`-specific sink acceleration, hardcoded JSON quote/backslash string
  widening, or any candidate that claims grammar-neutrality from JSON profile
  envelopes alone.

## §4 - Risks

REDRESS 96/97/98 are the binding union history. New union attempts are
unblocked only at category level and must cite those failures, name the material
differential, and avoid a sidecar substrate or parser-owned cursor
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:79`-`85`;
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:289`-`301`).

REDRESS 88/89 and REDRESS-126 govern PMULL, CSSC CTZ, prefix-XOR,
bulk-emission, and orphan SIMD inventory. The five demoted aarch64 primitives
remain history-only, and new attempts in the same spaces require scalar
reference, checkasm parity, material differential, and same-wave consumer
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:16`-`23`;
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:390`-`396`).

REDRESS 119/120 are history under the full-SOTA addendum, but P1 profile
signals do not reopen direct rows by themselves. Every JSON row reopen needs a
prior-fixpoint citation, fresh material differential, and same-harness strict
comparator evidence
(`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:115`-`128`).

Lock 14 rejects generic-crate grammar branches and hand-written per-grammar
runtime files. P2-F therefore rejects public `GrammarConfig` as an unapproved
surface in V1 and prefers generated `pub(crate)` per-grammar modules until
G-Omega/S-P3 changes the authority
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:272`-`281`).

Lock 16 rejects support-only SIMD. A primitive with no same-wave measured row
consumer is an orphan and must be skipped, deleted, or demoted with REDRESS
evidence
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:360`-`388`).

Lock 4 rejects a fused CSP/e-graph solver even though the SK-V13 decision
engine wants a resolver fold. The legal shape is output-piping:
regex facts -> e-graph candidates -> cost extraction -> CSP assignment ->
codegen. Any single hypergraph that imports e-graph directly into CSP or hides
domain ownership is a Lock 4 violation
(`restart/locks/LOCKS.md:58`-`62`).

## §5 - Sources

- `restart/prompts/skinny/PASS-2-RESEARCH.md`
- `restart/skinny/tranches/sk-v13/HANDOFF.md`
- `restart/locks/LOCKS.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md`
- `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md`
- `restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-pass-framework-leverage.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2e-parse-that-gaps.md`

## §6 - V3 Cross-Read Disposition

P2-F V3 incorporates the P2-A/B/C/D/E sibling artefacts, the V1 CH2 fold, and
the V2 CH1 CSS row-scope revise. CSS stylesheet/selectors, declaration-value
extension, visual functions, at-rules/media, nesting, and vendor/custom at-rule
taxonomy are now `CSS-ROW-SCOPE-CONDITIONAL`: row-production scopes requiring
fresh narrow CSS parser profile or same-wave strict row movement, not primitive
hot-leaf admissions.

The V2 accepted folds remain unchanged. P2-F still incorporates the P2-A/B/C/D/E
siblings and the V1 CH2 fold.
The literal C1-C8 mapping above resolves the cross-read blocker. No sibling
output requires changing the core Lock-14 verdict: grammar-neutrality is
admitted at byte-set, policy, fact-stream, regex-analysis, resolver, or
codegen-private same-substrate boundaries; JSON-specific wrappers remain
overfit; support-only SIMD remains inventory.

Inventory/drop decisions are binding for S-P3 unless a later accepted research
cycle adds fresh evidence: EOR3/BCAX, cache hints, standalone prefix/next/bulk
bitmap primitives, standalone `byte_context`, LD4/TBX/SMIN/SMAX refinements,
and D1 lazy capacity as a standalone wave do not enter the shortlist. Residual
V2 work is limited to challenge confirmation or candidate renames; P2-F should
not add new source claims outside that convergence scope.
