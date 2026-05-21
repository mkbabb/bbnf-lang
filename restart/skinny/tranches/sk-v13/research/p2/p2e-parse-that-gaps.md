# SK-V13 P2-E: parse-that Primitive Gaps

Pass: S-P2 Research. Cycle: V2.
Date: 2026-05-21.
Scope: parse-that primitive vocabulary gaps demanded by SK-V13 S-P1 hot leaves.
Output: this file.
P1 hot-leaf antecedents: generated JSON `dispatch_value` / direct envelopes, `match_tiny_plain_string_with_cap`, `parse_that_regex::unescape_string`, `read_hex_unit_scalar`, direct number envelopes, structural `scan_tail` / `scan_structurals`, CSS timer/fact-sink profile.
Lock surface: both — Lock 1 substrate union and Lock 14 grammar-neutrality; Lock 16 SIMD/checkasm is carried as the primitive admission gate.

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

V2 challenge fold note: CH1/CH2/CH6 did not require changing the P2-E
primitive set. The inventory/drop boundaries for P2E-6/P2E-8 remain governed by
P2-B/P2-C/P2-F: CSS byte-set run-skip needs a same-wave CSS scan-block
consumer, and structural scan remains transient/single-substrate only.

1. parse-that is already the right ownership boundary for strings, numbers, unicode, and regex-shaped leaf work, but its public vocabulary is still thinner than the SK-V13 hot leaves require. `parse-that-regex` currently exports string, number, unicode, and a tiny integration hook (`skinny/crates/parse-that-regex/src/lib.rs:4-8`, `skinny/crates/parse-that-regex/src/integration/simd_scan_hook.rs:3-18`), while the generated JSON parser still owns policy-specific dispatch, tiny-string, and delimiter control (`skinny/crates/runtime/src/grammars/json/generated.rs:45-56`, `skinny/crates/runtime/src/grammars/json/generated.rs:159-183`, `skinny/crates/runtime/src/grammars/json/generated.rs:290-303`).

2. The Layer-0 / Layer-1 split must stay explicit. Layer 0 is `bbnf-simd`: scalar/vector primitive bodies, scalar parity anchors, and checkasm tests. Layer 1 is parse-that: grammar-neutral byte-stream primitives consumed by generated per-grammar code. The live history names this two-layer vocabulary as the canonical split (`skinny/REDRESS.md:341-342`), and the S-P2 contract requires P2-E to place every gap in that scheme (`restart/prompts/skinny/PASS-2-RESEARCH.md:224-231`).

3. String scanning is policy-hardcoded even though the hot leaves demand policy-neutral spans. The current trusted JSON string matcher assumes quote `b'"'`, escape `b'\\'`, and JSON control cutoff (`skinny/crates/parse-that-regex/src/lib.rs:162-260`); the generated tiny-string fast path repeats those assumptions (`skinny/crates/runtime/src/grammars/json/generated.rs:169-183`). The value-api scoping audit marks this as a residual Lock-14 leak and says CSS/Sheets need generated string policy functions instead of JSON assumptions (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:94-99`, `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:196-206`).

4. Unicode escape decode is a named hot primitive but the API shape is still JSON-string materialization. S-P1 found `unescape_string` rank-1 on `unicode_escapes` direct Track 1 / Track 2 (`restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:74-75`, `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:105-106`), and parse-only `y_string_unicode` exposed `read_hex_unit_scalar` (`restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:85-86`). The current body has an aarch64 x4 `\uXXXX` path (`skinny/crates/parse-that-regex/src/lib.rs:384-459`) and a `Cow<str>` unescape API (`skinny/crates/parse-that-regex/src/lib.rs:718-809`), which is not enough for CSS escapes, selector identifiers, or direct fact streams without re-entering rejected sink-local decoded-string routes.

5. Number matching is JSON-shaped and duplicated by CSS. `match_number_span_from_first` allows JSON `-`, forbids `+`, forbids leading `.`, and couples mantissa/exponent facts to JSON token shape (`skinny/crates/parse-that-regex/src/number/mod.rs:37-103`). CSS generated declaration code has a separate leading-dot/plus/minus scanner (`skinny/crates/codegen/src/css_l4_declaration_values_templates/generated.rs:227-250`). The value-api audit names this as a partial leak and calls for generated `match_css_number()` or a `NUMBER_POLICY` (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:68`, `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:204-206`).

6. The regex analysis surface is scattered outside parse-that. IR nullability carries a local `regex_is_nullable` helper (`skinny/crates/ir/src/lib.rs:298-335`), and passes first-byte analysis special-cases regex strings (`skinny/crates/passes/src/lib.rs:779-796`). The decision-engine scoping packet makes regex/HIR extraction a critical path because e-graph/CSP rewrites need generic nullable/first-set/byte-class facts (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md:35-53`, `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md:476-538`).

7. Structural/byte-class SIMD exists below parse-that but not as a row-moving parse-that primitive. Mode III shows structural SIMD beats scalar scan on all 17 JSON corpora, with the biggest ratios on `mesh`, `canada`, and `numbers` (`restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:102-116`). The implementation surface is split: `bbnf-simd` exposes structural indexes and primitive dispatch (`skinny/crates/bbnf-simd/src/lib.rs:106-123`, `skinny/crates/bbnf-simd/src/lib.rs:231-260`), JSON owns string-aware structural scanning (`skinny/crates/runtime/src/grammars/json/scan.rs:22-35`, `skinny/crates/runtime/src/grammars/json/scan.rs:107-160`), and the generated JSON `attach_structural_index` is still a no-op (`skinny/crates/runtime/src/grammars/json/generated.rs:10-15`). A parse-that Layer-1 scan primitive must consume Layer-0 masks without creating a retained sidecar.

8. The SIMD inventory is real but dangerous. SK-V12 W4 demoted five aarch64 primitives as inventory-only and kept `a64_ascii_set_run_skip` as a microbench-passed production split (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:12-23`, `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:31-73`). Any parse-that gap that touches prefix-XOR, CTZ, bulk emit, byte context, cache hints, or ASCII-set skipping must keep scalar reference, checkasm parity, same-wave consumer, and zero-orphan disposition explicit (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:356-394`).

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

### P2E-1 — `StringPolicy` + `match_string_span_policy`

- **Shape:** Layer-1 parse-that primitive:
  `match_string_span_policy(input, offset, policy) -> Result<StringMatch, RegexError>`, where `policy` carries `terminator`, `escape: Option<u8>`, `control_limit`, UTF-8 validation mode, raw-newline rule, and flag semantics. Generated grammar modules supply the policy from per-grammar config; generic crates do not branch on grammar names.
- **Layer placement:** Layer 1 owns the policy and span API. Layer 0 remains `bbnf-simd::aarch64::string_block` for block scanning.
- **Scalar reference sketch:** byte-walk from `offset + 1` until terminator; on escape, delegate to the policy's escape validator; on control/non-ASCII, apply policy-specific flags and UTF-8 validation. It must reproduce current JSON behavior for `terminator=b'"'`, `escape=Some(b'\\')`, `control_limit=0x20`.
- **Checkasm expectation:** no new checkasm for the scalar-only policy wrapper; any SIMD path used by the wrapper must pass string-block parity across terminator/escape/control matrices, invalid UTF-8, raw control bytes, alignment 0-15, and tails.
- **Arch:** portable scalar first; optional aarch64 NEON through P2E-2.
- **P1 antecedent:** `distinct_values` parse-only exposes tiny-string, and unicode/string direct rows are worst-c/B rows (`restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:81-86`, `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:111-114`).
- **Same-wave consumer note:** generated JSON string/key path and one CSS string/identifier row must consume the policy in the admitting wave; support-only policy extraction is not enough.

### P2E-2 — `StringSpecialBlock64`

- **Shape:** Layer-0 `bbnf-simd` primitive plus Layer-1 parse-that wrapper:
  `scan_string_special_64(ptr, policy) -> StringSpecialBlock64`, returning terminator, escape, control, and non-ASCII masks for 64 bytes. Layer 1 exposes `find_string_special_policy(input, cursor, policy)`.
- **Layer placement:** Layer 0 owns 64-byte scalar reference and aarch64 implementation. Layer 1 owns chunk iteration, UTF-8 boundary handling, and policy mapping.
- **Scalar reference sketch:** four consecutive 16-byte scalar `StringSpecialBlock` scans OR-shifted into u64 masks; tail byte-walk for fewer than 64 bytes. The existing 16-byte scalar reference is `scan_string_special_block_scalar` (`skinny/crates/bbnf-simd/src/aarch64/string_block.rs:30-54`).
- **Checkasm expectation:** 150+ cases minimum: terminator/escape/control/non-ASCII permutations, long backslash runs split across chunks, all alignments 0-15, tails 0-63, and JSON plus CSS policies.
- **Arch:** aarch64 NEON first; no x86 in SK-V13.
- **P1 antecedent:** string-heavy direct rows and structural scan masking; `unicode_escapes` direct rank-1 is unescape, `distinct_values` exposes tiny string, and mode-III structural SIMD is positive on all rows (`restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:77-80`, `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:71-87`).
- **Same-wave consumer note:** JSON direct/string path or CSS escaped-identifier scan must call the 64-byte wrapper and move a measured row; do not land it as a union prerequisite only.

### P2E-3 — `EscapePolicy` + `decode_escape_run`

- **Shape:** Layer-1 parse-that primitive:
  `decode_escape_run(input, slash, policy, sink) -> Result<usize, EscapeError>`, where `policy` distinguishes JSON simple escapes, JSON `\uXXXX` surrogate pairs, CSS hex escapes with optional trailing whitespace, and grammar-specific literal escape rules. `sink` is a byte/char callback or fact-stream writer, not a JSON digest hook.
- **Layer placement:** Layer 1 owns escape grammar and sink callback shape. Layer 0 owns the nibble/TBL quartet decoder (`unescape_uxxxx_x4_neon`) and any future CSS hex-batch body.
- **Scalar reference sketch:** existing `decode_unicode_escape` / `read_hex_unit_scalar` for JSON (`skinny/crates/parse-that-regex/src/lib.rs:302-343`, `skinny/crates/parse-that-regex/src/lib.rs:918-966`), plus a CSS scalar hex-codepoint reader with 1-6 hex digits and optional single whitespace terminator.
- **Checkasm expectation:** quartet/x4 parity against scalar for BMP, surrogate pairs, invalid hex, low-surrogate-first, incomplete runs, mixed valid/invalid lanes; CSS hex escape parity if the CSS branch is selected.
- **Arch:** portable scalar + aarch64 NEON TBL x4 for JSON unicode; CSS batch only after a scalar oracle and same-row CSS consumer exist.
- **P1 antecedent:** `unescape_string` rank-1 on `unicode_escapes`, `read_hex_unit_scalar` on `y_string_unicode` (`restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:74-75`, `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:85-86`).
- **Same-wave consumer note:** JSON unicode direct row or CSS escaped-ident/string row must consume it. A generic `Cow<str>` replacement without row movement repeats REDRESS 54/55/66-69 risk.

### P2E-4 — `NumberPolicy` + `match_number_span_policy`

- **Shape:** Layer-1 parse-that primitive:
  `match_number_span_policy(input, offset, first, policy) -> Option<NumberSpan>`, with policy bits for leading sign (`+` allowed), leading dot, required integer digits, fractional digits, exponent, percent/dimension suffix stop, and materialization needs.
- **Layer placement:** Layer 1 owns grammar number syntax and `NumberSpan`. Layer 0 owns optional digit-run acceleration (P2E-5).
- **Scalar reference sketch:** parameterize the current JSON FSM in `match_number_span_from_first` (`skinny/crates/parse-that-regex/src/number/mod.rs:37-103`), then prove JSON policy is byte-for-byte identical to current spans. CSS policy must match current `starts_number` / `consume_number` behavior (`skinny/crates/codegen/src/css_l4_declaration_values_templates/generated.rs:227-250`).
- **Checkasm expectation:** none for policy scalar; if P2E-5 is selected, run digit-run checkasm before routing through this API.
- **Arch:** portable scalar first.
- **P1 antecedent:** `numbers`, `canada`, `mesh`, and direct envelopes show numeric rows remain row-moving candidates; direct `numbers` c/B is measured and `numbers` is singled out for D4 numeric reopen in scoping (`restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:88`, `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md:126`).
- **Same-wave consumer note:** generated JSON number path and CSS declaration-value/dimension path must use the same policy API in the admitting wave, with JSON guard and CSS strict equality.

### P2E-5 — `DigitRunMac4x` / UDOT digit-run consumer

- **Shape:** Layer-0 `bbnf-simd` primitive:
  `parse_4_digits_udot([u8; 4]) -> Option<u32>` and possibly `parse_16_digits_udot([u8; 16]) -> DigitRunFacts`, exposed to Layer 1 as an optional accelerator for `NumberPolicy`.
- **Layer placement:** Layer 0 only for UDOT/dotprod bodies and scalar references. Layer 1 decides whether a grammar's number policy and density warrant calling it.
- **Scalar reference sketch:** current scalar digit parsers in `parse-that-regex::number` (`parse_four_digits`, `parse_eight_digits`) are the executable reference (`skinny/crates/parse-that-regex/src/number/mod.rs:206-223`). `bbnf-simd::aarch64::digit_mac::parse_4_digits` already has a scalar fallback but has no production consumer (`skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:4-23`).
- **Checkasm expectation:** strict parity for valid/invalid lanes, mixed validity, overflow boundaries, endian/order, alignment, and dotprod feature gating. The scoping packet already says the current UDOT path is proof-only and needs x4 strict parity plus a JSON numeric consumer (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:296-314`).
- **Arch:** aarch64 dotprod only; portable scalar fallback mandatory.
- **P1 antecedent:** numeric direct rows and structural SIMD ratios (`restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:88`, `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:115-116`).
- **Same-wave consumer note:** if selected, wire through P2E-4 into JSON numeric projection or CSS numeric row in the same wave. Otherwise leave the existing file inventory-demoted; do not add another orphan.

### P2E-6 — `ByteSetRunSkip64`

- **Shape:** Layer-1 parse-that primitive:
  `find_next_in_set(input, cursor, end, set: ByteSet) -> usize` and `skip_while_in_set(...)`, backed by Layer-0 `byte_class_from_eq_set_64` / `byte_class_from_table_64`.
- **Layer placement:** Layer 0 owns byte-class masks; Layer 1 owns cursor/end semantics, duplicate-set handling, and grammar policy. For CSS comments, this primitive may only find delimiter bytes; comment consumption remains a policy-level scalar or separate parser primitive.
- **Scalar reference sketch:** byte-walk until `set.contains(byte)`, identical to the checkasm scalar in `checkasm_ascii_set_member_find_64.rs:20-27`.
- **Checkasm expectation:** current W4 checkasm covers cursor/end/tails/no-hit/first-hit/duplicate sets/high-bit bytes/CSS fixture/adversarial seeds (`skinny/REDRESS.md:3774-3782`, `skinny/crates/bbnf-simd/tests/checkasm_ascii_set_member_find_64.rs:103-191`). Production wave must add same-row CSS equality/throughput measurement.
- **Arch:** aarch64 NEON eq-set current body (`skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:27-73`), scalar fallback (`skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:20-38`).
- **P1 antecedent:** CSS profile exists but is timer/fact-sink dominated, so the stronger antecedent is SK-V12 W4's 4.72x microbench plus S-P1 structural scan facts. P1-F says CSS V2 equality holds but method mismatch makes it profile signal only (`restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:79-88`).
- **Same-wave consumer note:** CSS `skip_ws_and_comments`, trim, selector delimiter, or declaration delimiter loop must name this primitive and preserve lightningcss equality.

### P2E-7 — `RegexHirAnalysis`

- **Shape:** regex-analysis primitive vocabulary:
  `RegexHir`, `nullability_analysis`, `first_set_analysis`, `byte_class_analysis`, and optional `nfa_dfa_cost_facts`. The naming (`bbnf-regex` sibling vs. `parse-that-regex::regex`) is for S-P3 to settle against Lock 11 and G2; P2-E's requirement is the cohesive API surface, not the crate name.
- **Layer placement:** Layer 1 analysis, not Layer 0 SIMD. It feeds codegen/e-graph/CSP and can later select byte-class Layer-0 primitives.
- **Scalar reference sketch:** parse regex into a small HIR, evaluate nullable/first-set by recursive interpretation, and prove the current JSON whitespace/string/number cases produce the same facts as `ir` / `passes` helpers.
- **Checkasm expectation:** no checkasm unless the analysis selects a SIMD recognizer. Unit/property tests are mandatory for nullable, first-set, char-class, alternation, escapes, and unsupported-pattern fail-closed behavior.
- **Arch:** portable.
- **P1 antecedent:** generated parse/direct envelopes dominate S-P1 and hide primitive attribution; resolver work needs regex facts before S-P3 can split those envelopes into selectable primitive recognizers (`restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:64-80`, `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md:37-53`).
- **Same-wave consumer note:** decision-engine waves must consume the API immediately in IR/passes. A crate extraction with zero resolver/codegen consumer is a support-only landing and should fail CH6.

### P2E-8 — `GrammarStructuralScan`

- **Shape:** Layer-1 scan primitive:
  `scan_structural_blocks(input, ScanPolicy, consumer)` where `ScanPolicy` supplies structural byte set, optional string policy, and whether quote/escape state suppresses class hits. The consumer receives transient block masks or emitted offsets; it must not retain a sidecar.
- **Layer placement:** Layer 1 owns grammar policy and single-substrate consumption. Layer 0 owns byte-class, escape-mask, prefix-XOR, CTZ/bulk emit bodies.
- **Scalar reference sketch:** byte-walk with `in_string` and `escaped` state, matching JSON `scan_tail_byte` semantics for JSON policy (`skinny/crates/runtime/src/grammars/json/scan.rs:107-160`), then generalized by policy.
- **Checkasm expectation:** block-mask parity for JSON structural set and at least one CSS structural set; direct `escape_mask_64` checkasm must remain green on the xorshift falsifier and long backslash runs (`skinny/crates/bbnf-simd/tests/checkasm_escape_mask_64.rs:55-123`).
- **Arch:** portable scalar; optional aarch64 TBL, PMULL, CSSC CTZ, EOR3 only with material differential and same-wave consumer.
- **P1 antecedent:** structural SIMD beats scalar scan on all 17 rows, but S-P1 explicitly says this is not row admission and REDRESS 96/97/98 remain binding (`restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:115-133`, `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:131-133`).
- **Same-wave consumer note:** this candidate is legal only when consumed by generated JSON/CSS parsing or a union variant in the same wave. A retained `StructuralIndex`, parser-local cursor, or class side vector repeats rejected history.

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

| Candidate | JSON status | CSS L4 status | Sheets / BBNF-self status | Verdict |
|---|---|---|---|---|
| P2E-1 `StringPolicy` | Re-expresses current quote/backslash/control behavior exactly. | Needed for CSS quoted strings and escaped identifiers. | Sheets can set delimiter/escape equality; BBNF-self can set grammar string escapes. | Grammar-neutral if policy comes from generated config. |
| P2E-2 `StringSpecialBlock64` | Accelerates string spans and quote/escape/control scan. | Usable for CSS strings/idents only if policy supports CSS escapes. | General chunk-spanning token primitive. | Grammar-neutral Layer-0 if no JSON constants are baked into the body. |
| P2E-3 `EscapePolicy` | JSON unicode/simple escapes are one policy. | CSS hex/name escapes require a different policy and scalar oracle. | Sheets strings can use delimiter-as-escape policy; BBNF-self can use its grammar escape set. | Grammar-neutral only after the policy API exists; current `unescape_string` alone is JSON-shaped. |
| P2E-4 `NumberPolicy` | JSON strict policy is the regression oracle. | CSS leading-dot/sign/unit suffix policy is required for parity. | Sheets numeric literals and BBNF-self counts/weights can reuse policy. | Grammar-neutral if suffix/materialization policy is data, not grammar branch code. |
| P2E-5 `DigitRunMac4x` | Optional for dense JSON numeric spans. | Optional for CSS numbers/dimensions, likely sparse. | General digit-block primitive. | Grammar-neutral Layer-0; selectable only through cost/density facts. |
| P2E-6 `ByteSetRunSkip64` | Useful for delimiters/whitespace only with a named row consumer. | Directly applicable to CSS delimiters/layout skipping. | General byte-set run primitive. | Grammar-neutral; set bytes supplied by grammar config. |
| P2E-7 `RegexHirAnalysis` | Replaces hardcoded JSON regex facts. | Required for selector/value/comment recognizers. | General grammar regex facts. | Grammar-neutral analysis primitive. |
| P2E-8 `GrammarStructuralScan` | Can reproduce JSON structural scan. | Can scan CSS rule/declaration/selector boundaries if policy is supplied. | General scan primitive for token streams. | Grammar-neutral only if transient and single-substrate; retained sidecars violate Lock 1. |

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

- **REDRESS 28 + 33 / tiny-string NEON:** P2E-1/P2E-2 must not wire the rejected `match_tiny_plain_string` NEON class as a retained parse fix. REDRESS 72 explicitly admitted scalar cap widening because it did not wire the rejected NEON kernel (`skinny/REDRESS.md:1994-2004`); REDRESS 83 rejected the JSON-specific StringBlock16 tiny probe after every focused parse row missed (`skinny/REDRESS.md:2318-2345`).
- **REDRESS 50-55 / parser side tables, cursors, decoded-string sink hooks:** P2E-3 and P2E-8 must not introduce parse-time aux columns, parser-local event cursors, generic visitors, sink-local decoded stats, or quote-source streaming hashers. The redress record rejects aux side tables (`skinny/REDRESS.md:715-740`), cursor variants (`skinny/REDRESS.md:742-767`, `skinny/REDRESS.md:784-813`), and decoded-string sink attempts (`skinny/REDRESS.md:815-875`).
- **REDRESS 60-72 / direct materialization family:** string/unicode candidates must not reopen direct source-hook receiver shortcuts or semantic fact hashing under a new name; direct-string attempts failed without closing unicode rows (`skinny/REDRESS.md:1688-1732`, `skinny/REDRESS.md:1819-1835`, `skinny/REDRESS.md:1839-1886`).
- **REDRESS 80:** number work must not reopen `canada` mantissa-widen as a one-row local patch. P2E-4/P2E-5 need grammar-neutral number policy or digit-run facts with strict parity and same-row gates.
- **REDRESS 82:** P2E-3 must not repeat single-quartet unicode classification. The new route must be batched/policy-level with a row consumer, not one `\uXXXX` helper called per quartet (`skinny/REDRESS.md:2285-2316`).
- **REDRESS 84:** dispatch-envelope work must not pretend object-pair value-byte control compaction is enough; that route failed its focused gate and should not be smuggled through regex/dispatch extraction (`skinny/REDRESS.md:2360-2395`).
- **REDRESS 88/89/126:** P2E-8 may mention PMULL/CSSC/CTZ/bulk emit only as a material-differential union/scan route with checkasm and same-wave consumer. SK-V12 W4 demoted prefix-XOR, next-bit, bulk emit, byte context, and cache hints and routed ASCII-set production separately (`skinny/REDRESS.md:3766-3812`, `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:390-394`).
- **REDRESS 96/97/98:** structural scan wins do not authorize a retained union sidecar. Any union-adjacent use of P2E-8 must cite the historical regressions and name a material differential: compile-time per-rule shape, e-graph equivalence, or SIMD-first lane routing with scalar deletion only after parity (`skinny/REDRESS.md:2850-2906`, `skinny/REDRESS.md:2910-2949`).
- **REDRESS 119/120:** direct-row S-P1 signals are not admissions. JSON direct reopen waves must cite the prior fixpoint and provide strict same-harness comparator evidence (`restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:126-131`, `skinny/REDRESS.md:3539-3553`).

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

No external comparator or ISA source was newly consulted by P2-E; comparator and ISA teardown belongs to P2-A/P2-C. This artifact uses the checked-in S-P1 packet, source tree, locks, scoping audits, RESULTS, and REDRESS authority:

- `restart/prompts/skinny/PASS-2-RESEARCH.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md`.
- `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md`.
- `restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md`.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md`.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md`.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md`.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md`.
- `restart/skinny/tranches/sk-v13/HANDOFF.md`.
- `restart/locks/LOCKS.md`.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md`.
- `skinny/crates/parse-that-regex/src/lib.rs`.
- `skinny/crates/parse-that-regex/src/number/mod.rs`.
- `skinny/crates/parse-that-regex/src/integration/simd_scan_hook.rs`.
- `skinny/crates/bbnf-simd/src/lib.rs`.
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`.
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`.
- `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs`.
- `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs`.
- `skinny/crates/bbnf-simd/tests/checkasm_escape_mask_64.rs`.
- `skinny/crates/bbnf-simd/tests/checkasm_ascii_set_member_find_64.rs`.
- `skinny/crates/runtime/src/grammars/json/generated.rs`.
- `skinny/crates/runtime/src/grammars/json/scan.rs`.
- `skinny/crates/runtime/src/grammars/json/config.rs`.
- `skinny/crates/runtime/src/tape/mod.rs`.
