# SK-V12 P2-E: Parse-That Primitive Gaps

Pass: S-P2 Research. Cycle: V3.
Date: 2026-05-20.
Scope: parse-that and bbnf-simd primitive vocabulary gaps demanded by pin S-P1 hot leaves and the CSS L4 > lightningcss target.
Output: this file.
P1 hot-leaf antecedents: bounded_plain_string_scan; container_dispatch; unicode_escape_hex_decode; number_digit_span; simd_movemask; string_escape_decode; output_digest_hash; ascii_whitespace_skip; typed_direct_projection; serde_json_oracle_read_parse.
Lock surface: both: Lock 1 substrate union and Lock 14 grammar-neutrality; Lock 16 is binding for any SIMD/ASM candidate.

## §1 — Findings

S-P1 accepted ten hot families for S-P2: `bounded_plain_string_scan`, `container_dispatch`, `unicode_escape_hex_decode`, `number_digit_span`, `simd_movemask`, `string_escape_decode`, `output_digest_hash`, `ascii_whitespace_skip`, `typed_direct_projection`, and `serde_json_oracle_read_parse` (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:61-64`). The pin-era profile also states that CSS L4 has no generated Track 1 runtime, no same-plane lightningcss comparator, and no strict equality oracle (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:55-57`). P2-E therefore treats JSON hot leaves as primitive antecedents only; the same-wave consumer must be a CSS L4 generated parser/fact stream unless S-P3 records a measured CSS redress failure.

Current parse-that vocabulary is too JSON-shaped for that target. `parse-that-regex` exposes `skip_ascii_whitespace` but only for the four JSON whitespace bytes, with a private space-only SWAR helper (`skinny/crates/parse-that-regex/src/lib.rs:113-128`). String scanning is exposed as JSON-quote matching (`match_string_at_quote_trusted_utf8` / `match_string_at_quote`) and relies on private plain-span loops (`skinny/crates/parse-that-regex/src/lib.rs:162-234`, `:462`, `:547`). Unicode hex decode exists as JSON `\uXXXX` validation/materialization, with private scalar hex helpers (`skinny/crates/parse-that-regex/src/lib.rs:302-347`, `:945-959`). Number recognition is public only as a JSON number span matcher, while digit-run and fixed-width parse helpers are private (`skinny/crates/parse-that-regex/src/number/mod.rs:38-106`, `:207`).

`bbnf-simd` already has Layer-0 byte/mask substrate pieces, but most are not a CSS-ready Layer-1 vocabulary. It exposes byte-class table/eq-set masks, prefix-XOR, escape masks, and mask compaction (`skinny/crates/bbnf-simd/src/lib.rs:170-209`, `:235-271`). AArch64 string-special and unicode-hex helpers have scalar references (`skinny/crates/bbnf-simd/src/aarch64/string_block.rs:31-57`; `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40-125`). AArch64 digit MAC exists only as a 4-digit helper (`skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:5-27`). The SK-V12 audit names only three compliant Lock 16 primitives and five aarch64 orphans; the pin target requires zero orphan kernels at close (`restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md:117-193`; `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:73-78`).

CSS L4 introduces two gaps that the old JSON-only P2-E framing missed. First, CSS token classification needs grammar-derived byte sets for identifiers, strings, comments, separators, dimensions, percentages, hex colors, and at-rules (`grammar/css/l4/tokens.bbnf:7-9`; `grammar/css/l4/stylesheet.bbnf:12`, `:27`, `:44`; `grammar/css/l4/value-unit.bbnf:15-16`; `grammar/css/l4/color.bbnf:189-221`). Second, admission requires one canonical CSS fact stream shared by generated Track 1, independent oracle/Track 2, and lightningcss; JSON `JsonDirectDigest` is a benchmark-specific digest, not that stream (`skinny/crates/bbnf-bench/src/direct_struct.rs:16-48`; `skinny/crates/bbnf-bench/src/gate.rs:65`, `:164`, `:450`; `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:90-93`).

Layer split for this artifact:

| Layer | Meaning for P2-E |
|---|---|
| Layer 0 vendored substrate | Primitive byte/mask/SIMD cells in `bbnf-simd`: class masks, string-special blocks, digit MAC, hex nibble decode, movemask, prefix/escape masks, mask compaction. No grammar policy, no retained sidecar, scalar reference plus checkasm before routing. |
| Layer 1 bbnf primitive vocabulary | parse-that/runtime/codegen API shapes consumed by generated grammars: byte-set skip, token class span, string span, escape segment stream, digit span, fact-event emission, fact-stream digest. Grammar metadata supplies the policy. |

Non-candidates stay explicit. `serde_json_oracle_read_parse` is oracle evidence only. `simd_movemask` is Layer-0 support, not a parser API. `container_dispatch` becomes a grammar-derived token/FIRST-set classifier, not a retained structural projection. `typed_direct_projection` and `output_digest_hash` may only feed the grammar-neutral fact stream; they must not re-enter as JSON digest shortcuts.

## §2 — Candidate primitives

| Candidate | Class | Shape | Scalar-ref status | Checkasm/parity expectation | Same-wave consumer | Orphan / Lock 16 disposition | Micro-proof need |
|---|---|---|---|---|---|---|---|
| `pt_byte_set_run_skip` | Parser row-mover candidate | `fn skip_byte_set_run(input: &[u8], offset: usize, set: ByteSet) -> usize` returns first non-member offset. Comment/layout policy stays in generated grammar. | New scalar loop required; JSON `skip_ascii_whitespace` is a compatibility reference, not the generic API. | If SIMD-backed, strict `byte_class_from_eq_set_64` or `byte_class_from_table_64` parity over all generated sets, offsets, tails, and high-bit bytes. | CSS `@ws` skipper for whitespace/comment boundary, with JSON whitespace as guard. | Consumes class-mask helpers if used; otherwise touches no orphan. `escape_mask_64` N/A. | Empty/all-member spans, first/last miss, all offsets/tails, CSS comment boundary handoff, generated CSS equality. |
| `l0_byte_class_mask64` | Layer-0 support candidate | `fn byte_class_mask64(block: &[u8; 64], table_or_set: ClassSpec) -> u64` returns transient membership mask only. | Existing scalar refs cover table and <=8-byte eq-set masks (`skinny/crates/bbnf-simd/src/lib.rs:235-271`); CSS class specs need generated-table cases. | Required strict checkasm for TBL/eq-set bodies across density, alignment, tails, non-ASCII bytes, low-6 collisions, and every CSS delimiter set. | CSS delimiter/whitespace/identifier-start classifiers called by `pt_css_token_classify_span` and `pt_byte_set_run_skip` in the same wave. | Any new aarch64 body must be consumed same-wave or inventory-demoted; no retained position vector. | Generated `ClassSpec` corpus from CSS FIRST sets and caller equality. |
| `pt_css_token_classify_span` | Parser row-mover candidate | `fn classify_token_span(input, offset, token_table: TokenClassTable) -> TokenSpan { class, end, flags }`; flags are grammar-neutral facts such as escaped, non_ascii, numeric, or needs_host_decode. | New scalar DFA/reference required from grammar metadata. Existing JSON structural scan is not a CSS token oracle. | SIMD backing inherits checkasm for every Layer-0 mask cell it calls. | Generated CSS L4 lexer/front-end for `ident`, `selectorIdent`, strings, `@` rules, hex colors, dimensions, percentages, and keyframe stops. | Lock 16 applies through called SIMD cells; `escape_mask_64` only if string escape masks are fused. | Golden CSS token/fact stream against independent oracle/lightningcss adapter; fuzz mixed comments/strings/escapes; prove no JSON structural alphabet hardcode. |
| `pt_bounded_string_span` | Parser string candidate | `fn bounded_string_span(input, body_start, cap, policy: StringSpanPolicy) -> Result<StringSpan, ScanMiss>` returns closing delimiter/end and flags; delimiter/escape/control/UTF-8 policy is caller-owned. | New scalar public reference; existing generated JSON tiny loop and parse-that private plain loops are references (`skinny/crates/runtime/src/grammars/json/generated.rs:161-185`; `skinny/crates/parse-that-regex/src/lib.rs:462-547`). | AArch64 `scan_string_special_block` or widened bodies require 16/64-byte parity over all special positions, tails, caps, non-ASCII, and control limits. | CSS single/double quoted strings and selector escaped identifiers; JSON string rows as guard only. | SIMD route blocked by `escape_mask_64` if escape masks are consumed; any string_block/wide helper must be consumed or demoted. | Every delimiter/escape/control position under caps, non-ASCII before terminator, unterminated/tail cases, quote modes, caller-level CSS equality. |
| `pt_hex_unit_decode` | Parser escape subprimitive | `fn hex_unit_decode(input, offset, width: HexWidth) -> Result<HexUnit, HexError>` for fixed widths 3/4/6/8 and CSS variable-width policy supplied by caller. | Existing scalar `read_hex_unit_scalar` and `unescape_uxxxx_scalar` are references for width 4 (`skinny/crates/parse-that-regex/src/lib.rs:945-959`; `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40-47`). New scalar required for CSS hex widths. | x4 SIMD needs a scalar x4 oracle, invalid nibble in every lane/position, mixed valid/invalid quartets, alignment, boundaries, and surrogate handoff cases. | CSS hex colors `#RGB/#RRGGBB/#RGBA/#RRGGBBAA` and CSS/string escapes; JSON unicode escape guard. | Lock 16 applies to TBL bodies; `escape_mask_64` N/A unless fused into string-region handling. | Width 3/4/6/8 valid/invalid, mixed-case, short input, invalid offset, CSS termination policy; smoke-only tests are insufficient. |
| `pt_escaped_segments` | Parser escape candidate | `fn escaped_segments(input, body: Range<usize>, policy: EscapePolicy, visit: impl FnMut(EscapeSegment)) -> Result<()>`; emits raw spans and decoded scalar/byte units without allocating. | New scalar segment-stream oracle; `unescape_string` is semantic reference but materializes `String`/`Cow` (`skinny/crates/parse-that-regex/src/lib.rs:718-809`). | SIMD substeps require strict `checkasm_parity`/`checkasm_utf8_block`, x4 hex parity if used, and end-to-end segment parity. | CSS escaped strings/selectors and generated CSS fact stream; JSON unicode rows only guard. | SIMD admission blocked until `escape_mask_64` is fixed if quote/escape masks are consumed; no decoded scratch/stat/hash sidecar. | Segment parity against materialized reference, invalid escape offsets, surrogate/Unicode policy delegated to caller, CSS fact-stream equality. |
| `pt_digit_run_span_accumulate` | Parser numeric candidate | `fn digit_run_span_accumulate(input, offset, max_prefix_digits) -> DigitRun { end, digits, prefix_value, prefix_digits, truncated }`; sign/dot/exponent/unit policy stays outside. | New public scalar ref; private `scan_digit_run` and fixed-width parse helpers are references (`skinny/crates/parse-that-regex/src/number/mod.rs:106-223`). | UDOT needs x4/x16 parity, invalid-digit lanes, mixed valid/invalid groups, alignment, tails, truncation, and overflow checks. | CSS `<number>`, dimensions, percentages, keyframe stops; JSON numeric guard rows. | `digit_mac` is consumed only if same-wave caller lands; otherwise remains inventory, not admission. | Runs 0..128, block/tail offsets, truncation/overflow, leading-dot caller contexts, caller-level row movement. |
| `pt_number_parts` | Parser numeric template | `fn number_parts(input, offset, policy: NumberLexPolicy) -> Result<NumberPartsSpan, NumberError>` splits sign/int/fraction/exp/end while policy selects JSON/CSS/Sheets leading-dot and suffix rules. | New scalar ref over grammar policy; JSON `match_number_span_from_first` is one policy instance (`skinny/crates/parse-that-regex/src/number/mod.rs:38-90`). | N/A unless it consumes `pt_digit_run_span_accumulate`; inherited digit parity then applies. | CSS numeric values and units; Sheets numeric formulas; JSON guards. | Lock 16 N/A unless SIMD helper used; no f64 fallback or mantissa semantic change. | Equality to current JSON spans under JSON policy, CSS `.5` acceptance under CSS policy, suffix/unit boundary proof. |
| `pt_fact_event_emit` | Output-plane/fact-stream contract; parser-row-mover ineligible alone | `fn emit_fact_event(stream: &mut FactStream, class: u8, fact_id: FactId, span: Range<usize>, payload: FactPayload)` emits tape/fact events through grammar-derived ids. | New scalar oracle against generated event stream. Existing `EventGrammar`/`JsonFactId`/`SheetsFactId` witness the type shape but not CSS (`skinny/crates/runtime/src/tape/event_grammar.rs:4-24`; `skinny/crates/runtime/src/grammars/json/event_grammar_witness.rs:8-23`; `skinny/crates/runtime/src/grammars/sheets_witness/event_grammar_witness.rs:8-23`). | Checkasm N/A unless a concrete SIMD writer/hash body is separately proposed. | Canonical CSS L4 fact stream shared by generated Track 1, independent oracle/Track 2, and lightningcss adapter. | Cache-hint/hash support cannot remain orphaned; consume in fact writer or demote/remove. Lock 1 proof is mandatory. | Strict fact ordering and payload equality; generated CSS same-plane oracle; no grammar-name arms in generic crates. |
| `pt_fact_stream_digest` | Oracle/accounting; parser-candidate-ineligible | `fn digest_fact_stream(events: impl Iterator<Item = FactEvent>, config: DigestConfig) -> FactDigest` computes canonical output digest from fact events. | New scalar ref required. JSON `JsonDirectDigest` is an output-plane precedent, not reusable CSS policy (`skinny/crates/bbnf-bench/src/direct_struct.rs:16-48`, `:401-422`, `:717-734`). | Checkasm N/A unless a concrete SIMD hash is proposed; strict output equality/report gate is mandatory. | CSS fact-stream equality gate and lightningcss comparator; JSON direct digest rows are guard only. | Cache hints remain inventory-only unless a same-wave writer proves them; digest-only parser behavior is rejected. | Same event stream must digest identically across Track 1, oracle, and lightningcss adapter; reject missing comparator plane. |

## §3 — Grammar-neutrality

`pt_byte_set_run_skip` and `l0_byte_class_mask64` are grammar-neutral only when membership comes from grammar metadata. JSON whitespace, CSS comment-aware whitespace, Sheets formula whitespace, and BBNF trivia are different generated policies over the same byte-set/run primitive. CSS comments cannot be hidden inside a generic "whitespace" helper.

`pt_css_token_classify_span` is CSS-consuming but not CSS-overfit if the API is token-class table plus flags generated from Grammar IR. The same shape generalizes to Sheets formula tokens and BBNF-self tokens; the class table, allowed escapes, and fact ids are generated data, not generic crate branches.

`pt_bounded_string_span`, `pt_hex_unit_decode`, and `pt_escaped_segments` generalize to JSON strings, CSS strings/selectors/hex colors, Sheets doubled quotes, and BBNF literals/regex. The neutral primitive is span/escape/hex segmentation. JSON `\u` introducers, CSS variable-width escape termination, Sheets doubled-quote policy, and BBNF regex rules remain in generated caller policy.

`pt_digit_run_span_accumulate` and `pt_number_parts` generalize because digit runs and number parts are shared across JSON, CSS values, Sheets formulas, and BBNF literals. JSON leading-zero, CSS leading-dot, unit suffixes, percent tags, exponent policy, and materialized numeric type remain caller-owned.

`pt_fact_event_emit` and `pt_fact_stream_digest` are admissible only as grammar-derived fact-stream primitives. They are the CSS L4 output plane bridge demanded by the pin, not a JSON digest shortcut. Lock 1 holds because fact events must be the tape/projection or sink-only event stream; no parser-owned sidecar or retained class vector is allowed. Lock 14 holds only if CSS/Sheets/BBNF-self facts are generated from grammar metadata without `match grammar_name` arms in generic crates.

## §4 — Risks

Do not reopen substrate sidecars. Lock 1 says a SIMD mask stream is transient and retained structural projection is the tape (`restart/locks/LOCKS.md:52`). REDRESS 96/97/98 retire the measured union/class-column/streaming-cursor family (`skinny/REDRESS.md:2910-2919`). Any candidate that retains whitespace bitmaps, token class columns, structural cursor lists, or fact side vectors is rejected.

Do not repackage rejected string/digest routes. REDRESS 54/55/60-72 rejected decoded stats, quote-source materializers, parser-owned decoded scratch, and output-byte materialization families (`skinny/REDRESS.md:815-870`, `:1736-1886`). REDRESS 117/118 block decoded-source digest folds and output-digest host-sink shortcuts (`skinny/REDRESS.md:3443-3490`). The only legal digest route here is a new canonical CSS fact stream with independent same-plane comparator evidence.

Do not reopen numeric fallback work. REDRESS 80 rejects mantissa-widen/f64 fallback routes (`skinny/REDRESS.md:2217-2219`). Digit and number candidates may expose spans/parts; they may not alter materialization semantics without a separate same-wave oracle and guard evidence.

Do not promote proof-only SIMD. REDRESS 82/83 block single-quartet Unicode classifier and retained StringBlock16 proof-to-production reuse (`skinny/REDRESS.md:2287-2320`). REDRESS 88/89 block PMULL prefix-XOR and CSSC CTZ bulk consumers as default hot bodies (`skinny/REDRESS.md:2505-2538`). The user pin reopens ASM-gen categories only with scalar reference, checkasm, same-wave consumer, and material differential; it does not admit the historical implementations.

The `escape_mask_64` bug is a hard SIMD blocker. The user pin requires verifying and resolving the xorshift `0xCAFEF00DBAADF00D` falsifier before any new SIMD admission (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:97-105`). The checkasm report names the state-handoff failure between `escape_mask_64` and scan tail semantics (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:102-126`). P2-E candidates may use scalar references while that remains open, but SIMD-backed admission must wait.

Do not use JSON row movement as CSS proof. REDRESS 119/120 close JSON direct residuals as guard/fixpoint evidence, and CSS L4 is now the first target (`skinny/REDRESS.md:3497-3545`; `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-34`). A candidate can preserve JSON guards, but it cannot satisfy the lightningcss target without generated CSS Track 1, independent oracle/Track 2, same-plane lightningcss comparator, strict equality, and gate-consumed provenance.

## §5 — Sources

Local sources only:

- `restart/prompts/skinny/PASS-2-RESEARCH.md`
- `restart/prompts/ORCHESTRATOR.md`
- `restart/locks/LOCKS.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`
- `restart/skinny/tranches/sk-v12/research/skv12-totality-fold-scout.md`
- `restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/parse-that-regex/src/number/mod.rs`
- `skinny/crates/bbnf-simd/src/lib.rs`
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`
- `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs`
- `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`
- `skinny/crates/runtime/src/tape/event_grammar.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/runtime/src/grammars/json/scan.rs`
- `skinny/crates/runtime/src/grammars/json/event_grammar_witness.rs`
- `skinny/crates/runtime/src/grammars/sheets_witness/event_grammar_witness.rs`
- `skinny/crates/codegen/src/json_provider.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/gate.rs`
- `grammar/css/l4/tokens.bbnf`
- `grammar/css/l4/stylesheet.bbnf`
- `grammar/css/l4/value-unit.bbnf`
- `grammar/css/l4/color.bbnf`
- `grammar/google-sheets/google-sheets.bbnf`
- `grammar/bbnf/bbnf.bbnf`
