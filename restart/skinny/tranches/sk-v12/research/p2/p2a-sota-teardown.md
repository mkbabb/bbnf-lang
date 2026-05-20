# SK-V12 P2-A: SOTA Comparator Teardown

Pass: S-P2 Research. Cycle: V12.
Date: 2026-05-20.
Scope: architecture teardown of asmjson, sonic-rs, simdjson, and yyjson keyed to accepted SK-V12 P1 hot leaves.
Output: this file.
P1 hot-leaf antecedents: `bounded_plain_string_scan`, `container_dispatch`, `unicode_escape_hex_decode`, `number_digit_span`, `simd_movemask`, `string_escape_decode`, `output_digest_hash`, `ascii_whitespace_skip`, `typed_direct_projection`, `serde_json_oracle_read_parse`.
Lock surface: Lock 1 and Lock 14 are both in force; Lock 16 governs SIMD/ASM admission; Lock 8 supplies the SOTA comparator lane.

## §1 — Findings

SK-V12 P1 accepted the current hot-leaf family set as `bounded_plain_string_scan`, `container_dispatch`, `unicode_escape_hex_decode`, `number_digit_span`, `simd_movemask`, `string_escape_decode`, `output_digest_hash`, `ascii_whitespace_skip`, `typed_direct_projection`, and `serde_json_oracle_read_parse` (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:47`). The row surface remains JSON-heavy: parse rows are all S/NO-GO or L/NO-GO, direct rows contain only four A/GO guards and thirteen direct residual NO-GO rows, typed rows contain seven A/GO guards, and no generated non-JSON baseline exists yet (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:77`). The non-JSON blocker is concrete: codegen still gates generated runtime emission through the JSON provider, and the runtime grammar tree does not contain generated `css_l4`, `css_l4_declaration_values`, `sheets`, or `bbnf_self` modules (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:299`; `skinny/crates/codegen/src/json_provider.rs:4`; `skinny/crates/codegen/src/lib.rs:108`; `skinny/crates/runtime/src/grammars/json/generated.rs:1`).

Strict-vs-strict discipline materially changes the comparator read. asmjson is useful as an architecture probe but is not a strict JSON comparator anchor as published, because its own README says it treats any byte below `0x20` as whitespace and does not scan inside strings for unescaped control bytes [A3]. sonic-rs strict APIs remain admissible only on UTF-8-checking paths; its unchecked/lossy paths are flaw probes, not strict anchors [S5] [S6]. yyjson default `YYJSON_READ_NOFLAG` is the strict lane; its permissive flags for comments, trailing commas, invalid Unicode, extended whitespace, and JSON5-like behavior are excluded from strict comparison [Y1] [Y2]. simdjson's stage-1 structural index and On Demand plane are strict-comparator material only when their retained sidecar/tape design is treated as what the comparator does, not as something bbnf may import wholesale under Lock 1 [J1] [J6] [J8].

| Comparator | Strict comparator plane | Structural classification strategy | String fast path | Number fast path | Output plane | What comparator does that bbnf does not |
| --- | --- | --- | --- | --- | --- | --- |
| asmjson | Published parser is not strict enough for the strict anchor lane because of whitespace/control-character caveats; use as an architecture/flaw probe only [A3]. | Classifies 64-byte blocks with handwritten AVX-512BW or portable SWAR, producing byte-state masks for whitespace, string state, backslashes, quotes, and delimiters [A1] [A7]. | Skips string bodies through quote/backslash masks, but the published caveat says it does not check unescaped control bytes inside strings [A3] [A7]. | SAX events expose numbers as source slices; DOM/event paths are assembly/SWAR-driven rather than serde-shaped typed materialization [A4] [A8]. | Provides zero-tape SAX callbacks and a flat DOM `DomEntry` output, including handwritten assembly entry points that write events/DOM directly [A2] [A4] [A6] [A8]. | A 64-byte class-mask parser loop and direct assembly-to-SAX/DOM output writer. bbnf currently has scalar/SWAR string and whitespace helpers plus generated tape/direct walks, not a 64-byte transient classifier or assembly sink (`skinny/crates/parse-that-regex/src/lib.rs:113`; `skinny/crates/parse-that-regex/src/lib.rs:547`; `skinny/crates/runtime/src/grammars/json/generated.rs:292`; `skinny/crates/runtime/src/grammars/json/generated.rs:427`). |
| sonic-rs | Strict `from_slice`/serde paths perform UTF-8 checking; lossy and unchecked APIs are excluded from strict comparisons [S4] [S5] [S6]. | Uses SIMD for long strings, fractional floats, field access, and whitespace; object/container skipping computes string masks with prefix-xor state and structural masks [S1] [S2]. | Uses zero-copy borrowed string visiting when possible and a scalar 24-byte key path that bails on escape/control/no-quote cases [S7] [S8]. | Supports raw number visitation and typed numeric visitors; `sonic-number` contains AArch64 digit-pack/accumulate routines [S9] [S12]. | The serde path can parse directly into Rust structs without first building a temporary DOM/tape; LazyValue can retain raw input slices [S4] [S10]. | A strict typed-to-struct product path with no temporary JSON data structure, a 24-byte object-key probe, a local two-byte separator probe, and AArch64 digit packing. bbnf has typed/direct guards and an independent Track 2 hand parser, but no generated non-JSON direct/typed runtime product yet (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:205`; `skinny/RESULTS.md:143`; `skinny/RESULTS.md:144`). |
| simdjson | Strict lane is its normal DOM/On Demand parser with stage 1 validation and stage 2 materialization; it validates UTF-8/control constraints while indexing [J5] [J6]. | Splits parsing into stage 1 structural/scalar/string scanning and stage 2 tape building; stage 1 writes a retained structural index [J3] [J4] [J5] [J6]. | Finds quote, backslash, escaped, and in-string masks with prefix-xor state over blocks [J4]. | Stage 2 visits number tokens and writes typed values into its tape builder [J8]. | DOM tape is an array of 64-bit values plus string storage and skip/count metadata; On Demand iterates from the structural index without building the full DOM first [J1] [J2] [J7] [J8]. | A retained structural index plus an On Demand cursor over that index. bbnf's Lock 1 permits the tape substrate and transient SIMD masks, but not a second retained structural substrate; any analogous primitive must feed an existing tape/direct/typed consumer in the same loop (`restart/locks/LOCKS.md:52`; `skinny/crates/runtime/src/grammars/json/generated.rs:10`). |
| yyjson | `YYJSON_READ_NOFLAG` is strict RFC 8259 mode; permissive read flags are separate and excluded [Y1] [Y2]. | Uses a scalar table/FSM/goto reader with character-class tables and optional trivia skipping by flags [Y5] [Y9]. | Uses unrolled ASCII skip/copy loops, UTF-8 checks, escape decoding, and a Unicode escape reader [Y8] [Y10]. | Uses an unrolled scalar number reader and also exposes `yyjson_read_number` [Y4] [Y7]. | Builds a mutable document/DOM with 16-byte `yyjson_val` cells and documented memory growth behavior; non-insitu reading copies input and pads it [Y3] [Y6] [Y11]. | A tightly inlined scalar FSM and compact value-pool DOM. bbnf's generated JSON parser currently dispatches through generated recursive value/container functions and writes offsets/tape/direct products rather than a yyjson-style universal value pool (`skinny/crates/runtime/src/grammars/json/generated.rs:37`; `skinny/crates/runtime/src/grammars/json/generated.rs:310`; `skinny/crates/runtime/src/grammars/json/generated.rs:468`). |

The comparator commonality is not "SIMD everywhere." The repeated architecture pattern is a small set of byte-class, bounded-string, number-span, separator, and output-plane decisions that must be consumed immediately by a grammar-generated parser/product path. simdjson and asmjson show the upside of wide classification; sonic-rs shows typed direct projection and small structural probes; yyjson shows how much scalar inlining and memory layout can matter. The bbnf-specific implication is narrower than copying any one parser: each candidate must stay grammar-neutral, preserve Lock 1's single substrate, preserve Lock 14's generic-crate boundary, and carry a scalar reference plus a same-loop consumer if SIMD/ASM is involved.

## §2 — Candidate primitives

These are candidate primitive implications, not selected waves or an implementation order. V1 CHALLENGE folded the candidate-accounting triad here so S-P3 cannot infer a selectable primitive from comparator prose alone.

| Candidate | Scalar-reference status | Checkasm/parity expectation | Same-wave consumer note |
| --- | --- | --- | --- |
| C1 `class_mask64_transient` | Needs a scalar table/byte-set mask reference over 0..64 live bytes; existing `byte_class_from_eq_set_64_scalar` and `byte_class_from_table_64_scalar` are partial executable references for the Layer-0 forms. | Required for any SIMD body: strict scalar/corpus/checkasm parity over alignment, tail, high-bit, duplicate-set, and nonmember cases. | Must be consumed immediately by generated whitespace/layout, delimiter, string-interesting, or FIRST-set dispatch in the same wave. A retained class stream is rejected. |
| C2 `bounded_special_byte_string_end` | Needs a scalar bounded scan reference parameterized by terminator, escape, control floor, non-ASCII policy, and cap. Existing JSON and parse-that string helpers are demand evidence, not the generic oracle. | Required only if SWAR/NEON support is routed; otherwise scalar tests must cover every cap, terminator, escape, control, non-ASCII, and tail position. | Must replace a generated string/key/literal scan in the same wave, preferably after generated non-JSON baseline authority exists. A JSON-only direct residual retry is pre-blocked. |
| C3 `escape_segment_hex_run_decode` | Needs a scalar segment walker plus fixed-width hex-unit reference; existing parse-that JSON unescape paths are partial local evidence but not the grammar-neutral oracle. | Required for any batch hex/SIMD subprimitive, including invalid-lane and first-failing-offset parity. | Must feed a generated escape/materialization consumer in the same wave without retaining decoded-byte sidecars or decoded-string stats. |
| C4 `digit_run_accumulate` | Needs a scalar digit-run span/accumulate reference below grammar number policy. Existing parse-that number helpers provide local anchors but JSON number matching is not the generic oracle. | Required for UDOT or other native digit helpers; scalar tests still cover zero-length, boundaries, truncation, overflow, and non-digit stops. | Must be consumed by a generated number/literal parser in the same wave. It cannot reuse the rejected JSON numeric-slot/direct residual route. |
| C5 `separator_pair_probe_direct` | Scalar reference is generated FIRST/follow lookahead over one or two bytes, with grammar-owned separator policy. | Checkasm is N/A unless the probe is lowered through a SIMD byte-class helper; generated scalar branch parity and oracle equality are still required. | Must be consumed by generated direct/typed dispatch in the same wave and may not become object-carry state, hidden side table, or JSON container-tail retry. |
| C6 `output_plane_event_sink_contract` | Scalar reference is the generated output contract and independent oracle for the selected product. Existing digest/hash code is evidence only, not a parser primitive. | Checkasm is N/A unless a concrete SIMD hash/body is proposed; output equality/oracle parity is mandatory. | Must be row-owned output-plane work in the same wave. Digest/hash remains oracle-only unless a legal product consumes it. |
| C7 `generated_first_set_dispatch_template` | Scalar reference is generated FIRST/prefix/lookahead dispatch for the selected grammar, with branch arms and tables emitted from grammar metadata. | Checkasm is N/A unless it consumes a SIMD classifier; generated scalar dispatch parity against the grammar oracle is required. | Must be consumed by a generated parser/runtime path in the same wave. It cannot add directives, BIR variants, or grammar policy to generic crates. |

Scalar sketch floor: C1 is a byte loop that ORs bit `i` when
`class_table[input[offset + i]]` or a generated byte set matches; C2 is a
bounded byte loop returning the first terminator and rejecting escape/control
sentinels per caller policy; C3 walks raw spans and calls a scalar hex-unit
decoder before invoking a caller-owned segment sink; C4 scans ASCII digits and
accumulates only the bounded prefix requested by generated code; C5 evaluates
the generated one- or two-byte FIRST/follow branch table; C6 compares the
generated output product against its independent oracle; C7 dispatches through
generated FIRST/prefix tables without grammar-name branches. Any S-P3 shortlist
must replace these sketches with an executable scalar reference before SIMD or
native wiring.

### C1. `class_mask64_transient`

Shape: classify up to 64 bytes into transient masks for caller-supplied byte classes such as whitespace, quote, escape, structural delimiter, operator, scalar start, and grammar-specific FIRST-set bytes. The output is a short-lived mask bundle consumed by the current parse loop; it is not a retained sidecar.

P1 antecedents: `ascii_whitespace_skip`, `container_dispatch`, `simd_movemask`, `bounded_plain_string_scan`.

Comparator antecedents: asmjson's 64-byte SWAR/AVX-512 byte-state classifier [A1] [A7], simdjson's stage-1 scanner/indexer [J3] [J5] [J6], and sonic-rs' string/whitespace masks [S1] [S2].

bbnf delta: current bbnf has scalar/SWAR whitespace and string scanners plus AArch64 movemask support (`skinny/crates/parse-that-regex/src/lib.rs:113`; `skinny/crates/parse-that-regex/src/lib.rs:547`; `skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4`), but no grammar-parameterized 64-byte class-mask primitive consumed by generated parser code.

Admission boundaries: scalar reference first; optional SIMD implementation must pass Lock 16 scalar/corpus/checkasm parity. The mask may not become a retained structural-index substrate under Lock 1.

### C2. `bounded_special_byte_string_end`

Shape: given a start pointer and a maximum span, return the first special byte in a string-like token: terminator, escape marker, control byte, non-ASCII byte, or grammar-supplied sentinel. The caller owns grammar policy for whether the stop byte ends a string, starts an escape, rejects input, or falls through to a slower decoder.

P1 antecedents: `bounded_plain_string_scan`, `string_escape_decode`, `simd_movemask`.

Comparator antecedents: sonic-rs' zero-copy string visitor and 24-byte key probe [S7] [S8], yyjson's unrolled ASCII string skip/copy [Y10], simdjson's quote/escape string scanner [J4], and asmjson's string-body skip probe [A7].

bbnf delta: current JSON generation has tiny string matching and parse-that has string skipping/unescaping helpers (`skinny/crates/runtime/src/grammars/json/generated.rs:171`; `skinny/crates/parse-that-regex/src/lib.rs:162`; `skinny/crates/parse-that-regex/src/lib.rs:547`; `skinny/crates/parse-that-regex/src/lib.rs:718`), but the helper set is not yet a grammar-parameterized primitive available to generated non-JSON runtimes.

Admission boundaries: strict control-byte behavior must be explicit. asmjson's published caveat is a negative example, not a license to skip strict validation [A3].

### C3. `escape_segment_hex_run_decode`

Shape: split escaped string materialization into raw segment accounting plus fixed-width escape decoding, including a scalar four-hex-unit path and caller-owned surrogate/Unicode policy. The primitive reports decoded units and segment boundaries; it does not force a JSON-only decoded-string sink.

P1 antecedents: `string_escape_decode`, `unicode_escape_hex_decode`.

Comparator antecedents: yyjson's Unicode escape reader and escape-aware string reader [Y8] [Y10], simdjson's escape mask scanner [J4], and sonic-rs' strict versus lossy UTF-8 distinction [S3] [S5].

bbnf delta: parse-that has scalar Unicode escape validation/decoding and unescape paths (`skinny/crates/parse-that-regex/src/lib.rs:284`; `skinny/crates/parse-that-regex/src/lib.rs:302`; `skinny/crates/parse-that-regex/src/lib.rs:718`; `skinny/crates/parse-that-regex/src/lib.rs:945`), but REDRESS has rejected decoded-string stats sinks and eager decoded-byte folds as row-moving routes (`skinny/REDRESS.md:815`; `skinny/REDRESS.md:3436`).

Admission boundaries: strict scalar parity is mandatory. A SIMD or batch hex proof is support-only until it has an in-wave parser/product consumer and does not reopen the rejected decoded-string sink.

### C4. `digit_run_accumulate`

Shape: scan a grammar-owned digit run and optionally accumulate a bounded prefix into integer lanes while returning the end offset, digit count, overflow/truncation status, and decimal/exponent boundary metadata. JSON number policy, CSS unit policy, and formula numeric policy remain in generated callers.

P1 antecedents: `number_digit_span`.

Comparator antecedents: sonic-rs' raw/typed numeric visitor and AArch64 digit-pack routines [S9] [S12], plus yyjson's unrolled scalar number reader and standalone number reader [Y4] [Y7].

bbnf delta: parse-that already has number-span, digit-run, eight-digit parse, and materialization helpers (`skinny/crates/parse-that-regex/src/number/mod.rs:38`; `skinny/crates/parse-that-regex/src/number/mod.rs:106`; `skinny/crates/parse-that-regex/src/number/mod.rs:214`; `skinny/crates/parse-that-regex/src/number/mod.rs:247`). The missing comparator-shaped primitive is a grammar-neutral digit-run accumulator that generated non-JSON code can call without routing through JSON provider policy.

Admission boundaries: REDRESS 114 blocks treating prior numeric direct closure as established row movement (`skinny/REDRESS.md:3359`). This candidate must be re-proven against fresh generated non-JSON consumers if pursued later.

### C5. `separator_pair_probe_direct`

Shape: a local one- or two-byte lookahead probe for high-frequency separators and close delimiters, generated from grammar FIRST/follow facts. It returns a small branch code to the current direct/typed parser and does not retain object state or compact value bytes.

P1 antecedents: `container_dispatch`, `typed_direct_projection`.

Comparator antecedents: sonic-rs' object loop checks `,"` and `}` using a two-byte peek [S11], and yyjson's FSM reader uses tight local delimiter dispatch [Y9].

bbnf delta: the generated JSON parser has scalar container dispatch and direct object/array product paths (`skinny/crates/runtime/src/grammars/json/generated.rs:310`; `skinny/crates/runtime/src/grammars/json/generated.rs:348`; `skinny/crates/runtime/src/grammars/json/generated.rs:468`; `skinny/crates/runtime/src/grammars/json/generated.rs:508`), but no generic generated separator-pair primitive shared by CSS, Sheets, and BBNF-self outputs.

Admission boundaries: REDRESS 115 blocks the SK-V11 container-tail route as row-moving evidence (`skinny/REDRESS.md:3385`). This candidate cannot become an object-carry cursor, hidden side table, or JSON-only direct residual patch.

### C6. `output_plane_event_sink_contract`

Shape: a grammar-generated event/sink contract that can consume parse events into direct structs, typed projections, benchmark digest/hash products, or host-facing output without first requiring a DOM. The contract is an output-plane primitive, not a generic parser substrate.

P1 antecedents: `typed_direct_projection`, `output_digest_hash`, `serde_json_oracle_read_parse`, `container_dispatch`.

Comparator antecedents: asmjson's SAX sink and direct DOM assembly entry points [A4] [A6] [A8], sonic-rs' direct serde struct parse and LazyValue raw-slice plane [S4] [S10], and simdjson's DOM/On Demand output split [J1] [J8].

bbnf delta: current RESULTS distinguish Track 1 generated parser work from Track 2 independent hand-coded parser evidence, and warn that historical C++ sidecars are not strict anchors (`skinny/RESULTS.md:143`; `skinny/RESULTS.md:146`). bbnf has JSON direct digest/hash helpers (`skinny/crates/bbnf-bench/src/direct_struct.rs:123`; `skinny/crates/bbnf-bench/src/direct_struct.rs:717`), but no grammar-neutral generated output-plane contract for non-JSON baselines.

Admission boundaries: REDRESS 118 blocks output digest/hash as an unowned host sink without a legal residual row, consumer, and oracle (`skinny/REDRESS.md:3464`). This candidate can only count where the output product is part of the accepted measured row.

### C7. `generated_first_set_dispatch_template`

Shape: a generated scalar dispatch template over grammar FIRST/follow sets and local byte classes. It is the grammar-neutral version of yyjson's hand-tuned scalar FSM and sonic/asmjson local delimiter checks: the generic crate supplies templates and helpers; generated grammar modules supply the tables and branch arms.

P1 antecedents: `container_dispatch`, `ascii_whitespace_skip`, `typed_direct_projection`, `bounded_plain_string_scan`.

Comparator antecedents: yyjson's scalar FSM/goto reader [Y9], sonic-rs' object/string local probes [S8] [S11], and asmjson's class-driven state loop [A7].

bbnf delta: current generated JSON code hardcodes JSON value/container dispatch (`skinny/crates/runtime/src/grammars/json/generated.rs:37`; `skinny/crates/runtime/src/grammars/json/generated.rs:427`), while non-JSON generated runtime emission is blocked by the JSON provider gate (`skinny/crates/codegen/src/json_provider.rs:4`; `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:305`).

Admission boundaries: this may not add a new directive, BIR variant, or generic-crate grammar policy. Lock 14 requires grammar-specific behavior to live in generated per-grammar modules (`restart/locks/LOCKS.md:78`).

Support-only implications not counted as candidate primitives here: `movemask_pack64`, `quote_escape_state64`, scalar `hex4` batch proofs, and strict comparator manifests. They may be evidence scaffolding for C1-C4, but they are not independent row-moving candidates without a same-loop consumer.

## §3 — Grammar-neutrality

The comparator candidates must survive the generated non-JSON baseline requirement. CSS L4 declaration values, Google Sheets formulas, and BBNF-self are enough to expose JSON-only assumptions before S-P3. The relevant grammar facts are already present: CSS values include dimensions, numbers, functions, strings, URLs, colors, and raw identifiers (`grammar/css/l4/values.bbnf:29`; `grammar/css/l4/values.bbnf:39`; `grammar/css/l4/values.bbnf:52`; `grammar/css/l4/values.bbnf:61`; `grammar/css/l4/values.bbnf:84`); Sheets includes numeric literals, quoted strings, operators, function calls, arrays, and formula roots (`grammar/google-sheets/google-sheets.bbnf:6`; `grammar/google-sheets/google-sheets.bbnf:12`; `grammar/google-sheets/google-sheets.bbnf:97`; `grammar/google-sheets/google-sheets.bbnf:141`; `grammar/google-sheets/google-sheets.bbnf:159`); BBNF-self includes identifiers, literals, regexes, comments, alternation/concatenation, directives, and value expressions (`grammar/bbnf/bbnf.bbnf:9`; `grammar/bbnf/bbnf.bbnf:29`; `grammar/bbnf/bbnf.bbnf:41`; `grammar/bbnf/bbnf.bbnf:70`; `grammar/bbnf/expressions.bbnf:6`; `grammar/bbnf/expressions.bbnf:20`).

| Candidate | Grammar-neutrality check |
| --- | --- |
| C1 `class_mask64_transient` | Passes only if byte classes are supplied by generated grammar tables and the mask is consumed immediately. CSS operators/functions, Sheets operators/strings, and BBNF identifiers/literals all need different FIRST/follow sets. |
| C2 `bounded_special_byte_string_end` | Passes only if delimiter, escape, control, quote, raw-URL, regex, and doubled-quote policy is caller-owned. JSON string semantics cannot be baked into the helper. |
| C3 `escape_segment_hex_run_decode` | Passes as a low-level escape/hex decoder, not as a JSON string materializer. CSS escapes, BBNF literals/regexes, and Sheets doubled quotes require different policy at the caller. |
| C4 `digit_run_accumulate` | Passes as digit scanning/accumulation. CSS dimensions/percentages, Sheets numeric formulas, BBNF expression numbers, and JSON numbers differ above the digit-run layer. |
| C5 `separator_pair_probe_direct` | Passes only when generated from grammar FIRST/follow facts. CSS comma/function and Sheets argument/array separators are not JSON object separators. |
| C6 `output_plane_event_sink_contract` | Passes only if output products are grammar-generated and row-owned. It cannot be a JSON direct residual shortcut or a hidden host sink. |
| C7 `generated_first_set_dispatch_template` | Passes only if generic crates expose templates and primitives while per-grammar generated modules own branch arms and tables. This is the Lock 14 boundary. |

## §4 — Risks

Parse-only evidence is diagnostic, not a row mover. SK-V12 P1 found Mode III structural-scan absence and parse-only shape information, but P1 already pre-blocked parse-only/Mode III work as insufficient for accepted row movement (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:229`; `skinny/REDRESS.md:3040`).

Retained sidecars are the main simdjson hazard. simdjson's retained structural index is useful comparator evidence, but bbnf cannot add a parallel retained structural substrate under Lock 1 (`restart/locks/LOCKS.md:52`). Prior parse-time aux side tables, event cursors, structural-mask cursors, and W3 union/streaming cursor routes were already rejected (`skinny/REDRESS.md:715`; `skinny/REDRESS.md:742`; `skinny/REDRESS.md:784`; `skinny/REDRESS.md:2850`; `skinny/REDRESS.md:2910`).

String and escape work is high-risk because many nearby routes have already been closed. Decoded-string stats sinks, quote-source fused materializers, wide scans without legal consumers, and eager decoded-byte folds are rejected or blocked (`skinny/REDRESS.md:815`; `skinny/REDRESS.md:846`; `skinny/REDRESS.md:1380`; `skinny/REDRESS.md:1439`; `skinny/REDRESS.md:3150`; `skinny/REDRESS.md:3436`). C2/C3 are candidate primitive shapes only because they keep strict validation and defer grammar policy to generated callers.

Number and container candidates must not launder SK-V11 residual fixes back into SK-V12. REDRESS 114 rejected numeric direct closure as measured row movement; REDRESS 115 rejected the prior container-tail direct dispatch route (`skinny/REDRESS.md:3359`; `skinny/REDRESS.md:3385`). C4/C5 require fresh non-JSON generated consumers before they could matter later.

Output-plane evidence is tempting but dangerous. `output_digest_hash` is an accepted hot family, but REDRESS 118 blocks a host-sink/hash route without a legal row, consumer, and oracle (`skinny/REDRESS.md:3464`). C6 must be treated as an output-plane contract candidate, not a parser primitive and not a generic benchmark shortcut.

The generated non-JSON baseline remains the gating generality risk. REDRESS 111-113 rejected W1a/W1b/W2 movement because the non-JSON generated baseline was not actually emitted and the codegen path still routed through JSON provider policy (`skinny/REDRESS.md:3282`; `skinny/REDRESS.md:3313`; `skinny/REDRESS.md:3342`). Any candidate that cannot be expressed through generated CSS L4 declaration values, Sheets, or BBNF-self remains JSON-only research.

## §5 — Sources

Local SK-V12 and governance sources:

- `restart/prompts/skinny/PASS-2-RESEARCH.md:31` for P2-A scope and strict-vs-strict requirements.
- `restart/prompts/ORCHESTRATOR.md:104`, `restart/prompts/ORCHESTRATOR.md:118`, `restart/prompts/ORCHESTRATOR.md:197`, and `restart/prompts/ORCHESTRATOR.md:205` for convergence constraints, scalar references, same-wave consumers, and Lock 1/14 boundaries.
- `restart/skinny/tranches/sk-v12/HANDOFF.md:23` for SK-V12 next-step/refusal state.
- `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:122` for primitive source map.
- `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:221` for accepted product hot families.
- `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:229` for Mode III/structural absence pre-blocks.
- `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:99` for PMU cycle-per-byte context.
- `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:205` for canonical primitive loci and `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:299` for generated non-JSON blocker.
- `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:77` for current results classification.
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:47` for accepted hot families.
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:45` for replay capture scope and `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:145` for derived self-time tables.
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv:1` for replay command log.
- `skinny/RESULTS.md:143` for Track 1/Track 2 status and `skinny/RESULTS.md:146` for strict-anchor warning on historical C++ sidecars.
- `skinny/REDRESS.md:3282`, `skinny/REDRESS.md:3313`, `skinny/REDRESS.md:3342`, `skinny/REDRESS.md:3359`, `skinny/REDRESS.md:3385`, `skinny/REDRESS.md:3413`, `skinny/REDRESS.md:3436`, `skinny/REDRESS.md:3464`, and `skinny/REDRESS.md:3497` for SK-V11/SK-V12 pre-blocks.
- `restart/locks/LOCKS.md:52`, `restart/locks/LOCKS.md:78`, and `restart/locks/LOCKS.md:87` for Lock 1, Lock 14, and Lock 16.
- `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:36` and `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:23` as historical P2 context only.

Primary comparator sources:

- [A1] asmjson README performance/classification overview: `https://github.com/atomicincrement/asmjson/blob/3d6965d5a013677198366758cb50fb8637d54d58/README.md#L7-L12`.
- [A2] asmjson README parser architecture and output formats: `https://github.com/atomicincrement/asmjson/blob/3d6965d5a013677198366758cb50fb8637d54d58/README.md#L100-L113`.
- [A3] asmjson README conformance caveats: `https://github.com/atomicincrement/asmjson/blob/3d6965d5a013677198366758cb50fb8637d54d58/README.md#L209-L222`.
- [A4] asmjson assembly entry points: `https://github.com/atomicincrement/asmjson/blob/3d6965d5a013677198366758cb50fb8637d54d58/src/lib.rs#L190-L223`.
- [A5] asmjson portable DOM parser: `https://github.com/atomicincrement/asmjson/blob/3d6965d5a013677198366758cb50fb8637d54d58/src/lib.rs#L395-L416`.
- [A6] asmjson SAX parser entry points: `https://github.com/atomicincrement/asmjson/blob/3d6965d5a013677198366758cb50fb8637d54d58/src/lib.rs#L613-L655`.
- [A7] asmjson SWAR parser loop and byte-state classifier: `https://github.com/atomicincrement/asmjson/blob/3d6965d5a013677198366758cb50fb8637d54d58/src/lib.rs#L657-L760` and `https://github.com/atomicincrement/asmjson/blob/3d6965d5a013677198366758cb50fb8637d54d58/src/lib.rs#L1182-L1278`.
- [A8] asmjson SAX sink trait: `https://github.com/atomicincrement/asmjson/blob/3d6965d5a013677198366758cb50fb8637d54d58/src/sax.rs#L5-L47`.

- [S1] sonic-rs README SIMD feature bullets: `https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/README.md#L60-L66`.
- [S2] sonic-rs parser whitespace/string/container masks: `https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/src/parser.rs#L145-L200`.
- [S3] sonic-rs parser UTF-8 lossy mode: `https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/src/parser.rs#L251-L255`.
- [S4] sonic-rs README direct serde struct benchmark: `https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/README.md#L78-L90`.
- [S5] sonic-rs README UTF-8 validation note: `https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/README.md#L453-L456`.
- [S6] sonic-rs serde `from_slice` UTF-8 checks and unchecked contract: `https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/src/serde/de.rs#L1306-L1334`.
- [S7] sonic-rs parse string visitor: `https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/src/parser.rs#L336-L359`.
- [S8] sonic-rs 24-byte key fast path: `https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/src/parser.rs#L361-L394`.
- [S9] sonic-rs raw/typed number visitor: `https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/src/parser.rs#L396-L425`.
- [S10] sonic-rs LazyValue raw-slice path: `https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/src/parser.rs#L654-L685`.
- [S11] sonic-rs object separator probe: `https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/src/parser.rs#L500-L527`.
- [S12] sonic-rs AArch64 number routines: `https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/sonic-number/src/arch/aarch64.rs#L1-L137`.

- [J1] simdjson tape overview: `https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/doc/tape.md#L2-L6`.
- [J2] simdjson tape element/object/string representation: `https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/doc/tape.md#L68-L144`.
- [J3] simdjson haswell stage 1/stage 2 entry points: `https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/src/haswell.cpp#L132-L163`.
- [J4] simdjson string scanner masks: `https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/src/generic/stage1/json_string_scanner.h#L14-L91`.
- [J5] simdjson scanner structural/scalar definitions: `https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/src/generic/stage1/json_scanner.h#L15-L31` and `https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/src/generic/stage1/json_scanner.h#L93-L160`.
- [J6] simdjson structural indexer and validation: `https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/src/generic/stage1/json_structural_indexer.h#L24-L123`, `https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/src/generic/stage1/json_structural_indexer.h#L193-L247`, and `https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/src/generic/stage1/json_structural_indexer.h#L249-L290`.
- [J7] simdjson On Demand parser stage 1 handoff: `https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/include/simdjson/generic/ondemand/parser-inl.h#L54-L68`.
- [J8] simdjson tape builder visit methods: `https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/src/generic/stage2/tape_builder.h#L21-L115`.

- [Y1] yyjson strict default flags: `https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.h#L736-L744`.
- [Y2] yyjson permissive read flags: `https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.h#L759-L834`.
- [Y3] yyjson read options and document API: `https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.h#L920-L1003`.
- [Y4] yyjson standalone number reader: `https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.h#L1124-L1149`.
- [Y5] yyjson character classifiers: `https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.c#L875-L902`.
- [Y6] yyjson memory/value layout: `https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.h#L1104-L1122` and `https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.h#L4878-L4894`.
- [Y7] yyjson unrolled number reader: `https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.c#L3880-L4078`.
- [Y8] yyjson Unicode escape reader: `https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.c#L4666-L4722`.
- [Y9] yyjson scalar FSM/root reader and trivia skip: `https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.c#L3355-L3408` and `https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.c#L5210-L5328`.
- [Y10] yyjson optimized string reader: `https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.c#L4735-L5055`.
- [Y11] yyjson read option dispatch/input copy: `https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.c#L6226-L6295`.
