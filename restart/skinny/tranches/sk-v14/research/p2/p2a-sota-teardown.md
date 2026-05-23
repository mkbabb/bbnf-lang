# SK-V14 P2-A: SOTA Comparator Teardown

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-23.
Scope: architecture teardown of asmjson, sonic-rs, simdjson, yyjson — for each, the structural-classification strategy, number/string fast paths, tape/DOM/on-demand output plane, and strict-vs-strict comparator discipline keyed to the SK-V14 S-P1 hot leaves; names precisely what each does that bbnf does not.
Output: this file.
P1 hot-leaf antecedents: `dispatch_value` (`generated.rs:45`); `parse_object_value_at_direct::<S: JsonSink>` (`generated.rs:466`); `parse_array_element_at_direct::<S: JsonSink>` (`generated.rs:506`); `match_tiny_plain_string` / `match_tiny_plain_string_with_cap::<16>` (`generated.rs:159,169`); `match_number_at_digit` (`generated.rs:213`); `parse_number_direct` (`generated.rs:650`); `parse_that_regex::unescape_string` (`parse-that-regex/src/lib.rs:718`); `parse_that_regex::read_hex_unit_scalar` (`parse-that-regex/src/lib.rs:945`); `parse_that_regex::number::materialize_u64` / `materialize_f64`; `runtime::generated_json::scan::scan_structurals` (`scan.rs:22`); `scan_tail` / `scan_tail_byte` (`scan.rs:107,131`); `bulk_emit_positions_64_neon` (`bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:2`); `DirectParser::skip_value` / `DirectParser::skip_array` (`generated_real_typed.rs:2949`); `parse_option_scalar_string` (`generated_real_typed.rs:2197`); `parse_type_{plugin,mesh,marine_geometry_data,instrument,...}` typed monomorphisations (`generated_real_typed.rs`).
Lock surface: Lock 1 (substrate union — no parallel substrate, no retained sidecar); Lock 14 (grammar-neutral primitives; no per-grammar branches in generic crates); Lock 16 (SIMD/ASM allowlist + scalar-reference + checkasm prerequisite). The strict-vs-strict comparator discipline is bound by SK-V14 R1 + R2 + P-2 (eager-DOM `sonic_rs::from_slice::<Value>` masquerading as strict comparator is the canonical anti-pattern that audit pack PRUNE-1 reverts).

---

## §1 — Findings

### §1.1 — Why P2-A exists and what binds it

The S-P1 P1-E synthesis (`research/p1/p1e-hot-leaf-attribution.md §2.1-§2.3`) names the SK-V14 hot leaves as overwhelmingly **dispatch envelopes**: 13/17 parse-only rank-1 leaves resolve to `dispatch_value` (`runtime/src/grammars/json/generated.rs:45`); 14/17 direct rank-1 leaves resolve to `parse_object_value_at_direct` or `parse_array_element_at_direct` (`generated.rs:466,506`); 5/7 typed rank-1 leaves resolve to `DirectParser::skip_value` (`bbnf-bench/src/generated_real_typed.rs:2949`). P1-E §4.1 names this the **CH2 Lock-14 mis-attribution census** — the envelopes mask the inner primitives the dispatch resolves into.

S-P2 P2-A's job is to read the SOTA comparators against those envelopes — name what each comparator does *inside the dispatch shell* that bbnf does not — and to do so under **R1 strict-vs-strict discipline**: every comparator anchor cited here is named with its strictness plane, and no permissive / lossy / eager-DOM row anchors a SOTA-beat claim. Per `SK-V14/SYNTHESIS.md §0.4 P-2` and `HANDOFF.md §7`, `sonic_rs::from_slice::<Value>` as parse_only comparator is the central audit-falsified pattern; PRUNE-1 reverts every admit grounded in that anti-pattern.

The four comparators sort cleanly into two strictness planes:

- **Strict, host-runnable**: sonic-rs strict struct deser + serde_json strict struct deser are the **only** JSON SOTA comparators currently runnable in-tree on aarch64 / Apple M5 Max with same-run, same-plane, same-corpus, same-equality semantics (`skinny/RESULTS.md` schema cols 13-15; per SK-V14 SYNTHESIS §2 telemetry binding, columns `sonic-rs strict Mbps` + `serde_json Mbps` are mandatory).
- **Architecture pressure only**: simdjson DOM + On-Demand, yyjson default, asmjson SWAR/AVX-512, RapidJSON default — all four are either (a) historical / absent / `n/a` in the result columns (`SK-V14 SYNTHESIS §2` lists `simdjson DOM Mbps` + `yyjson default Mbps` + `asmjson AVX-512 Mbps` as required-when-runnable / flaw-probe slots, not gate anchors), or (b) carry strictness asymmetries (asmjson permissive on control bytes; sonic-rs `from_slice::<Value>` is eager DOM, not parse_only) that the addendum + audit pack forbid as admission anchors.

This split is not negotiable for S-P2. Per SK-V14 R1, a row cannot admit without a **plane-correct strict comparator**: parse_only → sonic-rs Skipper-class (structural skip); direct → sonic-rs strict struct deser per corpus; typed → per-corpus typed struct deser. No row admits until its plane's comparator is strict-vs-strict. P2-A's candidate primitive list therefore must surface only primitives whose SOTA-beat target is sonic-rs strict (the single binding comparator), with simdjson/yyjson/asmjson cited as architectural pressure on the **technique**, never as the gate anchor.

### §1.2 — The four comparator architectures, keyed to S-P1 envelopes

| Comparator | Structural classification | Number fast path | String fast path | Output plane | Strict-vs-strict plane (SK-V14 R1) | What it does that bbnf does not — keyed to S-P1 envelope |
|---|---|---|---|---|---|---|
| **asmjson** (crate 0.2.5; docs.rs README) | Pre-pass 64-byte AVX-512BW classifier producing transient whitespace + quote + structural masks; SWAR (8-byte SIMD-within-a-register) portable fallback. AVX-512BW path is **x86-only**; no aarch64 path. | No first-class number parse — number tokens are dispatched to a Rust `JsonWriter` vtable. Means: asmjson does not parse numbers in asm; the consumer crate parses them in Rust. | Bulk whitespace + quote scan in classifier; string body itself is **not** scanned for unescaped controls. README §211-222 documents this as a conformance caveat. | DOM tree and SAX writer trait (`JsonWriter`); unsafe AVX-512BW direct entry points published. | **Architecture pressure only / NON-ADMITTING.** asmjson is permissive on control bytes inside strings ([A3]); cannot anchor any strict JSON row on any plane. AVX-512BW is x86-only; cannot run on aarch64 / M5 Max. | The 64-byte transient classifier shape — *one wide window classifies every structural byte without retaining a stream* — is the shape bbnf's `scan_structurals` (`scan.rs:22`) already attempts, but bbnf retains an intermediate `StructuralIndex` (positions Vec) that the dispatch envelope then walks. asmjson never materialises the index: the classifier output feeds the SAX writer *in the same window*. Maps onto S-P1's `dispatch_value` envelope: that envelope re-scans bytes that the SIMD scan already classified. **Lock-1 caveat:** asmjson's "no retention" pattern is the right shape, but adopting it means *removing* `StructuralIndex` retention from the substrate; cannot be wired as a parallel substrate. |
| **sonic-rs** (HEAD `03545a95...` per audit pack; `README.md:60-90`) | **Explicitly rejects simdjson's two-stage structural-index architecture.** Uses SIMD targeted at four leaves only: (1) long string spans, (2) float fraction parsing, (3) field/object lookup ("lazy field lookup" — skip unwanted fields with SIMD), (4) whitespace runs. No retained structural index. | Float fraction SIMD — accelerates the decimal-after-point + exponent body. `RawNumber` type preserves byte spans so that high-precision parsing is deferred to consumer. Integer path is scalar with overflow check. | Long-string SIMD — accelerates the *body* of a string once the open-quote is seen (not the classifier). `LazyValue` + `LazyObject` allow skipping over string bodies without decoding when the field is not wanted (the "lazy field lookup" lever, `docs/benchmark_aarch64.md:140-151`). | (a) serde direct-to-struct (sonic-rs's central differentiator vs simdjson — direct struct deser avoids the temporary tape); (b) untyped mutable `Value`; (c) lazy iterator (`LazyValue`, `LazyObject`); (d) `RawNumber` for deferred number precision. | **STRICT ANCHOR (plane-correct):** strict struct deser on direct + typed planes is the SK-V14 R1 binding comparator. **AUDIT-FALSIFIED ANCHOR:** `sonic_rs::from_slice::<Value>` (eager DOM) is the audit pack's P-2 anti-pattern; cannot anchor parse_only, direct, or typed rows. **Skipper-class** API (the parse_only plane analogue) is the R8 obligation. | Targeted SIMD in the exact families S-P1 names: string bodies, float fractions, field/object lookup, whitespace. bbnf has zero SIMD wiring inside `parse_object_value_at_direct`'s body — the SIMD is bbnf-simd's `scan_structurals` *outside* the envelope, and the envelope itself walks bytes scalar. **Lazy field lookup** (skip the wanted-field body via SIMD without decoding) is the technique S-P3 should adopt under R7 for the typed plane: it explains why sonic-rs's typed direct-deser anchor is competitive even though it has no global structural index. Maps onto S-P1's `parse_object_value_at_direct` envelope (`generated.rs:466`) — bbnf has no skip primitive there, only a full-value parse. Also maps onto `DirectParser::skip_value` (`generated_real_typed.rs:2949`) which is *bbnf's* skip path but lacks SIMD; sonic-rs's lazy field skip is the SOTA shape this typed-plane envelope should be benchmarked against. |
| **simdjson** (HEAD `168ef580...` per audit pack; `doc/parse_many.md:54-57`, `doc/basics.md:343-350`, `doc/ondemand_design.md:71-89`) | Two-stage architecture: **stage 1** scans the entire input producing structural indexes + UTF-8 validation; **stage 2** consumes the index into a tape representation. Stage 1 uses CLMUL prefix-XOR (`_mm_clmulepi64_si128`) to mask quote-pairs, `vpshufb`/`vqtbl1q_u8` for byte classification, `vpcmpeqb`-class equality + reduction trees for delimiter masks. | f64 mantissa multiplication via the Eisel-Lemire fast-path scalar implementation (no SIMD in number kernel for the DOM path); On-Demand defers number decode to use site. | Quote/escape/in-string masks computed in stage 1 (CLMUL prefix-XOR + bitmap shifts); string body decode in stage 2 is scalar-but-tight (escape branch is rare). | (a) **DOM** (tape + value tree, materialised); (b) **On-Demand** (forward-only iterator that parses/skips values lazily — `doc/ondemand_design.md:71-89`). On-Demand is the architectural pressure most relevant to bbnf's typed plane. | **Architecture pressure / NOT host-runnable as strict anchor** without same-run sidecar wiring. Per `skinny/RESULTS.md:3,145-149` the simdjson columns are historical/absent; SK-V14 SYNTHESIS §2 names `simdjson DOM Mbps` + `simdjson On Demand Mbps` as `required when runnable, plane disclosed` — i.e. a flaw-probe slot, never the bind. simdjson is strict-by-default (RFC 8259 UTF-8 mandatory), so the strictness plane is correct *if* a same-run sidecar is wired; the gap is infrastructural, not strictness. | **The structural-index materialisation pattern + the On-Demand skip pattern.** bbnf's structural-index materialisation (`StructuralIndex` from `scan_structurals` → consumed by parse-only path) is the simdjson shape; but **bbnf's direct + typed planes re-scan structural bytes inside the envelope** rather than consuming the pre-built index. This is the substrate-union violation P1-E §4.4 names: the DirectParser cursor and `parse_object_value_at_direct`'s cursor are two structurally independent state machines on the same source. simdjson keeps the stage-1 output usable by stage-2; bbnf's stage-1 output (the `StructuralIndex`) is consumed only by parse_only. Under Lock 1, the simdjson lift is "the structural projection IS the tape" — the projection cannot be a retained sidecar (REDRESS 96/97/98 already falsified parallel retained class-columns), but it must be the **single** substrate the envelope consults. **On-Demand** is the typed-plane lift: `DirectParser::skip_value` is bbnf's equivalent, but it scans bytes without consulting any structural index — adopting On-Demand's "skip a value by consulting the index" would short-circuit `skip_value`'s 72.5-76.1 % self-time. |
| **yyjson** (HEAD `95f4c61b...` per audit pack; `README.md:10-19,73-78`, `src/yyjson.h:736-744`) | **No explicit SIMD.** Portable ANSI C. Relies on high ILP (Instruction-Level Parallelism), branch prediction, compact data layout, and force-inline discipline. Structural classification is a per-byte switch dispatch hot-fused by LTO + `always_inline`. | Specialized scalar integer + float readers with strict default flags. `parse_decimal` / `parse_integer` are inlined hot paths. Eisel-Lemire fast-path for f64. | Specialized scalar string readers; escape branch detection scalar-but-tight. | Immutable + mutable document/value model; object/array iteration is C value-tree walking. | **STRICT ANCHOR (plane-correct):** strict default per `src/yyjson.h:736-744`; opt-in JSON5/permissive features. Currently no same-run sidecar in `skinny/RESULTS.md`; SK-V14 SYNTHESIS §2 names `yyjson default Mbps` as `required when runnable, strictness disclosed` — flaw-probe / architecture-pressure tier until wired. | **The scalar-discipline lesson.** yyjson achieves 0.91 c/B twitter (29.5K Mbps on M5 Max per `LOCKS.md:268` Lock 15 evidence), beating simdjson DOM's 1.142 c/B — without SIMD — by force-inlining the parser into an ~18 KiB i-cache-resident hot function under LTO. Maps onto S-P1's `dispatch_value` envelope: bbnf's `dispatch_value` has the right scalar shape but is buried under a `cfg_attr(feature = "parse-attribution", inline(never))` plumbing that masks inner-primitive attribution (`generated.rs:43-44`). The yyjson lift is **not** "add SIMD" — it is "trust the scalar; LTO-fuse the hot path; verify i-cache residency". Per Lock 15 the JSON parser is already a 7,304-byte hot function (sub-budget); the open question is whether the dispatch envelope's force-inline discipline matches yyjson's. **Critically: yyjson refutes the assumption that SIMD is required for SOTA** — a candidate primitive that wins on microbench but loses on integrated row vs scalar yyjson-shape envelope is rejected (per `audit/totality/p2/2A-sota-landscape.md` LOCKS-AMENDMENT T2A-LAC-05). |

### §1.3 — Cross-comparator consensus + dissensus (what every SOTA does vs what only some do)

**Every SOTA does:**
1. Strict default with explicit opt-in to permissive behaviour. (yyjson opt-in JSON5 flags; simdjson strict RFC 8259; sonic-rs strict default with lossy as separate API.) The single permissive default in this set is asmjson (controls inside strings) — which is why asmjson cannot be a strict anchor.
2. UTF-8 validation either in-line (simdjson stage 1, fused) or deferred to a parallel pass (yyjson lazy `core::str::from_utf8` per the P1-B Track 2 serde rank-3 leaves at `unicode_basic-typed-Track2 26.01 %`, `random-typed-Track2 21.80 %`).
3. Separate output planes (DOM / direct-struct / on-demand / lazy) — sonic-rs and simdjson and yyjson all carry ≥2 surface shapes. The SK-V14 three-plane bench (parse_only, direct_to_struct, real_typed_struct) is a faithful counterpart.
4. Force-inline + LTO discipline on the hot path. yyjson is the extreme case (~18 KiB resident); simdjson and sonic-rs both pin LTO + `always_inline` on the hot kernels.

**Only some SOTAs do:**
1. **Pre-materialised structural index** — only simdjson + asmjson. sonic-rs explicitly rejects this. yyjson does not have one. bbnf has `StructuralIndex` produced by `scan_structurals` but only the parse_only path consumes it; direct + typed planes rescan. This is the central SK-V14 substrate-union question.
2. **Lazy field-skip** — only sonic-rs (`LazyObject` / `LazyValue`) and simdjson On-Demand. yyjson and asmjson eagerly materialise values. The typed plane's `DirectParser::skip_value` (`generated_real_typed.rs:2949`) is bbnf's equivalent but **lacks SIMD skip** — every comparator's typed direct-deser surface is competitive *because* of lazy/skip-aware iteration; bbnf's typed plane skip is the right shape with the wrong implementation.
3. **CLMUL-class prefix-XOR string-mask** — only simdjson + (potentially) asmjson AVX-512. sonic-rs does not, yyjson does not. bbnf has `scalar/bitmap_prefix_xor_64.rs` + `aarch64/bitmap_prefix_xor_64.rs` but the consumer is the structural scan, not the inside-string mask. **REDRESS 88 PMULL prefix-XOR as hot body is a pre-blocked route** (`SK-V14 SYNTHESIS §0.4`) — adopting simdjson's CLMUL pattern requires a fresh material differential.
4. **Targeted SIMD inside the parse envelope** (long-string, float-fraction, whitespace-run) — only sonic-rs. simdjson does this at the stage-1 boundary; sonic-rs does it *inside* the parser. bbnf has zero SIMD inside `parse_object_value_at_direct`'s body. This is the cleanest grammar-neutral lift in the S-P1 hot-leaf census.

### §1.4 — Strictness-plane discipline (R1 binding)

Per SK-V14 R1 the comparator must be strict on its own plane. The audit-pack P-2 anti-pattern is `sonic_rs::from_slice::<Value>` as parse_only comparator: it is *eager DOM deserialisation*, not parse_only. P2-A explicitly names every comparator-plane mapping below so S-P3 cannot inherit the wrong plane:

| Plane | bbnf surface | Strict-vs-strict comparator (R1) | Strictness witness |
|---|---|---|---|
| `parse_only` | `runtime::generated_json::parse_value_at` driving `scan_structurals` → consume positions; no value materialisation | **sonic-rs Skipper-class** (structural-skip iteration with no value materialisation). API gap: sonic-rs has `LazyValue::get` but no public "skip every value" API; S-P3 R8 obligation includes either (a) wrapping `LazyObject`/`LazyValue` traversal to skip-all-fields, or (b) measuring serde_json::Deserializer::from_slice::<IgnoredAny> as the strict parse_only baseline (rust strict default). The audit-falsified P-2 row family used `sonic_rs::from_slice::<Value>` — eager DOM, not parse_only. | sonic-rs default is RFC 8259 strict on UTF-8 + numbers; the Skipper-class iteration inherits that strictness. |
| `direct_to_struct` | `parse_object_value_at_direct::<JsonDigestSink>` + `parse_array_element_at_direct::<JsonDigestSink>` (`generated.rs:466,506`) | **sonic-rs strict struct deser per corpus** (`sonic_rs::from_slice::<MyCorpusStruct>`). Per-corpus typed deserialisation; same input, same output struct. The narrow 4 + broad 6 AUDIT-FALSIFIED rows per SYNTHESIS §0.2 reconciliation block all anchored against `from_slice::<Value>` (DOM); R1 rebind is to per-corpus struct. | sonic-rs strict default; per-corpus struct types defined in `bbnf-bench` for each of the 17 corpora is the SK-V14 R7 prerequisite. |
| `real_typed_struct` | `bbnf_bench::generated_real_typed::parse_type_*` typed monomorphisations + `DirectParser::skip_value` walk (`generated_real_typed.rs:516,1150,1330,2197,2949`) | **per-corpus typed struct deser** — serde-derived strict deser per corpus (`serde_json::from_slice::<MyCorpusStruct>` is the in-tree strict baseline; sonic-rs strict struct deser is the SOTA anchor). Per SYNTHESIS §0.2 reconciliation, the broad 11-row AUDIT-FALSIFIED population includes the W13.1-.4 + W15.1 + W6 cells where the admit cited `sonic_rs::from_slice::<Value>` instead of the per-corpus typed deser. | sonic-rs strict default + per-corpus serde-derived structs; serde_json strict default is the in-tree fallback baseline. |

The strictness witness is the load-bearing column. A comparator passes R1 iff (a) the comparator's strict mode is the configured mode (not the lossy/permissive opt-in), (b) the comparator output plane matches the bbnf plane (parse_only must consume bytes without materialising values; direct_to_struct must deserialise into the same struct shape; real_typed must deserialise into the same typed-struct shape), and (c) per R2 the equality is verified inside the timing region per iteration. asmjson fails (a) for controls-in-string; `sonic_rs::from_slice::<Value>` fails (b) for parse_only and direct; simdjson + yyjson fail nothing at the strictness level but currently fail (c) absence in the harness.

### §1.5 — What bbnf already does + what each comparator does that bbnf does not (envelope-by-envelope)

The S-P1 envelope census (P1-E §2.1-§2.3) gives the load-bearing surface. For each dominant envelope, the per-comparator delta:

**`dispatch_value` (parse-only rank-1 on 13/17 corpora; `generated.rs:45`).**
- *bbnf today*: branch-on-first-byte switch (`{` → object, `[` → array, `"` → string, `-`|`0..9` → number, etc.); inner primitives `match_tiny_plain_string`, `match_number_at_digit`, `match_string_at_quote` called from arms but `inline(always)` masks per-primitive attribution.
- *sonic-rs does not have a parse_only path beyond `LazyValue::get` — the analogue is iteration without materialisation; sonic-rs's targeted-SIMD-on-string-bodies happens inside its own dispatch, not at the structural boundary.
- *simdjson* has stage 1 / stage 2 separation; parse_only equivalent is "stage 1 only" — produce structural indexes and stop. bbnf's `scan_structurals` already does this; the gap is consumption.
- *yyjson* has scalar dispatch identical in shape to bbnf's. yyjson's edge over bbnf in c/B is **not** algorithmic; it is force-inline + i-cache discipline.
- *asmjson* fuses dispatch into its SAX writer — classifier feeds writer in same window.
- **bbnf gap**: dispatch_value is the right scalar shape but is masked by `cfg_attr(parse-attribution, inline(never))` plumbing being OFF in the bench build. Per P1-E §4.1, S-P2 must enable `parse-attribution` for one full profile pass to crack the envelope; this is the **F-V2-P1ABC-RERECORD** deferred packet (per SK-V14 P1 V2 fold note). Until then, the envelope-vs-primitive question is empirically unresolved at SK-V14 V1.

**`parse_object_value_at_direct::<JsonDigestSink>` (direct rank-1 on 8/17 corpora; `generated.rs:466`).**
- *bbnf today*: scalar walk of object {key: value, ...} bodies; calls `match_tiny_plain_string` for keys, `dispatch_value` for values, `parse_that_regex::unescape_string` for escaped keys + strings.
- *sonic-rs*: serde direct struct deser — SIMD-accelerated long-string body + lazy field skip when the target struct does not name the field. **bbnf has neither.**
- *simdjson On-Demand*: forward-only iterator skips object values by consulting the structural index, never re-scanning. **bbnf re-scans** — the `parse_object_value_at_direct` cursor and the `StructuralIndex` from `scan_structurals` are two structurally independent state machines per P1-E §4.4 + the SK-V14 dispatch context §1.
- *yyjson*: scalar value-tree walk with force-inline discipline.
- *asmjson*: out of scope (x86 + permissive).
- **bbnf gap**: no SIMD-accelerated long-string body inside the envelope; no lazy field-skip when the sink does not consume the field. The sonic-rs lift here is **grammar-neutral** (long-string SIMD applies to any quoted-string grammar — JSON, CSS strings, Sheets text, BBNF literals), satisfying Lock 14.

**`parse_array_element_at_direct::<JsonDigestSink>` (direct rank-1 on 6/17 corpora; `generated.rs:506`).**
- Same gap profile as object-value-at envelope, with the additional float-fraction SIMD opportunity for canada / mesh / marine_ik / numbers corpora (rank-2/3 leaves include `materialize_f64`, `materialize_u64`). sonic-rs has the float-fraction SIMD; simdjson defers to scalar Eisel-Lemire; yyjson is scalar Eisel-Lemire.
- **bbnf gap**: number kernel is `parse-that-regex::number::materialize_{u64,f64}` (scalar); no SIMD digit-block accumulation. Lock 16 lists arm64 NEON UDOT / SDOT 4-byte dot-product as admissible **abstract primitive** for digit-block decode — generalises to JSON number, CSS `<number>`, Sheets formulas — but the current `digit_mac.rs` is support-only (`bbnf-simd/src/aarch64/digit_mac.rs`). Per `audit/totality/p2/2A-sota-landscape.md` "support-only landings disallowed", this primitive is admissible only with same-wave row consumer that moves a number-heavy corpus.

**`DirectParser::skip_value` (typed rank-1 on 5/7 surfaced corpora; `generated_real_typed.rs:2949`).**
- *bbnf today*: 72.5 % (twitter), 76.1 % (citm_catalog), 41.7 % (marine_ik), 41.7 % (github_events), 39.5 % (mesh) self-time per P1-E §2.3. Scalar walk of arbitrary JSON value, advancing the cursor without materialising; called when the typed product struct does not name the current field. This is bbnf's lazy-field-skip path — but it has no SIMD, no structural-index consultation, and re-scans every byte.
- *sonic-rs*: `LazyValue::get` consults SIMD-accelerated field lookup; bypassed bodies are skipped via SIMD string-end scan + structural skip.
- *simdjson On-Demand*: skip-by-index — consult stage-1 indexes, jump cursor to next structural marker without rescanning bytes.
- *yyjson*: scalar walk, similar shape to bbnf, but with force-inline + scalar fast paths for the dominant cases.
- **bbnf gap (load-bearing)**: `DirectParser::skip_value` is the typed-plane envelope dominator at 39-76 % self-time across 5 corpora. **No comparator's skip path lacks structural-index consultation.** This is the largest single typed-plane primitive opportunity per the S-P1 census. Maps to candidate primitive **C2-typed-skip-with-index** in §2.

**`parse_that_regex::unescape_string` (direct rank-1 on `unicode_escapes` at 46.7 %; rank-2/3 on 17/56 profiles per P1-B §2.1).**
- *bbnf today*: scalar one-byte-at-a-time scan with backslash-escape branch and `read_hex_unit_scalar` for `\uXXXX`.
- *sonic-rs*: long-string body SIMD; UTF-8 lossy / strict modes (the lossy mode is the SK-V6 finding — not an admission anchor).
- *simdjson*: stage-1 in-string mask via CLMUL prefix-XOR; stage-2 escape branch.
- *yyjson*: scalar fast path; escape branch rare.
- **bbnf gap**: no SIMD string-body scan; the `aarch64/string_block.rs` + `aarch64/match_tiny_plain_string.rs` modules exist but the consumer wiring is incomplete (REDRESS 28+33 is pre-blocked; REDRESS 82-84 single-quartet unicode + StringBlock16 tiny-probe is pre-blocked). The `aarch64/unescape_uxxxx.rs` module is the right shape for `\uXXXX` decode but is support-only today.

**`parse_that_regex::read_hex_unit_scalar` (`y_string_unicode` parse-only rank-1 at 100.0 %; `parse-that-regex/src/lib.rs:945`).**
- *bbnf today*: 4-nibble scalar hex decode; full unicode escape path.
- *sonic-rs*: scalar with optimised nibble-class table.
- *simdjson*: rare-branch scalar; not hot in DOM/On-Demand.
- *yyjson*: scalar fast path.
- **bbnf gap**: the `aarch64/unescape_uxxxx.rs` NEON nibble-classify primitive is the right shape but support-only; same-wave consumer is the y_string_unicode + unicode_escapes corpora. Lock 16 admissibility holds (NEON TBL/TBX + nibble classify).

---

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent + grammar-neutrality verdict + REDRESS check)

Per `[no-god-modules]` + `[module-structure-codegen]`, every primitive below is a proper sub-module of `bbnf-simd` (Layer 1) or a per-grammar generated module (`runtime/src/grammars/<g>/...` consumer); none enter a kitchen-sink `utils`/`common`. Per Lock 14, no JSON-specific naming in a generic crate. Per Lock 16, every primitive carries a scalar reference + checkasm parity expectation + same-wave consumer note before admission.

### Candidate C1 — `lazy_field_skip_with_index`

- **Shape**: Given a structural index (positions Vec produced by `scan_structurals`) and a current cursor inside an object/array body, advance the cursor to the *next* structural marker (`,`, `]`, `}`) without re-scanning the value bytes. Returns the new cursor position + the marker byte. Grammar-neutral: works for any grammar whose structural delimiters are pre-classified into a positions stream.
- **Scalar reference status**: scalar reference is `DirectParser::skip_value` (`generated_real_typed.rs:2949`) without index consultation — re-scans bytes. New scalar reference required: walk positions Vec advancing-past-current-cursor, return first position > cursor whose marker is in {`,`, `]`, `}`} at object-nest-depth 0 relative to cursor's depth. The depth-tracking complexity (nested objects/arrays inside the skipped value) is the load-bearing scalar correctness witness.
- **Arch**: scalar (positions Vec binary-search or linear-scan); SIMD admissible only if positions Vec scan becomes the hot leaf. aarch64 NEON `vceqq_u8` set-membership classify against {`,`, `]`, `}`} could accelerate the marker filter; admit only with checkasm parity (`bbnf-simd/tests/checkasm_*`).
- **P1 antecedent**: `DirectParser::skip_value` at 39.5-76.1 % self-time on 5/7 typed rows (P1-E §2.3). Single largest typed-plane gap.
- **Grammar-neutrality**: GENERALISES. Lazy field skip via structural-index consultation is the lift from simdjson On-Demand + sonic-rs `LazyValue`. Grammar-neutral because: (a) structural index is grammar-neutral (the byte set is grammar-supplied via GrammarConfig — JSON's `{}[],:` is data, not code); (b) skip semantics are grammar-neutral (advance cursor past value-boundary at current depth). Generalises to CSS L4 (skip a declaration value; structural set is `;` + `{` + `}` + `,`), Sheets (skip a formula token; structural set is `,` + `)` + cell-ref boundary), BBNF-self (skip a grammar production body; structural set is `;` + alternation `|`).
- **REDRESS check**: does not re-open REDRESS 28+33 (tiny-string NEON), 50-55 (UTF-8 fusion), 60-72 (retained-parse + sidecar producers + digest cap-16), 80 (canada mantissa-widen), 82-84 (single-quartet unicode + StringBlock16 + object-pair compaction), 88 (PMULL prefix-XOR), 89 (CSSC CTZ next-bit), 96/97/98 (retained class-column / streaming cursor / class-lane-only). **Substrate-union compliance (Lock 1)**: this primitive *consumes* the existing `StructuralIndex` (single substrate); does not propose a parallel scan or a retained sidecar. The substrate union question is whether the `StructuralIndex` produced by `scan_structurals` (currently consumed only by parse_only) becomes the singular substrate consumed by direct + typed envelopes as well. Per P1-E §4.4 + the dispatch context §1, the substrate union must hold; this primitive is the same-wave consumer that demonstrates the union.
- **Same-wave consumer**: typed-plane `DirectParser::skip_value` rewrite consuming the structural index; direct-plane `parse_object_value_at_direct` (per-key conditional skip when sink does not consume); CSS L4 declaration-value skip (per S-P3 R6).

### Candidate C2 — `long_string_body_simd_scan`

- **Shape**: Given a cursor positioned after an open-quote, scan the string body via SIMD looking for the first of: `"` (close-quote), `\\` (escape), or control byte (< 0x20). Returns the end position + the stop reason. Grammar policy (which bytes are terminators, which are escapes, which are controls) is supplied via grammar config; the primitive itself owns no JSON-specific constants.
- **Scalar reference status**: scalar reference is `match_tiny_plain_string_with_cap::<16>` (`generated.rs:169`) for short strings + `parse_that_regex::unescape_string` (`parse-that-regex/src/lib.rs:718`) for the general path. The scalar reference for *long* strings (> 16 bytes plain, plus escape handling) needs to be lifted: a clean scalar fn `scan_string_body(input: &[u8], cursor: usize, policy: &StringPolicy) -> StringScanResult` whose policy is a generated table from GrammarConfig.
- **Arch**: aarch64 NEON wide-window (`vld1q_u8_x4` quad-load, Lock 16 admissible) + `vceqq_u8` for `"` + `\\` + `vcleq_u8` for control-byte; reduction-tree movemask via `vshrn_n_u16` + `vsriq_n_u8` + `vzip1q_u8` (Lock 16 Validark 2024 admissible). Checkasm: all alignments, escape positions, control positions, tails 0-63, long ASCII, high-bit bytes, grammar-specific terminators (JSON `"` vs CSS `'`/`"` vs BBNF `'`/`"`).
- **P1 antecedent**: `parse_that_regex::unescape_string` rank-1 on `unicode_escapes-direct-Track1` at 46.7 %; rank-2/3 on 17/56 P1-B profiles (twitter, gsoc-2018, mesh, github_events string-heavy direct rows). The sonic-rs long-string SIMD is the SOTA shape this primitive lifts.
- **Grammar-neutrality**: GENERALISES. String-body scan with terminator + escape + control policy is the canonical lift — every grammar with quoted strings has the same shape (JSON, CSS strings, Sheets text, BBNF literals, TOML, INI, SQL). Per Lock 14, the terminator + escape + control byte sets MUST come from generated grammar config or caller data — not hardcoded JSON constants. The `bbnf-simd::aarch64::string_block` module is the existing Layer-1 surface; consumer wiring is the gap.
- **REDRESS check**: must not replay REDRESS 28+33 (Class A NEON tiny-string wiring) without grammar-neutral policy + row movement; must not replay REDRESS 82-84 (single-quartet unicode classifier, StringBlock16 tiny-probe, object-pair compaction) without fresh material differential. Per CH3, the differential is: this primitive is the long-string body scan, not a tiny-string probe — different envelope (`parse_*_at_direct` long-string consumer, not `match_tiny_plain_string` short-cap consumer).
- **Same-wave consumer**: direct-plane long-string in `parse_object_value_at_direct` + `parse_array_element_at_direct` envelopes; same-wave row movement on twitter, gsoc-2018, mesh, github_events; CSS L4 string-row consumer per S-P3 R6.

### Candidate C3 — `digit_block_simd_accumulate`

- **Shape**: Given a cursor positioned at first digit, accumulate a digit run into integer/mantissa lanes via NEON `udot`/`sdot` 4-byte dot-product (Lock 16 admissible, Armv8.2-A). Returns: end position, digit count, overflow flag, decimal/exponent boundary, raw span. Number policy (sign, decimal separator, exponent letter, grouping) above the primitive layer; primitive emits raw integer mantissa lanes.
- **Scalar reference status**: scalar reference exists as `parse_that_regex::number::materialize_u64` + `materialize_f64`; need a clean scalar fn `accumulate_digit_run(input: &[u8], cursor: usize) -> DigitRunResult` that mirrors the SIMD primitive's output shape (lanes-per-4-bytes).
- **Arch**: aarch64 NEON UDOT/SDOT (`bbnf-simd/src/aarch64/digit_mac.rs` is the existing support-only module — same-wave consumer is the gap). Checkasm: 0/1/4/8/16/19/20-digit runs, overflow at u64 boundary, signs, decimal/exponent transition, invalid stops, CSS dimension/percentage samples, all alignments.
- **P1 antecedent**: `materialize_f64` rank-2 on canada-direct-Track1 (14.32 %), mesh-direct-Track1 (6.09 %), marine_ik-direct-Track1 (5.54 %); `materialize_u64` rank-3 on citm_catalog (2.04 %), random (0.83 %), instruments (2.60 %); `parse_decimal` / `parse_integer` rank-1-2 across serde_json Track 2 typed rows (mesh-typed-Track2 28.17 %, numbers-typed-Track2 58.68 %, marine_ik-typed-Track2 13.03 %).
- **Grammar-neutrality**: GENERALISES per Lock 16 abstract primitive ("byte-window multiply-accumulate, lifted from dav1d's FIR filter — applies to ANY grammar's digit-block decode"). JSON number, CSS L4 `<number>`, Sheets numerics, TOML/INI/SQL integer literals all share the digit-run shape. Number policy above the primitive (which characters are digits, sign convention, decimal separator) comes from grammar config.
- **REDRESS check**: does not re-open REDRESS 80 (canada mantissa-widen) — that route was canada-specific mantissa widening, not the generic digit-block accumulate primitive. Per CH3, the material differential is: this primitive is the digit-block accumulator (input → raw mantissa lanes), not the f64 widening pass (raw mantissa → f64). The two compose; primitive C3 is the producer, the f64 conversion (Eisel-Lemire fast-path scalar) is the consumer.
- **Same-wave consumer**: direct-plane number kernel in `parse_array_element_at_direct` on canada / mesh / marine_ik / numbers (number-heavy corpora); typed-plane `parse_vec_cap_10800_scalar_f64` (`generated_real_typed.rs` mesh-typed Track 1 rank-2 at 27.62 %); CSS L4 `<number>` / `<dimension>` per S-P3 R6.

### Candidate C4 — `force_inline_lto_envelope_discipline`

- **Shape**: NOT a SIMD/ASM primitive. The yyjson lift: codegen template force-inlines the dispatch envelope (`dispatch_value` + `parse_object_value_at_direct` + `parse_array_element_at_direct`) under `#[inline(always)]`; LTO `lto = "fat"` + `codegen-units = 1` fuses inner primitives into the envelope; verify the fused hot function size remains ≤ 20 KiB i-cache budget (Lock 15 binding; yyjson reference ~18 KiB; bbnf JSON parser currently 7,304 bytes at Lock 15 evidence point — sub-budget). This is the **scalar-discipline candidate**: not a new kernel, but a codegen + profile invariant that the existing envelope is force-inline + i-cache resident.
- **Scalar reference status**: N/A — this is a build invariant, not a kernel. Scalar reference exists by construction (the envelope IS scalar).
- **Arch**: arch-neutral; the discipline is RUSTC + LTO behaviour. Verification via `cargo asm` for fused-function size; verification via samply with `parse-attribution` OFF for hot-function residency.
- **P1 antecedent**: `dispatch_value` 13/17 parse-only rank-1 at 97-100 %; the c/B headroom gap vs yyjson (bbnf SK-V14 V1 ~1.14-5.67 c/B per P1-E §3 range; yyjson 0.91 c/B twitter per Lock 15) is the scalar-discipline gap, not a SIMD gap.
- **Grammar-neutrality**: GENERALISES. Force-inline + LTO + i-cache budget is a codegen template invariant; applies to every grammar's hot dispatch envelope. Per Lock 15 diagnostics: `BBNF-FORCE-INLINE-MISSED` fires when a rule mined as hot-path lacks `#[inline(always)]`; `BBNF-ICACHE-BUDGET-EXCEEDED` fires when fused-function size exceeds budget. Diagnostics are grammar-neutral.
- **REDRESS check**: does not re-open any REDRESS route — this is build-discipline + codegen, not a new kernel. No substrate addition. **Lock 1 compliant by construction** (no new substrate, no sidecar). **Lock 14 compliant by construction** (codegen template, grammar-neutral).
- **Same-wave consumer**: the codegen template itself; same-wave verification is `cargo asm` + samply re-record post-`parse-attribution`-flip to confirm the dispatch envelope hot-function size + per-leaf residency.

### Candidate C5 — `structural_index_singular_substrate_consumer`

- **Shape**: the substrate-union completion candidate. `parse_object_value_at_direct` + `parse_array_element_at_direct` + `DirectParser::skip_value` ALL consume the `StructuralIndex` produced by `scan_structurals` instead of re-scanning bytes. The `StructuralIndex` becomes the singular substrate (per Lock 1: "the structural projection IS the tape"); the direct + typed envelopes consult it via a shared cursor abstraction. **No new substrate; no parallel scan; no retained sidecar.**
- **Scalar reference status**: scalar reference is the existing `scan_structurals_scalar` (`scan.rs:32`) + a new scalar consumer in the dispatch envelopes that walks positions Vec instead of bytes. Equivalence proof: parse output (sink callbacks + tape positions emitted) is byte-equivalent under both consumers.
- **Arch**: scalar consumer; SIMD admissible only if positions Vec walk becomes a hot leaf (it shouldn't — the walk is amortised over value decode work).
- **P1 antecedent**: P1-E §4.4 substrate-union finding — DirectParser::cursor + parse_object_value_at_direct cursor are two structurally independent state machines; the SK-V14 dispatch context §1 binds that P2-D's tape interrogation must conclude the substrate union holds. C5 is the candidate that *operationalises* the union.
- **Grammar-neutrality**: GENERALISES. The structural projection (`StructuralIndex`) is grammar-neutral by construction (per Lock 14: "Shared `bbnf-simd`, parse-that, and future regex APIs expose grammar-neutral facts and primitives only. Quote, escape, control, delimiter, number, string, and no-string/no-number policy must come from generated grammar config or caller data, not hardcoded JSON/CSS constants"). The consumer wiring (direct + typed envelopes consult positions Vec) is per-grammar generated code in `runtime/src/grammars/<g>/`.
- **REDRESS check**: must not re-open REDRESS 96/97/98 (retained class-column / streaming cursor / class-lane-only) — those routes added a *parallel* substrate. C5 does the opposite: it removes the parallel substrate by making the existing structural projection the singular substrate. **Per CH3, the material differential is "no new retained data; only re-route existing consumer to existing producer"**. Substrate-target = `existing_tape` (per LOCKS.md:73-82 Lock 1 v+1 contract); retention_lifetime = `output_row`; policy_owner = `generated_grammar`. Per CH5 (Lock 1), this is the substrate-union-honouring candidate — opposite of a violation.
- **Same-wave consumer**: direct + typed envelopes in `parse_object_value_at_direct`, `parse_array_element_at_direct`, `DirectParser::skip_value` (same wave; mandatory same-wave consumer per Lock 1).

### Candidate C6 — `parse_attribution_envelope_cracker` (process, not primitive)

- **Shape**: a build-discipline candidate, not a kernel. Per P1-E §4.1 + dispatch context §1.1 CH2 F1: the `parse-attribution` feature is `runtime`-crate-private; bench-harness must invoke `--features runtime/parse-attribution` (transitive). 14 functions gated at `runtime/src/grammars/json/generated.rs:33-237` via `#[cfg_attr(feature = "parse-attribution", inline(never))]`. With the feature OFF, dispatch envelopes inline inner primitives and mask attribution; with the feature ON, the inner primitives become measurable separately.
- **Scalar reference status**: N/A — this is a profiling discipline, not a kernel.
- **Arch**: arch-neutral.
- **P1 antecedent**: every dispatch-envelope row in P1-E §2.1-§2.3 (27 of 34 parse-only + direct rank-1 leaves).
- **Grammar-neutrality**: GENERALISES. Every grammar's dispatch envelope benefits from cracking-open attribution at profile time. The `parse-attribution` feature is grammar-neutral by construction (it applies to the `runtime` crate's generated dispatch).
- **REDRESS check**: no REDRESS route opened.
- **Same-wave consumer**: the **F-V2-P1ABC-RERECORD** deferred packet (per SK-V14 dispatch context §1) — re-record P1-A/B/C with `--features runtime/parse-attribution` + longer github_events iter. This is wave-program work post-G-Omega per dispatch context §1; flagged here so S-P3 sequences it.

### Candidate C7 — `unicode_escape_neon_nibble_decode`

- **Shape**: NEON nibble-classify + accumulate primitive for `\uXXXX` 4-nibble hex decode + UTF-16 surrogate pair handling. Inputs: 6-byte window (`\\` + `u` + 4 hex nibbles) or 12-byte window (2 surrogates). Outputs: decoded scalar(s) + validity + surrogate state + raw span.
- **Scalar reference status**: scalar reference is `parse_that_regex::read_hex_unit_scalar` (`parse-that-regex/src/lib.rs:945`) + `parse_that_regex::unescape_string` (`parse-that-regex/src/lib.rs:718`). Public scalar oracle for the windowed decode (xN hex with surrogate state) is the gap.
- **Arch**: aarch64 NEON TBL/TBX (Lock 16 admissible) for nibble classify; `bbnf-simd/src/aarch64/unescape_uxxxx.rs` is the existing support-only module. Checkasm: valid hex, invalid hex (non-hex chars), low/high surrogate pair, lone surrogate, mixed raw + escaped, tails, alignment, xorshift adversarial.
- **P1 antecedent**: `read_hex_unit_scalar` 100.0 % rank-1 on `y_string_unicode` parse-only (P1-E §2.1); `unescape_string` 46.7 % rank-1 on `unicode_escapes` direct (P1-E §2.2) + 22.5 % rank-2 on `unicode_mixed` direct (P1-B §2 direct table).
- **Grammar-neutrality**: GENERALISES. `\uXXXX` hex-escape decode is a unicode escape primitive — applies to JSON, CSS escaped idents/strings (`\41` → `A`), JS template literals, regex escapes, BBNF literals. Per Lock 14, the escape syntax (which prefix character starts an escape, fixed-width vs variable-width, surrogate handling) comes from grammar config; the primitive owns the nibble decode only.
- **REDRESS check**: must not replay REDRESS 82 (single-quartet unicode classifier) without grammar-neutral policy. Per CH3, the material differential is: this primitive is the **windowed** nibble decode (`\\` + `u` + 4 nibbles, possibly chained to a second 6-byte surrogate window), not the single-quartet classifier (which classifies one quartet against a fixed table). Different shape, different consumer.
- **Same-wave consumer**: parse-only `y_string_unicode` row + direct-plane `unicode_escapes` + `unicode_mixed` rows; CSS L4 escaped-ident row per S-P3 R6.

### §2.1 — Candidate count + summary

**7 candidate primitives** surfaced from the four-comparator teardown keyed to S-P1 hot leaves:

| # | Candidate | Type | Grammar-neutral? | P1 antecedent dominance | Strict comparator anchor | REDRESS risk |
|---|---|---|---|---|---|---|
| C1 | `lazy_field_skip_with_index` | substrate-consumer | YES | `DirectParser::skip_value` 39-76 % on 5/7 typed rows | sonic-rs `LazyValue` + simdjson On-Demand (architecture pressure); sonic-rs strict struct deser as strict gate | low (does not reopen any REDRESS) |
| C2 | `long_string_body_simd_scan` | SIMD primitive (Layer 1) | YES (string policy from grammar config) | `unescape_string` 46.7 % on unicode_escapes; 17/56 rank-2/3 hit rate | sonic-rs long-string SIMD (architecture pressure); sonic-rs strict as strict gate | med (REDRESS 28+33 / 82-84 must not be replayed without differential) |
| C3 | `digit_block_simd_accumulate` | SIMD primitive (Layer 1) | YES (number policy from grammar config) | `materialize_f64`/`materialize_u64` rank-2/3 on canada/mesh/marine_ik/numbers; serde_json `parse_decimal` 28-59 % typed Track 2 | sonic-rs float-fraction SIMD (architecture pressure); sonic-rs strict + serde_json strict as strict gate | med-low (REDRESS 80 distinct; abstract primitive Lock 16 admissible) |
| C4 | `force_inline_lto_envelope_discipline` | codegen invariant | YES (grammar-neutral diagnostics) | `dispatch_value` 97-100 % on 13/17 parse-only rows; c/B headroom vs yyjson | yyjson scalar 0.91 c/B twitter (Lock 15 reference) | none (build invariant; Lock 15 already binding) |
| C5 | `structural_index_singular_substrate_consumer` | substrate-union completion | YES (structural index is grammar-neutral; per-grammar consumer in `runtime/src/grammars/<g>/`) | substrate-union P1-E §4.4 finding; 14/17 direct + 5/7 typed rank-1 envelopes consume re-scan | simdjson stage 1 / stage 2 architecture (substrate-union shape) | low (opposite of REDRESS 96/97/98 — removes parallel substrate, does not add one) |
| C6 | `parse_attribution_envelope_cracker` | profile discipline | YES (Cargo feature, generic) | every dispatch-envelope row (27/34 envelope rank-1) | N/A (profile discipline, not perf primitive) | none (process, not kernel) |
| C7 | `unicode_escape_neon_nibble_decode` | SIMD primitive (Layer 1) | YES (escape syntax from grammar config) | `read_hex_unit_scalar` 100 % on y_string_unicode; `unescape_string` 46.7 % on unicode_escapes | sonic-rs / yyjson scalar (no public SIMD comparator); strict via per-row equality on unicode_escapes | low (REDRESS 82 differential: windowed decode, not single-quartet classifier) |

**Grammar-neutrality count: 7 / 7.** Every candidate generalises beyond JSON to CSS L4 / Sheets / BBNF-self per Lock 14. C2/C3/C7 require grammar-config-supplied policy tables (string terminator/escape/control; number digit/sign/decimal/exponent; escape prefix/width/surrogate-policy); C1/C5 consume a grammar-neutral structural projection; C4 is a codegen invariant; C6 is a profile discipline.

---

## §3 — Grammar-neutrality (per candidate, mapped to CSS L4 / Sheets / BBNF-self per Lock 14)

The CSS L4 generalisation is made **from spec evidence + JSON profile jointly**, without CSS L4 profile corroboration (per dispatch context §1 CH2 F2: zero CSS L4 grammar-neutral primitive evidence at SK-V14; only `declaration_values` profiled, dominated by timer + fact-sink overhead). The Sheets + BBNF-self generalisations are spec-only at SK-V14.

| Candidate | JSON consumer (P1-grounded) | CSS L4 consumer (spec-evidence) | Sheets consumer (spec-evidence) | BBNF-self consumer (spec-evidence) | Grammar-neutral verdict |
|---|---|---|---|---|---|
| C1 `lazy_field_skip_with_index` | direct + typed envelopes skip-value path | skip declaration value to next `;` or `}` (structural set `;{},`) | skip formula token to next `,` or `)` | skip production body to next `;` or `\|` | GENERALISES — structural-set is grammar-config-supplied; skip semantics are grammar-neutral cursor-advance |
| C2 `long_string_body_simd_scan` | JSON string body in `parse_object_value_at_direct` + `parse_array_element_at_direct` | CSS string + URL + selector identifier body | Sheets text literal (with `""`-doubled-quote escape) | BBNF string literal | GENERALISES — terminator + escape + control policy from grammar config; primitive owns no JSON constants |
| C3 `digit_block_simd_accumulate` | JSON number direct decode (canada/mesh/marine_ik/numbers) | CSS `<number>` + `<integer>` + `<dimension>` digit run | Sheets numeric literal + cell reference column index | BBNF numeric literal | GENERALISES — Lock 16 abstract primitive; digit policy + sign + decimal/exponent from grammar config |
| C4 `force_inline_lto_envelope_discipline` | JSON dispatch envelope hot-function ≤ 20 KiB | CSS rule / declaration / value dispatch envelope | Sheets formula evaluation dispatch | BBNF rule dispatch | GENERALISES — Lock 15 already binds the discipline for every grammar's generated runtime; diagnostics (`BBNF-FORCE-INLINE-MISSED`, `BBNF-ICACHE-BUDGET-EXCEEDED`) are grammar-neutral |
| C5 `structural_index_singular_substrate_consumer` | JSON direct + typed envelopes consume `StructuralIndex` | CSS L4 declaration parser consumes CSS structural index | Sheets formula parser consumes formula-structural index | BBNF parser consumes its own structural index | GENERALISES — `StructuralIndex` definition is grammar-neutral; per-grammar consumer code in `runtime/src/grammars/<g>/` is grammar-generated, not generic-crate-branched |
| C6 `parse_attribution_envelope_cracker` | crack JSON dispatch envelope attribution | crack CSS dispatch envelope attribution | crack Sheets dispatch envelope attribution | crack BBNF dispatch envelope attribution | GENERALISES — Cargo feature applies to any generated runtime |
| C7 `unicode_escape_neon_nibble_decode` | JSON `\uXXXX` + UTF-16 surrogate decode | CSS `\41` variable-width hex escape | (not applicable — Sheets has no `\uXXXX`) | BBNF string-literal `\uXXXX` escape | GENERALISES (3/4 grammars) — escape syntax (prefix, width, surrogate handling) from grammar config; primitive owns nibble decode |

**Lock 14 verdict: 7 / 7 candidates pass grammar-neutrality.** None requires JSON-specific naming in a generic crate; none branches on grammar identity in `bbnf-simd` or `bbnf-parse` or `bbnf-runtime` or `parse-that-regex`. C2/C3/C7 require grammar-config policy tables (`StringPolicy`, `NumberPolicy`, `EscapePolicy`) which are GrammarConfig fields per Lock 14 contract; the policy tables are data, not branches.

**CH2 escalation note**: per dispatch context §1 CH2 F2, CSS L4 grammar-neutral primitive evidence is **absent** at SK-V14 (only one row profiled, dominated by timer overhead). The CSS L4 column above is spec-derived per the CH2 binding ("S-P2 generalization argument made on JSON profile + CSS L4 spec evidence jointly, without CSS L4 profile corroboration"). S-P3 sequencing must include CSS L4 corpus + profile work (R4 + R5) before any C2/C3/C7 candidate admits a CSS row.

---

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

Per CH3 + SK-V14 SYNTHESIS §0.4 P-1..P-7 + the pre-block list in dispatch context §3:

### Pre-blocked routes the candidates must not re-open

- **REDRESS 28+33** — Class A NEON tiny-string wiring. C2 (long_string_body_simd_scan) MUST be the long-string envelope consumer, not a tiny-string probe replay. Material differential: tiny-string is `match_tiny_plain_string_with_cap::<16>` (`generated.rs:169`), already wired; long-string is the body scan past the tiny cap. Different envelope.
- **REDRESS 50-55** — SK-V5 UTF-8 fusion. No candidate proposes fusing UTF-8 validation into the parser hot path; UTF-8 validation remains deferred to consumer (`core::str::from_utf8` per P1-B Track 2 serde rank-3 leaves).
- **REDRESS 60-72** — SK-V6 retained-parse + sidecar producers + digest cap-16. No candidate proposes retained parse output or sidecar producer; C1 + C5 consume the existing single substrate.
- **REDRESS 80** — canada mantissa-widen. C3 (digit_block_simd_accumulate) MUST be the generic digit-block primitive, not the canada-specific mantissa widen. Material differential: the primitive emits raw mantissa lanes for any number-heavy corpus; the f64 widening is a downstream Eisel-Lemire pass, not part of the primitive.
- **REDRESS 82-84** — single-quartet unicode classifier, StringBlock16 tiny-probe, object-pair compaction. C7 (unicode_escape_neon_nibble_decode) MUST be the windowed `\uXXXX`+surrogate decode primitive, not a single-quartet classifier replay. C2 MUST be the long-string body scan, not a StringBlock16 tiny-probe replay.
- **REDRESS 88** — PMULL prefix-XOR hot body. NO candidate proposes PMULL prefix-XOR as the hot body. Any S-P3 candidate that lifts simdjson's CLMUL prefix-XOR architecture for string-mask must cite REDRESS 88, name a fresh material differential, and prove same-loop consumer + row movement.
- **REDRESS 89** — CSSC CTZ next-bit bulk consumer. NO candidate proposes CSSC CTZ as bulk-consumer. Any S-P3 candidate that lifts bit-stream scanning past structurals must cite REDRESS 89.
- **REDRESS 96/97/98** — retained class-column / streaming structural cursor / class-lane-only replay. C5 (substrate-union completion) is the **opposite** of these routes: it removes parallel-substrate consumers by re-routing existing consumers to the existing producer. Substrate-target = `existing_tape`; no new retention; no class column. The Lock 1 v+1 contract (`LOCKS.md:73-82`) admits C5 because retention_lifetime = `output_row` and policy_owner = `generated_grammar`.

### REDRESS pre-block surface from dispatch context §3 CH3

The pre-block surface includes the eight REDRESS families above plus the historical blocked routes. Each candidate has been checked against the eight; verdicts in the table above. No candidate re-opens any pre-blocked route without fresh material differential + (where applicable) substrate-union compliance + same-wave consumer + row movement.

### Strictness-plane risks (R1 + P-2)

- **P-2 anti-pattern**: `sonic_rs::from_slice::<Value>` as parse_only / direct / typed comparator. Every candidate primitive's SOTA-beat target must be the **plane-correct** strict comparator (sonic-rs Skipper-class for parse_only; sonic-rs strict struct deser for direct; per-corpus typed struct deser for typed). The audit-falsified rows (5 parse_only + 4 direct narrow / 6 broad + 7 typed narrow / 11 broad = 24 carry-falsified rows per SYNTHESIS §0.2 reconciliation) all cited the P-2 anti-pattern.
- **R1 prerequisite**: the SK-V14 R1 wave (comparator rebind) must land before any candidate admit cites a strict comparator anchor. C1-C7 are **research candidates**; admission requires (a) R1 + R2 (strict comparator + per-iter equality), (b) the candidate's same-wave consumer named in §2, (c) row movement under the strict comparator.
- **CH6 paper-close risk**: a candidate that says "wins against eager-DOM comparator" without strict-vs-strict witness fails CH6. Every candidate's SOTA-beat claim in S-P3 must cite the strict comparator's column in `RESULTS.md`.

### Substrate-union risks (Lock 1 + CH5)

- **C1 + C5** introduce a shared structural-index consumer across direct + typed envelopes. Per Lock 1 v+1 substrate-ceiling fold + the dispatch context §1 substrate-union finding: this is the **honouring** direction (singular substrate). A candidate that proposes a *second* structural-index materialisation (e.g., a typed-plane-specific cursor with its own per-thread index) would violate Lock 1; C1 + C5 explicitly do not.
- **Lock 1 declaration triple** per LOCKS.md:73-82: every candidate consumer carries `substrate_target` + `retention_lifetime` + `policy_owner`. C1: `existing_tape` / `local_loop` / `generated_grammar`; C5: `existing_tape` / `output_row` / `generated_grammar`; C2/C3/C7: `local_temp_only` / `local_loop` / `generated_grammar` (the SIMD masks are transient producers, not retained sidecars); C4/C6: N/A (build invariants).

### Generated-LOC risks (Lock 13 + [generated-size-budget])

- C5 (substrate-union completion) rewrites direct + typed envelope bodies; per `[generated-size-budget]` the LOC delta must trace to O(N) generator regression check. Envelope rewrites are bounded by # of dispatch arms (constant per grammar); admissible per LOC budget if the per-arm consumer is ≤ existing arm size.
- C2/C3/C7 add Layer-1 sub-modules under `bbnf-simd/src/aarch64/` (existing module surface: `string_block.rs`, `digit_mac.rs`, `unescape_uxxxx.rs` — consumer wiring is the gap, not new module creation). LOC delta is bounded.

### CH6 paper-close risks per candidate

- C1: must carry scalar reference fn `skip_value_via_index` before SIMD discussion; checkasm parity vs scalar reference under the existing test infrastructure.
- C2: must carry scalar reference `scan_string_body` with policy-table input; checkasm parity over policy permutations.
- C3: must carry scalar reference `accumulate_digit_run`; checkasm parity over digit-count + overflow + decimal/exponent boundaries.
- C4: must demonstrate fused-function size via `cargo asm`; demonstrate i-cache residency via samply re-record.
- C5: must demonstrate substrate-union honesty via diff-test of parse output (sink callbacks + tape positions) byte-equivalent under index-consuming vs byte-rescanning consumer.
- C6: must demonstrate `parse-attribution` ON flips inner primitive attribution.
- C7: must carry scalar reference `decode_uxxxx_window`; checkasm parity over surrogate state machine.

Per Lock 16 + the dav1d/FFmpeg checkasm process (P2-B scope) every SIMD candidate (C2/C3/C7) must wire scalar reference + checkasm differential + same-wave row consumer before admission. CH4 cost gate is binding.

---

## §5 — Sources

### §5.1 — bbnf source anchors (path:line at HEAD `2547c750b`+SK-V14 V1 dispatch seed)

- `skinny/crates/runtime/src/grammars/json/generated.rs:43-44` — `cfg_attr(feature = "parse-attribution", inline(never))` plumbing
- `skinny/crates/runtime/src/grammars/json/generated.rs:45-237` — 14 `parse-attribution`-gated dispatch functions
- `skinny/crates/runtime/src/grammars/json/generated.rs:45` — `dispatch_value` envelope
- `skinny/crates/runtime/src/grammars/json/generated.rs:159,169` — `match_tiny_plain_string`, `match_tiny_plain_string_with_cap::<16>`
- `skinny/crates/runtime/src/grammars/json/generated.rs:213` — `match_number_at_digit`
- `skinny/crates/runtime/src/grammars/json/generated.rs:466` — `parse_object_value_at_direct`
- `skinny/crates/runtime/src/grammars/json/generated.rs:506` — `parse_array_element_at_direct`
- `skinny/crates/runtime/src/grammars/json/generated.rs:650` — `parse_number_direct`
- `skinny/crates/runtime/src/grammars/json/scan.rs:22` — `scan_structurals` (SIMD)
- `skinny/crates/runtime/src/grammars/json/scan.rs:32` — `scan_structurals_scalar` (scalar reference)
- `skinny/crates/runtime/src/grammars/json/scan.rs:107` — `scan_tail`
- `skinny/crates/runtime/src/grammars/json/scan.rs:131` — `scan_tail_byte`
- `skinny/crates/parse-that-regex/src/lib.rs:718` — `unescape_string`
- `skinny/crates/parse-that-regex/src/lib.rs:945` — `read_hex_unit_scalar`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:516,1150,1330,2197,2949` — typed monomorphisations + `DirectParser::skip_value`
- `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:2` — `bulk_emit_positions_64_neon`
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs` — Layer-1 string-body SIMD scan (consumer wiring is gap)
- `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs` — Layer-1 UDOT/SDOT digit-block (consumer wiring is gap)
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs` — Layer-1 nibble-classify (consumer wiring is gap)
- `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs` — Layer-1 set-membership (consumer wiring is partial)

### §5.2 — Prior tranche evidence (binding context)

- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md §2.1-§2.4` — hot leaf census 17/17 × 3 planes + mode III
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md §4.1` — CH2 Lock-14 mis-attribution census (envelope dominance)
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md §4.4` — substrate-union finding (DirectParser cursor + parse_object_value_at_direct cursor structurally independent)
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md §2 direct_to_struct + real_typed_struct tables` — per-corpus per-track rank-1/2/3 leaves
- `restart/skinny/tranches/sk-v14/research/p1/S-P1-DISPATCH-CONTEXT.md §0-§7` — S-P1 binding
- `restart/skinny/tranches/sk-v14/research/p2/S-P2-DISPATCH-CONTEXT.md §0-§5` — S-P2 binding (this dispatch)
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md §0.1 R10 close condition` + `§0.2 goalset` + `§0.3 R1-R10` + `§0.4 P-1..P-7 pre-blocks` + `§2 telemetry binding` — durable SK-V14 contract
- `restart/skinny/tranches/sk-v14/HANDOFF.md §3 honest baseline` + `§7 refusal conditions` — tranche handoff
- `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` — S-P0 prune list (74 findings; 40 audit-falsified rows)
- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md §22-46,§78-94` — addendum strict-vs-strict bar
- `restart/locks/LOCKS.md` — Lock 1 (substrate union + v+1 substrate-ceiling fold + declaration triple at lines 73-82); Lock 13 (no god directories); Lock 14 (grammar-neutral primitives at lines 220-263); Lock 15 (LTO + force-inline + i-cache ≤ 20 KiB at lines 265-280; yyjson reference); Lock 16 (SIMD/ASM allowlist + abstract primitives at lines 282-365)
- `restart/audit/totality/p2/2A-sota-landscape.md` — T-P2 V3 SOTA technique grounding table + LOCKS-AMENDMENT candidates T2A-LAC-01..05
- `restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md` — SK-V13 P2-A prior synthesis (carry-through; 7 candidate C1-C7 superseded by SK-V14 SOTA-keyed redesign)
- `skinny/RESULTS.md:3,145-149` — comparator column provenance (native Rust sonic same-run; C++/DOM sidecars historical/absent)
- `skinny/REDRESS.md:2823-2940` (REDRESS 96/97/98); `:3506-3544` (REDRESS 119); `:3603-3632` (REDRESS 122 escape_mask_64); `:3766-3820` (REDRESS 126 W4 ASCII run-skip); `:3824-3840` (REDRESS 127 CSS declaration-values W1b)

### §5.3 — External SOTA sources (audit-pack-pinned upstream HEADs per `audit/totality/p2/2A-sota-landscape.md` source register)

- **simdjson HEAD `168ef580757d75270475b379e83c2b39787a6765`** (verified 2026-05-21):
  - `doc/parse_many.md:54-57` stage 1 / stage 2: https://github.com/simdjson/simdjson/blob/168ef580757d75270475b379e83c2b39787a6765/doc/parse_many.md#L54-L57
  - `doc/basics.md:343-350` On-Demand iterator: https://github.com/simdjson/simdjson/blob/168ef580757d75270475b379e83c2b39787a6765/doc/basics.md#L343-L350
  - `doc/ondemand_design.md:71-89` skip/use-specific parsing: https://github.com/simdjson/simdjson/blob/168ef580757d75270475b379e83c2b39787a6765/doc/ondemand_design.md#L71-L89
  - `include/simdjson/implementation.h:40-75` runtime CPU dispatch: https://github.com/simdjson/simdjson/blob/168ef580757d75270475b379e83c2b39787a6765/include/simdjson/implementation.h#L40-L75
  - VLDB paper: Langdale, G. + Lemire, D. "Parsing Gigabytes of JSON per Second." VLDB Journal 28(6), 2019. https://arxiv.org/abs/1902.08318
- **sonic-rs HEAD `03545a9530346fe279b674dd496e037d94204bc5`** (verified 2026-05-21):
  - `README.md:60-66` targeted SIMD + rejection of two-stage: https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/README.md#L60-L66
  - `README.md:78-90` direct struct + lazy value + raw number: https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/README.md#L78-L90
  - `docs/benchmark_aarch64.md:1-15` M1 Pro twitter/citm anchors: https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/docs/benchmark_aarch64.md#L1-L15
  - `docs/benchmark_aarch64.md:140-151` field lookup: https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/docs/benchmark_aarch64.md#L140-L151
- **yyjson HEAD `95f4c61bc1e24176f2aa4f430902705a995f1c97`** (verified 2026-05-21):
  - `README.md:10-19` ANSI C / no explicit SIMD / RFC 8259 strict: https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/README.md#L10-L19
  - `README.md:73-78` ILP + branch predictor preference: https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/README.md#L73-L78
  - `src/yyjson.h:736-744` strict default flags: https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.h#L736-L744
  - `src/yyjson.h:759-837` non-standard opt-in flags: https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.h#L759-L837
- **asmjson crate `0.2.5`** (docs.rs published; no maintained upstream repository found):
  - `README.md:7-12` 64-byte AVX-512BW/SWAR classifier: https://docs.rs/crate/asmjson/0.2.5/source/README.md#7
  - `README.md:100-113` x86-only assembly + SAX-vs-DOM: https://docs.rs/crate/asmjson/0.2.5/source/README.md#100
  - `README.md:209-222` conformance caveats (permissive controls in strings): https://docs.rs/crate/asmjson/0.2.5/source/README.md#209
  - `README.md:295-300` 64-byte AVX-512/SWAR classifier (alt anchor): https://docs.rs/crate/asmjson/0.2.5/source/README.md#L295-L300
  - `README.md:321-329` runtime selection + unsafe AVX-512 entry: https://docs.rs/crate/asmjson/0.2.5/source/README.md#L321-L329
  - `README.md:457-470` output / API surface: https://docs.rs/crate/asmjson/0.2.5/source/README.md#L457-L470
- **Yet-another ISA + SOTA citations** (Lock 16 allowlist anchors used in candidate primitives):
  - Lemire, D. 2019. "Lookup-table based byte classification using vqtbl4q_u8." (vqtbl4q_u8 + vqtbl1q_u8 references)
  - Validark 2024. "Interleaved Vectors on ARM." validark.dev/posts/interleaved-vectors-on-arm/ (vld4q_u8 + per-channel classify; simdjson PR #2333; ~10 % drop in simdjson stage1 c/B on Apple arm64)
  - Lemire, D. 2026. "The fastest way to match characters on ARM processors." (NEON port of SVE2 `svmatch_u8` via `vceqq_u8` + `vorrq_u8` reduction tree)
  - WikiChip VPCLMULQDQ; BranchFree.org 2019 "Quote pairs with PCLMULQDQ"; Linux kernel CRC-32C reaches 45-60 GB/s vs ~7-8 GB/s SSE4.2 (prefix-XOR primitive at 4× width)
  - Lemire, D. 2023. "Parsing integers quickly with AVX-512" (vpdpbusd byte×byte→i32 digit-block accumulate; arm64 UDOT/SDOT counterpart)
  - dav1d HEAD `1718ff9aded99f0a89f5c7940d6afb8948301e33`:
    - `src/arm/cpu.c:87-95` Apple aarch64 feature detection: https://github.com/videolan/dav1d/blob/1718ff9aded99f0a89f5c7940d6afb8948301e33/src/arm/cpu.c#L87-L95
    - `tests/checkasm/loopfilter.c:177-188` call-ref/call-new/check/bench shape (the process P2-B documents): https://github.com/videolan/dav1d/blob/1718ff9aded99f0a89f5c7940d6afb8948301e33/tests/checkasm/loopfilter.c#L177-L188
  - FFmpeg HEAD `085714182302333dd83dcb9c36cf828dc4eba929`:
    - `tests/checkasm/checkasm.h:214-240` reference/new call macros: https://github.com/FFmpeg/FFmpeg/blob/085714182302333dd83dcb9c36cf828dc4eba929/tests/checkasm/checkasm.h#L214-L240
- **Arm Architecture Reference Manual** (Armv8.2-A: UDOT/SDOT; SHA3 extension: `vbcaxq_u8`, `veor3q_u8`; A64 ISA: `vld1q_u8_x4`, `vextq_u8`, `vbslq_u8`)

### §5.4 — Required reads (binding context for S-P2)

- `restart/prompts/skinny/PASS-2-RESEARCH.md` — S-P2 contract; §2 scope matrix row P2-A; §2.1 frontmatter; §3 CH1-CH6; §7 hard caps; §8 bbnf-lang specifics (item 1 = strict-vs-strict discipline, binding)
- `restart/prompts/ORCHESTRATOR.md §3W + §3Z + §8 non-negotiables` (Lock 1 substrate union; Lock 14 grammar-neutrality; scalar-reference + checkasm; same-wave consumer)
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7`
- `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` + `SYNTHESIS.md` + `HANDOFF.md` (durable SK-V14 contract)
- `restart/locks/LOCKS.md` — 16 locks; Lock 1 + Lock 14 + Lock 15 + Lock 16 load-bearing for P2-A
- `restart/skinny/tranches/sk-v14/research/p1/` — six S-P1 artefacts (P1-A through P1-F + this synthesis's P1-E primary)
- `skinny/RESULTS.md` — bench gate authority
- `skinny/REDRESS.md` — rejected-route ledger (CH3 binding)
- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md` — full-SOTA addendum + addendum strict-vs-strict bar

### §5.5 — Carry-through note (V1 dispatch posture)

This is the SK-V14 P2-A V1 artefact. SK-V13 P2-A (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md`) is the prior synthesis; SK-V14 P2-A supersedes its candidate list (SK-V13 C1-C8) with the seven-candidate set re-keyed to S-P1 hot leaves under R1 strict-vs-strict discipline + the audit-overlay verdict per row. The substrate-union candidate (C5 here, related to SK-V13's C6 `same_loop_structural_mask_consume`) is re-framed: SK-V14 C5 *removes* parallel substrate by re-routing existing consumers to existing producers (substrate-target = `existing_tape`); SK-V13 C6 was the same-loop mask consumer. The grammar-neutral lift is preserved; the Lock 1 framing tightens under the v+1 substrate-ceiling fold + the dispatch context §1 substrate-union finding.

S-P2 P2-D will interrogate the substrate union; this P2-A teardown frames the simdjson + sonic-rs + yyjson architectural pressure that motivates the union completion. S-P2 P2-F will adjudicate the grammar-neutrality of each candidate against CSS L4 + Sheets + BBNF-self spec evidence (per dispatch context §4 P2-F binding); this P2-A teardown provides the §3 spec-derived generalisation table P2-F refines.
