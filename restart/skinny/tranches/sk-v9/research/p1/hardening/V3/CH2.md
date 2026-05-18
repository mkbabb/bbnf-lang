# SK-V9 S-P1 V3 CH2 — Generality / Lock 14

Disposition: **REVISE** (4 of 6 reports REVISE; 2 ACCEPT; aggregate Lock 14 hygiene below the convergence bar — three load-bearing leaks must fold into V4).

## §1 — Method (Lock 14 audit protocol)

Lock 14 (`restart/locks/LOCKS.md:60`) forbids JSON-specific code, types,
modules, or match-arms in any generic crate; it admits per-grammar
deviation **only** via three declarative surfaces (grammar source +
workspace metadata + optional declaration crate). The skinny track is
permitted to capture JSON-empirical evidence, but every finding it
surfaces as a *primitive class* — one that S-P2 / S-P3 will fold into
the substrate ceiling — must be expressible against the
**StructuralAlphabet** abstraction
(`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md`
§4) and the broader grammar-neutral primitive vocabulary
(`restart/ARCHITECTURE.md` §1 substrate-union; Lock 16 admissibility).

A V3 report respects CH2 GENERALITY iff:

- (G1) Every named hot leaf surfaced as a primitive class is framed as
  *what the byte loop is doing* (per-string-span scanner; structural
  walker over StructuralAlphabet ordinals; escape codec), not as *its
  JSON-named symbol identity* used as a primitive antecedent.
- (G2) Correlations derived from JSON corpus structure are admitted as
  per-grammar instantiations of a substrate-neutral cost law (per-span
  delimiter cost; structural-element density vs lift), not as JSON
  facts.
- (G3) Wave names + S-P2/S-P3 antecedents reference primitive classes
  (string-plane / escape-codec / unicode-validation / digit-FSM /
  structural-emit), not JSON-named symbols or grammar-specific roles.
- (G4) Cleanup verdicts (SAFE-TO-DELETE / KEEP-IF-USED) audit consumer
  shape, not consumer name; a primitive consumed only by the JSON
  generated runtime can still be admissible if the *primitive class* is
  load-bearing for any future grammar that admits the same shape.
- (G5) Method blocks (capture protocols, probe binaries, classifier
  taxonomies, corpus iteration counts) do not bake `track1|track2` /
  `<corpus>.json` as a *protocol invariant* — the same harness shape
  must extend to CSS / Sheets / BBNF-self / user-grammar corpora when
  S-P2 lands.
- (G6) Pre-blocked-route umbrellas in HANDOFF §5 are stated as
  *substrate-shape* rejections (e.g. "retained string-scan widening"
  rather than "JSON string-scan widening"), so any future grammar that
  admits a delimited-span scan is bound by the same pre-block.

The audit reads each report against G1–G6, distinguishing **report
authorial framing** (where the writer's prose chose JSON-role names)
from **derived-evidence framing** (where the *evidence itself*
generalises and only the writer's caption is JSON-tinted). The latter
admits a paragraph-level redress in V4; the former requires the report
itself to be re-cast.

## §2 — Per-report disposition

### §2.1 — P1-V3-A (xctrace CPU Counters PMU capture) — ACCEPT

| # | Locus | Audit | Verdict |
|---|---|---|---|
| A.1 | §1.1 Probe binary (`<track:track1|track2>`) | The probe binary takes a 2-position CLI: `<corpus_path> <track:track1|track2> <iters>`. `track1` is `runtime::generated_json::parse`; `track2` is `bbnf_bench::track2::json::parse`. The probe *itself* is grammar-neutral (it imports two `parse` entry points and counts cycles around them) — the JSON-naming is contained inside the symbols the probe imports, not in the harness shape. G5 admits with a forward note: the same probe shape generalises by adding a third arm for CSS L4's generated parse entry. | ACCEPT — generalises by trivial probe-binary extension; no structural rewrite needed. |
| A.2 | §2 PMU table header (cycles/B, CPI as a metric class) | Cycles, instructions, CPI, cycles/B are hardware counters; the metric class is grammar-neutral by construction. The 17-row table tags each row by `<corpus>`, but the *measurement* (PMU read across a steady-state loop) is the same shape any grammar's parse-only loop admits. | ACCEPT. |
| A.3 | §3 cross-validation against samply / dispatch_value | The report cites `runtime::generated_json::generated::dispatch_value` as the single hot leaf at 95.6–99.6%. Per Lock 14 verification rules: `dispatch_value` is generated code inside `runtime/src/grammars/json/generated.rs` — a per-grammar runtime module, which is admissible. The report treats `dispatch_value` as a *fused dispatch surface*, not as a primitive — that framing is G1-compliant. | ACCEPT. |
| A.4 | §4 hot-leaf hint framing ("no per-corpus cohort has a named secondary hot leaf above 1% self-time on parse_only/track1") | The conclusion is stated against the JSON corpus but expressed as a measurement fact about the fused parse-only surface, not a JSON primitive. The "what S-P2 needs to break apart at a sub-symbol granularity" framing names *sub-symbol granularity*, not JSON. | ACCEPT. |
| A.5 | §6.4 "what this report does deliver" + reproduction script | The artefacts listed (`.trace` artefacts, cycles/B per row, Time Profiler traces) are all per-row reads of a launchable probe binary; the discipline ("real PMU vs ns-estimation") is grammar-neutral. | ACCEPT. |
| A.6 | Iteration count table (§1.2) | The 17 iteration counts are JSON-corpus-specific tuning. This is empirical configuration, not a primitive claim; the *method* of "tune iterations so each capture occupies 0.5–3 s steady state" is grammar-neutral. | ACCEPT. |

Aggregate: P1-V3-A's PMU evidence is the most grammar-neutral artefact
in the cohort. The probe binary's `<track1|track2>` CLI is the only
JSON-coupling, and it is contained inside the launched symbols, not the
harness shape. No V4 fold required.

### §2.2 — P1-V3-B (xctrace Time Profiler cross-validation) — REVISE

| # | Locus | Audit | Verdict |
|---|---|---|---|
| B.1 | §1.5 classifier taxonomy (`string_tiny_scan / string_full_scan / number_digit_scan / scan_structurals / simd_movemask / consume_structural / …`) | The classifier is **explicitly** grammar-neutral by construction: "it matches symbol substrings, not JSON-role names." The class names (`string_tiny_scan`, `string_full_scan`, `unicode_escape_hex`, `number_digit_scan`, `whitespace_skip`, `simd_movemask`, `object_walk`, `array_walk`, `consume_structural`) are the **right** primitive vocabulary — they map directly to StructuralAlphabet primitive classes. G1 ACCEPT. | ACCEPT (this is the model the other reports should follow). |
| B.2 | §2 per-symbol tables — captioning `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>` | The symbol *name* is JSON-namespaced (lives in `runtime/src/grammars/json/`); the symbol *role* under the classifier is `string_tiny_scan` — a substrate-neutral primitive class. The B classifier itself preserves G1. But the prose discussion in §3.2 and §3.4 names the JSON symbol identity ("the tiny scalar path dominates, the full SIMD scanner is in the tail") as if `match_tiny_plain_string` were itself the primitive antecedent rather than the per-grammar realisation of `string_tiny_scan`. | REVISE — re-cast §3.2/§3.4 prose to lead with `string_tiny_scan` (primitive class) and cite `match_tiny_plain_string` as the JSON realisation. Trivial paragraph rewrite. |
| B.3 | §3.2 — "string scanner pair carries ~75% of self-time on loss corpora" verdict | The verdict is framed as a JSON-corpus phenomenon. The generalisation: the **string-plane** (any per-grammar delimited-span scanner — JSON `"..."`, CSS L4 `"..."` + `'...'` + `url(...)`, Sheets `"..."` with `""` escape, BBNF-self string literals) carries the dominant share on grammars whose corpus is delimited-span-heavy. SC-6 §4.3's CSS L4 StructuralAlphabet already enumerates two string delimiters; the same correlation pattern is expected on CSS. The verdict needs to be stated as: "string-plane (per the grammar's StructuralAlphabet string-delimiter set) carries ~50–70% of self-time on delimited-span-heavy corpora; JSON's `q_frac ≥ 0.726` rows are the JSON instantiation." | REVISE — the §3 verdict block must restate the SC-4 dispatch in terms of *string-plane class* (a substrate primitive class), with JSON-row evidence as the per-grammar realisation. This is a load-bearing CH2 leak: a future CSS L4 / Sheets agent reading this section as written would not see that `string_tiny_scan` is a class spanning all delimited-span grammars. |
| B.4 | §3.2 — `read_hex_unit_scalar` + `hex_nibble` "unicode-escape codec" for y_string_unicode | These two symbols implement `\uXXXX` decoding. The unicode-escape codec class is **not** JSON-specific: CSS L4 admits `\HHHHHH` escapes (CSS Syntax 4.3.7 Consume an escape), JS-like grammars admit `\uXXXX` (and `\u{HHHHHH}` in strict mode), TOML admits `\uHHHH` + `\UHHHHHHHH`. The report names "the unicode-escape codec (`read_hex_unit_scalar` + `hex_nibble`)" but does not frame it as a primitive class spanning grammars. The new primitive class — `escape_codec_hex_unit` or `unicode_escape_kernel` — must be introduced explicitly. | REVISE — introduce `escape_codec_hex_unit` as the substrate-neutral primitive class, of which JSON's `\uXXXX` is one realisation; cite SC-6 §4 generalisation as the binding. |
| B.5 | §3.4 — "the V2 samply top-self-time table is **not measurable hot-leaf attribution** for the LTO-fused generated parser" | The framing names "LTO-fused generated parser" generically; the methodological conclusion (samply's frame-pointer walk coalesces inlines into the outer symbol; xctrace's DWARF inlined-frame walk resolves them) is grammar-neutral and applies to any LTO-fused generated parser the codegen emits. | ACCEPT. |
| B.6 | §5.3 — `direct_to_struct` route discussion ("the route does not change which leaf is hot; the mass shifts from scan-only to scan + materialise") | The "route" concept (parse-only vs sink-projection vs typed-projection) is substrate-neutral; the route is what `BackendShape` enumerates (`EagerTape` / `OffsetTape` / `EventTape` / `SinkOnly` / `CollapsedStage`). The discussion does not name JSON-roles; it names route shapes. | ACCEPT. |
| B.7 | §3.1 SC-1 verdict — `scan_structurals` is wasted on parse_only | The verdict is stated as a JSON-evidence claim ("0.00% on all 17 × 2 = 34 rows"). The **generalisation** (G2): for any grammar whose generated parse_only path re-derives every structural byte inside `dispatch_value`-shape recursive descent, the SIMD structural-scan output is unconsumed. That is the substrate-shape claim. The report should add one sentence: "this is a substrate-shape verdict — it generalises to any grammar where the generated parse_only surface re-derives StructuralAlphabet ordinals inside the fused recursive-descent body." | REVISE (one-paragraph fold). |
| B.8 | §5.3 — mode-II direct route artefact discussion (cycle-precision via primitive class) | The §5.3 derivation `primitive_class_cycles_per_byte ≈ row_cycles_per_byte × primitive_class_%` is **explicitly** primitive-class-keyed and G1-compliant. | ACCEPT (model). |

Aggregate: B's classifier is the canonical CH2 GENERALITY model for the
cohort. The prose discussion of verdicts (§3.2, §3.4) leaks JSON-role
identity in places where the classifier would have surfaced the
primitive class. V4 must re-cast verdict prose around the
already-correct classifier vocabulary; the *data* itself does not need
re-capturing.

### §2.3 — P1-V3-C (per-corpus deep hot-leaf attribution) — REVISE

| # | Locus | Audit | Verdict |
|---|---|---|---|
| C.1 | §1.3 classifier taxonomy (`structural_scan / string_scan / number_parse / escape_handling / tape_write / allocation / sync_overhead / traversal_other`) | The 8-class set is substrate-neutral by construction. The substring matchers (`match_tiny_plain_string`, `match_string_at_quote`, `skip_string_plain`, `string_special_mask`, `scan_string_special_block`, `string_body_range`, `parse_string`, `parse_key_colon`, `StringSpecial`) map JSON-specific symbol names into the neutral class. G1 ACCEPT for the classifier itself. | ACCEPT (this is the right shape). |
| C.2 | §4 SC-1 verdict — "the named SIMD scan symbols (`runtime::generated_json::scan::scan_structurals`, `neon::scan`, `bulk_emit_positions_64_neon`, `bitmap_prefix_xor_64_neon`)" | All four symbols live under JSON-named paths (`runtime/src/grammars/json/scan.rs`). Per Lock 14 verification: these are generated/per-grammar modules and admissible there. The *verdict* (SC-1 non-fusion of structural-scan output) is grammar-neutral — for any grammar admitting a SIMD structural scan whose output is then re-derived by `dispatch_value`-shape recursive descent, the same diagnosis holds. The report does not state this generalisation. | REVISE — add one paragraph in §4 stating: "this is a substrate-shape diagnosis; any future grammar whose generated parse_only path re-derives StructuralAlphabet ordinals inside fused recursive descent will exhibit the same `scan_structurals`-discarded pattern. The verdict is **not** JSON-specific." |
| C.3 | §5 SC-4 verdict — string-plane 75% share by `q_frac` (quotes / element_tokens) | "Quotes" is a JSON role for the substrate-neutral class "string-delimiter bytes" (the StructuralAlphabet's `string delimiters` field, SC-6 §4.1). On CSS L4 the analogous q_frac is `(delim_count for "..." + delim_count for '...' + delim_count for url(...))/element_tokens`. The report's `q_frac`-keyed correlation is the JSON instance of a per-grammar law: **delimited-span density correlates with string-plane self-time share, modulated by mean span length and escape density**. | REVISE — restate §5.1–§5.3 in terms of *delimited-span density* (the substrate-neutral quantity) with `quotes/element_tokens` as the JSON instantiation. Cite SC-6 §4.1 string-delimiter byte set. |
| C.4 | §5.4 Pearson + Spearman correlation (r=+0.720, ρ=+0.755) on string fraction × T1-defused string-class share | The statistic is sound, but the *naming convention* is "string fraction" rather than "delimited-span density (per-grammar StructuralAlphabet string-delimiter)". Same load-bearing leak as C.3. | REVISE (same paragraph). |
| C.5 | §6 "where V2 was wrong or shallow" item 4 — "V2 did not classify into the agreed structural-class taxonomy" | The 8-class taxonomy is grammar-neutral and the §6.4 framing is correct. But the *evidence* (which row hits which class) is JSON-specific by construction — no CSS L4 / Sheets corpus is profiled. The report does not say so. | REVISE — add a sentence: "the 17-row evidence is JSON-empirical; the classifier vocabulary is substrate-neutral and will accept the same 8 classes on future CSS/Sheets/BBNF-self corpora." |
| C.6 | §2.2 direct-to-struct row evidence — `parse_object_value_at_direct` / `parse_array_element_at_direct` / `JsonDigestSink::array_string` | These three symbols are JSON-specific *symbol names* but represent substrate-neutral primitive classes: `parse_struct_field_at_direct` (one per shape), `parse_repeated_element_at_direct` (one per shape), and `direct_sink_callback` (one per sink). The classifier buckets all three into `traversal_other`. That bucket name is too coarse; it hides the fact that the direct-route hot leaves are structurally the **field-projection** + **repeated-projection** + **sink-callback** classes. | REVISE — introduce three sub-classes under `traversal_other`: `direct_field_projection`, `direct_repeated_projection`, `direct_sink_callback`. JSON's `parse_object_value_at_direct` realises the first; CSS L4's declaration-list direct route would realise the same class. |
| C.7 | §5.2 corpus row table — uses `unescape_string` as the named escape-class hot leaf on unicode_escapes (47.5%) | The `unescape_string` symbol from `parse-that-regex` is generic crate code (`parse-that-regex/src/lib.rs:718`). The *class* is `escape_codec` — the per-grammar escape-decoding primitive. CSS L4's CSS Syntax 4.3.7 escape consumer will realise the same class. | REVISE — re-cast §5.2 captions around `escape_codec` (primitive class). |

Aggregate: C's 8-class classifier is the right substrate-neutral
vocabulary, but C's verdict prose leans on JSON-role surface phrases
("quotes", "string fraction", "the string class") where the
substrate-neutral terms (`string-delimiter density`, `string-plane
self-time`, `delimited-span scanner`) would have served. V4 fold is a
prose pass plus one sub-class refinement under `traversal_other`.

### §2.4 — P1-V3-D (structural breakdown vs throughput) — REVISE

| # | Locus | Audit | Verdict |
|---|---|---|---|
| D.1 | §1 correlation table — `q/B`, `n/B`, `sd`, `q_frac` | These are JSON-corpus structural densities. They map to substrate-neutral quantities: `string_delim_density`, `digit_density`, `structural_alphabet_density`, `delimited_span_fraction`. The mapping is one-to-one once SC-6's StructuralAlphabet is read as the abstraction. The report does not state the mapping. | REVISE — add a one-paragraph "Generalisation" note at §1 stating the per-row column names are JSON instantiations of substrate-neutral StructuralAlphabet-keyed densities. |
| D.2 | §2 SC-4 step-function (`q_frac ≤ 0.135` WIN / `q_frac ≥ 0.726` LOSS) | Same JSON-naming as D.1. The step function is a JSON instance of a per-grammar **delimited-span-fraction admission predicate**. CSS L4 and Sheets will have their own thresholds against their own delimited-span fractions; the *shape* (a step function in delimited-span fraction) is the substrate-neutral law. | REVISE — re-cast §2's "string-quote-density verdict" as "delimited-span-fraction verdict" with JSON-quotes as the JSON realisation. |
| D.3 | §2.3 banded-mean table + OLS regression in §5: `ns_per_byte = 8.64 * (quotes / bytes) + 1.47 * (numbers / bytes) + 0.410` | The OLS is empirical-JSON and the **coefficients are JSON-specific** (8.64 ns/quote is conditional on JSON's escape-complete rules + UTF-8 boundary check). But the **structural form** — `ns_per_byte = sum(class_marginal * class_density) + baseline` — is substrate-neutral. The "per-quote ~21× baseline" is JSON's instantiation of "per-string-span-delimiter ~21× baseline (under this grammar's escape-complete rules)"; on CSS L4 the per-`(` cost (function-call open) and per-`"` cost (delimited string) will be separate columns. | REVISE (this is the most load-bearing CH2 leak in the cohort): re-frame §5 as a per-grammar cost-law instantiation. JSON's OLS row stands; the generalisation note must say "for any grammar, the per-StructuralAlphabet-byte marginal cost is a separate column in the OLS; this report fits the JSON column set." Cite SC-6 §4 + Lock 16 admissibility. |
| D.4 | §5.4 "structural opens cost nearly free under the lazy tape" | The verdict generalises directly: any grammar whose StructuralAlphabet's structural-open ordinals are emitted to a lazy offset tape will see the same near-free cost. CSS L4's `{` / `(` / `[` opens admit the same lowering. The report does not state this. | REVISE — one sentence in §5.4 stating "this verdict is substrate-shape; any grammar admitting an OffsetTape lowering of structural opens inherits the same near-free cost." |
| D.5 | §5.5 "numbers are bbnf's currently strongest sub-plane" | "Numbers" is the JSON instance of the substrate-neutral **digit-FSM-emittable atom class** (any grammar whose atoms admit a finite-state digit scanner: JSON numbers, CSS L4 `<number>` + `<integer>` + `<percentage>` + `<dimension>`, Sheets numeric cells, BBNF-self integer literals). The report names it "the number FSM" without generalising. | REVISE — one sentence: "the digit-FSM primitive class is substrate-neutral; JSON's `parse_that_regex::number::scan_digit_run` realises it. CSS L4's number atom set admits the same FSM." |
| D.6 | §6.1 Wave V9-W1 — "string-plane cost cut (per-quote ~10–15%)" | "Per-quote" is the JSON instance of "per-string-delimiter". The V9-W1 framing should be "string-plane (delimited-span scanner) cost cut" — a substrate-neutral wave that admits CSS L4 / Sheets when those grammars land in skinny. As written, "per-quote" reads as a JSON-only wave. | REVISE — wave names: V9-W1 = "string-plane / delimited-span scanner cost cut"; V9-W2 = "digest-sink truth pass (sink-callback class)"; V10 = "unicode-validation kernel (escape-codec class)". |
| D.7 | §6.2 Wave V10 — "unicode validation kernel" | Same as D.6: the unicode-validation kernel class spans every grammar admitting `\uXXXX`-equivalent escapes. Currently framed as "for these rows" (unicode_mixed, unicode_escapes) — JSON-corpus-named. | REVISE — re-cast as "the unicode-validation kernel primitive class; JSON's escape-heavy corpora realise it; CSS L4 / Sheets escape paths will admit the same kernel." |
| D.8 | §4 direct-plane decorrelation from q/B (r = −0.033) | The finding is grammar-neutral: the direct-route (sink projection) cost is *byte-walk dominated, not structural-element dominated*. This is a substrate-shape claim and applies to any grammar with a `SinkOnly` route. The report says "the q_frac step function only describes the parse_only plane" — close to general, but stops short of naming the substrate-shape rule. | REVISE — one sentence: "this is a route-class verdict (BackendShape SinkOnly has byte-walk cost, BackendShape OffsetTape has structural-element cost); any future grammar admitting both routes will exhibit the same decoupling." |

Aggregate: D contains the cohort's two most load-bearing CH2 leaks
(D.3, D.6). The "per-quote ~21× baseline" coefficient and the V9-W1 /
V9-W2 / V10 wave names are presented as JSON facts where they are JSON
instantiations of substrate-neutral laws. V4 fold is a re-framing pass
(no new evidence, no new measurements); the OLS table can keep its
JSON-named columns provided the §5 prose names the generalisation
above the columns.

### §2.5 — P1-V3-E (legacy cleanup audit) — REVISE

| # | Locus | Audit | Verdict |
|---|---|---|---|
| E.1 | §2.2 `aarch64/match_tiny_plain_string.rs` SAFE-TO-DELETE per REDRESS 28+33 + REDRESS 72 | The verdict cites: (a) NEON kernel has no production caller; only test consumers; (b) the admitted scalar shape at `runtime/src/grammars/json/generated.rs:171-185` is a 4-line scalar loop. **However**: the SAFE-TO-DELETE audit asks "does any non-JSON grammar in the corpus use this primitive?" — and at present the corpus has only JSON. The deletion is correct *for the current corpus*, but the **primitive class** (`string_tiny_scan` per the B classifier) is substrate-neutral and any future grammar whose StructuralAlphabet admits short-delimited-span scanning may want the NEON kernel. The audit does not record this. | REVISE — E.2 must add a "primitive-class preservation note": deletion is corpus-scoped admissible; if a future grammar (CSS L4 ident scan; Sheets short-string scan) admits the `string_tiny_scan` class with NEON-attractive parameters, the kernel can be resurrected from git history. As a SAFE-TO-DELETE this is fine; as a SAFE-TO-DELETE *as a primitive class* it is not — the audit needs to distinguish "delete the JSON-bound wiring" from "retire the primitive class". |
| E.2 | §2.3 aarch64 utility orphan deletions (`quad_load.rs`, `byte_context.rs`, `digit_mac.rs`, `cache_hints.rs`) | Same shape as E.1: the audit names each deletion against current consumers. `digit_mac` cites REDRESS 80 (mantissa-widen rejected); that REDRESS rejection is JSON-corpus-evidence-driven ("zero-fallback canada"). A future CSS L4 / Sheets corpus with different digit distributions could re-admit the family. The audit must note: SAFE-TO-DELETE is corpus-scoped, not class-retiring. | REVISE — add a one-sentence per-deletion class-preservation note. |
| E.3 | §2.4 LIVE primitives "KEEP" list (`byte_class_from_eq_set_64`, `byte_class_from_table_64`, `bitmap_prefix_xor_64`, `bulk_emit_positions_64`, `eob_pad_clamp`, `classify_tbl4`) | These are **explicitly grammar-neutral primitives** per Lock 16's allowlist + SC-6 §4 (Lock 16's `BYTE_CLASS_FROM_EQ_SET_64` takes the byte set as a 64-byte argument, not a hardcoded constant). The KEEP verdicts are correct and CH2-aligned. | ACCEPT. |
| E.4 | §2.4 `aarch64/unescape_uxxxx.rs` KEEP rationale ("the LIVE consumer is the materialization path in `unescape_four_unicode_escapes` — a different surface") | The kernel implements `\uXXXX → utf-8` decoding — the **escape-codec primitive class**. The audit names "REDRESS 64+82 rejected the *single-quartet retained validator route*; the LIVE consumer is the materialization path" — correct rejection-vs-admission framing, but the **class** is unicode-escape codec, substrate-neutral. CSS L4's `\HHHHHH` escape consumer would consume the same kernel under codegen-emitted parameter binding (number of hex digits parameterised, surrogate-join policy parameterised). | REVISE — add a "primitive-class generalisation note" to E.2: this kernel realises the `unicode_escape_kernel` / `escape_codec_hex_unit` class; CSS L4 and JS-like grammars admit the same class under parameter binding. |
| E.5 | §2.1 x86_64 orphan kernel SAFE-TO-DELETEs (14 unimplemented! shells) | These are stubs from Wave 6 with no scalar reference, no consumer, no checkasm. Lock 16 requires scalar + consumer + checkasm; without those, they are not primitives — they are placeholders. SAFE-TO-DELETE is unambiguously correct and CH2-neutral. | ACCEPT. |
| E.6 | §3 path-pattern triage (`GRAND-SYNTHESIS-SK-V5`, etc.) | These are doc-tree path rewrites; no grammar coupling. | ACCEPT (out of CH2 scope). |
| E.7 | §6 R-risks (R1–R9) | R1 (`string_block::scan_string_special_block` KEEP-IF-USED) and R2 (`unescape_uxxxx_x4_neon` KEEP) name "a different surface" / "the materialization path" as the LIVE-consumer rationale. The "surface" naming is substrate-shape; the verdicts hold. But neither R1 nor R2 names the primitive class explicitly — same observation as E.1/E.4. | REVISE — one sentence each, naming the primitive class. |
| E.8 | Aggregate framing — "the audit is read-only triage; no deletions" | The triage discipline is CH2-neutral by construction. The CH2 leak is at the level of *justification language* (consumer-name-keyed rather than primitive-class-keyed), not at the level of deletion proposals. | REVISE (justification language only). |

Aggregate: E is the cohort's largest carrier of class-vs-consumer-name
conflation. None of E's deletions are *wrong* CH2-wise; the deletions
themselves are JSON-corpus-scoped admissible. The leak is that E
audits at the *consumer-name* layer without surfacing the
*primitive-class* layer. V4 fold is a per-deletion one-line class
annotation; no deletions need to be retracted.

### §2.6 — P1-V3-F (REDRESS reconciliation manifest) — REVISE

| # | Locus | Audit | Verdict |
|---|---|---|---|
| F.1 | §3.2 proposed HANDOFF §5 umbrella: "Retained or direct string-scan widening, trusted boundary collapse, value-byte/next-key carry, and per-quartet/per-segment unicode-escape classifier routes" | This is the right *class umbrella* shape — it names the substrate-shape rejections, not "JSON string-scan widening". G6 ACCEPT. | ACCEPT (model — this is the canonical CH2 framing for pre-block umbrellas). |
| F.2 | §3.2 proposed umbrella: "Direct source-hook field folding, parser-owned decoded scratch, byte-output `unescape_*` rewrites, and DirectBuild semantic-string-fact streaming for the digest workload" | The `unescape_*` naming uses the symbol prefix as a class identifier. The class is `direct_route_decoded_scratch` (a route-shape rejection, not a JSON-symbol-prefix rejection). The umbrella is mostly CH2-compliant but the `unescape_*` glob leaks. | REVISE — restate as "byte-output decoded-scratch rewrites in the direct-route family"; cite the existing REDRESS 66–69 entries for the JSON-realisation evidence. |
| F.3 | §3.2 proposed umbrella: "Bench-private hand Track 1 parsers or hand typed sinks presented as generated direct/typed proof" | The "Track 1" naming is bench-harness-shape, not grammar-shape; it generalises. G6 ACCEPT. | ACCEPT. |
| F.4 | §3.2 proposed umbrella: "PMU, cycles-per-byte, masking probes, structural-scan-only paths, and Criterion slope artefacts as Track 1, Track 2, typed product, direct product, or strict admission producers" | Substrate-neutral by construction. G6 ACCEPT. | ACCEPT. |
| F.5 | §2 REDRESS reconciliation table — `REDRESS 60-65, 82-84` "String-scanner widening / boundary-collapse class" | These REDRESS entries were JSON-evidence-driven rejections of specific routes. F's reconciliation treats them as a *class* rejection — correct CH2 framing. | ACCEPT. |
| F.6 | §1.2 contract verdict — "xctrace `cpu-counters` is a direct hardware-counter read through Apple Silicon's PMU via kernel `kpc` APIs" | Hardware-PMU and contract-language framing are grammar-neutral. | ACCEPT. |
| F.7 | §5 G-S-P1-RERUN-CONVERGED bar item 13 — "Generality discipline. Hot leaves named to grammar-neutral primitives; CH2 rejects JSON-role re-naming" | F explicitly names the CH2 GENERALITY discipline as a gate item. This is the cohort's clearest CH2-self-aware sentence. | ACCEPT (model). |
| F.8 | §4 proposed SPEC.md edit F — "V3 real-PMU c/B is a diagnostic characteriser of hot leaves, not a producer; it does not enable any behavior admission path that was blocked in V2" | The "characteriser-not-producer" framing is substrate-neutral and Lock 14-aligned. | ACCEPT. |

Aggregate: F is the cohort's most CH2-aware report — it explicitly
names CH2 as a gate criterion (§5.1 item 3, §5.3 item 13) and proposes
HANDOFF §5 class umbrellas that are substrate-shape rather than
JSON-symbol-shape. The one leak (F.2 `unescape_*` glob) is trivial.

## §3 — Aggregate verdict

| Report | Disposition | Load-bearing leaks |
|---|---|---:|
| P1-V3-A | ACCEPT | 0 |
| P1-V3-B | REVISE | 3 (B.2, B.3, B.4) |
| P1-V3-C | REVISE | 3 (C.3, C.6, C.7) |
| P1-V3-D | REVISE | 5 (D.1, D.2, D.3, D.6, D.7) |
| P1-V3-E | REVISE | 4 (E.1, E.2, E.4, E.7) |
| P1-V3-F | REVISE | 1 (F.2) |

**ACCEPT rate: 2 / 6 ≈ 33%.** Below the 95% convergence bar in §3Z.
Cycle V3 does **not** converge on CH2 GENERALITY in its current form;
V4 must fold the leaks below.

The cohort's evidence is grammar-neutral by construction: PMU counters
are hardware reads, samply/xctrace symbol attribution is methodology,
the OLS coefficients are JSON-empirical but the regression form is
substrate-neutral. **What is JSON-leaked is the captioning of that
evidence in V3 prose**, not the evidence itself. V4 fold is a prose
pass + minor sub-class refinement; no re-capture is required.

The two CH2-best reports (A's hardware-PMU surface; F's
class-umbrella + CH2-as-gate-item explicit naming) anchor the standard
B/C/D/E must redress to. B's classifier (`string_tiny_scan`,
`string_full_scan`, `unicode_escape_hex`, `number_digit_scan`,
`whitespace_skip`, `simd_movemask`, `consume_structural`,
`object_walk`, `array_walk`, `escape_handling`) is the canonical
substrate-neutral primitive vocabulary for the cohort; V4 must adopt
it cohort-wide.

## §4 — Specific Lock 14 leaks requiring V4 fold

The following are the **three load-bearing CH2 leaks** the V4 cycle
must redress. Lesser leaks (paragraph-level captioning) fold under
these three umbrellas.

### §4.1 — D.3 / D.6 "per-quote" cost is presented as JSON primitive

`P1-V3-D §5` fits `ns_per_byte = 8.64 * (quotes/bytes) + 1.47 *
(numbers/bytes) + 0.410` and concludes the per-quote cost is **~21×
the baseline byte**. The coefficient is *named* against JSON quotes;
the *law* is substrate-neutral (per-StructuralAlphabet-byte marginal
cost, with each grammar fitting its own column set). The V9-W1 wave
("string-plane cost cut, per-quote ~10–15%"), V9-W2 wave ("digest-sink
truth pass"), and V10 wave ("unicode validation kernel") inherit the
JSON-naming.

**V4 redress**: re-frame §5 OLS as a per-grammar instantiation. JSON's
column set is `{quote_density, number_density}`; CSS L4's column set
is `{double_string_delim_density, single_string_delim_density,
function_paren_density, declaration_terminator_density,
number_density, percentage_density, dimension_density,
identifier_density}` per SC-6 §4.3. The substrate-neutral law:
`ns_per_byte = sum(class_marginal * class_density) + baseline` where
class is a StructuralAlphabet ordinal class. Wave names: V9-W1 =
**string-plane / delimited-span-scanner cost cut**; V9-W2 =
**direct-route sink-callback cost truth pass**; V10 = **unicode /
escape-codec validation kernel**. JSON's `q_frac ≥ 0.726` rows are the
JSON realisation of "delimited-span fraction admission predicate"; CSS
L4 and Sheets will fit their own thresholds against their own
StructuralAlphabet delimited-span fractions.

Citation chain (already in tree): Lock 14
(`restart/locks/LOCKS.md:60`); StructuralAlphabet
(`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md` §4);
Lock 16 grammar-neutral primitive surface
(`restart/locks/LOCKS.md:94` + `bbnf-simd/ext/x86/bbnf.asm`); Lock 1
substrate union (`restart/ARCHITECTURE.md:1060`).

### §4.2 — B.3 / B.4 / C.3 / E.4 unicode-escape codec presented as JSON primitive

`P1-V3-B §3.2` names "the unicode-escape codec
(`read_hex_unit_scalar` + `hex_nibble`)" as the y_string_unicode
bottleneck; `P1-V3-C §5.2` cites `unescape_string` 47.5% on
unicode_escapes; `P1-V3-E §2.4` justifies `unescape_uxxxx.rs` KEEP
against "the materialization path in `unescape_four_unicode_escapes`".
All four loci frame the kernel as the JSON `\uXXXX` codec; the
*class* — **hex-quartet → utf-8 escape codec with surrogate-join
policy** — is substrate-neutral. CSS L4 admits `\HHHHHH` (variable
1–6 hex digits + whitespace terminator; no surrogate-join), JS-like
admits both `\uXXXX` and `\u{HHHHHH}`, TOML admits `\uHHHH` +
`\UHHHHHHHH`.

**V4 redress**: introduce the substrate-neutral primitive class
`escape_codec_hex_unit` (or `unicode_escape_kernel`) with parameters
`{hex_digit_count, surrogate_join_policy, terminator_policy}`. JSON's
`\uXXXX` is the instantiation `{4, surrogate-pair-join, no-terminator}`;
CSS L4's `\HHHHHH` is `{1..6, no-surrogate, whitespace-or-non-hex
terminator}`. The kernel at
`bbnf-simd/src/aarch64/unescape_uxxxx.rs` realises the JSON
instantiation under codegen-emitted parameter binding (SC-6 §4 +
Lock 16). Wave V10 ("unicode validation kernel") then targets this
substrate-neutral class, not a JSON-only kernel.

### §4.3 — C.6 / E.1 / E.2 cleanup audit operates at consumer-name layer, not primitive-class layer

`P1-V3-E §2.2` SAFE-TO-DELETEs `aarch64/match_tiny_plain_string.rs`
because "the admitted scalar shape lives in generated runtime, not as
a NEON primitive"; the audit is correct for the *current JSON-only
corpus* but does not record that the primitive *class*
(`string_tiny_scan` per B's classifier) is substrate-neutral and may
be re-admitted under a future grammar. Similarly `P1-V3-E §2.3`
SAFE-TO-DELETEs `digit_mac.rs` citing REDRESS 80 (canada
zero-fallback rate) — a JSON-corpus-empirical rejection treated as
class-retiring. `P1-V3-C §2.2` buckets
`parse_object_value_at_direct` / `parse_array_element_at_direct` /
`JsonDigestSink::array_string` into `traversal_other`, hiding the
substrate-neutral sub-classes (direct field projection, direct
repeated projection, direct sink callback).

**V4 redress**: rework E §2 deletion table to carry *two* columns —
"corpus-scoped consumer status" (current JSON-only) and
"primitive-class status" (substrate-neutral class identity). A
deletion that is SAFE-TO-DELETE for the current corpus may still be a
class that S-P2 / S-P3 admits for a future grammar; git history
preserves the kernel for re-introduction under codegen-emitted
parameter binding. Rework C §1.3 classifier to split
`traversal_other` into `dispatch_value`, `direct_field_projection`,
`direct_repeated_projection`, `direct_sink_callback`,
`parse_value_at`, and the per-route-shape primitives — each a
substrate-neutral class spanning grammars.

### §4.4 — Lesser leaks (fold under §4.1–§4.3 umbrellas)

The following paragraph-level leaks are admissible as one-sentence
folds inside the V4 redress of §4.1–§4.3 above; they do not require
separate V4 actions:

- B.2 — re-cast §3.2/§3.4 prose around `string_tiny_scan` (folds
  under §4.1).
- B.7 — add substrate-shape generalisation sentence to §3.1 SC-1
  verdict (folds under §4.3).
- C.4 — re-name "string fraction" → "delimited-span density" in §5.4
  (folds under §4.1).
- C.5 — add JSON-empirical scope sentence to §6.4 (folds under §4.3).
- C.7 — re-caption §5.2 around `escape_codec` class (folds under
  §4.2).
- D.1 — add StructuralAlphabet column-name mapping to §1 (folds under
  §4.1).
- D.2 — re-cast §2 verdict as delimited-span-fraction (folds under
  §4.1).
- D.4 / D.5 / D.8 — one-sentence class-generalisation notes
  (`structural_open_offset_tape`, `digit_FSM_class`, `route_class`).
- E.2 / E.7 — per-deletion class-preservation note (folds under §4.3).
- F.2 — restate `unescape_*` glob as "direct-route decoded-scratch"
  class umbrella (folds under §4.2).

## §5 — Convergence accounting

Cycle V3 disposition on CH2 GENERALITY: **REVISE** with 2 / 6 ACCEPT.
Below the 95% × 2-consecutive-cycle bar in `restart/prompts/ORCHESTRATOR.md`
§3Z.

The cohort's underlying evidence is grammar-neutral by construction;
no PMU row, no symbol attribution, and no OLS coefficient is
JSON-locked beyond the corpus that produced it. The CH2 leak is at the
*captioning + framing* layer, not the *evidence* layer. The V4 fold
predicted under §4.1–§4.3 is therefore a prose pass + classifier
refinement + cleanup-audit annotation; no re-capture or re-measurement
is required.

V4 must adopt B's classifier vocabulary
(`string_tiny_scan`, `string_full_scan`, `escape_codec_hex_unit`,
`number_digit_scan`, `whitespace_skip`, `simd_movemask`,
`consume_structural`, `direct_field_projection`,
`direct_repeated_projection`, `direct_sink_callback`,
`parse_value_at`) cohort-wide, and F's class-umbrella shape for the
HANDOFF §5 pre-block ledger. Once those folds land, the cohort
generalises by inspection.
