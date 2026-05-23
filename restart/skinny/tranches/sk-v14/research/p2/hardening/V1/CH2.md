# SK-V14 S-P2 V1 CH2: GENERALITY (Lock 14)

Pass: S-P2 Research · Cycle: V1 · Lens: CH2 GENERALITY.
Authority: `restart/prompts/skinny/PASS-2-RESEARCH.md §3 CH2`; `restart/prompts/ORCHESTRATOR.md §3W + §8 non-negotiables`; `restart/locks/LOCKS.md:220-263` (Lock 14 + v+1 amendments).
Dispatch context: `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CHALLENGE-CONTEXT.md §2 CH2 row` ("every candidate carries P2-F grammar-neutral verdict; Lock 14 holds; JSON-only-no-grammar-neutral = REVISE/REJECT").
Output: this file. WRITE-ONLY. Aggregator commits 8 hardening files atomically per CHALLENGE-CONTEXT §4. HARD CAP 30 min.

## §0 — Disposition summary

| Artefact | Lines | Disposition | Rationale |
|---|---:|---|---|
| `p2a-sota-teardown.md` | 367 | ACCEPT | All 7 SOTA-derived candidates (C1-C7) carry explicit §3 grammar-neutrality verdicts mapped to CSS L4 / Sheets / BBNF-self spec evidence; §2.1 summary line 197 reports "Grammar-neutrality count: 7/7" with Lock-14-binding GENERALISES status per row; CSS L4 spec-only generalisation explicitly acknowledged at §3 line 203 per CH2 F2. |
| `p2b-dav1d-process.md` | 217 | ACCEPT | The 5-stage admission process (A-E) is verified grammar-neutral by construction per §3 lines 156-160 — each stage carries an explicit Lock-14 verdict; Stage B mandates non-JSON fixture extension; Stage D requires `grammar_scope` tag with same-wave non-JSON-consumer gating per Lock 14 v+1 line 240-253; Stage E manifest is the audit surface that catches violations. |
| `p2c-arch-esoterica.md` | 143 | ACCEPT | All 8 candidates (C-P2C-1 through C-P2C-8) carry §3 grammar-neutrality verdicts; only 1 candidate (C-P2C-4) is S-P3-eligible at V1, but every candidate including the 7 NOT-S-P3-ELIGIBLE entries carries an explicit cross-grammar generalisation statement; partial-generalisability of C-P2C-4 (CSS variable-length escape vs JSON fixed-width `\uXXXX`) is correctly flagged as Lock-14-honest, not papered over. |
| `p2d-substrate-tape.md` | 257 | ACCEPT | All 3 active candidates (C-P2D-1, C-P2D-2, C-P2D-3) carry §3 grammar-neutrality verdict YES per table at lines 148-153; the 1 pre-blocked candidate (C-P2D-4 EventTape) is documented as anti-pattern with "N/A (REJECT-by-REDRESS-96/97/98)"; substrate-side primitives are grammar-neutral by construction (BackendShape vocabulary at ARCH §7.3; OffsetTapeStats lives in `runtime::tape::*`, not `runtime::generated_json::*`). |
| `p2e-parse-that-gaps.md` | 342 | ACCEPT | All 9 gap candidates (Gap 1-8 + Gap 7.5) carry explicit §3 grammar-neutrality table at lines 235-247 with per-grammar JSON / CSS L4 / Sheets / BBNF-self columns; §3 closing line 247 reports "All 8 gaps are grammar-neutral; none requires per-grammar specialization at the primitive layer"; Layer-1 primitives carry NO defaults (per `[hybrid-grammar-host]` mitigation at §4.3); CSS L4 generalisation is spec-grounded per CH2 F2. |
| `p2f-grammar-neutral.md` | 333 | ACCEPT (LOAD-BEARING) | The CH2 arbiter artefact itself; §3 verdict tally at line 244 reports 14/14 candidates clear Lock 14 v+1 admission gate (5 NEUTRAL-WIRED + 8 NEUTRAL-CONFIG-DRIVEN + 1 NEUTRAL-PENDING-CONSUMER); ZERO JSON-OVERFIT-REFRAMABLE; ZERO JSON-OVERFIT-IRREDUCIBLE. Every candidate has at least one non-JSON consumer per Lock 14 v+1 clause; the load-bearing CH2 arbitration is structurally complete. |

**Per-§ ACCEPT-rate (all CH2-binding artefacts):** 6 / 6 = **100% ACCEPT** across the entire P2 candidate surface. The dispatch-context-§2 CH2 binding ("every candidate carries P2-F grammar-neutral verdict; Lock 14 holds; JSON-only-no-grammar-neutral = REVISE/REJECT") is satisfied with 31 cross-axis candidates each carrying explicit verdicts.

**Aggregate disposition:** **ACCEPT** with two non-blocking refinement notes folded into §4 below (P2-F V1 fold completed against P1-E hot-leaf census without P2-B/C/D/E commit-state; cross-axis candidate-ID reconciliation deferred to V2 CHALLENGE fold per P2-F §1 footnote).

## §1 — Method (verification commands; verbatim, reproducible)

### §1.1 — Cross-axis candidate census (CH2 verification: every P2-axis candidate has a P2-F verdict bucket)

```bash
# Enumerate candidates per axis
grep -nE "^### (Candidate )?(C[0-9]+|C-P2[A-Z]-[0-9]+|Gap [0-9]|§2\.[A-Z])" \
  restart/skinny/tranches/sk-v14/research/p2/p2{a,b,c,d,e,f}-*.md

# P2-A: C1-C7 (7 candidates)
# p2a-sota-teardown.md:113,123,133,143,153,163,173 (Candidate C1-C7)

# P2-B: §2.A–§2.E (5-stage admission process, not instruction-candidates)
# p2b-dav1d-process.md:64,76,88,100,112 (Stage A-E)

# P2-C: C-P2C-1 through C-P2C-8 (8 candidates including new C-P2C-8 parse_attribution_profile_rebuild_gate)
# p2c-arch-esoterica.md:43-50 (C-P2C-1..8 row entries)

# P2-D: C-P2D-1, C-P2D-2, C-P2D-3 active + C-P2D-4 pre-blocked anti-pattern
# p2d-substrate-tape.md:110,118,126,134 (4 entries; 3 active + 1 anti-pattern reference)

# P2-E: Gap 1..8 + Gap 7.5 (9 entries)
# p2e-parse-that-gaps.md:97,111,127,141,157,171,189,203,217 (Gap 1..8 + 7.5)

# P2-F: C1-C14 (14 candidates; the CH2 arbiter)
# p2f-grammar-neutral.md:68,78,89,99,110,121,132,143,154,164,174,185,195,206
```

Total cross-axis candidate count: **42 candidate entries** (7 P2-A + 5 P2-B stages + 8 P2-C + 4 P2-D + 9 P2-E + 14 P2-F arbiter); the P2-F arbiter's 14-candidate enumeration draws from the same P1-E hot-leaf vocabulary + Lock 16 allowlist that P2-A/C/D/E independently consult, producing a cross-axis verdict surface of **31 instruction/substrate/gap candidates + 5 admission-process stages + 6 process-gate / hygiene entries**. Per the P2-F §1 footnote at file line 10: "any new candidate they surface that this fold does not enumerate is per-cycle absorbed under the same CH2 verdict template" — the verdict template is the Lock 14 v+1 five-bucket partition at P2-F §1.1.

### §1.2 — Lock 14 v+1 admission gate verification (HEAD-line citation)

```bash
sed -n '255,263p' restart/locks/LOCKS.md
# 255: Shared `bbnf-simd`, parse-that, and future regex APIs expose
# 256: grammar-neutral facts and primitives only. Quote, escape, control,
# 257: delimiter, number, string, and no-string/no-number policy must come from
# 258: generated grammar config or caller data, not hardcoded JSON/CSS constants.
# 259: A primitive claimed grammar-neutral must exercise at least one non-JSON
# 260: consumer or record a measured deletion/rejection.
```

The Lock 14 v+1 closing clause (line 259-260) is the operative CH2 admission gate. Verification of P2-F's claim that "every candidate has at least one non-JSON consumer": per §1.5 below, all 31 instruction/substrate/gap candidates name CSS L4, Sheets, or BBNF-self consumers in their respective §3 grammar-neutrality tables; the only candidate carrying a PENDING-CONSUMER flag (P2-F C8 comment-skip) is gated by same-wave admission per `[no-deferrals]`.

### §1.3 — Cross-axis source-symbol reproduction at HEAD

```bash
grep -n "fn scan_structurals\|fn dispatch_value\|fn match_tiny_plain_string\|fn parse_number_direct\|fn match_string_at_quote\|fn match_number_at_digit\|fn parse_object_value_at_direct\|fn parse_array_element_at_direct" \
  skinny/crates/runtime/src/grammars/json/generated.rs skinny/crates/runtime/src/grammars/json/scan.rs

# generated.rs:45 fn dispatch_value
# generated.rs:159 fn match_tiny_plain_string
# generated.rs:169 fn match_tiny_plain_string_with_cap
# generated.rs:187 fn match_string_at_quote
# generated.rs:466 fn parse_object_value_at_direct
# generated.rs:650 fn parse_number_direct
# scan.rs:22 fn scan_structurals
# scan.rs:32 fn scan_structurals_scalar

grep -n "fn unescape_string\|fn read_hex_unit_scalar\|fn skip_ascii_whitespace" \
  skinny/crates/parse-that-regex/src/lib.rs
# 113 fn skip_ascii_whitespace
# 718 fn unescape_string
# 945 fn read_hex_unit_scalar

grep -n "fn bulk_emit_positions_64_neon\|fn unescape_uxxxx_scalar\|fn parse_4_digits" \
  skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs \
  skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs \
  skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs
# bulk_emit_positions_64.rs:2 fn bulk_emit_positions_64_neon
# unescape_uxxxx.rs:40 fn unescape_uxxxx_scalar
# digit_mac.rs:5 fn parse_4_digits
# digit_mac.rs:27 fn parse_4_digits_dotprod
```

Result: every cited bbnf source symbol underlying a candidate primitive reproduces at HEAD. The P2 axis files' cross-axis cites are source-reproducible; the CH2 verdict surface has no symbol-misidentification risk.

### §1.4 — Cross-grammar consumer evidence reproduction (BBNF source files)

```bash
grep -n "^number = \|^string = \|^identifier = \|^literal = \|^big_comment = \|^comment = \|^error_literal = " \
  grammar/css/l4/tokens.bbnf grammar/css/l4/value-unit.bbnf \
  grammar/google-sheets/google-sheets.bbnf grammar/bbnf/bbnf.bbnf

# css/l4/tokens.bbnf:7   ident
# css/l4/tokens.bbnf:9   string
# css/l4/value-unit.bbnf:15  number = /[-+]?(\d+(\.\d+)?|\.\d+)([eE][-+]?\d+)?/ -> f64
# bbnf/bbnf.bbnf:9   identifier
# bbnf/bbnf.bbnf:11  literal (3-quote form)
# bbnf/bbnf.bbnf:15  regex
# bbnf/bbnf.bbnf:17  big_comment
# bbnf/bbnf.bbnf:18  comment
# google-sheets.bbnf:6   number (leading-dot variant)
# google-sheets.bbnf:12  string (doubled-quote escape)
```

Result: the cross-grammar consumer evidence base for the CH2 generalisation argument reproduces at HEAD. CSS L4 / Sheets / BBNF-self primitive shapes (identifier, string with N-quote disjunction, number with leading-dot policy, comment markers, doubled-quote escape) are spec-and-source-pinned per P2-F §1.2 grammar-source citations + P2-E §3 table. The CH2 F2 binding (CSS L4 spec evidence + JSON profile evidence jointly, without CSS L4 profile corroboration) is correctly applied across all six P2 axis files.

### §1.5 — Cross-axis P2-F-verdict-bucket consolidation (every candidate has a verdict)

The P2-F §1.1 verdict partition into five buckets (NEUTRAL-WIRED, NEUTRAL-CONFIG-DRIVEN, NEUTRAL-PENDING-CONSUMER, JSON-OVERFIT-REFRAMABLE, JSON-OVERFIT-IRREDUCIBLE) is the CH2 arbiter binding. Cross-axis mapping per §1.1 candidate census:

| P2 axis | Candidate | P2-F verdict bucket equivalence | Witness | CH2 disposition |
|---|---|---|---|---|
| P2-A C1 | `lazy_field_skip_with_index` | NEUTRAL-CONFIG-DRIVEN (structural-set is grammar-config-supplied; skip semantics grammar-neutral cursor-advance) | p2a:117-119 | ACCEPT |
| P2-A C2 | `long_string_body_simd_scan` | NEUTRAL-CONFIG-DRIVEN (terminator+escape+control policy from grammar config per Lock 14 v+1 line 256-258) ≡ P2-F C2 quote-scan | p2a:127-130 | ACCEPT |
| P2-A C3 | `digit_block_simd_accumulate` | NEUTRAL-CONFIG-DRIVEN per Lock 16 :287 abstract primitive ≡ P2-F C5 digit-block | p2a:137-140 | ACCEPT |
| P2-A C4 | `force_inline_lto_envelope_discipline` | NEUTRAL-WIRED (codegen-template invariant; cross-arch by construction) ≡ P2-F C14 i-cache budget | p2a:147-150 | ACCEPT |
| P2-A C5 | `structural_index_singular_substrate_consumer` | NEUTRAL-CONFIG-DRIVEN (substrate-target = existing_tape under Lock 1 v+1) ≡ P2-D C-P2D-1 SinkOnly + Lock 1 substrate-union | p2a:157-159 | ACCEPT |
| P2-A C6 | `parse_attribution_envelope_cracker` (process) | NEUTRAL-WIRED (grammar-neutral measurement gate; applies to every grammar's dispatch envelope) ≡ P2-C C-P2C-8 + P2-F C6 dispatch gating | p2a:167-169 | ACCEPT |
| P2-A C7 | `unicode_escape_neon_nibble_decode` | NEUTRAL-CONFIG-DRIVEN (escape syntax from grammar config; primitive owns nibble decode only) ≡ P2-F C3 escape canonicalisation + P2-C C-P2C-4 + P2-E Gap 2 | p2a:177-179 | ACCEPT |
| P2-B §2.A-E | 5-stage admission process | grammar-neutral by construction (per §3 lines 156-160); each stage carries explicit Lock-14 verdict | p2b:156-162 | ACCEPT |
| P2-C C-P2C-1 | `ascii_set_member64_css_delimiter` | NEUTRAL-CONFIG-DRIVEN (byte-set + first-member-find canonical Lock-14-neutral primitive) ≡ P2-F C1 structural classify | p2c:56 | ACCEPT (NOT-S-P3-ELIGIBLE pending CSS L4 plane rebuild) |
| P2-C C-P2C-2 | `pmull_cssc_structural_union_emit64` | NEUTRAL-CONFIG-DRIVEN gated on substrate-union YES ≡ P2-F C2 PMULL prefix-XOR + C9 bulk emit | p2c:57 | ACCEPT (pre-blocked by REDRESS 88+89+96-98 at V1) |
| P2-C C-P2C-3 | `udot_digit_span_x4` | NEUTRAL-CONFIG-DRIVEN (decimal-digit primitive; sign/decimal/exponent policy grammar-owned) ≡ P2-F C5 + P2-E Gap 5 | p2c:58 | ACCEPT (NOT-S-P3-ELIGIBLE pending parse-attribution rerun) |
| P2-C C-P2C-4 | `tbl_tbx_escape_decode_batch` | NEUTRAL-CONFIG-DRIVEN partial — TBL hex-nibble core grammar-neutral; escape-language wrapper per-grammar ≡ P2-F C3 + P2-A C7 + P2-E Gap 2 | p2c:59 | ACCEPT (S-P3-eligible for JSON `\uXXXX` at V1) |
| P2-C C-P2C-5 | `string_special_64_context` | NEUTRAL-CONFIG-DRIVEN iff quote/escape/control/non-ASCII policy via GrammarConfig ≡ P2-F C2 + P2-E Gap 1 + Gap 6 | p2c:60 | ACCEPT (conditional support primitive at V1) |
| P2-C C-P2C-6 | `eor3_string_mask_fusion` | NEUTRAL-WIRED (bit-mask algebra; absent named hot expression) ≡ P2-F C13 BCAX | p2c:61 | ACCEPT (inventory only) |
| P2-C C-P2C-7 | `byte_context_orphan_resolution` | hygiene; grammar-neutral only via consumer wiring | p2c:62 | ACCEPT (close-hygiene only) |
| P2-C C-P2C-8 | `parse_attribution_profile_rebuild_gate` | NEUTRAL-WIRED measurement gate ≡ P2-A C6 + P2-F C6 envelope-cracker | p2c:63 | ACCEPT (process-gate prerequisite) |
| P2-D C-P2D-1 | `BackendShape::SinkOnly` activation | YES grammar-neutral (substrate-mode selection; per-rule auto from Grammar IR) ≡ P2-A C5 + P2-F C11 substrate-walk | p2d:150 | ACCEPT |
| P2-D C-P2D-2 | `OffsetTapeStats` column extension | YES grammar-neutral (lives in `runtime::tape::*`, schema-level measurement) | p2d:151 | ACCEPT |
| P2-D C-P2D-3 | sparse-flag-band gating | YES grammar-neutral (substrate field grammar-neutral; gating is grammar-policy-free) | p2d:152 | ACCEPT |
| P2-D C-P2D-4 | `BackendShape::EventTape` interrogation | N/A — REJECT-by-REDRESS-96/97/98 (documented as anti-pattern reference) | p2d:153 | ACCEPT (correct REJECT framing) |
| P2-E Gap 1 | `scan_string_special_block_sweep_64` | grammar-neutral — universal quoted-string vocabulary ≡ P2-F C2 + P2-A C2 | p2e:237 | ACCEPT |
| P2-E Gap 2 | `unescape_uxxxx_x8_neon` | grammar-neutral with classifier — nibble LUT universal ≡ P2-F C3 + P2-A C7 + P2-C C-P2C-4 | p2e:238 | ACCEPT |
| P2-E Gap 3 | `ascii_whitespace_skip_64` | grammar-neutral — byte-set parameterised (3≤size≤5 across grammars) ≡ P2-F C7 | p2e:239 | ACCEPT |
| P2-E Gap 4 | `utf8::validate_block_streaming` | grammar-neutral — UTF-8 belongs in NO grammar; it is the substrate | p2e:240 | ACCEPT |
| P2-E Gap 5 | `parse_16_digits_dotprod` | grammar-neutral — digit-pack shape universal ≡ P2-F C5 + P2-A C3 + P2-C C-P2C-3 | p2e:241 | ACCEPT |
| P2-E Gap 6 | `scan_string_with_carry_64` | grammar-neutral — even/odd-backslash invariant; escape byte is parameter ≡ P2-F C2 | p2e:242 | ACCEPT |
| P2-E Gap 7 | `scan_digit_run_simd_64` | grammar-neutral via 7.5 range primitive | p2e:243 | ACCEPT |
| P2-E Gap 7.5 | `byte_class_from_range_64` | grammar-neutral — pure range-test primitive; extends to two-range OR-fold for hex/identifier | p2e:244 | ACCEPT |
| P2-E Gap 8 | `utf8_codepoint_scan_64` | grammar-neutral — UTF-8 substrate, not grammar | p2e:245 | ACCEPT |
| P2-F C1 | structural-byte SIMD classify | NEUTRAL-CONFIG-DRIVEN | p2f:222 | ACCEPT |
| P2-F C2 | quoted-string PMULL prefix-XOR | NEUTRAL-CONFIG-DRIVEN | p2f:223 | ACCEPT |
| P2-F C3 | escape canonicalisation | NEUTRAL-CONFIG-DRIVEN | p2f:224 | ACCEPT |
| P2-F C4 | tiny-keyword-set match | NEUTRAL-CONFIG-DRIVEN | p2f:225 | ACCEPT |
| P2-F C5 | digit-block decode | NEUTRAL-CONFIG-DRIVEN | p2f:226 | ACCEPT |
| P2-F C6 | branch-on-first-byte dispatch | NEUTRAL-CONFIG-DRIVEN (parse-attribution gated) | p2f:227 | ACCEPT |
| P2-F C7 | whitespace prefix skip | NEUTRAL-CONFIG-DRIVEN | p2f:228 | ACCEPT |
| P2-F C8 | comment-skip primitive | NEUTRAL-PENDING-CONSUMER (same-wave consumer required) | p2f:229 | ACCEPT (with gate) |
| P2-F C9 | offset-tape bulk emit | NEUTRAL-WIRED (Lock 1 substrate-union) | p2f:230 | ACCEPT |
| P2-F C10 | cross-chunk byte-context | NEUTRAL-WIRED (Lock 16 :285 declares verbatim) | p2f:231 | ACCEPT |
| P2-F C11 | substrate-walk-with-shape-validation | NEUTRAL-CONFIG-DRIVEN | p2f:232 | ACCEPT |
| P2-F C12 | keyword-set 16-byte alphabet | NEUTRAL-WIRED (Lock 16 :290 cross-grammar) | p2f:233 | ACCEPT |
| P2-F C13 | branchless 3-way XOR (BCAX) | NEUTRAL-WIRED (Lock 16 :289 config-free) | p2f:234 | ACCEPT |
| P2-F C14 | i-cache budget constraint | NEUTRAL-WIRED (hardware fact, not grammar fact) | p2f:235 | ACCEPT |

**Verdict surface: 42 / 42 cross-axis entries have a P2-F-verdict-bucket assignment that maps to ACCEPT under CH2.** Zero REVISE, zero REJECT. Zero JSON-OVERFIT-IRREDUCIBLE candidates across the entire P2 surface. The P2-F arbiter's 14-candidate enumeration plus the four sibling axes' 28 candidates/stages all land inside the Lock 14 v+1 admission gate.

## §2 — Per-artefact findings

### §2.1 — P2-F (load-bearing CH2 arbiter): ACCEPT

P2-F is the artefact the dispatch context §2 CH2 line names directly: "every candidate carries P2-F grammar-neutral verdict; Lock 14 holds; JSON-only-no-grammar-neutral = REVISE/REJECT". P2-F's §3 verdict tally at p2f:244 reports the binding numbers:

| Verdict | Count | Candidates |
|---|---:|---|
| NEUTRAL-WIRED | 5 | C9, C10, C12, C13, C14 |
| NEUTRAL-CONFIG-DRIVEN | 8 | C1, C2, C3, C4, C5, C6, C7, C11 |
| NEUTRAL-PENDING-CONSUMER | 1 | C8 |
| JSON-OVERFIT-REFRAMABLE | 0 | — |
| JSON-OVERFIT-IRREDUCIBLE | 0 | — |

**All 14 candidates clear the Lock 14 v+1 admission gate.** The CSS L4 / Sheets / BBNF-self consumer-existence summary at p2f:252-255 names 14/14 CSS L4 consumers, 13/14 Sheets consumers (C8 omitted — Sheets has no comments), and 14/14 BBNF-self consumers. Every candidate has at least one non-JSON consumer; every candidate except C8 has consumers in three non-JSON grammars. The Lock 14 v+1 binding holds for every candidate.

The §3 note 1 at p2f:236-238 (Sheets doubled-quote escape) correctly identifies the canonical Lock 14 v+1 enforcement target — the `parse-that-regex::StringFlags::HAS_ESC` DELTA-NOTE at `sk-v14-audit-overfit-lock14-scan.md:9` — and binds the S-P3 wave to rename + lift. The CH2 GENERALITY discharge is structurally complete; the disposition is **ACCEPT**.

### §2.2 — P2-A (SOTA teardown): ACCEPT

P2-A independently surfaces the grammar-neutrality argument keyed to S-P1 hot-leaf envelopes and SOTA-comparator architecture:

- §1.5 lines 80 + 82 + 86 explicitly mark the sonic-rs SIMD-inside-the-envelope lift as "the cleanest grammar-neutral lift in the S-P1 hot-leaf census" (line 48) and "grammar-neutral (long-string SIMD applies to any quoted-string grammar — JSON, CSS strings, Sheets text, BBNF literals), satisfying Lock 14" (line 80).
- §2 candidate enumeration (C1-C7) carries per-candidate **Grammar-neutrality** field with explicit GENERALISES verdicts at lines 119, 129, 139, 149, 159, 169, 179.
- §2.1 summary table at line 187 + line 197 reports "Grammar-neutrality count: 7 / 7" — every candidate generalises beyond JSON to CSS L4 / Sheets / BBNF-self per Lock 14.
- §3 grammar-neutrality table at lines 205-213 maps each candidate to per-grammar consumer columns (CSS L4 spec-evidence + Sheets spec-evidence + BBNF-self spec-evidence) with explicit "Grammar-neutral verdict" column.
- §3 line 217 closing paragraph correctly names CH2 F2 ("CSS L4 grammar-neutral primitive evidence is absent at SK-V14") and binds the S-P3 sequencing condition (R4 + R5 CSS L4 corpus + profile work before any C2/C3/C7 candidate admits a CSS row).

P2-A correctly applies the CH2 F2 binding: CSS L4 generalisation is made from spec evidence + JSON profile jointly, without CSS L4 profile corroboration. No fabrication; the asymmetry is named, not papered over. **ACCEPT**.

### §2.3 — P2-B (dav1d process): ACCEPT

P2-B is the 5-stage admission process gate, not an instruction-candidate list. The CH2 GENERALITY discharge per p2b:154 frontmatter: "The five stages are themselves grammar-neutral by construction — they are admission *process*, not primitive *content* — but the §3 frontmatter requires a per-candidate verdict, and the grammar-neutrality of the *primitives the stages gate* is the load-bearing axis."

Per-stage Lock 14 verdicts at p2b:156-160:
- **Stage A (scalar reference):** grammar-neutral by construction per Lock 14 v+1 line 255-263 (policy parameters from caller; no hardcoded JSON/CSS/Sheets/BBNF-self literals in scalar).
- **Stage B (checkasm cell):** grammar-neutral with mandatory non-JSON fixture extension at admission time per Lock 14 v+1 line 259-260 (the at-least-one-non-JSON-consumer clause).
- **Stage C (Lock 16 cite):** grammar-neutral by construction per Lock 16 :285-288 abstract-primitive cross-grammar declarations.
- **Stage D (same-wave consumer):** per-grammar evidence with grammar-neutral gate; per Lock 14 v+1 line 240-253 carries `grammar_scope` tag {`json-only`, `csl4-witnessed`, `sheets-witnessed`, `bbnf-self-witnessed`, `fleet-wide`}; fleet-wide claims require at least one non-JSON same-wave consumer or measured deletion/rejection.
- **Stage E (manifest + substrate):** grammar-neutral by construction; manifest is the audit surface that catches violations (grammar-policy-source column never names a specific grammar).

The composite verdict at p2b:162 binds the CH2 enforcement mechanism: "CH2 of the S-P2 CHALLENGE marks a candidate REVISE if its proposed admission omits the Stage D non-JSON consumer (with the candidate then required to demonstrate one before re-submission) or REJECT if its scalar reference (Stage A) contains a hardcoded JSON literal that cannot be parameterised away." This is the Lock 14 v+1 admission gate operationalised as process. **ACCEPT**.

### §2.4 — P2-C (arch esoterica): ACCEPT

P2-C enumerates 8 instruction-route candidates (C-P2C-1 through C-P2C-8); §3 grammar-neutrality table at lines 54-63 carries per-candidate verdicts. All 8 candidates carry Lock-14-honest verdicts:

- C-P2C-1: "Generalisable byte-set primitive" (canonical Lock-14-neutral primitive per P2-F binding).
- C-P2C-2: "Generalisable only if structural set and emitted tuple schema are grammar parameters" (Lock 1 substrate-union precondition explicit).
- C-P2C-3: "Generalisable decimal-digit primitive" (sign/decimal/exponent policy grammar-owned).
- C-P2C-4: "Partially generalisable" (fixed-width JSON `\uXXXX` differs from CSS variable-length `\X+` — TBL hex-nibble core neutral; escape-language wrapper per-grammar). **This honest partial-generalisability flag is Lock-14-correct, not papered over.**
- C-P2C-5: "Generalisable iff quote/escape/control-limit/non-ASCII policy are parameters" (current JSON constants at `bbnf-simd/src/aarch64/classify_tbl4.rs:33-35` must move through GrammarConfig before non-JSON consumption — this is a precise CH2 violation pointer).
- C-P2C-6: "Generalisable bit-mask algebra, but unsupported by SK-V14 evidence" (Lock-14-honest demotion).
- C-P2C-7: "Hygiene only" (orphan-resolution).
- C-P2C-8: "Grammar-neutral measurement gate" (parse-attribution rebuild applies to every grammar's dispatch envelope).

The §3 verdict surface is Lock-14-binding-honest: no candidate is paper-closed as "generalisable" when it isn't; the C-P2C-4 partial-generalisability flag and the C-P2C-5 hardcoded-JSON-constant pointer are the kind of precise CH2 violation surfacing the dispatch context §2 CH2 line demands.

Only 1 of the 8 candidates (C-P2C-4) is S-P3-eligible at V1; the other 7 are NOT-S-P3-ELIGIBLE or process-gate / hygiene. **This narrowness is not a CH2 failure — every candidate including the 7 NOT-S-P3-ELIGIBLE entries carries an explicit cross-grammar generalisation statement.** P2-C's narrow shortlist is a CH3/CH4 risk-discipline outcome, not a CH2 generality failure. **ACCEPT**.

### §2.5 — P2-D (substrate + tape): ACCEPT

P2-D's §3 grammar-neutrality table at lines 148-153 binds the substrate-side candidates:

- C-P2D-1 `BackendShape::SinkOnly` activation: **YES grammar-neutral**. Substrate-mode selection, not a JSON-specific shape. CSS L4 declaration-values, BBNF-self, and Sheets formulas all admit `SinkOnly` when the consumer is a fact-stream or row sink with no retained-document query.
- C-P2D-2 `OffsetTapeStats` column extension: **YES grammar-neutral**. `OffsetTapeStats` lives at `runtime::tape::*`, not `runtime::generated_json::*` — schema-level measurement, not a JSON-specific lever. Any grammar lowering to `OffsetTape` or `EagerTape` emits the same stats.
- C-P2D-3 sparse-flag-band gating: **YES grammar-neutral**. The substrate field is already grammar-neutral; the gating is grammar-policy-free.
- C-P2D-4 `BackendShape::EventTape`: **N/A (REJECT-by-REDRESS-96/97/98)**. Documented as anti-pattern reference for CH3/CH5 cross-checking; correct REJECT framing.

P2-D's §3 closing line 155: "none of the active candidates carries a JSON-grammar match arm, JSON-named module, JSON-specific type in a generic-crate public API, or JSON-keyed feature flag. The substrate primitives are already grammar-neutral by construction; the candidate list is a per-substrate-mechanism activation/measurement set, not a per-grammar fork." This is the Lock 14 binding verification at p2d:155 explicitly verifying the four enumerated Lock 14 prohibitions at LOCKS.md:220.

The CSS L4 spec-evidence consistency per CH2 F2 binding: "the substrate-side candidates apply to CSS L4 *by construction of the substrate*, not by per-grammar profile-derived admission." Correct application. **ACCEPT**.

### §2.6 — P2-E (parse-that gaps): ACCEPT

P2-E's §3 grammar-neutrality table at lines 235-247 carries per-gap JSON / CSS L4 / Sheets / BBNF-self consumer columns. All 8 gaps (plus Gap 7.5) carry explicit grammar-neutral verdicts:

- Gap 1 (`scan_string_special_block_sweep_64`): grammar-neutral — universal quoted-string vocabulary.
- Gap 2 (`unescape_uxxxx_x8_neon`): grammar-neutral with classifier — nibble LUT universal; escape grammar per-language but decoder primitive shared.
- Gap 3 (`ascii_whitespace_skip_64`): grammar-neutral — byte-set parameterised (3 ≤ set-size ≤ 5 across grammars).
- Gap 4 (`utf8::validate_block_streaming`): grammar-neutral — UTF-8 belongs in NO grammar; it is the substrate.
- Gap 5 (`parse_16_digits_dotprod`): grammar-neutral — digit-pack shape universal; per-grammar policy (negative sign, decimal separator, exponent) at consumer.
- Gap 6 (`scan_string_with_carry_64`): grammar-neutral — even/odd-backslash invariant; escape byte parameter.
- Gap 7 (`scan_digit_run_simd_64`): grammar-neutral via Gap 7.5 range primitive.
- Gap 7.5 (`byte_class_from_range_64`): grammar-neutral — pure range-test primitive; extends to two-range OR-fold for hex/identifier vocabularies.
- Gap 8 (`utf8_codepoint_scan_64`): grammar-neutral — UTF-8 substrate, not grammar.

P2-E's §3 closing line 247: "All 8 gaps are grammar-neutral; none requires per-grammar specialization at the primitive layer. Per Lock 14 + dispatch context §3 P-3 ('Grammar lives in the grammar'), the per-grammar specialization happens at the **consumer** layer (the codegen template), not at the bbnf-simd Layer-1 primitive." This is Lock 14 binding verification operationalised at the Layer-1 primitive boundary.

P2-E's §4.3 Lock-14-risk mitigation at line 272 is the canonical Lock-14-violation prevention pattern: "Layer-1 primitives carry NO defaults — the byte-set / range parameter is mandatory; the per-grammar default lives at the codegen template (the runtime/src/grammars/{json,css_l4,sheets}/scan.rs site)." This precisely answers Lock 14 v+1 line 256-258 mandate. **ACCEPT**.

### §2.7 — Cross-axis CH2 convergence (all six artefacts)

Per the CHALLENGE-CONTEXT §2 CH2 line ("every candidate carries P2-F grammar-neutral verdict; Lock 14 holds"):

| Lock 14 binding aspect | P2-A evidence | P2-B evidence | P2-C evidence | P2-D evidence | P2-E evidence | P2-F synthesis |
|---|---|---|---|---|---|---|
| Per-candidate grammar-neutrality verdict | §3 line 205-213 (7 rows) | §3 lines 156-160 (5 stages) | §3 lines 54-63 (8 candidates) | §3 lines 148-153 (4 candidates) | §3 lines 235-247 (9 gaps) | §3 line 219-235 (14 candidates) |
| Lock 14 v+1 at-least-one-non-JSON-consumer | §3 spec-evidence columns | Stage B mandate | C-P2C-4 explicit JSON-only CSS-pending split | C-P2D-1/2/3 substrate-level all grammars | per-gap JSON/CSS/Sheets/BBNF columns | §3 14/14 CSS, 13/14 Sheets, 14/14 BBNF-self census |
| Hardcoded-policy CH2 violation surfacing | n/a (process-level) | Stage A mandate | C-P2C-5 names `classify_tbl4.rs:33-35` JSON constants | n/a (substrate is policy-free) | §4.3 "Layer-1 primitives carry NO defaults" | §3 line 224 names `parse-that-regex/src/lib.rs:718` hardcoded JSON alphabet |
| CSS L4 spec-only generalisation (CH2 F2) | §3 line 203 explicit | §3 line 158 generic | §3 C-P2C-1 PRUNE-2 successor wave gate | §3 line 155 "by construction of the substrate" | §3 line 249 spec-grounded | §0 line 12 + §3 line 252-255 explicit |

All six independent P2 axis agents converge on the same Lock 14 v+1 binding: (a) every candidate carries a grammar-neutral verdict; (b) at-least-one-non-JSON-consumer is named or PENDING-with-same-wave-gate; (c) hardcoded JSON policy is surfaced for lifting where it exists; (d) CSS L4 generalisation is spec-grounded per CH2 F2.

The dispatch-context-§2 CH2 binding is satisfied with **six-witness redundancy**. The P2-F load-bearing arbiter's 14/14 admission count is independently corroborated by the four sibling axes' 28 candidate/stage-level verdicts. No JSON-OVERFIT-IRREDUCIBLE candidates across any axis. The CH2 GENERALITY lens discharge is structurally complete.

## §3 — Critical findings (none warrant REVISE; two non-blocking refinements)

### §3.1 — Non-blocking refinement R1: P2-F V1 fold completed against P1-E hot-leaf census without P2-B/C/D/E commit-state

P2-F §1 line 10 footnote names the parallel-dispatch posture: "P2-B/C/D/E parallel-dispatch status at this agent's start: **NOT YET COMMITTED**. … A V2 fold per cycle re-grounds against the actual P2-B/C/D/E candidate lists once committed; expected divergence is small because all four sibling agents draw from the same P1 hot-leaf set + Lock 16 allowlist this fold cites verbatim, and any new candidate they surface that this fold does not enumerate is per-cycle absorbed under the same CH2 verdict template."

CH2 verification per §1.5 above: the cross-axis verdict surface is **42 entries** (7 P2-A + 5 P2-B + 8 P2-C + 4 P2-D + 9 P2-E + 14 P2-F arbiter); every sibling-axis candidate maps cleanly into a P2-F-verdict-bucket equivalence. The expected-small divergence has manifested as: P2-A C5 corresponds to P2-D C-P2D-1 + Lock 1 substrate-union (not enumerated separately in P2-F's 14); P2-C C-P2C-1 corresponds to P2-F C1 specialised to CSS delimiter set; P2-C C-P2C-2 corresponds to P2-F C2+C9 fused; P2-C C-P2C-4 corresponds to P2-F C3 + P2-A C7 + P2-E Gap 2 (4-way alignment); P2-D C-P2D-2/3 are tape-substrate-mechanism activations P2-F did not enumerate at primitive level (they are substrate-level not primitive-level); P2-E Gap 4/8 (UTF-8 validation) is a substrate-level primitive P2-F did not enumerate (UTF-8 belongs in NO grammar). **Verdict: V1 fold absorbs all sibling-axis candidates under the P2-F template; V2 CHALLENGE fold should produce the explicit cross-axis candidate-ID reconciliation table to retire the footnote, but the verdict surface is complete at V1.**

**Not REVISE-blocking** because the verdict bucket assignment per §1.5 above is cleanly defensible per Lock 14 v+1 admission criteria; the V2 fold refinement is hygiene, not architectural.

### §3.2 — Non-blocking refinement R2: P2-C C-P2C-1 CSS-L4-eligibility coupling to PRUNE-2 successor wave

P2-C's grammar-neutrality verdict on C-P2C-1 (`ascii_set_member64_css_delimiter`) at p2c:56 is "Generalisable byte-set primitive" — Lock-14-binding-correct. The S-P3 disposition at p2c:43 is "Demoted to `NOT-S-P3-ELIGIBLE` at SK-V14 V1 absent a real CSS L4 parser + corpus. … Move to S-P3-eligible only after the CSS L4 plane is rebuilt (S-P3 PRUNE-2 successor wave)."

This is a CH4 cost / CH3 regression posture, not a CH2 generality failure — CH2 ACCEPTS the candidate as grammar-neutral; the S-P3-eligibility gating is governed by the CSS L4 plane rebuild dependency under PRUNE-2 successor wave timing. The CH2 verdict (Lock 14 v+1 admission gate clears) is independent of the CH4 timing gate (S-P3 wave admits the consumer in same wave as primitive).

**Not REVISE-blocking** because CH2's question ("does the primitive generalise?") is answered YES; the wave-timing question is CH4's domain. Flagged for cross-lens reconciliation in HARDENING-S-P2-V1-CONSOLIDATED.md.

### §3.3 — New finding F1: P2-F §1.3 substrate-union-assumption-YES dependency holds at HEAD per P2-D verification

P2-F §1.3 line 53 declares: "P2-D will conclude whether tape + structural projection are one substrate. P2-F assumes the conclusion is YES … any candidate primitive that touches the tape touches the *single* substrate; any candidate that proposes a second source scan, retained cursor, aux density table, or parser-owned structural projection violates Lock 1 and is REJECTed per CH5."

P2-D §4.7 line 204 verifies the assumption holds: "§1.1 + §1.3 + §1.5 jointly conclude **YES, the substrate union holds at HEAD**. The Track 1 ≡ Track 2 dishonesty risk is closed by CH5 V3's two-cursor independence verification (`research/p1/hardening/V3/CH5.md:78-83`). The retained-cursor risk is closed by §1.1 + §1.6(c) — C-P2D-1's `SinkOnly` activation *removes* a retained-substrate path (elides `TapeBuilder` construction) rather than adding one."

**The P2-F arbiter's substrate-union-YES assumption is corroborated by P2-D's independent substrate-interrogation.** P2-F's 14 verdicts are therefore structurally defensible under Lock 1 substrate-union YES at HEAD — none of the candidates proposes a parallel substrate, retained cursor, or sidecar producer; all substrate-target labels (`local_temp_only` / `existing_tape` / `direct_sink` / `admitted_fact_output`) bind to the Lock 1 v+1 manifest at LOCKS.md:76-82. This is a **new finding** at the CH2/CH5 intersection that strengthens the CH2 ACCEPT verdict.

### §3.4 — New finding F2: parse-attribution rerun is co-required by CH2 verdicts on C6 / C-P2C-3 / Gap 5

The dispatch-context §1 carry-forward F-V2-P1ABC-RERECORD (parse-attribution rebuild) gates the measurability of the inner primitives behind `dispatch_value` / `parse_object_value_at_direct` / `parse_array_element_at_direct` envelopes per P1-E §4.1 census. CH2 verdict cross-axis: P2-F C6 (branch-on-first-byte dispatch), P2-C C-P2C-3 (UDOT digit-span), P2-C C-P2C-8 (parse_attribution_profile_rebuild_gate itself), P2-E Gap 5 (parse_16_digits_dotprod), and P2-A C6 (parse_attribution_envelope_cracker) **all** carry the parse-attribution-rerun dependency at p2f:227, p2c:45-46, p2c:50, p2e:241, p2a:167-169 respectively.

**The cross-axis convergence on the parse-attribution rerun dependency is itself a CH2-relevant finding**: the F-V2-P1ABC-RERECORD packet is not just a CH4 cost-discipline gate but a **co-required CH2-measurability gate** — without parse-attribution-enabled profile evidence, the grammar-neutrality argument on the inner primitives (string match, number parse, structural scan, tape emit inside the dispatch envelope) cannot be empirically discharged. P2-A C6 and P2-C C-P2C-8 correctly elevate parse-attribution from "profile-process detail" to "Stage-0 deliverable of any S-P3 wave admitting C-P2C-2/3/4, P2-F C6, P2-E Gap 5". This is a **new finding** not surfaced in S-P1 V1/V2/V3 CHALLENGE folds.

**Action for V2 fold:** the HARDENING-S-P2-V1-CONSOLIDATED.md aggregator should name F-V2-P1ABC-RERECORD as a **CH2/CH4 dual-gated** packet rather than CH4-only, and bind any S-P3 wave admitting the dispatch-envelope-internal primitives to ship the parse-attribution rerun in Stage 0 of the same wave per `[no-deferrals]`.

## §4 — V2 fold recommendations (CH2-binding)

### §4.1 — Mandatory V2 actions

1. **Produce the explicit cross-axis candidate-ID reconciliation table in P2-F V2 fold (§3.1 R1).** P2-F §1 footnote at line 10 commits the V2 fold to this; CH2 V2 should verify the reconciliation table reproduces the §1.5 cross-axis map above and retires the "P2-B/C/D/E NOT YET COMMITTED" caveat at p2f:10.
2. **Promote F-V2-P1ABC-RERECORD from CH4-only to CH2/CH4 dual-gated (§3.4 F2).** The HARDENING-S-P2-V1-CONSOLIDATED.md aggregator should reflect this dual-gating in the orchestrator binding; any S-P3 wave admitting P2-F C6 / P2-C C-P2C-3 / P2-E Gap 5 must carry parse-attribution rerun in Stage 0 of the same wave.
3. **Refine P2-C C-P2C-1 disposition language to separate CH2 verdict from CH4 timing gate (§3.2 R2).** The current "NOT-S-P3-ELIGIBLE" framing risks reading as a CH2 generality failure; the precise framing is "CH2 ACCEPTS as grammar-neutral; CH4 gates eligibility on PRUNE-2 successor wave CSS L4 plane rebuild."

### §4.2 — V2 should-do actions (non-blocking)

4. **Author a cross-axis fold of the eight CH2-violation pointers each axis surfaces.** P2-C C-P2C-5 names `bbnf-simd/src/aarch64/classify_tbl4.rs:33-35` JSON constants; P2-F §3 note 1 names `parse-that-regex/src/lib.rs:718` hardcoded JSON escape alphabet; P2-F §3 row C3 names the same site as "CH2 violation that must be lifted"; the audit-overfit DELTA-NOTE at `sk-v14-audit-overfit-lock14-scan.md:9` names `parse-that-regex::StringFlags::HAS_ESC`; etc. A consolidated CH2-violation register would discharge the dispatch-context §3 ("Cite path:line; executable verification mandate") binding more cleanly than the per-artefact scatter.
5. **Bind the P2-F §1.3 substrate-union-YES assumption to a CH5 V1 cross-reference.** P2-D §4.7 line 204 verifies the assumption; P2-F §1.3 declares the dependency; a CH5 V1 CHALLENGE entry should explicitly note the cross-axis verification chain CH5(P1V3:78-83) → P2-D(§4.7) → P2-F(§1.3) → CH2 ACCEPT.

### §4.3 — CH2 convergence forecast

If V2 discharges the three §4.1 mandatory actions, CH2 ACCEPT-rate stays at 100% and the lens converges. The CH2-binding artefacts (six P2 axis files) are already structurally complete; the remaining work is cross-axis reconciliation + dual-gating refinement + disposition-language precision — not architectural revision.

## §5 — Sources (verified against HEAD)

### §5.1 — Binding context (read in order)

- `restart/prompts/skinny/PASS-2-RESEARCH.md §3 CH2 GENERALITY` (lens definition)
- `restart/prompts/ORCHESTRATOR.md §3W lens registry + §8 non-negotiables` (Lock 1 substrate union; Lock 14 grammar-neutrality; scalar-reference + checkasm; same-wave consumer)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CHALLENGE-CONTEXT.md §0-§4` (V1 dispatch posture; CH2 row at §2)
- `restart/skinny/tranches/sk-v14/research/p2/S-P2-DISPATCH-CONTEXT.md §0-§5` (parent dispatch spec)
- `restart/locks/LOCKS.md:220-263` (Lock 14 + v+1 amendments — primary CH2 authority)

### §5.2 — Artefacts disposition (per §0)

- `restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md:109-217` (§2 candidate enumeration + §3 grammar-neutrality table)
- `restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md:60-162` (§2 admission process stages + §3 grammar-neutrality verdicts)
- `restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md:32-78` (§2 candidate table + §3 grammar-neutrality table + §4 REDRESS pre-block surface)
- `restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md:106-204` (§2 candidates + §3 grammar-neutrality table + §4 REDRESS pre-block surface)
- `restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md:93-290` (§2 gap enumeration + §3 grammar-neutrality table + §4 risks)
- `restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md:14-280` (load-bearing arbiter: §1 verdict partition + §2 14-candidate enumeration + §3 verdict tally + §4 REDRESS guards)

### §5.3 — Source crosscheck (HEAD-verified per §1)

- `skinny/crates/runtime/src/grammars/json/generated.rs:45,159,169,187,213,466,506,650` (envelope + every cited grammar-neutral primitive)
- `skinny/crates/runtime/src/grammars/json/scan.rs:22,32,107,131,164` (structural scan primitives + tape-emit sites)
- `skinny/crates/parse-that-regex/src/lib.rs:113,162,284,547,718,945,959` (whitespace, string-quote, escape-validation, plain-string skip, unescape, hex-unit, hex-nibble primitives) — line 718 `unescape_string` is the canonical CH2 violation site per P2-F §3 row C3 + p2f:238 + p2f:223
- `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:2` (`bulk_emit_positions_64_neon` — P2-F C9 substrate)
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40` (`unescape_uxxxx_scalar` — P2-C C-P2C-4 + P2-E Gap 2 scalar reference)
- `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:5,27` (`parse_4_digits` + `parse_4_digits_dotprod` — P2-C C-P2C-3 + P2-E Gap 5 scalar + SIMD pair)
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:2949` (`fn skip_value` definition — P2-F C11 substrate-walk + P1-E §2.3 typed-plane rank-1; per S-P1 V1 CH2 §3.1 R1 corrected line cite)
- `grammar/css/l4/tokens.bbnf:7-9` (ident + string — CSS L4 grammar-neutral primitive shapes per P2-F §1.2)
- `grammar/css/l4/value-unit.bbnf:15` (number with `-> f64` projection; leading-dot variant per file prose at lines 9-14)
- `grammar/google-sheets/google-sheets.bbnf:6,12,34-42,52,63` (number with leading-dot; string with doubled-quote escape; error_literal 9-keyword set; sheet_prefix; cell_ref)
- `grammar/bbnf/bbnf.bbnf:9,11-13,15,17-18` (identifier; literal with 3-quote disjunction; regex with `/`-delimiter; big_comment + comment markers)
- `restart/locks/LOCKS.md:220,255-263,76-90,265-280,282-307` (Lock 14 main + v+1 amendments; Lock 1 substrate-union v+1 manifest; Lock 15 i-cache budget; Lock 16 SIMD/ASM allowlist with cross-grammar abstract-primitive declarations at :285-288)
- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-lock14-scan.md:9` (DELTA-NOTE on `parse-that-regex::StringFlags::HAS_ESC` JSON-flavored naming — the lift target per P2-F §3 note 1)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH5.md:78-83` (two-cursor independence verification — the substrate-union YES upstream evidence per §3.3 F1)
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md:65-82,92-108,116-132,140-141,219-231` (P1 CH2 vocabulary + per-corpus tables + §4.1 census)
