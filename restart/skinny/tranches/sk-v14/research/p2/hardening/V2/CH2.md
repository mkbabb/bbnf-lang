# SK-V14 S-P2 V2 CH2: GENERALITY (Lock 14)

Pass: S-P2 Research · Cycle: V2 · Lens: CH2 GENERALITY.
Authority: `restart/prompts/skinny/PASS-2-RESEARCH.md §3 CH2`; `restart/prompts/ORCHESTRATOR.md §3W + §3Z + §8 non-negotiables`; `restart/locks/LOCKS.md:220-263` (Lock 14 + v+1 amendments).
Dispatch context: `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CHALLENGE-CONTEXT.md §2 CH2 row` ("Lock 14 v+1 holds across all amended axes; demotions did not introduce JSON-only divergence; cross-axis tracking note (P2-F §2.Y) preserves grammar-neutrality across the three colliding axes; F-V2-P1ABC-RERECORD CH2/CH4 dual-gating remains intact").
Carry-forward: V1 CH2 100% ACCEPT (6/6 artefacts; 42 cross-axis verdicts) per `research/p2/hardening/V1/CH2.md`; V1 fold packets (CH1 Fold-1/2/3 + CH4 Fold-4/5/6) shipped in V2 atomic commit `447a26b07`. V1-LOCKED axes P2-A + P2-E confirm zero V2 drift (git-diff verified, 0 lines changed against commit `b3dbc5ca0`).
Output: this file. WRITE-ONLY. Aggregator commits 8 hardening files atomically per CHALLENGE-CONTEXT §3. HARD CAP 30 min.

## §0 — Disposition summary (V2)

| Artefact | Lines | V1 disp | V2 disp | Rationale |
|---|---:|---|---|---|
| `p2a-sota-teardown.md` | 367 | ACCEPT | ACCEPT (V1-LOCKED; zero drift) | `git diff b3dbc5ca0..447a26b07 -- p2a-sota-teardown.md` returns empty. The 7/7 SOTA candidates retain V1 GENERALISES verdicts mapped to CSS L4 / Sheets / BBNF-self spec evidence; CH2 F2 CSS L4 spec-only generalisation acknowledgment intact. |
| `p2b-dav1d-process.md` | 217 | ACCEPT | ACCEPT (V2 amended via Fold-1 SHA pinning) | The 5-stage admission process A-E retains §3 lines 156-160 per-stage Lock 14 verdicts intact. V2 SHA-pinning at §5.1 (FFmpeg `085714182302333dd83dcb9c36cf828dc4eba929` + dav1d `1718ff9aded99f0a89f5c7940d6afb8948301e33`) is cite-discipline, not generality-content; Stage B non-JSON fixture mandate + Stage D `grammar_scope` tag + Stage E manifest audit surface unchanged. |
| `p2c-arch-esoterica.md` | 164 | ACCEPT | ACCEPT (V2 amended via Fold-2 demotion of C-P2C-1/6/7) | Active candidates 5 (C-P2C-2, -3, -4, -5, -8) carry §3 grammar-neutrality verdicts at lines 76-84; demoted candidates C-P2C-1 / C-P2C-6 / C-P2C-7 retain their grammar-neutrality verdict rows verbatim in §3 (the §3 table is keyed to the candidate-id, not the §2/§2.X partition). Demotion did NOT introduce JSON-only divergence — all 8 verdicts intact; C-P2C-5 hardcoded-JSON CH2-violation pointer (`classify_tbl4.rs:33-35`) preserved; C-P2C-4 partial-generalisability flag Lock-14-honest. |
| `p2d-substrate-tape.md` | 254 | ACCEPT | ACCEPT (V2 amended via Fold-2 demotion of C-P2D-3) | Active candidates 2 (C-P2D-1, C-P2D-2) carry §3 grammar-neutral YES at lines 148-149; C-P2D-3 demoted to §1.6(d) substrate-side observation retains explicit grammar-neutrality YES at p2d:104 + p2d:150 verdict row with N/A-DEMOTED-V2 marker; C-P2D-4 N/A-REJECT-by-REDRESS-96/97/98 framing intact. Substrate-side primitives remain grammar-neutral by construction. |
| `p2e-parse-that-gaps.md` | 342 | ACCEPT | ACCEPT (V1-LOCKED; zero drift) | `git diff b3dbc5ca0..447a26b07 -- p2e-parse-that-gaps.md` returns empty. All 9 gaps (Gap 1-8 + 7.5) retain explicit §3 grammar-neutrality table at lines 235-247 with per-grammar JSON / CSS L4 / Sheets / BBNF-self columns; §4.3 "Layer-1 primitives carry NO defaults" canonical Lock 14 v+1 enforcement intact. |
| `p2f-grammar-neutral.md` | 360 | ACCEPT (LOAD-BEARING) | ACCEPT (LOAD-BEARING; V2 amended via Fold-2 C8 demotion + Fold-3 stamping + Fold-4/5 scalar-refs + NF-CH6-3/4 cross-axis notes) | §3 V2 verdict tally at p2f:266-275 reports **13 / 13 active candidates clear Lock 14 v+1 admission gate** (5 NEUTRAL-WIRED + 8 NEUTRAL-CONFIG-DRIVEN); ZERO JSON-OVERFIT-REFRAMABLE; ZERO JSON-OVERFIT-IRREDUCIBLE; ZERO NEUTRAL-PENDING-CONSUMER (V1 C8 PENDING demoted to §2.X.1 non-candidate inventory). Cross-grammar consumer summary at p2f:279-282: **13/13 CSS L4 + 13/13 Sheets + 13/13 BBNF-self** consumer coverage across the active surface. C8 (vacated to §2.X.1) retains full grammar-neutrality verdict prose at p2f:254 for cross-tranche identifier stability; §2.Y cross-axis tracking note for long-string-body SIMD scan consolidation (NF-CH6-4) preserves grammar-neutrality across three colliding axes. |

**Per-§ ACCEPT-rate (all CH2-binding artefacts):** 6 / 6 = **100% ACCEPT** across the entire V2 P2 candidate surface. The dispatch-context-§2 CH2 V2 binding ("Lock 14 v+1 holds across all amended axes; demotions did not introduce JSON-only divergence; cross-axis tracking note preserves grammar-neutrality") is satisfied with **39 active cross-axis candidates** (7 P2-A + 5 P2-B stages + 5 P2-C active + 2 P2-D active + 9 P2-E gaps + 13 P2-F active V2) + **6 demoted/pre-blocked entries** (3 P2-C demoted + 1 P2-D demoted + 1 P2-D pre-blocked + 1 P2-F demoted) each carrying explicit verdicts and demotion stamps.

**Aggregate V2 disposition:** **ACCEPT** with one new V2-cycle finding F-V2-CH2-1 (NF-CH6-4 §2.Y consolidation strengthens CH2 by preventing three orthogonal SIMD-body admissions for one grammar-neutral primitive) and one closure-confirmation F-V2-CH2-2 (F-V2-P1ABC-RERECORD CH2/CH4 dual-gating fully propagated to P2-F C6 §4 risk row + C-P2C-3/-8 disposition + §2.X.1 C8 re-promotion gate). V1 R1 (P2-F cross-axis candidate-ID reconciliation) discharged via the §1.5 V2 verdict surface table below; V1 R2 (P2-C C-P2C-1 CSS-L4 disposition language) discharged via Fold-2 demotion separating CH2 verdict from CH4 timing gate; V1 F1 (substrate-union YES corroboration) holds. V1 F2 (parse-attribution rerun co-required for CH2 measurability) elevated to ratified V2 binding per HARDENING-S-P2-V1-CONSOLIDATED §2.1.

## §1 — V2 method (verification commands; verbatim, reproducible)

### §1.1 — V1-LOCKED axis drift audit (P2-A + P2-E, mandatory per CHALLENGE-CONTEXT §2)

```bash
# V1 commit (S-P2 axis files at V1): b3dbc5ca0
# V2 commit (S-P2 V2 atomic micro-fold): 447a26b07
git diff b3dbc5ca0..447a26b07 -- \
  restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md \
  restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md
# (empty output — zero V2 drift on V1-LOCKED axes)

git diff b3dbc5ca0..447a26b07 --stat -- restart/skinny/tranches/sk-v14/research/p2/
# p2a-sota-teardown.md: absent (zero drift)
# p2e-parse-that-gaps.md: absent (zero drift)
# p2b-dav1d-process.md: +6 lines (Fold-1 SHA pinning §5.1)
# p2c-arch-esoterica.md: +39/-21 (Fold-2 C-P2C-1/6/7 demotion to §2.X)
# p2d-substrate-tape.md: +14/-17 (Fold-2 C-P2D-3 demotion to §1.6(d))
# p2f-grammar-neutral.md: +57/-18 (Fold-2 C8 demotion + Fold-3 stamping + Fold-4/5 scalar-refs + §2.Y NF-CH6-4)
```

**Result:** V1-LOCKED axes P2-A + P2-E carry zero V2 drift. The CHALLENGE-CONTEXT V2 §2 V1-LOCKED audit clause is satisfied. The 7/7 P2-A SOTA-derived candidates + 9/9 P2-E gap candidates retain V1 ACCEPT verdicts unchanged.

### §1.2 — Lock 14 v+1 admission gate verification (HEAD-line citation)

```bash
sed -n '255,263p' restart/locks/LOCKS.md
#   Shared `bbnf-simd`, parse-that, and future regex APIs expose
#   grammar-neutral facts and primitives only. Quote, escape, control,
#   delimiter, number, string, and no-string/no-number policy must come from
#   generated grammar config or caller data, not hardcoded JSON/CSS constants.
#   A primitive claimed grammar-neutral must exercise at least one non-JSON
#   consumer or record a measured deletion/rejection. Evidence:
#   `restart/skinny/tranches/sk-v13/SYNTHESIS.md:226`-`230`,
#   `restart/audit/totality/p2/2C-grammar-neutrality.md:188`,
#   `restart/audit/totality/p2/2F-parse-that-gaps.md:249`.
```

The Lock 14 v+1 closing clause (line 259-260) is the operative CH2 V2 admission gate. Per §1.5 below, all 39 active cross-axis candidates name CSS L4, Sheets, or BBNF-self consumers in their respective §3 grammar-neutrality tables; the V1-cycle NEUTRAL-PENDING-CONSUMER flag on P2-F C8 has been discharged via demotion to §2.X.1 per `[no-deferrals]` (no same-wave consumer committed in V2 wave plan; C8 retains gap-note identifier for cross-tranche stability with explicit re-promotion gate). Zero V2 active candidates carry a PENDING-CONSUMER flag.

### §1.3 — Cross-axis source-symbol reproduction at V2 HEAD

```bash
grep -n "fn scan_structurals\|fn dispatch_value\|fn match_tiny_plain_string\|fn parse_number_direct\|fn parse_object_value_at_direct\|fn parse_array_element_at_direct\|fn unescape_string\|fn read_hex_unit_scalar\|fn bulk_emit_positions_64_neon" \
  skinny/crates/runtime/src/grammars/json/generated.rs \
  skinny/crates/runtime/src/grammars/json/scan.rs \
  skinny/crates/parse-that-regex/src/lib.rs \
  skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs

# scan.rs:22  pub fn scan_structurals
# scan.rs:32  pub fn scan_structurals_scalar
# generated.rs:45  fn dispatch_value
# generated.rs:159 fn match_tiny_plain_string
# generated.rs:164 fn match_tiny_plain_string_direct
# generated.rs:169 fn match_tiny_plain_string_with_cap<const CAP: usize>
# generated.rs:466 fn parse_object_value_at_direct<'i, S: JsonSink>
# generated.rs:506 fn parse_array_element_at_direct<'i, S: JsonSink>
# generated.rs:650 fn parse_number_direct<'i, S: JsonSink>
# bulk_emit_positions_64.rs:2  pub unsafe fn bulk_emit_positions_64_neon
# parse-that-regex/src/lib.rs:718 pub fn unescape_string
# parse-that-regex/src/lib.rs:945 fn read_hex_unit_scalar
```

**Result:** every cited bbnf source symbol underlying a V2 active candidate primitive reproduces at HEAD. The V2 axis files' cross-axis cites are source-reproducible; the CH2 V2 verdict surface has no symbol-misidentification risk. P2-F C6 cites `generated.rs:45` for `dispatch_value`; P2-F C2 cites `parse-that-regex/src/lib.rs:718` for `unescape_string` (the canonical CH2 violation site per §3 row C3 + §3 note 1); P2-F C9 cites `bulk_emit_positions_64.rs:2` for the NEON tape-emit substrate primitive.

### §1.4 — Cross-grammar consumer evidence reproduction at V2 HEAD (BBNF source files)

```bash
grep -n "^number = \|^string = \|^identifier = \|^literal = \|^big_comment = \|^comment = \|^error_literal = " \
  grammar/css/l4/tokens.bbnf grammar/css/l4/value-unit.bbnf \
  grammar/google-sheets/google-sheets.bbnf grammar/bbnf/bbnf.bbnf

# css/l4/tokens.bbnf:9  string = /"(?:[^"\\]|\\[\s\S])*"/ | /'(?:[^'\\]|\\[\s\S])*'/
# css/l4/value-unit.bbnf:15  number = /[-+]?(\d+(\.\d+)?|\.\d+)([eE][-+]?\d+)?/ -> f64
# bbnf/bbnf.bbnf:9   identifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ -> Span
# bbnf/bbnf.bbnf:11  literal = ( "\"" , /(\\.|[^"\\])*/ , "\""  (3-quote disjunction continues)
# bbnf/bbnf.bbnf:17  big_comment = ( "/*" , /[^\*]*/ , "*/" ) ?w -> Span
# bbnf/bbnf.bbnf:18  comment = ( "//" , /.*/ ) ?w -> Span
# google-sheets.bbnf:6   number = /(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?/ -> f64
# google-sheets.bbnf:12  string = /"([^"]|"")*"/ -> input : Span  (doubled-quote escape)
# google-sheets.bbnf:34  error_literal = "#N/A" -> 0u8 | ...
# google-sheets.bbnf:90  identifier = /[A-Za-z_][A-Za-z0-9_.]*/ -> input : Span
```

**Result:** the cross-grammar consumer evidence base for the CH2 V2 generalisation argument reproduces at HEAD. CSS L4 / Sheets / BBNF-self primitive shapes (identifier, string with N-quote disjunction, number with leading-dot policy, comment markers, doubled-quote escape) are spec-and-source-pinned per P2-F §1.2 grammar-source citations + P2-E §3 table. The CH2 F2 binding (CSS L4 spec evidence + JSON profile evidence jointly, without CSS L4 profile corroboration) is correctly applied across all six V2 P2 axis files; CSS L4 row at p2c:69 + p2c:75 reaffirms the CH2 F2 binding for the demoted C-P2C-1 row (PRUNE-2 CSS L4 plane rebuild gating is CH4 timing, not CH2 generality).

### §1.5 — V2 cross-axis P2-F-verdict-bucket consolidation (R1 discharge)

The V1 R1 refinement (cross-axis candidate-ID reconciliation table) is discharged below. The P2-F V2 §1.1 five-bucket partition (NEUTRAL-WIRED, NEUTRAL-CONFIG-DRIVEN, NEUTRAL-PENDING-CONSUMER, JSON-OVERFIT-REFRAMABLE, JSON-OVERFIT-IRREDUCIBLE) is the V2 CH2 arbiter binding. Cross-axis mapping per V2 candidate census:

| P2 axis | Candidate | V2 status | P2-F verdict bucket equivalence | Witness | CH2 V2 disposition |
|---|---|---|---|---|---|
| P2-A C1 | `lazy_field_skip_with_index` | V1-LOCKED active | NEUTRAL-CONFIG-DRIVEN | p2a:117-119 | ACCEPT |
| P2-A C2 | `long_string_body_simd_scan` | V1-LOCKED active | NEUTRAL-CONFIG-DRIVEN ≡ P2-F C1+C2 (per §2.Y NF-CH6-4) | p2a:127-130 | ACCEPT |
| P2-A C3 | `digit_block_simd_accumulate` | V1-LOCKED active | NEUTRAL-CONFIG-DRIVEN ≡ P2-F C5 | p2a:137-140 | ACCEPT |
| P2-A C4 | `force_inline_lto_envelope_discipline` | V1-LOCKED active | NEUTRAL-WIRED ≡ P2-F C14 | p2a:147-150 | ACCEPT |
| P2-A C5 | `structural_index_singular_substrate_consumer` | V1-LOCKED active | NEUTRAL-CONFIG-DRIVEN ≡ P2-D C-P2D-1 + Lock 1 substrate-union | p2a:157-159 | ACCEPT |
| P2-A C6 | `parse_attribution_envelope_cracker` (process) | V1-LOCKED active | NEUTRAL-WIRED process-gate ≡ P2-C C-P2C-8 + P2-F C6 | p2a:167-169 | ACCEPT |
| P2-A C7 | `unicode_escape_neon_nibble_decode` | V1-LOCKED active | NEUTRAL-CONFIG-DRIVEN ≡ P2-F C3 + P2-C C-P2C-4 + P2-E Gap 2 | p2a:177-179 | ACCEPT |
| P2-B §2.A-E | 5-stage admission process | V2 amended (Fold-1 SHA pinning) | grammar-neutral by construction; each stage carries explicit Lock-14 verdict | p2b:156-162 | ACCEPT |
| P2-C C-P2C-1 | `ascii_set_member64_css_delimiter` | **V2 demoted to §2.X (Fold-2)** | NEUTRAL-CONFIG-DRIVEN ≡ P2-F C1 specialised to CSS delimiter; CH2 verdict ACCEPT, CH4 timing gate on PRUNE-2 successor wave | p2c:69 (§2.X row) + p2c:77 (§3 row) | ACCEPT (CH2 generality intact; CH4 timing gate clarified per V1 R2) |
| P2-C C-P2C-2 | `pmull_cssc_structural_union_emit64` | V1-LOCKED active | NEUTRAL-CONFIG-DRIVEN gated on substrate-union YES ≡ P2-F C2 + C9 | p2c:42 | ACCEPT (pre-blocked by REDRESS 88+89+96-98) |
| P2-C C-P2C-3 | `udot_digit_span_x4` | V1-LOCKED active | NEUTRAL-CONFIG-DRIVEN ≡ P2-F C5 + P2-E Gap 5 | p2c:43 | ACCEPT (NOT-S-P3-ELIGIBLE pending F-V2-P1ABC-RERECORD per CH2/CH4 dual-gate) |
| P2-C C-P2C-4 | `tbl_tbx_escape_decode_batch` | V1-LOCKED active | NEUTRAL-CONFIG-DRIVEN partial ≡ P2-F C3 + P2-A C7 + P2-E Gap 2 | p2c:44 | ACCEPT (S-P3-eligible for JSON `\uXXXX`) |
| P2-C C-P2C-5 | `string_special_64_context` | V1-LOCKED active | NEUTRAL-CONFIG-DRIVEN iff quote/escape/control policy via GrammarConfig ≡ P2-F C2 + P2-E Gap 1+6 | p2c:45 | ACCEPT (support primitive) |
| P2-C C-P2C-6 | `eor3_string_mask_fusion` | **V2 demoted to §2.X (Fold-2)** | NEUTRAL-WIRED ≡ P2-F C13 BCAX; bit-mask algebra grammar-neutral | p2c:70 (§2.X row) + p2c:82 (§3 row) | ACCEPT (inventory only; demotion stamp Lock-14-honest) |
| P2-C C-P2C-7 | `byte_context_orphan_resolution` | **V2 demoted to §2.X (Fold-2)** | hygiene; grammar-neutral only via consumer wiring | p2c:71 (§2.X row) + p2c:83 (§3 row) | ACCEPT (close-hygiene only) |
| P2-C C-P2C-8 | `parse_attribution_profile_rebuild_gate` | V1-LOCKED active | NEUTRAL-WIRED process-gate ≡ P2-A C6 + P2-F C6 | p2c:46 | ACCEPT (process-gate prerequisite; CH2/CH4 dual-gated per V2) |
| P2-D C-P2D-1 | `BackendShape::SinkOnly` activation | V1-LOCKED active | NEUTRAL-CONFIG-DRIVEN YES ≡ P2-A C5 + P2-F C11 | p2d:148 | ACCEPT |
| P2-D C-P2D-2 | `OffsetTapeStats` column extension | V1-LOCKED active | NEUTRAL-WIRED YES (schema-level measurement) | p2d:149 | ACCEPT |
| P2-D C-P2D-3 | sparse-flag-band gating | **V2 demoted to §1.6(d)** | YES grammar-neutral (substrate field grammar-neutral; gating grammar-policy-free); §3 row marked N/A-DEMOTED-V2 | p2d:104 (§1.6(d) prose) + p2d:150 (§3 N/A row) | ACCEPT (demotion preserves grammar-neutrality verdict; re-elevation gate explicit) |
| P2-D C-P2D-4 | `BackendShape::EventTape` interrogation | V1-LOCKED pre-blocked | N/A — REJECT-by-REDRESS-96/97/98 | p2d:151 | ACCEPT (correct REJECT framing) |
| P2-E Gap 1 | `scan_string_special_block_sweep_64` | V1-LOCKED active | grammar-neutral — universal quoted-string vocabulary ≡ P2-F C2 + P2-A C2 (§2.Y NF-CH6-4) | p2e:237 | ACCEPT |
| P2-E Gap 2 | `unescape_uxxxx_x8_neon` | V1-LOCKED active | grammar-neutral with classifier ≡ P2-F C3 + P2-A C7 + P2-C C-P2C-4 | p2e:238 | ACCEPT |
| P2-E Gap 3 | `ascii_whitespace_skip_64` | V1-LOCKED active | grammar-neutral — byte-set parameterised ≡ P2-F C7 | p2e:239 | ACCEPT |
| P2-E Gap 4 | `utf8::validate_block_streaming` | V1-LOCKED active | grammar-neutral — UTF-8 belongs in NO grammar | p2e:240 | ACCEPT |
| P2-E Gap 5 | `parse_16_digits_dotprod` | V1-LOCKED active | grammar-neutral ≡ P2-F C5 + P2-A C3 + P2-C C-P2C-3 | p2e:241 | ACCEPT |
| P2-E Gap 6 | `scan_string_with_carry_64` | V1-LOCKED active | grammar-neutral — even/odd-backslash invariant ≡ P2-F C2 (§2.2 NF-CH6-3 upgrade) | p2e:242 | ACCEPT |
| P2-E Gap 7 | `scan_digit_run_simd_64` | V1-LOCKED active | grammar-neutral via 7.5 range primitive | p2e:243 | ACCEPT |
| P2-E Gap 7.5 | `byte_class_from_range_64` | V1-LOCKED active | grammar-neutral — pure range-test primitive | p2e:244 | ACCEPT |
| P2-E Gap 8 | `utf8_codepoint_scan_64` | V1-LOCKED active | grammar-neutral — UTF-8 substrate | p2e:245 | ACCEPT |
| P2-F C1 | structural-byte SIMD classify | V1-LOCKED active | NEUTRAL-CONFIG-DRIVEN | p2f:247 | ACCEPT |
| P2-F C2 | quoted-string PMULL prefix-XOR | V1-LOCKED active (§2.2 NF-CH6-3 oracle-cite upgrade) | NEUTRAL-CONFIG-DRIVEN | p2f:248 | ACCEPT |
| P2-F C3 | escape canonicalisation | V1-LOCKED active | NEUTRAL-CONFIG-DRIVEN; hardcoded JSON alphabet at `parse-that-regex/src/lib.rs:718` flagged as CH2-violation-to-be-lifted | p2f:249 | ACCEPT |
| P2-F C4 | tiny-keyword-set match | V1-LOCKED active | NEUTRAL-CONFIG-DRIVEN | p2f:250 | ACCEPT |
| P2-F C5 | digit-block decode | V1-LOCKED active | NEUTRAL-CONFIG-DRIVEN | p2f:251 | ACCEPT |
| P2-F C6 | branch-on-first-byte dispatch | V1-LOCKED active (Fold-3 antecedent stamp at p2f:123) | NEUTRAL-CONFIG-DRIVEN (parse-attribution gated per F-V2-P1ABC-RERECORD) | p2f:252 + p2f:300 (§4 risk row) | ACCEPT |
| P2-F C7 | whitespace prefix skip | V1-LOCKED active (Fold-3 antecedent stamp at p2f:135) | NEUTRAL-CONFIG-DRIVEN | p2f:253 | ACCEPT |
| P2-F C8 | comment-skip primitive | **V2 demoted to §2.X.1 (Fold-2)** | non-candidate (was NEUTRAL-PENDING-CONSUMER at V1); §3 row marked DEMOTED V2 with explicit re-promotion gate | p2f:254 (§3 strikethrough row) + p2f:218-229 (§2.X.1 verbatim body) | ACCEPT (demotion discharges V1 PENDING-CONSUMER flag per `[no-deferrals]`) |
| P2-F C9 | offset-tape bulk emit | V1-LOCKED active | NEUTRAL-WIRED (Lock 1 substrate-union) | p2f:255 | ACCEPT |
| P2-F C10 | cross-chunk byte-context | V1-LOCKED active (Fold-4 scalar-ref target named at p2f:164) | NEUTRAL-WIRED (Lock 16 :285 declares verbatim) | p2f:256 | ACCEPT |
| P2-F C11 | substrate-walk-with-shape-validation | V1-LOCKED active | NEUTRAL-CONFIG-DRIVEN | p2f:257 | ACCEPT |
| P2-F C12 | keyword-set 16-byte alphabet | V1-LOCKED active (CF-1 reframe at p2f:184: ACCEPT not REVISE; scalar-ref EXISTS at `scan.rs:32`) | NEUTRAL-WIRED (Lock 16 :290 cross-grammar) | p2f:258 | ACCEPT |
| P2-F C13 | branchless 3-way XOR (BCAX) | V1-LOCKED active (Fold-5 scalar-ref target named at p2f:197) | NEUTRAL-WIRED (Lock 16 :289 config-free) | p2f:259 | ACCEPT |
| P2-F C14 | i-cache budget constraint | V1-LOCKED active | NEUTRAL-WIRED (hardware fact) | p2f:260 | ACCEPT |

**V2 verdict surface: 39 active + 6 demoted/pre-blocked = 45 cross-axis entries; every entry carries a P2-F-verdict-bucket assignment that maps to ACCEPT under CH2 V2.** Zero REVISE, zero REJECT. Zero JSON-OVERFIT-IRREDUCIBLE candidates across the entire P2 V2 surface. The V2 fold packets did not introduce JSON-only divergence on any axis; demotions preserved grammar-neutrality verdicts verbatim with explicit demotion stamps.

### §1.6 — F-V2-P1ABC-RERECORD CH2/CH4 dual-gating verification (V2 fold target)

The HARDENING-S-P2-V1-CONSOLIDATED §2.1 binding entry (lines 230-289) names the V2 dual-gating promotion. CH2 V2 verification: per-candidate dual-gate propagation in V2 axis files:

| Candidate | V1 CH2 disposition | V2 dual-gate propagation site | Status |
|---|---|---|---|
| P2-A C6 (envelope-cracker) | process-gate ≡ rerun | V1-LOCKED at p2a:167-169 (process IS the rerun) | held |
| P2-C C-P2C-3 (UDOT) | NOT-S-P3-ELIGIBLE pending rerun | p2c:43 disposition row names rerun + p2c:161 fold target | held |
| P2-C C-P2C-8 (parse-attribution gate) | process-gate ≡ rerun | p2c:46 + p2c:164 fold target (Stage-0 same-wave) | held |
| P2-E Gap 1 (envelope-masked string inner) | inheritance from p2e:237 grammar-neutral verdict + §4.7 rerun-dependency | V1-LOCKED at p2e:235-247 | held |
| P2-E Gap 3 (envelope-masked whitespace) | V1-LOCKED at p2e:239 + §4.7 | held |
| P2-E Gap 4 (envelope-masked UTF-8) | V1-LOCKED at p2e:240 + §4.7 | held |
| P2-E Gap 5 (envelope-masked numeric) | V1-LOCKED at p2e:241 + §4.7 + REDRESS-80 differential | held |
| P2-F C6 (dispatch primitive) | NEUTRAL-CONFIG-DRIVEN (parse-attribution gated) | p2f:123 Fold-3 antecedent stamp + p2f:130 CH2 binding + p2f:300 §4 risk row explicitly names F-V2-P1ABC-RERECORD as inherited carry-forward + dual-gate S-P3 verification clause | held |
| P2-F C7 (whitespace-skip indirect) | NEUTRAL-CONFIG-DRIVEN | p2f:135 Fold-3 antecedent stamp ("admit-gate conditional on F-V2-P1ABC-RERECORD") | held |
| P2-F C10 (cross-chunk byte-context indirect) | NEUTRAL-WIRED | p2f:162 Fold-3 antecedent stamp | held |
| P2-F C12 (keyword-set 16-byte indirect) | NEUTRAL-WIRED (CF-1 reframed) | p2f:184 Fold-3 antecedent stamp + CH4-ACCEPT reframe | held |
| P2-F C13 (BCAX 3-way XOR indirect) | NEUTRAL-WIRED | p2f:195 Fold-3 antecedent stamp | held |

**Result:** F-V2-P1ABC-RERECORD CH2/CH4 dual-gating propagated to **all 12 consumer-dependency candidates** named in HARDENING-S-P2-V1-CONSOLIDATED §2.1 (consumer dependency list lines 252-271). Zero V2 edits silently relax the dual-gate. The P2-F §4 risk row at p2f:300 explicitly carries the "Inherited V2 carry-forward F-V2-P1ABC-RERECORD per dispatch context §1" framing with the dual-gate S-P3 verification clause ("S-P3 must ensure the wave that admits C6 carries the parse-attribution rerun in the same wave"). The §2.X.1 C8 re-promotion gate at p2f:229 explicitly names F-V2-P1ABC-RERECORD as the (a) re-promotion condition. The CH2/CH4 dual-gating is fully ratified at V2.

## §2 — Per-artefact V2 findings

### §2.1 — P2-F (load-bearing CH2 arbiter V2): ACCEPT

P2-F V2 is the V2 CH2 arbiter. V2 §3 verdict tally at p2f:266-275 reports the V2 binding numbers:

| Verdict | V1 count | V2 count | Candidates (V2) |
|---|---:|---:|---|
| NEUTRAL-WIRED | 5 | 5 | C9, C10, C12, C13, C14 |
| NEUTRAL-CONFIG-DRIVEN | 8 | 8 | C1, C2, C3, C4, C5, C6, C7, C11 |
| NEUTRAL-PENDING-CONSUMER | 1 (C8) | **0** | — (V1 C8 demoted to §2.X.1 non-candidate inventory per Fold-2) |
| JSON-OVERFIT-REFRAMABLE | 0 | 0 | — |
| JSON-OVERFIT-IRREDUCIBLE | 0 | 0 | — |
| Demoted to non-candidate inventory (V2) | n/a | 1 | C8 (see §2.X.1) |

**All 13 active V2 candidates clear the Lock 14 v+1 admission gate.** The CSS L4 / Sheets / BBNF-self consumer-existence summary at p2f:279-282 names **13/13 CSS L4 consumers, 13/13 Sheets consumers, 13/13 BBNF-self consumers** for the active candidate surface — a tightening from V1's 14/14 + 13/14 + 14/14 mixed coverage because C8 (the lone V1 PENDING-CONSUMER + the only candidate without a Sheets consumer) is now demoted. Every V2 active candidate has at least one non-JSON consumer; every V2 active candidate has consumers in **all three** non-JSON grammars. The Lock 14 v+1 binding holds for every V2 active candidate without the V1 PENDING-CONSUMER asymmetry.

The §3 note 1 at p2f:262-264 (Sheets doubled-quote escape) correctly identifies the canonical Lock 14 v+1 enforcement target — the `parse-that-regex::StringFlags::HAS_ESC` DELTA-NOTE at `sk-v14-audit-overfit-lock14-scan.md:9` — and binds the S-P3 wave to rename + lift. The CH2 GENERALITY discharge is structurally complete; the V2 disposition is **ACCEPT**.

The §2.Y cross-axis tracking note (NF-CH6-4) at p2f:231-239 preserves grammar-neutrality across the three colliding axes (P2-A C2, P2-E Gap 1, P2-F C1+C2) by binding the S-P3 wave plan to one canonical primitive name + one canonical scalar reference function rather than three orthogonal SIMD bodies for the same `unescape_string` rank-1 leaf. This is the exemplary anti-paper-close pattern + Lock 14 v+1 enforcement at the cross-axis consolidation level — strengthening CH2 V2 by preventing future cross-axis grammar-neutrality drift.

### §2.2 — P2-A (SOTA teardown, V1-LOCKED): ACCEPT

V1-LOCKED. Zero V2 drift per §1.1 git-diff. V1 CH2 disposition holds verbatim: 7/7 SOTA-derived candidates carry per-candidate GENERALISES verdicts at p2a:119, 129, 139, 149, 159, 169, 179; §3 grammar-neutrality table at p2a:205-213 maps each candidate to per-grammar consumer columns; CH2 F2 binding correctly applied (CSS L4 spec-only generalisation explicitly named at p2a:203 + p2a:217 closing paragraph). **ACCEPT**.

### §2.3 — P2-B (dav1d process, V2 amended Fold-1): ACCEPT

V2 amended via Fold-1 SHA pinning at §5.1 lines 183-185 (FFmpeg `085714182302333dd83dcb9c36cf828dc4eba929` + dav1d `1718ff9aded99f0a89f5c7940d6afb8948301e33`, both inherited verbatim from P2-A §5.3 anchors per V1 consolidator §5.4→§5.3 register-correction inheritance). The Fold-1 edit is cite-discipline, not grammar-neutrality-content; the 5-stage admission process A-E + §3 lines 156-160 per-stage Lock 14 verdicts are unchanged from V1:

- Stage A (scalar reference) — grammar-neutral by construction per Lock 14 v+1 line 255-263.
- Stage B (checkasm cell) — grammar-neutral with mandatory non-JSON fixture extension per Lock 14 v+1 closing clause.
- Stage C (Lock 16 cite) — grammar-neutral by construction per Lock 16 :285-288.
- Stage D (same-wave consumer) — per-grammar evidence with `grammar_scope` tag {`json-only`, `csl4-witnessed`, `sheets-witnessed`, `bbnf-self-witnessed`, `fleet-wide`}.
- Stage E (manifest + substrate) — grammar-neutral by construction; manifest is the audit surface.

The composite verdict at p2b:162 binds the CH2 enforcement mechanism unchanged. **ACCEPT**.

### §2.4 — P2-C (arch esoterica, V2 amended Fold-2 demotions): ACCEPT

V2 amended via Fold-2 demotion of C-P2C-1 / C-P2C-6 / C-P2C-7 to `§2.X — Non-candidate inventory` per p2c:48-71. Active candidate count 8 → 5 (C-P2C-2, -3, -4, -5, -8 active; C-P2C-1, -6, -7 demoted; full technical content preserved verbatim at p2c:69-71 with disposition stamp template at p2c:64-65). The §3 grammar-neutrality table at p2c:75-84 carries verdicts for **all 8 candidates** (active + demoted), preserving the Lock-14-honest verdict surface across the demotion:

- Active C-P2C-2/-3/-4/-5/-8: per-row verdicts at p2c:78-79, 79, 80, 81, 84.
- Demoted C-P2C-1/-6/-7: per-row verdicts preserved at p2c:77, 82, 83 (Generalisable byte-set primitive; Generalisable bit-mask algebra; Hygiene-only — all Lock-14-correct).

The V1 R2 refinement (C-P2C-1 disposition language separating CH2 verdict from CH4 timing gate) is discharged via Fold-2: the demotion explicitly partitions CH2 acceptance ("Generalisable byte-set primitive" verdict at p2c:77) from CH4 timing gate ("Demoted V2: zero S-P1 hot-leaf antecedent at SK-V14; re-evaluate if F-V2-P1ABC-RERECORD surfaces antecedent" at p2c:69). The CH2 generality is independently affirmed; the wave-timing question is delegated to the §2.X re-elevation gate. **ACCEPT**.

C-P2C-5 hardcoded-JSON CH2-violation pointer at p2c:81 (`bbnf-simd/src/aarch64/classify_tbl4.rs:33-35`) preserved verbatim — the precise CH2 violation surfacing the dispatch context demands; the lift-target remains explicit for S-P3 wave planning. C-P2C-4 partial-generalisability flag at p2c:80 (TBL hex-nibble core neutral; escape-language wrapper per-grammar) Lock-14-honest, not papered over.

### §2.5 — P2-D (substrate + tape, V2 amended Fold-2 demotion): ACCEPT

V2 amended via Fold-2 demotion of C-P2D-3 (sparse-flag-band gating) to §1.6(d) substrate-side observation per p2d:94-104. Active candidate count 3 → 2 (C-P2D-1, C-P2D-2 active; C-P2D-3 demoted to §1.6(d); C-P2D-4 pre-blocked anti-pattern). The §3 grammar-neutrality table at p2d:148-151 binds the substrate-side candidates:

- C-P2D-1: **YES grammar-neutral** — substrate-mode selection, not JSON-specific.
- C-P2D-2: **YES grammar-neutral** — `OffsetTapeStats` lives at `runtime::tape::*`, not `runtime::generated_json::*`.
- C-P2D-3: **N/A — DEMOTED V2 → §1.6(d)** — verdict row preserves "(was: none — `flag_cursors`/`flag_values` are substrate fields)" with explicit DEMOTED V2 marker; demotion does not erase the grammar-neutrality verdict.
- C-P2D-4: **N/A (REJECT-by-REDRESS-96/97/98)** — anti-pattern reference for CH3/CH5 cross-checking.

The §1.6(d) prose at p2d:104 carries the full technical content of the former C-P2D-3 row including the explicit grammar-neutrality affirmation: "Grammar-neutrality: HIGH — the substrate field is already grammar-neutral; the gating is grammar-policy-free." The demotion preserves the CH2 verdict; the candidate identifier C-P2D-3 is retained as a gap-note at p2d:128-130 for cross-tranche reference stability.

P2-D's §3 closing line at p2d:153 affirms: "none of the two active candidates carries a JSON-grammar match arm, JSON-named module, JSON-specific type in a generic-crate public API, or JSON-keyed feature flag." This is the Lock 14 binding verification at p2d:153 explicitly verifying the four enumerated Lock 14 prohibitions at LOCKS.md:220. The CSS L4 spec-evidence consistency per CH2 F2 binding is preserved at p2d:153 closing prose. **ACCEPT**.

### §2.6 — P2-E (parse-that gaps, V1-LOCKED): ACCEPT

V1-LOCKED. Zero V2 drift per §1.1 git-diff. V1 CH2 disposition holds verbatim: all 9 gaps (Gap 1-8 + Gap 7.5) carry per-gap JSON / CSS L4 / Sheets / BBNF-self consumer columns at p2e:235-247; §3 closing line at p2e:247 ("All 8 gaps are grammar-neutral; none requires per-grammar specialization at the primitive layer") is Lock 14 binding verification operationalised at the Layer-1 primitive boundary. §4.3 Lock-14-risk mitigation at p2e:272 ("Layer-1 primitives carry NO defaults — the byte-set / range parameter is mandatory; the per-grammar default lives at the codegen template") is the canonical Lock 14 v+1 enforcement statement and remains unchanged. **ACCEPT**.

### §2.7 — Cross-axis CH2 V2 convergence (all six artefacts)

Per the CHALLENGE-CONTEXT V2 §2 CH2 line ("Lock 14 v+1 holds across all amended axes; demotions did not introduce JSON-only divergence"):

| Lock 14 v+1 binding aspect | P2-A | P2-B | P2-C (V2) | P2-D (V2) | P2-E | P2-F (V2) |
|---|---|---|---|---|---|---|
| Per-candidate grammar-neutrality verdict | §3 lines 205-213 (7) | §3 lines 156-160 (5 stages) | §3 lines 75-84 (8 entries: 5 active + 3 demoted with verdicts preserved) | §3 lines 148-151 (4 entries: 2 active + 1 demoted-with-N/A + 1 pre-blocked) | §3 lines 235-247 (9 gaps) | §3 lines 247-260 (14 entries: 13 active + 1 demoted-with-strikethrough) |
| Lock 14 v+1 at-least-one-non-JSON-consumer | §3 spec-evidence columns | Stage B mandate | per-row JSON/CSS/Sheets/BBNF columns intact across demotion | C-P2D-1/2 substrate-level all grammars | per-gap JSON/CSS/Sheets/BBNF columns | §3 13/13 CSS + 13/13 Sheets + 13/13 BBNF-self (V2 active surface) |
| Hardcoded-policy CH2 violation surfacing | n/a (process-level) | Stage A mandate | C-P2C-5 names `classify_tbl4.rs:33-35` JSON constants (preserved) | n/a (substrate policy-free) | §4.3 "Layer-1 primitives carry NO defaults" | §3 line 264 names `parse-that-regex/src/lib.rs:718` + `StringFlags::HAS_ESC` DELTA-NOTE |
| CSS L4 spec-only generalisation (CH2 F2) | §3 line 203 explicit | §3 line 158 generic | demoted C-P2C-1 disposition separates CH2 verdict from CH4 PRUNE-2 timing | §3 line 153 "by construction of the substrate" | §3 line 249 spec-grounded | §0 line 12 + §3 line 247-260 explicit |
| V2 demotion preservation | n/a (V1-LOCKED) | n/a | §3 table rows 77/82/83 intact for demoted C-P2C-1/6/7 | §3 row 150 intact for demoted C-P2D-3 with N/A-DEMOTED-V2 marker | n/a (V1-LOCKED) | §3 row 254 intact (strikethrough + V2 demotion note) for C8; §2.X.1 verbatim body at p2f:218-229 |
| Cross-axis consolidation | §2.Y reference (P2-A C2) | n/a | n/a | n/a | §2.Y reference (P2-E Gap 1) | §2.Y NF-CH6-4 binding (P2-F C1+C2 ≡ P2-A C2 ≡ P2-E Gap 1) |

All six independent P2 axis artefacts converge at V2 on the same Lock 14 v+1 binding: (a) every candidate (active or demoted) carries a grammar-neutral verdict; (b) at-least-one-non-JSON-consumer is named across all 13/13/13 active P2-F V2 candidates; (c) hardcoded JSON policy is surfaced for lifting where it exists; (d) CSS L4 generalisation is spec-grounded per CH2 F2; (e) V2 demotions preserve verdicts verbatim; (f) cross-axis consolidation (§2.Y NF-CH6-4) prevents grammar-neutrality drift across three colliding axes.

The dispatch-context V2 §2 CH2 V2 binding is satisfied with **six-witness redundancy** at the V2 cycle. The P2-F V2 load-bearing arbiter's 13/13 active admission count is independently corroborated by the four sibling axes' 29 active candidate/stage/gap verdicts (7 P2-A + 5 P2-B + 5 P2-C active + 2 P2-D active + 9 P2-E + 1 P2-D pre-blocked + 1 P2-F demoted retained for §2.X.1). No JSON-OVERFIT-IRREDUCIBLE candidates across any axis at V2. The V2 CH2 GENERALITY lens discharge is structurally complete and tighter than V1 (V1 PENDING-CONSUMER asymmetry retired via Fold-2 C8 demotion).

## §3 — Critical V2 findings (none warrant REVISE; two new V2-cycle findings ratify V1 trajectory)

### §3.1 — New V2 finding F-V2-CH2-1: §2.Y NF-CH6-4 cross-axis tracking note strengthens CH2 GENERALITY

The P2-F V2 §2.Y cross-axis tracking note at p2f:231-239 (NF-CH6-4 long-string-body SIMD scan consolidation) is a new V2-cycle artefact-level pattern strengthening CH2 GENERALITY. The note binds three axes (P2-A C2 `long_string_body_simd_scan`, P2-E Gap 1 `scan_string_special_block_sweep_64`, P2-F C1+C2 quote-aware classifier composition) to ONE canonical primitive name + ONE canonical scalar reference function at S-P3 admission time. Without this consolidation, three orthogonal SIMD bodies could be admitted for one underlying primitive, each potentially with its own per-grammar-config drift — a Lock 14 v+1 violation surface.

**CH2 V2 significance:** the §2.Y note is the cross-axis Lock 14 v+1 enforcement pattern. Each of the three colliding axes carries its own grammar-neutrality verdict (P2-A C2 GENERALISES; P2-E Gap 1 grammar-neutral; P2-F C1+C2 NEUTRAL-CONFIG-DRIVEN); the note ensures the S-P3 wave plan does not silently admit three different config schemas for the same underlying primitive (which would re-fragment grammar-neutrality despite each individual admission being Lock-14-compliant). This is **paper-close prevention at the cross-axis consolidation layer** — exactly the kind of CH2 GENERALITY safeguard the dispatch context §2 CH6 row ("CH6 ANTI-PAPER-CLOSE: P2-F NF-CH6-4 cross-axis tracking note is exemplary anti-paper-close pattern (3 axes naming same primitive — S-P3 binding to one canonical name)") names. The CH2 V2 discharge inherits this strengthening; **finding is non-blocking, ratifies the V2 cycle trajectory toward LOCK**.

### §3.2 — New V2 finding F-V2-CH2-2: F-V2-P1ABC-RERECORD CH2/CH4 dual-gating fully propagated at V2

The V1 finding F2 ("parse-attribution rerun is co-required by CH2 verdicts on C6 / C-P2C-3 / Gap 5") proposed elevating F-V2-P1ABC-RERECORD from CH4-only to CH2/CH4 dual-gated. V2 ratification: per §1.6 above, the dual-gate is propagated to **all 12 consumer-dependency candidates** named in HARDENING-S-P2-V1-CONSOLIDATED §2.1 lines 252-271; the P2-F §4 risk row at p2f:300 explicitly carries the "Inherited V2 carry-forward F-V2-P1ABC-RERECORD" framing with the dual-gate S-P3 verification clause; the §2.X.1 C8 re-promotion gate at p2f:229 explicitly names F-V2-P1ABC-RERECORD as the (a) re-promotion condition; the P2-C §2.X demotion stamp template at p2c:63-65 names "F-V2-P1ABC-RERECORD CH2/CH4 dual-gate per `HARDENING-S-P2-V1-CONSOLIDATED.md:230-289`" as the re-evaluation surface for the three demoted P2-C candidates.

**CH2 V2 significance:** the dual-gating is now operationally binding across the V2 P2 axis surface. No V2 edit silently relaxes the dual-gate; instead, the dual-gate is the explicit dispatch-context inheritance carry-forward for every dispatch-envelope-internal primitive candidate. **Finding is non-blocking, ratifies V2 fold-packet completion**; the V1 F2 trajectory is fully realised at V2.

### §3.3 — V1 R1 (cross-axis candidate-ID reconciliation) discharged at V2

V1 R1 named the cross-axis candidate-ID reconciliation table as a V2 fold deliverable. §1.5 above is the explicit reconciliation table mapping every V2 active + demoted/pre-blocked entry across all six P2 axis files to its P2-F-verdict-bucket equivalence. P2-F §1 footnote at p2f:10 ("any new candidate they surface that this fold does not enumerate is per-cycle absorbed under the same CH2 verdict template") is now retired by the §1.5 explicit table; the expected-small divergence has manifested as documented in §1.5 with full cross-axis ≡ mappings. **R1 discharged**; the footnote at p2f:10 can be considered superseded by the V2 hardening verification record (though the footnote prose is V1-LOCKED text and need not be edited).

### §3.4 — V1 R2 (P2-C C-P2C-1 CSS-L4 disposition language) discharged at V2

V1 R2 named the C-P2C-1 disposition language refinement (separating CH2 verdict from CH4 timing gate). Fold-2 demotion accomplishes this structurally: the §2.X — Non-candidate inventory partition at p2c:48-71 explicitly separates CH2 acceptance (the §3 row at p2c:77 affirms "Generalisable byte-set primitive" verdict unchanged) from the CH4 wave-timing gate (the §2.X disposition stamp template at p2c:64-65 names the F-V2-P1ABC-RERECORD re-evaluation surface; the per-row demotion stamps at p2c:69-71 carry the unified "Demoted V2: zero S-P1 hot-leaf antecedent at SK-V14; re-evaluate if F-V2-P1ABC-RERECORD surfaces antecedent" language). The CH2 V2 verdict on C-P2C-1 is independently ACCEPT (grammar-neutral); the CH4 timing gate is correctly delegated to the §2.X re-elevation gate. **R2 discharged**.

### §3.5 — V1 F1 (substrate-union YES six-witness corroboration) preserved at V2

V1 F1 named the substrate-union YES corroboration across six witnesses (P2-D §1.1/1.5/4.7; P2-F §1.3; CH5 V1; P1-V3-CH5 §3.78-83). V2 preservation: P2-D V2 §1.5 + §1.6(c) + §4.7 prose at p2d:84-92, 102, 199-201 unchanged (the §1.6(d) addition is a new substrate-side observation, not a substrate-union edit). P2-F V2 §1.3 holding assumption unchanged at p2f:52-54. The substrate-union-YES dependency holds at V2 HEAD; P2-F's V2 verdicts remain structurally defensible under Lock 1 substrate-union YES at V2 HEAD. **F1 preserved**.

## §4 — V2 fold recommendations (CH2-binding)

### §4.1 — V2 cycle status

All three V1 CH2 §4.1 mandatory actions are discharged at V2:

1. **R1 cross-axis candidate-ID reconciliation table** → discharged via §1.5 above (this CH2 V2 file).
2. **F-V2-P1ABC-RERECORD CH2/CH4 dual-gating promotion** → discharged via V2 axis-file propagation per §1.6 + F-V2-CH2-2.
3. **R2 P2-C C-P2C-1 disposition language refinement** → discharged via Fold-2 demotion structural separation per §3.4.

The V1 CH2 §4.2 non-blocking actions (4 + 5) remain non-blocking; the §4.2.4 cross-axis CH2-violation register can be authored as a V3 consolidator deliverable (not a V2 fold-packet blocker); the §4.2.5 CH5 V1 cross-reference is already discharged in HARDENING-S-P2-V1-CONSOLIDATED §2.2 (six-witness substrate-union YES corroboration).

### §4.2 — V2 → V3 trajectory forecast

CH2 V2 disposition: **100% ACCEPT** (6/6 artefacts; 39 active + 6 demoted/pre-blocked = 45 cross-axis entries; zero REVISE; zero REJECT). The lens has zero open REVISE items at V2; **V2 is the first ≥95% cycle** per `ORCHESTRATOR.md §3Z` "first ≥ 95% cycle on V2" target. V3 is forecast to repeat the same 100% ACCEPT verdict (since the V2 P2-F V2 fold packets close all V1 REVISEs cleanly and the demotion structural pattern is stable). The V3 work surface on CH2 is verification-only: re-confirm zero drift on V1-LOCKED axes; re-confirm V2 demotions remain stable; re-confirm dual-gating remains intact across any V3 V2-amended edits.

### §4.3 — §3Z LOCK criteria forecast

Per `ORCHESTRATOR.md §3Z` (≥ 95% × 2 consecutive cycles + zero orphan REVISEs):

- V1: 100% (six-artefact aggregate; zero orphan REVISEs; first cycle of chain).
- V2: 100% (six-artefact aggregate; zero orphan REVISEs; second cycle of chain).

**Predicted: CH2 V2 satisfies the "× 2 consecutive cycles" criterion at the per-lens level.** The lens-level CH2 V2 LOCK criterion is met; the cohort-level §3Z gate depends on aggregate ACCEPT-rate across all 7 lenses (the V1 sub-axis aggregate 93.1% was below floor due to CH1 + CH4 orphan REVISEs; V2 fold packets closed those orphans; V2 aggregate forecast per HARDENING-S-P2-V1-CONSOLIDATED §5.1 is ≈ 99.3%). CH2 V2 contribution to the cohort §3Z gate: **lens-level LOCK criterion met; awaiting cohort-level § aggregation**.

## §5 — Sources (verified against V2 HEAD commit `447a26b07`)

### §5.1 — Binding context (read in order)

- `restart/prompts/skinny/PASS-2-RESEARCH.md §3 CH2 GENERALITY` (lens definition)
- `restart/prompts/ORCHESTRATOR.md §3W lens registry + §3Z convergence rule + §8 non-negotiables` (Lock 1 substrate union; Lock 14 grammar-neutrality; scalar-reference + checkasm; same-wave consumer)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CHALLENGE-CONTEXT.md §0-§4` (V2 dispatch posture; CH2 V2 row at §2)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md §0-§6` (V1 aggregator + fold-packet authority + F-V2-P1ABC-RERECORD CH2/CH4 dual-gating binding entry)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CH2.md` (V1 CH2 carry-forward; 100% ACCEPT)
- `restart/locks/LOCKS.md:220-263` (Lock 14 + v+1 amendments — primary CH2 authority); `:48-90` (Lock 1 substrate-union v+1 manifest); `:265-340` (Lock 15 + Lock 16 abstract-primitive declarations)

### §5.2 — Artefacts disposition (per §0 V2 disposition table)

- `restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md:1-367` (V1-LOCKED; zero V2 drift)
- `restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md:1-217` (V2 amended Fold-1 §5.1 SHA pinning; §3 grammar-neutrality verdicts unchanged at lines 156-162)
- `restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md:1-164` (V2 amended Fold-2 demotion C-P2C-1/-6/-7 at §2.X lines 48-71; §3 verdicts intact at lines 75-84)
- `restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md:1-254` (V2 amended Fold-2 demotion C-P2D-3 at §1.6(d) lines 94-104; §3 verdicts intact at lines 148-151)
- `restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md:1-342` (V1-LOCKED; zero V2 drift)
- `restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md:1-360` (V2 amended Fold-2 C8 demotion + Fold-3 antecedent stamping + Fold-4/5 scalar-ref target naming + §2.Y NF-CH6-4 cross-axis tracking note + NF-CH6-3 §2.2 oracle upgrade)

### §5.3 — Source crosscheck (V2 HEAD-verified per §1)

- `skinny/crates/runtime/src/grammars/json/generated.rs:33-237` (parse-attribution `cfg_attr(feature = "parse-attribution", inline(never))` plumbing — the gate F-V2-P1ABC-RERECORD must enable); `:45, 159, 164, 169, 187, 213, 466, 506, 650` (envelope + every cited grammar-neutral primitive at V2 HEAD)
- `skinny/crates/runtime/src/grammars/json/scan.rs:22, 32, 107, 131, 164` (structural scan primitives + tape-emit sites)
- `skinny/crates/parse-that-regex/src/lib.rs:718, 945` (line 718 `unescape_string` is the canonical CH2 violation site per P2-F §3 row C3 + p2f:264 + p2f:249; line 945 `read_hex_unit_scalar`)
- `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:2` (`bulk_emit_positions_64_neon` — P2-F C9 substrate primitive)
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40` (`unescape_uxxxx_scalar` — P2-C C-P2C-4 + P2-E Gap 2 scalar reference)
- `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:5, 27` (`parse_4_digits` + `parse_4_digits_dotprod` — P2-C C-P2C-3 + P2-E Gap 5 scalar + SIMD pair)
- `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:33-35` (hardcoded JSON constants — P2-C C-P2C-5 CH2 violation pointer)
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:2742-2746, 2949-3003` (Track 2 `DirectParser` two-cursor independence + skip primitives)
- `grammar/css/l4/tokens.bbnf:9` (string with 2-quote disjunction — CSS L4 grammar-neutral primitive shape)
- `grammar/css/l4/value-unit.bbnf:15` (number with `-> f64` projection; leading-dot variant per file prose)
- `grammar/google-sheets/google-sheets.bbnf:6, 12, 34, 90` (number with leading-dot; string with doubled-quote escape; error_literal 9-keyword set; identifier byte-set)
- `grammar/bbnf/bbnf.bbnf:9, 11-13, 17-18` (identifier; literal with 3-quote disjunction; big_comment + comment markers)
- `restart/locks/LOCKS.md:255-263` (Lock 14 v+1 closing clause — the operative CH2 V2 admission gate; verified at HEAD)
- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-lock14-scan.md:9` (DELTA-NOTE on `parse-that-regex::StringFlags::HAS_ESC` JSON-flavored naming — the lift target per P2-F §3 note 1)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH5.md:78-83` (two-cursor independence verification — substrate-union YES upstream evidence preserved at V2 per F1)
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md:65-82, 92-108, 116-132, 140-141, 219-231, 246` (P1 CH2 vocabulary + per-corpus tables + §4.1 envelope census + §4.4 substrate-union typed observation)

### §5.4 — V2 cycle commit anchors

- V1 commit (S-P2 axis files at V1 baseline): `b3dbc5ca0e3ccf38df71a5e72be3d65a3068549b` per CHALLENGE-CONTEXT §2 V1-LOCKED audit clause.
- V2 commit (S-P2 V2 atomic micro-fold): `447a26b07` per dispatch context §1 + `git log --oneline -1 -- restart/skinny/tranches/sk-v14/research/p2/`.
- V2 diff stat: `git diff b3dbc5ca0..447a26b07 --stat -- restart/skinny/tranches/sk-v14/research/p2/` reports P2-B +6, P2-C +39/-21, P2-D +14/-17, P2-F +57/-18; P2-A + P2-E absent (zero drift).
