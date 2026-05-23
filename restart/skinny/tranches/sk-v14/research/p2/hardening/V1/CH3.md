# S-P2 V1 CHALLENGE — CH3 REGRESSION (REDRESS) Lens

Authored: 2026-05-23 (post-S-P2 V1 atomic commit of six P2 axis files).
Six artefacts under review: p2a (367 lines), p2b (330), p2c (143), p2d
(257), p2e (342), p2f (333). Authorities re-read end-to-end:
`restart/prompts/skinny/PASS-2-RESEARCH.md` §3 CH3,
`restart/prompts/ORCHESTRATOR.md` §3W (CH3 universal definition) + §3Z,
`restart/skinny/tranches/sk-v14/research/p2/S-P2-DISPATCH-CONTEXT.md`,
`restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CHALLENGE-CONTEXT.md`
§0–§4, and `skinny/REDRESS.md` (5041 lines; sampled via grep + line-bounded
Read per `[read-size-preflight]`).

CH3 binding restated (PASS-2-RESEARCH §3 CH3): no candidate re-opens
REDRESS routes (watch-list per dispatch §2): **28+33** (Class A NEON
tiny-string wiring), **50–55** (SK-V5 UTF-8 fusion / dispatch-table /
function-pointer alternates), **60–72** (SK-V6 retained-parse + sidecar
producers + cap-16 digest), **80** (canada mantissa-widen), **82–84**
(single-quartet unicode classifier / StringBlock16 tiny probe /
object-pair compaction), **88** (PMULL prefix-XOR as hot body), **89**
(CSSC CTZ next-bit bulk consumer), **96/97/98** (production-union /
class-column / streaming-cursor substrate routes), **119/120** (direct-
row fixpoint pre-blocks per SK-V13 P2-C carry), **126** (PEXT mask plan;
aarch64 has no PEXT).

Two dispatch-context anchors specific to V1:
1. **P2-B claim**: the 5-stage admission gate is "structurally REDRESS-
   pre-block-safe by construction." CH3 must verify each REDRESS
   family is pre-block-covered in at least one of the 14 (P2-F) + 8
   (P2-E) + 7 (P2-A) candidate enumerations.
2. **P2-E gap 5 differential flag**: gap 5 (`parse_16_digits_dotprod`)
   carries an explicit REDRESS-80 differential. P2-E §2 Gap 5 declares
   "Flagged for S-P3 decision per dispatch context §0; S-P2 does not
   bypass REDRESS 80, only records the differential."

## §1 — Disposition summary

| Axis | §2 candidates | REDRESS routes invoked? | Pre-block citation present? | Disposition |
|---|---:|---|---|---|
| p2a-sota-teardown | 7 (C1–C7) | C2 (REDRESS 28+33/82–84 differential per long-string envelope split); C3 (REDRESS 80 differential per generic vs canada-tweak split); C7 (REDRESS 82 differential per windowed vs single-quartet); C5 (REDRESS 96/97/98 differential per *substrate-union-honouring* re-route, not parallel substrate) | YES (§4 explicit per-candidate; all four cite differentials) | ACCEPT |
| p2b-dav1d-process | 5 stages (A–E) — process artefact, not new primitives | None (process artefact); §4 explicitly enumerates REDRESS 28+33, 50–55, 60–72, 80, 82–84, 88, 89, 96–98 and binds each to a Stage that fails it closed by construction | YES (§4 verbatim per-REDRESS-family CH3 catchall at p2b:175) | ACCEPT |
| p2c-arch-esoterica | 8 (C-P2C-1..-8) | C-P2C-2 (REDRESS 88+89+96–98 — explicitly PRE-BLOCKED at V1; admissible only via SIMD-first union consumer that DELETES scalar consume — same-wave gate); C-P2C-3 (REDRESS 80 — NOT-S-P3-ELIGIBLE at V1; parse-attribution rebuild gate); C-P2C-4 (REDRESS 82 — windowed vs single-quartet differential, JSON fixed-width only; CSS variant pre-blocked); C-P2C-7 (REDRESS-126 orphan resolution) | YES (§4 11-line CH3 audit naming REDRESS 88/89/90/96–98/82–84/SK-V10/60–72/50–55/28+33/80/119–120/126 + PEXT-arch-block) | ACCEPT |
| p2d-substrate-tape | 3 active + 1 explicitly pre-blocked-anti-pattern (C-P2D-4 = `EventTape` REJECT-by-REDRESS-96/97/98) | C-P2D-4 is named as **pre-blocked-anti-pattern reference**, not a candidate — load-bearing CH3 paper-trail anchor | YES (§4.1 REDRESS 96/97/98 verbatim; §4.2 50–55; §4.3 60–72; §4.4 80+82–84; §4.5 88+89; §4.6 Lock 1 binding) | ACCEPT |
| p2e-parse-that-gaps | 8 (Gap 1–8 incl. 7.5) | Gap 5 (REDRESS-80 differential, explicitly flagged for S-P3 decision); Gap 2 (REDRESS 82–84 doubling vs classifier differential); Gap 4 + Gap 8 (REDRESS 50–55 validate-only vs fused-materialisation differential); Gap 1 (REDRESS 28+33 wiring vs global cap-16 differential); Gap 6 (REDRESS 96/97/98 substrate-union composition vs new-substrate differential) | YES (§1.4 + §4.1 per-gap audit) | ACCEPT-WITH-NOTE (Gap 5 REDRESS-80 differential is the load-bearing observation; see §3 F-1) |
| p2f-grammar-neutral | 14 (C1–C14) | C2 (REDRESS 88 differential — split-lane vs hot-body wholesale); C4 (REDRESS 28+33 differential — SVE2-port primitive + cross-grammar consumer); C5 (REDRESS 80 differential — AVX-IFMA postdates + cross-grammar framing); C9 (REDRESS-126 — aarch64-orthogonal NEON form; x86_64 BMI2 sub-candidate routed separately); C7 (REDRESS 50–55 adjacency — standalone vs fused); C1+C2+C4+C9 (REDRESS 60–72 — no retention); C2+C3+C12 (REDRESS 82–84 — no single-quartet/StringBlock16/object-pair); C11 (REDRESS 96–98 — substrate-union-honouring with full Lock 1 v+1 manifest); C6 (parse-attribution F-V2-P1ABC-RERECORD carry-forward) | YES (§4 8-row REDRESS table at p2f:264–274, plus C8 CH6 paper-close risk, C10/12/13 scalar-reference CH4 risk, C11 CH5 substrate-union risk, aggregate-CH3 risk) | ACCEPT |

Per-axis ACCEPT-rate: **6/6 (100%)**. Zero candidates silently re-open a
pre-blocked REDRESS family. One ACCEPT-WITH-NOTE on p2e for the Gap 5
REDRESS-80 differential explicitly flagged by the dispatch context.
No REVISE; no REJECT.

## §2 — Per-candidate REDRESS adjacency table (full audit, 14+8+7+5+3+8 = 45 entries)

For every §2 candidate across all six artefacts, the closest REDRESS
family + the artefact's disposition treatment. Process artefacts (p2b
5-stage gate; p2d C-P2D-4 pre-blocked) and meta-candidates (p2a C4
codegen invariant; p2a C6 profile discipline; p2c C-P2C-8 measurement
gate; p2f C14 i-cache budget constraint) are tabulated with the
"none — process / build-discipline" disposition where applicable.

### §2.1 — p2a-sota-teardown (7 candidates)

| Candidate | Nearest REDRESS family | Cited in artefact? | Disposition |
|---|---|---|---|
| C1 `lazy_field_skip_with_index` | none (consumes existing structural index; no parallel substrate) | n/a | accepted |
| C2 `long_string_body_simd_scan` | REDRESS 28+33 + 82–84 (string SIMD wiring + tiny-probe) | YES (p2a:227, :231) — differential = long-string body envelope, not tiny-string cap probe | accepted |
| C3 `digit_block_simd_accumulate` | REDRESS 80 (canada mantissa-widen) | YES (p2a:230) — differential = generic digit-block primitive, not canada-specific mantissa widen | accepted |
| C4 `force_inline_lto_envelope_discipline` | none (build invariant; Lock 15 already binding) | n/a (build invariant) | accepted |
| C5 `structural_index_singular_substrate_consumer` | REDRESS 96/97/98 (production-union routes) | YES (p2a:234) — differential = **removes** parallel substrate (opposite of REDRESS 96/97/98); substrate_target=existing_tape | accepted |
| C6 `parse_attribution_envelope_cracker` | none (profile discipline, not kernel) | n/a (process) | accepted |
| C7 `unicode_escape_neon_nibble_decode` | REDRESS 82 (single-quartet unicode classifier) | YES (p2a:231) — differential = windowed `\uXXXX`+surrogate decode, not single-quartet classifier replay | accepted |

### §2.2 — p2b-dav1d-process (5 admission-gate stages, process artefact)

| Stage | Nearest REDRESS family | Cited in artefact? | Disposition |
|---|---|---|---|
| Stage A scalar-reference authoring | n/a (process) | n/a | accepted |
| Stage B differential checkasm cell | n/a (process) | n/a | accepted |
| Stage C Lock 16 cite + same-plane SOTA | n/a (process) | n/a | accepted |
| Stage D same-wave consumer + strict row movement | REDRESS 28+33 (tiny-string regress-twitter), REDRESS 80 (canada mantissa-widen row gate) | YES (p2b:168, :171) — Stage D's strict-row-movement clause IS the gate that fails such routes closed | accepted |
| Stage E manifest + substrate-target | REDRESS 50–55, 60–72, 82–84, 88–89, 96/97/98, 126 | YES (p2b:169–175) — Stage E's substrate_target / retention_lifetime / policy_owner columns block every named pattern by construction (Lock 1 v+1 manifest enumeration) | accepted |
| **§4 catchall** (p2b:175) | all dispatch-watch-list families | YES verbatim — "all blocked at Stage E substrate-target column by construction" | accepted |

### §2.3 — p2c-arch-esoterica (8 candidates)

| Candidate | Nearest REDRESS family | Cited in artefact? | Disposition |
|---|---|---|---|
| C-P2C-1 `ascii_set_member64_css_delimiter` | none directly; PRUNE-2 CSS L4 plane absence (CH4/CH7) | n/a | accepted (`NOT-S-P3-ELIGIBLE` at V1 absent CSS L4 corpus) |
| C-P2C-2 `pmull_cssc_structural_union_emit64` | **REDRESS 88 + 89 + 90 + 96/97/98** | YES (p2c:67–70) — PRE-BLOCKED at V1; admission requires SIMD-first union consumer that **deletes** scalar consume (per Item 88/89 failure mode); strict same-row non-regression on 11 rows + Lock 1 substrate union held + emitted-asm proof | accepted (PRE-BLOCKED; not silently re-opened) |
| C-P2C-3 `udot_digit_span_x4` | REDRESS 80 (canada mantissa-widen) | YES (p2c:75) — "no mantissa-widen route on aarch64; SK-V14 C-P2C-3 must not propose mantissa-widen as a route" | accepted (`NOT-S-P3-ELIGIBLE` at V1; gated on F-V2-P1ABC-RERECORD) |
| C-P2C-4 `tbl_tbx_escape_decode_batch` | REDRESS 82 (single-quartet) + SK-V10 unicode proof-only limits | YES (p2c:71) — JSON fixed-width `\uXXXX` only at V1; CSS variant requires PRUNE-2 successor + grammar-policy proof | accepted (S-P3-ELIGIBLE for JSON only) |
| C-P2C-5 `string_special_64_context` | adjacent to REDRESS 28+33 (tiny-string), 82–84 (StringBlock16) | YES (p2c:74) — "no second wiring path"; conditional on C-P2C-2 or C-P2C-4 landing | accepted (conditional support primitive only) |
| C-P2C-6 `eor3_string_mask_fusion` | none directly; CH6 paper-close risk | n/a | accepted (`NOT-S-P3-ELIGIBLE` inventory) |
| C-P2C-7 `byte_context_orphan_resolution` | REDRESS-126 (aarch64 orphan inventory) | YES (p2c:77) — "may wire or delete/demote, but cannot add or retain another orphan" | accepted (close-hygiene only) |
| C-P2C-8 `parse_attribution_profile_rebuild_gate` | none (process gate) | n/a | accepted (S-P3 process-gate prerequisite) |

p2c-arch-esoterica is the artefact with the densest REDRESS-cite surface
in the V1 cohort: §4 carries 11 distinct REDRESS-family pre-block notes
(88/89/90/96–98/82–84/SK-V10/60–72/50–55/28+33/80/119–120/126 + PEXT-
arch-block) verbatim at p2c:67–78. **This is the second canonical S-P2
V1 CH3 statement** (after p2b's §4 catchall).

### §2.4 — p2d-substrate-tape (3 active + 1 pre-blocked-anti-pattern)

| Candidate | Nearest REDRESS family | Cited in artefact? | Disposition |
|---|---|---|---|
| C-P2D-1 `BackendShape::SinkOnly` activation | none (re-uses existing five-shape vocabulary; *elides* TapeBuilder, not adds) | n/a | accepted |
| C-P2D-2 `OffsetTapeStats` column extension | none (re-uses existing substrate field; surfaces existing computation) | n/a | accepted |
| C-P2D-3 sparse-flag-band gating | none (substrate-side allocation discipline) | n/a (CH4-pre-block falsifier per p2d:131) | accepted |
| C-P2D-4 `BackendShape::EventTape` interrogation | **REDRESS 96 + 97 + 98** | YES (p2d:136–140) — "NOT a candidate at S-P2-D … REJECT-by-history" — load-bearing CH3 paper-trail anchor | accepted (explicit anti-pattern reference, not candidate) |

p2d is also the load-bearing artefact for **substrate-union holds at
HEAD** (§1.5 conclusion), which discharges the SK-V13 Pass Omega V1.1
union-variant receiver by architectural block — a CH5-load-bearing
finding with CH3 cross-checking value (§4.7 cross-check).

### §2.5 — p2e-parse-that-gaps (8 candidate gaps, including 7.5)

| Candidate | Nearest REDRESS family | Cited in artefact? | Disposition |
|---|---|---|---|
| Gap 1 `scan_string_special_block_sweep_64` | REDRESS 28+33 (tiny-string cap-16) | YES (p2e:82, :257) — parse-that consumer wiring, not global cap policy | accepted |
| Gap 2 `unescape_uxxxx_x8_neon` | REDRESS 82–84 (single-quartet / StringBlock16 / object-pair) | YES (p2e:86, :125, :261) — doubling of admitted `_x4` decoder, not classifier or tiny-probe | accepted |
| Gap 3 `ascii_whitespace_skip_64` | none directly | n/a | accepted |
| Gap 4 `utf8::validate_block_streaming` | REDRESS 50–55 (UTF-8 fusion) | YES (p2e:83, :155, :258) — validate-only, no decode, no sink (inverse of fused-materialisation) | accepted |
| **Gap 5 `parse_16_digits_dotprod`** | **REDRESS 80 (canada mantissa-widen)** | YES (p2e:85, :171, :260) — **explicitly flagged for S-P3 decision**; S-P2 does not bypass REDRESS 80, only records the primitive-vs-tweak differential | accepted-with-note (F-1 below) |
| Gap 6 `scan_string_with_carry_64` | REDRESS 88 (PMULL prefix-XOR) + 96/97/98 (union substrate) | YES (p2e:87, :187, :262, :264) — composes existing primitives; no PMULL; no new column substrate | accepted |
| Gap 7 `scan_digit_run_simd_64` | none directly (depends on Gap 7.5) | n/a | accepted |
| Gap 7.5 `byte_class_from_range_64` | none directly | n/a | accepted |
| Gap 8 `utf8_codepoint_scan_64` | REDRESS 50–55 (UTF-8 fusion) | YES (p2e:258) — width-only, no decode, no sink | accepted |

### §2.6 — p2f-grammar-neutral (14 candidates)

| Candidate | Nearest REDRESS family | Cited in artefact? | Disposition |
|---|---|---|---|
| C1 structural-byte SIMD classify | none directly | n/a | accepted |
| C2 quoted-string boundary scan (PMULL prefix-XOR) | REDRESS 88 (PMULL prefix-XOR hot body) | YES (p2f:86, :264) — split-lane framing (structural + quote-aware), not wholesale PMULL hot body; differential = AVX-512 VPCLMULQDQ (Lock 16 :294, NEW 2026-05-12) postdates REDRESS 88 | accepted |
| C3 escape canonicalisation | none directly (per-byte branching shape; only inner hex-decode is SIMD-amenable) | n/a | accepted |
| C4 tiny-keyword-set match | REDRESS 28+33 (Class A NEON tiny-string wiring) | YES (p2f:107, :265) — SVE2-port primitive (Lock 16 :290 NEW 2026-05-12) + cross-grammar consumer (CSS unit-keyword sets, Sheets error literals, BBNF directives) which 28+33 never carried | accepted |
| C5 digit-block number decode | REDRESS 80 (canada mantissa-widen) | YES (p2f:118, :267) — AVX-IFMA primitive (Lock 16 :295 NEW 2026-05-12) + cross-grammar consumer (CSS dimension values, Sheets formulas) | accepted |
| C6 branch-on-first-byte dispatch | F-V2-P1ABC-RERECORD carry-forward (parse-attribution feature gating) | YES (p2f:129, :273) — same-wave parse-attribution rerun gate | accepted |
| C7 leading-whitespace prefix skip | REDRESS 50–55 (SK-V5 UTF-8 fusion) adjacent | YES (p2f:140, :269) — standalone primitive, not fused with string decode | accepted |
| C8 comment-skip primitive | none (no P1 antecedent — CH6 paper-close risk; NEUTRAL-PENDING-CONSUMER) | n/a (CH6) | accepted (NEUTRAL-PENDING-CONSUMER) |
| C9 offset-tape bulk emit | REDRESS-126 (PEXT mask plan; aarch64 has no PEXT) | YES (p2f:161, :268) — aarch64 NEON form REDRESS-126-orthogonal; x86_64 BMI2 form routed as separate sub-candidate with REDRESS-126 framing | accepted |
| C10 cross-chunk byte-context propagation | none (Lock 16 :285 admits abstract primitive verbatim) | n/a | accepted (CH4: no scalar reference yet — flagged) |
| C11 substrate-walk-with-shape-validation | REDRESS 96/97/98 (production-union substrate-ceiling) | YES (p2f:182, :272) — substrate_target=existing_tape (not new substrate); per Lock 1 v+1 substrate-union-honouring; per P1-E §4.4 single primitive, not split into two | accepted |
| C12 keyword-set 16-byte alphabet membership | none directly (Lock 16 :290 admits) | n/a (CH4: no scalar reference yet — flagged) | accepted |
| C13 branchless 3-way XOR (BCAX) | none (Lock 16 :289 admits) | n/a (CH4: no scalar reference yet — flagged) | accepted |
| C14 i-cache budget constraint | none (Lock 15 invariant) | n/a (build invariant) | accepted |

### §2.7 — Coverage summary

**Total §2 candidate entries audited: 45** (7 p2a + 5 p2b + 8 p2c +
4 p2d + 9 p2e + 14 p2f, with p2d C-P2D-4 counted as the explicit
pre-blocked-anti-pattern reference). Every entry whose adjacency to a
pre-blocked REDRESS family is non-zero carries an explicit citation of
the relevant entry **and** a differential or substrate-target/retention-
lifetime/policy-owner column that blocks silent re-opening. **No
candidate implicitly re-opens any watch-list REDRESS family**.

## §3 — P2-B 5-stage gate verification (load-bearing dispatch claim)

The dispatch context binds (§2 CH3): "P2-B claims 5-stage gate is
structurally REDRESS-pre-block-safe by construction — verify each
REDRESS family has CH3 pre-block coverage in at least one of the 14+8+7
candidate enumerations."

Per-REDRESS-family coverage matrix across the 14+8+7 = 29 candidate
enumeration (p2f 14 + p2e 8 + p2a 7):

| REDRESS family | p2f covered? | p2e covered? | p2a covered? | p2b stage that fails closed | Coverage verdict |
|---|---|---|---|---|---|
| **28+33** (Class A tiny-string wiring) | YES (C4 with SVE2-port + cross-grammar differential, p2f:107) | YES (Gap 1, wiring-not-cap differential, p2e:82) | YES (C2 long-string envelope differential, p2a:227) | Stage D (strict-row-movement on twitter; p2b:168) | **3/3 + Stage D** — over-covered |
| **50–55** (UTF-8 fusion / dispatch-table alternates) | YES (C7 standalone-not-fused, p2f:140) | YES (Gap 4 + Gap 8 validate-only, p2e:83, :258) | n/a (no UTF-8-fusion or dispatch-table candidate) | Stage E (substrate-target column blocks function-pointer table; p2b:169) | **2/3 + Stage E** |
| **60–72** (retained-parse + sidecar producers + cap-16) | YES (C1+C2+C4+C9 no retention, p2f:270) | YES (Gap 1 + Gap 6 no retained parse, p2e:259) | YES (C1 + C5 consume existing single substrate, p2a:229) | Stage E (retention_lifetime column blocks `parser_owned`; p2b:170) | **3/3 + Stage E** |
| **80** (canada mantissa-widen) | YES (C5 AVX-IFMA + cross-grammar, p2f:118) | YES (Gap 5 **explicit S-P3-decision flag**, p2e:171) | YES (C3 generic digit-block differential, p2a:230) | Stage D (strict-row-movement on canada; p2b:171) | **3/3 + Stage D** |
| **82–84** (single-quartet unicode / StringBlock16 / object-pair) | YES (C2+C3+C12 no single-quartet, p2f:271) | YES (Gap 2 doubling-not-classifier, p2e:86, :261) | YES (C7 windowed-not-single-quartet, p2a:231) | Stage E (retention_lifetime + substrate-target columns; p2b:172) | **3/3 + Stage E** |
| **88** (PMULL prefix-XOR hot body) | YES (C2 split-lane framing + Lock 16 :294 differential, p2f:265) | YES (Gap 6 composes existing, no PMULL hot body, p2e:187) | YES (no candidate proposes PMULL hot body; explicit cite, p2a:232) | Stage C+D (same-plane SOTA cite + strict-row-movement; p2b:173) | **3/3 + Stages C+D** |
| **89** (CSSC CTZ next-bit bulk consumer) | n/a (no CSSC candidate) | YES (Gap 6 `trailing_zeros` on OR-fold, not CSSC dispatch, p2e:263) | YES (no candidate proposes CSSC; explicit cite, p2a:233) | Stage C+D (same as 88; p2b:173) | **2/3 + Stages C+D** |
| **96/97/98** (production-union / class-column / streaming-cursor) | YES (C11 substrate-union-honouring with full Lock 1 v+1 manifest, p2f:272) | YES (Gap 6 composes in-substrate-union, no new column, p2e:264) | YES (C5 **removes** parallel substrate — opposite of route, p2a:234) | Stage E (substrate-target column; abrogate-threshold clause; p2b:174) | **3/3 + Stage E** |
| **119/120** (direct-row fixpoint pre-blocks) | n/a (P2-F adjudicates grammar-neutrality, not direct-row) | n/a (P2-E candidate-gap level, not row-fixpoint) | n/a (P2-A SOTA-teardown, not row-fixpoint) | n/a — direct-row admission is S-P3 wave concern; p2c §4 carries the CH3 cite verbatim (p2c:76) | **p2c coverage** |
| **126** (PEXT mask plan; aarch64 has no PEXT) | YES (C9 x86_64 BMI2 sub-candidate routed separately; aarch64 NEON form orthogonal, p2f:161, :268) | n/a (P2-E parse-that gaps; PEXT is P2-C scope per p2e:87, :88) | n/a (P2-A SOTA-teardown; no PEXT proposal) | n/a — explicit arch-block per p2c §4 PEXT pre-block paragraph (p2c:78) | **p2f + p2c coverage** |

**Verdict on the P2-B claim**: **the 5-stage admission gate IS
structurally REDRESS-pre-block-safe by construction**. Every dispatch-
context watch-list REDRESS family is covered by:
- **at least one** of the 29 candidate enumerations (typically 2–3),
  AND
- **at least one** P2-B admission stage that fails the candidate
  closed by construction if the candidate slips past upstream gates.

The Lock 1 v+1 substrate manifest (`restart/locks/LOCKS.md:73-90`,
verbatim restated at p2b Stage E shape) is the load-bearing structural
guarantee: Stage E's `substrate_target ∈ {local_temp_only,
existing_tape, direct_sink, admitted_fact_output}`, `retention_lifetime
∈ {local_loop, generated_function, output_row}`, `policy_owner ∈
{generated_grammar, caller_data, none}` enumerations collectively
block every named REDRESS pattern by Lock-1-v+1 enforcement.

The two REDRESS families with weaker direct-candidate coverage
(**119/120** and **126**) are covered by p2c's §4 explicit cite (p2c
is the host-arch ASM/SIMD esoterica artefact and is the correct home
for these arch-keyed REDRESS families). The dispatch context's 14+8+7
phrasing covers only p2f+p2e+p2a; per CH3 universal scope, p2c is
also part of the V1 candidate surface and is the canonical home for
arch-keyed REDRESS coverage. **No gap.**

## §4 — Critical findings (CH3 lens, new)

### F-1 — P2-E Gap 5 REDRESS-80 differential flagged by dispatch context (load-bearing)

The dispatch context explicitly flags this: "P2-E flagged REDRESS 80
differential on gap 5." Per p2e §2 Gap 5 (`parse_16_digits_dotprod`)
at lines 158–171:

> REDRESS 80 was a canada-specific mantissa-widen plan with a per-corpus
> float-overfit posture. Gap 5 is grammar-neutral: every grammar with
> numeric literals (JSON, CSS L4 `<number>` per CSS Values L4 §4.1,
> Sheets formula numerals, BBNF-self regex `{n,m}` counts) uses the
> same digit-run primitive. The material differential: a Layer-1
> primitive, not a hot-path corpus tweak. **Flagged for S-P3 decision**
> per dispatch context §0; S-P2 does not bypass REDRESS 80, only
> records the differential.

CH3 disposition: **accepted-with-note**. p2e's framing is correct per
the CH3 binding ("cite the entry and mark pre-blocked, not implicitly
re-open"). The candidate explicitly names REDRESS-80, explicitly carries
a fresh material differential (primitive-vs-tweak), and explicitly
defers admission to S-P3 with the burden of proof preserved. This is
**not** a silent re-opening — it is the canonical correct CH3 shape
for a pre-blocked-route adjacency.

The note attaches because **the differential strength is S-P3's
adjudication**, not CH3's: per CH3 binding, S-P2 may surface the
candidate adjacent to the pre-blocked route iff (a) the REDRESS entry
is cited, (b) the candidate carries a fresh material differential, and
(c) admission is deferred to S-P3 evidence. p2e Gap 5 satisfies all
three. **The S-P3 V1 dispatch context must carry the REDRESS-80
re-evaluation checklist** (per Item 80 at REDRESS.md:2217 anchor and
per the canada-W2 rejection circumstances): same-wave canada
parse-only Track 1 row movement non-regressive, primitive-not-corpus-
tweak shape verified, cross-grammar consumer (CSS dimension /
Sheets formula) named at admission time.

Cross-check: p2f C5 (digit-block number decode) ALSO names the
REDRESS-80 differential (p2f:118, :267), with the same posture: AVX-
IFMA primitive (Lock 16 :295 NEW 2026-05-12) postdates REDRESS 80,
cross-grammar framing is new. **The two candidates align**: p2e Gap 5
is the parse-that-side (Layer-1 in `parse-that-regex/src/number/`)
proposal; p2f C5 is the cross-grammar/abstract-primitive proposal. Both
defer to S-P3 with the same checklist.

Per p2c C-P2C-3 (UDOT-digit-span-x4) the THIRD aligned proposal:
explicitly `NOT-S-P3-ELIGIBLE` at V1, gated on F-V2-P1ABC-RERECORD
naming a numeric leaf behind the dispatch envelope. **Three artefacts
converge on the same conclusion**: digit-block SIMD is admissible only
with (i) generic primitive framing, (ii) cross-grammar consumer,
(iii) parse-attribution rebuild evidence, (iv) REDRESS-80 checklist
honoured. No fourth admission criterion needed.

### F-2 — p2c C-P2C-2 PMULL+CSSC Union-C is correctly PRE-BLOCKED at V1 with explicit re-admission path

p2c C-P2C-2 (`pmull_cssc_structural_union_emit64`) is the densest
REDRESS-cite in the V1 cohort (REDRESS 88 + 89 + 90 + 96/97/98
simultaneously). The candidate's V1 disposition (p2c:44):

> **PRE-BLOCKED at SK-V14 V1 by REDRESS 88 + 89 + 96-98** unless
> S-P3 dispatches a Union-C wave with: (a) SIMD-first direct tuple
> writeback that DELETES the current scalar consume path (not adds-
> alongside); (b) strict same-row non-regression on the 11 row set
> Item 88/89 falsified … (c) Lock 1 substrate union held per P2-D;
> (d) emitted-asm proof of `pmull.1q` and `ctz` per SK-V7 W10b
> template. Local body-fill of the scalar delegates remains REJECTED.

CH3 disposition: **accepted**. The candidate is correctly PRE-BLOCKED
(not silently surfaced as admissible); the re-admission path is
explicit, evidence-based, and tied to the specific failure modes
documented at REDRESS.md:2510 (Item 88), :2544 (Item 89), :2589 (Item
90). The four S-P3 admission requirements collectively reproduce the
SK-V7 W10/W10b/W10c failure-mode hardening — the candidate cannot
slip through without re-running the specific bench harness that
falsified the original route.

**Cross-check with p2d §1.5**: p2d concludes substrate union holds at
HEAD (load-bearing CH5 finding; corroborates §4.7). C-P2C-2's
requirement (c) "Lock 1 substrate union held per P2-D" is therefore
**satisfied by p2d V1**. The candidate's re-admission path narrows to
requirements (a)+(b)+(d): SIMD-first-direct-tuple-writeback +
strict-row-non-regression + emitted-asm proof.

### F-3 — p2d C-P2D-4 is the canonical CH3 paper-trail anchor (REJECT-by-history reference)

p2d C-P2D-4 (`BackendShape::EventTape` interrogation) at p2d:134–140
is explicitly listed as **NOT a candidate** — REJECT-by-REDRESS-96/97/98.
The artefact's framing:

> Listed here as the canonical "what would a parallel-substrate
> proposal look like, and why is it pre-blocked" reference for
> CH3 / CH5 cross-checking.

CH3 disposition: **accepted**. This is the load-bearing
"anti-pattern-as-paper-trail" shape: the artefact documents the
pre-blocked route in candidate-list discipline so that future
CHALLENGE cycles can cross-check any new EventTape-shaped proposal
against an in-tree explicit reject. Per `[abrogate-before-patch]`
discipline, this is the correct shape: ask "can we delete?" before
"can we patch?" — and C-P2D-4 documents the historical "we already
asked, and the answer is no on this host."

### F-4 — p2c C-P2C-7 byte_context orphan resolution is the REDRESS-126 close-hygiene anchor

p2c C-P2C-7 (`byte_context_orphan_resolution`) at p2c:49 names the
REDRESS-126 (aarch64 orphan inventory) close-hygiene obligation: the
existing `bbnf-simd/src/aarch64/byte_context.rs` is an EXT helper
without a SK-V14 production consumer. The disposition:

> **`NOT-S-P3-ELIGIBLE` at SK-V14 V1** as a standalone wave. Close-
> hygiene only; may be folded into C-P2C-5 consumer or deletion /
> demotion evidence under a hygiene wave.

CH3 disposition: **accepted**. The candidate correctly honours the
REDRESS-126 zero-orphan binding: it does not propose a new orphan,
does not retain the existing orphan as support-only, and explicitly
routes resolution through either (a) consumer wiring (C-P2C-5) or
(b) deletion/demotion with REDRESS inventory proof. This is the same
close-hygiene shape SK-V13 P1-C ANOM-6 (REDRESS-126 zero-orphan) bound
at S-P1 V1; the SK-V14 P2-C carry-through is faithful.

### F-5 — p2b §4 catchall + p2c §4 enumeration are dual canonical S-P2 V1 CH3 statements

Two artefacts carry verbatim per-REDRESS-family enumerations covering
the full dispatch-context watch-list:

1. **p2b §4 catchall** (p2b:166–177): "REDRESS 28+33, 50-55, 60-72,
   80, 82-84, 88, 89, 96-98 + CH3 catchall." Each family is bound to
   a specific P2-B admission gate stage that fails it closed (Stage
   D strict-row-movement; Stage E substrate-target/retention-lifetime/
   policy-owner enumeration). This is the **structural** CH3 statement
   — the gate process structurally cannot admit a route that re-opens
   any watch-list family.

2. **p2c §4 enumeration** (p2c:67–78): "REDRESS 88, 89, 90, 96/97/98,
   82-84, SK-V10, 60-72, 50-55, 28+33, 80, 119/120, 126 + PEXT
   arch-block." Each family is bound to a specific candidate (C-P2C-2,
   C-P2C-3, C-P2C-4, C-P2C-7) whose disposition is either PRE-BLOCKED-
   at-V1 or NOT-S-P3-ELIGIBLE-at-V1. This is the **per-candidate** CH3
   statement — every C-P2C candidate has been checked against the full
   watch-list with explicit disposition.

These two statements, taken jointly with p2d §4.1–§4.7 (which is the
substrate-side-specific CH3 statement) and p2a §4 (which is the
SOTA-comparator-side CH3 statement), constitute the **quadruple
canonical S-P2 V1 CH3 statement**. The watch-list is covered with
redundant evidence across four orthogonal axes (admission process,
arch instruction, substrate, SOTA architecture). **No silent re-open
risk surfaces in the V1 artefact set**.

## §5 — V2 fold recommendations

1. **No REVISE recommended** on any of the six S-P2 V1 artefacts on
   CH3 grounds. The lens is satisfied at 100% ACCEPT (5 ACCEPT +
   1 ACCEPT-WITH-NOTE).

2. **S-P3 V1 dispatch context must carry the REDRESS-80 re-evaluation
   checklist for Gap 5 (p2e) + C5 (p2f) + C-P2C-3 (p2c)** per F-1.
   The three candidates align on a common admission path; the S-P3
   dispatch must enumerate (i) same-wave canada parse-only Track 1
   non-regression, (ii) primitive-not-corpus-tweak shape verified
   via emitted-asm + `cargo asm`, (iii) cross-grammar consumer
   (CSS dimension / Sheets formula) named at admission time, (iv)
   F-V2-P1ABC-RERECORD evidence naming a numeric inner leaf behind
   the dispatch envelope. **Without all four, the candidate fails
   the REDRESS-80 differential gate.**

3. **S-P3 V1 dispatch context must carry the REDRESS 88+89+90+96/97/98
   re-evaluation checklist for C-P2C-2** per F-2. The candidate's
   four explicit requirements at p2c:44 are the admission path; the
   S-P3 dispatch must bind them as the Union-C wave entry condition.
   p2d's substrate-union-holds verdict satisfies (c) at V1; the S-P3
   dispatch must verify (a)+(b)+(d) remain unfulfilled-at-V1 and
   that the Union-C wave plan addresses each before any commit.

4. **Pre-emptive CH3 fence for any C-P2D-4 (`EventTape`) re-surfacing**:
   p2d:134–140 names this as REJECT-by-REDRESS-96/97/98. The S-P3
   dispatch must carry this paper-trail anchor in its CH3 binding so
   that any future CHALLENGE cycle (V2/V3/etc.) can cross-check
   against the explicit anti-pattern reference without re-litigating
   the rejection.

5. **Pre-emptive CH3 fence for any future P2-F C8 (comment-skip)
   admission**: p2f:228 marks this NEUTRAL-PENDING-CONSUMER with
   same-wave-consumer requirement. The CH3 risk is that a later
   wave admits the primitive against a JSON-only consumer (which
   doesn't exist) without the BBNF-self or CSS L4 consumer landing
   in the same wave. Per `[no-deferrals]` the consumer cannot defer;
   the S-P3 dispatch must bind C8's same-wave non-JSON consumer
   requirement in the wave plan or drop the candidate.

## §6 — Sources verified (executable-verification mandate)

Verified existence + line-bounded read across all six V1 artefacts:

- `restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md` —
  367 lines; §4 REDRESS-block table at lines 226–250 verbatim
  verified (REDRESS 28+33, 50–55, 60–72, 80, 82–84, 88, 89, 96/97/98
  + strictness-plane + substrate-union risks + per-candidate CH6
  paper-close risks).
- `restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md` —
  330 lines; §4 catchall at lines 166–177 verbatim verified (5-stage
  gate structurally REDRESS-pre-block-safe); §2.0 5-stage flow
  diagram at lines 128–148 verified.
- `restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md` —
  143 lines; §4 11-REDRESS-family enumeration at lines 67–78 verbatim
  verified; C-P2C-2 PRE-BLOCKED disposition at line 44 verified;
  C-P2C-3 REDRESS-80 cite at line 75 verified; C-P2C-7 REDRESS-126
  zero-orphan binding at line 49 + line 77 verified.
- `restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md` —
  257 lines; §1.1 single-substrate verdict at lines 12–29 verified
  (3 hits for `struct.*Tape\b`); §1.5 substrate-union holds verdict
  at lines 84–92 verified; C-P2D-4 REJECT-by-REDRESS-96/97/98
  anti-pattern reference at lines 134–140 verified; §4.1–§4.7
  per-REDRESS-family cross-check at lines 159–204 verified.
- `restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md` —
  342 lines; §1.4 pre-block surface at lines 78–91 verified;
  Gap 5 REDRESS-80 differential at lines 158–171 + §4.1 at line 260
  verified (`Flagged for S-P3 decision; S-P2 does not bypass REDRESS
  80, only records the differential`); §4.1 per-gap audit at lines
  256–264 verified.
- `restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md` —
  333 lines (wc -l: 333, two-page Read via offset=232 limit=231 after
  initial 230-line head); §4 8-row REDRESS table at lines 263–273
  verbatim verified; verdict tally at lines 242–248 verified (5
  NEUTRAL-WIRED + 8 NEUTRAL-CONFIG-DRIVEN + 1 NEUTRAL-PENDING-CONSUMER
  + 0 OVERFIT-REFRAMABLE + 0 OVERFIT-IRREDUCIBLE).

Verified `skinny/REDRESS.md` (5041 lines via wc -l) anchors via grep:
- Item 80 (W2 mantissa-widen reject) at line 2217 — the canonical
  anchor for the REDRESS-80 differential flagged in p2e Gap 5.
- Item 88 (W10 PMULL prefix-XOR hot body reject) at line 2510 —
  the canonical anchor for the C-P2C-2 PRE-BLOCKED disposition.
- Item 89 (W10b CSSC CTZ bulk consumer reject) at line 2544 — same
  C-P2C-2 PRE-BLOCKED disposition.
- Item 90 (W10c B6 canary admit; PMULL+CSSC bitmap fills routed to
  later wave but not landed through SK-V8/9/10/11/12/13) at line
  2589 — the explicit "route not silently admitted later" anchor.

Verified P1 antecedent linkage at `research/p1/hardening/V3/CH5.md`
(per p2d:7 cite of `:78-83` 6/6 ACCEPT substrate-union verdict —
the CH5-load-bearing finding that satisfies C-P2C-2 admission
requirement (c)).

Verified dispatch-context anchors at
`restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CHALLENGE-CONTEXT.md`
§0–§4 — the binding for this CH3 lens.

## §7 — CH3 disposition (final)

**ACCEPT 6/6 artefacts.** Per-axis rate: **100% ACCEPT** on the §2
candidate population (45/45 entries either map to a pre-blocked REDRESS
family with explicit cite + fresh material differential, or are
process/build-invariant entries that do not propose a route at all, or
are explicit pre-blocked-anti-pattern references documenting the
rejection rather than re-opening it). **One ACCEPT-WITH-NOTE** for p2e
Gap 5 REDRESS-80 differential (F-1 — the dispatch-context-flagged
candidate; correctly framed but burden-of-proof properly deferred to
S-P3). **Five NEW findings logged** (F-1 through F-5); **zero REVISE;
zero REJECT**.

The dispatch §2 watch-list — REDRESS 28+33, 50–55, 60–72, 80, 82–84,
88, 89, 96/97/98, 119/120, 126 — is satisfied with **quadruple canonical
coverage**: p2b §4 catchall (admission-process axis), p2c §4 enumeration
(arch-instruction axis), p2d §4.1–§4.7 (substrate axis), p2a §4
(SOTA-comparator axis). p2e §4.1 + p2f §4 are the per-candidate audit
layers underneath.

**P2-B claim verification**: the 5-stage admission gate is indeed
structurally REDRESS-pre-block-safe by construction. Every watch-list
REDRESS family has CH3 pre-block coverage in at least one (typically
2–3) of the 29 candidate enumerations (p2f 14 + p2e 8 + p2a 7), AND
at least one P2-B admission stage that fails the route closed. The
Lock 1 v+1 substrate manifest is the load-bearing structural guarantee
(Stage E enumerations).

**P2-E gap 5 REDRESS-80 differential**: correctly flagged for S-P3
decision per dispatch context; not silently re-opened. The S-P3
dispatch must carry the REDRESS-80 re-evaluation checklist (F-1) and
recognise that p2e Gap 5, p2f C5, and p2c C-P2C-3 are three aligned
proposals converging on the same admission path.

CH3 V1 convergence vote: **CONVERGE**. No CH3-grounded blocker to
S-P3 dispatch.
