# SK-V14 S-P2 V1 CH1: Correctness

Pass: S-P2 CHALLENGE V1.
Date: 2026-05-23.
Lens: CH1 (CORRECTNESS) — per `restart/prompts/skinny/PASS-2-RESEARCH.md:95-100`.

Disposition vocabulary: ACCEPT / REVISE / REJECT, per artefact + per
candidate. The header verdict per artefact is the maximum-severity
disposition across that artefact's candidate pool plus its prose claims
(SOTA-comparator strictness, ISA citation, P1 antecedent grounding).

---

## §0 — Contract restated

Per `PASS-2-RESEARCH.md §3 CH1`:

1. **Antecedent grounding.** Every candidate primitive traces to a named
   S-P1 hot leaf in the SK-V14 P1-A/B/C/D/E census; a candidate without a
   P1 antecedent is a speculative kernel and CH1 REJECTs it.
2. **SOTA-comparator citation + strictness plane.** Every SOTA-comparator
   claim cites the correct source (asmjson / sonic-rs / simdjson /
   yyjson) with a pinned HEAD or release identifier, and names the
   strictness plane (strict vs permissive / lossy / eager-DOM) the row
   admits against. Anchoring against a non-plane-correct or permissive
   comparator is a CH1 failure under SK-V14 R1.
3. **ISA citation.** Every ISA / instruction / target-feature claim is
   cited against the architecture reference manual (Arm A64 / Arm ACLE /
   Arm Neon Intrinsics Reference / Intel SDM / AMD APM) or a primary
   architecture publication (WikiChip, vendor whitepaper, peer-reviewed
   architecture blog).

S-P1 input is valid: `research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md`
discharged S-P1 with the six-axis V2 fold (15-tap commit and orphan
REVISE close per the `069ba203c` log entry); P1-E is the binding hot-leaf
ledger.

Artefact-line count cross-check: dispatch context §1 records
`p2b-dav1d-process.md (330 lines)`; the file at HEAD is 217 lines. Both
the dispatch-context line count and the artefact's `:1-737`
`checkasm_parity.rs` citation are stale-line-count notes; symbol
identities verify at HEAD (verified via direct grep on
`runtime/src/grammars/json/generated.rs:33,43,45,466,506` +
`runtime/src/grammars/json/scan.rs:22,32,107,131` +
`parse-that-regex/src/lib.rs:718,945` +
`bbnf-bench/src/generated_real_typed.rs:2949`). The drift is recorded but
does not invalidate the antecedent census.

---

## §1 — Per-artefact verdict summary

| Artefact | Candidate pool | CH1 verdict | Headline |
|---|---|---|---|
| p2a-sota-teardown.md | C1–C7 (7) | **ACCEPT** | Every candidate carries a named P1 antecedent; comparator anchors pinned to HEAD SHAs per source; strictness-plane discipline explicit and R1-binding. |
| p2b-dav1d-process.md | §2.A–§2.E stages (5; process, not primitives) | **REVISE** | Process gates are P1-antecedent-bounded ("the antecedent surface is the full P1-E §2.1/§2.2/§2.4 hot-leaf census"). FFmpeg / dav1d source-citation lacks pinned upstream HEAD SHAs (paper-close adjacency; CH6 cross-fire). |
| p2c-arch-esoterica.md | C-P2C-1..C-P2C-8 (8) | **REVISE** | 4 candidates are stamped `NOT-S-P3-ELIGIBLE` or process-only at V1 (C-P2C-1, -3, -6, -7, -8 by various flavours); the 4 stamped routes retain identifiers in the candidate table without explicit "not a CH1-grounded S-P2 candidate at V1" disposition language that S-P3 can refuse to shortlist. C-P2C-6 has no SK-V14 hot expression by the artefact's own §1.7 admission. ISA cites are strong (ACLE + Neon Intrinsics Reference + Apple sysctl). |
| p2d-substrate-tape.md | C-P2D-1, C-P2D-2, C-P2D-3 active + C-P2D-4 documented as REJECT-by-history (3 + 1) | **REVISE** | C-P2D-3 (sparse-flag gating) is self-admitted as having **no P1 hot leaf** ("This candidate is a CH4-pre-block falsifier — the hot-leaf consequence is zero"); listed "for completeness so CH4 can dispose it". Per CH1 this is a speculative kernel disposition that should fold to a non-candidate, not a candidate-with-known-zero-P1-evidence row. |
| p2e-parse-that-gaps.md | Gap 1–8 + Gap 7.5 (9) | **ACCEPT** | Each gap names a parse-that-regex or generated-runtime consumer site; each carries a path:line scalar reference; each grounds on a P1-E primitive class (string / unicode / number / scan). Gap 8 grounds on a "composite" P1 antecedent (`skip_string_plain` UTF-8 path + `validate_utf8_codepoint`) — composite but not speculative. ISA cites to Arm AArch64 Reference Manual Issue J.a, SIMD intrinsics enumerated. |
| p2f-grammar-neutral.md | C1–C14 (14) | **REVISE** | C8 (comment-skip) is self-admitted at §2.8 to have **zero P1 antecedent** and depends on the §1.1 NEUTRAL-PENDING-CONSUMER bucket. C6 (dispatch primitive) and C7 (whitespace skip) carry "indirect" P1 antecedents that are envelope-masked and depend on the deferred F-V2-P1ABC-RERECORD packet for direct measurability; per CH1 the indirect grounding must be stamped explicitly as "P1 antecedent: deferred-via-rerun, admit-gate conditional on rerun" rather than a present P1 leaf. C10/C12/C13 carry "indirect via C1+C2+C4" antecedents and inherit the same conditional. |

**Aggregate ACCEPT rate at V1: 2 / 6 artefacts ACCEPT; 4 / 6 REVISE; 0
REJECT.** Per-candidate aggregate: 38 candidate rows reviewed across the
six artefacts; **31 ACCEPT, 7 REVISE, 0 REJECT** (per §2 per-candidate
table below).

The REVISE dispositions are all narrow: candidate-row disposition-language
sharpening (P2-C) or speculative-row demotion (P2-D C-P2D-3, P2-F C8) or
explicit conditional-antecedent stamping (P2-F C6/C7/C10/C12/C13).
**Zero candidates are REJECTed at V1**; the artefacts' candidate pool is
correctness-grounded subject to the V2 disposition-language sharpening
named in §3.

---

## §2 — Per-candidate verdict table

Per-candidate disposition per the three CH1 axes (antecedent / SOTA / ISA),
with the binding evidence trace.

### §2.1 — p2a-sota-teardown.md (7 candidates)

| Candidate | Antecedent | SOTA + plane | ISA | CH1 verdict |
|---|---|---|---|---|
| C1 `lazy_field_skip_with_index` | `DirectParser::skip_value` 39-76% on 5/7 typed rows (P1-E §2.3); verified at `generated_real_typed.rs:2949` (HEAD) | sonic-rs `LazyValue` + simdjson On-Demand as architecture pressure; sonic-rs strict struct deser as gate (R1-plane-correct per §1.4) | scalar consumer; aarch64 NEON `vceqq_u8` set-membership cited contingently | ACCEPT |
| C2 `long_string_body_simd_scan` | `unescape_string` 46.7% on `unicode_escapes-direct-Track1` (P1-E §2.2); 17/56 rank-2/3 hit rate (P1-B §2) | sonic-rs long-string SIMD architecture pressure; sonic-rs strict as gate (HEAD `03545a9530346fe279b674dd496e037d94204bc5` pinned) | NEON `vld1q_u8_x4` + `vceqq_u8` + `vcleq_u8` + `vshrn_n_u16` reduction tree (Lock 16 Validark 2024) | ACCEPT |
| C3 `digit_block_simd_accumulate` | `materialize_f64` rank-2 on canada/mesh/marine_ik direct Track1 (P1-E §2.4); `parse_decimal` 28-59% typed Track2 | sonic-rs float-fraction SIMD; sonic-rs strict + serde_json strict (R1-correct) | aarch64 NEON UDOT/SDOT (Armv8.2-A; ACLE `__ARM_FEATURE_DOTPROD`) | ACCEPT |
| C4 `force_inline_lto_envelope_discipline` | `dispatch_value` 97-100% on 13/17 parse-only (P1-E §2.1); c/B headroom vs yyjson | yyjson 0.91 c/B twitter (HEAD `95f4c61bc1e24176f2aa4f430902705a995f1c97` pinned; Lock 15 reference) | arch-neutral; rustc LTO + i-cache budget | ACCEPT |
| C5 `structural_index_singular_substrate_consumer` | P1-E §4.4 substrate-union finding; envelope dominance on 14/17 direct + 5/7 typed | simdjson stage-1/stage-2 architecture (HEAD `168ef580757d75270475b379e83c2b39787a6765` pinned) | scalar consumer; substrate-union (Lock 1 v+1) | ACCEPT |
| C6 `parse_attribution_envelope_cracker` | every dispatch-envelope row (27/34 envelope rank-1) | profile discipline (not SOTA-comparator-bound) | arch-neutral | ACCEPT (process-gate; explicit) |
| C7 `unicode_escape_neon_nibble_decode` | `read_hex_unit_scalar` 100% on `y_string_unicode` parse-only (P1-E §2.1); `unescape_string` 46.7% on `unicode_escapes` direct (P1-E §2.2) | sonic-rs / yyjson scalar (no public SIMD comparator) — explicitly noted | aarch64 NEON TBL/TBX (`vqtbl1q_u8` / `vqtbx4q_u8`); ACLE cite | ACCEPT |

P2-A verdict: **ACCEPT (7/7).** Comparator-strictness discipline is the
strongest in the cohort — §1.4 names the R1-binding plane per comparator
per row, explicitly flags `sonic_rs::from_slice::<Value>` as the
audit-falsified P-2 anti-pattern, and pins all four upstream HEADs to
2026-05-21-verified SHAs in §5.3.

### §2.2 — p2b-dav1d-process.md (5 process stages)

The artefact's scope is the admission process, not new primitives; per
its own §2 prelude, "This agent's scope is the *process*, not new SIMD
primitives. The five §2 entries below are therefore the **admission
gate stages** S-P3 must run every candidate primitive (from P2-C/D/E)
through." The CH1 axes apply to the process stages' prose-cited
antecedents and external citations.

| Stage | Antecedent | External cite | ISA | CH1 verdict |
|---|---|---|---|---|
| §2.A Scalar-Reference Authoring | every P1-E hot leaf classified `scan`/`string`/`unicode`/`number`/`tape` | dav1d C-reference + checkasm.{c,h} (unpinned HEAD) | portable | REVISE (cite-pin) |
| §2.B Differential Checkasm Cell | exists in `crates/bbnf-simd/tests/checkasm_*.rs` (11 cells); FFmpeg `checkasm.h` lineage | FFmpeg checkasm (unpinned HEAD) | conditional-compile | REVISE (cite-pin) |
| §2.C Lock 16 Allowlist Citation | per-row; Lock 16 surface at `LOCKS.md:282-307` | per-comparator cite per Stage C contents | per-arch | ACCEPT |
| §2.D Same-Wave Consumer | per-row; SK-V6 W2 Candidate-2/3 closure at `REDRESS.md:1385,1445` | n/a | per-arch | ACCEPT |
| §2.E Manifest-Row Cohesion + Substrate Declaration | Lock 1 v+1 manifest at `LOCKS.md:73-82` + Lock 16 manifest at `LOCKS.md:309-318` | n/a | per-arch | ACCEPT |

P2-B verdict: **REVISE.** Stages §2.A and §2.B cite "FFmpeg
`tests/checkasm/checkasm.{c,h}`" and "dav1d `tests/checkasm/`" without
pinned upstream HEAD SHAs in §5.1 (contrast with P2-A's four pinned
comparator HEADs). The dav1d `src/x86/msac.asm:80-220` cite at §5.1 IS
pinned (per `LOCKS.md:305` lineage), and the dav1d `src/arm/cpu.c:87-95`
+ `tests/checkasm/loopfilter.c:177-188` HEAD `1718ff9aded99f0a89f5c7940d6afb8948301e33`
appear in P2-A §5.4 (which P2-B inherits transitively), but P2-B's own
sources block lacks the upstream-SHA pinning P2-A established. CH1 fold
requirement: V2 should add the FFmpeg HEAD SHA + dav1d HEAD SHA inline
in §5.1 (the dav1d HEAD `1718ff9aded99f0a89f5c7940d6afb8948301e33` is
already pinned in P2-A §5.4 and can be inherited verbatim). The process
content itself is P1-antecedent-bounded and ACCEPT-grade; only the
external-cite pinning is the CH1 gap.

### §2.3 — p2c-arch-esoterica.md (8 candidates)

| Candidate | Antecedent | SOTA + plane | ISA | CH1 verdict |
|---|---|---|---|---|
| C-P2C-1 `ascii_set_member64_css_delimiter` | self-admitted: "no row will move under PRUNE-2 because no CSS L4 row has profile-attributable parser bytes"; SK-V13 W4 microbench inventory | n/a (microbench evidence, not SOTA-anchored) | aarch64 NEON TBL / eq-set | REVISE (demote to inventory-not-candidate per dispatch-disposition; the `NOT-S-P3-ELIGIBLE` stamp is correct but the candidate-table row should not present as "candidate" without explicit non-candidate framing) |
| C-P2C-2 `pmull_cssc_structural_union_emit64` | `scan_structurals` SIMD/scalar 1.49-5.04x (P1-E §2.4); structural delegates at `scan.rs:203,239,267` | architecture pressure only (no SOTA-comparator-row anchor) — explicitly PRE-BLOCKED by REDRESS 88+89+96-98 unless Union-C wave with deletion of scalar consume | Arm PMULL (FEAT_PMULL) + CSSC CTZ (FEAT_CSSC) — ACLE-cited | ACCEPT (PRE-BLOCKED disposition explicit) |
| C-P2C-3 `udot_digit_span_x4` | conditional on F-V2-P1ABC-RERECORD; current envelope-masked | DotProd / VPDPBUSD architecture pressure | UDOT (FEAT_DotProd; ACLE `__ARM_FEATURE_DOTPROD`) | ACCEPT (`NOT-S-P3-ELIGIBLE` with explicit re-evaluation gate) |
| C-P2C-4 `tbl_tbx_escape_decode_batch` | `unescape_string` 46.7% on `unicode_escapes` direct (P1-E §2.2); `read_hex_unit_scalar` 100% on `y_string_unicode` (P1-E §2.1) | sonic-rs / yyjson scalar reference; no SIMD SOTA-comparator-row | NEON TBL `vqtbl1q_u8` (already in tree at `unescape_uxxxx.rs:83,129`); TBX `vqtbx4q_u8` | ACCEPT (S-P3-ELIGIBLE for JSON `\uXXXX`) |
| C-P2C-5 `string_special_64_context` | `match_tiny_plain_string_with_cap::<16>` 96.3% on `distinct_values` (P1-E §2.1) | architecture pressure (no SOTA-comparator-row) | NEON EXT + shifts | ACCEPT (conditional-on-C2-or-C4-landing posture explicit) |
| C-P2C-6 `eor3_string_mask_fusion` | self-admitted: "None — SK-V14 scanner uses ANDs/ANDNs (`scan.rs:225-265`), not XOR triples" | n/a | NEON EOR3 (FEAT_SHA3) — ACLE-cited | **REVISE** — candidate with explicitly zero P1 antecedent at SK-V14; the `NOT-S-P3-ELIGIBLE` stamp is correct but per CH1 a "no current hot expression" candidate is the textbook speculative-kernel pattern. V2 must demote to non-candidate inventory or stamp explicit "no SK-V14 P1 antecedent, retained for cross-tranche identifier stability only". |
| C-P2C-7 `byte_context_orphan_resolution` | REDRESS-126 orphan inventory; no SK-V14 production caller | n/a | NEON EXT (`vextq_u8`) | REVISE (close-hygiene candidate; per CH1 same posture as C-P2C-6 — admit explicit "not a CH1-grounded candidate at V1") |
| C-P2C-8 `parse_attribution_profile_rebuild_gate` (NEW V1) | all 13/17 + 14/17 envelope-bound (P1-E §4.1) | n/a (process gate) | n/a | ACCEPT (process-gate; F-V2-P1ABC-RERECORD packet) |

P2-C verdict: **REVISE.** The Lock 16 ISA citations + Apple
M-series sysctl evidence + REDRESS pre-block notes are
all CH1-grade. The narrow REVISE is for **C-P2C-1, C-P2C-6, C-P2C-7
disposition-language sharpening**: per CH1's "candidate with no P1
antecedent is a speculative kernel — REJECT", these three rows should
either (a) be removed from the candidate-table and folded into a
"non-candidate inventory" sub-section, or (b) carry explicit
"NOT-S-P3-ELIGIBLE — zero P1 antecedent at SK-V14, retained for
cross-tranche identifier stability only" disposition language so S-P3
cannot accidentally shortlist them. The artefact already has the
`NOT-S-P3-ELIGIBLE` stamp; the V2 fold needs to lift these out of the
candidate enumeration entirely or harden the language.

### §2.4 — p2d-substrate-tape.md (3 active + 1 pre-blocked)

| Candidate | Antecedent | SOTA + plane | ISA | CH1 verdict |
|---|---|---|---|---|
| C-P2D-1 `BackendShape::SinkOnly` activation | `parse_object_value_at_direct::<JsonDigestSink>` 81.13% twitter direct Track 1 + 4 more rows ≥80% (P1-B §2 direct rows) | substrate-shape evidence per `SUBSTRATE.md`; ARCH §7.3 5-shape vocabulary | substrate-side / lowering-pass | ACCEPT |
| C-P2D-2 `OffsetTapeStats` column extension | P1-A §4 `copy_nonoverlapping` 9.5-11.4% rows (tape-commit pressure) | substrate-measurement (no SOTA-comparator-row anchor) | substrate-side / measurement | ACCEPT |
| C-P2D-3 sparse-flag gating | self-admitted: "**not a P1 hot leaf** (the flag-band cost is parser-state-init, not parse-time-loop) … the hot-leaf consequence is zero" | n/a | substrate-side / allocation | **REVISE** — by the artefact's own admission, this is a speculative-kernel-by-CH1 pattern. The artefact prose at §2 C-P2D-3 names it "a CH4-pre-block falsifier … Listed here for completeness so CH4 can dispose it; named as a substrate-side observation, not advocated as primitive." Per CH1 this is correct CH1 reading; V2 must demote from the candidate-list to a §1.6-style observation (where (a) and (b) already live) so S-P3 cannot shortlist on a zero-evidence row. The author already signals this; V2 makes the demotion explicit. |
| C-P2D-4 `BackendShape::EventTape` interrogation | REJECT-by-REDRESS-96/97/98 history | architecture-pressure-only reference | would-be SIMD-coupled | ACCEPT (explicitly NOT proposed; documented as anti-pattern reference) |

P2-D verdict: **REVISE.** The §1.5 substrate-union closure ("the
substrate union holds at HEAD; tape + structural projection ARE one
substrate; the producer asymmetry between Track 1 and Track 2 is the
projection-plane discriminant Lock 1 admits, not a substrate split") is
the artefact's load-bearing finding and is CH1-correct. The narrow CH1
fold is C-P2D-3 demotion to §1.6 observation-not-candidate as named
above. C-P2D-1 + C-P2D-2 are CH1-clean.

### §2.5 — p2e-parse-that-gaps.md (9 gaps)

| Gap | Antecedent | SOTA + plane | ISA | CH1 verdict |
|---|---|---|---|---|
| Gap 1 `scan_string_special_block_sweep_64` | `parse_that_regex::skip_string_plain_trusted` (envelope-masked, F-V2-P1ABC-RERECORD-pending); SIMD probe rank-1 16/17 corpora | simdjson aarch64 `find_quote_mask_and_bits` shape (cited) | NEON `vld1q_u8` × 4 + `vceqq_u8` + Arm A64 Reference Manual Issue J.a | ACCEPT |
| Gap 2 `unescape_uxxxx_x8_neon` | `read_hex_unit_scalar` 100% on `y_string_unicode` parse-only (P1-E §2.1) | sonic-rs / yyjson scalar baseline | NEON `vqtbl1q_u8` × 2 + `vminvq_u8` (Arm AArch64 RM J.a) | ACCEPT |
| Gap 3 `ascii_whitespace_skip_64` | `skip_ascii_whitespace` called at every JSON value position (P1-E §1.3); envelope-masked | n/a (substrate primitive) | reuses existing `byte_class_from_eq_set_64` per `bbnf-simd/src/lib.rs:282` | ACCEPT |
| Gap 4 `utf8::validate_block_streaming` | composite: `skip_string_plain` UTF-8 path + `validate_utf8_codepoint` (envelope-masked); `unescape_string` 46.7% inverse-shape antecedent | simdutf 5.x `arm_validate_utf8.cpp` + Lemire/Keiser Hoehrmann shape | NEON `vqtbl1q_u8` (Arm AArch64 RM J.a) | ACCEPT |
| Gap 5 `parse_16_digits_dotprod` | `number::scan_digit_run` on float-heavy mesh/canada/numbers (mode-III SIMD ratios 4.96-5.04x per P1-E §2.4); cited REDRESS-80 material differential | fast_float / Lemire Eisel-Lemire context | UDOT (FEAT_DotProd) + `vmulq_u32`; AVX-512 VNNI `vpdpwssd` x86 counterpart | ACCEPT |
| Gap 6 `scan_string_with_carry_64` | `parse_that_regex::skip_string_plain_trusted` (envelope-masked) + `scan_structurals` SIMD probe rank-1 16/17 (P1-E §2.4) | simdjson 3.x `find_quote_mask_and_bits` cited explicitly | NEON composition (no PMULL — REDRESS 88 differential) | ACCEPT |
| Gap 7 `scan_digit_run_simd_64` | `number::scan_digit_run` per P1-E mode-III §2.4 float-heavy | (same as Gap 5 chain) | depends on Gap 7.5 `byte_class_from_range_64` + Gap 5 | ACCEPT |
| Gap 7.5 `byte_class_from_range_64` | Gap 7 (digit-run); generalises to UTF-8 continuation, CSS hex-digit, BBNF-self ident | sibling of `byte_class_from_eq_set_64` (in-tree primitive) | NEON `vcgeq_u8` × 8 + `vandq_u8` × 4 + `movemask_u8x16` | ACCEPT |
| Gap 8 `utf8_codepoint_scan_64` | composite: `skip_string_plain` UTF-8 path + `validate_utf8_codepoint` per-cursor; same composite as Gap 4 | simdutf 5.x `arm_validate_utf8.cpp` Hoehrmann | NEON `vqtbl1q_u8` over 16-entry leading-byte class table | ACCEPT |

P2-E verdict: **ACCEPT (9/9).** Every gap names a parse-that-regex
consumer site at path:line, a scalar reference path:line (existing or
required), and a P1-antecedent class (with envelope-masking explicitly
disclosed where applicable). The §4.7 F-V2-P1ABC-RERECORD-dependency
note is a model CH6-paper-close-mitigation: "the **shape** of the gap
(the primitive's algorithmic kernel) does NOT depend on rerecord, only
the **per-row admit gate** at S-P3 does." Per CH1 this is correct
antecedent grounding because the inner-leaf list is named at dispatch
context §1 and the rerun gates S-P3 admission, not S-P2 enumeration.
External cites pin simdjson 3.x + simdutf 5.x + dav1d 1.4.x by version
band (slightly looser than P2-A's commit-SHA pinning but still
CH6-defensible).

### §2.6 — p2f-grammar-neutral.md (14 candidates)

| Candidate | Antecedent | SOTA + plane | ISA | CH1 verdict |
|---|---|---|---|---|
| C1 structural-byte SIMD classify | `scan_structurals` SIMD probe rank-1 16/17 corpora (P1-E §2.4) | sonic-rs / simdjson architecture pressure | NEON `vqtbl4q_u8` (Lock 16:284 Lemire 2019) | ACCEPT |
| C2 quoted-string boundary scan (CLMUL prefix-XOR) | `scan_structurals` rank-1 + `distinct_values` → `match_tiny_plain_string_with_cap::<16>` | simdjson founding primitive (Langdale & Lemire 2019, arXiv:1902.08318 cited) | Arm PMULL + AVX-512 VPCLMULQDQ (Lock 16:294) | ACCEPT (with REDRESS-88 fresh-differential framing) |
| C3 escape canonicalisation | `unescape_string` 46.7% on `unicode_escapes` direct + `read_hex_unit_scalar` 100% on `y_string_unicode` (P1-E §2.1+§2.2) | n/a (scalar primitive) | Lock 16:286 byte popcount + saturating-add cite (digit-block sub-step) | ACCEPT |
| C4 tiny-keyword-set match | `match_tiny_plain_string_with_cap::<16>` 96.3% on `distinct_values` (P1-E §2.1) | sonic-rs baseline | NEON `vqtbl1q_u8` (Lock 16:284) + LD4-interleaved (Lock 16:288) + svmatch port (Lock 16:290) | ACCEPT |
| C5 digit-block number decode | mesh/canada/numbers SIMD ratios 4.96-5.04x (P1-E §2.4); CSS prose-pinned `GENERIC_NUMBER_CONFIG` | architecture pressure | UDOT (Lock 16:287) + AVX-IFMA (Lock 16:295) + VNNI (Lock 16:296) | ACCEPT |
| C6 branch-on-first-byte dispatch | `dispatch_value` 13/17 + direct envelopes 14/17 (P1-E §2.1+§2.2; LARGEST single P1 finding) | n/a (dispatch primitive) | n/a | **REVISE** — P1 antecedent is the **envelope itself**; per the artefact's own §2.6 CH2 binding, "S-P2 must enable parse-attribution cargo feature" before inner primitives are measurable. V2 must add explicit "P1 antecedent: envelope-only at SK-V14; admit-gate conditional on F-V2-P1ABC-RERECORD" disposition language so CH1 can disambiguate "envelope is the P1 antecedent" (legitimate; this IS the dispatch primitive) from "primitive behind the envelope is the P1 antecedent" (deferred-via-rerun). |
| C7 leading-whitespace prefix skip | indirect — `dispatch_value` envelope calls whitespace-skip every value position | n/a | Lock 16:284+:286 | **REVISE** — indirect-antecedent stamping per C6 fold |
| C8 comment-skip primitive | **NONE** — self-admitted "(CH6 risk if this candidate is shortlisted on JSON profile alone; the BBNF-self consumer is mandatory)" | n/a | Lock 16:284 | **REVISE** — zero JSON P1 antecedent by §2.8 admission; NEUTRAL-PENDING-CONSUMER bucket per §1.1 partition. Per CH1 a candidate with zero JSON P1 antecedent admitted on cross-grammar evidence alone is the canonical Lock-14-v+1-fail-closed case: the §3 verdict NEUTRAL-PENDING-CONSUMER is correct, but V2 must explicit-stamp that S-P3 admission requires the same-wave non-JSON consumer to land in the wave that admits C8 OR the candidate drops. Already named in §4 "General risks" — V2 promotes to disposition language on the §2.8 row itself. |
| C9 offset-tape bulk emit | `distinct_values` → `bulk_emit_positions_64_neon` 48.2% (P1-E §2.4) | Lock 16:292 AVX-512 VBMI2 + :299 BMI2 PDEP | NEON existing primitive | ACCEPT |
| C10 cross-chunk byte-context propagation | indirect via C1+C4 | Lock 16:285 abstract-primitive lock-prose | NEON `vextq_u8` (Arm A64 ISA, dav1d lineage) | **REVISE** — indirect-antecedent stamping per C6 fold |
| C11 substrate-walk-with-shape-validation | `DirectParser::skip_value` 5/7 typed rows 39-76% (P1-E §2.3) | n/a | scalar | ACCEPT |
| C12 keyword-set 16-byte alphabet membership (SVE2 svmatch port) | indirect via C1 | Lock 16:290 Lemire 2026 NEW 2026-05-12 | NEON `vceqq_u8 + vorrq_u8` reduction tree | **REVISE** — indirect-antecedent stamping per C6 fold |
| C13 branchless 3-way XOR (BCAX / TBL fusion) | indirect via C1+C2+C12 (fusion primitive) | Lock 16:289 Arm SHA3 ext. NEW 2026-05-12 | NEON `vbcaxq_u8` + `veor3q_u8` (Armv8.2-A SHA3) | **REVISE** — indirect-antecedent stamping per C6 fold |
| C14 i-cache budget constraint | `parse_value_at` 7,304 bytes per Lock 15 v+1 | yyjson 0.91 c/B + ~18 KiB i-cache reference | cross-arch (hardware fact) | ACCEPT (constraint, not primitive) |

P2-F verdict: **REVISE.** The §1.1 5-bucket partition + the §3 verdict
tally (5 NEUTRAL-WIRED + 8 NEUTRAL-CONFIG-DRIVEN + 1 NEUTRAL-PENDING-CONSUMER
+ 0 JSON-OVERFIT-REFRAMABLE + 0 JSON-OVERFIT-IRREDUCIBLE) is CH1-clean
on the **grammar-neutrality** axis (which is CH2's primary axis — P2-F
crosses CH1/CH2 deliberately per its §1.2 binding inheritance). The
narrow CH1 fold is the **6 indirect-antecedent rows** (C6/C7/C10/C12/C13
+ C8 with zero JSON antecedent): each needs explicit per-row
"P1 antecedent: envelope-only / indirect-via-CN / deferred-via-rerun /
non-JSON-only" disposition language so the §3 verdict NEUTRAL-* tag and
the antecedent grounding cannot be conflated by S-P3 readers.

---

## §3 — Aggregate fold requirements (V1 → V2)

The 7 REVISE dispositions across the 6 artefacts collapse to **3
disposition-language sharpenings** the V2 fold must execute:

### §3.1 — Fold-1: external-cite SHA pinning (P2-B §5.1)

Add to `p2b-dav1d-process.md §5.1` the FFmpeg HEAD SHA +
dav1d HEAD SHA inline anchors. The dav1d HEAD
`1718ff9aded99f0a89f5c7940d6afb8948301e33` is already pinned in P2-A
§5.4 (and may be inherited verbatim per cross-artefact citation
consistency); FFmpeg HEAD `085714182302333dd83dcb9c36cf828dc4eba929` is
pinned in P2-A §5.4 likewise. V2 adds these two SHAs as line-item
anchors in P2-B's `tests/checkasm/checkasm.{c,h}` + `tests/checkasm/`
citations.

### §3.2 — Fold-2: zero-P1-antecedent candidate demotion

The four candidates with **explicitly zero SK-V14 P1 antecedent**:

- P2-C C-P2C-1 (CSS L4 row absent per PRUNE-2; SK-V13 microbench-only)
- P2-C C-P2C-6 (no SK-V14 three-input boolean hot expression)
- P2-C C-P2C-7 (REDRESS-126 orphan; no production caller)
- P2-D C-P2D-3 (sparse-flag gating; "hot-leaf consequence is zero" per
  artefact §2 self-admission)
- P2-F C8 (comment-skip; "NONE" antecedent per §2.8 self-admission)

V2 fold: either (a) lift these out of the `## §2 — Candidate primitives`
enumeration and into a `§2.X — Non-candidate inventory` or
`§2.X — Cross-tranche identifier stability inventory` sub-section, or
(b) prepend each candidate's body with the explicit single-line
disposition `**Disposition (CH1):** non-candidate at SK-V14 V1 — zero P1
antecedent; retained for [reason]; not S-P3-shortlist-eligible.` so
downstream readers (including S-P3) cannot accidentally shortlist on a
zero-evidence row. Either form satisfies CH1; the orchestrator's
preference per `[no-deferrals]` is the (a) sub-section demotion.

### §3.3 — Fold-3: indirect-/envelope-antecedent disposition language

The five P2-F candidates with **indirect or envelope-only antecedents**
(C6, C7, C10, C12, C13): each needs explicit single-line disposition
per the §2.6 table above:

- C6 "P1 antecedent: dispatch_value envelope (the candidate IS the
  dispatch primitive; inner-primitive measurability deferred to
  F-V2-P1ABC-RERECORD)."
- C7 "P1 antecedent: envelope-masked (whitespace-skip step inside
  dispatch_value); admit-gate conditional on F-V2-P1ABC-RERECORD."
- C10 "P1 antecedent: indirect via C1 + C4 (the fusion primitive
  applied inside the other primitives' inner loops); direct evidence
  requires F-V2-P1ABC-RERECORD."
- C12 "P1 antecedent: indirect via C1 (specialises the small-alphabet
  case of structural-byte classify); direct evidence requires
  F-V2-P1ABC-RERECORD."
- C13 "P1 antecedent: indirect via C1 + C2 + C12 (fusion primitive
  applied inside their inner loops); direct evidence requires
  F-V2-P1ABC-RERECORD."

Per CH1 the indirect-antecedent stamping is what distinguishes
"speculative kernel" (no antecedent) from "kernel grounded on the
envelope-masked inner-primitive that the parse-attribution rerun will
expose" (legitimate research candidate awaiting empirical confirmation).
The V1 cohort's §1.1 verdict partition implicitly captures this
(NEUTRAL-* buckets are about grammar-neutrality, not antecedent
directness); V2 makes the antecedent-directness an explicit per-row
column.

---

## §4 — CH1 cross-cuts (notes for the consolidation aggregator)

### §4.1 — F-V2-P1ABC-RERECORD packet propagates the indirect-antecedent surface

The dispatch context §1 carry-forwards inherit the F-V2-P1ABC-RERECORD
packet from S-P1 V1 fold; per P1-E §4.1 the `parse-attribution` cargo
feature is `runtime`-crate-private and must be invoked via
`--features runtime/parse-attribution`. Until this rerun lands, the
inner primitives behind `dispatch_value`, `parse_object_value_at_direct`,
`parse_array_element_at_direct` are envelope-masked. **Six candidates
across P2-A/P2-C/P2-E/P2-F depend on this rerun for direct
measurability**: P2-A C6 (process gate is exactly the rerun); P2-C C-P2C-3
(UDOT awaiting numeric-leaf naming); P2-C C-P2C-8 (process gate is the
rerun); P2-E Gap 1 + Gap 3 + Gap 4 (envelope-masked string/whitespace/UTF-8
inner primitives); P2-F C6/C7/C10/C12/C13 (indirect-antecedent). The
rerun is the orchestrator's same-wave deliverable for the first SK-V14
implementation wave per P2-C C-P2C-8 + dispatch context §1
inheritance carry-forwards.

### §4.2 — `sonic_rs::from_slice::<Value>` audit-falsification holds across the cohort

Every artefact in the cohort that names a sonic-rs anchor cites the
strict struct deser path (per-corpus typed deser), explicitly NOT
`from_slice::<Value>` (eager-DOM). P2-A §1.4 names this explicitly and
the cohort honours it; P2-F §5.2 inherits via Lock 8 v+1 at `LOCKS.md:120-137`.
This is the canonical R1 + P-2 audit-pack discipline holding across S-P2
V1. CH1 finding: this is the single largest comparator-discipline
consistency in the V1 cohort; no candidate's SOTA-anchor row violates
strict-vs-strict.

### §4.3 — ISA citations are uniformly Arm A64 + ACLE + Apple sysctl + Intel SDM (for x86 secondary refs)

Across P2-A, P2-C, P2-E, P2-F, every aarch64 ISA claim cites either Arm
ACLE (`__ARM_FEATURE_DOTPROD`, `__ARM_FEATURE_SHA3`, etc.), Arm Neon
Intrinsics Reference (`vqtbl4q_u8`, `vdotq_u32`, `vmull_p64`,
`vextq_u8`, `veor3q_u8`, `vbcaxq_u8`, etc.), Apple Silicon `sysctl
hw.optional.*` for feature-flag availability, or Lock 16 lock-prose
abstract-primitive declarations (lines 282-307) which carry their own
citation chains. x86 secondary citations (P2-C §1.9 + P2-F §2.x) cite
Intel Intrinsics Guide + WikiChip + BranchFree.org per primitive. CH1
finding: no ISA claim is unsourced.

### §4.4 — Stale line-count notes (process observation, not CH1 disposition)

Two line-count drifts observed:

- Dispatch context §1 records `p2b-dav1d-process.md (330 lines)`; the
  file at HEAD is 217 lines.
- P2-B §1.1 cites `tests/checkasm_parity.rs:1-737`; HEAD line range
  needs spot-verification (not done at this CH1 lens).

Neither drift invalidates the antecedent census or the substrate
findings; symbol identities verified at HEAD. CH1 fold optional: V2 may
re-spot-check the `checkasm_parity.rs` line range and update the
dispatch-context line count.

---

## §5 — CH1 final disposition + ACCEPT rate

**Artefact ACCEPT rate: 2 / 6 = 33.3%.**
**Candidate ACCEPT rate: 31 / 38 = 81.6%** (per the per-candidate
tables in §2.1–§2.6).

The 4 / 6 REVISE artefacts (P2-B, P2-C, P2-D, P2-F) collapse to **three
narrow disposition-language sharpenings** (§3.1–§3.3) the V2 fold must
execute: (1) external-cite SHA pinning in P2-B; (2) zero-P1-antecedent
candidate demotion across P2-C/P2-D/P2-F; (3) indirect-/envelope-
antecedent disposition language in P2-F. **Zero candidates are REJECTed
at V1**; the cohort's empirical substrate is correctness-grounded modulo
the three folds.

Per `ORCHESTRATOR.md §3Z` two-cycle-stable convergence threshold (≥95%
ACCEPT for two consecutive cycles), the V1 artefact-ACCEPT rate (33.3%)
is below threshold; the candidate-ACCEPT rate (81.6%) is also below
threshold. V2 dispatch is mandatory under §3Z; the three §3 folds are
the V2 input.

---

## §6 — Sources (CH1 lens-internal citations)

### §6.1 — Authority

- `restart/prompts/skinny/PASS-2-RESEARCH.md:95-100` (CH1 contract);
  `:120-137` (CH3 REDRESS pre-block surface — cross-cuts CH1
  via fresh-material-differential REDRESS-guard requirement)
- `restart/prompts/ORCHESTRATOR.md §3W + §3Z` (lens registry; convergence)
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7` (overfit cross-cut)
- `restart/skinny/tranches/sk-v14/research/p2/S-P2-DISPATCH-CONTEXT.md
  §0-§5` (S-P2 binding)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CHALLENGE-CONTEXT.md
  §0-§4` (this CHALLENGE wave's binding)
- `restart/locks/LOCKS.md` (Lock 1 substrate-union; Lock 14
  grammar-neutrality; Lock 15 i-cache budget; Lock 16 SIMD/ASM
  allowlist + abstract-primitive declarations)

### §6.2 — Artefacts under review

- `restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md` (367 L)
- `restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md`
  (217 L; dispatch context §1 records 330 L — line-count drift noted §4.4)
- `restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md` (143 L)
- `restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md` (257 L)
- `restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md` (342 L)
- `restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md` (334 L)

### §6.3 — P1 antecedent ledger (binding)

- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md
  §1.3` (CH2 primitive vocabulary);
  `§2.1` (parse-only 17/17);
  `§2.2` (direct 17/17);
  `§2.3` (typed 7/7);
  `§2.4` (mode-III SIMD/scalar ratios);
  `§4.1` (envelope-dominance Lock-14 mis-attribution census);
  `§4.4` (substrate-union typed-skip observation);
  `§4.5` (float-heavy SIMD ratio);
  `§4.7` (REDRESS guard reconciliation)
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md
  §2` (direct + typed per-corpus rank tables)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md`
  (S-P1 V2 close authorising S-P2)

### §6.4 — Bbnf source anchors (HEAD-verified for CH1)

- `skinny/crates/runtime/src/grammars/json/generated.rs:33,43,45,466,506`
  (`parse-attribution` cfg + `dispatch_value` + direct envelopes;
  symbol identities verified at HEAD)
- `skinny/crates/runtime/src/grammars/json/scan.rs:22,32,107,131`
  (`scan_structurals`, `_scalar`, `scan_tail`, `scan_tail_byte`;
  verified)
- `skinny/crates/parse-that-regex/src/lib.rs:718,945` (`unescape_string`,
  `read_hex_unit_scalar`; verified)
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:2949`
  (`DirectParser::skip_value`; verified)
- `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:2`
  (`bulk_emit_positions_64_neon`; verified)

### §6.5 — External (SOTA + ISA) citations cross-checked

- simdjson HEAD `168ef580757d75270475b379e83c2b39787a6765` (P2-A §5.3
  pinned; P2-E inherits)
- sonic-rs HEAD `03545a9530346fe279b674dd496e037d94204bc5` (P2-A §5.3
  pinned)
- yyjson HEAD `95f4c61bc1e24176f2aa4f430902705a995f1c97` (P2-A §5.3
  pinned)
- asmjson crate 0.2.5 (P2-A §5.3 pinned)
- dav1d HEAD `1718ff9aded99f0a89f5c7940d6afb8948301e33` (P2-A §5.4 pinned;
  P2-B §5.1 inherits-needs-explicit-cite per §3.1 fold)
- FFmpeg HEAD `085714182302333dd83dcb9c36cf828dc4eba929` (P2-A §5.4
  pinned; P2-B §5.1 inherits-needs-explicit-cite per §3.1 fold)
- Arm ACLE 2026Q1; Arm Neon Intrinsics Reference 2026Q1; Arm
  Architecture Reference Manual ARMv8-A + ARMv8.2-A SHA3 (P2-C §5.1 +
  P2-F §5.2 cited)
- Intel Intrinsics Guide; WikiChip; BranchFree.org (P2-C §5.1 secondary
  refs for x86 conceptual generalisation)
- Langdale & Lemire 2019 arXiv:1902.08318 (simdjson founding-primitive
  paper; P2-F §5.2 cited)
- Lemire 2019/2023/2024/2026 series; Validark 2024 (Lock 16 cited
  primitives; P2-A §5.3 + P2-C §5.1 + P2-F §5.2)
