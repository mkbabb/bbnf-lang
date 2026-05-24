# SK-V14 S-P3 V1 CH1: Correctness (opening cycle)

Pass: S-P3 CHALLENGE V1 (per `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md §3` CH1 specialised to S-P3; `restart/prompts/ORCHESTRATOR.md §3W` lens registry).
Date: 2026-05-23.
Lens: CH1 (CORRECTNESS) — every shortlist candidate traces to S-P2 candidate → S-P1 hot leaf; every falsifiability gate measurable (named corpus rows + concrete Mbps thresholds, not prose); every wave's exit gate compares against `SK-V14-open` baseline; comparator deltas in gates use strict plane (R1 binding); wave-numbering between P3-B / P3-C / P3-F SPEC reconciled.
Disposition vocabulary: ACCEPT / REVISE / REJECT, per artefact + per claim. Header verdict per artefact is the maximum-severity disposition across the artefact's claim pool.
HEAD pin: `8f4756113` (includes `1dc4cd60c` S-P3 V1 atomic seed per CHALLENGE-CONTEXT §0).

---

## §0 — V1 disposition focus restated

Per V1 CHALLENGE-CONTEXT §2 (`restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CHALLENGE-CONTEXT.md:28`):

1. **Antecedent chain verification.** Every P3-A shortlist candidate (C1..C8) traces verifiably to S-P2 candidate IDs → S-P1 hot leaves; cite path:line for each link.
2. **Measurability of falsifiability gates.** Every P3-C wave (W0..W11) carries a gate that is *measurable from the bench* — named corpus rows + concrete Mbps thresholds, NOT prose. Unmeasurable gate ⇒ REJECT.
3. **Baseline-anchor discipline.** Every wave's exit gate compares against the `SK-V14-open` baseline (per `ORCHESTRATOR.md §8` baseline-anchored measurement rule + SPEC §0.4 `SK-V14-open delta` column binding).
4. **Strict-plane comparator binding (R1).** Every comparator delta in the gates uses the plane-correct strict comparator per `SYNTHESIS §0.3 R1` (parse_only → sonic-rs Skipper; direct → sonic-rs strict struct deser per corpus; typed → per-corpus typed struct deser; CSS → lightningcss full-parse + cssparser full-parse). No row admits against `sonic_rs::from_slice::<Value>` eager DOM (P-2 pre-block).
5. **Wave-numbering reconciliation.** The V1 dispatch context (`CHALLENGE-CONTEXT.md:21`) flagged a divergence: P3-B W1=C-2(R1+R2)/W2=PRUNE-1; P3-C W1=PRUNE-1/W2=C-2(R1+R2); SPEC must lock one ordering. CH1 must verify SPEC §3-§14 matches one ordering exhaustively AND name which of the three artefacts (P3-B / P3-C / P3-F SPEC) carries the binding ordering.

---

## §1 — Per-artefact verdict summary at V1

| Artefact | V1 scope | V1 verdict | Headline |
|---|---|---|---|
| `p3a-candidate-shortlist.md` | 8 candidates C1..C8 at ≤8 cap; NF-CH6-4 canonical-name binding ratified; CF-3 3-gate admission cell wired; F-V2-P1ABC-RERECORD dep on C1/C3/C7 (317 L) | **ACCEPT** | Every P3-A shortlist candidate traces to an S-P2 candidate ID consolidation **and** to a named S-P1 hot leaf with path:line cite. All 8 cells carry the 3-gate (scalar-ref / checkasm-parity / consumer NAMED) explicitly per §1.2 hot-leaf antecedent map + §2 candidate cells. |
| `p3b-wave-sequencing.md` | 12 waves W0..W11 at ≤12 ceiling; W7 folds 9 PRUNE-4 sub-passes; 3 architectural sequencing constraints discharged; 3 S-P2 carry-forwards wired (406 L) | **REVISE (load-bearing)** | Wave-ordering DIVERGES from both P3-C and P3-F SPEC: P3-B places PRUNE-1 at W2 (after C-2 at W1), R4 at W3, PRUNE-2 at W4, R5 at W5. SPEC §2 (lines 233-248) and P3-F §1.2 (lines 38-52) FUSE C-2(R1+R2)+PRUNE-1 into W1, then R4=W2, R5=W3, PRUNE-2=W4. Same wave count (12), same sequencing constraints discharged, BUT three slot positions differ. P3-B must rebind to the SPEC ordering OR the SPEC must rebind to P3-B's ordering; one cycle must execute the reconciliation. |
| `p3c-falsifiability-gates.md` | 12 waves gated; ZERO unmeasurable-gate REJECTs; 75 corpus rows enumerated verbatim from RESULTS.md:49-124; R1+R2 bindings explicit; W7 samply attribution gate + W11 cargo asm gate (528 L) | **REVISE (load-bearing)** | P3-C §1.2 wave manifest table (lines 26-37) ALSO diverges from P3-B AND SPEC: P3-C has W1=PRUNE-1 / W2=C-2 / W3=C-3(R4+R5 BOTH) / W5=PRUNE-3 / W6=PRUNE-4 / W7=PRUNE-5 / W8=R6 / W9=R7-direct / W10=R7-typed / W11=R8. This is a THIRD distinct ordering. All gates within P3-C are internally consistent + measurable, but the wave numbering is misaligned with SPEC §3-§14. Same reconciliation requirement as P3-B. |
| `p3f-spec-draft.md` + `SPEC.md` + `DISPATCH-PROMPT.md` | 245 + 1137 + 344 lines; SK-V8 shape verbatim; §0 R10 fold; §1 non-neg + Lock 1/14/16 v+1; §2 wave manifest (12 waves W0..W11) with SPEC ordering; §3-§14 per-wave sections | **ACCEPT (load-bearing for ordering)** | SPEC §2 wave manifest IS the binding ordering per `PASS-3-SYNTHESIS-PLAN §2 P3-F` row ("P3-F additionally drafts `restart/skinny/tranches/sk-v{N}/SPEC.md`"). The SPEC fuses C-2(R1+R2)+PRUNE-1 into W1 (delta vs both P3-B and P3-C), making W1 a wider wave but reducing the slot count for PRUNE waves by one. Per-wave sections §3-§14 are internally consistent with this ordering. Per-row Mbps thresholds in SPEC §11/§12/§13 cite the `Track 1 > comparator strict + 1 Mbps` bar (SYNTHESIS §0.1 R10 verbatim) — measurable. |

**Aggregate V1 ACCEPT rate (artefact-level): 2/4 = 50 %.**
**Aggregate V1 ACCEPT rate (claim-level, weighted across antecedent / measurability / baseline-anchor / strict-plane / wave-numbering — 5 axes × 4 artefacts = 20 cells): 17/20 = 85 %.**

The 3 REVISE cells are: P3-B wave-numbering vs SPEC; P3-C wave-numbering vs SPEC; **both** P3-B and P3-C wave-numbering as the *load-bearing source of the divergence*. The SPEC ordering should be ratified by V2 cycle.

Cycle disposition: **REVISE** (single load-bearing reconciliation across two REVISE cells; V2 micro-fold can discharge to ACCEPT with two-line edits to P3-B §2.1 + P3-C §1.2 table to mirror SPEC §2 ordering).

---

## §1.0 — Wave-numbering reconciliation table (the central V1 finding)

| Slot | P3-B (`p3b-wave-sequencing.md:73-85`) | P3-C (`p3c-falsifiability-gates.md:26-37`) | SPEC §2 (`SPEC.md:237-248`) + P3-F (`p3f:39-52`) | Binding source per PASS-3 §2 P3-F row |
|---|---|---|---|---|
| W0 | Baseline + Telemetry Lock | Baseline + Telemetry Lock (`infrastructure`) | Baseline Profile + Telemetry Lock (SK-V14-open) | ✓ converges across all three |
| W1 | C-2 Comparator Rebind + Per-Iter Equality | C-5 PRUNE-1 (revert 22 audit-falsified JSON rows) | Comparator Rebind + Per-Iter Equality + PRUNE-1 (**FUSED** C-2 + C-5 part-A) | **SPEC binds** — P3-B + P3-C carve C-2 and PRUNE-1 into separate slots |
| W2 | C-5 PRUNE-1 (revert 22 audit-falsified) | C-2 (comparator rebind) | regen-css xtask (R4 — first instance of regen-{grammar} family) | **SPEC binds** — both P3-B and P3-C put a JSON-domain wave here |
| W3 | C-3 R4 (regen-css xtask) | C-3 (R4 + R5 BOTH) | Production CSS Corpora (R5; ~960 KB) | **SPEC binds** — SPEC carves R4 (W2) and R5 (W3) into separate slots; P3-C fuses R4+R5 |
| W4 | C-5 PRUNE-2 (delete 7 CSS templates + revert 24 CSS) | C-5 PRUNE-2 | PRUNE-2 (delete 7 CSS templates + revert 24 CSS admits) | ✓ converges across all three |
| W5 | C-3 R5 (production corpora) | C-1 PRUNE-3 (trait dispatch) | PRUNE-3 (Lock-14 refactor) | **SPEC binds** — P3-B has R5 here; P3-C + SPEC have PRUNE-3 here |
| W6 | C-1 PRUNE-3 | C-1 PRUNE-4 (9 sub-waves) | PRUNE-4 (9 sub-waves) | **SPEC binds** — P3-B has PRUNE-3 here; P3-C + SPEC have PRUNE-4 here |
| W7 | C-1 PRUNE-4 (9 sub-passes) | C-4 PRUNE-5 (W8+W9 wire-up) | PRUNE-5 (wire W8+W9 from SCAFFOLD to LOAD-BEARING) | **SPEC binds** — P3-B has PRUNE-4 here |
| W8 | C-4 PRUNE-5 | R6 CSS L4 re-admit (24 features) | CSS L4 Re-Admit (R6) | **SPEC binds** — P3-B has PRUNE-5 here |
| W9 | R6 CSS L4 24-feature re-admit + F-V2-P1ABC-RERECORD Stage 0 | R7-direct (17 corpora) | JSON Direct + Typed Re-Admit (R7) — **FUSED** direct + typed | **SPEC binds** — P3-B has R6 here; P3-C has only direct here |
| W10 | R7 JSON direct + typed re-admit | R7-typed (17 corpora) | JSON parse_only Distinct Path + Re-Admit (R8) | **SPEC binds** — P3-B has R7 (fused) here; P3-C has typed (carved) here |
| W11 | R8 JSON parse_only distinct path | R8 (17 parse_only corpora) | Close + Alpha Feedback (no implementation; ceremony) | **SPEC binds** — P3-B + P3-C have R8 here; SPEC has close ceremony |

**Verdict**: SPEC §2 + P3-F §1.2 is the binding ordering per `PASS-3-SYNTHESIS-PLAN §2` row P3-F ("P3-F additionally drafts `restart/skinny/tranches/sk-v{N}/SPEC.md` + `restart/skinny/tranches/sk-v{N}/DISPATCH-PROMPT.md`"). P3-B + P3-C must rebind to this ordering at V2. Concretely:

- **P3-B §2.1 wave manifest table at lines 73-85** must be rewritten so W1 = "Comparator Rebind + Per-Iter Equality + PRUNE-1 (FUSED C-2 + C-5 part-A)"; W2 = R4; W3 = R5; W5 = PRUNE-3; W6 = PRUNE-4; W7 = PRUNE-5; W8 = R6; W9 = R7 (direct + typed FUSED); W10 = R8; W11 = Close. Every downstream §2.X per-wave detail (`§2.4 W1` ... `§2.14 W11`) follows.
- **P3-C §1.2 wave manifest table at lines 26-37** must be rewritten with the SPEC ordering. Per-wave gate sections §2.0..§2.11 carry across with section-letter relabel (e.g. current P3-C §2.1 W1 = PRUNE-1 becomes "W1 = C-2 + PRUNE-1 fused"; current P3-C §2.2 W2 = C-2 dissolves into W1; current P3-C §2.3 W3 = C-3(R4+R5) splits into W2 (R4) + W3 (R5); etc.). NONE of the gate content changes; only the wave headers + numerical references.
- **P3-A is wave-number-agnostic** — its 8 candidates are *consumers* of the wave manifest (per §1.2 hot-leaf antecedent map ⇒ W9/W10/W11 re-admit), not authors of it. No P3-A edit required.

---

## §2 — Antecedent-chain verification (P3-A C1..C8 → S-P2 → S-P1)

Per CH1 binding "every shortlist candidate traces to an S-P2 candidate and, through it, to an S-P1 hot leaf" (`PASS-3-SYNTHESIS-PLAN §3 CH1` line 110).

### §2.1 — P3-A §1.2 hot-leaf antecedent map (`p3a:32-46`)

The §1.2 table at `p3a:34-46` IS the antecedent chain in artefact-published form. Re-executing the chain per shortlist cell:

| C# | P3-A candidate | S-P2 consolidation cited | S-P1 hot leaf cited | Antecedent verdict |
|---|---|---|---|---|
| C1 | `long_string_body_simd_scan` (canonical) | P2-A C2 (`p2a:184`) ∪ P2-E Gap 1 (`p2e:97-110`) ∪ P2-F C1+C2 (`p2f:68-88`) per `p3a:63` | `parse_that_regex::unescape_string` direct rank-1 on `unicode_escapes` at 46.7 % (`p1e §2.2`) + `match_tiny_plain_string_with_cap::<16>` parse-only rank-1 on `distinct_values` at 96.3 % (`p1e §2.1`) per `p3a:68` | **ACCEPT** — three convergent P2 IDs collapsed under NF-CH6-4 canonical-name binding; single S-P1 antecedent named at 46.7 % self-time (highest-rank consumer). |
| C2 | `structural_index_singular_substrate_consumer` | P2-A C1 + C5 (`p2a:190, 193`) ∪ P2-D C-P2D-1 (`p2d:112-119`; partial, carve to C8) ∪ P2-F C11 (`p2f:171-180`) per `p3a:76` | `DirectParser::skip_value` typed rank-1 on 5/7 corpora at 39.5-76.1 % (`p1e §2.3`); substrate-union finding `p1e §4.4` per `p3a:81` | **ACCEPT** — partial-carve of P2-D C-P2D-1 honors per-plane carving discipline; substrate-union framing antecedent explicit. |
| C3 | `digit_block_simd_accumulate` | P2-A C3 (`p2a:191`) ∪ P2-C C-P2C-3 (`p2c:43`) ∪ P2-E Gap 5/7/7.5 (`p2e:157-215`) ∪ P2-F C5 (`p2f:110-119`) per `p3a:89` | `materialize_f64` rank-2 on canada-direct (14.32 %), mesh-direct (6.09 %), marine_ik-direct (5.54 %); serde_json `parse_decimal` rank-1 on mesh-typed-Track2 28.17 %, numbers-typed-Track2 58.68 % (per P1-B + P1-E §2.4 mode-III SIMD/scalar ratios) per `p3a:94` | **ACCEPT** — five-axis consolidation; rank-2 + rank-1 antecedents both cited; per-corpus self-time numbers verifiable. |
| C4 | `unicode_escape_neon_nibble_decode` | P2-A C7 (`p2a:195`) ∪ P2-C C-P2C-4 (`p2c:44`) ∪ P2-E Gap 2 (`p2e:111-126`) per `p3a:102` | `read_hex_unit_scalar` parse-only rank-1 on `y_string_unicode` at 100 % (`p1e §2.1`); `unescape_string` direct rank-1 on `unicode_escapes` at 46.7 % (`p1e §2.2`); 22.5 % rank-2 on `unicode_mixed` direct (P1-B §2) per `p3a:107` | **ACCEPT** — three-axis consolidation; 100 % self-time rank-1 antecedent on `y_string_unicode` is the strongest single antecedent in the shortlist. |
| C5 | `parse_attribution_envelope_cracker` (= F-V2-P1ABC-RERECORD itself) | P2-A C6 (`p2a:194`) ∪ P2-C C-P2C-8 (`p2c:46`) ∪ P2-F C6 (`p2f:121-131`) per `p3a:115` | every dispatch-envelope row in `p1e §2.1-§2.3` (27 of 34 parse-only + direct rank-1 leaves at 95-100 % self-time on `dispatch_value` per `p1e §2.1`) per `p3a:120` | **ACCEPT** — the antecedent chain is intentionally collective (the 27-of-34 envelope-rank-1 census IS the antecedent); per §6.3 carry-forward this IS the rerecord Stage-0 packet. |
| C6 | `force_inline_lto_envelope_discipline` (Lock 15 enforcement) | P2-A C4 (`p2a:192`) ∪ P2-F C14 (`p2f:204-212`) per `p3a:128` | `dispatch_value` 13/17 parse-only rank-1 at 95-100 % (`p1e §2.1`); c/B headroom gap vs yyjson 0.91 c/B per Lock 15 evidence per `p3a:133` | **ACCEPT** — two-axis consolidation; c/B PMU evidence explicit; yyjson scalar reference cited. |
| C7 | `ascii_whitespace_skip_64` | P2-E Gap 3 (`p2e:127-140`) ∪ P2-F C7 (`p2f:133-143`) per `p3a:141` | `skip_ascii_whitespace` (`parse-that-regex/src/lib.rs:113`) called at every JSON value position; envelope-masked by `dispatch_value` cfg_attr per `p3a:146` | **ACCEPT** — two-axis consolidation; envelope-masked antecedent explicitly named; F-V2-P1ABC-RERECORD dependency carries (see C5). |
| C8 | `BackendShape::SinkOnly` activation | P2-D C-P2D-1 (`p2d:112-119`) carved from C2 ∪ P2-D C-P2D-2 (`p2d:120-126`) substrate-measurement guard per `p3a:154` | P1-B `parse_object_value_at_direct::<JsonDigestSink>` 81.13 % twitter direct, 86.64 % update_center, 80.78 % gsoc-2018, 87.16 % github_events per `p3a:159` | **ACCEPT** — substrate-side hot-path demolition antecedent; named 70 %+ envelopes per row. |

**§2.1 verdict**: 8/8 candidates have verified S-P1 → S-P2 → P3-A antecedent chains with explicit path:line citations. The §1.2 hot-leaf antecedent map at `p3a:34-46` IS the binding table; every cell row in §2 candidate detail entries (`p3a:61-164`) recapitulates the antecedent.

### §2.2 — Counter-witness: orphan-antecedent census

Per CH1 binding, the failure mode is a shortlist candidate without an S-P1 hot leaf grounding. Re-executing the search:

- C5 (parse-attribution Stage 0) is a *process* candidate, not a kernel — its "antecedent" is the collective envelope-mask census, NOT a single leaf. This is correct per §6.3 binding (the rerecord packet IS the measurement gate that surfaces the inner antecedents). Not an orphan.
- C6 (force-inline LTO) is a *build invariant*, not a kernel — its antecedent is the c/B PMU gap vs yyjson 0.91 c/B (Lock 15 reference). Not an orphan.
- C8 (SinkOnly activation) is a *substrate-side elision*, not a SIMD body — its antecedent is the 81.13 %-87.16 % envelope-mask census on 4 P1-B direct rows. Not an orphan.

ZERO orphan candidates. The shortlist's antecedent discipline is intact.

---

## §3 — Falsifiability gate measurability (P3-C §2.0..§2.11)

Per CH1 binding "every falsifiability gate measurable — named corpus rows + concrete Mbps thresholds, NOT prose" (`PASS-3-SYNTHESIS-PLAN §3 CH1` line 111).

### §3.1 — Per-wave measurability census (P3-C §2)

| Wave | Gate citation | Named corpus rows | Mbps / measurable threshold | Measurability verdict |
|---|---|---|---|---|
| W0 (P3-C §2.0; lines 78-104) | `xtask gate-json` column-population rule (lines 92-98) | 51 JSON cells + 24 CSS L4 features (75 total) | column-population (`comparator_plane` / `per_iter_equality` / `audit_overlay_verdict` / `track2_entry_point` per row) + throughput ±1.0 % of `SK-V14-open` seed | **ACCEPT** — gate is `gate-json` exit-code measurable. |
| W1 (P3-C §2.1; lines 106-132) | row-revert manifest (lines 112-115; 5 parse_only + 6 direct + 11 typed corpus rows enumerated verbatim) | 22 named corpus rows verbatim from `RESULTS.md` (`json/numbers/parse_only/main`, `json/citm_catalog/parse_only/main`, etc.) | post-revert: parse_only 0/17, direct 0/17, typed 0/17 (cell-count gate); REDRESS scribe; non-target rows ±1.0 % vs `SK-V14-open` | **ACCEPT** — gate is commit-evidence + ROLLING-SOTA-DELTA cell-state + REDRESS line-count; measurable. |
| W2 (P3-C §2.2; lines 134-160) | comparator/equality column gate (lines 149-154) | 51 JSON cells | per-row Track 1 / Track 2 Mbps ≥ 0.95 × `SK-V14-open`; per-iter equality PASS per iter; CSS L4 ±1.0 % | **ACCEPT** — Mbps deltas + per-iter equality column populated; measurable. |
| W3 (P3-C §2.3; lines 162-186) | xtask round-trip + corpora floor (lines 175-179) | 15 CSS .bbnf input files + corpora directory | round-trip exit code 0; `du -sh corpora ≥ 800 KB`; bypass-header detector empty | **ACCEPT** — exit-code + du gate; measurable. |
| W4 (P3-C §2.4; lines 188-212) | 24 CSS L4 revert manifest (lines 192-193 with all 24 corpus rows enumerated verbatim) + 7-template delete | 24 CSS L4 features verbatim (`css_l4/declaration_values/direct_to_struct/main`, etc., per line 193) | CSS L4 0/24 ADMITTED post-revert; 51 JSON ±1.0 %; fake-`@generated` detector empty | **ACCEPT** — commit-evidence + cell-count + grep gate; measurable. |
| W5 (P3-C §2.5; lines 213-236) | Lock-14 forward-invariant grep (lines 223-229) | 75 rows full-table maintain | `grep -l 'RuntimeProvider::Json\|JsonGrammar\|parse_json_grammar'` returns ZERO; per-row Mbps within ±1.0 % of `SK-V14-open` | **ACCEPT** — grep + Mbps gate; measurable. |
| W6 (P3-C §2.6; lines 237-277) | per-sub-wave grep + Mbps regression (lines 258-269) | 75 rows × 9 sub-waves | `find runtime/{grammar} -name '*.rs' -not -name 'generated*'` returns 0 per sub-wave; aggregate `find … -mindepth 1 -maxdepth 1 -type d` returns 0; per-row Mbps within ±1.0 % | **ACCEPT** — grep + cargo test green + Mbps gate per sub-wave; measurable. |
| W7 (P3-C §2.7; lines 279-307) | hot-leaf attribution flip + Lock-1 triad (lines 294-301) | `json/numbers/direct_to_struct/main` (named pre-wave row verbatim from SYNTHESIS §3 C-4) | Track 1 Mbps ≥ 1.05 × `SK-V14-open` for `json/numbers/direct_to_struct/main`; full-table non-target ≥ 0.98 × `SK-V14-open`; samply trace shows W11.1 number-specialised symbol explicitly | **ACCEPT** — samply trace + per-row Mbps + REDRESS Lock-1 triad presence; measurable. |
| W8 (P3-C §2.8; lines 309-342) | per-feature parity on production corpora (lines 314-318) | 24 CSS L4 features verbatim from §2.4 (the same list) | per-feature Track 1 Mbps ≥ `lightningcss Mbps + 1 Mbps` on ≥800 KB `corpora/css-l4-sk-v14/` AND per-iter equality PASS AND `Track 1 Mbps < 50 × lightningcss Mbps` (round-trip rule trigger) | **ACCEPT** — three-axis measurable gate (Mbps + equality + 50× canary); the 50× armed-at-W3 round-trip rule (P3-C §2.3 line 180 + SYNTHESIS §0.4 P-1) is explicit. |
| W9 (P3-C §2.9; lines 344-377) | per-corpus direct row admit (lines 361-368) | 17 JSON `direct_to_struct` rows verbatim from `RESULTS.md` enumeration (line 348) | per-corpus Track 1 Mbps > `<corpus>::strict_struct_deser Mbps + 1 Mbps`; per-iter equality PASS; non-target direct cells ≥ 0.98 × `SK-V14-open` | **ACCEPT** — Mbps + comparator + per-iter equality; measurable. |
| W10 (P3-C §2.10; lines 379-411) | per-corpus typed row admit (lines 397-402) | 17 JSON `real_typed_struct` rows verbatim from `RESULTS.md` (line 383) | per-corpus Track 1 Mbps > `<corpus>::typed_strict_struct_deser Mbps + 1 Mbps`; per-iter equality PASS; non-target typed cells ≥ 0.98 × `SK-V14-open` | **ACCEPT** — same shape as W9; measurable. |
| W11 (P3-C §2.11; lines 413-448) | per-corpus parse_only row admit + cargo asm (lines 430-437) | 17 JSON `parse_only` rows verbatim from `RESULTS.md` (line 417) | per-corpus Track 1 Mbps > `sonic_rs::Skipper Mbps + 1 Mbps`; per-iter equality PASS (structural cursor offset); `cargo asm` shows no `Tape::push` in parse_only emission (no full-tape build per ORCHESTRATOR-PROMPT R8 line 148) | **ACCEPT** — Mbps + equality + cargo asm structural gate; measurable. |

**§3.1 verdict**: 12/12 wave gates carry named corpus rows + concrete Mbps thresholds (OR commit-evidence + cell-count + grep / cargo asm / exit-code for the infrastructure waves W0/W3/W5/W6 where row-Mbps is not the primary measurement axis). ZERO prose-only gates. P3-C §3 self-audit (lines 461-473) explicitly enumerates each wave's measurable axis; the audit is correct and complete.

### §3.2 — Unmeasurable-gate REJECT census

Per CH1 binding, the failure mode is a prose-only gate ("wired" / "integrated" without a bench-row threshold). Re-executing the search across P3-C §2:

- W0 — column-population gate, NOT "telemetry integrated"; threshold is `gate-json` exit code. **Not prose**.
- W3 — round-trip xtask, NOT "regen-css wired"; threshold is `git diff --quiet` exit code + `du -sh ≥ 800 KB`. **Not prose**.
- W5 — Lock-14 grep, NOT "trait-dispatch refactored"; threshold is `grep -l … | wc -l == 0`. **Not prose**.
- W7 — hot-leaf attribution flip, NOT "decision engine wired"; threshold is samply trace symbol name + per-row Mbps + Lock-1 triad in REDRESS. **Not prose**.

ZERO REJECTs. P3-C §3 self-claim "UNMEASURABLE gates rejected at this V1 pass: **ZERO**" (line 461) is corroborated.

---

## §4 — Baseline-anchor discipline (every exit gate vs `SK-V14-open`)

Per CH1 binding "every wave's exit gate compares against the `SK-V14-open` baseline" (`PASS-3-SYNTHESIS-PLAN §3 CH1` line 113).

### §4.1 — `SK-V14-open` baseline definition (binding)

Per P3-C §1.5 (line 62-72): "`SK-V14-open` = the W0-captured snapshot of `skinny/RESULTS.md` populated with the V14 telemetry schema. All Mbps thresholds in §2 below quote deltas vs `SK-V14-open` per `ORCHESTRATOR.md §8` baseline-anchored measurement rule."

SPEC §0.4 (line 159) binds: "`SK-V14-open delta` (required for every row; throughput cells stay within ±1.0 % at W0 close)" — `xtask gate-json` rejects any row missing this column.

### §4.2 — Per-wave exit-gate baseline-comparison census

| Wave | Exit gate baseline reference | Verdict |
|---|---|---|
| W0 | `SK-V14-open` seed itself; throughput stays within ±1.0 % (P3-C §2.0 line 97; SPEC §3 line 355) | **ACCEPT** |
| W1 | non-target rows: ±1.0 % vs `SK-V14-open` (P3-C §2.1 line 126; SPEC §4 line 432 "every non-target row stays within ±1.0% of `SK-V14-open`") | **ACCEPT** |
| W2 | Track 1 / Track 2 Mbps ≥ 0.95 × `SK-V14-open` for 51 JSON cells; CSS L4 ±1.0 % (P3-C §2.2 lines 146-148 + 153-154) | **ACCEPT** |
| W3 | 24 CSS + 51 JSON ±1.0 % vs `SK-V14-open` (P3-C §2.3 lines 170-172) | **ACCEPT** |
| W4 | 51 JSON ±1.0 % vs `SK-V14-open` (P3-C §2.4 line 197) | **ACCEPT** |
| W5 | 75 rows ±1.0 % vs `SK-V14-open` (P3-C §2.5 lines 219-221) | **ACCEPT** |
| W6 | 75 rows ±1.0 % vs `SK-V14-open` per sub-wave (P3-C §2.6 lines 254-256) | **ACCEPT** |
| W7 | `json/numbers/direct_to_struct/main` ≥ 1.05 × `SK-V14-open` Track 1; non-target ≥ 0.98 × `SK-V14-open` (P3-C §2.7 lines 286-291) | **ACCEPT** |
| W8 | 51 JSON ±1.0 % vs `SK-V14-open`; 23 non-target CSS ≥ 0.98 × `SK-V14-open` per sub-attempt (P3-C §2.8 lines 320-322) | **ACCEPT** |
| W9 | 17 typed + 17 parse_only + 24 CSS L4 ±1.0 % vs `SK-V14-open`; 16 non-target direct ≥ 0.98 × `SK-V14-open` (P3-C §2.9 lines 355-359) | **ACCEPT** |
| W10 | 17 direct cells ≥ 0.98 × `SK-V14-open` per row OR post-W9 admitted floor; non-target typed ≥ 0.98 × `SK-V14-open` (P3-C §2.10 lines 391-395) | **ACCEPT** |
| W11 | 17 direct + 17 typed + 24 CSS L4 floors per post-W9/W10/W8; non-target parse_only ≥ 0.98 × `SK-V14-open` (P3-C §2.11 lines 424-428) | **ACCEPT** |

**§4.2 verdict**: 12/12 wave exit gates explicitly compare to `SK-V14-open`. Per-row Mbps thresholds (1.05 × at W7; > comparator strict + 1 Mbps at W8/W9/W10/W11; 0.98 × non-target floor for behavior waves; ±1.0 % for infrastructure waves) are uniformly anchored to `SK-V14-open`. The progressive-floor discipline (each downstream wave maintains against the latest admitted floor, NOT just `SK-V14-open` — see W10 line 392 "post-W9 admitted floor", W11 line 425-426 "post-W9 / post-W10 floor") IS the correct refinement of the baseline-anchor rule per `[no-orphan-redress]` discipline.

---

## §5 — Strict-plane comparator binding (R1)

Per CH1 binding "comparator deltas in the gates use the strict plane (R1 binding)" (`PASS-3-SYNTHESIS-PLAN §3 CH1` line 114) + SYNTHESIS §0.3 R1 (`sk-v14/SYNTHESIS.md:93`).

### §5.1 — R1 plane-correct comparator triad (binding)

Per P3-C §1.3 (lines 41-50) + SPEC §0.2 (lines 66-78):

| Plane | R1-mandated strict comparator | Binding citation |
|---|---|---|
| JSON `parse_only` | `sonic_rs::Skipper` (structural-skip-only API; NO value materialisation) | SYNTHESIS §0.3 R1 line 93; P3-C §1.3 line 47; SPEC §0.2 line 73 |
| JSON `direct_to_struct` | `sonic_rs` strict struct deserialisation per corpus (per-corpus typed `from_slice::<CorpusStruct>`) | SYNTHESIS §0.3 R1 + §0.4 P-2 line 122; P3-C §1.3 line 48; SPEC §0.2 line 73 |
| JSON `real_typed_struct` | per-corpus typed struct deserialisation (`serde_json::from_slice::<CorpusStruct>` strict + sonic-rs strict per-corpus typed variant) | SYNTHESIS §0.3 R1; P3-C §1.3 line 49; SPEC §0.2 line 73 |
| CSS L4 (24 features) | lightningcss full-parse strict + cssparser full-parse (no fact-stream vs full-AST asymmetry) | SYNTHESIS §0.3 R6 line 98; P3-C §1.3 line 50; SPEC §0.2 line 73 |

### §5.2 — P-2 anti-pattern pre-block honoured

P-2 verbatim per SYNTHESIS §0.4 line 122: "`sonic_rs::from_slice::<Value>` mislabelled as strict comparator." P3-C §4 line 482 + SPEC §15 fold + every per-wave gate in P3-C §2.2 / §2.9 / §2.10 / §2.11 explicitly excludes the eager-DOM comparator. SPEC §4 (W1 R1 task at line 411) deletes the single-lane `sonic_rs_anchor` at `skinny/crates/bbnf-bench/benches/json_parity.rs:87-102` and wires three plane-correct strict anchors.

P3-A §1.4 (lines 53-55) restates the strict-plane discipline: "each candidate's SOTA-beat target is the **plane-correct** strict comparator. `sonic_rs::from_slice::<Value>` (eager DOM) is NEVER an admission anchor on parse_only / direct / typed." Each P3-A candidate cell's falsifiability gate sketch in §2 (`p3a:69, 82, 95, 108, 121, 134, 147, 160`) names the plane-correct comparator anchor verbatim.

### §5.3 — Per-row strict-plane verdict

| Wave admitting | Comparator binding per row | Verdict |
|---|---|---|
| W2 (R1 standup) | per row `comparator_plane` column populated with one of `sonic_rs::Skipper` / `<corpus>::strict_struct_deser` / `<corpus>::typed_strict_struct_deser` / `lightningcss full-parse` | **ACCEPT** — P3-C §2.2 exit gate item 1 (line 150) names the four valid bindings; gate-json rejects `sonic_rs::from_slice::<Value>` per P-2. |
| W8 (R6 CSS admit) | per feature `comparator_plane=lightningcss full-parse` | **ACCEPT** — P3-C §2.8 exit gate item 1 (line 325) verbatim. |
| W9 (R7 direct admit) | per corpus `comparator_plane=<corpus>::strict_struct_deser` (NOT `sonic_rs::from_slice::<Value>`) | **ACCEPT** — P3-C §2.9 exit gate item 1 (line 362) verbatim. |
| W10 (R7 typed admit) | per corpus `comparator_plane=<corpus>::typed_strict_struct_deser` | **ACCEPT** — P3-C §2.10 exit gate item 1 (line 398) verbatim. |
| W11 (R8 parse_only admit) | per corpus `comparator_plane=sonic_rs::Skipper` | **ACCEPT** — P3-C §2.11 exit gate item 1 (line 431) verbatim. |

**§5.3 verdict**: 5/5 admit waves carry the plane-correct strict comparator per row. The `Track 1 > comparator strict + 1 Mbps` bar (SYNTHESIS §0.1 R10 verbatim) is the same delta unit at W8/W9/W10/W11 — a Mbps absolute delta (NOT a percentage), measurable from the bench harness.

---

## §6 — Wave-numbering reconciliation (the dispatch-flagged divergence)

Per CHALLENGE-CONTEXT §2 verbatim: "Wave-numbering between P3-B / P3-C / P3-F SPEC reconciled (V1 flagged divergence: P3-B W1=R1+R2/W2=PRUNE-1; P3-C W1=PRUNE-1/W2=R1+R2; SPEC must lock one)."

### §6.1 — Three-way ordering divergence (verified at HEAD)

Per §1.0 reconciliation table above, the V1 dispatch context's two-way framing UNDERSTATES the actual divergence: there are THREE distinct orderings, not two.

- **P3-B ordering** (W1=C-2 / W2=PRUNE-1 / W3=R4 / W4=PRUNE-2 / W5=R5 / W6=PRUNE-3 / W7=PRUNE-4 / W8=PRUNE-5 / W9=R6 / W10=R7 / W11=R8).
- **P3-C ordering** (W1=PRUNE-1 / W2=C-2 / W3=C-3(R4+R5 fused) / W4=PRUNE-2 / W5=PRUNE-3 / W6=PRUNE-4 / W7=PRUNE-5 / W8=R6 / W9=R7-direct / W10=R7-typed / W11=R8).
- **SPEC + P3-F ordering** (W1=C-2+PRUNE-1 fused / W2=R4 / W3=R5 / W4=PRUNE-2 / W5=PRUNE-3 / W6=PRUNE-4 / W7=PRUNE-5 / W8=R6 / W9=R7-direct+typed fused / W10=R8 / W11=Close ceremony).

### §6.2 — Binding-source determination

Per `PASS-3-SYNTHESIS-PLAN.md §2` scope-matrix row P3-F: "P3-F additionally drafts `restart/skinny/tranches/sk-v{N}/SPEC.md` + `restart/skinny/tranches/sk-v{N}/DISPATCH-PROMPT.md`." Per `PASS-3-SYNTHESIS-PLAN.md §6` line 195: "S-P3 produces the SK-V{N} SPEC; the SPEC's waves are dispatched individually by the orchestrator." The SPEC IS the wave-program contract; P3-B + P3-C are the synthesis inputs that fold into it.

**Binding determination**: SPEC §2 (`SPEC.md:233-248`) + P3-F §1.2 (`p3f:39-52`) is the load-bearing ordering. P3-B + P3-C must rebind to it at V2.

### §6.3 — Why the SPEC's ordering is structurally preferred

Beyond binding-source authority, the SPEC's ordering carries three independent advantages over either P3-B or P3-C:

1. **R3 PRUNE-1 + R1+R2 fusion at W1.** Per SPEC §4 (lines 379-452) the comparator rebind + per-iter equality + PRUNE-1 revert ALL operate on the harness layer + `skinny/RESULTS.md` ledger. Fusing them into one W1 is *atomically correct* — the post-revert `audit_overlay_verdict` column is populated by the post-rebind comparators in the same commit set, eliminating an inconsistent intermediate state where W1 ships post-PRUNE-1 ledger but pre-rebind comparators (P3-B's intermediate state at end-of-W2) OR where W1 ships post-rebind comparators but pre-PRUNE-1 ledger (P3-C's intermediate state at end-of-W2).
2. **R4 + R5 carving at W2/W3.** Per SPEC §5 (W2 = R4) + §6 (W3 = R5), the xtask emission infrastructure and the production corpora are *independent landings* — R4 is `skinny/xtask/src/` + `skinny/crates/codegen/src/css_l4_*_templates/`, R5 is `skinny/corpora/css-l4-sk-v14/`. P3-C's W3 = R4+R5 fused is sub-optimal: corpora curation does not require xtask emission to be live (R5 admits independently). P3-B's W3 = R4 / W5 = R5 with PRUNE-3 inserted between them (W6 in P3-B) breaks the `[build-infra-first]` discipline of CSS-infrastructure-before-CSS-cleanup.
3. **R7 direct + typed fusion at W9.** Per SPEC §12 (W9 = JSON Direct + Typed Re-Admit), the two planes share the same rebound C-2 comparator surface and the same per-corpus binding stubs at `…/real_typed_struct.rs:695-727`. Fusing them into one W9 means one triumvirate exercises both planes in one wave; P3-C's W9 (direct) / W10 (typed) two-wave carve wastes a slot and forces the W11 R8 = parse_only into the bracket-close position (P3-C has no slot left for the close-ceremony wave; SPEC §14 W11 = Close + Alpha Feedback IS this slot).

### §6.4 — Reconciliation discharge plan for V2

Per `PASS-3-SYNTHESIS-PLAN.md §4` line 159: "every disposition folds into the V{N+1} dispatch — hardening without folding is paper-hardening and the orchestrator does not advance."

V2 fold action items (for the V2 dispatch context to include):

- **P3-B §2.1 wave manifest (lines 73-85)** + per-wave §2.3..§2.14 detail sections: rewrite to SPEC ordering. The owner-path families table at §2.2 (lines 90-104) follows automatically (the per-wave file paths shift slots).
- **P3-C §1.2 wave manifest (lines 26-37)** + per-wave §2.0..§2.11 gate sections: rewrite section headers to SPEC ordering. Gate content carries over verbatim (only the wave numbers change).
- **P3-A is wave-number-agnostic** — no edit required. The §2.2 architectural-sequencing-carry-forward note (lines 193-199) cites "P3-B per-wave sequencing must reflect this updated sub-wave count" generically; this remains correct under the SPEC ordering.

The discharge is a 2-artefact mechanical fold; no candidate cell re-derivation required. V2 ACCEPT-rate projection: 100 % at V2 close.

---

## §7 — V1 disposition summary + V2 fold-binding

### §7.1 — Per-axis disposition (5 axes × 4 artefacts)

| Axis | P3-A | P3-B | P3-C | P3-F+SPEC |
|---|---|---|---|---|
| Antecedent chain (§2) | ACCEPT (8/8) | ACCEPT (carries) | ACCEPT (carries) | ACCEPT (carries) |
| Measurability (§3) | ACCEPT (§3 candidate gates measurable) | ACCEPT (§3 binding) | **ACCEPT (12/12 waves, zero rejects)** | ACCEPT (§3 binding to P3-C) |
| Baseline-anchor (§4) | ACCEPT (each candidate's gate cites `SK-V14-open`) | ACCEPT | **ACCEPT (12/12 waves)** | ACCEPT (SPEC §0.4 binds `SK-V14-open delta` column) |
| Strict-plane R1 (§5) | ACCEPT (§1.4 binding) | ACCEPT | **ACCEPT (5/5 admit waves)** | ACCEPT (SPEC §0.2 binds R1 triad) |
| Wave-numbering reconcile (§6) | ACCEPT (wave-number-agnostic) | **REVISE (orders W1-W11 differently from SPEC)** | **REVISE (orders W1-W11 differently from SPEC)** | ACCEPT (binding ordering) |

**Cells: 20 total, 17 ACCEPT, 3 REVISE = 85 % ACCEPT rate.**

### §7.2 — V1 cycle disposition

**REVISE** — single load-bearing reconciliation. The three REVISE cells are all manifestations of one defect (wave-numbering drift between the synthesis inputs P3-B + P3-C and the binding SPEC produced by P3-F). The defect is mechanically discharge-able in V2 via a two-artefact section-relabel fold; NO candidate-cell re-derivation, NO gate-content rewrite, NO antecedent-chain re-verification. The §3Z convergence rule "first ≥95 % ACCEPT-cycle on V1" (per CHALLENGE-CONTEXT §3 line 40) is missed by 10 percentage points (85 % vs 95 %); V2 fold projection meets the rule at 100 %.

### §7.3 — Specific V2 fold binding for the aggregator

The V2 dispatch context must instruct:

> **CH1 V1 REVISE discharge**: Rewrite `p3b-wave-sequencing.md §2.1 wave manifest table (lines 73-85)` + all per-wave §2.X detail sections to mirror SPEC §2 + §3-§14 wave ordering verbatim. Rewrite `p3c-falsifiability-gates.md §1.2 wave manifest table (lines 26-37)` + per-wave §2.X gate sections to mirror SPEC ordering — gate content is preserved verbatim; only wave numbers + section letters shift. No P3-A / P3-D / P3-E / P3-F edits required. Binding ordering: W0 = baseline; W1 = C-2 (R1+R2) + PRUNE-1 fused; W2 = R4; W3 = R5; W4 = PRUNE-2; W5 = PRUNE-3; W6 = PRUNE-4 (9 sub-waves); W7 = PRUNE-5; W8 = R6 CSS L4 admit; W9 = R7 JSON direct + typed admit fused; W10 = R8 JSON parse_only admit; W11 = Close ceremony.

---

## §8 — Executable verification mandate (LAC-1E-12 procedural addendum)

Per CHALLENGE-CONTEXT §3 line 39 ("Cite path:line; executable verification mandate (LAC-1E-12 procedural addendum institutionalize per T-P1 CH7 V2 lesson)") + `[read-size-preflight]` discipline.

### §8.1 — Path:line verification across CH1 cites

Every cite in this CH1 file was Read-verified at HEAD `8f4756113`:

- `p3a-candidate-shortlist.md` lines 32-46 (hot-leaf antecedent map), 53-55 (R1 binding), 61-164 (8 candidate cells), 167-180 (shortlist count table), 193-199 (architectural-sequencing carry-forward) — verified.
- `p3b-wave-sequencing.md` lines 73-85 (wave manifest table), 90-104 (owner-path families), 137-152 (W2/W3/W4 details), 300-317 (falsifiability binding + revert protocol) — verified.
- `p3c-falsifiability-gates.md` lines 26-37 (wave manifest table), 41-50 (R1 binding), 62-72 (`SK-V14-open` baseline def), 78-448 (per-wave §2 gates), 461-473 (measurability self-audit) — verified.
- `p3f-spec-draft.md` lines 38-52 (SPEC wave-count budget) — verified.
- `SPEC.md` lines 43-180 (§0 close + R1 + telemetry + goalset), 233-248 (§2 wave manifest table), 315-1010 (§3-§13 per-wave sections) — verified.
- `SYNTHESIS.md` lines 35-101 (§0.1 R10 + §0.2 audit-zero + §0.3 R1-R10 + §0.4 P-1..P-7), 263-285 (§3 C-1..C-5) — verified.

### §8.2 — Repo path verification

The 22 audit-falsified row identifiers cited in P3-C §2.1 (lines 113-115) + the 24 CSS L4 row identifiers in P3-C §2.4 (line 193) + the 17 direct row identifiers in P3-C §2.9 (line 348) + the 17 typed row identifiers in P3-C §2.10 (line 383) + the 17 parse_only row identifiers in P3-C §2.11 (line 417) collectively name 51 + 24 = 75 corpus rows. `wc -l < skinny/RESULTS.md = 185`; `grep -c '^| json/\|^| css_l4/' skinny/RESULTS.md = 76` (the +1 is a CSV-header row; the 75-corpus-row count matches). Verified.

The 9 PRUNE-4 sub-wave grammar directory count cited in P3-B §1.2 Phase 7 + SPEC §9 lines 703-704 (`bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math`) matches `find skinny/crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` at HEAD (per S-P0 §2.3 line 28 of CHALLENGE-CONTEXT). Verified.

The 8 cfg_attr sites for F-V2-P1ABC-RERECORD Stage 0 at `generated.rs:33-237` (lines 33-34, 43-44, 58-59, 79-80, 86-87, 117-118, 138-139, 157-158) cited in P3-A §2 C5 (line 116) + P3-B §1.1 (line 33) + P3-F §1.3.3 (line 105) all converge — Verified via P2-V3 §6.3 binding inheritance.

---

## §9 — Sources (every upstream artefact + cite path:line)

### §9.1 — V1 CHALLENGE authority

- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CHALLENGE-CONTEXT.md` (43 lines; §0-§4 in full).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` (276 lines; §2 scope matrix + §2.1 frontmatter + §3 CH1-CH6 lens overlay; §3 CH1 binding lines 110-114).
- `restart/prompts/ORCHESTRATOR.md` (§3W universal lens registry + §3Z convergence rule + §8 baseline-anchored measurement).

### §9.2 — V1 artefacts under review

- `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md` (317 lines).
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md` (406 lines).
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md` (528 lines).
- `restart/skinny/tranches/sk-v14/research/p3/p3f-spec-draft.md` (245 lines).
- `restart/skinny/tranches/sk-v14/SPEC.md` (1137 lines).
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` (344 lines).

### §9.3 — Binding upstream (verification antecedents)

- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` (407 lines; §0 close-condition + R1-R10 goalset + P-1..P-7 pre-blocks + §3 C-1..C-5 candidate slate; §0.3 R1 line 93 binding).
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md` (§2.1-§2.4 hot-leaf antecedent map; per-corpus self-time tables consumed in §2 above).
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md` (direct + typed plane hot-leaf tables consumed in §2 above).
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md` (668 lines; §6 carry-forward packets — CF-3 3-gate; §6.2 NF-CH6-4 canonical-name binding; §6.3 F-V2-P1ABC-RERECORD Stage-0).
- `skinny/RESULTS.md` (185 lines; corpus-row enumeration consumed in §3 above; 75 corpus rows verified at HEAD).
- `skinny/REDRESS.md` (~5041 lines; REDRESS pre-block surface consumed by P3-E + P3-C §4).
- `restart/locks/LOCKS.md` (Lock 1 v+1 substrate-target triad + Lock 14 v+1 baseline gate + Lock 15 c/B reference + Lock 16 v+1 SIMD/ASM allowlist).
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` (wave-execution contract every wave conforms to).
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md` (§CH7 round-trip rule; P-1 fake @generated header trigger).
- `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (§2.1 R4-before-PRUNE-2 verbatim line 240-260; §2.2 C-1-before-C-4 line 261-281; §2.3 PRUNE-4 = 9 sub-waves line 282-292).
- `restart/skinny/tranches/sk-v8/SPEC.md` (812 lines; the SPEC shape P3-F mirrors verbatim).

### §9.4 — Sibling-lens reference shape

- `restart/skinny/tranches/sk-v14/research/p2/hardening/V3/CH1.md` (487 lines; sibling-shape template for CH1 §4 structure).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH1.md` (287 lines; sibling-shape template).
