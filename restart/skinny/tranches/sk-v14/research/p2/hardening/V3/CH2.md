# SK-V14 S-P2 V3 CH2: GENERALITY (Lock 14) — confirming cycle

Pass: S-P2 Research · Cycle: V3 (confirming) · Lens: CH2 GENERALITY.
Authority: `restart/prompts/skinny/PASS-2-RESEARCH.md §3 CH2`; `restart/prompts/ORCHESTRATOR.md §3W + §3Z + §8 non-negotiables`; `restart/locks/LOCKS.md:220-263` (Lock 14 + v+1 amendments).
Dispatch context: `restart/skinny/tranches/sk-v14/research/p2/hardening/V3/CHALLENGE-CONTEXT.md §2 CH2 row` ("confirm Lock 14 + grammar-neutrality posture unchanged; F-V2-CH2-1/2 cross-axis tracking + dual-gating still intact").
Carry-forward: V2 CH2 100% ACCEPT (6/6 artefacts; 39 active + 6 demoted/pre-blocked = 45 cross-axis entries) per `research/p2/hardening/V2/CH2.md`; V2 was the second consecutive ≥95% cycle for CH2 at the per-lens layer; V1 CH2 also 100% per `research/p2/hardening/V1/CH2.md`. CH2 V2 lens-level § 3Z (≥95% × 2) was already satisfied at V2; V3 is a verification-only confirming cycle that extends the chain to three.
Output: this file. WRITE-ONLY. Aggregator commits 8 hardening files atomically per CHALLENGE-CONTEXT §3. HARD CAP 25 min.

## §0 — Disposition summary (V3 confirming cycle)

| Artefact | Lines | V1 disp | V2 disp | V3 disp | Rationale |
|---|---:|---|---|---|---|
| `p2a-sota-teardown.md` | 367 | ACCEPT | ACCEPT (V1-LOCKED) | ACCEPT (V2-LOCKED; zero V3 drift) | `git diff 4c70b6f19..ebe84954b -- p2a-sota-teardown.md` returns empty. 7/7 SOTA candidates retain V1+V2 GENERALISES verdicts; CH2 F2 CSS L4 spec-only generalisation acknowledgment at p2a:203 intact. |
| `p2b-dav1d-process.md` | 217 | ACCEPT | ACCEPT (V2 amended) | ACCEPT (V2-LOCKED; zero V3 drift) | `git diff 4c70b6f19..ebe84954b -- p2b-dav1d-process.md` returns empty. 5-stage admission process A-E + §3 lines 156-160 per-stage Lock 14 verdicts unchanged from V2; FFmpeg/dav1d SHA pinning at §5.1 intact. |
| `p2c-arch-esoterica.md` | 164 | ACCEPT | ACCEPT (V2 amended Fold-2) | ACCEPT (V2-LOCKED; zero V3 drift) | `git diff 4c70b6f19..ebe84954b -- p2c-arch-esoterica.md` returns empty. 5 active + 3 demoted candidates retain §3 grammar-neutrality verdicts verbatim; C-P2C-5 hardcoded-JSON CH2-violation pointer (`classify_tbl4.rs:33-35`) preserved; C-P2C-1/6/7 §2.X demotion stamps with explicit re-promotion gates intact. |
| `p2d-substrate-tape.md` | 254 | ACCEPT | ACCEPT (V2 amended Fold-2) | ACCEPT (V2-LOCKED; zero V3 drift) | `git diff 4c70b6f19..ebe84954b -- p2d-substrate-tape.md` returns empty. 2 active + 1 demoted (C-P2D-3 in §1.6(d)) + 1 pre-blocked (C-P2D-4) carry §3 grammar-neutral YES verdicts at lines 148-151 verbatim. |
| `p2e-parse-that-gaps.md` | 342 | ACCEPT | ACCEPT (V1-LOCKED) | ACCEPT (V1+V2-LOCKED; zero V3 drift) | `git diff 4c70b6f19..ebe84954b -- p2e-parse-that-gaps.md` returns empty. All 9 gaps retain explicit §3 grammar-neutrality table at lines 235-247; §4.3 "Layer-1 primitives carry NO defaults" canonical Lock 14 v+1 enforcement intact. |
| `p2f-grammar-neutral.md` | 360 | ACCEPT (LOAD-BEARING) | ACCEPT (LOAD-BEARING; V2 amended) | ACCEPT (LOAD-BEARING; V3 amended verb-tense only) | V3 diff is two atomic verb-tense cells (§2.10 C10 at p2f:164 + §2.13 C13 at p2f:197): "Authoring landed as Fold-N V2 deliverable" → "Authoring queued for S-P3 same-wave Lock 16 same-commit admission per HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-N; function body lands same-commit with SIMD body at S-P3". Zero verdict change; zero grammar-neutrality content change; zero §3 / §2.Y / §4 surface change. §3 verdict tally at p2f:266-275 (13 NEUTRAL-WIRED+CONFIG-DRIVEN active; 0 PENDING; 0 REFRAMABLE; 0 IRREDUCIBLE; 1 demoted) intact; §3 consumer-existence summary at p2f:279-282 (13/13 CSS L4 + 13/13 Sheets + 13/13 BBNF-self) intact; §2.Y NF-CH6-4 cross-axis tracking at p2f:231-239 intact; §4 C6 risk row at p2f:300 (F-V2-P1ABC-RERECORD dual-gate inheritance) intact. |

**Per-§ ACCEPT-rate (all CH2-binding artefacts):** 6 / 6 = **100% ACCEPT** at V3. The dispatch-context-§2 CH2 V3 binding ("confirm Lock 14 + grammar-neutrality posture unchanged; F-V2-CH2-1/2 cross-axis tracking + dual-gating still intact") is satisfied with the same **39 active cross-axis candidates** (7 P2-A + 5 P2-B stages + 5 P2-C active + 2 P2-D active + 9 P2-E gaps + 13 P2-F active V2) + **6 demoted/pre-blocked entries** (3 P2-C demoted + 1 P2-D demoted + 1 P2-D pre-blocked + 1 P2-F demoted) carried forward from V2 unchanged.

**Aggregate V3 disposition:** **ACCEPT**. Zero new V3-cycle findings; F-V2-CH2-1 (NF-CH6-4 §2.Y consolidation strengthens CH2 by preventing three orthogonal SIMD-body admissions) and F-V2-CH2-2 (F-V2-P1ABC-RERECORD CH2/CH4 dual-gating propagated to all 12 consumer-dependency candidates) both confirmed intact via V3 path:line re-verification below. V1 R1/R2 + F1 + F2 carry-forwards preserved. V3 verb-tense fix at p2f:164 + p2f:197 has zero impact on the CH2 GENERALITY lens (the edited cells are Stage-A authoring discipline framing within §2.10 + §2.13 candidate prose; CH2 verdict on C10/C13 remains NEUTRAL-WIRED per §3 row p2f:256 + p2f:259 unchanged).

## §1 — V3 method (verification commands; verbatim, reproducible)

### §1.1 — V2-LOCKED axis drift audit (P2-A/B/C/D/E; mandatory per CHALLENGE-CONTEXT §2 confirming-cycle clause)

```bash
# V2 commit (S-P2 hardening V2 challenge + consolidated): 4c70b6f19
# V3 commit (S-P2 V3 atomic micro-fold + V3 dispatch context): ebe84954b
git diff 4c70b6f19..ebe84954b -- \
  restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md \
  restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md \
  restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md \
  restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md \
  restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md
# (empty output — zero V3 drift on V2-LOCKED axes)

git show --stat ebe84954b | head -15
#  .../research/p2/hardening/V3/CHALLENGE-CONTEXT.md  | 40 ++++++++++++++++++++++
#  .../sk-v14/research/p2/p2f-grammar-neutral.md      |  4 +--
#  2 files changed, 42 insertions(+), 2 deletions(-)
```

**Result:** V2-LOCKED axes P2-A + P2-B + P2-C + P2-D + P2-E carry zero V3 drift. The CHALLENGE-CONTEXT V3 §2 "Special V3 attention — confirming-cycle discipline" clause (zero drift via `git diff 4c70b6f19..<V3 SHA>` returns empty for V2-LOCKED axes) is satisfied. The 7 P2-A SOTA candidates + 5 P2-B stages + 8 P2-C entries (5 active + 3 demoted) + 4 P2-D entries (2 active + 1 demoted + 1 pre-blocked) + 9 P2-E gaps retain V2 ACCEPT verdicts unchanged at V3 HEAD.

### §1.2 — P2-F V3 edit localisation audit (the only V3-amended axis)

```bash
git diff 4c70b6f19..ebe84954b -- restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md
# diff localised to:
#   p2f:164 (§2.10 C10 cross-chunk byte-context, Scalar-ref status cell)
#   p2f:197 (§2.13 C13 branchless 3-way XOR (BCAX), Scalar-ref status cell)
# both edits identical pattern: "Authoring landed as Fold-N V2 deliverable" → "Authoring queued for S-P3 same-wave Lock 16 same-commit admission per HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-N; function body lands same-commit with SIMD body at S-P3"
```

**Result:** the V3 P2-F diff is exactly two verb-tense cells — both inside the Stage-A authoring discipline framing of §2.10 + §2.13 candidate prose, both correcting a "landed" past-tense over-claim into a "queued for S-P3 same-wave admission" forward-looking claim aligned with Lock 16 same-commit discipline. **Neither edit alters CH2 GENERALITY content.** The cells are inside the "Scalar-ref status" bullet (Stage-A authoring framing); the candidate-level grammar-neutrality verdicts at the §2.10 / §2.13 bullet **Arch** / **P1 antecedent** / **Source anchor** lines are unchanged. The candidate-level §3 verdict rows at p2f:256 (C10 NEUTRAL-WIRED) + p2f:259 (C13 NEUTRAL-WIRED) reference Lock 16 :285 (C10) + :289 (C13) abstract-primitive declarations and are independent of the Stage-A authoring temporal framing. **Zero CH2 impact.**

### §1.3 — Lock 14 v+1 admission gate verification (V3 HEAD-line citation; unchanged from V2)

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

The Lock 14 v+1 closing clause (line 259-260) is the operative CH2 V3 admission gate; unchanged at V3 HEAD. Per the V2 §1.5 cross-axis reconciliation table inherited unchanged (re-verified §1.4 below), all 39 active cross-axis candidates name CSS L4, Sheets, or BBNF-self consumers in their respective §3 grammar-neutrality tables; zero V3 active candidates carry a PENDING-CONSUMER flag (V1 C8 demoted to §2.X.1 at V2 and preserved at V3).

### §1.4 — F-V2-CH2-1 (§2.Y NF-CH6-4 cross-axis tracking) confirming-cycle verification

The §2.Y cross-axis tracking note at p2f:231-239 (introduced V2; carried forward at V3 without edits) binds three axes (P2-A C2 `long_string_body_simd_scan`, P2-E Gap 1 `scan_string_special_block_sweep_64`, P2-F C1+C2 quote-aware classifier composition) to ONE canonical primitive name + ONE canonical scalar reference function at S-P3 admission time. V3 verification:

```bash
sed -n '231,239p' restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md
# §2.Y — Cross-axis tracking note (NF-CH6-4 long-string-body SIMD scan consolidation)
# Per HARDENING-S-P2-V1-CONSOLIDATED §2.3 + CH6 §4.4 NF-CH6-4: three artefacts surface the
# same long-string-body SIMD scan primitive under three distinct names, all grounded on the
# `unescape_string` direct rank-1 46.7 % `unicode_escapes` hot-leaf (P1-E §2.2):
#   • P2-A C2 long_string_body_simd_scan — names match_tiny_plain_string_with_cap +
#     unescape_string scalar refs.
#   • P2-E Gap 1 scan_string_special_block_sweep_64 — names
#     scan_string_special_block_scalar.
#   • P2-F C1 + C2 — names scan_structurals_scalar + P2-E Gap 6 composition.
# All three carry CH6 PASS scalar references; all three converge on the same underlying
# primitive. S-P3 consolidator binding: one canonical primitive name + one canonical
# scalar-ref function rather than three orthogonal SIMD bodies for one primitive.
```

**Result:** the §2.Y NF-CH6-4 cross-axis tracking note is intact at V3 HEAD. The three-axis convergence (P2-A C2 ≡ P2-E Gap 1 ≡ P2-F C1+C2) is preserved verbatim; the "ONE canonical primitive name + ONE canonical scalar reference function" S-P3 consolidator binding is preserved verbatim; the cross-references at p2f:239 (P2-A C2 row + P2-E Gap 1 + Gap 6 + this file §2.1 C1 + §2.2 C2 + §2.Y) are reproducible at V3 HEAD. **F-V2-CH2-1 holds at V3 — confirming-cycle disposition: intact.**

### §1.5 — F-V2-CH2-2 (F-V2-P1ABC-RERECORD CH2/CH4 dual-gating propagation) confirming-cycle verification

The V2 §1.6 dual-gate propagation table (12 consumer-dependency candidates) carries forward unchanged at V3. V3 confirming-cycle re-verification of the most-load-bearing propagation sites:

```bash
# 1. P2-F §4 risk row at p2f:300 — C6 dispatch primitive (F-V2-P1ABC-RERECORD inherited carry-forward)
sed -n '300p' restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md
# **C6 dispatch primitive (cargo feature gating)** | Inherited V2 carry-forward
# F-V2-P1ABC-RERECORD per dispatch context §1 | … | S-P3 must ensure the wave that
# admits C6 carries the parse-attribution rerun in the same wave …

# 2. P2-F §2.X.1 C8 re-promotion gate at p2f:229 (F-V2-P1ABC-RERECORD as (a) re-promotion condition)
sed -n '229p' restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md
# Re-promotion gate (CH1 + CH4 + CH6 joint condition): C8 may re-enter the candidate
# enumeration in a future cycle iff (a) F-V2-P1ABC-RERECORD surfaces a JSON-side
# measurable antecedent (unlikely — JSON grammar has no comments), OR (b) a CSS L4 /
# BBNF-self / json-commented wave commits a same-wave consumer in the V_n wave plan
# with measurable parser-bytes evidence.

# 3. P2-F §2.10 C10 antecedent stamp at p2f:162 (Fold-3 antecedent stamp)
sed -n '162p' restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md
# P1 antecedent (CH1): indirect via C1 + C4 (the fusion primitive applied inside the
# other primitives' inner loops); direct evidence requires F-V2-P1ABC-RERECORD.

# 4. P2-F §2.13 C13 antecedent stamp at p2f:195
sed -n '195p' restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md
# P1 antecedent (CH1): indirect via C1 + C2 + C12 (fusion primitive applied inside their
# inner loops); direct evidence requires F-V2-P1ABC-RERECORD.

# 5. P2-C §2.X disposition stamp template — re-evaluation surface naming F-V2-P1ABC-RERECORD
grep -n "F-V2-P1ABC-RERECORD" restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md | head -5
# (re-evaluation surface inherited from V2 Fold-2)
```

**Result:** F-V2-P1ABC-RERECORD CH2/CH4 dual-gating preserved at all 12 V2-propagated consumer-dependency sites:

| Candidate | V2 dual-gate site | V3 status |
|---|---|---|
| P2-A C6 (envelope-cracker) | p2a:167-169 | held (V2-LOCKED zero drift per §1.1) |
| P2-C C-P2C-3 (UDOT) | p2c:43 + p2c:161 fold target | held (V2-LOCKED zero drift per §1.1) |
| P2-C C-P2C-8 (parse-attribution gate) | p2c:46 + p2c:164 fold target | held (V2-LOCKED zero drift per §1.1) |
| P2-E Gap 1 (envelope-masked string inner) | p2e:235-247 + §4.7 | held (V1+V2-LOCKED zero drift) |
| P2-E Gap 3 (envelope-masked whitespace) | p2e:239 + §4.7 | held (V1+V2-LOCKED zero drift) |
| P2-E Gap 4 (envelope-masked UTF-8) | p2e:240 + §4.7 | held (V1+V2-LOCKED zero drift) |
| P2-E Gap 5 (envelope-masked numeric) | p2e:241 + §4.7 + REDRESS-80 differential | held (V1+V2-LOCKED zero drift) |
| P2-F C6 (dispatch primitive) | p2f:123 Fold-3 + p2f:130 CH2 + p2f:300 §4 risk row | held (V3 verb-tense edit at p2f:164 does NOT affect this) |
| P2-F C7 (whitespace-skip indirect) | p2f:135 Fold-3 antecedent stamp | held |
| P2-F C10 (cross-chunk byte-context indirect) | p2f:162 Fold-3 antecedent stamp | held (V3 verb-tense edit at p2f:164 is inside the SAME §2.10 cell but on a different bullet — the **P1 antecedent** bullet at :162 carrying the F-V2-P1ABC-RERECORD reference is UNCHANGED) |
| P2-F C12 (keyword-set 16-byte indirect, CF-1 reframe) | p2f:184 Fold-3 antecedent stamp + CH4-ACCEPT reframe | held |
| P2-F C13 (BCAX 3-way XOR indirect) | p2f:195 Fold-3 antecedent stamp | held (V3 verb-tense edit at p2f:197 is inside the SAME §2.13 cell but on a different bullet — the **P1 antecedent** bullet at :195 carrying F-V2-P1ABC-RERECORD is UNCHANGED) |

**Critical verification:** the V3 verb-tense edits at p2f:164 (C10) and p2f:197 (C13) sit on the **Scalar-ref status** bullet of their respective §2.10 / §2.13 cells; the **P1 antecedent** bullets at p2f:162 (C10) and p2f:195 (C13) — the cells carrying the F-V2-P1ABC-RERECORD dual-gate language — are immediately above the edited cells and are completely unchanged. The dual-gate propagation is byte-for-byte preserved. **F-V2-CH2-2 holds at V3 — confirming-cycle disposition: intact across all 12 sites.**

### §1.6 — Cross-grammar consumer evidence reproduction at V3 HEAD (BBNF source files unchanged from V2)

```bash
grep -n "^number = \|^string = \|^identifier = \|^literal = \|^big_comment = \|^comment = \|^error_literal = " \
  grammar/css/l4/tokens.bbnf grammar/css/l4/value-unit.bbnf \
  grammar/google-sheets/google-sheets.bbnf grammar/bbnf/bbnf.bbnf
# Same output as V2 §1.4 — grammar files are unchanged at V3 HEAD.
```

**Result:** the cross-grammar consumer evidence base for the CH2 V3 generalisation argument reproduces at V3 HEAD identically to V2 HEAD. CSS L4 / Sheets / BBNF-self primitive shapes (identifier, string with N-quote disjunction, number with leading-dot policy, comment markers, doubled-quote escape) are spec-and-source-pinned per P2-F §1.2 grammar-source citations + P2-E §3 table. The CH2 F2 binding (CSS L4 spec evidence + JSON profile evidence jointly) carries forward unchanged across all six V3 P2 axis files.

### §1.7 — Cross-axis source-symbol reproduction at V3 HEAD

```bash
grep -n "fn scan_structurals\|fn dispatch_value\|fn match_tiny_plain_string\|fn parse_number_direct\|fn parse_object_value_at_direct\|fn parse_array_element_at_direct\|fn unescape_string\|fn read_hex_unit_scalar\|fn bulk_emit_positions_64_neon" \
  skinny/crates/runtime/src/grammars/json/generated.rs \
  skinny/crates/runtime/src/grammars/json/scan.rs \
  skinny/crates/parse-that-regex/src/lib.rs \
  skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs
# All symbols reproduce at V3 HEAD per V2 §1.3 (no source-tree changes affecting these
# files between V2 commit 447a26b07 and V3 commit ebe84954b — the V3 commit touched
# only research/p2/p2f-grammar-neutral.md + research/p2/hardening/V3/CHALLENGE-CONTEXT.md).
```

**Result:** every cited bbnf source symbol underlying a V3 active candidate primitive reproduces at HEAD identically to V2. The V3 axis files' cross-axis cites are source-reproducible; no V3 symbol-misidentification risk has been introduced.

## §2 — Per-artefact V3 findings

### §2.1 — P2-F (load-bearing CH2 arbiter V3): ACCEPT (V3 verb-tense amended; CH2 content intact)

P2-F V3 is the V3 CH2 arbiter. V3 §3 verdict tally at p2f:266-275 is identical to V2 (the §3 tally was not touched by the V3 verb-tense fix):

| Verdict | V1 count | V2 count | V3 count | Candidates (V3) |
|---|---:|---:|---:|---|
| NEUTRAL-WIRED | 5 | 5 | 5 | C9, C10, C12, C13, C14 |
| NEUTRAL-CONFIG-DRIVEN | 8 | 8 | 8 | C1, C2, C3, C4, C5, C6, C7, C11 |
| NEUTRAL-PENDING-CONSUMER | 1 (C8) | 0 | 0 | — |
| JSON-OVERFIT-REFRAMABLE | 0 | 0 | 0 | — |
| JSON-OVERFIT-IRREDUCIBLE | 0 | 0 | 0 | — |
| Demoted to non-candidate inventory | n/a | 1 | 1 | C8 (§2.X.1) |

**All 13 active V3 candidates clear the Lock 14 v+1 admission gate — same as V2.** The CSS L4 / Sheets / BBNF-self consumer-existence summary at p2f:279-282 retains 13/13 × 3 across the active candidate surface. The V3 verb-tense fix at p2f:164 (C10 Scalar-ref bullet) + p2f:197 (C13 Scalar-ref bullet) is exclusively inside the Stage-A authoring discipline framing — the **Scalar-ref status** bullet carries the same target-path (`crates/bbnf-simd/src/scalar/byte_context_64.rs` for C10; `crates/bbnf-simd/src/scalar/bcax_64.rs` for C13) and the same sibling-of-existing pointer; only the verb is corrected from "Authoring landed as Fold-N V2 deliverable" (past-tense; over-claim because the scalar-ref file body is not authored in S-P2 hardening — only the target named) to "Authoring queued for S-P3 same-wave Lock 16 same-commit admission per HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-N; function body lands same-commit with SIMD body at S-P3" (forward-looking; correctly aligned with Lock 16 same-commit discipline).

The §2.Y cross-axis tracking note at p2f:231-239 (V2 introduction) carries forward at V3 verbatim; the §3 note 1 at p2f:262-264 (Sheets doubled-quote escape — canonical Lock 14 v+1 enforcement target on `parse-that-regex::StringFlags::HAS_ESC`) carries forward verbatim; the §4 C6 risk row at p2f:300 (F-V2-P1ABC-RERECORD dual-gate inheritance) carries forward verbatim. **CH2 V3 disposition: ACCEPT.**

### §2.2 — P2-A (SOTA teardown, V2-LOCKED): ACCEPT

V2-LOCKED at V3. Zero V3 drift per §1.1 git-diff. V2 CH2 disposition holds verbatim: 7/7 SOTA-derived candidates carry per-candidate GENERALISES verdicts at p2a:119, 129, 139, 149, 159, 169, 179; §3 grammar-neutrality table at p2a:205-213 maps each candidate to per-grammar consumer columns; CH2 F2 binding correctly applied (CSS L4 spec-only generalisation at p2a:203 + p2a:217 closing paragraph). **ACCEPT.**

### §2.3 — P2-B (dav1d process, V2-LOCKED): ACCEPT

V2-LOCKED at V3. Zero V3 drift per §1.1 git-diff. V2 CH2 disposition holds verbatim: 5-stage admission process A-E at §3 lines 156-160 carries per-stage Lock 14 verdicts; FFmpeg `085714182302333dd83dcb9c36cf828dc4eba929` + dav1d `1718ff9aded99f0a89f5c7940d6afb8948301e33` SHAs at §5.1 intact; Stage B non-JSON fixture mandate + Stage D `grammar_scope` tag + Stage E manifest audit surface unchanged. **ACCEPT.**

### §2.4 — P2-C (arch esoterica, V2-LOCKED): ACCEPT

V2-LOCKED at V3. Zero V3 drift per §1.1 git-diff. V2 CH2 disposition holds verbatim: 5 active candidates (C-P2C-2, -3, -4, -5, -8) carry §3 grammar-neutrality verdicts at lines 76-84; 3 demoted candidates (C-P2C-1, -6, -7) retain §2.X disposition stamps + §3 row verdicts verbatim with explicit re-promotion gates naming F-V2-P1ABC-RERECORD as re-evaluation surface. C-P2C-5 hardcoded-JSON CH2-violation pointer (`bbnf-simd/src/aarch64/classify_tbl4.rs:33-35`) preserved verbatim. C-P2C-4 partial-generalisability flag Lock-14-honest. **ACCEPT.**

### §2.5 — P2-D (substrate + tape, V2-LOCKED): ACCEPT

V2-LOCKED at V3. Zero V3 drift per §1.1 git-diff. V2 CH2 disposition holds verbatim: 2 active candidates (C-P2D-1, C-P2D-2) carry §3 grammar-neutral YES at lines 148-149; C-P2D-3 demoted to §1.6(d) with explicit grammar-neutrality YES affirmation; C-P2D-4 N/A-REJECT-by-REDRESS-96/97/98 framing intact. Substrate-side primitives remain grammar-neutral by construction. The §3 closing line at p2d:153 affirms the four enumerated Lock 14 prohibitions (no JSON match arm, no JSON-named module, no JSON-specific type in generic-crate public API, no JSON-keyed feature flag) — verified unchanged. **ACCEPT.**

### §2.6 — P2-E (parse-that gaps, V1+V2-LOCKED): ACCEPT

V1+V2-LOCKED at V3. Zero V3 drift per §1.1 git-diff. V1+V2 CH2 disposition holds verbatim: all 9 gaps carry per-gap JSON / CSS L4 / Sheets / BBNF-self consumer columns at p2e:235-247; §3 closing line ("All 8 gaps are grammar-neutral; none requires per-grammar specialization at the primitive layer") intact; §4.3 "Layer-1 primitives carry NO defaults" canonical Lock 14 v+1 enforcement statement unchanged. **ACCEPT.**

### §2.7 — Cross-axis CH2 V3 convergence (all six artefacts)

Per the CHALLENGE-CONTEXT V3 §2 CH2 confirming-cycle binding ("confirm Lock 14 + grammar-neutrality posture unchanged; F-V2-CH2-1/2 cross-axis tracking + dual-gating still intact"):

| Lock 14 v+1 binding aspect | P2-A | P2-B | P2-C | P2-D | P2-E | P2-F |
|---|---|---|---|---|---|---|
| Per-candidate grammar-neutrality verdict | §3 lines 205-213 (7) — V2-LOCKED | §3 lines 156-160 (5 stages) — V2-LOCKED | §3 lines 75-84 (8 entries) — V2-LOCKED | §3 lines 148-151 (4 entries) — V2-LOCKED | §3 lines 235-247 (9 gaps) — V1+V2-LOCKED | §3 lines 247-260 (14 entries) — V3 verb-tense cells outside §3 |
| At-least-one-non-JSON-consumer | §3 spec-evidence columns | Stage B mandate | per-row JSON/CSS/Sheets/BBNF columns | C-P2D-1/2 substrate-level all grammars | per-gap columns | §3 13/13 × 3 (V3 unchanged) |
| Hardcoded-policy CH2 violation surfacing | n/a | Stage A mandate | C-P2C-5 → `classify_tbl4.rs:33-35` | n/a | §4.3 "no defaults" | §3 line 264 → `parse-that-regex/src/lib.rs:718` + `StringFlags::HAS_ESC` (V3 unchanged) |
| CSS L4 spec-only generalisation (CH2 F2) | §3 line 203 explicit | §3 line 158 generic | C-P2C-1 disposition separates CH2 from CH4 PRUNE-2 | §3 line 153 "by construction" | §3 line 249 spec-grounded | §0 line 12 + §3 lines 247-260 explicit (V3 unchanged) |
| V2 demotion preservation | n/a | n/a | §3 rows 77/82/83 intact for demoted C-P2C-1/6/7 | §3 row 150 intact for demoted C-P2D-3 | n/a | §3 row 254 intact (C8 demoted) — V3 unchanged |
| Cross-axis consolidation (§2.Y NF-CH6-4) | §2.Y reference (P2-A C2) | n/a | n/a | n/a | §2.Y reference (P2-E Gap 1) | §2.Y binding at p2f:231-239 (V3 unchanged) |
| F-V2-P1ABC-RERECORD dual-gate propagation | P2-A C6 process-gate ≡ rerun | n/a | C-P2C-3 + C-P2C-8 disposition rows | n/a | Gap 1/3/4/5 §4.7 dependency | C6 §4 risk row + C8 §2.X.1 re-promotion + C10/C12/C13 antecedent stamps (V3 verb-tense edit does NOT touch these — see §1.5) |

All six independent P2 axis artefacts converge at V3 on the same Lock 14 v+1 binding as V2 with no drift in any cell:
- (a) every candidate (active or demoted) carries a grammar-neutral verdict;
- (b) at-least-one-non-JSON-consumer is named across all 13/13/13 active P2-F V3 candidates;
- (c) hardcoded JSON policy is surfaced for lifting where it exists (C-P2C-5, C3);
- (d) CSS L4 generalisation is spec-grounded per CH2 F2;
- (e) V2 demotions preserve verdicts verbatim;
- (f) cross-axis consolidation (§2.Y NF-CH6-4) prevents grammar-neutrality drift across three colliding axes;
- (g) F-V2-P1ABC-RERECORD dual-gating preserved at all 12 V2-propagated consumer-dependency sites.

The dispatch-context V3 §2 CH2 confirming-cycle binding is satisfied with **six-witness redundancy** at the V3 cycle — identical to V2. The P2-F V3 load-bearing arbiter's 13/13 active admission count is independently corroborated by the four sibling axes' 29 active candidate/stage/gap verdicts. No JSON-OVERFIT-IRREDUCIBLE candidates across any axis at V3. The V3 CH2 GENERALITY lens discharge is structurally complete and identical to V2 — **the verb-tense correction at p2f:164 + p2f:197 is CH4-confining (Stage-A authoring discipline temporal framing) and CH2-neutral**.

## §3 — Critical V3 findings (none warrant REVISE; confirming-cycle preserves V1+V2 trajectory)

### §3.1 — V2 finding F-V2-CH2-1 (NF-CH6-4 §2.Y cross-axis tracking) holds at V3

Per §1.4 above, the §2.Y NF-CH6-4 cross-axis tracking note at p2f:231-239 is intact at V3 HEAD; the three-axis convergence (P2-A C2 ≡ P2-E Gap 1 ≡ P2-F C1+C2) is preserved verbatim; the "ONE canonical primitive name + ONE canonical scalar reference function" S-P3 consolidator binding is preserved verbatim. **F-V2-CH2-1 confirming-cycle disposition: HELD (non-blocking, continues to ratify the V3 cycle trajectory toward cohort §3Z LOCK).**

### §3.2 — V2 finding F-V2-CH2-2 (F-V2-P1ABC-RERECORD CH2/CH4 dual-gating) holds at V3

Per §1.5 above, the F-V2-P1ABC-RERECORD CH2/CH4 dual-gate is preserved at all 12 V2-propagated consumer-dependency sites at V3 HEAD; the V3 verb-tense edits at p2f:164 + p2f:197 sit on **Scalar-ref status** bullets and do NOT touch the immediately-prior **P1 antecedent** bullets at p2f:162 + p2f:195 which carry the F-V2-P1ABC-RERECORD reference; the P2-F §4 C6 risk row at p2f:300 + §2.X.1 C8 re-promotion gate at p2f:229 are byte-for-byte intact. **F-V2-CH2-2 confirming-cycle disposition: HELD (non-blocking, continues to ratify V2 fold-packet completion at V3).**

### §3.3 — V1 R1 / R2 / F1 / F2 carry-forwards preserved at V3

- **V1 R1 (cross-axis candidate-ID reconciliation):** discharged at V2 via V2 CH2 §1.5 explicit reconciliation table; the table inputs (axis file contents) are V2-LOCKED at V3 per §1.1, so the reconciliation table outputs remain valid at V3 HEAD. **Preserved.**
- **V1 R2 (P2-C C-P2C-1 disposition language):** discharged at V2 via Fold-2 demotion structural separation; the §2.X partition at p2c:48-71 is V2-LOCKED at V3 per §1.1. **Preserved.**
- **V1 F1 (substrate-union YES six-witness corroboration):** preserved at V2; V3 makes no edits to substrate-union evidence sites — P2-D §1.5/1.6/4.7 (V2-LOCKED), P2-F §1.3 (V3-unchanged at p2f:52-54), CH5 V1, P1-V3-CH5 §3.78-83. **Preserved.**
- **V1 F2 (parse-attribution rerun co-required for CH2 measurability):** elevated to ratified V2 binding F-V2-P1ABC-RERECORD CH2/CH4 dual-gate; held at V3 per §1.5 / §3.2 above. **Preserved.**

### §3.4 — Confirming-cycle observation: V3 verb-tense fix is exclusively CH1+CH4 surface (no CH2 surface)

The two V3 atomic micro-fold cells (p2f:164 C10 + p2f:197 C13) are the V3 commit message's "Discharges F-V2-CH1-1 + F-V2-CH4-1 convergently. Stage-A authoring discipline framing preserved verbatim on both cells." The CH2 lens at V3 records this as an **observation-only** finding: the verb-tense correction lifts a CH1 cite-discipline + CH4 Stage-A authoring-discipline issue (the past-tense "landed" over-claim was inaccurate against the actual sk-v14 source tree — `crates/bbnf-simd/src/scalar/byte_context_64.rs` and `crates/bbnf-simd/src/scalar/bcax_64.rs` are not yet authored, only named as scalar-ref targets pending Lock 16 same-commit S-P3 admission). The CH2 GENERALITY lens has no over-claim to discharge; the C10 + C13 §3 grammar-neutrality verdicts (NEUTRAL-WIRED, Lock 16 :285 + :289 abstract-primitive declarations) are independent of the Stage-A authoring temporal framing. **V3 CH2 surface is verification-only; zero V3-introduced CH2 findings.**

## §4 — V3 fold recommendations (CH2-binding)

### §4.1 — V3 cycle status

All V1 + V2 CH2 mandatory + non-blocking actions remain discharged at V3:

1. V1 R1 cross-axis candidate-ID reconciliation table → discharged at V2 §1.5; preserved at V3 (V2-LOCKED inputs).
2. V1 R2 P2-C C-P2C-1 disposition language refinement → discharged at V2 via Fold-2; preserved at V3.
3. V1 F2 F-V2-P1ABC-RERECORD CH2/CH4 dual-gating promotion → discharged at V2 + held at V3 across all 12 consumer-dependency candidates per §1.5.

The V1 CH2 §4.2 non-blocking actions remain non-blocking; the §4.2.4 cross-axis CH2-violation register can be authored as a V3 consolidator deliverable (orchestrator-aggregator decision); the §4.2.5 CH5 V1 cross-reference remains discharged in HARDENING-S-P2-V1-CONSOLIDATED §2.2 (six-witness substrate-union YES corroboration).

### §4.2 — V2 → V3 → LOCK trajectory realisation

CH2 V3 disposition: **100% ACCEPT** (6/6 artefacts; 39 active + 6 demoted/pre-blocked = 45 cross-axis entries; zero REVISE; zero REJECT; identical to V2). The lens has zero open REVISE items at V3.

Cycle chain at the CH2 per-lens layer:
- V1: 100% (six-artefact aggregate; zero orphan REVISEs).
- V2: 100% (six-artefact aggregate; zero orphan REVISEs).
- V3: 100% (six-artefact aggregate; zero orphan REVISEs).

### §4.3 — §3Z LOCK criteria at the CH2 per-lens layer: THREE-CYCLE LOCK

Per `ORCHESTRATOR.md §3Z` (≥ 95% × 2 consecutive cycles + zero orphan REVISEs):

- CH2 V1 → V2 chain: lens-level LOCK criterion met at V2 (per V2 §4.3 prediction; ratified by V3 outcome).
- CH2 V2 → V3 chain: lens-level LOCK criterion met at V3 — **second consecutive ≥ 95% cycle from the V2 baseline of the chain that started with V1 fold-packet closure**.

**CH2 V3 contribution to the cohort §3Z gate:** lens-level LOCK criterion is met for the **third consecutive cycle** at V3 (an extension beyond the §3Z minimum); CH2 is now a 3-cycle LOCK lens. The cohort-level §3Z gate depends on aggregate ACCEPT-rate across all 7 lenses; the V3 cohort §3Z evaluation depends on CH1 + CH4 second-cycle ≥ 95% post-V3 verb-tense fix discharge (the two lenses with open V2 ACCEPT-WITH-NOTE findings that V3 was specifically dispatched to discharge). **CH2 V3 carries no obstruction to the cohort §3Z LOCK gate.**

## §5 — Sources (verified against V3 HEAD commit `ebe84954b`)

### §5.1 — Binding context (read in order)

- `restart/prompts/skinny/PASS-2-RESEARCH.md §3 CH2 GENERALITY` (lens definition)
- `restart/prompts/ORCHESTRATOR.md §3W lens registry + §3Z convergence rule + §8 non-negotiables` (Lock 1 substrate union; Lock 14 grammar-neutrality; scalar-reference + checkasm; same-wave consumer)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V3/CHALLENGE-CONTEXT.md §0-§4` (V3 dispatch posture; CH2 V3 row at §2)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CHALLENGE-CONTEXT.md` (V2 dispatch posture; carry-forward authority for V2 CH2 binding)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CH2.md` (V2 CH2 carry-forward; 100% ACCEPT; the V3 confirming-cycle baseline)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CH2.md` (V1 CH2 baseline; 100% ACCEPT)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md` (V2 aggregator + V3 fold-packet authority)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md` (V1 aggregator + fold-packet authority + F-V2-P1ABC-RERECORD CH2/CH4 dual-gating binding entry at §2.1)
- `restart/locks/LOCKS.md:220-263` (Lock 14 + v+1 amendments — primary CH2 authority); `:48-90` (Lock 1 substrate-union v+1 manifest); `:265-340` (Lock 15 + Lock 16 abstract-primitive declarations including :285 cross-chunk byte-context + :289 BCAX)

### §5.2 — Artefacts disposition (per §0 V3 disposition table)

- `restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md:1-367` (V2-LOCKED; zero V3 drift)
- `restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md:1-217` (V2-LOCKED; zero V3 drift)
- `restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md:1-164` (V2-LOCKED; zero V3 drift)
- `restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md:1-254` (V2-LOCKED; zero V3 drift)
- `restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md:1-342` (V1+V2-LOCKED; zero V3 drift)
- `restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md:1-360` (V3 verb-tense amended at p2f:164 + p2f:197; CH2 surface unchanged — see §1.2 + §2.1)

### §5.3 — Source crosscheck (V3 HEAD-verified per §1.6 + §1.7)

- `skinny/crates/runtime/src/grammars/json/generated.rs:33-237, 45, 159, 164, 169, 187, 213, 466, 506, 650` (envelope + every cited grammar-neutral primitive at V3 HEAD)
- `skinny/crates/runtime/src/grammars/json/scan.rs:22, 32, 107, 131, 164` (structural scan primitives + tape-emit sites)
- `skinny/crates/parse-that-regex/src/lib.rs:718, 945` (`unescape_string` canonical CH2 violation site; `read_hex_unit_scalar`)
- `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:2` (`bulk_emit_positions_64_neon` — P2-F C9 substrate primitive)
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40` (`unescape_uxxxx_scalar` — P2-C C-P2C-4 + P2-E Gap 2 scalar reference)
- `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:5, 27` (`parse_4_digits` + `parse_4_digits_dotprod` — P2-C C-P2C-3 + P2-E Gap 5 scalar + SIMD pair)
- `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:33-35` (hardcoded JSON constants — P2-C C-P2C-5 CH2 violation pointer)
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:2742-2746, 2949-3003` (Track 2 `DirectParser` two-cursor independence + skip primitives)
- `grammar/css/l4/tokens.bbnf:9` (string with 2-quote disjunction)
- `grammar/css/l4/value-unit.bbnf:15` (number with `-> f64` projection)
- `grammar/google-sheets/google-sheets.bbnf:6, 12, 34, 90` (number, doubled-quote string, error_literal 9-keyword set, identifier byte-set)
- `grammar/bbnf/bbnf.bbnf:9, 11-13, 17-18` (identifier, 3-quote literal, comment markers)
- `restart/locks/LOCKS.md:255-263` (Lock 14 v+1 closing clause — operative CH2 V3 admission gate; verified at HEAD)
- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-lock14-scan.md:9` (DELTA-NOTE `parse-that-regex::StringFlags::HAS_ESC` lift target)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH5.md:78-83` (two-cursor independence — substrate-union YES upstream evidence)

### §5.4 — V3 cycle commit anchors

- V2 commit (S-P2 hardening V2 challenge + consolidated): `4c70b6f19` per V3 CHALLENGE-CONTEXT §1 (the V2-LOCKED baseline for V3 drift audit).
- V3 commit (S-P2 V3 atomic micro-fold + V3 dispatch context): `ebe84954b1a6c31bb6183ca8f5e68d88647d9df7` per `git log --oneline -1 -- restart/skinny/tranches/sk-v14/research/p2/`.
- V3 diff stat: `git diff 4c70b6f19..ebe84954b --stat -- restart/skinny/tranches/sk-v14/research/p2/` reports p2f:+2/-2 (verb-tense at :164 + :197) + V3/CHALLENGE-CONTEXT.md:+40 lines new file; P2-A/B/C/D/E absent (zero drift).
