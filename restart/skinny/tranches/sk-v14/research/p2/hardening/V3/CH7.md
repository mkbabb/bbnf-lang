# S-P2 V3 — CH7 OVERFIT-PRUNE Lens (Confirming Cycle)

Lens: **CH7 Overfit-Prune** per `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7` (lines 62-87). V1 verdict (`research/p2/hardening/V1/CH7.md`): 6/6 ACCEPT, 100%, no REVISE. V2 verdict (`research/p2/hardening/V2/CH7.md`): 6/6 ACCEPT, 100%, no REVISE; mandates (3) + (5) strengthened. **V3 disposition (confirming cycle):** re-lens the one V3-amended axis (P2-F, two verb-tense cells §2.10 + §2.13) against the five CH7 mandates; confirm zero drift on the five V2-LOCKED axes (P2-A, P2-B, P2-C, P2-D, P2-E); confirm the V3 verb-tense edit is CH7-correct (anti-overstatement); audit-overlay binding preserved.

Per CHALLENGE-CONTEXT V3 §2 CH7 focus, V3 verifies four pinned properties: (a) Stage-A target naming overfit-asymmetry preserved (`byte_context_64.rs` + `bcax_64.rs` correctly absent; sibling templates present); (b) demotion-with-gap-note pattern intact (all 5 V2 demotions still preserve identifier + technical content); (c) V3 verb-tense fix is CH7-correct (removes overstated "landed" — strictly anti-overfit); (d) no V3 fake-pattern recurrence.

Mandate (verbatim from PASS-0 §CH7):

1. every new code added is grammar-derived (template + grammar metadata + emission command) — never hand-written under a `// @generated` header;
2. Lock 14 generic-crate compliance is preserved (no JSON/CSS/Sheets string literals, byte literals, function names, enum variants, or match arms in nominally-generic code);
3. every admit lands via a real parser/codegen/SIMD source change, measured against a strict-vs-strict comparator on the same plane, with a per-iteration equality oracle;
4. every "generated" output passes a round-trip test (delete + regen ⇒ byte-equivalent);
5. no SCAFFOLD-ONLY landing (research artefacts without source wiring) counts as an admit.

CH7 REJECT triggers immediate plan revise OR redress revert. CH7 cannot be carried as "acknowledged but not blocking".

Artefacts under review: 6 P2 axis files at V3 amended HEAD (commit `ebe84954b`); per CHALLENGE-CONTEXT V3 §1: P2-A + P2-B + P2-C + P2-D + P2-E V2-LOCKED (zero V3 edits); P2-F V3-amended at two verb-tense cells (§2.10 + §2.13).

---

## §0 — Executable verification

### §0.1 — V2-LOCKED axis drift audit (mandatory per CHALLENGE-CONTEXT V3 §2 confirming-cycle discipline)

```
$ git diff 4c70b6f19..ebe84954b --stat -- \
    restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md \
    restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md \
    restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md \
    restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md \
    restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md
(empty output)
```

**Zero drift on P2-A + P2-B + P2-C + P2-D + P2-E.** All five V2-LOCKED axes are byte-identical to the V2 commit `4c70b6f19`. CH7 V2 verdicts for these five artefacts (all ACCEPT) carry forward verbatim to V3 with no re-evaluation required. V2's §0.1 zero-drift evidence for P2-A + P2-E (vs V1) likewise carries forward.

### §0.2 — V3 amendment surface (1 file, 2 cells)

```
$ git diff 4c70b6f19..ebe84954b --stat -- \
    restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md
 .../sk-v14/research/p2/p2f-grammar-neutral.md | 4 ++--
 1 file changed, 2 insertions(+), 2 deletions(-)
```

V3 amends one axis file (P2-F) with two single-line verb-tense cells: §2.10 C10 (line 164) + §2.13 C13 (line 197). Net delta: -2 / +2 lines. No new candidate added; no candidate demoted; no Stage-A target path changed; no scalar-ref discriminator changed; no audit-overlay citation added or removed.

### §0.3 — V3 verb-tense cell content (the two atomic micro-fold edits)

```
$ grep -n "queued for S-P3" restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md
164:... Authoring queued for S-P3 same-wave Lock 16 same-commit admission per
     HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-4; function body lands
     same-commit with SIMD body at S-P3.
197:... Authoring queued for S-P3 same-wave Lock 16 same-commit admission per
     HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-5; function body lands
     same-commit with SIMD body at S-P3.
```

V3 replaces the V2 wording "Authoring landed as Fold-{4,5} V2 deliverable per HARDENING-S-P2-V1-CONSOLIDATED §3.4" with "Authoring queued for S-P3 same-wave Lock 16 same-commit admission per HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-{4,5}; function body lands same-commit with SIMD body at S-P3."

The V2 wording overstated tense: it asserted the scalar-ref *was authored* as the V2 hardening deliverable, when in fact §0.4 below executable-verifies that both `byte_context_64.rs` + `bcax_64.rs` do NOT exist at HEAD — they are explicitly Stage-A authoring targets. The V3 wording aligns the verb tense with the actual disposition: queued for S-P3 authoring at Lock 16 same-commit time, not landed as V2 work product. **The V3 verb-tense fix is strictly anti-overfit: it removes a phrasing that could be misread as a fake-completed claim and replaces it with the load-bearing Stage-A framing.**

### §0.4 — Stage-A target naming overfit-asymmetric preservation (CHALLENGE-CONTEXT V3 §2 CH7 item (a))

```
$ ls skinny/crates/bbnf-simd/src/scalar/byte_context_64.rs \
     skinny/crates/bbnf-simd/src/scalar/bcax_64.rs
ls: skinny/crates/bbnf-simd/src/scalar/bcax_64.rs: No such file or directory
ls: skinny/crates/bbnf-simd/src/scalar/byte_context_64.rs: No such file or directory

$ ls skinny/crates/bbnf-simd/src/scalar/
bitmap_next_set_bit.rs
bitmap_prefix_xor_64.rs        <-- sibling for bcax_64.rs (Fold-5)
bulk_emit_positions_64.rs
byte_class_from_eq_set_64.rs   <-- sibling for byte_context_64.rs (Fold-4)
byte_class_from_table_64.rs
eob_pad_clamp.rs
mod.rs
swar_8byte.rs
```

**Stage-A targets correctly absent; sibling templates present.** The CH7-affirmative overfit-asymmetry from V2 §3.3 holds verbatim at V3: not-yet-existent paths cited as authoring targets, with sibling-shape templates explicitly named for shape inheritance. The V3 verb-tense fix at §2.10 + §2.13 *strengthens* this asymmetry by removing the residual V2 phrasing that conflated framing with completion.

### §0.5 — Grammar enumeration (CH7 V2 binding preserved)

```
$ cargo metadata --format-version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident'
bbnf
json
css_l4
css_pretty
google_sheets
ebnf
bnf
csv
math
```

**Count: 9.** Identical to V1 + V2; Lock 14 + Lock 16 audit-overlay binding surface unchanged at V3.

### §0.6 — Per-mandate executable spot-checks (V3 re-verification of V2 binding evidence)

**Mandate (3) — scalar-ref discriminator for the P2-F C12 reframing-CH4-ACCEPT (the load-bearing V2 micro-fold; not V3-amended but re-verified):**

```
$ grep -n "scan_structurals_scalar" \
    skinny/crates/runtime/src/grammars/json/scan.rs
29:    scan_structurals_scalar(input)
32:pub fn scan_structurals_scalar(input: &[u8]) -> StructuralIndex {
39:    let index = scan_structurals_scalar(input);
280:    use super::{scan_structurals, scan_structurals_scalar, ScanBackend};
297:        let scalar = scan_structurals_scalar(input);
```

`scan_structurals_scalar` exists at line 32 at the V3 HEAD; identical to V2. C12 reframing-CH4-ACCEPT scalar-ref discriminator still met.

**Mandate (3) — P2-E Gap 1 scalar reference (P2-F §2.Y cross-axis tracking cite):**

```
$ grep -n "scan_string_special_block_scalar" \
    skinny/crates/bbnf-simd/src/aarch64/string_block.rs
31:pub fn scan_string_special_block_scalar(
```

Exists at line 31 at the V3 HEAD; identical to V2. P2-F §2.Y three-way convergence binding still live.

**Mandate (1) — no hand-written `// @generated` proposed:**

```
$ grep -rn "@generated\|GENERATED" restart/skinny/tranches/sk-v14/research/p2/p2{a,b,c,d,e,f}-*.md
(zero hits)
```

Zero `@generated` hand-write proposals across all six axis files at V3; identical to V2.

### §0.7 — Audit-overlay citation distribution (V3 re-audit)

```
$ grep -nc "audit-overlay\|audit-overfit\|SYNTHESIS-AUDIT-OVERFIT\|PRUNE-[0-9]" \
    restart/skinny/tranches/sk-v14/research/p2/p2{a,b,c,d,e,f}-*.md
p2a-sota-teardown.md:4
p2b-dav1d-process.md:0
p2c-arch-esoterica.md:5
p2d-substrate-tape.md:0
p2e-parse-that-gaps.md:1
p2f-grammar-neutral.md:5
```

**Total direct citations: 15.** Identical to V1 + V2 distribution (4 / 0 / 5 / 0 / 1 / 5 = 15). V3 verb-tense edits did not strip or add any audit-overlay citation. Indirect bindings (p2b via Lock 14 v+1 amendment; p2d via V3 CH5 substrate-union verdict) likewise preserved. **Audit-overlay binding intact at V3.**

---

## §1 — Per-artefact CH7 disposition at V3

| Artefact | V2 verdict | V3 changes | V3 verdict |
|---|---|---|---|
| `p2a-sota-teardown.md` | ACCEPT (carry-forward) | NONE (V2-LOCKED; zero diff per §0.1) | **ACCEPT (2-cycle LOCK; carry-forward; zero drift)** |
| `p2b-dav1d-process.md` | ACCEPT (strengthened) | NONE (V2-LOCKED; zero diff per §0.1) | **ACCEPT (2-cycle LOCK; carry-forward; zero drift)** |
| `p2c-arch-esoterica.md` | ACCEPT (strengthens mandate (5)) | NONE (V2-LOCKED; zero diff per §0.1) | **ACCEPT (2-cycle LOCK; carry-forward; zero drift)** |
| `p2d-substrate-tape.md` | ACCEPT (strengthens mandate (5)) | NONE (V2-LOCKED; zero diff per §0.1) | **ACCEPT (2-cycle LOCK; carry-forward; zero drift)** |
| `p2e-parse-that-gaps.md` | ACCEPT (carry-forward) | NONE (V2-LOCKED; zero diff per §0.1) | **ACCEPT (2-cycle LOCK; carry-forward; zero drift)** |
| `p2f-grammar-neutral.md` | ACCEPT (strengthens mandates (3) and (5)) | Two verb-tense cells (§2.10 C10 + §2.13 C13): "landed as Fold-{4,5} V2 deliverable" → "queued for S-P3 same-wave Lock 16 same-commit admission ...; function body lands same-commit with SIMD body at S-P3"; no scalar-ref target path changed; no audit-overlay citation changed; Stage-A target naming overfit-asymmetric preservation unchanged (per §0.4) | **ACCEPT (strengthens mandate (5) further: verb tense now matches not-yet-existent target status)** |

**V3 ACCEPT rate: 6/6 = 100%.** Composite cycle disposition: re-affirm V2 verdict; **mandate (5) further strengthened on P2-F via verb-tense alignment between cited disposition and on-disk reality**; mandates (1), (2), (3), (4) unchanged.

---

## §2 — Mandate-by-mandate roll-up at V3

| CH7 mandate | V2 status | V3 status | V3 delta |
|---|---|---|---|
| (1) Grammar-derived code; no `// @generated` hand-write | PASS 6/6 | PASS 6/6 | Zero new `@generated` hand-write proposals; zero `@generated` hits across all six V3 axis files per §0.6 grep. **Mandate (1) status unchanged.** |
| (2) Lock 14 v+1 generic-crate compliance | PASS 6/6 | PASS 6/6 | V3 verb-tense cells do not introduce JSON/CSS/Sheets string literals, byte literals, function names, enum variants, or match arms; the cells edit only English prose around dispositional tense. **Lock 14 v+1 unchanged.** |
| (3) Real source change + strict-vs-strict + per-iteration equality oracle | PASS 6/6 | PASS 6/6 | V2-strengthening evidence (P2-B SHA pinning; P2-F Fold-4 + Fold-5 Stage-A scalar-ref target naming; C12 reframing scalar-ref live at `scan.rs:32`) all carry forward verbatim per §0.1 zero-drift on five axes + §0.6 spot-checks for the P2-F cites. **Mandate (3) status unchanged; V2 strengthening preserved.** |
| (4) Round-trip test for `generated` output | PASS 6/6 | PASS 6/6 | V3 verb-tense cells do not propose a codegen hand-patch route; `tests/regen_parity.rs` family invariant unchanged; p2e structural exemption unchanged. **Mandate (4) status unchanged.** |
| (5) No SCAFFOLD-ONLY landing | PASS 6/6 | PASS 6/6 | **FURTHER STRENGTHENED.** The V3 verb-tense cells remove the residual V2 phrasing ("landed as Fold-{4,5} V2 deliverable") that could be misread as a fake-completed claim — the Stage-A target files demonstrably do NOT exist at HEAD per §0.4 (`ls` errors verbatim cited). The V3 phrasing ("queued for S-P3 same-wave Lock 16 same-commit admission ...; function body lands same-commit with SIMD body at S-P3") explicitly aligns the verb tense with the not-yet-existent file status. This is mandate (5) operating in its purest form: refusing to phrase research-artefact disposition as if source wiring had landed. |

**Composite at V3: 6/6 ACCEPT, no REVISE, no REJECT.** Mandate (5) further strengthened on P2-F; mandates (1), (2), (3), (4) unchanged with all V2 strengthening preserved.

---

## §3 — V3 OVERFIT-PRUNE-specific audit (CHALLENGE-CONTEXT V3 §2 CH7 focus items)

The V3 challenge context specifies four overfit-prune focus items for the confirming cycle. Each is re-lensed below.

### §3.1 — Stage-A target naming overfit-asymmetry preserved (`byte_context_64.rs` + `bcax_64.rs` correctly absent; sibling templates present)

**Verified per §0.4.** Both Stage-A target files (`crates/bbnf-simd/src/scalar/byte_context_64.rs` for Fold-4, `crates/bbnf-simd/src/scalar/bcax_64.rs` for Fold-5) are absent at the V3 HEAD (`ls` returns `No such file or directory` for both). Both sibling-shape templates (`byte_class_from_eq_set_64.rs` for Fold-4, `bitmap_prefix_xor_64.rs` for Fold-5) are present at the V3 HEAD. The overfit-asymmetric framing established in V2 §3.3 — not-yet-existent paths cited as Stage-A authoring targets with concrete sibling templates for shape inheritance — holds verbatim at V3, and is *strengthened* by the V3 verb-tense cells removing the residual "landed" phrasing.

**Overfit risk would be:** Stage-A targets quietly authored between V2 and V3 (which would convert the not-yet-existent framing into a fait-accompli admit without same-commit SIMD body); OR sibling templates removed (which would strip the executable shape evidence); OR Stage-A target path-strings altered in the prose (which would break the cross-tranche identifier). Neither pattern present. CH7-affirmative.

### §3.2 — Demotion-with-gap-note pattern intact (all 5 V2 demotions preserve identifier + technical content)

**Verified per §0.1 zero-drift on P2-C + P2-D + P2-F demotion-bearing axes.** The 5 V2 demotions (C-P2C-1, C-P2C-6, C-P2C-7 to P2-C §2.X; C-P2D-3 to P2-D §1.6(d) + §2 gap-note; C8 to P2-F §2.X.1) are byte-identical at V3 (P2-C + P2-D fully zero-drift; P2-F has only the two verb-tense cells at §2.10 + §2.13 — neither cell touches the §2.X.1 C8 demotion inventory). Every V2 demotion still preserves the candidate identifier as gap-note for cross-tranche stability; every V2 demotion still preserves the full technical content verbatim under a disposition stamp; every V2 demotion still names an explicit re-promotion gate.

**Overfit risk would be:** V3 silently dropping or trimming any of the 5 demoted rows (which would lose the cross-tranche audit surface); OR V3 promoting a demoted candidate without a same-wave consumer commit (which would violate the V1 + V2 anti-scaffold-admit pattern). Neither pattern present. The V3 §0.1 zero-drift `git diff` proves the demotion-with-gap-note pattern is byte-identical to V2. CH7-affirmative.

### §3.3 — V3 verb-tense fix is CH7-correct (removes overstated "landed" — strictly anti-overfit)

**Verified per §0.3 + §0.4.** The V3 verb-tense cells replace the V2 phrasing "Authoring landed as Fold-{4,5} V2 deliverable per HARDENING-S-P2-V1-CONSOLIDATED §3.4" with "Authoring queued for S-P3 same-wave Lock 16 same-commit admission per HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-{4,5}; function body lands same-commit with SIMD body at S-P3."

The V2 phrasing carried a verb-tense overstatement: "landed as ... V2 deliverable" implies the scalar-ref file was authored as part of the V2 hardening work product. The on-disk reality (§0.4 `ls` evidence) is that both target files do NOT exist at the V3 HEAD — same on-disk reality as at V2 HEAD. The V2 phrasing was thus a *latent fake-pattern recurrence risk*: a reader who didn't run the `ls` could read "landed" as completion. The V3 phrasing eliminates this risk by aligning verb tense with on-disk reality: "queued for S-P3 ... lands same-commit with SIMD body at S-P3" is unambiguous future-tense conditional on S-P3 dispatch.

**This is CH7 mandate (5) operating in its purest form on prose-level overfit.** The V3 edit is not a substantive scope change (no candidate added, removed, or re-classified; no scalar-ref discriminator changed; no audit-overlay citation altered); it is a precision-of-claim correction that removes the only residual phrasing across all 6 V2 axes where verb tense outran on-disk reality. CH7-affirmative; recommend the aggregator pin §3.3 as a precedent for prose-level overfit detection in future confirming cycles.

### §3.4 — No V3 fake-pattern recurrence

**Verified per §0.1 + §0.6 + §0.7.** The V3 cycle introduces:

1. Zero new `@generated` hand-write proposals (§0.6 grep returns no hits across all six axes).
2. Zero new candidates admitted (V3 diff stat: 4 lines / 2 cells; both cells edit existing §2.10 / §2.13 candidate dispositions, not new candidate slots).
3. Zero scalar-ref discriminator changes (§0.6 spot-checks for `scan_structurals_scalar` at `scan.rs:32` and `scan_string_special_block_scalar` at `string_block.rs:31` are byte-identical to V2).
4. Zero new audit-overlay citations (§0.7 distribution unchanged: 4 / 0 / 5 / 0 / 1 / 5 = 15 total).
5. Zero new Stage-A target paths (§0.4 confirms `byte_context_64.rs` + `bcax_64.rs` still absent; sibling templates still present; no third Stage-A target introduced).
6. Zero gate-relabel pattern (V3 does not relabel a V2 REVISE-pending gate as "ACCEPT-with-discharge"; V2 had no CH7 REVISE-pending gates to relabel; V3 verb-tense fix is a precision-of-claim correction, not a gate move).

**Overfit risk would be:** introducing any of the six patterns above. None present. The V3 cycle is a textbook *minimal confirming-cycle edit*: precision-of-claim correction at one prose surface, byte-identical preservation everywhere else.

---

## §4 — New findings (V3-specific)

1. **Verb-tense overstatement is a CH7 mandate (5) attack surface that survives multi-cycle ACCEPT.** The V2 CH7 verdict scored 100% ACCEPT and was V2-LOCK-eligible — yet still carried two prose cells where verb tense outran on-disk reality. The V3 confirming cycle catches this via the standing `ls`-existence executable verification protocol (§0.4): the same `ls` evidence that V2 cited as CH7-affirmative for Stage-A asymmetry is the lever that exposes the residual verb-tense mismatch. **Recommendation:** the aggregator should pin the V3 §3.3 precedent (verb-tense alignment with `ls`-existence reality) as a standing CH7 sub-test for all future Stage-A target naming surfaces. Any prose claim using completion verbs ("landed", "delivered", "shipped") on a Stage-A target path must be backed by an `ls`-existence verification at the cited cycle's HEAD; otherwise mandate (5) flags it.

2. **The V3 verb-tense fix demonstrates the confirming-cycle's residual-overfit-detection capacity.** §3Z confirming-cycle discipline is often perceived as a redundancy check (verify the prior cycle still holds), but the V3 cycle shows the confirming-cycle catches *prose-level* overfit that scope-substantive lenses (CH1-CH6) miss. CH7 is uniquely positioned for this catch because the on-disk `ls` executable verification is a content-independent ground truth: the same evidence supports the V2 ACCEPT (Stage-A naming framed correctly) and the V3 strengthening (verb tense aligned with on-disk reality).

3. **Five-axis V3 zero-drift confirmed; V2 CH7 verdicts carry forward verbatim on P2-A + P2-B + P2-C + P2-D + P2-E.** The V2 commit `4c70b6f19` and V3 commit `ebe84954b` are byte-identical on five of six axes. The five V2 CH7 findings specific to those five axes (demotion-with-gap-note pattern via P2-C + P2-D; SHA pinning via P2-B; structural exemption via P2-E; carry-forward zero-drift on P2-A) all hold at V3 with no re-evaluation required.

4. **P2-F V3-amended axis: only verb-tense cells touched; substantive content unchanged.** The two V3 cells edit *only* the disposition-verb at the tail of the scalar-ref status bullet in §2.10 + §2.13. Stage-A target path strings unchanged; sibling-template citations unchanged; arch / source-anchor / P1-antecedent / shape rows unchanged. The §2.Y cross-axis tracking note, the §2.X.1 C8 demotion inventory, the §3 partition table, the §3 verdict table, and the §3 consumer-existence summary all V3-zero-drift. **The V3 verb-tense fix is the minimum possible mandate (5) strengthening edit on P2-F.**

---

## §5 — Disposition

**V3 CH7 verdict: 6/6 ACCEPT. ACCEPT-rate: 100%.** Same composite as V1 + V2; mandate (5) further strengthened by the V3 verb-tense alignment on P2-F §2.10 + §2.13; mandates (1), (2), (3), (4) unchanged with all V2 strengthening preserved.

**Cycle disposition (§3Z gate evaluation):** V3 cycle achieves second consecutive ≥95% on the CH7 axis (V2: 100% ACCEPT; V3: 100% ACCEPT). CH7 axis was **2-cycle LOCK at V2** already (per CHALLENGE-CONTEXT V3 §2 framing: "5/7 per-lens 2-cycle LOCKs achieved (CH2/CH3/CH5/CH6/CH7)"); V3 confirming cycle therefore extends CH7 to a **3-cycle LOCK** (V1 + V2 + V3 all 6/6 ACCEPT, 100%, no REVISE, no REJECT). CH7 axis admits V3 LOCK confirmation pending aggregator commit.

Pinned for aggregator (`HARDENING-S-P2-V3-CONSOLIDATED.md`):

- §3.1 Stage-A target naming overfit-asymmetric preservation re-confirmed at V3 via §0.4 `ls`-existence executable verification.
- §3.2 demotion-with-gap-note pattern byte-identical at V3 per §0.1 zero-drift on P2-C + P2-D + P2-F (demotion inventory cells unaffected by V3 verb-tense edits).
- §3.3 V3 verb-tense fix as CH7 mandate (5) operating on prose-level overfit; recommend as standing precedent for future Stage-A target naming surfaces (finding 1).
- §3.4 zero V3 fake-pattern recurrence across six audited sub-properties.
- §4 finding 2: confirming-cycle discipline catches residual overfit that scope-substantive lenses miss; CH7's on-disk `ls` executable verification is the content-independent ground truth that enables this catch.
- §0.1 five-axis V3 zero-drift confirmation; V2 CH7 verdicts on P2-A + P2-B + P2-C + P2-D + P2-E carry forward verbatim.

No CH7 REJECT trigger fires; no S-P2 V3 revise required on the CH7 axis. V3 cycle ACCEPT. **CH7 3-cycle LOCK confirmed at V3** (V1 + V2 + V3 all ≥95% on the CH7 axis; cohort §3Z gate eligibility on the CH7 axis is open).
