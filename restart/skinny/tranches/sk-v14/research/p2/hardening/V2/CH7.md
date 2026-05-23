# S-P2 V2 — CH7 OVERFIT-PRUNE Lens

Lens: **CH7 Overfit-Prune** per `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7` (lines 62-87). V1 verdict (`research/p2/hardening/V1/CH7.md`): 6/6 ACCEPT, 100%, no REVISE. V2 disposition: re-lens the four amended axes (P2-B SHA pin, P2-C 3-demotion, P2-D 1-demotion, P2-F 6-sub-fold) against the five CH7 mandates; confirm zero drift on the two V1-LOCKED axes (P2-A + P2-E); audit-overlay binding preserved.

Mandate (verbatim from PASS-0 §CH7):

1. every new code added is grammar-derived (template + grammar metadata + emission command) — never hand-written under a `// @generated` header;
2. Lock 14 generic-crate compliance is preserved (no JSON/CSS/Sheets string literals, byte literals, function names, enum variants, or match arms in nominally-generic code);
3. every admit lands via a real parser/codegen/SIMD source change, measured against a strict-vs-strict comparator on the same plane, with a per-iteration equality oracle;
4. every "generated" output passes a round-trip test (delete + regen ⇒ byte-equivalent);
5. no SCAFFOLD-ONLY landing (research artefacts without source wiring) counts as an admit.

CH7 REJECT triggers immediate plan revise OR redress revert. CH7 cannot be carried as "acknowledged but not blocking".

Artefacts under review: 6 P2 axis files at V2 amended HEAD (commit `447a26b07`); per CHALLENGE-CONTEXT V2 §1: P2-A + P2-E V1-LOCKED (zero V2 edits); P2-B + P2-C + P2-D + P2-F amended via atomic micro-fold.

---

## §0 — Executable verification

### §0.1 — V1-LOCKED axis drift audit (mandatory per CHALLENGE-CONTEXT V2 §2)

```
$ git diff b3dbc5ca0..447a26b07 --stat -- \
    restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md \
    restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md
(empty output)
```

**Zero drift on P2-A + P2-E.** Both V1-LOCKED axes are byte-identical to the V1 commit `b3dbc5ca0`. CH7 V1 verdicts for these two artefacts (both ACCEPT) carry forward verbatim to V2 with no re-evaluation required.

### §0.2 — Grammar enumeration (CH7 V1 binding preserved)

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

**Count: 9.** Identical to V1; the Lock 14 + Lock 16 audit-overlay binding surface is unchanged. The 7 non-JSON-non-meta grammars (css_l4, css_pretty, google_sheets, ebnf, bnf, csv, math) remain the grammar-neutrality target set for the V2 amended axes.

### §0.3 — V2 amendment surface (4 files)

```
$ git show --stat 447a26b07
 .../research/p2/hardening/V2/CHALLENGE-CONTEXT.md  | 39 +++++++++++
 .../sk-v14/research/p2/p2b-dav1d-process.md        |  6 +-
 .../sk-v14/research/p2/p2c-arch-esoterica.md       | 39 ++++++++---
 .../sk-v14/research/p2/p2d-substrate-tape.md       | 31 ++++-----
 .../sk-v14/research/p2/p2f-grammar-neutral.md      | 75 +++++++++++++++-------
 5 files changed, 137 insertions(+), 53 deletions(-)
```

V2 amends 4 axis files net +84 lines; V2 dispatch context added (39 lines, scaffolding only). The four amended axes are the CH7 V2 re-lens target; P2-A + P2-E carry V1 verdicts forward.

### §0.4 — Per-mandate executable spot-checks

**Mandate (1) — no hand-written `// @generated` proposed:**

```
$ grep -rn "@generated\|GENERATED" restart/skinny/tranches/sk-v14/research/p2/p2{a,b,c,d,e,f}-*.md
(zero hits in candidate-admission contexts; one V1-carried hit at p2c-arch-esoterica.md:43 referencing "generated ASCII delimiter set" as the *mechanism*, not a hand-written header)
```

**Mandate (3) — scalar-ref discriminator for the P2-F C12 reframing-CH4-ACCEPT (the load-bearing V2 micro-fold):**

```
$ grep -n "scan_structurals_scalar" \
    skinny/crates/runtime/src/grammars/json/scan.rs
22:pub fn scan_structurals(input: &[u8]) -> StructuralIndex {
29:    scan_structurals_scalar(input)
32:pub fn scan_structurals_scalar(input: &[u8]) -> StructuralIndex {
39:    let index = scan_structurals_scalar(input);
280:    use super::{scan_structurals, scan_structurals_scalar, ScanBackend};
```

**`scan_structurals_scalar` exists at line 32.** P2-F §2.12 V2 (C12 reframing per CH4 §3 CF-1) cites this exact path:line verbatim — the C12 scalar-ref discriminator is met; C12 carries no Stage-A authoring gap. The V2 CH4 ACCEPT folds correctly.

**Mandate (3) — Stage-A sibling-shape authoring template existence (P2-F Fold-4 / Fold-5):**

```
$ ls -la skinny/crates/bbnf-simd/src/scalar/
bitmap_prefix_xor_64.rs        291  <-- sibling for bcax_64.rs (Fold-5)
byte_class_from_eq_set_64.rs  1596  <-- sibling for byte_context_64.rs (Fold-4)
bulk_emit_positions_64.rs
... etc

$ ls skinny/crates/bbnf-simd/src/scalar/byte_context_64.rs \
     skinny/crates/bbnf-simd/src/scalar/bcax_64.rs
(both files DO NOT EXIST at HEAD — confirming Stage-A authoring targets)
```

Both target paths (`byte_context_64.rs`, `bcax_64.rs`) do NOT exist yet at HEAD — confirming that P2-F V2 frames them as **Stage-A authoring targets** (Lock 16 same-commit discipline) rather than fake-completed primitives. The sibling templates `bitmap_prefix_xor_64.rs` + `byte_class_from_eq_set_64.rs` DO exist — providing the exact authoring shape the V2 fold cites. This is the CH7-affirmative pattern: paths cited as "to be authored at Stage-A under Lock 16 same-commit discipline," not as scaffolded-as-real.

```
$ head -5 skinny/crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs
#[inline]
pub fn bitmap_prefix_xor_64_scalar(mut mask: u64, carry_in: bool) -> u64 {
    mask ^= mask << 1;
    mask ^= mask << 2;
    mask ^= mask << 4;

$ head -5 skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs
//! Scalar reference for `BYTE_CLASS_FROM_EQ_SET_64` — the executable specification.
//! Contract: ...
```

Sibling-shape templates are bona fide executable scalar references — the authoring shape is concrete, not speculative.

**Mandate (3) — P2-E Gap 1 scalar reference (P2-F §2.Y cross-axis tracking cite):**

```
$ grep -n "scan_string_special_block_scalar" \
    skinny/crates/bbnf-simd/src/aarch64/string_block.rs
31:pub fn scan_string_special_block_scalar(
```

**Exists at line 31.** P2-F §2.Y cites `bbnf-simd/src/aarch64/string_block.rs:31` — executable-verified correct.

### §0.5 — Audit-overlay citation distribution (CH7 V1 §2 binding re-audited at V2)

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

**Total direct citations: 15.** Identical to V1 distribution (V1 = 15 direct). V2 demotions did NOT strip audit-overlay citations from the demoted rows — C-P2C-1 retains its PRUNE-2 citation in §2.X non-candidate inventory; C-P2D-3 retains the §1.6(b) "moves zero hot-leaf cycles" finding (audit-overlay-equivalent surface). Indirect bindings (p2b via Lock 14 v+1 amendment; p2d via V3 CH5 substrate-union verdict) likewise preserved. **Audit-overlay binding intact at V2.**

---

## §1 — Per-artefact CH7 disposition at V2

| Artefact | V1 verdict | V2 changes | V2 verdict |
|---|---|---|---|
| `p2a-sota-teardown.md` | ACCEPT | NONE (V1-LOCKED; zero diff per §0.1) | **ACCEPT (carry-forward; zero drift)** |
| `p2b-dav1d-process.md` | ACCEPT | SHA pinning at §5.1 (FFmpeg `08571418...` + dav1d `1718ff9a...`); strengthens mandate (3) verifiability (every external citation now has commit-pinned anchor + GitHub URL); no candidate added/demoted; admission process unchanged | **ACCEPT (strengthened)** |
| `p2c-arch-esoterica.md` | ACCEPT | C-P2C-1/6/7 demoted to §2.X non-candidate inventory; full technical content verbatim; disposition stamp ("Demoted V2: zero S-P1 hot-leaf antecedent at SK-V14; re-evaluate if F-V2-P1ABC-RERECORD surfaces antecedent"); identifiers retained as gap-note for cross-tranche reference | **ACCEPT (strengthens mandate (5))** |
| `p2d-substrate-tape.md` | ACCEPT | C-P2D-3 demoted to §1.6(d) substrate-side observation; full technical content verbatim; gap-note identifier preserved at §2; substrate-union math updated (3 active + 1 pre-blocked → 2 active + 1 demoted-to-§1.6(d) + 1 pre-blocked) | **ACCEPT (strengthens mandate (5))** |
| `p2e-parse-that-gaps.md` | ACCEPT | NONE (V1-LOCKED; zero diff per §0.1) | **ACCEPT (carry-forward; zero drift)** |
| `p2f-grammar-neutral.md` | ACCEPT | Fold-2: C8 DEMOTED to §2.X.1 non-candidate inventory (full row verbatim + re-promotion gate); Fold-3: C6/C7/C10/C12/C13 carry CH1 disposition stamps with C12 reframed CH4-ACCEPT per CF-1; Fold-4: C10 scalar-ref target `byte_context_64.rs`; Fold-5: C13 scalar-ref target `bcax_64.rs`; Fold-6: SKIPPED (default per `[no-deferrals]`); NF-CH6-3: C2 upgraded with P2-E Gap 6 three-way composition; NF-CH6-4: cross-axis tracking note §2.Y consolidates P2-A C2 + P2-E Gap 1/6 + P2-F C1+C2 to one primitive | **ACCEPT (strengthens mandates (3) and (5))** |

**V2 ACCEPT rate: 6/6 = 100%.** Composite cycle disposition: re-affirm V1 verdict + strengthen mandates (3) + (5) via demotions and Stage-A target naming.

---

## §2 — Mandate-by-mandate roll-up at V2

| CH7 mandate | V1 status | V2 status | V2 delta |
|---|---|---|---|
| (1) Grammar-derived code; no `// @generated` hand-write | PASS 6/6 | PASS 6/6 | Zero new `@generated` hand-write proposals; the only `@generated` reference in any V2 amendment is P2-D §3 verdict-table row update ("substrate primitives are already grammar-neutral by construction") which does not propose a hand-write. **Mandate (1) status unchanged.** |
| (2) Lock 14 v+1 generic-crate compliance | PASS 6/6 | PASS 6/6 | Demoted candidates do not introduce JSON-only divergence: C-P2C-1 demotion preserves its CSS L4 / PRUNE-2 binding; C-P2D-3 demotion preserves its HIGH grammar-neutral substrate field claim; C8 demotion preserves its `CommentMarkers` grammar-config schema. P2-F §2.Y cross-axis tracking note is itself a Lock 14 v+1 enforcement (prevents 3 axes from admitting 3 SIMD bodies for 1 grammar-neutral primitive). **Lock 14 v+1 strengthened.** |
| (3) Real source change + strict-vs-strict + per-iteration equality oracle | PASS 6/6 | PASS 6/6 | **STRENGTHENED.** P2-B SHA pinning provides commit-anchored upstream verifiability for the FFmpeg/dav1d strict comparators (mandate (3) "strict-vs-strict comparator" surface is now SHA-pinned). P2-F Fold-4 + Fold-5 name Stage-A scalar-ref target paths at sibling-shape locations (executable-verified §0.4); C12 reframing-CH4-ACCEPT cites a live scalar-ref at `scan.rs:32` (executable-verified §0.4). |
| (4) Round-trip test for `generated` output | PASS 6/6 | PASS 6/6 | No V2 amendment proposes a codegen hand-patch route; the repo invariant `tests/regen_parity.rs` family remains the binding. V1-LOCKED axes (P2-A + P2-E) carry forward. p2e's structural exemption from mandate (4) (parse-that has no codegen surface) remains. **Mandate (4) status unchanged.** |
| (5) No SCAFFOLD-ONLY landing | PASS 6/6 | PASS 6/6 | **STRENGTHENED.** V2 adds three more demotions (C-P2C-1, C-P2C-6, C-P2C-7 to §2.X; C-P2D-3 to §1.6(d); C8 to §2.X.1) — each is an explicit refusal-to-admit-scaffold-only. The V2 corpus now executes the anti-scaffold-admit pattern at **5 demotions in addition to V1's 7 P2-C demotions + 1 P2-D pre-block + 1 P2-F NEUTRAL-PENDING-CONSUMER**, totalling **13 explicit "not-admitted-without-same-wave-consumer" dispositions** across the 6 P2 artefacts. |

**Composite at V2: 6/6 ACCEPT, no REVISE, no REJECT.** Mandates (3) and (5) strengthened relative to V1; mandates (1), (2), (4) unchanged.

---

## §3 — V2 OVERFIT-PRUNE-specific audit (CHALLENGE-CONTEXT V2 §2 CH7 focus items)

The V2 challenge context specifies five overfit-prune focus items beyond the standing CH7 mandates. Each is re-lensed below:

### §3.1 — Demotions did not introduce overfit (P2-C C-P2C-1/6/7, P2-D C-P2D-3, P2-F C8 all demoted with full technical content preserved and gap-note identifier preservation)

**Verified.** Demotion mechanism is *retention with disposition stamp*, not deletion. Full technical content preserved verbatim:

- **C-P2C-1** (§2.X first row): full 8-column row preserved, plus disposition stamp appended.
- **C-P2C-6** (§2.X second row): full row preserved, plus disposition stamp appended.
- **C-P2C-7** (§2.X third row): full row preserved, plus disposition stamp appended.
- **C-P2D-3** (§1.6(d) substrate-side observation + §2 gap-note): full shape/scalar-ref/arch/grammar-neutrality content preserved in §1.6(d); §2 gap-note row points to §1.6(d) with re-elevation gate.
- **C8** (§2.X.1 non-candidate inventory): full row preserved verbatim plus re-promotion gate (CH1 + CH4 + CH6 joint condition).

**Overfit risk would be:** silent deletion that loses cross-tranche identifier mapping, OR scaffold-stripping the technical content so the row reads as "phantom completed work." Neither pattern present. The V2 demotion idiom is the textbook anti-overfit move: explicit non-admission with re-evaluation gate, identifier-preserving, content-preserving.

### §3.2 — Gap-note identifier preservation maintains cross-tranche stability per SK-V14 audit-overlay column discipline (no "fake @generated" / scaffold-as-load-bearing / gate-relabel pattern introduced)

**Verified.** The audit-overlay column discipline mandates per-row stable identifiers across tranches (SK-V13 → SK-V14 → SK-V15 cross-references); demotion-without-identifier-preservation would break the cross-tranche audit surface. V2 preserves:

- C-P2C-1 through C-P2C-7 identifier continuity at SK-V13 → SK-V14 V2 (per p2c-arch-esoterica.md:131-133 lineage anchor; preserved through demotion).
- C-P2D-3 identifier continuity at SK-V14 V1 → V2 (gap-note row in §2 + §1.6(d) cross-reference).
- C8 identifier continuity at SK-V13 → SK-V14 V1 → V2 (full V2 §2.X.1 inventory preserved; re-promotion gate names V_n future cycle).

**Fake-@generated / scaffold-as-load-bearing / gate-relabel pattern audit:**
- Zero `@generated` hand-write proposals across all V2 amendments (mandate (1) preservation; §0.4 grep).
- Zero load-bearing claims attributed to scaffolded-but-not-real surfaces (every cited path:line is either executable-verified-live at HEAD per §0.4, OR explicitly framed as Stage-A authoring target per Lock 16 same-commit discipline).
- Zero gate-relabel pattern (V2 does not relabel a V1 REVISE-pending gate as "ACCEPT-with-discharge"; the C12 reframing per CH4 §3 CF-1 is a *substantive* re-evaluation backed by executable scalar-ref evidence at `scan.rs:32`, not a gate-relabel).

### §3.3 — New scalar-ref Stage-A targets (P2-F C10 byte_context_64.rs, C13 bcax_64.rs) frame correctly as Stage-A authoring targets (not as fake-completed primitives)

**Verified per §0.4 spot-check.** Both target files do NOT exist at HEAD; both are named as Stage-A authoring targets to land same-commit with the SIMD body under Lock 16 same-commit discipline. The sibling-shape templates (`bitmap_prefix_xor_64.rs`, `byte_class_from_eq_set_64.rs`) DO exist and are bona fide executable scalar references — the V2 fold-4/5 prose explicitly names these siblings as the authoring shape ("sibling of existing `crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:1` shape", "sibling of existing `crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:1` shape").

**Overfit risk would be:** framing not-yet-existent paths as if they were live evidence (the "fake-completed" pattern). The V2 prose explicitly stamps these as "Stage-A authoring under same-wave Lock 16 same-commit discipline" — making the not-yet-existent status the *load-bearing* framing, not an oversight. This is CH7-affirmative.

### §3.4 — C8 demotion + Fold-6 SKIP is honest exclusion, not overfit-via-omission

**Verified.** Fold-6 is the C8 scalar-reference authoring step that V1 §3.4 conditional-gated on whether C8 ships. V2 chooses the `[no-deferrals]` default: demote C8, SKIP Fold-6. The honest-exclusion pattern is:

1. C8 row is **explicitly preserved** in §2.X.1 (not silently dropped).
2. Re-promotion gate is **explicitly named** (joint CH1 + CH4 + CH6 condition; F-V2-P1ABC-RERECORD JSON-side antecedent OR CSS L4 / BBNF-self / json-commented same-wave consumer commit).
3. Fold-6 SKIP is **explicitly documented** in CHALLENGE-CONTEXT V2 §1 + p2f §2.8 V2 disposition prose ("Fold-6 V2 scalar-reference authoring is SKIPPED per the V2 demotion; target placement `crates/parse-that/src/comment_skip.rs` per HARDENING-S-P2-V1-CONSOLIDATED §3.4 conditional gate, deferred indefinitely until same-wave consumer commits").
4. §3 verdict table row for C8 is updated (`~~**C8 comment-skip primitive (block + line)**~~ DEMOTED V2 | non-candidate (was NEUTRAL-PENDING-CONSUMER at V1) | ...`) — visibly retaining the row for cross-reference rather than removing it.
5. §3 partition summary updated correctly (`NEUTRAL-PENDING-CONSUMER | 0 | — (V1 had C8; demoted V2 per Fold-2 — see §2.X.1)` + new partition `Demoted to non-candidate inventory (V2) | 1 | C8 (see §2.X.1)`).
6. CSS L4 / Sheets / BBNF-self consumer-existence summary updated (V1: "14 of 14" → V2: "13 active candidates"); Lock 14 v+1 binding holds for every active V2 candidate.

**Overfit risk would be:** silent omission of C8 from the §3 partition table + consumer-existence summary while continuing to cite "14 candidates clear Lock 14 v+1" as the V1 prose did. V2 does not do this — the §3 prose is explicitly updated to "13 of 14 V1 candidates clear the Lock 14 v+1 admission gate at V2; C8 demoted to §2.X.1 non-candidate inventory." Mathematically honest.

### §3.5 — P2-F §2.Y cross-axis tracking note prevents naming-overfit (3 axes converging on same primitive under different names)

**Verified per §0.4 spot-check.** P2-A C2 `long_string_body_simd_scan`, P2-E Gap 1 `scan_string_special_block_sweep_64`, and P2-F C1+C2 (quote-aware classifier composition) all converge on the same underlying long-string-body SIMD scan primitive — grounded on the `unescape_string` direct rank-1 46.7% `unicode_escapes` hot-leaf evidence (P1-E §2.2). All three cite the same scalar-ref function family (`scan_string_special_block_scalar` at `string_block.rs:31` — executable-verified live).

The V2 §2.Y note is the **anti-naming-overfit binding**: it explicitly names the three convergent identifiers and binds S-P3 to produce ONE canonical primitive name + ONE canonical scalar-ref function, "rather than admitting three near-duplicates." Without this note, S-P3 could (under naming-overfit) admit three SIMD bodies for one primitive — a classic scaffolding-as-load-bearing pattern where the three axis files appear to contribute three primitives but actually contribute one.

The §2.Y note is the textbook anti-overfit, anti-paper-close consolidation gate. CH7-affirmative.

---

## §4 — New findings (V2-specific)

1. **Demotion-with-gap-note-preservation is the V2 corpus's executable anti-overfit signature.** The V2 micro-fold pattern is now visible across three artefacts (P2-C 3 demotions, P2-D 1 demotion, P2-F 1 demotion = 5 total V2 demotions on top of V1's 7 P2-C + 1 P2-D pre-block + 1 P2-F NEUTRAL-PENDING-CONSUMER baseline). Every V2 demotion preserves the candidate identifier as a gap-note for cross-tranche stability; every V2 demotion preserves the full technical content verbatim under a disposition stamp; every V2 demotion names an explicit re-promotion gate. This is the pattern V1 finding 1 named "anti-scaffold-admit pattern executed in vivo"; V2 generalises it from one artefact (p2c) to three (p2c, p2d, p2f).

2. **Stage-A target naming is overfit-asymmetric.** Stage-A targets (P2-F Fold-4 `byte_context_64.rs`, Fold-5 `bcax_64.rs`) are **not-yet-existent paths cited as authoring targets**, with sibling-shape templates explicitly named for shape inheritance. This is the OPPOSITE of the overfit pattern (citing paths that don't exist as if they were evidence) — the V2 prose explicitly stamps these as Stage-A under Lock 16 same-commit discipline, making the not-yet-existent status the load-bearing framing. CH7-affirmative; recommend S-P3 inherit this exact stamp pattern for any new primitive authoring target naming.

3. **P2-F §2.Y is the inter-axis anti-paper-close gate.** Three axes converging on one primitive could be a naming-overfit failure pattern; §2.Y converts it to a CH6-affirmative consolidation pin. The note's S-P3 binding ("must produce ONE canonical primitive name + ONE canonical scalar-ref function rather than admitting three near-duplicates") is the V2 corpus's strongest anti-paper-close enforcement surface — recommend the aggregator pin §2.Y as an exemplar for any future cross-axis convergence detection.

4. **P2-B SHA pinning strengthens mandate (3) without weakening any CH7 surface.** The pinning provides commit-anchored upstream verifiability for FFmpeg `085714182302333dd83dcb9c36cf828dc4eba929` + dav1d `1718ff9aded99f0a89f5c7940d6afb8948301e33`. The strict-vs-strict comparator surface (FFmpeg `checkasm.h:214-240` + dav1d `tests/checkasm/loopfilter.c:177-188`) is now SHA-pinned and GitHub-URL-pinned. No new candidate admitted; no CH7 surface weakened. Pure verifiability gain.

5. **V1-LOCKED axes (P2-A + P2-E) zero-drift confirmed; V1 CH7 verdicts carry forward verbatim.** The V1 commit `b3dbc5ca0e3ccf38df71a5e72be3d65a3068549b` and V2 commit `447a26b07` are byte-identical on p2a-sota-teardown.md and p2e-parse-that-gaps.md. No re-lensing needed for these two axes; the four V1 CH7 findings specific to them (anti-scaffold-admit pattern via p2c demotion, Lock 14 v+1 distributed enforcement via p2b, audit-overlay citation distribution asymmetry as benign, parse-that structural exemption from mandate (4)) all hold at V2.

---

## §5 — Disposition

**V2 CH7 verdict: 6/6 ACCEPT. ACCEPT-rate: 100%.** Same composite as V1; mandates (3) and (5) strengthened by V2 amendments; mandates (1), (2), (4) unchanged.

**Cycle disposition (§3Z gate evaluation):** V2 cycle achieves first ≥95% on the CH7 axis (100% ACCEPT, no REVISE, no REJECT). V2 → V3 trajectory: no V3 CH7 work expected unless P2-B SHA pin propagates to consolidated doc (CHALLENGE-CONTEXT V2 §2 carrying-forward correction) or P2-F C12 ACCEPT re-opens via CH4 V2 (out of CH7 scope). V2 → LOCK trajectory: CH7 axis admits V2 LOCK pending aggregator commit; CH7 is the second axis to be V2-LOCK-eligible after V1 LOCK on the V1-LOCKED axes (P2-A + P2-E) propagates forward.

Pinned for aggregator (`HARDENING-S-P2-V2-CONSOLIDATED.md`):

- §3.1 demotion-with-gap-note-preservation pattern as V2's executable anti-overfit signature (finding 1).
- §3.3 Stage-A target naming asymmetry as CH7-affirmative pattern recommended for S-P3 primitive authoring (finding 2).
- §3.5 / §4 finding 3 P2-F §2.Y cross-axis tracking note as exemplary anti-paper-close consolidation gate.
- §0.4 P2-B SHA pin strengthens mandate (3) verifiability surface (finding 4); pure gain.
- §0.1 V1-LOCKED zero-drift confirmation; V1 CH7 verdicts on P2-A + P2-E carry forward verbatim.

No CH7 REJECT trigger fires; no S-P2 V2 revise required on the CH7 axis. V2 cycle ACCEPT.
