# SK-V18 S-P3 CHALLENGE — CH2 SEQUENCING lens (cycle V8)

Lens: SEQUENCING. Question — is the SPEC wave-manifest order + per-wave entry/exit gates consistent
with the S-P2 lattice (`research/p2/SYNTHESIS-RESEARCH.md §2/§3`) and the addenda
(`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md §5`)? P5-before-G1, P4-before-G2/G3, P3-before-G6/G2,
G1-blocks-downstream, PROVE on G3∧G4 PARALLEL to G5/G6. Any wave dispatchable before its predecessor
closes? Any unfalsifiable gate / broken sequence / addenda violation? Every wave-gate / telemetry /
close claim under the lens judged ACCEPT / REVISE / REJECT.

Re-grounded INDEPENDENTLY this pass (not by trusting V6/V7) against: `SPEC.md` §0.1 (close-condition
ordering), §0.2/§0.4 (the pre-G2-baseline capture timing + per-wave telemetry assignment), §2 manifest
table (lines 433–443) + the §2.1 lattice diagram (lines 538–547) + the G-wave-consume-P-cluster block
(lines 795–798), §3.1–§3.6 (P-cluster entry/exit + the P3/P4/P5 sequencing notes), §4–§10
(G1/G2/G3/G4/G5-G6/PROVE/H1 entry + exit + the per-section Downstream-BLOCKS lines), §11 (route
ledger), and the close restatement; `SYNTHESIS-RESEARCH.md §2` (the coupling lattice 1–7) + `§3` (the
binding PRUNE→G1..G6→PROVE→H1 per-wave entry-gate enumeration); `SYNTHESIS-AUDIT-OVERFIT.md §5` (the
sequencing constraints + the entry-gate-chain diagram). Prior posture: V6 CH2 = 27/0/0, V7 CH2 = 18/0/0
(both asserting the manifest lattice STANDS and the downstream sets "form the exact transitive closure
… with no missing or spurious edge"). This V8 pass re-derives the partial order from scratch and hunts
the RESIDUAL precision REVISE the prior two cycles' "no missing edge" characterization may have papered
over, plus any genuine REJECT. Proportionate: a wording nit is a REVISE only if it would mislead an
implementer on dispatch order.

---

## A. Independent re-derivation of the partial order (the falsification attempt)

I reconstructed the entry-gate partial order from the THREE binding sources, then checked the SPEC's
manifest + diagram + per-section gates + the Downstream-BLOCKS restatement against it edge-for-edge.

S-P2 §3 entry-gates (authority): G1←P-cluster(P4 live); G2←G1∧P3(P4 live); G3←G1∧G2(P4
live)∧P3-row-collapse; G4←G1∧G2∧G3; G5/G6←P1∧P3∧G3∧S-P1-profile; PROVE←G3∧G4 (transit. G1∧P3),
PARALLEL to G5/G6; H1←G5/G6∧PROVE. S-P2 §2 couplings: #1 G3⊃{G1,G2}; #2 G2⊃{G1,P3}; #3
G6⊃{P1,P3,R-B(=G2),G3}; #4 G4⊃{G1,G2,G3}; #5 PROVE⊃{G3(R-A),G4(R-D)}; #6 P4-before-G2/G3; #7 R16
threads R-A/R-B/R-E. Audit §5 facts: P4-before-G2/G3; G1-blocks-{G2,G3,G4,PROVE}; G3-blocks-PROVE;
P3-dual-gates-G2; R16 binds to S-P3.

The SPEC §2.1 diagram (538–547) + every §4–§10 entry-gate body match this partial order edge-for-edge.
The single intentional departure from a literal S-P2 source is the S-P2 §3 ASCII tree drawing G5/G6
indented UNDER G4 while annotating its entry "P1∧P3∧G3" (NOT G4) — a transcription defect the SPEC
§2.1 diagram corrects (G5/G6 a `└─` SIBLING of G4's `├─` off G3) and the GROUND `seq.md` C7 folds. The
SPEC is MORE faithful to the S-P2 §3 entry-PREDICATE than the S-P2 §3 PICTURE. Documented divergence,
not a defect. **ACCEPT.**

---

## B. The five binding lens questions (each against the re-derived graph)

1. **P5-before-G1?** YES. Manifest G1 row "P4 live, P5 closed" (line 438); §3.5 "P5 closes first … G1
   re-asserts the metalang-leak-zero … on the G1-REGENERATED file" (lines 760–768); G1.1 "P5 closed
   specifically" (lines 822–828); the consume block (line 795–796). The `sota.md` P5↔G1 call-site
   finding (`json/generated.rs:841`/`:881` = the 91.5% leaf call sites the rename touches) is folded as
   a binding order with G1's identical-call-site byte-equivalence re-asserted on the regenerated file.
   **ACCEPT.**
2. **P4-before-G2/G3?** YES — framed as a hard EXIT obligation that IS an entry-gate ON G2/G3 ("an
   entry-gate ON G2/G3, not a preference", lines 704–707), restated §0.1.8 (lines 140–141), the
   manifest (line 436), §2.1, the G2.1 conjunct (line 942), and the G3.1 conjunct ("P4 MUST land BEFORE
   G3", lines 1091–1092). Mirrors S-P2 §3 P4 bullet + audit §5 fact 2. **ACCEPT.**
3. **P3-before-G6/G2?** YES. P3 dual-gates G2 ("a P3 failure blocks G2 INDEPENDENT of G1", lines
   683–685, 935–940); P3 is a NAMED G6 entry conjunct ("a P3 failure blocks G6 independent of G3", line
   1316; the singular-call-site falsifier "this is why G6 entry-gates on P3", line 1373). Matches S-P2
   §2 #2/#3. **ACCEPT.**
4. **G1-blocks-downstream?** Substantively YES, with ONE precision asymmetry (see §D / the lone
   REVISE): the G1 downstream-BLOCKS set is stated {G2, G3, G4, PROVE} (line 902–903) — it OMITS the
   transitive G1→G6 edge that G2's and G3's own downstream lines DO carry. The enforcement is sound (G6
   entry hard-requires G3, which hard-requires G1, so no early G6 dispatch is possible); only the
   advisory restatement is asymmetric. **REVISE** (one-word edit).
5. **PROVE on G3∧G4 PARALLEL to G5/G6?** YES. PROVE entry = explicit conjunction G3∧G4 with G4 a DIRECT
   (not merely transitive) predecessor (seq/C6, lines 1427–1436) because the Sheets value type
   instantiates the R-D trait; G5/G6 needs only G3 and runs PARALLEL (seq/C7, lines 1310–1312); both
   join at H1. The manifest "OVERLAPS G5/G6 but starts LATER" (line 443) is a genuine refinement —
   PROVE←G4←G3 is depth-2 from G3 while G5/G6←G3 is depth-1, so PROVE's earliest start is strictly after
   G5/G6's, yet both are concurrent branches off G3 — NOT a contradiction of "PARALLEL". **ACCEPT.**

**Addenda violation?** None. Each addendum's gate is placed at-or-after the wave that can satisfy it:
1→G1+G2; 2 (the 3-co-gate conjunction) → P3 lands the structural-collapse half,
`generator_grammar_branch_count==0`/`type_count==0` land at G3 ("at and after G3", §2.1 line 491),
`generator_grammar_count==3` at PROVE; 3→G3; 4→G4; 5→P2(delete warm)+G6(checkasm-only pre-H1, the
corpus-in-timer figure DEFERRED)+H1(corpus-in-timer); 6→G6 (profile-first, gated on the S-P1
measurement). No addendum gate is asserted before its satisfying wave. **No violation.**

**Any unfalsifiable gate / broken sequence?** None. The two sequencing-aware constructions I
stress-tested both hold and are measurement-order-correct: (i) the P3 post-collapse md5 falsifier GROWS
its witness set across waves — pre-collapse self-glob over `css_l4_*` RED, post-collapse CROSS-grammar
`{json,css_l4}` distinctness, sheets joining the trio at PROVE (lines 660–666; §9 line 1458–1459) —
correctly avoiding the single-file-no-pair vacuity; (ii) the CSS pre-G2 baseline is CAPTURED AT G2
ENTRY in one quiet run and the regression falsifier FIRES at G2 exit, so H1 re-confirms DIRECTIONALLY
and never re-measures pre-G2 code that is gone post-G2 (§0.1.6 lines 105–117, §0.2 line 181, G3.5
`g3_css_sota_ratio_held`, §10 lines 1557–1569). Both falsifiable, both order-correct. The G5/G6 entry
"the S-P1 94.1% hot-leaf measurement" is a present-artifact predicate (the SYNTHESIS-PROFILE artefact),
falsifiable by absence. **ACCEPT.**

---

## C. Transitive-predecessor closure (where an early-dispatch gap would hide)

- **G6's transitive R-B(=G2) predecessor (S-P2 §2 #3).** G6 entry names only P1∧P3∧G3, NOT G2. But G3
  entry ⊇ G2, so G3-closed ⟹ G2-closed; AND §5 line 1064 makes the edge explicit ("G2 REJECTION BLOCKS
  G3, G4, G6, PROVE"). No gap. **ACCEPT.**
- **PROVE's transitive G1∧P3 (S-P2 §3).** PROVE←G4←G3, and G3 entry ⊇ {G1, P3-row-collapse}; §9 names
  "transitively G1 ∧ P3" (line 1427/1436). No gap. **ACCEPT.**
- **H1's transitive G4 (via PROVE).** H1←PROVE←G4, so H1-dispatch ⟹ G4-closed; H1 needs nothing of G4
  directly (it consumes only G5/G6 ∧ PROVE telemetry, §10 lines 1540–1543). No gap. **ACCEPT.**
- **G5/G6's transitive G2 (it needs the P3-collapsed singular scan G2 derives).** G5/G6←G3⊇G2, and §8
  binds it concretely ("the retargeted call site MUST land into the P3-COLLAPSED single CSS scan …
  emitted by the single un-forked emitter", lines 1314–1317). No gap. **ACCEPT.**

Every transitive predecessor is either a named in-gate conjunct or guaranteed-closed by a stronger
named conjunct (G3⊃G2⊃G1; G4⊃G3⊃P3). **No wave is dispatchable before any predecessor — direct or
transitive — closes.** The G1→G6 asymmetry in §B.4/§D is an advisory-RESTATEMENT gap, NOT an
enforcement gap — it cannot admit an early dispatch.

---

## D. Manifest vs diagram vs body internal consistency (the Downstream-BLOCKS restatement)

The dispatch-status column (433–443), the §2.1 lattice (538–547), the G-wave-consume block (795–798),
and each §4–§10 entry-gate body state the SAME entry predicate for every wave; I cross-checked all 12
rows — no row's entry predicate differs across the four locations. Wave count = 12 (5 PRUNE + G1+G2+G3+G4
+ G5/G6-as-one + PROVE + H1) is arithmetically correct, at the skinny ceiling.

The per-section Downstream-BLOCKS lines, however, are NOT uniformly the exact transitive closure the
V6/V7 verdicts claimed ("no missing or spurious edge"). Enumerated:

- G1 (line 902–903): blocks **{G2, G3, G4, PROVE}** — **omits G6**.
- G2 (line 1064): blocks **{G3, G4, G6, PROVE}** — includes G6.
- G3 (line 1185): blocks **{G4, G6, PROVE}** — includes G6.
- G4 (line 1290–1292): blocks **{PROVE}**, explicitly NOT G6.
- G5/G6 (line 1407–1408): NOT PROVE; G5/G6 ∧ PROVE gate H1.
- PROVE (line 1528): PROVE ∧ G5/G6 gate H1.
- H1 (line 1605): on H1 close the campaign closes.

G6 ← G3 ← G2 ← G1 is a chain, so G1 transitively blocks G6 EXACTLY as G2 and G3 do. G2's and G3's
downstream lines carry their G6 edge; G1's does NOT. This is the lone internal asymmetry. It does NOT
mislead an implementer on dispatch order (G6 entry hard-requires G3, hard-requiring G1; the entry gate
itself is the enforcement, the Downstream lines are advisory). But it falsifies the V6/V7 "exact
transitive closure with no missing edge" assertion on the one wave whose blocking the lens question
explicitly names ("G1-blocks-downstream"), and a single-word edit restores cross-section symmetry.
This is the residual precision REVISE the prior two clean cycles glossed.

**REVISE (§4 G1 Downstream line, SPEC line 902–903):** change
`**Downstream: G1 REJECTION BLOCKS G2, G3, G4, PROVE**`
to
`**Downstream: G1 REJECTION BLOCKS G2, G3, G4, G6, PROVE**`
(restoring symmetry with the G2/G3 lines that carry their transitive G6 edge; the parenthetical
rationale "the un-forked emitter consumes G1's grammar-walk pattern …" already covers why every
downstream wave inherits G1 and needs no change).

All other Downstream-BLOCKS sets (G2, G3, G4, G5/G6, PROVE, H1) ARE the exact reverse-reachability of
the entry-gate graph — G4-NOT-G6 and G5/G6-NOT-PROVE are the seq/C7 sibling-independence, correct;
PROVE ∧ G5/G6 both-gate-H1 is the diagram join, correct. **ACCEPT** (those six).

---

## E. Cross-wave telemetry / close sequencing (consume-in-producing-wave discipline)

- **g6_speedup deferred to H1 (§8 lines 1359–1362, 1384; §10 lines 1551–1554; §0.4 line 255).** The
  speedup CLAIM is sequenced strictly AFTER the correctness gate: G6 reports only checkasm PASS/FAIL
  pre-H1 (`g6_speedup_median_mbps` null, does NOT gate G5/G6; outcome `C` until H1), H1 produces the
  figure on the symmetric corpus-in-timer timer; `corpus_in_timer` is correspondingly DEFERRED from G6
  to H1 (§0.4 line 255). Addendum 5 enforced AT H1 with the explicit "not one wave too late"
  justification. Clean cross-wave measurement-sequencing. **ACCEPT.**
- **`runtime_target_rows_collapsed` threaded P3→G2→G3 (lines 667–679, 940, 1093, 1139–1141 region).**
  P3 lands the structural-collapse half via the R16 full-row `PartialEq`; G2 re-asserts; the SAME
  invariant must hold ACROSS the G3 `emitter`-field removal. One derive, three waves, in order.
  **ACCEPT.**
- **Producer-only-field discipline.** §0.4 maps each supporting column to its consuming wave; each wave
  section closes "every emitted field is consumed in the same wave (no producer-only field)". The lone
  deliberate cross-wave deferral (`g6_speedup_median_mbps`) is null-at-G6 and explicitly H1-consumed.
  **ACCEPT.**
- **Close-condition §0.1 (1–12) ordering.** Each close clause cites the wave that discharges it; none
  depends on an undischarged-at-its-point column. Clause 6's pre-G2-baseline capture-at-G2-entry +
  fire-at-G2-exit is the non-trivial measurement-ordering correctness point and it holds. **ACCEPT.**
- **Route ledger §11 + close restatement.** Per-wave-attributed; each "must NOT re-open" row names the
  wave's own seam (e.g. G5/G6 "re-emitting the call site 7 ways (P3 re-fork)" — the P3→G6 coupling;
  PROVE "a `GoogleSheets =>` arm" — the import-closure-as-data sequencing). No row asserts a
  cross-wave dependency out of order. **ACCEPT.**

---

## Verdict

I re-derived the wave dependency partial order independently from S-P2 §2/§3 and audit §5 and checked
the SPEC's manifest + diagram + per-section gates + Downstream-BLOCKS restatement against it
edge-for-edge. The manifest order and every ENTRY/EXIT gate are consistent with the S-P2 lattice and
coupling graph; the one departure from a literal S-P2 source (G5/G6 drawn under G4 in the S-P2 §3
picture) is the documented seq/C7 correction making the SPEC MORE faithful to the entry-PREDICATE. The
seq/C6 (PROVE never before G4; G4 DIRECT, not merely transitive) and seq/C7 (G5/G6 needs only G3,
PARALLEL to G4, does not block PROVE) corrections are folded precisely across the table, the §2.1
diagram, and the §8/§9 bodies. No wave is dispatchable before any predecessor — direct or transitive —
closes; every transitive predecessor (G2 for G6; G1∧P3 for PROVE; G4 for H1; G2 for G5/G6) is either a
named in-gate conjunct or guaranteed-closed by a stronger named conjunct, and the ENFORCEMENT (the
entry gates) is airtight. No unfalsifiable gate (the P3 witness-set growth and the pre-G2
capture-at-entry are both falsifiable and order-correct), no broken sequence, no addenda violation.

ONE residual precision REVISE survives the two prior clean cycles: the §4 G1 Downstream-BLOCKS line
omits the transitive G1→G6 edge that the G2 and G3 Downstream lines BOTH carry — an internal asymmetry
in the advisory restatement (not an enforcement gap), on the one wave the lens question explicitly
names ("G1-blocks-downstream"), where the V6/V7 "exact transitive closure, no missing edge" assertion
is technically inaccurate; a single-word edit (add `G6`) restores symmetry. No REJECT: the sequence is
not broken and the gate is fully enforced by entry predicates regardless of the wording.

TALLY accept=18 revise=1 reject=0
