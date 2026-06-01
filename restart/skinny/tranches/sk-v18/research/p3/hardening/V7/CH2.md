# SK-V18 S-P3 CHALLENGE — CH2 SEQUENCING lens (cycle V7)

Lens: SEQUENCING. Question — is the SPEC wave-manifest order + per-wave entry/exit gates consistent
with the S-P2 lattice (`research/p2/SYNTHESIS-RESEARCH.md §2/§3`) and the addenda
(`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md §5`)? P5-before-G1, P4-before-G2/G3, P3-before-G6/G2,
G1-blocks-downstream, PROVE on G3∧G4 PARALLEL to G5/G6. Any wave dispatchable before its predecessor
closes? Any unfalsifiable gate / broken sequence / addenda violation? Every wave-gate / telemetry /
close claim under the lens judged ACCEPT / REVISE / REJECT.

Re-grounded INDEPENDENTLY this pass against: `SPEC.md` §0.1 (close-condition ordering), §0.4
(telemetry per-wave assignment), §2 (manifest table lines 430–441 + lattice diagram §2.1 lines
535–544), §3.1–§3.6 (P-cluster entry/exit + sequencing notes + the G-wave-consume-P-cluster block
lines 792–795), §4–§10 (G1/G2/G3/G4/G5-G6/PROVE/H1 entry/exit/downstream), §11 (route ledger), and
the close restatement; `SYNTHESIS-RESEARCH.md §2` (the coupling lattice 1–7) + `§3` (the binding
PRUNE→G1..G6→PROVE→H1 per-wave entry-gate enumeration); `SYNTHESIS-AUDIT-OVERFIT.md §5` (the
sequencing constraints + the entry-gate chain diagram). Prior posture: V6 CH2 = accept=27/0/0, the
manifest lattice STANDS, seq/C6+C7 folded. This V7 pass re-derives the dependency graph from scratch
(not by trusting V6) and hunts the RESIDUAL precision REVISE + any genuine REJECT; proportionate — a
wording nit is a REVISE only if it would mislead an implementer on dispatch order.

---

## A. Independent re-derivation of the dependency graph (the falsification attempt)

I reconstructed the partial order from the THREE binding sources and checked the SPEC against it,
rather than re-reading V6's enumeration.

S-P2 §3 entry-gates (the authority): G1←P-cluster(P4 live); G2←G1∧P3(P4 live); G3←G1∧G2(P4
live)∧P3-row-collapse; G4←G1∧G2∧G3; G5/G6←P1∧P3∧G3∧S-P1-profile; PROVE←G3∧G4 (transit. G1∧P3),
PARALLEL to G5/G6; H1←G5/G6∧PROVE. S-P2 §2 couplings: #1 G3⊃{G1,G2}; #2 G2⊃{G1,P3}; #3
G6⊃{P1,P3,R-B(=G2),G3}; #4 G4⊃{G1,G2,G3}; #5 PROVE⊃{G3(R-A),G4(R-D)}; #6 P4-before-G2/G3. Audit §5:
P4-before-G2/G3, G1-blocks-G2/G3/G4/PROVE, G3-blocks-PROVE, P3-dual-gates-G2.

SPEC §2.1 diagram (lines 535–544) + the per-section entry gates match this partial order edge-for-edge.
The ONE place the SPEC DIVERGES from a literal S-P2 source is by design: the S-P2 §3 ASCII tree (lines
138–140) draws G5/G6 indented UNDER G4, yet annotates its entry "P1∧P3∧G3" (NOT G4) — a transcription
defect. The SPEC §2.1 diagram corrects it (G5/G6 is a `└─` SIBLING of G4's `├─` off G3, lines 540–542),
and the GROUND re-validation (`seq.md` C7) folded this. The SPEC is MORE faithful to the S-P2 §3
entry-PREDICATE than the S-P2 §3 PICTURE. Correct divergence, documented. **Not a defect.**

### Transitive-predecessor closure check (where an early-dispatch gap would hide)

- **G6's transitive R-B(=G2) predecessor (S-P2 §2 #3).** G6 entry names only P1∧P3∧G3, NOT G2. But
  G3 entry ⊇ G2 (S-P2 §2 #1), so G3-closed ⟹ G2-closed; AND §5 line 1061 makes the edge explicit
  ("G2 REJECTION BLOCKS G3, G4, G6, PROVE"). No gap — G6 cannot dispatch with an open G2. **ACCEPT.**
- **PROVE's transitive G1∧P3 (S-P2 §3).** PROVE←G4←G3, and G3 entry ⊇ {G1, P3-row-collapse}; §9
  names "transitively G1 ∧ P3" (line 1432). No gap. **ACCEPT.**
- **H1's transitive G4 (via PROVE).** H1←PROVE←G4, so H1-dispatch ⟹ G4-closed; H1 needs nothing of
  G4 directly. No gap. **ACCEPT.**
- **G5/G6's transitive G2 (it needs the P3-collapsed singular scan G2 derives).** G5/G6←G3⊇G2, and
  the §8 entry text binds it concretely ("the retargeted call site MUST land into the P3-COLLAPSED
  single CSS scan … emitted by the single un-forked emitter", lines 1310–1313). No gap. **ACCEPT.**

No transitive predecessor is left to chance; every one is either a named in-gate conjunct or
guaranteed by a stronger named conjunct (G3⊃G2⊃G1; G4⊃G3⊃P3). **No wave is dispatchable before any
predecessor — direct or transitive — closes.**

---

## B. The five binding lens questions (each answered against the re-derived graph)

1. **P5-before-G1?** YES. Manifest G1 row "P4 live, P5 closed" (line 435); §3.5 "P5 closes first …
   G1 re-asserts the metalang-leak-zero" (lines 758–763); §4 G1.1 "P5 closed specifically" (line 819);
   the G-wave-consume block (line 792). Folded from `sota.md`'s P5↔G1 call-site finding
   (`json/generated.rs:841`/`:881` are the 91.5% leaf call sites the rename touches). **ACCEPT.**
2. **P4-before-G2/G3?** YES — framed as a hard EXIT obligation that is an entry-gate ON G2/G3
   ("an entry-gate ON G2/G3, not a preference", lines 701–704), restated §0.1.8, the manifest, §2.1,
   and the G3.1 conjunct ("P4 MUST land BEFORE G3"). Mirrors S-P2 §3 P4 bullet + audit §5 fact 2.
   **ACCEPT.**
3. **P3-before-G6/G2?** YES. P3 dual-gates G2 ("a P3 failure blocks G2 INDEPENDENT of G1", lines
   680–685, 932–936); P3 is a NAMED G6 entry conjunct ("a P3 failure blocks G6 independent of G3",
   line 1312; the singular-call-site falsifier "this is why G6 entry-gates on P3", line 1369). Matches
   S-P2 §2 #2/#3. **ACCEPT.**
4. **G1-blocks-downstream?** YES — "G1 REJECTION BLOCKS G2, G3, G4, PROVE" (line 899). Matches audit
   §5 fact 3. **ACCEPT.**
5. **PROVE on G3∧G4 PARALLEL to G5/G6?** YES. PROVE entry = explicit conjunction G3∧G4 with G4 a
   DIRECT (not merely transitive) predecessor (seq/C6, lines 1423–1432), because the Sheets value type
   instantiates the R-D trait; G5/G6 needs only G3 and runs PARALLEL (seq/C7); both join at H1. The
   "OVERLAPS G5/G6 but starts LATER" manifest phrasing (line 440) is a genuine refinement — PROVE
   wall-clock-starts at-or-after G5/G6 (it waits on G4←G3 where G5/G6 waits only on G3), yet both are
   concurrent branches off G3 — NOT a contradiction of "PARALLEL". **ACCEPT.**

**Addenda violation?** None. The 6 addenda place at-or-after the wave that can satisfy them: 1→G1+G2;
2 (the 3-co-gate conjunction) → P3 lands the structural-collapse half, `branch_count==0`/`type_count==0`
land at G3 ("at and after G3", §2.1 line 488), `generator_grammar_count==3` at PROVE; 3→G3; 4→G4;
5→P2(delete warm)+G6(checkasm-only pre-H1)+H1(corpus-in-timer figure); 6→G6 (profile-first, gated on
the S-P1 measurement). No addendum gate is asserted before its satisfying wave. **No violation.**

**Any unfalsifiable gate?** None under this lens. The two sequencing-aware gate constructions I
stress-tested both hold: (i) the P3 post-collapse md5 falsifier GROWS its witness set across waves —
pre-collapse self-glob over `css_l4_*` RED, post-collapse CROSS-grammar `{json,css_l4}` distinctness,
sheets joining the trio at PROVE (lines 656–664; §9 line 1454) — correctly avoiding the
single-file-no-pair vacuity; (ii) the CSS pre-G2 baseline is CAPTURED AT G2 ENTRY and the regression
falsifier FIRES at G2 exit, so H1 re-confirms DIRECTIONALLY and never re-measures pre-G2 code that no
longer exists post-G2 (§0.1.6, §0.2, G3.5 `g3_css_sota_ratio_held`, §10). Both are falsifiable and
measurement-order-correct.

---

## C. Cross-wave telemetry / close sequencing (the consume-in-producing-wave discipline)

- **g6_speedup deferred to H1 (§8 lines 1359–1360, 1384; §10 lines 1547–1548; §0.4 line 254).** The
  speedup CLAIM is sequenced strictly AFTER the correctness gate: G6 reports only checkasm PASS/FAIL
  pre-H1 (`g6_speedup_median_mbps` null, does NOT gate G5/G6), H1 produces the figure on the symmetric
  corpus-in-timer timer; G6 outcome is `C` until H1. Addendum 5 enforced AT H1 with the explicit
  "not one wave too late" justification. The `corpus_in_timer` column is correspondingly DEFERRED from
  G6 to H1 (§0.4 line 254). Clean cross-wave measurement-sequencing. **ACCEPT.**
- **`runtime_target_rows_collapsed` threaded P3→G2→G3 (lines 676–678, 931, 1107–1108, 1139–1141).**
  P3 lands the structural-collapse half; G2 re-asserts; the SAME R16 full-row `PartialEq` invariant
  must hold ACROSS the G3 `emitter`-field removal (G3.3 "must hold ACROSS the field removal"). One
  derive, three waves, in order. **ACCEPT.**
- **Producer-only-field discipline.** Every wave section closes with "every emitted column is consumed
  in the [wave] slice (no producer-only field)" and §0.4 maps each supporting column to its consuming
  wave. No column is emitted by one wave but gated only out-of-order downstream (the lone deliberate
  cross-wave deferral, `g6_speedup_median_mbps`, is null-at-G6 and explicitly H1-consumed). **ACCEPT.**
- **Close-condition §0.1 (1–12) ordering.** Each close clause cites the wave that discharges it; none
  depends on an undischarged-at-its-point column. Clause 6's pre-G2-baseline capture-at-G2-entry +
  fire-at-G2-exit is the non-trivial measurement-ordering correctness point and it holds. **ACCEPT.**

---

## D. Manifest-table vs diagram vs body internal consistency

The dispatch-status column (lines 430–441), the §2.1 lattice diagram (535–544), the
G-wave-consume-P-cluster block (792–795), and each §4–§10 entry-gate body state the SAME entry
predicate for every wave. I cross-checked all 12 rows; no row's entry predicate differs across the
four locations. Wave count = 12 (5 PRUNE + G1+G2+G3+G4 + G5/G6-as-one + PROVE + H1) is arithmetically
correct and at the skinny ceiling. The downstream-BLOCKS lines (G1→{G2,G3,G4,PROVE}; G2→{G3,G4,G6,PROVE};
G3→{G4,G6,PROVE}; G4→PROVE, NOT G6; G5/G6→NOT-PROVE; G5/G6∧PROVE→H1) form the exact transitive closure
of the entry-gate graph with no missing or spurious edge. **ACCEPT.**

---

## Verdict

I re-derived the wave dependency partial order independently from S-P2 §2/§3 and audit §5 and checked
the SPEC against it edge-for-edge. The manifest order and every entry/exit gate are consistent with
the S-P2 lattice and coupling graph; the one place the SPEC departs from a literal S-P2 source (G5/G6
drawn under G4 in the S-P2 §3 picture) is the documented seq/C7 correction making the SPEC MORE
faithful to the S-P2 entry-PREDICATE. The seq/C6 (PROVE never before G4; G4 DIRECT, not merely
transitive) and seq/C7 (G5/G6 needs only G3, PARALLEL to G4, does not block PROVE) corrections are
folded precisely and consistently across the manifest table, the §2.1 diagram, and the §8/§9 bodies.
No wave is dispatchable before any predecessor — direct or transitive — closes; every transitive
predecessor (G2 for G6; G1∧P3 for PROVE; G4 for H1; G2 for G5/G6) is either a named in-gate conjunct
or guaranteed-closed by a stronger named conjunct. No unfalsifiable gate (the P3 witness-set growth
and the pre-G2 capture-at-entry are both falsifiable and order-correct), no broken sequence, no
addenda violation under the sequencing lens. The "OVERLAPS but starts LATER" PROVE wording is a
refinement, not a contradiction. Twelve cross-location consistency checks plus four transitive-closure
checks all sound. Zero residual REVISE would materially clarify the dispatch order for an implementer
(this is V7 of a doc whose sequencing surface V6 already drove to 27/0/0; my from-scratch re-derivation
adds no new clarifying edit); zero REJECT. The sequencing lens is at its fixed point.

TALLY accept=18 revise=0 reject=0
