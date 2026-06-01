# SK-V18 S-P3 CHALLENGE — CH2 SEQUENCING lens (cycle V6)

Lens: SEQUENCING. Question — is the SPEC wave-manifest order + per-wave entry/exit gates consistent
with the S-P2 lattice (`research/p2/SYNTHESIS-RESEARCH.md §3`) and the addenda
(`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md §5`)? P5-before-G1, P4-before-G2/G3, P3-before-G6/G2,
G1-blocks-downstream, PROVE on G3∧G4 PARALLEL to G5/G6. Any wave dispatchable before its predecessor
closes? Every wave-gate/telemetry/close claim under the lens judged ACCEPT / REVISE / REJECT.

Re-grounded against: `SPEC.md` §0.1 (close), §2 (manifest table + lattice diagram §2.1),
§3.1–§3.6 (P-cluster entry/exit + sequencing notes), §4–§10 (G1/G2/G3/G4/G5-G6/PROVE/H1 entry/exit),
§11 (route ledger); `SYNTHESIS-RESEARCH.md §2` (coupling lattice) + `§3` (binding sequencing);
`SYNTHESIS-AUDIT-OVERFIT.md §5` (sequencing constraints). Prior posture: V1–V5 drove the manifest
monotonically tighter, zero REJECT, lattice STANDS (`HARDENING-S-P3-CONSOLIDATED.md §1/§2`); seq/C6+C7
folded. This cycle hunts residual precision REVISEs and any genuine REJECT.

---

## Enumerated sequencing claims under the lens (each ACCEPT / REVISE / REJECT)

### A. The lattice diagram + manifest table (SPEC §2 / §2.1)

1. **Lattice diagram §2.1 (lines 535–544)** — `P-cluster → G1 → G2 → G3 → {G4→PROVE | G5/G6} → H1`,
   with PROVE & G5/G6 as two parallel branches off G3 joining at H1. Matches S-P2 §3's tree exactly
   (P5-before-G1; P4-live-before-G2/G3; P3 dual-gates G2 + binds G3; G4 under G3; PROVE under G4;
   G5/G6 off G3 PARALLEL to G4/PROVE; H1 joins). The seq/C6 (PROVE never before G4) + C7 (G5/G6 needs
   only G3, hangs off G3 not under G4) corrections are folded verbatim. **ACCEPT.**

2. **Manifest table dispatch-status column (lines 430–441).** P1/P2/P3/P4/P5 "Dispatchable now";
   G1 "Conditional on P-cluster close (P4 live, P5 closed)"; G2 "G1 ∧ P3 close (P4 live)"; G3
   "G1 ∧ G2 close ∧ P4 live ∧ P3 row-collapse"; G4 "G1 ∧ G2 ∧ G3 close"; G5/G6 "P1 ∧ P3 ∧ G3 close ∧
   S-P1 measurement (PARALLEL to G4/PROVE)"; PROVE "G3 ∧ G4 close … starts LATER … NEVER dispatch
   PROVE before G4 closes"; H1 "G5/G6 ∧ PROVE close". Every entry predicate equals its S-P2 §3 row.
   No wave is marked dispatchable before its predecessor closes. **ACCEPT.**

3. **Wave count = 12 (line 443)** — 5 PRUNE + G1+G2+G3+G4 + G5/G6 (ONE wave) + PROVE + H1. Arithmetic
   correct; G5/G6 collapsed to one wave per the lattice. **ACCEPT.**

### B. PRUNE intra-cluster sequencing (SPEC §3)

4. **P1–P5 mutual independence (lines 553–559).** "P1, P2, P3, P5 have NO entry-gate … dispatchable
   as soon as the W-PRUNE triumvirate is dispatched"; disjoint file roots enumerated; P3/P4 commit
   serially only if they touch the same xtask file. Matches S-P2 §3 ("P1..P5 … no entry-gate").
   **ACCEPT.**

5. **P4-before-G2/G3 hard exit obligation (lines 701–704, 723–727; restated §0.1.8, §2.1).** "P4 has
   no entry-gate but a hard EXIT obligation: it MUST land BEFORE G2/G3" — framed correctly as an
   entry-gate ON G2/G3 (P4-live), not a preference. Mirrors S-P2 §3 P4 bullet + audit §5 fact 2.
   **ACCEPT.**

6. **P5-before-G1 (lines 751–760).** The GROUND `sota.md` P5↔G1 call-site finding folded: P5 closes
   first; G1 re-asserts metalang-leak-zero + hot-leaf preservation on the G1-REGENERATED file (not a
   stale one); the P5 rename touches `json/generated.rs:841`/`:881` (the 91.5% leaf call sites). The
   ordering is explicit and consistent with S-P2 §3.5. **ACCEPT.**

7. **P3 dual-gate-of-G2 + binds-G3 (lines 680–685).** "P3 is a dual-gate predecessor of G2 — G2
   entry-gates on BOTH G1 AND P3 (a P3 failure blocks G2 independent of G1)" + "P3 also binds to G3
   (the relocated-seam structural check IS the G3 un-fork's third close-gate surface)". Matches S-P2
   §2 coupling #2 + audit §5 fact 3. **ACCEPT.**

8. **P3 post-collapse md5 unfalsifiability fix (lines 656–664).** The two-phase md5 falsifier (pre-
   collapse self-glob RED; post-collapse CROSS-GRAMMAR `{json,css_l4}` distinctness, sheets joining at
   PROVE) correctly avoids the single-file-no-pair unfalsifiable check. This is a sequencing-aware
   gate construction (the witness set GROWS as PROVE lands the third grammar). Sound and falsifiable.
   **ACCEPT.**

9. **P1↔P4 x86-exclusion ordering (lines 728–729).** "commit P1 first OR same-wave so the dropped
   exclusion does not dangle on a still-present tree". A real intra-cluster ordering hazard correctly
   pinned. **ACCEPT.**

### C. G-wave entry gates (SPEC §4–§9)

10. **G1 entry = P-cluster closed, P4 live, P5 closed (§4, lines 812–820).** Adds P5-closed explicitly
    over S-P2 §3's "P-cluster closed (P4 live)" — a tightening, not a divergence. **ACCEPT.**

11. **G1 blocks downstream (line 893).** "G1 REJECTION BLOCKS G2, G3, G4, PROVE". Matches audit §5
    fact 3 ("G1 failure BLOCKS G2/G3/G4/PROVE"). **ACCEPT.**

12. **G2 dual entry gate (§5, lines 926–933).** "G2 dual-gates; a P3 failure blocks G2 INDEPENDENT of
    G1" + G1-closed + P3-closed + P4-live. Matches S-P2 §3 G2 row exactly. **ACCEPT.**

13. **G2 blocks G3/G4/G6/PROVE (line 1055).** "G2 REJECTION BLOCKS G3, G4, G6, PROVE". Critically,
    this is where the S-P2 §2 coupling #3 transitive predecessor "G6 entry-gates … on R-B" is
    discharged: G6's own entry gate names P1∧P3∧G3 (not G2), but since G3⊃G2, G2 is transitively
    closed before G6 dispatches, AND this downstream-blocker line makes the G2→G6 block explicit. No
    dispatchability gap. **ACCEPT.**

14. **G3 entry = G1 ∧ G2 closed ∧ P4 live ∧ P3 row-collapse (§6, lines 1079–1088).** 4-conjunct gate;
    each conjunct's rationale (DERIVED JSON body / non-const CSS input / neutrality-scan-as-authored /
    pre-enforced row invariant) is correct and matches S-P2 §3 G3 row. **ACCEPT.**

15. **G3 blocks G4/G6/PROVE (line 1176).** Matches audit §5 fact 3 (G3 un-fork failure BLOCKS PROVE).
    **ACCEPT.**

16. **G4 entry = G1 ∧ G2 ∧ G3 closed (§7, lines 1211–1217).** 3-conjunct; the rationale "a trait over
    two forked emitters is a trait over two substrates = LCD-flatten REJECT" correctly motivates the
    G3-closed conjunct. Matches S-P2 §2 coupling #4. **ACCEPT.**

17. **G4 does NOT block G6, blocks PROVE (lines 1280–1283).** "G4 REJECTION BLOCKS PROVE … G4 does
    NOT block G6 (G6 wires the NEON, independent of the value-API trait; G5/G6 hangs off G3, parallel
    to G4)". This is the seq/C7 correction applied precisely — G4 and G6 are independent siblings off
    G3. **ACCEPT.**

18. **G5/G6 entry = P1 ∧ P3 ∧ G3 closed ∧ S-P1 measurement, PARALLEL to G4/PROVE (§8, lines
    1300–1314).** Each conjunct grounded: P1 (single-arch kernel surface), P3 (singular collapsed
    scan — "a P3 failure blocks G6 independent of G3"), G3 (call site emitted by the un-forked
    emitter), S-P1 (the WIRE-branch profile mandate). Matches S-P2 §3 G5/G6 row + coupling #3.
    **ACCEPT.**

19. **G5/G6 does NOT block PROVE (line 1394).** "G5/G6 does NOT block PROVE (Sheets does not use the
    CSS NEON — PARALLEL). G5/G6 ∧ PROVE both gate H1." Matches S-P2 §3 (PROVE "PARALLEL to G5/G6 —
    Sheets does not use the CSS NEON"). **ACCEPT.**

20. **PROVE entry = G3 ∧ G4 closed (transitively G1 ∧ P3), PARALLEL to G5/G6, NEVER before G4 (§9,
    lines 1414–1423).** The seq/C6 correction explicit: "G4 is PROVE's DIRECT predecessor, NOT merely
    a transitive one … the §3 'transitively' phrasing that mis-scoped onto G4 is corrected here". G4
    is a DIRECT conjunct because the Sheets value type instantiates the R-D trait. Matches S-P2 §2
    coupling #5 + §3 PROVE row. **ACCEPT.**

21. **PROVE ∧ G5/G6 both gate H1 (lines 1394, 1514, 1526).** The two parallel branches join at H1;
    H1 entry = G5/G6 ∧ PROVE closed. Matches S-P2 §3 H1 row + the diagram join. **ACCEPT.**

22. **H1 is the last wave; no downstream (lines 1524, 1587).** Consistent; on H1 close the SK-V18
    generalization closes. **ACCEPT.**

### D. Telemetry-consumption sequencing + close (SPEC §3.6, §2.1, §0.1)

23. **P-cluster telemetry consumed in-slice (line 777).** "Every emitted P-cluster column is consumed
    in its named P-wave slice (no producer-only field)" — the consume-in-producing-wave discipline;
    each G-wave section repeats it. No column emitted by one wave but only gated downstream out of
    order. **ACCEPT.**

24. **G-wave entry-gates that consume the P-cluster (lines 786–789).** Re-states G1/G2/G3/G5-G6 P-
    consumption identically to the table + diagram. Internally consistent. **ACCEPT.**

25. **`runtime_target_rows_collapsed` co-gate timing across P3→G2→G3 (lines 676–678, 931, 1101–1104,
    1133–1135).** P3 lands the structural-collapse half; G2 re-asserts it; the invariant must hold
    ACROSS the G3 `emitter`-field removal. The G3 exit conjunct 4 + the P3-conjunct-re-assertion
    correctly thread the SAME R16 derive through three waves in order. **ACCEPT.**

26. **g6_speedup deferred to H1 (§8 lines 1348–1351, 1375; §10 lines 1538, 1571; §0.2 lines 202–204).**
    The speedup CLAIM is sequenced AFTER the correctness gate: G6 reports checkasm PASS/FAIL pre-H1
    (`g6_speedup_median_mbps` null), H1 produces the figure on the symmetric timer. The G6 outcome is
    `C` until H1 — a clean cross-wave measurement-sequencing rule (addendum 5 enforced at H1, "not one
    wave too late"). Matches S-P2 §3 G5/G6 timed-plane binding. **ACCEPT.**

27. **Close condition (§0.1 1–12) ordering.** Each close clause cites the wave that discharges it; no
    close clause depends on an undischarged-at-its-point telemetry column. The CSS pre-G2 baseline is
    CAPTURED AT G2 ENTRY and the regression falsifier FIRES at G2 exit (clauses 6, §0.2) — H1 only
    re-confirms directionally, correctly avoiding a re-measure of pre-G2 code that no longer exists
    post-G2. This is a non-trivial measurement-ordering correctness point and it is sound. **ACCEPT.**

---

## Cross-checks for the binding lens questions

- **P5-before-G1?** Yes — table + §3.5 + §4 entry, explicit. ✓
- **P4-before-G2/G3?** Yes — hard EXIT obligation framed as entry-on-G2/G3. ✓
- **P3-before-G6/G2?** Yes — P3 dual-gates G2; P3 is a G6 entry conjunct ("a P3 failure blocks G6
  independent of G3"). ✓
- **G1-blocks-downstream?** Yes — "G1 REJECTION BLOCKS G2, G3, G4, PROVE". ✓
- **PROVE on G3∧G4 PARALLEL to G5/G6?** Yes — G4 a DIRECT conjunct (seq/C6); G5/G6 off G3 PARALLEL
  (seq/C7); both join at H1. ✓
- **Any wave dispatchable before its predecessor closes?** No. Every entry predicate is the
  conjunction of its predecessors' closes; the only "Dispatchable now" waves are the entry-gate-free
  P-cluster. The transitive predecessors (G2 for G6; G1/P3 for PROVE) are either named in-gate or
  guaranteed-closed by a named conjunct (G3⊃G2; G4⊃G3⊃G2⊃G1; G3⊃P3 via G3's P3-row-collapse
  conjunct), so no transitive gap admits an early dispatch. ✓
- **Addenda violation?** None. The 6 addenda map to the waves in S-P2's order (1→G1+G2, 2→P3+G3+PROVE,
  3→G3, 4→G4, 5→P2+G6+H1, 6→G6); the sequencing places each addendum's gate at or after the wave that
  can satisfy it (e.g. addendum-2 conjuncts `branch_count==0`/`type_count==0` are G3's exit, asserted
  "at and after G3" per §2.1 line 488 — P3 lands only the structural-collapse half, correctly). ✓

---

## Verdict

The wave manifest order and every entry/exit gate are consistent with the S-P2 §3 lattice and the
S-P2 §2 coupling lattice; the seq/C6 (PROVE never before G4; G4 DIRECT not transitive) and seq/C7
(G5/G6 needs only G3, PARALLEL to G4, does not block PROVE) corrections are folded precisely. No wave
is dispatchable before its predecessor closes; no transitive-predecessor gap admits an early
dispatch; no unfalsifiable gate, no broken sequence, no addenda violation under the sequencing lens.
The post-collapse md5 falsifier's witness-set growth across P3→PROVE and the pre-G2-baseline capture-
at-G2-entry are non-trivial sequencing-correct constructions. Twenty-seven enumerated claims, all
sound. Zero residual REVISE materially clarifies the order for an implementer; zero REJECT.

TALLY accept=27 revise=0 reject=0
