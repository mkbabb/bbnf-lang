# SK-V18 S-P3 CHALLENGE — V9 / CH2 (SEQUENCING lens)

Lens: SEQUENCING. Is the wave-manifest order + entry-gates consistent with the S-P2 §3 lattice and
the §2 coupling lattice (P5-before-G1, P4-before-G2/G3, P3-before-G6/G2, G1-blocks-downstream, PROVE
on G3∧G4 PARALLEL to G5/G6)? Any wave dispatchable before its predecessor closes? Any unfalsifiable
gate / broken sequence / addenda violation?

Target re-grounded on disk this cycle: SPEC.md §2 manifest table (430–444), §2.1 lattice diagram
(538–547), §2.1 G-wave-consume-P-cluster block (795–798), per-section entry gates (G1 §4.1 821; G2
§5.1 935–942; G3 §6.1 1088–1097; G4 §7.1 1221; G5/G6 §8 1310–1319; PROVE §9 1427–1441; H1 §10 / table
444+546), the Downstream-BLOCKS restatement lines (902–905, 1064, 1185, 1290, 1407, 1528), the route
ledger §11 (1612–1625), and close-condition §0.1 (56–173). Cross-read against S-P2 §3 lattice
(`SYNTHESIS-RESEARCH.md:130–211`), §2 coupling lattice (62–122), and audit §5 standing order
(`SYNTHESIS-AUDIT-OVERFIT.md:181–224`). Prior cycles: V6 27A/0R, V7 18A/0R, V8 18A/1R — the V8 REVISE
is re-checked for fold below; it has NOT converged.

---

## A. The entry-gate DAG re-derived independently, then matched edge-for-edge

Direct-predecessor set per wave, taken verbatim from each section's entry gate AND cross-checked
against the three other locations (manifest table, §2.1 diagram, §2.1 consume-block):

| Wave | Direct predecessors (SPEC) | S-P2 §3 source | match |
|---|---|---|---|
| G1 | P-cluster closed (P4 live, P5 closed) | §3 line 135 | ✓ |
| G2 | G1 ∧ P3 closed ∧ P4 live (DUAL) | §3 line 136/167 | ✓ |
| G3 | G1 ∧ G2 closed ∧ P4 live ∧ P3 row-collapse | §3 line 137/176 | ✓ |
| G4 | G1 ∧ G2 ∧ G3 closed | §3 line 138/186 | ✓ |
| G5/G6 | P1 ∧ P3 ∧ G3 closed ∧ S-P1 94.1% leaf | §3 line 139/189 | ✓ |
| PROVE | G3 ∧ G4 closed (PARALLEL to G5/G6) | §3 line 140/200 | ✓ |
| H1 | G5/G6 ∧ PROVE both closed | §3 line 141/206 | ✓ |

Every direct-predecessor set is byte-faithful to S-P2 §3, and identical across the four SPEC
locations (manifest dispatch-status column, §2.1 diagram, §2.1 consume-block, per-section body). No
row's entry predicate differs across locations. **ACCEPT (×7, one per wave entry gate).**

The ONE place the SPEC departs from a literal S-P2 §3 picture — the §3 ASCII draws G5/G6 nested under
G4 (line 139 indents G5/G6 below the G4 line) — is the documented seq/C7 correction: the SPEC hangs
G5/G6 off G3 PARALLEL to G4 (§2.1 line 545, "G5/G6 needs only G3, NOT G4"). This makes the SPEC MORE
faithful to the S-P2 entry-PREDICATE (line 189: "entry = P1 ∧ P3 ∧ G3 closed", no G4 conjunct) than to
the S-P2 drawing. Folded consistently across the table (442), diagram (545), §8 body (1311), and the
GROUND `seq.md` C7 disposition (1648). **ACCEPT.**

## B. The six binding lens questions, each discharged

- **P5-before-G1?** Manifest G1 entry "P4 live, P5 closed" (438); §2.1 diagram (540); consume-block
  (795); §3.6 "P5 the BEFORE-G1 obligation" (1657); close §11 P5 row "fix at the SOURCE … before G1"
  (1618); GROUND `sota.md` folds the P5→G1 call-site sequencing (1644–1646). Explicit. **ACCEPT.**
- **P4-before-G2/G3?** §3.4 "MUST LAND BEFORE G2/G3" (690), framed as an entry-ON-G2/G3 obligation
  (705); G2 entry "P4 live" (942); G3 entry "P4 live … P4 MUST land BEFORE G3" (1091–1092); close §0.1
  clause 8 (140–141). Cross-cutting-predecessor framing correct. **ACCEPT.**
- **P3-before-G6/G2?** G2 dual-gates P3 "a P3 failure blocks G2 INDEPENDENT of G1" (935–940); G6 "P3
  closed … a P3 failure blocks G6 independent of G3" (1314–1316); §3.3 binding note (683–684); consume
  block (797). Both the G2 and G6 independent-P3-conjunct are present. **ACCEPT.**
- **G1-blocks-downstream?** The §4 Downstream line (902–905) names "G2, G3, G4, PROVE". The ENFORCEMENT
  is airtight (every G1 descendant entry-gates transitively through G3⊃G1 or directly), so the
  sequence is NOT broken. But the advisory restatement is asymmetric vs the G2/G3 lines — see REVISE-1.
  Sequence sound; restatement carries one residual.
- **PROVE on G3∧G4 PARALLEL to G5/G6?** PROVE entry "G3 ∧ G4 closed … PARALLEL to G5/G6 … NEVER admits
  before G4 closes" with G4 as the DIRECT (not transitive) conjunct (1427–1431); seq/C6 fold explicit;
  manifest "OVERLAPS G5/G6 but starts LATER — PROVE entry needs G4 closed … G5/G6 entry needs only G3"
  (443) — a refinement (PROVE's deeper gate makes it start later than G5/G6 in wall-clock), not a
  contradiction of PARALLEL. G5/G6 "does NOT block PROVE … PARALLEL" (1407); both join at H1 (546).
  seq/C6+C7 folded precisely across table, diagram, §8, §9. **ACCEPT (×2: the PROVE side and the
  G5/G6-parallel side).**
- **Any wave dispatchable before its predecessor closes?** No. The only "Dispatchable now" rows are the
  entry-gate-free P-cluster (P1/P2/P3/P5 independent; P4 independent-entry but hard BEFORE-G2/G3 exit
  obligation — 433–437, 556–557). Every G-wave/PROVE/H1 entry predicate is the conjunction of its
  predecessors' closes. Every TRANSITIVE predecessor is either a named in-gate conjunct or
  guaranteed-closed by a stronger named conjunct: G2-for-G6 via G3⊃G2; G1∧P3-for-PROVE via G4⊃G3⊃G1 and
  G3's P3-row-collapse conjunct; G4-for-H1 via PROVE⊃G4; G1-for-G6 via G3⊃G1. No transitive gap admits
  an early dispatch. **ACCEPT.**

## C. Producer/consumer + close-condition ordering (the telemetry-vs-wave axis)

- **Producer-only-field discipline.** §0.4 maps each supporting column to its consuming wave; each
  wave section closes "every emitted field consumed in the same wave". The lone deliberate cross-wave
  deferral `g6_speedup_median_mbps` is null-at-G6 and explicitly H1-consumed (442/482/1408). No column
  is emitted by one wave but gated only out-of-order downstream. **ACCEPT.**
- **Close-condition §0.1 (1–12) ordering.** Each close clause cites the wave that discharges it; none
  depends on an undischarged-at-its-point column. Clause 6's pre-G2-baseline CAPTURED-AT-G2-ENTRY +
  fire-at-G2-exit, with H1 re-confirming DIRECTIONALLY (never re-measuring the gone pre-G2 code), is the
  non-trivial measurement-ordering correctness point and it holds (108–113). **ACCEPT.**
- **Route ledger §11 (per-wave-attributed).** Each "must NOT re-open" row names the wave's OWN seam
  (G5/G6 "re-emitting the call site 7 ways (P3 re-fork)" — the P3→G6 coupling; PROVE "a `GoogleSheets
  =>` arm" — the import-closure-as-data sequencing). Per-wave-attributed by design, so it correctly does
  NOT restate full transitive closure; no row asserts a cross-wave dependency out of order. **ACCEPT.**

## D. Addenda-vs-sequencing placement

The 6 audit addenda map to the waves in S-P2's order and the SPEC places each addendum's gate AT OR
AFTER the wave that can satisfy it: addendum 1 (verbatim-blob) → G1+G2; addendum 2 (distinct-output,
the 3-co-gate conjunction) → P3 lands the structural-collapse half, G3 lands branch/type-count==0
("at and after G3", §2.1 line 491), PROVE lands the md5-distinct trio — correctly staged, never
asserting `branch_count==0` before G3 exists; addendum 3 (single-emitter) → G3; addendum 4
(phantom-generic) → G4; addendum 5 (timed-plane + corpus-in-timer) → P2 deletes the warm path, G6
defers any Mbps figure to H1's symmetric timer, H1 discloses framing; addendum 6 (accel-wiring) → G6,
gated on the S-P1 profile (a hard dependency placed BEFORE the G6 kernel, audit §5 fact 4). No
addendum's gate is asserted one wave too early; no addendum violation. **ACCEPT.**

## E. The one residual — the V8 REVISE re-checked for fold

V8/CH2 raised exactly one precision REVISE: the §4 G1 Downstream-BLOCKS line omits the transitive
G1→G6 edge that the G2 and G3 Downstream lines BOTH carry. I re-grounded the current SPEC: the line at
902–903 still reads **"Downstream: G1 REJECTION BLOCKS G2, G3, G4, PROVE"** — G6 absent. Meanwhile §5
line 1064 reads "G2 … BLOCKS G3, G4, **G6**, PROVE" and §6 line 1185 reads "G3 … BLOCKS G4, **G6**,
PROVE". The V8 REVISE was NOT folded into this SPEC; this axis has NOT reached a 2-consecutive-clean
fixed point.

The edge is genuine: G6 entry = P1 ∧ P3 ∧ G3 (1310); G3 entry = G1 ∧ G2 ∧ … (1088). So G1 is a
transitive predecessor of G6 exactly as G2 is — and G2's Downstream line names G6. On the ONE wave the
lens explicitly names ("G1-blocks-downstream"), an implementer reading the three Downstream lines
side-by-side sees G2 and G3 both block G6 but G1 (their apex predecessor) apparently does not — a
misleading internal asymmetry in the advisory restatement. (The omission of H1 is uniform across G1/G2/G3
and thus self-consistent — H1 is the universal sink; only the G6 omission is asymmetric, so only G6 is
the load-bearing fix.) This is materially clarifying for the implementer, hence a REVISE not a no-op nit.

It is NOT a REJECT: the ordering is fully enforced by entry predicates (G6 cannot dispatch until G3
closes, and G3 cannot close until G1 closes), so the sequence is not broken and no gate is
unfalsifiable — the defect is in the advisory prose only, precisely as V8 judged.

**REVISE-1.** §4 line 902–903 — change "**Downstream: G1 REJECTION BLOCKS G2, G3, G4, PROVE**" to
"**Downstream: G1 REJECTION BLOCKS G2, G3, G4, G6, PROVE**" (add `G6`, restoring symmetry with the
G2/G3 Downstream lines, since G1 is a transitive predecessor of G6 via G3 exactly as G2 is).

---

## Verdict

I re-derived the wave dependency partial order independently from S-P2 §2/§3 and audit §5 and checked
the SPEC's manifest table + §2.1 diagram + per-section entry gates + Downstream-BLOCKS restatement +
route ledger + close-condition §0.1 against it edge-for-edge. The manifest order and every ENTRY/EXIT
gate are consistent with the S-P2 lattice and coupling graph; the seq/C6 (PROVE never before G4; G4
DIRECT, not merely transitive) and seq/C7 (G5/G6 needs only G3, PARALLEL to G4, not under it)
corrections are folded precisely across the table, the diagram, and the §8/§9 bodies. No wave is
dispatchable before any predecessor — direct or transitive — closes; every transitive predecessor is a
named in-gate conjunct or guaranteed-closed by a stronger conjunct, and the enforcement (the entry
gates) is airtight. No unfalsifiable gate, no broken sequence, no addenda violation under the
sequencing lens.

ONE residual precision REVISE — the SAME one V8/CH2 raised and that was NOT folded into this SPEC: the
§4 G1 Downstream-BLOCKS line omits the transitive G1→G6 edge both the G2 and G3 lines carry, on the
exact wave the lens names ("G1-blocks-downstream"). A single-word add (`G6`) restores symmetry. Because
this finding persists unfolded from the immediately prior cycle, the sequencing axis is NOT at a
2-consecutive-clean fixed point; folding REVISE-1 (and re-confirming clean next cycle) reaches it. No
REJECT: the sequence is sound and the ordering is fully entry-gate-enforced regardless of the wording.

TALLY accept=20 revise=1 reject=0
