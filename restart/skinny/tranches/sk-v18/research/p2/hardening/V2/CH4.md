# SK-V18 S-P2 CHALLENGE — CH4 SEQUENCING-SOUNDNESS (cycle V2)

Lens: is the dependency lattice (SYNTHESIS §2/§3) correct/complete; any circular dependency or a
wave dispatchable before its predecessor closes; P4-before-G2/G3 explicit; per-wave falsifiers
actually falsifiable? Reviewer: orchestrator (infra dropped the sub-agent dispatch).
Read: SYNTHESIS-RESEARCH.md §2/§3 (post-fold) + audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md §5.

## Claims

### C1 [ACCEPT] — the chain is an acyclic DAG
PRUNE(P1..P5) → G1 → G2 → G3 → G4 → G5/G6 → PROVE → H1. Tracing every entry-gate: G1←P-cluster;
G2←G1∧P3∧P4; G3←G1∧G2∧P4∧P3; G4←G1∧G2∧G3; G5/G6←P1∧P3∧G3∧profile; PROVE←G3(transit G1∧P3∧G4);
H1←PROVE. No back-edge; no cycle. ACCEPT.

### C2 [ACCEPT] — P4-before-G2/G3 is explicit and load-bearing
§3 marks P4 "(the cross-cutting predecessor): no entry-gate, but MUST land BEFORE G2/G3" and the
G2/G3 entry predicates both carry "P4 live". The rationale (the Lock-14 gate must be meaningful when
the new emitter is authored) is sound; the falsifier (re-inject a JsonSink token → gate turns RED →
revert) is concretely falsifiable. ACCEPT.

### C3 [ACCEPT] — G2's dual entry-gate (G1 ∧ P3) is correctly stated
§3 G2 entry = "G1 ∧ P3 closed ∧ P4 live (DUAL gate — a P3 failure blocks G2 independent of G1)".
Correct: re-deriving the scan into 7 byte-identical files without P3 re-creates the replica overfit
(addendum 2). The coupling §2.2 grounds it. ACCEPT.

### C4 [ACCEPT] — the per-wave falsifiers are concrete and RED-able
Each wave's exit falsifier is an executable check (find …/src/x86_64 == 0; grep measure_mbps == 0;
md5-distinct + row-collapse; FORBIDDEN_GENERIC_TOKENS re-inject turns RED; byte-equivalence to
json_templates/; CSS_GENERATED_RS grep == 0; emitter_fork_present grep == 0; generated-caller census
non-empty). All are falsifiable, not assertions. ACCEPT.

### C5 [REVISE] — the §3 ASCII diagram's LINEAR nesting overstates PROVE's dependency on G5/G6
The §3 diagram nests `G5/G6 └─ PROVE`, implying PROVE waits for the NEON scan wave. But the entry
text correctly says "PROVE entry = G3 closed (transitively G1 ∧ P3 ∧ G4)" — PROVE depends on G3
(un-fork) + G4 (the trait the Sheets value type instantiates), NOT on G5/G6 (the CSS NEON, which
Sheets does not use). The Sheets emission can proceed in PARALLEL with G5/G6 once G4 closes. The
linear ASCII nesting is a false serialization that would needlessly block PROVE on the CSS
acceleration wave. EDIT (SYNTHESIS-RESEARCH §3 ascii ~133-136): branch PROVE off G4 (parallel to
G5/G6), not under G5/G6 — e.g. show G4 forking to {G5/G6} and {PROVE}, both feeding H1. The entry-gate
TEXT is already correct; only the diagram mis-serializes. REVISE (diagram precision; no dependency is
actually wrong, but the picture invites an over-strict schedule).

### C6 [ACCEPT] — hard caps + revert-dependency discipline present
§3 carries the standing dispatch-hard-cap defaults (research 20 / plan 15 / redress 30, "at 0.9N
commit, at N halt") and the binding "no wave dispatches over a REDRESSed predecessor". ACCEPT.

## Net
The lattice is acyclic and the entry-gate TEXT is sound; P4-before-G2/G3 and the dual G1∧P3 gate are
explicit; falsifiers are RED-able. One diagram-precision REVISE: the ASCII over-serializes PROVE under
G5/G6 when PROVE only needs G3∧G4 (parallelizable with the NEON wave).

TALLY accept=5 revise=1 reject=0
