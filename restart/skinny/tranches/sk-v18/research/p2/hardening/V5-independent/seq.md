# SK-V18 S-P3 V5-independent CHALLENGE — SEQUENCING-SOUNDNESS (clean re-validation)

Lens: SEQUENCING-SOUNDNESS. Is the COMMITTED S-P2 synthesis lattice
(`restart/skinny/tranches/sk-v18/research/p2/SYNTHESIS-RESEARCH.md` §2/§3) ACYCLIC and COMPLETE
(P4-before-G2/G3, P3-before-G6/G2, G1-blocks-downstream, PROVE on G3∧G4 parallel to G5/G6)? Any
circular dependency, or a wave dispatchable before its predecessor closes? Are the per-wave
falsifiers RED-able?

This is the CLEAN INDEPENDENT re-validation. The prior V2 CH4 (sequencing) was orchestrator-applied
under infra load (the sub-agent dispatch dropped twice); its one REVISE (C5: PROVE over-nested under
G5/G6) was folded into the synthesis diagram. This pass re-traces the lattice from disk, re-grounds
the cited symbols, and re-challenges every sequencing claim WITHOUT deferring to the prior verdict.
It surfaces ONE residual the V2 fold left half-resolved (the diagram's G5/G6 over-nesting under G4 —
the sibling-branch mirror of the C5 fix) and ONE internal inconsistency the V4 readiness sweep noted
as a "wording slip" but did not edit (the §3 per-wave PROVE gate mis-scopes "transitively" onto G4,
its DIRECT predecessor). Cited disk anchors re-verified at the live tree (`regen.rs:5-18`,
`sink_only.rs:27/48/135`, `RuntimeEmitterKind` in `grammar_provider.rs`/`lib.rs`/`runtime_generator.rs`).

---

## Claims under the SEQUENCING-SOUNDNESS lens

### C1 [ACCEPT] — the lattice is an acyclic DAG; no back-edge across any wave
Tracing every entry-gate (§3 per-wave, the binding dispatch predicate): G1←P-cluster closed;
G2←G1∧P3∧P4-live; G3←G1∧G2∧P4-live∧P3(row-collapse); G4←G1∧G2∧G3; G5/G6←P1∧P3∧G3∧S-P1-profile;
PROVE←G3∧G4 (transit. G1∧P3); H1←G5/G6∧PROVE. Drawing the precedence edges
{P→G1, G1→G2, G2→G3, G3→G4, G3→G5/G6, G3→PROVE, G4→PROVE, G5/G6→H1, PROVE→H1} yields a strict
partial order: every edge points from a lower topological rank to a higher one, the unique source is
the P-cluster, the unique sink is H1. No edge runs backward; no node reaches itself. Acyclic. ACCEPT.

### C2 [ACCEPT] — the lattice is COMPLETE: every recommended candidate maps to exactly one wave, every wave is reachable, H1 is the sink
§1's six recommendeds map one-to-one onto behavior waves with no orphan and no gap:
R-C→G1, R-B→G2, R-A→G3, R-D→G4, R-F→G5/G6, R-E-2→PROVE; the five PRUNEs (P1..P5) precede G1; H1 is
the honesty/regen sink. Every wave is reachable from the P-cluster along the precedence edges of C1,
and H1 dominates both terminal behavior branches (G5/G6 and PROVE). No candidate is stranded; no wave
lacks a predecessor path or a successor path to H1. Complete. ACCEPT.

### C3 [ACCEPT] — P4-before-G2/G3 is explicit, grounded, and RED-able
§3 marks P4 "(the cross-cutting predecessor): no entry-gate, but MUST land BEFORE G2/G3"; the G2/G3
entry predicates both carry "P4 live" (lines 136-137, 167, 176). The rationale is load-bearing — the
Lock-14 gate must be operational WHEN the un-forked emitter is authored, else a grammar-named branch
re-enters undetected. The falsifier ("re-inject a `JsonSink` token → gate turns RED, revert";
`lock14_gate_scans_codegen == true`; `FORBIDDEN_GENERIC_TOKENS ⊇ {CSS_,_RS,EventGrammar,*EventGrammar}`)
is concretely RED-able. Note the phrasing is "P4 LIVE" not "P4 closed" — correct and intentional: P4
is a standing gate that must remain operational across G1/G2/G3 authoring, not a one-shot close. ACCEPT.

### C4 [ACCEPT] — P3-before-G2/G6 holds, and is doubly-enforced (subsumed by the G1 cluster-gate + explicitly restated)
G1's entry is "P-cluster closed" (lines 135, 160) — the WHOLE P1..P5 cluster, so P3 (and P1, P4) are
necessarily closed before G1, hence transitively before G2/G3/G4/G5-G6/PROVE/H1. The synthesis ALSO
restates P3 explicitly in G2's dual gate (line 136/167, "a P3 failure blocks G2 independent of G1")
and in G5/G6 (line 139/189, "P1 ∧ P3 ∧ G3"). This is defense-in-depth, not a contradiction: the
cluster-gate is the floor, the per-wave restatement names the specific replica-overfit (addendum 2)
each wave would re-create absent P3. The G2 dual-gate is the binding form (a P3-only failure must
block G2 even if G1 closed). RED-able via md5-distinct + the `runtime_target_rows_collapsed` co-gate.
ACCEPT.

### C5 [ACCEPT] — G1 is the behavior root and blocks every downstream behavior wave
G1 (R-C JSON projection) is the first behavior wave after PRUNE; every downstream entry-gate
(G2←G1, G3←G1, G4←G1, PROVE-transit←G1, G5/G6←G3←G2←G1) contains G1 on its predecessor path. A G1
REDRESS therefore blocks G2/G3/G4/G5-G6/PROVE/H1 — matching §2.2 (G2 inherits G1's projecting-renderer
discipline) and §3's standing order ("a wave failing its exit gate BLOCKS every downstream wave that
entry-gates on it"). The G1 exit is itself RED-able (byte-equivalence to `json_templates/` oracle
BEFORE oracle deletion + `.bbnf`-mutation falsifier + `parse_object_value_at_direct` 91.5%-preserve +
`verbatim_blob_present == false`). ACCEPT.

### C6 [REVISE] — the §3 per-wave PROVE gate mis-scopes "transitively" onto G4, its DIRECT predecessor (lets PROVE dispatch before G4 closes if read literally)
§2.5 (line 101) and the ASCII diagram (line 140) BOTH correctly state PROVE "entry-gates on **G3 ∧
G4**" with G4 a **DIRECT** conjunct ("transit." in line 140 scopes only G1∧P3). But the §3 per-wave
binding predicate — line 200, the line dispatch actually reads — says: "entry = G3 closed
(**transitively** G1 ∧ P3 ∧ G4)". This places G4 inside the "transitively" set. That is a genuine
lattice error, not cosmetics: in the DAG the edge runs **G3 → G4** (G4 depends on G3), so there is NO
path G3 → … → G4 by which G4 could be "transitively" closed once G3 closes — the edge points the
other way. G4 is a DIRECT predecessor of PROVE. Read literally, "entry = G3 closed (transitively …
G4)" admits PROVE to dispatch on G3-closed alone, BEFORE G4 closes — yet PROVE's OWN exit gate (line
203) requires "the Sheets value type instantiates the G4 trait," which is unprovable until G4 has
landed the trait. So the per-wave gate as written is self-inconsistent with the same wave's exit and
with §2.5/the ASCII. This is precisely a "wave dispatchable before its predecessor closes" defect
under the lens.
EDIT (`restart/skinny/tranches/sk-v18/research/p2/SYNTHESIS-RESEARCH.md`, §3, line 200): change
`- **PROVE** (R-E-2): entry = G3 closed (transitively G1 ∧ P3 ∧ G4). Exit = …`
to
`- **PROVE** (R-E-2): entry = G3 ∧ G4 closed (transitively G1 ∧ P3). Exit = …`
so the per-wave predicate names G4 as the DIRECT conjunct it is, matching line 101 and line 140.
REVISE (gate-precision; the dependency ITSELF is correctly captured in §2.5 + the ASCII, only the
binding per-wave line under-states it — but that line is what S-P3 must transcribe into the SPEC
manifest, so the error must not propagate).

### C7 [REVISE] — the §3 ASCII over-serializes G5/G6 UNDER G4 (the sibling-branch mirror of the V2-CH4-C5 fix)
The V2 CH4 C5 REVISE moved PROVE out from under G5/G6 (it only needs G3∧G4, not the CSS NEON wave).
The fold re-rooted BOTH G5/G6 and PROVE as `├─`/`└─` children of **G4** (lines 138-140). But G5/G6's
binding gate (line 189) is "P1 ∧ P3 ∧ G3 closed ∧ S-P1 measurement" — it does **NOT** list G4; §2.3
confirms G6 "entry-gates transitively on P1∧P3∧R-B∧G3" (no G4). So G5/G6 is now drawn as a child of
G4 while its actual predecessor is G3. The C5 fix corrected PROVE's over-nesting but dragged G5/G6
under the wrong parent in doing so — the indentation serializes the CSS-NEON wave behind G4 (the
shared-trait wave) when G5/G6 needs only G3. This is the same false-serialization class C5 named, on
the sibling branch: G5/G6 can dispatch the instant G3 closes, in parallel with G4, yet the diagram
implies it waits for G4. PROVE genuinely needs G4 (child-of-G4 correct); G5/G6 does not.
EDIT (`SYNTHESIS-RESEARCH.md`, §3 ASCII, lines 137-140): branch G5/G6 off **G3** (sibling to G4), not
under G4. Concretely, restructure so G3 forks to {G4, G5/G6}, G4 forks to {PROVE}, and both G5/G6 and
PROVE feed H1 — e.g.:
```
                   └─ G3  un-fork emitter (R-A A) entry: G1 ∧ G2 closed ∧ P4 live ∧ P3 (row-collapse)
                         ├─ G5/G6  neutral scan (R-F A)   entry: P1 ∧ P3 ∧ G3 ∧ S-P1 profile (94.1% leaf)
                         └─ G4  shared trait + phantom (R-D A)  entry: G1 ∧ G2 ∧ G3 closed
                               └─ PROVE  Sheets (R-E-2)   entry: G3 ∧ G4 closed (transit. G1∧P3) — PARALLEL to G5/G6
                                     └─ H1 …   entry: G5/G6 ∧ PROVE closed
```
The per-wave gate TEXT (lines 186-199) is already correct (G4←G3; G5/G6←G3, no G4); only the picture
mis-roots G5/G6. REVISE (diagram precision; no dependency is wrong in the text, but S-P3 must encode
the parallel G3→{G4,G5/G6} fork in the SPEC manifest, not the diagram's serial G3→G4→G5/G6 reading).

### C8 [ACCEPT] — PROVE-on-G3∧G4-parallel-to-G5/G6 is the CORRECT schedule (the dependency is right; C6/C7 are the wording/picture that must transcribe it faithfully)
The substantive claim of the lens — PROVE gates on G3∧G4 and runs PARALLEL to G5/G6 because Sheets
does not use the CSS NEON — is CORRECT and grounded: §2.5 (Sheets value type instantiates the R-D
trait ⇒ G4 direct), §2.3 (G6 needs R-B∧G3, not G4), line 140's "PARALLEL to G5/G6 (Sheets does not
use the CSS NEON)". H1 joins both branches (line 141/206, entry = G5/G6 ∧ PROVE). The schedule is
sound; C6 (PROVE per-wave wording) and C7 (G5/G6 diagram rooting) are the two surface defects that, if
transcribed verbatim into the SPEC, would corrupt this correct schedule — they do not invalidate it.
ACCEPT (the schedule), with the two transcription fixes bound in C6/C7.

### C9 [ACCEPT] — every per-wave falsifier is concrete and RED-able; the cited disk anchors are real
Spot-re-grounded on disk this pass:
- G3 `emit_shape_source == lowered_program`: the falsifier "grep `render(program)` body for any read
  of `target.profile`/`target.emitter`/`target.output_labels`/`target.profile_contract` == 0" targets
  a real seam — `sink_only.rs:27 policy_summary: RuntimePolicySummary` + `:48 backend_shape:
  BackendShape` + `:135 backend_shape: policy.selected_shape` confirm the un-forked body CAN read its
  shape from `program.policy_summary.backend_shape`, so the "never from `target.*`" falsifier is
  RED-able against a real alternative.
- R16 row-collapse: `regen.rs:5 #[derive(Clone, Copy, Debug)]` (NO `PartialEq`) confirms the +1-line
  `PartialEq` derive is a real, minimal target; `regen.rs:15 emitter: RuntimeEmitterKind`,
  `:17 frontend_requirements` (struct field #11), `:18 output_labels` (#12) confirm the #11/#12 ordinal
  pins and the field G3 deletes — the full-row `PartialEq` recurses into BOTH nested structs as claimed.
- G3 fork-deletion: `RuntimeEmitterKind` is live in `grammar_provider.rs`, `lib.rs`,
  `runtime_generator.rs` — the `emitter_fork_present == false` grep falsifier has real targets to
  drive to zero.
- P1 (`find …/src/x86_64 …/ext/x86 -type f == 0`), P2 (`grep measure_mbps == 0`), P3 (md5-distinct +
  row-collapse), P4 (re-inject-and-revert), P5 (`grep -c parse_w11_1_number == 0`), G1
  (byte-equivalence to `json_templates/`), G2 (`CSS_GENERATED_RS` grep == 0 + cold corpus-in-timer
  `track1_rich/lightningcss >= S-P1 ratio`), G5/G6 (generated-`generated.rs` caller census non-empty,
  NOT `#[cfg(test)]`), PROVE (`generator_grammar_count == 3` + md5-distinct), H1 (`corpus_in_timer ==
  true`, regen --check clean) — all are executable grep-count / md5 / bool / re-inject checks, not
  assertions. RED-able. ACCEPT.

### C10 [ACCEPT] — the hard-cap + revert-dependency discipline is present and consistent with the lattice
§3 (lines 209-211) carries the standing `[dispatch-hard-cap]` defaults (research 20 / plan 15 /
redress 30, "at 0.9N commit, at N halt") with the documented larger cap for the MED-HIGH PROVE/G6
cluster; the standing order (line 127-128) binds "a wave failing its exit gate BLOCKS every
downstream wave that entry-gates on it … No wave dispatches over a REDRESSed predecessor." Both the
revert-dependency graph (the entry-gate chain) and the caps are present and consistent with the C1
DAG. ACCEPT.

---

## Net

The PRUNE→G1..G6→PROVE→H1 lattice is ACYCLIC (C1) and COMPLETE (C2); P4-before-G2/G3 is explicit and
RED-able (C3); P3-before-G2/G6 is doubly-enforced (C4); G1 blocks every downstream behavior wave (C5);
the PROVE-on-G3∧G4-parallel-to-G5/G6 SCHEDULE is correct (C8); every per-wave falsifier is concrete
and grounded in real disk symbols (C9); caps + revert-discipline are present (C10).

TWO transcription defects S-P3 must NOT carry into the SPEC manifest — both in how the §3 surface
RENDERS a correct dependency, not in the dependency itself:
- **C6 REVISE** — §3 line 200 mis-scopes "transitively" onto G4 (PROVE's DIRECT predecessor),
  contradicting §2.5/line 140 and PROVE's own exit gate; read literally it lets PROVE dispatch before
  G4 closes. Fix: "entry = G3 ∧ G4 closed (transitively G1 ∧ P3)".
- **C7 REVISE** — the §3 ASCII over-serializes G5/G6 UNDER G4 (the sibling-branch mirror of the
  V2-CH4-C5 fix); G5/G6 needs only G3 and runs PARALLEL to G4. Fix: branch G5/G6 off G3 (sibling to
  G4), both feeding H1.

Neither REVISE is a dependency error — the §2.5 couplings and the per-wave TEXT (C3-C5, C8-C9) carry
the correct DAG. They are picture/wording slips that, if S-P3 transcribes them verbatim, would encode
a false serialization (G5/G6 behind G4) and a false relaxation (PROVE before G4). S-P3 must encode the
TEXT's lattice (G3→{G4, G5/G6}; G4→PROVE; {G5/G6, PROVE}→H1), not the diagram/line-200 surface. No
circular dependency, no wave genuinely dispatchable before its predecessor (the only such hazard is
the C6 wording, fixed by the edit). All falsifiers RED-able.

TALLY accept=8 revise=2 reject=0
