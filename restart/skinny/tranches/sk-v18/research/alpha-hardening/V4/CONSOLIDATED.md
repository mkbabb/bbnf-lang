# SK-V18 Pass-Alpha CHALLENGE — V4 CONSOLIDATED

Cycle V4 (seven-lens) over the αF V4 contract (`SYNTHESIS.md` + `HANDOFF.md`) which
folded the three surviving V3 REVISEs. Bracket HEAD `318d9c046`. Per `PASS-ALPHA §3` +
`ORCHESTRATOR §3W/§3Z`. The αF V5 redress (cycle V5, alphaF) folds the V4 dispositions
below into `SYNTHESIS.md` + `HANDOFF.md` + the αA/αE feeders; this CONSOLIDATED records
the V4 wave result and the V5 resolution pointer.

## Per-lens tally (V4)

| Lens | ACCEPT | REVISE | REJECT | Rate | Verdict |
|---|---|---|---|---|---|
| CH1 Correctness | 6 | 1 | 0 | 85.7% | αE orphan: V3 CH5 §C.5 x86-scope not folded into αE |
| CH2 Generality | 29 | 1 | 0 | 96.7% | NEW §8.1: `runtime_target_rows_collapsed` projects onto 2 invariant columns, misses the 5 per-profile columns |
| CH3 Regression | 5 | 2 | 0 | 71.4% | αA + αE retain the false-green `src/`-scoped x86 P1 gate (FOLD-1 not propagated) |
| CH4 Cost | 6 | 0 | 0 | 100% | converged; F13 reach-scope cost-free; no orphan |
| CH5 Hidden Coupling | 25 | 0 | 0 | 100% | converged; both V3 REVISEs folded; gates structurally honest |
| CH6 Next-Tranche-Impact | 11 | 2 | 0 | 84.6% | §1 P1 deletion-list narrower than crate-wide grep (RED-by-construction); §13 fold-ledger self-citation drift |
| CH7 Overfit/Prune | 7 | 1 | 0 | 87.5% | αA x86 census + close-gate scoped `src/x86_64/` only (FOLD-1 not propagated to αA) |

**Wave aggregate: ACCEPT 89 / REVISE 9 / REJECT 0 (90.8%). Below the §3Z ≥95% bar → V4
does NOT converge; the REVISEs fold into V5.** Zero REJECTs; every REVISE is a tightening
(not a finding reversal) and carries a concrete fold mechanism.

## The V4 REVISE clusters (three distinct defects, all folded in V5)

### Cluster 1 — x86 FOLD-1 orphan-propagation (CH1 §αE, CH3 αA/αE, CH7 §1)

The V3→V4 FOLD-1 widened the x86 P1 deletion crate-wide (second surface: `ext/x86/` 3554
LOC + nasm `build.rs` + `lib.rs:247` ref) and landed correctly in αC / SYNTHESIS / HANDOFF
— but the `src/`-scoped close-gate SURVIVED in the αA and αE research feeders. A P1 keyed
to a `src/`-scoped `find …/src/x86_64 -type f`=0 PASSES GREEN over the live second surface
— the exact overfit-prune failure mode this audit exists to catch.

**V5 resolution:** αA was redressed to crate-wide in a prior pass (carries "V5 R-1" markers;
the two residual `find …/src/x86_64` mentions explicitly describe the OLD gate as the defect
being corrected). αE's P1 row (`:83`), P1 exit (`:93`), LOC budget (`:97`), candidate-A
summary (`:210`), net-LOC (`:216`) are now folded crate-wide + reach-complete, with a new
fold-ledger row **F15**. The binding contract (SYNTHESIS P1 row + `x86_tree_deleted`
telemetry + HANDOFF inv.3) was already crate-wide and is now reach-complete (cluster 2).

### Cluster 2 — P1 deletion-list reach mismatch (CH6 §1, the consequential one)

The V3→V4 FOLD-1 widened the P1 verify grep crate-wide but left the deletion-target list at
four items. Re-grepped at HEAD, the crate-wide grep ALSO fires on three ACTIVE x86 surfaces
the list never named: the `nasm-rs = "0.3"` build-dep (`Cargo.toml:19`), `lib.rs:5 pub mod
x86_64;` + the `#[cfg(target_arch="x86_64")]` dispatch arms (`lib.rs:285-288`), and the
in-crate doc surfaces — making the gate RED-by-construction (the mirror of the V3 escape it
fixes), a paper-close hazard on the mandatory lands-FIRST PRUNE gate.

**V5 resolution:** P1 close-condition + `x86_tree_deleted` telemetry + the §0.3 receiver row
+ §0.5 axis row (SYNTHESIS) and the P1 receiver + invariant 3 + Next-Move + gate consumer
(HANDOFF) are all EXTENDED with removal targets (e) the `nasm-rs` Cargo.toml dep; (f) the
`lib.rs` module decl + cfg-dispatch arms; (g) the doc surfaces scrubbed OR the grep scoped to
`--include='*.rs' --include='Cargo.toml'`. The deletion list is now reach-matched to the grep
(satisfiable-by-construction).

### Cluster 3 — `runtime_target_rows_collapsed` projection tuple (CH2 §8.1, NEW)

The V3 F13 fold correctly moved the relocated-overfit-seam defense from the arm-census regex
(syntactically incapable) to the P3-collapse structural row-count check — but bound it to a
`(source_roots, entry_rule)`-only projection. Disk-reproved at HEAD: the 7 css_l4 rows are
byte-identical on those 2 columns (GREEN) but carry 7 DISTINCT `fact_schema`/`output_plane`/
`row_id`/`emitter` values — the 5 columns where per-profile divergence lives and a relocated
branch can ride. The gate projects onto exactly the 2 invariant columns.

**V5 resolution:** every F13 site (SYNTHESIS G3 (iii), §0.4, the telemetry column, the gate
consumer; HANDOFF inv.5 (iv) + gate consumer; αE F13 ledger `:18`/`:85`/`:94`/`:214`) is
WIDENED to: all `RuntimeTarget` rows sharing one `grammar_name` byte-identical modulo the
generated-artefact path columns — `count(distinct config-tuple-minus-output_dir) == 1` per
`grammar_name` over all non-path columns. The gate is correctly RED pre-P3 (7 distinct
`fact_schema` today), GREEN only post-collapse. New fold-ledger row **F16**. The P3-collapse
MECHANISM was right; only the PROJECTION TUPLE widened.

### Cluster 4 — fold-ledger self-citation drift (CH6 §13, documentation-accuracy)

The V3→V4 fold-ledger narrative back-referenced prior-cycle line numbers (`:201`/`:423`/
`:377-378`) that the V4 edits themselves shifted ~50-60 lines. Machine-gate-UNAFFECTED (gate
rows + telemetry columns + gate consumer use named columns + greppable commands), but
audit-misleading in the authority document.

**V5 resolution:** the self-citations switched to fold-stable section/column anchors ("the G3
close-condition row," "the `generator_grammar_branch_count` telemetry column," "the Section 1
checkasm ledger") so the ledger no longer drifts on subsequent folds.

## V5 fold completeness (zero orphan)

| V4 REVISE | Lens(es) | V5 fold site | Status |
|---|---|---|---|
| x86 FOLD-1 orphan in αA | CH3, CH7 §1 | αA crate-wide (prior "V5 R-1" redress; residual mentions describe the corrected defect) | FOLDED |
| x86 FOLD-1 orphan in αE | CH1 §αE, CH3 | αE `:83`/`:93`/`:97`/`:210`/`:216` crate-wide + F15 ledger row | FOLDED |
| P1 deletion-list reach mismatch | CH6 §1 | SYNTHESIS P1 row + `x86_tree_deleted` + §0.3 + §0.5; HANDOFF P1 + inv.3 + Next-Move + gate consumer | FOLDED |
| `runtime_target_rows_collapsed` projection tuple | CH2 §8.1 | SYNTHESIS G3 (iii) + §0.4 + telemetry col + gate consumer; HANDOFF inv.5 (iv) + gate consumer; αE F13 `:18`/`:85`/`:94`/`:214` + F16 ledger row | FOLDED |
| fold-ledger self-citation drift | CH6 §13 | SYNTHESIS V3→V4 ledger anchors made fold-stable | FOLDED |

All five V4 REVISE clusters fold into V5 with concrete mechanisms; zero orphan; zero REJECT;
every fold is a tightening that deepens (not loosens) the gate. Direction of every V4 REVISE
preserved: the x86 deletion goes wider (more LOC deleted), the relocated-seam projection goes
wider (more columns checked), the ledger goes more stable.

## Convergence posture

V4 = 90.8% (sub-95%, non-converging). The V5 redress folds all V4 REVISEs orphan-free; a
clean V5 confirming CHALLENGE wave is required to record two consecutive ≥95% cycles per §3Z.
No architectural re-open, no new candidate, no stranded >SOTA: the V5 folds are scope/reach/
projection corrections atop a structurally-sound contract (CH4/CH5 at 100%, CH2/CH7 ≥87.5%,
the measurable core anti-paper-close throughout). The contract's R10 success criterion + the
goalset §4.1–§4.3 + telemetry schema are unchanged in substance; only the x86 close-gate reach
and the relocated-seam projection tuple are sharpened.
