# CH4 COST — T-P3 SK-V18 Hardening, Cycle V3

Lens: CH4 COST. Every delta must state a LOC budget, a propagation cost (surfaces
touched), a risk class, and a wave alignment; 3B NEW waves must carry a same-wave
consumer; 3C dispositions must be realistic. Cross-scope, uncited,
revived-refuted, and silent-drop deltas are REJECT bait.

Target packet: the 6 SK-V18 synthesis artefacts under
`restart/audit/totality/p3/` (3A, 3B, 3C-crystallisation, 3C-v+1-diff, 3D, 3E,
3F), mtimes 20:16–20:27. NOTE on cadence: the artefacts were NOT re-folded
between the V2 CH4 verdict (20:36) and this V3 cycle — the V3 packet is
BYTE-IDENTICAL to the packet V2 reviewed (no `fold-v2` edit landed; the stale
04:24 V3/CH*.md files are SK-V15-era leftovers being overwritten). My charge is
therefore an INDEPENDENT re-verification — not a re-read of V2's prose — that the
nine V1 CH4 REVISE findings remain folded on disk and that no CH4 cost defect
survives. The V3 CHALLENGE-CONTEXT's "67 Pattern H files" expected-invariant is
stale SK-V15 text; the SK-V18 packet honestly re-keys it to 71 (= 67 + 4
tape-fold). 71 is the live count (`find … = 71` confirmed) — not a defect.

## Verdict summary

The packet is CH4-strong and the V1 defect class (SK-V18/SK-V19 cost laundering)
is genuinely discharged. I re-verified every load-bearing fact from the live
tree, not from the artefacts' `v1_fold` prose. ONE residual CH4 cost-attribution
defect survives that V2 inspected and wrongly waved through: 3D's W-PRUNE summary
row carries an internally inconsistent component sum. It is a REVISE, not a
REJECT (every figure is individually defensible; the row mixes two of them).

No REJECT-class finding: no uncited delta, no revived refuted route, no
silent-drop candidate, no cross-scope violation, no W12/W13 overflow, no doc-only
implementation gate.

## Independent spot-verification of load-bearing facts (all resolve unless noted)

| claim | artefact | on-disk result | verdict |
|---|---|---|---|
| extracted 3C v+1 diff applies | 3C-v+1-diff | `awk`-extract (37 lines) → `git apply --check` **CLEAN**; header `@@ -622,6 +622,33 @@` well-formed | RESOLVES |
| 16 numbered locks | 3C invariant | `grep -cE '^[0-9]+\. \*\*' LOCKS.md` = **16** | RESOLVES |
| 5 `BackendShape` variants | all | `skinny/crates/ir/src/lib.rs:340` enum = exactly {EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}; bench mirror at `lock14_baseline.rs:5075` identical | RESOLVES |
| 7 byte-identical css_l4 replicas, md5 `b654562c`, 910 LOC | 3D/3F/3B P3 | 7 × `generated.rs` all md5 `b654562ccff46…`, all 910 LOC; unique-md5 = 1; **6×910 = −5460** exact | RESOLVES |
| `const CSS_GENERATED_RS: &str = r#"` courier | 3A-D05 / 3F-MH-010 / 3C-L06 | confirmed verbatim at `runtime_generator.rs:701` | RESOLVES |
| `css_types.rs` 66 LOC | 3F-MH-013 / 3C-L13 | 66 LOC; NAMED in `LOCKS.md:349` Lock-14 overfit-mess list | RESOLVES |
| x86 census baseline (P1 ≈−4500; gate "today 28") | 3B §13.7 P1 | `find …/x86_64 …/ext/x86 -type f` = **28** | RESOLVES |
| metalang leak (P5 gate "today 7") | 3B §13.7 P5 / 3A-D12 | `grep -c parse_w11_1_number json/generated.rs` = **7** | RESOLVES |
| live e-graph `Rewrite` (LAC-2D-V3-03 realism) | 3C disposition | `backend_egraph.rs:193` `impl Rewrite<DecisionNode,NoAnalysis> for NormalizeDirectSinkCost` is live (non-`#[cfg(test)]`), instantiated in the `enable_rewrites` path | RESOLVES — V1's realism REVISE truly folded |
| 9-grammar `idents` table | 3C-L13 / 3A-D11 / 3E-D16 | `crates/ir/src/registry/strategy.rs` `ManifestStrategyEntry` table; this is the **totality** tree, correctly fenced as SK-V19 R16 (SPEC:58-61 binds the SK-V18 surface to `skinny/crates/`, NOT `crates/core/`) | RESOLVES |
| runtime Pattern-H census | 3C-L13 / 3A-D12 | `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs'` = **71** | RESOLVES |
| P3 SPEC-authoritative figure | 3B/3F | `sk-v18/SPEC.md:435` = "≈ −5500 (6×910 replica bodies deleted; +1 PartialEq derive; ~−40 collapsed rows)" | RESOLVES — −5500 is the SPEC figure, NOT −5460 |
| net headline | all | `sk-v18/SPEC.md:21,:61,:454,:571` = verbatim **−10800**; SPEC contains BOTH the −10800 headline and the −10685 per-wave sum (internal-to-SPEC) | RESOLVES |

## Re-verification of the nine V1 CH4 REVISE folds (all hold on disk)

| V1 REVISE | V3 state on disk | now |
|---|---|---|
| 3C-v+1-diff malformed | `git apply --check` CLEAN; header re-cut to `+622,33` | HELD |
| 3A-D11 conflates +15 with +217 | D11a/D11b split present in V4 CH4 matrix (`3A:256-257`); D11b ≈+217 explicitly SK-V19-owned, "never laundered into D11a's +15" | HELD |
| 3A carried-V1 matrix lacks historical marker | header now "## CH4 Coverage Matrix (carried V3 / SK-V15-historical)" + per-W# re-key | HELD |
| 3B-D04 ad-hoc −10700 | dropped; `3B:213` carries P1−4500+P2−700+P3−5500+P4+15+P5 0 = ≈−10685 cited `SPEC:433-437` | HELD |
| 3C-L13 imports +217 inside 3-5 doc LOC | `3C:113` COST NOTE: 3-5 LOC is doc-only; 9-row collapse SK-V19-owned ≈+217, cross-ref MP.SK19.UNFORK / 3F-MH-013 | HELD |
| 3C-LAC-2D-V3-03 unverified "SATISFIED at HEAD" | `3C:135` replaced with spot-cited live `NormalizeDirectSinkCost` Rewrite; CSP-half deferred to SK-V19 entry | HELD (impl independently confirmed live) |
| 3D-D12 binds +1-line derive to 9-row cost | `3D` D12 cell scoped to skinny P3 +1-line `PartialEq` derive; 9-row scaling cross-ref'd SK-V19 R16 ≈+217 | HELD |
| 3E carried-D01..D11 dead wave-ids | disclaimer + `sk_v18_extension_note` re-key present | HELD |
| 3F-MH-009 P3 drift | `3F:76,:119,:188` carry P3 ≈−5500 cited `SPEC:435` with the −5460 replica-body reconcile | HELD |

## Per-delta CH4 disposition (V3)

### 3A — ARCHITECTURE (14 SK-V18 + 12 carried-historical)
V4 CH4 Coverage Matrix (`3A:238`) carries doc LOC / propagation count / risk /
wave / consumer-gate / fail-action for every SK-V18 row; D11a/D11b split clean;
D07 x86 −4500, D05 −910-after-provider, D03 ≤450 un-fork all honour the 12-wave
ceiling (`3A:240` "hard-cap fit excludes a W12 / challenge-time implementation").
Carried matrix re-stamped historical. **26/26 ACCEPT.**

### 3B — MASTER-PLAN (10 new SKV18-D01..D10 + 4 carried)
§13.7 NEW-wave block (`3B:144-157`): all 12 waves carry manual-LOC/risk, MASTER
alignment, a **same-wave consumer**, and a RED exit-gate falsifier with a
measurable baseline — the 3B-NEW-wave consumer requirement is fully MET. D04
folded (−10700 dropped; per-wave sum −10685 cites SPEC). P3 = −5500 throughout
3B, consistent with SPEC. **14/14 ACCEPT.**

### 3C — LOCKS crystallisation (11 clauses) + dispositions (21) + v+1 diff
Per-clause cost matrix (`3C:101-113`) carries doc-LOC / risk / owning gate /
consumer / propagation count / hard-cap fit / fail-action for ALL 11 clauses —
the model CH4 deliverable. 21/21 candidates disposed (8 T-P1: LAC-1E-V5-01..07 +
1A-LOCK1-AMEND-001; 13 T-P2: 3×2C + 4×2D + 3×2E + 3×2F), 0 silent drops
(`answered` frontmatter lists exactly 21; LAC-2F-V3-03 DEFER carries a named
re-entry trigger). Dispositions realistic: the two PLANNED co-gate symbols
(`runtime_target_rows_collapsed`, `bbnf_simd_single_mask_convention`) are
honestly disclosed PLANNED, not cited live; LAC-2D-V3-03 carries a spot-cited
live `Rewrite` impl (confirmed). v+1 diff applies clean, doc-only LOCKS addendum,
adds no lock/directive/substrate/sixth shape. **11 clauses + 21 dispositions +
diff = 33/33 ACCEPT.**

### 3D — skinny-fold (12 deltas)
CH4 coverage matrix complete for all 12; monotonic-fold direction preserved;
D12 scoped to the skinny +1-line derive. **D-row cost cells ACCEPT (12/12).**
BUT the W-PRUNE *summary* row at **`3D:155`** is a separate CH4 cost-attribution
defect (below). **1 REVISE on the W-PRUNE summary row; 12 D-row ACCEPT.**

### 3E — grammar-generalisation (7 new D12-D18 + 11 carried)
7 new deltas carry LOC/risk/wave/propagation inline; D12 "no body charged,
cross-reference only" is exemplary anti-double-budget hygiene; D16 owns docs/gate
(180-320 LOC) and defers lowerer carriers to 2D W7/W8/W9 via 3B/G-Omega; carried
D01-D11 matrix re-stamped historical + re-keyed; no-W12 routing explicit
(`3E:161`). **18/18 ACCEPT.**

### 3F — MIGRATION + HANDOFF (12 active; MH-002 removed-with-supersession)
CH4 V6 coverage matrix complete; five migration decisions priced (x86 −4500,
courier −910, replicas −5500, phantom decoration, css_types 66); MH-009 folded
(P3 −5500 cited SPEC:435 aligned across decisions table / collapse row / 3D);
MH-013 correctly defers css_types to SK-V19. **12/12 ACCEPT.**

## The one surviving CH4 defect (REVISE)

**`3D:155` — W-PRUNE summary row internal sum is inconsistent.** The row reads:

> "net LOC ≈ −10800 (P1 x86 ≈ −4500, P2 warm bench ≈ −700, **P3 7 replicas
> ≈ −5460**, P4 gate-fix, P5 metalang rename ≈ 0)"

This single sentence states a component breakdown whose sum is
−4500 −700 −5460 +0 +0 = **−10660**, while asserting net ≈ **−10800**, while the
SPEC-authoritative per-wave sum is **−10685** (P3 = −5500 per `SPEC:435`). The
row's own components (−10660) match NEITHER its stated net (−10800) NOR the
per-wave SPEC sum (−10685), and it is the ONLY artefact still carrying P3 −5460
in a component-sum context — every other artefact (3B §13.7 P3, 3B SKV18-D04,
3F-MH-009/011) uses the SPEC −5500 and reconciles to −10685.

V2 CH4 inspected this exact row (`V2/CH4.md:92-95`) and dismissed it as
"consistent with the SPEC's ≈−5500 breakdown, not a residual drift." That
dismissal is arithmetically wrong: `3D:155` does NOT carry the −5500 breakdown —
it carries the bare replica-body figure −5460 AND the −10800 headline in one
component-sum sentence, producing a row that is internally inconsistent on its
own terms. A CH4 cost matrix that states a component breakdown must sum to its
stated net or cite the SPEC figure verbatim.

**Severity:** low (cosmetic arithmetic; no laundering, no wave hidden). **Owner:**
3D. **Repair:** in `3D:155`, state P3 ≈ −5500 (= −5460 replica bodies + ~−40
collapsed rows + 1 `PartialEq` derive) per `sk-v18/SPEC.md:435`, so the
P1+P2+P3+P4+P5 components sum to the SPEC per-wave −10685 under the ≈ tilde to the
−10800 headline — matching the figure 3B/3F already carry. This is the SAME
P3-drift defect class V1 corrected in 3B-D04 and 3F-MH-009; it was never propagated
to 3D's W-PRUNE summary row.

## Residual observation (NOT a CH4 cost defect)

The P2 §13.7 exit-gate falsifier baseline reads "today 48" for
`measure_mbps|lightningcss_facts`, but a whole-crate grep returns **64**
(`nonjson_css_l4.rs:48` + `bin/gate.rs:16` are the warm-fixture surface; the
broader corpus carries more). This is an exit-gate FALSIFIER baseline count, not
a delta LOC budget — P2's −700 cost budget is independently SPEC-anchored
(`SPEC:434`), so the imprecise baseline does not corrupt a cost figure. Flagged
for the CH1 correctness lens (baseline accuracy), not REVISE'd here. Likewise the
−10800-vs-−10685 ~115-LOC gap is INTERNAL TO THE SPEC (the SPEC headlines −10800
AND contains rows summing to −10685); the artefacts faithfully carry the verbatim
headline and honestly exhibit the per-wave sum — correct CH4 discipline.

## Cross-scope / refuted-route / silent-drop scan (REJECT bait — none found)

- **No SK-V19 cost laundered into a skinny figure**: every `≈+217` is scoped to
  SK-V19 / D11b / MP.SK19.UNFORK / 3F-MH-013 — the V1 defect is fully discharged.
- **No sixth `BackendShape`**: 5 variants at `ir/src/lib.rs:340`; FactStream is an
  output-plane category (MASTER MP.NW6), not a shape.
- **No revived refuted route**: REDRESS 96/97/98 fenced in 3C-L10 / 3A-D08; G6 is
  RETARGET-not-author (refute 3, speedup DEFERs to H1 per refute 8);
  x86-closes-M5-row REFUTED (3A-D07/3D/3E/3C-L08); CollapsedStage is an inert
  REDRESS-fenced slot.
- **No silent drop**: 3C disposes 21/21; 3B retires consumed SK-V15 deltas with
  stated rationale; 3F removes MH-002 with stated supersession.
- **No W12/W13 overflow**: all references are explicit "no W12 spillover" guards;
  manifest is exactly 12 waves at the skinny ceiling.
- **No doc-only implementation gate**: the v+1 diff is a doc-text LOCKS addendum;
  every implementation body is charged to its owning G/P wave, never to a doc gate.

## Tally

Counting at the delta/clause/disposition granularity the lens enumerates:

- 3A: 26 ACCEPT (14 SK-V18 + 12 carried; D11 split + carried-matrix held)
- 3B: 14 ACCEPT (10 new + 4 carried; §13.7 consumer requirement MET)
- 3C: 33 ACCEPT (11 clauses + 21 dispositions + 1 v+1 diff)
- 3D: 12 ACCEPT (D-rows) + 1 REVISE (`3D:155` W-PRUNE summary internal sum)
- 3E: 18 ACCEPT (7 new + 11 carried)
- 3F: 12 ACCEPT (MH-009 held; MH-002 removed-with-supersession)

ACCEPT = 115, REVISE = 1, REJECT = 0. The single REVISE (`3D:155`) is the binding
output: it forces a V4 fold before this can count as a clean cycle. All other
folds were independently re-verified on the live tree (diff applies; 6×910=−5460;
x86=28; metalang=7; 5 shapes; 16 locks; 21/21 dispositions; live e-graph Rewrite;
all 12 §13.7 NEW waves carry a same-wave consumer). This is NOT a clean V3.

TALLY accept=115 revise=1 reject=0
