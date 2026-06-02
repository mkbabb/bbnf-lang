# CH4 COST — T-P3 SK-V18 Hardening, Cycle V2

Lens: CH4 COST. Every delta must state a LOC budget, a propagation cost (surfaces
touched), a risk class, and a wave alignment; 3B NEW waves must carry a same-wave
consumer; 3C dispositions must be realistic. Cross-scope, uncited,
revived-refuted, and silent-drop deltas are REJECT bait.

Target packet: the 6 regenerated SK-V18 synthesis artefacts under
`restart/audit/totality/p3/` (3A, 3B, 3C-crystallisation, 3C-v+1-diff, 3D, 3E,
3F), mtimes 20:16–20:27 — authored AFTER the V1 CH4 verdict (20:11). This is the
V2 fold cycle: the artefacts absorbed the V1 CH4 REVISE cluster, and my charge is
to independently re-verify (a) that each V1 REVISE was actually folded, not merely
claimed, and (b) that the fold introduced no new CH4 defect.

## Verdict summary

The V2 fold is clean on CH4. Every one of the nine V1 CH4 REVISE findings was
absorbed — I re-verified each on-disk, not from the artefacts' own `v1_fold`
prose. The single sharpest V1 finding (the malformed v+1 diff) is repaired and
the diff now applies. No new REJECT, no SK-V19 cost laundering, no W12 overflow,
no uncited delta, no silent drop, no cross-scope violation.

### V1 CH4 REVISE → V2 disposition (all re-verified on disk)

| V1 REVISE | V2 fold | independent verification | now |
|---|---|---|---|
| 3C-v+1-diff malformed (`-622,6 +622,38` body 5/33; `git apply --check` corrupt at line 38) | header re-cut to `@@ -622,6 +622,33 @@` | `awk`-extract → `git apply --check` returns **APPLIES CLEAN**; 6 ctx old-side, body well-formed | RESOLVED |
| 3A-D11 conflates skinny +15 with SK-V19 R16 +217 | split into D11a (P4 green-by-exclusion, ≈+15, SK-V18) + D11b (R16 9-ident row-collapse, ≈+217, SK-V19) | 3A:214,:233,:256-257 carry the split; D11b row reads "≈+217 … NOT an SK-V18 wave; SK-V19-owned … never laundered into D11a's +15" | RESOLVED |
| 3A carried-V1 matrix presents live SK-V15 wave-ids without a historical marker | HISTORICAL-ONLY WAVE-ID DISCLAIMER + per-W# re-key | 3A:102-110 maps W1→P2…W11→deleted; matrix wave column header relabelled "(SK-V15-historical; see disclaimer)" | RESOLVED |
| 3B-D04 third net figure (−10700 ad-hoc) | replaced with per-wave SPEC sum P1−4500+P2−700+P3−5500+P4+15+P5 0 = ≈−10685, P3 breakdown cited | 3B:23,:106,:213 carry the sum + the SPEC-cited P3 −5500 (=6×910 −5460 + ~−40 rows + 1 derive); the −10700 is dropped | RESOLVED |
| 3C-L13 clause imports SK-V19 ≈+217 inside 3-5 doc LOC | COST NOTE: the 3-5 LOC is doc-only; the 9-row collapse is SK-V19-owned ≈+217 | 3C:113 carries the COST NOTE cross-ref to MP.SK19.UNFORK / 3F-MH-013 | RESOLVED |
| 3C-LAC-2D-V3-03 disposition carries an unverified "SATISFIED at HEAD" e-graph activation claim | replaced with spot-cited live `NormalizeDirectSinkCost` Rewrite; CSP half deferred to SK-V19 entry | 3C:135 + verified on disk: `backend_egraph.rs:191-193` is a live `impl Rewrite<DecisionNode,NoAnalysis>` (non-`#[cfg(test)]`), instantiated `:75` in the `enable_rewrites` path | RESOLVED |
| 3D-D12 binds skinny +1-line derive to the totality 9-row collapse cost | CH4 cell scoped to skinny P3 +1-line derive; 9-row scaling cross-ref'd SK-V19 R16 ≈+217 | 3D:47,:132,:149 carry the scope split | RESOLVED |
| 3E carried-D01..D11 matrix costs rows to dead SK-V15 wave-ids | HISTORICAL-ONLY DISCLAIMER + `sk_v18_extension_note` re-key (W5→G2, W6→G2, W7→G3∧PROVE∧SK-V19) | 3E:170-178 carry the disclaimer + re-key; column header relabelled | RESOLVED |
| 3F-MH-009 P3 −5500 vs 3D −5460 drift | P3 cited verbatim from SPEC `:435` (≈−5500 = −5460 6×910 + ~−40 rows), aligned across decisions table / collapse row / 3D | 3F:27,:76,:119 carry the aligned figure with the explicit 3D −5460 reconcile | RESOLVED |

## Spot-verification of load-bearing facts (all resolve)

| claim | artefact | on-disk result | verdict |
|---|---|---|---|
| 16 numbered locks | 3C invariant | `grep -cE '^[0-9]+\. \*\*' LOCKS.md` = 16 | RESOLVES |
| runtime Pattern-H census | 3C-L13 / 3A-D12 | `find crates/core/src/runtime -mindepth 2 … = 71` | RESOLVES (the SK-V18 packet re-keys the absolute-67 baseline to per-file provenance; 71 is the live count, +4 tape-fold) |
| 5 `BackendShape` variants | all | `ir/src/lib.rs:341` enum = exactly {EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage} | RESOLVES |
| 9-grammar `strategy.rs` idents | 3C-L13 / 3E-D16 / 3F | `strategy.rs:137-185` = 9 `ManifestStrategyEntry` rows (json,sheets,css_l4,bbnf,csv,math,bnf,ebnf,css_pretty), consumed via `for_grammar_with_manifest:216` | RESOLVES |
| 7 byte-identical css_l4 replicas, md5 `b654562c`, 910 LOC each | 3D / 3F / 3B P3 | 7 × `skinny/.../grammars/css_l4_*/generated.rs` all md5 `b654562ccff46…`, all 910 LOC; 6×910 = **−5460** | RESOLVES — P3 arithmetic exact |
| `const CSS_GENERATED_RS: &str = r#"` courier | 3A-D05 / 3F-MH-010 | confirmed at `runtime_generator.rs:701` | RESOLVES |
| `css_types.rs` 66 LOC | 3F-MH-013 / 3C-L13 | 66 LOC; named in LOCKS:349 | RESOLVES |
| Lock-14 self-gate asserts ZERO / live count | 3C-L13 / 3A-D11 | LOCKS:349 verification cmd asserts ZERO; the live 9-row idents table escapes the narrow 4-name regex | RESOLVES — gate is RED |
| x86 census baseline (P1 ≈−4500; exit-gate "today 28") | 3B §13.7 P1 / 3F-MH-008 | `find …/x86_64 …/ext/x86 -type f` = **28** | RESOLVES — baseline exact |
| metalang leak (P5 exit-gate "today 7") | 3B §13.7 P5 / 3A-D12 | `grep -c parse_w11_1_number json/generated.rs` = **7** | RESOLVES — baseline exact |
| **extracted 3C v+1 diff applies** | 3C-v+1-diff | `git apply --check` → **APPLIES CLEAN** | RESOLVES — V1's sharpest finding repaired |

## Per-delta CH4 disposition (V2)

### 3A — ARCHITECTURE (14 SK-V18 deltas + 12 carried-historical)

D01–D14 ACCEPT: each carries LOC / propagation / risk / wave / consumer-gate /
fail-action; CollapsedStage D08 cost split (≈0 committed inert / ≤450 conditional
G5/G6-gated) realistic; D07 x86 −4500 / D05 −910-after-provider / D03 ≤450 un-fork
all honour the 12-wave ceiling with no W12. **D11 ACCEPT** (V1 REVISE folded into
D11a/D11b, ≈+217 charged to SK-V19). **Carried-V1 matrix ACCEPT** (V1 REVISE
folded: disclaimer + per-W# re-key at 3A:102-110). 26/26 ACCEPT.

### 3B — MASTER-PLAN (10 new SKV18-D01..D10 + 4 carried)

The §13.7 NEW-wave block is exemplary: all 12 waves carry manual-LOC/risk, MASTER
alignment, a **same-wave consumer**, and a RED exit-gate falsifier (3B:144-157) —
the 3B-NEW-wave consumer requirement is fully MET. **D04 ACCEPT** (V1 REVISE
folded: −10700 dropped; per-wave sum −10685 cites SPEC:433-437). SKV18-D01..D03,
D05..D10 + 4 carried ACCEPT. 14/14 ACCEPT.

### 3C — LOCKS crystallisation (11 clauses) + dispositions (21) + v+1 diff

Per-clause cost matrix (3C:101-113) carries doc-LOC, risk, owning gate, consumer,
propagation count, hard-cap fit, fail-action — the model CH4 deliverable. 21/21
candidates disposed, 0 silent drops (LAC-2F-V3-03 DEFER carries a named re-entry
trigger + audit-scope note). **L13 clause ACCEPT** (V1 REVISE folded: ≈+217 noted
SK-V19-owned). **LAC-2D-V3-03 disposition ACCEPT** (V1 REVISE folded: the
unverified "SATISFIED at HEAD" claim replaced by a spot-cited live `Rewrite` impl;
CSP half deferred). The two PLANNED co-gate symbols
(`runtime_target_rows_collapsed`, `bbnf_simd_single_mask_convention`) are honestly
disclosed as PLANNED, not cited live — correct CH4 discipline. **v+1 diff ACCEPT**
(V1 REVISE folded: header re-cut, `git apply --check` clean; doc-only LOCKS
addendum, preserves 16 locks + 5 shapes, adds no lock/directive/substrate/sixth
shape). 11 clauses + 21 dispositions + diff = 33/33 ACCEPT.

### 3D — skinny-fold (12 deltas)

CH4 coverage matrix complete for all 12; net −10685/≈−10800 traced; monotonic-fold
direction preserved. **D12 ACCEPT** (V1 REVISE folded: CH4 cell scoped to the
skinny +1-line `RuntimeTarget: PartialEq` derive; 9-row scaling cross-ref'd to
SK-V19 R16 ≈+217). D01–D11 ACCEPT. 12/12 ACCEPT. (Note: 3D:155 W-PRUNE row still
states the bare "P3 7 replicas ≈ −5460" — this is the literal 6×910 and is
consistent with the SPEC's ≈−5500 breakdown, not a residual drift; the D12 delta
and front-matter carry the reconciled figure.)

### 3E — grammar-generalisation (7 new D12-D18 + 11 carried D01-D11)

The 7 new deltas carry LOC/risk/wave/propagation inline; D12 "no body charged,
cross-reference only" is exemplary anti-double-budget hygiene; D16 9-grammar matrix
correctly owns docs/gate (180-320 LOC) and defers lowerer carriers to 2D W7/W8/W9
via 3B/G-Omega. **Carried-D01..D11 ACCEPT** (V1 REVISE folded: disclaimer +
`sk_v18_extension_note` re-key at 3E:170-178). 18/18 ACCEPT.

### 3F — MIGRATION + HANDOFF (12 active deltas; MH-002 removed with supersession)

CH4 V6 coverage matrix complete; the five migration decisions priced (x86 −4500,
courier −910, replicas −5500, phantom decoration, css_types 66 LOC). **MH-009
ACCEPT** (V1 REVISE folded: P3 cited verbatim SPEC:435 ≈−5500, aligned across
§0.0 carrier / decisions table / collapse row / 3D −5460). MH-001/003/004/005/006/
007/008/010/011/012/013 ACCEPT. MH-013 correctly defers css_types relocate-or-delete
to SK-V19 (not charged to SK-V18). 12/12 ACCEPT.

## Cross-scope / refuted-route / silent-drop scan (REJECT bait — none found)

- **No SK-V19 cost laundered into a skinny figure**: every `≈+217` reference is
  scoped to SK-V19 / cross-ref / D11b / MP.SK19 — the inverse of the V1 defect,
  now fully discharged across 3A-D11b, 3C-L13, 3D-D12.
- **No sixth `BackendShape`**: 5 variants confirmed at `ir/src/lib.rs:341`;
  FactStream is the output-plane/substrate-target category (MASTER MP.NW6), not a
  6th shape — consistent.
- **No revived refuted route**: REDRESS 96/97/98 (scalar-cheaper-than-SIMD-cursor)
  fenced in 3C-L10 / 3A-D08; G6 is RETARGET-not-author; x86-closes-M5-row REFUTED
  (3A-D07/3D/3E); CollapsedStage is an inert REDRESS-fenced slot.
- **No silent drop**: 3C disposes 21/21; 3B retires its consumed SK-V15 deltas
  with stated rationale; 3F removes MH-002 with stated supersession (3F:15,:23,:62).
- **No W12/W13 overflow**: all references are explicit "no W12 spillover" guards;
  the manifest is exactly 12 waves at the skinny ceiling.
- **No wide edit disguised as doc-only**: the v+1 diff is a +27-net-line LOCKS
  addendum (doc text), applies cleanly, adds no lock/shape/substrate; the 3C/3A/3B
  cost matrices charge implementation bodies to their owning G/P waves, never to a
  doc gate.

## Residual observation (NOT a defect)

The ≈−10800 headline (cited verbatim from `sk-v18/SPEC.md:21,:61,:454`) and the
per-wave manifest sum ≈−10685 differ by ~115 LOC. This gap is INTERNAL TO THE
SPEC itself (the SPEC both headlines −10800 and contains the manifest rows summing
to −10685); the artefacts faithfully carry the SPEC headline verbatim AND honestly
reconcile the per-wave sum to −10685 under the `≈` tilde. This is correct CH4
discipline — the headline is a verbatim source cite, not an artefact fabrication —
and the V2 fold made it MORE precise than V1 by exhibiting the exact per-wave sum.
No correction required. (One exit-gate falsifier baseline, P2's "today 48" for
`measure_mbps|lightningcss_facts`, reads 64 under a whole-crate grep vs the
narrower warm-fixture surface; this is an exit-gate count, not a delta LOC budget
— P2's −700 budget is independently SPEC-anchored — so it is not a CH4 cost
defect.)

## Tally

Counting at the delta/clause/disposition granularity the lens enumerates:

- 3A: 26 ACCEPT (14 SK-V18 + 12 carried; D11 split + carried-matrix folded)
- 3B: 14 ACCEPT (10 new + 4 carried; D04 folded; §13.7 consumer requirement MET)
- 3C: 33 ACCEPT (11 clauses + 21 dispositions + 1 v+1 diff; L13/LAC-2D-V3-03/diff folded)
- 3D: 12 ACCEPT (D12 folded)
- 3E: 18 ACCEPT (7 new + 11 carried; carried-matrix folded)
- 3F: 12 ACCEPT (MH-009 folded; MH-002 removed-with-supersession)

ACCEPT = 115, REVISE = 0, REJECT = 0. All nine V1 CH4 REVISE findings were
independently re-verified as folded on disk; the regenerated packet is CH4-clean.
This is a defended all-ACCEPT (not a paper-close): every fold was checked against
the live tree, the v+1 diff applies, the load-bearing arithmetic (6×910=−5460,
x86=28, metalang=7, 5 shapes, 16 locks, 9 idents, 71-file census) resolves, and
the SK-V19/SK-V18 cost-boundary — the V1 defect class — is now precisely
attributed in every affected delta.

TALLY accept=115 revise=0 reject=0
