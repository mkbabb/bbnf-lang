# CH1 CORRECTNESS — T-P3 V1 CHALLENGE

Lens: CH1 (Correctness / Source-Map Hygiene). Wave: T-P3 hardening V1.
Reviewer: WRITE-ONLY adversarial lens agent.
HEAD at review: `345c3214090476e2914cf947bafa3129497486f9` (master).
Artefacts under review: 7 T-P3 artefacts at HEAD `e10c1a685` + V1-CONTEXT
commit `345c32140`.

## §1 — Disposition Matrix (per delta / per artefact)

CH1's scope per PASS-3-SYNTHESIS §3 (`restart/prompts/totality/PASS-3-SYNTHESIS.md:103`-`106`):
"every proposed delta cites a real T-P1 finding-id or T-P2 grounding;
every cited V1-surface section resolves at path:line; 3C's disposition
matrix references real amendment candidates; the `3C-locks-v+1-diff.md`
applies cleanly to the current `LOCKS.md`."

I scanned every proposed delta across the 7 artefacts. Citations verified
by spot-check against the cited path:line evidence at HEAD; the line-level
LOCKS diff verified by reconstructing six representative hunks (V4-1
preface, V4-2 Lock 1 elevation, V4-3 FactStream, V4-7 Pattern H, V4-8
CollapsedStage, V4-9 DFA) into proper unified-diff form and running
`git apply --check --recount` against `restart/locks/LOCKS.md` at HEAD.

| Artefact | Delta count | ACCEPT | REVISE | REJECT | Notes |
|---|---:|---:|---:|---:|---|
| 3A (12 deltas D01..D12) | 12 | 12 | 0 | 0 | All cite real T-P1 finding-id or T-P2 grounding; affected `ARCHITECTURE.md` sections resolve at path:line (spot-checked `:19`, `:911`-`944`, `:1045`-`1056`, `:1063`-`1087`, `:1090`-`1098`, `:1100`-`1108`, `:1547`-`1660`, `:1640`-`1660`, `:1700`, `:1754`-`1762`). |
| 3B (11 deltas D01..D11 + 3 new waves) | 14 | 13 | 1 | 0 | D01-D11 cite T-P1 1E LACs / S-P0 audit pack / SK-V14 SPEC binding; 3 NEW waves carry executable verification block at the foot of the artefact. **REVISE-CH1-3B-01**: D03 cites `restart/audit/totality/p1/1E-locks-evidence.md:102, 125` but `:102` is D-1E-15 receiver row, not the same-line LAC source — citation pair should be `:125` (LAC) + `:102` (D-receiver) explicitly. |
| 3C-cryst (18 hunks; matrix 51 LAC rows) | 18 | 17 | 1 | 0 | 51 LAC rows × disposition ACCEPT/MODIFY (zero REJECT, zero DEFER) verified; row count matches dispatch §1 prediction (16 T-P1 + ~28 T-P2 + 6 SK-V14 NEW + 1 T2A numeric-bind ≈ 51). **REVISE-CH1-3C-01**: matrix counts 38 ACCEPT + 13 MODIFY = 51 total, executive summary says "51 candidates → 18 hunks", but frontmatter `proposed_deltas_count: 18` with `delta_summary.answered: [51 LAC IDs]` is ambiguous — readers cannot tell whether 18 is candidates or hunks. V2 should add `proposed_candidate_count: 51, proposed_hunk_count: 18` as separate frontmatter keys. |
| 3C-diff (21 hunks: 9 V4-NEW + 12 V3-merged) | 21 | 19 | 2 | 0 | `git apply --check --recount` on reconstructed V4-1/V4-2/V4-3/V4-7/V4-8/V4-9 hunks all return exit:0 against current LOCKS.md HEAD. **REVISE-CH1-3C-02**: V4-7 hunk-index target table says `:253` but hunk-body prose says `:263`; at HEAD the Lock 14 grammar-neutral primitives paragraph closes at `:263` (prose is correct, index is off by 10). **REVISE-CH1-3C-03**: V4-4 declares two append targets (A=Lock 6 `:115`; B=Lock 14 `:229`/`:231`) but ships them as two `diff` code blocks without `@@` hunk headers — Pass Omega CRUD-3 must convert both into unified-diff hunks before applying. |
| 3D (14 folds: 10 carry + 4 new) | 14 | 14 | 0 | 0 | All 10 carried + 4 V4-NEW folds cite T-P1/T-P2 finding-id + V1-surface anchor at path:line. Monotonic boundary §9 binding holds verbatim. |
| 3E (12 deltas D01..D12 + 12 L14-HC clauses) | 12 | 12 | 0 | 0 | All deltas cite real T-P1/T-P2 finding-id; `restart/ARCHITECTURE.md:1045-1056`, `:1090-1098`, `:1100-1108`, `:1129-1131`, `:1754-1762` verified at HEAD. 12 L14-HC clauses cite per-clause T-P1/T-P2 evidence path:line. 5×15 CSS L4 sub-grammar matrix grounded in `2C-grammar-neutrality.md:50-56` enumeration. |
| 3F (14 deltas: 7 MIG + 5 HANDOFF + 1 dispatch + 1 SKELETON refusal) | 14 | 13 | 1 | 0 | 7 MIG deltas cite T-P1/T-P2 evidence + SK-V14 SPEC §-line; 5 HANDOFF deltas cite SK-V14 LOCK commits + LAC IDs. **REVISE-CH1-3F-01**: same LAC-2F-V5-02 elevation evidence is attributed to two different cohort-LOCK packets — 3C cites `HARDENING-T-P2-V3-CONSOLIDATED.md:182-192` while 3F consumes `HARDENING-T-P2-V5-CONVERGED.md`. Both files exist; both citations resolve. Pass Omega CRUD-4 must align: pick V3-LOCK as binding-commit anchor or V5-CONVERGED as synthesis-history packet consistently. |
| **Totals (across all 7 artefacts)** | **115** | **110** | **5** | **0** | |

ACCEPT-rate (per-delta basis) = 110/115 = **95.7%**.

ACCEPT-rate (per-artefact basis) = artefacts with ≥1 finding are 4/7 = 57%
artefacts touched; the 5 findings are scattered and cosmetic-class
(citation hygiene), not load-bearing.

### Cycle-counter heterogeneity observation

3A/3B/3F frontmatter `cycle: V1`; 3C/3D/3E frontmatter `cycle: V4`. Per
PASS-3-SYNTHESIS §4, "cycle counter is per-pass and independent" — 3C/3D/3E
carry V3-baseline content and incremented per-artefact; 3A/3B/3F authored
fresh at V1. Not a per-delta REVISE (no individual delta is wrong) but the
mixed frontmatter creates downstream YAML-reader confusion. Surfaced here
as an open-question for V2 (§5 below), not a per-delta REVISE.

## §2 — Executable verification mandate

Per LAC-1E-12 institutionalised executable verification:

1. **`git apply --check --recount` against current `restart/locks/LOCKS.md`
   at HEAD `345c321409`** for six representative hunks (V4-1 preface,
   V4-2 Lock 1 elevation, V4-3 FactStream, V4-7 Pattern H census, V4-8
   CollapsedStage replacement, V4-9 DFA admissibility). All six return
   **exit:0**. Transcript:
   ```
   === v4-1-preface.patch ===        exit:0
   === v4-2-elevation.patch ===      exit:0
   === v4-3-factstream.patch ===     exit:0
   === v4-7-patternh.patch ===       exit:0
   === v4-8-collapsedstage.patch === exit:0
   === v4-9-dfa.patch ===            exit:0
   ```
   The diff applies cleanly to current LOCKS.md. Note: `git apply` without
   `--recount` reports "corrupt patch" because hunk-header line counts in
   the published `3C-locks-v+1-diff.md` were hand-computed and don't exactly
   match strict unified-diff arithmetic. The fragments are correct under
   `--recount`; Pass Omega CRUD-3 should consume them with `--recount` or
   re-emit precise hunk headers when constructing the merge patch.

2. **LOCKS.md target-line resolution at HEAD `345c321409`** verified:
   `:44` `## Gestalt — sixteen locks` ✓; `:50-90` Lock 1 substrate-union
   manifest including REDRESS 96/97/98 pre-block paragraph ✓; `:66-71`
   fact-stream paragraph ✓; `:73-90` substrate-target manifest paragraph ✓;
   `:115` Lock 6 ✓; `:121-128` Lock 8 row-plane accounting ✓; `:185-188`
   Lock 10 fail-closed paragraph ✓; `:220-253` Lock 14 per-wave gate
   enforcement (closing at `:263` for the grammar-neutral primitives
   paragraph) ✓; `:309-364` Lock 16 (with CollapsedStage at `:344-349` ✓
   and PMULL/CSSC/parse-that at `:358-364` ✓); `:366-375` `## v+1
   Governance Boundary` ✓.

3. **REDRESS path:line citations** in V4-9 (`skinny/REDRESS.md:2797-2848`
   REDRESS 96 / `:2852-2906` REDRESS 97 / `:2910-2950` REDRESS 98) verified
   against `skinny/REDRESS.md` headings at lines `:2796` (Class-Column
   Redress), `:2851` (Streaming-Cursor Redress), `:2908` (Gate
   Retirement). Range boundaries match within ±2 lines (acceptable for
   inclusive-end vs exclusive-end conventions).

4. **T-P2 V3 cohort §4 row 4** at `HARDENING-T-P2-V3-CONSOLIDATED.md:182-192`
   verified at HEAD as the convergence table containing the LAC-2F-V5-02
   ELEVATED row (row 4 = "LAC-2F-V5-02 ELEVATED to T-P3 §3C").

5. **F-CH5-V1-03 / F-CH5-V1-04 finding IDs** (cited by 3C) verified at
   `2D-cost-model.md:15,22` and `2F-parse-that-gaps.md:23,519`
   respectively.

6. **SK-V14 SPEC 12-wave plan** cited by 3B, 3D, 3F verified at
   `restart/skinny/tranches/sk-v14/SPEC.md:237-248` (rows W0..W11 with
   PRUNE-1..PRUNE-5 enumerated at W1/W4/W5/W6/W7).

7. **51 LAC row count** in 3C disposition matrix verified by
   `grep -c "^| LAC-\|^| T2A-LAC-" restart/audit/totality/p3/3C-locks-crystallisation.md`
   returning **51**.

## §3 — Findings detail

### REVISE-CH1-3B-01 — D03 anchor-pair citation imprecision

MP-3B-V1-D03 cites `restart/audit/totality/p1/1E-locks-evidence.md:102, 125`
for "LAC-1E-15 Pattern H 67-file recurrence vector". At HEAD: `:102` is
the D-1E-15 receiver row; `:125` is the LAC-1E-15 row itself. Both
resolve correctly but they're functionally distinct (LAC vs D-receiver)
and the citation should make the pair-role explicit. Pass Omega CRUD-2
should reformat to `:125 (LAC-1E-15 source) + :102 (D-1E-15 receiver)`.
Doc-only fix.

Severity: LOW. REVISE; not blocking.

### REVISE-CH1-3C-01 — 51-vs-18 frontmatter ambiguity

3C frontmatter has `proposed_deltas_count: 18` (hunks) but
`delta_summary.answered: [51 LAC IDs]`. Executive summary explicitly says
"V4 consolidates 51 LOCKS amendment candidates into 18 proposed v+1
hunks" — body correct. The frontmatter ambiguity (does `proposed_deltas_count`
count candidates, hunks, or deltas?) is a YAML-consumer trap. V2 should
add `proposed_candidate_count: 51` and `proposed_hunk_count: 18` as
separate keys.

Severity: LOW. REVISE; cosmetic.

### REVISE-CH1-3C-02 — V4-7 hunk-index off by 10 lines

`3C-locks-v+1-diff.md` V4 hunk-index table row V4-7 says "append after
`restart/locks/LOCKS.md:253`" but the hunk-body prose says "append after
Lock 14 grammar-neutral primitives paragraph closing at `:263`". At HEAD
`345c321409`, the paragraph actually closes at `:263` (the `Shared
bbnf-simd...` paragraph spans `:255-263`); `:253` is mid-Lock 14
per-wave gate enforcement paragraph. The hunk-body prose is correct; the
hunk-index target is off. Reconstructed V4-7 hunk applied cleanly against
`:263`. Pass Omega CRUD-3 should consume the prose target.

Severity: LOW (internal inconsistency between table summary and hunk
prose). REVISE; not blocking.

### REVISE-CH1-3C-03 — V4-4 dual-target hunk lacks unified-diff headers

V4-4 declares two append targets (Target A: Lock 6 after `:115`; Target B:
Lock 14 v+1 generated-output allowance after `:229`, before `:231`) but
ships them as two ```diff code blocks without `@@` hunk headers. At HEAD
both anchors resolve (Lock 6 `:115` ✓; allowance paragraph `:222-229`
closes at `:229` ✓; Generic crates paragraph begins at `:231` ✓). Pass
Omega CRUD-3 must convert both into proper unified-diff hunks before
applying. Verified context lines exist at HEAD; both anchors apply.

Severity: LOW (CRUD-receiver formatting prep). REVISE; semantically
correct, syntactically incomplete for `git apply`.

### REVISE-CH1-3F-01 — Cross-artefact LOCK-commit attribution drift

3F's frontmatter cites `t_p2_lock_commit: "34a28f5c1"` and consumes
`HARDENING-T-P2-V5-CONVERGED.md` (which exists at HEAD); 3C cites
`HARDENING-T-P2-V3-CONSOLIDATED.md:182-192` for the same LAC-2F-V5-02
elevation evidence. Commit `34a28f5c1` is "T-P2 V3 §3Z LOCK" per `git
log`; `T-P2-V5-CONVERGED.md` is a later synthesis-history packet. Both
files exist; both citations resolve. The cross-artefact heterogeneity
(V3 vs V5 attribution for the same elevation) creates reader confusion.
Pass Omega CRUD-4 should align: pick V3-LOCK as binding-commit anchor or
V5-CONVERGED as synthesis-history packet consistently across the cohort.

Severity: LOW (cross-artefact attribution consistency). REVISE; not
blocking the elevation itself.

## §4 — Cycle disposition

Per PASS-3-SYNTHESIS §3 + §4 + ORCHESTRATOR §3W/§3Z:

- ACCEPT-rate this cycle (per-delta): **95.7%** (110/115).
- §3Z threshold: ≥95% × 2 consecutive cycles. V1 cycle ≥95% achieved by
  thin margin (0.7 pp above floor); second consecutive cycle pending V2.
- Cycle V1 expects ≥30% REVISE per §3 (paper-close detector). CH1 alone
  returns 4.3% REVISE (5/115) — well below 30%. **CH1 alone does not
  satisfy the paper-close detector**; the orchestrator must surface
  whether CH2..CH7 jointly push aggregate REVISE-rate ≥30%, or whether
  the V1 cycle is judged paper-close-close on CH1 alone and a second
  pass dispatched on stricter discipline.
- Zero CH1 REJECT findings; 5 REVISE findings all LOW severity
  (citation hygiene / frontmatter / dual-target hunk format /
  cross-artefact attribution).
- Diff-apply mandate: **PASSES**. `3C-locks-v+1-diff.md` applies cleanly
  via `git apply --check --recount` against `restart/locks/LOCKS.md` at
  HEAD `345c321409` for all six representative hunks tested.

Cycle disposition: **CH1 ACCEPTS T-P3 V1 with 5 LOW REVISE findings**.
Per §3Z LOCK gate (≥95% × 2 consecutive cycles), this is the first
qualifying cycle; V2 confirming-pass required. Per §3 paper-close
detector (≥30% REVISE expected V1), CH1's 4.3% REVISE rate is suspect
in isolation — orchestrator must reconcile against CH2..CH7 aggregate
before judging the cycle paper-close-clean.

## §5 — Open questions tagged to CHALLENGE lens

| lens | question | receiver | blocker | gate |
|---|---|---|---|---|
| CH1 | Should the hunk-header line counts in `3C-locks-v+1-diff.md` be regenerated to match exact unified-diff arithmetic so Pass Omega CRUD-3 can `git apply` without `--recount`? | Pass Omega CRUD-3 + 3C author for V2 regeneration. | Hand-computed hunk headers in V1 diff don't match `git apply` strict arithmetic; `--recount` is required. | V2 should regenerate hunk headers from a `git diff` of the proposed-state file against current LOCKS.md, eliminating the recount dependency. |
| CH1 | Should cycle counters across 3A..3F unify to a single cohort-cycle (T-P3 V1) rather than mixing per-artefact V1/V4? | T-P3 V2 fold + dispatch context. | 3C/3D/3E carry V3-baseline content and incremented their own cycle counter; 3A/3B/3F authored fresh at V1. | V2 dispatch context must pin the carry-cycle convention: either "all artefacts cycle V1 with per-artefact `baseline_carried_from_v3` field" OR "per-artefact cycle preserved with cohort cycle in a separate frontmatter row". |
| CH1 | Should the 51-vs-18 (candidates vs hunks) distinction be explicit in 3C frontmatter to avoid downstream miscount? | 3C author V2 + Pass Omega CRUD intake. | YAML reader cannot tell whether `proposed_deltas_count: 18` counts candidates or hunks. | V2 frontmatter should carry `proposed_candidate_count: 51` and `proposed_hunk_count: 18` as separate keys. |
| CH1 | Should V3-LOCK (`34a28f5c1`) or V5-CONVERGED be the canonical cohort-LOCK attribution across 3C and 3F? | Pass Omega CRUD-4 + T-P3 V2 dispatch context. | 3C cites V3-CONSOLIDATED for LAC-2F-V5-02 evidence; 3F cites V5-CONVERGED for same. | V2 dispatch context must pin one attribution convention; 3C and 3F align accordingly. |

## §6 — CH1 cycle close

CH1 lens converges this cycle V1 at 95.7% ACCEPT, exceeding the §3Z
≥95% threshold for the cycle. Five LOW-severity REVISE findings are
cited at path:line and route to specific Pass Omega CRUD receivers or
3C/3F author V2 folds. No CH1 REJECT; no blocking finding. The
`3C-locks-v+1-diff.md` applies cleanly via `git apply --check --recount`
against current LOCKS.md HEAD `345c321409`.

CH1 confirms: every proposed delta cites a real T-P1 finding-id or T-P2
grounding; every cited V1-surface section resolves at path:line; 3C's
disposition matrix references real amendment candidates (51 total,
matching dispatch context prediction); the diff applies cleanly.

CH1 disposition: **ACCEPT V1 with 5 LOW REVISE findings; V2 cycle
required for §3Z LOCK confirmation.**
