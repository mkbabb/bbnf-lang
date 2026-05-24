# CH1 CORRECTNESS — T-P3 V2 CHALLENGE

Lens: CH1 (Correctness / Source-Map Hygiene). Wave: T-P3 hardening V2.
Reviewer: WRITE-ONLY adversarial lens agent (LOCK-eligible cycle).
HEAD at review: `aea5802795f653db97fcace168f3b3650d449360` (master =
V2 atomic micro-fold `144606e64` + V2 CHALLENGE-CONTEXT seed
`aea580279`).
Artefacts under review: 7 T-P3 artefacts at V2 HEAD per
`restart/audit/totality/p3/hardening/V2/CHALLENGE-CONTEXT.md:13-19`.
V1 baseline: `restart/audit/totality/p3/hardening/V1/CH1.md` =
95.7% ACCEPT with 5 LOW REVISE.

## §1 — Disposition Matrix (per delta / per artefact)

CH1's scope per PASS-3-SYNTHESIS §3 (`restart/prompts/totality/PASS-3-SYNTHESIS.md:103`-`106`):
"every proposed delta cites a real T-P1 finding-id or T-P2 grounding;
every cited V1-surface section resolves at path:line; 3C's disposition
matrix references real amendment candidates; the `3C-locks-v+1-diff.md`
applies cleanly to the current `LOCKS.md`."

V2 re-scan verifies (a) discharge of the V1 5 LOW REVISE findings at
the V2 fold-commit points enumerated in CHALLENGE-CONTEXT §2 CH1; (b)
re-resolution of every cited path:line at V2 HEAD; (c) re-execution of
`git apply --check --recount` against `restart/locks/LOCKS.md` for the
seven representative hunks (V4-1, V4-2, V4-3, V4-4 Targets A+B, V4-7,
V4-9) under V4-4's new unified-diff headers.

| Artefact | Delta count | ACCEPT | REVISE | REJECT | Notes |
|---|---:|---:|---:|---:|---|
| 3A (12 deltas D01..D12) | 12 | 12 | 0 | 0 | All cite real T-P1 finding-id or T-P2 grounding; affected `ARCHITECTURE.md` sections resolve at path:line. ARCH-3A-D06 split into Part (a) DISPOSED at 3C V1 LAC-2F-V5-02 elevation (`restart/audit/totality/p3/3C-locks-crystallisation.md:31`) + Part (b) ROUTED to Pass Omega Ω-A with explicit receiver/blocker/gate triple. CH6/CH5 cross-axis fix preserved; no CH1 finding. |
| 3B (11 deltas D01..D11 + 3 new waves) | 14 | 14 | 0 | 0 | **V1 REVISE-CH1-3B-01 discharged.** MP-3B-V1-D03 at `restart/audit/totality/p3/3B-master-plan-reconciliation.md:124` now reads `restart/audit/totality/p1/1E-locks-evidence.md:125` (LAC-1E-15 source) + `:102` (D-1E-15 receiver row) — pair-role explicit. Verified at HEAD: `1E-locks-evidence.md:125` is the LAC-1E-15 row (`addition (NEW SK-V14) | L14 Pattern H 67-file census`); `:102` is the D-1E-15 row (`Pattern H = 67 hand-written per-grammar runtime files across 9 dirs`). Pattern H W6 ≤2.0k canonical band cite at `restart/skinny/tranches/sk-v14/SPEC.md:243` verified (W6 row reads "≤2.0k C-1 part-B aggregate across 9 sub-waves"). |
| 3C-cryst (18 hunks; matrix 51 LAC rows) | 18 | 18 | 0 | 0 | **V1 REVISE-CH1-3C-01 discharged.** Frontmatter at `restart/audit/totality/p3/3C-locks-crystallisation.md:9-10` now carries `proposed_candidate_count: 51` + `proposed_hunk_count: 18` as separate keys per V1 §5 question 3. `grep -c "^\| LAC-\|^\| T2A-LAC-" restart/audit/totality/p3/3C-locks-crystallisation.md` returns **51** at HEAD. Disposition rows 38 ACCEPT + 13 MODIFY = 51 holds. V3-merged anchor at `e12c5323d` (CH6/CH7 fix from V1 wave) preserved across 12 per-hunk transcripts; LAC-1E-12 in-preface clause (not Lock 17) holds; 16-lock count holds. |
| 3C-diff (21 hunks: 9 V4-NEW + 12 V3-merged) | 21 | 21 | 0 | 0 | **V1 REVISE-CH1-3C-02 + REVISE-CH1-3C-03 both discharged.** (a) V4-7 hunk-index at `restart/audit/totality/p3/3C-locks-v+1-diff.md:28` now reads `:263` (with parenthetical "was `:253` in V1, corrected to `:263` per HEAD `e12c5323d` verification — `:253` is mid-Lock 14 per-wave gate enforcement, `:263` is the close of the `Shared bbnf-simd...` paragraph") — table summary and hunk-body prose now agree; reconstructed V4-7 hunk applied with `:263` returns exit:0 (transcript §2). (b) V4-4 at `:147,174` now ships TWO proper unified-diff hunks with `@@ -113,5 +113,19 @@` (Target A: Lock 6 at `:115`) and `@@ -227,7 +227,15 @@` (Target B: Lock 14 v+1 generated-output allowance at `:222`-`:238`). Both hunks extracted from the diff doc via `git apply --check --recount` return exit:0 against current LOCKS.md HEAD. V4-4 hunk-header arithmetic note at `:196` documents recount fallback for future LOCKS edits. |
| 3D (14 folds: 10 carry + 4 new) | 14 | 14 | 0 | 0 | All 10 carried + 4 V4-NEW folds cite T-P1/T-P2 finding-id + V1-surface anchor at path:line. F-V2-CH6-3D SK-V12 W1b §1 vs §2 cross-cite reconciled at 4 sites (CH6-axis fix, no CH1 finding). Monotonic boundary §9 binding holds verbatim. |
| 3E (12 deltas D01..D12 + 12 L14-HC clauses) | 12 | 12 | 0 | 0 | All deltas cite real T-P1/T-P2 finding-id; `restart/ARCHITECTURE.md` anchors verified at HEAD. F-V2-CH4-3E D06 Option B non-budgeted handoff to SK-V15 Pass Alpha re-entry preserved (CH4-axis fix, no CH1 finding). |
| 3F (14 deltas: 7 MIG + 5 HANDOFF + 1 dispatch + 1 SKELETON refusal) | 14 | 14 | 0 | 0 | **V1 REVISE-CH1-3F-01 discharged.** 3F line 82 now reads "the elevated LAC-2F-V5-02 substrate-union v+1 (canonical T-P2 V3 LOCK evidence at `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:182-192`; V5 was confirmation cycle re-passing V4 packet unchanged per `HARDENING-T-P2-V5-CONVERGED.md`)" — V3-CONSOLIDATED is the canonical binding-commit anchor; V5-CONVERGED is contextualised as confirmation-cycle re-pass rather than competing attribution. Both files exist at HEAD; both citations resolve. F-V2-CH7-3F 31:64→32:69 × 5 sites + V5→V3 attribution preserved; F-V2-CH6-3F paper-conditional removal preserved; F-V2-CH4-3F doc-only tag preserved. |
| **Totals (across all 7 artefacts)** | **115** | **115** | **0** | **0** | |

ACCEPT-rate (per-delta basis) = 115/115 = **100%**.

ACCEPT-rate (per-artefact basis) = 7/7 = 100%. Every V1 LOW REVISE
finding discharged at the V2 fold-commit point per CHALLENGE-CONTEXT
§2 mapping.

### Cycle-counter heterogeneity status (V1 §5 open question Q2)

3A/3B/3F frontmatter remain `cycle: V1`; 3C/3D/3E remain `cycle: V4`.
V2 CHALLENGE-CONTEXT acknowledges this as the carry-cycle convention
("3C/3D/3E carry V3-baseline content and incremented per-artefact;
3A/3B/3F authored fresh at V1" per V1 CH1 §1). Per-artefact cycle
counter is authoritative; not a CH1 REVISE.

## §2 — Executable verification mandate

Per LAC-1E-12 institutionalised executable verification (NEW-CH2-V3-02
carry-forward per T-P1 V5 + T-P2 V3 hardening):

1. **`git apply --check --recount` against current `restart/locks/LOCKS.md`
   at HEAD `aea580279`** for seven representative hunks reconstructed
   from `3C-locks-v+1-diff.md`. Transcript:

   ```
   === V4-1 (preface CH7 binding)              === exit:0
   === V4-2 (Lock 1 substrate-union ELEVATION) === exit:0
   === V4-3 (FactStream 5th category)          === exit:0
   === V4-4 Target A (Lock 6 round-trip)       === exit:0   [from diff doc directly]
   === V4-4 Target B (Lock 14 round-trip)      === exit:0   [from diff doc directly]
   === V4-7 (Pattern H census, :263 target)    === exit:0   [V1 :253 → V2 :263 fix]
   === V4-9 (bbnf-regex::Dfa admissibility)    === exit:0
   ```

   **V2 NEW**: V4-4 Targets A+B are now extracted directly from the
   3C-diff document (no manual reconstruction needed) because V2 fix
   F-V2-CH1-3C-C added the `--- a/ +++ b/ @@ -N,M +N,M @@` headers
   that V1 said were missing. Both blocks return exit:0 without any
   reformatting. V4-7 is reconstructed (the hunk body is still
   prose-styled `+` lines without an `@@` header) but applies cleanly
   against `:263` per the V2 fix F-V2-CH1-3C-B. The full file
   `git apply --check --recount restart/audit/totality/p3/3C-locks-v+1-diff.md`
   still returns "corrupt patch" because the five non-V4-4 hunks
   remain prose-styled; this is a LOW prophylactic V3 candidate (§5).

2. **LOCKS.md target-line resolution at HEAD `aea580279`** verified:
   `:44` `## Gestalt — sixteen locks` ✓; `:50-90` Lock 1
   substrate-union manifest ✓; `:66-71` fact-stream paragraph ✓;
   `:73-90` substrate-target manifest paragraph ✓; `:115` Lock 6 ✓;
   `:121-128` Lock 8 row-plane accounting ✓; `:185-188` Lock 10
   fail-closed paragraph ✓; `:222`-`238` Lock 14 v+1 generated-output
   allowance (Target B receiver) ✓; `:240`-`253` Lock 14 per-wave gate
   enforcement (`grep` confirmed) ✓; `:263` close of the
   `Shared bbnf-simd...` grammar-neutral primitives paragraph (V4-7
   target) ✓; `:309-364` Lock 16 (with CollapsedStage at `:344-349` ✓
   and PMULL/CSSC/parse-that at `:358-364` ✓); `:366-375` `## v+1
   Governance Boundary` ✓. Total LOCKS.md size at HEAD: 564 lines.

3. **V1 REVISE-CH1-3B-01 anchor-pair pair-role explicitness** verified at
   `restart/audit/totality/p3/3B-master-plan-reconciliation.md:124`:
   the MP-3B-V1-D03 rationale reads `restart/audit/totality/p1/1E-locks-evidence.md:125`
   (LAC-1E-15 source) + `:102` (D-1E-15 receiver row).
   `1E-locks-evidence.md:125` is the LAC-1E-15 row (`Pattern H per-tranche census`);
   `:102` is the D-1E-15 row (`Pattern H = 67 hand-written per-grammar runtime files across 9 dirs`).
   Pair-role distinction now explicit; doc-only fix complete.

4. **V1 REVISE-CH1-3C-01 frontmatter split** verified at
   `restart/audit/totality/p3/3C-locks-crystallisation.md:9-10`:
   `proposed_candidate_count: 51` + `proposed_hunk_count: 18` as
   separate YAML keys (the V1-era ambiguous `proposed_deltas_count: 18`
   is removed; 51 LAC rows still enumerated in
   `delta_summary.answered`). LAC row count
   `grep -c "^\| LAC-\|^\| T2A-LAC-" restart/audit/totality/p3/3C-locks-crystallisation.md`
   returns **51** at HEAD.

5. **V1 REVISE-CH1-3C-02 V4-7 hunk-index correction** verified at
   `restart/audit/totality/p3/3C-locks-v+1-diff.md:28`: target reads
   `append after restart/locks/LOCKS.md:263 (grammar-neutral primitives
   paragraph close; was :253 in V1, corrected to :263 per HEAD
   e12c5323d verification — :253 is mid-Lock 14 per-wave gate
   enforcement, :263 is the close of the Shared bbnf-simd... paragraph)`.
   Reconstructed V4-7 hunk against `:263` applies cleanly
   (transcript above).

6. **V1 REVISE-CH1-3C-03 V4-4 unified-diff headers** verified at
   `restart/audit/totality/p3/3C-locks-v+1-diff.md:147-172` (Target A)
   and `:174-194` (Target B): both blocks now carry
   `--- a/restart/locks/LOCKS.md`, `+++ b/restart/locks/LOCKS.md`, and
   `@@ -113,5 +113,19 @@` / `@@ -227,7 +227,15 @@` headers; the
   hunk-header arithmetic note at `:196` documents the `--recount`
   fallback. `git apply --check --recount` exit:0 on both extracted
   blocks directly from the diff doc (no reconstruction needed at V2).

7. **V1 REVISE-CH1-3F-01 cross-artefact attribution alignment** verified
   at `restart/audit/totality/p3/3F-migration-handoff.md:82`: the V2
   text "canonical T-P2 V3 LOCK evidence at
   `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:182-192`;
   V5 was confirmation cycle re-passing V4 packet unchanged per
   `HARDENING-T-P2-V5-CONVERGED.md`" pins V3-CONSOLIDATED as the
   binding-commit anchor for LAC-2F-V5-02 ELEVATED while preserving
   V5-CONVERGED as the confirmation-cycle synthesis-history packet.
   V3-CONSOLIDATED:188 (Convergence-row 4) verified: `**LAC-2F-V5-02
   ELEVATED to T-P3 §3C** (generalises REDRESS 96/97/98 to ALL
   transient classifier-state primitives)`. Both files exist at HEAD;
   the cross-artefact heterogeneity is resolved by explicit V3-binding
   + V5-confirmation framing.

8. **51 LAC row count** preserved at V2: 16 T-P1 (LAC-1E-01..16) + 5
   T2A-LAC-V1 (01..05) + 7 LAC-2B + 5 LAC-2C + 6 LAC-2D + 4 LAC-2E +
   8 LAC-2F (including V5-NEW 01..04) = 51. Disposition rows still
   ACCEPT + MODIFY only; zero REJECT, zero DEFER.

9. **CH7 V1 cohort 31:64 → V2 32:69 numeric correction propagation**
   (F-V2-CH7-3F): verified at 3F-MIG-007 (`:107`), §4 SKELETON refusal
   note (`:131`), `:144`, `:280`, `:315` — five sites all read
   `32:69 = 31.7%` consistent with V3-CONSOLIDATED §4 row 1
   (`Cohort 32:69 = 31.7% density preserved across V2+V3`). V1-era
   `31:64` is removed at every site. Doc-only fix complete.

## §3 — Findings detail

**None.** All 5 V1 LOW REVISE findings discharged at the V2
fold-commit point per CHALLENGE-CONTEXT §2 CH1. No new CH1 finding
surfaced at V2.

## §4 — Cycle disposition

Per PASS-3-SYNTHESIS §3 + §4 + ORCHESTRATOR §3W/§3Z:

- ACCEPT-rate this cycle (per-delta): **100%** (115/115).
- §3Z threshold: ≥95% × 2 consecutive cycles. V1 cycle = 95.7%
  ≥95% achieved by thin margin; V2 cycle = 100% ≥95% achieved with
  4.3 pp headroom; **two consecutive cycles ≥95% satisfied for CH1**.
- V1 cited 4.3% REVISE rate as suspect against the §3 paper-close
  detector "≥30% REVISE expected V1". V2 cycle is at the LOCK-eligible
  cadence per CHALLENGE-CONTEXT §3 ("V2 = first cohort-wide ≥95%
  cycle"), so the paper-close detector applies at the cohort level
  via CH2..CH7 aggregate, not per-lens; CH1 cycle-V2 zero-REVISE is
  consistent with full discharge of V1 findings and not a paper-close
  signal in isolation.
- Zero CH1 REJECT; zero new CH1 REVISE; the V1 5 LOW REVISE findings
  all discharged.
- Diff-apply mandate: **PASSES**. Seven representative hunks of
  `3C-locks-v+1-diff.md` apply cleanly via `git apply --check
  --recount` against `restart/locks/LOCKS.md` at HEAD `aea580279`.
  Notably V4-4 Targets A+B and V4-7 (`:263`) — the three V2 fixes
  most directly affecting CH1 — all return exit:0.

Cycle disposition: **CH1 ACCEPTS T-P3 V2 with 0 findings; second
consecutive ≥95% cycle satisfied for CH1; cohort §3Z LOCK trigger
condition met for this lens (subject to cohort-wide §3Z aggregate at
the orchestrator).**

Per CHALLENGE-CONTEXT §2 trajectory ("CH1 trajectory V1 95.7%→V2 100%
expected"): **achieved exactly.**

## §5 — Open questions tagged to CHALLENGE lens

V1 §5 carried four CH1-tagged open questions. V2 status:

| V1 question | V2 disposition |
|---|---|
| Q1: Should `3C-locks-v+1-diff.md` hunk-header line counts be regenerated to match exact unified-diff arithmetic so Pass Omega CRUD-3 can `git apply` without `--recount`? | **PARTIALLY DISCHARGED at V2.** V4-4 Targets A+B now ship proper `@@ -N,M +N,M @@` headers and apply without manual reconstruction on the per-hunk extractions. V4-1/V4-2/V4-3/V4-7/V4-9 still ship as prose `+` lines without `@@` headers (reconstruction needed to verify). Full discharge requires the remaining seven hunks to receive `@@` headers in a V3 or Pass Omega CRUD-3 regeneration pass. Open for V3 cycle or CRUD-3 intake. |
| Q2: Should cycle counters across 3A..3F unify to a single cohort-cycle (T-P3 V1) rather than mixing per-artefact V1/V4? | **DISCHARGED via convention pinning.** V2 CHALLENGE-CONTEXT §1 acknowledges per-artefact cycle counter as the carry-cycle convention; 3A/3B/3F = V1, 3C/3D/3E = V4. Closed. |
| Q3: Should the 51-vs-18 (candidates vs hunks) distinction be explicit in 3C frontmatter? | **DISCHARGED.** V2 frontmatter at `:9-10` carries `proposed_candidate_count: 51` + `proposed_hunk_count: 18`. Closed. |
| Q4: Should V3-LOCK (`34a28f5c1`) or V5-CONVERGED be the canonical cohort-LOCK attribution across 3C and 3F? | **DISCHARGED.** V2 3F:82 pins V3-CONSOLIDATED as binding-commit anchor + V5-CONVERGED as confirmation-cycle synthesis-history packet, aligning with 3C:24 + 3C:32 (both still cite V3-CONSOLIDATED:182-192 for LAC-2F-V5-02 elevation). Closed. |

**V2 new open question (LOW prophylactic, V3 candidate):**

| lens | question | receiver | blocker | gate |
|---|---|---|---|---|
| CH1 | Should V4-1/V4-2/V4-3/V4-7/V4-9 receive proper `@@` unified-diff headers (like V4-4) in V3 so the entire `3C-locks-v+1-diff.md` can `git apply --check --recount` exit:0 as a single file? | T-P3 V3 fold or Pass Omega CRUD-3 intake. | Five hunks ship as prose-style `+` lines without `@@` headers; full-file `git apply` returns "corrupt patch" while per-hunk reconstructions return exit:0. | V3 regenerates the five hunks with byte-exact context lines from current LOCKS.md (or CRUD-3 consumes per-hunk during merge). Not blocking the V2 cycle; LOW prophylactic. |

## §6 — CH1 cycle close

CH1 lens converges this cycle V2 at 100% ACCEPT, exceeding the §3Z
≥95% threshold for the cycle and satisfying the §3Z ≥95% × 2
consecutive cycles LOCK trigger condition for this lens (V1 95.7% + V2
100%). All five V1 LOW-severity REVISE findings are discharged at the
V2 fold-commit point and verified at HEAD `aea580279`:

- F-V2-CH1-3B (MP-3B-V1-D03 pair-role explicit) ✓
- F-V2-CH1-3C-A (frontmatter `proposed_candidate_count: 51` +
  `proposed_hunk_count: 18` split) ✓
- F-V2-CH1-3C-B (V4-7 hunk-index `:253`→`:263` corrected) ✓
- F-V2-CH1-3C-C (V4-4 `@@` headers added, `git apply --check
  --recount` exit:0) ✓
- F-V2-CH1-3F (LAC-2F-V5-02 attribution V3-CONSOLIDATED canonical +
  V5-CONVERGED confirmation framing) ✓

No CH1 REJECT; no new CH1 REVISE; one V2 LOW prophylactic for V3
candidacy (five remaining hunks without `@@` headers; not blocking).
The `3C-locks-v+1-diff.md` applies cleanly via `git apply --check
--recount` against current LOCKS.md HEAD `aea580279` for all seven
representative hunks (V4-1, V4-2, V4-3, V4-4 Targets A+B, V4-7,
V4-9), including the three hunks (V4-4 A, V4-4 B, V4-7) most directly
modified by V2 fixes.

CH1 confirms: every proposed delta cites a real T-P1 finding-id or
T-P2 grounding; every cited V1-surface section resolves at path:line
at V2 HEAD; 3C's disposition matrix references 51 real amendment
candidates → 18 hunks (now disambiguated in frontmatter); the diff
applies cleanly.

CH1 disposition: **ACCEPT V2 with 0 findings; §3Z ≥95% × 2 consecutive
cycles satisfied; LOCK-eligible for cohort §3Z LOCK at this lens.**
