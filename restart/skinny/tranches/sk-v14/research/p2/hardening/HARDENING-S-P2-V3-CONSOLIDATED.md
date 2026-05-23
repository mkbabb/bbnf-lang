# SK-V14 S-P2 Research — V3 CHALLENGE Consolidated (§3Z COHORT LOCK)

Aggregator: SK-V14 S-P2 V3 hardening aggregator (write-only).
Date (UTC): 2026-05-23.
Scope: seven-lens CHALLENGE V3 confirming cycle over the six S-P2 P2
axis artefacts at V3 HEAD `ebe84954b1a6c31bb6183ca8f5e68d88647d9df7`
(P2-A 367 L V1+V2-LOCKED; P2-B 217 L V2-LOCKED; P2-C 164 L V2-LOCKED;
P2-D 254 L V2-LOCKED; P2-E 342 L V1+V2-LOCKED; P2-F 360 L V3 amended
via 2-cell verb-tense micro-fold at `p2f:164` C10 + `p2f:197` C13). V3
atomic micro-fold commit `ebe84954b` landed 2 files (V3 dispatch context
+ P2-F amendment; 42 insertions / 2 deletions; zero Rust source modified);
V2-LOCKED axes P2-A + P2-B + P2-C + P2-D + P2-E carry zero V3 drift
verified per `git diff 4c70b6f19..ebe84954b -- <axis>` returning empty
for all five axes.
Authority: `restart/prompts/ORCHESTRATOR.md §3W` (universal lens registry)
+ `§3Z` (convergence rule: ≥95% × 2 consecutive cycles + zero orphan
REVISEs); `restart/prompts/skinny/PASS-2-RESEARCH.md §3 + §5 + §6`
(CH1-CH6 specialisations + S-P2 close binding);
`restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7` (Overfit-Prune
carry-forward); dispatch
`restart/skinny/tranches/sk-v14/research/p2/hardening/V3/CHALLENGE-CONTEXT.md`
§0-§4.
Input ledger: seven V3 lens dispositions under
`restart/skinny/tranches/sk-v14/research/p2/hardening/V3/`
(`CH1.md` 487 L; `CH2.md` 337 L; `CH3.md` 522 L; `CH4.md` 543 L;
`CH5.md` 364 L; `CH6.md` 725 L; `CH7.md` 249 L — 3227 lens lines + 40
CHALLENGE-CONTEXT lines).
V2 carry-forward: `HARDENING-S-P2-V2-CONSOLIDATED.md` (671 L; V2
aggregator + V3 fold-packet authority; V2 baseline 100.0% sub-axis
aggregate / 100.0% per-lens mean; zero orphan REVISE items; 5/7 per-
lens 2-cycle LOCKs achieved at V2 close).
V1 carry-forward: `HARDENING-S-P2-V1-CONSOLIDATED.md` (659 L; V1
aggregator + fold-packet authority; V1 baseline 93.1% sub-axis aggregate
/ 96.2% per-lens mean; ten orphan REVISE items across CH1 + CH4).

## §0 — §3Z COHORT LOCK DECLARATION

### §0.1 — Lock declaration (verbatim gate criterion + verdict)

`restart/prompts/ORCHESTRATOR.md §3Z` convergence rule, verbatim:

> "Cohort convergence requires sub-axis-weighted aggregate ACCEPT-rate
> ≥ 95 % for two consecutive CHALLENGE cycles AND zero orphan REVISE
> entries at the cohort level. Every lens in the seven-lens registry
> must satisfy the per-lens two-cycle LOCK rule (≥ 95 % × 2 consecutive
> cycles) before the cohort LOCK fires."

V3 cycle close evaluation against the verbatim gate criterion:

- **Sub-axis-weighted aggregate ACCEPT-rate ≥ 95 % × 2 consecutive cycles:**
  V2 = 100.0 % (145/145); V3 = 100.0 % (145/145). Two consecutive cycles
  at the 100 % ceiling. **GATE-A SATISFIED.**
- **Zero orphan REVISE entries at the cohort level:** V2 had 0 orphan
  REVISEs (only ACCEPT-WITH-NOTE on `p2f:164` + `p2f:197` per CH1 V2
  §4.4 dispute rule, explicitly NOT a REVISE); V3 has 0 orphan REVISEs
  (the V2 ACCEPT-WITH-NOTE was discharged by the V3 verb-tense fix to
  strict ACCEPT). **GATE-B SATISFIED.**
- **Per-lens two-cycle LOCK rule × 7 lenses:**
  - CH1 V2 100 % + V3 100 % → **2-cycle LOCK SATISFIED at V3.**
  - CH2 V1 100 % + V2 100 % + V3 100 % → **3-cycle LOCK SATISFIED.**
  - CH3 V1 100 % + V2 100 % + V3 100 % → **3-cycle LOCK SATISFIED.**
  - CH4 V2 100 % + V3 100 % → **2-cycle LOCK SATISFIED at V3.**
  - CH5 V1 100 % + V2 100 % + V3 100 % → **3-cycle LOCK SATISFIED.**
  - CH6 V1 100 % (46/46) + V2 100 % (47/47) + V3 100 % (47/47) →
    **3-cycle LOCK SATISFIED.**
  - CH7 V1 100 % + V2 100 % + V3 100 % → **3-cycle LOCK SATISFIED.**

  **All seven lenses satisfy the per-lens two-cycle LOCK rule.
  GATE-C SATISFIED.**

### §0.2 — Cohort §3Z LOCK declaration

**§3Z COHORT LOCK ACHIEVED.** All three §3Z gate clauses (A, B, C) are
satisfied at V3 cycle close. The S-P2 research-pass cohort has converged
per `ORCHESTRATOR.md §3Z` with five three-cycle LOCKs (CH2/CH3/CH5/CH6/CH7)
+ two two-cycle LOCKs (CH1/CH4). Zero REJECT across V1 + V2 + V3; zero
orphan REVISE at V3 close.

### §0.3 — S-P3 dispatch gate

Per the SK-V14 SK LOOP (the orchestrator-prompt phased gating discipline):

> "S-P3 dispatch opens when (and only when) the S-P2 cohort satisfies
> the §3Z LOCK criterion. S-P3 inherits the cohort-confirmed S-P2
> findings as carry-forward; no S-P3 sub-agent dispatches until the
> aggregator commits the §3Z LOCK declaration."

**S-P3 DISPATCH GATE OPEN.** The §3Z LOCK declared at §0.2 above is
the gate-opening event. S-P3 sub-agent dispatch may now proceed per
the SK LOOP. The S-P2 cohort findings are conferred to S-P3 as
carry-forward per §6 below.

## §1 — Cycle disposition table (V1 / V2 / V3 × 7 lenses)

### §1.1 — Per-lens ACCEPT-rate trajectory + per-lens LOCK chain

| Lens | V1 ACCEPT-rate | V2 ACCEPT-rate | V3 ACCEPT-rate | Per-lens LOCK | Cycle disposition |
|---|---:|---:|---:|---|---|
| CH1 CORRECTNESS | candidate 81.6 % / artefact 33.3 % (REVISE) | **candidate 100 % / artefact 100 %** (38/38 cand; 6/6 artefact) | **candidate 100 % / artefact 100 %** (38/38 cand; 6/6 artefact) | **2-cycle LOCK SATISFIED (V2 + V3)** | ACCEPT (V3 verb-tense fix at `p2f:164` + `p2f:197` discharges V2 §4.4 ACCEPT-WITH-NOTE on C10 + C13 cleanly to strict ACCEPT; second consecutive ≥ 95 % cycle achieved) |
| CH2 GENERALITY | 100 % (6/6) | **100 %** (6/6; 39 active + 6 demoted/pre-blocked = 45 cross-axis entries) | **100 %** (6/6; 39 active + 6 demoted/pre-blocked = 45 cross-axis entries) | **3-cycle LOCK SATISFIED (V1 + V2 + V3)** | ACCEPT (F-V2-CH2-1 §2.Y NF-CH6-4 cross-axis tracking note V3-intact at `p2f:231-239`; F-V2-CH2-2 F-V2-P1ABC-RERECORD CH2/CH4 dual-gating V3-intact across all 12 consumer-dependency candidates; V3 verb-tense fix CH2-orthogonal) |
| CH3 REGRESSION (REDRESS) | 100 % (6/6) | **100 %** (6/6) | **100 %** (6/6) | **3-cycle LOCK SATISFIED (V1 + V2 + V3)** | ACCEPT (three V2-strengthened REDRESS families V3-intact: REDRESS-126 via C-P2C-7 demotion; REDRESS-88 via NF-CH6-3 C2 scalar-ref evidence upgrade; REDRESS 60-72 via NF-CH6-4 §2.Y cross-axis tracking note; V3 verb-tense fix re-opens zero REDRESS routes) |
| CH4 COST | strict 91.9 % / alt 94.6 % (REVISE 3) | **strict 100 %** (36/36 active eligible; aggregate 5-stage 180/180 = 100 %) | **strict 100 %** (36/36 active eligible; aggregate 5-stage 180/180 = 100 %) | **2-cycle LOCK SATISFIED (V2 + V3)** | ACCEPT (V3 verb-tense fix discharges F-V2-CH4-1 cleanly; F-V2-CH4-2 OK-TO-DEFER to S-P3 wave-program admission manifest per CH4 V3 §2.2; second consecutive ≥ 95 % cycle achieved) |
| CH5 HIDDEN COUPLING | 100 % (6/6) | **100 %** (6/6) | **100 %** (6/6) | **3-cycle LOCK SATISFIED (V1 + V2 + V3)** | ACCEPT (substrate-union YES preserved at V3 HEAD via executable re-verification: three-hit `struct.*Tape` set at `tape/mod.rs:92,94` + `tape/assembler.rs:42`; two-cursor independence at `runtime/src/grammars/json/parser.rs:10` + `bbnf-bench/src/generated_real_typed.rs:2745`; V3 verb-tense fix CH5-orthogonal by construction — substrate-target slot `local_temp_only` preserved) |
| CH6 ANTI-PAPER-CLOSE | 100 % (46/46) | **100 %** (47/47 incl. new CH6-D demotion-honesty sub-test 6/6 PASS) | **100 %** (47/47 + 2/2 new CH6-E verb-tense-honesty sub-test discharged) | **3-cycle LOCK SATISFIED (V1 + V2 + V3)** | ACCEPT (V3 introduces new CH6-E verb-tense-honesty sub-test responding to V2 latent paper-close risk on `p2f:164` + `p2f:197`; V3 fix immediately discharges 2/2; NF-CH6-1 STRENGTHENED V3 via verb-tense alignment; NF-CH6-3/4/5 V3-carried; NF-CH6-6 V2 binding entry intact) |
| CH7 OVERFIT-PRUNE | 100 % (6/6; 30/30 mandate cells) | **100 %** (6/6; mandates (3) + (5) STRENGTHENED) | **100 %** (6/6; V3 verb-tense fix anti-overfit-strengthens) | **3-cycle LOCK SATISFIED (V1 + V2 + V3)** | ACCEPT (Stage-A target naming overfit-asymmetric preservation re-verified at V3 HEAD: `byte_context_64.rs` + `bcax_64.rs` correctly NOT-PRESENT; sibling templates `byte_class_from_eq_set_64.rs` + `bitmap_prefix_xor_64.rs` PRESENT; V3 verb-tense fix is strictly anti-overfit — removes overstated "landed" phrasing) |

### §1.2 — Aggregate ACCEPT-rate at V3

Two aggregation methods per `ORCHESTRATOR.md §3Z`:

- **Sub-axis / candidate-weighted (load-bearing for §3Z convergence):**
  CH1 candidate count (38 / 38) + CH4 candidate count (36 / 36) +
  CH2/CH3/CH5 per-artefact (6/6 each) + CH6 (47/47) + CH7 (6/6).
  Combined: (38 + 36 + 6 + 6 + 6 + 47 + 6) / (38 + 36 + 6 + 6 + 6 +
  47 + 6) = **145 / 145 = 100.0 %**.
- **Per-lens mean (informational; equal weight per lens):**
  (100 + 100 + 100 + 100 + 100 + 100 + 100) / 7 = **100.0 %**.

V3 sub-axis-weighted aggregate (100.0 %) is **≥ 95 % floor**; per-lens
mean (100.0 %) likewise. **Zero orphan REVISE items at V3 across all
seven lenses.** The V2 §2.4 CH1 + V2 §6.1 CH4 ACCEPT-WITH-NOTE on
`p2f:164` (C10) + `p2f:197` (C13) is fully discharged at V3 via the
V3 verb-tense micro-fold.

V2 → V3 delta: sub-axis aggregate 100.0 % → 100.0 % (no regression);
per-lens mean 100.0 % → 100.0 % (no regression); orphan REVISE count
0 → 0 (preserved at floor); ACCEPT-WITH-NOTE qualifications 1 → 0
(discharged).

### §1.3 — REJECT roster (verbatim)

**Zero REJECT findings across all 7 lenses at V3** (and at V1 + V2).
The V3 cycle confirms the V1 + V2 verdict that no architectural-grounds
falsification surfaces on any P2 axis claim across three CHALLENGE
cycles.

### §1.4 — REVISE roster (verbatim)

**Zero orphan REVISE entries at V3** (and zero at V2). All 10 V1 orphan
REVISE items were discharged at V2 (per `HARDENING-S-P2-V2-CONSOLIDATED.md
§0.4`); zero new REVISE entries surfaced at V3 across all seven lens
audits. The V3 cycle discharged the only V2-cycle carry-forward (the
§4.4 ACCEPT-WITH-NOTE on C10 + C13) to strict ACCEPT.

## §2 — §3Z gate evaluation (convergence chain summary)

### §2.1 — Per-lens convergence chain

`ORCHESTRATOR.md §3Z` requires ≥ 95 % aggregate × 2 consecutive cycles
+ zero orphan REVISEs at the cohort level. The per-lens accounting at
V3 close:

| Lens | V1 cycle | V2 cycle | V3 cycle | Cycles ≥ 95 % consecutive | Per-lens LOCK |
|---|---|---|---|---|---|
| CH1 | 81.6 % cand / 33.3 % artefact (REVISE) | 100 % cand / 100 % artefact | **100 % cand / 100 % artefact** | **2** (V2 + V3) | **YES — 2-cycle LOCK** |
| CH2 | 100 % | 100 % | **100 %** | **3** (V1 + V2 + V3) | **YES — 3-cycle LOCK** |
| CH3 | 100 % | 100 % | **100 %** | **3** (V1 + V2 + V3) | **YES — 3-cycle LOCK** |
| CH4 | 91.9 % strict (REVISE 3) | 100 % strict | **100 % strict** | **2** (V2 + V3) | **YES — 2-cycle LOCK** |
| CH5 | 100 % | 100 % | **100 %** | **3** (V1 + V2 + V3) | **YES — 3-cycle LOCK** |
| CH6 | 100 % (46/46) | 100 % (47/47 incl. CH6-D) | **100 % (47/47 + 2/2 CH6-E)** | **3** (V1 + V2 + V3) | **YES — 3-cycle LOCK** |
| CH7 | 100 % (30/30 mandate cells) | 100 % (30/30 + 5 V2-strengthening) | **100 % (30/30 + V3 anti-overfit strengthening)** | **3** (V1 + V2 + V3) | **YES — 3-cycle LOCK** |

**Cohort gate state at V3 close:** 7 / 7 per-lens LOCKs achieved (5
three-cycle LOCKs + 2 two-cycle LOCKs). **§3Z LOCK fully satisfied.**

### §2.2 — Orphan REVISE census (zero)

| Cycle | Cohort REVISE count | ACCEPT-WITH-NOTE qualifications | Status |
|---|---:|---:|---|
| V1 | 10 (7 CH1 candidate-rows + 3 CH4 candidates: C8/C10/C13) | 0 | All 10 discharged at V2 via Fold-1 through Fold-6 + CF-1 |
| V2 | **0** | 1 (CH1 §4.4 on `p2f:164` + `p2f:197`; carried to V3 as cosmetic) | V2 cycle close: zero orphan REVISE |
| V3 | **0** | **0** (V3 verb-tense fix discharged the V2 ACCEPT-WITH-NOTE) | V3 cycle close: zero orphan REVISE; zero ACCEPT-WITH-NOTE qualification |

### §2.3 — REJECT census (zero across all three cycles)

| Cycle | Cohort REJECT count | Status |
|---|---:|---|
| V1 | **0** | No architectural-grounds falsification on any P2 axis claim |
| V2 | **0** | Confirmed at V2 audit |
| V3 | **0** | Confirmed at V3 audit (three-cycle zero-REJECT chain) |

### §2.4 — Convergence vote

Per `ORCHESTRATOR.md §3Z` (≥ 95 % × 2 cycles, zero orphan REVISEs):

- V3 closes the **second consecutive ≥ 95 % cycle** for CH1 + CH4
  (the only two lenses requiring V3 confirmation; V1 was below floor
  on both, V2 was the first ≥ 95 % cycle).
- Per-lens 3-cycle LOCK satisfied on CH2 / CH3 / CH5 / CH6 / CH7 (all
  three cycles 100 % ACCEPT).
- Sub-axis aggregate **100.0 %** + per-lens mean **100.0 %**; both
  ≥ 95 % floor; zero orphan REVISEs across all three cycles for the
  five lenses with 3-cycle LOCK and across two cycles for the two
  lenses with 2-cycle LOCK.

**Cycle verdict: COHORT §3Z LOCK ACHIEVED at V3 close.** S-P2 research
pass is convergent; S-P3 dispatch gate opens per the SK LOOP.

## §3 — V3 confirming-cycle summary

### §3.1 — V3 verb-tense fix discharge ledger

The V3 atomic micro-fold commit `ebe84954b` landed exactly two cell-
level edits in P2-F (`p2f-grammar-neutral.md:164` C10 + `p2f-grammar-
neutral.md:197` C13), replacing:

> "Authoring landed as Fold-{4,5} V2 deliverable per
> HARDENING-S-P2-V1-CONSOLIDATED §3.4."

with:

> "Authoring queued for S-P3 same-wave Lock 16 same-commit admission
> per HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-{4,5}; function body
> lands same-commit with SIMD body at S-P3."

V3 lens-level disposition impact:

| V2 finding | V3 disposition | Lens(es) |
|---|---|---|
| F-V2-CH1-1 (CH1 §4.4 ACCEPT-WITH-NOTE on C10 + C13 verb-tense) | **DISCHARGED** to strict ACCEPT via V3 verb-tense fix | CH1 |
| F-V2-CH4-1 (CH4 §6.1 fold-suggestion on Stage-A target wording verb-tense) | **DISCHARGED** to strict ACCEPT via V3 verb-tense fix | CH4 |
| Latent paper-close risk on `p2f:164` + `p2f:197` (V2 phrasing implied present-tense completion contrary to V2 §7.3 audit) | **DISCHARGED + STRENGTHENED** via V3 verb-tense fix | CH6 |
| V2 NF-CH6-1 scalar-reference vocabulary uniformly grounded | **STRENGTHENED V3** via verb-tense alignment (the §2.10 / §2.13 cells now match V2 §7.3 audit-time verification) | CH6 |
| Stage-A target naming overfit-asymmetric preservation | **STRENGTHENED V3** — V3 verb-tense fix removes residual phrasing that could conflate framing with completion | CH7 |

### §3.2 — CH6-E new sub-test (V3-only)

V2's CH6 four-test apparatus (CH6-A comparator citation; CH6-B ISA
manual citation; CH6-C scalar-reference sketch; CH6-D demotion-honesty)
is extended at V3 with a fifth sub-check responding to the V2 §4.4
verb-tense risk:

> "**CH6-E (verb-tense honesty, V3-only).** A scalar-reference cite
> whose function body lands same-commit with the SIMD body at a named
> downstream wave (Lock 16 same-commit discipline) must surface the
> gap in present-future tense ('queued for', 'lands same-commit at
> S-P3', 'to be authored under Lock 16 same-commit') not past-perfect
> ('landed', 'delivered', 'shipped'). A past-perfect cite for a
> function that does not exist at HEAD is paper-close even if the cite
> chain (path:line + Fold-N reference) is otherwise complete — the
> orchestrator loses the wave-slot truth."

CH6-E applies to P2-F C10 (`p2f:164`) and P2-F C13 (`p2f:197`). The
V3 verb-tense fix immediately discharges 2/2. CH6-E is institutionalised
at V3 as a future-cycle audit anchor for any completion-verb claim on
a Stage-A target path; the discipline must be `ls`-existence-verified
at cycle HEAD before adopting past-perfect tense.

### §3.3 — V2-LOCKED axis zero-V3-drift confirmation (six lens-witnesses)

`git diff 4c70b6f19..ebe84954b -- p2a-sota-teardown.md p2b-dav1d-process.md
p2c-arch-esoterica.md p2d-substrate-tape.md p2e-parse-that-gaps.md`
returns empty per executable verification at six independent witness
points (one per lens audit):

1. CH1 V3 §2.4 — `git diff` returns empty; line-count match V2 (367 /
   217 / 164 / 254 / 342).
2. CH2 V3 §1.1 — same `git diff` verification with file-by-file
   accounting; V2-LOCKED axes byte-identical at V3 HEAD.
3. CH3 V3 §1 — drift audit table; all five V2-LOCKED axes
   "ZERO drift confirmed".
4. CH4 V3 §3 — `git diff ... | wc -l` returns 0; `git show --stat
   ebe84954b` confirms only 2 files in changeset (V3 dispatch context +
   P2-F amendment).
5. CH5 V3 §2 — per-artefact V3 disposition table; all five V2-LOCKED
   axes carry V2 ACCEPT through to V3 verbatim.
6. CH6 V3 §1.5 + CH7 V3 §0.1 — `git diff --stat` returns empty for
   the five V2-LOCKED axes (zero P2-A/B/C/D/E lines in the V2→V3
   changeset).

`wc -l` at V3 HEAD: P2-A 367 lines; P2-B 217 lines; P2-C 164 lines;
P2-D 254 lines; P2-E 342 lines; P2-F 360 lines (V3 amendment is in-
place same-line-count edit at lines 164 + 197). Zero V3 line-count
drift across all six axes.

### §3.4 — CH1 + CH4 second-consecutive-cycle confirmation

CH1 V1 → V2 → V3 trajectory: 81.6 % cand / 33.3 % artefact (REVISE) →
100 % cand / 100 % artefact → **100 % cand / 100 % artefact**. V3
achieves the second consecutive ≥ 95 % cycle for CH1 per `ORCHESTRATOR.md
§3Z` — **CH1 per-lens 2-cycle LOCK SATISFIED at V3.**

CH4 V1 → V2 → V3 trajectory: 91.9 % strict / 94.6 % alt (REVISE 3) →
100 % strict (36/36 active eligible; 5-stage aggregate 180/180) →
**100 % strict (36/36 active eligible; 5-stage aggregate 180/180)**.
V3 achieves the second consecutive ≥ 95 % cycle for CH4 per `ORCHESTRATOR.md
§3Z` — **CH4 per-lens 2-cycle LOCK SATISFIED at V3.**

The five other lenses (CH2 / CH3 / CH5 / CH6 / CH7) carry the V1 + V2
per-lens 2-cycle LOCK forward verbatim to a V1 + V2 + V3 3-cycle LOCK
chain. V3 is verification-only for these five lenses; per-lens 2-cycle
LOCK was already satisfied at V2 close.

## §4 — Cohort-level cross-lens convergence (final)

### §4.1 — §2.Y NF-CH6-4 cross-axis tracking note — V3 LOAD-BEARING CROSS-CH-AXIS STRUCTURAL WIN PRESERVED

The P2-F V2 §2.Y cross-axis tracking note at `p2f:231-239`
(introduced V2, V3-preserved verbatim per CH2 V3 §1.4, CH5 V3 §2,
CH6 V3 §1.4 cross-witnesses) simultaneously satisfies five distinct
lens objectives at the V3 cycle close:

1. **CH2 generality binding** (F-V2-CH2-1 V3-confirmed): preserves
   grammar-neutrality across three colliding axes by binding S-P3 to
   one canonical primitive name + one canonical scalar reference
   rather than admitting three orthogonal SIMD bodies for one
   underlying primitive (Lock 14 v+1 violation surface preserved
   closed).
2. **CH5 hidden-coupling discovery** (CH5-V2-C V3-confirmed): the
   §2.Y note operates at the DESIGN-DOC surface (S-P2 research
   artefacts) and SURFACES the would-be-violation BEFORE it ships at
   the S-P3 admitted-primitive surface; V3 cycle preserves per-axis
   grounding for all three convergent identifiers.
3. **CH6 anti-paper-close exemplar** (NF-CH6-4 V3-carried): V3 did
   NOT silently consolidate the three axis names — it preserved
   per-axis grounding AND retained the S-P3 canonical-name binding
   for the wave plan. **This remains the exemplary anti-paper-close
   pattern across the V3 corpus.**
4. **CH7 anti-naming-overfit pattern** (V2 finding 3 V3-confirmed):
   three axes converging on one primitive could have been a naming-
   overfit failure pattern; §2.Y converts it to a CH6-affirmative
   consolidation pin that explicitly names the three convergent
   identifiers + binds S-P3 to ONE canonical primitive name.
5. **CH4 wave-level cost reduction** (F-V2-CH4-4 V3-confirmed):
   three near-duplicate candidates collapse to one canonical primitive
   — one Stage-A authoring + one Stage-B checkasm + one Stage-C lock
   manifest tie + one Stage-D consumer binding, not three.

The §2.Y note is the **load-bearing V3 cross-CH-axis structural win**:
five lenses converge on the same artefact as exemplary, with each
lens validating from its own discipline angle at V3 cycle. The V2→V3
preservation discipline holds (zero V3 edits to §2.Y).

### §4.2 — F-V2-P1ABC-RERECORD CH2/CH4 dual-gating — V3 PROPAGATION INTACT (29 citations / 18 candidates / 5 axes)

The V2 finding F-V2-CH2-2 ("F-V2-P1ABC-RERECORD CH2/CH4 dual-gating
propagation across all 12 consumer-dependency candidates") carries
through V3 verbatim with cite-density verified at V3 HEAD:

- 29 citations across 5 of 6 axis files (P2-A 2; P2-C 12; P2-D 0
  intentional [substrate-side]; P2-E 7; P2-F 8).
- 18 candidates carry the dependency cite (1 P2-A C6 + 5 P2-C + 6
  P2-E gaps per §4.7 + 5 P2-F active + 1 P2-F C8 re-promotion gate).
- Orchestrator 11+ threshold exceeded (29 >> 11).

Most load-bearing propagation sites V3-verified (per CH2 V3 §1.5):
- P2-F §4 risk row at `p2f:300` retains "Inherited V2 carry-forward
  F-V2-P1ABC-RERECORD" framing with dual-gate S-P3 verification clause.
- P2-F §2.X.1 C8 re-promotion gate at `p2f:229` names
  F-V2-P1ABC-RERECORD as (a) re-promotion condition.
- P2-C §2.X demotion stamp template at `p2c:63-65` names
  "F-V2-P1ABC-RERECORD CH2/CH4 dual-gate" as the re-evaluation surface
  for the three demoted P2-C candidates (C-P2C-1, -6, -7).
- P2-F §2.10 C10 antecedent stamp at `p2f:162` (Fold-3 antecedent stamp)
  names indirect via C1 + C4 with direct evidence requiring
  F-V2-P1ABC-RERECORD.

Zero V3 edit silently relaxes the dual-gate; the dual-gate is the
explicit dispatch-context inheritance carry-forward for every
dispatch-envelope-internal primitive candidate at V3 close.

### §4.3 — F-V2-CH4-2 OK-TO-DEFER to S-P3 wave-program admission manifest

Per CH4 V3 §2.2 disposition: F-V2-CH4-2 (CF-3 partial §4 mirror —
P2-C §4 + P2-D §4 adoption of P2-A per-candidate CH4 enumeration shape)
is **CONFIRMED OK-TO-DEFER** to the S-P3 wave-program admission
manifest. Five reasons (CH4 V3 §2.2):

1. Load-bearing CH4 evidence IS present per §2 rows of every axis
   file (the §4 mirror is documentation-cohesion, not CH4-load-bearing).
2. V3 confirming-cycle discipline binds V3 to verb-tense-only edits
   (`V3/CHALLENGE-CONTEXT.md:3` "P2-A/B/C/D/E LOCKED at V2").
3. S-P3 wave plan is the natural locus (per-candidate admission-gate
   manifest, not S-P2 §4 risk-discharge stamp).
4. V2 aggregator explicitly marked it non-blocking.
5. No CH4 cohort LOCK risk — strict ACCEPT-rate 36/36 = 100.0 % at
   V2 AND V3 WITHOUT the §4 mirror landing.

The deferral does NOT jeopardise the §3Z gate; CF-3 promotes to the
S-P3 wave-program admission manifest as carry-forward per §6.1.

### §4.4 — Stage-A target naming overfit-asymmetric preservation at V3 HEAD

Per CH7 V3 §0.4 + CH1 V3 §2.5 executable verification:

- `byte_context_64.rs` + `bcax_64.rs` correctly **NOT-PRESENT** at V3
  HEAD (Stage-A authoring targets per P2-F §2.10 + §2.13 — correctly
  framed in V3 amendment as "queued for S-P3 same-wave Lock 16
  same-commit admission … function body lands same-commit with SIMD
  body at S-P3").
- Sibling templates **PRESENT** at V3 HEAD: `byte_class_from_eq_set_64.rs`
  (C10 sibling shape) + `bitmap_prefix_xor_64.rs` (C13 sibling shape).
- 8-file `scalar/` directory listing unchanged from V2 census:
  `bitmap_next_set_bit.rs`, `bitmap_prefix_xor_64.rs`,
  `bulk_emit_positions_64.rs`, `byte_class_from_eq_set_64.rs`,
  `byte_class_from_table_64.rs`, `eob_pad_clamp.rs`, `mod.rs`,
  `swar_8byte.rs`.

The V3 verb-tense fix STRENGTHENS the overfit-asymmetry by removing
residual V2 phrasing that could conflate framing with completion;
the Stage-A target naming is now framed as queued-for-S-P3-same-wave-
same-commit-Lock-16-admission, the load-bearing not-yet-existent
status fully visible at the per-row attribution.

### §4.5 — Substrate-union YES seven-witness corroboration preserved at V3

P2-D's load-bearing finding **substrate-union holds at HEAD** is
preserved at V3 with V3 cycle re-verification:

1. P2-D §1.1 line 27 — `grep -rn "struct.*Tape\b"
   skinny/crates/runtime/src/` returns 3 hits.
2. P2-D §1.5 lines 84-92 — architectural-block of new union variant.
3. P2-D §4.7 line 204 — "YES, the substrate union holds at HEAD".
4. P2-F §1.3 line 53 — holding assumption corroborated.
5. CH5 V2 §3 CH5-V2-A V3-re-verified: three hits at `tape/mod.rs:94`
   (`Tape<'input>`), `tape/assembler.rs:42` (`TapeBuilder<'input>`),
   `tape/mod.rs:92` (`TapeId(pub u64)`).
6. P1-V3-CH5 `research/p1/hardening/V3/CH5.md:78-83` — two-cursor
   independence verification.
7. CH5 V3 §2 per-artefact P2-D row: substrate-union YES preserved at
   V3 HEAD; two-cursor independence re-verified at `runtime/src/grammars/json/parser.rs:10`
   (Track 1) + `bbnf-bench/src/generated_real_typed.rs:2745` (Track 2).

The V2 demotion of C-P2D-3 carries through V3 unchanged with V3-explicit
anti-regression clause for future re-elevation preserved.

## §5 — Trajectory record + V1 → V2 → V3 close path

### §5.1 — Sub-axis aggregate trajectory

| Cycle | Sub-axis aggregate | Per-lens mean | Orphan REVISE | ACCEPT-WITH-NOTE qualifications | REJECT |
|---|---:|---:|---:|---:|---:|
| V1 | 93.1 % | 96.2 % | 10 | 0 | 0 |
| V2 | 100.0 % | 100.0 % | 0 | 1 (cosmetic; V3 carry) | 0 |
| V3 | **100.0 %** | **100.0 %** | **0** | **0** (V2 ACCEPT-WITH-NOTE discharged) | **0** |

Three-cycle journey: V1 below threshold on two lenses (CH1 + CH4) →
V2 first ≥ 95 % cycle on both via Fold-1 through Fold-6 + CF-1 reframe
discharges (orphan REVISE 10 → 0) → V3 second consecutive ≥ 95 % cycle
via 2-cell verb-tense micro-fold (ACCEPT-WITH-NOTE 1 → 0).

### §5.2 — Per-lens LOCK trajectory

| Lens | V1 → V2 → V3 ACCEPT-rate chain | LOCK chain length |
|---|---|---:|
| CH1 | 33.3 % artefact → 100 % → 100 % | 2 |
| CH2 | 100 % → 100 % → 100 % | 3 |
| CH3 | 100 % → 100 % → 100 % | 3 |
| CH4 | 91.9 % → 100 % → 100 % | 2 |
| CH5 | 100 % → 100 % → 100 % | 3 |
| CH6 | 100 % → 100 % → 100 % | 3 |
| CH7 | 100 % → 100 % → 100 % | 3 |

Five three-cycle LOCKs (CH2/CH3/CH5/CH6/CH7) + two two-cycle LOCKs
(CH1/CH4) = seven cohort lenses satisfying the per-lens two-cycle
LOCK rule at V3 close.

### §5.3 — Commit anchors

- V1 axis commit: `b3dbc5ca0e3ccf38df71a5e72be3d65a3068549b` ("docs(sk-v14-p2-hardening-V1): challenge V1 + consolidated").
- V2 axis commit: `447a26b07c353b217905c15a3d61c907a8e78410` ("docs(sk-v14-p2-V2): atomic micro-fold (4 axes amended) + V2 dispatch context").
- V2 hardening commit: `4c70b6f19` ("docs(sk-v14-p2-hardening-V2): challenge V2 + consolidated").
- V3 axis commit: `ebe84954b1a6c31bb6183ca8f5e68d88647d9df7` ("docs(sk-v14-p2-V3): atomic micro-fold (P2-F verb-tense 2 cells) + V3 dispatch context"); 2 files changed (V3 CHALLENGE-CONTEXT + P2-F amendment); 42 insertions / 2 deletions; zero Rust source files modified.
- V2 → V3 diff verification: `git diff 4c70b6f19..ebe84954b -- p2{a,b,c,d,e}-*.md` returns empty (5 V2-LOCKED axes zero-drift confirmed across six lens-witnesses).

## §6 — S-P3 carry-forward packet

The S-P3 dispatch context inherits the following three load-bearing
items from the S-P2 §3Z LOCK cohort:

### §6.1 — F-V2-CH4-2 / CF-3 → S-P3 wave-program admission manifest discipline

Per CH4 V3 §2.2 + §6.2 + the V2 → V3 OK-to-defer disposition, the
S-P3 dispatch context must carry forward the CF-3 documentation-
discipline as a **wave-program admission-gate manifest** entry:

> "Every shortlisted candidate's admission manifest carries the 3-gate
> CH4 cell explicitly: (scalar-ref status / checkasm-parity expectation
> / same-wave-consumer NAMED) per S-P2 CH4 V2 CF-3. This promotes CF-3
> from S-P2 §4 documentation-discipline to S-P3 wave-program
> admission-gate manifest, the natural locus."

S-P3 owners are responsible for the per-candidate 3-gate admission cell
on every shortlisted candidate at wave-program manifest authoring time.

### §6.2 — §2.Y NF-CH6-4 canonical-name binding for long-string-body SIMD scan

Per CH6 V3 §1.4 + the P2-F §2.Y cross-axis tracking note at `p2f:231-239`,
S-P3 dispatch context must carry forward the canonical-name binding:

> "Three artefacts (P2-A C2 `long_string_body_simd_scan`, P2-E Gap 1
> `scan_string_special_block_sweep_64`, P2-F C1+C2 quote-aware
> classifier composition) surface the same long-string-body SIMD scan
> primitive under three distinct names, all grounded on the
> `unescape_string` direct rank-1 46.7 % `unicode_escapes` hot-leaf
> (P1-E §2.2). S-P3 consolidator binding: ONE canonical primitive name
> + ONE canonical scalar-ref function rather than three orthogonal
> SIMD bodies for one primitive."

S-P3 owners must NOT admit three orthogonal SIMD bodies; the wave plan
admitting any of the three convergent identifiers must commit to the
single canonical primitive name at admission time.

### §6.3 — F-V2-P1ABC-RERECORD Stage-0 wave commitment

Per CH6 V3 §1.6 + the V1 consolidator §2.1 binding entry (lines
230-289), preserved verbatim at V3:

```
Packet: F-V2-P1ABC-RERECORD
Gating:  CH2 (measurability) + CH4 (cost-discriminator) dual-gate
Cargo:   cargo build --release -p bbnf-bench --features runtime/parse-attribution
Samply:  interactive samply record (NOT --save-only) per [samply-symbol-resolution]
         + cfg_attr flip verification at generated.rs:33-34, 43-44,
           58-59, 79-80, 86-87, 117-118, 138-139, 157-158 (8 sites;
           inline(always) → inline(never))
Wave:    Stage 0 of the first SK-V14 implementation wave admitting any
         dispatch-envelope-internal primitive
Consumers (must-bind, [no-deferrals]):
         P2-A C6 + P2-C C-P2C-3 + P2-C C-P2C-8 + P2-E Gap 1 + Gap 3
         + Gap 4 + Gap 5 + P2-F C6 + C7 + C10 + C12 + C13
Convention: per CH2 F2 elevation, any S-P3 wave admitting these
         primitives MUST ship the rerun in Stage 0 of the same wave
```

S-P3 owners must wire the F-V2-P1ABC-RERECORD packet as Stage 0 of any
wave admitting any of the 12 consumer-dependency primitives; the dual-
gating (CH2 measurability + CH4 cost-discriminator) is a no-deferral
binding.

## §7 — §3Z LOCK trajectory record (summary statistics)

### §7.1 — Aggregate-rate trajectory

- **V1:** 93.1 % sub-axis aggregate / 96.2 % per-lens mean (CH1 + CH4
  below §3Z floor; 10 orphan REVISE entries; 0 REJECT).
- **V2:** 100.0 % sub-axis aggregate / 100.0 % per-lens mean (all 7
  lenses ≥ 95 % floor; 0 orphan REVISE entries; 1 cosmetic ACCEPT-
  WITH-NOTE carried; 0 REJECT).
- **V3:** 100.0 % sub-axis aggregate / 100.0 % per-lens mean (all 7
  lenses ≥ 95 % floor; 0 orphan REVISE entries; 0 ACCEPT-WITH-NOTE
  qualifications; 0 REJECT).

### §7.2 — REVISE / ACCEPT-WITH-NOTE / REJECT census

- Orphan REVISE entries: V1 = 10 → V2 = 0 → V3 = 0 (zero for two
  consecutive cycles).
- ACCEPT-WITH-NOTE qualifications: V1 = 0 → V2 = 1 → V3 = 0
  (discharged by V3 verb-tense micro-fold).
- REJECT entries: V1 = 0 → V2 = 0 → V3 = 0 (zero across three cycles).

### §7.3 — Three-cycle journey summary

S-P2 research pass cohort §3Z LOCK trajectory: **V1 (below floor) →
V2 (first cycle at ceiling) → V3 (second consecutive cycle at ceiling)
= cohort §3Z LOCK ACHIEVED.** Five lenses (CH2/CH3/CH5/CH6/CH7) carried
clean V1-baseline 100 % ACCEPT through to V3 unchanged; two lenses
(CH1/CH4) achieved the §3Z gate via Fold-1 through Fold-6 + CF-1 V2
discharges then V3 verb-tense polish to discharge the V2 cosmetic
ACCEPT-WITH-NOTE.

## §8 — Closing: S-P3 unblocked

### §8.1 — §3Z LOCK closing declaration (verbatim)

**§3Z COHORT LOCK ACHIEVED.** All seven CHALLENGE lenses (CH1
CORRECTNESS, CH2 GENERALITY, CH3 REGRESSION, CH4 COST, CH5 HIDDEN
COUPLING, CH6 ANTI-PAPER-CLOSE, CH7 OVERFIT-PRUNE) satisfy the per-
lens two-cycle LOCK rule at V3 cycle close. Sub-axis-weighted aggregate
ACCEPT-rate 100.0 % × 2 consecutive cycles (V2 + V3) + zero orphan
REVISE entries at the cohort level satisfies the `ORCHESTRATOR.md §3Z`
gate criterion verbatim.

### §8.2 — S-P3 dispatch gate

**S-P3 DISPATCH GATE OPEN per SK LOOP.** S-P2 research-pass cohort
has converged; the §3Z LOCK is the gate-opening event per the
orchestrator-prompt phased gating discipline. S-P3 sub-agent dispatch
may now proceed; the three carry-forward packets enumerated at §6
above must be wired into the S-P3 dispatch context as inheritance
carry-forward.

### §8.3 — S-P2 close certificate

The S-P2 research pass is hereby certified converged per `ORCHESTRATOR.md
§3Z` + `PASS-2-RESEARCH.md §6`. The six P2 axis artefacts at V3 HEAD
`ebe84954b1a6c31bb6183ca8f5e68d88647d9df7` are LOCKED as cohort-final
S-P2 research deliverables; no further S-P2 CHALLENGE cycle is required;
S-P3 owns the next forward step in the SK LOOP.

## §9 — Sources

V3 lens dispositions (all verified existing at write-time):

- `restart/skinny/tranches/sk-v14/research/p2/hardening/V3/CH1.md` (487 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V3/CH2.md` (337 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V3/CH3.md` (522 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V3/CH4.md` (543 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V3/CH5.md` (364 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V3/CH6.md` (725 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V3/CH7.md` (249 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V3/CHALLENGE-CONTEXT.md` (40 lines)

V3 P2 axis artefacts under review (HEAD `ebe84954b`):

- `restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md` (367 lines; V1+V2-LOCKED — zero V3 drift)
- `restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md` (217 lines; V2-LOCKED — zero V3 drift)
- `restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md` (164 lines; V2-LOCKED — zero V3 drift)
- `restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md` (254 lines; V2-LOCKED — zero V3 drift)
- `restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md` (342 lines; V1+V2-LOCKED — zero V3 drift)
- `restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md` (360 lines; V3 amended via 2-cell verb-tense micro-fold at §2.10 line 164 + §2.13 line 197)

Prior-cycle precedents:

- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md` (671 lines; V2 aggregator + V3 fold-packet authority; format mirror for this aggregator)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md` (659 lines; V1 aggregator + fold-packet authority)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CH{1..7}.md` + `CHALLENGE-CONTEXT.md` (V2 lens dispositions; carry-forward)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CH{1..7}.md` + `CHALLENGE-CONTEXT.md` (V1 lens dispositions; carry-forward)

Binding authorities:

- `restart/prompts/skinny/PASS-2-RESEARCH.md §3 + §5 + §6` (CH1-CH6
  specialisations + S-P2 close binding)
- `restart/prompts/ORCHESTRATOR.md §3W` (universal CH1-CH6 lens registry) +
  `§3Z` (convergence rule)
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7` (Overfit-Prune
  lens definition; carry-forward from S-P0)
- `restart/skinny/tranches/sk-v14/research/p2/S-P2-DISPATCH-CONTEXT.md`
  (S-P2 dispatch spec; F-V2-P1ABC-RERECORD inheritance)
- `restart/locks/LOCKS.md` (Lock 1 substrate-union v+1 manifest;
  Lock 14 v+1 grammar-neutrality; Lock 15 i-cache budget; Lock 16
  SIMD/ASM allowlist + abstract-primitive declarations)

V3 cycle commit anchors:

- V2 hardening commit (V3 baseline): `4c70b6f19e...` ("docs(sk-v14-p2-hardening-V2): challenge V2 + consolidated").
- V3 axis commit: `ebe84954b1a6c31bb6183ca8f5e68d88647d9df7` ("docs(sk-v14-p2-V3): atomic micro-fold (P2-F verb-tense 2 cells) + V3 dispatch context"); 2 files changed; 42 insertions / 2 deletions; zero Rust source files modified.
- V2 → V3 diff verification: `git diff 4c70b6f19..ebe84954b -- p2a-sota-teardown.md p2b-dav1d-process.md p2c-arch-esoterica.md p2d-substrate-tape.md p2e-parse-that-gaps.md` returns empty (5 V2-LOCKED axes zero-drift confirmed across six lens-witnesses).

Bbnf source anchors (V3 HEAD-verified):

- `skinny/crates/runtime/src/grammars/json/scan.rs:32` (`scan_structurals_scalar` — P2-F C12 CF-1 reframing scalar reference; executable-verified live)
- `skinny/crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:2` (`bitmap_prefix_xor_64_scalar` — NF-CH6-3 composition primitive + sibling-shape template for Fold-5 `bcax_64.rs`)
- `skinny/crates/bbnf-simd/src/lib.rs:175` (`escape_mask_64` — NF-CH6-3 composition primitive)
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:31` (`scan_string_special_block_scalar` — NF-CH6-3 composition primitive + P2-E Gap 1 + §2.Y cross-axis tracking note cite)
- `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:1` (sibling-shape template for Fold-4 `byte_context_64.rs`)
- `skinny/crates/runtime/src/grammars/json/generated.rs:33-237` (parse-attribution `cfg_attr(feature = "parse-attribution", inline(never))` plumbing — F-V2-P1ABC-RERECORD toggle target)
- `skinny/crates/runtime/src/tape/mod.rs:92,94` (`TapeId`, `Tape<'input>` — substrate-union single-substrate verification)
- `skinny/crates/runtime/src/tape/assembler.rs:42` (`TapeBuilder<'input>` — substrate-union single-substrate verification)
- `skinny/crates/runtime/src/grammars/json/parser.rs:10` (`pub cursor: usize` — Track 1 cursor)
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:2745` (Track 2 cursor — two-cursor independence)
- `skinny/crates/bbnf-simd/src/scalar/` directory listing at V3 HEAD: 8 files (`bitmap_next_set_bit.rs`, `bitmap_prefix_xor_64.rs`, `bulk_emit_positions_64.rs`, `byte_class_from_eq_set_64.rs`, `byte_class_from_table_64.rs`, `eob_pad_clamp.rs`, `mod.rs`, `swar_8byte.rs`); `byte_context_64.rs` + `bcax_64.rs` NOT YET PRESENT (Stage-A authoring targets per P2-F §2.10 + §2.13 — correctly framed as "queued for S-P3 same-wave Lock 16 same-commit admission")

External (SOTA + ISA) citations pinned at V3 HEAD:

- simdjson HEAD `168ef580757d75270475b379e83c2b39787a6765` (P2-A §5.3)
- sonic-rs HEAD `03545a9530346fe279b674dd496e037d94204bc5` (P2-A §5.3)
- yyjson HEAD `95f4c61bc1e24176f2aa4f430902705a995f1c97` (P2-A §5.3)
- asmjson crate 0.2.5 (P2-A §5.3)
- dav1d HEAD `1718ff9aded99f0a89f5c7940d6afb8948301e33` (P2-A §5.3 + P2-B §5.1 inherited per V2 Fold-1)
- FFmpeg HEAD `085714182302333dd83dcb9c36cf828dc4eba929` (P2-A §5.3 + P2-B §5.1 inherited per V2 Fold-1)
- Arm ACLE 2026Q1; Arm Neon Intrinsics Reference 2026Q1; Arm
  Architecture Reference Manual A-profile Issue J.a + Armv8.2-A SHA3 +
  DotProd; Apple Silicon `sysctl hw.optional.*`; Intel Intrinsics Guide
  (x86 secondary); WikiChip VPCLMULQDQ / AVX-512 IFMA / BITALG;
  Lemire 2019/2023/2024/2026 series; Validark 2024 (LD4-interleaved);
  Travis Downs kreg-facts; Mula 2018-2024 GFNI + PDEP/PEXT;
  Langdale & Lemire 2019 VLDB arXiv:1902.08318
