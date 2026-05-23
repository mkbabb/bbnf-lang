# SK-V14 S-P2 Research — V2 CHALLENGE Consolidated

Aggregator: SK-V14 S-P2 V2 hardening aggregator (write-only).
Date (UTC): 2026-05-23.
Scope: seven-lens CHALLENGE V2 over the six S-P2 P2 axis artefacts at
V2 HEAD `447a26b07` (P2-A 367 L V1-LOCKED; P2-B 217 L V2-amended via
Fold-1; P2-C 164 L V2-amended via Fold-2; P2-D 254 L V2-amended via
Fold-2; P2-E 342 L V1-LOCKED; P2-F 360 L V2-amended via 6-sub-fold
packet). V1 atomic micro-fold commit `447a26b07` landed 4 amended axes
+ V2 dispatch context (5 files; 137 insertions / 53 deletions; zero
Rust source modified); V1-LOCKED axes P2-A + P2-E carry zero V2 drift
(`git diff b3dbc5ca0..447a26b07 -- p2a-sota-teardown.md
p2e-parse-that-gaps.md` returns empty).
Authority: `restart/prompts/ORCHESTRATOR.md §3W` (lens registry) +
`§3Z` (convergence rule: ≥95% × 2 consecutive cycles + zero orphan
REVISEs); `restart/prompts/skinny/PASS-2-RESEARCH.md §3` (CH1-CH6
specialisations); `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md
§CH7` (Overfit-Prune carry-forward); dispatch
`restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CHALLENGE-CONTEXT.md`
§0-§4.
Input ledger: seven V2 lens dispositions under
`restart/skinny/tranches/sk-v14/research/p2/hardening/V2/`
(`CH1.md` 526 L; `CH2.md` 359 L; `CH3.md` 548 L; `CH4.md` 680 L;
`CH5.md` 328 L; `CH6.md` 756 L; `CH7.md` 259 L — 3456 lens lines + 39
CHALLENGE-CONTEXT lines).
V1 carry-forward: `HARDENING-S-P2-V1-CONSOLIDATED.md` (659 L; V1
aggregator + fold-packet authority; V1 baseline 93.1% sub-axis
aggregate / 96.2% per-lens mean; ten orphan REVISE items across CH1 +
CH4).

## §0 — V2 cycle verdict

### §0.1 — Per-lens dispositions (V1 → V2 trajectory)

| Lens | V1 ACCEPT-rate | V2 ACCEPT-rate | Per-lens 2-cycle LOCK | Cycle disposition |
|---|---:|---:|---|---|
| CH1 CORRECTNESS | candidate 81.6 % / artefact 33.3 % (REVISE) | **candidate 100 % / artefact 100 %** (38/38 cand; 6/6 artefact) | **NO — V2 is first ≥95 % cycle on CH1** (V3 confirming cycle required) | ACCEPT (all 7 V1 REVISE candidate-rows discharged via Fold-1 + Fold-2 + Fold-3; one §2.4 verb-tense imprecision flagged ACCEPT-WITH-NOTE for V3 cosmetic fold; not a REVISE) |
| CH2 GENERALITY | 100 % (6/6) | **100 %** (6/6; 39 active + 6 demoted/pre-blocked = 45 cross-axis entries) | **YES — per-lens 2-cycle LOCK satisfied at V2** | ACCEPT (F-V2-CH2-1 §2.Y NF-CH6-4 cross-axis tracking note strengthens CH2; F-V2-CH2-2 F-V2-P1ABC-RERECORD CH2/CH4 dual-gating fully propagated to all 12 consumer-dependency candidates; V1 R1 + R2 + F2 discharged) |
| CH3 REGRESSION (REDRESS) | 100 % (6/6) | **100 %** (6/6; ACCEPT-WITH-NOTE on P2-E Gap 5 REDRESS-80 carries through V2 unchanged) | **YES — per-lens 2-cycle LOCK satisfied at V2** | ACCEPT (three REDRESS families STRENGTHENED at V2: REDRESS-126 via C-P2C-7 demotion-as-close-hygiene-proof; REDRESS-88 via C2 scalar-ref upgrade via Gap 6 composition; REDRESS 60-72 via NF-CH6-4 §2.Y single-canonical-primitive binding; 5 new V2-specific findings F-1..F-5) |
| CH4 COST | strict 91.9 % / alt 94.6 % (REVISE 3) | **strict 100 %** (36/36 active eligible; aggregate 5-stage 180/180 = 100 %) | **NO — V2 is first ≥95 % cycle on CH4** (V3 confirming cycle required) | ACCEPT (3 V1 REVISE all discharged: Fold-2 C8 demote; Fold-4 C10 Stage-A target name; Fold-5 C13 Stage-A target name; C12 CF-1 reframe landed at §2.12 + §4 in-row; 5 new findings F-V2-CH4-1..-5; two non-blocking V3 polish items) |
| CH5 HIDDEN COUPLING | 100 % (6/6) | **100 %** (6/6 active; substrate-union YES verified at HEAD post-P2-D §1.6(d) demotion) | **YES — per-lens 2-cycle LOCK satisfied at V2** | ACCEPT (4 V2-new findings CH5-V2-A..-D; substrate-union YES six-witness corroboration preserved; V2 demotions strengthen CH5 posture by reducing active candidate surface area; F-V2-P1ABC-RERECORD cargo-feature gating verified non-renaming) |
| CH6 ANTI-PAPER-CLOSE | 100 % (46/46) | **100 %** (47/47 incl. new CH6-D demotion-honesty sub-test 6/6 PASS) | **YES — per-lens 2-cycle LOCK satisfied at V2** | ACCEPT (NF-CH6-1..-5 discharged at V2; NF-CH6-6 consolidator binding entry remains for this V2 aggregator; F-V2-P1ABC-RERECORD cite-density 29 citations across 5 of 6 axis files; 18 candidates carry the dependency cite — orchestrator 11+ threshold exceeded) |
| CH7 OVERFIT-PRUNE | 100 % (6/6; 30/30 mandate cells) | **100 %** (6/6; mandates (3) + (5) STRENGTHENED) | **YES — per-lens 2-cycle LOCK satisfied at V2** | ACCEPT (5 V2-new findings; demotion-with-gap-note-preservation = V2 corpus's executable anti-overfit signature; Stage-A target naming = overfit-asymmetric CH7-affirmative pattern; §2.Y = anti-naming-overfit binding) |

### §0.2 — Aggregate ACCEPT-rate at V2

Two aggregation methods per `ORCHESTRATOR.md §3Z`:

- **Sub-axis / candidate-weighted (load-bearing for §3Z convergence):**
  CH1 candidate count (38 / 38) + CH4 candidate count (36 / 36) +
  CH2/CH3/CH5 per-artefact (6/6 each) + CH6 (47/47) + CH7 (6/6).
  Combined: (38 + 36 + 6 + 6 + 6 + 47 + 6) / (38 + 36 + 6 + 6 + 6 +
  47 + 6) = **145 / 145 = 100.0 %**.
- **Per-lens mean (informational; equal weight per lens):**
  (100 + 100 + 100 + 100 + 100 + 100 + 100) / 7 = **100.0 %**.

V2 sub-axis-weighted aggregate (100.0 %) is **≥ 95 % floor**; per-lens
mean (100.0 %) likewise. Zero orphan REVISE items at V2 across all
seven lenses; the §2.4 CH1 verb-tense imprecision (P2-F §2.10 + §2.13
"landed" wording) is flagged ACCEPT-WITH-NOTE for V3 cosmetic fold —
NOT a REVISE per CH1 V2 §4.4 disposition rule.

V1 → V2 delta: sub-axis aggregate 93.1 % → 100.0 % (+6.9 %); per-lens
mean 96.2 % → 100.0 % (+3.8 %); orphan REVISE count 10 → 0.

### §0.3 — REJECT roster (verbatim)

**Zero REJECT findings** across all 7 lenses at V2. V2 cycle confirms
V1 verdict that no architectural-grounds falsification surfaces on any
P2 axis claim; every V1 REVISE either discharges to ACCEPT via Fold-1
through Fold-5 + CF-1 reframing OR converts to ACCEPT-via-demotion per
`[no-deferrals]` default (Fold-2 demotion + Fold-6 SKIP for C8).

### §0.4 — REVISE roster (verbatim)

**Zero orphan REVISE entries at V2.** All 10 V1 orphan REVISE items
discharged:

**CH1 V1 → V2 (7 candidate-rows → 0):**

1. **V1 Fold-1 (P2-B §5.1 SHA pinning)** — DISCHARGED V2. FFmpeg HEAD
   `085714182302333dd83dcb9c36cf828dc4eba929` + dav1d HEAD
   `1718ff9aded99f0a89f5c7940d6afb8948301e33` pinned at `p2b:183-184`
   with inline P2-A §5.3 inheritance attribution; `dav1d
   src/x86/msac.asm:80-220` cite at `p2b:185` carries the same dav1d
   HEAD. CH1 V2 §2.1 verified discharge.
2. **V1 Fold-2 (zero-P1-antecedent demotion across P2-C/D/F: 5
   candidates)** — DISCHARGED V2. C-P2C-1/-6/-7 demoted to
   `p2c-arch-esoterica.md §2.X — Non-candidate inventory` at
   `p2c:48-71` with full 7-column technical content preserved verbatim
   + per-row disposition stamp `**Demoted V2: zero S-P1 hot-leaf
   antecedent at SK-V14; re-evaluate if F-V2-P1ABC-RERECORD surfaces
   antecedent.**`; C-P2D-3 demoted to `p2d-substrate-tape.md §1.6(d)`
   substrate-side observation at `p2d:104` with identifier stub
   gap-note at `p2d:128-130`; P2-F C8 demoted to `p2f-grammar-neutral.md
   §2.X.1` at `p2f:218-229` with explicit re-promotion gate (CH1 +
   CH4 + CH6 joint condition). CH1 V2 §2.2 verified discharge.
3. **V1 Fold-3 (P2-F C6/C7/C10/C12/C13 indirect-/envelope-antecedent
   disposition language stamping)** — DISCHARGED V2. Per-row stamps
   verbatim at `p2f:123` (C6) / `p2f:135` (C7) / `p2f:162` (C10) /
   `p2f:184` (C12 — additionally reframes CH4 ACCEPT per CF-1) /
   `p2f:195` (C13). CH1 V2 §2.3 verified discharge.

**CH4 V1 → V2 (3 candidates → 0):**

4. **V1 REVISE C8 comment-skip primitive (Stage-A scalar-ref + Stage-D
   consumer FLAGGED)** — DISCHARGED V2 via Fold-2 demotion + Fold-6
   SKIPPED per `[no-deferrals]` default (no same-wave consumer
   committed in V2 wave plan); C8 leaves the eligible-candidate
   denominator; re-promotion gate explicit at `p2f:229`. CH4 V2 §2.1
   verified discharge.
5. **V1 REVISE C10 cross-chunk byte-context propagation (Stage-A
   scalar-ref authoring REQUIRED)** — DISCHARGED V2 via Fold-4
   Stage-A target naming. `p2f:164` (C10): scalar-reference target
   path `crates/bbnf-simd/src/scalar/byte_context_64.rs` named with
   signature `byte_context_64_scalar(prev_chunk: &[u8; 64], cur_chunk:
   &[u8; 64], carry_bytes: usize) -> [u8; 64]` + sibling-file pattern
   `byte_class_from_eq_set_64.rs`. CH4 V2 §2.2 verified discharge.
6. **V1 REVISE C13 branchless 3-way XOR (BCAX) (Stage-A scalar-ref
   authoring REQUIRED)** — DISCHARGED V2 via Fold-5 Stage-A target
   naming. `p2f:197` (C13): scalar-reference target path
   `crates/bbnf-simd/src/scalar/bcax_64.rs` named with signature
   `bcax_64_scalar(a: u64, b: u64, c: u64) -> u64` returning `(a & !b)
   ^ c` + sibling-file pattern `bitmap_prefix_xor_64.rs`. CH4 V2 §2.3
   verified discharge.

### §0.5 — Convergence vote

Per `ORCHESTRATOR.md §3Z` (≥ 95 % × 2 cycles, zero orphan REVISEs):

- V2 is the **first ≥ 95 % cycle** on the load-bearing lenses CH1 +
  CH4 (V1 was below floor on both); per-lens 2-cycle LOCK satisfied
  on the five other lenses (CH2/CH3/CH5/CH6/CH7) which were 100 %
  ACCEPT at both V1 + V2.
- Sub-axis aggregate **100.0 %** + per-lens mean **100.0 %**; both
  ≥ 95 % floor; zero orphan REVISEs.
- §3Z gate state: **5/7 per-lens LOCKs achieved at V2; CH1 + CH4
  require V3 confirming cycle as their second consecutive ≥ 95 %
  cycle for cohort-level §3Z LOCK.**

**Cycle verdict: CONVERGED-V3-CONFIRMATION-REQUIRED.** V2 lifts the
sub-axis aggregate from 93.1 % to 100.0 % + drives orphan REVISEs to
zero; cohort-level §3Z LOCK awaits V3 as the second-consecutive ≥ 95 %
cycle on CH1 + CH4 specifically. The five lenses already at per-lens
2-cycle LOCK contribute clean baselines to the V3 confirmation.

## §1 — §3Z gate evaluation

### §1.1 — Cohort LOCK state at V2 close

`ORCHESTRATOR.md §3Z` requires ≥ 95 % aggregate × 2 consecutive cycles
+ zero orphan REVISEs at the cohort level. The per-lens accounting:

| Lens | V1 cycle | V2 cycle | Cycles ≥ 95 % consecutive | Per-lens LOCK |
|---|---|---|---|---|
| CH1 | 81.6 % cand / 33.3 % artefact (REVISE) | 100 % cand / 100 % artefact | **1** (V2 only) | NO — V3 confirming required |
| CH2 | 100 % | 100 % | **2** (V1 + V2) | **YES** |
| CH3 | 100 % | 100 % | **2** (V1 + V2) | **YES** |
| CH4 | 91.9 % strict (REVISE 3) | 100 % strict | **1** (V2 only) | NO — V3 confirming required |
| CH5 | 100 % | 100 % | **2** (V1 + V2) | **YES** |
| CH6 | 100 % (46/46) | 100 % (47/47 incl. new CH6-D) | **2** (V1 + V2) | **YES** |
| CH7 | 100 % (30/30 mandate cells) | 100 % (30/30 + 5 V2-strengthening) | **2** (V1 + V2) | **YES** |

**Cohort gate state:** 5 / 7 per-lens LOCKs at V2 (CH2 / CH3 / CH5 /
CH6 / CH7); 2 / 7 require V3 confirming cycle (CH1 + CH4). V3 dispatch
required for cohort §3Z LOCK.

### §1.2 — Predicted V3 → cohort §3Z LOCK shortcut

V3 work surface narrows to two cells (per §5 below): `p2f:164` C10 +
`p2f:197` C13 verb-tense polish (F-V2-CH1-1 + F-V2-CH4-1 convergent
finding); plus optional non-blocking F-V2-CH4-2 documentation-cohesion
mirror (P2-C + P2-D §4 adopt P2-A per-candidate CH4 enumeration —
non-LOCK-blocking). V3 expected:

- CH1: 100 % candidate / 100 % artefact (verb-tense fix discharges the
  §2.4 ACCEPT-WITH-NOTE finding cleanly; second consecutive ≥ 95 %
  cycle → CH1 per-lens 2-cycle LOCK achieved).
- CH4: 100 % strict / 100 % alt / 100 % 5-stage aggregate (verb-tense
  fix discharges F-V2-CH4-1; second consecutive ≥ 95 % cycle → CH4
  per-lens 2-cycle LOCK achieved).
- CH2 / CH3 / CH5 / CH6 / CH7: zero V3 work expected; per-lens 2-cycle
  LOCK already satisfied at V2; V3 is verification-only.

**Predicted cohort §3Z LOCK close at V3** with sub-axis aggregate +
per-lens mean both at 100.0 % across all seven lenses for two
consecutive cycles (V2 + V3) and zero orphan REVISEs.

## §2 — V2 strengthening packet

Each V2 fold packet (V1 Fold-1 through Fold-6) discharged the V1
REVISE roster per §0.4 above. The V2 strengthening surface beyond
REVISE discharge:

### §2.1 — NF-CH6-1 (scalar-reference vocabulary uniformly grounded) — RETAINED V2 + STRENGTHENED

V1 finding: 46-candidate corpus had zero "scalar reference TBD"
entries. V2 verification: P2-F §2.10 + §2.13 upgrade V1 "required with
shape" to Stage-A path:line + signature shape pinning; P2-F §2.2 (C2)
cites three-way Gap 6 composition with all three primitives
executable-verified at HEAD (`bitmap_prefix_xor_64_scalar` at
`skinny/crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:2`;
`escape_mask_64` at `skinny/crates/bbnf-simd/src/lib.rs:175`;
`scan_string_special_block_scalar` at
`skinny/crates/bbnf-simd/src/aarch64/string_block.rs:31`). NF-CH6-1
STRENGTHENED at V2.

### §2.2 — NF-CH6-2 (CH4-binding overlap C8/C10/C12/C13) — DISCHARGED V2

V1 finding: four P2-F candidates carried CH4-binding gaps. V2: C8
demoted; C10 + C13 Stage-A target naming; C12 CF-1 reframed ACCEPT
citing existing `scan_structurals_scalar` at
`skinny/crates/runtime/src/grammars/json/scan.rs:32` (executable-
verified). All four resolved cleanly.

### §2.3 — NF-CH6-3 (P2-F C2 PMULL prefix-XOR scalar-ref reframe) — DISCHARGED V2

V1 finding: P2-F C2 named "scalar reference required" but P2-A + P2-E
Gap 6 named existing composition. V2: `p2f:81` rewrites C2 scalar-ref
status to "EXISTS via composition per P2-E Gap 6" naming the three
composition primitives at path:line; all three executable-verified
live at HEAD. NF-CH6-3 DISCHARGED with executable composition
verification.

### §2.4 — NF-CH6-4 (long-string-body SIMD scan triple consolidation) — DISCHARGED V2

V1 finding: three artefacts surfaced same primitive under three names
(P2-A C2 + P2-E Gap 1 + P2-F C1+C2). V2: §2.Y cross-axis tracking
note at `p2f:231-239` records all three artefacts + explicit S-P3
consolidator binding ("S-P3 must produce ONE canonical primitive name
+ ONE canonical scalar reference function rather than admitting three
near-duplicates"). NF-CH6-4 DISCHARGED with explicit S-P3 binding;
exemplary anti-paper-close pattern preserved per-axis grounding rather
than silently consolidating.

### §2.5 — NF-CH6-5 (anti-paper-close exemplar pattern codification) — DISCHARGED V2

V1 finding: P2-D C-P2D-4 + P2-C C-P2C-2 demonstrated strongest
anti-paper-close discipline. V2: disposition-stamp template at P2-C
`§2.X.0` lines 63-65 codifies the pattern verbatim and applies to
C-P2C-1/-6/-7; same pattern appears at P2-D `§1.6(d)` ("Demoted V2:
zero hot-leaf consumer at SK-V14; re-elevate to candidate if S-P3
finds same-wave consumer") and P2-F `§2.X.1` ("Re-promotion gate
(CH1 + CH4 + CH6 joint condition)"). NF-CH6-5 DISCHARGED with codified
template uptake across three axes.

### §2.6 — NF-CH6-6 (F-V2-P1ABC-RERECORD single binding entry) — DISCHARGED V2 by this consolidator

V1 finding: F-V2-P1ABC-RERECORD surfaced in 4 of 6 files but no file
tied to specific S-P3 wave commit. V2: cite-density verified at 29
citations across 5 of 6 axis files (P2-A 2; P2-C 12; P2-D 0
intentional [substrate-side]; P2-E 7; P2-F 8); 18 candidates carry
the dependency cite (1 P2-A C6 + 5 P2-C + 6 P2-E gaps per §4.7 + 5
P2-F active + 1 P2-F C8 re-promotion gate); orchestrator 11+
threshold exceeded.

The V1 consolidator §2.1 binding entry (lines 230-289) authored the
single-binding-entry packet:

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
Consumers (must-bind, `[no-deferrals]`):
         P2-A C6 + P2-C C-P2C-3 + P2-C C-P2C-8 + P2-E Gap 1 + Gap 3
         + Gap 4 + Gap 5 + P2-F C6 + C7 + C10 + C12 + C13
Convention: per CH2 F2 elevation, any S-P3 wave admitting these
         primitives MUST ship the rerun in Stage 0 of the same wave
```

V2 cohort confirms the binding entry remains correct; the dual-gating
elevation per CH2 F-V2-CH2-2 finding fully propagates across all 12
consumer-dependency candidates with explicit per-row stamps. NF-CH6-6
DISCHARGED.

### §2.7 — CH6-D demotion-honesty sub-test (V2-new)

V2 CH6 lens introduces a fourth sub-check beyond V1's CH6-A
(comparator citation) / CH6-B (ISA citation) / CH6-C (scalar-reference
sketch): **CH6-D (demotion-honesty)** — a candidate moved out of
active enumeration must (i) preserve technical content verbatim, (ii)
carry explicit disposition stamp with re-promotion gate, (iii) retain
identifier for cross-tranche stability.

V2 CH6-D PASS rate: **6/6 = 100 %** across the six demoted /
pre-blocked rows (P2-C C-P2C-1, -6, -7; P2-C C-P2C-2 pre-block
exemplar; P2-D C-P2D-3; P2-D C-P2D-4 pre-block exemplar; P2-F C8).
This is V2's executable anti-overfit signature per CH7 V2 §4 finding 1.

## §3 — Cross-lens convergence findings

### §3.1 — §2.Y NF-CH6-4 cross-axis tracking note as load-bearing cross-CH-axis structural win

The P2-F V2 §2.Y cross-axis tracking note at `p2f:231-239`
simultaneously satisfies five distinct lens objectives at the V2
cycle:

1. **CH2 generality binding** (F-V2-CH2-1): preserves grammar-
   neutrality across three colliding axes by binding S-P3 to one
   canonical primitive name + one canonical scalar reference rather
   than admitting three orthogonal SIMD bodies for one underlying
   primitive (each potentially with its own per-grammar-config drift —
   a Lock 14 v+1 violation surface).
2. **CH5 hidden-coupling discovery** (CH5-V2-C): identifies that three
   artefacts surface the same long-string-body SIMD scan primitive
   under three distinct names; the §2.Y note operates at the DESIGN-DOC
   surface (S-P2 research artefacts) and SURFACES the would-be-violation
   BEFORE it ships at the S-P3 admitted-primitive surface.
3. **CH6 anti-paper-close exemplar** (NF-CH6-4 DISCHARGED): V2 did NOT
   silently consolidate the three axis names (which would have lost
   cross-axis provenance) — it preserved per-axis grounding AND added
   the S-P3 canonical-name binding for the wave plan.
4. **CH7 anti-naming-overfit pattern** (V2 finding 3): three axes
   converging on one primitive could be a naming-overfit failure
   pattern; §2.Y converts it to a CH6-affirmative consolidation pin
   that explicitly names the three convergent identifiers + binds
   S-P3 to ONE canonical primitive name.
5. **CH4 wave-level cost reduction** (F-V2-CH4-4): three near-duplicate
   candidates collapse to one canonical primitive — one Stage-A
   authoring + one Stage-B checkasm + one Stage-C lock manifest tie +
   one Stage-D consumer binding, not three. CH4 cost reduction, not
   addition.

The §2.Y note is the **load-bearing V2 cross-CH-axis structural win**:
five lenses converge on the same artefact as exemplary, with each
lens validating from its own discipline angle. The convergence
demonstrates the CHALLENGE lenses are compositional (multiple lenses
catch the same surface) and self-reinforcing (the lens that catches
the discovery also binds the remediation to S-P3).

### §3.2 — F-V2-P1ABC-RERECORD CH2/CH4 dual-gating propagation (F-V2-CH2-2)

The V1 finding F2 ("parse-attribution rerun is co-required by CH2
verdicts on C6 / C-P2C-3 / Gap 5") proposed elevating
F-V2-P1ABC-RERECORD from CH4-only to CH2/CH4 dual-gated. V2
ratification: the dual-gate is propagated to **all 12 consumer-
dependency candidates** named in HARDENING-S-P2-V1-CONSOLIDATED §2.1
lines 252-271:

- P2-F §4 risk row at `p2f:300` explicitly carries the "Inherited V2
  carry-forward F-V2-P1ABC-RERECORD" framing with the dual-gate S-P3
  verification clause ("S-P3 must ensure the wave that admits C6
  carries the parse-attribution rerun in the same wave").
- P2-F §2.X.1 C8 re-promotion gate at `p2f:229` explicitly names
  F-V2-P1ABC-RERECORD as (a) re-promotion condition.
- P2-C §2.X demotion stamp template at `p2c:63-65` names
  "F-V2-P1ABC-RERECORD CH2/CH4 dual-gate per HARDENING-S-P2-V1-CONSOLIDATED.md:230-289"
  as the re-evaluation surface for the three demoted P2-C candidates
  (C-P2C-1, -6, -7).
- CH6 V2 cite-density verification confirms 29 citations across 18
  candidates; orchestrator 11+ threshold exceeded.

Zero V2 edit silently relaxes the dual-gate; the dual-gate is the
explicit dispatch-context inheritance carry-forward for every
dispatch-envelope-internal primitive candidate.

### §3.3 — F-V2-CH7 strengthening: demotion-with-gap-note-preservation as executable anti-overfit signature

V2 corpus is now visible across three artefacts:
- P2-C: 3 demotions (C-P2C-1, -6, -7 to §2.X non-candidate inventory).
- P2-D: 1 demotion (C-P2D-3 to §1.6(d) substrate-side observation +
  §2 gap-note row at lines 128-130).
- P2-F: 1 demotion (C8 to §2.X.1 non-candidate inventory).

Total V2 demotions: **5** (on top of V1's 7 P2-C demotions + 1 P2-D
pre-block + 1 P2-F NEUTRAL-PENDING-CONSUMER baseline). Every V2
demotion preserves the candidate identifier as a gap-note for
cross-tranche stability; every V2 demotion preserves the full
technical content verbatim under a disposition stamp; every V2
demotion names an explicit re-promotion gate. This is the V1 finding
1 pattern ("anti-scaffold-admit pattern executed in vivo")
GENERALISED from one artefact (p2c) to three (p2c, p2d, p2f).

CH7 finding 2 additionally: **Stage-A target naming is overfit-
asymmetric** — V2 frames `byte_context_64.rs` + `bcax_64.rs` as
not-yet-existent paths cited as authoring targets with sibling-shape
templates explicitly named for shape inheritance. This is the
OPPOSITE of the overfit pattern (citing paths that don't exist as if
they were evidence); the V2 prose explicitly stamps these as
"Stage-A authoring under same-wave Lock 16 same-commit discipline" —
making the not-yet-existent status the load-bearing framing, not an
oversight. CH7-affirmative recommendation: S-P3 inherit this exact
stamp pattern for any new primitive authoring target naming.

### §3.4 — V1-LOCKED axes P2-A + P2-E carry zero V2 drift (six-witness confirmation)

`git diff b3dbc5ca0..447a26b07 -- p2a-sota-teardown.md
p2e-parse-that-gaps.md` returns empty per executable verification at
six independent witness points:
1. CH1 V2 §2.5 — explicit drift audit.
2. CH2 V2 §1.1 — git-diff stat verification with file-by-file
   accounting.
3. CH3 V2 §1 — V1-LOCKED axis drift audit table.
4. CH4 V2 §3 — diff result + line-count corroboration.
5. CH5 V2 §5.6 — V1 → V2 cycle commits with diff verification.
6. CH6 V2 §7.4 + CH7 V2 §0.1 — drift audit verification.

`wc -l` confirms: P2-A 367 lines (V1 367; match); P2-E 342 lines (V1
342; match). Zero V2 edits, zero line-count drift. V1 CH-lens
verdicts on these two axes (P2-A 7/7 + P2-E 9/9 = 16/16 active
candidates) carry verbatim into V2.

### §3.5 — Substrate-union YES seven-witness corroboration preserved at V2

P2-D's V1 load-bearing finding **substrate-union holds at HEAD** is
preserved at V2 with one additional witness:

1. P2-D §1.1 line 27 — `grep -rn "struct.*Tape\b" skinny/crates/runtime/src/` returns 3 hits.
2. P2-D §1.5 lines 84-92 — architectural-block of new union variant.
3. P2-D §4.7 line 204 — "YES, the substrate union holds at HEAD".
4. P2-F §1.3 line 53 — holding assumption corroborated.
5. CH5 V2 §3 CH5-V2-A — executable re-verification at V2 HEAD: three
   hits at `tape/mod.rs:94` (`Tape<'input>`), `tape/assembler.rs:42`
   (`TapeBuilder<'input>`), `tape/mod.rs:92` (`TapeId(pub u64)`).
6. P1-V3-CH5 `research/p1/hardening/V3/CH5.md:78-83` — two-cursor
   independence verification.
7. **V2-new witness:** P2-D §4.7 V2-amended explicit clause "the
   V2-demoted §1.6(d) sparse-flag observation (formerly C-P2D-3) is
   CH5-clean by construction (re-uses existing substrate field, no
   new sidecar)" + §4.6 V2-amended Lock 1 mitigation clause "even if
   re-elevated by a future same-wave consumer it does not introduce
   a new substrate field".

The V2 demotion of C-P2D-3 is the artefact's primary CH5 verification
at V2; the demotion preserves substrate-union YES verbatim and adds
a V2-explicit anti-regression clause for future re-elevation.

**Consequence for S-P3:** C-P2C-2 Lock 1 dependency condition (c)
remains satisfied at the dispatch level at V2; admission narrows to
wave-program deliverables (a) SIMD-first direct tuple writeback that
DELETES scalar consume, (b) strict same-row non-regression on the
11-row set Item 88/89 falsified, (d) emitted-asm proof of `pmull.1q` +
`ctz`.

## §4 — V3 fold packet (bounded; 3 LIGHT polish items)

V3 work surface is **bounded to 2 cells** (one convergent
verb-tense fix flagged by CH1 + CH4) + 1 non-blocking documentation
cohesion item. Total V3 wall time ≈ 5 min sequential; LOCK-blocking
items zero.

### §4.1 — F-V2-CH1-1 + F-V2-CH4-1 convergent verb-tense fix (LIGHT; 2 cells)

**Closes:** CH1 V2 §5.1 V3 Fold-1 + CH4 V2 §6.1 V3 fold suggestion
F-V2-CH4-1.

**Scope:** Two-line replacement at `p2f-grammar-neutral.md`:

- `p2f:164` (C10 trailing per-row Fold-N attribution): change
  "Authoring landed as Fold-4 V2 deliverable per
  HARDENING-S-P2-V1-CONSOLIDATED §3.4." → "Authoring queued for S-P3
  same-wave Lock 16 same-commit admission per
  HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-4; function body lands
  same-commit with SIMD body at S-P3."
- `p2f:197` (C13 trailing per-row Fold-N attribution): change
  "Authoring landed as Fold-5 V2 deliverable per
  HARDENING-S-P2-V1-CONSOLIDATED §3.4." → "Authoring queued for S-P3
  same-wave Lock 16 same-commit admission per
  HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-5; function body lands
  same-commit with SIMD body at S-P3."

**Severity:** BOUNDED (the load-bearing §2.10 + §2.13 status-prefix
"Stage-A authoring under same-wave Lock 16 same-commit discipline" is
CORRECT at V2; only the trailing Fold-N attribution postscript carries
the "landed" verb-tense imprecision a strict reader CAN misinterpret
as "the .rs file exists at HEAD").

**Cost:** ≈ 2 min wall (LIGHT). Two line replacements.

**Convergence impact:** discharges the §2.4 CH1 ACCEPT-WITH-NOTE
finding + F-V2-CH4-1 finding cleanly; CH1 + CH4 V3 ACCEPT 100 % with
zero residual ACCEPT-WITH-NOTE qualifications. V3 second-consecutive
≥ 95 % cycle achieved on both lenses.

### §4.2 — F-V2-CH4-2 partial §4 per-candidate CH4 mirror (LIGHT; non-blocking)

**Closes:** CH4 V2 §6.2 V3 fold suggestion F-V2-CH4-2 + V1 CF-3
documentation-discipline carry-forward.

**Scope:** P2-C `§4` + P2-D `§4` adopt P2-A `§4 paper-close subsection`
shape — one bullet per candidate naming the exact CH4 requirement
admission depends on. P2-F `§4` already mirrors much of P2-A's
per-candidate shape (with REDRESS bindings + V2-added "DISCHARGED V2"
stamps); P2-C + P2-D do not.

**Severity:** NON-BLOCKING. The load-bearing CH4 evidence IS present
per §2 candidate rows of each artefact; the §4 mirror is documentation-
cohesion improvement, not a load-bearing CH4 binding requirement. CH4
V2 reads this as ACCEPT-with-finding, not REVISE.

**Cost:** ≈ 15 min wall (LIGHT). Per-candidate bullet authoring at
P2-C §4 + P2-D §4 mirroring P2-A §4 lines 256-266 shape.

**Convergence impact:** zero (CH4 V3 verdict already ACCEPT regardless;
this is a cohesion improvement for cross-artefact documentation parity).
Optional; if not landed at V3, CF-3 carry-forward extends to S-P3
admission documentation.

### §4.3 — No other V3 fold targets surface from V2 cycle

The seven V2 lens audits surface zero new REVISE findings beyond the
two cells above. All V2 strengthening notes (NF-CH6-1 strengthened;
NF-CH6-2/-3/-4/-5 discharged; NF-CH6-6 discharged by this consolidator;
CH6-D 6/6 PASS; CH5 four V2-new findings A-D non-blocking; CH7 five
V2 findings affirmative) are ACCEPT-class with zero open REVISE
escalation surface.

## §5 — V3 dispatch shape

### §5.1 — Axes that fold V3 (P2-F §2.10 + §2.13 only; 2 cells)

- **P2-F** — F-V2-CH1-1 + F-V2-CH4-1 convergent verb-tense fix at
  `p2f:164` (C10) + `p2f:197` (C13). 2 line replacements. LIGHT
  mechanical edit.
- **(Optional non-blocking)** — P2-C §4 + P2-D §4 per-candidate CH4
  mirror per F-V2-CH4-2. If landed at V3, additionally folds P2-C +
  P2-D; if deferred, CF-3 carry-forward to S-P3 admission documentation.

### §5.2 — Axes that lock at V2 (5 axes)

- **P2-A** — V1-LOCKED + zero V2 drift; ACCEPT 7/7 lenses at both V1
  + V2; per-lens 2-cycle LOCK satisfied at V2 across CH2/CH3/CH5/CH6/CH7.
  V3 verification-only.
- **P2-B** — V2 Fold-1 SHA pinning discharged the only V1 REVISE (CH1
  §3.1); ACCEPT 7/7 lenses at V2; CH1 V3 confirming cycle required
  for per-lens 2-cycle LOCK. No V3 P2-B fold needed.
- **P2-C** — V2 Fold-2 demotion of C-P2C-1/-6/-7 discharged the only
  V1 REVISE on this axis; ACCEPT 8/8 (5 active + 3 demoted) at V2;
  optional F-V2-CH4-2 §4 per-candidate CH4 mirror non-blocking.
- **P2-D** — V2 Fold-2 demotion of C-P2D-3 discharged the only V1
  REVISE on this axis; ACCEPT 4/4 (2 active + 1 demoted-to-§1.6(d) +
  1 pre-blocked-anti-pattern) at V2; substrate-union YES preserved
  with seven-witness corroboration; optional F-V2-CH4-2 §4 per-candidate
  CH4 mirror non-blocking.
- **P2-E** — V1-LOCKED + zero V2 drift; ACCEPT 9/9 at both V1 + V2;
  CH3 ACCEPT-WITH-NOTE on Gap 5 REDRESS-80 differential carries
  through unchanged (correct CH3 shape per CH3 binding, not a fold
  target). V3 verification-only.

### §5.3 — Cross-axis V3 deliverables (cohort-wide)

- **CH1 + CH4 V3 confirming cycle**: re-verify the two-cell verb-tense
  polish lands cleanly + reconfirms ACCEPT 100 % on both lenses to
  satisfy the second consecutive ≥ 95 % cycle for per-lens 2-cycle
  LOCK.
- **CH2 / CH3 / CH5 / CH6 / CH7 V3 verification-only**: per-lens
  2-cycle LOCK already satisfied at V2; V3 reconfirms zero drift on
  the five lenses; no V3 work surface beyond verification.

## §6 — Predicted §3Z LOCK at V3 close

### §6.1 — V3 fold-only forecast

With the two-cell verb-tense polish landed (F-V2-CH1-1 + F-V2-CH4-1
convergent) + optional non-blocking F-V2-CH4-2 documentation-cohesion
mirror at P2-C + P2-D §4 (≈ 5 min wall convergent + ≈ 15 min wall
optional / total ≈ 20 min):

| Lens | V2 rate | Expected V3 rate (fold-only) | Net |
|---|---:|---:|---|
| CH1 | 100 % cand / 100 % artefact | 100 % cand / 100 % artefact | F-V2-CH1-1 verb-tense fix discharges §2.4 ACCEPT-WITH-NOTE |
| CH2 | 100 % | 100 % | per-lens 2-cycle LOCK satisfied at V2; V3 verification-only |
| CH3 | 100 % | 100 % | per-lens 2-cycle LOCK satisfied at V2; ACCEPT-WITH-NOTE on Gap 5 REDRESS-80 carries through V3 unchanged |
| CH4 | 100 % strict / 100 % 5-stage aggregate | 100 % strict / 100 % 5-stage aggregate | F-V2-CH4-1 verb-tense fix discharges trailing-attribution imprecision; F-V2-CH4-2 documentation cohesion optional |
| CH5 | 100 % | 100 % | per-lens 2-cycle LOCK satisfied at V2; V3 verification-only |
| CH6 | 100 % (47/47) | 100 % (47/47) | per-lens 2-cycle LOCK satisfied at V2; V3 verification-only |
| CH7 | 100 % (30/30 mandate cells + 5 V2-strengthening) | 100 % | per-lens 2-cycle LOCK satisfied at V2; V3 verification-only |

**Expected V3 sub-axis-weighted aggregate:** 100.0 % (145/145).
**Expected V3 per-lens mean:** 100.0 %.

### §6.2 — V3 cycle outcome forecast + §3Z LOCK close

**V3 outcome under fold-only:** ≥ 95 % on both aggregation methods +
zero orphan REVISEs (V2 already at 100.0 % + 0 REVISE; V3 maintains).
**Second consecutive ≥ 95 % cycle achieved on CH1 + CH4** — per-lens
2-cycle LOCK satisfied on all 7 lenses at V3 close.

**Cohort §3Z LOCK criterion:** ≥ 95 % aggregate × 2 consecutive cycles
+ zero orphan REVISEs. V2 + V3 satisfy both clauses on the load-bearing
sub-axis-weighted aggregate AND every individual lens.

**Predicted close path: V2 → V3 → COHORT §3Z LOCK.** V3 is the
confirming second-consecutive ≥ 95 % cycle on CH1 + CH4 (the only two
lenses requiring V3 confirmation); the five other lenses already
contribute clean per-lens 2-cycle LOCK baselines from V1 + V2. S-P3
dispatch gate opens at V3 LOCK per `PASS-2-RESEARCH.md §3` +
`ORCHESTRATOR.md §3Z`.

## §7 — Sources

V2 lens dispositions (all verified existing at write-time):

- `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CH1.md` (526 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CH2.md` (359 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CH3.md` (548 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CH4.md` (680 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CH5.md` (328 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CH6.md` (756 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CH7.md` (259 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CHALLENGE-CONTEXT.md` (39 lines)

V2 P2 axis artefacts under review (HEAD `447a26b07`):

- `restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md` (367 lines; V1-LOCKED — zero V2 drift)
- `restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md` (217 lines; V2-amended via Fold-1 SHA pinning)
- `restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md` (164 lines; V2-amended via Fold-2 demotion 8→5 active)
- `restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md` (254 lines; V2-amended via Fold-2 demotion 3→2 active)
- `restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md` (342 lines; V1-LOCKED — zero V2 drift)
- `restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md` (360 lines; V2-amended via 6-sub-fold packet)

Prior-cycle precedent:

- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md` (659 lines; V1 aggregator + fold-packet authority; format mirror for this aggregator)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CH{1..7}.md` + `CHALLENGE-CONTEXT.md` (V1 lens dispositions; carry-forward)

Binding authorities:

- `restart/prompts/skinny/PASS-2-RESEARCH.md §3` (CH1-CH6 specialisations) +
  `§8.6` (substrate union closing pin)
- `restart/prompts/ORCHESTRATOR.md §3W` (universal CH1-CH6 lens registry) +
  `§3Z` (convergence rule)
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7` (Overfit-Prune
  lens definition; carry-forward from S-P0)
- `restart/skinny/tranches/sk-v14/research/p2/S-P2-DISPATCH-CONTEXT.md`
  (S-P2 dispatch spec; F-V2-P1ABC-RERECORD inheritance)
- `restart/locks/LOCKS.md` (Lock 1 substrate-union v+1 manifest;
  Lock 14 v+1 grammar-neutrality; Lock 15 i-cache budget; Lock 16
  SIMD/ASM allowlist + abstract-primitive declarations)

V2 cycle commit anchors:

- V1 axis commit: `b3dbc5ca0e3ccf38df71a5e72be3d65a3068549b` ("docs(sk-v14-p2-hardening-V1): challenge V1 + consolidated").
- V2 axis commit: `447a26b07c353b217905c15a3d61c907a8e78410` ("docs(sk-v14-p2-V2): atomic micro-fold (4 axes amended) + V2 dispatch context"); 5 files changed (V2 CHALLENGE-CONTEXT + 4 amended axes; P2-A + P2-E zero drift); 137 insertions / 53 deletions; zero Rust source files modified.
- V1 → V2 diff verification: `git diff b3dbc5ca0..447a26b07 -- p2a-sota-teardown.md p2e-parse-that-gaps.md` returns empty (V1-LOCKED axes zero-drift confirmed across six lens-witnesses).

Bbnf source anchors (V2 HEAD-verified):

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
- `skinny/crates/bbnf-simd/src/scalar/` directory listing at HEAD: 8 files (`bitmap_next_set_bit.rs`, `bitmap_prefix_xor_64.rs`, `bulk_emit_positions_64.rs`, `byte_class_from_eq_set_64.rs`, `byte_class_from_table_64.rs`, `eob_pad_clamp.rs`, `mod.rs`, `swar_8byte.rs`); `byte_context_64.rs` + `bcax_64.rs` NOT YET PRESENT (Stage-A authoring targets per P2-F §2.10 + §2.13 — correctly framed)

External (SOTA + ISA) citations pinned at HEAD:

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
