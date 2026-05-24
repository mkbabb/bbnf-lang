# SK-V14 S-P3 V2 CHALLENGE CH4 — Cost Lens

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V2 (second consecutive
≥95% target per `ORCHESTRATOR.md §3Z`). Lens: CH4 COST.
Date: 2026-05-23. HEAD: 75657df14397c19790addd500999f6e7f2558e93.
Lens scope: every wave carries (LOC budget + hard cap + phase
breakdown research/plan/redress per SKINNY-TRIUMVIRATE.md +
same-wave-consumer per primitive); wave count ≤ 12 (skinny-bracket
ceiling per ORCHESTRATOR.md §3Z); shortlist ≤ 8; CF-3 3-gate
admission cell wired per candidate; W6 9-sub-wave folding (PRUNE-4)
carries cumulative cap with restatement at the sub-wave manifest
header (V2 fold target F-V2-CH4-1).
Authority: `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/
CHALLENGE-CONTEXT.md`; `PASS-3-SYNTHESIS-PLAN.md §3` CH4;
`SKINNY-TRIUMVIRATE.md §7 §8 §9`; `ORCHESTRATOR.md §3Z`;
`restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CH4.md` (V1
100% ACCEPT-bearing with one V2 clarity REVISE).
Discipline: write-only; no git add/commit; aggregator commits 8
hardening files atomically.
HARD CAP: 30 min.

## §1 — V2 disposition focus (from V2 CHALLENGE-CONTEXT §2)

Per `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CHALLENGE-CONTEXT.md:27`:

> CH4 COST: verify F-V2-CH4-1 §9 W6 810-min cumulative cap footnote;
> wave count = 12 + shortlist = 8 preserved at V2.

Per V2 CHALLENGE-CONTEXT §2 Special V2 attention bullets:
1. SPEC §2 W9 W1-only dependency (parallel-eligibility with W2-W8) —
   verify intentional per SPEC §0.1.
2. W11 close ceremony has no source LOC + no row gate (qualitatively
   different gate category) — flagged as P3-B fold observation,
   secondary CH4 concern.
3. W9 fused 34-row admit budget per 90-min cap — verify P3-C §12 LOC
   + cap accommodation.

The V1 cycle was 100% ACCEPT-bearing (8/8) with one single-line
clarity REVISE folded into V2 (the 810-min restatement). V2 must
verify (a) the V2 fold landed, (b) no V2 edit re-opened any
previously-ACCEPTed clause, (c) the three Special V2 attention
observations are accommodated.

## §2 — Deliverable (per-clause CH4 disposition at V2 HEAD)

### §2.1 — F-V2-CH4-1 §9 W6 810-min cumulative cap footnote landed — VERDICT: ACCEPT

**Verification.** SPEC §9 W6 sub-wave manifest at `restart/skinny/
tranches/sk-v14/SPEC.md:713` carries the V2-folded footnote verbatim:

> **Cap footnote (per §2 manifest restated for dispatch-time
> clarity):** Each W6.N sub-wave carries the ≤90-min implementation/
> redress cap; the W6 aggregate cumulative cap across W6.1..W6.9 is
> ≤810 min per `SPEC.md:243`. Any sub-wave or aggregate overflow
> returns REVISE per `[generated-size-budget]`.

The footnote sits immediately above the sub-wave manifest table
(`SPEC.md:715-725`) where it is dispatch-time visible to any agent
opening §9. It cites `SPEC.md:243` (the §2 manifest row) and binds
overflow handling to `[generated-size-budget]` per memory feedback.

Cross-check: `grep -c "810 min" SPEC.md` = 2 (one in §2 manifest row
W6 at line 243, one in §9 W6 footnote at line 713). Both citations
agree on the ≤90/sub-wave + ≤810/aggregate discipline.

Cross-check P3-B §2.1 row W6 at `restart/skinny/tranches/sk-v14/
research/p3/p3b-wave-sequencing.md:82` carries the same binding
verbatim: "≤90 min per sub-pass × 9 = ≤810 min cumulative cap per
SPEC §2; sub-pass split-before-dispatch if any single sub-pass
overflows the 90-min cap per SK-V8 SPEC §2." Cross-witnessed.

Per `HARDENING-S-P3-V1-CONSOLIDATED.md:516-519`:

> F-V2-CH4-1 — W6 sub-wave 810-min cumulative cap restatement [...]
> Action: one-line footnote restating 810-min cumulative cap (the
> cap exists verbatim at `SPEC.md:243`; restatement is non-load-
> bearing readability nit).

The V1 fold-packet target landed verbatim at V2; the restatement is
present, the dispatch-time clarity REVISE is discharged.

**Verdict: ACCEPT.** F-V2-CH4-1 landed; W6 cumulative-cap discipline
holds at both the §2 manifest level (line 243) and the §9 sub-wave
manifest header (line 713); no defect.

### §2.2 — Wave count = 12 preserved at V2 — VERDICT: ACCEPT

**Verification.** SPEC §2 wave-manifest rows at `SPEC.md:237-248`
enumerate W0, W1, W2, W3, W4, W5, W6, W7, W8, W9, W10, W11 — count
= 12.

Executable verification: `grep -nE '^\| W[0-9]' SPEC.md` returns the
12 manifest rows (237-248), plus 12 rerun-ceiling rows (279-290 — 12
rows including W6.1..W6.9 sub-wave line at 285), plus 9 W6 sub-wave
rows (717-725). Manifest itself = 12 lines exactly at the
`ORCHESTRATOR.md §3Z` + `SKINNY-TRIUMVIRATE.md §3` ≤12 ceiling.

Per V1 §2.5 ACCEPT (carry-forward): "Wave count = 12 at the
ceiling; 9 W6 sub-waves correctly folded under single wave slot to
preserve the count." V2 reads identical: 12 manifest rows; W6.1..
W6.9 sub-waves folded under §9 W6 row at `SPEC.md:243` + enumerated
at `SPEC.md:717-725`.

V2 edits to SPEC §11 + §12 + §13 (F-V2-CH6-1 unconditional Stage-0
binding to W10) do NOT introduce a new top-level wave; the Stage-0
binding lives within W10's existing §13 section.

V2 edits to SPEC §15 + §4 (F-V2-CH3-1/2/3 enumerations) do NOT
introduce a new wave row.

V2 edits to SPEC §9 W6 (F-V2-CH4-1 footnote) do NOT modify the
manifest row count.

**Verdict: ACCEPT.** Wave count = 12 verbatim at the ceiling; V2 +50
LOC SPEC edit (1137 → 1187) folded under existing wave sections; no
wave-count inflation.

### §2.3 — Shortlist = 8 preserved at V2 — VERDICT: ACCEPT

**Verification.** P3-A §2.1 candidate-shortlist table at `restart/
skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md:171-178`
enumerates 8 candidates:

| # | Candidate (canonical) |
|---|---|
| C1 | `long_string_body_simd_scan` |
| C2 | `structural_index_singular_substrate_consumer` |
| C3 | `digit_block_simd_accumulate` |
| C4 | `unicode_escape_neon_nibble_decode` |
| C5 | `parse_attribution_envelope_cracker` |
| C6 | `force_inline_lto_envelope_discipline` |
| C7 | `ascii_whitespace_skip_64` |
| C8 | `BackendShape::SinkOnly` activation |

Executable verification: `grep -c '^| C[1-9] ' p3a-candidate-
shortlist.md` = 16 (the 8 shortlist rows at §2.1 lines 171-178 + the
8 same-wave-consumer cells at §2.X lines 207-214). 8 ≤ 8 ceiling
preserved.

V2 P3-A amendment (per V2 CHALLENGE-CONTEXT §1 bullet 1):
- F-V2-CH2-1 C3 same-wave consumer = bbnf-simd checkasm row CSS-
  permissive `byte_class_from_range_64` (cited in the row 173 same-
  wave-consumer cell — does NOT add a new candidate, refines C3's
  consumer naming).
- F-V2-CH2-2 C4 same-shape consumer = BBNF-self string-escape +
  variable-width CSS \\HEXHEX carved out as measured-rejection (cited
  in row 174 — does NOT add a new candidate, refines C4's consumer
  naming).

Both V2 P3-A edits are within-row refinements; the shortlist row
count is unchanged at 8.

NF-CH6-4 canonical-name binding consolidations preserved verbatim at
`p3a:172` (C1 = P2-A C2 ∪ P2-E Gap 1 ∪ P2-F C1+C2) — convergent
identifier consolidation prevents the three orthogonal SIMD bodies
inflation per S-P2 V3 §6.2.

**Verdict: ACCEPT.** Shortlist = 8 verbatim at the ≤8 ceiling; V2
amendments refine same-wave-consumer naming per CH2 fold without
inflating the count.

### §2.4 — W11 close ceremony qualitatively-different gate category — VERDICT: ACCEPT

**Verification (Special V2 attention bullet 2).** SPEC §14 W11 row
at `SPEC.md:248` carries: "0 source LOC; docs/RESULTS/REDRESS/
HANDOFF/SPEC reconciliation only; ≤90 min". P3-B §2.1 row W11 at
`p3b:87` matches: "0 source LOC; docs/RESULTS/REDRESS/HANDOFF/SPEC
reconciliation only; close-honesty checklist + document
reconciliation." Same-wave consumer at `SPEC.md:1055`: "close
checklist and document reconciliation."

W11 IS qualitatively different from W0-W10:
- Zero source LOC (no implementation).
- Zero row gate (no Mbps threshold).
- Cap is ≤90 min for ceremony only (not implementation/redress).

Per `SKINNY-TRIUMVIRATE.md §3` + `ORCHESTRATOR.md §3Z`, the close
wave IS part of the bracket; the 12-wave ceiling is the implemen-
tation+ceremony envelope, not implementation-only. Reading the V1
§2.5 wave-count interpretation: "Wave count = 12 at the ceiling; 9
W6 sub-waves correctly folded under single wave slot" — W11 is the
12th slot, ceremony-typed.

The cap-without-LOC-and-without-row-gate combination IS coherent at
CH4: a ceremony-typed wave consumes wall-clock budget (≤90 min for
SK-V13 mirror discipline) but produces no parser/codegen LOC and
gates no performance row. The `SKINNY-TRIUMVIRATE.md §7` phase-cap
table accommodates ceremony waves (no research/CHALLENGE phases —
plan + redress collapse into a single reconciliation pass at SPEC §2
manifest row 248).

**Verdict: ACCEPT.** W11 ceremony gate category is intentional per
SK-V8 SPEC §2 mirror discipline + `SYNTHESIS.md §0.1 R10` close
condition; W11 is the 12th-slot ceremony wave, not a hidden
implementation slot. No CH4 binding is violated.

### §2.5 — W9 fused 34-row admit budget vs 90-min cap accommodation — VERDICT: ACCEPT

**Verification (Special V2 attention bullet 3).** P3-C §2.9 at
`restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-
gates.md:347-396` enumerates W9's row budget:
- 17 JSON `direct_to_struct` rows (verbatim at `p3c:351`).
- 17 JSON `real_typed_struct` rows (verbatim at `p3c:353`).
- Total admit budget = 34 rows under the ≤90-min cap.

SPEC §2 W9 row at `SPEC.md:246`: "≤450 source/test LOC; rows named
in wave plan | ≤90 min". P3-B §2.1 W9 row at `p3b:85`: "≤450 source/
test/REDRESS LOC (consumer wiring; primitives drawn from S-P2
LOCKED pool, never re-authored)".

The 90-min cap accommodates 34 rows because:

1. **Primitives drawn from S-P2 LOCKED pool, NEVER re-authored at
   W9** (verbatim at `p3b:85`). The candidate pool was COHORT-LOCKED
   at S-P2 V3 HEAD `ebe84954b`; W9 is consumer-wiring only.
   Per-row workload is wiring + threshold validation, not
   primitive authoring.

2. **W9 wiring is bench-harness mechanical**, not parser-source
   rewriting. The W9 owner-path family (per `SPEC.md:903-911`):
   - `bbnf-bench/benches/json_parity.rs` (row wiring against rebound
     R1 comparators).
   - `bbnf-bench/src/real_typed_struct.rs` (typed re-admit per
     corpus; per-corpus typed-struct binding stubs exist at lines
     695-727 per V1 CH1 §6.3).
   - `codegen/` only IF re-admit requires generated path changes
     (conditional on plan).
   - `RESULTS.md` + `ROLLING-SOTA-DELTA.md` row attribution.

3. **34 rows = 17 × 2 planes; per-row delta is comparator-binding
   selection + threshold cell** (`Track 1 Mbps ≥ sonic-rs strict
   struct deser Mbps + 1 Mbps` per `p3c:358` direct; `Track 1 Mbps ≥
   per-corpus typed struct deser Mbps + 1 Mbps` per `p3c:363`
   typed). The R1 rebound work (W1 binding) was already done in W1;
   W9 inherits and applies.

4. **LOC budget ≤450 source/test/REDRESS** (`p3b:85`) is conjunctive
   with the 90-min cap per `SPEC.md:255-256`: "A wave plan that
   exceeds either its LOC budget or the 90-minute implementation /
   redress cap must split before dispatch or return REVISE." If a
   subset of the 34 rows requires per-row codegen work that pushes
   either bound, the W9 plan splits before dispatch (e.g., direct
   plane wave + typed plane wave) per the no-deferrals + split-or-
   REVISE discipline.

5. **Per-row exit gates at `p3c:372-386`** are mechanically uniform:
   comparator_plane binding, per_iter_equality PASS, threshold cell,
   audit_overlay_verdict transition. The 34 rows are NOT 34
   independent investigations; they are 34 attestation cells against
   a fixed-shape oracle per plane.

6. **Per-corpus revert protocol at `p3c:392`** is row-level: "revert
   the per-corpus admit (per plane); row stays at W1
   audit_overlay_verdict=AUDIT-FALSIFIED posture; REDRESS records
   the threshold miss." Failure is per-row, not bracket-level; the
   90-min cap binds the wiring envelope, not the per-row admit
   success rate.

7. **Same-wave consumer** (`SPEC.md:942-944`): "selected JSON
   direct + typed rows consume generated Track 1 direct or typed
   work + independent Track 2 proof in the same wave" — confirms
   the wiring discipline; no orphan primitive risk.

The 34-row admit budget is accommodated by the (i) S-P2 LOCKED pool
consumer-only discipline + (ii) bench-harness mechanical wiring
shape + (iii) per-row split-or-REVISE escape hatch + (iv) ≤450 LOC
+ ≤90 min conjunctive cap.

**Cross-witness (V2 fold accommodation flag).** Per V2 CHALLENGE-
CONTEXT §2 Special V2 bullet 3: "W9 fused 34-row admit budget per
90-min cap — verify P3-C §12 LOC + cap accommodation." The above
six-point analysis verifies the accommodation; no fold edit is
required at V2. If W9 dispatch demonstrates a per-row workload that
overflows the 90-min cap, the split-or-REVISE discipline at
`SPEC.md:255-256` is the binding response — not a V2 SPEC edit.

**Verdict: ACCEPT.** W9's 34-row admit budget is accommodated by the
S-P2-LOCKED-consumer-only discipline + per-row split-or-REVISE
escape; the ≤90-min cap binds wiring envelope, not row-attestation
arithmetic.

### §2.6 — W9 W1-only dependency (parallel-eligibility with W2-W8) — VERDICT: ACCEPT

**Verification (Special V2 attention bullet 1).** SPEC §2 W9 row at
`SPEC.md:246`: "Conditional on W1 close (depends only on R1+R2, not
on PRUNE waves)". P3-B §2.1 W9 row at `p3b:85` matches: "Conditional
on W1 close (depends only on R1+R2, not on PRUNE waves per SPEC §2
line 246)". SPEC §12 W9 entry gate at `SPEC.md:919-923` matches:
"W1 admitted (R1 + R2 strict-comparator binding + per-iter equality
oracle)."

Per SPEC §0.1 R10 close condition + `SYNTHESIS.md §0.1` Pass Alpha
goalset: R1 (sonic-rs strict struct deser per corpus on direct
plane) + R2 (per-iter equality oracle inside timing region) are the
SHAPE-INDEPENDENT comparator-rebind primitives. PRUNE-1..PRUNE-5
(W1 part-A + W4 + W5 + W6 + W7) operate on shape selection, runtime
collapse, and SCAFFOLD→LOAD-BEARING promotion — none of which the
R1+R2 comparator rebind requires.

The W9 W1-only dependency is intentional because:

1. **R7 JSON direct + typed re-admit operates against the rebound
   strict comparators**, not against the post-PRUNE BackendShape
   selection. The comparator rebind happens at W1 (R1+R2);
   downstream PRUNE waves do not modify the comparator.

2. **PRUNE waves do not introduce a new direct or typed parser**;
   they re-route to emitted output from `regen-{grammar}` (W2-W6) +
   demote shape arms (W4-W5) + wire policy/union (W7). The W9 row
   admits CONSUME post-W1 comparators + existing JSON parser
   surface (or, conditionally, the W6.9-emitted JSON runtime if
   W6.9 closed first — but W9 does not block on W6.9).

3. **Parallel-eligibility window:** per the wave manifest
   conditional-dispatch chain, W9 may dispatch in parallel with any
   of W2..W8 as long as W1 has closed. The conditional-dispatch
   surface at `SPEC.md:202` ("Conditional on W{prior} close" per
   row) defines a partial order, not a total order; W9's only
   binding antecedent is W1.

4. **W10 dependency on both W1 AND W9** at `SPEC.md:247`:
   "Conditional on W1 + W9 close" — W10 is the first wave that
   binds BOTH the comparator rebind AND the W9 direct/typed
   re-admit. This confirms W9's antecedent set is {W1}, not
   {W1, ..., W6}.

5. **Per SPEC §0.1 R10**: "every JSON cell (51 = 17 × 3) ... ADMITs
   > strict-vs-strict OR carries architectural-block proof" — the
   close condition does NOT serialize JSON admits behind CSS PRUNE
   waves. JSON cells (R7 direct + R7 typed + R8 parse_only) are
   independently admissible per Pass Alpha goalset.

**Verdict: ACCEPT.** W9's W1-only dependency + parallel-eligibility
with W2-W8 is intentional per SPEC §0.1 R10 + the partial-order
conditional-dispatch chain; no CH4 cost binding is violated. The
12-wave ceiling counts W9 as a parallel-eligible slot, not a
serialized slot.

### §2.7 — Carry-forward verification: V1's 8/8 ACCEPT clauses preserved at V2 — VERDICT: ACCEPT

**Verification.** V1 CH4 (`restart/skinny/tranches/sk-v14/research/
p3/hardening/V1/CH4.md`) closed 8/8 ACCEPT-bearing with one V2
clarity REVISE on §2.2 (W6 sub-wave aggregate cap restatement). V2
must verify no V2 edit re-opened any V1 ACCEPT clause:

| V1 § | V1 clause | V2 HEAD status |
|---|---|---|
| §2.1 | Every wave carries a LOC budget | ACCEPT (12/12 wave rows at `SPEC.md:237-248` carry LOC budget cell; aggregate envelope at `SPEC.md:258-261`) |
| §2.2 | Every wave carries a hard cap | ACCEPT (12/12 wave rows carry ≤90 min cap; W6 also carries ≤810 min aggregate at `SPEC.md:243` AND restated at `SPEC.md:713` — V2 fold landed) |
| §2.3 | Phase breakdown (research/plan/redress) | ACCEPT (phase-cap table verbatim from SKINNY-TRIUMVIRATE §7 at `SPEC.md:263-273`; role-separation per §9 at `SPEC.md:218`) |
| §2.4 | Same-wave-consumer per primitive | ACCEPT (`grep -c "Same-wave consumer:" SPEC.md` = 12; one per W0..W11; SPEC §1 non-negotiable at `SPEC.md:216`) |
| §2.5 | Wave count ≤ 12 | ACCEPT (12 manifest rows at `SPEC.md:237-248`; W6.1..W6.9 folded under W6 slot) |
| §2.6 | Shortlist ≤ 8 | ACCEPT (8 candidates at `p3a:171-178`; NF-CH6-4 canonical-name binding preserved at `p3a:172`) |
| §2.7 | CF-3 3-gate admission cell per candidate | ACCEPT (8/8 candidates carry 3-gate cell at `p3a:171-178`; SPEC §1 non-negotiable at `SPEC.md:216`) |
| §2.8 | W6 9-sub-wave cumulative cap | ACCEPT (810-min cap at manifest `SPEC.md:243` + footnote at `SPEC.md:713`; V1 §2.2 REVISE discharged) |

All 8 V1 clauses preserved at V2 HEAD. No V2 edit re-opened any V1
disposition.

**V2 net delta:**
- SPEC: 1137 → 1187 lines (+50; six sub-folds per V2 CHALLENGE-
  CONTEXT §1 bullet 7; includes F-V2-CH4-1 footnote at line 713).
- P3-A: 8 candidates preserved; F-V2-CH2-1/2 within-row refinements.
- P3-B: full section-relabel to SPEC §2 ordering W0..W11; gate
  content + LOC budgets + caps preserved byte-identical aside from
  wave-id refresh.
- P3-C: 527 → 537 lines (+10; W1 fused C-2+PRUNE-1; W9 fused R7-
  direct+typed; new §2.11 W11; zero gate-content inconsistencies;
  all 75 corpus rows preserved verbatim).

CH4 cost discipline is preserved across all V2 edits; the V2 cycle
is CH4-coherent.

**Verdict: ACCEPT.** All 8 V1 ACCEPT clauses preserved at V2;
F-V2-CH4-1 V2 fold landed; no carry-forward regression.

## §3 — Falsifiability binding (executable verification commands at V2 HEAD)

Per `PASS-3-SYNTHESIS-PLAN.md §3` CH4 disposition vocabulary
(ACCEPT / REVISE / REJECT) + LAC-1E-12 executable-verification
procedural addendum. All commands run at HEAD `75657df14`.

| Clause | Verification command | V2 HEAD output | V1 expected | Status |
|---|---|---:|---:|---|
| F-V2-CH4-1 footnote landed | `grep -nE "Cap footnote" SPEC.md` returns line 713 | line 713 PRESENT | NEW (V2) | LANDED |
| 810-min cumulative cap | `grep -c "810 min" SPEC.md` | 2 | ≥1 (V1) → ≥2 (V2) | PASS |
| Wave count = 12 | manifest row count `^\| W[0-9]` at SPEC.md:237-248 | 12 | =12 | PASS |
| Shortlist = 8 | shortlist row count `^\| C[1-9] ` at p3a:171-178 | 8 | =8 | PASS |
| Same-wave consumer per wave | `grep -c "Same-wave consumer:" SPEC.md` | 12 | =12 | PASS |
| LOC budget per wave | all 12 manifest rows carry "Source/edit LOC budget" cell | 12 cells populated | ≥12 | PASS |
| Hard cap per wave | all 12 manifest rows carry "Implementation/redress cap" cell | 12 cells populated | ≥12 | PASS |
| Phase breakdown citation | `SPEC.md:263-273` reproduces SKINNY-TRIUMVIRATE §7 | reproduced verbatim | binding cite | PASS |
| W9 W1-only dependency | `SPEC.md:246` reads "Conditional on W1 close (depends only on R1+R2, not on PRUNE waves)" | verbatim PRESENT | (Special V2) | INTENTIONAL |
| W11 ceremony cap | `SPEC.md:248` reads "0 source LOC; docs/.../SPEC reconciliation only" + "≤90 min" | verbatim PRESENT | (Special V2) | INTENTIONAL |
| W9 34-row budget | `p3c:351 + p3c:353` enumerate 17 direct + 17 typed rows | 17 + 17 enumerated | (Special V2) | ACCOMMODATED |

Each disposition above carries the named verification command per
LAC-1E-12 ("any cited path:line in any wave's plan or redress MUST
be re-executed at HEAD before commit"). All commands re-executed at
HEAD 75657df14.

## §4 — Pre-blocked routes (CH4-specific, carry-forward from V1 §4)

Per V1 §4 CH4-binding pre-blocks + S-P2 V3 §6.1 CF-3 carry-forward,
the following CH4 anti-patterns remain pre-blocked at V2; no V2
edit may re-open any of these routes:

1. **Wave count inflation** — adding a 13th wave (e.g., promoting a
   W6 sub-wave to top-level) breaches `ORCHESTRATOR.md §3Z` ceiling.
   V2 HEAD: 12 manifest rows verbatim; no V2 edit inflated the
   count (verified §2.2).

2. **Shortlist inflation** — adding a 9th candidate breaches
   `PASS-3-SYNTHESIS-PLAN.md §2` ≤8 cap. V2 HEAD: 8 candidates
   verbatim; F-V2-CH2-1/2 are within-row refinements, not new
   candidates (verified §2.3).

3. **Missing same-wave-consumer** — any wave whose redress phase
   admits a primitive without naming its hot-path consumer in the
   same commit re-opens the SK-V5 orphan-kernel failure shape. V2
   HEAD: 12/12 "Same-wave consumer:" lines present in SPEC §3-§14
   (verified §2.7).

4. **Missing 3-gate CH4 admission cell** — per S-P2 V3 §6.1 CF-3
   binding, every shortlisted candidate's admission manifest carries
   (scalar-ref status / checkasm-parity expectation / same-wave-
   consumer NAMED). V2 HEAD: 8/8 candidates carry 3-gate cell at
   `p3a:171-178` (verified §2.7).

5. **W6 sub-wave dispatch without cumulative-cap awareness** — if a
   single W6.N sub-wave consumes >90 min, the per-sub-wave cap
   binds split-before-dispatch per `p3b:82` + `SPEC.md:243` + the
   NEW V2 footnote at `SPEC.md:713`. The 810-min aggregate is NOT
   retry room (verified §2.1 + §2.7).

6. **LOC budget overflow without REVISE** — per `SPEC.md:255-256`:
   "A wave plan that exceeds either its LOC budget or the 90-minute
   implementation / redress cap must split before dispatch or
   return REVISE." Silent overflow is REJECT (preserved at V2).

7. **Phase-role merger** — per `SKINNY-TRIUMVIRATE.md §9` + SPEC §1
   non-negotiable at `SPEC.md:218`: research / plan / CHALLENGE /
   redress phases remain distinct commits. Merging plan + redress
   into one commit re-opens the SK-V5 failure shape per V3
   triumvirate-discipline feedback (preserved at V2).

8. **NEW V2 pre-block — W9 34-row admit budget collapse into
   primitive-authoring**. Per `p3b:85` verbatim: "primitives drawn
   from S-P2 LOCKED pool, never re-authored". Any W9 plan that
   re-authors a primitive (instead of consuming the S-P2 LOCKED
   pool) breaches the consumer-wiring-only discipline that
   accommodates the 34-row budget under the 90-min cap; REJECT
   route (per V2 §2.5 analysis).

9. **NEW V2 pre-block — W9 dispatch behind any PRUNE wave**. SPEC
   §2 W9 row reads "Conditional on W1 close (depends only on R1+R2,
   not on PRUNE waves)"; the W1-only dependency is intentional per
   SPEC §0.1 R10. Adding a W2..W7 antecedent to W9 dispatch
   conditions breaches the partial-order conditional-dispatch chain
   (per V2 §2.6 analysis).

## §5 — Sources

### §5.1 — V2 CHALLENGE-CONTEXT authority

- `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CHALLENGE-CONTEXT.md` (43 lines; §0 authority + §1 artefacts + §2 V2 disposition focus + §3 discipline + §4 output structure).
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CH4.md` (V1 100% ACCEPT-bearing; single clarity REVISE folded as F-V2-CH4-1).
- `restart/skinny/tranches/sk-v14/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:80, 120, 141, 157, 516-519` (V1 aggregator + F-V2-CH4-1 fold packet).

### §5.2 — Contract authority

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md §3` CH4 verbatim.
- `restart/prompts/ORCHESTRATOR.md §3Z` (cohort LOCK = ≥95% × 2 consecutive cycles; V≤5 ceiling).
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md §3 §7 §8 §9` (12-wave ceiling; phase caps; same-wave-consumer rule; triumvirate role separation).

### §5.3 — P3 artefacts under V2 CH4 review

- `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md:171-178, 207-214` (8 shortlist rows + 3-gate cells; F-V2-CH2-1/2 within-row refinements at C3/C4).
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md:76-87` (12-wave manifest with LOC + cap columns + same-wave consumers).
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md:82` (W6 ≤810 min cumulative cap binding).
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md:85` (W9 W1-only dependency + ≤450 LOC + ≤90 min + S-P2 LOCKED pool consumer-wiring-only).
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md:347-396` (W9 fused R7-direct + R7-typed; 17 direct + 17 typed rows).
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md:436` (W11 close ceremony).

### §5.4 — SPEC under V2 CH4 review

- `restart/skinny/tranches/sk-v14/SPEC.md:216` (CF-3 3-gate non-negotiable).
- `restart/skinny/tranches/sk-v14/SPEC.md:218` (triumvirate-role-separation non-negotiable).
- `restart/skinny/tranches/sk-v14/SPEC.md:237-248` (12-wave manifest with LOC budget + hard cap columns).
- `restart/skinny/tranches/sk-v14/SPEC.md:243` (W6 ≤90/sub-wave + ≤810 aggregate cap).
- `restart/skinny/tranches/sk-v14/SPEC.md:246` (W9 W1-only dependency).
- `restart/skinny/tranches/sk-v14/SPEC.md:248` (W11 ceremony row).
- `restart/skinny/tranches/sk-v14/SPEC.md:252-256` (generated-output exemption + overflow-split-or-REVISE).
- `restart/skinny/tranches/sk-v14/SPEC.md:258-261` (aggregate envelope + 20%-overflow escalation per `[generated-size-budget]`).
- `restart/skinny/tranches/sk-v14/SPEC.md:263-273` (phase-cap table verbatim from SKINNY-TRIUMVIRATE §7).
- `restart/skinny/tranches/sk-v14/SPEC.md:275-292` (per-wave rerun ceilings).
- `restart/skinny/tranches/sk-v14/SPEC.md:363, 439, 498, 550, 608, 667, 755, 820, 882, 942, 1002, 1055` (12 Same-wave consumer lines W0..W11).
- `restart/skinny/tranches/sk-v14/SPEC.md:713` (**NEW V2 fold F-V2-CH4-1**: W6 sub-wave footnote restating 810-min cumulative cap).
- `restart/skinny/tranches/sk-v14/SPEC.md:715-725` (W6.1..W6.9 sub-wave manifest table).
- `restart/skinny/tranches/sk-v14/SPEC.md:919-944` (W9 entry gate + tasks + exit gate + same-wave consumer).

### §5.5 — S-P2 carry-forward authority (CF-3 binding preserved at V2)

- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md §6.1` (CF-3 3-gate admission cell binding).
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md §6.2` (NF-CH6-4 canonical-name binding for shortlist consolidation).
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md §6.3` (F-V2-P1ABC-RERECORD Stage-0 wave commitment).

### §5.6 — Memory feedback honored

- `[no-deferrals]` — phase-cap overflow forbids deferral to future tranches; in-pass split-or-REVISE is binding.
- `[dispatch-hard-cap]` — every dispatch carries cap; CH4 lens enforces.
- `[triumvirate-discipline]` — research / plan / redress role separation forbids merger.
- `[generated-size-budget]` — per-tranche line-count budget; overflow >20% blocks wave per SPEC §2; cited verbatim in F-V2-CH4-1 footnote at `SPEC.md:713`.
- `[execute-planned-architecture]` — same-wave-consumer rule prevents orphan-kernel retreat.
- `[no-workarounds]` — W9's 34-row budget under 90-min cap is accommodated by S-P2 LOCKED pool consumer discipline, not by deferral or shortcut.
- `[doc-integration-style]` — V2 fold F-V2-CH4-1 is integrated as a deft single-line footnote, not a bolted-on section.

---

## §6 — Lens disposition summary (V2)

| § | Clause | V2 Verdict |
|---|---|---|
| §2.1 | F-V2-CH4-1 §9 W6 810-min cumulative cap footnote landed | ACCEPT |
| §2.2 | Wave count = 12 preserved at V2 | ACCEPT |
| §2.3 | Shortlist = 8 preserved at V2 | ACCEPT |
| §2.4 | W11 close ceremony qualitatively-different gate category | ACCEPT |
| §2.5 | W9 fused 34-row admit budget vs 90-min cap accommodation | ACCEPT |
| §2.6 | W9 W1-only dependency (parallel-eligibility with W2-W8) | ACCEPT |
| §2.7 | Carry-forward verification: V1's 8/8 ACCEPT clauses preserved at V2 | ACCEPT |

**CH4 V2 ACCEPT-rate: 7/7 = 100%** (V1 was 8/8 = 100% with one V2
clarity REVISE; V2 verifies the REVISE landed + adds three Special-
V2-attention dispositions + the carry-forward verification clause —
re-collapsed into 7 V2 clauses without losing V1 coverage).

**Cycle disposition: V2 ACCEPT-bearing (second consecutive ≥95%
cycle).** CH4 cost-lens converges on V2: F-V2-CH4-1 landed; all V1
ACCEPT clauses preserved; three Special-V2-attention observations
(W9 W1-only, W11 ceremony, W9 34-row admit) accommodated with
explicit textual / structural / process evidence. Per
`ORCHESTRATOR.md §3Z`: V2 second consecutive ≥95% target met for
CH4. Predicted trajectory: V2 → V3 confirming → cohort LOCK at V3.

**Key V2 findings:**

1. **F-V2-CH4-1 V2 fold landed verbatim at `SPEC.md:713`**: one-line
   footnote restating ≤90-min sub-wave cap + ≤810-min cumulative
   cap with overflow-returns-REVISE binding per `[generated-size-
   budget]`. The cap restatement is dispatch-time visible above the
   W6.1..W6.9 sub-wave manifest table; V1 §2.2 clarity REVISE
   discharged.

2. **Wave count = 12 preserved verbatim** across V2's +50 SPEC LOC
   (1137 → 1187): V2 edits (F-V2-CH6-1 unconditional Stage-0
   binding at §11/§12/§13; F-V2-CH3-1/2/3 §15 + §4 enumerations;
   F-V2-CH5-1 §7 same-substrate-union gloss; F-V2-CH4-1 §9 W6
   footnote) all fold under existing wave sections without
   introducing a new wave row.

3. **Shortlist = 8 preserved verbatim**: F-V2-CH2-1 (C3 same-wave
   consumer = bbnf-simd checkasm row CSS-permissive
   `byte_class_from_range_64`) + F-V2-CH2-2 (C4 same-shape consumer
   = BBNF-self string-escape + variable-width CSS \\HEXHEX
   measured-rejection carve-out) are within-row refinements, not
   new candidates; NF-CH6-4 canonical-name binding preserved at
   `p3a:172`.

4. **W11 ceremony gate category is intentional and CH4-coherent**:
   zero source LOC + zero row gate + ≤90 min ceremony cap matches
   SK-V8 SPEC §2 mirror discipline + SYNTHESIS §0.1 R10 close
   condition; W11 is the 12th-slot ceremony wave, not a hidden
   implementation slot.

5. **W9 fused 34-row admit budget is accommodated by six-fold
   discipline**: (i) S-P2 LOCKED pool consumer-only, never re-
   authored; (ii) bench-harness mechanical wiring shape; (iii) ≤450
   LOC + ≤90 min conjunctive cap with split-or-REVISE escape; (iv)
   per-row mechanically-uniform exit gates; (v) per-row revert
   protocol (failure is row-level, not bracket-level); (vi)
   `[no-deferrals]` discipline prevents the budget from being
   stretched. NO V2 SPEC edit required.

6. **W9 W1-only dependency + parallel-eligibility with W2-W8 is
   intentional per SPEC §0.1 R10**: R1+R2 comparator rebind is
   shape-independent; PRUNE waves do not modify the comparator;
   W9's antecedent set = {W1}, NOT {W1, ..., W6}. W10 binds
   {W1, W9} because W10 is the first wave consuming BOTH the
   rebound comparator AND the W9 direct/typed re-admit.

7. **All 8 V1 ACCEPT clauses preserved at V2 HEAD**: no V2 edit
   re-opened any V1 disposition; the V1 → V2 trajectory is
   monotonic in CH4 coherence; V2 net delta consolidates the
   F-V2-CH4-1 fold and adds three Special-V2-attention dispositions.

8. **Two NEW V2 pre-blocked routes added** (§4 items 8 + 9): W9
   34-row admit budget collapse into primitive-authoring (route 8);
   W9 dispatch behind any PRUNE wave (route 9). Both routes capture
   the V2-cycle dispositions § 2.5 + § 2.6 as binding pre-blocks
   for downstream dispatch.

9. **No CH4-binding clause is missing or stub-coded at V2**; the
   cycle is CH4-coherent; CH4 lens predicts V2 → V3 confirming →
   cohort LOCK at V3 per `ORCHESTRATOR.md §3Z`.
