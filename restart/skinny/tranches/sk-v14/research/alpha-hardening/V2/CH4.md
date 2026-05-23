# CH4 COST — Pass Alpha V2 Disposition

Lens binding: `restart/prompts/ORCHESTRATOR.md:86` ("LOC budget, risk
class, wave alignment, and hard cap are stated and realistic; same-wave
consumer present per kernel/primitive"). V2 dispatch context:
`restart/skinny/tranches/sk-v14/research/alpha-hardening/V2/CHALLENGE-V2-ADDENDUM.md:46-51`
(§CH-4 V2 overlay). V1 disposition baseline:
`restart/skinny/tranches/sk-v14/research/alpha-hardening/V1/CH4.md`
(71% ACCEPT; 3 REJECT / 7 REVISE). The V2 overlay applies fold-landing
verification across α-F (SYNTHESIS + HANDOFF), α-E (candidate-shortlist),
α-A (results-extraction) per `V1/HARDENING-ALPHA-V1-CONSOLIDATED.md §2`.

## §0 — V2 Disposition summary

- **V2 ACCEPT-rate: 100 % (34 / 34 sectioned dispositions).**
- **V2 REJECT count: 0.** All three V1 REJECTs (R1, R2, R3) FOLD-LANDED in
  the V2 α-commit `958406257`; no new fold-pass REJECT surfaces.
- **V2 REVISE count: 0.** All seven V1 REVISEs (V1–V7) FOLD-LANDED. No
  fresh REVISE surfaces under §2 new-finding scan.
- **Critical findings: 0.** Cost surface is rebound across SYNTHESIS,
  HANDOFF, α-E, α-A with verbatim α-E narrative authority; cap discipline
  reads the addendum verbatim per CONSOLIDATED §0.5 reconciliation.
- **CH4 V2 verdict: CONVERGES.** Lens-local 100 % per §3.1 forecast in
  CONSOLIDATED.

### §0.1 — V1 → V2 fold-landing roll-up

| V1 finding | V2 fold token | Status | V2 evidence (`path:line`) |
|---|---|---|---|
| REJECT R1 (SYNTHESIS §3 risk column under-states) | F-3 | **FOLD-LANDED** | `sk-v14/SYNTHESIS.md:271,272,273,274,275` — C-1 VERY HIGH; C-2 HIGH; C-3 HIGH; C-4 VERY HIGH; C-5 MED-LOW |
| REJECT R2 (SYNTHESIS §3 LOC envelope absent) | F-4 | **FOLD-LANDED** | `sk-v14/SYNTHESIS.md:269` (column header), :271–275 (per-row values), :277 (total envelope ≈ 5.65k – 8.38k) |
| REJECT R3 (α-E §10 45-min cap miscategorisation) | E-2 | **FOLD-LANDED** | `alpha-E-candidate-shortlist.md:741-745` (C-1/C-2/C-3/C-5 = 30 min; only C-4 = 45 min); :747-758 cap-discipline reconciliation paragraph |
| REVISE V1 (C-1 LOC lower-bound under-states 64-file refactor) | E-11 | **FOLD-LANDED** | `alpha-E-candidate-shortlist.md:83` (table row: 2.8k – 3.4k); :178 ("C-1 LOC budget. 2.8k – 3.4k …") |
| REVISE V2 (C-2 LOC envelope omits Skipper fallback ~80 LOC) | E-12 | **FOLD-LANDED** | `alpha-E-candidate-shortlist.md:84` (600 – 1.08k); :268-273 ("Ceiling raised by ≈ 80 LOC per CH4 V2 to cover the in-tree Skipper-class fallback path α-B §316-320 flags") |
| REVISE V3 (§9 vs §6 C-1↔C-4 sequencing inconsistency) | E-13 | **FOLD-LANDED** | `alpha-E-candidate-shortlist.md:714` (matrix row: C-4 must serialise after C-1 ALL sub-waves); :716-726 (matrix-text resolution paragraph anchoring §6 as authoritative) |
| REVISE V4 (SYNTHESIS §3 same-wave consumer column absent) | F-5 | **FOLD-LANDED** | `sk-v14/SYNTHESIS.md:269` (column header includes "Same-wave consumer"); :271–275 per-row values lifted from α-E §3–7 |
| REVISE V5 (HANDOFF §6 hard caps absent from next-move) | F-7 | **FOLD-LANDED** | `sk-v14/HANDOFF.md:162-165` ("Hard caps echoed per `[dispatch-hard-cap]`: 30-min lens-agent cap; research 20 min / plan 15 min / redress 30 min (45 min only for the addendum-amended decision-engine fold + C-4 per CONSOLIDATED §0.5 cap discipline).") |
| REVISE V6 (α-A §5 c/B LOC budget absent) | A-3 | **FOLD-LANDED** | `alpha-A-results-extraction.md:296-316` ("c/B column LOC budget (carry-with-C-2): … bounded by ≈ 80-120 LOC … fits inside C-2's existing 600 LOC lower-bound envelope") |
| REVISE V7 (SYNTHESIS §4 per-wave LOC ceiling absent) | F-6 | **FOLD-LANDED** | `sk-v14/SYNTHESIS.md:326-329` ("S-P3 wave manifest inherits per-candidate LOC envelopes from α-E §2 (C-1 2.8k–3.4k; C-2 600–1.08k; C-3 1.2k–2.0k; C-4 800–1.4k; C-5 250–500; total ≈ 5.65k–8.38k); any wave exceeding its envelope by > 20 % escalates per `[generated-size-budget]`;") |

Roll-up: **10 / 10 V1 findings FOLD-LANDED.** Zero FOLD-PARTIAL; zero
FOLD-MISSING.

## §1 — Per-artefact disposition table (V2)

V2 disposition retains the V1 per-§ ACCEPT decisions; only the previously
REJECTed / REVISEd cells are re-evaluated below. Every other cell carries
its V1 ACCEPT through.

| Artefact | § | V1 disposition | V2 disposition | Reason |
|---|---|---|---|---|
| `SYNTHESIS.md` | §0.1 Close condition (R10) | ACCEPT | ACCEPT | Unchanged from V1; close-condition cost surface inherited from `ORCHESTRATOR-PROMPT.md`. |
| `SYNTHESIS.md` | §0.2 Goalset row enumeration | ACCEPT | ACCEPT | F-1 reconciliation paragraph lands at `SYNTHESIS.md:82-90` per CH6; CH4 cost surface unaffected — populations remain audit-zero. |
| `SYNTHESIS.md` | §0.3 R-target acceptance | ACCEPT | ACCEPT | R-target acceptance unchanged. |
| `SYNTHESIS.md` | §0.4 P-1 … P-7 pre-blocks | ACCEPT | ACCEPT | Pre-blocks unchanged; F-10 round-trip-rule trigger appended without cost impact. |
| `SYNTHESIS.md` | §0.5 Wave-by-wave gates deferred | ACCEPT | ACCEPT | Contract boundary preserved; §3 candidate table now carries LOC + same-wave consumer per F-4 + F-5. |
| `SYNTHESIS.md` | §1.1 Survives pillars | ACCEPT | ACCEPT | Carry-forward; no cost change. |
| `SYNTHESIS.md` | §1.2 Falsified items | ACCEPT | ACCEPT | F-1 reconciliation paragraph at `SYNTHESIS.md:200-209` widens the PRUNE-1 cost coverage from 4+7 to 6+11 rows; this propagates correctly to C-5's revert scope (`alpha-E:643`-style 29-row count holds since the extra 6 are typed/direct re-routes in REDRESS, not ledger reverts beyond the 29-keyed entries). |
| `SYNTHESIS.md` | §1.3 Honest rolling delta | ACCEPT | ACCEPT | Audit-zero baseline unchanged; cost-of-revert remains within C-5's 250–500 LOC envelope. |
| `SYNTHESIS.md` | §2 Telemetry binding | ACCEPT | ACCEPT | F-15 `track2_entry_point` column added at `SYNTHESIS.md:240`; CH5-bound, no cost overrun in C-2's 600–1.08k envelope (the column is a header + one xtask gate line ≈ 5-10 LOC). |
| `SYNTHESIS.md` | §3 Candidate shortlist | **REJECT** (R1, R2; V4) | **ACCEPT** | F-3 + F-4 + F-5 all landed; risk column matches α-E narrative verbatim; LOC envelope column present; same-wave consumer column present. See §3 below for fold-evidence. |
| `SYNTHESIS.md` | §4 S-P3 constraints | REVISE (V7) | **ACCEPT** | F-6 LOC-ceiling clause at `SYNTHESIS.md:326-329` lands the per-wave inheritance and the > 20 % escalation discipline. |
| `SYNTHESIS.md` | §5 Pre-blocked / unblocked routes | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §6 Close posture | ACCEPT | ACCEPT | Close-posture cost framing unchanged. |
| `HANDOFF.md` | §1 Bracket verdict | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §2 Authority list | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §3 Honest baseline summary | ACCEPT | ACCEPT | F-1 reconciliation propagated; cost surface unchanged. |
| `HANDOFF.md` | §4 Pre-S-P0 readiness | ACCEPT | ACCEPT | Unchanged; F-2 sole-author declaration is CH6-domain. |
| `HANDOFF.md` | §5 Pass sequence | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §6 Next-move | REVISE (V5) | **ACCEPT** | F-7 cap-echo paragraph at `HANDOFF.md:162-165` echoes the 30-min lens-agent cap + research/plan/redress caps with the addendum-amended 45-min carve-out (C-4 only) verbatim from CONSOLIDATED §0.5. F-8 G-Omega restoration also lands at `HANDOFF.md:159-160`. |
| `HANDOFF.md` | §7 Refusal conditions | ACCEPT | ACCEPT | F-16 UnionTape verbatim clause at `HANDOFF.md:230-233`; cost-bounded REVISE triggers hold. |
| `HANDOFF.md` | §8 V1 disposition | ACCEPT | ACCEPT | Unchanged. |
| `alpha-A-results-extraction.md` | §1–§4 per-plane row tables | ACCEPT | ACCEPT | Unchanged; A-1 + A-2 reconciliation rows are CH1-domain. |
| `alpha-A-results-extraction.md` | §5 c/B telemetry | REVISE (V6) | **ACCEPT** | A-3 c/B LOC-budget paragraph at `alpha-A-results-extraction.md:296-316` bounds the c/B add at ≈ 80-120 LOC; routes through C-2's existing 600 LOC envelope without ceiling raise. The decomposition (≈ 30-40 + 20-30 + 20-30 + 10-20 LOC per sub-component) is plausibly bounded and same-wave-consumer-compliant. |
| `alpha-A-results-extraction.md` | §6 verdict summary | ACCEPT | ACCEPT | Unchanged. |
| `alpha-A-results-extraction.md` | §7 forward pointers | ACCEPT | ACCEPT | Unchanged. |
| `alpha-B-competitor-deltas.md` | §316-320 Skipper absence | ACCEPT | ACCEPT | α-B STANDS unchanged per V2 addendum §0; estimate still correctly flagged. |
| `alpha-C-redress-digest.md` | full | ACCEPT | ACCEPT | C-1 (P-7 triple-check) is CH5-domain; CH4 cost-surface unchanged. |
| `alpha-D-validated-invalidated.md` | full | ACCEPT | ACCEPT | α-D STANDS unchanged per V2 addendum §0. |
| `alpha-E-candidate-shortlist.md` | §2 shortlist table | REVISE (V1) | **ACCEPT** | E-11 raises C-1 lower bound to 2.8k at `alpha-E-candidate-shortlist.md:83`; new envelope row (2.8k – 3.4k) reflects the 64-file refactor reality. Total envelope updated to 5.65k – 8.38k at :89. |
| `alpha-E-candidate-shortlist.md` | §3 C-1 narrative | ACCEPT | ACCEPT | E-7 forward-invariant clause + E-11 LOC reconciliation paragraph at :178-183 land cleanly. |
| `alpha-E-candidate-shortlist.md` | §4 C-2 narrative | REVISE (V2) | **ACCEPT** | E-12 ceiling raised to 1.08k at :84; rationale paragraph at :268-279 cites α-B §316-320 in-tree Skipper fallback (≈ 80 LOC) explicitly and bounds the fallback to `bbnf-bench/src/`. |
| `alpha-E-candidate-shortlist.md` | §5 C-3 narrative | ACCEPT | ACCEPT | E-1 round-trip gate expansion (CH7-binding) at :360-378 lands without cost overrun in the 1.2k – 2.0k envelope; E-6 family-shape binding lands at :294. |
| `alpha-E-candidate-shortlist.md` | §6 C-4 narrative | ACCEPT | ACCEPT | E-3 Lock-1 triad discipline at :464-475; E-4 module-path discipline; E-5 pre-wave hot-leaf citation; all land inside the 800–1.4k envelope. |
| `alpha-E-candidate-shortlist.md` | §7 C-5 narrative | ACCEPT | ACCEPT | E-10 scribe-contract count clarification (29 row-keyed entries) lands; cost envelope unchanged. |
| `alpha-E-candidate-shortlist.md` | §8 pre-blocked routes | ACCEPT | ACCEPT | Unchanged. |
| `alpha-E-candidate-shortlist.md` | §9 concurrency matrix | REVISE (V3) | **ACCEPT** | E-13 lands at :714 + :716-726; matrix now reads "C-4 \| (one shape at a time) \| C-1 (ALL sub-waves), C-2" and the §6-authoritative reconciliation paragraph names the prior matrix row as the contradiction source. |
| `alpha-E-candidate-shortlist.md` | §10 cost / caps / telemetry | **REJECT** (R3) | **ACCEPT** | E-2 lands at :741-745 (C-1/C-2/C-3/C-5 = 30 min; only C-4 = 45 min); :747-758 cap-discipline reconciliation paragraph cites CH4 R3 verbatim and bounds the 45-min eligibility to C-4 alone per the addendum. CH7 §3.3 per-sub-wave clarification correctly applied to C-4 only. |
| `alpha-E-candidate-shortlist.md` | §11 convergence + escalation | ACCEPT | ACCEPT | Unchanged. |
| `DISPATCH-CONTEXT.md` | full | ACCEPT | ACCEPT | STANDS unchanged per V2 addendum §0. |

## §2 — New findings (fresh-finding scan)

Per V2 addendum §1.2: "Look for issues the V1 lens did NOT catch."
COST-lens fresh scan executed over the V2 artefacts in entirety, with
emphasis on F-3 + F-4 + E-2 fold-touched surfaces, the SYNTHESIS §3
column-set arithmetic, the α-E §10 cap table, and the A-3 c/B
LOC-budget decomposition.

**No new REJECT.**
**No new REVISE.**

Fresh-finding notes (informational only; below the lens action
threshold):

- **N-1 (informational).** SYNTHESIS §3 total envelope reads
  "≈ 5.65k – 8.38k" at `SYNTHESIS.md:277`; α-E §2 reports "≈ 5.65k –
  8.38k" at `alpha-E-candidate-shortlist.md:89`. Arithmetic check:
  C-1 (2.8k–3.4k) + C-2 (0.6k–1.08k) + C-3 (1.2k–2.0k) + C-4 (0.8k–1.4k)
  + C-5 (0.25k–0.5k) = (2.8 + 0.6 + 1.2 + 0.8 + 0.25)k → (3.4 + 1.08 + 2.0
  + 1.4 + 0.5)k = 5.65k–8.38k. Matches exactly. No drift.

- **N-2 (informational).** CONSOLIDATED §3.1 forecast cited the V1
  total as "4.95k–8.3k"; the V2 total is 5.65k–8.38k. The 0.7k uplift
  on the lower bound is entirely attributable to E-11 (C-1: 2.1k → 2.8k);
  the 0.08k uplift on the upper bound is entirely attributable to E-12
  (C-2: 1.0k → 1.08k). Both reconcile to the V1 CH4 REVISE remedies
  verbatim. No drift; the headline `[generated-size-budget]` ceiling
  remains plausible for an 8-sub-wave + provider-trait collapse +
  64-file refactor cluster.

- **N-3 (informational).** A-3 c/B LOC budget at
  `alpha-A-results-extraction.md:296-316` allocates 80–120 LOC for the
  c/B column add and asserts it "fits inside C-2's existing 600 LOC
  lower-bound envelope (600–1.08k per α-E §2)". The envelope after
  E-12 is 600–1.08k (the E-12 ceiling-raise covers Skipper, not c/B);
  the c/B add and the Skipper fallback both share C-2 surface. Bounded
  worst case: 600 + 80 (Skipper) + 120 (c/B) = 800 LOC, still well
  inside the 1.08k ceiling. No envelope-ceiling collision; the
  same-wave consumer rule (`xtask gate-json` rejects rows without c/B)
  is preserved by A-3's same-commit placement. Bounded.

- **N-4 (informational).** α-E §10 cap reconciliation paragraph
  (`alpha-E-candidate-shortlist.md:747-758`) names "C-4 IS the W8 + W9
  decision-engine fold wiring the addendum names" — this is the right
  reading of the addendum's "W5–W9 fold and W12 union-SIMD wave"
  scope. C-4's per-CSP-selectable-shape 45-min cap (CH7 §3.3
  clarification) compounds correctly: N shapes × 45 min ≤ N × 45 min
  ceiling, with N ≥ 2 per the E-8 two-grammar-family requirement. The
  cluster total scales linearly with shape count; no cap-discipline
  drift.

- **N-5 (informational).** HANDOFF §6 cap echo at
  `HANDOFF.md:162-165` reads "research 20 min / plan 15 min /
  redress 30 min (45 min only for the addendum-amended decision-engine
  fold + C-4 per CONSOLIDATED §0.5 cap discipline)." This is precise:
  the addendum amendment covers W5–W9 + W12; C-4 IS the W8 + W9
  wiring; CONSOLIDATED §0.5 ratifies CH4 R3 as authoritative. The
  parenthetical "+ C-4" is technically redundant (C-4 ⊆ W5–W9 fold)
  but harmless as emphasis; below the lens threshold.

- **N-6 (informational).** SYNTHESIS §4 LOC-ceiling clause at :326-329
  uses "> 20 %" as the escalation threshold. `[generated-size-budget]`
  per memory does not explicitly bind a percentage threshold; the
  20 % choice is α-F's reasonable framing inside the lens's "stated
  and realistic" mandate. Bounded; no fresh REVISE.

## §3 — Critical findings detail (V2)

### V1 REJECT R1 — SYNTHESIS §3 risk column rebind — FOLD-LANDED

V1 finding: "SYNTHESIS §3 risk column under-states C-1, C-3, C-4 vs α-E
§3-6."

V2 fold F-3 evidence — `sk-v14/SYNTHESIS.md:271-275` risk column reads
verbatim:

> | **C-1** | … | VERY HIGH (architectural; multi-wave) |
> | **C-2** | … | HIGH (harness + comparator surface) |
> | **C-3** | … | HIGH (xtask + corpora + dual-tree round-trip) |
> | **C-4** | … | VERY HIGH (Lock-1 substrate-ceiling surface) |
> | **C-5** | … | MED-LOW (revert + REDRESS scribe) |

Matches CH4 V1 remedy ("C-1 VERY HIGH, C-2 HIGH, C-3 HIGH, C-4 VERY HIGH,
C-5 MED-LOW") exactly. C-4's annotation upgrades from "wires existing
scaffold" framing to "Lock-1 substrate-ceiling surface" — this is the
more rigorous CH5-aligned framing that the V1 lens was reaching for.
**ACCEPT.**

### V1 REJECT R2 — SYNTHESIS §3 LOC envelope column add — FOLD-LANDED

V1 finding: "SYNTHESIS §0.5 and §3 elide per-candidate LOC envelopes
that α-E §2 binds."

V2 fold F-4 evidence — `sk-v14/SYNTHESIS.md:269` table header gains a
`LOC budget` column; :271–275 carry the verbatim α-E values (with E-11
+ E-12 corrections applied): C-1 2.8k–3.4k; C-2 600–1.08k; C-3
1.2k–2.0k; C-4 800–1.4k; C-5 250–500. :277 carries the total envelope ≈
5.65k–8.38k as the SK-V14 PRUNE-phase ceiling, matching CH4 V1's
"optional summary cost envelope" remedy. **ACCEPT.**

### V1 REJECT R3 — α-E §10 hard-cap revert — FOLD-LANDED

V1 finding: "α-E §10 prescribes a 45-min redress cap for C-1 and C-3
without routing those waves through the addendum's
decision-engine-fold eligibility clause."

V2 fold E-2 evidence — `alpha-E-candidate-shortlist.md:739-745` table:

> | C-1 sub-waves (8 grammars; per sub-wave) | 20 min | 15 min | 30 min |
> | C-2                                       | 20 min | 15 min | 30 min |
> | C-3                                       | 20 min | 15 min | 30 min |
> | C-4 (per CSP-selectable shape)            | 20 min | 15 min | 45 min |
> | C-5                                       | 20 min | 15 min | 30 min |

Matches CH4 V1 remedy verbatim: "C-1 30 min, C-3 30 min, C-4 45 min, C-2
30 min, C-5 30 min." The reconciliation paragraph at :747-758 cites CH4
R3 explicitly as the binding lens disposition and the addendum
(`USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:129-134`) as authority.
CH7 §3.3 per-sub-wave clarification correctly applied to C-4 alone.

V2 fold F-7 echo at `sk-v14/HANDOFF.md:162-165` confirms downstream
binding: HANDOFF §6 next-move now carries the cap discipline verbatim
("research 20 min / plan 15 min / redress 30 min (45 min only for the
addendum-amended decision-engine fold + C-4 per CONSOLIDATED §0.5 cap
discipline)"). The CONSOLIDATED §0.5 cross-binding (CH4 ↔ CH7
reconciliation declaring CH4 authoritative) is preserved as the
downstream citation anchor. **ACCEPT.**

### Same-wave consumer audit (V2 re-check)

The same-wave consumer rule per `[execute-planned-architecture]` is
preserved across all V2 artefacts:

- SYNTHESIS §3 candidate table at `sk-v14/SYNTHESIS.md:269` now carries
  the "Same-wave consumer" column (F-5 landed); per-row values at
  :271–275 lift the α-E §3–7 consumer plans verbatim.
- SYNTHESIS §4 at :298-299 retains "support-only landings are invalid;
  every primitive lands WITH its hot-path consumer in the same commit
  per `[execute-planned-architecture]`".
- α-E §3–7 retain per-candidate consumer plans unchanged from V1; E-3
  (C-4 Lock-1 triad) and E-4 (module-path discipline) tighten the
  C-4 consumer plan further without weakening the rule.

**ACCEPT.**

## §4 — Escalation flag

NONE.

The CH4 V2 cycle converges at lens-local 100 % ACCEPT-rate; every V1
REJECT and REVISE folded verbatim into the V2 α-commit `958406257`; no
fresh-finding scan surface meets the REJECT / REVISE threshold. The
cost surface is rebound across SYNTHESIS, HANDOFF, α-E, α-A with
verbatim α-E narrative authority; cap discipline reads the addendum
verbatim. The V2 aggregator should consume this CH4 V2 disposition as
input to the CONSOLIDATED V2 verdict; no V3 CH4-specific re-fold is
required.

Per V2 addendum §4 convergence forecast: V2 aggregate is expected at
≥ 95 % under the "0–3 new findings per lens" historical assumption.
CH4 contributes 0 new findings to that aggregate (100 % lens-local
convergence); the SK-V14 bracket awaits V3 confirming pass per
`ORCHESTRATOR.md §3Z` two-cycle rule.
