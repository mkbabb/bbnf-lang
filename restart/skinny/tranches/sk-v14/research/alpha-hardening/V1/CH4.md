# CH4 COST — Pass Alpha V1 Disposition

Lens binding: `restart/prompts/ORCHESTRATOR.md:86` ("LOC budget, risk
class, wave alignment, and hard cap are stated and realistic; same-wave
consumer present per kernel/primitive"). Dispatch context:
`restart/skinny/tranches/sk-v14/research/alpha-hardening/V1/CHALLENGE-CONTEXT.md:130-146`
(§CH-4). The lens scope is COST realism + same-wave consumer truth across
the eight Pass Alpha artefacts.

## §0 — Disposition summary

- ACCEPT-rate: 71% (24 / 34 sectioned dispositions).
- REJECT count: 3.
  - R1. SYNTHESIS §3 risk column under-states C-1, C-3, C-4 vs α-E §3-6
    (`SYNTHESIS.md:241,243,244` vs `alpha-E-candidate-shortlist.md:179,353,456`).
  - R2. SYNTHESIS §0.5 and §3 elide per-candidate LOC envelopes that
    α-E §2 binds (`SYNTHESIS.md:136-145, 233-250` vs
    `alpha-E-candidate-shortlist.md:81-92`).
  - R3. α-E §10 prescribes a 45-min redress cap for C-1 and C-3 without
    routing those waves through the addendum's decision-engine-fold
    eligibility clause (`alpha-E-candidate-shortlist.md:617-626` vs
    `USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:129-134`).
- REVISE count: 7.
  - V1. α-E C-1 LOC envelope (2.1k–3.4k source) under-states 64-file
    refactor + 8 grammar sub-waves (`alpha-E-candidate-shortlist.md:168`).
  - V2. α-E C-2 LOC envelope omits in-tree Skipper-class fallback path
    flagged by α-B §316-320 (~80 additional LOC).
  - V3. α-E §3 C-1 declares dependency on C-3 + C-5 but §9 concurrency
    matrix permits C-1 JSON sub-waves to parallelise C-4 (post-C-1
    surface), creating a sequencing under-specification
    (`alpha-E-candidate-shortlist.md:599-609`).
  - V4. SYNTHESIS §3 candidate table omits the same-wave consumer
    column entirely (`SYNTHESIS.md:233-250`).
  - V5. HANDOFF §6 next-move chain elides hard caps for the CHALLENGE
    pass itself (`HANDOFF.md:144-161`).
  - V6. α-A §5 c/B telemetry surface remains schema-debt with no
    LOC budget assigned for the c/B column add
    (`alpha-A-results-extraction.md:244-261`).
  - V7. SYNTHESIS §4 S-P3 constraints invoke the same-wave consumer
    rule but omit any LOC budget ceiling per wave
    (`SYNTHESIS.md:252-287`).
- Critical findings: 3 (above REJECTs).

## §1 — Per-artefact disposition table

| Artefact | § | Disposition | Reason |
|---|---|---|---|
| `SYNTHESIS.md` | §0.1 Close condition (R10) | ACCEPT | Close-condition cost surface (per-cell architectural-block requirement) is verbatim from `ORCHESTRATOR-PROMPT.md:154-158`; cost realism inherited. |
| `SYNTHESIS.md` | §0.2 Goalset row enumeration | ACCEPT | Population counts (51 JSON × 3 planes + 24 CSS) match α-A §6 verdict matrix; no inflation. |
| `SYNTHESIS.md` | §0.3 R-target acceptance | ACCEPT | Each R-target has measurable acceptance; R3 sub-divides to 5 PRUNE waves consistent with α-E candidate slot allocation. |
| `SYNTHESIS.md` | §0.4 P-1 … P-7 pre-blocks | ACCEPT | Pattern pre-blocks bind every wave; cost of re-introducing the patterns is implicitly REJECT — consistent with `[no-deferrals]`. |
| `SYNTHESIS.md` | §0.5 Wave-by-wave gates deferred | ACCEPT | The deferral to S-P3 is contracted per `PASS-ALPHA.md §4.4`; the §4.4 layer rightly carries hard-cap + LOC budget per wave once S-P3 authors it. CH4 ACCEPTs the contract boundary; the deferral does not orphan the §3 candidate table because α-E §10 carries the cap table. |
| `SYNTHESIS.md` | §1.1 Survives pillars | ACCEPT | Pillar list is no-cost (carry-forward); inherits architectural cost from prior tranches. |
| `SYNTHESIS.md` | §1.2 Falsified items | ACCEPT | The four falsification classes name the cost domain each prune targets. |
| `SYNTHESIS.md` | §1.3 Honest rolling delta | ACCEPT | Restating to audit-zero is a deletion cost; no LOC budget required for revert in §1.3 (C-5 carries it in α-E §7). |
| `SYNTHESIS.md` | §2 Telemetry binding | ACCEPT | New columns (`comparator_plane`, `per_iter_equality`, `audit_overlay_verdict`) are cost-bounded within C-2's 600-1.0k LOC envelope per α-E §4. |
| `SYNTHESIS.md` | §3 Candidate shortlist | **REJECT (R1, R2; V4)** | Risk column mis-classifies C-1 / C-3 / C-4 relative to α-E originals; LOC envelopes absent; same-wave consumer column absent. See §2 below. |
| `SYNTHESIS.md` | §4 S-P3 constraints | REVISE (V7) | Constraints encode the same-wave consumer rule (line 263-265) but omit any per-wave LOC ceiling — the wave program can drift to monoliths without a written ceiling. |
| `SYNTHESIS.md` | §5 Pre-blocked / unblocked routes | ACCEPT | Pre-blocked list is no-cost (deletion); unblocked list correctly contingent on R-target landings. |
| `SYNTHESIS.md` | §6 Close posture | ACCEPT | Cost framing ("intentionally aggressive in obligation … surgical in sequencing") is consistent with the 8-sub-wave C-1 + parallel-C-2/C-3/C-5 sequence. |
| `HANDOFF.md` | §1 Bracket verdict | ACCEPT | Cost domain implicit (audit reversal is a sunk cost; no new admit cost authorised). |
| `HANDOFF.md` | §2 Authority list | ACCEPT | Read-order is no-cost. |
| `HANDOFF.md` | §3 Honest baseline summary | ACCEPT | Cost surface (eight pillars survive, four falsifications fall) cleanly enumerated. |
| `HANDOFF.md` | §4 Pre-S-P0 readiness | ACCEPT | Tree-state cost is zero (clean tree + doc seeds). |
| `HANDOFF.md` | §5 Pass sequence | ACCEPT | Ordering enforces PRUNE waves before re-admit waves — minimises sunk cost of misbinding propagation. |
| `HANDOFF.md` | §6 Next-move | REVISE (V5) | Next-move chain `CHALLENGE-V1 → G-Alpha → S-P0` lacks explicit hard caps for the CHALLENGE pass; the 30-min lens-agent cap is in CHALLENGE-CONTEXT but not echoed at HANDOFF §6, leaving downstream readers without budget. |
| `HANDOFF.md` | §7 Refusal conditions | ACCEPT | Refusal conditions are cost-bounded REVISE triggers; same-wave consumer rule (line 178-180) and Lock 14 substrate ban (line 186-192) hold. |
| `HANDOFF.md` | §8 V1 disposition | ACCEPT | "PENDING until CHALLENGE V1 returns" is correct cost posture. |
| `alpha-A-results-extraction.md` | §1–§4 per-plane row tables | ACCEPT | No COST claims to litigate; rows are evidence, not interventions. |
| `alpha-A-results-extraction.md` | §5 c/B telemetry | REVISE (V6) | c/B column remains schema-debt per `alpha-A-results-extraction.md:258-261`; no LOC budget assigned to closing it. C-2 covers comparator schema but not the c/B = `ns_per_byte × cpu_freq_GHz` extension. |
| `alpha-A-results-extraction.md` | §6 verdict summary | ACCEPT | 0/75 ADMITTED summary is cost-neutral; the 46 reclassifications each route through C-5's REDRESS scope. |
| `alpha-A-results-extraction.md` | §7 forward pointers | ACCEPT | Per-α handoff is cost-zero. |
| `alpha-B-competitor-deltas.md` | §316-320 Skipper absence | ACCEPT | The "~80 LOC in-tree Skipper" estimate is correctly flagged as expanding R1 scope; the cost is named even if α-E C-2 envelope under-includes it (see V2). |
| `alpha-C-redress-digest.md` | full | ACCEPT | REDRESS classifications carry cost implicitly (PRE-BLOCK shapes prevent recurrence cost). The 119-row digest does not claim LOC budgets; that is α-E's surface. |
| `alpha-D-validated-invalidated.md` | full | ACCEPT | Validated/invalidated classification is cost-neutral; carry-forward pillars and SK-V14 candidates are the surface α-E consumes. |
| `alpha-E-candidate-shortlist.md` | §2 shortlist table | REVISE (V1) | Total envelope 4.95k–8.3k is the right order of magnitude, but C-1's 2.1k-3.4k under-states a 64-file refactor + provider-trait collapse + 8 grammar sub-waves; the lower bound assumes delete-heavy refactor with no per-grammar emit branching, which the C-1 §3 narrative itself disclaims (line 169). |
| `alpha-E-candidate-shortlist.md` | §3 C-1 narrative | ACCEPT | Owner paths enumerate the 8 provider modules + 64-file `crates/core/src/runtime/` tree + decision-CSP hardcoded `"json"` rule string; same-wave consumer plan binds regen-derived runtime PLUS round-trip gate PLUS ±1% bench parity. Risk "very high" is correct. |
| `alpha-E-candidate-shortlist.md` | §4 C-2 narrative | REVISE (V2) | C-2 envelope 600-1.0k omits the in-tree Skipper-class fallback (~80 LOC) α-B §316 names; the §4 narrative ACKNOWLEDGES the fallback in pre-blocked routes (line 265-267) without amending the LOC ceiling. |
| `alpha-E-candidate-shortlist.md` | §5 C-3 narrative | ACCEPT | 1.2k-2.0k envelope covers xtask + harness + codegen wiring + corpora vendoring + provenance + bench extension; the cost decomposition (line 348-351) is realistic. |
| `alpha-E-candidate-shortlist.md` | §6 C-4 narrative | ACCEPT | 800-1.4k envelope covers codegen template-selection + CSP-to-codegen binding + runtime path + tests/telemetry; the wave-scoping mitigation (one CSP shape at a time, starting from W11.1 numeric-array dispatch) keeps cost bounded. |
| `alpha-E-candidate-shortlist.md` | §7 C-5 narrative | ACCEPT | 250-500 LOC delete-heavy + ledger + REDRESS entries is right-sized; the ≈ 5-8k generated LOC drop is correctly accounted under `[generated-size-budget]`. |
| `alpha-E-candidate-shortlist.md` | §8 pre-blocked routes | ACCEPT | P-1 … P-7 inherit the cost prevention rule from `SYNTHESIS.md §0.4`. |
| `alpha-E-candidate-shortlist.md` | §9 concurrency matrix | REVISE (V3) | Land order `C-5 + C-2 + C-3 in parallel → C-1 sub-waves → C-4` is sound; the matrix permits C-1 JSON sub-waves to parallelise C-4 (line 603), but C-4 §6 narrative names C-1 as a "must serialise after" dependency. Internal inconsistency. |
| `alpha-E-candidate-shortlist.md` | §10 cost / caps / telemetry | **REJECT (R3)** | The 45-min redress cap for C-1 and C-3 routes the addendum's decision-engine-fold cap eligibility outside the bound surface. Per `USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:129-134`, the 30 → 45 min uplift applies to "the W5–W9 fold and W12 union-SIMD wave" — C-1 (Lock-14 refactor) and C-3 (regen-css pipeline) are NOT decision-engine waves; only C-4 (W8 + W9 wiring) qualifies under the amendment. C-1 and C-3 default back to 30-min redress. |
| `alpha-E-candidate-shortlist.md` | §11 convergence + escalation | ACCEPT | Escalation cost paths are cleanly named (no strict Skipper → user escalation; round-trip failure → architectural-block; hot-leaf unchanged → REJECT + abrogate). |
| `DISPATCH-CONTEXT.md` | full | ACCEPT | The 45-min hard cap on α-agents (line 75) is the right α-pass cap; α-E correctly inherits it. |

## §2 — Critical findings

### REJECT R1 — SYNTHESIS §3 risk-class under-statement (`SYNTHESIS.md:241,243,244`)

The α-E candidate table marks C-1 "very high", C-2 "high", C-3 "High",
C-4 "Very high" (`alpha-E-candidate-shortlist.md:83-86, 179, 259, 353,
456`). The SYNTHESIS §3 condensation reads:

> | **C-1** | … | HIGH (architectural; multi-wave) |
> | **C-2** | … | MED (harness-local) |
> | **C-3** | … | MED (xtask + corpora) |
> | **C-4** | … | MED (wires existing scaffold) |

(`SYNTHESIS.md:241-244`).

The risk classification mismatches the α-E narrative on four of five
candidates. C-1 the narrative justifies "very high" because the refactor
touches generic codegen, generic runtime, public `bbnf` API surface,
decision-engine rule routing, and 8 grammars; "HIGH" loses the multi-
surface dimension. C-2 the narrative justifies "high" because the
sonic-rs API surface may not expose Skipper; "MED" elides the
architectural-block risk α-B §316 flags. C-3 the narrative justifies
"High" because the 15 `.bbnf` files may not compose into a single root
grammar; "MED" loses the grammar-composition risk. C-4 the narrative
justifies "very high" because the CSP solver may select a shape whose
runtime path is unimplemented; "MED (wires existing scaffold)" is
materially misleading — C-4 is the MOST architecturally risky candidate
per the α-E narrative (line 456-465).

**Remedy:** SYNTHESIS §3 candidate table risk column must carry the α-E
narrative classification: C-1 VERY HIGH, C-2 HIGH, C-3 HIGH, C-4 VERY
HIGH, C-5 MED-LOW (delete-heavy + audit-trail risk). The CHALLENGE-
CONTEXT §CH-4 check-list (line 137-141) endorsed the SYNTHESIS
classification — that endorsement contradicts the α-E narrative and is
itself REVISE per CH1 reconciliation.

### REJECT R2 — LOC envelope absent in SYNTHESIS §3 / §0.5 (`SYNTHESIS.md:136-145, 233-250`)

α-E §2 binds per-candidate LOC envelopes (line 81-92): C-1 2.1k-3.4k;
C-2 600-1.0k; C-3 1.2k-2.0k; C-4 800-1.4k; C-5 250-500. The total
envelope 4.95k-8.3k is the COST surface S-P3 consumes when authoring the
wave manifest. SYNTHESIS §3 elides every envelope, replacing the column
with a free-form prose description. SYNTHESIS §0.5 defers the entire
§4.4 wave-by-wave layer to S-P3 (which is contracted per `PASS-ALPHA.md
§4.4`), but the §3 candidate table is NOT contracted to defer LOC — it
is α-F's load-bearing artefact for downstream consumption.

The omission is critical because S-P3 cannot enforce `[generated-size-
budget]` discipline without per-candidate LOC ceilings carried forward.
The α-E envelope is the only authoritative source; SYNTHESIS must
reproduce it.

**Remedy:** SYNTHESIS §3 table gains a `LOC budget` column. Values
copied verbatim from `alpha-E-candidate-shortlist.md:83-87`. Optionally,
SYNTHESIS §0 carries a summary cost envelope ≈ 4.95k-8.3k as the
SK-V14 PRUNE-phase ceiling.

### REJECT R3 — α-E §10 hard-cap miscategorisation (`alpha-E-candidate-shortlist.md:617-626`)

The α-E §10 cap table reads:

> | C-1 sub-waves (8 grammars) | 20 min | 15 min | **45 min** |
> | C-2                        | 20 min | 15 min | 30 min |
> | C-3                        | 20 min | 15 min | **45 min** |
> | C-4                        | 20 min | 15 min | 45 min |
> | C-5                        | 20 min | 15 min | 30 min |

The addendum's hard-cap amendment (`USER-PIN-ADDENDUM-2026-05-21-FULL-
SOTA.md:129-134`) raises the redress cap from 30 → 45 min ONLY for the
W5-W9 decision-engine fold and the W12 union-SIMD wave. The
`ORCHESTRATOR-PROMPT.md:177-178` restatement is verbatim: "45 min
redress for decision-engine wiring waves". C-1 is the Lock-14 refactor
cluster, not a decision-engine wave; C-3 is the regen-css pipeline, not
a decision-engine wave. Only C-4 (W8 per-grammar policy + W9 same-
substrate union wiring) qualifies.

α-E §10 silently widens the 45-min eligibility to two candidates that
the addendum does not cover. This is a COST inflation that the CH3
REGRESSION lens may also catch (re-opening an addendum-closed
parameter), but the COST framing is CH4-primary: cap discipline must
inherit from the addendum verbatim, not be redefined at α-E.

**Remedy:** α-E §10 cap table revise to C-1 30 min, C-3 30 min, C-4
45 min, C-2 30 min, C-5 30 min. If C-1 or C-3 sub-waves require 45 min
in practice, the wave must request the amendment via user re-pin per
ORCHESTRATOR §ESCALATE — not assume eligibility at α-E.

### Same-wave consumer audit (ACCEPT with note)

The same-wave consumer rule per `[execute-planned-architecture]` and
`ORCHESTRATOR.md §8` non-negotiables is correctly bound in SYNTHESIS §4
(line 263-265):

> support-only landings are invalid; every primitive lands WITH its
> hot-path consumer in the same commit per `[execute-planned-architecture]`

Per α-E:

- C-1: same-wave consumer is "regen-derived runtime for every grammar
  emitted in the same waves; gate run before commit"
  (`alpha-E-candidate-shortlist.md:83, 145-152`). ACCEPT.
- C-2: same-wave consumer is "bench harness consumes the rebound
  comparators on every named JSON row; `xtask gate-json` enforces the
  schema" (line 84, 234-240). ACCEPT.
- C-3: same-wave consumer is "runtime regenerated from the 15 `.bbnf`
  files in the same wave; bench rows wired to the new corpora" (line
  85, 319-327). ACCEPT.
- C-4: same-wave consumer is "CSP-selected shape produces measurable
  runtime divergence on at least one named pre-wave row in the same
  wave" (line 86, 424-435). ACCEPT — and this is the critical anti-
  scaffold gate per P-5 + CH7-5.
- C-5: same-wave consumer is "REDRESS per row cites the validation
  §reference; ROLLING-SOTA-DELTA rebases to the audit-zero baseline in
  the same commit set" (line 87, 529-536). ACCEPT — revert + ledger
  consumer.

The COST discipline of same-wave consumption is encoded everywhere
except in SYNTHESIS §3 (the table omits the column — see V4). The
absence is presentational, not architectural: the underlying α-E
narrative carries the consumer plans, and SYNTHESIS §4 binds the rule.
But for downstream S-P3 readability, the SYNTHESIS table should re-state
the consumer column.

## §3 — Recommended folds for V2

If V1 does not converge ≥95% ACCEPT (the 71% rate above suggests it
will not), the V2 redispatch should fold:

1. **α-F (SYNTHESIS + HANDOFF) revise** — primary owner of R1, R2, V4,
   V5, V7. Reauthor §3 candidate table with risk class verbatim from
   α-E §3-7, LOC budget verbatim from α-E §2, same-wave consumer column
   added. Reauthor §4 S-P3 constraints to include per-wave LOC ceiling
   inheritance from α-E §2.
2. **α-E revise** — primary owner of R3, V1, V2, V3. Revise §10 cap
   table to default C-1 / C-3 / C-5 / C-2 to 30 min redress, leaving
   only C-4 at 45 min per the addendum. Reconcile C-1 LOC lower bound
   (raise to ≈ 2.8k to reflect 64-file refactor reality; the delete-
   heavy framing is true for net source LOC but not for refactor effort
   — `[generated-size-budget]` is a separate axis). Expand C-2 envelope
   ceiling by ≈ 80 LOC to cover in-tree Skipper fallback per α-B §316.
   Resolve §9 vs §6 dependency-matrix conflict for C-1 ↔ C-4
   parallelisation.
3. **α-A revise** — owner of V6. Add explicit LOC budget for c/B column
   addition; route through C-2's harness scope (the comparator rebind
   wave touches `report.rs` and can include c/B in the same commit).
4. **No source-side fold required** — every REJECT and REVISE is doc-
   surface; no code change is implicated.

The V2 redispatch should preserve the §3 candidate slate (C-1 through
C-5 are the right interventions); only the cost-surface metadata
requires correction. CH4 issues no escalation flag.

## §4 — Escalation flag

NONE. The cost surface is correctable at α-F + α-E V2; no
architectural-block proof emerges from the COST lens. The 45-min cap
overreach (R3) is the closest the lens comes to user re-pin territory,
and the remedy (default to 30 min unless the wave qualifies under the
addendum) keeps the parameter inside the addendum's stated envelope.
