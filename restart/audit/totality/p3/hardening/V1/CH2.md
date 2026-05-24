---
lens: CH2
name: GENERALITY / LOCK 14
pass: T-P3-synthesis
cycle: V1
generated_at: 2026-05-23T23:30:00-04:00
disposition: REVISE
scope: "CH2 generality and Lock 14 only"
artifacts_audited:
  - restart/prompts/totality/PASS-3-SYNTHESIS.md
  - restart/audit/totality/p3/3A-architecture-synthesis.md
  - restart/audit/totality/p3/3B-master-plan-reconciliation.md
  - restart/audit/totality/p3/3C-locks-crystallisation.md
  - restart/audit/totality/p3/3C-locks-v+1-diff.md
  - restart/audit/totality/p3/3D-skinny-fold.md
  - restart/audit/totality/p3/3E-grammar-generalisation.md
  - restart/audit/totality/p3/3F-migration-handoff.md
  - restart/audit/totality/p3/hardening/V1/CHALLENGE-CONTEXT.md
---

# T-P3 V1 CH2 Generality / Lock 14

## Lens Contract

PASS-3 §3 CH2 GENERALITY (`restart/prompts/totality/PASS-3-SYNTHESIS.md:108`-`111`)
requires Lock 14 to hold across 3A surface deltas, 3B wave reconciliation, and
3E grammar-generalisation; 3E concrete for CSS L4 / Sheets / BBNF-self; 3C
accepts no JSON-narrowing amendment; the future-grammar onboarding test
survives. PASS-3 §8.1 + §8.2 bind the 16-lock count and the 5-shape
`BackendShape` canon as invariants every artefact must preserve
(`restart/prompts/totality/PASS-3-SYNTHESIS.md:210`-`211`). The V1 dispatch
context narrows the lens to four CH2-binding focuses
(`restart/audit/totality/p3/hardening/V1/CHALLENGE-CONTEXT.md:22`-`23`,
`:28`-`:30`):

1. Lock 14 holds across 3A + 3B + 3E deltas.
2. 3E concrete for CSS L4 (15 sub-grammars per 2C V4) / Sheets / BBNF-self.
3. 3C accepts NO JSON-narrowing amendment across all 38 ACCEPT + 13 MODIFY
   dispositions (zero JSON-only narrowing language).
4. 3E future-grammar onboarding 7-step survives intact.
5. LAC-1E-14 FactStream lands as 5th SUBSTRATE category (NOT 6th
   `BackendShape`) — 5-shape canon per §8.2 preserved.

## Verdict

**REVISE.**

The cohort discharges 4 of 5 CH2 focuses cleanly: 3A/3B/3C/3D/3E all preserve
the 5-shape `BackendShape` canon, ratify FactStream as a Lock-1 substrate
category orthogonal to Lock 10 search domain, accept no JSON-narrowing
amendment in any of the 51 dispositions, and carry the 7-step CSS L4 / Sheets
/ BBNF-self future-grammar onboarding test intact. The single CH2 fault is
isolated to 3F: **MIG-004** (table row and proposed-text §4) calls
LAC-1E-14 FactStream a "5th BackendShape variant (gates Lock 1 + Lock 10 v+1)",
which directly contradicts the V1 dispatch binding, the 3C V4 disposition
matrix, the 3C v+1 diff hunk V4-3 text, and the 3A/3B/3D/3E coherence matrix.
This is a CH2 §8.2 coherence break — a single artefact misclassifies the
LAC-1E-14 carrier in a way that, if Pass Omega CRUD consumed verbatim, would
expand the `BackendShape` enum to six variants and re-open Lock 10 search
domain. The remaining cohort is correct; the repair is doc-only in 3F and
gated by re-stating the LAC-1E-14 carrier per 3C V4 hunk V4-3 wording.

## Evidence

| check | disposition | evidence |
|---|---|---|
| **F1: 5-shape canon preserved across 3A/3B/3D/3E** | ACCEPT | 3A executive summary explicitly preserves "the 5-shape canon, the substrate-union fence, and the no-new-directive/no-new-BIR/no-new-substrate gate per §8 PASS-3-SYNTHESIS" (`restart/audit/totality/p3/3A-architecture-synthesis.md:23`-`27`); ARCH-3A-D03 wording "preserve the 5-shape enum" (`:35`); ARCH-3A-D07 wording "NOT a sixth BackendShape" (`:39`). 3B coherence matrix row 1 binds "5-shape canon unchanged; FactStream is substrate-target classification, not 6th BackendShape" (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:179`). 3D row "CSS fact streams = output planes, not 6th shape" (`restart/audit/totality/p3/3D-skinny-fold.md:174`). 3E "keep the five `BackendShape` variants" (`restart/audit/totality/p3/3E-grammar-generalisation.md:30`-`31`); L14-HC-07 "do not create a sixth `BackendShape`" (`:208`). |
| **F2: LAC-1E-14 lands as Lock 1 substrate category, NOT Lock 10 BackendShape** | ACCEPT (3C) / REVISE (3F) | 3C V4 hunk V4-3 text is unambiguous: "5th admitted-product category at the Lock 1 substrate manifest... NOT a 6th `BackendShape` variant. The 5-shape `BackendShape` search domain at Lock 10 holds" (`restart/audit/totality/p3/3C-locks-v+1-diff.md:124`-`133`); 3C disposition matrix row "Carrier note per PASS-3 §8.1: this is a 5th *substrate* category at the Lock 1 manifest level, NOT a 6th `BackendShape` variant" (`restart/audit/totality/p3/3C-locks-crystallisation.md:119`). 3C Executive Summary confirms "16-lock count holds; LAC-1E-12 lands as a preface CH7-binding clause, not Lock 17" + "diff... adds no directive, BIR variant, `BackendShape` variant, public substrate API, or new lock" (`restart/audit/totality/p3/3C-locks-crystallisation.md:23`). **CONTRADICTION:** 3F-MIG-004 says "LAC-1E-14 proposes `FactStream` as 5th BackendShape variant — but this is G-Omega-gated per Lock 1 v+1 since it touches Lock 10 BackendShape canon too" (`restart/audit/totality/p3/3F-migration-handoff.md:104`); 3F text §4 repeats "LAC-1E-14 proposes `FactStream` as 5th BackendShape variant (gates Lock 1 + Lock 10 v+1)" (`:125`); 3F open question CH2 repeats the same misclassification (`:311`). |
| **F3: 3C accepts no JSON-narrowing amendment** | ACCEPT | 3C V4 routes 51 candidates with 38 ACCEPT + 13 MODIFY + 0 REJECT + 0 DEFER (`restart/audit/totality/p3/3C-locks-crystallisation.md:54`-`59`); silent-drop census is ZERO (`:186`). Cohort-wide `BBNF_SIMD_STRICT=1` precondition (3C-L10-bbnf-simd-strict-cohort) institutionalised at three sites with mutual cross-references; regex/HIR fact mandate applies cohort-wide (`:40`). Lock 14 hunks V4-7 (Pattern H census + `byte_class_from_range_64` sibling) + V4-3 (FactStream 5th substrate) + V4-4 (regen round-trip across JSON / Sheets / BBNF / EBNF / BNF / CSV / Math) explicitly extend non-JSON discipline; no hunk narrows a lock to JSON (`restart/audit/totality/p3/3C-locks-v+1-diff.md:158`-`164`, `:250`-`:264`). Zero JSON-only narrowing strings in the diff: `grep -nE "JSON-only\|json only\|JSON-narrow"` returns empty. The Lock 14 fence at the diff's Lock 14 v+1 generated-output allowance permits generated files under `runtime/src/grammars/<name>/` only when produced by rostered generator (`restart/audit/totality/p3/3C-locks-v+1-diff.md:170`-`178`), with no JSON narrowing. |
| **F4: 3E concrete for CSS L4 (15 sub-grammars) / Sheets / BBNF-self** | ACCEPT | 3E V4 broadens the matrix to 5 shapes × 15 CSS L4 sub-grammars (`restart/audit/totality/p3/3E-grammar-generalisation.md:91`-`128`) covering color/easing/filters/func-body/gradients/keyframes/keywords/media/properties/selectors/stylesheet/tokens/transforms/value-unit/values per 2C V4 §Executive Summary. The Other-Grammars matrix covers Sheets formulas / functions / arrays / infix and BBNF-self grammar / expression / directive routes (`:137`-`:149`). Primitive vocabulary transfer table maps every primitive family (byte-set classify, byte-range classify, string/escape, digit/number, direct/fact sink, regex/HIR facts, BackendShape resolver, cross-chunk byte-context, SIMD/ASM) across CSS L4 / Sheets / BBNF-self (`:153`-`:163`). |
| **F5: 7-step onboarding test survives intact** | ACCEPT | 3E §"Future-Grammar Onboarding Test (per 2C V4 7-step protocol)" reproduces all 7 steps verbatim (`restart/audit/totality/p3/3E-grammar-generalisation.md:165`-`190`): (1) grammar-source + metadata only; (2) regenerate rostered surfaces; (3) grammar-name leak scan (executable `rg` command); (4) grammar-shape leak scan (LAC-2C-02 census); (5) five-shape eligibility fixture; (6) primitive same-wave non-JSON consumer; (7) telemetry/provenance consumed by gate. Fail-closed rule preserved: "if onboarding requires a new directive, BIR variant, `BackendShape`, public substrate API, retained sidecar, or hand-coded generic behavior" (`:187`-`:190`). CSS L4 (15 sub-grammars, full step 1-7 cycle) is mandatory for SK-V14 per 2C V4 (`:192`-`:194`). |
| **F6: Lock 14 v+1 holds across 3A + 3B + 3E surface deltas** | ACCEPT | 3A: ARCH-3A-D08 enumerates Pattern H = 67 hand-written runtime files across 9 grammars, 0/9 carry `@generated` markers (`restart/audit/totality/p3/3A-architecture-synthesis.md:40`); ARCH-3A-D09 enumerates the 4 leak classes (8-variant `RuntimeProvider` enum + 8 per-grammar provider modules + 30 grammar-named symbols across 15 files + 127 grammar-named reexports) and binds the Lock 14 zero-new-`.rs`-files invariant (`:41`). 3B: MP-3B-V1-D03 binds Pattern H per-tranche census; MP-3B-V1-D09 binds Lock 14 v+1 generic-crate forward invariant ("ZERO `match grammar { Json => ..., CssL4 => ... }` arms; ZERO grammar-named modules; ZERO grammar-specific types in public APIs"; `restart/audit/totality/p3/3B-master-plan-reconciliation.md:124`, `:130`). 3E: L14-HC-01..L14-HC-12 enumerate twelve Lock 14 hardening clauses covering generated provider manifest, sink/fact/value/flag ownership, grammar-shape census, primitive policy ownership, CSS + negative control closure, resolver-generated shape facts, fact-stream output-plane carrier, RuntimeProvider 2→8 enum-drift fault, pass-layer JSON-byte/literal leaks, runtime root reexport + parser-name census, primitive policy_owner + FlagSchema + abstract-primitive sibling (`restart/audit/totality/p3/3E-grammar-generalisation.md:200`-`213`). All twelve hardening clauses cite T-P1 / T-P2 finding-ids with executable verification commands. |
| **F7: LAC-2F-V5-02 ELEVATED preserves substrate-union without introducing coupling** | ACCEPT (CH2 scope subset) | 3C V4 elevates LAC-2F-V5-02 to STRONGEST AMENDMENT SURFACE — "no cross-call retained classifier state, period" generalises REDRESS 96/97/98 to ALL transient classifier-state primitives (`restart/audit/totality/p3/3C-locks-crystallisation.md:31`, `:124`; v+1 diff hunk V4-2). This is a CH2-positive: the elevation hardens Lock 1 substrate-union v+1 by forbidding cross-call carry that would otherwise create a hidden retained sidecar — which would in turn imply a 6th substrate plane. The amendment STRENGTHENS the 5-shape canon by closing the substrate-coupling escape valve. (The detailed CH5 hidden-coupling analysis belongs to the CH5 lens; here it suffices that the elevation does not violate CH2 generality.) |
| **F8: CH2 V3 carry-forward continues to hold** | ACCEPT | The V3 CH2 ACCEPT verdict at `restart/audit/totality/p3/hardening/V1/CH2.md` (prior cycle) verified governing T-P1/T-P2 state, T-P2 Lock 14 transfer contract, 3A non-JSON generalisation, 3B planning concretion, 3E matrix concretion for CSS L4 / Sheets / BBNF-self, 7-step onboarding survival, 3C zero JSON-narrowing, generated/provider exception boundedness, CSS row as evidence not closure, and fact-stream-placement deferral to downstream. Every V3-positive check carries forward unchanged in V1: 3A V1 strengthens via D05/D07/D08/D09; 3B V1 sharpens via D03/D06/D09; 3C V4 layers six SK-V14 NEW-LACs without weakening any V3-merged Lock 14 disposition; 3E V4 expands matrix to 5×15 + 6 rows. |

## Findings

### REVISE-CH2-V1-01 — 3F-MIG-004 misclassifies LAC-1E-14 FactStream carrier

**Severity:** REVISE (blocking for V1 ACCEPT-cycle, doc-only repair).

**Defect:** 3F-MIG-004 (`restart/audit/totality/p3/3F-migration-handoff.md:104`)
and the corresponding proposed-text §4 (`:125`) and CH2 open question (`:311`)
classify LAC-1E-14 as proposing `FactStream` as a **5th BackendShape variant**
that **gates Lock 1 + Lock 10 v+1 simultaneously**. The dispatch context
binding, the 3C V4 disposition, the 3C v+1 diff hunk V4-3, the 3A executive
summary, the 3B coherence matrix, the 3D fold rows, and the 3E L14-HC-07
hardening clause all converge on the opposite carrier: LAC-1E-14 lands as the
**5th SUBSTRATE category at the Lock 1 manifest level only**; the 5-shape
`BackendShape` search domain at Lock 10 holds at `{EagerTape, OffsetTape,
EventTape, SinkOnly, CollapsedStage}`; a 6th `BackendShape` variant remains
G-Omega gated per PASS-3 §8.1 + §8.2.

If Pass Omega CRUD reads 3F verbatim, the conflicting carrier risks (a)
expanding the `BackendShape` enum to six variants, (b) re-opening the Lock 10
search-domain canon, (c) violating §8.2 5-shape coherence binding, and (d)
creating a hidden coupling between Lock 1 substrate manifest and Lock 10
search domain that LAC-1E-14 was explicitly authored to avoid. The defect is
purely artefact-internal coherence (3F vs cohort); the underlying LAC-1E-14
carrier is correctly disposed by 3C V4 hunk V4-3.

**Repair (doc-only, 3F V2):** Rewrite 3F-MIG-004 row + proposed-text §4 +
CH2 open question to mirror 3C V4 hunk V4-3 wording. Specifically:

- Replace "LAC-1E-14 proposes `FactStream` as 5th BackendShape variant" with
  "LAC-1E-14 lands `FactStream` as 5th admitted-product category at the Lock 1
  substrate manifest, NOT a 6th `BackendShape` variant; the 5-shape Lock 10
  search domain holds".
- Replace "gates Lock 1 + Lock 10 v+1" with "Lock 1 v+1 substrate-manifest
  amendment only; Lock 10 v+1 search domain unaffected".
- Replace the CH2 open question's framing of "5th variant vs `admitted_fact_output` substrate_target without canon expansion" with the
  resolved disposition: 3C V4 V1 has already disposed LAC-1E-14 as substrate
  category (not BackendShape variant); the remaining open question is
  FactStream cardinality across grammars (CSS-only vs grammar-neutral), which
  matches the 3B CH2 open question at line 190.

**Evidence chain for repair:**

- `restart/audit/totality/p3/hardening/V1/CHALLENGE-CONTEXT.md:30` — V1
  binding: "LAC-1E-14 FactStream as 5th SUBSTRATE category (NOT 6th
  BackendShape) preserves 5-shape canon per §8.2".
- `restart/audit/totality/p3/3C-locks-v+1-diff.md:118`-`140` — hunk V4-3 text:
  "5th admitted-product category at the Lock 1 substrate manifest... NOT a
  6th `BackendShape` variant. The 5-shape `BackendShape` search domain at
  Lock 10 holds".
- `restart/audit/totality/p3/3C-locks-crystallisation.md:119` — disposition
  matrix row for LAC-1E-14: "this is a 5th *substrate* category at the Lock 1
  manifest level, NOT a 6th `BackendShape` variant (the 5-shape canon at Lock
  10 holds)".
- `restart/audit/totality/p3/3B-master-plan-reconciliation.md:179` — coherence
  matrix row 1 already binds the correct carrier; 3F is the only outlier.

### F-CH2-V1-02 — F4/F6 cohort discharges all remaining CH2 obligations

The remaining CH2 focuses (non-JSON generalisation discipline, 5-shape canon
across 3A/3B/3D/3E, 7-step onboarding survival, zero JSON-narrowing
dispositions in 3C, Lock 14 v+1 hardening across 12 L14-HC clauses) are
ACCEPT-discharged per evidence rows F1-F8. No further REVISE warranted.

### F-CH2-V1-03 — CH2 open question (3E §Open Questions, line 277) properly routed

3E V1 §Open Questions row "Should the formal spec name CSS fact streams as
`SinkOnly` products, or as a distinct output-plane taxonomy that does not
expand `BackendShape`?" (`restart/audit/totality/p3/3E-grammar-generalisation.md:277`)
is correctly routed to 3A + 3C with the gate "accepted wording must preserve
five shapes and cite `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:96`-`97`".
The question is itself the answer: distinct output-plane taxonomy preserving
5 BackendShape values — exactly what 3A-D07, 3B-D06, 3C-L01-factstream-fifth-category, 3D-FOLD-3D-001, and 3E-D05 + L14-HC-07 collectively
discharge. CH2 closes this open question by V1 cohort convergence.

## Cycle Disposition

**CH2 disposition for T-P3 V1: REVISE.**

One blocking repair (REVISE-CH2-V1-01: 3F-MIG-004 carrier correction);
remainder of CH2 cohort ACCEPT. The defect is doc-only artefact-internal
coherence — the underlying LAC-1E-14 disposition at 3C V4 hunk V4-3 is
correct, and the 3A/3B/3D/3E cohort is correct. The V2 repair is mechanical:
3F V2 author re-words MIG-004 row + text §4 + CH2 open question to mirror 3C
V4 hunk V4-3 wording, citing PASS-3 §8.1 + §8.2 coherence binding.

ACCEPT-rate for CH2 V1: 7/8 evidence checks ACCEPT (F1, F3, F4, F5, F6, F7,
F8); 1 evidence check REVISE (F2 — the 3F-internal misclassification side of
the LAC-1E-14 carrier). Per PASS-3 §3 cycle-V1 convergence rule (cycle V1
expects ≥30% REVISE; all-ACCEPT is paper-close — `:99`), this REVISE
disposition is the honest CH2 verdict.

## Carry-Forward Constraints (Pass Omega / S-P3)

V3 CH2 carry-forward constraints remain in force, with two V1 additions:

1. **(V3-carried)** Preserve the exact Lock 14 fence from 3C: generated
   grammar names are allowed only as rostered generated output, never as
   hand-coded generic provider or role-policy branches.
2. **(V3-carried)** Do not reduce the negative-control rule below the T-P2/3E
   standard. A fleet-wide generality claim needs CSS L4 plus Sheets or
   BBNF-self witness/negative-control; the single CSS L4 declaration-values
   row remains admitted evidence only.
3. **(V3-carried)** Resolve the provider-manifest layout in the Lock 14
   registry wave by proving JSON, CSS, and a Sheets or BBNF-self provider
   without editing generic code.
4. **(V3-carried, V1-sharpened)** Resolve CSS fact-stream placement as an
   output-plane taxonomy or `SinkOnly` product only if the five-shape canon
   and no-retained-sidecar rule are preserved. LAC-1E-14 V4 disposition
   confirms 5th *substrate* category, not 6th BackendShape — Pass Omega CRUD
   must consume 3C V4 hunk V4-3 wording verbatim and reject any 3F-MIG-004
   wording until V2 repair lands.
5. **(V3-carried)** Keep shared primitive policy caller/generated-owned. JSON
   punctuation, string, number, quote, escape, and no-string/no-number policy
   must not become shared crate constants.
6. **(V1-NEW)** 3E L14-HC-09 RuntimeProvider 2→8 enum-drift fault baseline:
   future grammar additions MUST land via generated manifest + workspace
   metadata, never by editing `skinny/crates/codegen/src/grammar_profile.rs`;
   any wave that grows the hand-coded `RuntimeProvider` variant count without
   recording an emitter-source pair is a Lock 14 v+1 fault.
7. **(V1-NEW)** 3E L14-HC-10 pass-layer JSON-byte/literal leak repair:
   Sheets/BBNF-self onboarding requires BOTH 1B-D8 recognizer-byte plane AND
   1B-D10 materialization-role plane sourced from generated grammar metadata;
   neither alone unblocks. The Lock 14 census MUST scan `passes/src/lib.rs:331`
   + `:1059/1079/1102` + `:1300`-`:1391`.
