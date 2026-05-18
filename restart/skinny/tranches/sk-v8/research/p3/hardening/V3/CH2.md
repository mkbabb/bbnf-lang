# SK-V8 S-P3 Hardening V3 CH2 GENERALITY

Verdict: ACCEPT.

Confidence: 96%.

## Scope

Reviewed ORCHESTRATOR Section 3W/3Z, PASS-3-SYNTHESIS-PLAN, live
`SPEC.md`, `DISPATCH-PROMPT.md`, `HANDOFF.md`, P3-A through P3-F,
`p3-v3-citation-fold.md`, and the V1/V2 consolidated hardening files.
The review question is narrow: whether the V3 citation fold weakened Lock 14,
non-JSON proof, no-new-surface constraints, or grammar-neutrality.

## Blockers

None.

## Evidence

The CH2 standard did not change. ORCHESTRATOR defines CH2 as Lock 14
generality: no grammar-name leak and interventions that work for CSS L4,
Sheets, and BBNF-self, not only JSON
(`restart/prompts/ORCHESTRATOR.md:74-88`). It also keeps no new BBNF directives
and no JSON code in generic crates as non-negotiable CH2 checks
(`restart/prompts/ORCHESTRATOR.md:197-205`). PASS-3 requires SPEC Section 2.1
generality, non-JSON proof for every generic-crate edit, and CH2 failure if any
wave lets JSON policy into a generic crate
(`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:94-120`).

V1 and V2 governance preserve the same boundary. V1 consolidated hardening
accepted the CH2 boundary as non-blocking and preserved Lock 14, non-JSON proof,
W3 Tier A only, one retained `Tape`, no `tape_vs_tape` production consumer, no
sidecar/parser-owned cursor, and no directive/BIR/substrate/`BackendShape`/
`UnionTape`/public substrate API authorization
(`restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:20-27`,
`restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:54-69`).
V2 consolidated hardening returned REVISE only because CH1 citation traceability
remained too coarse; CH2 accepted at 96% with Lock 14, non-JSON proof gates, and
no-new-surface constraints intact
(`restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:13-28`).
The required V3 fold was citation hygiene plus preserving V2 W2 seeds,
dispatch lock, and LOC/time gates unchanged
(`restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:33-43`).

The V3 citation fold is scoped to traceability, not policy. It says V2 CH1
returned REVISE because bare paths replaced stale line numbers, then folds
material inline claims to stable section labels while keeping concrete paths in
Sources (`restart/skinny/tranches/sk-v8/research/p3/p3-v3-citation-fold.md:18-35`).
It explicitly preserves strict-vs-strict discipline, Lock 14 grammar neutrality,
no-new-directive/BIR/substrate/API/`BackendShape`/`UnionTape`, W2 seed bounds,
per-wave LOC/time gates, and the G-Alpha/W0-only dispatch lock
(`restart/skinny/tranches/sk-v8/research/p3/p3-v3-citation-fold.md:37-49`).
P3-F repeats that CH2 through CH6 V2 ACCEPT findings remain binding and that the
citation fold does not change Lock 14 or same-wave-consumer requirements
(`restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:76-82`).

The live SPEC still carries the controlling gates. Section 1 forbids new BBNF
directives, BIR variants, `BackendShape`, `UnionTape`, new substrate surface,
public substrate API, parser-owned cursor/facts, sidecar substrate, and JSON
policy in generic crates (`restart/skinny/tranches/sk-v8/SPEC.md:230-246`).
Section 2.1 requires public API scans, grammar-branch scans, primitive/table
scans, role/fact boundary, template/provider boundary, and CSS L4 / Sheets /
BBNF-self proof for any generic CostFacts, codegen, runtime, SIMD, or
parser-template edit (`restart/skinny/tranches/sk-v8/SPEC.md:300-325`).
The inherited pre-block ledger repeats the global blocks against new
directive/BIR/substrate/`BackendShape`/`UnionTape`/public substrate API and
generic JSON policy (`restart/skinny/tranches/sk-v8/SPEC.md:767-784`).

The live wave gates also remain grammar-neutral. W2 is generated typed product
work from explicit host/API schema facts; it blocks hidden directives and
generic JSON schema facts and requires Lock 14 plus non-JSON proof if generic
code changes (`restart/skinny/tranches/sk-v8/SPEC.md:442-497`). W3 remains one
S-P2 Tier A shape: structural-class cursor migration inside one retained
`Tape`, with opaque class ordinals, generated JSON retained parsing as the
same-wave consumer, and no non-JSON production migration claim
(`restart/skinny/tranches/sk-v8/SPEC.md:506-563`). W3 exits only if one retained
tape survives, old offset append and parser-owned cursor/fact slots are absent,
`tape_vs_tape` is not a production consumer, and Lock 14 plus non-JSON proof
pass (`restart/skinny/tranches/sk-v8/SPEC.md:565-586`). Its pre-block list names
new directive, BIR, `BackendShape`, `UnionTape`, public substrate API, sidecar,
parser-owned fact slots, Tier B, and `tape_vs_tape` as blocked
(`restart/skinny/tranches/sk-v8/SPEC.md:588-599`). W5 remains the explicit
grammar-neutral audit, with CSS L4 / Sheets / BBNF-self implications and blocks
against generic JSON public APIs, grammar-name branches, `StructuralAlphabet::json`,
and renamed JSON helpers (`restart/skinny/tranches/sk-v8/SPEC.md:663-708`).

DISPATCH and HANDOFF did not weaken the boundary. DISPATCH says W1-W6 cannot
dispatch from S-P3 alone, and generic-crate edits must include SPEC Section 2.1
Lock 14 and non-JSON proof when relevant
(`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:90-107`). Its W3 note keeps
the same generated JSON retained parser consumer and blocks Tier B,
`tape_vs_tape` as production consumer, sidecars, old offset append, new
substrate surface, `UnionTape`, new `BackendShape`, directive, BIR, and public
substrate API (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:117-140`).
HANDOFF keeps W3 conditional on W0/W1, exact paths, same-wave consumer,
measurement thresholds, measured-path strict proof, and challenge acceptance
(`restart/skinny/tranches/sk-v8/HANDOFF.md:56-96`), and requires public API,
grammar-branch, primitive/table, template/provider, and CSS L4 / Sheets /
BBNF-self proof for any generic-crate edit
(`restart/skinny/tranches/sk-v8/HANDOFF.md:119-137`).

P3-A through P3-F still carry the same CH2 content after citation relabeling:

| Artifact | CH2 disposition |
|---|---|
| P3-A | ACCEPT. The W3 candidate row pre-blocks `UnionTape`, `BackendShape`, new directive/BIR, parser-owned cursor/facts, and `tape_vs_tape` as consumer; W5 rejects JSON policy in generic crates and requires CSS L4 / Sheets / BBNF-self proof (`restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:27-29`, `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:52-63`). |
| P3-B | ACCEPT. W3 is Tier A only, W5 binds concrete Lock 14 scans and non-JSON proof, and generic JSON/new substrate routes remain blocked (`restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:44-50`, `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:70-82`, `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:84-95`). |
| P3-C | ACCEPT. Global predicates require Lock 14 every wave, forbid new directive/BIR/substrate/`UnionTape`/`BackendShape`, and W3/W5 negative gates reject generic leaks or new surfaces (`restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:24-32`, `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:179-197`, `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:239-266`). |
| P3-D | ACCEPT. Telemetry fields are declared telemetry-only, not BBNF directives, BIR variants, public substrate types, or sixth `BackendShape`; Lock 14 failure states reject generic JSON/corpus/role branches and external interpretation of ordinals (`restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:61-103`, `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:133-162`, `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:179-187`). |
| P3-E | ACCEPT. The pre-block ledger globally blocks new directive/BIR/`BackendShape`/`UnionTape`/public substrate API, generic JSON policy leakage, sidecar substrate, and telemetry-only W3 consumers (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:36-49`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:129-141`). |
| P3-F | ACCEPT. The fold preserves strict-vs-strict, Lock 14, non-JSON proof, no directive/BIR/`BackendShape`/`UnionTape`/public substrate API, no deferrals, and same-wave consumers (`restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:50-66`, `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:76-82`). |

## Residual Non-Blocking Risks

- P3-D's telemetry value spelling `retained_union_tape` is still easy to misread
  as a public `UnionTape` concept, but it is non-blocking here because P3-D marks
  the fields telemetry-only and the live SPEC/DISPATCH explicitly block
  `UnionTape`, new substrate surface, public substrate API, and a sixth
  `BackendShape` (`restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:92-103`,
  `restart/skinny/tranches/sk-v8/SPEC.md:230-246`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:127-140`).
- Stable section labels are less precise than file:line citations, but that is a
  CH1 traceability question. For CH2, the labels did not remove or soften the
  live Lock 14, non-JSON proof, no-new-surface, or W3 gate language.
- V3 CH2 ACCEPT does not dispatch implementation. SPEC and DISPATCH still say no
  SK-V8 implementation wave dispatches from S-P3 and G-Alpha can authorize W0
  only (`restart/skinny/tranches/sk-v8/SPEC.md:29-37`,
  `restart/skinny/tranches/sk-v8/SPEC.md:814-825`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:6-10`).

## Required Fold If REVISE

None. CH2 is ACCEPT for V3; no generality fold is required.
