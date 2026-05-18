# SK-V8 S-P3 Hardening V5 CH2 GENERALITY

Verdict: ACCEPT.

Confidence: 96.

## Scope

Reviewed the unchanged V4-folded S-P3 packet for the second consecutive
challenge cycle after V4 ACCEPT. Lens: CH2 generality, Lock 14, grammar
neutrality, no JSON policy leakage, no new directive/BIR/substrate/API,
no `BackendShape`, no `UnionTape`, and no Tier B work smuggled into W3 Tier A.
This review does not dispatch or implement any SK-V8 wave.

## Blockers

None.

## Evidence

- The governing CH2 lens requires Lock 14, no grammar-name leak, and
  grammar-neutral applicability to CSS L4, Sheets, and BBNF-self, not only JSON
  (`restart/prompts/ORCHESTRATOR.md:74-88`). The same governance block keeps
  convergence at two consecutive >=95% ACCEPT cycles with zero open critical
  defects (`restart/prompts/ORCHESTRATOR.md:118-125`) and names no new
  directives, no new BIR, no new substrate, and no JSON code in generic crates
  as non-negotiables (`restart/prompts/ORCHESTRATOR.md:197-205`).
- The S-P3 prompt keeps S-P3 read-only against `skinny/` source
  (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:44-46`) and specializes
  CH2 to the S-P2 grammar-neutral verdict plus a SPEC Section 2.1 gate requiring
  CSS L4, Sheets, and BBNF-self proof for generic-crate edits
  (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:116-120`). It also requires
  W0 baseline/telemetry before behavior waves
  (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:245-248`).
- The live SPEC preserves the dispatch lock: no implementation from S-P3,
  G-Alpha user signoff required, W0 only on G-Alpha close, and W1-W6 blocked
  until W0 plus exact per-wave gates
  (`restart/skinny/tranches/sk-v8/SPEC.md:29-37`). Its non-negotiables forbid
  new directives, BIR variants, `BackendShape`, `UnionTape`, new substrate
  surfaces, public substrate APIs, parser-owned structural facts, sidecars,
  generic JSON policy, and consumer-later primitives
  (`restart/skinny/tranches/sk-v8/SPEC.md:230-251`).
- SPEC Section 2.1 is grammar-neutral and operational: public API scan, grammar
  branch scan, primitive/table scan, role/fact boundary, template/provider
  boundary, and non-JSON proof for CSS L4, Sheets, and BBNF-self are explicit
  exit gates for generic edits (`restart/skinny/tranches/sk-v8/SPEC.md:300-325`).
- W2 remains limited to generated typed product rows from explicit host/API
  schema facts and independent Track 2/oracle proof. It blocks hidden schema
  facts, benchmark-private parsers, and generic JSON schema facts
  (`restart/skinny/tranches/sk-v8/SPEC.md:442-504`).
- W3 remains Tier A only: structural-class cursor migration inside one retained
  `Tape`, generated JSON retained parser as the production consumer, no old
  offset append path, no parser-owned cursor/fact slots, and no Tier B
  string-boundary/parity/CostFacts-template closure
  (`restart/skinny/tranches/sk-v8/SPEC.md:506-586`). The W3 pre-block list
  explicitly rejects new directive, BIR, `BackendShape`, `UnionTape`, public
  substrate API, sidecar, parser-owned slots, Tier B work, and `tape_vs_tape` as
  consumer (`restart/skinny/tranches/sk-v8/SPEC.md:588-599`).
- W5 closes CH2 directly: it audits generic crates for JSON policy and renamed
  JSON residue, requires CSS L4/Sheets/BBNF-self proof, and blocks generic JSON
  public APIs, grammar-name branches, `StructuralAlphabet::json`, renamed JSON
  helpers, and performance claims from cleanup
  (`restart/skinny/tranches/sk-v8/SPEC.md:663-708`).
- DISPATCH matches the SPEC: W1-W6 are not dispatchable from the prompt alone,
  generic-crate edits must include the Lock 14 Section 2.1 gate, W3 blocks Tier
  B, `tape_vs_tape`, sidecars, new substrate, `UnionTape`, `BackendShape`,
  directive, BIR, and public substrate API, and W5 blocks generic JSON public
  APIs and branches (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:90-107`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:127-140`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:150-155`).
- HANDOFF keeps the S-P2 lead narrow: Tier A is a single-`Tape` structural-class
  cursor migration, Tier B is separate, `tape_vs_tape` is telemetry only, and
  W3 remains blocked on W0/W1 closure, fresh plan owner paths, same-wave
  production consumer, revert protocol, thresholds, measured-path proof, and
  challenge acceptance (`restart/skinny/tranches/sk-v8/HANDOFF.md:56-96`).
  Generic edits require public API, grammar branch, primitive/table,
  template/provider, and CSS L4/Sheets/BBNF-self proof
  (`restart/skinny/tranches/sk-v8/HANDOFF.md:119-137`).
- The V4 fold was traceability-only and explicitly preserved the G-Alpha/W0
  dispatch lock, strict-vs-strict comparator discipline, Lock 14, non-JSON
  proof obligations, no-new-surface constraints, W3 Tier A/Tier B split,
  same-wave consumer requirement, `tape_vs_tape` demotion, and 90-minute caps
  (`restart/skinny/tranches/sk-v8/research/p3/p3-v4-exact-traceability-fold.md:28-39`).
- V4 consolidated is a qualifying ACCEPT cycle, with CH2 ACCEPT at 96 and no
  required fold (`restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V4-CONSOLIDATED.md:9-20`).
  Its disposition records no grammar-neutrality regression and keeps Lock 14,
  non-JSON proof, no new directive/BIR/substrate/API, no `BackendShape`, no
  `UnionTape`, and no Tier B smuggling binding
  (`restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V4-CONSOLIDATED.md:24-29`).
  It also requires this V5 cycle to review the unchanged V4-folded packet
  (`restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V4-CONSOLIDATED.md:31-39`).
- P3-A through P3-F remain aligned with the live SPEC. P3-A keeps W3 Tier A out
  of Tier B and blocks `UnionTape`, sixth `BackendShape`, new BIR/directive,
  and generic JSON branches
  (`restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:24-29`,
  `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:52-63`).
  P3-B sequences W0 first, blocks W3 until W0/W1/challenge, and states W5's
  Lock 14 proof requirements
  (`restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:20-30`,
  `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:70-82`).
  P3-C rejects generic JSON leaks, new directive/BIR/substrate/UnionTape/
  BackendShape/public API, and Tier B inside W3
  (`restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:24-32`,
  `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:191-199`).
  P3-D states its additions are telemetry-only and introduce no directive, BIR,
  public substrate type, or sixth `BackendShape`
  (`restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:92-103`).
  P3-E globally blocks new directive/BIR/`BackendShape`/`UnionTape`/public
  substrate API and generic JSON policy leakage
  (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:36-49`).
  P3-F preserves the dispatch lock, W3 Tier A boundary, Lock 14, no-new-surface
  constraints, and same-wave consumer gates
  (`restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:13-38`,
  `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:50-66`).

## Residual Non-Blocking Risks

- P3-D still uses the telemetry value `substrate_surface=retained_union_tape`.
  This is awkward wording, but it remains non-blocking because P3-D limits those
  additions to telemetry only and the live SPEC/DISPATCH explicitly forbid a
  public `UnionTape`, new substrate surface, new `BackendShape`, public
  substrate API, sidecar, or Tier B promotion.
- This CH2 V5 ACCEPT is one lens result. S-P3 convergence still requires the
  full V5 challenge/consolidation to qualify under ORCHESTRATOR Section 3Z; CH2
  alone does not authorize G-Alpha close or any implementation wave.

## Required Fold If REVISE

None. No new critical defect was found, so no V5 fold is proposed.
