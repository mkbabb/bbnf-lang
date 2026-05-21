# Handoff SK-V13

Date: 2026-05-21.

Status: Alpha-F contract handoff. Do not dispatch SK-V13 Wave 0 from this file
alone. `SPEC.md` and `DISPATCH-PROMPT.md` are intentionally absent and must be
authored downstream by S-P3 after the required G-Omega pre-W0 gate.

## 1. Authority List

Read in this order:

1. `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`
2. `restart/prompts/pass-contracts/PASS-ALPHA.md`
3. `restart/skinny/tranches/sk-v13/SYNTHESIS.md`
4. `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md`
5. `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
6. `restart/skinny/tranches/sk-v12/HANDOFF.md`
7. `skinny/RESULTS.md`
8. `skinny/REDRESS.md` through REDRESS-127
9. `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md`
10. `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md`
11. `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-pass-framework-leverage.md`
12. `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md`
13. `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md`
14. `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md`

The binding addendum controls conflicts. It requires full CSS L4 parity, every
JSON row/plane above sonic-rs strict including `parse_only`, indefinite
SK-V13-to-SK-V14+ continuation until full ADMIT or architectural-block proof,
and G-Omega closure before W0.

## 2. Current State

SK-V12 closed PASS-ADMIT on one CSS row:
`css_l4/declaration_values/direct_to_struct/main` on
`css_l4_declaration_value_fact_stream`, Track 1
`429.34420791225705 Mbps`, lightningcss `168.92962215656692 Mbps`, strict
equality pass, REDRESS-125/127. That row remains admitted but is not enough for
SK-V13 close.

SK-V13 opens with these obligations:

| Goal | Obligation |
|---|---|
| G1 | 24 CSS parity target, 1 admitted, 23 remaining |
| G2 | decision-engine fold: bbnf-regex, e-graph, active cost, CSP, cascade retirement |
| G3 | at least one union variant admitted or architectural-blocked |
| G4 | zero aarch64 orphans; W4 production split wired or rejected |
| G5 | 51 JSON rows above sonic-rs strict or architecturally blocked |
| G6 | totality V1.1 ratified through G-Omega before W0 |
| G7 | no silent demotion; bracket forward if any pinned target remains open |

## 3. G-Omega Block

G-Omega is mandatory before SK-V13 Wave 0. The Omega cycle must converge and
the user must close G-Omega before any implementation wave edits source,
generated runtime, gate/report code, `skinny/RESULTS.md`, or `skinny/REDRESS.md`.

Required Omega fold surface:

- SK-V12 CSS L4 strict lightningcss admission into BENCH/COMPILER/ARCHITECTURE/HANDOFF;
- REDRESS-119 direct fixpoint history into canonical bench rules, now superseded
  by the full-SOTA addendum;
- REDRESS-120 SK-V11 close history;
- REDRESS-121 Lock 14 GrammarConfig evidence and per-wave gate language;
- REDRESS-122 Lock 16 escape-mask prerequisite;
- REDRESS-123 through REDRESS-127 SK-V12 CSS, comparator, report, zero-orphan,
  and close evidence;
- non-JSON telemetry schema and rolling SOTA delta requirement;
- zero-orphan and same-wave-consumer discipline.

If G-Omega returns revise, S-P3 may continue doc planning only where it does not
contradict the requested revisions. Wave 0 remains blocked.

## 4. Concurrency Model

Allowed before G-Omega:

- SK-V13 S-P1, S-P2, and S-P3 research/planning work in
  `restart/skinny/tranches/sk-v13/`;
- Omega agents and CHALLENGE work in totality audit paths;
- read-only inspection of `skinny/RESULTS.md` and `skinny/REDRESS.md`.

Blocked before G-Omega:

- implementation Wave 0 and later waves;
- source changes under `skinny/crates/*`;
- generated runtime changes;
- gate/report code changes;
- appends or edits to `skinny/RESULTS.md` or `skinny/REDRESS.md`.

After G-Omega and S-P3 convergence, independent waves may run concurrently when
their file domains do not overlap:

- CSS expansion: `crates/runtime/src/grammars/css_l4_*` and
  `crates/codegen/src/css_*`;
- JSON row reopening: JSON runtime consumers, generated JSON templates, and
  `bbnf-simd` consumers;
- union substrate: shared substrate/runtime/codegen paths;
- decision-engine fold: `ir/`, `passes/`, codegen lowerer paths;
- SIMD/ASM production split: `bbnf-simd`, narrow CSS consumer, and Lock 16
  evidence paths.

Use worktrees for truly parallel source waves or any overlap risk. Redress
phases that append `skinny/RESULTS.md` or `skinny/REDRESS.md` must serialize.

## 5. Pass Sequence

1. Alpha-F contract draft: this `HANDOFF.md` and `SYNTHESIS.md`.
2. Alpha CHALLENGE: CH1-CH6 review the contract for correctness, generality,
   regression, cost, hidden coupling, and next-tranche impact.
3. G-Alpha: the user's SK-V13 dispatch and addendum pin the contract for
   planning; no separate stop occurs before G-Omega.
4. Omega cycle: Ω-A through Ω-F, CHALLENGE, consolidated verdict, and user
   G-Omega sign-off.
5. Totality CRUD: apply G-Omega-approved V1.1 surface amendments.
6. S-P1 Profile: fresh CSS and JSON profile truth; stale SK-V12 profiles are
   not enough.
7. S-P2 Research: candidate research for CSS parity, JSON rows, decision
   engine, union, SIMD/ASM, and telemetry gates.
8. S-P3 Synthesis-Plan: author `SPEC.md` and `DISPATCH-PROMPT.md` from G1-G7.
9. SK-V13 Wave 0 and later implementation waves: only after G-Omega and S-P3
   convergence.
10. Close or bracket: if G1-G7 are not fully admitted or architecturally
    blocked, Pass Alpha brackets SK-V14 immediately.

## 6. Immediate Next Steps

1. Run Alpha CHALLENGE against `restart/skinny/tranches/sk-v13/SYNTHESIS.md`
   and this handoff.
2. Dispatch Omega and hold Wave 0 behind G-Omega.
3. After G-Omega, have S-P1 refresh profile truth for CSS L4 and all JSON
   planes, including hot leaves and strict comparator rows.
4. Have S-P3 create the concrete wave plan, including rolling
   `restart/skinny/ROLLING-SOTA-DELTA.md` production and gate consumption.

## 7. Refusal Conditions

Return REVISE for any downstream plan that:

- dispatches W0 before G-Omega;
- omits `parse_only` from JSON SOTA admission;
- counts lossy/permissive comparators as SOTA;
- treats the SK-V12 CSS declaration-values row as full CSS parity;
- leaves any CSS feature `PARTIAL` at close;
- adds support-only primitives, union substrates, resolver infrastructure, or
  codegen paths without a same-wave measured consumer;
- inherits weaker scoping labels (`optional`, `fallback`, `diagnostic`,
  `support-only`, `scaffold-only`, or `future-tranche`) for pinned CSS/JSON/G2-G7
  work instead of converting them to admitted row targets, architectural-block
  proofs, or user re-pin issues;
- authorizes a new directive, BIR variant, `BackendShape`, public substrate API,
  or grammar-specific generic behavior through SPEC-local wording;
- wires `bbnf-simd` into CSS, union, JSON `parse_only`, or shared generated
  code without `G-SIMD-GRAMMAR-POLICY` proving the consuming grammar's
  quote/escape/control policy or a no-string policy, plus scalar parity,
  checkasm/differential coverage, same-wave row measurement, no public substrate
  API, and no sidecar classifier state;
- lets the hardcoded P1-P8 cascade silently serve JSON/CSS/Sheets/BBNF-self rows
  after the resolver fold rather than failing closed with visible
  rejection/non-admission;
- allows source/gate edits without telemetry and rolling delta updates;
- closes a tranche with implementation-limited misses instead of full ADMIT,
  architectural-block proof, or immediate bracket to the next tranche.
