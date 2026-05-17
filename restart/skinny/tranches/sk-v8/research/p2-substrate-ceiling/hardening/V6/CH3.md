# S-P2 V6 CH3 - Regression

Title: CH3 regression review of the user-authorized exceptional V6 substrate-ceiling packet.

Scope: Current HEAD `f20fbc46 docs(sk-v8-p2-research): authorize exceptional substrate-ceiling V6`, with emphasis on `restart/skinny/tranches/sk-v8/SYNTHESIS.md`, `restart/skinny/tranches/sk-v8/SPEC.md`, `restart/skinny/tranches/sk-v8/HANDOFF.md`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md`, V5 consolidated hardening, V5 CH4, ORCHESTRATOR §3W/§3Z, strict-vs-strict comparator discipline, Lock 14 grammar-neutrality, no new directive/BIR/substrate, no deferrals, and no automatic S-P3 from one V6 ACCEPT.

Verdict: ACCEPT.

Confidence: 96%.

## Blockers

None.

## Disposition Of V5 CH4 Blocker

V5 CH4's blocker was governance, not substrate design: the packet still budgeted a normal post-V5 qualifying cycle even though ORCHESTRATOR and S-P2 cap the pass at V5 (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/V5/CH4.md:13-16`, `restart/prompts/ORCHESTRATOR.md:118-127`, `restart/prompts/skinny/PASS-2-RESEARCH.md:155-162`). The V5 consolidation consequently required escalation and offered only three legal paths: user pin, explicit hard-ceiling override for an exceptional V6, or keeping S-P3 blocked (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V5-CONSOLIDATED.md:64-78`).

Current HEAD takes the explicit override path. `SYNTHESIS.md`, `SPEC.md`, and `HANDOFF.md` now state that the user authorized an exceptional V6 on 2026-05-17 and that V6 does not itself dispatch S-P3 (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:96-102`, `restart/skinny/tranches/sk-v8/SPEC.md:454-460`, `restart/skinny/tranches/sk-v8/HANDOFF.md:71-80`). They also preserve the convergence accounting: a V6 ACCEPT is only the first qualifying cycle after V5 REVISE unless the user pins S-P2 final or explicitly authorizes another over-ceiling cycle (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:191-195`, `restart/skinny/tranches/sk-v8/SPEC.md:454-460`, `restart/skinny/tranches/sk-v8/HANDOFF.md:77-80`). That closes the V5 CH4 blocker without pretending the hard cap disappeared globally.

## Regression Scan

- ORCHESTRATOR §3W CH3 asks whether a proposal re-opens `skinny/REDRESS.md`, correctly identifies the pre-block list, and avoids silently regressing admitted rows (`restart/prompts/ORCHESTRATOR.md:81-88`). The V6 authorization commit touches only the three packet surfaces and does not change the candidate design, pre-block list, owner paths, or row gates.
- The pre-block list remains explicit in the packet: REDRESS 28+33, 50-55, 60-72, 80, 82, 83, 84, 88, 89, plus historical blocked routes (`restart/skinny/tranches/sk-v8/SPEC.md:569-588`, `restart/skinny/tranches/sk-v8/HANDOFF.md:139-152`). W3 still must prove the union is not a renamed REDRESS 82, 83, 84, 88, or 89 route before selection (`restart/skinny/tranches/sk-v8/SPEC.md:413-420`).
- The SC-3 substrate candidate remains bounded to one retained `Tape`, one producer, and deletion of scalar structural rediscovery; `StructuralIndex` may not survive as an independent query/cache/attachment surface (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:118-123`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:286-295`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:407-423`). That preserves the V5 CH3 accept basis that the candidate is one retained `Tape`, opaque ordinals/fact ids, no old offset append fallback, and no unowned production consumer (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V5-CONSOLIDATED.md:28`).
- Strict-vs-strict discipline is preserved. The packet keeps same-run strict anchors separate from flaw probes and sidecar planning signals (`restart/skinny/tranches/sk-v8/SPEC.md:44-55`, `restart/skinny/tranches/sk-v8/SYNTHESIS.md:217-225`), and W3 selected rows must prove strict validation, comparator evidence, structural cursor work, and admitted tape facts inside the measured row (`restart/skinny/tranches/sk-v8/SPEC.md:476-492`).
- Lock 14 remains grammar-neutral. Generic code may store generated class ordinals and opaque fact ids, but grammar meaning stays in generated grammar modules; CSS L4, Sheets, and BBNF-self proof remains required for generic edits (`restart/skinny/tranches/sk-v8/SPEC.md:247-269`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:324-399`).
- No new directive, BIR variant, `BackendShape`, `UnionTape`, public substrate type, parser-owned cursor/facts, or parallel substrate is admitted (`restart/skinny/tranches/sk-v8/SPEC.md:180-192`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:545-550`). SC-3's proof row explicitly greps for `UnionTape`, `BackendShape::Union`, new directives, BIR variants, and second-substrate failures (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:480`).
- No deferral is introduced by the V6 authorization. W3 remains blocked on W0/W1 closure, fresh plan owner paths, same-wave production consumer, revert protocol, measurement thresholds, measured-path strict validation proof, and challenge acceptance (`restart/skinny/tranches/sk-v8/HANDOFF.md:80-90`, `restart/skinny/tranches/sk-v8/SPEC.md:447-452`). `tape_vs_tape` remains telemetry/residual only, not a W3 production consumer (`restart/skinny/tranches/sk-v8/SPEC.md:125-131`, `restart/skinny/tranches/sk-v8/HANDOFF.md:87-90`).

## Residual Non-Blocking Risks

- The exceptional V6 authorization is recorded in packet surfaces, not by editing ORCHESTRATOR or the S-P2 prompt. This is acceptable because the user explicitly selected the hard-ceiling override path, but future consolidations must not generalize this into a reusable post-V5 route.
- The Lock 1 refinement SC-6-L1-R1 remains a Pass Omega candidate, so a later W3 plan must either wait for Omega ratification or prove the union satisfies Lock 1 as written (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:254-265`, `restart/skinny/tranches/sk-v8/SPEC.md:462-464`).
- SC-3 Tier A is still a high-blast-radius W3 candidate. Its acceptance here is only regression/governance acceptance for S-P2 research; implementation still requires post-W0/W1 owner paths, tests, thresholds, scalar/checkasm parity where relevant, and challenge acceptance (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:469-508`).

## Required Folds If REVISE

None. This CH3 review is ACCEPT.

Preserve these folds in consolidation: V6 is a user-authorized exception only; one V6 ACCEPT does not dispatch S-P3; any further over-ceiling cycle needs explicit user authorization or S-P2 needs an explicit user final pin; the Tier A union remains one retained `Tape` with no sidecar; `tape_vs_tape` stays telemetry/residual; strict admission remains strict-vs-strict; Lock 14 grammar-neutrality and no-new-directive/BIR/substrate gates remain mandatory.
