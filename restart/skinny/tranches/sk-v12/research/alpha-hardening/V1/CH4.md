# SK-V12 Pass Alpha CHALLENGE V1 - CH4 Cost / Scope

Date: 2026-05-20.
Lens: CH4 cost / scope.
Scope: Pass Alpha SK-V11 -> SK-V12, Alpha-E/F, SK-V12 SYNTHESIS/HANDOFF,
and REDRESS 111-120.
Output: `restart/skinny/tranches/sk-v12/research/alpha-hardening/V1/CH4.md`.

## Overall Disposition

REVISE.

The Alpha packet has the right strategic shape: it does not authorize source
work, it keeps the generated non-JSON baseline ahead of any intervention, it
pre-blocks JSON direct retries, and Alpha-E supplies five candidates, each with
owner paths, scalar/oracle status, checkasm/parity status, falsifiability gates,
LOC budgets, risk labels, and same-wave consumers. That is enough to avoid
REJECT.

The packet is not yet CH4-clean because hard caps and split triggers are not
stated on the Alpha contract surface, and the first generated non-JSON baseline
budget is not yet credible without an explicit S-P1/S-P2 preflight proof. These
are fixable folds before G-Alpha.

## Disposition Matrix

| Area | Disposition | Finding |
|---|---|---|
| Candidate count and required metadata | ACCEPT | Alpha-E has exactly five candidates and each carries the PASS-ALPHA CH4 metadata shape. |
| LOC budgets and risk classes | REVISE | Budgets exist, but E1/E2/E3 under-bind the first generated non-JSON codegen/runtime unblock and E3 risk is understated. |
| Hard caps | REVISE | No candidate-level plan/redress minute cap or split-before-dispatch rule appears in Alpha-E, Alpha-F, SYNTHESIS, or HANDOFF. |
| Wave alignment | REVISE | Priority order is clear, but fallback baseline waves and the conditional JSON companion need an Alpha-level cap/wave-alignment seed. |
| Same-wave consumers | ACCEPT | Every candidate names a gate/report or production row consumer, and SYNTHESIS/HANDOFF make producer-only telemetry fail closed. |
| Micro-proof adequacy | REVISE | E4/E5 have useful preconditions; E1/E2/E3 need an explicit baseline preflight before a 460-520 LOC redress attempt. |
| Baseline/intervention separation | ACCEPT | The baseline-first rule is load-bearing and correctly carried through Alpha-E/F, SYNTHESIS, HANDOFF, and REDRESS 113. |
| JSON direct companion | ACCEPT with hard REJECT guard | E5 is acceptable only as post-non-JSON companion work; scheduling it earlier is REJECT. |

## Critical Findings

### CH4-1 - REVISE - Hard caps are absent from the Alpha contract surface

PASS-ALPHA's CH4 lens asks for LOC budget, risk classification, wave alignment,
and same-wave consumer per intervention
(`restart/prompts/pass-contracts/PASS-ALPHA.md:43`). The orchestrator's CH4
registry additionally requires stated and realistic hard caps
(`restart/prompts/ORCHESTRATOR.md:86`). PASS-ALPHA defers exact wave gates to
S-P3, but still says each downstream wave carries a hard cap, revert protocol,
and same-wave consumer (`restart/prompts/pass-contracts/PASS-ALPHA.md:114`-
`restart/prompts/pass-contracts/PASS-ALPHA.md:122`), and G-Alpha must present
LOC budget plus hard caps (`restart/prompts/pass-contracts/PASS-ALPHA.md:169`-
`restart/prompts/pass-contracts/PASS-ALPHA.md:171`).

Alpha-E gives LOC budgets for E1 through E5:

- E1 CSS baseline: <= 520 handwritten LOC
  (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:79`).
- E2 Sheets fallback: <= 480 LOC
  (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:129`).
- E3 BBNF-self fallback: <= 460 LOC
  (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:176`).
- E4 CSS intervention: <= 430 LOC
  (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:225`).
- E5 JSON companion: <= 300 LOC
  (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:287`).

But none of Alpha-E, Alpha-F, SYNTHESIS, or HANDOFF binds those LOC budgets to
the skinny wave caps: 30 minutes for plan and 60 minutes implementation plus
15 minutes measurement for redress
(`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:47`,
`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:71`). SYNTHESIS correctly
says S-P3 owns the later implementation packet
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:263`-
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:267`), but CH4 still needs an
Alpha-level cost seed so G-Alpha can judge scope before S-P3.

Required fold: add a compact Alpha cost/cap matrix to Alpha-F or SYNTHESIS and
mirror it in HANDOFF. It should list E1-E5, candidate status, LOC budget, risk
class, downstream wave slot, plan cap, redress cap, same-wave consumer, and a
split-before-dispatch rule. This does not create SPEC; it makes the existing
candidate costs auditable.

### CH4-2 - REVISE - The first generated non-JSON baseline budget hides the REDRESS 112 unblock

REDRESS 112 is the structural blocker: W1b failed because skinny codegen still
routes direct and typed emission through `json_provider::ensure_runtime_profile`
and no generated CSS L4 runtime existed
(`skinny/REDRESS.md:3313`-`skinny/REDRESS.md:3324`). REDRESS 113 then blocked
the intervention wave because it could not create the first measurable
non-JSON baseline and claim a grammar-generalized intervention in the same wave
(`skinny/REDRESS.md:3342`-`skinny/REDRESS.md:3348`).

E1 is scoped as one high-risk baseline wave across codegen, lowering,
runtime, gate/report, metadata, benches, CSS grammars, and research artifacts
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:42`-
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:54`).
It must create generated CSS Track 1, an independent same-plane oracle, strict
parity, benchmark evidence, and gate consumption
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:55`-
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:78`).
That can be feasible only if S-P1/S-P2 find a narrow generator extension point
and a runnable oracle path. If they do not, <= 520 LOC plus one redress cap is
not a realistic first-of-class codegen/runtime/harness rebuild.

Required fold: E1/E2/E3 need an entry preflight before S-P3 may dispatch a
baseline redress wave. Minimum preflight: generated runtime target exists or can
be emitted by a named codegen seam, one fixture corpus and independent oracle
are runnable, the non-JSON report lane from REDRESS 111 consumes the report, and
a compile/equality smoke can pass without behavior source edits. If that
preflight fails, split the work into an explicit generator/runtime unblock
wave and a later baseline-report wave, or record a measured BLOCKED route.

### CH4-3 - REVISE - E3 risk is understated for a REDRESS 112-class fallback

E3 is marked medium risk because the grammar surface may be smaller
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:176`-
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:180`).
That underrates the actual cost class. E3 still crosses the same generated
non-JSON codegen/runtime boundary as E1/E2, must define a generated BBNF-self
direct output plane, and has an oracle-independence problem because the project
grammar and generator are close together
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:156`-
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:158`).

Required fold: raise E3 to medium-high, or keep medium only if S-P1 proves that
BBNF-self already has a smaller runnable generated Track 1 and an independent
oracle path than CSS L4 or Sheets.

### CH4-4 - ACCEPT - Baseline and intervention are separated correctly

The costliest prior failure was trying to let an intervention wave create the
first baseline. REDRESS 113 explicitly blocks that: no W1b baseline means
`ceil(W1b_css_baseline_mbps * 1.01)` is undefined and W2 is not measurable
(`skinny/REDRESS.md:3342`-`skinny/REDRESS.md:3348`).

The SK-V12 packet fixes that ordering. SYNTHESIS requires exactly one generated
non-JSON baseline before JSON-only micro-waves
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:35`-
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:42`) and then requires the
intervention to consume that baseline and clear
`ceil(baseline_mbps * 1.01)`
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:43`-
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:48`). HANDOFF carries the same
priority order (`restart/skinny/tranches/sk-v12/HANDOFF.md:46`-
`restart/skinny/tranches/sk-v12/HANDOFF.md:53`). Alpha-E also makes E4
dependent on E1 and `W1_css_baseline_mbps`
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:188`-
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:229`).

No fold required.

### CH4-5 - ACCEPT - Same-wave consumers are present and fail-closed

E1 requires the generated CSS L4 benchmark row and `bbnf-bench --bin gate`
consumer in the same wave
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:64`-
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:66`).
E2 and E3 require the same non-JSON gate/report consumer pattern
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:115`-
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:117`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:162`-
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:163`).
E4 rejects helper-only change by requiring the generated CSS row and gate/report
consumer (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:211`-
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:213`).
E5 requires generated JSON Track 1 plus independent Track 2 consumed by
`gate-json` in the same wave
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:272`-
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:274`).

SYNTHESIS and HANDOFF backstop this by making producer-only telemetry fail
closed (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:65`-
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:67`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:74`-
`restart/skinny/tranches/sk-v12/HANDOFF.md:81`).

No fold required beyond the cap matrix in CH4-1.

### CH4-6 - ACCEPT WITH REJECT GUARD - E5 is not scope creep if it remains conditional

E5 is the only JSON direct candidate and is explicitly lowest priority
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:235`-
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:241`).
It requires post-E4 non-JSON proof, same-host caller microbench evidence, and
proof that the same source delta was already consumed by a generated non-JSON
row (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:242`-
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:251`).
This matches REDRESS 120's routed remainder: solve the generated non-JSON
baseline first and treat the 13 JSON residual rows as exhausted unless fresh
material evidence beyond REDRESS 114-119 appears
(`skinny/REDRESS.md:3545`-`skinny/REDRESS.md:3553`).

Disposition: ACCEPT as a conditional companion. REJECT if S-P3 schedules E5
before a generated non-JSON baseline and a measured grammar-generalized
intervention are gate-consumed, or if E5 repeats numeric slot, container-tail,
bounded string span, decoded-source fold, or digest host-sink routes without a
material differential.

## Required Folds

1. Add an Alpha-level cost/cap matrix for E1-E5: LOC budget, risk class, wave
   slot, plan cap, redress cap, same-wave consumer, split-before-dispatch rule,
   and expected row effect.
2. Add E1/E2/E3 preflight requirements before baseline redress dispatch:
   generator seam, runtime target, independent oracle, fixture corpus,
   compile/equality smoke, and non-JSON gate consumption.
3. Raise E3 risk to medium-high unless S-P1 proves a materially smaller runnable
   BBNF-self baseline path.
4. State that any S-P3 wave exceeding either the LOC budget or the skinny
   redress cap returns REVISE before behavior dispatch.

## Blockers To G-Alpha

G-Alpha should not present the packet as CH4-accepted while CH4-1 and CH4-2 are
open. After the folds above, CH4 can move to ACCEPT: the candidate count is
within the Alpha cap, same-wave consumers are present, JSON direct retries are
properly pre-blocked, and the baseline/intervention split is structurally
sound.
