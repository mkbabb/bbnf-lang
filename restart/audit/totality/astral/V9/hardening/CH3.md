# Pass Omega V9 CH3 Regression

Date: 2026-05-28.
Worker: Pass Omega V9 CH3 regression hardening.
Scope: V9 source packet at `17e7248fe`, SK-V15 SPEC dependency rows,
PASS-IMPL V1, T-P1/T-P2/T-P3 hardening, and REDRESS history around
183/184/209-213/215.
Disposition: ACCEPT.

## Verdict

ACCEPT. The Pass Omega V9 packet does not reopen rejected REDRESS routes or
old SK-V14/V8 implementation routes. It correctly treats SK-V14 row-ledger
evidence as historical, keeps SK-V15 W0-W11 as the current implementation
contract, and blocks closure through CSS broadcast admission, delete-before-
provider sequencing, stale W5B/W5C/W5D dispatch, fake generated CSS,
fact-stream/brace-counter CSS proof, Pattern H header-only close, runtime
regex/DFA substrate, retained sidecars, x86/AVX-512 evidence, production FNV,
W12/challenge-time overflow, or SK-V16 deferral.

No CH3-specific revision is required.

## Evidence

PASS-IMPL V1 is the regression anchor. It finds all 24 CSS L4 row admits are
one measurement broadcast, the CSS full-parse comparator is a brace-counter /
wrong-plane workload, the CSS generator is a 646-line `CSS_GENERATED_RS`
string literal, W5C/W5D only relocated provider content, Pattern H is still
67 files with 0/67 generated headers, Lock 14 excludes leak roots, Decision
Engine is scaffold, CSS has no Value API, and FNV closed-enum products are
bench-only (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21`-
`33`, `:37`-`:65`).

SK-V15 SPEC preserves the same regression boundaries. Global close requires no
CSS 24-row broadcast admit, retirement of `CSS_GENERATED_RS`,
`CssFullParseSummary`, fact-stream-only CSS `parse()`, and brace-counter
admission from live CSS proof, typed CSS value/document/view/visitor surfaces,
same-workload `cssparser` retime, restored Lock 14/16 gates, exactly 67
Pattern H files with true line-1 provenance, real Decision/lowerer evidence,
bench-only FNV, and PASS-IMPL V2 or row-level intrinsic-block proof
(`restart/skinny/tranches/sk-v15/SPEC.md:49`-`:84`). Its non-negotiables block
delete/retirement before rebuild proof, S-P2 rejects without fresh materially
different evidence, sidecars/public substrate/sixth BackendShape/production FNV,
and x86/AVX-512 admission (`restart/skinny/tranches/sk-v15/SPEC.md:135`-
`:153`).

The dependency rows are correctly load-bearing. They route CSS broadcast to W1
diagnostic demotion only, block `CSS_GENERATED_RS` and CSS summary/fact-stream
proof until W5/W6 typed CSS provider evidence, block CSS provider/template
deletion before W6 proof, reject Pattern H fake/header-only close, require
Decision Engine executable proof, split lowerer proof across W8/W9, quarantine
FNV in W10, and block W11 while any dependency row is orphaned
(`restart/skinny/tranches/sk-v15/SPEC.md:187`-`:204`).

The V9 MASTER proposal makes SK-V14/V8 historical and non-dispatching. It says
no W5B/W5C/W5D/W6/W7/W8/W9/W10 row in the historical SK-V14 block may bypass
SK-V15 W0, W1 CSS honesty, W2 gates, W5/W6 CSS typed provider/retime, W7-W9
Decision/lowerers, or W10 FNV quarantine (`restart/audit/totality/astral/V9/master-plan-diff.md:24`-
`:35`). The new SK-V15 block refuses stale CSS rows, documentation-only proof,
overfit comparators, x86/AVX-512 diagnostics, source inventory, and SK-V16
deferral as close (`restart/audit/totality/astral/V9/master-plan-diff.md:69`-
`:76`), and states there is no W12 or challenge-time implementation overflow
(`restart/audit/totality/astral/V9/master-plan-diff.md:93`-`:119`).

The V9 locks amendment is regression-tight, not permissive. It preserves 16
locks, exactly five `BackendShape` variants, no new substrate/API/sidecar/lock
retirement/sixth shape, and aarch64-only admission (`restart/audit/totality/astral/V9/locks-diff.md:3`-
`:11`). It rejects runtime regex/DFA substrate unless a later G-Omega amends
Lock 1, and says manifest plus consumer proof is necessary but not sufficient
(`restart/audit/totality/astral/V9/locks-diff.md:49`, `:71`). It also rejects
provider/template deletion before W5/W6 proof, repeated throughput tuples as
admission, Decision closure without real egraph/CSP/lowerer evidence, grammar
branches, x86/AVX-512 close, source-only primitive claims, and FNV/hash routes
without the required quarantine/consumer proof (`restart/audit/totality/astral/V9/locks-diff.md:57`-
`:71`).

Omega-D and Omega-F carry the same refusal posture into the active dispatch
surfaces. Omega-D's refused routes explicitly include CSS 24-row broadcast,
`CSS_GENERATED_RS`, fact-stream/brace-counter CSS proof, delete-before-provider,
header-only Pattern H close, gate exclusions, source-present primitives, x86/
AVX-512, sidecars/sixth shape, production FNV, W12/challenge overflow, and
SK-V16 deferral (`restart/audit/totality/astral/V9/ΩD-master-plan-reconciliation.md:102`-
`:118`). Omega-F makes W0 the next dispatch, keeps SK-V14 W5B.0 historical,
requires W0-W11 order, blocks old CSS proof, blocks delete/retire before
provider proof, blocks production FNV/retained sidecars/second tapes/public
substrate/sixth shape, and rejects W11 close via orphan dependency rows,
documentation-only evidence, or planned SK-V16 handoff (`restart/audit/totality/astral/V9/ΩF-migration-handoff.md:98`-
`:129`, `:131`-`:150`, `:191`-`:208`).

Omega-E is consistent with the SK-V15 authority transition. It says SK-V14 is
historical evidence, SK-V15 is active, W0 is first after Pass Omega/G-Omega,
CSS broadcast rows are diagnostic/NO-GO, Pattern H requires true provenance,
Decision Engine is scaffold, gates must report exclusions, and FNV remains
bench-only (`restart/audit/totality/astral/V9/ΩE-skinny-corpus.md:14`-`:31`).
Its cross-surface notes preserve REDRESS-183/184/209-215 and W5B-FRONTENDR as
historical while refusing current dispatch through them (`restart/audit/totality/astral/V9/ΩE-skinny-corpus.md:295`-
`:320`).

T-P hardening corroborates the route status. T-P1 V5 preserved CSS, Pattern H,
Lock 14/16, Decision, and FNV as open work (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:61`-
`:73`). T-P2 V3 locks research conclusions that JSON is scoped, CSS remains
refuted until typed provider plus same-workload `cssparser`, gates require
inclusion/exclusion reporting, Lock 16 is aarch64/scalar/parity/consumer gated,
Decision requires real egraph/CSP/lowerer evidence, and runtime regex/DFA import
remains blocked without consumer plus CH3/CH5 review (`restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:44`-
`:62`). T-P3 V5 specifically reports that REDRESS routes are not reopened and
stale receiver blocks stay historical/pre-block (`restart/audit/totality/p3/hardening/HARDENING-T-P3-V5-CONSOLIDATED.md:31`-
`:41`).

## Regression Matrix

| Route checked | Result | CH3 basis |
|---|---:|---|
| CSS 24-row broadcast admission | ACCEPT | PASS-IMPL refutes it; SK-V15 W1 demotes/collapses it; V9 MASTER/locks/HANDOFF all treat it as diagnostic only. |
| Delete-before-provider / stale W5B-W5D route | ACCEPT | SPEC dependency rows require same-wave/later provider proof; V9 marks SK-V14 W5B/W5C/W5D historical and non-dispatching. |
| `CSS_GENERATED_RS` / string-literal generator | ACCEPT | PASS-IMPL identifies the fake generator; SPEC blocks live proof until W5/W6 typed provider; locks-diff rejects centralization and fake generated templates. |
| Brace-counter / fact-stream CSS proof | ACCEPT | SPEC removes `CssFullParseSummary`, fact-stream-only `parse()`, and brace counters from live admission; V9 repeats same-workload typed `cssparser` retime. |
| Pattern H fake/header-only close | ACCEPT | V9 preserves 67 root runtime files and requires true line-1 generator provenance plus regen/check proof. |
| Runtime regex/DFA substrate | ACCEPT | Locks-diff says manifest/consumer proof is necessary but never sufficient before a G-Omega Lock 1 amendment. |
| Retained sidecars / second tape / public substrate / sixth shape | ACCEPT | SPEC, locks-diff, Omega-C, Omega-E, and Omega-F preserve the five-shape canon and reject retained sidecars and substrate expansion. |
| x86 / AVX-512 close | ACCEPT | SPEC and V9 packet classify x86/AVX-512 as diagnostic; admission stays Apple M5 Max / aarch64 only. |
| FNV production arbiter | ACCEPT | PASS-IMPL classifies FNV as bench-only; SPEC W10 quarantines it; V9 refuses production arbiters/selectors/hash correctness proof. |
| W12 / challenge-time overflow | ACCEPT | SPEC says W0-W11 consumes the ceiling; V9 MASTER and Omega-F say no W12 escape hatch or challenge-time implementation overflow. |
| SK-V16 deferral-as-close | ACCEPT | SPEC and V9 MASTER/Omega-F say SK-V16 receives proven remainder only after W11/PASS-IMPL V2 and cannot substitute for SK-V15 proof. |

## Required CH3 Folds

None. Carry the V9 packet forward with these guardrails unchanged:

1. SK-V14 and Pass Omega V8/W5B-W5D remain historical/pre-block evidence only.
2. SK-V15 W0-W11 is current; W0 is first after Pass Omega V9 convergence and
   G-Omega/CRUD authorization.
3. All CSS close goes through W1 demotion, W5 typed provider, and W6 same-
   workload typed `cssparser` retime.
4. All delete/retire/provider-template movement obeys the SK-V15 dependency
   rows, with provider proof no later than the delete/retire wave.
5. Pattern H remains 67 files until true line-1 provenance and non-writing
   regen/check proof cover the full root runtime surface.
6. Lock 1/14/16, Decision Engine, BackendShape, runtime regex/DFA, sidecar,
   FNV, and native-host boundaries remain fail-closed exactly as written in
   `locks-diff.md`.

Final CH3 result: ACCEPT.
