# CH6 Next-Tranche-Impact Challenge - SK-V8 Alpha V2

Date: 2026-05-17.

Scope: review the final SK-V8 packet against V1 CH6 and V1 CONSOLIDATED for
next-tranche dispatch impact: W0-only G-Alpha dispatch, per-wave revert
protocols, hard caps, telemetry goalset, CostFacts ordering, no dispatch with
open critical defects, and Pass Omega routing.

Read set:

- `restart/skinny/tranches/sk-v8/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v8/research/alpha-hardening/V1/CH6.md`
- `restart/skinny/tranches/sk-v8/research/alpha-hardening/V1/CONSOLIDATED.md`

Overall disposition: ACCEPT.

Dispatch disposition: ACCEPT W0-only dispatch after user `G-Alpha closed`.
REJECT any broad SK-V8 dispatch that treats W1-W6 as executable before W0
closes and the required post-W0 plan augmentations exist.

Remaining CH6 blockers for G-Alpha/W0: none.

Remaining non-W0 blockers: W1-W6 are intentionally conditional. They are not
dispatchable from this packet alone and require W0 close plus the exact owner
paths, row gates, pre-blocked routes, revert protocol, and same-wave consumer
named by their wave plans.

## Disposition Table

| Topic | V2 disposition | Resolution |
|---|---|---|
| W0-only G-Alpha dispatch | ACCEPT | `SYNTHESIS.md`, `SPEC.md`, `HANDOFF.md`, and `DISPATCH-PROMPT.md` all state that G-Alpha authorizes W0 only, with W1-W6 conditional on W0 close and plan augmentation. |
| Per-wave revert protocols | ACCEPT | `SPEC.md` gives W0-W6 revert protocol blocks. W0 and W1 also name downstream blocking effects; W3 and W5 name parse/close blockers; W6 names close-blocking mismatch handling. |
| Hard caps table | ACCEPT | `SPEC.md` and `DISPATCH-PROMPT.md` carry wave hard caps, default phase caps, max six research agents, and W0 dispatch phase limits. Conditional waves must refresh their plans before dispatch. |
| Telemetry goalset | ACCEPT | `SPEC.md` defines the current outcome enum including `K` and `N-direct`, required SK-V8 telemetry, gate consumption, and an opening row goalset for all 38 current main rows. |
| CostFacts ordering | ACCEPT | CostFacts binding is W1, before typed, parse, or direct behavior waves. W1 rejection blocks behavior waves. |
| No dispatch with open critical defects | ACCEPT | V1 critical defects are closed for W0 scope: final docs exist, W0 is the only dispatchable wave, CostFacts is before behavior, telemetry enum and goalset are specified, and Pass Omega is routed. This V2 review finds zero remaining CH6 critical defects for G-Alpha/W0. |
| Pass Omega decision | ACCEPT | `SYNTHESIS.md` and `HANDOFF.md` state that Pass Omega remains queued and separate and does not block G-Alpha for SK-V8 W0. |

## Evidence

### W0-Only Dispatch

The final packet resolves the V1 dispatch blocker. The synthesis says G-Alpha
user sign-off is required and authorizes only W0 unless the user explicitly
signs off on later post-W0 plan augmentations. The SPEC says no implementation
wave dispatches before G-Alpha and only W0 dispatches after G-Alpha. The
handoff repeats that only W0 is dispatchable from this packet. The dispatch
prompt begins with the same rule: do not dispatch any SK-V8 wave until G-Alpha
is closed; if closed, dispatch W0 only.

Disposition: ACCEPT.

### Revert Protocols

V1 CH6 required W0-W6 revert coverage with rollback slice, retained evidence,
REDRESS rule, and downstream effect. The final SPEC includes explicit revert
protocol sections for every wave:

- W0 reverts report, gate, and schema changes together and restores the opening
  RESULTS schema if telemetry cannot be populated or validated.
- W1 reverts CostFacts report/gate changes together and records the missing
  fact class.
- W2 reverts row additions or leaves them disabled only with explicit rejected
  status and restores generated outputs if behavior changed.
- W3 reverts runtime, template, generated, gate, and RESULTS changes as one
  slice, saves the rejected patch, and records REDRESS target and guard rows.
- W4 reverts behavior changes, keeps the triage report, and adds REDRESS for a
  failed behavior candidate.
- W5 fixes bounded drift or reverts the offending wave slice or marks close
  blocked with a named owner.
- W6 reopens the producing wave or marks close blocked with a mismatch list.

The revert matrix is no longer a CH6 blocker.

Disposition: ACCEPT.

### Hard Caps

V1 CH6 asked for dispatchable hard caps. The final SPEC has a wave manifest
with W0 180 min, W1 240 min, W2 300 min, W3 300 min, W4 240 min, W5 180 min,
and W6 120 min. It also sets default phase caps: research 30 min per agent,
max six agents; plan 30 min; redress is the wave-specific remainder; challenge
90 min when required.

The dispatch prompt makes W0 directly executable with 1-6 research agents, 30
minutes per agent, a plan phase, and a redress phase inside the W0 hard cap.
Because W1-W6 are not dispatchable from this prompt alone, their exact timeout
actions can be refreshed in the required post-W0 wave plans without blocking
G-Alpha/W0.

Disposition: ACCEPT.

### Telemetry Goalset

The final SPEC closes the V1 telemetry defects. It defines `A`, `C`, `G`, `K`,
`L`, and `N-direct` as the SK-V8 schema-v3 outcome enum and requires gate
rejection for unsupported outcomes after W0. It lists required telemetry:
profile artifact, cycles per byte, sample count, build flags, host triple,
feature mask, CostFacts rule id, CostFacts chosen shape, CostFacts rejected
alternative ids, REDRESS entry, wave id, run id, sidecar freshness, and
`SK-V8-open` delta.

Section 0.5 gives every current main row a W0 target. The W0 section requires
all 38 current main rows to satisfy telemetry, forbids throughput movement
beyond +/-1.0 percent versus `SK-V8-open`, rejects malformed sidecar manifests,
and forbids parser, scanner, SIMD, asm, codegen, or product-plane behavior
changes.

Disposition: ACCEPT.

### CostFacts Ordering

V1 CH6 rejected the Alpha-F order because CostFacts came after behavior waves.
The final SPEC moves CostFacts Gate Binding to W1. W2, W3, and W4 all require
W0 and W1 closure before dispatch. W1 binds chosen shape, rejected
alternatives, evidence source, wave id, and REDRESS references into the gate
report, and W1 rejection blocks behavior waves.

Disposition: ACCEPT.

### Critical Defects

V1 CH6 listed five dispatch-critical defects:

1. Final dispatch documents did not exist.
2. W1-W3 thresholds, owner paths, schema semantics, and Pass Omega status were
   open.
3. The final SPEC did not include W2-W6 revert protocols.
4. CostFacts ordering allowed route-fact changes before the gate was
   load-bearing.
5. Telemetry enum and missing-field semantics were not dispatchable.

For W0-only G-Alpha, those are closed. The final documents exist. Later-wave
thresholds and owner paths are deliberately moved behind W0 close and plan
augmentation, so they are not W0 dispatch blockers. W0-W6 revert protocols are
present. CostFacts is W1 before behavior. The enum and telemetry fields are
specified.

This V2 report therefore records zero remaining CH6 critical defects for
G-Alpha/W0. If a later packet tries to dispatch W1-W6 without the required
post-W0 plan updates, that is a new REJECT condition, not a blocker to W0.

Disposition: ACCEPT.

### Pass Omega

The V1 open item was binary: either Pass Omega blocks G-Alpha or it routes
after SK-V8 opens. The final packet chooses the second option. `SYNTHESIS.md`
says Pass Omega remains queued but does not block G-Alpha for SK-V8 W0 and owns
top-level CRUD, lock amendments, broad path cleanup, and non-skinny canonical
surface refresh. `HANDOFF.md` repeats that Omega is queued and separate and
does not block G-Alpha for SK-V8 W0.

Disposition: ACCEPT.

## Final CH6 V2 Disposition

ACCEPT G-Alpha presentation for W0-only dispatch.

ACCEPT the resolved next-tranche revisions:

- W0-only G-Alpha dispatch.
- Per-wave revert protocols.
- Hard caps table and W0 dispatch phase caps.
- Per-row telemetry goalset and enum mapping.
- CostFacts-before-behavior ordering.
- No remaining CH6 critical defects for W0 scope.
- Pass Omega non-blocking route.

REJECT broad dispatch:

- W1-W6 are not executable from this packet alone.
- Any behavior wave before W0 and W1 close remains rejected.
- Any later wave without exact owner paths, row gates, pre-block citations,
  same-wave consumer, and revert protocol remains rejected.
