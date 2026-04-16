# AV Retrospective — Substrate-Without-Activation

*Forensic read of `docs/tranches/AV/{AV.md,PROGRESS.md,FINAL.md,research/}`,
AU FINAL, AW.md, and the `be4b22b1..ceb27645` span (96 commits: 83 on
`bbnf-lang` + 13 on `parse-that`).*

## 1. Scope reality vs plan

AV.md committed ten waves V0–V10 with a between-wave workspace-
failure allowance on V3–V9. FINAL records V0–V5 landed; V6–V9
(document-level parallel parse, SIMD/PHF keyword dispatch, runtime
bloom+GADT dedup, walker migration closure) routed to AW; V10
degenerated into the artefact-composition script (`post-AV.json` +
FINAL.md). The handoff is a tranche-boundary scope cut, not an
incremental deferral.

## 2. Silent vs declared deferrals

At AV.md commit-time V6–V9 were declared in-tranche. FINAL documents
the routing ("orchestrator scope decision after V5 lands"); PROGRESS
2026-04-16 confirms "Per user direction". Declared, not silent — but
only at cut-time, not at plan-time. Within-wave deferrals that
landed silently: AV.3.6 fn-per-rule deletion moved V3 → V4-close →
never (routed to AW.1.3); AV.0.5's emitter substrate landed without
a firing consumer because the layout pass never admits
`TypeDesc::Named("Color")` (inert through V5 — AW.0.5 wires);
AV.2 kept post-order emission against the plan's "pre-order from
idx+1" assumption (AW.1.10 verifies); AV.3.1 CSS DTA produced 2473
states vs predicted ~1200 (AX-conditional).

## 3. Orchestration friction — PROGRESS evidence

V0 saw three API-termination losses (Agent B respawned with narrower
scope; av4-finaliser's first bbnf-tape test consumed ~150 GB under a
runaway build). Agent B's hand-patched `generated.rs` passed
`cargo check` but failed bootstrap regen because `.bbnf-cache`
staleness produced 23-line truncation — reverted, re-dispatched.
V4 and V5 cherry-picks required manual conflict resolution on
`bbnf-tape/src/lib.rs` and `emitter/dta.rs` (concurrent module-
declaration + interpolation-site additions).

## 4. Edict adherence — RESEARCH.md six-agent fan-out

AV.md §Precedent cites `docs/instructions/RESEARCH.md` (six agents:
four substrate-deepening + two departing-thesis). AV's `research/`
holds six deliverables dated "April 2026" but identifiable by their
own headers as AU-era research ("AU Architecture Research —
Higher-Dimensional SIMD…"). AV inherited AU's research; did not
re-fan-out. The plan drew from AU research + `typed-parity-audit.md`.
AV.md stayed untouched after commit.

## 5. Chronic 5+-tranche deferrals (enumerated in AW.md)

Three chains AV inherited and re-deferred:

- **Cost-model grid sweep.** AM.6 → AO.4.1 → AP.6.4 → AQ.9.4
  → AR ("manual calibration adequate") → AW-route-to-AX.
- **Global CSP solve.** AL-prototype → AO.4.2 → AP.6.5 → AQ.9.5
  → AR ("per-component sufficient at current scale") → AW-route-to-AX.
- **Scanner-architecture cluster.** AR.6.1/6.2/6.4/6.5/6.6/6.8 +
  AS.5.1/5.2/5.4/5.5 (`RegexClassMiner`, `ScanLut` registry,
  `WsCommentConfig`, `FnDescriptor`, HIR predicate re-exports;
  AS marked as "premature optimisation", AT silent) → AX-dedicated-tranche.

Additional 4+-tranche items re-deferred through AV: AT.4.3 NEON
17-digit fractional scan, AQ.7.3 length-bucketed PHF, AN.5 `u8x32`
SIMD widening.

## 6. Mid-tranche restructuring

None within AV. The V5-boundary scope cut was the only course
correction; the plan document stayed frozen. Contrast AW, which
split into AW-I + AW-III mid-execution (commit `ff0b7fe7`).

## 7. Lessons → AW process artefacts

AW.md §Friction encodes the AV cautionary tale structurally:

1. **Bench-between-every-wave.** AV's largest operational miss:
   no bench until V10 masked the 2.5–4.5× regression (canada 455
   vs post-AU 1231). Each AW wave closes with `post-AW-WN.json`;
   missing checkpoint reopens the wave.
2. **Commit-at-milestone.** After three V0 termination losses:
   agents commit at every natural milestone; narrower file bounds.
3. **Bootstrap-regen CI gate.** AW.0.7 — `check-bootstrap-clean.sh`
   wired into `ci.yml`; pre-bootstrap cache-clear + line-count >
   24000 verification mandatory. Agent B's hand-patch route
   becomes structurally impossible.
4. **Cherry-pick ordering pre-declared** per wave in PROGRESS
   (alphabetical module placement, named interpolation sites).
5. **No workspace-failure allowance.** AV's V3–V9 permissive policy
   produced the substrate-without-activation state AW inherits;
   AW forbids between-wave failures categorically.

## Closing

AV's substrate is internally coherent; invariants upheld (1076/0/66).
The cautionary weight is operational: a substrate that consults no
consumer is correctness work paid as regression. AW's bench-between-
every-wave contract, research-wave-per-tranche mandate, and no-
workspace-failure edict are each a direct AV corrective.
