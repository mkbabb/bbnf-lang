# Pass Omega V9 - Omega-A V1 Spec Coherence Audit

Date: 2026-05-28.
Worker: Omega-A V1 spec coherence.
Scope: SK-V15 totality closure against V1 surfaces.
Write path: `restart/audit/totality/astral/V9/ΩA-coherence-audit.md`.

## Verdict

REVISE REQUIRED before SK-V15 CRUD.

The live V1 spec surfaces are still materially V8 / SK-V14 oriented while the
current totality packet is SK-V15: T-P1 V5 clean-final/G1-pinned, T-P2 V3
normal 3Z locked, and T-P3 V5 final-convergence locked. The SK-V15 skinny packet
is internally coherent around PRUNE-then-REBUILD, W0-W11, Apple M5 Max/aarch64
admission only, CSS audit-demotion, Pattern H 67 provenance repair, Decision
Engine activation, all-five BackendShape lowerer proof, and FNV quarantine.

No live V1 surface should claim CSS closure, Pattern H collapse, Decision Engine
load-bearing status, Lock 14/16 gate closure, FNV production admissibility, or
CSS >SOTA until the SK-V15 W0-W11 gates produce executable evidence.

## Source Map

| Source | Coherence role |
|---|---|
| `restart/prompts/pass-contracts/PASS-OMEGA.md:35-46` | Omega-A scope: cross-document claims, lock refs, implementation pairings. |
| `restart/skinny/tranches/sk-v15/SYNTHESIS.md:34-50` | SK-V15 close conditions. |
| `restart/skinny/tranches/sk-v15/SPEC.md:49-80` | Locked global close condition. |
| `restart/skinny/tranches/sk-v15/SPEC.md:172-185` | W0-W11 wave manifest. |
| `restart/skinny/tranches/sk-v15/SPEC.md:195-204` | Dependency rows for CSS old proof, Pattern H, Decision Engine, lowerers, FNV. |
| `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:42-67` | Per-wave pre-dispatch checks. |
| `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21-31` | CSS broadcast / wrong-plane / string-literal blocker. |
| `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:37-58` | Pattern H, Lock 14 exclusion, Decision Engine, CSS value blockers. |
| `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:61-73` | Preserved open work. |
| `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:44-62` | Locked research conclusions. |
| `restart/audit/totality/p3/hardening/HARDENING-T-P3-V5-CONSOLIDATED.md:43-68` | Proposal-only status and Pass Omega V9 entry. |
| `restart/locks/LOCKS.md:100-109` | FactStream is substrate-manifest only; five BackendShape canon preserved. |
| `restart/locks/LOCKS.md:349-364` | Lock 14 generated-output and grammar-neutrality text. |
| `restart/locks/LOCKS.md:491-533` | Lock 16 strict/aarch64/CollapsedStage close-state constraints. |

Commit anchors checked with `git rev-parse --verify`: `8e7378025`,
`cbafeb566`, `cafb95682`, `77b6e9fd7`, and `6f1dd8aae` resolve.
`restart/audit/totality/p2/hardening/HARDENING-T-P2-V5-CONVERGED.md` does not
exist; current T-P2 authority is `HARDENING-T-P2-V3-CONSOLIDATED.md`.

Live source checks:

```text
pattern_h_files=67
generated_line1=0
```

`skinny/crates/ir/src/lib.rs:339-346` and
`skinny/crates/ir/src/cost.rs:333-340` preserve exactly five BackendShape
variants: EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage.
`grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` returns `16`.

## Findings

### OA-01 - HANDOFF points at the obsolete V8/SK-V14 dispatch state

Target surface: `restart/HANDOFF.md`.

Evidence:
- `restart/HANDOFF.md:3-28` says current status is Pass Omega V8 and SK-V14
  W5B.0 LOCK14-GATE.
- `restart/HANDOFF.md:70-83` says current measured authority is the SK-V14
  audit-corrected baseline and names SK-V14 locked surfaces.
- `restart/HANDOFF.md:149-177` still gives the V8 W5B-FRONTENDR next-cycle
  directive.
- Current SK-V15 handoff says SK-V15 is open PRUNE-then-REBUILD, the locked
  skinny output is W0-W11, and admission evidence is Apple M5 Max/aarch64 only
  (`restart/skinny/tranches/sk-v15/HANDOFF.md:13-23`).
- T-P3 V5 says Pass Omega V9 may dispatch against SK-V15 totality and G-Omega
  is next (`restart/audit/totality/p3/hardening/HARDENING-T-P3-V5-CONSOLIDATED.md:63-68`).

Disposition: update required.
Proposed CRUD owner: CRUD-4 HANDOFF + MIGRATION.

### OA-02 - HANDOFF reading order cites a nonexistent T-P2 V5 authority

Target surface: `restart/HANDOFF.md`.

Evidence:
- `restart/HANDOFF.md:91-94` names
  `HARDENING-T-P2-V5-CONVERGED.md` and T-P3 V4 as current reads.
- `HARDENING-T-P2-V5-CONVERGED.md` is absent; T-P2 current authority is V3
  (`restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:15-19`).
- T-P3 current authority is V5 final convergence, not V4
  (`restart/audit/totality/p3/hardening/HARDENING-T-P3-V5-CONSOLIDATED.md:16-21`).

Disposition: update required.
Proposed CRUD owner: CRUD-4 HANDOFF + MIGRATION.

### OA-03 - ARCHITECTURE authority ledger is still SK-V14/T-P3 V4

Target surface: `restart/ARCHITECTURE.md`.

Evidence:
- `restart/ARCHITECTURE.md:19-27` declares SK-V14 totality status and T-P3 V4
  as current authority.
- Current T-P1 explicitly advanced by clean-final G1 pin, not normal 3Z
  (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:21-28`).
- Current T-P2 is normal 3Z locked at V3
  (`restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:15-19`).
- Current T-P3 is V5 final-convergence lock
  (`restart/audit/totality/p3/hardening/HARDENING-T-P3-V5-CONSOLIDATED.md:16-21`).

Disposition: update required.
Proposed CRUD owner: CRUD-1 ARCHITECTURE.

### OA-04 - MASTER-PLAN still frames implementation through SK-V6/SK-V14 and old CSS parity rows

Target surface: `restart/MASTER-PLAN.md`.

Evidence:
- `restart/MASTER-PLAN.md:30-35` still names SK-V6 as the mandatory
  implementation handoff.
- `restart/MASTER-PLAN.md:145-149` includes old CSS/lightningcss and mixed-host
  rows as exact SOTA close rows.
- SK-V15 requires cssparser as near-term same-workload comparator and treats
  lightningcss as usable only after comparable CSSOM/value output exists
  (`restart/skinny/tranches/sk-v15/SPEC.md:61-63`).
- SK-V15 admission host is Apple M5 Max/aarch64 only; x86 and AVX-512 are
  diagnostic (`restart/skinny/tranches/sk-v15/SPEC.md:135-137`).
- The current SK-V15 wave manifest is W0-W11 (`restart/skinny/tranches/sk-v15/SPEC.md:172-185`),
  not the SK-V14 receiver block at `restart/MASTER-PLAN.md:751-827`.

Disposition: update required.
Proposed CRUD owner: CRUD-2 MASTER-PLAN.

### OA-05 - MIGRATION has no SK-V15 V9 receiver and still routes PRUNE through SK-V14 W5/W6/W7

Target surface: `restart/MIGRATION.md`.

Evidence:
- `restart/MIGRATION.md:30-52` defines the latest migration receiver as Pass
  Omega V2 carry-forward into SK-V14 PRUNE-3/4/5.
- `restart/MIGRATION.md:129-144` ends at Pass Omega V8 W5B-FRONTENDR.
- SK-V15 now routes CSS broadcast demotion, CSS typed provider, same-workload
  retime, Lock 14/16 gate restoration, Pattern H provenance, Decision Engine
  spine, BackendShape lowerers, and FNV quarantine through W0-W11
  (`restart/skinny/tranches/sk-v15/SPEC.md:172-185`).
- Dependency rows for the migration-critical delete/retire actions are now
  `DEP-W6-CSS-GENERATED-RS`, `DEP-W6-CSS-SUMMARY-FACT-STREAM`,
  `DEP-W3-W6-CSS-PROVIDER-TEMPLATE`, `DEP-W4-PATTERN-H-PROVENANCE`,
  `DEP-W7-DECISION-SPINE`, `DEP-W8-LOWERERS-A`, `DEP-W9-LOWERERS-B`, and
  `DEP-W10-FNV-QUARANTINE` (`restart/skinny/tranches/sk-v15/SPEC.md:195-204`).

Disposition: update required.
Proposed CRUD owner: CRUD-4 HANDOFF + MIGRATION.

### OA-06 - Lock file-line references in V1 surfaces are stale

Target surfaces: `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`,
`restart/MIGRATION.md`.

Evidence:
- `restart/ARCHITECTURE.md:41-50` cites Lock 1, Lock 5, Lock 14, and Lock 4 at
  old line numbers such as `LOCKS.md:48`, `:113`, and `:220`.
- `restart/MASTER-PLAN.md:23-28` cites Lock 1 at old `LOCKS.md:48`.
- `restart/MIGRATION.md:17-20` cites Lock 1/5/13/14 at old line numbers.
- Actual lock headers now begin at Lock 1 `restart/locks/LOCKS.md:75`, Lock 5
  `restart/locks/LOCKS.md:181`, Lock 13 `restart/locks/LOCKS.md:336`, Lock 14
  `restart/locks/LOCKS.md:349`, and Lock 16 `restart/locks/LOCKS.md:453`.

Disposition: citation repair required. This is not a lock-content amendment.
Proposed CRUD owner: CRUD-1, CRUD-2, CRUD-4; CRUD-6 may run the final citation
verification transcript.

### OA-07 - ARCHITECTURE implementation status risks preserving a CSS/SinkOnly paper close

Target surface: `restart/ARCHITECTURE.md`.

Evidence:
- `restart/ARCHITECTURE.md:1198-1202` says SinkOnly is admitted through the CSS
  L4 declaration-values row.
- PASS-IMPL V1 says CSS L4 is contrived, CSS rows are one measurement broadcast,
  CSS has no value API, and the CSS generator is a string-literal tokeniser
  (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21-31`,
  `:56-58`).
- SK-V15 marks CSS L4 as audit-demoted/reopened and requires typed value output
  plus same-workload cssparser comparison before CSS admission
  (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:55-68`,
  `restart/skinny/tranches/sk-v15/SPEC.md:54-63`).

Disposition: update required. Preserve the five-shape canon, but downgrade CSS
SinkOnly row evidence to diagnostic/audit-demoted until SK-V15 W5/W6 proof.
Proposed CRUD owner: CRUD-1 ARCHITECTURE.

### OA-08 - Pattern H language must shift from SK-V14 deletion/collapse to SK-V15 provenance repair

Target surfaces: `restart/HANDOFF.md`, `restart/MASTER-PLAN.md`,
`restart/MIGRATION.md`.

Evidence:
- `restart/HANDOFF.md:123-126` correctly states Pattern H count is 67, but in
  the SK-V14 W5/W6 frame.
- `restart/MASTER-PLAN.md:520-533` says F close should trend monotonically
  downward through the SK-V14 W6 collapse plan.
- `restart/MIGRATION.md:45` routes Pattern H to SK-V14 W6 PRUNE-4.
- SK-V15 requires the count to remain 67 and every file to carry true line-1
  generated provenance (`restart/skinny/tranches/sk-v15/SPEC.md:69-70`,
  `:327-332`).
- Live check: Pattern H files = 67; generated line-1 headers = 0.

Disposition: update required. Do not claim Pattern H collapse; route to SK-V15
W4 provenance/generator-check discipline.
Proposed CRUD owner: CRUD-2 MASTER-PLAN and CRUD-4 HANDOFF + MIGRATION.

### OA-09 - Decision Engine status must be scaffold/open, not load-bearing

Target surfaces: `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`,
`restart/MIGRATION.md`.

Evidence:
- `restart/MIGRATION.md:47` frames SK-V14 W7 as wiring policy/union scaffold to
  load-bearing.
- `restart/MASTER-PLAN.md:642` still routes decision replacement through the
  SK-V14 W7 receiver.
- Live source has zero e-graph rewrites:
  `skinny/crates/passes/src/backend_egraph.rs:65-67`.
- Live source still has grammar-named CSP fields/status:
  `skinny/crates/ir/src/cost.rs:242-243` and
  `skinny/crates/passes/src/decision_csp.rs:162-166`.
- SK-V15 W7 requires e-graph rewrite count >= 1, non-tautological CSP, and no
  `json_*`/`css_*` facts in generic selection
  (`restart/skinny/tranches/sk-v15/SPEC.md:378-392`).

Disposition: update required.
Proposed CRUD owner: CRUD-1 ARCHITECTURE, CRUD-2 MASTER-PLAN, CRUD-4
HANDOFF + MIGRATION.

### OA-10 - BackendShape canon is preserved, but lowerer admission claims need SK-V15 gating

Target surfaces: `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`.

Evidence:
- Locks preserve the five-shape canon and forbid a sixth shape
  (`restart/locks/LOCKS.md:100-109`, `restart/locks/LOCKS.md:271-280`).
- Live enum is exactly five variants (`skinny/crates/ir/src/lib.rs:339-346`).
- Four lowerers are label-string scaffolds:
  `skinny/crates/codegen/src/lower/eager_tape.rs:15-17`,
  `skinny/crates/codegen/src/lower/offset_tape.rs:15-17`,
  `skinny/crates/codegen/src/lower/event_tape.rs:15-17`,
  `skinny/crates/codegen/src/lower/collapsed_stage.rs:15-17`.
- SK-V15 W8/W9 require fixtures that fail the old scaffold and prove real
  output paths or gate-consumed rejected alternatives
  (`restart/skinny/tranches/sk-v15/SPEC.md:394-428`).

Disposition: update required. Preserve 5-shape canon; avoid treating shape
presence as lowerer admission.
Proposed CRUD owner: CRUD-1 ARCHITECTURE and CRUD-2 MASTER-PLAN.

### OA-11 - Lock 16 / host-admission language needs SK-V15 aarch64-only tightening

Target surfaces: `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`,
`restart/MIGRATION.md`.

Evidence:
- SK-V15 says Apple M5 Max/aarch64 is the only admission host; x86/AVX-512 are
  diagnostic (`restart/skinny/tranches/sk-v15/SPEC.md:135-137`).
- Lock 16 says AVX-512 literature is x86 pressure and cannot close M5/aarch64
  rows; CollapsedStage is mechanically refused on aarch64 until a generated
  aarch64 strategy lands (`restart/locks/LOCKS.md:515-533`).
- `restart/MASTER-PLAN.md:146-149` still carries x86 strict and mixed-host SIMD
  rows in the exact SOTA close table.
- `restart/MASTER-PLAN.md:669-716` keeps detailed AVX-512 rows; these are valid
  as diagnostics/allowlist, not SK-V15 admission anchors.
- `restart/MIGRATION.md:384-387` correctly requires strict checkasm and
  same-wave consumer, but should inherit the SK-V15 aarch64-only admission
  bracket for current close evidence.

Disposition: update required.
Proposed CRUD owner: CRUD-1 ARCHITECTURE, CRUD-2 MASTER-PLAN, CRUD-4
HANDOFF + MIGRATION.

### OA-12 - FNV quarantine is missing from V1 current-state receivers

Target surfaces: `restart/HANDOFF.md`, `restart/ARCHITECTURE.md`,
`restart/MASTER-PLAN.md`, `restart/MIGRATION.md`.

Evidence:
- PASS-IMPL V1 flags W11L/W11N/W11O FNV closed-enum products as bench-only and
  not production runtime evidence
  (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:60-65`).
- T-P1 preserves FNV/hash rows as W10 quarantine input only
  (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:71-73`).
- SK-V15 W10 requires production FNV scan and adversarial semantic fixtures;
  FNV cannot be runtime selector, production arbiter, or correctness proof
  (`restart/skinny/tranches/sk-v15/SPEC.md:430-445`).
- Current `restart/HANDOFF.md:149-177` still points at V8/SK-V14 next dispatch
  and has no SK-V15 W10 quarantine receiver.

Disposition: update required.
Proposed CRUD owner: CRUD-4 HANDOFF + MIGRATION plus CRUD-1/CRUD-2 for status
tables that discuss implementation closure.

### OA-13 - PASS-IMPL source-map path for Lock 14 gate is stale but content resolves

Target surface: audit source map / future CRUD citation hygiene.

Evidence:
- PASS-IMPL cites `skinny/xtask/src/lock14_baseline.rs:2370-2379`
  (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:47`,
  `:110`).
- Live file is `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2370-2379`;
  those lines define `GENERIC_SCAN_ROOTS` and omit `runtime_generator.rs`,
  `grammar_provider.rs`, `json_sink_direct.rs`, `json_typed_direct.rs`, and
  `json_templates/`.

Disposition: no V1 surface edit required unless a CRUD surface cites the old
path. Record corrected evidence path in any V9 source map.
Proposed CRUD owner: CRUD-6 AUDIT + CLEANUP.

## CRUD Routing Summary

| Owner | Required operations |
|---|---|
| CRUD-1 ARCHITECTURE | Replace SK-V14 authority ledger with SK-V15 T-P1/T-P2/T-P3 state; downgrade CSS/SinkOnly and lowerer claims; preserve 16 locks and five BackendShape canon; add FNV quarantine/current blockers where implementation status is discussed. |
| CRUD-2 MASTER-PLAN | Replace SK-V14 W5/W6/W7 receiver block as current with SK-V15 W0-W11 receiver; revise CSS comparator/host/SOTA rows; route Pattern H, Decision Engine, lowerers, Lock14/16, and FNV to SK-V15 waves. |
| CRUD-3 LOCKS | No immediate lock-count or lock-text amendment found by Omega-A. Preserve 16 locks, FactStream-as-substrate-manifest-only, five BackendShape canon, Lock 14, and Lock 16 aarch64/strict constraints. |
| CRUD-4 HANDOFF + MIGRATION | Update current override, read order, next dispatch, and migration receivers to SK-V15 / Pass Omega V9; remove nonexistent T-P2 V5 authority; stop saying SK-V14 W5B.0 is next. |
| CRUD-5 SKINNY CORPUS | No Omega-A direct finding against skinny packet; SK-V15 skinny surfaces are the current authority. |
| CRUD-6 AUDIT + CLEANUP | Run citation scrub for stale LOCKS line numbers and corrected Lock 14 gate source path; verify commit anchors and no orphan V8-only source maps in V9 CRUD logs. |

## No Live Edits

This audit proposes CRUD ownership only. It does not edit
`restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/MIGRATION.md`,
`restart/HANDOFF.md`, `restart/locks/LOCKS.md`, skinny sources, `RESULTS.md`,
or `REDRESS.md`.
