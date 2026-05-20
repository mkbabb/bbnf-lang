# SK-V12 S-P3 CH4 Cost/Scope Challenge - Cycle PIN-V3

Pass: S-P3 Synthesis-Plan.
Cycle: PIN-V3.
Lens: CH4 cost, caps, LOC budgets, owner-path breadth, generated-size budget,
W1b split adequacy, redress implementability, W1b-1 fallback cost, and W4
all-orphan cleanup visibility.
Date: 2026-05-20.
Packet under review: commit `4c53119f`.
Verdict: ACCEPT.
CH4: PASS.
Confidence: 94%.

## Disposition

PIN-V3 remains cost-safe to dispatch. The packet carries the tighter 20-minute
research, 15-minute plan, and 30-minute redress caps into both SPEC and
DISPATCH; splits CSS L4 into scaffold and comparator waves; keeps generated
output outside hand-LOC caps but gate-consumed through size telemetry; bounds
high-risk owner paths with exact row/output-plane gates and revert protocols;
and makes W4 one selected ASM-gen consumer plus visible five-row orphan
accounting rather than hidden all-orphan implementation work.

No required CH4 fixes.

## Findings

### CH4-1 - Phase caps and cap discipline are binding

Severity: ACCEPT.

Evidence:

- `restart/skinny/tranches/sk-v12/SPEC.md:239`-`:248` gives every wave a source
  LOC budget and a `<=30 min` redress cap.
- `restart/skinny/tranches/sk-v12/SPEC.md:250`-`:257` explicitly tightens the
  campaign to 20-minute research, 15-minute plan, and 30-minute redress caps,
  with 0.9x-cap record/commit behavior and a hard halt at cap.
- `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:95`-`:128` repeats the
  20/15/30 phase caps and limits redress to one implementation thread.
- `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:230`-`:231` repeats the
  0.9x-cap recording rule and cap halt.

Cost read:

The caps are not advisory prose. Dispatch agents get a hard phase budget and a
required blocking-state path when work does not fit.

Required fix: none.

### CH4-2 - W1b split is adequate for a 30-minute redress cap

Severity: ACCEPT.

Evidence:

- `restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:33`-`:40`
  states the split rationale: W1b-1 creates the generated Track 1 row and
  independent oracle/equality scaffold; W1b-2 adds lightningcss and admission.
- `restart/skinny/tranches/sk-v12/SPEC.md:244`-`:245` budgets W1b-1 at `<=360`
  hand LOC and W1b-2 at `<=300` hand/gate LOC, with generated output named
  separately.
- `restart/skinny/tranches/sk-v12/SPEC.md:425`-`:431` scopes W1b-1 to generated
  CSS Track 1, independent oracle/Track 2, strict equality, finite throughput,
  generated-size telemetry, and JSON guard state.
- `restart/skinny/tranches/sk-v12/SPEC.md:472`-`:480` scopes W1b-2 to the
  lightningcss fact extractor, three-way equality, sample-counted throughput,
  and same-wave report/gate consumption.
- `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:218`
  -`:229` forbids W1b-1 from recording CSS ADMIT, while
  `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:241`
  -`:264` reserves comparator ADMIT/REJECT for W1b-2.

Cost read:

The prior over-broad CSS target is now two bounded redress slices. W1b-1 can
fail on generation/oracle equality without also needing to build the
lightningcss comparator, and W1b-2 can measure the actual SOTA bar without
absorbing Track 1 generation work.

Required fix: none.

### CH4-3 - Owner path breadth is broad but bounded by exact wave purpose

Severity: ACCEPT.

Evidence:

- W1b-1 owner paths are concrete grammar/codegen/runtime/bench/gate/result paths
  in `restart/skinny/tranches/sk-v12/SPEC.md:392`-`:406`.
- W1b-2 narrows to the CSS bench/comparator/report/gate/result surface in
  `restart/skinny/tranches/sk-v12/SPEC.md:452`-`:461`.
- W3 owner paths are tape/codegen/generated-CSS/bench/gate paths with entry
  constraints against sidecar substrate drift
  (`restart/skinny/tranches/sk-v12/SPEC.md:501`-`:520`).
- W4 owner paths cover SIMD/scalar/checkasm/parse-that/generated-CSS/bench/gate
  files plus the orphan disposition artifact
  (`restart/skinny/tranches/sk-v12/SPEC.md:548`-`:561`).
- Dispatch redress may implement only SPEC owner paths; any other source path
  returns REVISE before editing
  (`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:124`-`:129`).
- P3-B calls out serialization of W3/W4 after W1b-2/W2 to avoid shared
  generated-runtime, bench, and gate-file races
  (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:97`
  -`:102`).

Cost read:

The owner sets are necessarily cross-cutting because generated grammars,
runtime, reports, and gates must move together. The packet keeps them bounded by
row id, output plane, entry gate, exit gate, same-wave consumer, and revert
protocol.

Required fix: none.

### CH4-4 - Generated-size budget is visible and gate-consumed

Severity: ACCEPT.

Evidence:

- The SPEC requires generated size tracking: hand LOC, generated LOC, module byte
  size, grammar source size, and O(N) growth guard; overflow blocks until traced
  (`restart/skinny/tranches/sk-v12/SPEC.md:273`-`:275`).
- W1b-1 must record generated LOC/module-size/O(N) guard
  (`restart/skinny/tranches/sk-v12/SPEC.md:430`-`:431`).
- W1b-2 must consume generated-size fields in the same-wave gate/report
  (`restart/skinny/tranches/sk-v12/SPEC.md:479`-`:480`).
- P3-D requires generated LOC, module bytes, grammar rule count, LOC per rule,
  regen command, generated diff artifact, on-budget status, and O(N) guard status
  (`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:184`
  -`:199`).

Cost read:

Generated output is not hidden inside hand-LOC accounting. The budget mechanism
is telemetry plus O(N) growth failure rather than a single fixed generated-LOC
number, which is acceptable for grammar-dependent output.

Required fix: none.

### CH4-5 - W1b-1 fallback wording has bounded cost impact

Severity: ACCEPT.

Evidence:

- The user pin says Sheets/BBNF-self become fallbacks only after a CSS L4 redress
  attempt fails, not after preflight failure
  (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18`-`:24`).
- W1b-1 failure records REDRESS and returns to plan, but explicitly does not
  satisfy the post-CSS-redress fallback condition
  (`restart/skinny/tranches/sk-v12/SPEC.md:438`-`:442`).
- W1b-2 is the first wave that records a measured lightningcss comparator /
  admission redress attempt and can unlock later fallback consideration
  (`restart/skinny/tranches/sk-v12/SPEC.md:482`-`:491`).
- P3-B mirrors that fallback discipline: fallback enters only after W1b-2 records
  a measured CSS L4 redress attempt as BLOCKED or REJECTED, and only via a future
  explicit fallback wave
  (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:104`
  -`:110`).

Cost read:

This wording can force an extra plan/S-P3 revision if W1b-1 cannot even create a
measurable CSS scaffold, because W1b-2 cannot run without W1b-1 PASS. That is a
real cost, but it is the correct cost under the pin: scaffold failure is not the
same as a measured CSS-vs-lightningcss redress attempt, and the packet prevents
fallback from being hidden inside CSS redress.

Required fix: none.

### CH4-6 - W3 and W4 redress are implementable attempts, not hidden campaigns

Severity: ACCEPT.

Evidence:

- S-P2 converged with no current shortlist-ready tape/union primitive; any
  same-tape CSS-local route is conditional after CSS Track 1, lightningcss
  comparator, strict equality, hot-leaf attribution, REDRESS 96/97/98 material
  differential, and CHALLENGE
  (`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:54`
  -`:57`).
- W3 is conditional after W1b-2 and mandatory for FIXPOINT only when no prior CSS
  row satisfies ADMIT
  (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:42`-`:48`).
- W3's SPEC entry requires a measured CSS row, fresh profile or same-host
  microbench, CHALLENGE acceptance, and proof against sidecar substrate drift
  (`restart/skinny/tranches/sk-v12/SPEC.md:513`-`:520`).
- W4 selects at most one primary ASM-gen candidate and requires scalar,
  checkasm/parity, microbench, same-wave consumer, and five-row orphan accounting
  (`restart/skinny/tranches/sk-v12/SPEC.md:563`-`:578`).

Cost read:

W3 and W4 are bounded measured attempts. They may reject and still provide
FIXPOINT evidence; they are not asked to solve all union or all ASM-gen design
space under a 30-minute redress cap.

Required fix: none.

### CH4-7 - W4 all-orphan cleanup is visible and cannot be silently absorbed

Severity: ACCEPT.

Evidence:

- The user pin names the five orphan aarch64 primitives and makes zero orphan
  kernels the SK-V12 close target
  (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:71`-`:78`).
- P3-B narrows W4 to one row-moving ASM/SIMD consumer and separately requires
  disposition of all five carried orphans by consumption, removal, or inventory
  demotion with evidence
  (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:50`
  -`:62`).
- SPEC W4 requires the plan to include a five-row orphan accounting table; any
  orphan requiring production consumption or removal outside the selected
  primitive blocks close or requires a later wave
  (`restart/skinny/tranches/sk-v12/SPEC.md:570`-`:578`).
- SPEC W4 tasks require status for all five production orphans
  (`restart/skinny/tranches/sk-v12/SPEC.md:581`-`:588`).
- P3-C defines the zero-orphan exit table and says production orphans at close
  invalidate FIXPOINT
  (`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:384`
  -`:402`).
- P3-D carries orphan disposition fields and allows `open` only during
  intermediate waves; `open` fails ADMIT and FIXPOINT close
  (`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:239`
  -`:255`).

Cost read:

This resolves the all-orphan cleanup visibility risk. W4 can stay within its cap
only if non-selected orphans are evidence-only demotions/removals; otherwise the
SPEC blocks close or requires later topology instead of silently expanding W4.

Required fix: none.

## Notes

- P3-A still summarizes the combined CSS C1 row as `<=620 hand LOC if split after
  C2` (`restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:100`
  -`:102`), while binding SPEC/DISPATCH allocate W1b-1 `<=360` plus W1b-2
  `<=300` (`restart/skinny/tranches/sk-v12/SPEC.md:244`-`:245`). This is not a
  blocker because SPEC/DISPATCH carry the dispatch budgets.
- P3-B's manifest has minor summary drift from SPEC/DISPATCH: W0 is `<=120`
  there while SPEC allows `<=160`, and W5 is `<=160` there while SPEC/DISPATCH
  bind `<=140` (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:77`
  -`:84`; `restart/skinny/tranches/sk-v12/SPEC.md:241`-`:248`;
  `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:71`-`:78`). This is
  editorial, not a CH4 failure, because the executable dispatch authority is
  bounded by SPEC/DISPATCH.

## Required Fixes

None.

## PASS/FAIL

CH4 PASS.
