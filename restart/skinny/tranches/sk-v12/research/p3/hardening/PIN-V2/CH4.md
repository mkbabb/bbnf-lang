# SK-V12 S-P3 CH4 Cost/Scope Challenge - Cycle PIN-V2

Pass: S-P3 Synthesis-Plan.
Cycle: PIN-V2.
Lens: CH4 cost, caps, LOC budgets, owner-path breadth, generated-size budget,
W1b split adequacy, redress implementability, and all-orphan cleanup visibility.
Date: 2026-05-20.
Packet under review: commit `7316d87b`.
Verdict: ACCEPT.
CH4: PASS.
Confidence: 93%.

## Disposition

PIN-V2 is cost-safe enough to dispatch. The packet binds the tighter
20-minute research, 15-minute plan, and 30-minute redress caps; splits the
previously over-broad CSS L4 work into W1b-1 and W1b-2; keeps generated output
named and separately budgeted; makes W3 conditional for ADMIT but mandatory for
FIXPOINT; and narrows W4 to one primary ASM-gen candidate plus explicit
five-row orphan accounting.

The remaining risk is ordinary high-risk wave execution risk, not a packet
scope defect. No required CH4 fixes.

## Findings

### CH4-1 - Phase caps and cap discipline are binding

Severity: ACCEPT.

Evidence:

- `restart/skinny/tranches/sk-v12/SPEC.md:239`-`:248` gives every wave a source
  LOC budget and a `<=30 min` redress cap.
- `restart/skinny/tranches/sk-v12/SPEC.md:250`-`:256` explicitly tightens the
  packet to 20-minute research, 15-minute plan, and 30-minute redress, with
  halt/record behavior at the cap.
- `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:95`-`:128` repeats the
  phase caps and names one redress agent.
- `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:230`-`:231` binds the
  0.9x-cap status/recording rule.

Cost read:

The caps are stated in both SPEC and dispatch authority. The packet no longer
relies on implicit "finish later" behavior; cap exhaustion records the blocking
state.

Required fix: none.

### CH4-2 - W1b split is adequate for the 30-minute redress cap

Severity: ACCEPT.

Evidence:

- `restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:33`-`:40`
  states why the original CSS baseline/comparator work was split: W1b-1 creates
  the generated Track 1 row and independent oracle/equality scaffold, while
  W1b-2 adds lightningcss and admission.
- `restart/skinny/tranches/sk-v12/SPEC.md:244`-`:245` budgets W1b-1 at `<=360`
  hand LOC and W1b-2 at `<=300` hand/gate LOC, with generated output named
  separately.
- `restart/skinny/tranches/sk-v12/SPEC.md:425`-`:431` scopes W1b-1 to generated
  CSS Track 1, independent oracle/Track 2, strict equality, baseline throughput,
  generated LOC/module-size/O(N) telemetry, and JSON guard state.
- `restart/skinny/tranches/sk-v12/SPEC.md:470`-`:478` scopes W1b-2 to
  lightningcss extraction, three-way equality, sample-counted throughput, and
  same-wave gate/report consumption.
- `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:220`
  -`:228` forbids W1b-1 from recording CSS ADMIT, and
  `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:241`
  -`:264` reserves ADMIT/REJECT for W1b-2 after the comparator attempt.

Cost read:

This is now two redress-sized implementation slices instead of one compound
slice. W1b-1 can honestly PASS/BLOCK on CSS generation plus oracle equality
without needing the lightningcss comparator, and W1b-2 can fail closed on
comparator/equality/throughput without hiding fallback work.

Required fix: none.

### CH4-3 - Owner path breadth is broad but bounded by exact wave purpose

Severity: ACCEPT.

Evidence:

- W1b-1 owner paths are concrete grammar/codegen/runtime/bench/gate/report
  paths in `restart/skinny/tranches/sk-v12/SPEC.md:392`-`:406`.
- W1b-2 narrows to the CSS bench/comparator/report/gate/result surface in
  `restart/skinny/tranches/sk-v12/SPEC.md:450`-`:459`.
- W3 names tape/codegen/generated CSS/bench/gate paths in
  `restart/skinny/tranches/sk-v12/SPEC.md:499`-`:509`, with entry constraints
  against sidecar substrate drift in `restart/skinny/tranches/sk-v12/SPEC.md:513`
  -`:518`.
- W4 names SIMD/scalar/checkasm/parse-that/generated CSS/bench/gate paths and
  the orphan disposition artifact in `restart/skinny/tranches/sk-v12/SPEC.md:546`
  -`:559`.
- Redress may implement only SPEC owner paths, and any other source path returns
  REVISE before editing (`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:126`
  -`:129`).

Cost read:

The owner sets are still wide because the work crosses codegen, generated
runtime, benchmark, and gate consumers. They are no longer unbounded families:
each high-risk wave has a fixed row, output plane, entry gate, exit gate, revert
protocol, and same-wave consumer requirement. P3-B also advises serializing W3
and W4 after W1b-2/W2 to avoid generated-runtime, bench, and gate-file races
(`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:97`-`:102`).

Required fix: none.

### CH4-4 - Generated-size budget is explicit and gate-consumed

Severity: ACCEPT.

Evidence:

- The SPEC requires generated size tracking: hand LOC, generated LOC, module
  byte size, grammar source size, and O(N) growth guard; overflow blocks until
  traced (`restart/skinny/tranches/sk-v12/SPEC.md:273`-`:275`).
- W1b-1 records generated LOC/module-size/O(N) guard
  (`restart/skinny/tranches/sk-v12/SPEC.md:430`-`:431`).
- W1b-2 consumes generated-size fields in the same-wave gate/report
  (`restart/skinny/tranches/sk-v12/SPEC.md:475`-`:478`).
- P3-D defines required generated-size fields, including generated LOC, module
  bytes, grammar rule count, LOC per rule, regen command, generated diff
  artifact, on-budget status, and O(N) guard status
  (`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:184`
  -`:199`).

Cost read:

Generated output is not hidden inside the hand LOC caps. The packet requires
separate size telemetry and makes growth overflow a blocking condition.

Required fix: none.

### CH4-5 - W3 is implementable as a measured attempt, not hidden full substrate work

Severity: ACCEPT.

Evidence:

- S-P2 says P2-D contributes no current shortlist-ready tape/union primitive and
  any same-tape CSS-local union route is conditional after generated CSS Track 1,
  lightningcss comparator, equality, hot-leaf attribution, REDRESS 96/97/98
  material differential, and CHALLENGE
  (`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:54`
  -`:57`).
- P3-B makes W3 conditional after W1b-2 and mandatory only for FIXPOINT when no
  prior CSS row satisfies ADMIT
  (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:42`-`:48`).
- W3's SPEC entry requires a measured CSS row, fresh profile or same-host
  microbench, CHALLENGE acceptance, and proof that there is no sidecar substrate
  (`restart/skinny/tranches/sk-v12/SPEC.md:511`-`:518`).
- The W3 exit gate allows measured reject or FIXPOINT credit when the attempt is
  materially differentiated and REDRESS records the miss
  (`restart/skinny/tranches/sk-v12/SPEC.md:526`-`:536`).

Cost read:

W3 is not asked to invent a general union substrate under the cap. It is a
single CSS-local attempt, measured or rejected, with sidecar shapes explicitly
falsifying the wave.

Required fix: none.

### CH4-6 - W4 does not hide all-orphan cleanup

Severity: ACCEPT.

Evidence:

- P3-B narrows W4 to at most one row-moving ASM/SIMD consumer and separately
  requires disposition of `bitmap_prefix_xor_64`, `bitmap_next_set_bit`,
  `bulk_emit_positions_64`, `byte_context`, and `cache_hints` by consumption,
  removal, or inventory demotion with evidence
  (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:50`-`:62`).
- The SPEC requires the plan to select at most one primary ASM-gen candidate and
  include a five-row orphan accounting table; non-selected orphans may be
  inventory-demoted only when no behavior source change is needed, and any orphan
  needing production consumption/removal outside the selected primitive blocks
  close or requires a later wave (`restart/skinny/tranches/sk-v12/SPEC.md:568`
  -`:575`).
- W4 tasks require scalar/checkasm refresh, same-wave consumer wiring, CSS/JSON
  measurement, and status for all five production orphans
  (`restart/skinny/tranches/sk-v12/SPEC.md:577`-`:586`).
- P3-C binds the zero-orphan exit table and says production orphans at close
  invalidate FIXPOINT (`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:384`
  -`:402`).
- The SIMD coverage audit supports this cost model: all five orphans are either
  scalar delegates/support-only/inventory-only and marked removable or
  demotable under clean-regen discipline
  (`restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md:34`
  -`:61`).

Cost read:

This directly folds the PIN-V1 CH4 problem. W4 can be a 30-minute redress only
when it is one selected primitive plus evidence-only disposition for
non-selected orphans. If any orphan needs independent production work, the SPEC
does not let W4 silently absorb it; close blocks or a later wave is required.

Required fix: none.

## Notes

- P3-A still summarizes the combined C1 CSS row as `<=620 hand LOC if split
  after C2` (`restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:100`
  -`:102`), while the executable SPEC/DISPATCH allocation is W1b-1 `<=360` plus
  W1b-2 `<=300` (`restart/skinny/tranches/sk-v12/SPEC.md:244`-`:245`). This is
  not a CH4 blocker because SPEC/DISPATCH carry the binding per-wave budgets, but
  a later editorial fold could align the candidate summary to `<=660` total or
  remove the combined C1 number.

## Required Fixes

None.

## PASS/FAIL

CH4 PASS.
