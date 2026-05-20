# SK-V12 S-P3 CH4 Cost/Scope Challenge - Cycle PIN-V4

Pass: S-P3 Synthesis-Plan.
Cycle: PIN-V4.
Lens: CH4 cost, caps, LOC budgets, W1b split adequacy, owner-path breadth,
generated-size budget, redress implementability, W1b-1 fallback wording cost,
and W4 all-orphan cleanup visibility.
Date: 2026-05-20.
Packet under review: commit `471bf53e`.
Verdict: ACCEPT.
CH4: PASS.
Confidence: 95%.

## Disposition

PIN-V4 is cost-safe to confirm. Commit `471bf53e` advances the packet from
PIN-V3 to PIN-V4 without changing the cost contract: the current packet still
binds 20-minute research, 15-minute plan, and 30-minute redress caps; keeps each
wave under explicit hand-LOC budgets; splits CSS L4 into W1b-1 scaffold and
W1b-2 comparator/admission; tracks generated size through gate-consumed fields;
and keeps W4 to one selected ASM-gen consumer plus explicit five-row orphan
accounting.

No required CH4 fixes.

## Findings

### CH4-1 - Phase caps, LOC budgets, and cap halt rules did not regress

Severity: ACCEPT.

Evidence:

- `restart/skinny/tranches/sk-v12/SPEC.md:239`-`:248` gives every wave a
  source/edit LOC budget and a 30-minute redress cap.
- `restart/skinny/tranches/sk-v12/SPEC.md:250`-`:257` binds 20-minute research,
  15-minute plan, 30-minute redress, 0.9x-cap recording, and halt-at-cap
  behavior.
- `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:95`-`:129` repeats the
  20/15/30 phase caps and limits redress to one implementation thread operating
  only inside SPEC owner paths.
- `restart/skinny/tranches/sk-v12/research/p3/p3f-spec-draft.md:64`-`:80`
  mirrors the same wave manifest, LOC caps, and orphan-disposition close rule.

Cost read:

The cap system is enforceable rather than advisory. No PIN-V4 label update
weakened the per-phase or per-wave budget.

Required fix: none.

### CH4-2 - W1b split remains adequate for the 30-minute redress cap

Severity: ACCEPT.

Evidence:

- `restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:33`-`:40`
  states the split rationale: W1b-1 creates generated Track 1 plus independent
  oracle/equality scaffold; W1b-2 adds same-plane lightningcss and admission.
- `restart/skinny/tranches/sk-v12/SPEC.md:387`-`:442` scopes W1b-1 to CSS
  generated Track 1, oracle/Track 2, strict equality, finite Mbps, generated
  size telemetry, and JSON guard state, while explicitly forbidding fallback
  unlock on scaffold failure.
- `restart/skinny/tranches/sk-v12/SPEC.md:447`-`:491` scopes W1b-2 to
  lightningcss extraction, three-way equality, sample-counted throughput, and
  measured CSS redress/fallback consequences.
- `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:202`
  -`:264` keeps W1b-1 from recording CSS ADMIT and reserves the strict
  `generated_css_l4_track1_mbps > lightningcss_mbps + 1` gate for W1b-2.

Cost read:

The prior oversized CSS target remains split into two bounded work slices. A
scaffold miss and a comparator/SOTA miss have separate failure paths.

Required fix: none.

### CH4-3 - Owner-path breadth is broad but bounded

Severity: ACCEPT.

Evidence:

- W1b-1 owner paths are concrete grammar, codegen, generated-runtime, bench,
  gate, fixture, RESULTS, and REDRESS paths in
  `restart/skinny/tranches/sk-v12/SPEC.md:392`-`:406`.
- W1b-2 narrows to the CSS bench/comparator/report/gate surface in
  `restart/skinny/tranches/sk-v12/SPEC.md:452`-`:461`.
- W3 is bounded to tape/codegen/generated-CSS/bench/gate paths with
  no-sidecar entry constraints in `restart/skinny/tranches/sk-v12/SPEC.md:501`
  -`:520`.
- W4 owner paths cover SIMD/scalar/checkasm/parse-that/generated-CSS/bench/gate
  files plus `orphan-disposition.md` in
  `restart/skinny/tranches/sk-v12/SPEC.md:548`-`:561`.
- Dispatch redress may implement only SPEC owner paths; otherwise it returns
  REVISE before editing (`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:124`
  -`:129`).

Cost read:

The owner sets are cross-cutting because generated parsers, runtime, reports,
and gates must move together. The packet bounds them by wave purpose, entry
gate, output plane, and same-wave consumer.

Required fix: none.

### CH4-4 - Generated-size budget is visible and gate-consumed

Severity: ACCEPT.

Evidence:

- `restart/skinny/tranches/sk-v12/SPEC.md:273`-`:275` requires hand LOC,
  generated LOC, module byte size, grammar source size, and an O(N) growth
  guard, with overflow blocking the wave until traced.
- W1b-1 records generated LOC/module-size/O(N) guard
  (`restart/skinny/tranches/sk-v12/SPEC.md:427`-`:431`).
- W1b-2 consumes generated-size fields in the same-wave gate/report
  (`restart/skinny/tranches/sk-v12/SPEC.md:472`-`:480`).
- `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:184`
  -`:198` requires generated LOC, module bytes, grammar rule count, LOC per
  rule, regen command, generated diff artifact, on-budget status, and O(N)
  guard status.

Cost read:

Generated output is named separately from hand LOC but is not hidden. The budget
mechanism is telemetry plus O(N) failure, which is the right shape for generated
CSS code.

Required fix: none.

### CH4-5 - W1b-1 fallback wording has a known, bounded cost impact

Severity: ACCEPT.

Evidence:

- The user pin allows Sheets/BBNF-self only after a CSS L4 redress attempt
  fails, not after preflight failure
  (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18`-`:24`).
- W1b-1 entry blocks Sheets/BBNF-self before a measured CSS redress attempt from
  W1b-2 (`restart/skinny/tranches/sk-v12/SPEC.md:421`-`:422`).
- W1b-1 scaffold failure records REDRESS and returns to plan, but does not
  satisfy fallback unlock; fallback remains blocked until W1b-2 records measured
  lightningcss comparator/admission redress
  (`restart/skinny/tranches/sk-v12/SPEC.md:438`-`:442`).
- W1b-2 is the first wave whose measured comparator/admission redress can enable
  later fallback consideration (`restart/skinny/tranches/sk-v12/SPEC.md:482`
  -`:491`).
- P3-B keeps fallback out of W1b-1/W1b-2 and requires an explicit later fallback
  wave after W1b-2 measured BLOCKED/REJECTED evidence
  (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:104`
  -`:110`).

Cost read:

This can cost an extra S-P3/topology revision if W1b-1 cannot create a
measurable CSS scaffold, but that cost is intentional under the pin. A scaffold
failure is not a measured CSS-vs-lightningcss redress attempt.

Required fix: none.

### CH4-6 - Redress implementability is scoped to measured attempts

Severity: ACCEPT.

Evidence:

- S-P2 convergence limits selectable candidates to rows with antecedents,
  scalar/checkasm or N/A, micro-proof or N/A, same-wave consumer status, and
  orphan disposition
  (`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:45`
  -`:53`).
- S-P2 also records that P2-D has no current shortlist-ready union primitive and
  any same-tape CSS-local route is conditional on CSS Track 1, lightningcss,
  equality, hot-leaf attribution, REDRESS 96/97/98 differential, and CHALLENGE
  (`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:54`
  -`:57`).
- P3-B scopes W3 as conditional and W4 as at most one row-moving ASM/SIMD
  consumer plus orphan disposition
  (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:42`
  -`:62`).
- W3 entry requires a measured CSS row, fresh profile or same-host microbench,
  CHALLENGE acceptance, and proof against sidecar substrate drift
  (`restart/skinny/tranches/sk-v12/SPEC.md:513`-`:520`).
- W4 entry selects at most one primary ASM-gen candidate and requires
  microbench, same-wave consumer, and orphan accounting
  (`restart/skinny/tranches/sk-v12/SPEC.md:563`-`:578`).

Cost read:

W3 and W4 are implementable as bounded attempts. They may reject and still
provide FIXPOINT evidence; neither is asked to exhaust all union or ASM-gen
design space under one redress cap.

Required fix: none.

### CH4-7 - W4 all-orphan cleanup is visible and fail-closed

Severity: ACCEPT.

Evidence:

- USER PIN D5 names the five orphan aarch64 primitives and sets zero orphan
  kernels as the SK-V12 close target
  (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:71`-`:78`).
- P3-B makes W4 mandatory before close, selects at most one row-moving consumer,
  and requires disposition of `bitmap_prefix_xor_64`, `bitmap_next_set_bit`,
  `bulk_emit_positions_64`, `byte_context`, and `cache_hints`
  (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:50`
  -`:62`).
- SPEC W4 requires a five-row orphan accounting table and blocks close or
  requires a later wave if a non-selected orphan needs production consumption or
  removal outside the selected primitive
  (`restart/skinny/tranches/sk-v12/SPEC.md:574`-`:578`).
- SPEC W4 tasks require recording all five production orphans by consumption,
  removal, or inventory demotion with evidence
  (`restart/skinny/tranches/sk-v12/SPEC.md:585`-`:588`).
- The telemetry schema requires the orphan table on close and SIMD/ASM waves;
  `open` fails ADMIT and FIXPOINT close
  (`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:239`
  -`:255`).

Cost read:

W4 is not hiding all-orphan implementation inside one cap. It is one selected
consumer plus visible disposition rows for all carried orphans.

Required fix: none.

### CH4-8 - PIN-V3 accepted facts carry forward without cost/cap regression

Severity: ACCEPT.

Evidence:

- PIN-V3 CH4 was already ACCEPT/PASS with 94% confidence
  (`restart/skinny/tranches/sk-v12/research/p3/hardening/PIN-V3/CONSOLIDATED.md:16`
  -`:23`).
- PIN-V3 load-bearing facts include the folded W1b-1 fallback correction, exact
  CSS row/output-plane/runtime path, W2 SIMD prerequisite, strict CSS ADMIT bar,
  measured FIXPOINT requirements, and zero-orphan requirement
  (`restart/skinny/tranches/sk-v12/research/p3/hardening/PIN-V3/CONSOLIDATED.md:29`
  -`:47`).
- Current PIN-V4 packet files keep those facts in the dispatch load-bearing
  facts and telemetry sections
  (`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:152`-`:183`).

Cost read:

The confirmation cycle is not adding work beyond the already-accepted topology.
It preserves the accepted split/fallback/orphan contract while relabeling the
packet for the required second clean S-P3 cycle.

Required fix: none.

## Required Fixes

None.

## CH4 Result

PASS.
