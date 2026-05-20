# SK-V12 S-P3 PIN-V4 CH1 Correctness Review

Pass: S-P3 Synthesis-Plan.
Cycle: PIN-V4.
Lens: CH1 correctness.
Packet under review: commit `471bf53e`.
Date: 2026-05-20.

## Disposition

ACCEPT.

Confidence: 95%.

CH1 result: PASS.

## Findings

No required correctness fixes.

1. PIN-V3 fallback correction is preserved without regression. PIN-V3 accepted
   the load-bearing rule that W1b-1 scaffold failure records REDRESS and returns
   to plan, but does not unlock Sheets/BBNF fallback; fallback remains blocked
   until W1b-2 records measured CSS lightningcss comparator/admission redress,
   unless the user re-pins or S-P3 revises topology
   (`restart/skinny/tranches/sk-v12/research/p3/hardening/PIN-V3/CONSOLIDATED.md:31`-`:34`).
   PIN-V4 SPEC now states the same rule in the W1b-1 exit gate: scaffold
   BLOCKED/FAIL records REDRESS, does not satisfy the post-CSS-redress fallback
   condition, and leaves Sheets/BBNF blocked until W1b-2 measured CSS
   lightningcss comparator/admission redress or explicit re-pin/topology
   revision (`restart/skinny/tranches/sk-v12/SPEC.md:438`-`:442`). P3-C and
   P3-E match that boundary by requiring W1b-2 measured evidence before fallback
   consideration (`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:258`-`:264`;
   `restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md:75`-`:76`).

2. The exact CSS row, output plane, and runtime path are consistent. PIN-V3
   accepted `css_l4/declaration_values/direct_to_struct/main`,
   `css_l4_declaration_value_fact_stream`, and
   `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
   (`restart/skinny/tranches/sk-v12/research/p3/hardening/PIN-V3/CONSOLIDATED.md:35`-`:38`).
   PIN-V4 repeats those exact values in SPEC W1b-1 entry and W1b-2 continuity
   (`restart/skinny/tranches/sk-v12/SPEC.md:413`-`:417`,
   `restart/skinny/tranches/sk-v12/SPEC.md:468`-`:470`), Dispatch load-bearing
   facts (`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:155`-`:160`),
   P3-B topology (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:37`-`:40`),
   P3-C gates (`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:207`-`:211`),
   and P3-D schema (`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:102`-`:111`).

3. Gate arithmetic remains strict and pin-correct. The user pin sets the CSS
   admission floor to `lightningcss_mbps + 1` and rescinds
   `ceil(baseline_mbps * 1.01)` (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:29`-`:37`).
   PIN-V4 SPEC requires strict `track1_mbps > lightningcss_mbps + 1`, states
   equality at `+1` is a miss, and rejects old close formulas
   (`restart/skinny/tranches/sk-v12/SPEC.md:40`-`:43`,
   `restart/skinny/tranches/sk-v12/SPEC.md:85`-`:87`,
   `restart/skinny/tranches/sk-v12/SPEC.md:484`-`:487`). P3-A/P3-C/P3-D/P3-E
   repeat the same strict arithmetic and fail the baseline-relative floor
   (`restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:317`-`:328`;
   `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:249`-`:255`;
   `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:116`-`:128`;
   `restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md:121`-`:123`).

4. W2/W1b-1/W1b-2 topology is internally consistent. SPEC orders W2 as Section
   5, W1b-1 as Section 6, and W1b-2 as Section 7, with W1b-1 scalar-only unless
   W2 has passed (`restart/skinny/tranches/sk-v12/SPEC.md:243`-`:245`,
   `restart/skinny/tranches/sk-v12/SPEC.md:351`-`:385`,
   `restart/skinny/tranches/sk-v12/SPEC.md:387`-`:445`,
   `restart/skinny/tranches/sk-v12/SPEC.md:447`-`:494`). Dispatch carries the
   same topology and explicitly permits W1b-1 before W2 only under an accepted
   scalar-only plan (`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:73`-`:83`).
   P3-B mirrors the same dependency graph and fallback order
   (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:79`-`:83`,
   `restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:86`-`:110`).

5. Entry and exit gates are falsifiable rather than paper-close clauses. W1b-1
   entry requires W1a, exact row/plane/runtime, named source/runtime/fixture,
   independent oracle, equality, benchmark, gate, and rollback slice
   (`restart/skinny/tranches/sk-v12/SPEC.md:408`-`:423`). W1b-1 exit requires
   compiled/executed generated Track 1, independent oracle equality, finite Mbps,
   generated-size telemetry, and forbids CSS ADMIT at scaffold time
   (`restart/skinny/tranches/sk-v12/SPEC.md:433`-`:442`). W1b-2 entry requires
   W1b-1 PASS and named lightningcss comparator/version/artifacts, and W1b-2 exit
   splits ADMIT candidate, measured baseline, and BLOCKED/FAIL outcomes
   (`restart/skinny/tranches/sk-v12/SPEC.md:463`-`:491`). The global telemetry
   gate rejects producer-only fields, stale run ids, missing lightningcss,
   missing independent oracle, parse-only admission, generic policy leaks, and
   orphan SIMD primitives (`restart/skinny/tranches/sk-v12/SPEC.md:169`-`:172`;
   `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:171`-`:183`).

6. Close criteria remain correct for ADMIT and FIXPOINT. SPEC Section 0 requires
   CSS L4 generated Track 1, same-plane lightningcss, strict equality,
   gate-consumed provenance, Lock 14, Lock 16 where applicable, zero aarch64
   production orphans, and JSON guard disposition for ADMIT
   (`restart/skinny/tranches/sk-v12/SPEC.md:36`-`:64`). FIXPOINT requires at
   least one measured CSS attempt, no fallback before CSS evidence, a new
   measured union attempt, a new measured ASM-gen attempt, zero orphans, and
   REDRESS routing (`restart/skinny/tranches/sk-v12/SPEC.md:66`-`:83`). W5
   preserves those same requirements and routes rather than paper-closes when
   neither close form holds (`restart/skinny/tranches/sk-v12/SPEC.md:616`-`:637`;
   `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:411`-`:450`).

7. Section numbering and dispatch references are consistent. The dispatch
   manifest maps W0/W1a/W2/W1b-1/W1b-2/W3/W4/W5 to SPEC Sections
   3/4/5/6/7/8/9/10 (`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:69`-`:78`),
   and SPEC defines those sections at the matching headings
   (`restart/skinny/tranches/sk-v12/SPEC.md:277`,
   `restart/skinny/tranches/sk-v12/SPEC.md:314`,
   `restart/skinny/tranches/sk-v12/SPEC.md:351`,
   `restart/skinny/tranches/sk-v12/SPEC.md:387`,
   `restart/skinny/tranches/sk-v12/SPEC.md:447`,
   `restart/skinny/tranches/sk-v12/SPEC.md:496`,
   `restart/skinny/tranches/sk-v12/SPEC.md:543`,
   `restart/skinny/tranches/sk-v12/SPEC.md:604`). The PIN-V4 commit changes
   labels from PIN-V3 to PIN-V4 and does not introduce a new section map.

## Required Fixes

None.

## Verdict

CH1 PASS. PIN-V4 is a correctness-preserving confirmation of the accepted
PIN-V3 S-P3 contract at commit `471bf53e`; no W1b fallback, CSS row/output
plane/runtime path, topology, arithmetic, entry/exit gate, section-numbering, or
falsifiability regression was found.
