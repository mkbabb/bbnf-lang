# SK-V12 S-P3 PIN-V2 CH1 Correctness Review

Pass: S-P3 Synthesis-Plan.
Cycle: PIN-V2.
Lens: CH1 correctness and gate consistency.
Date: 2026-05-20.
Packet under review: commit `7316d87b`.

Disposition: REVISE.
Confidence: 88%.

## Scope

Reviewed `SPEC.md`, `DISPATCH-PROMPT.md`, P3-A through P3-F,
`USER-PIN-W1-CSS-L4-SOTA.md`, `HANDOFF.md`, `SYNTHESIS.md`, and the pin-aware
S-P1/S-P2 converged hardening summaries. Focus was limited to exact CSS row,
output plane, runtime path, W2/W1b topology, admission arithmetic, entry/exit
consistency, section numbering, and falsifiability.

## Findings

### CH1-1 - W1b-1 can be read as unlocking fallback before W1b-2 measured CSS redress

`SPEC.md` correctly defines W1b-1 as the CSS L4 generated Track 1 plus
independent-oracle scaffold, explicitly without the lightningcss throughput gate
(`SPEC.md:387-390`). Its exit gate then says that if CSS cannot be generated or
measured inside W1b-1, Sheets/BBNF fallback requires a subsequent S-P3 or wave
plan revision "after this measured CSS redress attempt" (`SPEC.md:433-440`).

That is a correctness drift. The user pin allows Sheets/BBNF-self only after a
CSS L4 redress attempt fails, "not after preflight failure"
(`USER-PIN-W1-CSS-L4-SOTA.md:18-24`). The rest of the PIN-V2 packet correctly
keys fallback eligibility to W1b-2's measured CSS comparator/admission attempt:
P3-B says fallback can enter only after W1b-2 records a measured CSS L4 redress
attempt as BLOCKED or REJECTED (`p3b-wave-sequencing.md:104-110`), and P3-C
requires W1b-2 REDRESS before Sheets/BBNF-self are considered
(`p3c-falsifiability-gates.md:258-264`).

Required fix: revise the W1b-1 BLOCKED/FAIL text so W1b-1 scaffold failure
records REDRESS but does not itself satisfy the post-CSS-redress fallback
condition. Fallback eligibility should begin only after W1b-2 records measured
CSS lightningcss comparator/admission redress, unless the user re-pins or S-P3
explicitly revises the topology.

## Correctness Checks That Passed

- Exact CSS row, output plane, and generated runtime path are now consistent:
  `css_l4/declaration_values/direct_to_struct/main`,
  `css_l4_declaration_value_fact_stream`, and
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
  (`SPEC.md:413-417`, `DISPATCH-PROMPT.md:155-158`,
  `p3b-wave-sequencing.md:37-40`, `p3c-falsifiability-gates.md:207-211`).
- W2/W1b topology is coherent: W2 follows W1a and precedes SIMD/ASM admission;
  W1b-1 may proceed before W2 only under a scalar-only/no-SIMD proof; W1b-2
  follows W1b-1 (`SPEC.md:241-245`, `SPEC.md:408-423`,
  `DISPATCH-PROMPT.md:80-83`, `p3b-wave-sequencing.md:86-95`).
- Gate arithmetic is consistently strict:
  `track1_mbps > lightningcss_mbps + 1`; equality at `+1` is a miss
  (`USER-PIN-W1-CSS-L4-SOTA.md:29-35`, `SPEC.md:40-43`,
  `SPEC.md:480-485`, `p3c-falsifiability-gates.md:249-254`,
  `p3d-telemetry-schema.md:116-128`).
- Authoritative SPEC/DISPATCH section numbering matches the wave manifest from
  W0 through W5 (`SPEC.md:237-248`, `DISPATCH-PROMPT.md:67-78`). P3-C orders
  its detailed gate subsections as W1b-1/W1b-2 before W2
  (`p3c-falsifiability-gates.md:202-271`), but the gate IDs are unambiguous and
  the dispatch topology is carried elsewhere; this is not a blocking numbering
  defect.
- The plan is falsifiable: missing same-plane lightningcss evidence, missing
  independent oracle, stale run id, producer-only telemetry, unsupported outcome,
  generic policy leak, parse-only admission, unresolved `escape_mask_64`, or open
  production aarch64 orphan fails closed (`SPEC.md:169-172`,
  `p3d-telemetry-schema.md:257-295`).

## Required Fixes

1. Fix `SPEC.md` W1b-1 exit wording so scaffold failure is not named as the
   measured CSS redress attempt that unlocks fallback. Keep fallback keyed to
   W1b-2 measured CSS lightningcss comparator/admission redress, matching P3-B
   and P3-C.

FAIL for CH1.
