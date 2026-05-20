# SK-V12 S-P3 PIN-V3 CH1 Correctness Review

Pass: S-P3 Synthesis-Plan.
Cycle: PIN-V3.
Lens: CH1 correctness and gate consistency.
Date: 2026-05-20.
Packet under review: commit `4c53119f`.

Disposition: ACCEPT.
Confidence: 94%.

## Scope

Reviewed `SPEC.md`, `DISPATCH-PROMPT.md`, P3-A through P3-F,
`USER-PIN-W1-CSS-L4-SOTA.md`, `HANDOFF.md`, the PIN-V2 CH1 failure and
consolidated report, and the converged S-P1/S-P2 hardening summaries. Focus was
limited to the folded PIN-V2 CH1 issue, exact CSS row/output plane/runtime path,
W2/W1b-1/W1b-2 topology, gate arithmetic, entry/exit consistency, section
numbering, and falsifiability.

## Findings

### CH1-1 - PIN-V2 fallback ambiguity is folded

PASS. The blocking PIN-V2 issue was that W1b-1 scaffold failure could be read as
the CSS redress event that unlocks Sheets/BBNF-self fallback
(`research/p3/hardening/PIN-V2/CH1.md:22-43`,
`research/p3/hardening/PIN-V2/CONSOLIDATED.md:28-47`). PIN-V3 fixes the
authoritative SPEC text: W1b-1 BLOCKED/FAIL records REDRESS and returns to plan,
but "does not satisfy the post-CSS-redress fallback condition"; fallback remains
blocked until W1b-2 records measured CSS lightningcss comparator/admission
redress, unless the user re-pins or S-P3 revises topology (`SPEC.md:438-442`).

The rest of the packet agrees. W1b-1 entry bars Sheets/BBNF-self before a
measured CSS redress attempt from W1b-2 (`SPEC.md:421-422`), W1b-2 BLOCKED/FAIL
requires a subsequent plan revision after "this measured CSS redress attempt"
(`SPEC.md:488-491`), P3-B says fallback may enter only after W1b-2 records
BLOCKED or REJECTED (`research/p3/p3b-wave-sequencing.md:104-110`), P3-C
records W1b-2 REDRESS with no hidden same-redress fallback and allows
Sheets/BBNF-self only after that CSS L4 redress attempt
(`research/p3/p3c-falsifiability-gates.md:258-264`), and P3-E says only W1b-2
may record BLOCKED/REJECTED for later fallback routing
(`research/p3/p3e-preblocked-ledger.md:75-76`). This matches the user pin's
"not after preflight failure" rule (`USER-PIN-W1-CSS-L4-SOTA.md:18-24`) and the
S-P1/S-P2 converged hardening facts (`research/p1/hardening/HARDENING-S-P1-CONVERGED.md:55-59`,
`research/p2/hardening/HARDENING-S-P2-CONVERGED.md:38-42`).

### CH1-2 - CSS row, output plane, and runtime path are exact and stable

PASS. The packet consistently names the selected row as
`css_l4/declaration_values/direct_to_struct/main`, the output plane as
`css_l4_declaration_value_fact_stream`, and the generated runtime path as
`skinny/crates/runtime/src/grammars/css_l4_declaration_values/` (`SPEC.md:413-417`,
`DISPATCH-PROMPT.md:155-158`, `research/p3/p3b-wave-sequencing.md:37-40`,
`research/p3/p3c-falsifiability-gates.md:207-211`). W1b-2 preserves the same row
and output plane for the comparator/admission gate (`SPEC.md:468-470`,
`research/p3/p3c-falsifiability-gates.md:234-239`).

### CH1-3 - W2/W1b-1/W1b-2 topology is coherent

PASS. The wave manifest puts W2 after W1a and before SIMD/ASM admission, W1b-1
after W1a with a scalar-only exception before W2, and W1b-2 after W1b-1
(`SPEC.md:241-245`, `DISPATCH-PROMPT.md:71-75`). The detailed gates carry the
same rule: W1b-1 requires W2 PASS unless the plan proves scalar-only and touches
no SIMD/ASM-backed helper (`SPEC.md:408-423`), while W1b-2 requires W1b-1 PASS
(`SPEC.md:463-470`). P3-B's topology restates the minimum path and scalar-only
exception without changing the dependency (`research/p3/p3b-wave-sequencing.md:86-95`).

### CH1-4 - Admission arithmetic is strict and non-stale

PASS. The packet consistently uses strict CSS admission arithmetic:
`track1_mbps > lightningcss_mbps + 1`; equality at `+1` is a miss
(`USER-PIN-W1-CSS-L4-SOTA.md:29-35`, `SPEC.md:40-43`, `SPEC.md:482-491`,
`research/p3/p3c-falsifiability-gates.md:241-255`,
`research/p3/p3d-telemetry-schema.md:116-128`). The old `>= 1 Mbps` and
`ceil(baseline_mbps * 1.01)` close bars are explicitly rejected
(`SPEC.md:85-87`, `DISPATCH-PROMPT.md:187-192`).

### CH1-5 - Section numbering and falsifiability are acceptable

PASS. The authoritative SPEC and dispatch prompt align W0 through W5 against
Sections 3 through 10 (`SPEC.md:237-248`, `DISPATCH-PROMPT.md:67-78`). P3-C
orders its detailed subsections as W1b-1/W1b-2 before W2
(`research/p3/p3c-falsifiability-gates.md:202-271`), but the gate IDs and entry
rules are unambiguous and do not contradict the manifest.

The plan remains falsifiable. Missing same-plane lightningcss evidence, missing
independent oracle, stale run id, producer-only telemetry, unsupported outcome,
generic policy leak, parse-only admission, unresolved `escape_mask_64`, or open
production aarch64 orphan fails closed (`SPEC.md:169-172`,
`research/p3/p3d-telemetry-schema.md:257-295`,
`research/p3/p3c-falsifiability-gates.md:440-450`).

## Required Fixes

None.

PASS for CH1.
