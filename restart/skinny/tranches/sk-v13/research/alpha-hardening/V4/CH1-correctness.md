# CH1 Correctness - SK-V13 Pass Alpha V4

Date: 2026-05-21.

Verdict: ACCEPT.

Scope: second consecutive clean-cycle CH1 check per `ORCHESTRATOR.md` §3Z.
This pass read the V3 consolidated packet and rechecked the current SK-V13
Alpha packet for regression in line-resolving citations, comparator-plus-one
semantics, B0 row inventory including absent typed rows, CSS close authority,
and `G-SIMD-GRAMMAR-POLICY` evidence.

## V3 Baseline

The V3 consolidated packet recorded the first clean Alpha challenge cycle:
six-of-six ACCEPT, no open CH1 correctness blocker, and an explicit carry-forward
that V4 should re-run against the current packet before Alpha convergence
(`restart/skinny/tranches/sk-v13/research/alpha-hardening/V3/CONSOLIDATED.md:5-29`).

## Regression Checks

### Line-Resolving Citations

Status: ACCEPT.

No regression found. Alpha-F still carries line-resolving source anchors for
PASS-ALPHA, the addendum, SK-V12 close, CSS parity gap, profile truth, decision
engine, value/API union, and SIMD/ASM scope
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-F-contract-draft.md:10-33`).
Alpha-A still resolves the formerly whole-file evidence bullets to specific
profile-truth, value/API union, and SIMD/ASM ranges
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:206-216`).

### Comparator-Plus-One Semantics

Status: ACCEPT.

No regression found. Alpha-B still states the binding target as JSON Track 1
greater than `sonic-rs strict Mbps + 1` or architectural intrinsic-block proof
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-B-competitor-deltas.md:3`)
and applies that strict comparator-plus-one boundary to direct, typed, and
`parse_only` rows
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-B-competitor-deltas.md:38`,
`:62`, `:78`). Alpha-A's B0 table still defines margin as
`Track 1 - (strict comparator + 1)`, with CSS using lightningcss and JSON using
sonic-rs strict
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:88-93`).

### B0 Row Inventory And Absent Typed Rows

Status: ACCEPT.

No regression found. Alpha-A still carries a row-level B0 inventory with state,
strictness, plane, Track 1, Track 2, strict comparator, margin, c/B debt, and
hot-leaf evidence for the rendered JSON/CSS rows
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:95-138`).
The inventory still includes the ten absent `real_typed_struct` rows as
row-level absent-row debt, not omitted aggregate debt
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:139-148`).
Alpha-B independently preserves the same 7/17 typed coverage and names the ten
missing typed corpora
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-B-competitor-deltas.md:20-25`,
`:62-74`).

### CSS Close Authority

Status: ACCEPT.

No regression found. Alpha-A still treats the 24 non-OUT_OF_SCOPE CSS feature
families as the G1 surface, with one admitted declaration-values row and 23
remaining rows that must admit or carry architectural-level intrinsic-block
proof. Measured implementation rejection remains REDRESS evidence, not close
authority
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:150-157`).
Alpha-D still invalidates single-row CSS close and states that no `PARTIAL`
feature or implementation-limited block can close
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-D-validated-invalidated.md:143-153`).

### G-SIMD-GRAMMAR-POLICY Evidence

Status: ACCEPT.

No regression found. Alpha-E still identifies the correctness hazard:
`bbnf-simd` dispatch currently selects by alphabet only and the live aarch64 TBL
path hardcodes JSON quote, escape, and control-threshold constants, so non-JSON
consumers need an explicit grammar-policy gate
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:394-398`).
The packet still makes `G-SIMD-GRAMMAR-POLICY` a prerequisite for wiring
`bbnf-simd` into CSS, union, JSON `parse_only`, or shared generated code, with
grammar-specific policy selection, scalar/checkasm/differential coverage,
same-wave measured-row consumption, no public substrate API/config trait, no
retained sidecar classifier state, and rejection of the current alphabet-only
JSON-constant dispatch for non-JSON consumers
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:420-429`).

## Disposition

CH1 finds no correctness regression in the current SK-V13 Alpha packet. This is
the second consecutive clean CH1 cycle after the V1/V2 revise set and is
consistent with V3's consolidated ACCEPT baseline under `ORCHESTRATOR.md` §3Z.
