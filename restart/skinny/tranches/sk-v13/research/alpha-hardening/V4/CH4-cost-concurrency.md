# CH4 - Cost / Concurrency Confirmation for SK-V13 Alpha V4

Role: Alpha CH4 cost/concurrency confirmation for SK-V13 Alpha V4.
Disposition: **ACCEPT**.

## Scope Read

Inputs read:

- `restart/skinny/tranches/sk-v13/research/alpha-hardening/V3/CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v13/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v13/HANDOFF.md`

V3 consolidated records CH4 ACCEPT and states the cost-relevant fold directly:
the SIMD grammar-policy gate has dependency cost, but is bound to consuming
E5/E4-C3 waves; CSS LOC, hard caps, conflict matrix, and ledger serialization
hold. This V4 pass rechecks those claims for cost, cap, dependency, and
parallelism sufficiency.

## Verdict

**ACCEPT.** No V4 CH4 blocker found. The packet remains sufficient for
S-P1/S-P3 planning, provided SPEC carries the existing row-local accounting
rules forward instead of converting them into support-only or bundled
mega-waves.

## Confirmation Findings

### F1 - CSS LOC Arithmetic Remains Sufficient

Disposition: **ACCEPT**.

Alpha-E still scopes E1 as 23 remaining non-OUT_OF_SCOPE CSS L4 parity
features with an `8.0k-21.9k` source/test upper envelope and generated LOC
separately accounted. The arithmetic remains sound: `23 * 350 = 8,050` and
`23 * 950 = 21,850`.

This is not hidden as one CSS close. Alpha-E keeps W10.N as one wave per
remaining CSS parity feature unless S-P3 proves a bundle under one measured
same-plane gate. SYNTHESIS and HANDOFF also reject treating the single SK-V12
declaration-values admission as full CSS parity or leaving any CSS feature
`PARTIAL` at close.

CH4 conclusion: CSS LOC is large but explicit, row-fanned, and sufficiently
bounded for planning.

### F2 - Hard Caps Still Hold

Disposition: **ACCEPT**.

Alpha-E's hard caps remain concrete:

- E1 CSS, E2 policy/value/sink, E4 C1/C2, and ordinary E5 SIMD rows:
  `20 min` research, `15 min` plan, `30 min` redress.
- E3 W5-W9 decision fold and E4 C3 / W12 union-SIMD:
  `20 min` research, `15 min` plan, `45 min` redress.

The SIMD grammar-policy gate does not require a wider cap. Its work is charged
inside the consuming E5 wave, or inside E4 C3 only when the selected
union-SIMD variant consumes it. W11/W14 row work remains under the ordinary
30-minute redress cap unless it is explicitly part of the approved W12
union-SIMD amendment.

CH4 conclusion: hard caps remain sufficient and do not mask an uncapped policy
tranche.

### F3 - SIMD Policy Gate Cost And Dependency Impact Are Accounted

Disposition: **ACCEPT**.

Alpha-E identifies the dependency risk precisely: the live `bbnf-simd`
classifier path has JSON-specific quote, escape, and control constants, so
CSS, union, JSON `parse_only`, or shared generated consumers need an explicit
grammar-policy gate.

The packet carries that requirement consistently. `G-SIMD-GRAMMAR-POLICY` is a
prerequisite only when a wave wires `bbnf-simd` into CSS, union, JSON
`parse_only`, or shared generated code. It must prove the consuming grammar's
quote/escape/control policy or a no-string policy, scalar parity,
checkasm/differential coverage, same-wave measured row consumption, no public
substrate API, and no retained sidecar classifier state.

This adds real dependency cost and narrows parallelism around shared SIMD
dispatch, policy tables, and checkasm artifacts. It does not create a hidden
support-only wave because Alpha-E, SYNTHESIS, HANDOFF, and V3 consolidated all
bind the gate to a consuming row.

CH4 conclusion: the SIMD policy gate cap/dependency impact remains sufficient
and visible.

### F4 - Conflict Matrix Remains Sufficient

Disposition: **ACCEPT**.

Alpha-E's conflict matrix still serializes the correct hotspots:

- E1 CSS waves may parallelize only when runtime, codegen, comparator
  artifacts, and gates are disjoint.
- E2 policy/value/sink work serializes with most E1/E4 behavior waves until
  the consumed policy surface is stable.
- E3 W5-W9 serializes internally unless S-P3 proves disjoint owner paths.
- E4 union serializes with E2 policy tables, E3-selected shape ownership, and
  public substrate-adjacent files.
- E5 SIMD/ASM serializes shared `bbnf-simd` dispatch, checkasm reports, and
  RESULTS/REDRESS writes.

The V3 SIMD-policy fold lands exactly inside those serialized surfaces. S-P3
must still name owner paths, but CH4 does not need a new conflict class.

CH4 conclusion: the conflict matrix remains sufficient for safe concurrency.

### F5 - RESULTS / REDRESS Serialization Remains Sufficient

Disposition: **ACCEPT**.

HANDOFF blocks pre-G-Omega source, generated runtime, gate/report,
`skinny/RESULTS.md`, and `skinny/REDRESS.md` edits. After G-Omega/S-P3 it
allows parallel work only for non-overlapping file domains and requires any
phase that appends RESULTS or REDRESS to serialize. Alpha-E repeats that
RESULTS and REDRESS are single-writer ledgers even when redress worktrees run
in parallel.

The SIMD policy fold strengthens the need for deterministic append order but
does not change the mechanism. Parallel workers may prepare artifacts and
proposed rows; authoritative ledger mutation remains single-writer after
conflict review.

CH4 conclusion: RESULTS/REDRESS serialization remains sufficient.

## Required Carry-Forward

These are not V4 blockers:

1. Charge `G-SIMD-GRAMMAR-POLICY` to the consuming E5 or E4-C3 wave.
2. Keep CSS W10.N rows explicit unless S-P3 proves an exact measured bundle.
3. Preserve the 45-minute redress cap only for E3 W5-W9 and E4 C3 / W12
   union-SIMD.
4. Require owner-path notes for shared `bbnf-simd`, classifier policy,
   checkasm, generated runtime, gate/report, RESULTS, and REDRESS paths.
5. Keep RESULTS and REDRESS as single-writer ledgers after parallel worktrees
   converge.

## Final CH4 Disposition

**ACCEPT.** CSS LOC, hard caps, SIMD policy gate cap/dependency impact,
conflict matrix, and RESULTS/REDRESS serialization remain sufficient for
SK-V13 Alpha V4.
