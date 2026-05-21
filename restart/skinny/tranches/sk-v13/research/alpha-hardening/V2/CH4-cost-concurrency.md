# CH4 - Cost / Concurrency Review for SK-V13 Pass Alpha V2

Role: Alpha CH4 COST/CONCURRENCY challenge for SK-V13 Alpha V2.
Disposition: **ACCEPT**.

## Scope Read

- `PASS-ALPHA.md` makes Alpha-E responsible for candidate file paths, scalar
  and checkasm status, same-wave consumer plan, falsifiability gate, LOC budget,
  and risk classification; CH4 reviews LOC budget, risk class, wave alignment,
  hard-cap realism, and same-wave consumers.
- The 2026-05-21 addendum controls conflicts: 24 CSS parity features with 23
  remaining after SK-V12, 51 JSON rows, `parse_only` admission eligibility,
  no support-only landings, aggressive parallelism only for non-overlapping file
  domains, and 45-minute redress caps only for W5-W9 and W12 union-SIMD.
- V1 CH4 required three folds: recompute CSS LOC under the full 23-feature
  scope, assign hard caps per fanout, and add dependency/conflict constraints
  for parallelism and RESULTS/REDRESS serialization.

## Verdict

**ACCEPT for CH4.** The V2 Alpha packet is cost-realistic enough to proceed to
S-P1/S-P3 planning. It no longer hides the full CSS cost behind a compressed
3k-5k estimate, it assigns hard caps per family, and it adds a dependency and
conflict matrix that prevents unsafe parallel writes. S-P3 still must carry the
clarifications below into `SPEC.md`, but they are planning requirements rather
than Alpha-blocking defects.

## Findings

### F1 - CSS LOC Arithmetic Is Corrected

Disposition: **ACCEPT**.

Alpha-E now gives E1 an `8.0k-21.9k` source/test upper envelope across the
one-wave-per-feature CSS fanout, with generated LOC accounted separately. That
matches the stated `350-950 LOC per CSS feature family` over 23 remaining
features: `23 * 350 = 8,050` LOC and `23 * 950 = 21,850` LOC.

This fixes the V1 contradiction. The V2 text also preserves the addendum's
per-feature W10 shape: W3/W4 may cover foundation, but W10.N remains one wave
per non-OUT_OF_SCOPE CSS feature unless S-P3 proves one measured row/gate covers
an explicit bundle.

S-P3 carry-forward: every bundled CSS feature set must name the exact parity
matrix rows it covers and must keep a single same-plane lightningcss gate for
that bundle. Otherwise use separate W10.N waves.

### F2 - Candidate LOC Envelopes Are Plausible, With One Labeling Cleanup

Disposition: **ACCEPT with S-P3 cleanup**.

The non-CSS arithmetic is internally plausible:

| Family | V2 envelope | Arithmetic check |
|---|---:|---|
| E2 value/config/sink | 1.5k-2.1k | `600-900 + 900-1200` |
| E3 decision engine | 2.3k-3.6k | `210-330 + 850-1250 + 500-800 + 790-1210` |
| E5 SIMD/ASM | 800-1.6k | selected kernels plus tests, with subitems totaling about 1.1k before variance |

E4 is acceptable but should be labeled more tightly in S-P3. Alpha-E lists E4
as `550-1.4k`; its subvariants are C1 `150-250`, C2 `400-800`, and C3 `430`
plus E5 test cost. That is not a blocker because S-P3 will select variants, but
the eventual SPEC should state whether E4's budget is per selected variant, a
C1+C2 aggregate, or a C3+E5 coupled tranche.

### F3 - Hard Caps Are Now Explicit And Match The Addendum

Disposition: **ACCEPT**.

Alpha-E now includes a hard-cap table:

- E1, E2, E4 C1/C2, and ordinary E5 waves: 20 min research, 15 min plan,
  30 min redress.
- E3 W5-W9: 20 / 15 / 45.
- E4 C3 / W12 union-SIMD: 20 / 15 / 45.

This matches the addendum: research and plan caps remain 20/15, and the
45-minute redress amendment applies only to W5-W9 and W12 union-SIMD. The table
also correctly says W5 cannot be close-bearing infrastructure by itself.

S-P3 carry-forward: if a W11 or W14 fanout consumes E3/E5 outputs but is not
itself W5-W9 or W12 union-SIMD, it stays at the ordinary 30-minute redress cap
unless the user explicitly amends the cap.

### F4 - Dependency And Conflict Matrix Is Sufficient For S-P3

Disposition: **ACCEPT**.

V2 adds the missing concurrency matrix. It serializes:

- E1 CSS feature waves against shared CSS tokenizer/fact-stream schema edits,
  E2 prerequisite edits, and RESULTS/REDRESS writes.
- E2 against most E1/E4 behavior waves until the exact policy surface is stable.
- E3 W5-W9 internally unless S-P3 proves disjoint owner paths.
- E4 against E2 policy tables, E3-selected shape ownership, and public
  substrate-adjacent files.
- E5 against shared `bbnf-simd` dispatch, checkasm reports, and ledger writes.

This is enough for S-P3 to plan aggressive worktree fanout without pretending
that E2/E3/E4/E5 are freely parallel. The handoff also keeps G-Omega as a
pre-W0 gate and explicitly serializes `skinny/RESULTS.md` and
`skinny/REDRESS.md` appends.

S-P3 carry-forward: parallel redress worktrees may prepare measurements at the
same time, but the authoritative RESULTS/REDRESS append phase needs a single
writer and a deterministic row order.

### F5 - RESULTS / REDRESS Serialization Is Not Hidden

Disposition: **ACCEPT**.

The Alpha packet and handoff now state the important serialization rule plainly:
G-Omega closes before source implementation waves, and RESULTS/REDRESS are
single-writer ledgers even if implementation worktrees run in parallel. That
prevents a common false concurrency claim: row-moving waves can execute in
parallel, but campaign authority is recorded serially.

S-P3 carry-forward: SPEC should include a short ledger protocol for each wave:
measurement artifact path, proposed RESULTS row(s), proposed REDRESS entry, and
the order in which those rows append after parallel worktrees converge.

### F6 - S-P1 / S-P3 Can Proceed Without Hiding Overlarge Waves

Disposition: **ACCEPT**.

The current packet is explicit that:

- S-P1 must refresh profile truth for CSS and all JSON planes before row plans
  rely on hot leaves.
- S-P3 must author `SPEC.md` and `DISPATCH-PROMPT.md`; Alpha does not pretend
  the wave plan already exists.
- W10.N, W11.N, W14.N, W5-W9, W8/W12, and E5 consumers remain visible fanouts,
  not buried inside five Alpha-E candidate names.
- Support-only infrastructure is invalid unless bound to same-tranche row
  movement or intrinsic-block evidence.

That is enough for S-P1/S-P3 to proceed under the addendum. The remaining work
is ordinary S-P3 decomposition: attach exact owner paths, row gates, revert
protocols, and ledger serialization to each wave.

## Required S-P3 Fixes

These are carry-forward requirements, not Alpha V2 blockers:

1. Add a SPEC cost table that converts each Alpha-E family into concrete W3/W4,
   W10.N, W11.N, W14.N, W5-W9, W8/W12, and E5 wave budgets.
2. For CSS, list exact feature rows for any bundle; otherwise preserve one
   W10.N per feature.
3. Label E4 budgets per selected variant versus aggregate variant tranche.
4. Keep W11/W14 redress at 30 minutes unless the wave is explicitly part of
   W5-W9 or W12 union-SIMD.
5. Add the single-writer RESULTS/REDRESS append protocol to SPEC, including
   deterministic row order after parallel worktrees converge.

## Final CH4 Disposition

**ACCEPT.** V2 fixes the V1 CH4 blockers: CSS LOC arithmetic is realistic,
hard caps are explicit, dependency/concurrency rules are present, and
RESULTS/REDRESS serialization is visible. The packet can advance to S-P1/S-P3
without concealing overlarge waves, provided S-P3 carries the five concrete
planning requirements above into `SPEC.md`.
