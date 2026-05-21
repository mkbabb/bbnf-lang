# CH4 - Cost / Concurrency Review for SK-V13 Pass Alpha V3

Role: Alpha CH4 cost/concurrency challenge for SK-V13 Alpha V3.
Disposition: **ACCEPT**.

## Scope Read

- V2 CH4 accepted the corrected CSS LOC arithmetic, hard caps, dependency /
  conflict matrix, and RESULTS/REDRESS serialization, with S-P3 carry-forward
  requirements for exact row budgets and ledger protocol
  (`restart/skinny/tranches/sk-v13/research/alpha-hardening/V2/CH4-cost-concurrency.md:31-162`).
- V2 consolidated required a V3 fold for `G-SIMD-GRAMMAR-POLICY` and required
  CH4 carry-forward around bundled CSS rows, E4 variant LOC, W11/W14 row-local
  caps, non-parallel ledger writes, and per-wave ownership/conflict notes
  (`restart/skinny/tranches/sk-v13/research/alpha-hardening/V2/CONSOLIDATED.md:30-40`).
- This V3 recheck reads Alpha-E, SYNTHESIS, and HANDOFF after the fold, with
  specific focus on whether the new SIMD grammar-policy gate changes cost,
  dependency ordering, CSS LOC realism, hard caps, conflict matrix adequacy,
  or RESULTS/REDRESS serialization.

## Verdict

**ACCEPT for CH4.** The V3 fold adds a real dependency gate for SIMD consumers
without hiding an overlarge implementation wave. `G-SIMD-GRAMMAR-POLICY` is
bound to waves that already wire `bbnf-simd` into CSS, union, JSON
`parse_only`, or shared generated code; it is not presented as a new
standalone support wave. The cost surface remains realistic enough for S-P1 /
S-P3 planning, provided S-P3 preserves row-local budgets and accounts for the
policy work inside the consuming E5 or E4-C3 wave.

## Findings

### F1 - SIMD Grammar Policy Has Real Cap And Dependency Impact

Disposition: **ACCEPT with S-P3 accounting requirement**.

The V3 policy is not cosmetic. Alpha-E identifies the current risk precisely:
`bbnf-simd` classifier dispatch selects by alphabet only while the live
aarch64 TBL path hardcodes JSON quote, escape, and control constants; non-JSON
consumers therefore need an explicit grammar-policy gate
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:389-398`).

The added gate is scoped to concrete consumers. It is prerequisite only when a
wave wires `bbnf-simd` into CSS, union, JSON `parse_only`, or shared generated
code, and it requires the selected classifier to use the consuming grammar's
quote/escape/control policy or a no-string policy, plus scalar parity,
checkasm/differential coverage for JSON, CSS, and delimiter/no-string policies,
same-wave row consumption, no public substrate API, and no retained sidecar
classifier state
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:420-429`).
SYNTHESIS and HANDOFF carry the same rule into S-P3 constraints and refusal
conditions
(`restart/skinny/tranches/sk-v13/SYNTHESIS.md:226-231`,
`restart/skinny/tranches/sk-v13/HANDOFF.md:155-159`).

CH4 impact: this increases E5 and E4-C3 coordination cost and reduces
parallelism around shared `bbnf-simd` dispatch/checkasm files. It does not
create a sixth candidate or a hidden support-only tranche because Alpha-E keeps
the policy row-bound: CSS and JSON SIMD consumers are named, W11/W14 fanout
rows must bind reopened JSON or `parse_only` rows, and a parity pass with no
row movement must remove or demote the primitive
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:404-433`).

S-P3 must still charge the policy work to the consuming SIMD wave. If a CSS
string/identifier SIMD row needs policy tables, scalar references, and expanded
checkasm, that cost belongs in that W10/E5 wave's owner-path budget; it must
not be split into a support-only policy wave.

### F2 - CSS LOC Arithmetic Still Does Not Hide The Full Parity Wave

Disposition: **ACCEPT**.

The CSS scoping matrix records 1 admitted CSS parity row, 7 partial rows, 16
missing rows, and 6 out-of-scope rows
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:127-132`).
Alpha-E carries this as 23 remaining CSS L4 parity features and gives E1 an
`8.0k-21.9k` source/test upper envelope, generated LOC separately accounted
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:50-56`,
`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:114-119`).

That arithmetic remains correct: `23 * 350 = 8,050` LOC and
`23 * 950 = 21,850` LOC. The underlying scoping estimates support the per-row
range: stylesheet/selectors `350-500`, vars/calc/colors `600-840`, visual
functions `700-950`, at-rules/media `550-800`, nesting `380-600`, and
vendor/custom at-rules `350-550`
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:140-224`).

V3 does not collapse those rows into a vague CSS mega-wave. Alpha-E keeps
W10.N as one wave per non-OUT_OF_SCOPE CSS parity feature unless S-P3 proves
one measured gate covers a bundle
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:80-86`,
`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:96-112`).
The required S-P3 control remains: any bundle must list exact parity matrix
rows and one same-plane lightningcss gate.

### F3 - Hard Caps Remain Realistic After The V3 Fold

Disposition: **ACCEPT**.

Alpha-E's hard-cap table remains explicit: E1, E2, E4 C1/C2, and ordinary E5
waves are `20 min / 15 min / 30 min`; E3 W5-W9 and E4 C3 / W12 union-SIMD get
the `45 min` redress amendment
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:459-470`).
The V3 grammar-policy gate does not require broadening those caps.

The practical constraint is placement. `G-SIMD-GRAMMAR-POLICY` should consume
ordinary E5 cap when it is part of W4b/W11/W14 SIMD work, and the 45-minute
redress cap only when it is part of E4 C3 / W12 union-SIMD. A W11 or W14 row
that merely consumes E3/E5 output remains an ordinary 30-minute redress wave
unless the user amends the cap.

### F4 - Conflict Matrix Correctly Serializes The New Policy Hotspots

Disposition: **ACCEPT**.

Alpha-E's conflict matrix already serializes E5 against shared `bbnf-simd`
dispatch, checkasm reports, and RESULTS/REDRESS writes
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:472-483`).
That is exactly where `G-SIMD-GRAMMAR-POLICY` lands: the policy touches shared
dispatch semantics, checkasm/differential coverage, and row-consuming CSS/JSON
runtime paths. It therefore narrows safe parallelism but does not invalidate
the matrix.

The broader dependency ordering is still visible: E2 provides grammar-neutral
consumer policy for E5, E3 may select row/kernel pairings, and E4 C3 depends on
E5
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:445-449`).
SYNTHESIS also keeps independent waves parallel only when file domains do not
overlap and required gates have closed
(`restart/skinny/tranches/sk-v13/SYNTHESIS.md:206-215`).

S-P3 must represent grammar-policy owner paths explicitly. Parallel CSS rows
may proceed only when they do not share classifier policy tables, dispatch
modules, checkasm artifacts, generated runtime modules, gate/report rows, or
ledger appends.

### F5 - RESULTS / REDRESS Serialization Remains Explicit

Disposition: **ACCEPT**.

The handoff blocks source, generated runtime, gate/report, RESULTS, and
REDRESS edits before G-Omega
(`restart/skinny/tranches/sk-v13/HANDOFF.md:78-91`). After G-Omega and S-P3,
it permits parallel waves only for non-overlapping file domains and requires
redress phases that append `skinny/RESULTS.md` or `skinny/REDRESS.md` to
serialize
(`restart/skinny/tranches/sk-v13/HANDOFF.md:93-106`).
Alpha-E repeats that RESULTS and REDRESS are single-writer ledgers even when
redress worktrees run in parallel
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:482-483`).

The V3 SIMD policy does not weaken this. In fact, because policy coverage can
affect JSON, CSS, union, and shared generated rows, it makes deterministic
ledger order more important: each parallel worktree may prepare artifacts, but
authoritative RESULTS rows and REDRESS entries need one append order after
conflict review.

### F6 - No Overlarge Wave Is Hidden In The Five-Family Shortlist

Disposition: **ACCEPT**.

The five Alpha-E families remain a shortlist for planning, not a claim that
each family is one implementation wave. Alpha-E explicitly allows fanout into
W10/W11/W14 subwaves under the addendum
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:21-28`).
It also names the visible fanouts for CSS, decision engine, union, and SIMD:
W10.N CSS, W5-W9 decision fold, W11 residual JSON, W14 `parse_only`, W8/W12
union, and W4b/W11/W14 SIMD consumers
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:80-86`,
`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:218-227`,
`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:302-346`,
`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:379-413`).

SYNTHESIS independently requires W10-style CSS expansion, W11-style JSON
residual reopening, and W14-style `parse_only` admissions unless S-P3 proves a
different sequence covers all rows
(`restart/skinny/tranches/sk-v13/SYNTHESIS.md:213-215`). Therefore CH4 does
not see a concealed mega-wave. The remaining decomposition burden is ordinary
S-P3 work: concrete owner paths, row gates, caps, rollback criteria, and ledger
order per wave.

## Required S-P3 Carry-Forward

These are not Alpha V3 blockers, but SPEC must carry them:

1. Charge `G-SIMD-GRAMMAR-POLICY` to the consuming E5 or E4-C3 wave; do not
   create a support-only grammar-policy wave.
2. Keep CSS bundles explicit: list exact parity rows and the one same-plane
   lightningcss gate, or preserve one W10.N per feature.
3. Label E4 budgets per selected C1/C2/C3 variant and keep C3 coupled to the
   E5 policy/checkasm cost it consumes.
4. Keep W11/W14 redress at 30 minutes unless the wave is explicitly W5-W9 or
   W12 union-SIMD under the user-approved 45-minute amendment.
5. Add a ledger protocol: artifact paths, proposed RESULTS rows, proposed
   REDRESS entry, deterministic append order, and single-writer ownership after
   parallel worktrees converge.
6. Add conflict notes for any shared `bbnf-simd` dispatch, classifier policy,
   checkasm report, generated runtime, gate/report, RESULTS, or REDRESS path.

## Final CH4 Disposition

**ACCEPT.** The V3 fold gives `G-SIMD-GRAMMAR-POLICY` a real dependency role
and blocks unsafe non-JSON SIMD reuse without inflating Alpha-E into a hidden
large wave. CSS LOC, hard caps, conflict matrix, and RESULTS/REDRESS
serialization remain CH4-acceptable for S-P1/S-P3 planning.
