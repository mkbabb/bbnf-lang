# SK-V13 Alpha Hardening V4 - CH3 Regression / REDRESS

Date: 2026-05-21.

Role: Alpha CH3 regression/REDRESS confirmation for SK-V13 Alpha V4.

Verdict: ACCEPT.

## Scope Reviewed

- `restart/skinny/tranches/sk-v13/research/alpha-hardening/V3/CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v13/research/alpha-hardening/V3/CH3-regression-redress.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` through REDRESS-127
- `restart/skinny/tranches/sk-v13/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v13/HANDOFF.md`
- `restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v13/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v13/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md`

## Regression Checks

### REDRESS-119/120 Are History Only

Pass. V3 consolidated recorded CH3 as ACCEPT with REDRESS-119/120 history-only,
and the current packet keeps that posture. Alpha-C says the user pin supersedes
the older REDRESS-119/120 fixpoint close; both entries remain measured SK-V11
history but no longer close JSON direct residuals or `parse_only`. Its
pre-block table marks `119/120` as "History only under pin" and not a row-reopen
blocker.

Alpha-D independently invalidates REDRESS 119/120 as active JSON closure and
demotes the REDRESS 119 direct residual fixpoint to history-only under the
addendum. The synthesis and handoff carry the same rule: the REDRESS-119
13-row direct fixpoint and REDRESS-120 SK-V11 close history must be folded as
history, not as closure authority.

No regression from V3.

### All 51 JSON Rows Remain Mandatory, Including Absent Typed Rows

Pass. Alpha-A still frames G5 as 17 JSON corpora across 3 planes:
`parse_only`, `direct_to_struct`, and `real_typed_struct`. It records 41 JSON
rows in the rendered authority plus 10 absent `real_typed_struct` rows, and it
names the absent typed rows: `canada`, `random`, `gsoc-2018`, `instruments`,
`numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`,
`distinct_values`, and `y_string_unicode`.

Alpha-C carries the same accounting into S-P3 obligations: all 17 typed rows
are mandatory, the 7 rendered typed rows must satisfy the strict
comparator-plus-one rule or reopen, and the 10 absent typed rows must become
measured rows or carry architectural-block proof. Alpha-D repeats that all
17 corpora x 3 JSON planes are active SK-V13 campaign rows.

No regression from V3.

### Old Below-Bar A/GO Rows Reopen

Pass. The current `skinny/RESULTS.md` still contains old JSON `A / GO` rows
that miss the SK-V13 addendum bar. Alpha-A computes margin as
`Track 1 - (strict comparator + 1)` and records
`json/numbers/direct_to_struct/main` at margin `-437` and
`json/unicode_basic/direct_to_struct/main` at margin `-709`, despite their old
`A/GO` status. It also records `json/update_center/real_typed_struct/main` as
an old typed `A/GO` row below the strict comparator-plus-one bar.

Alpha-C requires one JSON direct wave per direct row that does not satisfy
`Track 1 > sonic-rs strict Mbps + 1`, regardless of older `A / GO` or
`N-direct / NO-GO` rendering. Alpha-D states that old admits must still satisfy
the new comparator bar or reopen.

No regression from V3.

### Direct Survey Is Priority/Risk Only

Pass. The profile-truth scoping packet explicitly says all 17 direct rows
remain G5 accounting, including older `A / GO` rows below
`Track 1 > sonic-rs strict Mbps + 1`, and that the matrix is priority/risk
triage for S-P1/S-P3, not eligibility or close authority. Its matrix labels
non-first-wave rows as reopenable or lower-priority/high-risk, not ineligible.

No regression from V3.

### `G-SIMD-GRAMMAR-POLICY` Cannot Paper-Close A Row

Pass. V3 consolidated noted the named `G-SIMD-GRAMMAR-POLICY` gate as the
additional CH3 paper-close concern, and the current packet keeps it
prerequisite-only. Alpha-E requires this gate before any wave wires `bbnf-simd`
into CSS, union, JSON `parse_only`, or shared generated code, but passing it
only proves the consuming grammar policy, scalar/checkasm/differential coverage,
same-wave row consumption, no public substrate API or public `GrammarConfig`
trait, and no retained sidecar classifier state.

The same Alpha-E section separately requires `G-WX-SIMD-CONSUMED-KERNEL` for
row impact: the production consumer must execute in the measured row, and Track
1 must beat the strict comparator or move measurably toward it. A parity pass
without row movement must remove or demote the primitive with REDRESS evidence.
Synthesis repeats the controlling anti-paper-close rule by rejecting
producer-only SIMD, union, resolver, or codegen artifacts without same-wave
consumer measurement, and by requiring `G-SIMD-GRAMMAR-POLICY` before non-JSON
or shared `bbnf-simd` consumers inherit any classifier dispatch.

Therefore `G-SIMD-GRAMMAR-POLICY` is a legality/correctness prerequisite, not a
paper-close mechanism.

No regression from V3.

## CH3 Disposition

V4 remains regression-safe against the V3 CH3 requirements. REDRESS-119/120 are
history-only; all 51 JSON rows remain mandatory accounting including the absent
typed rows; old below-bar `A/GO` rows reopen; the direct survey is priority/risk
triage only; and `G-SIMD-GRAMMAR-POLICY` cannot close anything without
same-wave measured row consumption and the strict comparator gate.

Concrete fixes required before Alpha convergence from CH3: none.
