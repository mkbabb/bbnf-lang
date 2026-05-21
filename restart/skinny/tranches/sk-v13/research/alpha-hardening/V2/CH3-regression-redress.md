# SK-V13 Alpha Hardening V2 - CH3 Regression / REDRESS

Date: 2026-05-21.

Role: Alpha CH3 regression/REDRESS challenge for SK-V13 Alpha V2.

Verdict: ACCEPT.

## Scope Reviewed

- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` through REDRESS-127
- `restart/skinny/tranches/sk-v13/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v13/HANDOFF.md`
- `restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v13/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md`

## Regression Checks

### REDRESS-119/120 Are History Only

Pass. Alpha-C states that the addendum supersedes the older REDRESS-119/120
fixpoint close, keeps REDRESS-119/120 only as evidence of prior SK-V11 attempts,
and says they no longer close JSON direct residuals or `parse_only`
(`alpha-C-redress-digest.md:13-15`). Its pre-block table also marks `119/120` as
"History only under pin" and says they cannot pre-block row reopen
(`alpha-C-redress-digest.md:93`). The synthesis repeats the same controlling
rule: REDRESS-119 and REDRESS-120 are history only and their 13-row direct
fixpoint does not block SK-V13 reopens (`SYNTHESIS.md:102-106`).

No fix required.

### All 51 JSON Rows Are Mandatory Accounting

Pass. Alpha-A normalizes the current rendered state to the full G5 target:
17 corpora x 3 planes, with `parse_only`, `direct_to_struct`, and
`real_typed_struct` all mandatory (`alpha-A-results-extraction.md:10-12`).
It records that the current first table has 41 JSON rows and that 10
`real_typed_struct` rows are absent but must become explicit rows or blockers
(`alpha-A-results-extraction.md:54-57`). It names the 10 absent typed rows
(`alpha-A-results-extraction.md:75-77`) and includes each as absent-row debt in
the B0 inventory (`alpha-A-results-extraction.md:139-148`).

Alpha-C carries the same accounting into the reopen ledger: all 17 typed rows
are mandatory, the seven rendered typed rows must satisfy strict comparator
plus one or reopen, and the 10 absent rows must become measured rows or carry
architectural-block proof (`alpha-C-redress-digest.md:201-204`). The
classification summary names the mandatory set as 51 JSON rows including 10
absent typed rows (`alpha-C-redress-digest.md:220`).

No fix required.

### Old A/GO Rows Below The Addendum Bar Reopen

Pass. Alpha-A computes margin as `Track 1 - (strict comparator + 1)` and exposes
old `A/GO` direct rows that are below the addendum bar, including
`json/numbers/direct_to_struct/main` at margin `-437` and
`json/unicode_basic/direct_to_struct/main` at margin `-709`
(`alpha-A-results-extraction.md:90-91`, `:127`, `:133`).

Alpha-C then makes the policy explicit: direct rows must admit by strict
equality versus sonic-rs strict with `Track 1 > sonic-rs strict + 1 Mbps`, with
no silent demotion of previous A/GO rows (`alpha-C-redress-digest.md:100-105`).
Its S-P3 obligations require one JSON direct wave per direct row that does not
satisfy the comparator-plus-one rule, regardless of older `A / GO` or
`N-direct / NO-GO` rendering (`alpha-C-redress-digest.md:195-198`).

The profile-truth survey also calls out older A/GO misses directly:
`numbers/direct` and `unicode_basic/direct` are both "older A/GO but misses
addendum bar" and reopenable (`sk-v13-scoping-profile-truth.md:126`,
`:129`).

No fix required.

### Direct-Row Survey Is Priority/Risk, Not Eligibility

Pass. The profile-truth scoping doc now states that all 17 direct rows remain
G5 accounting, including older A/GO rows below `Track 1 > sonic-rs strict Mbps
+ 1`, and that the direct matrix is "priority/risk triage for S-P1/S-P3, not
eligibility or close authority" (`sk-v13-scoping-profile-truth.md:107-112`).
The matrix labels non-first-wave rows as reopenable or lower-priority/high-risk,
not ineligible (`sk-v13-scoping-profile-truth.md:118-136`).

No fix required.

## CH3 Disposition

V2 satisfies the CH3 regression/REDRESS requirements from the V1 hardening
verdict. The packet correctly prevents old fixpoint closure, expands mandatory
JSON accounting to all 51 rows, reopens old admits that miss the addendum bar,
and demotes the direct-row survey to priority/risk triage.

Concrete fixes required before G-Alpha from CH3: none.
