# SK-V13 Alpha Hardening V3 - CH3 Regression / REDRESS

Date: 2026-05-21.

Role: Alpha CH3 regression/REDRESS challenge for SK-V13 Alpha V3.

Verdict: ACCEPT.

## Scope Reviewed

- `restart/skinny/tranches/sk-v13/research/alpha-hardening/V2/CH3-regression-redress.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` through REDRESS-127
- `restart/skinny/tranches/sk-v13/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v13/HANDOFF.md`
- `restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v13/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v13/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md`

## Regression Checks

### REDRESS-119/120 Are History Only

Pass. V2 already verified the Alpha packet demotes REDRESS-119/120 from closure
authority to historical evidence. The current Alpha-C still says the addendum
supersedes the REDRESS-119/120 fixpoint close, that those entries no longer
close JSON direct residuals or `parse_only`, and that implementation-limited
failures are reopens. Its pre-block table marks `119/120` as "History only
under pin; cannot pre-block row reopen by itself."

The synthesis carries the controlling rule: all 17 JSON corpora across all
three planes must beat sonic-rs strict or carry per-row architectural block, and
REDRESS-119/120 do not block fresh SK-V13 reopens.

No regression from V2.

### All 51 JSON Rows Remain Mandatory Accounting

Pass. Alpha-A continues to normalize G5 as 17 corpora x 3 planes:
`parse_only`, `direct_to_struct`, and `real_typed_struct`. It records 41 JSON
rows in the rendered authority plus 10 absent `real_typed_struct` rows, and
names the absent rows: `canada`, `random`, `gsoc-2018`, `instruments`,
`numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`,
`distinct_values`, and `y_string_unicode`.

Alpha-C carries the same requirement into S-P3: all 17 typed rows are mandatory,
the seven rendered typed rows must satisfy strict comparator plus one or reopen,
and the 10 absent rows must become measured rows or carry architectural-block
proof. Its summary names the mandatory set as 51 JSON rows, including the 10
absent typed rows.

No regression from V2.

### Old Below-Bar A/GO Rows Reopen

Pass. The current `skinny/RESULTS.md` still renders old direct `A / GO` rows
that miss the addendum bar, including `json/numbers/direct_to_struct/main`
at Track 1 `12240` against sonic-rs strict `12676`, and
`json/unicode_basic/direct_to_struct/main` at Track 1 `8134` against sonic-rs
strict `8842`.

Alpha-A explicitly computes margin as `Track 1 - (strict comparator + 1)` and
records those rows as below bar: `numbers/direct_to_struct` margin `-437` and
`unicode_basic/direct_to_struct` margin `-709`. Alpha-C requires one JSON direct
wave per row that does not satisfy `Track 1 > sonic-rs strict Mbps + 1`,
regardless of older `A / GO` or `N-direct / NO-GO` rendering.

No regression from V2.

### Direct Survey Is Priority/Risk, Not Eligibility

Pass. The profile-truth scoping doc states that all 17 direct rows remain G5
accounting, including older A/GO rows below `Track 1 > sonic-rs strict Mbps + 1`.
It labels the direct matrix as priority/risk triage for S-P1/S-P3, not
eligibility or close authority. The matrix marks non-first-wave rows as
reopenable or lower-priority/high-risk, not ineligible.

No regression from V2.

### `G-SIMD-GRAMMAR-POLICY` Does Not Create A Paper Close

Pass. V3 adds an explicit anti-paper-close check for the SIMD grammar-policy
gate. Alpha-E defines `G-SIMD-GRAMMAR-POLICY` as a prerequisite for wiring
`bbnf-simd` into CSS, union, JSON `parse_only`, or shared generated code. It
passes only with the consuming grammar's quote/escape/control policy or a
no-string policy, scalar parity, checkasm/differential coverage, same-wave
measured row consumption, no public substrate API, and no retained sidecar
classifier state. The same section separately requires
`G-WX-SIMD-CONSUMED-KERNEL` for actual row impact: the production consumer must
execute in the measured row, and Track 1 must beat the strict comparator or move
measurably toward it.

The synthesis and handoff preserve this distinction. They reject producer-only
SIMD, union, resolver, or codegen artifacts without same-wave consumer
measurement, and they reject non-JSON/shared `bbnf-simd` consumers unless the
grammar-policy gate proves the selected path cannot inherit JSON quote/escape
semantics. The SIMD/ASM scoping packet also flags proof-only artifacts as
inadmissible and requires real measured delta on JSON direct, JSON parse, or CSS
rows.

Therefore `G-SIMD-GRAMMAR-POLICY` is a legality and correctness prerequisite,
not an admission or close mechanism. It cannot create a paper close by itself.

## CH3 Disposition

V3 remains regression-safe against the V2 CH3 requirements and satisfies the
additional SIMD grammar-policy paper-close check. REDRESS-119/120 remain
history only; all 51 JSON rows remain mandatory accounting, including 10 absent
typed rows; below-bar A/GO direct rows reopen; the direct survey is triage only;
and `G-SIMD-GRAMMAR-POLICY` is constrained to prerequisite status with
same-wave measured row consumption required for any admission.

Concrete fixes required before G-Alpha from CH3: none.
