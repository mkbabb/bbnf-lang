# SK-V14 W1C: PRUNE-1 JSON Revert Ledger

Date: 2026-05-24.
Scope: W1 audit-falsified JSON row population and revert evidence.
Output: this file.

## §1 — Findings

- W1 PRUNE-1 population is 22 JSON rows: 5 `parse_only`, 6 `direct_to_struct`, and 11 `real_typed_struct`.
- `restart/skinny/tranches/sk-v14/SPEC.md:421-428` lists a by-number REDRESS mapping, but that mapping is unsafe as written: the direct list says REDRESS 131-135, while `skinny/REDRESS.md:3883` shows those entries are CSS, not JSON direct.
- Use row keys as the authoritative W1 revert manifest.
- The rolling file lives at `restart/skinny/ROLLING-SOTA-DELTA.md`, not `skinny/ROLLING-SOTA-DELTA.md`.

## §2 — Recommendations

Revert or mark these rows as audit-falsified in `skinny/RESULTS.md`, `restart/skinny/ROLLING-SOTA-DELTA.md`, and row-keyed `skinny/REDRESS.md` entries:

- parse_only: `json/numbers/parse_only/main`, `json/citm_catalog/parse_only/main`, `json/canada/parse_only/main`, `json/marine_ik/parse_only/main`, `json/mesh/parse_only/main`.
- direct_to_struct: `json/citm_catalog/direct_to_struct/main`, `json/apache_builds/direct_to_struct/main`, `json/marine_ik/direct_to_struct/main`, `json/instruments/direct_to_struct/main`, `json/numbers/direct_to_struct/main`, `json/unicode_basic/direct_to_struct/main`.
- real_typed_struct: `json/twitter/real_typed_struct/main`, `json/citm_catalog/real_typed_struct/main`, `json/apache_builds/real_typed_struct/main`, `json/github_events/real_typed_struct/main`, `json/update_center/real_typed_struct/main`, `json/mesh/real_typed_struct/main`, `json/random/real_typed_struct/main`, `json/marine_ik/real_typed_struct/main`, `json/instruments/real_typed_struct/main`, `json/numbers/real_typed_struct/main`, `json/unicode_basic/real_typed_struct/main`.

The exit state should show JSON parse_only 0/17, direct 0/17, and typed 0/17 admitted after W1.

## §3 — Risks

- Relying on the SPEC's direct REDRESS numbers will misroute W1 evidence to CSS history.
- `json/instruments/direct_to_struct/main` is already `OPEN` in `restart/skinny/ROLLING-SOTA-DELTA.md:45` but still appears as `A / GO` in `skinny/RESULTS.md`; W1 must reconcile both surfaces.
- PRUNE-1 is a revert/honesty wave, not a new admit wave.

## §4 — Sources

- `restart/skinny/tranches/sk-v14/SPEC.md:421-428`
- `skinny/RESULTS.md:7-106`
- `restart/skinny/ROLLING-SOTA-DELTA.md:14-64`
- `skinny/REDRESS.md:3883`
- `skinny/REDRESS.md:4767-5031`
