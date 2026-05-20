# SK-V11 W3-R3: Numeric Row Gates

Date: 2026-05-20.
Scope: W3 row gates and baseline evidence.
Output: this file.

## §1 — Findings

- SK-V11-open authority is `/tmp/skv11-open-criterion-3ce75df`, run id
  `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`, Apple M5 Max native, with
  `gate-json --with-cost-facts --check-results` green (`SPEC.md:21`).
- W3 defaults to one or two numeric target rows unless same-host microbench
  data justifies all four (`SPEC.md:452`).
- W3 exit rows and floors:
  - `canada/direct_to_struct`: Track 1 10316, Track 2 9819, sonic 11700,
    floor 10637; needs +321/+818 Mbps (`SPEC.md:124`, `skinny/RESULTS.md:12`).
  - `mesh/direct_to_struct`: Track 1 8561, Track 2 8652, sonic 9542,
    floor 8675; needs +114/+23 Mbps (`SPEC.md:127`, `skinny/RESULTS.md:23`).
  - `numbers/direct_to_struct`: Track 1 4479, Track 2 2366, sonic 2667,
    floor 2425; Track 2 needs +59 Mbps and row is W0-clamped
    (`SPEC.md:131`, `skinny/RESULTS.md:35`).
  - `instruments/direct_to_struct`: Track 1 11569, Track 2 10736, sonic 9865,
    floor 8969; both tracks exceed floor but row is W0-clamped
    (`SPEC.md:130`, `skinny/RESULTS.md:33`).
- Direct guard floors are `citm_catalog` 18191/17431, `apache_builds`
  11028/9996, `marine_ik` 8759/9248, and `unicode_basic` 2253/2182
  (`SPEC.md:139`, `SPEC.md:144`).
- Typed guard floors are `twitter` 17385/15593, `citm_catalog` 29928/17321,
  `apache_builds` 8308/6754, `github_events` 11633/12029, `update_center`
  11613/10150, `mesh` 9214/7739, and `marine_ik` 11552/9894 (`SPEC.md:149`,
  `SPEC.md:156`).
- REDRESS 113 blocks W2 before implementation. W3-W8 may continue only as
  direct-plane closure/fixpoint waves with non-JSON explicitly blocked
  (`skinny/REDRESS.md:3342`, `skinny/REDRESS.md:3352`).

## §2 — Recommendations

- Target `mesh/direct_to_struct` first. It is near-floor, non-clamped, and has
  the smallest required lift among W3 rows.
- If selecting two rows, add `numbers/direct_to_struct` only if the plan names
  W0-clamp provenance explicitly. Otherwise add `canada/direct_to_struct`.

## §3 — Risks

- W3 must not claim non-JSON proof closure; W2 remains BLOCKED.
- No f64 fallback, mantissa widening/table route, leading-zero/sign/exponent/
  suffix, or conversion-policy change may enter W3 (`SPEC.md:474`).
- Direct admission from W0-clamped `numbers` or `instruments` without measured
  wave provenance is pre-blocked (`SPEC.md:485`).
- W3 substrate repair, class columns, streaming cursors, class lanes, and
  sidecars remain barred (`SPEC.md:39`).

## §4 — Sources

- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v11/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`
