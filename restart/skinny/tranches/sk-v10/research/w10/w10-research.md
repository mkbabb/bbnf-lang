# SK-V10 W10 Research - Direct Residual Row Surface

Pass: Wave Research.
Cycle: W10.
Date: 2026-05-19.
Scope: read-only scan of residual `direct_to_struct` rows for a Section 13
direct-output/control mechanism.

## Entry Gate

PASS.

- W2 direct reclamation is closed under REDRESS 101.
- W3 parse-only firewall is closed under REDRESS 102.
- W10 may select one direct-output/control mechanism and at most three direct
  target rows, subject to CHALLENGE.

## Current Row Table

The live `skinny/RESULTS.md` table after W6 has one residual direct row that
already clears both Section 0.2 direct floors while still reporting
`N-direct / NO-GO`:

| Corpus | Track 1 | Track 2 | sonic direct | Floor | Current state |
|---|---:|---:|---:|---:|---|
| `instruments` | 12049 | 11236 | 12783 | 11086 | `N-direct / NO-GO` |

The next-nearest residual rows do not pass both tracks:

| Corpus | Track 1 | Track 2 | Floor | Miss |
|---|---:|---:|---:|---|
| `mesh` | 8796 | 9131 | 8916 | Track 1 |
| `random` | 7876 | 7104 | 7734 | Track 2 |
| `canada` | 10173 | 9855 | 10977 | both |

Rows outside this near set are farther below floor or already admitted.

## Mechanism Surface

The existing generated direct caller is
`bbnf_bench::direct_struct::track1_digest`, which invokes
`runtime::generated_json::parse_direct(input, &mut sink)`. The independent
Track 2/oracle is `bbnf_bench::direct_struct::track2_digest`, which invokes the
hand sink parser. `assert_direct_struct_parity` already checks Track 1, Track
2, serde, and sonic digest shape parity.

The W2 implementation intentionally limited admission to `apache_builds` and
`numbers`. W10 can be a direct residual follow-on by adding a new W10-limited
row admission predicate for `instruments` only, preserving the W1 direct
contract fields and rendering `REDRESS-109` / `SK-V10-W10`.

## Research Finding

Select only `instruments/direct_to_struct` for W10. Do not include `mesh` or
`random`; each misses one direct floor in the current table and would falsify a
multi-row W10 gate unless fresh measurement unexpectedly changed both tracks.

No parser/runtime behavior source is needed. The behavior is the direct
row-control mechanism already made executable by W1 and W2, extended to the one
residual row whose current evidence clears both direct tracks. Redress must
rerun targeted direct Criterion for `instruments` and guard rows before moving
`RESULTS.md`.
