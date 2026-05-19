# SK-V10 Close Research - Disposition Inventory

Pass: Wave Research.
Cycle: Close.
Date: 2026-05-19.
Gate: `G-CLOSE-SK-V10`.
Disposition target: close accounting.

## Scope

Close is a documentation and gate-reconciliation wave. It does not authorize
new behavior source, generated parser output, SIMD work, row movement, or
telemetry schema changes. The research question is whether W0-W10 now have
complete dispositions and whether the close packet can be reconciled without
changing accepted source or measurement commits.

## Wave Dispositions

| Wave | Disposition | REDRESS | Row effect |
|---|---|---|---|
| W0 | Closed | 99 | None |
| W1 | Closed | 100 | None |
| W2 | Admitted | 101 | `apache_builds/direct_to_struct`, `numbers/direct_to_struct` |
| W3 | Closed as firewall | 102 | None |
| W4 | Rejected | 103 | None |
| W5 | Closed as proof | 104 | None |
| W6 | Admitted | 105 | `github_events/real_typed_struct` |
| W7 | Rejected | 106 | None |
| W8 | Closed as proof | 107 | None |
| W9 | Rejected | 108 | None |
| W10 | Admitted | 109 | `instruments/direct_to_struct` |

The entry gate for Close is therefore satisfied: every dispatched wave W0-W10
is admitted, proof-closed, or REDRESS-rejected.

## Current Result Surface

The current `skinny/RESULTS.md` authority is the W10 full native Criterion
render with run id `sk-v9-open:criterion-fnv64-6f007527061ee26d`.

| Family | Current state |
|---|---|
| `parse_only` | 17 `S / NO-GO` |
| `direct_to_struct` | 6 `A / GO`, 11 `N-direct / NO-GO` |
| `real_typed_struct` | 7 `A / GO` |

The global report remains `N-direct / NoGo`; that is expected because eleven
direct residual rows remain `N-direct / NO-GO`.

## Close Checks

- `skinny/RESULTS.md` already contains W10 strict measured-row provenance for
  `instruments/direct_to_struct`: `SK-V10-W10`, `REDRESS-109`,
  `gate_json_direct_contract`, `direct-residual`.
- `skinny/REDRESS.md` records REDRESS 94-109, including W3 retirement,
  W4/W7/W9 measured rejections, and W10 admission.
- `SPEC.md`, `DISPATCH-PROMPT.md`, `HANDOFF.md`, and `SYNTHESIS.md` already
  identify Close as the next live wave after W10.
- `gate-json --with-cost-facts --check-results` passed against the W10
  Criterion root `/tmp/skv10-w10-full-criterion`.

## Routed Remainder

Close must route two non-implementation items:

- Pass Omega receives the REDRESS 98 substrate-ceiling lock amendment:
  profile-derived structural/substrate rewrites remain pre-blocked without a
  same-host micro-proof and a live output-plane target.
- The totality track receives the CSS L4 / Sheets / BBNF-self generalization
  risk. JSON-only SK-V10 results do not prove the full generator thesis.

## Research Disposition

No blocker was found. Close may proceed to plan.
