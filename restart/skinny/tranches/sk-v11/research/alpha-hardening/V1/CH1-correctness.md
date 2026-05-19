# SK-V11 Pass Alpha CH1 Correctness Review

Pass: Pass Alpha CHALLENGE V1.
Lens: CH1 correctness.
Date: 2026-05-19.
Scope: SK-V11 Alpha packet correctness: numbers, row counts, close condition,
pass ownership, and whether Alpha creates implementation authority.

## Disposition

ACCEPT-WITH-NITS.

The Alpha packet is correct on the load-bearing SK-V10 close surface and does
not create SK-V11 implementation authority. One non-blocking prose count in
`alpha-F-contract-draft.md` should be folded before consolidation: the typed
plane has six raw Track 1 throughput wins over sonic-rs typed strict, not five.

## Findings

### NIT-1 - Alpha-F undercounts typed raw sonic wins

`restart/skinny/tranches/sk-v11/research/alpha/alpha-F-contract-draft.md:59`
through `:65` says seven typed rows are `A / GO` and that "five of the seven"
beat sonic-rs strict outright, while `update_center` remains admitted by slack.
The close table has six positive raw deltas: `twitter`, `citm_catalog`,
`apache_builds`, `github_events`, `mesh`, and `marine_ik`; only
`update_center` is negative (`skinny/RESULTS.md:7`, `:10`, `:15`, `:18`,
`:21`, `:24`, `:31`). Alpha-B states this correctly at
`restart/skinny/tranches/sk-v11/research/alpha/alpha-B-competitor-deltas.md:126`
through `:129`.

This is not a blocking defect because the row count, guard surface, and close
condition do not depend on that prose sentence. Fold by changing Alpha-F's
"five of the seven" to "six of the seven".

## Correctness Checks

### SK-V10 close authority

The packet uses the correct measured close authority. SK-V10 close redress names
`skinny/RESULTS.md` as the final authority, rendered from
`/tmp/skv10-w10-full-criterion` with run id
`sk-v9-open:criterion-fnv64-6f007527061ee26d`
(`restart/skinny/tranches/sk-v10/research/close/close-redress.md:33` through
`:37`). The close surface is 17 `parse_only S / NO-GO`, 6
`direct_to_struct A / GO`, 11 `direct_to_struct N-direct / NO-GO`, and 7
`real_typed_struct A / GO` (`close-redress.md:39` through `:43`), with overall
`N-direct / NoGo` (`close-redress.md:45` through `:47`).

Alpha-A repeats the same count surface at
`restart/skinny/tranches/sk-v11/research/alpha/alpha-A-results-extraction.md:32`
through `:40`, and SK-V11 SYNTHESIS/HANDOFF carry the same result surface at
`restart/skinny/tranches/sk-v11/SYNTHESIS.md:85` through `:94` and
`restart/skinny/tranches/sk-v11/HANDOFF.md:30` through `:41`.

I independently recounted the rendered `skinny/RESULTS.md` table:

| Workload | Count |
|---|---:|
| `parse_only S / NO-GO` | 17 |
| `direct_to_struct A / GO` | 6 |
| `direct_to_struct N-direct / NO-GO` | 11 |
| `real_typed_struct A / GO` | 7 |

### Direct residual floors

The 11-row direct residual goalset is numerically correct. The floor convention
is `ceil(sonic-rs direct / 1.10)`, as stated in
`restart/skinny/tranches/sk-v11/SYNTHESIS.md:96` through `:100`. Recomputed
floors match the SYNTHESIS table at `SYNTHESIS.md:102` through `:114`,
the HANDOFF table at `HANDOFF.md:43` through `:60`, Alpha-B at
`alpha-B-competitor-deltas.md:68` through `:87`, and Alpha-F at
`alpha-F-contract-draft.md:45` through `:57`.

The `mesh` signed gap differs by convention between tables, not value:
SYNTHESIS/Alpha-F record `floor - measured = -30` for Track 2, while Alpha-B
records the passing margin as `+30` (`SYNTHESIS.md:108`,
`alpha-B-competitor-deltas.md:81`, `alpha-F-contract-draft.md:51`). This is
readable and not a correctness failure.

### Close condition

The SK-V11 close condition is measurable and follows the user dispatch. It
requires direct plane closure or per-row REDRESS fixpoint, preservation of
existing 7 typed and 6 direct `A / GO` rows, parse-only as diagnostic only,
W3 union/substrate pre-blocking, one admitted benchmarked non-JSON grammar
intervention, aarch64-only micro-prove-first for SIMD/ASM, strict-vs-strict
comparators, and agreement among the close documents
(`restart/skinny/tranches/sk-v11/SYNTHESIS.md:27` through `:83`).
HANDOFF carries the same three bound axes and close posture
(`restart/skinny/tranches/sk-v11/HANDOFF.md:62` through `:80`,
`:135` through `:140`).

### Pass ownership and implementation authority

Pass Alpha is allowed to produce `SYNTHESIS.md` and `HANDOFF.md`; `SPEC.md` and
`DISPATCH-PROMPT.md` are downstream S-P3 outputs
(`restart/prompts/pass-contracts/PASS-ALPHA.md:3` through `:5`,
`:27`, `:112` through `:122`). The Alpha CHALLENGE output path is correct under
`PASS-ALPHA.md:33` through `:49`.

The current Alpha packet respects that boundary:

- SYNTHESIS states it is not source implementation authority and that SPEC /
  DISPATCH are not created by this pass
  (`restart/skinny/tranches/sk-v11/SYNTHESIS.md:5` through `:8`).
- HANDOFF repeats that it does not authorize source work or create SPEC /
  DISPATCH (`restart/skinny/tranches/sk-v11/HANDOFF.md:5` through `:9`).
- Alpha-E states the shortlist does not authorize source redress, SPEC waves, or
  row movement (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:9`
  through `:13`).
- `restart/skinny/tranches/sk-v11/SPEC.md` and
  `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md` are absent in this V1
  packet.

No Alpha artifact improperly creates implementation authority.

### Pre-blocked W3/substrate route

The W3 union-substrate falsification is carried correctly. REDRESS 98 says the
union substrate thesis is retired, not merely blocked, after REDRESS 96/97
regressed uniformly (`skinny/REDRESS.md:2910` through `:2934`). Alpha-C carries
that as a hard pre-block at
`restart/skinny/tranches/sk-v11/research/alpha/alpha-C-redress-digest.md:28`
through `:49`, and SYNTHESIS/HANDOFF refuse renamed W3 routes
(`restart/skinny/tranches/sk-v11/SYNTHESIS.md:52` through `:54`,
`restart/skinny/tranches/sk-v11/HANDOFF.md:117` through `:123`).

### Commit and row provenance

Alpha-D's commit anchors resolve in git and align with the cited wave purposes
(`restart/skinny/tranches/sk-v11/research/alpha/alpha-D-validated-invalidated.md:27`
through `:43`). The row movement provenance also reconciles with REDRESS:
W2 moves `apache_builds/direct_to_struct` and `numbers/direct_to_struct`
(`skinny/REDRESS.md:3005` through `:3019`), W6 moves
`github_events/real_typed_struct` (`skinny/REDRESS.md:3108` through `:3122`),
and W10 moves `instruments/direct_to_struct` (`skinny/REDRESS.md:3226`
through `:3255`).

## CH1 Recommendation

Accept Alpha V1 after folding NIT-1 into Alpha-F or the consolidated hardening
note. There is no CH1 blocker to G-Alpha presentation: row counts, direct floors,
close condition, pass ownership, and implementation-authority boundaries are
correct.
