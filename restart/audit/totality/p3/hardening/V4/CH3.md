# T-P3 V4 CH3 Regression Hardening

Target packet: `e6c1c2a84` (`docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`)
Cycle: `V4`
Lens: `CH3 REGRESSION`
Verdict: `ACCEPT`

## Required Checks

| check | result |
|---|---|
| `git show --stat --oneline e6c1c2a84 -- restart/audit/totality/p3` | `e6c1c2a84 docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`; 7 proposal artifacts changed, 170 insertions, 123 deletions. |
| `git diff --check e6c1c2a84^ e6c1c2a84 -- restart/audit/totality/p3` | Clean; exit 0, no output. |
| Extract `3C-locks-v+1-diff.md` to `/tmp/tp3-locks-v4.diff`; `git apply --check /tmp/tp3-locks-v4.diff` | Clean; extracted diff has 36 lines and applies with exit 0. |
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16`. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `67`. |
| Required stale-pattern `rg -n ...` across 3A..3F and 3C diff | No matches; exit 1 is the expected no-match result. |

The required invariant outputs match the V4 challenge context: 16 numbered locks,
67 Pattern H runtime files, and no stale-pattern hits. A direct comparison of
the seven target proposal artifacts against `e6c1c2a84` was clean before this
report was written; the unrelated dirty worktree files are outside the T-P3
proposal artifacts and outside this report's ownership.

## Regression Review

No REDRESS route is reopened. The target packet uses REDRESS only as a
fail-closed outcome or historical/pre-block evidence, not as a direct
implementation shortcut. 3F keeps implementation blocked until T-P3 locks, G3
auto-passes, Pass Omega V5 CRUD-4 completes or records an exact
blocked/extension remainder, G-Omega closes, and only then SK-V15 W0 dispatches
(`restart/audit/totality/p3/3F-migration-handoff.md:25`,
`restart/audit/totality/p3/3F-migration-handoff.md:127`-`133`). 3B routes
missing row fields, stale-row reuse, delete-before-provider, self-exempting
gates, lowerer overflow, wrong primitive admission, and production FNV leakage to
block, intrinsic-block, REDRESS/revert, or G-Omega amendment with no W12 route
(`restart/audit/totality/p3/3B-master-plan-reconciliation.md:172`-`182`). 3C
applies the same posture to every lock clause: Pass Omega doc-only text is
consumed by owning gates, while runtime-regex/substrate expansion, header-only
close, delete-before-provider, sixth shape, generic branches, and primitive
admission gaps fail closed (`restart/audit/totality/p3/3C-locks-crystallisation.md:61`-`76`).

Stale receiver blocks stay historical or pre-block. 3B explicitly says the stale
part of MASTER is receiver framing, then requires one SK-V15 receiver block and
reclassifies old CSS and SK-V14 rows as historical/pre-block evidence
(`restart/audit/totality/p3/3B-master-plan-reconciliation.md:28`-`41`). The
classification ledger marks `SK-V14 W0..W11` as historical/superseded and
`MP-NW-01..14` as historical/superseded except refusal rows, with the current
route moved to SK-V15 W0..W11 and explicit DEP rows
(`restart/audit/totality/p3/3B-master-plan-reconciliation.md:112`-`113`). 3F
keeps existing Pass Omega V2..V8 MIGRATION sections as historical SK-V14 receiver
records rather than current SK-V15 dispatch authority, and points HANDOFF to
SK-V15 T-P3/Pass Omega V5 plus the extant SK-V15 dispatch prompt
(`restart/audit/totality/p3/3F-migration-handoff.md:40`,
`restart/audit/totality/p3/3F-migration-handoff.md:44`-`46`).

Delete-before-provider remains blocked. 3C's proposed Lock 6/14 clause requires
line-1 provenance, rostered generator, non-writing regen/check proof, and a
same-wave replacement provider before deletion or retirement; provider/template
deletion before W5/W6 typed replacement proof rejects
(`restart/audit/totality/p3/3C-locks-v+1-diff.md:50`). 3D carries the same
sequencing rule for `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream CSS
`parse()`, and brace-counter proof, and states that deletes, retirements,
diagnostic demotions, and rebuilds must consume dependency rows with provider
proof no later than the retire/delete wave
(`restart/audit/totality/p3/3D-skinny-fold.md:61`,
`restart/audit/totality/p3/3D-skinny-fold.md:68`). 3F makes this a MIGRATION
gate: no delete, retirement, provider/template removal, old CSS proof retirement,
or runtime-shim deletion before rebuild proof lands no later than the delete wave
(`restart/audit/totality/p3/3F-migration-handoff.md:42`). The SK-V15 dependency
table independently enforces that missing delete/retire rows reject the plan and
that CSS generated proof, CSS summary/fact-stream proof, provider/template
deletion, Pattern H destructive delete, and CSS legacy runtime shims remain
blocked until the named replacement/proof rows are consumed
(`restart/skinny/tranches/sk-v15/SPEC.md:189`-`204`).

The V3/V4/V5/V6/V7/V8 corrective gates are preserved. REDRESS keeps V3/V4 as
dispatch-route amendments while preserving REDRESS-183 as historical rejection
and REDRESS-184 as the live provider-deletion blocker until the V4 W4R route
moved deletion later (`skinny/REDRESS.md:5095`-`5101`,
`skinny/REDRESS.md:5105`-`5118`). V4 PRUNE rows explicitly performed ledger-only
CSS reclassification without source, generator, provider, template,
runtime-twin, or `regen_css` deletion (`skinny/REDRESS.md:5122`-`5127`). V5 and
V6 keep REDRESS-209 and REDRESS-210 historical while requiring W5A before W5B
deletion and W5B-GEN before W5C-DELETE (`skinny/REDRESS.md:5189`-`5193`,
`skinny/REDRESS.md:5212`-`5217`). V7 requires the W5B-GENR split and keeps
W5C-DELETE/W6/W7/W8-W10 blocked (`skinny/REDRESS.md:5241`-`5245`). V8 formalizes
W5B-FRONTEND sub-waves and keeps W5D-DELETE/W6/W7/W8-W10 blocked until aggregate
close (`skinny/REDRESS.md:5268`-`5272`). The target packet carries this forward:
3F says the migration clause prevents the V3/V4/V5/V6/V7/V8
delete-before-provider failure pattern from re-entering, and its open CH3 check
requires the pre-block list to preserve REDRESS 183/184/209-213, 215, and FNV
production migration (`restart/audit/totality/p3/3F-migration-handoff.md:42`,
`restart/audit/totality/p3/3F-migration-handoff.md:140`). SK-V15 SPEC's
pre-block list independently preserves those clusters
(`restart/skinny/tranches/sk-v15/SPEC.md:467`-`484`).

## Defects

None.

## Final Verdict

`ACCEPT`. V4 CH3 finds no reopened REDRESS route, no revived stale receiver
block, no delete-before-provider gap, and no regression of the V3/V4/V5/V6/V7/V8
corrective gates.
