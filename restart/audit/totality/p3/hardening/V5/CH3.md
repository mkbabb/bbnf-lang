# T-P3 V5 CH3 Regression Hardening

Target packet: `77b6e9fd7` (`docs(sk-v15-t-p3): repair V4 citation finding`)
Cycle: `V5`
Lens: `CH3 REGRESSION`
Verdict: `ACCEPT`

## Required Checks

| check | result |
|---|---|
| `git show --stat --oneline 77b6e9fd7 -- restart/audit/totality/p3` | `77b6e9fd7 docs(sk-v15-t-p3): repair V4 citation finding`; one proposal artifact changed: `restart/audit/totality/p3/3A-architecture-synthesis.md`, 1 insertion and 1 deletion. |
| `git diff --check 77b6e9fd7^ 77b6e9fd7 -- restart/audit/totality/p3` | Clean; exit 0, no output. |
| Extract `3C-locks-v+1-diff.md` to `/tmp/tp3-locks-v5.diff`; `git apply --check /tmp/tp3-locks-v5.diff` | Clean; extracted diff has 36 lines and applies with exit 0. |
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16`. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `67`. |
| Required stale-pattern `rg -n ...` across 3A..3F and 3C diff | No matches; exit 1 is the expected no-match result. |

The required invariant outputs match the V5 challenge context: 16 numbered
locks, 67 Pattern H runtime files, and no stale-pattern hits. The working tree
already contained unrelated dirty runtime/research files; the T-P3 proposal
artifacts, `restart/locks/LOCKS.md`, and top-level V1 surfaces had no dirty diff
before this report was written.

## Regression Review

No REDRESS route is reopened by the V5 target packet. The packet changes only
the repaired 3A citation at `restart/audit/totality/p3/3A-architecture-synthesis.md:56`;
that row remains proposal-only and does not create a live implementation route
(`restart/audit/totality/p3/3A-architecture-synthesis.md:54`-`57`). The
regression-sensitive fail actions remain closed: 3A routes self-exempting gates,
runtime regex/DFA substrate, production FNV use, wrong-host primitive evidence,
and retained sidecar/public substrate routes to rejection, REDRESS/revert,
intrinsic-block, or G-Omega amendment (`restart/audit/totality/p3/3A-architecture-synthesis.md:100`-`111`).
3B similarly routes missing row fields, stale-row reuse, delete-before-provider,
self-exempting gates, lowerer overflow, wrong primitive admission, and
production FNV leakage to block, intrinsic-block, REDRESS/revert, or G-Omega
amendment with no W12 route (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:172`-`182`).
3C keeps the lock clauses doc-only and fail-closed: runtime-regex/substrate
expansion, header-only closure, delete-before-provider, sixth shape, generic
branches, and primitive admission gaps remain rejected by owning gates
(`restart/audit/totality/p3/3C-locks-crystallisation.md:61`-`76`).

Stale receiver blocks stay historical or pre-block. 3B identifies stale
receiver framing in `MASTER-PLAN.md`, requires one SK-V15 receiver block, and
keeps old CSS/SK-V14 rows as historical or pre-block evidence
(`restart/audit/totality/p3/3B-master-plan-reconciliation.md:28`-`41`). Its
classification ledger marks the `SK-V14 W0..W11` block historical/superseded and
the `MP-NW-01..14` block historical/superseded except refusal rows; current
dispatch moves to SK-V15 W0..W11 and explicit dependency rows
(`restart/audit/totality/p3/3B-master-plan-reconciliation.md:112`-`113`). 3F
keeps existing Pass Omega V2..V8 MIGRATION sections as historical SK-V14 receiver
records, not current SK-V15 dispatch authority, and routes current HANDOFF
authority to SK-V15 T-P3/Pass Omega V5 plus the extant SK-V15 dispatch prompt
(`restart/audit/totality/p3/3F-migration-handoff.md:40`,
`restart/audit/totality/p3/3F-migration-handoff.md:44`-`46`).

Delete-before-provider remains blocked. 3C's proposed Lock 6/14 clause requires
line-1 provenance, rostered generator, non-writing regen/check proof, and a
same-wave replacement provider before deletion or retirement; provider/template
deletion before W5/W6 typed replacement proof rejects
(`restart/audit/totality/p3/3C-locks-v+1-diff.md:50`). 3B preserves the W4
Pattern H split and makes destructive deletion conditional on same-wave
replacement proof (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:144`-`150`).
3D carries the same sequencing for `CSS_GENERATED_RS`, `CssFullParseSummary`,
fact-stream CSS `parse()`, brace-counter proof, deletes, retirements, diagnostic
demotions, and rebuilds (`restart/audit/totality/p3/3D-skinny-fold.md:61`,
`restart/audit/totality/p3/3D-skinny-fold.md:68`). 3E keeps Pattern H deletion
blocked until same-wave replacement proof exists and limits its own row to
provenance-gate scope (`restart/audit/totality/p3/3E-grammar-generalisation.md:78`,
`restart/audit/totality/p3/3E-grammar-generalisation.md:150`). 3F makes the same
rule a MIGRATION gate: no delete, retirement, provider/template removal, old CSS
proof retirement, or runtime-shim deletion before rebuild proof lands no later
than the delete wave (`restart/audit/totality/p3/3F-migration-handoff.md:42`).
The SK-V15 dependency table independently rejects missing delete/retire rows and
blocks CSS generated proof, CSS summary/fact-stream proof, provider/template
deletion, Pattern H destructive delete, CSS legacy runtime shims, lowerer
scaffolds, FNV production migration, and orphan dependency rows until their
named proof rows are consumed (`restart/skinny/tranches/sk-v15/SPEC.md:189`-`204`).

The V3/V4/V5/V6/V7/V8 corrective gates are preserved. REDRESS keeps V3/V4 as
dispatch-route amendments while preserving REDRESS-183 as historical rejection
and REDRESS-184 as the provider-deletion blocker until deletion moved later
(`skinny/REDRESS.md:5095`-`5101`, `skinny/REDRESS.md:5105`-`5118`). V4 PRUNE
rows explicitly performed ledger-only CSS reclassification without source,
generator, provider, template, runtime-twin, or `regen_css` deletion
(`skinny/REDRESS.md:5122`-`5127`). V5 and V6 keep REDRESS-209 and REDRESS-210
historical while requiring W5A before W5B deletion and W5B-GEN before
W5C-DELETE (`skinny/REDRESS.md:5189`-`5193`, `skinny/REDRESS.md:5212`-`5217`).
V7 requires the W5B-GENR split and keeps W5C-DELETE/W6/W7/W8-W10 blocked
(`skinny/REDRESS.md:5241`-`5245`). V8 formalizes W5B-FRONTEND sub-waves and
keeps W5D-DELETE/W6/W7/W8-W10 blocked until aggregate close
(`skinny/REDRESS.md:5268`-`5272`). The target packet carries this forward: 3F
says the migration clause prevents the V3/V4/V5/V6/V7/V8 delete-before-provider
failure pattern from re-entering, and its open CH3 check requires preservation
of the pre-block list (`restart/audit/totality/p3/3F-migration-handoff.md:42`,
`restart/audit/totality/p3/3F-migration-handoff.md:140`). SK-V15 SPEC's
pre-block list independently preserves REDRESS 183/184/209-213, 215, and FNV
production migration (`restart/skinny/tranches/sk-v15/SPEC.md:467`-`484`).

## Defects

None.

## Final Verdict

`ACCEPT`. V5 CH3 finds no reopened REDRESS route, no revived stale receiver
block, no delete-before-provider gap, and no regression of the
V3/V4/V5/V6/V7/V8 corrective gates.
