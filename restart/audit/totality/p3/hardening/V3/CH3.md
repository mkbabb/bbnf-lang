# T-P3 V3 CH3 Regression Hardening

Target packet: `e6c1c2a84` (`docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`)
Context commit: `5b85f7d5d`
Lens: CH3 REGRESSION
Verdict: ACCEPT

## Required Local Checks

Preflight: the worktree already contained unrelated dirty runtime/restart files;
the T-P3 target artifacts, `restart/locks/LOCKS.md`, and top-level V1 surfaces
had no dirty diff in this audit slice. No files were staged or committed.

| check | result |
|---|---|
| `git show --stat --oneline e6c1c2a84 -- restart/audit/totality/p3` | Resolved target packet; 7 T-P3 files changed, 170 insertions, 123 deletions. |
| `git diff --check e6c1c2a84^ e6c1c2a84 -- restart/audit/totality/p3` | Clean; no whitespace errors. |
| Extract `3C-locks-v+1-diff.md` diff to `/tmp/tp3-locks-v3.diff`; `git apply --check /tmp/tp3-locks-v3.diff` | Clean; proposed LOCKS hunk applies. |
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16`; numbered lock invariant preserved. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `67`; Pattern H runtime-file invariant preserved. |
| Required stale-pattern `rg -n ...` over 3A..3F | No matches; exit 1 from no hits, as expected. |

## Regression Findings

No CH3 defects are present.

### REDRESS Routes

The V3 packet does not reopen REDRESS as an implementation shortcut. 3F keeps
implementation waves blocked until T-P3 locks, G3 auto-passes, Pass Omega V5
runs, CRUD-4 resolves current-state HANDOFF/MIGRATION cleanup or records an
exact blocked/extension remainder, and G-Omega closes; only then may SK-V15 W0
dispatch (`restart/audit/totality/p3/3F-migration-handoff.md:125`-`132`). W11
cannot close if any dependency row lacks proof, REDRESS route, revert evidence,
or intrinsic-block proof (`restart/audit/totality/p3/3F-migration-handoff.md:133`).

The added V3 CH4 coverage rows preserve fail-closed handling rather than
opening paper-close routes: 3A routes non-fit work to gate rejection,
intrinsic-block, REDRESS/revert, or G-Omega amendment (`restart/audit/totality/p3/3A-architecture-synthesis.md:98`-`111`);
3B does the same for MASTER deltas, including stale-row and delete-before-provider
cases (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:172`-`182`);
3D/3E keep CSS, Pattern H, FNV, sidecar, and receiver failures intrinsic-blocked
or REDRESSed (`restart/audit/totality/p3/3D-skinny-fold.md:74`-`83`;
`restart/audit/totality/p3/3E-grammar-generalisation.md:140`-`150`); and 3F
blocks W0 when CRUD-4, G-Omega, provider proof, stale authority cleanup, or row
gates are incomplete (`restart/audit/totality/p3/3F-migration-handoff.md:117`-`123`).

The governing SK-V15 dispatch also remains fail-closed: every wave must verify it
does not reopen an S-P2 REJECT or REDRESS pre-block (`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:50`-`56`), and the shared pre-block list is carried into every
wave (`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:334`-`336`).

### Stale Receiver Blocks

Stale SK-V13/SK-V14 receiver blocks are not revived as current dispatch. 3B names
the stale receiver framing and requires a new SK-V15 MASTER receiver block while
preserving old CSS and SK-V14 rows as historical/pre-block evidence
(`restart/audit/totality/p3/3B-master-plan-reconciliation.md:28`-`41`). Its
classification table marks the SK-V14 W0..W11 block historical/superseded and
the MP-NW-01..14 block historical/superseded except refusal rows
(`restart/audit/totality/p3/3B-master-plan-reconciliation.md:97`-`113`), and its
delta D04 says old rows must not gain current dispatch cap
(`restart/audit/totality/p3/3B-master-plan-reconciliation.md:159`,
`restart/audit/totality/p3/3B-master-plan-reconciliation.md:175`).

3F applies the same split to live HANDOFF/MIGRATION surfaces: existing Pass
Omega V2..V8 sections remain historical SK-V14 receiver records, not current
SK-V15 dispatch authority (`restart/audit/totality/p3/3F-migration-handoff.md:40`),
and current wave authority routes through the extant SK-V15
`DISPATCH-PROMPT.md`, not historical SK-V14 dispatch or a missing companion
prompt (`restart/audit/totality/p3/3F-migration-handoff.md:44`,
`restart/audit/totality/p3/3F-migration-handoff.md:86`-`89`). This matches the
SK-V15 HANDOFF ground truth that the locked skinny output is the W0-W11 contract
in `SPEC.md` and `DISPATCH-PROMPT.md` (`restart/skinny/tranches/sk-v15/HANDOFF.md:13`-`18`).

### Delete-Before-Provider

Delete-before-provider remains blocked across the packet. The 3C LOCKS proposal
requires line-1 provenance, rostered generator, byte-equivalent non-writing
regen/check proof, and a same-wave replacement provider before deletion or
retirement (`restart/audit/totality/p3/3C-locks-v+1-diff.md:50`). 3B splits W4
Pattern H into provenance gate, generator/check proof, runtime projection,
destructive deletion, and close transcript, and the destructive deletion row
requires same-wave replacement proof before deletion
(`restart/audit/totality/p3/3B-master-plan-reconciliation.md:137`-`150`).

3D and 3E preserve the CSS/provider sequencing: `CSS_GENERATED_RS`,
`CssFullParseSummary`, fact-stream-only CSS `parse()`, and brace-counter proof
retire only with or after typed CSS value/document/view/visitor proof and W6
same-workload retime (`restart/audit/totality/p3/3D-skinny-fold.md:61`;
`restart/audit/totality/p3/3E-grammar-generalisation.md:69`). 3F carries the
same rule into MIGRATION: no delete, retirement, provider/template removal, old
CSS proof retirement, or runtime-shim deletion may occur before its rebuild
provider lands no later than the delete wave (`restart/audit/totality/p3/3F-migration-handoff.md:42`).

The SK-V15 dependency table enforces this before redress: every delete,
retirement, diagnostic demotion, or neutralization must match a dependency row,
and missing rows reject the plan (`restart/skinny/tranches/sk-v15/SPEC.md:187`-`190`).
The concrete dependency rows block `CSS_GENERATED_RS`, CSS summary/fact-stream
proof, provider/template deletion, Pattern H destructive delete, and legacy CSS
runtime-shim deletion until the named provider/proof gates exist
(`restart/skinny/tranches/sk-v15/SPEC.md:195`-`199`).

### Corrective Gates

The V3/V4/V5/V6/V7/V8 corrective gates remain preserved as historical
pre-block evidence. REDRESS records V3/V4 as dispatch-route amendments only,
with REDRESS-183/184 remaining historical rejection or blocker records while CSS
provider/template deletion moved later (`skinny/REDRESS.md:5095`-`5101`);
V4 PRUNE rows reclassify CSS as ledger state only and perform no source,
generator, provider, template, runtime-twin, or `regen_css` deletion
(`skinny/REDRESS.md:5122`-`5127`). V5/V6 likewise keep REDRESS-209 and
REDRESS-210 historical while requiring W5A before W5B deletion and W5B-GEN
before W5C-DELETE (`skinny/REDRESS.md:5189`-`5193`;
`skinny/REDRESS.md:5212`-`5217`). V7 requires the W5B-GENR split and keeps
W5C-DELETE/W6/W7/W8-W10 blocked (`skinny/REDRESS.md:5241`-`5245`); V8
formalizes W5B-FRONTEND sub-waves and keeps W5D-DELETE/W6/W7/W8-W10 blocked
until aggregate close (`skinny/REDRESS.md:5268`-`5272`).

The target packet carries those gates forward: 3F says the MIGRATION gate
prevents the V3/V4/V5/V6/V7/V8 delete-before-provider failure pattern from
re-entering under SK-V15 (`restart/audit/totality/p3/3F-migration-handoff.md:42`);
3D asks Pass Omega to preserve old REDRESS preblocks while applying the new
dependency rows (`restart/audit/totality/p3/3D-skinny-fold.md:99`); and the SK-V15
pre-block list explicitly retains REDRESS 183/184/209-213, REDRESS 215, and FNV
closed-enum production migration blocks (`restart/skinny/tranches/sk-v15/SPEC.md:467`-`484`).

## Verdict

ACCEPT. V3 folds the CH4 field-coverage repairs without reopening REDRESS
routes, reviving stale SK-V13/SK-V14 dispatch blocks, weakening provider-before-delete
guards, moving deletion ahead of dependency rows, or regressing the V3/V4/V5/V6/V7/V8
corrective gates.
