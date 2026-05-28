# T-P3 V2 CH3 Regression Hardening

Verdict: ACCEPT

Target packet: `7885b29ab` (`docs(sk-v15-t-p3): fold V1 hardening into V2 synthesis`)
Context commit: `d1d073a50`

## Required Local Checks

- `git show --stat --oneline 7885b29ab -- restart/audit/totality/p3`: target packet resolves and changes the seven T-P3 artifacts only, with `287 insertions(+), 206 deletions(-)`.
- `git diff --check 7885b29ab^ 7885b29ab -- restart/audit/totality/p3`: clean, no whitespace/error output.
- Extracted `restart/audit/totality/p3/3C-locks-v+1-diff.md` fenced diff to `/tmp/tp3-locks-v2.diff`; `git apply --check /tmp/tp3-locks-v2.diff`: clean, no output.
- `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md`: `16`.
- `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`: `67`.
- Required stale-pattern `rg` over 3A..3F: no matches. This includes no revived `ORCHESTRATOR-PROMPT`, no stale `2F-parse-that-gaps.md:518`, no bad regex owner pairing, no docs-only cleanup route, and no stale open-question receiver phrasing.

## Regression Audit

No defects found.

### REDRESS Routes

The packet does not reopen REDRESS as an implementation escape route. The target 3F handoff keeps all implementation waves blocked until Pass Omega V5 CRUD-4 updates HANDOFF/MIGRATION and G-Omega authorizes the required V1 patches (`restart/audit/totality/p3/3F-migration-handoff.md:83`-`85`). It also routes current wave authority through the extant SK-V15 dispatch prompt, not a missing or stale authority path (`restart/audit/totality/p3/3F-migration-handoff.md:86`-`89`). The SK-V15 SPEC preserves fail-closed handling: misses become REDRESS, revert, demotion, or intrinsic block with proof, not closure (`restart/skinny/tranches/sk-v15/SPEC.md:82`-`83`), and W11 cannot close while any dependency row lacks proof or intrinsic-block evidence (`restart/skinny/tranches/sk-v15/SPEC.md:447`-`465`).

### Stale Receiver Blocks

Stale SK-V13/SK-V14 receiver blocks are not revived as current dispatch. 3B explicitly identifies the stale receiver framing and requires a new SK-V15 MASTER receiver block while reclassifying old CSS and SK-V14 rows as historical/pre-block evidence (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:30`-`35`). The old SK-V14 W0..W11 and MP-NW blocks are marked historical/superseded, with refusal rows retained only as pre-block evidence (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:111`-`112`). The concrete MASTER delta repeats that they must not be current dispatch (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:158`), and the consequences table preserves old SK-V14/MP.NW rows only as history and REDRESS pre-blocks (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:174`).

3F applies the same rule to HANDOFF/MIGRATION: existing Pass Omega V2..V8 sections remain historical SK-V14 receiver records, not current SK-V15 dispatch authority (`restart/audit/totality/p3/3F-migration-handoff.md:40`), and the current override must route cold-start agents to SK-V15 T-P3/Pass Omega V5 plus `DISPATCH-PROMPT.md`, not historical SK-V14 dispatch (`restart/audit/totality/p3/3F-migration-handoff.md:44`).

### Delete Before Provider

Delete-before-provider sequencing remains blocked. 3C makes provider-before-delete proof mandatory for generated output, and explicitly keeps header-only or delete-before-provider paths blocked (`restart/audit/totality/p3/3C-locks-crystallisation.md:49`). The 3C cost/gate matrix assigns this to W3/W4/W5/W6/W11 generator provenance, non-writing regen/check, and provider-before-delete gates (`restart/audit/totality/p3/3C-locks-crystallisation.md:68`).

3B splits Pattern H W4 into separate provenance, generator/check, projection, destructive deletion, and transcript rows so deletion cannot hide inside a generic close claim (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:136`-`149`). The destructive deletion sub-row requires a matching dependency row and same-wave replacement proof before deletion, with delete-before-provider reverted or REDRESSed and no deletion-only close (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:148`).

3F carries the same gate to MIGRATION: no delete, retirement, provider/template removal, old CSS proof retirement, or runtime-shim deletion may occur before the rebuild provider lands no later than the delete wave (`restart/audit/totality/p3/3F-migration-handoff.md:42`). The SK-V15 dependency table enforces this row-by-row before redress: every delete, retirement, diagnostic demotion, or neutralization needs a matching row, and missing rows reject the plan (`restart/skinny/tranches/sk-v15/SPEC.md:187`-`190`). The relevant rows block CSS provider/template deletion before W6 proof, destructive Pattern H deletion without proof, and legacy runtime shim removal without replacement proof (`restart/skinny/tranches/sk-v15/SPEC.md:195`-`199`).

### V3 Through V8 Corrective Gates

The V3/V4/V5/V6/V7/V8 corrective gates are preserved as pre-block evidence, not regressed. REDRESS records V3/V4 as route amendments only: REDRESS-183 remains the historical rejection, W2/W3 admitted under amended scope, and CSS provider/template deletion moved later (`skinny/REDRESS.md:5095`-`5101`). V4 PRUNE rows reclassify CSS as ledger state only and leave deletion routed after replacement provider generation exists (`skinny/REDRESS.md:5122`-`5127`).

For V5 and V6, REDRESS records G-Omega as dispatch-route-only amendments: REDRESS-209 remains historical while current dispatch requires W5A before W5B deletion (`skinny/REDRESS.md:5189`-`5193`), and REDRESS-210 remains historical while current dispatch requires W5B-GEN before W5C-DELETE (`skinny/REDRESS.md:5212`-`5217`). V7 requires the W5B-GENR split and keeps W5C-DELETE/W6/W7/W8-W10 blocked until the PRUNE chain is rerouted and closed (`skinny/REDRESS.md:5241`-`5245`). V8 formalizes W5B-FRONTEND sub-waves and keeps W5D-DELETE/W6/W7/W8-W10 blocked until aggregate W5B-FRONTEND close (`skinny/REDRESS.md:5268`-`5272`).

3F directly carries this correction: the MIGRATION gate clause says it prevents the V3/V4/V5/V6/V7/V8 delete-before-provider failure pattern from re-entering under SK-V15 (`restart/audit/totality/p3/3F-migration-handoff.md:42`). The SK-V15 pre-block list also keeps REDRESS 183/184/209-213 and 215 blocked, including "No provider/runtime/template delete before replacement proof" and "No CSS broadcast, brace-counter, or wrong-plane comparator admission" (`restart/skinny/tranches/sk-v15/SPEC.md:467`-`484`).

## Disposition

ACCEPT. No REDRESS route is reopened, stale SK-V13/SK-V14 blocks remain historical/pre-block evidence, delete/retire rows require provider proof before or in the same wave, and the V3/V4/V5/V6/V7/V8 corrective gates remain fail-closed.
