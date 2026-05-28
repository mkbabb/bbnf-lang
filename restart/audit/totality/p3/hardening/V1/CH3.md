# CH3 REGRESSION - T-P3 V1

## Verdict

ACCEPT.

The V1 packet does not reopen a REDRESS route, does not revive stale SK-V13/SK-V14 receiver blocks as current dispatch, keeps delete-before-provider sequences blocked, and keeps SK-V15 implementation waves behind Pass Omega CRUD plus G-Omega. The packet is proposal-only; no live V1 spec surface is amended by this CH3 file.

## Evidence Commands And Outputs

```sh
git show --stat --oneline 0a0508acd -- restart/audit/totality/p3
```

```text
0a0508acd docs(sk-v15-t-p3): add V1 synthesis packet
 .../audit/totality/p3/3A-architecture-synthesis.md |  92 ++++++++++++
 .../totality/p3/3B-master-plan-reconciliation.md   | 167 +++++++++++++++++++++
 .../audit/totality/p3/3C-locks-crystallisation.md  | 114 ++++++++++++++
 restart/audit/totality/p3/3C-locks-v+1-diff.md     |  76 ++++++++++
 restart/audit/totality/p3/3D-skinny-fold.md        |  93 ++++++++++++
 .../audit/totality/p3/3E-grammar-generalisation.md | 145 ++++++++++++++++++
 restart/audit/totality/p3/3F-migration-handoff.md  | 120 +++++++++++++++
 7 files changed, 807 insertions(+)
```

```sh
git diff --check 0a0508acd^ 0a0508acd -- restart/audit/totality/p3
```

```text
<no output; exit 0>
```

```sh
awk '/^```diff$/{in_diff=1; next} in_diff && /^```$/{exit} in_diff {print}' \
  restart/audit/totality/p3/3C-locks-v+1-diff.md > /tmp/tp3-locks-v1.diff
git apply --check /tmp/tp3-locks-v1.diff
```

```text
<no output; exit 0>
```

```sh
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
```

```text
16
      67
```

```sh
rg -n "(historical / superseded|not implementation dispatch|Only after|No SK-V15 implementation|Pass Omega/G-Omega -> SK-V15 W0)" \
  restart/audit/totality/p3/3B-master-plan-reconciliation.md \
  restart/audit/totality/p3/3F-migration-handoff.md
```

```text
restart/audit/totality/p3/3B-master-plan-reconciliation.md:102:| SK-V14 W0..W11 block | historical / superseded | MASTER carries SK-V14 W0..W11 as authoritative receiver block (`restart/MASTER-PLAN.md:751-827`), but SK-V15 S-P3 locked a new W0..W11 contract (`restart/skinny/tranches/sk-v15/research/p3/hardening/HARDENING-S-P3-V4-CONSOLIDATED.md:35-49`). | New SK-V15 W0..W11 receiver block. |
restart/audit/totality/p3/3B-master-plan-reconciliation.md:103:| MP-NW-01..14 block | historical / superseded except refusal rows | MASTER's 14 NEW rows mirror older MP.NW/SK-V14 commitments (`restart/MASTER-PLAN.md:837-869`). SK-V15 now consumes the same failure classes through W0-W11 and explicit DEP rows (`restart/skinny/tranches/sk-v15/SPEC.md:187-205`). | Keep as history/pre-block; do not use as current dispatch manifest. |
restart/audit/totality/p3/3B-master-plan-reconciliation.md:107:These are proposed MASTER receiver rows, not implementation dispatch. They are
restart/audit/totality/p3/3F-migration-handoff.md:46:| 3F-MH-007 | Replace the current "Pass Omega V8 next-cycle dispatch directive" with a **Pass Omega V5/G-Omega -> SK-V15 W0** directive: T-P3 locks, G3 auto-passes under active pin, Pass Omega V5 runs, CRUD-4 updates HANDOFF/MIGRATION, G-Omega authorizes required V1 patches, then W0 dispatches through SKINNY triumvirate. Implementation waves remain blocked until CRUD closes and G-Omega authorizes required spec patches. | T-P3 3F row requires next directive after Pass Omega V5 and says implementation waves do not begin until Pass Omega CRUD closes and G-Omega authorizes patches (`restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md:118`-`121`). Pass Omega CRUD-4 owns HANDOFF+MIGRATION (`restart/prompts/pass-contracts/PASS-OMEGA.md:57`-`74`) and G-Omega controls merge/next-cycle dispatch (`restart/prompts/pass-contracts/PASS-OMEGA.md:94`-`110`). | `restart/HANDOFF.md` next-cycle directive (`restart/HANDOFF.md:149`-`177`). | Receiver: Pass Omega V5 then SK-V15 W0. Blocker: no CRUD-4, no G-Omega, or unresolved invariant. Gate: G-Omega; then W0 entry gate in SK-V15 SPEC (`restart/skinny/tranches/sk-v15/SPEC.md:29`-`43`, `restart/skinny/tranches/sk-v15/SPEC.md:488`-`494`). | Gives the next worker a concrete, measurable dispatch path and prevents direct implementation dispatch from T-P3 prose. |
restart/audit/totality/p3/3F-migration-handoff.md:84:No SK-V15 implementation wave dispatches until Pass Omega V5 CRUD-4 has updated
restart/audit/totality/p3/3F-migration-handoff.md:110:5. Only after Pass Omega V5 CRUD-4 has closed and G-Omega has authorized the HANDOFF/MIGRATION patches may the orchestrator update HANDOFF to `ready-for-wave-W0` and dispatch SK-V15 W0 (`restart/skinny/tranches/sk-v15/SPEC.md:488`-`494`). W0..W11 then follow the dependency order in the dispatch lock (`restart/skinny/tranches/sk-v15/SPEC.md:29`-`43`) and the manifest (`restart/skinny/tranches/sk-v15/SPEC.md:172`-`185`).
```

```sh
rg -n "(DEP-W6-CSS-GENERATED-RS|DEP-W3-W6-CSS-PROVIDER-TEMPLATE|DEP-W4-PATTERN-H-PROVENANCE|DEP-W10-FNV-QUARANTINE|REDRESS 183/184/209-213|REDRESS 215|FNV closed enum)" \
  restart/skinny/tranches/sk-v15/SPEC.md \
  restart/audit/totality/p3/3F-migration-handoff.md
```

```text
restart/skinny/tranches/sk-v15/SPEC.md:195:| `DEP-W6-CSS-GENERATED-RS` | `CSS_GENERATED_RS` string-literal parser evidence and byte-identical generated CSS bodies as live parser proof | W6 | W5 typed CSS Value provider | `rg -n "CSS_GENERATED_RS|hand_written:CSS_GENERATED_RS"` over live admission paths plus CSS typed tests. | `yes:same-wave` in W6 | `blocked` before W6 | W1, W3, W5, W6, W11 | REDRESS 183/184/209-213/215 |
restart/skinny/tranches/sk-v15/SPEC.md:197:| `DEP-W3-W6-CSS-PROVIDER-TEMPLATE` | CSS provider/template/static profile roster and runtime family fanout | W3 neutralization; W6 deletion | W3 generic contract; W5/W6 typed CSS provider/proof for deletion | Lock 14 scan over codegen roots, generated-output diff, JSON 51/51 rerun if JSON-adjacent, W6 CSS typed proof for deletion. | `yes:same-wave` for neutralization; `no:block` for deletion before W6 | `blocked` for deletion before W6 | W2, W3, W5, W6, W11 | REDRESS 184/209-213 |
restart/skinny/tranches/sk-v15/SPEC.md:198:| `DEP-W4-PATTERN-H-PROVENANCE` | Pattern H root runtime files lacking true line-1 generated provenance | W4 provenance repair; destructive delete only with proof | W4 root runtime generator/check | `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`; line-1 provenance scan; non-writing regen check. | `yes:same-wave` for provenance proof; `no:block` for destructive delete without proof | `allowed` for truth repair, `blocked` for fake/header-only close | W4, W11 | REDRESS 183/213 |
restart/skinny/tranches/sk-v15/SPEC.md:203:| `DEP-W10-FNV-QUARANTINE` | W11L/W11N/W11O FNV closed-enum or hash-sidecar correctness claim | W10 quarantine only | W10 adversarial semantic fixtures and bench-only metadata | `rg -n "fnv|FNV"` over production runtime/generic codegen roots plus adversarial typed-equality tests. | `yes:same-wave` for quarantine; production migration is `no:block` | `quarantine-only` | W10, W11 | FNV closed-enum production migration |
restart/skinny/tranches/sk-v15/SPEC.md:481:| REDRESS 183/184/209-213 | No provider/runtime/template delete before replacement proof. |
restart/skinny/tranches/sk-v15/SPEC.md:482:| REDRESS 215 | No CSS broadcast, brace-counter, or wrong-plane comparator admission. |
restart/skinny/tranches/sk-v15/SPEC.md:484:| FNV closed enum | Bench-only quarantine; no production FNV arbiter, production hash correctness proof, or production migration without a future contract. |
restart/audit/totality/p3/3F-migration-handoff.md:42:| 3F-MH-003 | Add a MIGRATION gate clause: no delete, retirement, provider/template removal, old CSS proof retirement, or runtime-shim deletion may happen before its rebuild provider lands no later than the delete wave. | T-P1 `C-7` identifies REDRESS-183/184/209..213 as wave-graph-cycle precedent (`restart/audit/totality/p1/1D-skinny-lessons.md:158`-`159`). SK-V15 Synthesis requires delete/rebuild dependency columns (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:102`-`106`). | `restart/MIGRATION.md` sections 17 and 19 gates (`restart/MIGRATION.md:794`-`814`, `restart/MIGRATION.md:833`-`917`). | Receiver: every migration deletion/retirement row. Blocker: absent provider proof or absent dependency row. Gate: SK-V15 dependency rows `DEP-W6-CSS-GENERATED-RS`, `DEP-W6-CSS-SUMMARY-FACT-STREAM`, `DEP-W3-W6-CSS-PROVIDER-TEMPLATE`, `DEP-W4-PATTERN-H-PROVENANCE`, and `DEP-W4-W6-CSS-LEGACY-RUNTIME-SHIM` (`restart/skinny/tranches/sk-v15/SPEC.md:195`-`199`). | Prevents the V3/V4/V5/V6/V7/V8 delete-before-provider failure pattern from re-entering under SK-V15. |
restart/audit/totality/p3/3F-migration-handoff.md:72:| W10 FNV quarantine | FNV remains bench-only; no production arbiter. | production FNV correctness proof | DEP-W10-FNV-QUARANTINE |
```

## Findings

| id | regression surface | file:line evidence | result |
|---|---|---|---|
| CH3-R1 | REDRESS routes are not reopened. CSS broadcast, fact-stream/brace-counter CSS, FNV production migration, sidecar/structural stream families, and decoded-string/string64 retries remain pre-blocked. | `restart/skinny/tranches/sk-v15/SPEC.md:467`-`484`; `restart/audit/totality/p3/3D-skinny-fold.md:48`-`50`, `restart/audit/totality/p3/3D-skinny-fold.md:58`-`63`; `restart/audit/totality/p3/3A-architecture-synthesis.md:53`, `restart/audit/totality/p3/3A-architecture-synthesis.md:62`; `restart/audit/totality/p3/3B-master-plan-reconciliation.md:93`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:140`; `restart/audit/totality/p3/3F-migration-handoff.md:119`. | ACCEPT |
| CH3-R2 | Stale SK-V13/SK-V14 receiver blocks are not revived as current dispatch. They are classified as historical/superseded/pre-block evidence, with SK-V15 W0-W11 as the new pending receiver map. | `restart/audit/totality/p3/3B-master-plan-reconciliation.md:23`-`36`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:102`-`107`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:134`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:141`; `restart/audit/totality/p3/3F-migration-handoff.md:40`, `restart/audit/totality/p3/3F-migration-handoff.md:44`-`46`, `restart/audit/totality/p3/3F-migration-handoff.md:118`. | ACCEPT |
| CH3-R3 | Delete-before-provider sequences remain blocked. The proposed LOCKS addendum requires same-wave replacement provider before deletion/retirement, and MIGRATION gets an explicit no-delete/no-retire gate. | `restart/audit/totality/p3/3C-locks-crystallisation.md:44`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:50`; `restart/audit/totality/p3/3F-migration-handoff.md:42`; `restart/audit/totality/p3/3D-skinny-fold.md:59`, `restart/audit/totality/p3/3D-skinny-fold.md:66`; `restart/skinny/tranches/sk-v15/SPEC.md:140`, `restart/skinny/tranches/sk-v15/SPEC.md:187`-`204`. | ACCEPT |
| CH3-R4 | Dependency rows precede deletion/retirement. Every delete, retirement, diagnostic demotion, or neutralization must match a dependency row before redress, with missing rows rejecting the plan. | `restart/skinny/tranches/sk-v15/SPEC.md:187`-`204`; `restart/skinny/tranches/sk-v15/SPEC.md:272`-`281`, `restart/skinny/tranches/sk-v15/SPEC.md:315`-`316`, `restart/skinny/tranches/sk-v15/SPEC.md:352`-`376`, `restart/skinny/tranches/sk-v15/SPEC.md:455`-`465`; `restart/audit/totality/p3/3F-migration-handoff.md:41`-`42`, `restart/audit/totality/p3/3F-migration-handoff.md:66`-`73`. | ACCEPT |
| CH3-R5 | 3B and 3F do not authorize implementation before Pass Omega/G-Omega. 3B marks new rows as proposed receiver rows, not implementation dispatch; 3F blocks W0 until CRUD-4 and G-Omega authorize V1 patches. | `restart/audit/totality/p3/3B-master-plan-reconciliation.md:35`-`36`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:62`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:107`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:131`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:141`; `restart/audit/totality/p3/3F-migration-handoff.md:25`, `restart/audit/totality/p3/3F-migration-handoff.md:46`, `restart/audit/totality/p3/3F-migration-handoff.md:57`-`60`, `restart/audit/totality/p3/3F-migration-handoff.md:84`-`85`, `restart/audit/totality/p3/3F-migration-handoff.md:106`-`110`. | ACCEPT |
| CH3-R6 | V3/V4/V5/V6/V7/V8 wave-graph failures are not reintroduced. The old failure class was provider/runtime/template deletion or destructive regen before provider/runtime proof; V1 carries it as REDRESS history and binds it to SK-V15 dependency rows. | REDRESS precedents: `skinny/REDRESS.md:5092`-`5118`, `skinny/REDRESS.md:5173`-`5193`, `skinny/REDRESS.md:5197`-`5217`, `skinny/REDRESS.md:5221`-`5245`, `skinny/REDRESS.md:5249`-`5272`, `skinny/REDRESS.md:5276`-`5293`. V1 blockers: `restart/audit/totality/p3/3F-migration-handoff.md:42`; `restart/skinny/tranches/sk-v15/SPEC.md:195`-`199`, `restart/skinny/tranches/sk-v15/SPEC.md:481`; `restart/audit/totality/p3/3C-locks-v+1-diff.md:50`. | ACCEPT |

## Repair Directives

None. No CH3 non-ACCEPT finding is present.

## Residual Risk

1. Pass Omega CRUD can still misapply the proposal text. CH3 acceptance depends on preserving the packet's explicit historical/current split and the Pass Omega/G-Omega implementation block when CRUD-4 edits live `MIGRATION.md` and `HANDOFF.md`.
2. The old "Pass Omega V5" name collision is surfaced by 3F as a CH1/CH6 open question (`restart/audit/totality/p3/3F-migration-handoff.md:117`-`118`). It is not a CH3 blocker because the target packet explicitly labels current SK-V15 routing and keeps historical SK-V14 sections non-dispatch.
3. W1 still must choose whether CSS broadcast evidence becomes one diagnostic aggregate or 24 explicit non-admit rows (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:164`). Both permitted shapes preserve the CH3 invariant: no live CSS admit from W8R broadcast evidence.

## V3/V4/V5/V6/V7/V8 Wave-Graph Justification

The V3/V4/V5/V6/V7/V8 failure class was not merely "old wave names"; it was the graph error of deleting or retiring provider/runtime/template surfaces before the replacement provider existed and was consumed. REDRESS records that W2/W4/W5/W5B/W5B-GEN/W5B-FRONTEND/W6.0 attempts failed or required reroute because generator/runtime/provider proof was absent or oversized (`skinny/REDRESS.md:5092`-`5118`, `skinny/REDRESS.md:5173`-`5293`).

V1 avoids reintroducing that graph in three independent ways:

1. 3B reclassifies SK-V14 W0-W11 and MP-NW-01..14 as historical or superseded, not current dispatch (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:102`-`103`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:134`).
2. 3C and 3F add explicit same-wave provider/dependency guards for deletion and retirement (`restart/audit/totality/p3/3C-locks-v+1-diff.md:50`; `restart/audit/totality/p3/3F-migration-handoff.md:42`).
3. SK-V15 SPEC rows make the graph executable: CSS generated/parser proof cannot retire before W6 with W5 typed provider proof, provider/template deletion is blocked before W6 proof, Pattern H destructive delete is blocked without W4 proof, and FNV production migration is quarantine-only (`restart/skinny/tranches/sk-v15/SPEC.md:195`-`204`, `restart/skinny/tranches/sk-v15/SPEC.md:481`-`484`).

Therefore the packet preserves the REDRESS lessons as lock-strengthening and dependency rows, rather than reopening them as implementation paths.
