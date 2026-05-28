---
challenge_agent: CH7
name: OVERFIT-PRUNE
pass: T-P3-synthesis
cycle: V1
verdict: ACCEPT
owned_output: restart/audit/totality/p3/hardening/V1/CH7.md
---

# CH7 OVERFIT-PRUNE

## Verdict

ACCEPT.

The T-P3 V1 packet closes CH7 because it does more than document the PASS-IMPL
V1 contrivances. It routes each CH7 failure class into proposed lock text,
MASTER/MIGRATION/HANDOFF receivers, dependency rows, and fail-closed gates:
broadcast CSS admission is non-admit; CSS fake parity is blocked until typed
provider plus same-workload `cssparser` proof; gate exclusions must self-report;
wrong-host evidence is diagnostic only; FNV closed-enum products stay bench-only;
and delete/retirement rows require provider proof no later than the delete wave.

This is not a cycle-level clean-close claim. PASS-3 warns that V1 all-ACCEPT is a
paper-close smell (`restart/prompts/totality/PASS-3-SYNTHESIS.md:92`-`100`);
CH7 ACCEPT is defensible because the lens ran executable checks, the packet
touches no live V1 spec surface, and the target artifacts bind the contrivances
to explicit receivers/gates rather than treating them as solved implementation.
Later Pass Omega CRUD can still misapply the proposal; that is residual risk, not
a CH7 target-packet defect.

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
# no output; exit 0
```

```sh
awk '/^```diff$/{in_diff=1; next} in_diff && /^```$/{exit} in_diff {print}' \
  restart/audit/totality/p3/3C-locks-v+1-diff.md > /tmp/tp3-locks-v1.diff
git apply --check /tmp/tp3-locks-v1.diff
```

```text
# no output; exit 0
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
git diff --name-only 0a0508acd^ 0a0508acd -- \
  restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/locks/LOCKS.md \
  restart/HANDOFF.md restart/MIGRATION.md
```

```text
# no output; target packet does not edit live V1 spec surfaces
```

```sh
rg -n "broadcast_group_id|measurement_row_id|repeated throughput|W8R|diagnostic aggregate" \
  restart/audit/totality/p3/3A-architecture-synthesis.md \
  restart/audit/totality/p3/3B-master-plan-reconciliation.md \
  restart/audit/totality/p3/3C-locks-crystallisation.md \
  restart/audit/totality/p3/3C-locks-v+1-diff.md \
  restart/audit/totality/p3/3D-skinny-fold.md \
  restart/audit/totality/p3/3E-grammar-generalisation.md \
  restart/audit/totality/p3/3F-migration-handoff.md
```

```text
restart/audit/totality/p3/3A-architecture-synthesis.md:53:CSS 24/24 is audit-demoted to one diagnostic aggregate until typed CSS provider W5 and cssparser same-workload retime W6.
restart/audit/totality/p3/3C-locks-v+1-diff.md:52:Repeated throughput tuples across conceptual row IDs are non-admit unless each row has independent command/input/equality/timing.
restart/audit/totality/p3/3D-skinny-fold.md:58:V1 should bind measurement_row_id, measurement_origin, value_plane, css_comparator_workload, and broadcast_group_id.
restart/audit/totality/p3/3E-grammar-generalisation.md:66:every repeated CSS throughput tuple across distinct row ids is non-admit unless each row supplies independent command/input/equality/timing and measurement identity.
restart/audit/totality/p3/3F-migration-handoff.md:66:Demote/collapse W8R broadcast; no CSS live admit from shared tuple.
```

```sh
rg -n "excluded roots|gate-exclusion|self-exempt|included roots|Lock 14/16" \
  restart/audit/totality/p3/3A-architecture-synthesis.md \
  restart/audit/totality/p3/3B-master-plan-reconciliation.md \
  restart/audit/totality/p3/3C-locks-crystallisation.md \
  restart/audit/totality/p3/3C-locks-v+1-diff.md \
  restart/audit/totality/p3/3D-skinny-fold.md \
  restart/audit/totality/p3/3E-grammar-generalisation.md \
  restart/audit/totality/p3/3F-migration-handoff.md
```

```text
restart/audit/totality/p3/3A-architecture-synthesis.md:56:gate reports to list included roots, excluded roots, owner, reason, self-scan status, primitive status, gate consumer, affected rows, and disposition; self-exempting gates fail.
restart/audit/totality/p3/3C-locks-v+1-diff.md:60:Lock 14 gates report included roots, excluded roots, owner, reason, self-scan status, primitive status, gate consumer, affected rows, and disposition.
restart/audit/totality/p3/3D-skinny-fold.md:60:self-exempting scans reject.
restart/audit/totality/p3/3E-grammar-generalisation.md:73:same-change leak roots cannot be excluded silently.
restart/audit/totality/p3/3F-migration-handoff.md:67:Full-surface scan roots and exclusion reports are gate-consumed.
```

```sh
rg -n "FNV|closed-enum|production arbiter|correctness proof|bench-only|W11L|W11N|W11O" \
  restart/audit/totality/p3/3A-architecture-synthesis.md \
  restart/audit/totality/p3/3B-master-plan-reconciliation.md \
  restart/audit/totality/p3/3D-skinny-fold.md \
  restart/audit/totality/p3/3F-migration-handoff.md
```

```text
restart/audit/totality/p3/3A-architecture-synthesis.md:62:W11L/W11N/W11O FNV closed-enum products and generated CSS input_fnv64 outputs as bench/telemetry only; they cannot become runtime selectors, production arbiters, CSS Value API proof, retained identity, or semantic correctness proof.
restart/audit/totality/p3/3B-master-plan-reconciliation.md:124:FNV remains bench-only; adversarial fixtures consumed.
restart/audit/totality/p3/3D-skinny-fold.md:63:W11L/W11N/W11O closed-enum FNV products remain bench-only and cannot become production equality arbiters or correctness proof.
restart/audit/totality/p3/3F-migration-handoff.md:72:FNV remains bench-only; no production arbiter.
```

## Findings

| id | CH7 surface | status | target file:line evidence | controlling evidence | assessment |
|---|---|---|---|---|---|
| CH7-V1-01 | Wave-graph cycles | PASS | `restart/audit/totality/p3/3B-master-plan-reconciliation.md:107`-`125`, `restart/audit/totality/p3/3D-skinny-fold.md:66`, `restart/audit/totality/p3/3F-migration-handoff.md:41`-`42`, `restart/audit/totality/p3/3F-migration-handoff.md:106`-`111` | `restart/skinny/tranches/sk-v15/SPEC.md:31`-`43`, `restart/skinny/tranches/sk-v15/SPEC.md:187`-`204`; addendum at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:146`-`149` | The packet gives W0-W11 order, blocks direct implementation from T-P3, and maps delete/retire work to dependency rows. No receiver row deletes X before its provider lands no later than the delete/retire wave. |
| CH7-V1-02 | Broadcast admission | PASS | `restart/audit/totality/p3/3A-architecture-synthesis.md:53`, `restart/audit/totality/p3/3C-locks-crystallisation.md:45`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:52`, `restart/audit/totality/p3/3E-grammar-generalisation.md:66`, `restart/audit/totality/p3/3F-migration-handoff.md:66` | PASS-IMPL broadcast finding at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21`-`31`; SK-V15 telemetry and dependency rows at `restart/skinny/tranches/sk-v15/SPEC.md:100`-`122`, `restart/skinny/tranches/sk-v15/SPEC.md:194` | The old 24-row CSS admit cannot close. V1 requires row-local identity/equality/timing or explicit aggregate diagnostic status, with `measurement_row_id` and `broadcast_group_id` as gate-consumed fields. |
| CH7-V1-03 | Gate exclusions | PASS | `restart/audit/totality/p3/3A-architecture-synthesis.md:56`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:116`, `restart/audit/totality/p3/3C-locks-crystallisation.md:49`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:60`, `restart/audit/totality/p3/3D-skinny-fold.md:60`, `restart/audit/totality/p3/3E-grammar-generalisation.md:73`, `restart/audit/totality/p3/3F-migration-handoff.md:67` | Gate hole finding at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:45`-`47`; SK-V15 gate rule at `restart/skinny/tranches/sk-v15/SPEC.md:219`-`244` | V1 does not rely on the old allowlisted scan. It requires included roots, excluded roots, owner, reason, self-scan status, primitive status, gate consumer, affected rows, and disposition; self-exempting gates reject. |
| CH7-V1-04 | CSS fake parity / wrong comparator plane | PASS | `restart/audit/totality/p3/3A-architecture-synthesis.md:53`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:93`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:119`-`120`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:52`, `restart/audit/totality/p3/3D-skinny-fold.md:49`, `restart/audit/totality/p3/3E-grammar-generalisation.md:65`, `restart/audit/totality/p3/3F-migration-handoff.md:70` | PASS-IMPL CSS fake parity findings at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21`-`33`, `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:56`-`58`; SK-V15 close conditions at `restart/skinny/tranches/sk-v15/SPEC.md:54`-`63`, W5/W6 gates at `restart/skinny/tranches/sk-v15/SPEC.md:336`-`376` | The packet requires typed CSS value/document/view/visitor proof before old proof retirement and makes `cssparser` the near-term same-workload comparator. `lightningcss` remains diagnostic until comparable CSSOM/value output exists. |
| CH7-V1-05 | Wrong-host close evidence | PASS | `restart/audit/totality/p3/3A-architecture-synthesis.md:60`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:139`, `restart/audit/totality/p3/3C-locks-crystallisation.md:47`, `restart/audit/totality/p3/3C-locks-crystallisation.md:91`-`92`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:62`-`64`, `restart/audit/totality/p3/3E-grammar-generalisation.md:83`, `restart/audit/totality/p3/3E-grammar-generalisation.md:95` | Host-bound admission constraints at `restart/skinny/tranches/sk-v15/SPEC.md:135`-`145`; PASS-IMPL target-cpu/native warning at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:71`-`73` | V1 separates Apple M5 Max/aarch64 admission from x86/AVX-512 diagnostics and requires scalar oracle, strict parity/checkasm, hardware gate, same-wave consumer, row movement, and source-present disposition before primitive close. |
| CH7-V1-06 | FNV bench-contrivance leakage | PASS | `restart/audit/totality/p3/3A-architecture-synthesis.md:62`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:124`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:140`, `restart/audit/totality/p3/3D-skinny-fold.md:63`, `restart/audit/totality/p3/3F-migration-handoff.md:72` | PASS-IMPL bench-only FNV finding at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:60`-`65`; SK-V15 W10 gate at `restart/skinny/tranches/sk-v15/SPEC.md:430`-`445` | FNV is explicitly quarantined to bench/telemetry. V1 forbids production FNV arbiter, runtime selector, CSS Value API proof, retained identity, semantic correctness proof, and production migration without a future contract. |
| CH7-V1-07 | Delete-before-provider sequencing | PASS | `restart/audit/totality/p3/3A-architecture-synthesis.md:55`, `restart/audit/totality/p3/3A-architecture-synthesis.md:88`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:137`, `restart/audit/totality/p3/3C-locks-crystallisation.md:44`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:50`, `restart/audit/totality/p3/3D-skinny-fold.md:59`, `restart/audit/totality/p3/3E-grammar-generalisation.md:74`, `restart/audit/totality/p3/3F-migration-handoff.md:42`, `restart/audit/totality/p3/3F-migration-handoff.md:68`-`70` | SK-V15 dependency rows at `restart/skinny/tranches/sk-v15/SPEC.md:195`-`199`, preblock at `restart/skinny/tranches/sk-v15/SPEC.md:481`; PASS-IMPL addendum at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:146`-`149` | Header-only generated status, fake `@generated` templates, `CSS_GENERATED_RS` centralization, provider/template deletion, and Pattern H destructive delete are blocked until same-wave or prior replacement proof exists. |

## Repair Directives

None for CH7. No CH7 `REVISE` or `REJECT` finding is raised.

## Residual Risk

1. Pass Omega CRUD must preserve the fail-closed language when it applies the
   proposed deltas to live `ARCHITECTURE.md`, `MASTER-PLAN.md`, `LOCKS.md`,
   `MIGRATION.md`, and `HANDOFF.md`. A CRUD edit that weakens any CH7 gate should
   be challenged in Omega, not treated as covered by this ACCEPT.
2. W1 still has a legitimate representation choice: one diagnostic CSS aggregate
   or 24 explicit non-admit rows with broadcast metadata. Either is CH7-safe only
   if no shared W8R tuple remains a live admit.
3. This audit validates the T-P3 proposal packet, not SK-V15 implementation.
   CSS typed provider, same-workload retime, full-surface gate reports, aarch64
   primitive proof, and FNV adversarial fixtures remain future executable work.
4. The worktree was dirty before CH7 was written. The required invariant checks
   still returned 16 locks and 67 Pattern H runtime files, and the target commit
   did not edit live V1 spec surfaces.
