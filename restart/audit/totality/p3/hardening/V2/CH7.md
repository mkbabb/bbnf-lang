# CH7 OVERFIT-PRUNE - ACCEPT

Target packet: `7885b29ab` (`docs(sk-v15-t-p3): fold V1 hardening into V2 synthesis`).
Context commit: `d1d073a50`. The seven target artifacts in the worktree match
`7885b29ab`; only the V2 challenge context is context-local.

## Required Checks

- `git show --stat --oneline 7885b29ab -- restart/audit/totality/p3`: target
  resolves as `7885b29ab docs(sk-v15-t-p3): fold V1 hardening into V2 synthesis`,
  7 files changed, 287 insertions, 206 deletions.
- `git diff --check 7885b29ab^ 7885b29ab -- restart/audit/totality/p3`: clean,
  no whitespace errors.
- Extracted `3C-locks-v+1-diff.md` diff and ran `git apply --check
  /tmp/tp3-locks-v2.diff`: clean.
- `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md`: `16`.
- `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`:
  `67`.
- Required stale-pattern `rg` over 3A..3F: no matches, exit 1 as expected.

## Lens Result

No CH7 defects found. V2 preserves the SK-V15 forward-lens addenda and blocks
the overfit routes that V1 had to fold.

- Wave-graph cycles and delete-before-provider sequencing: target text requires
  same-wave or prior replacement proof before deletion/retirement in 3A
  (`restart/audit/totality/p3/3A-architecture-synthesis.md:63`), 3B
  (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:145`-`149`),
  3C (`restart/audit/totality/p3/3C-locks-crystallisation.md:49`,
  `restart/audit/totality/p3/3C-locks-v+1-diff.md:49`), 3D
  (`restart/audit/totality/p3/3D-skinny-fold.md:59`, `restart/audit/totality/p3/3D-skinny-fold.md:66`),
  3E (`restart/audit/totality/p3/3E-grammar-generalisation.md:76`), and 3F
  (`restart/audit/totality/p3/3F-migration-handoff.md:42`,
  `restart/audit/totality/p3/3F-migration-handoff.md:118`-`121`). This matches
  the forward lens and dependency rows (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:148`-`150`;
  `restart/skinny/tranches/sk-v15/SPEC.md:187`-`204`,
  `restart/skinny/tranches/sk-v15/SPEC.md:481`).
- Broadcast admission: target rows demote/collapse the CSS 24-row broadcast and
  require row-local measurement identity before live admission
  (`restart/audit/totality/p3/3A-architecture-synthesis.md:61`,
  `restart/audit/totality/p3/3B-master-plan-reconciliation.md:123`-`124`,
  `restart/audit/totality/p3/3C-locks-crystallisation.md:50`,
  `restart/audit/totality/p3/3E-grammar-generalisation.md:68`,
  `restart/audit/totality/p3/3F-migration-handoff.md:65`-`66`). This closes the
  PASS-IMPL broadcast blocker (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21`-`31`)
  and SK-V15 telemetry/close gates (`restart/skinny/tranches/sk-v15/SPEC.md:54`-`63`,
  `restart/skinny/tranches/sk-v15/SPEC.md:100`-`122`,
  `restart/skinny/tranches/sk-v15/SPEC.md:264`-`281`).
- Gate exclusions and self-exempting grep gates: V2 requires included/excluded
  roots, owners, reasons, self-scan status, primitive status, gate consumer, and
  disposition, with self-exempting scans rejected (`restart/audit/totality/p3/3A-architecture-synthesis.md:64`,
  `restart/audit/totality/p3/3C-locks-v+1-diff.md:59`,
  `restart/audit/totality/p3/3D-skinny-fold.md:60`,
  `restart/audit/totality/p3/3E-grammar-generalisation.md:75`,
  `restart/audit/totality/p3/3F-migration-handoff.md:67`). This matches the
  known allowlist-hole evidence (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:45`-`47`;
  `restart/audit/totality/p1/1E-locks-evidence.md:140`,
  `restart/audit/totality/p1/1E-locks-evidence.md:195`-`200`;
  `restart/skinny/tranches/sk-v15/SPEC.md:233`-`244`).
- CSS fake parity: V2 keeps CSSOM/lightningcss parity out of W5/W6 close unless
  typed CSS value/document/view/visitor output and same-workload cssparser proof
  exist (`restart/audit/totality/p3/3A-architecture-synthesis.md:61`,
  `restart/audit/totality/p3/3B-master-plan-reconciliation.md:128`-`129`,
  `restart/audit/totality/p3/3E-grammar-generalisation.md:67`,
  `restart/audit/totality/p3/3F-migration-handoff.md:70`). This conflicts with
  no target claim; it follows the fake-parity refutations
  (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:29`,
  `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:56`-`58`;
  `restart/skinny/tranches/sk-v15/SPEC.md:336`-`376`).
- Wrong-host close evidence: CollapsedStage/x86 evidence stays diagnostic unless
  Apple M5 Max/aarch64 proof, scalar oracle, parity/checkasm-equivalent evidence,
  hardware gate, same-wave consumer, and row movement exist
  (`restart/audit/totality/p3/3A-architecture-synthesis.md:68`,
  `restart/audit/totality/p3/3B-master-plan-reconciliation.md:132`,
  `restart/audit/totality/p3/3C-locks-crystallisation.md:52`,
  `restart/audit/totality/p3/3C-locks-v+1-diff.md:63`,
  `restart/audit/totality/p3/3E-grammar-generalisation.md:97`). This matches
  2D/2E host evidence (`restart/audit/totality/p2/2D-cost-model.md:97`,
  `restart/audit/totality/p2/2D-cost-model.md:118`;
  `restart/audit/totality/p2/2E-host-arch-esoterica.md:25`-`32`,
  `restart/audit/totality/p2/2E-host-arch-esoterica.md:68`-`82`).
- FNV bench leakage: V2 keeps W11L/W11N/W11O FNV products bench-only and routes
  production scans/quarantine to W10 (`restart/audit/totality/p3/3A-architecture-synthesis.md:70`,
  `restart/audit/totality/p3/3B-master-plan-reconciliation.md:133`,
  `restart/audit/totality/p3/3D-skinny-fold.md:63`,
  `restart/audit/totality/p3/3F-migration-handoff.md:72`). This matches the
  bench-only leakage evidence and SK-V15 quarantine gate
  (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:60`-`65`,
  `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:141`;
  `restart/skinny/tranches/sk-v15/SPEC.md:430`-`445`,
  `restart/skinny/tranches/sk-v15/SPEC.md:484`).

Defects: none.
