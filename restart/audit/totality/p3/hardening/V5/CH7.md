# CH7 Overfit-Prune - T-P3 V5

Verdict: ACCEPT.

## Scope

Target packet: `77b6e9fd7` (`docs(sk-v15-t-p3): repair V4 citation finding`).
Current target artifacts match that packet for the seven T-P3 proposal files; the
only `restart/audit/totality/p3` worktree additions observed before this write
were sibling V5 CH files. This CH7 audit did not stage, commit, or edit any file
other than this report.

V5 is a final confirmation lens over the V3 clean cycle, V4 citation-only
`REVISE`, and V5 citation repair. CH7 found no procedural-overfit defect and no
contrivance route that would let the packet smuggle implementation acceptance
without executable proof.

## Required Local Checks

| check | result |
|---|---|
| `git show --stat --oneline 77b6e9fd7 -- restart/audit/totality/p3` | Clean target scope: one changed file, `restart/audit/totality/p3/3A-architecture-synthesis.md`, with 1 insertion and 1 deletion. |
| `git diff --check 77b6e9fd7^ 77b6e9fd7 -- restart/audit/totality/p3` | Passed with no output. |
| Extracted `3C-locks-v+1-diff.md` hunk then `git apply --check /tmp/tp3-locks-v5.diff` | Passed with no output. |
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16`. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `67`. |
| Required stale-pattern `rg` scan from context | No matches; command exited 1 after printing only the lock/file counts in the combined run. |
| restart `path:line` citation validation across 3A-3F and 3C diff | 790 `restart/**/*.md` references, 36 unique paths, 0 missing paths, 0 out-of-range lines. |
| 3C live 1E/2X coverage | 42 source candidate ids found, 42 covered in 3C; disposition rows are 23 `ACCEPT`, 19 `MODIFY`, 0 `REJECT`, 0 `DEFER`. |

## CH7 Scan

| risk | disposition | target-packet evidence | why no overfit-prune defect remains |
|---|---|---|---|
| Wave-graph cycles | ACCEPT | 3B splits W4 Pattern H into provenance, generator/check, runtime projection, destructive deletion, and close-transcript sub-rows, and deletion requires replacement proof before deletion (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:137`-`150`). 3F blocks delete, retirement, provider/template removal, old CSS proof retirement, and runtime-shim deletion before rebuild proof (`restart/audit/totality/p3/3F-migration-handoff.md:42`). | Non-fit work routes to intrinsic block, REDRESS/revert, or G-Omega wave-graph amendment, not W12 or challenge-time implementation (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:148`-`150`; `restart/audit/totality/p3/3F-migration-handoff.md:118`-`119`). |
| Broadcast admission | ACCEPT | 3A demotes CSS 24/24 to a diagnostic aggregate until W5 typed CSS provider and W6 same-workload retime (`restart/audit/totality/p3/3A-architecture-synthesis.md:64`). The proposed Lock 8 text requires `measurement_row_id` and `broadcast_group_id` and rejects repeated tuples without independent row evidence (`restart/audit/totality/p3/3C-locks-v+1-diff.md:52`). 3F maps W1 to CSS broadcast demotion with no live admit from a shared tuple (`restart/audit/totality/p3/3F-migration-handoff.md:65`-`66`). | Broadcast evidence is preserved as diagnostic/failing telemetry, not admission or row movement. |
| Gate exclusions and self-exempting grep gates | ACCEPT | 3A requires included roots, excluded roots, owner, reason, self-scan status, primitive status, gate consumer, affected rows, and disposition; self-exempting gates fail (`restart/audit/totality/p3/3A-architecture-synthesis.md:67`). 3E states omitted `runtime_generator.rs`, `grammar_provider.rs`, direct JSON generators, or templates are non-evidence and same-change leak-root exclusions fail (`restart/audit/totality/p3/3E-grammar-generalisation.md:77`; `restart/audit/totality/p3/3E-grammar-generalisation.md:120`; `restart/audit/totality/p3/3E-grammar-generalisation.md:149`; `restart/audit/totality/p3/3E-grammar-generalisation.md:162`). | The packet does not let an allowlist define cleanliness; exclusions must be reported and gate-consumed. |
| CSS fake parity | ACCEPT | 3E requires generated typed CSS value/document/view/visitor output, row-local equality, and same-workload `cssparser` retime; fact streams and brace counters stay diagnostic (`restart/audit/totality/p3/3E-grammar-generalisation.md:69`). W5/W6 explicitly do not carry broad CSSOM rewrite scope (`restart/audit/totality/p3/3E-grammar-generalisation.md:127`; `restart/audit/totality/p3/3E-grammar-generalisation.md:141`; `restart/audit/totality/p3/3E-grammar-generalisation.md:159`). | CSSOM/value parity cannot be hidden as scoped provider or retime work; if it becomes implementation scope, it routes to intrinsic block or G-Omega amendment. |
| Wrong-host close evidence | ACCEPT | 3A requires Apple M5 Max/aarch64 primitive gates and rejects source-presence-only or wrong-host close evidence (`restart/audit/totality/p3/3A-architecture-synthesis.md:71`; `restart/audit/totality/p3/3A-architecture-synthesis.md:108`). 3B keeps CollapsedStage diagnostic unless aarch64 proof exists (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:133`; `restart/audit/totality/p3/3B-master-plan-reconciliation.md:163`). 3E states x86 diagnostics do not admit CollapsedStage for current SK-V15 grammars (`restart/audit/totality/p3/3E-grammar-generalisation.md:99`). | x86/AVX-512 evidence remains pressure/diagnostic only; primitive admission needs the close-host manifest and same-wave consumer proof. |
| FNV bench leakage | ACCEPT | 3A quarantines W11L/W11N/W11O FNV products and generated CSS `input_fnv64` outputs as bench/telemetry only, not runtime selectors, production arbiters, CSS Value API proof, retained identity, or semantic correctness proof (`restart/audit/totality/p3/3A-architecture-synthesis.md:73`). 3B adds W10 FNV quarantine and blocks production FNV correctness migration (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:134`; `restart/audit/totality/p3/3B-master-plan-reconciliation.md:165`; `restart/audit/totality/p3/3B-master-plan-reconciliation.md:181`). 3F keeps FNV bench-only with no production arbiter (`restart/audit/totality/p3/3F-migration-handoff.md:72`). | The packet does not promote FNV hashes from telemetry/bench artifacts into runtime identity or correctness. |
| Delete-before-provider sequencing | ACCEPT | 3C's proposed Lock 6/14 clause requires same-wave replacement provider before deletion or retirement and rejects fake generated headers or provider/template deletion before W5/W6 replacement proof (`restart/audit/totality/p3/3C-locks-v+1-diff.md:50`). 3B's destructive-deletion sub-row requires replacement proof before deletion (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:149`). 3D says typed CSS provider must land before old CSS proof retires (`restart/audit/totality/p3/3D-skinny-fold.md:61`). 3F fails closed on absent provider proof (`restart/audit/totality/p3/3F-migration-handoff.md:119`). | Retirement/deletion cannot be used to make evidence disappear before the replacement provider exists. |
| Stale/generated evidence | ACCEPT | 3A treats the 67 runtime files as a baseline, not success, and requires 67/67 line-1 provenance plus non-writing regen/check (`restart/audit/totality/p3/3A-architecture-synthesis.md:66`). 3B separates gate-only W4 from generator/check, projection, deletion, and transcript work (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:137`-`150`). | The invariant count is 67, and the packet does not claim generated closure from headers, stale census prose, or source presence. |
| Implementation acceptance without executable proof | ACCEPT | 3B says the new MASTER rows are proposed receiver rows, not implementation dispatch (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:117`-`120`). 3C states the lock matrix authorizes no implementation work and cannot launder wave overflow into challenge time (`restart/audit/totality/p3/3C-locks-crystallisation.md:61`; `restart/audit/totality/p3/3C-locks-crystallisation.md:129`). 3F blocks implementation waves until CRUD-4 current-state truth and G-Omega authorization complete (`restart/audit/totality/p3/3F-migration-handoff.md:84`-`98`; `restart/audit/totality/p3/3F-migration-handoff.md:125`-`133`). | The proposal text consistently requires later executable gates, transcripts, or explicit block/revert/amendment routes before acceptance. |

## Verdict

ACCEPT. V5 CH7 finds no remaining procedural-overfit or contrivance defect in
the target packet after the citation-only V5 repair. The packet preserves
proposal-only boundaries, exposes gate exclusions, rejects broadcast and wrong
host evidence, quarantines FNV, blocks delete-before-provider sequencing, keeps
CSS parity scoped to typed same-workload proof, and requires executable gates or
explicit block routes before any implementation acceptance.
