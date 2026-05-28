# CH7 Overfit-Prune - T-P3 V4

Verdict: ACCEPT.

Target packet: `e6c1c2a84` (`docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`).

## Required Checks

| check | result |
|---|---|
| `git show --stat --oneline e6c1c2a84 -- restart/audit/totality/p3` | `e6c1c2a84 docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`; 7 files changed, 170 insertions, 123 deletions. |
| `git diff --check e6c1c2a84^ e6c1c2a84 -- restart/audit/totality/p3` | exit 0, no output. |
| Extracted `3C-locks-v+1-diff.md` diff to `/tmp/tp3-locks-v4.diff`; `git apply --check /tmp/tp3-locks-v4.diff` | exit 0, no output. |
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16`. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `67`. |
| Required stale-pattern `rg` scan from `CHALLENGE-CONTEXT.md` | no matches, exit 1. |

The required invariant outputs match the V4 challenge context: 16 numbered
locks, 67 Pattern H runtime files, and no stale-pattern matches. The working
tree has unrelated dirty runtime/research/xtask edits and existing V4 CH2-CH6
reports; no mismatch was waived on that basis. The seven target artifacts have
no post-target drift in `HEAD` (`git diff --name-status e6c1c2a84..HEAD --`
over the seven files returned no output).

## CH7 Scan

| hazard | verdict | target packet evidence | controlling evidence |
|---|---|---|---|
| Wave-graph cycles | ACCEPT | 3B splits W4 Pattern H into provenance, generator/check, runtime projection, destructive deletion, and close-transcript sub-rows, with deletion blocked unless replacement proof has landed or lands in the same wave (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:137`-`150`). 3F blocks delete, retirement, provider/template removal, old CSS proof retirement, and runtime-shim deletion before rebuild proof (`restart/audit/totality/p3/3F-migration-handoff.md:42`). | SK-V15 forbids W12/challenge-time overflow and requires intrinsic block, REDRESS/revert, or G-Omega wave-graph amendment at cap (`restart/skinny/tranches/sk-v15/SPEC.md:165`-`170`). Dependency rows require provider proof no later than the delete/retire wave (`restart/skinny/tranches/sk-v15/SPEC.md:189`-`204`). |
| Broadcast admission | ACCEPT | 3A demotes CSS 24/24 to a diagnostic aggregate until W5 typed CSS provider and W6 same-workload retime (`restart/audit/totality/p3/3A-architecture-synthesis.md:64`). The proposed Lock 8 text requires `measurement_row_id` and `broadcast_group_id` and rejects repeated tuples without independent row evidence (`restart/audit/totality/p3/3C-locks-v+1-diff.md:52`). 3F maps W1 to CSS broadcast demotion with no live admit from a shared tuple (`restart/audit/totality/p3/3F-migration-handoff.md:65`-`66`). | PASS-IMPL records the 24 CSS rows as one broadcast measurement (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21`-`31`). SK-V15 close requires no CSS 24-row broadcast admit and gate-consumed anti-broadcast fields (`restart/skinny/tranches/sk-v15/SPEC.md:54`-`55`, `restart/skinny/tranches/sk-v15/SPEC.md:106`-`122`). |
| Gate exclusions and self-exempting grep gates | ACCEPT | 3A requires included roots, excluded roots, owner, reason, self-scan status, primitive status, gate consumer, affected rows, and disposition, and fails self-exempting gates (`restart/audit/totality/p3/3A-architecture-synthesis.md:67`). 3E states omitted `runtime_generator.rs`, `grammar_provider.rs`, direct JSON generators, or templates are non-evidence and same-change exclusions fail (`restart/audit/totality/p3/3E-grammar-generalisation.md:77`, `restart/audit/totality/p3/3E-grammar-generalisation.md:120`, `restart/audit/totality/p3/3E-grammar-generalisation.md:149`, `restart/audit/totality/p3/3E-grammar-generalisation.md:162`). | PASS-IMPL identifies Lock 14 allowlist holes for `runtime_generator.rs`, `grammar_provider.rs`, JSON direct/typed files, and templates (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:45`-`47`). SK-V15 requires omitted-root reporting and rejects self-exempting exclusions (`restart/skinny/tranches/sk-v15/SPEC.md:64`-`65`, `restart/skinny/tranches/sk-v15/SPEC.md:119`-`122`, `restart/skinny/tranches/sk-v15/SPEC.md:233`-`235`). |
| CSS fake parity | ACCEPT | 3E requires generated typed CSS value/document/view/visitor output, row-local equality, and same-workload `cssparser` retime; fact streams and brace counters remain diagnostic (`restart/audit/totality/p3/3E-grammar-generalisation.md:69`). 3E also states W5/W6 do not carry broad CSSOM and route larger CSSOM/value parity to intrinsic block or G-Omega amendment (`restart/audit/totality/p3/3E-grammar-generalisation.md:127`, `restart/audit/totality/p3/3E-grammar-generalisation.md:141`, `restart/audit/totality/p3/3E-grammar-generalisation.md:159`). | 2A refutes four-counter/fact-stream CSS close and current lightningcss admission (`restart/audit/totality/p2/2A-sota-landscape.md:56`-`62`). 2C says typed CSS close is blocked until generator-derived provider proof and same-workload measurement, not full CSSOM hidden in W5/W6 (`restart/audit/totality/p2/2C-grammar-neutrality.md:64`, `restart/audit/totality/p2/2C-grammar-neutrality.md:67`-`69`, `restart/audit/totality/p2/2C-grammar-neutrality.md:145`). |
| Wrong-host close evidence | ACCEPT | 3A requires Apple M5 Max/aarch64 primitive gates and rejects source-presence-only or wrong-host close evidence (`restart/audit/totality/p3/3A-architecture-synthesis.md:71`, `restart/audit/totality/p3/3A-architecture-synthesis.md:108`). 3B keeps CollapsedStage diagnostic unless aarch64 proof exists (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:133`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:163`). 3E states x86 diagnostics do not admit CollapsedStage for current SK-V15 grammars (`restart/audit/totality/p3/3E-grammar-generalisation.md:99`). | 2D treats AVX-512 CollapsedStage evidence as x86 diagnostic only and requires Apple M5 Max/aarch64 scalar/parity/hardware/consumer proof (`restart/audit/totality/p2/2D-cost-model.md:65`, `restart/audit/totality/p2/2D-cost-model.md:118`). 2E makes aarch64 the close route and x86 diagnostic only (`restart/audit/totality/p2/2E-host-arch-esoterica.md:25`-`32`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:86`-`96`). |
| FNV bench leakage | ACCEPT | 3A quarantines W11L/W11N/W11O FNV products and generated CSS `input_fnv64` as bench/telemetry only, not runtime selectors, production arbiters, CSS Value API proof, retained identity, or semantic correctness proof (`restart/audit/totality/p3/3A-architecture-synthesis.md:73`). 3B adds W10 FNV quarantine and blocks production FNV correctness migration (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:134`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:165`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:181`). 3F keeps FNV bench-only with no production arbiter (`restart/audit/totality/p3/3F-migration-handoff.md:72`). | PASS-IMPL flags FNV closed-enum products as bench-only and not production runtime (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:60`-`65`). SK-V15 forbids production FNV arbiter/correctness proof and requires W10 quarantine plus production scans (`restart/skinny/tranches/sk-v15/SPEC.md:74`-`75`, `restart/skinny/tranches/sk-v15/SPEC.md:147`-`153`, `restart/skinny/tranches/sk-v15/SPEC.md:430`-`445`). |
| Delete-before-provider sequencing | ACCEPT | 3C's proposed Lock 6/14 clause requires same-wave replacement provider before deletion or retirement and rejects fake generated headers or provider/template deletion before W5/W6 replacement proof (`restart/audit/totality/p3/3C-locks-v+1-diff.md:50`). 3B's W4 destructive-deletion sub-row requires replacement proof before deletion (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:149`). 3D says typed CSS provider must land before old CSS proof retires (`restart/audit/totality/p3/3D-skinny-fold.md:61`). 3F's CH4 matrix fails closed on absent provider proof (`restart/audit/totality/p3/3F-migration-handoff.md:119`). | SK-V15 dependency rows bind `CSS_GENERATED_RS`, summary/fact-stream proof, CSS provider/template deletion, Pattern H provenance, and legacy CSS runtime-shim retirement to replacement/provider proof (`restart/skinny/tranches/sk-v15/SPEC.md:192`-`199`). P1 records REDRESS 183/184/209-213 as delete-before-provider precedent (`restart/audit/totality/p1/1D-skinny-lessons.md:158`-`159`). |

## Findings

None.

The target packet does not admit broadcast CSS evidence, gate-excluded generic
cleanliness, fake CSS parity, wrong-host primitive close, production FNV
correctness, delete-before-provider sequencing, or self-exempting grep gates.
Each CH7 risk is routed to a consumed gate, REDRESS/revert, intrinsic block, or
G-Omega wave-graph amendment.
