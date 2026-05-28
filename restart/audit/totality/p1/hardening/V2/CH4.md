# SK-V15 T-P1 V2 CH4 Cost Hardening

Verdict: REVISE

Scope: CH4 cost/risk/receiver adequacy over the SK-V15 T-P1 V2 packet at
inventory fold commit `2fcbc1dc8`.

Summary: V2 materially improves the stale SK-V14 surface by adding current
SK-V15 LOC/risk fields and receiver hints across most inventories. CH4 cannot
accept the cycle yet because the 1E cost carrier is not reliably keyed to the
divergence and amendment rows it claims to budget, and several broad
implementation buckets still lack cap-valid, same-wave receiver detail.
PRUNE-before-REBUILD ordering is present and mostly adequate.

| id | disposition | finding | evidence | required revision |
|---|---|---|---|---|
| CH4-V2-001 | REVISE | 1E's V2 cost/wave carrier is ID-mismatched. The carrier rows use `D-1E-*` IDs, but many evidence notes match LAC amendment topics instead of the same-ID divergence rows, so actual divergences do not carry trustworthy LOC/risk/wave/hard-cap data. | `1E-locks-evidence.md:106` says `D-1E-V1-01` is CSS broadcast admission, while `:145` budgets the same ID as CSS fact-stream schema/API routing. `:112` says `D-1E-V1-07` is Lock 14 exclusions, while `:151` budgets the same ID as broadcast-admission detection. | Re-key the carrier or inline `loc_delta_estimate`, `risk`, `wave_hint`, and `hard_cap` in the divergence table. Every carrier row must describe the same row ID it budgets. |
| CH4-V2-002 | REVISE | 1E amendment candidates still do not directly state wave alignment per candidate. The candidate table has no wave column, and the V2 carrier covers only `LAC-1E-V2-15` under a LAC key; the other candidate hints appear to be present only through the mis-keyed `D-1E-*` rows. | Candidate table header is `candidate/type/target/proposed/supporting evidence` at `1E-locks-evidence.md:123`; candidate rows run `:125`-`:139`. The carrier header is at `:143`, but rows `:145`-`:158` are keyed as `D-1E-*`, not `LAC-1E-V1-01` through `LAC-1E-V1-14`. | Add a keyed wave/cost carrier for every `LAC-*` row, or add `wave_hint`, `loc_delta_estimate`, `risk`, and `hard_cap` columns directly to the LAC table. |
| CH4-V2-003 | REVISE | Hard-cap practicality is not proven for the largest implementation buckets. Open-ended or aggregate figures are cost signals, not cap-valid implementation routes. | `1C-runtime-evidence.md:121` uses `10,000+` for Pattern H. `1D-skinny-lessons.md:160` groups ten unimplemented claims into `1,500-8,000 implementation LOC plus gate work`; `:162` uses `400-3,000 implementation LOC per receiver`. ORCHESTRATOR CH4 requires realistic LOC budget, wave alignment, and hard cap at `restart/prompts/ORCHESTRATOR.md:86`. | Split these buckets into owner-path receivers or sub-waves with bounded upper caps, exit gates, and route/revert handling. Avoid `+` or "per receiver" without enumerating receivers. |
| CH4-V2-004 | REVISE | Same-wave receiver gates are named as a principle but not carried per primitive/kernel. The packet says primitives need same-wave consumers, but does not provide a primitive-to-consumer-to-proof table. | `1D-skinny-lessons.md:112` says candidate primitives require scalar oracle, checkasm, same-wave consumer, and manifest. `:184` says each source-present primitive must close as wired/deleted/scalar-delegate/blocked. `1E-locks-evidence.md:166` and `1F-coherence-scan.md:97` require manifests/classification, but neither names each primitive's same-wave consumer. ORCHESTRATOR CH4 requires same-wave consumer per kernel/primitive at `restart/prompts/ORCHESTRATOR.md:86`. | Add a receiver table for each source-present primitive/kernel: primitive, wave, consumer path/row, proof command, disposition if consumer is absent. |
| CH4-V2-005 | ACCEPT | PRUNE-before-REBUILD ordering is present and correctly distinguishes diagnostic demotion from provider deletion. | `1D-skinny-lessons.md:152`-`:153` records CSS deletion/retirement cannot outrun provider proof and cites REDRESS wave-cycle precedent. `1E-locks-evidence.md:139` adds delete/rebuild dependency proof. `1F-coherence-scan.md:130`-`:135` carries wave-cycle, CSS broadcast, gate-hole, Pattern H, and Decision Engine pre-blocks. | Preserve this ordering while fixing the budget/keying issues above. |

Required outcome: run another V2 fold or V3 inventory correction for the affected
cost carriers before CH4 can return ACCEPT.
