---
agent: CH4
pass: T-P1-excavation
cycle: V1
lens: COST
generated_at: 2026-05-21T00:00:00-04:00
disposition: REVISE
audited_artifacts:
  - restart/prompts/totality/PASS-1-EXCAVATION.md
  - restart/prompts/ORCHESTRATOR.md
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-past-corpora.md
---

## Lens Basis

`restart/prompts/totality/PASS-1-EXCAVATION.md:122` requires every divergence to carry realistic LOC-delta and risk class, and requires 1E amendment candidates to state a wave-alignment hint. `restart/prompts/ORCHESTRATOR.md:86` is stricter: CH4 also requires LOC budget, risk class, wave alignment, hard cap, and same-wave consumer per kernel/primitive. This V1 set is therefore not acceptable as final cost framing until hard caps and consumer routing are added.

## Findings

| disposition | finding | evidence | required V2 correction |
|---|---|---|---|
| ACCEPT | 1A divergences generally carry evidence-backed LOC/risk estimates. | The substrate table attaches LOC/risk to each divergent or partial row, e.g. `1A-SUB-004` at `restart/audit/totality/p1/1A-substrate-evidence.md:33`, `1A-SUB-007` at `restart/audit/totality/p1/1A-substrate-evidence.md:36`, and the divergence catalog at `restart/audit/totality/p1/1A-substrate-evidence.md:48-58`. The estimates are grounded in cited spec and implementation paths rather than free-floating numbers. | Preserve these estimates, but add hard caps and target wave/consumer fields where the row is a proposed closure route rather than pure census. |
| REVISE | 1A amendment framing lacks hard cap and same-wave consumer routing. | `1A-LOCK1-AMEND-001` has evidence and a wave hint at `restart/audit/totality/p1/1A-substrate-evidence.md:80-82`, but no maximum acceptable LOC cap and no named same-wave consumer proving the CSS fact stream will be routed through `TapeEmit` / `DirectBuild` or explicitly excluded. | Add `hard_cap` and `consumer_required` fields, e.g. "cap: <=N LOC docs/codegen/runtime delta" and "consumer: T-P3 3C must bind this to 1E LAC-1E-08 or reject substrate-closure wording." |
| ACCEPT | 1B gives realistic cost bands for concrete codegen divergences. | Divergence rows `P1-1B-D1` through `P1-1B-D9` at `restart/audit/totality/p1/1B-codegen-evidence.md:53-63` include LOC/risk and verify actions; high-risk rows such as marker lowerers and grammar-profile replacement cite live code paths at `restart/audit/totality/p1/1B-codegen-evidence.md:60-61`. | Keep the estimates. In V2, split broad high-risk rows into budgeted substeps if they remain intervention candidates. |
| REVISE | 1B amendment candidates have wave hints but no LOC/risk/hard-cap column. | The amendment table at `restart/audit/totality/p1/1B-codegen-evidence.md:83-88` gives target, evidence, and wave hint, but omits LOC/risk and hard cap even though CH4 requires cost framing for amendment candidates. | Add LOC/risk/hard-cap per candidate. The stale ARCH §7.4 doc edit can be capped as a small docs-only change; diagnostic vocabulary needs a separate cap and gate alignment because it may affect H/J reporting. |
| ACCEPT | 1C runtime census and divergence catalog are cost-aware and evidence-backed. | Runtime module rows include LOC counts and risk estimates at `restart/audit/totality/p1/1C-runtime-evidence.md:28-34`; divergence rows `1C-D1` through `1C-D6` carry evidence, LOC estimate, and risk at `restart/audit/totality/p1/1C-runtime-evidence.md:72-81`; verification evidence is supplied at `restart/audit/totality/p1/1C-runtime-evidence.md:100-102`. | Preserve, but V2 should add wave and hard-cap fields to each divergence row because several rows imply implementation work, not only observation. |
| REVISE | 1C lacks wave alignment and hard caps for all runtime divergences. | The divergence table header at `restart/audit/totality/p1/1C-runtime-evidence.md:74` has only `LOC estimate` and `Risk`; rows like the V1 nine-grammar runtime matrix at `restart/audit/totality/p1/1C-runtime-evidence.md:79` are large enough to require wave scoping and a stop condition. | Add wave alignment and hard cap columns. The nine-grammar row should not be a single open-ended 1,500-4,000 LOC bucket without a wave boundary and acceptance cap. |
| REVISE | 1D mixes evidence-backed route lessons with incomplete cost framing. | Some rows include cost/wave hints, e.g. `restart/audit/totality/p1/1D-skinny-lessons.md:68-74`; many finding rows only say "medium LOC/risk" or "high LOC/risk" without numeric LOC, hard cap, or same-wave consumer, e.g. `restart/audit/totality/p1/1D-skinny-lessons.md:45-64`. | Convert qualitative hints into numeric budget ranges plus hard caps for every divergence-like row. SIMD/ASM rows must name the production consumer in the same wave, not just "SIMD wave." |
| ACCEPT | 1E is the strongest amendment-cost artifact: every listed lock candidate carries supporting evidence, LOC/risk, and wave hint. | The 11 `LOCKS-AMENDMENTS-CANDIDATE` rows at `restart/audit/totality/p1/1E-locks-evidence.md:65-79` include evidence, LOC/risk, and wave alignment. Divergences at `restart/audit/totality/p1/1E-locks-evidence.md:49-63` also carry LOC/risk. | Keep this structure as the V2 template for other agents. |
| REVISE | 1E still fails the orchestrator hard-cap requirement and under-specifies consumer routing for primitive/lock changes. | The 1E tables have no hard-cap column at `restart/audit/totality/p1/1E-locks-evidence.md:30-47`, `restart/audit/totality/p1/1E-locks-evidence.md:51-63`, or `restart/audit/totality/p1/1E-locks-evidence.md:67-79`. Lock 16 traceability at `restart/audit/totality/p1/1E-locks-evidence.md:78` names H.W0 but does not require a same-wave production consumer/admission row. | Add `hard_cap` and, for L16/SIMD, `same_wave_consumer`. A primitive manifest without a row-moving or production-consuming wave should remain REVISE. |
| ACCEPT | 1F anti-pattern and coherence scans give plausible low/medium/high cost framing for live structural debt. | Anti-pattern rows include LOC/risk at `restart/audit/totality/p1/1F-anti-pattern.md:40-47`; coherence rows do the same at `restart/audit/totality/p1/1F-coherence-scan.md:47-54`; past-corpora pre-blocks correctly assign 0 LOC where the only cost is respecting a rejected route at `restart/audit/totality/p1/1F-past-corpora.md:41-48`. | Preserve the 0-LOC pre-block treatment; it is a useful cost distinction. |
| REVISE | 1F cost claims are not consistently evidence-backed where they derive from scans whose raw counts are not reproducible from the artifact. | `restart/audit/totality/p1/1F-anti-pattern.md:30-34` and `restart/audit/totality/p1/1F-coherence-scan.md:38-40` cite scan results, but the exact `wc`/child-count command output is summarized rather than captured. This is weaker than path:line evidence for cost claims that depend on file size and directory fanout. | Add command transcript snippets or generated audit rows for LOC and child-count evidence, or downgrade exact cost confidence to UNKNOWN with verify_action. |
| REVISE | Hard-cap framing is globally absent from V1. | The governing CH4 row requires a hard cap at `restart/prompts/ORCHESTRATOR.md:86`, but none of the audited artifact tables expose a `hard_cap` field: 1A divergence/gap rows at `restart/audit/totality/p1/1A-substrate-evidence.md:46-69`, 1B rows at `restart/audit/totality/p1/1B-codegen-evidence.md:51-74`, 1C rows at `restart/audit/totality/p1/1C-runtime-evidence.md:72-90`, 1D rows at `restart/audit/totality/p1/1D-skinny-lessons.md:66-83`, 1E rows at `restart/audit/totality/p1/1E-locks-evidence.md:49-79`, and 1F rows at `restart/audit/totality/p1/1F-coherence-scan.md:45-62`. | V2 must add hard caps to every divergence and amendment candidate. A recommended schema is `loc_budget`, `risk`, `wave`, `hard_cap`, `same_wave_consumer`, and `evidence_basis`. |

## Cycle Verdict

REVISE. V1 is not a paper close: most LOC/risk claims are plausible and tied to cited source or spec lines. It fails CH4 because hard caps are absent across the board, wave alignment is inconsistent outside 1E, and same-wave consumer routing is only partially expressed for SIMD/ASM and substrate-producing work.
