# T-P2 V3 CH1 - Correctness

Lens: `CH1 CORRECTNESS` / provenance / executable evidence.

Disposition: `ACCEPT`.

## Critical Findings

| id | severity | finding | evidence | convergence impact |
|---|---|---|---|---|
| CH1-V3-01 | none | The live 2D simdjson tape source remains repaired and supports the staged-tape claim. | `2D-cost-model.md:64` cites `https://simdjson.org/api/0.8.0/md_doc_tape.html`; `curl -L --max-time 20` returned HTTP `200`, and the fetched page contains "Tape structure in simdjson" plus tape-array materialization text. `2D-cost-model.md:129`-`131` records the 404-to-200 repair. | No block. |
| CH1-V3-02 | none | The retired `https://simdjson.github.io/simdjson/md_doc_tape.html` URL is not a live dossier citation. | Targeted `rg` over `2A`-`2F` found zero matches for the retired URL. The only live 2D tape-doc matches are the repaired simdjson.org source at `2D-cost-model.md:64`, `:126`, and `:131`. The historical addendum/V1 mentions are excluded by `V3/CHALLENGE-CONTEXT.md:34`-`37`. | No block. |
| CH1-V3-03 | none | The target packet under CH1 is unchanged from the V3 challenge target. | `git diff --name-status d11a9eec0 -- restart/audit/totality/p2/2A-sota-landscape.md ... 2F-parse-that-gaps.md` returned no target-packet differences. Working-tree diff over the same six dossiers also returned no differences. | No block. |
| CH1-V3-04 | none | Spot-checked provenance and executable evidence still match the packet's high-load claims. | simdjson raw `parse_many.md:54`-`57` at commit `79bbba3...` returned HTTP `200` and states stage 1 structural/UTF-8 discovery followed by stage 2 tape construction; raw `basics.md:344`-`350` returned HTTP `200` and supports the On-Demand lazy iterator claim. Local 2D scaffold checks still show zero e-graph rewrites, marker lowerers, and exactly five `BackendShape` variants. `sysctl` reports Apple M5 Max with PMULL/DotProd/I8MM/CSSC/SHA3/SME and no SVE2 key, matching 2E's host-gate posture. | No block. |

## Evidence Inspected

- Required authority and challenge context: `restart/audit/totality/p2/hardening/V3/CHALLENGE-CONTEXT.md`, `restart/audit/totality/p2/hardening/HARDENING-T-P2-V2-CONSOLIDATED.md`, `restart/prompts/totality/PASS-2-RESEARCH.md`, `restart/prompts/ORCHESTRATOR.md`, `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md`, and `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`.
- All six target dossiers: `2A-sota-landscape.md`, `2B-primitive-vocabulary.md`, `2C-grammar-neutrality.md`, `2D-cost-model.md`, `2E-host-arch-esoterica.md`, and `2F-parse-that-gaps.md`.
- URL/provenance checks: repaired simdjson tape URL returned HTTP `200`; retired GitHub Pages tape URL returned HTTP `404`; targeted grep confirmed the retired URL is absent from the six live dossiers.
- Local executable/source checks: `skinny/crates/passes/src/backend_egraph.rs:65`-`67`, `skinny/crates/passes/src/decision_csp.rs:53`-`83` and `:116`-`124`, lowerer marker strings at `skinny/crates/codegen/src/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs:15`-`17`, `skinny/crates/ir/src/lib.rs:339`-`345`, and `skinny/crates/ir/src/cost.rs:333`-`341`.
- Host transcript spot-check: `sysctl` reported `Apple M5 Max`; feature keys for AES, PMULL, DotProd, I8MM, CSSC, SHA3, and SME returned `1`; `hw.optional.arm.FEAT_SVE2` returned unknown.

## Fold Requirements

None.

## Convergence Impact

CH1 does not block T-P2 V3 convergence. If CH2-CH7 also return `ACCEPT`, this
CH1 result contributes to the second consecutive clean T-P2 hardening cycle
required by `ORCHESTRATOR.md` section 3Z and `V3/CHALLENGE-CONTEXT.md:27`-`31`.
