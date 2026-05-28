# CH1 Correctness - SK-V15 T-P2 V2

Lens: `CH1 CORRECTNESS` / provenance / executable evidence.

Disposition: `ACCEPT`.

## Critical Findings

| id | severity | finding | evidence | disposition |
|---|---|---|---|---|
| none | n/a | No CH1-critical correctness, provenance, or executable-evidence defect found in the V2 packet. | The V1 CH1 defect was the dead simdjson tape URL; 2D now cites `https://simdjson.org/api/0.8.0/md_doc_tape.html` and records the replacement check at `restart/audit/totality/p2/2D-cost-model.md:64` and `restart/audit/totality/p2/2D-cost-model.md:129`-`131`. | accepted |

## Evidence Inspected

- Governing CH1 contract: `restart/prompts/ORCHESTRATOR.md:81`-`88`, `restart/prompts/totality/PASS-2-RESEARCH.md:100`-`104`, and required CH file shape at `restart/audit/totality/p2/hardening/V2/CHALLENGE-CONTEXT.md:76`-`83`.
- V2-specific CH1 fold: `restart/audit/totality/p2/hardening/V2/CHALLENGE-CONTEXT.md:42`-`44`, `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:102`-`106`, and V1 source-repair obligation at `restart/audit/totality/p2/hardening/HARDENING-T-P2-V1-CONSOLIDATED.md:34`-`39`.
- Simdjson tape re-check: `curl -I -L https://simdjson.org/api/0.8.0/md_doc_tape.html` returned HTTP `200`; fetching the page showed the title "Tape structure in simdjson" and tape-materialization content. The retired `https://simdjson.github.io/simdjson/md_doc_tape.html` still returns HTTP `404`, but `rg` found it only in the V2 fold addendum and V1 consolidated history, not in the six live dossier citations.
- Live 2D support checks: the dossier's scaffold refutations match current source evidence for zero e-graph rewrite rules at `skinny/crates/passes/src/backend_egraph.rs:65`-`67`, CSP selected-index/grammar-name facts at `skinny/crates/passes/src/decision_csp.rs:53`-`83` and `:116`-`124`, marker lowerers at `skinny/crates/codegen/src/lower/eager_tape.rs:15`-`17`, `offset_tape.rs:15`-`17`, `event_tape.rs:15`-`17`, and `collapsed_stage.rs:15`-`17`, and the exact five-shape enum at `skinny/crates/ir/src/lib.rs:339`-`345`.
- URL and local-path audit: all checked public URLs returned HTTP `200` except the ACM DOI, which redirected to an ACM `403` under curl; Crossref resolved DOI `10.1145/151640.151642` to "Engineering a simple, efficient code-generator generator", ACM LOPLAS 1992. A local citation range audit found no missing root-local files and no short line ranges after classifying upstream GitHub file fragments and crate-local shorthand rows by their adjacent source register context.
- Executable host evidence spot-check: `sysctl` reports `Apple M5 Max` and the 2E-listed aarch64 feature keys for AES, PMULL, DotProd, I8MM, CSSC, SHA3, and SME; `hw.optional.arm.FEAT_SVE2` did not report, matching `restart/audit/totality/p2/2E-host-arch-esoterica.md:25`-`32`.
- All six target dossiers were inspected: `2A-sota-landscape.md`, `2B-primitive-vocabulary.md`, `2C-grammar-neutrality.md`, `2D-cost-model.md`, `2E-host-arch-esoterica.md`, and `2F-parse-that-gaps.md`.

## Fold Requirements

None. This CH1 pass has no `REVISE` or `REJECT` fold item.

## Convergence Impact

CH1 does not block T-P2 V2. This ACCEPT can contribute to a clean V2 hardening cycle, but V2 can only be the first clean cycle; `restart/audit/totality/p2/hardening/V2/CHALLENGE-CONTEXT.md:37`-`38` still requires a second consecutive clean challenge cycle before normal section 3Z convergence.
