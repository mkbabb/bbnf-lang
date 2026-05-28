# CH1 CORRECTNESS

Disposition: REVISE

## Critical Findings

| id | severity | finding | evidence |
|---|---:|---|---|
| CH1-V1-01 | high | 2D carries one unresolved primary-source URL. The staged-tape row cites "simdjson tape documentation" at `https://simdjson.github.io/simdjson/md_doc_tape.html`, but that URL returns HTTP 404. This is a bounded provenance defect because the same row also cites the simdjson paper; it does not prove the staged-tape assertion false, but the cited source does not exist at the referenced location. | Citation appears in `restart/audit/totality/p2/2D-cost-model.md:61` and the source index repeats it at `restart/audit/totality/p2/2D-cost-model.md:113`. Verification: `curl -L --max-time 10 -o /dev/null -s -w '%{http_code}' https://simdjson.github.io/simdjson/md_doc_tape.html` returned `404`. |

## Evidence Inspected

- Authority and lens contract: `restart/audit/totality/p2/hardening/V1/CHALLENGE-CONTEXT.md:47`-`55`, `restart/prompts/totality/PASS-2-RESEARCH.md:100`-`104`, `restart/prompts/ORCHESTRATOR.md:81`-`88`, `restart/prompts/ORCHESTRATOR.md:118`-`126`, `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md:78`-`103`.
- All six dossiers were read: `restart/audit/totality/p2/2A-sota-landscape.md`, `restart/audit/totality/p2/2B-primitive-vocabulary.md`, `restart/audit/totality/p2/2C-grammar-neutrality.md`, `restart/audit/totality/p2/2D-cost-model.md`, `restart/audit/totality/p2/2E-host-arch-esoterica.md`, `restart/audit/totality/p2/2F-parse-that-gaps.md`.
- Local citation resolution: extracted local backtick path citations from the six dossiers and checked them against `HEAD`; full `path:line` citations were in range. One apparent `crates/codegen/src/lib.rs` string is a cited scan-root literal inside `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2370`-`2379`, not a repository-root source citation.
- High-load local claims spot-checked at `HEAD`: CSS broadcast rows and platform/plane data in `skinny/RESULTS.md:112`-`135`; PASS-IMPL CSS broadcast / brace-counter findings in `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21`-`31`; `CSS_GENERATED_RS` and `CssFullParseSummary` in `skinny/crates/codegen/src/runtime_generator.rs:713`-`813`; aarch64 scalar delegates in `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1`-`4`, `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:1`-`4`, `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1`-`4`, `skinny/crates/bbnf-simd/src/aarch64/eob_pad_clamp.rs:1`-`6`; Decision Engine zero-rule and marker-lowerer claims in `skinny/crates/passes/src/backend_egraph.rs:65`-`67`, `skinny/crates/passes/src/decision_csp.rs:53`-`83`, and `skinny/crates/codegen/src/lower/eager_tape.rs:15`-`17`.
- External-source sweep: extracted unique HTTP(S) citations from the dossiers and checked with `curl -L -I` / `curl -L` where HEAD was inconclusive. The simdjson tape URL above was the only confirmed 404 relevant to an active source citation. The RFC 8259 URL resolves by GET, and the Fraser/Hanson/Proebsting DOI was corroborated by indexed bibliographic records despite DOI-site curl 403.
- Host probe for 2E was plausible on this machine: `sysctl` reports `machdep.cpu.brand_string: Apple M5 Max`, `FEAT_PMULL=1`, `FEAT_DotProd=1`, `FEAT_I8MM=1`, `FEAT_CSSC=1`, `FEAT_SHA3=1`, `FEAT_SME=1`, and no `hw.optional.arm.FEAT_SVE2` oid, matching `restart/audit/totality/p2/2E-host-arch-esoterica.md:25`-`29`.

## Fold Requirements

- In `2D`, replace `https://simdjson.github.io/simdjson/md_doc_tape.html` with a resolvable primary source for simdjson tape materialization, or remove that URL and explicitly ground the staged-tape assertion only in the existing simdjson paper/source citations.
- After replacement/removal, rerun a URL existence check for the amended 2D source list and preserve the command or result in the V2 dossier or fold note.

## Convergence Impact

This REVISE blocks T-P2 V1 convergence until folded. It is not a REJECT: I found no confabulated paper, no falsified refutation row, and no benchmark claim whose cited RESULTS lines lacked corpus/platform/plane metadata in the sampled high-load rows.
