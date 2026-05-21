# SK-V13 S-P1 V3 Hardening Consolidated

Pass: S-P1 Profile. Cycle: V3.
Date: 2026-05-21.
Scope: consolidated CH1-CH6 challenge verdict for the SK-V13 S-P1 V3 profile cohort.
Output: this file.

## Verdict

`G-S-P1-V3-CONVERGED`: FAIL.

V3 folds the V2 status, REDRESS, sidecar, and anti-paper-close gaps. Five
lenses accept. CH4 still requires one more reproducibility fold because the
mode-III and CSS helper sources remain temp-path sourced, and retained V1
parse/typed evidence is declared auditable but not fully rebuildable.

| Lens | Disposition | Load-bearing reason |
|---|---|---|
| CH1 correctness | ACCEPT | V3 uses explicit resolved/unresolved statuses for line-poor, missing, CSS nonparser, and unavailable-counter cases instead of pretending they are measured closures. |
| CH2 generality / Lock 14 | ACCEPT | JSON envelopes, JSON typed leaves, CSS timer/fact-sink overhead, and JSON-confirmed candidates are quarantined as non-admissions. |
| CH3 regression / REDRESS | ACCEPT | REDRESS 119/120, pre-pin route families, REDRESS 96/97/98, and REDRESS-126 guards are inline and canonicalized in the V3 ledger. |
| CH4 cost / reproducibility | REVISE | V3 is auditable, but retained V1 parse/typed is not fully rebuildable, and the mode-III/CSS harness sources are not durable enough outside `/tmp`. |
| CH5 hidden coupling | ACCEPT | Track 1/Track 2, direct/typed/CSS, structural-scan, sidecar metadata, and union-substrate boundaries remain clean. |
| CH6 anti-paper-close | ACCEPT | Save-only, function-only, CSS timer/fact-sink, and typed 7/17 gaps are explicitly non-closing and bounded as profile signals. |

Acceptance rate: 5/6 = 83.3%. Consecutive accepted cycles: 0.

## V3 Improvements Banked

- `support/evidence-ledger-v3.md` is the canonical row/primitive status ledger.
- `support/extract_hotleaf_top20.py` is a checked-in reproducer for the
  sidecar-backed top-20 TSV.
- `support/profile-provenance-v3.md` records toolchain, binary hashes, build
  and run commands, CSS command surface, and unavailable counter status.
- `support/mode3-harness-provenance.md` records the mode-III harness identity,
  hash, command surface, probe inventory, and fixture mapping.
- P1-A through P1-F now cite the V3 ledger/provenance and carry REDRESS
  guardrails inline.

## Required V4 Fold

1. Make the mode-III helper durable by checking in the complete source snapshot
   needed to rebuild it, including `Cargo.toml`, `Cargo.lock`, and `src/main.rs`
   or a single checked support appendix containing those exact contents.
2. Make the CSS profiler durable in the same way: preserve or inline
   `Cargo.toml`, `Cargo.lock`, and `src/main.rs`, plus a no-mutation
   verification command for the CSS equality/throughput/profile artefacts.
3. Preserve the retained V1 parse/typed limitation honestly. Either rerun
   parse/typed under the V2 build identity or state in the consolidated V4
   result that those rows are accepted as auditable-only historical capture
   because the exact V1 build command was not preserved and no behavior-source
   delta affects the profile interpretation.
4. Document regeneration for `direct_summary.tsv` and `mode3_summary.tsv` or
   stop citing them as reproducible support artefacts; `hotleaf_top20.tsv` is
   already covered by the checked-in extractor.

## Cycle Disposition

S-P1 V3 returns to profile fold. The V4 fold should be a narrow CH4
reproducibility patch. No S-P2 dispatch is authorized from V3.
