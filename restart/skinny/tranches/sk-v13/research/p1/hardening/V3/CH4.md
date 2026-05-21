# SK-V13 S-P1 V3 CH4: Cost/Reproducibility

Disposition: REVISE.

V3 is much closer than V2 and is reproducible enough to audit the current
profile packet, but it is not yet fully reproducible by a later third party from
checked-in materials alone. The direct, mode-III, CSS, PMU, and sidecar
extraction surfaces now have explicit command paths, run identities, hashes, and
status checks. The remaining CH4 blockers are narrower: retained V1 parse/typed
build provenance is explicitly limited, and temporary mode-III/CSS harness
sources are described by path/hash rather than preserved as rebuildable source.
The branch/L1/LLC counter gap is correctly routed as unavailable from the
current xctrace export and is not a blocker by itself.

## Evidence Reviewed

- `PASS-1-PROFILE.md:143`-`146` requires verbatim rerunnable commands and says
  profiles missing run id, host triple, or build flags fail CH4. The samply
  discipline at `PASS-1-PROFILE.md:251`-`254` still prefers interactive
  `samply record` over `--save-only`.
- V2 consolidation required V3 to make reproducibility durable by recording V1
  retained-capture provenance, preserving or inlining the mode-III harness and
  sidecar extractor, adding CSS command surface, and preserving branch/L1/LLC as
  unavailable unless numeric exports appear
  (`HARDENING-S-P1-V2-CONSOLIDATED.md:61`-`73`).
- P1-A retains V1 parse as authority and declares the save-only/offline
  symbolication limitation (`p1a-samply-mode-1.md:13`-`19`,
  `:80`, `:136`-`:137`).
- P1-B gives the V2 direct identity, build command, capture command, and status
  checks with 34/34 profiles and sidecars (`p1b-samply-mode-2.md:21`-`:59`).
- P1-C gives the V2 mode-III identity, build command, capture command, 85/85
  status, and explicit unsupported routes (`p1c-samply-mode-3.md:20`-`:59`).
- P1-D records V1/V2 PMU authorities and explicitly marks branch/L1/LLC counters
  `unavailable_from_current_export`, not zero (`p1d-pmu-cycles.md:19`-`:55`,
  `:121`-`:130`).
- P1-E now points to a checked-in hot-leaf extractor and records the remaining
  sidecar/offline symbolication boundary (`p1e-hot-leaf-attribution.md:30`-`:51`).
- P1-F keeps incomplete surfaces labelled: typed 7/17, CSS method-mismatch,
  same-run sonic parse PMU absent, and every profile classification as
  `profile_signal_not_gate_admission` (`p1f-results-delta.md:46`-`:55`,
  `:113`-`:125`).
- `profile-provenance-v3.md` records toolchain, V1 run identity, V1 binary
  hashes, V2 direct build/run commands, CSS build/run/samply commands, and the
  checked-in extractor command (`profile-provenance-v3.md:8`-`:151`).
- `mode3-harness-provenance.md` records mode-III harness hashes, build/capture
  commands, Cargo manifest contents, probe inventory, fixture mapping, and
  counter output shape (`mode3-harness-provenance.md:10`-`:80`).
- `extract_hotleaf_top20.py` is checked in and implements sidecar-backed
  top-20 extraction for V2 direct, mode-III, and CSS profiles
  (`extract_hotleaf_top20.py:17`-`:152`).
- Local artifact check found the referenced `/tmp` evidence present now: 34 V1
  parse profiles and sidecars, 14 V1 typed profiles and sidecars, 34 V2 direct
  profiles and sidecars, 85 V2 mode-III profiles and sidecars, one CSS profile
  and sidecar, direct status `34 0`, mode-III status `85 0`, and PMU rows
  `130 0`.

## Findings

| Id | Disposition | Evidence | CH4 impact | Required action |
|---|---|---|---|---|
| CH4-001 | ACCEPT | V2 direct now has run identity, host/build flags, exact cargo build, exact `samply record` command, status checks, profile paths, and sidecar paths (`p1b-samply-mode-2.md:21`-`:59`). Local status remains 34 rows, 0 bad rc, 34 profiles, and 34 sidecars. | The V1 direct panic-path reproducibility blocker is closed. | Preserve the V2 direct identity, status TSV, logs, profiles, sidecars, and top-20 TSV as the direct authority. |
| CH4-002 | REVISE | V3 records V1 retained parse/typed run identity, toolchain, capture command shapes, and binary hashes, but explicitly says the exact original cargo build invocation was not preserved and that V1 parse/typed are auditable artefacts, not a fully rebuildable command surface (`profile-provenance-v3.md:17`-`:52`). | The limitation is honestly declared, so this is not a REJECT, but retained V1 parse/typed evidence still cannot be rebuilt by a third party from the method block alone. | Either rerun parse/typed under the V2 build identity or preserve enough V1 build material to rebuild the hashed binaries: exact cargo command, target dir, RUSTFLAGS, profile settings, feature mask, lockfile state, and source checkout. |
| CH4-003 | REVISE | Mode III has exact V2 build/capture commands and 85/85 profile rows (`p1c-samply-mode-3.md:35`-`:59`). V3 adds hashes and a source summary, but the harness still lives at `/tmp/skv13-mode3-profiler` and `src/main.rs` is identified only by line count/hash plus summary, not by checked-in source (`mode3-harness-provenance.md:10`-`:21`, `:70`-`:80`). | Current reviewers can audit and rerun while `/tmp` survives, but a later third party cannot reconstruct the temporary harness from checked-in text alone. | Inline or check in the complete mode-III harness source, including `src/main.rs` and lockfile, or replace the temp harness with a durable repo helper. |
| CH4-004 | REVISE | CSS now has build, throughput/equality, and samply commands plus source/binary hashes (`profile-provenance-v3.md:97`-`:133`), and P1-E/P1-F correctly keep the CSS profile as timer/fact-sink dominated and method-mismatched (`p1e-hot-leaf-attribution.md:79`-`:83`; `p1f-results-delta.md:79`-`:100`). The profiler source remains a `/tmp/skv13-css-profiler` path described by hashes, not checked-in source. | CSS is inspectable and better-scoped than V2, but still not durable if the temp profiler source disappears. | Preserve the CSS profiler source or inline its source/hash appendix, then add a no-mutation verification command for the CSS log/profile/top-leaf outputs. |
| CH4-005 | ACCEPT WITH LIMITATION | V3 checks in `extract_hotleaf_top20.py`, and P1-E documents the checked-in command for regenerating the sidecar-backed top-20 TSV (`p1e-hot-leaf-attribution.md:40`-`:51`; `extract_hotleaf_top20.py:99`-`:152`). The script covers direct, mode-III, and CSS profile patterns for `hotleaf_top20.tsv`; it does not regenerate the separate `direct_summary.tsv` or `mode3_summary.tsv` files named as sources elsewhere. | The hot-leaf sidecar pipeline is reproducible enough for CH4 as an offline extraction package, while CH6 still owns the symbol-quality objection to save-only samply and function-only rows. The two summary TSVs remain `/tmp` artefacts unless another documented command regenerates them. | Preserve the checked-in extractor and sidecar input contract. Add a no-mutation verification command for `hotleaf_top20.tsv`, and document any separate summary-TSV regeneration path. Do not promote save-only/function-only rows as interactive samply-equivalent symbol closure. |
| CH4-006 | ACCEPT WITH ROUTED GAP | P1-D and the V3 ledger say branch misses, L1 misses, and LLC misses are `unavailable_from_current_export`; cycles, instructions, c/B, and CPI are available (`p1d-pmu-cycles.md:51`-`:55`; `support/evidence-ledger-v3.md:106`-`:113`). | Missing cache counters are correctly represented as unavailable, not zero or inferred. That is acceptable for CH4 if the consolidated fold preserves the label. | Keep branch/L1/LLC as `unavailable_from_current_export` unless a later tool/export path supplies row-level numeric counters. |
| CH4-007 | ACCEPT | P1-F preserves typed 7/17, stale/missing comparator state, CSS method mismatch, and `profile_signal_not_gate_admission` labels (`p1f-results-delta.md:46`-`:55`, `:113`-`:125`). | The incomplete surfaces are not being paper-promoted as gate-admissible or complete profile coverage. | Carry these labels into any V3 consolidated fold and S-P2 handoff. |

## Required Fold Set

1. Decide whether retained V1 parse/typed remains acceptable as auditable-only
   evidence. If it remains, the consolidated fold must state that those rows are
   not fully rebuildable from V3 method blocks.
2. Make the temporary mode-III harness durable by checking in or inlining the
   complete harness source, not only its hashes and source summary.
3. Make the CSS profiler durable in the same way, or clearly route CSS as
   inspectable-but-temp-sourced telemetry.
4. Preserve the checked-in sidecar extractor and its input/output contract;
   document separate regeneration for `direct_summary.tsv` and
   `mode3_summary.tsv` if those remain citable sources.
5. Preserve branch/L1/LLC as `unavailable_from_current_export`; do not interpret
   unavailable counters as zeros.

V3 is fit to audit and close several V2 CH4 gaps, but because the retained V1
build limitation and temporary harness source durability remain material,
CH4 should stay REVISE rather than ACCEPT.
