# SK-V13 S-P1 V2 CH4: Cost/Reproducibility

Disposition: REVISE.

CH4 asks whether a third party can reproduce the profile from the method blocks:
verbatim commands, run identity, host triple, build flags, and the artifact
paths required to audit the result. V2 materially improves V1: direct samply is
non-panic, mode III has 85/85 captured probe rows, PMU c/B rows exist, and the
branch/L1/LLC gap is now explicitly routed as unavailable from the current
xctrace export. It is still not an ACCEPT because several citable surfaces rely
on ephemeral or incomplete reproduction material: retained V1 parse/typed build
provenance, CSS capture commands, the temporary mode-III harness, and the
saved-profile sidecar extractor.

## Evidence Reviewed

- Prompt rule: `PASS-1-PROFILE.md:143`-`146` requires every method block to
  carry rerunnable commands and fails profiles missing run id, host triple, or
  build flags. The samply discipline at `PASS-1-PROFILE.md:251`-`254` also says
  `--save-only` loses symbol resolution and every P1-A/B/C block must name the
  exact `samply record` command.
- P1-A retains the V1 parse capture and identifies commit, host triple, build
  flags, run script, profile/sidecar paths, and a common run id
  (`p1a-samply-mode-1.md:7`-`10`, `:25`-`:31`, `:33`-`:49`,
  `:75`-`:79`, `:140`-`:149`).
- P1-B adds a V2 direct build and capture command, V2 identity, 34/34 status
  checks, and 34 sidecars (`p1b-samply-mode-2.md:7`-`:11`, `:17`-`:23`,
  `:25`-`:40`, `:42`-`:58`, `:98`-`:100`).
- P1-C adds the V2 mode-III build and capture command, V2 identity, 85/85
  status checks, and explicit unsupported routes
  (`p1c-samply-mode-3.md:7`-`:11`, `:17`-`:23`, `:25`-`:39`,
  `:41`-`:53`).
- P1-D carries PMU and xctrace export commands and correctly marks branch,
  L1, and LLC counters as `unavailable_from_current_export`, not zero
  (`p1d-pmu-cycles.md:7`-`:11`, `:17`-`:22`, `:34`-`:51`,
  `:57`-`:64`, `:117`-`:126`).
- P1-E records the saved-profile sidecar extractor path and sources, but the
  extractor itself lives under `/tmp` and the document says the script body was
  executed in the orchestrator shell (`p1e-hot-leaf-attribution.md:7`-`:11`,
  `:25`-`:39`, `:101`-`:115`).
- P1-F exposes the remaining profile-state gaps: typed coverage is 7/17, CSS is
  measured but method-mismatched, parse same-run sonic PMU is absent, and
  comparator sidecars remain absent or `n/a` (`p1f-results-delta.md:7`-`:11`,
  `:43`-`:50`, `:74`-`:82`, `:89`-`:95`, `:101`-`:118`).
- Local `/tmp` review found the cited artifacts present: V1 identity
  `/tmp/skv13-p1/artifacts/identity.txt`, V2 identity
  `/tmp/skv13-p1-v2/artifacts/identity.txt`, 34 parse profiles and 34 parse
  sidecars, 14 typed profiles and 14 typed sidecars, 34 V2 direct profiles and
  34 direct sidecars, 85 mode-III profiles and 85 mode-III sidecars, and one
  CSS profile plus sidecar. V2 direct and mode-III status ledgers report zero
  bad return codes; grep over V2 direct/mode-III/CSS logs found no panic text.

## Findings

| Id | Disposition | Evidence | CH4 impact | Required fold action |
|---|---|---|---|---|
| CH4-001 | ACCEPT | V2 direct is now a rerunnable profile surface: P1-B gives V2 identity, host/build flags, exact cargo build, exact `samply record` template, status checks, profile paths, and sidecar paths (`p1b-samply-mode-2.md:17`-`:52`). Local `/tmp` status confirms 34 direct rows, 0 bad rc, 34 profiles, 34 sidecars, and no panic text. | The V1 direct panic-path blocker is closed for CH4. | Preserve the V2 direct identity, status TSV, logs, profiles, sidecars, and top-20 TSV as the fold-forward authority. |
| CH4-002 | REVISE | P1-A keeps the V1 parse-only capture as authority and lists run id/build identity, but the build provenance is weaker than V2 direct: build flags are described as release/debug/split-debuginfo/LTO/native target CPU, while the exact cargo build command and exact `RUSTFLAGS` used for `/tmp/skv13-profile-target-0a7b41c5/release` are not carried in the P1-A method block (`p1a-samply-mode-1.md:7`-`:10`, `:25`-`:31`, `:33`-`:65`, `:140`-`:149`). P1-D and P1-E also retain V1 parse/typed PMU and typed profiles (`p1d-pmu-cycles.md:15`-`:22`, `p1e-hot-leaf-attribution.md:101`-`:106`). | A third party can inspect the retained V1 artifacts, but cannot rebuild the retained binary from the document alone with the same specificity as V2 direct. | Either rerun parse and typed under the V2 build identity, or add a canonical V1 retained-capture provenance block: exact cargo build command, `CARGO_TARGET_DIR`, `RUSTFLAGS`, profile settings, rustc version, binary paths, and binary hashes. |
| CH4-003 | REVISE | P1-C closes mode-III coverage with 85/85 rows and gives build/capture commands, but the harness source is a temporary crate at `/tmp/skv13-mode3-profiler` (`p1c-samply-mode-3.md:25`-`:39`). Local review confirms `/tmp/skv13-mode3-profiler/Cargo.toml`, `Cargo.lock`, and `src/main.rs` exist now, but they are not part of the checked-in artifact set. | The current reviewer can reproduce mode III while `/tmp` survives; a later third party cannot reconstruct the harness if `/tmp` is gone. | Fold the mode-III harness source into a checked-in profile helper, or inline a source/hash appendix that records `Cargo.toml`, `Cargo.lock`, `src/main.rs`, and the exact dependency path assumptions. |
| CH4-004 | REVISE | P1-A/B/C all use `samply record --save-only --unstable-presymbolicate -r 1000`; P1-A explicitly says this is not a clean interactive samply pass (`p1a-samply-mode-1.md:42`-`:49`, `:75`, `:131`-`:132`; `p1b-samply-mode-2.md:37`-`:39`, `:106`-`:109`; `p1c-samply-mode-3.md:35`-`:38`). P1-E resolves saved profiles via `/tmp/skv13-p1-v2/summary/extract-hotleaf-top20-equivalent.py`, and local review confirms the script and TSVs exist, but the extractor is only in `/tmp` (`p1e-hot-leaf-attribution.md:25`-`:39`). | The saved profiles and sidecars are auditable now, but the symbol-resolution pipeline remains sidecar-dependent and partly ephemeral. This is a CH4 reproducibility gap even before CH6 judges symbol quality. | Either recapture P1-A/B/C with the accepted interactive samply workflow, or fold a reproducible offline-symbolication package: checked-in extractor, samply version, sidecar generation command, input/output path contract, and a verification command that regenerates `hotleaf_top20.tsv` without changing the evidence. |
| CH4-005 | REVISE | CSS has a log and one profile/sidecar, and P1-F records equality/throughput/top-leaf signals (`p1f-results-delta.md:24`-`:27`, `:74`-`:82`, `:137`-`:140`; `p1e-hot-leaf-attribution.md:111`-`:115`). However, the P1 artifacts do not provide the exact CSS build command, binary/harness path, `samply record` command, iteration command, or CSS capture status ledger comparable to direct and mode III. | CSS evidence is inspectable but not rerunnable from the method block. That blocks CH4 ACCEPT for the CSS row. | Add a CSS method block with exact cargo build command, binary path, input fixture, iteration count, equality/throughput command, `samply record` command, profile/sidecar paths, log path, run identity, and status check. |
| CH4-006 | ACCEPT WITH ROUTED GAP | P1-D no longer pretends branch/L1/LLC are present. It gives the xctrace export command, confirms `cpu-state.xml` exists, and marks branch-miss, L1-miss, and LLC-miss fields `unavailable_from_current_export` rather than zero (`p1d-pmu-cycles.md:34`-`:51`, `:57`-`:64`, `:117`-`:120`). Local review confirms the xctrace capture status has 164 rows, 0 bad rc, and the exported `cpu-state.xml` is present. | CH4 can accept the branch/L1/LLC status as an explicit unsupported/export-unavailable route, but not as numeric PMU coverage. | Preserve the unavailable status in the consolidated fold. If numeric counters are later required, add tool/version evidence plus per-row branch/L1/LLC columns or per-row `unsupported:<reason>` fields. |
| CH4-007 | REVISE | P1-F is honest that typed coverage is 7/17, same-run sonic parse PMU is absent, CSS throughput is method-mismatched, and sidecar comparators are absent or `n/a` (`p1f-results-delta.md:43`-`:50`, `:89`-`:95`, `:101`-`:118`). | This is not a CH4 rejection by itself because the gaps are declared, but the consolidated fold must not present these surfaces as complete or gate-admissible measurements. | Carry explicit `missing`, `stale-comparator`, `method-mismatched`, and `profile_signal_not_gate_admission` labels into the consolidated S-P1 fold. Do not promote typed 10/17, parse same-run SOTA deltas, CSS absolute Mbps, or absent C/C++ sidecars as reproducible profile coverage. |

## Required Fold Set

1. Add canonical build/run provenance for every retained capture cohort: V1
   parse/typed/PMU, V2 direct, V2 mode III, and CSS. Include run id, host
   triple, cargo command, `CARGO_TARGET_DIR`, `RUSTFLAGS`, profile settings,
   rustc version, binary path, and binary hash.
2. Materialize the mode-III temporary harness outside `/tmp`, or record a
   source/hash appendix sufficient to rebuild it after `/tmp` is gone.
3. Make the saved-profile sidecar pipeline reproducible: checked-in extractor
   or fully inlined script, exact samply version/command, sidecar expectations,
   and a no-mutation verification command for the top-20 TSVs.
4. Add the missing CSS capture method: build command, equality/throughput
   command, `samply record` command, profile/sidecar/log/status paths, and run
   identity.
5. Keep branch/L1/LLC as `unavailable_from_current_export` unless a later fold
   produces row-level numeric exports. Do not treat missing cache counters as
   zeros.
6. Keep the P1-F labels for incomplete surfaces: typed 7/17 only, parse sonic
   PMU stale/absent, CSS absolute Mbps method-mismatched, and C/C++ comparator
   sidecars absent or `n/a`.

V2 is close enough to fold with revisions, but CH4 should not mark ACCEPT until
the ephemeral `/tmp` and saved-profile sidecar dependencies are made
third-party reproducible.
