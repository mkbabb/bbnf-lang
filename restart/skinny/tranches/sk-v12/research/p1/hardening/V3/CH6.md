# SK-V12 S-P1 Hardening V3 CH6: Anti-Paper-Close

Verdict: ACCEPT.
Date: 2026-05-20.
Lens: CH6 anti-paper-close after commit `ffe5553d`.
Scope: S-P1 artifacts, capture manifest, replay TSV, V1/V2 hardening and fold
files, `skinny/RESULTS.md`, and `skinny/REDRESS.md`.
Output: this file only.

## Finding 1 - The V2 blockers are folded, not papered over

V1 CH6 revised the packet because exact per-row self-time percentages were
absent. The V1 fold chose the export path, not the downgrade path: parse Time
Profiler XML was exported, product Time Profiler rows were recaptured under
`/tmp/skv12-p1/direct-xctrace/time-profiler-v2`, and derived self-time summary
and detail TSVs were generated.

V2 then left two non-CH6 blockers in the packet: line-zero source anchors in the
derived self-time tables and placeholder-based replay. Commit `ffe5553d` folds
both without behavior-source, RESULTS, or REDRESS changes. The current
`/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv` has 82 data rows with 0
`top_leaf_source` values ending in `:0`, 0 unresolved values, and 0
`UNRESOLVED_LINE_ZERO` markers. The detail table has 410 data rows with 0
`source` values ending in `:0`, 0 unresolved values, and 0 markers. The manifest
records the same state and names those TSVs as the retained self-time authority.

The replay fold is also concrete. `skv12-p1-replay.tsv` is a 14-column TSV with
506 replay rows and 0 placeholder rows: 82 `pmu`, 82 `samply`, 82
`xctrace-cpu-counters`, 82 `xctrace-time-profiler-primary`, 34
`xctrace-time-profiler-export`, 48 `xctrace-time-profiler-export-primary`, 48
`xctrace-time-profiler-product-v2`, and 48
`xctrace-time-profiler-product-v2-export`. Each row carries cwd, full command,
corpus, mode, alias, iteration count, binary path, rc policy, output artifact,
and status artifact. The remaining `<corpus>` examples in P1-A/P1-B/P1-D and
the manifest are explicitly readable command shapes; the replay TSV is now the
authoritative command surface.

## Finding 2 - Required P1 evidence is present or explicitly absent

The fresh primary capture is complete for the claimed P1 lanes:
`/tmp/skv12-p1/pmu/capture_status.tsv` has 328 PASS rows: 82 PMU captures, 82
samply captures, and 164 primary xctrace captures. The on-disk inventory matches
the packet shape: 82 samply `.json.gz` files, 82 samply `.json.syms.json`
sidecars, 164 primary `.trace` bundles, 34 parse XML exports, 48 product-v2 XML
exports, and 48 product-v2 trace bundles.

The packet remains honest about evidence it does not have. Samply `--save-only`
rows are retained artifact-only evidence, while exported xctrace Time Profiler
XML and the derived TSVs are the self-time authority. PMU rows expose cycles,
instructions, c/B, CPI, user ns, system ns, and checksums only; branch/L1/LLC
claims are absent and not inferred. Mode III call stacks remain explicitly
absent: P1-C reports 0/17 fresh Mode III samply rows and no fresh structural
capture under `/tmp/skv12-p1`, and the manifest forbids S-P2/S-P3 from using W0
Mode III symbols as fresh SK-V12 hot-leaf authority.

The source baseline split is acceptable. The profile capture baseline remains
`50bd1648`, while the audited repo commit is `ffe5553d`; the intervening fold is
documentation/replay hardening only. A source/RESULTS/REDRESS diff check from
`50bd1648` to `ffe5553d` over `skinny/crates`, `skinny/Cargo.toml`,
`skinny/Cargo.lock`, `skinny/RESULTS.md`, and `skinny/REDRESS.md` returns no
paths.

## Finding 3 - No row or gate is closed on profile-only evidence

The row authorities remain separate from S-P1 profiling. The manifest states
that `skinny/RESULTS.md` remains result authority and that the manifest moves no
rows. P1-D says PMU values do not move `skinny/RESULTS.md`, admit a direct or
typed row, or change the SK-V12 opening `N-direct / NoGo` surface. P1-E keeps
parse rows diagnostic, direct rows as four guards plus thirteen pre-blocked
residuals, and typed rows as guards. P1-F records the live surface as
`parse_only` 16 `S / NO-GO` plus one `L / NO-GO`, `direct_to_struct` four
`A / GO` plus thirteen `N-direct / NO-GO`, `real_typed_struct` seven
`A / GO`, no generated non-JSON baseline, and overall `N-direct / NoGo`.

`skinny/RESULTS.md` agrees: overall remains `N-direct / NoGo`, Track 1 is the
generated JSON parser, and Track 2 is the independent hand-coded parser.
REDRESS 119 closes the direct residuals only as a measured fixpoint, with no
behavior source intervention, gate semantic change, or RESULTS row movement.
REDRESS 120 closes SK-V11 as a measured fixpoint, not as overall direct `GO` or
grammar-generalization admission, and carries the generated non-JSON baseline
blocker forward.

## Finding 4 - S-P2/S-P3 constraints remain honest

The packet does not substitute JSON profile facts, PMU facts, parse-only facts,
masking probes, structural-scan facts, or the REDRESS 111 non-JSON report lane
for the required generated non-JSON baseline. P1-B says no non-JSON product row
exists in its capture set. P1-E names the current blocker: runtime/codegen still
routes through the JSON provider and no generated CSS L4, Sheets, or BBNF-self
runtime baseline exists. P1-F keeps the REDRESS 111 report lane separate from a
generated baseline and routes S-P2/S-P3 to stand up exactly one generated
non-JSON direct or typed baseline first.

Carry-forward constraints are therefore intact: exact xctrace self-time rows may
guide S-P2/S-P3 planning, but they do not admit rows; Mode III is W0 diagnostic
throughput only unless a later fresh call-stack capture exists; PMU/cycles and
structural/masking evidence remain nonproducers; JSON residuals stay pre-blocked
until the non-JSON priority succeeds or records a measured block and a later
pass supplies material evidence beyond REDRESS 114-119.

## Verdict

ACCEPT. The current S-P1 packet is anti-paper-close for CH6: required profile
evidence is either present and citable or explicitly absent, exact self-time and
replay blockers from V2 are folded with concrete artifacts, no row or gate is
closed on profile-only evidence, and S-P2/S-P3 constraints remain enforceable.
