# SK-V13 S-P1 V2 CH6: Anti-Paper-Close

Pass: S-P1 Profile. Cycle: V2.
Date: 2026-05-21.
Lens: CH6 ANTI-PAPER-CLOSE.
Disposition: REVISE.

## Verdict

REVISE. V2 is no longer the V1 paper-close state: direct samply panic captures
were replaced by 34 non-panic direct profiles, mode III has 85 profile/log/counter
rows, and CSS declaration-values now has a profile plus equality/throughput log.
Those artefacts exist on disk and are citable as artefacts.

V2 still cannot ACCEPT under CH6. PASS-1-PROFILE requires an orchestrator-citable
flame profile, resolvable symbols, and interactive/symbol-preserving samply rather
than `--save-only` (`restart/prompts/skinny/PASS-1-PROFILE.md:155` and `:251`).
P1-A/P1-B/P1-C still use `samply record --save-only --unstable-presymbolicate`,
the saved profiles report `symbolicated=false`, and the V2 top-20 TSV has broad
file:line gaps: 1,065 top-20 rows lack file or line, including 87 rank-1 rows
(85 mode-III rank-1 rows, direct `y_string_unicode` Track 2, and CSS).

The fold may preserve V2 as measured/provisional profile evidence, but must not
close S-P1 as fully symbol-resolved.

## Findings

### CH6-001 - Save-only sidecar symbolication remains provisional

Severity: REVISE.

Evidence:

- The prompt states that CH6 requires the flame file on disk and resolvable
  symbols, and that samply needs interactive `samply record`, not `--save-only`
  (`restart/prompts/skinny/PASS-1-PROFILE.md:155` through `:160`; `:251`
  through `:254`).
- P1-A retains the V1 parse capture and names the limitation explicitly:
  `--save-only --unstable-presymbolicate` at
  `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:10`,
  with the command at `:42` through `:48` and the warning that saved profiles
  report `symbolicated=false` at `:75`.
- P1-B direct V2 also uses save-only samply
  (`restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:10`,
  `:37` through `:39`) and asks CH6 to decide whether offline sidecars are
  sufficient (`:106` through `:109`).
- P1-C mode III also uses save-only samply
  (`restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:10`,
  `:35` through `:38`).
- On disk, sampled profiles inspected from `/tmp/skv13-p1/...` and
  `/tmp/skv13-p1-v2/...` report `meta.symbolicated=false`.

Assessment:

Sidecars are useful evidence, and direct V2 sidecars carry enough file:line data
for most rank-1 direct rows. They are not equivalent to the prompt's clean
interactive samply discipline. V2 must keep this as
`provisional_sidecar_symbolication` unless a symbol-preserving capture or a
complete post-symbolication ledger is supplied.

Required fold action:

- Keep every save-only-derived hot leaf tagged as sidecar/provisional unless the
  row cites profile path, sidecar path, log path, symbol, percent self-time, and
  source file:line.
- Rows with only function names or system/timer leaves remain unresolved for
  primitive attribution.

### CH6-002 - Direct profile artefacts exist, but one direct rank-1 row is timer/noise

Severity: REVISE.

Evidence:

- P1-B verifies 34 direct captures, 34 direct profiles, and 34 direct sidecars
  with zero bad return codes (`p1b-samply-mode-2.md:44` through `:52`).
- `/tmp/skv13-p1-v2/samply/direct_capture_status.tsv` has 34 data rows with
  `rc=0`, and `/tmp/skv13-p1-v2/samply/profiles/` contains 34
  `direct__*.json.gz` files plus 34 `direct__*.json.syms.json` sidecars.
- P1-B marks the direct panic-path defect as resolved at
  `p1b-samply-mode-2.md:96` through `:102`.
- The exception is `y_string_unicode` Track 2: P1-B reports rank-1
  `mach_absolute_time` (`p1b-samply-mode-2.md:84`) and warns it is timer-noisy
  (`:116` through `:118`). The corresponding
  `/tmp/skv13-p1-v2/summary/direct_summary.tsv` row has no file/line for that
  rank-1 timer leaf.

Assessment:

The V1 direct panic blocker is closed. Direct V2 is acceptable as measured
direct-profile evidence except for the timer-dominated `y_string_unicode`
Track 2 row, which cannot stand as a parser hot-leaf attribution.

Required fold action:

- Fold direct V2 as non-panic measured evidence.
- Keep `direct/y_string_unicode/track2` as `timer_noise_rank1`; cite a lower
  parser leaf from the top-20 TSV or recapture with a longer/narrower run before
  using it as a parser attribution.

### CH6-003 - Mode III artefacts exist, but source file:line resolution is not closed

Severity: REVISE.

Evidence:

- P1-C claims 85/85 captured profiles and zero bad return codes
  (`restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:11`,
  `:43` through `:49`), and routes unsupported probes at `:51` through `:53`.
- On disk, `/tmp/skv13-p1-v2/mode3/profiles/` contains 85
  `mode3__*.json.gz` profiles plus 85 `.json.syms.json` sidecars; the mode3 log
  directory contains 85 logs; `/tmp/skv13-p1-v2/mode3/capture_status.tsv` has
  85 `rc=0` data rows.
- P1-C's own text admits sidecar file:line gaps for NEON leaves
  (`p1c-samply-mode-3.md:58` through `:62`, `:90` through `:91`, `:121`
  through `:122`).
- The emitted `/tmp/skv13-p1-v2/summary/hotleaf_top20.tsv` has no file or line
  for any of the 85 mode-III rank-1 rows. This includes all structural scalar,
  structural SIMD, host-call, alternate-scalar, and cold-first rank-1 claims.

Assessment:

V2 fixes the V1 mode-III absence, so this is no longer a missing-profile
REJECT. It is still not ACCEPT because the citable top-20 ledger does not carry
source file:line for the rank-1 mode-III hot leaves. Manual representative
anchors for `scan_tail` and `scan_structurals` are not a substitute for
per-row symbol/file/line attribution.

Required fold action:

- Regenerate mode-III symbol extraction with file:line using a symbol-preserving
  capture, dSYM/addr2line-backed post-processing, or an equivalent audited
  mapping.
- Until then, mark the 85 mode-III rank-1 rows as `function_only_sidecar`, with
  profile/log/sidecar paths preserved and no claim of file:line closure.

### CH6-004 - CSS now has a profile, but it is timer/fact-sink dominated

Severity: REVISE.

Evidence:

- P1-E includes CSS in scope and reports CSS declaration-values 1/1 coverage
  (`restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:5`,
  `:11`).
- On disk, `/tmp/skv13-p1-v2/css/profiles/` contains
  `css_l4_declaration_values_all_modes.json.gz` and its `.json.syms.json`
  sidecar; `/tmp/skv13-p1-v2/css/logs/css_l4_declaration_values_all_modes.log`
  records `strict_equality pass`.
- P1-E reports the CSS top leaves as 17.6% `mach_absolute_time`, 13.7%
  `LocalFactSink::finish`, and 7.5% `FactSink::finish`, and classifies the row
  as throughput/equality measured but not parser-primitive-attributed
  (`p1e-hot-leaf-attribution.md:67` through `:71`).
- P1-F repeats that CSS top profile leaf is `mach_absolute_time` and that the
  profile is timer/fact-sink dominated
  (`restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:74`
  through `:82`).
- The CSS rows in `/tmp/skv13-p1-v2/summary/hotleaf_top20.tsv` have no file or
  line for the top 20 emitted leaves.

Assessment:

CSS is no longer "throughput-only"; a profile artefact exists. But it cannot
close CSS hot-leaf attribution because the rank-1 leaf is a timer, the next
leaves are sink finalization, and the TSV lacks source file:line.

Required fold action:

- Keep CSS as `throughput_equality_profiled_but_hot_leaf_unresolved`.
- Recapture or re-extract CSS with a narrower/longer profile that separates
  Track 1 parser work from timer and sink finalization, and cite parser leaf
  symbol + percent self-time + file:line before using it in S-P2.

### CH6-005 - Typed rows remain a known product-surface gap, not a profiler close

Severity: REVISE.

Evidence:

- P1-B says typed evidence is retained only for the V1 seven-row generated typed
  subset and that ten unsupported corpora were not invented in V2
  (`p1b-samply-mode-2.md:86` through `:88`, `:119` through `:120`).
- P1-D reports typed PMU coverage as 28 rows, meaning seven generated typed rows
  times four modes (`p1d-pmu-cycles.md:57` through `:64`) and reiterates that
  ten absent typed rows are product-surface gaps (`:123` through `:126`).
- P1-E frontmatter reports typed 7/17 and marks ten corpus cells as
  `missing typed row` (`p1e-hot-leaf-attribution.md:10` through `:11`, `:51`,
  `:56` through `:57`, `:59` through `:65`).
- P1-F reports JSON `real_typed_struct` as 7/17 fresh profile coverage and says
  the ten missing rows cannot be counted as profiled or admitted
  (`p1f-results-delta.md:43` through `:50`, `:101` through `:106`, `:113`
  through `:116`).
- On disk, `/tmp/skv13-p1/samply/profiles/` contains 14 typed profile files and
  14 typed sidecars: seven corpora times Track 1/Track 2.

Assessment:

This is not an unqualified paper close because V2 names the limitation. It is
still a closure blocker: the pass cannot state typed 17/17 hot-leaf coverage.

Required fold action:

- Preserve the ten missing typed rows as `missing_product_surface`, not
  `profiled`.
- If SK-V13 requires all 51 JSON rows for the addendum, typed generation or
  routing must happen before those ten rows can enter S-P1 as profiled surfaces.

### CH6-006 - PMU gaps are honestly routed

Severity: ACCEPT.

Evidence:

- P1-D reports parse, direct, typed, direct-log, and mode-III counter rows with
  zero bad return codes (`p1d-pmu-cycles.md:57` through `:64`).
- It explicitly states branch-miss, L1-miss, and LLC-miss columns are
  unavailable from the current xctrace export, not zero (`p1d-pmu-cycles.md:47`
  through `:51`, `:119` through `:120`).

Assessment:

For CH6, this is acceptable: absent branch/cache counters are not hidden or
papered over. They remain a PMU completeness limitation for consolidation, not
an unsupported "profiled" claim.

Required fold action:

- Keep branch/L1/LLC as `unavailable_from_current_export`; do not infer memory
  behavior from absent counters.

## Required V3 Fold Actions

1. Preserve V2 direct and mode-III artefact inventories, but tag save-only
   sidecar symbolication as provisional unless file:line is present per row.
2. Regenerate mode-III top-20 attribution with source file:line for every
   rank-1 hot leaf, or mark all 85 mode-III rank-1 rows
   `function_only_sidecar`.
3. Keep CSS declaration-values as equality/throughput/profile-exists evidence
   only; recapture or re-extract before naming a CSS parser hot leaf.
4. Keep direct `y_string_unicode` Track 2 as timer-noisy until a parser leaf is
   rank-1 or an explicit lower-rank parser leaf is cited.
5. Do not count the ten absent typed rows as profiled; keep them as
   `missing_product_surface` until generated/routed and captured.
6. Preserve P1-F's `profile_signal_not_gate_admission` boundary for all rows
   depending on stale comparators, missing typed surfaces, or CSS timer profiles.

## Disposition Matrix

| Artifact | CH6 disposition | Reason |
|---|---|---|
| P1-A | REVISE | Parse profiles and sidecars exist, but remain save-only/offline-symbolicated with unresolved or line-poor cells. |
| P1-B | REVISE | Direct V2 profiles are non-panic and mostly symbol/file-line resolved, but save-only remains provisional; typed is 7/17; `y_string_unicode` Track 2 is timer rank-1. |
| P1-C | REVISE | Mode-III artefacts exist and unsupported probes are routed, but the top-20 TSV lacks file:line for all 85 mode-III rank-1 rows. |
| P1-D | ACCEPT | Counter gaps are explicitly marked unavailable, not zero or profiled. |
| P1-E | REVISE | Honest synthesis, but it relies on provisional sidecar symbolication, CSS timer/fact-sink leaves, and missing typed rows. |
| P1-F | ACCEPT | Extraction ledger keeps profile signals separate from gate admission and names missing/stale surfaces. |

Overall S-P1 V2 CH6 disposition: REVISE.
