# SK-V13 S-P1 V1 CH6: Anti-Paper-Close

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-21.
Lens: CH6 ANTI-PAPER-CLOSE.
Disposition: REJECT.

## Verdict

REJECT. V1 contains useful evidence, but it cannot close S-P1 as a profile pass.
The parse and typed samply profile files exist on disk, and the PMU ledgers/logs
exist for parse/direct/typed rows, but several required "profiled" surfaces are
either unsymbolicated save-only captures, panic-path captures, or absent.

The fold must preserve the valid parse PMU/profile inventory and typed subset,
but it must not promote direct-to-struct, mode III masking probes, structural
scan-only, or CSS hot leaves as profiled.

## Findings

### CH6-001 - Samply parse evidence exists, but it is save-only and not cleanly symbolicated

Severity: REVISE.

Evidence:

- `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:10`
  declares `samply record --save-only --unstable-presymbolicate`.
- `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:34`
  and `:38` show the save-only commands, contrary to the prompt's interactive
  samply discipline.
- `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:67`
  admits the saved profiles report `symbolicated=false`.
- On disk, `/tmp/skv13-p1/samply/profiles/parse__twitter__track1.json.gz`
  has `meta.symbolicated=false` and no frame file/line in the profile JSON.
- `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:78`,
  `:79`, `:90`, and `:91` retain line-poor, unresolved, or generic inlined
  hot-leaf cells.

Assessment:

This is not a paper close for artifact existence: parse profile files and
sidecars exist for 17 corpora x 2 tracks. It is a paper close if V1 treats those
profiles as fully CH6-clean symbolicated samply evidence. Sidecar-derived
file:line attribution can be used as provisional evidence only where a concrete
source line is present.

Required fold action:

- Re-run P1-A with interactive `samply record` or equivalent symbol-preserving
  capture for 17/17 corpora x Track 1/Track 2, or keep all save-only rows marked
  `provisional_sidecar_symbolication`.
- For every row retained from sidecars, require a concrete source file:line and
  an unresolved-sample percentage. Rows with no file:line remain unresolved.

### CH6-002 - Mode II direct samply profiles are panic-path artifacts, not parser profiles

Severity: REJECT.

Evidence:

- `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:11`
  states direct samply has `0/17 valid hot-leaf profiles`.
- `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:55`
  through `:58` explain the samply wrapper returned `rc=0` while the workload
  panicked before timed parsing.
- `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:62`
  through `:68` refuse direct self-time symbols and quote the panic.
- `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:75`
  through `:93` mark every direct samply profile invalid.
- On disk, `/tmp/skv13-p1/samply/logs/direct__twitter__track1.log` and the
  other direct samply logs contain fixture lookup/read panics, while
  `/tmp/skv13-p1/pmu/logs/direct__twitter__track1.log` shows the non-panic PMU
  workload did run.

Assessment:

The direct PMU rows are valid throughput/cycles evidence, but the direct samply
profile files are not hot-leaf evidence. Any fold that says direct_to_struct was
profiled at symbol level from these samply files would be unsupported.

Required fold action:

- Fix the direct samply workload path handling and recapture 17/17 direct
  Track 1/Track 2 profiles with non-panic logs.
- Until recaptured, P1-B and P1-E must keep direct hot leaves as
  `unprofiled: samply panic`, with PMU c/B cited separately.

### CH6-003 - Mode III masking-probe and structural-scan coverage is absent

Severity: REJECT.

Evidence:

- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:10`
  states no dedicated mode III samply capture was found.
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:11`
  reports `0/17` P1-C mode III samply coverage.
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:49`
  says the fresh capture contains only `parse`, `direct`, and `typed` lanes.
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:53`
  through `:54` correctly refuses hot-leaf claims for the required mode III
  surfaces.
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:171`
  confirms masking-probe rows are not independent PMU rows in
  `/tmp/skv13-p1/pmu/pmu_rows.tsv`.
- On disk, `find /tmp/skv13-p1` finds no `host`, `eager`, `alternate`, `cold`,
  `structural`, or `scan` profile/PMU artifacts beyond the CSS TSV.

Assessment:

This is the largest CH6 blocker. P1-C was explicitly assigned masking-probe and
structural-scan-only profiles; V1 has none. Adjacent parse/direct/typed evidence
cannot be folded as mode III coverage.

Required fold action:

- Capture 17/17 samply and PMU rows for `host_call_eager_decode`,
  `alternate_scalar_plan`, `cold_first_parse`, and structural-scan-only.
- Either capture or explicitly route `host_call_dispatch_overhead`,
  `alternate_pext_mask_plan`, and `alternate_dispatch_table_plan` with a stated
  unsupported/invalid reason.
- Do not allow S-P1 convergence until P1-C has actual artifact paths, non-panic
  logs, symbol self-time, and file:line attribution for every supported mode III
  row.

### CH6-004 - CSS L4 has throughput evidence but no hot-leaf profile artifact

Severity: REJECT.

Evidence:

- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:5`
  includes CSS L4 measurement in the hot-leaf attribution scope.
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:11`
  states CSS measurement has one row and no samply profile.
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:81`
  through `:85` marks the CSS hot leaf unprofiled and cites only the
  measurement TSV.
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:128`
  through `:130` warns the CSS row cannot yet be attributed to CSS-class
  leaves.
- On disk, `/tmp/skv13-p1/css/css_l4_declaration_values_measurement.tsv` exists,
  but no CSS samply/xctrace profile exists under `/tmp/skv13-p1/samply/profiles`
  or `/tmp/skv13-p1/xctrace/traces`.

Assessment:

The CSS row can be cited for equality/throughput only. It cannot be used as CSS
hot-leaf coverage and cannot close a CSS attribution claim.

Required fold action:

- Capture CSS L4 declaration-values samply or xctrace Time Profiler artifacts
  with non-panic logs, symbol self-time, and source file:line.
- Keep the current CSS row as `throughput_measured_only` until that capture
  exists.

### CH6-005 - P1-E leaves direct and typed gaps explicit, but still cannot resolve every hot-leaf cell

Severity: REVISE.

Evidence:

- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:61`
  through `:79` mark all direct rows `unprofiled: direct samply panic`.
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:65`,
  `:70`, `:71`, `:73` through `:79` mark ten typed rows unprofiled because
  they were not captured.
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:122`
  through `:127` gives the direct panic cause and source file:line.

Assessment:

P1-E is honest about the gaps, so the document itself is not making an
unsupported direct/CSS hot-leaf claim. But it fails the S-P1 output goal of
resolving every unprofiled cell to named symbol + percent self-time + file:line.

Required fold action:

- After P1-B and CSS recapture, regenerate P1-E from the corrected artifacts.
- Keep the ten absent typed rows as explicit missing generated-row coverage
  unless the bench surface adds valid typed workloads for them.

### CH6-006 - P1-D PMU evidence is real but incomplete for the requested counter set and masking attribution

Severity: REVISE.

Evidence:

- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:69`
  through `:75` report 0 bad return codes for parse/direct/typed PMU and
  xctrace rows.
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:77`
  states branch misses, L1 misses, and LLC misses are absent from the row file.
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:149`
  repeats that only cycles, instructions, c/B, CPI, checksums, and log paths are
  exposed.
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:171`
  states masking-probe rows are absent from the PMU TSV.
- On disk, `/tmp/skv13-p1/pmu/pmu_rows.tsv` contains parse/direct/typed rows
  with `rc=0`, but its schema has no branch/L1/LLC columns.

Assessment:

PMU c/B is usable for parse/direct/typed rows. It does not satisfy the full P1-D
counter contract and cannot attribute masking signals.

Required fold action:

- Export branch-miss, L1-miss, and LLC-miss counters or explicitly mark the
  host/tool as unable to export them.
- Add PMU rows for mode III masking probes and structural-scan-only, or route
  the unsupported probes with explicit cause.

### CH6-007 - P1-F avoids hot-leaf promotion, but its fresh classifications rely on profile gaps

Severity: REVISE.

Evidence:

- `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:49`
  through `:52` accurately reports parse comparator gaps, direct comparator
  coverage, typed incompleteness, and CSS measurement-only status.
- `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:139`
  through `:161` list stale comparator/profile fields and tell P1-E to replace
  placeholders.

Assessment:

P1-F is acceptable as an extraction ledger, not as evidence that the missing
profile surfaces are closed. Its fresh classes must remain profile signals until
the missing CH6 artifacts land.

Required fold action:

- In the consolidated hardening fold, tag P1-F classifications as
  `profile_signal_not_gate_admission` wherever they depend on stale parse
  comparators, absent typed rows, or CSS measurement-only data.

## Required V2 Fold Actions

1. Re-run P1-A/P1-B/P1-C samply without save-only symbol loss, or preserve a
   named provisional state for sidecar-only symbolication.
2. Fix direct samply fixture/path handling and recapture 17/17 direct
   `direct_to_struct` profiles with non-panic workload logs.
3. Capture the missing P1-C mode III matrix: 17/17
   `host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse`, and
   structural-scan-only samply plus PMU rows.
4. Capture CSS L4 hot-leaf artifacts; the current CSS TSV is throughput-only.
5. Export or explicitly waive branch/L1/LLC counters for P1-D; do not imply
   zero misses.
6. Regenerate P1-E after corrected captures and keep all remaining unsupported
   rows explicitly `unprofiled:<cause>`.
7. Preserve P1-F as an extraction/profile-signal ledger only; do not use it to
   close missing profile evidence.

## Disposition Matrix

| Artifact | CH6 disposition | Reason |
|---|---|---|
| P1-A | REVISE | Parse artifacts exist, but save-only profiles are unsymbolicated and several rows are line-poor/unresolved. |
| P1-B | REJECT | Direct samply artifacts profile panic paths; typed subset is valid but incomplete by product surface. |
| P1-C | REJECT | Required mode III and structural-scan profile/PMU artifacts are absent. |
| P1-D | REVISE | PMU c/B rows are real, but branch/cache counters and masking-probe PMU rows are missing. |
| P1-E | REVISE | Honest gap reporting, but direct, CSS, and ten typed cells remain unprofiled. |
| P1-F | REVISE | Useful extraction ledger; not evidence that profile gaps are closed. |

Overall S-P1 V1 CH6 disposition: REJECT.
