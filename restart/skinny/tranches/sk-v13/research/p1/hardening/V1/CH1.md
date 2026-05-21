# SK-V13 S-P1 V1 CH1 Correctness

Disposition: REJECT

Scope reviewed: the six S-P1 V1 artifacts under
`restart/skinny/tranches/sk-v13/research/p1/`, checked against
`restart/prompts/skinny/PASS-1-PROFILE.md` CH1 and spot-checked against
`/tmp/skv13-p1`.

## Summary

The V1 profile corpus is useful but not CH1-converged. The PMU c/B ledger is
real for captured parse/direct rows and seven typed rows, but mandatory mode
III profiling is absent, direct samply profiles captured panic paths, several
hot-leaf claims lack per-row samply artifact/symbol-path citation, and P1-E
leaves all direct hot-leaf cells unresolved. These are blocking correctness
defects, not polish issues.

## Accepted CH1 Facts

- PMU c/B values in P1-D are derived from real `/tmp/skv13-p1/pmu/pmu_rows.tsv`
  rows, not Criterion estimates. P1-D cites the TSV schema with `cycles`,
  `instructions`, and `cycles_per_byte` at
  `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:51` and
  reports copied c/B values at
  `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:149`.
- Parse PMU coverage is 17 corpora x Track 1/Track 2, and direct PMU coverage
  is 17 corpora x Track 1/Track 2/sonic/serde. P1-D records this at
  `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:69`.
- P1-B and P1-E honestly identify the direct samply panic rather than treating
  those profiles as valid parser hot leaves. See
  `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:55` and
  `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:122`.

## Findings

### CH1-COR-001 - Mandatory P1-C mode III coverage is 0/17

P1-C is required to profile `host_call_eager_decode`, `alternate_scalar_plan`,
`cold_first_parse`, and the structural-scan-only path for all 17 corpora. V1
does not contain those captures. P1-C states "no dedicated mode III samply
capture found" and "Corpus coverage: 0/17" at
`restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:10` and
`restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:11`; its
negative inventory reports only parse/direct/typed lanes at
`restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:49`, and its
required-probe table marks every named P1-C probe missing at
`restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:93`.

Required fold action: rerun P1-C with 17/17 samply profile artifacts and PMU
rows for each required masking/structural surface, then update P1-C, P1-D, and
P1-E with per-corpus profile paths, symbol paths, % self-time, and file:line.
If any probe is unsupported on this host, record an explicit unsupported reason
per corpus instead of borrowing adjacent parse/direct evidence.

### CH1-COR-002 - Direct-to-struct hot leaves remain unresolved for 17/17 corpora

P1-B says direct samply has "0/17 valid hot-leaf profiles" at
`restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:11` and then
admits no direct self-time symbol at
`restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:62`. The
failure is real in `/tmp`: `direct__twitter__track1.log:1` panics at fixture
lookup, and `direct__update_center__track1.log:1` passes a quoted absolute path
that fails to read. P1-E consequently leaves every direct cell as
`unprofiled: direct samply panic` at
`restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:63`
through `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:79`.

Required fold action: fix the direct profiling command surface or fixture
resolution, rerun direct samply for all 17 corpora, and replace every direct
`unprofiled` cell with a named symbol, % self-time, full samply profile path,
sidecar/symbol path, and source file:line. Keep the PMU rows, but do not let
PMU throughput stand in for hot-leaf attribution.

### CH1-COR-003 - Hot-leaf citations are not per-claim complete

CH1 requires every hot-leaf claim to cite a samply symbol path, % self-time, and
source file:line. P1-A gives a profile path pattern once at
`restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:71`, but the
per-corpus claims cite only shorthand artifact names in the table starting at
`restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:73`. Some
claims explicitly lack file:line, including `match_tiny_plain_string` at
`restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:78` and
`match_tiny_plain_string_with_cap::<16>` at
`restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:90`. P1-E's
main attribution table has symbol, percent, class, and source, but no per-row
samply profile path or sidecar/symbol path at
`restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:61`.

Required fold action: for every admitted hot-leaf row, add explicit evidence in
the cell or an adjacent evidence column: `/tmp/skv13-p1/samply/profiles/*.json.gz`,
the matching `.json.syms.json`, the resolved symbol path/name, % self-time, and
source file:line. Rows without file:line must be marked unresolved and cannot be
used as resolved hot-leaf claims.

### CH1-COR-004 - P1-A and P1-E disagree on parse hot leaves for load-bearing rows

The artifacts do not agree on several parse Track 1 attributions. P1-A reports
`runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>`
for `distinct_values` at
`restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:90`, while
P1-E reports `runtime::generated_json::generated::dispatch_value` for the same
corpus at
`restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:78`.
P1-A reports `parse_that_regex::read_hex_unit_scalar` for
`y_string_unicode` at
`restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:91`, while
P1-E reports `dispatch_value` at
`restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:79`.

Required fold action: rerun or publish the exact extractor used for top
self-time aggregation, then reconcile P1-A and P1-E from the same raw sidecars.
The fold must state whether attribution is leaf-only, inlined-frame, or
envelope attribution, because those produce materially different S-P2 inputs.

### CH1-COR-005 - P1-D does not satisfy the full requested PMU counter set

P1-D correctly reports real cycles and c/B, but the prompt asked for cycles,
instructions, branch-misses, L1 misses, and LLC misses. P1-D states that the
row file "does not expose branch misses, L1 misses, or LLC misses" at
`restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:77`, repeats the
field absence at
`restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:149`, and lists
it as anomaly 1 at
`restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:165`.

Required fold action: either export branch/L1/LLC counters from xctrace into a
tabular artifact and cite them per row, or explicitly narrow the PMU claim in
P1-D/P1-F to cycles/instructions/c/B/CPI only. Do not leave the frontmatter
scope implying complete PMU counter coverage.

### CH1-COR-006 - RESULTS hot-leaf placeholders are not fully resolved

`skinny/RESULTS.md` still contains Criterion slope placeholders in the Hot leaf
column, for example `json/twitter/parse_only/main` at `skinny/RESULTS.md:5`,
`json/twitter/direct_to_struct/main` at `skinny/RESULTS.md:6`, and
`json/twitter/real_typed_struct/main` at `skinny/RESULTS.md:7`. P1-F records
that those fields remain stale at
`restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:159`, while
P1-E resolves parse and seven typed rows but leaves 17 direct rows and 10 typed
rows unprofiled. This is an honest explanation, but not a CH1 resolution.

Required fold action: replace every `criterion-slope-profile` placeholder in
the JSON row surface with a resolved samply/xctrace hot leaf or a row-specific
unsupported/unprofiled disposition that names the failing artifact and next
capture action. The V2 CH1 package must make it mechanically clear which
`skinny/RESULTS.md` hot-leaf cells are resolved, unsupported, or still open.

## Required V2 Fold Gate

V2 remains CH1-REJECT until all of the following are true:

1. P1-C has real mode III coverage or explicit unsupported dispositions for all
   17 corpora and required probe surfaces.
2. Direct-to-struct samply captures profile the parser, not fixture panic paths,
   for all 17 corpora.
3. Every hot-leaf claim includes profile path, sidecar/symbol path, % self-time,
   and file:line; no "no file:line" hot leaf is treated as resolved.
4. P1-A and P1-E are reconciled from a single declared extraction method.
5. PMU counter scope is either complete for branch/L1/LLC or narrowed honestly
   across P1-D/P1-F.
6. Every `skinny/RESULTS.md` hot-leaf placeholder has a resolved symbol or a
   specific row-level unresolved reason with fold action.
