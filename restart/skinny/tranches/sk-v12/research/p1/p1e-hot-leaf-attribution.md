# SK-V12 P1-E: Hot-Leaf Attribution

Pass: S-P1 Profile. Cycle: V12 pin reprofile.
Date: 2026-05-20.
Scope: synthesize hot-leaf attribution from fresh `/tmp/skv12-pin-p1`
self-time artifacts.
Output: this file.
Baseline: SK-V12 pin-open inspection at `cf7848b2`.
Host triple: `aarch64-apple-darwin`; Apple M5 Max.
Build flags: release profile, `RUSTFLAGS="-C target-cpu=native"`, target root
`/tmp/skv12-pin-profile-target-cf7848b2`.
Profile tool: xctrace Time Profiler XML derived tables under
`/tmp/skv12-pin-p1`; samply artifacts retained as companion evidence.
Corpus coverage: JSON hot-leaf coverage 17/17 parse, 17/17 direct, 7/7 typed
guard rows; CSS L4 0/0 because no generated skinny CSS L4 parser exists.

## Final Orchestrator Fold - 2026-05-20

This fold supersedes the partial-capture blocker ledger below. Final hot-leaf
authority:

| Artifact | Coverage |
|---|---:|
| `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv` | 82 data rows |
| `/tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv` | 410 data rows |
| `/tmp/skv12-pin-p1/time_profile_parse_table.md` | 34 parse rows |
| `/tmp/skv12-pin-p1/time_profile_direct_table.md` | 34 direct rows |
| `/tmp/skv12-pin-p1/time_profile_typed_table.md` | 14 typed rows |

Validation: summary 82/82 and details 410/410 have no `:0`, `unknown`, or
`none` source anchors in the load-bearing symbol/source fields.

Top-family distribution:

| Plane | Leading families |
|---|---|
| `parse` | `container_dispatch` 18 rows; `bounded_plain_string_scan` 12; `unicode_escape_hex_decode` 2; `number_digit_span` 1; `simd_movemask` 1 |
| `direct` | `output_digest_hash` 17 rows; `runtime_support` 14; `string_escape_decode` 2; `allocation_support` 1 |
| `typed` | `serde_json_oracle_read_parse` 7 rows; `typed_direct_projection` 6; `string_full_scan` 1 |

CSS L4 remains intentionally absent from the hot-leaf ledger: the skinny
runtime has no generated CSS L4 Track 1 parser or lightningcss comparator row
yet. That absence is the S-P1 boundary S-P2/S-P3 must consume; it is not a
fallback authorization to Sheets or BBNF-self.

## Section 1 - Method

Read-only inputs:

```sh
sed -n '1,240p' restart/prompts/skinny/PASS-1-PROFILE.md
sed -n '1,260p' restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md
sed -n '1,260p' restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md
sed -n '1,260p' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md
sed -n '1,220p' skinny/RESULTS.md
tail -n 260 skinny/REDRESS.md
find /tmp/skv12-pin-p1 -maxdepth 4 -type f
find /tmp/skv12-pin-p1 -maxdepth 5 -type d
sed -n '1,240p' /tmp/skv12-pin-p1/pmu/capture_status.tsv
sed -n '1,220p' /tmp/skv12-pin-p1/pmu/pmu-commands.sh
find /tmp/skv12-pin-p1 -maxdepth 4 -type f -name '*time*' -o -name '*summary*' -o -name '*details*'
pgrep -fl 'cargo|rustc|xctrace|samply|profile_direct|xctrace_probe'
```

P1-E did not run cargo, xctrace, samply, Criterion, or any benchmark binary.
The parent owns capture. This artifact records the hot-leaf evidence boundary
at P1-E inspection time and gives the patch-ready fold method for the next
cycle once parent self-time exports exist.

The pre-pin P1-E at baseline `50bd1648` and capture root `/tmp/skv12-p1` is a
format reference only. None of its hot-leaf rows are carried as fresh pin-era
claims, because the user pin requires fresh JSON plus CSS L4 profiling and
forbids anchoring on prior tranche prose.

## Section 2 - Fresh Artifact Inventory

Observed fresh pin-root files at `/tmp/skv12-pin-p1`:

| Artifact class | Observed state | P1-E consequence |
|---|---|---|
| PMU status | `/tmp/skv12-pin-p1/pmu/capture_status.tsv` existed with 26 direct rows plus header at inspection. | Useful for P1-D/cost only; not a hot-leaf source. |
| PMU commands | `/tmp/skv12-pin-p1/pmu/pmu-commands.sh` existed and enumerated JSON parse, direct, and typed PMU commands. | Replay surface only; not self-time attribution. |
| Direct PMU logs | `/tmp/skv12-pin-p1/logs/pmu-direct-*.rerun.log.{out,err}` existed for the observed rows. | No symbol, percent, or source line. |
| samply artifacts | No `/tmp/skv12-pin-p1/samply/` artifacts observed. | Hot-leaf claims unavailable. |
| xctrace Time Profiler traces | No `/tmp/skv12-pin-p1/*time-profiler*` traces observed. | Hot-leaf claims unavailable. |
| xctrace Time Profiler XML exports | No `*.time-profile.xml` exports observed. | Hot-leaf claims unavailable. |
| derived self-time summary | No `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv` observed. | Hot-leaf claims unavailable. |
| derived self-time details | No `/tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv` observed. | Hot-leaf claims unavailable. |
| CSS L4 target row | No CSS L4/lightningcss artifacts observed under `/tmp/skv12-pin-p1`. | P1-E cannot satisfy the pin-era grammar-generalization hot-leaf requirement. |

Process reconciliation at inspection found an active parent-owned
`profile_direct` process for a JSON direct PMU row. P1-E therefore treats the
pin capture as in progress, not failed.

## Section 3 - Blocker List

P1-E cannot make any fresh hot-leaf attribution until the parent capture lands
these artifacts:

1. `time_profile_hot_leaf_details.tsv` with one row per selected top leaf and
   columns equivalent to: `plane`, `corpus`, `mode`, `rank`, `symbol`,
   `percent_self`, `source`, `family`, `artifact`.
2. `time_profile_hot_leaf_summary.tsv` with one summary row per corpus x
   workload x track and columns equivalent to: `plane`, `corpus`, `mode`,
   `top_leaf`, `top_leaf_percent`, `top_leaf_source`, `family`.
3. Fresh xctrace Time Profiler XML exports or an equivalent symbolized
   self-time source for all 17 JSON corpora across parse/direct/typed lanes.
4. A CSS L4 target corpus self-time lane with the same symbol, percent, and
   source-line contract as the JSON rows.
5. A manifest update recording host triple, tool versions, build flags, run
   ids, and any accepted xctrace return-code policy for the pin-root capture.
6. Source-line normalization proof: no `:0` source anchors, no unresolved
   frames, and no `UNRESOLVED_LINE_ZERO` markers in either derived table.

Until those exist, every hot-leaf cell below is explicitly unavailable. No
S-P2 primitive or S-P3 wave should cite this P1-E as hot-leaf antecedent for a
kernel, union route, ASM-gen route, JSON guard route, or CSS L4 admission
route.

## Section 4 - Patch-Ready Fold Method

Once the parent supplies Time Profiler traces, export and derive self-time in
this shape:

```sh
find /tmp/skv12-pin-p1 -path '*time-profiler*' -name '*.trace' -print

mkdir -p /tmp/skv12-pin-p1/time-profiler-exports

xctrace export \
  --input /tmp/skv12-pin-p1/<lane>/time-profiler/<row>.trace \
  --xpath '/trace-toc/run[@number="1"]/data/table[@schema="time-profile"]' \
  > /tmp/skv12-pin-p1/time-profiler-exports/<row>.time-profile.xml
```

The derivation step must:

1. Filter startup/dyld/system frames from the selected target-binary running
   samples, but keep the retained denominator so coverage is auditable.
2. Select top leaves per corpus x workload x track, preserving raw symbol
   names, percent self-time, artifact path, and resolved file:line.
3. Reject any row whose top leaf lacks a concrete source anchor.
4. Map symbols to grammar-neutral families only after the raw symbol table is
   retained. Family labels do not replace symbol evidence.
5. Include CSS L4 as a first-class row. A JSON-only fold is not pin-converged.
6. Preserve Track 1 / Track 2 separation. Comparator/oracle symbols never
   prove generated Track 1 hot leaves.
7. Mark parse-only rows diagnostic-only; they are not admission rows under the
   user pin.

Acceptance checks for the fold:

```sh
test -s /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv
test -s /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv
rg -n ':0|UNRESOLVED_LINE_ZERO|unavailable|n/a' \
  /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv \
  /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv
```

The final `rg` must return no unresolved source anchors in admitted hot-leaf
tables. If a lane is legitimately absent, document it in this file as an
explicit blocker rather than leaving a silent blank.

## Section 5 - Hot-Leaf Claim Ledger

No fresh hot-leaf claims are available in the pin root. Each row is marked
unavailable because P1-E found no fresh self-time artifact with all three
required fields: symbol, percent self-time, and file:line.

| Corpus | parse hot leaf | direct hot leaf | typed hot leaf |
|---|---|---|---|
| `twitter` | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact |
| `citm_catalog` | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact |
| `canada` | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact | unavailable: no typed row in RESULT surface |
| `apache_builds` | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact |
| `github_events` | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact |
| `update_center` | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact |
| `mesh` | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact |
| `random` | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact | unavailable: no typed row in RESULT surface |
| `gsoc-2018` | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact | unavailable: no typed row in RESULT surface |
| `marine_ik` | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact |
| `instruments` | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact | unavailable: no typed row in RESULT surface |
| `numbers` | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact | unavailable: no typed row in RESULT surface |
| `unicode_mixed` | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact | unavailable: no typed row in RESULT surface |
| `unicode_escapes` | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact | unavailable: no typed row in RESULT surface |
| `unicode_basic` | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact | unavailable: no typed row in RESULT surface |
| `distinct_values` | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact | unavailable: no typed row in RESULT surface |
| `y_string_unicode` | unavailable: no fresh self-time artifact | unavailable: no fresh self-time artifact | unavailable: no typed row in RESULT surface |

| Non-JSON row | generated Track 1 hot leaf | comparator/oracle hot leaf |
|---|---|---|
| CSS L4 target corpus | unavailable: no fresh CSS L4 self-time artifact | unavailable: no fresh lightningcss/oracle self-time artifact |

## Section 6 - REDRESS And Pin Boundaries

This artifact proposes no intervention. It only records that P1-E lacks fresh
self-time evidence. The following route boundaries remain load-bearing for any
later P1-E fold:

| Boundary | Treatment |
|---|---|
| User pin D1/D2 | CSS L4 is authoritative; the close bar is generated CSS L4 Track 1 strictly greater than `lightningcss_mbps + 1`. |
| User pin D3/D4 | Union-substrate and ASM-gen categories are unblocked only for new material-differential implementations after CHALLENGE; prior REDRESS entries remain historical evidence. |
| User pin D5 | Zero orphan aarch64 primitives at close; P1-E must not hide orphan evidence behind family labels. |
| User pin D6 | Parse time and >SOTA are top priority, but parse-only remains diagnostic-only. |
| REDRESS 112/113 | Pre-pin generated non-JSON baseline failure is historical and superseded by the CSS L4 pin, but it still explains why CSS L4 evidence must be generated Track 1, not report-only. |
| REDRESS 114-120 | JSON direct residual rows and SK-V11 close remain guard/remainder evidence; a fresh pin-era route needs fresh profile plus material differential. |

## Section 7 - Delta vs SK-V11

Unavailable for hot-leaf purposes in this P1-E cycle. The current
`skinny/RESULTS.md` surface still records JSON parse/direct/typed rows, but
the pin-era P1-E delta cannot be computed from PMU logs alone. It requires the
fresh self-time tables named in Section 3 plus the CSS L4 target lane.

## Section 8 - Sources

- S-P1 profile contract: `restart/prompts/skinny/PASS-1-PROFILE.md`.
- User pin: `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`.
- Pre-pin P1-E format reference:
  `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md`
  as tracked before this pin-aware rewrite.
- Pre-pin capture manifest format reference:
  `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`.
- Result authority: `skinny/RESULTS.md`.
- REDRESS ledger through SK-V11 close and SK-V12 pin context:
  `skinny/REDRESS.md`.
- Fresh pin-root PMU status:
  `/tmp/skv12-pin-p1/pmu/capture_status.tsv`.
- Fresh pin-root PMU command ledger:
  `/tmp/skv12-pin-p1/pmu/pmu-commands.sh`.
