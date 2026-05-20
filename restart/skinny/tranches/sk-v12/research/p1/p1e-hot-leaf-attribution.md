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

This fold records final pin-era hot-leaf authority:

| Artifact | Coverage |
|---|---:|
| `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv` | 82 data rows |
| `/tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv` | 410 data rows |
| `/tmp/skv12-pin-p1/time_profile_parse_table.md` | 34 parse rows |
| `/tmp/skv12-pin-p1/time_profile_direct_table.md` | 34 direct rows |
| `/tmp/skv12-pin-p1/time_profile_typed_table.md` | 14 typed rows |

Validation: summary 82/82 and details 410/410 have no `:0`, `unknown`, or
`none` source anchors in the load-bearing symbol/source fields.

Top-family distribution, split by mode so Track 2/oracle work is not folded
into generated Track 1 antecedents:

| Plane/mode | Leading families |
|---|---|
| `parse/track1` | `bounded_plain_string_scan` 7; `container_dispatch` 7; `number_digit_span` 1; `simd_movemask` 1; `unicode_escape_hex_decode` 1 |
| `parse/track2` | `container_dispatch` 11; `bounded_plain_string_scan` 5; `unicode_escape_hex_decode` 1 |
| `direct/track1` | `output_digest_hash` 17 |
| `direct/track2` | `runtime_support` 14; `string_escape_decode` 2; `allocation_support` 1 |
| `typed/real_typed_track1` | `typed_direct_projection` 6; `string_full_scan` 1 |
| `typed/real_typed_track2` | `serde_json_oracle_read_parse` 7 |

Track 2 and oracle-only families are guard/comparator context. They are not
generated Track 1 optimization antecedents.

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
The parent owns capture. This artifact records the final hot-leaf evidence
boundary after the parent replay completed under `/tmp/skv12-pin-p1`.

The pre-pin P1-E artifact is a format reference only. None of its hot-leaf rows
are carried as fresh pin-era claims, because the user pin requires fresh JSON
plus CSS L4 profiling and forbids anchoring on prior tranche prose.

## Section 2 - Fresh Artifact Inventory

Observed final pin-root files at `/tmp/skv12-pin-p1`:

| Artifact class | Observed state | P1-E consequence |
|---|---|---|
| PMU status | 82/82 rows PASS in `/tmp/skv12-pin-p1/pmu/capture_status.tsv`. | Replay/cost companion; not self-time authority. |
| samply artifacts | 82/82 rows PASS in `/tmp/skv12-pin-p1/samply/capture_status.tsv`. | Retained companion artifacts, symbolized sidecars present. |
| xctrace capture | 212/212 rows PASS in `/tmp/skv12-pin-p1/xctrace/capture_status.tsv`. | Time Profiler and CPU Counter capture authority. |
| xctrace XML exports | 82 XML files present and nonzero; `/tmp/skv12-pin-p1/time_profile_export_status.tsv` records `SKIP` because exports already existed. | Valid self-time input; do not relabel status as PASS. |
| derived self-time summary | 82 rows in `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv`. | Hot-leaf summary authority. |
| derived self-time details | 410 rows in `/tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv`. | Raw top-leaf detail authority. |
| CSS L4 target row | No CSS L4/lightningcss artifacts under `/tmp/skv12-pin-p1`. | CSS L4 remains an S-P2/S-P3 bring-up prerequisite. |

No profiler process remains load-bearing for this artifact; the pin capture is
complete for JSON parse/direct/typed rows.

## Section 3 - Validation Checks

The final hot-leaf fold satisfies the JSON source-attribution prerequisites:

```sh
test -s /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv
test -s /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv

awk -F '\t' 'NR>1 {n++; if($16 ~ /:0([^0-9]|$)/ || $16 ~ /unknown/ || $15=="none") bad++}
  END{print n, bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv
# 82 0

awk -F '\t' 'NR>1 {n++; if($9 ~ /:0([^0-9]|$)/ || $9 ~ /unknown/ || $8=="none") bad++}
  END{print n, bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv
# 410 0
```

The admitted JSON hot-leaf tables are profile antecedents only. CSS L4 remains
unprofiled because the skinny runtime has no generated CSS L4 Track 1 parser,
lightningcss same-plane comparator, or strict equality oracle row.

## Section 4 - Derivation Method

The parent supplied Time Profiler traces, exported XML, and derived self-time in
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
tables. It does so for the JSON parse/direct/typed rows. CSS L4 is the one
legitimately absent lane and is recorded as a hard bring-up prerequisite.

## Section 5 - Hot-Leaf Claim Ledger

Fresh JSON hot-leaf claims are available in the pin root:

| Surface | Rows | Authority | Notes |
|---|---:|---|---|
| `parse/track1` | 17 | `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv` | Diagnostic only; not SOTA admission. |
| `parse/track2` | 17 | `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv` | Independent parse comparator/context. |
| `direct/track1` | 17 | `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv` | Generated JSON direct guard/diagnostic rows. |
| `direct/track2` | 17 | `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv` | Oracle/hand context; not generated Track 1 antecedent. |
| `typed/real_typed_track1` | 7 | `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv` | Generated JSON typed guard rows. |
| `typed/real_typed_track2` | 7 | `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv` | Serde/oracle context. |

| Non-JSON row | generated Track 1 hot leaf | comparator/oracle hot leaf |
|---|---|---|
| CSS L4 target corpus | unavailable: no fresh CSS L4 self-time artifact | unavailable: no fresh lightningcss/oracle self-time artifact |

## Section 6 - REDRESS And Pin Boundaries

This artifact proposes no intervention. It records fresh JSON self-time
evidence and the still-missing CSS L4 lane. The following route boundaries
remain load-bearing for any later S-P2/S-P3 fold:

| Boundary | Treatment |
|---|---|
| User pin D1/D2 | CSS L4 is authoritative; the close bar is generated CSS L4 Track 1 strictly greater than `lightningcss_mbps + 1`. |
| User pin D3/D4 | Union-substrate and ASM-gen categories are unblocked only for new material-differential implementations after CHALLENGE; prior REDRESS entries remain historical evidence. |
| User pin D5 | Zero orphan aarch64 primitives at close; P1-E must not hide orphan evidence behind family labels. |
| User pin D6 | Parse time and >SOTA are top priority, but parse-only remains diagnostic-only. |
| REDRESS 112/113 | Pre-pin generated non-JSON baseline failure is historical and superseded by the CSS L4 pin, but it still explains why CSS L4 evidence must be generated Track 1, not report-only. |
| REDRESS 114-120 | JSON direct residual rows and SK-V11 close remain guard/remainder evidence; a fresh pin-era route needs fresh profile plus material differential. |

## Section 7 - Delta vs SK-V11

The current `skinny/RESULTS.md` surface still records JSON parse/direct/typed
rows only. Pin-era JSON hot-leaf deltas are now computable from the fresh
self-time tables named above, but CSS L4 deltas remain unavailable until W1
creates a generated Track 1 CSS parser, lightningcss comparator, equality
oracle, and row telemetry.

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
- Tracked pin replay ledger:
  `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv`.
