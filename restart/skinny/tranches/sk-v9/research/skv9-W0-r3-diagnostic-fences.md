# SK-V9 W0 Research R3: Diagnostic Probe / Non-Producer Fences

Wave: W0 telemetry-lock.
Role: research schema, diagnostic probe/non-producer fences.
Date: 2026-05-18.
Output: `restart/skinny/tranches/sk-v9/research/skv9-W0-r3-diagnostic-fences.md`.
Edit boundary: this file only.

## Verdict

W0 may carry structural-scan-only evidence, masking probes, and
`cycles_per_byte` telemetry only as diagnostic non-producer sections. They are
not Track 1 producers, Track 2 producers, comparator sidecars, direct product
proof, Apache/CITM measured-row evidence, retained cursor state, or parser-owned
fact slots.

The hardening input is explicit: CH5 says structural-scan, masking-probe, and
cycles-per-byte surfaces are not fenced hard enough and require Lock 1 /
non-producer metadata (`restart/skinny/tranches/sk-v9/research/p1/hardening/V1/CH5.md:14`-`restart/skinny/tranches/sk-v9/research/p1/hardening/V1/CH5.md:18`).
CH6 says W0 is mandatory before behavior waves and cannot move parser/scanner/
SIMD/codegen behavior, throughput cells, or measured-row admission by itself
(`restart/skinny/tranches/sk-v9/research/p1/hardening/V1/CH6.md:98`-`restart/skinny/tranches/sk-v9/research/p1/hardening/V1/CH6.md:108`).

## Evidence Read

Requested inputs read:

| Input | Use |
|---|---|
| `restart/skinny/tranches/sk-v9/research/p1/hardening/V1/CH5.md` | Hidden-coupling defects and required diagnostic fences. |
| `restart/skinny/tranches/sk-v9/research/p1/hardening/V1/CH6.md` | Anti-paper-close requirement that W0 precede behavior waves. |
| `skinny/crates/bbnf-bench/src/report.rs` | `ProbeReportRow` has only corpus/probe/Mbps/ns/ratio/signal today, and rendering emits only those fields (`skinny/crates/bbnf-bench/src/report.rs:94`-`skinny/crates/bbnf-bench/src/report.rs:100`, `skinny/crates/bbnf-bench/src/report.rs:612`-`skinny/crates/bbnf-bench/src/report.rs:625`). |
| `skinny/crates/bbnf-bench/src/gate.rs` | Strict admission/schema checks and `TrackTag::SimdScan` schema handling. |
| `skinny/crates/bbnf-bench/src/bin/gate.rs` | `push_probe_rows` populates volatile probe rows; SIMD metadata validation recognizes `TrackTag::SimdScan`, `cycles_per_byte`, `structural_offsets`, and `offset bitmap` (`skinny/crates/bbnf-bench/src/bin/gate.rs:1412`-`skinny/crates/bbnf-bench/src/bin/gate.rs:1419`, `skinny/crates/bbnf-bench/src/bin/gate.rs:1500`-`skinny/crates/bbnf-bench/src/bin/gate.rs:1540`). |
| `skinny/crates/bbnf-bench/src/probes.rs` | Six configured masking probes and default thresholds (`skinny/crates/bbnf-bench/src/probes.rs:30`-`skinny/crates/bbnf-bench/src/probes.rs:57`). |
| `skinny/crates/bbnf-bench/benches/json_parity.rs` | Probe bodies: dispatch overhead, eager decode, serde scalar plan, x86 PEXT scalar structural scan, and cold first parse (`skinny/crates/bbnf-bench/benches/json_parity.rs:394`-`skinny/crates/bbnf-bench/benches/json_parity.rs:431`). |
| `skinny/crates/bbnf-bench/benches/simd_scan.rs` | Structural scan computes scalar/SIMD offsets, hashes parity, and benchmarks scalar/SIMD scans (`skinny/crates/bbnf-bench/benches/simd_scan.rs:16`-`skinny/crates/bbnf-bench/benches/simd_scan.rs:37`). |

## Producer Class Taxonomy

| Class | May render metrics | May populate Track 1/Track 2 columns | May feed strict admission | May define substrate/cursor/ValueRef | Allowed consumer |
|---|---|---|---|---|---|
| `main_row_producer` | yes | yes | only with normal strict gates | only through accepted row contract | main report/gate |
| `diagnostic_nonproducer` | yes, diagnostic section only | no | no | no | `gate-json` diagnostics and S-P1 gap ledger |
| `probe_nonproducer` | yes, diagnostic section only | no | no | no | `gate-json` diagnostics and S-P2 signal ledger |
| `source_product_only` | source/product proof only | no measured row by itself | no | no | REDRESS/source parity ledger |

R3 owns only the two non-producer classes. Any diagnostic row that attempts to
become `main_row_producer` without a later accepted wave is a W0 rejection.

## Canonical Diagnostic Row Schema

Every W0 diagnostic row must carry this metadata before rendering:

```text
schema_id = "skv9-w0-diagnostic-v1"
row_scope = "diagnostic"
run_id = "<SK-V9-open run id or absent:<reason>>"
corpus = "<fixture name>"
input_sha256 = "<same-run fixture sha256>"
input_bytes = "<same-run byte count>"
surface = "structural_scan_only" | "cycles_per_byte" | "masking_probe"
producer_class = "diagnostic_nonproducer" | "probe_nonproducer"
track_role = "none"
track1_column_allowed = false
track2_column_allowed = false
strict_admission = false
substrate_output = "none"
substrate_relation = "none" | "Lock1 observation"
observed_output_plane = "none" | "diagnostic_offset_bitmap"
observed_materialisation = "none" | "structural_offsets" | "<probe body>"
may_feed_row_admission = false
may_feed_tape_or_cursor = false
may_define_value_ref_contract = false
may_replace_hot_leaf = false
may_feed_direct_product_proof = false
may_feed_apache_citm_measured_rows = false
same_wave_consumer = "gate-json-diagnostics"
evidence_artifact = "<criterion/samply/perf artifact path or absent:<reason>>"
metric_status = "measured" | "absent:<reason>" | "invalid:<reason>"
```

Metric fields are optional by surface:

| Field | Structural scan | `cycles_per_byte` | Masking probe |
|---|---|---|---|
| `mbps` | allowed | allowed if derived from same run | allowed |
| `ns_per_iter` | allowed | allowed | allowed |
| `vs_track1_ratio` | forbidden | forbidden | allowed as comparison metadata only |
| `cycles` | optional PMU field | required for measured c/B | optional PMU field |
| `instructions` | optional PMU field | optional | optional |
| `branch_misses` | optional PMU field | optional | optional |
| `l1_misses` | optional PMU field | optional | optional |
| `llc_misses` | optional PMU field | optional | optional |
| `cycles_per_byte` | forbidden unless PMU cycles and input bytes are same-run | `cycles / input_bytes` only | allowed only if same-run PMU cycles and input bytes exist |

`vs_track1_ratio` is not a Track 1 output. It is a diagnostic comparison against
the current Track 1 timing cell and cannot populate Track 1 Mbps, Track 2 Mbps,
delta columns, row verdicts, hot-leaf replacement cells, or strict admission.

## Surface Rules

### Structural-Scan-Only

Structural scan is a second source scan over fixture bytes. The bench computes
scalar offsets, SIMD offsets, hashes both, asserts equality, and then benchmarks
scalar and SIMD offset collection (`skinny/crates/bbnf-bench/benches/simd_scan.rs:16`-`skinny/crates/bbnf-bench/benches/simd_scan.rs:37`).

Required R3 fence:

```text
surface = "structural_scan_only"
producer_class = "diagnostic_nonproducer"
substrate_relation = "Lock1 observation"
observed_materialisation = "structural_offsets"
observed_output_plane = "diagnostic_offset_bitmap"
substrate_output = "none"
track_role = "none"
strict_admission = false
may_feed_row_admission = false
may_feed_tape_or_cursor = false
may_define_value_ref_contract = false
```

The offset vector or bitmap may prove scalar/SIMD parity for diagnostics. It
must not become a retained cursor, parser-owned event vector, tape fact, public
substrate API, row output, direct digest proof, or Track 1/Track 2 surrogate.

### `cycles_per_byte`

Current metadata names the SIMD scan row as `TrackTag::SimdScan` with
`workload="cycles_per_byte"`, `materialisation="structural_offsets"`, and
`output_plane="offset bitmap"` (`skinny/crates/bbnf-bench/src/metadata.rs:267`-`skinny/crates/bbnf-bench/src/metadata.rs:280`).
The gate validates the same shape (`skinny/crates/bbnf-bench/src/bin/gate.rs:1412`-`skinny/crates/bbnf-bench/src/bin/gate.rs:1419`).

Required R3 fence:

```text
surface = "cycles_per_byte"
producer_class = "diagnostic_nonproducer"
track_role = "none"
strict_admission = false
substrate_relation = "Lock1 observation"
observed_materialisation = "structural_offsets"
observed_output_plane = "diagnostic_offset_bitmap"
cycles_per_byte = cycles / input_bytes only when both are same-run
```

The string `workload="cycles_per_byte"` must not make the row an admitted main
workload. W0 should render it under a diagnostic manifest section or reject it
if it is promoted into the main result table as Track 1, Track 2, product proof,
strict admission, Apache/CITM row evidence, or primitive ancestry.

No c/B may be inferred from ns/B, Mbps, CPU model, frequency, Criterion sample
cost, or wall time. CH6 calls that exact inference a paper-close
(`restart/skinny/tranches/sk-v9/research/p1/hardening/V1/CH6.md:50`-`restart/skinny/tranches/sk-v9/research/p1/hardening/V1/CH6.md:59`).

### Masking Probes

The configured masking matrix is:

| Probe | Source body | Producer class | Required fence |
|---|---|---|---|
| `host_call_dispatch_overhead` | Function pointer call over `&str` (`skinny/crates/bbnf-bench/benches/json_parity.rs:394`-`skinny/crates/bbnf-bench/benches/json_parity.rs:397`) | `probe_nonproducer` | no track role, no substrate output |
| `host_call_eager_decode` | Parse generated Track 1 root, then walk keys/strings through the view (`skinny/crates/bbnf-bench/benches/json_parity.rs:399`-`skinny/crates/bbnf-bench/benches/json_parity.rs:405`, `skinny/crates/bbnf-bench/benches/json_parity.rs:440`-`skinny/crates/bbnf-bench/benches/json_parity.rs:455`) | `probe_nonproducer` | diagnostic signal only; not a Track 1 alternate producer |
| `alternate_scalar_plan` | External `serde_json::Value` parse (`skinny/crates/bbnf-bench/benches/json_parity.rs:407`-`skinny/crates/bbnf-bench/benches/json_parity.rs:412`) | `probe_nonproducer` | external comparison only; no local hot-leaf replacement |
| `alternate_dispatch_table_plan` | Configured probe, but gate marks duplicate disabled (`skinny/crates/bbnf-bench/src/bin/gate.rs:1516`-`skinny/crates/bbnf-bench/src/bin/gate.rs:1524`) | `probe_nonproducer` | `metric_status=invalid:duplicate-probe-disabled` |
| `alternate_pext_mask_plan` | x86/x86_64 scalar structural offsets (`skinny/crates/bbnf-bench/benches/json_parity.rs:414`-`skinny/crates/bbnf-bench/benches/json_parity.rs:420`) | `probe_nonproducer` | host-feature diagnostic only |
| `cold_first_parse` | Clone bytes, UTF-8 view, generated parse (`skinny/crates/bbnf-bench/benches/json_parity.rs:422`-`skinny/crates/bbnf-bench/benches/json_parity.rs:431`) | `probe_nonproducer` | cold-sensitivity signal only |

Required R3 fence for every masking probe:

```text
surface = "masking_probe"
producer_class = "probe_nonproducer"
track_role = "none"
substrate_output = "none"
strict_admission = false
may_feed_row_admission = false
may_feed_tape_or_cursor = false
may_define_value_ref_contract = false
may_replace_hot_leaf = false
```

The current report renderer does not carry this metadata: `ProbeReportRow`
contains only corpus, probe, Mbps, ns/iter, Track 1 ratio, and signal
(`skinny/crates/bbnf-bench/src/report.rs:94`-`skinny/crates/bbnf-bench/src/report.rs:100`), and the `## Masking Probes` table renders only those fields
(`skinny/crates/bbnf-bench/src/report.rs:612`-`skinny/crates/bbnf-bench/src/report.rs:625`).
Therefore W0 should either add non-producer metadata before rendering or keep
the probe section absent with explicit `absent:<reason>` cells.

## Report Shape

If W0 renders diagnostics in `skinny/RESULTS.md` or a gate-consumed side
manifest, the diagnostic section must be visually and structurally separate from
the main schema-v3 result rows.

Recommended structural-scan / c/B table:

| Corpus | Surface | Producer class | Track role | Substrate relation | Observed materialisation | Observed output plane | PMU fields | c/B rule | Strict admission | Signal |
|---|---|---|---|---|---|---|---|---|---|---|
| `<corpus>` | `structural_scan_only` | `diagnostic_nonproducer` | `none` | `Lock1 observation` | `structural_offsets` | `diagnostic_offset_bitmap` | optional | no c/B unless PMU cycles exist | `false` | parity/floor signal |
| `<corpus>` | `cycles_per_byte` | `diagnostic_nonproducer` | `none` | `Lock1 observation` | `structural_offsets` | `diagnostic_offset_bitmap` | required for measured c/B | `cycles / input_bytes` | `false` | PMU/c/B signal |

Recommended masking-probe table:

| Corpus | Probe | Producer class | Track role | Substrate output | Mbps | ns/iter | vs Track 1 | Strict admission | Signal |
|---|---|---|---|---|---:|---:|---:|---|---|
| `<corpus>` | `<probe>` | `probe_nonproducer` | `none` | `none` | `<value or absent>` | `<value or absent>` | `<diagnostic ratio or n/a>` | `false` | `<signal>` |

The words "Track 1" may appear only in `vs Track 1` comparison metadata for
probe rows. They must not create a Track 1 result, Track 1 producer, or Track 1
hot-leaf replacement.

## Gate Refusals

W0 `gate-json` should reject or mark invalid any diagnostic row that:

1. Uses `producer_class=diagnostic_nonproducer` or `probe_nonproducer` while
   setting `track_role` to Track 1, Track 2, or competitor.
2. Places diagnostic Mbps, ns/iter, ratio, PMU, or c/B values into main Track 1
   or Track 2 Mbps columns.
3. Sets `strict_admission=true` or uses a diagnostic row to satisfy strict
   comparator admission.
4. Uses `structural_offsets`, `offset bitmap`, scalar parity hashes, or PEXT
   probe offsets as a retained cursor, tape output, parser fact slot, ValueRef
   contract, DirectBuild proof, or sidecar substrate.
5. Treats `alternate_scalar_plan` as Track 2, a same-run sidecar producer, or a
   local hot leaf. It is an external serde diagnostic probe.
6. Treats `host_call_eager_decode` as an alternate Track 1 producer because it
   parses Track 1 before walking the view. It is a masking signal only.
7. Computes c/B without same-run PMU cycles and same-run input bytes.
8. Allows diagnostic rows to admit Apache/CITM measured typed rows, Canada typed
   rows, direct digest rows, or structural parse implementation routes.

## Acceptance Criteria For W0 R3

R3 is satisfied when W0 has all of the following:

1. A diagnostic section schema carrying `producer_class`, `track_role`,
   `strict_admission`, `substrate_output`, `substrate_relation`, and all
   `may_feed_*` fence fields.
2. Structural-scan-only and `cycles_per_byte` rows labeled
   `diagnostic_nonproducer` with `substrate_relation=Lock1 observation`.
3. Masking probe rows labeled `probe_nonproducer` with `track_role=none`,
   `substrate_output=none`, and `strict_admission=false`.
4. c/B values derived only from same-run `cycles / input_bytes`, with missing
   PMU fields rendered as `absent:<reason>`.
5. No diagnostic value populating Track 1/Track 2 columns, row verdicts,
   strict admission, hot-leaf replacement cells, product proof, retained cursor
   state, or parser-owned facts.
6. Same-wave consumer limited to `gate-json-diagnostics`; no parser/scanner/
   SIMD/codegen behavior movement in W0.

Until those fields exist, diagnostic probes should remain absent from rendered
SK-V9-open results or be rendered only with explicit absence/invalid reasons.

## Routed Remainder

This R3 schema does not authorize behavior work. After W0, S-P1 still needs a
fresh profile rerun with resolved symbols, self-time percentages, source
file:line, profile artifacts, run id, and same-run PMU/c/B where claimed. CH6
requires S-P2 to reject primitive candidates whose antecedent is `absent:*`,
Criterion-slope-only, source-eligible-only, sidecar-historical-only, or stale
profile evidence (`restart/skinny/tranches/sk-v9/research/p1/hardening/V1/CH6.md:119`-`restart/skinny/tranches/sk-v9/research/p1/hardening/V1/CH6.md:124`).
