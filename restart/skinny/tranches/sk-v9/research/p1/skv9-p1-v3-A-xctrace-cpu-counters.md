# SK-V9 P1-V3-A: xctrace CPU Counters PMU Capture

Pass: S-P1 Profile. Cycle: V3 reframe.
Date: 2026-05-18.
Scope: real PMU counters (cycles, instructions retired, CPI, cycles/byte) for
all 17 JSON corpora × {Track 1 generated, Track 2 hand-coded} parse-only
surfaces, captured with `xcrun xctrace record --template "CPU Counters"`
wrapping a launchable per-corpus probe binary and supplemented by an
in-process `proc_pid_rusage(RUSAGE_INFO_V5)` PMU readout.
Output: this file.
Baseline: SK-V9-open at commit `90609aee`, run
`sk-v9-open:criterion-fnv64-cd1673844eeea12f`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max` (18 cores,
12 P + 6 E).
Build flags: `RUSTFLAGS=-C target-cpu=native`, release profile with
`debug=true`, default codegen-units, mimalloc allocator (per `bbnf-bench`
crate global allocator).
Profile tool: `xcrun xctrace version 26.0 (17A5241e)`, Xcode 26.0 active
developer dir; `proc_pid_rusage(RUSAGE_INFO_V5)` via Apple public
`libproc.h` (kpc-backed, no sudo).
Corpus coverage: 17/17 for both tracks.

## §1 - Capture methodology

### 1.1 Probe binary

The PMU capture target is a purpose-built launchable harness committed to
the workspace at
`skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs`. The binary takes
`<corpus_path> <track:track1|track2> <iters>` on its command line, reads
the corpus into a `&str` exactly once, then enters a black-boxed tight
loop calling either `runtime::generated_json::parse` (Track 1, generated)
or `bbnf_bench::track2::json::parse` (Track 2, hand-coded). The return
value's tape offset count is folded into a checksum that the optimiser
cannot lift the parser out of. The probe reads
`proc_pid_rusage(getpid(), RUSAGE_INFO_V5, &ri)` immediately before and
after the steady-state loop and prints `cycles = ri_after.ri_cycles -
ri_before.ri_cycles` and analogous instructions on stdout, plus elapsed
ns and the Mbps derived from `bytes × iters`.

`rusage_info_v5` is Apple's documented public rusage flavour (`<sys/resource.h>`,
`__MAC_10_5+`). `ri_cycles` and `ri_instructions` are populated from the
kernel's kpc framework — they are real PMU reads, not estimates from
`mach_absolute_time`. The probe carries one warm sanity-parse before the
loop to surface any parse error, but the PMU counters are read after
that, so the sanity parse is excluded from the delta.

**Corpus-name canonical mapping.** The corpus file on disk is
`skinny/test_data/update-center.json` (hyphenated, as shipped by the
upstream jsonexamples corpus). The `RESULTS.md` row name is
`update_center` (underscored, the canonical RESULTS schema spelling).
This report's PMU TSV (`/tmp/skv9-xctrace-v3/pmu_rows.tsv`) and trace
bundle dir (`/tmp/skv9-xctrace-v3/p1a/`) follow the *file-on-disk*
spelling `update-center`; sibling P1-V3-B's Time Profiler exports follow
the *RESULTS row* spelling `update_center`. Both refer to the same
corpus. Downstream aggregators that join PMU rows with TP symbol
exports on the corpus name must normalise hyphen ↔ underscore for the
`update_center` ↔ `update-center.json` row — no other corpus exhibits
this shear.

### 1.2 Per-row capture invocation

For each (corpus, track) pair the capture script ran:

```bash
xcrun xctrace record \
  --template "CPU Counters" \
  --no-prompt \
  --output /tmp/skv9-xctrace-v3/p1a/<corpus>__<track>.trace \
  --launch -- \
  /Users/mkbabb/Programming/bbnf-lang/skinny/target/release/xctrace_probe \
  /Users/mkbabb/Programming/bbnf-lang/skinny/test_data/<corpus>.json \
  <track> <iters>
```

Iteration counts were tuned so each capture occupied a 0.5 - 3 s
steady-state window, dwarfing process startup (~10 ms) and the single
sanity parse so the PMU sample reflects the parser inner loop:

| Corpus | Iters | Corpus | Iters | Corpus | Iters |
|---|---:|---|---:|---|---:|
| twitter | 4000 | apache_builds | 8000 | mesh | 2000 |
| citm_catalog | 2000 | github_events | 8000 | random | 2000 |
| canada | 2000 | y_string_unicode | 12000 | gsoc-2018 | 1000 |
| update-center | 2000 | unicode_basic | 2000 | marine_ik | 1000 |
| instruments | 8000 | unicode_mixed | 2000 | numbers | 8000 |
| distinct_values | 6000 | unicode_escapes | 2000 |   |   |

### 1.3 What the xctrace CPU Counters capture provides

The `.trace` directory artefact is produced for every row and stored
under `/tmp/skv9-xctrace-v3/p1a/<corpus>__<track>.trace`. This artefact
is Instruments.app-loadable for GUI inspection of the per-PMC event
counts. **However** — and this is the structural reason P1-D V2 was
blocked — Apple's `xcrun xctrace export` does NOT expose a public
schema that surfaces PMU counter rows from the CPU Counters template
on macOS 26.0 / Xcode 26.0 / Apple Silicon. The exportable schemas
under that template are: `tick`, `cpu-state`, `cpu-narrative`,
`thread-state`, `thread-narrative`, `time-profile`, `time-sample`,
`context-switch`, `syscall`, `dyld-library-load`, plus various
`kdebug` codes — none of which contains `cycles` or `instructions`
columns. The PMC bulkstore data is captured into the `.trace` directory
but its schema is closed to xctrace's CLI exporter.

This is *not* a regression and *not* sudo-blockable; it is a documented
Apple toolchain limitation. The cycles + instructions data therefore
comes from the in-process `rusage_info_v5` read, which Apple's
`proc_pid_rusage` populates from the same kpc kernel facility the
"CPU Counters" template internally consumes. The two sources are
equivalent for steady-state inner-loop measurements; only the
xctrace-side per-symbol PMC attribution is unavailable to the CLI.

For per-symbol attribution we keep two cross-validation paths:

1. The V2 samply profiles at `/tmp/skv9-p1-rerun/profiles/p1a/` (the
   existing P1-A samply mode-I capture, 17/17 corpora, ~95-99% of
   self-time on `runtime::generated_json::generated::dispatch_value`).
2. Time Profiler trace artefacts captured by sibling P1-V3-B under
   `/tmp/skv9-xctrace-v3/p1b-tp/<corpus>__<track>.trace`, which DO
   expose the `time-sample` schema with PC + backtrace. (P1-V3-B is
   the authoritative producer of those bundles; earlier drafts of
   this section referred to a co-located `p1a-time-profile/` path that
   was never populated. The canonical TP root for V3 is the P1-V3-B
   `p1b-tp/` tree.)

### 1.4 Reproducibility

The capture script is committed to `/tmp/skv9-xctrace-v3/capture.sh`
(reproduced verbatim in §5). It is deterministic given a fixed probe
binary and corpus set. Iteration counts are encoded in the script.

The probe binary is rebuilt with:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny && \
  RUSTFLAGS="-C target-cpu=native" \
  cargo build --release -p bbnf-bench --bin xctrace_probe
```

No production parser code was modified; the probe adds one Cargo bin
target on the existing `bbnf-bench` crate.

## §2 - Per-row PMU table

The columns are: `corpus`, `track`, `bytes` (corpus file size on disk),
`cycles` (Δ `ri_cycles` across the loop, real PMU), `instructions`
(Δ `ri_instructions`, real PMU), `CPI` = cycles ÷ instructions,
`cycles/B` = cycles ÷ (bytes × iters), `iters` (loop count chosen
for steady state).

<!-- PMU_TABLE_BEGIN -->
| corpus | track | bytes | iters | cycles | instructions | CPI | cycles/B |
|---|---|---:|---:|---:|---:|---:|---:|
| twitter | track1 | 631515 | 4000 | 5995321573 | 28452125502 | 0.211 | 2.373 |
| twitter | track2 | 631515 | 4000 | 7509192523 | 27448756725 | 0.274 | 2.973 |
| citm_catalog | track1 | 1727204 | 2000 | 4075618079 | 26508026902 | 0.154 | 1.180 |
| citm_catalog | track2 | 1727204 | 2000 | 5884212495 | 27506092417 | 0.214 | 1.703 |
| canada | track1 | 2251051 | 2000 | 9465887819 | 74758331429 | 0.127 | 2.103 |
| canada | track2 | 2251051 | 2000 | 9520359067 | 74074217088 | 0.129 | 2.115 |
| apache_builds | track1 | 127275 | 8000 | 2962681069 | 13011688155 | 0.228 | 2.910 |
| apache_builds | track2 | 127275 | 8000 | 2914475390 | 12029405820 | 0.242 | 2.862 |
| github_events | track1 | 65132 | 8000 | 1184041417 | 5698345758 | 0.208 | 2.272 |
| github_events | track2 | 65132 | 8000 | 1481794690 | 5468859255 | 0.271 | 2.844 |
| update-center | track1 | 533178 | 2000 | 3861982404 | 15547130667 | 0.248 | 3.622 |
| update-center | track2 | 533178 | 2000 | 4044073583 | 14787739793 | 0.273 | 3.792 |
| mesh | track1 | 723597 | 2000 | 3889319420 | 28735914175 | 0.135 | 2.687 |
| mesh | track2 | 723597 | 2000 | 4090676245 | 28960341543 | 0.141 | 2.827 |
| random | track1 | 510476 | 2000 | 3625540895 | 19309887977 | 0.188 | 3.551 |
| random | track2 | 510476 | 2000 | 4542125573 | 18831912925 | 0.241 | 4.449 |
| gsoc-2018 | track1 | 3327831 | 1000 | 5137237716 | 22567530034 | 0.228 | 1.544 |
| gsoc-2018 | track2 | 3327831 | 1000 | 5344133215 | 21784589810 | 0.245 | 1.606 |
| marine_ik | track1 | 2983466 | 1000 | 8034844121 | 54681497783 | 0.147 | 2.693 |
| marine_ik | track2 | 2983466 | 1000 | 8459648615 | 54736864978 | 0.155 | 2.836 |
| instruments | track1 | 220346 | 8000 | 3647568987 | 22408932230 | 0.163 | 2.069 |
| instruments | track2 | 220346 | 8000 | 5235135564 | 22375546393 | 0.234 | 2.970 |
| numbers | track1 | 150124 | 8000 | 2592178625 | 15179506412 | 0.171 | 2.158 |
| numbers | track2 | 150124 | 8000 | 2231139754 | 15339654734 | 0.145 | 1.858 |
| unicode_mixed | track1 | 1053086 | 2000 | 9759395821 | 25055384281 | 0.390 | 4.634 |
| unicode_mixed | track2 | 1053086 | 2000 | 8839455896 | 24308184130 | 0.364 | 4.197 |
| unicode_escapes | track1 | 1050797 | 2000 | 6319207033 | 27298188806 | 0.231 | 3.007 |
| unicode_escapes | track2 | 1050797 | 2000 | 6193791823 | 27242865417 | 0.227 | 2.947 |
| unicode_basic | track1 | 1048586 | 2000 | 6092872350 | 30997853856 | 0.197 | 2.905 |
| unicode_basic | track2 | 1048586 | 2000 | 6756755823 | 30957622055 | 0.218 | 3.222 |
| distinct_values | track1 | 153630 | 6000 | 3548937694 | 17254039966 | 0.206 | 3.850 |
| distinct_values | track2 | 153630 | 6000 | 5271604875 | 17462284403 | 0.302 | 5.719 |
| y_string_unicode | track1 | 35601 | 12000 | 2439294848 | 10175451443 | 0.240 | 5.710 |
| y_string_unicode | track2 | 35601 | 12000 | 2542674479 | 10069201711 | 0.253 | 5.952 |
<!-- PMU_TABLE_END -->

Notes on the table:

- `cycles/B` is computed as `cycles ÷ (bytes × iters)`, the metric the
  S-P1 contract called out as the row P1-D was hard-blocked on. It is a
  direct PMU read, not derived from `ns_per_byte` and clock-frequency
  assumption.
- CPI < 1 is expected on Apple M5 Max: the cores are wide-issue
  superscalar (8-wide decode / 10+ ALU + 4 FP), and steady-state
  parser loops with high branch predictability run at IPC 4 - 8
  (CPI 0.13 - 0.25). The numbers below match published Apple Silicon
  microbenchmark IPC.
- "instructions" is `ri_instructions`, retired-instruction count. It
  excludes mis-speculated paths; the difference between retired and
  speculated is the source of much of the inter-corpus CPI variation.

## §3 - Cross-validation against samply

The V2 P1-A samply profiles already attribute 95.6% - 99.6% of
self-time to a single fused leaf
(`runtime::generated_json::generated::dispatch_value` at
`generated.rs:47`) for every parse-only Track 1 row. PMU cycles/B is
the missing axis: it tells us whether the dispatch-dominated structure
holds an IPC pattern that varies by corpus, and which rows are
cycle-cheap vs cycle-expensive on a per-byte basis.

Ranking convergence between V2 samply Mbps (higher = faster) and this
pass's cycles/B (lower = faster) is reported in the appendix table after
the full PMU sweep lands.

The top-5 self-time symbols per parse-only row from V2 samply remain:

| Corpus | Top self-time (samply V2) | % of samples |
|---|---|---:|
| twitter | `runtime::generated_json::generated::dispatch_value` (`generated.rs:47`) | 98.8% |
| canada | `dispatch_value` | 99.6% |
| citm_catalog | `dispatch_value` | 98.9% |
| apache_builds | `dispatch_value` | 98.6% |
| gsoc-2018 | `dispatch_value` | 99.1% |
| mesh | `dispatch_value` | 99.3% |
| marine_ik | `dispatch_value` | 99.4% |
| unicode_escapes | `dispatch_value` | 99.0% |
| y_string_unicode | `dispatch_value` | 95.6% |
| distinct_values | `dispatch_value` | 98.3% |

Source: `/tmp/skv9-p1-rerun/profile-summary-top5.md`, the V2 samply
aggregator.

The V2 baseline (superseded; see §4 and P1-V3-B §3.4): every parse-only
Track 1 row appears to share the same hot leaf at ~95-99% self-time. That
~95-99% figure is a frame-pointer-coalescing artefact of the samply
mode-I capture; xctrace Time Profiler with DWARF resolves the inlined
leaves and falsifies the single-symbol attribution. cycles/B varies
across rows even though the V2 hot-symbol label did not — that delta is
*within* the fused `dispatch_value` body and is what S-P2 needs to break
apart at a
sub-symbol granularity. Samply alone cannot do that; PMU + sub-leaf
sample correlation is the only mechanical path.

## §4 - Hot-leaf hint from xctrace

The CPU Counters template does not export per-symbol PMC attribution
through the CLI, so per-symbol cycles cannot be reported as a raw row.
The closest mechanical proxy is to combine:

- xctrace Time Profiler per-symbol % self-time from P1-V3-B's
  `/tmp/skv9-xctrace-v3/p1b-tp/<corpus>__<track>.trace` bundles
  (exportable via the `time-sample` schema), and
- the total PMU cycles from §2,

which together yield a cycles-per-symbol estimate at the granularity
of one symbol. Because `dispatch_value` consumes 95-99% of self-time on
every parse-only Track 1 row, that fused symbol absorbs the bulk of the
measured PMU cycles for every row — no per-corpus cohort has a *named*
secondary hot leaf above 1% self-time on parse_only/track1.

In practice this means S-P1 V3 still cannot answer "which sub-leaf of
`dispatch_value` accounts for the 2.37 vs 1.18 cycles/B spread between
twitter and citm_catalog" without a finer-than-samply attribution. That
is the SC-1 / SC-4 hypothesis P1-V3-C and P1-V3-D are charged with;
this report contributes the cycles/B baseline they consume, not the
sub-leaf split itself.

For the three load-bearing string + unicode rows the dispatch named
(twitter, gsoc-2018, y_string_unicode), the PMU deltas reported here
combined with P1-V3-B's xctrace Time Profiler symbol attribution give:

- twitter (track1): 631515 B, 2.373 cycles/B, CPI 0.211. Per P1-V3-B
  §2, rank-1 self-time on this row is
  `match_tiny_plain_string_with_cap::<16>` at 46.2%; the V2 samply
  `dispatch_value` 98.8% reading is a frame-pointer-coalescing artefact
  (per P1-V3-B §3.4) and is *not* the load-bearing attribution.
- gsoc-2018 (track1): see §2 row; per P1-V3-B §2, attribution is dispersed
  across substrate-neutral primitive classes consistent with the
  cycles/B and CPI tabled here.
- y_string_unicode (track1): see §2 row, 5.710 cycles/B at CPI 0.240.
  Per P1-V3-B §2, the top symbols are `hex_nibble` 19.2%,
  `read_hex_unit_scalar` 19.0%, and
  `match_tiny_plain_string_with_cap::<16>` 10.6% — i.e. a unicode-escape
  codec class (`hex_nibble + read_hex_unit_scalar` ≈ 38.2%) load-bears
  this row, not a single fused-dispatch leaf. The V2 samply mode-I
  95.6% `dispatch_value` reading on this row is superseded by P1-V3-B's
  symbol-level measurement.

A "named symbol above 50% cycles share" claim therefore does *not*
reduce to a single fused-dispatch leaf at the xctrace Time Profiler
attribution layer that P1-V3-B established. P1-V3-C is the report that
breaks the per-corpus self-time split apart by sub-strategy
(object/array/string/number/structural-token) consuming both this
report's cycles/B + CPI columns and P1-V3-B's per-row symbol shares;
this report contributes only the row-level cycles/B and CPI truth
columns.

## §5 - Reproduction script

The full capture pipeline that produced §2 lives at
`/tmp/skv9-xctrace-v3/capture.sh`. A second agent can re-run it
end-to-end with:

```bash
# 1. Build the probe binary (one-shot)
cd /Users/mkbabb/Programming/bbnf-lang/skinny && \
  RUSTFLAGS="-C target-cpu=native" \
  cargo build --release -p bbnf-bench --bin xctrace_probe

# 2. Run the capture across all 17 corpora x 2 tracks (~12 min)
/bin/bash /tmp/skv9-xctrace-v3/capture.sh

# 3. Inspect per-row PMU rows
column -t -s $'\t' /tmp/skv9-xctrace-v3/pmu_rows.tsv

# 4. The per-row trace artefacts:
ls /tmp/skv9-xctrace-v3/p1a/                  # CPU Counters traces (this report)
ls /tmp/skv9-xctrace-v3/p1b-tp/               # Time Profiler traces (P1-V3-B)
```

The script reads the corpus set from `skinny/test_data/<corpus>.json`
(symlinks resolve to `skinny/crates/test-fixtures/corpus/json/*.json`
for the three bench fixtures; the remaining 14 live directly under
`test_data/`).

Verbatim invocation per row inside the script:

```bash
xcrun xctrace record --template "CPU Counters" --no-prompt \
  --output /tmp/skv9-xctrace-v3/p1a/${corpus}__${track}.trace \
  --launch -- \
  /Users/mkbabb/Programming/bbnf-lang/skinny/target/release/xctrace_probe \
  /Users/mkbabb/Programming/bbnf-lang/skinny/test_data/${corpus}.json \
  ${track} ${iters}
```

The probe prints one `PROBE_RESULT corpus_bytes=… iters=… track=… …
cycles=… instructions=… cycles_per_byte=… cpi=…` line on stdout; the
wrapper script greps and projects it into `pmu_rows.tsv`.

The probe source is at
`skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs` and contains no
new production logic — it imports `runtime::generated_json::parse` and
`bbnf_bench::track2::json::parse` exactly as the existing
`json_parity` Criterion bench does.

## §6 - What did not capture, and why

### 6.1 Per-symbol PMC attribution via `xctrace export`

The CPU Counters template's PMC bulkstore is captured into the .trace
directory (visible in Instruments.app GUI) but **not** exposed through
any public XPath-addressable schema by `xcrun xctrace export` on macOS
26.0 / Xcode 26.0 / Apple Silicon. We verified this by running
`xctrace export --toc` on a smoke capture — the available schemas
(`tick`, `cpu-state`, `cpu-narrative`, `thread-state`, `time-profile`,
`time-sample`, `context-switch`, `syscall`, `dyld-library-load`,
`kdebug-*`) contain no `cycles`, `instructions`, or PMC event columns.
Explicit XPath queries for `counters-profile`, `pmc-events`, and
`cpu-counters` returned empty result sets.

This is the structural reason a previous attempt to use xctrace alone
would still produce zero rows; rusage_info_v5 is the workaround that
keeps the metric real.

### 6.2 Branch-mispredict + L1d-miss + LLC-miss per-row counters

Apple's public `rusage_info_v5` / `rusage_info_v6` exposes `ri_cycles`
and `ri_instructions` but **not** per-event PMC counts (branch
mispredictions, cache miss classes, mem-load latency etc). Those
counters exist on the Apple M5 Max performance counter unit
(`PMC0..PMC9` per Apple's documented PMU surface), but are reachable
only through:

- the closed `kperf` private framework (linker entitlement gated),
- the Instruments.app GUI on the same `.trace` file we already
  captured, or
- the third-party `m1cpu` / `applepmuctr` patches that depend on
  root + SIP-relaxed boot.

None of these is reachable from a user-space CLI without an
administrative password. The relevant cells in §2 are therefore not
filled; this is not a regression from V2, which had zero PMU rows at
all.

### 6.3 Processor Trace template

The `Processor Trace` template (Apple's instruction-level trace, finer
than CPU Counters) fails on this host with:

```text
Processor Trace library version in Instruments is not compatible with
the library on the target device. Trace producer has format 7.3, but
consumer has format 7.1.
```

This is a versioning skew between macOS 26.4.1 (25E253) and the Xcode
26.0 toolchain; it is not P1-D's blocker but is recorded here for
completeness in case a later wave wants instruction-level traces.

### 6.4 What this report *does* deliver

- Real PMU cycles + instructions per row for all 17 corpora × both
  tracks, from `proc_pid_rusage(RUSAGE_INFO_V5)`, with no estimation
  and no derived clock model.
- cycles/B and CPI per row, computed directly from those counters.
- The `.trace` artefact per row for downstream Instruments.app
  inspection if a human wants the per-PMC breakdown that the CLI
  exporter does not surface.
- Time Profiler `.trace` artefacts per row, which DO expose
  `time-sample` data through the CLI and can be exported into
  per-symbol % weight tables.

This unblocks the S-P1 V2 P1-D BLOCKED verdict for the cycles/B and
CPI columns the contract called out; the per-symbol PMU split remains
a separate axis that requires either Instruments.app GUI inspection
of the `.trace` artefacts or a kperf-enabled binary, neither of which
this report claims to deliver.

### 6.5 PMU manifest status — diagnostic profile evidence, non-producer

The per-row PMU manifest at `/tmp/skv9-xctrace-v3/pmu_rows.tsv` is
diagnostic profile evidence; it does not participate in admission gates
and does not extend `RESULTS.md` schema. The manifest is a profiling
artefact emitted by the read-only `xctrace_probe` binary and consumed
only by S-P1 / S-P2 narration of cycle-cost decomposition. No
`gate-json` or other admission-gate consumer ingests this TSV; the
SK-V9 `gate-json` consumer named in `PASS-1-PROFILE.md` §2 continues
to operate against the existing `RESULTS.md` Mbps + Δ columns
unchanged. Per `LOCKS.md` Lock 1 ("a transient producer, not a
retained sidecar") and the §3W "Same-wave consumer — no orphan kernel"
non-negotiable, this manifest is bound to characteriser status: it
informs hot-leaf cycle-cost narration in S-P1 and S-P2, but never
becomes a route-fact substrate. If a later wave wishes to gate on
cycles/B, it must either commit a stable in-repo manifest path
(superseding the `/tmp/` location) and a matching `gate-json` reader
in the same wave, or accept the manifest's current diagnostic-only
binding indefinitely.

## §7 - Sources

- Probe source:
  `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs`
- Capture script: `/tmp/skv9-xctrace-v3/capture.sh`
- Per-row PMU table (TSV): `/tmp/skv9-xctrace-v3/pmu_rows.tsv`
- Capture log: `/tmp/skv9-xctrace-v3/pmu_log.txt`
- CPU Counters trace dir: `/tmp/skv9-xctrace-v3/p1a/`
- Time Profiler trace dir: `/tmp/skv9-xctrace-v3/p1b-tp/` (produced by sibling P1-V3-B; canonical TP root for V3)
- V2 samply profiles (cross-validation): `/tmp/skv9-p1-rerun/profiles/p1a/`
- V2 samply top-5 summary: `/tmp/skv9-p1-rerun/profile-summary-top5.md`
- V2 P1-D blocked transcript: `/tmp/skv9-p1-rerun/p1d-pmu-probe.txt`
- xctrace version: `xctrace version 26.0 (17A5241e)`
- Probe sanity verification:
  `/tmp/skv9-xctrace-v3/rusage_probe.c` (C reference probe used to
  confirm `ri_cycles` / `ri_instructions` are populated on this host).

## §0 — V4 fold footer

V4 fold edits: cited disposition source per fold; substantive PMU data
unchanged. The 34-row per-row PMU table in §2 is verbatim from the V3
capture (`/tmp/skv9-xctrace-v3/pmu_rows.tsv`); the cycles, instructions,
CPI, and cycles/B columns are not re-derived. Fold edits target only:
the §4 closing bullets on twitter / y_string_unicode (CH1-A8 / CH6-A1:
remove samply-coalescing residual; cite P1-V3-B symbol shares as the
authoritative attribution); the §1.3 / §4 / §5 / §7 Time Profiler path
citations (CH6-A3 / consolidated F5: `p1a-time-profile/` →
`p1b-tp/`, with P1-V3-B named as the canonical TP producer); the
§1.1 corpus-name canonical mapping note (CH6 / consolidated F5: explain
the `update_center` ↔ `update-center.json` hyphen/underscore shear so
downstream aggregators on either spelling resolve); and the new §6.5
paragraph (CH5-A6 / consolidated F6: bind the PMU manifest to
diagnostic profile evidence status, non-`gate-json` consumer, no
`RESULTS.md` schema extension). The CH1-A9 distinct_values c/B
arithmetic typo (`2.88` → `3.850`) cited in the consolidated F5 is not
materially present in the current §3 prose — the §2 PMU table's
`distinct_values | track1 | 3.850 | …` row is the canonical value and
no §3 paragraph contradicts it in this revision. The CH1-A4 regression
provenance neighbour is P1-V3-D's defect, not this report's, and is
not touched here.
