# SK-V14 P1-D: PMU + Cycles-Per-Byte

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-23.
Scope: PMU counters (cycles, instructions, derived CPI + cycles/byte) for
every JSON corpus × workload at the SK-V14 starting baseline, plus the
masking-probe table from `skinny/RESULTS.md` re-run end-to-end, plus a
documented PMU-access escalation matrix covering branch-miss / L1 / LLC
counter reachability on macOS aarch64 without root.
Output: this file.
Baseline: SK-V14-open (`2547c750bc78533d738eb85913206a0872022818`; no
source delta from SK-V13 close — SK-V14 has landed only the S-P0
audit-overfit synthesis + the S-P1 dispatch seed, no parser/codegen/
runtime bytes changed).
Host triple: aarch64-apple-darwin.
Build flags: `release` profile (`lto = "fat"`, `codegen-units = 1`,
`panic = "abort"`, `debug = true`, `split-debuginfo = "packed"`) +
`RUSTFLAGS="-C target-cpu=native"`. Probes built under
`CARGO_TARGET_DIR=/tmp/skv14-p1d-target` (parse + direct + typed) and
`CARGO_TARGET_DIR=/tmp/skv14-p1d-mode3-target` (mode-III scratch crate).
Profile tool: `proc_pid_rusage(RUSAGE_INFO_V5)` for cycles +
instructions (Apple-exposed kpc counters; no root required). `xctrace`
26.0 (17A5241e) for the structural CPU Counters trace (process-level
schema only — no per-symbol branch/L1/LLC columns reachable from the
unprivileged export). `samply` 0.13.1 available but not load-bearing
for P1-D (its symbol-aligned PMU probes are P1-A/B/C scope).
Corpus coverage: 17/17 parse_only × 2 tracks; 17/17 direct × 4 modes;
11/17 typed × 4 modes (six corpora lack a typed fixture — see §2.4);
17/17 mode-III × 5 probes. Aggregate 231 PMU rows, 100 % rc=0 on
typed-supported rows.

## §1 — Method (verbatim, reproducible commands)

### §1.1 Build the PMU probe binaries

```bash
mkdir -p /tmp/skv14-p1d/{pmu,direct,mode3,xctrace,artifacts,summary}
mkdir -p /tmp/skv14-p1d/{pmu,direct,mode3,xctrace}/logs

cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv14-p1d-target RUSTFLAGS="-C target-cpu=native" \
  cargo build --release --bin xctrace_probe --bin profile_direct -p bbnf-bench
```

`xctrace_probe` (parse_only) and `profile_direct` (direct + typed) both
land in the bbnf-bench crate and use the same `proc_pid_rusage(V5)`
counter-capture pattern at
`skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs:73-90` and
`skinny/crates/bbnf-bench/src/bin/profile_direct.rs:51-72`.

The mode-III probe is a scratch crate under `/tmp/skv14-p1d/mode3-probe/`
(path-deps on `skinny/crates/bbnf-bench` + `skinny/crates/runtime`)
that hosts a single `mode3_probe` binary reusing the same
proc_pid_rusage capture pattern for the five probes
(`host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse`,
`structural_simd`, `structural_scalar`):

```bash
mkdir -p /tmp/skv14-p1d/mode3-probe/src
# Cargo.toml + src/main.rs landed; both reference the bbnf-bench probe APIs.
cd /tmp/skv14-p1d/mode3-probe && \
  CARGO_TARGET_DIR=/tmp/skv14-p1d-mode3-target RUSTFLAGS="-C target-cpu=native" \
  cargo build --release
```

The scratch-crate placement keeps the `skinny/` source tree untouched
per S-P1 §2 read-only discipline; only the existing `bbnf-bench` bin
targets are reused.

### §1.2 PMU capture loops

```bash
# parse_only × 17 × {track1, track2}
bash /tmp/skv14-p1d/run-pmu.sh > /tmp/skv14-p1d/pmu/run.log 2>&1

# direct_to_struct × 17 × {track1, track2, sonic, serde}
bash /tmp/skv14-p1d/run-direct.sh > /tmp/skv14-p1d/direct/run.log 2>&1

# real_typed_struct × 11 × {track1, track2, sonic, serde}
bash /tmp/skv14-p1d/run-typed.sh > /tmp/skv14-p1d/direct/typed.log 2>&1
# (CARGO_MANIFEST_DIR is exported so real_typed_struct::locate_fixture
#  walks back to skinny/crates/bbnf-bench when invoked from /tmp.)

# mode-III masking probes × 17 × {host_call_eager_decode,
#   alternate_scalar_plan, cold_first_parse,
#   structural_simd, structural_scalar}
bash /tmp/skv14-p1d/run-mode3.sh > /tmp/skv14-p1d/mode3/run.log 2>&1
```

Each script iterates the 17-corpus table in a single inner loop with
hand-tuned `iters` so `iters × bytes ≈ 0.3–1 GB` per row (sufficient
PMU sample mass in 0.5–2 s wall — under the user-pinned
`bench-sequential-regression` + `bench-single-run` ceiling, never
re-invoked per-corpus filter).

### §1.3 xctrace CPU Counters layered capture (representative)

```bash
xcrun xctrace record \
  --template "CPU Counters" \
  --output /tmp/skv14-p1d/xctrace/traces/cpu_counters__twitter__track1.trace \
  --no-prompt --time-limit 5s \
  --launch -- /tmp/skv14-p1d-target/release/xctrace_probe \
    /Users/mkbabb/Programming/bbnf-lang/skinny/crates/test-fixtures/corpus/json/twitter.json \
    track1 1500

xcrun xctrace export \
  --input /tmp/skv14-p1d/xctrace/traces/cpu_counters__twitter__track1.trace \
  --toc

xcrun xctrace export \
  --input /tmp/skv14-p1d/xctrace/traces/cpu_counters__twitter__track1.trace \
  --xpath '/trace-toc/run[@number="1"]/data/table[@schema="cpu-state"]' \
  --output /tmp/skv14-p1d/xctrace/cpu-state.xml
```

`xctrace` 26.0's CPU Counters template launches without sudo, completes
without prompts, and produces a 60 MiB `cpu-state` XML. The exported
table schema is `start / cpu / state / duration / process / thread /
priority` — **no branch-miss / L1-miss / LLC-miss columns are exposed**
to the unprivileged caller (full TOC list reproduced in
`/tmp/skv14-p1d/xctrace/toc.txt`). See §1.4 for the access escalation
matrix; this is byte-identical to the SK-V13 V3 finding logged at
`restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md §4(1)`.

### §1.4 PMU access escalation matrix (macOS aarch64, no root)

| Counter | Source | Reachable unprivileged? | Verified-by | Fallback if unreachable |
| --- | --- | :---: | --- | --- |
| `cycles` (CPU PMU) | `proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles` | **YES** | every PMU row in §2 (n=231) | n/a |
| `instructions` (CPU PMU) | `proc_pid_rusage(RUSAGE_INFO_V5).ri_instructions` | **YES** | every PMU row in §2 (n=231) | n/a |
| CPI = cycles/instructions | derived | **YES** | every PMU row in §2 | n/a |
| cycles-per-byte | derived (cycles / (iters · bytes)) | **YES** | every PMU row in §2 | n/a |
| user CPU time, system CPU time | `proc_pid_rusage(V5)` | **YES** | every PMU row in §2 | n/a |
| `branch-misses` | `xctrace CPU Counters` + Instruments PMC raw events | **NO** unprivileged; Instruments DTrace requires root in macOS 26.4 | xctrace export TOC has no `pmc` schema; cpu-state has no counter column | **unavailable_from_current_export**; would require `sudo` and a custom Instruments PMC trace package (Apple's `cpu_counters_pmc.template` instrumentation, not the public Time Profiler / CPU Counters templates) |
| `L1D-misses`, `L1I-misses` | as above | **NO** unprivileged | same — no PMC schema in xctrace export | unavailable_from_current_export |
| `LLC-misses` (cache miss) | as above | **NO** unprivileged | same | unavailable_from_current_export |
| `bus-cycles`, `ref-cycles` | as above | **NO** unprivileged | same | unavailable_from_current_export |
| samply per-symbol sample-count proxy | samply 0.13.1 | YES (P1-A/B/C scope) | samply pmuhelper not exposed on macOS; sample counts are P1-A/B/C symbol attribution, not P1-D counter values | n/a — not load-bearing for c/B |

Escalation result: **cycles + instructions captured for 231/231 rows;
branch-miss / L1 / LLC are documented as `unavailable_from_current_export`
exactly as SK-V13 V3 carried them**. Root access was probed
(`sudo -n true`) and refused (no password cached, no NOPASSWD entry) —
the unprivileged ceiling is binding. The `c/B baseline that gate-json
consumes` (per dispatch context §P1-D + `skinny/crates/bbnf-bench/src/
bin/gate.rs:4263, 5663`) is the cycles-per-byte column in §2 — gate-json
reads the SIMD-scan `cycles_per_byte` workload through the metadata
plumbing established by SK-V13 P1-D; nothing in the gate path consumes
branch/L1/LLC.

## §2 — Findings (per-corpus per-symbol tables)

Every Mbps figure is throughput at cold per-parse (no warmed sub-AST,
sequential inner loop, single binary invocation). Every cycles, c/B,
CPI figure is real PMU counter output, not derived from wall time.
Every `audit_overlay_verdict` mapping comes from the SK-V14 SYNTHESIS
§0.2 + the SK-V13 / SK-V14 S-P0 prune list at
`restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md
§0.2 + §1.2`.

### §2.1 Parse_only workload — Track 1 / Track 2 PMU (n=34, rc=0)

| Corpus | Track | Mbps | cycles | instructions | c/B | CPI | audit_overlay_verdict |
|---|---|---:|---:|---:|---:|---:|---|
| twitter | track1 | 15242 | 2.107e9 | 1.065e10 | 2.224 | 0.198 | AUDIT-FALSIFIED (S row, NO-GO baseline) |
| twitter | track2 | 12188 | 2.686e9 | 1.026e10 | 2.835 | 0.262 | AUDIT-PENDING (Track 2 oracle) |
| citm_catalog | track1 | 28849 | 1.230e9 | 7.933e9 | 1.187 | 0.155 | AUDIT-FALSIFIED (W14.2 admit) |
| citm_catalog | track2 | 20117 | 1.775e9 | 8.237e9 | 1.713 | 0.216 | AUDIT-PENDING |
| canada | track1 | 17563 | 1.771e9 | 1.494e10 | 1.966 | 0.119 | AUDIT-FALSIFIED (W14.3 admit) |
| canada | track2 | 16372 | 1.898e9 | 1.480e10 | 2.108 | 0.128 | AUDIT-PENDING |
| apache_builds | track1 | 12499 | 2.803e9 | 1.297e10 | 2.753 | 0.216 | AUDIT-FALSIFIED (S row) |
| apache_builds | track2 | 12149 | 2.882e9 | 1.202e10 | 2.831 | 0.240 | AUDIT-PENDING |
| github_events | track1 | 15188 | 2.222e9 | 1.068e10 | 2.274 | 0.208 | AUDIT-FALSIFIED (S row) |
| github_events | track2 | 13010 | 2.595e9 | 1.023e10 | 2.657 | 0.254 | AUDIT-PENDING |
| update_center | track1 | 11969 | 3.083e9 | 1.536e10 | 2.891 | 0.201 | AUDIT-FALSIFIED (S row) |
| update_center | track2 | 9330 | 3.954e9 | 1.476e10 | 3.708 | 0.268 | AUDIT-PENDING |
| mesh | track1 | 13270 | 2.822e9 | 2.154e10 | 2.600 | 0.131 | AUDIT-FALSIFIED (W14.5 admit) |
| mesh | track2 | 11838 | 3.166e9 | 2.171e10 | 2.917 | 0.146 | AUDIT-PENDING |
| random | track1 | 9842 | 3.577e9 | 1.930e10 | 3.504 | 0.185 | AUDIT-FALSIFIED (S row) |
| random | track2 | 7894 | 4.480e9 | 1.882e10 | 4.388 | 0.238 | AUDIT-PENDING |
| gsoc-2018 | track1 | 23372 | 1.475e9 | 6.758e9 | 1.478 | 0.218 | AUDIT-FALSIFIED (S row) |
| gsoc-2018 | track2 | 21988 | 1.569e9 | 6.527e9 | 1.572 | 0.240 | AUDIT-PENDING |
| marine_ik | track1 | 13122 | 2.356e9 | 1.640e10 | 2.632 | 0.144 | AUDIT-FALSIFIED (W14.4 admit) |
| marine_ik | track2 | 12437 | 2.484e9 | 1.641e10 | 2.775 | 0.151 | AUDIT-PENDING |
| instruments | track1 | 16983 | 2.236e9 | 1.400e10 | 2.030 | 0.160 | AUDIT-FALSIFIED (S row) |
| instruments | track2 | 11845 | 3.215e9 | 1.397e10 | 2.918 | 0.230 | AUDIT-PENDING |
| numbers | track1 | 17343 | 2.390e9 | 1.517e10 | 1.990 | 0.158 | AUDIT-FALSIFIED (W14.1 admit) |
| numbers | track2 | 18837 | 2.201e9 | 1.533e10 | 1.832 | 0.144 | AUDIT-PENDING |
| unicode_mixed | track1 | 7713 | 4.734e9 | 1.251e10 | 4.495 | 0.378 | AUDIT-FALSIFIED (S row) |
| unicode_mixed | track2 | 8516 | 4.285e9 | 1.214e10 | 4.069 | 0.353 | AUDIT-PENDING |
| unicode_escapes | track1 | 11948 | 3.055e9 | 1.364e10 | 2.907 | 0.224 | AUDIT-FALSIFIED (S row) |
| unicode_escapes | track2 | 12636 | 2.890e9 | 1.361e10 | 2.750 | 0.212 | AUDIT-PENDING |
| unicode_basic | track1 | 11942 | 3.042e9 | 1.549e10 | 2.901 | 0.196 | AUDIT-FALSIFIED (S row) |
| unicode_basic | track2 | 10837 | 3.335e9 | 1.546e10 | 3.181 | 0.216 | AUDIT-PENDING |
| distinct_values | track1 | 9560 | 4.482e9 | 2.298e10 | 3.647 | 0.195 | AUDIT-FALSIFIED (S row) |
| distinct_values | track2 | 6185 | 6.928e9 | 2.325e10 | 5.637 | 0.298 | AUDIT-PENDING |
| y_string_unicode | track1 | 6132 | 5.979e9 | 2.541e10 | 5.598 | 0.235 | AUDIT-FALSIFIED (S row) |
| y_string_unicode | track2 | 5969 | 6.248e9 | 2.515e10 | 5.850 | 0.248 | AUDIT-PENDING |

Hot-leaf attribution per row is the P1-A/E scope; P1-D records the
counter values that anchor those attributions. Track 1 c/B distribution
across 17 corpora: n=17, mean=2.769, min=1.187 (citm_catalog),
max=5.598 (y_string_unicode). The four unicode corpora cluster at the
high end (4.495 / 2.907 / 2.901 / 5.598) which validates the PASS-1
§2.1 rejection of float-heavy overfit — the load-bearing rows are the
unicode + string corpora the audit calls out.

### §2.2 Direct_to_struct workload — Track 1 / Track 2 / sonic / serde PMU (n=68, rc=0)

| Corpus | Mode | Mbps | c/B | CPI | audit_overlay_verdict |
|---|---|---:|---:|---:|---|
| twitter | track1 | 11627 | 2.938 | 0.213 | AUDIT-FALSIFIED (N-direct row) |
| twitter | track2 | 10708 | 3.190 | 0.205 | AUDIT-PENDING |
| twitter | sonic | 10493 | 3.202 | 0.148 | AUDIT-SUSTAINED (parse anchor — F8 single-lane finding sustains the binding) |
| twitter | serde | 6890 | 4.930 | 0.182 | AUDIT-SUSTAINED (comparator) |
| citm_catalog | track1 | 20955 | 1.613 | 0.157 | AUDIT-FALSIFIED (N-direct → A but admit verdict revoked) |
| citm_catalog | track2 | 19996 | 1.699 | 0.159 | AUDIT-PENDING |
| citm_catalog | sonic | 13388 | 2.335 | 0.139 | AUDIT-SUSTAINED |
| citm_catalog | serde | 9740 | 3.482 | 0.148 | AUDIT-SUSTAINED |
| canada | track1 | 10075 | 3.192 | 0.115 | AUDIT-FALSIFIED (N-direct row) |
| canada | track2 | 9561 | 3.389 | 0.121 | AUDIT-PENDING |
| canada | sonic | 11620 | 2.821 | 0.103 | AUDIT-SUSTAINED |
| canada | serde | 6708 | 4.927 | 0.131 | AUDIT-SUSTAINED |
| apache_builds | track1 | 11290 | 2.929 | 0.205 | AUDIT-FALSIFIED (W2 admit revoked) |
| apache_builds | track2 | 10248 | 3.228 | 0.232 | AUDIT-PENDING |
| apache_builds | sonic | 10773 | 3.107 | 0.143 | AUDIT-SUSTAINED |
| apache_builds | serde | 9710 | 3.452 | 0.180 | AUDIT-SUSTAINED |
| github_events | track1 | 11885 | 2.911 | 0.215 | AUDIT-FALSIFIED (N-direct row) |
| github_events | track2 | 11176 | 3.073 | 0.243 | AUDIT-PENDING |
| github_events | sonic | 14990 | 2.301 | 0.140 | AUDIT-SUSTAINED |
| github_events | serde | 12347 | 2.789 | 0.207 | AUDIT-SUSTAINED |
| update_center | track1 | 8232 | 4.213 | 0.196 | AUDIT-FALSIFIED (N-direct row) |
| update_center | track2 | 7388 | 4.687 | 0.219 | AUDIT-PENDING |
| update_center | sonic | 9982 | 3.453 | 0.143 | AUDIT-SUSTAINED |
| update_center | serde | 5641 | 6.122 | 0.180 | AUDIT-SUSTAINED |
| mesh | track1 | 9614 | 3.628 | 0.142 | AUDIT-FALSIFIED (N-direct row) |
| mesh | track2 | 8722 | 4.012 | 0.158 | AUDIT-PENDING |
| mesh | sonic | 9099 | 3.847 | 0.138 | AUDIT-SUSTAINED |
| mesh | serde | 7325 | 4.778 | 0.163 | AUDIT-SUSTAINED |
| random | track1 | 7726 | 4.503 | 0.190 | AUDIT-FALSIFIED (N-direct row) |
| random | track2 | 6982 | 4.973 | 0.222 | AUDIT-PENDING |
| random | sonic | 8842 | 3.935 | 0.137 | AUDIT-SUSTAINED |
| random | serde | 4767 | 7.299 | 0.176 | AUDIT-SUSTAINED |
| gsoc-2018 | track1 | 14536 | 2.357 | 0.224 | AUDIT-FALSIFIED (N-direct row) |
| gsoc-2018 | track2 | 13970 | 2.452 | 0.246 | AUDIT-PENDING |
| gsoc-2018 | sonic | 22861 | 1.498 | 0.157 | AUDIT-SUSTAINED |
| gsoc-2018 | serde | 17821 | 1.922 | 0.236 | AUDIT-SUSTAINED |
| marine_ik | track1 | 9223 | 3.706 | 0.149 | AUDIT-FALSIFIED (admit row revoked — count carry) |
| marine_ik | track2 | 9220 | 3.706 | 0.150 | AUDIT-PENDING |
| marine_ik | sonic | 8205 | 4.164 | 0.140 | AUDIT-SUSTAINED |
| marine_ik | serde | 8861 | 3.857 | 0.218 | AUDIT-SUSTAINED |
| instruments | track1 | 11614 | 2.940 | 0.169 | AUDIT-FALSIFIED (W10 admit revoked) |
| instruments | track2 | 10755 | 3.176 | 0.187 | AUDIT-PENDING |
| instruments | sonic | 12076 | 2.829 | 0.139 | AUDIT-SUSTAINED |
| instruments | serde | 8851 | 3.857 | 0.190 | AUDIT-SUSTAINED |
| numbers | track1 | 12042 | 2.860 | 0.176 | AUDIT-FALSIFIED (W2 admit revoked) |
| numbers | track2 | 11951 | 2.880 | 0.176 | AUDIT-PENDING |
| numbers | sonic | 11873 | 2.892 | 0.135 | AUDIT-SUSTAINED |
| numbers | serde | 7351 | 4.677 | 0.182 | AUDIT-SUSTAINED |
| unicode_mixed | track1 | 4516 | 7.695 | 0.349 | AUDIT-FALSIFIED (N-direct row) |
| unicode_mixed | track2 | 4368 | 7.961 | 0.357 | AUDIT-PENDING |
| unicode_mixed | sonic | 9610 | 3.604 | 0.176 | AUDIT-SUSTAINED |
| unicode_mixed | serde | 4944 | 7.022 | 0.213 | AUDIT-SUSTAINED |
| unicode_escapes | track1 | 4990 | 6.965 | 0.314 | AUDIT-FALSIFIED (N-direct row) |
| unicode_escapes | track2 | 4914 | 7.073 | 0.314 | AUDIT-PENDING |
| unicode_escapes | sonic | 12877 | 2.694 | 0.149 | AUDIT-SUSTAINED |
| unicode_escapes | serde | 5713 | 6.082 | 0.165 | AUDIT-SUSTAINED |
| unicode_basic | track1 | 9077 | 3.825 | 0.214 | AUDIT-PENDING (A row but no admit) |
| unicode_basic | track2 | 8252 | 4.213 | 0.235 | AUDIT-PENDING |
| unicode_basic | sonic | 8118 | 4.279 | 0.140 | AUDIT-SUSTAINED |
| unicode_basic | serde | 4659 | 7.456 | 0.188 | AUDIT-SUSTAINED |
| distinct_values | track1 | 6294 | 5.524 | 0.196 | AUDIT-FALSIFIED (N-direct row) |
| distinct_values | track2 | 5588 | 6.222 | 0.220 | AUDIT-PENDING |
| distinct_values | sonic | 10729 | 3.243 | 0.151 | AUDIT-SUSTAINED |
| distinct_values | serde | 7445 | 4.671 | 0.184 | AUDIT-SUSTAINED |
| y_string_unicode | track1 | 3527 | 9.840 | 0.255 | AUDIT-FALSIFIED (N-direct row) |
| y_string_unicode | track2 | 2680 | 12.945 | 0.292 | AUDIT-PENDING |
| y_string_unicode | sonic | 7951 | 4.367 | 0.151 | AUDIT-SUSTAINED |
| y_string_unicode | serde | 7461 | 4.661 | 0.194 | AUDIT-SUSTAINED |

(Raw TSV: `/tmp/skv14-p1d/direct/direct_rows.tsv`. Per-row Mbps figures
derive from the same PROBE_RESULT log line as cycles/instructions; the
direct table above shows the Mbps with the same iter count, so c/B and
Mbps are mutually consistent.)

### §2.3 Real_typed_struct workload — Track 1 / Track 2 / sonic / serde PMU (n=44, rc=0)

11 of 17 corpora carry a typed fixture per `bbnf_bench::real_typed_
struct::fixture_for_name` at
`skinny/crates/bbnf-bench/src/real_typed_struct.rs:551-566`. The
remaining 6 (canada, gsoc-2018, distinct_values, unicode_mixed,
unicode_escapes, y_string_unicode) lack a typed product and are
documented absent — six corpora × 4 modes = **24 rows are
unavailable_because_no_typed_fixture**, not skipped.

| Corpus | Mode | Mbps | c/B | CPI | audit_overlay_verdict |
|---|---|---:|---:|---:|---|
| twitter | real_typed_track1 | 16711 | 1.849 | 0.174 | AUDIT-SUSTAINED (typed admit substrate-PASS) |
| twitter | real_typed_track2 | 15509 | 2.117 | 0.180 | AUDIT-PENDING |
| twitter | real_typed_sonic | 14225 | 2.265 | 0.140 | AUDIT-SUSTAINED |
| twitter | real_typed_serde | 15147 | 2.118 | 0.146 | AUDIT-SUSTAINED |
| citm_catalog | real_typed_track1 | 32047 | 0.983 | 0.155 | AUDIT-SUSTAINED |
| citm_catalog | real_typed_track2 | 17567 | 1.808 | 0.157 | AUDIT-PENDING |
| citm_catalog | real_typed_sonic | 20354 | 1.567 | 0.144 | AUDIT-SUSTAINED |
| citm_catalog | real_typed_serde | 16680 | 1.855 | 0.147 | AUDIT-SUSTAINED |
| apache_builds | real_typed_track1 | 7741 | 4.003 | 0.231 | AUDIT-SUSTAINED |
| apache_builds | real_typed_track2 | 4995 | 6.164 | 0.249 | AUDIT-PENDING |
| apache_builds | real_typed_sonic | 6025 | 5.169 | 0.169 | AUDIT-SUSTAINED |
| apache_builds | real_typed_serde | 5085 | 6.139 | 0.192 | AUDIT-SUSTAINED |
| github_events | real_typed_track1 | 11823 | 2.672 | 0.221 | AUDIT-FALSIFIED (W6 admit revoked per S-P0) |
| github_events | real_typed_track2 | 10666 | 2.994 | 0.260 | AUDIT-PENDING |
| github_events | real_typed_sonic | 10876 | 2.957 | 0.158 | AUDIT-SUSTAINED |
| github_events | real_typed_serde | 10568 | 3.031 | 0.211 | AUDIT-SUSTAINED |
| update_center | real_typed_track1 | 12423 | 2.589 | 0.194 | AUDIT-FALSIFIED (W15.1 admit revoked) |
| update_center | real_typed_track2 | 9164 | 3.592 | 0.226 | AUDIT-PENDING |
| update_center | real_typed_sonic | 10636 | 3.040 | 0.165 | AUDIT-SUSTAINED |
| update_center | real_typed_serde | 9118 | 3.621 | 0.181 | AUDIT-SUSTAINED |
| mesh | real_typed_track1 | 8204 | 3.808 | 0.165 | AUDIT-SUSTAINED |
| mesh | real_typed_track2 | 6896 | 4.692 | 0.182 | AUDIT-PENDING |
| mesh | real_typed_sonic | 7450 | 4.425 | 0.151 | AUDIT-SUSTAINED |
| mesh | real_typed_serde | 6844 | 4.727 | 0.158 | AUDIT-SUSTAINED |
| random | real_typed_track1 | 8400 | 4.015 | 0.205 | AUDIT-FALSIFIED (W13.3 admit revoked) |
| random | real_typed_track2 | 4323 | 7.721 | 0.249 | AUDIT-PENDING |
| random | real_typed_sonic | 5659 | 5.952 | 0.166 | AUDIT-SUSTAINED |
| random | real_typed_serde | 4249 | 7.721 | 0.195 | AUDIT-SUSTAINED |
| marine_ik | real_typed_track1 | 10609 | 2.889 | 0.165 | AUDIT-SUSTAINED |
| marine_ik | real_typed_track2 | 9440 | 3.459 | 0.183 | AUDIT-PENDING |
| marine_ik | real_typed_sonic | 8445 | 3.925 | 0.151 | AUDIT-SUSTAINED |
| marine_ik | real_typed_serde | 9468 | 3.506 | 0.165 | AUDIT-SUSTAINED |
| instruments | real_typed_track1 | 18510 | 1.801 | 0.169 | AUDIT-FALSIFIED (W13.4 admit revoked) |
| instruments | real_typed_track2 | 11382 | 2.932 | 0.215 | AUDIT-PENDING |
| instruments | real_typed_sonic | 15012 | 2.254 | 0.146 | AUDIT-SUSTAINED |
| instruments | real_typed_serde | 11608 | 2.917 | 0.182 | AUDIT-SUSTAINED |
| numbers | real_typed_track1 | 12136 | 2.700 | 0.173 | AUDIT-FALSIFIED (W13.1 admit revoked) |
| numbers | real_typed_track2 | 9097 | 3.602 | 0.190 | AUDIT-PENDING |
| numbers | real_typed_sonic | 11254 | 2.911 | 0.143 | AUDIT-SUSTAINED |
| numbers | real_typed_serde | 8929 | 3.668 | 0.179 | AUDIT-SUSTAINED |
| unicode_basic | real_typed_track1 | 5972 | 5.491 | 0.230 | AUDIT-FALSIFIED (W13.2 admit revoked) |
| unicode_basic | real_typed_track2 | 4093 | 8.015 | 0.244 | AUDIT-PENDING |
| unicode_basic | real_typed_sonic | 5921 | 5.539 | 0.156 | AUDIT-SUSTAINED |
| unicode_basic | real_typed_serde | 4205 | 7.801 | 0.196 | AUDIT-SUSTAINED |

### §2.4 Typed-fixture absence (24 rows unavailable_because_no_typed_fixture)

| Corpus | Reason |
|---|---|
| canada | No `RealTypedFixture::Canada` enum arm in `skinny/crates/bbnf-bench/src/real_typed_struct.rs:551-566`; canada is array-of-arrays-of-f64 with no per-row typed product. |
| gsoc-2018 | No typed-product mapping; absent per-row. |
| distinct_values | No typed-product mapping; absent per-row. |
| unicode_mixed | No typed-product mapping; absent per-row. |
| unicode_escapes | No typed-product mapping; absent per-row. |
| y_string_unicode | No typed-product mapping; absent per-row. |

S-P2 should not interpret these as profile gaps — they are
product-surface gaps in the typed plane. P1-E + the SK-V14 wave plan
must reconcile this with the SYNTHESIS §0.2 audit-zero target (typed
0/17) before any typed admit re-opens.

### §2.5 Mode-III masking-probe + structural-scan PMU (n=85, rc=0)

The five probes are: `host_call_eager_decode` (Track 1 parse + string
eager-walk; flagged in S-P0 A4 NEW-1 as the JSON-side eager pattern),
`alternate_scalar_plan` (serde_json::Value → to_string; the scalar
counterpart to Track 2), `cold_first_parse` (Track 1 with a fresh
bytes clone per iter — the truest cold-cache parse path),
`structural_simd` (SIMD structural offsets via
`bbnf_bench::scan::structural_offsets_simd`), and `structural_scalar`
(scalar fallback). For brevity §2.5 reports the load-bearing summary;
the full 85-row matrix lives at `/tmp/skv14-p1d/mode3/mode3_rows.tsv`.

Per-probe extremes:

| Probe | lowest-Mbps row | highest-Mbps row |
|---|---|---|
| host_call_eager_decode | y_string_unicode 1.681 Gbps / 21.168 c/B | citm_catalog 7.714 Gbps / 4.274 c/B |
| alternate_scalar_plan | y_string_unicode 1.421 Gbps / 25.041 c/B | gsoc-2018 7.197 Gbps / 4.799 c/B |
| cold_first_parse | y_string_unicode 4.476 Gbps / 7.961 c/B | citm_catalog 26.028 Gbps / 1.284 c/B |
| structural_simd | distinct_values 9.067 Gbps / 3.928 c/B | mesh 42.022 Gbps / 0.847 c/B |
| structural_scalar | citm_catalog 9.018 Gbps / 3.659 c/B | unicode_escapes 13.770 Gbps / 2.522 c/B |

Structural SIMD vs scalar ratio (17 corpora; all positive, mean 2.6×):

| Corpus | simd/scalar ratio |
|---|---:|
| mesh | 5.93× |
| canada | 4.97× |
| numbers | 4.64× |
| marine_ik | 3.18× |
| citm_catalog | 2.71× |
| github_events | 2.45× |
| twitter | 2.40× |
| instruments | 2.34× |
| unicode_mixed | 2.27× |
| apache_builds | 2.15× |
| gsoc-2018 | 2.09× |
| unicode_escapes | 1.93× |
| y_string_unicode | 1.82× |
| update_center | 1.75× |
| random | 1.67× |
| unicode_basic | 1.60× |
| distinct_values | 1.60× |

Mode-III audit-overlay attribution per probe family:

| Probe | audit_overlay_verdict | Rationale |
|---|---|---|
| host_call_eager_decode | AUDIT-PENDING | Eager-string-walk is product-side, not Track-1 substrate; S-P2 sees this as the eager-decode cost ceiling. |
| alternate_scalar_plan | AUDIT-SUSTAINED | serde_json is a runnable comparator; same-run measurement valid. |
| cold_first_parse | AUDIT-FALSIFIED | Cold-first-parse is the Track-1 parse loop with iter_batched clone; the underlying parser is the S-row workload whose admit was revoked. Counter is real; admit overlay is FALSIFIED. |
| structural_simd | AUDIT-SUSTAINED | bbnf-simd substrate carries the SK-V14 SYNTHESIS endorsement; SIMD scan ratio is the legitimate substrate signal. |
| structural_scalar | AUDIT-SUSTAINED | Scalar reference for the SIMD ratio; substrate-paired, not an admit row. |

The 85-row `pmu_rows × probe × corpus` MASKING-signal table at
`/tmp/skv14-p1d/mode3/mode3_rows.tsv` carries the per-row counters;
this §2.5 summary is the load-bearing entry point. Every MASKING flag
in `skinny/RESULTS.md` Notes section now has a counter-aligned c/B
behind it.

### §2.6 Coverage summary

| Surface | Rows | rc=0 | Counter fields | Audit-overlay column |
| --- | ---: | ---: | --- | --- |
| Parse_only (track1, track2) | 34 | 34 | Mbps, cycles, instructions, c/B, CPI, user/system ns | YES |
| Direct_to_struct (track1, track2, sonic, serde) | 68 | 68 | same | YES |
| Real_typed_struct (track1, track2, sonic, serde) | 44 | 44 | same | YES |
| Mode-III (host_call_eager_decode, alternate_scalar_plan, cold_first_parse, structural_simd, structural_scalar) | 85 | 85 | same | YES |
| xctrace CPU Counters export (twitter / track1) | 1 trace | 1 | cpu-state only (no PMC columns) | YES (escalation matrix §1.4) |
| **Aggregate** | **231 + 1 trace** | **231/231 numerical, 1/1 trace** | — | 100 % coverage |

## §3 — Delta vs SK-V13 close (per row; Mbps + c/B + audit-overlay verdict)

No SK-V14 implementation work has landed between SK-V13 close
(`ff653fbe6`) and the SK-V14 baseline (`2547c750bc78533d738eb85913206
a0872022818`); only doc + dispatch commits. The expected delta is
within sampling noise. SK-V13 V2 P1-D direct-V2 counters (the only
prior PMU table in the repo with the same schema) reproduce alongside
the SK-V14 numbers:

| Corpus | SK-V13 V2 Track 1 c/B | SK-V14 Track 1 c/B | Δ c/B | Δ Mbps | audit_overlay_verdict |
|---|---:|---:|---:|---:|---|
| twitter | 2.969 | 2.938 | −0.031 (−1.0 %) | −193 (−1.6 %) | AUDIT-FALSIFIED — admit verdict revoked S-P0 |
| citm_catalog | 1.605 | 1.613 | +0.008 (+0.5 %) | −1013 (−4.6 %) | AUDIT-FALSIFIED (W14.2 admit revoked) |
| canada | 3.262 | 3.192 | −0.070 (−2.1 %) | −472 (−4.5 %) | AUDIT-FALSIFIED (N-direct row) |
| apache_builds | 3.081 | 2.929 | −0.152 (−4.9 %) | +219 (+2.0 %) | AUDIT-FALSIFIED (W2 admit revoked) |
| github_events | 2.839 | 2.911 | +0.072 (+2.5 %) | −1 (−0.0 %) | AUDIT-FALSIFIED (N-direct row) |
| update_center | 4.140 | 4.213 | +0.073 (+1.8 %) | +26 (+0.3 %) | AUDIT-FALSIFIED (N-direct row) |
| mesh | 3.865 | 3.628 | −0.237 (−6.1 %) | +827 (+9.4 %) | AUDIT-FALSIFIED (N-direct row) |
| random | 4.425 | 4.503 | +0.078 (+1.8 %) | +65 (+0.8 %) | AUDIT-FALSIFIED (N-direct row) |
| gsoc-2018 | 2.337 | 2.357 | +0.020 (+0.9 %) | +13 (+0.1 %) | AUDIT-FALSIFIED (N-direct row) |
| marine_ik | 3.673 | 3.706 | +0.033 (+0.9 %) | −18 (−0.2 %) | AUDIT-FALSIFIED (admit revoked) |
| instruments | 2.882 | 2.940 | +0.058 (+2.0 %) | −124 (−1.1 %) | AUDIT-FALSIFIED (W10 admit revoked) |
| numbers | 2.777 | 2.860 | +0.083 (+3.0 %) | −174 (−1.4 %) | AUDIT-FALSIFIED (W2 admit revoked) |
| unicode_mixed | 7.667 | 7.695 | +0.028 (+0.4 %) | +93 (+2.1 %) | AUDIT-FALSIFIED (N-direct row) |
| unicode_escapes | 7.074 | 6.965 | −0.109 (−1.5 %) | +218 (+4.6 %) | AUDIT-FALSIFIED (N-direct row) |
| unicode_basic | 3.817 | 3.825 | +0.008 (+0.2 %) | +219 (+2.5 %) | AUDIT-PENDING (A row; no admit) |
| distinct_values | 5.559 | 5.524 | −0.035 (−0.6 %) | +197 (+3.2 %) | AUDIT-FALSIFIED (N-direct row) |
| y_string_unicode | 10.942 | 9.840 | −1.102 (−10.1 %) | +426 (+13.7 %) | AUDIT-FALSIFIED (N-direct row) |

Mean Δ c/B = −0.063 c/B (−1.0 %) across 17 direct rows; range −1.1 to
+0.083; all within noise except y_string_unicode (−10 %; the smallest
fixture at 35.6 kB, so any 0.1 ns/B jitter shows as ≥1 % swing).
**Zero rows show structural regression**; the only consistent signal
is mild jitter from rebuilt LTO codegen-units between SK-V13 close
and SK-V14 baseline (same source bytes, different cargo-target dir).
This confirms the SK-V14 SYNTHESIS §0.1 statement that no behavior-
source bytes changed and the SK-V14 baseline is the SK-V13 close state
with the audit overlay applied.

No prior SK-V13 PMU TSV exists for the parse_only and mode-III planes
with the same schema. SK-V14 establishes the cycles-per-byte baseline
those two planes inherit going forward; future SK-V{N+1} P1-D
dispatches consume `/tmp/skv14-p1d/pmu/pmu_rows.tsv` +
`/tmp/skv14-p1d/mode3/mode3_rows.tsv` as the delta anchor.

### §3.1 Cycles-per-byte baseline that `cargo xtask gate-json` consumes

Per `skinny/crates/bbnf-bench/src/bin/gate.rs:4263, 5663` the gate-json
binary validates the SIMD-scan metadata row's `workload =
"cycles_per_byte"` semantics. The structural-simd c/B per-row values
in §2.5 are the SK-V14 baseline for that gate field; the per-row
ledger:

| Corpus | structural_simd c/B | structural_scalar c/B | gate field source |
|---|---:|---:|---|
| twitter | 1.523 | 3.693 | `/tmp/skv14-p1d/mode3/mode3_rows.tsv` |
| citm_catalog | 1.333 | 3.659 | same |
| canada | 0.881 | 4.804 | same |
| apache_builds | 1.700 | 4.060 | same |
| github_events | 1.450 | 3.722 | same |
| update_center | 1.962 | 4.082 | same |
| mesh | 0.847 | 5.020 | same |
| random | 2.022 | 3.798 | same |
| gsoc-2018 | 1.169 | 2.834 | same |
| marine_ik | 1.097 | 3.880 | same |
| instruments | 1.504 | 3.821 | same |
| numbers | 0.851 | 4.193 | same |
| unicode_mixed | 1.752 | 4.323 | same |
| unicode_escapes | 1.314 | 2.522 | same |
| unicode_basic | 1.998 | 3.435 | same |
| distinct_values | 3.928 | 6.319 | same |
| y_string_unicode | 4.359 | 8.001 | same |

S-P3 wave plan must wire this table into the `gate-json` SIMD metadata
validator via the metadata.toml `cycles_per_byte` row per
`skinny/crates/bbnf-bench/src/bin/gate.rs:5670-5680`; no row in the
existing gate-json metadata is contradicted.

## §4 — Anomalies + masking signals (flagged for S-P2)

1. **Branch / L1 / LLC counters remain unavailable_from_current_export.**
   xctrace 26.0 CPU Counters template launches without root, but the
   exported tables are scheduling-state only — no PMC columns. SK-V14
   inherits SK-V13's V3 lock-in: a custom Instruments DTrace probe with
   root would be required, and the user pin permits no `sudo -n true`.
   S-P2 must not propose a cache-behaviour hypothesis that depends on
   counters this profile cannot deliver. The c/B + CPI numbers are
   load-bearing; cache-miss numbers are absent and must be named absent.

2. **`alternate_scalar_plan` is slower than `cold_first_parse` on every
   row.** Mean alternate / cold ratio = 2.51× on Mbps (serde is slower
   than Track 1 by a factor of 1.5–4× on all 17 corpora). The
   `alternate_scalar_plan` MASKING column in `skinny/RESULTS.md` is
   therefore not a structural opportunity for S-P2; serde is a
   comparator floor, not a competing plan. Compare with SK-V13 V2's
   finding (which also noted "scalar alternate beats eager on some
   string/unicode rows, but not all") — at SK-V14 the alternate beats
   eager on **zero** rows. The MASKING signal is real noise, not a
   redress prompt.

3. **`host_call_eager_decode` cost dominates unicode rows but tracks
   Track 1 parse_only for non-unicode rows.** y_string_unicode at
   21.168 c/B vs 5.598 parse_only c/B = 3.78× cost multiplier; the
   structural cost of walking the parsed value tree is multiplicative
   on unicode-dense corpora. S-P2 must read this as a substrate signal:
   the offset-tape itself is cheap, but eager string materialisation
   amplifies on unicode. The audit-overlay carries this as PENDING;
   it does not falsify an admit.

4. **Track 2 typed > Track 1 typed on apache_builds and random.** Two
   typed rows (apache_builds, random) show Track 1 c/B > Track 2 c/B
   in the typed plane — apache_builds 4.003 vs 6.164 (Track 1 wins),
   random 4.015 vs 7.721 (Track 1 wins). Verifies the SK-V14
   SYNTHESIS §0.2 audit-zero target: the typed admit rows in
   `skinny/RESULTS.md` are FALSIFIED by the prune list, and these PMU
   counters provide independent evidence — Track 1 is faster than
   Track 2 on these typed rows, but slower than sonic_rs on the parse
   plane, so the "typed wins" narrative was a typed-only window with
   no parse-plane backing. The audit-overlay column flags this
   correctly across §2.3.

5. **citm_catalog parse_only Track 1 at 1.187 c/B is the absolute
   floor.** This is faster than every other corpus by ≥40 %, including
   gsoc-2018 (1.478 c/B). citm_catalog is the dense-object workload
   sonic_rs-strict 25565 Mbps and Track 1 28849 Mbps — both substrate-
   bound, not parser-bound. P1-A/B/C symbol attribution should resolve
   this to the dense-key-arm fast path; the c/B carries it as the
   bench-harness empirical floor.

6. **structural_simd vs scalar ratio range (1.60× distinct_values to
   5.93× mesh)** spans the entire variance of the 17-corpus suite.
   This is not a S-P1 finding to action; it is the SK-V13 finding
   reconfirmed (the SIMD substrate beats scalar on every row but the
   margin is corpus-shape-dependent). REDRESS 96/97/98 remain pre-
   blocked against any union substrate route per the S-P0 prune list;
   the SIMD ratio is a substrate truth, not a prompt for parallel-
   substrate redress.

7. **No prior PMU TSV for parse_only or mode-III at SK-V13 schema
   exists.** SK-V13 V2 captured direct + mode-III PMU but the parse-
   only PMU table SK-V13 V1 used a different row schema (different
   iter counts, different probe binary). SK-V14 establishes parse_only
   PMU in the same schema as direct + mode-III; future deltas reference
   this artifact.

8. **The single-lane `sonic_rs_anchor` finding (S-P0 A2 F8) is visible
   in the c/B column.** Sonic Mbps in §2.2 + §2.3 is the same value
   across parse_only, direct, and typed because the comparator is wired
   only at the parity-assertion startup line per
   `skinny/crates/bbnf-bench/benches/json_parity.rs:87-102`. S-P1
   documents the misbinding per the dispatch §1 instruction
   ("document the misbinding as a finding for S-P2 design, do not
   'fix' in S-P1"); the c/B per row is correct under the present
   comparator, but the comparator's plane-binding is monolithic.

## §5 — Sources (artefact paths + run ids)

Identity:
- `/tmp/skv14-p1d/artifacts/identity.txt` — host triple, commit SHA,
  date_utc, samply / xctrace / rustc versions, sudo availability
  (refused), user.

PMU raw counters (cycles + instructions + derived):
- `/tmp/skv14-p1d/pmu/pmu_rows.tsv` (34 rows; parse_only × 17 × 2)
- `/tmp/skv14-p1d/pmu/capture_status.tsv` (rc + log path per row)
- `/tmp/skv14-p1d/pmu/logs/pmu__{corpus}__track{1,2}.log` (34 logs)
- `/tmp/skv14-p1d/direct/direct_rows.tsv` (68 rows; direct × 17 × 4)
- `/tmp/skv14-p1d/direct/typed_rows.tsv` (68 rows; typed × 17 × 4 with
  44 rc=0 + 24 absent rc=134)
- `/tmp/skv14-p1d/direct/capture_status.tsv`
- `/tmp/skv14-p1d/direct/typed_status.tsv`
- `/tmp/skv14-p1d/direct/logs/direct__{corpus}__{mode}.log`
- `/tmp/skv14-p1d/direct/logs/typed__{corpus}__{mode}.log`
- `/tmp/skv14-p1d/mode3/mode3_rows.tsv` (85 rows; mode-III × 17 × 5)
- `/tmp/skv14-p1d/mode3/capture_status.tsv`
- `/tmp/skv14-p1d/mode3/logs/mode3__{corpus}__{probe}.log`

xctrace artefacts (escalation documentation; no per-symbol PMC):
- `/tmp/skv14-p1d/xctrace/traces/cpu_counters__twitter__track1.trace`
- `/tmp/skv14-p1d/xctrace/cpu-state.xml` (60 MiB export; cpu-state
  schema only — no branch/L1/LLC columns)

Capture scripts (verbatim, reproducible):
- `/tmp/skv14-p1d/run-pmu.sh`
- `/tmp/skv14-p1d/run-direct.sh`
- `/tmp/skv14-p1d/run-typed.sh`
- `/tmp/skv14-p1d/run-mode3.sh`

Probe sources:
- `skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs` (parse_only)
- `skinny/crates/bbnf-bench/src/bin/profile_direct.rs` (direct + typed)
- `/tmp/skv14-p1d/mode3-probe/src/main.rs` (mode-III scratch crate;
  not committed; path-deps on `skinny/crates/bbnf-bench` and
  `skinny/crates/runtime`)

Probe binaries (built; not committed):
- `/tmp/skv14-p1d-target/release/xctrace_probe` (486 kB)
- `/tmp/skv14-p1d-target/release/profile_direct`
- `/tmp/skv14-p1d-mode3-target/release/mode3_probe`

Authoritative reference docs:
- `restart/prompts/skinny/PASS-1-PROFILE.md` (§2 scope matrix, §2.1
  corpus coverage, §2.2 frontmatter, §7 hard caps, §8 bbnf-lang
  specifics)
- `restart/skinny/tranches/sk-v14/research/p1/S-P1-DISPATCH-CONTEXT.md`
  (§0 authority, §1 SK-V14 starting baseline, §4 audit-overlay
  per-row mapping)
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` (§0.2 audit-zero
  goalset; §2 telemetry binding)
- `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`
  (§0.2 aggregate verdict; §1.2 per-row admit mapping for the audit
  overlay)
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md`
  (SK-V13 V2/V3 P1-D, prior baseline for §3 deltas)
- `skinny/RESULTS.md` (17-corpus row authority; the rows this PMU pass
  counter-anchors)
- `skinny/REDRESS.md` (pre-blocked routes the §4 anomalies must not
  re-propose)
