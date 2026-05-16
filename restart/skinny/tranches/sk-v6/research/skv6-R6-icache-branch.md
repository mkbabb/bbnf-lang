# SK-V6 Wave 1 R6 — own-binary i-cache + branch/PMU assessment

Date: 2026-05-14. Host: macOS 26.4.1 (25E253), arm64, Apple M5 Max. Workspace:
`/Users/mkbabb/Programming/bbnf-lang`.

## Authority read

Read before measurement:

- `restart/skinny/tranches/sk-v6/DISPATCH-PROMPT.md` §2
  profile-first rule, §5 Wave 1 R6, §7 Lock 15, and §9 same-symbol
  fuse-collapse warning.
- `restart/MASTER-PLAN.md` §13 Lock 15 / H.W rows.
- `skinny/RESULTS.md`.
- `restart/skinny/tranches/sk-v5/research/skv5-A1-comparative.md` Lock 15 /
  yyjson notes.
- `restart/skinny/tranches/sk-v5/research/skv5-B1-parse-attribution.md`.

## Tool availability / PMU status

Available: `samply 0.13.1`, `nm`, `otool`, `size`, `cargo`, `rustc`,
`xctrace`, `powermetrics`.

Not usable for the requested hardware counters in this session:

- `perf`: absent on this macOS host.
- `xctrace`: present, but `xctrace list instruments` fails because the active
  developer directory is CommandLineTools, not full Xcode.
- `powermetrics --show-process-ipc --show-process-amp`: fails with
  `powermetrics must be invoked as the superuser`.
- `samply`: usable for sampled PC/self-time, but it does not expose branch
  mispredict, L1i miss, or IPC counters.

Therefore R6 cannot report actual branch-mispredict rate, L1i miss rate, or IPC
without sudo/private Instruments setup. The evidence below is proxy evidence:
symbol sizes, static branch density from `otool -tV`, sampled self-time
fragmentation, and row-cluster behavior.

## Build artifacts

Requested diagnostic build:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv6-cargo/R6 \
  cargo build --release -p xtask --bin profile-lazy \
  --features runtime/parse-attribution
```

Binary: `/tmp/skv6-cargo/R6/release/profile-lazy`
Mach-O UUID: `E540A871-FD9E-38A9-B310-92C886FEBEBE`
`size -m`: `__TEXT,__text = 240172` bytes.

I also built a default no-`parse-attribution` binary in
`/tmp/skv6-cargo/R6-default/release/profile-lazy` only to answer the Lock 15
fuse-collapse question.

## Lock 15 hot-function size

Budget: 20 KiB = 20480 bytes.

| Build / hot set | Bytes | Budget share | Verdict |
|---|---:|---:|---|
| Default production `parse_value_at` | 8768 | 42.8% | Within Lock 15; still fused in default build. |
| Parse-attribution `parse_value_at` | 60 | 0.3% | Split by diagnostic feature. |
| Parse-attribution largest hot symbol, `match_string_at_quote` | 1976 | 9.6% | No single-symbol i-cache risk. |
| Parse-attribution outlined generated hot set, excluding `structural_capacity_for` | 6876 | 33.6% | Fits comfortably. |
| Same, adding `structural_capacity_for` | 9984 | 48.8% | Still below budget. |

Conclusion: the default build still fuse-collapses into one generated
`parse_value_at`, but the fused function is 8.6 KiB, not an obvious Lock 15
violation. The requested parse-attribution build successfully splits the hot
path into named callees; sampled self-time no longer collapses into a single
`parse_value_at` symbol.

## Parse-attribution symbol sizes and static branch density

Static counts are from `otool -tV /tmp/skv6-cargo/R6/release/profile-lazy`.
`branches` includes `b*`, `cbz/cbnz`, `tbz/tbnz`, calls, and `ret`;
`cond` includes conditional branches only.

| Symbol | Bytes | Instr | Branches | Cond | Calls | Branch density |
|---|---:|---:|---:|---:|---:|---:|
| `parse_value_at` | 60 | 15 | 4 | 1 | 1 | 26.7% |
| `dispatch_value` | 192 | 48 | 19 | 10 | 1 | 39.6% |
| `match_tiny_plain_string` | 80 | 20 | 6 | 4 | 0 | 30.0% |
| `skip_ws` | 204 | 51 | 15 | 11 | 0 | 29.4% |
| `consume_structural` | 292 | 73 | 19 | 14 | 1 | 26.0% |
| `match_number_at_digit` | 720 | 180 | 46 | 37 | 0 | 25.6% |
| `consume_container_next` | 560 | 140 | 34 | 26 | 1 | 24.3% |
| `parse_key_colon` | 712 | 178 | 43 | 29 | 5 | 24.2% |
| `match_string_at_quote` | 1976 | 494 | 68 | 53 | 0 | 13.8% |

Static reading: the branchiest dense leaves are tiny dispatch/byte-class
helpers (`dispatch_value`, `match_tiny_plain_string`, `skip_ws`), but they are
small. The largest leaf, `match_string_at_quote`, has the most absolute
conditional branches, but a lower branch density because it contains the larger
string scan body. This points away from a simple i-cache-capacity pathology and
toward per-byte/string/number branch behavior, pending real branch counters.

## Samply profiles

Profiles are in `/tmp/skv6-R6-profiles/`. Each row has:

- `/tmp/skv6-R6-profiles/<row>.profile.json.gz`
- `/tmp/skv6-R6-profiles/<row>.profile.json.syms.json`
- `/tmp/skv6-R6-profiles/<row>.samply.stderr`

Command shape:

```bash
samply record --rate 4000 --main-thread-only --unstable-presymbolicate \
  --save-only --no-open -o /tmp/skv6-R6-profiles/<row>.profile.json.gz \
  /tmp/skv6-cargo/R6/release/profile-lazy <iters> <fixture-path>
```

Measured loop throughput under samply:

| Row | Iters | Seconds | Mbps |
|---|---:|---:|---:|
| `citm_catalog` | 4000 | 3.33 | 16609 |
| `apache_builds` | 40000 | 3.78 | 10773 |
| `github_events` | 80000 | 3.81 | 10927 |
| `update_center` | 8000 | 4.33 | 7881 |
| `gsoc-2018` | 3000 | 4.05 | 19704 |
| `instruments` | 25000 | 4.66 | 9456 |
| `distinct_values` | 20000 | 4.66 | 5280 |
| `y_string_unicode` | 80000 | 4.16 | 5477 |
| `marine_ik` | 2000 | 5.01 | 9536 |

## Sampled self-time by row

Percentages are leaf/self samples in the parse-attribution build.

| Row | Generated self | Hot generated symbols >=1% | Dominant leaves |
|---|---:|---:|---|
| `citm_catalog` | 99.4% | 13 | `consume_container_next` 21.1%, `match_tiny_plain_string` 16.9%, `consume_structural` 11.6%, `skip_ws` 8.4%, `emit_plain_offset` 8.4%, `parse_key_colon` 8.2% |
| `apache_builds` | 98.9% | 11 | `match_tiny_plain_string` 36.3%, `match_string_at_quote` 21.9%, `consume_container_next` 9.4%, `parse_key_colon` 9.4% |
| `github_events` | 98.1% | 11 | `match_string_at_quote` 33.8%, `match_tiny_plain_string` 26.4%, `parse_key_colon` 8.1%, `consume_container_next` 7.2% |
| `update_center` | 99.4% | 9 | `match_tiny_plain_string` 40.3%, `match_string_at_quote` 25.0%, `consume_container_next` 6.4%, `emit_plain_offset` 5.8% |
| `gsoc-2018` | 99.7% | 7 | `match_string_at_quote` 63.9%, `match_tiny_plain_string` 18.4%, `parse_key_colon` 3.8%, `consume_container_next` 3.7% |
| `instruments` | 98.4% | 12 | `match_tiny_plain_string` 32.0%, `parse_key_colon` 15.4%, `match_string_at_quote` 11.1%, `consume_container_next` 10.7%, `match_number_at_digit` 8.3% |
| `distinct_values` | 99.4% | 7 | `match_tiny_plain_string` 57.2%, `match_string_at_quote` 16.9%, `consume_quote_at_cursor` 6.2%, `emit_plain_offset` 5.9% |
| `y_string_unicode` | 97.6% | 9 | `match_string_at_quote` 62.6%, `consume_container_next` 7.6%, `match_tiny_plain_string` 7.5%, `patch_flags` 4.5% |
| `marine_ik` | 99.8% | 12 | `match_number_at_digit` 34.2%, `consume_container_next` 21.0%, `dispatch_value` 8.8%, `emit_plain_offset` 8.7%, `parse_number` 8.1% |

## Row-cluster assessment

### String / key-string rows

Rows: `apache_builds`, `github_events`, `update_center`, `gsoc-2018`,
`distinct_values`, `y_string_unicode`, and part of `instruments`.

The common hot leaves are `match_tiny_plain_string` and
`match_string_at_quote`. `gsoc-2018` and `y_string_unicode` are dominated by
`match_string_at_quote` alone (63.9% and 62.6%), while `distinct_values` and
`update_center` are dominated by the tiny-string path (57.2% and 40.3%).

Proxy branch risk is plausible in the string path, but not yet proven:
`match_tiny_plain_string` is branch-dense but only 80 bytes; `match_string_at_quote`
is the largest leaf and carries 53 static conditional branches, but remains
only 1976 bytes. This does not look like a 20 KiB i-cache-capacity miss. It
looks more like row-dependent byte classification / string terminator /
escape / UTF-8 branch work inside a compact code footprint.

### Structural/key churn rows

Rows: `citm_catalog`, `instruments`.

`citm_catalog` is fragmented across 13 generated hot leaves >=1% with no leaf
above 21.1%. That is the strongest front-end proxy in this run: work bounces
across container progression, structural consume, whitespace, offset emission,
and key/colon parsing. Still, the whole outlined hot set is 6.9 KiB, so this
is branch/call-path diffusion, not an obvious i-cache residency failure.

`instruments` has the same key/structure shape plus a visible number component:
`match_tiny_plain_string` 32.0%, `parse_key_colon` 15.4%,
`consume_container_next` 10.7%, `match_number_at_digit` 8.3%.

### Numeric/array row

Row: `marine_ik`.

This row is number/container bound: `match_number_at_digit` 34.2% and
`consume_container_next` 21.0%. Static branch density for
`match_number_at_digit` is 25.6% with 37 conditional branches in 720 bytes.
Again, this is a compact branchy leaf, not a code-size/i-cache-capacity
candidate.

## R6 verdict

1. Lock 15 holds by symbol size. The default fused `parse_value_at` is 8768
   bytes, well below 20 KiB. The parse-attribution hot set is even smaller per
   symbol, with the largest outlined leaf at 1976 bytes.
2. The requested parse-attribution feature does split the same-symbol
   fuse-collapse for profiling. In the diagnostic build, `parse_value_at` is a
   60-byte dispatch wrapper and row self-time lands in named leaves.
3. No actual branch-mispredict, L1i-miss, or IPC conclusion is possible from
   this host session because useful PMU paths were blocked (`perf` absent,
   `xctrace` not usable with CLT-only setup, `powermetrics` requires
   superuser).
4. Proxy evidence does not support prescribing an i-cache-capacity kernel.
   The hot code footprint is too small relative to Lock 15 and yyjson's cited
   ~18 KiB fused proof point.
5. Proxy evidence does support further PC-level investigation of compact
   branchy leaves: string rows concentrate in `match_tiny_plain_string` /
   `match_string_at_quote`; `marine_ik` concentrates in `match_number_at_digit`
   / `consume_container_next`; `citm_catalog` shows broad structural hot-symbol
   fragmentation.

## Falsifiable diagnostic recommendation

For synthesis, run one privileged PMU diagnostic, not a kernel intervention:
collect PC-level branch-miss, L1i-miss, and IPC counters for exactly two
representative rows with the same parse-attribution binary:

- `gsoc-2018` for string-body dominance (`match_string_at_quote` 63.9%).
- `marine_ik` for number/container dominance (`match_number_at_digit` 34.2%,
  `consume_container_next` 21.0%).

Prediction to falsify: if R6's branch/front-end suspicion is real, the PMU run
should show elevated branch-miss or L1i-miss attribution at those dominant leaf
PCs, not just high sampled self-time. If the counters are flat or IPC remains
healthy inside those leaves, reject branch/i-cache as the Wave 1 explanation
and route synthesis back to per-byte work attribution inside the string and
number consumers. Do not prescribe a kernel intervention from this R6 report
alone.
