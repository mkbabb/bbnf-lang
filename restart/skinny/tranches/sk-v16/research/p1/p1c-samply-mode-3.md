# SK-V16 P1-C: Samply Mode III Masking Probes

Pass: S-P1 Profile. Cycle: V16.
Date: 2026-05-28.
Scope: cold per-parse masking probes: `host_call_eager_decode`,
`alternate_scalar_plan`, `cold_first_parse`, and `structural_scan_only`.
Output: this file.
Baseline: SK-V16-open (`5ed43f8e1`).
Host triple: `aarch64-apple-darwin`.
Build flags: release profile with debuginfo; `warmup_iters=0`.
Profile tool: `/Users/mkbabb/.cargo/bin/samply`; offline symbols via `atos -inlineFrames`.
Corpus coverage: 17/17 for each of four modes.

## Section 1 - Method

S-P1 V1 found that mode III could not be executed honestly through the old
binary: the probes existed only as Criterion benches, which would violate the
cold per-parse discipline. Commit `5ed43f8e1` added cold probe modes to
`profile_direct`; runtime behavior and admission gates were untouched.

```sh
cd /Users/mkbabb/Programming/bbnf-skv16-p1/skinny
cargo build --release -p bbnf-bench --bin profile_direct
./target/release/profile_direct 100 <corpus> host_call_eager_decode 0
./target/release/profile_direct 100 <corpus> alternate_scalar_plan 0
./target/release/profile_direct 100 <corpus> cold_first_parse 0
./target/release/profile_direct 100 <corpus> structural_scan_only 0

samply record --no-open --duration 3 -o /tmp/skv16-p1-mode3/profiles/<corpus>-<mode>.json.gz -- \
  ./target/release/profile_direct <iters> <corpus> <mode> 0
```

Run ledgers:

- `/tmp/skv16-p1-mode3/probe-results.tsv`
- `/tmp/skv16-p1-mode3/samply-artifacts.tsv`
- `/tmp/skv16-p1-mode3/samply-profile-top20-inline.tsv`
- `/tmp/skv16-p1-mode3/samply-mode-top20-inline.tsv`

## Section 2 - Findings

Average cycles per byte:

| Mode | Mean c/B |
|---|---:|
| structural_scan_only | 5.589994 |
| alternate_scalar_plan | 7.980442 |
| host_call_eager_decode | 8.921382 |
| cold_first_parse | 9.371465 |

Worst masking rows:

| Corpus | Mode | Mbps | c/B |
|---|---|---:|---:|
| unicode_mixed | cold_first_parse | 1480.548 | 19.093185 |
| y_string_unicode | cold_first_parse | 1531.562 | 18.625012 |
| y_string_unicode | host_call_eager_decode | 1585.552 | 17.960892 |
| unicode_mixed | host_call_eager_decode | 1608.118 | 17.532932 |
| unicode_escapes | cold_first_parse | 1843.354 | 15.656950 |
| unicode_escapes | host_call_eager_decode | 1843.567 | 15.568485 |

Mode-level top leaves:

| Mode | Dominant leaf |
|---|---|
| host_call_eager_decode | tape cursor/value lookup, string body range, UTF-8 validation, eager view walk |
| alternate_scalar_plan | serde allocation, decimal parse, string escape parse, hash table insert |
| cold_first_parse | clone/from_utf8 plus same view/string-body costs as eager decode |
| structural_scan_only | scalar structural scan tail plus local FNV checksum harness |

Full top-20 tables are in `/tmp/skv16-p1-mode3/samply-profile-top20-inline.tsv`.

## Section 3 - Delta Vs SK-V15

Mode III is diagnostic only. It does not change the 51 JSON admitted rows and
does not convert any CSS row from `OPEN` to `ADMITTED`.

## Section 4 - Anomalies And Masking Signals

`structural_scan_only` is cheaper than eager decode and cold-first parse, but
it measures a different plane. It is a masking diagnostic, not a comparator
for product-plane admission. The `alternate_scalar_plan` rows are dominated by
serde allocation and DOM construction; they are useful as a lower-bound
warning against product-plane shortcuts, not a Track 1 primitive source.

The x86-only PEXT probe is out of scope for Apple M5 Max and was not run.

## Section 5 - Sources

- `5ed43f8e1`
- `/tmp/skv16-p1-mode3/probe-results.tsv`
- `/tmp/skv16-p1-mode3/samply-artifacts.tsv`
- `/tmp/skv16-p1-mode3/samply-profile-top20-inline.tsv`
- `/tmp/skv16-p1-mode3/samply-mode-top20-inline.tsv`
