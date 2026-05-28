# SK-V16 P1-A: Samply Mode I Parse-Only

Pass: S-P1 Profile. Cycle: V16.
Date: 2026-05-28.
Scope: cold per-parse `parse_only_track1` over all seventeen JSON corpora.
Output: this file.
Baseline: SK-V16-open (`5ed43f8e1` profiling-tool anchor; baseline docs at `dafe288dd`).
Host triple: `aarch64-apple-darwin`.
Build flags: release profile with debuginfo; `warmup_iters=0`.
Profile tool: `/Users/mkbabb/.cargo/bin/samply`; offline symbols via `atos -inlineFrames`.
Corpus coverage: 17/17.

## Section 1 - Method

```sh
git worktree add --detach /Users/mkbabb/Programming/bbnf-skv16-p1 dafe288dd
git -C /Users/mkbabb/Programming/bbnf-skv16-p1 checkout 5ed43f8e1
cd /Users/mkbabb/Programming/bbnf-skv16-p1/skinny
cargo build --release -p bbnf-bench --bin profile_direct

# Probe matrix, single binary, no warmup.
./target/release/profile_direct 500 <corpus> parse_only_track1 0

# Per-corpus profile, repeated for all 17 corpora.
samply record --no-open --duration 3 -o /tmp/skv16-p1/profiles/<corpus>-parse_only_track1.json.gz -- \
  ./target/release/profile_direct <iters> <corpus> parse_only_track1 0

# Symbolication.
atos -inlineFrames -o /Users/mkbabb/Programming/bbnf-skv16-p1/skinny/target/release/profile_direct \
  -arch arm64 0x100000000+<profile_offset> | /Users/mkbabb/.cargo/bin/rustfilt
```

Run ledgers:

- `/tmp/skv16-p1/probe-results.tsv`
- `/tmp/skv16-p1/samply-artifacts.tsv`
- `/tmp/skv16-p1/samply-profile-top20-inline.tsv`
- `/tmp/skv16-p1/samply-mode-top20-inline.tsv`

## Section 2 - Findings

Full top-20 self-time tables are in `/tmp/skv16-p1/samply-profile-top20-inline.tsv`.
The table below records the top self-time inline leaf for each corpus.

| Corpus | Samples | Top % | Top inline leaf | Profile |
|---|---:|---:|---|---|
| twitter | 1205 | 4.48 | `parse_that_regex::skip_ascii_whitespace (lib.rs:121)` | `/tmp/skv16-p1/profiles/twitter-parse_only_track1.json.gz` |
| citm_catalog | 1228 | 4.97 | `runtime::generated_json::generated::parse_only_consume_array_next (generated.rs:0)` | `/tmp/skv16-p1/profiles/citm_catalog-parse_only_track1.json.gz` |
| canada | 1295 | 4.94 | `parse_that_regex::number::is_two_ascii_digits (mod.rs:172)` | `/tmp/skv16-p1/profiles/canada-parse_only_track1.json.gz` |
| apache_builds | 1244 | 9.16 | `parse_that_regex::first_control_byte (lib.rs:710)` | `/tmp/skv16-p1/profiles/apache_builds-parse_only_track1.json.gz` |
| github_events | 895 | 3.80 | `parse_that_regex::skip_string_plain_trusted (lib.rs:0)` | `/tmp/skv16-p1/profiles/github_events-parse_only_track1.json.gz` |
| update_center | 1222 | 4.83 | `parse_that_regex::first_control_byte (lib.rs:710)` | `/tmp/skv16-p1/profiles/update_center-parse_only_track1.json.gz` |
| mesh | 1198 | 8.43 | `parse_that_regex::skip_ascii_whitespace (lib.rs:121)` | `/tmp/skv16-p1/profiles/mesh-parse_only_track1.json.gz` |
| random | 1223 | 7.52 | `parse_that_regex::first_control_byte (lib.rs:710)` | `/tmp/skv16-p1/profiles/random-parse_only_track1.json.gz` |
| gsoc-2018 | 1189 | 7.74 | `parse_that_regex::skip_string_plain_trusted (lib.rs:0)` | `/tmp/skv16-p1/profiles/gsoc-2018-parse_only_track1.json.gz` |
| marine_ik | 1233 | 3.41 | `runtime::generated_json::generated::parse_only_value_iterative (mod.rs:0)` | `/tmp/skv16-p1/profiles/marine_ik-parse_only_track1.json.gz` |
| instruments | 1318 | 4.02 | `parse_that_regex::skip_ascii_whitespace (lib.rs:121)` | `/tmp/skv16-p1/profiles/instruments-parse_only_track1.json.gz` |
| numbers | 1157 | 8.90 | `parse_that_regex::number::scan_digit_run (mod.rs:0)` | `/tmp/skv16-p1/profiles/numbers-parse_only_track1.json.gz` |
| unicode_mixed | 1124 | 12.28 | `parse_that_regex::validate_string_escape (lib.rs:0)` | `/tmp/skv16-p1/profiles/unicode_mixed-parse_only_track1.json.gz` |
| unicode_escapes | 968 | 11.05 | `parse_that_regex::read_hex_unit_scalar (lib.rs:1100)` | `/tmp/skv16-p1/profiles/unicode_escapes-parse_only_track1.json.gz` |
| unicode_basic | 1174 | 6.22 | `memchr aarch64 NEON movemask (vector.rs:410)` | `/tmp/skv16-p1/profiles/unicode_basic-parse_only_track1.json.gz` |
| distinct_values | 1261 | 8.33 | `parse_that_regex::first_control_byte (lib.rs:711)` | `/tmp/skv16-p1/profiles/distinct_values-parse_only_track1.json.gz` |
| y_string_unicode | 957 | 9.30 | `parse_that_regex::hex_nibble (lib.rs:1111)` | `/tmp/skv16-p1/profiles/y_string_unicode-parse_only_track1.json.gz` |

The mode-level top leaves are parser-neutral primitives: whitespace scan,
plain-string scan, escape validation, numeric digit scan, generated iterative
parse frame handling, and aarch64 memchr/movemask support in string scanning.
These are valid S-P2 antecedents. They do not authorize native-SIMD work by
themselves; S-P3 must bind a scalar oracle and same-wave consumer before any
SIMD row opens.

## Section 3 - Delta Vs SK-V15

SK-V16 re-profiled the SK-V15 admitted JSON baseline; it did not change
admission status. The probe matrix recorded these parse-only c/B cliffs:

| Fast / slow | Corpus | Mbps | c/B |
|---|---|---:|---:|
| fastest | gsoc-2018 | 50381.939 | 0.682087 |
| fast | citm_catalog | 35337.518 | 0.973375 |
| fast | github_events | 28724.807 | 1.199009 |
| slow | unicode_escapes | 13447.711 | 2.460286 |
| slow | unicode_mixed | 11991.258 | 2.864197 |
| slowest | y_string_unicode | 8760.403 | 3.828575 |

Outcome class: `A/AUDIT-SUSTAINED` for all 17 parse-only JSON rows. Admit
effect: none.

## Section 4 - Anomalies And Masking Signals

The raw Samply JSON has `symbolicated=false`; this artifact therefore cites
offline `atos -inlineFrames` output instead of claiming in-file Samply symbol
resolution. The source file:line claims are still executable because the
profile binary was built with debug info and `atos -inlineFrames` resolves the
Mach-O offsets.

Unicode rows are the parse-only cliff. Numeric-heavy `canada` is not the worst
parse-only row after SK-V15; it becomes a product-plane cliff in P1-B.

## Section 5 - Sources

- `/tmp/skv16-p1/probe-results.tsv`
- `/tmp/skv16-p1/samply-artifacts.tsv`
- `/tmp/skv16-p1/samply-profile-top20-inline.tsv`
- `/tmp/skv16-p1/samply-mode-top20-inline.tsv`
- `/Users/mkbabb/Programming/bbnf-skv16-p1/skinny/target/release/profile_direct`
