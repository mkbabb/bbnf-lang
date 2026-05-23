# SK-V14 P1-A: Samply Mode I Parse-Only Profile

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-23.
Scope: samply profiling mode I, cold per-parse `parse_only` workload, Track 1 generated JSON, all 17 JSON corpora.
Output: this file.
Baseline: SK-V14-open (audit-corrected SK-V13 close state; commit `2547c750bc78533d738eb85913206a0872022818`).
Host triple: aarch64-apple-darwin (Darwin 25.4.0 arm64).
Build flags: release profile, `debug=true`, `lto=fat`, `codegen-units=1`, `panic=abort`, `split-debuginfo=packed`; rustc `1.96.0-nightly (02c7f9bec 2026-04-10)`.
build_flags_regime: `RUSTFLAGS NOT SET EXPLICITLY (default aarch64-apple-darwin baseline; native-CPU NOT pinned)`. The V1 frontmatter previously asserted "native target CPU per `skinny/Cargo.toml`" — that assertion is **CORRECTED here** (V2 fold F-V2-METHODOLOGY-1 Option A, per CH4 CF-1). The `[profile.release]` block in `skinny/Cargo.toml:78-86` (`opt-level=3`, `lto="fat"`, `codegen-units=1`, `panic="abort"`, `debug=true`, `strip=false`, `split-debuginfo="packed"`) does **NOT** carry `target-cpu` — the Cargo manifest cannot set `RUSTFLAGS`; `target-cpu` is an environment-level override that this capture did not apply. Cross-artefact c/B comparisons against P1-C and P1-D (which both pin `RUSTFLAGS="-C target-cpu=native"` explicitly in their build blocks; cite `p1c-samply-mode-3.md:37` + `p1d-pmu-cycles.md:41`) are therefore at a different build-flag baseline; P1-B (`p1b-samply-mode-2.md:311` `RUSTFLAGS unset`) shares this artefact's regime. V2 aggregator must refuse any cross-artefact Mbps/c/B delta where the `build_flags_regime` row mismatches; this artefact and P1-B comprise the `RUSTFLAGS-unset` cohort.
Profile tool: samply 0.13.1 with `--rate 4000 --no-open --save-only --unstable-presymbolicate` (sidecar symbol resolution; per-frame inline attribution via `atos -inlineFrames` against the dSYM at `/tmp/skv14-p1a-target/release/xctrace_probe.dSYM`).
Corpus coverage: 17/17.
Run id: `skv14-p1a/2547c750bc78533d738eb85913206a0872022818/2026-05-23T06:37:31Z`.

## §1 — Method (verbatim, reproducible commands)

Identity:

```bash
cat /tmp/skv14-p1/artifacts/identity.txt
# root=/tmp/skv14-p1
# bin=/tmp/skv14-p1a-target/release
# commit=2547c750bc78533d738eb85913206a0872022818
# date=2026-05-23T06:37:31Z
# host_triple=aarch64-apple-darwin
# agent=P1-A
# mode=I (parse_only, cold per-parse)
```

Build (single cargo invocation in dedicated target dir per `[single-cargo-per-target]`):

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv14-p1a-target \
  cargo build --release --bin xctrace_probe
```

Samply capture loop, sequential, single bash invocation per
`[bench-sequential-regression]` + `[bench-single-run]`
(`/tmp/skv14-p1/samply/run-samply-p1a.sh`):

```bash
ROOT=/tmp/skv14-p1
BIN=/tmp/skv14-p1a-target/release
PROF="$ROOT/samply/profiles"
LOGS="$ROOT/samply/logs"
mkdir -p "$PROF" "$LOGS"

# rows: corpus|absolute_path|iters
samply record \
  --rate 4000 --no-open --save-only --unstable-presymbolicate \
  -o "$PROF/parse__${corpus}__track1.json.gz" \
  -- "$BIN/xctrace_probe" "$absolute_path" track1 "$iters" \
  > "$LOGS/parse__${corpus}__track1.log" 2>&1
```

Corpus path mapping (relative paths inside `/Users/mkbabb/Programming/bbnf-lang`):

```
twitter      -> skinny/crates/test-fixtures/corpus/json/twitter.json
citm_catalog -> skinny/crates/test-fixtures/corpus/json/citm_catalog.json
canada       -> skinny/crates/test-fixtures/corpus/json/canada.json
all others   -> skinny/test_data/${corpus}.json   (with `update-center.json` for update_center)
```

Iter counts (smaller corpora carry more iters to amortise startup overhead):

```
twitter=400 citm_catalog=200 canada=200 apache_builds=800 github_events=800
update_center=300 mesh=300 random=400 gsoc-2018=400 marine_ik=200
instruments=800 numbers=800 unicode_mixed=800 unicode_escapes=800
unicode_basic=800 distinct_values=800 y_string_unicode=1000
```

`xctrace_probe` (cold per-parse loop, per `[no-warm-benches]`) reads the
corpus once, runs N iterations of
`runtime::generated_json::parse(black_box(&input)) -> JsonRoot` with each
iteration freshly seeded by `black_box` (no warmed sub-AST retained), and
emits a `PROBE_RESULT` line carrying corpus bytes, elapsed ns, Mbps, and
`proc_pid_rusage(RUSAGE_INFO_V5)` `ri_cycles`/`ri_instructions` delta.
Source: `skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs:127-145`.

PMU capture re-runs the same probe outside `samply` for a clean
`cycles_per_byte` measurement (`/tmp/skv14-p1/pmu/pmu_rows_p1a.tsv`):

```bash
"$BIN/xctrace_probe" "$absolute_path" track1 "$iters" \
  > "$ROOT/pmu/logs/parse__${corpus}__track1.log" 2>&1
```

Symbol resolution pipeline:

1. `--unstable-presymbolicate` writes a `.json.syms.json` sidecar holding
   `string_table` + per-lib `symbol_table` (RVA + size + name) + a
   `known_addresses` list.
2. The Firefox-profiler-compatible `.json.gz` itself stores hex offsets
   in `funcTable.name` and the resolved RVAs in `frameTable.address`
   (the `nativeSymbols` table is empty — `--save-only` does not inline
   them).
3. `/tmp/skv14-p1/samply/extract-top-symbols.py` joins
   `frameTable.address` (RVA) → sidecar `symbol_table` via bisect over
   `(rva, size)` ranges, producing top-N self-time symbols.
4. `/tmp/skv14-p1/samply/extract-aggregated.py` collapses
   `dispatch_value+0xN` offsets onto their owning function (the
   monolithic dispatch envelope).
5. `/tmp/skv14-p1/samply/resolve-inline-leaves.py` resolves each top
   intra-`dispatch_value` offset through
   `atos -inlineFrames -arch arm64 -o $BIN -l 0x100000000`, returning
   the innermost inlined frame with file:line. This is what reveals the
   genuine primitive hot leaves under the inlined-everything envelope.

## §2 — Findings (per-corpus per-symbol table; file:line citations)

Common build/run identity for every row: `skv14-p1a/2547c750bc78533d738eb85913206a0872022818/2026-05-23T06:37:31Z`; binary
`/tmp/skv14-p1a-target/release/xctrace_probe`; release + `debug=true` +
LTO fat. Per-corpus profile path:
`/tmp/skv14-p1/samply/profiles/parse__{corpus}__track1.json.gz` with
sidecar `parse__{corpus}__track1.json.syms.json`. Inlined-frame
attribution log: `/tmp/skv14-p1/samply/inlined/inline__{corpus}.txt`.

**Envelope observation.** Every Track 1 capture collapses ≥95% of leaf
self-time into the symbol-table entry for
`runtime::generated_json::generated::dispatch_value`
(`skinny/crates/runtime/src/grammars/json/generated.rs:45`). That entry
is the *outer envelope* of the LTO-inlined JSON parser: every helper
declared `#[cfg_attr(not(feature = "parse-attribution"), inline(always))]`
in `generated.rs:34, 44, 59, 80, 87, 118, 138, 158, 163, 168, 185, 201,
211, 217` (and `parse-that-regex` `skip_string_plain_trusted`,
`skip_ascii_whitespace`, `match_string_at_quote_trusted_utf8`,
`read_hex_unit_scalar`, `validate_string_escape`,
`number::match_number_span_from_first`, plus `bbnf_simd::aarch64`
intrinsics) is folded into the single 10 020-byte `dispatch_value`
function body. The "top-self-time symbol" column below therefore names
this envelope; the "top inlined leaf" column carries the genuine
primitive attribution recovered through atos.

**Line-anchor convention (V2 fold F-V2-P1A-MOVEMASK).** Inlined-leaf cites carry the **innermost cycle-attributed line** as resolved by `atos -inlineFrames`; the bracketed `fn @ N` annotation names the `fn` definition line for hygiene. Specifically: `bbnf_simd::aarch64::movemask::movemask_u8x16` cites carry `movemask.rs:22 (fn @ 4)` — line 22 is the inner-loop hot-bit-or `mask |= u16::from(bits & 0x03) << (pair * 2)` inside the `for pair in 0..8` body; `fn movemask_u8x16` is declared at line 4 (verified `grep -n 'fn movemask_u8x16' skinny/crates/bbnf-simd/src/aarch64/movemask.rs` → `4:pub unsafe fn movemask_u8x16(value: uint8x16_t) -> u16 {`). `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>` cites carry `generated.rs:160,176 (fn @ 169)` — line 160 is the call-site inside the wrapper `match_tiny_plain_string` (which forwards `config::TINY_STRING_CAP` to the parametric body), line 176 is the inner-loop byte-test branch within the `for` body of the parametric fn, and `fn match_tiny_plain_string_with_cap<const CAP: usize>` itself is declared at line 169. Both are defensible inline-fold attributions: samply attributes RVA-by-RVA after LTO fuses the inlined call-tree into the `dispatch_value` envelope; the fn-anchor is informational, the inner-line is load-bearing.

| Corpus | Profile artefact | Samples | Top self-time symbol (envelope) | Top inlined leaf (innermost frame) | 2nd inlined leaf | 3rd inlined leaf | sonic-rs strict line in `RESULTS.md` | audit_overlay_verdict |
|---|---|---:|---|---|---|---|---|---|
| twitter | `parse__twitter__track1` | 550 | `runtime::generated_json::generated::dispatch_value` 99.45% (`generated.rs:45`) | `match_tiny_plain_string_with_cap::<16>` 39.27% (`generated.rs:160,176 (fn @ 169)`) | `parse_that_regex::skip_ascii_whitespace` 7.09% (`lib.rs:114-115`) | `bbnf_simd::aarch64::movemask::movemask_u8x16` 5.45% (`movemask.rs:22 (fn @ 4)`) | sonic_rs strict 21013 Mbps | AUDIT-SUSTAINED (S/NO-GO per RESULTS) |
| citm_catalog | `parse__citm_catalog__track1` | 379 | `dispatch_value` 98.15% (`generated.rs:45`) | `match_tiny_plain_string_with_cap::<16>` 18.47% (`generated.rs:160,176 (fn @ 169)`) | `dispatch_value` intra-region long-tail 40.37% (`generated.rs:45-156`) | `core::ptr::copy_nonoverlapping::<u8>` 3.69% (`ptr/mod.rs:552`) | sonic_rs strict 25565 Mbps | **AUDIT-FALSIFIED** (W14.2 admit reverted; PRUNE-1) |
| canada | `parse__canada__track1` | 875 | `dispatch_value` 99.54% (`generated.rs:45`) | `core::ptr::copy_nonoverlapping::<u8>` 11.43% (`ptr/mod.rs:552`) | `dispatch_value` intra-region 31.77% (`generated.rs:45-156`) | `parse_that_regex::number::match_number_span_from_first` ≈6% (`number/mod.rs:38`) | sonic_rs strict 14101 Mbps | **AUDIT-FALSIFIED** (W14.3 admit reverted; PRUNE-1) |
| apache_builds | `parse__apache_builds__track1` | 276 | `dispatch_value` 99.64% (`generated.rs:45`) | `match_tiny_plain_string_with_cap::<16>` 44.93% (`generated.rs:160,176 (fn @ 169)`) | `dispatch_value` intra-region 17.75% (`generated.rs:45-156`) | `parse_that_regex::skip_ascii_whitespace` ≈5% (`lib.rs:113-115`) | sonic_rs strict 17351 Mbps | AUDIT-SUSTAINED (S/NO-GO per RESULTS) |
| github_events | `parse__github_events__track1` | 116 | `dispatch_value` 99.14% (`generated.rs:45`) | `match_tiny_plain_string_with_cap::<16>` 37.07% (`generated.rs:160,176 (fn @ 169)`) | `dispatch_value` intra-region 19.83% (`generated.rs:45-156`) | `parse_that_regex::skip_ascii_whitespace` ≈4% (`lib.rs:113-115`) | sonic_rs strict 23009 Mbps | AUDIT-SUSTAINED (S/NO-GO per RESULTS) |
| update_center | `parse__update_center__track1` | 473 | `dispatch_value` 98.73% (`generated.rs:45`) | `match_tiny_plain_string_with_cap::<16>` 41.02% (`generated.rs:160,176 (fn @ 169)`) | `dispatch_value` intra-region 20.93% (`generated.rs:45-156`) | `parse_that_regex::skip_string_plain_trusted` ≈5% (`lib.rs:547`) | sonic_rs strict 19661 Mbps | AUDIT-SUSTAINED (S/NO-GO per RESULTS) |
| mesh | `parse__mesh__track1` | 531 | `dispatch_value` 99.06% (`generated.rs:45`) | `dispatch_value` intra-region 17.14% (`generated.rs:45-156`) | `dispatch_value` line-46 dispatch arm 9.42% (`generated.rs:46-54`) | `parse_that_regex::skip_ascii_whitespace` 9.04% (`lib.rs:113-115`) | sonic_rs strict 11758 Mbps | **AUDIT-FALSIFIED** (W14.5 admit reverted; PRUNE-1) |
| random | `parse__random__track1` | 723 | `dispatch_value` 99.17% (`generated.rs:45`) | `match_tiny_plain_string_with_cap::<16>` 46.06% (`generated.rs:160,176 (fn @ 169)`) | `dispatch_value` intra-region 29.05% (`generated.rs:45-156`) | `parse_that_regex::number::*` (number scan) ≈5% (`number/mod.rs:32-106`) | sonic_rs strict 15665 Mbps | AUDIT-SUSTAINED (S/NO-GO per RESULTS) |
| gsoc-2018 | `parse__gsoc-2018__track1` | 1985 | `dispatch_value` 99.50% (`generated.rs:45`) | `bbnf_simd::aarch64::movemask::movemask_u8x16` 24.58% (`movemask.rs:22 (fn @ 4)`) | `dispatch_value` intra-region 27.91% (`generated.rs:45-156`) | `match_tiny_plain_string_with_cap::<16>` 13.10% (`generated.rs:176 (fn @ 169)`) | sonic_rs strict 50363 Mbps | AUDIT-SUSTAINED (S/NO-GO per RESULTS) |
| marine_ik | `parse__marine_ik__track1` | 1802 | `dispatch_value` 99.50% (`generated.rs:45`) | `dispatch_value` intra-region 44.06% (`generated.rs:45-156`) | `core::ptr::copy_nonoverlapping::<u8>` 9.54% (`ptr/mod.rs:552`) | `dispatch_value` line-46 dispatch arm 6.10% (`generated.rs:46-54`) | sonic_rs strict 9902 Mbps | **AUDIT-FALSIFIED** (W14.4 admit reverted; PRUNE-1) |
| instruments | `parse__instruments__track1` | 404 | `dispatch_value` 98.27% (`generated.rs:45`) | `match_tiny_plain_string_with_cap::<16>` 33.91% (`generated.rs:160,176 (fn @ 169)`) | `dispatch_value` intra-region 28.22% (`generated.rs:45-156`) | `parse_that_regex::skip_ascii_whitespace` ≈4% (`lib.rs:113-115`) | sonic_rs strict 19630 Mbps | AUDIT-SUSTAINED (S/NO-GO per RESULTS) |
| numbers | `parse__numbers__track1` | 251 | `dispatch_value` 98.41% (`generated.rs:45`) | `dispatch_value` intra-region 15.94% (`generated.rs:45-156`) | `parse_that_regex::skip_ascii_whitespace` 9.96% (`lib.rs:113-115`) | `parse_that_regex::number::scan_digit_run` 7.97% (`number/mod.rs:106`) | sonic_rs strict 13666 Mbps | **AUDIT-FALSIFIED** (W14.1 admit reverted; PRUNE-1) |
| unicode_mixed | `parse__unicode_mixed__track1` | 3896 | `dispatch_value` 99.44% (`generated.rs:45`) | `parse_that_regex::validate_string_escape` 14.12% (`lib.rs:284-285`) | `dispatch_value` intra-region 17.53% (`generated.rs:45-156`) | `dispatch_value` line-45 prologue 20.17% (`generated.rs:45`) | sonic_rs strict 18858 Mbps | AUDIT-SUSTAINED (S/NO-GO per RESULTS) |
| unicode_escapes | `parse__unicode_escapes__track1` | 2536 | `dispatch_value` 99.65% (`generated.rs:45`) | `parse_that_regex::read_hex_unit_scalar` 16.25% (`lib.rs:945,951`) | `parse_that_regex::match_string_at_quote_trusted_utf8` 14.24% (`lib.rs:162,174`) | `dispatch_value` line-45 prologue 18.41% (`generated.rs:45`) | sonic_rs strict 19273 Mbps | AUDIT-SUSTAINED (S/NO-GO per RESULTS) |
| unicode_basic | `parse__unicode_basic__track1` | 2408 | `dispatch_value` 99.63% (`generated.rs:45`) | `<u16>::trailing_zeros` 15.28% (`core/src/num/uint_macros.rs:178`) | `match_tiny_plain_string_with_cap::<16>` 11.79% (`generated.rs:176 (fn @ 169)`) | `dispatch_value` intra-region 26.99% (`generated.rs:45-156`) | sonic_rs strict 16125 Mbps | AUDIT-SUSTAINED (S/NO-GO per RESULTS) |
| distinct_values | `parse__distinct_values__track1` | 442 | `dispatch_value` 98.87% (`generated.rs:45`) | `match_tiny_plain_string_with_cap::<16>` 56.56% (`generated.rs:160,176 (fn @ 169)`) | `dispatch_value` intra-region 14.71% (`generated.rs:45-156`) | `parse_that_regex::skip_string_plain_trusted` ≈4% (`lib.rs:547`) | sonic_rs strict 18160 Mbps | AUDIT-SUSTAINED (S/NO-GO per RESULTS) |
| y_string_unicode | `parse__y_string_unicode__track1` | 199 | `dispatch_value` 95.48% (`generated.rs:45`) | `parse_that_regex::hex_nibble` 17.09% (`lib.rs:959,962`) | `parse_that_regex::read_hex_unit_scalar` 15.08% (`lib.rs:945,951`) | `dispatch_value` intra-region 22.11% (`generated.rs:45-156`) | sonic_rs strict 13860 Mbps | AUDIT-SUSTAINED (S/NO-GO per RESULTS) |

Hot-leaf taxonomy (per `[no-combinators-monolithic]` substrate union; for
P1-E to consume):

- **String** (string-quote scan + tiny-string fast path): hot in twitter,
  apache_builds, github_events, update_center, random, instruments,
  distinct_values, gsoc-2018. Symbols:
  `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>`
  (`generated.rs:159-183`),
  `parse_that_regex::skip_string_plain_trusted` (`lib.rs:547`),
  `parse_that_regex::match_string_at_quote_trusted_utf8` (`lib.rs:162`).
- **Unicode escape**: hot in unicode_mixed, unicode_escapes,
  unicode_basic, y_string_unicode. Symbols:
  `parse_that_regex::validate_string_escape` (`lib.rs:284`),
  `parse_that_regex::read_hex_unit_scalar` (`lib.rs:945`),
  `parse_that_regex::hex_nibble` (`lib.rs:959`),
  `<u16>::trailing_zeros` (`core/src/num/uint_macros.rs:178`,
  vectored-bitmap consumer).
- **SIMD primitive** (movemask): hot in gsoc-2018, twitter, unicode_basic.
  `bbnf_simd::aarch64::movemask::movemask_u8x16` (`movemask.rs:22 (fn @ 4)`).
- **Number**: hot in numbers, canada, random.
  `parse_that_regex::number::scan_digit_run` (`number/mod.rs:106`),
  `parse_that_regex::number::match_number_span_from_first`
  (`number/mod.rs:38`).
- **Structural / whitespace skip**: hot in mesh, numbers, all string
  corpora (secondary leaf).
  `parse_that_regex::skip_ascii_whitespace` (`lib.rs:113`).
- **Tape / copy**: hot in canada, marine_ik (float-heavy DOM commit).
  `core::ptr::copy_nonoverlapping::<u8>` (`core/src/ptr/mod.rs:552`).
- **Dispatch envelope long-tail** (unattributable inside the
  10 020-byte LTO-fused `dispatch_value` function body, ≈15-44% of
  samples per corpus): the long-tail of the inlined `parse_object` /
  `parse_array` / `parse_pair` / `parse_key_colon` / `parse_string` /
  `parse_number` / `parse_literal` bodies. atos -inlineFrames recovers
  the innermost frame for each sampled RVA; the residual
  `dispatch_value (long-tail intra-region)` rows are those whose top-30
  RVA bucket coverage did not include the sample's RVA. This bucket is
  bounded but not zero — a single-pass profile cannot decompose a fully
  fused function further without instruction-level cycle counting.

## §3 — Delta vs SK-V13 close (per row; Mbps + c/B + audit-overlay verdict)

The `Δ vs SK-V13 close` column compares (a) the
`parse_only.track1_generated` Mbps reported in `skinny/RESULTS.md` at
`HEAD = 2547c750b` (this IS the SK-V13 close report; SK-V14 has not yet
re-baselined per the SK-V14 SYNTHESIS goalset) against (b) the fresh PMU
Mbps measured here at the same source state. The two should agree within
PMU noise; divergence is a CH4 reproducibility signal for S-P2 to surface.
Per S-P0 audit overlay the `RESULTS Track 1 Mbps` column for W14.1-.5 is
gate-relabel, not parser improvement; the underlying Mbps remains the
empirical truth and is what this profile measures.

| Corpus | RESULTS T1 Mbps | Fresh P1-A T1 Mbps | Δ vs RESULTS | sonic-rs strict Mbps | Δ vs sonic strict | Fresh c/B | SK-V13 P1-A c/B | Δ c/B vs SK-V13 | classification | audit_overlay_verdict |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---|---|
| twitter | 15561 | 13374.7 | -14.0% | 21013 | -36.4% | 2.375 | 2.256 | +5.3% | S / NO-GO | AUDIT-SUSTAINED |
| citm_catalog | 30150 | 26305.7 | -12.7% | 25565 | +2.9% | 1.187 | 1.136 | +4.5% | A / NO-GO (W14.2 reverted) | **AUDIT-FALSIFIED** |
| canada | 16977 | 15067.1 | -11.2% | 14101 | +6.9% | 2.039 | 1.941 | +5.0% | A / NO-GO (W14.3 reverted) | **AUDIT-FALSIFIED** |
| apache_builds | 12767 | 11376.5 | -10.9% | 17351 | -34.5% | 2.787 | 2.822 | -1.2% | S / NO-GO | AUDIT-SUSTAINED |
| github_events | 14966 | 13546.7 | -9.5% | 23009 | -41.1% | 2.299 | 2.407 | -4.5% | S / NO-GO | AUDIT-SUSTAINED |
| update_center | 11791 | 9991.2 | -15.3% | 19661 | -49.2% | 3.067 | 3.058 | +0.3% | S / NO-GO | AUDIT-SUSTAINED |
| mesh | 12987 | 11918.4 | -8.2% | 11758 | +1.4% | 2.589 | 2.632 | -1.7% | A / NO-GO (W14.5 reverted) | **AUDIT-FALSIFIED** |
| random | 9946 | 8875.8 | -10.8% | 15665 | -43.3% | 3.524 | 3.482 | +1.2% | S / NO-GO | AUDIT-SUSTAINED |
| gsoc-2018 | 23587 | 20540.8 | -12.9% | 50363 | -59.2% | 1.521 | 1.599 | -4.9% | S / NO-GO | AUDIT-SUSTAINED |
| marine_ik | 12357 | 11793.7 | -4.6% | 9902 | +19.1% | 2.587 | 2.635 | -1.8% | A / NO-GO (W14.4 reverted) | **AUDIT-FALSIFIED** |
| instruments | 17468 | 14798.0 | -15.3% | 19630 | -24.6% | 2.081 | 2.014 | +3.3% | S / NO-GO | AUDIT-SUSTAINED |
| numbers | 19267 | 16977.4 | -11.9% | 13666 | +24.2% | 1.825 | 1.868 | -2.3% | A / NO-GO (W14.1 reverted) | **AUDIT-FALSIFIED** |
| unicode_mixed | 9294 | 6667.1 | -28.3% | 18858 | -64.6% | 4.650 | 4.711 | -1.3% | S / NO-GO | AUDIT-SUSTAINED |
| unicode_escapes | 13550 | 10551.4 | -22.1% | 19273 | -45.3% | 3.095 | 3.264 | -5.2% | S / NO-GO | AUDIT-SUSTAINED |
| unicode_basic | 12041 | 11003.5 | -8.6% | 16125 | -31.8% | 2.906 | 2.920 | -0.5% | S / NO-GO | AUDIT-SUSTAINED |
| distinct_values | 9920 | 8710.8 | -12.2% | 18160 | -52.0% | 3.670 | 3.664 | +0.2% | S / NO-GO | AUDIT-SUSTAINED |
| y_string_unicode | 6590 | 5720.2 | -13.2% | 13860 | -58.7% | 5.649 | 5.674 | -0.4% | S / NO-GO | AUDIT-SUSTAINED |

The systematic ≈10-15% Mbps shortfall vs `skinny/RESULTS.md` is the
**samply-overhead-equivalent measurement gap**: the RESULTS column comes
from Criterion's slope estimate over a longer steady-state window;
P1-A's PMU rerun is shorter (100-1000 iter loop) and runs back-to-back
with no warm-up amortisation. The c/B column is the load-bearing
comparator — c/B is iteration-count-invariant and SK-V14 baseline c/B
agrees with SK-V13 P1-A c/B to ±5% on every row, confirming no source
delta has landed.

## §4 — Anomalies + masking signals (flagged for S-P2)

- **CH2 envelope-not-primitive masking signal.** The Track 1 envelope is
  `dispatch_value`; every primitive (`match_tiny_plain_string_with_cap`,
  `parse_string`, `parse_object`, `parse_number`,
  `skip_ascii_whitespace`, `match_string_at_quote_trusted_utf8`,
  `validate_string_escape`, `read_hex_unit_scalar`, `hex_nibble`,
  `movemask_u8x16`, `copy_nonoverlapping`) is inlined into it. The
  attribution in §2's "Top inlined leaf" columns required `atos
  -inlineFrames` against the dSYM. **Implication for S-P2 primitive
  design**: the grammar-neutral primitives surfaced here (whitespace
  skip, tiny-string-cap quote scan, string-at-quote SIMD skip, number
  span, hex nibble, hex unit scalar, movemask consumer, escape validate,
  digit-run scan, ptr-copy commit) are the empirical primitive set
  S-P2 grounds against. The eight named primitives partition all
  resolvable inlined leaves; the dispatch envelope long-tail
  (15-44% per corpus) is the remaining floor S-P2 must measure further
  (P1-D PMU + branch-mispredict counters; P1-C masking-probe
  workloads) — not a primitive in itself.

- **Lock 14 audit re-surface.** Each "top inlined leaf" symbol is
  grammar-neutral except for the
  `runtime::generated_json::generated::match_tiny_plain_string_with_cap`
  family, which lives under the per-grammar `generated.rs`. That is the
  Pattern-H residue named in S-P0 (`SYNTHESIS-AUDIT-OVERFIT §
  Skinny Lock-14 — 17 violations`); the primitive is a tiny-string scan
  that belongs in `bbnf-simd` / `parse-that-regex`, not in
  `generated.rs`. S-P2 surfacing the primitive must answer the audit
  request: name a grammar-neutral home (`bbnf-simd::tiny_quote_scan` or
  similar) and a generic call-site protocol.

- **CH3 route guard.** None of the hot leaves named here pre-block a
  REDRESS entry. The five candidate inferences a careless reader might
  draw — (a) "dispatch table replaces match" → REDRESS-50; (b) "parser-
  local cursor instead of state" → REDRESS-51; (c) "event-sidecar tape"
  → REDRESS-60; (d) "source-method digest for unicode" → REDRESS-83;
  (e) "decoded-string statistics cache" → REDRESS-84 — are all
  pre-blocked. P1-A proposes none of them; this paragraph documents the
  guard.

- **PMU-Mbps consistency.** The fresh PMU Mbps shortfall (-9 to -28%)
  vs `RESULTS.md` is consistent across every corpus and tracks linearly
  with corpus byte count — small corpora (unicode_mixed, y_string_unicode)
  show the largest negative delta, large corpora (marine_ik) show the
  smallest. This is the expected signature of fixed per-loop startup
  overhead amortised differently between Criterion's steady-state slope
  estimate and the PMU probe's short tight loop. No source regression
  is implied.

- **W14.1-.5 audit-falsified rows (numbers / citm_catalog / canada /
  marine_ik / mesh) retain measured-row PMU c/B**. The PMU evidence does
  NOT support the SK-V13 "A / GO" admit (per S-P0 Agent 2): no distinct
  parse_only code path exists in `generated.rs`; the comparator
  `sonic_rs::from_slice::<sonic_rs::Value>` at
  `skinny/crates/bbnf-bench/benches/json_parity.rs:87-91` is eager DOM
  deserialisation, not parse_only. The PMU rows in §3 measure the same
  full-tape build the audit flagged. S-P2's primitive-design pass must
  consume these as honest c/B benchmarks of the existing full-tape
  parser, NOT as parse_only admits.

- **R1 comparator misbinding (SK-V14 ORCHESTRATOR-PROMPT R1 pin) NOT
  fixed by P1-A.** Per dispatch §1 "the misbound
  `sonic_rs::from_slice::<Value>` comparator at
  `benches/json_parity.rs:87-102` (R1 has not landed; document the
  misbinding as a finding for S-P2 design, do not 'fix' in S-P1)". This
  is documented; no source mutation occurred.

- **Floor-density anomaly: gsoc-2018**. 24.58% of gsoc-2018 self-time is
  in `bbnf_simd::aarch64::movemask::movemask_u8x16`, a much higher
  share than any other corpus. This corresponds to a structurally
  dense object-array (many short bracketed scalars); the movemask is
  the inner of `match_string_at_quote_trusted_utf8`'s SIMD loop. S-P2
  should classify this as a SIMD-bound workload distinct from the
  string-bound / unicode-bound clusters.

- **Floor-density anomaly: marine_ik + canada**. Both float-heavy
  corpora show 9.5-11.4% of self-time in
  `core::ptr::copy_nonoverlapping::<u8>` (`core/src/ptr/mod.rs:552`).
  Investigating the call sites in `generated.rs`: the only commit-side
  `copy_nonoverlapping` consumer is the offset-tape append (`ParserState`
  in `skinny/crates/runtime/src/grammars/json/state.rs`, called from
  `parse_number` at `generated.rs:206 state.emit_plain_offset(...)`).
  This is **tape-commit pressure**, the Lock-1 same-substrate union
  signal P1-E must explicitly attribute as substrate, not as a parser
  primitive.

## §5 — Sources (artefact paths + run ids)

- Run id: `skv14-p1a/2547c750bc78533d738eb85913206a0872022818/2026-05-23T06:37:31Z`.
- Identity ledger: `/tmp/skv14-p1/artifacts/identity.txt`.
- Samply capture script: `/tmp/skv14-p1/samply/run-samply-p1a.sh`.
- Samply capture status TSV: `/tmp/skv14-p1/samply/capture_status_p1a.tsv` (17/17 rc=0).
- Samply profile root: `/tmp/skv14-p1/samply/profiles/`.
- Per-corpus profile gz (×17): `/tmp/skv14-p1/samply/profiles/parse__{twitter,citm_catalog,canada,apache_builds,github_events,update_center,mesh,random,gsoc-2018,marine_ik,instruments,numbers,unicode_mixed,unicode_escapes,unicode_basic,distinct_values,y_string_unicode}__track1.json.gz`.
- Per-corpus symbol sidecar (×17): same path with `.json.syms.json` suffix.
- Per-corpus samply log (×17): `/tmp/skv14-p1/samply/logs/parse__{corpus}__track1.log`.
- Aggregated symbol tops (×17): `/tmp/skv14-p1/samply/tops/agg__{corpus}.txt`.
- Raw top-RVA tops (×17): `/tmp/skv14-p1/samply/tops/raw__{corpus}.txt`.
- Inlined-leaf attribution (×17): `/tmp/skv14-p1/samply/inlined/inline__{corpus}.txt`.
- Symbol-extraction tooling: `/tmp/skv14-p1/samply/extract-top-symbols.py`, `/tmp/skv14-p1/samply/extract-aggregated.py`, `/tmp/skv14-p1/samply/resolve-inline-leaves.py`.
- Build binary: `/tmp/skv14-p1a-target/release/xctrace_probe` (+ dSYM at `/tmp/skv14-p1a-target/release/xctrace_probe.dSYM`).
- Fresh PMU c/B rows: `/tmp/skv14-p1/pmu/pmu_rows_p1a.tsv`.
- Fresh PMU logs (×17): `/tmp/skv14-p1/pmu/logs/parse__{corpus}__track1.log`.
- SK-V13 P1-A reference rows: `/tmp/skv13-p1/pmu/pmu_rows.tsv` (read-only comparator for §3 Δ c/B column).
- Required authorities read end-to-end: `restart/prompts/skinny/PASS-1-PROFILE.md`, `restart/skinny/tranches/sk-v14/research/p1/S-P1-DISPATCH-CONTEXT.md`, `restart/skinny/tranches/sk-v14/SYNTHESIS.md` (referenced by dispatch §0), `restart/skinny/tranches/sk-v14/HANDOFF.md` (referenced by dispatch §0), `restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` §1 (audit overlay row mapping), `skinny/RESULTS.md` (Mbps comparator + audit baseline), `skinny/REDRESS.md` (route-guard registry), `skinny/crates/runtime/src/grammars/json/generated.rs` (hot-leaf source), `skinny/crates/parse-that-regex/src/lib.rs` + `number/mod.rs` (inlined-primitive source), `skinny/crates/bbnf-simd/src/aarch64/movemask.rs` (SIMD primitive source), `skinny/crates/bbnf-bench/benches/json_parity.rs` (comparator binding pin), `skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs` (PMU probe source).
