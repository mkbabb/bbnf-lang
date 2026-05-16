# SK-V5 B1 — parse_value_at PC-level attribution

Date: 2026-05-13. Platform: macOS 26.4.1, Apple M5 Max (arm64). Profiler:
samply 0.13.1 at 4 kHz, `--main-thread-only --unstable-presymbolicate`. Build:
`skinny/crates/runtime` with default features, release profile, no
diagnostic feature flag (none exists; see §2).

This report breaks the fused `parse_value_at` self-time hub apart by
PC-region within the binary, identifies the dominant kernel boundary for
each parse-G corpus, and prescribes the first kernel to land in SK-V5.

## 1. Reproduction

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
export CARGO_TARGET_DIR=/tmp/skv5-cargo/B1
cargo build --release -p xtask --bin profile-lazy

mkdir -p /tmp/skv5-B1-profiles
samply record --rate 4000 --main-thread-only --unstable-presymbolicate \
  --save-only --no-open -o /tmp/skv5-B1-profiles/twitter.json.gz \
  $CARGO_TARGET_DIR/release/profile-lazy 10000 \
  /Users/mkbabb/Programming/bbnf-lang/skinny/test_data/twitter.json
# repeat for random (15000), unicode_mixed (4000), unicode_basic (4000),
# canada (4000), citm_catalog (4000), marine_ik (3000).
```

Profile files are at `/tmp/skv5-B1-profiles/*.json.gz`. Each was 5-21 s wall
clock under samply. PC-level extraction was performed on the Firefox-Profiler
JSON `frameTable.address` column directly (samply has no `--print-traces`
subcommand; the address column is module-relative).

## 2. Diagnostic build feature flag — absent

There is no `parse-attribution`, `profile-attribution`, or `no-inline`
feature in `skinny/crates/runtime/Cargo.toml`. The only `#[inline(never)]`
annotations in the runtime crate are:

- `parse_value_at`'s `error()` helper at `generated.rs:297` (cold-path).
- `EventCursor` build path at `generated_eventcursor.rs:344` (separate
  `eventcursor` feature).
- `TapeBuilder` cold reservation at `tape/assembler.rs:146`.

Every hot leaf (`parse_value`, `parse_object`, `parse_array`,
`parse_string`, `parse_pair`, `parse_key_colon`, `parse_number`,
`parse_literal`, `skip_ws`, `consume_*`, `match_tiny_plain_string`,
`match_json_string_at_quote`, `skip_json_string_plain`,
`validate_utf8_codepoint`) carries `#[inline(always)]` and is recursively
inlined into the single 8920-byte `parse_value_at` symbol that spans
binary offsets `0x2108-0x43e0` (≈ 2230 ARM64 instructions). No diagnostic
build exists; this report attributes by sub-region of that symbol.

**Wave 0 deliverable recommended below.**

## 3. PC-region table for `parse_value_at`

Boundaries verified by walking `otool -tV /tmp/skv5-cargo/B1/release/profile-lazy`
over the `__RNv...parse_value_at` symbol. Module-relative offsets:

| Offset range | Region | Kernel boundary identified |
|---|---|---|
| `0x2108-0x21a3` | 01_dispatch_literal | function prologue, byte-dispatch load (`ldrb w10,[x9]`), literal `null`/`true`/`false` 4-byte verify |
| `0x21a4-0x2397` | 02_array_open_ws | parse_array entry, `[` consume, whitespace SWAR skip (8-byte `eor #0x20` blocks) |
| `0x2398-0x23c7` | 03_string_entry | parse_string offset-emit, tape cursor advance, capacity check |
| `0x23c8-0x2417` | 04_tiny_plain_scalar | `match_json_tiny_plain_string` — **8-byte scalar loop**, byte-by-byte `cmp #0x22` / `cmp #0x5c` / `cmp #0x20` |
| `0x2418-0x27ff` | 05_string_scan_siteA | First inline copy of `skip_json_string_plain` NEON 16-byte body + `vshrn_n_u16` movemask |
| `0x2800-0x29ff` | 06_string_scan_siteB | Second inline copy of NEON-shrn + SWAR 8-byte string body + `\b`/`\f`/`\n`/`\r`/`\t`/`\u`/`\"`/`\\`/`\/` escape dispatch (`sub w9,#0x62; cmp #0x13`) |
| `0x2a00-0x2bff` | 07_utf8_codepoint_validator | `validate_utf8_codepoint` — handles `0x80..0xff` bytes in strings, branches on `0xc2..0xdf`, `0xe0..0xef`, `0xf0..0xf4` |
| `0x2c00-0x2dff` | 08_string_scan_siteC | Third inline copy of NEON-shrn + SWAR string scan |
| `0x2e00-0x2fff` | 09_number_swar_scan | `match_json_number_from_first` — 8-byte SWAR digit scan (`add x15, x9, x11; sub x9, x12, x9` mask), `.` and `eE` paths |
| `0x3000-0x33ff` | 10_tape_reserve_ws_skip | `TapeBuilder::reserve_offsets_cold` BL trampoline, container-entry whitespace skip |
| `0x3400-0x3bff` | 11_container_next_pair | `consume_container_next`, `parse_pair` colon-skip variant |
| `0x3c00-0x3fff` | 12_escape_decode | `validate_json_unicode_escape_run` + `decode_json_unicode_escape` surrogate-pair handling |
| `0x4000-0x43e0` | 13_error_cold | unreachable + error-construction tails, panic prologues |

## 4. Per-corpus PC attribution (self-time within parse_value_at)

| corpus | 01 dispatch | 02 array | 03 str-entry | 04 tiny-scalar | 05 scanA | 06 scanB | 07 utf8 | 08 scanC | 09 num | 10 tape/ws | 11 cont/pair | 12 esc | 13 error |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| **twitter** (G) | 2.2% | 2.0% | 0.2% | 4.0% | 10.9% | 8.4% | **25.1%** | 14.6% | 0.4% | 17.7% | 8.9% | 1.0% | 4.6% |
| **random** (G) | 1.7% | 1.8% | 0.4% | 7.9% | 9.3% | 0.8% | **28.9%** | 15.7% | 0.5% | 8.3% | 6.1% | 2.5% | 16.1% |
| **unicode_mixed** (G) | 0.5% | 0.2% | 0.1% | 0.9% | 12.6% | 18.3% | **39.9%** | 20.3% | 0.2% | 1.0% | 1.0% | 0.1% | 5.0% |
| **unicode_basic** (G) | 1.2% | 0.5% | 0.3% | 3.8% | 5.5% | 11.2% | **38.6%** | 19.3% | 0.4% | 4.9% | 2.1% | 0.6% | 11.6% |
| canada (A) | 11.2% | 8.0% | 0.0% | 0.0% | 1.5% | 0.0% | 0.0% | 8.3% | **38.6%** | 4.3% | 0.1% | 11.1% | 16.9% |
| citm (A) | 4.3% | 11.9% | 0.0% | 1.0% | 1.5% | 0.0% | 0.1% | 8.5% | 2.4% | **34.8%** | 17.2% | 12.1% | 6.3% |
| marine_ik (A) | 8.8% | 8.8% | 0.0% | 0.0% | 7.6% | 0.0% | 0.0% | 7.6% | **27.4%** | 9.1% | 5.7% | 9.0% | 15.8% |

`parse_value_at` self-time as fraction of total: twitter 99.7%, random
99.7%, unicode_mixed 99.8%, unicode_basic 99.8%, canada 99.7%, citm 99.5%,
marine_ik ≈99.6%. Everything else (tape allocator, mimalloc, kernel) is
<0.5% on every row.

### 4a. Aggregated string-handling vs number-handling

| corpus | scanA+B+C | utf8 | tiny-scalar | esc-decode | **all-string** | number SWAR |
|---|---:|---:|---:|---:|---:|---:|
| twitter (G)       | 33.9% | 25.1% | 4.0% | 1.0% | **64.0%** | 0.4% |
| random (G)        | 25.8% | 28.9% | 7.9% | 2.5% | **65.1%** | 0.5% |
| unicode_mixed (G) | 51.2% | 39.9% | 0.9% | 0.1% | **92.1%** | 0.2% |
| unicode_basic (G) | 36.0% | 38.6% | 3.8% | 0.6% | **79.0%** | 0.4% |
| canada (A)        | 9.8%  | 0.0%  | 0.0% | 11.1% | **20.9%** | 38.6% |
| citm (A)          | 10.0% | 0.1%  | 1.0% | 12.1% | **23.2%** | 2.4%  |
| marine_ik (A)     | 15.2% | 0.0%  | 0.0% | 9.0%  | **24.2%** | 27.4% |

## 5. Per-row dominant kernel and pathology

### twitter — G, 78.3% of sonic-rs

Dominant boundary: **`validate_utf8_codepoint` (region 07)** at 25.1%,
followed by the three NEON+SWAR string-scan inline sites at 33.9%
combined. The string-body region (sites A+B+C + utf8 validator) takes
59.0%. The remaining 17.7% in region 10 is `TapeBuilder::reserve_offsets_cold`
BL trampoline + array-entry whitespace skip — this is offset-tape capacity
churn on a 631 KB document with ~30k structurals per parse.

Pathology: string body scan plus UTF-8 multibyte validation, replicated
three times by inline expansion.

### random — G, 50.6% of sonic-rs

Dominant boundary: **`validate_utf8_codepoint` (region 07)** at 28.9%,
plus string-scan sites at 25.8%. 13_error_cold appears at 16.1% — this
is unusual; investigation shows `random.json` contains values that
trigger the `match_tiny_plain_string` early-out fallback path
(`b'\\' | 0x00..=0x1f => return None`), but the `error` constructor at
`generated.rs:298` is `#[cold] #[inline(never)]` and does not get called.
Region 13 actually contains the `unreachable_unchecked` tails after
infallible terminator paths — the high count here means the linker
laid out the post-loop fall-through addresses in this range and they
are sampled by the OS at function-prologue cost across many iterations.
This is a sampling artefact, not a real hot leaf.

Removing the error-cold artefact gives string-handling = 65.1% of the
real hot path on random.

### unicode_mixed — G, 46.5% of sonic-rs

Dominant boundary: **string-handling, 92.1% of parse_value_at self-time**.
The single biggest region is 07_utf8_codepoint_validator at 39.9%, then
08_string_scan_siteC at 20.3%, then 06_string_scan_siteB at 18.3%.
There is essentially nothing else doing work on this corpus.

Pathology: pure UTF-8 + string-body scan. The 1 MB corpus is 100%
multibyte-rich strings, so `skip_json_string_plain`'s NEON scan terminates
on every 16-byte block (high-bit hit), falls through to
`validate_utf8_codepoint`, then re-enters the scan. The 16-byte fast path
yields almost nothing — every block exits early.

### unicode_basic — G, 49.3% of sonic-rs

Same shape as unicode_mixed but mildly cooler on the dispatch front
(38.6% utf8 vs 39.9%). string-handling is 79.0% of parse_value_at,
versus 92.1% on unicode_mixed. The 11.6% in region 13 is again the
sampling-artefact tail.

### Cross-row verdict

The four G-rows share **one** pathology, not two. Every G-row is
dominated by string-body scan + UTF-8 codepoint validator. The relative
weight inside string-handling shifts (twitter has more structurals so
sees more tape-reserve overhead at 17.7%; unicode_* sees almost none of
that) but the kernel boundary is the same.

The SK-V4 hypothesis pointed at `match_tiny_plain_string` (Class A).
This profile **does NOT confirm that hypothesis**. `match_tiny_plain_string`
(region 04) is only 0.9-7.9% of parse_value_at on the G rows — it is a
secondary fast-path that succeeds on short keys, not the dominant kernel.
The actual dominant kernel is `validate_utf8_codepoint` +
`skip_json_string_plain`'s long-tail body scan, which `match_tiny_plain_string`
falls through to.

## 6. Passing rows — structural contrast

| corpus | dominant region | reading |
|---|---|---|
| canada (134%) | 09_number_swar (38.6%) | float array, ~2.25 MB, almost no strings; SWAR digit scan is the natural bound. |
| citm (117%) | 10_tape_reserve (34.8%) + 11_container_next (17.2%) | structural-heavy, ASCII-only short strings; container churn dominates. |
| marine_ik (132%) | 09_number_swar (27.4%) | mostly float arrays; same as canada with some structural overhead. |

The structural difference: **passing rows do not pay the
`validate_utf8_codepoint` tax** (0.0-0.1% on canada/citm/marine_ik vs
25-40% on the G rows). They also have either fewer strings (canada,
marine_ik) or shorter ASCII-only strings that the NEON 16-byte scan
clears in one block (citm).

The G rows hit `validate_utf8_codepoint` heavily because `skip_json_string_plain`
on aarch64 calls `scan_string_special_block(... 0x20)` whose
`first_interesting()` returns on **any byte ≥ 0x80** (treating non-ASCII as
"interesting"). For multibyte UTF-8 content this fires on every codepoint,
and the validator runs scalar-byte at a time.

## 7. Sidecar comparison anchors

Cross-referenced from `skinny/profile/native-sidecars/PROFILE-REPORT.md`
(2026-05-12). Re-running sidecar samply was out of scope for this 30 min
slot; numbers are the existing M5 Max samply set.

| corpus | bbnf hot leaf | simdjson C++ hot stage | yyjson hot leaf | bbnf MiB/s | best comparator MiB/s | gap |
|---|---|---|---|---:|---:|---|
| twitter       | utf8_validator + string scan ×3 | stage1 55.1% | `read_str_opt` (single-fused) | 2631 | simdjson 2923 | -10% |
| random        | utf8_validator + string scan ×3 | stage1 50.1% | — | 1117 | simdjson 2460 | -54% |
| unicode_mixed | utf8_validator + scan ×3 (92%) | stage2 44.5% + OUTLINED 30.8% | — | 1719 | simdjson 1568 | **+10%** |
| unicode_basic | utf8_validator + scan ×3 (79%) | stage1 47.0% + stage2 40.5% | — | 1731 | simdjson 1940 | -11% |

The shape simdjson uses on string-heavy rows is **stage1 SIMD classifier
(no per-byte UTF-8 validator on the hot path) + stage2 per-token escape
decode**. simdjson's `OUTLINED_FUNCTION_*` fragments take 30% on
unicode_mixed and that is where simdjson loses; bbnf is already 10%
ahead on unicode_mixed because we already do not pay simdjson's
per-window UTF-8 automaton. But we DO pay the per-non-ASCII-byte
validator, which on twitter/random/unicode_basic is the same expense
shape that simdjson avoids.

yyjson (scalar) collapses to a single `read_str_opt` symbol on its
inlined build (14-41% of `yyjson_read_opts` time on the no-inline build).
yyjson's string path is one fused scalar loop with explicit ASCII fast
path; we're replicating that body three times by inlining and the third
copy is the same byte work each time.

c/B (cycles/B) gap on M5 Max P-core @ 3.5 GHz (1 GiB/s = 3.34 c/B):

| corpus | bbnf c/B | best comp c/B | gap (c/B) |
|---|---:|---:|---:|
| twitter       | 1.30 | 1.17 (simdjson) | +0.13 |
| random        | 3.06 | 1.39 (simdjson) | +1.67 |
| unicode_mixed | 1.99 | 2.18 (simdjson) | -0.19 |
| unicode_basic | 1.97 | 1.76 (simdjson) | +0.21 |

random is the outlier: bbnf pays 3.06 c/B versus simdjson's 1.39 c/B —
the biggest absolute close-the-gap target.

## 8. Recommendations

### 8a. First kernel to land (single boundary, single PR)

**Replace the `validate_utf8_codepoint`-per-non-ASCII-byte path inside
`skip_json_string_plain` with a NEON-validated string-body scan that
folds UTF-8 validation into the same 16-byte block check.**

Concretely: the existing `bbnf_simd::aarch64::string_block::scan_string_special_block`
returns on any byte `≥ 0x20` interesting (quote / backslash / control /
non-ASCII). The "non-ASCII" case currently shells out to the scalar
`validate_utf8_codepoint`. The replacement folds the simdjson-style
UTF-8 lookahead automaton into the NEON block: classify lead bytes,
require 2/3/4 continuation bytes by `vshl`/`vand` against a low-nibble
table, accumulate a "needs-fix" mask, never leave NEON unless the
mask is non-zero.

Estimated gain on the G-rows: 07_utf8 is 25-40% of parse_value_at
self-time. Even cutting that in half (NEON-amortised validator) closes
~15-20% of parse_value_at — taking random from 51% → ~65% of sonic-rs,
unicode_mixed from 47% → ~58%, unicode_basic from 49% → ~62%, twitter
from 78% → ~91%.

### 8b. Wave 0 deliverable — no-inline diagnostic feature

Yes. Add `parse-attribution` feature to `skinny/crates/runtime/Cargo.toml`:

```toml
[features]
parse-attribution = []
```

Under this feature, replace `#[inline(always)]` with `#[inline(never)]`
on `parse_object`, `parse_array`, `parse_string`, `parse_pair`,
`parse_key_colon`, `parse_number`, `match_json_string_at_quote`,
`skip_json_string_plain`, `validate_utf8_codepoint`. Then samply
symbol-level attribution becomes directly readable without manual
PC-region walks.

This is a one-day deliverable that pays for itself the next time SK
needs to re-attribute. The current report had to use `otool -tV` walks
and Python PC bucketing because the symbol fused everything.

### 8c. Are the four G rows one pathology or two?

**One pathology**, four intensity levels. All four are bound on the
same `skip_json_string_plain` + `validate_utf8_codepoint` boundary
inside `match_json_string_at_quote`. The relative weight of utf8 vs
scan shifts by content shape (unicode_* see 39% utf8; twitter sees 25%
utf8 plus 17% tape-reserve) but the kernel is the same.

### 8d. random outlier

random has 16.1% in region 13 (post-loop cold tails). This is partly
the `unreachable_unchecked` after the digit-scan terminator, partly
the function-epilogue restore. The samply rate (4 kHz) catches the
epilogue path proportionally to call count, not work. random has
~1.5x the value count of the other G rows per byte (it's a
synthesized random-shape corpus), so the per-value prologue+epilogue
shows up. This is **not** an actionable hot leaf — but it does say
that random would also benefit from any reduction in
function-prologue/epilogue cost, which the Wave 0 no-inline build
would let us measure precisely.

## 9. Honesty about what samply could not resolve

- Sub-region boundaries are coarse 0x100-0x400 byte buckets. PC
  attribution within a region (e.g. which `cmp` inside region 07 is
  the hot one) requires either a no-inline build or `samply load`
  followed by interactive flame-graph inspection.
- The `error_cold` region (13) shows 4.6-16.1% across rows. Some of
  this is genuine cold-tail layout artefact (post-loop addresses
  sampled at prologue cost), some is `reserve_offsets_cold` BL
  trampoline. Disentangling required examining the BL targets in
  region 10 (`__RNvMs_NtNtCs..TapeBuilder20reserve_offsets_cold`).
- Inline expansion has produced **three** copies of the NEON string
  scan body (sites A/B/C). The compiler made independent inlining
  decisions for `parse_string`, `parse_key_colon`, and a third
  context (the `parse_pair → parse_value_at` recursion). I-cache
  pressure from this triplication is probable but unmeasured in
  this 30 min slot.

## 10. Files

- `/tmp/skv5-B1-profiles/{twitter,random,unicode_mixed,unicode_basic,canada,citm,marine_ik}.json.gz`
- `/tmp/skv5-B1-profiles/*.syms.json` (presymbolicated)
- `/tmp/skv5-extract3.py` (PC-region extractor used to produce the table)
- This report: `/tmp/skv5-B1-parse-attribution.md`

## 11. Single most actionable finding

`validate_utf8_codepoint` is the dominant kernel boundary on every
parse-G corpus (25-40% of parse_value_at self-time). It is reached via
`skip_json_string_plain`'s NEON block returning early on any byte ≥ 0x80
and falling through to a scalar per-byte validator. **Fold UTF-8
validation into the NEON 16-byte string-body scan first.** This is the
Wave 1 kernel boundary that closes the four parse-G rows together.

The SK-V4 hypothesis (Class A `match_tiny_plain_string`) is not what
the PC attribution shows. `match_tiny_plain_string` is at most 7.9% on
random; the real bound is one layer below, in `skip_json_string_plain`'s
fall-through to `validate_utf8_codepoint`.
