# SK-V6 R2 parse-only PC attribution - original parse-G rows

Date: 2026-05-14.
Workspace: `/Users/mkbabb/Programming/bbnf-lang`.
Scope: Wave 1 R2, retained parse Track 1 only, original parse-G rows:
`twitter`, `random`, `unicode_mixed`, `unicode_basic`.

No tracked files were edited, staged, or committed. Existing staged changes
were present before this dispatch in:

- `skinny/crates/bbnf-bench/src/metadata.rs`
- `skinny/xtask/src/bin/capacity_probe.rs`

## Authority Read

Read before profiling:

- `restart/skinny/audit/IMPLEMENTATION-AGENT-PROMPT-SK-V6.md` sections 1-5,
  especially Wave 1 R2 and the ban on SK-V5 hypothesis transfer.
- `restart/skinny/audit/GRAND-SYNTHESIS-SK-V5.md` post-assay header and
  section 2.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md` entries 50-59.
- `restart/skinny/audit/SK-V5-COHORT/skv5-B1-parse-attribution.md`.
- `restart/skinny/audit/SK-V5-COHORT/skv5-D2-utf8-novelty.md`.

Binding correction: the SK-V5 Wave 3 UTF-8 fusion close route is refuted by
REDRESS 50, 51, 53, 54, 55, and recorded as a route-class rejection by item 59.
This report does not reopen it as a blanket prescription.

## Build And Capture

Build:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
export CARGO_TARGET_DIR=/tmp/skv6-cargo/R2
cargo build --release -p xtask --bin profile-lazy --features runtime/parse-attribution
```

Profile command shape:

```bash
samply record --rate 4000 --main-thread-only --unstable-presymbolicate \
  --save-only --no-open -o /tmp/skv6-R2-profiles/<corpus>.profile.json.gz \
  /tmp/skv6-cargo/R2/release/profile-lazy <iters> <corpus-or-path>
```

Symbol extraction note: samply saved module-relative frame addresses rather
than function names in the Firefox profile tables. I mapped leaf PCs against
`nm -nm /tmp/skv6-cargo/R2/release/profile-lazy | rustfilt`, subtracting the
Mach-O `__TEXT` vmaddr base `0x100000000`.

Cycles per byte use the requested 3.5 GHz baseline:

```text
c/B = 3.5e9 / (Mbps * 1e6 / 8) = 28000 / Mbps
```

## Results

| Corpus | Profile | Iters | Main-thread samples | Measured Mbps | c/B | Dominant generated boundary | Hot leaf class | SK-V5 diagnosis verdict |
|---|---|---:|---:|---:|---:|---|---|---|
| twitter | `/tmp/skv6-R2-profiles/twitter.profile.json.gz` | 10000 | 20366 | 9933 | 2.82 | `generated::match_tiny_plain_string` 33.1%, then `generated::match_string_at_quote` 23.5% | string prefix + trusted delimiter scan, not `validate_utf8_codepoint` | Broad string diagnosis confirmed, exact SK-V5 boundary incomplete; UTF-8 prescription does not target current hot leaf |
| random | `/tmp/skv6-R2-profiles/random.profile.json.gz` | 15000 | 36099 | 6782 | 4.13 | `generated::match_tiny_plain_string` 34.8%, then `consume_container_next` 15.3%, `match_string_at_quote` 9.0% | short-string prefix scan dominates; full trusted string scan secondary; not `validate_utf8_codepoint` | Diagnosis incomplete at the new generated boundary; not a scalar UTF-8 row |
| unicode_mixed | `/tmp/skv6-R2-profiles/unicode_mixed.profile.json.gz` | 4000 | 17647 | 7658 | 3.66 | `generated::match_string_at_quote` 71.8% | trusted full string/escape scan; not `validate_utf8_codepoint` | SK-V5 correctly found the string boundary, but the UTF-8-validator prescription failed/inapplicable |
| unicode_basic | `/tmp/skv6-R2-profiles/unicode_basic.profile.json.gz` | 4000 | 15922 | 8438 | 3.32 | `generated::match_tiny_plain_string` 28.0%, then `generated::match_string_at_quote` 25.5% | string prefix + trusted delimiter scan, not `validate_utf8_codepoint` | Broad string diagnosis confirmed, exact boundary incomplete; UTF-8 prescription does not target current hot leaf |

Combined string-boundary self time:

| Corpus | `match_tiny_plain_string` | `match_string_at_quote` | Combined string boundary |
|---|---:|---:|---:|
| twitter | 33.1% | 23.5% | 56.6% |
| random | 34.8% | 9.0% | 43.8% |
| unicode_mixed | 7.0% | 71.8% | 78.8% |
| unicode_basic | 28.0% | 25.5% | 53.5% |

No profile showed `validate_utf8_codepoint` as a sampled leaf. The generated
retained parser receives `&str`; `profile-lazy` performs one
`core::str::from_utf8` before the timed loop, and the generated parser calls
`parse_that_regex::match_json_string_at_quote_trusted_utf8`. That trusted path
does not validate raw UTF-8 per codepoint in the parse loop.

## Boundary Detail

The new retained parse hot boundary is:

```text
generated::parse_string / generated::parse_key_colon
  -> generated::consume_quote_at_cursor
  -> generated::match_tiny_plain_string
  -> generated::match_string_at_quote
     -> parse_that_regex::match_json_string_at_quote_trusted_utf8 (inlined)
        -> skip_json_string_plain_trusted (inlined)
```

`match_tiny_plain_string` is the scalar 8-byte prefix loop at
`0x10000cdf8..0x10000ce48`; it checks quote, backslash, and control bytes for
every string before the full matcher runs on misses.

`match_string_at_quote` is at `0x10000ce48..0x10000d600`; it contains the
trusted string delimiter scanner and escape validator. Its hot PCs are in:

- `0x10000ceb0..0x10000cfd0`: first 16-byte quote/backslash/control scan and
  first-interesting-byte reduction.
- `0x10000d0b4..0x10000d1d4`: second scan copy after scalar byte handling.
- `0x10000d238..0x10000d584`: escape validation, hot on `unicode_mixed`.

This differs materially from SK-V5 B1. B1 attributed the old fused
`parse_value_at` hub to `validate_utf8_codepoint` plus string scan. The current
parse-attribution build separates generated boundaries and shows that the raw
UTF-8 validator is not the retained parse leaf. The remaining work is string
prefix, quote/control/backslash detection, and escape validation over already
UTF-8-valid input.

## Why REDRESS 50-55 Do Not Cover This Boundary

- REDRESS 50, retained projection side tables, targeted view/projection facts
  by adding parse-time aux writes. The R2 hot boundary happens before projection
  and is dominated by string delimiter scanning, not missing retained view facts.
- REDRESS 51, byte-class whitespace cursor, and REDRESS 53, parser-local
  structural-mask cursor, targeted whitespace/structural navigation. They do
  not remove the per-string tiny prefix scan or the trusted string matcher, and
  item 53 measured the second-scanner shape as a large retained parse
  regression.
- REDRESS 54 and 55 are direct `SinkOnly` decoded-string materializer/hash
  routes. R2 is retained parse-only; it does not decode strings into the payload
  arena and does not run the direct digest sink.
- REDRESS 59 specifically blocks treating UTF-8 fusion as the close route.
  Here the parse loop is already on a trusted-UTF8 path, so fusing raw UTF-8
  validation into the NEON body scan would not address the sampled leaf.

## Candidate Cluster Intervention

Single candidate, profile-backed and bounded: collapse the generated retained
string prefix boundary into one trusted matcher so the first 8 bytes are not
scanned once by `match_tiny_plain_string` and then rescanned from `start` by
`match_string_at_quote` on misses. This is not the rejected Class A NEON route:
no new SIMD primitive, no retained side table, no cursor sidecar, and no
sink-local decoded materializer. The candidate is a control-flow and boundary
shape change around the existing trusted string matcher.

Falsifiability gate:

- Run the same four R2 rows on the normal release Track 1 baseline and the
  parse-attribution build.
- Accept only if the combined `match_tiny_plain_string + match_string_at_quote`
  self-time drops by at least 20% on two or more of the four rows, and retained
  Track 1 Mbps improves by at least 5% on `twitter`, `random`, and
  `unicode_basic` with no R2 row regressing more than 2%.
- If `unicode_mixed` remains dominated by `match_string_at_quote` after the
  prefix boundary is removed, the next report should treat it separately as a
  trusted escape/delimiter-scan row, not as evidence for raw UTF-8 fusion.

## Bottom Line

R2 confirms that the original parse-G rows remain string-bound, but it does not
confirm `validate_utf8_codepoint` as the current generated Track 1 parse leaf.
The SK-V5 diagnosis was correct only at the coarse "string boundary" level; it
was incomplete at the current generated symbol boundary for twitter, random,
and unicode_basic, and its UTF-8 fusion prescription is not applicable to this
trusted-UTF8 retained parse path. Unicode_mixed is the strongest confirmation
that string scanning itself is still the retained parse blocker, but the exact
leaf is `generated::match_string_at_quote`, not a scalar UTF-8 validator.
