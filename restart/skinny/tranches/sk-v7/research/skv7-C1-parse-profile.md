# SK-V7 cohort C1: parse-G PC-level attribution

Date: 2026-05-16. Working tree: `/Users/mkbabb/Programming/bbnf-lang`.

Workspace artifacts (this pass only; read-only on tracked files):

- `/tmp/skv7-cargo/C1/release/profile-lazy` — built with `runtime/parse-attribution`
  feature so each named kernel boundary is `#[inline(never)]` and resolvable
  by samply.
- `/tmp/skv7-C1-profiles/*.json.gz` + `*.syms.json` — thirteen presymbolicated
  samply captures (one per parse-G row).
- `/tmp/skv7-extract-symbols.py` — local extractor that walks the samply
  thread/stack/frame tables, resolves each PC against the
  `profile-lazy` `symbol_table` (rva ranges) and innermost inlined frame, and
  reports self-time outer/leaf attribution with `(file, line)`.

Build profile carries `debug=true` (release profile inherits per-workspace),
so symbol+line resolution is intact.

## 1. Reproduction

```
cd /Users/mkbabb/Programming/bbnf-lang/skinny
export CARGO_TARGET_DIR=/tmp/skv7-cargo/C1
cargo build --release -p xtask --bin profile-lazy --features runtime/parse-attribution

mkdir -p /tmp/skv7-C1-profiles
samply record --rate 4000 --main-thread-only --unstable-presymbolicate \
  --save-only --no-open \
  -o /tmp/skv7-C1-profiles/<corpus>.json.gz \
  /tmp/skv7-cargo/C1/release/profile-lazy <iters> \
  /Users/mkbabb/Programming/bbnf-lang/skinny/test_data/<corpus>.json
```

Iteration counts (target ~3-8 s wall per row, attribution-instrumented):

| corpus            | iters  | bytes/iter | wall (s) | Mbps (attr) |
| ----------------- | ------:| ----------:| --------:| -----------:|
| twitter           |  10000 |     631515 |     5.17 |        9764 |
| random            |  15000 |     510476 |     8.52 |        7188 |
| unicode_mixed     |   4000 |    1053086 |     5.16 |        6531 |
| unicode_basic     |   4000 |    1048586 |     4.03 |        8325 |
| unicode_escapes   |   4000 |    1050797 |     3.17 |       10623 |
| citm_catalog      |   5000 |    1727204 |     3.24 |       21292 |
| apache_builds     |  30000 |     127275 |     2.89 |       10578 |
| github_events     |  60000 |      65132 |     2.66 |       11757 |
| update-center     |   8000 |     533178 |     3.76 |        9069 |
| gsoc-2018         |   3000 |    3327831 |     4.08 |       19560 |
| instruments       |  18000 |     220346 |     2.61 |       12171 |
| distinct_values   |  25000 |     153630 |     4.06 |        7569 |
| y_string_unicode  | 100000 |      35601 |     5.44 |        5235 |

`parse-attribution` builds break inlining at the named boundaries; throughput
is therefore 0.85-0.95x of the bench-shape build. Mbps here is for
attribution-sanity, not the gate.

The `parse-attribution` feature is already on the `runtime` crate (V5 Wave 0
landed): `skinny/crates/runtime/Cargo.toml` declares it, and
`grammars/json/generated.rs` gates the named kernel boundaries with
`#[cfg_attr(feature = "parse-attribution", inline(never))]`.

## 2. Top-level self-time, all thirteen rows

Self-time is the top-of-stack PC mapped to the innermost inlined frame, then
collapsed to the outer parse-attribution boundary. `match_tiny_plain_string`
is the 16-byte scalar tiny-plain probe; `match_string_at_quote` wraps the
parse-that-regex path (`match_json_string_at_quote_trusted_utf8` + 16-byte
NEON `string_block::scan_string_special_block` + the escape validator).

| corpus | tiny_plain | str@quote | container_next | key_colon | quote@cursor | array_next | structural | dispatch | num@digit | skip_ws | emit_offset |
| ------ | ---------:| ---------:| --------------:| ---------:| ------------:| ----------:| ----------:| --------:| ---------:| -------:| -----------:|
| twitter          | 40.54 | 18.82 |  9.46 |  6.12 |  4.50 |  -    |  1.22 |  3.56 |  2.08 |  1.22 |  4.59 |
| random           | 38.20 |  9.59 |  8.89 |  7.94 |  6.79 |  4.02 |  2.72 |  4.22 |  2.40 |  1.97 |  6.75 |
| unicode_mixed    |  6.90 | 75.13 |  2.78 |  2.26 |  2.37 |  0.89 |  1.01 |  0.89 |  2.43 |  0.34 |  2.12 |
| unicode_basic    | 28.51 | 23.60 |  6.03 |  5.41 |  8.42 |  4.36 |  2.46 |  3.20 |  3.58 |  1.09 |  6.03 |
| unicode_escapes  |  0.78 | 90.93 |  0.90 |  0.82 |  0.92 |  0.48 |  0.69 |  0.42 |  0.72 |  -    |  1.17 |
| citm_catalog     | 19.94 |  1.46 | 12.84 |  9.31 |  4.06 |  9.08 |  7.68 |  5.42 |  5.60 |  6.78 |  8.81 |
| apache_builds    | 42.65 | 15.30 |  8.02 |  8.94 |  6.44 |  2.21 |  1.93 |  1.99 |  -    |  1.22 |  5.01 |
| github_events    | 41.62 | 21.25 |  6.30 |  6.74 |  4.24 |  -    |  1.57 |  2.19 |  3.76 |  1.35 |  4.54 |
| update-center    | 46.69 | 19.61 |  4.68 |  5.30 |  5.09 |  1.33 |  2.73 |  2.44 |  -    |  0.72 |  5.73 |
| gsoc-2018        | 24.77 | 59.28 |  3.89 |  2.70 |  2.31 |  -    |  0.87 |  0.74 |  -    |  0.77 |  2.40 |
| instruments      | 31.64 |  4.53 | 13.21 | 12.95 |  4.58 |  1.76 |  2.19 |  3.05 |  7.90 |  2.10 |  7.23 |
| distinct_values  | 47.10 | 11.54 |  5.73 | 10.16 |  8.46 |  0.57 |  0.68 |  2.83 |  0.92 |  0.30 |  6.29 |
| y_string_unicode |  9.56 | 63.22 |  -    |  -    |  4.75 |  6.78 |  -    |  1.42 |  -    |  0.01 |  3.41 |

(`-` means below 0.2% / not in top 15; raw `## Top 15 self-time` blocks per
row in `/tmp/skv7-all-extracts.txt`.)

The two cycle sinks dominating every parse-G row are
`match_tiny_plain_string` (16-byte scalar) and `match_string_at_quote`
(SIMD body scan + escape validator). Their combined self-time:

| corpus | tiny + str@quote (%) |
| ------ | -------------------: |
| twitter          | 59.36 |
| random           | 47.79 |
| unicode_mixed    | 82.03 |
| unicode_basic    | 52.11 |
| unicode_escapes  | 91.71 |
| citm_catalog     | 21.40 |
| apache_builds    | 57.95 |
| github_events    | 62.87 |
| update-center    | 66.30 |
| gsoc-2018        | 84.05 |
| instruments      | 36.17 |
| distinct_values  | 58.64 |
| y_string_unicode | 72.78 |

Eleven of thirteen rows spend more than 47% of self-time inside the two
string-recognizer boundaries. Only `citm_catalog` and `instruments` invert,
and both invert toward the container/key bookkeeping cluster
(`consume_container_next` + `parse_key_colon` + `consume_array_next` +
`consume_structural`).

## 3. Per-row PC-level attribution to `generated.rs`

The `## Top 12 source-line hot spots` blocks per row in
`/tmp/skv7-all-extracts.txt` give file:line for the innermost frame at each
hot PC. Across all rows the dominant lines are stable:

| line                        | source body                                            | meaning                                                        |
| --------------------------- | ------------------------------------------------------ | -------------------------------------------------------------- |
| `generated.rs:173`          | `match_tiny_plain_string_with_cap::<16>`               | 16-byte scalar tiny-string probe, hit before parse-that scan   |
| `generated.rs:186`/`190`    | `match_string_at_quote` wrapper                        | trusted-UTF8 string scan + escape validator (parse-that-regex) |
| `parse-that-regex/lib.rs:303` | `match_json_string_at_quote_trusted_utf8`            | actual loop body of the string scan when wrapper is inlined    |
| `generated.rs:309`          | `consume_container_next`                               | post-pair `,`/`}` resolution + tape offset emit                |
| `generated.rs:347`          | `consume_array_next`                                   | post-value `,`/`]` resolution + tape offset emit               |
| `generated.rs:90`           | `parse_key_colon`                                      | key string + `:` resolution                                    |
| `generated.rs:262`          | `consume_quote_at_cursor`                              | open-quote check + tape emit                                   |
| `generated.rs:291`          | `consume_structural`                                   | structural-byte check + skip_ws fallback                       |
| `generated.rs:212`          | `match_number_at_digit`                                | parse-that number span recognizer                              |
| `generated.rs:240`          | `skip_ws` (→ `parse-that-regex/lib.rs:129`)            | whitespace skip                                                |
| `generated.rs:48`           | `dispatch_value`                                       | match-on-byte dispatch                                         |

Every cycle attributed to `match_string_at_quote` for `unicode_escapes` and
`gsoc-2018` resolves to `parse-that-regex/lib.rs:303` (the trusted-UTF8 loop
body); the inner SIMD block and `validate_json_unicode_escape_run` are both
inlined into that loop and therefore co-attribute to the same instruction
window. See section 6 for the escape-vs-body split.

## 4. Cost classification by row

Classification key: `S-body` = plain string body scan (SIMD/scalar);
`S-esc` = string escape decode (`\uXXXX` + hex run);
`TinyS` = `match_tiny_plain_string` 16-byte scalar probe;
`Cont` = container/array next (`,`/`]`/`}` post-value);
`Key` = `parse_key_colon` + `consume_quote_at_cursor`;
`Num` = `match_number_at_digit`;
`WS` = `skip_ws`; `Emit` = `emit_plain_offset` self-time.

| corpus | dominant cost class | second class | third class | comment |
| ------ | -------------------- | ------------- | ------------ | ------- |
| twitter          | TinyS 40.5%   | S-body (str@quote) 18.8% | Cont 9.5%  | plain-string + cap-16 dominated |
| random           | TinyS 38.2%   | S-body 9.6%              | Cont 8.9% / Key 7.9% | high boundary churn + tiny-cap dominated |
| unicode_mixed    | S-body 75.1%  | TinyS 6.9%               | Cont 2.8%  | **zero `\uXXXX`**; pure body-scan pathology |
| unicode_basic    | TinyS 28.5%   | S-body 23.6%             | Key 13.8%  | tiny-cap + body-scan tied |
| unicode_escapes  | S-esc + S-body 90.9% | TinyS 0.8%        | -          | escape-validator + body-scan inseparable |
| citm_catalog     | Cont+Key+Struct 31.8% | TinyS 19.9%      | WS 6.8% / Num 5.6% | container-bookkeeping shape |
| apache_builds    | TinyS 42.7%   | S-body 15.3%             | Key 15.4%  | plain-string keyed objects |
| github_events    | TinyS 41.6%   | S-body 21.3%             | Key 11.0%  | plain-string keyed objects |
| update-center    | TinyS 46.7%   | S-body 19.6%             | Key 10.4%  | plain-string keyed objects |
| gsoc-2018        | S-body 59.3%  | TinyS 24.8%              | Cont 3.9%  | long strings exceed 16-byte tiny-cap |
| instruments      | TinyS 31.6%   | Cont 13.2% / Key 13.0%   | Num 7.9%   | mixed scalar+container, numbers significant |
| distinct_values  | TinyS 47.1%   | S-body 11.5%             | Key 18.6%  | **zero `\uXXXX`**; plain-string + key bookkeeping |
| y_string_unicode | S-body+S-esc 63.2% | TinyS 9.6%          | Array 6.8% | escape + body inseparable in single boundary |

Two pathologies, plus one outlier shape:

- **P1: TinyS-bound** — twitter, random, apache_builds, github_events,
  update-center, distinct_values, unicode_basic, instruments. The 16-byte
  scalar probe at `generated.rs:173` is 28-47% of total parse cycles.
- **P2: S-body bound** — unicode_mixed, unicode_escapes, gsoc-2018,
  y_string_unicode (and to a lesser extent unicode_basic). `match_string_at_quote`
  is 19-91% of total parse cycles; the body-scan SIMD block and the escape
  validator share this leaf with no further inlining boundary.
- **Outlier C: container-shape bound** — citm_catalog, instruments.
  `consume_container_next` + `consume_array_next` + `parse_key_colon` +
  `consume_structural` dominate.

## 5. Diff vs SK-V6 C1 (`restart/skinny/tranches/sk-v6/research/skv6-C1-retained-profile.md`)

V6 C1 reported attribution from `/tmp/skv6-B3-profiles/*` and
`skinny/profile/skinny-expanded/*` (older bin). The post-V6 admits cited by
the user prompt are:

1. **ContainerNext admit** — `consume_container_next` and `consume_array_next`
   replaced the older two-step `consume(',')` + `parse_value_at` pair, fusing
   `,`/`]`/`}` resolution with the post-pair tape emit.
2. **Tiny-string cap admit** — `match_tiny_plain_string` (16-byte cap) is the
   first probe in `parse_string` and `parse_key_colon`, only falling through
   to `match_string_at_quote` when the cap is exceeded.

What changed in the hot-leaf distribution:

| row | V6 best-evidence top self leaves | V7 (this) top self leaves | delta |
| --- | --- | --- | --- |
| twitter | tiny_plain 42.26%, str@quote 18.13%, container_next 9.82%, key_colon 5.48% | tiny_plain 40.54%, str@quote 18.82%, container_next 9.46%, key_colon 6.12% | stable; tiny_plain still dominant |
| update-center (V6 PMU) | tiny_plain key 31.0% + value 17.3% ≈ 48% | tiny_plain 46.69% (single boundary) | tiny still dominant |
| random (V6 PMU) | tiny_plain key 31.0% + value 20.5% ≈ 51% | tiny_plain 38.20% | tiny still dominant; key/value paths now share one symbol |
| unicode_escapes (V6 B3) | `match_string_at_quote` 90.44% self | `match_string_at_quote` 90.93% self | unchanged within noise |
| y_string_unicode (V6 PMU) | `\uXXXX` band 13.9%, key escape recovery 35.1%, tiny visible | str@quote 63.22%, tiny 9.56%, array_next 6.78% | now collapsed into single str@quote leaf; tiny present but minor |
| citm_catalog (V6 expanded) | `parse_value` 53.67%, `simd_scan` 31.60%, structural 10.11% | container_next 12.84%, key_colon 9.31%, array_next 9.08%, structural 7.68% | parse_value and simd_scan no longer visible; ContainerNext admit replaced the old container re-entry hotspots |
| gsoc-2018 | (no current sample in V6) | str@quote 59.28%, tiny 24.77% | new data; long-string body scan dominates |

The two V6 admits did what they advertised:

- **ContainerNext**: citm self-time inside the post-pair/post-value cluster
  is now visible as discrete `consume_container_next` / `consume_array_next`
  symbols. There is no `parse_value` 53% mountain anymore; the cycles are
  attributed to their actual leaves. Net wall-time on citm is also down (V6
  Track 1 was 32459 Mbps; V7 attribution is 21292 Mbps which is the lossy
  attribution build, but Track 1 retains row classification G per RESULTS.md
  due to Track 2 lag).
- **Tiny-string cap-16**: `match_tiny_plain_string` is now the single
  largest leaf on 8/13 rows. It absorbed the cost that previously sat in
  `match_string_at_quote` for short-string-heavy corpora. The leaf is at
  `generated.rs:173`, the scalar `while cursor < limit` loop inside
  `match_tiny_plain_string_with_cap::<16>`. This is the most-improvable
  single leaf in the corpus.

## 6. Per-`\uXXXX` TBL hypothesis check

The B1 candidate (per `restart/skinny/tranches/sk-v6/research/skv6-B1-asmjson-challenge.md`
and the V6 C1 "falsifiable intervention candidate" section) is to replace
the scalar per-`\uXXXX` nibble validate inside
`validate_json_unicode_escape_run` with a per-unit TBL classifier. That
hypothesis is verifiable here by counting `\uXXXX` occurrences in the
corpus and checking whether the `match_string_at_quote` self-time can be
attributed to escape decode rather than the SIMD body-scan block.

| corpus           | size B | `\uXXXX` count | `\uXXXX` bytes / total | str@quote self% | body-vs-esc reading |
| ---------------- | -----: | -------------: | ---------------------: | ---------------:| ------------------- |
| unicode_escapes  | 1050797 | 136682 | 78.0% | 90.93% | escape decode dominant within str@quote; B1 applies |
| y_string_unicode |   35601 |   4400 | 74.2% | 63.22% | escape decode dominant within str@quote; B1 applies |
| unicode_mixed    | 1053086 |      0 |  0.0% | 75.13% | plain body scan only; B1 does NOT apply |
| distinct_values  |  153630 |      0 |  0.0% | 11.54% | plain body scan; B1 does NOT apply |
| gsoc-2018        | 3327831 |   1292 |  0.2% | 59.28% | plain body scan dominant (escapes negligible); B1 does NOT apply |
| twitter          |  631515 |      0 |  0.0% | 18.82% | plain body scan only |
| random           |  510476 |      0 |  0.0% |  9.59% | plain body scan only |
| unicode_basic    | 1048586 |      0 |  0.0% | 23.60% | plain body scan only |
| update-center    |  533178 |      0 |  0.0% | 19.61% | plain body scan only |

`grep -o '\\u[0-9a-fA-F]\{4\}'` over the corpus files gives the counts. The
fraction is `count*6 / size` (six bytes per `\u` unit including the
backslash and `u`).

This is the load-bearing finding for B1:

- B1 applies to exactly two corpus rows where escape decode is genuinely
  dominant: `unicode_escapes` and `y_string_unicode`.
- All four "escape-dominated" rows the user prompt named are not all actually
  escape-dominated. `unicode_mixed` has zero `\uXXXX` units and is pure
  plain-body-scan; the 75.13% cost on `match_string_at_quote` resolves to
  `parse-that-regex/lib.rs:303` (the trusted-UTF8 outer loop) and through
  it to the 16-byte NEON `string_block::scan_string_special_block`
  (`bbnf-simd/src/aarch64/string_block.rs`). Same for `distinct_values` at
  zero `\uXXXX` and 11.5% str@quote.
- `gsoc-2018` looks escape-heavy by row name but is actually long-plain-string
  body scan (1292 `\uXXXX` in 3.3 MB is 0.2%). Its 59.28% str@quote cost
  comes from strings whose body exceeds the 16-byte tiny cap and falls
  through to the parse-that body scan.

So B1's reach is narrower than the V6 C1 report assumed; it benefits 2 rows
strictly. The other 11 rows of cycle attribution land on:

1. `match_tiny_plain_string_with_cap::<16>` scalar inner loop
   (`generated.rs:173`), 8 of 13 rows over 28%.
2. `skip_json_string_plain_trusted` body-scan loop
   (`parse-that-regex/lib.rs:679-705`) reached through
   `match_string_at_quote`, the rest of `match_string_at_quote` self-time
   not attributable to `\uXXXX`.

The same body-scan kernel is the one that asmjson generalization (skv6-A1
§3) abstracts as "per-scalar terminator_mask | escape_intro_mask |
invalid_body_mask"; on aarch64 the existing implementation is
`scan_string_special_block` over `b'"'`, `b'\\'`, `0x20`. The PC attribution
shows it firing in the same leaf as the per-`\uXXXX` validator, which means
a checkasm-style differential would need to split them (e.g. via
distinct symbol/inline boundary, an unstable but uniform attribution-only
inline wrapper, or a counter).

## 7. Strict-vs-strict implication (A1)

`restart/skinny/tranches/sk-v6/research/skv6-A1-asmjson-generalization.md` §1 and
§4 say sonic-rs is utf8_lossy and BBNF retained is `deferred /
view-boundary / yes` (current rows are not strict-vs-strict wins against
sonic). A1 does not publish a numeric "3-8% sonic regression post-strict"
prediction in §5; the cited 3-8% is the user prompt's working figure. With
that figure as a sensitivity bracket, the post-V6 RESULTS.md ratios
recompute as:

| corpus | current Track 1 / sonic | strict-rebuild sonic @3% slower | @5% | @8% | flips? |
| ------ | ----------------------: | -------------------------------:| ---:| ---:| ------ |
| twitter           | 73.6% | 75.9% | 77.5% | 80.0% | no (need ≥90% to flip A) |
| random            | 65.5% | 67.5% | 68.9% | 71.2% | no |
| unicode_mixed     | 56.1% | 57.8% | 59.1% | 61.0% | no |
| unicode_basic     | 91.7% | 94.5% | 96.5% | 99.7% | **flips to A at 8%**; B at 5% |
| unicode_escapes   | 80.4% | 82.9% | 84.6% | 87.4% | no |
| citm_catalog      | 130.3%| -     | -     | -     | already A on Track 1 |
| apache_builds     | 78.0% | 80.4% | 82.1% | 84.8% | no |
| github_events     | 68.8% | 70.9% | 72.4% | 74.8% | no |
| update_center     | 59.6% | 61.4% | 62.7% | 64.8% | no |
| gsoc-2018         | 53.6% | 55.3% | 56.4% | 58.3% | no |
| instruments       | 92.0% | 94.8% | 96.8% | 100.0%| **flips to A at 8%**; B at 5% |
| distinct_values   | 60.2% | 62.0% | 63.4% | 65.4% | no |
| y_string_unicode  | 46.0% | 47.4% | 48.4% | 50.0% | no |

`unicode_basic` and `instruments` are the only rows close enough to the
SOTA-beat boundary that a 5-8% strict-driven sonic-rs regression alone
would flip them. The remaining eleven rows stay G regardless; closing them
requires real cycles taken out of the BBNF parse path, not a strict-rebuild
sonic shift.

## 8. Recommendation

The single highest-impact kernel boundary in this corpus is
**`match_tiny_plain_string_with_cap::<16>`** at `generated.rs:173`. It is
the top self-time leaf on 8 of 13 parse-G rows and the second leaf on most
of the remaining 5. Its body is:

```rust
fn match_tiny_plain_string_with_cap<const CAP: usize>(input: &[u8], offset: usize) -> Option<usize> {
    let mut cursor = offset + 1;
    let limit = (cursor + CAP).min(input.len());
    while cursor < limit {
        match input[cursor] {
            b'"' => return Some(cursor + 1),
            b'\\' | 0x00..=0x1f => return None,
            _ => cursor += 1,
        }
    }
    None
}
```

That is a byte-at-a-time scalar loop with three-way branching. The 16-byte
cap was admitted post-V6 because it improved native rows materially (V6 C1
§Interpretation: twitter +27.5%, citm +49.2%, github_events +16.9%,
update_center +27.4%, random +21.8%, distinct_values +57.5%). The cost
that remains is the scalar loop body, not the cap policy.

A single 16-byte NEON block here can decide
`(terminator_mask | escape_mask | control_mask)` over the full window in
constant operations — the exact kernel
`bbnf_simd::aarch64::string_block::scan_string_special_block` already used
inside `skip_json_string_plain_trusted` for the body-scan fallback. The
generated tiny-plain probe is the only place in the parse path where a
scalar loop persists over a known 16-byte window: the input window is
already bounded, the alphabet is already `(b'"', b'\\', 0x20)`, and the
admission decision is "terminator before any escape/control" which is a
two-mask compare on the NEON result.

Concrete intervention: replace the scalar tiny-plain probe with a single
NEON 16-byte block compare (or x86 16/32-byte SSE2/AVX2 equivalent) that
returns `Some(cursor + ctz(terminator_mask)/8 + 1)` when
`terminator_mask != 0 && (escape_mask | control_mask) == 0` strictly before
the terminator bit, else `None`. The existing
`bbnf_simd::aarch64::string_block` primitive is the receiver; nothing else
in the parse-attribution kernel set has to change.

Cross-corroboration with B1:

- The B1 per-`\uXXXX` TBL classifier still targets a real cost, but only
  for two corpus rows (`unicode_escapes`, `y_string_unicode`) where escape
  decode is actually dominant.
- B1 sits inside `match_string_at_quote` /
  `validate_json_unicode_escape_run`. It is orthogonal to the tiny-plain
  intervention; both can land in parallel because they touch different
  kernel boundaries.
- The bigger-fan-out intervention is the tiny-plain SIMD admission, because
  it touches the larger set of parse-G rows. B1 is still needed for the
  escape-validator rows, but the tiny-plain intervention is the single
  highest-impact one.

## 9. Are the 13 parse-G rows 2 pathologies or more?

Not two. Three:

- **P1 tiny-plain scalar** (8 rows): twitter, random, apache_builds,
  github_events, update-center, distinct_values, unicode_basic, instruments.
  Fixed by SIMD-admitting `match_tiny_plain_string_with_cap::<16>`.
- **P2 string body / escape scan** (4 rows): unicode_mixed, unicode_escapes,
  gsoc-2018, y_string_unicode. The single boundary
  `match_string_at_quote` is dominant. Within those: unicode_escapes and
  y_string_unicode have real `\uXXXX` content (B1 prescription applies);
  unicode_mixed and gsoc-2018 are body-scan over long plain strings (B1
  does NOT apply; the existing SIMD body scan already runs, and the
  improvable surface is the loop control / `ctz` selection plus possibly
  the 16- vs 32-byte block size).
- **P3 container/key bookkeeping** (1 row strictly + 1 partial): citm_catalog
  decisively, instruments partially. `consume_container_next` +
  `consume_array_next` + `parse_key_colon` + `consume_structural` sum to
  30+% on these rows. Closing them requires reducing branching in the
  comma/close resolution path, not string-recognizer changes.

P1 is one intervention. P2 is two distinct interventions (B1 for the escape
half; a body-scan refinement for the plain-string half). P3 is a third,
narrower intervention. So three pathologies, four interventions, with the
tiny-plain SIMD admission as the single highest-impact one.
