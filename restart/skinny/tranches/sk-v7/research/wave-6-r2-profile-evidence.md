# SK-V7 W6 R2 - citm/instruments profile evidence

Date: 2026-05-16.
Workspace: `/Users/mkbabb/Programming/bbnf-lang`.

Scope: read-only research for SPEC section 8. No source files were edited.
Evidence read:

- `restart/skinny/tranches/sk-v7/SPEC.md` section 8.
- `restart/skinny/tranches/sk-v7/research/skv7-C1-parse-profile.md`.
- `restart/skinny/tranches/sk-v7/research/skv7-C2-direct-profile.md`.
- `restart/skinny/tranches/sk-v7/research/skv7-A4-parse-that-gaps.md`.
- `restart/skinny/tranches/sk-v7/research/wave-5-*.md`.
- `skinny/REDRESS.md` item 83, the W5 StringBlock16 rejection.
- `skinny/RESULTS.md` current gate rows.
- Local profile extracts under `/tmp/skv7-C1-profiles/`,
  `/tmp/skv7-C2-profiles/`, and `/tmp/skv7-all-extracts.txt`.
- Local comparator/profile context under `skinny/profile/*/PROFILE-REPORT.md`
  where it speaks to `citm_catalog`; no local native sidecar profile for
  `instruments` exists in the checked-in `skinny/profile/` tree.

## Bottom line

The top individual retained-parse leaf on both W6 rows is still
`match_tiny_plain_string_with_cap::<16>` at `generated.rs:173`:
19.94% self on `citm_catalog`, 31.64% self on `instruments`.

That is not a W6 reopen target. W5 item 83 already rejected the current
low-overhead-looking route for that leaf: the generated-retained
StringBlock16 wrapper was correctness/parity green, but zero of six named W5
rows crossed threshold and every named Track 1 row regressed by more than the
3% guard. The rejection says the existing AArch64 `string_block` movemask shape
is too expensive for the already-tiny quote-pair probe.

After that rejection, the W6-relevant dominant surface is an aggregate
bookkeeping cluster, not a single leaf:

| Row | Tiny-string leaf | Container/key/control cluster | Reading |
|---|---:|---:|---|
| `citm_catalog` parse | 19.94% | 38.91% (`consume_container_next` + `parse_key_colon` + `consume_array_next` + `consume_structural`) | W6 remains well-supported. |
| `instruments` parse | 31.64% | 30.11% for the same four leaves, or 41.92% if `consume_quote_at_cursor` + `emit_plain_offset` are included as key/control bookkeeping | W6 remains plausible but less clean; tiny-string is still the largest single leaf. |

The gate has a separate risk: current `RESULTS.md` parse rows are already
Track 1-fast and Track 2-limited. `citm_catalog` parse is Track 1 31784 Mbps
vs sonic 25509 Mbps, but Track 2 is 20817 Mbps (81.6% of sonic). `instruments`
parse is Track 1 18038 Mbps vs sonic 16312 Mbps, but Track 2 is 11678 Mbps
(71.6% of sonic). A generated-only W6 patch can improve attribution on Track 1,
but it cannot close the parse gate if the gate continues to require Track 2
substrate throughput. Either W6 must include a mirrored Track 2 hand-parser
compaction, or the gate must explicitly be about generated Track 1/direct
evidence only.

## SPEC section 8 check

SPEC section 8 says W6 owns:

- `runtime/src/grammars/json/generated.rs`, specifically the `citm` +
  `instruments` hot leaves.
- Potentially `bbnf-simd/src/aarch64/` for a key-byte run scan.

Tasks:

1. Profile `citm` + `instruments`; identify the container/key bookkeeping leaf
   at PC level.
2. Optimize the per-key dispatch path.
3. Bench `citm` + `instruments`.

The profile evidence satisfies task 1 for generated Track 1 retained parse and
generated direct Track 1. It does not yet satisfy a Track 2 parse diagnosis.

## Retained parse PC/line evidence

Profiles:

- `/tmp/skv7-C1-profiles/citm_catalog.json.gz`
- `/tmp/skv7-C1-profiles/instruments.json.gz`

Build:

- `/tmp/skv7-cargo/C1/release/profile-lazy`
- `runtime/parse-attribution` enabled, so the generated parser's named leaves
  are `#[inline(never)]` and resolvable by samply.

### citm_catalog parse

Total samples: 12971.

Top outer-symbol self-time from `/tmp/skv7-all-extracts.txt`:

| Self | Leaf |
|---:|---|
| 19.94% | `match_tiny_plain_string` |
| 12.84% | `consume_container_next` |
| 9.31% | `parse_key_colon` |
| 9.08% | `consume_array_next` |
| 8.81% | `ParserState::emit_plain_offset` |
| 7.68% | `consume_structural` |
| 6.78% | `skip_ws` |
| 5.60% | `match_number_at_digit` |
| 5.42% | `dispatch_value` |
| 4.06% | `consume_quote_at_cursor` |

Top PC offsets sampled from the same profile:

| Self | Samples | PC/RVA | Source line | Outer | Leaf |
|---:|---:|---|---|---|---|
| 6.41% | 831 | `0xce00` | `generated.rs:173` | `match_tiny_plain_string` | `match_tiny_plain_string_with_cap::<16>` |
| 4.82% | 625 | `0xcde8` | `generated.rs:173` | `match_tiny_plain_string` | `match_tiny_plain_string_with_cap::<16>` |
| 4.22% | 548 | `0xcdf4` | `generated.rs:173` | `match_tiny_plain_string` | `match_tiny_plain_string_with_cap::<16>` |
| 3.72% | 483 | `0xca80` | `parser.rs:35` | `ParserState::emit_plain_offset` | `ParserState::emit_plain_offset` |
| 3.66% | 475 | `0xca5c` | `parser.rs:35` | `ParserState::emit_plain_offset` | `ParserState::emit_plain_offset` |
| 2.84% | 368 | `0xd960` | `generated.rs:347` | `consume_array_next` | `consume_array_next` |
| 2.43% | 315 | `0xdea4` | `generated.rs:90` | `parse_key_colon` | `parse_key_colon` |
| 2.34% | 304 | `0xd6dc` | `generated.rs:291` | `consume_structural` | `consume_structural` |
| 1.65% | 214 | `0xcd84` | `generated.rs:262` | `consume_quote_at_cursor` | `consume_quote_at_cursor` |
| 1.61% | 209 | `0xd72c` | `generated.rs:291` | `consume_structural` | `consume_structural` |

Line-level parse-region table:

| Self | Line | Meaning |
|---:|---|---|
| 19.94% | `generated.rs:173` | 16-byte scalar tiny-string cap loop |
| 12.84% | `generated.rs:309` | object/array post-pair next-byte resolution |
| 9.31% | `generated.rs:90` | key string + colon resolution |
| 9.08% | `generated.rs:347` | post-array-element next-byte resolution |
| 7.68% | `generated.rs:291` | structural-byte consume + offset emit |
| 6.78% | `generated.rs:240` | whitespace skip |
| 5.60% | `generated.rs:212` | number span recognizer |
| 5.42% | `generated.rs:48` | value-byte dispatch |
| 4.06% | `generated.rs:262` | quote consume + offset emit |

Reading: the single hottest leaf is tiny-string, but W6's four-leaf
container/key/control cluster is 38.91% before counting offset emission and
quote consume. This is the strongest evidence for W6.

### instruments parse

Total samples: 10424.

Top outer-symbol self-time from `/tmp/skv7-all-extracts.txt`:

| Self | Leaf |
|---:|---|
| 31.64% | `match_tiny_plain_string` |
| 13.21% | `consume_container_next` |
| 12.95% | `parse_key_colon` |
| 7.90% | `match_number_at_digit` |
| 7.23% | `ParserState::emit_plain_offset` |
| 4.58% | `consume_quote_at_cursor` |
| 4.53% | `match_string_at_quote` |
| 3.05% | `dispatch_value` |
| 2.19% | `consume_structural` |
| 2.10% | `skip_ws` |
| 1.76% | `consume_array_next` |

Top PC offsets sampled from the same profile:

| Self | Samples | PC/RVA | Source line | Outer | Leaf |
|---:|---:|---|---|---|---|
| 8.70% | 907 | `0xce00` | `generated.rs:173` | `match_tiny_plain_string` | `match_tiny_plain_string_with_cap::<16>` |
| 8.25% | 860 | `0xcde8` | `generated.rs:173` | `match_tiny_plain_string` | `match_tiny_plain_string_with_cap::<16>` |
| 6.06% | 632 | `0xce08` | `generated.rs:173` | `match_tiny_plain_string` | `match_tiny_plain_string_with_cap::<16>` |
| 3.56% | 371 | `0xca80` | `parser.rs:35` | `ParserState::emit_plain_offset` | `ParserState::emit_plain_offset` |
| 3.35% | 349 | `0xca5c` | `parser.rs:35` | `ParserState::emit_plain_offset` | `ParserState::emit_plain_offset` |
| 3.12% | 325 | `0xce1c` | `generated.rs:173` | `match_tiny_plain_string` | `match_tiny_plain_string_with_cap::<16>` |
| 2.60% | 271 | `0xcdf4` | `generated.rs:173` | `match_tiny_plain_string` | `match_tiny_plain_string_with_cap::<16>` |
| 2.45% | 255 | `0xdffc` | `generated.rs:90` | `parse_key_colon` | `parse_key_colon` |
| 2.28% | 238 | `0xcd84` | `generated.rs:262` | `consume_quote_at_cursor` | `consume_quote_at_cursor` |
| 2.17% | 226 | `0xdbf0` | `generated.rs:309` | `consume_container_next` | `consume_container_next` |

Line-level parse-region table:

| Self | Line | Meaning |
|---:|---|---|
| 31.64% | `generated.rs:173` | 16-byte scalar tiny-string cap loop |
| 13.21% | `generated.rs:309` | object/array post-pair next-byte resolution |
| 12.95% | `generated.rs:90` | key string + colon resolution |
| 7.90% | `generated.rs:212` | number span recognizer |
| 4.58% | `generated.rs:262` | quote consume + offset emit |
| 4.53% | `parse-that-regex/lib.rs:303` | trusted string scan fallback |
| 3.05% | `generated.rs:48` | value-byte dispatch |
| 2.19% | `generated.rs:291` | structural-byte consume + offset emit |
| 2.10% | `generated.rs:240` | whitespace skip |
| 1.76% | `generated.rs:348` | array next-byte resolution |

Reading: `instruments` is more ambiguous than `citm_catalog`. The top single
leaf is tiny-string by a large margin, but the W6 cluster is still large enough
to explain a 5-10% target if the intervention is genuinely cheaper than the
current branch/offset path. It is not as clean a W6 row as `citm_catalog`.

## Direct Track 1 evidence

Profiles:

- `/tmp/skv7-C2-profiles/citm_catalog.track1.json.gz`
- `/tmp/skv7-C2-profiles/instruments.track1.json.gz`

Direct profiles do not have parse-attribution boundaries; source-line
attribution is recovered through inlining metadata/atos and is therefore the
more stable key than symbol names. These rows support W6's direct/codegen
dispatch-tax side.

### citm_catalog direct Track 1

Samples: 13956.

Per-function self-time:

| Self | Function |
|---:|---|
| 56.3% | `parse_array_element_at_direct::<JsonDigestSink>` |
| 40.5% | `parse_object_value_at_direct::<JsonDigestSink>` |
| 2.1% | `parse_that_regex::number::materialize_u64` |
| 0.9% | `JsonDigestSink` sink method |

Top lines:

| Self | Line | Meaning |
|---:|---|---|
| 49.5% | `generated.rs:517` | array element is object: recurse through `parse_object_direct` |
| 15.6% | `generated.rs:478` | object value is array: recurse through `parse_array_direct` |
| 11.2% | `generated.rs:485` | object numeric branch |
| 4.1% | `generated.rs:477` | object value is object: recurse |
| 3.1% | `generated.rs:503` | object value match epilogue |
| 1.7% | `generated.rs:516` | array value dispatch match |
| 1.4% | `generated.rs:480` | object string branch |
| 1.4% | `generated.rs:525` | array numeric branch |

Reading: direct `citm_catalog` is not string-scan-bound. It is array/object
dispatch recursion plus numeric branch mix. This supports a B6-style codegen
control compaction, not a string primitive.

### instruments direct Track 1

Samples: 20370.

Per-function self-time:

| Self | Function |
|---:|---|
| 59.1% | `parse_array_element_at_direct::<JsonDigestSink>` |
| 37.5% | `parse_object_value_at_direct::<JsonDigestSink>` |
| 2.4% | `parse_that_regex::number::materialize_u64` |
| 0.8% | `JsonDigestSink` sink method |

Top lines:

| Self | Line | Meaning |
|---:|---|---|
| 57.8% | `generated.rs:517` | array element is object: recurse through `parse_object_direct` |
| 13.7% | `generated.rs:477` | object value is object: recurse |
| 11.6% | `generated.rs:485` | object numeric branch |
| 3.5% | `generated.rs:503` | object value match epilogue |
| 2.3% | `generated.rs:478` | object value is array: recurse |
| 1.6% | `generated.rs:476` | object value dispatch match |
| 1.5% | `generated.rs:480` | object string branch |
| 1.0% | `parse-that-regex/number/mod.rs:248` | u64 materializer |

Reading: direct `instruments` is even more clearly a generated dispatch/control
row. `generated.rs:517` alone holds 57.8% of self-time.

## Current gate rows

Current checked-in `skinny/RESULTS.md`:

| Corpus | Workload | Track 1 | Track 2 | sonic strict | Track 1 / sonic | Track 2 / sonic | Gate reading |
|---|---:|---:|---:|---:|---:|---:|---|
| `citm_catalog` | parse_only | 31784 | 20817 | 25509 | 124.6% | 81.6% | Track 1 fast; Track 2 below the 1.10x ns-slack equivalent. |
| `instruments` | parse_only | 18038 | 11678 | 16312 | 110.6% | 71.6% | Track 1 fast; Track 2 well below slack. |
| `citm_catalog` | direct_to_struct | 21438 | 20280 | 19966 | 107.4% | 101.6% | Direct row already passes. |
| `instruments` | direct_to_struct | 11972 | 11086 | 12673 | 94.5% | 87.5% | Generated Track 1 near enough; Track 2 is the weak side. |

Implication: W6 can plausibly improve generated/direct hot code, especially
`instruments` direct Track 1, but the parse-only close is gated by Track 2
unless W6 also touches `bbnf-bench/src/track2/json.rs` or the gate is scoped
to Track 1 generated throughput.

## W5 rejection effect

W5 item 83 matters because it removes the obvious top-leaf intervention:

- Candidate: generated-retained StringBlock16 wrapper over
  `bbnf-simd::aarch64::string_block::scan_string_special_block`.
- Consumer: only `match_tiny_plain_string_with_cap::<16>` in generated
  retained parse; direct `CAP=8`, Track 2, parse-that-regex, and materializer
  surfaces untouched.
- Correctness/parity: green.
- Gate: failed decisively. Zero of six named rows crossed threshold; every
  named Track 1 row regressed beyond the 3% guard.
- Measured examples: `twitter` Track 1 10076 Mbps (-36.0% guard outcome),
  `update_center` 7375 (-34.1%), `unicode_basic` 7173 (-37.2%), `random` 5524
  (-43.8%), `unicode_mixed` 6646 (-17.3%), `distinct_values` 6111 (-8.2%).

Therefore the fact that `generated.rs:173` remains the top individual parse
leaf is not enough to make it the next admissible target. Any future tiny-string
candidate needs fresh PC evidence for a lower-overhead inline/asm
first-special extractor. W6 should not compensate by widening parse-that full
string scanning or by reopening materializer routes.

## Viability of W6 control/key compaction

Verdict: viable, but only with a precise gate statement.

Supported parts:

- `citm_catalog` retained parse has a 38.91% container/key/control cluster
  before including offset emission. That is larger than the rejected
  tiny-string leaf.
- `instruments` retained parse has a 30.11% narrow cluster, and 41.92% if key
  quote consume and offset emission are counted. That is enough to justify a
  small branch/control compaction attempt.
- Direct Track 1 evidence is strongly aligned with W6: `citm_catalog` and
  `instruments` spend 56-59% in `parse_array_element_at_direct`, and the top
  line on both is `generated.rs:517` object-in-array recursion/dispatch.
- A4 says the open parse-that-regex work is not a missing citm/instruments
  primitive; the current gap is shape/codegen contract work.
- Comparator context agrees for `citm`: simdjson's local profile records
  container bookkeeping as essentially free (`visit_object_*`/`visit_array_*`
  near 0%) while source-byte structural classification and string/number bodies
  dominate. That gives W6 a plausible direction: reduce per-container branch
  churn rather than add another string primitive.

Risks:

- No current PC-level profile for parse Track 2 was found. The live parse rows
  are Track 2-limited, so generated-only compaction may improve already-fast
  Track 1 without closing the parse gate.
- `instruments` retained parse still has `generated.rs:173` as the largest
  single leaf. W6 must be judged as an aggregate-control intervention, not as
  "dominant single leaf removal."
- `consume_container_next` alone is 12-13%, not a 30% leaf. A narrow one-leaf
  patch will likely be insufficient. The plausible intervention must compact
  the repeated branch pattern across `parse_key_colon`, `consume_container_next`,
  `consume_array_next`, `consume_structural`, and offset emission.

## Recommended W6 acceptance criteria

Before implementation:

1. Add or obtain a parse Track 2 profile for `citm_catalog` and `instruments`
   if the W6 gate still includes Track 2 closure.
2. Treat `generated.rs:173` as off-limits for W6 unless new evidence names a
   different, lower-overhead tiny-string implementation than the W5 wrapper.
3. Define the target cluster as:
   `parse_key_colon` + `consume_container_next` + `consume_array_next` +
   `consume_structural` + `consume_quote_at_cursor` + `emit_plain_offset`.

Implementation shape to test:

- A local compaction of next-byte resolution and offset emission, preferably
  reducing duplicate `cursor < len`, `get_unchecked`, `skip_ws`, and
  close/comma branch ladders.
- If a key-byte run scan is introduced, it must have a same-wave consumer in
  the key/colon path and must not be the rejected W5 movemask wrapper under a
  new name.
- Mirror the same shape into Track 2 if the parse gate is unchanged.

Reject W6 if:

- Only generated Track 1 improves while parse Track 2 remains below slack.
- The patch mainly retouches `match_tiny_plain_string_with_cap::<16>`.
- It increases direct `instruments` Track 2 or parse Track 2 cost by more than
  the existing W5/W6 no-regression guard.
