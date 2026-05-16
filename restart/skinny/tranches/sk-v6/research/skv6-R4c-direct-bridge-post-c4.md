# SK-V6 Wave 1c R4c - Direct-To-Struct Bridge Readiness After Candidate 4

Date: 2026-05-14
Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Scope: research only; no repository files edited.

## Inputs Read

- `skinny/RESULTS.md` direct table, current after REDRESS 63 / Candidate 4.
- `skinny/REDRESS.md` entries 54, 55, and 63.
- `restart/skinny/audit/GRAND-SYNTHESIS-SK-V6.md` §C and Candidate 3.
- Direct Track 1 source:
  - `skinny/crates/bbnf-bench/src/direct_struct.rs:401-405`
  - `skinny/crates/runtime/src/grammars/json/generated.rs:396-430`
  - `skinny/crates/runtime/src/grammars/json/sink.rs:16-92`

## Commands

Build:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv6-cargo/R4c \
  cargo build -p bbnf-bench --bin profile_direct --release \
  --features runtime/parse-attribution
```

Profiles:

```bash
samply record --rate 4000 --main-thread-only --unstable-presymbolicate \
  --save-only --no-open \
  -o /tmp/skv6-R4c-direct-profiles/<row>.profile.json.gz \
  /tmp/skv6-cargo/R4c/release/profile_direct <iters> <row> track1
```

Profile artefacts:

- `/tmp/skv6-R4c-direct-profiles/unicode_mixed.profile.json.gz`
- `/tmp/skv6-R4c-direct-profiles/unicode_escapes.profile.json.gz`
- `/tmp/skv6-R4c-direct-profiles/y_string_unicode.profile.json.gz`
- `/tmp/skv6-R4c-direct-profiles/distinct_values.profile.json.gz`
- `/tmp/skv6-R4c-direct-profiles/gsoc-2018.profile.json.gz`
- matching `*.profile.json.syms.json` files in the same directory

Note: one malformed shell-loop attempt created tiny profiles whose filenames
include the iteration count; they are ignored. The valid files are the five
row-named profile files above.

## Current Direct Gate Rows

From `skinny/RESULTS.md`:

| row | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | T1 / sonic | direct verdict |
|---|---:|---:|---:|---:|---|
| `unicode_mixed` | 3881 | 4137 | 10142 | 38.3% | NO-GO |
| `unicode_escapes` | 5143 | 5030 | 14485 | 35.5% | NO-GO |
| `y_string_unicode` | 3674 | 3679 | 8676 | 42.3% | NO-GO |
| `distinct_values` | 6072 | 5563 | 13185 | 46.1% | NO-GO |
| `gsoc-2018` | 15013 | 14458 | 24163 | 62.1% | NO-GO |

All five target rows remain direct blockers. Candidate 4 improved retained
array/control flow, but it did not materially change this direct-string plane.

## Track 1 Generated Runtime Proof

Static call chain:

- `bbnf_bench::direct_struct::track1_digest` calls
  `runtime::generated_json::parse_direct(input, &mut sink)` at
  `skinny/crates/bbnf-bench/src/direct_struct.rs:401-405`.
- The generated entry point is
  `runtime::generated_json::generated::parse_direct::<bbnf_bench::direct_struct::JsonDigestSink>`
  at `skinny/crates/runtime/src/grammars/json/generated.rs:398`.

Binary symbol proof from `/tmp/skv6-cargo/R4c/release/profile_direct`:

```text
runtime::generated_json::generated::parse_direct::<bbnf_bench::direct_struct::JsonDigestSink>
runtime::generated_json::generated::parse_string_direct
parse_that_regex::unescape_json_string
```

Sampled-path proof:

| row | `parse_direct` inclusive samples |
|---|---:|
| `unicode_mixed` | 99.98% |
| `unicode_escapes` | 99.98% |
| `y_string_unicode` | 99.94% |
| `distinct_values` | 99.95% |
| `gsoc-2018` | 99.97% |

R4c conclusion: Track 1 is generated runtime on every sampled direct row. This
is not the old bench-private `SinkParser` path.

## PC-Level Attribution

Self-sample shares from the parse-attribution build:

| row | `parse_string_direct` | `unescape_json_string` | `match_tiny_plain_string` | sink string/fold closure | notable residual |
|---|---:|---:|---:|---:|---|
| `unicode_mixed` | 49.40% | 23.62% | 3.62% | 3.97% | `_platform_memmove` 7.17% |
| `unicode_escapes` | 43.79% | 47.04% | 0.25% | 1.98% | `_platform_memmove` 1.09%, `_platform_memset` 0.51% |
| `y_string_unicode` | 14.94% | 22.48% | 0.41% | 3.73% | short-row sampling artefact: `mach_absolute_time` 28.13% |
| `distinct_values` | 22.08% | 0.00% | 21.03% | 19.20% | `parse_object_direct` 30.76% |
| `gsoc-2018` | 41.72% | 10.41% | 11.35% | 20.67% | `parse_object_direct` 6.89% |

Inclusive selected shares:

| row | `parse_string_direct` inclusive | `unescape_json_string` inclusive | generated container/object path |
|---|---:|---:|---:|
| `unicode_mixed` | 53.02% | 31.88% | object 99.96%, array 99.93%, object-value 93.53% |
| `unicode_escapes` | 44.04% | 48.97% | object 99.98%, array 99.95%, object-value 98.93% |
| `y_string_unicode` | 15.34% | 38.12% | array 99.94%, array-element 99.30% |
| `distinct_values` | 43.11% | 0.00% | object 99.55%, array 99.94%, object-value 50.73% |
| `gsoc-2018` | 53.06% | 13.08% | object 99.96%, object-value 86.78% |

The profile says the direct gap is still a string/materialization gap, but it
is not one uniform gap:

- Escape-heavy direct rows are decode/materialization bound:
  `unicode_escapes` spends 47.04% self in `unescape_json_string` and 43.79%
  self in `parse_string_direct`.
- Mixed Unicode rows split between string recognition, decode, and copy:
  `unicode_mixed` spends 49.40% self in `parse_string_direct`, 23.62% in
  `unescape_json_string`, and 7.17% in `_platform_memmove`.
- Plain/deduplicated string rows expose generated receiver/fold overhead:
  `distinct_values` spends 19.20% self in the generated sink array-string
  closure and 21.03% in `match_tiny_plain_string`; `gsoc-2018` spends 20.67%
  self in the generated sink array-string closure.

## REDRESS 54/55 Boundary

REDRESS 54 and 55 remain binding. A sink-local decoded-stat helper or
quote-source streaming materializer is not admissible; both are already
measured failures. The next direct-string intervention must be a genuine
field-layout / same-loop materializer, not a renamed sink helper:

- The generated parser must emit the direct string fact in the same parse loop
  that owns the string context.
- The materializer must avoid allocating a decoded `String` and then hashing a
  contiguous decoded buffer.
- The non-escaped path must avoid the current closure-heavy
  `array_string_source` / `object_string_source` receiver shape where the row
  profile names receiver/fold overhead.
- The escaped path must provide scalar reference semantics for Unicode escapes
  and surrogate pairs before any SIMD or specialized primitive is wired.

## Decision

R4c verdict: **Direct Field-Layout String Materializer is now the canonical
next direct-to-struct move, and it is not blocked by parse-G under the SK-V6
`parse shortlist exhausted` clause.**

Reasoning:

- The retained Wave 2 shortlist has been exercised through Candidate 4:
  Candidate 1 rejected, Candidate 2 rejected, Candidate 3 rejected, Candidate 4
  admitted but not closing SK-V6.
- `skinny/RESULTS.md` still has 13 parse-G rows, but the SK-V6 Candidate 3
  gate explicitly allows dispatch after parse-G <= 4 **or** after the parse
  shortlist is exhausted.
- The direct target is clearly measured on the new generated Track 1 baseline:
  all sampled rows spend effectively all inclusive samples under generated
  `parse_direct`.
- The five requested direct rows all remain severe N-direct blockers at
  35.5% to 62.1% of sonic-rs, and their dominant symbols match the Candidate 3
  field-layout class rather than a new retained parse scanner.

## Recommended Wave 3 Falsifiability Gate

Implement one field-layout / same-loop direct string materializer candidate and
measure only that candidate.

Rows:

- `unicode_escapes`
- `unicode_mixed`
- `y_string_unicode`
- `distinct_values`
- `gsoc-2018`

Throughput gate:

- `unicode_escapes` Track 1 >= 6172 Mbps (+20% over 5143).
- `unicode_mixed` Track 1 >= 4463 Mbps (+15% over 3881).
- At least two of `y_string_unicode`, `distinct_values`, `gsoc-2018` improve by
  >= 8%.
- No direct row regresses by >5%.

Profile gate:

- Combined self share of `parse_string_direct + unescape_json_string` drops by
  >= 20% relative on `unicode_escapes` and `unicode_mixed`.
- Sink receiver/fold closure self share drops by >= 30% relative on
  `distinct_values` and `gsoc-2018`.

If this candidate fails, record it as a new REDRESS item and do not re-open
REDRESS 54/55 under another name.

## Residual Risks

- `y_string_unicode` is small and the sample had a high `mach_absolute_time`
  artefact. Treat its row Mbps and string/unescape symbol presence as useful,
  but do not overfit its exact self percentages.
- `distinct_values` has no escape decode cost; it is the guard row that catches
  a direct materializer that only optimizes escaped strings while worsening
  plain/short string receiver costs.
- The materializer must remain grammar-general in shape: generated JSON may
  instantiate field/string facts, but generic crates must not grow JSON-only
  logic.
