# SK-V6 R2b parser dispatch/control-flow report

Date: 2026-05-14.
Workspace: `/Users/mkbabb/Programming/bbnf-lang`.
Scope: retained Track 1 generated runtime control-flow overhead outside string
scan. No repo files were edited, staged, or committed.

Pre-existing dirty/staged state observed before work:

- `skinny/crates/bbnf-bench/src/metadata.rs`
- `skinny/xtask/src/bin/capacity_probe.rs`

## Authority And Rejections

Current retained parse authority is `skinny/RESULTS.md`: 13 retained parse G
rows, four retained A rows (`canada`, `mesh`, `marine_ik`, `numbers`), and
Track 1 is generated runtime.

Relevant fresh redress:

- REDRESS 60 rejected removing retained `match_tiny_plain_string`; the focused
  row gate regressed every measured row.
- REDRESS 61 rejected the 64-byte trusted string scan as tested; it helped
  some long-string rows but failed the full retained matrix, with `canada` and
  `instruments` over budget.

This report does not reopen SK-V5 UTF-8 fusion, tiny-probe deletion, retained
side tables, parser-local structural cursors, or new string-scan primitives.

## Files Inspected

- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/runtime/src/grammars/json/parser.rs`
- `skinny/crates/codegen/src/json_templates/generated.rs`
- `skinny/crates/codegen/src/json_templates/parser.rs`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/parse-that-regex/src/lib.rs` for `skip_json_whitespace`
- `skinny/crates/codegen/src/json_sink_direct.rs` only to confirm it is direct
  path only and not the retained parser control surface.

Retained codegen is static-template driven. `codegen/src/lib.rs` includes
`json_templates/generated.rs` directly, then appends generated `SinkOnly`
direct code. There is no retained parser lowerer surface beyond the runtime
file and this template; any retained parser control-flow experiment must update
both.

## Quick Profiles

Built current HEAD attribution binary:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
export CARGO_TARGET_DIR=/tmp/skv6-cargo/R2b
cargo build --release -p xtask --bin profile-lazy --features runtime/parse-attribution
```

Captured with:

```bash
samply record --rate 4000 --main-thread-only --unstable-presymbolicate \
  --save-only --no-open -o /tmp/skv6-R2b-profiles/<row>.profile.json.gz \
  /tmp/skv6-cargo/R2b/release/profile-lazy <iters> <row-or-path>
```

These Mbps are attribution-build values, useful for local profile shape but
not replacements for `skinny/RESULTS.md`.

| Row | Profile | Iters | Samples | Mbps | c/B at 3.5 GHz |
|---|---|---:|---:|---:|---:|
| `citm_catalog` | `/tmp/skv6-R2b-profiles/citm_catalog.profile.json.gz` | 3000 | 10486 | 15872 | 1.76 |
| `canada` | `/tmp/skv6-R2b-profiles/canada.profile.json.gz` | 2500 | 14507 | 12493 | 2.24 |
| `random` | `/tmp/skv6-R2b-profiles/random.profile.json.gz` | 8000 | 19034 | 6867 | 4.08 |

## Attribution Findings

Self-time by generated symbol:

| Row | Dominant non-string control symbols | String self | Non-string named self |
|---|---|---:|---:|
| `citm_catalog` | `consume_container_next` 21.2%, `skip_ws` 9.7%, `emit_plain_offset` 9.0%, `consume_structural` 8.8%, `parse_key_colon` 7.9%, `dispatch_value` 4.2% | 22.9% | 76.3% |
| `canada` | `match_number_at_digit` 40.1%, `consume_container_next` 18.1%, `emit_plain_offset` 11.0%, `consume_structural` 7.8%, `dispatch_value` 6.1%, `parse_value_at` 2.7% | 0.0% | 99.8% |
| `random` | `consume_container_next` 15.4%, `parse_key_colon` 7.4%, `emit_plain_offset` 6.7%, `consume_quote_at_cursor` 5.6%, `dispatch_value` 4.2%, `consume_structural` 2.8% | 45.9% | 53.5% |

Important boundaries:

- `parse_value_at` does a bounds check, unchecked load, then delegates to
  `dispatch_value` (`generated.rs:37-42`).
- `dispatch_value` performs value-kind dispatch and calls object/array/string/
  number/literal helpers (`generated.rs:47-58`).
- `parse_object` and `parse_array` are loops over `parse_pair` or
  `parse_value_at`, followed by `consume_container_next` (`generated.rs:62-78`
  and `121-137`).
- `consume_container_next` re-loads the current byte, maybe calls
  `skip_json_whitespace`, compares comma/close, emits close offsets, and
  returns `Result<bool, ParseError>` (`generated.rs:299-327`).
- `parse_key_colon` performs the key quote/string work and then a second colon
  boundary with another direct-byte/whitespace path (`generated.rs:90-116`).
- `error` is cold and did not show as a hot leaf. `alloc::fmt::format_inner`
  samples are profiler/symbolication noise, not parser error construction.

Call-boundary interpretation has to be conservative. In normal release builds
these helpers are `inline(always)`; under `runtime/parse-attribution` they are
intentionally `inline(never)`. Therefore a candidate that only removes Rust
function calls is not admissible. The actionable evidence is the repeated
container transition work itself: delimiter byte load, whitespace boundary,
close/comma branch, close offset emit, then a fresh value dispatch load on the
next loop iteration.

## Dominant Non-String Candidates

1. Container transition / dispatch carry.

   `consume_container_next` is the largest non-string parser-control symbol on
   all three sampled rows: 21.2% `citm_catalog`, 18.1% `canada`, 15.4%
   `random`. The array-heavy `canada` profile also spends 6.1% in
   `dispatch_value` and 2.7% in `parse_value_at`, immediately after the
   delimiter transition. This suggests repeated "finish value -> find comma or
   close -> skip after comma -> re-enter parse_value_at -> reload and dispatch"
   is the main retained parser-control cluster.

2. Offset emission.

   `ParserState::emit_plain_offset` is 6.7-11.0% across the sampled rows. This
   is real retained tape cost, but it is substrate work rather than parser
   dispatch. REDRESS already records capacity/offset storage work, and changing
   this would become a tape representation intervention, not a Wave 1b
   parser-control candidate.

3. Number scanner on numeric rows.

   `match_number_at_digit` is 40.1% on `canada`. That is a parse primitive
   boundary, not object/array dispatch overhead. It is not a candidate for this
   dispatch/control-flow redeploy.

4. Result/Error construction.

   No hot evidence. The `Result`-returning APIs are visible in source, but cold
   `error` construction is absent from sampled hot leaves. Do not prescribe an
   error-type rewrite from this evidence.

5. Bounds checks.

   Bounds checks are present at each source-byte boundary, but individual
   checks did not emerge as separate profile symbols. They matter only as part
   of the container transition cluster, not as a standalone "remove bounds
   checks" prescription.

## One Falsifiable Candidate

Candidate: generate a loop-local container transition that carries the next
value byte across the comma boundary, instead of returning only `bool` from
`consume_container_next` and re-entering `parse_value_at` for a fresh
bounds/load/dispatch.

Shape:

- Keep a single source substrate. No side table, no cursor prepass, no retained
  structural index, no whitespace bitmap.
- Split retained object and array loops in `generated.rs` and
  `json_templates/generated.rs` so the container transition returns a local enum
  like `ContainerNext::{NextValue(u8), Done}` or directly tail-dispatches after
  comma whitespace skipping.
- For arrays, after comma and whitespace, load the first byte once, set the
  cursor, and call `dispatch_value` directly with that byte. This removes the
  next iteration's `parse_value_at` bounds/load layer and gives the compiler one
  loop body for "value then delimiter then next value".
- For objects, after comma and whitespace, call a key-at-current helper that
  assumes the loop boundary has already positioned the cursor but still validates
  the opening quote. Do not remove `match_tiny_plain_string`.
- Preserve existing close offset emission and existing whitespace validation.
- Keep direct `SinkOnly` code untouched; this is retained parse only.

Expected rows:

- Primary: `citm_catalog`, `canada`, `random`.
- Secondary if the shape is real: `mesh`, `marine_ik`, `numbers`,
  `apache_builds`, `github_events`.
- It is not expected to fix `unicode_mixed`, `unicode_escapes`, or
  `y_string_unicode`; those remain string-bound and REDRESS 61 already covers
  the rejected long-string scan attempt.

Exact gate:

- Build normal release Track 1 candidate and baseline from the same tree with
  `CARGO_TARGET_DIR=/tmp/skv6-R2b-baseline` and
  `/tmp/skv6-R2b-candidate`.
- Focused `profile-lazy` Mbps gate:
  - `canada` improves >= 5%.
  - `citm_catalog` improves >= 3%.
  - `random` improves >= 3%.
  - No measured retained row among `twitter`, `unicode_mixed`,
    `unicode_basic`, `apache_builds`, `github_events`, `instruments`,
    `distinct_values`, `y_string_unicode` regresses more than 2%.
- Attribution gate under `runtime/parse-attribution`:
  - On `citm_catalog` and `canada`, combined self time for
    `consume_container_next + parse_value_at + dispatch_value` drops by at
    least 25% relative.
  - `match_tiny_plain_string + match_string_at_quote` must not increase by more
    than 5% relative on `random`; if it does, the candidate is just moving cost
    into string boundaries.
- Full advisory gate before acceptance:
  - `cargo xtask bench-json --advisory` must show no retained Track 1 row
    regression above 2% and no current A row (`canada`, `mesh`, `marine_ik`,
    `numbers`) crossing back into NO-GO.

Reject immediately if the focused Mbps gate fails. This candidate is small
enough that failure should be recorded as another parser-control redress, not
iterated into a sidecar cursor or structural prepass.

## No-Admissible-Candidate Notes

There is no admissible candidate in:

- deleting `match_tiny_plain_string` (REDRESS 60);
- widening trusted string scan again without a new symbol/falsification setup
  (REDRESS 61);
- changing `Result`/`ParseError` shape without hot error evidence;
- adding retained side tables, structural cursors, whitespace cursors, or
  second scanners (REDRESS 50, 51, 53);
- removing release call boundaries only, because the current normal parser
  already compiles these helpers as `inline(always)`.

Bottom line: the only profile-backed non-string retained parser candidate I
would carry forward is container transition / next-byte dispatch carry. It has
a clear hot cluster and a tight falsifiability gate. If it fails, the remaining
large costs are tape offset emission, number scanning, and string scanning, not
generic parser dispatch.
