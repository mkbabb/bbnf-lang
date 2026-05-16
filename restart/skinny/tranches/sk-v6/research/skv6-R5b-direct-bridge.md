# SK-V6 Wave 1b R5b - Direct/Retained Bridge Boundary

Date: 2026-05-14.
Workspace: `/Users/mkbabb/Programming/bbnf-lang`.
Scope: read-only; no repo files edited, staged, or committed.

## Read Set

- `restart/skinny/tranches/sk-v6/SYNTHESIS-WAVE-1-PLAN.md`, especially Candidate 3.
- `skinny/REDRESS.md` entries 54, 55, and 57.
- Generated retained/direct runtime and sink code:
  - `skinny/crates/runtime/src/grammars/json/generated.rs`
  - `skinny/crates/runtime/src/grammars/json/sink.rs`
  - `skinny/crates/runtime/src/grammars/json/view.rs`
  - `skinny/crates/runtime/src/grammars/json/parser.rs`
  - `skinny/crates/codegen/src/json_sink_direct.rs`
  - `skinny/crates/bbnf-bench/src/direct_struct.rs`

I also noticed current `skinny/REDRESS.md` now contains items 60 and 61, which
reject the first two retained Wave 2 candidates as tested. The boundary below
does not depend on those items, but they make the conclusion stricter.

## Answer

The field-layout/string receiver change belongs strictly to Wave 3 direct work.
It should not be used as a retained Wave 2 intervention.

The only machinery that can be shared safely is a low-level string span/escape
scanner in `parse-that-regex` or `bbnf-simd`, because both retained and direct
parsers call the trusted JSON string matcher. But the direct field-layout
materializer itself has no retained parse consumer: retained parse records raw
source offsets plus `HAS_ESC` flags into the tape and decodes only later through
views. Adding decoded field facts to retained parse would either change the
retained output plane or create a parallel retained projection, which conflicts
with the single-substrate and same-wave-consumer rules.

## Evidence

Retained parse writes offsets only:

- `skinny/crates/runtime/src/grammars/json/generated.rs:90-116` parses object
  keys, marks `HAS_ESC` on the opening quote cursor when needed, and advances
  over the raw string.
- `skinny/crates/runtime/src/grammars/json/generated.rs:140-188` parses string
  values the same way: tiny plain early-out, trusted full matcher, optional
  flag patch, no decoded materialization.
- `skinny/crates/runtime/src/grammars/json/parser.rs:35-42` exposes only
  `emit_plain_offset` and `patch_flags` to generated retained parse.
- `skinny/crates/runtime/src/grammars/json/view.rs:206-216` performs
  `unescape_json_string` lazily in `JsonString::as_str`.

Direct parse materializes through the sink boundary:

- `skinny/crates/runtime/src/grammars/json/generated.rs:390-393`,
  `430-433`, `470-473`, and `511-514` call `string_source`,
  `object_string_source`, `array_string_source`, and `key_source`.
- `skinny/crates/runtime/src/grammars/json/generated.rs:560-590` returns
  `ParsedString { raw, needs_unescape }` from `parse_string_direct`.
- `skinny/crates/runtime/src/grammars/json/sink.rs:16-35`, `43-52`, and
  `84-93` are where escaped raw strings are decoded before reaching receiver
  methods.
- `skinny/crates/bbnf-bench/src/direct_struct.rs:59-63`, `123-128`,
  `300-312`, and `340-372` are the current digest receiver/fold hot path.

REDRESS keeps this boundary:

- Entry 54 rejects sink-local exact decoded stats: it regressed the
  escape-heavy direct rows even though it avoided allocation.
- Entry 55 rejects quote-source streaming hash: even one-pass source-hook
  streaming lost to the default allocate-then-contiguous-hash baseline.
- Entry 57 admits direct receiver inlining and generated direct tiny-string
  raw-span routing only as bounded direct-to-struct work, explicitly not as a
  retained parse-G fix. It also says the remaining direct close is a fused
  field-layout decoded string materializer, not another sink-local decoded hash
  path.

## Boundary

### Retained Wave 2 Candidate

Admissible retained work is string span discovery, not field materialization.
The retained candidate class is:

- Improve the trusted JSON string scan or its generated wrapper while preserving
  retained output as raw offset tape plus flags.
- If a new primitive is introduced, it must have scalar reference and same-wave
  retained consumer.
- It may live in shared string-scanning code, but the retained row must be the
  falsification target.

Exact files:

- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/codegen/src/json_templates/generated.rs`
- `skinny/crates/parse-that-regex/src/lib.rs`
- Optional only if a new SIMD primitive is needed:
  `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`

Do not route this through:

- `skinny/crates/runtime/src/grammars/json/sink.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`

Falsifiability gate for a retained redispatch:

- Rows: `unicode_mixed`, `gsoc-2018`, `y_string_unicode`, `twitter`,
  `random`, `unicode_basic`, and `distinct_values`.
- Throughput: canonical retained Track 1 must improve by at least 10% on at
  least two of `unicode_mixed`, `gsoc-2018`, and `y_string_unicode`, or by at
  least 5% on at least four named retained rows.
- Safety: no retained row may regress by more than 3%; no direct improvement
  may be credited unless direct rows are also measured.
- Attribution: if the new helper remains under the same wrapper symbol, use
  row Mbps and c/B as the gate; otherwise the new helper's self-time must
  explain the row delta.

### Direct Wave 3 Candidate

Admissible direct work is a field-layout or same-loop `SinkOnly` materializer
that produces the receiver's typed string facts directly, beating the current
allocate-then-contiguous-hash baseline. It must not be another sink-local exact
stats helper or quote-source streaming hash.

Exact files:

- `skinny/crates/runtime/src/grammars/json/sink.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/codegen/src/json_sink_direct.rs`
- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`

Falsifiability gate:

- Dispatch only after retained parse-G is back to `<= 4` or the retained parse
  shortlist is exhausted.
- Rows: `unicode_escapes`, `unicode_mixed`, `y_string_unicode`,
  `distinct_values`, and `gsoc-2018` direct-to-struct.
- Throughput: `unicode_escapes` direct Track 1 must improve by at least 20%;
  `unicode_mixed` direct Track 1 must improve by at least 15%.
- Safety: no direct row may regress by more than 5%.
- Attribution: combined `unescape_json_string + parse_string_direct` share must
  drop by at least 20% on `unicode_escapes` and `unicode_mixed`.
- Rejection rule: if it improves allocation counters but does not beat the
  current row Mbps baseline, reject it under REDRESS 54/55.

## Sharing Rule

Allowed sharing:

- A grammar-neutral scanner/decoder helper in `parse-that-regex` or a checked
  primitive in `bbnf-simd` can be shared if the active wave has its own
  same-wave consumer and row gate.
- A retained wave may change shared string matching code, because direct
  `parse_string_direct` also calls the trusted matcher; direct rows then need
  a regression check if behavior or hot code changes.

Disallowed sharing:

- Do not add decoded string field facts to retained tape to reuse a direct
  receiver. That is a new retained projection/parallel substrate.
- Do not add a direct field-layout materializer in Wave 2 and call it shared
  because it happens to use the same raw span. The consumer is `JsonSink`, not
  retained `OffsetTape`.
- Do not reopen REDRESS 54/55 as "shared machinery"; both failed exactly at the
  direct sink/materialization boundary.

Net: retained Wave 2 owns raw string span/flag discovery. Direct Wave 3 owns
decoded field-layout delivery. The bridge is a helper boundary, not the
materializer.

End of report.
