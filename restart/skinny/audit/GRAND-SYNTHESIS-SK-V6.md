# SK-V6 Grand Synthesis

Date: 2026-05-14.

Authority: SK-V6 Wave 1 cohort reports under
`restart/skinny/audit/SK-V6-COHORT/`, current `skinny/RESULTS.md`, and
`skinny/REDRESS.md` entries 50-59. This document is the Wave 1 PLAN artifact.
It does not prescribe a kernel from SK-V5 hypothesis transfer; every candidate
below is tied to fresh generated-runtime profile evidence.

## 1. Current Gate

The current measured authority remains `N-direct / NoGo`.

Retained parse has 13 outcome-G rows and four A rows (`canada`, `mesh`,
`marine_ik`, `numbers`). Direct-to-struct has four passing rows
(`citm_catalog`, `mesh`, `marine_ik`, `numbers`) and 13 red rows. Track 1 is
now generated runtime on the direct plane: R3 verified every sampled direct row
reaches `runtime::generated_json::parse_direct`.

Canada structural scan remains green at 41495 Mbps against the 40000 Mbps NEON
floor. The remaining work is not a structural scan floor repair.

## 2. Wave 1 Evidence Map

| Report | Scope | Binding finding |
|---|---|---|
| R1 | 9 retained regressed-from-PASS rows | Seven rows are generated string-wrapper bound; `citm_catalog` is structural/container churn; `marine_ik` is number/container and remains a retained GO control. |
| R2 | 4 original retained parse-G rows | Original G rows remain string-bound, but the current hot leaves are `match_tiny_plain_string` and `match_string_at_quote`, not `validate_utf8_codepoint`. |
| R3 | 13 direct red rows | Generated Track 1 is real. Direct rows split into string receiver/fold overhead, escaped-string decode/materialization, and number/emission residuals. |
| R4 | SK-V4 largest-regression diff | Largest retained regressions are string-boundary recognition in the honest generated runtime; no accessible SK-V4 leaf table splits below fused `parse_value_at`. |
| R5 | Sidecar refresh | Current strict rows live in `RESULTS.md` for sonic-rs / Rust simd-json; C++ simdjson and yyjson are stale profile-only until sources return; asmjson SWAR is permissive flaw-probe only. |
| R6 | Lock 15 / PMU proxy | Lock 15 holds by size. Default `parse_value_at` is 8768 B; parse-attribution splits it. No branch/L1i/IPC counters were accessible without privileged tooling. |

## 3. Diagnosis Revisions

### A. Retained String-Wrapper Cluster

Rows: `twitter`, `random`, `unicode_mixed`, `unicode_basic`,
`apache_builds`, `github_events`, `update_center`, `gsoc-2018`,
`distinct_values`, `y_string_unicode`, and the string share of `instruments`.

Revision: the retained parse blocker is not raw UTF-8 validation. The current
generated runtime admits the input as `&str` before the loop and calls the
trusted string matcher. Fresh attribution exposes
`generated::match_tiny_plain_string` and `generated::match_string_at_quote` as
the hot leaves. R2 reports combined string-boundary self-time of 56.6% on
`twitter`, 43.8% on `random`, 78.8% on `unicode_mixed`, and 53.5% on
`unicode_basic`. R1/R4 report the same shape on the newly regressed rows:
`distinct_values` is 81.3% string-boundary, `gsoc-2018` is 84.8%, and
`y_string_unicode` is dominated by `match_string_at_quote`.

Implication: SK-V5's broad "string boundary" diagnosis survives, but the
specific "fold raw UTF-8 validation into the NEON body scan" prescription is
invalid on the current baseline.

### B. Retained Structural/Container Cluster

Rows: `citm_catalog`, with the same shape present as a secondary component in
`instruments` and `marine_ik`.

Revision: structural cost is fragmented across generated boundaries, not a
single missing substrate. R1 records `citm_catalog` at 57.0% structural share
spread through `consume_container_next`, `skip_ws`, `consume_structural`,
`emit_plain_offset`, and `parse_key_colon`. R6 confirms 13 generated hot leaves
above 1% and no single i-cache-capacity fault.

Implication: the pre-blocked routes remain blocked. Retained side tables,
EventCursor/sidecar prepasses, capacity prescan, and generic SWAR whitespace do
not target the current fragmented generated-container cost and already carry
measurement rejections.

### C. Direct String/Unicode Cluster

Rows: `unicode_mixed`, `unicode_escapes`, `y_string_unicode`, plus the string
share of `twitter`, `apache_builds`, `github_events`, `update_center`,
`random`, `gsoc-2018`, `unicode_basic`, and `distinct_values`.

Revision: direct string rows now prove generated Track 1 and expose two
separate costs. Non-escaped or lightly escaped rows show generated
string/object parse plus receiver folding: R3 records receiver or fold symbols
above 10% on `apache_builds`, `github_events`, `update_center`, `random`,
`gsoc-2018`, `unicode_basic`, and `distinct_values`. Escape-heavy rows are
decode/materialization bound: `unicode_escapes` is 46.9%
`unescape_json_string` and 43.4% `parse_string_direct`; `unicode_mixed` is
22.8% `unescape_json_string` and 51.1% `parse_string_direct`.

Implication: the admissible direct-string class is a field-layout or same-loop
SinkOnly materializer that beats allocate-then-contiguous-hash. REDRESS 54/55
remain rejected; a sink-local decoded-stat helper or quote-source streaming
hash does not become valid merely because decode remains hot.

### D. Direct Number/Emission Cluster

Rows: `canada` direct remains red; `numbers` direct is passing; `mesh` and
`marine_ik` direct are passing in current `RESULTS.md`.

Revision: Eisel-Lemire is wired and the old `serde_json::parse_number` direct
diagnosis is obsolete for Track 1. R3 records `canada` direct as
`parse_number_array_direct` 49.1%, `materialize_f64` 12.3%, and
`emit_number_array_direct` 11.2%.

Implication: no Wave 2 retained-parse intervention should be routed through
number parsing. Direct `canada` can be addressed later by generated
number-array materialization/emission shape, but it is not the first retained
parse close.

### E. Front-End / I-Cache Cluster

Rows: all retained rows under the Lock 15 concern.

Revision: i-cache capacity is not the current explanation. R6 measured the
default fused `parse_value_at` at 8768 B, below the 20 KiB Lock 15 budget; the
parse-attribution hot set is below 10 KiB even including
`structural_capacity_for`. Branch density is plausible in compact string and
number leaves, but real PMU counters were inaccessible in this session.

Implication: do not prescribe an i-cache split as Wave 2. The useful follow-up
is a privileged PMU diagnostic on `gsoc-2018` and `marine_ik`, not a kernel
change.

## 4. SK-V5 Diagnosis Ledger

Reconfirmed:

- Generated direct Track 1 was mandatory. R3 verifies the old bench-private
  path is gone and every direct row reaches generated `parse_direct`.
- Codegen/substrate overhead is now honestly measurable. The direct rows expose
  generated parse, receiver folding, decode/materialization, and number emit
  symbols rather than `SinkParser::*`.
- String-heavy rows remain the dominant retained and direct blocker.
- Eisel-Lemire closed the missing-number-algorithm class; remaining number work
  is emission/materialization shape, not vendor-and-wire.
- Strictness disclosure remains mandatory. R5 confirms asmjson SWAR is still
  permissive flaw-probe only; strict target rows must remain sonic-rs,
  simd-json, simdjson C++, and yyjson with output planes named.

Invalidated or narrowed:

- The SK-V5 Wave 3 UTF-8 fusion prescription is invalid on the generated
  retained baseline. No R1/R2/R4 retained profile shows `validate_utf8_codepoint`
  as the hot leaf.
- Active Class A retained `match_tiny_plain_string` NEON wiring remains
  rejected. The hot symbol is `match_tiny_plain_string`, but REDRESS 28/33
  prove the prior retained wiring route regressed; the admissible candidate is
  boundary collapse, not standalone tiny-string SIMD.
- Old direct `SinkParser::*` attribution is obsolete. Direct Track 1 now uses
  generated runtime.
- Old direct `serde_json::parse_number` attribution is obsolete. Track 1 uses
  parse-that number materialization.
- Cost-model work is not aspirational. The present rows show lowering shape,
  receiver shape, and materialization shape directly affect the gate.

## 5. Wave 2 Candidate Shortlist

Wave 2 admits one intervention per dispatch. The shortlist is ordered by
expected retained parse impact, because SK-V6 Wave 2 targets parse-G recovery
before direct N-direct closure.

### Candidate 1: Collapse Retained Trusted String Boundary

Path:

- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/codegen/src/json_templates/generated.rs`
- `skinny/crates/codegen/src/json_sink_direct.rs` only if generated direct
  templates share the same helper boundary
- `skinny/crates/parse-that-regex/src/lib.rs` only if a shared helper is needed
  to preserve semantics without adding a new primitive

Mechanism: remove the double-probe shape where generated retained parsing runs
`match_tiny_plain_string` and then falls back to `match_string_at_quote` that
rescans from the quote. Replace it with a single trusted matcher boundary that
returns the same span/flags and preserves the short-string fast return without
making Class A NEON the retained parse fix.

Expected row impact: `twitter`, `random`, `unicode_basic`, `apache_builds`,
`github_events`, `update_center`, `distinct_values`, and `instruments` should
benefit most. `unicode_mixed`, `gsoc-2018`, and `y_string_unicode` may remain
full-string-scan bound after the duplicate prefix cost is removed.

Falsifiability gate:

- Focused retained release rows: `twitter`, `random`, `unicode_basic`,
  `apache_builds`, `distinct_values`, `gsoc-2018`, `y_string_unicode`.
- Parse-attribution gate: combined `match_tiny_plain_string +
  match_string_at_quote` self-time must drop by at least 20% on at least four
  of those rows.
- Throughput gate: canonical Track 1 Mbps must improve by at least 5% on
  `twitter`, `random`, `unicode_basic`, and `distinct_values`, with no retained
  row regressing more than 2%.
- Same-row rejection: if the profile self-time drops but Mbps does not move on
  the named rows, revert and record a REDRESS rejection.

Why this is admissible: it targets the fresh generated-runtime hot boundary and
does not reopen retained Class A NEON wiring, raw UTF-8 fusion, sidecar masks,
or decoded direct-string hashing.

### Candidate 2: Retained Long-String Trusted Scan Specialization

Path:

- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`
- scalar reference and checkasm only if a new SIMD primitive is introduced
- same-wave consumer in `runtime/src/grammars/json/generated.rs`

Mechanism: specialize the trusted full string scan used by
`match_string_at_quote` for already-UTF-8-valid input. The target is quote,
backslash, and control detection over long strings, not raw UTF-8 validation.
If implemented as a primitive, it must have a scalar executable spec and a
same-wave retained parser consumer.

Expected row impact: `unicode_mixed`, `gsoc-2018`, `y_string_unicode`, and
`twitter` long-string segments. This is the second candidate because it should
only be attempted if Candidate 1 leaves `match_string_at_quote` above 45%
self-time on long-string rows.

Falsifiability gate:

- Focused retained rows: `unicode_mixed`, `gsoc-2018`, `y_string_unicode`,
  `twitter`.
- Parse-attribution gate: `match_string_at_quote` self-time must drop below
  45% on `unicode_mixed`, `gsoc-2018`, and `y_string_unicode`.
- Throughput gate: Track 1 must improve by at least 10% on at least two of
  those three long-string rows with no row regression above 3%.

Why this is admissible: it uses the new R1/R2/R4 evidence that trusted string
scan itself is hot. It is not the SK-V5 raw UTF-8 fusion class and must not add
a retained sidecar substrate.

### Candidate 3: Direct Field-Layout String Materializer

Path:

- `skinny/crates/runtime/src/grammars/json/sink.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/codegen/src/json_sink_direct.rs`
- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`

Mechanism: direct-only materialization that lets generated SinkOnly produce
typed field/string digest facts in the same parse loop, without allocating a
decoded `String` and then hashing contiguous decoded bytes. This must be a
field-layout materializer or same-loop SinkOnly event, not a sink-local
decoded-stats helper and not quote-source streaming hash as previously tested.

Expected row impact: `unicode_escapes`, `unicode_mixed`, `y_string_unicode`,
`distinct_values`, and `gsoc-2018` direct. R3 shows the direct target clearly:
`unicode_escapes` is 46.9% `unescape_json_string`, `unicode_mixed` is 22.8%
`unescape_json_string`, and multiple non-escaped direct rows show receiver or
fold symbols above 10%.

Falsifiability gate:

- Dispatch only after parse-G is back to <= 4 or after the parse shortlist is
  exhausted, per SK-V6.
- Direct rows: `unicode_escapes`, `unicode_mixed`, `y_string_unicode`,
  `distinct_values`, `gsoc-2018`.
- Throughput gate: `unicode_escapes` must improve by at least 20% and
  `unicode_mixed` by at least 15%, with no direct row regressing more than 5%.
- Profile gate: `unescape_json_string + parse_string_direct` must drop by at
  least 20% combined on `unicode_escapes` and `unicode_mixed`.

Why this is admissible: it is a Wave 3 direct candidate, not the immediate
Wave 2 retained parse candidate. It preserves REDRESS 54/55 by requiring a
different materialization class and same-row falsification.

## 6. Not Candidates

- PMU/i-cache rewrite: Lock 15 holds; no real PMU counters were available.
- Raw UTF-8 fusion: no current retained hot leaf targets it.
- Retained side tables, EventCursor, structural sidecar prepass, parser-local
  structural-mask cursor, capacity prescan, generic SWAR whitespace: all are
  either pre-blocked or do not target the fresh hot leaves.
- Orphan primitives: still barred by same-wave consumer.
- asmjson SWAR parity: remains permissive flaw-probe only on this host.

## 7. Recommended Next Dispatch

Dispatch SK-V6 Wave 2 Candidate 1 only:

> Collapse retained trusted string boundary in generated runtime. HARD CAP:
> 60 min implementation + 15 min measurement. If the named retained rows do
> not move by the falsifiability gate, revert and add a REDRESS entry.

This is the highest-signal intervention because it covers both original G rows
and regressed-from-PASS rows, requires no new BIR variant, no new BBNF
directive, no parallel substrate, and no orphan primitive. It is also the most
direct test of the fresh Wave 1 finding: current retained parse spends the
largest share of time in duplicate string-boundary recognition rather than raw
UTF-8 validation.

## 8. Wave 1b Redispatch After Candidate 1/2 Redress

Candidate 1 and Candidate 2 have now been falsified and recorded in
`skinny/REDRESS.md` items 60-61. The Wave 1b cohort reports live at
`restart/skinny/audit/SK-V6-COHORT/skv6-R1b-string-boundary.md` through
`skv6-R6b-measurement-attribution.md`.

Candidate 1 result: removing the retained tiny-string probe was rejected. It
regressed every focused row because the probe is a real short-string completion
path, not redundant front matter. It must stay on both retained key and string
value paths.

Candidate 2 result: an always-wide 64-byte trusted string-special scanner was
rejected. It improved long-string rows, most notably `gsoc-2018`, but failed the
full gate because short/non-string sentinel rows (`canada`, `instruments`) paid
the wider first probe without enough long-string body to amortize it.

### Wave 1b Diagnosis Revision

- The retained string problem is distributional. `gsoc-2018`,
  `unicode_mixed`, and `unicode_basic` are long-byte dominated;
  `y_string_unicode` and `unicode_escapes` are escape dominated;
  `distinct_values` is mid-value dominated; `citm_catalog`, `instruments`, and
  `marine_ik` are short-key dominated.
- A single always-on string scanner is therefore the wrong abstraction. The
  generated retained parser needs a tiny-first, medium/long-gated string scan
  policy rather than either deleting the tiny probe or starting every fallback
  with a wide block.
- Non-string retained overhead is also real. R2b measured
  `consume_container_next` at 15-21% self on sampled rows and
  `emit_plain_offset` at 7-11%. Those are separate candidates; they should not
  be mixed into the next string intervention.
- The strict competitor lesson is broader than JSON: simd-json wins by making
  structural events the parse substrate. That route is not an immediate Wave 2
  redress because SK-V6 pre-blocks sidecar structural-index/EventCursor
  producer shapes. It belongs in the global architecture feedback loop as a
  Lock 1 substrate-union review item, not as an unreviewed Wave 2 patch.

### Candidate 3: Delayed-Wide Trusted String Scan

Path:

- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_string_block_64.rs`
- `skinny/xtask/src/main.rs`

Mechanism: keep `match_tiny_plain_string` unchanged. In
`skip_json_string_plain_trusted`, preserve the existing first 16-byte AArch64
probe. Only after the first 16-byte block reports no quote, backslash, or
control byte may the scanner enter a 64-byte block loop. This keeps the short
and key-dominated rows on the current cheap path while recovering the long-body
gain seen in REDRESS 61. If a 64-byte primitive is reintroduced, it must have a
scalar executable spec, checkasm parity, and a parse-attribution-visible helper
or PC-bin attribution so the wrapper-symbol problem is not repeated.

Expected row impact: `gsoc-2018`, `unicode_mixed`, `apache_builds`,
`github_events`, `update_center`, and `unicode_basic` should gain. `canada`,
`instruments`, `marine_ik`, `citm_catalog`, and `numbers` are sentinel rows
because Candidate 2 regressed short/non-string shapes.

Falsifiability gate:

- Production `profile-lazy` smoke: all 17 retained rows, three repetitions,
  median c/B, using the command ritual from `skv6-R6b-measurement-attribution`.
- Proceed to full Criterion only if `gsoc-2018` improves by at least 10% c/B,
  at least one of `unicode_mixed` or `apache_builds` improves by at least 6%,
  and no retained row regresses by more than 1.5% c/B.
- Full `bench-json --advisory` acceptance: `gsoc-2018` improves by at least
  10% Track 1 Mbps, at least two of `unicode_mixed`, `apache_builds`,
  `github_events`, `update_center`, and `unicode_basic` improve by at least
  5%, and no retained row regresses by more than 2%.
- Same-row rejection: if the delayed-wide path improves only the focused
  string rows but again regresses `canada` or `instruments`, revert and record
  the route as a blocked always/wide-string class extension.

Why this is admissible: it is not Candidate 2 repeated. The first block remains
the current 16-byte primitive, the tiny probe remains load-bearing, and the
wide path is reached only after row-local evidence of a longer plain span. It
does not add directives, BIR variants, retained side tables, or a parallel
substrate.

### Candidate 4: Container Next-Byte Carry

Path:

- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/codegen/src/json_templates/generated.rs`

Mechanism: split retained object/array loops so the delimiter transition after
a value carries the next value byte across the comma boundary instead of
returning only `bool` from `consume_container_next` and re-entering
`parse_value_at` for a fresh bounds/load/dispatch. Preserve whitespace
validation, close-offset emission, and the existing tape format.

Expected row impact: `citm_catalog`, `canada`, `random`, `mesh`,
`marine_ik`, `numbers`, `apache_builds`, and `github_events`. It is not
expected to close escape-heavy string rows.

Falsifiability gate:

- Focused retained rows: `canada`, `citm_catalog`, `random`, `mesh`,
  `marine_ik`, `numbers`, `apache_builds`, `github_events`, plus the string
  guard rows `unicode_mixed`, `unicode_basic`, `distinct_values`, and
  `y_string_unicode`.
- Throughput: `canada` improves at least 5%, `citm_catalog` and `random`
  improve at least 3%, and no measured retained row regresses more than 2%.
- Attribution: combined `consume_container_next + parse_value_at +
  dispatch_value` attributed c/B drops at least 25% on `citm_catalog` and
  `canada`.

Why this is admissible: it targets a current generated-runtime control-flow
cluster and keeps source bytes as the only retained parse substrate.

### Wave 1b Recommendation

Dispatch Candidate 3 first. It directly redresses the measured reason
Candidate 2 failed while preserving the long-string gain class. If Candidate 3
fails, do not attempt another string scanner without a new profile; dispatch
Candidate 4 as the next retained parser-control experiment. Candidate 3 is the
next Wave 2 redress target, not a Wave 3 direct-to-struct intervention.
