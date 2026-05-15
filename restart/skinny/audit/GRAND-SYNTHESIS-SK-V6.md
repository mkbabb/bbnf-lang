# SK-V6 Grand Synthesis

Date: 2026-05-14.

Authority: SK-V6 Wave 1 cohort reports under
`restart/skinny/audit/SK-V6-COHORT/`, current `skinny/RESULTS.md`, and
`skinny/REDRESS.md` entries 50-72. This document is the Wave 1 PLAN artifact.
It does not prescribe a kernel from SK-V5 hypothesis transfer; every candidate
below is tied to fresh generated-runtime profile evidence.

## 1. Current Gate

The current measured authority remains `N-direct / NoGo`.

Retained parse has 5 outcome-G rows: `twitter`, `update_center`, `random`,
`unicode_mixed`, and `unicode_basic`. The `semantic_full_digest_stressor`
direct plane has five passing rows (`citm_catalog`, `apache_builds`,
`github_events`, `instruments`, `distinct_values`) and 12 red guard rows. The
representative `real_typed_struct` rows for `twitter` and `update_center` pass
under the host/API output-schema plane. Track 1 is now generated runtime on
the direct plane: R3 verified every sampled direct row reaches
`runtime::generated_json::parse_direct`.

Canada structural scan remains green at 41495 Mbps against the 40000 Mbps NEON
floor. The remaining work is not a structural scan floor repair.

## 2. Wave 1 Evidence Map

| Report | Scope | Binding finding |
|---|---|---|
| R1 | 9 retained regressed-from-PASS rows | Seven rows are generated string-wrapper bound; `citm_catalog` is structural/container churn; `marine_ik` is number/container and remains a retained GO control. |
| R2 | 4 original retained parse-G rows | Original G rows remain string-bound, but the current hot leaves are `match_tiny_plain_string` and `match_string_at_quote`, not `validate_utf8_codepoint`. |
| R3 | 12 direct digest red rows | Generated Track 1 is real. Direct digest rows split into string receiver/fold overhead, escaped-string decode/materialization, and number/emission residuals; representative host/API typed rows pass after REDRESS 71. |
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

## 9. Wave 1c Redispatch After Candidate 3/4 Redress

Candidate 3 and Candidate 4 have now been measured. Candidate 3 was rejected
in `skinny/REDRESS.md` item 62: even with the tiny probe preserved and the
wide path delayed until after the first local block, the route regressed almost
every sentinel row. Candidate 4 was admitted in item 63: array next-byte carry
reduced the redundant value re-entry boundary and moved array-heavy rows, but
retained parse still has 13 outcome-G rows.

The Wave 1c cohort reports live at
`restart/skinny/audit/SK-V6-COHORT/skv6-R1c-retained-string-post-c4.md`
through `skv6-R6c-icache-branch-post-c4.md`. Their binding findings:

| Report | Binding finding |
|---|---|
| R1c | Remaining string-heavy retained rows are still matcher dominated; the fresh retained candidate is narrow Unicode-escape-run validation, not another wide/plain scanner. |
| R2c | No defensible retained string scanner threshold remains after Candidate 4. Raw length thresholds either were measured and rejected or require non-canonical side information. |
| R3c | Offset/tape emission is visible but single-digit; the only non-string retained candidate is low-ceiling object next-key carry. |
| R4c | Direct field-layout string materialization is now admissible under the SK-V6 shortlist-exhausted clause and is the next direct-to-struct move. |
| R5c | Remaining parse-G rows are parser-owned, not explained by sidecar anchor drift. |
| R6c | Lock 15 still holds; i-cache split, cold outlining, and monomorphization churn are not justified. |

### Wave 1c Diagnosis Revision

- The retained generic string-scanner class is exhausted. REDRESS 60 blocks
  deleting `match_tiny_plain_string`; REDRESS 61 blocks the always-wide
  scanner; REDRESS 62 blocks the delayed-wide scanner; R2c rejects corpus or
  raw-length thresholds because the needed local fact is not available without
  rescanning or retained side state.
- Retained Unicode-escape rows are a distinct class. R1c measured
  `unicode_escapes` at 92.3% inside `match_string_at_quote`, with the top PCs
  in escape dispatch and hex/surrogate validation. `y_string_unicode` has the
  same escape-unit validation shape. That is not raw UTF-8 fusion and not a
  plain-string delimiter scan.
- Retained object cadence remains a low-ceiling parser-control class. R3c
  measured `consume_container_next + parse_key_colon + parse_pair` as relevant
  on object-heavy rows, but string matching remains dominant on most remaining
  red rows.
- Direct string materialization is no longer blocked by retained parse work
  after the remaining retained candidates are either measured or explicitly
  rejected. R4c proves generated `parse_direct` is the Track 1 path and names
  `parse_string_direct`, `unescape_json_string`, and receiver/fold closure
  overhead as the direct blocker.

### Candidate 5: Retained Unicode-Escape Run Validator

Path:

- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/bbnf-simd/src/aarch64/` only if the scalar split first shows
  row impact
- `skinny/crates/runtime/src/grammars/json/generated.rs` and
  `skinny/crates/codegen/src/json_templates/generated.rs` only if an
  attribution-visible wrapper is needed at the generated call boundary

Mechanism: split the trusted string escape validation sub-boundary so
contiguous `\\uXXXX` units can be validated as a run. Start scalar and
attribution-visible: prove that hex/surrogate validation is the row-local
sub-boundary before adding NEON. If the scalar split moves the named rows,
admit an AArch64 primitive that validates fixed-width hex units and surrogate
pairs without materializing decoded characters. The primitive is
grammar-neutral: JSON lowers `\\uXXXX`; CSS or other grammars may lower their
own escaped-code-unit policy to the same hex/class primitive family.

Expected row impact: `unicode_escapes`, `y_string_unicode`, and the
escape-heavy portion of `unicode_mixed`. It is not expected to move
`distinct_values`, `unicode_basic`, `apache_builds`, or other plain/tiny rows.

Falsifiability gate:

- Focus rows: `unicode_escapes`, `y_string_unicode`, `unicode_mixed`.
- Guard rows: `apache_builds`, `github_events`, `update_center`, `gsoc-2018`,
  `unicode_basic`, `distinct_values`, `citm_catalog`, `canada`, `instruments`.
- Scalar split gate: if an attribution-visible scalar helper does not show at
  least 20% of `match_string_at_quote` self-time in escape-region PCs on
  `unicode_escapes` and `y_string_unicode`, reject before SIMD and record the
  measurement.
- Throughput gate: retained Track 1 must improve at least
  `unicode_escapes >= +12%`, `y_string_unicode >= +8%`, and one of
  `unicode_mixed` or `gsoc-2018 >= +5%`; no guard row may regress by more than
  2%.
- Attribution gate: escape-region PCs in `match_string_at_quote` drop at least
  25% on `unicode_escapes` and `y_string_unicode`; plain-scan PCs remain within
  noise on guard rows.

Why this is admissible: it targets a fresh post-Candidate4 PC cluster. It does
not reopen raw UTF-8 fusion, retained sidecars, wide plain scanners, or direct
decoded-string materialization.

### Candidate 6: Object Next-Key Carry

Path:

- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/codegen/src/json_templates/generated.rs`

Mechanism: mirror Candidate 4's parser-control shape for object loops. After
comma/whitespace, carry the known quote/key state into a `parse_pair_at_current`
helper instead of re-entering the generic object pair cadence. Preserve
`match_tiny_plain_string`, quote offset emission, colon validation, and the
offset-tape format.

Expected row impact: modest gains on `citm_catalog`, `random`,
`instruments`, and `update_center`; it is a control-flow recovery, not a
string/unicode close.

Falsifiability gate:

- Focus rows: `citm_catalog`, `random`, `instruments`, `update_center`.
- Guard row: `distinct_values`, because it is string-bound and should not pay
  object-cadence churn.
- Throughput gate: production `profile-lazy` Mbps improves by at least
  `citm_catalog >= +3%`, `random >= +2%`, `instruments >= +2%`, and
  `update_center >= +1.5%`; `distinct_values` regresses no more than 1%.
- Attribution gate: `consume_container_next + parse_key_colon + parse_pair`
  falls at least 15% relative on `citm_catalog` and `instruments`.

Why this is admissible: it is the only remaining non-string retained
parser-control surface after Candidate 4. It does not change the substrate or
introduce a structural sidecar.

### Candidate 7: Direct Field-Layout String Materializer

Path:

- `skinny/crates/runtime/src/grammars/json/sink.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/codegen/src/json_sink_direct.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`

Mechanism: generated direct parsing emits field/string digest facts in the
same parse loop that owns the string context. The route must avoid allocating
a decoded `String` and then hashing contiguous decoded bytes, and it must avoid
the closure-heavy `array_string_source` / `object_string_source` receiver
shape on non-escaped rows. REDRESS 54/55 remain binding: this cannot be a
renamed sink-local decoded-stats helper or quote-source streaming hash.

Expected row impact: `unicode_escapes`, `unicode_mixed`,
`y_string_unicode`, `distinct_values`, and `gsoc-2018` direct rows.

Falsifiability gate:

- Direct rows: `unicode_escapes`, `unicode_mixed`, `y_string_unicode`,
  `distinct_values`, `gsoc-2018`.
- Throughput gate: `unicode_escapes >= +20%`, `unicode_mixed >= +15%`, at
  least two of `y_string_unicode`, `distinct_values`, and `gsoc-2018 >= +8%`;
  no direct row regresses by more than 5%.
- Profile gate: `parse_string_direct + unescape_json_string` combined self
  share drops at least 20% relative on `unicode_escapes` and `unicode_mixed`;
  receiver/fold closure share drops at least 30% relative on `distinct_values`
  and `gsoc-2018`.

Why this is admissible: the retained parser-control and generic string-scanner
shortlist has been exercised. R4c proves the direct blocker is now measured on
generated Track 1 rather than the old bench-private parser.

### Wave 1c Recommendation

Dispatch Candidate 5 first, because it is the only fresh retained parse
candidate named by post-Candidate4 PC attribution and it is narrow enough to
reject quickly if the scalar split does not expose row impact. If Candidate 5
fails, record it in REDRESS and do not open another retained string scanner
without a new local fact. Then either test the low-ceiling Candidate 6 object
carry or move to Candidate 7 direct field-layout materialization under the
SK-V6 shortlist-exhausted clause; Candidate 7 is the higher-impact path for
the remaining direct N-direct matrix.

## 10. Wave 1d Revision After Direct Source-Hook Redress

Candidate 7's first concrete shape has now been falsified in REDRESS 66. The
route only removed direct source-hook receiver/closure overhead and left the
escaped-string materialization shape intact. It moved focused direct medians by
only +0.99% on `unicode_escapes`, +0.11% on `unicode_mixed`, +1.75% on
`y_string_unicode`, +1.54% on `distinct_values`, and -0.01% on `gsoc-2018`.
That rejects direct source-hook folding as the direct close.

The Wave 1d reports in `restart/skinny/audit/SK-V6-COHORT/` refine the direct
candidate without reopening REDRESS 54, REDRESS 55, or REDRESS 66:

- R1d names parser-owned decoded scratch as the next escaped-string
  materialization route. The parser owns one reusable decoded buffer, writes
  semantic UTF-8 into it while scanning the quoted string, and passes a normal
  `&str` to the sink.
- R2d supplies the focused `profile_direct` and samply gate. The candidate
  must move escaped rows in production release binaries and expose a named
  materializer boundary under `runtime/parse-attribution`.
- R3d confirms the generality path: this is a DirectBuild field-fact
  materializer, not a new BIR variant, new grammar directive, or JSON logic in
  generic crates.

### Candidate 8: Parser-Owned Decoded Scratch Direct Materializer

Path:

- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/codegen/src/json_sink_direct.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs` only for tests or
  attribution, not for sink-local decoded facts

Mechanism: generated direct parsing threads one reusable `String` scratch
through the direct parse helpers. Plain strings stay borrowed. Escaped strings
are materialized by the parser into the scratch while the quote-owned string
path validates escapes and Unicode. The parser then calls the existing semantic
sink methods (`key`, `string`, `array_string`, `object_string`) with
`scratch.as_str()`. The sink continues to consume normal decoded strings and
hash contiguous bytes using the existing digest path.

This is deliberately narrower than the phrase "field-layout string
materializer" in Candidate 7. It does not compute decoded length/hash in the
sink, does not stream hash during quote-source scanning, and does not add
direct source hooks.

Expected row impact:

- Primary: `unicode_escapes` and `unicode_mixed`, because their direct profile
  cost is escaped-string decode/materialization.
- Secondary: `y_string_unicode`, if dense Unicode escapes amortize the scratch
  materializer despite small fixture size.
- Guard: `distinct_values`, `gsoc-2018`, `unicode_basic`, `apache_builds`,
  `github_events`, `canada`, and `numbers` must not regress, because plain
  strings and numbers should keep their current production path.

Falsifiability gate:

- Correctness: `cargo test -p parse-that-regex --profile ax-iter`,
  `cargo test -p runtime --profile ax-iter`, `cargo test -p bbnf-bench
  --profile ax-iter`, `cargo run -p xtask --release -- check-json`, and
  `cargo run -p xtask --release -- check-conformance`.
- Throughput: production `profile_direct` medians against a same-tree
  baseline must satisfy `unicode_escapes >= +20%`, `unicode_mixed >= +15%`,
  and one of `y_string_unicode >= +8%` or escaped-string allocation count down
  at least 90%; no target or guard row may regress by more than 5%.
- Attribution: under `runtime/parse-attribution`, the candidate-specific
  materializer must appear as a named symbol. On `unicode_escapes` and
  `unicode_mixed`, combined `parse_string_direct + unescape_json_string` cost
  must drop at least 20% relative. `unescape_json_string` should disappear or
  become negligible for generated direct escaped strings.

Reject conditions:

- If the helper wraps the current matcher and then calls `unescape_json_string`
  or equivalent full second pass, reject before benchmarking.
- If the sink computes decoded length/hash directly from raw source, reject as
  REDRESS 54 recurrence.
- If the parser streams hash rather than materializing a contiguous semantic
  string, reject as REDRESS 55 recurrence.
- If the diff adds direct source hooks or specializes receiver folding, reject
  as REDRESS 66 recurrence.

### Wave 1d Recommendation

Dispatch Candidate 8 as the next single intervention. It is the only direct
route left that attacks escaped-string materialization itself while preserving
the baseline's contiguous decoded-byte hash and the generated SinkOnly Track 1
surface. If Candidate 8 fails its same-row gate, the direct string/Unicode
close must return to research with a new local fact; no remaining Candidate 7
sub-shape should be admitted by renaming source hooks or sink-local facts.

## 11. Wave 1e Revision After Parser-Owned Scratch Redress

Candidate 8 has now been falsified in REDRESS 67. Parser-owned decoded scratch
kept strict semantics and passed correctness, but production direct medians
moved the wrong way: `unicode_escapes` 4999 -> 2798 Mbps (-44.03%),
`unicode_mixed` 4541 -> 4318 Mbps (-4.91%), and the partial
`y_string_unicode` sample was also negative. Generated parser control is the
wrong owner for escaped-string materialization on this host.

The Wave 1e reports in `restart/skinny/audit/SK-V6-COHORT/` identify the next
local fact:

- R1e proposes a standalone decoded-string primitive under the existing
  `unescape_json_string` API.
- R2e shows `unicode_escapes` already routes 135,148 / 136,682 Unicode units
  through the existing x4 helper; the residual shared cost is materialization
  writes: segment copies plus simple/Unicode `push(char)` calls.
- R3e rejects new SIMD/checkasm first because the vector semantics already
  exist; the next candidate should be a scalar/reference byte writer that
  reuses existing AArch64 helpers.

### Candidate 9: Byte-Output `unescape_json_string` Materializer

Path:

- `skinny/crates/parse-that-regex/src/lib.rs`

Mechanism: keep `unescape_json_string(raw_content: &str) ->
Result<Cow<'_, str>, RegexError>` as the public API and preserve the current
generated direct consumer. The no-backslash path remains borrowed. The escaped
path changes only the materializer body: build decoded output as bytes in a
`Vec<u8>`, copy plain segments with `extend_from_slice`, emit simple escapes
through a byte table, encode Unicode scalars directly into a stack byte buffer,
and convert the final valid UTF-8 byte vector into `String`. Existing
`find_next_escape_or_control`, scalar Unicode validation, and the admitted
AArch64 `unescape_uxxxx_x4_neon` helper remain the semantic substrate.

Expected row impact:

- `unicode_escapes`: primary target. It has dense Unicode escape runs already
  covered by x4 decode but still pays per-character output.
- `unicode_mixed`: secondary target. It has no `\u` units; it only moves if
  the simple-escape byte subpath improves materialization.
- `y_string_unicode`: small but escape-dense; improvement may be noisy.
- `unicode_basic`, `distinct_values`, `gsoc-2018`, `apache_builds`,
  `github_events`, `canada`, and `numbers` are guards because plain strings
  and non-string rows should keep their existing path.

Falsifiability gate:

- Correctness: `cargo test -p parse-that-regex --profile ax-iter`,
  `cargo test -p runtime --profile ax-iter`, `cargo test -p bbnf-bench
  --profile ax-iter`, `cargo run -p xtask --release -- check-json`, and
  `cargo run -p xtask --release -- check-conformance`.
- Throughput scout: same-tree production `profile_direct` Track 1 medians
  must satisfy `unicode_escapes >= +8%`, `unicode_mixed >= +5%`,
  `y_string_unicode >= +3%` or `unescape_json_string` self-time down at least
  20% in a high-sample profile, `unicode_basic` no worse than -2%, and no
  guard row worse than -2%.
- Close gate: if the scout passes, rerun against the broader Wave 3 direct
  close thresholds before declaring direct recovery. The scout is not itself
  sufficient for SK-V6 close.

Reject conditions:

- If the change adds generated parser scratch, source hooks, sink-local decoded
  stats, or quote-source streaming hash, reject as REDRESS 54/55/66/67
  recurrence.
- If `unicode_escapes` improves but `unicode_mixed` is noise, record the
  Unicode half as too narrow and do not relabel it as direct-string close.
- If simple-escape rows regress, revert even if `unicode_escapes` improves.

### Wave 1e Recommendation

Dispatch Candidate 9 as one standalone materializer intervention. It is the
only remaining direct route with a new local fact, and it preserves the
generated Track 1 call graph that all parser/sink restructuring attempts have
shown should not be disturbed.

### Candidate 9 Outcome

Candidate 9 has now been falsified in REDRESS 68. The same-HEAD direct smoke
showed `unicode_escapes` regressing from 4970 Mbps to 4771 Mbps (-4.00%) when
the escaped-string materializer moved from `String` writes to a byte-output
buffer under the same public `Cow<str>` API. This exhausts the current local
materializer family: direct source hooks (REDRESS 66), parser-owned decoded
scratch (REDRESS 67), and byte-output `unescape_json_string` (REDRESS 68) all
failed on the new generated Track 1 baseline.

The next Wave 3 plan must therefore leave local escaped-string writer churn and
target DirectBuild field facts or a strict representation-level direct output
contract. It must not add directives, BIR variants, JSON code to generic
crates, or a parallel source pass.

## 12. Wave 1f Revision: DirectBuild Field Facts

The Wave 1f cohort is now archived under `restart/skinny/audit/SK-V6-COHORT/`:

- R1f names the next admissible route as an existing-BIR `DirectBuildField`
  fact extension, not a decoder/materializer tweak.
- R2f classifies the remaining direct gap as a field-layout / representation
  mismatch. The current generated direct contract collapses raw string spans
  into semantic `&str` events too early; escaped rows pay strict
  materialization, while plain/high-cardinality rows pay scanner plus digest
  fold pressure.
- R3f verifies the lock boundary: no new directive, no new top-level BIR
  variant, no parallel substrate, and no JSON-specific generic-crate branches.
  The field-fact payload may grow under existing `DirectBuild { shape, fields
  }`, and `SinkOnlyProgram` must remain the carrier.

### Candidate 10: DirectBuild Semantic String Field Facts

Path:

- `skinny/crates/ir/src/lib.rs`
- `skinny/crates/passes/src/lib.rs`
- `skinny/crates/codegen/src/lower/sink_only.rs`
- `skinny/crates/codegen/src/json_sink_direct.rs`
- `skinny/crates/runtime/src/grammars/json/sink.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`

Mechanism: extend the existing `DirectBuildField` payload with a generic field
materialization policy. For JSON strings, the direct field fact declares the
requested direct representation as a semantic string fact, not merely a raw
span and not an eager owned `String`. The generated `SinkOnly` path consumes
that fact in the same parse loop and routes string/key fields to a strict
semantic fact consumer when the direct output asks for it. The benchmark
consumer must compute the same shape/count/depth/number fields and the same
semantic length/fingerprint facts as the current digest; serde_json and
sonic-rs remain parity oracles on the same strict output plane.

This candidate is distinct from the rejected routes only if the authority for
the emitted fact comes from `DirectBuildField` and is visible in the lowered
`SinkOnlyProgram`. A patch that merely overrides `JsonDigestSink::*_source`,
rewrites `unescape_json_string`, threads parser scratch, or streams a raw-source
hash without DirectBuild field-fact authority is a REDRESS 54/55/66/67/68
recurrence and must be rejected before measurement.

Minimum implementation shape:

- Add a grammar-neutral field materializer enum under the existing direct field
  payload. It must model at least `BorrowSpan`, `SemanticStringFact`,
  `NumberScalar`, `LiteralMap`, `Child`, `Repeated`, and `Empty` without adding
  a new BIR expression.
- Populate the JSON string/key fields with `SemanticStringFact` in the direct
  field roster. This remains a skinny JSON fact table until generic shape
  mining graduates it, but the lowerer must consume facts rather than branch on
  ad hoc sink hooks.
- Preserve the fact in `SinkOnlyProgram` and make the JSON direct renderer emit
  fact-aware calls. The generic lowerer may carry names; grammar-specific method
  names belong only in generated JSON runtime output.
- Update the direct benchmark sink so Track 1 consumes semantic string facts
  under this declared representation and Track 2 / serde_json / sonic-rs
  produce the same strict fact output for parity.

Expected row impact:

- Primary: `unicode_escapes` and `unicode_mixed`, because the current direct
  contract over-materializes or over-scans semantic strings before producing
  the benchmark's required facts.
- Secondary: `y_string_unicode`, `distinct_values`, and `gsoc-2018`, because
  they expose either escape-heavy tiny strings or high-cardinality string
  scanner/fold pressure.
- Guards: `twitter`, `apache_builds`, `github_events`, `unicode_basic`,
  `canada`, `numbers`, `citm_catalog`, `mesh`, `marine_ik`, and `instruments`.

Falsifiability gate:

- Correctness: `cargo test -p runtime --profile ax-iter`, `cargo test -p
  bbnf-bench --profile ax-iter`, `cargo run -p xtask --release -- check-json`,
  and `cargo run -p xtask --release -- check-conformance`.
- Same-HEAD throughput scout: production `profile_direct` Track 1 medians over
  five paired samples must satisfy `unicode_escapes >= +20%`,
  `unicode_mixed >= +15%`, and at least two of `y_string_unicode`,
  `distinct_values`, and `gsoc-2018 >= +8%`.
- Guard safety: no guard direct row may regress by more than 3%, and no
  already-passing direct row may lose PASS status.
- Attribution: if the throughput scout passes, `runtime/parse-attribution`
  profiles on `unicode_escapes`, `unicode_mixed`, `distinct_values`, and
  `gsoc-2018` must show either combined
  `parse_string_direct + unescape_json_string + fold_string_scalar/hash_bytes`
  self-time falling by at least 20% on escaped rows and 15% on mixed rows, or a
  named DirectBuild fact materializer replacing that cost while the row
  throughput gate holds.

Reject conditions:

- Any patch that adds a directive, new top-level BIR variant, parallel source
  scan, retained side table, or JSON branch in a generic crate is rejected.
- Any patch whose hot-path change is only a sink-local decoded-stat helper,
  quote-source streaming hash, parser-owned scratch, direct source-hook
  receiver fold, or `unescape_json_string` byte writer is rejected as a prior
  REDRESS recurrence.
- If the candidate only changes metadata and cannot plausibly move the named
  rows, stop before implementation and dispatch a new research tranche.

### Wave 1f Recommendation

Dispatch Candidate 10 as one standalone intervention. It is the only remaining
Wave 3 route with a new architectural fact after REDRESS 66-68. If it fails,
record the measurements and stop reopening direct string materialization under
the current digest workload; the next admissible path would require a different
strict direct output contract, not another escaped-string local rewrite.

### Candidate 10 Outcome

Candidate 10 has now been falsified in REDRESS 69. The implementation carried
`SemanticStringFact` through `DirectBuildField` and generated fact-aware direct
sink calls, but the first production direct smoke regressed the primary
`unicode_escapes` row from average 4870 Mbps to 4129 Mbps (-15.22%) over two
paired samples. The code patch was reverted before commit.

This closes the current Wave 3 string/Unicode direct materialization family:
receiver/source folding (REDRESS 66), parser-owned scratch (REDRESS 67),
byte-output unescape (REDRESS 68), and DirectBuild semantic fact hashing
(REDRESS 69) all fail on the generated Track 1 baseline. The next admissible
plan is no longer another escaped-string materializer. It must reassess the
direct output contract itself: either benchmark a real typed-struct workload
with field-specific access patterns, or explicitly classify the synthetic
digest workload as a SOTA stressor that is not representative of DirectBuild
closure for arbitrary grammars.

## 13. Wave 1g Revision: Direct Output Contract Split

The Wave 1g cohort is now archived under `restart/skinny/audit/SK-V6-COHORT/`:

- R1g recommends adding a supplemental `real_typed_struct` gate first, not
  replacing the digest matrix immediately. The initial fixtures are `twitter`
  and `update_center`, both on owned Rust structs shared by generated Track 1,
  independent Track 2, sonic-rs, and serde_json.
- R2g classifies the current `direct_to_struct` digest as a
  `semantic_full_digest_stressor`. It remains valuable and strict, but it is
  not the representative DirectBuild closure gate because it touches every key
  and string byte globally.
- R3g routes the full future implementation through generated typed
  DirectBuild output from field facts. It explicitly rejects directives, new
  BIR variants, benchmark-private Track 1 parsers, and JSON branches in
  generic crates.

### Candidate 11: Real Typed Struct Gate

Path:

- `skinny/crates/bbnf-bench/src/real_typed_struct.rs` (new)
- `skinny/crates/bbnf-bench/src/lib.rs`
- `skinny/crates/bbnf-bench/src/bin/profile_direct.rs`
- later, after smoke stability: `skinny/crates/bbnf-bench/benches/json_parity.rs`
  and gate/report wiring

Mechanism: add a strict typed-output workload separate from the digest stressor.
The first implementation may hand-author the typed consumer as long as Track 1
uses generated `runtime::generated_json::parse_direct(input, &mut sink)` and
returns an owned typed struct before the checksum is computed. Track 2 must be
independent and must not call `parse_direct`. Sonic-rs and serde_json must
deserialize the exact same Rust output structs. The checksum is a black-box
post-parse consumer over the owned output, not a value produced during parse.

Initial fixtures:

- `twitter -> TwitterSearch`, covering nested objects, arrays, optionals,
  booleans, integers, text fields, real Unicode, and recursive retweeted
  status objects.
- `update_center -> UpdateCenter`, covering a dynamic plugin map,
  dependencies/developers/labels arrays, long strings, optionals, and required
  preservation of map keys.

Taxonomy change:

- Rename the existing direct digest plane in documentation and result tables to
  `semantic_full_digest_stressor`.
- Add `real_typed_struct` as the representative DirectBuild closure gate once
  the smoke row is stable.
- Keep the digest stressor as a guard row family. It can fail while typed
  DirectBuild closure passes, but it must be reported honestly rather than used
  to veto representative typed closure.

Falsifiability gate:

- Correctness: generated Track 1, independent Track 2, sonic-rs, and
  serde_json all return identical owned typed structs for `twitter` and
  `update_center`, and the post-parse checksum agrees across all tracks.
- Throughput scout: same-HEAD `profile_direct` or `profile_real_typed` medians
  over five paired samples. Generated Track 1 must be within `1.10x` sonic-rs
  time on at least one typed fixture and no worse than `1.25x` on the other
  during the scout. Track 2 must remain structurally independent and within
  the same broad band or be reported as the residual reference-parser cost.
- Guard: existing `semantic_full_digest_stressor` rows must remain
  correctness-green. Their throughput misses stay visible as stressor misses,
  not hidden or renamed as typed closure.
- Strictness: no broad `serde_json::Value` escape in the typed output except
  fields proven null-only in the checked fixture; if a fixture later makes such
  a field non-null, the typed workload fails until it is modeled.

Reject conditions:

- If Track 1 computes only a checksum during parse, reject: the workload must
  return owned typed output first.
- If Track 1 calls the independent parser or serde/sonic path, reject as a
  benchmark-private parser recurrence.
- If the typed structs are under-modeled with broad `serde_json::Value` for
  live non-null fields, reject as a weakened output plane.
- If implementation requires a new grammar directive or BIR variant, reject.

### Wave 1g Recommendation

Dispatch Candidate 11 as the next redress implementation. Start with a
separate `real_typed_struct` module and profiling mode so it cannot hide the
existing digest stressor. If the typed workload demonstrates parity and a
stable throughput signal, fold it into `BENCH.md`, `RESULTS.md`, and the gate
matrix as the representative DirectBuild closure row while retaining
`semantic_full_digest_stressor` as an explicit stressor family.

## 14. Wave 1h Revision: Candidate 11 Falsified as Close

Candidate 11 produced the right correctness shape but not the SOTA close. The
implementation is saved as `/tmp/skv6-wave3-candidate11-rejected.patch` and
the detailed measurements are REDRESS item 70.

Findings:

- The `real_typed_struct` oracle is valuable: it caught no correctness
  divergence across generated Track 1, independent Track 2, serde_json, and
  sonic-rs for `twitter` and `update_center`.
- The first materializer, `parse_direct -> serde_json::Value -> typed struct`,
  was predictably not SOTA-class: both scout fixtures landed near 0.53x
  sonic-rs.
- A same-loop generated Track 1 `UpdateCenterSink` plus zero-allocation
  post-parse checksum lifted `update_center` to a 4.84 Gbps median, but
  sonic-rs on the same owned Rust output shape measured 7.12 Gbps. The scout
  gate did not fire.
- The decisive architectural finding is schema-source, not another local
  string writer. JSON's grammar does not contain `TwitterSearch` or
  `UpdateCenter`; sonic-rs gets those shapes from Serde. A BBNF direct-output
  proof must therefore admit an explicit host/API output type contract consumed
  by `DirectBuild` field facts. That contract is not a new grammar directive
  and not a new BIR variant; it is a typed consumer schema supplied at the API
  boundary and lowered into existing `DirectBuild { shape, fields }`.

Next admissible plan:

- Do not reopen hand-authored JSON typed sinks as SOTA proof. They are useful
  profilers, but they do not prove grammar-general DirectBuild.
- Plan the schema-source contract first: how an external owned output type
  contributes field rosters, borrowed/owned representation policy, optionality,
  repeated fields, map fields, and exact string/number materializers into the
  existing `DirectBuild` payload.
- Once that contract exists, regenerate the typed sink from the field facts and
  measure again. A generated typed sink can close only if it removes the
  generic JSON event stack and dynamic key/string routing measured in Candidate
  11.

## 15. Wave 1i Plan: DirectBuild Output Schema Contract

REDRESS 70 turns the next Wave 3 intervention into a schema-source problem.
The implementation route is not another JSON sink and not another string
kernel. It is a host/API output schema lowered into the existing
`DirectBuild { shape, fields }` payload.

The schema-source cohort is archived at
`restart/skinny/audit/SK-V6-COHORT/`:

- `skv6-schema-A-output-schema-boundary.md`: schema enters at the host/API
  boundary and feeds `ShapeFacts` / `DirectBuildField`, not BBNF syntax.
- `skv6-schema-B-generated-typed-directbuild.md`: exact-current
  `DirectBuildField { name, source }` is a no-go; richer payload facts and a
  schema-specialized lowered program are required, but no new `BackendExpr`
  variant is required.
- `skv6-schema-C-redress-gates.md`: exact typed-row thresholds and digest
  guard rows for Candidate 12.

### Candidate 12: Generated Typed DirectBuild from Host/API Schema

Owner paths:

- `skinny/crates/ir/src/lib.rs`: extend `DirectBuildField` with output target
  and materialization policy facts. This is a payload extension, not a new BIR
  variant.
- `skinny/crates/passes/src/lib.rs`: add an options-bearing entry point,
  e.g. `compile_with_options(grammar, CompileOptions { direct_output })`, while
  preserving `compile(grammar)` as the grammar-native default. The direct
  output schema enters here from host/API code, not from a BBNF directive.
- `skinny/crates/codegen/src/lower/sink_only.rs`: preserve target/policy facts
  in `SinkOnlyProgram`.
- `skinny/crates/codegen/src/direct_schema.rs` (new): public schema-source
  data model and validation for output types, field rosters, optional/null
  policy, maps, duplicate keys, unknown fields, and recursive type IDs.
- `skinny/crates/codegen/src/lower/schema_direct.rs` (new): lower
  `SinkOnlyProgram + DirectOutputSchema` into a typed direct program. This is
  the contextual step that specializes JSON `object` into `UpdateCenter`,
  `Plugin`, `TwitterSearch`, etc.
- `skinny/crates/codegen/src/json_typed_direct.rs` (new): render a generated
  typed direct parser from the typed direct program. The renderer must not use a
  generic JSON event stack, `JsonSink`, `serde_json::Value`, or a
  benchmark-private hand sink; it emits field-specific builders and key
  dispatch from schema facts.
- `skinny/crates/bbnf-bench/src/direct_struct.rs`: add a typed-output mode that
  calls the generated typed entry point and computes a post-parse checksum over
  owned output. Keep `semantic_full_digest_stressor` unchanged.

No-go boundary:

- If the implementation keeps exact-current `DirectBuildField { name, source }`
  with no output-schema payload, it cannot represent host Rust type paths,
  source keys, required/optional/default/null policy, scalar target types,
  arrays, maps, recursion, duplicate/unknown-field policy, or owned
  construction policy. That path is a plan failure.
- If the implementation adds a new top-level BIR variant, grammar directive,
  hidden benchmark schema, `JsonSink` typed receiver, generic JSON event stack,
  or retained side table, the result is invalid rather than merely slow.

Minimal schema facts:

- `DirectOutputSchema { root_shape, rust_type_name, fields }`.
- Per field: output name, output type, presence (`required | optional |
  nullable`), cardinality (`one | repeated | map`), source path over grammar
  fields/keys, representation policy (`borrowed_span | owned_string |
  number_scalar | literal | child | repeated | map | empty`), and decode policy
  (`raw_ok | unescape_required | numeric_exact`).
- JSON-specific keys such as `plugins` or `statuses` are data in the schema
  instance and generated JSON output, never control flow in generic crates.

Generalization:

- For grammars whose output type is implied by grammar structure, the schema
  can be derived from `ShapeFacts`.
- For JSON and other open data grammars, the host/API type supplies the schema
  just as Serde supplies it to sonic-rs. This is the only honest way to compare
  direct-to-struct rows without hiding a benchmark-private parser.
- The same `DirectOutputSchema` shape covers CSS AST emission, CSV row structs,
  and Sheets formula nodes: field paths differ, but required/optional/repeated
  and borrowed/owned materialization policies are grammar-neutral.

Falsifiability gate for the first implementation:

- Correctness: generated Track 1 typed output, independent Track 2 typed
  output, sonic-rs, and serde_json all produce identical owned `UpdateCenter`
  output and equal post-parse checksum.
- Throughput: one generated Track 1 typed row must meet strict slack and the
  other must meet scout slack:
  - `update_center`: 6,470 Mbps strict floor (`sonic 7,117 / 1.10`) and
    5,694 Mbps scout floor (`sonic 7,117 / 1.25`).
  - `twitter`: 5,715 Mbps strict floor (`sonic 6,286 / 1.10`) and 5,029 Mbps
    scout floor (`sonic 6,286 / 1.25`).
  Track 2 may remain a slower reference for the first schema-source proof but
  must be reported separately.
- Regression guard: `semantic_full_digest_stressor` correctness remains green;
  its throughput miss remains reported and cannot be renamed as typed closure.
  No existing direct digest row may regress by more than 5%; rows currently
  above strict slack (`citm_catalog`, `marine_ik`, `numbers`, and `mesh` if the
  current RESULTS/prose mismatch resolves in its favor) must remain above it.
- Rejection: if generated schema-specific code still misses 6,470 Mbps, record
  the route in REDRESS and move the direct close from "missing schema" to
  "generated recursive descent cannot match Serde-shaped sonic-rs on this
  typed-output row without CollapsedStage or a lower-level typed builder
  primitive."

## 16. Wave 3 Result: Candidate 12 Accepted for Representative Typed Output

Candidate 12 landed as a schema-source DirectBuild proof rather than another
sink-local string kernel. The implementation keeps the grammar clean: the
host/API output schema is supplied by xtask/consumer code, lowered through a
grammar-neutral `DirectSchemaSet`, and consumed by generated code without
adding a BBNF directive or a BIR variant.

Accepted facts:

- `DirectBuildField` may carry optional output-target facts. This is a payload
  extension of `DirectBuild { shape, fields }`, not a new variant.
- `DirectSchemaSet` is the schema-source boundary for open data grammars such
  as JSON. It names roots, Rust type paths, fields, presence policy, duplicate
  policy, scalar/container types, object-entry vectors, capacity hints, and
  ignored-field skip facts.
- `real_typed_struct` is the representative typed-output row. It gates
  generated Track 1 against sonic-rs/serde sidecars on the same owned Rust
  struct plane. Track 2 remains a structurally different oracle and is
  reported separately. The maximal digest row remains
  `semantic_full_digest_stressor` and still reports its own misses.
- PC-level attribution named `DirectParser::skip_value` and skipped string
  values as the remaining `update_center` hot leaf. The accepted close was not
  raw key dispatch; it was schema-sourced ignored-field facts plus a skip-only
  plain-string fast path, both consumed by the generated typed parser.

Rejected sub-routes:

- Raw key byte dispatch regressed the `update_center` profile scout.
- Narrowing the `Plugin` output to only `name` and `version` made sonic-rs
  faster and failed Criterion.
- A global 40-byte tiny-string cap and a skip-only 64-byte cap both regressed
  the scout. The accepted skip-only cap is 96 bytes; materialized strings keep
  the 32-byte cap.

Measured close:

| row | Track 1 median | sonic-rs median | verdict |
|---|---:|---:|---|
| twitter `real_typed_struct` | 278.67 us | 422.12 us | PASS; generated Track 1 beats sonic-rs typed serde. |
| update_center `real_typed_struct` | 354.15 us | 351.23 us | PASS; generated Track 1 is inside `sonic-rs * 1.10` time slack. |

The result validates the SK/V1 direct-to-struct premise only for an honest
host/API typed-output plane. It does not rescue the maximal digest stressor:
`direct_to_struct` remains a visible N-direct family and must keep reporting
its failures until either a real consumer needs that exact semantic digest or a
new workload-specific profile names an admissible close.

## 17. Wave 2 Result: Candidate 13 Rejected on Canonical Guard Rows

Candidate 13 tested the narrowest remaining retained string first-probe lever:
raise `match_tiny_plain_string` from an 8-byte scalar probe to 16 bytes in the
generated retained parser and codegen template. This did not repeat the rejected
NEON tiny-string kernel and did not add a side table or second source pass.

The scout was attractive but misleading. `profile-lazy` improved the five
remaining parse-G rows and several guard rows, with no scout guard regression
above 5%. Canonical Criterion then refuted the route. The intended red rows
improved (`twitter`, `update_center`, `random`, `unicode_basic`), but multiple
retained guard rows regressed beyond the SK-V6 stop gate: `apache_builds`,
`github_events`, `gsoc-2018`, `instruments`, `unicode_escapes`,
`distinct_values`, and `y_string_unicode`.

Conclusion: global retained string first-probe widening is now falsified in
both broad and narrow forms. REDRESS 60 rejects deleting the tiny probe,
REDRESS 61/62 reject wide/delayed wide trusted scans, and REDRESS 72 rejects a
simple 16-byte tiny cap. The next retained parse wave must not propose another
unconditional string-threshold change. Any string work must arrive through
grammar-neutral cost facts that can choose a policy per generated shape, or
through a lower-level primitive with a same-row consumer and a guard matrix
that preserves already-passing rows under Criterion.
