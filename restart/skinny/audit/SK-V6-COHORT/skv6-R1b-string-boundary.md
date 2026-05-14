# SK-V6 R1b retained JSON string-boundary redeploy

Date: 2026-05-14
Workspace: `/Users/mkbabb/Programming/bbnf-lang`

## Authority read

- `skinny/REDRESS.md` items 60-61.
- `restart/skinny/audit/GRAND-SYNTHESIS-SK-V6.md` Candidate 1 and Candidate 2 sections.
- `skinny/crates/runtime/src/grammars/json/generated.rs`.
- `skinny/crates/parse-that-regex/src/lib.rs`.
- Existing focused Candidate 2 profiles under `/tmp/skv6-wave2-candidate2-profiles`.

No repository files were edited.

## Current retained string path

Retained generated parsing has two string entry sites: `parse_key_colon` and
`parse_string` in `skinny/crates/runtime/src/grammars/json/generated.rs`.
Both consume and emit the opening quote, then run the scalar
`match_tiny_plain_string` probe. Only if that probe fails do they call
`match_string_at_quote`, which wraps
`parse_that_regex::match_json_string_at_quote_trusted_utf8`.

The trusted matcher in `skinny/crates/parse-that-regex/src/lib.rs` no longer
validates raw UTF-8. It scans only for `"`, `\\`, and control bytes. The current
`skip_json_string_plain_trusted` fast path scans 16-byte AArch64 blocks, then
8-byte SWAR blocks, then returns the cursor without scalar tail completion.
The outer matcher advances one non-special byte and calls the helper again.

## Why Candidate 1 regressed

Deleting the tiny probe was not removing redundant work. It removed the only
cheap complete path for dense short plain strings.

For a successful tiny string, the current generated path scans the first small
window and returns directly, avoiding the full trusted matcher, span/error
mapping, and flag-patching branch. If the probe is deleted, every short key and
short string enters `match_json_string_at_quote_trusted_utf8`. For strings under
the 8-byte SWAR threshold, `skip_json_string_plain_trusted` returns immediately,
so the outer matcher advances byte-by-byte until it sees the closing quote.
That explains the REDRESS 60 shape: short-string rows like `apache_builds`
and `distinct_values` regressed heavily, and even long-string rows regressed
because their object keys still use the same retained key path.

Conclusion: the tiny probe is a required short-string specialization, not front
matter. The blocked route is deleting it or treating `match_tiny_plain_string`
as a removable duplicate.

## Why Candidate 2 improved locally but failed globally

Candidate 2 targeted the correct second boundary: long trusted string special
byte discovery under `match_string_at_quote`. That is why it improved rows with
long straight string spans: REDRESS 61 records focused retained gains of
`unicode_mixed` +16.9%, `gsoc-2018` +15.8%, and `y_string_unicode` +6.0%, and
the full matrix still showed `gsoc-2018` +15.4%, `unicode_mixed` +8.9%, and
`y_string_unicode` +7.1%.

It failed because the wider scanner was not the whole retained string-boundary
cost. The focused Candidate 2 profiles still show `match_string_at_quote` as
the dominant self symbol after the change:

| Row | Candidate 2 profile | Residual `match_string_at_quote` self | Total string boundary/state |
|---|---|---:|---:|
| `gsoc-2018` | `/tmp/skv6-wave2-candidate2-profiles/gsoc-2018.profile.json.gz` | 66.6% | 86.3% |
| `unicode_mixed` | `/tmp/skv6-wave2-candidate2-profiles/unicode_mixed.profile.json.gz` | 72.3% | 83.4% |
| `y_string_unicode` | `/tmp/skv6-wave2-candidate2-profiles/y_string_unicode.profile.json.gz` | 71.9% | 85.9% |

The full advisory failure is also important: `canada` regressed -9.8% and
`instruments` regressed -7.5%. `canada` has almost no string work, so that
regression is not explained by row-local string scanning. Treat it as a
whole-binary/code-layout or shared-inline-cost warning unless a later profile
proves otherwise. `instruments` is mixed short-string/number/container; it did
not have enough long-string work to amortize the wider scanner and was exposed
to the same shared cost.

Conclusion: a wider trusted scanner can help long-string rows, but the tested
64-byte primitive is now rejected. The remaining work is the retained matcher
control protocol and tail behavior, not raw UTF-8 validation and not another
standalone wide scanner.

## What remains hot

After both failures, retained string-boundary cost is split into:

1. Short-string success path: `match_tiny_plain_string`, which must stay.
2. Long/trusted fallback: `match_string_at_quote`, still dominant on long rows.
3. Per-string generated state: `consume_quote_at_cursor`, `emit_plain_offset`,
   and `patch_flags` on escaped strings.
4. Trusted matcher tail behavior: after 16-byte and 8-byte loops, the helper
   returns without scanning the scalar tail, leaving the outer matcher to step
   through non-special tail bytes one at a time.

The residual profile evidence points most strongly at item 4 as the next
smallest retained-only hypothesis. It is narrower than Candidate 1 and does
not reopen Candidate 2.

## Next retained-parse candidate

Candidate hypothesis: add scalar tail completion to the trusted string scanner
while preserving the generated tiny probe.

Exact file path:
`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/parse-that-regex/src/lib.rs`

Target function:
`skip_json_string_plain_trusted`.

Mechanism to test, not implement here: after the AArch64 16-byte loop and the
8-byte SWAR loop, scan the remaining bytes in the helper until quote,
backslash, control, or end-of-input, matching the untrusted helper's scalar
tail shape but without UTF-8 validation. Do not remove
`match_tiny_plain_string`; do not add a 64-byte primitive; do not add sidecar
state.

Row target:
`y_string_unicode`, `gsoc-2018`, and `unicode_mixed`, with `twitter`,
`instruments`, and `canada` as guard rows.

Falsifiability gate:

- Build focused retained release binaries with parse attribution.
- `match_string_at_quote` self-time must drop by at least 8% relative on at
  least two of `y_string_unicode`, `gsoc-2018`, and `unicode_mixed`.
- Track 1 retained Mbps must improve by at least 5% on `y_string_unicode` and
  at least 3% on one of `gsoc-2018` or `unicode_mixed`.
- `twitter`, `instruments`, and `canada` must not regress more than 2% in
  focused `profile-lazy`, and the full advisory matrix must have no retained
  row regression above 2%.
- If self-time moves but Mbps does not, reject as another string-boundary
  micro-optimization without row lift.

Confidence: medium-low but admissible. It is a small retained-only test that
targets a visible remaining control-flow cost. It should be rejected quickly if
the 8-byte/16-byte loops already dominate enough that scalar tail completion
does not move throughput.
