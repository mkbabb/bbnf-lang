# SK-V16 P1-E: Hot-Leaf Attribution

Pass: S-P1 Profile. Cycle: V16.
Date: 2026-05-28.
Scope: synthesis of P1-A/B/C hot leaves across all profiled rows.
Output: this file.
Baseline: SK-V16-open (`5ed43f8e1`).
Host triple: `aarch64-apple-darwin`.
Build flags: release profile with debuginfo; `warmup_iters=0`.
Profile tool: `samply`, `atos -inlineFrames`, `rustfilt`.
Corpus coverage: 17/17 for P1-A/B/C modes.

## Section 1 - Method

```sh
node /tmp/skv16-p1-symbolize.js
atos -inlineFrames -o /Users/mkbabb/Programming/bbnf-skv16-p1/skinny/target/release/profile_direct \
  -arch arm64 <address> | /Users/mkbabb/.cargo/bin/rustfilt
```

The script emitted:

- `/tmp/skv16-p1/samply-profile-top20-inline.tsv`
- `/tmp/skv16-p1/samply-mode-top20-inline.tsv`
- `/tmp/skv16-p1-mode3/samply-profile-top20-inline.tsv`
- `/tmp/skv16-p1-mode3/samply-mode-top20-inline.tsv`

## Section 2 - Findings

| Class | Evidence | S-P2 meaning |
|---|---|---|
| scanner/string | `parse_that_regex::first_control_byte`, `skip_string_plain_trusted`, `match_string_at_quote`, `validate_string_escape`, `read_hex_unit_scalar`, memchr/movemask | valid hot-leaf antecedent; must remain scalar-referenced and aarch64-only if SIMD scoped |
| scanner/whitespace | `parse_that_regex::skip_ascii_whitespace` | valid hot-leaf antecedent; low semantic risk |
| scanner/number | `scan_digit_run`, `is_two_ascii_digits`, serde decimal parse | valid antecedent but REDRESS numeric blocks require fresh BBNF-side framing |
| tape/view | `JsonNodeKind::at_cursor`, `Tape::offset_at`, `string_body_range`, `next_sibling_cursor` | valid diagnostic for eager decode; risk of hidden retained cursor/sidecar shortcuts |
| generated product | `parse_type_unicode_escapes_document`, `parse_type_gsoc_proposal`, `parse_type_unicode_mixed_document`, string-enum fold, `DirectParser::skip_value` | valid generated-code hot leaf; must be generalized or quarantined under Lock 14 |
| harness | `typed_checksum`, local FNV checksum, serde allocation/drop | measured cost but not parser primitive source |

Dominant cross-mode observations:

- Parse-only hot leaves are scanner primitives plus generated parse frame loops.
- Direct/typed rows are dominated by generated real-typed code and harness
  checksums; this is product-plane reality but not always a runtime primitive.
- Eager decode and cold-first parse expose view/tape cursor cost and string
  body range cost that parse-only does not.
- Structural-scan-only top rows are partly scan-tail and partly local FNV
  checksum. S-P2 must separate those before proposing a scan primitive.

## Section 3 - Delta Vs SK-V15

No admission delta. This file only resolves `unprofiled` cells into named
leaf classes for S-P2. It does not update `skinny/RESULTS.md`.

## Section 4 - Anomalies And Masking Signals

`samply` profiles were saved with `symbolicated=false`; all symbol claims use
the archived profile binary and `atos -inlineFrames`. This is acceptable as
long as the same binary path remains cited. It is not acceptable to cite raw
hex PCs without the symbol sidecar.

Checksum and local FNV loops are intentionally classified as harness. They are
not optimization targets unless S-P2 first removes the harness dependency from
the measured workload or proves that the production runtime pays the same cost.

## Section 5 - Sources

- `/tmp/skv16-p1/samply-profile-top20-inline.tsv`
- `/tmp/skv16-p1/samply-mode-top20-inline.tsv`
- `/tmp/skv16-p1-mode3/samply-profile-top20-inline.tsv`
- `/tmp/skv16-p1-mode3/samply-mode-top20-inline.tsv`
