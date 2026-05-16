# SK-V7 W4 R4 Implementation Risk: per-`\uXXXX` TBL Classifier

Date: 2026-05-16.

## Scope

This note synthesizes implementation risks for SPEC §6 Wave 4: the B1
per-`\uXXXX` TBL classifier. It is a read-only preflight artefact for the W4
implementer, not an implementation plan and not source authority.

Inputs read:

- `restart/skinny/tranches/sk-v7/SPEC.md` §6.
- `restart/skinny/tranches/sk-v7/SYNTHESIS.md` §3.3 and §6.
- SK-V7 cohort reports under `restart/skinny/tranches/sk-v7/research/`,
  especially `skv7-B1-uxxxx-tbl.md`, `skv7-C1-parse-profile.md`,
  `skv7-A4-parse-that-gaps.md`, `skv7-C2-direct-profile.md`,
  `skv7-C5-correlation.md`, and `skv7-A6-ledger-generalization.md`.
- Prior rejection evidence in `skinny/REDRESS.md` and SK-V6 escaped-string
  materializer research.

## Executive Finding

The safe W4 route is narrow:

**Keep the existing raw-span source shape and improve only the standalone
`unescape_json_string(raw_content) -> Cow<str>` owned path with a
single-quartet `\uXXXX` classifier that reuses the existing
`unescape_uxxxx_neon` kernel.**

Do not move decode into generated parser control, do not add direct-only sink
source hooks, do not introduce semantic string facts for the current digest
workload, and do not rewrite the whole materializer into a byte-output `Vec<u8>`
route. Those are SK-V6 rejected families.

C1 also narrows the row target: W4 is causally justified for exactly
`unicode_escapes` and `y_string_unicode`. `unicode_mixed` and
`distinct_values` have zero `\uXXXX` units in C1 and belong to W5 plain-string
scan work, not this classifier.

## Load-Bearing Source Facts

SPEC §6 says W4 should:

- add `parse-that-regex/src/unicode/escape_decode.rs`;
- reuse `bbnf-simd/src/aarch64/unescape_uxxxx.rs`;
- wire into `unescape_json_string` at the current `lib.rs:911-922` call site;
- bench `unicode_escapes` and `y_string_unicode` on parse and direct;
- keep the REDRESS record distinct from REDRESS 64.

SYNTHESIS §3.3 gives the correction that overrides older B1/A4 overreach:

| Corpus | C1 `\uXXXX` content | W4 reading |
|---|---:|---|
| `unicode_escapes` | 78.0% | B1 applies |
| `y_string_unicode` | 74.2% | B1 applies |
| `unicode_mixed` | 0.0% | B1 does not apply; W5 plain-body scan |
| `distinct_values` | 0.0% | B1 does not apply; W5 plain-body scan |

`skv7-C1-parse-profile.md` is the authoritative correction because it counted
actual `\uXXXX` occurrences and tied `match_string_at_quote` self-time to
escape decode versus body scan. Earlier reports still contain stale four-row
language. Treat those as pre-C1 candidates, not W4 gates.

## Distinction From REDRESS 64

REDRESS 64 rejected a retained four-unit contiguous-run validator:

- call site: `validate_json_unicode_escape_run`;
- operation: validate four adjacent `\uXXXX` units;
- kernel: `unescape_uxxxx_x4_neon`;
- success: `unicode_escapes` lifted +31.82%;
- failure: `y_string_unicode` regressed -3.72%, and companion rows did not move.

That rejection does not block W4 if W4 stays structurally different:

| Axis | REDRESS 64 rejected shape | W4 admissible shape |
|---|---|---|
| Granularity | four contiguous `\uXXXX` units | one quartet at a time |
| Hot caller | validation during string recognition | materialization in `unescape_json_string` |
| Main fact | dense-run amortization | per-unit nibble classification and surrogate join |
| Row fit | only dense `unicode_escapes` runs | short and boundary-heavy `y_string_unicode` too |
| Reuse | x4 path as primary route | existing x4 path retained for dense runs; single-quartet fallback added |

The highest REDRESS 64 recurrence risk is accidentally placing more work in
`validate_json_unicode_escape_run`, or making the W4 route require four
consecutive escapes. Either move reopens the rejected validator.

## Distinction From SK-V6 Escaped-String Materializer Rejects

The SK-V6 direct-string family is also binding:

- REDRESS 66: direct source-hook field-layout materializer was noise
  (`unicode_escapes` +0.99%, `unicode_mixed` +0.11%, `y_string_unicode`
  +1.75%). Receiver/closure removal was not the bottleneck.
- REDRESS 67: parser-owned decoded scratch regressed hard
  (`unicode_escapes` -44.03%, `unicode_mixed` -4.91%,
  `y_string_unicode` -16.76%). Folding decode into generated parser control is
  closed.
- REDRESS 68: byte-output `unescape_json_string` under the current `Cow<str>`
  API regressed `unicode_escapes` -4.00%. A whole-body `Vec<u8>` writer is
  closed.
- REDRESS 69: semantic string facts/hash for the current digest workload
  regressed `unicode_escapes` about -15.22%. One-pass semantic fact hashing is
  closed for the current digest representation.

Therefore W4 must not claim novelty by changing the direct receiver shape.
The acceptable novelty is smaller: replace the scalar per-quartet hex decode
inside the existing standalone materializer, while preserving the raw span
boundary and the public semantic string contract.

## Safest Source Shape

Name: **raw-span, API-preserving standalone materializer**.

Shape:

```text
generated parse_string_direct
  -> ParsedString { raw, needs_unescape }
  -> JsonSink::*_source(raw, true)
  -> parse_that_regex::unescape_json_string(raw)
  -> private per-quartet escape_decode helper
  -> existing bbnf_simd::aarch64::unescape_uxxxx_neon
```

For retained/view paths, the same safe shape is simply:

```text
retained string value/view
  -> parse_that_regex::unescape_json_string(raw)
  -> private per-quartet escape_decode helper
```

This is safest because:

- the same-wave consumer already exists in retained and direct paths;
- generated code does not need a new string representation;
- `ParsedString { raw, needs_unescape }` remains the source boundary;
- `unescape_json_string` remains the public semantic oracle;
- plain strings keep the borrowed fast path;
- escaped strings remain a standalone materializer, not a parser-control or
  sink-local special case;
- the existing AArch64 NEON primitive is reused, not re-admitted as a new SIMD
  body.

The riskiest part of the B1 wording is "fused materializer." Interpreted
narrowly, it means a private helper under `unescape_json_string` that decodes a
single quartet and appends the resulting scalar through the existing output
contract. Interpreted broadly, it becomes REDRESS 67/68 again. W4 should use
the narrow interpretation.

## Implementation Risk Matrix

| Risk | Why it matters | Mitigation / gate |
|---|---|---|
| Stale four-row B1 target leaks into W4 | B1/A4/C2 text names `unicode_mixed` and `distinct_values`, but C1 shows both have zero `\uXXXX` | Gate W4 on `unicode_escapes` and `y_string_unicode`; treat `unicode_mixed` and `distinct_values` only as no-regression guards |
| REDRESS 64 recurrence | Four-unit validation already failed on `y_string_unicode` | Do not edit `validate_json_unicode_escape_run` for the primary route; do not require contiguous quartets |
| Dense-run regression | `unicode_escapes` already benefits from the existing x4 materializer path | Keep the current `unescape_four_unicode_escapes` dispatch before single-quartet fallback |
| REDRESS 67 recurrence | Parser-owned scratch made `unicode_escapes` 44% slower | Do not thread decoded scratch through generated parser helpers |
| REDRESS 68 recurrence | Whole-body byte-output writer regressed the primary escaped row | Do not replace the whole `Cow<str>` materializer with a `Vec<u8>` finalization path |
| REDRESS 66/69 recurrence | Direct source hooks and semantic facts did not close the digest workload | Do not add direct-only sink overrides, digest facts, or consumer-specific string hashing |
| Error offset drift | Unicode escape errors are user-visible and parity-sensitive | Differential tests must pin invalid hex, lone low surrogate, missing low surrogate, bad pair, and non-character acceptance offsets |
| Inlining / code-size regression | B1 itself calls out cold-cache or helper-call overhead as a likely failure mode | Keep helper small; use attribution builds if samples vanish into anonymous frames; reject on any row regression >= 3% |
| AArch64-only behavior drift | Other targets must keep scalar behavior | Gate the NEON call under `#[cfg(target_arch = "aarch64")]`; scalar fallback must remain bit-identical |
| Lock/generalization drift | `parse-that-regex` still has JSON-named surfaces, but W4 is not the Lock 14 cleanup wave | Keep W4 localized; do not introduce new grammar directives, BIR variants, or source sidecars |

## Recommended Acceptance Framing

Use these W4 success conditions:

- `unicode_escapes` parse crosses the SPEC §6 threshold: 80.4% sonic to at
  least 95% sonic.
- `y_string_unicode` parse crosses the SPEC §6 threshold: 46.0% sonic to at
  least 70% sonic.
- Direct rows for the same two corpora move in the same direction through the
  existing `unescape_json_string` consumer.
- `unicode_mixed`, `distinct_values`, and plain-string guard rows do not
  regress by 3% or more.
- Correctness pins `decode_json_unicode_escape` parity for all BMP codepoints,
  valid surrogate pairs, invalid surrogate shapes, invalid hex, and exact error
  offsets.

If only `unicode_escapes` moves, reject rather than call it a partial admit:
that is the REDRESS 64 shape signal. If `y_string_unicode` moves but
`unicode_escapes` regresses, suspect x4-path ordering or helper overhead.

## Concrete No-Go Boundaries

Do not implement W4 as any of the following:

- a new or primary four-quartet contiguous validator;
- a new bbnf-simd primitive body without a new vector semantic;
- parser-owned decoded scratch;
- direct-only source-hook folding;
- sink-local decoded stats or semantic hash facts;
- whole-materializer byte-output finalization under the current `Cow<str>` API;
- a route that takes credit for `unicode_mixed` or `distinct_values` improvement
  unless a fresh C1-style count proves real `\uXXXX` content.

## Final Route Statement

The W4 route is admissible only as an internal `unescape_json_string`
improvement:

**preserve the raw span source shape, keep the dense x4 decode path, add a
single-quartet TBL classifier for the scalar fallback, and measure only the two
C1-valid Unicode rows as must-lift outcomes.**

That is different from REDRESS 64 because it is per-quartet materialization,
not four-unit validation. It is different from the SK-V6 escaped-string
materializer rejects because it does not change the generated parser, direct
source hooks, sink representation, or public `Cow<str>` contract.
