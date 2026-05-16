# SK-V7 W5 R4 Risk: B2 Plain-String Scan vs Pre-Blocked Routes

Date: 2026-05-16.

## Scope

Read-only preflight for SPEC section 7 Wave 5. This note distinguishes the admissible
B2 route from V5/V6 routes already rejected in `skinny/REDRESS.md`, names
no-touch surfaces, and records likely failure modes plus revert protocol.

Inputs read:

- `restart/skinny/tranches/sk-v7/SPEC.md` section 7.
- `restart/skinny/tranches/sk-v7/HANDOFF.md` section 3.
- `skinny/REDRESS.md` around entries 28, 33, 50-55, and 60-72.
- `skinny/crates/runtime/src/grammars/json/generated.rs:161-181`.
- `skinny/crates/parse-that-regex/src/lib.rs:298-333` and `:593-705`.
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`.
- `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs`.
- SK-V7 C1/C4 reports for row attribution.

## Executive Finding

W5 B2 is admissible only as a generated-retained, cap-16 tiny-plain-string
probe replacement:

```text
match_tiny_plain_string_with_cap::<16>
  scalar byte loop over at most 16 bytes
  -> one 16-byte quote/backslash/control mask probe
```

It must not become a parse-that full-string scanner change, a UTF-8 validation
fold, a Unicode escape/materialization route, a direct `SinkOnly` route, or a
global cap policy. The reason W5 is not pre-blocked is REDRESS 72: cap 16 is
already admitted for generated retained `OffsetTape` parsing only. W5 changes
the implementation cost of that admitted generated-retained cap-16 probe; it
does not re-argue the cap policy.

## What B2 Is

Current hot leaf:

- `skinny/crates/runtime/src/grammars/json/generated.rs:161-181`
- `match_tiny_plain_string` calls `match_tiny_plain_string_with_cap::<16>`.
- `match_tiny_plain_string_direct` stays `::<8>`.
- The body scans after the opening quote until either:
  - `"` returns `Some(raw_end)`;
  - `\` or byte `< 0x20` returns `None`;
  - cap or end is reached, returning `None`.

The intended implementation shape is a single 16-byte special-byte probe using
the existing `bbnf_simd::aarch64::string_block::scan_string_special_block`
semantics: quote mask, escape mask, control mask with control limit `0x20`.
Only quote/backslash/control masks matter for the tiny probe; non-ASCII bytes
must remain plain because generated retained input is already Rust `&str`.

Target rows per SPEC section 7 and C1 attribution: twitter, update_center,
unicode_basic, random, unicode_mixed, and distinct_values. C1 shows the main
leaf is `generated.rs:173` `match_tiny_plain_string_with_cap::<16>` on the
short-string-heavy rows, while `unicode_mixed` and `distinct_values` have zero
`\uXXXX` content and therefore belong to plain-body/tiny-string work rather
than W4 escape decode.

## Distinction From Pre-Blocked Routes

| Route | Prior verdict | Why W5 B2 is different |
|---|---|---|
| REDRESS 28/33 Class A `match_tiny_plain_string_neon` wiring | Rejected as parse-G fix; earlier Track 1/Track 2 wiring regressed twitter about 25% | W5 does not wire the old TBL exact-membership kernel globally. It replaces only the already-admitted generated-retained cap-16 scalar leaf, preferably via `string_block` quote/backslash/control masks. |
| REDRESS 50-55 SK-V5 UTF-8 fusion/materializer family | Rejected side tables, cursors, decoded stats, and quote-source fused materializer | W5 does not add retained aux facts, parser-local cursors, decoded-string stats, source hooks, or fused semantic materialization. |
| REDRESS 60 trusted-string boundary collapse | Rejected deletion of the tiny probe before `match_string_at_quote` | W5 preserves the tiny probe and makes it cheaper. It must not delete or bypass it. |
| REDRESS 61/62 long or delayed-wide trusted full-string scan | Rejected 64-byte `skip_json_string_plain_trusted` changes | W5 must not edit `skip_json_string_plain_trusted` or widen the parse-that full-string scanner. |
| REDRESS 64 retained Unicode-escape run validator | Rejected four-unit `\uXXXX` validator | W5 does not touch `validate_json_unicode_escape_run`, `unescape_json_string`, or W4 escape-decode code. |
| REDRESS 65 object next-key carry | Rejected object parser-control carry | W5 has no object-loop or `consume_object_next` change. |
| REDRESS 66-69 direct string/materialization family | Rejected source-hook folding, parser scratch, byte-output unescape, semantic string facts | W5 must not touch direct sinks or direct output representation. |
| REDRESS 70/71 typed-output routes | Typed-output schema surface, not string scan | W5 cannot claim real-typed output wins or change host/API schema facts. |
| REDRESS 72 cap-16 policy | Admitted only for generated retained; rejected for direct and Track 2 | W5 inherits this split. Generated retained may use cap 16; direct and hand Track 2 stay cap 8. |

## Concrete No-Touch Surfaces

- `skinny/crates/parse-that-regex/src/lib.rs:298-333`
  `match_json_string_at_quote_trusted_utf8`. Do not alter the dispatcher,
  escape handling, or the old 0x80/UTF-8-validation family around this path.
- `skinny/crates/parse-that-regex/src/lib.rs:593-675`
  validating string scanner. Do not change high-bit handling, UTF-8 prefix
  validation, Unicode escape validation, or error offsets.
- `skinny/crates/parse-that-regex/src/lib.rs:679-705`
  `skip_json_string_plain_trusted`. Do not widen, delay, retune, or add
  attribution-only wrapper changes here for W5.
- `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs`.
  Do not reuse the old Class A TBL exact-membership kernel as the W5 consumer;
  it is the REDRESS 28/33 route and does not naturally encode `< 0x20`.
- Direct and independent hand paths:
  `match_tiny_plain_string_direct::<8>`,
  `skinny/crates/bbnf-bench/src/track2/json.rs`,
  `skinny/crates/codegen/src/json_typed_direct.rs`,
  `skinny/crates/codegen/src/json_sink_direct.rs`.
- W4/B1 escape surfaces:
  `validate_json_unicode_escape_run`, `unescape_json_string`,
  `unicode/escape_decode.rs`, and `bbnf-simd` Unicode escape primitives.
- Generic substrate surfaces: no new BBNF directive, BIR variant, retained side
  table, source sidecar, parser-owned scratch, or DirectBuild semantic fact.

## Likely Failure Modes

| Failure mode | Symptom | Guard |
|---|---|---|
| Accidentally includes `non_ascii_mask` in the tiny decision | Non-ASCII trusted strings fall through or regress `unicode_basic` | Use only quote, escape, and control masks; ignore non-ASCII for tiny retained `&str`. |
| Treats `0x20` as an exact alphabet byte instead of control limit | Allows `0x00..0x1f` before quote or rejects spaces | Use `< 0x20` control semantics, matching `string_block` and scalar behavior. |
| Wrong precedence between quote and bad byte | Returns `Some` even when `\` or control occurs before quote | Accept only if first quote exists and no escape/control bit precedes it. |
| Over-read near EOF or cap tail | Undefined behavior or false success on short buffers | Only issue a 16-byte load under `cursor + 16 <= input.len()`; otherwise keep scalar tail. |
| Direct/Track 2 cap leak | Direct guard rows regress as in REDRESS 72 | Keep `match_tiny_plain_string_direct::<8>` and hand Track 2 untouched. |
| Old Class A TBL route reused | Reopens REDRESS 28/33 and may rebuild tables or miss control range | Prefer `string_block` masks or a tiny wrapper around them, not the old exact-membership API. |
| Call overhead beats scalar leaf | No W5 row crosses threshold despite correctness | Keep helper inline and measured; reject rather than widen into parse-that scanner work. |
| Template/runtime drift | Checked-in generated runtime differs from regen output | If implementation proceeds, change generated runtime and JSON template in lockstep, then run regen/parity gates. |
| Misattributed wide-string rows | `unicode_mixed`/`gsoc-2018` still dominated by `match_string_at_quote` | Do not compensate by editing `skip_json_string_plain_trusted`; record partial failure. |

## Revert Protocol

If implementation fails correctness, checkasm parity, or SPEC section 7's
falsifiability gate, revert the source slice and record a REDRESS rejection.
The rejection should preserve a patch at:

```text
/tmp/skv7-wave-5-b2-rejected.patch
```

Revert exactly the W5 source and generated/template/results edits. Do not
revert unrelated work in the tree. The REDRESS entry must state:

- whether the failure was correctness, checkasm, row threshold miss, or guard
  regression;
- the affected rows and same-run baseline/candidate numbers;
- that the candidate stayed separate from REDRESS 28/33, 50-55, and 60-72, or
  explicitly admit which blocked family it accidentally reopened;
- the routed remainder: full-string trusted scanner work belongs to a separate
  wave with fresh profiles, not to W5 B2.

If only one or two rows improve, treat it as a failed W5 gate rather than a
partial admit. SPEC section 7 requires at least four of six named rows to cross their
thresholds with no row regressing by 3% or more.

## Final Route Statement

W5 B2 is a narrow implementation-risk bet on the already-admitted
generated-retained cap-16 tiny probe:

**replace the scalar 16-byte tiny-plain loop with a quote/backslash/control
mask probe, preserve direct and Track 2 cap-8 behavior, and leave parse-that's
trusted full-string scanner and UTF-8/escape machinery untouched.**

That is the structural line that keeps W5 out of the pre-blocked V5/V6 route
families.
