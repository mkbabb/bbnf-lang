# SK-V7 W4 R1 - parse-that-regex Unicode unescape route

Status: read-only research artefact. No source edits.

## Read Scope

- `restart/skinny/tranches/sk-v7/SPEC.md` section 6, especially owner paths, tasks, gates, and revert protocol at lines 214-245.
- `restart/skinny/tranches/sk-v7/HANDOFF.md` section 3 and wave posture at lines 43-107.
- `skinny/REDRESS.md` entries relevant to UTF-8, Unicode escape validation, unescape/materialization, decoded string delivery, and direct string routes.
- `skinny/crates/parse-that-regex/src/lib.rs` around validation, Unicode decode, four-escape decode, unescape, and UTF-8/hex helpers.

## File:Line Findings

1. W4 is explicitly the B1 per-`\\uXXXX` TBL classifier wave, not a broad string refactor. The owner paths are `parse-that-regex/src/lib.rs:911-922`, new `parse-that-regex/src/unicode/escape_decode.rs`, existing `bbnf-simd/src/aarch64/unescape_uxxxx.rs`, and new `bbnf-simd/tests/checkasm_unicode_escape.rs` (`restart/skinny/tranches/sk-v7/SPEC.md:214-220`). The task list says to reuse the existing AArch64 TBL kernel, wire about 30 LOC into `unescape_json_string`, add checkasm parity, and bench only `unicode_escapes` plus `y_string_unicode` parse/direct (`restart/skinny/tranches/sk-v7/SPEC.md:222-227`).

2. The W4 row scope is narrower than the older B1 design text. SPEC says W4 applies only to `unicode_escapes` and `y_string_unicode`; `unicode_mixed` and `distinct_values` have 0 percent `\\uXXXX` and belong to W5 B2 (`restart/skinny/tranches/sk-v7/SPEC.md:223`). The close table matches that: `unicode_escapes` parse must move from 80.4 percent sonic to at least 95 percent, and `y_string_unicode` parse from 46.0 percent to at least 70 percent (`restart/skinny/tranches/sk-v7/SPEC.md:34-35`, `restart/skinny/tranches/sk-v7/SPEC.md:229-238`).

3. W4 is now unblocked by the written entry gate. HANDOFF says W4 starts after W3 closes or if parallel-safe (`restart/skinny/tranches/sk-v7/HANDOFF.md:100-107`), and REDRESS item 81 records the W3 capacity-hinted numeric Vec route as admitted with the W3 gate closed (`skinny/REDRESS.md:2250-2283`).

4. HANDOFF section 3 is binding. It pre-blocks REDRESS 50-55 UTF-8/materializer side routes and REDRESS 60-72 retained/direct materialization routes, including parser-owned decoded scratch, byte-output unescape, and DirectBuild semantic string facts (`restart/skinny/tranches/sk-v7/HANDOFF.md:66-80`). It also calls out twice-rejected Class A NEON tiny-string wiring (`restart/skinny/tranches/sk-v7/HANDOFF.md:81-82`) and older rejected routes such as 12-byte token churn, pair fusion, dispatch tables, capacity prescan, generic SWAR whitespace, separator elision, and EventCursor prepass (`restart/skinny/tranches/sk-v7/HANDOFF.md:84-93`).

5. Parse-time validation and decoded-value unescape are disjoint in `parse-that-regex`. `match_json_string_at_quote_trusted_utf8` marks `needs_unescape` and calls `validate_json_string_escape` on slash bytes (`skinny/crates/parse-that-regex/src/lib.rs:298-323`); the generic matcher does the same with flags (`skinny/crates/parse-that-regex/src/lib.rs:359-405`). `validate_json_string_escape` routes `\\u` into `validate_json_unicode_escape_run` (`skinny/crates/parse-that-regex/src/lib.rs:416-420`), which is a scalar run validator over `read_hex_unit_with_error_offset` and surrogate checks (`skinny/crates/parse-that-regex/src/lib.rs:479-514`). W4 SPEC does not name this validator as the owner call site.

6. The current decoded-value materializer is `unescape_json_string`. It returns borrowed content when there is no backslash after checking for control bytes (`skinny/crates/parse-that-regex/src/lib.rs:854-858`). Escaped strings allocate a `String`, use `find_next_escape_or_control`, copy literal segments, decode simple escapes, and handle `\\u` in the arm at lines 911-922 (`skinny/crates/parse-that-regex/src/lib.rs:860-945`). The scanner used by this materializer is the 8-byte SWAR `find_next_escape_or_control` path (`skinny/crates/parse-that-regex/src/lib.rs:949-975`).

7. The exact current W4 call site is the `Some(b'u')` arm: compute `slash`, try AArch64 `unescape_four_unicode_escapes(bytes, slash, &mut out)`, update `cursor`/`segment_start` and continue on success, otherwise call `decode_json_unicode_escape(bytes, slash)`, `out.push(ch)`, and advance to `next` (`skinny/crates/parse-that-regex/src/lib.rs:911-922`).

8. The scalar Unicode decoder is self-contained and offset-sensitive. `decode_json_unicode_escape` validates the leading `\\u`, reads the first hex unit, joins a following low surrogate when required, rejects lone low surrogates, and returns `(char, cursor)` (`skinny/crates/parse-that-regex/src/lib.rs:434-476`). Hex reads use `read_hex_unit_with_error_offset`, `read_hex_unit_scalar`, and `hex_nibble` (`skinny/crates/parse-that-regex/src/lib.rs:1054-1102`). Existing tests assert boundary codepoints and error offsets (`skinny/crates/parse-that-regex/src/lib.rs:1293-1325`).

9. The existing AArch64 SIMD substrate already has the single-quartet kernel W4 wants to reuse. `unescape_uxxxx_scalar` is the scalar parity anchor (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:33-47`), `unescape_uxxxx_neon` decodes one quartet with TBL plus ASCII range checks (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:58-121`), `unescape_uxxxx_x4_neon` decodes four quartets (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:123-166`), and `join_surrogate_pair_neon` is available for pair combine (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:168-175`).

10. `parse-that-regex/src/unicode/escape_decode.rs` does not exist yet. The current unicode module exports only `utf8_block` and `utf8_hoehrmann` (`skinny/crates/parse-that-regex/src/unicode/mod.rs:1-4`).

11. Existing SIMD parity coverage is partial for W4. `checkasm_parity.rs` tests the scalar `\\uXXXX` reference and single-quartet NEON parity over selected valid/invalid quartets and alignments (`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:446-458`, `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:649-668`). `checkasm_utf8_block.rs` checks one x4 packed case and surrogate join (`skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:58-67`). SPEC's W4 checkasm requirement is broader: every BMP codepoint, surrogate pairs, and invalid hex (`restart/skinny/tranches/sk-v7/SPEC.md:226-238`).

12. Generated retained parse does not call `unescape_json_string`; it records `HAS_ESC` on the offset tape after matching string boundaries (`skinny/crates/runtime/src/grammars/json/generated.rs:90-103`, `skinny/crates/runtime/src/grammars/json/generated.rs:142-156`). Generated direct paths do call sink source hooks with `needs_unescape` (`skinny/crates/runtime/src/grammars/json/generated.rs:439-442`, `skinny/crates/runtime/src/grammars/json/generated.rs:479-482`, `skinny/crates/runtime/src/grammars/json/generated.rs:519-522`, `skinny/crates/runtime/src/grammars/json/generated.rs:560-563`), and default sink hooks call `unescape_json_string` only when needed (`skinny/crates/runtime/src/grammars/json/sink.rs:17-31`, `skinny/crates/runtime/src/grammars/json/sink.rs:44-47`, `skinny/crates/runtime/src/grammars/json/sink.rs:85-88`). The bench direct parser has the same shape (`skinny/crates/bbnf-bench/src/direct_struct.rs:549-559`).

13. Existing SK-V7 profile evidence names `unescape_json_string` as real on direct Unicode rows: `unicode_escapes` spends 47.5 percent in unescape, with line 914 x4 wrapper at 15.9 percent and line 919 scalar fallback at 1.7 percent; `unicode_mixed` spends 23.8 percent in unescape but SPEC assigns it to W5 because it has no `\\uXXXX`; `y_string_unicode` shows line 919 fallback at 7.6 percent and line 914 x4 at 4.9 percent (`restart/skinny/tranches/sk-v7/research/skv7-C2-direct-profile.md:125-129`, `restart/skinny/tranches/sk-v7/research/skv7-C2-direct-profile.md:319-326`, `restart/skinny/tranches/sk-v7/research/skv7-C2-direct-profile.md:367-375`). This supports a W4 experiment, but it also says the dense-run x4 wrapper overhead is material on `unicode_escapes`.

## Exact W4 Owner / Call-Site Shape

The W4 implementation slice should be:

- Add `skinny/crates/parse-that-regex/src/unicode/escape_decode.rs`.
- Export it from `skinny/crates/parse-that-regex/src/unicode/mod.rs`.
- Reuse, not rewrite, `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_neon` and the scalar parity anchor in `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`.
- Replace only the `Some(b'u')` fallback region inside `unescape_json_string` at `skinny/crates/parse-that-regex/src/lib.rs:911-922`.
- Keep the existing dense-run `unescape_four_unicode_escapes` attempt first, because it is already wired and x4-specific.
- After x4 returns `None`, dispatch a single-quartet helper from `unicode::escape_decode` for the current `\\uXXXX`. Preserve existing absolute error offsets for invalid hex, missing low surrogate, invalid low surrogate, and lone low surrogate.
- Keep `decode_json_unicode_escape` available for tests and scalar parity; do not delete or broaden it during W4.
- Add the new dedicated checkasm coverage required by SPEC, rather than relying on the existing partial `checkasm_parity`/`checkasm_utf8_block` coverage.

The call-site shape should remain a local materializer change. No generated runtime template, sink trait, BIR, DirectBuild field fact, parser-owned scratch, retained side table, codegen receiver, or parse-time validator rewrite is part of the W4 owner shape.

## Prior Blocked Routes W4 Must Not Reopen

- Broad UTF-8 fusion / "another boundary check" is insufficient. Trusted UTF-8 string boundary matching was validated as necessary but not enough, and the close belongs elsewhere (`skinny/REDRESS.md:573-587`). The post-escape skip plus validation-batch route regressed `unicode_escapes` and was removed (`skinny/REDRESS.md:589-600`).
- Generic no-allocation decoded visitors over `unescape_json_string` are rejected; source hooks remain the seam, but generic decoded direct delivery must not be reintroduced (`skinny/REDRESS.md:685-713`).
- Parse-time aux side tables are rejected, even though retained traversal probes improved, because governing parse rows regressed (`skinny/REDRESS.md:715-740`).
- EventCursor and parser-local structural cursors are rejected retained-parser routes (`skinny/REDRESS.md:742-767`, `skinny/REDRESS.md:784-813`).
- Exact decoded-string stats sinks and quote-source fused streaming hashers are rejected. Both were correctness-green and lost on escaped-string direct rows (`skinny/REDRESS.md:815-844`, `skinny/REDRESS.md:846-882`).
- The four-unit retained Unicode-escape validator is rejected as shipped. It improved `unicode_escapes` by 31.82 percent but regressed `y_string_unicode` and failed companion lift; do not reopen it without a broader local fact (`skinny/REDRESS.md:1582-1635`).
- Direct source-hook receiver folding is rejected; receiver/closure removal was too small and must not be relabeled as W4 (`skinny/REDRESS.md:1686-1732`).
- Parser-owned decoded scratch is rejected; it made `unicode_escapes` 44.03 percent slower and showed allocation reuse was not the limiting factor (`skinny/REDRESS.md:1734-1785`).
- Byte-output `unescape_json_string` inside the current `Cow<str>` API is rejected; manual byte writes regressed `unicode_escapes` by 4.00 percent (`skinny/REDRESS.md:1787-1835`).
- DirectBuild semantic string fact hashing for the current digest workload is rejected; the primary escaped row regressed by about 15 percent (`skinny/REDRESS.md:1837-1886`).

## Conservative Recommendation

Proceed only as a narrow, falsifiable materializer experiment, and treat it as high-risk for the full W4 close.

The admissible first patch is a decode-only, call-site-local change: add `escape_decode.rs`, use the single-quartet TBL kernel after the existing x4 dense-run path returns `None`, preserve the current `String` output and `Cow<str>` public API, and keep all error offsets byte-identical to the existing tests. The safest version returns the same semantic shape as `decode_json_unicode_escape` (`char` plus next cursor, or a small internal outcome for high-surrogate consumption) and then continues to use the current `out.push(ch)`/cursor flow. That avoids reopening REDRESS 68's byte-output writer and avoids conflating W4 with the W5 plain-string scan widening.

Do not change `validate_json_unicode_escape_run` in W4. That would reopen REDRESS 64's retained validator route, while SPEC names `unescape_json_string` as the owner call site. Do not implement the broader streaming 16-byte body classifier from profile notes in this wave unless W4 is explicitly re-scoped, because it crosses into W5 plain-body scanning and risks repeating the rejected broad-fusion family.

Measurement should be interpreted strictly. If only `unicode_escapes` moves, that likely means the x4/dense-run fact was real but not the per-quartet W4 close. If `y_string_unicode` does not cross the SPEC threshold, revert and record REDRESS rather than widening the patch to parser scratch, sink-local hashing, byte writers, or validation-batch variants. The direct path definitely exercises `unescape_json_string`; retained parse does not appear to materialize decoded strings in the generated parser, so any parse-plane claim needs same-row evidence from `RESULTS.md`, not inference from the materializer call graph.
