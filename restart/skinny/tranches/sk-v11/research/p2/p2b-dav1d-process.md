# SK-V11 P2-B: DAV1D/FFmpeg ASM Process
Pass: S-P2 Research. Cycle: V2.
Date: 2026-05-19.
Scope: scalar-oracle and checkasm admission process for SK-V11 SIMD/ASM primitives.
Output: this file.
P1 hot-leaf antecedents: bounded_plain_string_scan; string_escape_decode; unicode_escape_hex_decode; number_digit_span; ascii_whitespace_skip; container_dispatch; simd_movemask; output_digest_hash.
Lock surface: Lock 1 + Lock 14.

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

1. S-P2 is documentation-only for this tranche: the research pass is read-only
   against `skinny/` source and produces design artifacts, with implementation
   deferred until post-S-P3 redress (`restart/prompts/skinny/PASS-2-RESEARCH.md:36`,
   `restart/prompts/skinny/PASS-2-RESEARCH.md:37`,
   `restart/prompts/skinny/PASS-2-RESEARCH.md:38`). The SK-V11 handoff repeats
   that S-P2 writes only under the research tree and edits no source
   (`restart/skinny/tranches/sk-v11/HANDOFF.md:102`,
   `restart/skinny/tranches/sk-v11/HANDOFF.md:103`).
2. The admission rule is already explicit: every SIMD/ASM kernel candidate needs
   a scalar reference, differential/checkasm where applicable, same-host
   microbench, representative slices, feature gate, and same-wave consumer
   (`restart/skinny/tranches/sk-v11/HANDOFF.md:77`,
   `restart/skinny/tranches/sk-v11/HANDOFF.md:78`,
   `restart/skinny/tranches/sk-v11/HANDOFF.md:79`,
   `restart/skinny/tranches/sk-v11/HANDOFF.md:80`). PASS-2 names the same
   scalar-oracle-first rule: SIMD is a checkasm differential against a scalar
   reference (`restart/prompts/skinny/PASS-2-RESEARCH.md:220`,
   `restart/prompts/skinny/PASS-2-RESEARCH.md:221`,
   `restart/prompts/skinny/PASS-2-RESEARCH.md:222`,
   `restart/prompts/skinny/PASS-2-RESEARCH.md:223`,
   `restart/prompts/skinny/PASS-2-RESEARCH.md:224`).
3. The local harness already implements the core FFmpeg-style shape: cloned
   source buffers, scalar `call_ref` versus candidate `call_new`, source-buffer
   mutation checks, alignment sweep, stack canary, signal guard, and robust
   outlier filtering (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:41`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:42`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:43`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:44`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:45`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:46`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:47`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:48`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:49`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:50`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:51`). Externally, checkasm is
   described by VideoLAN as a tool for assembly correctness verification and
   performance benchmarking, with examples that call reference and optimized
   implementations, compare outputs, and benchmark the optimized function [E1].
4. The local harness is intentionally differential and reproducible. It uses a
   deterministic PRNG and JSON-biased plus uniform-random distributions
   (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:53`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:54`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:55`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:56`);
   `checkasm_common.rs` has the reusable xorshift generator and stack-canary
   byte-exact backstop (`skinny/crates/bbnf-simd/tests/checkasm_common.rs:3`,
   `skinny/crates/bbnf-simd/tests/checkasm_common.rs:19`,
   `skinny/crates/bbnf-simd/tests/checkasm_common.rs:50`,
   `skinny/crates/bbnf-simd/tests/checkasm_common.rs:54`,
   `skinny/crates/bbnf-simd/tests/checkasm_common.rs:62`,
   `skinny/crates/bbnf-simd/tests/checkasm_common.rs:67`). FFmpeg checkasm
   likewise wraps checked calls in signal-handler state and stack clobbering,
   and its bench macro uses timers plus an outlier filter [E2].
5. The harness presently has a non-strict default and a strict promotion path.
   The report says random/misaligned divergences are recorded by default and
   promoted by `BBNF_SIMD_STRICT=1`
   (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:58`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:59`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:60`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:61`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:62`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:63`). The test file implements
   `BBNF_SIMD_INJECT_BUG` and `BBNF_SIMD_STRICT` controls
   (`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:107`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:108`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:109`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:112`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:113`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:114`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:115`).
6. Corpus parity is stricter than random parity today: the 17-corpus parity test
   always asserts, while randomized divergence can still be logged when strict
   mode is off (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:77`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:78`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:79`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:81`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:82`;
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:345`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:356`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:369`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:370`).
   SK-V11 production admission must require strict mode for the candidate cell,
   not only corpus parity.
7. A live harness gap is already known: the report documents an open NEON
   `escape_mask_64` boundary handoff divergence on random-noise inputs
   (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:102`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:104`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:105`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:106`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:115`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:116`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:117`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:118`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:119`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:120`,
   `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:121`). Any SK-V11 string or
   escape candidate that relies on the same boundary semantics must first close
   or avoid that exact handoff.
8. The current primitive surface already contains scalar anchors for
   `unicode_escape_hex_decode`, `bounded_plain_string_scan`, and string special
   block scanning: `unescape_uxxxx_scalar`
   (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:33`,
   `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40`,
   `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:46`),
   `match_tiny_plain_string_scalar` and `first_match_scalar`
   (`skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:32`,
   `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:38`,
   `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:54`), and
   `scan_string_special_block_scalar`
   (`skinny/crates/bbnf-simd/src/aarch64/string_block.rs:30`,
   `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:31`,
   `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:36`,
   `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:53`).
9. `unescape_uxxxx_x4_neon` exists and is tested only as a fixed valid packed
   example in the targeted inputs (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:123`,
   `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:125`,
   `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:160`,
   `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:165`;
   `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:58`,
   `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:59`,
   `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:60`,
   `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:61`,
   `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:63`). The x1
   `unescape_uxxxx_neon` path has broader valid/invalid alignment coverage in
   `sk_v3_intrinsic_parity_aarch64`
   (`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:643`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:646`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:647`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:650`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:651`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:656`). Gap: x4 cannot be
   treated as production on current evidence. A retained proof needs an x4 scalar
   oracle plus invalid-lane, mixed-validity, alignment, and surrogate-range
   checkasm coverage; production additionally needs a new source delta and must
   not claim the already-consuming `unescape_string` caller.
10. `match_tiny_plain_string_neon` already has a scalar reference, a table
    builder, a NEON body, and alignment-style parity coverage
    (`skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:63`,
    `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:81`,
    `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:87`,
    `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:89`,
    `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:123`,
    `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:130`;
    `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:582`,
    `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:588`,
    `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:589`,
    `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:594`,
    `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:602`,
    `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:608`). Gap: its
    low-6 table contract is only valid for collision-free alphabets and cannot
    be admitted as a general byte-set classifier without a fallback or
    collision-proofing gate (`skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:75`,
    `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:76`,
    `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:77`,
    `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:78`).
11. `scan_string_special_block` is the current general string-block primitive:
    the scalar reference emits terminator, escape, control, and non-ASCII masks,
    while the NEON body emits the same masks via movemask
    (`skinny/crates/bbnf-simd/src/aarch64/string_block.rs:5`,
    `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:6`,
    `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:7`,
    `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:8`,
    `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:9`,
    `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:10`,
    `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:56`,
    `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:57`,
    `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:66`,
    `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:67`,
    `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:68`,
    `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:69`). Its local
    primitive parity exists, but REDRESS 106 says the caller-level proof for the
    full string route failed, so primitive correctness is not production
    admission (`skinny/REDRESS.md:3152`, `skinny/REDRESS.md:3156`,
    `skinny/REDRESS.md:3157`, `skinny/REDRESS.md:3158`,
    `skinny/REDRESS.md:3159`, `skinny/REDRESS.md:3160`,
    `skinny/REDRESS.md:3161`, `skinny/REDRESS.md:3162`,
    `skinny/REDRESS.md:3163`, `skinny/REDRESS.md:3164`,
    `skinny/REDRESS.md:3165`, `skinny/REDRESS.md:3169`,
    `skinny/REDRESS.md:3170`).
12. The bbnf-simd report names next checkasm targets: direct TBL classifier,
    movemask exhaustive sweep, vext boundary primitives, `escape_mask_64`,
    quad-load, and dav1d-style primitive lifts
    (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:172`,
    `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:174`,
    `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:179`,
    `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:184`,
    `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:188`,
    `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:193`,
    `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:195`). SK-V11 should use that
    list as harness backlog, but only candidates with an S-P1 hot-leaf antecedent
    can enter S-P3 (`restart/prompts/skinny/PASS-2-RESEARCH.md:95`,
    `restart/prompts/skinny/PASS-2-RESEARCH.md:96`,
    `restart/prompts/skinny/PASS-2-RESEARCH.md:119`,
    `restart/prompts/skinny/PASS-2-RESEARCH.md:120`,
    `restart/prompts/skinny/PASS-2-RESEARCH.md:121`,
    `restart/prompts/skinny/PASS-2-RESEARCH.md:122`,
    `restart/prompts/skinny/PASS-2-RESEARCH.md:123`,
    `restart/prompts/skinny/PASS-2-RESEARCH.md:124`).
13. AArch64 is the only implementation target for SK-V11 SIMD/ASM
    (`restart/skinny/tranches/sk-v11/HANDOFF.md:77`,
    `restart/skinny/tranches/sk-v11/HANDOFF.md:111`,
    `restart/skinny/tranches/sk-v11/HANDOFF.md:132`,
    `restart/skinny/tranches/sk-v11/HANDOFF.md:133`). Existing tests already use
    AArch64 feature scoping at the test boundary
    (`skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:1`;
    `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:35`,
    `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:36`,
    `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:37`).
14. Lock 1 and Lock 14 are both binding. PASS-2 rejects a second source scan,
    retained cursor, aux density table, parser-owned structural projection, or
    JSON-only policy in a generic crate (`restart/prompts/skinny/PASS-2-RESEARCH.md:126`,
    `restart/prompts/skinny/PASS-2-RESEARCH.md:127`,
    `restart/prompts/skinny/PASS-2-RESEARCH.md:128`,
    `restart/prompts/skinny/PASS-2-RESEARCH.md:129`,
    `restart/prompts/skinny/PASS-2-RESEARCH.md:130`,
    `restart/prompts/skinny/PASS-2-RESEARCH.md:131`;
    `restart/prompts/skinny/PASS-2-RESEARCH.md:232`,
    `restart/prompts/skinny/PASS-2-RESEARCH.md:233`,
    `restart/prompts/skinny/PASS-2-RESEARCH.md:234`,
    `restart/prompts/skinny/PASS-2-RESEARCH.md:235`,
    `restart/prompts/skinny/PASS-2-RESEARCH.md:236`). Therefore every candidate
    below is a byte-set, classifier, span, mask, or caller-local process gate,
    not a parallel substrate.
15. The VideoLAN checkasm project says its current standalone project was forked
    from dav1d's internal checkasm copy, itself descended from FFmpeg/x264-style
    checkasm variants [E3]. That supports using one "dav1d/FFmpeg/VLC-style"
    admission discipline for SK-V11 rather than treating the names as separate
    processes.

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

Admission process common to all candidates:

- Scalar oracle first: scalar function or exact scalar caller mirror lands before
  the candidate cell is considered. The scalar path defines semantics,
  including invalid input, boundary state, fallback, and grammar policy.
- Differential cell second: candidate runs against cloned inputs, alignment
  sweep, source immutability check, stack canary, signal guard, deterministic
  random inputs, strict failure mode, and bug-injection proof.
- Feature gate third: AArch64 only; any narrower feature such as DotProd must be
  independently gated and have a scalar fallback with identical output.
- Micro-prove-first fourth: caller-local proof on representative slices happens
  before production wiring. Throwaway proof artifacts are allowed pre-S-P3;
  durable harness/production changes wait for the S-P3 wave packet.
- Same-wave consumer last: no orphan primitive. The same wave must consume it in
  a real generated/runtime path or reject it as proof-only.
- V2 hardening rule: a row with no same-wave consumer is explicitly proof-only,
  support-only, or process/oracle-only. It cannot be promoted by reusing an
  already-consuming caller or by primitive parity alone.

| Candidate | Shape + P1 antecedent | Scalar-ref status | Strict parity/checkasm | Micro-proof | Same-wave consumer | Feature/fallback | Reject boundary |
|---|---|---|---|---|---|---|---|
| `HEX_QUARTET_X4_PROOF` | Proof-only decode of four packed fixed-width hex quartets into four lane values; caller owns escape policy, surrogate policy, and fallback. P1: `unicode_escape_hex_decode`; `string_escape_decode`. | Scalar x1 exists; V2 requires a new scalar x4 oracle that calls x1 four times and returns per-lane value plus validity, preserving invalid-lane semantics (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40`, `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:123`). | Add an x4 checkasm cell under `BBNF_SIMD_STRICT=1` over valid lanes, invalid lanes, mixed-validity lanes, alignment 0..63, boundary splits, and surrogate-range code units. The fixed valid packed smoke is insufficient. | Proof-only harness evidence; no production `RESULTS.md` row and no caller speed claim. | None in V2. Specifically do not claim the already-consuming `unescape_string` production path; production requires a new escaped-segment/source delta such as `pt_escaped_string_segments` plus a direct or typed consumer. | AArch64 NEON optional; scalar x4 fallback is mandatory. JSON Unicode and surrogate policy stay outside `bbnf-simd`. | Reject production if the row only re-gates existing `unescape_string`, lacks a new source delta, lacks strict x4 checkasm, or embeds JSON-only surrogate semantics in the primitive. |
| `STRING_SPECIAL_BLOCK_CALLER_MICROPROOF` | One 16-byte block returns terminator, escape, control, non-ASCII masks, and first-interesting offset. P1: `bounded_plain_string_scan`; `string_escape_decode`; `simd_movemask`. | Scalar and NEON bodies exist (`skinny/crates/bbnf-simd/src/aarch64/string_block.rs:30`, `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:56`). A widened 64-byte body would need its own scalar 64 oracle before admission. | Keep primitive parity and require strict mode for any native body: quote offsets, caps, alignments 0..63, escapes, controls, non-ASCII bytes, tails, and close-or-avoid proof for the known `escape_mask_64` handoff. | Caller proof must run before production wiring on the named string corpus: `twitter`, `github_events`, `update_center`, `random`, `distinct_values`, `gsoc-2018`, `unicode_escapes`, `unicode_mixed`, and `y_string_unicode`. Accept only with at least +1.0% geomean movement on that corpus and no member worse than -0.5% versus scalar baseline after rerun. | Same-wave consumer must replace a real local string/key loop, such as generated string/key parse paths, Track 2 tiny-string handling, or direct_struct string loops. If the wave only proves the block primitive, keep it proof-only. | AArch64 NEON with scalar fallback; first admissible product shape is caller-local, not retained `StringBlock16` wrapper or default 64-byte scan. | REDRESS 106 blocks primitive-parity-only production. Reject widened retained scans, generated-retained `StringBlock16`, NEON tiny-string parser wiring, or any proof that omits caller rows/no-regression gates. |
| `BYTE_CLASS_TBL_CLASSIFIER` | TBL/TBX byte-set classifier over 16 or 64 bytes, returning transient masks or first interesting offsets for a grammar-supplied byte class. It emits no retained structural-position vector. P1: `bounded_plain_string_scan`; `ascii_whitespace_skip`; `container_dispatch`; `simd_movemask`. | Current classifier has indirect harness coverage; direct per-block scalar-vs-NEON cell is recommended (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:174`, `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:175`, `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:176`, `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:177`, `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:178`). Tiny-string low-6 table has scalar anchor but collision limits. | Direct strict checkasm must compare scalar byte-set classification to TBL/TBX output for every class-table shape admitted by the grammar template, including alignments, tails, all-member, no-member, and random bytes. | Micro-proof must be attached to the consuming byte-set skip or generated dispatch row; standalone classifier speed is support evidence only. | Same-wave consumer must be `WHITESPACE_BYTE_SET_SKIP`, `CONTAINER_DISPATCH_CLASSIFIER`, or another generated byte-set caller. | AArch64 NEON/TBL/TBX optional with scalar fallback. Low-6 tables require generation-time collision proof or scalar/TBX fallback. | Reject as a general classifier if collisions are not proven or handled, if it becomes FIRST-set/prefix-trie dispatch by itself, or if it adds a second scan/side table. |
| `DIGIT_SPAN_UDOT` | Consume ASCII decimal digit runs in blocks, possibly using DotProd/UDOT to aggregate or validate lanes, returning span length and optional small parsed chunk. P1: `number_digit_span`. | No scalar oracle observed in targeted inputs for an admitted AArch64 UDOT span. The report only names `digit_mac.rs` as a 4-digit MAC/parser module (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:21`). First write scalar `digit_span_ref(bytes, max)`. | Candidate cell sweeps lengths 0..N, alignments, digit/non-digit boundaries, long spans, overflow/truncation boundaries, and random bytes under strict mode. | Micro-proof must show digit-run/span movement without changing f64 fallback, mantissa policy, sign, radix, exponent, or overflow semantics. | Same-wave consumer must be the generated number scanner or a typed/direct numeric field path; otherwise proof-only. | AArch64 DotProd only when feature-gated; scalar fallback must produce identical `DigitRun`/span output. | Reject if it changes numeric conversion/fallback policy, cannot name a generated number consumer, or only proves a 4-digit MAC helper. |
| `WHITESPACE_BYTE_SET_SKIP` | Skip a grammar-provided byte set of whitespace bytes, returning first non-member offset. P1: `ascii_whitespace_skip`; `container_dispatch`. | No dedicated scalar/reference cell observed in targeted inputs. It needs scalar `skip_byte_set_ref(input, set)` and may reuse the byte-set classifier oracle shape. | Strict differential cell sweeps JSON whitespace, CSS whitespace, Sheets/BBNF-self whitespace sets, alignments, all-member, no-member, tail, and boundary cases. | Micro-proof must show the Layer-1 call does not regress short spans and moves at least one residual whitespace-heavy row; comment/trivia loops remain grammar policy. | Same-wave consumer must be a generated parser whitespace/layout call site. | AArch64 NEON/TBL optional with scalar fallback. Grammar supplies the byte set; comments and multi-byte trivia stay outside the primitive. | Reject if it creates a whitespace bitmap/cursor/class column/sidecar, adds a second scan, or hard-codes comment-aware trivia in generic code. |
| `MOVEMASK_EXHAUSTIVE_GATE` | Convert per-lane compare results to a 16-bit or 64-bit mask with stable LSB-first lane order. P1: `simd_movemask`; `container_dispatch`; `bounded_plain_string_scan`. | Movemask exists inside tiny-string and string-block paths; exhaustive sweep is recommended but not shown as landed in targeted inputs (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:179`, `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:180`, `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:181`, `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:182`, `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:183`). | Exhaustive or sampled-exhaustive strict checkasm against scalar lane-bit OR, including lane-order and alignment assertions. | Enabling proof only; attach any performance proof to byte-class, string-block, or dispatch consumer. | Same-wave consumer must be byte-class, string-block, or structural dispatch; no standalone product row. | AArch64 NEON with scalar bit-pack fallback. | Reject as standalone optimization or if lane order is ambiguous for any consumer. |
| `CONTAINER_DISPATCH_CLASSIFIER` | Classify container/structural dispatch bytes into a grammar-owned class value or mask without allocating a retained class column. P1: `container_dispatch`; `simd_movemask`; `bounded_plain_string_scan`. | Current scan parity harness compares scalar `scan_scalar` to `scan_dispatch` positions for a fixed structural alphabet (`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:30`, `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:33`, `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:119`, `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:129`). | Strict parity must compare scalar grammar-owned class/mask output to dispatch output over table shapes, alignments, tails, and mixed structural/non-structural bytes. | Micro-proof must be same-loop and existing-substrate only; no row movement from retained parse-only or sidecar facts. | Same-wave consumer must be a generated dispatch site or generic scan dispatch that keeps Lock 1. | AArch64 NEON dispatch with scalar fallback. | Reject retained class columns, parser-owned sidecars/projections, streaming cursors, object/key/value-byte carry, or any generic JSON role encoding. |
| `OUTPUT_DIGEST_HASH_ORACLE` | Process/oracle only: validate output digest/hash equivalence, or study a fixed digest update only if a real packet exists. P1: `output_digest_hash`. | No bbnf-simd scalar/candidate hash cell observed in targeted inputs; P1 only names the hot leaf (`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:41`, `restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:45`). A packet would need the exact scalar fold/mix oracle. | Product-output parity/oracle only by default. SIMD checkasm is N/A unless a concrete digest algorithm and SIMD body are proposed. | No parser row movement claim from digest-only speed. A concrete packet must prove same-output behavior and separate digest cost from parser work. | None by default. Promotion requires a same-wave report/gate or direct/typed/non-JSON host consumer that actually uses the digest update. | Default scalar process. SIMD only behind exact algorithm, feature gate, and scalar fallback. | Reject as generic parser primitive, `bbnf-simd` shortlist item, or row mover unless a real consumer packet names scalar source, parity, feature/fallback, and cost gate. |

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

| Candidate | Verdict | Grammar-neutral framing |
|---|---|---|
| `HEX_QUARTET_X4_PROOF` | Grammar-neutral proof core, not production. | The primitive is fixed-width hex-quartet decode only. JSON surrogate joining stays in generated JSON caller policy. CSS may use a separate variable-width escape template; Sheets and BBNF-self use their own escape policies where present. Do not put JSON Unicode policy in `bbnf-simd`, and do not claim production via existing `unescape_string`. |
| `STRING_SPECIAL_BLOCK_CALLER_MICROPROOF` | Generalisable. | The primitive accepts `terminator`, `escape`, and `control_limit`, and returns masks. It fits JSON strings, CSS quoted strings, Sheets text literals, and BBNF terminals as a byte-level "interesting byte" scan. |
| `BYTE_CLASS_TBL_CLASSIFIER` | Generalisable if table construction is grammar-owned. | JSON structurals are one byte set, not the primitive contract. CSS delimiters, Sheets formula separators/operators, and BBNF syntax bytes can use the same byte-class table if collisions/fallbacks are handled by the generator. |
| `DIGIT_SPAN_UDOT` | Generalisable. | ASCII digit spans are shared by JSON numbers, CSS numeric tokens, Sheets numerics, and BBNF-self numeric annotations. Grammar-specific sign, radix, exponent, decimal, and overflow policy remains outside the primitive. |
| `WHITESPACE_BYTE_SET_SKIP` | Generalisable. | The byte set is supplied by the grammar. JSON, CSS, Sheets, and BBNF-self can define different whitespace classes without changing the primitive. Comments or multi-byte whitespace remain caller policy, not generic SIMD policy. |
| `MOVEMASK_EXHAUSTIVE_GATE` | Generalisable. | Movemask is a lane-mask primitive with no grammar semantics. It is only admitted as support for byte-class, string-block, or dispatch consumers. |
| `CONTAINER_DISPATCH_CLASSIFIER` | Generalisable only as class/mask emission. | A grammar can map structural bytes to dispatch classes, but the primitive must not encode JSON object/array roles. It returns byte classes or positions for the existing substrate. |
| `OUTPUT_DIGEST_HASH_ORACLE` | Grammar-neutral as verification, not parser semantics. | Digest/hash equivalence is an output-plane guard and benchmark oracle. If accelerated, it must be tied to an exact report/gate or host consumer packet and not to JSON token policy. |

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

1. REDRESS 88: do not make PMULL prefix-XOR a default hot body. It was correct
   and visible in asm, but parse measurements regressed escape-heavy and narrow
   rows (`skinny/REDRESS.md:2510`, `skinny/REDRESS.md:2517`,
   `skinny/REDRESS.md:2520`, `skinny/REDRESS.md:2527`,
   `skinny/REDRESS.md:2535`, `skinny/REDRESS.md:2536`,
   `skinny/REDRESS.md:2537`, `skinny/REDRESS.md:2538`,
   `skinny/REDRESS.md:2539`, `skinny/REDRESS.md:2540`).
2. REDRESS 89: do not route CTZ/next-bit bulk consumers into production merely
   because checkasm passes. W10b passed correctness and primitive-checkasm, then
   failed per-row maintain gates (`skinny/REDRESS.md:2544`,
   `skinny/REDRESS.md:2552`, `skinny/REDRESS.md:2555`,
   `skinny/REDRESS.md:2573`, `skinny/REDRESS.md:2574`,
   `skinny/REDRESS.md:2580`, `skinny/REDRESS.md:2581`,
   `skinny/REDRESS.md:2582`, `skinny/REDRESS.md:2583`,
   `skinny/REDRESS.md:2584`, `skinny/REDRESS.md:2585`).
3. REDRESS 96 and 97: do not reintroduce class-column, move-consumed
   structural-index, retained cursor, or streaming-cursor substrate routes.
   Both correctness-green attempts regressed integrated parse-loop gates
   (`skinny/REDRESS.md:2795`, `skinny/REDRESS.md:2797`,
   `skinny/REDRESS.md:2798`, `skinny/REDRESS.md:2799`,
   `skinny/REDRESS.md:2800`, `skinny/REDRESS.md:2801`,
   `skinny/REDRESS.md:2802`, `skinny/REDRESS.md:2803`;
   `skinny/REDRESS.md:2852`, `skinny/REDRESS.md:2854`,
   `skinny/REDRESS.md:2855`, `skinny/REDRESS.md:2856`,
   `skinny/REDRESS.md:2857`, `skinny/REDRESS.md:2881`,
   `skinny/REDRESS.md:2897`, `skinny/REDRESS.md:2898`,
   `skinny/REDRESS.md:2899`, `skinny/REDRESS.md:2900`,
   `skinny/REDRESS.md:2901`, `skinny/REDRESS.md:2904`,
   `skinny/REDRESS.md:2905`, `skinny/REDRESS.md:2906`).
4. REDRESS 102: parse-only remains firewalled. No SIMD candidate may use a
   parse-only `S / NO-GO` fact as admission, and W4 or later work cannot name W3
   as substrate dependency (`skinny/REDRESS.md:3042`,
   `skinny/REDRESS.md:3043`, `skinny/REDRESS.md:3044`,
   `skinny/REDRESS.md:3045`, `skinny/REDRESS.md:3046`,
   `skinny/REDRESS.md:3047`, `skinny/REDRESS.md:3048`,
   `skinny/REDRESS.md:3049`, `skinny/REDRESS.md:3050`,
   `skinny/REDRESS.md:3051`, `skinny/REDRESS.md:3057`,
   `skinny/REDRESS.md:3058`).
5. REDRESS 106: string-block primitive parity does not admit the full string
   caller. The prior caller microproof failed aggregate threshold, so SK-V11
   must rerun a new caller proof before any string-block production claim
   (`skinny/REDRESS.md:3152`, `skinny/REDRESS.md:3156`,
   `skinny/REDRESS.md:3157`, `skinny/REDRESS.md:3158`,
   `skinny/REDRESS.md:3159`, `skinny/REDRESS.md:3160`,
   `skinny/REDRESS.md:3161`, `skinny/REDRESS.md:3162`,
   `skinny/REDRESS.md:3163`, `skinny/REDRESS.md:3164`,
   `skinny/REDRESS.md:3165`, `skinny/REDRESS.md:3169`,
   `skinny/REDRESS.md:3170`).
6. REDRESS 107: `unescape_uxxxx_x4_neon` is proof-closed, not production
   behavior. Its W8 evidence can guide SK-V11, but only a same-wave real caller
   delta can admit production (`skinny/REDRESS.md:3174`,
   `skinny/REDRESS.md:3175`, `skinny/REDRESS.md:3176`,
   `skinny/REDRESS.md:3177`, `skinny/REDRESS.md:3182`,
   `skinny/REDRESS.md:3183`, `skinny/REDRESS.md:3185`,
   `skinny/REDRESS.md:3186`, `skinny/REDRESS.md:3187`,
   `skinny/REDRESS.md:3188`, `skinny/REDRESS.md:3194`,
   `skinny/REDRESS.md:3195`, `skinny/REDRESS.md:3196`).
7. REDRESS 108: do not claim same-wave production by wrapping or re-gating an
   already-consumed caller. W9 was rejected because the exact caller had already
   consumed x4 before the wave and no real source delta existed
   (`skinny/REDRESS.md:3200`, `skinny/REDRESS.md:3201`,
   `skinny/REDRESS.md:3202`, `skinny/REDRESS.md:3203`,
   `skinny/REDRESS.md:3204`, `skinny/REDRESS.md:3205`,
   `skinny/REDRESS.md:3220`, `skinny/REDRESS.md:3221`,
   `skinny/REDRESS.md:3222`).

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

Local sources:

- `restart/prompts/skinny/PASS-2-RESEARCH.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`
- `skinny/crates/bbnf-simd/tests/checkasm_common.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs`
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`
- `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs`
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`
- `skinny/REDRESS.md`

External sources:

- [E1] VideoLAN checkasm project page, lines 56-58 and 95-126:
  https://www-test.videolan.org/projects/checkasm/
- [E2] FFmpeg `tests/checkasm/checkasm.h` source, lines 599-608 and 763-815:
  https://ffmpeg.org/doxygen/trunk/checkasm_8h_source.html
- [E3] VideoLAN checkasm project page, lines 189-193:
  https://www-test.videolan.org/projects/checkasm/
