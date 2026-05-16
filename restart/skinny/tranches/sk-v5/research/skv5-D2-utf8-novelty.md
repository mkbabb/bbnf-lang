# SK-V5 D2 — UTF-8 SIMD Fold Novelty Interrogation

Scope: verify whether the SK-V5 B1 / B2 / A3 cohort claim — "fold UTF-8
validation into the NEON 16-byte string-body scan" — is NEW work or
re-discovery / extant-planned.

## §1. Per-Claim Verdict

| # | Claim | Verdict | Citation |
|---|-------|---------|----------|
| C1 | `validate_utf8_codepoint` is a ~69 LOC scalar one-codepoint-per-branch validator at `lib.rs:637` | **NEW (verified)** | `parse-that-regex/src/lib.rs:637-706` (70 LOC, RFC-3629 ladder over `0xc2..=0xdf / 0xe0 / 0xe1..=0xec / 0xed / 0xee..=0xef / 0xf0 / 0xf1..=0xf3 / 0xf4`; no DFA, no SWAR, no SIMD) |
| C2 | The 16-byte NEON loop exits on every byte `>= 0x80` and falls through to scalar per-codepoint | **NEW (verified)** | `parse-that-regex/src/lib.rs:331` (`0x80..=0xff if mode != StringMode::ByteString => validate_utf8_codepoint`); the SIMD producer is `bbnf-simd/src/aarch64/string_block.rs:77` (`non_ascii_mask: movemask_u8x16(vcgeq_u8(chunk, vdupq_n_u8(0x80)))`) which is OR-ed into `interesting_mask` at line 24, so the loop returns at line 33 (`first_interesting`) on the first high-bit byte |
| C3 | No Lemire-style 64-byte SIMD UTF-8 validator exists in the tree | **NEW (verified)** | `grep -rni "utf8\|UTF-8\|validate_utf" skinny/crates/bbnf-simd/src/` returns zero hits. `aarch64/mod.rs` exports {`byte_class_from_eq_set_64`, `byte_context`, `cache_hints`, `classify_tbl4`, `digit_mac`, `match_tiny_plain_string`, `movemask`, `quad_load`, `string_block`, `unescape_uxxxx`} — no `utf8`, no `utf8_block`, no `block_validate` |
| C4 | Hoehrmann scalar DFA reference is missing | **NEW (verified)** | `grep -rni "hoehrmann"` returns zero hits across the entire repo. The only scalar UTF-8 validator is the codepoint-at-a-time ladder cited in C1 |
| C5 | Folding UTF-8 validation into the NEON body scan would close the 4 parse-G rows (`twitter` ASCII / `random` Cyrillic / `unicode_basic` / `unicode_mixed`) together | **EXTANT-PLANNED (with caveats)** | The kernel-boundary diagnostic is provable from current code (see §4); the REMEDIATION ("fold UTF-8 into the 16-byte body scan") is already cited in SOTA-BEAT-DESIGN.md:301-304 ("scan-boundary policy"), MASTER-PLAN.md:507 (H.W3 — parse-that primitive closure), and SK-V4 packet §4 line 158 ("invalid UTF-8 rejected at scan boundary, not view access"). The B1/B2/A3 cohort restates the same plan with a more specific kernel target (`scan_string_special_block` extension) |

## §2. Existing UTF-8 / NEON Intrinsic Inventory In `bbnf-simd`

Modules exported by `bbnf-simd/src/aarch64/mod.rs`:

| Module | Purpose | UTF-8 relevance |
|---|---|---|
| `byte_class_from_eq_set_64.rs` | Layer-1 byte-class membership (Lemire 2019 TBL4) | none — ASCII-only alphabet |
| `byte_context.rs` | byte context propagation | none |
| `cache_hints.rs` | prefetch / cache control | none |
| `classify_tbl4.rs` | JSON structural classification via `vqtbl4q_u8` over 64-byte low-6-bit table | none — only `"`, `,`, `:`, `[`, `\\`, `]`, `{`, `}` (see `json_ascii_table` line 61-76). High-byte handling is **absent**; UTF-8 leading-byte categorisation table not present |
| `digit_mac.rs` | digit SWAR for number parse | none |
| `match_tiny_plain_string.rs` | Class A short-string membership (vqtbl4 + shrn movemask) | none — alphabet must be `<= 0x7F` per module doc lines 16-17 ("every JSON structural byte is `<= 0x7F`") |
| `movemask.rs` | `vshrn_n_u16<4>` movemask emulation | none |
| `quad_load.rs` | `vld1q_u8_x4` wrapper | none |
| `string_block.rs` | 16-byte string-body scan (the C2 kernel) | **anti-relevant** — `non_ascii_mask` at line 77 *terminates* the SIMD loop on every `>= 0x80` byte; this is the defect B1/B2 identify |
| `unescape_uxxxx.rs` | NEON `\uXXXX` hex-quad decoder (Class B) | tangential — emits a `u32` scalar codepoint that the *caller* writes via `char::from_u32(...).encode_utf8()`; does not validate UTF-8 source bytes |

`vqtbl4q_u8` usage in the tree: `classify_tbl4.rs:52`, `match_tiny_plain_string.rs:87`. Both use the low-6-bit table shape for ASCII classification. The Lemire-2020 UTF-8 leading-byte LUT (which categorises `0xC2..=0xF4` into width buckets via a second TBL over the high nibble) is **not implemented**.

`vshrn_n_u16` usage: `match_tiny_plain_string.rs:127`, scoped to the movemask emulation. Not used for UTF-8 width extraction.

`vld1q_u8_x4` (64-byte block load — the natural admission width for a Lemire UTF-8 validator): `classify_tbl4.rs:25`, `quad_load.rs:5`. Used only for JSON structural classify; no UTF-8 consumer.

## §3. Git History Scan

`git log --all --oneline --grep -iE 'utf8|utf-8|lemire|hoehrmann|validate_utf|ascii_block'` returns one prior commit on this topic:

```
aa778c8c perf(bench): json_value walker consumes ChildIter, payload_string skips UTF-8 check
```

That commit (2026-04-15) replaced `std::str::from_utf8` with `from_utf8_unchecked` on the **walker** path (`Tape::payload_string`), justified by the decoder kernel `decode_json_string_to_arena` producing well-formed UTF-8 by construction. It is *downstream* of the scan-time validator and orthogonal to the B1/B2/A3 claims.

Wider search:

- `git log --all --grep 'duplicate UTF-8'`: covered by REDRESS.md item 31 (lines 348-351) — removed *duplicate* UTF-8 validation after `match_json_string_at_quote` had already validated the span. The remaining single-source validator is the scalar `validate_utf8_codepoint` that B1/B2/A3 target. No prior SIMD attempt.
- `git log --all --grep -iE 'lemire'`: 10+ hits, all about Eisel-Lemire **float** mantissa (e.g. `9cceb0fa`, `aa778c8c`, `0ca6cd55`, `0d94657e`). Zero hits on Lemire UTF-8 validator.
- `git log --all --grep -i 'hoehrmann'`: zero hits.

REDRESS.md UTF context (lines 27, 332, 348-351, 605): all references describe the *removal of the duplicate* downstream check, not a SIMD upstream replacement. No prior failed attempt at a Lemire/Hoehrmann SIMD validator.

## §4. Final Novelty Verdict

**Diagnostic (C1-C4): NEW, verified.** The four sub-claims about the current implementation are factually correct:

1. The scalar codepoint validator is the only UTF-8 validator in the tree (`parse-that-regex/src/lib.rs:637-706`, 70 LOC, branchful RFC-3629 ladder).
2. The 16-byte NEON body scan (`bbnf-simd/src/aarch64/string_block.rs:65-80`) aborts on `>= 0x80` via `non_ascii_mask` OR-ed into `interesting_mask`. The caller (`skip_json_string_plain` at `lib.rs:420-446`) returns on the first set bit, so for multibyte UTF-8 content the loop alternates 1-2 NEON iterations with a scalar `validate_utf8_codepoint` call — exactly the pathology B2 describes ("Cyrillic / CJK / emoji corpora alternate SIMD/scalar per codepoint").
3. Zero Lemire-style SIMD UTF-8 validator anywhere in `bbnf-simd/src/`. The `classify_tbl4.rs` infrastructure is the right primitive shape (vqtbl4q_u8 + 64-byte table) but is currently aimed only at ASCII JSON structurals.
4. Zero Hoehrmann scalar DFA. The closest is `is_utf8_continuation` (`lib.rs:708-711`), a 3-LOC byte predicate.

**Remediation (C5): EXTANT-PLANNED, but cohort B1/B2/A3 contribute the specific kernel boundary.** The high-level remedy ("UTF-8 validation at scan boundary, fused into the SIMD body scan") is already committed in three places:

- `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:301-304` — "UTF-8 validation is a scan-boundary policy".
- `restart/MASTER-PLAN.md:507` — H.W3 enumerates `UTF8_VALIDATED` flag plumbing, `parse_bytes` scan-boundary validation, and `unicode_*` row gates.
- `restart/skinny/tranches/IMPLEMENTATION-PACKET-SK-V4-ASMJSON-BEAT.md:158` (Wave 2) — "invalid UTF-8 rejected at scan boundary, not view access" as part of the string primitive contract.

The novel B1/B2/A3 contribution is **kernel-level specificity** absent from those plans:
- B1 names `scan_string_special_block` (`string_block.rs:65`) as the fold target and quantifies 25–40% `parse_value_at` self-time attribution on every parse-G row.
- B2 names the lib.rs:331 fall-through site and the per-codepoint alternation as the four-row common cause.
- A3 §`utf8_block.rs` (lines 235-236, 249, 369) proposes the concrete module split (`src/unicode/utf8_block.rs` — Hoehrmann scalar reference + Lemire SIMD dispatch hook) with module-LOC budget (220 LOC) and dependency contract ("no dependents inside this plan but unlocks block-validate everywhere").

**Net verdict:** the *defect attribution* is NEW (no prior cohort named the kernel boundary at this resolution). The *direction* is EXTANT-PLANNED at the policy level. The *implementation packet* (Hoehrmann DFA + Lemire SIMD validator hook + `utf8_block.rs` module split) is NEW work; no prior commit, branch, or document proposes that exact module shape.

Recommendation: accept B1/B2/A3 as the Wave 1 admission packet for `utf8_block.rs`. Treat C5's "closes four parse-G rows together" as a falsifiable claim — gate by the four `unicode_*` + `random` + `twitter` direct/parse rows together, per the BENCH §7.9 Gate 1 + Gate 2 schedule.

## §5. Prior Attempt Failure Mode

No prior attempt at a SIMD UTF-8 validator exists. The closest prior work is:

- `aa778c8c` (2026-04-15) — eliminated *downstream* `std::str::from_utf8` from the walker path by relying on the decoder kernel's by-construction validity. Failure mode: none — this commit succeeded but is unrelated to the upstream scan-time validator.
- REDRESS.md item 31 (2026-05-12) — removed *duplicate* UTF-8 validation after `match_json_string_at_quote`. Failure mode: none — the duplicate was redundant once span-level validation was authoritative. The remaining single-source validator is the one B1/B2/A3 target.

No RE-OPENING-WITH-EVIDENCE applies. The kernel work is unblocked.
