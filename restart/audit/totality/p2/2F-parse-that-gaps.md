---
agent: 2F
pass: T-P2-research
cycle: V5
generated_at: 2026-05-23T00:00:00Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F-coherence-scan, 1F-anti-pattern, 1F-past-corpora]
primary_sources_cited: 24
counted_source_ids: [SRC-COX-REGEX, SRC-RE2, SRC-RUST-REGEX, SRC-MEMCHR, SRC-FASTFLOAT, SRC-FNF, SRC-CLINGER, SRC-SIMDJSON-PAPER, SRC-SIMDJSON-SRC, SRC-SIMDUTF, SRC-HOEHRMANN, SRC-MULA-LEMIRE, SRC-XXHASH, SRC-RFC3629, SRC-RFC8259, SRC-PARSE-THAT-REGEX, SRC-PARSE-THAT-DOCS, SRC-BBNF-REGEX, SRC-BBNF-SIMD, SRC-BBNF-CODEGEN, SRC-BBNF-RUNTIME, SRC-BBNF-DIGEST, SRC-REDRESS, SRC-S-P2-V3-CONSOL, SRC-S-P3-A-V1, SRC-T-P1]
techniques_grounded: 14
techniques_refuted: 6
prior_cycle_dispositions_folded:
  accepted: [PTG-REGEX-HIR-ENGINE, PTG-REGEX-INFO-FACTS, PTG-REGEX-OPAQUE-PATTERN, PTG-RETAINED-STRUCTURAL-SUBSTRATE, PTG-DIGEST-SEMANTIC-MIX, PTG-FLOAT-CLINGER-EISEL, PTG-FLOAT-NO-FALLBACK, PTG-SAME-WAVE-CONSUMER]
  rejected: []
  revised: [PTG-SIMD-SPAN-SCAN, PTG-STRING-SCAN-UTF8, PTG-UNICODE-ESCAPE-CODEC, PTG-INTEGER-SWAR-DOTPROD, PTG-CSS-SCANNER-GAP]
  first_cycle_additions: [PTG-UNESCAPE-STRING-FRONTLOAD, PTG-UTF8-STREAMING-SPLIT, PTG-RANGE-CLASS-PRIMITIVE, PTG-PREV-IN-STRING-LOCK1]
v5_fold_authority:
  v2_fold_addendum: restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md
  v3_fold_addendum: restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md
  v4_fold_addendum: restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md
  v5_fold_dispatch: S-P3-P3-A-V1-C3-C4-SAME-WAVE-CONSUMER-BIND
parse_that_import_authority: workspace-collapsed-into-parse-that-regex-only
parse_that_workspace_status: parse-that base crate is NOT in skinny/Cargo.toml; only parse-that-regex is a workspace member (per skinny/Cargo.toml:10)
locks_amendment_candidates: 4
---

## Executive Summary

`parse-that-regex` (`skinny/crates/parse-that-regex/src/lib.rs:1-1214`) and
`bbnf-regex` (`skinny/crates/bbnf-regex/src/lib.rs:1-322`) are the
parse-that crate family at HEAD. The base `parse-that` crate referenced in
`docs/parse-that/regex-engine.md` is **not** present in
`skinny/Cargo.toml` — the workspace collapses to the two siblings, with
`bbnf-regex` a 322-line classifier crate (no DFA, no NFA, no `find_at`) and
`parse-that-regex` the Layer-1 string + unicode + number consumer surface.

Nine primitive gaps surface against the SK-V14 SPEC waves: **two regex/HIR
gaps** (the bespoke Thompson-NFA → DFA → Hopcroft pipeline documented at
`docs/parse-that/regex-engine.md:15-25` is not implemented at HEAD; lazy-DFA
fallback for state-limit-exceeding patterns at `regex-engine.md:62` is
absent). **Four SIMD-scan gaps** (64-byte structural-index sweep wrapper;
quote-aware classifier + prefix-XOR compose; streaming UTF-8 validator with
continuation-state carry; inclusive range membership). **Two string gaps**
(`unescape_string` 64-byte front-load on `find_next_escape_or_control`;
multi-codepoint UTF-8 width-decode driver). **One float gap** (16-digit
UDOT pack feeding the existing Eisel-Lemire mantissa — the 128-bit multiply
itself is not in scope).

Per gap: published primitive cited (Clinger 1990 PLDI; Lemire 2021 SPE
Eisel-Lemire; Langdale-Lemire 2019 VLDB simdjson; Keiser-Lemire 2020 UTF-8;
Hoehrmann 2009 UTF-8 DFA; Mula-Lemire 2019 shuffle byte-class), upstream-or-vendor
decision named (regex gaps upstream into `bbnf-regex`; SIMD/string/float gaps
vendor into `bbnf-simd` Layer-1 + `parse-that-regex` consumer wiring), and
the bbnf-specific need against SK-V14 SPEC waves W0..W11 stated (anchored by
S-P3 P3-A V1 candidates C1/C3/C4/C7 same-wave-consumer bindings).

Three load-bearing refutations: (i) the simdjson cross-call retained
`prev_in_string` quote-mask design is **inadmissible** under Lock 1
substrate-union (REDRESS 96/97/98 closed this in SK-V9); (ii)
`parse_that_regex::unescape_string` at `lib.rs:718` is **not** a SIMD body
— it is a fast-path classifier whose hot leaf is the 8-byte SWAR
`find_next_escape_or_control` at `lib.rs:813`; the SIMD body lives in
`bbnf-simd`, not in `parse-that-regex`; (iii) `regex-syntax` HIR is **not**
an admissible runtime dependency per `docs/parse-that/regex-engine.md:9` —
the lazy-DFA fallback must NOT pull `regex-automata` into the runtime
crate graph.

## V5 Fold Authority

This dossier folds V4 dispositions through the shared mechanical contracts
at V2/V3/V4 addenda plus the V5 same-wave-consumer binding from the S-P3
P3-A V1 LOCKED candidate shortlist (C3 + C4 cell-(c) cross-references).

| CH lens | 2F V5 fold |
|---|---|
| CH1 correctness / provenance | Pins parse-that-regex + bbnf-regex at workspace HEAD; collapses the V4 "parse-that worktree as conditional authority" frame into a definitive "base crate NOT in workspace" finding per `skinny/Cargo.toml:10` (executable-verified). The regex-engine.md pipeline becomes design documentation for `bbnf-regex` absorption, not import authority. |
| CH2 generality / Lock 14 | Each gap row carries an explicit grammar-neutrality verdict per S-P2 V3 P2-E §3 (8/8 gaps grammar-neutral); the regex/HIR gaps are grammar-neutral by construction (operate on patterns); the SIMD/string/float gaps carry parametric byte sets / ranges / escape bytes. |
| CH3 regression / REDRESS | REDRESS 96/97/98 (retained class-column / streaming structural cursor / class-lane-only) bind the prev-in-string refutation; REDRESS 50-55 (UTF-8 fusion routes) bind the UTF-8 streaming validator's validate-only frame; REDRESS 80 (canada mantissa-widen) binds the float gap's generic-primitive material differential per S-P3 P3-A C3 row `p3a:97`. |
| CH4 cost / executability | Every gap names: (a) scalar reference path:line PRESENT at HEAD; (b) checkasm-parity status (EXTENSION); (c) same-wave consumer named per S-P3 P3-A V1 cell-(c) bindings. The V4 admission ledger format (V4 fold addendum) is the per-row schema; consumer/state/disposition normalized via V3 addendum. |
| CH5 hidden coupling / Lock 1 | Substrate-target declared per gap: `local_temp_only` (kernels emitting transient masks/offsets) / `direct_sink` (number mantissa) / `existing_tape` (substrate-walk paired with C2). Zero gaps propose a parallel cursor or retained sidecar; the prev-in-string refutation is the load-bearing CH5 row. |
| CH6 anti-paper-close | Each gap's bbnf-specific need carries the named S-P1 hot-leaf antecedent + the row-moving consumer (not "SOTA does it this way"); the unescape_string refutation row is the load-bearing CH6 honesty contribution (clarifies that S-P3 P3-A's mapping of `lib.rs:718` to candidate C1 names the consumer, not the SIMD body). |

## Crate-surface frontmatter (executable-verified at HEAD)

`skinny/Cargo.toml:8-13` workspace members include `crates/parse-that-regex`
but **not** a `crates/parse-that` member (verified via direct read; the
docs/parse-that/* design docs at `docs/parse-that/regex-engine.md:1-135` +
`docs/parse-that/leaf-parsers.md` + `combinators.md` + `overview.md` +
`span-combinators.md` describe a base crate that does not appear in the
workspace).

`bbnf-regex` at `skinny/crates/bbnf-regex/src/lib.rs:1-322`: classifier crate
exporting `RegexFacts`, `ByteSet256` (256-bit bitset), `FirstSet::{Exact,
Unknown}`, `RegexKind::{Whitespace, QuotedString, Numeric, Unknown}`,
`StringFacts { delimiter: u8, escape: u8 }`, `analyze(pattern: &str) ->
RegexFacts` at `:45`. No DFA. No NFA. No Thompson construction. No
Hopcroft minimization. No `find_at`. The 322-line crate covers
nullability + first-set + byte-class + heuristic kind classification
(three literal-string matches at `:92-105` — `r"[ \t\n\r]*"` →
`Whitespace`, `pattern.starts_with('"')` → `QuotedString`, numeric prefix
match → `Numeric`).

`parse-that-regex` at `skinny/crates/parse-that-regex/src/lib.rs:1-1214` +
`number/mod.rs:1-280` + `number/eisel_lemire/{mod.rs:1-177, algorithm.rs:1-94,
table.rs:1-660}` + `unicode/{mod.rs:1-4, utf8_block.rs:1-36, utf8_hoehrmann.rs:1-87}`
+ `integration/{mod.rs, simd_scan_hook.rs:1-19}`: the Layer-1 string +
unicode + number consumer surface. `Cargo.toml:1-10` shows deps
`bbnf-simd.workspace=true` + `thiserror.workspace=true` (verified
executable). Workspace consumers per `grep -n "parse-that"
skinny/{Cargo.toml,crates/runtime/Cargo.toml,crates/bbnf-bench/Cargo.toml}`:
`runtime/Cargo.toml:11` + `bbnf-bench/Cargo.toml:17`.

## Source Registry

| ID | Primary source | Use |
|---|---|---|
| SRC-COX-REGEX | Russ Cox, "Regular Expression Matching Can Be Simple And Fast" (swtch.com/~rsc/regexp/regexp1.html, 2007) | Thompson-NFA route; refutes backtracking as totality baseline. |
| SRC-RE2 | Google RE2 at HEAD `972a15cedd008d846f1a39b2e88ce48d7f166cbd` (github.com/google/re2) | Production finite-automata regex discipline: linear-time, bounded memory. |
| SRC-RUST-REGEX | Rust `regex`, `regex-automata`, `regex-syntax` at HEAD `839d16bc65b60e2006d3599d20bfa6efc14049d8` (github.com/rust-lang/regex) | Comparator for the bespoke `Dfa::compile()` lazy fallback case at `regex-engine.md:62`. |
| SRC-MEMCHR | BurntSushi `memchr` at HEAD `db1a77d4b556a1321e136ca0514e43e74ea5fcc3` (github.com/BurntSushi/memchr) | Byte-search primitives for the 1/2/3-exit-byte self-loop acceleration at `regex-engine.md:108-115`. |
| SRC-FASTFLOAT | `fast_float` at HEAD `05087a303dad9c98768b33c829d398223a649bc6` (github.com/fastfloat/fast_float) | Lemire's C++ implementation; Rust `fast_float2` v0.2.3 is the direct copy source at `parse-that-regex/src/number/eisel_lemire/mod.rs:3`. |
| SRC-FNF | Noble Mushtak and Daniel Lemire, "Fast Number Parsing Without Fallback" (arXiv:2212.06644) | Refines Eisel-Lemire fallback risk; bbnf should measure fallback rather than claim no-fallback. |
| SRC-CLINGER | William D. Clinger, "How to Read Floating-Point Numbers Accurately", PLDI 1990, DOI 10.1145/93542.93557 | Correct-rounding basis for the Clinger fast path at `eisel_lemire/mod.rs:99 try_fast_path_f64`. |
| SRC-SIMDJSON-PAPER | Geoff Langdale and Daniel Lemire, "Parsing Gigabytes of JSON per Second", VLDB Journal 2019 (arXiv:1902.08318) | Structural-index + quote-aware classifier + backslash prefix-XOR carry shape. |
| SRC-SIMDJSON-SRC | simdjson at HEAD `168ef580757d75270475b379e83c2b39787a6765` (pinned per S-P2 V3 §5.3) | `include/simdjson/arm64/stage1.h find_structural_bits`; `arm64/simd.h find_quote_mask_and_bits`; `generic/stage2/string_parsing.h unescape_string`. |
| SRC-SIMDUTF | simdutf 5.x at github.com/simdutf/simdutf — `src/arm64/arm_validate_utf8.cpp`; `src/scalar/utf8.h` | Vectorized UTF-8 validation via `vqtbl1q_u8` over a 16-entry leading-byte class table. |
| SRC-HOEHRMANN | Björn Höhrmann, "Flexible and Economical UTF-8 Decoder" (bjoern.hoehrmann.de/utf-8/decoder/dfa/, 2009) | DFA with 9 states + 12 character classes; constant-space streaming validator. Reference impl at `parse-that-regex/src/unicode/utf8_hoehrmann.rs:1-87`. |
| SRC-MULA-LEMIRE | Wojciech Muła and Daniel Lemire, "Faster shuffle-based byte-class classification" (2019); Mula-Lemire 2018 PDEP/PEXT studies | `vqtbl1q_u8`/`tbl`/`vpshufb` for ≤8-element byte-class via low-nibble or low+high-nibble LUT. |
| SRC-XXHASH | xxHash at HEAD `e573d4d2aaeaba0f3e5a0a9a54144a1f2b4b56e7` (github.com/Cyan4973/xxHash) | Byte-hash baseline; not a substitute for the bbnf semantic-output direct digest contract. |
| SRC-RFC3629 | F. Yergeau, "UTF-8, a transformation format of ISO 10646", RFC 3629 (November 2003) | UTF-8 byte-sequence binding cited by JSON RFC 8259 §8.1. |
| SRC-RFC8259 | T. Bray, "The JavaScript Object Notation (JSON) Data Interchange Format", RFC 8259 (December 2017) | §6 numbers; §7 strings; §8.1 UTF-8 encoding mandate. |
| SRC-PARSE-THAT-REGEX | `skinny/crates/parse-that-regex/src/lib.rs:1-1214` + `number/mod.rs:1-280` + `number/eisel_lemire/{mod.rs:1-177, algorithm.rs:1-94, table.rs:1-660}` + `unicode/{mod.rs:1-4, utf8_block.rs:1-36, utf8_hoehrmann.rs:1-87}` + `integration/simd_scan_hook.rs:1-19` + `Cargo.toml:1-10` | Live skinny parse-that-regex Layer-1 surface. |
| SRC-PARSE-THAT-DOCS | `docs/parse-that/regex-engine.md:1-135` + `combinators.md` + `leaf-parsers.md` + `overview.md` + `span-combinators.md` | Design surface for the unimplemented NFA→DFA pipeline; runtime `find_at` described at `regex-engine.md:91-100`. |
| SRC-BBNF-REGEX | `skinny/crates/bbnf-regex/src/lib.rs:1-322` (322 lines including tests; no external deps in imports) | Live bbnf-regex classifier crate (no DFA at HEAD). |
| SRC-BBNF-SIMD | `skinny/crates/bbnf-simd/src/lib.rs` (Layer-1 surface) + `aarch64/` (NEON kernels) + `scalar/` (8-file scalar-reference siblings at `bitmap_next_set_bit.rs`, `bitmap_prefix_xor_64.rs`, `bulk_emit_positions_64.rs`, `byte_class_from_eq_set_64.rs`, `byte_class_from_table_64.rs`, `eob_pad_clamp.rs`, `mod.rs`, `swar_8byte.rs`) | Kernel + scalar-reference home for Layer-1 SIMD/string/float gap vendoring. |
| SRC-BBNF-CODEGEN | `skinny/crates/codegen/src/lower/sink_only.rs:19-93, 142-181`; `crates/codegen/src/json_sink_direct.rs:34-45, 315-370` | BIR `SinkOnlyExpr::RegexProgram` stores opaque `pattern: String` only — refuted in V4 carry-forward. |
| SRC-BBNF-RUNTIME | `skinny/crates/runtime/src/grammars/json/generated.rs:33-237, 466, 506`; `runtime/src/grammars/json/scan.rs:22, 32, 107, 164`; `runtime/src/tape/{mod.rs:92,94, assembler.rs:42}`; `runtime/src/grammars/json/parser.rs:10` | Generated JSON direct consumers + tape substrate surface; substrate-union YES at HEAD per CH5 V3 §2 seven-witness corroboration. |
| SRC-BBNF-DIGEST | `skinny/crates/bbnf-bench/src/direct_struct.rs:15-29, 58-105, 401-425, 716-742` | Direct semantic digest shape (Track 1/Track 2/serde/sonic-rs strict parity). |
| SRC-REDRESS | `skinny/REDRESS.md:517-557, 633-649, 700-713, 846-882, 2910-2940, 3495-3528, 3603-3633, 3780-3805` (V4 carry-forward) plus REDRESS 80 / 88 / 89 / 96 / 97 / 98 surfaces per V3 §1.4 of P2-E. | Pre-block ledger: Lock-1 substrate-union closure (96/97/98); float-overfit (80); PMULL prefix-XOR (88); CSSC CTZ (89). |
| SRC-S-P2-V3-CONSOL | `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md:1-668` (V3 §3Z LOCK declaration; §6.1 CF-3; §6.2 NF-CH6-4 canonical-name binding; §6.3 F-V2-P1ABC-RERECORD Stage-0 commitment) | S-P2 cohort LOCK authority binding the 9-gap shortlist into S-P3 candidates. |
| SRC-S-P3-A-V1 | `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md:1-316` (8-candidate shortlist with CF-3 3-gate admission cell; C3 row line 87-99; C4 row line 100-111) | S-P3 same-wave-consumer bindings; C3 + C4 same-wave-consumer choices implicate `parse-that-regex::unescape_string` at `lib.rs:718` per dispatch context. |
| SRC-T-P1 | `restart/audit/totality/p1/{1A,1B,1C,1D,1E,1F-coherence-scan,1F-anti-pattern,1F-past-corpora}.md` (T-P1 V5 LOCKED) + `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md` | T-P1 evidence base: substrate-union 1A-DIV-008; pattern-H 67 hand-written grammar files; LAC-1E-12 executable verification. |

## Technique Grounding Table

| Gap-id | Published source (registry) | Status | Crate-target / upstream-vs-vendor | bbnf-specific note |
|---|---|---|---|---|
| **PTG-REGEX-HIR-ENGINE** | SRC-COX-REGEX + SRC-RE2 + SRC-RUST-REGEX + SRC-PARSE-THAT-DOCS | partial (design only) | **Upstream into `bbnf-regex` crate** (bbnf-authored, no external deps at HEAD) | The compile-time NFA→DFA pipeline at `regex-engine.md:15-25` is absent; bbnf-regex carries only `RegexFacts` + `ByteSet256` + heuristic `classify` at `lib.rs:92-105`. SK-V14 SPEC W0..W11 do not admit a runtime regex engine; grammar-authoritative Phase 3 host functions may. |
| **PTG-REGEX-LAZY-DFA-FALLBACK** | SRC-RUST-REGEX (regex-automata::meta::Regex lazy-DFA) + SRC-PARSE-THAT-DOCS (`regex-engine.md:62` "Dfa::compile() returns None") | partial | **Build-script vendor** into `bbnf-regex` (compile-time only; never runtime per `regex-engine.md:9`) | Required only if the runtime DFA dispatch surface lands; covers the state-limit-exceeding case where the bespoke DFA bails. |
| **PTG-SIMD-SPAN-SCAN-SWEEP-64** | SRC-SIMDJSON-PAPER + SRC-SIMDJSON-SRC (arm64/stage1.h find_structural_bits) + SRC-MULA-LEMIRE | grounded kernel; sweep wrapper missing | **Vendor in `bbnf-simd::aarch64::string_block`** (16-byte body Wave-1-admitted at `bbnf-simd/src/aarch64/string_block.rs:57`; 64-byte sweep wrapper missing) | S-P3 P3-A candidate C1 `long_string_body_simd_scan` per `p3a:61-73` + S-P2 V3 §6.2 NF-CH6-4 canonical-name binding (one primitive across P2-A C2 ∪ P2-E Gap 1 ∪ P2-F C1+C2). Required for `unicode_escapes` / `twitter` / `gsoc-2018` / `mesh` / `github_events` direct-plane row movement. |
| **PTG-PREV-IN-STRING-LOCK1** | SRC-SIMDJSON-SRC (arm64/simd.h find_quote_mask_and_bits) + SRC-REDRESS 96/97/98 | **refuted as retained-substrate**; grounded as per-call composed form | **Vendor per-call form in `bbnf-simd::aarch64::string_block`** (NOT a `prev_in_string` parameter; carry stays inside one 64-byte call) | simdjson retains cross-call `prev_in_string` to achieve 1 GB/s; bbnf Lock 1 substrate-union closes that route (REDRESS 96/97/98). Per-call composition (S-P2 V3 Gap 6 `scan_string_with_carry_64`) is admissible but caps the per-call SIMD ceiling. |
| **PTG-STRING-SCAN-UTF8-STREAMING** | SRC-SIMDJSON-PAPER + SRC-SIMDUTF + SRC-HOEHRMANN + SRC-PARSE-THAT-REGEX | partial | **Vendor in `bbnf-simd::aarch64::utf8::validate_block_streaming`** (16-byte `validate_block` already at `aarch64/utf8/validate_block.rs:91`; streaming extension missing) | S-P2 V3 P2-E Gap 4 (`p2e:141-155`); consumer at `parse-that-regex/src/lib.rs:489-505` (NEON UTF-8 block + manual `complete_bytes` carry) collapses to one streaming call. Grammar-neutral by construction — UTF-8 belongs in NO grammar. |
| **PTG-UTF8-STREAMING-SPLIT** | SRC-HOEHRMANN + SRC-SIMDUTF + SRC-REDRESS 50-55 | refuted-as-fused / grounded-as-validate-only | **Consumer in `parse-that-regex/src/unicode/utf8_block.rs`** (file exists empty at `:1-36`; the kernel is `bbnf-simd::aarch64::utf8::validate_block`) | S-P2 V3 P2-E Gap 8 (`p2e:217-229`). REDRESS 50-55 closed sink-local decoded-stats + source-method digest folds; the validate-only width-scan is the inverse material differential — admissible. |
| **PTG-RANGE-CLASS-PRIMITIVE** | SRC-MULA-LEMIRE + Arm ACLE 2026Q1 (`vcgeq_u8`/`vcleq_u8`/`vandq_u8`) | partial (sibling of admitted `_eq_set_64`) | **Vendor in `bbnf-simd::aarch64::byte_class_from_range_64`** (sibling of existing `bbnf-simd/src/lib.rs:282 byte_class_from_eq_set_64`) | S-P2 V3 P2-E Gap 7.5 (`p2e:203-215`); enables S-P3 P3-A C3 `digit_block_simd_accumulate`. Generalizes to UTF-8 continuation `[0x80..=0xbf]` (`lib.rs:914`), CSS hex `[0-9a-fA-F]`, BBNF identifier `[a-zA-Z_]` — grammar-neutral by parameter. |
| **PTG-UNESCAPE-STRING-FRONTLOAD** | SRC-PARSE-THAT-REGEX (`lib.rs:718, 813, 832`) + SRC-SIMDJSON-SRC (generic/stage2/string_parsing.h unescape_string) | refuted-as-SIMD-body / grounded-as-classifier | **Wire consumer in `parse-that-regex/src/lib.rs:718 unescape_string`** + the `unescape_four_unicode_escapes` driver at `:386` extends to `_x8`; kernel in `bbnf-simd::aarch64::unescape_uxxxx` | S-P3 P3-A C4 (`p3a:100-111`) ∪ S-P2 V3 Gap 2 (`p2e:111-126`). Hot leaf is `read_hex_unit_scalar` at `parse-that-regex/src/lib.rs:945` (100% self-time on `y_string_unicode` parse_only per P1-E §2.1) + `unescape_string` direct rank-1 on `unicode_escapes` at 46.7% (P1-E §2.2). |
| **PTG-FLOAT-DIGIT-DOTPROD-16** | SRC-CLINGER + SRC-FASTFLOAT + SRC-FNF + Arm `FEAT_DotProd` (Armv8.4 UDOT) + Intel VNNI (`vpdpwssd`) | partial | **Vendor in `bbnf-simd::aarch64::digit_mac`** (extend existing `parse_4_digits_dotprod` at `:27` to 16-digit; Eisel-Lemire body unchanged) | S-P3 P3-A C3 (`p3a:87-99`) ∪ S-P2 V3 P2-E Gap 5 (`p2e:157-171`). Current 4-digit UDOT wastes 12 of 16 NEON lanes per cycle; float-heavy corpora carry 10-17-digit mantissae (mode-III SIMD ratios 5.04x / 5.01x / 4.96x on mesh / canada / numbers per P1-E §2.4). REDRESS 80 material differential: generic digit-block accumulate, not canada-specific widen. |

(V4-carried rows PTG-DIGEST-SEMANTIC-MIX, PTG-FLOAT-CLINGER-EISEL,
PTG-FLOAT-NO-FALLBACK, PTG-CSS-SCANNER-GAP, PTG-SAME-WAVE-CONSUMER carry
through V5 unchanged with their V4 disposition — see V4 file at git
history for the prior cycle text.)

## Architectural Assertions Defended

### 1. Eisel-Lemire + Clinger fast path is the **only** admissible f64 path

`parse-that-regex/src/number/eisel_lemire/mod.rs:1-177` is a verbatim port
of fast_float2 v0.2.3 (cite at `mod.rs:3` verbatim: `Copied from fast_float2
v0.2.3 (MIT licensed)`). The Clinger fast path is the first short-circuit
at `mod.rs:99 try_fast_path_f64`; the Eisel-Lemire 128-bit multiply is the
slow path at `eisel_lemire/algorithm.rs:14 compute_float`; the ambiguous-rounding
signal `power2 == -1` (`algorithm.rs:12` doc) falls back to
`text.parse::<f64>` at `parse-that-regex/src/number/mod.rs:270`. Primary
literature: SRC-CLINGER (PLDI 1990); SRC-FASTFLOAT (Lemire 2021 SPE). The
disguised-fast-path range extension to e10 ≤ 37 at `mod.rs:117-126` is
from fast_float2 (cite verbatim at `mod.rs:54`: `Disguised fast-path
reach: exponents up to 37 may succeed after pre-scaling the mantissa by
10^(e10-22)`).

### 2. simdjson structural-index two-stage architecture transfers (qualified)

SRC-SIMDJSON-PAPER + SRC-SIMDJSON-SRC ground the Stage-1
`find_structural_bits` shape mirrored at
`bbnf-simd/src/aarch64/classify_tbl4.rs:22 classify_chunk_from_table`
(16-byte `vqtbl4q_u8` multi-class probe); the prefix-XOR backslash carry
mirrored at `bbnf-simd/src/lib.rs:175 escape_mask_64` composed with
`bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:1`. Qualification per Lock 1
substrate-union: bbnf does **not** retain the structural index across
parser calls — index is a transient producer folded into the offset tape
by `compact_mask` at `bbnf-simd/src/lib.rs:227` (per S-P3 V1 §1.1 CH5 line
81 substrate-union YES).

### 3. `unescape_string` is a front-loading classifier, not a SIMD body

S-P3 P3-A V1 §1.2 line 40 maps `parse_that_regex::unescape_string`
(`parse-that-regex/src/lib.rs:718`) to candidate C1; the body at
`lib.rs:719-722` is the no-backslash fast-path classifier
(`Cow::Borrowed`); the hot leaf is `find_next_escape_or_control` at
`lib.rs:813` (8-byte SWAR over `string_escape_control_mask` at `lib.rs:832`)
which is replaceable by PTG-SIMD-SPAN-SCAN-SWEEP-64's 64-byte sweep. The
SIMD body for the `_x8_neon` batch lives in `bbnf-simd`, not in
`parse-that-regex`. The S-P3 V1 §3.3 CH2 cell-(c) bindings at HARDENING
lines 419-423 are consistent if read precisely: the consumer at C4 cell
(c) is the SAME SIMD body exercised by the BBNF-self literal-unescape
consumer at `grammar/bbnf/bbnf.bbnf:11-13` (executable-verified per S-P3
P3-A V1 line 106 grep evidence).

### 4. Hoehrmann UTF-8 DFA is the admissible scalar reference

`parse-that-regex/src/unicode/utf8_hoehrmann.rs:1-87` carries the Hoehrmann
state machine (SRC-HOEHRMANN); the 16-byte SIMD body at
`bbnf-simd/src/aarch64/utf8/validate_block.rs:91` is vectorized per
SRC-SIMDUTF (5.x `src/arm64/arm_validate_utf8.cpp`). The streaming
extension (PTG-STRING-SCAN-UTF8-STREAMING) and width-decode
(PTG-UTF8-STREAMING-SPLIT) do not propose fused-decode routes per REDRESS
50-55 pre-block.

### 5. Mula-Lemire shuffle byte-class generalizes only with explicit range primitive

SRC-MULA-LEMIRE is the source for `byte_class_from_eq_set_64` at
`bbnf-simd/src/lib.rs:282` (≤8-element set via `vqtbl1q_u8`). The inclusive
range form (PTG-RANGE-CLASS-PRIMITIVE) is a **separate** Layer-1 primitive
— per S-P2 V3 Gap 7.5 + S-P3 V1 §3.3 per-candidate same-wave-consumer
tightening. The two-primitive split is the load-bearing grammar-neutral
generalization vehicle for digit-run / UTF-8-continuation / CSS hex /
BBNF identifier classification.

### 6. Compile-time facts and runtime substrates are different objects

V4 carry-forward (re-affirmed): `bbnf-regex` may carry HIR, nullability,
first sets, byte classes, scanner plans, and automata facts into the
resolver. It must not retain masks, class streams, or cursor state across
parser phases. Runtime scanner outputs are `local_temp_only` unless
emitted as an admitted output row.

## Architectural Assertions Refuted

| Assertion | Refutation | Consequence |
|---|---|---|
| "`regex-syntax` HIR is admissible as a runtime dependency for the lazy-DFA fallback." | `docs/parse-that/regex-engine.md:9` verbatim: `There's no dependency on the regex crate at runtime—only regex-syntax for HIR parsing.` Compile-time HIR is build-script-only; runtime DFA fallback must NOT pull `regex-automata` into runtime. | Lock 16 amendment T2F-LOCK-AMEND-001 below: if Q1 resolves toward DFA absorption, the lazy-DFA fallback must be a build-script vendor + runtime trait-object surface, not a runtime crate dep. |
| "The simdjson `prev_in_string` cross-call retained quote-mask is the SIMD ceiling bbnf should target." | Lock 1 substrate-union + REDRESS 96/97/98 closed the retained class-column / streaming structural cursor / class-lane-only routes on M5 Max. | The per-call composed form (PTG-PREV-IN-STRING-LOCK1) caps the ceiling below simdjson's published 1 GB/s; admissible only under the per-call frame. |
| "`parse_that_regex::unescape_string` at `lib.rs:718` is the SIMD body that S-P3 C1 admits." | The body at `lib.rs:719-722` is a fast-path classifier; the hot leaf is `find_next_escape_or_control` at `lib.rs:813` (8-byte SWAR); the SIMD body for the `_x8` batch lives in `bbnf-simd::aarch64::unescape_uxxxx`. | S-P3 V2 P3-A C1 cell wording should clarify "consumer at `lib.rs:718` / SIMD body at `bbnf-simd`" — the cite chain is correct but the SIMD body location is `bbnf-simd`. |
| "Skinny `parse-that-regex` already provides the regex/HIR primitives the SPEC needs." | Live skinny `parse-that-regex` declares only `integration`, `number`, and `unicode` modules + JSON-shaped string/number helpers at `lib.rs:4-8`; the regex/HIR/DFA engine documented at `regex-engine.md:15-25` is NOT in `bbnf-regex` at HEAD (322-line classifier crate). | S-P3 must scope a `bbnf-regex` extraction wave before grammar-authoritative Phase 3 host-function dispatch lands. |
| "Opaque regex strings in BIR are enough for CSP/egraph/cost selection." | `SinkOnlyExpr::RegexProgram` at `crates/codegen/src/lower/sink_only.rs:19-93` stores `pattern: String` only; the upstream parse-that regex-engine.md surface exposes HIR + DFA + byte classes + regex facts. | Cost-model and egraph rows must consume compiled facts; pattern-string is insufficient (V4 carry-forward; LAC-2F-03). |
| "Current float parser is a no-fallback Eisel-Lemire implementation." | `compute_f64` at `eisel_lemire/mod.rs:147` returns `None` on ambiguous rounding; `materialize_f64` at `number/mod.rs:261-272` falls back to `text.parse::<f64>()`. | The next float wave must measure fallback rate (Q3 below) or land a no-fallback algorithm per SRC-FNF. |

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| **Q1.** Can the SK-V14 SPEC W0..W11 plan close R1-R10 acceptance without a runtime DFA dispatch surface in `bbnf-regex`? | Inspect grammar-authoritative migration Phase 3 host-function requirements per `[grammar-authoritative-status]`; if BBNF-self literal-regex matching (`grammar/bbnf/bbnf.bbnf:11-13`) is the only hot consumer, the 322-line classifier suffices. If CSS L4 `<at-rule>` block parsing or BBNF host-fn dispatch needs runtime DFAs, `bbnf-regex` must absorb the `regex-engine.md` pipeline (SK-V14 W11 the natural locus). |
| **Q2.** Where does the `_x8_neon` driver for unescape live — extend `unescape_four_unicode_escapes` at `parse-that-regex/src/lib.rs:386` or add a new `_x8` driver? | S-P3 V2 P3-A C4 row cell (c) must name the explicit consumer function name + line; current row at S-P3 V1 line 103 names `lib.rs:386` for `_x4` only. The two-layer split (`bbnf-simd` kernel / `parse-that-regex` consumer) is clean by Lock 16; the driver placement is ambiguous. |
| **Q3.** What is `materialize_f64`'s fallback rate + self-time on SK-V14 17-corpus census? | The doc-block at `eisel_lemire/mod.rs:91-97` claims "twitter.json 99.8%, canada.json ~85%" Clinger fast-path hit rate — unattributed. Instrument `compute_f64` at `eisel_lemire/mod.rs:147` over the 17-corpus census; the slow-path fraction informs whether PTG-FLOAT-DIGIT-DOTPROD-16 delivers material differential. S-P3 V2 P3-C falsifiability gate the natural locus. |
| **Q4.** Does CSS L4 declaration-values need a runtime regex/scanner engine or do generated scanner templates suffice? | Feed the SK-V14 CSS parity matrix through the bbnf-regex classifier at HEAD; for each declaration-value feature, record whether the heuristic `analyze` at `lib.rs:45` produces sufficient first-set + nullability + byte-class data, or whether a real DFA `find_at` is required. S-P3 R6 post-R4 the natural locus. |
| **Q5.** Can runtime scanner outputs stay purely local after the SK-V14 SPEC waves admit the SIMD gaps? | For each gap (PTG-SIMD-SPAN-SCAN-SWEEP-64, PTG-PREV-IN-STRING-LOCK1, PTG-STRING-SCAN-UTF8-STREAMING, PTG-RANGE-CLASS-PRIMITIVE, PTG-UTF8-STREAMING-SPLIT), declare `substrate_target`, `retention_lifetime`, `policy_owner`; any retained mask/class/cursor stream is REVISE unless separately admitted under Lock 1. V4 admission ledger discipline already binds this. |

## Per-gap dossier (concrete; upstream-or-vendor + bbnf-specific need)

### Gap 1 — PTG-REGEX-HIR-ENGINE: Bespoke NFA→DFA pipeline (compile-time + runtime)

- **Published primitive.** Thompson 1968 NFA construction (CACM 11:6);
  Hopcroft 1971 minimization (Stanford CS); Cox 2007 RE2 design
  (SRC-COX-REGEX); Lemire-Mula shuffle byte-class for `accel.rs` 4-8
  exit-byte case (SRC-MULA-LEMIRE). The full pipeline documented at
  SRC-PARSE-THAT-DOCS `regex-engine.md:15-25`.
- **Upstream-or-vendor decision.** **Upstream into `bbnf-regex` crate**.
  bbnf-regex at HEAD is bbnf-authored (322 lines, no external deps); the
  regex-engine.md pipeline is the design for the absorbing implementation.
  Per `[directory-module-structure]` + `[regex-generalized]` memory
  feedback, the engine lives in `bbnf-regex`, not bbnf-lang or
  parse-that-regex.
- **bbnf-specific need.** SK-V14 SPEC waves W0..W11 do not currently admit
  a runtime regex engine; grammar-authoritative Phase 3 host functions
  may. The classifier at `bbnf-regex/src/lib.rs:46-104` (`analyze` +
  `classify` + `regex_is_nullable` + `first_set`) is sufficient for
  compile-time first-set computation but lacks runtime `find_at`.
- **Same-wave consumer.** None at SK-V14 (Q1 above).

### Gap 2 — PTG-REGEX-LAZY-DFA-FALLBACK: Lazy-DFA for state-limit-exceeding patterns

- **Published primitive.** regex-automata `meta::Regex` lazy-DFA
  construction (SRC-RUST-REGEX); Cox 2007 (SRC-COX-REGEX) on-demand
  DFA-state materialization to bound space at the state-limit guard
  (default 512 per `regex-engine.md:62`).
- **Upstream-or-vendor decision.** **Build-script vendor into `bbnf-regex`**
  (compile-time only; never runtime per `regex-engine.md:9`); the runtime
  fallback path is a separate trait-object surface that does not pull
  `regex-automata` into the runtime crate graph.
- **bbnf-specific need.** Required only if the runtime DFA dispatch
  surface (Gap 1) lands. Covers the case where the bespoke DFA returns
  `None` per `regex-engine.md:62`.
- **Same-wave consumer.** None at SK-V14.

### Gap 3 — PTG-SIMD-SPAN-SCAN-SWEEP-64: `scan_string_special_block_sweep_64`

- **Published primitive.** SRC-SIMDJSON-PAPER (Langdale-Lemire 2019)
  Stage-1 `find_structural_bits` 64-byte sweep; SRC-SIMDJSON-SRC
  `include/simdjson/arm64/stage1.h`; SRC-MULA-LEMIRE shuffle byte-class
  primitive substrate.
- **Upstream-or-vendor decision.** **Vendor in
  `bbnf-simd::aarch64::string_block`**. The 16-byte body at
  `bbnf-simd/src/aarch64/string_block.rs:57 scan_string_special_block`
  is Wave-1-admitted; the 64-byte sweep wrapper composes 4 calls +
  OR-folds the masks. Layer-0 substrate: `vld1q_u8` × 4 + `vceqq_u8` /
  `vcltq_u8` / `vcgeq_u8` + existing Layer-1 `movemask_u8x16`
  (`aarch64/movemask.rs:4`).
- **bbnf-specific need.** S-P3 P3-A V1 candidate C1
  (`p3a-candidate-shortlist.md:64`) — SK-V14 SPEC W2
  `long_string_body_simd_scan` canonical name per S-P2 V3 §6.2 NF-CH6-4
  binding. Required for `unicode_escapes` / `twitter` / `gsoc-2018` /
  `mesh` / `github_events` direct-plane row movement (falsifiability gate
  at C1 row `p3a:69`).
- **Same-wave consumer.** `parse_that_regex::skip_string_plain_trusted`
  at `parse-that-regex/src/lib.rs:547` + long-string body inside the
  `parse_object_value_at_direct` envelope at
  `runtime/src/grammars/json/generated.rs:466`. F-V2-P1ABC-RERECORD
  Stage-0 dependency: YES.

### Gap 4 — PTG-PREV-IN-STRING-LOCK1: `scan_string_with_carry_64`

- **Published primitive.** SRC-SIMDJSON-SRC
  `include/simdjson/arm64/simd.h find_quote_mask_and_bits`;
  SRC-SIMDJSON-PAPER §4 prefix-XOR backslash-carry shape;
  SRC-MULA-LEMIRE shuffle byte-class composition.
- **Upstream-or-vendor decision.** **Vendor in
  `bbnf-simd::aarch64::string_block` (per-call composed form)**. NOT a
  `prev_in_string` parameter; the carry stays inside one 64-byte call
  (composes `classify_tbl4::classify_block_from_table` +
  `bitmap_prefix_xor_64` + `escape_mask_64`). Pure Layer-1; no Layer-0
  substrate add.
- **bbnf-specific need.** S-P2 V3 P2-E Gap 6 (`p2e:173-187`); consumer at
  `runtime/src/grammars/json/scan.rs resolve_string_masks_64`
  (P1-E §1.2) collapses three steps to one Layer-1 call.
- **Refutation note.** simdjson's cross-call `prev_in_string` retention
  is inadmissible under Lock 1 (REDRESS 96/97/98); the per-call form
  caps the ceiling below simdjson's 1 GB/s.
- **Same-wave consumer.** `runtime/src/grammars/json/scan.rs` +
  `parse_that_regex::skip_string_plain_trusted` envelope.

### Gap 5 — PTG-STRING-SCAN-UTF8-STREAMING: `utf8::validate_block_streaming`

- **Published primitive.** SRC-HOEHRMANN (DFA with 9 states + 12
  character classes); SRC-SIMDUTF 5.x `src/arm64/arm_validate_utf8.cpp`
  (vectorized via `vqtbl1q_u8` over 16-entry leading-byte class table);
  Keiser-Lemire 2020 "Validating UTF-8 in Less Than One Instruction Per
  Byte" SPE pre-print.
- **Upstream-or-vendor decision.** **Vendor in
  `bbnf-simd::aarch64::utf8::validate_block_streaming`**. The 16-byte
  `validate_block` at `aarch64/utf8/validate_block.rs:91` exists; the
  streaming extension carries `ValidateStatus::continues +
  complete_bytes` state across chunks. Scalar Hoehrmann reference at
  `parse-that-regex/src/unicode/utf8_hoehrmann.rs:1-87` (PRESENT).
- **bbnf-specific need.** S-P2 V3 P2-E Gap 4 (`p2e:141-155`); consumer
  at `parse-that-regex/src/lib.rs:489-505` (NEON UTF-8 block + manual
  `complete_bytes` carry) collapses to one streaming call. UTF-8 is
  grammar-neutral (Lock 14).
- **Same-wave consumer.** `parse-that-regex/src/lib.rs:489-505` +
  `validate_utf8_codepoint` at `:843` collapse to one driver. CSS L4
  declaration-value strings + Sheets text + BBNF-self comment text are
  cross-grammar consumers.

### Gap 6 — PTG-RANGE-CLASS-PRIMITIVE: `byte_class_from_range_64`

- **Published primitive.** SRC-MULA-LEMIRE shuffle byte-class
  classification; Arm ACLE 2026Q1 `vcgeq_u8`/`vcleq_u8`/`vandq_u8`
  range-test composition.
- **Upstream-or-vendor decision.** **Vendor in
  `bbnf-simd::aarch64::byte_class_from_range_64`** as a sibling of the
  existing `byte_class_from_eq_set_64` at `bbnf-simd/src/lib.rs:282`.
  Pure Layer-1 (Layer-0 substrate already present: `vcgeq_u8` × 8 +
  `vandq_u8` × 4 + `movemask_u8x16`).
- **bbnf-specific need.** S-P2 V3 P2-E Gap 7.5 (`p2e:203-215`); required
  by S-P3 P3-A C3 `digit_block_simd_accumulate`. Generalizes to UTF-8
  continuation `[0x80..=0xbf]` per `is_utf8_continuation`
  (`parse-that-regex/src/lib.rs:914`), CSS L4 hex-digit `[0-9a-fA-F]`
  (two-range OR-fold), BBNF identifier `[a-zA-Z_]` — grammar-neutral by
  parameter.
- **Same-wave consumer.** S-P3 P3-A C3 cell (c) at `p3a:93` —
  `parse-that-regex` digit-run scan replacing the 8/4/2/1 SWAR ladder at
  `parse-that-regex/src/number/mod.rs:106` + bbnf-simd checkasm row
  `checkasm_byte_class_from_range_64.rs` as the non-JSON same-wave
  exercise.

### Gap 7 — PTG-UNESCAPE-STRING-FRONTLOAD: `unescape_string` front-load + `_x8_neon` batched decode

- **Published primitive.** parse-that-regex `_x4_neon` shape at
  `bbnf-simd/src/aarch64/unescape_uxxxx.rs:125 unescape_uxxxx_x4_neon`
  (admitted Wave-1); SRC-SIMDJSON-SRC
  `include/simdjson/generic/stage2/string_parsing.h unescape_string`;
  the fast-path classifier idiom of "scan for backslash first, decode
  batch second" is a parse-that-regex idiom rather than a single
  published primitive.
- **Upstream-or-vendor decision.** **Vendor kernel `_x8_neon` in
  `bbnf-simd::aarch64::unescape_uxxxx`** + **wire consumer in
  `parse-that-regex/src/lib.rs:718 unescape_string`** (extend
  `unescape_four_unicode_escapes` driver at `lib.rs:386` to `_x8`
  first-pass). Front-load (Gap 3) consumed at
  `find_next_escape_or_control` at `lib.rs:813` — replace 8-byte SWAR
  loop with 64-byte sweep call.
- **bbnf-specific need.** S-P3 P3-A C4 (`p3a:100-111`) maps to S-P2 V3
  P2-E Gap 2 (`p2e:111-126`). Hot leaf is `read_hex_unit_scalar` at
  `parse-that-regex/src/lib.rs:945` (100% self-time on `y_string_unicode`
  parse_only per P1-E §2.1) + `unescape_string` direct rank-1 on
  `unicode_escapes` at 46.7% (P1-E §2.2).
- **Refutation note.** `unescape_string` at `lib.rs:718` is **not** a
  SIMD body — it is a fast-path classifier (no-backslash short-circuit
  at `:719-722`) + scalar canonicalization driver. The SIMD body lives
  in `bbnf-simd`.
- **Same-wave consumer.** Per S-P3 P3-A V1 line 106: BBNF-self
  literal-unescape consumer at `grammar/bbnf/bbnf.bbnf:11-13`
  (executable-verified per `grep -n "literal = " grammar/bbnf/bbnf.bbnf`
  line 11 + `grep -n "fn unescape_string" parse-that-regex/src/lib.rs`
  line 718). CSS L4 escaped-ident `\HEXHEX` is shape-orthogonal —
  carved out per Lock 14 v+1 "measured rejection".

### Gap 8 — PTG-UTF8-STREAMING-SPLIT: `utf8_codepoint_scan_64` (width-only)

- **Published primitive.** SRC-HOEHRMANN UTF-8 DFA augmented with
  `vqtbl1q_u8` over a 16-entry leading-byte class table for width-class
  extraction (cite: SRC-SIMDUTF 5.x `src/arm64/arm_validate_utf8.cpp`
  width-class shape).
- **Upstream-or-vendor decision.** **Consumer in
  `parse-that-regex/src/unicode/utf8_block.rs`** (file exists empty at
  `:1-36`); **kernel via `bbnf-simd::aarch64::utf8::validate_block`** at
  `bbnf-simd/src/aarch64/utf8/validate_block.rs:91` (already present,
  takes 16-byte chunk). The width-array extraction over 64-byte horizon
  is the new code.
- **bbnf-specific need.** S-P2 V3 P2-E Gap 8 (`p2e:217-229`). Required
  by `unescape_string` cursor advance bookkeeping + CSS L4
  declaration-value strings + Sheets text + BBNF-self comment text. Per
  REDRESS 50-55 material differential: validate-only width-scan is
  **NOT** a fused decode-into-sink route — the pre-block stands.
- **Same-wave consumer.** `parse-that-regex/src/lib.rs:602-627
  validate_utf8_prefix` + `:843-911 validate_utf8_codepoint` collapse
  to one driver + the new Layer-1 primitive.

### Gap 9 — PTG-FLOAT-DIGIT-DOTPROD-16: `parse_16_digits_dotprod`

- **Published primitive.** SRC-CLINGER + SRC-FASTFLOAT
  (`parse-that-regex/src/number/eisel_lemire/mod.rs:1-177` is the
  fast_float2 v0.2.3 copy); SRC-FNF (Mushtak-Lemire 2022 no-fallback).
  Digit-pack idiom from fast_float2 + Arm v8.4 DotProd UDOT (`FEAT_DotProd`
  per Arm Architecture Reference Manual Issue J.a) + Intel VNNI
  `vpdpwssd` on x86 AVX-512.
- **Upstream-or-vendor decision.** **Vendor in
  `bbnf-simd::aarch64::digit_mac`** extending existing
  `parse_4_digits_dotprod` at `bbnf-simd/src/aarch64/digit_mac.rs:27` to
  16-digit. **No float-algorithm change**; the SIMD wide-mantissa
  accumulator is upstream of the 128-bit multiply that
  `eisel_lemire/algorithm.rs:14 compute_float` already implements.
- **bbnf-specific need.** S-P3 P3-A V1 C3 (`p3a:87-99`); S-P2 V3 P2-E
  Gap 5 (`p2e:157-171`). Current 4-digit UDOT wastes 12 of 16 NEON
  lanes per cycle; float-heavy corpora carry 10-17-digit mantissae
  (mode-III SIMD ratios 5.04x / 5.01x / 4.96x on mesh / canada /
  numbers per P1-E §2.4). REDRESS 80 material differential: generic
  digit-block accumulate (Lock 16 abstract-primitive declaration per
  S-P3 P3-A C3 row at `p3a:97`), not canada-specific f64 widening.
- **Same-wave consumer.** S-P3 P3-A C3 cell (c) at `p3a:93` —
  direct-plane number kernel in `parse_array_element_at_direct`
  (`generated.rs:506`) on canada / mesh / marine_ik / numbers;
  typed-plane `parse_vec_cap_10800_scalar_f64` on mesh-typed Track 1;
  bbnf-simd checkasm row exercising the CSS L4 `<number>` byte-class
  config as the non-JSON exercise (Lock 14 v+1 discharge).
- **F-V2-P1ABC-RERECORD Stage-0 dependency.** YES.

## V5 Admission Ledger (per gap; per V4 + V3 fold addendum format)

Every V5 candidate that reaches S-P3 carries the shared V2/V3/V4 ledger columns
plus the V5 cross-binding to the S-P3 P3-A V1 LOCKED shortlist row.

| candidate_id | s-p3-row | scalar_ref | checkasm | same-wave consumer | substrate_target / lifetime / owner | F-V2-RERECORD dep | state |
|---|---|---|---|---|---|---|---|
| `bbnf_regex_hir_engine` | (none — not in P3-A V1 8/8 shortlist) | (Hoehrmann-style scalar at `regex-engine.md:28-44`) | (Wired only on absorption: equivalence to `regex-automata::meta::Regex::find` over byte stream) | (Q1 above; deferred until W11) | `local_temp_only` (compile-time facts) / `generated_function` / `generated_grammar` | NO | source_backed; blocker = absorption decision |
| `regex_lazy_dfa_fallback` | (none) | (regex-automata::Regex compile-time scalar ref) | (state-limit-exceeding patterns; Cox 2007 algorithm) | (Q1 above) | `local_temp_only` (build-script only) / N/A / N/A | NO | source_backed; blocker = absorption decision |
| `scan_string_special_block_sweep_64` | C1 (`p3a:61-73`) | `bbnf-simd/src/aarch64/string_block.rs:31 scan_string_special_block_scalar` (PRESENT) | EXTENSION at `bbnf-simd/tests/checkasm_scan_string_special_block_sweep_64.rs` | `parse-that-regex/src/lib.rs:547` + `runtime/src/grammars/json/generated.rs:466` | `local_temp_only` / `local_loop` / `generated_grammar` | **YES** (Gap 1 in 12-dep list per §6.3) | scalar_backed |
| `scan_string_with_carry_64` | (S-P2 V3 Gap 6; absorbed into C1 canonical-name per §6.2) | `bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:1` + scalar `escape_mask_64` body (PRESENT) | EXTENSION sibling-shape | `runtime/src/grammars/json/scan.rs resolve_string_masks_64` | `local_temp_only` / `local_loop` / `generated_grammar` | YES | scalar_backed |
| `utf8_validate_block_streaming` | (S-P2 V3 Gap 4; not in P3-A V1 8/8) | `parse-that-regex/src/unicode/utf8_hoehrmann.rs:1-87 validate_block` (PRESENT) | EXTENSION at `bbnf-simd/tests/` for streaming continuation-state carry | `parse-that-regex/src/lib.rs:489-505` | `local_temp_only` / `local_loop` / `generated_grammar` | YES (Gap 4 in 12-dep list per §6.3) | scalar_backed |
| `byte_class_from_range_64` | C3 (sibling per `p3a:93` non-JSON exercise row) | `bbnf-simd/src/scalar/byte_class_from_range_64.rs` (sibling-shape template PRESENT at HEAD via `byte_class_from_eq_set_64.rs:1` per §2.10 of P2-E V3) | EXTENSION sibling at `bbnf-simd/tests/checkasm_byte_class_from_range_64.rs` | S-P3 P3-A C3 cell (c) — `parse-that-regex/src/number/scan_digit_run_simd.rs` (new) | `local_temp_only` / `local_loop` / `generated_grammar` | YES (paired with Gap 5) | scalar_backed |
| `unescape_uxxxx_x8_neon` | C4 (`p3a:100-111`) | `bbnf-simd/src/aarch64/unescape_uxxxx.rs:40 unescape_uxxxx_scalar` (PRESENT) | EXTENSION at `bbnf-simd/tests/checkasm_unescape_uxxxx_x8.rs` | `parse-that-regex/src/lib.rs:718` `unescape_string` + `lib.rs:386` driver + BBNF-self literal-unescape per S-P3 V1 line 106 | `local_temp_only` / `output_row` / `generated_grammar` | NO (cleanly attributed in P1-E §2.1-§2.2) | scalar_backed |
| `utf8_codepoint_scan_64` | (S-P2 V3 Gap 8; not in P3-A V1 8/8) | `parse-that-regex/src/unicode/utf8_hoehrmann.rs:1-87` (PRESENT) | EXTENSION at `bbnf-simd/tests/checkasm_utf8_width_scan_64.rs` | `parse-that-regex/src/unicode/utf8_block.rs:1-36` (file exists empty) | `local_temp_only` / `local_loop` / `generated_grammar` | NO | source_backed |
| `parse_16_digits_dotprod` | C3 (`p3a:87-99`) | `parse-that-regex/src/number/mod.rs:214 parse_eight_digits` × 2 + `* 10_000_000_000` between (PRESENT) | EXTENSION at `bbnf-simd/tests/checkasm_digit_mac_x16.rs` | `parse-that-regex/src/number/mod.rs:106` ladder → 16-byte UDOT first-pass; `parse_array_element_at_direct` direct-plane consumer | `direct_sink` (mantissa emit) + `local_temp_only` (per-chunk accumulator) / `local_loop` / `generated_grammar` | **YES** (P2-C C-P2C-3 + P2-E Gap 5 both in 12-dep list per §6.3) | scalar_backed |

## LOCKS-AMENDMENTS-CANDIDATE

| ID | Lock | Amendment | Trigger |
|---|---|---|---|
| **LAC-2F-V5-01** | Lock 16 (SIMD/ASM allowlist + abstract-primitive declarations) | Add `bbnf-regex::Dfa` admissibility row IF Q1 resolves toward absorbing the `regex-engine.md` pipeline. Admissibility requires: (a) scalar reference (Hoehrmann/Thompson straightforward construction at `regex-engine.md:28-44`); (b) checkasm-parity equivalence to `regex-automata::meta::Regex::find` over byte stream; (c) same-wave consumer (any host-fn or leaf-parser dispatch site). | Q1 resolution + SK-V14 W11 wave admission. |
| **LAC-2F-V5-02** | Lock 1 (substrate-union v+1 manifest) | Add explicit `prev_in_string`-as-substrate refutation row: simdjson's cross-call retained-quote-mask design (`include/simdjson/arm64/simd.h find_quote_mask_and_bits`) is **inadmissible** under Lock 1 substrate-union; the per-call composed form (Gap PTG-PREV-IN-STRING-LOCK1) is the admissible primitive. Refutation is load-bearing because it caps the per-call SIMD ceiling below simdjson's published 1 GB/s. | REDRESS 96/97/98 + this refutation. |
| **LAC-2F-V5-03** | Lock 14 (grammar-neutrality v+1) | Pin `byte_class_from_range_64` (PTG-RANGE-CLASS-PRIMITIVE) as a sibling of `byte_class_from_eq_set_64` in the abstract-primitive declaration list; the two-primitive split (set ≤8 vs inclusive range) is the load-bearing grammar-neutral generalization vehicle for digit-run / UTF-8-continuation / CSS hex / BBNF identifier classification. Per `[regex-generalized]` memory feedback, the range primitive lives in `bbnf-simd` (not bbnf-lang). | S-P3 P3-A C3 admission. |
| **LAC-2F-V5-04** | Lock 10 (decision engine cost model) | Regex/HIR facts are mandatory inputs to CSP/egraph/cost selection; opaque pattern strings (`SinkOnlyExpr::RegexProgram { pattern: String }` at `crates/codegen/src/lower/sink_only.rs:19-93`) are insufficient for backend-shape or scanner selection. **Carry-forward from V4 LAC-2F-03**. | bbnf-regex absorption / Q1 resolution. |

## Sources (full citation; executable-verified at HEAD `069ba203c`)

SRC-PARSE-THAT-REGEX — `skinny/crates/parse-that-regex/src/lib.rs:1-1214`
+ `src/number/mod.rs:1-280` + `src/number/eisel_lemire/{mod.rs:1-177,
algorithm.rs:1-94, table.rs:1-660}` + `src/unicode/{mod.rs:1-4,
utf8_block.rs:1-36, utf8_hoehrmann.rs:1-87}` +
`src/integration/{mod.rs, simd_scan_hook.rs:1-19}` +
`Cargo.toml:1-10` (deps: `bbnf-simd.workspace=true`,
`thiserror.workspace=true`).

SRC-PARSE-THAT-DOCS — `docs/parse-that/{regex-engine.md:1-135,
combinators.md, leaf-parsers.md, overview.md, span-combinators.md}`.

SRC-BBNF-REGEX — `skinny/crates/bbnf-regex/src/lib.rs:1-322` (322 lines
including tests; no external deps in imports per executable-verified
`grep -n "^use" skinny/crates/bbnf-regex/src/lib.rs` returning nothing).

SRC-BBNF-SIMD — `skinny/crates/bbnf-simd/src/lib.rs` (Layer-1 surface),
`aarch64/` (NEON kernels: `string_block.rs:31, 57`,
`unescape_uxxxx.rs:40, 74, 125`, `digit_mac.rs:5, 27`,
`utf8/validate_block.rs:91`, `byte_class_from_eq_set_64.rs:1`,
`movemask.rs:4`, `classify_tbl4.rs:8, 22, 47, 75, 89`),
`scalar/` (8-file scalar-reference siblings).

SRC-CLINGER — Clinger, W.D. "How to Read Floating-Point Numbers
Accurately". PLDI 1990, pp. 92-101. DOI 10.1145/93542.93557.

SRC-FASTFLOAT — Lemire, D. "Number Parsing at a Gigabyte per Second".
Software: Practice & Experience 51:8 (Aug 2021), pp. 1700-1727. DOI
10.1002/spe.2984. Cited verbatim at
`parse-that-regex/src/number/eisel_lemire/mod.rs:1, 3, 17, 26, 54`.

SRC-FNF — Mushtak, N. and Lemire, D. "Fast Number Parsing Without
Fallback". arXiv:2212.06644 (Dec 2022).

SRC-SIMDJSON-PAPER — Langdale, G. and Lemire, D. "Parsing Gigabytes of
JSON per Second". VLDB Journal 28:6 (Nov 2019), pp. 941-960.
arXiv:1902.08318. DOI 10.1007/s00778-019-00578-5.

SRC-SIMDJSON-SRC — simdjson at HEAD
`168ef580757d75270475b379e83c2b39787a6765` (pinned per S-P2 V3 §5.3):
`include/simdjson/arm64/stage1.h find_structural_bits`;
`include/simdjson/arm64/simd.h find_quote_mask_and_bits`;
`include/simdjson/generic/stage2/string_parsing.h unescape_string`.

SRC-SIMDUTF — simdutf 5.x at github.com/simdutf/simdutf:
`src/arm64/arm_validate_utf8.cpp`, `src/scalar/utf8.h`.

SRC-HOEHRMANN — Höhrmann, B. "Flexible and Economical UTF-8 Decoder".
bjoern.hoehrmann.de/utf-8/decoder/dfa/ (2009). Reference impl at
`parse-that-regex/src/unicode/utf8_hoehrmann.rs:1-87`.

SRC-MULA-LEMIRE — Muła, W. and Lemire, D. "Faster shuffle-based
byte-class classification" (2019); Muła-Lemire 2018 PDEP/PEXT studies.

SRC-COX-REGEX — Cox, R. "Regular Expression Matching Can Be Simple And
Fast" (2007). swtch.com/~rsc/regexp/regexp1.html.

SRC-RE2 — Google RE2 at HEAD `972a15cedd008d846f1a39b2e88ce48d7f166cbd`.
github.com/google/re2.

SRC-RUST-REGEX — Rust `regex` + `regex-automata` + `regex-syntax` at HEAD
`839d16bc65b60e2006d3599d20bfa6efc14049d8`. github.com/rust-lang/regex.

SRC-MEMCHR — BurntSushi `memchr` at HEAD
`db1a77d4b556a1321e136ca0514e43e74ea5fcc3`. github.com/BurntSushi/memchr.

SRC-XXHASH — xxHash at HEAD
`e573d4d2aaeaba0f3e5a0a9a54144a1f2b4b56e7`. github.com/Cyan4973/xxHash.

SRC-RFC3629 — Yergeau, F. "UTF-8, a transformation format of ISO 10646".
RFC 3629 (Nov 2003). tools.ietf.org/html/rfc3629.

SRC-RFC8259 — Bray, T. "The JavaScript Object Notation (JSON) Data
Interchange Format". RFC 8259 (Dec 2017). tools.ietf.org/html/rfc8259.

SRC-S-P2-V3-CONSOL —
`restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md:1-668`
(V3 §3Z LOCK declaration; §6.1 CF-3; §6.2 NF-CH6-4 canonical-name
binding; §6.3 F-V2-P1ABC-RERECORD Stage-0 commitment).

SRC-S-P3-A-V1 —
`restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md:1-316`
(8-candidate shortlist; C3 line 87-99; C4 line 100-111 — C3+C4 cell (c)
same-wave-consumer rows).

SRC-REDRESS — `skinny/REDRESS.md:517-557, 633-649, 700-713, 846-882,
2910-2940, 3495-3528, 3603-3633, 3780-3805` (V4 carry-forward;
REDRESS 80 / 88 / 89 / 96 / 97 / 98 per V3 §1.4 of P2-E).

SRC-T-P1 — `restart/audit/totality/p1/{1A,1B,1C,1D,1E,1F-coherence-scan,
1F-anti-pattern,1F-past-corpora}.md` (V5 LOCKED) +
`restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md`.
