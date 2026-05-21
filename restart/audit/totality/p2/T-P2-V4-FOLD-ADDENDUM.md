# T-P2 V4 Fold Addendum

Pass: T-P2 Research.
Cycle: V4.
Date: 2026-05-21.
Scope: fold of V3 CH1 provenance mapping and CH4 executable-ledger revise set.
Output: this file.

## V4 Fold Authority

This addendum supplements `T-P2-V2-FOLD-ADDENDUM.md` and
`T-P2-V3-FOLD-ADDENDUM.md`. It does not weaken their Lock 14, Lock 1,
REDRESS, material-differential, anti-paper-close, state-machine, or numeric
abrogate contracts. It resolves the V3 revise set by adding:

- a corrected 2E counted-source convention whose IDs match the 2E source
  registry;
- a V4 executable ledger delta for rows whose V3 cells were still deferred;
- explicit non-shortlist blockers for source-backed rows that lack commands or
  first consumers today; and
- a REDRESS-slice ownership table mapping reopened route families back to
  candidate owners.

## 2E Counted-Source Repair

2E's counted evidence is now exactly the set of IDs defined in its local Source
Registry. The inherited FFmpeg and dav1d source pins remain available through
`SRC-V2-FOLD`; they are not separately counted as `SRC-FFMPEG` or `SRC-DAV1D`
rows in 2E.

| dossier | required count | counted ids |
|---|---:|---|
| 2E | 11 | `SRC-A64-ACLE`, `SRC-A64-NEON`, `SRC-A64-SVE2-MATCH`, `SRC-X86-INTEL`, `SRC-SCOPE`, `SRC-REDRESS`, `SRC-BBNF-A64`, `SRC-BBNF-DISPATCH`, `SRC-BBNF-CHECKASM`, `SRC-BBNF-X86`, `SRC-V2-FOLD` |

`SRC-REDRESS` and `SRC-BBNF-A64` are counted primary local evidence because
2E uses prior measured dispositions and local primitive bodies as direct
admissibility constraints. FFmpeg, dav1d, simdjson, sonic-rs, yyjson, egg,
OR-Tools, RE2, Rust regex, fast_float, Sneller, and parse-that remain inherited
support under `SRC-V2-FOLD` unless a dossier registers and counts them locally.

## V4 Executable Ledger Delta

The V3 executable ledger remains authoritative except for the rows below,
where V4 replaces deferred cells with exact commands / first consumers or marks
the candidate non-shortlist until the missing executable cell exists.

| candidate_id | V4 checkasm_or_parity_command | V4 first_consumer_path | V4 disposition_or_blocker |
|---|---|---|---|
| `escape_mask_64` | `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --test checkasm_escape_mask_64 -- --nocapture` | `skinny/crates/runtime/src/grammars/json/generated.rs` string escape scan path, or `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs` escaped identifier/string scan path once generated | Non-shortlist until one listed consumer is wired in the same wave; prerequisite-only checkasm cannot admit. |
| `tbl_tbx_escape_decode_batch` | Non-shortlist: no strict JSON+CSS variable-width escape parity command exists at V4. Required future command shape is `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --test checkasm_escape_decode_batch -- --nocapture` after the test is created. | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs` escaped identifier/string materializer, or JSON unicode materializer only after CSS policy split exists | Non-shortlist until the strict parity test exists and covers JSON fixed-width plus CSS 1-6 digit escapes, invalid tails, surrogate policy, and grammar-specific rejection. |
| `digit_run_accumulate_udot` | Non-shortlist: no strict DOTPROD digit materializer parity command exists at V4. Required future command shape is `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --test checkasm_digit_mac -- --nocapture` after the test is created. | `skinny/crates/runtime/src/grammars/json/generated.rs` number materializer or `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs` number/dimension materializer | Non-shortlist until scalar policy, overflow/invalid-lane parity, and first consumer are present. |
| `pmull_cssc_structural_union_emit64` | `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --test checkasm_bitmap_prefix_xor_64 -- --nocapture` plus `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --test checkasm_bitmap_next_set_bit -- --nocapture`; a combined density/order matrix is still required before admission | No V4 first consumer. Future candidates must name the exact JSON or CSS structural consumer that deletes or bypasses the old scalar cost source. | Non-shortlist until combined strict matrix and first consumer exist; PMULL/CSSC/union category remains research-eligible but not S-P3-row-executable from this packet alone. |
| `string_context_64` | Non-shortlist: no strict string-context parity command exists at V4. Required future command shape is `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --test checkasm_string_context_64 -- --nocapture` after the test is created. | `skinny/crates/runtime/src/grammars/json/generated.rs` string scanner or CSS escaped identifier/string scanner | Non-shortlist until grammar policy, cross-chunk/tail parity, and first consumer exist. |
| `cache_hint_prefetch_store` | Non-shortlist: no semantic parity command applies because the primitive is placement-only. Any future use must carry an equality/perf harness for the exact caller. | No V4 first consumer. Candidate must either be deleted or name an exact generated hot caller before S-P3. | Non-shortlist until exact caller placement exists; support-only hint modules do not close Lock 16. |

Rows not listed above retain their V3 ledger values. A later S-P3 wave may
select a non-shortlist row only by first adding the missing command and first
consumer in its plan, passing CHALLENGE, and keeping the same-wave redress gate.

## REDRESS-Slice Ownership Table

This table bridges the V2 REDRESS matrices and the V3/V4 candidate ledger. A
slice may reach S-P3 only if its `blocker` is cleared in the wave plan; otherwise
it remains research evidence, not a dispatchable intervention.

| redress_slice_id | prior REDRESS row(s) | candidate_id | owner | first_consumer_path | expected_row_gate | loc_budget | rollback_path | abrogate_threshold | blocker |
|---|---|---|---|---|---|---:|---|---|---|
| `RS-JSON-DIRECT-twitter` | REDRESS-119 | `semantic_digest_simd_mix` | 2F | `skinny/crates/runtime/src/grammars/json/generated.rs` direct sink / `JsonDirectDigest` caller | `twitter/direct_to_struct` strict sonic row movement with prior A/GO guards held | 80-180 | disable SIMD digest fold and keep scalar mix | semantic digest mismatch or admitted-row regression | Byte-hash substitution remains blocked until scalar oracle + SIMD parity exist. |
| `RS-JSON-DIRECT-canada` | REDRESS-119 | `digit_run_accumulate_udot` | 2E/2F | JSON number materializer in generated runtime | `canada/direct_to_struct` strict sonic row movement | 120-240 | disable DOTPROD branch and keep scalar/SWAR path | no numeric hot leaf after fresh profile, parity fail, or row miss | Non-shortlist: DOTPROD checkasm and first consumer absent. |
| `RS-JSON-DIRECT-github_events` | REDRESS-119 | `semantic_digest_simd_mix` | 2F | `JsonDirectDigest` caller | `github_events/direct_to_struct` strict sonic row movement | 80-180 | disable SIMD digest fold and keep scalar mix | digest mismatch or admitted-row regression | Needs scalar oracle + SIMD parity before shortlist. |
| `RS-JSON-DIRECT-update_center` | REDRESS-119 | `semantic_digest_simd_mix` | 2F | `JsonDirectDigest` caller | `update_center/direct_to_struct` strict sonic row movement | 80-180 | disable SIMD digest fold and keep scalar mix | digest mismatch or admitted-row regression | Needs scalar oracle + SIMD parity before shortlist. |
| `RS-JSON-DIRECT-string-unicode` | REDRESS-119, REDRESS-122 | `escape_mask_64` / `string_context_64` / `tbl_tbx_escape_decode_batch` | 2B/2E/2F | JSON string/unicode materializer or CSS escaped identifier/string materializer | unicode direct/parse/typed row movement or CSS escape parity row | 20-320 | keep scalar scanner/decode path | parity fail, grammar-policy leak, or row miss | Only `escape_mask_64` has current checkasm; decode/context rows are non-shortlist. |
| `RS-JSON-DIRECT-numbers` | REDRESS-119 | `digit_run_accumulate_udot` | 2E/2F | JSON number materializer | `numbers/direct_to_struct` strict sonic row movement | 120-240 | disable DOTPROD branch and keep scalar/SWAR path | parity fail, overflow mismatch, or row miss | Non-shortlist until strict DOTPROD parity exists. |
| `RS-UNION-PMULL-CSSC` | REDRESS 88, 89, 96, 97, 98 | `pmull_cssc_structural_union_emit64` | 2B/2E | none at V4; must be named by future union wave | JSON or CSS structural row movement with guard floors held | 280-520 | remove PMULL/CSSC body and keep scalar scan | replay of prior regression, no consumer, parity fail, or guard regression | Non-shortlist until combined matrix and exact first consumer exist. |
| `RS-SOURCE-PRESENT-prefix-next-bulk` | REDRESS 88, 89, 126 | `pmull_cssc_structural_union_emit64` | 2E | none at V4 | wire/delete/scalar-delegate disposition for prefix-xor, next-bit, bulk emit | 280-520 | retain scalar delegate or remove feature branch | any guard regression or orphan at close | Candidate only if a row-local PMULL/CSSC consumer clears `RS-UNION-PMULL-CSSC`. |
| `RS-SOURCE-PRESENT-byte-context` | REDRESS-126 | `string_context_64` | 2B/2E/2F | JSON/CSS string scanner | strict string/unicode or CSS escaped row movement | 160-300 | remove wide context path and keep current scanner | no grammar policy, parity fail, or row miss | Non-shortlist until strict string-context parity and first consumer exist. |
| `RS-SOURCE-PRESENT-cache-hints` | REDRESS-126 | `cache_hint_prefetch_store` | 2B/2E | none at V4 | strict no-regression plus row movement on exact caller row | 40-100 | delete hint call and module if unconsumed | no caller or any regression | Non-shortlist; delete-or-wire decision belongs to S-P3 only with an exact caller. |
| `RS-CSS-ASCII-RUN-SKIP` | REDRESS-126, REDRESS-127 | `ascii_set_member64_css_delimiter` | 2B/2E | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs` scan block or generated successor | CSS L4 strict lightningcss/cssparser row movement, `Track1 > lightningcss + 1` | 80-140 | remove production call and keep scalar scan block | equality miss, checkasm fail, or CSS row regression | Dispatchable candidate once S-P3 names the exact generated scan-block row. |
| `RS-PARSE-THAT-HIR` | REDRESS-119, REDRESS-127 | `bbnf_regex_hir_import` / `regex_info_to_backendexpr_facts` / `scanner_plan_import` | 2F | resolver fact provider or generated scanner loop | JSON/CSS equality and P1-P8 cascade replacement without regression | 160-700 | remove import link/fact provider/scanner plan | unpinned snapshot, retained runtime stream, stale cost, or row miss | Candidate only after snapshot/license/HIR mapping closes without new BIR/API. |

## Summary-State Wording

`admissibility_state` remains reserved for the normalized enum introduced in
V3. Local tables in 2B, 2E, and 2F are owner summaries only. If they carry
prose, the column must be read as `summary_status`, `disposition`, or
`blocker`, never as a gate-consumed admission state.
