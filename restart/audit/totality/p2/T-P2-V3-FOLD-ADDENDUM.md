# T-P2 V3 Fold Addendum

Pass: T-P2 Research.
Cycle: V3.
Date: 2026-05-21.
Scope: fold of V2 CH1 provenance/counting and CH4 cost/executability revise set.
Output: this file.

## V3 Fold Authority

This addendum supplements `T-P2-V2-FOLD-ADDENDUM.md`. It does not replace the
V2 Lock 14, Lock 1, REDRESS, material-differential, or anti-paper-close
contracts. It resolves the V2 CH1 and CH4 revise set by adding:

- source-register pin repairs for the remaining generic repository roots;
- reproducible `primary_sources_cited` metadata through explicit
  `counted_source_ids` lists in 2A-2F;
- one shared executable admission ledger for 2B, 2E, and 2F routes;
- a normalized admission-state vocabulary; and
- numeric or dereferenceable abrogate caps for decision-engine gates.

## Provenance Pin Repairs

Rows below are binding source authority for V3. Any downstream row that cites
the same upstream source uses these SHAs or records a newer source-date in its
own source register.

| source | V3 authority |
|---|---|
| simdjson | `168ef580757d75270475b379e83c2b39787a6765` from V2; no generic-root-only citation remains valid. |
| RE2 | `972a15cedd008d846f1a39b2e88ce48d7f166cbd` from V2; no generic-root-only citation remains valid. |
| Rust regex | `839d16bc65b60e2006d3599d20bfa6efc14049d8` from V2; no generic-root-only citation remains valid. |
| fast_float | `05087a303dad9c98768b33c829d398223a649bc6` from V2; no generic-root-only citation remains valid. |
| memchr | `db1a77d4b556a1321e136ca0514e43e74ea5fcc3`, verified by `git ls-remote https://github.com/BurntSushi/memchr.git HEAD` on 2026-05-21. |
| xxHash | `e573d4d2aaeaba0f3e5a0a9a54144a1f2b4b56e7`, verified by `git ls-remote https://github.com/Cyan4973/xxHash.git HEAD` on 2026-05-21. |

## Counted Source Convention

The frontmatter field `primary_sources_cited` is reproducible only through the
frontmatter field `counted_source_ids`. The count must equal the length of that
list. Broad local evidence bundles may be counted when the dossier uses them as
primary local evidence, but inherited addendum rows count only when their ID is
listed.

V3 counted-source lists are:

| dossier | required count | counted ids |
|---|---:|---|
| 2A | 15 | `T2A-SRC-V2-FOLD`, `T2A-SRC-SIMDJSON-PAPER`, `T2A-SRC-SIMDJSON-SRC`, `T2A-SRC-SONIC`, `T2A-SRC-YYJSON`, `T2A-SRC-ASMJSON-README`, `T2A-SRC-ASMJSON-SAXDOM`, `T2A-SRC-ASMJSON-CONFORMANCE`, `T2A-SRC-FFMPEG-CHECKASM`, `T2A-SRC-DAV1D-CHECKASM`, `T2A-SRC-REDRESS`, `T2A-SRC-RESULTS`, `T2A-SRC-T-P1-1A`, `T2A-SRC-T-P1-1E`, `T2A-SRC-S-P1-LEDGER` |
| 2B | 24 | `T2B-SRC-FFMPEG-C`, `T2B-SRC-FFMPEG-H`, `T2B-SRC-VIDEOLAN-CHECKASM`, `T2B-SRC-ARM-ACLE`, `T2B-SRC-ARM-NEON`, `T2B-SRC-P2-PROMPT`, `T2B-SRC-T-P1-1D`, `T2B-SRC-T-P1-1E`, `T2B-SRC-T-P1-HARDENING`, `T2B-SRC-V2-FOLD`, `T2B-SRC-V3-FOLD`, `T2B-SRC-LOCKS`, `T2B-SRC-SK-V13-SYNTHESIS`, `T2B-SRC-SK-V13-HANDOFF`, `T2B-SRC-SIMD-SCOPING`, `T2B-SRC-P1-B`, `T2B-SRC-P1-C`, `T2B-SRC-P1-E`, `T2B-SRC-P1-LEDGER`, `T2B-SRC-P2-B`, `T2B-SRC-P2-C`, `T2B-SRC-RESULTS`, `T2B-SRC-REDRESS`, `T2B-SRC-BBNF-SIMD` |
| 2C | 7 | `T2C-SRC-CSS-SYNTAX`, `T2C-SRC-SELECTORS`, `T2C-SRC-CSS-VALUES`, `T2C-SRC-CSS-VARIABLES`, `T2C-SRC-OPENFORMULA`, `T2C-SRC-V2-FOLD`, `T2C-SRC-LOCAL-EVIDENCE` |
| 2D | 11 | `SRC-01`, `SRC-02`, `SRC-03`, `SRC-04`, `SRC-05`, `SRC-06`, `SRC-07`, `SRC-08`, `SRC-09`, `SRC-10`, `SRC-11` |
| 2E | 11 | `SRC-A64-ACLE`, `SRC-A64-NEON`, `SRC-A64-SVE2-MATCH`, `SRC-INTEL-X86`, `SRC-FFMPEG`, `SRC-DAV1D`, `SRC-SCOPE`, `SRC-BBNF-DISPATCH`, `SRC-BBNF-CHECKASM`, `SRC-BBNF-X86`, `SRC-V2-FOLD` |
| 2F | 21 | `SRC-COX-REGEX`, `SRC-RE2`, `SRC-RUST-REGEX`, `SRC-MEMCHR`, `SRC-FASTFLOAT`, `SRC-FNF`, `SRC-CLINGER`, `SRC-SIMDJSON-PAPER`, `SRC-SIMDJSON-SRC`, `SRC-UTF8`, `SRC-XXHASH`, `SRC-BBNF-PTR`, `SRC-UPSTREAM-REGEX`, `SRC-UPSTREAM-SCAN`, `SRC-BBNF-SIMD`, `SRC-BBNF-CODEGEN`, `SRC-BBNF-RUNTIME`, `SRC-BBNF-DIGEST`, `SRC-REDRESS`, `SRC-T-P1`, `SRC-V2-ADDENDUM` |

## Admission State Vocabulary

The only values allowed in an `admissibility_state` field are:

```text
source_backed
scalar_backed
checkasm_backed
micro_proven
production_wired
row_admitted
measured_rejected
architectural_block
```

Disposition labels such as `conditional`, `conditional-high-risk`,
`inventory`, `partial`, `ADMITTED-EVIDENCE`, and `NOT-VALIDATED` are allowed
only in a separate `disposition_or_blocker` field. Mappings:

| old label | admissibility_state | disposition_or_blocker |
|---|---|---|
| `conditional` | nearest completed state, usually `source_backed` or `scalar_backed` | missing consumer/checkasm/equality step named row-locally |
| `conditional-high-risk` | `source_backed` | REDRESS material-differential checklist not yet passed |
| `inventory` | `source_backed` | source-present, no consumer; must wire, delete, or block |
| `partial` | nearest completed state | missing grammar policy or row consumer |
| `ADMITTED-EVIDENCE` | not an admission-state value | historical row evidence only |
| `NOT-VALIDATED` | not an admission-state value | missing witness/negative-control route |

## Executable Admission Ledger

This shared table is the V3 owner ledger for 2B, 2E, and 2F. T-P3 may copy
rows into wave plans, but cannot relax their required fields.

| candidate_id | owner | scalar_reference | checkasm_or_parity_command | same_wave_consumer_path | expected_row_or_feature_gate | loc_budget | risk_class | rollback_path | abrogate_threshold | admissibility_state | disposition_or_blocker | substrate_target | retention_lifetime | policy_owner |
|---|---|---|---|---|---|---:|---|---|---|---|---|---|---|---|
| `ascii_set_member64_css_delimiter` | 2B/2E | W4 scalar byte-walk reference in `checkasm_ascii_set_member_find_64.rs` | `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --test checkasm_ascii_set_member_find_64 -- --nocapture` plus CSS delimiter cases | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs` scan block or generated successor | CSS L4 scan-block row strict equality vs lightningcss/cssparser and `Track1 > lightningcss + 1` | 80-140 | medium | remove production call and retain scalar scan block | any equality miss, checkasm fail, or CSS row regression | `micro_proven` | production consumer missing | `local_temp_only` | `local_loop` | `generated_grammar` |
| `escape_mask_64` | 2B/2F | existing scalar escape-run oracle in `bbnf-simd` tests | `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --test checkasm_escape_mask_64 -- --nocapture` | JSON/CSS string or escape row consumer | string/escape row movement or prerequisite-only no-admit | 20-60 | low | keep scalar escape scanner | any xorshift/adversarial parity failure | `checkasm_backed` | prerequisite only until consumed | `local_temp_only` | `local_loop` | `generated_grammar` |
| `tbl_tbx_escape_decode_batch` | 2B/2E/2F | JSON fixed-width `read_hex_unit_scalar` plus CSS 1-6 digit scalar oracle | strict x4 JSON/CSS escape parity test; command to be supplied by S-P3 if selected | `parse-that-regex` escape materializer or CSS escaped identifier/string parser | JSON unicode direct/parse/typed row or CSS escaped identifier/string parity row | 160-320 | high | remove SIMD branch and retain scalar decode | any invalid/surrogate/tail mismatch or row regression | `scalar_backed` | CSS policy and strict checkasm incomplete | `direct_sink` or `admitted_fact_output` | `generated_function` | `generated_grammar` |
| `digit_run_accumulate_udot` | 2B/2E/2F | `digit_mac.rs::parse_4_digits` plus number-policy scalar span | new strict DOTPROD parity test over signs, decimals, exponent, invalid lanes, overflow | JSON numeric direct/parse row or CSS number/dimension scanner | sonic strict numeric JSON row or lightningcss CSS numeric row movement | 120-240 | medium | disable DOTPROD feature branch and keep scalar/SWAR path | no numeric leaf in fresh profile, parity fail, or row miss | `source_backed` | scalar/checkasm/consumer incomplete | `direct_sink` or `admitted_fact_output` | `local_loop` | `generated_grammar` |
| `pmull_cssc_structural_union_emit64` | 2B/2E | scalar `prefix_xor_64` + `bitmap_next_set_bit_scalar` + bulk-order oracle | strict structural matrix checkasm over densities, escapes, tails, PMULL/CSSC feature gates | row-local union/structural consumer that bypasses or deletes old scalar cost source | JSON parse/direct structural row or CSS structural row movement with guards | 280-520 | high | remove PMULL/CSSC body and keep existing scalar scan | replay of REDRESS 88/89/96/97/98, no row consumer, parity fail, or guard regression | `source_backed` | material-differential checklist not passed | `existing_tape`, `direct_sink`, or `admitted_fact_output` | `local_loop` or `generated_function` | `generated_grammar` |
| `string_context_64` | 2B/2E/2F | 64-byte scalar oracle composed from current 16-byte string block and byte-context scalar | strict string-context parity over alignments, tails, quote/escape/control policies | JSON string row or CSS string/identifier scanner | sonic strict JSON string row or lightningcss CSS string/identifier row movement | 160-300 | medium | remove wide context path and keep current scalar/string block | no grammar policy, parity fail, or row miss | `source_backed` | support-only until consumer lands | `local_temp_only` | `local_loop` | `generated_grammar` |
| `cache_hint_prefetch_store` | 2B/2E | no semantic scalar reference; placement must be no-op equivalent | perf/equality harness for exact caller placement if selected | named store/prefetch hot caller only | strict no-regression and row movement on caller row | 40-100 | high | delete hint call and module if unconsumed | no caller or any regression | `source_backed` | inventory; delete or wire | `local_temp_only` | `local_loop` | `none` |
| `bbnf_regex_hir_import` | 2F | pinned parse-that HIR snapshot mapped to existing bbnf fact model | JSON/CSS equality fixture for imported HIR facts; no SIMD checkasm | resolver fact provider consumed by generated parser rows | P1-P8 replacement evidence plus JSON/CSS equality | 300-700 | high | remove import link and keep opaque regex as non-admit | unpinned snapshot/license gap, unmapped HIR field, or grammar-name leak | `source_backed` | import/snapshot not closed | `local_temp_only` | `generated_function` | `generated_grammar` |
| `regex_info_to_backendexpr_facts` | 2F | compiled fact extraction over current `RegexProgram` fixtures | before/after resolver fixture parity | e-graph/CSP/cost resolver consuming regex facts | cascade replacement without JSON regression | 160-320 | medium | disable regex fact provider | any fact mismatch, stale cost over 30%, or unresolved fallback | `source_backed` | opaque pattern remains default | `local_temp_only` | `generated_function` | `generated_grammar` |
| `scanner_plan_import` | 2F | scalar generated scanner plan interpreter | JSON/CSS equality fixtures; SIMD checkasm only if hardware body selected | CSS generated scanner loop or JSON string/number loop | strict lightningcss or sonic row movement | 220-420 | high | remove imported scanner plan and keep generated scalar loop | retained mask/class/cursor stream, no row consumer, or row miss | `source_backed` | runtime substrate risk | `local_temp_only` | `local_loop` | `generated_grammar` |
| `semantic_digest_simd_mix` | 2F | `JsonDirectDigest` scalar `mix`/`hash_bytes` oracle | SIMD parity over chunk/tail/endian/raw-decoded cases if selected | `JsonDirectDigest` strict Track 1/Track 2 consumer | sonic strict direct row movement with all prior A/GO guards held | 80-180 | medium | disable SIMD digest fold and keep scalar mix | any semantic digest mismatch or admitted-row regression | `source_backed` | byte-hash substitution blocked | `direct_sink` | `local_loop` | `generated_grammar` |

## Numeric Abrogate Caps

The V3 caps below replace elastic phrases in V2. A later SPEC may tighten them,
but cannot loosen them without a challenge disposition.

| gate | V3 cap | disposition |
|---|---|---|
| e-graph saturation | `<= 50_000` e-nodes, `<= 10_000` e-classes, `<= 30` iterations, `<= 512 MiB` resident memory per grammar | measured reject or reduce rewrite set; no silent cascade fallback |
| CSP solve | `<= 1s` per grammar on the SK-V13 host | measured reject and name unresolved constraint |
| stale cost evidence | `<= 30%` of candidate expressions using stale/static fallback per grammar and output plane | refresh profile evidence or demote candidate |
| generated LOC growth | candidate's generated LOC delta must stay within its ledger `loc_budget` upper bound; if the SPEC later names a stricter per-wave budget, the stricter bound wins | halt wave and inspect O(N) regression |
| row regression | any previously admitted JSON/CSS row below its prior admitted gate | reject unless architectural-block/user re-pin records the demotion |
| parity/checkasm/equality | any scalar, checkasm, strict equality, or independent-oracle failure | reject; no support-only landing |
