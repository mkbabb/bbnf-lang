**Fresh Wave**

I deleted the old wave artifacts under [`.profiles/samply/`](</Users/mkbabb/Programming/bbnf-lang/.profiles/samply/>) and [`.profiles/targets/au-wave/`](</Users/mkbabb/Programming/bbnf-lang/.profiles/targets/au-wave/>) before rerunning. The fresh shared target dir is `/Users/mkbabb/Programming/bbnf-lang/.profiles/targets/au-wave`, and the new wave contract is in [wave.tsv](/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/prebuild/wave.tsv) and [binaries.tsv](/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/prebuild/binaries.tsv). All five fresh runs have the required retained artifacts.

Fresh timings from the retained files:
- `json_monolithic/canada`: `1289 MB/s` in [bench.txt](</Users/mkbabb/Programming/bbnf-lang/.profiles/samply/json_monolithic/canada/bench.txt>) and `1256 MB/s` in [record.txt](</Users/mkbabb/Programming/bbnf-lang/.profiles/samply/json_monolithic/canada/record.txt>)
- `css_l4/tailwind`: `591 MB/s` in [bench.txt](</Users/mkbabb/Programming/bbnf-lang/.profiles/samply/css_l4/tailwind/bench.txt>) and `587 MB/s` in [record.txt](</Users/mkbabb/Programming/bbnf-lang/.profiles/samply/css_l4/tailwind/record.txt>)
- `google_sheets_monolithic/parse_simple`: `91 MB/s` in [bench.txt](</Users/mkbabb/Programming/bbnf-lang/.profiles/samply/google_sheets_monolithic/parse_simple/bench.txt>) and `90 MB/s` in [record.txt](</Users/mkbabb/Programming/bbnf-lang/.profiles/samply/google_sheets_monolithic/parse_simple/record.txt>)
- `bbnf_monolithic/bbnf_self`: `388 MB/s` in [bench.txt](</Users/mkbabb/Programming/bbnf-lang/.profiles/samply/bbnf_monolithic/bbnf_self/bench.txt>) and `377 MB/s` in [record.txt](</Users/mkbabb/Programming/bbnf-lang/.profiles/samply/bbnf_monolithic/bbnf_self/record.txt>)
- `json_value/sonic_canada`: `1544 MB/s` in [bench.txt](</Users/mkbabb/Programming/bbnf-lang/.profiles/samply/json_value/sonic_canada/bench.txt>) and `1497 MB/s` in [record.txt](</Users/mkbabb/Programming/bbnf-lang/.profiles/samply/json_value/sonic_canada/record.txt>)

The `record.txt` and `load.txt` files match the expected ports `3130/3131`, `3140/3141`, `3150/3151`, `3160/3161`, and `3170/3171`.

**Per-Bench**

- JSON: [expand.rs](/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/prebuild/expand/json_monolithic/expand.rs:2484) proves scalar projection is firing. `__value` uses `scan_number_strict_f64`, carries `__payload_tag`, and emits `push_leaf_with_u8`, `push_leaf_with_bool`, `push_leaf_with_f64`, plus one plain `push_leaf`. The fresh expand counts are `push_compound=8`, `push_leaf=1`, `push_leaf_with=3`. The saved profile is dominated by `JsonParser::__value` with `3620` leaf samples, then `compute_f64` with `499`, then `TapeBuilder::push_compound` with `185`. String decode is not firing end-to-end: the string path is still `scan_quoted_string_strict(state)).map(|_| ())` in the expand output.
- CSS: [expand.rs](/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/prebuild/expand/css_l4/expand.rs:103147) shows a `push_leaf_with_u32` path for hex color, so the fresh artifacts do not support the old “hex mapping broken” claim. The fresh expand counts are `push_compound=234`, `push_leaf=22`, `push_leaf_with=7`, `scan_ws_block_comments=319`, `scan_number_f64(...).map(|_|())=20`, `DEFAULT_IDENT_CONFIG=7`, `CSS_IDENT_CONFIG=1`, `push_leaf_with_u32=1`. The fresh profile is dominated by `__compoundSelector` (`2854` leaf samples), `__declaration` (`1782`), and `scan_ws_block_comments_slow` (`751`). That proves whitespace/comment scanning is active and material, while CSS number decode is still computed and discarded.
- Sheets: [expand.rs](/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/prebuild/expand/google_sheets_monolithic/expand.rs:9152) shows the precedence tower rooted at `__unary_expr`, with repeated `mark_children` and `push_compound`, and digit scanning via `scan_digits_mut`. The fresh expand counts are `push_compound=37`, `push_leaf=0`, `push_leaf_with=0`, `scan_digits_mut=18`, `scan_digits_star_mut=3`. The fresh profile is dominated by `__unary_expr` (`1231`), `__exp_expr` (`395`), `__mul_expr` (`381`), `__add_expr` (`348`), `__concat_expr` (`345`), `__comparison_expr` (`315`), plus `_mi_heap_realloc_zero`, `grow_one`, and `finish_grow`. That is direct evidence of both precedence churn and tape growth pressure.
- BBNF: the bench expand file only proves the timed parse call remains under `bench_with_timeout` in [expand.rs](/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/prebuild/expand/bbnf_monolithic/expand.rs:152); the parser body itself is the generated bootstrap parser surfaced in [syms-proof.txt](</Users/mkbabb/Programming/bbnf-lang/.profiles/samply/bbnf_monolithic/bbnf_self/syms-proof.txt>). The fresh profile is dominated by `BbnfBootstrap::__mapped_factor` (`2173` leaf samples), `__rhs` (`787`), `__big_comment` (`698`), `parse` (`607`), `__binary_factor` (`549`), and `__directive` (`421`). `trim_leading_whitespace_scan_and_cache` is present but secondary (`54` leaf samples). `grow_one` and `finish_grow` are present here too.
- sonic competitor: [value.rs](/Users/mkbabb/Programming/bbnf-lang/crates/core/benches/json/value.rs:51) proves this run is the sonic benchmark on `canada.json`. The fresh profile is dominated by `sonic_rs::Parser<...>::parse_array` (`2939` leaf samples) and `parse_object` / `Value::parse_with_padding` inclusively, with `DocumentVisitor::visit_container_end` and memmove also visible. This proves the competitor hot path for the same dataset, but it is still a sonic-only artifact set.

**Synthesis**

What is firing:
- JSON scalar number/bool/null projection is firing. The fresh expand output proves typed leaf pushes are emitted, and the fresh profile proves `compute_f64` is on the hot path.
- CSS whitespace/comment scanning is firing. The fresh profile directly names `scan_ws_block_comments_slow`, and the expand output contains `319` call sites.
- CSS hex payload conversion is firing at least for the generated code path shown by `push_leaf_with_u32`.
- Sheets and BBNF are both dominated by generated parser control flow, not by I/O.
- Tape growth pressure is real in Sheets and BBNF because `RawVec<TapeRec>::grow_one` and `finish_grow` appear in the fresh saved profiles.

What is not firing:
- End-to-end JSON string decode is not firing in the BBNF JSON parser. The string branch still discards the scanner result in [json expand](/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/prebuild/expand/json_monolithic/expand.rs:2508).
- CSS number payload retention is not firing. The fresh expand artifact still has `20` discarded `scan_number_f64(...).map(|_|())` sites.
- There is no fresh artifact proof that broader SIMD kernels dominate beyond CSS whitespace/comment scanning.
- There is no fresh artifact proof of end-to-end semantic parse parity improvement versus competitors. The fresh wave proves sonic’s `canada` throughput and BBNF monolithic JSON throughput, but the benchmark semantics are not the same value path, so parity is still unproven.

Common themes across the fresh wave:
- Tape size/shape: JSON is mixed but still structure-heavy; CSS is overwhelmingly compound-heavy; Sheets is entirely compound in the fresh expand counts; BBNF’s profile shape is recursive-rule heavy with some tape growth. The strongest tape-shape evidence is `push_compound=234` for CSS, `push_compound=37` with zero leaves for Sheets, and `grow_one`/`finish_grow` in Sheets and BBNF.
- Tape surgery: the best tape-surgery targets are CSS and Sheets, not JSON. JSON already has scalar leaves firing; CSS and Sheets still spend most of their work building compound structure.
- Bit-packing: the runtime already packs kind/meta in [builder.rs](/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-tape/src/builder.rs:87). The fresh artifacts do not prove bit-packing itself is the bottleneck, so this remains a secondary proposal for compound-heavy paths.
- SIMD: only CSS whitespace/comment scanning has direct fresh evidence.
- String decode: still missing for JSON.
- Number decode: firing and retained in JSON; firing but discarded in CSS.

Per-bench next moves supported by the fresh artifacts:
- JSON: optimize `__value`, `compute_f64`, and string materialization before anything else.
- CSS: keep the whitespace fast path, stop discarding numeric decode, and reduce selector/declaration compound churn.
- Sheets: attack precedence recursion and tape growth together.
- BBNF: focus on `__mapped_factor`, `__rhs`, `__big_comment`, and `__binary_factor`; treat whitespace trimming as secondary.
- Global: improve record-capacity heuristics for compound-heavy grammars and only pursue competitor-parity claims after profiling a BBNF value-path benchmark on the same semantics as `sonic_canada`.

Artifacts used: [wave.tsv](/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/prebuild/wave.tsv), [json expand](/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/prebuild/expand/json_monolithic/expand.rs:2484), [css expand](/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/prebuild/expand/css_l4/expand.rs:103147), [sheets expand](/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/prebuild/expand/google_sheets_monolithic/expand.rs:9152), the five fresh bench/profile directories under [`.profiles/samply/`](</Users/mkbabb/Programming/bbnf-lang/.profiles/samply/>), and [builder.rs](/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-tape/src/builder.rs:63) for the current capacity/packing behavior.