---
agent: 2F
pass: T-P2-research
cycle: V1
generated_at: 2026-05-28T02:38:40-04:00
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 20
techniques_grounded: 8
techniques_refuted: 6
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: []
  first_cycle_additions: [PTG-2F-01, PTG-2F-02, PTG-2F-03, PTG-2F-04, PTG-2F-05, PTG-2F-06, PTG-2F-07, PTG-2F-08]
locks_amendment_candidates: 4
sk_cycle: SK-V15
t_p1_entry_state: CLEAN-FINAL-G1-AUTO-PINNED-NOT-NORMAL-3Z
implementation_floor: PASS-IMPL-V1-CSS-CONTRIVANCE-JSON-HONEST
host_close_route: Apple-M5-Max-aarch64
stale_sk_v14_material_reused: none
---

## Executive Summary

The live parse-that family is narrower than the old SK-V14 dossier implied:
`skinny/Cargo.toml:10-12` contains `parse-that-regex`, `bbnf-regex`, and
`bbnf-simd`, but no base `parse-that` crate. JSON string/number primitives are
real and consumer-wired, but CSS L4 is still outside the primitive family: the
current CSS path is a generated-looking string literal parser
(`skinny/crates/codegen/src/runtime_generator.rs:713`) emitting fact streams
and `CssFullParseSummary` counters, not a typed value API.

SK-V15 should **vendor SIMD/string/float kernels only inside `bbnf-simd` with
scalar references and checkasm parity**, then wire them through
`parse-that-regex` or generated consumers. It should **upstream regex/HIR facts
into local `bbnf-regex`**, not import a runtime regex engine unless a concrete
generator consumer proves need. It should **not route CSS value parsing through
JSON-shaped `parse-that-regex`**; W5/W6 need a grammar-derived CSS typed
provider with `cssparser` as same-workload oracle. No CSS broadcast or
brace-counter proof can close a parse-that gap.

## Source Registry

| ID | Primary source | Use |
|---|---|---|
| SRC-COX | Russ Cox, ["Regular Expression Matching Can Be Simple And Fast"](https://swtch.com/~rsc/regexp/regexp1.html), 2007. | Thompson NFA / finite-automata baseline; refutes backtracking as the generator contract. |
| SRC-RUST-REGEX-HIR | `regex-syntax` HIR docs: <https://shadow.github.io/docs/rust/regex_syntax/hir/struct.Hir.html>. | HIR as analysis surface between regex AST and matcher. |
| SRC-RUST-REGEX | Rust regex repository, <https://github.com/rust-lang/regex>. | Finite-automata matcher split from syntax analysis; candidate source only for build-time study, not runtime import. |
| SRC-MEMCHR | `memchr2` docs, <https://docs.rs/memchr/latest/memchr/fn.memchr2.html>, and `BurntSushi/memchr`, <https://github.com/BurntSushi/memchr>. | Byte-search primitive used today by trusted JSON string scans. |
| SRC-SIMDJSON | Langdale and Lemire, "Parsing Gigabytes of JSON per Second", VLDB 2019, and simdjson source, <https://github.com/simdjson/simdjson>. | Structural/quote/backslash mask technique; admissible only as transient local masks. |
| SRC-SIMDUTF | Keiser and Lemire, "Validating UTF-8 In Less Than One Instruction Per Byte", arXiv:2010.03090, and simdutf source, <https://github.com/simdutf/simdutf>. | UTF-8 block validation model; bbnf needs scalar parity and aarch64 gate. |
| SRC-HOEHRMANN | Bjoern Hoehrmann, ["Flexible and Economical UTF-8 Decoder"](https://bjoern.hoehrmann.de/utf-8/decoder/dfa/). | Scalar streaming UTF-8 reference. |
| SRC-CLINGER | William D. Clinger, "How to Read Floating-Point Numbers Accurately", PLDI 1990. | Correct-rounding basis for decimal-to-binary conversion. |
| SRC-EISEL-LEMIRE | Daniel Lemire, ["Number Parsing at a Gigabyte per Second"](https://arxiv.org/abs/2101.11408), 2021. | Fast f64 conversion basis. |
| SRC-FNF | Noble Mushtak and Daniel Lemire, ["Fast Number Parsing Without Fallback"](https://arxiv.org/abs/2212.06644), 2022. | Refutes claiming no-fallback when fallback remains live. |
| SRC-RFC8259 | RFC 8259, <https://www.rfc-editor.org/rfc/rfc8259>. | JSON string, number, and UTF-8 correctness floor. |
| SRC-CSS-SYNTAX | W3C CSS Syntax Level 3, <https://www.w3.org/TR/css-syntax-3/>. | CSS tokenization and parser algorithms; establishes that brace counters are not CSS parser parity. |
| SRC-CSS-TYPED-OM | W3C CSS Typed OM Level 1, <https://www.w3.org/TR/css-typed-om-1/>. | Typed CSS value surface expectation. |
| SRC-CSSPARSER | Servo `cssparser` docs, <https://doc.servo.org/cssparser/struct.Parser.html>. | Near-term same-workload CSS parser oracle. |
| SRC-LIGHTNINGCSS | Lightning CSS repository, <https://github.com/parcel-bundler/lightningcss>. | Typed property-value and CSSOM comparator only after bbnf emits comparable output. |
| SRC-PARSE-THAT-REGEX | `skinny/crates/parse-that-regex/src/lib.rs:1-1240`; `number/mod.rs:1-340`; `number/eisel_lemire/mod.rs:1-177`; `number/eisel_lemire/algorithm.rs:1-93`; `Cargo.toml:1-12`. | Live string, UTF-8, and number primitive consumer. |
| SRC-BBNF-REGEX | `skinny/crates/bbnf-regex/src/lib.rs:1-220`; `docs/parse-that/regex-engine.md:1-134`. | Live regex-fact crate vs historical NFA/DFA design doc. |
| SRC-BBNF-SIMD | `skinny/crates/bbnf-simd/src/lib.rs:1-292`; `aarch64/string_block.rs:1-72`; `aarch64/utf8/validate_block.rs:1-158`; `aarch64/digit_mac.rs:1-71`. | Kernel home and scalar/checkasm boundary. |
| SRC-CODEGEN | `skinny/crates/codegen/src/runtime_generator.rs:1-190,713-1119`; `grammar_profile.rs:1-130`; `grammar_provider.rs:31-74`; `lower/sink_only.rs:68-85`; `skinny/crates/ir/src/lib.rs:354-371`; `skinny/crates/passes/src/lib.rs:910-918,1302-1305`. | Generator integration and current gaps. |
| SRC-SK-V15-FLOOR | `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21-31,102-130`; `restart/skinny/tranches/sk-v15/SPEC.md:54-63,194-196,361-368`; `skinny/RESULTS.md:112-135`; `skinny/REDRESS.md:4081-4094,5316-5340`. | SK-V15 prune/rebuild floor, broadcast proof, and prior regex-fact non-consumption. |

## Technique Grounding Table

| spec-claim or gap id | published source cited | grounded / refuted / partial | bbnf-specific note |
|---|---|---|---|
| PTG-2F-01 regex/HIR facts | SRC-COX; SRC-RUST-REGEX-HIR; SRC-BBNF-REGEX; SRC-CODEGEN | partial | **Upstream local** into `bbnf-regex`. Live `bbnf-regex` exposes `RegexFacts`, `FirstSet`, `ByteClass`, and a thin `RegexHir` kind at `src/lib.rs:1-64`, but not the documented Thompson NFA/DFA pipeline in `docs/parse-that/regex-engine.md:15-26`. The bbnf need is generator selection facts, not a runtime dependency. |
| PTG-2F-02 runtime regex/DFA matcher | SRC-COX; SRC-RUST-REGEX; SRC-BBNF-REGEX; SRC-SK-V15-FLOOR | refuted as SK-V15 close requirement | **Do not vendor runtime `regex-automata` into parser runtime.** The only current production use is analysis: `passes/src/lib.rs:910-918` consumes first bytes, and `BackendExpr::RegexProgram` still stores only `pattern: String` at `ir/src/lib.rs:368-370`. REDRESS records regex facts as not row-moving until e-graph/CSP/generated runtime consume them. |
| PTG-2F-03 SIMD byte scan / structural scan | SRC-SIMDJSON; SRC-MEMCHR; SRC-BBNF-SIMD; SRC-PARSE-THAT-REGEX | grounded primitive, partial integration | **Vendor in `bbnf-simd`; wire consumers in generated code and `parse-that-regex`.** `bbnf-simd` has 64-byte table/equality-mask primitives at `src/lib.rs:254-292`, but `active_backend()` still returns scalar at `src/lib.rs:139-142`; string scan uses 16-byte aarch64 blocks at `aarch64/string_block.rs:57-71` and `memchr2` for trusted scans at `parse-that-regex/src/lib.rs:685-697`. |
| PTG-2F-04 string / escape / UTF-8 primitives | SRC-RFC8259; SRC-SIMDUTF; SRC-HOEHRMANN; SRC-PARSE-THAT-REGEX; SRC-BBNF-SIMD | grounded for JSON, partial for grammar-neutrality | **Vendor kernels in `bbnf-simd`; keep `parse-that-regex` as consumer API.** JSON string APIs are real (`match_string_at_quote*` at `lib.rs:163-408`, unescape at `lib.rs:867-959`), and aarch64 UTF-8 block validation exists at `bbnf-simd/src/aarch64/utf8/validate_block.rs:76-158`. Gap: no CSS string/comment escape consumer and no grammar-neutral scanner contract that parameterizes CSS terminators, newline rules, comments, and escapes. |
| PTG-2F-05 float parsing | SRC-CLINGER; SRC-EISEL-LEMIRE; SRC-FNF; SRC-PARSE-THAT-REGEX; SRC-BBNF-SIMD | grounded with refuted no-fallback claim | **Keep vendored Eisel-Lemire in `parse-that-regex`; vendor digit-block acceleration in `bbnf-simd` only after scalar/checkasm parity.** Current `materialize_f64` falls back to `text.parse::<f64>()` at `number/mod.rs:261-271`; Eisel-Lemire returns `None` on ambiguous rounding at `eisel_lemire/mod.rs:166-169`. The aarch64 DotProd helper is only 4 digits at `bbnf-simd/src/aarch64/digit_mac.rs:25-49` and is not wired into number scanning. |
| PTG-2F-06 CSS value parsing | SRC-CSS-SYNTAX; SRC-CSS-TYPED-OM; SRC-CSSPARSER; SRC-LIGHTNINGCSS; SRC-CODEGEN; SRC-SK-V15-FLOOR | refuted for current implementation | **Upstream into the generated CSS typed provider, not into JSON-shaped `parse-that-regex`.** Current generated CSS emits fact strings (`emit_fact_stream` at `runtime_generator.rs:717-760`) and `CssFullParseSummary` counters (`runtime_generator.rs:765-812`; generated file `css_l4_declaration_values/generated.rs:50-101`). CSS Syntax requires token streams, component values, declarations, rules, and error recovery; Typed OM requires typed values. |
| PTG-2F-07 generator integration | SRC-BBNF-REGEX; SRC-CODEGEN; SRC-SK-V15-FLOOR | partial, current CSS route refuted | **Upstream into `codegen` after W0/W1 truth repair.** JSON `PassCompiled` emits real generated runtime (`runtime_generator.rs:19-47`); CSS `FrontendFacts` copies `CSS_GENERATED_RS` (`runtime_generator.rs:81-104`). Regex facts reach passes (`passes/src/lib.rs:895,910-918`) but `SinkOnlyExpr::RegexProgram` preserves only the pattern string at `lower/sink_only.rs:81-85`. The bbnf need is fact-bearing generated selection across JSON, CSS, Sheets, BBNF-self. |
| PTG-2F-08 CSS broadcast / row-local equality | SRC-SK-V15-FLOOR; SRC-CSSPARSER; SRC-LIGHTNINGCSS | refuted as admission evidence | **No upstream or vendor primitive can compensate for broadcast evidence.** `skinny/RESULTS.md:112-135` repeats one CSS tuple across conceptual rows, and SK-V15 SPEC `DEP-W1-CSS-BROADCAST` requires telemetry demotion before rebuild. The verify action is duplicate-signature detection over `measurement_row_id`, `measurement_origin`, `broadcast_group_id`, value plane, equality, and timing. |

## Architectural Assertions Defended

1. **Regex facts belong upstream in local `bbnf-regex`, not as a runtime crate import.** Cox and Rust regex HIR both support finite-automata/analysis separation. The local implementation already carries nullability, first-set, byte-class, and HIR-shaped facts (`bbnf-regex/src/lib.rs:1-64`), and passes consume first bytes at `passes/src/lib.rs:910-918`. The missing part is not "add `regex` to runtime"; it is making generator selection consume richer facts.

2. **SIMD primitives belong below parse-that consumers.** `bbnf-simd` is the kernel and parity home; `parse-that-regex` is the JSON string/number consumer. This preserves Lock 16: scalar reference, hardware gate, checkasm parity, same-wave consumer. A primitive promoted directly inside generated CSS or `parse-that-regex` without a scalar/checkasm sibling would repeat the SK-V14 paper-close pattern.

3. **The current JSON number stack is literature-grounded but not no-fallback.** Clinger and Eisel-Lemire justify the fast path; SRC-FNF explains why no-fallback is a distinct claim. The live path falls back (`number/mod.rs:270-271`) after ambiguous Eisel-Lemire cases (`eisel_lemire/mod.rs:166-169`), so SK-V15 can defend "correct f64 with fallback" and must measure fallback rate before any no-fallback assertion.

4. **CSS value parsing is a generated-provider problem.** CSS Syntax and Typed OM require a typed token/value/rule/declaration surface. Current `CSS_GENERATED_RS` is a monolithic string literal generator output with fact streams and summary counters. SK-V15 W5/W6 correctly place the rebuild provider before retiring old proof paths (`SPEC.md:194-196,361-368`).

5. **`cssparser` is the near-term same-workload oracle; Lightning CSS is diagnostic until output parity exists.** The SK-V15 SPEC says cssparser sets the immediate CSS floor and Lightning CSS counts only after comparable CSSOM/value output. Lightning CSS is valuable because it exposes typed property values, but a bbnf brace-counter or fact stream is not comparable to that workload.

## Architectural Assertions Refuted

| assertion | refutation | consequence |
|---|---|---|
| "The base `parse-that` crate is present and can own SK-V15 primitive work." | `skinny/Cargo.toml:3-15` has `parse-that-regex`, `bbnf-regex`, and `bbnf-simd`; no base `parse-that` member exists. | Dossier decisions target the live crates only. Any base-crate resurrection needs a SPEC amendment and generator consumer. |
| "`parse-that-regex` already has the regex/HIR/DFA engine from the docs." | `docs/parse-that/regex-engine.md:15-26` describes HIR -> Thompson NFA -> DFA -> minimization, but live `bbnf-regex/src/lib.rs:45-64` returns facts from heuristic classification and first-set extraction. | Upstream regex work is an implementation gap. It cannot be cited as closed until a source-present DFA/HIR path and consumer exist. |
| "Runtime regex import is the right fallback." | The docs themselves separate runtime dependency from `regex` (`docs/parse-that/regex-engine.md:9`), and SK-V15 forbids new substrates/directives without explicit disposition. | If DFA overflow matters, vendor/build-script generation or local `bbnf-regex` extension is the route; no runtime `regex-automata` dependency without a concrete CH3/CH5 review. |
| "JSON string primitives are enough for CSS strings and comments." | JSON string mode validates JSON escapes and controls (`parse-that-regex/src/lib.rs:410-521,867-959`). CSS Syntax defines CSS strings, comments, escaped code points, component values, and error recovery separately. | CSS typed provider must own CSS tokenization/value semantics; parse-that string kernels may be reused only below a CSS-specific consumer. |
| "`CssFullParseSummary` is CSS value parsing." | The generated summary has four counters (`generated.rs:53-59`) and `runtime_generator.rs:765-812` emits only those counters. CSS Typed OM requires typed numeric/keyword/unparsed/value objects, and SK-V15 explicitly retires `CssFullParseSummary`. | W5 must build typed CSS value/document/view/visitor surfaces before W6 retimes. |
| "A primitive can move CSS rows even while W8R broadcast remains." | PASS-IMPL V1 and `RESULTS.md:112-135` show one repeated timing tuple; SK-V15 `DEP-W1-CSS-BROADCAST` makes demotion/telemetry a prerequisite. | No parse-that gap admits CSS row movement before W0/W1 telemetry and broadcast demotion pass. |

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| Does any SK-V15 consumer require a runtime DFA matcher rather than first-set/nullability/byte-class facts? | Before adding DFA code, run `rg -n "RegexProgram|find_at|SpanParser::Regex|regex_facts" skinny/crates restart/skinny/tranches/sk-v15` and record the consumer path. If no emitted runtime path consumes DFA states, keep the work scoped to `bbnf-regex` analysis facts. |
| Which CSS token/value primitives can reuse JSON string kernels? | In W5 research/plan, map CSS Syntax algorithms for comments, strings, url tokens, escaped code points, numeric tokens, and declaration values to either existing `bbnf-simd` kernels or a new CSS-specific scalar oracle. Reject any row that uses `parse-that-regex::unescape_string` as the CSS semantic parser. |
| What is the fallback rate of `materialize_f64` on the SK-V15 JSON census and future CSS numeric values? | Instrument `parse-that-regex::number::materialize_f64` around `compute_f64` and fallback at `number/mod.rs:261-271`; emit row-local counts in gate telemetry before claiming no-fallback or adding DotProd digit-block work. |
| Does `bbnf-simd::active_backend()` staying scalar affect SK-V15 aarch64 close evidence? | Run the W2 Lock 16/source-present primitive report over `bbnf-simd/src/lib.rs:139-142` and checkasm tests; either wire aarch64 dispatch for the named primitive or mark it source-present/unwired so it cannot be cited as row movement. |
| Can CSS typed provider deletion order satisfy NEW-CH3-V5-01? | Before retiring `CSS_GENERATED_RS`, run `rg -n "CSS_GENERATED_RS|CssFullParseSummary|Result<String, CssFactError>|fact_stream|brace" skinny/crates/codegen skinny/crates/runtime` and require a same-wave typed provider test against `cssparser`. |

## LOCKS-AMENDMENTS-CANDIDATE

| candidate | amendment | reason | verify action |
|---|---|---|---|
| LAC-2F-V1-01 | Lock 16 should classify parse-that primitives by owner: `bbnf-regex` for compile-time regex facts, `bbnf-simd` for kernels, generated providers for grammar semantics, and `parse-that-regex` for JSON/string/number consumers. | Prevents stale "parse-that owns everything" reasoning and keeps scalar/checkasm boundaries enforceable. | Add a gate row that reports owner, scalar reference, checkasm status, hardware gate, and same-wave consumer for every parse-that-family primitive. |
| LAC-2F-V1-02 | Runtime regex engines are inadmissible without a named generated-runtime consumer and CH3/CH5 review. | The live repo has analysis facts but no row-moving DFA consumer; runtime imports risk a hidden substrate. | Before any runtime DFA plan, require the `rg` consumer scan from Q1 plus a REDRESS scan for prior regex/DFA attempts. |
| LAC-2F-V1-03 | CSS typed value parsing may reuse byte kernels but not JSON string/number semantic APIs as the parser. | CSS Syntax and Typed OM semantics differ from JSON; current CSS proof is fact-stream/summary-counter based. | W5 plan must list CSS token/value scalar oracles and same-workload `cssparser` equality tests before W6 retirement. |
| LAC-2F-V1-04 | Primitive row movement must be blocked while broadcast telemetry is unresolved. | A SIMD or parse-that improvement cannot repair one timing tuple broadcast across 24 CSS conceptual rows. | Gate duplicate timing/equality signatures across `measurement_row_id`, `measurement_origin`, `broadcast_group_id`, `value_plane`, command, input, equality, and timing. |
