---
agent: 2F
pass: T-P2-research
cycle: V3
generated_at: 2026-05-21T04:42:44-04:00
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 21
counted_source_ids: [SRC-COX-REGEX, SRC-RE2, SRC-RUST-REGEX, SRC-MEMCHR, SRC-FASTFLOAT, SRC-FNF, SRC-CLINGER, SRC-SIMDJSON-PAPER, SRC-SIMDJSON-SRC, SRC-UTF8, SRC-XXHASH, SRC-BBNF-PTR, SRC-UPSTREAM-REGEX, SRC-UPSTREAM-SCAN, SRC-BBNF-SIMD, SRC-BBNF-CODEGEN, SRC-BBNF-RUNTIME, SRC-BBNF-DIGEST, SRC-REDRESS, SRC-T-P1, SRC-V2-ADDENDUM]
techniques_grounded: 13
techniques_refuted: 6
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: [CH1, CH2, CH3, CH4, CH5, CH6]
  first_cycle_additions: [PTG-REGEX-HIR-ENGINE, PTG-REGEX-INFO-FACTS, PTG-SIMD-SPAN-SCAN, PTG-STRING-SCAN-UTF8, PTG-UNICODE-ESCAPE-CODEC, PTG-FLOAT-CLINGER-EISEL, PTG-INTEGER-SWAR-DOTPROD, PTG-DIGEST-SEMANTIC-MIX, PTG-CSS-SCANNER-GAP]
locks_amendment_candidates: 4
v2_fold_addendum: restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md
v3_fold_addendum: restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md
parse_that_import_authority: conditional-local-head-051a6d681da95a180e6b67f956526722d1d33322
v3_fold_additions: [PINNED-SOURCE-REGISTER-REPAIR, EXECUTABLE-PARSE-THAT-LEDGER, NORMALIZED-STATE-FIELD]
---

## Executive Summary

The live skinny crate named `parse-that-regex` is not a regex/HIR engine. It
exports JSON-oriented string, number, unicode, and a minimal SIMD hook surface
(`skinny/crates/parse-that-regex/src/lib.rs:4-8`), while the separate
`parse-that` worktree contains the actual HIR, NFA, DFA, byte-class, regex-info,
and span-scanner machinery (`/Users/mkbabb/Programming/parse-that/rust/regex/src/lib.rs:1-38`).
V2 pins that sibling worktree only as conditional import authority:
`051a6d681da95a180e6b67f956526722d1d33322`, with dirty/untracked state
recorded in `T-P2-V2-FOLD-ADDENDUM.md`. Therefore `bbnf-regex` extraction is
not an unconditional architecture step yet. It is a conditional candidate until
the revision is vendored/tagged or snapshotted, license and import boundaries
are recorded, and the required HIR/regex facts map into current bbnf types.
The primitive split remains valid: compile-time regex/HIR/scanner facts may
come from parse-that or established automata libraries; runtime masks, byte
classes, cursors, and scanner streams must stay transient inside generated
consumer loops. Low-level aarch64 kernels remain in `bbnf-simd` under Lock 16.
Float and integer materializers are partially present, but fallback telemetry,
DOTPROD wiring, same-wave row consumers, and admission ledgers are missing.
Digest primitives remain bbnf semantic-output primitives, not generic byte-hash
substitutions. CSS L4 currently uses a hand-coded scanner, so full parity
expansion needs generated scanner plans or grammar-local templates, not one-off
feature loops.

## V2 Fold Summary

This dossier folds V1 CH1-CH6 through the shared V2 mechanical contract in
`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`.

| lens | V2 fold into 2F |
|---|---|
| CH1 correctness/provenance | Pins parse-that local HEAD `051a6d681da95a180e6b67f956526722d1d33322`, records that dirty/untracked state makes it conditional authority, and removes absolute-path presence as sufficient proof. |
| CH2 generality/Lock 14 | Fences shared parse-that / future `bbnf-regex` APIs to grammar-neutral facts and plans; grammar-named helpers belong only in generated grammar modules or grammar-local facades. |
| CH3 regression/REDRESS | Carries REDRESS 119 row-reopen discipline and keeps proof-only REDRESS 122/126 outcomes out of admission claims. |
| CH4 cost/executability | Adds candidate admission ledger requirements with LOC/risk/consumer/gate/rollback fields before S-P3 can scope an import or scanner route. |
| CH5 hidden coupling/Lock 1 | Splits compile-time scanner facts from runtime scanner substrates and requires `substrate_target`, `retention_lifetime`, and `policy_owner` on every candidate. |
| CH6 anti-paper-close | Downgrades `bbnf-regex` extraction from "mandatory" to conditional until revision/license/import/HIR mapping and a same-wave consumer are complete. |

V2 preserves the V1 findings that skinny `parse-that-regex` is JSON-shaped,
that upstream regex/HIR facts are the likely missing decision-engine substrate,
that retained structural sidecars remain blocked by REDRESS 96/97/98, and that
semantic digest equality cannot be replaced by a byte-hash comparator.

## Source Registry

| ID | Primary source | Use in this dossier |
|---|---|---|
| SRC-COX-REGEX | Russ Cox, "Regular Expression Matching Can Be Simple And Fast" ([swtch.com](https://swtch.com/~rsc/regexp/regexp1.html)) | Thompson-NFA / automata route; refutes backtracking or ad hoc regex parsing as the totality baseline. |
| SRC-RE2 | Google RE2 source/README at HEAD `972a15cedd008d846f1a39b2e88ce48d7f166cbd` ([github.com/google/re2](https://github.com/google/re2/tree/972a15cedd008d846f1a39b2e88ce48d7f166cbd)) | Production finite-automata regex discipline: linear-time, bounded memory, parser/compiler/engine separation. |
| SRC-RUST-REGEX | Rust `regex`, `regex-automata`, and `regex-syntax` source at HEAD `839d16bc65b60e2006d3599d20bfa6efc14049d8` ([github.com/rust-lang/regex](https://github.com/rust-lang/regex/tree/839d16bc65b60e2006d3599d20bfa6efc14049d8)) | Established Rust HIR/NFA/DFA building blocks and the alternative to a bespoke skinny-only engine. |
| SRC-MEMCHR | BurntSushi `memchr` source at HEAD `db1a77d4b556a1321e136ca0514e43e74ea5fcc3` ([github.com/BurntSushi/memchr](https://github.com/BurntSushi/memchr/tree/db1a77d4b556a1321e136ca0514e43e74ea5fcc3)) | Primary Rust source for byte-search primitives used by upstream parse-that span scanners. |
| SRC-FASTFLOAT | `fast_float` official implementation at HEAD `05087a303dad9c98768b33c829d398223a649bc6` ([github.com/fastfloat/fast_float](https://github.com/fastfloat/fast_float/tree/05087a303dad9c98768b33c829d398223a649bc6)) | Fast exact decimal-to-binary float implementation lineage. |
| SRC-FNF | Noble Mushtak and Daniel Lemire, "Fast Number Parsing Without Fallback" ([arXiv 2212.06644](https://arxiv.org/abs/2212.06644)) | Refines Eisel-Lemire fallback risk; bbnf should measure fallback rather than assume no fallback. |
| SRC-CLINGER | William D. Clinger, "How to Read Floating-Point Numbers Accurately", PLDI 1990, DOI 10.1145/93542.93557 ([doi.org](https://doi.org/10.1145/93542.93557)) | Correct-rounding basis for Clinger-style fast path and exact fallback discipline. |
| SRC-SIMDJSON-PAPER | Geoff Langdale and Daniel Lemire, "Parsing Gigabytes of JSON per Second", VLDB Journal 2019 ([arXiv 1902.08318](https://arxiv.org/abs/1902.08318)) | SIMD string/structural-scan architecture and the distinction between scanner speed and consumer admission. |
| SRC-SIMDJSON-SRC | simdjson source/README at HEAD `168ef580757d75270475b379e83c2b39787a6765` ([github.com/simdjson/simdjson](https://github.com/simdjson/simdjson/tree/168ef580757d75270475b379e83c2b39787a6765)) | Production source for validated SIMD JSON parsing, UTF-8, and lossless number claims. |
| SRC-UTF8 | John Keiser and Daniel Lemire, "Validating UTF-8 In Less Than One Instruction Per Byte" ([arXiv 2010.03090](https://arxiv.org/abs/2010.03090)) | SIMD UTF-8 validation route; refutes scalar-only UTF-8 as the only defensible primitive. |
| SRC-XXHASH | xxHash official source/README at HEAD `e573d4d2aaeaba0f3e5a0a9a54144a1f2b4b56e7` ([github.com/Cyan4973/xxHash](https://github.com/Cyan4973/xxHash/tree/e573d4d2aaeaba0f3e5a0a9a54144a1f2b4b56e7)) | Useful byte-hash baseline; explicitly not a replacement for bbnf semantic direct digest. |
| SRC-BBNF-PTR | `skinny/crates/parse-that-regex/src/lib.rs:4-8`, `:157-209`, `:547-573`, `:718-840`; `skinny/crates/parse-that-regex/src/number/mod.rs:31-272`; `skinny/crates/parse-that-regex/src/number/eisel_lemire/mod.rs:1-177` | Live skinny parse-that facade: string, unicode, number, and fallback behavior. |
| SRC-UPSTREAM-REGEX | `/Users/mkbabb/Programming/parse-that/rust/regex/src/lib.rs:1-38`; `/Users/mkbabb/Programming/parse-that/rust/regex/src/hir/mod.rs:1-277`; `/Users/mkbabb/Programming/parse-that/rust/regex/src/automata/dfa.rs:1-170` | Local upstream regex/HIR/NFA/DFA engine not present in skinny `parse-that-regex`. |
| SRC-UPSTREAM-SCAN | `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/span_parser/scan.rs:1-183`; `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/span_parser/span_scanner.rs:1-42` | Local upstream span scanners: memchr1/2/3, LUT, portable SIMD, and monolithic grammar-neutral scanner enum. |
| SRC-BBNF-SIMD | `skinny/crates/parse-that-regex/src/integration/simd_scan_hook.rs:1-19`; `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:30-71`; `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:1-71` | Current SIMD hook plus string and DOTPROD primitive bodies. |
| SRC-BBNF-CODEGEN | `skinny/crates/codegen/src/lower/sink_only.rs:19-93`, `:142-181`; `skinny/crates/codegen/src/json_sink_direct.rs:34-45`, `:315-370` | BIR `RegexProgram` is an opaque string and the JSON direct renderer requires only String/Number/Whitespace spans. |
| SRC-BBNF-RUNTIME | `skinny/crates/runtime/src/grammars/json/generated.rs:10-15`, `:393-421`, `:608-653`; `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:38-164`, `:211-240` | Generated JSON direct consumers and current hand-coded CSS scanner. |
| SRC-BBNF-DIGEST | `skinny/crates/bbnf-bench/src/direct_struct.rs:15-29`, `:58-105`, `:401-425`, `:716-742` | Direct semantic digest shape and strict Track 1/Track 2/serde/sonic parity consumer. |
| SRC-REDRESS | `skinny/REDRESS.md:517-557`, `:633-649`, `:700-713`, `:846-882`, `:2910-2940`, `:3495-3528`, `:3603-3633`, `:3780-3805` | Measured local dispositions: Eisel-Lemire, SinkOnly, digit scan, string/unicode rejections, union falsification, direct fixpoint, Lock 16 prerequisite, ASCII run-skip micro-proof. |
| SRC-T-P1 | `restart/audit/totality/p1/1B-codegen-evidence.md`, `1C-runtime-evidence.md`, `1D-skinny-lessons.md`, `1E-locks-evidence.md`, `1F-past-corpora.md` | Current totality evidence: Lock 14 drift, REDRESS-119/120 history-only framing, same-wave consumer requirements. |
| SRC-V2-ADDENDUM | `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md` | Shared V2 contract: parse-that provenance, Lock 14 transfer, Lock 1 substrate-kind fields, REDRESS reopen matrix, material-differential checklist, admission ledger, and close-state vocabulary. |

## Technique Grounding Table

| spec claim / T-P1 divergence id | published source cited | grounded / refuted / partial | upstream-or-vendor decision | bbnf-specific note |
|---|---|---|---|---|
| PTG-REGEX-HIR-ENGINE | SRC-COX-REGEX, SRC-RE2, SRC-RUST-REGEX, SRC-UPSTREAM-REGEX, SRC-V2-ADDENDUM | grounded candidate / conditional import | Import or vendor the parse-that regex surface only after revision, license, snapshot/tag, and HIR-to-bbnf mapping are completed; do not grow HIR/NFA/DFA inside skinny `parse-that-regex`. | SK-V13 G2 needs a real regex/HIR language for rewrites and scanner selection. Live skinny only exposes `integration`, `number`, and `unicode`, plus string functions, so `parse-that-regex` cannot satisfy G2 by name alone. Local parse-that HEAD `051a6d681da95a180e6b67f956526722d1d33322` is evidence, not admission. |
| PTG-REGEX-INFO-FACTS | SRC-RUST-REGEX, SRC-UPSTREAM-REGEX, SRC-V2-ADDENDUM | grounded candidate / conditional import | Import parse-that regex info/classification facts only as compile-time facts, then adapt to bbnf `BackendExpr` and egraph types. | The upstream crate appears to expose `RegexInfo`, `EngineSet`, first-character facts, byte sets, DFA options, and egraph modules; bbnf needs these as cost-model facts, not as runtime parser calls or retained sidecars. |
| PTG-REGEX-OPAQUE-PATTERN | SRC-BBNF-CODEGEN, SRC-UPSTREAM-REGEX | refuted | Replace opaque `RegexProgram { pattern: String }` planning with compiled HIR/facts before S-P3 wave-scoping. | `SinkOnlyExpr::RegexProgram` currently stores a string and span kind only. That cannot drive CSP/egraph/cost rewrites, byte-class scanner selection, or CSS parity feature coverage. |
| PTG-SIMD-SPAN-SCAN | SRC-MEMCHR, SRC-UPSTREAM-SCAN, SRC-BBNF-SIMD, SRC-V2-ADDENDUM | partial / conditional | Upstream/import scanner-plan selection as compile-time facts; keep hardware-specific aarch64 bodies in `bbnf-simd` with checkasm. Runtime masks/classes/cursors must be local temporaries consumed by generated grammar code in-loop. | Upstream parse-that routes memchr1/2/3, LUT, and portable SIMD scans. Skinny has only a `SimdScannerHook` trait and separate aarch64 primitives, so it lacks the planner that decides which scanner fits a grammar span. Importing that planner cannot create a second substrate. |
| PTG-RETAINED-STRUCTURAL-SUBSTRATE | SRC-SIMDJSON-PAPER, SRC-REDRESS | refuted as a default bbnf route | Do not upstream a retained structural-index substrate into parse-that. Allow transient scanner consumers only. | simdjson validates SIMD scanning as a production parser design, but bbnf REDRESS 96/97 measured two faithful retained-union variants as regressions on this host. A parse-that scanner plan must be transient and row-consumed, not a sidecar substrate. |
| PTG-STRING-SCAN-UTF8 | SRC-SIMDJSON-PAPER, SRC-SIMDJSON-SRC, SRC-UTF8, SRC-BBNF-PTR, SRC-BBNF-SIMD | partial | Vendor grammar-neutral string scanner parameters through bbnf facade; keep aarch64 string blocks in `bbnf-simd`. | Live skinny string matching is JSON-shaped: double quote, backslash, control limit, trusted UTF-8. CSS and Sheets need delimiter/escape/control policy from GrammarConfig, plus scalar reference/checkasm for any SIMD block. |
| PTG-UNICODE-ESCAPE-CODEC | SRC-BBNF-PTR, SRC-REDRESS | partial / prior consumer shapes refuted | Keep the scalar + aarch64 unicode escape codec only with a row-moving generated consumer; do not admit proof-only quartet decode. | `unescape_string` has `\uXXXX` batching, but REDRESS 49/55 showed decoded direct delivery can regress. The primitive is real; the missing part is a same-loop consumer that beats allocate-then-contiguous-hash or typed field materialization. |
| PTG-FLOAT-CLINGER-EISEL | SRC-CLINGER, SRC-FASTFLOAT, SRC-FNF, SRC-BBNF-PTR, SRC-REDRESS | partial | Keep vendored fast-float lineage, but add fallback telemetry and a no-reparse/fallback decision before claiming closure. | `compute_f64` has Clinger and Eisel-Lemire paths, then `materialize_f64` falls back to `text.parse::<f64>()`. REDRESS 39 says exact-number gap improved but float quality remains residual. |
| PTG-FLOAT-NO-FALLBACK | SRC-FNF, SRC-BBNF-PTR | refuted for current skinny | Do not claim no-fallback parsing until fallback counts are measured or a no-fallback algorithm lands. | The local comments say ambiguous Eisel-Lemire returns `None`, and the caller re-reads the raw span through Rust `parse`. That is correct as a fallback, but it is not the "without fallback" primitive. |
| PTG-INTEGER-SWAR-DOTPROD | SRC-BBNF-PTR, SRC-BBNF-SIMD, SRC-REDRESS | partial | Keep SWAR integer scan in parse-that facade; wire DOTPROD only if a number parser consumes it and moves number-heavy rows. | The current scanner has 8/4/2 digit chunks and span-native i64/u64 materializers. AArch64 UDOT code exists, but no parse-that consumer was found; REDRESS 46 already rejected a local digit probe as a broad close. |
| PTG-DIGEST-SEMANTIC-MIX | SRC-XXHASH, SRC-BBNF-DIGEST, SRC-REDRESS | grounded as bbnf-owned, refuted as generic byte-hash substitution | Do not upstream a generic digest primitive as a semantic sink. Keep bbnf semantic digest scalar-first; optional SIMD mix must prove strict equality and row movement. | Direct rows compare semantic counts, number classes, depth, and fingerprint across Track 1/Track 2/serde/sonic. A fast byte hash like XXH3 can be an implementation detail for string bytes, not the direct output contract. |
| PTG-CSS-SCANNER-GAP | SRC-UPSTREAM-SCAN, SRC-BBNF-RUNTIME, SRC-REDRESS | grounded gap | Use upstream scanner plans or generated scanner templates for CSS rows; avoid hand-growing one scanner loop per CSS feature. | CSS declaration-values currently has manual block/declaration/token loops. Full lightningcss parity needs generated scanners for identifiers, comments, strings, numbers, nesting, and functions under one policy surface. |
| PTG-SAME-WAVE-CONSUMER | SRC-BBNF-SIMD, SRC-REDRESS, SRC-T-P1, SRC-V2-ADDENDUM | grounded | Every imported or vendored primitive needs scalar ref, checkasm/parity where SIMD/ASM exists, and a same-wave generated consumer named in the admission ledger. | REDRESS 122 closed correctness only and REDRESS 126 explicitly withheld production admission despite a 4.718x microbench. This must govern parse-that imports too: primitive presence is not row movement. |

## Architectural Assertions Defended

1. **A real regex/HIR fact source is mandatory for SK-V13 G2, but the
   parse-that import is conditional.** The published regex route and the local
   parse-that regex crate indicate the needed HIR, NFA/DFA, byte equivalence
   classes, regex facts, and egraph hooks. The skinny `parse-that-regex` crate
   exposes none of those planning facts. The local parse-that worktree is only
   conditional authority until its HEAD, dirty state, license, import route, and
   HIR-to-bbnf mapping are closed.

2. **Primitive ownership should be layered.** Regex/HIR/scanner planning belongs
   in imported/snapshotted parse-that facts, established automata crates, or a
   future `bbnf-regex` extraction; aarch64 bodies belong in `bbnf-simd`;
   generated grammar consumers call a narrow grammar-policy facade. This keeps
   Lock 14 grammar policy out of generic intrinsics code and Lock 16 hardware
   gates out of high-level regex facts.

3. **Float parsing is partially solved, not closed.** The local path has Clinger
   and Eisel-Lemire code, integer materializers, and generated/hand consumers,
   but it still has a raw-span fallback and REDRESS records numeric residuals.
   SK-V13 should require fallback-rate telemetry and row-level attribution.

4. **String/unicode work must target output representation, not only scanner
   speed.** REDRESS showed that no-allocation and fused decoded hashing can be
   correctness-green and still lose the direct digest row. The next string route
   must name the field-layout or generated SinkOnly consumer it improves.

5. **Digest primitives are semantic-output primitives.** The direct plane's hash
   is part of the strict oracle contract over parsed JSON facts. A library hash
   may speed a sub-step, but substituting the contract would invalidate the
   comparator rather than beat it.

6. **Compile-time facts and runtime substrates are different objects.**
   `bbnf-regex` may carry HIR, nullability, first sets, byte classes, scanner
   plans, and automata facts into the resolver. It must not retain masks,
   class streams, or cursor state across parser phases. Runtime scanner outputs
   are `local_temp_only` unless they are emitted as an admitted output row.

## Architectural Assertions Refuted

| assertion | refutation | consequence |
|---|---|---|
| "`parse-that-regex` already provides the regex/HIR primitives the spec needs." | Live skinny `parse-that-regex` declares only `integration`, `number`, and `unicode` modules and exports JSON string/number helpers; the HIR/DFA engine is in the separate parse-that worktree. | S-P3 must scope a real extraction/import wave before decision-engine rewrites. |
| "Opaque regex strings in BIR are enough for CSP/egraph/cost selection." | Current `RegexProgram` carries only `pattern: String` and `SpanKind`; upstream parse-that regex carries actual HIR, DFA, byte classes, and regex facts. | Cost-model and egraph rows must consume compiled facts, not parse string patterns repeatedly. |
| "A fast SIMD structural scanner implies bbnf should retain a structural substrate." | simdjson supports SIMD scanning, but bbnf REDRESS 96/97 falsified the retained union-substrate thesis on the M5 Max. | parse-that scanner imports must be transient consumers and cannot reopen W3 by default. |
| "Proof-only SIMD or ASM primitives can close parse-that gaps." | REDRESS 122 and 126 explicitly admit correctness/microbench prerequisites while withholding row admission; T-P1 1D/1E carry same-wave consumer discipline. | Every imported scanner, string, digit, or digest primitive needs a production caller and row movement. |
| "Current float parser is a no-fallback Eisel-Lemire implementation." | `compute_f64` returns `None` on ambiguous rounding and `materialize_f64` falls back to `text.parse::<f64>()`. | The next float wave must either accept measured fallback cost or land a no-fallback implementation. |
| "Generic byte hashing can replace direct digest work." | Direct digest equality depends on JSON semantic shape, number class, depth, string bytes, and Track 1/Track 2/serde/sonic agreement. | Hash libraries are implementation candidates only inside the semantic contract. |
| "The local parse-that worktree can be treated as an admitted dependency because it exists on disk." | V2 pins HEAD `051a6d681da95a180e6b67f956526722d1d33322` but also records dirty/untracked state. | S-P3 may scope an import/snapshot wave, but cannot treat local-path presence as a closed architecture proof. |

## V2 Parse-That Import Boundary

| surface | V2 admissible use | not admissible without new proof |
|---|---|---|
| HIR / regex syntax | Compile-time facts for e-graph/CSP/cost resolver; must map to current bbnf `BackendExpr` or a lock-approved fact side table. | Adding a new BIR variant or generic grammar policy leak just to mirror parse-that internals. |
| NFA/DFA / automata facts | Resolver inputs for scanner selection, first/nullable/follow facts, and cost estimates. | Runtime parser-owned DFA state retained as a public substrate or second tape. |
| Byte classes / equivalence sets | Generated grammar data or caller data that feeds byte-set classify/run-skip primitives. | JSON/CSS hard-coded alphabets in shared parse-that or `bbnf-simd` APIs. |
| Scanner plans | Compile-time or generated-function-local choice of memchr/LUT/SIMD/string/escape scanner. | Persistent mask/class/cursor streams shared across phases. |
| Runtime scanner outputs | Local temporaries consumed in the same generated loop into `existing_tape`, `direct_sink`, or `admitted_fact_output`. | Sidecars, retained scanner caches, or `UnionTape`-like public substrate. |
| String/escape policy | Generated grammar-owned quote/escape/control/terminator/suffix policy structs. | Shared `JsonSink`, JSON flag names, or `\uXXXX`-only semantics in generic APIs. |

`bbnf-regex` extraction is therefore **conditional route-production** until the
following blockers close:

1. parse-that revision is vendored, tagged, or snapshotted from HEAD
   `051a6d681da95a180e6b67f956526722d1d33322`, with dirty/untracked files
   excluded or explicitly imported.
2. License and dependency boundary are recorded in the wave plan.
3. HIR/regex/byte-class facts map to current bbnf IR/egraph/cost structures
   without new directives, BIR variants, or public substrate APIs unless the
   user signs a Lock amendment.
4. Runtime scanner outputs declare `substrate_target`, `retention_lifetime`,
   and `policy_owner`.
5. First production use names a same-wave generated consumer and a row/feature
   gate, not just a crate extraction.

## V2 Admission Ledger Requirements

V3 fold note: the executable parse-that ledger is centralized in
`restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md`. The table below remains
an owner summary; the V3 addendum is authoritative for LOC, risk, rollback,
abrogate thresholds, normalized `admissibility_state`, and separate
`disposition_or_blocker` values.

Every 2F candidate that reaches S-P3 must carry the shared V2 ledger columns:

```text
candidate_id
owner_dossier
source_paths_or_external_source
scalar_reference
checkasm_or_parity_command
BBNF_SIMD_STRICT_status
corpus_or_equality_oracle
hardware_gate
same_wave_consumer_path
expected_row_or_feature_gate
loc_budget
risk_class
rollback_path
abrogate_threshold
admissibility_state
substrate_target
retention_lifetime
policy_owner
```

Minimum 2F ledger rows, with state/disposition normalized by the V3 addendum:

| candidate_id | state / disposition | same_wave_consumer_path | expected gate | substrate_target / lifetime / owner | blocker before admission |
|---|---|---|---|---|---|
| `bbnf_regex_hir_import` | `source_backed`; import/snapshot blocker | `skinny/crates/codegen/src/lower/sink_only.rs` resolver path or generated CSS scanner templates | JSON/CSS equality plus resolver-selected row movement | `local_temp_only` facts at compile time / `generated_function` / `generated_grammar` | parse-that snapshot/license/HIR mapping not complete. |
| `regex_info_to_backendexpr_facts` | `source_backed`; opaque-pattern blocker | e-graph/CSP/cost resolver consuming regex facts | P1-P8 cascade replacement without JSON regression | `local_temp_only` / `generated_function` / `generated_grammar` | opaque `RegexProgram { pattern }` still sole fact source. |
| `scanner_plan_import` | `source_backed`; runtime-substrate blocker | CSS L4 generated scanner loop or JSON string/number scanner loop | strict lightningcss or sonic row movement | runtime `local_temp_only` / `local_loop` / `generated_grammar` | retained mask/class/cursor stream or no row consumer. |
| `string_escape_policy_surface` | `source_backed`; grammar-policy blocker | CSS escaped-ident/string row or JSON unicode direct row | strict equality plus >SOTA row movement | `direct_sink` or `admitted_fact_output` / `generated_function` / `generated_grammar` | JSON-only quote/backslash/control policy leaks. |
| `float_fallback_telemetry` | scalar_backed | JSON/CSS number materializer consumer | fallback-rate TSV and numeric row movement or measured reject | `direct_sink` / `local_loop` / `generated_grammar` | fallback rate and self-time not measured. |
| `digit_dotprod_materializer` | `source_backed`; checkasm/consumer blocker | JSON numeric direct/parse row or CSS number/dimension row | strict equality plus number-heavy row movement | `direct_sink` / `local_loop` / `generated_grammar` | DOTPROD primitive not wired through scalar/checkasm/consumer path. |
| `semantic_digest_simd_mix` | `source_backed`; semantic-contract blocker | `JsonDirectDigest` strict Track 1/Track 2 consumer | sonic strict direct row movement without comparator change | `direct_sink` / `local_loop` / `generated_grammar` | byte-hash substitution or changed semantic contract. |

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| Can local parse-that HEAD `051a6d681da95a180e6b67f956526722d1d33322` be vendored or imported cleanly despite dirty/untracked state? | Inspect Cargo metadata, license, dependency graph, and dirty/untracked paths from `T-P2-V2-FOLD-ADDENDUM.md`; choose vendored snapshot, tag, or no-import before S-P3. |
| Can upstream parse-that HIR map losslessly into skinny `BackendExpr` and `crates/egraph/` without adding a new BIR variant or public substrate API? | Build a read-only prototype map from `Hir`/`RegexInfo` to existing `BackendExpr::RegexProgram` facts and record every required field; any missing field becomes a T-P3 amendment candidate, not an implicit import license. |
| What is the `materialize_f64` fallback rate and self-time on SK-V13 JSON/CSS number corpora? | Add temporary profile counters in a research worktree only, run fresh S-P1 rows, and remove counters before redress; decide no-fallback adoption from TSVs. |
| Does DOTPROD digit parsing move any number-heavy JSON row after codegen/decision-engine changes? | Microbench `digit_mac` against the existing SWAR scan, then wire only in a same-wave number parser consumer with strict equality and row gate. |
| Which CSS L4 features can be expressed through imported regex/HIR scanner plans rather than manual scanner loops? | Feed the SK-V13 CSS parity matrix through upstream parse-that regex scanner classification and record per-feature span scanner requirements. |
| Can a semantic digest SIMD mix beat the current scalar `mix` without changing oracle output? | Write a scalar reference over `JsonDirectDigest`, add a SIMD candidate behind checkasm/parity, and benchmark direct rows before any substitution. |
| Can runtime scanner outputs stay purely local after scanner-plan import? | For each candidate scanner plan, declare `substrate_target`, `retention_lifetime`, and `policy_owner`; any retained mask/class/cursor stream is REVISE unless separately admitted under Lock 1. |

## LOCKS-AMENDMENTS-CANDIDATE

| Candidate | Type | Lock(s) | Proposed amendment candidate | Supporting evidence |
|---|---|---|---|---|
| LAC-2F-01 | refinement | Lock 14 | Generic parse-that/regex APIs must expose grammar-neutral facts (`Hir`, byte classes, scanner plans, number/string policy structs); grammar-named JSON/CSS helper APIs are allowed only in generated grammar modules or grammar-local facades. | Skinny `parse-that-regex` is JSON-shaped; T-P1 1B/1C/1E report grammar-name and grammar-shape leaks; upstream parse-that regex is conditional authority until revision/license/import/HIR mapping closes. |
| LAC-2F-02 | addition | Lock 16 | Every parse-that primitive that calls `bbnf-simd`, intrinsics, or `asm!` must trace to `{scalar reference, parity/checkasm, hardware gate, same-wave consumer, measured row}`; a parse-that facade cannot hide an undocumented SIMD loop. | `string_block` and `digit_mac` are valid bodies, but REDRESS 122/126 show correctness/microbench-only status is not admission. |
| LAC-2F-03 | addition | Lock 10 / decision engine | Regex/HIR facts are mandatory inputs to CSP/egraph/cost selection; opaque pattern strings are not sufficient for backend-shape or scanner selection. | `SinkOnlyExpr::RegexProgram` currently stores only pattern text, while upstream parse-that regex exposes HIR, DFA, byte classes, and regex info. |
| LAC-2F-04 | refinement | SOTA/direct gate | Direct digest hashing is a semantic-output contract. Byte-hash libraries may optimize string sub-hashes only when Track 1/Track 2/serde/sonic strict equality and prior A/GO rows hold. | `JsonDirectDigest` includes semantic counters and shape checks; REDRESS 119/120 are history-only under SK-V13 and cannot close by changing comparator meaning. |
