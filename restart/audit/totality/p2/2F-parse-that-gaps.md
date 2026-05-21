---
agent: 2F
pass: T-P2-research
cycle: V1
generated_at: 2026-05-21T04:42:44-04:00
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 19
techniques_grounded: 13
techniques_refuted: 6
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: []
  first_cycle_additions: [PTG-REGEX-HIR-ENGINE, PTG-REGEX-INFO-FACTS, PTG-SIMD-SPAN-SCAN, PTG-STRING-SCAN-UTF8, PTG-UNICODE-ESCAPE-CODEC, PTG-FLOAT-CLINGER-EISEL, PTG-INTEGER-SWAR-DOTPROD, PTG-DIGEST-SEMANTIC-MIX, PTG-CSS-SCANNER-GAP]
locks_amendment_candidates: 4
---

## Executive Summary

The live skinny crate named `parse-that-regex` is not a regex/HIR engine. It
exports JSON-oriented string, number, unicode, and a minimal SIMD hook surface
(`skinny/crates/parse-that-regex/src/lib.rs:4-8`), while the separate
`parse-that` worktree contains the actual HIR, NFA, DFA, byte-class, regex-info,
and span-scanner machinery (`/Users/mkbabb/Programming/parse-that/rust/regex/src/lib.rs:1-38`).
SK-V13's decision-engine fold therefore depends on importing/upstreaming that
regex crate as `bbnf-regex`, not on extending the skinny JSON facade by hand.
The primitive split should be explicit: regex/HIR analysis and scanner-plan
selection come from upstream parse-that or established automata libraries;
low-level aarch64 kernels remain in `bbnf-simd` under Lock 16; generated
grammar consumers call a thin parse-that facade. Float and integer
materializers are partially present, but fallback telemetry, DOTPROD wiring, and
same-wave row consumers are missing. Digest primitives are bbnf semantic-output
primitives, not generic byte-hash substitutions. CSS L4 currently uses a
hand-coded scanner, so full parity expansion needs generated scanner plans, not
one-off feature loops.

## Source Registry

| ID | Primary source | Use in this dossier |
|---|---|---|
| SRC-COX-REGEX | Russ Cox, "Regular Expression Matching Can Be Simple And Fast" ([swtch.com](https://swtch.com/~rsc/regexp/regexp1.html)) | Thompson-NFA / automata route; refutes backtracking or ad hoc regex parsing as the totality baseline. |
| SRC-RE2 | Google RE2 source/README ([github.com/google/re2](https://github.com/google/re2)) | Production finite-automata regex discipline: linear-time, bounded memory, parser/compiler/engine separation. |
| SRC-RUST-REGEX | Rust `regex`, `regex-automata`, and `regex-syntax` source ([github.com/rust-lang/regex](https://github.com/rust-lang/regex)) | Established Rust HIR/NFA/DFA building blocks and the alternative to a bespoke skinny-only engine. |
| SRC-MEMCHR | BurntSushi `memchr` source ([github.com/BurntSushi/memchr](https://github.com/BurntSushi/memchr)) | Primary Rust source for byte-search primitives used by upstream parse-that span scanners. |
| SRC-FASTFLOAT | `fast_float` official implementation ([github.com/fastfloat/fast_float](https://github.com/fastfloat/fast_float)) | Fast exact decimal-to-binary float implementation lineage. |
| SRC-FNF | Noble Mushtak and Daniel Lemire, "Fast Number Parsing Without Fallback" ([arXiv 2212.06644](https://arxiv.org/abs/2212.06644)) | Refines Eisel-Lemire fallback risk; bbnf should measure fallback rather than assume no fallback. |
| SRC-CLINGER | William D. Clinger, "How to Read Floating-Point Numbers Accurately", PLDI 1990, DOI 10.1145/93542.93557 ([doi.org](https://doi.org/10.1145/93542.93557)) | Correct-rounding basis for Clinger-style fast path and exact fallback discipline. |
| SRC-SIMDJSON-PAPER | Geoff Langdale and Daniel Lemire, "Parsing Gigabytes of JSON per Second", VLDB Journal 2019 ([arXiv 1902.08318](https://arxiv.org/abs/1902.08318)) | SIMD string/structural-scan architecture and the distinction between scanner speed and consumer admission. |
| SRC-SIMDJSON-SRC | simdjson source/README ([github.com/simdjson/simdjson](https://github.com/simdjson/simdjson)) | Production source for validated SIMD JSON parsing, UTF-8, and lossless number claims. |
| SRC-UTF8 | John Keiser and Daniel Lemire, "Validating UTF-8 In Less Than One Instruction Per Byte" ([arXiv 2010.03090](https://arxiv.org/abs/2010.03090)) | SIMD UTF-8 validation route; refutes scalar-only UTF-8 as the only defensible primitive. |
| SRC-XXHASH | xxHash official source/README ([github.com/Cyan4973/xxHash](https://github.com/Cyan4973/xxHash)) | Useful byte-hash baseline; explicitly not a replacement for bbnf semantic direct digest. |
| SRC-BBNF-PTR | `skinny/crates/parse-that-regex/src/lib.rs:4-8`, `:157-209`, `:547-573`, `:718-840`; `skinny/crates/parse-that-regex/src/number/mod.rs:31-272`; `skinny/crates/parse-that-regex/src/number/eisel_lemire/mod.rs:1-177` | Live skinny parse-that facade: string, unicode, number, and fallback behavior. |
| SRC-UPSTREAM-REGEX | `/Users/mkbabb/Programming/parse-that/rust/regex/src/lib.rs:1-38`; `/Users/mkbabb/Programming/parse-that/rust/regex/src/hir/mod.rs:1-277`; `/Users/mkbabb/Programming/parse-that/rust/regex/src/automata/dfa.rs:1-170` | Local upstream regex/HIR/NFA/DFA engine not present in skinny `parse-that-regex`. |
| SRC-UPSTREAM-SCAN | `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/span_parser/scan.rs:1-183`; `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/span_parser/span_scanner.rs:1-42` | Local upstream span scanners: memchr1/2/3, LUT, portable SIMD, and monolithic grammar-neutral scanner enum. |
| SRC-BBNF-SIMD | `skinny/crates/parse-that-regex/src/integration/simd_scan_hook.rs:1-19`; `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:30-71`; `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:1-71` | Current SIMD hook plus string and DOTPROD primitive bodies. |
| SRC-BBNF-CODEGEN | `skinny/crates/codegen/src/lower/sink_only.rs:19-93`, `:142-181`; `skinny/crates/codegen/src/json_sink_direct.rs:34-45`, `:315-370` | BIR `RegexProgram` is an opaque string and the JSON direct renderer requires only String/Number/Whitespace spans. |
| SRC-BBNF-RUNTIME | `skinny/crates/runtime/src/grammars/json/generated.rs:10-15`, `:393-421`, `:608-653`; `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:38-164`, `:211-240` | Generated JSON direct consumers and current hand-coded CSS scanner. |
| SRC-BBNF-DIGEST | `skinny/crates/bbnf-bench/src/direct_struct.rs:15-29`, `:58-105`, `:401-425`, `:716-742` | Direct semantic digest shape and strict Track 1/Track 2/serde/sonic parity consumer. |
| SRC-REDRESS | `skinny/REDRESS.md:517-557`, `:633-649`, `:700-713`, `:846-882`, `:2910-2940`, `:3495-3528`, `:3603-3633`, `:3780-3805` | Measured local dispositions: Eisel-Lemire, SinkOnly, digit scan, string/unicode rejections, union falsification, direct fixpoint, Lock 16 prerequisite, ASCII run-skip micro-proof. |
| SRC-T-P1 | `restart/audit/totality/p1/1B-codegen-evidence.md`, `1C-runtime-evidence.md`, `1D-skinny-lessons.md`, `1E-locks-evidence.md`, `1F-past-corpora.md` | Current totality evidence: Lock 14 drift, REDRESS-119/120 history-only framing, same-wave consumer requirements. |

## Technique Grounding Table

| spec claim / T-P1 divergence id | published source cited | grounded / refuted / partial | upstream-or-vendor decision | bbnf-specific note |
|---|---|---|---|---|
| PTG-REGEX-HIR-ENGINE | SRC-COX-REGEX, SRC-RE2, SRC-RUST-REGEX, SRC-UPSTREAM-REGEX | grounded | Upstream/import `/Users/mkbabb/Programming/parse-that/rust/regex` as the `bbnf-regex` surface; do not grow HIR/NFA/DFA inside skinny `parse-that-regex`. | SK-V13 G2 needs a real regex/HIR language for rewrites and scanner selection. Live skinny only exposes `integration`, `number`, and `unicode`, plus string functions, so `parse-that-regex` cannot satisfy G2 by name alone. |
| PTG-REGEX-INFO-FACTS | SRC-RUST-REGEX, SRC-UPSTREAM-REGEX | grounded | Upstream parse-that regex info/classification facts, then adapt to bbnf `BackendExpr` and egraph types. | The upstream crate re-exports `RegexInfo`, `EngineSet`, first-character facts, byte sets, DFA options, and egraph modules; bbnf needs these as cost-model facts, not as runtime parser calls. |
| PTG-REGEX-OPAQUE-PATTERN | SRC-BBNF-CODEGEN, SRC-UPSTREAM-REGEX | refuted | Replace opaque `RegexProgram { pattern: String }` planning with compiled HIR/facts before S-P3 wave-scoping. | `SinkOnlyExpr::RegexProgram` currently stores a string and span kind only. That cannot drive CSP/egraph/cost rewrites, byte-class scanner selection, or CSS parity feature coverage. |
| PTG-SIMD-SPAN-SCAN | SRC-MEMCHR, SRC-UPSTREAM-SCAN, SRC-BBNF-SIMD | partial | Upstream/import scanner-plan selection; keep hardware-specific aarch64 bodies in `bbnf-simd` with checkasm. | Upstream parse-that already routes memchr1/2/3, LUT, and portable SIMD scans. Skinny has only a `SimdScannerHook` trait and separate aarch64 primitives, so it lacks the planner that decides which scanner fits a grammar span. |
| PTG-RETAINED-STRUCTURAL-SUBSTRATE | SRC-SIMDJSON-PAPER, SRC-REDRESS | refuted as a default bbnf route | Do not upstream a retained structural-index substrate into parse-that. Allow transient scanner consumers only. | simdjson validates SIMD scanning as a production parser design, but bbnf REDRESS 96/97 measured two faithful retained-union variants as regressions on this host. A parse-that scanner plan must be transient and row-consumed, not a sidecar substrate. |
| PTG-STRING-SCAN-UTF8 | SRC-SIMDJSON-PAPER, SRC-SIMDJSON-SRC, SRC-UTF8, SRC-BBNF-PTR, SRC-BBNF-SIMD | partial | Vendor grammar-neutral string scanner parameters through bbnf facade; keep aarch64 string blocks in `bbnf-simd`. | Live skinny string matching is JSON-shaped: double quote, backslash, control limit, trusted UTF-8. CSS and Sheets need delimiter/escape/control policy from GrammarConfig, plus scalar reference/checkasm for any SIMD block. |
| PTG-UNICODE-ESCAPE-CODEC | SRC-BBNF-PTR, SRC-REDRESS | partial / prior consumer shapes refuted | Keep the scalar + aarch64 unicode escape codec only with a row-moving generated consumer; do not admit proof-only quartet decode. | `unescape_string` has `\uXXXX` batching, but REDRESS 49/55 showed decoded direct delivery can regress. The primitive is real; the missing part is a same-loop consumer that beats allocate-then-contiguous-hash or typed field materialization. |
| PTG-FLOAT-CLINGER-EISEL | SRC-CLINGER, SRC-FASTFLOAT, SRC-FNF, SRC-BBNF-PTR, SRC-REDRESS | partial | Keep vendored fast-float lineage, but add fallback telemetry and a no-reparse/fallback decision before claiming closure. | `compute_f64` has Clinger and Eisel-Lemire paths, then `materialize_f64` falls back to `text.parse::<f64>()`. REDRESS 39 says exact-number gap improved but float quality remains residual. |
| PTG-FLOAT-NO-FALLBACK | SRC-FNF, SRC-BBNF-PTR | refuted for current skinny | Do not claim no-fallback parsing until fallback counts are measured or a no-fallback algorithm lands. | The local comments say ambiguous Eisel-Lemire returns `None`, and the caller re-reads the raw span through Rust `parse`. That is correct as a fallback, but it is not the "without fallback" primitive. |
| PTG-INTEGER-SWAR-DOTPROD | SRC-BBNF-PTR, SRC-BBNF-SIMD, SRC-REDRESS | partial | Keep SWAR integer scan in parse-that facade; wire DOTPROD only if a number parser consumes it and moves number-heavy rows. | The current scanner has 8/4/2 digit chunks and span-native i64/u64 materializers. AArch64 UDOT code exists, but no parse-that consumer was found; REDRESS 46 already rejected a local digit probe as a broad close. |
| PTG-DIGEST-SEMANTIC-MIX | SRC-XXHASH, SRC-BBNF-DIGEST, SRC-REDRESS | grounded as bbnf-owned, refuted as generic byte-hash substitution | Do not upstream a generic digest primitive as a semantic sink. Keep bbnf semantic digest scalar-first; optional SIMD mix must prove strict equality and row movement. | Direct rows compare semantic counts, number classes, depth, and fingerprint across Track 1/Track 2/serde/sonic. A fast byte hash like XXH3 can be an implementation detail for string bytes, not the direct output contract. |
| PTG-CSS-SCANNER-GAP | SRC-UPSTREAM-SCAN, SRC-BBNF-RUNTIME, SRC-REDRESS | grounded gap | Use upstream scanner plans or generated scanner templates for CSS rows; avoid hand-growing one scanner loop per CSS feature. | CSS declaration-values currently has manual block/declaration/token loops. Full lightningcss parity needs generated scanners for identifiers, comments, strings, numbers, nesting, and functions under one policy surface. |
| PTG-SAME-WAVE-CONSUMER | SRC-BBNF-SIMD, SRC-REDRESS, SRC-T-P1 | grounded | Every imported or vendored primitive needs scalar ref, checkasm/parity where SIMD/ASM exists, and a same-wave generated consumer. | REDRESS 122 closed correctness only and REDRESS 126 explicitly withheld production admission despite a 4.718x microbench. This must govern parse-that imports too: primitive presence is not row movement. |

## Architectural Assertions Defended

1. **`bbnf-regex` extraction is mandatory for SK-V13 G2.** The published regex
   route and the local upstream parse-that regex crate both expose HIR, NFA/DFA,
   byte equivalence classes, regex facts, and egraph tests. The skinny
   `parse-that-regex` crate exposes none of those planning facts.

2. **Primitive ownership should be layered.** Regex/HIR/scanner planning belongs
   in upstream parse-that or a `bbnf-regex` extraction; aarch64 bodies belong in
   `bbnf-simd`; generated grammar consumers call a narrow parse-that facade.
   This keeps Lock 14 grammar policy out of generic intrinsics code and Lock 16
   hardware gates out of high-level regex facts.

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

## Architectural Assertions Refuted

| assertion | refutation | consequence |
|---|---|---|
| "`parse-that-regex` already provides the regex/HIR primitives the spec needs." | Live skinny `parse-that-regex` declares only `integration`, `number`, and `unicode` modules and exports JSON string/number helpers; the HIR/DFA engine is in the separate parse-that worktree. | S-P3 must scope a real extraction/import wave before decision-engine rewrites. |
| "Opaque regex strings in BIR are enough for CSP/egraph/cost selection." | Current `RegexProgram` carries only `pattern: String` and `SpanKind`; upstream parse-that regex carries actual HIR, DFA, byte classes, and regex facts. | Cost-model and egraph rows must consume compiled facts, not parse string patterns repeatedly. |
| "A fast SIMD structural scanner implies bbnf should retain a structural substrate." | simdjson supports SIMD scanning, but bbnf REDRESS 96/97 falsified the retained union-substrate thesis on the M5 Max. | parse-that scanner imports must be transient consumers and cannot reopen W3 by default. |
| "Proof-only SIMD or ASM primitives can close parse-that gaps." | REDRESS 122 and 126 explicitly admit correctness/microbench prerequisites while withholding row admission; T-P1 1D/1E carry same-wave consumer discipline. | Every imported scanner, string, digit, or digest primitive needs a production caller and row movement. |
| "Current float parser is a no-fallback Eisel-Lemire implementation." | `compute_f64` returns `None` on ambiguous rounding and `materialize_f64` falls back to `text.parse::<f64>()`. | The next float wave must either accept measured fallback cost or land a no-fallback implementation. |
| "Generic byte hashing can replace direct digest work." | Direct digest equality depends on JSON semantic shape, number class, depth, string bytes, and Track 1/Track 2/serde/sonic agreement. | Hash libraries are implementation candidates only inside the semantic contract. |

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| Which exact upstream parse-that regex revision should be imported or vendored into skinny as `bbnf-regex`? | Capture `git -C /Users/mkbabb/Programming/parse-that rev-parse HEAD`, inspect its Cargo metadata/license, and decide crates.io dependency vs vendored workspace crate before S-P3. |
| Can upstream parse-that HIR map losslessly into skinny `BackendExpr` and `crates/egraph/` without adding a new BIR variant? | Build a read-only prototype map from `Hir`/`RegexInfo` to existing `BackendExpr::RegexProgram` facts and record every required field; any missing field becomes a T-P3 amendment candidate. |
| What is the `materialize_f64` fallback rate and self-time on SK-V13 JSON/CSS number corpora? | Add temporary profile counters in a research worktree only, run fresh S-P1 rows, and remove counters before redress; decide no-fallback adoption from TSVs. |
| Does DOTPROD digit parsing move any number-heavy JSON row after codegen/decision-engine changes? | Microbench `digit_mac` against the existing SWAR scan, then wire only in a same-wave number parser consumer with strict equality and row gate. |
| Which CSS L4 features can be expressed through imported regex/HIR scanner plans rather than manual scanner loops? | Feed the SK-V13 CSS parity matrix through upstream parse-that regex scanner classification and record per-feature span scanner requirements. |
| Can a semantic digest SIMD mix beat the current scalar `mix` without changing oracle output? | Write a scalar reference over `JsonDirectDigest`, add a SIMD candidate behind checkasm/parity, and benchmark direct rows before any substitution. |

## LOCKS-AMENDMENTS-CANDIDATE

| Candidate | Type | Lock(s) | Proposed amendment candidate | Supporting evidence |
|---|---|---|---|---|
| LAC-2F-01 | refinement | Lock 14 | Generic parse-that/regex APIs must expose grammar-neutral facts (`Hir`, byte classes, scanner plans, number/string policy structs); grammar-named JSON/CSS helper APIs are allowed only in generated grammar modules or grammar-local facades. | Skinny `parse-that-regex` is JSON-shaped; T-P1 1B/1C/1E report grammar-name and grammar-shape leaks; upstream parse-that regex is grammar-neutral by construction. |
| LAC-2F-02 | addition | Lock 16 | Every parse-that primitive that calls `bbnf-simd`, intrinsics, or `asm!` must trace to `{scalar reference, parity/checkasm, hardware gate, same-wave consumer, measured row}`; a parse-that facade cannot hide an undocumented SIMD loop. | `string_block` and `digit_mac` are valid bodies, but REDRESS 122/126 show correctness/microbench-only status is not admission. |
| LAC-2F-03 | addition | Lock 10 / decision engine | Regex/HIR facts are mandatory inputs to CSP/egraph/cost selection; opaque pattern strings are not sufficient for backend-shape or scanner selection. | `SinkOnlyExpr::RegexProgram` currently stores only pattern text, while upstream parse-that regex exposes HIR, DFA, byte classes, and regex info. |
| LAC-2F-04 | refinement | SOTA/direct gate | Direct digest hashing is a semantic-output contract. Byte-hash libraries may optimize string sub-hashes only when Track 1/Track 2/serde/sonic strict equality and prior A/GO rows hold. | `JsonDirectDigest` includes semantic counters and shape checks; REDRESS 119/120 are history-only under SK-V13 and cannot close by changing comparator meaning. |
