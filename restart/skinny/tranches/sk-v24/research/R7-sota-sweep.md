# R7 — SOTA SWEEP (2025–2026): grammar→Rust+NEON parser generator on Apple Silicon

Recon cartographer product for SK-V24. Every external claim carries a URL; every "steal THIS
campaign" line is keyed to PROVE-01..14. Local reuse assets verified by `ls`/`rg`, cited
`path:line`. Read-only sweep; no builds run.

---

## 0. Settled question — Apple M-series vector ISA (binds F1/F3/PROVE-01..05, -13)

**NEON is the only practical SIMD plane on M-series today. Do not spec SVE/SVE2/SME for
byte classification.** Concrete findings:

- M4 CPU cores **do not support non-streaming SVE/SVE2**. Running a non-streaming SVE
  instruction faults `EXC_BAD_INSTRUCTION`. SVE runs *only* inside streaming (SSVE) mode.
  [tzakharko/m4-sme-exploration §01](https://github.com/tzakharko/m4-sme-exploration/blob/main/reports/01-sme-overview.md)
- **SME exists only in streaming mode** (SSVE), streaming vector length **512 bits** (64 B
  reg, 4096 B ZA tile). Base SVE FMLA in streaming mode = "disappointing 31 GFLOPs FP32,"
  *below* a single NEON P-core; only the ZA-tile SME-specific FMLA hits 250 GFLOPs — a matmul
  path, no byte/classification benefit, heavy mode-switch cost.
  [same](https://github.com/tzakharko/m4-sme-exploration/blob/main/reports/01-sme-overview.md);
  [HN](https://news.ycombinator.com/item?id=41501494);
  [zenn.dev SME writeup](https://zenn.dev/mod_poppo/articles/arm-scalable-matrix-extension?locale=en)
- **STEAL:** lock the cost model to 128-bit NEON kernels (16-B lanes). SME/SVE = explicitly
  out of scope in the SPEC; no `target_feature` gate beyond `neon`. Confirms the existing
  `crates/simd-scan/src/neon.rs` 16-B-lane discipline is the right and only substrate for
  PROVE-01/04/05/13.

---

## 1. simdjson On-Demand + NEON structural indexing (F1 · PROVE-01, -04, -06)

- On-Demand model (Keiser & Lemire, *SP&E* 54(6), 2024): a lazy iterator that materializes
  values on access rather than building a full DOM — the reference design for PROVE-06's
  stop-early/lazy path.
  [arXiv 2312.17149](https://arxiv.org/pdf/2312.17149)
- Structural indexing: read **64-B blocks**, emit a 64-bit bitset marking structural chars
  `[ ] { } : ,` + value starts; each 64-B block yields 0–32 index entries. Stage 1
  (index) / stage 2 (materialize) split.
  [DeepWiki simdjson](https://deepwiki.com/simdjson/simdjson);
  [github/simdjson](https://github.com/simdjson/simdjson)
- NEON: no native movemask → the community idiom is `vshrn_n_u16 #4` narrowing a 16×u8
  compare-mask to a 64-bit lane (8-B per 16-B chunk = 4 bits/byte). Already implemented
  locally — `crates/simd-scan/src/neon.rs:683-712` (`movemask_u8x16` via `vshrn_n_u16 #4`).
- **STEAL:** PROVE-04 (multi-block 2×64B pipelined quote/structural scan) is a *direct*
  transcription of simdjson stage-1 onto the existing NEON stripe scanner
  (`scan_multi` at `crates/simd-scan/src/neon.rs:362`, `classify_stripe_multi:435`,
  `raw_quotes_and_bs_stripe:502`). PROVE-06 lazy path = On-Demand iterator over the tape.

## 2. Lemire ARM 2025 — HTML/DNS scanning at tens of GB/s (F1 · PROVE-01, -04, -05, -13)

- "Scanning HTML at Tens of Gigabytes Per Second on ARM" (Lemire, *SP&E* 2025): **20× over
  scalar** via vectorized byte classification on ARM NEON; two browser engines (WebKit,
  Blink) shipped SIMD HTML scan.
  [Wiley spe.3420](https://onlinelibrary.wiley.com/doi/10.1002/spe.3420)
- simdzone / "Parsing Millions of DNS Records per Second" (Koekkoek, *SP&E* 2025): the same
  2-stage index(64-B block, vectorized classify)→parse skeleton generalized *beyond JSON* —
  the template for PROVE-13's grammar-agnostic generality proof.
  [Wiley spe.3396](https://onlinelibrary.wiley.com/doi/full/10.1002/spe.3396);
  [arXiv 2411.12035](https://arxiv.org/pdf/2411.12035)
- A public NEON CSV parser (64-B/pass, simdjson-derived) already exists — direct comparator +
  idiom source for PROVE-13 (CSV grammar through the one generator).
  [letsdatascience CSV SIMD](https://letsdatascience.com/news/developer-builds-simd-csv-parser-processing-64-bytes-4f87a52d)
- **STEAL:** the generalization thesis is the load-bearing argument for F1/F3+PROVE-13 — the
  same block-classify kernel drives HTML, DNS, CSV, JSON, CSS with only the alphabet LUT
  swapped (mirrors `crates/simd-scan/src/alphabet.rs::StructuralAlphabet`). No grammar-name
  gates — one kernel schedule, per-grammar LUT.

## 3. Hyperscan prefilter lineage — shufti / truffle / teddy (F1/F5 · PROVE-01, -10)

- Hyperscan (NSDI '19) decomposes regex into string + FA components; SIMD "fast path" front
  end finds candidates, slow back end confirms.
  [USENIX nsdi19-wang](https://www.usenix.org/system/files/nsdi19-wang-xiang.pdf)
- **shufti**: `pshufb`/`vqtbl` low+high nibble LUT AND → byte-class membership in ≤4 ops
  (exactly the local `classify_chunk_nibble`, `crates/simd-scan/src/neon.rs:208-231`, two
  `vqtbl1q_u8` + AND). **truffle**: same shape, extended to arbitrary 256-char sets via a
  second table (local `scan_wide`/`classify_chunk_wide:335`, four `vqtbl1q_u8`). **teddy**:
  SIMD bucketed multi-literal matcher, 2–72 literals, front-end candidates + verify.
  [Teddy ICPP21](https://oaciss.uoregon.edu/icpp21/views/includes/files/pap205s4-file2.pdf);
  [Langdale Suricon16](https://suricon.net/wp-content/uploads/2016/11/SuriCon2016_GeoffLangdale.pdf)
- **STEAL:** PROVE-01 (shufti/truffle prefilter into scan dispatch, fused GATE=16) *is*
  productionizing the already-vendored shufti(nibble)/truffle(wide) kernels behind the cost
  model. Teddy = the multi-keyword tier for PROVE-10's prefilter ladder.

## 4. rust regex-automata — lazy/hybrid DFA + prefilter tiers (F5 · PROVE-10)

- Meta engine picks among: literal/prefilter → **lazy (hybrid) DFA** → one-pass DFA →
  bounded backtracker → PikeVM. Lazy DFA builds states *during* search under a configurable
  memory budget; on cache-exhaustion or a **quit byte** it returns `MatchError` and the meta
  engine falls back (never a wrong answer).
  [burntsushi regex-internals](https://burntsushi.net/regex-internals/);
  [docs.rs regex_automata hybrid](https://docs.rs/regex-automata/latest/regex_automata/hybrid/index.html)
- LazyStateID reserves tag bits (unknown/dead/quit/start/match) — the encoding to copy for a
  compact state cache.
  [LazyStateID](https://shadow.github.io/docs/rust/regex_automata/hybrid/struct.LazyStateID.html)
- Prefilter tiers: memchr/Two-Way (single, rare-byte-ranked) → Teddy (multi, SIMD) →
  Aho-Corasick (tens of thousands). Prefilter runs on start-state entry; a hit triggers an
  *anchored* full-engine confirm at that offset only. Literal extraction yields exact
  (skip-engine) vs inexact (confirm) literals, order-preserving for leftmost-first.
  [burntsushi regex-internals](https://burntsushi.net/regex-internals/)
- **STEAL:** PROVE-10's "4096-state bail + prefilter tiers" = the lazy-DFA-with-budget +
  three-tier prefilter design verbatim, retargeted onto parse-that's own DFA. Local targets
  exist: `parse-that/rust/regex/src/automata/dfa.rs`, `.../accel.rs` (acceleration
  detection), `.../nfa.rs`. Cache-exhaust → bail to NFA/backtracker is the local isomorph of
  the meta-engine fallback. Contrast baseline = the `regex` crate itself.

## 5. sonic-rs — bitmap parsing + direct-to-struct (F1/F4 · PROVE-02, -03, -06, -07)

- Not simdjson's 2-stage tape: sonic-rs parses **directly into the Rust struct, no temporary
  tape** — the PROVE-07 thesis (beat serde_json + the tape hop).
  [github cloudwego/sonic-rs](https://github.com/cloudwego/sonic-rs)
- Techniques (from `docs/performance.md`): branchless in-string bitmap
  (`get_escaped_branchless_u64`, simdjson-derived) → **XOR in-string mask with bracket mask**
  to recover true structurals; whitespace skip caches the non-space bitmap per 64-B window
  (`skip_space`, no recompute); **container skip by bracket counting** (JSONSki,
  `skip_container_loop`) — skip whole objects/arrays without byte walk; `get_from_str` lazy
  pointer path; numbers: `simd_str2int` for ≤16-char runs, switch-table to cut SIMD ops for
  ≤17 sig-digit f64.
  [sonic-rs performance.md](https://github.com/cloudwego/sonic-rs/blob/main/docs/performance.md)
- **STEAL:** PROVE-06 lazy get = mirror `get_from_str` over the local tape/view. PROVE-07 =
  emit sonic-style struct-filler from the index (skip tape). Container-skip-by-count is a
  free stop-early win for PROVE-06 partial decode. Baseline gate for PROVE-06/-07 is
  literally sonic-rs `get_from_str` / typed decode.

## 6. Numbers — Eisel–Lemire + SIMD digit runs (F1 · PROVE-02, the canada redress)

- Eisel–Lemire `ParseNumberF64`: optimistic fast path (~99% of JSON numbers), exact
  fallback to strtod-class on the hard 1% — ~80 LOC + powers-of-10 table. fast_float hits
  ~1.2 GB/s, near 1.5 GB/s on M1; canada dataset 1200 MiB/s vs Rust std 130 MiB/s (~10×).
  [Lemire number-parsing GB/s](https://lemire.me/blog/2021/01/29/number-parsing-at-a-gigabyte-per-second/);
  [Nigel Tao Eisel-Lemire](https://nigeltao.github.io/blog/2020/eisel-lemire.html);
  [fast_float practice](https://lemire.me/blog/2020/03/10/fast-float-parsing-in-practice/)
- SIMD digit-run: parse an 8/16-byte digit block by subtract-`'0'` + validate + Horner via
  packed mul-add (Mula/Lemire `simd_str2int` idiom) to feed the Eisel–Lemire mantissa.
- **STEAL:** PROVE-02 is exactly "SIMD digit-run → Eisel–Lemire" to close the canada loss vs
  sonic-rs. Local seam to extend: `crates/simd-scan/src/parity.rs` already references number
  parse; `crates/core/src/lower/value_expr/literals.rs` and `atom.rs` are the emit sites for
  number leaves.

## 7. UTF-8 validation — lookup algorithm, fused with unescape (F1 · PROVE-03)

- Keiser–Lemire "Validating UTF-8 in Less Than One Instruction Per Byte" (2020, arXiv
  2010.03090): shuffle-LUT range check, **>10× faster**, <1 cycle/byte, portable to
  NEON/SSE (`lemire/fastvalidate-utf-8`, range algorithm `cyb70289/utf8` NEON+SSE4+AVX2).
  [arXiv 2010.03090](https://arxiv.org/abs/2010.03090);
  [fastvalidate-utf-8](https://github.com/lemire/fastvalidate-utf-8);
  [cyb70289/utf8 NEON](https://github.com/cyb70289/utf8)
- **STEAL:** PROVE-03 (SIMD string unescape + fused UTF-8 validation) = run the lookup-LUT
  validator in the same pass as the `\`-escape "copy-and-find" bulk copy (sonic-rs
  `escaped_mask` idiom). Extend the local quote scanner
  (`crates/simd-scan/src/neon.rs:502` `raw_quotes_and_bs_stripe`, `:539` `quote_stripe_masked`)
  to also emit the continuation-byte class. twitter-strings is the gate corpus.

## 8. lightningcss internals (F1/F3 · PROVE-05)

- Built on Mozilla `cssparser` + `selectors` (Servo/Firefox lineage). Perf levers: **parse
  values by the CSS-spec grammar into typed value structs** (not generic tokens), **minimize
  AST passes**, memory-frugal. Benches: Bootstrap 4 4.16 ms, Animate.css 1.97 ms,
  **Tailwind 43.4 ms** (vs esbuild 107.7 ms).
  [github parcel-bundler/lightningcss](https://github.com/parcel-bundler/lightningcss);
  [lightningcss.dev](https://lightningcss.dev/)
- **STEAL:** PROVE-05 target band (2.07–3.47× vs lightningcss) is beatable because
  lightningcss has **no SIMD tokenizer** — it is a scalar hand-written lexer. A `vqtbl` CSS
  token-class kernel (ident/number/delim/string classes via nibble-LUT, reusing
  `classify_chunk_nibble`) is the wedge. Typed-value-per-property is the parity bar
  (preserve-rich-AST). Tailwind is the stress corpus. Baseline `crates/core/src/grammar/
  generated/css_l4.rs` + `css_pretty.rs` already exist as the parity oracle.

## 9. yyjson — allocation discipline (F4 · PROVE-06, -07, -14)

- Gigabytes/s pure-C. Levers: **arena allocator** (else per-node malloc collapses on big
  input), **in-place parse** in the input buffer (no string copy), twitter.json = **3
  allocs / 19 MB** vs Foundation 6600 allocs / 378 MB.
  [github ibireme/yyjson](https://github.com/ibireme/yyjson);
  [yyjson doxygen](https://ibireme.github.io/yyjson/doc/doxygen/html/)
- **STEAL:** the alloc-count metric (3 allocs on twitter) is the honest gate for PROVE-06/-07
  and PROVE-14 tape unification — a flat arena/tape decoded in-place, zero per-node alloc, is
  the layout to make identical across Rust + TS typed arrays.

## 10. serde-free direct-to-struct (F2/F4 · PROVE-07, -08)

- serde_json zero-copy: `&str`/`Cow<str>` fields borrow the input (only `from_str`, not
  `from_reader`); halves allocations on small-doc floods; `Cow` borrows unescaped, allocates
  only when an escape forces it. Established but **still tape-then-visitor** — the hop
  sonic-rs/PROVE-07 removes.
  [rust-lang forum zero-copy](https://users.rust-lang.org/t/zero-copy-deserialize-arbitrary-json-data-with-serde/96324);
  [MS Rust patterns ch11](https://microsoft.github.io/RustTraining/rust-patterns-book/ch11-serialization-zero-copy-and-binary-data.html)
- **STEAL:** PROVE-07 emits a generated struct-filler straight off the index/tape (skip
  serde's Visitor dispatch); PROVE-08 proves the HM+CSP `->` inference yields the typed
  signature that *names* those struct fields at zero runtime cost. Baseline: serde_json +
  sonic-rs typed decode.

## 11. V8 uplift for the TS combinator (F5 · PROVE-11, informs -14)

- Monomorphism is the whole game: hidden classes (Maps) turn property access into
  fixed-offset loads; ICs cache one shape (monomorphic = fast, polymorphic = degraded,
  megamorphic = deopt). **Instantiate every node's fields in the same order**; never add
  fields post-construction (forces a Map transition + deopt).
  [V8 hidden classes/IC](https://richardartoul.github.io/jekyll/update/2015/04/26/hidden-classes.html);
  [web.dev speed-v8](https://web.dev/articles/speed-v8)
- **STEAL:** PROVE-11 (parse-that TS scratch copy) = force one Map per node kind (uniform
  constructor field order), monomorphic dispatch (avoid megamorphic combinator call sites),
  typed-array tape for PROVE-14. Baseline = current `parse-that/typescript` v1.0.0
  (confirmed `package.json` "version":"1.0.0"). vitest `bench()` per no-tinybench rule.

## 12. Emit-spine residualization (F3 · PROVE-12)

- Two-stage / dav1d discipline: generator stage const-evals + minimizes DFA; residual is
  straight-line Rust; asm earned per-kernel. No fresh external paper needed — the SIMD-HTML
  (§2) and simdjson (§1) codegen posture is the precedent: specialize the classifier per
  alphabet at generate time, emit branch-free straight-line kernels.
- **STEAL:** PROVE-12 = const-eval/beta-reduce one hot rule to straight-line Rust, verify by
  `cargo asm` diff (per protocol: visual verification mandatory). Isomorphic to the existing
  regex DFA codegen in the local tree.

---

## LOCAL REUSE MAP (verified path:line → PROVE id)

| Asset | Path | Serves |
|-------|------|--------|
| NEON nibble shufti classify | `crates/simd-scan/src/neon.rs:208` `classify_chunk_nibble` | PROVE-01, -05 |
| NEON wide truffle classify | `crates/simd-scan/src/neon.rs:335` `classify_chunk_wide` | PROVE-01 |
| Multi-block stripe scan | `crates/simd-scan/src/neon.rs:362` `scan_multi`, `:435` | PROVE-04 |
| Quote/backslash stripe | `crates/simd-scan/src/neon.rs:502`, `:539`, `:569` | PROVE-03, -04 |
| `vshrn_n_u16 #4` movemask | `crates/simd-scan/src/neon.rs:683` `movemask_u8x16` | PROVE-01/04 |
| Structural alphabet LUT | `crates/simd-scan/src/alphabet.rs` `StructuralAlphabet` | PROVE-13 |
| Number-parse seam | `crates/simd-scan/src/parity.rs`; `crates/core/src/lower/value_expr/literals.rs`, `atom.rs` | PROVE-02 |
| CSS parity oracle | `crates/core/src/grammar/generated/css_l4.rs`, `css_pretty.rs` | PROVE-05 |
| CSV grammar (generality) | `crates/core/src/grammar/generated/csv.rs` (+`.registry.json`) | PROVE-13 |
| parse-that combinator arena | `parse-that/rust/parse_that/src/bump_slab.rs`; nightly gate at `.../src/lib.rs:1-3` (`portable_simd`, `fmt_helpers_for_derive`, `stmt_expr_attributes`) | PROVE-09 |
| parse-that DFA + accel | `parse-that/rust/regex/src/automata/dfa.rs`, `accel.rs`, `nfa.rs` | PROVE-10 |
| parse-that TS baseline (v1.0.0) | `parse-that/typescript/src/parse/parser.ts` | PROVE-11, -14 |

**Gotchas:** (1) M-series has NO usable SVE/SVE2/SME for byte work — spec NEON-only. (2)
PROVE-09 de-nightly is blocked by three `#![feature(...)]` at `parse-that/rust/parse_that/
src/lib.rs:1-3`; `portable_simd` is the hard one (stable NEON intrinsics via
`core::arch::aarch64` is the escape). (3) parse-that is touched ONLY via scratch copies
(non-clobber law) for PROVE-09/-10/-11. (4) lightningcss is scalar — beat it with a `vqtbl`
tokenizer, but keep typed-value-per-property parity (preserve-rich-AST). (5) simd-scan
already ships AVX2/AVX512/wasm siblings — reuse `parity.rs` as the scalar oracle, do not
re-derive.
