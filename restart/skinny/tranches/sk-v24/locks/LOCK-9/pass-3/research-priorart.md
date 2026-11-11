# LOCK-9 pass-3 — PRIOR-ART RESEARCH: the scanner & kernel plane (parse-that rust)

No family allegiance. Sweep of what exists, what won and lost elsewhere for THIS problem class
(NEON byte-scan kernels, movemask emulation, DFA-lexer scanners, prefilter tiers, shared SIMD
kernel homes, streaming input surfaces), which constraints our own evidence base already imposes
(SPEC §0 laws + the PROVE-09 refutation-as-law + PROVE-10), and the traps named in comparable
systems. Citations inline; internal evidence cited by artifact path.

Scope anchor (R3 §2/§4): the eight `portable_simd` scanners
(`scanners.rs`, `parsers/scan/{decode,ws_comment,structural_bitmap,quoted_simd,quote_parity,number_simd}.rs`,
`span_parser/scan.rs`, `state.rs`), the two x86-mixed files (`number_simd.rs`, `quote_parity.rs`),
the `PaddedView` 64-byte pad invariant (`state.rs:57`), and the kernel-sharing question with
`skinny/crates/bbnf-simd` (24 aarch64 files, 2324 LOC).

---

## 0. The constraints our own evidence base already imposes (binding, non-negotiable)

These are LAW before any prior art is weighed — a prior-art win that contradicts one of these is
a NO-GO here regardless of its pedigree elsewhere.

- **The movemask idiom is FIXED, contract-split (SPEC §0/03, §0/04; PROVE-10 disasm).** The
  certified fused string scan (feature 03, CONFIRMED, twitter parse_tape 0.898–0.931× cyc/B) is
  the **`vshrn` narrow** movemask (`shrn.8b v0, v0, #4` → `fmov` → mask), disasm-verified in
  PROVE-10 REPORT §2c. Block scheduling was refuted as a lever (SPEC §0/04, 1.05–1.06× only —
  the M-class OoO core already pipelines the single-block loop; classify is ALU-bound on the
  movemask bit-stitch). The F1 lever is the **movemask narrow + vectorized compaction**, not
  block scheduling. Directive F1 splits it: LEAN nibble-`vshrn` vs DENSE dense-64 `vpaddq` tree.
- **The double-`addv` movemask is the named trap (PROVE-09 VERIFY, LAW).** The `portable_simd`→NEON
  shim's `to_bitmask` emitted **2271 `addv.8b` vs baseline 509** — 2× the reduction work per mask —
  and the claimed 1.34–1.57× speedup was REFUTED to parity-or-slower on fresh paired windows. The
  standing lesson: two-arm window asymmetry is the classic self-deception; **paired interleaving is
  mandatory even for "obvious" wins**, and stable-NEON delivers STABILITY value, not speed
  (SPEC §0/09). Any reborn kernel gates on measured **no-regression**, never a speed claim.
- **No digit-scan SIMD (SPEC §0/02, NO_GO).** `number_simd.rs` ports its NEON arm to stable and
  purges x86, but does NOT pursue Mula/Lemire in-vector digit accumulation: −37/−48% instructions
  but a latency-bound CPI inversion (0.13→0.30) that regresses the product lane. The redress target
  is FLOAT CONVERSION (Eisel–Lemire materialize tax, +1.48 cyc/B), never the digit scan.
- **x86 is dead; M-series is NEON-only (SPEC §0 carried).** No SVE/SVE2/SME for byte work; the
  four-layer x86 abrogation reaches parse-that source (GN-0). Warm benches are disinformation
  (cold per-parse, paired, N≥200 only).
- **One-regex-home = parse-that (XL-C, settled; SPEC §1).** Grammar tier keeps `Regex(StringId)`
  opaque; engine tier keeps rich `Hir`; a string/format bridge joins them — NOT a single-HIR
  dependency. Vendored parse-that-regex inside skinny is the DFA substrate.

---

## 1. Movemask emulation on aarch64 — the central kernel problem class

**What won elsewhere.** ARM's own guidance ("Porting x86 vector bitmask optimizations to Arm
Neon") is exactly our contract-split, independently arrived at:
- **`shrn`/`vshrn_n_u16::<4>` is the default.** Shift the 16-bit compare result right by 4 and
  narrow to 8 bits → a 64-bit register where every matching bit repeats 4× (nibble stride). This
  is the LEAN producer. ARM notes portable shims (SSE2NEON, SIMDe) "require at least 4 instructions
  each with ≥2 cycles latency whereas `movemask` takes a single cycle on x86" — i.e. the naive
  emulation IS the trap, matching PROVE-09's double-`addv` finding.
- **The `vpaddq` pairwise-accumulation tree wins for DENSE 32/64-byte blocks.** ARM: SIMDJSON
  preferred pairwise (a `0x01,0x02,…,0x80` bit-position constant + sequential `vpaddq_u8`), ZSTD
  preferred `shrn`; "it does not necessarily mean the pairwise version is always faster because it
  requires additional tables and more registers." This is precisely directive F1's DENSE dense-64
  `vpaddq` tree vs LEAN nibble-`vshrn` split — and the guidance is **measure both**, which our
  paired-cold protocol already mandates.
- BurntSushi/memchr's aarch64 work (PR #114) corroborates: `shrn` for ≤16-byte / short runs,
  `umaxp`-then-`shrn` (existence-only, no position) for longer loops; the naive movemask emulation
  produced "significant 20% slowdowns in huge/common cases." This is the **short-run vs long-run**
  divide the scanners live on (PROVE-10 §2 notes short runs favor membership/structural tiers).

**Implication for LOCK-9.** The canonical producers (F1: LEAN nibble-`vshrn` / DENSE dense-64
`vpaddq` tree) are absent from `bbnf-simd` and must be authored once regardless of home. Note the
extant `bbnf-simd/src/aarch64/movemask.rs` is a HYBRID-NAIVE form: `vshrn_n_u16::<4>` narrow
followed by a **scalar `for pair in 0..8` bit-unpack loop** (`movemask.rs:20-24`) — it produces the
narrow but then re-serializes it in scalar, which is neither the pure-`fmov`-to-`u64` LEAN form
(PROVE-10's `fmov x8, d0; ands x8,#0x8888…`) nor the DENSE tree. Both canonical producers are owed.

Sources: [ARM: Porting x86 vector bitmask optimizations to Arm Neon](https://developer.arm.com/community/arm-community-blogs/b/servers-and-cloud-computing-blog/posts/porting-x86-vector-bitmask-optimizations-to-arm-neon),
[memchr PR #114 aarch64 neon](https://github.com/BurntSushi/memchr/pull/114),
internal: PROVE-09/VERIFY.md, PROVE-10/REPORT.md §2c, `bbnf-simd/src/aarch64/movemask.rs`.

## 2. portable_simd vs core::arch::aarch64 — the codegen-quality question (F2)

**What is known.** The `portable_simd` `to_bitmask` lowering on aarch64 is the weak link: it emits
the multi-`addv` reduction the shim's disasm exposed (PROVE-09 VERIFY: 2271 vs 509 `addv.8b`).
The community consensus (Shnatsel, "State of SIMD in Rust 2025") is that `std::simd` is still
nightly-only and its aarch64 lowering is uneven vs hand `core::arch` intrinsics; stdarch issue
#1659 tracks the reverse (using portable intrinsics *inside* stdarch) but that is orthogonal. The
DEV benchmark "std::simd vs NEON on Apple M4" reports hand-NEON edges on reduction-heavy kernels —
consistent with our refutation that the win, where it appeared, was window asymmetry not codegen.

**The honest position (SPEC §0/09, PT-2 §1.1).** The de-nightly `core::arch::aarch64` port's value
is STABILITY (leaving the ICE-prone `nightly-2026-04-11` pin — 93 recorded `on_disk_cache` ICEs,
R3 §1), not speed. Gate = measured no-regression, paired. Stable `core::arch::aarch64` gives
byte-lane intrinsic control (the exact `vshrn`/`vpaddq` producers) that `portable_simd` abstracts
away — which is the correctness argument for the port independent of any perf claim.

**The `smallbox` nightly cut (F2 blast radius).** Sole use site is the parser vtable
(`parse.rs:1,62,74`); `smallbox!`≡`SmallBox::new` (alloc-neutral, PROVE-09 §"Alloc profile"). Prior
art for "small-box on stable": `smallbox` 0.8 stable path drops the inline-storage niche optimization
but keeps S32 inline/heap behavior; the bump-slab-arena alternative is lifetime-invasive over 86
`Parser::new` sites (the `'a` input lifetime), honestly carried as a tranche-scoped refactor, not a
prover edit (PROVE-09 §"Why the bump-slab arena was NOT the vtable substitute"). The
`fmt_helpers_for_derive` residual is a GENERATOR fix (emit `#[derive(Debug)]` not its expansion),
per the clean-regen edict — not a hand-patch.

Sources: [Shnatsel, State of SIMD in Rust 2025](https://shnatsel.medium.com/the-state-of-simd-in-rust-in-2025-32c263e5f53d),
[stdarch #1659](https://github.com/rust-lang/stdarch/issues/1659),
[std::simd vs NEON on Apple M4](https://dev.to/erioharrison/rust-simd-benchmark-stdsimd-vs-neon-on-apple-m4-47jd),
internal: R3 §2, PROVE-09/REPORT.md, PT-2.md §1.

## 3. Structural-scanner architecture — simdjson / sonic-rs (the JSON kernel lineage)

**What won.** simdjson's two-stage scanner is the reference design our scanners already mirror:
compute a quote mask Q, remove escaped quotes via odd-length backslash runs (OD), carry-less
multiply (`clmul`/`pmull`) to turn quote-open positions into an in-string mask R, then classify
structural `[]{},:` into mask S. The quote-parity via carry-less multiply is Langdale's
PCLMULQDQ trick ("Finding quote pairs with carry-less multiply"); on aarch64 the equivalent is
`vmull_p64`/`pmull` — which is exactly the gated aarch64 path in our `quote_parity.rs:51-56`
(`vmull_p64` under `target_feature="aes"`), with the x86 `_mm_clmulepi64_si128` arm to be purged.

**What lost / the honest limits.** sonic-rs deliberately does NOT use simd-json's full two-stage
algorithm (it found the staged structural index too costly for its on-demand model). This validates
our own §0 carried law: **tape-navigation is NOT the get lane** (feature 06 measured stop-early at
150–1700× the tape-nav cost) — the structural-bitmap scanner earns its keep only for eager full-tape
parse, and the stop-early/get lanes route around it. sonic-rs' lazy/on-demand posture is the prior
art for our value-surface split (eager tape / stop-early get / direct-to-struct / TS tape).

**Trap named.** simdjson arranges code to HIDE `pclmulqdq` latency (do not follow it with a
dependent op). Our `quote_parity.rs` must preserve that scheduling on the `pmull` path; a naive
port that serializes on the `pmull` result forfeits the latency hiding.

Sources: [Langdale, quote pairs with carry-less multiply](https://branchfree.org/2019/03/06/code-fragment-finding-quote-pairs-with-carry-less-multiply-pclmulqdq/),
[sonic-rs](https://github.com/cloudwego/sonic-rs), [simdjson.org](https://simdjson.org/),
internal: R3 §4 (`quote_parity.rs`), SPEC §0/06 carried law.

## 4. Scanner-as-DFA — re2c / ragel / lexgen (family 9C prior art, F3)

**What won.** re2c compiles regex→DFA and emits **direct-coded** lexers (conditional jumps +
comparisons), which are "faster and often smaller than table-driven analogues" and easier to debug;
it uses lookahead-TDFA for submatch extraction at minimal overhead, and is "usually a bit faster
than ragel." This is the strongest evidence FOR family 9C (quote/number/ws scanning as compiled
DFA/prefilter products of the bootstrapped grammar): direct-threaded DFA codegen is a mature, SOTA
technique with a decades-long track record. re2c targets Rust among 14 languages.

**What loses / the trap for us.** DFA scanning is byte-at-a-time; it does NOT vectorize the hot
inner scan. Directive F3's own caution is the counterweight: "a small-multistate serial `vqtbl1q`
chain is a measured negative on the SHORT runs these scanners see." A grammar→DFA scanner competes
with the SIMD membership/structural tiers only where the run is long AND the class is
SIMD-unfriendly. The over-ceiling denominators (255-class / 4096-state) mean rich CSS `url()` /
wide-unicode leaves may exceed the tractable `[u8;256]`→SIMD classifier tier — the census owes the
coverage %. Prior art says the classical `nfa→dfa→minimize` pipeline (which `bbnf-regex` already
has: `dfa.rs` Hopcroft minimization) is the right substrate; LOCK-5 owns whether e-graph/CSP
extraction beats it, gated head-to-head (measured wins stay, losses are named negative results).

**Byte-class classifier prior art.** simdjson/hyperscan use a `[u8;256]`→nibble-LUT classifier
(`vqtbl1q_u8` shuffle on hi/lo nibbles) — exactly `bbnf-simd/src/aarch64/classify_tbl4.rs` /
`byte_class_from_table_64.rs`. The known trap (F3): a **naive nibble-only emit misclassifies the
structural set** when two structural bytes share a nibble bucket — the emit pass needs
partition/separability selection, not a blind nibble split.

Sources: [re2c.org](https://re2c.org/), [re2c: lexer generator based on lookahead-TDFA](https://re2c.org/2020_trofimovich_re2c_a_lexer_generator_based_on_lookahead_tdfa.pdf),
[skvadrik/re2c](https://github.com/skvadrik/re2c),
internal: R3 §3 (`dfa.rs` Hopcroft), DIRECTIVES F3, `bbnf-simd/aarch64/classify_tbl4.rs`.

## 5. Prefilter / Teddy tier — the multi-byte entry accelerator (PROVE-10 lineage)

**What won (already proven in-house).** PROVE-10 productionised a start-state memchr prefilter into
`Dfa::find` — **9.6×–29× match throughput** on prefilterable patterns, **0.99× controls**,
compile-neutral, 44/44 tests (SPEC §0/10 CONFIRMED 8.8–29.5×). It reuses the `Memchr1/2/3` tier
vocabulary from `accel.rs`. This is the direct descendant of regex-automata's prefilter ladder and
memchr's rare-byte heuristic.

**The named next tier (out of PROVE-10 scope).** For entry sets >3 bytes, memchr falls to `None`
(parity). The prior art here is **Teddy** (aho-corasick `packed/teddy`, learned from Hyperscan): a
SIMD packed multi-substring matcher using nibble-LUT `pshufb`/`vqtbl1q` fingerprints, and the
rare-byte heuristic ("prefer heuristically-rare bytes; when >3 starting bytes, look for 3 distinct
bytes at any position"). Teddy is the lawful extension of the prefilter tier to larger literal sets —
but it is a match-entry accelerator, NOT the scanner itself. Its cost model (build-time LUT, minimum
haystack length) means it loses on the short runs the parse-that scanners see; weight membership +
structural tiers there (directive F3).

Sources: [aho-corasick Teddy README](https://github.com/BurntSushi/aho-corasick/blob/master/src/packed/teddy/README.md),
[Gallant, regex engine internals as a library](https://burntsushi.net/regex-internals/),
internal: PROVE-10/REPORT.md, SPEC §0/10.

## 6. Shared kernel home vs own kernels (F1 — family 9A vs 9B)

**Prior art on cross-crate SIMD reuse.** simdutf/simdutf8 are the reference for a shared byte-scan
kernel library consumed by many callers; the `multiversion` crate is the standard for runtime
feature dispatch. But the recurring finding (CuriousCoding "Distributing Rust SIMD Binaries";
Shnatsel 2025) is that **cross-crate `#[inline(always)]` of SIMD kernels works but bloats code
size**, and runtime dispatch adds a branch the fused inner loop wants to avoid. For a fixed-ISA
target (aarch64-only, no multiversioning needed — x86 is dead) the dispatch concern evaporates; the
only question is the inline boundary.

**What our own evidence already settles (F1 "do not re-derive").** Cross-crate `#[inline]` across
the parse-that↔bbnf-simd rlib boundary is measured-LIVE (0 in-loop indirect calls); de-facing a
facade is bbnf-simd-internal hygiene, not a perf lever. So 9A (one shared aarch64 home) vs 9B (own
kernels) is NOT a performance question — it is a maintenance/drift-cost question. The residual
LOC/substrate divergence (padded-view/clamp conventions, single- vs multi-table shuffle, capacity
constants) between one-home and two-home layouts is the real deliverable, and the canonical movemask
producers must be authored once into whichever home regardless (they are absent from bbnf-simd today).
The `PaddedView` 64-byte pad (`state.rs:57`) vs bbnf-simd's `eob_pad_clamp.rs` is a concrete
convention-divergence site to census.

**Trap named (F1).** The `#[inline]` cross-seam guarantee is config-dependent: under a low cgu count
or thin-LTO-off build, a large kernel may fail to inline across the crate boundary and become a real
call. The pass owes the exact (cgu, LTO) config where the seam appears — evidence, not assertion.

Sources: [CuriousCoding, Distributing Rust SIMD Binaries](https://curiouscoding.nl/posts/distributing-rust-simd-binaries/),
[multiversion crate](https://docs.rs/multiversion/latest/multiversion/),
[simdutf8](https://crates.io/crates/simdutf8/0.1.2),
internal: DIRECTIVES F1, R3 §4 (`state.rs:57` PaddedView), `bbnf-simd` file census.

## 7. Windowed vs streaming input surface (F4 — family 9D)

**What exists in the ecosystem.** winnow's `Partial<I>` tags input so parsers return
`ErrMode::Incomplete` when short and switch to complete-parsing at runtime when the reader reports
true EOF; nom v8 back-propagated `parse_complete`. simdjson's on-demand and serde_json's
`StreamDeserializer` handle document-at-a-time streaming. sonic-rs' lazy model is the on-demand
analogue.

**The honest limit for parse-that (F4).** parse-that's scanners consume a whole owned `PaddedView`
(64-byte trailing NUL lets every stripe skip a tail guard, `state.rs:57`). Whole-document-granularity
streaming (the only shape winnow/serde offer cheaply) does NOT reduce peak tree RSS — the tree is
built whole. So 9D (streaming-first) is a real feature ONLY if there is an in-wave chunked consumer;
absent one, it is consumer-less substrate (Constitution viii, Y.13 — forbidden). The census owes:
does any shipped grammar feed chunked/`from_reader`, and what is the rewind distribution
(`furthest_offset − offset`, P50/P99/max)? If backtrack is BOUNDED, a fixed ring buffer suffices and
full streaming is unwarranted; if unbounded, streaming needs a materialization boundary the current
combinator core does not have. Prior art (winnow) shows the runtime complete/partial switch is the
lawful mechanism — but only pays for itself with a genuine reader-fed consumer.

Sources: [winnow Partial](https://docs.rs/winnow/latest/winnow/stream/struct.Partial.html),
[winnow streaming discussion #169](https://github.com/winnow-rs/winnow/discussions/169),
internal: DIRECTIVES F4, R3 §4 (`state.rs` PaddedView), Constitution viii (Y.13).

---

## 8. Synthesis — the prior-art verdict per family

- **9A/9B (shared vs own home):** prior art (simdutf, multiversion) says shared kernels are
  mainstream but the perf question is moot here (aarch64-only, inline measured-live). The decision is
  maintenance/drift-cost; the canonical `vshrn`/`vpaddq` producers are owed to EITHER home. Neither
  9A nor 9B is refuted; the deliverable is the residual-divergence census + the config where inline
  fails to cross.
- **9C (scanner-as-DFA):** re2c proves direct-coded DFA scanning is SOTA and Rust-targetable, but it
  does not vectorize the hot short-run scan — a measured negative on short runs (F3). Lawful only
  where run-length × class-unfriendliness justifies it; the SIMD membership/structural tiers keep the
  hot path. Census the over-ceiling coverage %.
- **9D (streaming):** winnow/serde offer document-granularity streaming that does NOT cut peak RSS;
  lawful only with an in-wave chunked consumer (else Y.13-forbidden). Owe the rewind histogram +
  streaming-surface grep.
- **The invariants that survive every prior-art comparison:** movemask = `vshrn` LEAN / `vpaddq`
  DENSE (ARM + PROVE-10), NEVER double-`addv` (PROVE-09 LAW); no digit-scan SIMD (§0/02); stable-NEON
  = stability not speed, gate on no-regression paired (§0/09); pmull latency-hiding preserved on the
  quote-parity path (simdjson trap).
