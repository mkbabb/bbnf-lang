# 26 — SOTA in Data-Parallel Parser GENERATION: The 2025–2026 Frontier Refresh

Scope: the *generator* landscape, not the hand-written-parser landscape. The load-bearing
question is narrow and unchanged from the prior campaign: does any system, in the published
record through mid-2026, take a **declarative grammar** and **emit block-wide SIMD structural
classification**, over a **backend-agnostic IR**, with a **grammar-derived typed value plane
over one zero-copy tape**? That four-part conjunction is the entire novelty surface. This
report re-runs the adjudication against everything dated 2025 and 2026, separates what is alive
from what is aspirational, and returns a refreshed verdict.

External claims carry URLs / arXiv IDs. The prior adjudication being refreshed is the SK-V20
report at `restart/skinny/tranches/sk-v20/research/32-novelty-prior-art.md` (the four-part
conjunction is stated there at lines 105–126).

---

## 0. The refresh verdict up front

**The seam is still empty. The four-part conjunction remains unattested as of July 2026.**
Nothing dated 2025 or 2026 crosses it. What *did* move, and must be folded into the totality
plan's honesty ledger:

1. **The Parabix bitstream-compilation lineage is demonstrably ALIVE in 2025–2026** — not the
   dormant artifact the prior report treated it as. BitGen (MICRO'25) and HybridSA (OOPSLA'24)
   both compile through Parabix's bitstream IR to *GPU* CUDA kernels; an ARM-NEON Parabix
   re-port carries a Feb-2026 project report. The lineage revived — but on the **regex** axis
   and toward **GPU/ARM backends**, *not* toward a general CFG/PEG grammar generator. The "the
   general grammar→SIMD generator was scoped but never shipped" claim survives, now against
   fresh 2025–26 evidence, and the lineage's aliveness *strengthens* the "why has nobody built
   this" framing rather than weakening it.

2. **A new and genuinely close adjacent field emerged** that the prior report underweighted:
   **LLM constrained-decoding grammar engines** (XGrammar, XGrammar-2, llguidance). These are
   *the* 2024–2026 systems that take a declarative CFG and JIT-compile a high-performance
   recognizer. They are the strongest "declarative grammar + compilation + microsecond-class
   performance" competitors in existence — but they solve a *different problem* (masking an LLM
   token distribution, not parsing input text into typed values), use a byte-level **pushdown
   automaton / Earley** core with **no SIMD block classification** and **no zero-copy value
   plane**. They belong on a third axis and must be named in any prior-art section, or a
   reviewer will raise them.

3. **The Axis-V (hand-written vectorized parser) frontier advanced materially** in 2025 —
   simdjson On-Demand hardened (SPE 2024), Lemire's HTML-scanning-on-ARM paper (SPE 2025) and
   simdzone/DNS (SPE 2025) landed, plus genomics (Helicase) and astronomical-data SIMD parsers.
   Every one is **hand-written for a single format**. The Axis-V verdict ("SIMD but bespoke per
   format, no grammar input") is *reinforced*, not challenged, by the 2025 crop.

4. **The Axis-P (generator) frontier is quiescent on vectorization.** tree-sitter, pest,
   chumsky, winnow, nom, logos, LALRPOP all shipped through 2026 with active development, and
   **none emits SIMD**. The one relevant 2026 survey (an empirical comparison of six general CF
   parsers) benchmarks GLR/GLL/Earley/CYK — all scalar. logos, the fastest Rust lexer
   generator, explicitly relies on compiler auto-vectorization, not emitted intrinsics, and a
   hand-SIMD lexer beats it by ≥20%.

Net: the architecture bbnf-lang is chasing is *more* clearly unoccupied in mid-2026 than it was
at SK-V20, because the fields that *could* have occupied it (Parabix lineage, LLM grammar
engines, Lemire's vectorized-scan line) all published in 2025–26 and all stopped short of the
seam — each for a structural reason the totality plan can name.

---

## 1. The three axes (updated from SK-V20's two)

SK-V20 framed the thesis as the product of two lineages (Axis P: parser generation; Axis V:
vectorized structural parsing + zero-copy). The 2024–2026 literature forces a **third axis**
into the picture:

- **Axis P — parser generation.** Declarative grammar → emitted specialized parser. Yacc/Bison,
  ANTLR, tree-sitter, pest, chumsky, winnow, nom, logos, LALRPOP, ModelCC, Ragel, Parabix.
  *Uniformly scalar output.*
- **Axis V — hand-written vectorized structural parsing + zero-copy value plane.** simdjson,
  sonic-rs, simd-json, yyjson, simdcsv, simdzone, Lemire's HTML scanner, Helicase, Mison/Pikkr,
  FlatBuffers/Cap'n Proto. *SIMD, but bespoke per format; no grammar input.*
- **Axis G — declarative-grammar constrained *generation* (new).** XGrammar, XGrammar-2,
  llguidance, Outlines/Lark-based decoders. *Declarative CFG in, JIT recognizer out, µs-class —
  but the recognizer masks an LLM's next-token distribution; it never parses text into typed
  values, never SIMD-classifies a block, never builds a tape.*

The thesis is the still-unbuilt **P × V product**. Axis G is a decoy that looks like it crosses
the seam (declarative grammar + compilation + speed) but sits orthogonal — it must be
explicitly distinguished, not ignored.

---

## 2. Axis P — the generator set, 2026 state (all scalar)

The generator field is healthy and shipping, and vectorization is absent from every emitter.

- **tree-sitter** remains the dominant incremental generator: incremental GLR, compile-time
  ambiguity via precedence + run-time via GLR, grammars authored in JavaScript, emitting a
  large table-driven `parser.c` lexer (2026 coverage: dasroot.net/posts/2026/02/
  incremental-parsing-tree-sitter-code-analysis; en.wikipedia.org/wiki/Tree-sitter_(parser_generator)).
  The generated lexer is a scalar 2-D table walk; the "external scanner" escape hatch is
  hand-written C. SIMD appears nowhere in generated output. This reconfirms SK-V20
  `32-novelty-prior-art.md:39` against 2026 sources.

- **The Rust combinator/generator crates** all advanced but none emit SIMD:
  - **chumsky** (1.0-alpha line; moved to Codeberg) — zero-copy recursive-descent PEG-class
    combinators, expressive, fast, scalar (github.com/zesterer/chumsky; lib.rs/crates/chumsky).
  - **winnow** (nom fork, the current speed leader among Rust combinators) — ~627 MB/s on a
    reference bench vs chumsky 533, nom 213, pest 57 (docs.rs/winnow `_topic::why`;
    epage.github.io/blog/2023/07/winnow-0-5). Scalar.
  - **nom** — the mature baseline; scalar.
  - **pest** — PEG, slowest of the set; scalar (lib.rs/parsing).
  - **logos** — the fastest Rust *lexer* generator: compiles token specs to a jump-table state
    machine, batches reads, unrolls loops — but **relies on the compiler's auto-vectorizer, not
    emitted intrinsics** (github.com/maciejhirsz/logos; logos.maciej.codes). A hand-written SIMD
    lexer beats logos by ≥20% in pure throughput precisely because logos does *not* emit
    explicit vector code (alic.dev/blog/fast-lexing). This is a direct data point: even the
    fastest emitter in the ecosystem leaves SIMD on the table.

- **LALRPOP** — LR(1) generator for Rust, actively maintained through the 2026 IDE cycle
  (github.com/lalrpop/lalrpop; plugins.jetbrains.com/plugin/15229-lalrpop). LALR table walk,
  emits AST structs. Scalar.

- **The 2026 general-CF-parser survey** ("An Empirical Comparison of General Context-Free
  Parsers", Vo/Liyanage/Kang/Rubin/Gopinath, Univ. Sydney, arXiv:2606.08465) benchmarks CYK,
  Valiant, Earley, GLL, RNGLR, BRNGLR + LL(1)/LR(1) baselines, all in Rust with shared data
  structures across 22 grammars up to C++/Java. Finding: GLR is the practical generalized
  default at ~3× median slowdown over LR(1). **The entire modern generalized-parsing frontier
  is scalar algorithmic work** — the survey does not consider vectorization as a dimension at
  all. That absence is itself the signal: SIMD is simply not on the parser-generation research
  community's radar as of mid-2026.

- **ModelCC** (model-based generator, arXiv:1501.03458) and derivative-based / visibly-pushdown
  generators (arXiv:2109.04258) round out the academic generator set — all scalar.

**Axis-P verdict (refreshed):** unchanged and *reinforced*. "Generate a specialized parser from
a declarative grammar" is a solved, crowded, decades-old space that continues to ship in 2026,
and **not one member emits block-wide SIMD structural classification.** logos is the proof by
strongest case: the fastest emitter in the ecosystem deliberately defers to auto-vectorization
and loses ≥20% to hand-SIMD as a result.

---

## 3. Axis V — the hand-written vectorized frontier, 2025–2026 (SIMD, but bespoke)

The Axis-V field is where 2025 was busiest, and every advance is a *hand-tuned, single-format*
kernel — no grammar, no generator.

- **simdjson On-Demand** matured into a peer-reviewed result: Keiser & Lemire, "On-Demand JSON",
  *Software: Practice and Experience* 54(6), 2024 (arXiv:2312.17149). Branchless quoted-substring
  detection, two predictable passes (structural find + validate, then materialize only what's
  used). Still **JSON-only, hand-written C++**; the structural alphabet `{}[],:"` is baked into
  source (github.com/simdjson/simdjson). simdjson 4.x carries a NEON unescape kernel — still
  hand-written, still JSON.

- **Lemire, "Scanning HTML at Tens of Gigabytes per Second on ARM Processors"**, *SPE* 55(7),
  1256–1265, 2025 (arXiv:2503.01662; code github.com/lemire/htmlscanning). WebKit and Blink
  adopted **hand-written NEON vectorized classification** for HTML scanning; ~20× over scalar;
  a 64-bit index over 64 bytes via NEON intrinsics. This is the *exact block-classification
  technique* bbnf-lang aims to generate — and here it is, in 2025, **hand-written for HTML by
  the browser vendors**, one format, no grammar. It is the cleanest possible illustration of
  the seam: the technique is mainstream and mature; *generating* it from a grammar is what
  nobody does.

- **simdzone / "Parsing Millions of DNS Records Per Second"** (Koekkoek, *SPE* 2025;
  arXiv:2411.12035). Two-stage: SIMD 64-byte indexing → parse. Several× Knot DNS.
  **DNS-zone-file-only, hand-written.** Note the architecture is *identical in shape* to
  simdjson stage-1/stage-2 and to bbnf's classify-then-tape — independently re-derived per
  format because there is no generator to share it.

- **Helicase** ("Vectorized parsing and bitpacking of genomic sequences", 2025) — SIMD register
  loads + per-character bitmasks + movemask. **Genomics-only, hand-written.**

- **Astronomical-data SIMD parser** (2025, openastronomy.org / Medium) — an 8-records-at-once
  AoS→SoA transpose so corresponding bytes are lane-contiguous, AVX2 classify + scalar numeric
  tail (hybrid). **One record format, hand-written.**

- **sonic-rs / simd-json** — Rust SIMD JSON, borrowing simdjson's branchless algorithms;
  sonic-rs adds direct-to-struct rejecting the two-stage tape (github.com/cloudwego/sonic-rs).
  **JSON-only, hand-written.** yyjson remains the scalar-ILP SOTA-on-some-corpora refutation
  anchor.

- **Mison/Sparser/Pikkr** and **FlatBuffers/Cap'n Proto** — unchanged from SK-V20; query-driven
  JSON speculation and producer-controlled wire formats respectively. Neither accepts a grammar.

**Axis-V verdict (refreshed):** *reinforced by an unusually productive 2025.* The block-wide
NEON classify-64 technique is now demonstrably mainstream (browsers ship it for HTML, DNS
resolvers for zone files, simdjson for JSON) — and every instance is **re-derived and
hand-written per format**. The proliferation of near-identical two-stage classify→materialize
architectures across HTML, DNS, JSON, genomics, and astronomy in a single year is the strongest
market signal yet that a *generator* for this pattern is the missing piece. Nobody has one.

---

## 4. The seam — the Parabix lineage is ALIVE (and still short of the seam)

This is the most important *update* over SK-V20, which treated Parabix as a dormant, mostly
historical seam-ancestor. In 2024–2026 the bitstream-compilation lineage published actively:

- **BitGen** — "Interleaved Bitstream Execution for Multi-Pattern Regex Matching on GPUs",
  *MICRO'25* (58th IEEE/ACM Int'l Symposium on Microarchitecture), doi 10.1145/3725843.3756052;
  artifact github.com/getianao/BitGen; Zenodo 16664499. **BitGen uses Parabix to compile regexes
  into bitstream programs, then — unlike Parabix, which lowers to CPU SIMD — emits optimized GPU
  CUDA kernels** (MISD-style: each CTA runs a regex-derived bitstream program over a shared
  input; Python-implemented, NVRTC-JIT'd; RTX 3090 / H100 / L40S). This is a live 2025 *revival*
  of Parabix bitstream compilation targeting a new backend.

- **HybridSA** — "GPU Acceleration of Multi-pattern Regex Matching using Bit Parallelism",
  *OOPSLA/PACMPL* 2024, doi 10.1145/3689771. Same family: bit-parallel multi-regex on GPU.

- **Parabix on ARM** — a project report dated **2026-02-01** (mdsz.ca/experience/
  parabix-arm-project-report) rewrites `IDISA_ARM_Builder` functions in **NEON intrinsics** and
  demonstrates `icgrep`, `wc`, and `csv2json`. Verified delivered scope: **regex matching and
  text-processing utilities**, via the **Pablo** IR (PabloBuilder → PabloAST → LLVM). It states
  Parabix "is capable of compiling regular expressions to machine code" — **regex-specific, not
  a general BNF/EBNF grammar framework.**

What the 2025–2026 Parabix lineage confirms, verified against fresh sources:

1. **The general grammar→SIMD generator still has not shipped.** The Feb-2026 ARM report's
   delivered artifacts are icgrep/wc/csv2json — regex + utilities. The BNF/EBNF parser-generator
   objective remains, per the fresh evidence, unshipped; the lineage's *actual* 2024–26 output is
   **regex** on new backends (GPU, ARM NEON). SK-V20 `32:80-85` holds, now against 2026 sources.

2. **The IR is bitstream-specific (Pablo), and the lineage's 2025 energy went to backend
   *retargeting* (GPU via BitGen, ARM via NEON re-port), not to grammar *generalization*.** Pablo
   models inductive-doubling bitstream transposition, not simdjson-style 64-byte block
   classification into per-kind masks. Related family, different mechanism (SK-V20 `32:88-96`).

3. **No typed grammar-derived value plane anywhere in the lineage.** Parabix/BitGen produce
   match bitstreams / parse-event markers. There is no `->` typed projection, no lazy/eager
   unified `ValueRef` over a SoA tape, no direct-to-struct bridge. The value-plane conjunct has
   *zero* prior art in the bitstream lineage.

**Seam verdict:** the one genuine ancestor is *more* active than SK-V20 recorded — and its
2024–26 activity (BitGen GPU, ARM NEON re-port, HybridSA) is entirely on the **regex + backend
retargeting** vector, never toward a general declarative-CFG generator with a typed value plane.
The seam the thesis occupies is confirmed empty against the very lineage most likely to fill it.

---

## 5. Axis G — the decoy: LLM constrained-decoding grammar engines

This is the new adjacent field, and it is the one a sharp reviewer will raise, so the totality
plan's prior-art section must name and dispatch it.

- **XGrammar** (arXiv:2411.15100; OpenReview rjQfX0YgDl) and **XGrammar-2** (arXiv:2601.04426,
  Jan 2026) — a byte-level **pushdown automaton** interpreting a CFG; the PDA stack-top indexes
  an *adaptive token-mask cache*; tokens split into context-independent (precomputable, >99%)
  vs context-dependent. Default structured-generation backend for vLLM, SGLang, TensorRT-LLM as
  of March 2026; <40 µs/token (catalyst.cs.cmu.edu/projects/xgrammar).
- **llguidance** (github.com/guidance-ai/llguidance) — Microsoft, Rust **Earley** parser,
  ~50 µs/token, negligible startup; near-parity with XGrammar (arXiv:2502.05111).
- Plus token-space-compression accelerators (arXiv:2605.29986) and Lark/Outlines-based decoders.

Why this is a *decoy*, not a seam-crosser — three structural disqualifiers:

1. **Different problem.** These systems compute, at each generation step, the set of tokens a
   grammar permits *next*, to mask an LLM's output distribution. They never ingest a document
   and materialize its typed values. There is no input text, no parse tree, no value plane.
2. **No SIMD block classification.** The hot path is PDA-stack / Earley-chart manipulation +
   token-mask bitset ops; the "vectorization" is over the *vocabulary bitmask*, not over input
   *bytes* in 64-byte NEON blocks. It is not the classify-64 technique at all.
3. **No zero-copy typed value plane.** Output is a boolean legality mask over vocabulary, not a
   `ValueRef` over a tape.

They *are*, however, the strongest existing "declarative CFG in → JIT-compiled µs-class
recognizer out" systems, and they prove the market wants grammar-driven compiled recognizers.
The honest framing: bbnf-lang and XGrammar share "declarative grammar → compiled fast
recognizer" but diverge completely on *what is recognized* (input text → typed values vs
LLM-token legality) and *how* (NEON block classify + tape vs PDA/Earley + token-mask). Name it,
distinguish it, move on.

---

## 6. Novelty adjudication — refreshed against 2025–2026

The SK-V20 four-part conjunction (`32-novelty-prior-art.md:105-126`), tested conjunct-by-conjunct
against everything dated 2025–26:

| Conjunct | Closest 2025–26 prior art | Crosses? |
|---|---|---|
| (1) Declarative grammar input | Every Axis-P generator; XGrammar/llguidance (Axis G) | No — universal, not novel alone |
| (2) Emits block-wide SIMD structural classification | Lemire HTML-scan (hand-written), simdzone (hand-written), simdjson (hand-written); logos (auto-vec only, no intrinsics); **no generator emits it** | **No — empty** |
| (3) Backend-agnostic IR a specializing emitter lowers | Parabix/Pablo (bitstream-specific, not backend-agnostic); BitGen retargets Pablo→GPU but IR is still bitstream-shaped | No — Pablo is not backend-agnostic classify-IR |
| (4) Typed grammar-derived value plane over one zero-copy tape | FlatBuffers/Cap'n Proto (producer-controlled wire, no parse, no SIMD, no grammar) | **No — no prior art over parsed third-party text** |

**No 2025–2026 system holds even the (1)×(2) pair** — a declarative grammar that *generates*
block-wide SIMD structural classification. The full four-way conjunction is, to the published
record through July 2026, **unattested.** The SK-V20 defensible novelty statement
(`32:194-204`) stands verbatim; the refresh adds only that the fields most able to refute it all
published in 2025–26 and none did.

**What the refresh changes about the *framing* (must fold into the plan):**

- The claim "Parabix's general grammar→SIMD generator was scoped but never shipped" must now be
  stated against **2025–26 evidence** (BitGen MICRO'25, Feb-2026 ARM report) showing the lineage
  is *active but pointed elsewhere* (regex, GPU/ARM backends) — a stronger, better-cited claim
  than SK-V20's, which leaned on a course-project page.
- Any prior-art section MUST name **XGrammar / llguidance** and explicitly place them on Axis G
  (constrained generation), or the omission reads as ignorance of the 2024–26 grammar-engine
  boom. They are the most visible "declarative grammar + compilation + speed" systems alive.
- The Axis-V proliferation argument is now *empirically* strong: HTML (browsers), DNS (simdzone),
  JSON (simdjson/sonic-rs), genomics (Helicase), astronomy — five domains independently
  re-deriving the same two-stage classify→materialize architecture *by hand* within ~18 months.
  This is the market-pull evidence for a generator, and it should headline the motivation, not
  sit in a footnote.

---

## 7. What could still refute the claim; claims to not make

- **A hidden hardware-vectorization patent lineage exists.** Search surfaced a 1990s patent,
  "Vectorized LR parsing of computer programs" (US 5193192), and "register vector grammar"
  compilation (US 5095432). These are vector-*processor*-era LR schemes, not NEON block
  classification, and predate the simdjson technique by decades — but a novelty *filing* (not
  merely a paper) would need a patent search, not just a literature search. Do **not** claim
  "first ever to vectorize parsing"; claim "first *generator* to emit grammar-derived block-wide
  SIMD structural classification with a typed value plane." The narrow claim is clean; the broad
  one is refutable by 30-year-old patents.
- **Do NOT ignore Axis G.** Claiming "first declarative-grammar-driven compiled fast recognizer"
  is false — XGrammar/llguidance are exactly that on their axis. Scope to "parsing input text
  into a typed value plane," not "recognizer."
- **Do NOT claim novel SIMD kernels.** The classify-64 / escape-carry / prefix-XOR / nibble-LUT
  technique is Langdale–Lemire and is now *mainstream* (browsers ship it). The novelty is
  *generating* it from grammar, never inventing it (SK-V20 `32:228-230`, still binding).
- **Do NOT claim the generalization is shipped.** SK-V20 `32:180-188` (thesis proven in skinny
  for JSON/CSS, un-folded in `crates/`) is an internal-state claim this web report cannot
  refresh — but it gates the "best-in-class" claim regardless of the novelty verdict. Novel ≠
  best-in-class; the frontier (simdjson 4.x C++ NEON, GLR at 3× LR(1), µs-class grammar engines)
  is real and the demonstration must actually beat it, not a 2023-era pin.

---

## 8. Sources

Parabix lineage: mdsz.ca/experience/parabix-arm-project-report (ARM report, 2026-02-01);
dl.acm.org/doi/10.1145/3725843.3756052 (BitGen, MICRO'25); github.com/getianao/BitGen;
zenodo.org/records/16664499; dl.acm.org/doi/10.1145/3689771 (HybridSA, OOPSLA'24);
dl.acm.org/doi/10.1145/3373376.3378461 (bitwise speculation, ASPLOS'20).
Axis-V frontier: arXiv:2312.17149 (On-Demand JSON, SPE'24); github.com/simdjson/simdjson;
arXiv:2503.01662 + onlinelibrary.wiley.com/doi/pdf/10.1002/spe.3420 (HTML-scan on ARM, SPE'25);
github.com/lemire/htmlscanning; arXiv:2411.12035 (DNS records/simdzone, SPE'25);
researchgate.net Helicase (genomics, 2025); openastronomy.org/Universe_OA (astro SIMD, 2025);
github.com/cloudwego/sonic-rs.
Axis-P generators: dasroot.net/posts/2026/02/incremental-parsing-tree-sitter-code-analysis;
en.wikipedia.org/wiki/Tree-sitter_(parser_generator); github.com/zesterer/chumsky;
docs.rs/winnow; epage.github.io/blog/2023/07/winnow-0-5; github.com/maciejhirsz/logos;
logos.maciej.codes; alic.dev/blog/fast-lexing; github.com/lalrpop/lalrpop;
arXiv:2606.08465 (empirical comparison of general CF parsers, 2026); arXiv:1501.03458 (ModelCC).
Axis-G grammar engines: arXiv:2411.15100 + openreview.net/pdf?id=rjQfX0YgDl (XGrammar);
arXiv:2601.04426 (XGrammar-2, 2026); github.com/guidance-ai/llguidance; arXiv:2502.05111;
arXiv:2605.29986 (token-space compression); catalyst.cs.cmu.edu/projects/xgrammar.
Prior-art / negative-space: arXiv:2605.17978 (AutoVecCoder); arXiv:2507.15224 (SimdBench);
US 5193192 (vectorized LR parsing, 1990s patent); US 5095432 (register vector grammar).
