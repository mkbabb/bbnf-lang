# AW-V N2 — Novel Parsing Approaches Beyond the Seven Levers

## Executive summary

Seven novel parsing approaches are proposed, each grounded in a named
comparator lacking it and a concrete instruction sequence. Three
exploit bbnf's grammar-IR substrate (shape-transition speculation,
shape-cached LazyRef, kind-separated stage-1 indices); three exploit
ISA capabilities no comparator uses (SIMD-speculative Alt-branch
prefix-match, runtime CPU-capability fn-pointer dispatch, PMC-feedback
adaptive kernels); one is a conservative alternative (Cranelift JIT
per-schema). Each lists applicability (JSON/CSS/Sheets/BBNF), cycle
budget on Apple M P-core or AVX2, novelty cross-check (sonic-rs /
simdjson / Mison / JSONSki / tree-sitter / pest / nom / serde),
integration cost, and AW-V composability. The combined portfolio
projects +1.2–1.5× over the AW-V §Novel baseline on structurally-
repetitive grammars (CSS bootstrap, tailwind) without parallel fork.

## N2.1 — Shape-transition speculation

**One-liner.** Predict the next compound's shape from the current
via an IR-mined Markov matrix; speculatively parse in parallel with
finalising the current; roll back on mispredict.

**Mechanism.** `shape_mining.rs` emits a
`shape_transition_matrix: &'static [[u8; 8]; 8]` into
`GrammarProfile` — per-grammar conditional probability P(shape_j |
shape_i), quantised u8. After compound N (shape S_N), parser reads
`matrix[S_N][*]` top-1 → predicted shape S_{N+1}. A speculative
scratchpad (48-byte arena on `TapeBuilder`) begins N+1 under predicted
shape while current `end_compound` finalises. Mispredict → `scratchpad
.reset()` (one u32 store) + reparse from `pos_before_speculate`.

**ISA grounding (Apple M P-core NEON).** On hit, speculation overlaps
~3–5 c of `end_compound` with ~3–5 c of predicted `begin_compound`
(3-wide OoO ROB naturally aliases the two boundaries). Break-even
hit-rate ≈ 0.75. CSS bootstrap transition matrix top-1 accuracy
>0.85 per ruleBlock repetition bitmaps.

**Novelty.** sonic-rs / simdjson parse compounds sequentially with
zero predictor and no IR from which matrices are derivable; tree-
sitter's incremental reparse uses node identity not shape prediction;
pest / nom / JSONSki / Mison have no analog.

**Applicability.** CSS bootstrap ≈ +8%; tailwind ≈ +10%; JSON twitter
≈ +2% (value-alt near-equiprobable); Sheets nested ≈ +5%.

**Integration.** New IR pass
`crates/ir/src/passes/recognizers/shape_transition.rs`; per-shape
emitter inserts `speculate_next_shape` before `end_compound`. Risk:
moderate (tape-writeback ordering invariant; wire-contract test with
adversarial predictor).

**Composability.** Stacks Lever 4 (column-SoA — speculation amortises
over same vector store), Lever 6 (ShapeRef dedup). N/A to Pratt /
Unordered. Conflicts with N2.2 (speculation assumes eager).

## N2.2 — Gradient parsing with shape-cached LazyRef re-entry

**One-liner.** Visitor declares per-field materialisation budgets;
over-budget subtrees emit `LazyRef` (span + shape tag); on-demand
`.parse_into()` replays only the shape-tagged slice.

**Mechanism.** `ValueVisitor` gains
`budget_for_key(&self, key_hash: u64) -> u32`. Parser tracks
`bytes_consumed_in_subtree`; over-budget → visitor receives
`LazyRef { span_lo, span_hi, shape_tag, shape_hash }` vs materialised
value. On `lazy.parse_into::<V>()`, re-entry at `span_lo` pre-
dispatches via `shape_tag` — bypasses value-dispatcher byte-match.
`shape_hash` routes through SHAPE_DICT fast-path when eligible.

**ISA grounding.** First-pass saves ~8 cycles per over-budget subtree
(16-byte LazyRef single `vst1q_u8` vs ~40-byte compound × children).
On-demand reparse: 6–10 c fixed + shape body. Pays when demand rate
< 0.85.

**Novelty.** sonic-rs `LazyValue` is JSON-only, no budget + no shape-
cache. simdjson On-Demand is lazy but single-grammar + no shape-cache.
serde `RawValue` is token-string only, not shape-tagged. tree-sitter
/ pest / nom / JSONSki / Mison: no analog. Our gradient is
cross-grammar (JSON / CSS / BBNF / Sheets).

**Applicability.** Any workload ignoring >30% of input subtrees —
serde `#[serde(skip)]`, CSS-`@media`-only consumers, BBNF rule-names-
only consumers. +20–40% on ignoring workloads; neutral otherwise.

**Integration.** `crates/bbnf-tape/src/lazy.rs` + `ValueVisitor`
budget method. Risk: moderate (re-entry must be referentially
transparent).

**Composability.** Stacks all AW-V Levers; most productive with Lever
3 (multi-key compare guides budget decisions). Conflicts N2.1.

## N2.3 — Kind-separated stage-1 indices (Mison-refined)

**One-liner.** Stage-1 emits one position-vector per structural-byte
kind; per-shape loops consult the right kind-index in O(1).

**Mechanism.** Today: `StructuralIndex { positions, kinds }`.
Extension: `StructuralIndexKinds { delim_kinds: [Vec<u32>; K] }`
where index_k holds positions of byte class k. `parse_object`
consults `delim_kinds[IDX_COLON]`; `parse_array` consults
`delim_kinds[IDX_COMMA]`. Stage-1 compaction cost neutral (same
cmpeq stripe scan; tzcnt step widens to K streams but each stream is
K× sparser).

**ISA grounding (Apple M).** Per-dispatch savings: `ldp x0, x1,
[pos]` (2 c) replaces `ld1q + cmpeq + tbl + shrn + vget_lane + tzcnt`
(~6 c avg). Gain ~4 c × scans-per-shape. Twitter object-shape
dispatches ~4 times per key-value (open-quote, close-quote, colon,
comma/brace); 4 × 4 c × 13K pairs ≈ +3% twitter.

**Novelty.** Mison does kind-separated *logical bitmaps* but
collapses at query time — it's a query engine, not a shape-parser
substrate. simdjson collapses all structural bytes into one bitmap.
sonic-rs / JSONSki / tree-sitter / pest / nom / serde: no analog.

**Applicability.** All delimited grammars. CSS L4 (7 classes)
benefits most; JSON (6); Sheets (5); BBNF (4–5). +3–7% across five
parse benches.

**Integration.** `crates/bbnf-simd-scan/src/compaction.rs` extends
`compact_stripe_tzcnt` to K streams; `bbnf-tape/src/stage1.rs` adds
kind-indexed view. Risk: low. Memory cost neutral (same total u32
positions, K-partitioned).

**Composability.** Stacks Lever 2 (kind-separation is another kernel
dimension), N2.4 (Alt lane dispatch consumes kind-specific next
positions), and all per-shape loops.

## N2.4 — SIMD-speculative Alt-branch parallel prefix-match

**One-liner.** Pack up to 16 branch prefixes into a 16-byte lane;
one `vceqq_u8` + `shrn`+`extract` picks the winner in ~3–5 cycles
regardless of branch count.

**Mechanism.** For Alt of disjoint-FIRST branches with prefix ≤ 2
bytes, codegen emits a 32-byte compile-time table (16 branches × 2
bytes). NEON: `vld1q_u8(prefix_table)` (1 c), `vld1q_u8(input_window)`
(1 c), `vceqq_u8` (2 c), `vshrn_n_u16 #4 + vget_lane_u64` (2 c),
`tzcnt` (1 c) — branch dispatch in ~5 c regardless of count.

**ISA grounding (AVX2 Skylake).** 32-branch: `vmovdqu + vpbroadcastb
+ vpcmpeqb + vpmovmskb + tzcnt` = 7 c. Break-even at ≥ 4 branches
vs today's linear `cmp/je` chain (3–7 c order-luck).

**Novelty.** sonic-rs Alt via LLVM monomorphisation emits linear
`cmp/je` chain — no SIMD. simdjson stage-2 uses 256-entry jump-table
(O(1) i-cache cost, ignores byte 2+). pest / nom / tree-sitter use
parser-combinator linear chains. JSONSki skips substructures but no
SIMD for branch discriminator.

**Applicability.** CSS `compoundSelector` (5 branches); BBNF
`directive` (8); Sheets `error_literal` (9); JSON `value` (6).
+2–6% on these specific Alt rules.

**Integration.** `crates/core/src/backend/rust/emitter/shapes/
alt_simd.rs` — emitted when Alt width ∈ [4, 16] ∧ all prefixes ≤ 2
bytes. Risk: low.

**Composability.** Composes with Lever 3 (object-internal analog),
N2.3 (kind-separated indices pre-narrow).

## N2.5 — Runtime CPU-capability auto-dispatch with cached fn-pointer

**One-liner.** Codegen emits 5 kernel variants (NEON u8x16, AVX2
u8x32, AVX-512 VBMI2, SVE2, scalar); main() detects via CPUID /
getauxval / sysctlbyname; caches fn-pointer per kernel slot; ~0 c
overhead after initial dispatch.

**Mechanism.** `static KERNELS: [AtomicPtr<()>; NUM_KERNELS]`. LLVM
`.init_array` hook: CPUID on x86; `getauxval(AT_HWCAP) + AT_HWCAP2`
on Linux aarch64; `sysctlbyname("hw.optional.neon")` on macOS. Select
best per slot; `KERNELS[i].store(variant_fn)`. Runtime: `(KERNELS[i]
.load(Relaxed))(input)` — one indirect call, branch-predicted after
first invocation (0–1 c on M P-core OoO).

**ISA grounding.** Per-call overhead 0–2 c; invisible vs ~1 cyc/B
parser. Size cost: 5 × ~500 B/shape × 7 shapes = ~17.5 KB — fits
M L1-I 192 KB. Single binary serves NEON Apple M / NEON Graviton /
AVX2 Skylake / VBMI2 Ice Lake / WASM simd128.

**Novelty.** sonic-rs compile-time `#[cfg]`. simdjson runtime-
dispatches (haswell / icelake / westmere) but JSON-only, not grammar-
parameterised. tree-sitter no SIMD. WasmEdge JIT is runtime code-gen
not kernel-selection.

**Applicability.** Distribution artefacts (crates.io, npm, PyPI)
where target CPU is unknown. +10–25% on Ice Lake / Graviton 3 users
vs today's NEON-baseline binary.

**Integration.**
`crates/bbnf-simd-scan/src/runtime_dispatch.rs`. Risk: moderate
(static-init ordering proof; fallback to scalar on race).

**Composability.** Orthogonal every AW-V lever — a per-deployment
multiplier. Most productive with Lever 2 (kernel-shape selection
becomes runtime-CPU × grammar-IR cross-product).

## N2.6 — PMC-feedback adaptive kernel selection

**One-liner.** Runtime performance-counter reads after first N bytes
inform which kernel variant the rest uses; self-tuning per input
distribution.

**Mechanism.** On Apple M / Linux: `mrs x0, pmccntr_el0` or `rdpmc`.
After initial 1024 B with default kernel K_0: compute
`cycles_per_byte_observed`; if > `kernel_profile[K_0].expected +
10%`, switch to K_1 for remainder. Per-grammar IR codegen emits a
`kernel_profile` table with expected cyc/B per kernel per input
shape.

**ISA grounding.** PMC read on M: 1 c. Per 1024-B evaluation: ~12 c
total (read before + read after + delta + branch). Break-even at
input ≥ 10 KB; sub-10 KB stays on K_0.

**Novelty.** No OSS parser uses PMCs for kernel selection. simdjson
/ sonic-rs benchmark at compile time; tree-sitter incremental is
input-aware not cycle-aware; JSONSki is workload-aware via structural
metadata not hardware counters.

**Applicability.** Cross-workload dynamic dispatch — same grammar
may favour NibbleLut on low-density CSS but MultiCmp on pseudo-heavy
CSS. +5–15% mixed; neutral single-distribution.

**Integration.** `crates/bbnf-simd-scan/src/pmc_adaptive.rs`. Risk:
moderate (Linux may need `perf_event_open` fallback for unprivileged
PMC; macOS `mach_absolute_time` unprivileged; graceful degradation
to static on no-PMC).

**Composability.** Stacks N2.5 (PMC-feedback selects within runtime-
dispatched pool), Lever 2. Orthogonal others.

## N2.7 — Cranelift JIT per-schema parser

**One-liner.** For workloads re-parsing the same schema many times,
Cranelift JIT-specialises the parser to observed schema at second
parse; third+ calls JIT-compiled function.

**Mechanism.** Visitor with `#[derive(JitParse)]`: first parse emits
an IR profile (observed field hashes, value-type distribution).
Second parse triggers Cranelift JIT producing specialised parser with
value-dispatcher Alt collapsed to observed set. Third+ calls the
JIT function. JIT target is `__dta_walker_inline::run` with
schema-specific dispatch collapsing.

**ISA grounding.** Cranelift JIT cost ~10 ms (40M cycles) one-time
for 2000-LOC parser. Static-codegen baseline ~1 cyc/B; JIT
specialised ~0.6 cyc/B. Break-even at ~1000 parses × 100 KB = 100 MB
total throughput.

**Novelty.** sonic-rs / simdjson: zero JIT. tree-sitter incremental
reuses tree, no JIT. serde has compile-time `#[derive]` not runtime
JIT. PikeVM / hyperscan JIT regexes, not parsers. No OSS JSON parser
JIT-compiles; ours is grammar-parameterised.

**Applicability.** Data-pipeline (ETL, streaming analytics, log
ingestion) with static schemas. +30–80% over static codegen on
schema-heavy; adds 10 ms first-parse latency.

**Integration.** `crates/bbnf-jit/` (new workspace member) via
`cranelift-jit 0.105`. Risk: high (JIT correctness requires emitter
reproducibility from IR profile; function-pointer lifetime model;
gate behind `--features jit`).

**Composability.** Conflicts AW-V static-codegen invariant — best as
*additional* deployment mode (static for binaries, JIT for services).
Stacks N2.5 cleanly.

## Appendix A — Per-proposal cycle-budget derivation

### N2.1 speculation
- `end_compound` latency: 3 c (vector store + SoA column advance).
- `begin_compound` latency: 3 c (allocate + shape-tag write).
- 3-wide OoO overlap: 2 c → saves 1 c/compound on hit.
- Mispredict: 4 c (scratchpad reset + reparse 1–4 byte prefix).
- Break-even hit rate ≥ 0.80; CSS ruleBlock measured 0.85+.

### N2.2 gradient
- First-pass skip savings: 7 c × `skip_rate` (compound × 7 cols × 4 B).
- Re-entry: 10 c fixed + shape body.
- Net win: demand_rate < 0.85.

### N2.3 kind-separated
- Per-dispatch: 4 c saving (one load vs SIMD cmpeq chain).
- Scans/shape: ~4 object, ~2 array.
- Stage-1 cost delta: neutral.

### N2.4 SIMD Alt
- NEON: 5 c (vld1q + vceqq + shrn + extract + tzcnt).
- AVX2: 7 c (vmovdqu + vpbroadcastb + vpcmpeqb + vpmovmskb + tzcnt).
- Linear baseline: 3–7 c order-luck.
- Break-even branch_count ≥ 4.

### N2.5 runtime dispatch
- Cached fn-pointer indirect-call: 0–1 c predicted.
- Init dispatch (once): ~100 c CPUID + getauxval + table fill.

### N2.6 PMC-feedback
- PMC read: 1 c.
- Evaluation per 1024 B: ~12 c.
- Break-even input ≥ 10 KB.

### N2.7 JIT
- Cranelift JIT cost: ~10 ms one-time.
- Static baseline: ~1 cyc/B.
- JIT specialised: ~0.6 cyc/B.
- Break-even ~1000 parses × 100 KB = 100 MB.

## Appendix B — Novelty cross-check table

| Proposal | sonic-rs | simdjson | serde | tree-sitter | pest | nom | JSONSki | Mison |
|---|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
| N2.1 Shape-transition speculation | no | no | no | no (node-identity only) | no | no | no | no |
| N2.2 Gradient + shape-cached | partial (LazyValue, JSON-only, no budget, no shape-cache) | partial (On-Demand, no shape-cache) | partial (RawValue string-only) | no | no | no | partial (skip API, no shape-cache) | partial (query-oriented) |
| N2.3 Kind-separated stage-1 | no | no (collapsed bitmap) | no | no | no | no | no (intervals) | partial (bitmap-per-class but collapsed at query) |
| N2.4 SIMD-speculative Alt | no | partial (256-entry first-byte jump-table) | no | no | no | no | no | no |
| N2.5 Runtime CPU auto-dispatch | no (compile-time cfg) | yes (JSON-only, haswell/icelake/westmere) | no | no | no | no | no | no |
| N2.6 PMC-feedback adaptive | no | no | no | no | no | no | no | no |
| N2.7 Cranelift JIT per-schema | no | no | no | no | no | no | no | no |

**Summary.** N2.1 / N2.4 / N2.6 fully novel (zero comparator). N2.3
overlaps Mison bitmap-per-class but Mison collapses at query time —
ours exposes position-stream-consumable vectors to shape-parsers.
N2.2 extends sonic `LazyValue` with budget + shape-cache (neither
sonic nor simdjson On-Demand has both). N2.5 extends simdjson runtime-
dispatch from JSON-only to grammar-agnostic + kernel-parameterised.
N2.7 applies Cranelift JIT patterns to grammar-parameterised parsing
(no comparator).

## Composition matrix vs AW-V Levers

| | L1 shape-mined | L2 kernel-sel | L3 multi-key | L4 SoA | L5 bounded-regex | L6 ShapeRef | L7 multi-visitor |
|---|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
| N2.1 | stack | stack | stack | amortise | neutral | stack | stack |
| N2.2 | stack | stack | stack | neutral | stack | stack | stack |
| N2.3 | stack | stack | neutral | neutral | stack | neutral | neutral |
| N2.4 | stack | stack | orthogonal | neutral | neutral | neutral | neutral |
| N2.5 | stack | stack | stack | stack | stack | stack | stack |
| N2.6 | stack | stack | neutral | stack | neutral | neutral | neutral |
| N2.7 | conflict | — | — | — | — | — | — |

**Preferred landing order.** N2.3 first (smallest delta, composes
with everything) → N2.4 (emitter-local) → N2.5 (orthogonal
multiplier) → N2.1 (speculative, needs tape-replay invariant) →
N2.2 (requires visitor API redesign). N2.6 and N2.7 defer to a
successor tranche as self-tuning infrastructure.
