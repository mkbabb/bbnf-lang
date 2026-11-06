# 02 — The modern-SIMD frontier (web + code), mapped onto both repos

Scope: the 2026 state of NEON/SIMD parsing — Hyperscan/Vectorscan classification lineage,
the vshrn idiom family, SIMD number parsing, and simdutf/simdjson stage-1 developments —
mapped concretely onto bbnf-lang's kernel plane (`crates/simd-scan`, `crates/core` regex
emit) and parse-that's regex engine (`rust/regex`) and scanners (`rust/parse_that/.../scan`).
Every item is placed by WHERE it lands, its expected win class, and its measurement gate,
under the dav1d discipline settled in SK-V21 research 25 (intrinsics default, per-kernel asm
gate, checkasm-liveness) and the settled negatives (no universal pre-scan presize; hand asm
obviated on classify).

The load-bearing frame carried forward: on Apple silicon in 2026 the large SIMD wins are
*algorithmic* (SIMD-vs-scalar, 4–10×), not asm-vs-intrinsics (0–5% except at register-pressure
failures), and — for a full-materialization JSON/CSS target — the ceiling lives in
value materialization and number parsing, not structural classification (SK-V21 research
22 §2, §4; canada corpus stage-2 1.847 c/B vs stage-1 0.547 c/B). W2 therefore prioritizes
the idioms the compiler cannot synthesize and the prefilter tiers that shrink the byte
budget reaching the slow path.

---

## 1. The single highest-confidence retrofit: the vshrn movemask, at two live anti-pattern sites

SK-V21 settled the `vshrn_n_u16 #4` movemask as +35% over the alternatives; SK-V21 research
25 §2.4 documents the mechanism (3–4 instructions, 4-bits-per-byte output, replaced glibc's
`addp` approach for a 10–15% strlen SPEC gain) and the correctness trap (all downstream mask
arithmetic is in the nibble domain — `tzcount >> 2` for the byte index). The audit found that
**neither repo's hand-written NEON movemask uses it.** Both use technique #2 from that
research — the per-lane power-of-two bitmask funnelled through a `vaddv` horizontal reduction:

- **bbnf** `crates/simd-scan/src/neon.rs:690-710` — `movemask_u8x16` loads a `[1,2,4,8,…]`
  pattern, `vandq_u8`, then two `vaddv_u8` reductions (`vget_low`/`vget_high`) stitched into a
  u16. Its own doc comment (`:688-698`) *describes* the vshrn idiom then implements the vaddv
  one. This function is on every hot path: it is called by `classify_chunk_nibble`,
  `classify_chunk_wide`, `classify_stripe_multi`, `digraph_stripe`, and `raw_quotes_and_bs_stripe`.
- **parse-that** `rust/parse_that/src/parsers/scan/number_simd.rs:31-61` — `count_leading_digits_neon`
  does the identical vaddv per-bit-weight construction to count leading digits, on the number
  hot path feeding Eisel-Lemire.

WHERE it lands: bbnf `neon.rs` movemask (single function, fan-out to all five callers);
parse-that `number_simd.rs`. Both are hand-NEON, so both are inside the dav1d asm-gate and
should carry a checkasm-style parity+bench binding.

Win class: settled +35% on the movemask primitive itself; on classify-bound stripes the mask
extract is a meaningful fraction of the per-stripe cost, so a mid-single-digit-to-teens percent
whole-kernel win is the honest expectation (the vaddv reduction is a horizontal op competing
for the same pipes as the classify shuffles). Note the 4-bit-per-byte output changes the
compaction contract: `compaction::compact_stripe_tzcnt` and every `trailing_zeros()`/`<< (k*16)`
stitch in `neon.rs` must move to the nibble domain or the mask must be re-compressed — this is
the correctness trap SK-V21 25 §2.4 flags as non-optional.

Measurement gate: byte-exact parity of `StructuralIndex.positions/kinds` against the scalar
reference (`crates/simd-scan/src/scalar.rs`) across the existing `parity.rs` harness, plus a
cold per-stripe cycle bench on twitter/citm/canada. Ship only if the whole-kernel cold number
does not regress and the primitive micro-bench shows the +35%.

Adjacent note: parse-that's *portable*-SIMD sites (`structural_bitmap.rs:43`,
`quoted_simd.rs:45`, `decode.rs:83` all call `std::simd ... .to_bitmask()`) leave the movemask
lowering to LLVM, which on aarch64 does not reliably pick vshrn. These sit OUTSIDE the asm-gate
and cannot carry a per-kernel liveness binding while portable. W2 must decide per-kernel whether
each stays portable (accept LLVM's lowering, no gate) or graduates to hand-NEON with the vshrn
idiom and a checkasm binding — a greenfield delineation item, not an automatic rewrite.

---

## 2. Hyperscan/Vectorscan lineage → parse-that regex engine (the prefilter tiers)

The Vectorscan/Hyperscan classification family — shufti, truffle, Teddy, and vectorized
DFA (sheng) — is the canonical "shrink the bytes reaching the automaton" toolkit. parse-that
has the *first rung* of this ladder and three gaps.

### 2.1 shufti — present as `NibbleLut`, but ORPHANED from the DFA search

`rust/regex/src/automata/accel.rs:23-30,113-131` implements exactly shufti: two 16-byte nibble
LUTs (`lo_lut` indexed by `b & 0xF`, `hi_lut` by `b >> 4`, AND the lookups), built for
DFA self-loop states whose exit set is 4–8 bytes; `detect_accel` (`:41`) also emits
`Memchr1/2/3` for 1–3 exit bytes. This is the correct Vectorscan tiering (memchr for tiny sets,
shufti for small classes). **But the accelerator is detected and never consumed:** a grep of
`rust/regex/src/automata/dfa.rs` for `accel`/`memchr`/`nibble`/`swizzle`/`simd` returns nothing —
the DFA search loop does not call the acceleration table. `AccelStrategy`/`detect_accel` are
`pub use`d from `lib.rs:27` and consumed only by feasibility bookkeeping in `info/mod.rs`.
`accel.rs:100-106` even records that the former `ScalarLut` variant was deleted as "dead code …
detected here but never consumed by any emitter" — the *same fate* has quietly befallen the
whole accel table on the search side.

WHERE it lands: wire `detect_accel`'s `StateAccel` into the DFA search inner loop
(`rust/regex/src/automata/dfa.rs`), so a self-loop state's `Memchr*`/`NibbleLut` strategy
drives a skip-to-next-exit-byte instead of one-byte-at-a-time transitions. Its 16-byte shufti
step is precisely the `vqtbl1q`-cheap regime (SK-V21 25 §2.5: 2c latency, 4/cyc).
Win class: this is the classic `[^"\\]+`-style skip — on string-body / whitespace / comment
runs it is a 4–10× algorithmic win over per-byte DFA stepping, exactly the class Vectorscan
built shufti for. Measurement gate: match-parity of the DFA against its current per-byte search
across the regex corpus (`rust/regex/tests/`), then cold ns/byte on a `[^"\\]*`-dominated input.

### 2.2 truffle — the missing tier for arbitrary/large byte classes

shufti's nibble-LUT is capped at 8 targets because each target consumes one of 8 bits
(`accel.rs:124` `.take(8)`; `build_nibble_luts`). Vectorscan's **truffle** removes that cap:
it classifies an *arbitrary* 256-byte set with two `vqtbl` lookups — a hi-nibble set-membership
table and a lo-nibble one — testing the byte's high-nibble bit against its low-nibble bitmap.
This is the natural home for the `accel.rs:100-106` "9+ exit bytes → no accelerator" gap and
for negated Unicode-ish classes that exceed 8 bytes.
WHERE it lands: a new `AccelStrategy::Truffle` in `rust/regex/src/automata/accel.rs`, consumed
by the same DFA-search wiring as 2.1. Win class: extends the shufti skip win to the class of
states that currently fall back to per-byte (the large-exit-set self-loops), where the win is
largest because those are the states shufti *refused*. Measurement gate: as 2.1, on an input
exercising a >8-byte negated class.

### 2.3 Teddy — multi-literal prefilter, absent

parse-that extracts a single fixed literal prefix/suffix (`rust/regex/src/info/literal_prefix.rs`)
and feeds memchr-class acceleration, but has no **Teddy**: Vectorscan's SIMD multi-literal
matcher that scans for many short literals at once via nibble-shuffle buckets. It is the right
engine when a regex is an alternation of literals (`foo|bar|baz`) or when many grammar keywords
share a scan. WHERE it lands: a literal-set extractor feeding a `Teddy` prefilter in
`rust/regex/src/info` + `automata`, gated by `EngineSet` (which today only carries
`MEMCHR1/2/3`, `info/mod.rs:64-66`). Win class: a prefilter that reaches the DFA only at
candidate offsets — large on keyword-alternation grammars, none on single-literal patterns.
Measurement gate: reachable only after a corpus with alternation-of-literals patterns exists;
absent that, this is a *speculative* tier and should be scheduled behind 2.1/2.2, not with them.

### 2.4 sheng / vectorized DFA — deliberately deferred

Vectorscan's sheng packs a ≤16-state DFA transition table into a NEON register and steps it with
`vqtbl1q` (state = shuffle(state, input)). It is powerful but narrow (≤16 states) and its win
over a well-accelerated byte DFA on Apple silicon is dominated by the SIMD↔GPR crossing cost
(SK-V21 25 §2.5: ~12c each way) unless the whole search stays in-vector. Given parse-that's DFAs
are small and the 2.1 accel wiring is the unclaimed first-order win, sheng is a *research-tier*
item: prototype-only, gated behind proof that 2.1+2.2 have saturated the skip wins. This honors
the "hand asm obviated on classify" negative — sheng's value is algorithmic, not asm.

---

## 3. The idiom family (settled + frontier), per landing site

### 3.1 vqtbl LUT classification bounds — already respected; hold the line
SK-V21 25 §2.5: `vqtbl1q`/`vqtbl2q` (≤32-byte table) is 2c/4-per-cyc; `vqtbl4q` (64-byte) is
4c/1.33-per-cyc, 3× the reciprocal cost. bbnf already sizes to this: `neon.rs` nibble path is
two `vqtbl1q` (`:219-220`), wide path is four `vqtbl1q` over paired 32-byte tables (`:348-351`) —
never a `vqtbl4q`. parse-that `structural_bitmap.rs:40-42` and regex `accel.rs` likewise stay
≤32 bytes. This is a settled invariant to *encode as a cost-model guard*, not a change: any
future classifier that would spill to a 64-byte table must prove it pays. No new work; a lint.

### 3.2 Multi-block pipelining / unrolling — the Apple-silicon ILP lever
SK-V21 25 §2.1: Apple P-cores have four 128-bit SIMD pipes; the way to saturate them is four
independent 128-bit dependency chains in flight, not wider vectors. bbnf's 64-byte stripe
(`neon.rs:44` `STRIPE=64`, four `u8x16` chunks) has the *shape* but the chunks are combined
through a serial `mask |= m << (k*16)` fold (`classify_stripe_nibble:200-204`,
`classify_stripe_wide:326-331`, `classify_stripe_multi:439-448`) that serializes on the movemask.
WHERE it lands: restructure the stripe so four classify chains run to four independent
sub-masks before the stitch, exposing ILP to the four pipes; combined with the §1 vshrn change
the stitch itself moves to the nibble domain. Win class: this is dav1d's genuine residual asm
win (SK-V21 25 §3, item 2 — loops where the register allocator serializes) — low-tens-of-percent
*only if* profiling shows the current fold is pipe-starved; otherwise ~0. Measurement gate:
`samply`/PMU per-block IPC on the stripe loop before and after; ship only on a demonstrated IPC
lift with cold-parity held. This is explicitly a "measure first" item, not a speculative rewrite.

### 3.3 Interleaved ld1x4 — the load-side companion
SK-V21 25 §2.2: Firestorm sustains 3×128b loads/cyc; `LDP` pairing is ~40% faster than separate
loads. The four per-stripe `vld1q_u8(ptr.add(k*16))` calls (`neon.rs:201,216,440`) can become one
`ld1 {v0-v3}, [ptr]` (`vld1q_u8_x4`) — one interleaved load of the whole 64-byte stripe.
WHERE it lands: bbnf `neon.rs` stripe loaders; parse-that `structural_bitmap.rs:52-58`
(portable — would need hand-NEON to express `ld1x4`). Win class: load bandwidth is *not* the
bottleneck for a 16-byte/iter scanner (SK-V21 25 §2.2 states this explicitly — 48 B/cyc peak vs
the scanner's classification-bound cost), so this is a *small* win (instruction-count / scheduling
tidiness) and only worth doing alongside §3.2. Measurement gate: same PMU block as §3.2; not a
standalone tranche item.

### 3.4 SWAR hybrids for sub-16-byte tails
The frontier idiom: for the <16-byte remainder, a SWAR (in-GPR-register) compare beats spinning
up SIMD. bbnf's tails are currently either a 16-byte SIMD loop then a *scalar per-byte* epilogue
(`neon.rs:114-187` nibble path) or a straight scalar loop (`:289-310` wide, `:407-428` multi).
parse-that dissolved its tails entirely via the padded-view contract: `structural_bitmap.rs:28,138`
reads into 64 bytes of guaranteed zero padding (`INPUT_PAD_BYTES`) so the last stripe needs no
tail loop, only a mask clamp. That padded-view approach is strictly cleaner than a SWAR tail and
is the pattern bbnf should ADOPT rather than SWAR-optimizing its scalar epilogue. WHERE it lands:
give bbnf's `StructuralIndex` scan the same trailing-pad contract parse-that uses, deleting the
scalar epilogues in `neon.rs`. Win class: removes a branchy scalar tail from every scan (helps
small documents specifically — SK-V21 22 §5 flags tiny-doc fixed cost as a simdjson weakness).
SWAR-tail is the *fallback* only where a padded view is impossible. Measurement gate: parity on
inputs whose length mod 64 ≠ 0; cold bench on small (<1 KB) documents.

### 3.5 Bulk unescape keyed on HasEsc — present in parse-that, portable to bbnf
parse-that already implements the settled "fused-sink, bulk-copy-on-HasEsc" idiom:
`rust/parse_that/src/parsers/scan/decode.rs:47-92` classifies escape+quote in one `u8x16` stripe,
takes a zero-copy Borrowed path when no backslash is present, and on the owned path copies each
escape-free run 16 bytes at a time between escapes (`:11-17,85-92`). This is the SK-V21 fused-sink
adjudication realized. WHERE it lands: bbnf has no equivalent bulk-unescape — its quote handling
(`neon.rs:498-675`) only *masks* string bodies for structural filtering; the actual string
materialization path is elsewhere and should adopt decode.rs's HasEsc-keyed Borrowed/Owned split.
Win class: SK-V21 22 §5 measures escape-heavy strings as simdjson's worst corpus (4.968 c/B,
30% cold-path); a HasEsc fast path makes the common escape-free string zero-copy. Measurement gate:
cold bench on an escape-free-dominated string corpus vs an escape-heavy one; parity of decoded
bytes against the scalar unescape.

### 3.6 The multi-quote-class scalar bubble (bbnf-specific hot-path regression)
`neon.rs:626-675` `multi_class_inside` — invoked whenever a grammar has >1 quote class (CSS uses
`"` and `'`) — is a **per-bit scalar loop** (64 iterations, inner loop over classes) sitting
inside the SIMD stripe. This is a classify-in-SIMD / decide-in-scalar-per-byte anti-pattern that
defeats the whole stripe. WHERE it lands: bbnf `neon.rs`. The two-class inside-string mask is
computable with two independent single-class prefix-XORs (`parity::prefix_xor_64`, already the
single-class fast path at `:556-562`) composed by tracking which class opened first — a vector
algorithm, not a per-bit walk. Win class: removes a 64-iteration scalar loop from every CSS
stripe — potentially large on CSS, the second headline grammar. Measurement gate: parity against
the current `multi_class_inside` output across CSS inputs with interleaved `"`/`'`; cold CSS bench.

---

## 4. SIMD number parsing SOTA

The settled adjudications (canada = Eisel-Lemire, N*=3 crossover) are *already realized* in
parse-that: `rust/parse_that/src/parsers/scan/number_f64.rs:15,32-34` calls
`eisel_lemire::compute_f64` for ≤19-digit mantissas with an integer fast path and a fast_float2
fallback. The float *conversion* is SOTA; the frontier gap is upstream of it — the **digit
scan and mantissa accumulation feeding it are not fully SIMD:**

- `number_simd.rs:31-61` `count_leading_digits_neon` uses the vaddv anti-pattern (§1 above).
- `number_simd.rs:70-102` `accumulate_digits_neon` is *SWAR, not SIMD* — it calls
  `parse_eight_digits` (scalar multiply-shift folds) and does short runs byte-by-byte. The
  Mula/Lemire "parse 8/16 digits in SIMD" technique (subtract '0', multiply-accumulate by
  positional weights in-vector) is not used.

WHERE it lands: parse-that `number_simd.rs`, both functions. The frontier technique is Lemire's
SIMD integer parse: load 16 digit bytes, `vsubq_u8` '0', multiply-accumulate against a positional
weight vector to fold 8 digits per `vmull`/`vpadd` chain, producing the u64 mantissa without the
per-byte scalar loop. Win class: SK-V21 22 §2/§4 identifies number parsing as *the* stage-2
dominator on numeric corpora (canada 1.847 c/B) — this is the highest-leverage materialization
win, directly on the frontier's own "beat simdjson cold means beat number parsing" conclusion.
Measurement gate: cold c/B on canada-class (coordinate-heavy) input; exact-value parity of the
accumulated mantissa against the scalar `parse_eight_digits` reference across random digit runs.
Eisel-Lemire itself needs no change — the win is feeding it a SIMD-scanned mantissa.

---

## 5. simdutf / simdjson stage-1 (2026) — what to absorb, what to skip

SK-V21 22 §1 established the frontier's own posture: On-Demand supersedes the tape, but its
margin comes entirely from *unread* substructure — a full-materialization target (bbnf's
standing constraint) gets none of it. The stage-1 *kernel* has not changed; 2025–26 simdjson
work is correctness hardening, not a new classifier (22 §1). Absorptions:

1. **Structural classification is already stage-1-shaped** in both repos (nibble-LUT + quote
   parity + escape mask): bbnf `neon.rs` + `parity.rs`; parse-that `structural_bitmap.rs` +
   `decode.rs`. The `parity.rs` prefix-XOR (`:143-157`) already selects PMULL vs the 6-op
   shift-XOR ladder exactly as simdjson's arm64 backend does (22 §2). This is at parity; no work.
   One gate note: `parity.rs:144` gates PMULL on static `target_feature="aes"`, not the dav1d
   *runtime* dispatch — on Apple-M-only (post-x86-abrogation) PMULL is always present, so the
   static gate is correct there, but the dav1d model wants it resolved once at init, not by
   compile-time cfg. Fold into the kernel-table dispatch, not a per-call branch.

2. **simdutf UTF-8 validation** — neither repo runs a simdutf-class vectorized UTF-8 validator on
   the input. For a JSON/CSS parser that must validate, the simdutf lookup-table validator (two
   `vqtbl` classify + continuation-count check) is the SOTA. WHERE it lands: a new validation
   kernel in bbnf `crates/simd-scan` and/or parse-that `scan`, run as a stage-0 pass or fused into
   stage-1 classification. Win class: correctness feature with SIMD throughput — only pays if the
   parser currently validates scalar-ly or defers validation (check current cost first).
   Measurement gate: cold c/B of validation on a UTF-8-heavy corpus vs the scalar validator.

3. **Skip the query-projection tricks.** JSONSki/Mison/On-Demand field-skip (22 §3) are
   explicitly *not* for a full-materialization typed-AST target (22 §4, `feedback_preserve_rich_ast`).
   Do not schedule. The one transferable idea — grammar-rule-compiled skip for provably-unread
   positions — is an IR/codegen item, not a SIMD-kernel item, and belongs in a different tranche.

---

## 6. Landing table (item → site → win class → gate)

| # | Item | Lands in | Win class | Gate |
|---|---|---|---|---|
| 1a | vshrn movemask | bbnf `neon.rs:690` (5 callers) | +35% primitive; mid-single→teens % kernel | parity vs scalar.rs; cold stripe bench |
| 1b | vshrn movemask | pt `number_simd.rs:31` | +35% primitive on digit count | mantissa parity; cold canada c/B |
| 2.1 | shufti wiring (accel→search) | pt `automata/dfa.rs` consume `accel.rs` | 4–10× on skip-heavy runs | DFA match-parity; `[^"\\]*` bench |
| 2.2 | truffle (>8-byte classes) | pt `automata/accel.rs` new strategy | extends skip to large-class states | as 2.1 on >8-byte negated class |
| 2.3 | Teddy multi-literal | pt `info`+`automata`, `EngineSet` | prefilter on keyword-alternation | speculative — behind a corpus |
| 2.4 | sheng vectorized DFA | pt research-tier only | narrow (≤16 states); SIMD↔GPR bound | prototype-only, behind 2.1/2.2 |
| 3.2 | multi-block ILP unroll | bbnf `neon.rs` stripe fold | tens-% *iff* pipe-starved | PMU IPC before/after |
| 3.3 | ld1x4 interleaved load | bbnf `neon.rs`; pt (needs hand-NEON) | small (load not bottleneck) | with §3.2 only |
| 3.4 | padded-tail (adopt pt pattern) | bbnf scan (delete scalar epilogues) | small-doc fixed-cost cut | parity on len%64≠0; small-doc bench |
| 3.5 | bulk unescape (adopt decode.rs) | bbnf string materialization | large on escape-free strings | decoded-byte parity; escape corpus |
| 3.6 | multi-quote vectorize | bbnf `neon.rs:626` `multi_class_inside` | large on CSS (kills 64-iter scalar loop) | CSS `"`/`'` parity; cold CSS bench |
| 4 | SIMD integer/mantissa parse | pt `number_simd.rs:70` | highest materialization leverage | mantissa parity; canada c/B |
| 5.2 | simdutf validation | new kernel, both repos | correctness + throughput (if validating) | validation c/B; UTF-8 corpus |

---

## 7. Honored negatives (carried, not re-litigated)

- **No universal pre-scan presize.** bbnf's own emitter records the deletion:
  `crates/core/src/generate/regex/emit/simd.rs:1-16` — the AO/AP whole-input `structural_cursor`
  pre-scan was "a ~190µs net-regression on citm"; v2 is stripe-local, no cursor, no pre-scan.
  The `with_capacity(input.len()/8 + 1)` heuristic in `neon.rs:68,240,371` is a *sizing* estimate,
  not a pre-scan, and stays. SK-V21 22 §4/§6.5 concurs: size from the grammar shape, not a
  reactive whole-input pass.
- **Hand asm obviated on classify.** SK-V21 25 §3: intrinsics are the default; literal asm is
  reserved for the handful of kernels where a checkasm-style measurement proves LLVM serialized
  the pipes (§3.2 is the only candidate here, and it is measurement-gated). No item above proposes
  hand asm as the *first* move on any classify kernel.
- **On-Demand/query-skip inapplicable** to a full typed-AST target (§5.3).

## 8. The delineation this implies (greenfield vs carry)

- **Carry, retrofit in place:** bbnf `parity.rs` (at parity with simdjson arm64 — only fold the
  PMULL gate into runtime dispatch); parse-that `decode.rs` bulk-unescape and `eisel_lemire`
  (SOTA already); parse-that `accel.rs` shufti tables (correct — just orphaned, wire them).
- **Retrofit (idiom swap, same structure):** the vshrn movemask at both sites (§1); the SIMD
  mantissa accumulate (§4); the multi-quote vectorization (§3.6).
- **Greenfield (new kernel/tier):** truffle + Teddy tiers in parse-that regex (§2.2, §2.3);
  simdutf validation (§5.2); the DFA-search accel *consumption* wiring (§2.1 — the accel table
  exists but the search that should call it does not).
- **Cross-repo hygiene:** parse-that's portable-`std::simd` scanners (`structural_bitmap.rs`,
  `quoted_simd.rs`) sit outside the dav1d asm-gate; each needs a per-kernel decision — stay
  portable (no liveness binding) or graduate to hand-NEON with vshrn + checkasm. This is the
  single largest "delineation" surface the SIMD axis exposes.

## Sources (internal)

- bbnf: `crates/simd-scan/src/neon.rs:44,68,190-228,316-358,435-451,498-675,690-710`;
  `crates/simd-scan/src/parity.rs:39-200`; `crates/core/src/generate/regex/emit/simd.rs:1-60`.
- parse-that: `rust/regex/src/automata/accel.rs:15-131`; `rust/regex/src/automata/dfa.rs`
  (no accel consumption — grep-negative); `rust/regex/src/info/mod.rs:64-66,175-377`;
  `rust/regex/src/info/literal_prefix.rs`; `rust/parse_that/src/parsers/scan/number_simd.rs:31-136`;
  `.../scan/number_f64.rs:15,32-34`; `.../scan/structural_bitmap.rs:28,37-60,138`;
  `.../scan/decode.rs:11-17,47-92`; `.../scan/quoted_simd.rs:45`.
- Frontier priors: SK-V21 research `25-sota-dav1d-neon.md` (§2.1 four-pipe ILP, §2.4 vshrn +35%,
  §2.5 vqtbl bounds + SIMD↔GPR cost, §3 asm bound, §4 checkasm-liveness); `22-sota-simdjson.md`
  (§1 On-Demand, §2 arm64 stage-1, §4 cold-parse decomposition, §5 weaknesses, §6 extractables).
