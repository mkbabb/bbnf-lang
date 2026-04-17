# AW-III R1 — simdjson / sonic-rs cycle attribution & bbnf gap map

Source-level grounding for `arch-comparison.md`. Numeric claims cite
arXiv:1902.08318v7 (Langdale & Lemire), named commit hashes, file paths
in this worktree, or instruction-set reference manuals.

## 1. Per-stage cycle budget (simdjson, published)

Paper §4.3 regression fit, Skylake 3.4 GHz:

- Stage 1 (structural index): **`1.7·S + 0.62·B`** c (S=struct chars, B=bytes).
- Stage 2 (tape build):       **`19·F + 8.7·S + 0.31·B`** c.
- Total:                      **`19·F + 11·S + 0.92·B`** c/doc.

Instruction counts (§4.4, Tbl 8): **8.3 instr/byte** vs sajson 14.7,
RapidJSON 18.7. Throughput (§4.5, Fig 9a): gsoc-2018 **3.2 GB/s**,
twitter **2.2 GB/s**; six files >2 GB/s on Skylake AVX2.

(a) **AVX-512 Ice Lake** — not in paper; icelake kernel uses VBMI2
compress-store; ~4–5 GB/s on twitter per simdjson CI.

(b) **AVX2 Skylake** — stage 1 ≈ 0.62 c/byte → ~5.5 GB/s
pure-structural. Stage 2 for twitter (S≈7%, F≈1%):
`0.31 + 8.7·0.07 + 19·0.01 ≈ 1.11` c/byte → 3.1 GHz/1.11 = 2.8 GB/s;
paper reports 2.2 GB/s, consistent.

(c) **Apple M1/M2/M3 NEON** — paper §5 footnote 15 notes ARM support
with no figures. Published M1 minify **7.8 GB/s** (simdjson#1658,
jkeiser). Full twitter parse ~2.0–2.5 GB/s at 3.2 GHz Firestorm.

## 2. Hot-loop intrinsics

**x86 stage 1** (paper §3.1 + branchfree 2019-03-06):
`_mm256_loadu_si256` → `vpcmpeqb` against `{`, `}`, `[`, `]`, `:`, `,`,
`"`, `\` (or `vpshufb` low-nibble LUT collapsing N cmpeqs to 1 shuffle)
→ `vpmovmskb` → 32-bit mask → OR halves → 64-bit bitmap/block.

**Quote-state via CLMUL** (§3.1.1):
```
Q = pmovmskb(cmpeq(input, '"'))         // quote bits
Q'= Q & ~escape_mask                      // backslash-filtered
R = clmul(Q', 0xFFFF...F)                 // parallel prefix-XOR
in_string = R XOR prev_iter_in_string
S = classify_structural(input) & ~in_string
```
PCLMULQDQ latency 6, recip throughput 1 on Skylake (branchfree). One
CLMUL per 64 bytes. Accounts for ~0.1 of the 0.62 c/byte.

**PDEP correction**: paper §3 does NOT name PDEP. simdjson's
`flatten_bits` uses `tzcnt` + branch unroll, NOT pdep. sonic-rs
similarly. `arch-comparison.md` line 47's "`_pdep_u64` / PEXT → indices"
is misattributed; the actual mechanism is tzcnt-loop.

**NEON (AArch64)** — no pmovmskb, no PCLMULQDQ:
- `vceqq_u8`: L=2, recip-tp 0.25 on Firestorm (firestorm-simd.html).
- Movemask substitute: `vshrn_n_u16(mask, #4)` narrows each 16-bit
  lane to a 4-bit nibble; `vget_lane_u64` extracts a 64-bit int where
  each matching byte occupies 4 repeated bits (Arm Community Blog
  2022). Trailing-zero count divided by 4.
- `vqtbl1q_u8`: 16-byte LUT shuffle.
- Prefix XOR emulation: 6-op shift-XOR ladder (`x^=x<<1;…;x^=x<<32`)
  per 64-bit block. Both sonic-rs NEON and simdjson ARM use this.

**bbnf has none of this on the hot path.** The prior
`parse-that::filter_quote_parity` (`docs/tranches/AO/AO.md:75-84`,
`AP/AP.md:489`) was a scalar byte walk tracking `in_string` through an
index array. **Not CLMUL, not shift-XOR.** Deleted in commit
**`2f7c1bd4`** (AQ.5 "delete structural dispatch infrastructure") after
net-regression vs AP.3.1's SIMD WS bitmap. bbnf never had real
SIMD quote-parity. Genuine gap.

## 3. Apple M-series upper bound

Firestorm P-core: 8-wide decode; 4× 128-bit NEON FP units (eclectic
light co / dougallj); 3 loads/cycle (Wikipedia M1). 3.2 GHz.

Stage-1 op budget per 16-byte SIMD lane: 1 load + 4 cmpeqs + 1 shrn +
6 shift-XOR (parity) + 1 extract + 1 AND ≈ **13 SIMD ops / 16 B**. At
4 SIMD ops/cycle (two sustained 128b FP units): 13/4 ≈ 3.25 c / 16 B
= **0.20 c/byte → 16 GB/s stage-1 ceiling at 3.2 GHz.**

Reality: simdjson M1 minify 7.8 GB/s = 0.41 c/byte (discussion #1658);
full twitter parse ~2.0–2.5 GB/s = 1.3–1.6 c/byte (stage 2 dominates).
M2/M3 P-core ceiling: ~20–25 GB/s stage 1, ~4 GB/s full parse. bbnf's
current twitter 120 MB/s (perf-01-json.md:73) is **17× under full
ceiling, 133× under stage-1 ceiling**.

## 4. bbnf deficit map (post-DTA, post-AW-II)

For each simdjson/sonic-rs technique I re-inventory against current
HEAD (master `f34531e7`), superseding AR-audit §2 which was against
fn-per-rule RD.

| Technique | bbnf today | simdjson/sonic-rs | Gap cycles/byte | Addressed in |
|---|---|---|---|---|
| Structural SIMD bitmap | **ABSENT** after AQ.5 deletion at `2f7c1bd4`; `crates/core/src/generate/regex/emit/simd.rs:3-9` | stage 1 kernel | ~+0.6 c/B lost | NEW AW-III.W5.5 (arch-comparison.md line 575) |
| CLMUL quote-parity | absent; prior scalar path deleted | PCLMULQDQ 1 op / 64B on x86, 6-op emulation on NEON | ~+0.1 c/B | Not scoped; implicit in W5.5 |
| Dispatch loop | `dispatch_one` match on `table.states[state_idx]` — 20+ arms, jump-table `crates/bbnf-tape/src/driver.rs:852-867`. Every byte visits it. | sonic-rs: 2 hot loops (`parse_object`, `parse_array`) = **90%+ of cycles** (perf-05 table) | dispatch ≈25% self-time everywhere (SYNTHESIS.md §1) | AW-III.W5.6 codegen-specialised walker |
| Scanner closure | `cached_dfa(pattern)` per scan → `HashMap<String,Arc<Dfa>>` + Sip13 hash `crates/core/src/backend/rust/emitter/grammar.rs:244`, twin at generated.rs:14094 | Inline DFA on stage-2 arm | 13–33% self-time on twitter/CSS (perf-01) | AW-III.W1.8 scanner closure (SYNTHESIS line 109) |
| PCLMULQDQ for escape-in-quote | absent | CLMUL chain | minor | Not scoped |
| Tape writes (fused) | `reserve_compound` = **7 separate `Vec::push`** (arch-comparison.md §SoA 233-243) | simdjson: 1×`u64` store per record | 6–19% self-time on citm/BBNF (perf-01, perf-04) | AW-III.W5 fused writes |
| Lazy value decode | Eager f64 decode (PSI path `psi::write_decoded`) | simdjson lazy `as_double()` | 1–3% (PSI share) | not load-bearing |
| PDEP bit extraction | N/A (no bitmap) | `tzcnt` loop (NOT `pdep` — paper §3) | — | — |
| Padded 64-byte buffer | absent (AR-audit row) | `x"x\0…` pad | modest bounds-check elim | AR proposal #4 (deferred) |
| Tape cap heuristic | `input.len()*4` (AR-audit line 63) | `len/2 + 2` | waste only, not c/B | AR proposal #1 |

Two genuine **AW-III.W5.5-class** gaps remain:
1. Stage-1 SIMD structural bitmap (with SWAR/shift-XOR or CLMUL
   parity). Not in current AW-III/IV scope formally; `W5.5` proposed
   in `arch-comparison.md` line 575 but not yet in the wave plan.
2. CLMUL-parity quote masking — architecturally implied by (1) but
   separately called out because of aarch64 emulation cost.

## 5. Is dispatch_one architecturally fundamental? No.

SYNTHESIS.md §1 labels `dispatch_one` "canonical state-machine-
interpreter overhead" with 24% self-time floor. The question: does
sonic-rs have an equivalent?

**Answer: sonic-rs does NOT have a dispatcher.** Its `parse_object`,
`parse_array`, `parse_string`, `parse_number` are MONOMORPHIC hot loops
over `PaddedSliceRead` (perf-05 table). LLVM inlines each fully. The
**function boundary IS the dispatcher**, but it's resolved at compile
time, not at each byte. Decomposition of sonic-rs speed:

- (a) Total absence of byte-level dispatch — **YES**. Dispatch lives in
  the call graph at function-entry time, not per byte. Contribution
  estimate: removes the ~25% dispatch_one floor directly.
- (b) PEXT-flattened structural index — **PARTIAL**. sonic-rs does
  maintain a structural bitmap in its `Stage1Scanner` but the stage-2
  loop iterates bytes directly between structural events, not a
  `Vec<u32>` index. Contribution: ~15% (fewer branches in the walker's
  inner scan).
- (c) LLVM inlining of grammar-specialised functions — **YES**. This
  is the dominant win. `parse_object::<DocumentVisitor>` is
  monomorphised at the visitor type, so every literal field access in
  the visitor inlines into the parse function. Contribution:
  ~50–60% of the remaining gap.
- (d) SIMD stage 1 + cache-friendly arena — ~10% marginal.

**Proportioning**: of sonic-rs's 17–36× lead over bbnf (perf-05 table),
~1.3× comes from stage-1 SIMD, ~2× from dispatch-elimination (a+c),
~1.4× from fused writes / arena, ~1.2× from scanner inlining.

## 6. Bottom line

**DTA is conceptually capable of reaching the sonic-rs speed class.**
Four architectural changes are MINIMUM:

1. **Codegen-specialised per-grammar walker**
   (`crates/core/src/backend/rust/emitter/grammar.rs:~244` — currently
   emits a single `DtaDfaScanner` + trait-object `dta_run`; must emit
   `fn dta_run_json`/`_css`/`_bbnf` with all `DtaState` arms as
   inlined control flow). **Largest single lever** — eliminates
   `dispatch_one` as a leaf.
2. **Scanner closure hoisted onto `DtaState::Regex`**
   (`crates/bbnf-tape/src/dta.rs:100-104` — add `pattern_dfa:
   &'static Arc<Dfa>` field; lift-time populate; remove
   `cached_dfa(pattern)` call at driver.rs:894).
3. **Stage-1 SIMD structural scan** (new crate `bbnf-simd-scan` per
   arch-comparison.md §Cat A line 413; per-grammar emitter pass reading
   `IR::structural_alphabet` already in
   `crates/ir/src/passes/sets/structural_alphabet.rs`). This is the
   AW-III.W5.5 that is NOT yet formally scheduled. Without it, bbnf
   chases a 10–20× gap instead of 3–5×.
4. **Fused SoA write path** (one capacity check + 7 unchecked stores;
   arch-comparison.md §233-267). Landable in AW-III.W5.

Properties bbnf can NEVER match without abandoning DTA:
- **Nothing structural** — every simdjson/sonic-rs technique has a
  DTA-compatible analog. Grammar-specialised codegen (#1) is the
  architectural discriminator; DTA already permits it.

The claim in SYNTHESIS.md that `dispatch_one`'s 24% is "canonical
state-machine-interpreter overhead" is **correct for the current
interpreter-style driver** (driver.rs:852 is literally a match over
`table.states[state_idx]`). It is NOT canonical for DTA-class
architectures; it is a property of the concrete implementation. The
AW-III.W5.6 codegen-specialised walker eliminates it.

**Ceiling with all four**: ≈0.8–1.2 c/byte on twitter (sonic-rs parity
or +10%). bbnf's 120 MB/s → ~2500 MB/s ceiling on M2 P-core;
arch-comparison.md §472 projects 1650–2290 MB/s with full AW-IV,
consistent with this analysis once W5.5 is added. Without W5.5, ceiling
drops to ~800–1200 MB/s (scanner+dispatch fixes alone).

## Citations

arXiv:1902.08318v7 §3.1.1, §4.3-4.5, §5. branchfree.org 2019-03-06
(CLMUL). Arm Community Blog 2022 (NEON movemask). dougallj
firestorm-simd.html. simdjson#1658 (M1 7.8 GB/s).
`crates/bbnf-tape/src/dta.rs:93-202`,
`crates/bbnf-tape/src/driver.rs:852-1348`,
`crates/core/src/backend/rust/emitter/grammar.rs:244`,
`crates/core/src/generate/regex/emit/simd.rs:3-9`,
commit `2f7c1bd4`. perf-01-json.md:73,85-104; perf-05-json-value.md:86-114;
SYNTHESIS.md §1; arch-comparison.md §§29-200,413,575; AR audit-sonic-gap.md §2.
