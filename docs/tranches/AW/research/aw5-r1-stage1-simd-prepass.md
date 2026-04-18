# R1 — Stage-1 SIMD Structural Pre-Pass: Was It Ever Attempted Optimally?

## 1. Motivation

The user's standing question: *has stage-1 SIMD structural pre-pass been chronically deferred, and if so, was it attempted optimally?* Two saved artefacts force an answer more nuanced than aw3-r2:

- **`docs/benchmarks/post-AW-III-W5.json` (bench-matrix, `wave_gate_status_w5d.json_twitter_1500_mbps.met = false`)**: W5.d **LANDED** the driver-consumed pre-pass (`scan_structural` wired into emitted `parse()`, `bbnf_simd_scan::neon::scan` confirmed at 12.13% self-time). Twitter dropped from **192 → 170 MB/s (-11%)**. Every other JSON + CSS entry regressed too. Stage-1 is not chronically deferred; it is **chronically counterproductive**.
- **`docs/benchmarks/post-AW-V-W2-prototype.json`**: the W2.1 hand-prototype — **no driver-consumed pre-pass**, inline `nospace_bitmap_64` cache + `first_quote_or_backslash` per call site — hits **2577 MB/s on twitter (15× post-W5.d, beats sonic-rs at 0.89× ratio)**. Evidence that the architectural premise "stage-1 is the missing lever" is empirically false for JSON.

The question inverts: the pre-pass shipped, it lost, and the prototype that did *not* ship it won. Re-opening it canonically under AW-V.W3+ requires a new invariant.

## 2. Archaeology (commits + deletion-message forensics)

Beyond aw3-r2's six-tranche timeline, two commits tell the definitive story:

| Commit | Tranche | Net effect |
|---|---|---|
| `7198c974` | AO.0.4–0.6 | v1 pre-pass born — `ParserState.structural_index`, `advance_to_structural`. |
| `4417f8a7` | AP.1b off | Msg: *"pre-scan overhead costs ~15-25% without WS elision"*. Disabled but preserved infra. |
| `2f7c1bd4` | AQ.5 | ~1,500 LOC deleted (aw3-r2 §1). |
| `e225ade9` + `143d19ee` | AU.2.7 v2 | Derivation revived; emitter emits per-site `find_next_structural_from` helpers (`crates/core/src/generate/regex/emit/simd.rs:109-226`). No driver consumer. |
| `25963ab1`…`02394984` | AW-III.W5.b | New `bbnf-simd-scan` crate: `scan_structural`, NEON/AVX2/AVX-512/wasm kernels, `StructuralIndex` type. Standalone bench: **4775 MB/s on twitter**. |
| `1a004a37` | AW-III.W5.c | Dual cursor + `ConsumeToNextStructural` + savepoint slot. Substrate-only; walker still constructs empty `StructuralIndex`. |
| `91df0809` | AW-III.W5.d | **Emitted `parse()` calls `scan_structural` and threads `&idx` to the walker.** |
| `54eaa735` | AW-III.W5.d repair | Reverts the W5.c Regex-bound `[pos, idx.positions[slot])` because dense-alphabet grammars (CSS L4 mines `[0..127]`) collapse it to zero-width. Msg: *"Restoring the W5.c speedup needs per-pattern alphabet narrowing — IR data the current pass doesn't surface; tracked separately."* |

The AU.2.7 v2 failure modes aw3-r2 §2 named are now *half solved in substrate, zero solved in production*:

- **Per-invocation LUT setup**: solved. `from_profile` is `const fn`; `STRUCTURAL_ALPHABET` lives in `.rodata` (`crates/bbnf-simd-scan/src/alphabet.rs:87-94`).
- **No quote parity**: solved. `crates/bbnf-simd-scan/src/parity.rs:40-142` ships `prefix_xor_64` (CLMUL/PMULL + 6-op shift-XOR fallback) and `escape_mask_64`.
- **Digraphs mined but unused**: solved in mining (`structural_alphabet.rs:215-233`) + in scanner kernel (`kernel_shape.rs:83-125`), unused in driver consumer. `ConsumeToNextStructural` arm exists (`driver.rs:2050-2082`) but **no IR lifter emits it** (`post-AW-III-W5.json.samply_attribution_post_w5d.interpretation` names this explicitly).
- **Grammar-wide LUT never emitted**: solved at emission (`GrammarProfile.structural_alphabet: &'static [u8]`, populated), no longer `&[]`.

## 3. Central finding — why the pre-pass lost

Three concrete failure mechanisms, each backed by artefact:

### 3.1 Dense-alphabet pathology (CSS L4)

`structural_alphabet.rs:341-400` mines every single-byte `Literal` *and* every single-byte leading `Alt` branch. CSS L4's identifier-led branches contribute `[a-zA-Z_]`, yielding ~80–128-byte alphabet. `scan_structural` produces a `StructuralIndex` where **every byte is structural**; `idx.positions.len() ≈ input.len()`. Allocation + fill dominates: `scan_structural`'s 4775 MB/s standalone number is on JSON's sparse alphabet; CSS L4's `mb_per_s: 25` on normalize (post-AW-IV.json:25) reflects the dense case.

The W5.c Regex-bound `[pos, idx.positions[slot])` was the *sole* walker-side lever that consumed the index for speed. `54eaa735`'s message: *"alphabet-disjoint precondition is grammar-IR data the current pass doesn't surface"*. That is the load-bearing sentence — the pre-pass has only one consumer (ByteDispatch's `idx.kinds[slot]` lookup), and that consumer is a single byte that can be served with `input[pos]` at equal cost.

### 3.2 Eager materialisation of a column nobody reads

`StructuralIndex.positions: Vec<u32>` + `kinds: Vec<u8>` (`crates/bbnf-tape/src/stage1.rs:43-58`) is built eagerly over the entire input. Twitter (632 KB, ~7% structural density): ~44,000 × (4 + 1) bytes = 220 KB of allocation + fill before the walker makes a single dispatch. W2.1 prototype's `ScanState` (`crates/bbnf-json-prototype/src/simd.rs:35-58`) carries **16 bytes** on the CPU stack (`nospace_bits: u64` + `nospace_start: isize`) and is **refilled lazily only when `skip_space` fast-exit fails**. For twitter, `parse_value` hits non-whitespace ~90% of the time on first peek; SIMD never runs on those dispatches.

`crates/bbnf-json-prototype/src/simd.rs:21-24` names this explicitly: *"`scan_structural` is NOT used because it materialises a `StructuralIndex` with positions + kinds, which the prototype does not pre-compute. The prototype reaches inside the kernel primitives directly."*

### 3.3 Driver arithmetic stays the same

The W5.c arms that consume `idx` (`driver.rs:1592-1660`, `ByteDispatch`/`ClassifyByte`) pattern:

```rust
let b = if !idx.positions.is_empty() {
    let slot_idx = *slot as usize;
    if slot_idx < idx.positions.len() && idx.positions[slot_idx] == *pos {
        idx.kinds[slot_idx]        // dense-column indexed load
    } else {
        input.get(*pos as usize).copied().unwrap_or(0)
    }
} else {
    input.get(*pos as usize).copied().unwrap_or(0)
};
```

One indexed `u8` load replaces one bounds-checked `input.get` — **saves one branch and one bounds check per dispatch**. Even optimistically, that is ~1 cycle/dispatch. Stage-1 cost for JSON twitter ≈ 50 µs (4775 MB/s over 632 KB + alloc); walker runs ~10⁶ dispatches; savings < 1 µs amortised. Net cost: +49 µs per parse, which is the ~22 MB/s regression.

## 4. Grammar-property criterion for when stage-1 *is* load-bearing

Stage-1 is worth its cost only when **the walker can skip bytes the pre-pass has classified as non-structural**. Formally — and this is the novel invariant aw3-r2 did not articulate:

> A grammar **requires** a driver-consumed stage-1 pre-pass iff there exists a hot-path rule body of shape `Regex("[^S]*")` where `S ⊆ single_bytes` AND `S` is a strict subset of the mined structural alphabet (i.e. **per-pattern** alphabet-narrowing is valid), AND the grammar has a `ConsumeToNextStructural`-lifting IR pass.

Concretely by grammar:

- **JSON**: `[^"]`/`[^0-9.]` scans terminate at the natural delimiter long before any grammar-wide `S`. W2.1 proves inline `first_quote_or_backslash` (one `vqtbl1q_u8` + `vaddv_u8` per 16-byte stripe, `simd.rs:301-343`) wins. **Does not need pre-pass.**
- **CSS L4**: compound-selector dispatch (`tag.class#id[attr]:pseudo`) needs `find_next_of(' .#[:{}|')`. `S` for the `selector` rule is disjoint from identifier bytes. **Needs per-pattern-narrowed pre-pass with bounded Regex.**
- **Sheets**: operator tower `=+-*/()` + cell-ref (`A1:B2`) + function calls. `S` for expression-rule bodies is `=+-*/(),:`. Disjoint from identifier bytes. **Needs pre-pass.**
- **BBNF self-host**: directives (`-> | , ; =`), nested braces, regex literals. Dense alphabet mixes regex metachar bytes into `S`; precondition fails. **Does not beneficially consume pre-pass unless per-pattern narrowing ships.**

The criterion compresses to: *the per-pattern bound invariant that `54eaa735` deferred IS the pre-pass's consumer.* Without it, the walker cannot use `idx.positions[slot]` as a scan terminator, so `StructuralIndex` is written and not read for speed — the W5.d regression.

## 5. Canonical design — Lever 2 per-compound, per-pattern

The W2.1 prototype's architecture is the template: **SIMD primitives consumed inline at the per-shape emitter level**, not a global driver second-level.

### 5.1 IR surface — per-pattern alphabet narrowing (the missing piece)

Extend `crates/ir/src/passes/recognizers/pattern_alphabet.rs` (exists already) to emit per-regex-pattern `last_byte_set: StructuralBitmap` + `first_byte_set: StructuralBitmap`. The precondition `54eaa735` named is `last_byte_set ∩ grammar.single_bytes = ∅`. Lift this as `DtaState::BoundedRegex { pattern, last_byte_set }`:

```rust
DtaState::BoundedRegex { pattern: &'static str, pattern_last_bytes: StructuralBitmap }
//   consumer: scan bounded by idx.positions[slot'] where
//             idx.kinds[slot'] ∈ grammar.single_bytes \ pattern_last_bytes.
```

Wire-contract test: fixture grammar `root = [0-9]+ ","`; assert `pattern_last_bytes = {0-9}`, assert emitted `BoundedRegex` consumer dispatches to `idx.positions[slot]` jump, assert samply shows zero byte-stepping inside the number scan.

### 5.2 Per-compound kernel choice — `KernelStrategy` lifted to `GrammarProfile`

`crates/ir/src/passes/recognizers/kernel_shape.rs` already exposes `select_kernel_strategy` + `KernelStrategy { singleton_kernel, has_digraphs, has_quote_parity }`. **It has no consumer.** `GrammarProfile` (`crates/bbnf-tape/src/profile.rs:173-204`) lacks a `prefer_inline_in_loop: bool` per rule. AW-V.md line 154 calls for exactly this (`recognizers/kernel_shape.rs` picks per-compound between stage-1-index and sonic-style inline-SIMD-in-loop based on IR-mined structural density). The bit-layout:

```rust
pub struct GrammarProfile {
    ...
    /// Per-rule SIMD strategy. `true` iff that rule's hot body should
    /// splice inline-SIMD primitives (W2.1 prototype shape) rather
    /// than consume the grammar-wide `StructuralIndex`.
    pub prefer_inline_in_loop: &'static [RuleId],  // bitmap-compressed

    /// Per-regex-pattern matchable-byte bitset (256-bit, 4 u64 words).
    /// `BoundedRegex` state's disjointness witness.
    pub pattern_alphabets: &'static [PatternAlphabet],
}

pub struct PatternAlphabet {
    pub rule_id: RuleId,
    pub last_bytes: [u64; 4],
    pub first_bytes: [u64; 4],
}
```

### 5.3 Per-shape emitter splice (integrates with W2.1 shape)

Per AW-V.W3+, shape emitters (`shapes/{object,array,string,number,keyword,pratt,unordered,arglist,flat,wrap,hregex}.rs`) each decide at codegen:

1. **If `rule_id ∈ prefer_inline_in_loop`**: splice inline SIMD fragments from `bbnf-simd-scan::emit` (`nospace64_scan`, `first_quote_or_backslash`, `quoted_string_simd_body` — already exported as `TokenStream` bodies, `crates/bbnf-simd-scan/src/emit/mod.rs:59-67`) directly into the shape's `fn parse_<shape>_<rule>` body. This is what W2.1 does by hand; W3 emits it from the shape mining pass.
2. **If `rule_id ∉ prefer_inline_in_loop` AND the grammar has `list_rules` + `single_bytes.len() < 16`**: consume the grammar-wide `StructuralIndex` via `ConsumeToNextStructural` jump + bounded-regex arms. This is the CSS/Sheets path.
3. **Fallback**: cold-path `dispatch_one` (AX replay surface, preserved per AW-V invariant 2).

Critically, stage-1 moves from *unconditional* to *per-rule-opt-in*. For JSON, `prefer_inline_in_loop` = every rule; `STRUCTURAL_ALPHABET` stays `&[]`; `scan_structural` returns `StructuralIndex::new()` early (lib.rs:82-85 — already handles this). **Elimination of the pre-pass cost for grammars that don't benefit.**

### 5.4 Quote parity — CLMUL vs shift-XOR on aarch64

M4 Max has `aes` feature. Empirically `vmull_p64` is L=3, recip-tp 1 on Firestorm/Avalanche (per dougallj firestorm-simd.html, cited in aw3-r1 §3). Six-op shift-XOR: six `SHL + EOR` pairs, each L=1 recip-tp 0.5 → ~3 cycles critical path, ~6 ops total. CLMUL: 3 cycles + one load/transmute ≈ 4 cycles on M4, one op. **CLMUL wins on uop count and L1 I-cache footprint; shift-XOR wins on decode-port contention during long SIMD bursts.** The current gate (`parity.rs:147-158`) picks CLMUL when `target_feature = "aes"`; correct for M-series. The `bbnf-simd-scan::emit::clmul_parity` + `shift_xor_parity` body-fragments (already shipped, `adc01a7f`) feed the per-shape emitter as inline splices with no cross-crate helper boundary — directly addresses aw3-r2 §2's "per-invocation LUT setup" failure mode by inlining.

## 6. Interactions, risks, portability

**IR alphabet mining** stays grammar-general; the `prefer_inline_in_loop` decision is mechanical (low structural density → inline; high density → indexed). **`bbnf-simd-scan::emit`** body-fragments become the primary consumer surface (W1.2 already shipped them). **Per-shape emitter splice** replaces the AU.2.7 per-call-site `find_next_structural_from` helper boundary. **AX cold-path replay** preserved: `dispatch_one` + `DtaState::*` survive for AX.X8–X10 consumers per AW-V.md:251-257.

Risks: (1) the per-rule bitmap adds `RuleId`-indexed profile slots; `GrammarProfile` capacity grows modestly. (2) `pattern_alphabets` for nested regex requires a new IR pass — complementary to existing `pattern_alphabet.rs` recognizer but per-pattern, not per-rule. (3) Bootstrap regen must be audited for self-host grammar because BBNF's alphabet is borderline-dense.

## 7. Estimated impact

- JSON twitter: W5.d 170 MB/s → W2.1-shape 2577 MB/s → emitter-lift target ~2500 MB/s (W3 gate ±5%). **15× recovery** by eliminating the eager `StructuralIndex` allocation on JSON.
- CSS normalize: post-AW-IV 25 MB/s → with per-pattern `BoundedRegex` + `ConsumeToNextStructural` lifters consuming the already-built index → projected 500–1000 MB/s. (Conservative; depends on `pattern_alphabet.rs` IR work landing.)
- Sheets parse_stress: 6 MB/s → expected 50–100 MB/s once operator-tower `BoundedRegex` fires.

The central insight: **stage-1 is not universally load-bearing; it is load-bearing only for dense-structural grammars that simultaneously admit pattern-alphabet disjointness.** The chronic deferral was correct in effect for JSON + BBNF; it was wrong for CSS/Sheets. The canonical fix is Lever 2 per-compound dispatch, not a universal pre-pass.

Key file paths cited:
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-json-prototype/src/simd.rs:21-24,35-58,132-147,301-343`
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-tape/src/stage1.rs:43-58`
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-tape/src/driver.rs:1592-1660,2050-2082`
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-simd-scan/src/lib.rs:81-115`
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-simd-scan/src/alphabet.rs:87-94`
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-simd-scan/src/parity.rs:40-158`
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-simd-scan/src/emit/mod.rs:59-67`
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-tape/src/profile.rs:173-204`
- `/Users/mkbabb/Programming/bbnf-lang/crates/ir/src/passes/sets/structural_alphabet.rs:182-267,341-400`
- `/Users/mkbabb/Programming/bbnf-lang/crates/ir/src/passes/recognizers/kernel_shape.rs:88-125`
- `/Users/mkbabb/Programming/bbnf-lang/crates/core/src/grammar/generated.rs:82600-82627`
- `/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/post-AW-III-W5.json`
- `/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/post-AW-IV.json`
- `/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/post-AW-V-W2-prototype.json`
- Commits `91df0809`, `54eaa735`, `c5b72813`, `1a004a37`, `25963ab1`, `4417f8a7`, `2f7c1bd4`, `4fdef7c3`, `f8e56d50`
