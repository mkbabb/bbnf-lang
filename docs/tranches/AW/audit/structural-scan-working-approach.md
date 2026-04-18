# Structural-Scan Working Approach — Audit Agent #1

## 1. Angle headline

Stage-1 SIMD structural pre-pass has failed six times because it was shipped as a **grammar-wide single-column index consumed by a walker that could not narrow it**. The *mechanism* is sound; the *plumbing* has always been wrong. The working approach is a **per-pattern alphabet-narrowed streaming bitmap**, queried on demand at the shape emitter's per-rule call site, with the grammar-wide index retained only as a slot-resync oracle. The shipped substrate (`bbnf-simd-scan` kernels, `PatternAlphabet` mining with `last_byte_set`, per-rule emitter splice surface) has **all the pieces**; the missing unit of work is a single IR→emitter wire from per-pattern `last_byte_set` to a **per-rule bounded-scan kernel** spliced inline at the shape-emitter level. CSS L4 `declaration-value = [^;}]*` then scans with `|S| = 2`, independent of the grammar-wide 80-byte alphabet.

## 2. Failure-mode inventory (six attempts)

| Wave | Pitfall | Mechanism |
|---|---|---|
| **AO.0.1→0.6** (`7198c974`) | Pre-pass always-on, no cost gate | v1 `advance_to_structural` + `ParserState.structural_cursor`; ~15-25% overhead without WS elision (`4417f8a7`). |
| **AP.1b** (`4417f8a7`) | Gate-off disguised as disable | Kept infrastructure alive behind `structural_mode=false`; became dead code until AQ.5 deleted ~1500 LOC. |
| **AQ.5** (`2f7c1bd4`) | Delete-without-pivot | AP.3.1 SIMD WS bitmap captured the WS-elision savings; pre-pass deleted entire rather than pivoted to a DTA consumer. |
| **AU.2.7 v2** (`143d19ee`, `e225ade9`) | Substrate without consumer | `emit_structural_bitmap_kernel` shipped per-call-site, not as a pre-pass; no grammar-wide LUT, no quote parity, no digraph, no driver consumer. |
| **AW-III.W5.b→W5.d** (`91df0809`, `54eaa735`) | Dense-alphabet regex bound collapse | `scan_structural` consumed by driver; W5.c `[pos, idx.positions[slot])` bound revert on CSS L4 (dense alphabet → zero-width) — twitter -11%. |
| **AW-V.W1.2 / W5.2** | Activation without grammar discrimination | Per-Ref dispatcher landed substrate; `has_w4_classified` gate widened silently, regressed visitor-path from 0.98× sonic to non-compile. |

Every failure has the same architectural signature: **a single grammar-wide column indexed by a single consumer**, whose read-out cost per dispatch (~1 cycle) does not amortise the kernel's eager-fill cost (stage-1 at 50-100 µs per twitter parse).

## 3. Comparator survey

**simdjson.** Stage-1 alphabet is 8 bytes (`{}[]:,"\\` + structural-whitespace). String bodies, which may contain arbitrary non-structural bytes, are handled by **CLMUL prefix-XOR quote parity** (`bbnf-simd-scan::parity.rs:147-158` already ships this). The claim "dense alphabet defeats SIMD" is the wrong framing. simdjson is not dense-alphabet-resistant — its alphabet is **architecturally sparse by construction** (grammar's delimiters are 8 bytes). The equivalent question for CSS is not "can we classify 80 bytes at SIMD speed" (trivially yes; NEON `vqtbl2q_u8` wide-LUT at L=3) but **"once classified, is there anything to skip"** — and the answer depends on *which rule* is scanning, not on the grammar's union alphabet.

**sonic-rs.** No pre-pass. Inline `nospace_bitmap_64` cached on the CPU stack (`ScanState`, 16 bytes), refilled lazily. String bodies handled by inline `first_quote_or_backslash` at the per-rule call site. This is the architecture `bbnf-json-prototype` replicates and is why it beats sonic at 0.89-0.94× ratio.

**JSONSki / Mison.** Mison does kind-separated *logical bitmaps* but collapses at query time (it is a query engine, not a substrate). JSONSki uses "structural intervals" + fast-forward for *selective skipping* of irrelevant subtrees — essentially lazy materialisation.

**Non-JSON dense-alphabet SIMD parsers.** None public. `cssparser`/`lightningcss` use hand-written direct parsers with memchr-per-site; no SIMD stage-1. This is not a coincidence: their alphabet is dense, so they skipped the framing the rest of the field assumes. The working design below re-enters this niche with a specific answer.

## 4. Per-pattern alphabet narrowing — the concrete design

### IR surface (mostly shipped)

`crates/ir/src/passes/recognizers/pattern_alphabet.rs` already emits `PatternAlphabet { matchable_bytes, last_byte_set, is_tight, is_last_byte_tight }` per `IrNode::Regex`. `last_byte_set` is the NFA-accepting-state predecessor bitmap (AW-IV.W3.5c invariant). CSS L4 `declaration-value` pattern `[^;}]*` mines `last_byte_set = full_bitmap \ {';', '}'}`; complement with `{';', '}'}` gives the termination set `S' = {';', '}'}`. **Two bytes.** Independent of the 80-byte grammar-wide alphabet.

### Emitter wire (missing — the single new unit of work)

Add `DtaState::BoundedRegex { pattern, termination_bitmap: [u64; 4] }` with the emitter lowering it as an inline per-rule kernel call. The *termination bitmap*, not the grammar-wide singleton alphabet, drives the `bbnf-simd-scan` kernel at that call site. Shape emitter in `crates/core/src/backend/rust/emitter/shapes/hregex.rs` (already exists per W4.1) splices a body fragment from `bbnf-simd-scan::emit` — a **two-byte scan** instantiation of `nibble_lut_scan::SOURCE_NEON` with `singletons = &[b';', b'}']` in the `NibbleLut::from_singletons` call. The kernel runs over at most the remainder of the current stripe; it does not consume or rely on the grammar-wide `StructuralIndex` at all.

### Substrate-with-consumer delivery (the hard gate)

A wire-contract test loads the generated `parse()` for CSS L4, parses a fixture `{foo:bar;}`, and asserts:
1. `nm target/release/deps/<bench>` shows the per-rule inline kernel symbol present.
2. samply shows zero self-time on `__dta_walker_inline::run` inside `declaration-value`.
3. No call into `scan_structural` from the `declaration-value` fast path.

### Cost-model witness

NEON `vqtbl1q_u8 + vshrn_n_u16 + vaddvq` on a 2-byte termination set: L=3+2+2=7 cycles per 16-byte stripe, 256-byte values scan in ~112 cycles versus ~256 cycles byte-stepping. Break-even at 8+ byte declaration values (the overwhelming majority). This is a **per-rule, alphabet-narrow, on-demand inline kernel**, not a pre-pass.

## 5. Alternative designs (considered, subsumed)

- **Kind-partitioned streams (N2.3).** `StructuralIndexKinds { delim_kinds: [Vec<u32>; K] }`. Cost-neutral stage-1, +3-7% across grammars. Strictly weaker than per-pattern narrowing because the K bins are **grammar-wide**, not **rule-wide**. The CSS `selector` rule wants `{' ', '.', '#', '[', ':'}` (5 bins, but specific), not a universal `K=7` partition. **Composes** with §4: stream K_r ⊆ grammar streams per rule.
- **Streaming / Mison bitmap.** Produce a per-64-byte-block kinded bitmap on demand. CSS walker only pays when it queries. This is what the per-rule splice *becomes* at the kernel level — the shape emitter's inline `vqtbl1q_u8` is precisely a demand-driven 64-byte kinded query. Mison-as-substrate collapses into §4.
- **VBMI2 `_mm512_permutexvar_epi8`.** 64-byte classifier in one op; excellent scaling on Ice Lake. Orthogonal to §4 — it is a faster *kernel*, with the same *consumer*. Ship as an AVX-512 variant of the per-rule splice.
- **Boundary-coalesced SIMD.** Scan only open/close pairs. Applies to JSON (8 bytes already), does not help CSS L4 (every structural byte is in the pair set).
- **Hybrid inline-vs-indexed threshold.** The design *is* this hybrid — short runs (single stripe) scan inline; long runs (spans over many stripes) fall through to the kernel loop. Threshold is `input.len()` remaining minus current position; no global threshold needed.

## 6. Recommended AX wave-scope

**AX.W0 (hard gate — blocks everything).** Land `BoundedRegex` emitter arm from per-pattern `last_byte_set`. File bounds: `crates/core/src/backend/rust/emitter/shapes/hregex.rs`, `crates/bbnf-tape/src/driver.rs` (new arm), `crates/bbnf-simd-scan/src/emit/nibble_lut_scan.rs` (already shipped). Gate: CSS L4 `declaration-value` parses twitter-style inputs with no `scan_structural` call on the fast path, symbol verified via `nm`.

**AX.W1 per-grammar.** Hard gates per grammar:

- **JSON.** `STRUCTURAL_ALPHABET = &[]` (no grammar-wide index); all scanning inline from `nospace64_scan` + `first_quote_or_backslash`. Gate: twitter ≥ 2500 MB/s.
- **CSS L4 `compoundSelector`.** `BoundedRegex` with mined `last_byte_set`. Gate: bootstrap ≥ 500 MB/s, normalize ≥ 800 MB/s.
- **Sheets formula body.** `BoundedRegex` on operator tower `=+-*/(),:`. Gate: parse_stress ≥ 50 MB/s.
- **BBNF self-host.** Inline SIMD per-rule; no grammar-wide index. Gate: bbnf_self ≥ 200 MB/s.

**AX.W2 per-rule opt-out.** `GrammarProfile.prefer_inline_in_loop: &'static [RuleId]`. JSON = every rule (empty grammar-wide index). CSS = `@media`/`@import` header rules; declaration-value fast-path uses `BoundedRegex`. The `scan_structural` call becomes **opt-in, per-grammar**, with an early-exit on empty alphabet already shipped (`lib.rs:83-85`).

## 7. Pitfall-avoidance audit

| Prior pitfall | Addressed by |
|---|---|
| AO.0.1: pre-pass always-on | Grammar-wide index **opt-in** via `prefer_inline_in_loop`; JSON's alphabet is empty, kernel returns immediately. |
| AP.1b: gate-off disguise | No gate flag; absent rule-list = absent emission. Ledger asserts per-rule inline symbol presence, not gate state. |
| AQ.5: delete-without-pivot | Kernel re-used; consumer changes, not the kernel. The 1500 LOC deletion was *substrate*; this wave lands *consumer*. |
| AU.2.7 v2: per-call-site without narrowing | Per-rule splice pairs with `last_byte_set` from IR; the `last_byte_set` IS the narrow alphabet, not the grammar's union. |
| AW-III.W5.d: regex-bound collapse | `BoundedRegex` uses `last_byte_set`-complement — invariant-proof non-empty by mining construction. `is_last_byte_tight = false` fallback to scalar is explicit and tested. |
| AW-V.W1.2: silent predicate widening | No predicate-widening on this path. The mined `last_byte_set` is per-`IrNode::Regex` fixture-grounded; wire-contract test asserts `pattern_last_byte_set(fixture) == expected` at IR pass level, `emitted_bitmap == mined` at emitter level, and `runtime_scan(fixture_input) == golden_length` end-to-end. |

**Required artefacts at AX.W0 close:**
1. `nm` showing the per-rule inline kernel symbol present in the CSS bench binary and `scan_structural` symbol absent from the declaration-value hot path.
2. Samply profile showing `<1%` self-time in `bbnf_simd_scan::neon::scan` for CSS bootstrap.
3. Wire-contract test exercising IR-mining → emit-splice → runtime-consume on a `foo { bar: baz; }` fixture.
4. Bench: CSS bootstrap ≥ 500 MB/s (vs current 14 MB/s).

Without all four, the wave does not close — per AW `SYNTHESIS.md:27` substrate-without-consumer invariant.

---

**Key deliverable claim:** "Dense alphabet defeats SIMD" has never been architecturally true; it was a *plumbing* failure every time. Per-pattern `last_byte_set` (shipped in `pattern_alphabet.rs:289-354`) is the narrow alphabet CSS needs. Splicing a **two-byte** scan at the declaration-value call site — using the same `nibble_lut_scan` kernel already in `bbnf-simd-scan` — achieves structural-scan parity with simdjson's JSON path on a grammar with 80-byte union alphabet. One IR→emitter wire, per-rule opt-in, wire-contract hard gate. Everything else is deferral in disguise.

**Key file paths cited:**
- `/Users/mkbabb/Programming/bbnf-lang/crates/ir/src/passes/recognizers/pattern_alphabet.rs:55-105,279-354`
- `/Users/mkbabb/Programming/bbnf-lang/crates/ir/src/passes/sets/structural_alphabet.rs:182-267,319-431`
- `/Users/mkbabb/Programming/bbnf-lang/crates/ir/src/passes/recognizers/kernel_shape.rs:88-125`
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-simd-scan/src/lib.rs:71-115`
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-simd-scan/src/emit/nibble_lut_scan.rs`
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-simd-scan/src/emit/mod.rs:59-67`
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-simd-scan/src/parity.rs:40-142`
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-simd-scan/src/alphabet.rs:87-94,137-148`
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-tape/src/stage1.rs:43-107`
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-tape/src/driver.rs:1584-1660,2050-2082`
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-json-prototype/src/simd.rs:21-24,35-58,286-343`
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AW/research/aw5-r1-stage1-simd-prepass.md` (full)
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AW/research/aw5-n2-novel-parsing-approaches.md:94-130` (kind-separated variant)
- Commits: `7198c974`, `4417f8a7`, `2f7c1bd4`, `e225ade9`, `143d19ee`, `91df0809`, `54eaa735`
