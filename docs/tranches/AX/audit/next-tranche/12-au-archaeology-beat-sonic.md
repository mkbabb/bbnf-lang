# AY Planning — AU Archaeology + BEAT-Sonic Trajectory

Standalone reference extracting post-AU bench numbers + the substrate lessons that drive AY's BEAT-sonic targets. Read in conjunction with `11-synthesis-v2.md` and `docs/tranches/AU/FINAL.md`.

## 1. Post-AU bench snapshot (commit `3b8b757`, Apr 15)

Per `docs/tranches/AU/FINAL.md` §Appendix:

| Bench | Entry | AU ns/iter | AU MB/s | AU bytes/cyc @ 3.2 GHz |
|-------|-------|----------:|--------:|----------:|
| json_monolithic | canada | 1,827,428 | 1,231 | 0.385 |
| json_monolithic | citm | 708,191 | 2,438 | **0.762** (94% of sonic) |
| json_monolithic | data_s | 20,326 | 1,746 | 0.546 |
| json_monolithic | data_xl | 18,037,587 | 1,179 | 0.369 |
| json_monolithic | twitter | 320,995 | **1,967** | **0.615** (76% of sonic) |
| css_l4 | bootstrap | 616,486 | 454 | 0.142 |
| css_l4 | normalize | 8,351 | 735 | 0.230 |
| css_l4 | tailwind | 7,331,274 | 496 | 0.155 |
| google_sheets_monolithic | parse_simple | 5,271 | 95 | 0.030 |
| google_sheets_monolithic | parse_nested | 11,333 | 128 | 0.040 |
| google_sheets_monolithic | parse_stress | 15,121 | 121 | 0.038 |
| bbnf_monolithic | bbnf_self | 13,003 | 394 | 0.123 |
| bbnf_monolithic | css_l4_grammar | 102,451 | 496 | 0.155 |
| bbnf_monolithic | css_pretty | 3,950 | 647 | 0.202 |
| bbnf_monolithic | ebnf | 6,490 | 223 | 0.070 |
| bbnf_monolithic | google_sheets | 8,731 | 858 | 0.268 |
| bbnf_monolithic | json | 1,892 | 283 | 0.088 |

## 2. Current master snapshot (HEAD `411eabfd`, Apr 20)

Per A1-A3 fresh audit:

| Fixture | Current ns | Current MB/s | Current bytes/cyc | AU delta | % of AU |
|---------|----------:|--------:|----------:|---------:|---------:|
| twitter 631 kB | 1,444,314 | 437 | **0.137** | -4.50× | 22% |
| canada 2.25 MB | 12,062,762 | 186 | **0.058** | -6.61× | 15% |
| citm 1.73 MB | 3,947,874 | 437 | **0.137** | -5.58× | 18% |
| data_s 35 kB | 84,135 | 421 | 0.132 | -4.14× | 24% |
| data_xl 21 MB | 79,518,033 | 267 | 0.083 | -4.41× | 22% |
| bootstrap 274 kB | 2,447,950 | 112 | 0.035 | -4.06× | 25% |
| tailwind 3.6 MB | 26,051,530 | 138 | 0.043 | -3.59× | 28% |
| parse_stress | 103,150 | 17 | 0.005 | -7.11× | 14% |
| bbnf_self | 59,535 | 108 | 0.034 | -3.65× | 27% |

**JSON lost 4-8× across the board. CSS ~4×. Sheets ~7×. BBNF ~3.7×.**

## 3. Sonic-rs / simdjson baseline

Per A1's fresh competitor bench:

| Fixture | sonic-rs ns | sonic-rs MB/s | sonic-rs bytes/cyc |
|---------|------------:|--------:|----------:|
| twitter | 242,448 | 2,605 | **0.814** |
| canada | 1,463,570 | 1,538 | 0.481 |
| citm | 571,068 | 3,025 | **0.945** |
| data_s | 14,898 | 2,380 | 0.744 |
| data_xl | 14,495,441 | 1,467 | 0.459 |

simd-json (simdjson-rs): ~0.42 bytes/cyc twitter. Slower than sonic-rs on the Value-materialize path but comparable on stage-1 SIMD scan.

**Algorithmic ceiling for Value-materialize JSON parsers is ~1.0 bytes/cyc.** sonic-rs citm at 0.945 is ≈ceiling; twitter at 0.814 has room but is near ceiling.

## 4. The regression chain

Chronological archaeology (per `docs/tranches/AX/audit/next-tranche/01-prior-tranche-archaeology.md` §4 + AU FINAL §5):

| Tranche | Wave | What landed | Perf impact |
|---------|------|-------------|-------------|
| AU close | `3b8b757` | **Flat AoS `Vec<TapeRec>` + unified `push_leaf_with(PayloadData)` + per-grammar push_fingerprint + `.map(|_| ())` elimination** | baseline: twitter 0.615 bytes/cyc, citm 0.762 (94% of sonic) |
| AV / AW-I | (various) | SoA 7-column pivot (7 Vec pushes per structural push) | -91% on bootstrap per doc 01 §4; masked as "parse failures masked as bootstrap win" |
| AW-III | (various) | `dispatch_one` walker consolidation; 0% self-time on the symbol BUT geomean 0.08× | walker cost hidden in state-machine; compound-wrap emission introduced |
| AW-IV.W5.2 | `86424b39` / `95b819f0` | sonic-rs + lightningcss parity harnesses CI-gated | no perf impact |
| AW-V.W2.1 | `4fdef7c3` / `0dcf9743` | **json-prototype hand-tuned = 0.89-0.94× of sonic** | proved substrate still capable |
| AW-V.W3 close | `c1e86ab3` | emitter visitor-path matched prototype ±2% | fleeting parity restored |
| AW-V.W6 | (various) | `has_w4_classified` gate over-admission → wrap-compound emission on every JSON value | ~2× regression reintroduced |
| AX.W0a | `6b03dd53` | parity harness restoration 77/77 | no perf change |
| AX.W0b | `a206b962` + `0adabb23` | walker deleted at runtime | no perf change (emitter shape was the cost) |
| AX.W1r | (various) | grammar-derived view work | no perf change |

**Net**: (a) SoA 7-column write pivot + (b) compound-wrap emission on scalar-leaf rules. Both landed without perf gating. Parity tests pass; perf tests didn't exist.

## 5. Load-bearing AU primitives to restore

### 5.1 Flat AoS `Vec<TapeRec>` write path (AY.W1.1)

AU had `Tape { records: Vec<TapeRec>, arena: Vec<u8> }`. Per push: one 16-byte store, one bounds check, one possible realloc.

Current: `Columns { kinds, flags, extra, span_lo, span_hi, sib_skip, child_off: Vec<_> }` — 7 Vec pushes per structural record, 7 heterogeneous type stores LLVM cannot fuse.

**Restoration path**: collapse to `Vec<TapeRec>` primary + optional `PackedRecord` sidecar (W1.D landed) as read-side cache. Transpose is near-identity post-revert (already AoS in 16-byte form, sidecar widens to 32-byte cache-line).

### 5.2 Unified `push_leaf_with(kind, PayloadData)` (AY.W1.1)

AU.6.7 commits `3b75463` + `9a1186e` + `7fc0adf`:
```rust
pub enum PayloadData<'a> {
    None,
    InlineScalar(u32),      // ≤4 bytes → column rank
    WideScalar(u64),        // 8 bytes  → column rank
    Aggregate(&'a [u8]),    // ≤16 B packed → arena offset
    Bytes(&'a [u8]),        // framed → arena offset
}
```
Ten+ `push_leaf_with_*` methods collapsed into one. Still exists in `crates/tape/src/builder.rs:275` today — **but it now delegates to the SoA 7-column push_structural.** Restoring AU's AoS primary makes this entry point fast again.

### 5.3 Per-grammar push_fingerprint (AY.W1.4)

AU.6.2 commits `ff32c0b` + `c2664f3`: `compute_push_fingerprint` IR pass → `(numer, denom)` per grammar → `Vec::with_capacity(input.len() * numer / denom)` at parse entry. JSON canada +18%, BBNF json.bbnf +49%.

Current: `GrammarProfile::compounds_per_input_byte` + `leaves_per_input_byte` fields are live (per A8 §Part 3). But must verify `Tape::with_capacity` threads the fingerprint to ALL Vec allocations post-revert.

### 5.4 `.map(|_| ())` elimination invariant (already holds)

AU.6.5 commit `4e4a75e`: 309 sites → 0. User-feedback `no-value-discard` enforced. Current state: invariant holds (per user memory).

## 6. json-prototype — the speed-ceiling oracle

`crates/json-prototype/` (AW-V.W2.1, commits `4fdef7c3` + `0dcf9743`) hand-tuned JSON parser hit:

| Fixture | bbnf-prototype / sonic-rs |
|---------|-------------------------:|
| data_s | 0.92× |
| twitter | 0.89× |
| citm | 0.94× |
| canada | 0.91× |
| data_xl | 0.90× |

Average 0.91×. Within striking distance of sonic-rs.

**The prototype's shape** (per `crates/json-prototype/src/lib.rs` doctring):
- Single entry: `parse_json` over a user-supplied visitor.
- Five shape functions, each `#[inline(always)]`, each monomorphised at the call site under `parse_json::<V>`.
- Zero references to `dispatch_one` / walker residue.
- Inline SIMD kernels consumed from `simd-scan` + crate-local `simd` module.
- Inline Eisel-Lemire via `compute_f64`.
- Borrow-safe string leaves on the no-escape path (`push_leaf_borrowed_string`).

**Grammar-derived emission of this shape** is the AY.W3b.2 lever:
- IR's TypeDesc + shape classification provide all inputs for emitting per-shape inline fns.
- `<Grammar>Value` TypeDesc-collapse shapes variants to match prototype's `Value` enum shape.
- Materialize path: 5 `#[inline(always)]` fns per grammar.
- Call site at `parsed.to_value()` inlines the full tree-build into one flat function.

**Expected**: bbnf-grammar-derived reaches prototype's 0.91× sonic ceiling. Plus the 4-5% gain from SIMD unescape + Eisel-Lemire direct-column (W4) = **≥1.05× sonic = BEAT-sonic by 5%**.

Aggressive goal: **1.15-1.40× sonic** = 15-40% faster than sonic-rs. Requires W4 lever landing cleanly + W2 wrap-elision firing on 40%+ of JSON's record count.

## 7. Per-wave bytes/cyc trajectory

| Checkpoint | twitter bytes/cyc | vs sonic | Lever at this step |
|---|---:|---:|---|
| AU close (Apr 15) | 0.615 | 76% | flat AoS + unified push_leaf_with + push_fingerprint |
| Current master | 0.137 | 17% | SoA + wrap-compound (regression) |
| **Post-AY.W1** | **~0.45** | **56%** | AU AoS revert + finalise fuse + structural scan |
| **Post-AY.W2** | **~0.85** | **104%** | G3 wrap-elision cuts record count 50% (matches sonic node count) |
| **Post-AY.W3** | **~1.00** | **123%** | json-prototype per-shape inline pattern |
| **Post-AY.W4** | **~1.15-1.40** | **142-173%** | SIMD unescape + Eisel-Lemire direct-column |

**Target: first Rust JSON parser to BEAT sonic-rs on its own eager-materialize benchmark.**

## 8. Non-JSON headline targets

CSS L4:
- Current tailwind: 0.043 bytes/cyc. lightningcss tailwind: 0.036 bytes/cyc (bbnf already 1.2× faster).
- Post-AY.W1+W2+W4: expected 2-3× over current = 0.08-0.13 bytes/cyc. lightningcss gap widens.
- **BEAT target**: bbnf ≥ 2.5× lightningcss at scale (bootstrap + tailwind).

Sheets:
- Current parse_stress: 0.005 bytes/cyc (absurdly low due to Pratt-tower overhead + tiny fixtures).
- Post-AY.W1 (Pratt Option C inline): expected +10% Sheets.
- Post-AY.W2 (wrap-elision + G5 Pratt simplification): expected +20% Sheets.
- Post-AY.W4 (operator byte-class dispatch): expected +5% Sheets.
- No external Sheets comparator; target is AU restoration (+3.9× vs current to match AU's 0.03-0.04 bytes/cyc).

BBNF (self-hosted):
- Current bbnf_self: 0.034 bytes/cyc. AU was 0.123 (3.6× gain available).
- Post-AY.W1 (AoS revert): expected ~0.10 bytes/cyc.
- Post-AY.W2 (wrap-elision on `mapped_factor` rule): expected ~0.13.
- Matches AU + exceeds.

## 9. What "BEAT" means operationally

**Hard-gate definitions** at AY.W7 close:

1. **Twitter eager-lane**: `bbnf_value_twitter / sonic_value_twitter ≤ 0.85` (bbnf ≥ 15% faster).
2. **Multi-fixture**: same ratio ≤ 0.85 on ≥ 3 of 5 JSON fixtures (twitter, citm, data, data_xl; canada may be harder due to f64-heavy).
3. **Lazy-lane**: `bbnf_get_twitter / sonic_get_twitter ≤ 1.0` (match sonic on the get_by_path lane).
4. **Parallel fork** (W8): tailwind + data_xl multi-threaded ≥ 1.5× single-thread.
5. **bytes/cyc on 4 universally-profiled benches**: ≥ 0.85 on 3 of (twitter, citm, bootstrap, bbnf_self).

Soft targets that would be beat-with-margin:
- twitter 0.75 (beat by 25%): requires AY.W4 to land cleanly.
- canada ≤ 0.90 (match): f64-heavy; Eisel-Lemire direct-column must carry the wave.
- tailwind bbnf ≥ 2.5× lightningcss (currently 1.67×).

## 10. Commitment

AY plan + sub-wave specs now encode BEAT-sonic as hard-gate, not aspiration. Scope-reveal in any wave that threatens the BEAT target flags to user at first detection per operational posture §6. The AU archaeology is load-bearing: every wave close cites AU delta in `docs/benchmarks/post-AY-W<N>-bytes-cyc.txt`.
