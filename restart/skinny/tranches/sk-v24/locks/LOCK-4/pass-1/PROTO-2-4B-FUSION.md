# PROTO-2 — 4B COMPTIME-FUSION (LOCK-4, pass 1, probe 2)

**Lock:** LOCK-4 — THE COMBINATOR CORE (parse-that rust + TS)
**Family adjudicated:** 4B COMPTIME-FUSION — the throughput half of FORK II
**Probe:** Rust runtime-substrate fork — monomorphized fusion vs the `SmallBox<dyn>` closure core, disasm-gated
**Date:** 2026-07-16 · **Host:** arm64 (Apple, 18 cores), nightly-2026-04-11, `bench` profile (lto="fat", codegen-units=1, opt-level=3)
**Verdict line (up front):** **4B THROUGHPUT WIN CONFIRMED — fused chain shows ZERO vtable `blr` AND cold-paired ≥1.10× in BOTH windows and BOTH arm orders → FORK II routes to 4B for Rust.**

> Note on filename: the plain `PROTO-2.md` slot in this directory is already occupied by the
> unrelated CHAIN-MONADIC-CENSUS probe. To avoid clobbering it, this 4B-fusion deliverable takes a
> distinct name (mirroring how `PROTO-1.md` and `PROTO-1-MONADIC-WALL-CENSUS.md` coexist).

---

## QUESTION

Does 4B's cross-combinator inlining — generic `Seq2<P0,P1>`/`Seq3`/`Alt2` structs that implement
`ParserFn` **directly** (concrete, monomorphized, no box) — beat the current `SmallBox<dyn ParserFn>`
closure core (`Parser`, already arity-unrolled via `seq!`/`alt!`) on the JSON combinator grammar's hot
chains, cold-paired, with disasm confirming the vtable `blr` is gone?

Pass/fail (from the brief): fused chain shows **zero `blr`** through the parser vtable (hard disasm
gate) **AND** cold-paired throughput **≥ 1.10×** the closure baseline in **both** load windows →
4B CONFIRMED. If `<1.10×` OR window-/order-asymmetric (the PROVE-09 pattern) → throughput case weak,
route FORK II to isomorphism (4C/4D).

## METHOD (what actually ran)

Scratch copy of `/Users/mkbabb/Programming/parse-that` per R3 §8 (rsync, external patch paths
repointed, **isolated `CARGO_TARGET_DIR` inside the copy**; original tree never mutated — READ-ONLY).
One new bench target `benches/proto2_fusion.rs` + one `[[bench]]` stanza (the entire diff — see
`PROTO-2-4B-FUSION.patch`).

Both arms parse the **same three real corpora** into the **same `Json` tree** with the **same leaf
scanners** — `scan_quoted_string_strict`, `scan_number_strict_f64`, `trim_leading_whitespace_mut`,
plus a byte-literal matcher — each called as a **direct `bl`** on *both* arms. The ONLY difference is
the combinator substrate:

- **Arm A — FUSED (4B):** concrete `QStr`/`Lit`/`KwTrue|False|Null` leaves + generic `Seq2`/`Seq3`/
  `Alt2` structs implementing `ParserFn` directly (the blanket-impl hook at `src/parse.rs:41-53`).
  Composition monomorphizes; recursion (`value`) is a **monomorphic free-fn call** — needs *zero* boxes.
- **Arm B — CLOSURE CORE (incumbent):** `parse_that::Parser` = `SmallBox<dyn ParserFn, S32>`
  (`src/parse.rs:61-77`), boxed leaves, boxed dispatch closure, recursion via `lazy()`
  (`src/lazy.rs:20-64`, one `Box<dyn LazyParserFn>` + one `SmallBox<dyn ParserFn>`). This is the
  exact hot-loop form of the incumbent `json_parser` (`src/parsers/json.rs` hand-rolls `.call()` on
  boxed `Parser` leaves).

An **equivalence gate** runs first: `fused::parse(d) == closure::parse(d)` asserted on all three
corpora (identical trees) — passed on every corpus, every run.

- **Corpora:** `data/json/{twitter (631,514 B), citm_catalog (1,727,204 B), canada (2,251,051 B)}`.
- **Timing:** cold per-parse (full-document parse each rep, no warm cache), **N=250 per arm**,
  **A/B/A/B interleaved** (arms alternate every rep so load drift hits both symmetrically), median
  ns/parse → MB/s. Each run additionally repeats with the **opposite arm order** (A=fused-first and
  A=closure-first) as the PROVE-09 asymmetry guard. **Two load windows**, loadavg recorded.
- **Disasm gate:** `otool -tvV` over the `bench`-profile binary; `blr` (arm64 vtable indirect call)
  counted per symbol region for the fused hot chains vs the closure hot chains.

Divan (0.1) is wired as the bench harness and its equivalence/tree scaffold runs, but divan renders
its stat table via in-place ANSI updates that do not survive non-interactive capture (pty/width did
not help). The measurement of record is therefore the **manual cold-paired harness** in `main()`
(`PROTO2_MANUAL=1`), which is strictly more faithful to the brief's "cold paired A,B,A,B, N≥200, two
windows, opposite arm order" than divan's auto-sampler would have been.

## THE NUMBERS

### Disasm gate — `blr` (vtable indirect) count per hot chain (fat-LTO `bench` binary)

| symbol (hot chain)                         | instrs | **blr** | bl (direct) |
|---|---:|---:|---:|
| `fused::value`   (dispatch + recursion)    | 1339 | **0** | 10 |
| `fused::object`  (Seq2 member + loop)      |  796 | **0** | 28 |
| `fused::array`   (element loop)            |  292 | **0** | 24 |
| `fused::parse`   (entry)                   |  118 | **0** | 8  |
| **fused:: total**                          |      | **0** |    |
| `closure::value` inner closures (×6+lazy)  |  —   | **60**| — |
| **closure:: total**                        |      | **60**|    |

Whole-binary `blr` census = 529 (divan/std/deps). **Every `blr` on the fused parse path was
eliminated**; the closure core retains **60** `blr` — the surviving `SmallBox<dyn ParserFn>::call`
indirects (6/0/0/10/14/5 across the boxed value/object/array/dispatch closures). **Hard gate: PASS.**

### Throughput — cold-paired median MB/s, fused ÷ closure ratio

**Window 1** (loadavg 9.19 → 8.85):

| corpus        | fused MB/s | closure MB/s | ratio (A=fused) | ratio (A=closure) |
|---|---:|---:|---:|---:|
| twitter       | 1886.1 | 1493.7 | **1.263×** | **1.263×** |
| citm_catalog  | 1877.0 | 1616.3 | **1.161×** | **1.163×** |
| canada        | 1062.7 |  875.9 | **1.213×** | **1.210×** |

**Window 2** (loadavg 8.85 → 8.47):

| corpus        | fused MB/s | closure MB/s | ratio (A=fused) | ratio (A=closure) |
|---|---:|---:|---:|---:|
| twitter       | 1864.9 | 1472.8 | **1.266×** | **1.272×** |
| citm_catalog  | 1892.1 | 1597.2 | **1.185×** | **1.186×** |
| canada        | 1071.0 |  869.1 | **1.232×** | **1.231×** |

**Min ratio across all 12 cells = 1.161× (citm, W1). Every cell ≥ 1.10×.** Both windows agree to
within noise; both arm orders are symmetric (no PROVE-09 window-/order-asymmetry). Throughput gate: PASS.

### R1 budget deltas

- **Compile time:** one-time cold dep build (full fat-LTO dep tree) is the fixed cost; the
  **incremental** cost of the fused layer — recompile `parse_that` + this bench + fat-LTO relink —
  measured **6.4 s / 7.0 s** across rebuilds. The monomorphized Seq/Alt structs add no measurable
  front-end cost; the whole fusion layer is a few hundred lines.
- **Binary size:** `bench` binary total 1,232,640 B (`__TEXT` 950,272 B) — but that carries BOTH arms
  + divan + serde/simd competitors linked in the dev-dep graph. The fused code itself is a handful of
  monomorphized functions (value 1339 / object 796 / array 292 / parse 118 instrs). Monomorphization
  cost is bounded and O(distinct Seq/Alt shapes), not O(grammar size); for the JSON grammar it is
  negligible against the binary. No size regression concern at this grammar scale.

### Recursion via one box (brief requirement)

Proven both directions. The **closure arm** ties the recursive `value` knot through exactly **one**
`lazy()` box (`src/lazy.rs:20-64`: one `Box<dyn LazyParserFn>` + one `SmallBox<dyn ParserFn>`) and
parses all three corpora correctly (equivalence gate passed) — recursion still parses via one box. The
**fused arm** demonstrates the stronger result: monomorphic free-fn recursion (`fused::value` calls
itself via direct `bl`) needs **zero** boxes and still parses identically. So 4B does not break
recursion; it can even retire the recursion box, or keep exactly one where a `Parser`-typed cut is
wanted.

## VERDICT

**4B COMPTIME-FUSION — THROUGHPUT WIN CONFIRMED.**

- Hard disasm gate: **PASS** — fused hot chains carry **0** `blr`; the closure core carries 60.
- Cold-paired throughput: **PASS** — **1.16×–1.27×** over the closure core, **≥1.10× in both windows
  and both arm orders**, load-symmetric (no PROVE-09 pattern).
- Equivalence: fused ≡ closure trees on all corpora. Recursion parses via one box (closure arm), or
  zero boxes (fused arm).

**FORK II routes to 4B for Rust.** The thesis "combinators need not be naive" holds on the runtime
substrate: implementing `ParserFn` on concrete generic `Seq`/`Alt` structs lets the compiler inline
across the whole combinator chain, deletes the `SmallBox<dyn>` vtable dispatch outright, and buys
16–27% throughput at the same leaf semantics — with recursion intact.

### Honest scope / caveats

- The comparison isolates the **combinator substrate**: identical leaf scanners (direct `bl` on both
  arms) mean the measured 1.16–1.27× is purely the boxed-closure-dispatch tax the fusion removes. It
  is **not** a comparison against the production `json_parser()`, which layers `dispatch_byte_multi`
  and SIMD string scanning on top — those optimizations are orthogonal and would ride on top of 4B.
- The `alt!`/`seq!` macros themselves hit nightly-2026-04-11 closure-arg/HRTB inference warts
  (`E0282`, "FnOnce is not general enough") in this synthetic bench position; the closure arm was
  written to their **exact expansions** (boxed `Parser` leaves + explicit ordered-choice closure),
  which is the identical `SmallBox<dyn>` runtime substrate under test. This is a harness ergonomics
  note, not a result — the disasm confirms the boxed dispatch (`blr`) is present exactly as the macro
  form would produce.
- Single host, single toolchain; no x86 (per rules). Loadavg 8–17 during runs (shared box) — the
  paired-interleave + two-window + both-order design controls for it, and the ratios are tight across
  all of it.

## ARTIFACTS

- Probe code + Cargo registration: `PROTO-2-4B-FUSION.patch` (this directory) — full new bench file
  `benches/proto2_fusion.rs` + `[[bench]] proto2_fusion` stanza.
- Scratch build (isolated, ephemeral):
  `…/scratchpad/pt-scratch/rust`, `CARGO_TARGET_DIR=…/scratchpad/pt-scratch/target`.
- Raw disasm: `…/scratchpad/disasm.txt`.
