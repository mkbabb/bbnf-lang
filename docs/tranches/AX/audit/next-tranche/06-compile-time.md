# AX Planning Audit — 06 — Compile-Time Across 6 Grammar Derive Sites

Sub-agent: `W1a.A6`. Worktree: `../bbnf-wt-az-a6` from master
`ededfc7c`. `rustc 1.96.0-nightly (9602bda1d 2026-04-05)`,
Apple Silicon, `profile.ax-iter` (inherits `dev`, `debug=0`,
per-package `opt-level=1` on `bbnf-ir`/`csp-solver`/
`parse_that`). `.bbnf-cache` cleared before every cold build.
`CARGO_BUILD_JOBS=1` for per-grammar isolation, default 8 for
workspace.

## 1. Per-grammar compile-time + expansion size

Single-derive-`Parser` test binary, `cargo test --no-run`,
deps warm so timings isolate proc-macro expansion + test-binary
rustc.

| Grammar     | Binary               | wall     | peak RSS | cache bytes  | cache lines |
| ----------- | -------------------- | -------: | -------: | -----------: | ----------: |
| json        | `json_decode`        |   0.36 s |  145 MB  |      199,610 |       3,843 |
| bnf\*       | (gorgeous submodule) | ~0.15 s† | ~120 MB  |      165,927 |       3,156 |
| ebnf        | `ebnf_prettify`      |   0.66 s |  216 MB  |      443,023 |       8,649 |
| sheets      | `sheets_self_parity` |   0.96 s |  248 MB  |      781,031 |      14,036 |
| bbnf        | `bbnf_self_parity`   |   1.32 s |  289 MB  |    1,116,924 |      19,340 |
| **css\_l4** | `css_l4`             | **5.81 s** | **877 MB** | **13,003,357** | **196,760** |

\* BNF has no standalone test binary; derive lives only in
`crates/gorgeous/src/bnf.rs`. Cache byte-count isolated by
building gorgeous with cleared cache and matching
`__bnfparser_emit_impl` module tag.
† Extrapolation: gorgeous rebuilds all 6 derives in ~0.58 s
wall-clock warm; bnf is the smallest entry.

**Aggregate `.bbnf-cache`**: 7 entries, **14.86 MB / 241,114
lines** of cached TokenStream. **CSS L4 = 87.6 % of bytes,
81.6 % of lines.** Next-largest (sheets 781 KB) is 16.6×
smaller. Cache is token-serialised — pretty-printed
`cargo expand` is >3× higher (~600 kloc for css\_l4).

## 2. Workspace cold-build wall-clock + top-5 crates

`cargo clean && cargo build --workspace --profile ax-iter
--timings` on 8 cores:

- **Wall-clock 61.9 s.** Peak RSS 1.92 GB. User time 399.7 s
  (6.5× wall-clock — near-perfect core saturation).

Per-unit rustc times from `target/cargo-timings/
cargo-timing.html`. Each local crate has two units (lib +
test/bin targets); both are real rustc invocations.

| Rank | Crate         | unit-1 | unit-2 | combined | Notes |
| ---: | ------------- | -----: | -----: | -------: | ----- |
|    1 | `parse_that`  | 54.6 s | 43.9 s |   98.5 s | Single slowest. Path-dep at `../parse-that/rust/parse_that`. opt-level=1 already applied. |
|    2 | `bbnf-ir`     | 13.0 s | 11.8 s |   24.8 s | 17-pass IR pipeline. opt-level=1 applied. |
|    3 | `bbnf`        |  8.6 s |  8.5 s |   17.1 s | `crates/core` — owns 29,693-line `generated.rs` + backend + emitter. |
|    4 | `csp-solver`  |  2.33 s |  2.17 s |   4.50 s | opt-level=1 applied. |
|    5 | `gorgeous`    |    —   | 1.80 s |   1.80 s | 6-derive-site crate. Cold-cache adds ≈ 0 s (cache miss re-runs the bbnf pipeline but at a fraction of library-compile cost). |

External of note: `ls-types` 5.31 s, `lightningcss` 7.48 s
(dev-dep for parity), `syn` ×2 3.52 s.

## 3. rustc phase hotspots on the css\_l4 test binary

css\_l4 is **≥ 9× slower** than the next grammar.
`-Ztime-passes` on the test rustc invocation (total 5.28 s):

- **macro\_expand\_crate 0.74 s (+300 MB RSS).** The
  `#[derive(Parser)]` proc-macro runs the full pipeline:
  multi-file `@import` load, CSP-lattice type inference, 17 IR
  passes, backend emission of 13 MB / 197 kloc of tokens.
  `compile_pipeline` bench measures the pipeline alone at
  21.08 ms (post-W0a). The remaining ≈ 700 ms is cache-file
  read + `TokenStream::parse` on 13 MB.
- **type\_check\_crate 0.76 s + MIR\_borrow\_checking 1.71 s =
  2.47 s.** MIR borrow-check alone adds 144 MB RSS. Every
  generated `fn __state_N` is a fresh monomorphisation subject
  to typeck + borrow analysis.
- **LLVM\_passes 1.24 s + codegen\_crate 1.36 s = 2.60 s.**
  css\_l4 is the only grammar where LLVM self-time exceeds the
  proc-macro cost; every other grammar is expand-dominated.
  Under `ax-iter` (codegen-units=16) LLVM gets per-CGU
  parallelism. Under release (cgu=1 + LTO=fat) it blows up —
  W0a.2.d observed a **26 GB RSS peak** on a 5-grammar
  aggregate before AX.W0a.2.e split to per-grammar binaries.

Peak RSS grows sub-linearly in expansion-line-count up to
sheets (18 MB/kloc) and **super-linearly at css\_l4**
(877 MB @ 197 kloc = 4.5 MB/kloc emitted but +300 MB in
expand + +144 MB in MIR borrow-check).

## 4. Emitter phases dominating expansion

CSS L4 exercises **all 11 shape emitters** in
`crates/core/src/backend/rust/emitter/shapes/`. Every rule
gets a per-shape `fn __…_state_N` plus a separate
`__…_prettify_state_N` under `prettify`. `dfa_codegen.rs`
emits per-regex DFA transition tables (> 30 regex leaves on
css\_l4) as inline `match` ladders, not as hoisted consts.
`classify_byte.rs` emits the per-grammar byte-class LUT;
`keyword_dispatch.rs` emits PHF/trie tables; `grammar.rs`
emits the top-level state dispatch — per-state arms on css\_l4
average 40-80 lines each across hundreds of states.

Single-LOC hotspots are not the driver. **Fan-out is.** css\_l4
has the largest distinct rule count + the deepest `@import`
chain (15 files, 973 source lines).

## 5. Historical comparison

- **`post-AX-W0a-close-compile.txt`** (last recorded):
  `compile_pipeline` bench — json 167 µs, ebnf 513 µs, bbnf
  2.56 ms, sheets 11.37 ms, **css\_l4 21.08 ms.** Workspace
  build under `--profile bench` (LTO=fat, cgu=1, debug=true):
  **49.94 s.**
- **Current master** under `ax-iter`: **61.9 s workspace cold,
  1.92 GB RSS.** Not apples-to-apples with 49.94 s (profile
  differs — ax-iter is iteration-optimised, not output-
  optimised).
- **No prior `-Ztime-passes` per-grammar data** in
  `docs/tranches/AX/audit/` or `AW*/`. W0a.2.d's 26 GB RSS peak
  is on an aggregate-binary shape that no longer exists.
- **W0b deleted ~85 kloc of source** but wall-clock did not drop
  proportionally. Source size is dominated by the monomorphised
  `parse_that` + `bbnf-ir` generic tower, not deleted bbnf
  subsystems. Estimated W0b delta: ≈ 10-15 %.

## 6. Compile-time reduction levers

### Lever A — split CSS L4 emission across rustc modules

**Hypothesis.** 197-kloc single-module expansion pessimises
MIR borrow-check (1.71 s, +144 MB) because rustc runs borrow
analysis per-module on a giant fn table. Split at `@import`
file boundaries (N ≈ 13 on css\_l4).

**Mechanism.** `crates/core/src/backend/rust/emitter/
grammar.rs` emits `mod __part_N { pub(super) fn … }` groups
keyed by the import file each rule came from.
`#[inline(always)]` on cross-module state calls. rustc shards
typeck + MIR borrow-check across CGUs under cgu=16.

**Expected savings.** 30-50 % off css\_l4 rustc time on
ax-iter (→ 3-4 s). Under release (cgu=1) savings collapse but
release time isn't the iteration bottleneck.

**Verification.** `-Ztime-passes` before/after + `nm` on
release bench binary to confirm cross-module calls inline
away (architecture invariant §"cross-crate inlining via nm").

### Lever B — out-of-line DFA + keyword tables as const slices

**Hypothesis.** Per-regex DFA transition tables + PHF keyword
tables are currently `match` ladders and nested arrays inside
fn bodies. Every arm hits full typeck + MIR. Hoist to
`pub(crate) const TRANSITION_N: &[[u32; 256]] = &[...];`;
state bodies index-load. Also satisfies architecture invariant
§"hoist emitter-known data into emitted code".

**Mechanism.** `dfa_codegen.rs`, `keyword_dispatch.rs`,
`classify_byte.rs` flip dispatcher emission to const-table
index-load; keep `match` only for ≤ 3-entry tables.

**Expected savings.** 20-30 % of css\_l4 line-count moves from
fn bodies to const items; const-item rustc is far cheaper
(no borrow-check, no MIR). Net 0.5-1.0 s off css\_l4; also
trims sheets + bbnf proportionally.

**Verification.** `wc -l` on cache file before/after.
`-Ztime-passes` `MIR_borrow_checking` delta.

### Lever C — de-generic-ify parse\_that hot API

**Hypothesis.** `parse_that` is the single slowest crate
(98.5 s combined) and every bbnf + gorgeous build re-
monomorphises its generics.

**Mechanism.** Profile-driven, not static: run `cargo
llvm-lines -p bbnf --test css_l4` to rank the top-5
monomorphisations; replace with `dyn` boxing or byte-
specialised paths where monomorph count > 10.

**Expected savings.** 15-25 % off workspace cold wall-clock
(9-15 s).

**Verification.** `cargo llvm-lines` before/after;
`-Ztime-passes` `monomorphization_collector_*` delta.

### Lever D — downgrade `#[inline(always)]` on cold-path dispatchers

**Hypothesis.** W0a.2.f downgraded `#[inline(always)]` on
compound shape fns (`9ffe50db`) to break the LLVM inliner
SIGBUS cycle. The next cohort: `shapes/dispatcher.rs` +
`shapes/alt_dispatch.rs` — per-Ref value-position routing tags
variants `#[inline(always)]` even for once-per-rule call-sites.

**Mechanism.** Emitter tags each generated fn with an
IR-refs-mined call-site count estimate; emit
`#[inline(always)]` only where count ≥ 2 or body ≤ 8 tokens;
`#[inline]` otherwise; no hint on > 100-token bodies.

**Expected savings.** 0.3-0.6 s off css\_l4 LLVM\_passes; more
under release (LTO=fat).

**Verification.** `nm` parity + parse-bench regression check.

## 7. Hard-gate summary

6-row per-grammar table populated; workspace cold-build
61.9 s; top-5 crates named with times; historical delta cited;
4 lever proposals. **Dominant: A** (split css\_l4 emission).
B + D co-schedulable (disjoint file bounds); C is a separate
wave — path-dep `parse_that`, profiler-driven.
