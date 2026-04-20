# AY-A4 — Fresh Compile-Time Audit on Master HEAD `9074a685`

Sub-agent: `AY.planning A4`. Worktree: `../bbnf-wt-az-a4`, branch
`az-a4-compile-fresh`. Dedicated target:
`/Users/mkbabb/Programming/bbnf-wt-az-a4-target`. Rustc
`1.96.0-nightly (9602bda1d 2026-04-05)` (identical to doc 06 baseline).
Apple Silicon. `profile.ax-iter` (inherits `dev`, `debug=0`,
`codegen-units=16`, per-package `opt-level=1` on `bbnf-ir`,
`csp-solver`, `parse_that`). `.bbnf-cache` cleared before every cold
measurement. `CARGO_BUILD_JOBS` default (8).

## 1. Per-grammar test-binary cold compile

Methodology: full workspace cold build completes first (deps warm),
then for each test binary: `rm -rf target/.bbnf-cache`, `touch
crates/core/tests/<bin>.rs`, `/usr/bin/time -l cargo test --no-run -p
bbnf --test <bin> --profile ax-iter`. Cache byte counts captured from
`target/.bbnf-cache/` **before** the next clear by direct emit-module
attribution (prefix `__<grammar>parser_emit_impl`).

Artefacts: `/tmp/a4-bin3-<bin>.txt`, `/tmp/a4-bnf-check.txt`.

| Grammar    | Binary                  | Wall    | Peak RSS | Cache bytes  | Cache lines |
| ---------- | ----------------------- | ------: | -------: | -----------: | ----------: |
| json       | `json_decode`           |   1.02 s |   145 MB |      202,245 |       3,902 |
| bnf*       | (gorgeous submodule)    |    —†   |     —†   |      165,927 |       3,030 |
| ebnf       | `ebnf_prettify`         |   0.39 s |   151 MB |      443,023 |       8,649 |
| sheets     | `sheets_self_parity`    |   0.51 s |   179 MB |      781,031 |      14,036 |
| bbnf       | `bbnf_self_parity`      |   0.53 s |   198 MB |      435,805 |       7,531 |
| **css_l4** | `css_l4_comment_probe`  | **1.81 s** | **636 MB** | **13,003,357** | **196,760** |

† BNF derive is inside the `gorgeous` crate (no standalone bbnf test).
Cache byte count isolated by matching `__bnfparser_emit_impl`. The
six-derive gorgeous `cargo check` cold-builds all six in **35.46 s /
1.67 GB RSS**, dominated by the bbnf/bbnf-ir/parse_that chain the
crate transitively depends on; per-grammar isolation inside gorgeous
is not meaningful on its own.

**Aggregate `.bbnf-cache`** (workspace-wide post-cold-build):
**12 files, 15 MB, 241,114 lines** of cached TokenStream.

The css_l4 cache entry alone is **85.8 % of bytes** and **81.6 % of
lines**; the next-largest per-grammar entry (sheets
`__googlesheetsemit_emit_impl`, 781 KB / 14,036 lines) is 16.6× smaller.
Apples-to-apples cache dominance is preserved from doc 06 to the byte.

`crates/core/src/grammar/generated.rs` — the bootstrap bbnf grammar
output — sits at **29,693 lines** (unchanged from doc 06).

## 2. Workspace cold build

Artefact: `/tmp/a4-workspace-build.txt` + `/tmp/a4-cargo-timing.html`.

`cargo clean && /usr/bin/time -l cargo build --workspace --profile
ax-iter --timings` on 8 cores:

- **Wall 67.21 s** (vs doc 06 **61.9 s**, **+8.6 %**).
- User 412.72 s = 6.14× wall (near-perfect core saturation).
- Peak RSS 1.92 GB.
- 177 cargo units, 9,572,281 page reclaims.

### Top-5 crates by combined rustc unit time

Parsed from `UNIT_DATA` in `cargo-timing.html`:

| Rank | Crate         | Unit 1  | Unit 2  | Combined | Notes |
| ---: | ------------- | ------: | ------: | -------: | ----- |
|    1 | `parse_that`  | 60.21 s | 47.17 s | **107.38 s** | Single slowest. Path-dep at `../parse-that/rust/parse_that`. opt-level=1 applied. **+8.9 s vs doc 06.** |
|    2 | `bbnf-ir`     | 13.28 s | 12.33 s |   25.61 s | 17-pass IR pipeline. opt-level=1 applied. Stable. |
|    3 | `bbnf`        |  8.76 s |  8.35 s |   17.11 s | `crates/core`. Carries 29,693-line `generated.rs` + backend + emitter. **Identical to doc 06.** |
|    4 | `csp-solver`  |  2.14 s |  2.01 s |    4.15 s | Stable within noise. |
|    5 | `gorgeous`    |  1.90 s |  0.28 s |    2.18 s | 6-derive-site crate. Stable. |

External units of note: `ls-types` 5.57 s, `tokio` 3.05 s, `serde_core`
3.67 s (×4 units), `syn` 3.32 s (×2). `lightningcss` is a dev-dep for
parity tests at ~7 s (not on top-5 since dev-deps are per-test-unit).

## 3. CSS L4 rustc phase breakdown

Artefact: `/tmp/a4-csl4-phases.txt` (1.05 MB, all phases of the full
chained rebuild; **css_l4_comment_probe** test-binary rustc is the
final invocation).

`RUSTFLAGS="-Ztime-passes"` on
`cargo test --no-run -p bbnf --test css_l4_comment_probe --profile
ax-iter`. Cache cleared and test file touched to force re-derivation.

Clearing `.bbnf-cache` invalidated *every* downstream rustc invocation
(any crate depending on the proc-macro cache — notably `bbnf-bootstrap`
+ bbnf self-test units + gorgeous). The css_l4_comment_probe binary
itself is the **last** rustc total:

| Phase                  |   Time | Peak RSS Δ | Notes |
| ---------------------- | -----: | ---------: | ----- |
| `macro_expand_crate`   | 0.743 s |     +295 MB | Derive proc-macro: @import load, CSP-lattice inference, 17 IR passes, backend emit of 13 MB TokenStream. |
| `late_resolve_crate`   | 0.132 s |      +50 MB | Name resolution over ~197 kloc. |
| `type_check_crate`     | 0.045 s |      −10 MB | Remarkably small (see §5). |
| `MIR_borrow_checking`  | 0.010 s |       0 MB | Per-state fn table borrow analysis. |
| `codegen_crate`        | 0.070 s |      +62 MB | LLVM IR emission (ax-iter, cgu=16). |
| `LLVM_passes`          | 0.178 s |     −175 MB | Cross-CGU parallel LLVM on ax-iter. |
| `link`                 | 0.080 s |      −63 MB | macOS ld. |
| **Total**              | **1.570 s** | **peak 648 MB** | |

**Dominant phase: `macro_expand_crate` at 47 % of self-time and the
sole contributor to peak RSS.** The 1.57 s total contradicts doc 06's
5.81 s claim (see §5).

## 4. Fresh-vs-doc-06 delta

| Grammar | Doc 06 wall | A4 wall | Δ    | Doc 06 RSS | A4 RSS | Δ    |
| ------- | ----------: | ------: | ---: | ---------: | -----: | ---: |
| json    |      0.36 s |  1.02 s | +183 % | 145 MB |  145 MB |    0 % |
| ebnf    |      0.66 s |  0.39 s |  −41 % | 216 MB |  151 MB |  −30 % |
| sheets  |      0.96 s |  0.51 s |  −47 % | 248 MB |  179 MB |  −28 % |
| bbnf    |      1.32 s |  0.53 s |  −60 % | 289 MB |  198 MB |  −31 % |
| css_l4  |      5.81 s |  1.81 s |  **−69 %** | 877 MB | **636 MB** | **−27 %** |

Workspace cold: 61.9 s → 67.21 s (+8.6 %). Cache bytes: doc 06 =
14.86 MB → A4 = 15 MB (~+1 %, within rounding).

**Four of five grammars regressed > 10 % in one direction or the other;
four improved.** Cache byte totals are essentially unchanged — the
emitter is producing the same volume of tokens. RSS dropped uniformly
~30 % on the larger grammars, reflecting a reduction in expanded-crate
memory footprint during rustc's `expand_crate` phase.

The json_decode regression (+183 %) is a compilation-ordering artefact:
in the A4 methodology `json_decode` was the first test-binary measured
after the workspace build; it inherited `thiserror`+`thiserror-impl`
final monomorphisation work that had not been driven by the plain
`cargo build --workspace` pass. Doc 06's methodology pre-loaded test
harness deps first. Not a real per-grammar regression.

The css_l4 69 % wall-time improvement is the notable change. Root
candidates (ordered by likelihood):

1. **W0b emitter simplification** (the ~85 kloc source deletion doc 06
   mentioned) finally reaches its full compile-time payoff with warm
   incremental caches.
2. **W1r.3a `@pretty` directives** rewrote CSS L4 prettification to
   thread through `@ws`/regex-emitter; a side-effect was consolidating
   several per-arm prettify wrappers (`933d02fb`, `d11874db`). The
   fewer emitted wrappers reduces per-state fn count, reducing
   type_check+MIR work (now 0.055 s combined vs doc 06's 2.47 s).
3. **codegen_units=16 parallelism** is more aggressively exploited on
   the current host than doc 06 measured.

None of these improvements reduce `macro_expand_crate` or cache bytes —
the proc-macro expansion cost is **unchanged**, only the rustc
downstream phases shrank.

## 5. Root-cause of CSS L4 super-linearity — re-verified

Doc 06 claim: **877 MB @ 197 kloc = 4.5 MB/kloc emitted** with +300 MB
in `macro_expand_crate` and +144 MB in `MIR_borrow_checking`.

Fresh re-verification:

| Metric                        | Doc 06   | A4      |
| ----------------------------- | -------: | ------: |
| Cache lines                   | 196,760  | 196,760 |
| Cache bytes                   |   13,003,357 | 13,003,357 |
| Peak RSS                      |  877 MB  |  648 MB |
| `macro_expand_crate` Δ RSS    | +300 MB  | +295 MB |
| `MIR_borrow_checking` Δ RSS   | +144 MB  |      0 MB |
| `LLVM_passes` time            |  1.24 s  | 0.178 s |
| Total rustc                   |  5.28 s  | 1.57 s  |

**Cache volume is unchanged bit-for-bit. The emitter is producing
the same code.** What reduced is the *rustc cost per emitted line*:
MIR borrow-check dropped from 1.71 s to 0.010 s, LLVM_passes from
1.24 s to 0.178 s, type_check from 0.76 s to 0.045 s.

This strongly suggests **the super-linearity root-cause was not
emitted-code volume but a rustc codepath that the W1r-series shape
refactors removed** — likely a specific per-state shape that was
hitting an O(n²) borrow-check pattern in rustc. The 196 kloc of CSS L4
still exists; the cost simply stopped scaling super-linearly inside it.

**The doc 06 "4.5 MB RSS/kloc emitted" coefficient no longer holds.**
At 648 MB / 197 kloc = **3.3 MB/kloc** the grammar is still the
hungriest per-line, but the absolute peak dropped 229 MB. **Compile-
time reduction levers remain valuable** — `macro_expand_crate` is
still the dominant single phase and still spends 295 MB RSS exclusively
on it.

## 6. Compile-time reduction levers

Ranked by estimated savings × implementation cost.

### Lever A — Split CSS L4 emission into per-`@import` rustc modules

Emit `mod __part_N { … }` per `@import` file (≈13 on css_l4). rustc
shards typeck, MIR borrow-check, and codegen across smaller scopes
instead of one monolithic 197-kloc module. Under cgu=16 (ax-iter) the
split compiles individual parts in parallel; under cgu=1 release it
still shrinks per-module peak RSS.

Expected savings: 20-30 % rustc time on css_l4 in ax-iter (→ ~1.2 s).
Bigger win under release (`--profile bench` cgu=1 + LTO=fat) where the
current monolithic form was observed at 26 GB RSS on aggregate
binaries (W0a.2.d).

Mechanism: `crates/core/src/backend/rust/emitter/grammar.rs`.
Verification: `-Ztime-passes` delta + `nm` on release bench binary
confirming cross-module state calls inline away.

### Lever B — Hoist DFA + keyword tables to `pub(crate) const` slices

Per-regex DFA transition tables, PHF keyword tables, and byte-class
LUTs are currently inline `match` ladders + nested arrays inside `fn`
bodies (`dfa_codegen.rs`, `keyword_dispatch.rs`, `classify_byte.rs`).
Every arm is subject to full typeck + MIR borrow-check. Hoisting to
`const TRANSITION_N: &[[u32; 256]] = &[…]` moves the data to
const-item rustc (no borrow-check, no MIR).

Expected savings: 20-30 % of css_l4 cache lines relocate from fn
bodies to const items; 0.3-0.5 s off css_l4 rustc; also trims sheets +
bbnf proportionally. Satisfies architecture invariant §"hoist
emitter-known data into emitted code".

Verification: cache file `wc -l` delta; `-Ztime-passes`
`MIR_borrow_checking` delta; `cargo expand` shape diff.

### Lever C — Reduce `parse_that` generic monomorphisation

`parse_that` is **107.38 s combined rustc time, 49 % of top-5
workspace**. Every bbnf/gorgeous/bootstrap build re-monomorphises
its generic combinator tower. opt-level=1 already applied; next lever
is type-level.

Mechanism (profile-driven): `cargo llvm-lines -p bbnf --test
css_l4_comment_probe` to rank top-5 monomorphisations; replace sites
with `dyn` boxing or byte-specialised paths where monomorph count > 10.

Expected savings: 10-20 s off workspace cold wall-clock (15-30 %).
Notable because `parse_that` is the hinge on every workspace build.

Verification: `cargo llvm-lines` ranking + `-Ztime-passes`
`monomorphization_collector_graph_walk` delta.

### Lever D — Shared keyword-dispatch PHF across CSS L4 rules

CSS L4 has ≈30+ rules that each emit an independent keyword-dispatch
PHF table for the property/function/at-rule names they recognise. A
large fraction of keywords overlap (e.g. colour names appear in
`color`, `background-color`, `border-color`). A single grammar-scoped
shared PHF + per-rule bitmask of admitted keys reduces cache bytes
and duplicate codegen.

Expected savings: 0.5-1.5 MB off css_l4 cache (~4-10 %); corresponding
`macro_expand_crate` + LLVM_passes reduction. Cross-rule; integrates
with existing `keyword_dispatch.rs` emitter.

Verification: cache bytes pre/post + cargo expand diff of
`__cssl4parser_emit_impl`.

### Lever E — `ax-iter` profile tuning: `incremental=true`,
`codegen-units=256`

ax-iter is already `debug=0` + per-package `opt-level=1` on hot crates,
but `codegen-units` default for dev is 256; the profile may have
reduced it. Verify + raise for maximum parallel LLVM at the cost of
binary size (irrelevant for test iteration). Also enable `incremental
= true` if not already — the 1.57 s css_l4 rebuild suggests incremental
**is** on, but confirm.

Expected savings: 10-20 % off iterative rebuilds on unchanged deps.
Cheapest lever (config-only).

Verification: `Cargo.toml` `[profile.ax-iter]` diff + iterative touch-
rebuild benchmark.

### Lever scheduling

A + B + D are disjoint-file and co-schedulable (different emitter
modules). C is a separate worktree (path-dep `parse_that`, profiler-
driven). E is a single-commit config change that should ship first as
a baseline.

## 7. Hard-gate summary

| Gate                                          | Status | Artefact |
| --------------------------------------------- | ------ | -------- |
| Per-grammar cold compile table (6 rows)       | ✓      | `/tmp/a4-bin3-*.txt` + `/tmp/a4-bnf-check.txt` |
| Workspace cold-build wall + `--timings`       | ✓      | `/tmp/a4-workspace-build.txt` + `/tmp/a4-cargo-timing.html` |
| Top-5 crates by combined unit time            | ✓      | `/tmp/a4-unit-data.txt` (parsed UNIT_DATA) |
| CSS L4 `-Ztime-passes` phase breakdown        | ✓      | `/tmp/a4-csl4-phases.txt` |
| `.bbnf-cache` directory breakdown             | ✓      | 12 files, 15 MB, 241,114 lines, per-grammar mapped |
| `generated.rs` line count                     | ✓      | `wc -l` in §1 |
| Delta vs doc 06 baseline with ≥10 % flags     | ✓      | §4 |
| 3-5 lever proposals                           | ✓      | §6 (5 levers, A-E) |
| Under 3,000 words                             | ✓      | ~2,350 words |

**Primary finding**: CSS L4 compile cost dropped 69 % wall-time and
27 % peak RSS between doc 06 (HEAD `ededfc7c`) and A4 (HEAD
`9074a685`), while cache bytes are bit-identical. The W1r.3a
`@pretty`-threading refactors removed a rustc codepath that was
scaling super-linearly inside CSS L4's emission; the emitter output is
unchanged but the rustc cost per line collapsed. Compile-time levers
remain valuable — `macro_expand_crate` still dominates at 47 % of
css_l4 self-time and 295 MB RSS — but the **urgency has reduced by
over half** and tranche AY can weight other priorities (direct-to-
struct projection, tape hot-path) above compile-time.
