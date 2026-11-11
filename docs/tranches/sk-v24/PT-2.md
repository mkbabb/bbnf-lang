# PT-2 — THE RUST CORE REFORM

*parse-that Rust side · full-implementation tranche · SK-V24 greenfield novum codex*

---

## 1. Charter

### 1.1 Mission

Take the parse-that Rust combinator core off the nightly toolchain and onto stable —
`portable_simd`, `smallbox[nightly]`, `stmt_expr_attributes`, and the generated-file
`fmt_helpers_for_derive` residual all excised — while purging the last x86 arms from its two
mixed scanners and installing the aarch64-only guard rails. The tranche productionizes the
de-nightly proof (PROVE-09) into a stable, maintainable core; it **makes zero performance
claim**. Its value is STABILITY: parse-that leaves the ICE-prone pinned nightly
(`nightly-2026-04-11`, adopted to dodge 93 recorded `on_disk_cache` ICEs — R3 §1) for the
stable channel, and the four-layer x86 abrogation reaches parse-that's source as it already
governs bbnf-lang.

The mission's honesty is load-bearing. PROVE-09's headline — a 1.34–1.57× speedup from the
`std::simd`→NEON shim — was **REFUTED** by adversarial re-measure (VERIFY.md): fresh paired
windows show the port neutral-to-slower, the claimed win an artifact of two-arm window
asymmetry. PT-2 carries the port's *engineering* forward and discards its *speed narrative*.
The only performance obligation herein is a **no-regression floor**, and it is a gate that
can fail — the proof-grade shim failed it (VERIFY window A, 0.81–0.98×).

### 1.2 Precept bindings

Bound by number from the sk-v23 research/07 codex:

- **P2 / P6 / P50** — no workarounds, no legacy, no feature-flag-and-keep: the nightly
  features are *deleted*, not `cfg`-gated behind a stable/nightly fork.
- **P5** — one path: the `simd/` primitive module is the single SIMD vocabulary; no
  parallel `std::simd`-vs-`core::arch` codepaths survive.
- **P14 / P15** — no god modules, directory-modules over flat siblings: PROVE-09's flat
  `src/simd128.rs` shim is productionized into a `src/simd/` directory-module split by
  concern (§4 W1).
- **P16 / P54** — tests in `tests/`, never `#[cfg(test)]` in `src/`.
- **P53** — generated files are fresh-regen output, never hand-patched: the Debug fix lands
  at the *generator* and regenerates via the bootstrap script (§4 W3); idempotent-on-disk.
- **P8 / P37 / P40** — Y.13: every wave lands its consumer in-wave (§4, each wave names it).
- **P43–P49** — the profiling standing law: cold per-parse, paired interleaving, N≥200,
  multi-window, per-block loadavg, ratios certified / absolutes directional (§3.4).
- **P4 / P39** — abrogate-before-patch and the negative-result close: W4 (the arena) closes
  either on a sound landing or on a *measured refutation* — both are valid closes with an
  artifact.

### 1.3 Refutation guards (SPEC §0 laws that constrain this tranche)

1. **No shim perf claims (SPEC §0/09 — REFUTED).** PT-2 asserts NO speedup. The word
   "faster" appears nowhere in a gate. The de-nightly is a stability port whose *expected*
   outcome is cyc/B parity, and the gates test exactly that.
2. **No digit-scan SIMD (SPEC §0/02 — NO_GO).** `number_simd.rs` is one of the eight ported
   files. W1 ports its aarch64 NEON path to stable and **purges its x86 arm** — it does NOT
   pursue the Mula/Lemire in-vector digit accumulation (latency-bound CPI inversion
   0.13→0.30 regresses the product lane). The port is byte-for-byte behaviour-preserving.
3. **No block-scheduling pursuit (SPEC §0/04 — refuted).** The port introduces no
   multi-block software pipelining; the movemask primitive is fixed at the narrow idiom (W1),
   not by unrolling classify over blocks.
4. **Warm benches forbidden (P45).** Every bench herein is cold per-parse, single-touch,
   black-boxed, N≥200.
5. Carried, non-binding-but-respected: fold-over-cursor dead, tape-navigation-not-the-get-lane
   — bbnf-side laws; PT-2 touches neither surface.

### 1.4 Supersessions

- PT-2 supersedes and **fully implements** SPEC §2b **PT-N3** ("de-nightly, honestly"),
  upgrading it from REVIEW-NOT-EXECUTE to full-implementation depth per the owner's PT-1/PT-2
  full-implementation dispatch. The proof-grade patch (PROVE-09 `patch.diff`, +358/−14, 12
  files) is the *seed*; this tranche specifies its productionization.
- **Placement law UNCHANGED (P23, non-clobber, HANDOFF §6.9).** `~/Programming/parse-that`
  is READ-ONLY to the bbnf-lang campaign. PT-2 is authored bbnf-side (its durable home is
  this file) and rides letter **BBNF-PT-G-2026-07-16** as a contingent proposal with the
  patch attached; its landing is executed by the owner or a parse-that-side session against
  a scratch copy (R3 §8 recipe). The one cross-repo exception is W3's generator fix, which
  is bbnf-lang source (§4 W3, §2 interlock G3).

---

## 2. Interlocks

| Edge | Partner | Binding |
|---|---|---|
| **G0 — shared handshake** | **PT-1.W0** | PT-2.W0 *is* PT-1.W0 — one joint wave. Both parse-that Rust tranches (PT-1 = the prefilter tier, PROVE-10; PT-2 = this reform) operate on ONE scratch copy of parse-that and ONE captured baseline. The handshake stands up the scratch copy once, captures the baseline once, and coordinates the single BBNF-PT-G letter — preventing the two tranches from racing on shared files (P21, agent-orchestration). Neither tranche's W1 opens until W0's baseline is banked. |
| **G1 — depends-on** | GN-0.(x86 purge) | PT-2's x86 abrogation (W1 excision + W5 guard rails) mirrors GN-0's bbnf-lang x86 purge; the structural-token-gate pattern and workspace-lint form are inherited from GN-0, not reinvented. PT-2 does not *depend* on GN-0 landing first (parse-that is a separate repo) but adopts its enforcement shape verbatim. |
| **G2 — depends-on** | PT-1.W-flagship | The prefilter patch (PROVE-10) adds `regex/prefilter.rs` to the same scratch tree PROVE-09 noted was "shared/clobbered by a concurrent PROVE-10 session" (REPORT §Integration). W0 sequences the two tranches' file territories so PT-2's `simd/` and PT-1's `prefilter.rs` never co-write; PT-2.W1 rebases onto PT-1's tree state as banked at W0. |
| **G3 — cross-repo seam** | bbnf-lang `crates/codegen/src/emit/regex/` (greenfield, post GN-1 port) | W3's generator Debug fix is bbnf-lang source (landable in-campaign, not read-only). The regenerated `generated.rs` output flows to parse-that via the parse-that-side `scripts/bootstrap-regex.sh` run. **After GN-0 archives the legacy `crates/`, W3 targets the greenfield regex-emit home ported by GN-1 (`crates/codegen/src/emit/regex/`), NOT the retired `crates/core/src/generate/regex/emit/`.** W3 also inherits a repoint precondition: parse-that's `.cargo/config.toml` `bbnf -> ../../bbnf-lang/crates/core` patch (which `scripts/bootstrap-regex.sh` invokes as the generator) must repoint to the greenfield facade `crates/bbnf` — owned by GN-0.W2's SEAM repoint extended beyond egraph, or carried on BBNF-PT-G alongside the egraph patch-table repoint (§4 W3). W3 has a bbnf-lang half (emit `#[derive(Debug)]`) and a parse-that half (repoint + regen + drop the residual feature); both must land for the stable build to go green. |
| **G4 — feeds** | letter BBNF-PT-G-2026-07-16 | PT-2's five waves + the challenge wave ride the letter as PT-N3's implementation body, with `patch.diff` + `VERIFY.md` attached as the honest-value evidence (stability, refuted-speed). |

**Owner gates (SPEC §3, defaults proceed absent veto):**
- §3.2 **release.yml aarch64-only in parse-that CI** — W5 is the **sole parse-that home** for it (PT-N3 = de-nightly = PT-2, per the SPEC's "when PT-N3 lands"); PT-1.W6 and PT-3.W5 INHERIT it, they do not re-author. Owner ratifies (default yes, mirrors the x86 abrogation).
- The letter delivery itself (§3.1) — owner or parse-that-side session places BBNF-PT-G.

---

## 3. Wave manifest

| Wave | Name | Scope (one line) | LOC (+new/−del) | Gate | Hard cap |
|---|---|---|---|---|---|
| **W0** | The Handshake | Joint with PT-1.W0: one scratch copy, one banked baseline, letter coordination | +40 / −0 | born-verified (baseline banked) | 20 min |
| **W1** | The Stable Port | `portable_simd`→`core::arch::aarch64` across 8 scanners via a `simd/` module; x86 arms purged; movemask fixed at the narrow idiom | +520 / −190 | **born-RED** (no-regression + insn-count) | 45 min |
| **W2** | The Excisions | `smallbox[nightly]`→stable `smallbox!`; drop `portable_simd` + `stmt_expr_attributes` from `lib.rs` | +12 / −6 | born-verified (alloc-neutral) | 15 min |
| **W3** | The Generator Debug Fix | Emit `#[derive(Debug)]` not its expansion; regen `generated.rs`; drop `fmt_helpers_for_derive` | +20 / −55 | **born-RED** (full stable build) | 30 min |
| **W4** | The Arena (or its refutation) | Parser-ownership bump-slab over the 86 `Parser::new` sites — lifetime-sound OR not at all | +300 / −150 *or* +30 / −0 | **born-RED** (alloc gate ∨ measured refutation) | 45 min |
| **W5** | The Guard Rails | Toolchain→stable; `[workspace.lints]`; aarch64 CI assertion; x86 structural token gate | +50 / −5 | born-verified (stable build + zero x86 tokens) | 25 min |
| **W6** | Challenge: Re-Measure | Adversarial re-audit of every PT-2 numeric claim on fresh windows | +40 / −0 | born-RED (all claims reproduce) | 40 min |

---

## 4. Per-wave specification

### W0 — The Handshake (joint with PT-1.W0)

**Objective.** Stand up the single shared substrate for both parse-that Rust tranches and
bank one baseline, so W1 measures against a fixed anchor and the two tranches never race.

**File-level plan.**
- Materialize the scratch copy per **R3 §8** verbatim: `rsync -a --exclude target/ --exclude
  .git/ --exclude node_modules/ $SRC/ $DST/`, then repoint the seven external sibling patch
  paths in `$DST/rust/.cargo/config.toml` to absolute originals (read-only consumption). The
  `rust-toolchain.toml` at `$DST/` carries the `nightly-2026-04-11` pin; `data/` (124M,
  gitignored) is copied by the rsync for benches. `CARGO_TARGET_DIR=$DST/target` (P27:
  one cargo per target dir).
- Author `$DST/scripts/pt-scratch-recipe.sh` (+40) — the codified R3 §8 recipe so both
  tranches (and W6) rebuild the identical scratch deterministically. This is the sole new
  file; nothing lands in `$SRC`.
- Bank the **baseline** on the pristine copy FIRST (before any patch): build both bench
  binaries, capture the paired divan tables for all six JSON corpora (`apache-builds`,
  `canada`, `citm_catalog`, `data`, `data_xl`, `twitter`) + the CSS corpora, N≥200, two
  windows, per-block loadavg. Snapshot `otool -tv` reduction-instruction counts on the hot
  classify/movemask functions (baseline `addv.8b` ≈ 509, `tbl` ≈ 516 — VERIFY attack #2).
  Bank `cargo run --example alloc_count` (construct = 6 allocs/5960 B; parse twitter = 5390
  allocs — PROVE-09 REPORT).

**Seams.** R3 §8 (recipe), R3 §1 (patch table, the 7 external sibling dirs), R3 §6 (divan
bench targets + exact commands), PROVE-09 `examples/alloc_count.rs` (the alloc probe).

**Gate (born-verified).** `bash scripts/pt-scratch-recipe.sh && cargo test -p parse_that`
→ EXIT 0 (~400 tests green on the pristine copy); the baseline tables + insn counts + alloc
counts written to `docs/tranches/sk-v24/pt-baseline/`. Failure semantics: a non-building
scratch copy (broken patch table) halts both tranches — the recipe is load-bearing.

**Consumer in-wave (Y.13).** The banked baseline is consumed immediately by W1's
no-regression gate; the scratch recipe is consumed by every subsequent wave and by W6.

---

### W1 — The Stable Port

**Objective.** Replace `portable_simd` with `core::arch::aarch64` NEON across the eight
scanner files, structured as a colocated `simd/` directory-module; purge the two x86 arms;
and fix the movemask so the port does not cost cycles — the exact defect that refuted the
proof-grade shim.

**File-level plan (colocation edict — productionizes PROVE-09's flat `simd128.rs`).**

Replace the flat proof-grade shim with a directory-module split by concern:

```
rust/parse_that/src/simd/
  mod.rs        (~40)  — the public u8x16/mask8x16 vocabulary + re-exports; cfg pivot
  neon.rs       (~210) — #[cfg(target_arch="aarch64")] core::arch::aarch64 impl
  scalar.rs     (~130) — #[cfg(not aarch64)] [u8;16] parity oracle (no target intrinsics)
```

- The eight scanner sites (R3 §2a) each swap one `use` line `std::simd::prelude::*` →
  `crate::simd::*`: `scanners.rs` (22 hits), `parsers/scan/decode.rs` (17),
  `parsers/scan/ws_comment.rs` (16), `parsers/scan/structural_bitmap.rs` (13),
  `parsers/scan/quoted_simd.rs` (7), `span_parser/scan.rs` (7),
  `parsers/scan/quote_parity.rs::classify_stripe_64` (portable half), `state.rs` (comments
  only — no live churn).
- **The movemask, the load-bearing production fix.** PROVE-09's `to_bitmask` used a bit-
  position `vand` + two `vaddv_u8` halves; `otool -tv` on the refuted patch showed **2271
  `addv.8b` vs baseline 509** (VERIFY attack #2) — the shim did ~4× the reduction work per
  `to_bitmask`, the mechanism behind window A's regression. The production `mask8x16` packs
  its iteration mask via the **`vshrn` narrow idiom** (SPEC §0/03 CONFIRMED; empirical spine:
  "vshrn intrinsic obviates hand asm for the mask-narrow idiom, +35% over scalar"): a single
  `vshrn_n_u16(reinterpret_u16(cmp), 4)` yields a 64-bit mask at **4-bit-per-lane stride**.
  The ~5 consumers that *iterate* the mask (`structural_bitmap.rs`, `quoted_simd.rs`,
  `quote_parity.rs`, `ws_comment.rs`, `decode.rs`) migrate their bit-scan to the nibble
  stride (`index = tz >> 2`); sites needing only presence stay on `any()` = `vmaxvq_u8`
  (cheap, unchanged). This is real, bounded call-site churn (~+80/−40) — the honest cost of
  optimal codegen that the zero-churn shim traded away for a regressing reduction tree.
- **x86 purge (the abrogation reaches parse-that).** Excise the x86 intrinsic arms:
  `parsers/scan/number_simd.rs:145-235` (SSE4.2 `_mm_*` + `is_x86_feature_detected!`), keep
  the NEON `:29-136` + scalar `:239` (−~90); `parsers/scan/quote_parity.rs:42-49`
  (`pclmulqdq` `prefix_xor`), keep the aarch64 `vmull_p64` `:51-56` + scalar `:58-68`
  (−~30). No `#[cfg(target_arch="x86_64")]` survives in `parse_that/src`.
- Productionize `examples/alloc_count.rs` into `parse_that/tests/alloc_probe.rs` +
  `benches/alloc_probe.rs` (the divan/probe home; P16 — out of `examples/` into the tracked
  test+bench surface).

**Seams.** R3 §2a (the 8-file portable_simd census, exact hit counts), R3 §4
(`number_simd.rs` + `quote_parity.rs` x86 loci, line-exact), PROVE-09 `patch.diff`
(the `use`-swap adaptation, the scalar oracle — lands as-is minus the movemask), VERIFY.md
attack #2 (the 2271-vs-509 counterexample the movemask must not reproduce), SPEC §0/03 +
empirical spine (the vshrn certification).

**Gate (born-RED — four clauses, each falsifiable).**
1. **Stable-clean surface** — `rg -c 'std::simd|portable_simd' rust/parse_that/src` → **0**.
   The `portable_simd` feature is gone from every scanner (remaining stable-build blockers
   are ONLY the W2/W3 residuals: `smallbox[nightly]`, the generated Debug expansion).
2. **Correctness** — `cargo test -p parse_that && cargo test -p parse_that --features
   diagnostics` → **0 failed** (~400 tests, R3 §7), plus a new `tests/simd_test.rs` that
   cross-checks every `neon.rs` primitive against `scalar.rs` on 10k random 16-byte vectors
   (byte-equality oracle).
3. **Instruction-count no-regression** — build baseline + patched combinator bench binaries;
   `otool -tv` reduction-instruction count (`addv.8b` + `shrn`/`vshrn`) on the hot
   classify/movemask functions must be **≤ 1.0× the W0 baseline** (guards the double-`vaddv`
   counterexample; baseline ≈ 509). Failure ⇒ the movemask is wrong ⇒ redress before landing.
4. **Paired no-regression bench** — the divan paired-binary protocol (§3.4): two windows,
   opposite arm order, N≥200, per corpus, min + median + best-min. Patched median must be
   **≥ 0.98× baseline on EVERY corpus in BOTH windows**. Failure ⇒ the port costs cycles ⇒
   the wave does not close. *This is precisely the gate PROVE-09's shim failed (window A
   0.81–0.98×); a production port that reproduces that failure is rejected.*

**Consumer in-wave (Y.13).** The `simd/` primitives are consumed by all eight scanners,
which are consumed by the JSON + CSS parsers that the ~400 tests and the paired bench drive.
No dormant kernel — every primitive has a live call site in the gate's own workload.

---

### W2 — The Excisions

**Objective.** Drop the `smallbox` nightly feature and the two remaining `lib.rs` feature
attributes that W1's port made dead, proving alloc-neutrality.

**File-level plan (PROVE-09 `patch.diff` lands nearly verbatim).**
- `parse_that/Cargo.toml:94` — `smallbox = { version="0.8", features=["nightly"] }` →
  `smallbox = "0.8"`.
- `parse_that/src/parse.rs:1,74` — `use smallbox::{SmallBox, space::S32}` gains `smallbox`;
  `SmallBox::new(parser_fn)` → `smallbox!(parser_fn)` (stable `dyn`-unsizing; same S32 inline
  / heap-spill threshold — codegen- and alloc-neutral, PROVE-09 §Alloc).
- `parse_that/src/lib.rs:1,3` — delete `#![feature(portable_simd)]` (W1 excised its last
  use) and `#![feature(stmt_expr_attributes)]` (dead — PROVE-09 REPORT). `fmt_helpers_for_derive`
  STAYS one wave more (W3 clears it); `pub mod simd;` is declared here.

**Seams.** R3 §2c (the smallbox sole use site `parse.rs:1,62,74`), PROVE-09 `patch.diff`
lines 5-10, 100-122, 77-98 (the exact edits).

**Gate (born-verified — alloc-neutral).** `cargo build -p parse_that` (nightly, since W3's
residual still forces it) → green; `cargo test -p parse_that` → 0 failed; the alloc probe
(`tests/alloc_probe.rs`) shows construction allocs = **6** and per-parse twitter = **5390**,
**byte-identical to the W0 baseline** (P: `smallbox!` ≡ `SmallBox::new`). Failure semantics:
any alloc delta means the unsizing macro is not equivalent — investigate before landing.

**Consumer in-wave (Y.13).** `Parser::new`'s `smallbox!` site is exercised by every
constructed parser in the test suite; the alloc probe is the direct consumer measuring it.

---

### W3 — The Generator Debug Fix

**Objective.** Make `regex/generated.rs` carry `#[derive(Debug)]` instead of its expanded
`#[automatically_derived] impl` (which calls the perma-unstable
`core::fmt::Formatter::debug_tuple_field1_finish`), so the last nightly feature
(`fmt_helpers_for_derive`) falls and the FULL stable build goes green. Regen-only (P53).

**File-level plan (cross-repo — G3 interlock).**
- **Diagnose the expansion source (first task).** `generated.rs` is `@generated` from
  `bootstrap/regex.bbnf` via parse-that's `scripts/bootstrap-regex.sh`, which invokes the
  bbnf-lang generator through the `.cargo/config.toml` `bbnf` patch. **Repoint precondition
  (G3):** that patch currently reads `bbnf -> ../../bbnf-lang/crates/core` (R3 §1), but GN-0
  archives the legacy `crates/` and GN-1 mints the greenfield generator, so the patch must
  repoint to the greenfield facade `bbnf -> ../../bbnf-lang/crates/bbnf` (which re-exports
  `crates/codegen`). This repoint is owned by GN-0.W2's SEAM repoint extended beyond egraph,
  or is carried on the BBNF-PT-G letter alongside the egraph patch-table repoint — W3 does not
  land until it resolves (the bootstrap script cannot invoke a generator behind a dangling
  patch path). The expansion enters at one of two seams; grep both:
  1. **bbnf-lang emitter (greenfield)** — `crates/codegen/src/emit/regex/` (the ported regex
     codegen home; `mod.rs`, `scanner_plan.rs`, `dfa/`, `simd.rs`, `negated_class.rs` — GN-1
     decomposes the retired `crates/core/src/generate/regex/emit/` into the greenfield
     `codegen` crate's `emit/`). If the emitter writes an `impl core::fmt::Debug` textually
     (rather than a `#[derive(Debug)]` attribute on the generated struct), the fix is here.
  2. **parse-that bootstrap** — `scripts/bootstrap-regex.sh`. If a `cargo expand`/rustfmt
     post-step expands the derives, the fix is to stop expanding.
- **The fix.** Emit the *attribute* `#[derive(Debug)]` on the generated parser structs, never
  its `automatically_derived` expansion. This is a bbnf-lang source change (landable
  in-campaign) if seam 1; a parse-that-side script change (via the letter) if seam 2. Both
  halves — the generator emitting the attribute AND the parse-that regen — must land.
- **Regenerate** by running `scripts/bootstrap-regex.sh`; the emitted `generated.rs` shrinks
  (the hand-expanded Debug impls collapse to one derive attribute per struct, ~−55/+8).
- `parse_that/src/lib.rs` — delete `#![feature(fmt_helpers_for_derive)]` (the residual is
  now unforced).

**Seams.** PROVE-09 REPORT §"Nightly surface" + `patch.diff:82-91` (the residual diagnosis:
"all 20–21 stable errors are in `generated.rs`"), VERIFY attack #5 (the retained feature is
itself an `E0554` — the port is not stable-clean until this falls), `crates/codegen/src/emit/
regex/` (the greenfield codegen home, GN-1-ported from the retired `crates/core/src/generate/
regex/emit/`), GN-0.W2 SEAM + BBNF-PT-G (the `bbnf` patch-table repoint owner), P53
(regen-only, idempotent-on-disk).

**Gate (born-RED — the cumulative stable-build gate, can fail).**
1. **Debug de-expanded** — `rg -c 'debug_tuple_field1_finish|automatically_derived.*Debug'
   rust/parse_that/src/regex/generated.rs` → **0**.
2. **Regen idempotent** — run `scripts/bootstrap-regex.sh` twice; `git diff --stat
   generated.rs` after the second run → **empty** (byte-identical, P53).
3. **FULL stable build** — `cargo +stable build --workspace` (in the scratch copy, requires
   W1+W2 already landed) → **EXIT 0, zero E0554**. This is the tranche's cumulative
   stable-build gate; the last residual falls here. Failure semantics: if `generated.rs`
   still carries the expansion, `fmt_helpers_for_derive` is still forced and the stable build
   fails — exactly PROVE-09's unmet gate clause (1). The gate proves de-nightly is COMPLETE.

**Consumer in-wave (Y.13).** The regenerated regex parser is consumed by the regex-bridge
tests (`tests/regex.rs` + the `#[path]` subtests, R3 §7) and the `regex-bootstrap` bench;
the stable build itself is the consumer proving the derive fix compiles without the feature.

---

### W4 — The Arena (or its measured refutation)

**Objective.** Move the combinator vtable boxes off the per-construction heap into a
Parser-ownership bump-slab arena — **lifetime-sound or not at all** (owner edict). The
alloc-count probe is the arbiter; a measured refutation is a valid close (P4/P39).

**File-level plan.**
- The target: `Parser<'a,O>` owns `parser_fn: SmallBox<dyn ParserFn+'a, S32>`
  (`parse.rs:61`). An arena would own every closure and hand back `&'arena dyn ParserFn`.
  The tree already ships `BumpSlab` (`bump_slab.rs`, used by the regex generated parser at
  `regex/generated.rs:174` — PROVE-09 §Arena). The refactor threads an `'a`-scoped arena
  through the `Parser` type and the **86 `Parser::new` call sites** (PROVE-09 REPORT).
- **The named unsoundness trap (why "or not at all").** `ParserFn<'a>` is parameterized by
  the INPUT lifetime `'a`. A `'static`/thread-local arena holding `'a`-parameterized closures
  is **unsound** (PROVE-09 REPORT §Arena) — the arena's lifetime must be `≤ 'a` and threaded
  explicitly, never smuggled. The sound formulation makes the arena a parameter of the parse
  entry point, `'a`-bounded; if the borrow checker cannot accept a sound threading within the
  cap, the refactor does NOT land.
- The reward ceiling is *known and small*: the arena moves only the **6 construction allocs**
  (5960 B), never the **5390 per-parse allocs** (Vec growth in `many`/`sep_by` + string
  arena — the real mass, untouched by vtable boxing; PROVE-09 §Alloc). W4 is high-risk,
  low-reward by measurement — which is precisely why its close may be a refutation.

**Seams.** PROVE-09 REPORT §"Why the bump-slab arena was NOT the vtable substitute"
(the soundness analysis, the 86-site count, the 6-vs-5390 alloc split), R3 §5
(`parse.rs:61` Parser type, `bump_slab.rs` BumpSlab, the alloc hotspot census),
`regex/generated.rs:174` (the existing BumpSlab consumer, the sound-usage template).

**Gate (born-RED — dual-outcome, either branch closes with an artifact).**
- **Branch A (landed).** `cargo run --example alloc_count` (or `tests/alloc_probe.rs`):
  construction allocs **strictly < 6** (vtable boxes coalesced into ≤1 arena alloc) AND
  per-parse twitter allocs **== 5390** (the arena must not touch the hot loop). Soundness:
  `cargo build` green with NO lifetime `transmute`, and `cargo +nightly miri test
  --test alloc_probe` clean. ~400 tests still green.
- **Branch B (refuted).** If no sound `'a`-threading compiles under the cap: W4 closes on the
  written refutation recording the unsoundness counterexample (the `'static`-arena-holds-`'a`-
  closures trap) + the confirmed 6-alloc reward ceiling, filed to `docs/tranches/sk-v24/
  NEGATIVE-RESULTS.md`. The arena is retired; per P39 this is a legitimate close because the
  *measurement* is the artifact (mirrors SPEC §0's refutations-as-law), not a scope-reveal.
- Failure semantics (the ONE illegitimate outcome): landing an *unsound* arena (a lifetime
  `transmute`, a leaked `'static`) — that fails the gate absolutely. Sound-or-refuted, never
  sound-appearing.

**Consumer in-wave (Y.13).** The alloc probe is the direct consumer of the construction path;
the full parse suite consumes the arena on the hot path proving per-parse invariance.

---

### W5 — The Guard Rails

**Objective.** Modernize the toolchain off the ICE-pinned nightly onto stable, and install
the four-layer x86 abrogation in parse-that source, mirroring GN-0.

**File-level plan.**
- `rust-toolchain.toml` (repo root, R3 §1) — `channel = "nightly-2026-04-11"` → `channel =
  "stable"` (the pin existed only to dodge nightly ICEs — R3 §1 comment; de-nightly removes
  its reason). Drop `rustc-codegen-cranelift` from components if stable-unavailable.
- `rust/Cargo.toml` — add `[workspace.lints.rust]` / `[workspace.lints.clippy]` denying the
  x86 surface and unsafe-without-safety-comment; each member gains `[lints] workspace = true`.
- `.github/workflows/*.yml` — the **aarch64 CI assertion** (owner gate §3.2): the release/CI
  job runs on `macos-14` (Apple silicon) and asserts the structural token gate. Mirror
  GN-0's release.yml aarch64-only shape.
- Codify the **structural x86 token gate** (abrogation layer 1) as a CI step +
  `tests/no_x86.rs`: `rg -E '_mm_|_mm256_|_mm512_|x86_64::|is_x86_feature' rust/*/src` must
  return zero — a deliberate probe test that fails if any x86 intrinsic returns.
- **Sole-home partition (owner §3.2 + the abrogation layer set).** W5 is the ONE parse-that
  author of the four abrogation layers — structural token-gate CI step + `tests/no_x86.rs`,
  workspace lints, aarch64 CI assertion, and `release.yml` aarch64-only (§3.2). PT-1.W6 performs
  the x86 scan-file *deletion* those layers enforce and CONSUMES them (its gate greps `scan/`);
  PT-3.W5 inherits `release.yml` at its 1.1.0 cut. No other wave re-authors this layer set.

**Seams.** R3 §1 (the toolchain pin + its ICE rationale), R3 §4 (the now-purged x86 loci
W1 cleared), SPEC §4 + HANDOFF §5 (x86 abrogation 4-layer: structural token gate primary,
workspace lints, aarch64 CI assertion, emitter stops stamping AVX), SPEC §3.2 (the owner
gate for release.yml).

**Gate (born-verified).** `cargo +stable build --workspace` green under the new channel;
`cargo test -p parse_that --test no_x86` → pass (0 x86 tokens); `cargo clippy --workspace
-- -D warnings` clean under the workspace lints. Failure semantics: any re-introduced x86
token or a nightly-only construct trips the structural gate — the abrogation is enforced by
artifact, not convention.

**Consumer in-wave (Y.13).** CI is the consumer of the toolchain + lints; `tests/no_x86.rs`
is the live consumer of the structural token gate.

---

## 5. W6 — Challenge: Re-Measure (the one adversarial wave)

**Objective.** Turn PROVE-09's own refutation discipline on PT-2. A non-author fleet
re-measures every numeric claim on FRESH windows and audits every "done" for prototype
residue — the exact method (paired binaries, opposite arm order, `otool` instruction census)
that refuted the shim's speedup.

**Audit list (each maps to a prior wave's gate).**
1. **W1 no-regression, re-measured.** Rebuild baseline + patched combinator/CSS bench
   binaries from the W0 recipe; run the divan paired protocol on **two fresh windows,
   opposite arm order** from W1's. Confirm patched ≥ 0.98× on every corpus in both windows.
   *Named counterexample hunted:* window asymmetry (VERIFY.md's finding) — if the two windows
   disagree in sign on any corpus, the no-regression claim is inside the noise floor and the
   port must be re-instrumented, not declared neutral.
2. **W1 movemask, re-audited.** `otool -tv` the patched hot functions; assert the
   reduction-instruction count did NOT balloon (the 2271-vs-509 defect). Confirm the `vshrn`
   idiom lowered, not a hidden double-`vaddv`.
3. **W3 stable build, independently re-proven.** `cargo +stable build --workspace` from a
   clean scratch rebuild. *Named counterexample:* the retained `fmt_helpers_for_derive`
   `E0554` (VERIFY attack #5) — the challenge fails if any nightly feature survives.
4. **W2/W4 alloc profile, re-run.** `alloc_probe`: construction + per-parse counts match the
   waves' claims (W2: 6/5390 unchanged; W4: <6 construction if landed, or the refutation
   holds).
5. **W1/W5 x86 purge, re-grepped.** `rg -E '_mm_|x86_64::|is_x86_feature' rust/*/src` → 0.
   *Named counterexample:* a surviving `pclmulqdq`/SSE4.2 arm behind a `cfg`.
6. **Honest-value audit.** Confirm no gate, doc line, or letter clause claims a *speedup* —
   the value is stability (§1.1). A resurrected perf claim is a defect.

**Gate (born-RED).** Every clause reproduces on independent re-measure; the challenge report
+ a `tests/pt2_challenge.rs` codifying the x86-grep and stable-build assertions land in
`docs/tranches/sk-v24/`. Failure semantics: any claim that does not reproduce (as PROVE-09's
1.34–1.57× did not) is REFUTED and its owning wave re-opens before the letter ships.

**Consumer in-wave (Y.13).** W6 consumes every prior wave's emitted artifact (binaries,
generated.rs, alloc probe, CI config) as its measurement surface.

---

## 6. LOC ledger, exit criteria, risks

### 6.1 LOC ledger

| Wave | +new | −deleted | Net |
|---|---|---|---|
| W0 | 40 | 0 | +40 |
| W1 | 520 | 190 | +330 |
| W2 | 12 | 6 | +6 |
| W3 | 20 | 55 | −35 |
| W4 (landed) | 300 | 150 | +150 |
| W4 (refuted) | 30 | 0 | +30 |
| W5 | 50 | 5 | +45 |
| W6 | 40 | 0 | +40 |
| **Total (W4 landed)** | **982** | **406** | **+576** |
| **Total (W4 refuted)** | **712** | **256** | **+456** |

Generated-file budget: `generated.rs` net **shrinks** (~−55) at W3 — the derive de-expansion
reduces generated LOC, a healthy direction (P53 generated-size discipline).

### 6.2 Exit criteria

1. `cargo +stable build --workspace` green under the stable channel (W1+W2+W3+W5).
2. `rg 'std::simd|portable_simd|smallbox.*nightly|fmt_helpers|stmt_expr' rust/parse_that/src
   rust/parse_that/Cargo.toml` → **0** (all four nightly enablers excised).
3. `rg -E '_mm_|x86_64::|is_x86_feature' rust/*/src` → **0** (x86 abrogation reaches parse-that).
4. ~400 tests + `simd_test.rs` + `no_x86.rs` green; W1 no-regression bench passes on two
   windows; alloc profile within claim.
5. W4 closed on a sound landing OR a filed measured refutation.
6. W6 reproduces every claim; the letter ships with `patch.diff` + `VERIFY.md`, value framed
   as STABILITY, zero speedup claim.

### 6.3 Risks with named counterexamples

- **R1 — The movemask regresses again.** The `vshrn` nibble-stride mask changes bit-scan
  semantics; a botched consumer migration re-introduces the double-`vaddv` cost or corrupts
  index arithmetic. *Counterexample:* PROVE-09's 2271 `addv.8b`. *Guard:* W1 gate clause 3
  (insn count ≤ baseline) + clause 2 (`simd_test.rs` oracle catches index corruption) + W6
  re-audit.
- **R2 — Stable build still blocked after W3.** The Debug expansion has a second source
  (rustfmt/expand step) not caught by the emitter fix. *Counterexample:* VERIFY attack #5's
  surviving `E0554`. *Guard:* W3 gate clause 3 (full `cargo +stable build`) is born-RED and
  cannot pass with any residual; W3's first task greps BOTH seams.
- **R3 — The arena lands unsound.** A tempting `'static` arena or lifetime `transmute` passes
  the borrow checker via `unsafe` and appears to work. *Counterexample:* the
  `'static`-arena-holds-`'a`-closures trap (PROVE-09 §Arena). *Guard:* W4 gate forbids
  lifetime `transmute`, requires MIRI-clean; sound-or-refuted, never sound-appearing.
- **R4 — Cross-repo drift at W3/G3.** The bbnf-lang emitter fix and the parse-that regen land
  out of sync; `generated.rs` in parse-that lags the generator. A sharper variant: GN-0 removes
  the legacy `crates/` before the `bbnf` patch repoints, so `scripts/bootstrap-regex.sh`
  invokes a dangling generator path and W3 cannot regen at all. *Guard:* W3 requires BOTH
  halves + idempotent regen + the resolved `bbnf -> crates/bbnf` repoint (owned by GN-0.W2 SEAM
  or BBNF-PT-G); G3 names both the greenfield seam and the repoint precondition explicitly.
- **R5 — A resurrected speed narrative.** An executor re-frames the port as "faster" from a
  cherry-picked window. *Counterexample:* PROVE-09's refuted 1.34–1.57×. *Guard:* W6 clause 6
  (honest-value audit) + §1.1's zero-perf-claim charter.
- **R6 — Scratch-copy race with PT-1.** PT-2's `simd/` and PT-1's `regex/prefilter.rs`
  co-write the shared scratch tree (PROVE-09 noted the concurrent-session clobber).
  *Guard:* W0/G2 sequences the file territories and banks one tree state before either W1.
