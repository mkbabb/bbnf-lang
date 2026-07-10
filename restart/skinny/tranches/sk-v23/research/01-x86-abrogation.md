# SK-V23 W1 — The x86 Abrogation Census (both repos)

Date: 2026-07-09. Mandate: aarch64 / Apple-M only, everywhere. This is the
complete inventory of every x86 / SSE / AVX / wasm-SIMD surface across
bbnf-lang and parse-that, with file:line and LOC, plus the enforcement
design that prevents regrowth. parse-that is READ-ONLY (the non-clobber law);
its items are recorded as PT-track proposals, never direct edits.

Headline: the abrogation is NOT the deletion SK-V22 already booked. SK-V22
W0.6 owns only the *legacy-tree archival* (the `simd-scan` crate and its
skinny stub vanish when legacy-core archives at GW0.c/GW1). Four surfaces
survive that archival and are NEW work: (1) the two live emit sites in
crates/core that *stamp* AVX2 into every generated file, (2) a live x86
`avx512bw` capability flag inside skinny's greenfield cost model, (3) the CI
matrix — every bbnf workflow runs on x86 `ubuntu-latest`, so the aarch64 NEON
paths are never exercised, and (4) there is no enforcement at all (no
`[workspace.lints]`, no structural gate). All four are SK-V23 W1.

---

## Part A — bbnf-lang census

### A.1 The `simd-scan` crate (legacy core; SK-V22 archives the whole crate)

`crates/simd-scan/` — per-arch structural-bitmap kernel, consumed by
`crates/core` (Cargo.toml:18) and benched by skinny bbnf-bench as `simd_scan`
(skinny/crates/bbnf-bench/Cargo.toml:40).

| file | LOC | disposition | evidence |
|---|---|---|---|
| `src/avx2.rs` | 370 | DELETE (pure x86 AVX2) | 66 x86 tokens; `_mm256_*` nibble-LUT kernel |
| `src/avx512.rs` | 149 | DELETE (pure x86 AVX-512 VBMI2) | 24 tokens; `_mm512_mask_compressstoreu_epi8` |
| `src/wasm.rs` | 433 | DELETE (wasm32 SIMD — in scope per "wasm-simd") | `i8x16.swizzle` / `i8x16.bitmask` |
| `src/parity.rs` | 249 | STRIP x86 arm | arch-neutral, but `prefix_xor_64` selects CLMUL vs shift-XOR by `cfg!(target_feature)` (head-doc:27-31); the x86 CLMUL path goes, NEON PMULL + shift-XOR stay |
| `src/compaction.rs` | 112 | STRIP x86 arm | `tzcnt`-loop arch-neutral (keep) + `_pext_u64` `bmi2` cfg branch (delete) — head-doc:19-25 |
| `src/lib.rs` | 114 | REWRITE dispatch | module gates `avx2`/`avx512`/`wasm` at 58-65; runtime `is_x86_feature_detected!` at 93-109; reduce to `neon` (55-56) + `scalar` |
| `src/neon.rs` | 719 | KEEP (the sole kernel) | aarch64 NEON |
| `src/scalar.rs` | 129 | KEEP (portable fallback) | — |
| `src/alphabet.rs`,`src/index.rs` | 229,103 | KEEP (arch-neutral) | — |
| `Cargo.toml` | 32 | STRIP | `avx512` cargo feature (line ~32); description enumerates "NEON / AVX2 / AVX-512 / WASM SIMD" (line 5) |

Note: `crates/ir/src/passes/recognizers/kernel_shape.rs` (3 hits) is the
arch-neutral selector feeding `simd-scan`; its doc-comments claim x86
(`vpshufb per 16-byte lane on AVX2` :12,:45; `CLMUL/PMULL parity (x86)` :18).
Dies with simd-scan or its docs prune.

### A.2 The emit sites — THE TRUE FIX (NEW; survive archival)

Two generators in crates/core stamp the `#[cfg(all(target_arch = "x86_64",
target_feature = "avx2"))]` arm into *every* generated grammar file. Under
clean-regen discipline these are the only correct place to fix; the generated
files (A.3) are their output.

- **`crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs`**
  — 902 LOC, the structural-kernel emitter. Emits `nospace_bitmap_64_avx2`
  and `first_quote_or_backslash_avx2` under x86 cfg: dispatch arms at
  541-542 and 644-645; AVX2 bodies at 595-598 (`nospace...avx2`, `use
  core::arch::x86_64::*`) and 683+ (`first_quote...avx2`). Fix: emit only the
  aarch64 NEON arm + a scalar `#[cfg(not(target_arch = "aarch64"))]` fallback;
  drop the x86_64 arm entirely.
- **`crates/core/src/generate/regex/emit/simd.rs`** — 341 LOC, the regex-scan
  emitter. Emits the AVX2 nibble-LUT scan: `'avx2_scan` block gated at
  140-142 (`#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]` →
  `use ::core::arch::x86_64::*`), with the `#[cfg(not(...))]` scalar
  complement (133-134). Doc-header 87-99 describes the AVX2 vs fallback split.

### A.3 The generated stamps (output of A.2 — regenerate, never hand-patch)

Each carries the dead AVX2 pair (`nospace_bitmap_64_avx2` +
`first_quote_or_backslash_avx2`). After A.2 is fixed, `cargo xtask regen`
removes them; the regen-drift gate then forbids re-adding.

| file | x86 stamp lines |
|---|---|
| `crates/core/src/grammar/generated/bbnf.rs` | 102 (multiple kernels: nospace @4971, first_quote @5057, dispatch @4918/5019) |
| `css_l4.rs` | 26 (nospace_avx2 @16162, dispatch @16109) |
| `json.rs`,`math.rs`,`csv.rs`,`bnf.rs`,`ebnf.rs`,`css_pretty.rs`,`google_sheets.rs` | 26 each |

Total ≈ 310 lines of dead AVX2 stamped across the generated tree today.

### A.4 CI matrix — every workflow is x86 (NEW; the silent inversion)

- `.github/workflows/ci.yml:12` — `runs-on: ubuntu-latest` (x86_64) for the
  `rust` job; `:95` extension job likewise. **Consequence: the aarch64 NEON
  kernels are never CI-compiled or tested; only the x86 AVX2 + scalar arms
  run.** The abrogation *and* the mandate to CI-test NEON both require this
  flip to aarch64 (`macos-14` Apple Silicon or `ubuntu-24.04-arm`).
- `.github/workflows/bench-iai.yml:25` — `runs-on: ubuntu-latest` (x86); the
  iai cycle-count baseline is measured on the wrong arch. Flip.
- `.github/workflows/release.yml:16-33` — LSP release matrix ships five
  targets incl. `x86_64-unknown-linux-gnu`, `x86_64-apple-darwin`,
  `x86_64-pc-windows-msvc`. Under "aarch64/Apple-M only" the x86 + windows
  targets drop; retain `aarch64-apple-darwin` + `aarch64-unknown-linux-gnu`.
  (Policy call — record for adjudication; a distributed LSP arguably keeps
  x86 targets even when the parser is NEON-only. Flag, do not presume.)
- skinny has **no CI** (`skinny/.github` absent) — a greenfield aarch64-only
  workflow is NEW.

### A.5 skinny — one live x86 residue; otherwise clean

Skinny's kernel plane is already aarch64 + scalar only:
`bbnf-simd/src/dispatch.rs` and `lib.rs` gate solely `#[cfg(target_arch =
"aarch64")]` NeonTbl4 with a scalar default; `parse-that-regex/src/lib.rs`
(all aarch64/scalar, e.g. :523,:785,:813); skinny codegen stamps **no** x86.

The exception is a live x86 capability flag in the greenfield cost model:
- **`skinny/crates/passes/src/lib.rs:380`** —
  `avx512bw: cfg!(all(target_arch = "x86_64", target_feature = "avx512bw"))`
  inside `TargetFeatures::host()`; consumed at `admits_collapsed_stage`
  (~:960) `target.avx512bw && …`; test seed at :1944. On aarch64 this is
  permanently `false`, so the collapsed-stage admission path is dead — but it
  *models an x86 capability inside greenfield*. Abrogate the field and its
  consumer as part of greenfield ratification (WART-class residue).

### A.6 Dependencies (transitive x86 — arch-neutral source)

- `memchr` (skinny/Cargo.toml:44, parse-that-regex Cargo.toml:11) — internal
  AVX2/SSE2 dispatch, but arch-aware and NEON-capable. Keep.
- Competitor-bench deps that transitively pull `cpufeatures` (x86 runtime
  detection, no-op on aarch64): `sonic-rs`/`simd-json` at crates/core
  Cargo.toml:88,101 and skinny bbnf-bench Cargo.toml:22-23. Bench-only,
  arch-neutral source; retain but note they still *compile* cpufeatures on
  aarch64. No `getrandom` direct x86 dep found.

---

## Part B — parse-that census (READ-ONLY; PT-track proposals only)

All NEW. Non-clobber: recorded here as PT-R*/PT-T* proposals; prototyping on
scratch copies only.

| file:line | LOC | x86 surface |
|---|---|---|
| `rust/parse_that/src/parsers/scan/quote_parity.rs:42-48` | ~7 | `#[cfg(all(target_arch="x86_64", target_feature="pclmulqdq"))]` — `_mm_clmulepi64_si128` prefix-XOR. aarch64 AES/PMULL arm (:51-55) + scalar fallback stay; drop x86 arm |
| `rust/parse_that/src/parsers/scan/number_simd.rs:138-235` | ~100 | full x86 SSE4.2 impl: `count_leading_digits_sse` (:145), `accumulate_digits_sse` (:171), `scan_digits_simd` (:210) with `is_x86_feature_detected!("sse4.2")` (:222). aarch64 NEON (:29,:68,:116) + `cfg(not(any(aarch64,x86_64)))` scalar (:239) stay |
| `rust/parse_that/src/parsers/scan/decode.rs:168,222` | 0 | doc-claim only (`_mm_storeu_si128 (x86)`); prune prose |
| `rust/parse_that/src/scanners.rs:229,309` | 0 | doc-claim only (`two VPSHUFB`, `AVX2/SSE2` via memchr) |
| `rust/parse_that/src/state.rs:51,62`; `tests/padded_input_test.rs:104` | 0 | doc-claim only (AVX-512 zmm 64-byte alignment rationale) |
| `rust/parse_that/Cargo.toml:93,107` | — | `memchr 2.5` (keep, arch-aware); `simd-json 0.14` competitor bench (pulls cpufeatures) |
| `rust/parse_that/.github/workflows/ci.yml` | — | PT proposal: inspect its runner arch; assert aarch64-only |

---

## Part C — Enforcement design (so x86 cannot regrow)

Four layers. Layer 3 is the load-bearing one; the rest are defense-in-depth.

### C.1 Emitter guarantee + regen-drift gate (the structural root)

The generated x86 stamps exist because A.2's emitters write them. Fix the two
emit sites to emit only `{aarch64 NEON arm, scalar cfg(not(aarch64)) arm}` —
no `target_arch = "x86_64"` token is ever produced. Then `cargo xtask regen`
purges the generated tree, and the **already-existing** drift gate
(ci.yml:55-58 `cargo xtask regen --check`, diff vs checked-in → non-zero
exit) makes any hand-added x86 stamp fail CI mechanically. This converts
"don't add x86" from a review rule into a build invariant. (Aligns with the
clean-regen-discipline + one-codegen-path precepts.)

### C.2 Structural token gate (model: Lock-14 allowlist + skv15_w0 rejects)

A workspace test that walks `git ls-files` over `src/` + generated + emitters
and greps for the forbidden token set —
`target_arch = "x86_64"`, `core::arch::x86_64`, `_mm`, `__m128/256/512`,
`target_feature = "avx…|sse…|pclmul…|bmi2"`, `target_arch = "wasm32"`,
`is_x86_feature_detected!` — asserting **zero** hits outside a small
`ALLOWLIST`. Build it exactly on two extant patterns:
- the const-table shape of
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs` (`pub const ALLOWLIST:
  &[AllowlistEntry]` with path/class/mutability) — post-abrogation the
  allowlist holds only the gate file itself (self-reference) and, transiently,
  the aarch64/scalar arms of any remaining runtime-detect dispatch;
- the falsification-test discipline of `skinny/xtask/tests/skv15_w0.rs`
  (`validate_*` + a suite of `rejects_*` negative tests) so the gate is proven
  to actually reject a smuggled x86 arm, not merely pass on clean input.
Lands in both workspaces (`xtask/tests/` root, `skinny/xtask/tests/`).

### C.3 Workspace lint floor (`[workspace.lints]` — today absent)

Neither `Cargo.toml`, `skinny/Cargo.toml`, nor `crates/core/Cargo.toml`
declares `[workspace.lints]`. Add `[workspace.lints.rust]` with
`unexpected_cfgs` in deny mode plus a `check-cfg` allow-list that names only
the sanctioned cfgs (`target_arch = "aarch64"`, `target_arch = "wasm32"` iff
kept, feature flags) — any *new* cfg key (a resurrected `avx512` feature, a
stray `target_feature`) then surfaces as an unexpected-cfg denial at compile.
`unexpected_cfgs` does not catch a bare `target_arch = "x86_64"` (a real cfg),
so C.2 remains the primary; C.3 catches feature-flag regrowth and typos.

### C.4 CI arch assertion (aarch64-only, hard-failing)

Flip ci.yml + bench-iai.yml runners to `macos-14` (Apple Silicon) or
`ubuntu-24.04-arm`, and add a first step that hard-fails on wrong arch:
`case "$(uname -m)" in arm64|aarch64) ;; *) echo "x86 runner forbidden";
exit 1;; esac`. Add the missing skinny workflow (same assertion). This both
enforces the abrogation and finally CI-exercises the NEON kernels that x86 CI
never touched. release.yml x86-target drop is a flagged policy decision
(A.4), not an automatic flip.

---

## Part D — Ownership ledger (SK-V22 owned vs SK-V23 W1 NEW)

**SK-V22 W0.6 already owns (SUPERSEDED → GW0.c/GW1):** deletion of the
`crates/simd-scan` crate (A.1) via whole-tree legacy-core archival; deletion
of skinny's empty `simd-scan` stub (SPEC.md:70,79,101). SK-V22 §0 (:305-308)
declares the *posture* "no x86 SIMD work" — but books no census, no emitter
fix, no enforcement.

**NEW for SK-V23 W1 (not covered by any prior plan):**
1. Emitter-side stamp removal — support.rs + emit/simd.rs (A.2); the fix that
   makes the archival stick instead of regrowing when codegen graduates.
2. skinny `TargetFeatures.avx512bw` x86 residue — passes/src/lib.rs:380 (A.5).
3. CI arch flip + skinny CI creation + release-matrix policy call (A.4).
4. All four enforcement layers (Part C) — none exist today.
5. Entire parse-that surface (Part B) — PT-track proposals, non-clobber.
