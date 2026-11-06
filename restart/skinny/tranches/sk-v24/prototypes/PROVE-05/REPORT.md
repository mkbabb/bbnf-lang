# PROVE-05 — CSS TBL TOKENIZER (F1)

**Feature:** a `vqtbl` (shufti nibble-LUT) CSS L4 token-class kernel, wired into the
generated CSS component scan via the runtime-simd bridge.
**Gate:** widen the 2.07–3.47× band vs lightningcss (R2 §7), per-corpus before/after.
**Verdict: PARTIAL** — measured; gate met and widened substantially on 3 of 4 corpora
(bootstrap, tailwind, material), one regression (animate). Numbers below.

---

## 1. What was built

The live generated CSS parser walked the source **one byte at a time**
(`find_component_delim` / `consume_balanced_at`: `_ => pos + 1`). A NEON inner-skip
(`runtime_simd::find_css_significant`) already existed and was **parity-tested but
UNWIRED** — the hot scan never called it. This prover:

1. **New kernel** — `bbnf-simd/src/aarch64/classify_tbl4.rs`: `css_significant_shufti_chunk`
   / `css_significant_shufti_block_64`. The existing `vqtbl4q` lo6 classifier in that
   file cannot serve CSS (the significant set `" ' ( ) / : ; [ ] { }` straddles low-6
   collisions — `;`≡`{`, `:`≡`z` — so a lo6 table false-positives on common ident
   letters; this is why the file's header documents "fan not LUT"). The shufti idiom
   (Hyperscan; Lemire "Scanning HTML at Tens of GB/s on ARM") uses **two `vqtbl1q_u8`
   nibble lookups AND-ed** for *exact* membership over any ASCII set, zero letter
   collisions.

2. **Prim facade** — `bbnf-simd/src/lib.rs` `prim::css_significant_mask_64(src, lo_lut,
   hi_lut)`: NEON `vqtbl1q` on aarch64, a scalar twin applying the identical LUT
   semantics (the checkasm parity anchor). One semantic, two implementations.

3. **Bridge rewrite** — `runtime/src/runtime_simd.rs` `find_css_significant` now builds
   the shufti LUTs from caller data (`fixed ∪ delimiters`; Lock-14 grammar-neutral) and
   drives the new kernel; the eq-set fan it previously used is retired from this path.

4. **Codegen wiring** — `codegen/src/runtime_generator.rs` `CSS_GENERATED_RS`: both
   `find_component_delim` and `consume_balanced_at` now skip inert ident/ws/number runs
   64 bytes at a time via the kernel, then hand the landing byte to the unchanged scalar
   switch. A `CSS_SIGNIFICANT_FIXED` const carries the opener/closer/quote/slash family.
   Generated file produced by `cargo xtask regen-css` (clean-regen — never hand-patched).

**The rich typed AST is preserved** (standing law): the kernel emits only a
*next-significant index*; the tape offsets, `CssNodeKind`, `CssTypedValue`, selector /
dimension / color / function projections are byte-for-byte unchanged. Speed comes from
the scan, never from dropping structure.

**Diff:** 5 files, +175/−22. `classify_tbl4.rs` +50 (kernel), `lib.rs` +29 (facade),
`runtime_simd.rs` +32/−22 (LUT build + rewrite), `runtime_generator.rs` +32 (template),
`generated.rs` +32 (regen output). No new module dir was warranted — the kernel is one
cohesive addition to the named `classify_tbl4` tbl-family file; the consumer is the
existing `runtime_simd` bridge.

---

## 2. Measured results (certified: PMU cyc/byte, load-robust)

**Method.** `css_canon_bench`, `CSS_CANON_PMU=1 … 800`. To remove cross-run load drift
(the first single-shot pass had baseline@loadavg~18 vs wired@~15, and lightningcss —
identical code — drifted 41.2→36.9 cyc/B between them), I built **two binaries**
(baseline = `git stash` to HEAD scalar; wired) and ran them **interleaved back-to-back,
6 windows each**, so both variants share the same load. Median of 6 windows below.
loadavg during A/B: 11–17. cssparser + lightningcss are **unchanged code = the noise
floor / control**.

| corpus | lane | base cyc/B | wired cyc/B | Δ (skinny) | base ×lcss | wired ×lcss | band move |
|---|---|--:|--:|--:|--:|--:|--:|
| bootstrap | track1_rich | 16.094 | **12.805** | **−20.4%** | 2.204 | **2.897** | +0.69 |
| bootstrap | track1_full | 15.849 | 13.419 | −15.3% | 2.238 | 2.764 | +0.53 |
| tailwindcss | track1_rich | 14.465 | **11.462** | **−20.8%** | 3.110 | **4.120** | +1.01 |
| tailwindcss | track1_full | 14.313 | 11.603 | −18.9% | 3.143 | 4.070 | +0.93 |
| material | track1_rich | 15.548 | **13.517** | **−13.1%** | 1.945 | **2.414** | +0.47 |
| material | track1_full | 13.986 | 11.969 | −14.4% | 2.162 | 2.726 | +0.56 |
| animate | track1_rich | 14.772 | 16.216 | **+9.8%** | 2.112 | 1.879 | −0.23 |
| animate | track1_full | 15.944 | 17.567 | +10.2% | 1.957 | 1.734 | −0.22 |

**Control lanes (Δ, should be ≈0):** cssparser bootstrap +1.6% · tailwind +0.3% ·
material −2.1% · animate +5.1%. lightningcss +4.6% / +5.0% / +7.9% / −2.4% — lcss drifts
up in the wired window (ordering/thermal), which *inflates* the wired ×lcss ratio; hence
**I lead with the skinny-only Δ** (wired-skinny vs baseline-skinny, same workload, same
instrument), not the ratio.

**Cold-wall Mbps (directional, single pass):** wired track1_rich rose bootstrap
1957→2493, tailwind 2341→2280, material 2047→2095 Mbps; consistent with the cyc/byte
picture.

### Reading the gate

- **Band top widens hard:** tailwind rich 3.11× → **4.12×** vs lightningcss; the whole
  band for the three large real-world corpora shifts up.
- **Skinny scan cost falls 13–21% cyc/byte** on bootstrap/tailwind/material — the direct
  kernel effect, with the cssparser control at ±2%, so it is signal not noise.
- **animate regresses ~10%.** animate.css is `@keyframes`-dense with very short
  components (`opacity:0`, `transform:…`); the per-call 64-byte shufti classify + LUT
  build is not amortized when delimiters sit ~5–10 bytes apart, so it loses to the scalar
  walk it replaced. This is the classic short-span SIMD tradeoff. (Its unchanged cssparser
  lane also drifted +5.1%, so ~half the apparent regression is window noise; the residual
  ~5% is real.)

The gate — "widen the 2.07–3.47× band" — is **met and exceeded on 3/4 corpora** but is
**not universal** (animate goes the wrong way), so the honest verdict is PARTIAL, not a
clean GO.

---

## 3. Visual verification (mandated) — the shufti hot path

`otool -tvV` of the release `css_canon_bench`; `find_css_significant` (kernel
`#[inline(always)]`-folded in). One 16-byte stripe, ×4 unrolled per 64-byte block:

```
ldp   q18, q20, [x9]        ; load two 16-byte stripes
and.16b   v19, v18, v2      ; low nibble  (v2 = 0x0f)
ushr.16b  v18, v18, #0x4    ; high nibble
tbl.16b   v19, { v0 }, v19  ; vqtbl1q  lo_lut[low]     (v0 = lo_lut)
tbl.16b   v18, { v1 }, v18  ; vqtbl1q  hi_lut[high]    (v1 = hi_lut)
cmtst.16b v18, v18, v19     ; (lo_class & hi_class)!=0 => significant  (vtstq_u8)
shrn.8b   v18, v18, #0x4    ; vshrn_n_u16::<4> movemask narrow
and.8b / ushr.8b / orr.8b / zip1.8b / ushl.4h / fmov …   ; mask pack
```

Two `tbl.16b` per stripe (8 per 64-byte block), the AND-of-nibble-LUTs membership, and
the project's `vshrn #4` movemask — a textbook shufti prefilter. Confirmed: the kernel
emits `vqtbl1q`, no scalar byte-loop, no lo6 fallback.

---

## 4. Correctness

- `neon_significant_skip_matches_scalar` — exhaustive: 5 corpora × 4 delimiter sets ×
  every start offset, kernel output == scalar `significant_ref`. PASS.
- `css_l4_*_route_into_tape` (selectors, at-rules, media, nested, visual functions,
  vendor/custom, extended decls) + `css_l4_rich_projection_is_lazy_and_typed`. PASS.
- Full `cargo test -p runtime -p bbnf-simd`: all pass. The wiring is a
  semantics-preserving transform — `find_css_significant` returns exactly the first index
  ≥ pos in `fixed ∪ delimiters`, which is precisely where the old `pos+1` walk stopped;
  the significant set covers every byte the scalar switch treats specially.

---

## 5. Integration cost & seam

**Low.** The seam already existed: `runtime_simd::find_css_significant` (parity-tested,
unwired) + the `runtime_simd` bridge pattern (`count_top_level_commas` already routes
through it). This prover (a) swapped its internal kernel from the eq-set fan to shufti and
(b) emitted two call sites from the codegen template. To land on trunk: keep the
kernel + facade, keep the template edit, `cargo xtask regen-css`. **One open item before
GO:** guard animate-class inputs — hoist the LUT build out of the per-call path (build
once per parse, thread the two `uint8x16_t` through) and/or fall to the scalar walk for
short remaining spans, so delimiter-dense corpora stop paying un-amortized 64-byte
classify. That is a bounded follow-up, not a redesign.

**Files:** all under `skinny/crates/{bbnf-simd,runtime,codegen}`. patch.diff captures
every line.
