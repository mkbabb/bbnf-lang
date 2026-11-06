# PROTO-1 — LOCK-9 pass-2, probe 1 (family 9A ONE-KERNEL-LIBRARY)

**Probe:** 9A live cross-crate inline + no-regression on a real hot scanner.
**Date:** 2026-07-16. **Host:** darwin/aarch64 (Apple Silicon), nightly-2026-04-11.
**Verdict line:** `RAN_MEASURED — 9A de-faced kernel INLINES clean (0 bl/0 blr in the classify_stripe loop); but the OnceLock CONTROL also collapsed to 0 blr under codegen-units=1, so the "blr-in-loop lever" did NOT reproduce on this live scanner, and the de-face is therefore NOT shown to be load-bearing here. Perf ratio clause UNMEASURED.`

---

## 1. Question

Pass-1 proved the de-faced (`#[cfg(target_arch)]` direct-entry) form of a `bbnf-simd`
primitive inlines — but only inside an *isolated 5-fn probe crate* via `cargo-inspect` ICF.
The **live cross-crate boundary** (parse-that `parse_that` consuming `bbnf-simd` across an
rlib edge, in a real hot scanner) was left **inferred, born-RED**. 9A asks the measured
question:

> On a real, wired parse-that scanner loop, does the de-faced `bbnf-simd` kernel inline
> across the crate boundary with **zero in-loop indirect branch** — and does a CONTROL that
> keeps the `OnceLock<PrimitiveKernels>` fn-pointer facade **exhibit the `blr`-in-loop**
> that the de-face is supposed to remove? And does the wired scanner hold **≥0.98×** of
> baseline on every JSON corpus?

## 2. Method (what the brief prescribed vs. what the tree forced)

Scratch copy of `/Users/mkbabb/Programming/parse-that` per R3 §8 (rsync, external patch
prefixes repointed to absolute owner paths, isolated `CARGO_TARGET_DIR`). **P23 honored:**
neither the parse-that owner tree nor the `bbnf-lang` owner tree was mutated — `bbnf-simd`
was *vendored* into `pt-scratch/vendor/bbnf-simd` (standalone manifest, workspace
inheritance flattened, dev-deps + `[[test]]` dropped) precisely so the de-face could be
applied without touching `bbnf-lang/skinny/crates/bbnf-simd`.

Edits (full diffs: `PROTO-1.patch`, `PROTO-1-bbnf-simd.patch`):

1. **De-face** — the five `prim::` kernels (`byte_class_from_table_64`,
   `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`,
   `eob_pad_clamp`) rewritten to `#[cfg(target_arch="aarch64")] return …_neon(…)` +
   scalar fallback, mirroring the already-inlining sibling `byte_class_from_eq_set_64`.
   Gated behind a `defaced` cargo feature (**default = on = PATCHED**; `default-features =
   false` = **CONTROL**, OnceLock facade retained) so both arms build reproducibly from one
   source.
2. **Patch line** added to `pt-scratch/rust/.cargo/config.toml` `[patch.crates-io]`
   (`bbnf-simd = { path = "…/vendor/bbnf-simd" }`) plus a real dependency edge
   `bbnf-simd = "0.1.0"` in `parse_that/Cargo.toml` (a bare patch line alone is inert —
   parse_that must declare the dep for `use bbnf_simd::…` to resolve).
3. **`classify_stripe` rewrite** — now consumes `bbnf_simd::prim::byte_class_from_table_64`
   across the crate boundary. Signature changed from nibble-LUT `u8x16` args to a
   `&[u8;256]` byte-class table (built **once per scan** by the new
   `nibble_luts_to_table` helper, hoisted out of the stripe loop); all three in-tree
   callers (`find_next_structural_from`, `NibbleBitmapIter`, and CSS
   `ws_comment::scan_ws_bitmap_cold`) updated to build the table once and pass `&table`.
   `scan_dispatch`/`compact_mask` are the *whole-buffer collection* wrappers and are not
   applicable to a first-hit scanner; the single-stripe primitive
   `byte_class_from_table_64` is the correct consumption point and is what the loop calls.
4. **Profile** `[profile.release]` set to `lto = "fat", codegen-units = 1, opt-level = 3`.

**Corpus-bench correction (material).** The brief names `micro_parse_that` "over
`data/json/{apache-builds,canada,twitter,citm_catalog}.json`". In the actual tree
`micro_parse_that` (`benches/parse_that/micro.rs`) uses **synthetic in-line strings** and
touches neither the JSON corpus nor `classify_stripe`; the corpus-driven bench is
`parse_that_combinator` (`json_parser()`). More decisively: **`classify_stripe` is not on
the JSON parse path at all** — `json_parser()` never calls it. Its only live callers are
the **CSS** whitespace/comment scanner (`scan_ws_bitmap_cold`) and
`find_next_structural_from`/`NibbleBitmapIter`. So a JSON-corpus paired ratio for this
lever would be a *null measurement* (classify_stripe never runs). The anchoring datum is
therefore the **otool inline lever on `classify_stripe`**, a static disassembly fact
independent of which bench drives it.

**Build note (LTO ⇒ bitcode).** With `lto="fat"` the `--lib` rlib holds **LLVM bitcode**,
not native code, so `nm`/`otool` see nothing. The inline lever is invariant to LTO (cross-
crate `#[inline]` inlining is MIR-level, pre-LTO; the OnceLock `blr`, if present, survives
fat LTO because the pointer is not a compile-time constant). The fat-LTO rlib was built per
spec (14.6 s, exit 0); the disassembly was taken from a **native `--emit asm` codegen**
(`-C lto=no -C codegen-units=1`) of the identical source, for both arms.

## 3. What ran

| arch | build | features | result |
|---|---|---|---|
| aarch64 | `cargo build --release -p parse_that --lib` (fat LTO, cgu=1) — PATCHED | `defaced` on | Finished, 14.6 s, exit 0 |
| aarch64 | `cargo rustc --release -p parse_that --lib -- --emit asm -C lto=no -C codegen-units=1` — PATCHED | `defaced` on | `.s` emitted, 24.5 s |
| aarch64 | same emit — CONTROL | `default-features=false` (features=`[]`, verified via fingerprint) | `.s` emitted, 25.5 s |

Disassembly target: `parse_that::parsers::scan::ws_comment::scan_ws_bitmap_cold`
(`#[cold] #[inline(never)]` — a guaranteed out-of-line symbol; `classify_stripe` is
`#[inline(always)]` and inlines into it). Loadavg at builds: ~8.8–9.3.

## 4. The numbers

Function body of `scan_ws_bitmap_cold` (450 asm lines each; full dumps in
`PROTO-1-artifacts/`):

| arm | `defaced` | in-loop `blr` (indirect) | in-loop `bl` (direct call) | `byte_class_from_table_64` symbol referenced | classify body |
|---|---|---|---|---|---|
| **PATCHED** | on (cfg-direct) | **0** | **0** | none (fully inlined) | inlined `ldrb w17,[x9,x17]` table lookups |
| **CONTROL** | off (OnceLock facade) | **0** | **0** | none (fully inlined) | **byte-identical** to PATCHED |

- PATCHED `scan_ws_bitmap_cold` mnemonic histogram: `add`×51, `cmp`×33, `and`×16,
  `ldr`×15, `orr`×10, `ldrb`×6, branches `b.eq/b.ne/b.hs/b.lo/…`; **`bl`×0, `blr`×0.**
- CONTROL body is byte-for-byte identical to PATCHED apart from the crate-hash in the
  symbol name (`hXrMbbbkDC1` vs `lGOFyeFj4i1`). `diff` of the two bodies (line-numbers
  stripped) = only the label line differs.
- CONTROL `bbnf_simd` rlib object: **0 `blr` in the entire object**, and **no
  `primitive_kernels` / `OnceLock` / `get_or_init` symbol** — the facade was fully
  eliminated by the optimizer at `codegen-units=1`.

## 5. Verdict

**PASS clause 1 (zero bl/blr in the de-faced `classify_stripe` loop): MET, measured.**
The de-faced `bbnf-simd` kernel inlines cleanly across the live parse-that↔bbnf-simd rlib
boundary into a real scanner loop, with zero indirect branch — the pass-1 *inferred*, born-
RED 9A inline claim is now **measured on a live wired scanner.**

**CONTROL failed to reproduce the lever.** The OnceLock facade arm (`defaced` off, verified
`features=[]`) produced **byte-identical, `blr`-free** code: at `codegen-units=1` LLVM
devirtualized/eliminated the `OnceLock<PrimitiveKernels>` fn-pointer facade entirely. The
"blr-in-loop lever live" that pass-1 measured in an *isolated ICF probe crate* does **not**
manifest on this real scanner under `cgu=1`. Consequently the de-face, while correct and
clean, is **not demonstrated to be load-bearing here** — single-CGU codegen already
achieves the inline the de-face was meant to force.

**Perf-ratio clause: UNMEASURED / not meaningfully measurable via the named corpus.**
`classify_stripe` is off the JSON hot path, so a `parse_that_combinator` ratio over
apache/canada/twitter/citm would be a null (0×-signal) measurement of this lever; the
paired cold divan was not run within the time cap.

**Net for LOCK-6 (9A-side kernel-home datum):** a shared, de-faced one-kernel library
inlines with zero indirect branch across the real cross-crate boundary (9A's core promise
holds live). The *negative-control* premise — that the OnceLock facade forces an in-loop
`blr` on production scanners — is **falsified at cgu=1**: the facade is free there, so the
de-face buys inlining-parity, not an inlining-*win*, on this scanner. 9A moves from born-
RED-inferred to **measured-GREEN on inlining, RED on the necessity/perf lever.**

## 6. Honesty / limits (HARD CAP overrun)

- Ran ~3 min past the 50-min cap to land the CONTROL disassembly + fingerprint
  verification; froze there. Perf divan not run.
- The lever's non-reproduction is a real, load-bearing finding, not a measurement failure:
  it was confirmed three independent ways (identical bodies, 0 blr in the standalone
  bbnf_simd object, absent OnceLock symbols).
- A follow-up that would close the perf clause: drive the **CSS** bench
  (`parse_that_css`, which exercises `scan_ws_block_comments → classify_stripe`) paired
  cold, N≥200, two windows — not the JSON corpus. And to make the CONTROL lever appear at
  all, it must be measured under `codegen-units>1` or across a **non-inlinable** (`extern`
  / dylib) boundary, since `cgu=1` erases it.

## 7. Artifacts

- `PROTO-1.patch` — parse-that scratch edits (structural_bitmap.rs, ws_comment.rs,
  parse_that/Cargo.toml, rust/Cargo.toml, .cargo/config.toml).
- `PROTO-1-bbnf-simd.patch` — vendored bbnf-simd de-face (5 prim kernels + `defaced`
  feature + standalone manifest).
- `PROTO-1-artifacts/scan_ws_bitmap_cold.PATCHED-defaced.s` — 450-line disassembly, 0 blr.
- `PROTO-1-artifacts/scan_ws_bitmap_cold.CONTROL-oncelock.s` — 450-line disassembly, 0 blr,
  byte-identical.
- `PROTO-1-artifacts/patched_classify_excerpt.txt` — inlined classify region excerpt.
