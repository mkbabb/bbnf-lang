# LOCK-9 · Family 9A — ONE-KERNEL-LIBRARY (pass-3, research-A)

Base pins for every measurement below: parse-that HEAD `ef10d5b`; bbnf-lang/skinny
HEAD `06e70053f`; host aarch64-apple-darwin (Apple M), Darwin 25.x; parse-that
toolchain `nightly-2026-04-11` (its pin), bbnf-simd stable-clean
(`core::arch::aarch64`, only `blake3` dep, no `#![feature]` in `src/`).

**Thesis.** One aarch64 kernel home serves both repos. `bbnf-simd`
(`skinny/crates/bbnf-simd`) is the substrate: already NEON-native, already
stable, already parity-gated (11 `checkasm_*` admission suites, scalar reference
as executable spec). parse-that's eight `portable_simd` scanner files become thin
consumers; the rust↔rust isomorphism becomes physical, not asserted. The move
discharges parse-that's WIDEST nightly blocker (`portable_simd`, 8 files) by
consumption rather than the refuted shim, and forces the canonical movemask to be
authored **once**.

---

## 1. The mechanism, made concrete

**(1a) Author the contract-split canonical movemask producers into the shared
home; purge the refuted idiom.** The refutation is physically present in the
substrate today. `byte_class_from_eq_set_64.rs:79-87` packs each 16-lane compare
with `vandq_u8(bit-pattern)` + **two `vaddv_u8`** (low/high halves) — the exact
double-addv the PROVE-09 verify counted at 2271 `addv.8b` vs a 509 baseline.
`byte_class_from_table_64.rs:1-4` is worse: the "NEON" body merely calls the
scalar reference — the dense-64 producer does not exist. The lawful LEAN idiom
(memchr's aarch64 backend: `cmeq.16b` → `shrn.8b v,v,#4` → `fmov` → `and
#0x8888…`, seen in the PROVE-10 disasm) exists in the substrate only as
`aarch64/movemask.rs:5` — a `u16` helper with a scalar stitch loop, unwired into
any 64-byte path. The two canonical forms to author:
  - **LEAN nibble-`vshrn`** — find-first / membership masks consumed by one CTZ.
    `vshrn_n_u16::<4>` narrows 16×u8→8×nibble; `& 0x8888_8888_8888_8888`; then
    `rbit`+`clz` (or nibble-CTZ). One `shrn` + one `and` per 16 B, **zero**
    reduction op. Replaces the `vaddv` pair.
  - **DENSE dense-64** — true one-bit-per-byte 64-bit masks that are stitched
    across four stripes, re-iterated, and AND/XOR-combined with sibling masks
    (structural∧quote∧escape). Four `vshrn`-narrowed stripes packed by shift-or
    (the simdjson-NEON dense form), NOT a per-stripe `vaddv` tree. Wholly absent
    from `byte_class_from_table_64`.

The classify fan-out already collects the right mask vectors
(`classify_tbl4.rs:31-43`: `vqtbl4q_u8` lo6 table, `vceqq` for
structural/terminator/escape/control). Only the pack layer (`movemask_u8x16` at
`classify_tbl4.rs:5` and the eq-set helper) carries the refuted reduction — so
authoring is a **pack-layer replacement**, not a kernel rewrite.

**(1b) Physicalize the sync gate.** parse-that already patches every bbnf-lang
crate it consumes through `parse-that/rust/.cargo/config.toml` `[patch.crates-io]`
(`bbnf`, `bbnf-ir`, `bbnf-ser`, `egraph`, `egraph-derive` → `../../bbnf-lang/…`;
`csp-solver` → `../../csc411/…`). Adding `bbnf-simd → …/bbnf-lang/<home>` is **one
line in an already-load-bearing table** — the vendoring seam physically exists.
Post-GN-0 the home lives in the greenfield `crates/` (skinny retires); the repoint
is the PT-R.5 note re-timed to GN-0/GN-1. No new build machinery.

**(1c) Rewrite the scanners as consumers.** Each `use std::simd::prelude::*` →
`use bbnf_simd::…`; the per-stripe classify body → a call to the shared `prim::*`
entry; the STRIPE HARNESS (`PaddedView`, tail clamp, CTZ iterate, positions `Vec`)
stays consumer-side (glue, not kernel). This is the opposite of the PROVE-09 route
(a per-repo `simd128.rs` shim re-implementing `std::simd`'s vocabulary that
shipped the double-addv): a shim duplicates the kernel plane; consumption unifies
it.

---

## 2. Scanner-file mapping census (owed by F1)

Eight `portable_simd` files (R3 §2a) vs the substrate's ~18 `aarch64/` kernels:

| parse-that file | hits | class | shared-home target (status) |
|---|---|---|---|
| `ws_comment.rs` | 16 | **1:1-kernel** | `prim::comment_body_mask_64` (EXISTS, NEON) |
| `quote_parity.rs` | 4 | **1:1-kernel** | `prefix_xor_64`=`bitmap_prefix_xor_64` (EXISTS); pmull `aes` gate |
| `quoted_simd.rs` | 7 | **1:1-kernel** | `escape_mask_64` (`lib.rs:174`, EXISTS) |
| `structural_bitmap.rs` | 13 | **1:1-kernel** | `byte_class_from_table_64`/`classify_tbl4` (EXISTS; dense pack owed) |
| `decode.rs` | 17 | **vector-tier** | classify shared; `store_chunk` 16-B arena write is consumer-arena (no-go) |
| `scanners.rs` | 22 | **vector-tier** | `find_ascii_set_member64`/`byte_class_from_eq_set_64` |
| `span_parser/scan.rs` | 7 | **vector-tier** | same membership kernels; span-state harness stays |
| `state.rs` | 2 | **no-go** | `PaddedView` comments only — harness, never kernel |

**Split: 4 files 1:1-kernel (40 hits), 3 vector-tier (46 hits), 1 no-go (2 comment
tokens).** The substrate is already a *superset* of parse-that's scan vocabulary —
it additionally ships `structural_classify_64`, `bracket_depth_mask_64`,
`bitmap_next_set_bit`, `bulk_emit_positions_64`, `unescape_uxxxx`, `string_block`,
`match_tiny_plain_string`, `utf8/validate_block` — so the consumption direction is
bbnf-simd→parse-that, not a merge of equals.

---

## 3. F1 — residual one-home vs two-home divergence (enumerated deltas)

After the canonical producers are authored once and the double-addv purged:

- **Padded-view / clamp convention.** parse-that: `PaddedView` +
  `INPUT_PAD_BYTES=64` trailing NUL (`state.rs:57,122`); the tail stripe reads
  into the pad, bits `≥ len` masked off (`structural_bitmap.rs:134-151`).
  bbnf-simd: fixed `&[u8;64]` blocks + a **scalar** tail loop (`lib.rs:117-121`)
  and a separate `EobBlock`/`eob_pad_clamp`. **This delta does not live in the
  kernel** — every shared entry takes `&[u8;64]`; view/clamp is harness. Net
  kernel divergence from this axis: **zero.**
- **Single- vs multi-table shuffle** (the genuine duplication risk). "byte ∈ S"
  has three impls: nibble-LUT (parse-that `classify_16`, `structural_bitmap.rs:37-44`
  — lo/hi `swizzle_dyn`, 2 `tbl`/16 B, arbitrary 256-set); lo6 single-table
  (`classify_tbl4.rs`, `vqtbl4q_u8`, 1 `tbl`/16 B, gated by `lo6_table_admissible`,
  `dispatch.rs:101-113`); `vceqq`-fan (`byte_class_from_eq_set_64.rs:51-57`, ≤8
  members). Distinct cost points, not redundant. **Two-home cost:** each repo
  picks a tier and re-authors it with its own scalar reference + parity test
  (~150-200 LOC/tier). **One-home cost:** the home exposes the tier once behind a
  selection predicate (the `lo6_table_admissible` gate generalized; ties to
  9C/LOCK-2 cost model); both repos share the parity suite. Saved: 2 duplicated
  membership kernels + oracles.
- **Capacity constants.** `STRIPE=64` agree. `INPUT_PAD_BYTES=64` has no bbnf-simd
  analogue (harness). `Vec::with_capacity(len/8+8)` (`lib.rs:107`) is a
  positions-density heuristic — consumer-side. No kernel delta.

**Maintenance / drift basis (empirical).** The two homes have *already drifted on
the load-bearing axis*: bbnf-simd hand-packs with the refuted double-`vaddv`;
parse-that rides `std::simd::to_bitmask`, which lowers to the lawful `shrn #4`. A
single fix must otherwise be applied and re-verified twice, and the observed drift
proves the mirror does not self-synchronize. One home makes the movemask
refutation a **single** edit propagating to both consumers under **one** checkasm
parity gate. Both directions remain valid to report, but the basis favors one home
precisely because the drift is already real.

---

## 4. F1 — where cross-crate `#[inline]` fails the seam

Direct entries are measured-live at 0 in-loop indirect because every `prim::*`
façade and every aarch64 body is `#[inline]`/`#[inline(always)]` (`lib.rs:255-352`,
`byte_class_from_eq_set_64.rs:32`, `classify_tbl4.rs:16,21,46`) — their MIR crosses
the rlib boundary and inlines even under `lto=false`. The failure surface is
**precise and singular**: the `OnceLock<PrimitiveKernels>` fn-pointer table
(`dispatch.rs:49-87`). Its members (`byte_class_from_table_64`,
`bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`,
`eob_pad_clamp`) are **runtime-selected function pointers** — an indirect call no
`#[inline]` and no LTO can devirtualize (callee chosen at runtime). Under the bench
profile that governed prior measurements (`lto=false`, `cgu=16`, O3), any producer
that were *not* `#[inline]` and not generic would additionally stay a static
cross-CGU call. So the shared home must expose the **direct
`#[cfg(target_arch="aarch64")]` return** form (as
`prim::byte_class_from_eq_set_64`/`structural_classify_64` already do,
`lib.rs:283-309`) and must NOT route the hot path through the `OnceLock` table. On
M-class silicon that table is pure overhead regardless: NEON is architectural
baseline on aarch64 — there is no non-NEON aarch64 to detect, so the runtime
dispatch buys nothing and costs one indirect call per block. **9A recommendation:**
the consumer surface is cfg-gated direct entries; `dispatch.rs`'s fn-pointer
indirection retires for aarch64 (kept only if a scalar-fallback build target
survives, which x86-abrogation says it need not).

---

## 5. Prior art (citations)

- **memchr 2.x aarch64 backend** — the LEAN reference: `cmeq.16b` + `shrn #4`
  movemask + `& 0x8888…` nibble mask, 64 B/iter unrolled with `orr`+`umaxp`
  presence reduce (reproduced in the PROVE-10 disasm). Already a parse-that dep
  and the PROVE-10 patch dep — the producer bbnf-simd's pack layer should emit.
- **simdjson / sonic-rs NEON stage-1** — the four-stripe dense-64 movemask via
  `shrn`-narrow + shift-or pack (Langdale & Lemire, "Parsing Gigabytes of JSON per
  Second"; Keiser/Lemire NEON follow-up). The DENSE producer to author.
- **Lemire & Mula, "Faster Bitmap Movemask on ARM NEON"** — establishes
  `vshrn_n_u16(#4)` as the canonical `to_bitmask` and documents the `vaddv` tree as
  the *slow* alternative; the primary citation for purging the double-addv.
- **rust `std::simd` aarch64 `Mask::to_bitmask`** — lowers to `shrn #4` on recent
  toolchains; explains why parse-that's native (non-shim) scanners are *closer to
  lawful* than bbnf-simd's hand pack — but nightly and per-repo, which the shared
  home replaces with a stable, single-authored equivalent.
- **Google Highway / dav1d checkasm** — the cross-target "kernel + scalar oracle +
  differential parity" discipline bbnf-simd already mirrors (`tests/checkasm_*`,
  scalar reference as executable spec, `lib.rs:245-248`). One-home keeps it
  single-sourced.
- **regex-automata prefilter/accel tiering** — PROVE-10's `Prefilter::from_dfa`
  reuses `Memchr1/2/3`; the shared home's membership-tier selection is the same
  shape one level down (byte-set→kernel), reinforcing that *selection* is
  cost-model work (9C/LOCK-2), *kernels* are shared primitives.

---

## 6. Named risks & counterexamples

- **The home must not re-ship the refutation.** If extraction copies
  `byte_class_from_eq_set_64.rs`/`classify_tbl4.rs` verbatim, both repos inherit
  the double-addv. 9A is a win only if the pack-layer purge (§1a) is a precondition
  of extraction, gated by checkasm parity + `otool` reduction-op count ≤ the LEAN
  target.
- **PaddedView coupling.** parse-that kernels are entered through `PaddedView<'_>`
  (a borrowed view over parse-that `State`). Sharing those signatures drags
  parse-that types into the home. Mitigation: shared entries take `&[u8;64]` /
  `&[u8]` only (as bbnf-simd already does). Counterexample: `decode.rs`'s
  `store_chunk` writes a parse-that arena `&mut Vec<u8>` — correctly **no-go**;
  forcing it in imports consumer state.
- **`dispatch.rs` indirection leak.** If consumers call the fn-pointer route
  (`prim::byte_class_from_table_64`) instead of a direct cfg entry, the seam
  re-introduces an in-loop indirect call — the very thing "0 in-loop indirect"
  certifies against.
- **Version/name collision — absent here** (unlike LOCK-5's `bbnf-regex 0.1.0`
  dual). parse-that ships **no** `bbnf-simd` crate; the home is single-sourced, so
  the sync gate is a patch repoint, not a name-authority contest.
- **`prefix_xor` feature gating.** `quote_parity.rs:51` gates the pmull path on
  `target_feature="aes"`; `bitmap_prefix_xor_64` must match that gate or the shared
  entry silently drops to the 5-shift scalar identity on a non-`aes` build — a
  correctness-preserving but perf-divergent path that must be parity-tested.
- **Skinny-retirement timing.** The home's final address is the greenfield
  `crates/` (GN-1 port-in), not `skinny/`. A 9A prototype before GN-0 patch-points
  at `skinny/crates/bbnf-simd` and must carry the address-moves note, else the sync
  gate is stale on day one.

---

## 7. What a prototype must test to move 9A

1. **Pack-layer purge, parity-locked.** Replace the `vaddv` pack in
   `byte_class_from_eq_set_64.rs:79-87`; author the dense-64 producer in
   `byte_class_from_table_64.rs` with `vshrn #4` forms; gate on the existing
   `checkasm_byte_class_from_eq_set_64`/`checkasm_byte_class_from_table_64` suites
   + an INJECT_BUG differential (bit-parity vs scalar reference, alignment sweep,
   `black_box` inputs; verify a naïve nibble-stride corrupts dense-64 parity).
   Evidence: `otool -tv` reduction-op count per site (target: 0 `addv`, N `shrn`),
   base-pinned to `06e70053f`.
2. **Consume one 1:1 file end-to-end.** Repoint `ws_comment.rs` (or
   `quote_parity.rs`) at `bbnf-simd` via the one-line patch; delete its
   `portable_simd` body; confirm parse-that's `ws_comment_test`/`quote_parity_test`
   pass on **stable** (removes `portable_simd` from that file's `lib.rs` blast
   radius).
3. **Cross-seam inline proof.** Build the parse-that combinator bench against the
   patched home under `lto=false, cgu=16`; disasm the hot classify: shared `prim`
   body inlined (no `bl` to a bbnf-simd symbol in-loop) AND no path through the
   `OnceLock` fn-pointer table (0 in-loop indirect), base-pinned.
4. **Paired cold no-regression** (PROVE-09 law: paired interleave mandatory, two
   windows, opposite arm order, per-block loadavg). Consumed-home vs pristine
   scanner, JSON+CSS, N≥200: gate is **non-regression** (≥0.98×), NOT a speed
   claim — 9A's value is unification + stability + single-authored lawfulness; the
   refuted 1.34× is law. Name N, both windows, their HEAD.
5. **Duplication ledger.** Enumerate, as concrete LOC, the membership kernels +
   scalar oracles + parity tests a two-home layout would duplicate vs the one-home
   count — the empirical basis for the F1 divergence verdict.
