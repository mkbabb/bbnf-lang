# LOCK-9 · Family 9A — ONE-KERNEL-LIBRARY (pass-3, research-A)

Base pins for every measurement below: parse-that HEAD `ef10d5b`; bbnf-lang/skinny HEAD
`c43126422`; both hosts aarch64-apple-darwin, parse-that toolchain `nightly-2026-04-11`,
bbnf-simd stable-clean (`core::arch::aarch64`). The thesis: one shared aarch64 kernel home
serves both repos; parse-that scanners become thin consumers; the isomorphism is physical, not
asserted. The evidence says the home already exists (`skinny/crates/bbnf-simd`) as a
directory-module with a scalar/checkasm oracle spine — the open work is authoring the canonical
producers once, purging a refuted idiom that the shared home currently ships, and de-facing the
one structural barrier to cross-crate inlining.

## The substrate that exists (bbnf-simd is already the home)

`skinny/crates/bbnf-simd` is the candidate one-home, structured exactly as a shared kernel
library must be: `src/aarch64/` (18 NEON bodies), `src/scalar/` (parity references — the
executable specification), `src/dispatch.rs` (backend selection), `src/classifier.rs`, and a
`prim` facade at `src/lib.rs:250-353`. Every aarch64 body has a scalar twin and a
`tests/checkasm_*.rs` asm-parity oracle (dav1d model) — `checkasm_ascii_set_member_find_64`,
`checkasm_byte_class_from_table_64`, `checkasm_structural_terminator_64`,
`checkasm_bitmap_prefix_xor_64`, etc. Kernels present and named: `byte_class_from_table_64`
(full `[u8;256]`-table membership), `byte_class_from_eq_set_64` (≤8-member set,
`src/aarch64/byte_class_from_eq_set_64.rs:33`), `classify_tbl4` (single lo-6-bit `vqtbl1q`
table, `dispatch.rs:24-33`), `bitmap_prefix_xor_64`, `bitmap_next_set_bit`,
`bulk_emit_positions_64` (vectorised compaction, `lib.rs:227-242`), `comment_body_mask_64`,
`bracket_depth_mask_64`, `structural_classify_64` (simdjson stage-1 fan), `eob_pad_clamp`,
`string_block`, `unescape_uxxxx`, `digit_mac`, `match_tiny_plain_string`, `movemask`,
`quad_load`. This is the simdjson/sonic-rs stage-1 vocabulary already homed, oracle-gated.

The precedent for parse-that consuming a sibling skinny crate is live: `rust/.cargo/config.toml`
(R3 §1) already `[patch.crates-io]`-redirects `bbnf`, `bbnf-ir`, `bbnf-ser`, `egraph`,
`csp-solver` to `../../bbnf-lang/crates/*`. A `bbnf-simd` patch edge is one more line of the same
mechanism — extraction-as-vendor via the existing patch table, no publish step. And parse-that
already depends on two shared SIMD-kernel crates (`memchr 2.5`, `aho-corasick 1.1`, R3 §1): the
"one kernel library, many consumers" model (memchr → ripgrep/regex-automata/aho-corasick) is
already the in-tree pattern, not a novel imposition.

## The canonical producers are absent AND the home ships the refuted idiom

F1's premise is confirmed at the instruction level and it is worse than "absent." The refuted
double-`vaddv` movemask — PROVE-09/VERIFY §attack-2, which emitted **2271 `addv.8b` vs baseline
509** and is the mechanical cause of the refuted 1.34–1.57× claim (SPEC §0 rows 04/F1;
"the shim emits 2× the movemask reduces") — is **presently coded into five bbnf-simd sites**:
`byte_class_from_eq_set_64.rs:83-84`, `bracket_depth_mask_64.rs:78-79`,
`comment_body_mask_64.rs:72-73`, `structural_classify_64.rs:151-152`, and the hybrid
`movemask.rs:6-24` (a `vshrn_n_u16::<4>` narrow followed by a scalar `packed[pair*2]`
reconstruction loop — neither the LEAN nor the DENSE canonical form). Only
`match_tiny_plain_string.rs:101` uses the correct narrow idiom (`vshrn_n_u16::<4>` → `fmov`).
So extracting bbnf-simd *as-is* would propagate the refuted idiom into parse-that — the purge is a
**precondition of 9A**, not a cosmetic follow-on.

The two canonical producers to author once (the contract-split F1 names):
- **LEAN nibble-`vshrn`**: `cmeq.16b` → `shrn.8b v,v,#4` → `fmov x,d` yields a 64-bit "4-bits-
  per-lane" mask; `ands` with `0x8888…` / `count_ones` extracts positions. This is exactly the
  idiom PROVE-10/REPORT §2c disassembled as the *correct* hot path (`shrn.8b v0,v0,#4; fmov x8,d0;
  ands x8,#0x8888888888888888`) and SPEC §0-03's CONFIRMED "Fused NEON string scan (vshrn narrow),
  0.898-0.931× cyc/B, won in both interleave slots." For membership sites the four 16-lane compare
  vectors feed one `vshrn` each, `fmov`-stitched at offsets 0/16/32/48 into the u64.
- **DENSE dense-64 `vpaddq` tree**: for the wide `structural_classify_64`-style fan where many
  lanes are live, a `vpaddq_u8` reduction tree packs four stripes without the per-lane
  bit-position `vand` the double-`vaddv` form pays — the "won't reproduce the 509→2271 regression
  on dense masks" branch.

Both replace all five double-`vaddv` sites with a shared `mask64_from_four_stripes(...)` producer;
the per-site checkasm parity oracle (already present) is the correctness net across the swap.

## F1(a) — residual one-home vs two-home divergence, after purge

After the producers are authored once and the refuted idiom purged, the residual substrate
divergence across ALL membership + dense scan sites reduces to **two named conventions**;
everything else is byte-identical kernel logic already in bbnf-simd. Concrete deltas:

1. **Padded-view / clamp convention (the primary divergence).** parse-that uses a *whole-buffer*
   pad invariant: `state.rs:57` `INPUT_PAD_BYTES = 64`, `PaddedView` guarantees `bytes.len() ==
   len + 64` trailing NUL, so every stripe over-reads into the pad and masks off match bits `≥
   view.len()` (`structural_bitmap.rs` header contract) — the tail guard and scalar-tail loops
   vanish globally. bbnf-simd instead operates on fixed `&[u8;64]` blocks and clamps the final
   partial block *per call* via `eob_pad_clamp → EobBlock` (`lib.rs:276`, `scalar/eob_pad_clamp`).
   These are genuinely different boundary strategies (global over-read vs per-block copy-clamp);
   a one-home must pick one and thread it through both consumers. This is the single item that
   costs real reconciliation and needs a cross-boundary parity oracle.
2. **Membership-table representation (three forms today, must select).** parse-that's
   `structural_bitmap.rs:classify_16` uses the Langdale **dual nibble-LUT** shuffle
   (`lo.swizzle_dyn(lo_n) & hi.swizzle_dyn(hi_n)`). bbnf-simd offers a **single lo-6-bit table**
   `classify_tbl4` (admissible only when `lo6_table_admissible` — distinct low-6-bit slots,
   `dispatch.rs:101-113`) AND a **full `[u8;256]` table** `byte_class_from_table_64`. One-home
   collapses to one membership family with a separability predicate selecting the tier (the
   `lo6_table_admissible` guard, generalised).

Everything else converges with **zero** residual: movemask (one producer post-purge), prefix-XOR
(`bitmap_prefix_xor_64` ≡ parse-that `quote_parity::prefix_xor`), escape-mask (`escape_mask_64` at
`lib.rs:174-205` ≡ `quoted_simd::odd_parity_backslashes`), position emit
(`bulk_emit_positions_64` supersedes parse-that's inline CTZ push). Capacity constants align (both
STRIPE=64; both pad=64).

**Basis / drift-cost.** Two-home drift is not hypothetical — it is *already observed*: the
double-`vaddv` idiom exists independently in bbnf-simd (5 sites) AND was independently proposed
in parse-that's PROVE-09 `simd128.rs` shim, and *both* got it wrong the same way. Two homes
re-derive every kernel and re-introduce the same refuted idiom by parallel evolution; the
maintenance basis for one-home is 1 producer + 1 checkasm oracle vs 2 producers + 2 oracles + a
drift-detector. One-home residual = **2 reconciliation items** (pad convention, table tier);
two-home residual = **every kernel authored twice**. Both directions are technically valid; the
measured drift already on disk weights strongly toward one-home.

## F1(b) — where `#[inline]` fails to cross the crate seam

The non-inlinable boundary is **structural, not a cgu/LTO artifact**: it is the fn-pointer
dispatch facade. `dispatch.rs:50-56` `PrimitiveKernels` is a struct of `fn(...)` pointers,
resolved once through a `OnceLock` (`dispatch.rs:58-61`) and called indirectly by the `prim`
wrappers (`lib.rs:256-278` `byte_class_from_table_64`, `bitmap_prefix_xor_64`,
`bitmap_next_set_bit`, `bulk_emit_positions_64`, `eob_pad_clamp`). An indirect `blr` through a
function pointer is **never inlined regardless of LTO mode or cgu count** — LLVM cannot
devirtualise a runtime `OnceLock` load. That is the precise non-inlinable seam. bbnf-simd is
*inconsistent*: `byte_class_from_eq_set_64`, `structural_classify_64`, `comment_body_mask_64`,
`bracket_depth_mask_64` already bypass the facade with direct `#[cfg(target_arch="aarch64")]`
static dispatch (`lib.rs:285-289`, `:305-309`, `:322-328`, `:344-352`) — no fn-pointer.
**De-facing = making all prims direct-cfg**, which the directive correctly frames as
bbnf-simd-internal hygiene, one route, not a perf lever.

Once de-faced (direct `#[cfg]` dispatch to `#[inline]` bodies — the `_neon` bodies are already
`#[inline]`, e.g. `byte_class_from_eq_set_64.rs:32`), the *other* seam — plain cross-crate
inlining — behaves by the rustc rule: a `pub fn` that is **neither `#[inline]` nor generic** is
codegen'd only in its home crate's CGU and reached by a real `bl` across the rlib boundary
whenever the consumer is built **without LTO** (cargo default: dev, `cargo test`, and even plain
`--release` are `lto=false`). rustc only serialises `#[inline]`/generic function MIR into crate
metadata for cross-crate inlining; a bare `pub fn` is opaque across the seam. So the boundary
appears precisely under **`lto=false` + a non-`#[inline]` `pub fn` body**. Under parse-that's
`[profile.bench]` (fat-LTO, cgu=1, O3, `Cargo.toml:10`) and `release-lto` (lto=true), LLVM sees
both crates' IR and inlines regardless. Evidence path: PROVE-10/REPORT §2c already disassembled a
fat-LTO consumer and confirmed `memchr::memchr` inlines fully into `Dfa::find` across the crate
seam — the same holds for bbnf-simd's `#[inline]` bodies once the fn-pointer facade is gone. Net:
cgu count / LTO mode is *not* the lever for the facade (indirection defeats all of them); it *is*
the lever for the plain-`pub fn` seam, where the fix is `#[inline]` on the bodies (already present)
so 0-in-loop-indirect holds even at `lto=false`.

## F1(c) — scanner-file mapping census (the ~8 portable_simd files + number_simd)

`1:1-kernel` = the file *is* a kernel mapping to an existing bbnf-simd primitive; `vector-tier` =
a per-domain shell/dispatch over kernels; `no-go` = cannot/should-not share.

| parse-that file (LOC) | class | bbnf-simd target | note |
|---|---|---|---|
| `parsers/scan/structural_bitmap.rs` (282) | **1:1-kernel** | `byte_class_from_table_64`/`classify_tbl4` + `bulk_emit_positions_64` | dual-nibble `classify_16`→64-bit mask is the membership kernel verbatim; cleanest first cutover |
| `parsers/scan/quoted_simd.rs` (189) | **1:1-kernel** | `escape_mask_64`/`bitmap_prefix_xor_64` | `odd_parity_backslashes` ≡ `escape_mask_64` |
| `parsers/scan/quote_parity.rs` (210) | **1:1-kernel** | `bitmap_prefix_xor_64` | pt uses `vmull_p64`/clmul (`:54`), bbnf-simd uses SWAR — reconcile which body |
| `scanners.rs` (370) | **vector-tier** | `byte_class_from_eq_set_64` + `byte_class_from_table_64` (+memchr/aho-corasick) | `find_first_of` count-routed dispatch shell |
| `parsers/scan/ws_comment.rs` (306) | **vector-tier** | `classify_tbl4` + `comment_body_mask_64` | tiered (1-byte/16-byte/64-byte) WS shell |
| `parsers/scan/decode.rs` (405) | **vector-tier** | `string_block` + `unescape_uxxxx` + `escape_mask_64` | arena decode shell; `store_chunk` u8x16 store |
| `span_parser/scan.rs` (184) | **vector-tier** | membership kernels | enum-dispatched span-parser inline scan |
| `state.rs` (492, 2 hits) | **no-go** | — | `PaddedView`/`INPUT_PAD_BYTES` is the *convention* (F1a item 1), not a shareable kernel |
| `parsers/scan/number_simd.rs` (267) | **no-go** | — | **digit-scan SIMD is refuted** (SPEC §0 law iv; §0: "never the digit scan"); retire, don't port. bbnf-simd's `digit_mac.rs` is suspect under the same law |

**Split**: 3 kernel-tier (structural_bitmap, quoted_simd, quote_parity — direct 1:1 to an existing
bbnf-simd primitive), 4 vector-tier (scanners, ws_comment, decode, span_parser/scan — domain
shells that *call* kernels but keep their own control flow), 2 no-go (state.rs convention,
number_simd refuted). Of the 9 SIMD-bearing files, 7 become thin consumers (3 direct + 4
shell-over-kernel) and 2 are excluded — a **7/9 consumer conversion** with the kernel mass fully
in the shared home.

## F1(d) — evidence a prototype must bank (base-pinned)

Static disasm insn + reduction-op counts per movemask/membership site, `otool -tv` of the built
bench binary, base-pinned to skinny `c43126422` / parse-that `ef10d5b`:
- Pre-purge: the five double-`vaddv` sites emit `addv.8b` pairs (baseline reduction census
  ≈ 509 `addv` on the pt combinator binary per VERIFY §2; the refuted shim ballooned to 2271).
- Post-purge (canonical LEAN producer): `addv.8b` → **0** on the swapped sites, replaced by
  `shrn.8b #4` + `fmov` (the CONFIRMED §0-03 idiom); `tbl` count unchanged (516=516, swizzle is
  codegen-equivalent, contributes zero delta per VERIFY §2).
- Paired cold divan ratios naming N + both windows + HEAD: gate is **≥ 0.98× no-regression**
  (the PROVE-09 refutation law — the port is *neutral*, claim nothing more); the double-`vaddv`
  counterexample (2271-vs-509) must not reproduce; checkasm parity must stay green per site.

## Prior art (cited)

- **memchr / aho-corasick / regex-automata** (Andrew Gallant): the canonical one-SIMD-kernel-
  library-many-consumers model; parse-that already consumes `memchr 2.5` + `aho-corasick 1.1`
  (R3 §1) — the shared-kernel dependency shape is already in-tree.
- **simdjson** (Langdale & Lemire, "Parsing Gigabytes of JSON per Second", VLDBJ 2019): stage-1
  structural classification, the movemask + prefix-XOR + escape-mask kernels bbnf-simd homes;
  **sonic-rs** is the Rust re-expression parse-that benches against (R3 §6).
- **Geoffrey Langdale, "universal classifier"** (`branchfree.org`): the dual-nibble-LUT `vqtbl1q`
  shuffle = parse-that `classify_16`'s two-table form; the lo-6-bit single-table specialisation is
  bbnf-simd `classify_tbl4`.
- **Lemire/Mula AArch64 movemask**: `vshrn_n_u16::<4>` narrow (correct, `match_tiny_plain_string.rs:101`)
  vs the `vand`+dual-`vaddv` bit-position spill (the refuted idiom, 5 sites) — the exact F1 contrast.
- **dav1d `checkasm`**: the scalar-reference + asm-parity oracle bbnf-simd's `tests/checkasm_*.rs`
  already adopts — the correctness substrate that makes a *shared* home safe to cut over incrementally.
- **rustc cross-crate inlining** (`#[inline]`/generic-only in crate metadata; LTO for the rest):
  the F1(b) seam rule; **std::arch::aarch64** (stable since Rust 1.61) is what lets the shared home
  be stable-clean while parse-that's `portable_simd`/`smallbox` nightly deps stay orthogonal (F2
  blast radius).

## Named risks & counterexamples

1. **The home ships the refuted idiom (5 sites).** Extract-as-is propagates the double-`vaddv`
   defect into parse-that. Purge is a precondition, gated by the 2271-vs-509 counterexample.
2. **Facade indirection defeats inlining structurally.** The `OnceLock` fn-pointer table
   (`dispatch.rs`) is a `blr` no LTO can remove; must de-face to direct `#[cfg]` dispatch (4 prims
   already do) before any "0-in-loop-indirect" claim.
3. **Padded-view divergence is a correctness seam, not cosmetic.** Whole-buffer over-read (pt) vs
   per-block clamp (bbnf-simd) differ in tail semantics; reconciling one convention needs a
   cross-boundary parity oracle or the shared home silently mis-handles final blocks.
4. **Toolchain skew.** parse-that pins `nightly-2026-04-11` (ICE-avoidance, R3 §1); the shared home
   must build stable AND not re-introduce nightly. The `bbnf-regex 0.1.0` double-home (in both
   `parse-that/rust/regex` and top-level `crates/bbnf-regex`) is the cautionary
   name+version-collision precedent for how a shared crate drifts without a sync gate.
5. **Vendoring vs extraction is a governance choice.** Extraction (patch-table edge, R3 §1
   mechanism) keeps one source; vendoring (copy under a sync gate) risks the bbnf-regex-style
   collision. A checkasm-parity CI running in *both* repos is the mandatory sync gate either way.
6. **digit-scan is refuted** — `number_simd.rs` (and possibly `digit_mac.rs`) must retire under
   SPEC §0 law iv, not migrate.

## What a prototype must test to move 9A

1. Author the two canonical producers (LEAN `vshrn`→`fmov`; DENSE `vpaddq` tree) once in bbnf-simd;
   replace all five double-`vaddv` sites with the shared producer; re-run every `checkasm_*` parity
   oracle (must stay green) + `otool -tv` reduction census (`addv`→0 on LEAN sites, `shrn`+`fmov`
   present, `tbl` unchanged).
2. De-face the fn-pointer facade to direct `#[cfg]` dispatch; disasm a consumer hot loop under
   `lto=false` AND fat-LTO; show 0 in-loop indirect only when de-faced.
3. Rewire one parse-that 1:1-kernel scanner (`structural_bitmap.rs`, cleanest) to consume
   `bbnf-simd::prim::byte_class_from_table_64` + `compact_mask` across the crate seam via the R3 §8
   scratch-copy + patch-table recipe; cold divan paired ratio vs pristine, gate **≥ 0.98×**, naming
   N + both windows + HEAD (`ef10d5b`).
4. Reconcile the padded-view/clamp convention to one strategy serving both membership + dense sites;
   prove it with a cross-boundary parity oracle (pt `PaddedView` result byte-identical to bbnf-simd
   `eob_pad_clamp` result over a boundary-length sweep).
