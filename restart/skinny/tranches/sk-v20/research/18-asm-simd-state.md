# 18 — ASM/SIMD State: Census and Gap Analysis

Audit scope: every NEON/aarch64 intrinsic site in both trees. HEAD 36ca10646.
aarch64/Apple-M only (precept 4); x86 and wasm kernels assessed as scope leakage.

## 1. Kernel census

Six structural/scan NEON kernels exist, across two trees that do not share a
crate. Listed with input/output contract, dispatch site, and provenance.

### Full tree — `crates/simd-scan`

| Kernel | File:line | Algorithm | Dispatch |
|---|---|---|---|
| `scan` (shape-dispatched) | `neon.rs:47` | nibble-LUT (`vqtbl1q_u8` lo/hi AND), wide-LUT (4×`vqtbl1q_u8`), or `vceqq` multi-cmp, selected by `KernelShape::select` | `lib.rs:90` from `scan_structural` |
| `classify_chunk_nibble` | `neon.rs:208` | low/high-nibble `vqtbl1q_u8` against `(lo,hi)` per-bit LUTs, `vandq`, `vtstq`, movemask | inline |
| digraph stripe | `neon.rs:456` | shifted `vld1q_u8(ptr+1)` + paired `vceqq` + `vandq` | inline, gated on `digraph_pairs` |
| quote parity | `neon.rs:539` | per-class `vceqq` → `escape_mask_64` → `prefix_xor_64` (PMULL or shift-XOR) | inline, gated on `quote_classes` |

Contract: `(input, &StructuralAlphabet) -> StructuralIndex` (dense positions +
kinds). Handwritten. Alphabet is `&'static` caller data (`alphabet.rs:19`), the
shape is picked by cardinality (`alphabet.rs:118`) — this is the one genuinely
Lock-14-clean kernel: zero grammar-name branches, data-driven lowering.

### Skinny tree — `bbnf-simd` + runtime

| Kernel | File:line | Algorithm | Dispatch |
|---|---|---|---|
| `classify_tbl4` (lo6-LUT) | `aarch64/classify_tbl4.rs:22` | `vqtbl4q_u8` over 64-entry low-6-bit table, `vceqq(class,chunk)` AND `vcgtq(class,0)` | `dispatch.rs:23` (`NeonTbl4`) |
| `structural_classify_64` (vceqq fan) | `aarch64/structural_classify_64.rs:98` | per-kind `vceqq`+`vorrq` fan into open/close/quote/slash/stop masks; significant = OR of five | `prim::structural_classify_64`, `lib.rs:301` |
| `byte_class_from_eq_set_64` | `aarch64/byte_class_from_eq_set_64.rs:33` | `vceqq`+`vorrq` set-membership fan (≤8) | `prim`, `lib.rs:283` |
| `bracket_depth_mask_64` | `aarch64/bracket_depth_mask_64.rs` | scalar running-balance (NOT CTZ; NEON only classifies open/close) | `prim`, `lib.rs:336` |
| `comment_body_mask_64` | `aarch64/comment_body_mask_64.rs` | `vceqq` comment-delim classify + carry | `prim`, `lib.rs:316` |
| JSON `neon::scan` | `runtime/.../json/scan.rs:213` | lo6-LUT classify + `escape_mask_64` + `prefix_xor_64` string masking | **dead on live path** |

Contracts: `structural_classify_64` is `(&[u8;64], &KindPartition) ->
StructuralClassify64`; `KindPartition` is caller data (`runtime_scan.rs:76`).
All handwritten with a scalar reference as the parity anchor (checkasm gate).

The skinny `simd-scan` crate is an empty husk: `skinny/crates/simd-scan/src/`
contains no `.rs` files — a directory with nothing in it.

## 2. Finding A — three competing structural schemes, none unified

The campaign carries **three distinct structural-classification schemes** for
the same problem (classify a block into structural-byte masks), each with its
own justification doc, none reconciled:

1. **lo6-LUT** (`vqtbl4q_u8`, 64-entry low-6-bit table) — `classify_tbl4.rs`,
   and the JSON `STRUCTURAL_CLASS_TABLE_LO6` (`json/scan.rs:22`). Admissible
   only when the alphabet has no low-6-bit collisions (`dispatch.rs:101`).
2. **vceqq fan** — `structural_classify_64.rs`, chosen for CSS precisely
   *because* its 11-byte set `' " / ( ) [ ] { } ; :` HAS lo-6 collisions
   (`;`=0x3B ≡ `{`=0x3B, documented at `structural_classify_64.rs:13-16`).
3. **nibble lo/hi two-LUT** (`vqtbl1q_u8` × lo + hi) — the full tree's
   `neon.rs` NibbleLut path (`neon.rs:219`).

These are three answers to one question. The skinny `structural_classify_64.rs`
header (line 16-21) admits the simdjson lo×hi two-LUT AND-fold is "the upgrade
path" but "is NOT built here" — while the full tree's `neon.rs` already built
exactly that lo×hi nibble fold. The two trees independently solved the same
kernel, each unaware of the other's solution. This is the precept-5
duplication failure at crate granularity: two `simd-scan`-shaped crates (one
empty), plus a third lo6 table inlined into the JSON runtime.

## 3. Finding B — seven `movemask_u8x16` copies, three algorithms, lying docs

`rg "fn movemask_u8x16"` returns seven+ definitions: `neon.rs:690`,
`parse-that-regex/src/lib.rs:847`, `bracket_depth_mask_64.rs:74`,
`movemask.rs:4`, `match_tiny_plain_string.rs:100`,
`structural_classify_64.rs:147`, `comment_body_mask_64.rs:68`,
`byte_class_from_eq_set_64.rs:79`.

Three different algorithms:

- **`vand`+`vaddv` weighted-sum** (the slow one): `neon.rs:706`,
  `structural_classify_64.rs:151`, `byte_class_from_eq_set_64.rs:83`. Two
  horizontal `vaddv_u8` reductions per 16 lanes — `vaddv` is a serializing
  cross-lane reduction, the slowest movemask on Apple-M.
- **`vshrn_n_u16::<4>` narrow** (the fast, canonical one): `movemask.rs:5`,
  `match_tiny_plain_string.rs:101`, and the generated `first_quote_or_backslash`
  (`support.rs:665`). One `shrn` + a `u64` read.
- **elaborate `vsri`/`vzip` variant**: `movemask.rs` then mixes a scalar `for
  pair in 0..8` byte loop into the result — slower than the bare shrn, defeating
  its own purpose.

The doc comments are actively dishonest. `neon.rs:14`, `:683`, `:696` all
advertise "vshrn_n_u16 #4 movemask"; the actual body (`neon.rs:690-708`) is the
`vand`+`vaddv` weighted-sum. `byte_class_from_eq_set_64.rs:60-65` claims to
mirror "Lemire + Mula … AArch64 movemask spill" — but `vaddv` is not the
Lemire/Mula shrn trick. There is a designated home (`movemask.rs`) that six
copies ignore; the two hottest structural kernels use the slow form.

## 4. Finding C — `byte_class_from_table_64_neon` is a scalar stub on the live path

`aarch64/byte_class_from_table_64.rs` is four lines:

```rust
pub fn byte_class_from_table_64_neon(src: &[u8; 64], table: &[u8; 256]) -> u64 {
    crate::scalar::byte_class_from_table_64::byte_class_from_table_64_scalar(src, table)
}
```

It is wired into the live `PrimitiveKernels` aarch64 table at `dispatch.rs:67`
and exposed through `prim::byte_class_from_table_64` (`lib.rs:256`). This is a
stub masquerading as a NEON body — precept 5 ("NO stubs"). A 256-entry table
lookup over 64 bytes is a `vqtbl4q_u8`×4 problem already solved in
`classify_tbl4.rs`; the "NEON" entry just calls scalar and pretends.

## 5. Finding D — the most advanced kernel is dead on the live JSON path

The skinny JSON `neon::scan` (`json/scan.rs:213`) is the only kernel that fuses
lo6-LUT classification with the full simdjson string-state machinery:
`escape_mask_64` (odd-backslash carry, `lib.rs:174`), `prefix_xor_64` (PMULL
parity), and a fast-path/strict-resolve split for escaped quotes
(`json/scan.rs:240-270`). It is the campaign's best structural scanner.

It is **dead on the live path**. The file header (`json/scan.rs:2-13`) states
the live `CapacityPlan` is `GrowOnly`, which `structural_capacity_for` answers
with a constant `256` (`json/scan.rs:64`), touching none of the scanners.
`scan_structurals` + `neon::scan` are reachable only under
`BBNF_TAPE_CAPACITY=A|B|C` and the bench/parity harnesses. So the headline
"JSON beats sonic-rs" claim is delivered by the scalar tape walk, NOT by this
NEON stage-1 — yet the SIMD investment sits unused behind an env var. Either the
claim does not need SIMD (then delete the scanner) or it does (then wire it).
The current state is dead weight presented as if active.

## 6. Finding E — the full-tree `scan_structural` is live for at most one grammar

The CTNS probe substrate (`OnceCell<StructuralIndex>` + `ensure_structural_index`
→ `simd_scan::scan_structural`) is emitted into all 8 generated grammars (site
counts: json 2, css_l4 2, sheets 3, etc.). But the probe that *consumes* the
index is gated by `ctns_probe_admits` (`support.rs:70-85`), whose comment
(`support.rs:62-69`) states it admits **Sheets only** (19 bytes, no whitespace),
and explicitly excludes JSON ("too sparse to beat bitmap"), BBNF, and CSS L4
("over-broad mining"). So the entire full-tree `crates/simd-scan` NEON
machinery — the cleanest, most generalized kernel in the campaign — has a single
live consumer: Sheets whitespace-run skipping. Every other grammar pays the
OnceCell field + substrate emission for nothing. A large, well-engineered,
nearly-dead subsystem.

## 7. Finding F — orphan admission bodies and x86 scope leakage

Orphan NEON kernels with **no live caller** (only checkasm tests / bench
manifest strings): `byte_context.rs`, `cache_hints.rs`, `quad_load.rs`,
`digit_mac.rs`. Speculative admission bodies kept alive by their parity tests
alone — dead code under precept 5. (`match_tiny_plain_string`, `string_block`,
`unescape_uxxxx` DO have live callers in the JSON-template codegen and
parse-that-regex.)

x86/wasm out-of-scope leakage (precept 4, aarch64-only): the full tree carries
`avx2.rs` (370), `avx512.rs` (149), `wasm.rs` (433) = 952 lines, plus AVX2 paths
in the generated `support.rs` (`nospace_bitmap_64_avx2`,
`first_quote_or_backslash_avx2`). The `parity.rs` and `kernel_shape.rs` docs are
threaded with x86 framing (`vpshufb`, `PCLMULQDQ`, `avx512vbmi2`). For an
aarch64-Apple-M-only campaign this is ~1000 lines of dead, CI-untested
intrinsics carrying maintenance and doc-confusion cost.

## 8. Finding G — `escape_mask_64`/`prefix_xor_64` reimplemented twice

The simdjson carry tricks exist in BOTH trees independently: full-tree
`parity.rs:71` (`escape_mask_64`) and `:40`/`:175` (`prefix_xor_64` with PMULL),
and skinny `bbnf-simd/src/lib.rs:174`/`:169`. Same algorithm (Langdale & Lemire
§4.2 escape carry, shift-XOR or PMULL prefix), two maintained copies, neither
aware of the other. The shift-XOR ladder, the PMULL `vmull_p64(mask, !0)` trick,
and the odd-backslash run partition are duplicated. One must be canonical.

## 9. Gap vs simdjson aarch64 stage-1

simdjson's NEON stage-1 (Langdale & Lemire) does: (a) one lo×hi nibble two-LUT
`vqtbl1q_u8` AND-fold classifying ALL byte classes in ONE pass over a 64-byte
block; (b) `vshrn_n_u16` movemask (never `vaddv`); (c) escape carry + prefix-XOR
string masking; (d) classifies each block EXACTLY ONCE, threading carries.

Adopted: nibble two-LUT (full tree only), escape/prefix tricks (both, duplicated).
**Unadopted or regressed:**

- **Single-pass classification.** The G6 `css_balanced_component_scan` re-runs
  the full `vceqq` fan from *each new `pos`* (`runtime_scan.rs:150-155`) every
  time it jumps past a string/recursion — re-classifying overlapping bytes.
  simdjson classifies each block once and carries depth/quote state. The
  re-derive-on-jump is a real O(re-scan) regression the doc itself flags
  (`runtime_scan.rs:147-149`).
- **`vshrn` movemask** is unadopted in the two hottest kernels (Finding B).
- **The lo×hi two-LUT** is unadopted in skinny (Finding A) despite being the
  documented "upgrade path" and already existing in the full tree.

## 10. Unification verdict and ranked next-kernel candidates

**Unify into the lo×hi nibble two-LUT scheme.** The full tree's `neon.rs`
NibbleLut/WideLut already implements it correctly and grammar-neutrally. The
skinny `vceqq` fan is justified only by the CSS lo-6 *collision* — but the lo×hi
nibble fold does NOT collide on that set (lo-nibble × hi-nibble distinguishes
`;`=0x3B from `{`=0x7B: hi-nibbles 3 vs 7). The vceqq fan should be retired for
the two-LUT, which scales to ≤16 bytes at constant 4 `vqtbl1q_u8`/lane vs the
fan's `set_len` compares. lo6-LUT (`classify_tbl4`) is a special case the
two-LUT subsumes. One scheme, one crate, one `movemask` (the `vshrn` form), one
`parity` module.

Ranked next-kernel candidates by expected Apple-M win (128-bit NEON ×4 pipes,
no SVE — width comes from 4-way ILP across the 64-byte block, not vector length):

1. **Replace all `vaddv` movemasks with `vshrn_n_u16::<4>`** (Finding B). Lowest
   effort, highest certainty: `vaddv` is a serializing reduction that stalls the
   M-series; `shrn`+`u64`-read is one cheap op feeding the 4 NEON pipes. On the
   live CSS `structural_classify_64` path.
2. **Single-pass block classification for the CSS G6 scan** (§9). Classify each
   64-byte block once, carry bracket-depth + quote-state across blocks (reuse
   `bracket_depth_mask_64` + `escape_mask_64`), stop re-classifying from each
   jump. Removes the documented re-scan; directly attacks the 94% scalar leaf.
3. **Wire the JSON `neon::scan` onto the live path** (Finding D) OR delete it.
   If the JSON-beats-sonic claim wants stage-1, the kernel exists and is good —
   it is gated off by `GrowOnly`. Decide and commit.
4. **Collapse the three structural schemes to the lo×hi two-LUT** (§10) and the
   seven movemasks / two parity modules to one each. Deletes ~1500 lines
   (orphan bodies + x86 leakage + duplicate parity) under precepts 4/5.
