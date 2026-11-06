# LOCK-9 Family 9A — ONE-KERNEL-LIBRARY (research, pass 1)

*One shared aarch64 kernel home serving both repos; parse-that scanners become thin
consumers; the isomorphism is physical.*

---

## 0. The thesis in one line

There already exist **two aarch64 SIMD kernel worlds** in this dual-repo campaign, and they
re-implement the same simdjson-lineage algorithms twice, on two toolchains, behind two
correctness stories. 9A's mechanism is to collapse them into **one crate** — the mature,
stable-clean, checkasm-gated `bbnf-simd` — extended with a lane-vocabulary tier, and to make
parse-that's eight scanners consume it. The win is not DRY hygiene: **physical kernel-sharing
is the structural mechanism that hands parse-that the `vshrn` movemask and the scalar-parity
gate for free**, which is precisely the fix the refuted `simd128` shim needed and did not get.

## 1. The two kernel worlds today (the substrate to unify)

**World A — `bbnf-simd`** (`skinny/crates/bbnf-simd`, 5084 LOC; workspace member at
`skinny/Cargo.toml:12`). A *primitive* library: grammar-**neutral** whole-kernels over
`&[u8;64]` blocks and `&[u8;16]` lanes, each with a scalar reference that is the executable
spec and a NEON body checkasm-gated against it. Three-tier structure — `aarch64/` (NEON),
`scalar/` (reference oracle), a `prim` facade in `lib.rs` doing a `#[cfg(target_arch =
"aarch64")] return neon; scalar` inline pivot, plus a `dispatch.rs` `OnceLock<PrimitiveKernels>`
fn-pointer table for the runtime-selected subset. Stable-clean (`core::arch::aarch64`, no
`#![feature]`). Kernel inventory (block u64-mask and lane u16-mask): `movemask_u8x16`
(`aarch64/movemask.rs`, **the `vshrn_n_u16::<4>` idiom** — see §5), `byte_class_from_table_64`,
`byte_class_from_eq_set_64`, `structural_classify_64` (simdjson stage-1, caller-supplied
`KindPartition`), `comment_body_mask_64`, `bracket_depth_mask_64`, `bitmap_prefix_xor_64`,
`escape_mask_64` (`lib.rs`), `bulk_emit_positions_64`/`compact_mask`, `string_block.rs`
(quote/backslash/control/non-ascii trio over `&[u8;16]` → `StringSpecialBlock`), `digit_mac.rs`,
`unescape_uxxxx.rs` (NEON TBL `\uXXXX`), `utf8/validate_block.rs`, `match_tiny_plain_string.rs`.
Correctness harness: an FFmpeg-`checkasm.h` port (`tests/checkasm_parity.rs`, `checkasm_common.rs`)
— randomized ref-vs-NEON differential, 1 KiB stack-canary clobber detection, `libc::signal`
SIGSEGV/BUS/ILL guard, `for align in 0..64 × for len in {1,16,32,64,128,1024,8192}` sweep,
outlier filter; blake3 parity hashing (`lib.rs hash_positions`). 18 `checkasm_*` test files.

**World B — parse-that scanners** (`parse-that/rust/parse_that/src`, in the 13,553-LOC crate).
Inline hand-SIMD in the **`std::simd::prelude::*` `u8x16` vocabulary** (`splat`/`from_slice`/
`simd_eq`/`to_bitmask`/`swizzle_dyn`/`& | >>`), ~88 hits across eight files behind
`#![feature(portable_simd)]` on pinned `nightly-2026-04-11`. Files and their algorithms:
`scanners.rs` (22 hits), `parsers/scan/decode.rs` (17, JSON string decode-to-arena incl.
`store_chunk` u8x16 unaligned store, `\uXXXX` via `decode_one_escape`), `parsers/scan/ws_comment.rs`
(16), `parsers/scan/structural_bitmap.rs` (13, simdjson structural index), `parsers/scan/quoted_simd.rs`
(7, `odd_parity_backslashes`), `span_parser/scan.rs` (7), `parsers/scan/quote_parity.rs`
(`prefix_xor` pclmulqdq/`vmull_p64`/scalar at `:41`, `escape_mask_64` `:89`,
`compute_in_string_bitmap` `:121`, `classify_stripe_64` u8x16 `:71`), `parsers/scan/number_simd.rs`
(NEON+SSE4.2 digit accumulate). Feeds a `PaddedView` invariant (`state.rs:57`
`INPUT_PAD_BYTES=64`, trailing NUL so stripe loads skip the tail guard).

**The duplication is exact, not analogous.** parse-that `quote_parity.rs::prefix_xor` ≡
bbnf-simd `bitmap_prefix_xor_64`. parse-that `escape_mask_64` ≡ bbnf-simd `escape_mask_64`
(same even/odd carry-propagation over backslash runs). parse-that `quoted_simd.rs::odd_parity_backslashes`
+ `classify_stripe_64` ≡ bbnf-simd `string_block.rs::scan_string_special_block` (both emit a
quote/escape/control mask over a 16-byte lane). parse-that `structural_bitmap.rs` ≡
`structural_classify_64` + `compact_mask`/`bulk_emit_positions_64`. parse-that `decode.rs`
`\uXXXX` ≡ `unescape_uxxxx.rs`. Two implementations, two toolchains, one parity story missing
on the parse-that side (its scanners have no scalar-reference differential gate — only
integration tests). 9A supplies the missing gate by construction.

## 2. The mechanism — one crate, two consumer tiers, physical seam

**One home.** `bbnf-simd` is the single kernel crate. Per the frozen codex, skinny is retired
as a tree and `bbnf-simd` **ports into the bbnf-lang greenfield workspace** (SPEC §1; GN-1
"Port-in: bbnf-simd with the 01+03 patches"). So the one kernel home post-campaign is the
greenfield `bbnf-simd` crate. It grows one tier and gains parse-that-origin kernels; nothing
about its checkasm/scalar-reference philosophy changes.

**The added tier — `vector/`.** bbnf-simd today exposes whole-kernels and `movemask_u8x16`,
but *not* a composable `u8x16` arithmetic wrapper type; each kernel drops to raw
`core::arch::aarch64` internally. parse-that's compositional scanners (those that do not map
1:1 to a whole-kernel) need exactly that vocabulary. So bbnf-simd gains `src/vector/`
{`mod.rs` cfg-pivot, `neon.rs`, `scalar.rs`} exposing the `u8x16`/`mask8x16` newtype
vocabulary — `splat`/`from_slice`/`simd_eq`/`simd_ne`/`& | >>`/`swizzle_dyn`/`any`/`all`, and
`to_bitmask` **delegating to the existing `movemask_u8x16` `vshrn` reduction**. This is the
promotion of the refuted-shim's 276-LOC `simd128.rs` into the shared crate, minus its
regressing double-`vaddv` movemask (replaced by bbnf-simd's `vshrn`), plus its `[u8;16]`
scalar oracle as the `vector/scalar.rs` reference. The whole-kernels may then be refactored
to dogfood `vector/` (optional), and parse-that's compositional scanners import it directly.

**Two consumer tiers for parse-that:**
- *Kernel tier (the strong physical iso).* Where a parse-that scan maps to an existing
  bbnf-simd whole-kernel, the hand copy is **deleted** and the kernel consumed: `prefix_xor`→
  `prim::bitmap_prefix_xor_64`, `escape_mask_64`→`escape_mask_64`, `classify_stripe_64`+
  `odd_parity_backslashes`→`string_block::scan_string_special_block`, `structural_bitmap`→
  `structural_classify_64`+`compact_mask`, decode `\uXXXX`→`unescape_uxxxx`.
- *Vector tier (the escape hatch).* Where a scan has no kernel equivalent (`decode.rs
  store_chunk`, `ws_comment.rs`, `scanners.rs`, `span_parser/scan.rs`), rewrite the `use
  std::simd::prelude::*` line to `use bbnf_simd::vector::*` — one-line swaps, exactly the
  PROVE-09 shim ergonomics, but the vocabulary is now the shared crate, not a parse-that-local
  file, so both repos share the movemask and the scalar oracle.

**The physical seam — path-dep, not vendor (primary).** parse-that already carries a
seven-entry `[patch.crates-io]` table in the gitignored `rust/.cargo/config.toml`
(`bbnf`→`../../bbnf-lang/crates/core`, `egraph`, `egraph-derive`, `csp-solver`, `pprint`, …).
Add one line: `bbnf-simd → ../../bbnf-lang/crates/bbnf-simd`. parse-that consumes the greenfield
crate as a path-dep exactly as it already consumes `bbnf`/`egraph`/`csp-solver` — no copy, no
sync gate, drift impossible because there is one source. This is the **memchr model**:
`BurntSushi/memchr` is one aarch64 byte-search kernel crate consumed by `regex-automata`,
`aho-corasick`, and `ripgrep`; the 9A shape is identical. It is also the FFmpeg/dav1d model at
the correctness layer: one scalar reference is the spec, N NEON bodies are checkasm-parity
bodies, all consumers share the gate.

**The vendor fallback (secondary).** If parse-that must build standalone without the
bbnf-lang sibling present, vendor `bbnf-simd/src/` into parse-that under a **canonical-diff
sync gate** — the exact discipline already in force for `csp-solver` (re-vendored from the
csc411 sibling under a tracked canonical-diff gate; `crates/csp-solver` is the landed vendor).
Cost: the sync gate must re-run on every kernel change or the copies drift — a recurring chore
the csc411 re-vendor cadence demonstrates. **Recommendation: path-dep primary** (zero
duplication, the memchr archetype); **vendor+sync-gate only if a standalone parse-that build
is a hard requirement.** The charter's "extracted OR vendored" is answered: extracted-and-
path-depended is the strong form; vendored-under-gate is the compatibility form.

## 3. Integration seams (path:line where extant code serves)

Deletion/rewrite sites in parse-that (the thin-consumer conversions):
- `parse-that/rust/parse_that/src/parsers/scan/quote_parity.rs:41` `prefix_xor` → `bbnf_simd::prefix_xor_64` / `prim::bitmap_prefix_xor_64`; also excises the x86 `pclmulqdq` arm (`:42-49`) and the `vmull_p64` arm (`:51-56`), which the shared kernel subsumes.
- `.../quote_parity.rs:89` `escape_mask_64` → `bbnf_simd::escape_mask_64` (verify even/odd-carry equivalence in a parity test before deleting).
- `.../quote_parity.rs:71` `classify_stripe_64` + `.../quoted_simd.rs` `odd_parity_backslashes` → `bbnf_simd::aarch64::string_block::scan_string_special_block` (both 16-byte-lane quote/escape/control classifiers).
- `.../structural_bitmap.rs` → `bbnf_simd::prim::structural_classify_64` + `compact_mask` + `bulk_emit_positions_64` (`lib.rs`).
- `.../decode.rs:296` `decode_one_escape` `\uXXXX` → `bbnf_simd::aarch64::unescape_uxxxx`.
- `.../decode.rs`, `ws_comment.rs`, `scanners.rs`, `span_parser/scan.rs`, `state.rs` — `use std::simd::prelude::*` → `use bbnf_simd::vector::*` (vector-tier, one-line swaps).

Provider sites in bbnf-simd (what serves, and what is added):
- `skinny/crates/bbnf-simd/src/aarch64/movemask.rs` — the `vshrn` movemask every parse-that `to_bitmask` reroutes onto (the refutation-compliant reduction).
- `.../src/lib.rs prim` module — the direct-`cfg` inline facade (NOT the `dispatch.rs` `OnceLock` fn-pointer table — see risk 2).
- `.../src/aarch64/{string_block,unescape_uxxxx,structural_classify_64}.rs`, `.../src/lib.rs escape_mask_64`/`prefix_xor_64` — the 1:1 kernels.
- ADD `.../src/vector/{mod,neon,scalar}.rs` (the promoted shim tier).
- EXTEND `.../tests/checkasm_*.rs` with scalar references for the parse-that-origin surface (prefix_xor and escape_mask already have bbnf-simd scalar bodies; the in-string-bitmap composition needs one).

Homing: `bbnf-simd` moves under GN-1's greenfield port-in; the path-dep target becomes
`crates/bbnf-simd` in the greenfield tree; the parse-that `.cargo/config.toml` line points
there. G3's existing `bbnf → crates/core` repoint precedent shows the mechanics.

## 4. Prior art with citations

- **memchr (BurntSushi)** — the archetype: one aarch64 byte-search kernel crate, many
  consumers (regex-automata, aho-corasick, ripgrep). Justifies path-dep over vendor; the exact
  9A "physical isomorphism = one crate, thin consumers" shape. PROVE-10 already pulls memchr's
  NEON `vshrn` byte-search into the regex prefilter tier — the same kernel crate already
  serves the regex side.
- **FFmpeg `checkasm.h` / dav1d / x264** — the "one scalar reference is the spec, N SIMD
  bodies are parity-gated, randomized differential + stack-canary + signal-guard + alignment
  sweep" discipline. bbnf-simd's `tests/checkasm_parity.rs` is a direct port (its report maps
  each checkasm.h feature to a Rust construct). This IS the correctness substrate that makes
  cross-repo kernel-sharing safe — parse-that inherits the gate it currently lacks.
- **simdjson ARM64 port** — `neon_movemask` via `vshrn_n_u16` (the "SSE `_mm_movemask_epi8`
  has no NEON equivalent; narrow-shift is the idiom" solution); `structural_classify` stage-1
  via `vqtbl` classification; prefix-XOR quote-parity and odd-backslash escape masks. bbnf-simd
  reproduces these; parse-that duplicates them. Citation anchors the algorithm equivalence.
- **Validark "Adventures with simdjson" (2021); Lemire "Parsing short hex strings with SIMD"
  (2022); Sneller "Unicode escapes in JSON without branches"** — the TBL-nibble `\uXXXX` decode
  (three NEON µops/nibble) cited inline in bbnf-simd's `unescape_uxxxx.rs`; the shared kernel's
  provenance for the decode.rs mapping.
- **Rust `std::simd` (portable_simd) vs `core::arch::aarch64`; `wide`/`simdeez` stable-SIMD
  crates** — the generic-abstraction alternative to a domain kernel crate. 9A's stance: reject
  the generic SIMD-abstraction dependency; the domain crate (bbnf-simd) already carries the
  parity gate AND the correct movemask, which a generic wrapper does not.
- **csp-solver canonical-diff vendor gate (in-tree, `crates/csp-solver` re-vendored from the
  csc411 sibling)** — the repo's own precedent for the vendor-under-sync-gate fallback.

## 5. The refutation law and why 9A satisfies it structurally

The binding law (SPEC §0/09, PROVE-09 VERIFY): the `portable_simd→NEON` de-nightly shim's
1.34–1.57× headline **did not reproduce** (paired windows parity-to-slower); the mechanism was
disproven at the instruction level — the shim's `to_bitmask` emitted **2271 `addv.8b` vs the
baseline 509** (it did ~4× the reduction work per movemask via a bit-position `vand` + two
`vaddv_u8` halves). Standing lesson: *stable NEON must be measured no-regression; `vshrn`
narrow, never the double-`vaddv` movemask; two-arm window asymmetry is the classic self-
deception, paired interleaving is mandatory.*

9A satisfies this **by construction, not by discipline**: bbnf-simd's `movemask.rs` already
uses `vshrn_n_u16::<4>` (SPEC §0/03 CONFIRMED — "vshrn obviates hand-asm for the mask-narrow
idiom, +35% over scalar", won in both interleave slots). Routing every parse-that `to_bitmask`
through the shared kernel hands parse-that the correct reduction automatically — 9A *contains*
the PT-2 W1 movemask fix as a corollary of sharing, rather than re-deriving it. The no-
regression gate (≥0.98× per corpus, both windows, N≥200, `otool -tv` reduction-count ≤ baseline
509) is inherited verbatim from PT-2 W1 / PROVE-09 VERIFY attack #2 and is the close condition.

## 6. Named risks and counterexamples

1. **Cross-crate inlining (PRIMARY, make-or-break).** parse-that's hot inner scan currently
   inlines `std::simd`. If a shared primitive is reached through bbnf-simd's `dispatch.rs`
   `OnceLock<PrimitiveKernels>` **fn-pointer table**, the hot loop takes an *indirect call* per
   block — a near-certain regression that fails the no-regression law. Mitigation: parse-that
   MUST consume the **direct-`cfg` `prim` facade** (`byte_class_from_eq_set_64`,
   `structural_classify_64` already dispatch via inline `#[cfg] return neon`), never the
   fn-pointer table, and the crate must build fat-LTO so `#[inline]` crosses the path-dep
   boundary. Prototype must `otool -tv` the wired scanner and prove the kernel inlined (no
   `bl`/`blr` to the primitive in the block loop).
2. **Abstraction-level mismatch.** bbnf-simd kernels are 64-byte-block whole-scans; several
   parse-that scans are 16-byte-lane compositions. Smaller than feared — bbnf-simd already
   spans both granularities (`string_block.rs` is a `&[u8;16]` kernel; `movemask_u8x16` is
   16-byte). But scanners with no kernel match fall to the vector tier, so the *physical
   isomorphism is lane-level, not kernel-level* for those — weaker than the charter ideal.
   The prototype's census (§7.1) quantifies the kernel-tier vs vector-tier split.
3. **Padding-contract seam.** parse-that assumes `PaddedView`/`INPUT_PAD_BYTES=64`
   (`state.rs:57`) trailing NUL so stripe loads skip tail guards; bbnf-simd has its own
   `eob_pad_clamp` tail handling (`prim::eob_pad_clamp`). The shared kernels must accept
   parse-that's padding guarantee or parse-that must feed `eob_pad_clamp` — risk of double-
   padding or a contract mismatch at the tail block.
4. **Digit-scan is NO_GO (SPEC §0/02).** `number_simd.rs` ↔ bbnf-simd `digit_mac.rs` is a
   tempting mapping but **forbidden as a perf pursuit**: the Mula/Lemire in-vector digit
   accumulate is latency-bound (CPI 0.13→0.30) and regresses the product lane. The port keeps
   number scanning behaviour-preserving (scalar or its existing NEON), excises only the x86
   SSE4.2 arm; it does not adopt `digit_mac` as an optimization. Named counterexample.
5. **Grammar-neutrality must hold.** Sharing works because bbnf-simd kernels are grammar-
   neutral (caller-supplied `KindPartition`/table/set) — JSON and CSS structural chars are both
   just caller partitions. Any parse-that scan needing a grammar-baked constant that cannot be
   lifted to a parameter cannot consume the kernel. Prototype must confirm each candidate's
   grammar-specificity is parameterizable.
6. **Toolchain skew (benign but watched).** bbnf-simd is stable-clean; consuming it moves
   parse-that's SIMD surface off nightly (aligns with the de-nightly direction). The shared
   crate must stay `#![feature]`-free so it never re-imports a nightly dep transitively.

## 7. What a prototype must test to move 9A

1. **The mapping census.** For each of the eight parse-that SIMD scanner files, classify:
   1:1 to an existing bbnf-simd whole-kernel (name it) | needs a new kernel | vector-tier only
   | NO_GO (number_simd). Deliverable: the table. Moves the abstraction-mismatch risk from
   speculation to measured fact.
2. **The cross-crate inline + no-regression proof (the gate).** Wire ONE hot scanner —
   best first candidate `structural_bitmap.rs` → `structural_classify_64`, second
   `quote_parity.rs::prefix_xor` → `bitmap_prefix_xor_64` — to consume bbnf-simd as a
   `[patch]` path-dep, build fat-LTO, `otool -tv` to confirm the kernel **inlines** (no
   indirect call), and run the PROVE-09 paired-window divan protocol (N≥200, two windows,
   opposite arm order, per JSON corpus) proving **≥0.98× no-regression** on every corpus in
   both windows. This is the make-or-break: the physical iso is only real if measured-neutral.
3. **The movemask parity.** `otool -tv` the rerouted `to_bitmask` sites; confirm `vshrn`
   (not double-`vaddv`); reduction-instruction count **≤ 509 baseline** (PROVE-09 VERIFY
   attack #2 gate, reproduced).
4. **The checkasm extension.** Add scalar references for the shared parse-that-origin surface
   (in-string-bitmap composition; prefix_xor/escape_mask already have bbnf-simd scalar bodies)
   to `checkasm_parity.rs`; green differential run over the union proves the parity gate covers
   the merged surface — the correctness dividend parse-that gains from sharing.
5. **Path-dep vs vendor decision artifact.** Build parse-that against bbnf-simd as (a) a
   `[patch]` path-dep and (b) a vendored copy under a canonical-diff gate; record the
   maintenance/build cost of each; certify the recommendation (path-dep primary).
