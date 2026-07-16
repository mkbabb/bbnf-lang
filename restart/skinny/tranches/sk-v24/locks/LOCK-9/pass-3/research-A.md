# LOCK-9 · Family 9A — ONE-KERNEL-LIBRARY (research-A, pass-3)

**Thesis.** One aarch64 kernel home — `skinny/crates/bbnf-simd` — authors the contract-split
mask producers *once*, parity-gates them with a dav1d-style checkasm oracle *once*, and both
repos consume them; parse-that's scanners become thin call-sites over that home rather than
re-authoring a private `simd/neon.rs`. The physical isomorphism (constitution iii) is a shared
`.rlib` and a shared scalar reference, not two hand-mirrored NEON files that drift. This report
develops that route to its strongest form and reports the substrate, the divergence deltas, the
seam census, and exactly what a prototype must prove.

---

## 1. The substrate — bbnf-simd is a real, stable, checkasm-gated kernel library

`skinny/crates/bbnf-simd` (Cargo.toml: `edition.workspace`, sole dep `blake3`, **no nightly**,
pure `core::arch::aarch64`). Its shape is exactly the shared-home shape 9A wants, already built:

- **Runtime dispatch table** — `dispatch.rs:50` `PrimitiveKernels { byte_class_from_table_64,
  bitmap_prefix_xor_64, bitmap_next_set_bit, bulk_emit_positions_64, eob_pad_clamp }` as
  `fn`-pointers, selected once through `OnceLock` (`dispatch.rs:58` `primitive_kernels()`), NEON
  arm at `:63-75`, scalar arm at `:77-87`. A second, `cfg`-direct dispatch path exists for the
  richer producers (`lib.rs:283` `byte_class_from_eq_set_64`, `:301` `structural_classify_64`,
  `:316` `comment_body_mask_64`, `:336` `bracket_depth_mask_64`) that inline through
  `#[cfg(target_arch="aarch64")]` return-then-fallthrough, no fn-pointer.
- **Scalar reference per producer** — every `aarch64/X.rs` has a `scalar/X.rs` twin; `lib.rs:246`
  states the law verbatim: "the scalar reference is the executable specification per the checkasm
  admission gate; the NEON body is parity-checked against it."
- **dav1d checkasm oracle** — `tests/checkasm_common.rs`: stack-canary XOR-fold clobber detector
  (`:50` `with_stack_canary_xor_fold`) + AArch64 callee-saved register sentinel check
  (`:85` `callee_saved_register_then`, reads/writes x19–x28 via `core::arch::asm!`). Plus
  `tests/corpus_parity.rs`, `tests/classifier_parity.rs`, `tests/aarch64_primitives.rs`, and a
  per-primitive `checkasm_*` file (17 of them). This is the differential oracle F2 asks for,
  already homed here.
- **The classifier front** — `classifier.rs`/`dispatch.rs:42` `select_classifier` + a `tbl4`
  lo6-nibble NEON classifier (`aarch64/classify_tbl4.rs`, 104 L) with an admissibility gate
  (`dispatch.rs:101` `lo6_table_admissible`).

**Consequence for 9A:** the shared home is not hypothetical. The work is *finishing* it (§3) and
adding a consumer edge, not standing it up.

---

## 2. F1a — residual divergence, one-home vs two-home, enumerated

Both directions are *buildable*; the question is residual LOC + drift cost. The two-home baseline
is PT-2 W1's plan: a fresh `parse_that/src/simd/{mod,neon,scalar}.rs` (~380 L) that re-authors
the u8x16 vocabulary, the movemask, and a `simd_test.rs` 10k-vector oracle — all of which
bbnf-simd already ships. Concrete deltas that must be reconciled in EITHER layout, and that a
one-home layout collapses to a single definition:

| Convention | parse-that today | bbnf-simd today | one-home resolution |
|---|---|---|---|
| **Tail/clamp** | `state.rs:57` `INPUT_PAD_BYTES=64` trailing-NUL `PaddedView` (`:122`); stripe loads skip the tail guard | `prim::eob_pad_clamp`→`EobBlock` materialises a padded 64-block on demand (`lib.rs:276`) | one clamp contract; the padded-view guarantee is the caller's, `eob_pad_clamp` is the fallback for un-padded input — pick one, document at the seam |
| **Shuffle strategy** | `swizzle_dyn` == single `vqtbl1q_u8` (one 16-entry table) | `classify_tbl4` == `vqtbl4q` over a 64-entry lo6 table (`aarch64/classify_tbl4.rs`) | multi-table `tbl4` subsumes single-table; single-`tbl1` stays for the ≤16-symbol case, selected by `lo6_table_admissible` |
| **Block capacity** | `STRIPE=64` (`quote_parity.rs:34`), 64-B stripe loads | 64-B block everywhere; `positions` cap `input.len()/8+8` (`lib.rs:107`) | aligned already — zero delta |
| **Movemask** | needs u16-per-16 + a nibble-stride *iterate* (`tz>>2`) for the ~5 mask-iterating scanners | placeholder hybrid (§3) | ONE `vshrn` LEAN producer + ONE dense-64 producer, authored once |
| **Number/digit** | `number_simd.rs` NEON `vaddv` digit MAC | `aarch64/digit_mac.rs` (71 L) | **no-go — do NOT share** (SPEC §0/02); each keeps its own, refutation law |

**Drift-cost basis.** The load-bearing divergence is not LOC — it is the *reduction idiom* (§3).
In a two-home layout the canonical LEAN/DENSE producers are authored twice; a single hand-edit on
one side that reverts to the double-`vaddv` movemask re-introduces the refuted idiom on that side
only, and no cross-repo test catches it. In a one-home layout the producer is one definition
behind one checkasm gate; the refutation is enforced by artifact for both consumers. That is the
maintenance-cost argument for 9A, and it is measurable (a single `rg 'vaddv_u8'` census over the
shared crate is the whole audit surface, vs two).

**Residual LOC if one-home:** parse-that deletes PT-2 W1's `simd/neon.rs` (~210) + `simd/scalar.rs`
(~130) + `simd_test.rs` (~replaced by bbnf-simd's checkasm), retaining only the ~8 `use` swaps and
a thin vector-tier vocabulary shim (§5) for the store/inline sites. Net parse-that surface for the
port drops from ~+380 to ~+60; the ~320 saved LOC (and its oracle) live once in bbnf-simd.

---

## 3. The canonical producers are ABSENT, and the refuted idiom is PERVASIVE (the census)

This is the concrete state the charter's "must be authored once into any home regardless of
ownership" refers to, verified by source read:

**(a) Five NEON producers are unbuilt stubs that delegate straight to scalar** — 4–6 LOC each:
`aarch64/byte_class_from_table_64.rs:2` (the DENSE-64 structural classifier — the primary
producer both repos' structural scan needs — is `return …_scalar(src, table)`),
`aarch64/bitmap_prefix_xor_64.rs`, `aarch64/bulk_emit_positions_64.rs`,
`aarch64/bitmap_next_set_bit.rs`, `aarch64/eob_pad_clamp.rs`. The dense-64 movemask producer does
not exist in NEON at all.

**(b) The four *built* producers pack their 16-lane mask with the REFUTED double-`vaddv`:**
`aarch64/byte_class_from_eq_set_64.rs:83-84`, `structural_classify_64.rs:151-152`,
`comment_body_mask_64.rs:72-73`, `bracket_depth_mask_64.rs:78-79` — each does
`vaddv_u8(vget_low_u8(bits))` + `vaddv_u8(vget_high_u8(bits))` against a `[1,2,4,…,128]` bit LUT.
That is exactly the idiom refuted at the instruction level (a paired disasm showed the double-`vaddv`
movemask emits ~4× the reduction ops of `vshrn`, and the port that used it regressed on window A;
the refutation is SPEC §0 law). `movemask.rs:5` *starts* with `vshrn_n_u16::<4>` but then degrades
into a `vand/vorr/vsri/vzip1q` + **scalar `for pair in 0..8`** loop (`:20-24`) — neither the clean
LEAN nibble-stride nor a DENSE tree. Only `match_tiny_plain_string.rs:101` uses `vshrn` cleanly.

**The once-authoring, precisely.** Author two contract-split producers into bbnf-simd, purge the
refuted idiom, re-point all six mask-emitting producers at them:
- **LEAN** (iterate/sparse): `vshrn_n_u16::<4>(reinterpret_u16(cmp))` → 64-bit nibble-stride mask
  in a `d`-register; consumers scan with `tz>>2`, index `= bit>>2`. One `shrn` per 16-B stripe, no
  horizontal reduction. This is the confirmed idiom and is what memchr's own NEON hot path emits
  (`shrn.8b v0,v0,#4` then `fmov`/`ands #0x8888…`).
- **DENSE-64** (true bit-i↔byte-i bitmap for a 64-B block): four `cmeq.16b` → a narrow tree feeding
  one 64-bit result, or four `vshrn` stripes shifted into `u64` lanes — **never** four `vqtbl1q`
  chained on the short runs these scanners see, and **never** the double-`vaddv`.
  `byte_class_from_eq_set_64.rs:66-71` already assembles four stripes `lo0|(lo1<<16)|…`; the fix is
  replacing its inner `movemask_u8x16` (the double-`vaddv` at `:79-87`) with the `vshrn` narrow.

---

## 4. F1b — where `#[inline]` actually fails to cross the seam

Cross-crate `#[inline]` over the parse-that↔bbnf-simd rlib boundary does **not** break for the
`cfg`-direct producers: `byte_class_from_eq_set_64`/`structural_classify_64`/`comment_body_mask_64`/
`bracket_depth_mask_64` are `#[inline]` (`lib.rs:282,300,315,335`), so rustc emits their MIR into
the rlib metadata and the consumer crate inlines them regardless of codegen-units or LTO — the
monomorphised body crosses the seam. A non-inlinable boundary appears in exactly two named places,
**neither of which is the crate seam**:

1. **The `OnceLock` fn-pointer table** (`dispatch.rs:50-61`). `primitive_kernels().byte_class_from_table_64`
   is an indirect call through a runtime-selected pointer; the indirection is *intentional* (load-time
   scalar-vs-NEON selection) and is present in either home — copying `dispatch.rs` into a two-home
   parse-that reproduces it verbatim. This is the only in-loop indirect, and it is a design choice,
   not a seam artifact. (9A can additionally *retire* it for the aarch64-only product: with x86
   abrogated and scalar only a `#[cfg(not(aarch64))]` fallback, the `cfg`-direct form dominates and
   the fn-pointer table can shrink to genuinely runtime-featured kernels, if any remain.)
2. **A producer left un-`#[inline]` under `cgu>1` + `lto=false`.** If the finished DENSE-64 body is
   added *without* `#[inline]`, cross-crate inlining fails only under the iteration profile
   (`cgu=16`, no LTO); under `[profile.bench]` fat-LTO it inlines regardless. The guard is trivial:
   every producer carries `#[inline]`, as the four built ones already do. Evidence to capture: a
   paired `otool -tv` of a consumer bench binary showing zero `bl _…byte_class…` in the hot loop
   under both `ax-iter` and `bench` profiles.

---

## 5. F1c — scanner-file mapping census (owed): kernel-tier / vector-tier / no-go

The eight `portable_simd` scanner files, classified 1:1 against bbnf-simd producers:

| parse-that scanner | hits | maps to bbnf-simd producer | tier |
|---|---|---|---|
| `parsers/scan/structural_bitmap.rs` | 13 | `byte_class_from_table_64` / `structural_classify_64` | **kernel 1:1** |
| `parsers/scan/ws_comment.rs` | 16 | `comment_body_mask_64` | **kernel 1:1** |
| `parsers/scan/quoted_simd.rs` | 7 | `escape_mask_64` (`lib.rs:174`) + `bitmap_prefix_xor_64` | **kernel 1:1** |
| `parsers/scan/quote_parity.rs` | 4 | `bitmap_prefix_xor_64` (pmull) + `structural_classify_64` | **kernel 1:1** |
| `parsers/scan/decode.rs` | 17 | `string_block`/`unescape_uxxxx` (kernel) **+** `store_chunk` u8x16 store | **mixed** |
| `scanners.rs` | 22 | `find_ascii_set_member64` (`lib.rs:208`)/`byte_class_from_eq_set_64` (kernel) **+** inline u8x16 | **mixed** |
| `span_parser/scan.rs` | 7 | generic u8x16 inline scans | **vector-tier** |
| `parsers/scan/number_simd.rs` | — | `digit_mac` exists but SPEC §0/02 bars sharing | **no-go** |

**Split:** ~6 files have a 1:1 kernel-tier producer already in bbnf-simd (structural, ws/comment,
quote/escape, eq-set membership); ~2–3 files carry a *vector-tier* residue — generic `u8x16`
load/compare/`store_chunk`/`swizzle_dyn` idioms with no named kernel — which want a thin shared
`u8x16`/`mask8x16` vocabulary module (~60 L, the LEAN movemask + splat/cmp/store) that BOTH repos
consume, homed in bbnf-simd alongside `prim`; and **1 no-go** (number_simd) that stays per-repo by
refutation law. The kernel-tier is where 9A's win concentrates; the vector-tier is the small shared
vocabulary; the no-go is explicitly not shared.

---

## 6. The sync-gate substrate — extract-vs-vendor, with a live precedent

The charter's "extracted or vendored under a sync gate" is not novel: `crates/csp-solver` is
vendored into bbnf-lang from sibling repo `csc411` under a **canonical-diff re-vendor gate** — the
commit ledger records `chore(csp-solver/canonical-diff): re-vendor src/ from csc411@<sha>` beats,
and `crates/core/tests/{json,css_l4}_canonical_parity.rs` enforce byte-parity of the vendored copy.
That is the exact mechanism 9A needs, proven in-tree. Two concrete layouts:

- **Vendor (recommended, mirrors csp-solver).** bbnf-simd stays authoritative in `skinny/crates/`;
  parse-that consumes it via one `.cargo/config.toml` patch line
  (`bbnf-simd -> ../../bbnf-lang/skinny/crates/bbnf-simd`) — the patch table already cross-repo-links
  `bbnf`, `bbnf-ir`, `bbnf-ser`, `egraph`, `csp-solver` (7 external sibling patches; adding one more
  is mechanical). A `canonical-diff` beat + a `bbnf_simd_canonical_parity` test on the parse-that
  side gates drift, exactly as csp-solver does. No code moves; the isomorphism is a shared rlib.
- **Extract to a neutral workspace crate.** Only if bbnf-lang-specific `blake3` parity-hashing or the
  `StructuralAlphabet`/`StructuralIndex` domain types (`lib.rs:19,71`) are judged too coupled;
  the pure `aarch64/` + `scalar/` + `checkasm_common.rs` core is domain-neutral and would extract
  cleanly (the domain types stay bbnf-side). Higher churn; only if a coupling counterexample lands.

**Vendor is favored by the constitution's "consumer-less substrate forbidden" (viii):** bbnf-simd
already has an in-repo consumer (the skinny scanning plane); vendoring adds parse-that as a second
consumer without minting an orphan crate.

---

## 7. F2-adjacent — the shared home is stable-native and oracle-bearing

bbnf-simd is *already* the stable-toolchain target parse-that's de-nightly is reaching for: pure
`core::arch::aarch64`, no `portable_simd`, no `smallbox[nightly]`. Consuming it discharges PT-2 W1's
port as a *dependency edge* rather than a re-authored NEON file — the de-nightly of the scanner
plane becomes "delete `std::simd`, call `bbnf_simd::prim::*`". The one nightly residual in
parse-that's own tree (the `@generated` `fmt_helpers_for_derive` in `regex/generated.rs`) is
orthogonal to the kernel home and unaffected.

**The oracle already answers F2's parity question.** checkasm (`checkasm_common.rs`) is the
differential harness: xorshift black-box inputs + stack-canary + callee-saved clobber detection +
scalar-reference bit-equality. The **naive-nibble-on-dense-64 parity trap** F2/F3 names is real and
must be a test case: a LEAN `vshrn>>4` nibble stride is correct for *iterating* sparse positions but
**corrupts a dense 64-bit bit-i↔byte-i bitmap** (nibble stride packs 4 bits/lane, not 1) — so the
DENSE-64 producer must NOT be a naive nibble emit. The checkasm sweep for `byte_class_from_table_64`
must assert full 64-bit parity against the scalar table classifier, on aligned + misaligned +
all-set + all-clear vectors, to catch precisely that corruption.

---

## 8. Prior art (citations)

- **dav1d checkasm** (VideoLAN, `tests/checkasm/checkasm.c`): the callee-saved-register + stack
  clobber + reference-parity discipline bbnf-simd's `checkasm_common.rs` reproduces. The admission
  gate ("scalar reference is the spec, NEON parity-checked") is dav1d's primitive-lift protocol.
- **rust `regex-automata` / `memchr`** (BurntSushi): the `vshrn_n_u16::<4>` movemask + `ands #0x8888…`
  nibble-stride scan is memchr's shipped aarch64 hot path; the entry-byte prefilter tier (Byte1/2/3
  → memchr/2/3) is regex-automata's `Prefilter`. Confirms the LEAN idiom as canonical and the
  1/2/3-byte membership tier as the tractable one.
- **simdjson stage-1** (Langdale & Lemire, *Parsing Gigabytes of JSON per Second*): the structural
  bitmap + `prefix_xor` (carry-less-multiply / `vmull_p64`) quote-parity that
  `bitmap_prefix_xor_64`/`quote_parity.rs` implement. The dense-64 classifier is stage-1's
  `classify_chunk`.
- **Lemire & Mula, movemask-on-ARM**: the `vshrn` narrow vs the `vand`+`vaddv` bit-sum — the exact
  contrast between the confirmed and refuted idioms here.
- **cargo `[patch]` + workspace path-dep vendoring**: the csp-solver canonical-diff beat is the
  in-tree instance of the "one source, re-vendored under a parity gate" pattern.

---

## 9. Named risks & counterexamples

- **R1 (the refuted idiom re-enters).** Finishing the stub producers with the pervasive
  double-`vaddv` movemask (§3b) reproduces the refuted reduction tree. *Guard:* a
  `rg 'vaddv_u8|vaddvq_u8' skinny/crates/bbnf-simd/src/aarch64` census in the checkasm gate must
  return only the digit-MAC site; every mask producer must show `shrn` in disasm.
- **R2 (dense-64 nibble corruption).** A naive `vshrn>>4` LEAN emit used where a dense bit-i↔byte-i
  bitmap is required silently misclassifies (4-bit stride). *Guard:* §7's full-64-bit parity sweep;
  the two producers are contract-split by name, never interchanged.
- **R3 (clamp-convention mismatch).** parse-that's trailing-NUL `PaddedView` vs bbnf-simd's
  `eob_pad_clamp` disagree on the last <64 bytes; a shared producer that assumes the wrong one
  over/under-reads. *Guard:* the seam fixes ONE clamp contract, checkasm feeds both padded and
  un-padded 64-blocks.
- **R4 (no-go leakage).** A "unify everything" impulse shares a digit kernel via `digit_mac`,
  violating SPEC §0/02. *Guard:* number_simd is census-classified no-go; the vendor edge excludes it.
- **R5 (vendor drift).** The parse-that copy lags bbnf-simd. *Guard:* the csp-solver canonical-diff
  beat + a `bbnf_simd_canonical_parity` test (byte-identity of the vendored src).
- **R6 (perf-neutrality, not win).** 9A's value is single-source correctness + drift-elimination +
  the refuted-idiom purge, NOT a speedup — the port itself is cyc/B-neutral (SPEC §0/09 law). A
  resurrected speed claim is a defect; the only perf obligation is a measured no-regression floor.

---

## 10. What a prototype must test to move family 9A

1. **Finish + purge, once.** Author the LEAN `vshrn` and DENSE-64 producers into bbnf-simd; replace
   the double-`vaddv` in the four built producers and the placeholder in `movemask.rs`; build the
   five scalar-stub NEON bodies. Gate: checkasm full-64-bit parity for all, `rg vaddv` clean.
2. **Consumer edge.** Add the `.cargo/config.toml` patch line + swap the ~6 kernel-tier scanners'
   `std::simd` calls to `bbnf_simd::prim::*`; author the ~60-L shared `u8x16` vocabulary for the
   ~2–3 vector-tier sites. Gate: parse-that's ~400 tests green on stable, `rg 'std::simd'` → 0 in
   the ported files.
3. **Seam-inlining disasm.** Paired `otool -tv`/`cargo asm` of a parse-that combinator bench binary,
   both `ax-iter` (`cgu=16`) and `bench` (fat-LTO): assert zero `bl _…byte_class…` in the hot loop
   (the `#[inline]` producers cross the seam) and that the only indirect is the intentional
   `OnceLock` pointer. Report insn + `shrn`/`addv` counts base-pinned.
4. **One-home vs two-home LOC ledger.** Measure the residual: parse-that surface with the vendor
   edge (~+60) vs PT-2 W1's private `simd/` (~+380), and the audit surface (`rg vaddv` over one
   crate vs two). Both directions built; report the delta as data.
5. **No-regression floor.** Paired divan (two windows, opposite arm order, N≥200) of the vendored-
   kernel parse-that vs the private-`simd/` parse-that on the six JSON + CSS corpora; assert
   ≥0.98× every corpus, both windows. The claim is neutrality, not speedup.
6. **Sync-gate beat.** Reproduce the csp-solver canonical-diff mechanism for bbnf-simd: a re-vendor
   commit shape + a `bbnf_simd_canonical_parity` byte-identity test; prove drift is caught.
