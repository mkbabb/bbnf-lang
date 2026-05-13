# SK-V5 D4 — Novelty interrogation: "bbnf-simd JSON god-module split"

Audit subject: SK-V5 cohort agent A5's finding that `skinny/crates/bbnf-simd/`
violates Lock 14 / Lock 16 by carrying a 716-LOC god-module `lib.rs` with
JSON-specific names, types, and intrinsic bodies; and A4's finding that
`skinny/crates/simd-scan/` is a parallel dead-substrate fossil left behind by
the MIGRATION rename.

Method: walked the bbnf-simd tree, counted per-file LOC and JSON-isms, read
MIGRATION.md §9.3 + §3.x, walked the simd-scan fossil, walked the skinny
workspace metadata, replayed the latest commit (9eef728c), and cross-checked
A4/A5 citations against the live tree.

---

## §1 — `bbnf-simd` current structure (per-file LOC + responsibility)

`skinny/crates/bbnf-simd/src/` — 5 entries; structure is per-ISA-first, not per-primitive-first.

| Path | LOC | Responsibility |
|---|---:|---|
| `lib.rs` | **716** | Mixed surface: 5 ISA-neutral structs (`StructuralAlphabet`, `StructuralIndex`, `JsonParseIndex`, `ScalarParityReport`, `ScanBackend`), 12 ISA-neutral free fns (`scan_dispatch`, `scan_json_structurals`, `scan_json_parse_index`, `scalar_positions`, `scan_json_tail`, `scan_json_tail_parse`, `scalar_json_parse_index`, `is_json_punctuation`, `escape_mask_64`, `prefix_xor_64`, `compact_mask`, `resolve_json_string_masks_64`, `hash_positions`), the 230-LOC `mod neon` (lines 463-693), one `match_json_tiny_plain_string` shim, and finally a 16-line `pub mod prim` (lines 701-716) holding the only grammar-neutral entry-point (`byte_class_from_eq_set_64`) added in the latest commit. |
| `classifier.rs` | 13 | Trait `SimdClassifier` + `ClassifyResult` struct (4 u64 masks). Grammar-neutral. |
| `dispatch.rs` | 49 | `SelectedClassifier`. Backend enum is `enum SelectedBackend { Scalar, NeonJson }` (`dispatch.rs:13`). Only fast path the dispatcher knows is `NeonJson`. |
| `aarch64/` (12 files) | 845 | Per-feature primitives: `byte_class_from_eq_set_64.rs:87` (NEW, grammar-neutral), `classify_tbl4.rs:76` (JSON-baked; see §4), `match_tiny_plain_string.rs:162`, `unescape_uxxxx.rs:152`, `string_block.rs:103`, plus 7 small helpers (movemask, quad_load, digit_mac, byte_context, cache_hints, mod.rs). |
| `scalar/` (3 files) | 60 | `byte_class_from_eq_set_64.rs:38` (NEW, grammar-neutral *reference*), `swar_8byte.rs:18`, `mod.rs:4`. |
| `x86_64/` (3 root + 8 subdirs) | 671 | 1 grammar-neutral primitive at root (`byte_class_from_eq_set_64.{asm,rs}`); 8 per-ISA-feature directories (`avx2/`, `avx512_bitalg/`, `avx512_gfni/`, `avx512_kmask/`, `avx512_vbmi2/`, `avx512_vnni/`, `avx512_vpclmul/`, `avx_ifma/`) each holding skeleton classifier/compress/carry stubs with `unimplemented!("Wave 6: ...")` bodies. Every `classify_block_scalar` skeleton (4 separate files) hardcodes `matches!(b'{' | b'}' | b'[' | b']' | b',' | b':' | b'"')`. |

Total crate src ≈ **2,354 LOC** across 39 files; the `lib.rs` alone is 30% of that, and inside it `mod neon` (lines 463-693) is 230 LOC — A5's "230-line mod neon" count is exact.

Verification of A5's specific claim: `lib.rs:642-648` is the literal 6-`vceqq_u8` fan-in, again at `:665-670`:

    let mut punctuation = vceqq_u8(chunk, vdupq_n_u8(b'{'));
    punctuation = vorrq_u8(punctuation, vceqq_u8(chunk, vdupq_n_u8(b'}')));
    punctuation = vorrq_u8(punctuation, vceqq_u8(chunk, vdupq_n_u8(b'[')));
    punctuation = vorrq_u8(punctuation, vceqq_u8(chunk, vdupq_n_u8(b']')));
    punctuation = vorrq_u8(punctuation, vceqq_u8(chunk, vdupq_n_u8(b':')));
    punctuation = vorrq_u8(punctuation, vceqq_u8(chunk, vdupq_n_u8(b',')));

— exactly the 6 punctuation comparisons A5 cited, twice (`classify_chunk` + `classify_parse_chunk`). The DAV1D shape A2 mandates (per-primitive directory, scalar reference per primitive, ISA file per primitive) is NOT what `lib.rs` shows; the per-primitive shape is only realised for the ONE primitive that landed end-to-end in 9eef728c (`byte_class_from_eq_set_64`).

---

## §2 — `simd-scan` fossil status

Path: `skinny/crates/simd-scan/`
Contents:
  - `Cargo.toml` (10 lines, self-name `simd-scan`, single dep `blake3`).
  - `src/lib.rs` — **584 LOC** monolith.

Workspace membership:
  - `skinny/Cargo.toml:3-15` workspace.members = [bbnf, grammar, ir, passes, codegen, runtime, parse-that-regex, **bbnf-simd**, bbnf-bench, test-fixtures, xtask]. **simd-scan is NOT a member.**
  - `skinny/Cargo.toml:23-33` workspace.dependencies declares `bbnf-simd`; **does not declare `simd-scan`.**
  - `grep -rn "simd-scan\\s*=" skinny/crates/*/Cargo.toml` → ZERO matches. No skinny crate depends on it.
  - The one apparent hit, `skinny/crates/bbnf-bench/Cargo.toml:34 name = "simd_scan"`, is the criterion `[[bench]]` target NAME (the bench file is `benches/simd_scan.rs`, which uses `bbnf_bench::scan::structural_offsets_simd` — i.e. it calls into `bbnf-simd` via `bbnf-bench::scan`). It is NOT a `simd-scan` crate dep.

Contents leak: the fossil's `src/lib.rs:1-50` is a near-verbatim duplicate of the current `bbnf-simd/src/lib.rs:13-100` (`ScanBackend`, `StructuralAlphabet`, `JSON_STRUCTURAL`, `StructuralIndex`). Same JSON-isms (`is_json_structural`, `JsonParseIndex`, `scan_json_structurals`, `scan_json_parse_index`) appear at the fossil's `:28, :43, :77, :112, :125`.

Comparison to the *root* workspace's `crates/simd-scan/`: 10 files, 2607+ LOC (`alphabet.rs`, `avx2.rs`, `avx512.rs`, `compaction.rs`, `index.rs`, `lib.rs`, `neon.rs`, `parity.rs`, `scalar.rs`, `wasm.rs`). That is the OLDER pre-skinny crate, owned by the legacy root workspace (`/Users/mkbabb/Programming/bbnf-lang/Cargo.toml:2` lists `crates/simd-scan`), unrelated to the skinny tree.

Deletable: **yes, mechanically**. The skinny fossil has zero workspace gravity and zero callers; deletion is a one-line `rm -r` plus a one-grep verification (`grep -rn "simd-scan\\|simd_scan" skinny/`, excluding the unrelated bench-target-name and the criterion output directory).

---

## §3 — MIGRATION.md prior-art audit

`restart/MIGRATION.md` names this rename in four sections; the intent is documented, the deletion was never done:

| Site | Verbatim text (paraphrased citation) |
|---|---|
| `MIGRATION.md:49` | "KEEP-OUTRIGHT … `bbnf-simd` (renamed from legacy `simd-scan`; primitive boundary per Lock 14 + Lock 16) …" |
| `MIGRATION.md:75` | "`crates/simd-scan` → **`crates/bbnf-simd`** — rename per Lock 14 + Lock 16; primitive boundary, no JSON-specific code." |
| `MIGRATION.md:104` | "Keep and wire to BIR; rename to `bbnf-simd` per Lock 14/16 (grammar-neutral primitive boundary)." |
| `MIGRATION.md:158-159` | partly KEEP-OUTRIGHT (generic core retained); partly to `passes/src/recognizers/` (BBNF-specific recognizer wiring); `bbnf-simd/`. |
| `MIGRATION.md:259-269` | §3.x dedicated subsection "`crates/simd-scan` → `crates/bbnf-simd` (rename per Lock 14 + Lock 16)". File-and-family table: scalar → `bbnf-simd/scalar`, NEON → `bbnf-simd/aarch64/`, AVX-2/AVX-512 → `bbnf-simd/x86_64/{...}/`, dispatch → `bbnf-simd/dispatch`. |
| `MIGRATION.md:489-493` §9.3 | "`simd-scan` → `bbnf-simd` … grammar-neutral primitive boundary per Lock 14 + Lock 16." |

What MIGRATION.md NEVER says: "delete the old `simd-scan/` directory tree under `skinny/crates/`." The plan reads as a single-location rename; in execution, the new tree was created and the old crate was left in place.

`restart/skinny/WORKSPACE.md:123` already lists the post-rename workspace as `members = [..., "crates/bbnf-simd", ...]` — no `simd-scan` entry. `restart/skinny/SUBSTRATE.md:259` calls out the per-ISA NEON shape under `bbnf-simd/aarch64/`. The docs are post-rename; the FS state lags.

`restart/skinny/BENCH.md:1355` notes the SWAR scalar fallback lives at `bbnf-simd/scalar/`, again matching the post-rename shape. No doc mentions splitting `lib.rs` god-module.

---

## §4 — JSON-ism count by file

| Path | LOC | "json" hits | JSON-byte literals (`b'{' \| ... \| b'"'`) |
|---|---:|---:|---|
| `src/lib.rs` | 716 | **40** | `:333` (`is_json_punctuation`), `:642-647` (NEON `classify_chunk`), `:665-670` (NEON `classify_parse_chunk`) |
| `src/classifier.rs` | 13 | 0 | 0 |
| `src/dispatch.rs` | 49 | 5 | 0 — but enum variant `NeonJson` at `:13` |
| `src/aarch64/classify_tbl4.rs` | 76 | 7 | `:65-71` — 6 JSON punctuation bytes baked into the TBL4 LUT at compile time |
| `src/aarch64/match_tiny_plain_string.rs` | 162 | 8 | embeds `b'"'`, `b'\\'`, `0x20` (a JSON-string-specific specials set per A5) |
| `src/aarch64/unescape_uxxxx.rs` | 152 | 5 | (JSON `\\uXXXX` decode) |
| `src/aarch64/byte_class_from_eq_set_64.rs` | 87 | 2 | 0 — references "JSON parsing paper" in citation prose only; impl is grammar-neutral |
| `src/scalar/byte_class_from_eq_set_64.rs` | 38 | 2 | 0 — citation prose only; impl is grammar-neutral |
| `src/x86_64/byte_class_from_eq_set_64.rs` | 54 | 2 | 0 — citation prose only |
| `src/x86_64/avx2/classify.rs` | 48 | 5 | `:31` `classify_block_scalar` — 7 JSON bytes baked |
| `src/x86_64/avx2/prefix_xor.rs` | 48 | 4 | 0 |
| `src/x86_64/avx2/bmi2_emit.rs` | 43 | 3 | 0 |
| `src/x86_64/avx512_vbmi2/classify.rs` | 44 | 3 | `:28` — 7 JSON bytes baked |
| `src/x86_64/avx512_vbmi2/{compress,carry,mask_fuse}.rs` | 110 | 8 | 0 |
| `src/x86_64/avx512_gfni/classify_affine.rs` | 59 | 6 | `:31` — 7 JSON bytes baked, plus `JSON_STRUCTURAL_AFFINE_MATRIX` / `_BIAS` constant slots at `:43-44` |
| `src/x86_64/avx512_bitalg/multiclass.rs` | 56 | 2 | `:30` — 7 JSON bytes baked in a `_scalar_reference` |
| `src/x86_64/avx512_kmask/arithmetic.rs` | 44 | 2 | 0 |
| `src/x86_64/avx512_vnni/digit_mac.rs` | 40 | 3 | 0 |
| `src/x86_64/avx512_vpclmul/prefix_xor.rs` | 51 | 5 | 0 |
| `src/x86_64/avx_ifma/mantissa.rs` | 38 | 2 | 0 |

Total JSON-naming hits in `src/`: ~107 occurrences of the substring `json` (case-insensitive). Of those, the irreducible grammar-leak set is the JSON-byte-literal baked references (5 sites: `lib.rs:333,642-647,665-670`, `aarch64/classify_tbl4.rs:65-71`, `avx2/classify.rs:31`, `avx512_vbmi2/classify.rs:28`, `avx512_gfni/classify_affine.rs:31`, `avx512_bitalg/multiclass.rs:30`). A5's specific claim "AVX-2/AVX-512 scalar-reference skeletons hardcode `b'{' | b'}' | ...`" is verified exact: 4 separate `classify_block_scalar` functions across `avx2/classify.rs`, `avx512_vbmi2/classify.rs`, `avx512_gfni/classify_affine.rs`, `avx512_bitalg/multiclass.rs` each ship the same 7-byte hardcoded set.

---

## §5 — What is grammar-neutral vs grammar-leaked

GRAMMAR-NEUTRAL (1 of 9 Layer-1 macros end-to-end + 2 universal helpers):

| Surface | Location | Status |
|---|---|---|
| `BYTE_CLASS_FROM_EQ_SET_64` asm contract | `ext/x86/bbnf.asm` (Layer 1 macro declaration) | NEUTRAL by construction |
| `BYTE_CLASS_FROM_EQ_SET_64` AVX-512 BW body | `src/x86_64/byte_class_from_eq_set_64.asm` + Rust FFI shim `:.rs:1-55` | NEUTRAL — `set: &[u8]` runtime parameter |
| `BYTE_CLASS_FROM_EQ_SET_64` NEON body | `src/aarch64/byte_class_from_eq_set_64.rs:33-72` | NEUTRAL — `set: &[u8]` runtime parameter |
| `BYTE_CLASS_FROM_EQ_SET_64` scalar reference | `src/scalar/byte_class_from_eq_set_64.rs:26-38` | NEUTRAL — `set: &[u8]` runtime parameter, parity anchor |
| Dispatcher `pub mod prim` | `src/lib.rs:701-716` | NEUTRAL |
| `escape_mask_64`, `prefix_xor_64`, `compact_mask`, `checked_position`, `hash_positions` | `src/lib.rs:351-410, 405-409, 337-339, 341-349` | NEUTRAL (operate on `u64` masks; no byte literals) |
| `SimdClassifier` trait, `ClassifyResult` struct | `src/classifier.rs:1-13` | NEUTRAL |
| AArch64 helper primitives | `src/aarch64/{movemask, quad_load, byte_context, cache_hints, digit_mac, string_block}.rs` | NEUTRAL |
| x86_64 helper primitives | `src/x86_64/avx512_{vnni/digit_mac, vpclmul/prefix_xor, kmask/arithmetic, vbmi2/compress, vbmi2/carry, vbmi2/mask_fuse}.rs`, `avx_ifma/mantissa.rs`, `avx2/prefix_xor.rs`, `avx2/bmi2_emit.rs` | NEUTRAL (skeletons + scalar refs over generic `u64`s) |

GRAMMAR-LEAKED:

| Surface | Location | Form |
|---|---|---|
| `JSON_STRUCTURAL` const alphabet | `lib.rs:76` | `b"{}[],:\""` literal |
| `is_json_structural`, `is_json_structural_alphabet`, `is_json_punctuation` | `lib.rs:40, 54-64, 332-334` | hardcoded JSON tests |
| `JsonParseIndex` (and 3 typed offset streams) | `lib.rs:110-136` | JSON-named public type |
| `scan_dispatch`, `scan_json_structurals`, `scan_json_parse_index`, `scalar_json_parse_index` | `lib.rs:138-180, 315-329` | JSON-named entry points |
| `scan_json_tail`, `scan_json_tail_parse`, `resolve_json_string_masks_64` | `lib.rs:244-275, 277-313, 411-461` | JSON-grammar state machines (200 LOC) |
| `match_json_tiny_plain_string`, `match_json_tiny_plain_string_scalar` | `lib.rs:190-206` | JSON-string-specific specials |
| `mod neon` — `scan_json`, `scan_json_parse`, `classify_chunk`, `classify_parse_chunk`, `backslash_chunk` | `lib.rs:463-693` | 230 LOC; 6× hardcoded `vceqq_u8(vdupq_n_u8(b'{')) ...` (twice over) |
| `enum SelectedBackend { Scalar, NeonJson }` | `dispatch.rs:13` | backend variant named for one grammar |
| `select_backend` → routes JSON alphabet to `NeonJson` | `dispatch.rs:39-48` | only fast path = JSON |
| `classify_json_chunk`, `classify_json_block`, `classify_json_ascii`, `json_ascii_table` | `aarch64/classify_tbl4.rs:8, 23, 49-58, 61-76` | JSON-named NEON kernels + 8-byte JSON LUT baked into `json_ascii_table` |
| `match_json_string_specials_neon` | `aarch64/match_tiny_plain_string.rs:110-122` | duplicates a grammar-neutral primitive sitting above it |
| 4× `classify_block_scalar` skeletons | `avx2/classify.rs:28-36`, `avx512_vbmi2/classify.rs:25-33`, `avx512_gfni/classify_affine.rs:28-36`, `avx512_bitalg/multiclass.rs:25-33` | each hardcodes `b'{' \| b'}' \| b'[' \| b']' \| b',' \| b':' \| b'"'` |
| `JSON_STRUCTURAL_AFFINE_MATRIX`, `JSON_STRUCTURAL_AFFINE_BIAS` | `avx512_gfni/classify_affine.rs:43-44` | grammar-named `const u64`/`const u8` slots set to 0 |
| Public NEON entry-point names in tests | `aarch64/string_block.rs` JSON-specific names | name leak (impl is generic over `&[u8]`) |

Ratio: **~85% of the SIMD surface area carries JSON identifiers**; **~15%** (the 9eef728c primitive plus a handful of helper modules) is structurally grammar-neutral. The asm contract layer `ext/x86/bbnf.asm` is grammar-neutral by construction, exactly as A5 said.

---

## §6 — Final novelty verdict

| Claim under audit | Verdict | Citations |
|---|---|---|
| "`bbnf-simd/src/lib.rs` is a 716-LOC god-module with JSON-isms" | **DOCUMENTED-BUT-INCOMPLETE** | The split-by-primitive shape is documented in `MIGRATION.md:259-269` and `restart/skinny/SUBSTRATE.md:259`. The split landed ONCE in 9eef728c (`byte_class_from_eq_set_64` end-to-end across `scalar/`, `aarch64/`, `x86_64/`). The other 8 Layer-1 macros remain inside `lib.rs` as scalar + NEON bodies (`scan_json_tail`, `scan_json_parse`, `mod neon::*`). Per-primitive directory shape exists (`x86_64/avx2/`, `x86_64/avx512_*/`) but the bodies are `unimplemented!()` stubs; the actual NEON body still lives in `lib.rs:463-693`. |
| "`simd-scan` fossil exists" | **HALF-DONE** (rename done, deletion not done) | `skinny/crates/simd-scan/` is a complete duplicate scanner crate (`src/lib.rs:584 LOC`, single file). It is NOT in `skinny/Cargo.toml:3-15` workspace.members; it is NOT declared in `skinny/Cargo.toml:23-33` workspace.dependencies; ZERO skinny Cargo.toml files name it as a dep. The shape A4 reports (`StructuralIndex`, `JsonParseIndex`, `scan_json_structurals`, `scan_json_parse_index` duplicating bbnf-simd) is exact — verified at fossil `src/lib.rs:46,77,112,125`. |
| "MIGRATION renamed `simd-scan → bbnf-simd` but didn't delete" | **HALF-DONE** (documented intent; FS deletion not done) | `MIGRATION.md` mentions the rename in 5 sections (`:49, :75, :104, :158-159, :259-269, :489-493`). The new crate exists and is wired into the skinny workspace; the old crate exists in the tree with no callers. No MIGRATION line says "delete `skinny/crates/simd-scan/`"; the implicit assumption (a rename is a move, not a copy) was violated in execution. |
| "AVX-2 / AVX-512 scalar-reference skeletons hardcode JSON punctuation" | **NEW** (precisely as reported) | 4 separate scalar-reference functions all hardcode the same 7-byte JSON structural set: `avx2/classify.rs:31`, `avx512_vbmi2/classify.rs:28`, `avx512_gfni/classify_affine.rs:31`, `avx512_bitalg/multiclass.rs:30`. Each carries a doc comment claiming "scalar reference — parity anchor" while the parity it anchors is JSON-specific. The same JSON byte-set is *also* hardcoded in `aarch64/classify_tbl4.rs:65-71` (TBL4 LUT) and in two locations in `lib.rs:642-647, 665-670` (NEON `classify_chunk`/`classify_parse_chunk`). The architectural shape A5 mandates — Layer-1 contract takes the byte-set as a runtime/codegen-emitted parameter, exactly as `byte_class_from_eq_set_64` does — is followed by precisely 1 primitive of the planned 9. |

Overall novelty verdict on the cohort A5 + A4 finding pair: **NEW finding with respect to the FS state; the *intent* was named by MIGRATION but the realisation is 1/9 done.** A5 identifies 8 more Layer-1 macros that should follow `BYTE_CLASS_FROM_EQ_SET_64` out of the god-module; A4 identifies the parallel-crate dead-substrate fossil. Both are mechanically actionable. The `lib.rs` decomposition is not redundant with prior commits — 9eef728c lifted exactly one primitive and explicitly left the other eight where they were. The `simd-scan` deletion is not redundant — no commit yet has touched the fossil directory.

Confidence: HIGH. Falsifier (would refute this finding): (a) a commit on master that already lifts `scan_json_tail` / `resolve_json_string_masks_64` / `mod neon::scan_json` out of `lib.rs` into per-primitive directories — none exists (`git log --all --oneline -- skinny/crates/bbnf-simd/src/lib.rs` shows only 2 commits: `c7d2bf93` concretization + `9eef728c` Layer-1 vocabulary skeleton, and the latter touches `lib.rs` only to add the 16-line `pub mod prim` shim). (b) a `bbnf-simd` Cargo.toml feature/cfg that gates the JSON-named functions out of `lib.rs` by default — none exists (all functions are unconditional `pub fn`).
