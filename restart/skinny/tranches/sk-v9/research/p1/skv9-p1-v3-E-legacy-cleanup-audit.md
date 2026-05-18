# SK-V9 P1-V3-E — Legacy Cleanup Audit Manifest

Date: 2026-05-18.
Scope: read-only triage manifest. No deletions; no moves; no commits.
Authority: `skinny/REDRESS.md` (rejected-route ledger), `restart/skinny/tranches/sk-v7/RESTRUCTURE.md` (commit 75746318 sweep), Lock 16 (no orphan SIMD primitives).

---

## §1 Doc-corpus triage

Group conventions: KEEP = current authority or active tranche surface; ARCHIVE-MOVE = preserved historical authority that should leave the active tranche tree; DELETE = pure stale duplicate / dead link / superseded by later authority.

### 1.1 `restart/skinny/` top-level surfaces (6 files)

| File | LOC | Verdict | Reason |
|---|---:|---|---|
| `INDEX.md` | 176 | KEEP-STALE | Active spec index; declares SK-V6 as dispatch anchor and points to non-existent `audit/IMPLEMENTATION-AGENT-PROMPT-SK-V6.md` + `audit/GRAND-SYNTHESIS-SK-V5.md` + `audit/SOTA-BEAT-DESIGN.md` + `audit/NUKE-PLAN-SK-V5.md` + `audit/SK-V5-COHORT/` (audit/ dir does not exist post-commit 75746318). Surface itself authoritative; path rewrites required by sk-v7 RESTRUCTURE.md "220 path-swap" deferred-Omega bucket. |
| `SUBSTRATE.md` | 748 | KEEP-STALE | Same audit/ dead-link pathology (cites `audit/pass-3-runtime/PASS-3.md` and `audit/pass-2-codegen/PASS-2.md`; both moved to `restart/pass-*/`). Spec content authoritative. |
| `COMPILER.md` | 995 | KEEP | Active spec; cited by sk-v7/SPEC.md. |
| `BENCH.md` | 2208 | KEEP | Active spec; cited by every wave plan. |
| `WORKSPACE.md` | 695 | KEEP | Active spec. |
| `HARDENING.md` | 202 | KEEP | Active spec; sk-v7 RESTRUCTURE R2 confirms not-a-duplicate of `restart/prompts/audit-specs/HARDENING-LENS-SET.md`. |

### 1.2 `restart/skinny/tranches/shared/` (1 file)

| File | LOC | Verdict | Reason |
|---|---:|---|---|
| `SOTA-BEAT-DESIGN.md` | ~1200 | KEEP | Cross-tranche design authority cited by SK-V5/V6/V7 and INDEX.md. References stale `IMPLEMENTATION-PACKET-SK-V3/V4/V6` paths (deferred-Omega bucket per sk-v7 RESTRUCTURE). |

### 1.3 `restart/skinny/tranches/sk-v3.5/research/` (6 files; tranche surface incomplete — no SYNTHESIS/SPEC/HANDOFF/DISPATCH-PROMPT)

| File | LOC | Verdict | Reason |
|---|---:|---|---|
| `01-git-history.md` | 401 | ARCHIVE-MOVE | Pre-SK-V5 git-archaeology research; load-bearing only for failure narrative; superseded by REDRESS 16-72 and the V9.5-PSI excavation. |
| `02-archive-deep-read.md` | ~600 | ARCHIVE-MOVE | Same provenance — pre-SK-V5 archive deep-read. |
| `03-failure-anatomy.md` | ~700 | ARCHIVE-MOVE | Pre-SK-V5 anatomy of SK-V3 retreat. |
| `04-skv3-vs-psi-diff.md` | ~500 | ARCHIVE-MOVE | Cites `IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT` (dead link). |
| `05-fsm-correctness.md` | ~600 | ARCHIVE-MOVE | FSM correctness preliminaries — V1 substrate now lives in SUBSTRATE.md §1. |
| `06-go-no-go-synthesis.md` | ~800 | ARCHIVE-MOVE | Pre-SK-V5 go/no-go; superseded by SK-V5/V6/V7 SYNTHESIS layer. |

sk-v3.5 has no SYNTHESIS/SPEC/HANDOFF/DISPATCH-PROMPT — the tranche structure precepts (sk-v7 commit 75746318) were not applied retroactively because sk-v3.5 was the legacy pre-formalism cohort.

### 1.4 `restart/skinny/tranches/sk-v5/` (19 files: 4 top-level + 15 research)

Top-level:

| File | LOC | Verdict | Reason |
|---|---:|---|---|
| `SYNTHESIS.md` | 25,121 (bytes) | ARCHIVE-MOVE | Historical authority; superseded by SK-V6 → SK-V7 → SK-V8 → SK-V9 SYNTHESIS chain. |
| `SPEC.md` | 36,886 (bytes) | ARCHIVE-MOVE | Historical SK-V5 spec; folded into SUBSTRATE/COMPILER/BENCH. |
| `HANDOFF.md` | 17,290 (bytes) | ARCHIVE-MOVE | Historical handoff; explicit reference to deleted `audit/GRAND-SYNTHESIS-SK-V5.md`. |
| `NUKE-PLAN.md` | 18,701 (bytes) | ARCHIVE-MOVE | Wave 0-4 deletion ledger; load-bearing for `simd-scan/` fossil rationale (see §2.7). |

`research/skv5-A1..A6, B1..B3, D1..D6` (15 files): ARCHIVE-MOVE block. All A/B/D-pass deep research; cited by SK-V5 SYNTHESIS only. No active tranche cites these directly.

### 1.5 `restart/skinny/tranches/sk-v6/` (56 files: 5 top-level + 51 research)

Top-level:

| File | LOC | Verdict | Reason |
|---|---:|---|---|
| `SYNTHESIS.md` | 12,548 (bytes) | ARCHIVE-MOVE | Historical SK-V6 SYNTHESIS; superseded. |
| `SYNTHESIS-WAVE-1-PLAN.md` | 62,583 (bytes) | ARCHIVE-MOVE | Historical Wave-1 plan; cited by REDRESS 65/66/67/68/69 for §9/§10/§11/§12 falsifiability anchors only. Citations into this file are static (already folded into REDRESS prose), so the file itself is archival. |
| `SPEC.md` | 10,641 (bytes) | ARCHIVE-MOVE | Historical. |
| `HANDOFF.md` | 4,110 (bytes) | ARCHIVE-MOVE | Historical. |
| `DISPATCH-PROMPT.md` | 26,306 (bytes) | ARCHIVE-MOVE | Historical SK-V6 dispatch authority. |

`research/skv6-A1..A6, B1..B6, C1..C6, R1..R6 (incl R1b..R6c), schema-A/B/C` (51 files): ARCHIVE-MOVE block. SK-V7 RESTRUCTURE R1 explicitly recommends splitting `SK-V6-COHORT` into `cohort/` (primary tier) and `cohort-redress/` (R*-redress mid-iteration tier). The recommendation has not yet been executed; this audit echoes it as ARCHIVE-MOVE with optional sub-tiering.

### 1.6 `restart/skinny/tranches/sk-v7/` (94 files: 5 top-level + 89 research)

Top-level:

| File | LOC | Verdict | Reason |
|---|---:|---|---|
| `SYNTHESIS.md` | 15,981 (bytes) | ARCHIVE-MOVE | Historical SK-V7 SYNTHESIS; superseded by SK-V8/V9. |
| `SPEC.md` | 20,474 (bytes) | ARCHIVE-MOVE | Historical SK-V7 SPEC. |
| `HANDOFF.md` | 9,009 (bytes) | ARCHIVE-MOVE | Historical handoff. |
| `DISPATCH-PROMPT.md` | 10,108 (bytes) | ARCHIVE-MOVE | Historical SK-V7 dispatch authority. |
| `RESTRUCTURE.md` | 11,925 (bytes) | KEEP | The 6-agent restructure synthesis itself; load-bearing — this is the proximate cause of all the doc-tree renames since commit 75746318. Should remain accessible without an archive hop. |

`research/skv7-A1..A6 (6), B1..B6 (6), C1..C6 (6), restructure-R1..R6 (6), wave-0..wave-10c (65 files)` (89 files): ARCHIVE-MOVE block. Wave 0-10c plan + R* research files are mid-iteration; SK-V8 W0 baseline supersedes them as the current measured authority.

### 1.7 `restart/skinny/tranches/sk-v8/` (352 files: 4 top-level + 348 research)

Top-level:

| File | LOC | Verdict | Reason |
|---|---:|---|---|
| `SYNTHESIS.md` | 15,086 (bytes) | ARCHIVE-MOVE | Historical SK-V8 SYNTHESIS; superseded by SK-V9. |
| `SPEC.md` | 38,573 (bytes) | ARCHIVE-MOVE | Historical SK-V8 SPEC. |
| `HANDOFF.md` | 19,900 (bytes) | KEEP-IF-CITED | Cited by current SK-V9 dispatch; verify before archive. SK-V9 is in-flight (today 2026-05-18). |
| `DISPATCH-PROMPT.md` | 10,436 (bytes) | ARCHIVE-MOVE | Historical SK-V8 dispatch. |

`research/`:
- `alpha/` (6 files: alpha-A..F): KEEP-IF-CITED. SK-V9 has its own `alpha/` cohort that supersedes; verify no cross-reference before archive.
- `alpha-hardening/V1, V2/` (~14 files): ARCHIVE-MOVE. SK-V8 alpha-hardening; superseded by SK-V9 alpha-hardening V1-V4.
- `g-alpha/G-ALPHA-PRESENTATION.md` (1 file): ARCHIVE-MOVE.
- `p2-substrate-ceiling/` (6 files + hardening/ subdir, 55 files total): ARCHIVE-MOVE. P2 work folded into SK-V9 SPEC §1.
- `p3/` (10 files + hardening/ subdir, 44 files total): ARCHIVE-MOVE. P3 wave-sequencing now SK-V9 P1.
- `skv8-W0..W6-*.md` (~25 files): ARCHIVE-MOVE. Wave-0-6 plans + research; SK-V8 closed honestly per REDRESS 91-93.
- `wave-0-hardening/V1..V12/` (84 files), `wave-2-hardening/V1..V5/` (5 dirs), `wave-3..wave-6-hardening/V*/` (~30 files): ARCHIVE-MOVE block. All per-wave hardening passes; closed once REDRESS 91-93 landed.

### 1.8 `restart/skinny/tranches/sk-v9/` (67 files: 4 top-level + 63 research)

| File group | Verdict | Reason |
|---|---|---|
| Top-level (SYNTHESIS, SPEC, HANDOFF, DISPATCH-PROMPT) | KEEP | Active in-flight tranche (last edit 2026-05-18 14:32). |
| `research/skv9-W0-*.md` (8 files) | KEEP | W0 close + R1-R6 reports — fresh, in-tranche. |
| `research/alpha/` (6 files), `research/alpha-hardening/V1..V4/` (28 files) | KEEP | Active S-P1 cohort. |
| `research/g-alpha/G-ALPHA-PRESENTATION.md` | KEEP | Active alpha presentation. |
| `research/p1/p1a..p1f-*.md` (6 files) | KEEP | Active P1 V1 cohort. |
| `research/p1/hardening/V1, V2 + HARDENING-S-P1-V*-CONSOLIDATED.md` (20 files) | KEEP | Active P1 hardening — adjacent to in-progress S-P1 V3 reframe. |

### 1.9 Doc-triage rollup

| Tranche | Files | KEEP | KEEP-STALE | KEEP-IF-CITED | ARCHIVE-MOVE | DELETE |
|---|---:|---:|---:|---:|---:|---:|
| restart/skinny top | 6 | 4 | 2 | 0 | 0 | 0 |
| shared/ | 1 | 1 | 0 | 0 | 0 | 0 |
| sk-v3.5 | 6 | 0 | 0 | 0 | 6 | 0 |
| sk-v5 | 19 | 0 | 0 | 0 | 19 | 0 |
| sk-v6 | 56 | 0 | 0 | 0 | 56 | 0 |
| sk-v7 | 94 | 1 | 0 | 0 | 93 | 0 |
| sk-v8 | 352 | 0 | 0 | 2 | 350 | 0 |
| sk-v9 | 67 | 67 | 0 | 0 | 0 | 0 |
| **Total** | **601** | **73** | **2** | **2** | **524** | **0** |

No file in the entire tree merits unconditional DELETE — the corpus is REDRESS-traceable evidence and must be preserved. Every legacy item is either KEEP (active) or ARCHIVE-MOVE (preserved historical authority).

---

## §2 Code-corpus triage

### 2.1 x86_64 orphan SIMD kernels (14 `unimplemented!()` shells)

REDRESS authority: REDRESS 50-55 wave-5 admission rule ("primitives without consumers cannot close … and cannot be credited toward SOTA … the next implementation packet must either land the missing consumers in the same wave or remove those primitive bodies from the Wave 5 close condition"). Lock 16 verbatim: scalar reference + same-wave consumer + checkasm gate required for admission.

| File:line | Body status | Consumer | Verdict |
|---|---|---|---|
| `skinny/crates/bbnf-simd/src/x86_64/avx2/classify.rs:45-46` | `unimplemented!("Wave 6: AVX-2 vpshufb …")` | test refs only (`tests/checkasm_parity.rs:458`) | SAFE-TO-DELETE per REDRESS 50-55 admission rule |
| `skinny/crates/bbnf-simd/src/x86_64/avx2/bmi2_emit.rs:42` | `unimplemented!("Wave 6: pext-driven …")` | test refs only (`tests/checkasm_parity.rs:464`) | SAFE-TO-DELETE |
| `skinny/crates/bbnf-simd/src/x86_64/avx2/prefix_xor.rs:46-47` | `unimplemented!("Wave 6: PCLMULQDQ …")` | test refs only (`tests/checkasm_parity.rs:467`) | SAFE-TO-DELETE |
| `skinny/crates/bbnf-simd/src/x86_64/avx512_vbmi2/classify.rs:42-43` | `unimplemented!("Wave 6: vpshufbitqmb …")` | test refs only (`tests/checkasm_parity.rs:477`) | SAFE-TO-DELETE |
| `skinny/crates/bbnf-simd/src/x86_64/avx512_vbmi2/carry.rs:30-31` | `unimplemented!("Wave 6: kshiftlq + korq …")` | none | SAFE-TO-DELETE |
| `skinny/crates/bbnf-simd/src/x86_64/avx512_vbmi2/mask_fuse.rs:33-34` | `unimplemented!("Wave 6: kandnq + korq …")` | test refs only (`tests/checkasm_parity.rs:493`) | SAFE-TO-DELETE |
| `skinny/crates/bbnf-simd/src/x86_64/avx512_vbmi2/compress.rs:41-42` | `unimplemented!("Wave 6: vpcompressd …")` | none | SAFE-TO-DELETE |
| `skinny/crates/bbnf-simd/src/x86_64/avx512_gfni/classify_affine.rs:56-57` | `unimplemented!("Wave 6: vgf2p8affineqb …")` | test refs only (`tests/checkasm_parity.rs:478`) | SAFE-TO-DELETE |
| `skinny/crates/bbnf-simd/src/x86_64/avx512_bitalg/multiclass.rs:63-64` | `unimplemented!("Wave 6: vpshufbitqmb 8-class …")` | test refs only (`tests/checkasm_parity.rs:484`) | SAFE-TO-DELETE |
| `skinny/crates/bbnf-simd/src/x86_64/avx512_kmask/arithmetic.rs:32-33` | `unimplemented!("Wave 6: kandnq + korq …")` | none | SAFE-TO-DELETE |
| `skinny/crates/bbnf-simd/src/x86_64/avx512_kmask/arithmetic.rs:42-43` | `unimplemented!("Wave 6: kortestq …")` | none | SAFE-TO-DELETE |
| `skinny/crates/bbnf-simd/src/x86_64/avx512_vnni/digit_mac.rs:38-39` | `unimplemented!("Wave 6: vpdpbusd …")` | test refs only (`tests/checkasm_parity.rs:502`) | SAFE-TO-DELETE |
| `skinny/crates/bbnf-simd/src/x86_64/avx512_vpclmul/prefix_xor.rs:49-50` | `unimplemented!("Wave 6: vpclmulqdq …")` | none | SAFE-TO-DELETE per REDRESS 88 (PMULL prefix-XOR rejected on aarch64; x86 vpclmul analog has even less viability evidence and zero consumers) |
| `skinny/crates/bbnf-simd/src/x86_64/avx_ifma/mantissa.rs:36-37` | `unimplemented!("H.W5/SK-V5 Wave 5: vpmadd52luq …")` | test refs only (`tests/checkasm_parity.rs:497`) | SAFE-TO-DELETE per REDRESS 80 (mantissa-widen rejected with zero measured fallback rate; this is the same family on x86) |

Surviving x86_64 module with admitted consumer: `byte_class_from_eq_set_64.rs` (54 LOC) — consumed by `bbnf-simd/src/lib.rs:266`. KEEP.

If all 14 orphan files are removed, this also enables removing their parent module declarations in `x86_64/mod.rs` (lines 1-9: `avx2`, `avx512_vbmi2`, `avx512_gfni`, `avx512_vpclmul`, `avx512_vnni`, `avx512_bitalg`, `avx512_kmask`, `avx_ifma`) and the corresponding test groups in `tests/checkasm_parity.rs:458-502`. Estimated dead LOC: ~440 src + ~80 tests.

### 2.2 aarch64 dead/orphan modules

| File:line | Body status | Consumer | Verdict |
|---|---|---|---|
| `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs` (full file, 136 LOC) | NEON kernel (`match_tiny_plain_string_neon`, scalar reference, `build_class_table_lo6`) | TEST-ONLY: `tests/checkasm_parity.rs:432, 436, 584, 595, 599, 603` | SAFE-TO-DELETE per REDRESS 28+33 (active NEON `match_tiny_plain_string` kernel rejected as retained parse-G fix) + REDRESS 72 (admitted scalar shape lives in generated runtime, not as a NEON primitive). The admitted `match_tiny_plain_string_with_cap::<16>` at `skinny/crates/runtime/src/grammars/json/generated.rs:171-185` and `skinny/crates/codegen/src/json_templates/generated.rs:171-185` is a 4-line scalar loop, NOT this NEON kernel. The `aarch64::match_tiny_plain_string` module has no production caller; only the differential test parity. Per Lock 16: a primitive with only test consumers is an orphan kernel. |
| `skinny/crates/bbnf-simd/src/aarch64/string_block.rs` (full file, 72 LOC) | NEON `scan_string_special_block` + scalar reference | LIVE: `parse-that-regex/src/lib.rs:472, 551` (in `skip_string_plain` / `skip_string_plain_trusted`) | KEEP-IF-USED. REDRESS 61/62/83 rejected this primitive as a *retained-generated trusted-string scanner wrapper*. The parse-that-regex consumers are a different surface: UTF-8-validating plain-string scanning inside `match_string_at_quote_trusted_utf8`. Verify the parse-that-regex consumers are themselves required by current generated runtime (they are — see `runtime/src/grammars/json/generated.rs:193` calling `match_string_at_quote_trusted_utf8`). |

### 2.3 aarch64 utility modules (test-only callers)

| File:line | Consumer | Verdict |
|---|---|---|
| `skinny/crates/bbnf-simd/src/aarch64/movemask.rs` (25 LOC) | LIVE: `aarch64/utf8/validate_block.rs:3` (`use crate::aarch64::movemask::movemask_u8x16`); test refs in `tests/aarch64_primitives.rs:28` | KEEP-IF-USED. utf8/validate_block is LIVE (consumed by parse-that-regex). |
| `skinny/crates/bbnf-simd/src/aarch64/quad_load.rs` (6 LOC) | TEST-ONLY: `tests/aarch64_primitives.rs:78` | SAFE-TO-DELETE — orphan utility kernel; no production consumer. |
| `skinny/crates/bbnf-simd/src/aarch64/byte_context.rs` (11 LOC) | TEST-ONLY: `tests/aarch64_primitives.rs:97, 98` | SAFE-TO-DELETE — orphan utility. |
| `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs` (71 LOC) | TEST-ONLY: `tests/aarch64_primitives.rs:170, 174, 182` | SAFE-TO-DELETE per REDRESS 80 (mantissa-widen rejected; digit-MAC family has no admitted same-wave consumer). |
| `skinny/crates/bbnf-simd/src/aarch64/cache_hints.rs` (33 LOC) | TEST-ONLY: `tests/aarch64_primitives.rs:190` | SAFE-TO-DELETE — orphan utility (Wave 5 streaming-store body, no admitted consumer). |

### 2.4 aarch64 LIVE primitives (KEEP — verified consumers)

For the record, the surviving aarch64 modules with production callers:

| File | Consumer |
|---|---|
| `aarch64/byte_class_from_eq_set_64.rs` | `bbnf-simd/src/lib.rs` dispatch |
| `aarch64/byte_class_from_table_64.rs` | `bbnf-simd/src/dispatch.rs:68` |
| `aarch64/bitmap_prefix_xor_64.rs` (delegates to scalar per REDRESS 88) | Implicitly via dispatch table |
| `aarch64/bitmap_next_set_bit.rs` (delegates to scalar per REDRESS 89) | `bbnf-simd/src/dispatch.rs` |
| `aarch64/bulk_emit_positions_64.rs` | `bbnf-simd/src/dispatch.rs:72` |
| `aarch64/eob_pad_clamp.rs` | `bbnf-simd/src/dispatch.rs:73` |
| `aarch64/classify_tbl4.rs` | `runtime/src/grammars/json/scan.rs:214`, `bbnf-simd/src/dispatch.rs:24` |
| `aarch64/unescape_uxxxx.rs` | `parse-that-regex/src/lib.rs:402, 419` (`unescape_uxxxx_x4_neon` + `join_surrogate_pair_neon`). Note REDRESS 64+82 rejected the *single-quartet retained validator route*; the LIVE consumer is the materialization path in `unescape_four_unicode_escapes` — a different surface. |
| `aarch64/utf8/validate_block.rs` + `aarch64/utf8/mod.rs` | `parse-that-regex/src/lib.rs:491`, `parse-that-regex/src/unicode/utf8_block.rs:25` |

### 2.5 Scalar reference modules (KEEP)

`skinny/crates/bbnf-simd/src/scalar/` (7 files): all referenced as Lock 16 scalar oracles by either checkasm tests or aarch64 delegates. KEEP all.

### 2.6 dispatch / classifier / lib

`skinny/crates/bbnf-simd/src/{dispatch,classifier,lib}.rs`: active dispatch surface. KEEP.

### 2.7 Pre-existing fossil crate

| Path | Status |
|---|---|
| `skinny/crates/simd-scan/src/` | Empty directory; not in `skinny/Cargo.toml` `[workspace] members`. Confirmed dead per SK-V5 NUKE-PLAN.md Wave 4 ("NUKE fossil crate"). The decision is recorded; the empty directory remains. SAFE-TO-DELETE (zero LOC, no consumer). |

### 2.8 Code-triage rollup

| Class | Files | LOC | Backing REDRESS |
|---|---:|---:|---|
| SAFE-TO-DELETE x86_64 orphan kernels | 14 src + 8 test groups | ~440 src + ~80 tests | REDRESS 50-55 admission rule; REDRESS 80, 88 |
| SAFE-TO-DELETE aarch64 NEON `match_tiny_plain_string` | 1 src + 1 test group | 136 src + ~80 tests | REDRESS 28+33 + REDRESS 72 (scalar admitted, NEON orphan) |
| SAFE-TO-DELETE aarch64 utility orphans (quad_load, byte_context, digit_mac, cache_hints) | 4 src + 1 test file | 121 src + tests | REDRESS 50-55 admission rule; REDRESS 80 (digit_mac) |
| SAFE-TO-DELETE fossil crate dir | 1 dir | 0 LOC | SK-V5 NUKE-PLAN Wave 4 |
| KEEP-IF-USED (verify consumer chain) | 2 (string_block, movemask) | 97 src | parse-that-regex/utf8 consumer chain |
| KEEP (verified consumers) | ~17 aarch64/scalar/dispatch files | n/a | Lock 16 admitted primitives |
| **Total proposed code reduction** | **~19 src + ~10 test groups** | **~700 src + ~160 test** | — |

---

## §3 Path-pattern triage (commit 40a50c72 SK-V7 tranche-formalism sweep escape)

Fresh `rg -l 'GRAND-SYNTHESIS-SK\|IMPLEMENTATION-PACKET-SK\|SK-V\d+-COHORT\|HARDENING-ORCHESTRATOR\|RESEARCH-FOLD-ORCHESTRATOR'`: 46 files hit. Aggregate occurrences:

| Pattern variant | Files | Total hits |
|---|---:|---:|
| `GRAND-SYNTHESIS-SK-V5` | 4 | ~6 |
| `GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D` | 1 | 1 |
| `IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT` | 3 | 3 |
| `IMPLEMENTATION-PACKET-SK-V4-ASMJSON-BEAT` | 1 | 1 |
| `IMPLEMENTATION-PACKET-SK-V5` | 3 | ~4 |
| `IMPLEMENTATION-PACKET-SK-V6-SOTA-RECOVERY` | 2 | 2 |
| `SK-V5-COHORT` | 1 | 1 |
| `SK-V6-COHORT` | 1 | 1 |
| `HARDENING-ORCHESTRATOR` | 10 | ~25 |
| `RESEARCH-FOLD-ORCHESTRATOR` | 6 | ~10 |

By file class:

| Surface | Files affected | Recommendation |
|---|---|---|
| `restart/HANDOFF.md`, `restart/README.md` | 2 | Path-rewrite to current paths in next CRUD wave (sk-v7 RESTRUCTURE deferred-Omega bucket). |
| `restart/skinny/{INDEX,SUBSTRATE,SOTA-BEAT-DESIGN-via-shared}.md` | 3 | Path-rewrite (CRUD wave). |
| `restart/prompts/{sub-orchestrators,pass-contracts,audit-specs}/*.md` | 6 | These reference `HARDENING-ORCHESTRATOR` / `RESEARCH-FOLD-ORCHESTRATOR` as logical role names, not file paths. Verify each hit is role-naming (KEEP) vs file-path (CRUD-rewrite). |
| `restart/research/CORPUS-AUDIT-*.md`, `restart/research/V1-FOLD-CANDIDATES.md` | 4 | Same role-vs-path verification. |
| `restart/skinny/tranches/sk-v{3.5,5,6,7}/**/*.md` | 27 | Archive moves (per §1) will physically relocate these and reduce active-tree hits to zero. After archive, the dead refs only persist inside archived files referencing each other — acceptable. |
| `skinny/REDRESS.md` | 1 | Single hit; should be path-rewritten (live spec surface). |
| `skinny/crates/bbnf-simd/build.rs` | 1 | Single hit; verify it is a doc-comment reference (KEEP if so) vs functional. |

**Synthesis**: ~16 hits in *active* (non-archive) docs require CRUD-wave path rewrites; ~25 hits in the prompt/orchestrator-role corpus are role-naming and should be left alone after a verification pass; the remaining ~27 are inside tranches that will move to archive en bloc.

---

## §4 Archive hygiene proposal

### 4.1 Target structure

```
restart/skinny/
├── INDEX.md, SUBSTRATE.md, COMPILER.md, BENCH.md, WORKSPACE.md, HARDENING.md   (KEEP)
├── tranches/
│   ├── shared/SOTA-BEAT-DESIGN.md                                              (KEEP, current cross-tranche design)
│   ├── sk-v9/                                                                  (KEEP, active in-flight)
│   └── sk-v7/RESTRUCTURE.md                                                    (KEEP, load-bearing restructure record; leave at sk-v7 root for git-log continuity)
└── archive/
    ├── sk-v3.5/                                                                (6 research files moved verbatim)
    ├── sk-v5/                                                                  (4 top-level + research/ moved verbatim)
    ├── sk-v6/                                                                  (5 top-level + research/ moved verbatim)
    │   └── (optional sub-tier: research/primary/ + research/redress/ per sk-v7 RESTRUCTURE R1)
    ├── sk-v7/                                                                  (5 top-level minus RESTRUCTURE.md + research/ moved)
    └── sk-v8/                                                                  (4 top-level + research/ moved, with KEEP-IF-CITED holdback for alpha/ + HANDOFF.md until SK-V9 closes)
```

### 4.2 Rationale

- `restart/skinny/archive/` separates lived authority (current tranche surfaces + cross-cutting design) from historical authority (closed tranches). Inbound REDRESS citations and SK-V7 RESTRUCTURE traces are preserved verbatim via `git mv` (no path-rewrite inside archived files; rewrites occur only in the live-surface docs identified in §3).
- `sk-v7/RESTRUCTURE.md` stays at `tranches/sk-v7/` rather than the archive so the chain `RESTRUCTURE.md → archive/sk-v{3.5,5,6}/` is one hop. Optionally, move it to `tranches/shared/RESTRUCTURE-SK-V7.md` to put it adjacent to SOTA-BEAT-DESIGN.md.
- The `restructure-R1` recommendation of `archive/sk-v6/cohort/` + `archive/sk-v6/cohort-redress/` sub-tiering inside the sk-v6 archive is admissible but not required for first-pass cleanup; first-pass archive can use one flat `sk-v6/research/` directory.

### 4.3 Sequence (recommendation, not execution)

1. Path-rewrite §3's ~16 active-doc hits FIRST (preserves `git log --follow` rename detection on the moved files; per sk-v7 RESTRUCTURE).
2. Verify §1.7's two KEEP-IF-CITED items (sk-v8 HANDOFF.md, sk-v8 research/alpha/) against active SK-V9 dispatch — promote to KEEP or demote to ARCHIVE-MOVE as the inbound trace dictates.
3. `git mv` per §4.1 layout.
4. CHALLENGE pass: re-grep §3 patterns; report any residual hits in non-archive paths.
5. Run §2's SAFE-TO-DELETE crate deletions in a SEPARATE commit (decoupled from the doc archive so any test regression is bisectable).

---

## §5 Aggregate counts

### Doc corpus

| Class | Files |
|---|---:|
| KEEP (active) | 73 |
| KEEP-STALE (active surface with dead links) | 2 |
| KEEP-IF-CITED (verify before move) | 2 |
| ARCHIVE-MOVE | 524 |
| DELETE | 0 |
| **Total triaged** | **601** |

### Code corpus

| Class | Files | Approx LOC |
|---|---:|---:|
| SAFE-TO-DELETE (REDRESS-backed) | ~19 src files + corresponding test groups + 1 fossil dir | ~700 src + ~160 test |
| KEEP-IF-USED (verify consumer chain) | 2 | 97 src |
| KEEP (verified production consumers) | ~17 src files + scalar/ + dispatch/lib | n/a |

### Path-pattern hits

| Class | Files |
|---|---:|
| Active-surface CRUD rewrite required | ~16 |
| Role-naming false positives (likely KEEP) | ~25 |
| Inside archive-bound docs (will move en bloc) | ~27 |

---

## §6 Risks (looks-dead-but-isn't)

### R1 — `string_block::scan_string_special_block` (parse-that-regex consumer)

REDRESS 61/62/83 rejected this primitive as a generated-retained trusted-string scanner wrapper. The CURRENT consumer at `parse-that-regex/src/lib.rs:472, 551` is `skip_string_plain` / `skip_string_plain_trusted` — a UTF-8-validating plain-string scanner used by `match_string_at_quote_trusted_utf8`. The generated runtime calls `match_string_at_quote_trusted_utf8` at `runtime/src/grammars/json/generated.rs:193`. **VERDICT: KEEP**. The REDRESS rejection is about wiring `scan_string_special_block` as a *retained generated parser* hot-leaf wrapper (different call site); the parse-that-regex utf8-validating consumer is the LIVE admitted shape.

### R2 — `unescape_uxxxx_x4_neon` (parse-that-regex consumer)

REDRESS 64+82 rejected the *single-quartet retained validator route*. The LIVE consumer at `parse-that-regex/src/lib.rs:402, 419` is `unescape_four_unicode_escapes` inside the materialization path (4-unit packed decode + surrogate-pair join). **VERDICT: KEEP**. The 4-unit decode is the admitted materializer shape, not the rejected validator wrapper.

### R3 — `aarch64::movemask::movemask_u8x16` internal reuse

Only test caller at `tests/aarch64_primitives.rs:28`, BUT the module is `pub use`d into `aarch64/utf8/validate_block.rs:3` as a private intra-crate helper. **VERDICT: KEEP**. Deleting `movemask.rs` breaks `utf8::validate_block` which has LIVE parse-that-regex consumers. The local copy of `movemask_u8x16` inside `match_tiny_plain_string.rs` is independent — that file can still be deleted.

### R4 — sk-v6 SYNTHESIS-WAVE-1-PLAN.md is REDRESS-cited

REDRESS entries 65/66/67/68/69 cite "`restart/skinny/tranches/sk-v6/SYNTHESIS-WAVE-1-PLAN.md` §9/§10/§11/§12" as falsifiability anchors. The citations are STATIC (already folded into REDRESS prose; the file is a quoted §). **VERDICT: ARCHIVE-MOVE**. The citations remain pointing at `restart/skinny/archive/sk-v6/SYNTHESIS-WAVE-1-PLAN.md` after the move, which is acceptable; REDRESS path-rewrites are deferred-Omega.

### R5 — sk-v8 alpha/ and HANDOFF.md are KEEP-IF-CITED

SK-V9 has its own `alpha/` cohort (also 6 files alpha-A..F). It is plausible that SK-V9 SYNTHESIS or DISPATCH-PROMPT cites the sk-v8 `alpha/` directly (e.g., for cohort-digest continuity). Before moving sk-v8 alpha/ to archive, grep `restart/skinny/tranches/sk-v9/` for any `tranches/sk-v8/research/alpha` or `sk-v8/HANDOFF` references and verify. **VERDICT: hold pending verification**.

### R6 — `digit_mac.rs` parse_4_digits has multiple test refs

`aarch64::digit_mac::parse_4_digits` and `dot4_i8` have three test refs but ZERO production callers. REDRESS 80 rejected the mantissa-widen / Eisel-Lemire scale family with zero measured fallback rate on canada. **VERDICT: SAFE-TO-DELETE**. The tests are testing the orphan kernel itself, so they also delete.

### R7 — parse-that-regex `validate_utf8_prefix` / `validate_utf8_codepoint` lifetime

These functions exist in parse-that-regex (called by `skip_string_plain` at lines 482/498/517/537). They are NOT in the REDRESS-rejected list. They depend on `bbnf_simd::aarch64::utf8::validate_block`. The full chain is LIVE. No risk from §2's proposed deletions.

### R8 — `unimplemented!()` shells aren't compiled in release

Several x86_64 `unimplemented!()` bodies are gated on `#[cfg(all(target_arch = "x86_64", target_feature = "..."))]`. On aarch64 (current dev host) they are absent from the binary, which is why nobody has noticed they panic. On x86_64 with the relevant target features ENABLED in user builds, the panic would fire at runtime. **VERDICT: SAFE-TO-DELETE is correct and also improves x86_64 build correctness**.

### R9 — bbnf-simd `build.rs` mentions `HARDENING-ORCHESTRATOR`

Single hit in `skinny/crates/bbnf-simd/build.rs`. If it is a build-time string emit (e.g., `cargo:rerun-if-changed=...`), it is functional and must be path-rewritten alongside §3. If it is a doc-comment, KEEP. **VERDICT: verify before §3 CRUD wave**.

---

## §7 Closing notes (not execution)

This manifest is read-only triage. The orchestrator decides which classes (ARCHIVE-MOVE en bloc / SAFE-TO-DELETE crate cleanup / CRUD path-rewrite) execute and in what order. Recommended order: §3 path rewrites first → §1 archive moves → §2 crate deletions in a separate bisect-safe commit. Hard caps and CHALLENGE per dispatch protocol.

No `cargo test` / `cargo build` was run during this audit (read-only contract). Any execution path must validate `cargo test --workspace --profile ax-iter` and `cargo run -p xtask --release -- check-json` after the §2 deletions to confirm REDRESS-backed orphan removal does not break the differential checkasm parity gates.
