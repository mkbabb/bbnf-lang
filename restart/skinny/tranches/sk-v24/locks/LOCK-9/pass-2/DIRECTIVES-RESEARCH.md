# LOCK-9 (Scanner & Kernel Plane, parse-that rust) — Pass-2 Research Directives

Steering questions only. Answer each with cited evidence (paths, disasm, instruction-counts, corpora),
never a verdict. Name a cross-lock predecessor's status (buildable/owned), not who should win; make no
"faster" claim in any gate.
## Shared settled facts (build on; do not re-litigate)
- Kernel-entry lever: a `OnceLock` fn-pointer facade blocks `#[inline]` under fat-LTO (indirect branch
  survives in-loop); a direct `#[cfg(target_arch="aarch64")]` entry ICF-folds to the existing inline body.
  This lever is orthogonal to whether one crate or two carry the kernels.
- Physical seam: parse-that `.cargo/config.toml [patch.crates-io]` path-deps bbnf/bbnf-regex/csp-solver;
  `bbnf-simd` is absent. parse-that is READ-ONLY to the bbnf-lang campaign (non-clobber P23) — treat any
  parse-that mutation as requiring a named cross-lock supersession; report it, never presume it granted.
- Movemask taxonomy: (a) nibble-stride `vshrn_n_u16::<4>`; (b) dense-64 `vpaddq` tree for the single 64-bit
  stripe assembling `classify_stripe_64`; (c) double-`vaddv` dense-16 is refuted, live at
  `number_simd.rs:54-55`. All intrinsics stable since 1.59; NEON is baseline-mandatory (no nightly).
## Kernel-home axis (9A one shared crate ⟂ 9B parse-that keeps its own)
- Mapping census: for each of the 8 hot scanner files, classify 1:1-to-kernel | vector-tier | NO_GO and
  quantify the kernel-tier vs vector-tier split. Evidence: file-by-file table + the deciding NEON body each.
- Cross-crate inline reality: does fat-LTO carry `#[inline]` across the `[patch]` path-dep on a LIVE hot
  scanner (not ICF in an isolated probe crate)? Evidence: `otool -tv`, zero indirect-in-loop.
- Divergence datum: after a native rebirth of two idiomatically-opposite kernels (a position scan, idiom
  (a); the dense-64 stripe, idiom (b)), is LOC/substrate divergence large or near-identical (axes:
  `PaddedView`-64 vs unpadded; `vqtbl1q_u8` single-table vs `vqtbl4q_u8`)? Evidence: LOC-duplicated count +
  substrate-divergence table + shared differential-conformance corpus as data. Both outcomes valid; no home verdict.
## Canonical movemask producer (newly opened formulation)
- Three producers narrow a compare to a mask: bbnf-simd `movemask_u8x16` (dense-u16 via `vshrn`+repack),
  the lean `shrn.8b→fmov→4-bit-stride` no-repack form, and parse-that `to_bitmask` sites. Is the repack
  load-bearing (does anything downstream require dense 1-bit-per-lane semantics), or would a lean
  nibble-stride producer eliminate it? Evidence: disasm of all three + an INSTRUCTION-COUNT comparison (not
  reduction-op count) of repack vs lean at a real classify site.
## Scanner-as-DFA (9C)
- Register budget for the ASCII archetype leaves is measured closed — ONE narrow item; it does not speak to
  the family. Open: (1) dense-classification parity — a DFA-lowered dense classify vs hand simdjson on the
  dominant every-byte pass; (2) a census INCLUDING the explosion candidate (rich-CSS `url()` / a wide-unicode
  leaf) sourced from `bootstrap/regex.bbnf`, yielding a measured coverage percentage; (3) class-map∘Sheng
  composition — does `state' = tbl(row[class], state)` incur a per-byte serial-dependency stall?
- Cross-lock predecessor (report status, do not assume it lands): which regex crate is the DFA
  source-of-truth — parse-that's in-repo fork (`bbnf-regex = { path = "regex" }`) or bbnf-lang
  `crates/bbnf-regex` (potentially TWO crates to reconcile)? Does LOCK-5 build the byte-fixpoint-gated
  grammar→DFA pipeline at the fidelity a scanner needs? Absent that substrate the family has no source.
## Lazy / windowed input-surface (9D)
- Cheap decider: instrument rewind distance (`furthest_offset − offset`) across JSON+CSS, with and without
  `diagnostics` recovery — bounded (< a small window) or unbounded? parse-that is a backtracking PEG; report
  whether its semantics give ANY bounded-backtrack guarantee. Evidence: measured distribution.
- Consumer: is there ANY in-wave chunked-feed streaming consumer for parse-that's grammars? (simdjson
  `document_stream` streams whole-document granularity, never a byte-partial document.) A consumer-less
  windowed substrate is a design defect, not a feature.
- Constraint: make no throughput claim — the straddle guard is expected neutral-to-slower. Only latency /
  time-to-first-structural is a candidate value, and must be MEASURED (peak-RSS + TTFS), never asserted.
