# LOCK-9 — SCANNER & KERNEL PLANE · PASS-3 RESEARCH DIRECTIVES

Steering questions only — pose, do not conclude. Every measurement names its HEAD, probes base-pin to a
stated commit (XL-H), skinny@HEAD is the substrate. Report answers as data (insn/reduction-op counts,
coverage %, distributions), not verdicts.

## F1 — Shared-vs-own kernel home & the canonical producer surface
*Do not re-derive: cross-crate `#[inline]` across the parse-that↔bbnf-simd rlib boundary is measured-live
(0 in-loop indirect); de-facing a facade is bbnf-simd-internal hygiene scoped to one route, not a perf
lever; the canonical movemask producers (contract-split — LEAN nibble-`vshrn` / DENSE dense-64 `vpaddq`
tree) are absent from bbnf-simd and must be authored once into any home regardless of ownership.*
- After authoring those producers into a scratch shared home (and purging any refuted reduction idiom
  there), what is the residual LOC/substrate divergence between one-home and two-home layouts across ALL
  membership + dense scan sites? Enumerate concrete deltas (padded-view/clamp conventions, single- vs
  multi-table shuffle, capacity constants); both directions valid; state the maintenance/drift-cost basis.
- Under which config (cgu count, LTO mode) does `#[inline]` fail to cross the crate seam — where would a non-inlinable boundary actually appear? Name it precisely, with evidence.
- Scanner-file mapping census (owed): classify each of the ~8 scanner files 1:1-kernel | vector-tier | no-go, quantifying the kernel-tier/vector-tier split.
- Evidence: static disasm insn + reduction-op counts per-site, base-pinned; paired cold ratios name N,
  both windows, their HEAD.

## F2 — Native aarch64 rebirth: stable toolchain + correctness oracle
- Does the reborn kernel surface build and pass on stable `core::arch::aarch64` with `portable_simd` and nightly-only deps removed? What is the exact blast radius of the smallbox nightly→stable cut?
- What differential oracle bit-parity-checks an emitted mask vs the scalar baseline (INJECT_BUG rate,
  alignment sweep, black_box inputs)? Does a naive nibble-stride form on the dense-64 site corrupt parity?
- Evidence: stable-toolchain build log; differential parity-harness results.

## F3 — Scanner derived from a DFA
*Cross-lock (XL-C, settled): canonical regex engine home is parse-that (one-regex-home); grammar tier keeps
`Regex(StringId)` opaque, engine tier keeps rich Hir joined by a string/format bridge (no single-HIR
dependency). Vendored parse-that-regex inside skinny is the substrate; top-level `crates/bbnf-regex` is
legacy (archives at GN-0).*
- Does a byte-fixpoint-gated grammar→DFA pipeline at scanner fidelity exist / is it buildable (LOCK-5)? Given two crates share `bbnf-regex 0.1.0`, which source is the authoritative DFA source-of-truth, and how is that name+version collision resolved?
- Census a rich-CSS `url()` leaf and a wide-unicode leaf sourced from `bootstrap/regex.bbnf`, reporting
  classes/states over the 255-class / 4096-state ceilings as denominators: what coverage % do the tractable
  tiers reach? (A small-multistate serial `vqtbl1q` chain is a measured negative on the short runs these
  scanners see — weight membership + structural tiers.)
- Can a `[u8;256]`→SIMD classifier be emitted (emit pass + partition/separability selection) matching the
  hand classifier, and does a naive nibble-only emit misclassify the structural set?
- Evidence: grammar-sourced census with over-ceiling denominators; if a classifier is lowered, otool
  reduction-insn ≤ hand + paired ≥0.98×, base-pinned.

## F4 — Windowed vs streaming input surface
- Is there ANY in-wave streaming consumer for parse-that's shipped grammars (chunked feed/reader), or does
  every grammar build a whole owned tree? (Whole-document-granularity streaming does not reduce peak tree
  RSS — say so if that is the only surface available.)
- At the parser's restore sites over JSON+CSS, what is the rewind distance (`furthest_offset − offset`) distribution — P50/P99/max? Is backtrack bounded (a bounded ring buffer suffices) or unbounded?
- Evidence: rewind histogram (diagnostics on/off, P50/P99/max, base-pinned); grep census of the streaming
  surface (`feed|push_bytes|Decoder|BufRead|document_stream|from_reader`).
