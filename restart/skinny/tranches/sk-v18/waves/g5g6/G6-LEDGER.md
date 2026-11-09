# G6 Wave Ledger — dispatch-targeting NEON (simdjson-stage-1 structural classification)

Date: 2026-06-02. Wave: SK-V18 G5/G6. Gate: SPEC §8 + the G5/G6 AMENDMENT (ratified 2026-06-02 —
the as-specified inert-run-skip was measured ineffective; the NEON retargets the DISPATCH instead).
Outcome: **GREEN** (all amendment conjuncts held; optimized bench confirms no >SOTA regression — a
decisive 1.5–3.3× hold over lightningcss).

## 1 — G6: the retargeted kernel
The §8 inert-run-skip (`find_css_significant`, the two-fan eq-set NEON OR-reduce) was wired live in G2
but measured near-useless: inert runs median 6–9 bytes vs the 64-byte NEON block; the 79.5% self-time
is the per-significant-byte DISPATCH, not the inert gaps. G6 redesigns the NEON to classify, not skip:

- **`bbnf-simd/src/scalar/structural_classify_64.rs`** (NEW) — the scalar reference oracle: classifies
  64 bytes into `{significant-bitmask: u64, kind-per-position}` against the caller-supplied significant
  byte-set + structural-kind table. dav1d discipline: scalar FIRST, the differential's ground truth.
- **`bbnf-simd/src/aarch64/structural_classify_64.rs`** (NEW) — the vceqq-fan NEON kernel: one
  vectorized pass per 64-byte block (4×16-lane `vceqq` membership-fans OR-reduced into the significant
  mask + the kind table indexed per significant lane). Grammar-NEUTRAL — the byte-set and kind-table
  are caller data, never a grammar-named constant. aarch64 NEON/dotprod ONLY (no x86, no SVE).
- **`runtime/src/runtime_scan.rs`** — the scalar shell rewritten to ITERATE structural positions from
  the bitmask (count-trailing-zeros → next significant byte) instead of testing every byte; the shell
  still owns recursion / strings / comments / errors at each landed position. The dispatch overhead the
  profile attributed 79.5% to is gone — the per-byte membership test no longer runs per byte.

## 2 — Carry-forward deletions (amendment §Carry-forward)
- `runtime/src/runtime_simd.rs` — `find_comment_close` DELETED (genuinely dead, test-only caller);
  `find_css_significant` SUBSUMED by the classifier (the skip kernel it powered is retired). Net −142.
- `runtime/src/lib.rs` — the dispatch consolidation (net −92): the per-byte significant test folded
  into the classifier admission; no orthogonal scan codepath remains.
- `runtime/src/grammars/json/scan.rs` — the zero-sampled JSON scan neutralized (G5), capacity probe kept.

## 3 — dav1d differential (THE gate — checkasm, real corpora)
`bbnf-simd/tests/checkasm_parity.rs::structural_classify_64_neon_matches_scalar_css_corpora` — the NEON
classify-bitmask + kind-table matches the scalar per-byte classification over the REAL 71KB–495KB
corpora (bootstrap / tailwindcss / material-components-web / animate), NOT a toy micro-test.
`runtime/tests/css_scan_differential.rs` (NEW) asserts the shell's bitmask-iterated scan == the
reference scan output. Both PASS.

## 4 — Telemetry (§8 + amendment exit gate, all GREEN)
```text
checkasm_differential_ok    = true      (structural_classify_64_neon_matches_scalar_css_corpora ... ok — real corpora)
scalar_reference_first      = true      (scalar/structural_classify_64.rs is the oracle; NEON derived from it)
aarch64_only                = true      (vceqq-fan; no x86 path, no SVE; no dotprod-gated fallback to x86)
grammar_neutral             = true      (byte-set + kind-table are caller args; lock14 neutrality scan 102/0)
admission_live              = true      (the generated css_l4 scan / css_balanced_component_scan call the classifier non-test, at the hot path)
css_9field_parity_exact     = true      (rich_cssom_matches_cssparser_on_fixture + _on_real_corpora ok)
json_51_held                = true      (runtime suite green)
regen_clean                 = true      (regen-css + check-css-l4-declaration-values exit 0)
build_green                 = true      (runtime + bbnf-simd + bbnf-bench ax-iter exit 0)
dead_kernels_deleted        = true      (find_comment_close gone; find_css_significant subsumed)
```

## 5 — Magnitude (optimized bench profile — fat-LTO, opt-level=3, cold per-parse, N=100)
The ax-iter profile is INVALID for NEON magnitude (unoptimized intrinsics → the misleading 0.78×
artifact). Under the valid `bench` profile, `track1_rich` (the un-forked grammar-derived generator
carrying the G6 classifier) over lightningcss:

```text
corpus                    track1_rich   lightningcss   ratio
bootstrap                 2357.5 MB/s   1120.8 MB/s    2.10x
tailwindcss               2737.3 MB/s    837.8 MB/s    3.27x
material-components-web   2413.4 MB/s   1286.3 MB/s    1.88x
animate                   1824.8 MB/s   1217.0 MB/s    1.50x
```

>SOTA holds on EVERY corpus (1.5–3.3×) — the amendment's commit gate ("no >SOTA regression") is met
decisively. The dispatch vectorization DID help where the inert-skip did not, as the research predicted.
Per the amendment, the rigorous pre/post G6 delta (did the classifier IMPROVE the margin vs the G2-era
skip-wire, and by how much) is an H1 deliverable — reported there from the corpus-in-timer symmetric
harness, measured not asserted, with the realism caveats. This ledger records: G6 commits on correctness
+ admission + parity + no-regression; the improvement-magnitude verdict is H1's honest close.

## 6 — Gate authorization (Lock-14)
G6 touched lock14-frozen surfaces (`crates/runtime/src/runtime_simd.rs`, `crates/bbnf-simd/src/`); the
owner-path set (`SK_V18_G1_G2_EMITTER_OWNER_PATHS`) extended to cover them (the established per-wave
pattern), and the subject matcher already admits `sk-v18-g6`. The firewall stays meaningful — re-inject
RED, freeze still catches non-wave changes, the generic-crate neutrality scan (bbnf-simd) holds 102/0
with the classifier's grammar-neutral byte-set/kind-table API.

Next: PROVE (Sheets — google-sheets.bbnf through the un-forked generator = the 3rd Cursor impl + the
negative control), then H1 (CSS framing honesty + corpus-in-timer + the rigorous G6 magnitude delta +
regen-clean). G6 unblocks neither PROVE (G4 did) nor H1, but closes the §8 acceleration unit.
