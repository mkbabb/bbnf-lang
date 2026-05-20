# SK-V12 P3-A: Candidate Shortlist

Pass: S-P3 Synthesis-Plan. Cycle: PIN-V2.
Date: 2026-05-20.
Scope: regenerate the pin-aware candidate shortlist from the converged S-P2
cohort, superseding the stale pre-pin V5 packet.
Output: this file.
Pass Alpha goalset: ADMIT requires a generated CSS L4 row whose generated
Track 1 throughput is strictly greater than `lightningcss_mbps + 1` on the
same corpus, same output plane, same host, with strict equality and independent
oracle/Track 2 evidence; FIXPOINT requires measured CSS redress plus at least
one new measured union-substrate attempt and one new measured ASM-gen attempt.
Candidate pool: `research/p2/` post-CHALLENGE survivors under the
2026-05-20 user pin.

## Section 1 - Synthesis

The user pin resets P3-A around one first-class target: generated CSS L4. The
old V5 shortlist treated CSS, Sheets, and BBNF-self as preflight-equivalent and
used `ceil(baseline_mbps * 1.01)` for intervention admission. Both are now
wrong. CSS L4 is authoritative; Sheets and BBNF-self are fallback-only after a
measured CSS redress attempt; the CSS close bar is generated Track 1
`> lightningcss_mbps + 1`, not a bbnf baseline lift.

S-P1 supplies JSON hot-family antecedents and, more importantly, records the
absence of a generated CSS runtime, same-plane lightningcss comparator, and
strict CSS oracle row. S-P2 converged on that boundary. Its selectable
performance candidates are grammar-neutral primitives only when exercised by
the generated CSS row: byte-set/class masks, generated FIRST/follow dispatch,
bounded string/escape spans, hex decode, digit spans, and a CSS fact-stream
output plane. P2-D contributes no current shortlist-ready union primitive, but
the user pin unblocks the union category; a CSS-local same-tape fact route is
eligible after the CSS baseline exists and after it cites REDRESS 96/97/98
with a material differential. P2-C/P2-B likewise make ARMv9.2/TBL/UDOT/PMULL
and CSSC CTZ routes eligible only with scalar reference, checkasm/parity,
micro-proof, and a same-wave generated consumer; REDRESS 88/89/90 remain
historical evidence, not category blockers.

Two prerequisites are load-bearing enough to be shortlisted because they decide
whether any later CSS or SIMD admission is legal:

- `GrammarConfig` or equivalent generated metadata must resolve the seven
  Lock-14 leaks before CSS L4 emission can leave the JSON templates.
- `escape_mask_64` must reproduce the xorshift falsifier and pass strict
  scalar/checkasm/corpus parity before any new SIMD admission.

The shortlist below is intentionally eight items. C1 is the first target and
the only direct ADMIT candidate in this pass. C2 and C3 are legality gates. C4
is the required D3 union-substrate attempt category. C5-C7 are row-relevant
CSS SIMD/ASM candidates. C8 is a fallback envelope that stays dormant until a
CSS redress attempt is measured.

## Section 2 - Deliverable

| ID | Candidate | Status | Gate row |
|---|---|---|---|
| C1 | CSS L4 generated admission row + lightningcss comparator | First target; ADMIT-capable | `css_l4/declaration_values/direct_to_struct/main` |
| C2 | `GrammarConfig` / generated metadata legality | Required before C1 source emission | JSON guards + Lock 14 scan |
| C3 | `escape_mask_64` correctness unblocker | Required before C5-C7 SIMD admission | checkasm/corpus parity |
| C4 | CSS-local same-tape fact union | D3-unblocked, post-C1 | selected CSS row |
| C5 | ARMv9.2 TBL/TBX byte-class + ASCII set-run skip | SIMD row candidate, post-C1/C3 as needed | selected CSS row |
| C6 | ARMv9.2 string/escape scanner + hex quartet x4 | SIMD row candidate, post-C1/C3 | selected CSS row |
| C7 | ARMv9.2 UDOT digit-run span | SIMD row candidate, post-C1/C3 | selected CSS row |
| C8 | Post-CSS-redress fallback baseline envelope | Dormant until measured CSS redress | Sheets, then BBNF-self |

### C1 - CSS L4 Generated Admission Row + Lightningcss Comparator

- Owner paths: `grammar/css/l4/{tokens,values,value-unit,properties}.bbnf`,
  `skinny/crates/codegen/src/lib.rs`,
  `skinny/crates/codegen/src/json_provider.rs`,
  `skinny/crates/codegen/src/json_templates/`,
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`,
  `skinny/crates/runtime/src/lib.rs`,
  `skinny/crates/bbnf-bench/src/report.rs`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs`,
  `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`,
  `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`,
  `skinny/crates/bbnf-bench/Cargo.toml`,
  `restart/skinny/tranches/sk-v12/research/w1b/css_l4_declaration_values.css`,
  and the companion report
  `restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json`.
- Scalar/reference state: no admitted CSS Track 1 parser exists. The generated
  CSS parser is the Track 1 reference for the row, and it must be generated
  from CSS grammar metadata after C2, not hand-authored or cloned from JSON
  policy. The oracle/Track 2 must be same-plane, strict, independent, and fresh.
- Checkasm/parity state: N/A for the scalar scaffold. If C1 calls any
  SIMD/ASM-backed helper, C3 / W2 must already be green. C1 parity is strict CSS
  fact-stream equality between generated Track 1, independent oracle/Track 2,
  and lightningcss-derived facts.
- Same-wave consumer: the `sk-v12-nonjson-generated-v1` companion gate and
  Criterion row consume the generated CSS runtime, the oracle, the lightningcss
  comparator, equality artifact, and provenance in the same wave.
- Falsifiability gate: `css_l4/declaration_values/direct_to_struct/main`
  generated Track 1 must be strictly `> lightningcss_mbps + 1`; strict equality
  must pass; oracle/Track 2 Mbps must be finite and same-run; sample count must
  be at least 30; host/build/feature/run/provenance fields must be gate
  consumed. `report.rs` currently still contains an intervention
  `ceil(baseline * 1.01)` branch; C1 must replace or bypass that for CSS
  admission with the lightningcss floor.
- LOC/risk: high, <=620 hand LOC if split after C2; generated LOC must be
  tracked with an O(N) grammar-size budget. Risk is high because it touches
  codegen/runtime/bench/gate and must preserve JSON guard rows.
- Pre-blocked routes by reference: REDRESS 111 report-only lane as baseline,
  REDRESS 112/113 generated non-JSON blocker/future promise, JSON provider
  cloning, hand-only CSS parser, parse-only SOTA admission, and any
  baseline-mbps close bar.

### C2 - `GrammarConfig` / Generated Metadata Legality

- Owner paths: new or equivalent `skinny/crates/runtime/src/tape/grammar_config.rs`,
  `skinny/crates/runtime/src/tape/mod.rs`,
  `skinny/crates/runtime/src/tape/assembler.rs`,
  `skinny/crates/runtime/src/grammars/json/`,
  `skinny/crates/codegen/src/json_templates/`,
  `skinny/crates/codegen/src/lib.rs`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs`,
  `skinny/crates/bbnf-bench/src/report.rs`, and `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- Scalar/reference state: the reference is compile/runtime parity of the
  existing JSON generated parser plus generated metadata dumps for structural
  alphabet, FIRST/follow tables, escape policy, number policy, flag scheme,
  sink/view/kind templates, and output plane. No SIMD scalar is involved.
- Checkasm/parity state: N/A. Lock-14 proof is negative and executable:
  generic crates carry no `JsonParser|CssL4Parser|GoogleSheetsParser` grammar
  branches, no grammar-named generic modules, and no JSON structural alphabet
  in shared templates.
- Same-wave consumer: C1's CSS emission path. C2 is not a row mover alone and
  must not close as "legalized later"; it is consumed when C1 emits and gates a
  generated CSS row.
- Falsifiability gate: `cargo xtask check-json`, `cargo xtask check-real-typed`,
  the Lock-14 grep/gate, and the admitted JSON direct/typed guard floors hold.
  If C2 changes generic runtime/codegen output, JSON guards refresh or REDRESS
  records measured demotion.
- LOC/risk: medium-high, <=360 hand LOC. Risk is hidden coupling: renaming
  JSON templates as generic would fail C2.
- Pre-blocked routes by reference: generic JSON policy leaks, new directive /
  BIR variant / BackendShape expansion, public substrate API, hand-authored
  per-grammar runtime shortcuts, and REDRESS 70/71 benchmark-private typed
  sink routes.

### C3 - `escape_mask_64` Correctness Unblocker

- Owner paths: `skinny/crates/bbnf-simd/src/lib.rs`,
  `skinny/crates/bbnf-simd/tests/checkasm_*`,
  `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`, and the current consumer
  `skinny/crates/runtime/src/grammars/json/scan.rs`.
- Scalar/reference state: existing scalar carry semantics are the behavioral
  oracle, but C3 must make the state handoff explicit for
  `(bs_mask, carry_in) -> (escaped_mask, new_carry)`.
- Checkasm/parity state: required. It must reproduce the xorshift seed
  `0xCAFEF00DBAADF00D`, iter 0, 128-byte JSON-pool falsifier, run finite
  carry/mask sweeps where practical, pass `BBNF_SIMD_STRICT=1`, and preserve
  17-corpus JSON parity.
- Same-wave consumer: the existing JSON scan caller remains the correctness
  consumer; C3 is a correctness admission, not throughput credit. Future C5-C7
  consumers may dispatch only after C3 is green.
- Falsifiability gate: strict checkasm and corpus parity PASS; no CSS or JSON
  throughput row can admit from C3 itself.
- LOC/risk: medium, <=180 hand LOC. Risk is correctness regression in current
  JSON scan state handoff.
- Pre-blocked routes by reference: any SIMD admission before C3 is green;
  REDRESS 28/33/88/89-style primitive claims based on benchmark speed while
  parity is failing.

### C4 - CSS-Local Same-Tape Fact Union

- Owner paths: `skinny/crates/runtime/src/tape/`,
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`,
  `skinny/crates/codegen/src/lower/`,
  `skinny/crates/codegen/src/json_templates/` or successor generated templates,
  `skinny/crates/bbnf-bench/src/report.rs`, and `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- Scalar/reference state: missing until C1 creates a scalar/generated CSS fact
  stream and same-plane oracle. The reference must be same-tape opaque facts
  derived by generated CSS metadata, not a structural-position vector.
- Checkasm/parity state: N/A unless the fact emission uses a SIMD helper.
  Required parity is strict CSS fact-stream equality, retained tape/fact
  equality, and JSON guard parity.
- Same-wave consumer: C1's CSS fact-stream comparator or a retained CSS
  fact-view walker that reads the same tape in the same wave.
- Falsifiability gate: selected CSS row remains strictly
  `> lightningcss_mbps + 1`; strict equality passes; JSON guard floors hold;
  no second retained substrate appears. If C4 is part of a FIXPOINT close
  rather than ADMIT, it still must record fresh profile, microbench, equality,
  and REDRESS evidence.
- LOC/risk: high, <=420 hand LOC. Risk is reintroducing a class column,
  cursor list, whitespace bitmap, or side vector under a new name.
- Pre-blocked routes by reference: REDRESS 96 class-column substrate,
  REDRESS 97 streaming cursor, REDRESS 98 class-lane-only paper close, and
  Lock 1 sidecar/parallel-substrate routes. USER PIN D3 unblocks the category
  only with material differential: CSS fact-stream, same-tape, generated-owned,
  no parser-owned cursor.

### C5 - ARMv9.2 TBL/TBX Byte-Class + ASCII Set-Run Skip

- Owner paths: `skinny/crates/parse-that-regex/src/lib.rs`,
  `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs`,
  `skinny/crates/bbnf-simd/src/scalar/byte_class_from_table_64.rs`,
  `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs`,
  `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_table_64.rs`,
  `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs`,
  `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`,
  `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_table_64.rs`,
  and the generated CSS runtime caller.
- Scalar/reference state: eq-set and table scalar references exist; a generic
  `pt_byte_set_run_skip` scalar reference must be promoted for row movement.
- Checkasm/parity state: existing eq-set/table checkasm is reusable, but CSS
  byte sets, tails, alignment, high-bit bytes, duplicate/empty sets, and
  low-6 collision cases must run in strict mode before production wiring.
- Same-wave consumer: generated CSS layout/trivia skip, delimiter dispatch,
  string-interesting scan, or FIRST-set branch in the selected CSS row.
- Falsifiability gate: selected CSS row remains strictly
  `> lightningcss_mbps + 1`; strict equality passes; caller-local microbench
  proves the selected CSS hot loop beats its scalar/reference path; JSON
  guards hold. C5 may use CSSC CTZ only as local first-nonmember/first-set-bit
  support with REDRESS 89 material differential, not as global bulk emission.
- LOC/risk: medium, <=300 hand LOC if it reuses existing helpers. Risk is
  orphaning a classifier or retaining masks as a structural side stream.
- Pre-blocked routes by reference: retained class stream, sidecar cursor,
  JSON structural alphabet in generic code, REDRESS 89 global CTZ/bulk route,
  and any new aarch64 body without same-wave consumer.

### C6 - ARMv9.2 String/Escape Scanner + Hex Quartet x4

- Owner paths: `skinny/crates/parse-that-regex/src/lib.rs`,
  `skinny/crates/parse-that-regex/src/unicode/`,
  `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`,
  `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`,
  `skinny/crates/bbnf-simd/src/aarch64/byte_context.rs`,
  `skinny/crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs`,
  `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs`,
  `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`,
  `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs`,
  `skinny/crates/bbnf-simd/tests/checkasm_bitmap_prefix_xor_64.rs`,
  and the generated CSS string/identifier/url/escape caller.
- Scalar/reference state: 16-byte string-block and x1 hex references exist;
  64-byte string scan, escaped-segment stream, and x4 hex scalar oracles are
  missing and must be added before admission.
- Checkasm/parity state: required for 64-byte string masks, x4 hex decode,
  byte-context handoff, and any PMULL prefix-XOR support. C3 must be green
  first if escape masks participate.
- Same-wave consumer: generated CSS strings, identifiers with escapes, `url()`
  spans, hex colors, or declaration-value fallback tokens. JSON unicode rows
  are guard evidence only.
- Falsifiability gate: selected CSS row remains strictly
  `> lightningcss_mbps + 1`; strict equality passes; caller-local microbench
  beats scalar/reference for the selected CSS string/escape caller; JSON
  guards hold. PMULL is admissible only as a narrow D4 consumer inside this
  caller, with REDRESS 88 material differential: not the default
  `bitmap_prefix_xor_64` body, not parse-only JSON, feature-gated fallback, and
  row measurement.
- LOC/risk: high, <=430 hand LOC. Risk is repeating REDRESS 106 broad
  string-proof regression or REDRESS 107/108 proof-only x4 reuse.
- Pre-blocked routes by reference: REDRESS 54/55/60-72 decoded scratch/stats,
  REDRESS 82 per-quartet parser-owned classifier, REDRESS 88 PMULL default
  body, REDRESS 106 full-string aggregate proof miss, REDRESS 107 proof-only
  x4, and REDRESS 108 no-source production reuse.

### C7 - ARMv9.2 UDOT Digit-Run Span

- Owner paths: `skinny/crates/parse-that-regex/src/number/mod.rs`,
  `skinny/crates/parse-that-regex/src/number/`,
  `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs`,
  `skinny/crates/bbnf-simd/tests/aarch64_primitives.rs`,
  new or extended digit-span checkasm tests, and the generated CSS number /
  dimension / percentage / calc caller.
- Scalar/reference state: `parse_4_digits` scalar fallback and parse-that
  digit scanners exist; a public digit-run span/accumulate oracle covering
  CSS caller policy is missing.
- Checkasm/parity state: partial smoke exists only. C7 needs invalid-digit
  lane sweeps, mixed valid/invalid groups, tails, truncation/overflow,
  alignment, leading-dot handoff, and CSS/JSON/Sheets/BBNF numeric slices.
- Same-wave consumer: generated CSS `<number>`, dimensions, percentages,
  keyframe stops, color alpha, or function numeric argument parser. JSON
  numeric rows are guard evidence only.
- Falsifiability gate: selected CSS row remains strictly
  `> lightningcss_mbps + 1`; strict equality passes; caller-local microbench
  beats scalar/reference for the selected CSS numeric caller; JSON guards hold.
- LOC/risk: medium, <=260 hand LOC. Risk is reopening REDRESS 114 numeric
  direct closure by using JSON numeric slots rather than CSS number policy.
- Pre-blocked routes by reference: REDRESS 80/114 JSON numeric direct routes,
  host f64/mantissa semantic changes, and orphan `digit_mac` smoke proof
  without same-wave caller.

### C8 - Post-CSS-Redress Fallback Baseline Envelope

- Owner paths after eligibility: `grammar/google-sheets/google-sheets.bbnf`,
  `grammar/bbnf/*.bbnf`, generated runtime modules under
  `skinny/crates/runtime/src/grammars/sheets/` or
  `skinny/crates/runtime/src/grammars/bbnf_self/`,
  `skinny/crates/codegen/src/`, `skinny/crates/bbnf-bench/src/report.rs`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs`, and
  `skinny/crates/bbnf-bench/benches/nonjson_baseline.rs`.
- Scalar/reference state: fallback generated parsers must be generated from
  their grammar sources. `sheets_witness` is compile-only evidence and cannot
  be Track 1.
- Checkasm/parity state: N/A unless fallback calls C5-C7 helpers; inherited
  helper gates apply.
- Same-wave consumer: `sheets/formula/direct_to_struct/main` first, then
  `bbnf_self/grammar/direct_to_struct/main` only if Sheets is rejected or
  routed by measured evidence.
- Falsifiability gate: no fallback dispatch before a measured CSS redress
  attempt records BLOCKED or REJECTED evidence. If eligible, fallback uses the
  same `sk-v12-nonjson-generated-v1` strict equality/oracle/gate consumption
  discipline; it does not satisfy the CSS lightningcss ADMIT target unless the
  user pin is amended.
- LOC/risk: medium-high; Sheets scout estimates <=480 non-generated LOC for
  its own path, but this is not a CSS substitute. Risk is violating D1 by
  treating easier non-JSON proof as equivalent to CSS.
- Pre-blocked routes by reference: Sheets/BBNF-self before CSS measured
  redress, hand-only witness baseline, JSON provider clone, and any claim that
  fallback admission closes the pinned CSS L4 target without user amendment.

## Section 3 - Falsifiability Binding

The P3-C gate writer should bind these thresholds without reviving the old
baseline formula:

| Gate family | Required row(s) | Threshold |
|---|---|---|
| CSS ADMIT | `css_l4/declaration_values/direct_to_struct/main` | generated Track 1 strictly `> lightningcss_mbps + 1`; strict equality PASS; same-plane independent oracle/Track 2 fresh and finite; sample count >= 30 |
| CSS intervention ADMIT | same selected CSS row | still strictly `> lightningcss_mbps + 1`; caller-local microbench beats scalar/reference; no `ceil(baseline_mbps * 1.01)` close bar |
| CSS intervention FIXPOINT credit | same selected CSS row or post-CSS-redress guard row | measured source or microbench evidence, REDRESS material differential, strict equality/parity where wired, JSON guard disposition, and zero-orphan state where applicable; no ADMIT label unless the CSS lightningcss bar is met |
| C2 legality | JSON direct and typed guard rows | current guard floors hold or REDRESS records measured demotion |
| C3 correctness | checkasm/corpus parity | xorshift falsifier reproduced then fixed; `BBNF_SIMD_STRICT=1` PASS; 17-corpus JSON parity PASS |
| C4 union CSS ADMIT | selected CSS row + JSON guards | CSS row strictly `> lightningcss_mbps + 1`; no retained side substrate; REDRESS 96/97/98 material differential recorded |
| C4 union FIXPOINT credit | selected CSS row or post-CSS-redress guard row | measured implementation or accepted microbench rejection, REDRESS 96/97/98 material differential, strict equality/parity evidence, and guard disposition; not ADMIT unless CSS clears the lightningcss bar |
| C5-C7 SIMD/ASM CSS ADMIT | selected CSS row + JSON guards | CSS row strictly `> lightningcss_mbps + 1`; scalar reference/checkasm/microbench/same-wave consumer PASS; zero new orphans |
| C5-C7 SIMD/ASM FIXPOINT credit | selected CSS row or post-CSS-redress guard row | measured implementation or accepted microbench rejection, REDRESS 88/89/90 differential when adjacent, scalar/checkasm/microbench/same-wave consumer evidence, and zero-orphan disposition; not ADMIT unless CSS clears the lightningcss bar |
| C8 fallback | Sheets/BBNF-self row only after CSS redress | strict non-JSON generated gate PASS; does not close CSS ADMIT without pin amendment |

JSON guard floors carried from the pin-aware SYNTHESIS:

| Guard row | Track 1 floor | Track 2/oracle floor |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 18191 | 17431 |
| `apache_builds/direct_to_struct` | 11028 | 9996 |
| `marine_ik/direct_to_struct` | 8759 | 9248 |
| `unicode_basic/direct_to_struct` | 2253 | 2182 |
| `twitter/real_typed_struct` | 17385 | 15593 |
| `citm_catalog/real_typed_struct` | 29928 | 17321 |
| `apache_builds/real_typed_struct` | 8308 | 6754 |
| `github_events/real_typed_struct` | 11633 | 12029 |
| `update_center/real_typed_struct` | 11613 | 10150 |
| `mesh/real_typed_struct` | 9214 | 7739 |
| `marine_ik/real_typed_struct` | 11552 | 9894 |

## Section 4 - Pre-Blocked Routes

Still blocked for every candidate:

- `parse_only` SOTA admission; parse-only remains diagnostic.
- CSS admission against `ceil(baseline_mbps * 1.01)` or against bbnf-only
  baseline movement instead of lightningcss.
- Sheets or BBNF-self before a measured CSS L4 redress attempt.
- Hand-only CSS/Sheets/BBNF parsers, report fixtures, or witness modules as
  generated Track 1.
- Generic-crate JSON policy leaks, grammar-name branches, grammar-named modules
  in generic crates, new directive/BIR/BackendShape variants, public substrate
  APIs, or parser-owned sidecars.
- New SIMD/ASM admission before C3 resolves `escape_mask_64`.
- Orphan aarch64 primitives at close: `bitmap_prefix_xor_64`,
  `bitmap_next_set_bit`, `bulk_emit_positions_64`, `byte_context`, and
  `cache_hints` must be consumed, removed, or inventory-demoted with evidence.
- x86 work; SK-V12 remains aarch64/Apple Silicon only.

Category-unblocked but still REDRESS-bound:

- Union / event-model / class-column / streaming cursor / class-lane category
  under USER PIN D3. Any C4 implementation must cite REDRESS 96/97/98 and name
  the CSS same-tape material differential.
- ASM-gen PMULL/CSSC/canary-adjacent category under USER PIN D4. Any C5-C7 use
  of PMULL, CSSC CTZ, or adjacent bitmap support must cite REDRESS 88/89/90
  and name the local-consumer material differential.

Dropped from the PIN-V2 shortlist:

- LD4 interleaved classifier: no canonical interleaved stream, no scalar
  deinterleave oracle, and side-stream manufacturing would violate Lock 1.
- SHA3 EOR3/BCAX ternary fold: no P1 hot leaf names a concrete three-input
  formula and no scalar/body/consumer exists.
- PRFM/STNP cache hints: no parser primitive semantics or row-owned consumer;
  inventory/drop unless a later fact-stream writer proves identical output and
  row movement.
- JSON direct residual row closure: guard ledger only under REDRESS 119/120
  unless a later tranche presents fresh material evidence.

## Section 5 - Sources

- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`.
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`.
- `restart/skinny/tranches/sk-v12/HANDOFF.md`.
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`.
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`.
- `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`.
- `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md`.
- `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md`.
- `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`.
- `restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md`.
- `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md`.
- `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`.
- `restart/skinny/tranches/sk-v12/research/skv12-W1-A1-css-l4-preflight.md`.
- `restart/skinny/tranches/sk-v12/research/skv12-W1-A5-bench-oracle-gate.md`.
- `restart/skinny/tranches/sk-v12/research/skv12-W1-A7-sheets-execution-scout.md`.
- `restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md`.
- `restart/skinny/tranches/sk-v12/research/skv12-profile-truth-audit.md`.
- `restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md`.
- `restart/skinny/tranches/sk-v12/research/skv12-decision-engine-audit.md`.
- `restart/skinny/tranches/sk-v12/research/skv12-totality-fold-scout.md`.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md` through REDRESS 120.
