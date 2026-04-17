# AW-IV Emergency Profile — P1 json_monolithic

**HEAD**: `2ca0f7af` (post AW-IV.W1.4-aggro). **Binary**: `.profiles/shared-target/release/deps/json_monolithic-7dd5d16c2fddcae3` (675 712 B, Apr 17 18:00).

**Drift notes.** `wave.tsv` names stale `json_monolithic-dda276c29cb57503` (only `.dSYM` dir); `binaries.tsv` header-only. Live binary re-emitted after W1.4-aggro is `-7dd5d16c2fddcae3`; I consumed it. No `cargo bench` / `cargo expand` reruns. Bench group is `data_s` + `data_xl` (`crates/core/benches/json/monolithic.rs:49-53`); `--bench data` substring-matches both; the `data` artefact is a combined profile dominated 99.87 % by data_xl. Use §3.5 for clean data_xl attribution.

## 1. Bench numbers + cycle/byte

Source: `.profiles/samply/json_monolithic/<entry>/bench.txt`. Bytes = `input.len()` (`monolithic.rs:21-24`). cyc/B = ns/B × 3.5.

| entry    | ns/iter     | bytes      | MB/s  | ns/B | cyc/B |
| -------- | ----------: | ---------: | ----: | ---: | ----: |
| data_s   | 152 656     |     35 491 | 232.5 | 4.30 | 15.05 |
| twitter  | 2 734 801   |    631 514 | 230.9 | 4.33 | 15.16 |
| citm     | 6 355 393   |  1 727 204 | 271.8 | 3.68 | 12.88 |
| canada   | 16 849 762  |  2 251 051 | 133.6 | 7.49 | 26.20 |
| data_xl  | 120 879 662 | 21 281 177 | 176.1 | 5.68 | 19.88 |

(twitter 231 MB/s ≠ the 241 MB/s claimed in the prompt — the live binary still shows twitter flat around 231 MB/s on this run.)

## 2. Symbol-presence audit (`nm $BIN`)

Absent (fully inlined): `__dfa_match_*`, `scan_quoted_string_simd`, `decode_json_string_to_arena`, `emit_leaf`, `push_compound_fused`, `close_compound`.

Present (real call boundaries in hot path):

- `__dta_walker_inline::run` — hot entry, 22 008 B one-function body.
- `psi::write_decoded` @ `0xd9a8`
- `bbnf_tape::driver::advance_or_pop_with` @ `0x6124`
- `bbnf_tape::finaliser::finalise` @ `0xd3f4`
- `core::num::imp::dec2flt::lemire::compute_float<f64>` @ `0x19500`
- `PayloadJob`'s `RawVec::grow_one` @ `0x56fd4`

Re-runnable via `nm $BIN | grep -E '__dfa_match|scan_quoted|compute_float|write_decoded|advance_or_pop|finaliser|__dta_walker'`. W1.4-aggro's "DFA bodies inline" claim verified; W2.1's fused-builder inlining verified.

## 3. Per-entry top self-time tables

Self-time = innermost inlined frame owning each sample (RVA binary-search against `profile.json.syms.json::data[1].symbol_table`, inner frames last). Source: `.profiles/samply/json_monolithic/<entry>/profile.json.{gz,syms.json}`. Syslib rows (libsystem_platform/kernel stubs from mimalloc growth + unwinding; aggregate 2-4 %) omitted below; recoverable from raw artefact.

**3.1 data_s** — combined profile 99.87 % dominated by data_xl; use §3.5. Unique data_s signal is 232 MB/s (§1), matching twitter at same string-ratio.

### 3.2 twitter (631 514 B — 64 % strings) · 3 138 samples

| self% | samp | symbol |
| ----: | ---: | --- |
| 61.50 | 1930 | `__jsonparser_emit_impl::__dta_walker_inline::run` |
| 11.28 |  354 | `bbnf_tape::finaliser::finalise` |
| 10.42 |  327 | `bbnf_tape::driver::advance_or_pop_with` |
|  5.67 |  178 | `bbnf_tape::psi::write_decoded` |
|  4.11 |  129 | `<json_monolithic::JsonParser>::parse` (walker outer) |
|  1.66 |   52 | `<bbnf_tape::driver::FrameStack>::nearest_variant_frame` |
|  1.08 |   34 | `core::str::converts::from_utf8` |
|  0.25 |    8 | `<alloc::vec::Vec<u8>>::append_elements` |
|  0.16 |    5 | `<f64 as FromStr>::from_str` |
|  0.06 |    2 | `<bbnf_tape::dta::LiteralPayload>::write_le` |
| — | — | (syslibs aggregate ≈ 3.7 %) |

### 3.3 citm (1 727 204 B — 16 % strings, 71 % ws) · 2 645 samples

| self% | samp | symbol |
| ----: | ---: | --- |
| 56.82 | 1503 | `__jsonparser_emit_impl::__dta_walker_inline::run` |
| 14.67 |  388 | `bbnf_tape::driver::advance_or_pop_with` |
| 14.06 |  372 | `bbnf_tape::finaliser::finalise` |
|  4.16 |  110 | `<json_monolithic::JsonParser>::parse` (walker outer) |
|  3.14 |   83 | `bbnf_tape::psi::write_decoded` |
|  2.61 |   69 | `<bbnf_tape::driver::FrameStack>::nearest_variant_frame` |
|  1.25 |   33 | `core::str::converts::from_utf8` |
|  0.64 |   17 | `<f64 as FromStr>::from_str` |
| — | — | (syslibs aggregate ≈ 2.4 %) |

### 3.4 canada (2 251 051 B — 0 % strings, 90 % numbers) · 7 086 samples

| self% | samp | symbol |
| ----: | ---: | --- |
| 50.93 | 3609 | `__jsonparser_emit_impl::__dta_walker_inline::run` |
| 16.53 | 1171 | `bbnf_tape::driver::advance_or_pop_with` |
| 13.56 |  961 | `bbnf_tape::finaliser::finalise` |
|  4.81 |  341 | `<f64 as FromStr>::from_str` |
|  4.04 |  286 | `core::str::converts::from_utf8` |
|  3.10 |  220 | `<json_monolithic::JsonParser>::parse` (walker outer) |
|  2.38 |  169 | `<bbnf_tape::driver::FrameStack>::nearest_variant_frame` |
|  1.28 |   91 | `core::num::imp::dec2flt::lemire::compute_float::<f64>` |
|  0.78 |   55 | `bbnf_tape::psi::write_decoded` |
| — | — | (syslibs aggregate ≈ 2.3 %) |

### 3.5 data_xl (21 281 177 B — 42 % strings, 40 % ws) · 49 437 samples

| self% | samp | symbol |
| ----: | ---: | --- |
| 54.21 | 26802 | `__jsonparser_emit_impl::__dta_walker_inline::run` |
| 13.14 |  6497 | `bbnf_tape::finaliser::finalise` |
| 11.90 |  5884 | `bbnf_tape::driver::advance_or_pop_with` |
|  5.59 |  2763 | `<json_monolithic::JsonParser>::parse` (walker outer) |
|  4.92 |  2432 | `bbnf_tape::psi::write_decoded` |
|  2.62 |  1293 | `<bbnf_tape::driver::FrameStack>::nearest_variant_frame` |
|  1.86 |   922 | `<f64 as FromStr>::from_str` |
|  1.68 |   833 | `core::str::converts::from_utf8` |
| — | — | (syslibs aggregate ≈ 3.8 %) |

`JsonParser::parse` is the outer wrapper of `__dta_walker_inline::run` + `TapeBuilder::finish`; its self-time is the outer-frame prologue visible to the unwinder — effectively walker tail.

## 4. Category aggregation

Groups: *walker* = walker + outer `JsonParser::parse`; *tape_emit* = `advance_or_pop_with` (other fused-builder syms absent post-W2.1); *psi* = `psi::write_decoded` + `PayloadJob`; *finaliser* = `finaliser::finalise`; *f64* = `FromStr::from_str` + `lemire::compute_float`; *utf8* = `str::from_utf8` (the decode/scan syms are absent → inlined into walker); *fstk* = `FrameStack::nearest_variant_frame`; *vec* = `Vec::append_elements` + `LiteralPayload::write_le` + `RawVec::grow_one`; *alloc* = `mi_*`; *syslibs* = platform/kernel stubs; *misc* = residual.

| entry   | walker | tape_emit | psi  | finaliser | f64  | utf8 | fstk | vec  | alloc | syslibs | misc |
| ------- | -----: | --------: | ---: | --------: | ---: | ---: | ---: | ---: | ----: | ------: | ---: |
| data\*  | 59.62% |   12.02%  | 4.87%|  13.29%   | 1.79%|1.72% |2.41% |0.02% | 0.08% |  4.01%  |0.18% |
| twitter | 65.62% |   10.42%  | 5.67%|  11.28%   | 0.19%|1.08% |1.66% |0.32% | 0.03% |  3.66%  |0.06% |
| citm    | 60.98% |   14.67%  | 3.14%|  14.06%   | 0.64%|1.25% |2.61% |0.00% | 0.04% |  2.42%  |0.19% |
| canada  | 54.04% |   16.53%  | 0.78%|  13.56%   | 6.10%|4.04% |2.38% |0.00% | 0.04% |  2.34%  |0.20% |
| data_xl | 59.80% |   11.90%  | 4.92%|  13.14%   | 1.86%|1.68% |2.62% |0.03% | 0.04% |  3.81%  |0.19% |

## 5. Cross-entry quantification

Byte-class densities (`data/json/<file>`):

| file               | total B    | strings | struct | ws    | numeric |
| ------------------ | ---------: | ------: | -----: | ----: | ------: |
| data.json          |     35 491 |  63.1%  |   4.9% | 29.5% |   2.4%  |
| twitter.json       |    631 514 |  64.2%  |   4.8% | 26.1% |   2.0%  |
| citm_catalog.json  |  1 727 204 |  15.9%  |   5.4% | 71.0% |   7.3%  |
| canada.json        |  2 251 051 |   0.0%  |   9.9% |  0.0% |  90.1%  |
| data_xl.json       | 21 281 177 |  42.4%  |   6.7% | 40.0% |  10.7%  |

Workload signatures: string-heavy (twitter, data_xl) → walker peaks 61-66 %, *psi* 4.9-5.7 %, f64 near zero. Whitespace-heavy (citm) → *tape_emit* (advance_or_pop_with) peaks 14.7 %, PSI 3.1 %. Number-heavy (canada) → walker drops to 54 %, f64 total 6.1 %, utf8 4 % (number slices through `from_utf8_unchecked` before `f64::from_str`), PSI 0.8 %.

## 6. Findings for JSON-only prototype

- **(a) `psi::write_decoded` hot?** YES on string-heavy: twitter 5.67 % · data_xl 4.92 % · citm 3.14 % · canada 0.78 %. Symbol @ `0xd9a8`. Validates W2.3 PSI-elision for string-heavy; canada irrelevant.
- **(b) `scan_quoted_string_simd`?** ABSENT, fully inlined (cost absorbed into walker's 54-66 %).
- **(c) `eisel_lemire::compute_f64`?** PRESENT as libcore's `dec2flt::lemire::compute_float::<f64>` @ `0x19500` (not inlined). Canada: 1.28 % direct + 4.81 % `<f64>::from_str` dispatcher = 6.10 %. Hand-inlined Eisel-Lemire reclaims ≈6 % canada, ≈2 % data_xl.
- **(d) `TapeBuilder::finish` share?** Named `finaliser::finalise` @ `0xd3f4`; 11.28-14.06 % flat across all inputs. Flat on data_xl (13.14 %) rules out small-input amortisation — per-tape linear cost; the `derive_frame_depth` O(tape) post-pass is the fusion target.
- **(e) Byte-class ratios:** §5. canada = pure number emission; citm = pure structural-advance (71 % ws); twitter/data_xl = string emission with structural bookkeeping secondary.

## 7. One-line conclusions per entry

- **data_s**: 232 MB/s, 152 µs total — artefact contaminated by data_xl; verdict transfers from twitter (same string-ratio).
- **twitter**: walker 61.5 % + PSI 5.7 % dominate; ≈25 % recoverable via PSI elision + `#[inline(always)]` on `advance_or_pop_with` / `psi::write_decoded` / `finaliser::finalise` (collapses three cross-crate boundaries).
- **citm**: `advance_or_pop_with` 14.7 % (71 % ws advance-cost) + finaliser 14.1 %; ≈15-20 % recoverable via walker ws-run fast-path + finaliser single-pass fusion.
- **canada**: walker 54 % + f64 6.1 % + tape_emit 16.5 %; ≈6 % via inlined Eisel-Lemire, ≈8 % via `advance_or_pop_with` inlining. 26.2 cyc/B is worst of the five — hardest case for generalised trajectory.
- **data_xl**: walker 54 % + finaliser 13 % + tape_emit 12 % + PSI 5 %; scaled average of twitter+citm; lever set identical to twitter → ≈25-30 %.

## 8. Synthesis recommendation

Generalised AW-IV remains the right lever. The anticipated smoking gun (fused-builder syms as self-time) does NOT occur — W2.1 fused cleanly. The four remaining cross-crate `bbnf_tape` boundaries — `advance_or_pop_with` (10-17 %), `psi::write_decoded` (0-6 %), `finaliser::finalise` (11-14 %), `FrameStack::nearest_variant_frame` (1.7-2.6 %) — together account for **26-34 % of every parse across all workloads**. Forcing them inline (workspace LTO + `#[inline(always)]`, or per-grammar body emission) is the highest-leverage next step and does not require a JSON-only prototype. A JSON-only hand-tuned parser is only justified if, after those four inlinings, canada still cannot reach ≈400 MB/s — it stays f64-bound at 133 MB/s until an inlined Eisel-Lemire joins.

**Artefacts**: `.profiles/samply/json_monolithic/{data,twitter,citm,canada,data_xl}/` — seven required files each.
