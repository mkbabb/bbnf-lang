# SK-V10 P2-D: Substrate + Tape Design

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-19.
Scope: interrogate the lazy offset tape, direct digest output plane, logical-vs-allocated tape ratios, and Lock 1 substrate-union boundary.
Output: this file.
P1 hot-leaf antecedents: `direct_struct`, `array_walk`, `object_walk`, `string_tiny_scan`, `string_full_scan`, `string_escape`, `unicode_escape_hex`, `number_digit_scan`, `number_scan`, `whitespace_skip`, `alloc`, `simd_movemask`.
Lock surface: Lock 1 primary; Lock 14 secondary where any contract surface must remain grammar-neutral.

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

1. The substrate answer is single-substrate: the current retained JSON document is already an offset tape plus sparse flags, and direct retained views borrow through that tape. `Tape` owns `source`, `offsets`, `flag_cursors`, `flag_values`, `payloads`, and `TapeId` in `skinny/crates/runtime/src/tape/mod.rs:94`; `ValueRef` holds `&Tape` plus cursor in `skinny/crates/runtime/src/tape/mod.rs:175`. `TapeBuilder` emits offsets and sparse flags into the same builder in `skinny/crates/runtime/src/tape/assembler.rs:42` and seals one `Tape` in `skinny/crates/runtime/src/tape/assembler.rs:115`.

2. Lock 1 forbids parallel substrates and says retained structural offsets are the tape, not a sidecar: `restart/locks/LOCKS.md:52`. SK-V10 inherits this as a hard boundary: W3 union substrate is retired as falsified, direct is the primary frontier, and parse-only SOTA is retired in `restart/skinny/tranches/sk-v10/HANDOFF.md:6`; refusal conditions reject reopening W3 or a renamed union substrate in `restart/skinny/tranches/sk-v10/HANDOFF.md:75`.

3. The lazy-materialization counters support the current offset-tape shape, not another tape-width change. Current rows publish zero payload bytes and 0/0 payload writes/allocations across Track 1 and Track 2, while logical/allocated tape ratios vary by corpus: twitter 0.19x logical and 0.21x allocated (`skinny/RESULTS.md:95`), canada 0.40x and 0.47x (`skinny/RESULTS.md:100`), mesh 0.44x and 0.72x (`skinny/RESULTS.md:113`), gsoc-2018 0.05x and 0.08x (`skinny/RESULTS.md:119`), and y_string_unicode 0.50x and 0.75x (`skinny/RESULTS.md:141`). The worst allocation ratios are capacity/shape pressure, not payload-arena pressure.

4. Direct digest is an output plane, not retained tape proof. `skinny/RESULTS.md:51` records `direct_to_struct` rows as `sink_only_digest`, with `n/a` retained materialization and `zero_or_inert` sidecar state. The digest itself is a scalar product-plane summary in `skinny/crates/bbnf-bench/src/direct_struct.rs:16`, Track 1 calls generated `parse_direct` in `skinny/crates/bbnf-bench/src/direct_struct.rs:401`, Track 2 calls the independent hand parser in `skinny/crates/bbnf-bench/src/direct_struct.rs:408`, and parity compares Track 1, Track 2, serde, and sonic shapes in `skinny/crates/bbnf-bench/src/direct_struct.rs:420`.

5. P1 does not name a uniform substrate ceiling. P1-E says live direct-plane losses are primitive/call-site specific, not one substrate ceiling (`restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:78`). The hot-leaf map names string, unicode, number, whitespace, array/object walk, digest fold, alloc, and movemask classes (`restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:36`), while P1-F makes direct digest the primary JSON frontier with 14 `N-direct / NO-GO` rows (`restart/skinny/tranches/sk-v10/research/p1/p1f-results-delta.md:84`).

6. Structural scan remains diagnostic. P1-C shows SIMD structural scan is faster than scalar on every row, but explicitly says that fact is not enough to reopen W3 because SK-V9 W3 V1/V2 already regressed must-improve and maintain rows (`restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md:111`). REDRESS 98 retires `G-W3-UNION-SUBSTRATE`; no SK-V9 wave may force, amend, or split W3 to preserve that class (`skinny/REDRESS.md:2910`, `skinny/REDRESS.md:2934`).

7. Tape/direct union is already an implemented redress invariant. REDRESS records Track 1 and Track 2 consuming the same one-buffer `TapeBuilder` (`skinny/REDRESS.md:110`), tape/direct-to-struct remaining one substrate (`skinny/REDRESS.md:126`), payload arena staying cold (`skinny/REDRESS.md:134`), lazy-offset tape union migration eliminating the parser sidecar (`skinny/REDRESS.md:246`, `skinny/REDRESS.md:252`), and sparse flags plus direct spare-capacity offset writes landing (`skinny/REDRESS.md:274`).

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

### C1 — Tape Capacity/Flag Economy Contract

- Shape: retain the current `u32` offset stream plus sparse `(cursor, flags)` side stream; any S-P3 work may only tune capacity planning, flag lookup, or sealing telemetry against the existing `TapeBuilder` contract. It may not add a retained class column, sidecar bitmap, structural index, or second source pass.
- Scalar-ref status: existing scalar reference is the current `TapeBuilder`/view path plus materialization report. A future micro-proof must compare logical bytes, allocated bytes, flag bytes, payload bytes, and Track 1/Track 2 row Mbps against the current report before integration.
- Arch: scalar memory/layout contract; architecture-specific prefetch or store hints belong to P2-C/P2-E only after a same-host microbench.
- P1 antecedent: capacity pressure is grounded in lazy materialization ratios from `skinny/RESULTS.md:95` through `skinny/RESULTS.md:141`, not in a new hot-leaf symbol. Therefore this is a contract surface, not an optimization primitive by itself.

### C2 — Direct Product-Plane Contract

- Shape: keep direct digest and typed direct as `SinkOnly`/typed product-plane materializations with no queryable retained document identity; Track 1 generated direct, Track 2 independent hand direct, serde, and sonic remain the parity plane. A promoted direct row needs a product/control-path contract, not a tape relabel.
- Scalar-ref status: scalar reference exists: `track2_digest` is independent hand code (`skinny/crates/bbnf-bench/src/direct_struct.rs:408`) and parity checks against serde and sonic (`skinny/crates/bbnf-bench/src/direct_struct.rs:420`). Any new direct surface must keep this scalar oracle or provide its replacement before SIMD/ASM work.
- Arch: scalar control/output contract first; no ISA dependency.
- P1 antecedent: `direct_struct` / digest fold on `distinct_values`, plus the 14-row `N-direct` frontier in P1-F (`restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:68`, `restart/skinny/tranches/sk-v10/research/p1/p1f-results-delta.md:84`).

### C3 — Existing-Substrate Container Walk Contract

- Shape: specialize generated direct object/array walk and scalar-in-container sink calls without changing retained tape shape. The contract is "same source cursor, same sink event stream, no retained sidecar." This is the admissible substrate-adjacent target for `array_walk` and `object_walk`.
- Scalar-ref status: current hand direct parser is the scalar oracle for direct digest; retained parse Track 2 remains the oracle for retained tape rows. Any S-P3 primitive must have a checkasm-style scalar reference and a same-wave generated consumer.
- Arch: scalar first; SIMD/ASM only if P2-C/P2-E prove a grammar-neutral caller primitive.
- P1 antecedent: `array_walk` / `object_walk` on `canada`, `mesh`, `marine_ik`, `citm_catalog`, and `instruments` in P1-E (`restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:45`, `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:60`).

### C4 — Existing-Substrate String/Unicode Lazy Decode Contract

- Shape: keep escaped-string work behind the current source-span and flag contract; improve only the string/escape leaf consumed by direct or retained views. No decoded scratch column, semantic-fact sidecar, receiver shortcut, or eager decode plane is admissible without a material differential.
- Scalar-ref status: scalar reference exists in `parse-that-regex` string/unescape paths and current direct Track 2 string handling. A future primitive must prove byte-exact parity against that scalar path and avoid eager decoded-value materialization.
- Arch: scalar + optional SIMD/string primitive; architecture selection belongs to P2-C/P2-E.
- P1 antecedent: `string_full_scan`, `string_escape`, `unicode_escape_hex`, and `alloc` on `unicode_mixed`, `unicode_escapes`, and `y_string_unicode` (`restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:41`, `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:65`, `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:84`). P1-C rejects eager decode as 1.93x to 5.71x slower (`restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md:77`).

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

- C1 is grammar-neutral if expressed as `OffsetTape<EventGrammar>` economy: event offsets, sparse flags, payload arena counters, and materialization telemetry. It becomes JSON-only if flags are named as JSON escape/control bits in a generic crate; Lock 14 forbids that class (`restart/locks/LOCKS.md:78`).
- C2 is grammar-neutral as a product-plane sink contract: a grammar emits event callbacks or typed fields to a sink without a retained document. JSON digest is only the current comparator; the abstraction is `SinkOnly` typed output.
- C3 is grammar-neutral as container/event walk lowering over grammar-emitted enter/exit/scalar events. It is JSON-only if it hardcodes object/array punctuation outside generated grammar code.
- C4 is grammar-neutral as a lazy string/escape codec over a source span plus per-token flags. JSON `\uXXXX` is a grammar instantiation; CSS strings, Sheets strings, and BBNF-self literals can consume the same lazy-decode contract with grammar-owned escape semantics.

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

- Do not reopen W3 union/event substrate, class-column, streaming cursor, or class-lane-only variants. REDRESS 96/97/98 falsified the route and retired it (`skinny/REDRESS.md:2910`, `skinny/REDRESS.md:2934`).
- Do not introduce sidecar producers, parser-owned structural cursors/facts, retained whitespace bitmaps, aux projection columns, or a second source pass. REDRESS blocks these surfaces as sidecar/cursor routes (`skinny/REDRESS.md:767`, `skinny/REDRESS.md:793`, `skinny/REDRESS.md:2673`).
- Do not relabel direct digest as typed product proof. SK-V10 synthesis pre-blocks this explicitly (`restart/skinny/tranches/sk-v10/SYNTHESIS.md:127`).
- Do not route through eager decoded strings, decoded scratch, semantic facts, or receiver/source-hook shortcuts without a new material differential. P1-C rejects eager decode (`restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md:77`), and REDRESS 66-69 exhaust the direct string materialization family (`skinny/REDRESS.md:1831`, `skinny/REDRESS.md:1881`).
- Do not scope a substrate or kernel wave without same-host micro-proof and same-wave consumer. SK-V10 synthesis requires micro-prove-first for substrate/kernel intervention (`restart/skinny/tranches/sk-v10/SYNTHESIS.md:145`), and P1 hardening records direct/typed PMU absence as instrumentation fact only (`restart/skinny/tranches/sk-v10/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md:47`).

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

- `restart/prompts/skinny/PASS-2-RESEARCH.md`
- `restart/locks/LOCKS.md`
- `restart/skinny/tranches/sk-v10/HANDOFF.md`
- `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1f-results-delta.md`
- `restart/skinny/tranches/sk-v10/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `skinny/crates/runtime/src/tape/mod.rs`
- `skinny/crates/runtime/src/tape/assembler.rs`
- `skinny/crates/runtime/src/grammars/json/parser.rs`
- `skinny/crates/runtime/src/grammars/json/view.rs`
- `skinny/crates/runtime/src/grammars/json/value.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
