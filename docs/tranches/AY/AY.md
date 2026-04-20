# Tranche AY — Canonical Packed Substrate and Near-Parity Closure

AY is the parity tranche. It closes the post-AU architectural drift by
making the grammar-derived parser write one canonical packed substrate
directly, then driving the default eager JSON path to near sonic-rs
parity on that substrate. AY does not mix in replay, recovery,
incremental tooling, or build-iteration reform; those move to AZ and
BB. AY does not open a second runtime path; it replaces the current
generic tape-first hot contract with a single hybrid substrate informed
by sonic-rs-class direct construction, simdjson-class structural
leverage, and the bbnf research corpus where those ideas survive as
fully general mechanisms.

## Architectural thesis

1. **One parser, one substrate.** The parser emits exactly one
   canonical runtime substrate. `view()`, `to_value()`, `get()`,
   debug/readback, replay, and future incremental consumers all read
   that same output.
2. **The target is a generalized hybrid, not a copy.** AY combines
   sonic-rs direct write, simdjson skip/count/index leverage, and
   bbnf-native direct-to-struct, Pratt flattening, and structural
   mining without hard-coded grammar-name dispatch.
3. **BBNF remains the semantic source.** `.bbnf` files remain the sole
   semantic and structural source within reason; host typing and
   projection enrich but do not replace them with handwritten product
   parser logic.
4. **Parity-critical work stays in AY.** Anything required to reach the
   near-parity default eager JSON target belongs in AY. Anything else
   routes to AZ, BA, or BB.
5. **Runtime truth closes the tranche.** AY closes on benches,
   profiles, `cargo expand`, `cargo asm`, and wire-contract tests, not
   on architectural intent.

## Invariants

1. No orthogonal parse/runtime paths.
2. No grammar-name specialization.
3. No revived DTA/PSI runtime substrate.
4. Direct-to-struct ships only as a grammar-derived general mechanism.
5. Pratt flattening ships only as a grammar-derived general mechanism.
6. Structural side information accelerates the same runtime path.
7. Value/view/debug consumers share the same substrate.
8. No substrate addition lands without a same-wave consumer.
9. Near parity is the floor, not a stretch-only aspiration.
10. Compile/build/tooling work stays out unless load-bearing for AY
    parity closure.

## Operational posture

1. Every remaining AY wave benches the default eager JSON path at wave
   close; W8 also captures the full 19-entry parse matrix.
2. Every new substrate field, side table, or emitted path ships with a
   production consumer in the same wave.
3. `cargo expand`, `nm`, `cargo asm`, and Samply outputs are required
   evidence for activation claims; grep-only gates do not close waves.
4. Grammar-derived generality is enforced at plan time: no JSON-only,
   CSS-only, or Sheets-only handwritten semantic paths.
5. When a lever does not prove out, it is documented in AY and routed
   cleanly to AZ/BA/BB at tranche close rather than left as a dead
   surface.

## Wave summary

| Wave | Spec | Headline | Opens after |
|---|---|---|---|
| **W0** | [waves/W0.md](waves/W0.md) | Legacy prune, stale test retirement, AX FINAL, inherited-state cleanup | tranche open |
| **W1** | [waves/W1.md](waves/W1.md) | Canonical write-path repair, finalise fusion, structural activation, Pratt normalization | W0 |
| **W2** | [waves/W2.md](waves/W2.md) | Named preservation, wrap-compound elision, canonicalisation | W1 |
| **W3** | [waves/W3.md](waves/W3.md) | Grammar-derived value surface and first-class value benchmarks | W2 |
| **W4** | [waves/W4.md](waves/W4.md) | SIMD string/number and regex hot-path repairs | W3 |
| **W5** | [waves/W5.md](waves/W5.md) | Canonical packed substrate contract and direct JSON write | W4 |
| **W6** | [waves/W6.md](waves/W6.md) | Consumer unification, general direct-to-struct, and Pratt/operator lowering | W5 |
| **W7** | [waves/W7.md](waves/W7.md) | Minimal globally informed optimizer integration | W6 |
| **W8** | [waves/W8.md](waves/W8.md) | Near-parity close, FINAL, and AZ/BA/BB handoff | W7 |

## AY → AZ / BA / BB handoff contract

AY does not close until all of the following are true:

1. Default JSON parse writes the canonical packed substrate directly;
   the hot eager path no longer reconstructs from a generic tape-first
   contract.
2. `view()`, `to_value()`, and `get()` all read that same substrate.
3. Grammar-derived direct-to-struct and Pratt/operator lowering are
   emitted and consumed without grammar-name routing.
4. Any mined or emitted surface kept by AY has a production consumer;
   dead surfaces are retired.
5. Near-parity gates hold on the default eager JSON path:
   `twitter <= 1.15`, `canada <= 1.20`, `citm <= 1.20`, and 5-fixture
   geomean `<= 1.20`.
6. CSS, Sheets, and BBNF preserve their current functional guarantees;
   AY does not buy parity by regressing grammar generality.

## Defensible floor

AY's defensible floor is not "architectural parity later." The minimum
closeable AY outcome is:

1. W5 lands one canonical packed substrate and default JSON writes it
   directly.
2. W6 moves `view()`, `to_value()`, and `get()` onto that substrate and
   lands general direct-to-struct admission.
3. W7 proves the optimizer decisions that choose packed layout,
   direct-to-struct, Pratt lowering, and structural-side use are driven
   by shared facts rather than isolated heuristics.
4. W8 closes on the declared near-parity gates with no second parser
   and no runtime split.

Anything less is a tranche miss, not an acceptable floor.

## Post-tranche review candidates

Decision at W8 close, not mid-wave:

- How much provenance must stay in the substrate proper versus AZ side
  metadata.
- Whether any retained structural-side accelerator still fails the
  consumer-benefit threshold and should be deleted in BA.
- Whether any shared-cost or recognizer fact path still duplicates
  work across regex, e-graph, and materialization after W7.
- Whether the canonical packed layout needs a second post-parity pass
  for beyond-parity JSON/CSS exceedance in BA.

## Indefatigability

When AY closes correctly, bbnf has one grammar-derived parser, one
canonical packed substrate, one truthful hot eager JSON path in the
sonic-rs class, and one substrate handoff that AZ, BA, and BB can
extend without reopening runtime duality or substrate drift.
