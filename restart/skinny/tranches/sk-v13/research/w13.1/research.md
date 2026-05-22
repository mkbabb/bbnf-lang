# SK-V13 W13.1 Research - Numbers Typed Product Surface

Wave: W13.1 typed product surface completion.
Scope: `json/numbers/real_typed_struct/main`.
Date: 2026-05-22.

## Cohort

Six read-only slices were redeployed against Section 17:

- Slice A, missing typed-row census: `numbers` is one of ten absent
  `real_typed_struct` product rows in `restart/skinny/ROLLING-SOTA-DELTA.md`;
  `update_center` is present but below the full-SOTA addendum bar.
- Slice B, fixture/oracle map: existing typed routing lives in
  `skinny/crates/bbnf-bench/src/real_typed_struct.rs`, the generated parsers
  in `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, and the generator
  schema in `skinny/xtask/src/real_typed_schema.rs`. A row becomes benchable
  when `fixture_for_name()` returns `Some`.
- Slice C, codegen/schema surface: the typed generator already supports
  top-level `Vec<T>` roots, scalar `f64`, capacity hints, and generated vector
  helpers.
- Slice D, gate/report surface: a new typed row must be gate-consumed; direct
  digest admission and old slack metadata are not sufficient under the
  addendum.
- Slice E, below-SOTA typed row audit: `update_center` remains an optimization
  target, but it is not the smallest missing product surface.
- Slice F, pre-block ledger: REDRESS 70-72 and 103-110 are mixed precedent.
  Typed product generation is admissible; hidden typed sinks, proof-only roots,
  direct digest rows, and no-op wrappers are blocked.

## Finding

`numbers` is the lowest-risk first W13 typed subwave. The fixture is a
top-level array of 10,001 numbers, already counted in `RESULTS.md`; it does not
require a new object schema, map entries, string escape policy, union type, BIR
variant, directive, or substrate. The existing generator can express the row as
`Vec<f64>` with a `10_001` capacity hint.

The row still carries a real gate. The generated Track 1 parser must materialize
the typed `Vec<f64>` product, Track 2 must be an independent typed oracle
(`serde_json`), sonic must parse the same typed product, and the row admits only
if Track 1 exceeds same-run sonic strict by at least 1 Mbps while all four
checksums agree.

## Candidate Ranking

1. `numbers`: top-level `Vec<f64>`, no schema expansion beyond a typed root.
2. `y_string_unicode`: top-level string vector, useful but escape-heavy.
3. `unicode_basic`: regular string/numeric record array, larger schema.
4. `gsoc-2018`: regular but string-heavy object map, larger schema volume.
5. `canada`: feasible only with deeper coordinate vectors and more prior
   REDRESS history.
6. `random` / `instruments`: heterogeneous enough to defer until the typed
   surface and decision machinery have more rows banked.

## Revert Protocol

If W13.1 fails, revert the `numbers` typed root, generated parser, fixture
enum/output/checksum/bench routing, and row report changes. REDRESS records the
exact failure class: schema impossibility, parity mismatch, sonic threshold
miss, gate-consumption failure, or Track 2 coupling.
