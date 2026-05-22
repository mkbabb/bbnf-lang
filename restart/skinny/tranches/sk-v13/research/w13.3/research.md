# SK-V13 W13.3 Research - Random Typed Product Surface

Wave: W13.3 typed product surface completion.
Scope: `json/random/real_typed_struct/main`.
Date: 2026-05-22.

## Cohort

Six read-only slices were redeployed after W13.2:

- Slice A, geospatial/map rows: `canada` needs a fixed coordinate-pair product
  to avoid tens of thousands of inner `Vec` allocations; `gsoc-2018` is a
  cleaner map-entry candidate but has a strong sonic string baseline.
- Slice B, simple residual rows: `random` is the next honest missing product
  surface that fits the current schema generator without new codegen features.
  `distinct_values` either skips the dynamic payload or needs a mixed known
  field plus dynamic-entry collector.
- Slice C, existing/open typed rows: `update_center` already has a typed
  product surface and remains below sonic+1; it is an optimization wave, not a
  missing-surface wave.
- Slice D, unicode-heavy rows: `unicode_mixed`, `unicode_escapes`, and
  `y_string_unicode` are escape/allocation-heavy and should follow with their
  own row-specific evidence.
- Slice E, codegen surface: the generator already supports struct roots,
  nested structs, capacity-hinted `Vec<T>`, borrowed strings, booleans, and
  integers. No new directive, BIR variant, `BackendShape`, or substrate API is
  required for `random`.
- Slice F, gate/report surface: W13.2 generalized the W13 typed-product
  companion gate to row-specific criterion specs. W13.3 can extend that table
  only if it moves the row through Criterion and `gate-json`.

## Fixture Shape

`skinny/test_data/random.json` is a root object with keys:

```text
{ id, jsonrpc, total, result }
```

`result` has 1,000 user records. Each user has the same product shape:

```text
{ id, avatar, age, admin, name, company, phone, email, birthDate, friends, field }
```

`friends` is a vector of three records:

```text
{ id, name, phone }
```

The full typed product can be represented as
`crate::real_typed_struct::RandomDocument<'i>` with nested
`RandomUser<'i>` and `RandomFriend<'i>` structs. The independent Track 2/oracle
route is the existing `serde_json` typed fixture harness, with same-run
`sonic_rs` typed strict as the SOTA comparator.

## Finding

`random` is the next admissible W13 missing typed product target after
`numbers` and `unicode_basic`. It exercises a real nested host/API output,
Russian UTF-8 strings, booleans, integers, and a fixed small child vector
without introducing a generator extension. The row may still miss sonic+1
because it has many short strings and a broad object shape; that would be a
measured reject, not a schema blocker.

## Revert Protocol

If W13.3 fails, revert the random typed root, generated parser, fixture
enum/output/checksum/bench routing, companion report extension, RESULTS and
rolling updates, and the REDRESS entry. Record whether the failure is schema
impossibility, parity mismatch, sonic threshold miss, gate-consumption failure,
or Track 2 coupling.
