# SK-V7 W3 Phase 1 Research: Typed Vec Helper Specialization

Date: 2026-05-16

Scope: `json_typed_direct` helper collection, `rust_type`, helper `type_key`, Vec helper emission, and comparison to the existing `MapEntriesVec` capacity shape.

## Findings

1. SPEC §5 makes `DirectTypeRef::Vec` specialization load-bearing for Wave 3. The close row is `mesh | real_typed_struct | >=100% sonic`, and the named mechanism is `DirectTypeRef::Vec specialisation + mesh schema` at `restart/skinny/tranches/sk-v7/SPEC.md:32`. The concrete Wave 3 task list names `json_typed_direct.rs:306-315` as the shape-blind Vec helper, requires adding `capacity_hint: Option<usize>` to `DirectTypeRef::Vec`, requires including that hint in `type_key()`, and asks for `Vec::with_capacity(hint.unwrap_or(0))` plus numeric-array specialization at `restart/skinny/tranches/sk-v7/SPEC.md:179` through `restart/skinny/tranches/sk-v7/SPEC.md:195`.

2. The current `Vec` schema shape has no capacity channel. `DirectTypeRef::Vec(Box<DirectTypeRef>)` is defined at `skinny/crates/codegen/src/direct_schema.rs:60` through `skinny/crates/codegen/src/direct_schema.rs:74`, while `MapEntriesVec` already carries `capacity_hint: Option<usize>` at `skinny/crates/codegen/src/direct_schema.rs:66` through `skinny/crates/codegen/src/direct_schema.rs:72`. Validation treats `Vec`, `MapString`, `MapEntriesVec`, and `Option` as one recursive class at `skinny/crates/codegen/src/direct_schema.rs:201` through `skinny/crates/codegen/src/direct_schema.rs:218`, so changing the enum shape is mechanically small but must update every pattern match and constructor.

3. `rust_type()` already abstracts away container metadata correctly. `DirectTypeRef::Vec(inner)` returns only `Vec<inner>` at `skinny/crates/codegen/src/json_typed_direct.rs:166` through `skinny/crates/codegen/src/json_typed_direct.rs:187`. `MapEntriesVec` also returns only `Vec<entry_rust_type>` there, ignoring capacity and field names. Recommendation: keep capacity hints out of `rust_type()`; they are emission metadata, not Rust type identity.

4. Helper collection is keyed by `type_key()`, so a capacity hint must participate in the key before different Vec capacities can coexist. `Renderer::new()` only collects helpers from struct fields at `skinny/crates/codegen/src/json_typed_direct.rs:141` through `skinny/crates/codegen/src/json_typed_direct.rs:154`. `collect_helpers()` recurses into inner container types first, then inserts one helper per `type_key()` at `skinny/crates/codegen/src/json_typed_direct.rs:213` through `skinny/crates/codegen/src/json_typed_direct.rs:229`. Today `type_key()` collapses all `Vec<T>` helpers to `vec_{type_key(T)}` at `skinny/crates/codegen/src/json_typed_direct.rs:358` through `skinny/crates/codegen/src/json_typed_direct.rs:370`; this would wrongly share the same helper for `Vec<T>` fields with different capacity hints or specialized parse strategy.

5. The current Vec helper is shape-blind and allocation-blind. It emits `let mut out: Vec<{inner_ty}> = Vec::new();`, parses `[`, loops with `out.push({inner_expr}?)`, and handles commas one at a time at `skinny/crates/codegen/src/json_typed_direct.rs:306` through `skinny/crates/codegen/src/json_typed_direct.rs:315`. Existing generated output confirms this exact shape for `statuses: Vec<Tweet>` at `skinny/crates/bbnf-bench/src/generated_real_typed.rs:286` through `skinny/crates/bbnf-bench/src/generated_real_typed.rs:299`.

6. `MapEntriesVec` is the direct precedent for capacity-aware helper emission. Its arm destructures `capacity_hint`, derives `let capacity = capacity_hint.unwrap_or(0);`, and emits `let mut out: {return_ty} = Vec::with_capacity({capacity});` at `skinny/crates/codegen/src/json_typed_direct.rs:326` through `skinny/crates/codegen/src/json_typed_direct.rs:341`. The checked-in generated real-typed module shows a concrete `Vec::with_capacity(768)` for plugin entries at `skinny/crates/bbnf-bench/src/generated_real_typed.rs:331` through `skinny/crates/bbnf-bench/src/generated_real_typed.rs:343`. The serde oracle for the same shape also uses map `size_hint()` to allocate entries at `skinny/crates/bbnf-bench/src/real_typed_struct.rs:253` through `skinny/crates/bbnf-bench/src/real_typed_struct.rs:275`.

7. The spec's scalar target is not currently representable as written. SPEC §5 asks for mesh fields like `Vec<f32>` and `Vec<u32>` and specialization for `DirectScalar::F64/U32` at `restart/skinny/tranches/sk-v7/SPEC.md:191` through `restart/skinny/tranches/sk-v7/SPEC.md:192`, but `DirectScalar` only has `String`, `Bool`, `I64`, `U64`, and `F64` at `skinny/crates/codegen/src/direct_schema.rs:76` through `skinny/crates/codegen/src/direct_schema.rs:83`; `json_typed_direct` likewise only maps those scalar variants at `skinny/crates/codegen/src/json_typed_direct.rs:173` through `skinny/crates/codegen/src/json_typed_direct.rs:197`. Wave 3 cannot honestly emit `Vec<f32>` or `Vec<u32>` without either adding scalar variants or using `F64/U64` as a temporary schema compromise.

8. `lower/schema_direct.rs` only validates that JSON sink-only shapes and literals exist, then clones the supplied schema into the typed program at `skinny/crates/codegen/src/lower/schema_direct.rs:11` through `skinny/crates/codegen/src/lower/schema_direct.rs:42`. It does not infer Vec capacity from sink-only IR. Therefore capacity hints must be authored in the schema fixture or a new schema-building pass, not expected from `lower/sink_only.rs`. The existing fixture builder constructs `vec(inner)` as `DirectTypeRef::Vec(Box::new(inner))` at `skinny/xtask/src/real_typed_schema.rs:159` through `skinny/xtask/src/real_typed_schema.rs:161`, while `map_entries(...)` already accepts a capacity argument and wraps it in `Some(capacity_hint)` at `skinny/xtask/src/real_typed_schema.rs:163` through `skinny/xtask/src/real_typed_schema.rs:177`.

## Implementable Recommendations

1. Change the schema enum to:

   ```rust
   Vec {
       capacity_hint: Option<usize>,
       inner: Box<DirectTypeRef>,
   }
   ```

   This is more extensible than `Vec(Box<DirectTypeRef>, Option<usize>)` and matches the named fields used by `MapEntriesVec`. Update `validate_type_ref()`, `rust_type()`, `parse_expr()`, `collect_helpers()`, `render_helper()`, test fixtures, and xtask schema helpers in the same commit.

2. Keep `rust_type()` output unchanged except for the new pattern:

   ```rust
   DirectTypeRef::Vec { inner, .. } => Ok(format!("Vec<{}>", self.rust_type(inner)?))
   ```

   Capacity must not affect the Rust type string. It should only affect helper identity and helper body.

3. Change helper keys to include capacity and future strategy bits:

   ```rust
   DirectTypeRef::Vec { capacity_hint, inner } => {
       format!("vec_cap_{}_{}", capacity_hint.unwrap_or(0), type_key(inner))
   }
   ```

   If a numeric fast path is added in the same wave, include that mode in the key as well, or derive it purely from `inner` so the key remains complete. Do not leave the current `vec_{inner}` shape, because it would collide if one `Vec<U64>` field has hint `3` and another has hint `1024`.

4. Mirror `MapEntriesVec` for the first capacity-only patch:

   ```rust
   let capacity = capacity_hint.unwrap_or(0);
   let mut out: {return_ty} = Vec::with_capacity({capacity});
   ```

   Prefer `{return_ty}` over `Vec<{inner_ty}>` for consistency with `MapEntriesVec` and to reduce duplicate type formatting. This is a low-risk Phase 1 specialization and gives immediate allocation parity with the existing map-entry path.

5. Split numeric specialization into a second, measurable patch after capacity hints compile and generated output is stable. The current parser only exposes scalar `parse_u64()` and `parse_f64()`; a SWAR-across-commas loop for homogeneous numeric arrays should be admitted only after defining exact scalar coverage. For mesh, first decide whether to add `DirectScalar::{U32,F32}` or to model indices/vertices as `U64/F64` and downcast in a typed constructor. Adding `U32/F32` is cleaner but touches schema, parser materialization, generated Rust type mapping, serde oracle parity, and fixture structs.

6. Add focused tests/goldens before bench work:

   - codegen unit: two fields of the same inner scalar with different capacity hints generate two different helper names.
   - codegen unit: `capacity_hint: Some(N)` emits `Vec::with_capacity(N)` and `None` emits `Vec::with_capacity(0)`.
   - generated fixture check: existing `statuses` output changes only from `Vec::new()` to the chosen capacity form when a hint is supplied.
   - regression check: existing `MapEntriesVec` helper still emits `Vec::with_capacity(768)`.

## Risks

1. Helper collision risk is real and silent. If `type_key()` is not updated with the Vec hint, generated code will compile but one field can call a helper with another field's capacity or strategy. This matters because helper collection deduplicates by key before emission at `skinny/crates/codegen/src/json_typed_direct.rs:220` through `skinny/crates/codegen/src/json_typed_direct.rs:224`.

2. Source compatibility risk is broad but mechanical. `DirectTypeRef::Vec` appears in schema validation, helper collection, rendering, tests, and xtask fixture construction. Missing one match arm will fail compile; missing a fixture helper update will block regeneration.

3. Scalar mismatch risk can invalidate Wave 3 if not resolved early. The spec says `Vec<f32>` and `Vec<u32>`, but current schema cannot express either. Treating mesh as `Vec<f64>`/`Vec<u64>` may benchmark a different workload from the stated close condition; adding `F32/U32` is more faithful but increases the wave's blast radius.

4. Performance risk: `Vec::with_capacity(0)` is allocation-equivalent to `Vec::new()` for empty initial capacity, so capacity-only specialization only helps fields with authored nonzero hints. Mesh must provide realistic hints in `xtask/src/real_typed_schema.rs`; otherwise the helper shape changes without addressing allocation churn.

5. Numeric fast-path risk: the existing helper loop interleaves whitespace, scalar parse, comma, and close-bracket handling. A SWAR comma/digit scan must preserve JSON number grammar, whitespace semantics, error offsets, empty array behavior, and trailing-comma rejection. It should be gated behind parity tests before being tied to the W3 close benchmarks.

## Sources

- `restart/skinny/tranches/sk-v7/SPEC.md:32`
- `restart/skinny/tranches/sk-v7/SPEC.md:179`
- `restart/skinny/tranches/sk-v7/SPEC.md:181`
- `restart/skinny/tranches/sk-v7/SPEC.md:189`
- `restart/skinny/tranches/sk-v7/SPEC.md:191`
- `skinny/crates/codegen/src/direct_schema.rs:60`
- `skinny/crates/codegen/src/direct_schema.rs:66`
- `skinny/crates/codegen/src/direct_schema.rs:76`
- `skinny/crates/codegen/src/direct_schema.rs:201`
- `skinny/crates/codegen/src/json_typed_direct.rs:121`
- `skinny/crates/codegen/src/json_typed_direct.rs:166`
- `skinny/crates/codegen/src/json_typed_direct.rs:213`
- `skinny/crates/codegen/src/json_typed_direct.rs:306`
- `skinny/crates/codegen/src/json_typed_direct.rs:326`
- `skinny/crates/codegen/src/json_typed_direct.rs:358`
- `skinny/crates/codegen/src/lower/schema_direct.rs:11`
- `skinny/crates/codegen/src/lower/sink_only.rs:95`
- `skinny/xtask/src/real_typed_schema.rs:159`
- `skinny/xtask/src/real_typed_schema.rs:163`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:286`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:331`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs:253`
