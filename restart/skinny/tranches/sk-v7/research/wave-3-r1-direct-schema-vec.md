# SK-V7 W3 R1: Direct schema Vec capacity hint

## Scope

Research scope: direct schema `Vec` capacity hint for SK-V7 Wave 3 Phase 1. This report covers `DirectTypeRef::Vec` constructors/usages, the codegen helper path, and gates/risks for the requested `capacity_hint` specialization. No source changes are included here.

## Findings

1. The SK-V7 Wave 3 spec explicitly makes the `DirectTypeRef::Vec` schema shape part of the implementation surface. The owner paths name `skinny/crates/codegen/src/json_typed_direct.rs:306-315`, `skinny/crates/codegen/src/lower/sink_only.rs`, and `skinny/crates/codegen/src/direct_schema.rs:64`; the task list says to add `capacity_hint: Option<usize>` to `DirectTypeRef::Vec`, update `type_key()` to include it, specialize scalar Vec helpers, add the mesh schema, and bench mesh plus marine_ik (`restart/skinny/tranches/sk-v7/SPEC.md:181`, `restart/skinny/tranches/sk-v7/SPEC.md:189`, `restart/skinny/tranches/sk-v7/SPEC.md:196`).

2. The handoff identifies the blocking condition before any mesh typed schema should be admitted: mesh DirectBuild is blocked by codegen Vec helper shape-blindness and needs `DirectTypeRef::Vec` specialization first (`restart/skinny/tranches/sk-v7/HANDOFF.md:30`, `restart/skinny/tranches/sk-v7/HANDOFF.md:35`). The same handoff pre-blocks capacity prescan and several broad retained/direct-materialization routes (`restart/skinny/tranches/sk-v7/HANDOFF.md:66`, `restart/skinny/tranches/sk-v7/HANDOFF.md:75`, `restart/skinny/tranches/sk-v7/HANDOFF.md:84`).

3. `DirectTypeRef::Vec` currently carries only the inner type. `MapEntriesVec` already carries `capacity_hint: Option<usize>`, so the requested schema extension is structurally local but pattern-changing: every match on `Vec(inner)` must become a struct-pattern or equivalent (`skinny/crates/codegen/src/direct_schema.rs:60`, `skinny/crates/codegen/src/direct_schema.rs:64`, `skinny/crates/codegen/src/direct_schema.rs:66`, `skinny/crates/codegen/src/direct_schema.rs:70`). Validation recursively descends through `Vec(inner)` today and can keep the same semantic behavior after destructuring the new field (`skinny/crates/codegen/src/direct_schema.rs:201`, `skinny/crates/codegen/src/direct_schema.rs:214`).

4. The current generated `Vec` helper is shape-blind and allocates with `Vec::new()` unconditionally. It parses all element types through the same loop, with per-element `parser.ws()`, delimiter checks, and `out.push(inner_expr?)` (`skinny/crates/codegen/src/json_typed_direct.rs:306`, `skinny/crates/codegen/src/json_typed_direct.rs:313`). `MapEntriesVec` is the local capacity-hint precedent: it unwraps the hint and emits `Vec::with_capacity(capacity)` (`skinny/crates/codegen/src/json_typed_direct.rs:326`, `skinny/crates/codegen/src/json_typed_direct.rs:335`, `skinny/crates/codegen/src/json_typed_direct.rs:340`).

5. Helper naming will collide unless the hint participates in the key. Helpers are deduplicated by `type_key(ty)` before rendering (`skinny/crates/codegen/src/json_typed_direct.rs:213`, `skinny/crates/codegen/src/json_typed_direct.rs:220`), and `Vec` currently keys only on the inner type (`skinny/crates/codegen/src/json_typed_direct.rs:358`, `skinny/crates/codegen/src/json_typed_direct.rs:362`). This would incorrectly share one helper for `Vec<f64>` fields with different capacity hints.

6. All current constructors/usages found for `DirectTypeRef::Vec` are small and source-local. `skinny/xtask/src/real_typed_schema.rs:159` has a `vec(inner)` helper that returns `DirectTypeRef::Vec(Box::new(inner))`; `skinny/crates/codegen/src/lib.rs:398` has a test schema literal using `DirectTypeRef::Vec(Box::new(DirectTypeRef::Scalar(DirectScalar::U64)))`; generated output currently contains `parse_vec_type_tweet` with `Vec::new()` (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:286`, `skinny/crates/bbnf-bench/src/generated_real_typed.rs:287`).

7. The scalar coverage implied by mesh is not fully represented by `DirectScalar` today. `DirectScalar` has `String`, `Bool`, `I64`, `U64`, and `F64`, but no `U32` or `F32` (`skinny/crates/codegen/src/direct_schema.rs:76`, `skinny/crates/codegen/src/direct_schema.rs:82`). The renderer maps those same scalar variants to `u64`/`f64` parse methods only (`skinny/crates/codegen/src/json_typed_direct.rs:173`, `skinny/crates/codegen/src/json_typed_direct.rs:177`), and `DirectParser` exposes `parse_u64()` and `parse_f64()`, not `parse_u32()` or `parse_f32()` (`skinny/crates/codegen/src/json_typed_direct.rs:514`, `skinny/crates/codegen/src/json_typed_direct.rs:522`). A mesh fixture with `Vec<f32>` and `Vec<u32>` therefore needs either new scalar variants/parsers or an explicit decision to materialize as `Vec<f64>`/`Vec<u64>` for Phase 1.

8. `sink_only` does not consume `DirectTypeRef` and has no Vec emission path to update. It lowers backend `DirectBuild` shapes and records shape/fact summaries only (`skinny/crates/codegen/src/lower/sink_only.rs:95`, `skinny/crates/codegen/src/lower/sink_only.rs:125`, `skinny/crates/codegen/src/lower/sink_only.rs:172`). The W3 instruction to verify `sink_only` should be treated as a non-change gate: confirm no direct schema `Vec` plumbing is expected there.

9. The current real-typed fixture set has only twitter and update_center. The fixture enum, dispatch, and typed parity code must be extended before mesh/marine_ik `real_typed_struct` can be measured (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:9`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:74`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:111`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:125`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:146`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:160`).

## Recommendations and Gates

1. Implement the schema change as `DirectTypeRef::Vec { capacity_hint: Option<usize>, inner: Box<DirectTypeRef> }` or equivalent named fields. Named fields are preferable because later scalar-array specialization can add fields without ambiguous tuple churn.

2. Update all match sites in `direct_schema.rs` and `json_typed_direct.rs`, plus the constructor helpers in `skinny/xtask/src/real_typed_schema.rs:159` and test literal in `skinny/crates/codegen/src/lib.rs:398`. Keep existing call sites defaulting to `capacity_hint: None` unless a fixture-specific hint is known.

3. Update `type_key()` before enabling mixed hints. Suggested key form: `vec_hint_{hint-or-none}_{inner-key}`. This gate is required because helper collection deduplicates by key before render (`skinny/crates/codegen/src/json_typed_direct.rs:220`).

4. First implementation gate: generated `Vec` helper with `capacity_hint: Some(n)` must emit `Vec::with_capacity(n)`, while `None` may emit `Vec::new()` or `Vec::with_capacity(0)`. Existing twitter generated output should remain semantically equivalent.

5. Second implementation gate: do not claim mesh W3 completion from capacity alone. The spec asks for scalar numeric specialization and the prior C3 analysis predicts rejection if the current per-element parser cadence remains (`restart/skinny/tranches/sk-v7/research/skv7-C3-typed-profile.md:294`, `restart/skinny/tranches/sk-v7/research/skv7-C3-typed-profile.md:303`, `restart/skinny/tranches/sk-v7/research/skv7-C3-typed-profile.md:408`).

6. Type-surface gate: decide whether W3 Phase 1 admits `DirectScalar::U32`/`DirectScalar::F32` and parser methods, or documents that mesh uses widened `u64`/`f64` fields initially. The spec text names `Vec<f32>` and `Vec<u32>` (`restart/skinny/tranches/sk-v7/SPEC.md:192`), but the current schema cannot express those exact types.

7. Bench gates are the SK-V7 spec gates: mesh `real_typed_struct` >=100% sonic-strict, marine_ik `real_typed_struct` >=100% sonic-strict, mesh `direct_to_struct` stays >=91.8%, and twitter `real_typed_struct` stays >=151.5% (`restart/skinny/tranches/sk-v7/SPEC.md:196`, `restart/skinny/tranches/sk-v7/SPEC.md:202`).

## Risks and Pre-Blocked Routes

1. Capacity hints alone are unlikely to clear mesh. C3 says the current `Vec<f64>` path pays per-element whitespace/delimiter branches and geometric reallocations, with no SIMD/SWAR across comma boundaries (`restart/skinny/tranches/sk-v7/research/skv7-C3-typed-profile.md:266`, `restart/skinny/tranches/sk-v7/research/skv7-C3-typed-profile.md:273`, `restart/skinny/tranches/sk-v7/research/skv7-C3-typed-profile.md:279`). The synthesis says current codegen would place mesh around 91.8% sonic and must sequence after `DirectTypeRef::Vec(DirectScalar::F64|U32)` specialization (`restart/skinny/tranches/sk-v7/SYNTHESIS.md:109`, `restart/skinny/tranches/sk-v7/SYNTHESIS.md:111`, `restart/skinny/tranches/sk-v7/SYNTHESIS.md:114`).

2. Do not reopen capacity prescan. HANDOFF §3 pre-blocks capacity prescan (`restart/skinny/tranches/sk-v7/HANDOFF.md:84`, `restart/skinny/tranches/sk-v7/HANDOFF.md:88`). The allowed path is a direct schema hint supplied from fixture/schema knowledge, not scanning every array to discover capacity at parse time.

3. Avoid generic broad routes while closing this narrow schema gap. HANDOFF §3 pre-blocks retained/direct materialization routes including hand-authored real typed sinks and direct source-hook field-layout materializers (`restart/skinny/tranches/sk-v7/HANDOFF.md:75`, `restart/skinny/tranches/sk-v7/HANDOFF.md:78`). W3 should stay in generated schema/codegen, not bench-private hand parsing.

4. Helper key churn can silently alter unrelated generated functions. Because helper names are derived from `type_key()` and then snake-cased (`skinny/crates/codegen/src/json_typed_direct.rs:221`, `skinny/crates/codegen/src/json_typed_direct.rs:222`, `skinny/crates/codegen/src/json_typed_direct.rs:373`), include snapshot/generation review for generated helper names after adding hints.

## Sources

- `restart/skinny/tranches/sk-v7/SPEC.md:179-212`
- `restart/skinny/tranches/sk-v7/HANDOFF.md:30-36`
- `restart/skinny/tranches/sk-v7/HANDOFF.md:66-89`
- `restart/skinny/tranches/sk-v7/SYNTHESIS.md:109-118`
- `restart/skinny/tranches/sk-v7/research/skv7-C3-typed-profile.md:241-320`
- `restart/skinny/tranches/sk-v7/research/skv7-C3-typed-profile.md:399-430`
- `skinny/crates/codegen/src/direct_schema.rs:60-74`
- `skinny/crates/codegen/src/direct_schema.rs:201-218`
- `skinny/crates/codegen/src/json_typed_direct.rs:166-202`
- `skinny/crates/codegen/src/json_typed_direct.rs:213-225`
- `skinny/crates/codegen/src/json_typed_direct.rs:298-370`
- `skinny/crates/codegen/src/json_typed_direct.rs:507-535`
- `skinny/crates/codegen/src/lower/sink_only.rs:95-123`
- `skinny/crates/codegen/src/lower/sink_only.rs:125-181`
- `skinny/xtask/src/real_typed_schema.rs:147-177`
- `skinny/crates/codegen/src/lib.rs:372-408`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs:9-80`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs:111-174`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:286-349`
