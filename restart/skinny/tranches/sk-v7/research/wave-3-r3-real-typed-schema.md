# SK-V7 W3 Phase 1 Research — real_typed_schema for mesh/marine_ik

Scope: determine the schema and regeneration work needed to admit `mesh` and
`marine_ik` into the `real_typed_struct` workload. No source edits performed.

## Verdict

`xtask/src/real_typed_schema.rs` currently owns only two real typed roots:
`parse_twitter_search` and `parse_update_center` (`skinny/xtask/src/real_typed_schema.rs:7-22`).
The generated module reflects that exact schema hash and root set:
`generated_real_typed.rs` has only `parse_twitter_search` and
`parse_update_center` (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1-4`,
`:31-51`).

For W3, add `mesh` and `marine_ik` as real typed roots, but do not treat this
as an xtask-only edit. The schema additions require matching host structs,
fixture dispatch/checksum arms, and `DirectTypeRef::Vec` capacity support before
`regen-real-typed` can produce useful generated code. The SK-V7 packet names
this directly: W3 targets mesh and marine_ik real_typed_struct PASS
(`restart/skinny/tranches/sk-v7/SPEC.md:32-33`) and owner paths include
`DirectTypeRef::Vec`, `json_typed_direct`, `real_typed_schema`, and
`generated_real_typed` (`restart/skinny/tranches/sk-v7/SPEC.md:179-195`).

## Current Machinery

- `xtask` wires `regen-real-typed` and `check-real-typed` through
  `real_typed_schema::schema()` and writes/checks under
  `crates/bbnf-bench/src` (`skinny/xtask/src/main.rs:135-151`).
- `DirectTypeRef::Vec` currently has no capacity hint, while
  `MapEntriesVec` already does (`skinny/crates/codegen/src/direct_schema.rs:60-74`).
- The Vec helper emits `Vec::new()` for every `Vec<T>`
  (`skinny/crates/codegen/src/json_typed_direct.rs:306-315`), whereas
  `MapEntriesVec` emits `Vec::with_capacity(capacity_hint.unwrap_or(0))`
  (`skinny/crates/codegen/src/json_typed_direct.rs:326-341`).
- Helper identity currently ignores capacity for all Vecs because
  `type_key()` is `vec_{inner}` (`skinny/crates/codegen/src/json_typed_direct.rs:358-369`).
- The bench harness already auto-adds real typed benches when
  `fixture_for_name()` returns a fixture (`skinny/crates/bbnf-bench/benches/json_parity.rs:20-27`,
  `:261-351`).
- `gate-json` already reads and classifies `track1_real_typed_struct`,
  `track2_real_typed_struct`, `sonic_rs_real_typed_struct`, and
  `serde_json_real_typed_struct` (`skinny/crates/bbnf-bench/src/bin/gate.rs:127-164`,
  `:280-295`, `:510-522`).

## Required Owner Edits

### 1. Codegen schema model

Owner: `skinny/crates/codegen/src/direct_schema.rs`.

Change `DirectTypeRef::Vec(Box<DirectTypeRef>)` into a capacity-hinted shape,
for example:

```rust
Vec {
    inner: Box<DirectTypeRef>,
    capacity_hint: Option<usize>,
}
```

Then update validation destructures that currently match `Vec(inner)` at
`skinny/crates/codegen/src/direct_schema.rs:214-217`.

### 2. Typed direct renderer

Owner: `skinny/crates/codegen/src/json_typed_direct.rs`.

Update every `DirectTypeRef::Vec(inner)` destructure in:

- Rust type rendering (`skinny/crates/codegen/src/json_typed_direct.rs:166-187`).
- Parse expression/helper collection (`skinny/crates/codegen/src/json_typed_direct.rs:190-229`).
- Helper emission (`skinny/crates/codegen/src/json_typed_direct.rs:298-315`).
- Helper keying (`skinny/crates/codegen/src/json_typed_direct.rs:358-369`).

The helper emission should mirror `MapEntriesVec`: use
`Vec::with_capacity(capacity_hint.unwrap_or(0))`. Include the capacity hint in
`type_key()` so `Vec<f64>` helpers with different known lengths do not collide.
SK-V7 §5 explicitly calls out the helper-collision risk and asks for
`type_key()` to include `capacity_hint` (`restart/skinny/tranches/sk-v7/SPEC.md:188-192`).

### 3. Host real typed structs and dispatch

Owner: `skinny/crates/bbnf-bench/src/real_typed_struct.rs`.

Current fixture enum/output/dispatch covers only Twitter and UpdateCenter
(`skinny/crates/bbnf-bench/src/real_typed_struct.rs:9-13`, `:69-80`,
`:111-158`, `:176-180`). Add:

- `RealTypedFixture::{Mesh, MarineIk}`.
- `fixture_for_name()` arms for `"mesh"` and `"marine_ik"`.
- `RealTypedOutput::{Mesh(Mesh), MarineIk(MarineIk<'a>)}` or an owned
  `MarineIk` if the chosen schema keeps no borrowed strings.
- `track1_typed`, `serde_typed`, and `sonic_typed` arms calling the generated
  roots and sidecars.
- Checksums for both outputs; fold numeric vectors by length and element bits,
  and fold retained strings/bools for marine_ik if retained.
- Unit tests paralleling the existing real typed sidecar tests
  (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:281-298`).

Recommended mesh host shape is the existing B5 design: `Mesh` with
`batches`, `positions`, `tex0`, `colors`, `influences`, `normals`, and
`indices`, plus `MeshBatch` for `indexRange`, `vertexRange`, and `usedBones`
(`restart/skinny/tranches/sk-v7/research/skv7-B5-mesh-typed.md:39-78`).
Correct one detail: `influences` in the actual fixture is `Vec<Vec<f64>>`, not
flat `Vec<f64>`; the fixture walk shows `influences` length 3600 and each
element length 2.

For `marine_ik`, use a deliberately numeric-heavy subset that matches the
target row and avoids retaining the whole 2.8 MiB object:

- `MarineIk { geometries: Vec<MarineGeometry>, object: MarineObject }` plus
  optional small top-level metadata if checksum needs it.
- `MarineGeometry { data: MarineGeometryData }`, retaining `data.uvs:
  Vec<Vec<f64>>`, `vertices: Vec<f64>`, `skinWeights: Vec<f64>`,
  `skinIndices: Vec<u64>`, `normals: Vec<f64>`, `faces: Vec<u64>`,
  `bones: Vec<MarineBone>`, and `animations: Vec<MarineDataAnimation>`.
- `MarineObject { matrix: Vec<f64>, children: Vec<MarineObjectChild> }`.
- Skip high-string side tables (`images`, `textures`, `materials`,
  root `animations`) unless a parity/checksum requirement needs them.

The observed marine_ik shape is stable and numeric-heavy: `vertices` 17220,
`skinWeights` 11480, `skinIndices` 11480, `normals` 17208, `faces` 74087,
`bones` 64, and animation hierarchy/key vectors under `geometries[].data`
(`skinny/test_data/marine_ik.json`; manifest size/hash at
`skinny/crates/test-fixtures/corpus/json/manifest.toml:55-59`).

### 4. xtask real typed schema

Owner: `skinny/xtask/src/real_typed_schema.rs`.

Add roots after the current two roots:

```rust
DirectRootSchema {
    function_name: "parse_mesh".to_string(),
    rust_type: "crate::real_typed_struct::Mesh".to_string(),
    type_id: "Mesh".to_string(),
},
DirectRootSchema {
    function_name: "parse_marine_ik".to_string(),
    rust_type: "crate::real_typed_struct::MarineIk<'i>".to_string(),
    type_id: "MarineIk".to_string(),
},
```

Add helpers:

```rust
fn f64_ty() -> DirectTypeRef { DirectTypeRef::Scalar(DirectScalar::F64) }
fn bool_ty() -> DirectTypeRef { DirectTypeRef::Scalar(DirectScalar::Bool) }
fn vec_with_capacity(inner: DirectTypeRef, hint: usize) -> DirectTypeRef {
    DirectTypeRef::Vec {
        inner: Box::new(inner),
        capacity_hint: Some(hint),
    }
}
```

Keep `vec(inner)` as a no-hint wrapper for existing Twitter/UpdateCenter uses
(`skinny/xtask/src/real_typed_schema.rs:159-161`). Update schema hash from
`sk-v6-real-typed-v1` (`skinny/xtask/src/real_typed_schema.rs:7-11`) to a
W3-specific value.

Mesh schema entries:

- `Mesh`: `batches: Vec<MeshBatch>` hint 1; `positions: Vec<f64>` hint 10800;
  `tex0: Vec<f64>` hint 7200; `colors: Vec<f64>` hint 3600;
  `influences: Vec<Vec<f64>>` outer hint 3600, inner hint 2; `normals:
  Vec<f64>` hint 10800; `indices: Vec<u64>` hint 33408.
- Ignore `morphTargets` as `DirectSkipKind::Object`.
- `MeshBatch`: `indexRange` and `vertexRange` as `Vec<u64>` hint 2;
  `usedBones` as `Vec<u64>` hint 1 or 4.

Marine_ik schema entries:

- `MarineIk`: retain `geometries: Vec<MarineGeometry>` hint 1 and
  `object: MarineObject`; ignore `images`, `textures`, `metadata`,
  `materials`, and root `animations` with the appropriate skip kinds.
- `MarineGeometry`: retain `data: MarineGeometryData`; optionally retain
  `type`/`uuid` only if the host checksum needs string side evidence.
- `MarineGeometryData`: retain the numeric vectors named above; ignore
  `metadata` and `name` if not retained.
- Nested data animation/bone/key structs: keep numeric arrays (`pos`, `scl`,
  `rot`/`rotq`) and skip retained strings unless needed.
- Use capacity hints from the measured fixture: `uvs` outer 1 inner 10532,
  `vertices` 17220, `skinWeights` 11480, `skinIndices` 11480, `normals` 17208,
  `faces` 74087, `bones` 64, `object.matrix` 16, child matrices 16.

## Regeneration and Check Commands

Run from `/Users/mkbabb/Programming/bbnf-lang/skinny`.

1. `cargo check -p codegen --profile ax-iter`
2. `cargo check -p bbnf-bench --profile ax-iter`
3. `cargo run -p xtask -- regen-real-typed`
4. `cargo run -p xtask -- check-real-typed`
5. `cargo test -p bbnf-bench real_typed_struct -- --nocapture`
6. `cargo run -p xtask -- check-conformance`
7. Focused smoke/profile:
   `cargo run -p bbnf-bench --release --bin profile_direct -- 1000 mesh real_typed_track1`
   and
   `cargo run -p bbnf-bench --release --bin profile_direct -- 250 marine_ik real_typed_track1`
   (`profile_direct` supports these real typed modes at
   `skinny/crates/bbnf-bench/src/bin/profile_direct.rs:57-93`).
8. Focused criterion:
   `cargo bench -p bbnf-bench --bench json_parity -- json/mesh`
   and
   `cargo bench -p bbnf-bench --bench json_parity -- json/marine_ik`.
9. Full exit protocol:
   `cargo run -p xtask --release -- check-conformance`,
   `cargo run -p xtask --release -- bench-json`,
   `cargo run -p xtask --release -- gate-json`, as required by SK-V7
   (`restart/skinny/tranches/sk-v7/SPEC.md:72-81`).

After regeneration, inspect
`skinny/crates/bbnf-bench/src/generated_real_typed.rs` for:

- `parse_mesh` and `parse_marine_ik` roots.
- `schema_hash` updated from `sk-v6-real-typed-v1`.
- Literal `Vec::with_capacity(10800)`, `Vec::with_capacity(33408)`, and
  `Vec::with_capacity(74087)` helpers.
- Distinct helpers for different `Vec<f64>` capacity hints.

## Correctness Gates

- `cargo run -p xtask -- check-real-typed` must be clean; this verifies the
  generated file is not stale (`skinny/xtask/src/main.rs:143-151`).
- Real typed sidecars must match Track 1, Track 2, serde_json, and sonic-rs
  checksums. The existing oracle compares all four checksums
  (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:160-174`).
- `gate-json` must emit `real_typed_struct` rows for both `mesh` and
  `marine_ik`; its classifier marks rows as direct projection failures when
  Track 1 exceeds sonic by the configured slack (`skinny/crates/bbnf-bench/src/bin/gate.rs:280-295`).
- SK-V7 W3 falsifiability requires both `mesh real_typed_struct` and
  `marine_ik real_typed_struct` at >=100% sonic-strict, plus no regression to
  existing mesh direct_to_struct and twitter real_typed_struct rows
  (`restart/skinny/tranches/sk-v7/SPEC.md:196-204`).

## Risks

- Schema-only admission will fail to compile until host structs and dispatch
  arms exist; the generated roots return `crate::real_typed_struct::*` types.
- Without capacity in `type_key()`, multiple `Vec<f64>` helpers may silently
  share the wrong capacity. This is explicitly called out by the packet and by
  the earlier B5 report (`restart/skinny/tranches/sk-v7/SPEC.md:188-192`;
  `restart/skinny/tranches/sk-v7/research/skv7-B5-mesh-typed.md:435-443`).
- `mesh` B5 notes list `influences` as flat `Vec<f64>`, but the fixture is
  nested `Vec<Vec<f64>>`. Implementing the flat shape would make serde/sonic
  parity fail.
- `marine_ik` can explode LOC if modeled fully. Keep a numeric-heavy retained
  subset and let unknown fields skip through `UnknownFieldPolicy::Skip`, which
  the renderer emits as `_ => parser.skip_value()?`
  (`skinny/crates/codegen/src/json_typed_direct.rs:102-110`).
- `bbnf-bench` has a tight LOC budget and a warning cliff near 3250 LOC
  (`skinny/xtask/src/main.rs:154-177`); prefer compact checksums and avoid
  full mirror structs for skipped fields.

## Recommended Sequence

1. Land `DirectTypeRef::Vec { inner, capacity_hint }` and renderer/keying
   support first.
2. Add host `Mesh` and `MarineIk` structs/dispatch/checksum/tests.
3. Add xtask roots/types/helpers and bump schema hash.
4. Regenerate `generated_real_typed.rs`.
5. Run focused parity/profile, then focused benches for `json/mesh` and
   `json/marine_ik`.
6. Run full SK-V7 exit protocol and update `RESULTS.md`/`REDRESS.md` only if
   W3 falsifiability passes.
