# SK-V7 W3 Phase 1 Research: real_typed_struct mesh + marine_ik bench wiring

Scope: `bbnf-bench` real_typed_struct harness and fixture wiring only.
No source edits in this research pass.

## SPEC anchor

`restart/skinny/tranches/sk-v7/SPEC.md` §5 asks Wave 3 to add B5
mesh DirectBuild + `DirectTypeRef::Vec` specialisation, then wire and
bench `mesh` plus `marine_ik` through `real_typed_struct`.

The relevant §5 gates are:

| fixture | workload | gate |
| --- | --- | --- |
| `mesh` | `real_typed_struct` | Track 1 >= 100% sonic-strict |
| `marine_ik` | `real_typed_struct` | Track 1 >= 100% sonic-strict |
| `mesh` | existing `direct_to_struct` | stays >= 91.8% |
| `twitter` | existing `real_typed_struct` | stays >= 151.5% |

Current `skinny/RESULTS.md` has real typed rows only for `twitter` and
`update_center`. `mesh` and `marine_ik` currently have parse/direct rows
but no real typed rows:

| fixture | parse Track 1 | parse sonic strict | direct Track 1 | direct sonic | current real typed |
| --- | ---: | ---: | ---: | ---: | --- |
| `mesh` | 14265 | 11754 | 8798 | 9902 | absent |
| `marine_ik` | 13797 | 10070 | 9391 | 8465 | absent |

## Current harness behavior

`skinny/crates/bbnf-bench/benches/json_parity.rs` already has the
complete benchmark block for real typed workloads. The block is guarded
only by:

```rust
if let Some(real_typed) = bbnf_bench::real_typed_struct::fixture_for_name(&fixture.name) {
    ...
}
```

Once `fixture_for_name("mesh")` and `fixture_for_name("marine_ik")`
return `Some`, the harness automatically emits these four Criterion
benchmarks per fixture:

| benchmark function | row metadata |
| --- | --- |
| `track1_real_typed_struct` | `BenchFacts::bbnf_json_workload(..., TrackTag::Track1Generated, "real_typed_struct", ...)` |
| `track2_real_typed_struct` | `BenchFacts::bbnf_json_workload(..., TrackTag::Track2Handcoded, "real_typed_struct", ...)` |
| `sonic_rs_real_typed_struct` | competitor `sonic-rs`, version `0.5.8`, materialisation `real_typed_struct` |
| `serde_json_real_typed_struct` | competitor `serde_json`, version `workspace`, materialisation `real_typed_struct` |

No `json_parity.rs` source change is needed for the bench functions
themselves. The required additions are in `real_typed_struct.rs`,
`generated_real_typed.rs` regeneration, and the schema source that feeds
the generated module.

`skinny/crates/bbnf-bench/src/bin/gate.rs` already reads the four
Criterion paths:

```rust
real_typed_track1: read_slope_ns(&group, "track1_real_typed_struct"),
real_typed_track2: read_slope_ns(&group, "track2_real_typed_struct"),
real_typed_sonic: read_slope_ns(&group, "sonic_rs_real_typed_struct"),
real_typed_serde: read_slope_ns(&group, "serde_json_real_typed_struct"),
```

It emits a `real_typed_struct` result row when
`estimates.real_typed_track1.is_some()`. No gate/report code is needed
unless the Wave changes the row policy.

## Fixture enum and dispatch additions

Current enum:

```rust
pub enum RealTypedFixture {
    Twitter,
    UpdateCenter,
}
```

Required enum:

```rust
pub enum RealTypedFixture {
    Twitter,
    UpdateCenter,
    Mesh,
    MarineIk,
}
```

Required `fixture_for_name` arms:

```rust
"mesh" => Some(RealTypedFixture::Mesh),
"marine_ik" | "marine-ik" => Some(RealTypedFixture::MarineIk),
```

Required `candidate_names` arm if hyphenated aliases are accepted:

```rust
"marine_ik" => ["marine_ik", "marine-ik"],
"marine-ik" => ["marine_ik", "marine-ik"],
```

Required `RealTypedOutput` arms:

```rust
pub enum RealTypedOutput<'a> {
    Twitter(TwitterSearch<'a>),
    UpdateCenter(UpdateCenter<'a>),
    Mesh(Mesh),
    MarineIk(MarineIk<'a>),
}
```

`Mesh` can be owned because its proposed checked fields are numeric
only. `MarineIk<'a>` should borrow strings if the struct includes names
or UUIDs; the minimal heavy-array schema below can also be owned if all
string fields are skipped.

Required `track1_typed` arms:

```rust
RealTypedFixture::Mesh => crate::generated_real_typed::parse_mesh(input)
    .map(RealTypedOutput::Mesh)
    .map_err(|error| DirectStructError::Parse(error.to_string())),
RealTypedFixture::MarineIk => crate::generated_real_typed::parse_marine_ik(input)
    .map(RealTypedOutput::MarineIk)
    .map_err(|error| DirectStructError::Parse(error.to_string())),
```

Required `serde_typed` arms:

```rust
RealTypedFixture::Mesh => serde_json::from_slice::<Mesh>(bytes)
    .map(RealTypedOutput::Mesh)
    .map_err(|error| DirectStructError::Serde(error.to_string())),
RealTypedFixture::MarineIk => serde_json::from_slice::<MarineIk<'a>>(bytes)
    .map(RealTypedOutput::MarineIk)
    .map_err(|error| DirectStructError::Serde(error.to_string())),
```

Required `sonic_typed` arms:

```rust
RealTypedFixture::Mesh => sonic_rs::from_slice::<Mesh>(bytes)
    .map(RealTypedOutput::Mesh)
    .map_err(|error| DirectStructError::Sonic(error.to_string())),
RealTypedFixture::MarineIk => sonic_rs::from_slice::<MarineIk<'a>>(bytes)
    .map(RealTypedOutput::MarineIk)
    .map_err(|error| DirectStructError::Sonic(error.to_string())),
```

`track2_typed` already delegates to `serde_typed`, so no separate
Track 2 parser arm is required beyond the `serde_typed` arm.

## Mesh exact typed shape

Observed `skinny/test_data/mesh.json` shape:

| key | shape |
| --- | --- |
| `batches` | array len 1 of `{ indexRange, vertexRange, usedBones }` |
| `morphTargets` | empty object |
| `positions` | `Vec<f64>`, len 10800 |
| `tex0` | `Vec<f64>`, len 7200 |
| `colors` | `Vec<u64>`, len 3600 |
| `influences` | `Vec<Vec<f64>>`, len 3600 outer, len 2 inner |
| `normals` | `Vec<f64>`, len 10800 |
| `indices` | `Vec<u64>`, len 33408 |

Recommended host structs:

```rust
#[derive(Debug, Deserialize)]
pub struct Mesh {
    #[serde(default)]
    pub batches: Vec<MeshBatch>,
    #[serde(default)]
    pub positions: Vec<f64>,
    #[serde(default)]
    pub tex0: Vec<f64>,
    #[serde(default)]
    pub colors: Vec<u64>,
    #[serde(default)]
    pub influences: Vec<Vec<f64>>,
    #[serde(default)]
    pub normals: Vec<f64>,
    #[serde(default)]
    pub indices: Vec<u64>,
}

#[derive(Debug, Deserialize)]
pub struct MeshBatch {
    #[serde(default, rename = "indexRange")]
    pub index_range: Vec<u64>,
    #[serde(default, rename = "vertexRange")]
    pub vertex_range: Vec<u64>,
    #[serde(default, rename = "usedBones")]
    pub used_bones: Vec<u64>,
}
```

Schema root to add to `skinny/xtask/src/real_typed_schema.rs`:

```rust
DirectRootSchema {
    function_name: "parse_mesh".to_string(),
    rust_type: "crate::real_typed_struct::Mesh".to_string(),
    type_id: "Mesh".to_string(),
}
```

Schema types:

```rust
struct_ty(
    "Mesh",
    "crate::real_typed_struct::Mesh",
    vec![
        default("batches", "batches", vec_with_capacity(ty("MeshBatch"), 1)),
        default("positions", "positions", vec_with_capacity(f64_ty(), 10_800)),
        default("tex0", "tex0", vec_with_capacity(f64_ty(), 7_200)),
        default("colors", "colors", vec_with_capacity(u64_ty(), 3_600)),
        default(
            "influences",
            "influences",
            vec_with_capacity(vec_with_capacity(f64_ty(), 2), 3_600),
        ),
        default("normals", "normals", vec_with_capacity(f64_ty(), 10_800)),
        default("indices", "indices", vec_with_capacity(u64_ty(), 33_408)),
    ],
)
.with_ignored_fields(vec![ignored("morphTargets", DirectSkipKind::Object)]),
struct_ty(
    "MeshBatch",
    "crate::real_typed_struct::MeshBatch",
    vec![
        default("indexRange", "index_range", vec_with_capacity(u64_ty(), 2)),
        default("vertexRange", "vertex_range", vec_with_capacity(u64_ty(), 2)),
        default("usedBones", "used_bones", vec_with_capacity(u64_ty(), 4)),
    ],
),
```

Generated module additions expected after regen:

| generated symbol | purpose |
| --- | --- |
| `parse_mesh` | Track 1 entrypoint |
| `parse_type_mesh` | root object parser |
| `parse_type_mesh_batch` | nested batch parser |
| `parse_vec_*_scalar_f64` | numeric array helpers with capacity hints |
| `parse_vec_*_scalar_u64` | integer array helpers with capacity hints |
| `parse_vec_*_vec_*_scalar_f64` | `influences` nested array helper |

## Marine exact typed shape

Observed `skinny/test_data/marine_ik.json` heavy numeric shape:

| path | shape |
| --- | --- |
| `geometries[0].data.uvs[0]` | `Vec<f64>`, len 10532 |
| `geometries[0].data.vertices` | `Vec<f64>`, len 17220 |
| `geometries[0].data.skinWeights` | `Vec<f64>`, len 11480, mixed JSON ints/floats |
| `geometries[0].data.skinIndices` | `Vec<u64>`, len 11480 |
| `geometries[0].data.normals` | `Vec<f64>`, len 17208, mostly floats with 3 ints |
| `geometries[0].data.faces` | `Vec<u64>`, len 74087 |
| `geometries[0].data.bones` | len 64; `pos[3]`, `rotq[4]`, `scl[3]`, `parent` |
| `geometries[0].data.animations` | len 5; each has `hierarchy[64]` with key arrays |

Phase 1 should keep the marine schema narrow and numeric-heavy:
`geometries -> data -> { uvs, vertices, skinWeights, skinIndices,
normals, faces }`. This avoids overfitting the full three.js object
surface while still covering the dominant numeric arrays.

Recommended host structs:

```rust
#[derive(Debug, Deserialize)]
pub struct MarineIk<'a> {
    #[serde(default, borrow)]
    pub geometries: Vec<MarineGeometry<'a>>,
}

#[derive(Debug, Deserialize)]
pub struct MarineGeometry<'a> {
    #[serde(default, borrow)]
    pub data: Option<MarineGeometryData<'a>>,
}

#[derive(Debug, Deserialize)]
pub struct MarineGeometryData<'a> {
    #[serde(default, borrow)]
    pub uvs: Vec<Vec<f64>>,
    #[serde(default)]
    pub vertices: Vec<f64>,
    #[serde(default, rename = "skinWeights")]
    pub skin_weights: Vec<f64>,
    #[serde(default, rename = "skinIndices")]
    pub skin_indices: Vec<u64>,
    #[serde(default)]
    pub normals: Vec<f64>,
    #[serde(default)]
    pub faces: Vec<u64>,
    #[serde(skip)]
    _borrow: std::marker::PhantomData<&'a ()>,
}
```

The `PhantomData` is needed only if keeping the lifetime on this minimal
numeric schema. Simpler alternative: make `MarineIk`, `MarineGeometry`,
and `MarineGeometryData` owned with no lifetime. If no strings are kept,
the owned variant is cleaner.

Schema root:

```rust
DirectRootSchema {
    function_name: "parse_marine_ik".to_string(),
    rust_type: "crate::real_typed_struct::MarineIk".to_string(),
    type_id: "MarineIk".to_string(),
}
```

Schema types:

```rust
struct_ty(
    "MarineIk",
    "crate::real_typed_struct::MarineIk",
    vec![default(
        "geometries",
        "geometries",
        vec_with_capacity(ty("MarineGeometry"), 1),
    )],
)
.with_ignored_fields(vec![
    ignored("images", DirectSkipKind::Array),
    ignored("textures", DirectSkipKind::Array),
    ignored("metadata", DirectSkipKind::Object),
    ignored("materials", DirectSkipKind::Array),
    ignored("object", DirectSkipKind::Object),
    ignored("animations", DirectSkipKind::Array),
]),
struct_ty(
    "MarineGeometry",
    "crate::real_typed_struct::MarineGeometry",
    vec![default("data", "data", opt(ty("MarineGeometryData")))],
)
.with_ignored_fields(vec![
    ignored("type", DirectSkipKind::String),
    ignored("uuid", DirectSkipKind::String),
]),
struct_ty(
    "MarineGeometryData",
    "crate::real_typed_struct::MarineGeometryData",
    vec![
        default("uvs", "uvs", vec_with_capacity(vec_with_capacity(f64_ty(), 10_532), 1)),
        default("vertices", "vertices", vec_with_capacity(f64_ty(), 17_220)),
        default("skinWeights", "skin_weights", vec_with_capacity(f64_ty(), 11_480)),
        default("skinIndices", "skin_indices", vec_with_capacity(u64_ty(), 11_480)),
        default("normals", "normals", vec_with_capacity(f64_ty(), 17_208)),
        default("faces", "faces", vec_with_capacity(u64_ty(), 74_087)),
    ],
)
.with_ignored_fields(vec![
    ignored("animations", DirectSkipKind::Array),
    ignored("metadata", DirectSkipKind::Object),
    ignored("name", DirectSkipKind::String),
    ignored("influencesPerVertex", DirectSkipKind::Number),
    ignored("bones", DirectSkipKind::Array),
]),
```

Generated module additions expected after regen:

| generated symbol | purpose |
| --- | --- |
| `parse_marine_ik` | Track 1 entrypoint |
| `parse_type_marine_ik` | root object parser |
| `parse_type_marine_geometry` | `geometries[]` parser |
| `parse_type_marine_geometry_data` | dominant numeric-array parser |
| capacity-hinted `Vec<f64>` helpers | `uvs`, `vertices`, `skinWeights`, `normals` |
| capacity-hinted `Vec<u64>` helpers | `skinIndices`, `faces` |

## Checksum additions

`typed_checksum` must add:

```rust
RealTypedOutput::Mesh(value) => checksum_mesh(value),
RealTypedOutput::MarineIk(value) => checksum_marine_ik(value),
```

Mesh checksum:

```rust
fn checksum_mesh(value: &Mesh) -> u64 {
    let mut hash = mix(0x6d657368, value.batches.len() as u64);
    for batch in &value.batches {
        hash = mix(hash, checksum_mesh_batch(batch));
    }
    hash = fold_f64_vec(hash, &value.positions);
    hash = fold_f64_vec(hash, &value.tex0);
    hash = fold_u64_vec(hash, &value.colors);
    hash = fold_f64_vec_vec(hash, &value.influences);
    hash = fold_f64_vec(hash, &value.normals);
    fold_u64_vec(hash, &value.indices)
}

fn checksum_mesh_batch(value: &MeshBatch) -> u64 {
    let mut hash = 0x6261746368;
    hash = fold_u64_vec(hash, &value.index_range);
    hash = fold_u64_vec(hash, &value.vertex_range);
    fold_u64_vec(hash, &value.used_bones)
}
```

Marine checksum:

```rust
fn checksum_marine_ik(value: &MarineIk) -> u64 {
    let mut hash = mix(0x6d6172696e655f696b, value.geometries.len() as u64);
    for geometry in &value.geometries {
        hash = mix(hash, checksum_marine_geometry(geometry));
    }
    hash
}

fn checksum_marine_geometry(value: &MarineGeometry) -> u64 {
    let mut hash = 0x67656f6d;
    if let Some(data) = &value.data {
        hash = mix(hash, checksum_marine_geometry_data(data));
    }
    hash
}

fn checksum_marine_geometry_data(value: &MarineGeometryData) -> u64 {
    let mut hash = 0x67656f6d5f64617461;
    hash = fold_f64_vec_vec(hash, &value.uvs);
    hash = fold_f64_vec(hash, &value.vertices);
    hash = fold_f64_vec(hash, &value.skin_weights);
    hash = fold_u64_vec(hash, &value.skin_indices);
    hash = fold_f64_vec(hash, &value.normals);
    fold_u64_vec(hash, &value.faces)
}
```

Shared helpers:

```rust
fn fold_f64_vec(mut hash: u64, values: &[f64]) -> u64 {
    hash = mix(hash, values.len() as u64);
    for value in values {
        hash = mix(hash, value.to_bits());
    }
    hash
}

fn fold_f64_vec_vec(mut hash: u64, values: &[Vec<f64>]) -> u64 {
    hash = mix(hash, values.len() as u64);
    for value in values {
        hash = fold_f64_vec(hash, value);
    }
    hash
}

fn fold_u64_vec(mut hash: u64, values: &[u64]) -> u64 {
    hash = mix(hash, values.len() as u64);
    for value in values {
        hash = mix(hash, *value);
    }
    hash
}
```

Checksum rationale: it is deterministic across Track 1, Track 2,
`serde_json`, and `sonic-rs`; it validates all dominant numeric arrays
without making Track 2 a performance gate.

## Bench additions by row

With the enum and dispatch additions in place, `json_parity.rs` emits
these exact new benchmark paths and metadata rows:

| fixture | Criterion group | benchmark | row workload | track/comparator |
| --- | --- | --- | --- | --- |
| `mesh` | `json/mesh` | `track1_real_typed_struct` | `real_typed_struct` | `TrackTag::Track1Generated` |
| `mesh` | `json/mesh` | `track2_real_typed_struct` | `real_typed_struct` | `TrackTag::Track2Handcoded` |
| `mesh` | `json/mesh` | `sonic_rs_real_typed_struct` | `real_typed_struct` | `sonic-rs 0.5.8` |
| `mesh` | `json/mesh` | `serde_json_real_typed_struct` | `real_typed_struct` | `serde_json workspace` |
| `marine_ik` | `json/marine_ik` | `track1_real_typed_struct` | `real_typed_struct` | `TrackTag::Track1Generated` |
| `marine_ik` | `json/marine_ik` | `track2_real_typed_struct` | `real_typed_struct` | `TrackTag::Track2Handcoded` |
| `marine_ik` | `json/marine_ik` | `sonic_rs_real_typed_struct` | `real_typed_struct` | `sonic-rs 0.5.8` |
| `marine_ik` | `json/marine_ik` | `serde_json_real_typed_struct` | `real_typed_struct` | `serde_json workspace` |

Expected metadata files:

```text
target/criterion/json_mesh/track1_real_typed_struct/metadata.toml
target/criterion/json_mesh/track2_real_typed_struct/metadata.toml
target/criterion/json_mesh/sonic_rs_real_typed_struct/metadata.toml
target/criterion/json_mesh/serde_json_real_typed_struct/metadata.toml
target/criterion/json_marine_ik/track1_real_typed_struct/metadata.toml
target/criterion/json_marine_ik/track2_real_typed_struct/metadata.toml
target/criterion/json_marine_ik/sonic_rs_real_typed_struct/metadata.toml
target/criterion/json_marine_ik/serde_json_real_typed_struct/metadata.toml
```

Expected `RESULTS.md` row class:

```text
| mesh | real_typed_struct | ... | typed direct | Track 1 ... | Track 2 ... | sonic-rs strict ... | ... | serde_json ... |
| marine_ik | real_typed_struct | ... | typed direct | Track 1 ... | Track 2 ... | sonic-rs strict ... | ... | serde_json ... |
```

## Verification commands for implementation phase

After implementing the wiring and regenerating `generated_real_typed.rs`:

```sh
cargo test -p bbnf-bench real_typed_struct
cargo bench -p bbnf-bench --bench json_parity -- json/mesh
cargo bench -p bbnf-bench --bench json_parity -- json/marine_ik
cargo run -p bbnf-bench --bin gate --release
```

Gate checks to record:

```text
mesh real_typed_struct Track 1 Mbps >= sonic_rs_real_typed_struct Mbps
marine_ik real_typed_struct Track 1 Mbps >= sonic_rs_real_typed_struct Mbps
mesh direct_to_struct Track 1 remains >= 91.8% of sonic_rs_direct_to_struct
twitter real_typed_struct Track 1 remains >= 151.5% of sonic_rs_real_typed_struct
```

## Implementation note

This Phase 1 research found no missing bench-harness function for
Track 1, Track 2, sonic, or serde. The additions are exactly:

1. Add `Mesh` and `MarineIk` fixtures to `RealTypedFixture`.
2. Add fixture-name aliases.
3. Add `RealTypedOutput` variants.
4. Add Track 1 generated-parser dispatch arms.
5. Add serde/Track 2 dispatch arms.
6. Add sonic dispatch arms.
7. Add checksums for the selected typed fields.
8. Add schema roots/types and regenerate `generated_real_typed.rs`.

The `json_parity.rs` benchmark block and gate/report readers already
consume those additions automatically.
