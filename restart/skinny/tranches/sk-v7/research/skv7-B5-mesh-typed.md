# SK-V7 B5 — Mesh DirectBuild + typed-output product-plane expansion

Status: design, pre-implementation.
Pre-req: SK-V6 Wave 3 admitted host-output-schema typed DirectBuild
(commit `ab06ff11`). Twitter + update_center real_typed_struct rows are
landed; cohort C2 (`restart/skinny/tranches/sk-v6/research/skv6-C2-direct-profile.md`)
nominates `mesh` as the third typed product fixture.

A4 ranking carries this as Top 2: "mesh real_typed_struct DirectBuild
schema — extends twitter/update_center typed-output proof to
numeric-array-heavy corpus via codegen + host schema fixture; no
parse-that-regex change. ~100-200 LOC, all outside the crate."

## 1. Corpus shape

`skinny/test_data/mesh.json` (723597 bytes; manifest entry at
`skinny/crates/test-fixtures/corpus/json/manifest.toml:37-41`). Top-level
keys, measured by `python3 -c 'import json; d=json.load(open(...))'`:

| key            | type   | length | leaf shape                                       |
| -------------- | ------ | ------ | ------------------------------------------------ |
| `batches`      | array  | 1      | `Batch { indexRange:[u32;2], vertexRange:[u32;2], usedBones:[u32] }` |
| `morphTargets` | object | 0      | empty object (skip)                              |
| `positions`    | array  | 10800  | `f64` (per-vertex xyz, 3600 vertices)            |
| `tex0`         | array  | 7200   | `f64` (uv pairs)                                 |
| `colors`       | array  | 3600   | `f64` (per-vertex colour)                        |
| `influences`   | array  | 3600   | `f64` (per-vertex bone influence)                |
| `normals`      | array  | 10800  | `f64` (per-vertex normal xyz)                    |
| `indices`      | array  | 33408  | `u64` (triangle index list)                      |

Total leaf elements: 80016 numbers + 4 small u32 ranges + 1 small u32
array. C2 row 57 reports the SinkOnly digest currently spends 4.8% in
`materialize_f64`, 2.6% in `materialize_u64`, 92.4% in the structural
walker (`parse_array_element_at_direct` + `parse_object_value_at_direct`).
In the *typed* product plane the walker collapses to the struct's
field-arm; the cost re-projects onto Eisel-Lemire + the per-element push
loop.

## 2. Typed-struct schema (host side)

Add to `skinny/crates/bbnf-bench/src/real_typed_struct.rs` (currently 298
LOC; mirrors the Twitter/UpdateCenter pattern at lines 16-67):

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
    pub colors: Vec<f64>,
    #[serde(default)]
    pub influences: Vec<f64>,
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

No `<'a>` lifetime is required — every leaf is numeric, no borrowed
string. `morphTargets` is ignored (treated as `DirectSkipKind::Object`
via the schema-set ignored-fields mechanism at
`crates/codegen/src/direct_schema.rs:43-58`).

### 2a. Schema-set extension

Add a third root to `xtask/src/real_typed_schema.rs:7-22` (currently 181
LOC; budget allows up to ~240):

```rust
DirectRootSchema {
    function_name: "parse_mesh".to_string(),
    rust_type: "crate::real_typed_struct::Mesh".to_string(),
    type_id: "Mesh".to_string(),
},
```

Plus two type entries in the schema's `types:` vector (lines 23-96):

```rust
struct_ty(
    "Mesh",
    "crate::real_typed_struct::Mesh",
    vec![
        default("batches",    "batches",    vec_with_capacity(ty("MeshBatch"), 1)),
        default("positions",  "positions",  vec_with_capacity(f64_ty(), 10_800)),
        default("tex0",       "tex0",       vec_with_capacity(f64_ty(),  7_200)),
        default("colors",     "colors",     vec_with_capacity(f64_ty(),  3_600)),
        default("influences", "influences", vec_with_capacity(f64_ty(),  3_600)),
        default("normals",    "normals",    vec_with_capacity(f64_ty(), 10_800)),
        default("indices",    "indices",    vec_with_capacity(u64_ty(), 33_408)),
    ],
).with_ignored_fields(vec![
    ignored("morphTargets", DirectSkipKind::Object),
]),
struct_ty(
    "MeshBatch",
    "crate::real_typed_struct::MeshBatch",
    vec![
        default("indexRange",  "index_range",  vec_with_capacity(u64_ty(), 2)),
        default("vertexRange", "vertex_range", vec_with_capacity(u64_ty(), 2)),
        default("usedBones",   "used_bones",   vec_with_capacity(u64_ty(), 4)),
    ],
),
```

Note `f64_ty()`, `u64_ty()`, `vec_with_capacity(inner, hint)` are new
helpers (mirror the existing `u64_ty`, `string`, `vec`, `map_entries`
helpers at lines 147-181). `vec_with_capacity` is the critical extension
— see §3.

## 3. Codegen — capacity-hinted Vec helper

### 3a. The omission

`crates/codegen/src/direct_schema.rs:60-74` currently defines:

```rust
pub enum DirectTypeRef {
    Type(String),
    Scalar(DirectScalar),
    Vec(Box<DirectTypeRef>),                      // no capacity hint
    MapString(Box<DirectTypeRef>),
    MapEntriesVec { entry_rust_type, key_field, value_field,
                    capacity_hint: Option<usize>, value },
    Option(Box<DirectTypeRef>),
}
```

Only `MapEntriesVec` carries a capacity hint. The plain `Vec(inner)` arm
emits `Vec::new()` (renderer `crates/codegen/src/json_typed_direct.rs:313`)
which forces ~17 reallocations for `Vec<f64>` of length 10800 (capacity
doubles 4,8,16,...,16384 — 14 grow events, log₂(10800) ≈ 13.4) and ~16
reallocations for `Vec<u64>` of length 33408. For Eisel-Lemire-dominated
workloads, the reallocation chain plus copy cost is observable (cohort
C2 row 57 is currently 1.10x sonic on the *digest* plane).

### 3b. The minimal change

Extend `DirectTypeRef::Vec` to carry an optional capacity hint:

```rust
// crates/codegen/src/direct_schema.rs:64
Vec { inner: Box<DirectTypeRef>, capacity_hint: Option<usize> },
```

This is the *single* schema change. Renderer side
(`crates/codegen/src/json_typed_direct.rs:306-315`) becomes:

```rust
DirectTypeRef::Vec { inner, capacity_hint } => {
    let inner_ty   = self.rust_type(inner)?;
    let inner_expr = self.parse_expr(inner)?;
    let capacity   = capacity_hint.unwrap_or(0);
    out.push_str(&format!(
        "fn {name}<'i>(parser: &mut DirectParser<'i>) -> \
         Result<{return_ty}, DirectBuildError<'i>> {{\n    \
         let mut out: Vec<{inner_ty}> = Vec::with_capacity({capacity});\n    \
         parser.ws();\n    parser.expect(b'[')?;\n    \
         parser.ws();\n    if parser.take(b']') {{ return Ok(out); }}\n    \
         loop {{\n        out.push({inner_expr}?);\n        \
         parser.ws();\n        if parser.take(b',') {{ parser.ws(); continue; }}\n        \
         parser.expect(b']')?;\n        return Ok(out);\n    }}\n}}\n\n"
    ));
}
```

All other `DirectTypeRef::Vec(...)` constructions throughout the codebase
must thread through; the existing `vec(inner)` helper at
`xtask/src/real_typed_schema.rs:159-161` becomes:

```rust
fn vec(inner: DirectTypeRef) -> DirectTypeRef {
    DirectTypeRef::Vec { inner: Box::new(inner), capacity_hint: None }
}
fn vec_with_capacity(inner: DirectTypeRef, hint: usize) -> DirectTypeRef {
    DirectTypeRef::Vec { inner: Box::new(inner), capacity_hint: Some(hint) }
}
```

`type_key` (json_typed_direct.rs:362), `validate_type_ref` (direct_schema.rs:214),
`collect_helpers` (json_typed_direct.rs:215), and `rust_type`
(json_typed_direct.rs:178) all destructure `Vec(inner)` — each becomes
`Vec { inner, .. }`. Pure rename + destructure, no behavioural change.

### 3c. Why not batch-decode?

A3 esoterica (UDOT lane-vector decode) is *deferred*; the per-element
push-loop is the established baseline. Eisel-Lemire is already SIMD-tight
in `parse-that-regex/src/number.rs`; the bottleneck for `Vec<f64>` is
the *delimiter scan* between consecutive numbers (the comma + whitespace
inside the loop), not the decode itself. Pre-sizing the Vec removes the
allocation chain; the loop body remains scalar-Eisel-Lemire.

### 3d. Generality across numeric-heavy corpora

The capacity-hint extension applies to:

| corpus      | dominant array        | typed shape proposal                       | hint source |
| ----------- | --------------------- | ------------------------------------------ | ----------- |
| mesh        | Vec<f64> (40400 elt)  | `Mesh { positions, normals, ... }`         | manifest    |
| marine_ik   | Vec<f64>              | `MarineIk { ... heavy nested matrices }`   | one-shot probe |
| canada      | Vec<Vec<Vec<f64>>>    | nested coordinate arrays                   | one-shot probe |
| numbers     | Vec<f64>              | `Numbers { values: Vec<f64> }` (single)    | manifest    |

For string-heavy corpora (`twitter`, `gsoc-2018`, `unicode_*`) the
existing typed DirectBuild path already works (twitter at 151.5% sonic
per HANDOFF §2). The capacity-hint extension is *strictly additive* —
unset hint yields `Vec::with_capacity(0)`, identical to today's
`Vec::new()`.

## 4. Bench-harness wiring

### 4a. Fixture enum

`crates/bbnf-bench/src/real_typed_struct.rs:9-13`:

```rust
pub enum RealTypedFixture { Twitter, UpdateCenter, Mesh }
```

`fixture_for_name` (lines 74-80) gains `"mesh" => Some(RealTypedFixture::Mesh)`.

### 4b. Output enum + dispatch

Lines 69-72:

```rust
pub enum RealTypedOutput<'a> {
    Twitter(TwitterSearch<'a>),
    UpdateCenter(UpdateCenter<'a>),
    Mesh(Mesh),                // no lifetime — numeric only
}
```

`track1_typed` / `track2_typed` / `sonic_typed` (lines 111-158) gain a
`Mesh` arm each — three lines apiece, calling
`crate::generated_real_typed::parse_mesh(input)`,
`serde_json::from_slice::<Mesh>(bytes)`,
`sonic_rs::from_slice::<Mesh>(bytes)`.

### 4c. Checksum

Lines 176-225 gain `checksum_mesh` + `checksum_batch`:

```rust
fn checksum_mesh(value: &Mesh) -> u64 {
    let mut h = 0x6d657368;
    h = mix(h, value.batches.len() as u64);
    for b in &value.batches { h = mix(h, checksum_batch(b)); }
    h = fold_f64_vec(h, &value.positions);
    h = fold_f64_vec(h, &value.tex0);
    h = fold_f64_vec(h, &value.colors);
    h = fold_f64_vec(h, &value.influences);
    h = fold_f64_vec(h, &value.normals);
    h = fold_u64_vec(h, &value.indices);
    h
}
fn fold_f64_vec(mut h: u64, v: &[f64]) -> u64 {
    h = mix(h, v.len() as u64);
    for x in v { h = mix(h, x.to_bits()); }
    h
}
fn fold_u64_vec(mut h: u64, v: &[u64]) -> u64 {
    h = mix(h, v.len() as u64);
    for x in v { h = mix(h, *x); }
    h
}
```

`typed_checksum` (lines 176-181) gains a `Mesh` arm.

### 4d. Criterion bench

`crates/bbnf-bench/benches/json_parity.rs:20-26` already dispatches via
`fixture_for_name`; adding `"mesh"` is automatic. The `track1_*`,
`track2_*`, `sonic_rs_*`, `serde_json_real_typed_struct` groups (lines
262-340) all parameterise on `RealTypedFixture` and run unchanged.

The mesh row will appear automatically in the criterion output under
`json/mesh/track1_real_typed_struct`, `json/mesh/track2_real_typed_struct`,
`json/mesh/sonic_rs_real_typed_struct`, `json/mesh/serde_json_real_typed_struct`.

### 4e. Report aggregation

`crates/bbnf-bench/src/report.rs` reads criterion JSON and matches by
group name; the new mesh rows surface in `RESULTS.md` under
`real_typed_struct` workload class automatically (same machinery that
emits the twitter / update_center rows).

## 5. LOC accounting

| layer                                           | LOC   | file                                  |
| ----------------------------------------------- | ----- | ------------------------------------- |
| `Mesh` + `MeshBatch` + ignored marker           | 25    | `real_typed_struct.rs` +21..+45       |
| `RealTypedFixture::Mesh` arm                    |  1    | `real_typed_struct.rs:13`             |
| `fixture_for_name` arm                          |  1    | `real_typed_struct.rs:77`             |
| `RealTypedOutput::Mesh` arm                     |  1    | `real_typed_struct.rs:71`             |
| dispatch (`track1`, `track2`, `sonic`, `serde`) |  8    | `real_typed_struct.rs:115..158`       |
| checksum (`checksum_mesh` + `fold_*_vec`)       | 22    | `real_typed_struct.rs:182..*`         |
| schema entries (`Mesh`, `MeshBatch`, root)      | 30    | `xtask/src/real_typed_schema.rs:7..96`|
| `f64_ty`, `vec_with_capacity` helpers           |  6    | `xtask/src/real_typed_schema.rs:155..*` |
| `DirectTypeRef::Vec` capacity field             |  3    | `crates/codegen/src/direct_schema.rs:64` |
| renderer destructure + capacity emission        | 10    | `crates/codegen/src/json_typed_direct.rs:313, 178, 215, 362, 198` |
| `validate_type_ref` arm                         |  2    | `crates/codegen/src/direct_schema.rs:214` |
| `Cargo.toml` features (none)                    |  0    | —                                     |
| regenerate `generated_real_typed.rs`            | ~60   | regen via `cargo xtask regen-real-typed` |
| **subtotal hand-written**                       | **109** |                                    |
| **subtotal generated**                          | **~60** | `generated_real_typed.rs` grows from 597 to ~660 |
| **total touch**                                 | **169** |                                    |

Within A4's 100-200 LOC band.

## 6. Falsifiability gate

### 6a. Predicted rows

Working from C2's same-plane observation that the digest Track 1 sits at
8139 Mbps vs HandParser 8314 Mbps (97.9% of HandParser) for mesh, and
that the typed product plane collapses the structural walker into the
struct's field arm, the typed Track 1 should rise above the digest
Track 1 (no JsonDigestSink fingerprint hashing per element).

| row                                       | current | predicted Track 1 | gate          |
| ----------------------------------------- | ------- | ----------------- | ------------- |
| mesh direct_to_struct (SinkOnly digest)   | 8818 Mbps Track 1 vs 9606 Mbps sonic = 91.8% | retained (untouched) | floor: 91.8% |
| mesh real_typed_struct (NEW)              | n/a     | ~10500-12000 Mbps | >= 100% of sonic-rs typed |
| mesh sonic_rs typed                       | n/a (probe) | ~9500-10500 Mbps probed | reference |

Rationale for predicted typed Track 1 of 10500-12000 Mbps:

1. `mesh` direct_to_struct currently at 8818 Mbps; the digest sink folds
   every element into a fingerprint hash (`fold_number_f64_scalar` at
   `direct_struct.rs:140-150`). The typed Track 1 omits the digest fold
   and replaces it with `out.push(materialize_f64(...)?)` — strictly less
   work per element.
2. Capacity-hinted Vec eliminates 14 reallocations per `Vec<f64>`
   (positions, normals) and 16 reallocations for `Vec<u64>` (indices).
   For 33408 u64 indices alone, the `Vec::new()` chain grows
   2→4→8→...→32768→65536 with `ptr::copy_nonoverlapping` at every step,
   totalling ~65000 element-copies. Eliminating this saves an estimated
   200-400 µs per parse (mesh parse-time is ~85 µs at 8818 Mbps; the
   capacity win is fractional but additive).
3. Track 2 (independent hand oracle, structurally different) is a
   correctness oracle, not a perf oracle — required to PASS parity, but
   not gated for throughput.
4. The reference: twitter typed Track 1 = 18129 Mbps (151.5% sonic),
   update_center = 12044 Mbps (99.2% sonic). Mesh is numeric, not
   string-heavy → Eisel-Lemire is the limiter, not unescape. Expect
   parity with sonic ±10%.

### 6b. Gate criteria

ADMIT iff all four hold:

1. `mesh real_typed_struct` Track 1 >= 100% of `sonic_rs_real_typed_struct`
   (the predicted-row PASS).
2. `mesh direct_to_struct` Track 1 retained at >= 91.8% of sonic-rs (the
   existing row not regressed by the codegen change — capacity-hint
   should be strictly additive).
3. `twitter real_typed_struct` Track 1 retained at >= 150% of sonic
   (no regression on the V6 admit).
4. `update_center real_typed_struct` Track 1 retained at >= 99% of sonic.

REJECT iff (1) fails. Document measurements in `skinny/REDRESS.md` per
the existing redress-doc cadence.

### 6c. Stretch admit (optional)

If mesh PASSes by >= 110% of sonic, propose adding `marine_ik` and
`numbers` typed rows in B5-followup (single additional schema fixture per
corpus; same codegen path, no further DirectTypeRef changes). Expected
incremental LOC: ~50 per added corpus.

## 7. Implementation steps

1. **Schema extend.** Modify `DirectTypeRef::Vec` →
   `Vec { inner, capacity_hint }`. Add `vec_with_capacity` helper. Sweep
   the 5 destructure sites cited at §5. Compile: `cargo check -p codegen
   --profile ax-iter`.

2. **Host fixture.** Add `Mesh`, `MeshBatch`, `RealTypedFixture::Mesh`,
   `RealTypedOutput::Mesh`, `checksum_mesh`, dispatch arms in
   `real_typed_struct.rs`. Compile: `cargo check -p bbnf-bench
   --profile ax-iter`.

3. **Schema entry.** Add mesh roots + types in
   `xtask/src/real_typed_schema.rs`. Compile xtask.

4. **Regenerate.** `cargo xtask regen-real-typed` rewrites
   `crates/bbnf-bench/src/generated_real_typed.rs`. Inspect diff: expect
   new `parse_mesh`, `parse_type_mesh`, `parse_type_mesh_batch`,
   `parse_vec_with_capacity_10800_scalar_f64`, etc. helpers. Verify
   `Vec::with_capacity(10800)` literally appears.

5. **Parity test.** Run `cargo test -p bbnf-bench --release
   real_typed_struct -- --nocapture` — exercises the parity assertion
   at `benches/json_parity.rs:20-26` plus the `#[cfg(test)]` block
   at `real_typed_struct.rs:281-298`. Add a `generated_mesh_typed_parser_matches_sidecars`
   test paralleling the twitter test (lines 286-290).

6. **Bench.** Single invocation:
   `cargo bench -p bbnf-bench --bench json_parity -- --save-baseline mesh-typed json/mesh`.
   Record Mbps from criterion's per-iteration mean. Update `RESULTS.md`
   with the new row.

7. **Decide.** Apply §6 gate. If PASS: commit (codegen change, host
   fixture, schema entry, regen, RESULTS.md row). If FAIL: REJECT
   commit, write REDRESS entry with measurements and root-cause
   speculation.

## 8. Risks

- **Eisel-Lemire is already saturated.** mesh direct_to_struct already
  beats most parsers; the typed Track 1 win comes from the *omission* of
  the digest fold, not from a faster decoder. If criterion measurement
  noise is wider than the expected gain, the result may sit at 95-105%
  of sonic — passing the gate but tight. Mitigation: A3 UDOT esoterica
  for batch f64 decode (deferred; out of scope for B5).

- **MapEntriesVec capacity-hint type-key collisions.** The `type_key`
  function (json_typed_direct.rs:358-371) currently omits the
  `capacity_hint` from `MapEntriesVec`'s key (line 364-368, only
  `entry_rust_type` + value). Same omission for the new `Vec.capacity_hint`
  is intentional — two Vec<f64> with different hints share a helper
  function and that helper hard-codes one of the hints. *Mitigation:*
  include `capacity_hint` in the type-key. Cost: one helper per distinct
  hint. For mesh's 4 distinct f64 hints (10800, 7200, 3600, 10800 with
  dup, 33408 for u64), this is 4-5 helpers vs 2 today — acceptable.

- **Bench infra LOC budget.** `crates/bbnf-bench` budget at
  `xtask/src/main.rs:164` is 3300 LOC. Current size unknown without
  recount, but the +57 LOC for the host fixture should fit; the lint
  warning fires at 3250. Sanity check `cargo xtask lint-loc` after the
  change.

- **`sonic_rs::from_slice::<Mesh>` may need owned `Mesh`** (no `<'a>`
  parameter). Confirmed by §2: the Mesh struct is numeric-only, no
  borrowed strings, so the standard owning Deserialize path is sufficient
  for both serde and sonic.

## 9. Order vs B1

B1 (per-`\uXXXX` TBL classifier) targets 9 parse-G rows (largest impact:
unicode_escapes, gsoc-2018, twitter parse, citm). B5 targets 1-3 direct
rows (narrower impact, but it lands the typed product plane).

Recommended sequence:

1. **B1 first** — bigger row count, ~6-8 PRs (parse plane).
2. **B5 in parallel or immediately after** — small surface, single PR,
   exercises the existing codegen + schema-set machinery without
   touching parse-that-regex.

B5 is *cheap insurance* against any B1 regression: if B1 introduces a
parse-plane perf cliff, B5 provides an independent typed-output row that
proves the schema-set + codegen path is still healthy on a numeric
corpus.

## 10. Citations

- C2 row 57 (mesh same-plane): `restart/skinny/tranches/sk-v6/research/skv6-C2-direct-profile.md:57`
- mesh direct_to_struct row: `skinny/RESULTS.md:35`
- mesh fixture manifest: `skinny/crates/test-fixtures/corpus/json/manifest.toml:37-41`
- Twitter/UpdateCenter schema (pattern): `skinny/xtask/src/real_typed_schema.rs:7-181`
- DirectTypeRef enum (extension target): `skinny/crates/codegen/src/direct_schema.rs:60-74`
- Vec helper renderer (extension target): `skinny/crates/codegen/src/json_typed_direct.rs:306-315`
- MapEntriesVec capacity pattern (template): `skinny/crates/codegen/src/json_typed_direct.rs:326-341`
- Real-typed test pattern: `skinny/crates/bbnf-bench/src/real_typed_struct.rs:281-298`
- Bench harness dispatch: `skinny/crates/bbnf-bench/benches/json_parity.rs:20-26, 261-340`
- HANDOFF directive: `restart/HANDOFF-SK-V6.md` §2
- A4 ranking: `skv7-A4-parse-that-gaps.md` Top 2

## 11. Summary

Mesh typed-output expansion is **mechanical** — every required machinery
is already in place from the SK-V6 Wave 3 admit. The single non-trivial
codegen change is the `Vec` capacity-hint extension (10 LOC + 5
destructure sweeps). Host fixture, schema entry, bench wiring are all
copy-paste from the twitter/update_center pattern.

**Falsifiability:** mesh real_typed_struct Track 1 >= 100% of sonic-rs
typed. Predicted 10500-12000 Mbps vs ~9500-10500 Mbps sonic →
high-confidence PASS.

**Row impact:** 1 confirmed (mesh), 2-3 stretch (marine_ik, numbers,
canada) after capacity-hint is in place. Total typed product plane goes
from 2 rows → 3-5 rows.
