# SK-V7 W3 R5 Prior Routes And Fixture Shape

Date: 2026-05-16.

## Scope

This note covers the prior rejected DirectBuild/materialization routes and the fixture-shape constraints for SK-V7 Wave 3. It is intentionally a preflight warning document for the W3 implementer, not a new implementation plan.

## Pre-Blocked Routes

Do not reopen the SK-V6 Wave 3 direct string/materializer family under new names:

- Direct source-hook field-layout materializer is rejected. REDRESS item 66 says the route added direct-only source hooks and digest folding, stayed within the direct generated runtime surface, passed tests, but failed all required lift thresholds; the finding was that receiver/closure removal was too small and escaped-string decode/materialization plus parser control remained dominant (`skinny/REDRESS.md:1688`, `skinny/REDRESS.md:1698`, `skinny/REDRESS.md:1718`, `skinny/REDRESS.md:1723`, `skinny/REDRESS.md:1727`).
- Parser-owned decoded scratch is rejected. REDRESS item 67 records a reusable `String` scratch threaded through generated `parse_direct`, green correctness, then a decisive `unicode_escapes` regression; it explicitly says not to reopen parser-owned scratch and requires a new fact beyond allocation reuse/parser-owned decode (`skinny/REDRESS.md:1736`, `skinny/REDRESS.md:1746`, `skinny/REDRESS.md:1762`, `skinny/REDRESS.md:1768`, `skinny/REDRESS.md:1777`).
- Byte-output `unescape_json_string` inside the current `Cow<str>` API is rejected. REDRESS item 68 kept the public API and generated consumer, switched escaped-string writes to `Vec<u8>`, passed correctness, then regressed the primary escaped row; it states that the direct-string allocation/receiver/byte-writing family is exhausted under the current direct digest workload (`skinny/REDRESS.md:1789`, `skinny/REDRESS.md:1797`, `skinny/REDRESS.md:1814`, `skinny/REDRESS.md:1819`, `skinny/REDRESS.md:1828`).
- DirectBuild semantic string fact hashing for the current digest workload is rejected. REDRESS item 69 carried semantic string facts through `DirectBuild`, but the primary row regressed by about 15%; the ledger permits DirectBuild field facts as an architecture, but blocks one-pass semantic fact hashing when the consumer is decoded byte length plus fingerprint (`skinny/REDRESS.md:1839`, `skinny/REDRESS.md:1847`, `skinny/REDRESS.md:1863`, `skinny/REDRESS.md:1867`, `skinny/REDRESS.md:1877`).

Do not use a benchmark-private hand typed sink as proof of DirectBuild:

- Candidate 11 is rejected as a SOTA close. It added hand typed output for `twitter` and `update_center`, then found the important architectural requirement: JSON output schema is not in the JSON grammar, so a conforming receiver must name a host/API type contract consumed by `DirectBuild` field facts, not a hidden BBNF directive or benchmark-private parser (`skinny/REDRESS.md:1890`, `skinny/REDRESS.md:1897`, `skinny/REDRESS.md:1908`, `skinny/REDRESS.md:1931`, `skinny/REDRESS.md:1934`).

The only admitted typed-output route to build on is the Candidate 12 shape:

- Candidate 12 accepts generated typed `DirectBuild` from a host/API output schema, through `codegen::direct_schema`, a `SinkOnlyProgram + DirectSchemaSet` lowerer, and generated JSON typed parser rendering. The schema enters from xtask/host code, not BBNF, with no new BIR variant, retained side table, or benchmark-private Track 1 parser (`skinny/REDRESS.md:1944`, `skinny/REDRESS.md:1946`, `skinny/REDRESS.md:1950`).
- Its successful payload facts were `MapEntriesVec { capacity_hint }`, generated skip-only plain-string scanning for ignored fields, and explicit ignored-fields skip kinds. Candidate sub-routes also reject full-ish schemas without capacity hints, `MapEntriesVec` alone, raw key byte dispatch, narrow selected-output `Plugin` plane, and global/tiny skip caps (`skinny/REDRESS.md:1959`, `skinny/REDRESS.md:1966`, `skinny/REDRESS.md:1977`).
- Gate accounting now splits representative typed rows from the old maximal digest stressor. W3 must not relabel an old `direct_to_struct` miss as typed success; it must wire `real_typed_struct` sidecars and parity like Candidate 12 did (`skinny/REDRESS.md:1985`).

The SK-V7 handoff repeats these as hard stops: REDRESS 60-72 include retained/direct materialization blocks, direct source-hook field-layout materializer, parser-owned decoded scratch, byte-output unescape, and DirectBuild semantic string facts (`restart/skinny/tranches/sk-v7/HANDOFF.md:66`, `restart/skinny/tranches/sk-v7/HANDOFF.md:75`, `restart/skinny/tranches/sk-v7/HANDOFF.md:78`).

## Current W3 Contract

SK-V7 W3 is not "try DirectBuild again." It is specifically "B5 mesh DirectBuild + DirectTypeRef::Vec specialisation":

- Handoff says mesh DirectBuild is blocked by codegen Vec helper shape-blindness and needs `DirectTypeRef::Vec` specialisation first (`restart/skinny/tranches/sk-v7/HANDOFF.md:35`, `restart/skinny/tranches/sk-v7/HANDOFF.md:52`).
- SPEC owner paths name the shape-blind helper, `DirectTypeRef::Vec`, `xtask/src/real_typed_schema.rs`, and generated/bench wiring. Tasks require adding `capacity_hint` to `DirectTypeRef::Vec`, including it in `type_key()`, specialising Vec helpers for numeric arrays, adding a mesh schema fixture, and benching mesh plus marine_ik (`restart/skinny/tranches/sk-v7/SPEC.md:179`, `restart/skinny/tranches/sk-v7/SPEC.md:181`, `restart/skinny/tranches/sk-v7/SPEC.md:188`).
- SPEC gates require both mesh and marine_ik `real_typed_struct` at or above 100% sonic-strict, no existing mesh direct_to_struct regression below 91.8%, and no twitter real_typed_struct regression (`restart/skinny/tranches/sk-v7/SPEC.md:196`, `restart/skinny/tranches/sk-v7/SPEC.md:202`, `restart/skinny/tranches/sk-v7/SPEC.md:206`).
- SYNTHESIS explains why sequencing matters: current Vec helper has no `Vec::with_capacity`, no SWAR, and no bulk delimiter scan; mesh would land around 91.8% sonic under current codegen, below PASS (`restart/skinny/tranches/sk-v7/SYNTHESIS.md:109`, `restart/skinny/tranches/sk-v7/SYNTHESIS.md:111`, `restart/skinny/tranches/sk-v7/SYNTHESIS.md:114`).

Current code matches that warning:

- `DirectTypeRef::Vec` is currently just `Vec(Box<DirectTypeRef>)`, while `MapEntriesVec` already carries `capacity_hint: Option<usize>` (`skinny/crates/codegen/src/direct_schema.rs:60`, `skinny/crates/codegen/src/direct_schema.rs:64`, `skinny/crates/codegen/src/direct_schema.rs:66`, `skinny/crates/codegen/src/direct_schema.rs:70`).
- The Vec helper currently emits `let mut out: Vec<T> = Vec::new()` plus scalar loop, whitespace, comma, and closing-bracket checks; there is no capacity hint or numeric-array specialization (`skinny/crates/codegen/src/json_typed_direct.rs:306`, `skinny/crates/codegen/src/json_typed_direct.rs:313`).
- By contrast, `MapEntriesVec` already emits `Vec::with_capacity(capacity)` from `capacity_hint`, which is the established pattern to mirror for plain Vec (`skinny/crates/codegen/src/json_typed_direct.rs:326`, `skinny/crates/codegen/src/json_typed_direct.rs:335`, `skinny/crates/codegen/src/json_typed_direct.rs:340`).
- Current real typed schemas only include `Twitter` and `UpdateCenter`; fixtures and generated routes for mesh/marine_ik do not exist yet (`skinny/xtask/src/real_typed_schema.rs:11`, `skinny/xtask/src/real_typed_schema.rs:23`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:10`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:74`).

## Fixture Shape Warnings

`mesh.json` is a minified one-line file, so line evidence is necessarily line 1. Its top-level shape is not the Three.js-style `geometries[0].data` shape used by `marine_ik`; it is a flat mesh object:

- Top-level keys are `batches`, `colors`, `indices`, `influences`, `morphTargets`, `normals`, `positions`, and `tex0` (`skinny/test_data/mesh.json:1`).
- `batches` is an array with one object carrying `indexRange`, `vertexRange`, and `usedBones`; the first batch has index range `[0,33408]`, vertex range `[0,3600]`, and used bone `[22]` (`skinny/test_data/mesh.json:1`).
- Numeric vector lengths from structured inspection: `positions` 10800, `normals` 10800, `colors` 3600, `indices` 33408, `tex0` present instead of `uvs`, and `morphTargets` is an empty object. There is no top-level `vertices`, `geometries`, `skinIndices`, or `skinWeights` field (`skinny/test_data/mesh.json:1`).

Schema correctness warning for mesh: SPEC says `Mesh { vertices: Vec<f32>, normals: Vec<f32>, indices: Vec<u32>, ... }` (`restart/skinny/tranches/sk-v7/SPEC.md:192`), but the actual fixture uses `positions`, not `vertices`, and `tex0`, not `uvs` (`skinny/test_data/mesh.json:1`). A schema that names `vertices` will silently default/skip if fields are optional/defaulted, producing a fast but semantically wrong typed workload. For a representative mesh typed row, fields should map to actual JSON keys and checksums should include at least positions/normals/indices and one of colors/tex0/batches so the parser cannot "win" by skipping the fixture.

`marine_ik.json` is a nested Three.js-style scene/export, not the flat mesh shape:

- It has top-level `images`, `geometries`, `materials`, `metadata`, `object`, `textures`, and top-level `animations` (`skinny/test_data/marine_ik.json:2`, `skinny/test_data/marine_ik.json:7`, `skinny/test_data/marine_ik.json:47812`, `skinny/test_data/marine_ik.json:47818`, `skinny/test_data/marine_ik.json:47833`, `skinny/test_data/marine_ik.json:47849`).
- The first geometry has `type`, `uuid`, and nested `data`; `data.uvs` begins early, `data.animations` begins at line 12, `data.vertices` begins at line 47398, and the file later contains material/object/top-level animation sections (`skinny/test_data/marine_ik.json:7`, `skinny/test_data/marine_ik.json:10`, `skinny/test_data/marine_ik.json:11`, `skinny/test_data/marine_ik.json:12`, `skinny/test_data/marine_ik.json:47398`).
- Structured inspection shows `geometries[0].data` has keys `animations`, `bones`, `faces`, `influencesPerVertex`, `metadata`, `name`, `normals`, `skinIndices`, `skinWeights`, `uvs`, and `vertices`; `uvs` is an outer array of length 1 with 10532 scalars, animations length is 5, the first animation hierarchy length is 64, first key shape is `{ pos, time, scl, rot }`, `vertices` length is 17220, `faces` length is 74087, `bones` length is 64, and `skinIndices`/`skinWeights` are each 11480 (`skinny/test_data/marine_ik.json:11`, `skinny/test_data/marine_ik.json:12`, `skinny/test_data/marine_ik.json:121`, `skinny/test_data/marine_ik.json:47398`).

Schema correctness warning for marine_ik: a flat `Mesh` schema cannot cover this fixture. If W3 wants marine_ik PASS, it needs either a separate nested schema rooted at `{ geometries: Vec<Geometry>, ... }` with a selected but representative `GeometryData` payload, or it must explicitly defer marine_ik. Mapping marine_ik to the flat mesh schema would mostly exercise unknown-field skipping and would repeat the Candidate 12 "selected-output" risk without the documented representative premise.

## Bench And Parity Wiring Risks

- Real typed benchmarks only run when `fixture_for_name()` returns a typed fixture. Today that covers only `twitter` and `update_center`/`update-center`, so adding schema alone is insufficient; mesh and marine_ik must be added to `RealTypedFixture`, track1, track2/serde, sonic, checksum, and fixture lookup (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:74`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:111`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:125`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:132`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:146`).
- Parity and benchmark rows are gated by `fixture_for_name()`, then generate Track 1, Track 2, sonic-rs, and serde_json `real_typed_struct` rows (`skinny/crates/bbnf-bench/benches/json_parity.rs:261`, `skinny/crates/bbnf-bench/benches/json_parity.rs:286`, `skinny/crates/bbnf-bench/benches/json_parity.rs:310`, `skinny/crates/bbnf-bench/benches/json_parity.rs:331`).
- Gate classification also checks real typed parity only through `fixture_for_name()`, so a missing mesh/marine_ik enum arm will suppress the intended W3 outcome rather than fail loudly (`skinny/crates/bbnf-bench/src/bin/gate.rs:127`, `skinny/crates/bbnf-bench/src/bin/gate.rs:137`).
- Existing sidecar constants include a simdjson comparator for mesh, but that is separate from sonic-rs real typed rows; W3 must populate real_typed_struct sidecars, not infer typed PASS from existing direct or simdjson data (`skinny/crates/bbnf-bench/src/bin/gate.rs:370`, `skinny/crates/bbnf-bench/src/bin/gate.rs:377`).

## Recommendations

1. Implement `DirectTypeRef::Vec { inner, capacity_hint }` before adding mesh/marine schemas. Update validation, helper naming/type keys, schema helpers, and generated code so two Vec helpers with different hints or scalar specialisations cannot collide.
2. For mesh, name actual fixture keys: `positions`, `normals`, `indices`, `colors`, `tex0`, `batches`, `influences`, `morphTargets`. Do not use `vertices` unless the schema field has `json_key: "positions"` and the Rust field is intentionally renamed.
3. For marine_ik, use a separate nested schema. A minimal representative schema should include `geometries: Vec<Geometry>`, `Geometry { data }`, and `GeometryData` fields for at least `vertices`, `faces`, `normals`, `uvs`, `skinIndices`, `skinWeights`, `bones`, and enough animation hierarchy to prevent skip-only success.
4. Make checksums prove work. Include vector lengths and sampled numeric content for all required arrays, plus batch/range or animation/bone counts. Avoid optional/default-only schemas that let missing keys pass.
5. Keep rejected SK-V6 routes closed. W3 should not touch source-hook direct folding, parser-owned string scratch, byte-output unescape under `Cow<str>`, semantic string fact hashing for the digest workload, or benchmark-private Track 1 typed sinks.
6. Treat `mesh` and `marine_ik` as two schemas, not one. The fixture shapes differ enough that sharing only a flat `Mesh` struct is a schema correctness bug.
7. Verification should include `check-real-typed`, real typed parity tests, a gate run proving mesh and marine_ik `real_typed_struct` rows exist, and a guard that existing mesh `direct_to_struct` remains above the SPEC floor.
