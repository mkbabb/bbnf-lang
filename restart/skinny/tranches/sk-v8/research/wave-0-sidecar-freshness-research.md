# SK-V8 W0 Sidecar Freshness Research

## Scope

This research covers the W0 sidecar freshness and malformed-manifest gate work
only. It does not propose parser, scanner, SIMD, asm, codegen, RESULTS, REDRESS,
SPEC, or HANDOFF edits.

Controlling constraints:

- `SPEC.md:69-78`: strict admission requires strict-vs-strict, matching output
  plane, same-run native strict or same-run sidecar freshness, and validation in
  the measured row. Sidecar-only, stale, historical, lossy, permissive, or
  plane-mismatched evidence is guard telemetry only.
- `SPEC.md:140-142`: every emitted field must be consumed by `gate-json` in the
  same wave; missing fields, unsupported outcomes, strictness mismatch, stale
  sidecar, and producer-only telemetry reject.
- `SPEC.md:357-370`: W0 must add sidecar freshness validation, malformed
  manifest rejection, explicit `sidecar_freshness=absent:<reason>` for missing
  sidecars, and manifest/freshness coverage for populated sidecar values.
- `p3c-falsifiability-gates.md:49-54`: sidecar cells are a row gate. Populated
  cells require manifest/freshness coverage; missing cells require explicit
  absence reasons; `gate-json` rejects malformed manifests and strict-admission
  rows that fail comparator plane/strictness/freshness/measured-row predicates.
- `p3d-telemetry-schema.md:76-80,108-109,121-128,173`: comparator Mbps fields
  need native or sidecar freshness, sidecar freshness has a closed vocabulary,
  W0 must reject at least one malformed sidecar manifest, and populated C++
  sidecar cells need corpus, binary, hardware, build flags, run date/run id,
  comparator plane, comparator strictness, and freshness.

## Current Model

Current comparator data has two separate provenance classes:

- Same-run Rust comparators are read from Criterion estimates in
  `skinny/crates/bbnf-bench/src/bin/gate.rs:337-349`. These include sonic-rs
  strict/lossy, simd-json borrowed/owned, and serde_json.
- External C++ sidecar comparators are not read from a manifest. They are
  hard-coded in `sidecar_comparators` at
  `skinny/crates/bbnf-bench/src/bin/gate.rs:368-405`, then copied into report
  comparator columns.

The rendered report surface currently has no manifest or freshness carrier for
individual sidecar cells:

- `skinny/crates/bbnf-bench/src/report.rs:7` defines a 26-column schema-v3
  table with comparator Mbps columns but no per-comparator provenance columns.
- `skinny/crates/bbnf-bench/src/report.rs:19-29` stores comparator cells as
  bare `Option<f64>` values.
- `skinny/crates/bbnf-bench/src/report.rs:170-223` validates required row text
  and same-run anchor comparators, but does not validate C++ sidecar manifest
  coverage or absence reasons.

Row metadata has only a generic freshness string:

- `skinny/crates/bbnf-bench/src/metadata.rs:20-65` includes
  `RowMetadata.sidecar_freshness`.
- `skinny/crates/bbnf-bench/src/metadata.rs:355-400` only checks that
  `sidecar_freshness` is non-empty. It does not validate the W0 vocabulary,
  populated-vs-absent consistency, manifest coverage, strictness predicates, or
  stale sidecar strict claims.
- `skinny/crates/bbnf-bench/src/metadata.rs:156,229,276` currently populates
  `"same-run"` broadly for bbnf, competitor, and SIMD rows. That value is not
  sufficient to prove C++ sidecar freshness and must not be allowed to promote a
  static sidecar comparator into strict admission evidence.

`gate-json` schema validation currently cannot catch malformed sidecar state:

- `skinny/crates/bbnf-bench/src/gate.rs:97-116` delegates schema validation to
  `RowMetadata::required_fields_present`.
- `skinny/crates/bbnf-bench/src/gate.rs:118-140` maps failed schema validation
  to `JSchemaFail`, but the schema check has no sidecar manifest parser or
  populated/absent sidecar validation.
- `skinny/crates/bbnf-bench/src/bin/gate.rs:505-538` silently drops malformed
  benchmark metadata with `.ok()`. W0 sidecar manifests must not follow this
  pattern; malformed sidecar metadata must be a hard gate failure.

## Exact Missing Validation

W0 is missing these concrete checks:

1. A gate-consumed sidecar manifest schema for C++ comparators.
2. A one-to-one join between each populated C++ report cell and a manifest
   entry keyed by row/corpus, comparator id, comparator plane, value, and run id.
3. Explicit absence evidence for each missing C++ sidecar cell. `None`, `n/a`,
   blank strings, or prose-only notes are not enough; the gate needs
   `sidecar_freshness=absent:<reason>`.
4. Freshness vocabulary validation:
   `same-run`, `same-run-native`, `sidecar-same-run`, `stale:<reason>`,
   `absent:<reason>`, `historical:<id>`, or `n/a`, with non-empty suffixes where
   required.
5. Populated-vs-absent consistency:
   a populated Mbps cell cannot have `absent:*`; an absent cell cannot have
   `same-run`, `sidecar-same-run`, or `stale:*` without a value.
6. Sidecar manifest required fields for populated C++ cells: corpus identity,
   input hash/bytes, binary identity, comparator version or binary hash,
   hardware/host triple, build flags, feature mask where relevant, run date,
   run id, comparator plane, comparator strictness, freshness, source artifact,
   value unit, and Mbps value.
7. Strict admission refusal for any row whose decisive comparator is stale,
   historical, sidecar-only without same-run manifest coverage, permissive,
   lossy, plane-mismatched, or validated outside the measured row.
8. Parse-error rejection for sidecar manifests. Invalid TOML/JSON, unknown
   comparator ids, duplicate entries, missing required fields, and unsupported
   freshness tokens must produce `JSchemaFail` or equivalent invalid exit, not a
   dropped sidecar.

## Implementation Points

Recommended narrow implementation shape:

1. Add sidecar evidence types in `skinny/crates/bbnf-bench/src/metadata.rs` or a
   small sibling module re-exported from there:

   ```rust
   pub struct SidecarManifest {
       pub schema_version: u32,
       pub run_id: String,
       pub row_id: String,
       pub corpus: String,
       pub input_sha256: String,
       pub input_bytes: u64,
       pub host_triple: String,
       pub cpu_model: String,
       pub build_flags: String,
       pub entries: Vec<SidecarEntry>,
   }

   pub struct SidecarEntry {
       pub comparator_id: String,
       pub comparator_plane: String,
       pub comparator_strictness: String,
       pub sidecar_freshness: String,
       pub mbps: Option<f64>,
       pub absence_reason: Option<String>,
       pub comparator_version: Option<String>,
       pub binary_sha256: Option<String>,
       pub run_date_utc: Option<String>,
       pub source_artifact: Option<String>,
   }
   ```

   The struct names are illustrative; the important boundary is that sidecar
   provenance is structured data, not a note string.

2. Add a validator returning diagnostics rather than `bool`, for example:

   ```rust
   pub fn validate_sidecar_manifest(
       manifest: &SidecarManifest,
       expected: &SidecarExpectedContext,
       cxx_cells: &CxxSidecarCells,
   ) -> Result<(), SchemaError>
   ```

   It should:

   - require one evidence entry for every C++ sidecar slot:
     `simdjson_dom`, `simdjson_ondemand`, `yyjson_default`, `asmjson_swar`,
     `asmjson_avx512`, and `rapidjson_default`;
   - reject duplicate comparator entries for the same row;
   - reject unsupported comparator ids, planes, strictness tokens, or freshness
     tokens;
   - require `sidecar-same-run` or `same-run` plus complete manifest identity
     for populated same-run sidecar cells;
   - allow `stale:<reason>` or `historical:<id>` only as non-admission planning
     evidence, and reject if that evidence is used for strict admission;
   - require `absent:<reason>` with a non-empty reason for absent sidecar cells;
   - reject value/freshness contradictions such as `Some(mbps)` plus `absent:*`,
     or `None` plus `sidecar-same-run`.

3. Stop treating `sidecar_comparators` as sidecar proof. In
   `skinny/crates/bbnf-bench/src/bin/gate.rs:337-405`, make the C++ comparator
   population path consume a sidecar manifest or produce explicit absence
   evidence. If no manifest is supplied, the correct W0 state is absent cells
   with reasons such as `absent:manifest-not-provided`, not hard-coded populated
   Mbps values.

4. Do not parse sidecar manifests with the `.ok()` pattern used by
   `read_metadata_rows` and `read_simd_metadata_row` at
   `skinny/crates/bbnf-bench/src/bin/gate.rs:505-538`. Sidecar parsing should be
   a `Result`; parse failure should become `JSchemaFail` and an invalid process
   exit.

5. Extend `skinny/crates/bbnf-bench/src/report.rs` so `ComparatorSet` has either
   structured sidecar evidence or a parallel `SidecarEvidenceSet`. The report
   validator at `report.rs:170-223` must consume that evidence for each sidecar
   comparator cell. The rendered table can keep the existing Mbps columns, but
   the gate must have structured evidence for the table values it emits.

6. Extend `skinny/crates/bbnf-bench/src/gate.rs:97-116` so schema validation
   consumes row metadata, report rows, estimates, and sidecar evidence together.
   A bare `RowMetadata::required_fields_present` pass must not be enough to
   classify a row as schema-valid.

7. Keep strict-vs-strict predicates separate from sidecar freshness validation.
   Manifest-valid C++ sidecar data is still planning evidence unless the row
   also satisfies output-plane equality, `Strictness=strict`,
   `comparator_strictness=strict`, allowed same-run freshness, and measured-row
   validation. `parse_only` rows with `parse_utf8=view-boundary` remain
   non-admission guard rows.

## Focused Tests

Add a focused W0 test around the sidecar validator before broad report plumbing:

### `malformed_populated_sidecar_metadata_rejects`

Location: `skinny/crates/bbnf-bench/src/gate.rs` or the module that owns
`validate_sidecar_manifest`.

Setup:

- Build expected context for one existing W0 row, for example `twitter` with
  known input hash/bytes and row id.
- Set the sidecar cells to include one populated C++ value, for example
  `simdjson_dom = Some(24522.0)`.
- Provide a manifest entry for `simdjson_dom` that is intentionally malformed:
  missing `run_id`, missing `binary_sha256`, corpus mismatch, unsupported
  comparator plane, or `sidecar_freshness=absent:not-collected` while a value is
  populated.

Expected result:

- `validate_sidecar_manifest(...)` returns `Err`.
- The error identifies the malformed sidecar manifest and the failing
  comparator id.
- The gate path maps the error to `JSchemaFail` or an invalid schema exit.

### `absent_sidecars_require_explicit_absence_reasons`

Location: same module.

Setup:

- Build a sidecar evidence set with all C++ comparator cells absent.
- For every absent C++ comparator, provide `sidecar_freshness=absent:<reason>`
  with a non-empty reason, such as `absent:not-collected-on-this-host`.

Expected result:

- The validator accepts the absent sidecars as explicit non-evidence.
- Changing any absence token to `n/a`, an empty string, `absent:`, or omitting
  the entry makes the validator return `Err`.

### `stale_sidecar_cannot_feed_strict_admission`

Location: `skinny/crates/bbnf-bench/src/gate.rs` once strict-admission checks are
centralized.

Setup:

- Create a row that otherwise claims strict admission.
- Give it a C++ sidecar comparator with `comparator_strictness=strict` but
  `sidecar_freshness=stale:old-profile` or `historical:sk-v6`.

Expected result:

- Sidecar manifest structure may be valid as planning telemetry, but admission
  rejects before `A`, `G`, or `GO`.

### `malformed_sidecar_parse_is_not_silent`

Location: `skinny/crates/bbnf-bench/src/bin/gate.rs` after extracting sidecar
manifest loading into a testable function.

Setup:

- Pass malformed TOML/JSON with a populated sidecar value into the sidecar
  manifest loader.

Expected result:

- The loader returns `Err`; no `.ok()` path drops the manifest.
- The binary/gate integration reports schema failure instead of silently
  treating the sidecar as absent or falling back to static numbers.

## Regression And Strictness Risks

- The highest-risk regression is accidentally promoting static C++ sidecar
  numbers into strict admission evidence. The fix is to keep manifest validity
  and admission eligibility as separate checks, and to require strict-vs-strict
  same-plane measured-row validation before admission.
- The existing `sidecar_freshness="same-run"` on generic row metadata can be
  misread as C++ sidecar freshness. W0 should either scope that field to the
  row/comparator it describes or replace it with comparator-keyed evidence.
- If hard-coded sidecar values remain in `sidecar_comparators`, W0 can emit
  populated C++ cells without manifest proof. That fails the W0 row gate.
- If missing sidecars render only as `n/a`, downstream readers can confuse
  absence with unknown provenance. Every missing C++ sidecar needs a gate-read
  `absent:<reason>` token.
- Broadly changing `read_metadata_rows` to reject malformed historical Criterion
  metadata could make old local benchmark directories fail unexpectedly. The
  required W0 hard failure is for sidecar manifests; broader metadata strictness
  should be done only if the wave explicitly owns that compatibility break.
- Report-only notes are insufficient. The current note in
  `bin/gate.rs:217-219` correctly says C++ sidecars are not same-run strict
  anchors, but W0 requires machine-consumed evidence, not prose.

## Research Disposition

W0 sidecar freshness is not currently enforceable. The code can emit or report
populated C++ sidecar comparator cells without manifest coverage, and missing
sidecars have no explicit absence reasons. A narrow W0 implementation should add
structured sidecar evidence, make malformed sidecar parsing fatal, require
`absent:<reason>` for missing sidecars, and keep all sidecar evidence out of
strict admission unless the strict-vs-strict same-plane measured-row predicates
also pass.
