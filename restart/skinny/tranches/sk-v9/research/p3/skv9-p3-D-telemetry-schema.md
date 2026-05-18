# SK-V9 P3-D: Telemetry-Schema Binding

Pass: S-P3 Synthesis-Plan. Cycle: V1.
Date: 2026-05-18.
Scope: Bind every SK-V9 wave's measurement to a `gate-json`-consumed schema —
field table, per-wave population, outcome enum, measured-row admission fields,
PMU manifest disposition, schema-version tag, same-wave consumption rule.
Output: this file.
Pass Alpha goalset: SPEC §0 close-condition — W0 produces/consumes a coherent
`SK-V9-open` telemetry manifest; no producer-only telemetry; every emitted
field consumed by `gate-json` in the same wave.
Candidate pool: research/p2/ post-CHALLENGE survivors.

## §1 — Method

The SK-V9 telemetry schema is already two-layered in code and must stay so.
SK-V9 does not invent a fresh schema; it **confirms and extends** what W0 froze.

The two layers, as they exist in `skinny/crates/bbnf-bench/`:

1. **Schema-v3 row table** — the rendered SOTA table in `skinny/RESULTS.md`,
   26 columns, frozen as the `SCHEMA_V3_HEADER` / `SCHEMA_V3_ALIGN` constant
   pair in `report.rs:8-9`, validated row-by-row by
   `RowMetadata::validate_schema_v3` (`report.rs:220-274`).

2. **SK-V9 W0 telemetry manifest** — the 22-column `## SK-V9 W0 Telemetry
   Manifest` block in `skinny/RESULTS.md:44+`, backed by the `SkV8Telemetry`
   struct (`report.rs:44-67`, 21 fields) plus the `SkV8ComparatorEvidence`
   struct (`report.rs:33-40`, 6 fields), validated by
   `RowMetadata::validate_sk_v8_w0` (`report.rs:276-388`).

The SPEC §0.4 "Required Telemetry" list of 31 field names is the union of
these two layers' gate-consumed identifiers. The list is already complete:
W0 closed against it (`G-W0-TELEMETRY-LOCK` PASS,
`sk-v9-open:criterion-fnv64-cd1673844eeea12f`). P3-D's job is therefore not
addition but **binding** — stating which wave populates which field, and
codifying the same-wave-consumption rule so no behaviour wave emits a
producer-only column.

Source grounding: the 31-name list is verified against `report.rs` field
names one-to-one; the W0-accepted outcome set is read from
`validate_w0_outcome` (`report.rs:977-988`); the run-id grammar from
`SK_V9_OPEN_RUN_ID_PREFIX` / `is_skv9_open_run_id` (`report.rs:685-695`);
the measured-row admission fields from P2-C §2.4; the per-row Mbps
projection methodology from P2-E §6.

## §2 — The SK-V9 RESULTS schema

### §2.1 — Confirmation: no new fields

The W0-frozen schema is **carried forward unchanged** for SK-V9 W1–W5. No
behaviour wave adds a column. The S-P1 V3–V6 convergence (real PMU data at
`/tmp/skv9-xctrace-v3/pmu_rows.tsv`, per-symbol Time Profiler exports,
deep hot-leaf attribution) produced *diagnostic* evidence; per the SPEC §1
non-negotiable, "no PMU or cycles-per-byte … used as a producer", that
evidence does not earn a gate column (see §5). The schema therefore stays
at its W0 cardinality: 26 schema-v3 columns + 22 manifest columns, 31
distinct gate-consumed required-field identifiers.

### §2.2 — Field table

The 31 required identifiers, their layer, their producer, and the wave
that first populates a non-placeholder value.

| # | Field | Layer | Type / domain | First non-placeholder wave |
|--:|---|---|---|---|
| 1 | `row_id` | manifest | `json/<corpus>/<workload>/main` | W0 |
| 2 | `grammar_id` | manifest | `json` (W0 rejects non-`json`) | W0 |
| 3 | `domain` | manifest | `json_bench` (W0 rejects others) | W0 |
| 4 | `corpus` | schema-v3 | fixture name | W0 |
| 5 | `workload` | schema-v3 | `parse_only` / `direct_to_struct` / `real_typed_struct` | W0 |
| 6 | `outcome_id` | schema-v3 | outcome enum §3 | W0 |
| 7 | `verdict` | schema-v3 | `GO` / `GO-WITH-FOCUS` / `NO-GO` / `INVALID` | W0 |
| 8 | `strictness` | schema-v3 | `strict` / `permissive` / `deferred` | W0 |
| 9 | `output_plane` | schema-v3 | `borrowed view…` / `digest` / `typed direct` | W0 |
| 10 | `track1_mbps` | schema-v3 | f64 Mbps, generated parser | W0 |
| 11 | `track2_mbps` | schema-v3 | f64 Mbps, hand-coded oracle | W0 |
| 12 | `comparator_id` | manifest (`SkV8ComparatorEvidence`) | e.g. `sonic_rs_strict` | W0 |
| 13 | `comparator_plane` | manifest | `DOM` / `typed direct` / … | W0 |
| 14 | `comparator_strictness` | manifest | `strict` / `permissive` | W0 |
| 15 | `comparator_freshness` | manifest | `same-run-native` / `historical:…` / `absent:…` | W0 |
| 16 | `measured_validation_path` | manifest | e.g. `view-boundary` | W0 |
| 17 | `profile_artifact` | manifest | `criterion-slope-profile:<path>` | W0 |
| 18 | `sample_cost` | manifest | `ns_per_byte=…;track1_ns=…;bytes=…` | W0 |
| 19 | `sample_count` | manifest | u64 > 0 | W0 |
| 20 | `build_flags` | manifest | `profile=bench;rustflags=…;target_cpu=…` | W0 |
| 21 | `host_triple` | manifest | `aarch64-apple-darwin;arch=…;cpu=…` | W0 |
| 22 | `feature_mask` | manifest | `arch=…;os=…;simd=…;target_cpu=…` | W0 |
| 23 | `costfacts_rule_id` | manifest | `none:pre-W1` pre-behaviour; rule id at W2+ | W2 (was `none` W0–W1) |
| 24 | `costfacts_chosen_shape` | manifest | shape token; `none:pre-W1` until a wave produces CostFacts | W2 |
| 25 | `costfacts_rejected_alternative_ids` | manifest | id list; `none:pre-W1` until a wave produces CostFacts | W2 |
| 26 | `redress_entry` | manifest | REDRESS anchor or `none` | W0 (`none`); behaviour waves bind a real entry |
| 27 | `wave_id` | manifest | `SK-V9-open` (W0); per-wave id thereafter | W0 |
| 28 | `run_id` | manifest | `sk-v9-open:criterion-fnv64-<16 hex>` | W0 |
| 29 | `sidecar_freshness` | manifest (`SkV8ComparatorEvidence`) | `n/a` / `historical:…` | W0 |
| 30 | `sk_v9_open_delta` | manifest | `baseline` (W0); signed Δ thereafter | W0 (`baseline`) |
| 31 | `substrate_surface` | manifest | `borrowed_view_over_offset_tape` / … | W0 |
| 32 | `structural_projection_status` | manifest | `discarded_after_capacity` / … | W0 |
| 33 | `substrate_cardinality` | manifest | `one` (substrate-union invariant) | W0 |
| 34 | `same_wave_consumer_class` | manifest | `gate_only` (W0); `<kernel>→<consumer>` at behaviour waves | W0 |
| 35 | `track2_independence_status` | manifest | `independent_verified` | W0 |
| 36 | `diagnostic_nonproducer_status` | manifest | fixed `structural_scan+masking_probes+pmu+cycles:nonproducer` | W0 |

The SPEC §0.4 list names 31 of these (it elides `corpus`/`workload`/
`verdict`/`strictness`/`output_plane` as "implicit schema-v3 head" and the
three `costfacts_*` as one `CostFacts ids` token); the canonical
gate-consumed identifier set is the 36-row table above, which is the exact
union of `RowMetadata` schema-v3 fields + `SkV8Telemetry` fields +
`SkV8ComparatorEvidence` fields. SK-V9 P3-D pins this 36-identifier set as
the SK-V9 schema. No SK-V9 wave adds a 37th.

### §2.3 — Per-wave population obligation

Each wave must populate the schema as follows. A wave that emits a value
for a field below without it being gate-consumed in that same wave fails
its exit gate (§6 rule).

| Wave | Schema obligation |
|---|---|
| **W0** (closed) | Populates all 36 fields for the 38-row baseline. Behaviour-class fields take their pre-behaviour constants: `costfacts_* = none:pre-W1`, `redress_entry = none` (except already-redressed rows), `wave_id = SK-V9-open`, `sk_v9_open_delta = baseline`, `same_wave_consumer_class = gate_only`. `gate-json` consumes every field via `validate_schema_v3` + `validate_sk_v8_w0`. |
| **Interlock (S-P1 rerun)** | Produces NO RESULTS rows. The PMU table (`pmu_rows.tsv`) and Time Profiler exports are diagnostic artefacts (§5); they populate no schema field. The rerun's only schema touch is confirming the W0 manifest still validates. |
| **W1** (revised S-P2/S-P3 release) | Planning gate only. No row emission, no field population. It authorises which behaviour fields W2–W4 may move. |
| **W2** (Apache/CITM typed admission) | Populates the full 36-field set for two new `real_typed_struct` rows (Apache, CITM) and refreshes `run_id` across the file (P2-C §2.2–§2.4). First wave to emit non-`none` `costfacts_*`? — **No**: P2-C §2.4 binds `costfacts_* = none:pre-W1` for the row-table wave (Lock 14: a row-table admission wave produces no CostFacts). `wave_id` becomes the per-wave id (P2-C §2.3 names `sk-v9-real-typed-w{n}`); `sk_v9_open_delta` becomes a signed Δ against `SK-V9-open`; `redress_entry` binds the new REDRESS anchor; `same_wave_consumer_class` = `gate_only` (the row IS the artefact). |
| **W3** (tape + structural projection) | If released, populates `substrate_surface` / `structural_projection_status` / `substrate_cardinality` with the retained-substrate identity. `substrate_cardinality` MUST stay `one`. `costfacts_*` carries the chosen retained-shape rule id if the wave's intervention is cost-model-driven; otherwise `none:pre-W1`. `same_wave_consumer_class` names the retained-tape kernel → its production consumer. |
| **W4** (direct contract) | Populates `output_plane` / `measured_validation_path` for the direct-contract rows; `same_wave_consumer_class` names the codec/scanner kernel → `unescape_string` (the P2-E production consumer, `parse-that-regex/src/lib.rs:718`). `costfacts_*` carries the chosen-shape rule id if a CostFacts decision drives the kernel selection. |
| **W5** (close) | No new field. Reconciles RESULTS / REDRESS / SPEC / DISPATCH-PROMPT / HANDOFF; verifies the 36-field schema renders identically across all admitted/rejected rows. |

The `costfacts_*` triad is the only field group whose *value class* changes
across waves: `none:pre-W1` through W0/W1, then a real rule id only in a
wave whose intervention is selected by a CostFacts decision. P2-C §2.4 is
explicit that the typed row-table wave is NOT such a wave. The triad
therefore stays `none:pre-W1` unless a behaviour wave's plan demonstrates
a CostFacts-driven shape choice; that demonstration is the wave's burden,
gated in the same wave.

## §3 — Outcome enum

### §3.1 — The enum in code vs the SPEC

`gate::Outcome` (`gate.rs:4-66`) defines 15 variants:
`A B C D E F-positive F-noise G I J K L M N-direct S`.

`validate_w0_outcome` (`report.rs:977-988`) restricts the **W0-admissible**
set to 10: `A C G I J K L M N-direct S`.

SPEC §0.3 names the SK-V9 outcome enum as `A C G K L N-direct S` — 7
identifiers. This is a strict subset of the W0-admissible 10; it omits
`I` (parity-oracle fail), `J` (schema fail), and `M` (memory-residency
fail).

### §3.2 — Verdict: no new outcome; reconcile the SPEC list

SK-V9 needs **no new outcome variant**. The SPEC §1 non-negotiables forbid
"no new BIR variant / no new BackendShape variant"; an outcome variant is
not in that list, but the §0 close-condition's spirit — "no behaviour
moved" — argues against minting a code-level enum change in a
telemetry-recovery bracket. Every behaviour the SK-V9 waves can produce is
already classifiable:

- **A** — beat-and-parity (W2 typed GO target; current twitter typed row).
- **C** — substrate-parity-codegen-acceptable (the GO-without-beat band).
- **G** — substrate failure.
- **K** — SIMD parity-hash fail (the checkasm differential gate).
- **L** — SIMD throughput fail (the substrate-guard hard-failure axis).
- **N-direct** — direct-projection failure (W4 digest-guard rows today).
- **S** — substrate-guard non-admission (W0 parse rows; the W0
  `w0_parse_non_admission` demotes admission-capable parse outcomes to S).

The S/L semantics the prompt flags are already correct in code:
`Outcome::SSubstrateGuardNonAdmission` maps to `Verdict::NoGo`
(`gate.rs:84-90`) and is the W0 demotion target; `Outcome::LSimdThroughputFail`
is a hard-failure axis carried unchanged through `w0_parse_non_admission`
(`gate.rs:374-379`). No semantic change is required.

**However** the SPEC §0.3 list is **narrower than the W0-admissible code
set** and the *rendered* RESULTS.md. Three of the 38 baseline rows can carry
`I`/`J`/`M` outcomes — `validate_w0_outcome` admits them, and they are
real diagnostic verdicts (`I` = oracle disagreement, `J` = invalid-input
schema rejection, `M` = memory-residency). P3-D's binding ruling:

> **The SK-V9 outcome enum is the 10-identifier W0-admissible set
> `A C G I J K L M N-direct S`, not the 7-identifier SPEC §0.3 subset.**
> The SPEC §0.3 list must be corrected to the 10-identifier set, because
> `validate_w0_outcome` already gate-admits all ten and the rendered
> baseline can carry `I`/`J`/`M`. A 7-identifier SPEC enum would make
> `gate-json` reject a row the code itself produces — a producer/consumer
> contradiction. This is a SPEC-text fix (P3-F's burden), not a code
> change.

`B`, `D`, `E`, `F-positive`, `F-noise` remain defined in `gate::Outcome`
but are **not** SK-V9-admissible (`validate_w0_outcome` rejects them as
"non-W0 outcome"); they are dormant variants from prior brackets. SK-V9
neither uses nor deletes them — deletion would be a behaviour-code change
outside the telemetry bracket.

## §4 — Measured-row admission fields (per P2-C)

P2-C §2.4 enumerates the schema-v3 + manifest fields a measured Apache/CITM
typed row must carry to be admitted by `validate_w0_admission_boundary`
(`report.rs:383`) and the strict-admission check `validate_strict_admission`
(`gate.rs:170-181`). These are not new fields — they are the §2.2 fields
bound to their *measured-row* values. The binding:

| Field | Measured-row value (Apache/CITM typed) | Source |
|---|---|---|
| `comparator_id` | `sonic_rs_strict` and `serde_json` (both rows present) | P2-C §2.4 |
| `comparator_plane` | `typed direct` — MUST match the row's `output_plane`; strict admission is strict-vs-strict on a matching plane only (SPEC §1) | P2-C §2.4 |
| `comparator_strictness` | `strict` for the SOTA anchor; `permissive` rows are flaw probes, never the gate | P2-C §2.4, `gate.rs:170-181` |
| `comparator_freshness` | `same-run-native` — a historical/sidecar/absent comparator cannot be the strict anchor (SPEC §1: no stale/historical/sidecar evidence as strict admission) | P2-C §2.4 |
| `run_id` | fresh `sk-v9-open:criterion-fnv64-<16 hex>` satisfying `is_skv9_open_run_id`; refreshed across the whole RESULTS file in the same wave | P2-C §2.2, §2.3 |
| `wave_id` | the per-wave id `sk-v9-real-typed-w{n}` — distinguishes "telemetry-lock recovery" (W0) from "first behaviour wave" | P2-C §2.3 |
| `costfacts_rule_id` / `costfacts_chosen_shape` / `costfacts_rejected_alternative_ids` | all `none:pre-W1` — a row-table admission wave produces no CostFacts (Lock 14) | P2-C §2.4 |
| `sk_v9_open_delta` | signed Δ vs the `SK-V9-open` baseline throughput for that row | §2.2 |
| `measured_validation_path` | the typed-product validation path, not `view-boundary`; the row admits only on generated-Track-1 typed output + independent Track 2/oracle | P2-C §2.5, SPEC §6 |
| `profile_artifact` | `criterion-slope-profile:<expected path>` matching `expected_profile_path(row_id)` | `report.rs` `validate_w0_profile_artifact` |
| `sample_cost` / `sample_count` | `ns_per_byte=…` (no `n/a`); `sample_count > 0` | `report.rs:276-388` |
| `outcome_id` | `A` (beat-and-parity) — must agree with the baseline outcome for `validate_w0_admission_boundary` to admit | P2-C §2.5 |
| `redress_entry` | the new `## SK-V9 Wave {n} Apache+CITM Typed Row-Table Admission Redress` anchor | P2-C §2 (d) |
| `same_wave_consumer_class` | `gate_only` — the measured row IS the artefact; no kernel ships in W2 | P2-C §2 (c) |

The admission rule: a measured typed row is admitted only when **all**
fields above carry their measured-row value, the comparator is
strict-vs-strict on the `typed direct` plane, and the row-table gate
(`cargo xtask gate-json --advisory --check-results`) passes after
promotion. Any field left at its W0 placeholder (`view-boundary`,
`none`, a stale run_id) blocks admission — this is exactly P2-C's "five
blocking deltas". CostFacts ids are bound to `none:pre-W1`: they are
required-present (non-empty) but their value is the no-CostFacts
constant, not an absent field.

## §5 — PMU manifest disposition

P1-V3-A established the real PMU table at
`/tmp/skv9-xctrace-v3/pmu_rows.tsv` — 34 rows of `cycles`, `instructions`,
`CPI`, `cycles/B` per (corpus, track), kpc-backed, produced by
`skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs`. P1-V3-C derives
per-class cycles/B from it; P2-E §6 uses it for per-row Mbps projection.

**Disposition: the PMU manifest stays diagnostic-only. SK-V9 promotes
NO PMU field into the gate schema.**

Grounds:

1. **SPEC §1 non-negotiable, verbatim**: "No structural-scan-only, masking
   probe, PMU, or Criterion slope artifact used as a producer for Track 1,
   Track 2, typed product, direct product, or strict admission."
2. **The `diagnostic_nonproducer_status` field is a fixed constant**:
   `validate_sk_v8_w0` (`report.rs`) hard-rejects any row whose
   `diagnostic_nonproducer_status != "structural_scan+masking_probes+pmu+cycles:nonproducer"`.
   The gate *enforces* PMU's non-producer status as a schema invariant.
   Promoting a `cycles_per_byte` column would contradict the very field
   that asserts PMU is a non-producer.
3. **The W0 close artifact and SPEC §3 exit-gate item 6** require
   "structural scan, masking probes, PMU, and cycles-per-byte remain
   diagnostic non-producers". SK-V9 cannot weaken that without re-opening
   W0.
4. **P2-E §6 uses cycles/B for *projection*, not *gating***: the per-row
   Mbps projections (`y_string_unicode` 5,457→~7,837, etc.) are
   research-grade forecasts that *inform* a falsifiability gate's
   threshold; the gate itself is then measured from the bench's Criterion
   Mbps, never from `pmu_rows.tsv`. The PMU table is an input to plan
   authoring, not a runtime gate producer.

The PMU table's role in SK-V9:

- **Diagnostic input** to S-P1/S-P2/S-P3 plan authoring — per-class c/B
  decomposition, hot-leaf attribution, the four uncloseable-row diagnosis.
- **NOT** a column in either the schema-v3 table or the W0 manifest.
- **NOT** consumed by `gate-json`. `xctrace_probe` is a standalone binary;
  its TSV output lives under `/tmp`, never under `skinny/RESULTS.md`.

Equivalent treatment for the per-symbol Time Profiler exports
(`/tmp/skv9-xctrace-v3/p1b-tp/exports/…`) and the structural-scan /
masking-probe artefacts: all diagnostic, none promoted.

The `hot_leaf` schema-v3 column is the one place hot-attribution touches
the gate — but its value is `criterion-slope-profile:<path>;hot-leaf=…;
row=…`, i.e. a *reference to* the Criterion profile, not a PMU figure.
That column predates SK-V9 and is unchanged.

## §6 — Schema-version tag + same-wave consumption rule

### §6.1 — Schema-version tag

The schema-v3 layer is identified in code by the constant pair
`SCHEMA_V3_HEADER` / `SCHEMA_V3_ALIGN` (`report.rs:8-9`). The "v3" suffix
is the **schema-shape version** — it denotes the 26-column SOTA-table
shape and is *not* bumped per SK bracket. SK-V9 keeps `schema-v3`: no
column is added or removed (§2.1), so the shape is unchanged.

The SK-V9-specific version is carried by **two orthogonal tags**, not by
a schema-shape bump:

1. **The wave-id tag** `SK-V9-open` — the manifest's `wave_id` field; the
   `## SK-V9 W0 Telemetry Manifest` heading; the validator hard-checks
   `wave_id == "SK-V9-open"` for baseline rows (`report.rs:335`).
2. **The run-id tag** `sk-v9-open:criterion-fnv64-<16 hex>` — the
   `SK_V9_OPEN_RUN_ID_PREFIX` constant (`report.rs:685`), validated by
   `is_skv9_open_run_id` (`report.rs:687-695`). The current baseline
   run-id is `sk-v9-open:criterion-fnv64-cd1673844eeea12f`.

**The SK-V9 schema-version string is therefore `schema-v3 / SK-V9-open`**,
realised as the pair `(SCHEMA_V3_HEADER constant, wave_id="SK-V9-open")`.
Behaviour waves W2–W4 do **not** bump `schema-v3`; they bump the
per-wave id (P2-C §2.3: `sk-v9-real-typed-w{n}`) inside the `wave_id`
field and mint a fresh `run_id` under the same `sk-v9-open:` prefix.
A schema-shape bump to `schema-v4` would only be warranted if a wave added
a column — which §2.1 forbids for SK-V9.

### §6.2 — Same-wave consumption rule

**The rule, verbatim for the SPEC:**

> Every field a wave emits into `skinny/RESULTS.md` MUST be consumed by
> `gate-json` in the same wave. A field rendered into the table or
> manifest but not read by `validate_schema_v3` (schema-v3 layer) or
> `validate_sk_v8_w0` / `validate_strict_admission` (manifest layer) is a
> **producer-only artefact** and fails the wave's exit gate. There is no
> "emit now, consume later". Symmetrically, `gate-json` must not require a
> field the wave does not emit — a required-but-absent field fails
> closed with an explicit absence reason.

Enforcement is already mechanical: `Report::validate_schema_v3`
(`report.rs:499-507`) and `Report::validate_sk_v8_w0` (`report.rs:509-521`)
iterate every row and call the per-row validators, each of which
enumerates the required identifiers and errors on any empty value. A wave
that adds a column to `SCHEMA_V3_HEADER` without adding the matching check
to `validate_schema_v3` would render a column the gate never reads —
caught by the schema-v3 header/align literal mismatch and by CHALLENGE
CH5 (hidden coupling). A wave that adds a check without the column fails
because `gate-json` cannot find the field.

Consequence for the SK-V9 wave plan:

- **W2** adds two *rows*, not columns; every field of those rows is
  consumed by the existing validators in the same wave. Compliant.
- **W3/W4**, if they add a kernel, populate `same_wave_consumer_class`
  with `<kernel>→<consumer>` and the wave must land that consumer in the
  same commit (the `no-deferrals` / orphan-kernel discipline). The field
  is consumed by `validate_sk_v8_w0`'s required-text loop. Compliant.
- No SK-V9 wave may emit a `cycles_per_byte` / PMU column — it would be
  producer-only by construction (§5), since no validator reads it.

The rule is the binding contract between this P3-D schema and P3-C's
falsifiability gates: P3-C's gates measure throughput from the
schema-v3 `track1_mbps` / `track2_mbps` columns and the comparator
columns — all of which are gate-consumed in-wave — so every P3-C gate is
measurable from a same-wave-consumed field.

## §7 — Sources

- `restart/skinny/tranches/sk-v9/SPEC.md` — §0.3 outcome posture, §0.4
  required-telemetry 31-name list, §1 non-negotiables (PMU non-producer),
  §3 W0 exit gate, §6 typed-row placeholder.
- `skinny/RESULTS.md` — schema-v3 rendered 26-column table; `## SK-V9 W0
  Telemetry Manifest` 22-column block; `sk-v9-open:criterion-fnv64-cd1673844eeea12f`.
- `skinny/crates/bbnf-bench/src/report.rs` — `SCHEMA_V3_HEADER`/`_ALIGN`
  (8-9); `SkV8ComparatorEvidence` (33-40); `SkV8Telemetry` (44-67);
  `RowMetadata` schema-v3 fields (73-91); `validate_schema_v3` (220-274,
  499-507); `validate_sk_v8_w0` (276-388, 509-521); `validate_w0_outcome`
  (977-988); `SK_V9_OPEN_RUN_ID_PREFIX` / `is_skv9_open_run_id` (685-695).
- `skinny/crates/bbnf-bench/src/gate.rs` — `enum Outcome` (4-66); verdict
  mapping (76-90); outcome-id string table (96-131); `validate_strict_admission`
  (170-181); `classify` (216-262); `worst_outcome` ordering (312-326).
- `skinny/crates/bbnf-bench/src/bin/gate.rs` — `Estimates::required_present`
  (1036+); `read_metadata_rows` bench list; `w0_parse_non_admission`
  (372-379).
- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
  — PMU table at `/tmp/skv9-xctrace-v3/pmu_rows.tsv` (34 rows; cycles,
  instructions, CPI, cycles/B); per-symbol Time Profiler exports;
  diagnostic-only posture.
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-C-hot-leaf-attribution.md`
  — `class_cycles_per_byte ≈ row_cycles_per_byte × class_%self`;
  `xctrace_probe.rs` as the launchable PMU producer.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-C-apache-citm-admission.md`
  — §2.2 artefact set; §2.3 run-id/wave-id provenance; §2.4 measured-row
  telemetry-row schema-v3 fields; §2.5 outcome classification + admission
  boundary.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-E-unicode-escape-codec.md`
  — §4.1 JSON production consumer (`unescape_string`,
  `parse-that-regex/src/lib.rs:718`); §6 per-row Mbps projection from the
  PMU TSV + per-class c/B (projection, not gating).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` — §2 P3-D scope; §8.2
  telemetry-binding load-bearing discipline.
