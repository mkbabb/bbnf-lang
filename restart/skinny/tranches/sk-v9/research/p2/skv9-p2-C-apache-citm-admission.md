# SK-V9 P2-C: Apache + CITM Measured-Row Typed Admission Methodology

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-18.
Scope: Methodology for lifting `apache_builds/real_typed_struct` and
`citm_catalog/real_typed_struct` from source/product parity (REDRESS 91) to
measured `A / GO` rows in `skinny/RESULTS.md` under the full SOTA-strict gate.
Output: this file.
P1 hot-leaf antecedents: per-string-span tiny scanner
`match_tiny_plain_string_with_cap::<16>` (apache 56.0% self-time, citm 24.0%),
whitespace skip (citm 23.1%, apache 10.3%), fused-dispatch structural walker
`dispatch_value`, `consume_array_next` (citm 5.1%) — from
`restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-C-hot-leaf-attribution.md`
§§154-191. PMU c/B Apache track1 2.910, CITM track1 1.180
(`/tmp/skv9-xctrace-v3/pmu_rows.tsv` rows 4-9).
Lock surface: this artefact does not touch Lock 1 (substrate union) or Lock
14 (frozen roots) directly; the methodology it proposes consumes both as
preconditions and produces only measured-row admission plus row-table gate
ownership for the existing real-typed schema path.

## §1 — REDRESS 91 differential: what kept the rows out

The W2 admission (commit `12aff1e4`, `skinny/REDRESS.md:2620-2659`) is partial
by construction. Source/product parity is admitted; benchmark row-table
admission is rejected. Five blocking deltas separate "source/product parity"
from "measured `A / GO` row." Each is a load-bearing artefact gap, not a
correctness gap.

1. **No measured row in the SK-V9-open report.** `skinny/RESULTS.md` (run
   `sk-v9-open:criterion-fnv64-cd1673844eeea12f`) carries exactly four
   `real_typed_struct A / GO` rows: `twitter`, `update_center`, `mesh`,
   `marine_ik` (RESULTS.md lines 7, 14, 17, 24 in the row block; lines 50
   and downstream in the schema-v3 block). Apache and CITM real-typed rows
   are absent. The W0 baseline table in `report.rs:709-958` and the gate
   admission-boundary helper `w0_real_typed_metadata_expected` in
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1199-1201` both confirm:
   `w0_real_typed_metadata_expected("apache_builds") == false`,
   `w0_real_typed_metadata_expected("citm_catalog") == false`. This is the
   load-bearing artefact gap — the gate intentionally does not require the
   Apache/CITM real-typed metadata bundle because the row-table baseline
   never admitted it.

2. **No same-run sonic-rs/serde strict anchor in the SK-V9-open Criterion
   manifest.** Although Criterion artefacts exist for
   `json_apache_builds/{track1,track2,sonic_rs,serde_json}_real_typed_struct/`
   under `skinny/crates/bbnf-bench/target/skv9-w0/criterion/`, the strict
   anchor pair has never been promoted into the W0 manifest because admission
   requires `validate_w0_admission_boundary` (`report.rs:384`) to recognise
   the row in `SK_V8_OPEN_BASELINE`. The W2 hardening V3 fold made the
   gate stop *requiring* source-fixture typed metadata
   (`HARDENING-W2-V3-CONSOLIDATED.md` §"Required Fold"), which fixed the
   `cargo xtask gate-json --advisory --check-results` failure but did not
   admit the typed row.

3. **Run-id provenance not bound to a typed-row admission gate.** The W0
   run-id validator `is_skv9_open_run_id` (`report.rs:687-695`) checks for
   the `sk-v9-open:criterion-fnv64-` prefix + 16-hex-digit suffix. The
   typed Criterion artefacts under `skv9-w0/criterion/json_apache_builds/`
   exist but their `metadata.toml` was never validated against the W0
   `cd1673844eeea12f` fingerprint as a measured-row promotion; they sit as
   parity-only ground truth (HARDENING-W2-V3 explicitly noted the run-id
   drift remained from independent causes).

4. **Track-2 oracle independence is not structurally independent.** W2
   Hardening V1 CH1 #3 (`wave-2-hardening/V1/CH1.md:13`) flagged that
   `track2_typed` and `serde_typed` are both `serde_json::from_slice` — i.e.
   Track 2 ≡ Track 2/oracle ≡ same engine. W2 V2 accepted this by renaming
   the lane to "serde_json is the Track 2/oracle path" and adding a separate
   sonic-rs strict parity check (`real_typed_struct.rs:251-281`), but that
   leaves the existing four typed `A / GO` rows with the same architecture:
   the Track 2 oracle is *not* a structurally-independent typed parser, it
   is a serde fold. SOTA-strict admission still passes (sonic-rs strict is
   the comparator anchor; serde-as-oracle is structurally different from the
   generated DirectBuild at the implementation level), but the methodology
   must name the oracle independence claim explicitly.

5. **Lock 14 W2 parent-diff allowance scope.** Per
   `wave-2-hardening/V2/HARDENING-W2-V2-CONSOLIDATED.md:14-19`, the Lock 14
   allowance for `sk-v8-real-typed-w2` is scoped to `sk-v8-wave2` commits
   touching the three real-typed owner paths
   (`real_typed_schema.rs`, `real_typed_struct.rs`, `generated_real_typed.rs`).
   A V9 row-table wave that touches *any* additional owner path (e.g.,
   `gate.rs`, `report.rs`, `metadata.rs`) must own a fresh Lock 14
   allowance under a new schema identity (`sk-v9-real-typed-w{n}`).

REDRESS 91's verbatim posture: *"W2 therefore admits source/product parity
only and does not claim six measured `real_typed_struct A / GO` rows"*
(`REDRESS.md:2651-2652`). The W6 V2 hardening + alpha-C-redress-digest §"SK-V9
framing that may admit" set the binding criterion: *"A dedicated typed
benchmark row-table tranche may admit Apache/CITM only if it owns
run-id/metadata validation, produces fresh measured rows, keeps the four
existing typed GO rows as guards, and preserves independent Track 2/oracle
proof"* (`alpha-C-redress-digest.md:88-91`).

## §2 — Measured-row admission methodology: the artefact set

A typed `real_typed_struct A / GO` row at the full SOTA-strict gate requires
the artefact set below. Every item is mechanically enforced; each citation
is to the producer (gate/report) and the consumer (admission test).

### §2.0 — Per-slice LOC + minute sub-budgets

The aggregate envelope (300 LOC, ≤90 min, per HANDOFF §3 row 1) decomposes
across five disjoint artefact slices. The sub-budgets below are preliminary
(S-P3 P3-B finalises wave-level caps); the discipline is that every slice
carries its own LOC + minute ceiling and its own one-sentence revert
protocol. Cap policy follows the CH4 §4.1 schedule: hand-LOC ≤30 → ~15
min; 30-100 → ~30 min; >100 → ~45-60 min; regen → ~10 min plus the
codegen-template hand-LOC.

| Slice | Artefact surface | LOC sub-budget | Minute sub-cap | Same-wave consumer | Revert protocol |
|---|---|---:|---:|---|---|
| **(a) Baseline manifest edits** | `report.rs:709` `SK_V8_OPEN_BASELINE` — add Apache + CITM rows; possibly rename constant to `SK_V9_OPEN_BASELINE` under the wave-id bump path (§2.3) | ~30 hand (2 `sk_v8_open_baseline!` entries × ~7 lines + optional constant rename) | ~15 min | gate (telemetry; not a parse-loop consumer) | If the baseline-vs-measured slack check at `validate_w0_admission_boundary` fires, revert the two new entries; the four existing typed GO rows act as guards and must hold their A/GO outcome unchanged |
| **(b) Gate test flips** | `gate.rs:1820-1831` regression test `w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures` — flip Apache/CITM assertions from `!w0_real_typed_metadata_expected(...)` to `w0_real_typed_metadata_expected(...)` | ~10 hand (2 assertion edits + comment refresh) | ~15 min | self (the test IS the consumer for the baseline-fixture contract) | If the test refuses to compile or the assertion flip exposes a baseline/fixture drift, revert both assertions to `!expected` and route back to S-P2/S-P3 |
| **(c) RESULTS table promotions** | `skinny/RESULTS.md` — promoted row block (two new `real_typed_struct A / GO` rows + two new schema-v3 telemetry rows); refreshed run-id across the whole file | ~120 (4 row block + 2 schema-v3 telemetry rows ≈ 70 lines + run-id refresh across ~50 lines) | ~30 min | self (the row IS the artefact) | If `cargo xtask gate-json --advisory --check-results` fails after promotion, revert RESULTS.md to the pre-promotion run-id snapshot |
| **(d) REDRESS entry** | `skinny/REDRESS.md` — new entry `## SK-V9 Wave {n} Apache+CITM Typed Row-Table Admission Redress` recording the promotion, the fresh run-id, the no-regression guard against the four existing typed GO rows, the structurally-independent Track 2/oracle claim, and the per-row throughput | ~80 (single entry, ~80 lines of prose modelled on REDRESS 91's shape) | ~15 min | docs (not a runtime consumer) | If any §4.3 falsifiability gate fires, the REDRESS entry's "promotion" framing is replaced by a "falsification report" framing and the promotion itself is reverted |
| **(e) HANDOFF + LOCKS reflections** | `restart/skinny/tranches/sk-v9/HANDOFF.md` §3 row 1 — move candidate from "may admit" to "admitted under SK-V9 W{n}"; `restart/locks/LOCKS.md` (Lock 14) — add `sk-v9-real-typed-w{n}` parent-diff allowance entry scoped to the seven owner paths | ~15 (HANDOFF state-update ~5 lines + Lock 14 allowance ~10 lines) | ~10 min | scoped to seven owner paths | If Lock 14 `cargo test -p bbnf-bench lock14_baseline` fails, revert the LOCKS.md allowance and route the wave through Lock 14 amendment instead |
| **Aggregate** | five slices | **~255 hand + run-id refresh** ≈ **~300 total** | **~85 min** ≤ 90 min | gate + self + docs + scoped Lock allowance | per §4.3 close: halt at redress, record falsified gate, route back to S-P2/S-P3 without admitting |

The sub-budgets sum within the HANDOFF envelope (300 LOC, ≤90 min) with a
small minute-margin (~5 min) preserved for the verification matrix at
§2.9. The Criterion capture artefacts under
`skinny/crates/bbnf-bench/target/skv9-w{n}/criterion/` are not LOC-bearing
(capture is the artefact, not source); the capture's minute budget is
the row-bench wall-clock and runs out-of-band of the five-slice
authoring window.

### 2.1 Row-baseline admission

The row identity must be added to `SK_V8_OPEN_BASELINE`
(`skinny/crates/bbnf-bench/src/report.rs:709`), one entry per admitted row:

```rust
sk_v8_open_baseline!(
    "json/apache_builds/real_typed_struct/main",
    "A",
    "GO",
    <track1_mbps>,
    <track2_mbps>,
),
sk_v8_open_baseline!(
    "json/citm_catalog/real_typed_struct/main",
    "A",
    "GO",
    <track1_mbps>,
    <track2_mbps>,
),
```

Adding these triggers two cascading effects:
- `w0_real_typed_metadata_expected("apache_builds") = true`
  (`gate.rs:1199-1201`), which makes `required_metadata_specs(true)` add the
  four real-typed metadata specs (`gate.rs:1383-1419`).
- The W2 V3 regression test
  `w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures`
  (`gate.rs:1826-1831`) must be updated to flip the Apache/CITM assertions
  from `!expected` to `expected`. The test name explicitly states the
  invariant: measured baseline drives the metadata requirement, not source
  fixtures. A row-table admission wave is the only mechanism for the flip.

### 2.2 Same-run Criterion comparator anchor

Each admitted row needs four Criterion groups under one same-run capture:

| Criterion id | Track | Role |
|---|---|---|
| `track1_real_typed_struct` | bbnf generated DirectBuild | row throughput |
| `track2_real_typed_struct` | independent typed oracle | parity + slack |
| `sonic_rs_real_typed_struct` | sonic-rs strict | SOTA anchor |
| `serde_json_real_typed_struct` | serde_json strict | strict floor |

The four ids are already wired in `gate.rs:1383-1419` and consumed in the
report's row-rendering code (`report.rs:1029, 1119, 1355, 1549, 1561`). The
capture must agree across the four groups on host triple, build flags, CPU
model, OS kernel, RUSTFLAGS, target_cpu, profile, bbnf commit, warmup
samples, warmup time, sample size, measurement time, confidence interval,
outlier rejection, and statistical method (`validate_w0_capture_metadata`
+ `CaptureMetadata::validate_same_capture`, `gate.rs:1221-1264`).

### 2.3 Run-id provenance

Telemetry `run_id` must satisfy `is_skv9_open_run_id`:
`sk-v9-open:criterion-fnv64-<16 hex>` (`report.rs:685-695`). For a
post-SK-V9-open row-table wave, the run-id may carry a fresh fingerprint
under the same prefix — but the report-level invariant in `report.rs:518-533`
requires *one* run-id across the whole report. Two paths exist:

- **Co-promotion path.** Re-render the full report under a single fresh
  run-id `sk-v9-open:criterion-fnv64-<new16>` covering the 21 existing
  measured rows + Apache + CITM typed (= 23 rows total once the four
  typed-metadata groups are bound). The four existing typed GO rows
  (twitter, update_center, mesh, marine_ik) act as guards: their measured
  values must not regress against the SK-V9-open baseline anchor by more
  than the configured slack.
- **Wave-id bump path.** Promote the wave id to `SK-V9-w{n}` with a new
  baseline constant (`SK_V9_W{n}_BASELINE`) and a new run-id prefix. The
  expected-wave check at `report.rs:335` (`telemetry.wave_id != "SK-V9-open"`)
  must then accept the new id, and the SK-V9-open baseline becomes a
  named guard table. This is the cleaner architectural path because it
  separates "telemetry-lock recovery" (W0) from "first behaviour wave"
  (W{n}).

### 2.4 Telemetry-row schema-v3 fields (per
`report.rs:280-326` + `report.rs:609-680`)

Every promoted row carries:
- `row_id = json/<corpus>/real_typed_struct/main`
- `comparator_strictness = strict`
- `comparator_plane = typed direct` (sonic_rs and serde rows both;
  `report.rs:1355,1358,1561`)
- `row_output_plane = typed direct`
- `measured_validation_path = view-boundary` (existing typed GO rows; the
  typed parser validates UTF-8 at the view boundary, same as current rows
  at RESULTS.md lines 7,14,17,24)
- `comparator_freshness = same-run-native`, `sidecar_freshness = n/a`
  (required by `validate_strict_admission`, `gate.rs:170-181`)
- `parse_utf8 = view-boundary`, `escape_complete = yes`
- `costfacts_* = none:pre-W1` (Lock 14: row-table wave does not produce
  CostFacts)
- `same_wave_consumer_class = gate_only`
- `diagnostic_nonproducer_status =
  structural_scan+masking_probes+pmu+cycles:nonproducer`
- `host_triple = aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`
- `build_flags = profile=bench;rustflags=-C target-cpu=native;target_cpu=native`
- `feature_mask = arch=aarch64;os=macos;simd=Scalar;target_cpu=native`
- `sample_count` from Criterion, `sample_cost = ns_per_byte=<x>;track1_ns=<y>;bytes=<z>`
- `profile_artifact = criterion-slope-profile:json_<corpus>/track1_real_typed_struct/new/estimates.json`
  (per `expected_profile_path`, `report.rs:1024-1033`)
- `hot_leaf =
  <profile_artifact>;hot-leaf=criterion-slope-profile;row=json/<corpus>/real_typed_struct/main`

### 2.5 Outcome classification + admission boundary

The W0 outcome whitelist (`validate_w0_outcome`, `report.rs:977-988`)
permits `A` for any row. The substrate-guard restriction at `report.rs:375-382`
applies only to `parse_only` rows. For `real_typed_struct`, the row passes
`validate_w0_admission_boundary` if its outcome agrees with the baseline
or relabels under `w0_allows_fresh_diagnostic_outcome` (which permits only
G/L/M/S relabels, `report.rs:990-994`). For an admitted A row, the
baseline outcome must already be `A`.

### 2.6 Comparator-evidence validation

`validate_comparator_evidence(&row_id, &workload="real_typed_struct",
&telemetry.comparators)` (`report.rs:383`) enforces:
- `sonic_rs_strict` row present, plane=`typed direct`, strictness=strict,
  freshness=`same-run-native`, source=`criterion:json_<corpus>/sonic_rs_real_typed_struct/new/estimates.json`.
- `serde_json` row present, plane=`typed direct`, strictness=strict,
  freshness=`same-run-native`, source=`criterion:json_<corpus>/serde_json_real_typed_struct/new/estimates.json`.
- C++ sidecar comparators (`simdjson_dom`, `simdjson_ondemand`,
  `yyjson_default`, `asmjson_swar`, `asmjson_avx512`, `rapidjson_default`)
  may be `absent:not-collected-for-real_typed_struct` (the existing four
  typed GO rows all carry that sidecar shape — see RESULTS.md line 50 for
  `twitter/real_typed_struct/main`).

### 2.7 Track-2 oracle independence claim

The existing four typed GO rows pass the SOTA gate using serde_json as
the Track 2/oracle (`real_typed_struct.rs:259-282`,
`wave-2-hardening/V2/HARDENING-W2-V2-CONSOLIDATED.md:18-19`). The
methodology preserves that choice. The independence claim is:
- Track 1 = generated DirectBuild typed parser
  (`generated_real_typed::parse_apache_builds`,
  `generated_real_typed::parse_citm_catalog`).
- Track 2/oracle = `serde_json::from_slice::<ApacheBuilds<'a>>` /
  `serde_json::from_slice::<CitmCatalog<'a>>`
  (`real_typed_struct.rs:266-270`).
- Strict parity = full-fixture `assert_real_typed_parity` checksum equality
  across {generated, serde, sonic} (`real_typed_struct.rs:310-323`).

This is structurally independent at the implementation level: generated
DirectBuild visits tape positions and projects field-by-field; serde
walks a parser-decoded value stream. They share no scanner, no parser,
no allocator, and no codegen template.

**JSON-specificity of the "structurally independent" definition.** The
claim "Track 2 oracle is structurally independent" is, as stated here, a
JSON-internal definition: it is satisfied by `serde_json::from_slice`
because serde walks a JSON-decoded value stream that shares no
implementation surface with the generated DirectBuild typed parser.
`serde_json` is the JSON oracle and has no cross-grammar equivalent —
there is no `serde_json` for CSS L4 or Sheets. A future non-JSON typed
admission wave (per §5.1) therefore cannot inherit this oracle; it must
nominate a per-grammar independent typed parser whose oracle shape is
appropriate to that grammar. The methodology body does not commit to any
particular oracle engine — the structural-independence criterion (shares
no scanner, no parser, no allocator, no codegen template with Track 1) is
the invariant, and each grammar's row-table wave selects an oracle that
satisfies it. Cross-grammar tracks may use oracle shapes that look
nothing like a serde value-stream fold: CSS L4 may use a `lightningcss`
`Visitor` walk over a decoded stylesheet AST; Sheets may use a
cell-by-cell reference CSV parser; TOML may use a `taplo` AST walker. The
oracle's *shape* is grammar-dependent; the oracle's *independence
property* is the grammar-neutral invariant the methodology binds.

### 2.8 PMU evidence for the typed track

**Pre-block.** The S-P1 PMU table at `/tmp/skv9-xctrace-v3/pmu_rows.tsv`
captures parse-only only — the probe binary
(`skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs`) is hard-wired to
`runtime::generated_json::parse` for Track 1 and
`bbnf_bench::track2::json::parse` for Track 2
(`skv9-p1-v3-A-xctrace-cpu-counters.md:28-46`). The probe has no typed
codepath. PMU cycles-per-byte for `real_typed_struct` rows therefore does
not exist in the SK-V9-open evidence. The HANDOFF declares G-Alpha and
the four typed GO rows on parse-only PMU evidence + same-run Criterion
slope; the same posture admits Apache/CITM under measured-row promotion
without a typed-probe PMU extension, because PMU is currently a diagnostic
non-producer
(`diagnostic_nonproducer_status=...pmu+cycles:nonproducer`,
`report.rs:341-348`).

If the wave wishes to bind typed cycles-per-byte, the probe needs a
`track1_real_typed` / `track2_real_typed` mode that calls
`parse_apache_builds` / `parse_citm_catalog`. This is *optional* under the
current schema and not required for `A / GO` admission, but it would
unblock CH4 cost-modelling for downstream waves.

### 2.9 Full verification matrix (per HARDENING-W2-V2 §"Verification")

The admission commit verifies under:
- `cargo xtask regen-real-typed` (existing typed-schema regen)
- `cargo test -p bbnf-bench lock14_baseline -- --nocapture` (Lock 14
  W{n} parent-diff allowance must be present and scoped)
- `cargo test -p bbnf-bench real_typed -- --nocapture` (full-fixture
  parity, including new Apache/CITM guards)
- `cargo xtask check-real-typed` (frozen typed-schema set)
- `cargo test -p codegen typed_direct -- --nocapture` (codegen-side
  typed scalars)
- `cargo xtask check-json`, `cargo xtask check-conformance`
- `cargo xtask gate-json --advisory --check-results` — must succeed
  after promotion (the W2 V3 fold left the Apache/CITM source-fixture
  expansion in place, but the gate now derives requirements from the
  measured baseline; promoting the rows extends both)
- `git diff --check`

## §3 — Apache + CITM specifics: per-row

### 3.1 `apache_builds/real_typed_struct`

| Field | Value |
|---|---|
| Host-API output schema | `ApacheBuilds<'i>` (root `mode: Option<Cow<str>>`, `nodeName: Option<Cow<str>>`, `jobs: Vec<ApacheJob<'i>>`) + `ApacheJob<'i>` (`name`, `url`, `color`: all `Option<Cow<str>>`); pre-sized `jobs` capacity 875 — `real_typed_schema.rs:57-74`, `real_typed_struct.rs:33-51` |
| Track 1 binding | `parse_apache_builds(input)` — `generated_real_typed.rs` (regen'd from `real_typed_schema.rs`); fixture key `apache_builds` / `apache-builds` (`real_typed_struct.rs:185`) |
| Track 2 / oracle | `serde_json::from_slice::<ApacheBuilds<'a>>(bytes)` (`real_typed_struct.rs:266-270`); structurally independent from generated DirectBuild |
| Sonic strict anchor | `sonic_rs::from_slice::<ApacheBuilds<'a>>(bytes)` (`real_typed_struct.rs:292-294`) → Criterion id `sonic_rs_real_typed_struct` |
| Comparator plane | `typed direct` (both sonic and serde) |
| Measured validation path | view-boundary (UTF-8 at the typed accessor boundary, matching existing typed GO rows) |
| PMU c/B (parse-only, S-P1 V3) | Track 1 2.910, Track 2 2.862 (`/tmp/skv9-xctrace-v3/pmu_rows.tsv` rows 8-9). Typed track not measured. |
| Parse-only Mbps (RESULTS line 8) | Track 1 11917, Track 2 11410, sonic strict 15536; bbnf is -23.3% behind sonic on parse_only |
| Direct-to-struct Mbps (RESULTS line 9) | Track 1 10577, Track 2 9126, sonic strict 9073; bbnf is +16.6% over sonic, but flagged `N-direct/NO-GO` under W0 no-admission clamp |
| Expected typed throughput | Apache parse-only is hot in `match_tiny_plain_string_with_cap::<16>` (56.0% self-time) — the same tiny-key scanner the typed path uses. The typed product plane projects only `mode`, `nodeName`, and each job's three strings; the projection is strict-subset of the parse_only work plus per-field allocation/borrow. Expected behaviour: typed throughput between parse_only Mbps and direct_to_struct Mbps, comfortably ≥ `ceil(sonic_strict_typed_Mbps / 1.10)`. SK-V8-open baseline (now superseded) recorded Apache typed Track 1 8306 / Track 2 7796 Mbps on the SK-V8 capture (`report.rs:760-766`, currently classed `N-direct/NO-GO` because never re-measured under SK-V9-open). |
| Falsifiability Mbps gate | Apache `track1_real_typed_struct` ≥ `ceil(sonic_rs_real_typed_struct_Mbps / 1.10)`. Per existing typed GO rows: twitter Track1 14761 vs sonic 14665 (+0.7%); update_center 11345 vs 11874 (-4.5%); mesh 8919 vs 8531 (+4.6%); marine_ik 11259 vs 8990 (+25.2%). Apache must clear the same +/- 9.1% strict slack. |

### 3.2 `citm_catalog/real_typed_struct`

| Field | Value |
|---|---|
| Host-API output schema | `CitmCatalog<'i>` (`events: Vec<CitmEventEntry<'i>>` via custom `MapAccess`-driven `deserialize_citm_event_entries`) + `CitmEvent<'i>` (`id: Option<u64>`, `name: Option<Cow<str>>`, `subTopicIds: Vec<u64>`, `topicIds: Vec<u64>`); pre-sized events capacity 184 — `real_typed_schema.rs:75-99`, `real_typed_struct.rs:53-75` |
| Track 1 binding | `parse_citm_catalog(input)` — generated typed parser with keyed-entry `map_entries` codegen primitive |
| Track 2 / oracle | `serde_json::from_slice::<CitmCatalog<'a>>(bytes)` (`real_typed_struct.rs:269-272`) |
| Sonic strict anchor | `sonic_rs::from_slice::<CitmCatalog<'a>>(bytes)` (`real_typed_struct.rs:295-297`) |
| Comparator plane | `typed direct` |
| Measured validation path | view-boundary |
| PMU c/B (parse-only, S-P1 V3) | Track 1 1.180 (corpus-wide low), Track 2 1.703 (`/tmp/skv9-xctrace-v3/pmu_rows.tsv` rows 4-5). Typed track not measured. |
| Parse-only Mbps (RESULTS line 5) | Track 1 29215, Track 2 19600, sonic strict 23590; bbnf is +23.8% over sonic on parse_only |
| Direct-to-struct Mbps (RESULTS line 6) | Track 1 20229, Track 2 19065, sonic strict 18742; bbnf is +7.9% over sonic; flagged `A/GO` (the only direct GO row that's not unicode_basic/marine_ik) |
| Expected typed throughput | Citm typed projects keyed event entries with two u64 vectors per event. The typed path's bottleneck is `match_tiny_plain_string_with_cap::<16>` (24.0%) + whitespace skip (23.1%) — both shared with parse_only. The typed map-entries codegen adds per-entry `CitmEventEntry` allocation + `key` Cow projection. Expected throughput: ≥ sonic strict typed Mbps × 1.10⁻¹. Citm's high parse_only headroom (+23.8% over sonic) and its existing direct-GO status suggest the typed row will admit at A. |
| Falsifiability Mbps gate | CITM `track1_real_typed_struct` ≥ `ceil(sonic_rs_real_typed_struct_Mbps / 1.10)`; same +/- 9.1% strict slack as existing typed GO rows. |

Both rows are pre-admitted at the source layer (REDRESS 91): the typed
parsers exist (`generated_real_typed.rs` is regen'd from
`real_typed_schema.rs:57-99`), the strict parity tests pass
(`real_typed_struct.rs:595-618`), and the schema identity is
`sk-v8-real-typed-w2`. The methodology only needs to produce the
measured-row capture and promote the baseline.

## §4 — Wave shape, owner files, falsifiability gates

S-P2 surfaces this as a candidate for S-P3 to sequence. The wave is
proposed as **V9-W{first behaviour wave after S-P3 close}**, and is
named here as **W{n}-A: Apache + CITM Typed Row-Table Admission**. The
wave is row-table-only; it produces no new parser, no new codegen
template, no new substrate, no new SIMD primitive, no Lock 1 or Lock 14
amendment. The HANDOFF candidate-boundary table
(`restart/skinny/tranches/sk-v9/HANDOFF.md` §3 row 1) names this exact
candidate: *"A V9 row-table wave must own fresh run-id/metadata
validation and measured rows before claiming six measured
`real_typed_struct A / GO` rows"*; LOC budget 300, hard cap ≤90 min.

### 4.1 Owner files

Per §2.0 the seven owner paths cluster into five disjoint artefact
slices. The table below carries the slice label + per-file LOC sub-budget
+ minute sub-cap from §2.0; rationale prose carries the reason-for-ownership
for each path within its slice.

| Slice | File | LOC sub-budget | Minute sub-cap | Reason for ownership |
|---|---|---:|---:|---|
| (a) baseline | `skinny/crates/bbnf-bench/src/report.rs:709` (`SK_V8_OPEN_BASELINE`) | ~30 | ~15 min | Add the two new rows. Possibly rename the constant to `SK_V9_OPEN_BASELINE` if W{n} bumps the wave id (§2.3). |
| (b) gate test | `skinny/crates/bbnf-bench/src/bin/gate.rs:1820-1831` (regression test `w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures`) | ~10 | ~15 min | Flip the Apache/CITM assertions from `!w0_real_typed_metadata_expected(...)` to `w0_real_typed_metadata_expected(...)`. |
| (c) RESULTS | `skinny/RESULTS.md` | ~120 | ~30 min | Promoted row block (two new `real_typed_struct A / GO` rows + two new schema-v3 telemetry rows). Run-id refreshed across the whole file. |
| (capture, out-of-band) | `skinny/crates/bbnf-bench/target/skv9-w{n}/criterion/` | non-LOC | wall-clock | Fresh same-run Criterion capture across 21+2 rows × 4 typed Criterion ids. The capture is the artefact (not source); minute budget runs out-of-band of the five-slice authoring window. |
| (d) REDRESS | `skinny/REDRESS.md` | ~80 | ~15 min | New entry: `## SK-V9 Wave {n} Apache+CITM Typed Row-Table Admission Redress` — records the promotion, the fresh run-id, the no-regression guard against the four existing typed GO rows, the structurally-independent Track 2/oracle claim, and the per-row throughput. Supersedes REDRESS 91's "not measured rows" clause for these two rows only; REDRESS 91 remains binding for `canada/real_typed_struct`. |
| (e) HANDOFF | `restart/skinny/tranches/sk-v9/HANDOFF.md` §3 row 1 | ~5 | ~5 min | Move the candidate from "may admit" to "admitted under SK-V9 W{n}." |
| (e) LOCKS | `restart/locks/LOCKS.md` (Lock 14) | ~10 | ~5 min | Add `sk-v9-real-typed-w{n}` parent-diff allowance entry scoped to the seven owner paths above (no source under `runtime/`, no `bbnf-simd`, no `codegen` outside the already-frozen typed schema). |
| **Aggregate** | seven paths (five slices) | **~255 hand** ≈ **~300 total with run-id refresh** | **~85 min** ≤ 90 min | within the HANDOFF §3 row 1 envelope; ~5 min margin preserved for the §2.9 verification matrix |

Out of scope (must remain untouched, per REDRESS 91 + alpha-C-redress-digest):
- `skinny/crates/runtime/`, `skinny/crates/bbnf-simd/`, `skinny/crates/codegen/`
  (except as touched by `cargo xtask regen-real-typed` if rerun, which must
  produce a byte-identical `generated_real_typed.rs`).
- `skinny/xtask/src/real_typed_schema.rs` (already W2-frozen at schema
  identity `sk-v8-real-typed-w2`; the W{n} wave does not change the schema).
- `canada/real_typed_struct` (REDRESS 91 rejection still binds; the wave
  does not weaken the checksum-mismatch route-out).

### 4.2 Dispatch sequence (single-redress)

1. **Pre-flight (read-only).** Re-run `cargo test -p bbnf-bench
   real_typed -- --nocapture` against current HEAD to confirm Apache/CITM
   parity still passes under SK-V9-open. Confirm
   `cargo xtask check-real-typed` still rejects Canada.
2. **Capture.** Bind a fresh same-run Criterion capture covering the 21
   existing measured rows + the four Apache typed Criterion ids + the
   four CITM typed Criterion ids = 21 + 8 - 4 (Apache parse/direct already
   counted) - 4 (CITM parse/direct already counted) = 21 + 8 - 8 = 21
   row-bench groups extended by 8 new Criterion ids. Use
   `RUSTFLAGS="-C target-cpu=native"` and the W0 capture envelope.
3. **Promotion.** Add the two baseline entries, flip the regression test
   assertions, render `skinny/RESULTS.md` end-to-end with the fresh
   run-id, write the REDRESS entry.
4. **Verify.** Full verification matrix per §2.9, plus a no-regression
   check on the four existing typed GO rows (twitter, update_center,
   mesh, marine_ik) against the SK-V9-open baseline anchors at
   `report.rs:718-724, 795-801, 810-816, 853-859`.
5. **Redress + handoff.** Commit `feat(skv9-w{n}): admit Apache+CITM
   measured typed rows` covering the owner-file changes only.

### 4.3 Falsifiability gates (Mbps thresholds)

The wave halts and reverts if any of the following fail at measurement:

| Gate | Threshold | Source |
|---|---|---|
| Apache typed Track 1 ≥ sonic strict typed × 1.10⁻¹ | bbnf must clear `ceil(<sonic_apache_typed_Mbps> / 1.10)` | `DIRECT_PROJECTION_SONIC_SLACK = 1.10` (`gate.rs:56`); typed slack is the same per existing GO rows (twitter -0.6% to +4.6% range) |
| CITM typed Track 1 ≥ sonic strict typed × 1.10⁻¹ | same | same |
| Typed Track 1 parity passes for both rows | full-fixture checksum equality bbnf ≡ serde ≡ sonic | `assert_real_typed_parity` (`real_typed_struct.rs:310-323`) |
| Existing four typed GO rows hold their `A / GO` outcome | no regression below sonic × 1.10⁻¹ for twitter, update_center, mesh, marine_ik typed Track 1 | `validate_w0_admission_boundary` (`report.rs:384`); plus an explicit "no regression vs SK-V9-open typed baseline" guard in the wave's redress |
| Direct row admission (`direct_to_struct` rows) unchanged | the two existing direct rows for Apache (N-direct/NO-GO) and CITM (A/GO) must remain at their SK-V9-open verdicts; this wave does not touch direct admission | `w0_allows_fresh_diagnostic_outcome` (`report.rs:990-994`) bars baseline drift for A rows; the wave's redress documents zero direct changes |
| Lock 14 W{n} parent-diff scope satisfied | `cargo test -p bbnf-bench lock14_baseline -- --nocapture` green | `lock14_baseline.rs` (per W2 V1 CH1 #2 evidence) |
| Run-id provenance | every row in the rendered report carries the fresh `sk-v9-open:criterion-fnv64-<new16>` or `sk-v9-w{n}:criterion-fnv64-<new16>` run-id | `is_skv9_open_run_id` (`report.rs:687-695`); if the wave id bumps, the regex extends |

A wave that misses any gate halts at the redress phase, records the
falsified gate in REDRESS, and routes back into S-P2/S-P3 without
promoting the row.

## §5 — Generalisation to other rows

The 14 `N-direct / NO-GO` rows (RESULTS.md lines 6, 11, 13, 15, 17 row,
20, 23, 25, 30, 32, 34, 36, 38, 40, 42) and the three `direct_to_struct
A / GO` rows (citm, marine_ik, unicode_basic) all use a `digest`
comparator plane — they are scalar-folded checksum probes, not typed
products. Lifting them to typed `real_typed_struct` requires the same
prerequisite REDRESS 91 satisfied for Apache/CITM: a host/API typed
output schema must exist, and full-fixture DirectBuild-vs-serde parity
must pass.

| Corpus | Host-API schema status | Typed-row lift candidate? |
|---|---|---|
| twitter | already admitted (`TwitterSearch`, `Tweet`) | already typed GO |
| apache_builds | admitted source slice (`ApacheBuilds`) | THIS WAVE |
| citm_catalog | admitted source slice (`CitmCatalog`) | THIS WAVE |
| update_center | already admitted (`UpdateCenter`, `Plugin`) | already typed GO |
| mesh | already admitted (`Mesh`, `MeshBatch`) | already typed GO |
| marine_ik | already admitted (`MarineIk`, `MarineGeometry`) | already typed GO |
| canada | host schema not viable | BLOCKED — REDRESS 91 long-decimal checksum mismatch; route-out is binding |
| github_events | no host schema in `real_typed_schema.rs` | Eligible: build a host/API schema from the github events typed view; risk is per-fixture polymorphic event payloads |
| random | synthetic corpus, no canonical host schema | Not a natural typed candidate; consider closing as N-direct permanent |
| gsoc-2018 | no host schema | Eligible after schema design; large nested project payload |
| numbers | numeric stress corpus | Not a typed-product candidate; numbers are the projection |
| instruments | no host schema | Eligible after schema design |
| unicode_mixed, unicode_escapes, unicode_basic, y_string_unicode, distinct_values | string-validation stressors, no host schema | Not typed candidates — these are scanner-correctness probes, not product surfaces |

The methodology in §2 generalises cleanly to **github_events**,
**gsoc-2018**, and **instruments** if a host/API schema is authored and
admitted under the same shape (source/product parity first, then a
measured-row wave). It does not generalise to canada (REDRESS 91
blocker), random (synthetic), numbers (stressor), or the unicode/string
corpora (scanner probes, not products).

### §5.1 — Cross-grammar transposition: the generic pattern

The seven owner-file shape (baseline constant + gate regression test +
RESULTS.md row block + Criterion capture + REDRESS entry + HANDOFF
state-update + Lock 14 parent-diff allowance) is grammar-neutral by
construction. The generalisation rule, stated abstractly: *any grammar
whose host-API schema admits a fact-typed Track 1 product plane can use
this methodology to promote a measured `<grammar>_real_typed_struct A /
GO` row, provided source/product parity is already established and the
Track 2/oracle is structurally independent at the implementation level.*

A future CSS L4 / Sheets / TOML / BBNF-self typed-product row-table wave
under SK-V{N>9} replicates the shape with `<grammar>_real_typed_struct`
row ids and a `sk-v{N>9}-<grammar>-real-typed-w{n}` schema identity. The
methodology does not require codegen-side changes to admit non-JSON
typed product planes; the codegen-emitted DirectBuild path is grammar-
neutral by Lock 14 (the substrate carries zero grammar-specific code),
and the per-grammar binding lives entirely in the grammar metadata +
generated typed-schema set.

Cross-grammar examples (illustrative; not in scope for SK-V9 skinny):

| Grammar | Host-API equivalent (Track 1 source) | Track 2/oracle candidate |
|---|---|---|
| **CSS L4** | `lightningcss` visitor pattern (Rust): the `Visitor` trait over `StyleSheet<'i>` produces a fact-typed accessor onto declarations, selectors, at-rules; the typed product plane is `Stylesheet<'i>` with `Vec<Rule<'i>>` and per-rule typed views | a second CSS parser walking a parser-decoded AST stream (e.g., `cssparser`'s tokenizer + AST visitor) — structurally independent from the lightningcss visit |
| **Sheets (CSV/spreadsheet)** | cell-by-cell typed access: each cell carries a `CellValue<'i>` (number / string / boolean / formula / blank); the typed product plane is `Sheet<'i>` with `Vec<Row<'i>>` and per-cell typed dispatch | a second CSV parser (e.g., a recursive-descent reference walker) producing the same `CellValue<'i>` stream from independent scan |
| **TOML** | `toml-rs`'s `de::from_slice::<T>()` Owned/Borrowed typed plane; the typed product is `Document<'i>` with table/array/value typed views | a second TOML parser (e.g., `taplo` AST walker) producing the same typed values from an independent parse |
| **BBNF-self** | the grammar AST itself is a typed-product plane: `Grammar<'i>` with `Vec<Rule<'i>>` and per-rule typed payloads | the prior BBNF lexer + parser pair produces the same AST from an independent scanner — already exercised in the bootstrap loop |

The corresponding `*_real_typed_struct` row ids would be
`css_l4_<corpus>/real_typed_struct/main`,
`sheets_<corpus>/real_typed_struct/main`, etc. Each grammar's row-table
wave authors its own seven owner-file slice set, its own falsifiability
gates against that grammar's strict-anchor comparator (e.g.,
`lightningcss_strict` for CSS L4 in place of `sonic_rs_strict` for JSON),
and its own Lock 14 schema identity under
`sk-v{N>9}-<grammar>-real-typed-w{n}`. The methodology body — slice
shape, capture envelope, run-id provenance, telemetry-row schema,
falsifiability gate structure — transposes verbatim.

Per S-P1 V3-C structural breakdown (`skv9-p1-v3-D-structural-breakdown.md`):
the typed plane's leverage is highest where the tiny-key scanner
dominates parse-only and the typed projection is a strict subset of the
parsed work. Apache (q_frac 0.999, +56% tiny-key share) and CITM (q_frac
0.630, +24% tiny-key share, low q/B 0.0154) sit at opposite ends of the
spectrum and both win on parse_only — they are the strongest typed-row
candidates outside the existing four GO rows. github_events (40.5%
tiny-key share, -33% parse-only delta) is the next-best candidate
because the gap is on the parse side; a typed projection that elides
non-required keys would skip much of that loss.

## §6 — Pre-block risk + REDRESS citations

**Does admitting Apache/CITM measured typed rows reopen REDRESS 60-72
retained-parse routes?** No, provided the wave stays inside its declared
owner-file set.

REDRESS 60-72 (`REDRESS.md:1346-2059`) collectively reject retained
parsing routes: trusted-string boundary collapse, long-string trusted
scan, delayed-wide retained trusted string scan, Unicode-escape run
validator, object next-key carry, direct source-hook field-layout
materializer, parser-owned decoded scratch, byte-output unescape
materialization, DirectBuild semantic string field facts, the first
`real_typed_struct` implementation as a SOTA close, hand-authored typed
sinks, and global cap-16 (admitted only for generated retained
`OffsetTape`). The Apache + CITM admission methodology:
- adds no retained parsing surface (uses the existing generated DirectBuild
  typed path, untouched since W2 commit `12aff1e4`);
- adds no semantic string field facts;
- adds no hand-authored typed sink;
- adds no parser-owned scratch or sidecar;
- does not extend cap-16 beyond its admitted scope (REDRESS 72);
- does not reopen the rejected first typed SOTA-close route (REDRESS 70)
  because admission criterion is *measured-row throughput*, not "SOTA
  close" — the four existing typed GO rows already demonstrate the admitted
  shape, and Apache + CITM extend it without claiming the close state itself.

REDRESS 71 (`REDRESS.md:1944-1993`) admits *generated typed DirectBuild
from a host/API output schema*, which is precisely the shape this wave
uses. REDRESS 71 is the admitted route, not a pre-block.

**Pre-blocks the wave must honour (binding):**
- REDRESS 91: Canada remains rejected. The wave admits Apache + CITM only
  and does not weaken the canada checksum-mismatch route-out.
- REDRESS 92 (`REDRESS.md:2661-2690`): retained class/event grammar +
  `ValueRef` cursor proof is required before any structural-heavy parse
  wave. This typed row-table wave is *not* a structural-heavy parse wave
  — it is row-table admission only, no new structural surface. REDRESS 92
  is not reopened.
- REDRESS 93 (`REDRESS.md:2692-2729`): direct guard scalar-parent fold
  is rejected. Direct rows in this wave (Apache `N-direct`, CITM `A`)
  remain at their SK-V9-open verdicts; the wave does not touch direct
  admission and does not reopen REDRESS 93.
- HANDOFF §5 pre-blocked routes (`HANDOFF.md:104-130`): "Apache/CITM
  measured-row overclaim from REDRESS 91" is the *first* item; the wave
  closes it by satisfying the admission criterion (fresh measured
  evidence, exact owner paths, same-wave consumer, no-regression gate,
  REDRESS citation, challenge acceptance) rather than reopening it.

**Substrate union (Lock 1)** is unaffected: the wave produces no new
substrate, no sidecar, no parallel tape, no parser-owned cursor. The
typed parser already shares the offset-tape + structural-projection
substrate with the parse_only and direct_to_struct rows.

**Lock 14** is preserved by adding a new scoped allowance for
`sk-v9-real-typed-w{n}` covering the seven owner files in §4.1. The
schema identity `sk-v8-real-typed-w2` remains frozen; the W{n}
allowance carries the row-table promotion only.

## §7 — Sources

External / internal documents cited:

1. `skinny/REDRESS.md` Item 91 (lines 2620-2659) — W2 typed product-plane
   source/product parity admission.
2. `skinny/REDRESS.md` Items 60-72 (lines 1346-2059) — retained-parse
   pre-blocks; Item 71 = admitted host/API typed DirectBuild route.
3. `skinny/REDRESS.md` Item 92 (lines 2661-2690) — W3 structural-heavy
   parse pre-block.
4. `skinny/REDRESS.md` Item 93 (lines 2692-2729) — W4 direct guard
   scalar-parent-fold rejection.
5. `skinny/RESULTS.md` — 21 SK-V9-open measured rows; 4 `real_typed_struct
   A / GO` rows (twitter, update_center, mesh, marine_ik); 14 `N-direct /
   NO-GO`; 3 `direct_to_struct A / GO`.
6. `restart/skinny/tranches/sk-v9/HANDOFF.md` §3 row 1 — candidate
   boundary for Apache/CITM typed row-table wave; LOC budget 300, hard
   cap ≤90 min.
7. `restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md`
   lines 59-94 — REDRESS 91 detail + SK-V9 framing for admission.
8. `restart/skinny/tranches/sk-v8/research/wave-2-hardening/V1/CH1.md`
   §§1-5 — original blocking governance defects.
9. `restart/skinny/tranches/sk-v8/research/wave-2-hardening/V1/CH4.md`
   — Track-2/oracle wording challenge.
10. `restart/skinny/tranches/sk-v8/research/wave-2-hardening/V1/CH5.md`
    — final hardening acceptance contingent on REDRESS 91 retention.
11. `restart/skinny/tranches/sk-v8/research/wave-2-hardening/V2/HARDENING-W2-V2-CONSOLIDATED.md`
    — ACCEPT 6/6 for the source slice; Lock 14 W2 allowance scope; serde
    as Track 2/oracle wording.
12. `restart/skinny/tranches/sk-v8/research/wave-2-hardening/V3/HARDENING-W2-V3-CONSOLIDATED.md`
    — gate fold: source-fixture typed metadata must not imply
    unadmitted Criterion rows.
13. `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
    — S-P1 V6 convergence; PMU table at `/tmp/skv9-xctrace-v3/pmu_rows.tsv`.
14. `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-C-hot-leaf-attribution.md`
    §§154-191 — Apache + CITM hot-leaf breakdowns; tiny-key scanner share.
15. `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-A-xctrace-cpu-counters.md`
    §§24-46 — probe methodology; typed track not measured.
16. `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-D-structural-breakdown.md`
    lines 77-90, 162-274 — Apache + CITM structural placement.
17. `/tmp/skv9-xctrace-v3/pmu_rows.tsv` — 34 rows of cycles, instructions,
    CPI, cycles/byte; parse-only Track 1 + Track 2 only; rows 4-5 (CITM),
    8-9 (Apache).
18. `restart/prompts/skinny/PASS-2-RESEARCH.md` — S-P2 scope contract +
    six-lens CHALLENGE wave (CH1–CH6).
19. `skinny/crates/bbnf-bench/src/real_typed_struct.rs` lines 1-323 —
    typed structs, fixture map, generated/serde/sonic engines, parity
    asserter, checksum.
20. `skinny/crates/bbnf-bench/src/generated_real_typed.rs` — generated
    typed DirectBuild parsers (regen'd from `real_typed_schema.rs`).
21. `skinny/xtask/src/real_typed_schema.rs` lines 7-99 — DirectSchemaSet
    `sk-v8-real-typed-w2`; Apache + CITM root entries; CITM map-entries
    codegen primitive.
22. `skinny/crates/bbnf-bench/src/gate.rs` lines 56-204 —
    `DIRECT_PROJECTION_SONIC_SLACK = 1.10`; `validate_strict_admission`;
    `validate_schema`.
23. `skinny/crates/bbnf-bench/src/bin/gate.rs` lines 1199-1201,
    1270-1419, 1820-1831, 1826-1831 — `w0_real_typed_metadata_expected`,
    `required_metadata_specs(real_typed_expected)`, and the regression
    test that enforces the measured-baseline-not-source-fixture rule.
24. `skinny/crates/bbnf-bench/src/report.rs` lines 280-410, 518-695,
    709-958, 977-1033 — telemetry-row schema-v3 validation; W0 run-id
    validator; `SK_V8_OPEN_BASELINE`; profile-artifact path derivation;
    outcome whitelist; admission-boundary helper.
25. `restart/locks/LOCKS.md` Lock 1 + Lock 14 — substrate union;
    frozen-root parent-diff scope.

## §0 — V2 fold footer

Cycle: V2. Date: 2026-05-18. This artefact carries the V2 fold of the
S-P2 V1 CHALLENGE dispositions against P2-C. P2-C entered V2 as the
cleanest cohort report (CH1 100%, CH3 100%, CH4 3 ACCEPT + 4 REVISE,
CH6 100%); the folds are surgical and the load-bearing claims of §1
(REDRESS 91 differential), §3 (per-row specifics), §4 (wave shape),
§6 (pre-block citations), and §7 (sources) are preserved verbatim.

Folds applied, per `HARDENING-S-P2-V1-CONSOLIDATED.md` F4 + F5:

- **F4 — per-slice LOC break-out (CH4 §2.3 rows C.1-C.3, C.5; CH4 §4.1).**
  New §2.0 decomposes the aggregate 300 LOC / ≤90 min HANDOFF envelope
  into five disjoint artefact slices — (a) baseline manifest edits,
  (b) gate test flips, (c) RESULTS table promotions, (d) REDRESS entry,
  (e) HANDOFF + LOCKS reflections — each with an LOC sub-budget, a
  minute sub-cap, a same-wave consumer, and a one-sentence revert
  protocol. §4.1's owner-files table is restated against the same
  five-slice clustering with per-file LOC + minute sub-caps. The
  sub-budgets sum to ~255 hand-LOC + run-id refresh ≈ 300 total at
  ~85 min, within the HANDOFF envelope with a ~5 min margin for the
  §2.9 verification matrix.

- **F5 — cross-grammar transposition prose (CH2 §2.3 row C.4).** New
  §5.1 reframes the §5 closing generalisation from a JSON-corpus
  enumeration into the GENERIC pattern: any grammar whose host-API
  schema admits a fact-typed Track 1 product plane can use the
  methodology. CSS L4 (lightningcss `Visitor`), Sheets (cell-by-cell
  `CellValue<'i>` typed access), TOML (`toml-rs` typed plane), and
  BBNF-self (the grammar AST as typed product) are named as
  cross-grammar examples — illustrative, not in scope for the SK-V9
  skinny iteration.

- **F5 — Track-2 oracle JSON-specificity acknowledgment (CH2 §2.3
  row C.5).** §2.7 gains a paragraph stating that "Track 2 oracle is
  structurally independent" is a JSON-internal definition satisfied by
  `serde_json`'s value-stream fold; cross-grammar waves cannot inherit
  the JSON oracle and must nominate a per-grammar independent typed
  parser. The structural-independence *property* (shares no scanner,
  parser, allocator, or codegen template with Track 1) is the
  grammar-neutral invariant; the oracle's *shape* is grammar-dependent
  (lightningcss visit, reference CSV cell parser, taplo AST walk).

No architectural reshape; no change to §1's REDRESS 91 posture, §3's
Apache/CITM per-row throughput tables, §4.2/§4.3's dispatch sequence and
falsifiability gates, §6's pre-block citations, or §7's source list.
