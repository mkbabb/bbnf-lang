# SK-V14 P3-D: Telemetry Schema Binding

Pass: S-P3 Synthesis-Plan. Cycle: V1.
Date: 2026-05-23.
Scope: Bind the `skinny/RESULTS.md` column schema for SK-V14 — carry-forward SK-V8 SPEC §0.4 + schema-v3 surface, name SK-V14 ADDITIONS, specify `xtask gate-json` rejection rules per `[typed-materialization-invariant]`.
Output: this file.
Pass Alpha goalset: SYNTHESIS §0.1 R10 close-condition (every JSON cell + every CSS L4 feature ADMITs strict-vs-strict on the same plane / corpus / equality semantics) + R1 (comparator rebind) + R2 (per-iter equality oracle).
Candidate pool: research/p2/ post-CHALLENGE survivors (LOCKED at S-P2 §3Z, commit `4db55d2841158f1bd85e4ac4430a9ffb31c68d0d`).

## §1 — Synthesis

The SK-V14 telemetry schema is the **gate contract** — every emitted column must be consumed by `cargo xtask gate-json` in the same wave it lands; an emitted field not consumed by the gate is a producer-only artefact and FAILS the wave per `PASS-3-SYNTHESIS-PLAN.md:240-244` (§8.2 telemetry-binding load-bearing) + `[typed-materialization-invariant]`.

**Carry-forward foundation.** SK-V8 SPEC §0.4 (`restart/skinny/tranches/sk-v8/SPEC.md:103-146`) declares 27 required telemetry fields layered atop the visible 26-column `skinny/RESULTS.md` surface (header at `skinny/RESULTS.md:3`). SK-V8 §0.4 explicitly permits the schema-v3 carrier surface ("rendered as columns, a gate-consumed manifest, or a gate-consumed JSON payload") — SK-V14 inherits both rendering modes verbatim. The current `TelemetryRow` / `SkV8Telemetry` Rust binding lives at `skinny/crates/bbnf-bench/src/report.rs:34-97`; SK-V14 extends the struct, it does not rewrite it.

**SK-V14 ADDITIONS** (per `restart/skinny/tranches/sk-v14/SYNTHESIS.md:232-258` §2 telemetry binding table, verified verbatim against the orchestrator-prompt R1+R2 acceptance criteria at `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md:98-108`):

1. **`comparator_plane`** — RE-BOUND with NEW SK-V14 R1 rejection semantics. The column name already exists in SK-V8 §0.4 (line 117) as a per-comparator sub-field; SK-V14 R1 lifts it to a per-row REQUIRED column carrying the strict-mode comparator identity per plane (`parse_only` → `sonic_rs::Skipper`; `direct` → `sonic_rs strict struct deser`; `typed` → per-corpus typed struct deser). The new rejection rule (NOT present in SK-V8 §0.4): the gate REJECTS any row whose comparator does work asymmetric to Track 1 (e.g. `sonic_rs::from_slice::<Value>` eager DOM materialisation when the row claims strict struct deser).
2. **`per_iter_equality`** — NEW (R2). Boolean column emitted per criterion iteration; PASS only if equality verified inside the timing region per `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md:105-108`. Replaces startup-only checksum parity.
3. **`audit_overlay_verdict`** — NEW (audit overlay). Enum (AUDIT-FALSIFIED / AUDIT-SUSTAINED / AUDIT-PENDING) per row; AUDIT-FALSIFIED rows cite the validation-pack §reference that falsified per SYNTHESIS §4 (`restart/skinny/tranches/sk-v14/SYNTHESIS.md:323-325`).
4. **`track2_entry_point`** — NEW (CH5 hidden-coupling). Symbol path of the Track 2 oracle entry-point; the gate rejects any row where the Track 1 and Track 2 entry-point symbol paths share a common ancestor in `runtime::tape::` beyond the public `Tape` / `OffsetFlags` types per SYNTHESIS §2 line 240.

**Total bound surface:** 27 SK-V8 §0.4 carry-forward fields + 3 truly-NEW SK-V14 columns + 1 re-bound (`comparator_plane`, lifted from comparator-sub-field to per-row column with new asymmetric-work rejection rule). 31 unique column slots. The visible `skinny/RESULTS.md` 26-column table surface remains; SK-V14 additions render either as new columns OR as the schema-v3 gate-consumed JSON payload — both modes are gate-equivalent per SK-V8 SPEC §0.4 line 105-108.

**Bench-row floor.** JSON rows cover 17 corpora × 3 planes = 51 cells; CSS L4 rows cover 24 non-OUT_OF_SCOPE features per `restart/skinny/tranches/sk-v14/SYNTHESIS.md:234`. Every cell carries the full 31-column schema; a missing required column is a wave-blocking gate failure.

## §2 — Deliverable

### §2.1 — Carry-forward columns (SK-V8 SPEC §0.4 verbatim; 27 entries)

For each: NAME | SEMANTICS | POPULATION SOURCE | GATE CONSUMPTION | REJECTION RULE.

1. **`row_id`** | Canonical row identity (`{domain}/{corpus}/{workload}/main`). | `bbnf-bench` harness `report.rs:77-78` (corpus/workload composition). | REQUIRED. | Empty → reject; collision across rows → reject (`gate.rs` row-key dedupe).
2. **`grammar_id`** | Grammar family (`json` / `css_l4` / `bbnf_self` / …). | `bbnf-bench` harness — emitted by the per-grammar entry point. | REQUIRED. | Empty → reject; not in registered grammar set (`workspace.metadata.bbnf.grammars`) → reject.
3. **`domain`** | Coarse domain bucket (e.g. `json` / `css`). | `bbnf-bench` harness `report.rs:51`. | REQUIRED. | Empty → reject.
4. **`comparator_id`** | Identifier of the comparator implementation under attribution. | `bbnf-bench` `ComparatorSet` emission (`report.rs:36-44`). | REQUIRED for every comparator entry. | Empty → reject; absent from comparator registry → reject.
5. **`comparator_plane`** | (SEE §2.2 #1 — RE-BOUND with new SK-V14 semantics.) | (See §2.2.) | (See §2.2.) | (See §2.2.)
6. **`comparator_strictness`** | `strict` / `permissive` / `lossy`. | `bbnf-bench` `SkV8ComparatorEvidence::comparator_strictness` (`report.rs:39`). | REQUIRED per comparator. | Strictness mismatch with row claim → reject per SK-V8 SPEC §0.4 line 144.
7. **`comparator_freshness`** | `same-run-native` / `historical:sk-vN-sidecar-profile` / `absent:…`. | `bbnf-bench` `SkV8ComparatorEvidence::comparator_freshness` (`report.rs:40`). | REQUIRED per comparator. | Stale sidecar (freshness ≠ `same-run-native` on the strict anchor) → reject per SK-V8 SPEC §0.4 line 144.
8. **`measured_validation_path`** | Symbol/path of the validation point measured. | Harness emission (`report.rs:52`). | REQUIRED. | Empty → reject.
9. **`profile_artifact`** | Criterion slope-profile / samply trace pointer. | Harness emission (`report.rs:53`). | REQUIRED. | Empty → reject; non-existent path → reject (post-CH7 LAC-1E-12 executable-verification mandate).
10. **`sample_cost`** | Cycles per byte (or equivalent sample cost). | Harness emission (`report.rs:54`). | REQUIRED. | Empty → reject; non-numeric → reject.
11. **`sample_count`** | Criterion sample count (u64). | Harness emission (`report.rs:55`). | REQUIRED. | < statistical floor (W0-locked threshold) → reject.
12. **`build_flags`** | `profile=bench;rustflags=…;target_cpu=…`. | Harness emission (`report.rs:56`). | REQUIRED. | Missing `target-cpu=native` on aarch64-pin row → reject.
13. **`host_triple`** | `aarch64-apple-darwin;arch=…;cpu=Apple M5 Max`. | Harness emission (`report.rs:57`). | REQUIRED. | Mismatch with user-pinned aarch64 / Apple M5 Max → reject per dispatch §2 line 67.
14. **`feature_mask`** | `arch=…;os=…;simd=…;target_cpu=…`. | Harness emission (`report.rs:58`). | REQUIRED. | Empty → reject.
15. **`costfacts_rule_id`** | CostFacts rule attribution per W1. | `codegen::cost_facts_from_source` (`xtask/src/main.rs:331`). | REQUIRED post-W1. | Pre-W1 row → may carry `none:pre-W1`; W1+ row with empty value → reject per SK-V8 §0.4 line 144 ("missing W1 CostFacts").
16. **`costfacts_chosen_shape`** | The CSP-selected shape (e.g. `borrowed_view_over_offset_tape` / `typed_direct_projection`). | `codegen::cost_facts_from_source` → CSP decision driver. | REQUIRED post-W1. | Empty post-W1 → reject.
17. **`costfacts_rejected_alternative_ids`** | List of CSP-rejected shape ids. | CSP decision driver emission. | REQUIRED post-W1 (may be empty list, never absent). | Field absent → reject; non-list → reject.
18. **`redress_entry`** | `REDRESS-{N}` pointer into `skinny/REDRESS.md`. | Harness emission per wave-redress commit. | REQUIRED. | Empty → reject; REDRESS id not present in `skinny/REDRESS.md` → reject (post-redress executable verification per dispatch §2 line 66).
19. **`wave_id`** | `SK-V{N}-W{n}` or `SK-V{N}-W{n}.{sub}`. | Harness emission. | REQUIRED. | Empty → reject; mis-matched against current SK-V14 wave manifest → reject.
20. **`run_id`** | Per-run identifier (e.g. `parse-row-added` / `typed-row-added` / `direct-reclaimed`). | Harness emission (`report.rs:64`). | REQUIRED. | Empty → reject.
21. **`sidecar_freshness`** | `same-run-native` / `historical:sk-vN-sidecar-profile` / `absent:…`. | Harness emission (`report.rs:41`). | REQUIRED. | Stale sidecar on SOTA anchor → reject per SK-V8 §0.4 line 144.
22. **`sk_v14_open_delta`** | Δ vs the W0-locked `SK-V14-open` baseline; was `sk_v9_open_delta` in current HEAD struct (`report.rs:65`). | Harness emission post-W0 baseline capture. | REQUIRED post-W0. | Empty post-W0 → reject; |Δ| > 1.0% on the W0-rendered maintain row without redress citation → reject per `restart/skinny/tranches/sk-v8/SPEC.md:160` ("keep every throughput cell within +/-1.0% of the captured seed").
23. **`substrate_surface`** | E.g. `borrowed_view_over_offset_tape` / `sink_only_digest` / `typed_direct_projection`. | Harness emission (`report.rs:66`). | REQUIRED. | Empty → reject; new substrate surface not declared in Lock-1 triad → reject per SYNTHESIS §4 line 317-319.
24. **`structural_projection_status`** | `discarded_after_capacity` / `n/a` / `independent_verified` / …. | Harness emission (`report.rs:67`). | REQUIRED. | Empty → reject; W3 "side substrate" leak → reject per SK-V8 §0.4 line 145.
25. **`substrate_cardinality`** | `one` / `zero_or_inert` / …. | Harness emission (`report.rs:68`). | REQUIRED. | Empty → reject; substrate union breach (Lock 1 — two live substrates on the same row) → reject per SYNTHESIS §0.4 P-5.
26. **`same_wave_consumer_class`** | E.g. `generated_json_parse_only_contract` / `gate_json_typed_contract` / …. | Harness emission (`report.rs:69`). | REQUIRED per `[no-deferrals]`. | Empty → reject; class not present in the same wave's commit set → reject (orphan-kernel pre-block per SYNTHESIS §0.4 P-1).
27. **`track2_independence_status`** | `independent_verified` / `dependent_on_track1` / …. | Harness emission (`report.rs:70`). | REQUIRED. | Empty → reject; `dependent_on_track1` on an admit row → reject (Track 1 ≡ Track 2 dishonesty per `PASS-3-SYNTHESIS-PLAN.md:135-137` CH5).

### §2.2 — SK-V14 ADDITIONS (4 entries, per SYNTHESIS §2 verified verbatim)

For each: NAME | SEMANTICS | POPULATION SOURCE | GATE CONSUMPTION | REJECTION RULE.

1. **`comparator_plane`** — RE-BOUND per-row + R1 asymmetric-work rejection.
   - SEMANTICS: Names the strict-mode comparator bound per plane for the row: `parse_only` → `sonic_rs::Skipper` (structural-skip-only); `direct` → sonic_rs strict struct deserialization per corpus; `typed` → per-corpus typed struct deserialization. The column is per-row, not per-comparator-sub-field as in SK-V8 §0.4.
   - POPULATION SOURCE: `bbnf-bench` harness — extended `SkV8Telemetry` struct field (`skinny/crates/bbnf-bench/src/report.rs:48-73`); written from C-2 comparator-rebind landing per SYNTHESIS §3 candidate row.
   - GATE CONSUMPTION: REQUIRED for every row (JSON 51 cells + CSS 24 features) per `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md:98-103` R1 acceptance.
   - REJECTION RULE: Empty → reject. Comparator does work asymmetric to Track 1 (e.g. row claims `direct` strict struct deser but `comparator_plane` resolves to `sonic_rs::from_slice::<Value>` eager DOM materialisation) → reject. Mis-bound comparator (plane-vs-comparator-id mismatch against the C-2 registered table) → reject. Per R1 verbatim: "No row admits until its plane's comparator is strict-vs-strict."

2. **`per_iter_equality`** — NEW (R2 per-iter equality oracle).
   - SEMANTICS: Boolean per criterion iteration: TRUE if equality between Track 1 output and the strict-mode `comparator_plane` reference was verified inside the timing region for that iteration; FALSE if any iteration failed equality. Column carries the per-iteration aggregate (e.g. `pass_all_iters=true; iter_count=100; mismatch_count=0`).
   - POPULATION SOURCE: `bbnf-bench` harness, equality assertion call-site bound inside the criterion `iter_batched` closure (C-2 deliverable). Quote the harness line per `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md:106-107`.
   - GATE CONSUMPTION: REQUIRED for every row.
   - REJECTION RULE: Empty → reject (`xtask gate-json` rejects any row whose equality column is empty per `restart/skinny/tranches/sk-v14/SYNTHESIS.md:272`, C-2 falsifiability gate). Startup-only checksum value (lacking per-iter aggregate) → reject. `pass_all_iters=false` on any row marked `A/GO` → reject per R2 + R10 ("Cells previously HOLDING under wrong oracle hold again under per-iter oracle OR are reverted").

3. **`audit_overlay_verdict`** — NEW (audit overlay gate enforcement).
   - SEMANTICS: Enum value per row — `AUDIT-FALSIFIED` / `AUDIT-SUSTAINED` / `AUDIT-PENDING`. `AUDIT-FALSIFIED` rows MUST cite the validation-pack §reference that falsified them (e.g. `v6 §1 rows 3-4`, `v2 §1-4`, `v1 §1-6`).
   - POPULATION SOURCE: `bbnf-bench` harness extension + the S-P0 SYNTHESIS-AUDIT-OVERFIT.md prune-list cross-reference; written from C-5 PRUNE-1 + PRUNE-2 redress commits per SYNTHESIS §3.
   - GATE CONSUMPTION: REQUIRED for every row per `restart/skinny/tranches/sk-v14/SYNTHESIS.md:323`.
   - REJECTION RULE: Empty → reject. `AUDIT-FALSIFIED` with empty validation-pack §reference → reject. `AUDIT-FALSIFIED` row claiming `A/GO` outcome without fresh material differential evidence (cited per REDRESS) → reject per SYNTHESIS §4 line 324-325.

4. **`track2_entry_point`** — NEW (CH5 hidden-coupling).
   - SEMANTICS: Symbol path of the Track 2 oracle entry-point (e.g. `bbnf_runtime::json::typed::parse_typed_entry`). Distinct from the Track 1 generated entry point.
   - POPULATION SOURCE: `bbnf-bench` harness; resolved at row emission time by introspecting the Track 2 closure binding.
   - GATE CONSUMPTION: REQUIRED for every row per SYNTHESIS §2 line 240.
   - REJECTION RULE: Empty → reject. Track 1 and Track 2 entry-point symbol paths share a common ancestor in `runtime::tape::` beyond the public `Tape` / `OffsetFlags` types → reject per SYNTHESIS §2 line 240 (CH5 hidden-coupling pre-block — Track 1 ≡ Track 2 dishonesty).

### §2.3 — Schema-v3 surface (carrier mode)

Per SK-V8 SPEC §0.4 line 105-108, the 27 carry-forward + 4 SK-V14 columns may render as:
- **Visible RESULTS.md table extension** (most legible for human review of `skinny/RESULTS.md`);
- **Gate-consumed manifest sidecar** (e.g. `skinny/skv14-comparator-rebind-report.json` consumed by a new `--skv14-comparator-rebind-report <path>` flag in `validate_gate_json_passthrough` at `skinny/xtask/src/main.rs:265-302`);
- **Gate-consumed JSON payload** inline per row.

All three modes are gate-equivalent; the choice is rendering-discipline, NOT contract-discipline. Every choice MUST be consumed by `cargo xtask gate-json` in the wave it lands (per `[typed-materialization-invariant]`); a producer-only render fails the wave.

### §2.4 — `xtask gate-json` extension surface

The current `validate_gate_json_passthrough` at `skinny/xtask/src/main.rs:265-302` recognises the SK-V8 + SK-V12 + SK-V13 report flags. SK-V14 W0 (baseline + telemetry lock per `[build-infra-first]`) MUST extend this allowlist with at minimum:
- `--skv14-comparator-rebind-report <path>` (consumes the per-row R1 `comparator_plane` evidence);
- `--skv14-per-iter-equality-report <path>` (consumes the per-iter R2 oracle column);
- `--skv14-audit-overlay-report <path>` (consumes the AUDIT-FALSIFIED / SUSTAINED / PENDING enum per row);
- `--skv14-track2-entry-point-report <path>` (consumes the Track 2 oracle symbol path per row).

`gate.rs` (`skinny/crates/bbnf-bench/src/bin/gate.rs`) ingests each report and applies the §2.1–§2.2 rejection rules. The exact `--check-results` snapshot at `skinny/xtask/src/main.rs:246-248` extends the W0-locked RESULTS.md snapshot to include the new columns; W0 close conditional on snapshot lock.

## §3 — Falsifiability binding

The schema is itself falsifiable. The gate's contract:

1. **Column-presence gate.** For every row in `skinny/RESULTS.md`, `cargo xtask gate-json --check-results` parses the row and asserts presence of all 31 column slots (27 carry-forward + 4 SK-V14 additions). Missing → gate exits non-zero with the column name + row_id. Wave does not commit.
2. **Asymmetric-comparator gate (R1).** For every row, the gate cross-checks `comparator_plane` value against the row's `output_plane` field (`report.rs:86`) and the C-2 strict-comparator registry. Asymmetric work (eager DOM materialisation when strict struct deser claimed) → reject. Test: a row with `output_plane=direct` but `comparator_plane=sonic_rs::from_slice::<Value>` MUST be rejected; if accepted, C-2 is mis-implemented.
3. **Per-iter equality gate (R2).** For every row marked `A/GO`, the gate parses `per_iter_equality` and asserts `pass_all_iters=true`. Any iteration mismatch on an admit row → reject. Test: artificially corrupt one iteration's equality and confirm the row falls back to `NO-GO`; if it does not, C-2 is mis-implemented.
4. **Audit-overlay gate.** For every row, the gate parses `audit_overlay_verdict`; AUDIT-FALSIFIED rows on `A/GO` outcome → reject unless fresh material differential cited per REDRESS. Test: an AUDIT-FALSIFIED row without REDRESS citation MUST reject.
5. **Hidden-coupling gate (CH5).** For every row, the gate parses `track2_entry_point` and computes the common-ancestor prefix vs Track 1 entry-point. Common ancestor in `runtime::tape::` beyond `Tape` / `OffsetFlags` → reject. Test: artificially point Track 2 at a private tape internal and confirm rejection.
6. **W0 baseline lock.** All 31 column slots populated on every existing row at W0 close. `SK-V14-open` baseline captures the schema-locked snapshot; |Δ| > 1.0% on any maintain row without REDRESS → reject.

Named corpus rows for schema-presence verification (W0):
- JSON: all 17 corpora × 3 planes = 51 cells per SYNTHESIS §0.2.
- CSS L4: all 24 non-OUT_OF_SCOPE features per SYNTHESIS §0.1.

Mbps threshold: SCHEMA gate is presence-based, not throughput-based; throughput thresholds bind per-wave under P3-C (falsifiability gates), not here.

## §4 — Pre-blocked routes

REDRESS entries the SK-V14 telemetry schema MUST NOT re-open:

- **REDRESS 28, 33** — startup-only checksum parity; C-2 R2 column kills this route by requiring per-iter equality inside the timing region.
- **REDRESS 50–55** — comparator plane mismatch (sonic_rs lossy as SOTA anchor; sonic_rs::from_slice::<Value> as comparator for direct/typed rows); C-2 R1 `comparator_plane` REQUIRED + asymmetric-work rejection rule kills this route.
- **REDRESS 60–72** — stale sidecar freshness on SOTA anchor; carry-forward §2.1 #7 + #21 (`comparator_freshness` + `sidecar_freshness`) REQUIRED + same-run-native enforcement on the strict anchor kills this route.
- **REDRESS 80, 82–84, 88, 89** — producer-only telemetry (emitted columns not consumed by gate); §1 `[typed-materialization-invariant]` discipline applied verbatim — every column in §2.1 + §2.2 must be consumed by `cargo xtask gate-json` in the same wave it lands.
- **REDRESS 96–98** — orphan kernel (primitive landed without same-wave consumer); §2.1 #26 `same_wave_consumer_class` REQUIRED + empty-rejection rule kills this route per `[no-deferrals]`.
- **REDRESS 119/120** — sidecar event vector / aux density table (parser-owned structural projection); §2.1 #24 + #25 `structural_projection_status` + `substrate_cardinality` REQUIRED + substrate-union enforcement (Lock 1) kills this route.
- **REDRESS 126** — Track 1 ≡ Track 2 dishonesty; §2.2 #4 `track2_entry_point` REQUIRED + common-ancestor rejection kills this route.

SYNTHESIS §0.4 pattern-level pre-blocks P-1 … P-7 the schema must honour:
- **P-1 orphan kernel** — §2.1 #26 (`same_wave_consumer_class`).
- **P-2 comparator weakness** — §2.2 #1 (`comparator_plane`).
- **P-3 startup-only oracle** — §2.2 #2 (`per_iter_equality`).
- **P-4 producer-only telemetry** — every column gate-consumed (§2.4).
- **P-5 substrate-union breach** — §2.1 #24 + #25 (Lock 1).
- **P-6 hidden coupling** — §2.2 #4 (`track2_entry_point`).
- **P-7 paper-close on future-phase promise** — §2.1 #18–#20 (`redress_entry` / `wave_id` / `run_id`) REQUIRED + present-in-same-wave commit set.

Forbidden schema mutations (the gate must reject if proposed):
- Adding any column that lacks a gate-consumption rule (producer-only artefact).
- Renaming `comparator_plane` to re-introduce the SK-V8 per-comparator-sub-field semantics (would silently re-open REDRESS 50–55).
- Permitting `per_iter_equality` to carry a startup-only value (would silently re-open REDRESS 28, 33).
- Permitting any column to be NULL/empty on a row marked `A/GO`.

## §5 — Sources

- `restart/skinny/tranches/sk-v14/research/p3/S-P3-DISPATCH-CONTEXT.md:1-89` — dispatch authority + 3 architectural sequencing constraints + S-P2 LOCKED candidate pool.
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:61` — P3-D scope row.
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:240-244` — §8.2 telemetry-binding load-bearing + `[typed-materialization-invariant]` discipline.
- `restart/skinny/tranches/sk-v8/SPEC.md:103-146` — §0.4 27-field carry-forward schema + schema-v3 carrier surface + the "missing required fields → reject" mandate.
- `restart/skinny/tranches/sk-v8/SPEC.md:148-189` — §0.5 opening row goalset (W0 ±1.0% maintain budget).
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md:225-261` — §2 SK-V14 telemetry binding table (verbatim source for the 4 SK-V14 additions).
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md:323-325` — §4 audit-overlay gate enforcement.
- `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md:98-108` — R1 (comparator rebind) + R2 (per-iter equality oracle) acceptance criteria.
- `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md:142-149` — R7 (JSON direct + typed re-admit) + R8 (parse_only distinct path) consumer constraint.
- `skinny/RESULTS.md:3` — current 26-column visible header surface; `skinny/RESULTS.md:58-185` — current schema-v3 carrier-payload rows.
- `skinny/crates/bbnf-bench/src/report.rs:34-97` — current `TelemetryRow` + `SkV8Telemetry` + `SkV8ComparatorEvidence` Rust binding; extension target for SK-V14 additions.
- `skinny/xtask/src/main.rs:242-302` — current `gate_json` + `validate_gate_json_passthrough`; extension target for the 4 new `--skv14-*-report` flags per §2.4.
- `skinny/crates/bbnf-bench/src/bin/gate.rs:1-5698` — gate ingestion + per-row rejection logic.
- `restart/locks/LOCKS.md:73-82` — Lock 1 substrate-union triad; binding for §2.1 #24 + #25 rejection rules.
- `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` — validation-pack §references cited by AUDIT-FALSIFIED rows per §2.2 #3.
