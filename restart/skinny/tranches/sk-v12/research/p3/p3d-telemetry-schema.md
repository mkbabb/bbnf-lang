# SK-V12 P3-D: Telemetry Schema Binding

Pass: S-P3 Synthesis-Plan. Cycle: PIN-V1.
Date: 2026-05-20.
Scope: bind the pin-aware SK-V12 telemetry schema and fail-closed gate rules for CSS L4 > lightningcss admission, JSON guard maintenance, Lock 14, Lock 16, union, ASM-gen, and aarch64 orphan disposition.
Output: this file.
Pass Alpha goalset: generated CSS L4 Track 1 must run strictly faster than `lightningcss_mbps + 1` on the same corpus, same output plane, same host, with strict equality and independent oracle/Track 2 evidence; JSON guards hold or record measured REDRESS demotions; Sheets and BBNF-self are fallback-only after a measured CSS L4 redress attempt.
Candidate pool: research/p2/ post-CHALLENGE survivors.

## §1 - Synthesis

P3-D is the telemetry contract for the pin-aware SK-V12 SPEC. It does not edit
bench, gate, source, generated runtime, or `skinny/RESULTS.md` code. Later wave
redress must implement the concrete validator changes in the same slice that
emits new fields.

The user pin changes the telemetry burden. A generated non-JSON placeholder is
not enough, a baseline-relative lift is not enough, and a prose Lock 14 claim is
not enough. A row admits only when the gate consumes a complete CSS L4 evidence
record:

- generated CSS L4 Track 1 source and runtime provenance;
- same-corpus, same-output-plane `lightningcss` comparator artifact and Mbps;
- strict output equality against an independent oracle or Track 2;
- grammar, input, generated source, and output checksums;
- run/build/host/feature/profile provenance;
- Lock 14 and Lock 16 status, where applicable;
- generated-size and O(N) budget evidence;
- JSON guard maintain or measured demotion evidence;
- union and ASM-gen attempt evidence when those routes are dispatched;
- carried aarch64 orphan disposition.

The inherited JSON report surface remains schema-v3 plus the rendered
`skinny/RESULTS.md` table. SK-V8's telemetry precedent remains binding:
required fields may be rendered as columns, a gate-consumed manifest, or a
gate-consumed JSON payload, but every emitted field must be consumed by
`gate-json` or the named companion gate in the same wave. Producer-only fields
fail closed.

The live seed surface is unchanged from SK-V11 close: `parse_only` is
diagnostic, JSON `direct_to_struct` and `real_typed_struct` are guard surfaces,
and there is no admitted CSS L4 row. Pin-aware S-P1 and S-P2 converged on this
same basis. The accepted S-P2 packet limits selectable SIMD candidates to
micro-proven, grammar-neutral, same-wave-consumed primitive families and keeps
`escape_mask_64` correctness as a prerequisite before any new SIMD admission.

## §2 - Deliverable

### §2.1 Inherited identifiers

SK-V12 keeps the inherited outcome enum:

```text
A C G I J K L M N-direct S
```

No wave may add an outcome identifier. `S`, `L`, and `N-direct` remain
non-admission outcomes. `parse_only` cannot satisfy the user pin.

SK-V12 also carries the schema-v3 JSON row identifiers already consumed by
`gate-json`:

```text
row_id grammar_id domain corpus workload outcome_id verdict strictness
parse_utf8 escape_complete flaw_probe output_plane track1_mbps track2_mbps
comparator_id comparator_plane comparator_strictness comparator_freshness
sidecar_freshness comparator_value_mbps comparator_source_artifact
measured_validation_path profile_artifact sample_cost sample_count build_flags
host_triple feature_mask costfacts_rule_id costfacts_chosen_shape
costfacts_rejected_alternative_ids redress_entry wave_id run_id
sk_v9_open_delta substrate_surface structural_projection_status
substrate_cardinality same_wave_consumer_class track2_independence_status
diagnostic_nonproducer_status comparator_set
```

JSON rows may keep the current rendered table shape as long as the manifest or
gate payload reconstructs the required identifiers. Missing required
identifiers, unsupported outcomes, stale run ids, comparator-plane mismatch,
oracle coupling, generic JSON policy leakage, or producer-only telemetry reject.

### §2.2 CSS L4 admission schema

The CSS L4 admission companion schema is:

```text
sk-v12-css-l4-sota-v1
```

If a wave chooses to render the CSS L4 row directly into `skinny/RESULTS.md`,
it must update every RESULTS consumer and `gate-json` in that same wave. If it
uses a companion report, the named companion gate is the same-wave consumer.
The legacy `sk-v12-nonjson-generated-v1` schema remains historical W0/W11
evidence only unless the same-wave gate explicitly upgrades it to the field set
below. It cannot admit the user-pin target without the CSS L4 and lightningcss
fields.

Required CSS L4 fields:

| Field | Required value or shape | Gate rule |
|---|---|---|
| `schema_id` | `sk-v12-css-l4-sota-v1` | Unknown or legacy-only schemas fail admission. |
| `row_id` | `css_l4/<corpus>/<workload>/main` | Stable join key; duplicates fail. |
| `grammar_id` | `css_l4` | `json`, `sheets`, and `bbnf_self` cannot satisfy CSS ADMIT. |
| `domain` | `css_l4_generated` | Generic non-JSON labels are insufficient for admission. |
| `corpus_id` | SPEC-named CSS L4 corpus id | Must match input, oracle, lightningcss, bench, and profile artifacts. |
| `workload` | generated direct or typed CSS fact-stream workload | `parse_only` fails. |
| `output_plane` | canonical CSS fact stream or SPEC-named equivalent | Must match Track 1, oracle/Track 2, and lightningcss. |
| `strictness` | `strict` | Permissive/lossy comparators are planning signals only. |
| `outcome_id` / `verdict` | inherited enum plus `GO`/`NO-GO` | No new outcome variants. |
| `generated_track1_source_path` | generated CSS parser/runtime source path | Hand-only witnesses fail. |
| `generated_runtime_path` | runtime module loaded by the benchmark | Must be CSS L4, not generated JSON or `sheets_witness`. |
| `generated_source_checksum` | checksum of generated files used for Track 1 | Must match build artifact. |
| `grammar_source_path` / `grammar_checksum` | CSS L4 grammar source and checksum | Must be gate-consumed. |
| `input_path` / `input_checksum` / `input_bytes` | fixture path, checksum, byte count | Must match all measured lanes. |
| `generated_config_checksum` | checksum of GrammarConfig or generated metadata | Required before CSS L4 emission is legal. |
| `track1_mbps` | finite measured Mbps | Admission requires `track1_mbps > lightningcss_mbps + 1`. Equality at `+1` fails. |
| `track1_artifact` | Criterion or equivalent artifact path | Must resolve and match row/run/build. |
| `oracle_or_track2_source_path` | independent oracle or Track 2 source | Must not call generated Track 1 or generated runtime internals. |
| `oracle_or_track2_mbps` | finite measured Mbps or justified N/A for non-admitting support rows | CSS ADMIT requires finite measured value. |
| `track2_independence_status` | `independent_verified` or structured failure | Self-attestation without source separation fails. |
| `strict_output_equality` | `pass` or structured failure | Any failure blocks admission. |
| `equality_artifact` | diff/digest/fact-stream equality artifact | Must bind Track 1, oracle/Track 2, and lightningcss output. |
| `lightningcss_command` | exact command or harness invocation | Missing comparator command fails. |
| `lightningcss_version` | version or build hash | Required for same-host comparator evidence. |
| `lightningcss_mbps` | finite measured Mbps | Admission floor is computed from this value only. |
| `lightningcss_artifact` | benchmark and output artifact path | Must resolve and match row/run/build. |
| `lightningcss_output_checksum` | comparator output checksum | Must equal canonical strict output. |
| `admission_floor_mbps` | `lightningcss_mbps + 1` | `ceil(baseline_mbps * 1.01)` is obsolete and fails. |
| `run_id` | stable SK-V12 run id | Same run across Track 1, oracle/Track 2, lightningcss, guards, and gate unless the gate marks failure. |
| `build_id` / `source_commit` | build provenance | Required for reproducibility. |
| `host_triple` / `cpu_model` | host provenance | Must identify Apple Silicon/aarch64 host. |
| `feature_mask` | ISA feature mask | Required for SIMD/ASM and comparator parity. |
| `build_flags` | includes `RUSTFLAGS="-C target-cpu=native"` when used | Required for row admission. |
| `sample_count` / `sample_cost` | positive count and cost tuple | Missing or zero sample evidence fails. |
| `benchmark_artifact_path` | full capture path | Must resolve under wave capture root or named artifact root. |
| `profile_artifact` | xctrace/samply/PMU profile path or no-source-touch proof | Behavior candidates require fresh profile evidence. |
| `pmu_tsv_path` | PMU TSV consumed by S-P3/wave gate | Missing PMU TSV fails intervention scoping. |
| `time_profile_artifact` | xctrace Time Profiler artifact/export | Required for fresh hot-leaf claims. |
| `samply_artifact` | samply artifact or structured absence | Artifact-only; xctrace remains self-time authority. |
| `wave_id` / `redress_entry` | wave and REDRESS id or `pending` | Failed waves must produce REDRESS evidence. |
| `gate_status` | gate-authored `pass`, `fail`, or `blocked` | Bench output alone cannot admit. |

### §2.3 Lock 14 fields

Every wave that touches generic crates, codegen, runtime templates, reports,
or gates must emit and consume Lock 14 evidence:

| Field | Required shape | Gate rule |
|---|---|---|
| `lock14_status` | `pass`, `fail`, or `not_applicable` | CSS L4 emission requires `pass`. |
| `grammar_config_trait_status` | `present:<path>` or failure | Required before CSS L4 generated output is legal. |
| `grammar_config_checksum` | checksum of generated/per-grammar config | Must match Track 1 build. |
| `lock14_leak_resolution` | seven leak ids mapped to fixed/untouched status | Missing entries fail W1 legality. |
| `generic_crate_scan_artifact` | scan artifact for grammar names/match arms/features | Missing scan fails generic-crate edits. |
| `generated_metadata_paths` | generated metadata/config files | Must be per-grammar, not generic JSON policy. |
| `json_policy_leak_status` | `none` or structured failure | Any generic JSON policy leakage fails. |

The seven leak classes from the value/API audit are structural alphabet, value
dispatch, string escape/quote policy, number policy, quoted-key/object-pair
policy, `OffsetFlags` meaning, and JSON sink callbacks. A wave may mark a leak
`untouched` only when the edited owner paths cannot affect CSS L4 emission and
the gate consumes the no-touch proof.

### §2.4 Lock 16, SIMD, and ASM-gen fields

Every SIMD/ASM primitive, including union-adjacent or ASM-gen routes reopened
by the user pin, must emit and consume:

| Field | Required shape | Gate rule |
|---|---|---|
| `lock16_status` | `pass`, `fail`, or `not_applicable` | SIMD/ASM row admission requires `pass`. |
| `primitive_id` | candidate primitive id from S-P2/P3 | Must trace to accepted S-P2 candidate or support route. |
| `scalar_reference_status` | `present:<path>` or failure | Required before SIMD/ASM admission. |
| `checkasm_or_parity_status` | pass/fail plus artifact | Missing parity fails. |
| `checkasm_artifact` | test output/report path | Must include corpus or edge-case parity for admitted primitive. |
| `microbench_status` | pass/fail plus artifact | No kernel/substrate route reaches wave close without a microbench. |
| `same_wave_consumer_class` | named hot-path consumer | `harness_only` and `producer_only` fail. |
| `isa_feature_required` | NEON/TBL/TBX/PMULL/CSSC/SHA3/UDOT/etc. | Must be aarch64/Apple Silicon only; x86 fails. |
| `dispatch_fallback_status` | scalar fallback and feature guard status | Required for feature-gated primitives. |
| `escape_mask_64_status` | verified fixed or not-applicable with reason | Any new SIMD admission before the bug is verified/resolved fails. |
| `escape_mask_64_falsifier` | seed/status when relevant | Must cite `0xCAFEF00DBAADF00D` resolution for string/escape primitives. |
| `asm_visibility_artifact` | disasm/feature proof when ASM-gen is claimed | Required for PMULL/CSSC/SHA3/UDOT claims when compiler lowering matters. |

### §2.5 Generated-size and O(N) fields

CSS L4 codegen can fail by size even when correctness is green. Required size
fields:

| Field | Required shape | Gate rule |
|---|---|---|
| `generated_loc` | line count of generated CSS runtime/source | Missing count fails generated waves. |
| `generated_module_bytes` | byte size of generated module(s) | Required for generated-size budget. |
| `grammar_rule_count` | CSS grammar/rule count used by generator | Required for O(N) accounting. |
| `generated_loc_per_rule` | computed ratio or equivalent growth metric | Required for regression tracing. |
| `regen_command` | exact command that produced generated output | Missing command fails. |
| `generated_diff_artifact` | diff or checksum set | Required when generated output changes. |
| `on_budget_status` | pass/fail with SPEC budget | Overflow blocks the wave until traced. |
| `o_n_guard_status` | pass/fail plus method | Superlinear growth fails unless REDRESS records a measured block. |

### §2.6 JSON guards and demotions

JSON guards are second priority after CSS L4, but they remain binding. A wave
that can affect JSON-producing paths must emit either a no-touch proof or a
measured guard table:

| Field | Required shape | Gate rule |
|---|---|---|
| `json_guard_state` | `not_refreshed:no_touch_proven` or `refreshed` | Missing state fails. |
| `json_no_touch_artifact` | diff/source path proof | Required when guards are not refreshed. |
| `json_guard_run_id` | run id for refreshed guards | Must match wave provenance. |
| `json_guard_rows` | all 4 direct + 7 typed guard rows with Track 1/Track 2/floor/outcome | Partial guard tables fail. |
| `json_guard_floor_source` | SPEC or P3-C floor source | Required for maintain/lift decisions. |
| `json_guard_demotions` | row -> REDRESS id + measurement | A guard miss without REDRESS demotion fails. |
| `parse_only_status` | diagnostic-only | Any parse-only SOTA claim fails. |

The seed direct guards are `citm_catalog`, `apache_builds`, `marine_ik`, and
`unicode_basic`. The seed typed guards are `twitter`, `citm_catalog`,
`apache_builds`, `github_events`, `update_center`, `mesh`, and `marine_ik`.

### §2.7 Union and ASM-gen attempt fields

The user pin reopens union and ASM-gen categories at the category level; it
does not make historical implementations admissible. Any new attempt must
consume these fields:

| Field | Union route rule | ASM-gen route rule |
|---|---|---|
| `route_category` | `union_substrate` | `asm_gen` |
| `historical_redress_citations` | Includes REDRESS 96/97/98 when adjacent | Includes REDRESS 88/89/90 when adjacent |
| `material_differential` | Names how this differs from class-column, streaming cursor, and class-lane variants | Names how this differs from PMULL default, CSSC bulk, or canary-hardening routes |
| `fresh_profile_artifact` | CSS L4 or JSON guard hot leaf profile naming the route | CSS L4 or JSON guard hot leaf profile naming the primitive |
| `microbench_artifact` | Required before wave scoping | Required before wave scoping |
| `parity_or_equality_artifact` | Same-tape/equality proof | Scalar/checkasm/equality proof |
| `consumer_path` | Existing tape/fact-stream/direct consumer, same wave | Same-wave generated CSS or guard-row consumer |
| `substrate_cardinality` | Must remain `one` | Must not create side substrate |
| `public_api_delta` | No new public substrate API unless SPEC explicitly authorizes under pin | No x86 or orphan API |
| `attempt_status` | admitted/rejected/blocked with REDRESS id | admitted/rejected/blocked with REDRESS id |

### §2.8 Aarch64 orphan disposition

Close requires zero production aarch64 orphans. The gate must carry this table
on close waves and on any SIMD/ASM wave:

```text
bitmap_prefix_xor_64
bitmap_next_set_bit
bulk_emit_positions_64
byte_context
cache_hints
```

Each row needs `orphan_status` (`consumed`, `removed`, `inventory_demoted`, or
`open`), `consumer_path` or demotion/removal artifact, `lock16_status`, and
`redress_entry`. `open` is legal during intermediate waves but fails ADMIT and
FIXPOINT close.

## §3 - Falsifiability binding

Telemetry is falsifiable only when a gate consumes it. The SK-V12 gates must
reject:

1. CSS L4 admission without `grammar_id=css_l4`.
2. CSS L4 admission using `ceil(baseline_mbps * 1.01)` or any baseline-relative
   floor instead of `track1_mbps > lightningcss_mbps + 1`.
3. Missing same-plane lightningcss command, artifact, version, output checksum,
   or Mbps.
4. Missing strict equality, missing equality artifact, or equality that ignores
   the canonical CSS fact stream.
5. Oracle/Track 2 coupling to generated Track 1, generated runtime internals,
   or generated SinkOnly helpers.
6. Missing grammar/input/generated checksums or stale generated provenance.
7. Generic-crate JSON policy leakage or unresolved GrammarConfig legality.
8. New SIMD admission before `escape_mask_64` is verified and resolved.
9. SIMD/ASM primitive rows without scalar reference, checkasm/parity,
   microbench, feature guard, and same-wave consumer.
10. Union/ASM-gen attempts without historical REDRESS citations and material
    differentials.
11. JSON guard refresh with partial guard rows, or a guard miss without REDRESS
    demotion.
12. Open production aarch64 orphans at close.
13. Producer-only telemetry: any emitted field not consumed by `gate-json` or
    the named companion gate in the same wave.

The CSS L4 ADMIT gate passes only if all admission fields are present and:

```text
grammar_id == css_l4
strictness == strict
strict_output_equality == pass
track2_independence_status == independent_verified
track1_mbps > lightningcss_mbps + 1
lock14_status == pass
json_guard_state is valid
orphan_status has no open production primitive at close
```

Lock 16 is additionally required when a wave admits SIMD/ASM behavior. FIXPOINT
close, if ADMIT is uncloseable, must carry at least one measured union attempt
and one measured ASM-gen attempt with the fields in §2.7 plus REDRESS entries.

## §4 - Pre-blocked routes

Still blocked:

- CSS L4 admission against `ceil(baseline_mbps * 1.01)` or any non-lightningcss
  floor.
- Sheets or BBNF-self as a preflight-equivalent substitute before measured CSS
  L4 redress. Fallback reports must carry `fallback_after_css_redress_id` and
  cannot satisfy CSS ADMIT.
- `parse_only` SOTA claims.
- Hand-only CSS witnesses, stale `sheets_witness`, or report-only rows as
  generated Track 1.
- Generic JSON policy in generic crates or substrate code.
- Track 1/Track 2 source coupling.
- New directive, BIR variant, BackendShape variant, public substrate API, or
  parser-owned sidecar outside explicit SPEC authority.
- x86 implementation work.
- SIMD admission while the `escape_mask_64` bug remains unresolved.
- Orphan production SIMD primitives at close.

Unblocked at category level by the user pin:

- Union substrate routes adjacent to REDRESS 96/97/98.
- ASM-gen routes adjacent to REDRESS 88/89/90.

Those unblocks require telemetry, not rhetoric: fresh profile, microbench,
material differential, parity/equality, same-wave consumer, gate consumption,
and REDRESS disposition.

## §5 - Sources

- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`: D1-D6, CSS L4
  first, lightningcss floor, union/ASM-gen category unblocks, zero orphan
  target, `escape_mask_64` prerequisite.
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`: ADMIT/FIXPOINT close,
  telemetry binding, JSON guard priority, candidate space, W0 revalidation.
- `restart/skinny/tranches/sk-v12/HANDOFF.md`: goalset, telemetry binding,
  seed wave split, required S-P3 derivation.
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`: P3-D scope, schema
  inheritance, same-wave gate consumption, CHALLENGE lens obligations.
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`:
  pin profile authority, CSS absence boundary, accepted hot families, PMU
  artifact roots.
- `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`:
  accepted S-P2 candidate packet and pin-aware constraints.
- `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md` through
  `p2f-grammar-neutral.md`: SOTA comparator discipline, candidate legality,
  Lock 14/16 verdicts, union/ASM-gen route status.
- `restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md`:
  orphan set and ARMv9.2 surface.
- `restart/skinny/tranches/sk-v12/research/skv12-profile-truth-audit.md`:
  PMU truth discipline and stale narrative rejection.
- `restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md`: seven
  Lock 14 leaks and GrammarConfig surface.
- `restart/skinny/tranches/sk-v12/research/skv12-decision-engine-audit.md`:
  generated-size and decision-surface constraints.
- `restart/skinny/tranches/sk-v12/research/skv12-totality-fold-scout.md`:
  `escape_mask_64` falsifier and Lock 14/16 fold requirements.
- `restart/skinny/tranches/sk-v8/SPEC.md`: required telemetry precedent and
  producer-only fail-closed rule.
- `skinny/RESULTS.md`: live JSON result and guard surface.
- `skinny/REDRESS.md`: REDRESS 88/89/90, 96/97/98, 111/112/113, 119/120
  measured history.
