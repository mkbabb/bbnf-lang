# SK-V17 P3-D: Telemetry-Schema Binding

Pass: S-P3 Synthesis-Plan. Cycle: V3.
Date: 2026-05-29.
Scope: Bind the `skinny/RESULTS.md` column schema for SK-V17 (CSS-tape subject). Carry
forward the SK-V8 24-field telemetry schema + the schema-v3 surface; name the SK-V17
CSS additions (Section 2 of `sk-v17/SYNTHESIS.md`); specify the `gate-json` rejection
rules for missing required columns. aarch64-only, benched skinny tree only.
Output: this file.
Pass Alpha goalset: `sk-v17/SYNTHESIS.md` §0.1 close-condition (CSS L4 typed > lightningcss
full-CSSOM on >=1 regular corpus at N>=50 median; JSON 51/51 guard; tape activation;
preserve-rich-ast; EXACT 8-field cssparser equality before speed) + §Section 2 telemetry
binding (the 22 CSS columns) + §0.5 per-corpus close conditions.
Candidate pool: `research/p2/` post-CHALLENGE survivors L1-L9 (consolidation
`HARDENING-S-P2-V3-CONSOLIDATED.md`, 99.1% ACCEPT, commit `f87ee713a`).

V3 fold (from V2 CHALLENGE): every P3-D-specific disposition was ACCEPT, twice (V1 CH1
§184 / CH2 §153-155 / CH6 §121 / CH7 §2,55-57; V2 CH1 §121 "columns map to gate
consumers", CH2 §169-177 "P3-D telemetry — ACCEPT: BOTH riders byte-equal +
`sheets_witness` invalid + `simd_non_json_exercise=css_l4`", CH6 §199-202 "P3-D telemetry
binding / producer-only rejection — ACCEPT: N≥50/median/cold/full-cssom enforced as
rejection rules"). The V2 packet carried ZERO P3-D-targeted REVISE; the four V2 residual
REVISEs route elsewhere (CH3-cosmetic SPEC §9 W5 citation re-key → P3-E + SPEC; CH4-6 L4
W2/W3 placement → P3-A + P3-C; CH5-2 L8 W1/W2 keying → P3-C; CH6 REVISE-1 W2 maintain
budget 0%→-2.0% → SPEC). All four are folded into their owners (the SPEC W2 maintain
budget now reads the bench-falsifiable -2.0% band, `SPEC.md:563-569`). The deliverable
schema is therefore UNCHANGED in substance this cycle.

This V3 applies only the precision re-keying the SPEC drift forced — the V2 fold grew the
SPEC when the W1 consumer enumeration + R-CH2-1 byte-equal line + W2 maintain band landed,
so this artefact's SPEC line citations are re-keyed to the converged anchors so the schema
stays cohesive with the canonical 6-wave SPEC (W0 baseline / W1 PRUNE-tape / W2 projection /
W3 NEON / W4 commit-by-construction / W5 close), no route re-opened, no orphan REVISE:
(a) **R-CH2-1** (load-bearing): the `projection_generality_exercise` byte-equal-re-emission
gate is now anchored at `SPEC.md:550-557` ("JSON rider re-emits byte-equal THROUGH the new
generator (R-CH2-1, load-bearing)") + the W2 projection gate `SPEC.md:534-536,561-562`
(was `:495,509,513`) — a CSS-only generator that leaves JSON's hand-written `value_from_ref`
untouched still FAILS (`SPEC.md:556-557`); (b) the `dirty_generated_state==clean` gate is
keyed to the **W5 close** exit gate (`SPEC.md:748,757`, was `:687,696`), NOT W0 — W0 lands
NO generated change (`SPEC.md:375` "NO parser/scanner/SIMD/codegen behavior or generated
parser output change lands"); (c) `tape_activated`/`w5c_profile_array_retired` W1 evidence
covers the FULL `emit_fact_stream` round-trip consumer set the SPEC W1 owner-paths
enumerate (`codegen/src/lib.rs:581,1001,1035` generator-output assertions +
`runtime_generator.rs:621,666,694` + the `css_l4_*_emit_fact_stream` round-trip test
consumers, `SPEC.md:415-422,453-457`) so no dangling round-trip assertion survives the W1
PRUNE — the prior V2 path-list `lib.rs:581,597,1001,1035,1109,1113` is superseded by the
SPEC's authoritative enumeration.

## §1 — Synthesis (concrete; cites P1 row, P2 candidate, REDRESS entry, or goalset line)

### 1.1 — The schema problem SK-V17 inherits

The benched JSON `skinny/RESULTS.md` table is a 26-column surface today
(`head -3 skinny/RESULTS.md` header pipe-split = 26 columns: `Corpus | Workload |
Outcome | Verdict | Strictness | parse_utf8 | escape_complete | flaw_probe |
Output plane | Track 1 Mbps | Track 2 Mbps | sonic-rs strict Mbps | sonic-rs lossy
Mbps | simdjson DOM Mbps | simdjson On Demand Mbps | yyjson default Mbps | asmjson
SWAR Mbps | asmjson AVX-512 Mbps | RapidJSON default Mbps | serde_json Mbps | Δ vs
SK-V6 | Δ vs sonic-strict | Δ vs simdjson DOM | Δ vs yyjson | Hot leaf | Signal`).
Behind that rendered surface sits the SK-V8 **24-field required-telemetry schema**
(`restart/skinny/tranches/sk-v8/SPEC.md` §0.4), implemented as the
`SkV8RowMetadata` struct family in `skinny/crates/bbnf-bench/src/report.rs:153-198`
(`row_id`, `grammar_id`, `domain`, `measured_validation_path`, `profile_artifact`,
`sample_cost`, `sample_count:u64`, `build_flags`, `host_triple`, `feature_mask`,
`costfacts_rule_id`, `costfacts_chosen_shape`, `costfacts_rejected_alternative_ids`,
`redress_entry`, `wave_id`, `run_id`, `comparator_plane`, `per_iter_equality`,
`audit_overlay_verdict`, `audit_overlay_reference`, `sidecar_freshness`,
`substrate_surface`, `structural_projection_status`, `substrate_cardinality`,
`same_wave_consumer_class`, `track2_independence_status`) plus the
`SkV8ComparatorEvidence` cohort (`report.rs:140-147`:
`comparator_id`/`comparator_plane`/`comparator_strictness`/`comparator_freshness`/
`sidecar_freshness`/`value_mbps`/`source_artifact`). The schema is consumed by
`gate::validate_schema(&rows)` (`skinny/crates/bbnf-bench/src/gate.rs:185`) which
the `gate-json` bin gates on via `schema_ok: gate::validate_schema(&rows) &&
estimates.required_present()` (`skinny/crates/bbnf-bench/src/bin/gate.rs:579`).

This schema is **carried forward verbatim for the JSON guard rows** — the JSON
51/51 tripwire (`sk-v17/SYNTHESIS.md` §0.1 JSON-guard gate) is enforced through it.
SK-V17 adds the CSS >SOTA close columns on top; it does NOT replace the 24-field
schema and does NOT invent a new SPEC shape (PASS-3 §8.1; SK-V8 SPEC §0.4 "may keep
the existing schema surface").

### 1.2 — Why the schema is load-bearing for THIS subject (the W8R broadcast tripwire)

The SK-V16-close CSS rows in `skinny/RESULTS.md` are the **24 W8R broadcast
diagnostics** (lines 112-135, grep-verified `grep -c '^| css_l4/.*/direct_to_struct/
main ' skinny/RESULTS.md` = 24), every one `not_admitted:SK-V15-W0-broadcast-
diagnostic` / `AUDIT-FALSIFIED`, all carrying the SINGLE broadcast tuple
`track1_mbps=2319.041; cssparser_mbps=2362.037; lightningcss_mbps=929.281`
projected across 24 rows (one timing tuple → 24 conceptual rows). There are ZERO
admitted typed CSS rows (`sk-v17/SYNTHESIS.md` §0.2). The telemetry schema's
single most load-bearing job for SK-V17 is therefore to make the gate **reject the
broadcast**: a CSS row with `css_sample_count == 1`, or one timing tuple replicated
across multiple corpus rows, is the exact regression the W8R failure embodies, and
`sk-v17/SYNTHESIS.md` §Section 2 final paragraph names it as the gate's mandatory
rejection ("any single-tuple broadcast (`sample_count == 1` or one tuple across
multiple corpus rows — the W8R regression tripwire)"). This is pre-block route
(§0.4: "The 24-row broadcast measurement … pre-blocked").

### 1.3 — Candidate→column trace (every shortlist candidate is observable)

Per PASS-3 §2 (P3-D feeds the gate that every wave's exit gate keys on) and the
`typed-materialization-invariant` discipline (an emitted field not consumed by the
gate is a producer-only artefact and fails the wave), every L1-L9 candidate that
lands behaviour must register an observable telemetry fact:

| Candidate (P2 survivor) | Telemetry fact it must surface | Column |
|---|---|---|
| L1 NEON byte-class classifier (`select_classifier(alphabet)`, `bbnf-simd/src/dispatch.rs:42`) | non-JSON SIMD exercise named + parity/checkasm status | `simd_non_json_exercise`, `native_simd_status` |
| L2 `push_plain_offset` tape append (`assembler.rs:71`) | tape became the live CSS parse substrate (PayloadArena write/alloc counters) | `tape_activated` |
| L3 lazy `ValueRef<G>` projection (`json/value.rs:143` isomorph) | rich CSSOM produced lazily over the tape, not flattened/eager; ONE `BackendRule`-walking generator emits the view for EACH grammar (JSON the witness re-emitting byte-equal, CSS the first-mover rider) | `css_rich_ast_preserved`, `lazy_view_generated`, `projection_generality_exercise` |
| L4 tokenize-once reuse | (consumer of L1 index; observed via `tape_activated` + Mbps lift) | `tape_activated`, `css_track1_typed_median_mbps` |
| L5 `comment_body_mask_64` (net-new) | folds into L1 classifier; SIMD parity status | `native_simd_status` |
| L6 `bracket_depth_mask_64` (net-new, scalar-balance default) | folds into L1; SIMD parity status | `native_simd_status` |
| L7 one-shot SIMD reserve | (capacity on existing `offsets`; observed via Mbps lift, no own column) | `css_track1_typed_median_mbps` |
| L8 sparse-flag side-table (flag = `BackendRule` branch-tag) | supports L3 projection; observed via `css_rich_ast_preserved` | `css_rich_ast_preserved` |
| L9 commit-by-construction Alt-mode (CONDITIONAL, post-tape re-profile gate) | observed via Mbps lift on the structurally regular corpora | `css_track1_typed_median_mbps`, `delta_vs_lightningcss` |

L1's binding carry-forward condition (L1/L4 index == tape-offsets identity; L8
flag = `BackendRule` branch-tag) is itself NOT a free-standing telemetry column —
it is asserted in the wave's substrate audit and surfaced through
`tape_activated == true` plus the structural-projection-status field of the
inherited SK-V8 schema (`report.rs:193`), which must read a single retained tape
(Lock 1; §0.4 "if structural offsets are retained, the structural projection IS
the tape").

## §2 — Deliverable (the SK-V17 telemetry schema)

### 2.1 — Carried-forward base: SK-V8 24-field schema (UNCHANGED)

The SK-V8 §0.4 required-telemetry schema is carried forward verbatim, implemented as
`SkV8RowMetadata` (`report.rs:153-198`) + `SkV8ComparatorEvidence` (`report.rs:140-147`),
consumed by `gate::validate_schema` (`gate.rs:185`). These 24 fields gate every JSON
guard row and every CSS admission row alike (a CSS admit row still carries `row_id`,
`grammar_id=css_l4`, `domain`, `measured_validation_path`, `profile_artifact`,
`sample_cost`, `sample_count`, `build_flags`, `host_triple`, `feature_mask`,
`wave_id`, `run_id`, `substrate_surface`, `structural_projection_status`,
`substrate_cardinality`, `same_wave_consumer_class`, `track2_independence_status`).
The rendered 26-column JSON table surface is preserved for the JSON rows
(`sk-v17/SYNTHESIS.md` §Section 2 final ¶: "The retained JSON schema … stays in
force for the JSON guard rows").

No SK-V8 field is removed. No SK-V8 field changes type. SK-V17 is purely additive.

### 2.2 — SK-V17 CSS additions (22 columns; verbatim from SYNTHESIS §Section 2)

These are the SK-V17-specific required fields. They are emitted as the
gate-consumed CSS report (`--skv17-css-sota-report <path>`), rendered as columns OR
as a gate-consumed JSON payload (per SK-V8 §0.4 "rendered as columns, a gate-consumed
manifest, or a gate-consumed JSON payload, but they must be consumed by `gate-json`
in the same wave"). Type and required-flag verbatim from `sk-v17/SYNTHESIS.md`
§Section 2:

| # | Column | Type | Required |
|---|---|---|---|
| 1 | `css_corpus` | string (one of `bootstrap`/`tailwindcss`/`material-components-web`/`animate` — `css_l4_corpus.rs:22-54`) | yes for CSS |
| 2 | `css_sample_count` | integer (>=50) | yes for CSS |
| 3 | `css_sample_statistic` | enum (`median`) | yes for CSS |
| 4 | `css_sample_mode` | enum (`cold`/`warm`) — must be `cold` | yes for CSS |
| 5 | `css_track1_typed_median_mbps` | number | yes for CSS |
| 6 | `css_lightningcss_full_cssom_median_mbps` | number (same-run re-baseline; the materializing bar) | yes for CSS |
| 7 | `css_cssparser_tokenscan_median_mbps` | number | optional (flaw probe only) |
| 8 | `css_comparator_plane` | enum (`full-cssom`/`typed-direct`/`token-scan`/`none`) | yes for CSS |
| 9 | `delta_vs_lightningcss` | number (% or ratio, per-corpus, vs same-run median) | yes for CSS |
| 10 | `css_track1_typed_passes` | integer | yes for CSS |
| 11 | `css_cssparser_typed_passes` | integer | yes for CSS |
| 12 | `css_typed_summary_equal` | boolean (EXACT 8-field, gate before speed) | yes for CSS admission |
| 13 | `css_rich_ast_preserved` | boolean (CSSOM via lazy `ValueRef` projection, not flattened, not eager) | yes for CSS |
| 14 | `css_provider_source` | string (grammar source path) | yes for CSS |
| 15 | `tape_activated` | boolean (benched `track1::parser::parse` emits into skinny `Tape`, read via `ValueRef`; proven by `PayloadArena` write/alloc counters; NOT satisfiable by grep in `crates/core/`) | yes for CSS |
| 16 | `lazy_view_generated` | boolean (skinny accessor generator emits document/value/view/visitor over `BackendRule`) | yes for CSS |
| 17 | `projection_generality_exercise` | enum-set over `{json, css_l4}` — BOTH riders re-emit byte-equal THROUGH the single W2 `BackendRule`-walking generator (JSON the witness, CSS the first-mover rider). A bare named value is producer-only: the gate verifies the JSON `value_from_ref` rider re-emits byte-equal through the NEW generator (if it changes, W2 FAILS). `sheets_witness` NOT valid — no `BackendRule`; non-CSS-non-JSON projection is SK-V18 | yes for projection-generality claims |
| 18 | `w5c_profile_array_retired` | boolean (`W5C_REQUEST_FACT_PROFILES` deleted; CSS routing grammar-derived) | yes for CSS |
| 19 | `dirty_generated_state` | enum (`clean`/`retired`/`routed-intrinsic-block`) | yes for generated checks |
| 20 | `native_simd_status` | enum (`scalar`/`parity-pass`/`checkasm-pass`/`not-applicable`) | yes for SIMD claims |
| 21 | `simd_non_json_exercise` | string (named grammar exercising the SIMD leaf via shared `select_classifier(alphabet)`: `css_l4`; distinct from `projection_generality_exercise`) | yes for SIMD claims |
| 22 | `css_8field_equality_witness` | string (the 8 equality counts as banked: `rules=10136,style=9561,sel=9561,decls=20043,track1_errors=0,cssparser_errors=0` + 4/4 corpora; re-proven post-conversion) | yes for CSS admission |

Columns 1-21 are verbatim from `sk-v17/SYNTHESIS.md` §Section 2. Column 22
(`css_8field_equality_witness`) is the SK-V17 P3-D **named addition**: the boolean
`css_typed_summary_equal` (#12) asserts equality but does not record WHICH counts
were compared; the §0.1 "CSS typed equality" gate is keyed to EXACT 8-field counts
(`rules=10136, style=9561, sel=9561, decls=20043, track1_errors=0,
cssparser_errors=0`, 4/4 corpora, banked `1c5bd7a25`) and the §0.3 receiver
obligation is "re-prove EXACT 8-field structural equality … before any speed counts".
A bare boolean is producer-only against that gate (the gate cannot re-verify the
counts moved post-conversion vs the banked figure); the witness column makes the
equality assertion gate-auditable. It is a string evidence field, not a behaviour
field, so it adds no LOC to any behaviour wave and is emitted by the equality
re-proof step the receiver already runs.

**Total SK-V17 schema: 24 SK-V8 base fields + 22 CSS additions = 46 required/optional
fields**, of which the CSS-admission-blocking subset is enumerated in §3.

### 2.3 — Column-to-candidate / column-to-close-gate map

Every CSS column traces to a §0.1 close gate or a §0.4 pre-block tripwire:

| Column | Close gate / pre-block it serves |
|---|---|
| `css_corpus` | §0.5 benched-set bound (reject phantom `normalize`) |
| `css_sample_count`, `css_sample_statistic`, `css_sample_mode` | §0.1 "Telemetry honesty (N>=50 fix)"; W8R single-sample retired |
| `css_track1_typed_median_mbps`, `css_lightningcss_full_cssom_median_mbps`, `delta_vs_lightningcss` | §0.1 "CSS >SOTA on regular corpora"; §0.5 per-corpus bar |
| `css_cssparser_tokenscan_median_mbps`, `css_comparator_plane` | §0.6 strict comparator gate (cssparser = flaw probe, lightningcss full-cssom = fair bar) |
| `css_track1_typed_passes`, `css_cssparser_typed_passes`, `css_typed_summary_equal`, `css_8field_equality_witness` | §0.1 "CSS typed equality (gate before speed)" |
| `css_rich_ast_preserved` | §0.1 "preserve-rich-ast"; §0.4 AZ-IV eager-tree pre-block |
| `tape_activated` | §0.1 "Tape activation (not dead code)"; retires W6 unwired-dead-code finding |
| `lazy_view_generated`, `css_provider_source`, `projection_generality_exercise` | §0.1 "Layout-driven projection"; §0.4 generality clause; W2 exit gate byte-equal re-emission of BOTH riders through ONE generator (`SPEC.md:534-536,550-557,561-562`) — JSON the witness re-emitted byte-equal, CSS the first-mover rider, from ONE `BackendRule`-walking generator |
| `w5c_profile_array_retired` | §0.1 "Layout-driven projection"; §0.4 `W5C_REQUEST_FACT_PROFILES` pre-block (W1 PRUNE) |
| `dirty_generated_state` | §0.1 "Generated-state cleanliness" (`regen --check` 9/9) — gated at the **W5 close** exit gate (`SPEC.md:748,757`); W0 lands NO generated change (`SPEC.md:375`) so W0 does NOT key this column |
| `native_simd_status`, `simd_non_json_exercise` | §0.1 "NEON hot-leaf union"; Lock 16 SIMD parity; Lock 14 non-JSON exercise |

## §3 — Falsifiability binding (named corpus rows + Mbps thresholds)

### 3.1 — gate-json consumer binding (executable, same-wave)

S-P3 binds the executable consumer (`sk-v17/SYNTHESIS.md` §Section 2):

```
(cd skinny && cargo xtask gate-json --check-results --skv17-css-sota-report <path>)
```

This follows the existing companion-report-path flag pattern
(`skinny/crates/bbnf-bench/src/bin/gate.rs:947-1011` — `--skv12-css-l4-sota-report`,
`--skv13-css-*-report`, each requiring `--check-results`). The new
`--skv17-css-sota-report` flag is added to the gate-json passthrough allowlist in
`skinny/xtask/src/main.rs` (the `gate_json` arg-validation block, currently
`main.rs:389` allowlist + `:425` reject-unsupported) and parsed by a new
`skv17_css_sota_report_path(args)` companion in `gate.rs`, mirroring
`skv12_css_l4_sota_report_path`. The report payload is validated by a new
`gate::validate_skv17_css_schema(&css_rows)` returning the same `schema_ok` boolean
that already folds into `schema_ok` at `gate.rs:579`. SAME-WAVE: the producer (the
N>=50 CSS harness emitting the report) and the consumer (the gate validating it)
land in the W0 telemetry-lock slice — an emitted field not consumed in the same
wave is a producer-only artefact and fails the wave (SK-V8 §0.4; `typed-
materialization-invariant`).

### 3.2 — gate-json rejection rules (the measurable gate)

`gate-json --skv17-css-sota-report <path>` REJECTS (exit nonzero) when ANY CSS row
fails ANY of these — verbatim from `sk-v17/SYNTHESIS.md` §Section 2 plus the named
additions:

| Rejection rule | Source gate | Falsifier |
|---|---|---|
| `css_sample_count < 50` | §0.1 telemetry honesty | any row with N<50 |
| `css_sample_statistic != median` | §0.1 telemetry honesty | mean/single-sample row |
| `css_sample_mode != cold` | §0.1 telemetry honesty; `no-warm-benches` | a warm/cached row |
| `css_comparator_plane != full-cssom` for the lightningcss bar | §0.6 strict comparator gate | token-scan/none plane claimed as the >SOTA bar |
| `css_typed_summary_equal != true` before any speed admission | §0.1 equality-before-speed | a row claiming >SOTA without equality |
| `css_8field_equality_witness` counts != banked (`rules=10136/style=9561/sel=9561/decls=20043`, errors=0/0, 4/4 corpora) | §0.1 / §0.3 re-proof | a post-conversion count drift |
| `css_rich_ast_preserved != true` | §0.1 preserve-rich-ast; §0.4 AZ-IV | a flattened/eager-materialized row |
| `css_corpus` not in `{bootstrap, tailwindcss, material-components-web, animate}` | §0.5 benched-set bound | phantom `normalize` row |
| `css_sample_count == 1` OR one timing tuple replicated across >1 corpus row | §0.4 W8R broadcast pre-block | the 24-row broadcast |
| `tape_activated != true` (no `PayloadArena` write/alloc counter evidence) | §0.1 tape-activation | fact-stream String emission still live |
| `w5c_profile_array_retired != true` | §0.1 layout-driven projection | `W5C_REQUEST_FACT_PROFILES` still present |
| `lazy_view_generated != true` for a CSS admission | §0.1 layout-driven projection | hand-routed projection |
| `projection_generality_exercise` lacks a byte-equal `json` rider re-emitted THROUGH the new W2 generator (the JSON `value_from_ref` output changed, OR JSON's hand-written path was left untouched) | §0.4 generality clause; SPEC W2 exit gate (`SPEC.md:534-536,550-557`) | a CSS-only generator naming `css_l4` while JSON's hand-written projection is bypassed (the CH2 generic-named-CSS-generator failure mode) |
| `native_simd_status` not in `{scalar, parity-pass, checkasm-pass, not-applicable}` for any SIMD claim | §0.1 NEON gate; Lock 16 | unverified kernel |
| `simd_non_json_exercise != css_l4` for a SIMD claim | §0.1 / Lock 14 non-JSON exercise | JSON-only SIMD |
| `dirty_generated_state != clean` (unless `routed-intrinsic-block` with proof) — checked at the **W5 close** exit gate ONLY (`SPEC.md:748,757`); W0 does NOT require it (W0 lands no generated change, `SPEC.md:375`) | §0.1 generated cleanliness (`regen --check` 9/9) | dirty generated files at W5 close |

### 3.3 — The >SOTA exit threshold (per-corpus, against same-run lightningcss)

The numeric >SOTA gate is per-corpus, NOT a fixed prior number. The prior figures
(793/833/929/974, run-dependent) are NOT the gate (`sk-v17/SYNTHESIS.md` §0.2/§0.5).
The gate is:

- For a regular corpus row (`css_corpus ∈ {animate, bootstrap}`): ADMIT iff
  `css_track1_typed_median_mbps > css_lightningcss_full_cssom_median_mbps`
  (i.e. `delta_vs_lightningcss > 1.0x`) on the SAME run at N>=50 median, with
  `css_typed_summary_equal == true` and `css_rich_ast_preserved == true`.
- Tranche-level success: at least ONE of {`animate`, `bootstrap`} crosses
  (§0.5 tranche criterion). The gate emits a tranche-pass boolean derived from
  the per-corpus delta rows.
- For `tailwindcss`: the gate does NOT require crossing. It requires the row be
  PRESENT with N>=50 cold median + a non-empty `Hot leaf` attribution; a short
  landing is recorded honestly (`delta_vs_lightningcss < 1.0x` admitted as a REDRESS
  residual, NOT a gate failure) per §0.5 fallback. The gate REJECTS only a MISSING
  tailwind row or a paper-closed tailwind claim (no per-corpus median).
- For `material-components-web`: per-corpus median REPORTED; the full-corpus row is
  the integration check, not a single-corpus gate (§0.5).

All per-corpus lightningcss endpoints are UNMEASURED-PENDING until the Wave-0 N>=50
harness emits the per-corpus split (§0.5); the gate therefore keys on the
SAME-RUN delta, never on an inferred per-corpus endpoint (animate↔164,
tailwind↔51, material↔60 are INFERRED, self-flagged, NOT gate inputs).

### 3.4 — Wave-0 telemetry-lock falsifiability gate

Per PASS-3 §8.3 (W0 is always baseline + telemetry) and `build-infra-first`, the W0
exit gate is itself a telemetry-schema gate, measurable:

- `gate-json --check-results --skv17-css-sota-report <W0-report>` exits 0 with every
  CSS corpus row present (4/4 benched corpora), `css_sample_count >= 50`,
  `css_sample_mode == cold`, `css_sample_statistic == median`, and the lightningcss
  full-CSSOM comparator re-baselined this run (`css_comparator_plane == full-cssom`,
  same-run).
- JSON 51/51 guard rows still pass `gate::validate_schema` and stay A/GO within
  +/-1.0% of the captured `SK-V17-open` seed (the JSON tripwire; §0.1 JSON guard).
- The W6 single-sample harness (`W6_SAMPLE_COUNT=1`) is grep-absent from the
  SK-V17 harness path; the gate rejects any `css_sample_count == 1` row.
- W0 lands NO behaviour change AND NO generated change (`SPEC.md:375`: "NO
  parser/scanner/SIMD/codegen behavior or generated parser output change lands"; tape
  still inert for CSS at W0; W0 is profile + comparator wiring + schema only). The
  `tape_activated` column reads its honest pre-activation value at W0 and the gate
  does not yet require `true` (it requires `true` only at the W1 tape-activation
  exit, §3.2). This prevents a W0 paper-close on `tape_activated`.
- The `dirty_generated_state == clean` (`regen --check` 9/9) gate is NOT a W0 gate.
  W0 lands no generated change, so there is nothing to regen-check at W0; the column
  is keyed to the **W5 close** exit gate (`SPEC.md:748,757`). Filing it at W0 would
  be a mis-attribution (W0 cannot fail a clean-regen check it does not perform).

## §4 — Pre-blocked routes (REDRESS entries each wave must NOT re-open via telemetry)

The telemetry schema is the enforcement surface for several §0.4 pre-blocks. A wave
that emits a telemetry value satisfying a pre-blocked route is REJECTed:

- **W8R 24-row broadcast** (§0.4; the source of the 24 falsified `css_l4/*/
  direct_to_struct/main` rows, RESULTS lines 112-135): the gate rejects
  `css_sample_count == 1` and one-tuple-across-multiple-rows. The schema must NOT
  carry a single shared timing tuple; each `css_corpus` row carries its OWN
  N>=50 median.
- **CSS fact-stream String as admission plane** (§0.4; `emit_fact_stream`,
  `generated.rs:5`): `tape_activated == true` (PayloadArena counter evidence) is
  required for any CSS admit; a row whose Track 1 still returns `String` cannot
  satisfy it. The fact-stream comparator (`assert_lightningcss_strict_equality`
  against a fact stream, `nonjson_css_l4.rs:776`) is retired — the comparator
  column must read `full-cssom`, not a fact-stream plane. PER the same-wave-consumer
  non-negotiable, the W1 PRUNE that retires `emit_fact_stream` must migrate/delete
  EVERY round-trip consumer in the same commit — the SPEC W1 owner-paths enumerate
  the authoritative set (`SPEC.md:415-422,453-457`): the `css_l4_*_emit_fact_stream`
  round-trip test consumers (`SPEC.md:417`), the generator-output-string assertions
  `.contains("emit_fact_stream")` at `codegen/src/lib.rs:581,1001,1035`
  (`SPEC.md:419,456`), and the `emit_fact_stream` emitters at
  `runtime_generator.rs:621,666,694` (`SPEC.md:422`). `tape_activated == true` at the
  W1 exit therefore also gates that NO dangling `emit_fact_stream` round-trip
  assertion survives (`SPEC.md:457` "NO `emit_fact_stream` round-trip assertion may
  dangle"); a surviving assertion is a producer-only residual of the retired plane and
  the gate treats the wave as not-pruned (CH3 same-wave-consumer).
- **`W5C_REQUEST_FACT_PROFILES` hand-coded routing** (§0.4; `codegen/src/lib.rs:336`):
  `w5c_profile_array_retired == true` is required; relocating its per-rule branching
  into projection DATA is the overfit re-entry seam — the gate's
  `projection_generality_exercise` and `w5c_profile_array_retired` columns force
  the routing to be grammar-derived, not re-hardcoded.
- **AZ-IV eager value-tree materialization** (§0.4; the 118x regression):
  `css_rich_ast_preserved == true` requires lazy `ValueRef` projection; an eager
  per-leaf `Box::new` / f64-alloc-per-number / per-color `Box<CssColor>` row cannot
  honestly set it true. The gate cannot directly measure laziness, so the
  same-wave consumer requirement (§3.1) + the PayloadArena counter (no per-leaf
  alloc spike) is the evidence floor.
- **x86 / AVX / SVE** (§0.4): `native_simd_status` and `simd_non_json_exercise`
  describe aarch64 NEON only; the schema carries no AVX-512/SVE column for CSS (the
  JSON `asmjson AVX-512 Mbps` column stays `n/a` for CSS rows — it is a JSON
  comparator column, not an SK-V17 CSS production claim).
- **FNV / fixture contrivance** (§0.4): no telemetry column admits an FNV-arbiter or
  per-corpus hand-coded fixture as correctness proof; correctness rides
  `css_typed_summary_equal` + `css_8field_equality_witness` (cssparser oracle), and
  `track2_independence_status` (inherited SK-V8 field) must show Track 2 is
  structurally distinct from Track 1 (Lock 1, CH5).
- **Sidecar / second-substrate telemetry** (§0.4 hidden-coupling pre-block; Lock 1):
  the inherited `substrate_surface` / `substrate_cardinality` /
  `structural_projection_status` SK-V8 fields (`report.rs:192-194`) must read a
  SINGLE retained tape; a row whose substrate_cardinality > 1 (a second tape /
  retained cursor / aux density table) is REJECTed. The SIMD `Vec<u32>` structural
  index is a transient producer, NOT a retained sidecar (§0.4: "A SIMD mask stream
  is a transient producer, not a retained sidecar").

Inherited REDRESS pre-block families enforced through the schema (semantics, not
just ids; `sk-v17/SYNTHESIS.md` §0.4): `28+33` (NEON/TBL tiny-string as parse-close
— `native_simd_status` alone cannot admit a row; needs `css_track1_typed_median_mbps`
lift), `50-55` / `60-72` / `80` (parser-owned aux tables / cursors / scratch —
blocked by `substrate_cardinality == 1`), `82-84` / `88` / `89` (single-quartet
classifier, PMULL default body, CTZ production consumer — `native_simd_status` +
profile-first), `96-98`, `183/184/209-213`, `215`, `242-247`, FNV closed-enum
production migration.

## §5 — Sources (every upstream artefact cited)

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` — the pass contract (§2 P3-D
  scope row: carry SK-V8 24-column schema + schema-v3 surface, name SK-V{N}
  additions, specify `gate-json` rejection rules; §8.1 SPEC mirrors SK-V8; §8.2
  telemetry binding load-bearing, gate-json rejects rows missing required columns;
  §8.3 W0 always baseline + telemetry).
- `restart/skinny/tranches/sk-v8/SPEC.md` §0.4 — the carried-forward 24-field
  required-telemetry schema + the "rendered as columns / manifest / JSON payload,
  consumed by gate-json in the same wave" rule + "producer-only telemetry rejects
  the wave".
- `restart/skinny/tranches/sk-v17/SYNTHESIS.md` — Pass Alpha goalset: §0.1 close
  conditions (JSON guard / tape activation / layout projection / CSS typed equality /
  preserve-rich-ast / CSS >SOTA / honest tailwind / telemetry honesty N>=50 / NEON /
  generated cleanliness / foldable); §0.2 starting state (24 W8R broadcast rows, zero
  admitted typed CSS); §0.4 pre-blocks; §0.5 per-corpus close conditions; §0.6 strict
  comparator gate; §Section 2 telemetry binding (the 22 CSS columns + gate-json
  consumer + rejection rules).
- `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`
  (commit `f87ee713a`) — the L1-L9 survivor pool (99.1% ACCEPT) each candidate must
  surface a telemetry fact for; the REJECTed set (orphan udot, net-new i8mm, FNV,
  asmjson FSM, lo6-on-CSS, D6 second substrate) barred; binding carry-forward
  conditions (L1/L4 index==tape-offsets identity, L8 flag=BackendRule branch-tag,
  L9 post-CF-1 re-profile gate).
- `skinny/RESULTS.md` — the live 26-column JSON table surface (header pipe-split =
  26); the 24 W8R broadcast CSS rows (lines 112-135, `grep -c '^| css_l4/.*/
  direct_to_struct/main '` = 24).
- `skinny/crates/bbnf-bench/src/report.rs:140-204` — the `SkV8RowMetadata` /
  `SkV8ComparatorEvidence` / `TelemetryRow` structs implementing the carried schema.
- `skinny/crates/bbnf-bench/src/gate.rs:185` (`validate_schema`), `:32-198` (the
  gate types) — the schema consumer.
- `skinny/crates/bbnf-bench/src/bin/gate.rs:579` (`schema_ok` fold), `:947-1011`
  (the companion-report-path flag pattern the `--skv17-css-sota-report` flag
  mirrors), `:106-462` (the `--check-results` requirement per companion flag).
- `skinny/xtask/src/main.rs:59` (`gate-json` dispatch), `:389` (the gate-json arg
  allowlist), `:425` (reject-unsupported) — where the new flag is registered.
- `restart/locks/LOCKS.md` — Lock 1 (one substrate, `substrate_cardinality == 1`),
  Lock 14 (grammar-neutrality, `simd_non_json_exercise`/`projection_generality_
  exercise`), Lock 16 (SIMD parity, `native_simd_status`).
- `restart/skinny/tranches/sk-v17/SPEC.md` (canonical 6-wave manifest, this cycle's
  reconciled SPEC) — `:153-198` (§0.4 required CSS telemetry columns + gate-json
  consumer binding + producer-only-rejects rule), `:373` (W0 lands no
  generated/behaviour change), `:415-422,453-457` (W1 PRUNE: full `emit_fact_stream`
  round-trip consumer set + W5C array retired → tape activation), `:534-536,550-557,561-562`
  (W2 exit gate: ONE `BackendRule`-walking generator emits the view for JSON + CSS,
  `projection_generality_exercise ∈ {json, css_l4}` with both riders re-emitted
  byte-equal through the new generator — the R-CH2-1 byte-equal promotion,
  load-bearing line at `:550-557`), `:563-569` (W2 maintain budget = bench-falsifiable
  -2.0% band vs W1 typed-tape baseline, CH6 REVISE-1 fold), `:743,752` (W5 close:
  `dirty_generated_state=clean`, `regen --check` 9/9 exit 0).
- V1 + V2 CHALLENGE (`research/p3/hardening/V{1,2}/CH*.md`) — every P3-D-specific
  disposition ACCEPT for two consecutive cycles: V1 CH1:184 / CH2:153-155 / CH6:121 /
  CH7:2,55-57; V2 CH1:121 (columns map to gate consumers), CH2:169-177 (`projection_
  generality_exercise` BOTH riders byte-equal + `sheets_witness` invalid +
  `simd_non_json_exercise=css_l4`), CH6:199-202 (producer-only rejection +
  N≥50/median/cold/full-cssom enforced). The V2 packet carried ZERO P3-D-targeted
  REVISE; the four V2 residual REVISEs (CH3-cosmetic SPEC §9 citation, CH4-6 L4 W2/W3,
  CH5-2 L8 W1/W2, CH6 REVISE-1 W2 maintain budget) route to P3-A / P3-C / P3-E / SPEC,
  not P3-D. This V3 re-keys only the SPEC line citations the V2 SPEC growth drifted
  (`:550-557` R-CH2-1; `:743,752` W5 close; `:415-422,453-457` W1 consumer set; the
  superseded V2 path-list `lib.rs:581,597,1001,1035,1109,1113` is replaced by the
  SPEC's authoritative W1 owner-path enumeration).
