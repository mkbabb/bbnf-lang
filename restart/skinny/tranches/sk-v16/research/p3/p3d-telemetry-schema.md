# SK-V16 P3-D: Telemetry Schema Binding

Pass: S-P3 Synthesis-Plan. Cycle: V16.
Date: 2026-05-28.
Scope: bind the `skinny/RESULTS.md` schema and SK-V16 side-report consumers.
Output: this file.
Pass Alpha goalset: JSON 51/51 guard, CSS typed equality and cssparser SOTA, dirty-generated disposition, Pattern H roundtrip collapse, FNV quarantine, and conditional aarch64 SIMD proof.
Candidate pool: research/p2/ post-CHALLENGE survivors.

## Section 1 - Synthesis

SK-V16 preserves the visible schema-v3 RESULTS table and SK-V8/SK-V15
gate-consumed telemetry. It adds only fields that the gate must consume in the
same wave. Rendering a field without validation is a producer-only artifact
and rejects.

## Section 2 - Deliverable

Visible schema-v3 columns stay in the current order:

```text
Corpus
Workload
Outcome
Verdict
Strictness
parse_utf8
escape_complete
flaw_probe
Output plane
Track 1 Mbps
Track 2 Mbps
sonic-rs strict Mbps
sonic-rs lossy Mbps
simdjson DOM Mbps
simdjson On Demand Mbps
yyjson default Mbps
asmjson SWAR Mbps
asmjson AVX-512 Mbps
RapidJSON default Mbps
serde_json Mbps
Delta vs SK-V6
Delta vs sonic-strict
Delta vs simdjson DOM
Delta vs yyjson
Hot leaf
Signal
```

SK-V16 additions:

| Field | Required when | Gate rejection rule |
|---|---|---|
| `css_track1_typed_passes` | CSS report rows | Missing or non-integer rejects. |
| `css_cssparser_typed_passes` | CSS report rows | Missing or non-integer rejects. |
| `css_typed_summary_equal` | CSS equality/admission | Must be `true` before speed counts. |
| `css_provider_source` | CSS report rows | Must be grammar-derived; `CSS_GENERATED_RS`, fact-stream, `parse_full`, brace, or FNV proof rejects admission. |
| `dirty_generated_state` | generated checks | `clean`, `retired_by_regen`, or `manifested_intrinsic_block`; `dirty_unrouted` rejects. |
| `native_simd_status` | native claims | `not_in_scope`, `profile_first_scalar_ref_checkasm_same_wave`, or `blocked`; any production claim without the tuple rejects. |
| `typed_materialization_invariant` | typed CSS/product admission | Must be `pass`; missing layout/emitter proof or payload drop rejects. |

Required side-report flags:

```text
--skv16-css-typed-report <path>
--skv16-dirty-generated-report <path>
--skv16-pattern-h-roundtrip-report <path>
--skv16-native-simd-report <path>   # only when SIMD is scoped
```

Each flag requires `--check-results`, validates the current RESULTS snapshot,
parses a typed JSON report with unknown-field rejection, and fails if a report
field is emitted but not consumed.

## Section 3 - Falsifiability Binding

| Fixture | Expected result |
|---|---|
| JSON 51 rows with inherited telemetry and explicit SK-V16 non-applicable values | PASS, subject to JSON guard thresholds |
| CSS row with `css_typed_summary_equal=false` and CSS admission claim | FAIL |
| CSS row with `css_provider_source=CSS_GENERATED_RS` and admission claim | FAIL |
| Dirty-generated report missing `git status --short`, broad command, owner, or disposition | FAIL |
| Pattern H report with count other than 67 or header-only provenance | FAIL |
| Native SIMD report with x86/AVX evidence or missing scalar/checkasm/same-wave consumer | FAIL |
| Extra `producer_only_field` in any SK-V16 report | FAIL |

## Section 4 - Pre-Blocked Routes

Telemetry must prevent relabeling of CSS broadcast/fact-stream proof, dirty
generated state, FNV production proof, x86 evidence, retained sidecar
substrates, incomplete Lock 14 scans, and source-present unwired primitives.

## Section 5 - Sources

- `restart/skinny/tranches/sk-v16/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v16/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md`
- `skinny/RESULTS.md`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/xtask/src/main.rs`
- `skinny/REDRESS.md`
