# SK-V12 SPEC - S-P3 Wave Plan Draft

Date: 2026-05-20.

Status: S-P3 V1 planning draft. This file is not implementation dispatch
authority until S-P3 CHALLENGE converges and the orchestrator promotes the
packet. It folds the SK-V12 Pass Alpha goalset, converged S-P1 profile,
converged S-P2 research, and this P3-F draft into a W0-W4 wave plan.

Authority:

- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv`
- `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3f-spec-draft.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Dispatch lock:

- No SK-V12 implementation wave dispatches from this S-P3 V1 draft.
- W0-W4 become dispatchable only after S-P3 convergence under
  `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`.
- Every behavior wave still requires its own wave-triumvirate research, plan,
  CHALLENGE when required, and redress per
  `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.

## Section 0 - Close Condition And Goalset

### Section 0.1 - Global Close Condition

SK-V12 closes only when all of these are true:

1. S-P3 converges and W0 locks the SK-V12-open telemetry/report/gate surface.
2. Exactly one generated non-JSON direct or typed baseline is admitted or a
   measured REDRESS block proves no generated baseline can be created inside
   the accepted SK-V12 owner surface.
3. If W1 admits a baseline, W2 admits one measured grammar-generalized
   intervention against that same baseline at >=
   `ceil(W1_baseline_track1_mbps * 1.01)`, unless W2 records a measured reject.
4. The 4 admitted JSON direct rows and 7 admitted JSON typed rows hold their
   guard floors in every behavior wave that refreshes results.
5. The 13 JSON direct residual rows remain pre-blocked unless W3's material
   reopen entry gate passes.
6. `parse_only` remains diagnostic: no parse row supports SK-V12 SOTA admission.
7. Every new non-JSON row, comparator/oracle field, run id, gate field, or
   companion report is consumed by a same-wave gate.
8. No generic crate or shared runtime path learns JSON policy; any generic edit
   carries executable non-JSON proof for the selected grammar.
9. No primitive, SIMD kernel, parse-that helper, generated parser path, or
   output-plane contract ships without scalar reference, parity/checkasm when
   applicable, micro-proof, and a same-wave generated consumer.
10. `skinny/RESULTS.md`, `skinny/REDRESS.md`, `SYNTHESIS.md`, `HANDOFF.md`,
    this SPEC, and `DISPATCH-PROMPT.md` agree at close.

Close target: one generated non-JSON baseline plus one measured
grammar-generalized intervention, with JSON guards preserved. Honest close may
be a measured `BLOCKED` generated-baseline route if W1 proves the accepted
owner surface cannot create the baseline. JSON direct work is never first.

### Section 0.2 - Comparator Classes

SK-V12 uses three comparator classes:

| Class | Examples | Admission use |
|---|---|---|
| Same-run strict anchor | sonic-rs strict for JSON output-plane matches, selected non-JSON oracle if structurally independent | May support admission only when the output plane matches and validation/equality occurs in the measured row. |
| Same-run flaw probe | sonic-rs lossy, permissive flags, unchecked APIs | Planning only; never strict admission. |
| Sidecar planning signal | historical simdjson, yyjson, asmjson, RapidJSON unless refreshed under same-run rules | Planning only until freshness, strictness, and output-plane rules are gate-consumed. |

For the W1/W2 non-JSON rows, the admission anchor is not a stale JSON
comparator. It is the same-run independent oracle or Track 2 named by the W1
plan, with strict output equality and provenance consumed by the gate.

### Section 0.3 - Outcome Enum

The SK-V12 packet uses the existing schema-v3 outcome set only:

```text
A
C
G
I
J
K
L
M
N-direct
S
```

No wave may add an outcome variant. `S` remains the explicit diagnostic
parse-only/non-admission spelling. `N-direct` remains the JSON direct residual
NO-GO spelling. W1/W2 may use an existing admitting outcome only if the gate
records strict non-JSON equality, provenance, and row-owned evidence.

### Section 0.4 - Required Telemetry

SK-V12 inherits the schema-v3 discipline already rendered in
`skinny/RESULTS.md` and may also use a companion gate-consumed report for
non-JSON rows. Required evidence for any new or refreshed row:

```text
row_id
grammar_id
domain
corpus_or_workload
output_plane
workload_class
outcome_id
verdict
strictness
measured_validation_path
track1_mbps
track2_or_oracle_mbps
track1_source_path
track2_or_oracle_source_path
track2_independence_status
strict_output_equality
generated_input_provenance
generated_runtime_path
run_id
host_triple
feature_mask
build_flags
sample_count
sample_cost
benchmark_artifact
baseline_row_id
baseline_track1_mbps
intervention_threshold_mbps
profile_artifact
wave_id
redress_entry
same_wave_consumer_class
scalar_reference_status
checkasm_or_parity_status
json_guard_state
fail_closed_gate_status
comparator_set
```

Every emitted field must be consumed by `gate-json` or the named non-JSON gate
in the same wave. Missing required fields, unsupported outcomes, stale run ids,
oracle coupling, Track 1/Track 2 dishonesty, parse-only SOTA claims, W3
reopen claims, direct digest as typed proof, generic JSON policy leakage, or
producer-only telemetry rejects the wave.

### Section 0.5 - Opening Row Goalset

Current SK-V12 seed state from `skinny/RESULTS.md` and SK-V12 Alpha:

| Family | Current state | SK-V12 posture |
|---|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | Diagnostic only; no row admission. |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | Guards plus REDRESS 119 pre-blocked residuals. |
| `real_typed_struct` | 7 `A / GO` | Product-plane guard surface. |
| generated non-JSON parser | none admitted | First material target. |
| overall | `N-direct / NoGo` | Seed outcome. |

W1 target candidates, in selection order:

| Selection | Candidate row | Baseline gate |
|---|---|---|
| 1 | `css_l4/declaration_values/direct/main` or typed equivalent | generated Track 1, independent oracle/Track 2, strict equality, finite positive Mbps |
| 2 | `sheets/formula/direct/main` or typed equivalent | same gate if CSS preflight cannot fit/pass |
| 3 | `bbnf_self/grammar/direct/main` or typed equivalent | same gate if CSS and Sheets cannot fit/pass |

W2 target is the exact W1 selected row, with Track 1 >=
`ceil(W1_baseline_track1_mbps * 1.01)`.

JSON direct guard floors:

| Row | Track 1 maintain | Track 2 maintain |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 18191 | 17431 |
| `apache_builds/direct_to_struct` | 11028 | 9996 |
| `marine_ik/direct_to_struct` | 8759 | 9248 |
| `unicode_basic/direct_to_struct` | 2253 | 2182 |

JSON typed guard floors:

| Row | Track 1 maintain | Track 2/oracle maintain |
|---|---:|---:|
| `twitter/real_typed_struct` | 17385 | 15593 |
| `citm_catalog/real_typed_struct` | 29928 | 17321 |
| `apache_builds/real_typed_struct` | 8308 | 6754 |
| `github_events/real_typed_struct` | 11633 | 12029 |
| `update_center/real_typed_struct` | 11613 | 10150 |
| `mesh/real_typed_struct` | 9214 | 7739 |
| `marine_ik/real_typed_struct` | 11552 | 9894 |

JSON direct residual reopen floors, only if W3 entry gate passes:

| Row | Track 1 | Track 2 | sonic direct | floor |
|---|---:|---:|---:|---:|
| `twitter/direct_to_struct` | 11613 | 10816 | 15113 | 13740 |
| `canada/direct_to_struct` | 10316 | 9819 | 11700 | 10637 |
| `github_events/direct_to_struct` | 11918 | 10596 | 14743 | 13403 |
| `update_center/direct_to_struct` | 8187 | 7474 | 11064 | 10059 |
| `mesh/direct_to_struct` | 8561 | 8652 | 9542 | 8675 |
| `random/direct_to_struct` | 7693 | 6949 | 8665 | 7878 |
| `gsoc-2018/direct_to_struct` | 2665 | 2578 | 4110 | 3737 |
| `instruments/direct_to_struct` | 11569 | 10736 | 9865 | 8969 |
| `numbers/direct_to_struct` | 4479 | 2366 | 2667 | 2425 |
| `unicode_mixed/direct_to_struct` | 3753 | 2427 | 2846 | 2588 |
| `unicode_escapes/direct_to_struct` | 1345 | 1341 | 3785 | 3441 |
| `distinct_values/direct_to_struct` | 1750 | 1625 | 2923 | 2658 |
| `y_string_unicode/direct_to_struct` | 1983 | 1029 | 4344 | 3950 |

## Section 1 - Non-Negotiables

- No behavior source change before W0 closes.
- No JSON-only direct wave before W1/W2 generated non-JSON priority admits,
  rejects, or blocks with measurement.
- No parse_only SOTA admission.
- No W3 union, class column, streaming cursor, retained structural vector,
  `UnionTape`, sidecar substrate, or W4-through-W3 cascade.
- No new BBNF directive, BIR variant, `BackendShape`, public substrate API, or
  parser-owned fact/scratch slot.
- No JSON policy in generic crates or shared runtime outside generated
  per-grammar modules.
- No x86 implementation target in SK-V12.
- No strict admission from permissive, lossy, stale, sidecar-only, historical,
  or output-plane-mismatched comparator evidence.
- No primitive, SIMD/ASM kernel, parse-that helper, generated path, or
  output-plane contract without scalar reference, parity/checkasm where
  applicable, micro-proof, and same-wave generated consumer.
- No checkasm-only, harness-only, report-only, or telemetry-only performance
  admission.
- Research, plan, CHALLENGE when required, redress, and close remain distinct.
- Every miss becomes REDRESS evidence or an explicit routed residual.
- No wave closes on "wired", "integrated", "future consumer", or any other
  future-phase promise.

## Section 2 - Wave Manifest, Caps, And Reruns

| Wave | Section | Name | Initial dispatch status | Source/edit LOC budget | Implementation/redress cap |
|---|---|---|---|---:|---:|
| W0 | Section 3 | Baseline Profile And Telemetry Lock | Dispatchable after S-P3 convergence | <=180 report/gate/test/doc LOC; 0 behavior LOC | <=90 min |
| W1 | Section 4 | Generated Non-JSON Baseline | Conditional on W0 close | <=520 CSS, <=480 Sheets, <=460 BBNF-self; generated output named separately | <=75 min |
| W2 | Section 5 | Selected-Baseline Measured Intervention | Conditional on W1 admit | <=430 source/test/gate LOC; generated output named separately | <=75 min |
| W3 | Section 6 | Conditional JSON Direct Companion | Conditional on W1/W2 disposition plus material reopen gate | <=300 source/test/gate LOC; 0 LOC if entry gate blocks | <=75 min |
| W4 | Section 7 | Close And Alpha Feedback | Conditional on W0-W3 dispositions | <=120 docs/report/gate reconciliation LOC; 0 behavior LOC | <=90 min |

Phase caps:

| Phase | Cap |
|---|---:|
| Research | 30 min per agent, max 6 agents |
| Plan | 30 min |
| CHALLENGE | 90 min when first-of-class, primitive, generic-crate, or high-risk |
| Redress | 75 min for W1-W3 behavior; 90 min for W0/W4 gate/docs |

Rerun ceilings:

| Wave | Focused verification | Rerun ceiling |
|---|---|---|
| W0 | gate/report tests, replay manifest checks, unchanged JSON row surface | one gate refresh plus one confirm rerun if variance invalidates lock |
| W1 | generated non-JSON compile/equality/oracle/bench gate and JSON guard maintain | one full selected-baseline gate refresh |
| W2 | selected intervention microbench, scalar/parity/checkasm, baseline delta, JSON guard maintain | one focused microbench plus one full selected-row gate refresh |
| W3 | material reopen proof, strict direct row gate, independent Track 2, JSON guard maintain | one focused probe plus one full gate refresh; no retry if entry evidence is absent |
| W4 | close checklist and document reconciliation | no performance rerun unless a source/report mismatch is found |

### Section 2.1 - Generality And Lock 14 Gate

Every wave has this exit gate, with extra checks when generic crates are
edited:

- Public API scan: no new public JSON-named API appears in generic crates.
- Grammar branch scan: no generic branch selects behavior by JSON grammar name,
  corpus name, object/array role, field name, string role, or layout role.
- Primitive/table scan: generic byte sets, tables, masks, digit runs, and
  escape policies are supplied by generated grammar metadata or caller policy.
- Runtime boundary: generated per-grammar modules own syntax, escape policy,
  number policy, output projection, and host declarations.
- Non-JSON proof: the selected CSS L4, Sheets, or BBNF-self generated row
  compiles, runs, and passes strict oracle equality for any generic codegen,
  runtime, parse-that, or bbnf-simd edit.

Allowed grammar-specific surfaces are grammar input files, generated
per-grammar output, per-grammar providers/templates, tests, fixtures, and
host/API schema facts. Generic code consumes grammar-derived facts, not
hard-coded JSON policy under neutral names.

### Section 2.2 - Micro-Prove, Scalar Reference, And Checkasm Gate

No W2 or W3 primitive reaches redress without:

1. executable scalar reference;
2. same-host isolated microbench proving the selected caller-local movement;
3. checkasm/parity for native SIMD/ASM bodies under strict mode;
4. generated same-wave consumer in the selected row;
5. strict output equality and independent oracle/Track 2 proof;
6. REDRESS pre-block citation and material differential if adjacent to a
   rejected route.

Support-only primitives may land only as part of the same consumer commit. An
orphan primitive is a wave rejection.

## Section 3 - W0 Baseline Profile And Telemetry Lock

Owner paths:

- `skinny/crates/bbnf-bench/`
- `skinny/xtask/src/`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` only if W0 rejects
- `restart/skinny/tranches/sk-v12/research/` using the wave W0 naming pattern

Entry gate:

- S-P3 converged and the orchestrator dispatches W0.
- S-P1 convergence authority exists at
  `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`.
- No behavior source change is staged.

Tasks:

1. Bind the SK-V12-open source baseline `50bd1648`, capture root
   `/tmp/skv12-p1`, replay TSV, and self-time TSVs into the gate/report
   surface.
2. Verify current `skinny/RESULTS.md` remains the SK-V11 close result surface:
   17 parse diagnostic rows, 4 direct `A / GO`, 13 direct `N-direct / NO-GO`,
   and 7 typed `A / GO`.
3. Make the non-JSON gate/report lane reject producer-only fields, stale run
   ids, oracle coupling, admission claims without generated Track 1, and
   generic JSON policy leaks.
4. Preserve all JSON row outcomes and guard floors.
5. Record the W0 REDRESS entry only if the lock fails.

Exit gate `G-W0-SK-V12-OPEN`:

- All 41 current JSON main rows keep opening outcomes and no behavior drift.
- The SK-V12-open profile paths and replay authority are gate-consumed.
- The non-JSON report/gate lane is available for W1 generated baseline
  evidence and rejects malformed/coupled/producers-only evidence.
- No parser, scanner, SIMD/ASM, codegen behavior, generated runtime output, or
  benchmark body changes.

Same-wave consumer: `gate-json` and the non-JSON gate consume every emitted
telemetry/report field.

Pre-blocked routes: all behavior changes, JSON row movement, generated
non-JSON admission by report fixture alone, and stale sidecars as strict
anchors.

Revert protocol: revert W0 gate/report/RESULTS changes together, restore the
opening report surface, and add REDRESS naming the missing field, stale run id,
or malformed gate.

Downstream effect: W0 rejection blocks W1-W4.

## Section 4 - W1 Generated Non-JSON Baseline

Owner paths:

- `skinny/crates/codegen/`
- `skinny/crates/runtime/src/lib.rs`
- `skinny/crates/runtime/src/grammars/`
- `skinny/crates/bbnf-bench/`
- selected non-JSON fixtures under the W1 research/output directory
- `skinny/RESULTS.md` only if W1 deliberately renders the admitted row there
- `skinny/REDRESS.md` if admitted or rejected

Entry gate:

- W0 admitted.
- W1 plan selects exactly one target in this order: CSS L4 declaration values,
  Sheets formula, BBNF-self grammar.
- If the plan skips an earlier target, it cites a concrete preflight failure
  inside the W1 owner surface.
- The plan names generated Track 1 path, runtime module path, fixture corpus,
  independent oracle/Track 2 path, strict equality command, gate command, and
  rollback slice.

Tasks:

1. Stand up one generated non-JSON direct or typed parser baseline.
2. Break the JSON-provider-only emission blocker without adding generic JSON
   policy or a new directive/BIR/backend shape.
3. Build the generated runtime module for the selected grammar.
4. Add a fixture corpus and independent oracle/Track 2 for the same output
   plane.
5. Measure finite positive Track 1 and oracle/Track 2 Mbps.
6. Gate-consume generated provenance, equality, run/build/host/sample telemetry,
   and JSON guard state.

Exit gate `G-W1-GENERATED-NONJSON-BASELINE`:

- Exactly one selected non-JSON row is admitted with generated Track 1 and
  independent oracle/Track 2 evidence.
- Track 1 Mbps > 0 and oracle/Track 2 Mbps > 0.
- Strict output equality passes.
- The non-JSON gate consumes every required Section 0.4 field.
- The selected generated runtime compiles from grammar facts.
- All JSON guard floors in Section 0.5 hold if JSON results are refreshed.
- Lock 14 and Section 2.1 pass.

Same-wave consumer: the selected generated parser row and its oracle/Track 2
consumer.

Pre-blocked routes: REDRESS 111 report fixture as baseline, REDRESS 112/113
future-phase promise, hand-only non-JSON parser, stale `sheets_witness`, JSON
provider cloning under a neutral name, generic JSON policy, directive/BIR
additions, and source-only baseline claims without measured Mbps.

Revert protocol: revert codegen/runtime/bench/report/gate/RESULTS changes and
generated files for the selected grammar as one slice, save
`/tmp/skv12-waveW1-rejected.patch`, and add REDRESS with the failed preflight
or measurement.

Downstream effect: W1 admission unblocks W2. W1 rejection or measured block
routes W2 and W3 to close/reroute unless CHALLENGE accepts a split baseline
plan inside the bracket cap.

## Section 5 - W2 Selected-Baseline Measured Intervention

Owner paths:

- exact owner paths named by the W2 plan
- likely families: `skinny/crates/parse-that-regex/`,
  `skinny/crates/bbnf-simd/`, `skinny/crates/codegen/`,
  `skinny/crates/runtime/src/grammars/<selected>/`,
  `skinny/crates/bbnf-bench/`
- selected generated output and fixtures named by the plan
- `skinny/RESULTS.md` if W2 renders the row there
- `skinny/REDRESS.md` if admitted or rejected

Entry gate:

- W1 admitted a selected baseline row and recorded `W1_baseline_track1_mbps`.
- W2 plan selects exactly one S-P2-surviving intervention family tied to the
  selected baseline hot leaf: byte-set/classifier/run-skip, bounded string
  span, escape/hex segment decode, digit-run span, layout/trivia skip, or
  generated FIRST/prefix/lookahead dispatch.
- The plan includes scalar reference, microbench, parity/checkasm where
  applicable, same-wave generated consumer, strict oracle equality, and guard
  floors.
- Mandatory CHALLENGE accepts the plan.

Tasks:

1. Implement the selected intervention only for the selected baseline consumer
   and legal support helpers.
2. Keep grammar policy in generated code or caller-owned policy.
3. Run microbench first; abort and record REDRESS if the caller-local movement
   is not positive or equality fails.
4. Measure the selected baseline row against W1.
5. Preserve JSON guard floors if results are refreshed.

Exit gate `G-W2-SELECTED-NONJSON-INTERVENTION`:

- Selected row Track 1 Mbps >= `ceil(W1_baseline_track1_mbps * 1.01)`.
- Oracle/Track 2 remains finite, independent, and strict-equal.
- Every primitive has scalar reference and strict parity/checkasm where
  applicable.
- The same-wave generated consumer appears in the sampled/profiled path or
  focused proof for the selected row.
- All required telemetry is gate-consumed.
- Lock 14 and Section 2.1 pass.

Same-wave consumer: the selected generated parser/direct/typed row consuming
the intervention.

Pre-blocked routes: orphan kernels, proof-only string/hex/digit/mask helpers,
JSON-only direct residual patches, decoded-byte sidecars, numeric slot reuse,
container-tail replay, output digest host-sink replay, retained masks, and x86
implementation work.

Revert protocol: revert the intervention, generated output, tests, gate/report,
RESULTS, and REDRESS changes as one slice, save
`/tmp/skv12-waveW2-rejected.patch`, and preserve W1 baseline evidence.

Downstream effect: W2 admission satisfies the material SK-V12 target. W2
rejection still closes honestly if W1 admitted and the measured failure is
recorded.

## Section 6 - W3 Conditional JSON Direct Companion

Owner paths:

- no default behavior owner paths.
- If entry gate passes, the W3 plan must name exact JSON owner paths and may
  not include substrate, parse_only, or W3 union families.
- `skinny/REDRESS.md` for routed block/admit/reject evidence.
- `skinny/RESULTS.md` only if a row movement is admitted by gate.

Entry gate:

- W1 and W2 have admitted, rejected, or routed.
- The non-JSON priority has succeeded or is explicitly measured-blocked.
- The W3 plan names one REDRESS 119 residual direct row, fresh material
  evidence beyond REDRESS 114-119, scalar/oracle proof, same-host microbench,
  independent Track 2, strict sonic direct floor, same-wave gate consumer, and
  owner paths.
- Mandatory CHALLENGE accepts the material differential.

Tasks:

1. If the entry gate fails, record a routed W3 block with no source edit.
2. If the entry gate passes, implement exactly one direct companion
   intervention against one selected residual row.
3. Preserve all direct and typed guard rows.
4. Keep parse_only diagnostic and substrate routes closed.

Exit gate `G-W3-CONDITIONAL-JSON-COMPANION`:

- If behavior dispatches, selected row Track 1 and Track 2 both clear the
  Section 0.5 floor and strict same-run sonic direct evidence is consumed.
- If no behavior dispatches, W3 records why no current S-P2 candidate passes
  material reopen and moves no source/RESULTS row.
- All guard floors hold.
- No pre-blocked route reopens.

Same-wave consumer: selected direct row Track 1 plus independent Track 2, only
if behavior dispatches.

Pre-blocked routes: REDRESS 96/97/98 W3 substrate family, REDRESS 114 numeric
slot, REDRESS 115 container-tail, REDRESS 116 bounded string, REDRESS 117
escaped segment, REDRESS 118 digest host-sink, REDRESS 119 fixpoint rows,
parse_only SOTA, W0-clamped docs-only admission, sidecars, and generic JSON
policy.

Revert protocol: default is no source edit. If behavior dispatches and fails,
revert the W3 source/generated/bench/gate/RESULTS slice, save
`/tmp/skv12-waveW3-rejected.patch`, and add REDRESS with row evidence.

Downstream effect: W3 disposition feeds W4 close.

## Section 7 - W4 Close And Alpha Feedback

Owner paths:

- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/research/close/`
- `skinny/REDRESS.md` only if close reconciliation needs an entry
- `skinny/RESULTS.md` only if reconciling a documented report mismatch without
  behavior change

Entry gate:

- W0-W3 each admitted, rejected, or routed with REDRESS evidence.

Tasks:

1. Reconcile every wave disposition.
2. Ensure RESULTS, REDRESS, SYNTHESIS, HANDOFF, SPEC, and DISPATCH-PROMPT agree.
3. Present G-Alpha facts for SK-V12 -> SK-V13.
4. Route residual JSON direct, parse_only, and totality/Lock 14 lessons.

Exit gate `G-W4-CLOSE`:

- Every wave disposition is recorded.
- W1/W2 success or W1/W2 measured block is stated without paper-close language.
- JSON guard rows preserve their state or any demotion is explicitly measured.
- No accepted source change lacks profile, row threshold, scalar/parity proof,
  Lock 14 proof, same-wave consumer, and REDRESS id.
- G-Alpha can be presented.

Same-wave consumer: close checklist and document reconciliation.

Pre-blocked routes: paper close, missing REDRESS, missing RESULTS/report rows,
dropping guard rows, claiming grammar generalization by prose, direct digest as
typed proof, parse_only admission, and omitting the W1/W2 measured outcome.

Revert protocol: no source revert by default. Reopen the producing wave or mark
close blocked with exact files, rows, and missing evidence.

## Section 8 - Pre-Blocked Routes

Every wave inherits this route ledger. A route may reopen only with fresh
SK-V12 profile evidence, material differential, scalar/reference proof,
same-wave consumer, no-regression gate, REDRESS citation, and CHALLENGE
acceptance.

Global blocks:

- New directive, BIR variant, `BackendShape`, substrate surface, `UnionTape`,
  public substrate API, parser-owned cursor/facts, sidecar substrate, and
  parallel substrate.
- Generic JSON policy in generic crates, including renamed helper policy.
- Sidecar/permissive/lossy/stale comparator evidence as strict admission.
- `parse_only` or telemetry rows as production row movement.
- Orphan primitives, checkasm-only admission, harness-only hardening, and
  microbench-only proof as performance admission.
- Track 1/Track 2 coupling or benchmark-private parsers.
- x86 implementation work.

Specific REDRESS blocks:

- REDRESS 28/33/72: TBL/tiny-string correctness and cap-16 routes as retained
  parse/direct closes.
- REDRESS 36-38 and 85-86: Lock 14 residue, old JSON helpers, generic JSON
  branches, and renamed JSON policy.
- REDRESS 50/51/53: side tables, byte-class whitespace/event cursors, and
  parser-local structural-mask cursors.
- REDRESS 54/55/60-69/72/82/83/116/117: decoded-string, string-boundary,
  eager materialization, unicode/string/object metadata, bounded span, and
  escaped-segment routes without legal generated consumers.
- REDRESS 80/114: numeric fallback, mantissa widening, and JSON numeric slot
  reuse as direct row movement.
- REDRESS 88/89/90: PMULL prefix-XOR default body, CSSC CTZ bulk consumer, and
  canary hardening as row movement.
- REDRESS 96/97/98: W3 union, class-column, streaming cursor, and substrate
  ceiling routes.
- REDRESS 111: non-JSON report lane as generated baseline.
- REDRESS 112/113: generated non-JSON baseline blocker and intervention entry
  block as future-phase promise.
- REDRESS 115: container-tail direct dispatch replay.
- REDRESS 118: output digest/hash host-sink as parser/direct proof.
- REDRESS 119/120: direct residual fixpoint and SK-V11 close route.

## Section 9 - G-Alpha And Dispatch Scope

This draft presents the intended SK-V12 dispatch scope:

- W0 is first after S-P3 convergence.
- W1 is the first behavior wave and must create exactly one generated non-JSON
  baseline or record a measured block.
- W2 consumes W1's baseline and must measure a >=1% same-row lift or record a
  measured reject.
- W3 is conditional and defaults to routed block unless the material JSON
  direct reopen gate passes.
- W4 reconciles close and presents G-Alpha.

No SK-V12 implementation dispatches from S-P3 alone. No wave may start unless
its entry gate passes. No close occurs without measured W1/W2 disposition.
