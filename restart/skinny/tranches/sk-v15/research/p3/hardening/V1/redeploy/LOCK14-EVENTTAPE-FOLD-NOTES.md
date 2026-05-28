# SK-V15 S-P3 V2 Fold Notes: Lock 14 / EventTape / CH5-CH7

Owned output: this file only.
Date: 2026-05-28.
Role: redeployed fold support for hidden-coupling and overfit-prune gates.
Input files read: `p3b-wave-sequencing.md`, `p3c-falsifiability-gates.md`, `p3d-telemetry-schema.md`, `SPEC.md`, `DISPATCH-PROMPT.md`, `hardening/V1/CH5.md`, and `hardening/V1/CH7.md`.
CH2 status: `hardening/V1/CH2.md` was absent at read time.

## 1. Required V2 Fold Summary

S-P3 V2 should fold four executable changes before another CH5/CH7 pass:

1. Replace P3-B's W0 telemetry aliases with the exact P3-D/SPEC ten-field vocabulary.
2. Make the CH5 forbidden set load-bearing in P3-C, SPEC, and DISPATCH, not only in P3-E or prose.
3. Counter-bind W7 `EventTape` as an existing BackendShape lowerer, not a sidecar event vector or new substrate.
4. Promote Lock 14 / Lock 16 exclusion reporting into a gate-consumed table used by W2/W3/W4/W6/W7/W8.

The current evidence is:

| Surface | Current problem | Fold target |
|---|---|---|
| P3-B W0 | Uses aliases: `sample_count`, `row_claim_scope`, `comparator_workload_id`, `producer_path`, `generator_source_id`, `semantic_output_kind`, `strictness_source`. | Use only P3-D/SPEC fields or explicitly bump schema and require the gate to consume the mapping. |
| P3-C | Still uses the older W0 plus PRUNE/REBUILD names and carries strong primitive reject language but not W7-specific EventTape anti-sidecar wording. | Reindex to W0-W9 and add the W7 EventTape paragraph below. |
| SPEC Section 13 | Narrower than CH5: it blocks retained sidecar tables, cursor streams, class columns, public `UnionTape`, and second tape, but omits the full hidden-coupling vocabulary. | Add the full forbidden set and make it fail-close language for all affected waves. |
| DISPATCH | Says gates consume reports but does not carry the report schema, alias rejection, or EventTape anti-sidecar text. | Add the Lock 14 / Lock 16 report table and W7 paragraph as dispatch prerequisites. |

## 2. Canonical Telemetry Vocabulary

P3-B W0 must use the exact ten SK-V15 fields from P3-D/SPEC:

```text
measurement_row_id
measurement_origin
value_plane
css_comparator_workload
generator_source
lock14_scan_scope
lock16_status
checkasm_or_parity_status
gate_exclusion_report
broadcast_group_id
```

Fold language:

```text
P3-B W0 must not substitute aliases for the ten SK-V15 fields. Alias-only
telemetry rejects as producer-only telemetry. A renamed field is legal only
after an explicit schema-version bump that maps the alias to one of the ten
canonical fields and after the gate consumes that mapping before verdict
calculation.
```

Alias rejection table:

| Alias seen in V1 | Required canonical field | Disposition |
|---|---|---|
| `sample_count` | Carrier metadata inside `measurement_origin` / hidden signature check | Reject if used as a substitute for `measurement_origin` or broadcast detection. |
| `row_claim_scope` | `broadcast_group_id` plus row verdict/status | Reject if it allows multiple admits from one timing row. |
| `comparator_workload_id` | `css_comparator_workload` | Reject if it does not name the CSS comparator output plane. |
| `producer_path` | `measurement_origin` and `generator_source` as applicable | Reject if it is not split into timing provenance and generator provenance. |
| `generator_source_id` | `generator_source` | Reject unless it preserves the P3-D rejection rules for hand-written CSS tokenizers and missing Pattern H provenance. |
| `semantic_output_kind` | `value_plane` | Reject if it permits fact-stream, full-parse summary, or brace-counter output to close CSS typed Value. |
| `strictness_source` | SK-V8 strictness fields plus `checkasm_or_parity_status` where relevant | Reject if it hides parity/checkasm or comparator-plane proof. |

## 3. Full CH5 Forbidden Vocabulary

S-P3 V2 should carry this set verbatim into SPEC Section 1, SPEC Section 13, P3-C primitive rejection rules, and DISPATCH challenge/redress prerequisites:

```text
parser-owned structural projection
retained cursor
retained cursor list
retained sidecar table
aux density table
aux projection table
sidecar event vector
parallel source pass
second tape
public UnionTape
retained class column
retained class stream
retained structural stream
retained cursor stream
whitespace bitmap
schema-shaped builder
harness hash
Track 1 == Track 2 sidecar
new substrate API
new BackendShape
sixth BackendShape
alternate document projection
production FNV arbiter
production hash correctness proof
```

Fold language:

```text
Any wave that introduces, retains, or renames one of the forbidden hidden
coupling forms rejects unless the same wave deletes it, scalar-delegates it,
or routes it to REDRESS with intrinsic-block proof. A gate-only rename does
not clear the block. A new API or shape that carries equivalent retained
state is treated as the blocked form.
```

## 4. EventTape Anti-Sidecar Wording

W7 is allowed to implement the existing `EventTape` BackendShape lowerer. It is not allowed to create a sidecar event vector. Add this wording to P3-C W7, SPEC Section 10, and DISPATCH W7:

```text
EventTape is one of the existing five BackendShape lowerers:
`EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, and `CollapsedStage`.
The W7 EventTape task may only lower selected grammar facts into the accepted
generated runtime substrate or emit a gate-consumed rejected alternative. It
must not create a retained sidecar event vector, parser-owned event stream,
parallel event tape, public substrate API, alternate document projection,
sixth BackendShape, or any retained event/class/cursor stream under the
EventTape name.
```

W7 close should require:

| Requirement | Rejects when |
|---|---|
| Existing-shape proof | The generated output names a new `BackendShape` variant or public substrate API. |
| No sidecar event vector | Any event list/vector/table is retained outside the accepted runtime substrate. |
| No parser-owned stream | Parser state owns events after parse rather than emitting through generated runtime output. |
| Runtime-relevant diff | The lowerer only emits labels, placeholders, pass-through shells, or documentation. |
| Non-JSON receiver | Only JSON fixtures prove the generic lowerer path. |

## 5. Lock 14 / Lock 16 Exclusion Report

Promote this table into SPEC and DISPATCH. The gate must consume it before any admission verdict, and producer-only reports reject.

| Field | Required value | Rejects when |
|---|---|---|
| `gate_name` | `lock14`, `lock16`, or combined gate id. | Empty or not parsed by the gate. |
| `scan_kind` | `generic-cleanliness`, `generated-provenance`, `simd-primitive`, `checkasm`, `lowerer`, or `fnv-quarantine`. | Vague labels such as `misc` or `manual`. |
| `included_roots` | Exact roots scanned. | Roots omit known leak paths, generated roots, validator code, or files under test. |
| `excluded_roots` | Exact paths excluded, or `none:full-surface-scan`. | Empty, hidden, glob-only, or missing exclusions. |
| `exclusion_reasons` | One reason per excluded path. | Reason is absent, circular, or says the file is trusted because it is the gate. |
| `exclusion_owner` | Wave or artifact owner for each exclusion. | Owner is absent or points to the file being exempted. |
| `self_scan_status` | Proof that the exclusion list and validator files were scanned or intentionally failed as non-admission. | Validator, report parser, scan root list, checkasm target, or files under test are self-exempted. |
| `lock16_primitive_status` | `not-applicable`, `scalar-only`, `simd-claimed`, `asm-claimed`, `source-present-unwired`, `deleted`, or `architectural-block-with-redress`. | `simd-claimed` or `asm-claimed` lacks M5 Max / aarch64 parity; `source-present-unwired` closes without deletion, scalar delegate, or REDRESS. |
| `gate_consumer` | Exact command or gate module that reads the report. | Report is written but not consumed before verdict calculation. |
| `affected_rows` | Row ids or `n/a:gate-only`. | Admission rows affected by missing coverage still close. |
| `disposition` | `admit`, `diagnostic-only`, `fail-closed`, `redress`, or `intrinsic-block`. | Exclusions are reported but do not affect verdict. |

Per-wave report consumers:

| Wave | Required report use |
|---|---|
| W2 | Lock 14 / Lock 16 repair is not closed until scan roots, exclusions, and primitive statuses are printed and consumed by the gate. |
| W3 | Generic codegen cleanliness claims must cite the W2 report plus a W3 leak scan over codegen, passes, providers, runtime generator, JSON templates, and CSS provider surfaces. |
| W4 | Pattern H provenance checks must report any generated root, root runtime file, or regen/check path excluded from the 67-file proof. |
| W6 | Decision Engine facts and CSP tests must report excluded generated fixtures, fact roots, and selection/lowering test inputs. |
| W7 | BackendShape lowerer proof must report excluded lowerer files, generated fixtures, and EventTape-specific anti-sidecar scan roots. |
| W8 | FNV quarantine must report production runtime scan roots and any skipped bench/xtask helper that can influence correctness or selection. |

## 6. Non-JSON Proof Receivers For Generic Crates

Generic crate cleanup cannot close on JSON-only proof. Each generic edit must name at least one non-JSON receiver, and any edit that touches generation, selection, lowerers, or parser helpers should prefer two non-JSON receivers.

| Generic surface | Minimum non-JSON receivers | Required proof shape |
|---|---|---|
| `skinny/crates/codegen/src/grammar_provider.rs` | CSS L4 plus Google Sheets or BBNF-self | Generated provider output changes or stays byte-identical under a command that exercises both receivers; no JSON/CSS branch remains in generic code. |
| `skinny/crates/codegen/src/runtime_generator.rs` | CSS L4 typed value and BBNF-self runtime projection | Generated output or rejection fixture proves grammar metadata drives behavior, not embedded JSON/CSS recognizers. |
| `skinny/crates/codegen/src/lower/*.rs` | CSS L4 and Google Sheets, or BBNF-self if Sheets is not available | Each lowerer emits runtime-relevant output or a gate-consumed rejected alternative for a non-JSON grammar. |
| `skinny/crates/passes/src/backend_egraph.rs` | CSS L4 and BBNF-self | At least one asserted rewrite changes selected plan or generated behavior for a non-JSON receiver. |
| `skinny/crates/passes/src/decision_csp.rs` | CSS L4 and Google Sheets or BBNF-self | Removing a required non-JSON fact changes satisfiability or selected plan. |
| `skinny/crates/ir/src/cost.rs` | CSS L4 plus any one of CSV, Math, Google Sheets, or BBNF-self | CostFacts remain grammar-neutral and do not encode JSON structural roles or CSS profile names. |
| `skinny/xtask/src/main.rs` regen paths | CSS L4 plus BBNF-self or Google Sheets | Regen/check command proves no per-grammar enum fanout or static CSS roster is needed for generic dispatch. |
| `skinny/crates/bbnf-bench/src/bin/gate.rs` and `report.rs` | CSS L4 plus one non-CSS/non-JSON grammar where rows exist; otherwise explicit `n/a:no-current-row` plus REDRESS | Gate consumes schema, exclusion report, and same-wave proof without hardcoding JSON or CSS row exceptions. |

Receiver priority for SK-V15:

1. CSS L4, because it is the active contrivance-prune and typed Value rebuild receiver.
2. BBNF-self, because it proves generic grammar machinery is not JSON-shaped or CSS-shaped.
3. Google Sheets, because it stresses structured document/value output beyond JSON.
4. CSV or Math, only when the edited surface actually affects those grammar paths and an executable receiver exists.

If a non-JSON receiver does not exist for a touched generic surface, the wave must say so as an intrinsic block or split the work. It must not close by saying JSON stayed green.

## 7. CSS Broadcast Metrics Placement

CH7 requires moving W8R numbers out of live CSS typed-admission floors. S-P3 V2 should keep `2319.041`, `2362.037`, and `929.281` only as diagnostic negative fixtures until W5 captures fresh same-run typed-output comparator data.

Fold language:

```text
The W8R CSS numbers are negative fixtures for broadcast detection. They are
not typed CSS Value floors. W5 must first emit typed CSS value/document output
and then capture fresh same-run `cssparser` typed-value or typed-document
comparator data. A CSS admit may cite only that fresh same-plane run.
```

## 8. Close Test For This Fold

S-P3 V2 CH5/CH7 can accept this fold only if all are true:

| Gate | Acceptance condition |
|---|---|
| Telemetry | P3-B, P3-D, SPEC, and DISPATCH use one canonical vocabulary or a gate-consumed schema bump. |
| Hidden coupling | SPEC and DISPATCH carry the full forbidden vocabulary as fail-close language. |
| EventTape | P3-C, SPEC, and DISPATCH explicitly forbid sidecar event vectors and sixth-shape/API interpretations. |
| Lock 14 / Lock 16 | SPEC and DISPATCH include the report schema and require gate consumption before verdict. |
| Generic proof | Generic crate waves name non-JSON receivers and reject JSON-only close evidence. |
| CSS floors | W8R broadcast metrics appear only as diagnostic negative fixtures, not typed-admission floors. |
