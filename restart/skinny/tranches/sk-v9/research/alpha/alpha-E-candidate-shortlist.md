# SK-V9 Alpha-E Candidate Shortlist

Date: 2026-05-18.

Scope: shortlist only for PASS-ALPHA SK-V8 -> SK-V9. This file dispatches no
SK-V9 implementation waves and changes no source, generated output, benchmark
data, `skinny/RESULTS.md`, or `skinny/REDRESS.md`.

Inputs read:

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/skinny/tranches/sk-v8/research/skv8-W6-close-and-alpha-feedback.md`
- `restart/skinny/tranches/sk-v8/research/skv8-W6-close-reconciliation-research.md`
- SK-V8 W2/W3/W4/W5 research, plan, redress, and hardening artifacts
- `restart/skinny/tranches/sk-v8/research/wave-0-telemetry-gate-research.md`
- `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v8/research/skv8-W1-c-comparator-admission.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Baseline facts carried forward:

- `skinny/RESULTS.md` remains the SK-V8 W0 report authority with 38
  `SK-V8-open` manifest rows and four measured `real_typed_struct A / GO`
  rows: `twitter`, `update_center`, `mesh`, and `marine_ik`
  (`skinny/RESULTS.md:3-40`).
- W2 admitted only Apache/CITM source/product typed parity. Apache/CITM are not
  measured `RESULTS.md` real-typed rows yet (`skinny/REDRESS.md:2620-2659`).
- W3 was rejected/routed before source because scanner structural positions and
  retained `ValueRef` tape events are not isomorphic
  (`skinny/REDRESS.md:2661-2690`).
- W4 was rejected/routed after the hand Track 2 scalar-parent fold failed the
  selected native Criterion row gate (`skinny/REDRESS.md:2692-2729`).
- W5 admitted only the named Lock 14 provider-boundary cleanup and makes no
  performance or row-table claim
  (`restart/skinny/tranches/sk-v8/HANDOFF.md:235-248`).

Candidate-local admission boundary:

- No candidate may present a new row as strict SOTA admission while bbnf-side
  validation remains `Strictness=deferred` or `parse_utf8=view-boundary`.
  If an accepted future wave does not change the measured validation path and
  gate semantics, the row signal must remain a non-strict product/guard signal.
- Every candidate gate must render `Strictness`, `parse_utf8`,
  `escape_complete`, output plane, comparator id, comparator strictness,
  comparator freshness, and measured validation path in the row telemetry.
- Generic CostFacts, codegen, runtime, SIMD, tape, parser-template, report, or
  gate edits require Lock 14 proof: public API scan, grammar branch scan,
  primitive/table scan, role/fact boundary check, template/provider boundary
  check, and CSS L4 / Sheets / BBNF-self non-JSON compile/lower/cost/run proof.

## 1. Typed measured row-table admission for Apache/CITM

Reason to shortlist: W2 already admitted `apache_builds/real_typed_struct` and
`citm_catalog/real_typed_struct` as source/product parity rows, but rejected
benchmark row-table admission because the W0 run-id/metadata validator failed
closed on existing Criterion drift. SK-V9 should admit these rows only through a
fresh measured row-table tranche, not by reinterpreting W2 source evidence.

Owner paths:

- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/metadata.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs` as frozen generated
  product surface unless the wave explicitly re-runs `regen-real-typed`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md` only if the checked gate accepts the refreshed rows
- `skinny/REDRESS.md` only if a row rejects or routes

Scalar-reference status:

- Available through existing generated Track 1 DirectBuild, serde_json-backed
  Track 2/oracle parity, and the separate sonic-rs typed checksum lane.
- `canada/real_typed_struct` remains rejected/routed from W2 because generated
  DirectBuild and serde diverged on long decimal coordinate payloads.

Checkasm status:

- Not applicable. No SIMD or assembly primitive is in scope.

Same-wave consumer plan:

- `gate-json` must consume the Apache/CITM metadata requirements in the same
  wave that renders or admits the rows.
- A `RESULTS.md` edit is allowed only after same-run Criterion rows exist for
  Track 1, Track 2/serde oracle, sonic-rs typed, and serde_json typed.
- The source/product fixture map alone must not make the W0/SK-V9 report gate
  require unadmitted Criterion rows.

Falsifiability gates:

- `typed_row_admission_apache_citm`: `json/apache_builds/real_typed_struct/main`
  and `json/citm_catalog/real_typed_struct/main` may enter `RESULTS.md` only
  with full same-run metadata, sample count 100, matching input hash/bytes,
  checksum parity across generated Track 1, serde_json Track 2/oracle, and
  sonic-rs typed, and selected comparator `sonic_rs_strict` on the
  `typed direct` plane.
- `typed_floor_apache_citm`: each new row must be `A / GO`: generated Track 1
  >= `sonic_rs_real_typed_struct / 1.10`. Track 2 remains a structural oracle
  and must preserve checksum parity and coherent metadata rather than being used
  as a SOTA speed floor.
- `typed_existing_go_maintain`: current measured typed rows remain GO under the
  master maintain floors: `twitter` Track 1 >= 15027 Mbps, `update_center`
  Track 1 >= 11719 Mbps, `mesh` Track 1 >= 9431 Mbps, and `marine_ik`
  Track 1 >= 11548 Mbps. Their Track 2/oracle rows must stay checksum-correct
  and no worse than -2.0% throughput versus the SK-V8-open values.
- `typed_strict_boundary`: Apache/CITM may not be presented as strict SOTA
  admissions unless the same accepted wave supplies strict bbnf-side validation
  or a measured validation path accepted by the strict comparator gate. Without
  that, any admitted rows remain explicitly non-strict product-plane rows.
- `typed_no_source_only_admission`: if the checked report path rejects run-id,
  mixed-capture, missing metadata, or missing selected comparator evidence,
  `skinny/RESULTS.md` remains unchanged and the wave records rejection/routing.

LOC budget:

- Up to 300 source/test LOC plus a `RESULTS.md` refresh if admitted. Generated
  output churn is allowed only if byte-diff audited and tied to `regen-real-typed`.

Risk:

- Medium. The source/product code exists, but the admission path failed before
  on run-id/metadata validation. The main risk is weakening the validator to get
  rows instead of fixing measurement provenance.

Pre-blocked route notes:

- Do not admit Apache/CITM as measured rows from W2 source evidence alone.
- Do not reopen `canada/real_typed_struct` without a fresh decimal-coordinate
  parity proof.
- Do not turn direct digest rows into typed product proof.
- Do not weaken W0/SK-V9 run-id drift checks to bypass the known row-table
  admission blocker.

## 2. Retained class/event grammar plus `ValueRef` proof

Reason to shortlist: W3 found the right architectural direction but proved the
current slice was too large. The scanner emits structural punctuation plus real
quotes; the retained tape is a generated parser event stream containing
container opens/closes, opening quotes, number starts, and literal starts.
`ValueRef` traversal depends on the retained event stream. SK-V9 needs a proof
tranche before any renewed structural-heavy parse implementation wave.

Owner paths:

- `skinny/crates/bbnf-simd/src/lib.rs`
- `skinny/crates/runtime/src/grammars/json/scan.rs`
- `skinny/crates/runtime/src/tape/mod.rs`
- `skinny/crates/runtime/src/tape/assembler.rs`
- `skinny/crates/runtime/src/tape/offsets.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/runtime/src/grammars/json/parser.rs`
- `skinny/crates/runtime/src/grammars/json/view.rs`
- `skinny/crates/runtime/src/grammars/json/value.rs`
- `skinny/crates/codegen/src/json_templates/generated.rs`
- `skinny/crates/codegen/src/json_templates/parser.rs`
- `skinny/crates/codegen/src/json_templates/view.rs`
- `skinny/crates/codegen/src/json_templates/value.rs`
- `skinny/crates/bbnf-bench/src/parity.rs`
- `skinny/crates/bbnf-bench/src/materialization.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`

Scalar-reference status:

- The current generated retained parser and sealed offset tape are the scalar
  reference. The proof must show any proposed retained class/event grammar can
  reproduce current `ValueRef` semantics before production parser wiring.

Checkasm status:

- Not applicable for the proof tranche. If a later implementation changes SIMD
  class emission, that later wave needs scalar reference parity and checkasm
  before runtime consumption.

Same-wave consumer plan:

- The same wave must include the `ValueRef` cursor proof as the consumer of the
  class/event grammar. A producer-only `StructuralIndex` or `tape_vs_tape`
  comparison is not sufficient.
- No measured parse row can be admitted from this candidate unless generated
  retained Track 1 parsing consumes the retained grammar in the same wave.

Falsifiability gates:

- `retained_event_grammar_isomorphic`: the grammar must account for container
  opens/closes, opening string quotes, string quote ownership, number starts,
  literal starts, object keys, array values, and nesting without a second tape,
  sidecar, `UnionTape`, new `BackendShape`, BIR variant, directive, or public
  substrate API.
- `value_ref_contract_complete`: `ValueRef` over strings, numbers, booleans,
  nulls, arrays, objects, and nested key/value traversal must return the same
  kinds, spans, and traversal order as the current retained tape on all valid
  JSON fixtures; the conformance suite must still accept 21 valid fixtures and
  reject 7 invalid fixtures.
- `w3_example_explained`: the proof must explain the documented mismatch for
  `{"a":[1,true]}` where scanner positions and retained tape offsets differ,
  without dropping key quote ownership, colon/comma semantics, number starts, or
  literal starts.
- `no_row_claim_without_consumer`: if no generated retained Track 1 consumer is
  landed, `skinny/RESULTS.md` remains unchanged.
- Optional implementation gate if the proof and consumer land together:
  `twitter/parse_only` Track 1 >= 16524 Mbps and
  `apache_builds/parse_only` Track 1 >= 15368 Mbps; W3 guard rows maintain
  within -2.0% of SK-V8-open for `canada/parse_only`, `mesh/parse_only`,
  `numbers/parse_only`, and `marine_ik/parse_only`.
- `retained_strict_boundary`: proof-only artifacts do not create GO/SOTA rows.
  A future row-moving retained wave must supply same-wave generated retained
  Track 1 consumption, strict validation posture, output-plane validation, and
  grammar-aware comparator telemetry.

LOC budget:

- Up to 450 source/test LOC for the proof tranche. Any production parser/tape
  rewrite that exceeds this should split into a later challenged implementation
  candidate.

Risk:

- High. The owner surface spans SIMD, tape layout, generated parser, generated
  view/value, codegen templates, parity, materialization, and gates. A shortcut
  is likely to recreate REDRESS 51/53 sidecar cursor failures.

Pre-blocked route notes:

- Do not reopen REDRESS 51 byte-class whitespace/event cursor or REDRESS 53
  parser-local structural-mask cursor.
- Do not introduce parser-owned structural facts, aux tables, sidecar
  substrates, `UnionTape`, a second tape, or a renamed side substrate.
- Do not claim parse-row movement from proof-only artifacts.
- Do not hide JSON-specific policy in generic crates; Lock 14 remains binding.
- Do not reopen REDRESS 73 by assuming generated retained helper shape transfers
  monotonically to hand Track 2; profile the hand parser's code layout directly.

## 3. Direct output/control-path contract

Reason to shortlist: W4 proved that a narrow hand Track 2 scalar-parent fold is
not enough. Apache improved, but `random` stayed below sonic/1.10 and
`numbers` regressed in Track 2 time. Remaining direct misses route to a direct
output contract or direct control-path tranche because digest-only evidence is
guard-plane evidence, not typed product proof.

Owner paths:

- `skinny/crates/codegen/src/direct_schema.rs`
- `skinny/crates/codegen/src/typed_direct.rs`
- `skinny/crates/codegen/src/lower/schema_direct.rs`
- `skinny/crates/codegen/src/lower/sink_only.rs`
- `skinny/crates/codegen/src/sink_direct.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/runtime/src/grammars/json/sink.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`

Scalar-reference status:

- For a direct output contract, serde_json plus typed checksum remains the
  scalar/reference oracle. For a control-path stressor, the current independent
  hand Track 2 direct parser remains the guard reference but cannot become
  product proof.

Checkasm status:

- Not applicable unless the candidate adds a SIMD/asm primitive. Any such
  primitive must be split or carry scalar reference and checkasm before use.

Same-wave consumer plan:

- The same wave must make the gate/report layer consume the contract it defines:
  either a typed/direct product output row with serde/sonic parity, or an
  explicitly named control-path stressor row that remains guard-plane only.
- A helper-only control-path edit or digest arithmetic shortcut cannot be
  admitted without the row classifier and selected comparator consuming it.

Falsifiability gates:

- `direct_contract_no_digest_product`: `direct_to_struct` digest rows must not
  be relabeled as typed product proof. If the output plane remains `digest`, the
  signal must say guard/control-path, not product.
- `direct_control_row_identity`: if a control-path stressor is retained, it must
  have a stable workload identity distinct from `real_typed_struct` and must
  bind Track 1, Track 2, sonic-rs, serde_json, run id, and output plane in the
  report manifest.
- `direct_selected_floor`: any behavior wave that targets the W4 selected rows
  must clear the existing direct floors without scalar-parent folding:
  `apache_builds/direct_to_struct` Track 1 and Track 2 >= 8048 Mbps,
  `numbers/direct_to_struct` Track 1 and Track 2 >= 7230 Mbps, and
  `random/direct_to_struct` Track 1 and Track 2 >= 7401 Mbps.
- `direct_go_maintain`: current direct GO rows stay GO:
  `citm_catalog/direct_to_struct`, `marine_ik/direct_to_struct`, and
  `unicode_basic/direct_to_struct`.
- `typed_product_escape_hatch`: if the route is product output rather than
  control path, at least one typed product row must checksum-match serde_json
  and sonic-rs on the `typed direct` plane. A digest-only row cannot satisfy
  this gate.
- `direct_strict_boundary`: direct rows cannot become strict product admissions
  while `Strictness=deferred`, `parse_utf8=view-boundary`, or digest output
  remains the measured plane. Without a changed measured validation path, they
  remain guard/control-path rows.

LOC budget:

- Up to 600 source/test LOC for a contract plus gate/report consumer. A broader
  generated direct runtime rewrite should be split after the contract closes.

Risk:

- High. Direct rows have many exhausted string/materializer routes, and the W4
  Track 2-only patch already failed under Criterion. The highest risk is
  inventing a new label that launders guard evidence into product evidence.

Pre-blocked route notes:

- Do not reopen REDRESS 54, 55, 66, 67, 68, or 69 string/materializer families.
- Do not reopen REDRESS 72 cap-16 direct routing, REDRESS 80 mantissa widening,
  REDRESS 84 object-pair value-byte compaction, or REDRESS 93 scalar-parent
  folding under another name.
- Do not reopen REDRESS 73 by transferring generated retained helper shape to
  hand Track 2 or control paths without direct hand-parser profiling.
- Do not couple Track 2 to generated SinkOnly or generated Track 1.
- Do not make direct digest evidence a typed product claim.

## 4. Comparator same-run evidence manifest

Reason to shortlist: PASS-ALPHA requires strict comparator deltas against
sonic-rs strict, simdjson DOM/On Demand, yyjson, asmjson, RapidJSON, and
serde_json where runnable. SK-V8 W0 keeps C++ sidecars historical or absent, and
W1 blocks sidecar/permissive/historical evidence from strict admission. A
same-run manifest is justified if SK-V9 wants comparator deltas beyond native
Rust anchors.

Scope: gate-only evidence ingestion. This candidate must not produce parser
data, retained tape data, row output, substrate, or strict admission by itself.

Owner paths:

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/gate.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/metadata.rs`
- manifest parsing in an existing report/gate module or a later named module;
  either way it remains evidence ingestion only
- `skinny/xtask/src/main.rs`
- optional checked-in sidecar manifest/provenance path under `skinny/` only if
  the wave names it and the gate consumes it
- `skinny/RESULTS.md` only after the manifest parser validates the run

Scalar-reference status:

- Not applicable to bbnf parser behavior. Comparator correctness is established
  by corpus hash/bytes, strictness declaration, output plane, comparator binary
  identity, and row-level manifest validation.

Checkasm status:

- Not applicable on the current aarch64 host. asmjson AVX-512 rows are x86-only
  comparator sidecars and must remain absent/non-admission unless collected on
  matching hardware with manifest evidence.

Same-wave consumer plan:

- `gate-json` must parse and validate the same-run sidecar manifest in the same
  wave that renders any same-run sidecar cell.
- The selected admission comparator id must be bound in the report/gate path.
  Presence of a valid native anchor in the same row must not validate a sidecar
  or permissive comparator.
- Comparator manifest ingestion may support admission only when the row also
  carries measured bbnf validation, same-run freshness, same output plane, and
  strictness-declared comparator evidence consumed by the gate.

Falsifiability gates:

- `sidecar_same_run_manifest_required`: every populated same-run sidecar cell
  must carry comparator id, binary/version hash, command, corpus hash, bytes,
  strictness, output plane, host triple, CPU/features, build flags, run id,
  sample count or equivalent, and source artifact. Missing or mixed fields
  reject.
- `sidecar_plane_strictness_gate`: DOM sidecars cannot admit digest or
  `typed direct` rows; lossy/permissive comparators remain flaw probes only.
- `sidecar_evidence_only`: sidecar evidence cannot act as a producer,
  substrate, row output source, retained tape source, or strict shortcut.
- `sidecar_malformed_fixture`: at least one malformed manifest fixture must
  fail in a focused test.
- `sidecar_refresh_rows`: if SK-V9 refreshes parse sidecars, begin with named
  rows where W0 carries historical values: `twitter/parse_only`,
  `apache_builds/parse_only`, and `citm_catalog/parse_only`. If same-run
  collection is incomplete, those cells remain `historical:*` or
  `absent:*` and non-admission.
- `telemetry_only_no_behavior_drift`: parser/scanner/generated throughput cells
  must not move by more than +/-1.0% unless a separate behavior wave is
  dispatched later.

LOC budget:

- Up to 500 source/test LOC plus optional manifest fixture data.

Risk:

- Medium-high. The risk is treating foreign sidecar data as strict admission
  without row-plane, run-id, or freshness proof.

Pre-blocked route notes:

- Do not relabel SK-V7 historical C++ sidecars as same-run.
- Do not allow `sonic_rs_lossy`, RapidJSON permissive behavior, or any
  permissive/flaw-probe row to count as strict SOTA evidence.
- Do not use sidecar evidence to admit digest or typed rows unless the output
  plane matches exactly.
- Do not make sidecar-only values strict anchors without a structured manifest.
  Even with a structured manifest, bbnf measured validation in the row remains
  mandatory.

## 5. W0 telemetry/gate refresh for SK-V9-open

Reason to shortlist: SK-V8 W0 closed as an executable telemetry lock, but W2
row-table admission exposed a practical SK-V9 need: row admission must own
run-id/metadata validation instead of fighting the frozen SK-V8 opening capture.
If SK-V9 wants new measured rows or same-run sidecars, it needs a refreshed
opening manifest/gate mode without weakening W0's fail-closed behavior.

Scope: gate-only report refresh. This candidate must not change parser,
scanner, SIMD, asm, codegen, generated output, product behavior, or row
throughput.

Owner paths:

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/gate.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/metadata.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` only if the refresh rejects or routes

Scalar-reference status:

- Not applicable. This candidate is telemetry/gate only and must not change
  parser, scanner, SIMD, asm, codegen, generated output, or product behavior.

Checkasm status:

- Not applicable.

Same-wave consumer plan:

- The SK-V9-open manifest must be produced and consumed by `gate-json` in the
  same wave. A rendered table with unconsumed telemetry fields is a rejection.
- The gate must keep W0's negative tests: mixed capture rejects, dynamic run-id
  drift rejects, stale sidecar strict claims reject, and producer-only telemetry
  rejects.

Falsifiability gates:

- `skv9_open_manifest_count`: a pure refresh starts from the 38 current main
  rows. Apache/CITM `real_typed_struct` rows are added only if Candidate 1's
  measured admission gate passes; otherwise source-only typed fixtures do not
  inflate the manifest.
- `skv9_run_id_drift_detector`: a coherent same-run Criterion capture passes;
  a mixed-capture mutation, input hash mismatch, byte mismatch, missing metadata
  row, or dynamic admitted-row mutation fails closed.
- `skv9_required_fields_consumed`: every required PASS-ALPHA telemetry field
  in the report has a gate consumer or an explicit rejectable placeholder. Plain
  `n/a` is not accepted where a predecessor/baseline reason is known.
- `skv9_refresh_no_behavior_drift`: unless paired with an admitted row-table
  measurement wave, throughput cells remain within +/-1.0% of SK-V8-open and
  generated output is byte-identical.
- `skv9_check_results_mode`: `cargo xtask gate-json --advisory --check-results`
  must compare against the chosen SK-V9-open run id rather than silently falling
  back to the SK-V8 frozen capture.
- `skv9_strict_boundary`: telemetry refresh cannot turn deferred/view-boundary
  rows into strict SOTA admissions. It may only make the validation state
  explicit unless a separate accepted behavior wave changes that state.

LOC budget:

- Up to 450 source/test LOC plus a `RESULTS.md` refresh if admitted.

Risk:

- Medium. This is mostly gate/report code, but it sits on the admission path for
  every later candidate. Over-broad refresh logic can make future waves
  incomparable.

Pre-blocked route notes:

- Do not weaken run-id, input hash, byte count, metadata, selected comparator,
  sidecar freshness, or Track 2 independence checks.
- Do not turn source-only Apache/CITM typed fixtures into measured rows.
- Do not edit parser/scanner/SIMD/codegen behavior in this refresh.
- Do not dispatch SK-V9 behavior waves from this telemetry candidate.

## Rejected-as-shortlist routes

- W3 Tier A production implementation as a storage-only swap is not shortlisted;
  it must pass the retained class/event grammar and `ValueRef` proof first.
- W4 scalar-parent folding is not shortlisted; REDRESS 93 already falsified it.
- PMULL, CTZ, bulk bitmap, tiny-string, Unicode escape, REDRESS 73 helper-shape
  transfer, cap-16 direct, and
  object-pair value-byte routes remain pre-blocked unless a later alpha/challenge
  names a changed measurement framing and fresh gates.
- Pass Omega residuals such as SC-6-L1-R1, broad lock amendments, canonical path
  cleanup, and top-level surface refresh are outside this Alpha-E SK-V9
  shortlist.
