# SK-V16 SPEC - S-P3 Wave Plan

Date: 2026-05-28.

Status: S-P3 V1 planning packet. This file is not an implementation
dispatch. It folds Pass Alpha's SK-V16 goalset, S-P0 prune blocks, S-P1
profile, S-P2 survivor boundaries, and S-P3 P3-A through P3-F into a W0-W11
wave plan.

Authority:

- `restart/skinny/tranches/sk-v16/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v16/HANDOFF.md`
- `restart/skinny/tranches/sk-v16/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`
- `restart/skinny/tranches/sk-v16/research/p1/hardening/V2/CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v16/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v16/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v16/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v16/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v16/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v16/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v16/research/p3/p3f-spec-draft.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/locks/LOCKS.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Dispatch lock:

- No SK-V16 implementation wave dispatches from S-P3 itself.
- The next mandatory user relinquish is G-Omega for Pass Omega or a
  spec-class amendment. G-Alpha auto-passes under the active user pin.
- W0 is the first legal implementation wave after S-P3 convergence and any
  required gate authorization.
- W1-W11 are conditionally gated by this packet and each wave still requires
  research, plan, and redress per `SKINNY-TRIUMVIRATE.md`.

## Section 0 - Close Condition And Goalset

### Section 0.1 - Global Close Condition

SK-V16 closes only when all of these are true:

1. W0 creates a checked `SK-V16-open` baseline and report-consumer lock.
2. All 51 JSON rows remain admitted, strict, same-plane, and measured on
   Apple M5 Max / aarch64.
3. CSS L4 provider proof is grammar-derived from `grammar/css/l4/*.bbnf`;
   `CSS_GENERATED_RS`, fact streams, brace summaries, FNV metadata, stale
   sidecars, and W8R broadcast rows are diagnostic only.
4. CSS exposes typed document/value/view/visitor surfaces.
5. CSS Track 1 typed summary equals cssparser same-workload typed summary
   before speed counts.
6. CSS Track 1 beats cssparser on the same typed workload before any CSS row
   admits.
7. Dirty generated CSS and real-typed state is cleanly regenerated, retired,
   or intrinsically blocked with an exact manifest and broad command proof.
8. Pattern H remains exactly 67 files and advances to generator-owned
   grammar-id template collapse, not header-only provenance.
9. Lock 14 and Lock 16 gates report their own exclusions and reject silent
   self-exemption.
10. Decision Engine and all five BackendShape lowerer proofs remain
    load-bearing and grammar-neutral; no sixth BackendShape exists.
11. FNV remains bench-only; production migration remains blocked.
12. Native SIMD is in scope only with fresh S-P1 hot leaf, scalar reference,
    strict checkasm/parity, same-wave consumer, cold measurement, and
    aarch64-only proof.
13. PASS-IMPL V3 accepts every axis or records row-level intrinsic-block proof.

### Section 0.2 - Comparator Classes

| Class | Examples | Admission use |
|---|---|---|
| Same-run strict anchor | JSON strict same-plane comparators; CSS typed output vs cssparser typed same-workload comparator | May admit only when output plane, strictness, host, corpus, and equality semantics match. |
| Same-run flaw probe | CSS W8R fact streams, full-parse summaries, lightningcss before comparable output | Planning only. |
| Diagnostic signal | stale sidecars, x86/AVX rows, FNV/checksum rows, dirty generated state | Planning or block evidence only. |

### Section 0.3 - Required Telemetry

SK-V16 preserves the visible schema-v3 RESULTS surface and inherited SK-V8/SK-V15
telemetry. It adds gate-consumed report fields:

```text
css_track1_typed_passes
css_cssparser_typed_passes
css_typed_summary_equal
css_provider_source
dirty_generated_state
native_simd_status
typed_materialization_invariant
```

Required report flags:

```text
--skv16-css-typed-report <path>
--skv16-dirty-generated-report <path>
--skv16-pattern-h-roundtrip-report <path>
--skv16-native-simd-report <path>   # only when SIMD is scoped
```

Every emitted field must be parsed and validated by the gate in the same wave.

### Section 0.4 - Opening Row Goalset

| Surface | Opening state | SK-V16 target |
|---|---:|---|
| JSON parse_only | 17 / 17 admitted | Maintain strict guard. |
| JSON direct_to_struct | 17 / 17 admitted | Maintain strict guard. |
| JSON real_typed_struct | 17 / 17 admitted | Maintain strict guard. |
| CSS L4 | 0 / 24 admitted | Rebuild grammar-derived typed equality and cssparser SOTA. |
| Pattern H | 67 files | Generator-owned collapse and roundtrip proof. |
| BackendShape | 5 / 5 | Preserve five-shape canon. |

## Section 1 - Non-Negotiables

1. Apple M5 Max / aarch64 is the only admission host.
2. x86, AVX, PEXT, and x86 side evidence are diagnostic only.
3. No warm benches; cold per-parse evidence only.
4. No generated-output claim closes without regeneration/check proof.
5. No provider/template/runtime deletion before replacement proof no later than
   the delete wave.
6. No primitive or native kernel lands without scalar reference, parity/checkasm
   where relevant, same-wave consumer, and cold row measurement.
7. No retained sidecar, retained cursor/list, aux density/projection table,
   parser-owned structural stream, second tape, public `UnionTape`, public
   substrate API, or sixth `BackendShape`.
8. No JSON/CSS grammar policy in generic crates.
9. No CSS admission from W8R broadcast, fact-stream, full-parse summary, brace
   counter, FNV, stale sidecar, or wrong-plane comparator.
10. No FNV/checksum/hash sidecar in production as selector, arbiter, or
    correctness proof.
11. No paper close: "wired", "integrated", and "future consumer" language
    rejects without measured evidence and gate consumption.

## Section 2 - Wave Manifest, Caps, And Global Gates

Every wave follows `SKINNY-TRIUMVIRATE.md`: research, plan, redress.

| Phase | Hard cap | Output |
|---|---:|---|
| Research | <=30 minutes | Read-only wave research artifact. |
| Plan | <=30 minutes | One intervention, owner paths, gates, revert route. |
| Redress | <=75 minutes | Implementation or ledger repair plus measurement. |

| Wave | Receiver | Risk | Manual LOC | Generated | Docs LOC | Entry gate | Exit gate |
|---|---|---|---:|---|---:|---|---|
| W0 | Baseline/report consumers | Medium | 80-300 | none | 80-180 | S-P3 converged | SK-V16 report consumers exist; JSON guard captured; no behavior drift. |
| W1 | Dirty generated disposition | High | 80-260 | named dirty files | 80-180 | W0 | Dirty manifest, broad commands, owner/disposition consumed by gate. |
| W2 | Lock 14/16 scan expansion | High | 120-360 | reports/fixtures | 80-180 | W0/W1 | Included/excluded roots self-report; silent self-exemption fails. |
| W3 | CSS legacy proof quarantine | High | 120-360 | none or diagnostics | 80-180 | W1/W2 | Legacy CSS proof paths are non-admission. |
| W4 | CSS grammar provider | High | 180-650 | CSS outputs named | 100-220 | W1-W3 | Provider source is grammar-derived; no CSS admit. |
| W5 | CSS typed API/equality | High | 180-650 | reports/results | 100-220 | W4 | 24 CSS typed summaries equal cssparser same workload. |
| W6 | CSS typed SOTA | High | 80-360 | reports/results | 100-220 | W5 | CSS Track 1 beats cssparser typed workload or rejects with REDRESS. |
| W7 | Pattern H census/roundtrip | High | 120-360 | manifest/report | 80-180 | W1/W2 | Count 67; generator-owned roundtrip report consumed. |
| W8 | Pattern H collapse | High | 180-650 | runtime output named | 100-220 | W7 | Generator replacement is byte-equivalent or intrinsically blocked. |
| W9 | Decision/BackendShape guard | High | 120-360 | fixtures/reports | 80-180 | W2 | E-graph/CSP/all-five lowerer proof remains real. |
| W10 | Conditional primitive/tape/native consumer | High | 120-450 | selected generated output | 100-220 | W5 and legal plan | One S-P2 survivor meets scalar/parity/same-wave row gate, or not-scoped. |
| W11 | Close reconciliation | Medium | 0-120 | none | 120-260 | W0-W10 disposed | PASS-IMPL V3 accepts or records intrinsic block for every axis. |

### Section 2.1 - Generality / Lock 14 Gate

Every generic or generated edit must carry:

| Required column | Meaning |
|---|---|
| Generic owner path | Exact generic crate, generator, pass, SIMD/helper, or gate/report file touched. |
| Forbidden token scan | `Json`, `CssL4`, grammar/corpus names, `json_`, `css_`, hardcoded structural roles, and provider families. |
| Non-JSON receiver | CSS L4 plus Sheets or BBNF-self when the path is generic; otherwise intrinsic block with proof. |
| Proof command | Regen/check/test command that exercises the changed generic path. |
| Generated-output expectation | Byte-identical no-diff, named generated diff, or gate-consumed rejected alternative. |
| Fail action | Revert, REDRESS, scalar-delegate, delete, or intrinsic block. |

Gate exclusions must list included roots, excluded roots, reason, owner,
self-scan status, primitive status, gate consumer, affected rows, and
disposition.

## Section 3 - W0 Baseline And SK-V16 Report Consumers

Tasks:

- Capture `SK-V16-open`.
- Add or validate consumers for CSS typed, dirty-generated, Pattern H, and
  native SIMD reports.
- Prove missing or producer-only fields reject.

Exit gate:

- JSON 51 rows obey P3-C maintain formulas.
- CSS remains 0/24 admitted.
- No parser/runtime/codegen behavior drift.

Revert protocol: revert report/gate/schema edits and record the baseline miss.

## Section 4 - W1 Dirty Generated Disposition

Tasks:

- Record the exact dirty generated CSS and real-typed manifest.
- Run broad commands or cite W0-controlled reruns.
- Retire, regenerate, or intrinsically block each dirty file with owner proof.

Exit gate:

- Dirty-generated report carries manifest, `git status --short`, broad command,
  owner, and disposition.
- `dirty_generated_state` is not `dirty_unrouted`.

Revert protocol: revert generated/report slice and save rejected patch evidence.

## Section 5 - W2 Lock 14/16 Scan Expansion

Tasks:

- Expand scan coverage over codegen, runtime generator, xtask, JSON templates,
  CSS provider material, report/gate roots, and primitive/checkasm manifests.
- Report every exclusion.

Exit gate:

- The gate consumes its own exclusion report.
- No generic crate is declared clean by omission.

Revert protocol: revert scan/gate source changes and route intrinsic block.

## Section 6 - W3 CSS Legacy Proof Quarantine

Tasks:

- Quarantine `CSS_GENERATED_RS`, `emit_fact_stream`, `CssFullParseSummary`,
  `parse_full`, brace/delimiter summaries, FNV metadata, W8R broadcast, stale
  sidecars, and wrong-plane comparator proof from CSS admission.

Exit gate:

- CSS legacy proof can appear only as diagnostic non-admission evidence.
- No provider/template deletion occurs before W4-W6 replacement proof.

Revert protocol: revert CSS report/gate edits and record REDRESS.

## Section 7 - W4 CSS Grammar-Derived Provider

Tasks:

- Bind CSS provider source to `grammar/css/l4/*.bbnf` and workspace metadata.
- Generate or check CSS provider/runtime outputs without string-literal proof.

Exit gate:

- `css_provider_source` is grammar-derived.
- Lock 14 gate sees the provider path.
- No CSS speed/admission claim is made.

Revert protocol: revert provider/generator/generated outputs together.

## Section 8 - W5 CSS Typed API And Equality

Tasks:

- Expose CSS typed document/value/view/visitor surfaces.
- Produce Track 1 typed summary and cssparser same-workload typed summary.

Exit gate:

- All 24 CSS rows have equal typed summaries and matching pass/error counts.
- Speed is ignored until equality passes.

Revert protocol: revert typed API/comparator/report slice; CSS remains OPEN.

## Section 9 - W6 CSS Typed SOTA

Tasks:

- Retime Track 1 against cssparser on the same typed workload.
- Admit CSS only if equality remains true.

Exit gate:

- Every admitted CSS row satisfies `track1_typed_mbps >= cssparser_typed_mbps + 1.000`.
- JSON 51 guard rows maintain.

Revert protocol: revert speed intervention/results/redress and record miss rows.

## Section 10 - W7 Pattern H Census And Roundtrip Gate

Tasks:

- Verify `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`
  remains 67.
- Bind generator-owned roundtrip report.

Exit gate:

- Pattern H count is 67.
- Header-only provenance rejects.

Revert protocol: revert report/generator slice.

## Section 11 - W8 Pattern H Generator-Owned Collapse

Tasks:

- Collapse root runtime files through grammar-id template ownership where legal.
- Destructive replacement only with same-wave generator restoration.

Exit gate:

- Byte-equivalent roundtrip from generator or row-level intrinsic block.
- No count drift.

Revert protocol: revert generator/template/generated runtime output together.

## Section 12 - W9 Decision/BackendShape Preservation

Tasks:

- Add adversarial CSP/egraph invalid-selection proof if missing.
- Preserve all five BackendShape lowerer proofs.

Exit gate:

- E-graph/CSP/all-five lowerer reports are consumed.
- No sixth shape, EventTape sidecar, label-string lowerer, or generic grammar
  branch appears.

Revert protocol: revert decision/lowerer slice and record failed fixture.

## Section 13 - W10 Conditional Primitive/Tape/Native Consumer

Tasks:

- Select at most one S-P2 survivor: byte-set/class-table scan, string-special
  mask, escape/hex/digit atom, sealed tape/view scalar operation, or
  materialization ratio report.
- Native SIMD is optional and requires the full S-P1/scalar/checkasm/consumer
  tuple.

Exit gate:

- Selected row floors from P3-C pass.
- Full JSON 51 maintain formulas pass.
- Same-wave consumer is measured; source-present unwired primitives reject.
- If no legal primitive plan exists, record `native_simd_status=not_in_scope`
  or equivalent not-scoped proof.

Revert protocol: revert primitive/consumer/generated output/results together.

## Section 14 - W11 Close Reconciliation

Tasks:

- Reconcile every wave as ADMIT, REJECT, ROUTE, or intrinsic BLOCK.
- Run PASS-IMPL V3 close audit packet.
- Update HANDOFF and close checklist.

Exit gate:

- All Section 0 close conditions are met or each miss has row-level
  intrinsic-block proof.
- No orphan candidate, future-promise close, or relabeled routed block remains.

Revert protocol: revert close docs if any overclaim is found.

## Section 15 - Pre-Blocked Routes

SK-V16 blocks REDRESS 28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98,
183/184/209-213, 215, 242-247, and FNV production migration under old
framing. Reopening requires fresh P1 evidence, scalar/oracle proof,
same-wave consumer, executable row gate, and REDRESS non-reopen proof.

## Section 16 - Dispatch Scope

S-P3 produces this SPEC and `DISPATCH-PROMPT.md`. Wave implementation begins
only after S-P3 converges and any required G-Omega authorization closes. Each
wave is dispatched independently through the triumvirate. Any spec-class
amendment, Lock change, or V1 corpus change routes through Pass Omega and
G-Omega.
