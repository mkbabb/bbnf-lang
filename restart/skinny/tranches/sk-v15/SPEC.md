# SK-V15 SPEC - S-P3 Wave Plan

Date: 2026-05-28.

Status: S-P3 V2 planning packet. This file is not an implementation
dispatch. It folds Pass Alpha's SK-V15 goalset, PASS-IMPL V1, S-P2 V3
locked survivor boundaries, and S-P3 P3-A through P3-F into a conditional
W0-W11 wave plan. The shape preserves SK-V15 prune-before-rebuild
direction and the V2 hardening split of CSS provider/retime and
Decision/lowerer work.

Authority:

- `restart/skinny/tranches/sk-v15/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v15/HANDOFF.md`
- `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md`
- `restart/skinny/tranches/sk-v15/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/locks/LOCKS.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Dispatch lock:

- No SK-V15 implementation wave dispatches from S-P3 itself.
- The next mandatory user relinquish is G-Omega for Pass Omega; G-Alpha
  auto-passes under the active user pin.
- W0 is the first legal implementation wave after S-P3 convergence and
  required gate authorization.
- W1-W11 are conditionally gated by this packet; each wave still requires
  a research, plan, and redress triumvirate.
- W1 dispatches after W0. W2 dispatches after W1 demotes or blocks CSS
  admission. W3 dispatches after W2. W4 dispatches after W2 plus W3.
  W5 dispatches after W1-W4. W6 dispatches after W5 typed CSS provider.
  W7 dispatches after W6. W8 dispatches after W7. W9 dispatches after W8.
  W10 dispatches after W9. W11 dispatches after W1-W10 are admitted,
  reverted, redressed, or intrinsically blocked with proof.

## Section 0 - Close Condition And Goalset

### Section 0.1 - Global Close Condition

SK-V15 closes only when all of these are true:

1. W0 creates a checked `SK-V15-open` baseline and telemetry lock.
2. All 51 JSON rows remain admitted, strict, same-plane, and measured on
   native Apple M5 Max / aarch64.
3. No CSS 24-row broadcast admit remains. CSS rows are either one
   diagnostic aggregate or independently measured typed-output rows.
4. `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only CSS
   `parse()`, and brace-counter admission are retired from live CSS
   admission.
5. CSS exposes typed value, document, view, and visitor surfaces comparable
   in capability to JSON's Value API.
6. CSS retiming uses a same-workload comparator. `cssparser` is the
   near-term comparator; `lightningcss` counts only after Track 1 emits
   comparable CSSOM/value output.
7. Lock 14 and Lock 16 gates scan the leak roots that were previously
   omitted, report every exclusion, and fail on self-exempting scans.
8. Generic codegen has no JSON/CSS runtime mode split, per-grammar regen
   enum/match fanout, hardcoded CSS profile table, or generic-pass JSON
   byte recognizer.
9. Pattern H remains exactly 67 root runtime files and every file has true
   generator provenance at line 1, backed by regeneration/check proof.
10. Decision Engine has at least one asserted e-graph rewrite, a
    non-tautological CSP, grammar-neutral facts, and all five BackendShape
    lowerers emit real implementation paths.
11. W11L/W11N/W11O FNV closed-enum products remain bench-only and the
    strict-product comparator catches closed-enum sidecar coupling.
12. Every close row cites HEAD command output, generated artifacts or diffs
    where relevant, strict parity/checkasm where relevant, and cold
    per-parse measurement evidence.
13. PASS-IMPL V2 accepts every axis or supplies row-level intrinsic-block
    proof.

No implementation-limited miss closes SK-V15. A miss becomes REDRESS,
revert, demotion, or intrinsic block with proof. SK-V16 routing is routed
remainder after proof, not SK-V15 close evidence.

### Section 0.2 - Comparator Classes

| Class | Examples | Admission use |
|---|---|---|
| Same-run strict anchor | JSON strict same-plane comparators; CSS typed output vs `cssparser` typed value/document comparator | Admission only when output plane, corpus, equality semantics, strictness, and host match. |
| Same-run flaw probe | lossy/permissive JSON comparators; CSS parser sidecars on a different output plane | Planning only. |
| Diagnostic planning signal | stale sidecars, W8R CSS tuple, `lightningcss` before comparable CSSOM output, x86/AVX-512 diagnostics | Planning only. |

### Section 0.3 - Outcome Posture

JSON rows may remain admitted only as strict same-plane measured rows.
CSS rows that still carry W8R broadcast evidence are diagnostic or NO-GO,
not admits. Ledger demotion is truth repair, not throughput regression.

### Section 0.4 - Required Telemetry

SK-V15 preserves the visible schema-v3 `skinny/RESULTS.md` surface and the
SK-V8 telemetry carrier. It additionally requires these gate-consumed
fields:

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

Every emitted field must be parsed by `gate-json` or its SK-V15 successor.
Missing fields, producer-only telemetry, hidden one-to-N CSS measurement
stamps, self-exempting gate exclusions, source-present unwired primitives,
and native-platform mismatch reject close.

### Section 0.5 - Opening Row Goalset

| Family | Opening state | SK-V15 target |
|---|---|---|
| JSON parse_only | 17 / 17 admitted | Maintain 17 / 17 strict same-plane rows. |
| JSON direct_to_struct | 17 / 17 admitted | Maintain 17 / 17 strict same-plane rows. |
| JSON real_typed_struct | 17 / 17 admitted | Maintain 17 / 17 strict same-plane rows. |
| CSS L4 | 24 rows audit-demoted | Collapse broadcast evidence to diagnostic aggregate or rebuild independent typed rows. |

## Section 1 - Non-Negotiables

1. Apple M5 Max / aarch64 is the only admission host.
2. x86 and AVX-512 are diagnostic signals only.
3. No warm benches; cold per-parse evidence only.
4. No generated-output claim closes without generator provenance and
   regeneration/check evidence.
5. No delete or retirement before rebuild proof per `NEW-CH3-V5-01`.
6. No S-P2 REJECT re-enters without fresh Alpha/P1 evidence and a new
   materially different route.
7. No primitive, SIMD/ASM kernel, or parser helper lands without scalar
   reference or oracle, parity/checkasm where relevant, and same-wave
   consumer.
8. No documentation-only close.

## Section 2 - Wave Manifest, Caps, And Global Gates

Every wave follows `SKINNY-TRIUMVIRATE.md`: research, plan, redress.

| Phase | Hard cap | Output |
|---|---:|---|
| Research | <=20 minutes | Read-only wave research artifact. |
| Plan | <=15 minutes | One intervention, owner paths, gates, revert route. |
| Redress | <=30 minutes | Implementation or ledger edit, measurements, REDRESS/revert/admit. |

At 0.9x cap, commit or checkpoint; at cap, halt with evidence. If a wave
cannot fit the cap, split before redress.

| Wave | Receiver | Risk | Manual LOC | Generated | Docs LOC | Entry gate | Exit gate |
|---|---|---|---:|---|---:|---|---|
| W0 | Baseline and telemetry lock | Medium | 60-160 | None | 80-180 | S-P3 and G-Omega closed | Gate consumes SK-V15 telemetry; CSS broadcast is diagnostic; no provider deletion. |
| W1 | CSS admission honesty | Medium | 80-200 | None | 80-180 | W0 admitted | 24 CSS broadcast admits are demoted or collapsed; no W8R live admit. |
| W2 | Lock 14 / Lock 16 gate restoration | High | 120-280 | Reports/fixtures only | 80-180 | W1 admitted or CSS blocked | Gates report roots/exclusions and source-present primitive status; self-exemptions fail. |
| W3 | Codegen leak abrogation | High | 150-320 | Regen/check evidence | 80-180 | W2 admitted | One coherent generic leak family is removed with same-wave generator consumer. |
| W4 | Pattern H generated discipline | High | 120-280 | Runtime generated checks | 80-180 | W2 and W3 admitted/routed | 67 root runtime files have true provenance and non-writing regen/check proof. |
| W5 | CSS typed Value provider | High | 180-360 | 220-440 allowed from named provider | 80-180 | W1-W4 admitted/routed | Typed CSS value/document/view/visitor provider exists; old proof remains diagnostic. |
| W6 | CSS same-workload retime and old-proof retirement | High | 160-340 | Reports/results | 100-220 | W5 admitted | Fresh typed cssparser comparison sets any CSS floor; old CSS proof paths retire. |
| W7 | Decision Engine spine | High | 140-300 | Selection fixtures | 80-180 | W6 admitted/routed | E-graph rewrite and non-tautological CSP are gate-consumed. |
| W8 | BackendShape harness plus EagerTape/OffsetTape | High | 140-300 | 180-360 fixtures | 80-180 | W7 admitted | Harness rejects label scaffold; EagerTape/OffsetTape emit runtime-relevant output. |
| W9 | EventTape/SinkOnly/CollapsedStage plus all-five gate | High | 160-340 | 220-420 fixtures | 100-220 | W8 admitted | Remaining lowerers are real and all-five gate proves exactly five BackendShape variants. |
| W10 | FNV quarantine | Medium | 80-220 | 100-240 bench fixtures/reports | 80-180 | W9 admitted/routed | FNV stays bench-only and production FNV scan/adversarial fixtures are consumed. |
| W11 | Close and PASS-IMPL V2 handoff | Medium | 0-80 | None except evidence from prior waves | 120-420 | W1-W10 resolved | PASS-IMPL V2 accepts each axis or records row-level intrinsic-block proof at HEAD. |

### Section 2.1 - Dependency Table

| Dependency | Provider wave | Consumer/delete wave | Gate |
|---|---|---|---|
| CSS SOTA admission | W5 typed provider plus W6 fresh retime | W6 and later | No CSS SOTA admit before W6 same-workload retime. |
| CSS provider/template deletion | W5 typed provider and W6 old-proof retirement | W6 or later | No delete before provider proof no later than delete wave. |
| Pattern H generated claim | W4 root runtime regen/check | W4 and later | No generated claim before W4 proof. |
| Decision-driven row movement | W7 Decision Engine spine | W8/W9 lowerers | No lowerer row movement before W7. |
| All-five BackendShape claim | W8 partial lowerers plus W9 remaining lowerers | W9 | No all-five close before W9. |
| FNV correctness/selection role | W10 quarantine | W10 and close | No production role; bench-only quarantine proof required. |
| Close | W11 PASS-IMPL V2 | W11 | No close before PASS-IMPL V2 consumes packet. |

### Section 2.2 - Generality / Lock 14 Gate

Every wave plan that touches generic crates or generic generators must
carry this table. A missing row rejects redress.

| Required column | Meaning |
|---|---|
| Generic owner path | Exact generic crate, pass, generator, lowerer, or SIMD/parser-helper file touched. |
| Forbidden token scan | `Json`, `CssL4`, Sheets/corpus names, JSON structural roles, CSS profile names, `json_`, `css_`, `RuntimeProvider`, and aliases. |
| Non-JSON receiver | CSS L4 plus Sheets or BBNF-self when generic behavior can affect multiple grammars; otherwise intrinsic block with proof. |
| Proof command | Regen/check/test command that exercises the changed generic path for the receiver. |
| Generated-output expectation | Byte-identical no-diff, named generated diff, or gate-consumed rejected alternative. |
| Fail action | Revert, REDRESS, scalar-delegate, delete, or intrinsic block. |

Gate exclusions must list included roots, excluded roots, reason, owner,
self-scan status, primitive status, gate consumer, affected rows, and
disposition. EventTape is only one BackendShape lowerer; it cannot become
a sidecar vector, class column, sixth shape, public `UnionTape`, retained
stream, public substrate API, or alternate document projection. Generated
output requires line-1 provenance plus non-writing regen/check; header-only
proof rejects. Lowerers cannot be label strings, `todo!`, or pass-through
shells.

## Section 3 - W0 Baseline And Telemetry Lock

Tasks:

- Capture `SK-V15-open` for the 51 JSON rows.
- Preserve current CSS rows as diagnostic broadcast evidence unless W1 has
  already demoted them.
- Add or validate SK-V15 telemetry fields.
- Prove gate consumption of every emitted field.

Exit gate:

- JSON row cells remain within +/-1.0% if rerun; no verdict downgrades.
- CSS rows with W8R broadcast have `broadcast_group_id` and non-admit
  status.
- `gate-json` rejects missing SK-V15 fields and hidden broadcast.
- Host telemetry is Apple M5 Max / aarch64.

## Section 4 - W1 CSS Admission Honesty

Tasks:

- Demote the current 24 CSS L4 broadcast admits or collapse them to one
  diagnostic aggregate.
- Reject any live CSS admit that shares one measurement tuple across
  multiple feature rows.
- Keep live CSS providers until W5 unless typed replacement proof lands in
  the same wave.

Exit gate:

- No CSS row closes from W8R broadcast evidence.
- `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only `parse()`,
  and brace-counter output are diagnostic only.
- JSON 51/51 guard stays within W0 budget.

## Section 5 - W2 Lock 14 / Lock 16 Gate Restoration

Tasks:

- Restore scan coverage for previously omitted codegen/runtime/bench/gate
  roots.
- Emit gate-exclusion reports and make gates consume them.
- Classify source-present SIMD/ASM primitives as wired, scalar-delegated,
  deleted, blocked, or strict-checkasm admitted.

Exit gate:

- Lock 14 reports scan roots and every exclusion.
- Lock 16 reports primitive status and strict checkasm/parity proof where
  relevant.
- Self-exempting scans reject.

## Section 6 - W3 Codegen Leak Abrogation

Tasks:

- Remove grammar-family runtime modes and hardcoded CSS profile rosters
  from generic codegen.
- Remove JSON/CSS recognizers from generic passes or route them to generated
  grammar metadata.
- Exercise changed generator paths with a same-wave regen/check command.

Exit gate:

- Generic crates do not branch on `Json`, `CssL4`, Sheets, corpus names,
  JSON structural roles, or CSS profile names.
- If JSON-adjacent generation changes, JSON 51/51 reruns in the same wave.
- CSS provider/template deletion remains blocked until W6 proof.

## Section 7 - W4 Pattern H Generated Discipline

Tasks:

- Restore Pattern H provenance over the 67 root runtime files.
- Add non-writing check and delete-plus-regen proof before destructive
  runtime deletion.
- Reject header-only generated status.

Exit gate:

- `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`
  returns 67.
- All 67 intended files carry true line-1 generated provenance.
- Generator/check proof can reproduce them or emits an intrinsic block.

## Section 8 - W5 CSS Typed Value Provider

Tasks:

- Build typed CSS value, document, view, and visitor output.
- Keep old CSS proof diagnostic until W6 same-workload retime closes.
- Prove typed provider output through tests/gates.

Exit gate:

- CSS Track 1 emits typed value/document facts, not a fact stream or
  four-counter summary.
- CSS provider surfaces are comparable to JSON Value API capability.
- W8R tuple values are diagnostic negative fixtures only, never floors.
- Generic provider edits prove CSS plus Sheets or BBNF-self stability when
  the generic path can affect multiple grammars.

## Section 9 - W6 CSS Same-Workload Retime And Old-Proof Retirement

Tasks:

- Run fresh same-run `cssparser` typed-value/document comparator evidence.
- Set any CSS typed-admission floor from the fresh W6 typed run only.
- Retire `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only
  `parse()`, and brace-counter proof from live admission.

Exit gate:

- Track 1 CSS typed row meets or beats same-run cssparser on the same
  workload.
- Optional per-feature rows have distinct measurements and no shared hidden
  signature.
- JSON 51/51 maintains >=98% of W0 if behavior changes.

## Section 10 - W7 Decision Engine Spine

Tasks:

- Add at least one asserted e-graph rewrite.
- Make CSP non-tautological.
- Remove grammar-named facts from generic decision records.
- Prove decision output can change generated behavior or selection.

Exit gate:

- `egraph_rewrite_count >= 1`.
- Removing a required fact can change CSP satisfiability or selection.
- No `json_*` or `css_*` facts drive generic selection.

## Section 11 - W8 BackendShape Harness Plus EagerTape/OffsetTape

Tasks:

- Add lowerer fixtures that fail against label-string scaffolds.
- Implement real output paths for EagerTape and OffsetTape, or
  gate-consumed rejected alternatives.
- Exercise generic lowerer paths with CSS L4 plus Sheets or BBNF-self when
  the generic path changes.

Exit gate:

- EagerTape and OffsetTape lowerers are not placeholders, label strings,
  `todo!`, or pass-through shells.
- Generated fixture output is runtime-relevant or rejected by a consumed
  gate.

## Section 12 - W9 EventTape/SinkOnly/CollapsedStage And All-Five Gate

Tasks:

- Implement real output paths for EventTape, SinkOnly, and CollapsedStage,
  or gate-consumed rejected alternatives.
- Add the all-five BackendShape gate.
- Enforce EventTape anti-sidecar discipline.

Exit gate:

- EventTape/SinkOnly/CollapsedStage lowerers are not placeholders.
- The all-five gate sees exactly `{EagerTape, OffsetTape, EventTape,
  SinkOnly, CollapsedStage}`.
- EventTape is not a sidecar vector, sixth shape, retained stream, public
  substrate API, or alternate document projection.

## Section 13 - W10 FNV Quarantine

Tasks:

- Quarantine W11L/W11N/W11O FNV closed-enum products to bench/xtask.
- Add adversarial semantic fixtures that distinguish hash equality from
  typed semantic equality.
- Scan production roots for FNV use.

Exit gate:

- FNV cannot act as runtime selector, production arbiter, or correctness
  proof.
- Strict-product gate consumes quarantine metadata and negative fixtures.
- Production FNV hits are absent or routed to REDRESS with a new contract.

## Section 14 - W11 Close Reconciliation

Tasks:

- Reconcile RESULTS, REDRESS, rolling delta, and HANDOFF.
- Run PASS-IMPL V2.
- Prepare SK-V16 Pass Alpha input only after SK-V15 proof exists.

Exit gate:

- No dependency-table row lacks proof or intrinsic-block evidence.
- Close evidence is command output, generated diffs, strict manifests, and
  cold measurements, not docs-only claims.
- PASS-IMPL V2 accepts every axis or records row-level intrinsic-block proof
  at HEAD.
- SK-V16 routing is routed remainder after proof; it cannot substitute for
  an SK-V15 repair.

## Section 15 - Pre-Blocked Routes

The shared pre-block list is:

`28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, and FNV closed-enum production migration`

| Route family | Block |
|---|---|
| REDRESS 28+33, 60, 72, 83 | No tiny-string/StringBlock replay under a new name. |
| REDRESS 50-55, 96-98 | No retained sidecar tables, cursor streams, class columns, public `UnionTape`, or second tape. |
| REDRESS 60-72, 84 | No retained parse shortcuts or global direct/Track 2 cap changes. |
| REDRESS 80 | No numeric/digit route without fresh P1 BBNF-side hot leaf. |
| REDRESS 82-84 | No one-quartet Unicode/object-pair production promotion. |
| REDRESS 88, 89 | No PMULL or CSSC production hot-body promotion from checkasm/ISA alone. |
| REDRESS 183/184/209-213 | No provider/runtime/template delete before replacement proof. |
| REDRESS 215 | No CSS broadcast, brace-counter, or wrong-plane comparator admission. |
| REDRESS 242-247 | No decoded-string, structural-stream, string64, or fixed-shape unicode retry under old framing. |
| FNV closed enum | Bench-only quarantine; no production migration without a future contract. |

## Section 16 - Dispatch Posture

S-P3 produces this contract; it does not execute it. On S-P3 convergence,
the orchestrator updates `HANDOFF.md` to `ready-for-wave-W0`, runs any
required Pass Omega/G-Omega step for spec amendments, and then dispatches
W0 through the SKINNY triumvirate.

Only G-Omega is a mandatory user gate under the active pin. Every other
gate auto-passes unless an intrinsic invariant violation cannot be repaired
inside the pass discipline.
