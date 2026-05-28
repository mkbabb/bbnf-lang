# SK-V15 SPEC - S-P3 Wave Plan

Date: 2026-05-28.

Status: S-P3 V1 planning packet. This file is not an implementation
dispatch. It folds Pass Alpha's SK-V15 goalset, PASS-IMPL V1, S-P2
V3 locked survivor boundaries, and S-P3 P3-A through P3-F into a
conditional W0-W9 wave plan. The shape mirrors the SK-V8/SK-V14 SPEC
contract while preserving the SK-V15 prune-before-rebuild direction.

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
- W1-W9 are conditionally gated by this packet; each wave still requires
  a research, plan, and redress triumvirate.

## Section 0 - Close Condition And Goalset

### Section 0.1 - Global Close Condition

SK-V15 closes only when all of these are true:

1. W0 creates a checked `SK-V15-open` baseline and telemetry lock.
2. All 51 JSON rows remain admitted, strict, same-plane, and measured
   on native Apple M5 Max / aarch64.
3. No CSS 24-row broadcast admit remains. CSS rows are either one
   diagnostic aggregate or independently measured typed-output rows.
4. `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only CSS
   `parse()`, and brace-counter admission are retired from live CSS
   admission.
5. CSS exposes typed value, document, view, and visitor surfaces
   comparable in capability to JSON's Value API.
6. CSS retiming uses a same-workload comparator. `cssparser` is the
   near-term comparator; `lightningcss` counts only after Track 1 emits
   comparable CSSOM/value output.
7. Lock 14 and Lock 16 gates scan the leak roots that were previously
   omitted, report every exclusion, and fail on self-exempting scans.
8. Generic codegen has no JSON/CSS runtime mode split, per-grammar regen
   enum/match fanout, hardcoded CSS profile table, or generic-pass JSON
   byte recognizer.
9. Pattern H remains exactly 67 root runtime files and every file has
   true generator provenance at line 1, backed by a regeneration/check
   proof rather than header-only edits.
10. Decision Engine has at least one asserted e-graph rewrite, a
    non-tautological CSP, grammar-neutral facts, and all five
    BackendShape lowerers emit real implementation paths.
11. W11L/W11N/W11O FNV closed-enum products remain bench-only and the
    strict-product comparator catches closed-enum sidecar coupling.
12. Every close row cites HEAD command output, generated artifacts or
    diffs where relevant, strict parity/checkasm where relevant, and
    cold per-parse measurement evidence.
13. PASS-IMPL V2 accepts every axis or supplies row-level intrinsic-block
    proof.

No implementation-limited miss closes SK-V15. A miss becomes REDRESS,
revert, demotion, or intrinsic block with proof.

### Section 0.2 - Comparator Classes

| Class | Examples | Admission use |
|---|---|---|
| Same-run strict anchor | JSON strict same-plane comparators; CSS typed output vs `cssparser` typed value/document comparator | Admission only when output plane, corpus, equality semantics, strictness, and host match. |
| Same-run flaw probe | lossy/permissive JSON comparators; CSS parser sidecars on a different output plane | Planning only. |
| Diagnostic planning signal | stale sidecars, `lightningcss` before comparable CSSOM output, x86/AVX-512 diagnostics | Planning only. |

### Section 0.3 - Outcome Posture

JSON rows may remain admitted only as strict same-plane measured rows.
CSS rows that still carry W8R broadcast evidence are diagnostic or
NO-GO, not admits. Ledger demotion is not a throughput regression; it is
truth repair.

### Section 0.4 - Required Telemetry

SK-V15 preserves the visible schema-v3 `skinny/RESULTS.md` surface and
the SK-V8 telemetry carrier. It additionally requires these gate-consumed
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

Every emitted field must be parsed by `gate-json` or its SK-V15
successor. Missing fields, producer-only telemetry, hidden one-to-N CSS
measurement stamps, self-exempting gate exclusions, source-present
unwired primitives, and native-platform mismatch reject close.

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

| Wave | Receiver | Entry gate | Owner path family | Exit gate |
|---|---|---|---|---|
| W0 | Baseline and telemetry lock | S-P3 convergence and G-Omega authorization | `skinny/RESULTS.md`, `skinny/REDRESS.md`, `restart/skinny/ROLLING-SOTA-DELTA.md`, gate/report/telemetry surfaces | SK-V15 telemetry is present and gate-consumed; CSS broadcast is visible as diagnostic; no provider deletion. |
| W1 | PRUNE-A CSS admission honesty | W0 admitted | CSS bench/report/gate surfaces and rolling delta | 24 CSS broadcast admits demoted or replaced by independently measured typed rows; JSON 51/51 unchanged. |
| W2 | PRUNE-B Lock 14 / Lock 16 gate restoration | W0 and W1 admitted or W1 blocks CSS as non-admission | Lock 14/16 gates, checkasm reports, scan roots | Gates scan/report leak roots and exclusions; self-exemptions fail closed. |
| W3 | PRUNE-C codegen leak abrogation | W2 admitted | codegen, grammar provider/profile, runtime generator, passes, xtask | No generic JSON/CSS runtime mode split, static CSS roster, per-grammar workaround, or JSON-shaped generic pass leak. |
| W4 | PRUNE-D Pattern H generated discipline | W2 and W3 admitted/routed | root `crates/core/src/runtime/**`, skinny runtime projections, xtask regen | 67 Pattern H files have true generated provenance and non-writing regen/check proof; no destructive delete without proof. |
| W5 | REBUILD-E CSS typed Value API | W1-W4 admitted/routed with no open delete dependency | CSS grammar/codegen/runtime/bench/gate surfaces | Typed CSS value/document/view/visitor output exists; same-workload comparator retiming; old summary/fact-stream proof retired only with same-wave provider proof. |
| W6 | REBUILD-F.1 Decision Engine spine | W5 admitted or intrinsically routed | `backend_egraph.rs`, `decision_csp.rs`, passes, IR, tests | e-graph rewrite count >=1; CSP is non-tautological; grammar-neutral facts can change selection. |
| W7 | REBUILD-F.2 BackendShape lowerers | W6 admitted | `skinny/crates/codegen/src/lower/*.rs`, generated fixtures | All five lowerers emit real output or gate-consumed rejected alternatives; no label strings. |
| W8 | REBUILD-G FNV quarantine | W7 admitted/routed | bench/xtask FNV helpers, strict-product gates, adversarial fixtures | FNV is bench-only and cannot arbitrate runtime correctness or selection; adversarial semantic fixtures pass. |
| W9 | Close reconciliation and PASS-IMPL V2 handoff | W1-W8 admitted/rejected/routed | RESULTS, REDRESS, rolling delta, HANDOFF, PASS-IMPL V2 packet | Close artifacts agree; no dependency-table orphan remains; PASS-IMPL V2 executes. |

## Section 3 - W0 Baseline And Telemetry Lock

Tasks:

- Capture `SK-V15-open` for the 51 JSON rows.
- Preserve current CSS rows as diagnostic broadcast evidence unless W1
  has already demoted them.
- Add or validate SK-V15 telemetry fields.
- Prove gate consumption of every emitted field.

Entry gate: S-P3 convergence, authorized wave dispatch, clean ownership
of gate/report/telemetry files.

Exit gate:

- JSON row cells remain within +/-1.0% if rerun; no verdict downgrades.
- CSS rows with W8R broadcast have `broadcast_group_id` and non-admit
  status.
- `gate-json` rejects missing SK-V15 fields and hidden broadcast.
- Host telemetry is Apple M5 Max / aarch64.

Revert protocol: revert W0 telemetry/report/gate/RESULTS edits, preserve
failing output, and add REDRESS naming the missing field or drift.

## Section 4 - W1 PRUNE-A CSS Admission Honesty

Tasks:

- Demote the current 24 CSS L4 broadcast admits or collapse them to one
  diagnostic aggregate.
- Reject any live CSS admit that shares one measurement tuple across
  multiple feature rows.
- Keep live CSS providers until W5 unless typed replacement proof lands
  in the same wave.

Entry gate: W0 admitted.

Exit gate:

- No CSS row closes from W8R broadcast evidence.
- `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only `parse()`,
  and brace-counter output are diagnostic only.
- JSON 51/51 guard stays within W0 budget.

Revert protocol: revert CSS report/gate/RESULTS edits, record REDRESS
with offending row ids and duplicate measurement identity.

## Section 5 - W2 PRUNE-B Lock 14 / Lock 16 Gate Restoration

Tasks:

- Restore scan coverage for previously omitted codegen/runtime/bench/gate
  roots.
- Emit gate-exclusion reports and make gates consume them.
- Classify source-present SIMD/ASM primitives as wired, scalar-delegated,
  deleted, blocked, or strict-checkasm admitted.

Entry gate: W0 admitted and W1 has removed CSS admission pressure or
explicitly blocked CSS as non-admission.

Exit gate:

- Lock 14 reports scan roots and every exclusion.
- Lock 16 reports primitive status and strict checkasm/parity proof where
  relevant.
- Self-exempting scans reject.

Revert protocol: revert gate/report edits, save failing gate output, add
REDRESS naming the omitted root or self-exemption.

## Section 6 - W3 PRUNE-C Codegen Leak Abrogation

Tasks:

- Remove grammar-family runtime modes and hardcoded CSS profile rosters
  from generic codegen.
- Remove JSON/CSS recognizers from generic passes or route them to
  generated grammar metadata.
- Exercise changed generator paths with a same-wave regen/check command.

Entry gate: W2 admitted.

Exit gate:

- Generic crates do not branch on `Json`, `CssL4`, Sheets, corpus names,
  JSON structural roles, or CSS profile names.
- If JSON-adjacent generation changes, JSON 51/51 reruns in the same wave.
- CSS provider/template deletion remains blocked until W5 proof.

Revert protocol: revert codegen/generator/gate edits together and record
the leak token and generated-output diff in REDRESS.

## Section 7 - W4 PRUNE-D Pattern H Discipline

Tasks:

- Restore Pattern H provenance over the 67 root runtime files.
- Add non-writing check and delete-plus-regen proof before destructive
  runtime deletion.
- Reject header-only generated status.

Entry gate: W2 admitted and W3 admitted/routed.

Exit gate:

- `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`
  returns 67.
- All 67 intended files carry true line-1 generated provenance.
- Generator/check proof can reproduce them or emits an intrinsic block.

Revert protocol: revert provenance/generator/report edits, save 67-file
inventory, add REDRESS for unreproducible files.

## Section 8 - W5 REBUILD-E CSS Typed Value API

Tasks:

- Build typed CSS value, document, view, and visitor output.
- Retire string/fact/summary/brace-counter CSS admission proof only after
  typed provider proof lands.
- Retest against same-workload `cssparser` comparator and maintain JSON
  guard rows.

Entry gate: W1 and W2 admitted; W3/W4 admitted or routed if their owner
paths affect the CSS provider.

Exit gate:

- CSS Track 1 emits typed value/document facts, not a fact stream or
  four-counter summary.
- CSS comparator workload matches the output plane.
- Track 1 CSS typed row meets or beats same-run cssparser on the same
  workload; optional per-feature rows have distinct measurements.
- JSON 51/51 maintains >=98% of W0 if behavior changes.

Revert protocol: revert CSS runtime/codegen/bench/gate/RESULTS edits and
any provider retirement as one slice; add REDRESS for missed CSS row or
JSON guard failure.

## Section 9 - W6 REBUILD-F.1 Decision Engine Spine

Tasks:

- Add at least one asserted e-graph rewrite.
- Make CSP non-tautological.
- Remove grammar-named facts from generic decision records.
- Prove decision output can change generated behavior or selection.

Entry gate: W5 admitted or intrinsically routed.

Exit gate:

- `egraph_rewrite_count >= 1`.
- Removing a required fact can change CSP satisfiability or selection.
- No `json_*` or `css_*` facts drive generic selection.

Revert protocol: revert Decision Engine edits and generated diffs; add
REDRESS naming tautological facts or non-driving rewrites.

## Section 10 - W7 REBUILD-F.2 BackendShape Lowerers

Tasks:

- Implement real lowerer paths for EagerTape, OffsetTape, EventTape,
  SinkOnly, and CollapsedStage or gate-consumed rejection alternatives.
- Add tests that would fail against label-string scaffolds.
- Keep row movement claims tied to generated runtime evidence.

Entry gate: W6 admitted.

Exit gate:

- No lowerer is a placeholder, label string, `todo!`, or pass-through shell.
- Each lowerer has a runtime-relevant generated diff or explicit rejected
  alternative.
- Any behavior diff obeys JSON maintain and strict equality gates.

Revert protocol: revert lowerer/generated-output edits, save failing
lowerer report, and add REDRESS.

## Section 11 - W8 REBUILD-G FNV Quarantine

Tasks:

- Quarantine W11L/W11N/W11O FNV closed-enum products to bench/xtask.
- Add adversarial semantic fixtures that distinguish hash equality from
  typed semantic equality.
- Scan production roots for FNV use.

Entry gate: W7 admitted or routed with independence proof.

Exit gate:

- FNV cannot act as runtime selector, production arbiter, or correctness
  proof.
- Strict-product gate consumes quarantine metadata and negative fixtures.
- Production FNV hits are absent or routed to REDRESS with a new contract.

Revert protocol: revert FNV quarantine/gate/report edits, save failing
strict-product differential output, add REDRESS.

## Section 12 - W9 Close Reconciliation

Tasks:

- Reconcile RESULTS, REDRESS, rolling delta, and HANDOFF.
- Run PASS-IMPL V2.
- Prepare SK-V16 Pass Alpha input.

Entry gate: W1-W8 admitted, reverted, redressed, or intrinsically
blocked with proof.

Exit gate:

- No dependency-table row lacks proof or intrinsic-block evidence.
- Close evidence is command output, generated diffs, strict manifests,
  and cold measurements, not docs-only claims.
- PASS-IMPL V2 accepts every axis or routes SK-V16 prune inputs.

Revert protocol: revert close-packet edits, preserve PASS-IMPL V2
failure output, and add REDRESS.

## Section 13 - Pre-Blocked Routes

| Route family | Block |
|---|---|
| REDRESS 28+33, 60, 72, 83 | No tiny-string/StringBlock replay under a new name. |
| REDRESS 50-55, 96-98 | No retained sidecar tables, cursor streams, class columns, public `UnionTape`, or second tape. |
| REDRESS 60-72, 84 | No retained parse shortcuts or global direct/Track 2 cap changes. |
| REDRESS 80 | No numeric/digit route without fresh P1 BBNF-side hot leaf. |
| REDRESS 82-84 | No one-quartet Unicode/object-pair production promotion. |
| REDRESS 88, 89 | No PMULL or CSSC production hot-body promotion from checkasm/ISA alone. |
| REDRESS 183, 184, 209-213 | No provider/runtime/template delete before replacement proof. |
| REDRESS 215 | No CSS broadcast, brace-counter, or wrong-plane comparator admission. |
| REDRESS 242-247 | No decoded-string, structural-stream, string64, or fixed-shape unicode retry under old framing. |
| FNV closed enum | Bench-only quarantine; no production migration without a future contract. |

## Section 14 - Dispatch Posture

S-P3 produces this contract; it does not execute it. On S-P3 convergence,
the orchestrator updates `HANDOFF.md` to `ready-for-wave-W0`, runs any
required Pass Omega/G-Omega step for spec amendments, and then dispatches
W0 through the SKINNY triumvirate.

Only G-Omega is a mandatory user gate under the active pin. Every other
gate auto-passes unless an intrinsic invariant violation cannot be
repaired inside the pass discipline.
