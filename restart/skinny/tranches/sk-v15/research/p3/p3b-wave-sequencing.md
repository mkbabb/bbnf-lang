# SK-V15 P3-B: Wave Sequencing

Pass: S-P3 Synthesis-Plan. Cycle: V2.
Date: 2026-05-28.
Scope: Order SK-V15 into a cap-valid W0..W11 wave graph after V1
hardening.
Output: this file.

## Section 1 - Synthesis

P3-B is a planning artifact. It is read-only against `skinny/` and
implementation sources. No SK-V15 implementation wave dispatches until
S-P3 converges and the required G-Omega authorization closes.

The final SK-V15 wave order is:

`W0 -> W1 -> W2 -> W3 -> W4 -> W5 -> W6 -> W7 -> W8 -> W9 -> W10 -> W11`

This is 12 waves, exactly the skinny-bracket ceiling. No additional
top-level wave may be added without removing or folding an existing wave.
The two V2 splits are load-bearing:

- old W5 splits into W5 CSS typed provider and W6 same-workload retime /
  old-proof retirement;
- old W7 splits into W8 lowerer fixture/EagerTape/OffsetTape and W9
  EventTape/SinkOnly/CollapsedStage plus the all-five gate.

Every implementation wave uses the SK-V15 Alpha caps:

| Phase | Hard cap | Rule |
|---|---:|---|
| Research | <=20 minutes | Read-only; may end with intrinsic block. |
| Plan | <=15 minutes | One intervention, owner paths, gates, revert route. |
| Redress | <=30 minutes | Implement, measure, and admit, redress, revert, or block. |

At 0.9x cap the wave commits or checkpoints. At cap it halts with
evidence. Challenge time is not implementation overflow.

## Section 2 - Deliverable Wave Table

Common entry gate for every wave: S-P3 convergence, G-Omega authorization,
dirty/staged inspection, unrelated work preserved, owner paths named, and
wave plan committed before redress.

Manual LOC excludes generated output. Generated output may be large only
when deterministic, attributed to a named generator, and checked by a
non-writing command or regenerated in the same wave.

| Wave | Receiver | Risk class | Manual source/test LOC | Generated LOC status | Docs/ledger LOC | Entry gate | Owner path family | Exit / same-wave consumer |
|---|---|---|---:|---|---:|---|---|---|
| W0 | Baseline and telemetry lock | Medium ledger/gate | 60-160 | None; behavior unchanged. | 80-180 | S-P3 and G-Omega closed. | `skinny/RESULTS.md`, `skinny/REDRESS.md`, `restart/skinny/ROLLING-SOTA-DELTA.md`, gate/report/telemetry surfaces. | Gate consumes the exact SK-V15 fields from P3-D: `measurement_row_id`, `measurement_origin`, `value_plane`, `css_comparator_workload`, `generator_source`, `lock14_scan_scope`, `lock16_status`, `checkasm_or_parity_status`, `gate_exclusion_report`, `broadcast_group_id`. CSS broadcast is diagnostic; no provider deletion. |
| W1 | CSS admission honesty | Medium ledger/gate | 80-200 | None; CSS providers remain live unless W6-grade proof also lands. | 80-180 | W0 admitted. | CSS bench/report/gate surfaces and rolling delta. | 24 CSS broadcast admits are demoted or collapsed to one diagnostic aggregate. W8R values are negative fixtures only. CSS report/gate consumes broadcast identity and rejects hidden one-to-N measurement. |
| W2 | Lock 14 / Lock 16 gate restoration | High gate repair | 120-280 | Gate fixtures only; not scan coverage. | 80-180 | W0 admitted and W1 has removed CSS admission pressure or blocked CSS as non-admission. | Lock 14/16 gates, reports, checkasm manifests, scan roots. | Gate prints included roots, excluded roots, exclusion reasons, and scan of its own exclusion list. Lock 16 source-present primitives are wired, scalar-delegated, deleted, or blocked. |
| W3 | Codegen leak abrogation | High codegen prune | 150-320 | Generated runtime diffs are proof outputs, not manual source. | 80-180 | W2 admitted. | `skinny/crates/codegen/`, `skinny/crates/passes/`, `skinny/crates/ir/`, `skinny/xtask/`. | One coherent generic leak family is removed. Generic edits require non-JSON proof through CSS L4 and one of Sheets or BBNF-self when feasible, or intrinsic block. JSON-adjacent changes rerun JSON 51/51. |
| W4 | Pattern H generated discipline | High provenance/gate | 100-260 | Root runtime generated output checked or regenerated in-wave. | 80-180 | W2 admitted and W3 admitted/routed. | `crates/core/src/runtime/**`, skinny runtime projections, `skinny/xtask`. | 67 root runtime files have true line-1 generator provenance and non-writing regen/check proof. Destructive root deletion is blocked until delete-plus-regen proof. |
| W5 | CSS typed Value provider | High CSS provider | 180-360 | CSS generated output allowed only from named grammar/codegen provider. | 80-180 | W1-W4 admitted/routed with no open delete dependency. | `grammar/css/l4/**`, CSS runtime provider/value/document/view/visitor surfaces, CSS tests. | Typed CSS value/document/view/visitor provider exists and is gate-consumed by a typed provider proof or one diagnostic aggregate row. Old CSS proof is not retired unless W6-grade proof also lands. Generic generator/provider edits also prove Sheets or BBNF-self stability. |
| W6 | CSS retime and old-proof retirement | High benchmark/gate | 160-340 | Generated CSS output must already be provider-backed. | 100-220 | W5 admitted. | CSS bench/report/gate, RESULTS, rolling delta, old CSS proof contracts. | Fresh same-run cssparser typed-value/document comparison exists after Track 1 emits typed CSS output. `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only `parse()`, and brace-counter proof retire only with same-wave typed provider proof. JSON 51/51 maintains >=98% if behavior changed. |
| W7 | Decision Engine spine | High generic decision | 140-300 | Generated selection fixtures only. | 80-180 | W6 admitted or intrinsically routed. | `backend_egraph.rs`, `decision_csp.rs`, passes, IR, decision tests. | At least one asserted e-graph rewrite and non-tautological CSP condition are consumed by a decision gate/test/generated-selection fixture. Facts are grammar-neutral; non-JSON receiver proof is required. |
| W8 | Lowerers A: EagerTape and OffsetTape | High lowerer | 140-300 | Fixture output must fail against old scaffold. | 80-180 | W7 admitted. | Lowerer fixture harness, EagerTape, OffsetTape, generated fixtures. | EagerTape and OffsetTape lowerers are real output paths or gate-consumed rejected alternatives. CSS L4 plus Sheets or BBNF-self fixtures exercise generic lowerer paths. |
| W9 | Lowerers B: EventTape, SinkOnly, and CollapsedStage | High lowerer | 160-340 | Fixture output must fail against old scaffold. | 80-180 | W8 admitted. | EventTape, SinkOnly, CollapsedStage, all-five lowerer gate. | EventTape/SinkOnly/CollapsedStage lowerers are real or rejected by gate. EventTape is only an existing BackendShape lowering; no sixth shape, sidecar event vector, retained parser-owned stream, public substrate API, or alternate projection. |
| W10 | FNV quarantine | Medium bench/gate | 80-220 | Bench fixture output only. | 80-180 | W9 admitted/routed; if W9 blocks, W10 needs independence proof. | Bench/xtask FNV helpers, strict-product gates, adversarial fixtures, production FNV scan. | FNV remains bench-only and cannot be runtime selector, arbiter, or correctness proof. Strict-product gate consumes adversarial semantic fixtures and quarantine metadata. |
| W11 | Close reconciliation and PASS-IMPL V2 handoff | Medium close | 0-80 | None except close artifacts. | 120-260 | W1-W10 admitted, reverted, redressed, or intrinsically blocked. | RESULTS, REDRESS, rolling delta, HANDOFF, PASS-IMPL V2 packet. | PASS-IMPL V2 accepts each axis or records row-level intrinsic-block proof at HEAD. SK-V16 routing is routed remainder after proof, never SK-V15 close evidence. |

## Section 2.1 - Generality / Lock 14 Gate

Every wave plan that touches generic crates or generic generators must carry
this table. A missing row rejects redress.

| Required column | Meaning |
|---|---|
| Generic owner path | Exact generic crate, pass, generator, lowerer, or SIMD/parser-helper file touched. |
| Forbidden token scan | `Json`, `CssL4`, Sheets/corpus names, JSON structural roles, CSS profile names, and aliases. |
| Non-JSON receiver | CSS L4 plus Sheets or BBNF-self when the generic behavior can affect multiple grammars; otherwise the plan records intrinsic block with proof. |
| Proof command | Regen/check/test command that exercises the changed generic path for the receiver. |
| Generated-output expectation | Byte-identical no-diff, named generated diff, or gate-consumed rejected alternative. |
| Fail action | Revert, REDRESS, scalar-delegate, delete, or intrinsic block. |

CSS-only owner paths may close with CSS proof. If W5 or W6 touches generic
generator/provider/codegen files, it also needs Sheets or BBNF-self
stability proof.

## Section 3 - NEW-CH3-V5-01 Dependency Obligations

Every delete, retirement, demotion, or neutralization keeps a visible row
in the final SPEC and DISPATCH packet.

| Retired/deleted artifact | Delete/retire wave | Rebuild provider wave | Proof command | Provider no later than delete/retire | Status |
|---|---|---|---|---|---|
| CSS 24-row full-parse admission claim from one timing tuple | W1 | W1 gate/report telemetry | Gate over `measurement_row_id`, `broadcast_group_id`, `measurement_origin`, row verdicts. | Yes; demotion only. | Diagnostic-only. |
| `CSS_GENERATED_RS` live parser evidence and static CSS companion body | W6 | W5 typed CSS provider plus W6 typed retime | `rg -n "CSS_GENERATED_RS|CssFullParseSummary|Result<String, CssFactError)"` over live contracts plus typed CSS tests and same-workload bench. | Must be yes in W6. | Blocked before W6. |
| `CssFullParseSummary`, fact-stream `parse()`, brace-counter summary | W6 | W5 typed CSS value/document/view/visitor provider | CSS semantic tests, same-workload cssparser comparison, and live-contract `rg`. | Must be yes in W6. | Blocked before W6. |
| CSS provider/template/static profile roster and runtime family fanout | W3 neutralizes; W6 deletes live CSS proof if needed | W3 generic contract plus W5/W6 typed CSS provider/proof | Lock 14 scan, non-JSON receiver proof, JSON guard if adjacent, CSS typed proof for deletion. | W3 may neutralize; deletion waits for W6. | Blocked for deletion before W6. |
| Root Pattern H runtime files lacking generated provenance | W4 provenance; destructive deletion only after W4 proof | W4 root runtime generator/check | 67-file count, line-1 provenance, non-writing root regen/check or delete-plus-regen proof. | Must be yes before destructive delete. | Conditional. |
| CSS `LegacyPath` or equivalent root runtime shim | W4 only with replacement proof; otherwise W5/W6 | W4 root projection generator/check or W5/W6 CSS typed provider | `rg -n "LegacyPath|LegacySegment"` plus root regen/check or CSS typed proof. | Must be yes same wave. | Blocked without proof. |
| Decision Engine scaffold status fields | W7 | W7 e-graph/CSP provider | Decision gate/test/generated-selection fixture and scaffold-string grep. | Must be yes in W7. | Conditional. |
| Label-string lowerers | W8/W9 | W8/W9 real lowerers | Lowerer fixtures that fail against old scaffold plus generated-output diff/rejected alternative. | Must be yes in W8/W9. | Conditional. |
| W11L/N/O FNV closed-enum/hash-sidecar correctness claim | W10 | W10 adversarial semantic fixtures and bench-only quarantine | Production `rg -n "fnv|FNV"` plus strict-product adversarial tests. | Yes for quarantine. | Bench-only. |

## Section 4 - Pre-Blocked Routes

The shared pre-block list for P3-B/P3-C/P3-E/P3-F/SPEC/DISPATCH is:
28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98,
183/184/209-213, 215, 242-247, and FNV closed-enum production
migration. Old-framing reuse is rejected.

## Section 5 - Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/prompts/ORCHESTRATOR.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v15/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v15/HANDOFF.md`
- `restart/skinny/tranches/sk-v15/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v15/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v15/research/p3/hardening/V1/redeploy/`
