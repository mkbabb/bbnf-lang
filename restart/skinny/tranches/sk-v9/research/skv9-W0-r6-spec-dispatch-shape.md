# SK-V9 W0-R6: SPEC/DISPATCH Recovery Wave Draft

Date: 2026-05-18.
Scope: draft the minimal SK-V9 SPEC/DISPATCH shape that makes W0 telemetry-lock first, blocks behavior waves until W0 plus a fresh S-P1 rerun converge, and names measurable falsifiability gates.
Output: this file.

## §1 - Findings (concrete, file:line cited)

1. S-P3 owns the SPEC/DISPATCH artifact pair, but it does not invent scope. The S-P3 prompt says it consumes Pass Alpha's goalset, sequences waves W0..Wn, binds each to a falsifiability gate, and writes `sk-v{N}/SPEC.md` plus `sk-v{N}/DISPATCH-PROMPT.md` (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:10-17`). Its artifact shape requires cited synthesis, deliverable, falsifiability binding, pre-blocked routes, and sources (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:73-92`).

2. The SPEC shape should not be reinvented. The S-P3 prompt requires SK-V8 shape: close condition, comparator classes, outcome enum, required telemetry, opening-row goalset, non-negotiables, wave manifest, phase/LOC caps, Lock 14 gate, per-wave owner/tasks/entry/exit/revert/downstream sections, pre-blocked routes, and dispatch scope (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:94-100`). The bbnf-specific S-P3 axes repeat that W0 is always baseline plus telemetry before behavior waves, gates must name same-row maintain/lift targets, and no candidate may transfer from old hypotheses without a fresh SK-V9 S-P1 profile (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:231-260`).

3. The per-wave triumvirate gives this W0 research file its schema and constrains the later plan/redress shape. Research is read-only and writes one artifact under `restart/skinny/tranches/sk-v{N}/research/`; plan must specify owner paths, falsifiability gate, hard cap, revert protocol, same-wave consumer, and pre-blocked routes; redress must measure or reject (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:11-39`, `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:41-88`). The same-wave consumer rule is load-bearing: every primitive/kernel/new generated path must include and profile its hot-path caller in the same redress commit (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:177-188`).

4. SK-V9 is post-G-Alpha but pre-behavior. The handoff states G-Alpha is closed, S-P1 V1 is only an opening gap ledger, and no `SPEC.md` or `DISPATCH-PROMPT.md` exists yet (`restart/skinny/tranches/sk-v9/HANDOFF.md:5-9`). It then gives the next move: fold S-P1 V1 hardening into recovery-only W0, author SPEC/DISPATCH with W0 first, execute W0 with `gate-json` as same-wave consumer, freeze parser/scanner/SIMD/codegen behavior, and only after W0 rerun S-P1 before releasing behavior S-P2/P3 waves (`restart/skinny/tranches/sk-v9/HANDOFF.md:68-80`).

5. S-P1 V1 is not an empirical ancestor for behavior. The hardening consolidation reports REVISE, 2/6 ACCEPT, and no convergence (`restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md:1-10`). It explicitly says S-P1 V1 is an honest opening ledger, not a completed profile, and lacks SK-V9-open manifest, fresh 17-corpus samply captures, resolved top-symbol tables, PMU/cycles rows, masking-probe telemetry, and fresh SK-V9 delta (`restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md:14-25`). Folded requirements make W0 mandatory, behavior-frozen, and require a fresh S-P1 rerun before behavior candidates are eligible (`restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md:40-58`).

6. W0's scope is a recovery telemetry-lock, not an implementation wave. S-P1 hardening permits W0 to update run identity, report labels, manifest validation, replay metadata, and diagnostic fences, but forbids parser/scanner/SIMD/codegen behavior movement, throughput-cell movement, Apache/CITM measured-row admission, direct-product claims, or strict admission from deferred/view-boundary rows (`restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md:42-48`). The prior gate failure, `twitter SIMD metadata invalid: SIMD metadata is from a different capture`, is direct W0 evidence, not behavior evidence (`restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md:72-83`).

7. Alpha names three behavior routes and two gate-only prerequisites. The behavior routes are Apache/CITM measured typed rows, retained class/event grammar plus `ValueRef` proof, and direct output/control path; the gate-only prerequisites are comparator sidecar same-run manifest and SK-V9-open telemetry/gate refresh (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:37-52`). The cost matrix says sidecar manifest and SK-V9-open telemetry are no-behavior/no-throughput prerequisites consumed by `gate-json`, while measured row additions require their own accepted behavior candidate gate (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:111-120`).

8. SK-V9 close conditions are already measurable. Every admitted change must tie to current `SK-V8-open` rows, fresh measured evidence, and telemetry schema; Apache/CITM need fresh measured `real_typed_struct A / GO` or REDRESS rejection; structural implementation is blocked before retained class/event grammar and retained `ValueRef` proof; direct digest is not product proof without a direct output/control-path tranche; strict comparator claims require strict same-run or gate-consumed structured evidence with output-plane compatibility (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:77-96`).

9. The required telemetry is gate-consumed and broad enough for W0. `skinny/RESULTS.md` and any gate-consumed SK-V9 manifest must carry required columns including grammar/domain/corpus/workload/outcome/verdict/strictness/output plane/Track 1/Track 2/comparator Mbps/deltas/hot leaf/sample cost/profile/run id/host metadata/CostFacts/redress/sidecar freshness/comparator ids and planes/measured validation/substrate surface/structural projection/substrate cardinality/same-wave consumer/Track 2 independence/signal; `gate-json` must reject any row missing required fields (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:242-296`).

10. Strict admission cannot be inferred from current rows. SK-V9 strict admission rejects deferred strictness, stale sidecar-only evidence, lossy/permissive comparator, output-plane mismatch, missing measured validation, missing c/B or sample cost, or missing hot-leaf attribution (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:220-240`). The handoff says all current main rows remain `Strictness=deferred` and C++ sidecars are historical or absent unless a later gate creates a structured same-run manifest (`restart/skinny/tranches/sk-v9/HANDOFF.md:36-38`).

11. PASS-3 runtime commitments are future behavior constraints, not W0 work. Runtime PASS-3 keeps one tape/direct substrate identity, discards `ParseStream`, parallel substrates, rewrite-mode, grammar Unicode algebra, and fixture registries, and requires executable consumer gates rather than prose handoffs (`restart/audit/pass-3-runtime/PASS-3.md:16-23`, `restart/audit/pass-3-runtime/PASS-3.md:138-146`, `restart/audit/pass-3-runtime/PASS-3.md:521-538`). The SK-V9 SPEC/DISPATCH should therefore keep W0 out of runtime API/substrate edits and carry PASS-3 surfaces only as downstream pre-blocked constraints.

## §2 - Recommendations (named falsifiability gates)

### Minimal SPEC schema

Draft `restart/skinny/tranches/sk-v9/SPEC.md` with this minimal section set:

| Section | Required content | W0 recovery rule |
|---|---|---|
| `§0.1 Global Close Condition` | SK-V9 closes only by admitted/rejected measured waves plus preserved route boundaries. | W0 can close only telemetry-lock; it cannot close behavior. |
| `§0.2 Candidate Status` | Three behavior candidates plus two gate-only prerequisites from Alpha. | Mark behavior candidates `blocked-pending-W0-and-S-P1-rerun`. |
| `§0.3 Required Telemetry` | SK-V9 schema copied from SYNTHESIS Section 4.3. | `gate-json` rejects missing/empty required fields; no producer-only fields. |
| `§0.4 Opening Baseline` | Current 38-row `SK-V8-open` state and deferred strictness. | W0 renders `SK-V9-open` run identity without throughput/admission movement. |
| `§1 Non-Negotiables` | Lock 14, no sidecar substrate, no parser-owned fact slots, no public substrate API, no PASS-3 stale surfaces. | Freeze parser/scanner/SIMD/codegen/runtime/product surfaces. |
| `§2 Wave Manifest` | W0 dispatchable; behavior waves listed as conditional placeholders; S-P1 rerun listed as an interlock, not a redress wave. | W0 first; all behavior waves require W0 admit plus fresh converged S-P1. |
| `§2.1 Generality and Lock 14` | Generic-crate edits require CSS L4, Sheets, and BBNF-self proof or demotion to JSON-local. | W0 report/gate code must not encode JSON comparator policy as universal grammar policy. |
| `§3 W0 Telemetry-Lock Recovery` | Owner paths, freeze paths, tasks, entry gate, exit gate, revert protocol, same-wave consumer. | `gate-json` is the same-wave consumer; no row admission. |
| `§4 Conditional Behavior Gates` | Apache/CITM typed rows, retained class/event proof, direct output/control path. | Each remains blocked until `G-BEHAVIOR-RELEASE` passes. |
| `§5 Pre-Blocked Routes` | REDRESS 91/92/93/73 plus sidecar/substrate/public API and generic policy leaks. | Any touched rejected boundary needs changed-shape proof and challenge. |
| `§6 Dispatch Scope` | W0-only initial dispatch; no behavior dispatch from SPEC text alone. | Dispatch prompt must refuse W1+ until release gate passes. |

Recommended minimal wave manifest:

| Slot | Status in initial SPEC | Objective | Consumer | Exit gate |
|---|---|---|---|---|
| W0 | dispatchable after W0 plan | SK-V9-open telemetry-lock recovery: manifest, run identity, schema validation, cache-coherence diagnostics, strict-admission fences. | `cargo xtask gate-json --advisory --check-results` plus report renderer. | `G-W0-TELEMETRY-LOCK`. |
| Interlock: S-P1 rerun | not a wave triumvirate | Fresh S-P1 profile rerun packet over SK-V9-open evidence. | S-P1 hardening/challenge consumes packet. | `G-S-P1-RERUN-CONVERGED`. |
| W1+ behavior placeholders | blocked placeholders | Behavior candidates only after W0 plus fresh S-P1 rerun converge and S-P2/S-P3 reselect. | Candidate-specific: gate/report classifier, generated Track 1 consumer, direct output classifier, or retained cursor proof. | Candidate gate plus full-table maintain. |

### Minimal DISPATCH-PROMPT schema

Draft `restart/skinny/tranches/sk-v9/DISPATCH-PROMPT.md` as a W0-only dispatch contract:

```markdown
# SK-V9 DISPATCH-PROMPT

Entry condition:
- G-Alpha closed.
- S-P1 V1 treated as opening gap ledger only.
- Initial dispatch is W0 only.

Global refusal:
- If requested wave != W0 and `G-BEHAVIOR-RELEASE` is not PASS, refuse dispatch.
- If a plan moves parser/scanner/SIMD/codegen/runtime/product behavior in W0, refuse dispatch.
- If a plan admits strict rows from deferred/view-boundary/stale-sidecar evidence, refuse dispatch.

W0 owned paths:
- `skinny/crates/bbnf-bench/`
- `skinny/xtask/src/` only if needed for `gate-json` pass-through.
- `skinny/RESULTS.md`
- `restart/skinny/tranches/sk-v9/research/skv9-W0-*.md`
- `skinny/REDRESS.md` only if W0 rejects.

W0 freeze paths:
- `skinny/grammars/json.bbnf`
- `skinny/crates/runtime/src/grammars/json/`
- `skinny/crates/runtime/src/tape/`
- `skinny/crates/bbnf-simd/`
- `skinny/crates/codegen/`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/src/track2/`
- `skinny/crates/bbnf-bench/src/parity.rs`
- `skinny/crates/bbnf-bench/src/scan.rs`
- `skinny/crates/bbnf-bench/src/materialization.rs`

W0 plan must specify:
- exact owner paths;
- exact freeze-path diff checks;
- `gate-json` same-wave consumer;
- malformed-manifest and strict-admission rejection tests;
- `skinny/RESULTS.md` generation path;
- revert protocol for report/gate-only edits.
```

### Named gates

`G-W0-TELEMETRY-LOCK`: PASS only if all conjuncts hold:

- `cargo xtask gate-json --advisory --check-results` passes against the SK-V9-open manifest and no cache-coherence mismatch remains.
- The manifest has exactly the current 38 main row identities unless a later accepted S-P3 revision explicitly changes row scope; no W0 row additions, row admissions, outcome upgrades, verdict upgrades, or throughput-cell edits occur.
- Every required SK-V9 telemetry field from SYNTHESIS Section 4.3 is present, non-empty unless explicitly `n/a`/`absent:<reason>`, and consumed by `gate-json`.
- Strict-admission validation rejects deferred strictness, view-boundary output, stale/historical/absent sidecar-only strict claims, output-plane mismatch, missing measured validation, missing sample cost, and missing hot-leaf attribution.
- `parse_only` rows remain non-admission guard rows; Apache/CITM measured typed rows remain absent unless a separate behavior wave later admits them.
- Freeze-path diff checks over parser/scanner/SIMD/codegen/runtime/product/generated/direct/Track2/parity/scan/materialization surfaces are empty.
- W0 writes a machine-readable rerun handoff for S-P1 with run id, profile artifact roots, host/build metadata, feature mask, comparator freshness, and any explicit absent reasons.

`G-S-P1-RERUN-CONVERGED`: PASS only if a fresh post-W0 S-P1 rerun exists and its hardening consolidation converges. Minimum evidence:

- P1-A/P1-B/P1-C have fresh symbol-resolving samply captures over the SK-V9-open run id.
- P1-D has same-run PMU/cycles or a measured rejection explaining host/tool absence.
- P1-E has actual hot-leaf attribution, not Criterion-slope-only substitution.
- P1-F has a fresh delta against SK-V9-open.
- S-P1 hardening returns converged disposition with no behavior candidate sourced from `absent:*`, source-eligible-only, sidecar-historical-only, Criterion-slope-only, or stale SK-V4/SK-V8 fused evidence.

`G-BEHAVIOR-RELEASE`: PASS only if both `G-W0-TELEMETRY-LOCK` and `G-S-P1-RERUN-CONVERGED` pass, then S-P2 and S-P3 re-run or revise their candidate pool against the fresh S-P1 evidence. Before this gate passes, behavior wave sections are planning placeholders and the dispatch prompt must refuse W1+ behavior redress.

`G-TYPED-MEASURED-APACHE-CITM`: candidate-specific future gate. PASS only if selected Apache/CITM `real_typed_struct` rows emit fresh measured `A / GO` rows with generated Track 1 DirectBuild, independent serde/oracle validation, sonic-rs strict same-run parity lane, output-plane compatibility, run-id/metadata validation, full-table maintain, and REDRESS 91 overclaim not reopened. Canada remains blocked until full-fixture DirectBuild-vs-serde checksum proof exists.

`G-STRUCTURAL-PROOF-FIRST`: candidate-specific future gate. PASS only if retained class/event grammar and retained `ValueRef` cursor proof are accepted before any structural-heavy parser/tape implementation. Proof-only waves move no `RESULTS.md` rows. Any later parse implementation must name selected parse rows and meet the SYNTHESIS row thresholds with same-wave generated retained Track 1 consumer, output-plane validation, strict validation posture, and challenge acceptance.

`G-DIRECT-CONTRACT`: candidate-specific future gate. PASS only if selected direct rows meet both Track 1 and Track 2 direct floors from SYNTHESIS Section 4.1, full-table maintain holds, the output/control-path contract is consumed by gate/report row classification, Track 2 remains independent, and REDRESS 93 scalar-parent/parent-digest fold is not reopened.

## §3 - Risks (REDRESS entries to pre-block)

- REDRESS 91 row-table overclaim: Apache/CITM source/product parity is not measured row-table progress; Canada typed remains blocked without full-fixture checksum proof (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:212-218`, `restart/skinny/tranches/sk-v9/SYNTHESIS.md:313-314`).
- REDRESS 92 structural implementation before retained class/event grammar plus retained `ValueRef` cursor proof (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:315-316`).
- REDRESS 93 scalar-parent or renamed parent-digest direct fold without V9-aware checked gate, full-table maintain proof, and independent Track 2 digest-arithmetic backstop (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:317-319`).
- REDRESS 73 helper-shape transfer from generated retained array continuation to hand Track 2 or control-path work without direct hand-parser code-layout profiling (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:320-323`).
- Sidecar substrate, parser-owned cursor/fact slots, `UnionTape`, new `BackendShape`, new directive/BIR, public substrate API, and `tape_vs_tape` as production consumer are pre-blocked (`restart/skinny/tranches/sk-v9/HANDOFF.md:98-100`).
- PMULL prefix-XOR and CTZ/bulk production rewires as default hot paths, generic JSON policy leaks, and Lock 14 weakening remain pre-blocked (`restart/skinny/tranches/sk-v9/HANDOFF.md:101-108`).
- PASS-3 stale public-surface hazards: do not introduce `ParseStream`, parallel substrates, rewrite-mode, grammar Unicode algebra, fixture registries, or prose-only consumer acceptance into SK-V9 W0 (`restart/audit/pass-3-runtime/PASS-3.md:16-23`, `restart/audit/pass-3-runtime/PASS-3.md:561-570`).
- Anti-paper-close risk: W0 must not close by writing a manifest that `gate-json` does not consume, by leaving required fields as producer-only text, or by letting behavior candidates cite S-P1 V1 absence-coded rows. The S-P1 hardening explicitly blocks behavior until W0 plus revised S-P1 data cycle converge (`restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md:53-58`, `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md:85-90`).

## §4 - Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/audit/pass-3-runtime/PASS-3.md`
- `restart/skinny/tranches/sk-v9/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v9/HANDOFF.md`
- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v8/research/wave-0-plan.md`

Self-verdict: ACCEPT as W0 research schema; this file does not dispatch behavior or edit SPEC/DISPATCH.
