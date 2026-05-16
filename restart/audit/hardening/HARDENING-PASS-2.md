# HARDENING-PASS-2 — Double-back audit

## §1 Target identification

Target: `PASS-2`.

Audited outputs:

| Output | Lines audited |
|---|---:|
| `restart/audit/pass-2-codegen/PASS-2.md` | 1-391 |
| `restart/audit/pass-2-codegen/agent-1-backend-ir-architect.md` | 1-107 |
| `restart/audit/pass-2-codegen/agent-2-rust-lowerer-architect.md` | 1-103 |
| `restart/audit/pass-2-codegen/agent-3-wasm-lowerer-simd-architect.md` | 1-69 |
| `restart/audit/pass-2-codegen/agent-4-runtime-template-architect.md` | 1-103 |
| `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md` | 1-93 |
| `restart/audit/pass-2-codegen/agent-6-codegen-coherence-auditor.md` | 1-83 |

Authority read:

| Source | Authority surface |
|---|---|
| `restart/README.md` | Tape/direct-to-struct union, two onboarding surfaces, workspace ownership, performance gates, voice discipline (`restart/README.md:13-25`, `restart/README.md:31-58`, `restart/README.md:285-314`, `restart/README.md:324-340`, `restart/README.md:450-452`) |
| `restart/locks/LOCKS.md` | 14-lock audit standard; especially Locks 1, 5, 8, 13, 14 (`restart/locks/LOCKS.md:34-60`, `restart/locks/LOCKS.md:118-125`) |
| `docs/precepts/instructions/STYLE.md` | direct, calibrated audit voice (`docs/precepts/instructions/STYLE.md:5-16`, `docs/precepts/instructions/STYLE.md:75-91`) |
| `docs/precepts/instructions/LESSONS-LEARNED.md` | same-wave consumer, producer/consumer gates, generated-size budget (`docs/precepts/instructions/LESSONS-LEARNED.md:17-26`, `docs/precepts/instructions/LESSONS-LEARNED.md:74-80`, `docs/precepts/instructions/LESSONS-LEARNED.md:274-292`) |
| `docs/precepts/instructions/CONSUMING.md` | shared precepts stay read-only here (`docs/precepts/instructions/CONSUMING.md:13-20`, `docs/precepts/instructions/CONSUMING.md:69-72`) |
| `restart/prompts/PASS-1-SUBSTRATE.md` | PASS-1 contract consumed by PASS-2 (`restart/prompts/PASS-1-SUBSTRATE.md:57-70`) |
| `restart/prompts/PASS-2-CODEGEN.md` | target scope and expected output shape (`restart/prompts/PASS-2-CODEGEN.md:3`, `restart/prompts/PASS-2-CODEGEN.md:30-37`, `restart/prompts/PASS-2-CODEGEN.md:47-60`) |
| `restart/prompts/PASS-3-RUNTIME.md` | PASS-3 owns user API, diagnostics ledger, ecosystem (`restart/prompts/PASS-3-RUNTIME.md:24-35`, `restart/prompts/PASS-3-RUNTIME.md:49-56`, `restart/prompts/PASS-3-RUNTIME.md:83`) |
| `restart/prompts/SYNTHESIS.md` | synthesis ownership of authoritative architecture and reconciliation (`restart/prompts/SYNTHESIS.md:26-41`, `restart/prompts/SYNTHESIS.md:112-119`) |
| `restart/corpora/CENSUS.md` | grammar-specific leaks, god modules, runtime duplication, generated LOC surfaces (`restart/corpora/CENSUS.md:103-123`, `restart/corpora/CENSUS.md:321-354`, `restart/corpora/CENSUS.md:435-527`) |
| `restart/corpora/MODULES.md` | `simd-scan` keep, codegen/runtime surfaces, pipeline ordering (`restart/corpora/MODULES.md:47-69`, `restart/corpora/MODULES.md:735-879`, `restart/corpora/MODULES.md:914-1000`, `restart/corpora/MODULES.md:1170-1289`) |
| `restart/corpora/RESTART-SKETCH.md` | OpenFrame pathology and direct-projection/tape inheritance conflict (`restart/corpora/RESTART-SKETCH.md:154-184`, `restart/corpora/RESTART-SKETCH.md:273-285`, `restart/corpora/RESTART-SKETCH.md:367-379`) |
| `restart/corpora/SOTA.md` | sonic-rs, simdjson, lightning-css competitor anchors (`restart/corpora/SOTA.md:50-58`, `restart/corpora/SOTA.md:73-89`, `restart/corpora/SOTA.md:130-136`) |
| `restart/inheritance/INDEX.md` | legacy carry ledger and stale ParseStream/rewrite-mode residues (`restart/inheritance/INDEX.md:29-40`, `restart/inheritance/INDEX.md:58-68`) |

Audit commit basis: `015317db`.

Time consumed: 43 minutes.

Final verdict: **AMENDMENT-REQUIRED**.

The target survives as a PASS-2 architecture: Backend IR boundary, Tape-backed runtime template, BIR-only Rust/WASM lowerers, generated budget, xtask equality, and generic `simd-scan` all stand. It does not yet advance cleanly because Backend IR ownership drifts into `codegen`, the Lock 14 `yaml.bbnf` onboarding proof is absent, PASS-2-owned diagnostic/friction surfaces lack verbatim gates, and deferrals need receiver/blocker/gate closure.

## §2 Cohort verdict

| Lane | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|
| 1 — Lock-Adherence | AMENDMENT-REQUIRED | 10 | 3 | 1 | Keep the thesis; move Backend IR ownership to `ir`, add Lock 14 onboarding, tighten SOTA row claims. |
| 2 — Sequencing Discipline | N/A | 1 | 0 | 0 | PASS-2 is a single pass, not a multi-wave execution plan; carries are audited in Lane 8. |
| 3 — Cohesion | AMENDMENT-REQUIRED | 5 | 2 | 0 | Add source-of-truth ownership and consumer acceptance gates for the BIR/runtime contracts. |
| 4 — SOTA Anchoring | AMENDMENT-REQUIRED | 4 | 2 | 0 | Separate mechanism rows from throughput gates; every throughput row must name competitor, dataset, platform. |
| 5 — Grammar-Authoritative Discipline | AMENDMENT-REQUIRED | 4 | 3 | 0 | Greps pass for match arms; add `yaml.bbnf` and per-X tables for broad grammar claims. |
| 6 — Generated-Code + LOC Budget | AMENDMENT-REQUIRED | 3 | 2 | 0 | Per-grammar generated budget stands; add non-generated module/file budget and baseline for snapshot timing. |
| 7 — Friction Forecast | AMENDMENT-REQUIRED | 2 | 4 | 0 | Add PASS-2-owned verbatim diagnostics and cookbook handoff gates. |
| 8 — Carry & Deferral Audit | AMENDMENT-REQUIRED | 3 | 2 | 0 | Add receiver/blocker/receiving-gate ledger for every defer/carry. |
| 9 — Greenfield Discipline | AMENDMENT-REQUIRED | 6 | 2 | 0 | Replacement posture is sound; ownership and genericity proofs require surgery. |
| **Total** | **AMENDMENT-REQUIRED** | **38** | **20** | **1** | **Nine-item punch list before PASS-2 advances.** |

Final decision: **requires amendments**.

## §3 Lane 1 — Lock-Adherence

Lane standard: every one of the 14 locks is settled authority. PASS-2 need not own every lock fully, but it must either honour it at its layer or declare the nonownership without contradiction. Particular force falls on Lock 1, Lock 5, Lock 8, Lock 13, and Lock 14 for this target.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:13`, `restart/audit/pass-2-codegen/PASS-2.md:36` | Lock 1: Tape/direct-to-struct | PASS-2 names Tape as the runtime substrate and couples typed views to `&'i Tape<'i>` plus node id. | Honours the settled Tape authority in `restart/locks/LOCKS.md:34` and `restart/README.md:285-314`; it kills OpenFrame checkpoint clones using the corpus pathology at `restart/corpora/RESTART-SKETCH.md:154-184`. | The target still inherits prompt/corpus contradictions and does not give a Tape record byte-layout; PASS-3 owns final API layout. | A hardener could demand PASS-2 fully specify tape byte layout. That fails scope: PASS-2 owns codegen/runtime template, and it supplies enough shape for lowerers while handing API/layout finalization to PASS-3. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:67`, `restart/audit/pass-2-codegen/agent-4-runtime-template-architect.md:82` | Lock 2: layout-lowering canon | PASS-2 has a `Layout` BIR node and template parameter for layout policy. | It keeps `@layout` in V1 and does not resurrect TypeDesc/StructLayout as runtime literals. | It does not spell the canonical `passes/layout/` owner because PASS-1 owns the pass. | The counter-position is that codegen cannot lower layout safely unless the upstream canonical pass is named. Add a handoff row naming PASS-1 layout output and BIR `Layout` consumer. | REINVENT |
| `restart/audit/pass-2-codegen/agent-2-rust-lowerer-architect.md:34`, `restart/audit/pass-2-codegen/PASS-2.md:260-273` | Lock 3: cursor + byte-skip unified | PASS-2 refuses to resurrect `runtime/path.rs` and hands selectors to PASS-3. | It avoids a second path alphabet; CENSUS marks the duplicate runtime path as deletion surface (`restart/corpora/CENSUS.md:237-265`). | PASS-2 lacks a parse-signature gate proving eager/lazy wrappers consume one generated parse core. | The steelman is that codegen can accidentally emit two parse implementations. Add a gate that generated `parser.rs` has one parse core plus wrapper signatures. | REINVENT |
| `restart/audit/pass-2-codegen/PASS-2.md:32`, `restart/audit/pass-2-codegen/PASS-2.md:280-289` | Lock 4: per-domain optimization | PASS-2 consumes PASS-1 outputs: cost scores, shape mining, e-graph extraction, host inference. | It does not fuse CSP/e-graph/cost into codegen; PASS-2 remains a consumer. | It trusts unavailable PASS-1 artefacts during parallel dispatch. | The challenge is that assumptions can drift. The synthesis reconciliation gate named at `restart/audit/pass-2-codegen/PASS-2.md:291` covers the drift if made explicit in Lane 8. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:32`, `restart/audit/pass-2-codegen/PASS-2.md:139-181`, `restart/audit/pass-2-codegen/agent-1-backend-ir-architect.md:25` | Lock 5: IR + per-backend lower | Lowerers consume Backend IR only; no lowerer imports Grammar IR. | The boundary is the right codegen contract and replaces the current GrammarIR-walking driver (`restart/audit/pass-2-codegen/PASS-2.md:5`). | PASS-2 places `backend_ir/` under `codegen/src/`, while README says `ir` owns Grammar IR plus Backend IR types (`restart/README.md:43`, `restart/README.md:108-112`). | The strongest counter-position is ownership clarity: IR definitions in `codegen` make codegen both contract owner and consumer. That defeats the current tree. Move the BIR type tree to `ir/src/backend_ir/`; keep only lowerer adapters in `codegen`. | DISCARD |
| `restart/audit/pass-2-codegen/PASS-2.md:46`, `restart/audit/pass-2-codegen/PASS-2.md:243-258` | Lock 6: xtask emits committed source | PASS-2 keeps content-equality writing and splits `xtask` regen. | Honours committed generated artefacts, no proc-macro façade; cites current content-equality writing (`xtask/src/regen.rs:400-461`). | It does not give per-module LOC budgets for the new split. | The codegen target can still pass with generated equality while hand-written regen modules bloat. Lane 6 covers the missing non-generated budget. | KEEP |
| `restart/audit/pass-2-codegen/agent-2-rust-lowerer-architect.md:34`, `restart/audit/pass-2-codegen/PASS-2.md:260-273` | Lock 7: path crate consolidation | PASS-2 keeps path ergonomics out of runtime template and defers selectors to PASS-3. | It respects CENSUS's duplicate-path deletion (`restart/corpora/CENSUS.md:237-265`) and PASS-3's path ownership (`restart/prompts/PASS-3-RUNTIME.md:31`). | The generated parse signatures are not tied to `path-core` consumer acceptance. | Add a PASS-3 handoff gate: `path-core` can type-check against emitted registry metadata without reading generated internals. | REINVENT |
| `restart/audit/pass-2-codegen/PASS-2.md:321-335` | Lock 8: SOTA | PASS-2 cites sonic-rs, simdjson, lightning-css numbers and declines final perf claims. | The global anchor is sound and cites SOTA corpus lines for JSON, scan, and CSS (`restart/corpora/SOTA.md:50-58`, `restart/corpora/SOTA.md:73-89`, `restart/corpora/SOTA.md:130-136`). | The trajectory table mixes mechanism rows with gate-like rows without competitor/dataset/platform per row. | The challenge wins partly: mechanism rows are allowed, but any parse-throughput gate must carry full Lock 8 tuple. Reword rows or add tuple columns. | REINVENT |
| `restart/audit/pass-2-codegen/agent-4-runtime-template-architect.md:94`, `restart/audit/pass-2-codegen/PASS-2.md:98-116` | Lock 9: slice-borrow primary | PASS-2 emits borrowed payloads and owned parse remains explicit. | It prepares the runtime template for PASS-3 parse surfaces. | It lacks final `parse / parse_in / parse_owned` signatures, which PASS-3 owns. | A codegen pass can stop at parameter schema if PASS-3 receives enough shape. That is true; no amendment beyond the handoff gate. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:128-135`, `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:22-30` | Lock 10: Pratt/SIMD/PHF auto-detected | PASS-2 states recognizers are auto-detected and logged; no directives. | Directly honours `restart/locks/LOCKS.md:52` and `restart/README.md:180-182`. | Verbatim misfire diagnostics are absent. | The architecture survives; user friction is Lane 7 surgery. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:65`, `restart/audit/pass-2-codegen/PASS-2.md:226` | Lock 11: path-deps for incubating sister crates | PASS-2 keeps Unicode in `parse-that/regex` and host primitives generic. | It avoids grammar-level Unicode algebra, matching README lines `131-143`. | It does not say whether `parse-that`/regex remains path-dep until stable. | This is minor but real: add a one-line nonownership note so PASS-2 does not imply a publication decision. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:137-258` | Lock 12: ser/gorgeous archive precondition | PASS-2 does not touch archive ceremony. | The target does not modify execution sequencing; it is a PASS architecture audit. | It is silent. | Silence is acceptable only if synthesis owns Tranche A. PASS-2 should not carry archive ceremony; no surgery here. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:137-258`, `restart/audit/pass-2-codegen/PASS-2.md:310` | Lock 13: no god directories | PASS-2 gives codegen/runtime/host/simd/xtask trees and exempts generated files from file cap only. | It responds to CENSUS god modules (`restart/corpora/CENSUS.md:321-354`) and Lock 13 (`restart/locks/LOCKS.md:58`). | The tree lists modules but not target file counts or child-count proof. | Add a non-generated LOC/child-count budget table. The architecture remains sound. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:44`, `restart/audit/pass-2-codegen/PASS-2.md:48`, `restart/audit/pass-2-codegen/agent-4-runtime-template-architect.md:23` | Lock 14: full grammar generalisation | PASS-2 rejects per-grammar declaration crates by default and template-emits runtime modules. | This honours the two onboarding surfaces (`restart/README.md:13-25`) and rejects hardcoded grammar arms. | The target lacks the required future-grammar `yaml.bbnf` onboarding test and per-X table for all broad grammar claims. | The target cannot advance without this proof. Add `yaml.bbnf` source+metadata-only gate and per-grammar output-shape table. | REINVENT |

Lane verdict: **AMENDMENT-REQUIRED** — KEEP 10 / REINVENT 3 / DISCARD 1.

Surgery list:

| Surgery | Source |
|---|---|
| Move Backend IR type ownership to `ir/src/backend_ir/`; `codegen` consumes it. | DISCARD, Lock 5 |
| Add `yaml.bbnf` two-surface onboarding test and per-X runtime emission table. | REINVENT, Lock 14 |
| Add parse-core/path handoff and non-generated LOC/child-count gates. | REINVENT, Locks 3/7/13 |

## §4 Lane 2 — Sequencing Discipline

Lane standard: multi-wave targets must prove every substrate has a same-wave or next-wave consumer. This target is a single PASS-level architecture output, not an execution-wave plan. PASS-2 contains inheritance wave carries, but those are legacy carry ledgers rather than sequencing claims for implementation waves; Lane 8 audits them.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:360-372` | Legacy wave-by-wave carries | PASS-2 maps BC/BD wave substance into the greenfield codegen layer. | It avoids pretending PASS-2 itself executes multi-wave work; it marks TS production and BD parity as downstream. | The row title says "Wave-by-wave", which can be misread as sequencing discipline. | The hardener could force Lane 2 to apply. That overreads the target: these are inheritance rows, not executable waves. Lane 8 remains the correct audit lane. | KEEP |

Lane verdict: **N/A** — KEEP 1 / REINVENT 0 / DISCARD 0.

Surgery list: none.

## §5 Lane 3 — Cohesion

Lane standard: every claim must be verifiable from artefacts the target produces or cites. PASS-2 must not leave orphan claims, orphan deliverables, or contracts whose producer/consumer pair is absent.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:50-76` | 23-variant BIR table | The table fixes the codegen node alphabet and per-backend lowering obligations. | It is concrete, bounded, and defends cardinality using BC research anchors (`restart/audit/pass-2-codegen/PASS-2.md:78`). | Ownership is wrong when the tree places BIR under `codegen`. | The variant set survives; the namespace does not. Cohesion requires one owner: `ir`. | REINVENT |
| `restart/audit/pass-2-codegen/PASS-2.md:80-96` | `BackendLowerer` API | The 8-method surface collapses the old broad `Emitter` trait. | It is verifiable against current trait bloat cited at `crates/core/src/backend/emitter.rs:31-566`. | It lacks import-deny gate text in the main synthesis; agent 6 has a partial `rg GrammarIR` check (`restart/audit/pass-2-codegen/agent-6-codegen-coherence-auditor.md:39-43`). | A lowerer API without an import-deny gate can drift back to Grammar IR. Add the gate to the PASS-2 punch list. | REINVENT |
| `restart/audit/pass-2-codegen/PASS-2.md:98-116` | runtime template parameter schema | PASS-2 names the inputs each generated runtime file consumes. | The schema cites the BB template precedent and keeps regen equality. | It does not enumerate every current grammar's emitted file set. | The schema is sound; Lock 14 needs per-X proof. Lane 5 surgery covers it. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:118-126` | SIMD coverage matrix | PASS-2 names aarch64, x86_64, wasm32, scalar kernels. | It is backed by current `simd-scan` module inventory and dispatch. | It does not state fixture dimensions per emitted alphabet. | Agent 3 adds parity fixtures (`restart/audit/pass-2-codegen/agent-3-wasm-lowerer-simd-architect.md:42`); sufficient for PASS-2. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:260-273` | PASS-3 handoffs | PASS-2 hands parse signatures, views, visitor hooks, error vocabulary, host metadata, WASM ABI, materialisation cost to PASS-3. | This matches PASS-3 ownership of user API and ecosystem (`restart/prompts/PASS-3-RUNTIME.md:24-35`). | Consumer acceptance tests are not listed. | Add acceptance gates, but the handoff categories are correct. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:276-291` | PASS-1 handoffs | PASS-2 states the upstream products it assumes from PASS-1. | It is honest about parallel dispatch and names SYNTHESIS as reconciler. | It lacks a receiving gate name for synthesis reconciliation. | Lane 8 surgery adds receiver/blocker/gate; the contract shape is coherent. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:374-385` | PASS-2 punch list | The punch list names implementation tasks. | It captures BIR, runtime tape, xtask, SIMD, Rust/WASM ordering, and conflict guards. | It does not route amendments found by this hardening; that is expected because this file predates hardening. | The target needs amendment, not re-draft. | KEEP |

Lane verdict: **AMENDMENT-REQUIRED** — KEEP 5 / REINVENT 2 / DISCARD 0.

Surgery list:

| Surgery | Source |
|---|---|
| Add import-deny gate to main PASS-2 synthesis: lowerers and runtime template may not import Grammar IR; only the PASS-1 BIR producer may. | REINVENT |
| Re-home BIR definitions in `ir`; codegen owns lowerers only. | REINVENT |

## §6 Lane 4 — SOTA Anchoring

Lane standard: every parse-throughput gate names competitor, dataset, and platform. Mechanism gates may cite SOTA as rationale, but must not claim Lock 8 closure unless the tuple is present.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:323` | global perf target paragraph | PASS-2 gathers JSON/CSS/scan gates from README and SOTA. | It cites sonic-rs M1 Pro rows (`restart/corpora/SOTA.md:50-58`), simdjson On-Demand (`restart/corpora/SOTA.md:73-89`), and lightning-css CSS rows (`restart/corpora/SOTA.md:130-136`). | It is one paragraph, not a per-gate table. | The global citation is sufficient as an orientation, not as a row-level close gate. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:327-333` | trajectory table | PASS-2 maps mechanisms to future evidence artefacts. | It correctly refuses final perf wins at `restart/audit/pass-2-codegen/PASS-2.md:335`. | Rows such as "JSON structural scan" and "CSS scanner/layout" look like gates but omit competitor/dataset/platform. | The challenge wins partially: re-label as mechanism trajectory or add tuple columns. | REINVENT |
| `restart/audit/pass-2-codegen/agent-3-wasm-lowerer-simd-architect.md:20` | WASM perf row | Agent 3 references WASM twitter ≤2.5ms from BD.W2. | It explicitly refuses BD parity overclaim. | The competitor anchor is absent and BD.W2 is not a SOTA source. | PASS-2 may carry WASM smoke, not Lock 8 throughput. Move this under carry/deferral or add competitor tuple if retained. | REINVENT |
| `restart/audit/pass-2-codegen/PASS-2.md:337-347` | per-construct contribution plan | PASS-2 explains how constructs close the SOTA gap. | Each construct cites a source rationale: OpenFrame pathology, simdjson scan, regex Unicode, Pratt LUT, host chains, layout. | It is qualitative. | Qualitative mechanisms are allowed because line 335 blocks final win claims. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:118-126` | SIMD platform matrix | PASS-2 names NEON, AVX2, AVX512, wasm-simd128, scalar. | It matches README platform scope (`restart/README.md:338-340`) and current `simd-scan` modules. | It is a correctness/parity matrix, not throughput. | It does not claim Lock 8 closure; keep. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:329` | OpenFrame cost removal | PASS-2 uses samply pathology to justify TapeBuilder checkpoints. | It cites the old 86.07% cost and gives the root-cause mechanism. | No competitor is named. | This is not a parse-throughput gate; it is root-cause surgery. No competitor needed. | KEEP |

Lane verdict: **AMENDMENT-REQUIRED** — KEEP 4 / REINVENT 2 / DISCARD 0.

Surgery list:

| Surgery | Source |
|---|---|
| Rewrite §7 trajectory rows to include `Competitor / Dataset / Platform / bbnf target / PASS-2 mechanism`, or label rows as non-Lock-8 mechanism gates. | REINVENT |
| Remove the BD.W2 WASM twitter row from SOTA closure unless a competitor tuple is supplied. | REINVENT |

## §7 Lane 5 — Grammar-Authoritative Discipline

Lane standard: generic crates must carry zero grammar-specific code; per-grammar data belongs in grammar source, workspace metadata, `@host fn`, or generated runtime output. The target must include per-X tables for broad grammar claims and the future-grammar onboarding test.

Required greps run against `restart/audit/pass-2-codegen/PASS-2.md` plus `agent-*.md`:

```text
rg -ni 'json|css_l4|bbnf|google_sheets|sheets|css_pretty|bnf|csv|ebnf|math' restart/audit/pass-2-codegen/PASS-2.md restart/audit/pass-2-codegen/agent-*.md
rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*=>' restart/audit/pass-2-codegen/PASS-2.md restart/audit/pass-2-codegen/agent-*.md
```

Result: grammar-name grep returned expected matches in LOC tables, fixture examples, SOTA rows, and verification commands. Match-arm grep returned zero matches.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:297-308`, `restart/audit/pass-2-codegen/agent-6-codegen-coherence-auditor.md:47-58` | grammar names in generated LOC budget | The target uses grammar names as per-X budget rows. | This is ratified: budgets must be per grammar per Lock 14 budget block (`restart/locks/LOCKS.md:118-125`). | Budget rows alone do not prove onboarding generality. | Keep the budget table; add onboarding proof elsewhere. | KEEP |
| `restart/audit/pass-2-codegen/agent-6-codegen-coherence-auditor.md:40` | grammar-name `rg` verification command | The command searches for forbidden public types in generic crates. | This is a legitimate audit anchor derived from Lock 14 (`restart/locks/LOCKS.md:60`). | It names only four legacy markers. | Add the full current marker set or a metadata-driven deny-list generated from workspace metadata. | KEEP |
| `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:63-68` | detector examples for JSON/CSS/BBNF/Sheets | Agent 5 gives concrete detector expectations. | Examples help explain recognizer behaviour and cite SOTA/current emitter evidence. | It covers only four grammars and can read as plan logic keyed by grammar family. | Recast as examples and add a full 9-grammar detector expectation table if the claim is "all 9". | REINVENT |
| `restart/audit/pass-2-codegen/PASS-2.md:44`, `restart/audit/pass-2-codegen/agent-4-runtime-template-architect.md:31` | all per-grammar runtime files generated | PASS-2 claims every per-grammar runtime subdir is template output. | This is the right Lock 14 direction. | No per-grammar emitted-file table shows all 9 existing grammars. | Broad "all" claims require per-X proof. Add a 9-row table plus `yaml.bbnf` smoke. | REINVENT |
| `restart/audit/pass-2-codegen/PASS-2.md:48`, `restart/audit/pass-2-codegen/PASS-2.md:226` | no declaration crates by default | PASS-2 rejects per-grammar declaration crates for existing grammars. | Honours README's two onboarding surfaces and rare escape valve (`restart/README.md:13-25`, `restart/README.md:145-157`). | The rare escape valve is named but not fenced with an approval/gate. | Add "no escape for the 9 extant grammars; new escape requires synthesis-level amendment and generated host table proof." | KEEP |
| target-wide | future grammar onboarding | The target should prove `yaml.bbnf` enters through source + metadata only. | This is the Lock 14 verification gate in README (`restart/README.md:13-25`, `restart/README.md:396`). | `rg -n yaml restart/audit/pass-2-codegen/*.md` finds no target text. | The challenge wins: PASS-2 is not Lock 14-ready without this test. | REINVENT |
| target-wide | forbidden match arms | The target must not propose `match grammar { Json => ... }` arms. | The required regex returned zero. | Zero docs matches do not prove future code, but the plan also adds a command gate. | The plan survives this check. | KEEP |

Lane verdict: **AMENDMENT-REQUIRED** — KEEP 4 / REINVENT 3 / DISCARD 0.

Surgery list:

| Surgery | Source |
|---|---|
| Add `yaml.bbnf` onboarding test: source file + `[workspace.metadata.bbnf.grammars.yaml]` only; generated BIR snapshot, runtime files, Rust check, no generic-code diff. | REINVENT |
| Add per-grammar runtime emission table for all 9 current grammars and `yaml` smoke. | REINVENT |
| Recast partial detector table as examples or expand to all current grammars. | REINVENT |

## §8 Lane 6 — Generated-Code + LOC Budget

Lane standard: generated output and codegen scaffolding need size budgets. PASS-2 must name generated baselines, per-grammar deltas, xtask wall budgets, and non-generated file-size constraints for new module trees.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:293-310` | per-grammar generated LOC | PASS-2 sets +2% ceilings for all 9 generated grammars. | Strong: cites baseline and gives per-grammar numbers, with CSS L4 hotspot exposed. | The +2% is an initial ceiling, not a steady-state reduction target. | A hardener could demand reduction targets now. PASS-2 is allowed an initial ceiling while template transition lands; later tranches can reduce. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:312-319` | xtask regen wall budgets | PASS-2 caps `regen --check`, single grammar regen, BIR snapshot, and write phase. | Good operational budget; content-equality mtime preservation is cited. | BIR snapshot ≤5s lacks an observed current baseline. | Add measurement baseline or mark as provisional. | REINVENT |
| `restart/audit/pass-2-codegen/agent-6-codegen-coherence-auditor.md:45-58` | agent budget duplicate | Agent 6 repeats the per-grammar budget. | Confirms synthesis did not invent budget late. | Duplication is not a problem in audit outputs. | Keep. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:137-258` | proposed crate/module trees | PASS-2 lists `codegen`, `runtime`, `host`, `simd-scan`, `xtask` trees. | Trees are cohesive and respond to Lock 13. | No non-generated LOC target per module or child-count proof is included. | The challenge wins partly: Lock 13 cannot be verified from tree shape alone. Add budgets. | REINVENT |
| `restart/audit/pass-2-codegen/PASS-2.md:310` | generated files exempt from file cap | PASS-2 states generated files are exempt from per-file cap but still budgeted. | Correct reading of Lock 13 plus generated output discipline. | None material. | Keep. | KEEP |

Lane verdict: **AMENDMENT-REQUIRED** — KEEP 3 / REINVENT 2 / DISCARD 0.

Surgery list:

| Surgery | Source |
|---|---|
| Add non-generated LOC/child-count table for each new module tree, with `<=500 LOC` file caps except generated/intrinsic kernel exemptions. | REINVENT |
| Add or mark provisional observed baselines for the BIR snapshot wall budget. | REINVENT |

## §9 Lane 7 — Friction Forecast

Lane standard: every user, grammar-author, and implementer friction surface must have a cookbook, migration page, or verbatim diagnostic/error message. PASS-2 owns compiler/codegen diagnostics even when PASS-3 owns public docs.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:24`, `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:61-68` | auto-decision logs | PASS-2 requires decision rows for selected/rejected Pratt/SIMD/PHF choices. | This is a real debugging surface, not hand-waving. | It lacks verbatim warnings/notes. | Keep logs; add messages. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:128-135`, `restart/audit/pass-2-codegen/agent-2-rust-lowerer-architect.md:19` | lookbehind diagnostics | PASS-2 rejects unbounded lookbehind upstream. | Correct semantic boundary. | No error string says what the grammar author sees. | Add: `error[bbnf::lookbehind::unbounded]: lookbehind for rule {rule} has no fixed width; rewrite the predicate to a bounded literal or regex.` | REINVENT |
| `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:13`, `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:80` | Pratt misfire | Agent 5 names misclassification risk. | It requires snapshots and canonical output. | No diagnostic states selected/rejected Pratt reason. | Add note/error strings for rejected and selected Pratt plans with rule, operator set, and fallback. | REINVENT |
| `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:15`, `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:81` | SIMD misfire | PASS-2 compares scalar and SIMD costs. | It records costs in `SimdPlan`. | No user-facing note explains why SIMD was not applied. | Add: `note[bbnf::simd::not-selected]: rule {rule} stayed scalar because setup cost {simd_cost} exceeds scalar cost {scalar_cost} for expected length {n}.` | REINVENT |
| `restart/audit/pass-2-codegen/PASS-2.md:67`, `restart/audit/pass-2-codegen/agent-4-runtime-template-architect.md:82` | layout lowering errors | PASS-2 consumes layout policy and emits BIR `Layout`. | The data path exists. | No verbatim layout error or cookbook handoff. | Add: `error[bbnf::layout::ambiguous]: rule {rule} can lower as {candidates}; add @layout(...) or refactor the grammar.` | REINVENT |
| `restart/audit/pass-2-codegen/PASS-2.md:69`, `restart/audit/pass-2-codegen/agent-3-wasm-lowerer-simd-architect.md:19` | host chains and WASM imports | PASS-2 handles `HostCall` chains and WASM host imports. | Correctly routes through generic primitives or external `@host fn`. | No diagnostic tells users when a host primitive cannot lower to wasm32. | Add host-chain and WASM import diagnostics; PASS-3 may document, but PASS-2 must emit. | REINVENT |
| `restart/audit/pass-2-codegen/PASS-2.md:260-273` | PASS-3 API friction handoff | PASS-2 hands parse signatures, views, visitor hooks, and cost table to PASS-3. | PASS-3 owns pointer/select/lifetime cookbook per prompt (`restart/prompts/PASS-3-RUNTIME.md:54-56`, `restart/prompts/PASS-3-RUNTIME.md:83`). | The handoff lacks acceptance gates. | Keep handoff; add consumer acceptance tests. | KEEP |

Lane verdict: **AMENDMENT-REQUIRED** — KEEP 2 / REINVENT 4 / DISCARD 0.

Surgery list:

| Surgery | Source |
|---|---|
| Add PASS-2 diagnostic ledger with verbatim messages for unbounded lookbehind, Pratt selected/rejected, SIMD not selected, layout ambiguity, host-chain/wasm-lowering failure, and lowerer-import boundary violation. | REINVENT |
| Add cookbook/migration handoff rows to PASS-3 for pointer/select, lifetime API, crate split, and adding a grammar; PASS-2 provides emitted metadata accepted by those docs. | REINVENT |

## §10 Lane 8 — Carry & Deferral Audit

Lane standard: every deferral/carry must name receiver, blocker, and receiving gate. PASS-2 may defer user API and package parity, but dangling "later" language is fault.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:17`, `restart/audit/pass-2-codegen/PASS-2.md:366-369` | TS production deferral | PASS-2 keeps TS scaffold only and routes production TS out of scope. | This honours PASS-2 prompt Q28 scope (`restart/prompts/PASS-2-CODEGEN.md:3`). | Receiver and receiving gate are split across prose and legacy rows; blocker is not explicit. | Add ledger row: receiver PASS-3/BD TS tranche; blocker public API/package surface; gate TS runtime activation + parity matrix. | REINVENT |
| `restart/audit/pass-2-codegen/PASS-2.md:274`, `restart/audit/pass-2-codegen/PASS-2.md:385` | BD.W5 parity | PASS-2 leaves 81-cell cross-backend parity downstream. | Receiver and gate are named with BD.W5 citation (`docs/tranches/BD/waves/W5.md:181-217`). | Blocker should be explicit: Rust/WASM lowerers plus PASS-3 public package surface. | Add blocker text; otherwise keep. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:291`, `restart/audit/pass-2-codegen/agent-6-codegen-coherence-auditor.md:62` | PASS-1 artefact reconciliation | PASS-2 assumes PASS-1 outputs because parallel dispatch made final PASS-1 unavailable. | Honest and necessary. | Receiver is SYNTHESIS; blocker is PASS-1 unavailable; receiving gate is not named as a concrete check. | Add receiving gate: SYNTHESIS must diff PASS-1 BIR variant/cost trait against PASS-2 BIR and amend before master plan. | REINVENT |
| `restart/audit/pass-2-codegen/PASS-2.md:260-273`, `restart/audit/pass-2-codegen/agent-4-runtime-template-architect.md:84` | PASS-3 user API | PASS-2 sends parse signatures, document/view names, visitor hooks, selectors, owned escapes to PASS-3. | Receiver and scope are clear. | Blocker is implicit: PASS-3 owns public API. | This is sufficient if consumer acceptance gates are added in Lane 3/7. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:371-372` | publication/package and fixtures | PASS-2 routes BD.W3 publication and BD.W4 fleet fixtures out of scope. | Correct nonownership. | Receiving gates are cited in legacy row but not summarized in a deferral ledger. | Add ledger to prevent carry-blindness. | KEEP |

Lane verdict: **AMENDMENT-REQUIRED** — KEEP 3 / REINVENT 2 / DISCARD 0.

Surgery list:

| Surgery | Source |
|---|---|
| Add carry ledger table with columns `Item / Receiver / Blocker / Receiving gate / PASS-2 artefact supplied` for TS production, BD.W5 parity, PASS-1 reconciliation, PASS-3 API docs, publication, and fixtures. | REINVENT |

## §11 Lane 9 — Greenfield Discipline

Lane standard: no quick solutions, no workarounds, no uncontested legacy code, no contrivance, no overfit; idiomatic Rust and SOTA-like cohesion are the standard.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:5-7` | replacement over patch | PASS-2 says current codegen/runtime wiring must be replaced, not patched. | Directly matches greenfield mandate (`restart/README.md:5`) and source evidence. | Replacement scope is large. | Large scope is warranted because current driver walks Grammar IR and runtime has manual grammar modules. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:36`, `restart/audit/pass-2-codegen/PASS-2.md:379` | OpenFrame deletion | PASS-2 replaces cloned checkpoints with TapeBuilder length checkpoints. | Root-cause fix for the 86.07% pathology, not a workaround. | Tape checkpoint side tables must be complete. | Agent 2 names checkpoint side-table risk (`restart/audit/pass-2-codegen/agent-2-rust-lowerer-architect.md:91`); keep with tests. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:40`, `restart/audit/pass-2-codegen/agent-3-wasm-lowerer-simd-architect.md:15-16` | raw WAT demotion | PASS-2 makes wasm32 Rust source the production path; raw WAT remains smoke. | Avoids duplicated parser logic and matches BD.W2 inheritance. | Requires binding ABI discipline. | The ABI module and smoke fixture are sufficient for PASS-2. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:78`, `restart/audit/pass-2-codegen/agent-1-backend-ir-architect.md:57-66` | 23-variant BIR cardinality | PASS-2 defends the variant count against compiler IR comparators. | Concrete and bounded; avoids both 7-variant placeholder and variant sprawl. | Comparators are local research anchors and rustdoc links, not all local. | The defense is adequate; the ownership move does not alter cardinality. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:139-181` | `codegen/src/backend_ir` tree | PASS-2 puts the BIR contract in codegen. | Locality with lowerers is tempting. | Violates the workspace owner's split: `ir` owns Backend IR types. | Greenfield discipline favours clean ownership over local convenience. Move it. | REINVENT |
| `restart/audit/pass-2-codegen/PASS-2.md:226`, `restart/audit/pass-2-codegen/PASS-2.md:389-391` | no per-grammar crates, no stale extensions | PASS-2 rejects declaration crates by default, ParseStream, rewrite mode, grammar-level Unicode sets. | Strong adherence to settled authority. | Escape valve needs fencing. | Keep with rare-escape text. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:118-126`, `restart/audit/pass-2-codegen/PASS-2.md:241` | `simd-scan` genericity | PASS-2 keeps SIMD scanner data-driven and grammar-free. | MODULES marks `simd-scan` clean and KEEP-AS-IS (`restart/corpora/MODULES.md:47-69`). | None material. | Keep. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:44`, `restart/audit/pass-2-codegen/agent-4-runtime-template-architect.md:78` | generated runtime subdirs | PASS-2 moves hand-maintained runtime modules to template-emitted subdirs. | Correct direction against CENSUS runtime inventory (`restart/corpora/CENSUS.md:435-527`). | Without `yaml` proof and per-X table, it remains asserted. | Add proof. | REINVENT |

Lane verdict: **AMENDMENT-REQUIRED** — KEEP 6 / REINVENT 2 / DISCARD 0.

Surgery list:

| Surgery | Source |
|---|---|
| Move BIR type tree to `ir`; keep codegen lowerer-local. | REINVENT |
| Add concrete genericity proofs for runtime template claims. | REINVENT |

## §12 Punch list

| # | Target site | Verbatim edit or surgery | Source verdict | Owner | Scope | Lanes |
|---:|---|---|---|---|---|---|
| 1 | `restart/audit/pass-2-codegen/PASS-2.md:139-181`; `restart/audit/pass-2-codegen/agent-1-backend-ir-architect.md:25` | Replace `codegen/src/backend_ir/` with `ir/src/backend_ir/` in the proposed tree. Add: "The `ir` crate owns Backend IR types; `codegen` owns lowerers and adapters only." | DISCARD | PASS-2 amendment agent | PASS-2 synthesis + agent-1 correction note | 1, 3, 9 |
| 2 | `restart/audit/pass-2-codegen/PASS-2.md:32`; `restart/audit/pass-2-codegen/agent-6-codegen-coherence-auditor.md:39-43` | Add an import-deny gate: `rg -n "GrammarIR" crates/codegen/src/lower crates/codegen/src/runtime_template` must return zero; only `passes`/BIR producer code may import Grammar IR. | REINVENT | PASS-2 amendment agent | PASS-2 §2 / §9 | 1, 3 |
| 3 | `restart/audit/pass-2-codegen/PASS-2.md:44`; `restart/audit/pass-2-codegen/agent-4-runtime-template-architect.md:23` | Add `yaml.bbnf` future-grammar onboarding test: source file plus `[workspace.metadata.bbnf.grammars.yaml]` only; generated BIR snapshot, generated runtime files, Rust check, no generic-crate diff, no per-grammar match arms. | REINVENT | PASS-2 amendment agent | PASS-2 §2 / §9 | 1, 5, 9 |
| 4 | `restart/audit/pass-2-codegen/PASS-2.md:44`; `restart/audit/pass-2-codegen/PASS-2.md:295-308` | Add per-grammar runtime emission table for bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, math, plus yaml smoke; columns: `generated.rs`, `parser.rs`, `host.rs`, host source, layout source, error source, Pratt/SIMD source. | REINVENT | PASS-2 amendment agent | PASS-2 §2 or §6 | 5, 9 |
| 5 | `restart/audit/pass-2-codegen/PASS-2.md:321-335`; `restart/audit/pass-2-codegen/agent-3-wasm-lowerer-simd-architect.md:20` | Rebuild the perf trajectory table with columns `Competitor / Dataset / Platform / bbnf target / PASS-2 mechanism`; demote non-throughput rows to mechanism gates. | REINVENT | PASS-2 amendment agent | PASS-2 §7 | 4 |
| 6 | `restart/audit/pass-2-codegen/PASS-2.md:137-258`; `restart/audit/pass-2-codegen/PASS-2.md:312-319` | Add non-generated LOC and child-count budget table for `codegen`, `runtime`, `host`, `xtask`; add observed/provisional baseline for BIR snapshot ≤5s. | REINVENT | PASS-2 amendment agent | PASS-2 §3 / §6 | 1, 6 |
| 7 | `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:13-17`; `restart/audit/pass-2-codegen/agent-4-runtime-template-architect.md:82` | Add PASS-2 diagnostic ledger with verbatim strings for unbounded lookbehind, Pratt selected/rejected, SIMD not selected, layout ambiguity, host-chain wasm failure, and lowerer GrammarIR import violation. | REINVENT | PASS-2 amendment agent | PASS-2 §2 / §9 | 7 |
| 8 | `restart/audit/pass-2-codegen/PASS-2.md:17`; `restart/audit/pass-2-codegen/PASS-2.md:274`; `restart/audit/pass-2-codegen/PASS-2.md:291`; `restart/audit/pass-2-codegen/PASS-2.md:385` | Add carry ledger table: `Item / Receiver / Blocker / Receiving gate / PASS-2 artefact supplied` for TS production, BD.W5 parity, PASS-1 reconciliation, PASS-3 API docs, publication, and fixtures. | REINVENT | PASS-2 amendment agent | PASS-2 §4 / §8 / §9 | 8 |
| 9 | `restart/audit/pass-2-codegen/PASS-2.md:260-273`; `restart/audit/pass-2-codegen/agent-2-rust-lowerer-architect.md:84` | Add PASS-3 consumer acceptance gates: emitted parse signatures compile under PASS-3 API wrapper, document/view metadata feeds visitor/selectors, materialisation cost table is generated and documented. | REINVENT | PASS-2 amendment agent | PASS-2 §4 | 3, 7, 8 |

Punch-list size: **9**.

## §13 Final readiness

> **Decision: amendment-required**
>
> PASS-2 has the right middle-layer architecture: Backend IR as codegen contract, Tape-backed runtime template, BIR-only Rust/WASM lowerers, generic `simd-scan`, generated LOC budgets, and xtask equality. It is not ready to advance because one ownership fault and several proof gaps would compound in synthesis. The required amendments are surgical: re-home Backend IR ownership, add Lock 14 onboarding/per-X proof, make SOTA rows row-complete, add PASS-2-owned diagnostics, and close deferral ledgers. No re-draft is warranted.
>
> Hereupon dispatch a narrow PASS-2 amendment agent against the nine-item punch list before synthesis treats PASS-2 as settled.
