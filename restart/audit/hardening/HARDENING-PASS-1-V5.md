# HARDENING-PASS-1-V5

## §1 Target Identification

- Agent: Phase 0 V5 metahardening worker for target `PASS-1`.
- Target surface: `restart/audit/pass-1-substrate/PASS-1.md`.
- Output surface: `restart/audit/hardening/HARDENING-PASS-1-V5.md`.
- Workdir: `/Users/mkbabb/Programming/bbnf-lang`.
- Date: 2026-05-05.
- Scope rule: this report audits and routes amendments; it does not patch target surfaces.
- Touch rule followed while drafting: only this output file is authored.
- Primary verdict: `AMENDMENT-REQUIRED`.
- Reason for non-READY verdict: PASS-1 is internally strong, but the current synthesis corpus still carries a binding conflict in `restart/ARCHITECTURE.md:1049-1081`.
- Highest-risk conflict: Architecture §8.1 sketches bodyless `@host fn`, prefix lookbehind, and `=>` mapping while PASS-1 §6 commits block-bodied `HostFn`, infix `Expr |< Expr`, and rule-level `Expr -> f1 -> f2`.
- Carry-aware framing: V1-V4 closed most PASS-1 issues; V5 is stricter about cross-document surfaces that downstream workers will read as canonical.
- Mandatory reading consumed before writing: README, Locks, V1-V4 consolidated hardening, research index, hardening prompts, amendment dispatch, style/process precepts, Architecture, Migration, Master Plan, PASS-1, PASS-2, PASS-3, and PASS-1 hardening V1-V3.
- Audit hygiene: claims below are bound to observed file lines or to rerun local `rg` checks.
- Lane note: Lane 2 is `N/A` for PASS targets per `restart/prompts/audit-specs/HARDENING-LENS-SET.md:56-58` and `restart/prompts/audit-specs/HARDENING-LENS-SET.md:171-175`.

## §2 Carry-Aware Lens Table A-E

| Row | Lens | Site | Verified finding | Carry status | Required surgery |
|---|---|---|---|---|---|
| A1 | Narrative coherence | `restart/audit/pass-1-substrate/PASS-1.md:176-217` vs `restart/ARCHITECTURE.md:1049-1081` | PASS-1 owns the most precise BBNF grammar surface; Architecture §8.1 currently contradicts it on host declarations, lookbehind shape, and map/chain syntax. | New V5 blocker after V4 READY. | Rewrite Architecture §8.1 to cite or embed PASS-1 §6 grammar productions verbatim. |
| A2 | Narrative coherence | `restart/MASTER-PLAN.md:88-104` and `restart/MASTER-PLAN.md:748` | Master treats the Architecture BBNF contract as synthesis authority, but the PASS-1 reconciliation gate checks only Architecture §7 schema against PASS-1 §2 enum. | Carry gate is too narrow. | Add an Architecture §8.1 vs PASS-1 §6 equivalence check to the PASS-1 reconciliation row. |
| A3 | Narrative coherence | `restart/audit/pass-1-substrate/PASS-1.md:221-227` and `restart/ARCHITECTURE.md:1275-1331` | Yaml onboarding is coherent as a two-surface invariant, but no single narrative walks from source grammar to metadata to generated output to path schema. | V4 accepted tables; V5 wants author-facing proof. | Add one yaml onboarding walkthrough without adding new onboarding surfaces. |
| A4 | Narrative coherence | `restart/audit/pass-1-substrate/PASS-1.md:237-247`, `restart/MASTER-PLAN.md:615-652` | Generated LOC budget fields and seed ceilings now agree in shape, but Master cites stale Architecture line ranges for §12.1 baseline rows. | Provenance gap, not concept gap. | Correct Master citation from `ARCHITECTURE.md:1273-1281` to the actual baseline table lines. |
| A5 | Narrative coherence | `restart/audit/pass-1-substrate/PASS-1.md:249-259`, `restart/MIGRATION.md:606-633`, `restart/MASTER-PLAN.md:562-580` | PASS-1 inheritance ledger, migration sequence, and master commit chain all route old substrate work into restart tranches. | Closed from V4; no new surgery. | Keep as evidence for READY once A1/A2 close. |
| B1 | Vocabulary drift | `restart/audit/pass-1-substrate/PASS-1.md:111-126`, `restart/ARCHITECTURE.md:393-450`, `restart/audit/pass-2-codegen/PASS-2.md:69` | PASS-1 crate tree uses `passes` children including `types`, `layout`, and `facts`; PASS-2/Architecture carry public `LayoutFacts`, `LayoutSink`, and `passes::layout`, with `TypeFacts` internal. | Prior V3 noted acceptable target-local absence; V5 keeps a watch item. | Add a short PASS-1 handoff sentence that the public layout vocabulary is defined downstream, or keep the Architecture/PASS-2 carry gate explicit. |
| B2 | Vocabulary drift | `restart/audit/pass-1-substrate/PASS-1.md:39`, `restart/audit/pass-2-codegen/PASS-2.md:52-79`, `restart/ARCHITECTURE.md:875-901` | PASS-1 states Backend IR has 22 variants without `Return`; PASS-2/Architecture carry 23-node language, with Architecture noting the equivalence requirement if `Return` is omitted. | Acceptable only while equivalence proof remains visible. | Keep the equivalence proof as a hard gate in PASS-2 or synthesis close. |
| B3 | Vocabulary drift | `restart/audit/pass-1-substrate/PASS-1.md:57`, `restart/locks/LOCKS.md:34`, `restart/audit/pass-2-codegen/PASS-2.md:455` | `OpenFrame` is consistently retired in favor of tape/direct runtime and `TapeBuilder` checkpoint language. | Closed. | No amendment. |
| B4 | Vocabulary drift | `restart/audit/pass-1-substrate/PASS-1.md:196`, `restart/ARCHITECTURE.md:1065` | PASS-1 uses infix lookbehind `Expr "|<" Expr`; Architecture sketch uses prefix `|< Suffix`. | Amendment blocker. | Replace Architecture production or mark it non-normative and bind to PASS-1 §6. |
| B5 | Vocabulary drift | `restart/audit/pass-1-substrate/PASS-1.md:183`, `restart/audit/pass-1-substrate/PASS-1.md:211`, `restart/ARCHITECTURE.md:1081` | PASS-1 rejects bodyless host declarations; Architecture sketch accepts `@host fn ... ;`. | Amendment blocker. | Replace `HostDecl` with PASS-1 `HostFn ... Block` production. |
| B6 | Vocabulary drift | `restart/audit/pass-1-substrate/PASS-1.md:205-217`, `restart/ARCHITECTURE.md:1077-1079` | PASS-1 rule-level chains use `->`; Architecture uses method-chain grammar plus `=>` map expression. | Amendment blocker. | Move method-chain grammar under host-body expression only and remove rule-level `=>`. |
| B7 | Vocabulary drift | `restart/audit/pass-2-codegen/PASS-2.md:540-541`, `restart/MASTER-PLAN.md:204` | Master rejects `@pratt` and `@simd` directives, but PASS-2 diagnostics tell users to promote with `@pratt` or force with `@simd`. | New V5 blocker adjacent to PASS-1 Lock 10. | Rewrite diagnostic strings around automatic detection, cost-model evidence, and grammar restructuring only. |
| B8 | Vocabulary drift | `restart/audit/pass-3-runtime/PASS-3.md:197-208`, `restart/ARCHITECTURE.md:346-356` | `bbnf/src/` 8-child layout is coherent across PASS-3 and Architecture. | Closed. | No amendment. |
| C1 | Worked-example scarcity | `restart/audit/pass-1-substrate/PASS-1.md:221-227` | The yaml proof table verifies the two-surface invariant but does not show a concrete source-to-generated flow. | V4 allowed; V5 asks for one author-facing example. | Add one compact yaml onboarding walkthrough with exact input paths and generated-only outputs. |
| C2 | Worked-example scarcity | `restart/audit/pass-3-runtime/PASS-3.md:82-115`, `restart/ARCHITECTURE.md:268-284` | `pointer!` and `select!` are named and gated, but there is no query example over a generated yaml or json document that touches both macros and visitor/path schema metadata. | Coverage gap. | Add a small query example in cookbook/plan receiver, not a new PASS-1 requirement. |
| C3 | Worked-example scarcity | `restart/audit/pass-3-runtime/PASS-3.md:158-190` | Incremental parse policy has data thresholds, but no step-by-step edit example from dirty range to fallback/no-fallback outcome. | Coverage gap. | Add one fault-tolerant incremental parse walkthrough tied to `DocumentSnapshot` and `ReparsePlan`. |
| C4 | Worked-example scarcity | `restart/audit/pass-1-substrate/PASS-1.md:51`, `restart/audit/pass-3-runtime/PASS-3.md:160` | `@error(recover = ...)` is routed, but no example shows how generic rule typing behaves when recovery shells feed downstream host/layout code. | Coverage gap. | Add one typed `@error(recover = ...)` example that includes diagnostic and resulting typed placeholder/value surface. |
| C5 | Worked-example scarcity | `restart/MASTER-PLAN.md:208-537` | The tranches are detailed, but there is no one-grammar A->F->J trajectory that lets a reader see a grammar become metadata, runtime, parity, and publish evidence. | Coverage gap. | Add one A->F->J grammar trajectory, preferably yaml or json, without changing gates. |
| D1 | Coverage gap | `restart/audit/pass-1-substrate/PASS-1.md:221-227`, `restart/MASTER-PLAN.md:764-770` | Unfamiliar grammar onboarding ergonomics are asserted by two-surface and cookbook rows, but no "first failed registration" diagnostic path is shown. | User-facing gap. | Add an onboarding failure example for missing workspace metadata and forbidden manual Rust registry edit. |
| D2 | Coverage gap | `restart/audit/pass-3-runtime/PASS-3.md:181-190` | Fault-tolerant incremental parsing has fallback thresholds and LSP silence policy, but no debug output shape for failed anchors beyond prose. | User-facing gap. | Add a bench/debug sample row for anchor miss, fallback reason, and hidden LSP behavior. |
| D3 | Coverage gap | `restart/audit/pass-3-runtime/PASS-3.md:119-156` | Debug/runtime hooks are specified as tape identity and DAP reuse, but line 156 uses advisory "should" rather than a close gate. | Soft wording risk. | Promote DAP/debug identity reuse to a must-gate or route it to a named tranche gate. |
| D4 | Coverage gap | `restart/audit/pass-1-substrate/PASS-1.md:71`, `restart/audit/pass-1-substrate/PASS-1.md:217` | Generic host/rule typing under chain flow is described, but not stress-tested under recovery values and host overload selection. | Test-design gap. | Add a gate for generic chain typing when a prior step is recovered or substituted. |
| D5 | Coverage gap | `restart/audit/pass-1-substrate/PASS-1.md:67`, `restart/MASTER-PLAN.md:374-405` | WASM host primitives are routed, but the primitive-set stability boundary is thinner than Rust V1's host dispatch language. | Coverage gap. | Add a WASM host primitive manifest smoke gate in H or PASS-2 handoff. |
| D6 | Coverage gap | `restart/audit/pass-3-runtime/PASS-3.md:179-190`, `restart/MASTER-PLAN.md:482-500` | LSP fallback policy is coherent; default silence and debug-only reporting are clear. | Closed. | No amendment. |
| D7 | Coverage gap | `restart/locks/LOCKS.md:54`, `restart/MASTER-PLAN.md:524` | Lock 11 incubation is stable: sister crates stay path-deps until stability gate. | Closed. | No amendment. |
| E1 | Lock tension | `restart/locks/LOCKS.md:34`, `restart/audit/pass-1-substrate/PASS-1.md:57` | Lock 1 tape/direct union and PASS-1 builder-frame substrate agree; `OpenFrame` is deletion archaeology only. | Resolved tension. | No amendment. |
| E2 | Lock tension | `restart/locks/LOCKS.md:40`, `restart/audit/pass-1-substrate/PASS-1.md:73-75` | Lock 4 keeps egraph and CSP separate; PASS-1 keeps `AnalysisCost` extraction separate from HM/CSP choices. | Resolved tension. | No amendment. |
| E3 | Lock tension | `restart/locks/LOCKS.md:52`, `restart/audit/pass-2-codegen/PASS-2.md:540-541` | Lock 10 says Pratt/SIMD are automatic, but PASS-2 diagnostic strings reintroduce directive vocabulary. | Amendment blocker. | Remove `@pratt`/`@simd` from diagnostics and any user-facing remediation. |
| E4 | Lock tension | `restart/locks/LOCKS.md:58`, `restart/audit/pass-1-substrate/PASS-1.md:221-227` | Lock 14 two-surface grammar generalization is carried by PASS-1 yaml proof. | Resolved tension. | Strengthen with worked example only. |
| E5 | Lock tension | `restart/locks/LOCKS.md:46`, `restart/audit/pass-3-runtime/PASS-3.md:82-115` | Lock 7 path crates and PASS-3 path API agree on `pointer!`, `select!`, `path`, `path-core`, `path-ts`. | Resolved tension. | No amendment. |
| E6 | Lock tension | `restart/locks/LOCKS.md:36`, `restart/ARCHITECTURE.md:971-990` | Layout side-table public/private split is coherent in Architecture, but PASS-1 should not be read as defining public `TypeFacts`. | Watch item only. | Keep `LayoutFacts` public, `TypeFacts` internal in downstream specs. |

## §3 LLM-Pathology Table F-H

| Row | Lens | Site | Verified pathology | Risk | Required surgery |
|---|---|---|---|---|---|
| F1 | LLM bias | `restart/ARCHITECTURE.md:1049-1081` | Architecture §8.1 looks like a generic grammar sketch imported from common parser conventions rather than PASS-1's settled grammar. | Downstream workers may implement the wrong syntax because Architecture is high-authority. | Replace sketch with PASS-1-bound productions or make it explicitly illustrative and non-normative. |
| F2 | LLM bias | `restart/audit/pass-3-runtime/PASS-3.md:156` | "Debug and DAP should reuse this identity" is softer than the rest of the tape identity contract. | Advisory wording weakens an otherwise hard user-surface invariant. | Change receiver gate to "must reuse" or name the tranche gate that enforces it. |
| F3 | LLM bias | `restart/audit/pass-2-codegen/PASS-2.md:540-541` | Diagnostics offer familiar directive escape hatches (`@pratt`, `@simd`) even though the restart locks retired those directives. | Model-like completion from prior language designs leaks into committed strings. | Rewrite remediation strings to automatic detection and cost evidence. |
| F4 | LLM bias | `restart/README.md:473` | README closing line still compresses BBNF extensions as "rewrite-mode + lookbehind + Unicode sets" even though PASS-1 and Architecture route rewrite/Unicode differently. | A worker reading the close before PASS-1 can resurrect retired scope. | Replace with settled extension vocabulary or point to Locks. |
| H1 | Hallucination/provenance | `restart/MASTER-PLAN.md:636` | Master says Architecture §12.1 baseline rows are at `ARCHITECTURE.md:1273-1281`, but the per-grammar table is at `ARCHITECTURE.md:1322-1331`. | Wrong citations make future audit workers chase the wrong evidence. | Correct the line citation or cite section name only. |
| H2 | Hallucination/provenance | `restart/audit/pass-2-codegen/PASS-2.md:174`, `restart/audit/pass-2-codegen/PASS-2.md:539` | PASS-2 references `BBNF-SEM040` at line 478, but the diagnostic table row is now line 539. | A carry ledger can look closed while the evidence pointer is stale. | Update the inline line reference. |
| H3 | Hallucination/provenance | `restart/audit/pass-1-substrate/PASS-1.md:81-91`, `restart/ARCHITECTURE.md:723-760` | PASS-1 rare-escape fence lists six review fields; Architecture later defines an eight-field declaration-crate form. | Direct PASS-1 readers may miss deletion-path/reviewer fields. | Add a pointer from PASS-1 fence to Architecture's canonical eight-field review form, or expand PASS-1's field list. |
| H4 | Hallucination/provenance | `restart/README.md:135` | README uses "Latest Unicode standard version" with a concrete version/date assertion. | "Latest" assertions rot and need a research source or fixed-version wording. | Replace "latest" with an explicit Unicode version policy and source route. |
| G1 | Overfitting | `restart/ARCHITECTURE.md:1077-1079` | Method-chain grammar and `=>` map syntax fit common language patterns but overfit away from PASS-1's `->` rule-chain contract. | Implementers could support a non-existent grammar-rule method-chain surface. | Delete `=>` from the grammar-rule surface and scope method chains to `@host fn` bodies. |
| G2 | Overfitting | `restart/audit/pass-1-substrate/PASS-1.md:221-227` | Yaml proof overfits to the desired "two surfaces only" invariant without showing user mistakes or generated side effects. | Onboarding can pass as a table but fail for unfamiliar authors. | Add a negative and positive onboarding example. |
| G3 | Overfitting | `restart/audit/pass-3-runtime/PASS-3.md:181-188` | Incremental thresholds overfit to named corpora but do not show a concrete dirty-range walk. | A worker can implement benchmark counters without the intended repair model. | Add a small edit trace showing anchor reuse and fallback. |
| G4 | Overfitting | `restart/MASTER-PLAN.md:634-652` | Seed LOC baseline table gives precise ceilings but relies on a stale Architecture pointer. | Precision appears stronger than its provenance. | Correct citation and require A.W2 to refresh recorded numerics. |

## §4 Compressed 9-Lane Verification

| Row | Lane | Site | Verification | Result |
|---|---|---|---|---|
| 1 | Lane 1 - Authority | `restart/README.md:11-25` | Two onboarding surfaces and no per-grammar match arms are still the restart premise. | PASS for PASS-1; worked example still needed. |
| 2 | Lane 1 - Authority | `restart/locks/LOCKS.md:34-60` | Locks cover tape/direct, layout, BIR, path, SOTA, Pratt/SIMD auto, Lock 11, Lock 14. | PASS with one downstream diagnostic conflict. |
| 3 | Lane 1 - Authority | `restart/audit/pass-1-substrate/PASS-1.md:5-20` | PASS-1 declares conflict closure and key reconciliations. | PASS internally. |
| 4 | Lane 1 - Authority | `restart/ARCHITECTURE.md:1049-1081` | Architecture grammar sketch conflicts with PASS-1 grammar. | FAIL; amendment required. |
| 5 | Lane 2 - Multi-wave | `restart/prompts/audit-specs/HARDENING-LENS-SET.md:56-58` | Multi-wave dispute lane is not applicable to PASS targets. | N/A. |
| 6 | Lane 3 - Work preservation | `restart/audit/pass-1-substrate/PASS-1.md:249-259` | PASS-1 routes old research into keep/reinvent/discard without editing old crates. | PASS. |
| 7 | Lane 3 - Work preservation | `restart/MIGRATION.md:111-165` | Migration keeps mixed-fate crosswalk and receiver gates. | PASS. |
| 8 | Lane 4 - Handoff | `restart/audit/pass-1-substrate/PASS-1.md:154-174` | PASS-1 sends BIR, lowering, tape/direct, and host/layout/error obligations to PASS-2/PASS-3. | PASS. |
| 9 | Lane 4 - Handoff | `restart/audit/pass-2-codegen/PASS-2.md:112-151` | Lowerer API and template schema bind codegen consumers. | PASS with diagnostic-string amendment. |
| 10 | Lane 5 - Architecture | `restart/audit/pass-1-substrate/PASS-1.md:111-143` | Compiler crate tree and rationale are complete enough for PASS-1. | PASS. |
| 11 | Lane 5 - Architecture | `restart/ARCHITECTURE.md:393-450` | Compiler tree uses downstream `passes/src/layout/` and `shapes/` organization. | PASS; vocabulary watch only. |
| 12 | Lane 6 - Grammar semantics | `restart/audit/pass-1-substrate/PASS-1.md:176-217` | PASS-1 formal grammar is the precise BBNF surface. | PASS. |
| 13 | Lane 6 - Grammar semantics | `restart/ARCHITECTURE.md:1065-1081` | Architecture sketch diverges on lookbehind, map/chain, and host fn body. | FAIL. |
| 14 | Lane 7 - Diagnostics | `restart/audit/pass-1-substrate/PASS-1.md:92-103` | PASS-1 binds BBNF1004 / BBNF-LOOKBEHIND-WIDTH / LookbehindWidth. | PASS. |
| 15 | Lane 7 - Diagnostics | `restart/audit/pass-2-codegen/PASS-2.md:539-541` | PASS-2 routes SEM040 but OPT diagnostics reintroduce `@pratt`/`@simd`. | FAIL narrow. |
| 16 | Lane 8 - Generated budgets | `restart/audit/pass-1-substrate/PASS-1.md:237-247` | Budget schema covers `generated_loc`, `regen_wall_ms`, evidence, and pressure source. | PASS. |
| 17 | Lane 8 - Generated budgets | `restart/MASTER-PLAN.md:615-652` | Master provides total and per-grammar budget gates. | PASS with stale line citation. |
| 18 | Lane 9 - Closing posture | `restart/audit/pass-1-substrate/PASS-1.md:276-282` | PASS-1 explicitly retires independent-proceed and OpenFrame residues. | PASS. |
| 19 | Lane 9 - Closing posture | `restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md:104-112` | V4 cohort verdict was READY after known amendments. | Carry acknowledged; V5 adds new stricter cross-doc check. |
| 20 | Lane 9 - Closing posture | `restart/MASTER-PLAN.md:730-770` | Master carry/friction ledger routes cross-target conflicts. | PASS except PASS-1 reconciliation gate needs §8.1 coverage. |

Lane compression result:

- PASS-1 itself remains a high-quality substrate report.
- The amendment is not a request to rewrite PASS-1 broadly.
- The blocker is that downstream canonical documents contradict PASS-1 on BBNF syntax and one adjacent Lock 10 diagnostic surface.

## §5 16-Command Gate-Rerun

Rerun date: 2026-05-05.

Rerun mode: local `rg` evidence checks against current restart docs and PASS files.

Gate 1:

```text
rg -n "ParseStream|rewrite-mode|Unicode class algebra" restart/ARCHITECTURE.md restart/MIGRATION.md restart/MASTER-PLAN.md restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-2-codegen/PASS-2.md restart/audit/pass-3-runtime/PASS-3.md | wc -l
```

- Result: 60 matches.
- Interpretation: expected legacy-vocabulary discussion remains, but PASS-1 excludes rewrite-mode and grammar-level Unicode algebra at `PASS-1.md:178` and `PASS-1.md:213`.
- Gate status: PASS with README close-line watch.

Gate 2:

```text
rg -n "bbnf-path|bbnf-test-fixtures|path!" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-3-runtime/PASS-3.md | wc -l
```

- Result: 7 matches.
- Interpretation: `path!` survives only as legacy citation in PASS-3; canonical macros are `pointer!` and `select!`.
- Gate status: PASS.

Gate 3:

```text
rg -n "codegen/src/backend_ir" restart/ARCHITECTURE.md restart/audit/pass-2-codegen/PASS-2.md | wc -l
```

- Result: 3 matches.
- Interpretation: Backend IR isolation is named and routed.
- Gate status: PASS.

Gate 4:

```text
rg -n "fixtures/yaml" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-2-codegen/PASS-2.md restart/audit/pass-3-runtime/PASS-3.md | wc -l
```

- Result: 7 matches.
- Interpretation: fixtures are parity/post-onboarding, not an onboarding input surface.
- Gate status: PASS.

Gate 5:

```text
rg -n "@recover" restart/ARCHITECTURE.md restart/audit/pass-3-runtime/PASS-3.md | wc -l
```

- Result: 3 matches.
- Interpretation: standalone `@recover` is documented only as legacy/migration alias; `@error(recover = ...)` is canonical.
- Gate status: PASS.

Gate 6:

```text
rg -n "OpenFrame" restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-2-codegen/PASS-2.md restart/MASTER-PLAN.md | wc -l
```

- Result: 16 matches.
- Interpretation: matches are retirement/deletion discussion; PASS-1 replaces OpenFrame with builder frames and TapeBuilder checkpoints.
- Gate status: PASS.

Gate 7:

```text
rg -n "GrammarIR" restart/audit/pass-2-codegen/PASS-2.md restart/ARCHITECTURE.md | wc -l
```

- Result: 3 matches.
- Interpretation: codegen boundary rejects Grammar IR as lowerer input.
- Gate status: PASS.

Gate 8:

```text
rg -n "__EAGER_EMPTY_PATH|CursorDecision::Skip" restart/MASTER-PLAN.md restart/MIGRATION.md | wc -l
```

- Result: 2 matches.
- Interpretation: old eager-empty path decisions route through cursor skip semantics.
- Gate status: PASS.

Gate 9:

```text
rg -n "twitter|canada|citm|bootstrap|animate|On-Demand" restart/MASTER-PLAN.md restart/audit/pass-3-runtime/PASS-3.md | wc -l
```

- Result: 21 matches.
- Interpretation: corpus and benchmark names are present in incremental/SOTA gates.
- Gate status: PASS.

Gate 10:

```text
rg -n "receiver|blocker|receiving gate" restart/MIGRATION.md restart/MASTER-PLAN.md restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-2-codegen/PASS-2.md restart/audit/pass-3-runtime/PASS-3.md | wc -l
```

- Result: 15 matches.
- Interpretation: carry rows generally name receiver/blocker/gate.
- Gate status: PASS.

Gate 11:

```text
rg -n "yaml.bbnf|workspace.metadata.bbnf.grammars.yaml" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-2-codegen/PASS-2.md restart/audit/pass-3-runtime/PASS-3.md | wc -l
```

- Result: 13 matches.
- Interpretation: yaml two-surface invariant is present.
- Gate status: PASS with worked-example request.

Gate 12:

```text
rg -n "generated_loc|regen_wall|xtask" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-2-codegen/PASS-2.md restart/audit/pass-3-runtime/PASS-3.md | wc -l
```

- Result: 60 matches.
- Interpretation: generated budget and regen gates are present.
- Gate status: PASS with stale citation fix.

Gate 13:

```text
rg -n "BBNF-LIFE|BBNF-LAYOUT|BBNF-OPT|BBNF-GRAMMAR|BBNF-POINTER|lookbehind|HostSignature" restart/ARCHITECTURE.md restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-2-codegen/PASS-2.md restart/audit/pass-3-runtime/PASS-3.md | wc -l
```

- Result: 56 matches.
- Interpretation: diagnostic vocabulary exists, but BBNF-OPT strings need Lock 10 cleanup.
- Gate status: AMEND.

Gate 14:

```text
rg -n "child count|500 LOC|exception rationale" restart/ARCHITECTURE.md restart/MASTER-PLAN.md | wc -l
```

- Result: 10 matches.
- Interpretation: child-count and 500 LOC governance are present.
- Gate status: PASS.

Gate 15:

```text
rg -n "declaration-crate review|why metadata|deletion path|reviewer" restart/ARCHITECTURE.md restart/MIGRATION.md | wc -l
```

- Result: 7 matches.
- Interpretation: Architecture carries the stronger declaration-crate review form.
- Gate status: PASS, with PASS-1 fence cross-reference requested.

Gate 16:

```text
rg -n "CPU model|compiler flags|input hash|competitor version|warmup|sample" restart/MASTER-PLAN.md restart/MIGRATION.md | wc -l
```

- Result: 4 matches.
- Interpretation: benchmark provenance terms exist, but sample/provenance density remains thin.
- Gate status: PASS for Phase 0; J tranche must refresh evidence.

V5 focused check 17:

```text
rg -n "HostDecl|Lookbehind    ::=|MapExpr|HostFn|MapTail|ChainExpr" restart/ARCHITECTURE.md restart/audit/pass-1-substrate/PASS-1.md
```

- Result: direct conflict found at `ARCHITECTURE.md:1065`, `ARCHITECTURE.md:1079`, `ARCHITECTURE.md:1081` against `PASS-1.md:183`, `PASS-1.md:196`, `PASS-1.md:205-217`.
- Gate status: AMENDMENT-REQUIRED.

V5 focused check 18:

```text
rg -n "@pratt|@simd" restart/ARCHITECTURE.md restart/MIGRATION.md restart/MASTER-PLAN.md restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-2-codegen/PASS-2.md restart/audit/pass-3-runtime/PASS-3.md
```

- Result: `PASS-2.md:540-541` user-facing diagnostic strings conflict with `MASTER-PLAN.md:204` rejected-scope row.
- Gate status: AMENDMENT-REQUIRED.

Gate conclusion:

- The inherited 16 gates remain useful.
- The inherited gate set is not sufficient for V5 because it does not compare grammar productions across PASS-1 and Architecture §8.1.
- Add the two V5 focused checks to the consolidated close checklist.

## §6 Cross-Document Binding Ledger To ARCH / MASTER / MIGRATION

| Row | PASS-1 claim | PASS-1 site | Architecture binding | Master binding | Migration binding | Status |
|---|---|---|---|---|---|---|
| L1 | Grammar IR owns source syntax and side-table facts. | `PASS-1.md:24-37` | `ARCHITECTURE.md:808-874` | `MASTER-PLAN.md:88-104` | `MIGRATION.md:377-402` | Bound. |
| L2 | Backend IR is the codegen boundary. | `PASS-1.md:39-53` | `ARCHITECTURE.md:875-969` | `MASTER-PLAN.md:201` | `MIGRATION.md:558-581` | Bound with 22/23 equivalence gate. |
| L3 | PASS-2 receives BIR and owns emitter details. | `PASS-1.md:55-69` | `ARCHITECTURE.md:1208-1225` | `MASTER-PLAN.md:276-309` | `MIGRATION.md:512-535` | Bound. |
| L4 | Type system combines HM, bidirectional checking, CSP choices, and cost extraction. | `PASS-1.md:71-75` | `ARCHITECTURE.md:1103-1120` | `MASTER-PLAN.md:243-274` | `MIGRATION.md:377-402` | Bound. |
| L5 | BBNF grammar surface excludes rewrite-mode and grammar-level Unicode algebra. | `PASS-1.md:176-215` | `ARCHITECTURE.md:1091-1102` | `MASTER-PLAN.md:200` | `MIGRATION.md:651-671` | Bound, except Architecture §8.1 grammar sketch contradicts PASS-1 syntax. |
| L6 | `@host fn` is block-bodied. | `PASS-1.md:183`, `PASS-1.md:211` | `ARCHITECTURE.md:1081` | `MASTER-PLAN.md:393` | `MIGRATION.md:690-770` | Broken in Architecture §8.1. |
| L7 | Rule-level chain syntax is `Expr -> f1 -> f2`. | `PASS-1.md:205-217` | `ARCHITECTURE.md:1077-1079` | `MASTER-PLAN.md:393` | `MIGRATION.md:690-770` | Broken in Architecture §8.1. |
| L8 | Grammar-level lookbehind is finite-width `|<` / `|<!` with BBNF1004. | `PASS-1.md:196`, `PASS-1.md:215` | `ARCHITECTURE.md:1008-1016`, `ARCHITECTURE.md:1065` | `MASTER-PLAN.md:393` | `MIGRATION.md:690-770` | Diagnostic binding works; grammar shape broken in Architecture §8.1. |
| L9 | `OpenFrame` is retired. | `PASS-1.md:57`, `PASS-1.md:282` | `ARCHITECTURE.md:1158-1207` | `MASTER-PLAN.md:198` | `MIGRATION.md:715-728` | Bound. |
| L10 | Yaml onboarding is two surfaces only. | `PASS-1.md:221-227` | `ARCHITECTURE.md:1275-1331` | `MASTER-PLAN.md:744`, `MASTER-PLAN.md:770` | `MIGRATION.md:690-770` | Bound; needs example. |
| L11 | Generated budgets need `generated_loc`, `regen_wall_ms`, and evidence. | `PASS-1.md:237-247` | `ARCHITECTURE.md:1227-1268` | `MASTER-PLAN.md:615-652` | `MIGRATION.md:583-604` | Bound; citation fix needed in Master. |
| L12 | Rare declaration crate path is fenced. | `PASS-1.md:81-91` | `ARCHITECTURE.md:723-760` | `MASTER-PLAN.md:762-770` | `MIGRATION.md:772-783` | Bound but PASS-1 should cite canonical eight-field form. |
| L13 | Path/query macro names are `pointer!` and `select!`. | PASS-1 handoff only | `ARCHITECTURE.md:268-284` | `MASTER-PLAN.md:416-425` | `MIGRATION.md:427-449` | Bound. |
| L14 | Incremental parse and debug runtime are PASS-3 surfaces with PASS-1 substrate requirements. | `PASS-1.md:114`, `PASS-1.md:133` | `ARCHITECTURE.md:262`, `ARCHITECTURE.md:1338` | `MASTER-PLAN.md:482-500` | `MIGRATION.md:404-425` | Bound; needs worked example/debug gate. |
| L15 | Lock 11 incubation keeps sister crates path-dep gated until stable. | PASS-1 inherits lock only | `ARCHITECTURE.md:1350-1377` | `MASTER-PLAN.md:524` | `MIGRATION.md:690-770` | Bound. |

Ledger conclusion:

- Architecture is the only high-authority document with a direct PASS-1 syntax contradiction.
- Master needs a gate expansion because its current PASS-1 reconciliation row would not catch that contradiction.
- Migration is mostly clean; its role is amendment sequencing, not BBNF syntax definition.

## §7 Deduped Punch List

1. `restart/ARCHITECTURE.md:1049-1081`
   - Surgery: replace the Core Grammar Sketch productions for lookbehind, map/chain, and host declarations with PASS-1 §6 equivalents or mark Architecture §8.1 as non-normative and bind to PASS-1.
   - Acceptance gate: `rg -n 'HostDecl.*;|MapExpr.*=>|Lookbehind    ::= "\\|<" Suffix' restart/ARCHITECTURE.md` returns zero, and `rg -n 'HostFn|MapTail|ChainExpr|Expr "\\|<" Expr' restart/ARCHITECTURE.md restart/audit/pass-1-substrate/PASS-1.md` shows coherent forms.
   - Lens origin: A1, B4, B5, B6, F1, G1.

2. `restart/MASTER-PLAN.md:748`
   - Surgery: expand the PASS-1 reconciliation gate from "Architecture §7 schema matches PASS-1 §2 enum" to include "Architecture §8.1 BBNF surface matches PASS-1 §6 grammar productions."
   - Acceptance gate: `rg -n 'PASS-1 reconciliation|Architecture §8.1|PASS-1 §6' restart/MASTER-PLAN.md` shows the broadened gate in one row.
   - Lens origin: A2, H1.

3. `restart/audit/pass-2-codegen/PASS-2.md:540-541`
   - Surgery: remove `@pratt` and `@simd` remediation language from BBNF-OPT diagnostics; replace with automatic detection evidence and grammar restructuring guidance.
   - Acceptance gate: `rg -n '@pratt|@simd' restart/audit/pass-2-codegen/PASS-2.md restart/ARCHITECTURE.md restart/MASTER-PLAN.md` returns only rejected-scope rows, not user-facing diagnostics.
   - Lens origin: B7, E3, F3.

4. `restart/MASTER-PLAN.md:636`
   - Surgery: correct the stale Architecture line citation for §12.1 per-grammar baselines.
   - Acceptance gate: `rg -n 'ARCHITECTURE.md:1273-1281' restart/MASTER-PLAN.md` returns zero.
   - Lens origin: A4, H1, G4.

5. `restart/audit/pass-2-codegen/PASS-2.md:174`
   - Surgery: update the stale `BBNF-SEM040` line reference from line 478 to the current diagnostic table row.
   - Acceptance gate: `rg -n 'BBNF-SEM040.*line 478|line 478.*BBNF-SEM040' restart/audit/pass-2-codegen/PASS-2.md` returns zero.
   - Lens origin: H2.

6. `restart/audit/pass-1-substrate/PASS-1.md:81-91`
   - Surgery: either add `deletion path` and `reviewer` fields to the rare declaration-crate fence, or cite Architecture's canonical eight-field review form.
   - Acceptance gate: `rg -n 'deletion path|reviewer|Architecture.*eight-field|declaration-crate review' restart/audit/pass-1-substrate/PASS-1.md` shows the added bridge.
   - Lens origin: H3, D1.

7. `restart/audit/pass-1-substrate/PASS-1.md:221-227` plus receiver in Architecture/Master cookbook rows
   - Surgery: add one yaml onboarding walkthrough: add `grammars/yaml.bbnf`, add one workspace metadata block, run generation, observe generated runtime/path/visitor outputs, and reject a manual Rust registry edit.
   - Acceptance gate: `rg -n 'yaml onboarding walkthrough|grammars/yaml.bbnf|workspace.metadata.bbnf.grammars.yaml|manual Rust registry' restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-1-substrate/PASS-1.md` finds the positive and negative path.
   - Lens origin: C1, D1, G2.

8. `restart/audit/pass-3-runtime/PASS-3.md:82-115`
   - Surgery: add one `pointer!` + `select!` query example over generated metadata, tied to a path schema sidecar and one diagnostic path.
   - Acceptance gate: `rg -n 'pointer!.*select!|select!.*pointer!|path schema.*diagnostic' restart/audit/pass-3-runtime/PASS-3.md restart/MASTER-PLAN.md` finds the example or receiver.
   - Lens origin: C2.

9. `restart/audit/pass-3-runtime/PASS-3.md:158-190`
   - Surgery: add one incremental edit walkthrough showing dirty range, anchors, reuse/fallback decision, bench ledger, and silent LSP output.
   - Acceptance gate: `rg -n 'dirty range|anchor|fallback reason|BBNF_LSP_DEBUG|silent on fallback' restart/audit/pass-3-runtime/PASS-3.md restart/MASTER-PLAN.md` finds the example/gate.
   - Lens origin: C3, D2, G3.

10. `restart/audit/pass-3-runtime/PASS-3.md:156`
    - Surgery: promote debug/DAP tape identity reuse from advisory wording to a hard receiver gate.
    - Acceptance gate: `rg -n 'Debug and DAP must reuse|DAP.*must reuse|debug.*tape identity.*gate' restart/audit/pass-3-runtime/PASS-3.md restart/MASTER-PLAN.md` finds mandatory wording.
    - Lens origin: D3, F2.

11. `restart/MASTER-PLAN.md:208-537`
    - Surgery: add one A->F->J trajectory for a single grammar, showing metadata admission, BIR/codegen, runtime/path surface, parity, budget, and docs evidence.
    - Acceptance gate: `rg -n 'A->F->J|A to F to J|grammar trajectory|yaml trajectory|json trajectory' restart/MASTER-PLAN.md restart/ARCHITECTURE.md` finds the trajectory.
    - Lens origin: C5.

12. `restart/README.md:473`
    - Surgery: remove stale "rewrite-mode + lookbehind + Unicode sets" summary or replace it with settled extension vocabulary from PASS-1 and Locks.
    - Acceptance gate: `rg -n 'rewrite-mode \\+ lookbehind \\+ Unicode sets' restart/README.md` returns zero.
    - Lens origin: F4.

Punch-list priority:

- Must fix before READY: items 1, 2, 3.
- Provenance cleanup: items 4, 5, 6.
- Worked-example hardening: items 7, 8, 9, 10, 11.
- Stale close-line cleanup: item 12.

## §8 V1->V4 History Note

V1 found PASS-1 amendment needs around formal grammar, BBNF surface, OpenFrame deletion, yaml proof, generated budgets, and handoffs.

V2 marked PASS-1 READY after Wave 1.1 and Wave 2 amendments landed.

V3 kept PASS-1 READY and listed only structural residuals: rare escape fence shape, synthesis input-normalization gate, closure research tranche binding, and similar carry-through items.

V4 consolidated the cohort as READY and treated PASS-1 as carried through from V3.

V5 does not reject the V2-V4 conclusion that PASS-1 itself became coherent.

V5 changes the audit strictness:

- It compares high-authority downstream grammar sketches back to PASS-1 §6.
- It checks user-facing diagnostic strings for retired syntax.
- It treats stale line citations as evidence risk, not harmless prose.
- It asks for worked examples where V4 accepted dense tables.

Therefore the correct V5 posture is narrow amendment, not broad re-draft.

## §9 LLM-Pathology Summary

The dominant pathology is generic-language drift.

Architecture §8.1 appears to have reintroduced common grammar-design shapes:

- Bodyless function declarations.
- Prefix lookbehind.
- Method chains at the grammar-rule surface.
- `=>` map expressions.

Those shapes are plausible in isolation, but they are wrong for the settled PASS-1 contract.

The second pathology is retired-directive leakage.

PASS-2 diagnostic strings mention `@pratt` and `@simd`, even while Master names those directives as rejected scope.

The third pathology is citation confidence.

Master and PASS-2 carry a few stale line references that can hide drift during later amendment dispatch.

The mitigation pattern is simple:

- Bind grammar productions to one canonical source.
- Convert soft advisory statements into receiver gates where the surface matters.
- Delete user-facing strings that teach retired syntax.
- Prefer one small worked example over another dense assertion table when onboarding or debugging behavior is the risk.

## §10 Verdict

Verdict: `AMENDMENT-REQUIRED`.

Target health:

- PASS-1 is internally coherent enough to remain the canonical substrate source.
- PASS-1 should not be re-drafted wholesale.

Blocking amendments:

- Architecture §8.1 must stop contradicting PASS-1 §6.
- Master PASS-1 reconciliation gate must include Architecture §8.1 grammar-surface equivalence.
- PASS-2 OPT diagnostics must stop exposing `@pratt` and `@simd`.

Ready condition:

- After those three edits and the two stale-citation cleanups, this target can return to READY without reopening the full V1-V4 hardening set.

## §11 Closing Posture

Estimated amendment wall time: 2.5 to 3.5 hours.

Suggested amendment order:

1. Patch Architecture §8.1 grammar sketch against PASS-1 §6.
2. Patch Master §24 PASS-1 reconciliation gate.
3. Patch PASS-2 BBNF-OPT diagnostics.
4. Patch stale citations in Master and PASS-2.
5. Add worked examples only if the amendment window includes documentation hardening beyond readiness blockers.

Stop condition:

- If the three blocking amendments pass the focused V5 checks, classify remaining worked examples as follow-up hardening, not a PASS-1 readiness blocker.
