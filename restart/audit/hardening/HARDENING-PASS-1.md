# HARDENING-PASS-1

## §1 Target identification

| Field | Value |
|---|---|
| Target | PASS-1 |
| Output path | `restart/audit/hardening/HARDENING-PASS-1.md` |
| Target files | `restart/audit/pass-1-substrate/PASS-1.md`; `restart/audit/pass-1-substrate/agent-1-ir-architect.md`; `restart/audit/pass-1-substrate/agent-2-type-system-designer.md`; `restart/audit/pass-1-substrate/agent-3-csp-egraph-architect.md`; `restart/audit/pass-1-substrate/agent-4-cost-model-architect.md`; `restart/audit/pass-1-substrate/agent-5-grammar-extension-designer.md`; `restart/audit/pass-1-substrate/agent-6-substrate-coherence-auditor.md` |
| Commit audited | `015317db283ea1e9652401a6a7438ffa5baf028c` |
| Lines audited | PASS-1 synthesis 1-150; six sub-agent reports 1-61 or 1-62 each |
| Ground truth read | `restart/README.md`; `restart/locks/14-LOCKS.md`; `docs/precepts/instructions/{STYLE,LESSONS-LEARNED,CONSUMING}.md`; `restart/prompts/{PASS-1-SUBSTRATE,PASS-2-CODEGEN,PASS-3-RUNTIME,SYNTHESIS}.md`; `restart/corpora/{CENSUS,MODULES,RESTART-SKETCH,SOTA}.md`; `restart/inheritance/INDEX.md` |
| Time consumed | Completed inside the 45-minute PASS hard cap |

## §2 Cohort verdict

| Lane | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|
| Lane 1 - Lock-Adherence | AMENDMENT-REQUIRED | 7 | 7 | 0 | Add explicit hand-off gates for locks that PASS-1 touches only by implication, chiefly Lock 14, Lock 3, Lock 6, Lock 7, Lock 11, and Lock 12. |
| Lane 2 - Sequencing Discipline | N/A | 0 | 0 | 0 | Single PASS target has no multi-wave sequencing claim. |
| Lane 3 - Cohesion | AMENDMENT-REQUIRED | 4 | 3 | 1 | Retain the thesis; repair formal grammar, module rationale, field schemas, and orphan spec punches. |
| Lane 4 - SOTA Anchoring | AMENDMENT-REQUIRED | 4 | 2 | 0 | PASS-1 may define cost taxonomy, but any throughput gate must carry competitor, dataset, platform, and receiving gate. |
| Lane 5 - Grammar-Authoritative Discipline | AMENDMENT-REQUIRED | 4 | 4 | 0 | Add yaml onboarding proof, per-X tables, and a rare escape-valve fence. |
| Lane 6 - Generated-Code + LOC Budget | AMENDMENT-REQUIRED | 2 | 3 | 0 | Convert generated-code pressure from a phrase into budgets, baselines, and regen wall gates. |
| Lane 7 - Friction Forecast | AMENDMENT-REQUIRED | 2 | 5 | 0 | Add verbatim diagnostics and cookbook receivers for PASS-1-owned grammar/type surfaces. |
| Lane 8 - Carry & Deferral Audit | AMENDMENT-REQUIRED | 2 | 4 | 1 | Replace vague later/SYNTHESIS carries with receiver, blocker, and receiving gate; delete the independent-proceed clause. |
| Lane 9 - Greenfield Discipline | AMENDMENT-REQUIRED | 5 | 1 | 1 | Preserve the greenfield direction; stop treating legacy closure machinery and OpenFrame internals as safe inheritance without replacement terms. |
| **Total** | **AMENDMENT-REQUIRED** | **30** | **29** | **3** | **19-item punch list before target advancement.** |

Final decision: **AMENDMENT-REQUIRED**. PASS-1 keeps the right substrate, rejects the stale ParseStream rename, rejects rewrite-mode, delegates Unicode class algebra to regex, and separates Grammar IR from Backend IR. It cannot advance unchanged because the proof surfaces are too thin: Lock 14 lacks the yaml onboarding test, the BBNF EBNF contradicts the accepted `@host fn` and lookbehind surfaces, generated-code budgets are absent, and several deferrals name no blocker or receiving gate.

## §3 Lane 1 - Lock-Adherence

Lane standard: Each of the 14 settled locks is tested against PASS-1 and its six sub-agent outputs. The audit verifies adherence; it does not reopen the lock.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-1-substrate/PASS-1.md:7`; `restart/audit/pass-1-substrate/agent-6-substrate-coherence-auditor.md:7`; `restart/locks/14-LOCKS.md:34` | Lock 1 - tape retained, ParseStream rename discarded | PASS-1 keeps tape as substrate, unioned with direct-to-struct, and rejects columnar/parallel substrates. | Directly follows the current lock; agent 6 names ParseStream rename dead. | The target still cites stale README and inheritance text as risks. | The steelman alternative is a neutral stream name to avoid prior baggage; Lock 1 now settles tape, so the alternative fails. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:53`; `restart/audit/pass-1-substrate/agent-2-type-system-designer.md:24`; `restart/locks/14-LOCKS.md:36` | Lock 2 - layout-lowering canon | PASS-1 includes `passes/layout/` and separates semantic type, layout, and materialization. | The module tree has the canonical `layout/` child. | It still uses imprecise phrases like "layout representation" and "TypeDesc/layout split" without stating the canonical pass name. | A reader can infer the separation, but locks require named surfaces. Add one sentence: "Layout lowering is the pass name; semantic type inference is separate." | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:128`; `restart/locks/14-LOCKS.md:38` | Lock 3 - cursor-parse plus byte-skip unification | PASS-1 reanchors BA.W4 cursor/unification to source normalization plus tape value substrate. | The legacy inheritance is not forgotten. | It does not assert one parse implementation, skip consultation, or empty-path branch elision. | PASS-1 is substrate, but it already hands value/path contracts forward; therefore it must name the parse unification invariant as a receiver gate. | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:13`; `restart/audit/pass-1-substrate/agent-3-csp-egraph-architect.md:13`; `restart/locks/14-LOCKS.md:40` | Lock 4 - per-domain optimization | CSP and e-graph remain bridged, not fused; cost chooses legal alternatives. | Strong alignment with the lock and with README optimizer structure. | Adapter schemas are still pending. | The hypergraph alternative reduces adapters, but it violates the lock and hides domain ownership. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:9`; `restart/audit/pass-1-substrate/PASS-1.md:26`; `restart/locks/14-LOCKS.md:42` | Lock 5 - IR plus per-backend lower | Grammar IR and Backend IR are named; Backend IR is the executable codegen contract. | Correct architectural boundary; VM consumes Backend IR. | Variant payloads and lower-time invariants are not specified in PASS-1. | The challenge is that PASS-2 owns final lowerer shape; PASS-1 still supplies the contract, so fields must be named or routed. | KEEP |
| `restart/audit/pass-1-substrate/agent-4-cost-model-architect.md:41`; `restart/locks/14-LOCKS.md:44` | Lock 6 - xtask emits committed source | Generated-code budget data is routed to metadata or side tables. | PASS-1 does not propose proc-macro codegen output. | It never states `cargo xtask regen --check` or committed emitted source as a PASS-2 receiving gate. | The lock is codegen-owned, but PASS-1 owns Backend IR and generated-code pressure; the hand-off must carry regen equality. | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:141`; `restart/locks/14-LOCKS.md:46` | Lock 7 - path consolidation | PASS-1 proposes `path-api.md`. | The path API is recognized as a substrate consumer. | It does not mention `path`, `path-core`, or `path-ts`, so consolidation can drift in PASS-3. | PASS-3 owns the path crates, but PASS-1's path/value contract must name the allowed crate triplet. | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:14`; `restart/audit/pass-1-substrate/agent-4-cost-model-architect.md:14`; `restart/locks/14-LOCKS.md:48` | Lock 8 - SOTA gates | PASS-1 keeps named SOTA gates as cost-model evidence. | References the README gate table and SOTA corpus. | PASS-1's own text lacks the numbers. | Cost taxonomy may cite the gate source instead of restating all numbers; later throughput gates must carry the full anchor. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:7`; `restart/audit/pass-1-substrate/agent-6-substrate-coherence-auditor.md:23`; `restart/locks/14-LOCKS.md:50` | Lock 9 - slice-borrow primary | Tape/direct value union is explicitly tied to slice-borrow and value API. | Correctly avoids bumpalo-first or owned-first posture. | API signatures are PASS-3-owned. | The challenge is that slice-borrow can be lost when API design lands; PASS-1 correctly routes the value contract to PASS-3. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:129`; `restart/audit/pass-1-substrate/agent-5-grammar-extension-designer.md:24`; `restart/locks/14-LOCKS.md:52` | Lock 10 - Pratt and SIMD auto-detected | Recognizer facts feed CSP/cost; no `@pratt` or `@simd` directives survive. | Clear adherence. | Misfire diagnostics are absent. | Directives would give author control, but the lock requires mining and diagnostics, not user annotation. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:57`; `restart/audit/pass-1-substrate/PASS-1.md:58`; `restart/audit/pass-1-substrate/PASS-1.md:59`; `restart/locks/14-LOCKS.md:54` | Lock 11 - path-deps for sister crates | `egraph`, `csp-solver`, and `parse-that` appear as workspace crates. | Sister-crate boundaries are preserved. | Incubating path-dep policy and publication trigger are silent. | PASS-1 need not solve publication, but it must prevent early registry coupling. | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:123`; `restart/inheritance/INDEX.md:31`; `restart/locks/14-LOCKS.md:56` | Lock 12 - ser plus gorgeous archive before execution | PASS-1 inheritance table exists. | The archive obligation exists in inheritance ground truth. | PASS-1 does not restate the precondition. | It is not a substrate design issue, but hardening all locks requires a line that PASS-1 depends on Tranche A.W0 archive completion. | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:46`; `restart/audit/pass-1-substrate/PASS-1.md:61`; `restart/locks/14-LOCKS.md:58` | Lock 13 - no god directories | The per-crate tree keeps 4-10 children and cites Lock 13. | Correct child counts for PASS-1 crates. | Rationale is thin. | The skeletal table could become ornamental, but the shape obeys child-count discipline and needs only deeper rationale under Lane 3. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:36`; `restart/audit/pass-1-substrate/PASS-1.md:86`; `restart/README.md:13`; `restart/locks/14-LOCKS.md:60` | Lock 14 - full grammar generalization | PASS-1 rejects default per-grammar crates and routes host logic through primitives, metadata, or `@host fn`. | Correct thesis. | No yaml onboarding test, no per-X tables, and no rare escape-valve fence. | Generic claims are true in spirit; Lock 14 requires a proof surface, not merely the theorem. | REINVENT |

Lane verdict: **AMENDMENT-REQUIRED**. KEEP 7 / REINVENT 7 / DISCARD 0.

## §4 Lane 2 - Sequencing Discipline

Lane standard: This lane applies to multi-wave targets. PASS-1 is a single PASS output, not a tranche or wave plan. The lane is therefore N/A unless PASS-1 makes multi-wave sequencing claims; it does not.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-1-substrate/PASS-1.md:63`; `restart/audit/pass-1-substrate/PASS-1.md:74` | PASS-level hand-offs, no wave sequence | PASS-1 names contracts to PASS-2 and PASS-3 but does not define multi-wave execution. | Correct for a PASS target. | Carry quality is audited in Lane 8. | Treating this as wave sequencing would fabricate a lane. | N/A |

Lane verdict: **N/A**. Counts excluded from cohort totals.

## §5 Lane 3 - Cohesion

Lane standard: Every claim must be supported by target text or cited ground truth, and every deliverable must have a consumer. This lane tests whether the PASS-1 artefact is internally usable by SYNTHESIS without silent invention.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-1-substrate/PASS-1.md:7`; `restart/audit/pass-1-substrate/PASS-1.md:8`; `restart/audit/pass-1-substrate/PASS-1.md:150` | Tape and ParseStream reconciliation | PASS-1 decides the naming conflict and states the surviving substrate. | Clear, settled, and repeated. | It relies on stale README/inheritance citations as negative evidence. | A reader could be confused by stale citations, but PASS-1 marks them stale and gives the decision. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:5`-`restart/audit/pass-1-substrate/PASS-1.md:20` | Verdict ledger coverage | The ledger enumerates the main PASS-1 concerns and maps agent evidence to decisions. | Strong summary surface for SYNTHESIS. | It compresses several REINVENT items without naming exact redesign. | The full agent reports carry the per-item discipline, so the ledger can remain compressed if punch-list entries are surgical. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:24` | Grammar IR variant list | The line proposes the semantic node inventory. | Compact and consistent with the two-IR thesis. | No field schema, side-table keys, producers, or consumers are present. | The challenge is to leave field detail to `restart/specs`; that makes SYNTHESIS invent the contract. | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:26`; `restart/audit/pass-1-substrate/agent-1-ir-architect.md:15` | Backend IR variant list | The list names executable plan nodes. | It includes the needed op families: dispatch, regex, SIMD, Pratt, host, layout, tape, direct build. | It lacks per-variant payloads, lower-time invariants, and backend ownership. | PASS-2 refines Backend IR, but PASS-1 is the first contract; it must include at least payload categories and forbidden upstream leaks. | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:32`; `restart/audit/pass-1-substrate/agent-4-cost-model-architect.md:17` | Cost evidence | Extraction records selected and rejected alternatives. | This has a consumer: VM/debug hooks and backend emitters. | Evidence schema is not named in the synthesis. | The agent proposes `extraction-evidence.md`; the synthesis should name the side-table columns or route them. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:91`; `restart/README.md:149`-`restart/README.md:155` | `@host fn` formal grammar | PASS-1 EBNF says `HostFn ... ";"`; README's accepted surface has a block body with closure semantics. | The target keeps host functions. | The formal grammar erases the body and contradicts closure semantics. | A declaration-only host signature could be useful for metadata, but the settled surface is an in-grammar function body. Replace the production. | DISCARD |
| `restart/audit/pass-1-substrate/PASS-1.md:46`-`restart/audit/pass-1-substrate/PASS-1.md:60`; `restart/README.md:100` | Per-crate `src/` tree | PASS-1 supplies child lists for bottom-layer crates. | All entries sit within the 4-10 child range. | The prompt required per-module rationale; the target gives one sentence for the whole table. | A table is enough for a sketch, but SYNTHESIS needs rationale and public sibling API uniformity. | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:125`-`restart/audit/pass-1-substrate/PASS-1.md:133` | Inheritance ledger | Legacy substances are sorted into carries, dissolves, and reanchors. | Useful, cited, and not overlong. | Some rows defer exact mechanics. | The ledger is an inheritance map, not execution text; the mechanics are handled by later master plan gates. | KEEP |

Lane verdict: **AMENDMENT-REQUIRED**. KEEP 4 / REINVENT 3 / DISCARD 1.

## §6 Lane 4 - SOTA Anchoring

Lane standard: Throughput gates must cite competitor, dataset, and platform. Engineering gates that are not throughput gates must not claim Lock 8 by implication.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-1-substrate/PASS-1.md:14`; `restart/README.md:326`-`restart/README.md:334` | Cost model cites SOTA gate source | PASS-1 ties the cost model to the README SOTA table. | The cited README includes twitter, canada, citm, bootstrap, animate, and simdjson On-Demand targets. | PASS-1 does not restate the values. | For PASS-1, citing the canonical table is acceptable; throughput gates later must restate numbers. | KEEP |
| `restart/audit/pass-1-substrate/agent-4-cost-model-architect.md:15`; `restart/corpora/SOTA.md:50`-`restart/corpora/SOTA.md:58`; `restart/corpora/SOTA.md:83`-`restart/corpora/SOTA.md:89` | sonic-rs and simdjson evidence | Agent 4 names direct/lazy and tape/on-demand evidence. | Grounded in SOTA corpus lines. | It is architectural evidence, not a benchmark gate. | The agent does not misuse it as a throughput close gate. | KEEP |
| `restart/audit/pass-1-substrate/agent-4-cost-model-architect.md:14`; `restart/corpora/SOTA.md:130`-`restart/corpora/SOTA.md:135` | lightning-css gate source | CSS SOTA is available through README and SOTA corpus. | Correct competitor family. | PASS-1 does not name bootstrap and animate values in its own text. | PASS-1 should add a compact table in `sota-gates.md` or route to PASS-2/3 gate text. | REINVENT |
| `restart/audit/pass-1-substrate/agent-4-cost-model-architect.md:24`; `docs/precepts/instructions/LESSONS-LEARNED.md:274`-`docs/precepts/instructions/LESSONS-LEARNED.md:292` | Generated LOC as cost pressure | Generated LOC is scored separately from throughput. | Avoids false Lock 8 claim. | It has no numeric budget yet. | This belongs under Lane 6, not SOTA. | KEEP |
| `restart/audit/pass-1-substrate/agent-4-cost-model-architect.md:58`; `restart/prompts/PASS-2-CODEGEN.md:56`-`restart/prompts/PASS-2-CODEGEN.md:57` | "Old exact numeric gates are not PASS-1 requirements" | Agent 4 limits PASS-1 scope. | Prevents premature perf promises in substrate docs. | It does not name the receiving PASS-2 gate that will set the numbers. | Scope control survives only if the receiver gate is named. | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:32`; `restart/audit/pass-1-substrate/agent-4-cost-model-architect.md:17` | Extraction evidence | Cost evidence records why a plan won. | Useful for perf triage without claiming throughput. | None for SOTA anchoring. | The evidence must later be connected to benchmark deltas, but PASS-1 is right to keep it non-throughput. | KEEP |

Lane verdict: **AMENDMENT-REQUIRED**. KEEP 4 / REINVENT 2 / DISCARD 0.

## §7 Lane 5 - Grammar-Authoritative Discipline

Lane standard: Lock 14 deep-dive. The target must avoid grammar-specific code in generic crates, must carry per-X tables for all-grammar/all-backend claims, and must include the future-grammar onboarding proof.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| verification | Grammar-id grep classification | `rg -ni 'json|css_l4|bbnf|google_sheets|sheets|css_pretty|bnf|csv|ebnf|math' restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-1-substrate/agent-*.md` returns examples, anti-pattern citations, and fixture/corpus references. | Matches are ratified as evidence cells or anti-pattern examples, not plan logic. | The target still lacks a written classification table. | Add this command and classification to PASS-1 hardening amendments. | KEEP |
| verification | Match-arm grep | `rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-1-substrate/agent-*.md` returned zero. | Directly satisfies the zero match-arm check for the target text. | This does not prove future codegen templates. | PASS-1 text passes this grep. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:36`; `restart/inheritance/INDEX.md:62` | Default per-grammar declaration crates discarded | Normal grammars use primitives, metadata, and `@host fn`. | Correct and settled. | "Rare escape" is not fenced. | The default is right; the escape needs criteria. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:86`; `restart/audit/pass-1-substrate/PASS-1.md:119` | Rewrite-mode and grammar-level Unicode excluded | The formal section explicitly excludes both. | Correct against current authority. | The EBNF still has other syntax faults. | The exclusion itself survives. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:135`-`restart/audit/pass-1-substrate/PASS-1.md:142`; `restart/README.md:13`; `restart/README.md:396` | Future grammar onboarding | PASS-1 has no `yaml.bbnf` two-surface walkthrough. | The README supplies the rule. | PASS-1 was asked to verify it via the coherence auditor; no target line does so. | A generic declaration is insufficient; add the concrete yaml test. | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:34`; `restart/audit/pass-1-substrate/PASS-1.md:82` | All-grammar/all-backend claims | PASS-1 claims regex ownership, host chains, and cross-backend parity pressure. | Correct direction. | No per-X table exists for "all grammars", "normal grammars", or "Rust/TS/WASM". | The claim can stand only with per-X rows or explicit narrowing. | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:20`; `restart/audit/pass-1-substrate/PASS-1.md:36` | Rare escape valve | The target says escape valve is named and audited. | It avoids default declaration crates. | It does not say who approves, what metadata failed, where the code lives, or how it is fenced from generic crates. | The steelman is that rare cases need escape; the fence must be explicit. | REINVENT |
| `restart/audit/pass-1-substrate/agent-6-substrate-coherence-auditor.md:52`; `restart/corpora/CENSUS.md:115`-`restart/corpora/CENSUS.md:117` | Runtime variation routed to generated code or metadata | Agent 6 points grammar variation out of generic crates. | Correct high-level route. | The target does not forbid hand-written per-grammar runtime files in the amendment text. | Lock 14 requires "template-generated only" stated as a gate. | REINVENT |

Lane verdict: **AMENDMENT-REQUIRED**. KEEP 4 / REINVENT 4 / DISCARD 0.

## §8 Lane 6 - Generated-Code + LOC Budget

Lane standard: Every generated-code-affecting proposal needs baseline, per-grammar projection, generated LOC ceiling, and xtask regen-cycle wall budget.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-1-substrate/PASS-1.md:32`; `restart/audit/pass-1-substrate/agent-4-cost-model-architect.md:24` | Generated LOC as scored pressure | Cost model considers generated-code pressure. | Correctly prevents output bloat from hiding behind speed. | It is a pressure, not a budget. | This is a good hook if numeric gates follow. | KEEP |
| `restart/audit/pass-1-substrate/agent-4-cost-model-architect.md:41` | Budget data in metadata or side tables | PASS-2 receives generated-code budget data. | Keeps source comments out of the contract. | No schema columns are named. | The target can pass if the amendment names `generated_loc_before`, `generated_loc_after`, and `regen_wall_ms`. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:24`-`restart/audit/pass-1-substrate/PASS-1.md:36`; `restart/prompts/PASS-2-CODEGEN.md:56` | Per-grammar generated LOC projection | Grammar IR, Backend IR, and layout decisions all affect generated output. | PASS-1 knows which constructs pressure output. | No per-grammar projection or ceiling appears. | PASS-2 owns codegen, but PASS-1 must route construct-level pressure into the budget schema. | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:137`-`restart/audit/pass-1-substrate/PASS-1.md:141`; `docs/precepts/instructions/LESSONS-LEARNED.md:274`-`docs/precepts/instructions/LESSONS-LEARNED.md:292` | Generated-size budget gate | The punch list proposes specs but no generated-size table. | Spec drafting can hold the budget. | The close gate has no numeric pass/fail condition. | Prior generated bloat was a named incident; PASS-1 cannot leave it implicit. | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:61`; `restart/README.md:206`; `restart/prompts/PASS-2-CODEGEN.md:3` | xtask regen-cycle wall budget | Regen equality is a settled execution discipline. | PASS-1 does not propose proc macro output. | No wall-time budget is included for PASS-1-derived emitted artifacts. | Add a receiver gate: PASS-2 `§6 Generated-LOC budget` must include xtask wall ceilings. | REINVENT |

Lane verdict: **AMENDMENT-REQUIRED**. KEEP 2 / REINVENT 3 / DISCARD 0.

## §9 Lane 7 - Friction Forecast

Lane standard: User and grammar-author friction surfaces need cookbook receivers or verbatim errors. PASS-1 owns grammar/type/host/layout diagnostics; PASS-3 owns end-user API cookbooks.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-1-substrate/PASS-1.md:38` | Error vocabulary | PASS-1 names error categories. | The right category set exists. | Categories are not diagnostics. | The vocabulary is a base, not the user-facing surface. | KEEP |
| `restart/audit/pass-1-substrate/agent-2-type-system-designer.md:14` | Source spans on constraints | Bidirectional checking requires source spans. | Good diagnostic foundation. | It does not give actual messages. | Span discipline survives as infrastructure. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:103`-`restart/audit/pass-1-substrate/PASS-1.md:105`; `restart/audit/pass-1-substrate/PASS-1.md:119` | Lookbehind legality | Lookbehind is formalized, but finite-width diagnostics are absent. | The operator is in scope. | No verbatim `LookbehindWidth` error is defined. | Add: `BBNF1004: lookbehind in rule {rule} must have finite maximum width; {expr} is unbounded after {operator}.` | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:91`; `restart/audit/pass-1-substrate/PASS-1.md:121` | Host function confusion | Host calls and grammar closures have separate runtime/compile-time roles. | The distinction is named. | No `HostSignature` or purity/allocation error text appears. | Add: `BBNF1201: host function {name} cannot satisfy signature {expected}; argument {index} inferred {actual} at {span}.` | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:93`; `restart/audit/pass-1-substrate/PASS-1.md:116` | Layout conflict | `@layout` and type shapes need explainable failures. | Error vocabulary includes `LayoutConflict`. | No conflict message tells authors what to change. | Add: `BBNF1302: @layout({wanted}) on rule {rule} conflicts with inferred {inferred}; remove the hint or change {field}.` | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:113`-`restart/audit/pass-1-substrate/PASS-1.md:114`; `restart/audit/pass-1-substrate/PASS-1.md:42` | Chain syntax and cookbook | Chain expressions use method syntax in one place and map-tail syntax in another. | Multi-function chaining is accepted. | No cookbook or diagnostic explains type flow through chained calls. | Add a PASS-3 cookbook receiver and PASS-1 type error for failing chain steps. | REINVENT |
| `restart/audit/pass-1-substrate/agent-3-csp-egraph-architect.md:15`; `restart/audit/pass-1-substrate/agent-5-grammar-extension-designer.md:60` | Pratt/SIMD auto-detect misfire | Recognizer facts are generated automatically. | Honours Lock 10. | No warning tells an author why a shape was or was not classified. | Add: `BBNF2103: rule {rule} was not lowered as Pratt; candidate operator {op} lacks stable precedence metadata.` | REINVENT |

Lane verdict: **AMENDMENT-REQUIRED**. KEEP 2 / REINVENT 5 / DISCARD 0.

## §10 Lane 8 - Carry & Deferral Audit

Lane standard: Every later, deferred, carry, or future item must name receiver, blocker, and receiving gate.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-1-substrate/PASS-1.md:65`-`restart/audit/pass-1-substrate/PASS-1.md:71` | PASS-2 core receivers | Grammar IR, Backend IR, cost, e-graph, and host metadata name receiving modules. | Good starting hand-off table. | Blocker and receiving gate columns are missing. | Add columns; keep the receivers. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:78`-`restart/audit/pass-1-substrate/PASS-1.md:81` | PASS-3 core receivers | Host dispatch, errors, debug hooks, and path/value API name runtime consumers. | Correct receiver set. | No gate line is named. | Keep the rows with gate references to PASS-3 §6 and §2. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:72` | "later value/runtime modules" | Tape/direct value contract is handed to `ir/side_tables/`, value/runtime modules, and VM runner. | It names plausible consumers. | "later" has no receiver gate or blocker. | Replace with PASS-3 value API gate and PASS-2 runtime template gate. | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:82`; `restart/inheritance/INDEX.md:38`-`restart/inheritance/INDEX.md:40` | Cross-backend parity carry | Rust/TS/WASM share Backend IR semantics later. | Parity pressure is real. | TS is scope-deferred; WASM is V1; BD is legacy, not a current receiving gate. | Split Rust/WASM V1 and TS deferred receivers with blockers. | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:137`-`restart/audit/pass-1-substrate/PASS-1.md:141` | Draft spec punch list | PASS-1 wants pass-1 specs drafted. | These specs would close many cohesion gaps. | No owner, blocker, or gate; output path is outside PASS-1 target. | Route each to SYNTHESIS `ARCHITECTURE.md` §7 or §8, or amend PASS-1 itself. | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:142`; `restart/prompts/SYNTHESIS.md:112`-`restart/prompts/SYNTHESIS.md:118` | Stale clause reconciliation | PASS-1 tells SYNTHESIS to reconcile ParseStream, rewrite-mode, and Unicode conflicts. | Correct receiver family. | It lacks the exact receiving gate and blocker. | Add blocker: stale PASS-2/PASS-3 prompts; gate: SYNTHESIS conflict table before ARCHITECTURE §8. | REINVENT |
| `restart/audit/pass-1-substrate/PASS-1.md:150`; `restart/prompts/PASS-2-CODEGEN.md:81`; `restart/prompts/PASS-3-RUNTIME.md:3` | "PASS-2 and PASS-3 may proceed independently" | The line permits sister passes to advance while naming conflicts remain. | Parallelism saves wall time. | PASS-2 and PASS-3 prompts still contain ParseStream and rewrite-mode drift; independent advancement compounds faults. | Delete the independent-proceed clause and require SYNTHESIS reconciliation before advancement. | DISCARD |

Lane verdict: **AMENDMENT-REQUIRED**. KEEP 2 / REINVENT 4 / DISCARD 1.

## §11 Lane 9 - Greenfield Discipline

Lane standard: The audit tests no quick solutions, no workarounds, no legacy code uncontested, no overfitting, idiomatic Rust boundaries, and architectural transpositions.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-1-substrate/PASS-1.md:7`; `restart/audit/pass-1-substrate/agent-6-substrate-coherence-auditor.md:13` | Tape substrate restored correctly | PASS-1 treats old tape failure as implementation failure, not conceptual death. | Honours current authority and avoids direct-only dogma. | Requires careful same-consumer wiring later. | The old SOTA corpus argues against tape, but current lock settles tape unioned with direct-to-struct. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:16`; `restart/audit/pass-1-substrate/PASS-1.md:17`; `restart/audit/pass-1-substrate/PASS-1.md:86` | Rewrite and Unicode pruned | PASS-1 rejects rewrite-mode and moves Unicode algebra to regex. | Ruthless excision of unnecessary grammar surface. | Stale prompts still disagree. | The target correctly names the stale prompt as stale. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:30`; `restart/audit/pass-1-substrate/agent-3-csp-egraph-architect.md:13` | CSP/e-graph bridge | Separate substrates compose through explicit facts. | Idiomatic separation and no fused solver. | Adapter schema not yet written. | The bridge is the right transposition; schema detail is amendable. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:20`; `restart/audit/pass-1-substrate/PASS-1.md:36` | Per-grammar crates rejected as default | Host functions decompose through generic primitives, metadata, and `@host fn`. | Abrogates overfit declaration-crate sprawl. | Fence criteria absent. | The posture is right; Lane 5 supplies the fence surgery. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:46`-`restart/audit/pass-1-substrate/PASS-1.md:60` | Cohesive bottom-layer module shape | PASS-1 splits bottom-layer crates by concern. | Greenfield workspace shape is comprehensible and Lock 13-compatible. | Rationale thin. | The structure is viable; flesh out rationale before SYNTHESIS. | KEEP |
| `restart/audit/pass-1-substrate/PASS-1.md:121`; `docs/precepts/instructions/LESSONS-LEARNED.md:82`-`docs/precepts/instructions/LESSONS-LEARNED.md:90` | Legacy closure beta-reduction machinery | PASS-1 cites current closure lowering as existing machinery. | Source signal can inform design. | The line risks inheriting legacy code without contest. | Reframe as "research signal to replace" and require a fresh spec plus verification. | REINVENT |
| `restart/audit/pass-1-substrate/agent-6-substrate-coherence-auditor.md:17`; `restart/corpora/CENSUS.md:321`-`restart/corpora/CENSUS.md:328` | OpenFrame as backend-internal stack detail | Agent 6 permits runtime builders as internals. | A private stack can be legitimate during lowering. | The old OpenFrame ladder is the named failure mode; "do not expose" is too weak. | Delete OpenFrame preservation language; replace with generated Backend IR builder frames with no public or generic substrate role. | DISCARD |

Lane verdict: **AMENDMENT-REQUIRED**. KEEP 5 / REINVENT 1 / DISCARD 1.

## §12 Punch list

| # | Target site | Verbatim edit or surgery | Source verdict | Owner | Scope | Lane(s) |
|---:|---|---|---|---|---|---|
| 1 | `restart/audit/pass-1-substrate/PASS-1.md:24` | Expand the Grammar IR row into a table with variant, fields, stable id keys, producer pass, consumer pass, and forbidden backend leakage. | REINVENT | PASS-1 amendment agent | PASS-1 synthesis | 1, 3 |
| 2 | `restart/audit/pass-1-substrate/PASS-1.md:26` | Add Backend IR payload categories and lower-time invariants for every listed variant; state that PASS-2 may refine payloads but not bypass Backend IR. | REINVENT | PASS-1 amendment agent | PASS-1 synthesis | 1, 3 |
| 3 | `restart/audit/pass-1-substrate/PASS-1.md:91` | Replace `HostFn = ... ";" ;` with a block-bodied production: `HostFn = "@host" "fn" Ident GenericParams? "(" Params? ")" "->" Type HostAttrs? Block ;`. | DISCARD | PASS-1 amendment agent | BBNF formal spec | 3, 7 |
| 4 | `restart/audit/pass-1-substrate/PASS-1.md:103`-`restart/audit/pass-1-substrate/PASS-1.md:105` | Align lookbehind with settled `|<` surface or explicitly state regex-style `(?<=...)` is regex-only; add finite-width legality rule. | REINVENT | PASS-1 amendment agent | BBNF formal spec | 3, 7 |
| 5 | `restart/audit/pass-1-substrate/PASS-1.md:113`-`restart/audit/pass-1-substrate/PASS-1.md:114` | State the canonical chain syntax and type-flow rule for `-> f1 -> f2` versus method-chain form; remove ambiguity. | REINVENT | PASS-1 amendment agent | Type/grammar spec | 3, 7 |
| 6 | `restart/audit/pass-1-substrate/PASS-1.md:46`-`restart/audit/pass-1-substrate/PASS-1.md:60` | Add per-crate rationale and sibling API uniformity notes for each child directory. | REINVENT | PASS-1 amendment agent | Module architecture | 1, 3 |
| 7 | `restart/audit/pass-1-substrate/PASS-1.md:63`-`restart/audit/pass-1-substrate/PASS-1.md:82` | Add `Blocker` and `Receiving gate` columns to both hand-off tables. | REINVENT | PASS-1 amendment agent | Handoffs | 1, 8 |
| 8 | `restart/audit/pass-1-substrate/PASS-1.md:72` | Replace "later value/runtime modules" with named receivers: PASS-2 runtime template gate and PASS-3 value API gate. | REINVENT | PASS-1 amendment agent | Handoffs | 8 |
| 9 | `restart/audit/pass-1-substrate/PASS-1.md:82` | Split Rust/WASM V1 parity from TS deferred parity; name the TS blocker as Q28 scope deferral and the receiving SYNTHESIS/Tranche-J gate. | REINVENT | PASS-1 amendment agent | Handoffs | 8 |
| 10 | `restart/audit/pass-1-substrate/PASS-1.md:137`-`restart/audit/pass-1-substrate/PASS-1.md:141` | Replace free-floating "Draft `restart/specs/pass-1/...`" items with either inline PASS-1 amendments or explicit SYNTHESIS destinations in `ARCHITECTURE.md` §7/§8. | REINVENT | PASS-1 amendment agent | Punch list | 3, 8 |
| 11 | `restart/audit/pass-1-substrate/PASS-1.md:142` | Rewrite as: "SYNTHESIS must include a conflict table before `ARCHITECTURE.md` §8 resolving stale PASS-2/PASS-3 ParseStream/rewrite-mode/Unicode clauses; blocker: prompts contain stale surfaces." | REINVENT | PASS-1 amendment agent | Punch list | 8 |
| 12 | `restart/audit/pass-1-substrate/PASS-1.md:150` | Delete "PASS-2 and PASS-3 may proceed independently"; replace with "SYNTHESIS must reconcile conflicting sister-pass outputs before any target advances." | DISCARD | PASS-1 amendment agent | Closing posture | 8 |
| 13 | New subsection after `restart/audit/pass-1-substrate/PASS-1.md:119` | Add the future-grammar onboarding test: `yaml.bbnf` plus `[workspace.metadata.bbnf.grammars.yaml]`, zero Rust crate, zero match arm, zero generic-crate edit; include verification grep. | REINVENT | PASS-1 amendment agent | Lock 14 proof | 1, 5 |
| 14 | New tables near `restart/audit/pass-1-substrate/PASS-1.md:34` and `restart/audit/pass-1-substrate/PASS-1.md:82` | Add per-X tables for every "normal grammars", "all grammars", and "Rust/TS/WASM" claim. | REINVENT | PASS-1 amendment agent | Lock 14 proof | 5 |
| 15 | `restart/audit/pass-1-substrate/PASS-1.md:20`; `restart/audit/pass-1-substrate/PASS-1.md:36` | Add rare escape-valve fence: approval owner, failed metadata primitive, declaration location, no generic-crate import, verification command. | REINVENT | PASS-1 amendment agent | Host-fn architecture | 5 |
| 16 | New subsection after `restart/audit/pass-1-substrate/PASS-1.md:32` | Add generated-code budget schema: per grammar baseline LOC, projected LOC, allowed delta, construct pressure source, and `cargo xtask regen --check` wall ceiling. | REINVENT | PASS-1 amendment agent | Cost model | 6 |
| 17 | New subsection after `restart/audit/pass-1-substrate/PASS-1.md:38` | Add verbatim diagnostics for `LookbehindWidth`, `HostSignature`, `LayoutConflict`, chain-step type failure, and Pratt/SIMD non-application. | REINVENT | PASS-1 amendment agent | Friction forecast | 7 |
| 18 | `restart/audit/pass-1-substrate/PASS-1.md:121` | Reframe current closure beta-reduction machinery as research signal only; require fresh greenfield spec and verification before reuse. | REINVENT | PASS-1 amendment agent | Greenfield discipline | 9 |
| 19 | `restart/audit/pass-1-substrate/agent-6-substrate-coherence-auditor.md:17` | Remove the claim that existing OpenFrame builders are useful backend-internal stack detail; replace with generated Backend IR builder-frame design and no OpenFrame preservation. | DISCARD | PASS-1 amendment agent | Sub-agent correction | 9 |

## §13 Final readiness

> **Decision: amendment-required**
>
> PASS-1 has the right load-bearing decisions: tape retained, ParseStream rename discarded, Grammar IR separated from Backend IR, CSP and e-graph bridged, rewrite-mode removed, Unicode algebra delegated to regex, and per-grammar declaration crates rejected as default. The defects are amendment-sized rather than thesis-breaking. The target must still prove Lock 14 with a yaml onboarding test, repair the BBNF formal grammar, add generated-code budgets, define friction diagnostics, and route every carry to a receiver, blocker, and gate.
>
> Hereupon dispatch a narrow PASS-1 amendment agent to apply the punch list before PASS-1 advances into synthesis.
