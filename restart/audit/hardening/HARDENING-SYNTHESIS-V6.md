# HARDENING-SYNTHESIS-V6 - research-fold verification

## §1 Target Identification, History, And Corpus

Target: SYNTHESIS trio.

Output: `restart/audit/hardening/HARDENING-SYNTHESIS-V6.md`.

Write scope used: this report only.

Source surfaces audited and not edited:

| Surface | Lines audited | Role |
|---|---:|---|
| `restart/ARCHITECTURE.md` | 1,477 | Workspace, API, IR, BBNF grammar, runtime, codegen, performance, and YAML onboarding contract. |
| `restart/MIGRATION.md` | 813 | Current-file disposition, migration gates, archive route, and implementation carry routing. |
| `restart/MASTER-PLAN.md` | 844 | A-J tranche sequence, hard gates, A->F->J YAML trajectory, carry/friction ledger, and close rules. |
| `restart/research/fold-synthesis.md` | 357 | Research topic 1-8 fold classification and escalation scan. |

Current workspace HEAD audited: `c5e3aab741ab2354486593e0cfbed97972ab1225`.

Synthesis-trio commit history in scope:

| Commit | Role |
|---|---|
| `91af4882` | V5.1 narrow synthesis amendment: formal grammar, provenance, YAML, WASM ABI, and one-grammar trajectory. |
| `0c72433b` | First research-fold amendment touching the synthesis trio: type, bridge, cost, tape, incremental, and SIMD/regex grounding. |
| `00c51814` | Latest synthesis-trio research-fold amendment: six additional lines over `ARCHITECTURE.md`, `MIGRATION.md`, and `MASTER-PLAN.md`. |
| `5ea41850` | PASS-2 V6 hardening report present in HEAD baseline. |
| `c5e3aab7` | PASS-3 V6 hardening report present in HEAD baseline. |

V5/V5.1 to V6 history:

| Prior gate | Prior result | V6 regression question | V6 result |
|---|---|---|---|
| V5.1 formal grammar | `restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md:49-53` closed block-bodied `@host fn`, infix lookbehind, rule-level chains, and stale grammar syntax. | Did research fold reopen bodyless host declarations, prefix lookbehind, `path!`, `@pratt`, or `@simd`? | No. Current grammar sketch keeps `HostFn ... Block`, `Lookbehind ::= Expr "|<" Expr | Expr "|<!" Expr`, `ChainExpr`, `@error`, and `@layout` at `restart/ARCHITECTURE.md:1079-1110`; `@pratt`/`@simd` appear only as forbidden output at `restart/MASTER-PLAN.md:204`. |
| V5.1 YAML onboarding | `restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md:60-64` closed the two-surface YAML proof and host route. | Did research fold add fixture creep or a declaration-crate default? | No. YAML remains `grammars/yaml.bbnf` plus `[workspace.metadata.bbnf.grammars.yaml]` at `restart/ARCHITECTURE.md:1336-1343`, with fixtures only a later parity gate at `restart/ARCHITECTURE.md:1376`. |
| V5.1 WASM route | `restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md:65-68` closed H.W3 ABI routing without invented numbers. | Did research fold turn WASM host primitives into grammar syntax or fake performance precision? | No. H.W3 still uses `{N}`/`{M}` as measured placeholders with owner and blocker at `restart/MASTER-PLAN.md:479`, and the ABI matrix records exported names, host-call shape, marshalling, primitive coverage, and scalar/SIMD parity at `restart/MASTER-PLAN.md:483-491`. |
| V5.1 A->F->J trajectory | `restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md:69-72` closed one grammar trajectory. | Did research fold split YAML into a special implementation path? | No. The trajectory is still a receiving-gate proof, not a special YAML route, at `restart/MASTER-PLAN.md:208-224`. |
| Research fold classification | `restart/research/fold-synthesis.md:351-357` says AMENDMENT-REQUIRED, FOLD-ONLY, NO ESCALATION before the trio amendments landed. | Did the landed fold create a new synthesis-blocking amendment? | No. Folded evidence is absorbed; remaining residue is README/research-index/bibliography/PASS hygiene outside this report's write scope (`restart/research/fold-synthesis.md:355`). |

Required reading consumed: `restart/prompts/audit-specs/HARDENING-LENS-SET.md`, `restart/README.md` §13, `restart/locks/LOCKS.md`, the three synthesis surfaces end-to-end, `restart/research/fold-synthesis.md`, topic docs 1-8 as needed for research pressure, PASS-2/PASS-3 V6 calibration reports, and `HARDENING-SYNTHESIS-V5.1.md`.

## §2 Cohort Verdict

Final decision: **READY**.

The research fold introduced no synthesis-blocking amendment. It sharpened type/layout boundaries, bridge/cost evidence, tape identity, incremental recovery, regex/SIMD verifier routing, benchmark metadata, and carry rows without reopening retired syntax, grammar-specific crates, weak-source overclaims, or invented performance numbers.

Nine-lane table:

| Lane | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|
| 1 Lock-Adherence | READY | 6 | 0 | 0 | Advance; no lock amendment required. |
| 2 Sequencing Discipline | READY | 6 | 0 | 0 | Advance to tranche drafting; A-J consumers are named at stub level. |
| 3 Cohesion | READY | 6 | 0 | 0 | Advance; folded claims have local receivers and evidence. |
| 4 SOTA Anchoring | READY | 5 | 0 | 0 | Advance; H/J rows must measure, not invent, remaining values. |
| 5 Grammar-Authoritative Discipline | READY | 6 | 0 | 0 | Advance; YAML remains two-surface and generated-only thereafter. |
| 6 Generated-Code And LOC Budget | READY | 5 | 0 | 0 | Advance; budget rows are receiver-gated. |
| 7 Friction Forecast | READY | 5 | 0 | 0 | Advance; cookbook/diagnostic receivers are explicit. |
| 8 Carry And Deferral | READY | 6 | 0 | 0 | Advance; no dangling synthesis carry. |
| 9 Greenfield Discipline | READY | 5 | 0 | 0 | Advance; no workaround or overfit substrate reappears. |

Research-fold binding ledger:

| Topic | Fold pressure | Current synthesis evidence | V6 verdict |
|---|---|---|---|
| 1 HM foundations | HM is a principal-scheme core; "CSP-backed unification" overclaims. | `restart/ARCHITECTURE.md:1151-1159` splits HM equality, expected checking, bounded coercion, finite CSP, host chains, lookbehind, and layout/error facts; `restart/MASTER-PLAN.md:313` keeps `TypeFacts` and `TypeObligationLog` internal. | KEEP. |
| 2 Bidirectional | Pierce-Turner check/synth is local; DK higher-rank evidence is a future proof gate, not V1 proof. | `restart/ARCHITECTURE.md:1161-1166` closes higher-rank/existential/indexed/GADT-like constructs unless a later proof gate lands; `restart/research/fold-synthesis.md:241` and `restart/MIGRATION.md:500-507` avoid overclaiming Roc or unverified bibliography leads. | KEEP. |
| 3 CSP/GADTs/generic rules | Plain `Object<V>` is rank-1 parametric; local equalities need explicit future machinery; monomorphization needs finiteness. | `restart/ARCHITECTURE.md:1155`, `restart/ARCHITECTURE.md:1161-1171`, and `restart/MASTER-PLAN.md:347` bind finite `(RuleId, TypeArgs)` evidence and generic-cycle diagnostics. | KEEP. |
| 4 E-graphs | Bridge facts need stable IDs, monotone exchange, representative-stability, and proof payloads; egglog is a counterargument, not a lock contradiction. | `restart/ARCHITECTURE.md:32-36` records fusion as post-V1 research; `restart/ARCHITECTURE.md:1008` exposes `BridgeJustification`; `restart/MASTER-PLAN.md:316-317` and `restart/MASTER-PLAN.md:702` add stable maps, rewrite budget, and justification gates. | KEEP. |
| 5 Cost models | Scalar-only `Cost` is too weak; objective vector, profile, Pareto/frontier, rejected/dominated alternatives, and extraction method must survive. | `restart/ARCHITECTURE.md:1010`, `restart/MASTER-PLAN.md:317`, `restart/MASTER-PLAN.md:776`, and `restart/MIGRATION.md:592` bind `CostDecision`/`CostFacts` to objective and provenance evidence. | KEEP. |
| 6 Tape/direct union | Union must mean one authoritative identity with typed projections, payload policy, and benchmark metadata. | `restart/ARCHITECTURE.md:1211-1245` defines tape/direct as one substrate family; `restart/ARCHITECTURE.md:1237` requires `(TapeId, node id, payload class)` identity; `restart/MASTER-PLAN.md:778` carries materialisation metadata. | KEEP. |
| 7 Green/red incremental | Incremental identity must be snapshot-scoped with reuse maps, invalidation keys, fallback reasons, and typed recovery. | `restart/MASTER-PLAN.md:523` binds snapshot `TapeId`, reuse maps, query invalidation, and fallback ledger; `restart/MASTER-PLAN.md:806` adds YAML syntax-error friction with `DocumentSnapshot` and `ReparsePlan`. | KEEP. |
| 8 SIMD/DFA/regex | Exact scans need scalar parity; prefilters need verifier-before-tape; `regex-automata` is oracle/reference until bespoke parity is proven. | `restart/ARCHITECTURE.md:935-936`, `restart/ARCHITECTURE.md:964`, `restart/MASTER-PLAN.md:477`, `restart/MASTER-PLAN.md:777`, `restart/MIGRATION.md:494`, and `restart/MIGRATION.md:594` bind `RegexProgram`, `SimdScanMode::{Exact, Prefilter}`, oracle parity, and grammar-owned delta. | KEEP. |

Cross-target binding ledger:

| Binding | Producer/receiver | Current evidence | V6 result |
|---|---|---|---|
| PASS-1 to synthesis formal surface | PASS-1 owns Grammar IR/type/BBNF producer detail; synthesis owns consolidated contract. | PASS-1 records Grammar IR, lookbehind, host/layout/error, type split, generics, declaration-crate fence, and formal grammar at `restart/audit/pass-1-substrate/PASS-1.md:24-41`, `:73-83`, `:97`, `:192-231`; synthesis consumes at `restart/ARCHITECTURE.md:827-889` and `restart/ARCHITECTURE.md:1067-1136`. | Bound. |
| PASS-2 to synthesis BIR/lowerer/runtime generation | PASS-2 owns lowerer/runtime-template producer detail; synthesis owns architecture and schedule. | PASS-2 binds `LayoutFacts`/`LayoutSink` at `restart/audit/pass-2-codegen/PASS-2.md:69`, WASM ABI as lowerer/runtime concern at `restart/audit/pass-2-codegen/PASS-2.md:112`, YAML two-surface codegen at `restart/audit/pass-2-codegen/PASS-2.md:389-395`, and SOTA rows at `restart/audit/pass-2-codegen/PASS-2.md:454-471`; PASS-2 V6 returns no PASS-2 blocker at `restart/audit/hardening/HARDENING-PASS-2-V6.md:67-68`. | Bound. |
| PASS-3 to synthesis runtime/API/LSP | PASS-3 owns user/runtime receivers; synthesis owns A-J placement. | PASS-3 V6 verdict is READY at `restart/audit/hardening/HARDENING-PASS-3-V6.md:11`; it closes tape identity, YAML recovery, pointer/select, benchmark metadata, and scanner verifier routing at `restart/audit/hardening/HARDENING-PASS-3-V6.md:73-75`, `:120-155`, and `:190-198`. | Bound. |
| Migration to master carry truth | Migration routes implementation residue to one master ledger. | `restart/MIGRATION.md:790-801` points migration receivers to `MASTER-PLAN.md` §24; `restart/MASTER-PLAN.md:762-792` carries receiver, blocker, and gate rows. | Bound. |

## §3 Lane 1 - Lock-Adherence

Lane standard: the synthesis trio must honor the 14 locks without moving lock text, inventing exemptions, or treating research pressure as license to reopen settled decisions.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/ARCHITECTURE.md:21-23`, `restart/MASTER-PLAN.md:23-28` | Lock 1 tape/direct substrate | Tape is the substrate and direct-to-struct is a projection over the same event stream; `ParseStream` is rejected as a runtime rename. | The trio names tape, rejects OpenFrame ladders, and makes direct builders share spans/source slices/payload slots at `restart/ARCHITECTURE.md:1241-1245`. | `ParseStream` still appears in migration archaeology and grep gates. | Re-draft if any `ParseStream` hit exists. | KEEP. The hits are conflict/deletion/proc-macro exception contexts, not a runtime substrate. |
| `restart/locks/LOCKS.md:36`, `restart/ARCHITECTURE.md:995-1013`, `restart/MIGRATION.md:237`, `restart/MASTER-PLAN.md:313` | Lock 2 layout vocabulary | `passes::layout` is public pass vocabulary; `LayoutFacts` is public; `LayoutSink` remains the lock-level consuming trait; `TypeFacts` is internal only. | Synthesis surfaces do not introduce `TypeMap`, `StructLayout`, `TypeDesc`, or public `TypeFacts`; PASS-2 names `LayoutSink` at `restart/audit/pass-2-codegen/PASS-2.md:69`. | Architecture does not repeat `LayoutSink` in its side-table matrix. | Add a synthesis punch item demanding a `LayoutSink` row before readiness. | KEEP. No competing sink or retired term appears in the trio; PASS-2 owns the BIR consumer trait and synthesis preserves the producer/side-table contract. |
| `restart/ARCHITECTURE.md:891-950`, `restart/MASTER-PLAN.md:380-384` | Lock 5 Backend IR lowerer boundary | Lowerers consume Backend IR, not Grammar IR. | `restart/ARCHITECTURE.md:984` requires import-deny and snapshots; `restart/MASTER-PLAN.md:201` forbids lowerers importing Grammar IR as emitter input. | Current implementation still has old walkers as migration targets. | Allow transition lowerers to walk Grammar IR until E/F. | KEEP. Migration classifies old walkers as replacement targets at `restart/MIGRATION.md:320-332`; no synthesis text permits them. |
| `restart/ARCHITECTURE.md:1282-1320`, `restart/MASTER-PLAN.md:125-150` | Lock 8 SOTA | Throughput gates must name competitor, dataset, target, and platform. | Exact rows name sonic-rs, simd-json, lightning-css, simdjson On-Demand, M1 Pro, M-series NEON, and x86 AVX2. | H.W3 WASM still has `{N}` and `{M}` placeholders. | Treat any `TBD` as a blocking Lock 8 failure. | KEEP. The only `TBD` is a measured H.W3 placeholder with owner/blocker at `restart/MASTER-PLAN.md:479`, not an asserted performance fact. |
| `restart/ARCHITECTURE.md:725-776`, `restart/MASTER-PLAN.md:771` | Lock 14 declaration-crate fence | Declaration crates are rare escape valves, not default per-grammar crates. | The fence requires reason, owner, metadata failure proof, `@host fn` failure proof, location, no-generic-import proof, deletion path, reviewer, and receiving gate. | The architecture keeps documentation slots even when `allow = false`. | Delete declaration-crate schema entirely to avoid tempting future special cases. | KEEP. The default writes nothing to runtime at `restart/ARCHITECTURE.md:772-776`; the fence makes the exception harder, not easier. |
| `restart/ARCHITECTURE.md:1331-1376`, `restart/MASTER-PLAN.md:208-224` | Lock 14 future grammar proof | YAML must enter by source plus metadata and no generic crate edit. | Both Architecture and Master enforce two author surfaces, generated derivative output, no fixture authority, and no declaration-crate onboarding. | YAML rows are concrete and can look like a special case. | Remove YAML and rely on abstract prose. | KEEP. Lock 14 requires an executable future-grammar probe; YAML is proof evidence, not plan logic. |

Lane verdict: READY. KEEP 6, REINVENT 0, DISCARD 0.

## §4 Lane 2 - Sequencing Discipline

Lane standard: this multi-tranche plan must avoid substrate-first/consumer-later failure. Every producer in A-J must have a same-wave or next-layer consumer or a named receiving gate.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/MASTER-PLAN.md:160-171` | A-J close gates | Each tranche has a primary close gate. | A closes metadata and crate graph; B closes tape/direct; C closes facts; E closes BIR/VM; F closes generated runtime; J closes parity/SOTA/docs. | Stub waves are not full specs. | Require full wave docs before synthesis readiness. | KEEP. The synthesis phase is explicitly stub-level at `restart/MASTER-PLAN.md:173`; full drafting is next phase. |
| `restart/MASTER-PLAN.md:180-191` | Carry matrix | Every tranche carries from earlier authority and to later consumers. | B carries to F/G/H/I; C carries to D/E/F/H/I; H carries to J; I carries to J. | Some carry cells name several recipients. | Demand one row per wave consumer now. | KEEP. Stub-level multiplicity is acceptable because §24 supplies concrete receiver/blocker/gate rows. |
| `restart/MASTER-PLAN.md:208-224` | YAML A->F->J trajectory | One grammar proves handoffs across all layers. | YAML admission, BIR, runtime generation, path schema, WASM ABI, recovery/LSP, docs, and publication are each gated. | It spans every tranche, not just A/F/J. | Compress to A, F, J only. | KEEP. The added B/C/D/E/G/H/I rows prevent hidden substrate gaps. |
| `restart/MASTER-PLAN.md:312-317` | C tranche consumers | Type facts, shape facts, recognizers, bridge facts, and cost facts feed direct builder and BIR extraction. | C.W1-C.W5 each names the downstream evidence consumer. | Full APIs remain future work. | Merge C into E/F to avoid abstract facts. | KEEP. C has direct B/E/F consumers and is not a free-floating substrate tranche. |
| `restart/MASTER-PLAN.md:407-416` | F tranche generated runtime | Rust lowerer emits runtime only after BIR and runtime substrate exist. | F consumes B/D/E and closes equality/LOC/nine-grammar generation. | Generated output can dominate code churn. | Delay all generation to J. | KEEP. F must produce generated runtime before G path/visitor and H performance consumers can close. |
| `restart/MASTER-PLAN.md:472-481` | H tranche recognizers/WASM | H consumes C recognizers, E BIR, and F runtime template. | Pratt, SIMD, WASM, JSON, and CSS gates are consumers of prior facts rather than new syntax. | H.W3 retains measured placeholders. | Move WASM out of V1. | KEEP. H.W3 is ABI/measurement-gated and does not block synthesis readiness. |

Lane verdict: READY. KEEP 6, REINVENT 0, DISCARD 0.

## §5 Lane 3 - Cohesion

Lane standard: every synthesis claim must be verifiable from local artefacts, local citations, or named generated outputs. Research fold evidence must not stand on weak or unverified source leads.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/research/fold-synthesis.md:13-24`, `restart/ARCHITECTURE.md:1137-1171` | Topics 1-3 type fold | The fold decomposes type work into HM, check/synth, bounded coercion, finite CSP, and later proof gates. | Architecture mirrors the decomposition and avoids "full HM with subsumption". | README still has compressed type prose outside this report's target. | Mark synthesis amendment-required until README is rewritten. | KEEP. The trio under audit carries the precise contract; README hygiene is routed by `restart/research/fold-synthesis.md:355`. |
| `restart/research/fold-synthesis.md:124-146`, `restart/ARCHITECTURE.md:32-36`, `restart/ARCHITECTURE.md:1008` | Topic 4 bridge fold | The egraph/CSP bridge uses stable proof refs and rejects fusion as V1 default. | Architecture records the egglog alternative and keeps fusion post-V1; Master adds representative stability and justification gates. | Lock file itself lacks the egglog counterargument. | Escalate to lock amendment. | KEEP. `restart/research/fold-synthesis.md:281-283` says no structural lock contradiction; rationale fold is enough. |
| `restart/research/fold-synthesis.md:148-165`, `restart/ARCHITECTURE.md:1010`, `restart/MASTER-PLAN.md:776` | Topic 5 cost fold | Cost evidence survives as records, not scalar folklore. | Selected, rejected, dominated, objective mode, target, profile, and extraction method are in the carry gate. | The exact API will be implementation work. | Demand an implementation API before synthesis. | KEEP. This is a plan gate; the receiver and blocker are sufficient for readiness. |
| `restart/MIGRATION.md:500-507`, `restart/research/fold-synthesis.md:259-264`, `restart/research/fold-synthesis.md:270` | Weak-source hygiene | Unverified Hubbard, Almomany, Deb, Ungar/Adams, HelpMate, and Roc-bidirectional claims are not used as migration evidence. | Migration cites local corpora, PASS contracts, and `regex-automata` oracle lane instead. | Research index still contains stale leads. | Treat stale index leads as synthesis-blocking. | KEEP. The trio avoids overclaiming weak sources; index cleanup is outside write scope. |
| `restart/ARCHITECTURE.md:1067-1136`, `restart/audit/pass-1-substrate/PASS-1.md:192-231` | Formal grammar reconciliation | Architecture copies PASS-1's settled BBNF grammar surface. | Block-bodied `@host fn`, infix lookbehind, generics, rule-level chains, `@error(recover)`, and `@layout` are reconciled; rewrite-mode and grammar Unicode algebra are deletion rows. | Architecture uses examples, not a parser implementation. | Require grammar parser code now. | KEEP. Synthesis defines the contract; implementation tranches execute it. |
| `restart/MASTER-PLAN.md:762-807`, `restart/MIGRATION.md:790-801` | Single carry truth | Migration does not duplicate carry truth; it points to Master §24. | Receiver, blocker, and gate rows cover declaration crates, layout, SOTA, regex, runtime materialisation, YAML, WASM, BIR snapshots, and friction. | A large ledger can hide stale rows. | Split by document to reduce size. | KEEP. Single ledger avoids contradictory carry truth. |

Lane verdict: READY. KEEP 6, REINVENT 0, DISCARD 0.

## §6 Lane 4 - SOTA Anchoring

Lane standard: throughput gates must name competitor, dataset, platform, and target. Non-throughput engineering gates must not present themselves as Lock 8 wins.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/ARCHITECTURE.md:1282-1320` | Exact gate rows | JSON, CSS, and SIMD gates bind competitor baselines and restart targets. | Rows name `json/twitter`, `json/citm`, `json/canada`, `css/bootstrap`, `css/animate`, `simd/structural_scan`, competitors, M1 Pro, M-series, and x86 AVX2. | Some numbers come from corpus anchors, not fresh reruns. | Require fresh competitor runs before plan readiness. | KEEP. H/J tranches own measurement; synthesis owns the evidence floor. |
| `restart/MASTER-PLAN.md:129-136` | Master SOTA rows | Master repeats competitor, target, platform, and owner rows. | H.W4/H.W5/J.W1 owners are named. | H rows are early targets, not final close. | Collapse early and final rows into one J gate. | KEEP. Early gates are consumer proof; J retains final SOTA closure. |
| `restart/MASTER-PLAN.md:138-150` | Benchmark reproducibility | Rows serialize CPU, OS, compiler flags, input hash, competitor version, bbnf commit, warmup, and sample policy. | Prevents invented or incomparable numbers. | The exact schema file is future work. | Demand schema before synthesis ready. | KEEP. Field floor is enough for tranche drafting. |
| `restart/MASTER-PLAN.md:479` | WASM H.W3 placeholder | H.W3 uses `{N}` and `{M}` rather than inventing latency numbers. | Owner, blocker, fixture, browser/runtime metadata, competitor baseline, and source hash are required. | The word `TBD` remains. | Fail every `TBD`. | KEEP. This is the one acceptable measured-value placeholder: routed, blocked, and gated. |
| `restart/MASTER-PLAN.md:775`, `restart/MASTER-PLAN.md:788` | Carry rows for SOTA/WASM | Missing benchmark metadata and WASM ABI are receiver-gated. | H/J and H.W3/J.W3 carry blockers and gates. | Carry rows defer real proof. | Pull all proof into synthesis. | KEEP. Synthesis cannot run future benches; it correctly binds receivers. |

Lane verdict: READY. KEEP 5, REINVENT 0, DISCARD 0.

## §7 Lane 5 - Grammar-Authoritative Discipline

Lane standard: generic crates cannot hardcode grammar logic. Future grammar onboarding is source plus metadata; generated output is derivative, fixtures are parity evidence, and declaration crates remain fenced exceptions.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/ARCHITECTURE.md:606-618`, `restart/ARCHITECTURE.md:725-734` | Two author surfaces | Adding a grammar means one source file plus one metadata block. | Metadata may name files/profiles/features, not Rust parser types or generated modules. | Metadata is still Cargo-root state. | Treat metadata as a third code surface. | KEEP. Lock 14 allows workspace metadata; no Rust source changes are admitted. |
| `restart/ARCHITECTURE.md:739-776` | Declaration-crate fence | Rare declaration crates require a full review form. | Required fields include mechanism reason, owner, metadata failure proof, `@host fn` failure proof, location, no-generic-import proof, deletion path, reviewer, and receiving gate. | The old `allow_declaration_crate` keys remain in schema examples. | Remove the escape valve. | KEEP. The default is false and partial fences are rejected. |
| `restart/ARCHITECTURE.md:1331-1376` | YAML onboarding | YAML proves Lock 14 through source plus metadata and generated outputs. | Runtime, path schema, diagnostics, host route, and bench manifest are generated derivatives; no fixtures during onboarding. | The generated `host.rs` path appears. | Treat generated host output as hand-authored. | KEEP. The row says "No handwritten Rust input" at `restart/ARCHITECTURE.md:1375`. |
| `restart/ARCHITECTURE.md:1378-1417` | Per-grammar authority matrix | The matrix proves all seed and YAML probe routes without match arms. | Host route, path schema, fixture manifest, generated LOC, and declaration status are explicit for each grammar. | Concrete rows can invite overfitting. | Replace rows with "all grammars". | KEEP. Hardening requires per-X proof for all-grammar claims. |
| `restart/MASTER-PLAN.md:215-224` | YAML trajectory | YAML travels through A-J as a proof grammar. | F emits runtime; G consumes path schema; H conditionally evaluates WASM; I consumes diagnostics; J records proof. | A single named grammar can become a mascot. | Use an abstract future grammar instead. | KEEP. A named probe is the executable Lock 14 test. |
| Negative grep gate | No generic match arms | `rg -nP` for `match ... Json =>`, `CssL4 =>`, `Bbnf... =>`, and `GoogleSheets... =>` over the trio returned zero. | The docs still name grammar rows as evidence. | Fail on any grammar name. | KEEP. Evidence rows are allowed; generic match logic is absent. |

Lane verdict: READY. KEEP 6, REINVENT 0, DISCARD 0.

## §8 Lane 6 - Generated-Code And LOC Budget

Lane standard: generated output must be committed, equality-checked, budgeted by grammar/target, and separated from handwritten LOC discipline.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/ARCHITECTURE.md:1302-1329` | Performance and LOC metadata | Generated LOC budgets are tied to PASS-2 baseline plus 2 percent. | Nine seed total, per-grammar runtime, YAML probe, and WASM/SIMD target-specific output are separated. | Some budgets are inherited from PASS-2 section references. | Inline every PASS-2 budget row only here. | KEEP. Architecture and Master mirror the firm per-grammar rows. |
| `restart/ARCHITECTURE.md:1389-1400` | Per-grammar LOC rows | Each grammar has current-to-max generated LOC. | `css_l4`, `google_sheets`, `bbnf`, and YAML ceilings are explicit; YAML remains provisional and outside seed budget. | The table is large. | Remove table and reference PASS-2. | KEEP. V5.1 required a complete local matrix. |
| `restart/MASTER-PLAN.md:647-684` | LOC trajectory | Generated growth is staged from F through J. | Wall budgets and target-attributed WASM/SIMD/CSS/JSON growth are named. | Some walls are future estimates. | Delete wall ceilings until measured. | KEEP. Provisional ceilings are receiver-gated and prevent silent churn. |
| `restart/MASTER-PLAN.md:686-693` | Budget enforcement rows | F, H, J, and handwritten support have different gates. | Separates generated LOC budget from Lock 13 handwritten tree/LOC discipline. | Implementation may need adjusted ceilings. | Treat any future adjustment as free. | KEEP. Adjustments require amendment; synthesis does not grant free drift. |
| `restart/MIGRATION.md:526-550`, `restart/MIGRATION.md:755-763` | Generated migration/equality | Generated files are committed, hash-headed, equality-checked, and budgeted. | `cargo xtask bbnf build --all`, `git diff --exit-code`, and generated budget gate are named. | Commands are not runnable before greenfield crates exist. | Remove future commands. | KEEP. These are migration gates, not current CI claims. |

Lane verdict: READY. KEEP 5, REINVENT 0, DISCARD 0.

## §9 Lane 7 - Friction Forecast

Lane standard: likely author/user confusion must have a cookbook, diagnostic, or gate receiver. The report rejects vague friction and accepts only named artefacts.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/MASTER-PLAN.md:797-805` | Path and YAML onboarding friction | Library consumers and grammar authors need explicit path schema and registration rules. | `pointer!`/`select!` have grammar-qualified examples; YAML asks "Where do I register yaml in Rust?" and receives metadata-only guidance. | Cookbook files are future docs. | Require cookbook files now. | KEEP. J.W2 owns docs; G/I/J carry the receiver. |
| `restart/MASTER-PLAN.md:799-803` | Runtime/lifetime/layout/recognizer friction | Parse lifetime, visitor mutation, layout, and Pratt/SIMD decisions have named diagnostics. | `BBNF-LIFETIME-ESCAPE`, `BBNF-ARENA-MISMATCH`, `BBNF-LAYOUT-CONFLICT`, `BBNF-PRATT-NOT-APPLIED`, and `BBNF-SIMD-NOT-SELECTED` are tied to artefacts. | Diagnostic strings can drift in implementation. | Block until all strings are implemented. | KEEP. Diagnostic catalogue is bound at `restart/ARCHITECTURE.md:1027-1059`; implementation gates verify. |
| `restart/MASTER-PLAN.md:806` | YAML syntax-error recovery | LSP fault tolerance must not become silent recovery folklore. | The row names `DocumentSnapshot`, `TapeId` reuse maps, recovery facts, `BBNF-RECOVERY001`, and fallback reason. | It is one example. | Add full recovery cookbook before synthesis. | KEEP. The example is enough to preserve the receiver. |
| `restart/ARCHITECTURE.md:1027-1059` | Diagnostic vocabulary | Error codes include lookbehind, layout, pointer, host, generic-cycle, recovery, and generated-output gates. | `BBNF-LOOKBEHIND-WIDTH` and `BBNF1004` are reconciled; `BBNF-HOST003` routes missing WASM primitives. | Verbatim strings live in PASS producers. | Duplicate all strings here. | KEEP. Architecture binds identifiers and producers; producer docs own exact strings. |
| `restart/MASTER-PLAN.md:483-491` | WASM host primitive route | WASM host primitives are ABI/lowerer/runtime concerns. | Export names, host-call shape, marshalling, primitive coverage, and parity must exist before latency/size acceptance. | No worked host primitive example appears here. | Add grammar syntax for WASM primitives. | KEEP. That would violate the route; H.W3 ABI matrix is the correct receiver. |

Lane verdict: READY. KEEP 5, REINVENT 0, DISCARD 0.

## §10 Lane 8 - Carry And Deferral Audit

Lane standard: every deferral must name receiver, blocker, and gate. Vague "future" or unowned "TBD" is fault.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/MASTER-PLAN.md:762-792` | Main carry ledger | One ledger carries synthesis and migration rows. | Each row has Item, Receiver, Blocker, Gate, and Source. | Receivers sometimes name multiple tranches. | Require one wave-level receiver per row now. | KEEP. Stub-level synthesis can name tranche sets; detailed wave docs refine. |
| `restart/MASTER-PLAN.md:771-779` | High-risk research rows | Declaration crates, layout, cursor skip, SOTA metadata, cost evidence, regex oracle, materialisation, and YAML are gate-backed. | This directly absorbs research topics 4-8. | Some gates are future xtask/test names. | Treat future commands as speculative. | KEEP. The plan must state gates before implementation creates them. |
| `restart/MASTER-PLAN.md:788-792` | WASM/generated/BIR/path-ts rows | WASM ABI, generated headers, path-ts timing, and BIR snapshots are receiver-gated. | H/J, F, J, and E/F receivers are explicit. | path-ts and publication can slip. | Publish all at once regardless of stability. | KEEP. Slip conditions are recorded and safer than forced publication. |
| `restart/MIGRATION.md:790-801` | Migration punch list consolidation | Migration no longer carries a second table. | Avoids duplicate receiver truth. | Readers must jump to Master §24. | Keep duplicate migration rows for convenience. | KEEP. Duplicate rows rot; one ledger is cleaner. |
| `restart/research/fold-synthesis.md:277-285`, `restart/research/fold-synthesis.md:349-357` | Escalation scan | No lock contradiction escalates; deferred items are out of scope. | README, lock-file, PASS, bibliography, research-index, and future implementation receivers are named. | Some deferred items remain real cleanup. | Block synthesis until all adjacent docs are polished. | KEEP. They are not synthesis-trio blockers. |
| `restart/MASTER-PLAN.md:479` | Remaining `TBD` | `{N}` and `{M}` stay unknown by design. | Owner, blocker, baseline, fixture, browser/runtime, version, commit, and hash metadata are listed. | The literal `TBD` triggers alarm. | Fail the plan on any `TBD`. | KEEP. This is the only routed measurement placeholder found. |

Lane verdict: READY. KEEP 6, REINVENT 0, DISCARD 0.

## §11 Lane 9 - Greenfield Discipline

Lane standard: no quick solutions, no workarounds, no default declaration crates, no retired syntax, no hidden legacy substrate, and no grammar overfitting.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/ARCHITECTURE.md:10-30` | Conflict ledger | The architecture opens by naming superseded material and resolutions. | Tape stays tape; `ParseStream`, columnar SoA, rewrite-mode, BBNF Unicode algebra, default declaration crates, grammar switches, and direct Grammar IR emitters are rejected. | It names legacy terms repeatedly. | Remove old terms to avoid repetition. | KEEP. Conflict ledgers make archaeology non-authoritative. |
| `restart/MIGRATION.md:42-60`, `restart/MIGRATION.md:299-375` | Replace, not patch | Migration classifies old code by fate and replaces backend/runtime/host leaks. | `core` backend walkers, OpenFrame fallback, grammar host shims, and serialization are replacement/delete targets. | Large replacement plan carries risk. | Patch current code in place. | KEEP. Patching would preserve the architecture failures being retired. |
| `restart/MASTER-PLAN.md:195-206` | Forbidden outputs | Each tranche says what must not land. | ParseStream runtime rename, third optimized IR, BBNF Unicode algebra, Grammar IR lowerers, proc-macro codegen facade, path registries, `@pratt`/`@simd`, LSP-only semantics, and new J architecture decisions are barred. | Forbidden rows are terse. | Move all forbidden detail into prose. | KEEP. Table form is surgical and auditable. |
| `restart/ARCHITECTURE.md:1131-1136`, `restart/MASTER-PLAN.md:338-350` | Retired syntax discipline | Rewrite-mode, grammar Unicode algebra, regex-style lookbehind, standalone `@recover`, and default declaration crates stay out. | D.W4 proves rewrite rejection and regex Unicode routing. | README still mentions rich regex and chaining in older compressed language. | Reopen BBNF grammar to include rewrite/Unicode algebra. | KEEP. The trio's normative BBNF grammar rejects those surfaces. |
| `restart/MASTER-PLAN.md:826-844` | Close posture | Implementation begins from synthesis outputs, not by editing prompts/locks/PASS outputs. | Master close lists architecture, migration, A-J stubs, and settled authority as readiness criteria. | This relies on future tranche discipline. | Allow implementation tranches to rewrite inputs opportunistically. | KEEP. That would violate the greenfield contract; current close posture is disciplined. |

Lane verdict: READY. KEEP 5, REINVENT 0, DISCARD 0.

## §12 Gate Rerun, Punch List, And Residue

Minimum verification commands were run before commit. Results:

| Command | Result |
|---|---|
| Broad synthesis token scan over README, ARCHITECTURE, MIGRATION, MASTER-PLAN, and fold-synthesis | Nonzero by design. Hits classified as current surfaces (`@host fn`, `pointer!`, `select!`, `LayoutFacts`, `passes::layout`, YAML, WASM), deletion/prohibition contexts (`@pratt`, `@simd`, rewrite-mode, grammar Unicode algebra, runtime `ParseStream`, `OpenFrame`), or routed measurements (`TBD` only at `restart/MASTER-PLAN.md:479`). No unclassified stale-positive hit. |
| Amendment/readiness/carry scan over V5.1, fold-synthesis, ARCHITECTURE, MIGRATION, MASTER-PLAN | Nonzero by design. V5.1 says READY at `restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md:202`; fold-synthesis says AMENDMENT-REQUIRED before fold and no escalation at `restart/research/fold-synthesis.md:351-355`; current carry rows route receiver/blocker/gate truth through Master §24. |
| Corrected generic match-arm scan over the trio | Zero matches for `match ... Json =>`, `CssL4 =>`, `Bbnf... =>`, or `GoogleSheets... =>`. |
| Retired syntax scan over the trio | Only `@pratt`/`@simd` forbidden-output hit at `restart/MASTER-PLAN.md:204` and `@recover` deletion-row hit at `restart/ARCHITECTURE.md:1134`; no `path!`, stale `HostDecl`, `=> TypeExpr`, `MapExpr`, prefix lookbehind production, full-HM-with-subsumption, CSP-backed-unification, SIMD-first, or mandatory DFA-codegen hit. |
| `git diff --check` | Clean after report creation. |
| `git status --short` | Only this report changed before staging. |

Punch list before SYNTHESIS advances: none.

Routed residue:

| Residue | Receiver | Blocker | Gate | Blocking? |
|---|---|---|---|---|
| README/research-index bibliography and phrasing hygiene named by research topics. | Future README/research-index cleanup, outside this report's write scope. | User explicitly denied edits outside `HARDENING-SYNTHESIS-V6.md`; fold-synthesis routes these out at `restart/research/fold-synthesis.md:355`. | A future cleanup should cite verified primary/local evidence or mark leads as bibliography gaps. | No. |
| H.W3 WASM latency `{N}` / `{M}` values. | H.W3 and J.W3. | Lightning-css WASM comparison and host/browser measurement not yet run. | H.W3 records baseline, runtime, fixture hash, ABI matrix, parity, and measured values before acceptance (`restart/MASTER-PLAN.md:479`, `:483-491`). | No. |
| Full per-wave tranche specs. | Next drafting phase. | Phase 2 synthesis intentionally keeps A-J as stubs. | Detailed tranche drafting after hardening READY. | No. |

## §13 Final Readiness

**Decision: READY**

The research fold did not introduce a synthesis-blocking amendment. Topics 1-8 are folded into the trio with local evidence, and weak/unverified source leads are treated as bibliography residue rather than architecture evidence. Formal BBNF syntax, layout vocabulary, rare declaration-crate fencing, two-surface YAML onboarding, WASM ABI routing, benchmark metadata, A->F->J trajectory, and receiver/blocker/gate discipline remain executable.

Hereupon the next step is per-tranche full-spec drafting from the current synthesis trio.
