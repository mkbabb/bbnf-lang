# CORPUS-AUDIT-2 — `restart/audit/pass-{1,2,3}*/` directories

Corpus-audit agent #2 — pass-1-substrate / pass-2-codegen / pass-3-runtime, against HARDENING-CONSOLIDATED-V8 simplification cohort verdicts.

## §1 Audit scope

Three pass directories carry 21 files total:

| Directory | Files | Roles |
|---|---:|---|
| `restart/audit/pass-1-substrate/` | 7 | `PASS-1.md` synthesis (324 lines, last touched `c06d10c1` Phase-7.5b) + 6 sub-agent reports (May 4, sealed Wave-1 evidence) |
| `restart/audit/pass-2-codegen/` | 7 | `PASS-2.md` synthesis (612 lines, last touched `3dc95460` Phase-7.2 fold) + 6 sub-agent reports (May 4, sealed) |
| `restart/audit/pass-3-runtime/` | 7 | `PASS-3.md` synthesis (587 lines, last touched `d9414a2f` Phase-7.2 fold) + 6 sub-agent reports (May 4, sealed) |

V8 cohort verdict applied (`restart/audit/hardening/HARDENING-CONSOLIDATED-V8.md:7-12`):

- PASS-1 = SIMPLIFY-AVAILABLE (139 lines V8 report)
- PASS-2 = READY (5 non-blocking) (400 lines V8 report)
- PASS-3 = AMENDMENT-REQUIRED (additive trim) (173 lines V8 report)

Audit lens: V2-deferral occurrences must fold V1 or delete; classify ASPIRATIONAL/SPECULATIVE V8 candidates as must-fold-V1 / must-delete / acceptable-with-rewording. Sub-agent reports are sealed Wave-1 evidence.

## §2 PASS-1/2/3 syntheses disposition

### §2.1 PASS-1 (`restart/audit/pass-1-substrate/PASS-1.md`)

**Authoritative role.** Defines the substrate contract: Grammar IR (15 variants), Backend IR (22 variants), type-system algorithm (HM + DK13 + first-order unification + finite CSP), CSP/e-graph composition, cost-model API, BBNF formal grammar (6-directive surface + lookbehind + match/tuple + lambda), and per-crate trees for `error`/`source`/`grammar`/`ir`/`passes`/`vm`/`host`/`cost-model`/`egraph`/`csp-solver`/`parse-that`.

**Post-Phase-7.2 fold completeness.**

| Item | Folded? | Citation |
|---|---|---|
| DK13 algorithmic completeness | YES | `PASS-1.md:73` |
| GADT substrate (CSP `Implication { givens, wanted }`) | YES (substrate) / V2 amendment (surface) | `PASS-1.md:81` |
| Closure capture by `&'i Tape<'i>` | YES | `PASS-1.md:75` (BBNF surface), `PASS-1.md:249` (formal grammar) |
| Match/tuple typing | YES | `PASS-1.md:77`, `PASS-1.md:251` |
| `path!` rename | n/a (PASS-3 scope) | — |
| Backend trait integration | n/a (PASS-2 scope) | — |
| `RegexProgram` rename | n/a (PASS-2 scope) | — |
| `format()` public method | n/a (PASS-3 scope) | — |
| `parse-that-regex` naming | YES (`parse-that` regex sub-crate) | `PASS-1.md:147` |
| Closure runtime / `path-ts` defer | n/a (PASS-3 scope) | — |
| Schema-mining miner | YES | `PASS-1.md:79` |
| Higher-rank explicit-annotation rule | YES | `PASS-1.md:73` |
| Function values + `FnType` | YES | `PASS-1.md:75`, `PASS-1.md:240-241` |

**V8 simplification candidates affecting it.** Per HARDENING-CONSOLIDATED-V8 §3 cohort ledger:

| V8 ID | Surgery | Disposition |
|---|---|---|
| α2 | Type-system stack 7 → 5 mechanisms (CHR-improvement defers V2) | UPDATE `PASS-1.md:73` to elide CHR-shaped from V1 surface |
| α3 | BIR alphabet 22 → 19 (3 pair-collapses) | UPDATE `PASS-1.md:41` Backend IR variant list |
| α4 | Grammar-IR `Map` + `HostCall` merge | UPDATE `PASS-1.md:24`, `PASS-1.md:35` Grammar IR variant list |
| α6 | Generic validation 3 paths → 2 | UPDATE `PASS-1.md:81` (drop structural-decrease detector) |
| β1 | Retire diagnostic numeric alias system | UPDATE `PASS-1.md:107-122` ledger to drop `BBNF1004`/`BBNF1201`/etc. numeric column |
| γ1 | Drop bbnf-side `&'i` enforcement | UPDATE `PASS-1.md:75` to leverage rustc borrow checker |
| γ2 | Match exhaustiveness leverage rustc | UPDATE `PASS-1.md:77` to defer-to-rustc |
| γ8 | Generic monomorphisation lean on rustc | UPDATE `PASS-1.md:81` |
| γ9 | Function-arrow-unification leverage rustc HM | UPDATE `PASS-1.md:73` |
| δ1 | DK13 rank-N body → tranche-D body | acceptable-with-rewording (rank-N is V1 surface, body work routes to tranche) |
| δ2 | Schema-mining miner telemetry refinement → tranche-D body | acceptable-with-rewording |
| δ3 | CHR-improvement layer body → V2 amendment | UPDATE `PASS-1.md:73` to drop CHR-shaped V1 mention |
| δ4 | GADT V2 amendment | acceptable-with-rewording (already routed via `BBNF-LOCAL-EQUALITY-ANNOTATION`) |

**V2-deferral occurrences.** See §4 ledger.

**Disposition:** **UPDATE** — Phase-8.4 fold per V8 §8 Agent A (~75 min). PASS-1 is the V8 SIMPLIFY-AVAILABLE target with the highest density of architectural simplifications (4 cardinality reductions, 4 rustc-leverage delegations, 3 ASPIRATIONAL routes). No re-architect needed.

### §2.2 PASS-2 (`restart/audit/pass-2-codegen/PASS-2.md`)

**Authoritative role.** Defines codegen + runtime + backends contract: Backend IR variant table (23 variants), `BackendLowerer` trait (8 methods), `Backend` trait integration per ARCH §7.5, runtime template parameter schema (16 fields), SIMD coverage matrix, detection thresholds, function-value lowering options (inline at known call site / monomorphise per call site / stack-allocated reference frame), per-crate trees for `ir`/`codegen`/`runtime`/`host`/`simd-scan`/`xtask`, generated LOC budgets, perf-gate trajectory.

**Post-Phase-7.2 fold completeness.**

| Item | Folded? | Citation |
|---|---|---|
| Backend trait integration (ARCH §7.5) | YES | `PASS-2.md:134-144` |
| `RegexProgram` rename (canonical) | YES | `PASS-2.md:34`, `PASS-2.md:65`, `PASS-2.md:81` |
| `parse-that-regex` cross-engine parity | YES | `PASS-2.md:81`, `PASS-2.md:493` |
| Function-value lowering (3 options) | YES | `PASS-2.md:194-203` |
| Closure-by-`&'i` runtime | YES (stack-allocated reference frame) | `PASS-2.md:201` |
| Lookbehind co-amendment | YES | `PASS-2.md:188-190` |
| Cursor/byte-skip Lock 3 ratification | YES | `PASS-2.md:192` |
| WASM defer post-V1 | YES (V2 amendment route) | `PASS-2.md:141` |
| TS defer post-V1 | YES (V2 amendment route) | `PASS-2.md:142` |
| OpenFrame deletion archaeology | YES | `PASS-2.md:491`, `PASS-2.md:598` |

**V8 simplification candidates affecting it.** Per V8 §3:

| V8 ID | Surgery | Disposition |
|---|---|---|
| α1 | Backend trait 5 methods → 2 (`emit_artefacts`) | UPDATE `PASS-2.md:134-144` per-backend obligation table; route to ARCH §7.5 owner |
| α7 | `BackendLowerer` 8-method polymorphism clarification | UPDATE `PASS-2.md:118-130` to clarify single-impl-vs-future-impl shape |
| β1 | Retire numeric alias (PASS-2 ledger surface) | UPDATE `PASS-2.md:567-578` diagnostic ledger to drop numeric column |
| β3 | Rename "OpenFrame clone absence" perf gate | UPDATE `PASS-2.md:489-495` mechanism gates |
| γ1 | Closure capture borrow-checker leverage | UPDATE `PASS-2.md:201` to defer-to-rustc |
| γ8 | Generic monomorphisation rustc leverage | UPDATE `PASS-2.md:194-203` lowering options |
| ε1 | 23-variant alphabet count vs ARCH §7.2 LayoutPush/Pop split (24 post-lowering) reconciliation | UPDATE `PASS-2.md:50-79` to pin authoritative count |
| ε2 | Cost-model trait sharing parser+regex citation | UPDATE `PASS-2.md:391-405` PASS-1 hand-off table |
| ε3 | `parse_in` arena lifetime vs closure-environment frame clarification | UPDATE `PASS-2.md:201` to anchor stack-bound rule |
| ε4 | E-graph rewrite-category cardinality audit (route to PASS-1 / ARCH §10) | acceptable-with-rewording (PASS-2 is consumer) |

**V2-deferral occurrences.** See §4 ledger; PASS-2 has 2 V2-deferral surface (WASM + TS routes), both load-bearing for Lock 5/Lock 7 amendment surfaces.

**Disposition:** **UPDATE** — Phase-8.4 fold per V8 §8 Agent B (~60 min). V8 verdict is READY with 5 non-blocking simplifications; PASS-2 is the cleanest of the three. Backend-trait collapse (α1) is the only architectural change; rest are payload + ledger trim.

### §2.3 PASS-3 (`restart/audit/pass-3-runtime/PASS-3.md`)

**Authoritative role.** Defines user-facing runtime + ecosystem contract: parse constructors (`parse` / `parse_in` / `parse_owned`), `DocumentView` trait, `path!`/`select!` macro family, visitor + `VisitTypes` bitflag pruning, `DocumentSnapshot` + `ReparsePlan` incremental parse, error recovery + `RecoveryFacts`, LSP/DAP scaffolding, fixture separation from Lock 14 onboarding, per-grammar feeder table (10 rows), compiler diagnostic ledger, benchmark + SOTA gates.

**Post-Phase-7.2 fold completeness.**

| Item | Folded? | Citation |
|---|---|---|
| `path!` macro rename (from `pointer!`) | YES | `PASS-3.md:16`, `PASS-3.md:87`, `PASS-3.md:92` |
| `BBNF-PATH-*` codes | YES | `PASS-3.md:16`, `PASS-3.md:455-457` |
| `format()` public method | YES | `PASS-3.md:78`, `PASS-3.md:81` |
| Closure stack frames (4 committed sites) | YES | `PASS-3.md:191` |
| `parse-that-regex` regex sub-crate | YES | `PASS-3.md:16` |
| `path-ts` defer post-V1 | YES (V2 amendment route) | `PASS-3.md:93`, `PASS-3.md:387-395` |
| WASM defer post-V1 | YES (V2 amendment route) | `PASS-3.md:474`, `PASS-3.md:525` |
| 6-directive grammar surface | YES | `PASS-3.md:16` |
| Function values + lambda literals | YES | `PASS-3.md:16`, `PASS-3.md:191` |
| Tape/direct union (Lock 1) | YES | `PASS-3.md:31`, `PASS-3.md:163-185` |

**V8 simplification candidates affecting it.** Per V8 §3:

| V8 ID | Surgery | Disposition |
|---|---|---|
| β1 | Retire numeric alias (PASS-3 ledger surface) | UPDATE `PASS-3.md:446-468` diagnostic ledger |
| β2 | SIMPLIFY `BBNF-OPT001/002` + reserved `BBNF-LOCAL-EQUALITY-ANNOTATION` to cookbook-only | UPDATE `PASS-3.md:452-453`, `PASS-3.md:468` |
| γ3 | Diagnostic infra: bind to `thiserror` + `miette` | UPDATE `PASS-3.md:444-470` ledger framing |
| γ4 | Visitor: leverage `syn::visit` precedent | UPDATE `PASS-3.md:148` visitor surface (HYBRID) |
| γ5 | LSP scaffolding: bind to `tower-lsp` | UPDATE `PASS-3.md:320-342` LSP routes |
| γ6 | DAP scaffolding: bind to `dap-types` | UPDATE `PASS-3.md:188-192`, `PASS-3.md:336-342` DAP routes |
| γ7 | Incremental parse: cite salsa as design language | UPDATE `PASS-3.md:208-223` `ReparsePlan` framing |
| δ5 | DAP integration body → tranche I body | acceptable-with-rewording (V1 surface, body deferred) |
| δ6 | LSP completion / semantic-tokens / imports → tranche I body | acceptable-with-rewording |
| δ7 | Incremental + reuse-map cookbook content → tranche I/J body | acceptable-with-rewording |

**V2-deferral occurrences.** See §4; PASS-3 carries the largest count (8 occurrences across `path-ts`, WASM ABI, `BBNF-LOCAL-EQUALITY-ANNOTATION` reserved code, `BBNF-HOST003` WASM reframing, H.W3/J.W3 deferrals).

**Disposition:** **UPDATE** — Phase-8.4 fold per V8 §8 Agent C (~75 min). V8 verdict is AMENDMENT-REQUIRED (additive trim). Per V8 verdict, PASS-3 carries the most ASPIRATIONAL/SPECULATIVE deferrals; trim is additive (route to receivers, not architectural rework).

## §3 Sub-agent reports disposition

Sub-agent reports are sealed Wave-1 (May 4) dispatch outputs. Each per-pass synthesis (PASS-N.md) absorbed sub-agent material at Wave-1.1/Wave-1.2 and ratified through Phase 7.2 / 7.5b. Sub-agents are not reread by later phases — they remain frozen as evidence.

| Directory | Sub-agents | Cited by current PASS-N.md? | Wave-1 dispatch date | Phase amendments touched? | Disposition |
|---|---|---|---|---|---|
| `pass-1-substrate/agent-{1..6}*.md` | IR Architect / Type System Designer / CSP-Egraph Architect / Cost Model Architect / Grammar Extension Designer / Substrate Coherence Auditor | agent-6 cited at `PASS-1.md` (substrate-coherence auditor), others absorbed by synthesis tables `PASS-1.md:5-21` (Verdict Ledger names "Agent" column with 1, 4, 6 / 1, 6 / 1 / 1, 3, 4 / 2 / 3 / 4 / 5 etc.) | 2026-05-04 (Wave-1) + agent-6 amendment 2026-05-04 18:28 | None — synthesis carries forward; agent-6 §5 records OpenFrame retirement post-amendment | **EXPLICATE** as sealed Wave-1 evidence; **PRUNE** if archive-discipline demands tighter restart/ surface (synthesis carries content forward) |
| `pass-2-codegen/agent-{1..6}*.md` | Backend IR Architect / Rust Lowerer Architect / WASM Lowerer + SIMD Architect / Runtime Template Architect / Pratt + SIMD Auto-Detection / Codegen Coherence Auditor | dispatched by `PASS-2.md:23-28` table; agent-5 carries `PASS-2.md` lookbehind co-amendment cross-cite (agent-5.md:97) | 2026-05-04 (Wave-1) | agent-5.md amendment notes Phase-7.2 lookbehind co-amendment | **EXPLICATE** as sealed evidence — PASS-2 synthesis is the live carrier; sub-agents remain auditable Wave-1 anchors |
| `pass-3-runtime/agent-{1..6}*.md` | Value API Designer / Path-Select DSL Designer / Visitor Surface Designer / Tape Union Architect / Error-Recovery Incremental Parsing / Ecosystem Architect | dispatched by `PASS-3.md:7-12`; agent-1.md:81 + agent-3.md:92 + agent-4.md:113 cite PASS-3 Wave-2 amendment receivers | 2026-05-04 (Wave-1) | Amendment cross-references at Wave-2 land in synthesis, not in sub-agents | **EXPLICATE** — sub-agents are sealed evidence; live carrier is PASS-3.md |

**Bulk disposition:** **EXPLICATE** for all 18 sub-agent reports. They are sealed Wave-1 dispatch evidence, carried forward by per-pass synthesis. Retirement (PRUNE) is not warranted in V8 cohort — they remain auditable record of the Wave-1 dispatch. If a future archive-discipline pass tightens `restart/` surface, the sub-agents may move to `restart/archive/` but **not** delete; the synthesis is the live document and the sub-agents are the audit trail.

## §4 V2-deferral occurrence ledger

Per `restart/prompts/audit-specs/HARDENING-LENS-SET.md` post-Phase-8.1 amendment + V8 §3 Tier δ: ASPIRATIONAL/SPECULATIVE items must fold V1, route to a tranche body receiver, or delete. No naked V2 deferrals.

### §4.1 PASS-1 V2-deferral ledger

| # | Path:line | Verbatim | V8 ID | Classification |
|---:|---|---|---|---|
| 1 | `PASS-1.md:73` | "OutsideIn-style implication constraints carry into the solver for branch-local equality plumbing as internal substrate" + "CHR-shaped where applicable" | δ3 | **must-fold-V1** (CHR layer body defers V2; surface mention drops; substrate stays). Edit: drop `CHR-shaped where applicable` clause; the substrate language already covers the surface. |
| 2 | `PASS-1.md:77` | "or-patterns and guards defer to V2" | n/a (V8 doesn't surface) | **acceptable-with-rewording** — narrow surface, has receiver (V2 pattern amendment). |
| 3 | `PASS-1.md:81` | "GADT/branch-local-equality machinery is internal substrate" + "when the surface lands post-V1" + "user-facing row-poly surface defers to a later type-system research gate, not to V1" | δ4 | **acceptable-with-rewording** (substrate is V1; surface routes via `BBNF-LOCAL-EQUALITY-ANNOTATION` receiver). The "later type-system research gate" phrasing is V2-amendment receiver. |
| 4 | `PASS-1.md:117` | `BBNF-LOCAL-EQUALITY-ANNOTATION` diagnostic string registered with no V1 emission | δ4 + β2 | **must-delete** per V8 β2 (cookbook-only suffices; emission infra unjustified). |
| 5 | `PASS-1.md:249` | "Closure-capture-by-move and the `Fn*` trait split defer to V2 amendment" | n/a (V8 acceptable) | **acceptable-with-rewording** — narrow surface, V2 amendment receiver named, diagnostic `BBNF-CLOSURE-CAPTURE-BY-MOVE` registered. |
| 6 | `PASS-1.md:251` | "Or-patterns and guards defer to V2" | n/a | **acceptable-with-rewording** — duplicate of #2. |

PASS-1 V2-deferral count: 6 occurrences. Of these: 1 must-fold-V1 (CHR-shaped clause), 1 must-delete (`BBNF-LOCAL-EQUALITY-ANNOTATION` emission infra), 4 acceptable-with-rewording (each has named receiver + narrow surface).

### §4.2 PASS-2 V2-deferral ledger

| # | Path:line | Verbatim | V8 ID | Classification |
|---:|---|---|---|---|
| 7 | `PASS-2.md:136-144` | Per-backend obligation table: WasmBackend + TsBackend "Deferred post-V1" with V2 amendment routes per Lock 5/7 | n/a (V8 acceptable) | **acceptable-with-rewording** — V2 amendment routes named, BIR shape ready, Backend trait pre-existence verified. |
| 8 | `PASS-2.md:200` | "user-facing GADT surface defers to V2 amendment via `BBNF-LOCAL-EQUALITY-ANNOTATION`" | δ4 | **acceptable-with-rewording** — receiver is V2 amendment + diagnostic code (per #4 above; if PASS-1 deletes the code per β2, this line must follow). |
| 9 | `PASS-2.md:586` | "TS production is deferred by the PASS-2 prompt" → carry ledger row | n/a | **acceptable-with-rewording** — has BD.W1 receiver. |
| 10 | `PASS-2.md:580-591` | Carry ledger 8 rows (TS production / WASM host primitive ABI / `path-ts` proc-macro shell / etc.) | n/a | **acceptable-with-rewording** — every row has Receiver/Blocker/Receiving-gate per HARDENING-CONSOLIDATED §4.39. |

PASS-2 V2-deferral count: 4 occurrences. All acceptable-with-rewording — every deferral has named receiver. PASS-2 is the cleanest of the three on V2-deferral discipline.

### §4.3 PASS-3 V2-deferral ledger

| # | Path:line | Verbatim | V8 ID | Classification |
|---:|---|---|---|---|
| 11 | `PASS-3.md:87`, `PASS-3.md:93`, `PASS-3.md:136`, `PASS-3.md:387-395` | "`path-ts` defers post-V1" — multiple occurrences in path/select section + crate tree | n/a | **acceptable-with-rewording** — Lock 7 amendment receiver, TS-native parse+runtime fork named, `path-ts` deletion-gate scan exclusion explicit. |
| 12 | `PASS-3.md:191` | "Function-value broadening beyond the four sites...defers to a Lock 1 reuse-map amendment" | n/a | **acceptable-with-rewording** — narrow surface (4 closure sites V1), receiver is Lock 1 reuse-map amendment. |
| 13 | `PASS-3.md:466` | `BBNF-HOST003`: "WASM lower-and-bench programme defers post-V1 alongside the V2 `WasmBackend: Backend` impl" | n/a | **acceptable-with-rewording** — V2 amendment receiver named, V1 emits reframed diagnostic. |
| 14 | `PASS-3.md:468` | `BBNF-LOCAL-EQUALITY-ANNOTATION` (reserved) — "reserved; no V1 emission" + "V2 amendment opens the user-facing annotation surface" | δ4 + β2 | **must-delete** per V8 β2 (cookbook-only suffices; reserved-without-emission carries unjustified V1 emission infrastructure). |
| 15 | `PASS-3.md:474` | "WASM host primitive route (V2 deferred). The runtime/WASM path...defers post-V1 alongside the V2 `WasmBackend: Backend` impl per Lock 5 amendment...The H.W3 / J.W3 WASM measurement rows defer to V2" | n/a | **acceptable-with-rewording** — Lock 5 amendment receiver, H.W3/J.W3 SOTA gates measure Rust-line only at V1. |
| 16 | `PASS-3.md:525` | Cross-pass hand-off: "WASM host primitive ABI descriptor (V2 deferred)" | n/a | **acceptable-with-rewording** — duplicate of #15 routing. |
| 17 | `PASS-3.md:528`, `PASS-3.md:575` | "`path-ts` defers post-V1 alongside the TS-native parse+runtime fork per Lock 7 amendment" | n/a | **acceptable-with-rewording** — duplicate of #11 routing. |
| 18 | `PASS-3.md:576` | Carry ledger: "Bench harness target numbers and machine profiles" — Blocker: "Bench rows become aspirational" | δ8 | **must-fold-V1** (V8 §3 δ8 — V1 SOTA-parity is correctness floor; SOTA-beat is audacious aspirational at tranche-H body). Edit: edit the row to clarify SOTA-parity (load-bearing V1) vs SOTA-beat (aspirational tranche-H body). |

PASS-3 V2-deferral count: 8 occurrences. Of these: 1 must-delete (`BBNF-LOCAL-EQUALITY-ANNOTATION` emission infra row), 1 must-fold-V1 (bench-row aspirational/load-bearing classification), 6 acceptable-with-rewording.

### §4.4 Aggregate V2-deferral count

| Pass | Total | must-fold-V1 | must-delete | acceptable-with-rewording |
|---|---:|---:|---:|---:|
| PASS-1 | 6 | 1 | 1 | 4 |
| PASS-2 | 4 | 0 | 0 | 4 |
| PASS-3 | 8 | 1 | 1 | 6 |
| **Total** | **18** | **2** | **2** | **14** |

Of the 18 V2-deferral occurrences: 4 fail V8 discipline (2 must-fold-V1 + 2 must-delete); 14 are acceptable-with-rewording (each has named receiver per HARDENING-CONSOLIDATED §4.39 carry-ledger discipline). The 4 failures collapse to 2 distinct architectural decisions:

1. **CHR-improvement layer** (V8 δ3 + occurrence #1): drop V1 surface mention; substrate is internal-only.
2. **`BBNF-LOCAL-EQUALITY-ANNOTATION` reserved-without-emission** (V8 β2 + occurrences #4 + #14): retire diagnostic code; the V2 surface lands with its own code at amendment time. The cookbook-only carry suffices.
3. **Bench-row aspirational language** (V8 δ8 + occurrence #18): edit row Blocker to distinguish SOTA-parity (load-bearing) vs SOTA-beat (aspirational).

## §5 Recommended actions

### §5.1 PASS-1 fold (Phase-8.4 Agent A, ~75 min)

1. **α3** BIR alphabet 22 → 19: rewrite `PASS-1.md:41` Backend IR variant list (3 pair-collapses; specific items per PASS-1 V8 punch list).
2. **α4** Grammar-IR Map + HostCall merge: rewrite `PASS-1.md:24`, `PASS-1.md:35`.
3. **α6** Generic validation 3 → 2 paths: rewrite `PASS-1.md:81`.
4. **β1** PASS-1 §6b ledger surface: drop numeric alias column.
5. **γ1, γ2, γ8, γ9** rustc-leverage delegations: HYBRID phrasing in §3 type-system algorithm.
6. **δ1, δ2, δ3, δ4** routing: ensure each ASPIRATIONAL item has tranche-D / V2-amendment receiver named inline.
7. **must-fold-V1** (#1): drop "CHR-shaped where applicable" clause from `PASS-1.md:73`.
8. **must-delete** (#4): retire `BBNF-LOCAL-EQUALITY-ANNOTATION` row from `PASS-1.md:117` diagnostic-strings table.

### §5.2 PASS-2 fold (Phase-8.4 Agent B, ~60 min)

1. **α1** Backend trait 5 methods → 2: rewrite `PASS-2.md:134-144` (delegate ARCH §7.5 owner).
2. **α7** `BackendLowerer` polymorphism clarification: rewrite `PASS-2.md:118-130`.
3. **β1** PASS-2 §8 ledger surface: drop numeric alias column.
4. **γ1, γ8** rustc-leverage delegations: HYBRID phrasing in §2 function-value lowering.
5. **ε1-ε4** five hygiene additions: 23-variant alphabet count, cost-model trait sharing citation, `parse_in` arena/closure-frame clarification, e-graph rewrite-category cardinality routing.

### §5.3 PASS-3 fold (Phase-8.4 Agent C, ~75 min)

1. **β1** PASS-3 §6b ledger surface: drop numeric alias column.
2. **β2** retire `BBNF-OPT001/002` + `BBNF-LOCAL-EQUALITY-ANNOTATION` to cookbook-only.
3. **γ3, γ4, γ5, γ6, γ7** host-leverage delegations: bind diagnostic infra to `thiserror`/`miette`; visitor to `syn::visit`; LSP to `tower-lsp`; DAP to `dap-types`; incremental to salsa design language.
4. **δ5, δ6, δ7** routing: DAP body → tranche I; LSP body → tranche I; incremental cookbook → tranche I/J body. Each receiver named inline.
5. **must-delete** (#14): retire `BBNF-LOCAL-EQUALITY-ANNOTATION` row from `PASS-3.md:468` diagnostic ledger.
6. **must-fold-V1** (#18): edit `PASS-3.md:576` carry ledger Blocker to distinguish SOTA-parity (load-bearing) vs SOTA-beat (aspirational).

### §5.4 Sub-agent reports

No action. Sub-agents are sealed Wave-1 evidence. Phase-8.4 fold edits the live syntheses (PASS-N.md), not the sealed Wave-1 reports.

## §6 Cross-cuts with V8 simplification candidates

V8 distributes 41 simplification candidates across 4 targets. The pass-directory subset:

| Tier | PASS-1 | PASS-2 | PASS-3 | Subtotal |
|---|---:|---:|---:|---:|
| α (architectural cardinality) | 4 (α2, α3, α4, α6) | 2 (α1, α7) | 0 | 6 |
| β (diagnostic vocabulary) | 1 (β1 ledger surface) | 1 (β1 ledger surface) | 2 (β1, β2) | 4 |
| γ (host-leverage) | 4 (γ1, γ2, γ8, γ9) | 2 (γ1, γ8) | 5 (γ3, γ4, γ5, γ6, γ7) | 11 |
| δ (meta-grammar deferral) | 4 (δ1-δ4) | 0 | 3 (δ5-δ7) | 7 |
| ε (V8 hygiene) | 0 | 4 (ε1-ε4) | 0 | 4 |
| **Subtotal** | **13** | **9** | **10** | **32** |

Out of 41 V8 cohort candidates, 32 (78%) route into pass-directory edits. The remaining 9 are MASTER-PLAN-only (γ10 Cargo.toml carrier; δ8-δ10 V2 amendment carries; ε5 carry ledger row). Pass-directory Phase-8.4 fold is the largest portion of V8.1 surgical surface.

**Cross-cut conflicts:** zero. V8 §4 confirms zero new cross-target architectural conflicts; the 32 pass-directory candidates distribute cleanly across single-target surfaces.

**Diagnostic numeric-alias retirement (β1)** is the largest cross-cut: 4 surfaces (ARCH §7.4 + PASS-1 §6b + PASS-2 §8 + PASS-3 §6b) need coordinated edit. Phase-8.4 Agent D (SYNTHESIS trio fold) owns the cross-target editorial.

## §7 Open questions

1. **V8 SIMPLIFY-AVAILABLE acceptance.** V8 §6 verdict: SIMPLIFY-AVAILABLE → Phase-8.4 dispatches if the user accepts. If not accepted, V7.1 baseline operates; per-tranche full-spec drafting (Wave 9+) unblocks immediately. Question: does this corpus audit recommend acceptance?
   - **Recommendation:** Accept Phase-8.4. The 4 must-fold/must-delete items (#1, #4, #14, #18) are not optional — they're V8 discipline failures; the rewords/retirements are necessary regardless of broader α/γ acceptance. Even a minimum Phase-8.4 (only these 4 surgical edits) lands V8.1 READY.

2. **Sub-agent retirement timing.** If Phase-8.4 lands V8.1, the per-pass syntheses are the V8.1 live carriers; sub-agents become Wave-1 historical evidence. Should `restart/archive/` open to receive sub-agents post-V8.1?
   - **Recommendation:** Defer to a separate archive-discipline pass; sub-agents are sealed evidence and unlikely to be reread, but the audit trail value persists.

3. **`BBNF-LOCAL-EQUALITY-ANNOTATION` retirement vs. reserved-as-cookbook-mention.** V8 β2 says "SIMPLIFY...to cookbook-only" — retire the V1 emission infrastructure, but the cookbook may mention the V2 surface. PASS-1.md:117 + PASS-3.md:468 both register the diagnostic code. Question: does cookbook-only require retiring the diagnostic-code row entirely, or rewriting it as a forward-reference?
   - **Recommendation:** retire the row from both ledgers; the V2 amendment will land with its own diagnostic code at amendment time. Reserved-without-emission codes are LLM-distribution artefacts (V8 β1 names the pathology).

4. **Carry-ledger distinguishment between `acceptable-with-rewording` and `must-fold-V1`.** Of the 18 V2-deferral occurrences, 14 are acceptable; 4 fail. The acceptance criterion is HARDENING-CONSOLIDATED §4.39: every deferral carries Receiver / Blocker / Receiving-gate. Question: does the audit reading of "named receiver" require the receiver be a specific tranche wave (e.g., D.W3, I.W2) or is "V2 amendment per Lock N" sufficient?
   - **Recommendation:** "V2 amendment per Lock N" is sufficient when the Lock amendment names the receiver; "tranche I body" is sufficient when the tranche owner is named. Anonymous "post-V1" or "future amendment" without receiver fails. The 14 acceptable cases each have Lock or tranche receiver named; the 4 failures lack either (CHR-shaped V1-surface mention with no receiver; reserved diagnostic codes without emission contract).

5. **Phase-8.4 Agent D scope reconciliation.** V8 §8 names Agent D for SYNTHESIS trio fold (~75 min): tier α 1, 2, 5 + tier β 1 + tier δ 8, 9, 10 + tier ε 5. This corpus audit covers pass directories only; SYNTHESIS trio fold (ARCH/MIGRATION/MASTER-PLAN) is out of scope for this audit but coordinates with Agent A/B/C edits. Question: should Phase-8.4 Agent D synthesise the cross-target diagnostic numeric-alias retirement (β1) or split across A/B/C/D?
   - **Recommendation:** Agent D synthesises β1 ARCH §7.4 catalogue; Agents A/B/C each retire their own ledger surfaces. The catalogue + per-pass ledgers must agree; cross-target editorial is Agent D's role.

---

**Audit closure.** PASS-1 + PASS-2 + PASS-3 syntheses are live, fold-complete through Phase-7.5b/Phase-7.2, and substrate-coherent. Sub-agent reports are sealed Wave-1 evidence. V2-deferral discipline holds at 14/18 acceptable-with-rewording; 4 fail (2 must-fold-V1, 2 must-delete) — all 4 collapse to 2 architectural decisions (CHR-improvement V1 surface drop; `BBNF-LOCAL-EQUALITY-ANNOTATION` retirement) plus 1 carry-ledger phrasing edit (SOTA-parity vs SOTA-beat). Phase-8.4 fold per V8 §8 Agent A/B/C dispatches the surgical edits; Agent D coordinates cross-target β1 retirement. V8.1 READY lands at ~120 min wall.
