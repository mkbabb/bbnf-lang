# PASS-2 research fold classification

## §1 Target and source artefacts read

Target surface: `restart/audit/pass-2-codegen/PASS-2.md`.

Classification output: `restart/research/fold-pass-2.md`.

Read set:

| Artefact | Use |
|---|---|
| `restart/research/topic-1-hm-foundations.md` | Out-of-scope scan except generic monomorphisation handoff pressure. |
| `restart/research/topic-2-bidirectional.md` | Out-of-scope scan; chain/subsumption diagnostics route to PASS-1/SYNTHESIS. |
| `restart/research/topic-3-csp-gadts.md` | Partial fold for finite generic-rule lowering and generated-budget pressure. |
| `restart/research/topic-4-egraphs.md` | Fold for bridge/extraction evidence reaching BIR without unified hypergraph. |
| `restart/research/topic-5-cost-models.md` | Fold for `CostDecision`, objective evidence, and profile-aware recognizer decisions. |
| `restart/research/topic-6-tape.md` | Fold for `TapeShape`/`ValueShape`, payload policy, scalar-cache evidence, and benchmark metadata. |
| `restart/research/topic-7-green-red-incremental.md` | Out-of-scope scan except no PASS-2 cache ownership. |
| `restart/research/topic-8-simd-dfa.md` | Fold for exact SIMD parity, prefilter verifier route, regex execution plan, and oracle routing. |
| `restart/research/INDEX.md` §2/§3 | Research output contract and topic routing. |
| `restart/prompts/sub-orchestrators/RESEARCH-FOLD.md` §4/§5 | Phase 2 fold rule: fold §5/§7, surface §6 contradictions. |
| `restart/prompts/sub-orchestrators/AMENDMENT-DISPATCH.md` §1 | Verify-then-patch discipline. |
| `restart/README.md` gestalt and §13 | Voice, two-surface, optimization, tape, performance, and discipline anchors. |
| `restart/locks/LOCKS.md` | Lock 1, 4, 5, 6, 8, 10, 13, and 14 checks. |
| `restart/audit/pass-2-codegen/PASS-2.md` | Current amendment surface. |
| `restart/audit/pass-1-substrate/PASS-1.md` cited rows | BIR producer, diagnostic ownership, cost/egraph/type handoffs. |
| `restart/audit/pass-3-runtime/PASS-3.md` cited rows | Materialisation cost and benchmark receiver boundaries. |
| `restart/ARCHITECTURE.md` cited rows | BIR, side-table, regex/SIMD, and performance context. |
| V5/V5.1 reports | `HARDENING-PASS-2-V5.md`, `HARDENING-CONSOLIDATED-V5.md`, `HARDENING-PASS-1-PASS-2-V5.1.md`, `HARDENING-PASS-1-PASS-2-V5.1A.md`. |

Current PASS-2 pre-fill:

| Surface | Current state | Classification effect |
|---|---|---|
| BIR ownership | PASS-2 is payload refiner, not BIR re-owner (`PASS-2.md:81-118`). | Do not rename variants unilaterally; use compatibility notes for regex execution plan. |
| Tape/direct | `TapeShape` and `ValueShape` exist but are underspecified (`PASS-2.md:36`). | Fold payload/traversal/scalar-cache policy. |
| Cost | `AltDispatch` and `AltSpeculative` say generation site is "cost model" (`PASS-2.md:56-57`). | Fold `passes::extract` + `CostDecision` evidence. |
| SIMD | Detection row has cost/tiny/Unicode rejection but lacks exact-vs-prefilter verifier contract (`PASS-2.md:165-171`). | Fold exact scalar parity and prefilter verifier-before-tape. |
| Diagnostics | `BBNF-OPT001`/`BBNF-OPT002` no longer teach `@pratt`/`@simd` after V5.1A. | Keep no-force rule; add objective/exactness wording only. |

## §2 Item routing table

| Item | Source | Target | Route | Rationale |
|---|---|---|---|---|
| T4-R1/R2 | Topic 4 §5 | PASS-2 §5 handoff and §2 payload-refiner text | FOLD | Stable IDs and solved facts are upstream, but PASS-2 must say lowerers consume selected BIR plus evidence, not stale e-node representatives. |
| T4-R3/R4 | Topic 4 §5 | PASS-2 §5 handoff | FOLD | Bridge justifications reach BIR extraction as evidence; no Lock 4 change required. |
| T4-R5/R6 | Topic 4 §5 | MASTER rows | OUT-OF-SCOPE | Target is `MASTER-PLAN.md`, denied to this worker. |
| T4-R7 | Topic 4 §5 | README rewrite-budget paragraph | OUT-OF-SCOPE | Target is README, denied. PASS-2 can only consume extraction evidence. |
| T4-R8/R9 | Topic 4 §5 | PASS-2 §5 cost handoff | FOLD | `ExtractionConstraint` and profile metadata affect what PASS-2 may expect from `CostFacts`. |
| T4-R10/R11 | Topic 4 §5 | Research INDEX / MASTER | DEFER | Bibliography and MASTER lock-close rows are outside allowed paths. |
| T4-R12 | Topic 4 §5 | Architecture pipeline note | OUT-OF-SCOPE | Architecture target denied; no PASS-2 patch. |
| T4-S1-S4 | Topic 4 §7 | README/ARCH/MASTER | OUT-OF-SCOPE | Sibling fold owns these surfaces. |
| T4-S5 | Topic 4 §7 | PASS-2 §5 handoff | FOLD | Backend IR builder receiving selected alternatives plus proof refs is a codegen contract. |
| T4-S6-S11 | Topic 4 §7 | README/ARCH/MASTER/INDEX | OUT-OF-SCOPE/DEFER | Non-PASS-2 targets or bibliography cleanup. |
| T4-S12 | Topic 4 §7 | PASS bridge rows | DEFER | Primary target is PASS-1 bridge/cost rows; PASS-2 only mirrors the evidence handoff. |
| T5-R1/R2 | Topic 5 §5 | PASS-2 §5 cost handoff and §8 diagnostics | FOLD | Shared regex/parser cost means shared `CostDecision` evidence envelope, not shared internals. |
| T5-R3/R4 | Topic 5 §5 | PASS-1 | OUT-OF-SCOPE | PASS-1 cost-model layout is denied. |
| T5-R5/R6 | Topic 5 §5 | ARCH/MASTER | OUT-OF-SCOPE | Synthesis fold owns these. |
| T5-R7 | Topic 5 §5 | PASS-2 §2 BIR table | FOLD | Dispatch/speculation generation sites should name `passes::extract` using `CostDecision`. |
| T5-R8 | Topic 5 §5 | PASS-2 detection thresholds | FOLD | SIMD rejection must consider target legality, opaque regex summary, and selected objective profile. |
| T5-R9/R10 | Topic 5 §5 | PASS-3/INDEX | DEFER | Sibling/INDEX targets are outside scope. |
| T5-S1-S5 | Topic 5 §7 | README/PASS-1/ARCH/MASTER | OUT-OF-SCOPE | Not PASS-2. |
| T5-S6 | Topic 5 §7 | PASS-2 §2 BIR table | FOLD | Same as R7; lowerers consume extraction output. |
| T5-S7 | Topic 5 §7 | PASS-2 detection thresholds | FOLD | Same as R8; no force directive. |
| T5-S8-S10 | Topic 5 §7 | PASS-3/INDEX | DEFER | Outside allowed files. |
| T5-S11 | Topic 5 §7 | PASS-2 diagnostic ledger | FOLD | `BBNF-OPT001`/`BBNF-OPT002` can mention profile/exactness without changing diagnostic ownership. |
| T5-S12 | Topic 5 §7 | MASTER carry row | OUT-OF-SCOPE | MASTER target denied. |
| T6-R1/R2 | Topic 6 §5 | README/PASS-3 | OUT-OF-SCOPE | Sibling surfaces; PASS-2 folds only the shape/evidence consequences. |
| T6-R3 | Topic 6 §5 | PASS-2 §2 commitment | FOLD | `TapeShape`/`ValueShape` need payload, span, traversal, and scalar-cache ownership. |
| T6-R4-R9 | Topic 6 §5 | ARCH/PASS-3 | OUT-OF-SCOPE | Not allowed; metadata consequence mirrored in PASS-2 perf gates where possible. |
| T6-R10 | Topic 6 §5 | PASS-2 BIR `Rule` row | FOLD | `tape kind` is too thin; PASS-2 can sharpen payload and traversal policy. |
| T6-R11 | Topic 6 §5 | Research INDEX | DEFER | Citation hygiene outside scope. |
| T6-R12 | Topic 6 §5 | README | OUT-OF-SCOPE | Fold result noted, no README edit. |
| T6-S1 | Topic 6 §7 | README | OUT-OF-SCOPE | Not allowed. |
| T6-S2 | Topic 6 §7 | PASS-2 §2 commitment | FOLD | Directly assigned tape lower-time obligation. |
| T6-S3-S8 | Topic 6 §7 | ARCH/PASS-3 | OUT-OF-SCOPE | Sibling fold owns exact runtime/API text. |
| T6-S9 | Topic 6 §7 | PASS-2 BIR `Rule` row | FOLD | Same as R10. |
| T6-S10/S11 | Topic 6 §7 | README/INDEX | DEFER | Outside scope. |
| T6-S12 | Topic 6 §7 | Future implementation gate | FOLD | Add PASS-2 generated materialisation-cost receiver fields without implementing. |
| T6-S13 | Topic 6 §7 | Future benchmark schema | FOLD | PASS-2 perf gates should require validation/source-ownership/materialisation metadata. |
| T6-S14-S16 | Topic 6 §7 | Future runtime/API tests | DEFER | Future implementation/runtime receiver; mention in gate plan only if tied to PASS-2 evidence. |
| T8-R1-R5 | Topic 8 §5 | README/ARCH | OUT-OF-SCOPE | Not allowed; PASS-2 may consume via regex-program compatibility note. |
| T8-R6/R7 | Topic 8 §5 | PASS-2 BIR regex/SIMD rows | FOLD | Regex verifier and `Exact | Prefilter` mode are lower-time correctness contracts. |
| T8-R8 | Topic 8 §5 | Architecture diagnostic | OUT-OF-SCOPE | Synthesis owns. |
| T8-R9 | Topic 8 §5 | PASS-2 scanner lowering gate | FOLD | Add false-positive discard, no false-negative proof, and scalar offset-vector parity. |
| T8-R10 | Topic 8 §5 | PASS-2 detection thresholds | FOLD | Exact scans require scalar parity; prefilters require verifier route. |
| T8-R11 | Topic 8 §5 | PASS-2 diagnostic ledger | FOLD | Keep no-force text; add verifier-first fallback. |
| T8-R12 | Topic 8 §5 | Agent report | OUT-OF-SCOPE | Denied file. |
| T8-S1 | Topic 8 §7 | ARCH + PASS-2 | PARTIAL-FOLD | Fold PASS-2 `SimdScanMode`/verifier contract; ARCH target remains sibling-owned. |
| T8-S2 | Topic 8 §7 | PASS-2 scanner gate | FOLD | Directly in scope. |
| T8-S3/S4 | Topic 8 §7 | README/ARCH | OUT-OF-SCOPE | Not allowed. |
| T8-S5 | Topic 8 §7 | PASS-2 `RegexDfa` row | FOLD | Add note that full DFA is one plan under regex program; do not rename variant alphabet unilaterally. |
| T8-S6/S7 | Topic 8 §7 | README/ARCH | OUT-OF-SCOPE | Sibling surfaces. |
| T8-S8/S9 | Topic 8 §7 | Agent/MASTER | OUT-OF-SCOPE | Denied files. |
| T8-S10 | Topic 8 §7 | PASS-2 lowerer contract | FOLD | Lowerers consume `SimdScan` only after exactness/verifier validation. |
| T8-S11 | Topic 8 §7 | PASS-2 diagnostic ledger | FOLD | Add exact fallback wording without `@simd`. |
| T8-S12 | Topic 8 §7 | README | OUT-OF-SCOPE | Not allowed. |
| T3-F1-F3/F5-F9 | Topic 3 §5 | README/ARCH/PASS-1/INDEX | OUT-OF-SCOPE/DEFER | Type-system fold belongs to PASS-1/SYNTHESIS. |
| T3-F4 | Topic 3 §5 | PASS-2 §6 generated LOC | FOLD | Finite `(RuleId, TypeArgs)` instance-set validation prevents codegen budget blow-up. |
| T3-Srg1-Srg5 | Topic 3 §7 | README/ARCH/PASS-1 | OUT-OF-SCOPE | Not PASS-2. |
| T3-Srg6 | Topic 3 §7 | PASS-2 generated-budget receiver | FOLD | Add finite monomorphization/generic-cycle evidence gate to PASS-2 budget. |
| T3-Srg7-Srg10 | Topic 3 §7 | ARCH/PASS-1/INDEX | OUT-OF-SCOPE/DEFER | Denied files. |
| T1-F5/S4 | Topic 1 §5/§7 | PASS-2 generated budget | PARTIAL-FOLD | Same finite monomorphisation-set pressure as Topic 3; no type-system prose folded. |
| T1 remaining | Topic 1 §5/§7 | README/ARCH/PASS-1/MASTER | OUT-OF-SCOPE | Type-system foundation belongs to PASS-1/SYNTHESIS. |
| T2 all | Topic 2 §5/§7 | README/ARCH/PASS-1/INDEX/MASTER | OUT-OF-SCOPE | No direct PASS-2 codegen contract beyond sibling-owned chain typing. |
| T7 all | Topic 7 §5/§7 | README/PASS-3/MASTER/INDEX | OUT-OF-SCOPE | Incremental/green-red runtime routing is PASS-3/SYNTHESIS work; no PASS-2 edit. |

## §3 §6 adversarial finding reconciliation

| Finding | Verbatim pressure preserved | Route | Rationale |
|---|---|---|---|
| T4-A1 | "representative promotion is unstable" and extraction is the choice point. | FOLD | Locks survive; PASS-2 wording can require selected BIR plus solved facts/justification evidence. |
| T4-A2 | Lock 4 lacks the egglog counterargument. | DEFER | Lock/Architecture explication is outside scope; no structural contradiction. |
| T4-A3 | "seven rewrite categories need budget gates." | DEFER | Rewrite-budget gates are README/MASTER/PASS-1 owned; PASS-2 consumes extraction evidence. |
| T4-A4 | "bridge facts need proof/explanation payloads." | FOLD | PASS-2 may require bridge justifications on selected alternatives without editing locks. |
| T4-A5 | egglog source catalogue provenance gap. | DEFER | Research INDEX cleanup outside scope. |
| T5-A1 | "scalar trait is too strong"; scalar fast path only if vectors survive. | FOLD | PASS-2 can require `CostDecision` evidence with objective vector/profile; no lock change. |
| T5-A2 | branch iterator double-counts DAG sharing. | FOLD | PASS-2 can name stable child/e-class/BIR identities as evidence consumed from extraction. |
| T5-A3 | shared-with-regex can violate domain opacity. | FOLD | PASS-2 can consume opaque `RegexCostSummary`, not regex internals. |
| T5-A4 | Topic 5 lock pointer is stale. | DEFER | Research INDEX cleanup outside scope. |
| T5-A5 | SMT-backed cost composition is under-specified. | FOLD | PASS-2 accepts `ObjectiveMode`/profile evidence but does not design the solver. |
| T5-A6 | Almomany and Deb provenance gaps. | DEFER | Bibliography outside scope; do not cite them in PASS-2. |
| T6-A1 | "scalar materialisation cost is overstated." | FOLD | Expand `TapeShape`/`ValueShape` and materialisation-cost evidence. |
| T6-A2 | "`union` can be misread as two trees." | FOLD | PASS-2 can state one tape identity with typed projections and declared caches. |
| T6-A3 | On-Demand forward-only semantics conflict with bbnf tooling. | DEFER | Runtime/API prose is PASS-3/README; PASS-2 avoids On-Demand API claims. |
| T6-A4 | in-situ competitors can make benchmark rows unfair. | FOLD | PASS-2 perf metadata should record validation/source ownership/materialisation mode. |
| T6-A5 | Hubbard comparative study is a provenance gap. | DEFER | INDEX cleanup; no PASS-2 citation. |
| T6-A6 | UTF-8 validation entry point needs explicit split. | FOLD | PASS-2 benchmark metadata can distinguish `parse(&str)` prevalidation from byte/file validation. |
| T8-A1 | SIMD positive versus DFA negative is under-specified. | FOLD | Add `Exact | Prefilter`, scalar parity, verifier-before-tape. |
| T8-A2 | Full DFA codegen cannot be mandatory for rich Unicode regex. | FOLD | Add compatibility note: `RegexDfa` is an execution plan under regex program, not full-DFA mandate. |
| T8-A3 | Bespoke regex risks reimplementing `regex-automata` without clear delta. | FOLD | Add oracle/parity receiver for regex program evidence; do not replace SOTA by assertion. |
| T8-A4 | "SIMD-first everywhere" can over-select. | FOLD | PASS-2 already cost-selects; sharpen exactness/profile rejection. |
| T8-A5 | Hyperscan/Vectorscan multi-pattern expectations may be a false friend. | DEFER | Research footnote/MASTER perf gate, not PASS-2. |
| T3-A6 | Monomorphization lacks an explicit finiteness gate. | FOLD | Generated-budget gate needs finite `(RuleId, TypeArgs)` instance set. |
| T3-A1-A5/A7 | CSP/type-system/source-row pressures. | OUT-OF-SCOPE/DEFER | PASS-1/SYNTHESIS/INDEX own these. |
| T1-A1-A3 | HM/subtyping wording and record narrowing. | OUT-OF-SCOPE | Type-system claims do not edit PASS-2 except generic budget mirror. |
| T2-A-E | Lock drift, HM/subsumption, DK gate, coercion sites, Roc provenance. | OUT-OF-SCOPE/DEFER | Not PASS-2; no lock contradiction for codegen. |
| T7-A1-A5 | Owning representation, snapshot identity, recovery, cache survival, provenance gaps. | OUT-OF-SCOPE/DEFER | PASS-3/SYNTHESIS/INDEX own; no PASS-2 amendment. |

Escalation candidates:

| Candidate | Verdict | Reason |
|---|---|---|
| Rename `RegexDfa` to `RegexProgram` everywhere. | DEFER, not ESCALATE | PASS-2 is not BIR re-owner; add a compatibility note unless PASS-1/SYNTHESIS renames the alphabet. |
| Add egglog rationale to Lock 4. | DEFER, not ESCALATE | Lock survives and only needs explanatory hardening outside scope. |
| Repair stale lock/source rows in INDEX for Topics 3/4/5/6/7. | DEFER | Research INDEX denied to this worker. |

No finding requires structural lock amendment before the PASS-2 fold. The lock file is weakened in prose in several places, but the actual locks survive.

## §4 Accepted amendment plan for PASS-2

1. In §2 commitments, expand tape/direct-to-struct as one identity: `TapeShape` owns token kind, span class, payload class, traversal skip policy, and scalar-cache policy; `ValueShape` owns generated projection over the same node id.
2. In the BIR table, change `Rule` payload from "value shape, tape kind" to "`ValueShape`, `TapeShape`, payload policy, traversal policy"; change dispatch/speculation generation sites to `passes::extract` using `CostDecision` evidence.
3. Add a regex compatibility note near the BIR table: `RegexDfa` is a compatibility name for the regex-program payload; full DFA is not mandatory for every regex, and VM/lazy-DFA/full-DFA selection plus Unicode/state limits stay below BIR.
4. Strengthen scanner lowering tests with SIMD false-positive discard, no false-negative proof, scalar offset-vector equality for exact scans, and verifier-before-tape for prefilters.
5. Strengthen detection thresholds: SIMD selects only when target legality, exactness/verifier route, and selected objective profile win; opaque `RegexCostSummary` feeds comparison without regex internals.
6. Add a PASS-1 handoff row for `CostDecision`/`BridgeJustification`/`RegexCostSummary` evidence reaching BIR extraction.
7. Add generated budget text requiring PASS-1 finite `(RuleId, TypeArgs)` monomorphisation set before PASS-2 emits generic-rule instances; generated LOC reports group by that set.
8. Add perf metadata floor: validation mode, source ownership mode, materialisation mode, scalar-cache policy, objective profile, and parse entry (`parse(&str)` prevalidated vs byte/file validated) must be recorded beside competitor/platform rows.
9. Update `BBNF-OPT001`/`BBNF-OPT002` strings to mention objective profile/exactness and verifier-first fallback while preserving no `@pratt`/`@simd` force directive.

Rejected amendment actions:

| Action | Reason |
|---|---|
| Rename BIR variant alphabet. | PASS-2 may not re-own BIR variants. |
| Edit README, locks, INDEX, ARCHITECTURE, MASTER, PASS-1, PASS-3, or agent files. | Explicitly denied. |
| Cite Hubbard, Almomany, or exact Deb 2014 as evidence. | Provenance gaps. |
| Add grammar author force directives for SIMD/Pratt. | Lock 10 forbids. |

## §5 Gate plan

Minimum pre-commit gate for classification:

```text
git status --short
git diff --check
git diff --cached --check
```

Minimum pre-commit gate for PASS-2 amendment:

```text
git status --short
rg -n "CostDecision|Pareto|objective|scalar Cost|TapeShape|ValueShape|parallel substrate|SIMD|DFA|prefilter|scalar parity|regex-automata|validation|Hubbard|Almomany|Deb|@pratt|@simd|path!|Wave 4" restart/audit/pass-2-codegen/PASS-2.md restart/research/fold-pass-2.md
git diff --check
git diff --cached --check
```

Acceptance checks for amendment text:

| Gate | Expected result |
|---|---|
| `CostDecision` grep | PASS-2 names evidence reaching dispatch/speculation and handoffs. |
| `TapeShape`/`ValueShape` grep | PASS-2 says both project one tape identity and declare payload/traversal/scalar-cache policy. |
| `prefilter`/`scalar parity` grep | PASS-2 names exact scans and verifier-before-tape prefilters. |
| `regex-automata` grep | PASS-2 keeps oracle/parity route, not blind replacement claim. |
| `validation` grep | PASS-2 benchmark metadata records validation/source ownership/materialisation mode. |
| `@pratt`/`@simd` grep | Zero hits in PASS-2 unless prohibition-only; amendment must add none. |
| provenance grep | `Hubbard`, `Almomany`, exact `Deb` do not become PASS-2 evidence. |

## §6 Classification verdict

Verdict: FOLD-READY for PASS-2 after narrow amendments.

Summary:

| Bucket | Count posture |
|---|---|
| FOLD | Cost/egraph evidence, tape shape policy, generic monomorphisation budget, SIMD exact/prefilter contract, regex plan note, benchmark metadata. |
| DEFER | Research INDEX provenance/lock drift; lock rationale; MASTER/ARCH/README/PASS-3 rows. |
| OUT-OF-SCOPE | Type-system prose, incremental runtime identity, recovery/LSP, agent report edits. |
| ESCALATE | None. |

The accepted PASS-2 fold is textual and gate-level. It preserves Lock 1's no-parallel-substrate rule, Lock 4's bridge-not-fusion rule, Lock 5's BIR boundary, Lock 6's committed-source rule, Lock 8's explicit SOTA rows, Lock 10's no-force-directive rule, and Lock 14's grammar-neutral generic code rule. The fold should proceed to commit 2 with only `restart/audit/pass-2-codegen/PASS-2.md` edited.
