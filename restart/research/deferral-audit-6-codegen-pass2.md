# Deferral Audit 6 — Codegen / PASS-2 / Lowering

Audit lane: V6-READY corpus; codegen + lowering; greenfield mandate.

## §1 — Scope and corpus references

This audit reads the V6 PASS-2 surface plus the four research topics that touch
codegen (4, 5, 6, 8), the codegen sections of Architecture, the codegen-bearing
tranche rows of MASTER-PLAN, and the V6 hardening consolidation. The question
is which deferrals stand on greenfield ground, which fold into V1, and where
deferrals reach the lowering layer load-bearing enough to require row-level
disposition.

Corpus read:

| File | Use |
|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:1-589` | Primary surface; verdict, BIR table, lowerer trees, perf rows, carry ledger, diagnostics. |
| `restart/research/topic-4-egraphs.md:1-926` | E-graph extraction, bridge-vs-fusion, rewrite categories, run budgets. |
| `restart/research/topic-5-cost-models.md:1-895` | `CostDecision`, `ObjectiveMode`, Pareto/lexicographic/box selection, scalar fast path, regex/parser cost sharing. |
| `restart/research/topic-6-tape.md:1-504` | `TapeShape`/`ValueShape`, simdjson tape posture, lazy/eager scalar, validation/source-ownership benchmarks. |
| `restart/research/topic-8-simd-dfa.md:1-931` | `Exact|Prefilter` SIMD modes, DFA execution plans, regex-automata oracle, bespoke regex delta. |
| `restart/research/fold-pass-2.md:1-216` | Authoritative item routing for Topic 4/5/6/8 into PASS-2 versus sibling targets. |
| `restart/ARCHITECTURE.md:889-1330` | BIR final-23 table, side tables, runtime architecture, codegen lowerers, perf gates. |
| `restart/MASTER-PLAN.md:330-505` | Tranches D (extension surface), E (BIR + VM), F (Rust lowerer + runtime template), H (Pratt/SIMD/WASM). |
| `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md:161-356` | §5 punch list (closed/residual), §7 lane crosswalk, §8 topic ownership matrix. |

Citation discipline: every disposition row carries a path:line reference into
the corpus. No prose is unbacked.

## §2 — Backend deferrals

### §2.1 — TS backend posture

PASS-2 explicitly defers TS production to BD.W1 / SYNTHESIS post-PASS-3 while
preserving scaffold capability and a BIR shape that "supports TS lower without
retrofit when scope opens" (`restart/audit/pass-2-codegen/PASS-2.md:563`). The
final BIR variant table at `PASS-2.md:54-79` keeps a "TS status" column whose
entries are uniformly `scaffold`, which is the architectural signal that BIR
parity for TS is contractually live even though emission is not.

| Item | Current posture | Greenfield value if folded into V1 | Disposition |
|---|---|---|---|
| TS production lowerer (full) | Deferred to BD.W1 / post-PASS-3 (`PASS-2.md:563`). | Low: shifts the V1 cohort from 2 lowerers (Rust, WASM-via-wasm32) to 3, multiplies parity matrix from 2-lane to 3-lane, expands `BackendLowerer` impl set, doubles the "9 grammars × ≥3 fixtures × N backends" cell count beyond the 81-cell BD.W5 target. | DEFER — keep PASS-2 posture. The Lock 5 boundary already accommodates a TS lowerer once scope opens; the BIR carries scaffold rows already; the V1 cost is parity-matrix inflation without throughput coverage that Rust+WASM does not deliver. |
| TS scaffold compile/smoke at PASS-2 | KEEP (`PASS-2.md:579`, `PASS-2.md:563`). | Already in V1: the scaffold proves the BIR shape is TS-lowerable without freezing the emitted source. | FOLD-AS-IS. The scaffold is already V1. |
| Other backends (C, Java, Python) | Out of V1; not named in any V6 corpus row. | None: the seed-grammar corpus (json, css_l4, csv, bbnf, bnf, ebnf, css_pretty, google_sheets, math) plus yaml smoke is fully covered by Rust+WASM throughput. | EXCLUDE. No architectural decision in the V6 corpus boxes a future C/Java/Python lowerer out; `BackendLowerer` is a generic trait at `PASS-2.md:119-130` and the BIR is backend-neutral by Lock 5. |

The architectural floor that protects future backends is `BackendLowerer` plus
the import-deny rule at `PASS-2.md:241-256`. Any backend that cannot consume
BIR alone is rejected; that constraint binds future backends as much as the
current Rust+WASM pair.

### §2.2 — WASM posture (folded, not deferred)

WASM V1 is "wasm32 Rust plus binding layer" (`PASS-2.md:40`), with raw WAT
demoted to a smoke fixture. This is not a deferral — it is a narrowing of the
WASM scope from "two production lowerers" to "one production lowerer with two
ABIs." The H.W3 measurement row at `restart/MASTER-PLAN.md:479` keeps the WASM
performance gate live but defers absolute numbers (`{N}`, `{M}`) to H.W3
measurement, which is a routed measurement deferral, not an architectural one.

| Item | Current posture | Disposition |
|---|---|---|
| WASM V1 (wasm32 Rust + binding) | KEEP, V1 production (`PASS-2.md:40`, `MASTER-PLAN.md:479-491`). | FOLD-AS-IS. |
| Raw WAT lowerer | DEMOTE to smoke fixture (`PASS-2.md:40`, `crates/codegen/src/lower/wasm/smoke_wat.rs` per `PASS-2.md:225`). | FOLD-AS-IS. |
| WASM SIMD-128 + scalar fallback | KEEP, parity-required (`PASS-2.md:163-165`, `MASTER-PLAN.md:485-491`). | FOLD-AS-IS. |
| WASM host primitive ABI descriptor | Route to H.W3 / BD.W2 / BD.W3 (`PASS-2.md:568`). | DEFER MEASUREMENT, not architecture. The descriptor schema is fixed by `PASS-2.md:483-491`; only the measured row at H.W3 is open. |

## §3 — Lowering deferrals

This is the load-bearing audit lane. Each row names what the V1 lowerer is
required to produce, what the deferral surfaces, and where it returns.

### §3.1 — Generic-rule lowering

PASS-1's HM core generalises rule schemes; PASS-2 lowers each instantiation
once finite `(RuleId, TypeArgs)` evidence exists
(`restart/ARCHITECTURE.md:1155`, `PASS-2.md:417`). The deferral here is _not_
the generic-rule lowering itself — that is V1 — it is whether the finite
instance-set proof is upstream-ready when PASS-2 emits.

| Item | Producer | Consumer | Disposition |
|---|---|---|---|
| Finite `(RuleId, TypeArgs)` instance set | PASS-1 monomorphisation pass + `BBNF-GENERIC-CYCLE` diagnostic at `ARCHITECTURE.md:1051`. | PASS-2 generic-rule lowerer (`PASS-2.md:417`). | FOLD-AS-IS. The hardening matrix at `HARDENING-CONSOLIDATED-V6.md:318` names the receiver as D.W1/F.W1 and the producer as PASS-1. |
| Generic-rule emission | PASS-2 Rust lowerer per instance row. | Generated `runtime/src/grammars/<name>/generated.rs`. | FOLD-AS-IS. The emission is gated on the upstream instance set; no architectural deferral. |
| Generic-cycle pathology guard | PASS-1 (producer); PASS-2 budget gate (`PASS-2.md:417`). | `cargo xtask bbnf generated-loc-budget --check`. | FOLD-AS-IS. |

### §3.2 — Function-value / first-class function lowering

This is the load-bearing question. The V6 corpus is unambiguous: BBNF closures
are not first-class functions in the host sense. The full closure type
inventory at `ARCHITECTURE.md:1187-1207` lists exactly four closure forms (host
chain, map, predicate, recovery) with bounded captures and explicit lowerings
(`HostChain`, `ValueProject`/`DirectBuild`, predicate inline, `ErrorRecover`).
The forbidden-behavior row at `ARCHITECTURE.md:1200-1207` rules out "capturing
arbitrary host process state through grammar syntax."

This is _not_ deferred first-class fn support. It is rejected first-class fn
support, with closures admitted only as bounded codegen sugar.

| Item | Current posture | Disposition |
|---|---|---|
| Host chain closure | Lowers to `HostChain` BIR variant (`ARCHITECTURE.md:1195`, `PASS-2.md:74`). | FOLD-AS-IS. V1. |
| Map closure | Lowers to `ValueProject`/`DirectBuild` (`ARCHITECTURE.md:1196`). | FOLD-AS-IS. V1. |
| Predicate closure | Inlined (`ARCHITECTURE.md:1197`). | FOLD-AS-IS. V1. |
| Recovery closure | Lowers to `ErrorRecover` (`ARCHITECTURE.md:1198`). | FOLD-AS-IS. V1. |
| First-class generic functions (general fn values) | Out of V1 closure inventory; not represented in BIR or in any tranche row. | EXCLUDE (not DEFER). The closure semantics are "intentionally narrow … to model host chains and typed grammar mappings without turning BBNF into a general programming language" (`ARCHITECTURE.md:1189-1191`). |
| Higher-rank polymorphism | Out of V1 by `ARCHITECTURE.md:1161-1166`. | EXCLUDE for V1. Architecturally reserved by named amendment gate (Dunfield-Krishnaswami / OutsideIn). |
| GADTs / branch-local equalities | Out of V1 (`ARCHITECTURE.md:1161-1166`); reserved diagnostic `BBNF-LOCAL-EQUALITY-ANNOTATION` (`ARCHITECTURE.md:1052`). | EXCLUDE for V1. |

The §6 of this audit examines the three lowering options for first-class fns
in detail. The V1 verdict is option 3 (inline at known call sites only),
implemented via the four closure lowerings above. No vtable, no
monomorphisation explosion, no environment heap-frames.

### §3.3 — Pattern-matching / match-expression lowering

| Item | Current posture | Disposition |
|---|---|---|
| Grammar-level alternation (`Alt`) | Lowers to `DispatchAlt` (cost-disjoint) or `SpeculativeAlt` (cost-overlapping) per `passes::extract` evidence (`PASS-2.md:56-57`, `ARCHITECTURE.md:930-931`). | FOLD-AS-IS. The "match expression" of the grammar is alternation; the BIR has explicit dispatch and speculation variants. |
| Jump tables | Generated by Rust lowerer when `DispatchAlt` evidence shows byte-disjoint or PHF-favorable arms (`PASS-2.md:172`). | FOLD-AS-IS. V1. |
| User pattern matching | Not a BBNF surface. | EXCLUDE. BBNF authors patterns through alternation + map closures; there is no general `match` expression. |

### §3.4 — Lookbehind lowering

| Item | Current posture | Disposition |
|---|---|---|
| Bounded `|<` lookbehind | V1 BIR variant `Lookbehind` (`PASS-2.md:78`), reverse predicate emission, finite-width gate (`PASS-2.md:175-179`). | FOLD-AS-IS. |
| Unbounded lookbehind | Rejected via `BBNF1004` (PASS-1) + `BBNF-SEM040` (PASS-2 BIR validation guard) (`PASS-2.md:553`, `ARCHITECTURE.md:1058`). | FOLD-AS-IS. Diagnostic-routed deferral, not architectural. |

### §3.5 — `RegexProgram` / `RegexDfa` lowering

This is the canonical compatibility-name pair. PASS-2 keeps `RegexDfa` as the
BIR alphabet entry (`PASS-2.md:65`, `PASS-2.md:81`); Architecture ratifies
`RegexProgram` at `ARCHITECTURE.md:903-935` as the BIR final-23 spelling.
SYNTHESIS owns the alphabet decision. The PASS-2 surface treats the spelling
as a compatibility-name slot whose payload is a "regex-program contract: VM,
lazy-DFA, full-DFA, literal prefilter, and Unicode-table plans" (`PASS-2.md:81`).

| Item | Current posture | Disposition |
|---|---|---|
| `RegexProgram` payload contract | KEEP — VM / lazy-DFA / full-DFA / prefilter execution plan, Unicode metadata, verifier contract (`PASS-2.md:65`, `ARCHITECTURE.md:935`). | FOLD-AS-IS. |
| Full DFA codegen mandate | RETIRED — full DFA is one execution plan, not a per-regex requirement (`PASS-2.md:81`). | FOLD-AS-IS (correct retraction). |
| `RegexDfa` vs `RegexProgram` rename | DEFER to SYNTHESIS reconciliation (`fold-pass-2.md:145`). | DEFER. PASS-2 may not re-own BIR variant alphabet (`PASS-2.md:85-114`). |

### §3.6 — Generic monomorphisation budget

This is the load-bearing generated-LOC defense for V1. Without finite-instance
proof, the +2% PASS-2 budget can be silently exceeded by a single recursive
generic family.

| Item | Current posture | Disposition |
|---|---|---|
| Finite instance-set evidence | Producer = PASS-1; consumer = PASS-2 LOC-budget gate (`PASS-2.md:417`). | FOLD-AS-IS. |
| Per-grammar +2% LOC ceiling | Set in `PASS-2.md:401-413`; closed by `cargo xtask bbnf generated-loc-budget --check`. | FOLD-AS-IS. |

## §4 — Optimization deferrals

### §4.1 — E-graph rewrite categories

Lock 6 (now Lock 4 in the V6 numbering) names seven V1 rewrite categories at
`restart/README.md:229-239`: algebraic, charclass merging, keyword-set
detection, operator-chain detection, repeat-loop hoisting, tail-call
elimination, non-progressing-Alt removal. Topic 4 §6 A3 raises a budget-gate
deferral concern; `fold-pass-2.md:115` routes the full rewrite-budget
implementation detail outside PASS-2.

| Category | V1 status | Disposition |
|---|---|---|
| Algebraic | V1 (`README.md:231`). | FOLD-AS-IS. |
| Charclass merging | V1 (`README.md:233`). | FOLD-AS-IS. |
| Keyword-set detection | V1 (`README.md:234`); feeds PHF dispatch (`PASS-2.md:172`). | FOLD-AS-IS. |
| Operator-chain detection | V1 (`README.md:235`); feeds `PrattSpine` BIR variant (`PASS-2.md:75`). | FOLD-AS-IS. |
| Repeat-loop hoisting | V1 (`README.md:236`). | FOLD-AS-IS. |
| Tail-call elimination | V1 (`README.md:237`). | FOLD-AS-IS. |
| Non-progressing `Alt` removal | V1 (`README.md:238`). | FOLD-AS-IS. |
| Per-category run budgets | DEFER to C.W4/C.W5 implementation gates (`HARDENING-CONSOLIDATED-V6.md:177` R5). | DEFER MEASUREMENT, not architecture. |

The seven-category set is V1; the implementation detail (node limit, iteration
limit, per-category timeout) is implementation work routed to C.W4/C.W5 with
named owner per the hardening matrix.

### §4.2 — Cost-model decisions

`CostDecision` evidence shape is the load-bearing PASS-1 → PASS-2 contract.
PASS-2 consumes records that include scalar score, objective vector, selected
profile, legality, stable child/e-class/BIR ids, selected alternative, and
rejected/dominated alternatives (`PASS-2.md:374`).

| Item | V1 status | Disposition |
|---|---|---|
| Scalar fast-path score | V1 (`PASS-2.md:374-377`); permitted only when full evidence record survives. | FOLD-AS-IS. |
| Objective vector (multi-dimensional cost) | V1 evidence shape; carries runtime estimate, compile-time, generated LOC, allocation, branch/setup, fallback risk (`topic-5-cost-models.md:537-541`). | FOLD-AS-IS. |
| `ObjectiveMode` (weighted / lexicographic / Pareto / box) | V1 evidence shape (`topic-5-cost-models.md:544-547`); mode selection is implementation. | FOLD-AS-IS as evidence carrier; mode policy is implementation work. |
| SMT-backed cost composition | DEFER (`topic-5-cost-models.md` A5, `fold-pass-2.md:122`). | DEFER. Z3 vocabulary supplies framing, not V1 implementation. |
| `RegexCostSummary` opaque envelope | V1 (`PASS-2.md:375`); regex internals stay below the cost boundary. | FOLD-AS-IS. |

### §4.3 — Specialization

| Item | Current posture | Disposition |
|---|---|---|
| Instance-specific specialization (per `(RuleId, TypeArgs)`) | V1 via finite monomorphisation (`PASS-2.md:417`). | FOLD-AS-IS. |
| Profile-guided specialization | Not in V1; no row in MASTER-PLAN tranches A-J. | DEFER post-V1. The hardening `HARDENING-CONSOLIDATED-V6.md:321` carries `CostDecision` profile evidence, but feedback-loop PGO is unrouted. |

### §4.4 — Inlining

V1 inlining is structural (predicate closures, single-call host functions),
not the general function-call inliner. Because first-class fns are excluded
(see §3.2), the inlining surface is bounded.

| Item | Current posture | Disposition |
|---|---|---|
| Predicate closure inlining | V1 (`ARCHITECTURE.md:1197`). | FOLD-AS-IS. |
| Host-call inlining at known sites | V1 — `CallHost` and `HostChain` lowerers can inline single-step chains (`PASS-2.md:74`, `ARCHITECTURE.md:939-940`). | FOLD-AS-IS. |
| General function-call inliner | EXCLUDE. No first-class fn surface to inline against. | EXCLUDE. |

### §4.5 — Pratt detection

| Item | Current posture | Disposition |
|---|---|---|
| Auto-detected Pratt via `PrattSpine` | V1 (`PASS-2.md:75`, `MASTER-PLAN.md:476`). | FOLD-AS-IS. |
| `@pratt` user directive | RETIRED. Lock 10 forbids force directives (`HARDENING-CONSOLIDATED-V6.md:326-327`); diagnostic `BBNF-OPT001`/`BBNF-PRATT-NOT-APPLIED` no longer teaches `@pratt`. | RETIRED — correctly. |

### §4.6 — SIMD detection

| Item | Current posture | Disposition |
|---|---|---|
| Auto-detected `SimdScan` via cost-model + recognizer mining | V1 (`PASS-2.md:76`, `MASTER-PLAN.md:477`). | FOLD-AS-IS. |
| `Exact` mode (scalar-parity-required) | V1 (`PASS-2.md:106`, `ARCHITECTURE.md:936`). | FOLD-AS-IS. |
| `Prefilter` mode (verifier-before-tape) | V1 (`PASS-2.md:106`, `ARCHITECTURE.md:936`). | FOLD-AS-IS. |
| `@simd` user directive | RETIRED. Lock 10 forbids; diagnostic `BBNF-OPT002`/`BBNF-SIMD-NOT-SELECTED` no longer teaches `@simd` (`HARDENING-CONSOLIDATED-V6.md:326`). | RETIRED — correctly. |

### §4.7 — Layout optimization

| Item | Current posture | Disposition |
|---|---|---|
| `LayoutFacts` consumption via `LayoutPush`/`LayoutPop` (or `Layout` BIR variant per PASS-2 spelling) | V1 (`PASS-2.md:67-69`, `ARCHITECTURE.md:941-942`). | FOLD-AS-IS. |
| Layout-pass-driven specializations | V1 via `LayoutSink` consumer (`PASS-2.md:69`). | FOLD-AS-IS. |

## §5 — Generated-code deferrals

| Item | V1 coverage | Disposition |
|---|---|---|
| Generated visitor LOC | V1 — emitted from BIR view shapes via `runtime_template/files.rs` (`PASS-2.md:151`). | FOLD-AS-IS. |
| Generated path-schema (`<grammar>.path-schema.toml`) | V1 — `MASTER-PLAN.md:1374-1376` (yaml row), `ARCHITECTURE.md:1392-1399` (per-grammar table). | FOLD-AS-IS. |
| Generated tape identity (`runtime/src/grammars/<name>/`) | V1 — emitted under one template (`PASS-2.md:44`, `ARCHITECTURE.md:1247-1257`). | FOLD-AS-IS. |
| Bench-report generation | V1 — perf metadata floor at `PASS-2.md:462`; SOTA gate trajectory rows at `PASS-2.md:452-460`. | FOLD-AS-IS. |
| Regen tooling (`cargo xtask regen --check`) | V1 — content-equality writing kept from current source (`PASS-2.md:46`); split into `xtask/src/regen/` per `PASS-2.md:319-332`. | FOLD-AS-IS. |
| `cargo xtask bbnf bir --all --check` BIR snapshot gate | V1 — close gate at `PASS-2.md:246-247`. | FOLD-AS-IS. |
| `cargo xtask bbnf cost-table --check` materialisation cost table | V1 — PASS-3 consumer gate at `PASS-2.md:359`. | FOLD-AS-IS. |
| `cargo xtask bbnf generated-loc-budget --check` | V1 — `PASS-2.md:417`. | FOLD-AS-IS. |

The generated-code surface is unusually V1-complete. The deferrals here are
measurement (CSS L4 ≤ 12s wall, BIR snapshot ≤ 5s) and the yaml smoke row
(provisional baseline owner = SYNTHESIS Wave-2 per `PASS-2.md:412`), not
architecture.

## §6 — `regex-automata` removal — codegen view

The user's mandate is that `regex-automata` is _not_ used at runtime;
parse-that owns regex. The V6 corpus surfaces three roles for `regex-automata`
that need disambiguating:

1. **Runtime regex execution.** Forbidden by the mandate. Production parsers
   call `parse-that/regex` only.
2. **Codegen-time DFA compilation.** PASS-2's regex-program payload says VM /
   lazy-DFA / full-DFA / prefilter selection happens at codegen time and is
   "proved by `parse-that/regex`" (`PASS-2.md:81`). DFA is _pre-compiled at
   codegen time_; the runtime executes the compiled program through
   `parse-that/regex` runtime, not through `regex-automata`.
3. **Test/oracle parity lane.** Topic 8 positions `regex-automata` as the
   oracle that proves bespoke `parse-that/regex` algorithms produce identical
   accept sets (`topic-8-simd-dfa.md:431-451`, `PASS-2.md:81`,
   `PASS-2.md:470`, `MASTER-PLAN.md:499` — `cargo test -p parse-that
   regex_automata_oracle`).

The user's mandate forces role 1 off and folds role 2 entirely into
parse-that. The mandate does not automatically force role 3 off — an oracle
lane is a development-time correctness check, not a runtime dependency.
However, PASS-2 must be auditable for whether `regex-automata` appears as a
production import path in any emitted parser.

| Question | Audit answer |
|---|---|
| Does PASS-2 currently emit code that imports `regex-automata`? | NO. The lowerer trees at `PASS-2.md:202-235` and `PASS-2.md:201-258` import only `ir::backend_ir::*` plus `parse-that/regex` runtime APIs. The verbatim deny command at `PASS-2.md:248-256` scans for `GrammarIR` only, but the import-deny floor at `PASS-2.md:240-247` requires the lowerer to consume BIR alone; the regex runtime call site is `parse-that` per `restart/README.md:62`. |
| Does PASS-2 emit code that imports `parse-that`? | YES, indirectly via the runtime template. `runtime/src/grammars/<name>/parser.rs` calls into the regex runtime owned by `parse-that/regex` whenever `RegexProgram` lowering fires (`ARCHITECTURE.md:935`, `PASS-2.md:65`). |
| What does `RegexProgram` lowering look like? | Codegen time: `parse-that/regex` parses the regex literal, produces a `RegexProgram` payload with execution-plan metadata (VM / lazy-DFA / full-DFA / prefilter), and the lowerer emits a call to the `parse-that/regex` runtime entry point. The DFA (when chosen) is pre-compiled at codegen time and serialized into the generated runtime; runtime execution is by `parse-that/regex`, never by `regex-automata`. |
| What does the user's `regex-automata` ban imply for the oracle lane? | The oracle lane is fixture-only. Implication: PASS-2 carries an audit obligation that the H.W2/H.W4 fixtures running `regex_automata_oracle` are compiled into `cfg(test)` or `dev-dependencies` of `parse-that` only, never into emitted runtime code. The check is `rg -n "regex_automata\|regex-automata" crates/runtime/src/ crates/codegen/src/lower/` returning zero outside `cfg(test)`. |

| Item | Current posture | Disposition |
|---|---|---|
| `parse-that/regex` as primary regex path | V1 (`README.md:62`, `PASS-2.md:81`). | FOLD-AS-IS. |
| `regex-automata` as runtime dependency | FORBIDDEN by user mandate; absent from V6 corpus runtime rows. | EXCLUDE — V1 already excludes. |
| `regex-automata` as codegen-time DFA compiler | NOT USED. DFA compilation happens inside `parse-that/regex` only. | EXCLUDE. |
| `regex-automata` as test/oracle lane | OPTIONAL; gated by user mandate interpretation. If "oracle" is a dev-only fixture lane behind `cfg(test)`, it survives; if "no use anywhere" is the read, the oracle lane is replaced by a self-consistent parse-that regression suite. | DEFER to user clarification. The harder reading folds `regex-automata` out entirely; the softer reading keeps it as `cfg(test)` only. PASS-2 must record the call before H.W4 measurement lands. |

The deferral here is not architectural. The architectural posture is settled:
`parse-that/regex` owns runtime regex; `regex-automata` is at most a dev-time
oracle. The deferral is whether the oracle lane survives at all, and that
needs a one-line user decision before H.W4.

## §7 — Function-value lowering — per-option analysis

Critical implementation question per the prompt: if first-class fns ever
arrive in BBNF, what does PASS-2's lowering look like? Three options surface
in standard compiler practice:

### §7.1 — Option 1: Monomorphise all uses (Rust-like)

| Aspect | Analysis |
|---|---|
| Mechanism | Each call site with a distinct `(fn_kind, capture_set, type_args)` instantiation is monomorphised into a fresh BIR rule. |
| `CostDecision` evidence shape | Reuses the `(RuleId, TypeArgs)` evidence shape at `PASS-2.md:374-377`. Each instantiation carries its own scalar score + objective vector. |
| BIR impact | No new variants; expands the `(RuleId, TypeArgs)` instance set. |
| Generated LOC | High variance. A grammar with deep callback chains can multiply LOC by the capture-set lattice. |
| Compile time | High; saturating monomorphisation. |
| Runtime | Fast; no indirection. |
| Tape identity preservation | Trivial; each monomorphisation owns its own tape contribution. |
| Risk | LOC budget blow-up; unbounded generic recursion under capture-polymorphism. |

### §7.2 — Option 2: Box and dispatch dynamically (vtable)

| Aspect | Analysis |
|---|---|
| Mechanism | Each fn value is a heap-allocated closure with a vtable; calls dispatch through the vtable. |
| `CostDecision` evidence shape | New `BoxedCallee` evidence kind needed; objective vector grows allocation pressure cost. |
| BIR impact | New variant `IndirectCall` (or extension of `CallHost` payload to carry `dispatch = Direct | Indirect`). |
| Generated LOC | Low; one dispatch path per fn type. |
| Compile time | Low. |
| Runtime | Slow; vtable lookup, allocation, indirect branch. The `restart/README.md:285-318` tape posture rejects per-call heap allocation as a hot-path pattern. |
| Tape identity preservation | Possible but requires every closure environment frame to carry tape identity, which conflicts with the bounded-rollback discipline at `ARCHITECTURE.md:1233-1237`. |
| Risk | Hot-path allocation; conflicts with simdjson-class throughput targets at `PASS-2.md:452-460`. |

### §7.3 — Option 3: Inline at codegen time when call site is known

| Aspect | Analysis |
|---|---|
| Mechanism | Closure form is admitted only when the call site is statically known at codegen time. The closure is inlined into the call site as straight-line BIR. |
| `CostDecision` evidence shape | Reuses existing scalar score + objective vector. The inline-vs-not decision is one Pareto point. |
| BIR impact | Zero. The closure is consumed at lowering before any new variant fires. |
| Generated LOC | Bounded; the closure body appears once per call site, like any other expression. |
| Compile time | Bounded; inline is a single rewrite. |
| Runtime | Fastest; no indirection, no allocation. |
| Tape identity preservation | Trivial; the inlined closure body emits tape contributions like any other expression. |
| Risk | Restricts the closure surface to bounded forms — which is exactly what `ARCHITECTURE.md:1187-1207` already does. |

### §7.4 — V1 recommendation

**Option 3.** This is what the V6 corpus already specifies. The four V1
closure forms (host chain, map, predicate, recovery) at `ARCHITECTURE.md:1187-1207`
are all inline-at-known-call-site lowerings. They produce no new BIR variant,
no environment heap-frame, no vtable. The "bounded captures" rule at
`ARCHITECTURE.md:1195-1198` is the precise restriction that makes option 3
sound: every closure has a known input shape (previous host result, matched
value, parser state, diagnostic context), and every closure has a known
lowering target (`HostChain`, `ValueProject`/`DirectBuild`, predicate inline,
`ErrorRecover`).

The recommendation is therefore: **do not introduce first-class fn values in
V1.** The four bounded closure forms cover every grammar-derived callback need
in the seed corpus (json, css, sheets, math, bbnf, bnf, csv, ebnf, css_pretty,
plus yaml). The architectural floor at `ARCHITECTURE.md:1189-1191` —
"intentionally narrow … without turning BBNF into a general programming
language" — is the right posture. If post-V1 work introduces general fn
values, the path is option 3 first (extend the closure inventory with one new
bounded form), option 1 only when the call site is unknown but the
instantiation set is finite, and option 2 never (vtable dispatch is
incompatible with the throughput targets at `PASS-2.md:452-460`).

The cost-model evidence binding for option 3: each inlined closure body
contributes to the parent rule's `CostDecision` exactly once. No new evidence
shape is required. The existing scalar + objective-vector record at
`PASS-2.md:374-377` covers the inlined contribution.

| Option | V1 fold | Future amendment |
|---|---|---|
| Option 1 (monomorphise) | EXCLUDE for fn values; KEEP for generic rules. | Possible if future BBNF admits unknown-call-site fn values with finite instantiation. |
| Option 2 (vtable) | EXCLUDE. | EXCLUDE — incompatible with throughput targets. |
| Option 3 (inline at known call site) | V1 — already implemented as the four-form closure inventory. | Path of any future fn-value extension. |

## §8 — Cross-cutting (with audits #1, #2, #5)

This audit is #6 in the lane. The cross-cutting concerns:

| Cross-cut | Carry | Disposition for PASS-2 |
|---|---|---|
| Audit #1 (type system) — first-class fns / higher-rank / GADTs | All EXCLUDED for V1 per `ARCHITECTURE.md:1161-1166`. | PASS-2 inherits the exclusion and emits no BIR variant for them. The four-form closure inventory is the V1 surface. |
| Audit #2 (extension surface) — block-bodied `@host fn`, generic rules, chains | All V1 per `MASTER-PLAN.md:330-360`. | PASS-2 lowers via `HostCall`, `HostChain`, `(RuleId, TypeArgs)` monomorphisation. No deferral. |
| Audit #5 (perf gates) — `Exact`/`Prefilter` SIMD, scalar parity, verifier-before-tape | V1 per `PASS-2.md:106`, `ARCHITECTURE.md:936`. | PASS-2 carries the gates. No deferral beyond H.W3 measurement. |
| Audit #5 → audit #6 — `regex-automata` posture | Oracle-only per Topic 8 §3; user mandate forces runtime exclusion. | PASS-2 must add a one-line audit that runtime emission contains zero `regex-automata` symbols; oracle lane survives only as `cfg(test)`. |
| Audit #1 → audit #6 — finite monomorphisation | PASS-1 produces; PASS-2 consumes. | Already gated at `PASS-2.md:417`. |

Testing / parity gates (asked by the prompt):

| Gate | V1 status | Disposition |
|---|---|---|
| BIR snapshot tests (`cargo xtask bbnf bir --all --check`) | V1 (`PASS-2.md:246-247`). | FOLD-AS-IS. |
| Regen-equality tests (`cargo xtask regen --check`) | V1 (`PASS-2.md:46`, `PASS-2.md:550` `BBNF-CODEGEN021`). | FOLD-AS-IS. |
| Per-grammar parity gates against fixtures | DEFER to BD.W4 / BD.W5 (`PASS-2.md:351`, `PASS-2.md:564`). | DEFER MEASUREMENT, not architecture. The 81-cell parity matrix is BD's load. |

## §9 — Recommended V1 folds (sorted by greenfield value)

This is the disposition tail. The audit recommends no architectural folds
beyond what the V6 corpus already binds. The rows below sort routed deferrals
by greenfield value — meaning, by how much the V1 surface gains by closing the
deferral now versus deferring it to a downstream tranche.

| Rank | Item | Current V1 status | Greenfield value of folding now | Recommended action |
|---|---|---|---|---|
| 1 | One-line user decision on `regex-automata` oracle lane survival | PENDING (§6 of this audit). | High — closes the only ambiguous regex-runtime row. Costs zero engineering work. | DECIDE before H.W4. If "no use anywhere," delete oracle lane and replace with a self-consistent `parse-that/regex` regression suite. If "dev-only `cfg(test)`," document the gate and move on. |
| 2 | `RegexDfa` ↔ `RegexProgram` rename canonicalisation | PASS-2 keeps `RegexDfa` as compatibility name; ARCH uses `RegexProgram`. | Medium — eliminates a documentation tax. | DEFER to SYNTHESIS reconciliation per `fold-pass-2.md:145`; PASS-2 cannot re-own alphabet. |
| 3 | E-graph rewrite-budget implementation detail | DEFER to C.W4/C.W5 per `HARDENING-CONSOLIDATED-V6.md:177`. | Medium — closes a measurement risk. | DEFER. The seven-category set is V1; budgets are tranche-implementation. |
| 4 | `CostDecision` `ObjectiveMode` selection policy | Evidence shape V1; mode policy implementation. | Low — V1 lowerers consume the evidence shape; the mode is an extraction-time policy. | DEFER. |
| 5 | yaml smoke regen baseline (≤ 4s wall) | Provisional, owner = SYNTHESIS Wave-2 per `PASS-2.md:412`. | Low — a row in the perf budget table. | DEFER to first onboarding execution. |
| 6 | H.W3 WASM `{N}`/`{M}` measured values | Placeholders per `MASTER-PLAN.md:479`. | Low — measurement-only; cannot be folded earlier without measurement. | DEFER to H.W3. |
| 7 | TS production lowerer | DEFER per `PASS-2.md:563`. | Negative for V1 — folding now triples parity matrix and re-introduces the 27-cell BC.W2 carry without throughput coverage that Rust+WASM does not deliver. | KEEP DEFERRED. |
| 8 | First-class fn values / higher-rank / GADTs | EXCLUDED per `ARCHITECTURE.md:1161-1166` and `ARCHITECTURE.md:1187-1207`. | Negative for V1 — admits a programming-language surface that is not in the seed grammar corpus. | KEEP EXCLUDED. |
| 9 | Vtable closure dispatch (option 2 in §7) | EXCLUDED implicitly. | Negative for V1 — incompatible with throughput targets at `PASS-2.md:452-460`. | KEEP EXCLUDED. |
| 10 | Profile-guided specialization | Unrouted in MASTER-PLAN. | Negative for V1 — requires a measurement loop that does not exist in V1. | DEFER post-V1. |

### §9.1 — Posture summary

V1 codegen is dense. The PASS-2 surface already binds:

- 23-variant Backend IR with payload refinement (no re-ownership).
- One Rust V1 lowerer + one WASM V1 lowerer (wasm32 binding) + TS scaffold.
- One generic monomorphisation surface gated on PASS-1 finite-instance proof.
- One `RegexProgram` payload contract with VM/lazy-DFA/full-DFA/prefilter
  execution plans, owned by `parse-that/regex`.
- One `SimdScan` payload with `Exact` (scalar-parity) and `Prefilter`
  (verifier-before-tape) modes.
- One `PrattSpine` payload, auto-detected only.
- One four-form closure inventory (host chain, map, predicate, recovery), all
  inlined at known call sites — option 3 of §7.
- Seven V1 e-graph rewrite categories.
- Generated runtime, visitor, path-schema, host-table, materialisation-cost,
  and bench-report artefacts.
- Regen equality, BIR snapshot, and LOC budget close gates.

The only architectural deferrals that meaningfully affect lowering are:

1. The `regex-automata` oracle-lane survival decision (§6).
2. The `RegexDfa`/`RegexProgram` rename, deferred to SYNTHESIS.

Everything else is measurement deferral (yaml baseline, H.W3 WASM, BD.W5
parity) or correctly-excluded surface (TS production, first-class fns,
vtable dispatch, GADTs, profile-guided specialization).

The load-bearing implementation question — function-value lowering — has the
right answer in the V6 corpus. The four-form bounded closure inventory is
option 3 from §7, lowered inline at known call sites. No new BIR variant, no
environment heap-frame, no vtable. The architectural floor at
`ARCHITECTURE.md:1187-1207` is the right floor; the V1 cost-model evidence
shape at `PASS-2.md:374-377` carries the inlined contribution without
extension.

Codegen audacity meets reality at exactly one row: the regex-automata oracle
decision. That row is the only V1 fold this audit recommends closing now.
