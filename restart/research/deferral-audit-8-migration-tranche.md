# Deferral audit #8 — Migration + Master-Plan + Tranche residue

Audit lane: V6-READY corpus; greenfield mandate; deferral-audit lane #8
(retry of the prior 600s-stalled dispatch). The cluster owned is the executable
plan layer — `restart/MASTER-PLAN.md`, `restart/MIGRATION.md`, the V6 hardening
residue (`HARDENING-CONSOLIDATED-V6.md` §5/§8/§10), and the legacy BA-BD
inheritance map (`restart/inheritance/INDEX.md`). Sibling audits #1-#7 own the
type-system, function/value, BBNF surface, sibling crates, runtime, and
codegen/lowering deferrals; this audit's surface is the *plan* — the tranche
sequence, carry ledger, cookbook ledger, R1-R7 residue, and inheritance map —
asked from one question: **which rows are work-deferral, and which rows hide
an architectural prerequisite the greenfield mandate should fold before
per-tranche drafting begins?**

The closing posture: most plan-layer rows are honest work-deferral routed by
receiver, blocker, and gate. A small set are architectural prerequisites
masquerading as carry rows — items where the *plan shape* is itself the
artefact under-specified, and where deciding now means tranches do not
re-litigate. The audit returns those folds, sorted by greenfield value, plus
re-sequencing implications if sibling audit folds land.

---

## §1 — Scope and corpus references

### §1.1 Required reading walked

| Surface | Path | Lines |
|---|---|---:|
| Master plan executive + verdict ledger | `restart/MASTER-PLAN.md` | 1-105 |
| Tranche set + calendar + outputs + yaml trajectory | `restart/MASTER-PLAN.md` | 152-225 |
| Per-tranche stub waves (A-J) | `restart/MASTER-PLAN.md` | 226-569 |
| Schema/commit/timeline/archive/LOC trajectory | `restart/MASTER-PLAN.md` | 571-693 |
| Lock ownership + docs + risks | `restart/MASTER-PLAN.md` | 695-760 |
| **Carry and friction ledger §24** | `restart/MASTER-PLAN.md` | 762-806 |
| **Cookbook friction ledger §25** | `restart/MASTER-PLAN.md` | 793-806 |
| Implementation order + close | `restart/MASTER-PLAN.md` | 808-844 |
| Migration disposition alphabet | `restart/MIGRATION.md` | 30-61 |
| **§3 Current crates → Restart crates + crosswalk** | `restart/MIGRATION.md` | 62-273 |
| Migration tranche sequence | `restart/MIGRATION.md` | 665-686 |
| Migration gates 19.1-19.7 | `restart/MIGRATION.md` | 704-789 |
| **§20 Unresolved migration punch list** | `restart/MIGRATION.md` | 790-801 |
| V6 cohort verdict + residue R1-R7 | `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md` | 36-198 |
| V6 topic-ownership matrix | `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md` | 308-332 |
| V6 verification checklist | `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md` | 333-357 |
| V6 V5.1→V6 history | `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md` | 358-370 |
| Inheritance map BA-BD | `restart/inheritance/INDEX.md` | 1-73 |
| Sibling audit #1 (type system) | `restart/research/deferral-audit-1-type-system.md` | 1-393 |
| Sibling audit #2 (function/value) | `restart/research/deferral-audit-2-function-value-system.md` | 1-442 |
| Sibling audit #3 (BBNF surface) | `restart/research/deferral-audit-3-bbnf-surface-directives.md` | 1-359 |
| Sibling audit #4 (sibling crates) | `restart/research/deferral-audit-4-sibling-crates.md` | 1-335 |
| Sibling audit #5 (runtime/PASS-3) | `restart/research/deferral-audit-5-runtime-pass3.md` | 1-270 |
| Sibling audit #6 (codegen/PASS-2) | `restart/research/deferral-audit-6-codegen-pass2.md` | 1-458 |

### §1.2 Audit posture

The plan layer's deferrals fall into three classes:

1. **Routed work-deferral** — receiver, blocker, gate are present, and the row
   describes work the architecture has already settled. Tranches consume these
   without reopening. No fold.
2. **Architectural prerequisite hiding as carry** — the row references a
   contract the architecture has *not* settled, and the implementing tranche
   would have to settle it before the row's work can begin. Fold candidate.
3. **Sibling-audit cross-cut** — the row references a surface that audits #1-#6
   propose to fold; the plan row's receiver may shift if the fold lands.
   Re-sequence candidate.

The user's mandate makes class 2 the load-bearing question. A V1 that defers
architectural shape into "implementation" risks the failure mode the V6
hardening already names: "stub-level plan might need full tranche specs before
readiness" (`HARDENING-CONSOLIDATED-V6.md:243`). Class 2 folds turn implicit
architecture decisions into explicit master-plan or migration rows.

---

## §2 — Per-tranche residue (A-J)

Each tranche carries between two and five identifiable deferral-pressure
points. The disposition column records the audit verdict per row; rows marked
**SETTLED** are honest work-deferral, rows marked **FOLD** route to §8.

### §2.1 — Tranche A (Workspace Genesis)

| Item | Path:line | Pressure | Disposition |
|---|---|---|---|
| 24-crate skeleton names + prefix rule | `MASTER-PLAN.md:245` | Cross-checked against ARCHITECTURE §1; row spells out unprefixed list. | SETTLED. |
| Metadata schema for current nine grammars | `MASTER-PLAN.md:246` | Schema is implicit ("validates current nine"); architecture should pin the schema not the count. | **FOLD §8.A1**: pin the metadata schema as ARCHITECTURE prose, not a tranche-A discovery. |
| Generalisation lint coverage | `MASTER-PLAN.md:248`, `MIGRATION.md:704-714` | Three regexes (`bbnf-strategy`, `PRODUCTION_MANIFEST_TABLE`, etc.); no standing lint manifest. | **FOLD §8.A2**: lift the lint manifest into ARCHITECTURE so tranche A consumes a contract, not designs one. |
| Declaration-crate review form | `MASTER-PLAN.md:771` | Eight fields named at consolidation level (`HARDENING-CONSOLIDATED-V6.md:355`); no template stored. | **FOLD §8.A3**: store the template at ARCHITECTURE §13 or a pinned `restart/templates/` slot so A.W4 consumes it. |
| Archive destination for ser/gorgeous | `MASTER-PLAN.md:780` | "outside production workspace" — *which* destination? `archive/<crate>/`, `restart-archive/`, top-level `archive/`? | SETTLED in `MIGRATION.md:271` ("`restart-archive`/legacy reference only"). Cross-doc, but not architecture-pending. |

### §2.2 — Tranche B (Runtime Substrate)

| Item | Path:line | Pressure | Disposition |
|---|---|---|---|
| Tape/direct one-identity API | `MASTER-PLAN.md:277-281` | One snapshot identity is settled; concrete API surface (return types, lifetime parameters, `BorrowType` shape on `DocumentView`) lives in PASS-3 only. | SETTLED. PASS-3 §6 owns; tranche consumes. |
| Direct builder shell ↔ ShapeFacts contract | `MASTER-PLAN.md:280, 314` | C.W2 says "Direct-builder shell contract consumes ShapeFacts in a C fixture and records B integration gaps." Records gaps — but who closes them? | **FOLD §8.B1**: name the close gate for B/C shape-fact integration explicitly; avoid "records gaps" as terminal disposition. |
| `parse`, `parse_in`, `parse_owned` lifetime contract | `MASTER-PLAN.md:707` (Lock 9) | Lock proof exists but the actual three signatures are not in any of the trio. | SETTLED via PASS-3; cross-cut with sibling audit #5. |

### §2.3 — Tranche C (IR + Optimisation Core)

| Item | Path:line | Pressure | Disposition |
|---|---|---|---|
| HM principal-scheme core (W1) | `MASTER-PLAN.md:313` | Names HM + check + finite CSP; the *algorithm choice* (rank-1 vs DK13) is settled rank-1 by V6 (`HARDENING-CONSOLIDATED-V6.md:131`). | Cross-cut with sibling audit #1 §2.1; if §1 §2.1 folds DK13, **re-sequence** C.W1 algorithm scope (§6). |
| ShapeFacts and value-shape mining (W2) | `MASTER-PLAN.md:314` | Shape miner shape is settled but the miner-as-peer-of-recogniser-miners refactor sibling audit #1 §2.10 proposes is not in the plan. | Cross-cut with sibling audit #1 §2.10; **re-sequence** C.W2 if §1 folds. |
| RecognizerFacts feed E (W3) | `MASTER-PLAN.md:315` | "Facts feed E-owned BIR snapshots, not placeholder hints." Settled. | SETTLED. |
| CSP/egraph bridge tables (W4) | `MASTER-PLAN.md:316` | Six concrete sub-items: stable IDs, monotone exchange, rewrite guard, **rewrite budget**, representative stability, justification records. R5 (`HARDENING-CONSOLIDATED-V6.md:177`) routes rewrite-budget detail to "Tranche C/E implementation specs" — i.e., not yet specified. | **FOLD §8.C1**: rewrite-budget categories, node/iteration limits, and acceptance thresholds belong in ARCHITECTURE §10 before C.W4, not in C.W4 implementation spec. |
| `CostFacts` + frontier extraction (W5) | `MASTER-PLAN.md:317` | Settled via Topic 5 fold (`HARDENING-CONSOLIDATED-V6.md:135`). | SETTLED. |

### §2.4 — Tranche D (BBNF Extension Surface)

| Item | Path:line | Pressure | Disposition |
|---|---|---|---|
| Lookbehind parser + bounds checker (W0) | `MASTER-PLAN.md:346` | Settled. | SETTLED. |
| Generic rules + monomorphisation set (W1) | `MASTER-PLAN.md:347` | Finite `(RuleId, TypeArgs)` proof is settled; sibling audit #2 F1-F9 proposes function-arrow type, function values, function-typed parameters, lambdas, capture, composition, partial application, pattern-match, tuple values — **none in the V6 surface**. | Cross-cut with sibling audit #2; if F1-F9 fold, D.W1 absorbs the additional grammar productions and the C.W1 type-checker handles them. **Re-sequence** D.W1 (§6). |
| Block-bodied `@host fn` (W2) | `MASTER-PLAN.md:348` | Settled. | SETTLED. |
| Multi-function chaining (W3) | `MASTER-PLAN.md:349` | Settled in narrow form; sibling audit #2 F6 (composition operator) and F7 (partial application) propose extensions. | Cross-cut; if §2 folds, D.W3 expands. |
| `@error`, `@layout`, regex Unicode routing, rewrite rejection (W4) | `MASTER-PLAN.md:350` | Settled. | SETTLED. |
| Standalone `@recover` migration alias | `HARDENING-CONSOLIDATED-V6.md:345` | "Positive standalone `@recover` count must be zero" — this is settled. | SETTLED. |

### §2.5 — Tranche E (Backend IR + VM)

| Item | Path:line | Pressure | Disposition |
|---|---|---|---|
| 23-variant Backend IR enum + validation (W0) | `MASTER-PLAN.md:380` | Settled at PASS-2 §52-76. | SETTLED. |
| Grammar IR + side tables → BIR builder (W1) | `MASTER-PLAN.md:381` | Settled. | SETTLED. |
| VM core control flow (W2) | `MASTER-PLAN.md:382` | Settled. | SETTLED. |
| VM tape/direct, host, path, recovery, debug marks (W3) | `MASTER-PLAN.md:383` | "Replays all BIR variants" — broad scope; debug-mark contract is not pinned. | **FOLD §8.E1**: pin the debug-mark contract (BIR variant `DebugBreak`/`Trace`) in ARCHITECTURE §7.2 so VM and lowerers consume the same alphabet. |
| Lowerer trait + boundary tests (W4) | `MASTER-PLAN.md:384` | Settled via Lock 5. | SETTLED. |

### §2.6 — Tranche F (Rust Lowerer + Runtime Template)

| Item | Path:line | Pressure | Disposition |
|---|---|---|---|
| Rust lowerer skeleton (W0) | `MASTER-PLAN.md:411` | Settled. | SETTLED. |
| Tape/direct emit + builder integration (W1) | `MASTER-PLAN.md:412` | Settled. | SETTLED. |
| Host calls/chains, layout, `@error(recover = ...)` (W2) | `MASTER-PLAN.md:413` | Settled. | SETTLED. |
| Generated module template + headers (W3) | `MASTER-PLAN.md:414, 789` | Migration row 789: "generated header omits grammar, metadata, or Backend IR hashes." The header *fields* are listed; the *file layout* is not. | SETTLED via PASS-2 §7. Cross-doc lookup. |
| Generated LOC budget tooling (W4) | `MASTER-PLAN.md:415, 663-685` | Settled with per-grammar baselines; yaml provisional row is honest. | SETTLED. |
| Nine seed grammar regeneration (W5) | `MASTER-PLAN.md:416` | Settled. | SETTLED. |

### §2.7 — Tranche G (Path, Value, Visitor)

| Item | Path:line | Pressure | Disposition |
|---|---|---|---|
| `path-core` AST/parser/evaluator (W0) | `MASTER-PLAN.md:444` | Settled. | SETTLED. |
| Rust `pointer!` and `select!` (W1) | `MASTER-PLAN.md:445` | Settled; sibling audit #3 proposes `pointer!` → `path!` rename. **Cross-cut**; if §3 folds, all `BBNF-POINTER*` codes rename to `BBNF-PATH*` and the master-plan row updates verbatim. | Cross-cut with sibling audit #3; **re-sequence** §6. |
| `ValueRef`, `ValueOwned`, shape-backed projection (W2) | `MASTER-PLAN.md:446` | Settled via PASS-3 §6. | SETTLED. |
| Read-write visitor mutation (W3) | `MASTER-PLAN.md:447` | Settled. | SETTLED. |
| `path-ts` schema + future grammar test (W4) | `MASTER-PLAN.md:448` | Settled. | SETTLED. |

### §2.8 — Tranche H (Pratt, SIMD, WASM)

| Item | Path:line | Pressure | Disposition |
|---|---|---|---|
| Pratt + BIR `PrattSpine` (W0) | `MASTER-PLAN.md:476` | Settled via Topic 8 fold. | SETTLED. |
| SIMD `Exact`/`Prefilter` + verifier-before-tape (W1) | `MASTER-PLAN.md:477` | Settled. | SETTLED. |
| AVX2/NEON/scalar dispatch (W2) | `MASTER-PLAN.md:478` | Settled. | SETTLED. |
| **WASM V1 + lightning-css comparison (W3)** | `MASTER-PLAN.md:479`, R6 (`HARDENING-CONSOLIDATED-V6.md:178`) | `{N}` and `{M}` placeholders carry owner+blocker; R6 is honest measurement-deferral. The *ABI matrix* at `MASTER-PLAN.md:485-491` is settled. | SETTLED. The placeholder is a measurement, not architecture. |
| Early JSON SOTA gates (W4) | `MASTER-PLAN.md:480` | Settled with concrete numerics. | SETTLED. |
| Early CSS SOTA gates (W5) | `MASTER-PLAN.md:481` | Settled. | SETTLED. |

### §2.9 — Tranche I (Recovery, Incremental, LSP)

| Item | Path:line | Pressure | Disposition |
|---|---|---|---|
| RecoveryFacts + diagnostic codes (W0) | `MASTER-PLAN.md:522` | Settled. | SETTLED. |
| Incremental snapshots, snapshot-scoped `TapeId`, reuse maps (W1) | `MASTER-PLAN.md:523` | Settled via Topic 7 fold. | SETTLED. |
| LSP diagnostics + semantic index (W2) | `MASTER-PLAN.md:524` | Settled. | SETTLED. |
| Debug/replay + playground hooks (W3) | `MASTER-PLAN.md:525` | "VM trace displayed through server/debug API" — server API surface is implicit. Sibling audit #5 may pin DAP shape. | Cross-cut with sibling audit #5; **re-sequence** if §5 folds DAP. |
| CLI/LSP parity for diagnostics (W4) | `MASTER-PLAN.md:526` | Settled. | SETTLED. |

### §2.10 — Tranche J (Parity, Docs, Publication Close)

| Item | Path:line | Pressure | Disposition |
|---|---|---|---|
| Cross-backend parity matrix (W0) | `MASTER-PLAN.md:553` | Settled. | SETTLED. |
| Final SOTA gate + benchmark report (W1) | `MASTER-PLAN.md:554` | Settled. | SETTLED. |
| Public docs redo (W2) | `MASTER-PLAN.md:555` | Cookbook pages enumerated in §25 (`MASTER-PLAN.md:797-806`); per-page contract not pinned. | **FOLD §8.J1**: per-cookbook-page table-of-contents (sections, code samples, diagnostic codes, gate command) lifted into a `restart/templates/cookbook-page.md` shape so J.W2 produces uniform pages, not seven varieties. |
| Package readiness + Lock 11 publication split (W3) | `MASTER-PLAN.md:556` | Settled with two-gate split. | SETTLED. |
| Archive + migration audit (W4) | `MASTER-PLAN.md:557` | Settled. | SETTLED. |
| Restart close report (W5) | `MASTER-PLAN.md:558` | Settled. | SETTLED. |

### §2.11 — Tranche residue summary

Of fifty-three identified rows, forty-five are **SETTLED** (work-deferral with
gate present), four are **FOLD** candidates (§8.A1, §8.A2, §8.A3, §8.C1,
§8.E1, §8.J1 — six rows across four tranche-local fold targets), and four are
**cross-cut** with sibling audits (C.W1, C.W2, D.W1, D.W3, G.W1, I.W3 — six
rows; re-sequencing in §6).

---

## §3 — Carry ledger §24 fold candidates

`MASTER-PLAN.md` §24 carries 23 rows (lines 762-792) plus 7 cookbook rows
(lines 793-806). Per-row disposition:

| # | Item | Receiver | Blocker | Gate | Source-side | Disposition |
|---:|---|---|---|---|---|---|
| 1 | Declaration-crate escape valve | A/D | Review form fields | Metadata validator | synthesis + migration | **FOLD §8.A3** — the form is named at consolidation but not stored. Lift to ARCHITECTURE §13 template. |
| 2 | Layout lowering | D/F | LayoutFacts + BIR `LayoutPush`/`LayoutPop` | Replay test | synthesis | SETTLED. |
| 3 | Cursor skip | B/H | Empty-path proof | `__EAGER_EMPTY_PATH` fixtures | synthesis + migration | SETTLED. |
| 4 | PASS-3 consumers | F/G/I | Generated runtime omissions | Consumer smokes | synthesis | SETTLED. |
| 5 | SOTA metadata | H/J | Bench metadata schema | Schema rejects incomplete | synthesis + migration | SETTLED. |
| 6 | Cost evidence | C/F/H/J | Selected-only loses provenance | `cost-model` evidence report | synthesis | SETTLED. |
| 7 | Regex oracle lane | D/H/J | Bespoke regex without grammar-owned delta | `parse-that/regex` parity | synthesis | SETTLED. |
| 8 | Runtime materialisation metadata | B/F/J | Direct/tape rows hide cost class | Generated materialisation report | synthesis | SETTLED. |
| 9 | yaml onboarding | A/F/G/J | Manual Rust edit | Two-surface proof | synthesis | SETTLED. |
| 10 | Archive closure | A/J | `ser`/`gorgeous` in workspace | Membership check | synthesis + migration | SETTLED. |
| 11 | TS production | G/I/J | TS path emitter naming grammars | `path-ts` schema dump | synthesis | SETTLED. |
| 12 | BD parity | F/J | Parity matrix not run | `cargo xtask parity --all` | synthesis | SETTLED. |
| 13 | PASS-1 reconciliation | C/D | Architecture §7 vs PASS-1 §2 drift | Schema match | synthesis | SETTLED. |
| 14 | PASS-3 API docs | G/I/J | Public docs missing diagnostics | Cookbook close | synthesis | SETTLED. |
| 15 | Publication readiness | A/J | Crate names/license/deps | Dry-run publish | synthesis | SETTLED. |
| 16 | Fixture handoff | A/G/J | Duplicate fixtures | Fixture audit at J.W4 | synthesis | SETTLED. |
| 17 | `path-ts` schema | G | Same `path-core` semantics | Schema dump round-trip | synthesis | SETTLED. |
| 18 | WASM ABI | H/J | V1 binding ABI | H.W3 ABI matrix + J.W3 dry-run | synthesis + migration | SETTLED. |
| 19 | Generated header fields | F | Header omits hashes | `lint-generated-headers` | migration | SETTLED. |
| 20 | `path-ts` package publication timing | J | Forced before parity | J.W3 sequence | migration | SETTLED. |
| 21 | PASS-2 BIR snapshots | E/F | Snapshots outside `ir::backend_ir` | `BBNF-GEN001` deny gate | migration | SETTLED. |

The §24 ledger is **healthy**: only the declaration-crate escape valve (row 1)
exposes a missing artefact (the eight-field review form). All other rows have
receiver, blocker, gate, and source-side cited.

Cookbook rows (`MASTER-PLAN.md:797-806`) are §4 territory.

---

## §4 — Cookbook §25 deferrals

The §25 cookbook ledger covers seven friction rows. Each names a target user,
a mental model, a confusion point, the resolving artefact, and the diagnostic.

| # | Friction | Resolving artefact | Pinned? | Disposition |
|---:|---|---|---|---|
| 1 | `pointer!` and `select!` | `cookbook/path-pointer.md` + `path-ts` schema dump | named, not drafted | **FOLD §8.J1** — page contract template. |
| 2 | Lifetime constructors | `cookbook/parse-lifetimes.md` + `runtime` API doc | named, not drafted | FOLD §8.J1. |
| 3 | Visitor mutation | `cookbook/visitor-mutation.md` + PASS-3 visitor contract | named, not drafted | FOLD §8.J1. |
| 4 | Layout errors | `cookbook/layout.md` | named, not drafted | FOLD §8.J1. |
| 5 | Pratt/SIMD decisions | `cookbook/recognizers.md` + `cargo xtask explain-recognizer` | named, not drafted | FOLD §8.J1. The `explain-recognizer` xtask is also **architecturally implicit** — no row in `MASTER-PLAN.md` §16 owns it. **FOLD §8.J2**: pin `cargo xtask explain-recognizer` in MASTER-PLAN §16 schema-handoff with owning tranche H/J. |
| 6 | Crate split migration | `cookbook/migration-crate-split.md` + MIGRATION §3.1 | named, not drafted | FOLD §8.J1. |
| 7 | Adding yaml | `cookbook/add-grammar.md` + Architecture §12.1 | named, not drafted | FOLD §8.J1. |
| 8 | yaml syntax error | recovery cookbook + `DocumentSnapshot` trace | named, not drafted | FOLD §8.J1. |

The §25 ledger is **deferred-for-content** (each page is a writing task) for
seven rows. One row (#5, `explain-recognizer` xtask) is **deferred-for-
architecture** because the xtask itself is named in the cookbook but not
catalogued in any tranche's outputs. That is §8.J2.

---

## §5 — V6 R1-R7 architectural prerequisites

The V6 hardening residue (`HARDENING-CONSOLIDATED-V6.md` §5) lists seven
non-blocking residual items. Each is audited for hidden architectural prereq.

| R# | Topic | Receiver | Class | Architectural prereq? |
|---:|---|---|---|---|
| R1 | Research-index + bibliography hygiene | `restart/research/INDEX.md` | Pure work-deferral (source classification). | **None.** Closed by R1/R2 pass. |
| R2 | README precision | `restart/README.md` | Closed by R1/R2 pass. | **None.** |
| R3 | Hardening command-harness precision | `restart/audit/hardening/HARDENING-CONSOLIDATED.md` | Closed by R3/R4 pass. | **None.** |
| R4 | Lock 4 rationale (egglog fusion pressure) | `restart/locks/LOCKS.md` | Closed by R3/R4 pass. | **None.** |
| R5 | Rewrite-budget implementation detail | C.W4/C.W5 implementation specs | Routed; tests do not yet exist. | **YES.** The *categories*, *node/iteration limits*, and *acceptance thresholds* are architectural. **FOLD §8.C1**: lift to ARCHITECTURE §10 (optimization) before C.W4 begins, not as test-shape during. |
| R6 | H.W3 WASM placeholders `{N}`, `{M}` | H.W3 measurement | Pure measurement-deferral. | **None.** The placeholders are unmeasured numbers, not unmade decisions. |
| R7 | Per-tranche full-spec drafting | Next drafting phase | Synthesis intentionally ends at A-J stub. | **None directly.** But every architectural-prereq fold from this audit (§8.A1, §8.A2, §8.A3, §8.C1, §8.E1, §8.J1, §8.J2) **reduces R7's per-tranche reconsideration cost** by lifting decisions out of tranche-local drafting. R7 is the **container** that the §8 folds shrink. |

R5 is the only direct R-row architectural fold. R7 is structural — the
§8 folds *change the shape* of what R7 must produce, not its existence.

---

## §6 — Tranche re-sequencing implications

Sibling audits #1-#3 propose folds whose receivers are tranche C, D, F, G.
The plan rows above name those tranches; if the folds land, the *contents*
of those waves expand. The question is whether tranche **ordering** is
affected.

### §6.1 — If sibling audit #1 §2.1 folds (DK13 higher-rank algorithm)

| Receiver | Plan row impact | Re-sequence? |
|---|---|---|
| C.W1 | Algorithm changes from rank-1 HM to DK13 ordered contexts. ~600-1200 LOC at `passes/types/`. | **No** — same wave; expanded scope. |
| D.W1 | Generic-cycle proof unchanged; DK13 absorbs uniformly. | No. |
| D.W3 | Chain expected-flow uses DK13 application judgment. | No. |

The fold *adds work to C.W1* but does not change tranche sequence.

### §6.2 — If sibling audit #1 §2.10 folds (Schema-mining miner)

| Receiver | Plan row impact | Re-sequence? |
|---|---|---|
| C.W2 | New miner sibling to `ShapeFacts`; ~300-500 LOC at `passes/recognizers/` or `passes/shapes/`. | **No** — same wave; new sibling. |
| F.W3 | Generated record names become inferred where shapes are stable. | No. |

Same posture: add scope, not re-order.

### §6.3 — If sibling audit #2 F1-F9 fold (function values, lambdas, match, tuples)

| Receiver | Plan row impact | Re-sequence? |
|---|---|---|
| D.W0-W4 | Adds five productions (`FnType`, `FnRef`, `Match`, `Pattern`, `Tuple`) to BBNF surface; one or two new IR variants per addition. | **Yes** — D.W0 (lookbehind) becomes the smallest of D's waves; the additions warrant a new D.W5 (function/match/tuple surface) so D's five waves grow to six. |
| C.W1 | Function-arrow type unification + bidirectional check on closures and matches. Composes naturally with §6.1 if both fold. | No. |
| F.W1 | Codegen for `Match` and closure capture through tape lifetimes. | No. |

D's wave count grows from 5 → 6. The §5.1 calendar matrix (`MASTER-PLAN.md:182`)
is dependency-ordered, not wall-clock; the row count change is documentation
only.

### §6.4 — If sibling audit #3 `pointer!` → `path!` rename folds

| Receiver | Plan row impact | Re-sequence? |
|---|---|---|
| G.W1 | `path!` macro instead of `pointer!`. Diagnostics rename `BBNF-POINTER*` → `BBNF-PATH*`. | **No** — verbatim rename. |
| §24/§25 ledgers | Cookbook page renames `path-pointer.md` → `path-macro.md` or similar. | No. |
| ARCHITECTURE §7.4 | Diagnostic vocabulary table renames. | No. |

Pure rename; no re-order.

### §6.5 — If sibling audit #4 retires `regex-automata`

| Receiver | Plan row impact | Re-sequence? |
|---|---|---|
| D.W4 | Regex Unicode routing already lives in `parse-that/regex`. | No. |
| H.W1 | Regex oracle parity uses `parse-that/regex` end-to-end instead of `regex-automata`. | **Yes** — H.W1 adds a parity-against-bbnf-regex-oracle step before scanner integration; doesn't reorder waves but expands the same-wave consumer gate. |

Tranche H's wave count is unchanged; H.W1's gate text expands.

### §6.6 — Aggregate re-sequencing verdict

If **all** sibling audit folds land:

| Tranche | Wave count change | Calendar slot change |
|---|---|---|
| A | 5 → 5 (lift to architecture) | unchanged. |
| B | 5 → 5 | unchanged. |
| C | 6 → 6 (algorithm replacement, not new wave) | unchanged. |
| D | 5 → **6** (new function-surface wave) | unchanged (D's slot is "calendar 4"). |
| E | 5 → 5 | unchanged. |
| F | 6 → 6 | unchanged. |
| G | 5 → 5 (rename) | unchanged. |
| H | 6 → 6 (W1 gate expanded) | unchanged. |
| I | 5 → 5 | unchanged. |
| J | 6 → 6 | unchanged. |

**No tranche calendar slot moves**; only D's wave count grows. Sibling audit
folds are absorbed by waves within tranches, not by re-ordering tranches.
This confirms the existing A-J plan as resilient to fold pressure — the
greenfield discipline that named "implementation order is dependency, not
calendar" (`MASTER-PLAN.md:177`) is load-bearing here.

---

## §7 — Inheritance / BA-BD reconsideration

`restart/inheritance/INDEX.md` maps legacy plan-set BA-BD (~18,200 lines) to
new tranches A-J (line 29-40). The legacy waves were drafted before Lock 14
codification (line 51); legacy survival is selective.

### §7.1 — Items routed forward that V1 should reconsider

| Legacy row | New tranche | V1 fold candidate? |
|---|---|---|
| BA W3a/b/c (path triplet rename + path-core extraction) | A | SETTLED via Lock 7. |
| BA W5a-e (per-grammar direct-to-struct migration) | F | SETTLED — convergent pivot. |
| BB W1a/b/c (CSS L4/BBNF/Sheets — REDUNDANT under option (a)) | F (folded) | SETTLED — already folded into F. |
| BB W3a/b/c (CSP layout + e-graph + miners + Pratt + SIMD detection) | C | SETTLED. |
| BB W4a/b (slice-borrow + escape hatches; lifetime cookbook) | G | SETTLED via PASS-3 §6. |
| BB W5a (pointer + LazyValue) | G | **CROSS-CUT** with sibling audit #3 rename + sibling audit #5 LazyValue routing. |
| BC W0a-c (typed IR contract; Rust lowerer smoke; AscentStrategy) | E | SETTLED. |
| BC W1a/b (full Rust emitter refactor + regen-equality) | F | SETTLED. |
| BC W2 (TS+WASM emitter scaffolds; deferred activation) | H | SETTLED. |
| BC W5a-d (sister-crate API freeze; bbnf-regex endpoint; parse-that disposition; worktree fixture) | I | **CROSS-CUT** with sibling audit #4 (sibling crates). If audit #4 retires `regex-automata`, BC W5b "bbnf-regex endpoint reconciliation" is **already settled** (the option-A rename happened); the row is past tense. |
| BD W0/W1 (TS proc-macro + TS runtime — deferred per Q28) | H | The deferral is settled per Q28; sibling audit #5 confirms TS scope-deferred. |
| BD W2 (WASM compilation pipeline) | H | SETTLED. |
| BD W3-W6 (sister-crate publication + worktree fixture + parity + close) | J | SETTLED. |

### §7.2 — What does NOT inherit (settled deletions)

The "What does NOT inherit" list (`INDEX.md:58-67`) is settled:

| Settled deletion | V1 fold? |
|---|---|
| Per-grammar declaration crates | SETTLED. |
| `bbnf-` prefix on internal crates | SETTLED. |
| 22-variant Backend IR table as final (refined to 23) | SETTLED. |
| "Convergent pivot at Tranche E" framing (sharpened to staggered closures) | SETTLED. |
| **Tape rebranding moratorium under Lock 1** (`INDEX.md:66`) | **WATCH** — `INDEX.md:66` says "tape's *name* dies; tape's *structural insight* survives as **ParseStream**." This is **stale** under V6 (`HARDENING-CONSOLIDATED-V6.md` consistently uses tape as the substrate name; `MASTER-PLAN.md:24` "Tape is the substrate name"). The `INDEX.md` row reflects pre-V6 reasoning. **FOLD §8.I1**: amend `INDEX.md:66` to record the V6-final disposition (tape stays tape; ParseStream is `syn::ParseStream` archaeology only) so future inheritance consultations are not misled. |
| Two-stage hardening protocol | SETTLED. |

### §7.3 — Inheritance reconsideration verdict

One row (§7.2 ParseStream) is a documentation drift fix. All other inheritance
rows are settled per Lock 14 + V6 consolidation. Legacy BA-BD waves do not
introduce new V1 fold candidates beyond what sibling audits #1-#6 already
proposed.

---

## §8 — Recommended V1 folds (sorted by greenfield value)

The folds are sorted by architectural value. Each carries: target file:line,
fold proposal, implementation impact, risk, greenfield value, recommendation.

### §8.C1 — Lift rewrite-budget categories + thresholds into ARCHITECTURE §10

| Field | Value |
|---|---|
| Source | `MASTER-PLAN.md:316`; R5 (`HARDENING-CONSOLIDATED-V6.md:177`) |
| Current language | "Egraph/rewrite-budget tests name categories, node/iteration limits, representative stability, and cost/bridge evidence." Routed to "Tranche C/E implementation specs". |
| V1 fold proposal | Move the **policy** (categories list, default node/iteration ceilings, fail-closed posture, representative-stability protocol) into ARCHITECTURE §10. C.W4's gate then **consumes** the policy and proves it; C.W4 does not author it. |
| Implementation impact | ~30-50 lines of ARCHITECTURE §10 prose. C.W4 test surface unchanged in shape but pinned in numerics. |
| Risk | Low. The numbers (default 1000 iterations / 10K nodes / 5 categories) live in egg/Cranelift literature and the topic-4 deep-dive (`restart/research/topic-4-egraphs.md`). |
| Greenfield value | **HIGH.** Without the fold, C.W4 implementation tranche budgets are decided per-implementor, defeating the V6 promise that "stub-level plan" is settled architecture. With the fold, the architecture commits and C.W4 verifies. |
| Recommendation | **FOLD.** Lift to ARCHITECTURE §10. |

### §8.A2 — Lint-manifest in ARCHITECTURE (generalisation lints as contract)

| Field | Value |
|---|---|
| Source | `MASTER-PLAN.md:248, 751`; `MIGRATION.md:704-714` |
| Current language | Three regex patterns scattered across master and migration; `cargo xtask lint-grammar-generalization` named without specification. |
| V1 fold proposal | Catalog the lint manifest in ARCHITECTURE §13 (tree discipline lifted to a "Lint Contract" sub-section): lint name, pattern set, allowlist syntax, exit semantics, owning tranche. The xtask consumes the contract; tranche A executes. |
| Implementation impact | ~20-30 lines of ARCHITECTURE §13 prose; one new sub-section. |
| Risk | None. |
| Greenfield value | **HIGH.** Lint contracts are architecture; running them is implementation. The current scatter (master row, migration §19.1, Lock 14) defeats single-source-of-truth. |
| Recommendation | **FOLD.** |

### §8.A3 — Declaration-crate review form template

| Field | Value |
|---|---|
| Source | `MASTER-PLAN.md:771`; `HARDENING-CONSOLIDATED-V6.md:355` |
| Current language | Eight fields named at consolidation level (reason, owner, metadata failure, `@host fn` failure, location, no-generic-import, deletion path, reviewer); no template stored. |
| V1 fold proposal | Store the template at `restart/templates/declaration-crate-review.md` (or as ARCHITECTURE §13 appendix). Metadata validator (A.W4) reads the field list from the template. |
| Implementation impact | One template file ~30 lines. |
| Risk | None. |
| Greenfield value | **MEDIUM-HIGH.** The escape valve is rare (`MASTER-PLAN.md:361`); the template makes the rarity *operational*. Without it, the first user of the escape valve invents the form. |
| Recommendation | **FOLD.** |

### §8.J1 — Cookbook page contract template

| Field | Value |
|---|---|
| Source | `MASTER-PLAN.md:797-806`; J.W2 (`MASTER-PLAN.md:555`) |
| Current language | Seven cookbook pages named with target user, mental model, confusion point, resolving artefact, and diagnostic. No per-page table-of-contents contract. |
| V1 fold proposal | Pin a `restart/templates/cookbook-page.md` shape: §1 audience + mental model, §2 minimum running example with code samples, §3 diagnostic codes table, §4 close gate command. J.W2 produces seven uniform pages; sibling tranches' cookbook contributions (D's `@error` cookbook, G's pointer/visitor cookbook, I's recovery cookbook) consume the same shape. |
| Implementation impact | One template file ~40 lines. |
| Risk | None. |
| Greenfield value | **MEDIUM-HIGH.** Without the template, J.W2 is a writing task whose shape is decided post-hoc. With it, the seven pages have one teaching shape. |
| Recommendation | **FOLD.** |

### §8.A1 — Pin metadata schema in ARCHITECTURE, not "current nine grammars"

| Field | Value |
|---|---|
| Source | `MASTER-PLAN.md:246` |
| Current language | "Metadata validation accepts current nine grammars." |
| V1 fold proposal | The metadata schema is *the* author surface (`README.md:11-25`); it deserves pinning by *shape*, not by *count*. ARCHITECTURE §5 already names per-grammar tables; lift the full metadata schema (required fields, optional fields, default values, validation predicates) to ARCHITECTURE §5 as a TOML schema fragment. A.W2 consumes the fragment; A.W4 lints it. |
| Implementation impact | ~50-80 lines of ARCHITECTURE §5 schema; one validator that reads it. |
| Risk | Low. The schema lives in one form already (the implicit one passing nine grammars); making it explicit is a net reduction. |
| Greenfield value | **MEDIUM.** Forces the "two-surface onboarding" anthem to have a written second surface. |
| Recommendation | **FOLD.** |

### §8.E1 — Pin BIR debug-mark contract

| Field | Value |
|---|---|
| Source | `MASTER-PLAN.md:383`; ARCHITECTURE §7.2 |
| Current language | "VM support for tape/direct, host, path, recovery, debug marks." Debug marks unenumerated. |
| V1 fold proposal | Add a `DebugBreak` and `Trace` BIR variant (or extend an existing one) to ARCHITECTURE §7.2's 23-variant table. The VM (E.W3) and the lowerers (F.W2/F.W3) consume the same alphabet. The DAP integration (sibling audit #5 territory) reads only these variants, not VM-internal scratch. |
| Implementation impact | One or two new BIR variants → ARCHITECTURE §7.2 row count grows from 23 → 24 or 25. |
| Risk | Low. Debug marks are routinely BIR-bearing in existing compilers (LLVM's `DBG_*`, Cranelift's debug locations). |
| Greenfield value | **MEDIUM.** The 23-variant table is settled architecture; making the debug surface a peer instead of an "and friends" footnote means E/F/I cannot drift. |
| Recommendation | **FOLD.** |

### §8.J2 — Pin `cargo xtask explain-recognizer` in MASTER-PLAN §16

| Field | Value |
|---|---|
| Source | `MASTER-PLAN.md:803`; `MASTER-PLAN.md:571-589` |
| Current language | Cookbook §25 row 5 names `cargo xtask explain-recognizer`; §16 schema-handoff does not catalog it. |
| V1 fold proposal | Add an `xtask` row to MASTER-PLAN §16 schema-handoff naming `explain-recognizer` (and any other diagnostic xtasks: `explain-cost`, `explain-bridge`, etc.) with owning tranche H/J and downstream consumer (cookbook + LSP fallback). |
| Implementation impact | One row in §16 + one to two xtask implementations (H.W2 or J.W2). |
| Risk | None. |
| Greenfield value | **LOW-MEDIUM.** xtasks are diagnostic surface; they earn architectural rows when the cookbook depends on them. |
| Recommendation | **FOLD.** |

### §8.I1 — Amend `INDEX.md:66` to record V6-final tape disposition

| Field | Value |
|---|---|
| Source | `restart/inheritance/INDEX.md:66` |
| Current language | "tape's *name* dies; tape's *structural insight* survives as **ParseStream**." |
| V1 fold proposal | Replace with V6-final language: tape is the substrate name (`MASTER-PLAN.md:24`); `ParseStream` survives only as `syn::ParseStream` archaeology in proc-macro code (`MASTER-PLAN.md:291`); the BA-period rebranding is retired. |
| Implementation impact | ~5 lines of inheritance documentation. |
| Risk | None. |
| Greenfield value | **LOW.** Documentation drift fix; future inheritance consultations are not misled. |
| Recommendation | **FOLD.** |

### §8.0 — Fold ranking

| Rank | Fold | Class | Cost | Greenfield value |
|---:|---|---|---:|---|
| 1 | §8.C1 rewrite-budget policy → ARCHITECTURE §10 | Architectural prereq | ~30-50 LOC docs | HIGH |
| 2 | §8.A2 lint manifest → ARCHITECTURE §13 | Architectural prereq | ~20-30 LOC docs | HIGH |
| 3 | §8.A3 declaration-crate review template | Operational artefact | ~30 LOC template | MEDIUM-HIGH |
| 4 | §8.J1 cookbook page contract template | Operational artefact | ~40 LOC template | MEDIUM-HIGH |
| 5 | §8.A1 metadata schema → ARCHITECTURE §5 | Architectural prereq | ~50-80 LOC docs | MEDIUM |
| 6 | §8.E1 BIR debug-mark variant pinned in §7.2 | Architectural prereq | 1-2 BIR rows | MEDIUM |
| 7 | §8.J2 `explain-recognizer` xtask in §16 | Operational artefact | ~5 LOC docs | LOW-MEDIUM |
| 8 | §8.I1 INDEX.md tape disposition fix | Documentation drift | ~5 LOC docs | LOW |

Cumulative cost: **~150-250 LOC of documentation + 1 template file + 1-2 BIR
variants**. None requires implementation work; all are master-plan + architecture
+ migration prose adjustments (plus two new template files in
`restart/templates/`).

The cumulative greenfield value is the difference between per-tranche drafting
that **discovers** these contracts mid-implementation versus per-tranche
drafting that **consumes** them as architecture inputs. R7 (full per-tranche
specifications) is the receiver of every fold above; R7's cost shrinks as §8
folds land.

### §8.X — What does not fold

The audit identifies seven plan-layer items that **look like** fold candidates
but resolve as honest work-deferral:

| Item | Path:line | Why not fold |
|---|---|---|
| H.W3 `{N}`/`{M}` placeholders | `MASTER-PLAN.md:479` | Measurement, not architecture. R6 is honest. |
| Generated LOC `+2 percent` ceiling | `MASTER-PLAN.md:663-685` | Settled with per-grammar baselines + yaml provisional row. |
| F.W3 generated header schema | `MASTER-PLAN.md:414, 789` | Settled via PASS-2 §7. |
| Migration §17 tranche-level migration sequence | `MIGRATION.md:665-686` | Mirror of §5.1 calendar; intentional cross-doc redundancy. |
| Migration §20 unresolved punch list (now consolidated to §24) | `MIGRATION.md:790-801` | Single carry-truth principle; intentional. |
| Risk register (§23) | `MASTER-PLAN.md:747-760` | Mitigations are gate-named; not architecture-pending. |
| BA-BD legacy wave map | `inheritance/INDEX.md:29-40` | Mapping table; not active plan. |

These rows are mentioned to make explicit that the §8 fold list is not
maximalist. A row is a fold candidate only when its absence forces a
mid-implementation architectural decision.

---

## §9 — Closing posture

The plan layer's deferrals are **mostly healthy**. Forty-five of fifty-three
tranche rows audited carry receiver, blocker, and gate. Twenty-one of twenty-
three carry-ledger rows are settled. Six of seven cookbook rows are deferred-
for-content (writing tasks). R1-R7 residue carries one architectural-prereq
fold (R5 → §8.C1) and one structural shrinkage opportunity (R7 receives every
§8 fold).

The eight folds in §8 cost ~150-250 LOC of documentation + 1-2 template files
+ 1-2 BIR variants. None reorders tranches. None changes Lock 1-14 semantics.
All increase the per-tranche-spec drafting *consumption surface* and decrease
the *invention surface*.

Sibling audit folds (#1-#6) are absorbed by waves within tranches (most
notably D growing from 5 → 6 waves if function-value folds land), not by
re-ordering tranches. The dependency-ordered A-J calendar is resilient to
sibling fold pressure; this confirms the V6 verdict that "full specs are the
next phase, not a prerequisite to V6 readiness"
(`HARDENING-CONSOLIDATED-V6.md:243`) but adds the §8 folds as architectural
inputs to that next phase.

Hereupon the plan layer is audited, the eight folds are routed, and the
re-sequencing implications are tabulated. The next phase consumes this audit
plus sibling audits #1-#7 as the per-tranche drafting input set.
