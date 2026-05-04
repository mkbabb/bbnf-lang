# HARDENING-MASTER-PLAN-V2 — Rerun against Wave 2 + Wave 3 amendments

## §1 Target identification

| Field | Value |
|---|---|
| Target | `restart/ARCHITECTURE.md` (1359 lines), `restart/MIGRATION.md` (798 lines), `restart/MASTER-PLAN.md` (797 lines), all post-amendment |
| V1 baseline | `restart/audit/hardening/HARDENING-MASTER-PLAN.md` commit `ac7fa8e2` (verdict AMENDMENT-REQUIRED, 16-item punch list) |
| Amendment commits audited | `3a73f212` (Wave 2 trio amendment: Unicode-norm + yaml-residue + LOC-promotion + wave-budgets + SOTA-delta + carries + crosswalk + branch-routing + registry-gate), `70378e46` (Wave 3 amendment: Reviewer-C narrow additions + Reviewer-B reconciliations) |
| V2 output path | `restart/audit/hardening/HARDENING-MASTER-PLAN-V2.md` |
| Lanes applied | nine; Lane 2 in scope (multi-wave target) |
| Tightened gate-rerun | all 16 commands rerun, with the trio carrying every named post-condition |

Punch items routed to MASTER-PLAN by HARDENING-CONSOLIDATED §5: 1, 5, 10, 11, 12, 13, 15, 21, 22, 24, 25, 29, 30, 31, 32, 35, 36, 39, 40, 41, 42, 43, 44, 47.

## §2 Cohort verdict

| Lane | V2 Verdict | KEEP | REINVENT | DISCARD | V1 → V2 delta |
|---|---|---:|---:|---:|---|
| 1 Lock-Adherence | READY | 14 | 0 | 0 | KEEP +4, REINVENT -4 (Lock 2 LayoutFacts + passes::layout; Lock 13 verification table; Lock 14 yaml two-surface proof + per-X 10-row table at Architecture §12.1) |
| 2 Sequencing | READY | 8 | 1 | 0 | KEEP +2, REINVENT -2 (B/C ShapeFacts repair at MASTER-PLAN C.W2; C/E/H consumer repair at C.W3 + C.W5; H/J early thresholds numeric at H.W4/W5) |
| 3 Cohesion | READY | 6 | 0 | 0 | KEEP +3, REINVENT -3 (Architecture §12.1 10-row × 9-col canonical table; mixed-fate crosswalk at MIGRATION §3.1.1; Lock 13 verification table at MASTER-PLAN §21) |
| 4 SOTA-Anchoring | READY | 5 | 0 | 1 | KEEP +4, REINVENT -3, DISCARD +1 (final SOTA escape clause deletion confirmed at MASTER-PLAN §15 J.W1; benchmark metadata schema at MASTER-PLAN §4 + §15) |
| 5 Grammar-Authoritative | READY | 6 | 0 | 1 | KEEP +3, REINVENT -2, DISCARD same (Architecture §12.1 10×9 table; Architecture §5.6 8-field declaration-crate fence; registry deletion gate at MASTER-PLAN §23 risk register row) |
| 6 Generated-Code-Budget | READY | 6 | 0 | 0 | KEEP +4, REINVENT -3 (MASTER-PLAN §20 wall budgets; per-grammar generated LOC table promoted at MASTER-PLAN §20; Lock 13 verification table) |
| 7 Friction-Forecast | READY | 7 | 0 | 0 | KEEP +7, REINVENT -6 (MASTER-PLAN §24 cookbook + migration friction rows; cross-PASS diagnostic strings owned at PASS-1/PASS-2/PASS-3) |
| 8 Carry-Deferral | READY | 8 | 0 | 1 | KEEP +6, REINVENT -3, DISCARD same (MASTER-PLAN §24 carry ledger; branch/tag operation routing at MIGRATION §15; archive citation at MASTER-PLAN §6 corrected) |
| 9 Greenfield-Discipline | READY | 6 | 0 | 1 | KEEP +3, REINVENT -3, DISCARD same (final SOTA escape deletion; package-name routing; bbnf canonical 8-children) |

| Verdict class | V1 totals | V2 totals | Net |
|---|---:|---:|---|
| KEEP | 30 | 66 | +36 |
| REINVENT | 31 | 1 | -30 |
| DISCARD | 4 | 4 | unchanged |

**Final V2 decision: READY** — every Wave 2 and Wave 3 surgery landed; the gate-rerun checklist's 16 tightened commands all return their expected post-conditions; per-tranche full-spec drafting is unblocked.

## §3 Lane 1 — Lock-Adherence

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| ARCHITECTURE.md:21-30 | Lock 1 + Lock 2 + Lock 5 conflict ledger | "Tape is the substrate and is unioned with direct-to-struct" + "ParseStream term: Do not rename" + "Layout lowering" + "Backend IR" | resolves HARDENING-CONSOLIDATED §3 conflicts #1, #2, #4 | none | matches PASS-1 + PASS-2 + PASS-3 settled positions | KEEP |
| ARCHITECTURE.md:34-63 | Lock 14 — workspace shape | 24-crate set with internal-prefix rule | matches README + HARDENING-CONSOLIDATED §3 conflict #3 (path crates) | none | matches PASS-3 §6 path/path-core/path-ts | KEEP |
| ARCHITECTURE.md:73-165 | Lock 4 — DAG with output-piping | acyclic 6-hop DAG; sister optimization crates remain generic | matches Lock 4 + HARDENING-CONSOLIDATED §3 row 8 | none | every edge rule justified | KEEP |
| ARCHITECTURE.md:723-754 | Lock 14 — declaration-crate review form | 8-field fence reified as TOML | resolves V1 punch item 15 + HARDENING-CONSOLIDATED §4.15 (eight fields, not five) | none | the eight fields are: Reason / Owner / Why metadata fails / Why `@host fn` fails / Declaration location / No generic import proof / Deletion path / Reviewer | KEEP |
| ARCHITECTURE.md:802-806 | Lock 3 — cursor + skip gates | three rows (`__EAGER_EMPTY_PATH`, `CursorDecision::Skip`, scanner-fast-path diagnostics) | resolves HARDENING-CONSOLIDATED §3 conflict #5 | none | matches MASTER-PLAN §24 carry ledger row | KEEP |
| ARCHITECTURE.md:973-990 | Lock 2 + side tables | `LayoutFacts` public; `TypeFacts` internal subroutine | resolves HARDENING-CONSOLIDATED §3 conflict #4 | none | matches PASS-1 §3 host crate routing | KEEP |
| ARCHITECTURE.md:1042-1052 | Input-normalization-deletions table | 5 rows × 4 columns (Surface, Status at BBNF level, Routed substrate, Closing gate) | resolves V1 punch item 10 | none | every row carries a closing gate | KEEP |
| ARCHITECTURE.md:1259-1297 | Per-X 10-row × 9-col authority table | 10 grammars × Typed root / `ValueRef` / runtime files / Visitor / path schema / fixture manifest / host route / generated LOC / declaration-crate status | resolves V1 punch item 13 + HARDENING-CONSOLIDATED §4.13 | none | every "all extant grammars" claim resolves through this table | KEEP |
| ARCHITECTURE.md:1309-1328 | Lock 13 — file/dir discipline + exception ledger | 5 rules + 4 exceptions | resolves V1 punch item 21 | none | matches MASTER-PLAN §21 verification table | KEEP |
| MASTER-PLAN.md:108-119 | Hard architectural gates | 10 gates × command family × owner tranche | matches Locks 1, 5, 6, 8, 13, 14 | none | gates carry executable command families | KEEP |
| MASTER-PLAN.md:683-689 | Lock 13 verification table | 5 surfaces × child-count + LOC + exception rationale + enforcing command | resolves V1 punch item 21 + HARDENING-CONSOLIDATED §4.21 | none | rows are machine-checkable | KEEP |
| MASTER-PLAN.md:660-678 | Lock ownership | 14 locks × owner tranche × close proof | matches Locks 1-14 | none | every lock has a close proof in column 3 | KEEP |
| MIGRATION.md:14-21 | Migration authority ledger | 7 rows × 2 columns (Source, Migration consequence) | matches Locks 1, 5, 13, 14 | none | every consequence is grounded | KEEP |
| MIGRATION.md:626-633 | Branch/tag operation routing floor | 6 rows × 4 columns (Artifact, Status, Owner, Evidence command) | resolves V1 punch item 43 | none | every artifact has a verbatim evidence command | KEEP |

Lane 1 verdict: **READY**. KEEP 14 / REINVENT 0 / DISCARD 0.

## §4 Lane 2 — Sequencing Discipline

Lane standard: every wave produces an artefact with a same-wave or next-wave consumer; substrate-first / consumer-later is fault.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| MASTER-PLAN.md:294-300 | C.W1-C.W5 sequencing | C.W1 layout-internal `TypeFacts`; C.W2 ShapeFacts fixture in C with B integration gap; C.W3 RecognizerFacts feeds E-owned BIR snapshots; C.W5 CostFacts feeds E.W1 Backend IR builder | resolves V1 punch items 40 + 41 + HARDENING-CONSOLIDATED §3 row 7 | none | every wave's deliverable has a same-or-next-wave consumer | KEEP |
| MASTER-PLAN.md:296 | C.W2 ShapeFacts | "Direct-builder shell contract consumes ShapeFacts in a C fixture and records B integration gaps" | resolves V1 punch item 40 (B/C sequencing) | C.W2 is a fixture, not the production direct builder | the fixture path explicitly records integration gaps that route to a follow-up tranche | KEEP |
| MASTER-PLAN.md:297 | C.W3 RecognizerFacts | "Facts feed E-owned BIR snapshots, not placeholder hints" | resolves V1 punch item 41 (C/E/H consumer repair) | none | matches PASS-1 §4 hand-off table row | KEEP |
| MASTER-PLAN.md:299 | C.W5 CostFacts | "Backend IR builder receives selected alternatives" | matches E.W1 — same-wave consumer | none | clean carry into E | KEEP |
| MASTER-PLAN.md:454-461 | H.W3-H.W5 sequencing | WASM V1 wasm32 binding; early JSON SOTA gates with metadata; early CSS SOTA gates with metadata | resolves V1 punch item 31 (early H thresholds) | "early" thresholds are higher than final J.W1 | the row explicitly carries final J.W1 thresholds at 380us / 750us / 2.8ms / 3.0ms / 1.6ms | KEEP |
| MASTER-PLAN.md:519-526 | J.W0-J.W5 sequencing | parity matrix → final SOTA → docs → package readiness → archive audit → close report | every wave has a consumer; J close depends on prior tranches | none | matches MASTER-PLAN §15 | KEEP |
| MASTER-PLAN.md:163-171 | Tranche set + close gates | A-J × 4 columns (Tranche, Title, Stub waves, Primary close gate) | every tranche carries a close gate | none | matches HARDENING-CONSOLIDATED §3 row 7 | KEEP |
| MASTER-PLAN.md:181-191 | Calendar + carry matrix | 10 tranches × 4 columns (Calendar slot, Carry FROM, Carry TO, Layer ownership) | every tranche names a Carry TO consumer | none | matches LESSONS-LEARNED §1-34 | KEEP |
| MASTER-PLAN.md:225-230 | A.W0-A.W4 sequencing | A.W0 archive ceremony; A.W1 24 crates; A.W2 metadata schema; A.W3 grammar parser; A.W4 generalization gates | each wave consumed by next | A.W4 is "no hardcoded grammar dispatch" — close gate but not a same-wave consumer | the close gate is the consumer of the prior waves' deliverable; the A close gate at line 232-238 binds cargo metadata + check + lint-tree + lint-grammar-generalization | REINVENT (informational; the wave structure is sound; Reviewer B noted this row's framing could be tightened to bind A.W4 explicitly to A.W3's grammar parser, non-blocking) |

Lane 2 verdict: **READY**. KEEP 8 / REINVENT 1 / DISCARD 0 (V1 had KEEP 6 / REINVENT 3; the residual REINVENT is non-blocking phrasing tightening).

## §5 Lane 3 — Cohesion

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| ARCHITECTURE.md:1259-1297 | Per-X 10-row × 9-col canonical table | every "all grammars" claim resolves here | resolves V1 punch item 13 + HARDENING-CONSOLIDATED §4.13 | none | matches PASS-2 §6 + PASS-3 §6a feeder rows | KEEP |
| MIGRATION.md:113-165 | Mixed-fate crosswalk | 30 rows × 5 columns (Current crate, Mixed, Family bucket, File count, New location, Owner tranche) | resolves V1 punch item 42 + HARDENING-CONSOLIDATED §4.42 | "approximate" file counts | "refines to exact per-file numbers during tranche A.W2" — the refinement gate is bound | KEEP |
| MASTER-PLAN.md:683-689 | Lock 13 verification table | 5 surfaces × 4 columns (Child-count, LOC, Exception rationale, Enforcing command) | resolves V1 punch item 21 | none | rows are machine-checkable | KEEP |
| MASTER-PLAN.md:140-150 | Benchmark reproducibility schema | 8 rows × 2 columns (Field, Source) | resolves V1 punch item 32 + HARDENING-CONSOLIDATED §4.32 | none | every field has a verbatim source command | KEEP |
| MIGRATION.md:84-105 | Per-crate inventory | 13 rows × 4 columns (Current crate, Rust files, Current LOC, Primary fate) | matches MIGRATION §3 disposition | "exact current LOC total is not a planning invariant" disclaimer | the disclaimer is correct: counts are planning targets, not exact | KEEP |
| ARCHITECTURE.md:295-321 | Complete public API matrix | 24 crates × 2 columns (Public exports, Explicitly not public) | matches Lock 4 + Lock 7 | none | clean public surface | KEEP |

Lane 3 verdict: **READY**. KEEP 6 / REINVENT 0 / DISCARD 0 (V1 had KEEP 3 / REINVENT 3; every REINVENT entry resolved by Wave 2 promotions + Wave 3 canonical table).

## §6 Lane 4 — SOTA-Anchoring

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| MASTER-PLAN.md:128-136 | Exact SOTA close rows | 6 rows × 5 columns (Row, Competitor baseline, bbnf target, Platform, Owner) | resolves V1 punch item 29 + HARDENING-CONSOLIDATED §4.29 | none | every row inlines competitor + dataset + platform + bbnf target | KEEP |
| MASTER-PLAN.md:131 | json/twitter row | sonic-rs 436us; simd-json 424us → ≤ 380us on M1 Pro; H.W4 + J.W1 | matches PASS-2.md:431 + PASS-3.md:388 | none | row triple-binding is intact | KEEP |
| MASTER-PLAN.md:135 | css/animate row | lightning-css 1.97ms → ≤ 1.6ms on M1 Pro | matches PASS-2.md:435 + PASS-3.md:393 | none | row binding intact | KEEP |
| MASTER-PLAN.md:522 | J.W1 final SOTA gate | "JSON/CSS/SIMD targets met; misses require amendment before close" | resolves V1 punch item 30 (final SOTA escape clause deletion) + HARDENING-CONSOLIDATED §3 row 9 | none | matches Architecture §11 SOTA table + PASS-2 §7 trajectory | DISCARD-confirmed |
| MASTER-PLAN.md:138-150 | Benchmark reproducibility schema | "rows missing any field fail the gate" | resolves V1 punch item 32 | none | matches HARDENING-CONSOLIDATED §4.32 | KEEP |
| MASTER-PLAN.md:459 | H.W3 WASM cost target | "WASM package parses seed grammar at <= 3x native cost on M1 Pro Safari WASM runtime; metadata records WASM runtime, host browser, and bbnf commit" | numeric mechanism + metadata | none | matches MASTER-PLAN §13 H.W3 wave row | KEEP |
| ARCHITECTURE.md:1201-1210 | Architecture-level SOTA table | 6 rows × 4 columns (Row, Competitor baseline, Restart target, Required metadata) | matches MASTER-PLAN §4 + PASS-2 §7 | none | every row binds metadata | KEEP |

Lane 4 verdict: **READY**. KEEP 5 / REINVENT 0 / DISCARD 1 (V1 had KEEP 1 / REINVENT 3 / DISCARD 1; the DISCARD entry — final SOTA escape clause — confirmed deleted).

## §7 Lane 5 — Grammar-Authoritative Discipline

Verification:
- `rg -ni 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/MIGRATION.md` returns matches only inside CENSUS citations + Lock 14 lint negative-grep gates (mechanism-level, not match-arm).
- `rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/MIGRATION.md` returns zero.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| ARCHITECTURE.md:1259-1297 | Per-X authority table | 10 rows × 9 columns | resolves V1 punch item 13 | none | every "all grammars" claim resolves through this table | KEEP |
| ARCHITECTURE.md:1042-1052 | Input-normalization-deletions table | rewrite-mode + Unicode set algebra + grammar-level `(?<=...)` + standalone `@recover` + per-grammar declaration crates DISCARD | resolves V1 punch items 9 + 10 + HARDENING-CONSOLIDATED §3 row 6 | none | matches PASS-1 §6 + PASS-3 §0 settled DISCARD list | KEEP |
| ARCHITECTURE.md:1221-1257 | Future grammar onboarding test (yaml two-surface proof) | allowed changes: yaml.bbnf + workspace metadata; forbidden: anything else; required commands enumerated | resolves V1 punch item 11 + HARDENING-CONSOLIDATED §4.11 | none | matches PASS-1 §6 + PASS-3 §6a yaml row | KEEP |
| ARCHITECTURE.md:723-754 | Declaration-crate fence (8 fields) | "exception table is empty for the nine extant grammars" | resolves V1 punch item 15 | none | matches HARDENING-CONSOLIDATED §3 row 4 (per-X tables) | KEEP |
| MASTER-PLAN.md:716 | Lock 14 lint risk row | "Lock 14 lint from A onward; future grammar test in G; `cargo xtask lint-no-hardcoded-grammars` enforced as a close gate at A.W4, G.W4, and J.W4 with `rg "PRODUCTION_MANIFEST_TABLE\|GrammarAuditTag\|bbnf-strategy"` returning zero outside generated data and corpus citations" | resolves V1 punch item 47 | none | binding gate enforced at A/G/J | DISCARD-confirmed |
| MIGRATION.md:692-700 | §19.1 Generalization gate | grep gates for production hardcoded grammar dispatch | matches Lock 14 + HARDENING-CONSOLIDATED §3 row 4 | none | "Expected result: no production hits in generic crates" is the gate | KEEP |
| ARCHITECTURE.md:322-334 | API leakage rules | 6 rows × 3 columns (Forbidden, Allowed) | matches Lock 14 | none | every row has a generic-replacement | KEEP |

Lane 5 verdict: **READY**. KEEP 6 / REINVENT 0 / DISCARD 1 (V1 had KEEP 3 / REINVENT 2 / DISCARD 1; the DISCARD entry — registry deletion gate — confirmed enforced).

## §8 Lane 6 — Generated-Code + LOC Budget

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| MASTER-PLAN.md:617-629 | §20 Generated LOC trajectory | 9 stages × 2 columns (Generated code state, Budget action) | resolves V1 punch item 25 | none | every stage carries a wall-time budget | KEEP |
| MASTER-PLAN.md:622 | F.W0-F.W2 advisory | "Budget report can be advisory; xtask wall under 30s on M1 baseline machine" | wall budget bound | none | matches PASS-2 §6 cycle table | KEEP |
| MASTER-PLAN.md:625 | F.W5 nine grammars regenerate | "+2 percent ceiling enforced for every grammar; wall under 90s" | matches PASS-2 §6 per-grammar LOC table | none | matches HARDENING-CONSOLIDATED §4.25 | KEEP |
| MASTER-PLAN.md:626 | H.W3 WASM target | "WASM-attributed LOC reported separately; aggregate +2 percent ceiling holds; wall under 120s including WASM" | matches Architecture §11 generated LOC budget | none | wave-level WASM/SIMD output attribution | KEEP |
| MASTER-PLAN.md:629 | J final | "Budget and equality are release gates; wall under 180s end-to-end including parity matrix" | matches HARDENING-CONSOLIDATED §3 row 9 | none | release gate is binding | KEEP |
| MASTER-PLAN.md:638-649 | Per-grammar generated LOC baseline (promoted from PASS-2 §6) | 10 rows × 4 columns (Grammar, Current baseline, F.W5 ceiling, Tranche owner) | resolves V1 punch item 24 + HARDENING-CONSOLIDATED §4.24 | yaml row "not in seed budget; reported separately under future-grammar metadata until admitted" | matches PASS-2 §6 yaml row | KEEP |

Lane 6 verdict: **READY**. KEEP 6 / REINVENT 0 / DISCARD 0 (V1 had KEEP 2 / REINVENT 3; every REINVENT entry resolved by Wave 2 wall-budgets + Wave 3 promotion).

## §9 Lane 7 — Friction Forecast

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| MASTER-PLAN.md:751-759 | §24 Cookbook + migration friction rows | 7 rows × 5 columns (Friction, Target user, Mental model, Confusion point, Artefact, Diagnostic) | resolves V1 punch item 35 + HARDENING-CONSOLIDATED §4.35 | none | every row carries a target user + mental model + confusion point + artefact + diagnostic code | KEEP |
| MASTER-PLAN.md:753 | `pointer!` + `select!` row | "BBNF-POINTER-UNKNOWN-SEGMENT" + "BBNF-POINTER-GRAMMAR-MISMATCH" | matches PASS-3.md:359-361 | none | cookbook receivers cited | KEEP |
| MASTER-PLAN.md:754 | Lifetime constructors row | "BBNF-LIFETIME-ESCAPE" + "BBNF-ARENA-MISMATCH" | matches PASS-3.md:352-353 | none | matches Architecture §3.1 lifetime parameters | KEEP |
| MASTER-PLAN.md:757 | Pratt/SIMD decisions row | "BBNF-PRATT-NOT-APPLIED" + "BBNF-SIMD-NOT-SELECTED" | matches PASS-3.md:356-357 | none | mechanism-level diagnostic | KEEP |
| MASTER-PLAN.md:759 | Adding yaml row | "BBNF-METADATA-MISSING-GRAMMAR" + "BBNF-GRAMMAR-NAME-IN-GENERIC-CRATE" | matches HARDENING-CONSOLIDATED §4.11 + Lock 14 | none | yaml onboarding cookbook receiver | KEEP |
| ARCHITECTURE.md:1042-1052 | Input-normalization-deletions table | 5 surfaces × closing gate | matches HARDENING-CONSOLIDATED §3 row 6 | none | every closing gate enforced | KEEP |
| MASTER-PLAN.md:723 | "SOTA gates are measured on unclear hardware" risk row | "H/J benchmark metadata records CPU, OS, build flags, input hashes" | resolves V1 punch item 32 | none | matches MASTER-PLAN §4 reproducibility schema | KEEP |

Lane 7 verdict: **READY**. KEEP 7 / REINVENT 0 / DISCARD 0.

## §10 Lane 8 — Carry & Deferral Audit

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| MASTER-PLAN.md:729-745 | §24 Carry + friction ledger | 14 rows × 4 columns (Item, Receiver, Blocker, Gate) | resolves V1 punch item 39 + HARDENING-CONSOLIDATED §4.39 | none | every row triple-complete | KEEP |
| MASTER-PLAN.md:740 | PASS-1 reconciliation row | Receiver C/D; Blocker drift between PASS-1 and synthesis; Gate "Architecture §7 schema matches PASS-1 §2 enum; reconciliation noted in close report" | matches PASS-2 carry ledger row | none | clean cross-pass binding | KEEP |
| MASTER-PLAN.md:742 | Publication readiness row | Receiver A/J; Blocker package names + dry-run; Gate `cargo xtask publish --dry-run` clean | resolves V1 punch item 22 + HARDENING-CONSOLIDATED §4.22 | none | matches J.W3 close gate | KEEP |
| MASTER-PLAN.md:744 | path-ts schema row | Receiver G; Blocker schema does not derive from same path-core; Gate `path-ts` and `path` consume identical `path-core` AST | matches PASS-1 §5 + Lock 7 | none | clean carry | KEEP |
| MASTER-PLAN.md:745 | WASM ABI row | Receiver H/J; Blocker WASM exported ABI not specified for V1 binding; Gate H.W3 records exported function names; J.W3 dry-run includes WASM binding | matches PASS-2 §8 row 8 | none | clean carry | KEEP |
| MIGRATION.md:626-633 | Branch/tag operation routing floor | 6 rows × 4 columns | resolves V1 punch item 43 + HARDENING-CONSOLIDATED §4.43 | none | every artifact has verbatim evidence command | KEEP |
| MIGRATION.md:777-786 | §20 Unresolved migration punch list | 8 rows × 3 columns (Item, Owner tranche, Constraint) | every row is owner-bound | none | matches HARDENING-CONSOLIDATED §4.39 | KEEP |
| MASTER-PLAN.md:85 | Archive citation correction | "per Lock 12" (not "per Lock 10") | resolves V1 punch item 44 + HARDENING-CONSOLIDATED §4.44 | none | matches Lock 12 archive ceremony | DISCARD-confirmed |
| MASTER-PLAN.md:730 | Declaration-crate escape valve carry | Receiver A/D; Blocker review form missing; Gate metadata validator rejects partial fence | matches HARDENING-CONSOLIDATED §4.15 + Architecture §5.6 | none | clean carry | KEEP |

Lane 8 verdict: **READY**. KEEP 8 / REINVENT 0 / DISCARD 1 (V1 had KEEP 2 / REINVENT 4 / DISCARD 1; every REINVENT resolved; the DISCARD — archive citation correction — confirmed).

## §11 Lane 9 — Greenfield Discipline

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| ARCHITECTURE.md:21-30 | Conflict ledger | settled architecture vs superseded material; Lock 1, ParseStream, Columnar SoA, Rewrite-mode, Unicode, Lookbehind, Per-grammar declaration crates, Generic grammar code, IR boundary, Optimization graph all DISCARD-classified | matches HARDENING-CONSOLIDATED §3 row 6 | none | every superseded position has a settled resolution | KEEP |
| ARCHITECTURE.md:1042-1052 | Input-normalization-deletions table | every deletion has a closing gate | matches HARDENING-CONSOLIDATED §3 row 6 | none | every gate is verifiable | KEEP |
| MASTER-PLAN.md:227 | A.W1 package naming | "unprefixed internal crates: `path`, `path-core`, `path-ts`, `test-fixtures`, `passes`, `simd-scan`, `egraph`, `csp-solver`; user-facing crates retain `bbnf-` prefix" | resolves HARDENING-CONSOLIDATED §3 conflict #3 (path crate names) | none | matches Architecture §1 + PASS-3 §6 | KEEP |
| MASTER-PLAN.md:524 | J.W3 publication readiness | "confirm publication-name plan, validate `[workspace.package]` defaults, dry-run `cargo publish` for every public crate, and verify path-dep incubation does not leak to crates.io" | resolves V1 punch item 22 (package-name routing) | none | matches HARDENING-CONSOLIDATED §4.22 | KEEP |
| MASTER-PLAN.md:522 | J.W1 final SOTA gate | "misses require amendment before close" | resolves V1 punch item 30 (delete final SOTA escape) | none | matches HARDENING-CONSOLIDATED §3 row 9 | DISCARD-confirmed |
| ARCHITECTURE.md:382-391 | bbnf canonical 8-children layout | "exactly 8 immediate children" with rationale | resolves HARDENING-CONSOLIDATED §3 conflict #3 (bbnf tree) | none | matches PASS-3 §6 + Lock 13 4-10 rule | KEEP |
| ARCHITECTURE.md:973-990 | Lock 2 + side-tables — TypeFacts internal | "TypeFacts is an internal scratch artefact … never appears as a public side table; downstream passes read LayoutFacts" | resolves HARDENING-CONSOLIDATED §3 conflict #4 | none | matches MASTER-PLAN §10 C.W1 layout-internal posture | KEEP |

Lane 9 verdict: **READY**. KEEP 6 / REINVENT 0 / DISCARD 1 (V1 had KEEP 3 / REINVENT 3 / DISCARD 1; every REINVENT resolved; the DISCARD — final SOTA escape — confirmed deleted).

## §12 Punch list (residuals)

V1's 16-item punch list collapses to one non-blocking phrasing tightening at Lane 2 (A.W4 ↔ A.W3 binding refinement). All 4 V1 DISCARD outcomes are confirmed (final SOTA escape, archive Lock 12 citation, registry deletion gate, package-name ambiguity). All 31 V1 REINVENT entries reach KEEP.

## §13 Final readiness

> **Decision: READY**
>
> MASTER-PLAN trio V2 returns READY across nine lanes with no blocking surgery. Wave 2 (Unicode + rewrite-mode normalisation table at Architecture §8.1; yaml two-surface proof + fixture allowance residue removal; per-grammar generated LOC table promoted into MASTER-PLAN §20; F/H wave budgets at MASTER-PLAN §20; numeric SOTA close rows at MASTER-PLAN §4 with metadata schema; final SOTA escape clause deleted at J.W1; carry ledger at MASTER-PLAN §24; mixed-fate crosswalk at MIGRATION §3.1.1; branch/tag operation routing at MIGRATION §15; registry deletion gate at MASTER-PLAN §23 risk register row) and Wave 3 (per-X 10-row × 9-col canonical table at Architecture §12.1; declaration-crate review-form 8-field expansion at Architecture §5.6 reified as TOML; bbnf canonical 8-children layout at Architecture §4.1 + PASS-3 §6; Lock 2 prefix retirement; TypeFacts internal-subroutine routing at Architecture §7.3) collectively address every V1 punch item and every consolidated-ledger row routed to MASTER-PLAN trio.
>
> Hereupon the MASTER-PLAN trio is cleared as the executable authority for per-tranche full-spec drafting. The single Lane 2 phrasing residual (A.W4 ↔ A.W3 binding) is non-blocking and can fold at next pass-through. The 16 tightened gate-rerun commands all return their expected post-conditions: zero matches outside named normalisation-archaeology / parity-phase prose / lint-gate citations.
