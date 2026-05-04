# HARDENING-CONSOLIDATED

## §1 Target identifications

| Target | Target output audited | Hardening report | Hardening commit | Target commit audited | Verdict | KEEP | REINVENT | DISCARD | Punch list |
|---|---|---|---|---|---|---:|---:|---:|---:|
| PASS-1 | `restart/audit/pass-1-substrate/PASS-1.md` + 6 sub-agent reports | `restart/audit/hardening/HARDENING-PASS-1.md` | `8389c077b75686d87d315d233ec48617ebe7f4e0` | `015317db283ea1e9652401a6a7438ffa5baf028c` | AMENDMENT-REQUIRED | 30 | 29 | 3 | 19 |
| PASS-2 | `restart/audit/pass-2-codegen/PASS-2.md` + 6 sub-agent reports | `restart/audit/hardening/HARDENING-PASS-2.md` | `303b91a91f99ded8c4d3f76d6dedd27828732463` | `015317db283ea1e9652401a6a7438ffa5baf028c` | AMENDMENT-REQUIRED | 38 | 20 | 1 | 9 |
| PASS-3 | `restart/audit/pass-3-runtime/PASS-3.md` + 6 sub-agent reports | `restart/audit/hardening/HARDENING-PASS-3.md` | `c839de98e922bb4013bdd141f5c48e7d15f066e7` | `015317db283ea1e9652401a6a7438ffa5baf028c` | AMENDMENT-REQUIRED | 19 | 47 | 0 | 12 |
| MASTER-PLAN | `restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, `restart/MASTER-PLAN.md` | `restart/audit/hardening/HARDENING-MASTER-PLAN.md` | `ac7fa8e2886ae531afe0cf4bacfc62edd473188e` | `015317db283ea1e9652401a6a7438ffa5baf028c` | AMENDMENT-REQUIRED | 30 | 31 | 4 | 16 |

| Cohort | KEEP | REINVENT | DISCARD | Punch-list rows before dedupe | Final verdict |
|---|---:|---:|---:|---:|---|
| Four-target hardening cohort | 117 | 127 | 8 | 56 | AMENDMENT-REQUIRED |

The four reports agree on the governing shape: tape remains tape and is unioned with direct-to-struct; rewrite-mode is out; Unicode class algebra belongs below BBNF in `parse-that/regex`; `@host fn`, generics, multi-function chaining, `@error`, and `@layout` remain in; default per-grammar declaration crates are out.

The four reports also agree that the artefact set is not ready for tranche full-spec drafting. The defects are concentrated in proof surfaces, gates, naming, budgets, and hand-off discipline. No report returns RE-DRAFT. No cumulative conflict forces re-draft.

Phase 6 therefore consolidates to AMENDMENT-REQUIRED.

## §2 Cohort verdict

| Lane | PASS-1 | PASS-2 | PASS-3 | MASTER-PLAN | Cumulative |
|---|---|---|---|---|---|
| 1 Lock-Adherence | AMENDMENT-REQUIRED; KEEP 7 / REINVENT 7 / DISCARD 0 | AMENDMENT-REQUIRED; KEEP 10 / REINVENT 3 / DISCARD 1 | AMENDMENT-REQUIRED; KEEP 7 / REINVENT 7 / DISCARD 0 | amendment-required; KEEP 10 / REINVENT 4 / DISCARD 0 | AMENDMENT-REQUIRED; KEEP 34 / REINVENT 21 / DISCARD 1 |
| 2 Sequencing | N/A; excluded | N/A; KEEP 1 / REINVENT 0 / DISCARD 0 | N/A; excluded | amendment-required; KEEP 6 / REINVENT 3 / DISCARD 0 | AMENDMENT-REQUIRED; KEEP 7 / REINVENT 3 / DISCARD 0 |
| 3 Cohesion | AMENDMENT-REQUIRED; KEEP 4 / REINVENT 3 / DISCARD 1 | AMENDMENT-REQUIRED; KEEP 5 / REINVENT 2 / DISCARD 0 | AMENDMENT-REQUIRED; KEEP 2 / REINVENT 6 / DISCARD 0 | amendment-required; KEEP 3 / REINVENT 3 / DISCARD 0 | AMENDMENT-REQUIRED; KEEP 14 / REINVENT 14 / DISCARD 1 |
| 4 SOTA-Anchoring | AMENDMENT-REQUIRED; KEEP 4 / REINVENT 2 / DISCARD 0 | AMENDMENT-REQUIRED; KEEP 4 / REINVENT 2 / DISCARD 0 | AMENDMENT-REQUIRED; KEEP 1 / REINVENT 5 / DISCARD 0 | amendment-required; KEEP 1 / REINVENT 3 / DISCARD 1 | AMENDMENT-REQUIRED; KEEP 10 / REINVENT 12 / DISCARD 1 |
| 5 Grammar-Authoritative | AMENDMENT-REQUIRED; KEEP 4 / REINVENT 4 / DISCARD 0 | AMENDMENT-REQUIRED; KEEP 4 / REINVENT 3 / DISCARD 0 | AMENDMENT-REQUIRED; KEEP 3 / REINVENT 5 / DISCARD 0 | amendment-required; KEEP 3 / REINVENT 2 / DISCARD 1 | AMENDMENT-REQUIRED; KEEP 14 / REINVENT 14 / DISCARD 1 |
| 6 Generated-Code-Budget | AMENDMENT-REQUIRED; KEEP 2 / REINVENT 3 / DISCARD 0 | AMENDMENT-REQUIRED; KEEP 3 / REINVENT 2 / DISCARD 0 | AMENDMENT-REQUIRED; KEEP 0 / REINVENT 5 / DISCARD 0 | amendment-required; KEEP 2 / REINVENT 3 / DISCARD 0 | AMENDMENT-REQUIRED; KEEP 7 / REINVENT 13 / DISCARD 0 |
| 7 Friction-Forecast | AMENDMENT-REQUIRED; KEEP 2 / REINVENT 5 / DISCARD 0 | AMENDMENT-REQUIRED; KEEP 2 / REINVENT 4 / DISCARD 0 | AMENDMENT-REQUIRED; KEEP 2 / REINVENT 7 / DISCARD 0 | amendment-required; KEEP 0 / REINVENT 6 / DISCARD 0 | AMENDMENT-REQUIRED; KEEP 6 / REINVENT 22 / DISCARD 0 |
| 8 Carry-Deferral | AMENDMENT-REQUIRED; KEEP 2 / REINVENT 4 / DISCARD 1 | AMENDMENT-REQUIRED; KEEP 3 / REINVENT 2 / DISCARD 0 | AMENDMENT-REQUIRED; KEEP 0 / REINVENT 7 / DISCARD 0 | amendment-required; KEEP 2 / REINVENT 4 / DISCARD 1 | AMENDMENT-REQUIRED; KEEP 7 / REINVENT 17 / DISCARD 2 |
| 9 Greenfield-Discipline | AMENDMENT-REQUIRED; KEEP 5 / REINVENT 1 / DISCARD 1 | AMENDMENT-REQUIRED; KEEP 6 / REINVENT 2 / DISCARD 0 | AMENDMENT-REQUIRED; KEEP 4 / REINVENT 5 / DISCARD 0 | amendment-required; KEEP 3 / REINVENT 3 / DISCARD 1 | AMENDMENT-REQUIRED; KEEP 18 / REINVENT 11 / DISCARD 2 |

| Verdict class | Count | Meaning |
|---|---:|---|
| KEEP | 117 | Ratified architecture, gates, or proof surfaces that survive challenge. |
| REINVENT | 127 | Sound intent, defective current expression; requires surgical amendment. |
| DISCARD | 8 | Current target text or clause must be deleted/replaced. |

### Lane 1 - Lock-Adherence

PASS-1 keeps tape and the two-IR split, but its lock proof is thin around Lock 14, Lock 3, Lock 6, Lock 7, Lock 11, and Lock 12.

PASS-2 keeps the BIR-only lowerer thesis, but places Backend IR type ownership under `codegen`, which contradicts the architecture ownership split.

PASS-3 keeps the tape/direct union and public value posture, but carries stale path crate names, `path!` public wording, incomplete SOTA gates, a too-wide `bbnf/src/` tree, and missing yaml proof.

MASTER-PLAN keeps the governing plan family, but must amend Lock 2 naming, Lock 3 parser gates, Lock 8 gate rows, and Lock 14 onboarding proof.

### Lane 2 - Sequencing

PASS-level hardening is N/A except for PASS-2's counted inheritance row.

MASTER-PLAN is the decisive target for sequencing.

The B/C conflict is concrete: B.W3 builds direct views before C.W2 produces `ShapeFacts`, while C.W2 says the direct builder consumes those facts.

The C/E/H conflict is concrete: C.W3/C.W5 produce recognizer and extraction facts before real BIR and Pratt/SIMD consumers exist.

The H/J relation survives after H gains numeric early thresholds and J keeps final close authority.

### Lane 3 - Cohesion

PASS-1 needs explicit variant field schemas, lower-time invariants, and module rationale.

PASS-2 needs source-of-truth ownership for Backend IR and consumer acceptance gates for runtime contracts.

PASS-3 needs final crate names, final benchmark target rows, a coherent `bbnf` crate tree, and gate-ready carries.

MASTER-PLAN needs migration crosswalks, Lock 13 child-count proof, and package-name routing.

### Lane 4 - SOTA-Anchoring

Every target recognizes the correct competitors.

No target carries the full executable gate shape everywhere it is needed.

The final gate must inline competitor, dataset, platform, and bbnf target number.

The clause allowing SOTA to be met or merely routed at close is discarded.

### Lane 5 - Grammar-Authoritative

All match-arm greps pass.

Every target still needs stronger all-grammar proof.

The yaml onboarding proof must permit exactly two changes: grammar source and workspace metadata.

Fixtures are post-onboarding parity evidence, not an onboarding surface.

### Lane 6 - Generated-Code-Budget

PASS-2 contains the strongest budget seed.

The budget must be lifted into Master/Architecture and decomposed by wave.

PASS-3 must add generated visitor, path metadata, tape identity, and bench-report budget rows.

Every generated budget needs an xtask wall-time budget and a current or provisional observed baseline.

### Lane 7 - Friction-Forecast

Every target under-specifies diagnostics relative to the surface it owns.

The consolidated diagnostic ledger must include pointer/select, lifetime constructors, layout lowering, lookbehind, host signatures, chain steps, Pratt/SIMD decisions, yaml onboarding, WASM host lowering, and lowerer import boundaries.

Cookbook and migration receivers must be named.

The reports identify no re-draft-class UX contradiction; they identify missing committed strings and docs gates.

### Lane 8 - Carry-Deferral

The repeated defect is missing receiver/blocker/receiving-gate triples.

PASS-1's independent-proceed clause is discarded for target advancement purposes.

PASS-2 needs a carry ledger for TS production, parity, PASS-1 reconciliation, PASS-3 docs, publication, and fixtures.

PASS-3 needs carry gates for tape ABI, metadata schema, input normalization, host escape, and incremental fallback reporting.

MASTER-PLAN must route package names, branch/tag operations, migration unresolved items, path-ts timing, WASM ABI, and SOTA misses.

### Lane 9 - Greenfield-Discipline

The greenfield thesis survives.

OpenFrame preservation language does not survive.

Package-name ambiguity does not survive.

The final performance escape clause does not survive.

Legacy code remains research signal unless a replacement design and verification gate ratify it.

## §3 Cross-target conflicts

| Conflict | Sources | Per-target verdicts | Resolution recommendation |
|---|---|---|---|
| Backend IR ownership | PASS-2 places `codegen/src/backend_ir/` under codegen; MASTER-PLAN treats Backend IR as the lowerer contract; README gives `ir` ownership. Sources: `HARDENING-PASS-2.md:276`; `HARDENING-MASTER-PLAN.md:204`; `restart/README.md:108-113`. | PASS-2 DISCARD; MASTER-PLAN KEEP on contract but amendment-required on proof. | Move Backend IR type definitions to `ir/src/backend_ir/`. Leave `codegen` with lowerers, adapters, snapshots, and import-deny gates. |
| Public path macro name | PASS-3 exposes `path!`; README/API contract names `pointer!`; MASTER-PLAN uses path crates correctly. Sources: `HARDENING-PASS-3.md:187`; `restart/README.md:266-283`. | PASS-3 REINVENT; MASTER-PLAN no conflicting verdict. | Rename public authored Rust macro to `pointer!`; keep `PathPlan` or similar as internal type naming only. |
| Path crate names | PASS-3 carries `bbnf-path*`; MASTER-PLAN uses `path`, `path-core`, `path-ts`. Sources: `HARDENING-PASS-3.md:189`; `HARDENING-MASTER-PLAN.md:52`. | PASS-3 REINVENT; MASTER-PLAN KEEP. | Canonicalize on `path`, `path-core`, `path-ts`, and `test-fixtures`; no prefixed internal substrate crate names. |
| Layout terminology | PASS-1, PASS-2, PASS-3 keep layout intent but miss canonical wording; MASTER-PLAN still exposes `TypeFacts` vocabulary. Sources: `HARDENING-PASS-1.md:53`; `HARDENING-PASS-2.md:84`; `HARDENING-PASS-3.md:58`; `HARDENING-MASTER-PLAN.md:204`. | All amendment-required; MASTER-PLAN REINVENT. | Use `layout lowering`, `LayoutFacts`, and `passes::layout` as the public pass surface; keep HM/CSP type checking as subroutine language only. |
| Cursor/byte-skip proof | PASS-1 and PASS-3 mention hand-offs; MASTER-PLAN claims lock ownership without explicit runtime tests. Sources: `HARDENING-PASS-1.md:53`; `HARDENING-PASS-3.md:58`; `HARDENING-MASTER-PLAN.md:205`. | PASS-1 REINVENT; PASS-3 REINVENT; MASTER-PLAN REINVENT. | Add explicit tests for eager empty-path cursor elision and cursor `Skip` lowering into byte-skip in the same generated parse implementation. |
| BBNF extension surface | PASS-1 formal grammar erases block-bodied `@host fn`; PASS-3 introduces `@recover`; all targets reject rewrite-mode and grammar-level Unicode. Sources: `HARDENING-PASS-1.md:182`-`HARDENING-PASS-1.md:184`; `HARDENING-PASS-3.md:203`; `HARDENING-MASTER-PLAN.md:23`-`HARDENING-MASTER-PLAN.md:41`. | PASS-1 DISCARD/REINVENT; PASS-3 REINVENT; MASTER-PLAN KEEP on thesis. | Amend BBNF spec to block-bodied `@host fn`, canonical `|<` lookbehind with finite-width legality, canonical `-> f1 -> f2` chain syntax, and `@error(recover)` rather than standalone `@recover`. |
| Lock 14 yaml onboarding | PASS targets omit yaml proof; MASTER-PLAN includes yaml proof but allows `fixtures/yaml/*`. Sources: `HARDENING-PASS-1.md:192`; `HARDENING-PASS-2.md:278`; `HARDENING-PASS-3.md:199`; `HARDENING-MASTER-PLAN.md:212`. | PASS-1 REINVENT; PASS-2 REINVENT; PASS-3 REINVENT; MASTER-PLAN DISCARD on fixture allowance. | Lock 14 proof admits exactly two input changes: `yaml.bbnf` and `[workspace.metadata.bbnf.grammars.yaml]`. Fixture manifests may appear in a separate post-onboarding parity gate. |
| Per-X proof | PASS reports demand per-X tables; MASTER-PLAN relies partly on PASS-2 budget tables. Sources: `HARDENING-PASS-1.md:193`; `HARDENING-PASS-2.md:279`; `HARDENING-PASS-3.md:201`; `HARDENING-MASTER-PLAN.md:213`. | All REINVENT. | Add architecture-owned tables for all nine extant grammars plus yaml: runtime emission, value/root, visitor, path schema, fixture manifest, host route, generated LOC, declaration-crate status. |
| Generated budget authority | PASS-2 has a useful +2 percent seed; PASS-1/PASS-3/MASTER-PLAN find it insufficiently propagated. Sources: `HARDENING-PASS-1.md:195`; `HARDENING-PASS-2.md:280`-`HARDENING-PASS-2.md:281`; `HARDENING-PASS-3.md:207`; `HARDENING-MASTER-PLAN.md:213`-`HARDENING-MASTER-PLAN.md:214`. | PASS-2 KEEP plus REINVENT; other targets REINVENT. | Lift PASS-2 budget rows into Master/Architecture, then add wave-level F/H/J budgets, generated visitor/path/tape budgets, non-generated LOC caps, and xtask wall budgets. |
| SOTA close gate | PASS-2/PASS-3 lack row-complete tables; MASTER-PLAN permits a routing escape at final close. Sources: `HARDENING-PASS-2.md:280`; `HARDENING-PASS-3.md:193`; `HARDENING-MASTER-PLAN.md:210`-`HARDENING-MASTER-PLAN.md:211`. | PASS-2 REINVENT; PASS-3 REINVENT; MASTER-PLAN DISCARD. | Inline exact numeric SOTA gates in Master and PASS surfaces. Delete the routing escape; a miss opens a named amendment and blocks close. |
| PASS hardening says amend before SYNTHESIS | PASS hardeners phrase next step as amendment before SYNTHESIS consumption, but pipeline order runs hardening after SYNTHESIS. Sources: `HARDENING-PASS-1.md:202`-`HARDENING-PASS-1.md:204`; `HARDENING-PASS-2.md:290`-`HARDENING-PASS-2.md:294`; `HARDENING-PASS-3.md:215`-`HARDENING-PASS-3.md:219`; orchestrator contract. | PASS reports AMENDMENT-REQUIRED; no report asks re-draft. | Treat the phrase as "before tranche drafting and before these PASS decisions are treated as settled." Amendment agents update PASS outputs and/or the synthesis trio as needed, then hardening reruns. |
| OpenFrame residue | PASS-1 agent permits OpenFrame-like internal builders; PASS-2 replaces cloned checkpoints with TapeBuilder. Sources: `HARDENING-PASS-1.md:198`; `HARDENING-PASS-2.md:263`. | PASS-1 DISCARD; PASS-2 KEEP on replacement. | Delete OpenFrame preservation language. Use generated Backend IR builder frames and TapeBuilder checkpoints with no generic substrate role. |
| Package-name ambiguity | PASS-3 carries prefixed names; MASTER-PLAN leaves package-name adjustment open. Sources: `HARDENING-PASS-3.md:189`; `HARDENING-MASTER-PLAN.md:216`. | PASS-3 REINVENT; MASTER-PLAN REINVENT. | Bind workspace crate names in Architecture; route package-name publication details to A.W1/J.W3 without reopening internal crate ownership. |
| Fixture role | PASS-3 treats fixtures as generated/public ecosystem proof; MASTER-PLAN fixture allowance conflicts with Lock 14 onboarding. Sources: `HARDENING-PASS-3.md:199`; `HARDENING-MASTER-PLAN.md:212`. | PASS-3 REINVENT; MASTER-PLAN DISCARD. | Separate onboarding proof from fixture parity. Data fixtures are allowed after onboarding, never as the proof surface. |

The conflicts are surgical. They do not invalidate the greenfield thesis, the tranche set, the tape/direct union, or the two-IR contract.

## §4 Punch list consolidation

The 56 report-local punch-list rows collapse to the following consolidated amendment queue. Items appearing in multiple reports cite each source. Where reports differ on surgery, the most surgical change is named.

### A. Contract and ownership amendments

1. Backend IR ownership.
   Sources: `HARDENING-PASS-1.md:181`; `HARDENING-PASS-2.md:276`; `HARDENING-MASTER-PLAN.md:204`.
   Surgery: Move Backend IR type definitions and variant ownership to `ir/src/backend_ir/`; keep `codegen` limited to lowerers, adapters, snapshots, and emission tests.
   Gate: lowerers compile against `ir::backend_ir` and never define their own BIR node alphabet.
   Owner: PASS-2 plus SYNTHESIS amendment.

2. Lowerer import-deny proof.
   Sources: `HARDENING-PASS-2.md:277`; `HARDENING-PASS-3.md:209`.
   Surgery: Add the gate `rg -n "GrammarIR" crates/codegen/src/lower crates/codegen/src/runtime_template` returns zero; only the BIR producer may import Grammar IR.
   Gate: codegen close fails on direct Grammar IR imports.
   Owner: PASS-2 amendment.

3. Grammar IR schema.
   Sources: `HARDENING-PASS-1.md:180`; `HARDENING-MASTER-PLAN.md:204`.
   Surgery: Expand Grammar IR into a table with variant, fields, stable id keys, producer pass, consumer pass, and forbidden backend leakage.
   Gate: Architecture §7 owns the schema and PASS-1 references it without free-floating spec files.
   Owner: PASS-1 plus SYNTHESIS amendment.

4. Backend IR payload and invariants.
   Sources: `HARDENING-PASS-1.md:181`; `HARDENING-PASS-2.md:276`; `HARDENING-MASTER-PLAN.md:204`.
   Surgery: Add payload categories, lower-time invariants, and per-backend lowering obligations for every BIR variant.
   Gate: PASS-2 may refine payloads; it may not bypass or re-own Backend IR.
   Owner: PASS-1/PASS-2 amendment.

5. PASS-3 emission contract.
   Sources: `HARDENING-PASS-2.md:284`; `HARDENING-PASS-3.md:209`.
   Surgery: Add consumer acceptance gates proving emitted parse signatures compile under PASS-3 API wrappers, document/view metadata feeds visitors/selectors, and materialisation cost tables are generated and documented.
   Gate: PASS-3 cannot close on prose-only hand-offs.
   Owner: PASS-2/PASS-3 amendment.

### B. BBNF language surface amendments

6. Block-bodied `@host fn`.
   Sources: `HARDENING-PASS-1.md:182`; `HARDENING-MASTER-PLAN.md:217`.
   Surgery: Replace declaration-only `HostFn = ... ";"` with a block-bodied production: `HostFn = "@host" "fn" Ident GenericParams? "(" Params? ")" "->" Type HostAttrs? Block ;`.
   Gate: closure semantics and host primitive typing are represented in the formal grammar.
   Owner: PASS-1 amendment.

7. Lookbehind surface.
   Sources: `HARDENING-PASS-1.md:183`; `HARDENING-PASS-2.md:282`; `HARDENING-MASTER-PLAN.md:215`.
   Surgery: Align grammar-level lookbehind with `|<`; state regex-style `(?<=...)` is regex-only; add finite-width legality and diagnostic.
   Gate: unbounded lookbehind fails before codegen.
   Owner: PASS-1/PASS-2 amendment.

8. Chain syntax and type flow.
   Sources: `HARDENING-PASS-1.md:184`; `HARDENING-PASS-1.md:196`.
   Surgery: State canonical multi-function chain syntax and type-flow rule for `-> f1 -> f2`; keep method-chain form only where the host-function body syntax owns it.
   Gate: chain-step type failure has a named diagnostic.
   Owner: PASS-1 amendment.

9. Recovery directive surface.
   Sources: `HARDENING-PASS-3.md:203`; `HARDENING-MASTER-PLAN.md:215`.
   Surgery: Fold standalone `@recover` into `@error(recover)` or label it a legacy compatibility alias without separate grammar-level surface.
   Gate: BBNF formal spec has one recovery directive family.
   Owner: PASS-3 plus SYNTHESIS amendment.

10. Unicode and rewrite-mode normalization.
    Sources: `HARDENING-PASS-1.md:30`; `HARDENING-PASS-3.md:7`; `HARDENING-MASTER-PLAN.md:42`.
    Surgery: Keep rewrite-mode out and keep Unicode class algebra below BBNF in `parse-that/regex`; add this to the SYNTHESIS input-normalization table.
    Gate: Architecture §8 has zero grammar-level rewrite-mode or Unicode-class algebra productions.
    Owner: SYNTHESIS amendment.

### C. Lock 14 and grammar-authoritative amendments

11. Yaml two-surface proof.
    Sources: `HARDENING-PASS-1.md:192`; `HARDENING-PASS-2.md:278`; `HARDENING-PASS-3.md:199`; `HARDENING-MASTER-PLAN.md:212`.
    Surgery: Add a `yaml.bbnf` onboarding test that permits exactly a grammar source file and one `[workspace.metadata.bbnf.grammars.yaml]` block.
    Gate: zero Rust edits, zero generic-crate diff, zero per-grammar match arms, zero declaration crate.
    Owner: all amendment targets, with Architecture as authority.

12. Fixture separation.
    Sources: `HARDENING-PASS-3.md:199`; `HARDENING-MASTER-PLAN.md:212`.
    Surgery: Remove `fixtures/yaml/*` from the Lock 14 onboarding allowance; move fixtures into a later parity or cookbook gate.
    Gate: onboarding proof remains two surfaces only.
    Owner: SYNTHESIS amendment.

13. Per-X grammar proof table.
    Sources: `HARDENING-PASS-1.md:193`; `HARDENING-PASS-2.md:279`; `HARDENING-PASS-3.md:201`; `HARDENING-MASTER-PLAN.md:213`.
    Surgery: Add a table for bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, math, and yaml.
    Gate: table columns include typed root, `ValueRef`, runtime files, visitor, path schema, fixture manifest, host route, generated LOC, and declaration-crate status.
    Owner: SYNTHESIS amendment, fed by PASS amendments.

14. Runtime template emission proof.
    Sources: `HARDENING-PASS-2.md:279`; `HARDENING-PASS-3.md:201`.
    Surgery: Add per-grammar runtime emission rows for `generated.rs`, `parser.rs`, `host.rs`, host source, layout source, error source, Pratt/SIMD source.
    Gate: every per-grammar runtime file is template-emitted or data-only; hand-written runtime files are forbidden.
    Owner: PASS-2 amendment.

15. Rare declaration-crate fence.
    Sources: `HARDENING-PASS-1.md:194`; `HARDENING-PASS-2.md:278`; `HARDENING-PASS-3.md:197`; `HARDENING-MASTER-PLAN.md:217`.
    Surgery: Write the review form: reason, owner, why metadata and `@host fn` fail, declaration location, no generic import, deletion path, reviewer, and receiving gate.
    Gate: exception table is empty for the nine extant grammars.
    Owner: SYNTHESIS amendment.

16. Grammar-name grep classification.
    Sources: `HARDENING-PASS-1.md:112`; `HARDENING-PASS-2.md:174`; `HARDENING-PASS-3.md:117`; `HARDENING-MASTER-PLAN.md:135`.
    Surgery: Record grammar-name grep results as ratified examples, fixture/data paths, benchmark datasets, corpus citations, or faults.
    Gate: match-arm grep remains zero across target text.
    Owner: amendment hardening follow-up.

### D. Workspace and module-shape amendments

17. Path crate naming.
    Sources: `HARDENING-PASS-3.md:189`; `HARDENING-MASTER-PLAN.md:216`.
    Surgery: Rename proposed `bbnf-path-core`, `bbnf-path`, `bbnf-path-ts`, and `bbnf-test-fixtures` to `path-core`, `path`, `path-ts`, and `test-fixtures`.
    Gate: internal workspace crates stay unprefixed unless they are user-facing brand crates.
    Owner: PASS-3 plus Architecture amendment.

18. Public `pointer!` surface.
    Sources: `HARDENING-PASS-3.md:187`; `HARDENING-MASTER-PLAN.md:215`.
    Surgery: Replace public `path!` wording with `pointer!`; keep internal `PathPlan` type names where useful.
    Gate: path/select docs and diagnostics use `pointer!` and `select!`.
    Owner: PASS-3 amendment.

19. `bbnf` aggregator child-count repair.
    Sources: `HARDENING-PASS-3.md:191`; `HARDENING-MASTER-PLAN.md:209`.
    Surgery: Restructure `crates/bbnf/src/` to 4-10 immediate children such as `lib.rs`, `prelude.rs`, `parse/`, `document/`, `query/`, `visitor/`, `diagnostics/`, `metadata/`.
    Gate: Lock 13 child-count proof passes.
    Owner: PASS-3 amendment.

20. PASS-1 crate rationale.
    Sources: `HARDENING-PASS-1.md:185`; `HARDENING-MASTER-PLAN.md:209`.
    Surgery: Add per-crate rationale and sibling API uniformity notes for PASS-1 child directories.
    Gate: Architecture §2 can consume the table without inventing rationale.
    Owner: PASS-1 amendment.

21. Lock 13 verification table.
    Sources: `HARDENING-PASS-2.md:281`; `HARDENING-PASS-3.md:191`; `HARDENING-MASTER-PLAN.md:209`.
    Surgery: Add a table per crate/directory: child count, file-size gate, exception rationale, enforcing command.
    Gate: generated dirs, proc-macro roots, and SIMD intrinsic files are explicit exceptions.
    Owner: SYNTHESIS amendment.

22. Package-name publication routing.
    Sources: `HARDENING-PASS-3.md:189`; `HARDENING-MASTER-PLAN.md:216`.
    Surgery: Bind architecture crate names now; route crate package-name publication details to A.W1/J.W3.
    Gate: publication constraints cannot reopen internal ownership names.
    Owner: MASTER-PLAN amendment.

### E. Generated-code and regen-budget amendments

23. Budget schema.
    Sources: `HARDENING-PASS-1.md:195`; `HARDENING-MASTER-PLAN.md:214`.
    Surgery: Add generated-code budget schema with baseline LOC, projected LOC, allowed delta, construct pressure source, and xtask wall ceiling.
    Gate: budget rows are machine-checkable.
    Owner: PASS-1/SYNTHESIS amendment.

24. Per-grammar generated LOC table.
    Sources: `HARDENING-PASS-2.md:279`; `HARDENING-MASTER-PLAN.md:213`.
    Surgery: Carry PASS-2's per-grammar generated LOC table into Master or Architecture.
    Gate: all "nine seed grammars" claims are auditable without chasing PASS-2.
    Owner: SYNTHESIS amendment.

25. Wave-level generated budget.
    Sources: `HARDENING-PASS-2.md:281`; `HARDENING-MASTER-PLAN.md:214`.
    Surgery: Add per-wave generated LOC and xtask wall-time budgets for F.W0-F.W5 and H.W3-H.W5, including WASM/SIMD target-specific output attribution.
    Gate: generated growth cannot hide behind tranche-level close.
    Owner: MASTER-PLAN amendment.

26. PASS-3 generated-surface budget.
    Sources: `HARDENING-PASS-3.md:207`; `HARDENING-MASTER-PLAN.md:214`.
    Surgery: Add generated visitor LOC, path-schema metadata Rust/sidecar byte budgets, tape identity field/method delta, bench-report generation budget, and regen wall budget.
    Gate: visitor/path/tape additions have explicit ceilings.
    Owner: PASS-3 amendment.

27. Non-generated LOC and child-count budgets.
    Sources: `HARDENING-PASS-2.md:281`; `HARDENING-MASTER-PLAN.md:209`.
    Surgery: Add non-generated LOC budgets for `codegen`, `runtime`, `host`, `xtask`, plus child-count proof.
    Gate: no non-generated file exceeds 500 LOC outside named exceptions.
    Owner: PASS-2/SYNTHESIS amendment.

28. Xtask wall baseline.
    Sources: `HARDENING-PASS-2.md:281`; `HARDENING-MASTER-PLAN.md:214`.
    Surgery: Add observed or explicitly provisional baselines for BIR snapshot and regen wall budgets.
    Gate: budgets have a starting measurement or a provisional label with owner.
    Owner: PASS-2 amendment.

### F. Performance and benchmark amendments

29. SOTA table.
    Sources: `HARDENING-PASS-2.md:280`; `HARDENING-PASS-3.md:193`; `HARDENING-MASTER-PLAN.md:210`.
    Surgery: Inline exact rows: twitter `<= 380 us` M1 Pro vs sonic-rs `436 us` / simd-json `424 us`; canada `<= 2.8 ms` vs sonic-rs `3.144 ms`; citm `<= 750 us` vs sonic-rs `854 us` / simd-json `831 us`; bootstrap `<= 3.0 ms` vs lightning-css `4.16 ms`; animate `<= 1.6 ms` vs lightning-css `1.97 ms`; simdjson OD `>= 5 GB/s` M-series and `>= 7 GB/s` x86.
    Gate: every parse-throughput row names competitor, dataset, platform, and bbnf target.
    Owner: PASS-2/PASS-3/MASTER-PLAN amendment.

30. Delete final SOTA escape.
    Sources: `HARDENING-MASTER-PLAN.md:211`.
    Surgery: Delete the final-close routing escape; replace with "If a target is missed, J.W1 fails and opens a named architecture amendment before close."
    Gate: final close cannot pass with an unresolved performance miss.
    Owner: MASTER-PLAN amendment.

31. Early H thresholds.
    Sources: `HARDENING-MASTER-PLAN.md:207`; `HARDENING-MASTER-PLAN.md:210`.
    Surgery: Replace H progress reports with numeric early thresholds and leave final thresholds to J.
    Gate: H benches cannot pass as mere reports.
    Owner: MASTER-PLAN amendment.

32. Benchmark metadata.
    Sources: `HARDENING-MASTER-PLAN.md:218`.
    Surgery: Bind benchmark metadata to H.W4/H.W5/J.W1: CPU model, OS, compiler flags, input hash, competitor version, bbnf commit, warmup policy, sample policy.
    Gate: every benchmark row carries reproducibility metadata.
    Owner: MASTER-PLAN amendment.

33. BBNF self-host internal gate.
    Sources: `HARDENING-PASS-3.md:195`.
    Surgery: Add `< 100 ms full self-parse + format roundtrip` as a non-Lock-8 internal gate.
    Gate: row explicitly says no SOTA peer claim attaches.
    Owner: PASS-3 amendment.

### G. Diagnostics and friction amendments

34. Compiler diagnostic ledger.
    Sources: `HARDENING-PASS-1.md:196`; `HARDENING-PASS-2.md:282`; `HARDENING-PASS-3.md:205`; `HARDENING-MASTER-PLAN.md:215`.
    Surgery: Add verbatim diagnostics for lookbehind width, host signature mismatch, layout conflict, chain-step type failure, Pratt not applied, SIMD not selected, pointer unknown segment, pointer grammar inference, lifetime escape, arena mismatch, yaml metadata missing, host-chain WASM failure, and lowerer GrammarIR import violation.
    Gate: diagnostics are committed strings, not later prose.
    Owner: PASS-1/PASS-2/PASS-3/SYNTHESIS amendment.

35. Cookbook and migration receivers.
    Sources: `HARDENING-PASS-2.md:282`; `HARDENING-PASS-3.md:205`; `HARDENING-MASTER-PLAN.md:215`.
    Surgery: Add cookbook/migration gates for pointer/select, lifetime constructors, visitors, layout errors, Pratt/SIMD decisions, crate split migration, and adding yaml.
    Gate: every friction row has target user, mental model, confusion point, artefact, and diagnostic.
    Owner: PASS-3/MASTER-PLAN amendment.

36. Incremental fallback reporting.
    Sources: `HARDENING-PASS-3.md:197`; `HARDENING-MASTER-PLAN.md:215`.
    Surgery: Add fallback-rate and snapshot-reuse gates by dataset; keep user-facing LSP output quiet unless policy says otherwise.
    Gate: fallback cannot become an unreported workaround.
    Owner: PASS-3/MASTER-PLAN amendment.

### H. Carry, deferral, and sequencing amendments

37. PASS hand-off tables.
    Sources: `HARDENING-PASS-1.md:186`; `HARDENING-PASS-1.md:187`; `HARDENING-PASS-1.md:188`; `HARDENING-PASS-2.md:283`; `HARDENING-PASS-3.md:197`.
    Surgery: Add `Receiver`, `Blocker`, and `Receiving gate` columns to PASS hand-offs and unresolved punch lists.
    Gate: every carry has all three fields.
    Owner: PASS amendment agents.

38. Delete PASS-1 independent-proceed clause.
    Sources: `HARDENING-PASS-1.md:191`.
    Surgery: Delete "PASS-2 and PASS-3 may proceed independently"; replace with "SYNTHESIS must reconcile conflicting sister-pass outputs before any target advances."
    Gate: this is target-advancement language, not a change to the completed pipeline order.
    Owner: PASS-1 amendment.

39. TS/parity/publication carry ledger.
    Sources: `HARDENING-PASS-1.md:188`; `HARDENING-PASS-2.md:283`; `HARDENING-MASTER-PLAN.md:216`.
    Surgery: Add carry ledger rows for TS production, BD.W5/J parity, PASS-1 reconciliation, PASS-3 API docs, publication, fixtures, `path-ts`, and WASM ABI.
    Gate: receiver, blocker, receiving gate, and artefact supplied are named.
    Owner: PASS-2/MASTER-PLAN amendment.

40. B/C sequencing repair.
    Sources: `HARDENING-MASTER-PLAN.md:206`.
    Surgery: Move ShapeFacts before B.W3, split B.W3 into shell plus C-owned materialization, or change C.W2's consumer away from B direct builder.
    Gate: no wave consumes a fact from a later wave.
    Owner: MASTER-PLAN amendment.

41. C/E/H consumer repair.
    Sources: `HARDENING-MASTER-PLAN.md:207`.
    Surgery: Give C.W3/C.W5 same-wave BIR snapshot consumers or move recognizer/extraction proof into E/H where real BIR and Pratt/SIMD consumers exist.
    Gate: optimizer substrates have real consumers when they land.
    Owner: MASTER-PLAN amendment.

42. Migration crosswalk.
    Sources: `HARDENING-MASTER-PLAN.md:208`.
    Surgery: Add current directory/family, file count, fate count, and owner tranche columns for mixed migration rows.
    Gate: aggregate 834-file disposition is auditable from family rows.
    Owner: MIGRATION amendment.

43. Branch/tag operation routing.
    Sources: `HARDENING-MASTER-PLAN.md:216`.
    Surgery: Route branch/tag operation to A.W0 with evidence commands, including `git rev-parse pre-restart-2026-05-04` and branch-exists checks.
    Gate: no future branch operation remains ownerless.
    Owner: MASTER-PLAN/MIGRATION amendment.

44. Archive citation correction.
    Sources: `HARDENING-MASTER-PLAN.md:219`.
    Surgery: Correct `restart/MASTER-PLAN.md:85-86` from "per Lock 10" to the archive lock row.
    Gate: archive ceremony cites Lock 12 material.
    Owner: MASTER-PLAN amendment.

### I. Legacy-contestation amendments

45. Closure beta-reduction as research signal.
    Sources: `HARDENING-PASS-1.md:197`.
    Surgery: Reframe current closure beta-reduction machinery as research signal only; require fresh greenfield spec and verification before reuse.
    Gate: no legacy closure code is inherited by default.
    Owner: PASS-1 amendment.

46. OpenFrame deletion.
    Sources: `HARDENING-PASS-1.md:198`; `HARDENING-PASS-2.md:263`.
    Surgery: Remove claims that existing OpenFrame builders are useful backend-internal stack detail; replace with generated Backend IR builder-frame design and TapeBuilder checkpoints.
    Gate: no OpenFrame name or substrate survives in generic runtime/codegen plan text.
    Owner: PASS-1/PASS-2 amendment.

47. Registry deletion gate.
    Sources: `HARDENING-PASS-3.md:183`; `HARDENING-MASTER-PLAN.md:135`.
    Surgery: Keep metadata route and make hardcoded registry deletion a close gate.
    Gate: `rg` checks for grammar registries return zero outside generated data.
    Owner: PASS-3/SYNTHESIS amendment.

## §5 Final readiness verdict

**AMENDMENT-REQUIRED**

Every hardening report returns AMENDMENT-REQUIRED.

No hardening report returns RE-DRAFT.

No cumulative conflict demands re-draft. The conflicts concern ownership correction, naming correction, proof expansion, numeric gates, generated budgets, diagnostics, and carry ledgers. Those are substantive surgeries, but they are bounded and preserve the governing architecture.

The cohort cannot advance to per-tranche full-spec drafting until the consolidated amendment queue lands and the hardening gate reruns against the amended artefacts.

Apply the amendment against both PASS outputs and the Phase 2 synthesis trio where the same proof surface appears in both places. The MASTER-PLAN trio must be the executable authority after amendment; PASS outputs remain evidence and source design records.

The final decision is AMENDMENT-REQUIRED because the reports ask for substantive surgeries and because those surgeries affect the tranche gates that would otherwise govern months of execution.

### Amendment routing matrix

The amendment dispatch must preserve write ownership. One agent may apply multiple entries only when the output paths are identical and no neighboring agent is writing there.

| Route | Primary output | Consolidated items | Required close evidence |
|---|---|---|---|
| PASS-1 amendment | `restart/audit/pass-1-substrate/PASS-1.md` and named PASS-1 sub-agent correction notes | 3, 4, 6, 7, 8, 20, 23, 34, 37, 38, 45, 46 | Diff shows only PASS-1 audit paths; formal grammar compiles as prose; hand-off tables include receiver/blocker/gate. |
| PASS-2 amendment | `restart/audit/pass-2-codegen/PASS-2.md` and named PASS-2 sub-agent correction notes | 1, 2, 4, 5, 7, 14, 24, 27, 28, 29, 34, 37, 39, 46 | Diff shows BIR ownership moved to `ir` in plan text; codegen import-deny gate present; runtime emission table covers nine grammars plus yaml. |
| PASS-3 amendment | `restart/audit/pass-3-runtime/PASS-3.md` and named PASS-3 sub-agent correction notes | 5, 9, 11, 12, 13, 17, 18, 19, 26, 29, 33, 34, 35, 36, 37, 47 | Diff uses `pointer!`, unprefixed path crates, repaired `bbnf` tree, yaml proof, generated budget, and diagnostic strings. |
| SYNTHESIS amendment | `restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, `restart/MASTER-PLAN.md` | 1, 5, 10, 11, 12, 13, 15, 21, 22, 24, 25, 29, 30, 31, 32, 35, 36, 39, 40, 41, 42, 43, 44, 47 | Diff carries executable gates in the trio; MASTER-PLAN remains the tranche authority; migration counts become auditable. |

### Target-specific amendment floor

PASS-1 cannot advance until these floor items land:

- Grammar IR schema table.
- Backend IR payload and invariant hand-off.
- Block-bodied `@host fn` grammar production.
- Canonical `|<` lookbehind and finite-width legality.
- Canonical chain syntax and type-flow rule.
- Per-crate rationale and sibling API notes.
- Receiver/blocker/gate columns on hand-offs.
- Yaml two-surface proof.
- Per-X broad-claim tables.
- Rare escape-valve fence.
- Generated-code budget schema.
- Verbatim diagnostics for grammar/type surfaces.
- Legacy closure code reframed as research signal.
- OpenFrame preservation text deleted.

PASS-2 cannot advance until these floor items land:

- Backend IR type ownership moved to `ir`.
- Lowerer import-deny gate added.
- Yaml onboarding smoke added.
- Runtime emission table covers all extant grammars plus yaml.
- SOTA trajectory rows become row-complete or mechanism-only.
- Non-generated LOC and child-count budgets added.
- BIR snapshot baseline recorded or marked provisional.
- PASS-2 diagnostic ledger added.
- Carry ledger added.
- PASS-3 consumer acceptance gates added.

PASS-3 cannot advance until these floor items land:

- `pointer!` is the authored macro name.
- `path`, `path-core`, `path-ts`, and `test-fixtures` are the crate names.
- `bbnf/src/` obeys 4-10 immediate children.
- Exact benchmark rows are present.
- BBNF self-host internal gate is present.
- Carry rows include receiver, blocker, and gate.
- Yaml proof uses two surfaces only.
- Per-X value/path/visitor table exists.
- `@recover` is folded into `@error(recover)` or fenced as legacy alias.
- Diagnostic strings are lifted into the synthesis.
- Generated visitor/path/tape budgets are present.
- Backend IR and Lock 3 hand-off gates are explicit.

The MASTER-PLAN trio cannot advance until these floor items land:

- Public lowering term becomes `layout lowering` / `LayoutFacts` / `passes::layout`.
- Cursor/skip tests are named.
- B/C sequencing is repaired.
- C/E/H consumer proof is repaired.
- Migration crosswalk counts mixed-fate rows.
- Lock 13 child-count and exception table exists.
- Master SOTA gate table carries exact numbers.
- Final-close SOTA routing escape is deleted.
- Yaml fixture allowance is removed from onboarding proof.
- Per-grammar generated LOC table appears in Master or Architecture.
- F/H wave budgets include generated LOC and xtask wall time.
- Friction ledger includes cookbook, user, confusion point, and diagnostic.
- Future/unresolved items carry receiver, blocker, and gate.
- Declaration-crate review form exists.
- Benchmark metadata schema binds to H/J gates.
- Archive lock citation is corrected.

### Gate rerun checklist

The follow-up hardening rerun starts with these checks:

1. `rg -n "ParseStream|rewrite-mode|Unicode class algebra" restart/ARCHITECTURE.md restart/MIGRATION.md restart/MASTER-PLAN.md restart/audit/pass-*/*.md` classifies every match as stale-input discussion or deleted surface.
2. `rg -n "bbnf-path|bbnf-test-fixtures|path!" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-3-runtime` returns no proposed public/internal crate or macro names except migration references.
3. `rg -n "codegen/src/backend_ir" restart/ARCHITECTURE.md restart/audit/pass-2-codegen` returns no proposed ownership path.
4. `rg -n "fixtures/yaml" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-*` returns no Lock 14 onboarding allowance.
5. `rg -n "@recover" restart/ARCHITECTURE.md restart/audit/pass-3-runtime` returns only compatibility-alias text or zero.
6. `rg -n "OpenFrame" restart/audit/pass-1-substrate restart/audit/pass-2-codegen restart/MASTER-PLAN.md` classifies every match as deletion pathology, never preservation.
7. `rg -n "GrammarIR" restart/audit/pass-2-codegen/PASS-2.md restart/ARCHITECTURE.md` finds a lowerer import-deny gate.
8. `rg -n "__EAGER_EMPTY_PATH|CursorDecision::Skip" restart/MASTER-PLAN.md restart/MIGRATION.md` finds explicit Lock 3 gates.
9. `rg -n "twitter|canada|citm|bootstrap|animate|On-Demand" restart/MASTER-PLAN.md restart/audit/pass-3-runtime/PASS-3.md` finds numeric competitor rows.
10. `rg -n "receiver|blocker|receiving gate" restart/MIGRATION.md restart/MASTER-PLAN.md restart/audit/pass-*` finds complete carry-ledger columns.
11. `rg -n "yaml.bbnf|workspace.metadata.bbnf.grammars.yaml" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-*` finds the two-surface proof.
12. `rg -n "generated_loc|regen_wall|xtask" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-*` finds per-grammar and per-wave budgets.
13. `rg -n "BBNF-LIFE|BBNF-LAYOUT|BBNF-OPT|BBNF-GRAMMAR|BBNF-POINTER|lookbehind|HostSignature" restart/ARCHITECTURE.md restart/audit/pass-*` finds committed diagnostic strings.
14. `rg -n "child count|500 LOC|exception rationale" restart/ARCHITECTURE.md restart/MASTER-PLAN.md` finds the Lock 13 verification table.
15. `rg -n "declaration-crate review|why metadata|deletion path|reviewer" restart/ARCHITECTURE.md restart/MIGRATION.md` finds the rare escape form.
16. `rg -n "CPU model|compiler flags|input hash|competitor version|warmup|sample" restart/MASTER-PLAN.md restart/MIGRATION.md` finds benchmark metadata gates.

### Re-draft threshold

The amendment rerun escalates to RE-DRAFT only if one of these conditions appears after amendment:

- The tape/direct union is replaced by direct-only, ParseStream-only, OpenFrame, columnar SoA, or parallel substrate text.
- Backend IR remains owned by `codegen` or any lowerer walks Grammar IR directly.
- The yaml onboarding proof still requires a third surface.
- SOTA close still permits success without meeting numeric gates or naming a blocking amendment.
- B/C or C/E/H sequencing still requires a wave to consume a later-wave artefact without a same-wave proof.
- Generated-code budgets remain absent from F/H/J execution gates.
- Carry ledgers still contain future work without receiver, blocker, and receiving gate.
- The public API still exposes prefixed internal path crates or `path!` as the authored Rust macro.
- Standalone `@recover`, grammar-level rewrite-mode, or grammar-level Unicode class algebra remains part of the BBNF surface.
- OpenFrame preservation remains a proposed implementation detail rather than deletion-path archaeology.

None of those conditions exists as a settled architectural necessity in the current cohort. They are amendment tests, not present grounds for immediate re-draft.

## §6 Voice + discipline locks

The amended documents must preserve the voice and discipline locks from `restart/README.md` §13.

| Lock | Consolidated requirement | Enforcement |
|---|---|---|
| Calibrated, direct prose | State the fault and surgery without softening. | Hardening follow-up rejects hedged punch-list rows. |
| Archaic-permissive, not ornate | Terms such as hereupon may appear where useful; technical clarity wins. | Style review against `docs/precepts/instructions/STYLE.md`. |
| No metalanguage | Do not cite conversation history or agent process as authority. | Source references are path:line, report item, or committed artefact. |
| Path:line citations | Concrete claims about target text carry citations. | Amendment diff checks citations on changed rows. |
| Per-X tables | All "all grammars", "nine seed grammars", "all backends", and "every generated grammar" claims get tables. | Architecture and Master carry the canonical tables. |
| No placeholder wording | Every carry has receiver, blocker, and receiving gate. | Carry ledger must be complete. |
| No quick solutions | Workarounds do not close gates. | Lane 9 rerun checks root-cause replacement. |
| No legacy code uncontested | Legacy code is research signal until ratified. | Migration crosswalk names fate and owner tranche. |
| No overfitting | Generic crates carry no grammar-specific code. | Lock 14 grep gates remain hard gates. |
| Idiomatic gestalt | Rust crate boundaries match ownership. | Backend IR lives in `ir`; codegen consumes; path crates are unprefixed. |
| Generated-code budget | Generator changes carry LOC and wall-time ceilings. | F/H/J wave gates must include the budget table. |
| SOTA anchoring | Throughput gates name competitor, dataset, platform, target. | Final-close routing escapes are forbidden. |
| Carry discipline | Every deferral names receiver, blocker, gate. | No future/pending wording survives without ledger row. |
| Diagnostic specificity | Friction surfaces carry verbatim diagnostics. | PASS-3 and compiler-facing surfaces own committed strings. |

The amended artefacts must keep tables liberal and prose spare. The proof surfaces need more exactness, not more ceremony.

## §7 Closing posture

Hereupon the next step is a narrow amendment dispatch, not tranche drafting: amend PASS-1, PASS-2, PASS-3, and the SYNTHESIS trio against the consolidated punch list, then rerun the four-target hardening gate before any per-tranche full-spec work begins.
