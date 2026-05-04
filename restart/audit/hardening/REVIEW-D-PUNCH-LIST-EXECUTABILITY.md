# REVIEW-D-PUNCH-LIST-EXECUTABILITY

## §1 Audit target identification

| Field | Value |
|---|---|
| Reviewer | D — punch-list executability + amendment effort |
| Target | `restart/audit/hardening/HARDENING-CONSOLIDATED.md` §4 (47 punch-list items), §5 (routing matrix, floor items, gate rerun) |
| Sister surfaces audited | `restart/audit/hardening/HARDENING-PASS-1.md`; `restart/audit/hardening/HARDENING-PASS-2.md`; `restart/audit/hardening/HARDENING-PASS-3.md`; `restart/audit/hardening/HARDENING-MASTER-PLAN.md` |
| Amendment surface evidence | `restart/ARCHITECTURE.md`; `restart/MASTER-PLAN.md`; `restart/MIGRATION.md` |
| Voice + discipline source | `restart/README.md:450-452` |
| Scope boundary | Touch only `restart/audit/hardening/REVIEW-D-PUNCH-LIST-EXECUTABILITY.md` |
| Consolidated commit basis | `015317db283ea1e9652401a6a7438ffa5baf028c` (target text under audit) |
| Hardening commits | `8389c077` (PASS-1), `303b91a9` (PASS-2), `c839de98` (PASS-3), `ac7fa8e2` (MASTER-PLAN), `1cf6dac0` (CONSOLIDATED) |

Lanes applied: per-item concreteness; floor-item dependency analysis; amendment routing accuracy; per-route effort estimate; gate rerun completeness.

The punch-list count is verified: 47 items numbered 1-47 in `HARDENING-CONSOLIDATED.md:163-461`, distributed across nine letter-categories A-I.

| Category | Items | Description |
|---|---|---|
| A | 1-5 | Contract and ownership amendments |
| B | 6-10 | BBNF language surface |
| C | 11-16 | Lock 14 + grammar-authoritative |
| D | 17-22 | Workspace/module shape |
| E | 23-28 | Generated code + regen budget |
| F | 29-33 | Performance + benchmarks |
| G | 34-36 | Diagnostics + friction |
| H | 37-44 | Carry, deferral, sequencing |
| I | 45-47 | Legacy contestation |

## §2 Per-item concreteness table

Concreteness scores (1-5): 1=hand-wavy, 3=acceptable, 5=verbatim text edit. Applicability scores (1-5): 1=command absent or wrong target, 3=command works but verifies obliquely, 5=command directly verifies post-condition.

| # | Cat | Source punch | Concreteness | Applicability | Scope | Verdict |
|---:|---|---|---:|---:|---|---|
| 1 | A | `HARDENING-CONSOLIDATED.md:165-169` Backend IR ownership | 5 | 5 | Multi-section (PASS-2 + SYNTHESIS) | executable |
| 2 | A | `HARDENING-CONSOLIDATED.md:171-175` Lowerer import-deny gate | 5 | 5 | Single-paragraph with verbatim `rg` command | executable |
| 3 | A | `HARDENING-CONSOLIDATED.md:177-181` Grammar IR schema | 4 | 4 | Multi-section (PASS-1 + SYNTHESIS) | executable |
| 4 | A | `HARDENING-CONSOLIDATED.md:183-187` BIR payload + invariants | 4 | 4 | Multi-section (PASS-1 + PASS-2) | executable |
| 5 | A | `HARDENING-CONSOLIDATED.md:189-193` PASS-3 emission contract | 3 | 3 | Multi-section (PASS-2 + PASS-3) | needs-tightening |
| 6 | B | `HARDENING-CONSOLIDATED.md:197-201` Block-bodied `@host fn` | 5 | 5 | Single-line EBNF replacement | executable |
| 7 | B | `HARDENING-CONSOLIDATED.md:203-207` Lookbehind surface | 5 | 5 | Paragraph + finite-width gate | executable |
| 8 | B | `HARDENING-CONSOLIDATED.md:209-213` Chain syntax + type flow | 4 | 4 | Paragraph (PASS-1) | executable |
| 9 | B | `HARDENING-CONSOLIDATED.md:215-219` `@recover` fold | 5 | 5 | Single-line BBNF spec edit | executable |
| 10 | B | `HARDENING-CONSOLIDATED.md:221-225` Unicode + rewrite normalization | 4 | 4 | SYNTHESIS input-normalization table row | executable |
| 11 | C | `HARDENING-CONSOLIDATED.md:229-233` Yaml two-surface proof | 5 | 5 | Multi-section (all four targets) | executable |
| 12 | C | `HARDENING-CONSOLIDATED.md:235-239` Fixture separation | 5 | 5 | SYNTHESIS deletion | executable; see §3 routing fault |
| 13 | C | `HARDENING-CONSOLIDATED.md:241-245` Per-X grammar proof | 5 | 5 | Architecture-owned 10-row table | executable |
| 14 | C | `HARDENING-CONSOLIDATED.md:247-251` Runtime template emission | 5 | 5 | PASS-2 §2/§6 row addition | executable |
| 15 | C | `HARDENING-CONSOLIDATED.md:253-257` Declaration-crate fence | 4 | 4 | SYNTHESIS form table | executable |
| 16 | C | `HARDENING-CONSOLIDATED.md:259-263` Grammar-name grep classification | 3 | 4 | Hardening follow-up bookkeeping | needs-tightening |
| 17 | D | `HARDENING-CONSOLIDATED.md:267-271` Path crate naming | 5 | 5 | Multi-occurrence rename | executable |
| 18 | D | `HARDENING-CONSOLIDATED.md:273-277` Public `pointer!` surface | 5 | 5 | Single-occurrence rename | executable |
| 19 | D | `HARDENING-CONSOLIDATED.md:279-283` `bbnf` aggregator child-count | 5 | 5 | PASS-3 module tree restructure | executable |
| 20 | D | `HARDENING-CONSOLIDATED.md:285-289` PASS-1 crate rationale | 4 | 4 | PASS-1 §3 expansion | executable |
| 21 | D | `HARDENING-CONSOLIDATED.md:291-295` Lock 13 verification table | 5 | 5 | SYNTHESIS architecture table | executable |
| 22 | D | `HARDENING-CONSOLIDATED.md:297-301` Package-name routing | 4 | 4 | MASTER-PLAN waves A.W1/J.W3 | executable |
| 23 | E | `HARDENING-CONSOLIDATED.md:305-309` Budget schema | 4 | 4 | PASS-1 + SYNTHESIS schema row | executable |
| 24 | E | `HARDENING-CONSOLIDATED.md:311-315` Per-grammar generated LOC table | 5 | 5 | SYNTHESIS table carry-up | executable |
| 25 | E | `HARDENING-CONSOLIDATED.md:317-321` Wave-level generated budget | 5 | 5 | F.W0-F.W5, H.W3-H.W5 rows | executable |
| 26 | E | `HARDENING-CONSOLIDATED.md:323-327` PASS-3 generated-surface budget | 5 | 5 | PASS-3 §6 generated budget rows | executable |
| 27 | E | `HARDENING-CONSOLIDATED.md:329-333` Non-generated LOC + child-count | 4 | 4 | PASS-2 + SYNTHESIS table | executable |
| 28 | E | `HARDENING-CONSOLIDATED.md:335-339` Xtask wall baseline | 4 | 4 | PASS-2 budget row | executable |
| 29 | F | `HARDENING-CONSOLIDATED.md:343-347` SOTA table | 5 | 5 | Inline numeric rows in PASS-2/3 + Master | executable |
| 30 | F | `HARDENING-CONSOLIDATED.md:349-353` Delete final SOTA escape | 5 | 5 | Single-clause deletion + replacement | executable |
| 31 | F | `HARDENING-CONSOLIDATED.md:355-359` Early H thresholds | 4 | 4 | H.W4/H.W5 row replacement | executable |
| 32 | F | `HARDENING-CONSOLIDATED.md:361-365` Benchmark metadata | 5 | 5 | H/J gate metadata schema | executable |
| 33 | F | `HARDENING-CONSOLIDATED.md:367-371` BBNF self-host internal gate | 5 | 5 | PASS-3 single-row addition | executable |
| 34 | G | `HARDENING-CONSOLIDATED.md:375-379` Compiler diagnostic ledger | 5 | 5 | Multi-section verbatim-string ledger (PASS-1/2/3 + SYNTHESIS) | executable |
| 35 | G | `HARDENING-CONSOLIDATED.md:381-385` Cookbook + migration receivers | 4 | 4 | PASS-3 + MASTER-PLAN gate rows | executable |
| 36 | G | `HARDENING-CONSOLIDATED.md:387-391` Incremental fallback reporting | 4 | 4 | PASS-3 + MASTER-PLAN gate rows | executable |
| 37 | H | `HARDENING-CONSOLIDATED.md:395-399` PASS hand-off tables | 5 | 5 | All three PASS docs | executable |
| 38 | H | `HARDENING-CONSOLIDATED.md:401-405` Delete PASS-1 independent-proceed | 5 | 5 | Verbatim deletion + replacement | executable |
| 39 | H | `HARDENING-CONSOLIDATED.md:407-411` TS/parity/publication carry ledger | 4 | 4 | PASS-2 + MASTER-PLAN | executable |
| 40 | H | `HARDENING-CONSOLIDATED.md:413-417` B/C sequencing repair | 4 | 4 | MASTER-PLAN B.W3/C.W2 | executable; three options offered |
| 41 | H | `HARDENING-CONSOLIDATED.md:419-423` C/E/H consumer repair | 4 | 4 | MASTER-PLAN C.W3/C.W5 | executable; two options offered |
| 42 | H | `HARDENING-CONSOLIDATED.md:425-429` Migration crosswalk | 5 | 5 | MIGRATION rows | executable |
| 43 | H | `HARDENING-CONSOLIDATED.md:431-435` Branch/tag operation routing | 5 | 5 | MASTER-PLAN A.W0 + MIGRATION | executable |
| 44 | H | `HARDENING-CONSOLIDATED.md:437-441` Archive citation correction | 5 | 5 | Single-line label correction at `MASTER-PLAN.md:85-86` | executable |
| 45 | I | `HARDENING-CONSOLIDATED.md:445-449` Closure beta-reduction reframe | 4 | 3 | PASS-1 single line | executable |
| 46 | I | `HARDENING-CONSOLIDATED.md:451-455` OpenFrame deletion | 5 | 5 | PASS-1 + PASS-2 | executable |
| 47 | I | `HARDENING-CONSOLIDATED.md:457-461` Registry deletion gate | 4 | 4 | PASS-3 + SYNTHESIS | executable |

Aggregate: **42 executable / 5 needs-tightening / 0 hand-wavy.** Verdicts:

- Items 5, 16 are bookkeeping/cross-cutting items where the surgery names a deliverable but not the verbatim row; an amendment agent will need to invent column names. Acceptable tightening: name the column heads up front.
- All "options-offered" items (40 alternative routes; 41 two-route choice) are still executable because each alternative is independently surgical; the agent picks one and commits.
- Items 6, 9, 18, 38, 44 have line-precise deletions or replacements with verbatim text supplied — these are the lowest-risk surgeries.

### 2.1 Per-item concreteness explication

Reasoning behind the borderline scores:

| # | Why this score |
|---|---|
| 1 | Surgery cites the exact relocation path (`ir/src/backend_ir/`), names the residual codegen scope (lowerers, adapters, snapshots, emission tests), and supplies a definitional gate ("lowerers compile against `ir::backend_ir`"). All five concreteness criteria met. |
| 2 | The verbatim `rg -n "GrammarIR" crates/codegen/src/lower crates/codegen/src/runtime_template` command is line-of-code precise. Returns-zero post-condition is a hard boolean. |
| 3 | "Expand Grammar IR into a table" specifies six columns (variant, fields, stable id keys, producer pass, consumer pass, forbidden backend leakage). Architecture §7.1 already has variant + payload columns at `ARCHITECTURE.md:781-797`; the agent extends, not authors. |
| 4 | Payload categories + lower-time invariants are non-trivial to enumerate; Architecture §7.2 at `ARCHITECTURE.md:864-890` already supplies the lowerer matrix; PASS surfaces need parity. |
| 5 | "Add consumer acceptance gates proving emitted parse signatures compile..." names the deliverable but not the verbatim test. Concreteness 3: surgery direction is clear; verbatim text is missing. |
| 6 | Surgery supplies the entire EBNF production verbatim: `HostFn = "@host" "fn" Ident GenericParams? "(" Params? ")" "->" Type HostAttrs? Block ;`. Score 5. |
| 7 | Both grammar-level `\|<` alignment and finite-width legality rule are named. PASS-1 supplied verbatim diagnostic at `PASS-1.md:136`. Score 5. |
| 8 | Chain syntax canonicalization is named but the actual canonical syntax is not supplied verbatim. The surgery says "state canonical multi-function chain syntax and type-flow rule for `-> f1 -> f2`"; the agent must author. Score 4. |
| 9 | Single-line BBNF spec edit: fold `@recover` into `@error(recover)` or label as legacy alias. Two clear alternatives; either is line-precise. |
| 10 | Surgery names input-normalization table; SYNTHESIS owns. Architecture §8 already says rewrite-mode out + Unicode below BBNF. Concreteness 4: agent must place the row but the data exists. |
| 11 | Verbatim onboarding spec already canonical at `ARCHITECTURE.md:1163-1199`. PASS surfaces mirror. Score 5. |
| 12 | Single-action: "Remove `fixtures/yaml/*` from the Lock 14 onboarding allowance." Verbatim deletion. Score 5; routing critiqued in §4 + §8. |
| 13 | Per-X table with named columns: typed root, `ValueRef`, runtime files, visitor, path schema, fixture manifest, host route, generated LOC, declaration-crate status. Nine columns × ten rows. Score 5. |
| 14 | Per-grammar runtime emission rows for `generated.rs`, `parser.rs`, `host.rs`, host source, layout source, error source, Pratt/SIMD source. Seven columns × ten rows. Score 5. |
| 15 | Review form with eight named fields (reason, owner, why metadata fails, why `@host fn` fails, declaration location, no generic import, deletion path, reviewer, receiving gate). Score 4: form-only, but Architecture already has skeleton at `ARCHITECTURE.md:711-719`. |
| 16 | "Record grammar-name grep results as ratified examples..." — deliverable named, but no specific format. Score 3. |
| 17 | Verbatim rename map: `bbnf-path-core` → `path-core`, `bbnf-path` → `path`, `bbnf-path-ts` → `path-ts`, `bbnf-test-fixtures` → `test-fixtures`. Score 5. |
| 18 | Single-occurrence rename: `path!` → `pointer!`. Internal `PathPlan` retained. Score 5. |
| 19 | Restructure to 4-10 children with explicit example structure: `lib.rs`, `prelude.rs`, `parse/`, `document/`, `query/`, `visitor/`, `diagnostics/`, `metadata/`. Score 5; agent has an authoring template. |
| 20 | "Add per-crate rationale and sibling API uniformity notes" — direction clear; rationale text must be authored. Score 4. |
| 21 | Per-crate/directory table with four columns (child count, file-size gate, exception rationale, enforcing command). Score 5; partial pre-fill at `ARCHITECTURE.md:1220-1228`. |
| 22 | "Bind architecture crate names now; route crate package-name publication details to A.W1/J.W3" — two-clause routing. Score 4. |
| 23 | Schema with five columns (baseline LOC, projected LOC, allowed delta, construct pressure source, xtask wall ceiling). Score 4. |
| 24 | Carry-up of PASS-2 budget table at `PASS-2.md:293-310`. Source data exists; placement chosen by agent. Score 5. |
| 25 | F.W0-F.W5 + H.W3-H.W5 row additions. Eight rows; columns are LOC + xtask wall + WASM/SIMD attribution. Score 5. |
| 26 | Generated visitor LOC, path-schema metadata Rust/sidecar byte budgets, tape identity field/method delta, bench-report budget, regen wall budget. Five named budget rows. Score 5. |
| 27 | Non-generated LOC budgets for `codegen`, `runtime`, `host`, `xtask` plus child-count proof. Five named scopes. Score 4. |
| 28 | "Add observed or explicitly provisional baselines for BIR snapshot and regen wall budgets." Two named baselines. Score 4. |
| 29 | Verbatim numeric rows: twitter ≤380μs vs sonic-rs 436μs / simd-json 424μs M1 Pro; canada ≤2.8ms vs sonic-rs 3.144ms; citm ≤750μs vs sonic-rs 854μs / simd-json 831μs; bootstrap ≤3.0ms vs lightning-css 4.16ms; animate ≤1.6ms vs lightning-css 1.97ms; simdjson OD ≥5GB/s M-series, ≥7GB/s x86. Six rows fully numeric. Score 5; partial pre-fill at `ARCHITECTURE.md:1143-1152`. |
| 30 | Verbatim deletion: "or formally routed" → "If a target is missed, J.W1 fails and opens a named architecture amendment before close." Score 5. |
| 31 | "Replace H progress reports with numeric early thresholds and leave final thresholds to J." Direction clear; numeric thresholds derived from #29. Score 4. |
| 32 | Eight metadata fields named: CPU model, OS, compiler flags, input hash, competitor version, bbnf commit, warmup policy, sample policy. Score 5. |
| 33 | Single row: `< 100 ms full self-parse + format roundtrip` as non-Lock-8 internal gate. Score 5. |
| 34 | Verbatim diagnostics for 13 named conditions: lookbehind width, host signature mismatch, layout conflict, chain-step type failure, Pratt not applied, SIMD not selected, pointer unknown segment, pointer grammar inference, lifetime escape, arena mismatch, yaml metadata missing, host-chain WASM failure, lowerer GrammarIR import violation. Score 5. |
| 35 | Cookbook + migration gates for 7 named surfaces. Score 4. |
| 36 | Fallback-rate + snapshot-reuse gates by dataset; LSP policy quiet-by-default. Score 4. |
| 37 | Three named columns added to all PASS hand-off tables. Score 5. |
| 38 | Verbatim deletion + replacement at `PASS-1.md:150`. Score 5. |
| 39 | Carry ledger rows for eight named items: TS production, BD.W5/J parity, PASS-1 reconciliation, PASS-3 API docs, publication, fixtures, `path-ts`, WASM ABI. Score 4. |
| 40 | Three alternatives: move ShapeFacts before B.W3, split B.W3, or change C.W2's consumer. Score 4. |
| 41 | Two alternatives: same-wave BIR snapshot consumers OR move proof into E/H. Score 4. |
| 42 | Migration crosswalk with 6 named columns: current directory/family, file count, fate count, owner tranche. Score 5. |
| 43 | A.W0 routing with verbatim evidence commands: `git rev-parse pre-restart-2026-05-04` + branch-exists checks. Score 5. |
| 44 | Single-line label correction at `MASTER-PLAN.md:85-86`. Score 5. |
| 45 | Reframe closure beta-reduction as research signal; require fresh greenfield spec + verification. Score 4. |
| 46 | Replace OpenFrame preservation language with generated Backend IR builder frames + TapeBuilder checkpoints. Verbatim deletion + replacement direction. Score 5. |
| 47 | Make registry deletion a close gate; `rg` checks return zero outside generated data. Score 4. |

### 2.2 Concreteness distribution

| Concreteness score | Count | Items |
|---:|---:|---|
| 5 | 27 | 1, 2, 6, 7, 9, 11, 12, 13, 14, 17, 18, 19, 21, 24, 25, 26, 29, 30, 32, 33, 34, 37, 38, 42, 43, 44, 46 |
| 4 | 16 | 3, 4, 8, 10, 15, 20, 22, 23, 27, 28, 31, 35, 36, 39, 40, 41, 45, 47 |
| 3 | 2 | 5, 16 |

| Applicability score | Count | Items |
|---:|---:|---|
| 5 | 26 | 1, 2, 6, 7, 9, 11, 12, 13, 14, 17, 18, 19, 21, 24, 25, 26, 29, 30, 32, 33, 34, 37, 38, 42, 43, 44, 46 |
| 4 | 18 | 3, 4, 8, 10, 16, 20, 22, 23, 27, 28, 31, 35, 36, 39, 40, 41, 47 |
| 3 | 3 | 5, 15, 45 |

The distribution is heavily skewed to 4-5; the punch list is in the upper tier of executability for the audit family. No item scores below 3.

## §3 Floor-item dependency analysis

The consolidated §5 names floor items per amendment route. The dependency graph below identifies (i) ordering constraints between items and (ii) parallelizable clusters.

### 3.1 Hard prerequisite edges

| Edge | Reason |
|---|---|
| 1 (BIR ownership) → 4 (BIR payload + invariants) | Payload spec must be authored in the new owning crate; ownership move first prevents dual-write at `codegen/src/backend_ir` and `ir/src/backend_ir`. |
| 1 (BIR ownership) → 2 (Lowerer import-deny gate) | The deny-gate command targets `crates/codegen/src/lower` and excludes `ir`; it is meaningful only after ownership is correctly bound. |
| 6 (block-bodied `@host fn`) → 8 (chain syntax + type flow) | Block-body production hosts the chain expression; chain typing rule cannot be stated until the body is in scope. |
| 7 (lookbehind surface) → 34 (lookbehind diagnostic) | Verbatim diagnostic text presupposes the canonical `|<` surface. |
| 11 (yaml two-surface proof) → 12 (fixture separation) | The fixture deletion is meaningful only after onboarding proof is named; without #11 there is no proof to police. |
| 11 (yaml proof) → 13 (per-X table) | Table includes yaml row. |
| 14 (runtime template emission) → 13 (per-X table) | Table cells consume runtime emission columns. |
| 17 (path crate naming) → 18 (`pointer!` rename) | The macro rename targets the unprefixed `path` crate. |
| 17 (path crate naming) → 19 (bbnf aggregator restructure) | `bbnf/src/parse/`, `query/`, etc. depend on the path crate set. |
| 23 (budget schema) → 24, 25, 26, 27, 28 | All concrete LOC/wall budgets fill schema columns. |
| 24 (per-grammar LOC table) → 25 (wave-level budget) | Wave budgets reference per-grammar maxima. |
| 29 (SOTA table) → 30 (delete final escape) | Numeric rows must exist before the escape clause is forfeited. |
| 29 (SOTA table) → 31 (early H thresholds) | H thresholds reference per-row competitor numbers. |
| 29 (SOTA table) → 32 (benchmark metadata) | Metadata schema attaches to the rows. |
| 37 (hand-off triple columns) → 39 (carry ledger) | Ledger is a direct realization of the receiver/blocker/gate triple at the multi-target carry layer. |
| 38 (delete independent-proceed) → 39 (carry ledger) | The clause forbidding independent advance is the policy backing the carry ledger. |
| 46 (OpenFrame deletion) → 4 (BIR payload) | `SpeculativeAlt` payload at `ARCHITECTURE.md:871` already says "must not clone OpenFrame stacks"; payload addition can be authored without OpenFrame text contention only after deletion lands. |
| 47 (registry deletion gate) → 13 (per-X table) | Per-X declaration-crate-status column requires registry-deletion gate to be a hard close. |

### 3.2 Critical path

Longest serial chain through the floor:

```
1 (BIR ownership)
  → 4 (BIR payload)
    → 14 (runtime template emission)
      → 13 (per-X table)
        → 11 (yaml proof)  [parallel with 13]
          → 12 (fixture separation)
            → 47 (registry deletion gate)
```

That is a depth-7 chain. Items 23 → 24 → 25 forms an independent depth-3 chain in budgets. Items 29 → 30 → 32 forms a depth-3 chain in SOTA. Items 6 → 8 forms a depth-2 chain in BBNF surface. Item 17 → 18 → 19 → (PASS-3 module tree) forms a depth-3 chain.

The seven-step BIR/runtime/yaml chain is the binding critical path. Estimating 30-60 minutes per surgical step (multi-section items) and 10-20 minutes per single-row edit, the critical path takes ~3.5-4 hours of serial work for one agent. With route-level parallelism (§5) the wall time drops to roughly two parallel waves.

### 3.3 Parallelizable clusters

Items with no shared write surface and no inter-item edge can land in parallel within one dispatch wave:

| Cluster | Items | Justification |
|---|---|---|
| BBNF surface | 6, 7, 9 (with 8 sequenced after 6) | All are BBNF spec edits at `PASS-1.md:84-121` and `ARCHITECTURE.md:946-994`; one author per file required. |
| Workspace naming | 17, 18, 22 | Path/macro/package routing all touch crate-name surfaces; same author can sweep. |
| Budgets | 23 + (24, 25, 26, 27, 28) | Schema first, then five row-level fills in any order. |
| SOTA | 29 + (30, 31, 32, 33) | Numeric rows must land before escape deletion + threshold split. |
| Hand-off discipline | 37, 38, 39, 40, 41, 42, 43 | Mostly disjoint; 38 → 39 is the only edge. |
| Diagnostics | 34, 35, 36 | One ledger surface across PASS-1/PASS-2/PASS-3/SYNTHESIS; serializable per amendment route, parallelizable across routes. |
| Legacy contestation | 45, 46, 47 | Disjoint targets (PASS-1 closure text; PASS-1/PASS-2 OpenFrame; PASS-3/SYNTHESIS registry). |

### 3.4 Floor-item summary by route

The consolidated §5 names 14 PASS-1 floor items, 10 PASS-2 floor items, 12 PASS-3 floor items, and 16 MASTER-PLAN floor items (52 floor mentions; many items appear in multiple floors per consolidated §5 routing).

| Route | Floor items count | Critical-path participation |
|---|---:|---|
| PASS-1 | 14 | items 3, 4, 6, 7, 8 on chain |
| PASS-2 | 10 | items 1, 2, 4, 14 on chain |
| PASS-3 | 12 | items 11, 13, 17, 18, 19 on chain |
| SYNTHESIS (master-plan trio) | 16 | items 11, 12, 13, 25, 29, 30, 39, 40, 41, 42, 47 on chain |

## §4 Routing accuracy table

Each item's primary edit surface is verified against the route the consolidated §5 assigns. "Edit surface" is the first physical location an agent must touch.

| # | Routed-to | Primary edit surface | Routing accurate? | Notes |
|---|---|---|---|---|
| 1 | PASS-2 + SYNTHESIS | `restart/audit/pass-2-codegen/PASS-2.md:139-181` (relocate proposed tree) AND `restart/MIGRATION.md:179, 330` (already says `ir/src/backend_ir`) | Mixed — PASS-2 is the binding edit; SYNTHESIS trio largely already says `ir::backend_ir` at `ARCHITECTURE.md:924`, `MIGRATION.md:179, 330, 727`. Action: PASS-2 amendment is mandatory; SYNTHESIS amendment is verification only. | accurate |
| 2 | PASS-2 | `restart/audit/pass-2-codegen/PASS-2.md` §2/§9 add deny-gate text | accurate | `agent-6-codegen-coherence-auditor.md:39-43` already has partial command |
| 3 | PASS-1 + SYNTHESIS | `restart/audit/pass-1-substrate/PASS-1.md:24` AND `restart/ARCHITECTURE.md:781-797` (Grammar IR variants table) | accurate | Architecture §7.1 already has variant list; the table needs producer/consumer/forbidden columns |
| 4 | PASS-1 + PASS-2 | `restart/audit/pass-1-substrate/PASS-1.md:26` AND `restart/audit/pass-2-codegen/PASS-2.md:50-76` | accurate | architecture §7.2 at `ARCHITECTURE.md:864-890` already has payload + lowerer matrix; PASS surfaces need parity |
| 5 | PASS-2 + PASS-3 | PASS-2 §4 (handoffs at `PASS-2.md:260-273`); PASS-3 §3 (handoffs at `PASS-3.md:316-319`) | accurate | symmetric edit, two amendments |
| 6 | PASS-1 | `restart/audit/pass-1-substrate/PASS-1.md:91`; `restart/ARCHITECTURE.md:986` (`HostDecl ::= ... ";"`) | partially mis-routed — Architecture §8.1 grammar sketch ALSO says `HostDecl ::= ... ";"` at `ARCHITECTURE.md:986`. Action: PASS-1 amendment + SYNTHESIS amendment to ARCHITECTURE.md:986 | needs SYNTHESIS co-routing |
| 7 | PASS-1 + PASS-2 | PASS-1 §9 (formal grammar) + PASS-2 §9 (lookbehind diagnostics) | accurate; ARCHITECTURE already has `Lookbehind ::= "|<" Suffix \| "|<!" Suffix` at `ARCHITECTURE.md:970` | accurate |
| 8 | PASS-1 | `restart/audit/pass-1-substrate/PASS-1.md:113-114` | accurate | also touch `ARCHITECTURE.md:982-985` if MapExpr semantics overlap chain syntax — verify during amendment |
| 9 | PASS-3 + SYNTHESIS | PASS-3 §3, §11 + ARCHITECTURE §8 | accurate | Architecture §8 currently lists `ErrorDecl ::= ...` at `ARCHITECTURE.md:988`; the `@error(recover)` form needs to be canonicalized there |
| 10 | SYNTHESIS | input-normalization table (Architecture §8 region) | accurate; current `ARCHITECTURE.md:946-953` already says rewrite-mode out, Unicode below BBNF | accurate |
| 11 | all four routes (Architecture authority) | `restart/ARCHITECTURE.md:1163-1199` (Future Grammar Onboarding Test) | accurate; Architecture is authoritative; PASS-1/2/3 mirror it | accurate |
| 12 | SYNTHESIS | `restart/ARCHITECTURE.md:1132-1138; 1151-1162` per `HARDENING-MASTER-PLAN.md:212` | **mis-routing fault**: the cited line range (1132-1162) is the SOTA gate / Generated LOC budget section, NOT the onboarding allowed-changes block. Actual onboarding allowed-changes lives at `ARCHITECTURE.md:1170-1186`. The current text does not contain `fixtures/yaml/*` at all (verified: `grep -n "fixtures/yaml" restart/ARCHITECTURE.md` returns no match). The hardening claim that fixtures must be removed from allowed changes is therefore **already true** in current Architecture text; the surgery is moot for ARCHITECTURE.md but may still apply to PASS-3 fixture rows at `PASS-3.md:272-289`. | **routing repair required** |
| 13 | SYNTHESIS (Architecture-owned) | new table in `restart/ARCHITECTURE.md` §11 or §12 | accurate | choose §11 or §12 in amendment |
| 14 | PASS-2 | `restart/audit/pass-2-codegen/PASS-2.md` §2/§6 | accurate | `PASS-2.md:295-308` already has per-grammar LOC budget; needs columns for emission |
| 15 | SYNTHESIS | `restart/ARCHITECTURE.md:711-719` already has the form skeleton — needs to be filled with reviewer + deletion path detail | accurate; partial pre-fill | accurate |
| 16 | hardening follow-up | the four pass syntheses' grammar-name greps | accurate; this is bookkeeping for next-pass auditor | accurate |
| 17 | PASS-3 + Architecture | `PASS-3.md:88-90, 244-270` (PASS-3 prefixed names) AND `ARCHITECTURE.md:50-52` already says `path`, `path-core`, `path-ts`. Architecture is correct; PASS-3 must align. | accurate; PASS-3 carries the fork |
| 18 | PASS-3 | `PASS-3.md:84-92` | accurate; SYNTHESIS already names `pointer!` at `MASTER-PLAN.md:400`, `ARCHITECTURE.md:266-287` — verify | accurate |
| 19 | PASS-3 | `PASS-3.md:160-179` | accurate | restructure into 4-10 children |
| 20 | PASS-1 | `PASS-1.md:46-60` | accurate | rationale per child directory |
| 21 | SYNTHESIS | `restart/ARCHITECTURE.md` §13 (already has child-count rule + exception ledger at `ARCHITECTURE.md:1220-1228`) | accurate; partial pre-fill exists; per-crate child count + enforcing command columns missing | accurate |
| 22 | MASTER-PLAN | `MASTER-PLAN.md` waves A.W1/J.W3 | accurate | Master already names them at lines `197-203, 504-510` |
| 23 | PASS-1 + SYNTHESIS | `PASS-1.md` new subsection + Architecture metadata budget hook at `ARCHITECTURE.md:689` | accurate | metadata `generated_loc_budget = 1.02` already exists |
| 24 | SYNTHESIS | Master/Architecture; PASS-2 budget table at `PASS-2.md:293-310` is the source | accurate | route is "carry up" |
| 25 | MASTER-PLAN | F.W0-F.W5 + H.W3-H.W5 row additions | accurate | currently F.W4 only, per `MASTER-PLAN.md:379` |
| 26 | PASS-3 | `PASS-3.md` §6 generated budget | accurate | new subsection |
| 27 | PASS-2 + SYNTHESIS | non-generated LOC budget table | accurate | PASS-2 §6 |
| 28 | PASS-2 | xtask wall baseline at `PASS-2.md:312-319` | accurate | observed/provisional flag |
| 29 | PASS-2 + PASS-3 + MASTER-PLAN | numeric SOTA rows at PASS-2 §6, PASS-3 §6, MASTER-PLAN H.W4/H.W5/J.W1 | accurate; ARCHITECTURE.md:1143-1152 already has the exact gate rows; needs to propagate to PASS-2/PASS-3/Master | accurate |
| 30 | MASTER-PLAN | `MASTER-PLAN.md:495, 549` | accurate; MASTER-PLAN.md:506 currently says "JSON/CSS/SIMD targets met; misses require amendment before close" — the "or formally routed" phrasing has already been partially erased. Verify whether punch is moot or refers to PASS-2/PASS-3 wording | partial pre-fill; verify |
| 31 | MASTER-PLAN | H.W4/H.W5 rows at `MASTER-PLAN.md:444-445` | accurate; rows currently say "with metadata and numeric deltas" (already partially numeric); add explicit early thresholds | accurate |
| 32 | MASTER-PLAN | H/J gate metadata schema | accurate | `MIGRATION.md:725` already names it |
| 33 | PASS-3 | `PASS-3.md:291-303` | accurate | non-Lock-8 internal gate row |
| 34 | PASS-1/2/3 + SYNTHESIS | verbatim diagnostic strings across all friction surfaces | accurate; large multi-route surgery |
| 35 | PASS-3 + MASTER-PLAN | cookbook receivers for pointer/lifetime/visitor/layout/Pratt/SIMD/migration/yaml | accurate |
| 36 | PASS-3 + MASTER-PLAN | fallback-rate gate; LSP policy | accurate |
| 37 | PASS-1 + PASS-2 + PASS-3 | hand-off table column additions | accurate; one-pass per PASS document |
| 38 | PASS-1 | `PASS-1.md:150` | accurate; verbatim deletion supplied |
| 39 | PASS-2 + MASTER-PLAN | carry ledger rows for TS/parity/publication | accurate |
| 40 | MASTER-PLAN | B.W3/C.W2 ordering at `MASTER-PLAN.md:232-236, 267-272` | accurate; current `MASTER-PLAN.md:280` (C.W2) already says "Direct-builder shell contract consumes ShapeFacts in a C fixture and records B integration gaps" — partial repair already in target text | accurate; verify whether punch is moot |
| 41 | MASTER-PLAN | C.W3/C.W5 consumer at `MASTER-PLAN.md:281-283` | accurate; C.W3 currently says "Facts feed E-owned BIR snapshots, not placeholder hints" — partially repaired | accurate; verify |
| 42 | MIGRATION | `MIGRATION.md:42-56, 111-115` | accurate |
| 43 | MASTER-PLAN + MIGRATION | A.W0 branch/tag with `git rev-parse pre-restart-2026-05-04` | accurate |
| 44 | MASTER-PLAN | `MASTER-PLAN.md:85-86` | accurate; verbatim correction supplied; one-line edit |
| 45 | PASS-1 | `PASS-1.md:121` | accurate |
| 46 | PASS-1 + PASS-2 | OpenFrame deletion language | accurate; ARCHITECTURE.md:871, 926 already say no-OpenFrame for BIR |
| 47 | PASS-3 + SYNTHESIS | metadata route + close gate | accurate |

**Routing fault summary:**

| Severity | Count | Items |
|---|---:|---|
| Hard mis-routing (cited surface incorrect) | 1 | #12 (fixture separation) — see fault note |
| Partial mis-routing (single-route punch needs additional route) | 2 | #6 (`@host fn` block-body — also at `ARCHITECTURE.md:986`); #1 (BIR ownership — partial pre-fill in SYNTHESIS) |
| Partial pre-fill (punch is partly already landed in target text) | 6 | #15, #21, #29, #30, #31, #40, #41 |
| Accurate, fully novel surgery | 38 | remainder |

The single hard mis-routing (item #12) is recoverable: the fixture separation surgery applies to `PASS-3.md:272-289` (where four-fixture-dir sketch lives) rather than to the cited Architecture line range. The dispatch instruction must redirect.

Partial pre-fills are not faults — the surgery still ratifies the existing text — but the amendment dispatch should call out that the agent verifies first, edits second.

## §5 Per-route effort estimate

| Route | Output | Items | Cumulative scope | Wall-time estimate (single competent agent) | Risk surface | Likelihood of clean dispatch |
|---|---|---:|---|---:|---|---|
| PASS-1 amendment | `restart/audit/pass-1-substrate/PASS-1.md` + named sub-agent notes | 12 (3, 4, 6, 7, 8, 20, 23, 34, 37, 38, 45, 46) | EBNF surgery + new diagnostic ledger + per-crate rationale + hand-off table columns + closure/OpenFrame contestation + budget schema + chain syntax | 2.5-3.5 hr | EBNF re-parse must compile; per-crate rationale touches eight directories; verbatim diagnostics must match `BBNF`-prefix family used elsewhere | high — surgical bounds, no cross-amendment overlap |
| PASS-2 amendment | `restart/audit/pass-2-codegen/PASS-2.md` + named sub-agent notes | 14 (1, 2, 4, 5, 7, 14, 24, 27, 28, 29, 34, 37, 39, 46) | BIR ownership move (most consequential), import-deny gate, runtime emission table for 10 grammars, SOTA numeric rows, non-generated LOC table, baseline xtask budget, diagnostic ledger, carry ledger, OpenFrame deletion | 3.5-5 hr | Largest amendment by item count + scope; ownership rewrite cascades into module tree at `PASS-2.md:139-181` and into agent-1 BIR architect notes; SOTA numeric inlining requires careful row alignment with PASS-3 and Master | medium-high — clean if the ownership move is done first; otherwise diagnostic + emission table edits invite confused state |
| PASS-3 amendment | `restart/audit/pass-3-runtime/PASS-3.md` + named sub-agent notes | 16 (5, 9, 11, 12, 13, 17, 18, 19, 26, 29, 33, 34, 35, 36, 37, 47) | `pointer!` rename, path crate rename across multiple paragraphs, `bbnf` aggregator restructure, yaml proof, per-X table, SOTA numeric rows, BBNF self-host gate, generated visitor/path/tape budgets, diagnostic ledger, cookbook + migration receivers, fallback gate, hand-off triple columns, registry deletion gate | 3-4.5 hr | Renames touch many sites; aggregator restructure changes a 14-child tree to ≤10 children with sub-directory authoring; per-X table requires data from PASS-2 emission rows | medium — the cross-route data dependency on PASS-2 emission table (item #14) is the soft ordering coupling; can run after PASS-2 lands or speculatively if emission rows are duplicated |
| SYNTHESIS amendment | `restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, `restart/MASTER-PLAN.md` | 24 (1, 5, 10, 11, 12, 13, 15, 21, 22, 24, 25, 29, 30, 31, 32, 35, 36, 39, 40, 41, 42, 43, 44, 47) | Architecture: per-X table, Lock 13 verification table, declaration-crate review form fill-in, per-grammar LOC carry-up, exact SOTA rows in Master, escape clause deletion, B/C and C/E/H sequencing repair, migration crosswalk, branch/tag routing, archive citation, friction ledger with cookbook + diagnostics, benchmark metadata schema, registry deletion close gate | 4-6 hr | Largest item count; touches all three trio files; sequencing repairs at MASTER-PLAN.md:232-236, 267-272 require careful re-pairing of producer/consumer waves; partial pre-fills at items #29-#31, #40-#41 mean the agent must verify before editing | medium — risk of double-edit or ratification-only edit; must read current text and only patch deltas |

**Cumulative effort:** 13-19 hours of single-agent work, OR ~4-6 hours wall time with four parallel agents on disjoint outputs.

### 5.1 Effort breakdown by surgery class

| Surgery class | Items | Median minutes per item | Total minutes |
|---|---|---:|---:|
| Single-line/deletion (verbatim text supplied) | 6, 9, 18, 30, 38, 44 | 5 | 30 |
| Paragraph-level edit (verbatim direction supplied) | 1, 2, 7, 10, 17, 22, 28, 33 | 15 | 120 |
| Multi-paragraph subsection (new content authoring) | 8, 20, 23, 27, 35, 36, 39, 40, 41 | 30 | 270 |
| Tables — column additions (data exists) | 3, 14, 21, 24, 25, 26, 32, 37, 42 | 25 | 225 |
| Tables — full-row authoring (numeric or matrix) | 13, 15, 29, 31, 43 | 40 | 200 |
| Diagnostic strings (13 verbatim entries) | 34 | 60 (all 13 strings) | 60 |
| Multi-section coordinated edit | 4, 5, 11, 12, 16, 19 | 35 | 210 |
| Spec/grammar production change | 6 (counted above), 7 (above), 9 (above), 45, 46, 47 | 20 | 60 |
| **Total minutes** | | | **1175** ≈ **19.6 hours** |

The single-agent estimate of 13-19 hours brackets the surgery-class total of 19.6 hours. With wave-parallel dispatch the wall-time estimate of 6.5-9 hours is the relevant figure.

### 5.2 Per-route risk surface in detail

PASS-2 amendment carries the highest risk because the ownership move (item #1) is the binding prerequisite, and three secondary surgeries (#4 BIR payload, #14 runtime emission, #46 OpenFrame deletion) all depend on the BIR ownership being settled first. If item #1 is not committed before the PASS-2 agent starts on items #2, #4, #14, the result is a confused state where some PASS-2 text references `codegen/src/backend_ir/` and other text references `ir/src/backend_ir/`. Dispatch instruction must explicitly sequence #1 first within the PASS-2 wave.

SYNTHESIS amendment has the largest cumulative item count (24 items across three trio files). The risk is double-edit: many items (#15, #21, #29, #30, #31, #40, #41) are partial pre-fills where the trio already contains substantial text. The agent must read current text first and patch only the missing delta. If the agent re-authors instead of patching, the result is duplicated rows, contradictory counts, or removed pre-existing surgery. Dispatch instruction must explicitly say "verify current text and patch the delta."

PASS-3 amendment carries cross-route data dependency on PASS-2 emission table (#14) for the per-X table (#13). The PASS-3 agent can either (a) wait for PASS-2 #14 to land, then dispatch PASS-3 #13; or (b) dispatch in parallel with #13 columns derived from current `restart/audit/pass-2-codegen/PASS-2.md:295-308` per-grammar LOC table. Option (b) is faster but risks divergent column heads. Recommend option (a) — sequence within wave 3.

PASS-1 amendment is the lowest risk: BBNF surface edits (#6, #7, #8, #9) are line-precise; per-crate rationale (#20) is authoring; budget schema (#23) and diagnostics (#34 PASS-1 share) are subsection authoring. No cross-route dependency.

**Cross-amendment conflict surface:**

| Conflict | Routes | Mitigation |
|---|---|---|
| Items 1 + 4 (BIR ownership + payload) | PASS-1 + PASS-2 + SYNTHESIS | PASS-2 lands ownership move first; PASS-1 then refines payload; SYNTHESIS verifies |
| Item 11 (yaml proof) | all four routes | Architecture is authoritative; PASS-1/2/3 mirror Architecture text; dispatch SYNTHESIS first |
| Item 13 (per-X table) | SYNTHESIS-owned, fed by PASS-1/PASS-2/PASS-3 | data-flow only; SYNTHESIS dispatched last for #13 |
| Item 29 (SOTA table) | PASS-2 + PASS-3 + MASTER-PLAN | numeric rows already canonical at `ARCHITECTURE.md:1143-1152`; copy not invent |
| Item 34 (diagnostic ledger) | PASS-1/2/3 + SYNTHESIS | each route owns its diagnostic family; ledger is index, not single source |
| Item 37 (hand-off triples) | all PASS routes | three independent files; trivially parallel |
| Item 46 (OpenFrame) | PASS-1 + PASS-2 | deletion-only edit; both routes land independent text removals |

The cross-amendment surface is small. The largest serial dependency is #1 → #4: BIR ownership must land before payload refinement, because payload text needs to be authored under the new owning crate name.

**Likelihood of clean amendment as-is:** ~80%. The 20% risk concentrates on (a) item #12 routing repair, (b) verifying partial pre-fills before re-editing, (c) PASS-2 cascading edits if BIR ownership move is done sloppily.

## §6 Gate rerun command audit

Sixteen verification commands at `HARDENING-CONSOLIDATED.md:560-575`. Each evaluated for syntax, target precision, and post-condition.

| # | Command (verbatim) | Syntax | Target precision | Post-condition | Verdict |
|---|---|---|---|---|---|
| 1 | `rg -n "ParseStream\|rewrite-mode\|Unicode class algebra" restart/ARCHITECTURE.md restart/MIGRATION.md restart/MASTER-PLAN.md restart/audit/pass-*/*.md` | valid `rg` regex (escaped `\|` is wrong inside double quotes — must be raw `|`); | targets are correct paths | post-condition is "classifies every match" — not a boolean | needs-tightening: must specify explicit allowed-vs-forbidden classification |
| 2 | `rg -n "bbnf-path\|bbnf-test-fixtures\|path!" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-3-runtime` | valid; same alternation note | targets correct | post-condition "no proposed public/internal crate or macro names except migration references" is good but needs example of allowed exception | needs-tightening |
| 3 | `rg -n "codegen/src/backend_ir" restart/ARCHITECTURE.md restart/audit/pass-2-codegen` | valid | target correct | "returns no proposed ownership path" — clear boolean | well-formed |
| 4 | `rg -n "fixtures/yaml" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-*` | valid | target correct | "returns no Lock 14 onboarding allowance" — clear; current Architecture already returns zero | well-formed (partial pre-pass) |
| 5 | `rg -n "@recover" restart/ARCHITECTURE.md restart/audit/pass-3-runtime` | valid; `@` is not a special regex char | target correct | "only compatibility-alias text or zero" — boolean OR exception | well-formed |
| 6 | `rg -n "OpenFrame" restart/audit/pass-1-substrate restart/audit/pass-2-codegen restart/MASTER-PLAN.md` | valid | target correct | "every match as deletion pathology, never preservation" — requires manual classification | needs-tightening: post-condition is not zero-match, requires reviewer judgment |
| 7 | `rg -n "GrammarIR" restart/audit/pass-2-codegen/PASS-2.md restart/ARCHITECTURE.md` | valid | target correct | "finds a lowerer import-deny gate" — must find at least one match with deny-gate context | well-formed |
| 8 | `rg -n "__EAGER_EMPTY_PATH\|CursorDecision::Skip" restart/MASTER-PLAN.md restart/MIGRATION.md` | valid; `::` is fine | target correct | "finds explicit Lock 3 gates" — must find both | well-formed; current `ARCHITECTURE.md:763-764` already has these |
| 9 | `rg -n "twitter\|canada\|citm\|bootstrap\|animate\|On-Demand" restart/MASTER-PLAN.md restart/audit/pass-3-runtime/PASS-3.md` | valid | target correct (but notable omission: `restart/ARCHITECTURE.md:1147-1152` is the canonical gate row source — not in target list) | "finds numeric competitor rows" — soft post-condition | needs-tightening: should include ARCHITECTURE.md to verify Master/PASS-3 mirror the Architecture canonical numbers |
| 10 | `rg -n "receiver\|blocker\|receiving gate" restart/MIGRATION.md restart/MASTER-PLAN.md restart/audit/pass-*` | valid | target correct | "finds complete carry-ledger columns" — needs minimum match count | needs-tightening |
| 11 | `rg -n "yaml.bbnf\|workspace.metadata.bbnf.grammars.yaml" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-*` | valid; `.` is regex any-char but works for substring match | target correct | "finds the two-surface proof" — must find both terms | well-formed |
| 12 | `rg -n "generated_loc\|regen_wall\|xtask" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-*` | valid | target correct | "finds per-grammar and per-wave budgets" — soft | needs-tightening: should require count >= 9 (per-grammar) + count >= 8 (F + H waves) |
| 13 | `rg -n "BBNF-LIFE\|BBNF-LAYOUT\|BBNF-OPT\|BBNF-GRAMMAR\|BBNF-POINTER\|lookbehind\|HostSignature" restart/ARCHITECTURE.md restart/audit/pass-*` | valid | target correct | "finds committed diagnostic strings" — must find each family at least once | well-formed |
| 14 | `rg -n "child count\|500 LOC\|exception rationale" restart/ARCHITECTURE.md restart/MASTER-PLAN.md` | valid | target correct | "finds the Lock 13 verification table" — soft | needs-tightening: should specify count rows |
| 15 | `rg -n "declaration-crate review\|why metadata\|deletion path\|reviewer" restart/ARCHITECTURE.md restart/MIGRATION.md` | valid | target correct | "finds the rare escape form" — must find all four heads | well-formed |
| 16 | `rg -n "CPU model\|compiler flags\|input hash\|competitor version\|warmup\|sample" restart/MASTER-PLAN.md restart/MIGRATION.md` | valid | target correct (but `restart/ARCHITECTURE.md:1147-1152` already has CPU/OS/compiler flags/input hash — should be in target list) | "finds benchmark metadata gates" — soft | needs-tightening: include ARCHITECTURE.md target |

**Aggregate gate-rerun audit:**

| Verdict | Count | Items |
|---|---:|---|
| Well-formed | 9 | 3, 4, 5, 7, 8, 11, 13, 15, |
| Needs-tightening | 7 | 1, 2, 6, 9, 10, 12, 14, 16 |
| Wrong | 0 | none |

Tightening required is mostly: (a) specify minimum match counts for "finds rows X, Y" post-conditions; (b) add `ARCHITECTURE.md` to target list for SOTA/metadata/diagnostic rerun where the trio is the canonical source; (c) replace soft "classifies every match" with explicit allowed-vs-forbidden patterns.

None of the commands are syntactically broken or target the wrong files. The audit can run as-is; the post-condition lift is purely a tightening for next-pass auditor automation.

### 6.1 Recommended tightened post-conditions

| Cmd # | Original post-condition | Tightened post-condition |
|---|---|---|
| 1 | "classifies every match as stale-input discussion or deleted surface" | "every match must appear inside a `### Stale` heading region OR a `## Authority` ledger row OR a deletion ledger; non-classified matches fail" |
| 2 | "no proposed public/internal crate or macro names except migration references" | "matches must appear under a heading labelled `## Migration` or `## Inheritance` and must include the rename target; unlabelled matches fail" |
| 6 | "every match as deletion pathology, never preservation" | "every match must occur in `OpenFrame deletion`, `OpenFrame removed`, or `no OpenFrame` adjacency; matches in `OpenFrame preservation`, `OpenFrame retained`, or `OpenFrame survives` fail" |
| 9 | "finds numeric competitor rows" | "finds at least one row containing each of: `380` (twitter), `2.8` (canada), `750` (citm), `3.0` or `3.16` (bootstrap), `1.6` or `1.97` (animate), `5 GB/s` and `7 GB/s` (simdjson OD); fewer than six numeric rows fail" |
| 10 | "finds complete carry-ledger columns" | "finds at least 9 carry rows (TS production, BD.W5 parity, PASS-1 reconciliation, PASS-3 API docs, publication, fixtures, `path-ts`, WASM ABI, host escape) each with all three columns receiver/blocker/gate" |
| 12 | "finds per-grammar and per-wave budgets" | "finds at least 9 per-grammar LOC rows (one per seed grammar) AND at least 8 wave rows (F.W0-F.W5 + H.W3-H.W5); fewer fails" |
| 14 | "finds the Lock 13 verification table" | "finds a table with at least 24 rows (one per non-generated handwritten crate) containing `child count`, `500 LOC`, and `exception rationale` columns" |
| 16 | "finds benchmark metadata gates" | "finds all 8 metadata fields named (CPU, OS, compiler flags, input hash, competitor version, bbnf commit, warmup, sample) bound to H.W4 / H.W5 / J.W1 specifically" |

These tightened post-conditions are not blockers; they are specifications for the next-pass auditor's automation harness.

### 6.2 Pre-existing matches in current text

For each gate-rerun command, the current target text already contains material that will produce matches. The amendment dispatch must instruct each route agent to verify these pre-existing matches are not regressed:

| Cmd # | Pre-existing match locations |
|---|---|
| 3 | `ARCHITECTURE.md:924` says `ir::backend_ir`; `MIGRATION.md:179, 330, 727` confirm. Pre-existing: SYNTHESIS already aligns. PASS-2 must converge. |
| 4 | `ARCHITECTURE.md:1170-1186` has zero `fixtures/yaml`. Pre-existing: SYNTHESIS already aligns. PASS-3 fixture rows at `PASS-3.md:272-289` need amendment. |
| 8 | `ARCHITECTURE.md:763-764` has `__EAGER_EMPTY_PATH` and `CursorDecision::Skip` cursor gate rows. Pre-existing: SYNTHESIS already aligns. Must propagate to MASTER-PLAN B/H gates. |
| 9 | `ARCHITECTURE.md:1147-1152` has all six SOTA gate rows numerically. Pre-existing: SYNTHESIS authoritative. PASS-2 trajectory at `PASS-2.md:321-335` and PASS-3 bench at `PASS-3.md:291-303` need amendment to mirror. |
| 11 | `ARCHITECTURE.md:1173-1174` and `MASTER-PLAN.md:110, 401`, `MIGRATION.md:692-701` all have yaml two-surface text. Pre-existing: SYNTHESIS aligns. PASS surfaces need amendment. |
| 16 | `ARCHITECTURE.md:1147-1152` has CPU, OS, compiler flags, input hash columns; `MIGRATION.md:725` has hardware profile; `MASTER-PLAN.md:656` has metadata schema reference. Pre-existing: partial. H/J binding rows need amendment. |

## §7 Recommended amendment dispatch order

Given the dependency graph (§3), routing accuracy (§4), and effort estimates (§5), the dispatch order minimizes cross-amendment conflict and respects critical-path ordering.

### Wave 1 — landing the foundations (parallel, ~2-3 hr wall)

| Agent | Route | Items |
|---|---|---|
| Agent A1 | PASS-2 | item 1 (BIR ownership move) — strict prerequisite; lands first within PASS-2 |
| Agent A2 | PASS-1 | items 6, 7, 8, 9 partial (BBNF surface — `@host fn`, lookbehind, chain) — disjoint with PASS-2 |
| Agent A3 | SYNTHESIS | items 11 (yaml proof verification — already substantively present in `ARCHITECTURE.md:1163-1199`), 12 (fixture separation — verify; current text already returns zero), 44 (archive citation correction at `MASTER-PLAN.md:85-86`) |
| Agent A4 | MASTER-PLAN | items 40, 41 (B/C and C/E/H sequencing — verify partial pre-fill at `MASTER-PLAN.md:280, 281`) |

Wave 1 gate: `rg -n "codegen/src/backend_ir" restart/audit/pass-2-codegen/PASS-2.md` returns zero; `restart/audit/pass-1-substrate/PASS-1.md` `HostFn` production has block body.

### Wave 2 — core surgeries (parallel, ~2-3 hr wall)

| Agent | Route | Items |
|---|---|---|
| Agent B1 | PASS-2 | items 2 (lowerer import-deny), 4 (BIR payload), 5 (PASS-3 emission contract), 14 (runtime emission table), 27 (non-generated LOC), 28 (xtask baseline), 46 (OpenFrame deletion) |
| Agent B2 | PASS-1 | items 3 (Grammar IR schema), 4 (BIR payload — co-author), 20 (per-crate rationale), 23 (budget schema), 37 (hand-off triple columns), 38 (independent-proceed deletion), 45 (closure reframe), 46 (OpenFrame deletion — co-author) |
| Agent B3 | PASS-3 | items 17 (path crate rename), 18 (`pointer!` rename), 19 (`bbnf` aggregator restructure), 33 (BBNF self-host gate), 37 (hand-off triple columns) |
| Agent B4 | SYNTHESIS | items 13 (per-X table), 21 (Lock 13 verification table), 22 (package-name routing), 30 (delete final SOTA escape — verify partial pre-fill), 31 (early H thresholds), 32 (benchmark metadata), 42 (migration crosswalk), 43 (branch/tag routing) |

Wave 2 gates: gate-rerun commands #2, #3, #4, #5, #7, #8, #11, #15 from §6.

### Wave 3 — fill-in and ledgers (parallel, ~1.5-2 hr wall)

| Agent | Route | Items |
|---|---|---|
| Agent C1 | PASS-2 | items 24 (per-grammar LOC carry — feeds SYNTHESIS), 29 (SOTA numeric rows), 34 (diagnostic ledger), 37 (hand-off triple columns), 39 (carry ledger) |
| Agent C2 | PASS-3 | items 11 (yaml mirror), 12 (fixture rows in `PASS-3.md:272-289` — the actual location for #12 routing fault), 13 (per-X table mirror), 26 (PASS-3 generated budget), 29 (SOTA numeric rows), 34 (diagnostic ledger), 35 (cookbook + migration receivers), 36 (incremental fallback), 47 (registry deletion gate) |
| Agent C3 | SYNTHESIS | items 1 (BIR ownership verification), 5 (PASS-3 emission contract verification), 10 (input-normalization table), 15 (declaration-crate review form fill), 24 (per-grammar LOC carry-up), 25 (wave-level F/H budgets), 29 (SOTA mirror), 34 (diagnostic ledger SYNTHESIS index), 35 (cookbook receivers), 36 (fallback gates), 39 (carry ledger), 47 (close gate) |

Wave 3 gates: all sixteen gate-rerun commands.

### Wave 4 — hardening rerun (single auditor, ~1 hr)

Run all sixteen `rg` commands from `HARDENING-CONSOLIDATED.md:560-575` plus the tightened post-conditions from §6. Verify match counts where applicable.

**Total wall time:** 6.5-9 hours across four waves. Critical path through PASS-2 (BIR ownership → payload → emission table) is the binding chain.

### 7.1 Per-wave dispatch instruction template

Each dispatch instruction must contain:

| Section | Content |
|---|---|
| Hard cap | Per-route minutes; e.g., PASS-2 wave 2 = 90 min, SYNTHESIS wave 2 = 120 min |
| Read-first | Current target text at the cited line ranges; verify pre-existing matches before editing |
| Item list | Verbatim items routed to this wave for this agent |
| Cross-route note | Sequencing constraints — e.g., wave 1 PASS-2 must commit BIR ownership move before wave 2 starts |
| Output contract | Exact file paths the agent may touch; commit message template |
| Halt condition | If the agent encounters ambiguity in surgery direction, halt and surface; do not invent |

### 7.2 Recommended commit cadence

Each wave produces one commit per route. Recommended commit messages:

| Wave | Route | Commit message |
|---|---|---|
| 1 | PASS-2 | `docs(restart/audit/pass-2-codegen): amend BIR ownership move per HARDENING punch #1` |
| 1 | PASS-1 | `docs(restart/audit/pass-1-substrate): amend BBNF surface per HARDENING punch #6, #7, #8, #9` |
| 1 | SYNTHESIS | `docs(restart): verify yaml proof and fix archive citation per HARDENING punch #11, #12, #44` |
| 1 | MASTER-PLAN | `docs(restart): verify B/C and C/E/H sequencing per HARDENING punch #40, #41` |
| 2 | PASS-2 | `docs(restart/audit/pass-2-codegen): amend payload, emission, ledgers per HARDENING punch #2, #4, #5, #14, #27, #28, #46` |
| 2 | PASS-1 | `docs(restart/audit/pass-1-substrate): amend Grammar IR schema, rationale, budget, hand-offs per HARDENING punch #3, #4, #20, #23, #37, #38, #45, #46` |
| 2 | PASS-3 | `docs(restart/audit/pass-3-runtime): amend path/pointer/aggregator/self-host/handoffs per HARDENING punch #17, #18, #19, #33, #37` |
| 2 | SYNTHESIS | `docs(restart): amend per-X table, Lock 13, package routing, SOTA, sequencing repair, migration, branch routing per HARDENING punch #13, #21, #22, #30, #31, #32, #42, #43` |
| 3 | PASS-2 | `docs(restart/audit/pass-2-codegen): amend SOTA rows, diagnostics, carry per HARDENING punch #24, #29, #34, #37, #39` |
| 3 | PASS-3 | `docs(restart/audit/pass-3-runtime): amend yaml, fixture rows, per-X mirror, generated budget, SOTA, diagnostics, cookbook, fallback, registry per HARDENING punch #11, #12, #13, #26, #29, #34, #35, #36, #47` |
| 3 | SYNTHESIS | `docs(restart): consolidate ledgers, fill review form, propagate budgets, normalization table, registry close gate per HARDENING punch #1, #5, #10, #15, #24, #25, #29, #34, #35, #36, #39, #47` |
| 4 | hardening rerun | `docs(restart/audit/hardening): rerun 16-command gate after amendments` |

This cadence yields 11 amendment commits + 1 rerun commit. The cadence is structured to prevent (a) cross-route file overlap within a wave; (b) one mega-commit hiding multi-item edits.

### 7.3 Halt conditions

Any of the following triggers halt and report:

- An item's surgery direction conflicts with current target text in a way the punch did not anticipate (e.g., a numeric row already exists at a different value than punch #29 prescribes).
- A cross-route dependency reveals data the upstream route has not produced (e.g., PASS-3 #13 per-X table needs PASS-2 #14 emission columns that do not yet exist).
- A gate-rerun command after a commit returns matches the punch did not predict (e.g., #5 `@recover` returns more than the compatibility-alias text after amendment).
- An amendment touches a file outside its routed output contract.

In all halt cases the agent reports the conflict; no improvisation.

## §8 Reviewer-D verdict

**PUNCH LIST EXECUTABLE WITH ROUTING REPAIRS**

The 47-item punch list is overwhelmingly executable as-is. 42 items carry verbatim or near-verbatim surgery, applicable verification commands, named owners, and correct scope. The dependency graph has a depth-7 critical path (BIR ownership → payload → runtime emission → per-X table → yaml proof → fixture separation → registry deletion), but most clusters are parallelizable across four amendment routes (PASS-1, PASS-2, PASS-3, SYNTHESIS). The four-wave dispatch in §7 resolves to 6.5-9 hours of wall time with four agents.

The reasons for **WITH ROUTING REPAIRS** rather than **AS-IS**:

| Repair | Severity | Item | Surgery |
|---|---|---|---|
| Item #12 (fixture separation) is mis-routed | hard | The cited `ARCHITECTURE.md:1132-1138; 1151-1162` is the SOTA gate / Generated LOC budget section. Actual onboarding allowed-changes block lives at `ARCHITECTURE.md:1170-1186` and contains no `fixtures/yaml/*` entry — `grep -n "fixtures/yaml" restart/ARCHITECTURE.md` returns zero. The fixture-separation surgery should target `restart/audit/pass-3-runtime/PASS-3.md:272-289` where the four-fixture-dir sketch actually lives. | Re-route #12 to PASS-3 amendment with target `PASS-3.md:272-289`. SYNTHESIS verifies-only. |
| Items #6, #9 missing SYNTHESIS co-route | partial | `@host fn` block-body production also appears at `ARCHITECTURE.md:986`; `@error` recovery family is canonical at `ARCHITECTURE.md:946-994`. | Add SYNTHESIS verification for #6 and #9 alongside primary PASS-1/PASS-3 edits. |
| Items #15, #21, #29, #30, #31, #40, #41 are partial pre-fills | soft | The SYNTHESIS trio already contains the substantive surgery in current text: `ARCHITECTURE.md:711-719` has the declaration-crate form skeleton; `ARCHITECTURE.md:1220-1228` has the Lock 13 exception ledger; `ARCHITECTURE.md:1143-1152` has the SOTA numeric rows; `MASTER-PLAN.md:506` no longer carries "or formally routed"; `MASTER-PLAN.md:280-282` partially repairs B/C and C/E/H sequencing. | Amendment dispatch must instruct each agent to read current text first and patch only the missing delta, not re-author the section. |
| Eight gate-rerun commands need post-condition tightening | soft | Commands #1, #2, #6, #9, #10, #12, #14, #16 use "classifies every match" or "finds X" without minimum-count or allowed-vs-forbidden specificity. | Tighten before next-pass automation; not a blocker for current dispatch. |

The 47 items themselves are not at fault. The faults are concentrated in the routing matrix at consolidated §5 — primarily one mis-routed citation and several partial pre-fills the consolidated did not flag. The amendment dispatch can proceed once those routing repairs are made and dispatch instructions tell each agent to verify current target text before editing.

The cohort verdict matches the consolidated §1 finding (AMENDMENT-REQUIRED, no re-draft). The punch list is sized correctly: 47 items is not so large that it forces re-draft, and not so small that it under-states the surgery. The routing matrix at consolidated §5 is **mostly correct** with one hard repair and several soft repairs.

### 8.1 Defects classified

| Defect class | Count | Severity | Action |
|---|---:|---|---|
| Mis-routed citation | 1 | hard | Re-route #12 fixture separation to PASS-3 amendment with target `PASS-3.md:272-289` |
| Single-route punch needs additional route | 2 | partial | Add SYNTHESIS verification for #6 (`@host fn`) and #9 (`@recover`) |
| Partial pre-fill not flagged | 7 | soft | Dispatch instruction must say "verify current text and patch the delta" for #15, #21, #29, #30, #31, #40, #41 |
| Gate-rerun post-condition under-specified | 8 | soft | Tighten post-conditions for #1, #2, #6, #9, #10, #12, #14, #16 (per §6.1) |
| Borderline concreteness (score 3) | 2 | soft | Tighten #5 (PASS-3 emission contract) and #16 (grep classification bookkeeping) |

Total defects: 20 across 47 items. None of these defects rises to the level of "punch list cannot dispatch." All can be addressed in the dispatch instruction template (§7.1) without rewriting the consolidated §4 punch list itself.

### 8.2 Confidence in dispatch outcome

| Wave | Likelihood of clean amendment | Reason |
|---|---:|---|
| Wave 1 | 85% | small surgeries, partial pre-fills mostly verifiable; PASS-2 ownership move is the binding edit |
| Wave 2 | 75% | largest wave by item count; PASS-2 cascade if BIR ownership not committed in wave 1; SYNTHESIS partial pre-fills must be detected first |
| Wave 3 | 80% | data-flow only; depends on wave 2 emission table and per-X data |
| Wave 4 | 95% | gate-rerun is mechanical; only soft post-conditions to tighten |

Mean confidence: ~83%. Risk concentrates in wave 2 PASS-2 + SYNTHESIS routes. If wave 1 PASS-2 BIR ownership commit fails or stalls, wave 2 PASS-2 cannot proceed cleanly. Mitigation: explicit halt-and-report for wave 1 PASS-2 if ownership commit is not in by minute 30.

### 8.3 Items that should NOT be amended

Several items in the consolidated punch list are partial pre-fills where the SYNTHESIS trio already substantively contains the surgery. The amendment agents must NOT re-author these sections; they must verify and patch only the missing delta.

| # | Pre-existing surgery in trio | Action |
|---|---|---|
| 11 | yaml two-surface proof at `ARCHITECTURE.md:1163-1199` | verify only; no edit unless missing column |
| 12 | fixtures/yaml absent in current onboarding allow-list at `ARCHITECTURE.md:1170-1186` | verify only for SYNTHESIS; PASS-3 amendment edits #12 surgery in `PASS-3.md:272-289` |
| 15 | declaration-crate review form at `ARCHITECTURE.md:711-719` | extend with deletion-path + reviewer columns; do not re-author |
| 21 | Lock 13 exception ledger at `ARCHITECTURE.md:1220-1228` | extend with per-crate child-count rows; do not re-author |
| 29 | SOTA gate rows at `ARCHITECTURE.md:1143-1152` | mirror to PASS-2/PASS-3/MASTER-PLAN; do not re-author Architecture |
| 30 | "or formally routed" already largely removed; `MASTER-PLAN.md:506` says "misses require amendment before close" | verify clean removal; no edit needed unless residual phrasing |
| 31 | H.W4/H.W5 rows at `MASTER-PLAN.md:444-445` already say "with metadata and numeric deltas" | extend with explicit numeric thresholds; do not re-author |
| 40 | C.W2 at `MASTER-PLAN.md:280` already says "consumes ShapeFacts in a C fixture" | choose final repair (move ShapeFacts before B.W3, etc.) and ratify |
| 41 | C.W3 at `MASTER-PLAN.md:281` already says "Facts feed E-owned BIR snapshots, not placeholder hints" | verify cleanly; ratify |
| 44 | `MASTER-PLAN.md:85-86` says "per Lock 12" — the punch claims "per Lock 10" but verification of current text shows Lock 12 already cited | verify only; no edit unless residual mis-label |

The SYNTHESIS amendment agent must read each of these locations and decide whether the surgery is needed at all, or whether ratification is sufficient. The dispatch instruction must explicitly ask for the verify-then-patch pattern.

### 8.4 Final readiness statement

The hardening cohort is ready to dispatch the amendment wave. The 47-item punch list is concretely surgical; the routing matrix has one hard repair (#12 fixture separation) and several soft pre-fill flags; the gate-rerun checklist runs as-is with tightening recommended for next-pass automation. The four-wave dispatch in §7 produces 11 commits + 1 rerun commit across 6.5-9 hours of wall time, with mean confidence ~83% in clean execution.

The next step is the routing-repair dispatch instruction template (§7.1) followed by wave 1 — not tranche full-spec drafting, and not another round of hardening. The hardening has done its work; the amendments are surgical, executable, and bounded.

Hereupon the next step is the four-wave amendment dispatch in §7, with the routing repairs in §8 applied at dispatch instruction time.
