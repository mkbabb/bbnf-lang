# HARDENING-MASTER-PLAN

## §1 Target identification

Target: `MASTER-PLAN`.

Audited target outputs:

| Path | Lines audited |
|---|---:|
| `restart/MASTER-PLAN.md` | 695 |
| `restart/ARCHITECTURE.md` | 1213 |
| `restart/MIGRATION.md` | 701 |

Audit base commit: `015317db283ea1e9652401a6a7438ffa5baf028c`.

Required context read: `restart/README.md`, `restart/locks/LOCKS.md`, `docs/precepts/instructions/STYLE.md`, `docs/precepts/instructions/LESSONS-LEARNED.md`, `docs/precepts/instructions/CONSUMING.md`, `restart/prompts/{PASS-1-SUBSTRATE,PASS-2-CODEGEN,PASS-3-RUNTIME,SYNTHESIS}.md`, `restart/corpora/{CENSUS,MODULES,RESTART-SKETCH,SOTA}.md`, and `restart/inheritance/INDEX.md`.

PASS hardening signal: no committed files existed under `restart/audit/hardening/` at start. PASS syntheses under `restart/audit/pass-1-substrate/`, `restart/audit/pass-2-codegen/`, and `restart/audit/pass-3-runtime/` were read as ordinary synthesis context where the target cites them.

Time consumed: one bounded MASTER-PLAN hardening slot.

## §2 Cohort verdict

| Lane | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|
| 1 Lock-Adherence | amendment-required | 10 | 4 | 0 | Keep the governing shape; amend Lock 2, Lock 3, Lock 8, and Lock 14 proof text. |
| 2 Sequencing Discipline | amendment-required | 6 | 3 | 0 | Repair B/C and C/E/H consumer timing before full tranche drafting. |
| 3 Cohesion | amendment-required | 3 | 3 | 0 | Add verifiable crosswalks for migration families, tree discipline, and close gates. |
| 4 SOTA Anchoring | amendment-required | 1 | 3 | 1 | Inline competitor, dataset, platform, and target numbers into H/J gates; discard "formally routed" as a final perf gate. |
| 5 Grammar-Authoritative Discipline | amendment-required | 3 | 2 | 1 | Remove fixture as an onboarding surface; add per-X proof tables for all-grammar claims. |
| 6 Generated-Code + LOC Budget | amendment-required | 2 | 3 | 0 | Carry PASS-2 budgets into wave-level F/H/J gates, including xtask wall budgets. |
| 7 Friction Forecast | amendment-required | 0 | 6 | 0 | Add cookbook and verbatim diagnostic gates for path, lifetime, layout, Pratt/SIMD, migration, and YAML onboarding. |
| 8 Carry & Deferral Audit | amendment-required | 2 | 4 | 1 | Every unresolved item needs receiver, blocker, and receiving gate; discard open-ended SOTA routing. |
| 9 Greenfield Discipline | amendment-required | 3 | 3 | 1 | Keep the greenfield thesis; remove residual implementation-time ambiguity and mixed-fate inheritance. |

Final decision: **AMENDMENT-REQUIRED**.

Total verdicts: **KEEP 30 / REINVENT 31 / DISCARD 4**.

Punch-list size: **16**.

The trio is architecturally viable. It honors the settled tape/direct substrate, rejects `ParseStream`, rejects rewrite-mode, routes Unicode below BBNF, and avoids default per-grammar declaration crates. It does not yet pass the gate because several proof surfaces are weaker than their claims: the YAML onboarding test admits a third surface, SOTA gates lack numbers at the tranche rows that execute them, generated-budget authority stays mostly in PASS-2 instead of the master plan, and deferrals do not consistently name blocker plus receiver gate.

## §3 Lane 1 - Lock-Adherence

Lane standard: each settled lock is binding. The target need not restate every source, but each lock must have an executable proof site in the trio and no conflicting plan logic.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/MASTER-PLAN.md:23-28`; `restart/ARCHITECTURE.md:21-23`; `restart/MIGRATION.md:17` | Lock 1, tape/direct substrate | The trio keeps tape as the runtime substrate and joins it to direct views. | This follows current authority and blocks `ParseStream`, OpenFrame ladders, and parallel substrate work. | The old corpus still contains anti-tape research, so execution gates must keep proving the union rather than the name. | A direct-only plan would be simpler, but it would relitigate a lock and break current authority. | KEEP |
| `restart/MASTER-PLAN.md:609`; `restart/ARCHITECTURE.md:715-747`; `restart/ARCHITECTURE.md:919-926` | Lock 2, layout-lowering term | The trio claims Lock 2 ownership, but the pipeline still names `type inference`, `TypeFacts`, and type producers as first-class pass vocabulary. | Type facts are real and useful. | Lock 2 says layout lowering is the canonical IR pass name and retired type-projection terms are faults. The target does not reconcile the two vocabularies. | Keep HM/CSP type checking as a subroutine, but make `passes/layout` and `LayoutFacts` the named lowering surface. | REINVENT |
| `restart/MASTER-PLAN.md:610`; `restart/MASTER-PLAN.md:232-236`; `restart/MIGRATION.md:656-665` | Lock 3, cursor-parse plus byte-skip | The target owns the lock in a table, but B waves and migration gates do not spell out cursor consultation, byte-skip generation, or eager empty-path elision. | Runtime substrate work is already in the correct tranche. | A table row is not an implementation gate. The Era V failure mode returns if cursor/skip is postponed behind a generic runtime shell. | Add explicit B/H gates for `__EAGER_EMPTY_PATH` elision and cursor `Skip` lowering. | REINVENT |
| `restart/MASTER-PLAN.md:260-272`; `restart/ARCHITECTURE.md:30`; `restart/ARCHITECTURE.md:744` | Lock 4, orthogonal optimization | CSP, egraph, miners, and cost model remain separate and compose through facts. | This honors the no-hypergraph lock and leaves sister crates publishable. | C.W4 still needs concrete bridge API tests, but the target names them. | A fused solver would simplify some global choices but would violate the lock. | KEEP |
| `restart/MASTER-PLAN.md:30-34`; `restart/ARCHITECTURE.md:813-844`; `restart/MIGRATION.md:645-654` | Lock 5, Backend IR lowerers | Backend IR is the lowerer contract and Grammar IR walking is banned. | The BIR table, lowerer matrix, and migration grep support the claim. | The final implementation must still enforce crate boundaries. | A trait emitter walking Grammar IR would be faster to port, but repeats the old backend fault. | KEEP |
| `restart/MASTER-PLAN.md:116-117`; `restart/MASTER-PLAN.md:358`; `restart/ARCHITECTURE.md:1099-1100` | Lock 6, committed source generation | Generated Rust is emitted by xtask and checked by equality. | This preserves greppable output and avoids proc-macro codegen. | The xtask wall budget is not carried into master gates; Lane 6 covers that. | Proc-macro generation would hide large output and break incremental expectations. | KEEP |
| `restart/MASTER-PLAN.md:389-401`; `restart/ARCHITECTURE.md:266-287`; `restart/MIGRATION.md:371-392` | Lock 7, path split | The trio uses `path`, `path-core`, and `path-ts` and migrates current hardcoded path registries. | It removes the old three-path duplication and keeps Rust/TS semantics shared. | Crate package aliases need final policy so `bbnf-path-core` does not reappear accidentally. | A single proc-macro crate cannot serve TS; the split is necessary. | KEEP |
| `restart/MASTER-PLAN.md:119`; `restart/MASTER-PLAN.md:433-434`; `restart/MASTER-PLAN.md:495`; `restart/ARCHITECTURE.md:1104-1110` | Lock 8, SOTA gates | Architecture lists the target family, but executable tranche rows use generic "progress report" and "formally routed" language. | The source numbers exist in README and SOTA. | The master plan lets perf gates close without competitor, dataset, platform, and target number at the row that executes. | Inline the exact rows: sonic-rs/simd-json M1 Pro JSON, lightning-css M1 Pro CSS, simdjson OD x86/M-series. | REINVENT |
| `restart/ARCHITECTURE.md:190-210`; `restart/MASTER-PLAN.md:616` | Lock 9, slice-borrow API | `parse`, `parse_in`, and `parse_owned` are first-class. | The API surfaces borrow default, arena opt-in, and owned escape. | Friction docs are weak; Lane 7 covers that. | An arena-first API would hide allocations and fight the lock. | KEEP |
| `restart/MASTER-PLAN.md:421-434`; `restart/ARCHITECTURE.md:887`; `restart/ARCHITECTURE.md:909` | Lock 10, Pratt/SIMD auto | Pratt and SIMD are mined and no `@pratt` or `@simd` directive appears. | The recognizer facts and BIR variants make the mechanism testable. | Misfire diagnostics are absent. | Manual annotations would be easier for authors but violate the auto-detect lock. | KEEP |
| `restart/MASTER-PLAN.md:81`; `restart/ARCHITECTURE.md:176`; `restart/MIGRATION.md:394-437` | Lock 11, sister crates | `egraph`, `egraph-derive`, `csp-solver`, `parse-that`, and `simd-scan` stay generic. | This keeps the substrate publishable and free of grammar terms. | The old lock text names `bbnf-regex`; current authority routes regex under `parse-that`. | Restoring `bbnf-regex` as a user-facing crate would fight the prefix rule. | KEEP |
| `restart/MASTER-PLAN.md:199`; `restart/MASTER-PLAN.md:580`; `restart/MIGRATION.md:439-454` | Lock 12, archive `ser`/`gorgeous` | Archive work is first in A and production membership is removed. | The migration target is clear and body-bearing commit evidence is required. | `restart/MASTER-PLAN.md:85-86` says "per Lock 10" while citing the Lock 11/12 region. | Fix the citation label, but the plan substance is right. | KEEP |
| `restart/MASTER-PLAN.md:118`; `restart/ARCHITECTURE.md:543-545`; `restart/ARCHITECTURE.md:1173-1181` | Lock 13, no god directories | The target sets 4-10 child and 500 LOC gates. | It names lint commands and maps current oversized files. | Some architecture trees include one-child or special-case dirs without an exception table, and the master does not bind those exceptions to tranche rationales. | Add an exception ledger for proc-macro roots, generated dirs, and intrinsic SIMD files. | REINVENT |
| `restart/ARCHITECTURE.md:1127-1162`; `restart/MASTER-PLAN.md:110`; `restart/MIGRATION.md:699-701` | Lock 14, full grammar generalization | The trio makes grammar source plus metadata the onboarding rule. | No generic-code match arms were found in the target, and registry deletion gates are strong. | The YAML test allows `fixtures/yaml/*` as an allowed change, giving a third onboarding surface. | Fixtures are useful after onboarding, but the proof test must be two surfaces only. | REINVENT |

Lane verdict: **amendment-required**. Counts: KEEP 10 / REINVENT 4 / DISCARD 0.

## §4 Lane 2 - Sequencing Discipline

Lane standard: a producer must have a same-wave or next-wave consumer. Stub tranches may be high-level, but the consumer cannot be merely eventual.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/MASTER-PLAN.md:197-203` | Tranche A waves | Archive, skeleton, metadata, grammar API, and lints are consumed inside A or by the next tranche. | A.W0/A.W1/A.W2 have immediate build or validation gates. | A.W3 minimal grammar API is still broad, but it has a seed parse consumer. | Splitting A further would add ceremony without reducing risk. | KEEP |
| `restart/MASTER-PLAN.md:232-236`; `restart/MASTER-PLAN.md:267-272` | B direct builder versus C ShapeFacts | B.W3 builds direct views before C.W2 produces `ShapeFacts`, yet C.W2 says the direct builder consumes `ShapeFacts`. | B owns runtime identity; C owns inference facts. | The current sequence asks B to consume a fact that does not exist yet. | Reorder B/C, split B direct builder into a shell plus C-driven materialization, or change the C.W2 consumer gate. | REINVENT |
| `restart/MASTER-PLAN.md:270-272`; `restart/MASTER-PLAN.md:333-337` | C recognizer/extraction outputs | C.W3/C.W5 create facts and selected alternatives before E/H produce real BIR and performance consumers. | Early facts keep optimizer work visible. | Placeholder BIR hints are weak consumers and invite substrate-first closure. | Move recognizer/BIR hint proof into E or H, or add same-wave snapshot tests that construct real BIR nodes. | REINVENT |
| `restart/MASTER-PLAN.md:300-304`; `restart/MASTER-PLAN.md:364-369` | D extension outputs into F generation | Extension parser, host, chain, layout, and error work feed F.W2 generation. | The next consuming tranche is explicit. | D.W4 also routes Unicode to regex; the receiver gate needs exact regex tests. | Keeping D before F is correct; strengthen the regex gate elsewhere. | KEEP |
| `restart/MASTER-PLAN.md:333-337`; `restart/MASTER-PLAN.md:364-369` | E Backend IR into F lowerer | E creates BIR/VM/lowerer boundary and F consumes it immediately. | This is the best consumer pairing in the plan. | None material. | A grammar-walking lowerer would be faster but violates Lock 5. | KEEP |
| `restart/MASTER-PLAN.md:364-369`; `restart/MASTER-PLAN.md:397-401` | F generated runtime into G path/value | Generated runtime appears before path/value/visitor and future grammar proof. | This pairs runtime output with public consumers. | Generated LOC budget must be present before F.W5, not just at close. | Keep ordering; add budget gates in Lane 6. | KEEP |
| `restart/MASTER-PLAN.md:429-434`; `restart/MASTER-PLAN.md:494-495` | H early SOTA into J final SOTA | H produces Pratt/SIMD/WASM and early reports; J consumes with final parity and SOTA. | There is a downstream close. | H gates are "progress report" rather than executable targets. | Replace progress reports with numeric early thresholds and let J close final thresholds. | REINVENT |
| `restart/MASTER-PLAN.md:463-467`; `restart/MASTER-PLAN.md:494-499` | I LSP/recovery into J close | I produces diagnostics and incremental behavior; J consumes via parity/docs/close report. | The top-layer consumer is clear. | None material. | Folding I into J would overload close. | KEEP |
| `restart/MASTER-PLAN.md:492-499` | J close waves | J consumes all prior artifacts and closes parity, docs, publication, and archive audit. | The close is comprehensive. | J.W1 "formally routed" weakens SOTA closure; Lane 4 discards that clause. | Keep J as close tranche after replacing the SOTA gate. | KEEP |

Lane verdict: **amendment-required**. Counts: KEEP 6 / REINVENT 3 / DISCARD 0.

## §5 Lane 3 - Cohesion

Lane standard: claims must be verifiable from the target or cited artifacts, and deliverables must be consumed by named gates.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/MASTER-PLAN.md:10-40`; `restart/ARCHITECTURE.md:10-30`; `restart/MIGRATION.md:9-22` | Authority ledger | The trio states what governs and what is superseded. | It resolves the stale `ParseStream`, rewrite, Unicode, and declaration-crate conflicts. | None material. | Omitting the ledger would force every tranche to re-resolve old conflicts. | KEEP |
| `restart/MIGRATION.md:42-56`; `restart/MIGRATION.md:111-115` | Aggregate disposition versus mixed rows | The migration gives fate counts for 834 files, then admits some rows mix fates and need refinement before editing. | It avoids pretending every file was listed verbatim. | The aggregate counts cannot be audited from family rows that mix fates without file-count crosswalks. | Add a disposition crosswalk by current directory with file count, fate count, and owner tranche. | REINVENT |
| `restart/ARCHITECTURE.md:336-545`; `restart/ARCHITECTURE.md:1164-1181` | Tree-shape proof | The architecture lists crate trees and lints. | It makes Lock 13 visible. | It does not tabulate child counts or exception rationale per crate/directory. | Add a Lock 13 table: directory, child count, exception if any, enforcing gate. | REINVENT |
| `restart/MASTER-PLAN.md:512-533`; `restart/ARCHITECTURE.md:576-709` | Cargo schema handoff | Metadata owners and consumers are named. | The source/metadata grammar contract is concrete. | Package-name adjustment remains open in Architecture without a receiver gate. | Route naming policy to A.W1/J publication gate. | REINVENT |
| `restart/MASTER-PLAN.md:535-553`; `restart/MIGRATION.md:550-566` | Commit chain disposition | Commit evidence, staging discipline, generated-output bodies, and benchmark metadata are named. | This follows local commit discipline. | None material. | Squashing would erase gate evidence. | KEEP |
| `restart/MASTER-PLAN.md:680-695`; `restart/ARCHITECTURE.md:1195-1213`; `restart/MIGRATION.md:691-701` | Close posture | The close conditions name architecture, migration, plan, and settled authority. | The trio has a crisp close thesis. | Close is not ready until the punch list edits land. | Keep close posture and amend its gates. | KEEP |

Lane verdict: **amendment-required**. Counts: KEEP 3 / REINVENT 3 / DISCARD 0.

## §6 Lane 4 - SOTA Anchoring

Lane standard: every parse-throughput gate must cite competitor, dataset, platform, and target. Non-throughput gates must not claim Lock 8 honor.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/ARCHITECTURE.md:1104-1110`; `restart/README.md:328-333`; `restart/corpora/SOTA.md:50-89`; `restart/corpora/SOTA.md:130-136` | Architecture SOTA target family | Architecture points to the correct competitor evidence. | The numbers match the settled target family: sonic-rs/simd-json JSON, lightning-css CSS, simdjson OD throughput. | It relies on README/SOTA for exact row detail rather than carrying a full gate table. | Keep the architecture section and mirror its numbers into executable tranches. | KEEP |
| `restart/MASTER-PLAN.md:119`; `restart/MASTER-PLAN.md:615` | Master "SOTA" gate labels | The master knows H/J own SOTA. | Ownership exists. | The gate label lacks competitor, dataset, platform, and numbers. | Replace the label with a numeric table or link to a specific SOTA gate table in the same file. | REINVENT |
| `restart/MASTER-PLAN.md:433-434`; `restart/MASTER-PLAN.md:442-443` | H.W4/H.W5 progress reports | H runs early JSON/CSS benches. | Early benches are well placed after Pratt/SIMD/WASM activation. | "progress report" can pass without being near sonic-rs or lightning-css. | Give H early thresholds and J final thresholds. | REINVENT |
| `restart/MASTER-PLAN.md:495`; `restart/MASTER-PLAN.md:505-506` | J.W1 "met or formally routed" | J closes final SOTA and benchmark report. | It acknowledges that hard evidence must land. | "Formally routed" allows final close without the target being met and without naming a receiver. | Discard that clause. Final close either meets the numeric gate or opens a named amendment with blocker and owner. | DISCARD |
| `restart/MASTER-PLAN.md:656`; `restart/MIGRATION.md:688` | Benchmark hardware profiles | The risk register and migration punch list require machine metadata. | CPU, OS, build flags, and input hashes are the right evidence. | They do not name the exact benchmark gate rows that consume the metadata. | Attach metadata schema to H.W4/H.W5/J.W1. | REINVENT |

Lane verdict: **amendment-required**. Counts: KEEP 1 / REINVENT 3 / DISCARD 1.

## §7 Lane 5 - Grammar-Authoritative Discipline

Lane standard: no generic crate may gain grammar-specific match arms, modules, parser types, or default declaration crates. "All grammars" claims require per-X proof, and the YAML test uses exactly two onboarding surfaces.

Grep verification:

| Command | Result | Classification |
|---|---|---|
| `rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>\|CssL4\s*=>\|Bbnf\w*\s*=>\|GoogleSheets\w*\s*=>' restart/MASTER-PLAN.md restart/ARCHITECTURE.md restart/MIGRATION.md` | zero matches | Ratified. No proposed generic match arm appears in the target. |
| `rg -ni 'json\|css_l4\|bbnf\|google_sheets\|sheets\|css_pretty\|bnf\|csv\|ebnf\|math' restart/MASTER-PLAN.md restart/ARCHITECTURE.md restart/MIGRATION.md` | matches found | Mostly ratified: crate names, fixture paths, metadata examples, grep gates, per-grammar generated-budget rows, and current-corpus deletion sites. Faults are listed below. |

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/ARCHITECTURE.md:326-329`; `restart/MIGRATION.md:627-633` | Generic-code match-arm ban | The target forbids parser types, runtime grammar modules, strategy registries, and hardcoded grammar tags in generic code. | Strong grep gates support Lock 14. | None material. | Metadata-derived manifests are enough; Rust match arms are unnecessary. | KEEP |
| `restart/ARCHITECTURE.md:1127-1162` | YAML onboarding test | The target proves a future grammar can be added. | The test names `yaml.bbnf`, metadata, build, LSP, and diff checks. | `fixtures/yaml/*` is listed as an allowed change, violating the two-surface proof. | Move fixtures to a post-onboarding parity test, not the Lock 14 onboarding proof. | DISCARD |
| `restart/MASTER-PLAN.md:110`; `restart/MASTER-PLAN.md:401`; `restart/MIGRATION.md:699-701` | Future grammar gate | Master and migration both require no Rust edits for a new grammar. | This is the right hard gate. | The master row does not explicitly say "two surfaces only"; architecture contradicts it with fixture allowance. | Add the two-surface wording to every future-grammar gate. | REINVENT |
| `restart/MASTER-PLAN.md:369`; `restart/MASTER-PLAN.md:597`; `restart/audit/pass-2-codegen/PASS-2.md:297-308` | "Nine seed grammars" claims | PASS-2 has a per-grammar generated LOC table. | The budget table covers all current grammars by name. | Master does not carry a per-X table for all "nine seed grammars" claims, forcing readers to chase PASS-2. | Inline a short per-grammar gate table in Master §20 or cite a single architecture-owned table. | REINVENT |
| `restart/ARCHITECTURE.md:999-1009`; `restart/ARCHITECTURE.md:703` | Declaration-crate escape valve | The target fences declaration crates with reason and review. | This honors current authority while retaining a rare escape. | The review form and deletion path are not written. | Keep the escape valve but add the A/D gate and form. | KEEP |
| `restart/MIGRATION.md:303-309`; `restart/corpora/CENSUS.md:103-122` | Current grammar leaks | `css_types.rs`, strategy registries, path registries, and runtime shims are deletion/replacement targets. | The migration does not preserve these as patterns. | Mixed-fate rows still need a file-count crosswalk. | The direction is correct; Lane 3 carries the crosswalk amendment. | KEEP |

Lane verdict: **amendment-required**. Counts: KEEP 3 / REINVENT 2 / DISCARD 1.

## §8 Lane 6 - Generated-Code + LOC Budget

Lane standard: generated code needs per-grammar LOC ceilings, per-wave budget gates, regen wall-time budgets, and target-specific attribution where WASM/SIMD add output.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:293-319`; `restart/MASTER-PLAN.md:601-602` | PASS-2 generated LOC authority | PASS-2 gives current and max LOC per grammar plus regen wall budgets. | The numbers are concrete and source-backed. | Master cites them but does not reproduce enough for tranche execution. | PASS-2 remains authority; master must carry executable gates. | KEEP |
| `restart/MASTER-PLAN.md:368`; `restart/MASTER-PLAN.md:376-377`; `restart/MASTER-PLAN.md:594-599` | F generated output budget | F.W4 and F.W5 enforce the +2 percent budget and equality. | The core generated-output tranche has a real gate. | F.W0-F.W2 are only advisory in §20, with no wave ceilings or wall-time budgets. | Add F.W0-F.W5 rows for allowed generated delta and `xtask` time. | REINVENT |
| `restart/MASTER-PLAN.md:598`; `restart/MIGRATION.md:543` | H SIMD/WASM generated output | H may add target-specific generated output. | Attribution by target is the right concept. | No numeric ceiling exists for Rust+WASM generated additions or scanner tables. | Add H.W3-H.W5 budget rows by target and backend. | REINVENT |
| `restart/ARCHITECTURE.md:689`; `restart/ARCHITECTURE.md:1119-1123` | Metadata budget hook | Metadata includes `generated_loc_budget = 1.02` and architecture names a LOC budget API. | The data model can carry budgets. | The schema only shows JSON and does not define per-grammar default/override semantics. | Add a schema rule for inherited default, per-grammar override, and failure diagnostic. | REINVENT |
| `restart/MIGRATION.md:527-548`; `restart/MIGRATION.md:667-675` | Migration LOC trajectory and gates | Migration names steady-state intent and equality/budget commands. | It distinguishes generated from handwritten LOC. | It depends on Master/Architecture for exact ceilings. | Keep as migration summary after master gains per-wave numbers. | KEEP |

Lane verdict: **amendment-required**. Counts: KEEP 2 / REINVENT 3 / DISCARD 0.

## §9 Lane 7 - Friction Forecast

Lane standard: every user-facing friction surface needs a cookbook page or migration page plus verbatim diagnostics that tranche gates verify.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/ARCHITECTURE.md:190-210`; `restart/MASTER-PLAN.md:616`; `restart/MASTER-PLAN.md:635` | `parse` / `parse_in` / `parse_owned` | The three parse constructors are present and doc-owned. | The API follows Lock 9. | No verbatim lifetime/copy diagnostic is gated. | Add cookbook and errors such as `BBNF0012: parse_owned copies input; use parse or parse_in when the document need not outlive the source.` | REINVENT |
| `restart/ARCHITECTURE.md:266-287`; `restart/MASTER-PLAN.md:397-401`; `restart/MASTER-PLAN.md:635` | `pointer!` and `select!` | Path/select macros exist and have compile-time diagnostics. | The ergonomics are familiar. | The target does not commit error strings for unknown segment, terminal ambiguity, or grammar metadata absence. | Gate verbatim `BBNF_PATH001`, `BBNF_PATH002`, and `BBNF_PATH003` diagnostics plus path cookbook examples. | REINVENT |
| `restart/ARCHITECTURE.md:995`; `restart/MASTER-PLAN.md:633` | Layout lowering errors | Layout/error directives produce facts. | The compiler can diagnose conflicts. | No error message explains why a rule has no resolvable layout. | Add `BBNF_LAYOUT001: rule {rule} cannot lower to {layout}; branch {branch} yields {shape} while branch {other} yields {shape}. Add @layout(...) or split the rule.` | REINVENT |
| `restart/MASTER-PLAN.md:429-434`; `restart/ARCHITECTURE.md:887`; `restart/ARCHITECTURE.md:909` | Pratt/SIMD auto-detection misfires | Auto detection is in the plan. | Authors need not learn directives. | No warning explains a rejected Pratt or SIMD candidate. | Add `BBNF_OPT010` and `BBNF_OPT020` notes with reason, candidate rule, and scalar fallback. | REINVENT |
| `restart/ARCHITECTURE.md:1127-1162`; `restart/MASTER-PLAN.md:401` | Future grammar onboarding | YAML onboarding exists. | This is the right Lock 14 proof. | It lacks a human-facing failure message when Rust edits are detected. | Add `BBNF_META014: grammar yaml changed Rust source outside generated output; onboarding allows only grammars/yaml.bbnf and metadata.` | REINVENT |
| `restart/MIGRATION.md:62-82`; `restart/MASTER-PLAN.md:626-638` | Crate split migration | Migration maps old crates to new crates and docs own migration notes. | Current users and contributors get a path. | No migration page sections or command snippets are gated. | Add a J.W2 migration page gate covering old-to-new crate imports, path macros, CLI command names, and generated output location. | REINVENT |

Lane verdict: **amendment-required**. Counts: KEEP 0 / REINVENT 6 / DISCARD 0.

## §10 Lane 8 - Carry & Deferral Audit

Lane standard: each deferral or carry must name receiver, blocker, and receiving gate. "Future" without those three fields is a fault.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/MASTER-PLAN.md:38-40`; `restart/ARCHITECTURE.md:25`; `restart/MASTER-PLAN.md:104` | Unicode class algebra below BBNF | Unicode set algebra is routed to `parse-that/regex` and D/H tests. | Receiver and gate are mostly named. | The blocker is implicit: grammar-level surface was rejected. | Add one phrase: blocker is Lock 14 and grammar-surface minimization. | KEEP |
| `restart/MASTER-PLAN.md:146`; `restart/MASTER-PLAN.md:694-695` | Full wave docs after hardening | The plan keeps tranche stubs and routes full drafts after hardening. | This matches the phase contract. | The receiver is implicit. | It is acceptable because the hardening prompt itself names the receiver step. | KEEP |
| `restart/ARCHITECTURE.md:639-641` | Package names adjusted during implementation | Cargo naming policy is left open. | Publication reality may require package-name details. | Receiver, blocker, and receiving gate are absent. | Route to A.W1 for workspace naming and J.W3 for publication dry-run. | REINVENT |
| `restart/MIGRATION.md:565-566`; `restart/MIGRATION.md:557-559` | Branch operation future work | The synthesis does not create branches. | Correctly avoids git operations during synthesis. | The exact branch action has no receiver gate in the target. | Route to A.W0 with `git rev-parse pre-restart-2026-05-04` and branch-exists evidence. | REINVENT |
| `restart/MIGRATION.md:682-689` | Unresolved migration punch list | Six implementation details are listed with owner and constraint. | The list is concrete enough to route. | It lacks blocker and receiving gate columns. | Add columns: receiver wave, blocker, receiving close gate. | REINVENT |
| `restart/MASTER-PLAN.md:495`; `restart/MASTER-PLAN.md:549` | SOTA "formally routed" | SOTA can be routed elsewhere at close. | It tries to avoid fake success. | It names no receiver, blocker, or receiving gate, and weakens Lock 8. | Discard this clause; final SOTA needs numeric pass or a named amendment before close. | DISCARD |
| `restart/MIGRATION.md:686-687`; `restart/MASTER-PLAN.md:432` | `path-ts` timing and WASM ABI | Deferred package timing and WASM ABI details have owners. | TS production is correctly not forced too early. | Blockers and receiving gates need precision. | Add blockers: parity matrix for `path-ts`; wasm32 Rust binding ABI test for WASM. | REINVENT |

Lane verdict: **amendment-required**. Counts: KEEP 2 / REINVENT 4 / DISCARD 1.

## §11 Lane 9 - Greenfield Discipline

Lane standard: the target must solve root causes, retire contested legacy architecture, avoid quick patches, keep Rust-idiomatic boundaries, and perform architectural transpositions where they simplify or speed execution.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/README.md:5`; `restart/MASTER-PLAN.md:16-40`; `restart/ARCHITECTURE.md:1197-1210` | Greenfield thesis | The target chooses grammar-derived infrastructure, tape/direct union, two IRs, and bounded BBNF extensions. | It is a root-cause rebuild, not a patch list. | The proof gates need amendments. | A smaller patch plan would preserve the current fault lines. | KEEP |
| `restart/MIGRATION.md:158-186`; `restart/MIGRATION.md:299-309` | Legacy code contested | Current backend, runtime, host, registry, and grammar-shim code is mined or replaced. | The migration does not carry legacy code uncontested. | Mixed-fate rows dilute the proof. | Add crosswalks rather than changing the direction. | REINVENT |
| `restart/ARCHITECTURE.md:999-1009`; `restart/MASTER-PLAN.md:651` | Host functions without default declaration crates | Host work decomposes through primitives, metadata, and `@host fn`. | This is the right anti-overfit transposition. | Rare escape valve needs the review form. | Keep with a fenced form and deletion path. | KEEP |
| `restart/MASTER-PLAN.md:127-146`; `restart/MASTER-PLAN.md:659-675` | Multi-tranche sequencing | A-J tranches are ordered by dependencies. | The shape is coherent and avoids one omnibus wave. | B/C and C/E/H facts have consumer timing gaps. | Amend those gaps rather than redraft the tranche set. | REINVENT |
| `restart/ARCHITECTURE.md:639-641`; `restart/ARCHITECTURE.md:630-636` | Package-name ambiguity | Implementation can adjust package names. | Cargo publication constraints are real. | Open-ended naming is an overfit aperture for old `bbnf-` internal prefixes. | Bind the policy to A.W1/J.W3 and require an architecture amendment for ownership changes. | REINVENT |
| `restart/MASTER-PLAN.md:580-586`; `restart/MIGRATION.md:439-454` | Archive discipline | Archive-only code leaves production. | This is root-cause removal, not compatibility shimming. | The exact path belongs to A, which is acceptable if gated. | Keep with A.W0 evidence. | KEEP |
| `restart/MASTER-PLAN.md:495`; `restart/MASTER-PLAN.md:656` | Final perf escape | The plan allows SOTA to be "formally routed" at final close. | It acknowledges measurement risk. | It permits a non-result to pass the lock gate. | Discard as greenfield discipline failure; missed SOTA requires amendment before close. | DISCARD |

Lane verdict: **amendment-required**. Counts: KEEP 3 / REINVENT 3 / DISCARD 1.

## §12 Punch list

Each item is a surgical edit required before target advancement. Source verdicts are REINVENT or DISCARD only.

| # | Target site | Verbatim edit or surgery | Source verdict | Owner | Scope | Lane(s) |
|---:|---|---|---|---|---|---|
| 1 | `restart/MASTER-PLAN.md:609`; `restart/ARCHITECTURE.md:715-747`; `restart/ARCHITECTURE.md:919-926` | Rename the named lowering surface to `layout lowering` / `LayoutFacts` / `passes::layout`. Keep HM/CSP type checking as a subroutine inside layout/type checking, but do not let `TypeFacts` be the public pass name for Lock 2 closure. | REINVENT | amendment agent | target trio | 1 |
| 2 | `restart/MASTER-PLAN.md:610`; `restart/MIGRATION.md:656-665` | Add explicit gates: `cargo test -p runtime cursor_skip_empty_path_elision`, `cargo test -p runtime cursor_skip_byte_skip_consultation`, and `rg "__EAGER_EMPTY_PATH|CursorDecision::Skip" crates/runtime/src crates/codegen/src`. | REINVENT | amendment agent | master + migration | 1 |
| 3 | `restart/MASTER-PLAN.md:232-236`; `restart/MASTER-PLAN.md:267-272` | Repair B/C sequencing: either move ShapeFacts before B.W3, split B.W3 into a direct-view shell and C-owned materialization, or change C.W2's consumer away from the B direct builder. | REINVENT | amendment agent | master | 2 |
| 4 | `restart/MASTER-PLAN.md:270-272`; `restart/MASTER-PLAN.md:333-337`; `restart/MASTER-PLAN.md:429-434` | Give C.W3/C.W5 same-wave BIR snapshot consumers or move recognizer/extraction proof into E/H where real BIR and Pratt/SIMD consumers exist. | REINVENT | amendment agent | master | 2 |
| 5 | `restart/MIGRATION.md:42-56`; `restart/MIGRATION.md:111-115` | Add a migration crosswalk table: current directory/family, file count, KEEP-OUTRIGHT, KEEP-MODIFY, ABROGATE-MOVE, ABROGATE-REPLACE, ABROGATE-DELETE, GENERATED-REPLACE, owner tranche. Mixed-fate rows cannot remain uncounted. | REINVENT | amendment agent | migration | 3, 9 |
| 6 | `restart/ARCHITECTURE.md:336-545`; `restart/ARCHITECTURE.md:1173-1181` | Add a Lock 13 verification table per crate/directory: child count, file-size gate, exception rationale, enforcing command. Include egraph-derive, generated directories, and SIMD intrinsic files. | REINVENT | amendment agent | architecture | 1, 3 |
| 7 | `restart/MASTER-PLAN.md:119`; `restart/MASTER-PLAN.md:433-434`; `restart/MASTER-PLAN.md:495` | Inline a SOTA gate table in Master: twitter <= 380 us on M1 Pro versus sonic-rs 436 us / simd-json 424 us; canada <= 2.8 ms on M1 Pro versus sonic-rs 3.144 ms; citm <= 750 us on M1 Pro versus sonic-rs 854 us / simd-json 831 us; bootstrap <= 3.0 ms on M1 Pro versus lightning-css 4.16 ms; animate <= 1.6 ms on M1 Pro versus lightning-css 1.97 ms; simdjson OD >= 5 GB/s M-series and >= 7 GB/s x86. | REINVENT | amendment agent | master | 1, 4 |
| 8 | `restart/MASTER-PLAN.md:495`; `restart/MASTER-PLAN.md:549` | Delete "or formally routed" from final SOTA close. Replace with: "If a target is missed, J.W1 fails and opens a named architecture amendment before close." | DISCARD | amendment agent | master | 4, 8, 9 |
| 9 | `restart/ARCHITECTURE.md:1132-1138`; `restart/ARCHITECTURE.md:1151-1162` | Remove `fixtures/yaml/*` from allowed changes in the Lock 14 onboarding test. Add a separate post-onboarding fixture/parity test if desired, clearly outside the two-surface proof. | DISCARD | amendment agent | architecture | 1, 5 |
| 10 | `restart/MASTER-PLAN.md:369`; `restart/MASTER-PLAN.md:597`; `restart/audit/pass-2-codegen/PASS-2.md:297-308` | Carry a per-grammar generated LOC table into Master or Architecture so every "nine seed grammars" claim is auditable without chasing PASS-2. | REINVENT | amendment agent | master or architecture | 5, 6 |
| 11 | `restart/MASTER-PLAN.md:594-599`; `restart/ARCHITECTURE.md:689`; `restart/audit/pass-2-codegen/PASS-2.md:312-319` | Add per-wave generated LOC and xtask wall-time budgets for F.W0-F.W5 and H.W3-H.W5, including WASM/SIMD target-specific output attribution. | REINVENT | amendment agent | master + architecture | 6 |
| 12 | `restart/MASTER-PLAN.md:626-638`; `restart/ARCHITECTURE.md:266-287`; `restart/ARCHITECTURE.md:978-996` | Add a friction ledger with cookbook page, target users, confusion point, and verbatim diagnostics for `pointer!`, `select!`, parse lifetimes, layout lowering, Pratt/SIMD decisions, crate split migration, and YAML onboarding. | REINVENT | amendment agent | master + architecture | 7 |
| 13 | `restart/MIGRATION.md:682-689`; `restart/MIGRATION.md:565-566`; `restart/ARCHITECTURE.md:639-641` | Add receiver wave, blocker, and receiving gate columns for every unresolved/future item. Route package names to A.W1/J.W3 and branch/tag operation to A.W0. | REINVENT | amendment agent | migration + architecture | 8 |
| 14 | `restart/ARCHITECTURE.md:703`; `restart/ARCHITECTURE.md:999-1009`; `restart/MIGRATION.md:685` | Write the rare declaration-crate review form: reason, owner, why metadata and `@host fn` fail, deletion path, reviewer, and A/D gate. | REINVENT | amendment agent | architecture + migration | 5, 9 |
| 15 | `restart/MASTER-PLAN.md:656`; `restart/MIGRATION.md:688` | Bind benchmark metadata to H.W4/H.W5/J.W1 gates: CPU model, OS, compiler flags, input hash, competitor version, bbnf commit, warmup/sample policy. | REINVENT | amendment agent | master + migration | 4 |
| 16 | `restart/MASTER-PLAN.md:85-86` | Correct the archive citation label from "per Lock 10" to the archive lock row. The cited line range covers Lock 11/12 material, not Lock 10. | REINVENT | amendment agent | master | 1 |

## §13 Final readiness

> **Decision: AMENDMENT-REQUIRED**
>
> The MASTER-PLAN trio is the right plan family: tape remains tape, direct-to-struct is unioned into the same substrate, Backend IR is the lowerer contract, rewrite-mode is out, Unicode algebra stays under regex, and declaration crates are not default. The faults are gate-shape faults rather than thesis faults. The target must be amended before tranche drafting so Lock 2 terminology, Lock 3 parser gates, Lock 8 SOTA gates, Lock 14 two-surface proof, generated LOC budgets, friction diagnostics, and deferral routing are executable.
>
> Hereupon the next step is amendment agent dispatch against the 16-item punch list, followed by re-running this hardening gate on the amended trio.
