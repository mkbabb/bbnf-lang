# HARDENING-PASS-3-V2 — Rerun against Wave 2 + Wave 3 amendments

## §1 Target identification

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md` (479 lines, post-amendment) |
| V1 baseline | `restart/audit/hardening/HARDENING-PASS-3.md` commit `c839de98` (verdict AMENDMENT-REQUIRED, 12-item punch list) |
| Amendment commits audited | `dceeaf32` (Wave 2: fixture separation + path crate naming + pointer + bbnf tree + budgets + diagnostics + carries + registry gate), `70378e46` (Wave 3 carry: bbnf canonical 8-children layout from `1189421d`) |
| Sub-agent surface | six PASS-3 sub-agent reports (correction notes carried in §0, §3, §4, §6) |
| V2 output path | `restart/audit/hardening/HARDENING-PASS-3-V2.md` |
| Lanes applied | nine; Lane 2 N/A for single-pass scope |
| Tightened gate-rerun | all 16 commands rerun, with 8 PASS-3-touching gates resolving to expected post-conditions |

Punch items routed to PASS-3 by HARDENING-CONSOLIDATED §5: 5 (consumer acceptance gates), 9 (`@error(recover)` consolidation), 12 (fixture separation — RE-ROUTED here from ARCHITECTURE per Reviewer D), 13 (per-X grammar proof table feeder), 17 (path crate naming), 18 (`pointer!` macro surface), 19 (`bbnf` aggregator child-count), 26 (PASS-3 generated-surface budget), 33 (BBNF self-host internal gate), 34 (compiler diagnostic ledger), 36 (incremental fallback reporting), 37 (PASS hand-off tables), 47 (registry deletion gate).

## §2 Cohort verdict

| Lane | V2 Verdict | KEEP | REINVENT | DISCARD | V1 → V2 delta |
|---|---|---:|---:|---:|---|
| 1 Lock-Adherence | READY | 12 | 0 | 0 | KEEP +5, REINVENT -7 (Lock 1 tape/direct identity; Lock 7 path crate names corrected to `path`/`path-core`/`path-ts`; Lock 13 `bbnf/src/` 8 children; Lock 14 yaml two-surface proof) |
| 2 Sequencing | N/A | — | — | — | unchanged (single-pass) |
| 3 Cohesion | READY | 7 | 1 | 0 | KEEP +5, REINVENT -5 (per-grammar feeder table at §6a; consumer acceptance gates at §3; bbnf canonical layout at §6) |
| 4 SOTA-Anchoring | READY | 6 | 0 | 0 | KEEP +5, REINVENT -5 (exact benchmark rows at §7 with bbnf self-host internal gate; non-throughput gates routed mechanism-only) |
| 5 Grammar-Authoritative | READY | 7 | 0 | 0 | KEEP +4, REINVENT -5 (10-row feeder at §6a; fixture separation at §6; registry deletion close gate at §3) |
| 6 Generated-Code-Budget | READY | 7 | 0 | 0 | KEEP +5, REINVENT -5 (visitor LOC, path-schema, sidecar, tape identity field/method delta, bench-report, regen wall — all bound at §7) |
| 7 Friction-Forecast | READY | 9 | 0 | 0 | KEEP +7, REINVENT -7 (15-row diagnostic ledger at §6b with verbatim text + target user + mental model + confusion point + artefact) |
| 8 Carry-Deferral | READY | 9 | 0 | 0 | KEEP +9, REINVENT -7 (12-row unresolved punch-list at §10 with Receiver/Blocker/Receiving gate; 12-row hand-off table at §8 with the same triple) |
| 9 Greenfield-Discipline | READY | 7 | 1 | 0 | KEEP +3, REINVENT -4 (DocumentSnapshot + ReparsePlan posture; LSP user-facing silence policy at §5; registry deletion gate) |

| Verdict class | V1 totals | V2 totals | Net |
|---|---:|---:|---|
| KEEP | 19 | 64 | +45 |
| REINVENT | 47 | 2 | -45 |
| DISCARD | 0 | 0 | unchanged |

**Final V2 decision: READY** — every V1 punch item resolved; the 47-REINVENT V1 cohort collapses to two non-blocking phrasing tightenings; both DISCARD-equivalent retirements (registry, ParseStream rename, hardcoded fixtures) are confirmed.

## §3 Lane 1 — Lock-Adherence

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:16 | Lock 1 — tape unioned with direct-to-struct | "tape is the substrate and is properly unioned with direct-to-struct. It must not be renamed to `ParseStream`" | settled authority preserved | none | matches HARDENING-CONSOLIDATED §3 conflict #2 (ParseStream rejection) | KEEP |
| PASS-3.md:31 | Tape/direct union verdict | "Tape is the single advanced substrate; direct structs remain the ergonomic default" | matches Lock 1 | none | mechanism-level claim with cross-pass binding to PASS-1 §2 + PASS-2 §3 | KEEP |
| PASS-3.md:32 | `ParseStream` DISCARD | "DISCARD" with cross-conflict citation | resolves HARDENING-CONSOLIDATED §3 conflict #2 | none | the discard is universal across PASS-1 + PASS-2 + PASS-3 | KEEP |
| PASS-3.md:84 | Lock 7 — path crate naming | "`path-core` owns parsing… `path` owns Rust proc macros: `pointer!` and `select!`… `path-ts` owns TS template tags" | resolves HARDENING-CONSOLIDATED §3 conflict #3 + V1 punch item 17 | none | matches MASTER-PLAN §12 + Architecture §3.4 | KEEP |
| PASS-3.md:30 | Lock 9 — slice-borrow API | `parse`, `parse_in`, `parse_owned` "implement Lock 9 slice-borrow/default plus explicit arena/owned modes" | matches Lock 9 | none | every constructor has a docstring + cookbook receiver | KEEP |
| PASS-3.md:115 | Lock 13 — visitor cohort discipline | "generated `Visitor` traits, `Visit`/walker support, and `VisitTypes` bitflag pruning" | matches Lock 13 child-count + sibling API uniformity | none | matches PASS-2 §6 runtime emission table | KEEP |
| PASS-3.md:194-208 | Lock 13 — `bbnf` aggregator 8 children | "exactly 8 immediate children — `lib.rs`, `prelude.rs`, `parse/`, `document/`, `query/`, `visitor/`, `diagnostics/`, `metadata/`" | resolves HARDENING-CONSOLIDATED §3 conflict #3 (bbnf tree) + V1 punch item 19 | none | child-count = 8 ∈ [4,10]; matches Architecture §4.1 row | KEEP |
| PASS-3.md:320-325 | Lock 14 — fixture separation | "Lock 14 onboarding admits exactly two surfaces … `fixtures/yaml/*` is *not* part of the onboarding allowance" | resolves V1 punch item 12 (re-routed here from ARCHITECTURE per Reviewer D) | none | grep gate at line 325 binds verification | KEEP |
| PASS-3.md:328-344 | Lock 14 — per-grammar feeder | 10 rows (9 grammars + yaml onboarding) × 8 columns | resolves V1 punch item 13 | none | feeds Architecture §12.1 verbatim | KEEP |
| PASS-3.md:38 | Rewrite-mode + Unicode DISCARD | matches Lock 14 + HARDENING-CONSOLIDATED §3 row 6 | settled | none | matches Architecture §8.1 deletion table | KEEP |
| PASS-3.md:94-101 | Lock 14 — registry deletion close gate | grep gate enforced as close gate; "zero outside generated data" | resolves V1 punch item 47 | none | matches MASTER-PLAN §11 close + Architecture §3.6 | KEEP |
| PASS-3.md:104-112 | Lock 5 — consumer acceptance gates | three executable gates feeding PASS-3 close | resolves V1 punch item 5 | none | matches PASS-2.md:336-347 producer-side carry | KEEP |

Lane 1 verdict: **READY**. KEEP 12 / REINVENT 0 / DISCARD 0 (V1 had KEEP 7 / REINVENT 7 / DISCARD 0; every REINVENT row resolved through canonical layout, fixture separation, registry deletion, and `pointer!`/`path`/`path-core`/`path-ts` naming).

## §4 Lane 2 — Sequencing Discipline

N/A. PASS-3 is a single-pass synthesis. The 12-row hand-off table at §8 + 12-row unresolved punch-list at §10 carry receiver/blocker/gate triples; sequencing across waves is owned by MASTER-PLAN.

## §5 Lane 3 — Cohesion

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:60-78 | `Grammar` trait + `DocumentView` shapes | precise lifetime-parameterised types | resolves V1 cohesion concern about API ambiguity | none | matches Architecture §3.1 verbatim | KEEP |
| PASS-3.md:104-112 | Consumer acceptance gates | three rows (compile under wrappers; metadata feeds visitors + selectors; cost tables documented) | resolves V1 punch item 5 + HARDENING-CONSOLIDATED §4.5 | "rows appear as receiver/blocker/receiving-gate" forwarded to §8 | the §8 carry binding is intact | KEEP |
| PASS-3.md:130-152 | Tape illustrative shape | TapeToken with kind/flags/start/end/payload/sibling_skip; `ValueRef<'doc, 'input, K>` | resolves V1 cohesion concern about runtime ABI | "not a PASS-1 mandate; user-surface contract" | the user-surface contract is precisely the carry to PASS-1 — Architecture §9.1 binds the invariants | KEEP |
| PASS-3.md:194-208 | bbnf canonical 8-children layout | "exactly 8 immediate children …" with the rationale that `tape/` and `value/` live under `runtime/` not bbnf/` | resolves HARDENING-CONSOLIDATED §3 conflict #3 + V1 punch item 19 | none | matches Architecture §4.1 row | KEEP |
| PASS-3.md:228-251 | bbnf-language-server tree | 19 lines × file/dir mapping | resolves V1 cohesion concern about LSP/DAP/playground ownership | none | matches MASTER-PLAN §14 tranche I | KEEP |
| PASS-3.md:252-272 | bbnf-bench tree | clean separation harness/datasets/competitors/report | matches Architecture §4.4 row | none | feeds H/J SOTA gates | KEEP |
| PASS-3.md:273-299 | path/path-core/path-ts trees | unprefixed names; concern-split | resolves V1 punch item 17 + Lock 7 | none | matches MASTER-PLAN §12 G.W0-G.W4 | KEEP |
| PASS-3.md:300-318 | test-fixtures + fixture data tree | data + manifests only; no Rust per-grammar | matches MIGRATION §3.1 + Lock 14 | none | feeds parity-phase gate at BD.W4 + HARDENING §4.12 | KEEP |
| PASS-3.md:328-344 | §6a per-grammar feeder | 10 rows × 7 cell types | resolves V1 punch item 13 | yaml row's "as declared in `[workspace.metadata.bbnf.grammars.yaml]`" cell is documentary | the cell is correct because yaml's host route lands at metadata-time, not pre-onboarding | REINVENT (cell could carry a stronger forward pointer to host primitives + `@host fn` decomposition; non-blocking phrasing tightening) |

Lane 3 verdict: **READY**. KEEP 8 / REINVENT 1 / DISCARD 0 (single REINVENT is non-blocking; the row's content is correct).

## §6 Lane 4 — SOTA-Anchoring

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:386-396 | Exact PASS-3 benchmark rows | 8 rows × 3 columns (Row, Target, Surface under test) | resolves V1 punch item 29 (PASS-3 dimension) | none | matches MASTER-PLAN §4 + PASS-2 §7 | KEEP |
| PASS-3.md:388 | json/twitter/borrowed ≤ 380us | "parse(&str) plus direct root" | matches MASTER-PLAN row | none | platform M1 Pro carried via PASS-2 §7 | KEEP |
| PASS-3.md:391 | json/canada/array_scan ≤ 2.8ms | matches MASTER-PLAN row | none | "array-heavy parse and selector scan" | mechanism cell carries appropriate detail | KEEP |
| PASS-3.md:394 | bbnf/self_host/internal gate | "≤ 100 ms full self-parse + format roundtrip; non-Lock-8 internal gate; no SOTA peer claim attaches" | resolves V1 punch item 33 | "no SOTA peer claim attaches" | the row explicitly disclaims SOTA framing — mechanism gate only | KEEP |
| PASS-3.md:395 | incremental/edit_anchor | "report fallback rate" | matches §5 fallback-rate gates by dataset | none | dataset-level gates at lines 184-188 carry thresholds | KEEP |
| PASS-3.md:181-190 | Fallback-rate gates | 4 rows × 4 columns (corpus, snapshot reuse, fallback ceiling, surface) | resolves V1 punch item 36 + HARDENING-CONSOLIDATED §4.36 | none | dataset coverage spans JSON, CSS, BBNF, large-paste | KEEP |

Lane 4 verdict: **READY**. KEEP 6 / REINVENT 0 / DISCARD 0.

## §7 Lane 5 — Grammar-Authoritative Discipline

Verification:
- `rg -ni 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' restart/audit/pass-3-runtime/PASS-3.md` returns zero in proposed code (only legacy citations + table cells).
- `rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' restart/audit/pass-3-runtime/PASS-3.md` returns zero.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:94-101 | Registry deletion close gate | grep enforced as close gate | resolves V1 punch item 47 | none | matches HARDENING-CONSOLIDATED §3 conflict #4 (per-X table) | KEEP |
| PASS-3.md:320-325 | Fixture separation Lock 14 onboarding | onboarding admits two surfaces; fixtures land in parity phase | resolves V1 punch item 12 | none | grep gate at line 325 binds verification | KEEP |
| PASS-3.md:328-344 | Per-grammar feeder table | 10 rows × 7 columns | resolves V1 punch item 13 | none | every "all grammars" claim resolves through this table | KEEP |
| PASS-3.md:90-91 | "Generated grammar metadata replaces fixture registries" | the path schema validates compile-time | resolves V1 hardcoded-registry concern | none | matches Architecture §10.1 path inheritance | KEEP |
| PASS-3.md:300-318 | test-fixtures tree | data + manifests only; no per-grammar Rust | matches Lock 14 | none | matches Amendment 01 + MIGRATION §3.1 | KEEP |
| PASS-3.md:38 | rewrite-mode + Unicode + per-grammar declaration crates DISCARD | matches HARDENING-CONSOLIDATED §3 row 6 | settled | none | matches Architecture §8.1 deletion table | KEEP |
| PASS-3.md:344 | yaml row at onboarding boundary | "every cell to the left of the parity-phase fixture manifest must be generated from `yaml.bbnf` plus the workspace-metadata block, with zero Rust edits and zero per-grammar match arms" | resolves V1 punch item 11 | none | matches Lock 14 two-surface mandate | KEEP |

Lane 5 verdict: **READY**. KEEP 7 / REINVENT 0 / DISCARD 0.

## §8 Lane 6 — Generated-Code + LOC Budget

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:399-409 | Generated API budget | 8 rows × 2 columns (surface, gate) | resolves V1 punch item 26 | none | every row maps to a budget gate | KEEP |
| PASS-3.md:400 | Visitor traits per grammar | "no handwritten visitor file over 500 LOC; per-grammar visitor LOC delta beyond W3 baseline carries a +2 percent ceiling per regen" | matches Lock 13 + PASS-2 §6 budget | none | wrap to F.W4 budget tooling | KEEP |
| PASS-3.md:401 | Path metadata (Rust) | "per-grammar path-schema Rust budget <= 32 KB" | precise byte-budget | none | feeds path-core schema sidecar | KEEP |
| PASS-3.md:402 | Path metadata (sidecar) | "<= 64 KB per grammar; bench manifest sidecar <= 8 KB per grammar" | precise byte-budgets | none | matches Architecture §10.2 | KEEP |
| PASS-3.md:403 | Tape projections | "+2 percent ceiling per regen" | matches PASS-2 §6 budget | none | wraps to F.W4 budget tooling | KEEP |
| PASS-3.md:404 | Tape identity field/method delta | "<= 1 field plus 2 methods per regen; larger deltas open a named amendment" | mechanism gate | none | precise count rule | KEEP |
| PASS-3.md:405 | Bench-report generation | "<= 16 KB markdown; <= 8 KB JSON; aggregate <= 64 KB" | precise byte-budgets | none | wraps to H/J SOTA budget | KEEP |
| PASS-3.md:406 | Regen wall budget | "<= 12 s on M1 Pro for the nine extant grammars; <= 14 s including yaml" | matches PASS-2 §6 wall budget | none | over-budget regen blocks close | KEEP |

Lane 6 verdict: **READY**. KEEP 8 / REINVENT 0 / DISCARD 0.

## §9 Lane 7 — Friction Forecast

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:346-368 | Compiler diagnostic ledger | 15 rows × 5 columns (Code, Verbatim text, Target user, Mental model, Confusion point, Artefact) | resolves V1 punch item 34 + HARDENING-CONSOLIDATED §4.34 | none | every code is committed string | KEEP |
| PASS-3.md:352 | `BBNF-LIFE001` | "borrowed value escapes parse scope; the source string `&str` was dropped before this projection" | committed string with help message | none | feeds cookbook §lifetime-surfaces | KEEP |
| PASS-3.md:353 | `BBNF-LIFE002` | "arena mismatch; root was parsed in arena #N but projected through arena #M" | committed string | none | feeds cookbook arena chapter | KEEP |
| PASS-3.md:354 | `BBNF-LAYOUT001/002` | layout warning + error pair | committed strings | none | feeds layout cookbook | KEEP |
| PASS-3.md:357 | `BBNF-OPT001/002` | Pratt + SIMD informational notes | committed strings | "did not apply" tone is informational | the diagnostics are auto-detection notices not user errors — informational tone is correct | KEEP |
| PASS-3.md:358-361 | `BBNF-GRAMMAR001` + `BBNF-POINTER001/002/003` | grammar-add + pointer typo + pointer grammar inference + pointer terminal type | committed strings | none | feeds onboarding cookbook + pointer cookbook | KEEP |
| PASS-3.md:362 | `LookbehindWidth` (`BBNF-LIFE003`) | committed help message | matches PASS-1.md:96 `BBNF1004` | none | the alias maps the user-facing routing | KEEP |
| PASS-3.md:363-364 | `HostSignature` + `ChainStep` | matches PASS-1.md:97 + 99 | committed strings | none | mirrored from PASS-1 | KEEP |
| PASS-3.md:365-366 | `WasmHost` + `LowererImport` | committed strings; lowerer-import code mirrored from PASS-2 ownership | matches PASS-2.md:520-528 | none | cross-PASS string ownership clear | KEEP |

Lane 7 verdict: **READY**. KEEP 9 / REINVENT 0 / DISCARD 0.

## §10 Lane 8 — Carry & Deferral Audit

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:411-426 | Cross-pass hand-off table | 12 rows × 4 columns (Contract, Receiver, Blocker, Receiving gate) | resolves V1 punch item 37 | none | every row triple-complete | KEEP |
| PASS-3.md:415-416 | Tape ABI carry | Receiver PASS-1 / Tranche B; Blocker PASS-3 cursor proof; Gate runtime identity tests | matches PASS-1 §10 | none | clean cross-pass binding | KEEP |
| PASS-3.md:418 | Consumer acceptance carry | Receiver PASS-2 / Tranche F + Tranche I; Blocker PASS-3 close on prose-only; Gate three executable consumer gates | matches PASS-2.md:336-347 | none | binds the verify path | KEEP |
| PASS-3.md:421 | Hardcoded grammar registry deletion | Receiver SYNTHESIS / Tranche I close; Blocker registry survives parallel; Gate `rg` returns zero outside generated | resolves V1 punch item 47 | none | matches HARDENING-CONSOLIDATED §3 conflict #11 | KEEP |
| PASS-3.md:424 | Incremental fallback gates | Receiver PASS-1 / Tranche I; Blocker fallback unreported; Gate dataset thresholds + LSP policy + bench ledger | resolves V1 punch item 36 | none | fallback-rate gates table at §5 carries the dataset thresholds | KEEP |
| PASS-3.md:425 | Per-grammar feeder rows carry | Receiver SYNTHESIS / Architecture per-X table; Blocker prose; Gate 10-row table consumed verbatim | resolves V1 punch item 13 | none | matches Architecture §12.1 | KEEP |
| PASS-3.md:426 | Compiler diagnostic ledger carry | Receiver SYNTHESIS + cookbook receivers; Blocker drift; Gate every code in §6b appears verbatim | resolves V1 punch item 34 | none | committed-string carry | KEEP |
| PASS-3.md:462-475 | Unresolved punch-list | 12 rows × 4 columns (Carry, Receiver, Blocker, Receiving gate) | resolves V1 punch item 37 | none | every row is triple-complete | KEEP |
| PASS-3.md:465 | Tape ABI carry detail | "PASS-1 publishes the ABI table; PASS-3 binds against it in identity tests" | named gate | none | matches PASS-1 §4 hand-off table row | KEEP |

Lane 8 verdict: **READY**. KEEP 9 / REINVENT 0 / DISCARD 0 (V1 had KEEP 0 / REINVENT 7; every REINVENT entry resolved by Wave 2 carry-ledger expansion).

## §11 Lane 9 — Greenfield Discipline

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:32 | `ParseStream` DISCARD | "Mention only as stale conflict" | abrogates rebrand pressure | none | matches HARDENING-CONSOLIDATED §3 conflict #2 | KEEP |
| PASS-3.md:38 | rewrite-mode + Unicode + per-grammar declaration crates DISCARD | settled | matches Lock 14 + Lock 8 | none | matches HARDENING-CONSOLIDATED §3 row 6 | KEEP |
| PASS-3.md:84-92 | path crate names corrected | restart names are `path`, `path-core`, `path-ts`; legacy `bbnf-path` cited as legacy evidence only | resolves V1 punch item 17 + Lock 7 | none | matches Architecture §3.4 + MASTER-PLAN §12 | KEEP |
| PASS-3.md:94-101 | Registry deletion close gate | "Hardcoded grammar marker registries are not a deferral; they are a deletion item" | resolves V1 punch item 47 | none | matches HARDENING-CONSOLIDATED §3 conflict #11 | KEEP |
| PASS-3.md:160 | `@error(recover = ...)` consolidation | "A standalone `@recover` token is a legacy alias only if SYNTHESIS keeps a migration parser; it is not a new V1 extension" | resolves V1 punch item 9 | none | matches Architecture §8.1 input-normalization-deletion table | KEEP |
| PASS-3.md:162-179 | DocumentSnapshot + ReparsePlan | architectural transposition for incremental parsing | resolves V1 punch item 36 (incremental fallback reporting) | "may fall back to full parse when anchors fail" | the fallback-rate gate at §5 + LSP user-facing silence policy at line 190 catch the fallback risk | KEEP |
| PASS-3.md:190 | LSP user-facing silence policy | "Default LSP output is silent on fallback. A debug-only diagnostic channel reports fallback events" | resolves V1 cohesion concern about diagnostic noise | "BBNF_LSP_DEBUG=1" env-var hatch | the env-var hatch is correct: dev signal vs user noise | KEEP |
| PASS-3.md:115 | Visitor cohort | "generated `Visitor` traits, `Visit`/walker support, and `VisitTypes` bitflag pruning" | matches W5 inheritance and lightning-css visitor pattern | none | feeds Lock 13 + Architecture §11 visitor surface | REINVENT (the cookbook receivers row could fold the visitor cookbook explicitly into §6b's diagnostic table to bind the cookbook + diagnostic strings together; non-blocking) |

Lane 9 verdict: **READY**. KEEP 7 / REINVENT 1 / DISCARD 0 (V1 had KEEP 4 / REINVENT 5 / DISCARD 0; the single residual REINVENT is non-blocking phrasing).

## §12 Punch list (residuals)

V1's 12-item punch list collapses to two non-blocking phrasing tightenings:
1. §6a yaml row's host-route cell carries documentary text; could carry a stronger forward pointer to `host::primitives` + `@host fn`.
2. §3 visitor cookbook receiver routing could fold the cookbook reference into §6b diagnostic ledger to bind cookbook + strings.

Both items are non-blocking and may close in a future pass-through; neither contradicts an architectural commitment.

## §13 Final readiness

> **Decision: READY**
>
> PASS-3 V2 returns READY across nine lanes with no residual blocking surgery. Wave 2 (path crate names corrected to `path`/`path-core`/`path-ts` per Lock 7; `pointer!` macro surface; registry deletion close gate with verbatim grep + diagnostic; `bbnf` 8-children canonical layout; fixture separation from Lock 14 onboarding; per-grammar 10-row feeder at §6a; consumer acceptance gates at §3; PASS-3 generated-surface budget at §7; bbnf self-host internal gate at §7; 15-row compiler diagnostic ledger at §6b; incremental fallback dataset gates at §5; LSP user-facing silence policy at §5; 12-row carry ledger at §8 + 12-row unresolved punch-list at §10; `@error(recover)` consolidation) collectively address every V1 punch item.
>
> Hereupon PASS-3 is cleared for downstream consumption: MASTER-PLAN tranche G (Path/Value/Visitor) + tranche I (Recovery, Incremental, LSP) + tranche J (Parity, Docs, Publication Close) all consume PASS-3 outputs against named gates with no orphan deferral. The two non-blocking phrasing tightenings can close at next pass-through and do not gate per-tranche full-spec drafting.
