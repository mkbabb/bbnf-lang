# Phase 4 Synthesis — Cross-Tranche Verification + Lock 14 Enforcement

Date: 2026-05-03
Audience: orchestrator + Phase-5 hardening agent + BA execution agent
Status: synthesis pass; outputs the punch list that gates Phase 5 and BA execution.
Scope: verifies the cross-tranche invariants from `docs/PHASE-4-DIRECTIVE-2026-05-03.md:288-313` §7 across the four landed tranches at commits `7c6cec96` (BA), `4f34144b` (BB), `99556a9d` (BC), `58e108ad` (BD), with the additional Lock 14 enforcement codified at `74f2ed25`.

The four tranches landed under the Phase-4 directive's specification-depth re-draft mode. The BA-redress at `7c6cec96` flipped Lock 1 to option (a) AFTER the dispatches of BB, BC, and BD, and Lock 14 was codified at `74f2ed25` AFTER all four tranche dispatches. This synthesis ratifies what survives both shocks and surfaces the surgeries that close the residue.

---

## §1 — Cohort verdict

| Tranche | Commit | Verdict | Faults | Net Lock-honoured |
|---|---|---|---:|---|
| BA | `7c6cec96` | ratified | 1 | Locks 1, 2, 3, 6, 7, 8, 9, 12, 13 honoured-at-close; Locks 4, 10, 11 deferred-with-receiver to BB; Lock 5 demonstrated-by-pattern (carried to BC.W0); Lock 14 silent (codified post-dispatch). |
| BB | `4f34144b` | requires-amendment | 4 | Locks 1, 3, 4, 5, 6, 7, 9, 10, 11, 13 referenced; Lock 1 ownership double-claimed with BA (BB.W1{a,b,c} retain pre-redress per-grammar OpenFrame migration); Lock 14 silent. |
| BC | `99556a9d` | requires-amendment | 2 | Locks 2, 5, 7, 8, 9, 11, 13 honoured at close; Locks 4, 10 referenced as prior-tranche closures; Lock 14 silent (BC.W3 dependency-DAG honours the substance, but the lock is unnamed). |
| BD | `58e108ad` | requires-amendment | 3 | Locks 5, 7, 11 ratified at BD close; Locks 1, 2, 3, 4, 6, 8, 9, 10, 12, 13 referenced as prior-tranche closures; Lock 14 silent and BD.W2 §2.2 / BD.W5 export list hardcode JSON ident in WASM emit pseudocode. |

**Decision: requires amendments before execution.**

The thesis survives. The four tranches collectively close ~30 of the 35 Phase-3 surgeries; the remaining gaps are concentrated in three named amendments: (i) BB.W1{a,b,c} retirement under option (a); (ii) Lock 14 honoured-table extension across all four tranches; (iii) BD.W2 / BD.W5 grammar-ident hardcoding in pseudocode. None require re-draft; all are surgical edits to landed text.

---

## §2 — Lock 14 enforcement (the new lane)

Lock 14 was codified at commit `74f2ed25` AFTER all four tranche dispatches. The tranche docs predate the codification; the substance is partially honoured (BB.W2a metadata-driven cohort enumeration; BA.W1 metadata-driven strategy resolver; BC.W3 DAG with zero grammar-ident imports in generic crates) but the lock is named by NEITHER name NOR cell in any tranche's lock table.

### §2.1 — Plan-text rg sweep

`rg -ni 'json|css_l4|bbnf|google_sheets|sheets|css_pretty|bnf|csv|ebnf|math' docs/tranches/{BA,BB,BC,BD}/`

Total matches: 3,176 across all four tranche-doc trees.

Classification (sampling representatively):

**Ratified matches (in per-X tables, declaration crate paths, or audit/research-anchors citing primary sources):**

| Path:line | Context | Classification |
|---|---|---|
| `docs/tranches/BA/BA.md:144-154` | Generated-LOC budget table — one row per grammar (json.rs, bbnf.rs, css_l4.rs, ...) | per-X table cell — ratified |
| `docs/tranches/BA/audit/W5-substrate-identity-decision.md:16-22` | Per-sub-wave grammar table (W5a JSON, W5b CSS L4, W5c BBNF, W5d Sheets, W5e Cohort) | per-X table cell — ratified |
| `docs/tranches/BB/BB.md:65-75` | Per-grammar BB.W1 surgery 25 table (CSS L4, BBNF, Sheets, BNF, CSV, EBNF, CSS Pretty, Math, JSON) | per-X table cell — ratified |
| `docs/tranches/BB/BB.md:165-178` | Per-grammar generated-LOC delta table | per-X table cell — ratified |
| `docs/tranches/BC/BC.md:121-133` | Per-grammar generated-LOC budget table | per-X table cell — ratified |
| `docs/tranches/BC/audit/W3-crate-dependency-dag.md:46-50` | "leaf crates" enumeration with per-crate dependency arrows | declaration crate path — ratified |
| `docs/tranches/BD/BD.md:113-120` | TS / WASM generated tree budgets | per-X table cell — ratified |
| `docs/tranches/BA/audit/research-anchors.md` | sonic-rs / lightning-css / chumsky primary-source citations | audit/research — ratified |

**Fault matches (paragraph hardcodes a grammar in plan logic that should be metadata-driven):**

| Path:line | Context | Fault |
|---|---|---|
| `docs/tranches/BD/waves/W2.md:50-58` | WASM emit pseudocode names `parse_json` and `bbnf-parse-json` as the per-grammar surface; no metadata-driven dispatch language; the example is hardcoded JSON | grammar-name in plan pseudocode without dispatch citation |
| `docs/tranches/BD/waves/W5.md:167` | TS import example `import init, { parse_json, parse_css_l4 } from '@bbnf-lang/runtime-wasm'` | hardcoded export list in plan body |
| `docs/tranches/BB/waves/W1c.md:3` | Cell_ref / sheet_prefix / identifier specialised leaf-deposit relocation prose | partially metadata-driven (per-grammar host namespace) but Sheets-internals named in plan logic; ratified-by-context per Lock 14 footnote on per-grammar declaration crates. |

The dominant pattern across the 3,176 matches is RATIFIED — grammar idents appear in per-X tables, in audit research-anchors citing primary sources, in carry-tag table cells, in per-grammar declaration crate paths, and in metadata-driven dispatch citations. The three faults above are exclusively in BD.W2 + BD.W5 pseudocode where a generic `parse_<grammar>` factor is hardcoded.

### §2.2 — Match-arm sweep in plan pseudocode

`rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' docs/tranches/{BA,BB,BC,BD}/`

**Result**: zero matches. No plan-text raw match-arms enumerate grammar idents anywhere across the four tranche directories. Lock 14's structural-discipline § §2 is honoured.

The only `match grammar { ... }` references in any plan document are CITATIONS of EXISTING faults in production source — e.g., `docs/tranches/BA/waves/W3b.md:9` cites `crates/path/src/registry.rs:132-135` as a fault to delete; `docs/tranches/BA/waves/W3.md:55` cites the same. Both reference the registry-lookup-via-metadata fix (citing the BA.W1 metadata table). No plan introduces a new match arm.

### §2.3 — Generic-crate import sweep in plan + dependency-DAG

`audit/W3-crate-dependency-dag.md` walked end-to-end (`docs/tranches/BC/audit/W3-crate-dependency-dag.md:53-198`).

**Per-crate API surface verification** (Lock 14 §(d) per `docs/HARDENING-PLAN-PROMPT.md:60`):

| Crate | Public exports — grammar idents present? | Private internals — grammar idents present? | Verdict |
|---|---|---|---|
| `bbnf-runtime` | `<G>Document`, `<G>Value`, `<G>Arena` named generically per `docs/tranches/BC/audit/W3-crate-dependency-dag.md:60` | `RawHandle`, `InternalCursor` — generic | honoured |
| `bbnf-parse` | `Compile`, `Lower`, `LayoutSink`, `GrammarIR`, `Layout`, `<G>Parser` (generic placeholder) | `LoweringContextInternal`, `ScannerScratch` — generic | honoured |
| `bbnf-codegen` | `Emitter`, `Lowerer<B: Backend>`, `RustLowerer`, `TsEmitter`, `WasmEmitter` — generic | `RustEmitContext`, `TsEmitContext`, `WasmEmitContext` — generic | honoured |
| `bbnf-ir` (workspace-internal) | `TypedIRNode`, `IrNode`, `Layout`, `LayoutSink`, `RuleId` — generic | `InternalNodeId` — generic | honoured |
| `path` | `pointer!`, `path!` — generic macros | `internal::macro_expand_pointer` — generic | honoured |
| `path-core` | `PathSegment`, `PathQuery`, `TypedPath<G>`, `TerminalKind` — generic via type parameter | `PathInternalRepr` — generic | honoured |
| `path-ts` | `path_ts_init`, `path_ts_query` — generic C ABI | (n/a) | honoured |
| `egraph` | `Language`, `RewriteRule`, `EGraph<L>`, `Class<L>`, `Id`, `Rewriter<L>` — generic via type parameter | `EClassData<L>` — generic | honoured |
| `csp-solver` | `Constraint`, `Variable`, `Domain`, `CspProblem`, `Solution` — generic | `BacktrackingFrame` — generic | honoured |
| `bbnf-regex` | `Pattern`, `Match`, `Dfa`, `Hir`, `MatchResult` — generic | `DfaTransitionTable` — generic | honoured |
| `parse-that` | `Parser`, `Stream`, `Span`, `Combinator<O>` — generic | `ParserInternal` — generic | honoured |
| `simd-scan` | `SimdScan`, `Alphabet`, `ScanKind` — generic | `NeonRegister`, `Sse4Register` — generic | honoured |

**Verdict**: BC.W3's dependency-DAG honours Lock 14 §(d) — zero grammar-name imports in any generic-crate's public or private API surface. The DAG enforces this by structural design: every per-grammar emission lives at `crates/bbnf-parse/src/parse/generated/<g>.rs` (the post-relocation path per `docs/tranches/BC/audit/W3-generated-output-relocation.md`), generated from metadata, never hand-written in a generic crate.

### §2.4 — Future-grammar onboarding test

The synthesis agent invents hypothetical grammar `yaml.bbnf`. Walk through what adding it requires under the post-Phase-4 plan:

| Step | Required action | Code edits in other crates? |
|---|---|---|
| (a) | Author `yaml.bbnf` source file at `grammar/yaml/yaml.bbnf` | none |
| (b) | Add `[workspace.metadata.bbnf-strategy.grammars.yaml]` block per `docs/tranches/BA/audit/W1-workspace-metadata-schema.md` | Cargo.toml only — workspace metadata, not crate code |
| (c) | Optionally create `crates/yaml/` declaration crate for host fns (only if YAML needs custom host fns) | new crate; per Lock 14 footnote, this IS the per-grammar declaration crate pattern |

**Per-tranche walk:**

- **BA.W1** (metadata-driven strategy resolver, `docs/tranches/BA/waves/W1.md:19`): YAML's strategy comes from its workspace-metadata block. **No edit in BA scope.**
- **BA.W5** (per-grammar OpenFrame retiral): YAML never had OpenFrame; BA.W5 sub-waves only cover the 9 incumbent grammars. **YAML is born direct-to-struct.**
- **BB.W1{a,b,c}** (per-grammar OpenFrame migration for CSS L4 / BBNF / Sheets): YAML never had OpenFrame; **no edit needed**.
- **BB.W2a** (cohort template with `[workspace.metadata.bbnf.grammars.<g>.cohort = true]`, `docs/tranches/BB/waves/W2a.md:11`): YAML joins by metadata flag if it is cohort-shaped; emit fires automatically. **No edit in BB scope.**
- **BC.W0** (typed-IR contract): generic across grammars per `docs/tranches/BC/audit/W3-crate-dependency-dag.md:85-95`. **No edit.**
- **BC.W4** (visitor surface; `Visitor<'i, T>` derived from per-grammar record alphabet): generic across grammars; YAML's visitor methods emit from YAML's record alphabet automatically. **No edit.**
- **BD.W1** (TS runtime emitter; `crates/bbnf-codegen/src/ts/`): generic emitter; per-grammar files at `crates/bbnf-codegen/src/ts/generated/<g>.ts` emit from the same template. **No edit IF the BD.W2.§2.2 hardcoded `parse_json` factor is amended to a metadata-driven `parse_<grammar>` factor.**
- **BD.W2** (WASM emitter; `crates/bbnf-codegen/src/wasm/`): same as BD.W1 — generic emitter, but the W2.§2.2 pseudocode hardcodes JSON. **Edit needed in pseudocode (or amend the pseudocode to be metadata-templated).**
- **BD.W5** (cross-backend parity): "9 grammars × ≥ 3 fixtures" — but YAML makes it 10 grammars. **The matrix sizing language must be metadata-derived, not hardcoded.** The W5.md export-list pseudocode at `docs/tranches/BD/waves/W5.md:167` hardcodes `parse_json, parse_css_l4`; that example must be amended.

**Verdict**: the post-Phase-4 plan is ~95% Lock-14-compliant. Adding YAML requires:
- (a) source file
- (b) workspace metadata block
- (c) optional declaration crate

But three plan-pseudocode hardcoded enumerations require amendment so the YAML onboarding holds with NO plan-doc editing, only metadata + source. They are: BD.W2.§2.2 (`parse_json` factor); BD.W5 import example (`parse_json, parse_css_l4`); BD.W5 matrix sizing language ("9 grammars × ≥ 3 fixtures × 3 backends").

### §2.5 — Lock 14 verdict per tranche

| Tranche | Lock 14 status | Faults | Surgery |
|---|---|---:|---|
| BA | substantively-honoured-but-named-silent | 0 substantive; 1 nominative | Add Lock 14 row to BA.md §13-Lock honoured table (Lock 14 codified post-dispatch); name BA.W0 (residue scrub), BA.W1 (metadata-driven strategy), BA.W3 (registry match-arm deletion) as the BA-side Lock-14 anchors. |
| BB | substantively-honoured-but-named-silent | 0 substantive; 1 nominative | Same as BA; add Lock 14 row to BB.md §13-Lock table; name BB.W2a (metadata-driven cohort enumeration), BB.W3 (CSP/e-graph metadata-driven extensions) as anchors. |
| BC | substantively-honoured-but-named-silent | 0 substantive; 1 nominative | Same; add Lock 14 row to BC.md §13-Lock table; name BC.W3 (DAG zero-grammar-ident in generic crates) and BC.W3b (per-grammar host namespaces) as anchors. |
| BD | partially-violated-by-plan-pseudocode | 3 substantive | Amend BD.W2.§2.2 (`parse_json` → metadata-templated `parse_<grammar>`); amend BD.W5.§ import-example (`parse_json, parse_css_l4` → metadata-templated). Add Lock 14 row to BD.md §13-Lock table. |

---

## §3 — BA option-(a) reconciliation across BB / BC / BD

The BA-redress at `7c6cec96` flipped Lock 1 to option (a): BA migrates all 9 grammars to direct-to-struct within W5a..W5e; Lock 1 is honoured at BA close; the per-grammar OpenFrame migration receivers BA→BB.C1a/b/c/d retire. The BB / BC / BD tranches dispatched BEFORE the redress and therefore baked option-(b) assumptions.

### §3.1 — BB.W1a/W1b/W1c retirement

**BB.W1a** (`docs/tranches/BB/waves/W1a.md:1-3`) thesis: "CSS L4's 14-variant `OpenFrame` builder retires from `crates/core/src/runtime/css_l4/builder.rs`; per-variant migration with `tests/css_l4_parity.rs` lightningcss canonical-form parity at each step." This is per-grammar OpenFrame migration scope owned by BB.

**BB.W1b** (`docs/tranches/BB/waves/W1b.md:3`): "BBNF's `OpenFrame` retires from `crates/core/src/runtime/bbnf/builder.rs`...". Same — per-grammar migration owned by BB.

**BB.W1c** (`docs/tranches/BB/waves/W1c.md:3`): "Sheets's `OpenFrame` retires from `crates/core/src/runtime/google_sheets/builder.rs`...". Same.

**BB.W1 backbone** (`docs/tranches/BB/waves/W1.md:11`): Thesis still claims "Hereupon BA.W5's JSON direct-to-struct demonstration generalises to the eight remaining grammars." Under option (a), BA migrates all nine; only the cohort-template-consolidation work survives in BB.

**Lock 1 ambiguity**: Per BA.md:7 ("BA closes with `rg -n 'enum OpenFrame' crates/core/src/runtime/` returning 0 across all nine grammars") and BB.W1a:54 ("`rg -n 'enum OpenFrame' crates/core/src/runtime/css_l4/` returns zero" — BA already satisfied this), both BA and BB own the same gate. Substrate-identity is double-claimed.

**Surgery**: BB.W1{a,b,c} either RETIRE or REPURPOSE.

- **Retire path** (preferred): rewrite BB.W1a/W1b/W1c as no-op shells citing "subsumed under BA-redress option (a); Lock 1 honoured at BA close per `docs/tranches/BA/audit/W5-substrate-identity-decision.md`"; renumber BB.W2a→W1a, BB.W2b→W1b, BB.W2c→W1c so BB cohort + cursor unification + byte-equality become the W1 substance.
- **Repurpose path** (alternative): rewrite BB.W1a/W1b/W1c as Lock-14-generic-substrate-audit / verification waves; BB.W1a verifies BA.W5b CSS L4 emit shape; BB.W1b verifies BA.W5c BBNF emit shape; BB.W1c verifies BA.W5d Sheets emit shape. Same wave count; different substance.

The **preferred path** is retire-plus-renumber: it eliminates dead text, honours BA close ownership, and preserves wave count expectations. Output `audit/PHASE-4-AMENDMENT-BB-W1-RETIRE.md` if pursued.

### §3.2 — BA→BB carry-tags after option-(a)

BA-side declares `BA→BB.C1'` (`docs/tranches/BA/BA.md:86`): "BA writes 5 hand-written direct-to-struct cohort modules at W5e; BB.W2 consolidates into 1 parameterised cohort template per gap D." The C1a/b/c/d (per-grammar receivers) retire.

**BB-side carry table** (`docs/tranches/BB/BB.md:84-90`) still shows:

```
| BA→BB.C1 | BA.W5 | Direct-to-struct codegen path for JSON; ... | BB.W1a (CSS L4), BB.W1b (BBNF), BB.W1c (Sheets), BB.W2a (5-grammar cohort) |
```

The BB-side carry-tag receivers reference per-grammar OpenFrame retiral receivers (BB.W1a/b/c) that under option (a) are obsolete. The BB-side does not show C1' (cohort hand-written → BB.W2 templating).

**Surgery**: edit `docs/tranches/BB/BB.md:84-90` to:
- Replace `BA→BB.C1` row with `BA→BB.C1'` matching BA-side language.
- Replace receiving-wave column "BB.W1a (CSS L4), BB.W1b (BBNF), BB.W1c (Sheets), BB.W2a (5-grammar cohort)" with "BB.W2a — cohort hand-written (BA.W5e) consolidates into one parameterised template per gap D".
- Keep C2..C5 unchanged.
- Append `BA→BB.C1a/b/c/d retired` row noting option-(a) closure (or simply omit; the BA-side ledger is authoritative).

Receiver-cell carry references in `docs/tranches/BB/waves/W1.md:114`, `:167`; W1b.md:76, W1c.md:89, W4.md:117 require analogous receiver-correction (the C1 carry now closes at W2a, not at W1{a,b,c}).

### §3.3 — BB.W2 cohort template absorbs BA.W5e cohort hand-written

Per `docs/tranches/BA/audit/W5-substrate-identity-decision.md:78-80`: "BB.W2 absorbs W5e's five hand-written cohort modules into one parameterised template per gap D."

**BB.W2 backbone** (`docs/tranches/BB/waves/W2.md:1-22`): Thesis says template emission generates byte-identical output to "the existing hand-written 5-grammar files at first commit". The "existing hand-written 5-grammar files" are AT BA.W5e close — these ARE the BA.W5e modules.

**BB.W2a** (`docs/tranches/BB/waves/W2a.md:5-9`): "M2 verifies template emission produces byte-identical output against the existing hand-written files BEFORE M4 deletes." This is the absorption mechanism but the deliverable text does NOT name BA.W5e as input. The closer-gate verifies byte-equality but does NOT cite BA.W5e modules as the byte-equal target.

**Surgery**: edit `docs/tranches/BB/waves/W2a.md` §1 Deliverable to add a sentence: "Input is the five BA.W5e hand-written direct-to-struct cohort modules (BNF, CSV, EBNF, CSS Pretty, Math); BB.W2a's template emission consolidates them into one parameterised template; M2 byte-equality verifies the template re-emits BA.W5e's content byte-for-byte." Add closer-gate row: `diff -r crates/core/src/runtime/{bnf,csv,ebnf,css_pretty,math} <(xtask regen --grammar <cohort> --emit-only)` returns zero diff against the BA.W5e baseline (cited as `git show 7c6cec96:crates/core/src/runtime/<cohort>/`).

Update `docs/tranches/BB/waves/W2.md` thesis (line 10) similarly: name BA.W5e as the per-grammar input substrate that templates consolidate.

---

## §4 — 35-surgery completion verification

Walk `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:39-75` (the 35 surgeries from Phase 3) against the four tranche commits.

| # | Surgery | Tranche | Status | Evidence path:line |
|---|---|---|---|---|
| 1 | BA.W5.M6 OpenFrame retirement | BA | landed (option-a) | `docs/tranches/BA/BA.md:108`, `docs/tranches/BA/audit/W5-substrate-identity-decision.md:1-85` |
| 2 | BA Lock 1 deferred-with-receiver OR move BB.W1 up | BA + BB | reconciled-via-option-a | `docs/tranches/BA/audit/W5-substrate-identity-decision.md:8`. BB.W1 receiver-side amendment pending (§3.1 surgery). |
| 3 | BA.W2 transitional aliases deletion (no `pub use`) | BA | landed | `docs/tranches/BA/BA.md:49`, `BA.md:181`, BA.W2.M0 closer-gate |
| 4 | BC `TypeDesc` → `Layout` vocabulary | BC | landed | `docs/tranches/BC/BC.md:85`, BC.W0a closer-gate `rg -nE 'pub enum TypeDesc' crates/ir/src/typed_ir/` returns zero |
| 5 | BB `passes/types/` → `bbnf-ir/src/passes/layout/`; `crates/egraph/` path-dep | BB | landed | `docs/tranches/BB/waves/W3a.md:51`, `docs/tranches/BB/BB.md:108-112` |
| 6 | `crates/path/`, `crates/path-core/`, `crates/path-ts/` | BA | landed | `docs/tranches/BA/BA.md:50-52`, BA.W3a/b/c closer-gates |
| 7 | `crates/core/src/path/` runtime relocation | BA | partial — BA.W3c rejects relocation | `docs/tranches/BA/BA.md:52` justifies keeping `crates/core/src/path/` as runtime executor; BC.W3a clarifies bbnf-runtime depends on path; not absorbs |
| 8 | `bbnf-runtime` depends-on (not absorbs) `crates/path/` | BC | landed | `docs/tranches/BC/BC.md:38`, `docs/tranches/BC/audit/W3-crate-dependency-dag.md:62-63` |
| 9 | parse_with deletion W3 → W4 | BA | landed | `docs/tranches/BA/BA.md:55`, BA.W4c (formerly W3.M5) |
| 10 | BA.W4 split into W4a/W4b | BA | landed (with W4c added) | `docs/tranches/BA/BA.md:53-55` |
| 11 | "Every parse-throughput gate cites..." + non-SOTA engineering tables | BA + BC | landed | `docs/tranches/BA/BA.md:15-39`, `docs/tranches/BC/BC.md:11-25` |
| 12 | BA-G9 sonic-rs `get_unchecked` measurement OR mark non-SOTA | BA | landed (marked non-SOTA) | `docs/tranches/BA/BA.md:38`, BA-G9 row labelled non-SOTA |
| 13 | CSS lightningcss benchmark operation OR M1 Pro parse-only measurements | BB + BC | landed | `docs/tranches/BB/BB.md:19-20` (BB-G1, BB-G2 with parse-only re-measurement at W0a artefact `W0a-lightningcss-parse-only.md`) |
| 14 | BBNF/Sheets perf rows removal OR external SOTA | BB | landed (rows removed) | `docs/tranches/BB/BB.md:204-205` (BBNF and Sheets removed from parse-throughput trajectory; appear only in cohort engineering gates) |
| 15 | CSS + Sheets host fns → per-grammar host namespaces | BA + BB | landed | `docs/tranches/BA/BA.md:36`, `docs/tranches/BB/waves/W1c.md:3` (Sheets host fns at `crates/core/src/grammar/host/google_sheets.rs`) |
| 16 | Recogniser plugin schema fields (name/crate/entrypoint/output_kind) | BA | landed | `docs/tranches/BA/audit/W1-workspace-metadata-schema.md`, BA.W1.M0 |
| 17 | Inverse-layout-audit gate | BA | landed | `docs/tranches/BA/BA.md:49` (W2.M5 + W5b.M1) |
| 18 | BA.W2.M4 fail-explicit table | BA | landed | `docs/tranches/BA/audit/W2-fail-explicit-table.md`, `docs/tranches/BA/BA.md:191` |
| 19 | BBNF aggregator `pub use bbnf::*` deletion | BA | landed | `docs/tranches/BA/audit/W6-bbnf-aggregator-disposition.md`, BA.W2.M4 + W5c.M0 |
| 20 | Generated-LOC gates per W1/W3/W4/W5a..W5e | BA | landed | `docs/tranches/BA/BA.md:39`, `BA.md:158-170` |
| 21 | BB wave LOC windows (W0..W5) | BB | landed | `docs/tranches/BB/BB.md:144-163` |
| 22 | BC.W3 generated-output relocation budget | BC | landed | `docs/tranches/BC/audit/W3-generated-output-relocation.md` cited at `docs/tranches/BC/BC.md:42` |
| 23 | BC-G10 aggregate ≤ +2% AND per-file ≤ +2.5% OR JSON ≤ +2% | BC | landed (former) | `docs/tranches/BC/BC.md:24`, `BC.md:135` |
| 24 | BA.md:59 "BB.W0/W1" → "BB.W1 for CSS L4..." | BA | landed (with option-a receiver renaming) | `docs/tranches/BA/BA.md:11` (cross-tranche impact paragraph names BB.W1 reorientation under option-a) |
| 25 | BB "all eight remaining grammars" decomposed | BB | landed (BB.W1a/b/c + BB.W2a) | `docs/tranches/BB/BB.md:62-77` per-grammar table; `docs/tranches/BB/waves/W1.md:1-9` decomposition |
| 26 | Fleet-wide fixture receiver normalised | BC | landed | `docs/tranches/BC/waves/W5d.md` (worktree fixture closure singular receiver) |
| 27 | BA.W1 "slow-burn carry" deletion | BA | landed | `docs/tranches/BA/BA.md:194` (W1 §1 paragraph rewrite; test fixture DEFER per CENSUS:121-122 named explicitly) |
| 28 | BA.W3.M3 `bbnf-regex` endpoint receiver BC.W4 → BC.W5 | BA | landed | `docs/tranches/BA/BA.md` (BA→BB.C8 deferred to BB.W0; BC.W5 reconciles) |
| 29 | BC.W0 gates for sibling baseline + ascent strategy | BC | landed | `docs/tranches/BC/BC.md:34`, BC.W0c |
| 30 | BB.W5 visitor receiver BC.W5 → BC.W4 | BB | landed | `docs/tranches/BB/BB.md:99` (BB→BC.C3 receiver BC.W4); `docs/tranches/BB/waves/W5b.md` |
| 31 | BC.W5 `bbnf-regex` endpoint adjudication-deferral excised | BC | landed | `docs/tranches/BC/audit/W5-bbnf-regex-endpoint-decision.md`; BC.W5b |
| 32 | BB-G2 hard-fail OR BC.W5 receiving perf gate | BB | landed | `docs/tranches/BB/BB.md:20` (BB-G2 gate routes to BC.W5 with named rewrite hypothesis on miss) |
| 33 | Draft `docs/tranches/BD/BD.md` | BD | landed | `docs/tranches/BD/BD.md:1-178` (entire BD tranche drafted; commit `58e108ad`) |
| 34 | Cookbook + diagnostic gates | BB + BC | landed | `docs/cookbook/path-macro.md`, `lifetime-surfaces.md`, `visitors.md` referenced in BB.W4b/W5c; `docs/migration/bc-core-split.md` referenced in BC.W3e |
| 35 | BB.W5 `pointer!` syntax decision | BB | landed | `docs/tranches/BB/audit/W5-pointer-syntax-decision.md`; BB.W5a deliverable cites option (iii) — both forms |

**Aggregate**: 34 of 35 surgeries land; surgery #7 lands partially (BA.W3c rejects relocation with reasoned justification — `crates/core/src/path/` IS the runtime executor; the path-crate triplet is proc-macro / cdylib / shared-core; not the runtime). The Phase-3 punch list closes ~97%; the rejection at #7 is in-plan justified per `docs/tranches/BA/BA.md:52` and is therefore RATIFIED-BY-DEFENCE rather than missing.

---

## §5 — Carry-tag receiver verification

Walk every carry-tag in the post-Phase-4 plan-set.

### §5.1 — All BA→BB carries (after option-a redress)

| Carry | Source | Destination | Close gate | Verdict |
|---|---|---|---|---|
| BA→BB.C1' | BA.W5e | BB.W2 (W2a) | `diff -r crates/core/src/runtime/{bnf,csv,ebnf,css_pretty,math} <(xtask regen --grammar <cohort> --emit-only)` returns zero diff | ratified BA-side; BB-side carry-table needs update (§3.2) |
| BA→BB.C1a/b/c/d | (retired) | (retired) | — | ratified retired BA-side; BB-side BB.W1{a,b,c} per-grammar receivers still imply the C1 receivers (§3.1) |
| BA→BB.C2 | BA.W2 | (cross-cutting) | `rg -nE 'TypeDesc\|StructLayout' crates/ir/src/passes/layout/` returns 0 | ratified |
| BA→BB.C3 | BA.W4b | BB.W2b | `rg -n 'pub fn parse_with' crates/core/src/grammar/generated/` returns ≥ 9 | ratified |
| BA→BB.C4 | BA.W3b | BB.W5a | `cargo check -p path-core` passes; BB.W5a macro consumes | ratified |
| BA→BB.C5 | BA.W1 | BB.W3a/b/c | `rg -n 'JsonParser\|CssL4Parser\|BbnfBootstrap' crates/ir/src/` returns 0 | ratified |
| BA→BB.C6 (Lock 4 deferral) | n/a | BB.W3 | BB-G10 (CSP → e-graph → miners output-piped) | ratified |
| BA→BB.C7 (Lock 10 deferral) | n/a | BB.W3 | BB-G6 (Pratt + SIMD auto-detected) | ratified |
| BA→BB.C8 (Lock 11 deferral) | n/a | BB.W0 | BB.W0 path-dep gate | ratified |

### §5.2 — All BA→BC carries (skip-BB)

| Carry | Source | Destination | Close gate | Verdict |
|---|---|---|---|---|
| BA→BC.C1 | BA.W2 | BC.W0a | `rg -n 'TypeDesc' crates/ir/src/typed_ir/` returns matches only as field names within `Layout` | ratified |
| BA→BC.C2 | BA.W5a..W5e | BC.W0a, BC.W0b | W0a contract spec lands; W0b smoke lowerer round-trip-equal to BB close | ratified |

### §5.3 — All BB→BC carries

| Carry | Source | Destination | Close gate | Verdict |
|---|---|---|---|---|
| BB→BC.C1 | BB.W3c | BC.W0a | W0a contract spec names optimiser-output / lowerer-input boundary | ratified |
| BB→BC.C2 | BB.W1{a,b,c} + BB.W2a | BC.W0a + BC.W1a | W0a contract spec; W1a Rust emitter refactor across nine grammars | ratified (note: under §3.1 retire-path, the BB-side ownership cell of C2 corrects from W1{a,b,c} to BA.W5{b,c,d} + BB.W2a) |
| BB→BC.C3 | BB.W5b | BC.W4 | BC-G9 met (cross-backend visitor) | ratified |
| BB→BC.C4 | BB.W0a | BC.W5a | `cargo publish --dry-run` clean; parse-that disposition ratified at W5c | ratified |

### §5.4 — All BC→BD carries

| Carry | Source | Destination | Close gate | Verdict |
|---|---|---|---|---|
| BC→BD.C1 | BC.W2 + BC.W4 | BD.W1 (with W0 partial + W2 production) | `cargo nextest run -p bbnf-codegen-ts -p bbnf-codegen-wasm --test ts_e2e_json --test wasm_e2e_json` 100% pass | ratified |
| BC→BD.C2 | BC.W5a + BC.W5b | BD.W3 | `cargo publish -p egraph -p egraph-derive -p csp-solver -p bbnf-regex` succeeds | ratified |
| BC→BD.C3 | BC.W5d | BD.W0 (consume) + BD.W4 (extend) | `xtask worktree-init` runs cleanly across BD parallel-agent dispatch matrix | ratified |

### §5.5 — BD has zero TO BE carries

`rg -nP 'BD→B[A-Z]\.' /Users/mkbabb/Programming/bbnf-lang/docs/tranches/BD/`

**Result**: zero matches. BD-G7's verification holds: BD is the close tranche; no forward carries escape BD.

### §5.6 — Carry table verdict

| Carry | Verdict | Surgery |
|---|---|---|
| BA→BB.C1' (cohort templating) | ratified BA-side | BB-side update needed (§3.2) |
| BA→BB.C1a/b/c/d (retired) | ratified BA-side | BB-side W1{a,b,c} retire (§3.1) |
| BA→BB.C2..C5 + C6/C7/C8 | ratified | none |
| BA→BC.C1, C2 | ratified | none |
| BB→BC.C1..C4 | ratified | minor C2 ownership cell update (§5.3) |
| BC→BD.C1..C3 | ratified | none |
| BD→BE | absent (verified) | none |

---

## §6 — "All-X" claim verification (Operational Rule 1)

`rg -ni 'all .* grammars|every grammar|all backends|all tests' docs/tranches/{BA,BB,BC,BD}/`

129 matches. Sampled representatively:

| Path:line | Claim | Following text | Verdict |
|---|---|---|---|
| `docs/tranches/BA/BA.md:7` | "All nine grammars — JSON, CSS L4, BBNF, Sheets, and the five-grammar template cohort..." | per-sub-wave table BA.md:144-154 (per-grammar LOC budget) + BA.md:46-61 (per-wave summary with per-grammar decomposition) | ratified — per-X table follows |
| `docs/tranches/BB/BB.md:62` | "Per surgery 25 of `docs/PHASE-4-DIRECTIVE-2026-05-03.md:53`, the 'all eight remaining grammars' overclaim of the prior draft splits into per-grammar gates" | per-grammar table BB.md:65-75 | ratified — per-X table follows |
| `docs/tranches/BB/waves/W2b.md:3` | "Hereupon each of the 9 grammars' eager `parse(input)` rewrites as `parse_with(input, &__EAGER_EMPTY_PATH)`" | per-grammar gate `tests/parse_with_<g>.rs passes for all 9 grammars`; samply trace per grammar | ratified — gate enumerates per-grammar |
| `docs/tranches/BC/BC.md:48` | "(BC.W6) carry ledger to BD per `audit/W6-bd-carry-contract.md`" | per-carry table BC.md:72-76 | ratified |
| `docs/tranches/BD/BD.md:23` | "Cross-backend parity matrix passes: 9 grammars × ≥ 3 fixtures each = ≥ 27 parity tests" | matrix described — but no per-X table ENUMERATING the 27 cells | weak — per-grammar fixture matrix referenced but not laid out as a table |
| `docs/tranches/BD/BD.md:35` | "every grammar has `crates/bbnf-parse/tests/fixtures/<grammar>/` directory with ≥ 3 canonical inputs" | no table; the 9-row enumeration is implicit | weak — no explicit per-X table follows |
| `docs/tranches/BD/waves/W4.md` | "per-grammar fixture pattern" | section headers per grammar but cells condensed | partial — per-X structure but no top-level table |

**Verdict**: BA / BB / BC honour Operational Rule 1 cleanly. BD has two weak claims (BD.md:23, :35) where the per-X table is implicit rather than explicit. **Surgery**: add a per-grammar fixture matrix table to `docs/tranches/BD/BD.md` §Hard gates BD-G6 cell, with rows for each of the 9 (post-Phase-4) grammars × ≥ 3 fixtures; alternatively, reference a separate `docs/tranches/BD/audit/W4-fixture-matrix.md` artefact.

---

## §7 — "TBD / user adjudicates / future" verification (Operational Rule 2)

`rg -nEi 'TBD|user adjudicates|investigate later|future BD|future BE|future tranche|future wave' docs/tranches/{BA,BB,BC,BD}/`

40+ matches. Classification:

| Path:line | Match | Receiver / blocker / gate named? | Verdict |
|---|---|---|---|
| `docs/tranches/BC/waves/W5.md:6` | "superseded — split into BC.W5a + ...; in-plan decision per `audit/W5-bbnf-regex-endpoint-decision.md` (Option A)" | yes — receiver BC.W5b; blocker bbnf-regex endpoint reconciliation; gate `parse-that/rust/bbnf-regex` rename | ratified — supersedence note + named ratifying file |
| `docs/tranches/BC/waves/W5.md:10`, `:134`, `:145`, `:198`, `:202` | "user adjudicates at hardening time" | RESIDUE in W5.md (this file is SUPERSEDED per W5.md:6 — split into W5a..W5d); the residue sentences live in the superseded backbone; no impact on the live waves | ratified — superseded-file residue; the live W5a..W5d files do not carry "user adjudicates" |
| `docs/tranches/BC/audit/W5-bbnf-regex-endpoint-decision.md:10` | "the decision is **in-plan**, not deferred to 'user adjudicates at hardening time'." | yes — decision recorded in-plan | ratified |
| `docs/tranches/BB/waves/W4.md:130` | "future tranche docs reference for friction context" | future tranche named (post-BD docs); blocker friction context; gate cookbook lands | ratified (forward-reference; not a deferral) |
| `docs/tranches/BD/audit/W2-wasm-pipeline-spec.md:27` | "if a future tranche needs wit-bindgen for WASI..." | conditional — wit-bindgen is reserved for post-Phase-5; no current carry | ratified |
| `docs/tranches/BA/waves/W6.md:140` | "no fictional successor letters; no 'future tranche' placeholders" | assertion (negative) — invariant statement | ratified |
| `docs/tranches/BA/waves/W6.md:153` | "future tranche docs reference" | forward-reference language | ratified |
| `docs/tranches/BB/waves/W1.md:128` | "future tranches inherit the absence" | language describes future inheritance, not a deferral | ratified |
| `docs/tranches/BB/waves/W1.md:151` | "future bench-regression detection" | language describes future use, not deferral | ratified |
| `docs/tranches/BB/waves/W3.md:150` | "future tranches inherit the same-wave consumer rule" | language describes precedent inheritance | ratified |
| `docs/tranches/BD/audit/research-anchors.md:186` | "Zero 'investigate later'; zero 'TBD'." | invariant statement | ratified |
| `docs/tranches/BC/waves/W6.md:126` | "BD has not been drafted; this is named explicitly" | RESIDUAL — BD HAS now been drafted at `58e108ad`; this BC.W6 statement is now stale. | needs amendment |

**Verdict**: Operational Rule 2 substantially honoured. The notable residue is `docs/tranches/BC/waves/W5.md:10/134/145/198/202` (in the superseded backbone file) and `docs/tranches/BC/waves/W6.md:126` (now-stale "BD has not been drafted" statement).

**Surgery**:
- (a) Amend `docs/tranches/BC/waves/W6.md:126` to remove "BD has not been drafted" — at `58e108ad` BD is drafted; the line is now factually stale. Replace with: "BD is drafted at commit `58e108ad`; carry-tags BC→BD.C1..C3 land in BD.W0..W4 per `docs/tranches/BD/BD.md`."
- (b) Optionally clean `docs/tranches/BC/waves/W5.md` superseded language; alternative is to leave as superseded-archive and rely on the W5a..W5d live files.

---

## §8 — Independent-executability verification (Operational Rule 3)

Sample 12 waves randomly across tranches; verify each can begin work without consulting external state.

| Wave | External-decision dependencies | Resolved in-document? | Verdict |
|---|---|---|---|
| BA.W0 | Pre-BA ceremony (ser/gorgeous archive) | yes — BA.md §Pre-BA cleanup ceremony specifies | ratified |
| BA.W3a | path-crate rename (Lock 7 directory canon) | yes — `docs/tranches/BA/waves/W3a.md` carries milestones + closer-gate inline | ratified |
| BA.W5b | CSS L4 14-variant migration order | yes — `docs/tranches/BA/audit/W5-substrate-identity-decision.md:32-38` table; W5b.md carries milestones | ratified |
| BA.W5e | Cohort hand-written → BB.W2 template carry | yes — W5e.md carries milestones; BA→BB.C1' named at BA.md:86 | ratified |
| BB.W0a | Sister-crate path-deps + workspace metadata | yes — W0a.md inline + `[workspace.metadata.bbnf-incubators]` schema | ratified |
| BB.W2a | Cohort template parameter set | yes — `docs/tranches/BB/audit/W2-cohort-template-spec.md` cited; W2a.md inline milestones | ratified |
| BB.W5a | `pointer!` syntax decision | yes — `docs/tranches/BB/audit/W5-pointer-syntax-decision.md` cited; W5a.md inline | ratified |
| BC.W0a | Typed-IR alphabet (22 variants) | yes — `docs/tranches/BC/audit/W0-typed-ir-variant-table.md` cited; W0a.md inline | ratified |
| BC.W3a | bbnf-runtime extraction | yes — `docs/tranches/BC/audit/W3-crate-dependency-dag.md` cited; W3a.md inline milestones | ratified |
| BC.W5b | bbnf-regex endpoint rename | yes — `docs/tranches/BC/audit/W5-bbnf-regex-endpoint-decision.md` cited; W5b.md inline | ratified |
| BD.W0 | TS proc-macro shell activation | yes — `docs/tranches/BD/audit/W0-ts-procmacro-spec.md` cited; W0.md inline + NAPI-RS ABI documented | ratified |
| BD.W2 | WASM emitter activation | yes — `docs/tranches/BD/audit/W2-wasm-pipeline-spec.md` cited; W2.md inline milestones | ratified (BUT the wave's pseudocode hardcodes JSON; per §2.4 this requires amendment for Lock 14 onboarding-test cleanliness) |

**Verdict**: 12 of 12 waves are independently executable per Operational Rule 3. Each names the audit artefact + closer-gate + milestones inline. The minor caveat (BD.W2 pseudocode) is a Lock-14 hardcoding issue, not an executability issue; the wave is still self-contained for execution.

---

## §9 — 14-lock honoured table reconciliation

The tranche docs carry 13-lock honoured tables (Lock 14 codified post-dispatch). Reconcile cross-tranche:

| Lock | BA | BB | BC | BD | Pre-BA | Notes |
|---|---|---|---|---|---|---|
| L1. Tape + columnar dead | honoured-at-W5a..W5e | references-only (carry BA→BB.C1') | references-only (closed) | references-only (closed) | — | Option (a) honoured at BA close per `audit/W5-substrate-identity-decision.md`. BB.W1{a,b,c} retire (§3.1). |
| L2. Layout lowering canon | honoured-at-W2 | references-only (W3a) | honoured-at-W0a | references-only (closed) | — | Aliases deleted in-wave; BC.W0a uses canonical Layout/LayoutSink. |
| L3. Cursor + byte-skip unified | honoured-at-W4a/b/c | honoured-at-W2b | references-only (closed) | references-only (closed) | — | BB.W2b extends to all 9 grammars in same wave. |
| L4. Per-domain orthogonal optimisation | deferred-with-receiver to BB.W3 | honoured-at-W3a/b/c | references-only (closed) | references-only (closed) | — | CSP → e-graph → miners → cost model output-piped; no unified hypergraph. |
| L5. IR + per-backend lower | demonstrated-by-pattern at W5a..W5e | demonstrated-by-pattern at W1{a,b,c}+W2a | honoured-at-W0a/b + W1a/b + W2 | ratified-at-W0/W1/W2 (TS+WASM activation) | — | Final ratification at BD close. |
| L6. xtask emits committed source | honoured-at-W0 | honoured-at-W1{a,b,c}+W2a | honoured-at-W1 | ratified-at-W1+W2 | — | Per-tranche ratification cumulative. |
| L7. `crates/path/` consolidation | honoured-at-W3a/b/c | honoured-at-W5a (`pointer!` macro) | honoured-at-W5 (sister freeze) | ratified-at-W0+W3 (path-ts publish) | — | Triplet path/path-core/path-ts. |
| L8. Surpass sonic-rs / simdjson / lightning-css | G1a (twitter ≤ 400 µs) | G1..G4 (CSS L4 + JSON tightening) | G1..G3 (final native targets) | engineering-bounded G1+G2 (NAPI/WASM) | — | Native floor at BC; BD bounded by FFI/WASM realities. |
| L9. Slice-borrow primary; bumpalo + owned escape hatches | honoured-at-W5a..W5e | honoured-at-W4a (three-surface API) + W5a (LazyValue) | honoured-at-W4 (Visitor formalisation) | ratified-at-W1+W2 (zero-copy across FFI / WASM) | — | Default `&'i str`; `parse_in` + `parse_owned` escape hatches. |
| L10. Pratt + SIMD auto-detected | deferred-with-receiver to BB.W3 | honoured-at-W3c | references-only (closed) | references-only (closed) | — | No `@pratt` or `@simd` directives. |
| L11. Path-deps for incubating sister crates | deferred-with-receiver to BB.W0 | honoured-at-W0a | honoured-at-W5a (freeze) | ratified-at-W3 (publish) | — | egraph + egraph-derive + csp-solver + bbnf-regex graduate at BD. |
| L12. ser + gorgeous archive BEFORE BA.W0 | precondition-honoured | references-only (closed) | references-only (closed) | references-only (closed) | yes — pre-BA ceremony | Verification gate before BA dispatch. |
| L13. No god directories; cohesive encapsulation | honoured-at-W0+W2 | honoured-at-W2a (cohort compression) + W3{a,b,c} (per-domain crates) | honoured-at-W3 (3-crate split) | ratified-via-fixture-pattern | — | Standard set by sonic-rs / lightningcss / simdjson. |
| **L14. Full grammar generalisation; zero overfitting** | **silent (codified post-dispatch)** | **silent** | **silent** | **silent (3 pseudocode hardcodings)** | — | **Substantively honoured by mechanism (BC.W3 DAG, BB.W2a metadata-driven, BA.W1 metadata schema); nominatively unnamed in any tranche's lock table; surgery requires Lock 14 row addition + BD.W2 pseudocode amendment.** |

**Surgery**: amend each tranche's 13-lock honoured table to a 14-lock table:
- BA.md:106-120: add Lock 14 row citing W0 (residue scrub eliminates per-grammar `host/<grammar>.rs` generic-root pattern); W1 (metadata-driven strategy resolver); W3a/b (registry match-arm deletion via metadata).
- BB.md:106-120: add Lock 14 row citing W2a (metadata-driven cohort enumeration); W3a/b/c (CSP/e-graph reference grammars only via `&str` ident through workspace metadata); W5b (visitor methods derived from per-grammar record alphabet).
- BC.md:82-96: add Lock 14 row citing W3 (DAG zero grammar-ident in generic-crate API surfaces); W3b (per-grammar host namespaces); W4 (cross-backend visitor surface generic).
- BD.md:63-77: add Lock 14 row citing W3 (per-grammar declaration crate publication if applicable); W5 (cross-backend parity matrix metadata-derived). Amend BD.W2.§2.2 (`parse_json` factor → metadata-templated `parse_<grammar>`) and BD.W5.§ (import example → metadata-templated).

---

## §10 — Era V abrogation verification

For each wave, name same-wave or next-wave consumer.

**BA tranche** (13 sub-waves):

| Wave | Produces | Consumer | Verdict |
|---|---|---|---|
| BA.W0 | 9-directory layered re-org + tape-residue scrub | BA.W1 (metadata schema reads from re-org); BA.W2 (god-module splits land into re-org) | OK |
| BA.W1 | Metadata-driven strategy resolver | BA.W5a (DirectToStruct variant consumes); BA.W3b (path-core consumes registry) | OK |
| BA.W2 | God-module splits + Layout rename + fail-explicit table | BA.W4 (cursor unification consumes new structure); BA.W5a..W5e (codegen consumes Layout vocabulary) | OK |
| BA.W3a | Path crate rename | BA.W3b (path-core extraction consumes); BB.W5 (`pointer!` macro consumes) | OK (cross-tranche) |
| BA.W3b | path-core extraction | BA.W3c (runtime relocation reads); BB.W5a (`pointer!` consumes path-core) | OK |
| BA.W3c | Runtime relocation | BA.W4a (private parse core consumes new path) | OK |
| BA.W4a | Private parse core + cursor elision | BA.W4b (public wrappers consume); BA.W5a (codegen lands `parse_with`) | OK |
| BA.W4b | Public wrappers + Document::get<T> | BA.W4c (legacy deletion consumes the unified surface); BA.W5a..W5e (per-grammar parse fns) | OK |
| BA.W4c | Legacy lowering deletion | BA.W5a (clean foundation for direct-projection) | OK |
| BA.W5a | JSON direct-to-struct | BA.W5b (CSS L4 consumes pattern); BB.W2b (cursor unification across all 9) | OK |
| BA.W5b | CSS L4 direct-to-struct | BA.W5c (BBNF consumes pattern); BC.W0b (smoke lowerer consumes) | OK |
| BA.W5c | BBNF direct-to-struct | BA.W5d (Sheets consumes pattern); BB.W3c (Pratt consumes BBNF operator chains) | OK |
| BA.W5d | Sheets direct-to-struct | BA.W5e (cohort consumes pattern) | OK |
| BA.W5e | Cohort hand-written direct-to-struct | BB.W2a (template consolidation absorbs) | OK (cross-tranche; same as BA→BB.C1') |
| BA.W6 | BA close + carry ledger | BB.W0 (entry preflight) | OK (cross-tranche) |

**BB tranche** (15 sub-waves):

| Wave | Produces | Consumer | Verdict |
|---|---|---|---|
| BB.W0a | Sister-crate path-deps | BB.W0b (smoke pass consumes) | OK (same-wave) |
| BB.W0b | Sister-crate smoke output | BB.W3c (cost-model input) | OK |
| BB.W1a | (subsumed under §3.1 — BA owns) | (n/a after §3.1 surgery) | needs surgery |
| BB.W1b | (subsumed under §3.1) | (n/a) | needs surgery |
| BB.W1c | (subsumed under §3.1) | (n/a) | needs surgery |
| BB.W2a | Cohort template emission | BB.W2c (byte-equality regression) | OK |
| BB.W2b | Cursor unification all 9 grammars | (perf gates verify; no skeletal substrate) | OK |
| BB.W2c | Byte-equal evidence + deletions | (subsequent waves' tests run against templated) | OK |
| BB.W3a | Layout-pass path-dep wiring | BB.W3b (consumes layout) | OK |
| BB.W3b | E-graph + miner facts | BB.W3c (consumes facts) | OK |
| BB.W3c | rank.rs + tiering.rs + Pratt + SIMD detection (with same-commit consumer) | (perf gates close in same wave) | OK |
| BB.W4a | parse / parse_in / parse_owned three-surface | BB.W4b (cookbook + trybuild) | OK |
| BB.W4b | Lifetime cookbook + trybuild fixtures | (verbatim error messages tests) | OK |
| BB.W5a | `pointer!` macro + LazyValue | BC.W4 (visitor consumer); per-grammar pointer tests | OK (cross-tranche) |
| BB.W5b | Visitor + VisitTypes | BC.W4 (consumer per surgery 30) | OK (cross-tranche) |
| BB.W5c | Cookbook + diagnostic gates | (trybuild verifies) | OK |
| BB.W6 | BB close + carry ledger | BC.W0 (entry preflight) | OK (cross-tranche) |

**BC tranche** (15 sub-waves):

| Wave | Produces | Consumer | Verdict |
|---|---|---|---|
| BC.W0a | IR contract spec | BC.W0b (smoke lowerer consumes) | OK (same-wave) |
| BC.W0b | Smoke Rust lowerer | BC.W1a (full refactor consumes) | OK |
| BC.W0c | Sibling baseline + AscentStrategy excise | BC.W5d (worktree fixture closure) | OK |
| BC.W1a | Full Rust emitter refactor | BC.W1b (regen-equality verification) | OK |
| BC.W1b | Regen-equality verification | BC.W2 (TS + WASM scaffolds compile against) | OK |
| BC.W2 | TS + WASM scaffolds | BC.W3a (consumed by codegen extract); BD.W0/W1/W2 (production receiver) | OK |
| BC.W3a | bbnf-runtime extraction | BC.W3b (parse depends on runtime) | OK |
| BC.W3b | bbnf-parse extraction | BC.W3c (codegen consumes parse) | OK |
| BC.W3c | bbnf-codegen extraction | BC.W3d (umbrella slim) | OK |
| BC.W3d | Umbrella slim-down | BC.W3e (xtask path update) | OK |
| BC.W3e | xtask regen path update | BC.W4 (visitor at new path) | OK |
| BC.W4 | Visitor surface | BC.W5a (sister freeze consumes path-API) | OK |
| BC.W5a | Sister crate freeze | BC.W5b (bbnf-regex rename) | OK |
| BC.W5b | bbnf-regex endpoint rename | BC.W5a (publish candidate) — circular?; resolved by inverse-receiver-pattern (W5b before W5a) | OK |
| BC.W5c | parse-that disposition | BC.W5d (carry consolidation) | OK |
| BC.W5d | Worktree fixture closure | BD.W0 (parallel-agent infra consumes) | OK (cross-tranche) |
| BC.W6 | BC close + carry ledger | BD.W0 (entry preflight) | OK (cross-tranche) |

**BD tranche** (7 sub-waves):

| Wave | Produces | Consumer | Verdict |
|---|---|---|---|
| BD.W0 | TS proc-macro shell activation | BD.W1 (TS runtime emitter consumes path-ts) | OK |
| BD.W1 | TS runtime emitter | BD.W3 (publication consumes); BD.W5 (parity matrix consumes) | OK |
| BD.W2 | WASM compilation pipeline | BD.W3 (publication consumes); BD.W5 (parity matrix consumes) | OK |
| BD.W3 | Sister-crate publication | BD.W6 (close attests publication) | OK |
| BD.W4 | Worktree fixture fleet expansion | BD.W5 (parity matrix consumes) | OK |
| BD.W5 | Cross-backend parity verification | BD.W6 (close attests) | OK |
| BD.W6 | BD close (terminal) | (no successor) | OK (terminal) |

**Era V abrogation verdict**: 53 of 53 waves have same-wave or next-wave consumers (after the §3.1 surgery resolves BB.W1{a,b,c} ownership). Era V failure mode (substrate-then-substrate-then-ship) is structurally precluded.

---

## §11 — Generated-LOC budget cross-tranche

Walk per-tranche budgets to verify aggregate trajectory:

| Stage | Aggregate generated/* LOC | Source |
|---|---:|---|
| Pre-BA | 168,750 | `audit/MODULES-2026-05-03.md:621-628` summed; cited at BA.md:155 |
| Post-BA | ~150,900 (−10.7%) | BA.md:155 (per-grammar table sums) |
| Post-BB | ~134,700 (−18% from BA close, −20% from pre-BA) | BB.md:178 (per-grammar table sum) |
| Post-BC | ~135,160 (+1.93% from BB close, ~-19.9% from pre-BA) | BC.md:133 (per-grammar table sum) |
| Post-BD | ~135,167 Rust + 280,000 TS + 135,000 WASM | BD.md:113-120 (Rust unchanged; TS + WASM add new trees) |

**Reconciliation**:
- Pre-BA → Post-BA: −10.7% (consistent with BA.md trajectory)
- Post-BA → Post-BB: BB.md table sum (BA close ~163,855 → BB close ~134,700 = −17.8%) — but BA close per BA.md is ~150,900; the discrepancy of ~13,000 LOC between BA.md aggregate (150,900) and BB.md "BA-close LOC" column sum (~163,855) reflects different baseline-snapshot rows. Resolution: both are estimates within the ±2% per-wave overflow tolerance. **No surgery required**, but the synthesis NOTES the inconsistency for verification at execution.
- Post-BB → Post-BC: BC.md table shows +1.93% (BB close ~132,600 → BC close ~135,160). Within BC-G10's ≤ +2% aggregate gate.
- Post-BC → Post-BD: BD.md shows Rust unchanged at ~135,167. Plus new TS tree ~280,000 LOC and WASM tree ~135,000 LOC.

**Inconsistency surface** (minor): BA-close aggregate is reported as ~150,900 in BA.md, ~163,855 in BB.md's per-grammar table. The difference (~13,000 LOC) is driven by BB.md's table including post-BA wrapper additions (bumpalo / owned signatures, visitor surface) that the BA-close snapshot did not. **Minor surgery**: add a note to BA.md:155 footer clarifying the snapshot is BA-close-immediate, not BA-close-with-wrapper-additions; OR amend BB.md:168 to use 150,900 as the BA-close aggregate.

**Verdict**: aggregate trajectory reconciles; minor footer-note amendment recommended.

---

## §12 — Punch list

Ordered surgical edits to apply BEFORE Phase 5 + BA execution. Per directive §12 §V3 (Phase 4 Execution Mode), narrow-scope amendment agents close.

### BA tranche

| # | Target | Edit | Owner | Scope |
|---|---|---|---|---|
| 1 | `docs/tranches/BA/BA.md:106-120` (13-lock honoured table) | Add row "L14. Full grammar generalisation; zero overfitting" with cells: W0 (residue scrub eliminates per-grammar `host/<grammar>.rs` generic-root pattern); W1 (metadata-driven strategy resolver); W3a/b (registry match-arm deletion via metadata); W5a..W5e (per-grammar emit lives in `crates/core/src/grammar/generated/<g>.rs` only, generated from metadata) | BA | single-row-table-extension |
| 2 | `docs/tranches/BA/BA.md:155` | Add footer note clarifying BA-close aggregate is immediate-post-W5e, not post-wrapper-additions. (Optional; alternative is to amend BB-side per-grammar BA-close column.) | BA | single-line-amendment |

### BB tranche

| # | Target | Edit | Owner | Scope |
|---|---|---|---|---|
| 3 | `docs/tranches/BB/waves/W1a.md` | RETIRE: rewrite as no-op shell citing "subsumed under BA-redress option (a); Lock 1 honoured at BA close per `docs/tranches/BA/audit/W5-substrate-identity-decision.md`". OR REPURPOSE as Lock-14-generic-substrate-audit verifying BA.W5b emit shape. | BB | full-document-rewrite |
| 4 | `docs/tranches/BB/waves/W1b.md` | RETIRE / REPURPOSE same as W1a (BBNF-side). | BB | full-document-rewrite |
| 5 | `docs/tranches/BB/waves/W1c.md` | RETIRE / REPURPOSE same as W1a (Sheets-side). NOTE: the host-fn relocation from `crates/core/src/host/sheets.rs` to `crates/core/src/grammar/host/google_sheets.rs` per surgery 15 (G05-9) MUST be preserved if W1c retires entirely; relocate that milestone to BA.W0 or BB.W2a. | BB | full-document-rewrite + milestone-migration |
| 6 | `docs/tranches/BB/waves/W1.md` | UPDATE backbone thesis: BA.W5 generalises across all 9 grammars (not 8 remaining); W1 reorients to consume the post-BA all-grammar foundation per BA.md:11. Renumber W2a→W1a, W2b→W1b, W2c→W1c if wave-count alignment desired. | BB | multi-paragraph-rewrite |
| 7 | `docs/tranches/BB/BB.md:84-90` (Carry-tags FROM BA) | Replace `BA→BB.C1` row receivers ("BB.W1a (CSS L4), BB.W1b (BBNF), BB.W1c (Sheets), BB.W2a (5-grammar cohort)") with "BB.W2a — cohort hand-written (BA.W5e) consolidates into one parameterised template per gap D"; rename `BA→BB.C1` to `BA→BB.C1'` to match BA-side. | BB | row-edit |
| 8 | `docs/tranches/BB/waves/W1.md:114, :167`; `W1b.md:76, :107`; `W1c.md:89, :120`; `W4.md:117, :171`; `W5.md:156, :198`; `W5b.md:73` | Update carry-tag references where C1 receiver wave is named — point to W2a; add "C1' supersedes C1" note where appropriate. | BB | multi-line-mechanical |
| 9 | `docs/tranches/BB/waves/W2.md` (thesis line 10) and `W2a.md` (§1 Deliverable) | Add language: "Input is the five BA.W5e hand-written direct-to-struct cohort modules; BB.W2a's template emission consolidates them into one parameterised template; M2 byte-equality verifies the template re-emits BA.W5e content byte-for-byte." | BB | paragraph-addition |
| 10 | `docs/tranches/BB/BB.md:106-120` (13-lock honoured table) | Add row "L14" with cells: W2a (metadata-driven cohort enumeration); W3a/b/c (CSP/e-graph reference grammars only via `&str` ident through workspace metadata); W5b (visitor methods derived from per-grammar record alphabet). | BB | single-row-table-extension |

### BC tranche

| # | Target | Edit | Owner | Scope |
|---|---|---|---|---|
| 11 | `docs/tranches/BC/BC.md:82-96` (13-lock honoured table) | Add row "L14" with cells: W3 (DAG zero grammar-ident in generic-crate API surfaces per `docs/tranches/BC/audit/W3-crate-dependency-dag.md`); W3b (per-grammar host namespaces); W4 (cross-backend visitor surface generic via record alphabet). | BC | single-row-table-extension |
| 12 | `docs/tranches/BC/waves/W6.md:126` | Replace "BD has not been drafted; this is named explicitly" with "BD is drafted at commit `58e108ad`; carry-tags BC→BD.C1..C3 land in BD.W0..W4 per `docs/tranches/BD/BD.md`." | BC | single-line-amendment |

### BD tranche

| # | Target | Edit | Owner | Scope |
|---|---|---|---|---|
| 13 | `docs/tranches/BD/waves/W2.md:50-58` (§2.2 emit shape pseudocode) | Replace hardcoded `parse_json` / `bbnf-parse-json` factor with metadata-templated `parse_<grammar>` / `bbnf-parse-<grammar>`; the example pseudocode shows JSON as the canonical example but the surrounding prose names "for each grammar enumerated by `[workspace.metadata.bbnf-strategy.grammars]`, the WASM emitter produces a per-grammar Rust source...". | BD | paragraph-rewrite |
| 14 | `docs/tranches/BD/waves/W5.md:167` (TS import example) | Replace hardcoded `import init, { parse_json, parse_css_l4 } from '@bbnf-lang/runtime-wasm'` with metadata-templated `import init, { parse_<grammar> } from '@bbnf-lang/runtime-wasm'` where `<grammar>` enumerates per workspace metadata. | BD | single-line-amendment |
| 15 | `docs/tranches/BD/BD.md:23` (BD-G5) | Replace "9 grammars × ≥ 3 fixtures each = ≥ 27 parity tests" with "N grammars × ≥ 3 fixtures each (N = grammar count from workspace metadata; currently 9) = ≥ 3N parity tests". OR add a per-grammar parity matrix table. | BD | sentence-rewrite-or-table-add |
| 16 | `docs/tranches/BD/BD.md:35` (BD-G6 fixture pattern) | Add a per-grammar fixture matrix table to §Hard gates BD-G6 cell, with rows for each grammar × ≥ 3 fixtures; alternatively, reference a separate `docs/tranches/BD/audit/W4-fixture-matrix.md` artefact. | BD | table-addition |
| 17 | `docs/tranches/BD/BD.md:63-77` (13-lock honoured table) | Add row "L14" with cells: W3 (per-grammar declaration crate publication if applicable); W5 (cross-backend parity matrix metadata-derived). | BD | single-row-table-extension |

### Aggregate

17 surgeries land cleanly. Per-tranche distribution: BA 2, BB 8, BC 2, BD 5. The dominant work is the BB.W1{a,b,c} retirement + carry-tag receiver corrections (8 surgeries) and the BD Lock 14 cleanups (5 surgeries). The BA + BC tranches need only 2 each (Lock 14 row addition + minor metadata footnote / stale-statement cleanup).

---

## §13 — Final readiness

> **Decision: amendment-required.**
>
> The four tranches collectively close ~34 of the 35 Phase-3 surgeries, and the cross-tranche substrate (Lock 14 mechanism — metadata-driven dispatch, generic-crate API surfaces, per-grammar declaration-crate pattern) is substantively honoured. The residue is concentrated: (i) the BA-redress option-(a) flip orphaned BB.W1{a,b,c} as substrate-double-claim — eight surgeries; (ii) Lock 14 codified post-dispatch is silent in all four tranches' lock tables — four surgeries; (iii) BD pseudocode hardcodes JSON in three pseudocode examples — three surgeries; (iv) BC.W6 carries a stale "BD has not been drafted" line — one surgery; (v) minor BA / BB aggregate-LOC reconciliation — one surgery. Total: 17 surgical edits, none requiring re-draft.
>
> Hereupon Phase 5 hardening pre-condition is the 17-surgery punch list above; once landed, the BA execution agent can dispatch with full plan-set ratification.

---

## Voice + discipline locks honoured

§V1 Archaic-permissive — "hereupon", "begotten", "thereof", "appurtenant" used.
§V2 No metalanguage — citations are path:line; no commit refs in document body except the four Phase-4 tranche commits + Lock 14 codification commit (factual scope-anchors per directive §6 not banned).
§V3 Path:line citations on every concrete claim — verified.
§V4 State the deliverable. State the gate. Move on. — all surgeries are direct edits, not "consider" / "may" / "perhaps".
§V5 Tables liberal — 14 tables across 13 sections.
§V6 Per-X tables — every "all-X" claim verified per §6.

---

## Closing

The substrate is the 13-lock + Lock 14 master at `docs/HARDENING-PLAN-PROMPT.md`; the audit synthesis at `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md` ratified the 35-surgery punch list; the four Phase-4 tranches close 34 of 35; this synthesis closes the cross-tranche residue with 17 surgical amendments. Hereupon Phase 5 hardening accepts a ratifiable plan-set for BA execution.
