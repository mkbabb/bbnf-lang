# Handoff — bbnf-lang Greenfield Restart

Date: 2026-05-04
Status: amendment-required, executable; awaiting Wave 1 dispatch.
Audience: the next agent or human picking up this work.

This document is the single source of truth for orienting cold. Read it end-to-end before reading anything else; it tells you what the project is, where the work has been, where it is now, and what the next move is. Every claim cites a file path so you can verify.

---

## §1 — What this project is

bbnf-lang is a **grammar-driven, multi-backend parser generator** producing SOTA-class typed parsers from `.bbnf` grammar files. The user-facing API is familiar (sonic-rs lazy-value idioms; lightning-css visitor idioms; jq-style path access); the internals are the apotheosis (CSP-backed bidirectional type system; e-graph-driven rewrite engine; shape miner that auto-detects Pratt and SIMD opportunities; cost model unified across the parser and the regex engine; IR + per-backend lowerer).

The anthem: **everything is grammar-derived.** Every grammar plugs into the fleet via two declarative surfaces — (a) a grammar source file `<name>.bbnf` and (b) a workspace metadata block `[workspace.metadata.bbnf.grammars.<name>]` in the root `Cargo.toml`. Adding a 10th grammar requires nothing else: no new crate, no per-grammar match arm in any generic crate, no per-grammar hand-written runtime module. This is Lock 14 — full grammar generalisation; zero overfitting — and it is the single most consequential discipline of the restart.

Read in order:

1. `restart/README.md` — gestalt synthesis (446 lines). The architectural commitments, the BBNF extensions, the optimization apotheosis, the type system, the value API, the SOTA synthesis, the 14 locks, the process.
2. `restart/locks/14-LOCKS.md` — the 14 architectural commitments (249 lines). Lock 1 was reframed 2026-05-04: tape is the substrate when properly implemented; the prior wholesale retirement of the name was an over-correction.
3. `docs/precepts/instructions/STYLE.md` — voice + discipline (governs all writing).
4. `docs/precepts/instructions/LESSONS-LEARNED.md` — failure-mode anatomy (governs all decisions).

---

## §2 — Where the work has been

The bbnf-lang corpus carries roughly **2,662 commits** across six eras. Two prior restart attempts compounded into contrivance before the current attempt:

- **Eras I–VI** — the operational corpus: 9 grammars, ~17K LOC core crate, ~168K LOC generated tree, multiple architectural pivots
- **Phase-1 / Phase-3 audit** — the BA-restart plan-set drafted at `docs/tranches/{BA, BB, BC, BD}/` (~18,200 lines combined; never executed)
- **Phase-4 spec-depth re-draft** — sub-waved BA-BD with audit artefacts; ~2,500-5,500 lines per tranche
- **First restart attempt** — archived at `restart-archive-2026-05-04/`; produced ~32,000 lines of pass syntheses + master plan + 4 stage-1 hardening reports + 4 stage-2 hardening reports before the user flagged compounded contrivance
- **Re-restart (current)** — single-round suite at `restart/`; the 35-question interrogation settled architecture; the four-target hardening returned AMENDMENT-REQUIRED

### Settled decisions from the 35-question interrogation

The full interrogation document is archived at `restart-archive-2026-05-04/INTERROGATION-2026-05-04.md`. The user's settled answers govern. Critical highlights:

- **24-crate workspace** (Q1 balanced split). The `bbnf-` prefix is **dropped from internal substrate crates** (passes / ir / runtime / codegen / etc.) and **retained only on user-facing crates** (`bbnf` aggregator, `bbnf-cli`, `bbnf-language-server`, `bbnf-bench`).
- **VM kept** (Q7) — CSP/egraph rule oracle + debug runtime + incremental-parse substrate.
- **Two IRs** (Q9): Grammar IR + Backend IR. Side tables (CSP-typed annotations, cost annotations, shape hints) live keyed by Grammar IR node IDs.
- **Hybrid value type** (Q10): leaf rules → slice/scalar wrappers; Seq → struct; Alt → enum; Repeat → Vec; Optional → Option. Deep enum support.
- **Tape + direct-to-struct UNION** is the substrate (Q25). The 2,000-commit prior failure was *implementation*, not *naming*. Tape lives at `runtime/src/tape/`; typed values borrow `&'i Tape<'i>` + index. Same-wave consumer wiring at Tranche F precludes Era V failure mode.
- **Hindley-Milner inference + bidirectional check/synth in Pierce-Turner style + CSP-backed unification** for the type system (Q20+Q23). Generic rules V1 (Q22).
- **CSP main driver + e-graph for saturation; bridge architecture between them** (Q18). Not a fused type — explicit interface methods + bidirectional name map.
- **Pratt + SIMD auto-detected** from grammar shape; no `@pratt` / `@simd` directives (Lock 10).
- **Path DSL: dual macro** (Q24): `pointer!(Json, ["a","b",0])` (sonic-rs idiom; compile-time key/index path) + `select!(Css, "rule > declaration[property=color]")` (lightning-css/treesitter selector DSL). Both compile-time proc-macros at `path` + `path-core` + `path-ts`. Public macro is `pointer!`, never `path!`.
- **First-class SIMD on all platforms** (Q29): NEON + AVX2 + AVX-512 + WASM-SIMD + scalar fallback.
- **Incremental parsing**: opt-in feature for batch parsers; always-on for LSP (Q30).
- **All 16 SOTA projects deep-dived** (Q31): sonic-rs, simdjson, lightning-css, treesitter, rust-analyzer, swc, chumsky, lalrpop, pest, nom, rowan, logos, regex-automata, egg, z3, antlr4, megaparsec.
- **Single-round hardening, no Stage-2 or Stage-3** (Q34+Q35): the prior contrivance is dead.

### Settled BBNF language extensions

Per `restart/README.md` §5 + the user's 2026-05-04 reframe:

| Extension | Status | Notes |
|---|---|---|
| Lookbehind `\|<` | V1 | Bounded-width; rolling window; types as constraint vs capture (lookbehind contributes nothing to value, everything to constraint) |
| Rich regex with first-class modern Unicode | V1 | At regex layer (`parse-that/regex/`), not grammar layer. Latest Unicode standard; `\p{L}`, `\p{XID_Start}`, set algebra inside `[…]`, grapheme awareness, normalisation modifiers, named regex classes |
| `@host fn` directive | V1 | Block-bodied: `@host fn parse_hex_color(s: regex(...)) -> Color { Color::Rgb(parse_hex_pair(s[1..3]), …) }`. Closure semantics; bidirectional inference |
| Multi-function chaining | V1 | `-> f1 -> f2 -> f3`; type flows through stages; CSP backs constraint collection |
| Generic rules | V1 | `Object<V> = "{" pair<V> ("," pair<V>)* "}"`. CSP propagates type variables; codegen monomorphises |
| `@error(skip \| recover \| halt)` directive | V1 | Auto-inferred default via shape mining; directive overrides |
| `@layout(struct \| enum \| tuple \| slice)` hint | V1 | Optional type-layout override |
| Rewrite mode | dropped | The Visitor surface + cost-model peephole covers the use case |
| Grammar-level Unicode char-class algebra | dropped | Regex-level Unicode covers V1 grammars; grammar-level surface premature |
| `@pratt` / `@simd` directives | forbidden by Lock 10 | Auto-detected from grammar shape only |

The ffuzzy three-primitive proposal (`docs/ffuzzy.md`) reduced to: lookbehind kept; rewrite mode dropped; Unicode at regex layer.

### Lock 1 reframe (2026-05-04)

The prior restart's wholesale retirement of the tape name was an over-correction against the implementation failure (orthogonal codepaths; OpenFrame parallel substrate; the 86.07% Vec<OpenFrame>::clone samply pathology; substrate-first / consumer-later Era V failure mode). The user has confirmed: **tape is fine if implemented properly.** Lock 1 in `restart/locks/14-LOCKS.md:34` codifies the reframe: spirit (no parallel substrate; no orthogonal codepath; no Vec<OpenFrame>::clone pathology) holds; the no-rename clause is amended.

The greenfield's tape:
- Lives at `runtime/src/tape/`
- Is contiguous parsed-token-stream-with-payload-arena (simdjson structural insight)
- Typed values borrow into via `&'i Tape<'i>` + index (sonic-rs LazyValue idiom)
- Per-grammar runtime modules template-emit at `runtime/src/grammars/<name>/{generated.rs, parser.rs, host.rs}` and emit accessors
- One materialisation surface; one Visitor pattern; no parallel substrate
- Same-wave consumer wiring at Tranche F retires OpenFrame across all 9 grammars in a single architectural movement

---

## §3 — Where the work is now

Six prompts compose the single-round suite at `restart/prompts/`:

| Prompt | Lines | Purpose |
|---|---:|---|
| `PASS-1-SUBSTRATE.md` | 97 | Bottom layer: source / grammar / IR / passes / VM / host / cost-model / egraph / csp-solver / type system / BBNF extensions / error vocabulary |
| `PASS-2-CODEGEN.md` | 83 | Middle layer: Backend IR / Rust + WASM lowerers / runtime template / SIMD scanner kernels / Pratt + SIMD auto-detection |
| `PASS-3-RUNTIME.md` | 85 | Top layer: bbnf aggregator / value API / path + select DSLs / visitor surface / tape + direct-to-struct union / error recovery / incremental parsing / LSP / CLI |
| `SYNTHESIS.md` | 160 | Consolidator: produces ARCHITECTURE.md + MIGRATION.md + MASTER-PLAN.md |
| `HARDENING.md` | 177 | Per-target audit: 9 lanes; KEEP/REINVENT/DISCARD verdicts; Pro/Con/Explication/Challenge per-item discipline |
| `HARDENING-ORCHESTRATOR.md` | 195 | Four-target sequencer (Phase 1 parallel: PASS-1/2/3; Phase 2 serial: MASTER-PLAN; Phase 3 consolidate) |
| `AMENDMENT-DISPATCH.md` | 211 | Four-wave amendment orchestrator with verify-then-patch discipline (THE ACTIVE NEXT STEP) |

The pipeline executed end-to-end:

1. **PASS-1, PASS-2, PASS-3** — three pass orchestrators each dispatched 6 sub-agents; outputs at `restart/audit/pass-{1-substrate, 2-codegen, 3-runtime}/`. Sub-agent reports are individual files; per-PASS synthesis is `PASS-{1,2,3}.md`. Total: 18 sub-agent reports + 3 PASS syntheses.
2. **SYNTHESIS** — produced `restart/ARCHITECTURE.md` (1,259 lines), `restart/MIGRATION.md` (740 lines), `restart/MASTER-PLAN.md` (727 lines). The trio is the authoritative architectural specification + per-file disposition + tranche execution plan.
3. **HARDENING** — four-target hardening per `HARDENING-ORCHESTRATOR.md`. Reports at `restart/audit/hardening/HARDENING-{PASS-1, PASS-2, PASS-3, MASTER-PLAN}.md` + consolidated at `HARDENING-CONSOLIDATED.md`.
4. **Multi-reviewer audit** — four reviewers (A consolidation-fidelity, B architectural-integrity, C Lock-14-greenfield-discipline, D punch-list-executability) audited the hardening cohort. Reports at `restart/audit/hardening/REVIEW-{A,B,C,D}-*.md`.

### The four-target hardening verdict

`restart/audit/hardening/HARDENING-CONSOLIDATED.md` § verdict:

| Target | KEEP | REINVENT | DISCARD | Punch list |
|---|---:|---:|---:|---:|
| PASS-1 | 30 | 29 | 3 | 19 |
| PASS-2 | 38 | 20 | 1 | 9 |
| PASS-3 | 19 | 47 | 0 | 12 |
| MASTER-PLAN | 30 | 31 | 4 | 16 |
| **Cumulative** | **117** | **127** | **8** | **56 → 47 deduplicated** |

Final decision: **AMENDMENT-REQUIRED.** Not RE-DRAFT. The greenfield thesis survives; 13 cross-target conflicts are surgical, not structural; the architecture's substrate identity (tape + direct union, two IRs, path triplet, layout-lowering canon, 24-crate workspace, A-J tranches, 14 locks) is internally coherent and survives.

### The 13 cross-target conflicts (most consequential first)

1. **Backend IR ownership** — PASS-2 placed BIR in `codegen/`; MASTER-PLAN treats as lowerer contract; README puts in `ir/`. **Resolution**: BIR → `ir/src/backend_ir/`. Codegen owns lowerers, snapshots, adapters, import-deny gates.
2. **Public macro name** — PASS-3 exposed `path!`; README + MASTER-PLAN use `pointer!`. **Resolution**: rename to `pointer!`.
3. **Path crate names** — PASS-3 carried `bbnf-path*`; MASTER-PLAN uses unprefixed `path` / `path-core` / `path-ts`. **Resolution**: drop `bbnf-` prefix.
4. **Layout terminology drift** — `TypeFacts` survives as peer side-table at ARCHITECTURE §7.3 / MASTER-PLAN C.W1 / PASS-1 §3. **Resolution**: `passes::layout` + `LayoutFacts` are public; `TypeFacts` is internal-subroutine-only. Lock 2 in `14-LOCKS.md:36` carries a stale `bbnf-` prefix at `bbnf-ir/src/passes/layout/`.
5. **Lock 14 yaml onboarding** — MASTER-PLAN admitted fixture allowance as a third surface. **Resolution**: two surfaces only (grammar source + metadata block); fixtures are post-onboarding parity.
6. **SOTA close routing escape** — MASTER-PLAN allowed unmet SOTA gates to "route" at close. **Resolution**: DELETE the escape; missed SOTA opens a named amendment + blocks close. (Reviewer C says this is **already deleted post-amendment**.)
7. **OpenFrame residue** — PASS-1 permitted "OpenFrame-like internal builders"; PASS-2 replaced with TapeBuilder. **Resolution**: delete OpenFrame preservation language. (Reviewer C says **honoured post-amendment** — every match classifies as pathology / retiral / gate / invariant.)
8. **B/C and C/E/H sequencing** — punches #40 + #41. (Reviewer C + D say **already absorbed** in MASTER-PLAN; Reviewer B audited a stale snapshot.)
9. **Block-bodied @host fn** — PASS-1's formal grammar erased the body. Resolution: full block production.
10. **Lookbehind surface canonicalisation** — `|<` is grammar-level; `(?<=...)` is regex-only. Resolution: spec the operator + finite-width legality + diagnostic.
11. **`@recover` vs `@error(recover)`** — PASS-3 introduced `@recover` separately. Resolution: fold into `@error(recover)`.
12. **`bbnf` aggregator child-count** — too wide for Lock 13. Resolution: 4-10 children.
13. **Carry receiver/blocker/gate triples** — PASS hand-offs lacked the three required columns. Resolution: every carry has all three.

### The four-reviewer cohort

| Reviewer | Verdict | Faults | Commit |
|---|---|---:|---|
| A — consolidation fidelity | RATIFIED with citation-precision drift | 6 imprecise citations (4-15 lines off; non-blocking) | `dbbf1e7f` |
| B — architectural integrity | REQUIRES STRUCTURAL AMENDMENT | 3 (`bbnf/src/` divergence; layout drift; B+C says sequencing not absorbed) | `ffe212a4` |
| C — Lock 14 + greenfield discipline | HONOURED with two narrow additions | 2 (per-X 10×9 table; declaration-crate 8-field fence) | `c262813d` |
| D — punch-list executability | EXECUTABLE WITH ROUTING REPAIRS | 1 hard mis-routing (#12 fixture separation); 2 soft; 7 partial-pre-fills not flagged | `d4d69d17` |

### Reviewer reconciliations baked into the amendment dispatch

- **B vs C on sequencing (#40 + #41)**: C + D correct; sequencing absorbed. B audited stale snapshot OR was reading PASS-2/3 (which legitimately don't carry MASTER-PLAN's sequencing). Wave 2 verifies + cleans residue text only.
- **A's 6 imprecise citations**: non-blocking; optional precision pass during Wave 4 hardening rerun.
- **D's hard mis-routing #12**: punch #12 (fixture separation) cites `ARCHITECTURE.md:1132-1138/1151-1162` (incorrect — that range is the SOTA gate / Generated LOC budget block). Actual fixture surface lives at `PASS-3.md:272-289`. Re-routed.
- **D's soft mis-routings #6 + #9**: co-routed PASS-1/PASS-3 primary + SYNTHESIS reference.

### Pre-fills (verify-then-patch discipline)

Reviewer D §8.3 surfaced **seven punch-list items have substantial pre-existing surgery already landed in the SYNTHESIS trio**. Naive amendment dispatch would re-author them — wasting 4-6 hours and risking regressions from re-write churn. Each amendment agent's dispatch prompt classifies items as **full-author / patch-delta / verify-only-stub** at compose time:

| Item | Pre-fill state | Surgery type |
|---|---|---|
| 15 declaration-crate fence | partial: ARCHITECTURE §5.6 has 5 fields, PASS-1 §2 has 6, need 8 | patch-delta |
| 21 Lock 13 verification table | likely full | verify + delta |
| 29 SOTA table | likely partial | patch-delta |
| 30 final SOTA escape deletion | full per Reviewer C | verify-only-stub |
| 31 early H thresholds | likely partial | patch-delta |
| 40 B/C sequencing repair | full per Reviewer C ("C.W2 ShapeFacts fixture in C with explicit B integration gap recording") | verify-only-stub |
| 41 C/E/H consumer repair | full per Reviewer C ("C.W3 RecognizerFacts feed E-owned BIR snapshots not placeholder hints" + "C.W5 CostFacts feed E.W1 Backend IR builder") | verify-only-stub |
| 44 archive citation | full per Reviewer D ("per Lock 12") | verify-only-stub |

---

## §4 — Where the next move is

The active next step is **dispatching `restart/prompts/AMENDMENT-DISPATCH.md`** (211 lines; commit `e61a047f`). Four waves:

| Wave | Hours | Scope | Parallelism |
|---|---|---|---|
| 1 | 3-4 | PASS-1 + PASS-2 BIR foundations (items 1, 3, 4 + import-deny gate) | serial (shared write surface) |
| 2 | 3-5 | PASS-1 cont. + PASS-2 cont. + PASS-3 + SYNTHESIS amendments | 4 parallel |
| 3 | 1-2 | Reviewer-C 10×9 table + 8-field fence + Reviewer-B `bbnf/src/` canonical layout + Lock 2 stale prefix | single SYNTHESIS agent |
| 4 | 1-2 | Hardening orchestrator rerun with tightened gate-rerun checklist (Reviewer D §6: 8 of 16 commands need post-condition tightening) | single hardening orchestrator |

**Total: 6.5-9 hr wall (parallel) / 13-19 hr serial. Mean dispatch confidence per Reviewer D: 83%.**

The amendment dispatch's three baked-in disciplines:

1. **Verify-then-patch** — every dispatch prompt classifies each item as full-author / patch-delta / verify-only-stub before any edits; pre-fills enumerated with verification commands
2. **Reviewer reconciliation directives** — B vs C sequencing disagreement resolved (C correct); A's citation drift non-blocking; D's hard mis-routing #12 re-routed; D's soft mis-routings #6 + #9 co-routed
3. **Wave parallelism** — 4 parallel where targets don't share write paths; serial where they do

### Wave 4 decision rules

- **READY** verdict → user advances to per-tranche full-spec drafting (the fresh tranche set A-J at `restart/MASTER-PLAN.md` §5; legacy inheritance per `restart/inheritance/INDEX.md`)
- **AMENDMENT-REQUIRED-RERUN** verdict → narrow-scope follow-up + re-rerun (autonomous; collapses to single-agent fix-and-rerun)
- **RE-DRAFT** verdict → escalate to user (none of 10 thresholds at HARDENING-CONSOLIDATED §5 currently met; would require post-amendment divergence)

---

## §5 — File map

### Authoritative architectural specification (the trio)

| File | Lines | Role |
|---|---:|---|
| `restart/ARCHITECTURE.md` | 1,259 | Master architectural spec: workspace, per-crate src/, dependency DAG, Cargo.toml schema, IR contract, BBNF formal spec, Lock 13 verification, future-grammar onboarding test |
| `restart/MIGRATION.md` | 740 | Per-file disposition for current `crates/` (~834 .rs files): KEEP-OUTRIGHT / KEEP-MODIFY / ABROGATE-DELETE / ABROGATE-MOVE / ABROGATE-REPLACE; per-tranche file movement; commit-chain disposition; archive ceremony; LOC trajectory |
| `restart/MASTER-PLAN.md` | 727 | Authoritative tranche set A-J + execution plan + hard gates + carry-tags + 14-lock honoured table + risks + LOC trajectory |

### Pass syntheses + sub-agent reports (the inputs to the trio)

| Path | Notes |
|---|---|
| `restart/audit/pass-1-substrate/PASS-1.md` (235 lines) | Substrate synthesis |
| `restart/audit/pass-1-substrate/agent-{1-6}-*.md` | 6 sub-agent reports (~60 lines each) |
| `restart/audit/pass-2-codegen/PASS-2.md` (467 lines) | Codegen synthesis |
| `restart/audit/pass-2-codegen/agent-{1-6}-*.md` | 6 sub-agent reports (~70-110 lines each) |
| `restart/audit/pass-3-runtime/PASS-3.md` (382 lines) | Runtime synthesis |
| `restart/audit/pass-3-runtime/agent-{1-6}-*.md` | 6 sub-agent reports (~80-200 lines each) |

### Hardening reports

| Path | Lines | Verdict |
|---|---:|---|
| `restart/audit/hardening/HARDENING-PASS-1.md` | 206 | AMENDMENT-REQUIRED |
| `restart/audit/hardening/HARDENING-PASS-2.md` | 294 | AMENDMENT-REQUIRED |
| `restart/audit/hardening/HARDENING-PASS-3.md` | 219 | AMENDMENT-REQUIRED |
| `restart/audit/hardening/HARDENING-MASTER-PLAN.md` | 227 | AMENDMENT-REQUIRED |
| `restart/audit/hardening/HARDENING-CONSOLIDATED.md` | 619 | AMENDMENT-REQUIRED — 47-item punch list, routing matrix, gate rerun checklist |

### Multi-reviewer audit reports

| Path | Verdict | Commit |
|---|---|---|
| `restart/audit/hardening/REVIEW-A-CONSOLIDATION-FIDELITY.md` | RATIFIED with citation-precision drift | `dbbf1e7f` |
| `restart/audit/hardening/REVIEW-B-ARCHITECTURAL-INTEGRITY.md` | REQUIRES STRUCTURAL AMENDMENT | `ffe212a4` |
| `restart/audit/hardening/REVIEW-C-LOCK-14-GREENFIELD.md` | HONOURED with two narrow additions | `c262813d` |
| `restart/audit/hardening/REVIEW-D-PUNCH-LIST-EXECUTABILITY.md` | EXECUTABLE WITH ROUTING REPAIRS | `d4d69d17` |

### Reference corpora (read on demand; carried forward from prior restart)

| Path | Purpose |
|---|---|
| `restart/corpora/CENSUS.md` | Per-file kill-list (pathologies surfaced); the empirical signal that drove Lock 14 codification |
| `restart/corpora/MODULES.md` | Per-file fates from prior audit + 17-step pipeline |
| `restart/corpora/RESTART-SKETCH.md` | JSON parse trace + the 86.07% Vec<OpenFrame>::clone pathology |
| `restart/corpora/SOTA.md` | sonic-rs / simdjson / lightning-css benchmark corpus + 16-project SOTA inventory |

### Inheritance ledger

| Path | Purpose |
|---|---|
| `restart/inheritance/INDEX.md` (73 lines) | BA-BD legacy plan-set survival map per new tranche A-J; what survives, what dies, what re-anchors |

### Legacy plan-set (the inheritance source)

| Path | Lines | Status |
|---|---:|---|
| `docs/tranches/BA/` | ~3,850 | Drafted Phase-2/Phase-4; never executed; archives at Tranche-A.W0 to `docs/tranches/archive/legacy-Y-BD/BA/` |
| `docs/tranches/BB/` | ~5,578 | same |
| `docs/tranches/BC/` | ~4,458 | same |
| `docs/tranches/BD/` | ~4,329 | same |
| **Total** | **~18,200** | |

### Prior restart attempt (archived; research signal only)

`restart-archive-2026-05-04/` — prior compounded restart's full corpus (~32,000 lines). Includes the 35-question interrogation document at `restart-archive-2026-05-04/INTERROGATION-2026-05-04.md` + full PASS / SYNTHESIS / Stage-1 + Stage-2 hardening reports. The greenfield uses these as *research signal*; do not relitigate.

### The bbnf-lang source tree (read for inheritance signal at amendment-dispatch time)

| Path | Notes |
|---|---|
| `crates/core/` | Kitchen-sink core (~17K LOC) — fractures into `bbnf-parse`, `bbnf-codegen`, `bbnf-runtime`, etc. per ARCHITECTURE §1 |
| `crates/ir/` | IR types + passes + registry — fractures into `ir`, `passes`, `vm` per ARCHITECTURE §1 |
| `crates/{egraph, csp-solver, simd-scan, parse-that, bbnf-regex}/` | Sister optimiser crates — path-deps until stable per Lock 11 |
| `crates/{ser, gorgeous}/` | **Archive ceremony precondition** for Tranche A.W0 per Lock 12 |
| `crates/{analysis, lsp}/` | Consolidate into `bbnf-language-server` per Pass C |
| `grammar/` | Grammar source tree (9 grammars: bbnf, json, css/l4, css/pretty, google-sheets, ebnf, bnf, csv, math) |

---

## §6 — How to verify your reading is current

Before any work, run:

```bash
git log --oneline -15
```

The most recent commit should be `e61a047f docs(restart/prompts): land AMENDMENT-DISPATCH — verify-then-patch four-wave orchestrator` (or later if work has progressed).

If the most recent commit is older — or if no `restart/prompts/AMENDMENT-DISPATCH.md` exists — this handoff is stale; the restart has been amended after this document and you must re-read the current `restart/README.md` + the four reviewer reports.

If the most recent commit shows new amendment-wave commits (e.g., `docs(restart/{audit/pass-N or trio}): wave-N amendment — {scope}`), Wave 1+ has begun; you are picking up mid-amendment. Read the latest amendment-wave commit body + check `git status` + diff against the consolidated punch list before any further dispatch.

---

## §7 — How to dispatch Wave 1

The amendment-dispatch contract at `restart/prompts/AMENDMENT-DISPATCH.md` §3 names Wave 1 verbatim:

> Wave 1 — Foundations (~3-4 hr; serial)
>
> PASS-1 + PASS-2 share Backend IR ownership; serial.
>
> | Order | Agent | Items | Primary surface |
> |---|---|---|---|
> | 1.1 | PASS-1 amendment | 1 (BIR ownership), 3 (Grammar IR schema), 4 (BIR payload + invariants) | `restart/audit/pass-1-substrate/PASS-1.md` + sub-agent correction notes |
> | 1.2 | PASS-2 amendment | 1 (BIR ownership confirmation; verify-only stub since Wave 1.1 lands the surgery), 2 (lowerer import-deny gate), 4 (BIR payload refinement) | `restart/audit/pass-2-codegen/PASS-2.md` + sub-agent correction notes |

Compose the dispatch prompt for the PASS-1 amendment agent first (Wave 1.1). The prompt must include:

1. Wave + role label: "Wave 1.1 — PASS-1 amendment agent"
2. The verify-then-patch discipline verbatim from `AMENDMENT-DISPATCH.md` §1
3. The reviewer-reconciliation directives verbatim from §2 (those relevant to Wave 1.1 — primarily B vs C sequencing for items 40+41, even though Wave 1.1 doesn't touch them; the dispatch prompt establishes discipline for sister waves)
4. The per-item table for Wave 1.1: items 1, 3, 4 with source punch-list directives, target file:line, surgery type, pre-fill verification commands, acceptance gates
5. The pre-fill verification step as Step 1: read `restart/audit/pass-1-substrate/PASS-1.md`; classify each item; commit the classification before any edits
6. Voice + discipline locks per `restart/README.md` §13
7. Hard cap: 60 min
8. Cross-tranche scope boundary: touch ONLY `restart/audit/pass-1-substrate/PASS-1.md` + sub-agent correction notes
9. Output commit: `docs(restart/audit/pass-1-substrate): wave-1.1 amendment — BIR ownership + Grammar IR schema + BIR payload`

When Wave 1.1 commits, dispatch Wave 1.2 (PASS-2 amendment) per the same pattern — its primary surface is `restart/audit/pass-2-codegen/PASS-2.md` and its items are 1 (verify-only-stub since 1.1 landed BIR ownership), 2 (lowerer import-deny gate), 4 (BIR payload refinement).

When Wave 1.2 commits, Wave 1 closes. Dispatch Wave 2 (4 parallel agents per `AMENDMENT-DISPATCH.md` §3 Wave 2 table). When Wave 2's four amendments commit, dispatch Wave 3 (single SYNTHESIS agent). When Wave 3 commits, dispatch Wave 4 (single hardening orchestrator).

Wave 4's verdict gates the next phase. READY → per-tranche full-spec drafting (10 agents A-J, ~3,000-5,000 lines per tranche, inheriting from BA-BD per `restart/inheritance/INDEX.md`). AMENDMENT-REQUIRED-RERUN → narrow-scope follow-up. RE-DRAFT → escalate to user.

---

## §8 — Voice + discipline (governs all writing)

Per `restart/README.md` §13:

- Calibrated, trenchant, approachable. Mild poetic undercurrent welcome; no grandiloquence.
- Archaic-permissive ("hereupon", "thereof", "appurtenant", "begotten", "extant") deployed where befitting. Per `feedback_archaic-diction-is-voice`.
- No corporate hedging ("might", "consider", "perhaps", "may"). State the deliverable; state the gate; move on.
- No metalanguage. Documents do NOT reference commits, conversation history, or "the user said". Path:line citations on every concrete claim.
- Tables liberal; markdown tables for every multi-row enumeration.
- Per-X tables for every "all-X" claim (Operational Rule per Lock 14 enforcement).
- No "TBD" / "user adjudicates" / "future without receiver". Decisions in-plan or named with receiver + blocker + receiving gate.
- No quick solutions; no workarounds; no legacy code uncontested. Architectural transpositions for elegance, simplicity, performance are mandatory.
- Idiomatic gestalt: Rust-idiomatic; sonic-rs / lightning-css / simdjson cohesion the standard.
- Steelman every challenge. Per the Pro/Con/Explication/Challenge discipline at `HARDENING.md` § per-item table, KEEP verdicts must explicitly defeat the steelman; REINVENT and DISCARD verdicts must explicitly survive it.

---

## §9 — Closing posture

Hereupon the next move is Wave 1 dispatch. The four-wave amendment dispatch is the sole remaining step before per-tranche full-spec drafting opens. The 14 locks govern. The precepts speak. The greenfield holds.

The amendment is surgical reconciliation, not relitigation. The pre-fills are verified, not re-authored. The cross-reviewer reconciliations resolve in the dispatch prompt's bake. The hardening reruns at Wave 4 against the amended trio + amended PASS syntheses; the rerun's READY verdict gates per-tranche full-spec drafting.

Read `restart/prompts/AMENDMENT-DISPATCH.md` end-to-end. Then verify the most recent commit hasn't moved past Wave 1. Then compose the Wave 1.1 dispatch prompt per §7 above. Then dispatch.
