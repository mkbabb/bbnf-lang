# SYNTHESIS — Master Plan + Architecture + Migration (Greenfield Restart)

You are the synthesizer-orchestrator. Three pass orchestrators (PASS-1 substrate, PASS-2 codegen, PASS-3 runtime) have committed their outputs (each a synthesis of six sub-agents). You consume all three plus the 18 sub-agent reports plus the corpora plus the locks plus the precepts plus the legacy BA-BD plan-set, and produce the **authoritative master plan + architecture + migration document set.** Single-round suite — your output is the contract that hardening verifies.

You do NOT dispatch sub-agents. You synthesise directly.

## Required reading (mandatory; in order, all paths absolute)

1. `/Users/mkbabb/Programming/bbnf-lang/restart/README.md` — gestalt anchor; settled positions Q1-Q35
2. `/Users/mkbabb/Programming/bbnf-lang/restart/locks/14-LOCKS.md`
3. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/STYLE.md` + `LESSONS-LEARNED.md` + `CONSUMING.md` + `ORCHESTRATION.md`
4. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/tranche/{SPEC, START, RESEARCH, CHALLENGE, WAVE_SPEC, AGENT_DISPATCH_TEMPLATE, DOC_UPDATE_WAVE, README}.md`
5. `/Users/mkbabb/Programming/bbnf-lang/docs/ffuzzy.md` — three primitives
6. `/Users/mkbabb/Programming/bbnf-lang/restart/audit/pass-1-substrate/PASS-1.md` — substrate synthesis + 6 sub-agent reports
7. `/Users/mkbabb/Programming/bbnf-lang/restart/audit/pass-2-codegen/PASS-2.md` — codegen synthesis + 6 sub-agent reports
8. `/Users/mkbabb/Programming/bbnf-lang/restart/audit/pass-3-runtime/PASS-3.md` — runtime synthesis + 6 sub-agent reports
9. `/Users/mkbabb/Programming/bbnf-lang/restart/inheritance/INDEX.md` — BA / BB / BC / BD legacy survival pointer
10. `/Users/mkbabb/Programming/bbnf-lang/restart/corpora/CENSUS.md`, `MODULES.md`, `RESTART-SKETCH.md`, `SOTA.md`
11. The legacy plan-set at `docs/tranches/{BA,BB,BC,BD}/` — the inheritance source for the new tranche set
12. The current bbnf-lang source tree at `crates/` — for migration disposition

## Output Contract — Three Documents

You produce three authoritative documents.

### Document 1 — `restart/ARCHITECTURE.md` (~1,500-2,500 lines)

The new workspace + module structure + dependency DAG + per-file rationale.

§1 — Workspace shape (24 crates per Q1 + Q3 + Q8) — the table from `README.md` §2 finalised with PASS-1/2/3 amendments
§2 — Per-crate `src/` tree — every crate's module layout (4-10 children per dir; sibling-API uniformity per Lock 13); consolidated from PASS-1/2/3 §3 outputs
§3 — Dependency DAG — acyclic; 6-hop ceiling per Q3; depicted as ASCII diagram with explicit arrows; per-edge rationale
§4 — Per-crate public API surface table — exported types, traits, functions (no grammar-name leakage in generic crates per Lock 14)
§5 — Per-crate private internals — must not appear in `pub use` (no leakage); rationale for each
§6 — Workspace `Cargo.toml` schema — full `[workspace]` block + `[workspace.metadata.bbnf.grammars.<ident>]` schema (recogniser plugin schema; host-fn schema; layout / pratt / simd eligibility; output_dir; features)
§7 — IR contract specification — Grammar IR variants (PASS-1) + Backend IR variants (PASS-2); per-variant shape, payload, lower-time invariants, per-backend lowering rules; example grammar fragment that emits each variant
§8 — BBNF formal specification — the canonical EBNF (or formal definition) of the new BBNF (rewrite-mode + lookbehind + Unicode + generics + `@host fn` + `@error` + `@layout` + multi-function chaining + closure semantics)
§9 — File-size + child-count discipline — Lock 13 verification per crate (every dir has 4-10 children mixing one cohesive concern; no file >500 LOC outside `generated/`)
§10 — Future-grammar onboarding test — verbatim walkthrough of adding `yaml.bbnf` (TWO surfaces: source file + metadata block; zero code edit elsewhere)
§11 — Voice + discipline locks
§12 — Closing posture

### Document 2 — `restart/MIGRATION.md` (~1,500-2,500 lines)

Per-file disposition for every file in the current `crates/` tree (~834 .rs files inventoried; abbreviated by directory where uniform).

§1 — Aggregate disposition table:

| Bucket | Count | LOC delta |
|---|---:|---:|
| KEEP-OUTRIGHT | n | (no change) |
| KEEP-MODIFY | n | (positive or negative) |
| ABROGATE-DELETE | n | (negative; total LOC retired) |
| ABROGATE-MOVE | n | (no LOC change; relocation) |
| ABROGATE-REPLACE | n | (per-replacement design) |
| **Total** | **~834** | net |

§2 — Per-crate disposition:

For each current `crates/<name>/`, a table:

| File | New location | Bucket | Rationale | Source PASS finding |
|---|---|---|---|---|

For uniform directories (e.g., 9 per-grammar runtime dirs all ABROGATE-REPLACE → consolidated runtime template), summarise as one row per directory with file count.

§3 — New facilities — items not extant today; located at named paths under the new workspace; PASS-1/2/3 §4 outputs consolidated
§4 — Migration sequencing — per-tranche file movement; pre-tranche-A archive ceremony (Lock 12: `crates/ser/`, `crates/gorgeous/` → `archive/`); commit-chain disposition (per Pass-C Option 3 + Stage-2 PASS-C ratification: keep verbatim + branch reset; tag `pre-restart-2026-05-04`; new branch `master-greenfield-2026-05-04`)
§5 — LOC trajectory — pre-restart 168,750 generated LOC; per-tranche delta; post-restart steady-state
§6 — Inheritance carry-forward — per-tranche table referencing `inheritance/INDEX.md` for BA-BD waves whose substance migrates
§7 — Voice + discipline locks
§8 — Closing posture

### Document 3 — `restart/MASTER-PLAN.md` (~2,500-4,000 lines)

The authoritative tranche set + execution plan.

§1 — Executive summary (3-5 paragraphs; archaic-permissive prose; the thesis)
§2 — Verdict ledger by PASS — KEEP / REINVENT / DISCARD totals from each PASS synthesis
§3 — Workspace shape (cite ARCHITECTURE.md)
§4 — IR + BBNF specifications (cite ARCHITECTURE.md §7 + §8)
§5 — Tranche set — fresh; ≥10 tranches; named A through J (or further; the PASS outputs may demand more):

| Tranche | Title | Wave count | Calendar | Carry FROM | Carry TO | Layer ownership |
|---|---|---:|---|---|---|---|

Suggested allocation (refine per PASS findings):

- **A** — Workspace genesis (Lock 12 archive ceremony; Cargo.toml restructure; per-crate skeletons; commit-chain disposition execution; docs tree restructure; Lock 13 god-directory split)
- **B** — Substrate foundation (`error` + `pipeline` + `source` + `grammar` + `ir` types + `host` dispatch + `host-prims` library)
- **C** — Optimisation pipeline (`passes` + `egraph` + `csp-solver` + `cost-model` + the CSP↔egraph union; type system landing; Hindley-Milner + bidirectional + Pierce-Turner)
- **D** — BBNF extensions + bootstrap (rewrite-mode + lookbehind + Unicode sets + generics + `@host fn` + `@error` + `@layout` + multi-function chaining; bbnf grammar self-host)
- **E** — Codegen IR contract (Backend IR variants finalised; Emitter trait; Rust lowerer scaffold + smoke; regen-equality discipline)
- **F** — Rust lowerer + runtime template (the convergent pivot — Locks 1 + 13 + 14 retire here; per-grammar runtime modules emit from template; OpenFrame retiral workspace-wide; ParseStream union lands)
- **G** — Value API + path + visitor surface (`bbnf` aggregator; pointer! + select!; Visitor + VisitTypes; `parse / parse_in / parse_owned`; cookbook + friction artefacts)
- **H** — Pratt + SIMD auto-detection + WASM lowerer (shape miners; cost-model thresholds; WASM emit; SIMD scanner kernels; first-class platforms per Q29)
- **I** — Error recovery + incremental parsing + LSP (treesitter-class recovery; opt-in batch + always-on for LSP; `bbnf-language-server` consolidation; debug VM hooks; playground compatibility extended)
- **J** — Cross-backend parity + sister-crate publication + close (parity matrix; final perf gates against SOTA; egraph + csp-solver publish; commit chain publication readiness; greenfield close)

§6 — Per-tranche stub — for each tranche (A through J or further), a stub (~150-300 lines) carrying gestalt + hard gates + wave summary table + carry-tags FROM/TO + 14-lock honoured table + risks + build/iter time gate + voice locks + closing posture. **Stubs only at this stage; full waves draft AFTER hardening returns ready and the user signs off.**
§7 — Workspace + Cargo.toml schema (cite ARCHITECTURE.md §6)
§8 — Commit chain disposition (Option 3 ratified per Pass C + Stage-2 PASS-C; operational sequence; tag + branch names; reversibility analysis)
§9 — Docs re-do plan (Pass C six-wave plan; `docs/{lang, perf, howto, process, spec}/` structure)
§10 — Migration timeline (cite MIGRATION.md §4)
§11 — Archive disposition (legacy `docs/tranches/{Y..BD}/` → `docs/tranches/archive/legacy-Y-BD/` at Tranche-A.W0; current `restart/` already archived at `restart-archive-2026-05-04/`)
§12 — 14-lock honoured table at greenfield completion — every lock has owning tranche + verification command (rg / find / cargo / shell)
§13 — Generated-LOC trajectory (cite MIGRATION.md §5)
§14 — Risks + mitigations across tranches
§15 — Voice locks
§16 — Closing posture

## Methodology

Synthesise directly. Read PASS-1/2/3 syntheses end-to-end; cross-reference per-agent reports as evidence; reconcile cross-pass conflicts (cite the deciding lock / precept / settled position).

When passes disagree, cite the deciding authority. Do NOT relitigate the 14 locks. Do NOT relitigate the 35-answer interrogation. Do NOT relitigate the precepts.

Particularly: where any PASS proposes a workspace shape variant deviating from `README.md` §2's 24-crate baseline, evaluate the deviation per the user's settled position (Q1 lean balanced split, more granular if befitting, terse if befitting; Q3 6-hop DAG fine; Q8 path-deps until stable). Where deviations carry net architectural improvement, ratify; where they carry overfit risk, retract.

## Voice + Discipline

(Per `restart/README.md` §13. Calibrated; archaic-permissive; no metalanguage; path:line citations; tables liberal; per-X tables for every "all-X" claim; no "TBD" / "user adjudicates"; no quick solutions; idiomatic gestalt.)

## Hard cap

120 minutes. At minute 110, commit work-in-progress (whatever documents have landed; commit message names which are pending). At minute 120, halt and report.

Recommended cadence:
- Phase 1 (read; 30 min): all required reading
- Phase 2 (ARCHITECTURE.md; 30 min): commit
- Phase 3 (MIGRATION.md; 30 min): commit
- Phase 4 (MASTER-PLAN.md; 30 min): commit final + closing report

## Output commits

Phase 2: `docs(restart): land ARCHITECTURE.md — workspace + IR + BBNF`
Phase 3: `docs(restart): land MIGRATION.md — per-file disposition`
Phase 4: `docs(restart): land MASTER-PLAN.md — fresh tranche set + execution sequence`

## Cross-tranche scope boundary

Touch:
- `restart/ARCHITECTURE.md`
- `restart/MIGRATION.md`
- `restart/MASTER-PLAN.md`

Do NOT modify:
- PASS-1/2/3 outputs (read-only)
- `restart/README.md` (suite anchor; only the user amends)
- `restart/prompts/` (suite definition; read-only)
- `restart/locks/`, `restart/corpora/`, `restart/inheritance/` (read-only)
- `crates/`, `docs/`, `restart-archive-2026-05-04/`

## Background

You are the consolidation point. After your three documents commit, the user invokes the hardening prompt at `restart/prompts/HARDENING.md` (target=MASTER-PLAN). Hardening verifies; if *ready*, the user advances to per-tranche full-spec drafting (one drafting agent per tranche; ~3,000-5,000 lines per tranche; inheritance from BA-BD waves per `inheritance/INDEX.md`).

If hardening returns *amendment-required*, narrow-scope amendment agents apply the punch list. If *re-draft*, the corresponding PASS or this synthesis re-runs.

Your output is the authoritative artefact governing the next ~6-12 months of bbnf-lang work. The user-stated discipline at synthesis: **no contrivance, no overcomplication, no overengineering, ruthless excise of the unnecessary, distillation of essence.** The greenfield is greenfield; carry forward only what is viable, idiomatic, performant.
