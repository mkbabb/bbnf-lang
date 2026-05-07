# Corpus Audit 1 — Top-Level + Prompts + Locks + Inheritance + Corpora

Date: 2026-05-07
Scope: `restart/{README, ARCHITECTURE, MASTER-PLAN, MIGRATION, HANDOFF}.md` (5 files), `restart/prompts/*.md` (5 files), `restart/locks/14-LOCKS.md`, `restart/inheritance/INDEX.md`, `restart/corpora/{CENSUS, MODULES, RESTART-SKETCH, SOTA}.md` (4 files). Total: 16 files.

This audit classifies each file as EXPLICATE / UPDATE / PRUNE / MERGE / ASK; surfaces overlap; and proposes the post-audit shape the synthesis should adopt. Two cross-cutting questions own this audit specifically: orchestrator-prompt reconciliation (which is the right one?) and top-level-doc reconciliation (5 docs, all needed?). The independent-Codex-hardening protocol question is the third surface.

---

## §1 — Audit scope and corpus references

The audited corpus presents itself in four layers:

1. **Top-level orientation** — `HANDOFF.md` (170 lines), `README.md` (479 lines), and three executable spec/plan/migration docs (`ARCHITECTURE.md` 1,699 lines, `MASTER-PLAN.md` 848 lines, `MIGRATION.md` 816 lines).
2. **Dispatch infrastructure** — `prompts/ORCHESTRATOR.md` (144 lines), `prompts/HARDENING-ORCHESTRATOR.md` (188 lines), `prompts/RESEARCH-FOLD-ORCHESTRATOR.md` (262 lines), `prompts/AMENDMENT-DISPATCH.md` (211 lines), `prompts/HARDENING.md` (268 lines).
3. **Settled commitments** — `locks/14-LOCKS.md` (249 lines).
4. **Reference corpora** — `inheritance/INDEX.md` (73 lines) and `corpora/{CENSUS, MODULES, RESTART-SKETCH, SOTA}.md` (47K, 98K, 44K, 29K characters respectively).

Corpus state per `restart/HANDOFF.md:5`: V7.1 READY at commit `aaeab682`; Phase 8.1 prompt restructure landed at `bc31560c`; V8 simplification audit consolidated at `28987de4` (verdict SIMPLIFY-AVAILABLE). HANDOFF.md is one phase stale relative to git head.

---

## §2 — Top-level docs disposition

| Path | Role (1 sentence) | Disposition | Rationale |
|---|---|---|---|
| `restart/HANDOFF.md` | Cold-start orientation: what the project is + where the work has been + current state + next move + verification rituals + reading order. | **UPDATE** | Single source of truth for cold-start agent (`HANDOFF.md:7`). Stale against git head: claims V7.1 READY (`HANDOFF.md:4-5`), but commits `bc31560c` (Phase 8.1 prompt restructure) + four V8 hardening agents + V8 consolidation `28987de4` have landed since. The §4 prompt-structure section (`HANDOFF.md:67-80`) describes Phase 8.1 as future ("ORCHESTRATOR.md (NEW Phase 8.1) — main entry"), but Phase 8.1 has committed. §3 "current state" + §4 "prompt structure" + §7 "next move" all need a one-paragraph rewrite to reflect Phase 8.2-8.3 completion. |
| `restart/README.md` | Gestalt synthesis: the anthem, workspace shape, IR architecture, BBNF extensions, optimization apotheosis, type system, value API, performance, SOTA, locks carried forward, process, voice, provenance. | **UPDATE** | Authoritative gestalt anchor (referenced 47 times across the audited corpus). §12 prompt-suite shape (`README.md:404-450`) describes the SIX-prompt PASS/SYNTHESIS pipeline that retired at Phase 8.0 — names `PASS-1-SUBSTRATE.md`, `PASS-2-CODEGEN.md`, `PASS-3-RUNTIME.md`, `SYNTHESIS.md`, `HARDENING.md`, `HARDENING-ORCHESTRATOR.md`, all but the last two of which are deleted. Per `HANDOFF.md:78-79`, retired at Phase 8.0. The §12 table needs replacement with the current 5-prompt structure (or removal — see ASK below). All other sections (§1-§11, §13-§15) remain authoritative. |
| `restart/ARCHITECTURE.md` | Phase-2 architectural contract: workspace shape, dependency DAG, public APIs, private internals, Cargo metadata, IR contract, BBNF surface, runtime, codegen, performance targets, future-grammar onboarding, file/directory discipline. | **EXPLICATE** | The single executable architectural spec; bound by §0 authority ledger to README + 14 locks + precepts + PASS syntheses. §7.5 Backend trait + §8.1 directive production + §12 future-grammar onboarding test are load-bearing. Verdict from V8 consolidation pending; assume EXPLICATE with possible Lens-I/J/K simplifications routed through Phase 8.4. |
| `restart/MASTER-PLAN.md` | Phase-2 tranche-set master plan: synthesis-verdict ledger, final workspace, IR contract pointers, hard architectural gates, tranche A-J calendar + outputs + carry matrix, lock ownership, risk register, carry/friction ledger, implementation order. | **EXPLICATE** | The executable plan; sequences ARCHITECTURE's commitments into A-J tranches with same-wave consumer wiring (Lock 4 + Era V failure-mode discipline). §24 carry/friction ledger + §25 implementation order are referenced from HANDOFF.md and required-reading lists across orchestrator prompts. EXPLICATE pending V8 verdict. |
| `restart/MIGRATION.md` | Phase-2 per-file disposition contract: scope/authority, disposition alphabet, aggregate disposition, current crates → restart crates, per-crate disposition tables, generated code, tests, LOC trajectory, legacy BA-BD inheritance, tranche-level migration sequence, migration gates, unresolved punch list. | **EXPLICATE** | The third leg of the executable trio; tranche authors consult per-file. Distinct from MASTER-PLAN: MASTER-PLAN sequences tranches (waves + gates + ledgers); MIGRATION dispositions files (KEEP / ABROGATE / ARCHIVE / GENERATED-REPLACE). No structural overlap. EXPLICATE pending V8 verdict. |

### §2.1 — Top-level reconciliation

The five docs partition cleanly. **HANDOFF** is the cold-start orientation pointer (170 lines; minimal); **README** is the gestalt anchor (479 lines; the synthesis); **ARCHITECTURE / MASTER-PLAN / MIGRATION** form an executable trio (1,699 + 848 + 816 = 3,363 lines; bound to one another by §0-§1 authority ledgers in each). The pairwise overlap question:

- **README ↔ HANDOFF**: README is gestalt (the architectural commitments synthesised); HANDOFF is orientation (current state + next move). Distinct surfaces. README answers "what is bbnf-lang?"; HANDOFF answers "where is the work right now?". Both required.
- **ARCHITECTURE ↔ MASTER-PLAN**: ARCHITECTURE is the spec ("here is what V1 looks like"); MASTER-PLAN is the plan ("here is the order in which V1 lands"). Same artefact in two perspectives only if one collapses spec into plan; the trio's authority ledgers (`ARCHITECTURE.md:10-37` + `MASTER-PLAN.md:0-67`) explicitly partition. ARCHITECTURE §7 IR contract + §8 BBNF surface are inputs MASTER-PLAN treats as fixed (`MASTER-PLAN.md:88-105`).
- **MASTER-PLAN ↔ MIGRATION**: MASTER-PLAN sequences tranches; MIGRATION dispositions files. Tranche A.W0 archives `ser` + `gorgeous` (MASTER-PLAN concern); the per-file fate of every `ser/` source file (MIGRATION concern). Distinct.

**Verdict**: 5 docs, all needed. HANDOFF + README sit above the trio; the trio partitions executably. No merge proposed. UPDATE on HANDOFF (V7.1 → V8 currency) + UPDATE on README §12 (prompt-suite obsolete table). EXPLICATE on the trio pending V8 surfacing.

---

## §3 — Prompts disposition

| Path | Role (1 sentence) | Disposition | Rationale |
|---|---|---|---|
| `restart/prompts/ORCHESTRATOR.md` | Phase-8.1 main entry: phase identification + fan-out to one of three sub-orchestrators per phase type. | **UPDATE** | Genuine entry post-Phase-8.1 (`ORCHESTRATOR.md:3` "single main orchestrator prompt"). §4 phase-8 dispatch table (`ORCHESTRATOR.md:42-50`) is stale — claims 8.1 "IN-PROGRESS"; commits `25addd94` + `597ac678` + `cd6c2b4c` + `624b5af2` show Phase 8.2 complete and `28987de4` is Phase 8.3 V8 consolidation. UPDATE the §4 table to reflect 8.0/8.1/8.2/8.3 DONE; 8.4 PENDING. |
| `restart/prompts/HARDENING-ORCHESTRATOR.md` | Sub-orchestrator dispatching hardening cycles (V1 through V8+); orchestrates per-target hardener agents + Phase-6 consolidation. | **UPDATE** | Pre-Phase-8.1 vintage; the §Required-reading list (`HARDENING-ORCHESTRATOR.md:9-15`) names retired prompts (`PASS-1-SUBSTRATE`, `PASS-2-CODEGEN`, `PASS-3-RUNTIME`, `SYNTHESIS`). Phase 1/2 (PASS dispatch + SYNTHESIS) sections are dead branches (those phases committed during Wave 1; the prompts retired Phase 8.0). The live surface is Phase 3/6 (4-parallel hardening + consolidation) which is what current waves use. UPDATE: cut Phases 1/2 (dead); rename to reflect sub-orchestrator role; remove retired-prompt refs. |
| `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md` | Sub-orchestrator dispatching research deep-dives (8 topics) + fold cycles (V5 metahardening + Phase-1 research + Phase-2 fold + escalation + V6 hardening + V6 consolidation). | **EXPLICATE** | The richest sub-orchestrator: encodes the V5-V6 cycle that landed Wave 5. Live as a re-runnable phase template; future research-fold cycles invoke this prompt. §1 required reading is current (cites HARDENING-CONSOLIDATED-V4 as carry baseline; that's the right anchor for re-running the V5-V6 cycle from scratch). EXPLICATE; no edits required. |
| `restart/prompts/AMENDMENT-DISPATCH.md` | Sub-orchestrator dispatching verify-then-patch amendment cycles (the four-wave Wave-4.1 + Phase-7.5 + future cycles pattern). | **UPDATE** | The §3 Wave-1/2/3/4 plan is V1-vintage (cites V1 punch-list items 1-47 + Reviewer A/B/C/D reports + V1 reconciliation directives). Live discipline (verify-then-patch + reviewer-reconciliation + per-item routing in §1-§2 + §4-§7) is reusable; the §3 wave-specific table is V1-historical. UPDATE: hoist §1-§2 + §4-§7 (the discipline) above the §3 wave-specific table; rewrite §3 as a schematic ("each amendment cycle dispatches per the §1 verify-then-patch contract; per-cycle wave-tables live in the dispatching commit's research notes"). |
| `restart/prompts/HARDENING.md` | Per-target audit specification: 9 lanes + lenses A-K + per-item Pro/Con/Explication/Challenge discipline + KEEP/REINVENT/DISCARD verdicts (+SIMPLIFY/CONSOLIDATE/LEVERAGE/HYBRID/LOAD-BEARING/ASPIRATIONAL/SPECULATIVE per V8+). | **UPDATE** | Live contract; every hardening agent reads it. The §Cycle-specific lens application table (`HARDENING.md:189-194`) shows V1-V4 + V5-V7 + V8+ rows (current). **`HARDENING.md:53` carries explicit "SPECULATIVE (Lens K) — V2+; cite the V2 amendment receiver" — this is the language the user retires.** Mirror at `HARDENING.md:186`. UPDATE: rename SPECULATIVE / fold "V2 amendment receiver" language; rephrase as SIMPLIFY-or-DELETE-not-defer. See §7 V2-deferral ledger below. |

### §3.1 — Prompts as a coherent suite

Five prompts. Three are sub-orchestrators (HARDENING-, RESEARCH-FOLD-, AMENDMENT-DISPATCH); one is the main orchestrator (ORCHESTRATOR); one is the audit specification (HARDENING). The compositional shape is correct: ORCHESTRATOR fans out to one of three sub-orchestrators per phase type; HARDENING is the contract every hardener (dispatched by HARDENING-ORCHESTRATOR or RESEARCH-FOLD-ORCHESTRATOR-Phase-3 or AMENDMENT-DISPATCH-Wave-4) reads.

The structure composes; no MERGE is required across the five. UPDATE on three (currency) + EXPLICATE on RESEARCH-FOLD-ORCHESTRATOR + UPDATE on HARDENING (V2-deferral language).

---

## §4 — Orchestrator reconciliation verdict

The user's question: **which is the right one?**

### §4.1 — The three orchestrator-class prompts

`ORCHESTRATOR.md` (Phase 8.1; main entry; 144 lines), `HARDENING-ORCHESTRATOR.md` (sub-orchestrator; 188 lines), `RESEARCH-FOLD-ORCHESTRATOR.md` (sub-orchestrator; 262 lines).

### §4.2 — Do they actually compose?

**Yes, post-Phase-8.1.** ORCHESTRATOR.md §3 phase-type table (`ORCHESTRATOR.md:33-39`) names HARDENING-ORCHESTRATOR + RESEARCH-FOLD-ORCHESTRATOR + AMENDMENT-DISPATCH as the three sub-orchestrators it fans out to. ORCHESTRATOR.md §7 dispatch protocol (`ORCHESTRATOR.md:96-106`) explicitly transfers wave/agent dispatch to the sub-orchestrator; ORCHESTRATOR's own scope is dispatch invocation + HANDOFF rewrite + consolidation (`ORCHESTRATOR.md:110-113`).

### §4.3 — Is ORCHESTRATOR ceremonial or genuine?

**Genuine, narrowly.** ORCHESTRATOR.md adds three things HARDENING-ORCHESTRATOR + RESEARCH-FOLD-ORCHESTRATOR alone do not provide:

1. **Phase identification protocol** (`ORCHESTRATOR.md:22-28`) — git-state-driven phase recognition; the cold-start agent reads HANDOFF + git log + most-recent HARDENING-CONSOLIDATED to identify the active phase. Neither sub-orchestrator carries this (each presumes the phase has already been identified externally).
2. **Phase-8 dispatch table** (`ORCHESTRATOR.md:42-50`) — current-phase status tracking. This is HANDOFF-adjacent but lives at the orchestrator (orchestrator-agent updates the table after each phase commits). HANDOFF carries narrative; ORCHESTRATOR carries the executable status table.
3. **Hardening-cycle naming canon** (`ORCHESTRATOR.md:54-68`) — the V1 → V2 → V3 → V4 → V5 → V6 → V7 → V8 lineage table, with predecessor + trigger + outputs per cycle. This is the canon any future cycle inherits; HARDENING-ORCHESTRATOR.md alone does not surface it.

### §4.4 — Do sub-orchestrators duplicate ORCHESTRATOR's responsibilities?

**Partially — HARDENING-ORCHESTRATOR carries Phase 1/2 dead branches.** `HARDENING-ORCHESTRATOR.md:17-77` (Phase 1 PASS dispatch + Phase 2 SYNTHESIS) describes phases that the corpus has already passed; the prompts they dispatched (`PASS-1-SUBSTRATE`, `PASS-2-CODEGEN`, `PASS-3-RUNTIME`, `SYNTHESIS`) retired at Phase 8.0 (`HANDOFF.md:78-79`). The Phase 3/6 sections (4-parallel hardening + consolidation) are the live surface — exactly the surface ORCHESTRATOR.md fans out to per §3.

`RESEARCH-FOLD-ORCHESTRATOR.md` does not duplicate; it encodes a 4-phase pipeline (V5 + research + fold + V6) distinct from ORCHESTRATOR's main-entry role. Re-runnable as-is.

### §4.5 — Final verdict

**Three is the right number, post-cleanup.** The architecture is:

- **ORCHESTRATOR.md** — main entry; phase identification + fan-out to sub-orchestrator. Lives at `restart/prompts/ORCHESTRATOR.md`. Cold-start agent reads this first.
- **HARDENING-ORCHESTRATOR.md** — hardening-cycle sub-orchestrator (V1 → V8+ + future). Live surface: Phase 3/6 (per-target hardener dispatch + consolidation).
- **RESEARCH-FOLD-ORCHESTRATOR.md** — research-fold sub-orchestrator (V5/V6 cycle template). Live surface: full 4-phase pipeline, re-runnable.
- **AMENDMENT-DISPATCH.md** — amendment-cycle sub-orchestrator (verify-then-patch discipline; per-cycle wave plans live in dispatching-commit research notes).
- **HARDENING.md** — per-target audit specification; the contract every hardener (regardless of dispatching sub-orchestrator) reads.

**Surgeries**:
1. **HARDENING-ORCHESTRATOR.md: prune Phase 1/2 sections** (`HARDENING-ORCHESTRATOR.md:17-53`). Those phases retired Phase 8.0; the prompts they dispatched are deleted; the section is dead doc-history. Renumber Phase 3 → Phase 1, Phase 6 → Phase 2. Rename file role: "hardening-cycle sub-orchestrator (4-parallel hardener + consolidation)" — drop the legacy six-phase framing.
2. **HARDENING-ORCHESTRATOR.md: required-reading purge** (`HARDENING-ORCHESTRATOR.md:9-15`). Drop refs to retired prompts; replace with current 5-prompt + per-target HARDENING.md mandate.
3. **AMENDMENT-DISPATCH.md: §3 hoist + §1-§2 + §4-§7 promotion**. Hoist verify-then-patch discipline + reviewer-reconciliation directives + per-wave dispatch-prompt template + acceptance gates above the V1-historical wave-table. Rewrite §3 as schematic.
4. **ORCHESTRATOR.md §4 phase-8 dispatch table refresh**. Phases 8.0/8.1/8.2/8.3 DONE; 8.4 PENDING (conditional on V8 SIMPLIFY-AVAILABLE verdict per `28987de4`).

**The three-orchestrator structure is correct. The duplication is V1-historical bloat in HARDENING-ORCHESTRATOR + AMENDMENT-DISPATCH; surgical UPDATE closes it.**

---

## §5 — Locks + inheritance + corpora disposition

| Path | Role (1 sentence) | Disposition | Rationale |
|---|---|---|---|
| `restart/locks/14-LOCKS.md` | Settled architectural commitments (14 locks; post-Phase-7.1 amendments to 4/5/6/7/8/10/12 + 3 NEW: Backend trait, egraph decoupling, 6-directive grammar). | **UPDATE** | Live contract; every audited file cites it. Lock 4 (`14-LOCKS.md:40`) carries "user-facing GADT surface defers to V2 amendment via `BBNF-LOCAL-EQUALITY-ANNOTATION`" — V2-deferral language the user retires. Lock 7 (`14-LOCKS.md:46`) carries "`crates/path-ts/` defers post-V1 alongside the TS-native parse+runtime fork". Lock 8 (`14-LOCKS.md:48`) carries "the WASM lower-and-bench programme awaits the V2 `WasmBackend: Backend` impl". These are user-adjudicated V2 carries (TS+WASM scope-deferred per Phase-7.1; not the SPECULATIVE-V2-amendment-receiver pattern), not stub deferrals. **The user retires the V2 amendment receiver pattern but retains user-adjudicated V1/V2 scope partitions** — the locks' V2 language describes the latter, not the former. UPDATE: re-read each V2 reference; confirm scope-partition (KEEP) vs amendment-receiver (DELETE/INLINE); §7 V2 ledger below disambiguates per occurrence. |
| `restart/inheritance/INDEX.md` | Legacy BA-BD plan-set inheritance map: per-tranche legacy survival ledger; what does + does not inherit; inheritance discipline. | **EXPLICATE** | 73 lines; no V2-deferral language; no metalanguage; clean per-tranche table. Does its job. |
| `restart/corpora/CENSUS.md` | Frozen 2026-05-03 mechanical census: tape archaeology, grammar-specific code in generic crates, legacy markers, duplicated effort, god modules, dynamic typing abuse, inline tests, dead exports, 9-grammar runtime inventory, top-line summary. | **EXPLICATE** | Frozen reference; cited from MIGRATION + ARCHITECTURE for evidence rows. Static; no edits. |
| `restart/corpora/MODULES.md` | Frozen 2026-05-03 module-by-module explication: per-crate module inventories, layered re-org synthesis, crate-level re-org, pipeline ordering, hard-cap tally. | **EXPLICATE** | Same — frozen reference; cited heavily across MIGRATION; static. |
| `restart/corpora/RESTART-SKETCH.md` | Frozen 2026-05-03 JSON parse trace + post-restart pipeline sketch. | **EXPLICATE** | Same — frozen reference. |
| `restart/corpora/SOTA.md` | Frozen 2026-05-03 SOTA survey: cross-comparison matrix, per-target findings, tape-vs-direct feasibility, sonic-class API survey, source list. | **EXPLICATE** | Same — frozen reference; cited from ARCHITECTURE §11 + MASTER-PLAN §H tranche gates. |

---

## §6 — Independent-Codex-hardening protocol

The user's question: **how do we harden this with an independent Codex agent?**

### §6.1 — Cold-start reading order (the critical-path)

Per `RESEARCH-FOLD-ORCHESTRATOR.md:3` ("written to be executed by a coding-agent (Codex, Claude Code, or equivalent)"), the corpus already presupposes Codex-class agents. The cold-start protocol exists implicitly across HANDOFF + ORCHESTRATOR + sub-orchestrators; the question is whether it's surfaced explicitly enough for an agent that arrives blind.

**Cold-start reading order (mandatory; in this order)**:

1. `restart/HANDOFF.md` (170 lines) — orientation; current state; next move; verification rituals at §6.
2. `restart/prompts/ORCHESTRATOR.md` (144 lines) — main entry; phase identification protocol; fan-out table.
3. `restart/README.md` (479 lines) — gestalt anchor; required to interpret what the locks govern.
4. `restart/locks/14-LOCKS.md` (249 lines) — 14 architectural commitments (post-Phase-7.1 + Phase-8.1 amended).
5. The most recent `restart/audit/hardening/HARDENING-CONSOLIDATED-V{N}.md` — terminal-verdict carry baseline. Currently V8 at `28987de4`.
6. `docs/precepts/instructions/STYLE.md` + `LESSONS-LEARNED.md` — voice + discipline (governs all writing).
7. The three executable surfaces (`ARCHITECTURE.md`, `MASTER-PLAN.md`, `MIGRATION.md`) read on-demand per phase obligation, not cover-to-cover.

This list lives at `HANDOFF.md:19-25` and `ORCHESTRATOR.md:7-18`; the union is sufficient.

### §6.2 — "git clone + cargo build" equivalent for the corpus

The verification-ritual block at `HANDOFF.md:108-138` specifies the cold-start verification commands:

```bash
git log --oneline -10               # commit-head identification
git status --short                  # working-tree cleanliness

rg -n 'DK13|Dunfield|higher-rank|GADT.*hidden|closure.*&.i' restart/locks/14-LOCKS.md
rg -n 'ImportDecl.*HostFn.*PrettyDecl' restart/audit/pass-1-substrate/PASS-1.md restart/ARCHITECTURE.md
rg -nC2 '7\.5 Backend|trait Backend|RustBackend.*Backend' restart/ARCHITECTURE.md
rg -n 'path!' restart/audit/pass-3-runtime/PASS-3.md restart/MASTER-PLAN.md
rg -n 'pointer!' restart/                              # only deletion archaeology
rg -n 'parse-that-regex' restart/
rg -n 'regex-automata|bbnf-regex' restart/             # only deletion archaeology
```

This block is the closest equivalent to "cargo build". It identifies the commit head + verifies key invariants are present + verifies retired terms remain retired. **The block is sufficient for V7.1; it predates V8 and Phase-8.1 prompt restructure** — needs an UPDATE to add ORCHESTRATOR.md + Phase-8 lens-I/J/K verifications.

### §6.3 — Recommended Codex-hardening protocol (the proposal)

The cold-start agent (Codex-class) executes:

**Step 1 — Identification**
```bash
git log --oneline -10                               # confirm commit head
git status --short                                  # confirm working tree clean
cat restart/HANDOFF.md                              # one-pass read
```
Acceptance: HANDOFF.md "current state" §3 names the latest committed verdict; the verdict's HARDENING-CONSOLIDATED-V{N} file exists.

**Step 2 — Required reading**
```bash
cat restart/prompts/ORCHESTRATOR.md                 # phase identification + fan-out
cat restart/README.md                               # gestalt
cat restart/locks/14-LOCKS.md                       # commitments
cat restart/audit/hardening/HARDENING-CONSOLIDATED-V{N}.md   # carry baseline
cat docs/precepts/instructions/{STYLE,LESSONS-LEARNED}.md
```
Acceptance: each file read end-to-end; the cold-start agent can name the next-phase from ORCHESTRATOR.md §4.

**Step 3 — Phase identification + sub-orchestrator selection**
Per ORCHESTRATOR.md §3 phase-type table, route to one of:
- HARDENING (verify; rerun cycle) → `HARDENING-ORCHESTRATOR.md`
- RESEARCH-FOLD (ground SOTA; absorb) → `RESEARCH-FOLD-ORCHESTRATOR.md`
- AMENDMENT (verify-then-patch a punch list) → `AMENDMENT-DISPATCH.md`
- SURFACE FOLD (hardening-orchestrator → amendment-dispatch chain) — chained
- PER-TRANCHE FULL-SPEC (out of scope; future)

**Step 4 — Verification before dispatch**
The verification block at `HANDOFF.md:122-138` runs (with the Phase-8.1 additions proposed in §6.2). Failures halt; the cold-start agent surfaces the regression to user.

**Step 5 — Dispatch + monitor + consolidate** per the selected sub-orchestrator's per-phase wave-table.

### §6.4 — Is the protocol sufficient for cold-start?

**Mostly yes; one gap.** The required reading at HANDOFF + ORCHESTRATOR is explicit enough for a Codex-class agent to land on the right phase. The gap: there is no single document titled "BUILD.md" or "BOOTSTRAP.md" that the agent searches for first. HANDOFF carries the role; the agent has to know to read HANDOFF before anything else. The `restart/HANDOFF.md:7` explicit "Read this end-to-end before reading anything else" sentence is load-bearing.

**Proposed surgery**: HANDOFF.md §1 first sentence is "This document is the single source of truth for orienting cold." (`HANDOFF.md:7`). That's the signal. **No new document is required**; the cold-start agent reads HANDOFF, then ORCHESTRATOR, then dispatches. If the user wants extra explicitness, a one-line `restart/README.md:1` lead-in ("Cold-start agent: see `HANDOFF.md` first") would close the gap without adding a doc.

### §6.5 — Verification-ritual currency

Per `HANDOFF.md:122-138`, the verification block needs three additions for V8/Phase-8.1 currency:

```bash
# Phase 8.1 prompt restructure landed
ls restart/prompts/ORCHESTRATOR.md restart/prompts/HARDENING-ORCHESTRATOR.md restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md restart/prompts/AMENDMENT-DISPATCH.md restart/prompts/HARDENING.md

# V8 simplification audit landed
ls restart/audit/hardening/HARDENING-{PASS-1,PASS-2,PASS-3,MASTER-PLAN}-V8.md
ls restart/audit/hardening/HARDENING-CONSOLIDATED-V8.md

# Lenses I/J/K added to HARDENING.md
rg -n 'Lens I|Lens J|Lens K' restart/prompts/HARDENING.md  # ≥3 hits
```

These additions land in the proposed HANDOFF UPDATE.

---

## §7 — V2-deferral occurrence ledger

The user retires the "V2 amendment receiver" pattern; ASPIRATIONAL/SPECULATIVE V8 verdicts must fold V1 or be deleted, not routed to a hypothetical V2.

### §7.1 — Disambiguation

Two distinct uses of "V2" appear in the corpus:

- **(a) User-adjudicated V1/V2 scope partition** — TS + WASM backends explicitly deferred to V2 per Phase-7.1 user adjudication. These are *commitments*, not deferrals: the V2 backend impls have named class (`WasmBackend: Backend` / `TsBackend: Backend` per ARCH §7.5), receiving locks (Lock 5 + Lock 8 + Lock 11), and explicit user adjudication (`README.md` §5 "TS+WASM deferred V2"). These do NOT match the retired pattern.
- **(b) "V2 amendment receiver" pattern** — surfaces routed to a hypothetical V2 without commitment, owner, or receiving artefact. These match the retired pattern.

### §7.2 — Per-occurrence ledger (top-level + prompts + locks + inheritance)

| Path:line | Phrase | Class | Action |
|---|---|---|---|
| `MIGRATION.md:71` | "ABROGATE-MOVE deferred to V2 ... reconstituted as `path-ts` in V2" | (a) scope-partition | KEEP (user-adjudicated TS-V2 commitment) |
| `MIGRATION.md:681` | "WASM defers post-V1 alongside the V2 `WasmBackend: Backend` impl" | (a) scope-partition | KEEP (user-adjudicated WASM-V2 commitment) |
| `MIGRATION.md:803` | "to V2 amendment and no longer occupy V1 carry rows" | **(b) amendment-receiver** | **DELETE** — the phrase "V2 amendment" generalises beyond TS+WASM scope; rephrase per §7.3 below |
| `MASTER-PLAN.md:60` | "`path-ts` defers post-V1 alongside the V2 `TsBackend: Backend` impl" | (a) | KEEP |
| `MASTER-PLAN.md:80` | "`path-ts` defers post-V1 per Lock 7/11 amendments and the V2 `TsBackend: Backend`" | (a) | KEEP |
| `MASTER-PLAN.md:174` | "WASM defers post-V1 via `WasmBackend: Backend`" | (a) | KEEP |
| `MASTER-PLAN.md:209` | "WASM V1 (defers post-V1 alongside V2 `WasmBackend: Backend`)" | (a) | KEEP |
| `MASTER-PLAN.md:227` | "WASM lowering of yaml host primitives defers post-V1" | (a) | KEEP |
| `MASTER-PLAN.md:250` | "`path-ts` defers post-V1 alongside the V2 `TsBackend: Backend` impl" | (a) | KEEP |
| `MASTER-PLAN.md:443` | "`path-ts` defers post-V1" | (a) | KEEP |
| `MASTER-PLAN.md:454` | "TS schema (`path-ts`) defers post-V1 alongside the V2 `TsBackend: Backend` impl" | (a) | KEEP |
| `MASTER-PLAN.md:475` | "WASM defers post-V1 as `WasmBackend: Backend`" | (a) | KEEP |
| `MASTER-PLAN.md:476` | "WASM SOTA defers post-V1" | (a) | KEEP |
| `MASTER-PLAN.md:557` | "the V2 `TsBackend: Backend` impl per `restart/ARCHITECTURE.md` §7.5 publishes `path-ts` in V2" | (a) | KEEP |
| `MASTER-PLAN.md:705` | "`path-ts` defers post-V1 per Lock 7/11 amendments" | (a) | KEEP |
| `MASTER-PLAN.md:781` | **"TS production | V2 amendment | TS lowering defers post-V1"** | (a) but with **(b) amendment-receiver phrasing** | **EDIT** — replace "V2 amendment" with "post-V1 V2 release"; the carry is real (TS lowering deferral) but the receiver naming uses the retired pattern. The carry-receiver column should name `TsBackend: Backend` per ARCH §7.5, not "V2 amendment". |
| `MASTER-PLAN.md:782` | "WASM/TS parity defers post-V1 alongside V2 `WasmBackend: Backend` + `TsBackend: Backend`" | (a) | KEEP |
| `MASTER-PLAN.md:787` | **"`path-ts` schema | V2 amendment**" | (a) but with **(b) phrasing** | **EDIT** — replace "V2 amendment" with "V2 release: TsBackend impl" |
| `MASTER-PLAN.md:788` | **"WASM ABI | V2 amendment**" | (a) but with **(b) phrasing** | **EDIT** — replace "V2 amendment" with "V2 release: WasmBackend impl" |
| `MASTER-PLAN.md:790` | **"`path-ts` package publication timing | V2 amendment**" | (a) but with **(b) phrasing** | **EDIT** — replace |
| `ARCHITECTURE.md:1293` | "user-facing GADT surface defers to V2 amendment via `BBNF-LOCAL-EQUALITY-ANNOTATION`" | **(b) amendment-receiver** | **DECISION REQUIRED** — `BBNF-LOCAL-EQUALITY-ANNOTATION` is a diagnostic-code receiver, not a user-adjudicated V2 commitment. Either: (i) FOLD V1 (rank-1 HM is settled per Lock 4; the GADT-hidden / branch-local-equality machinery already lives V1; the user-facing surface is the question) or (ii) DELETE the deferral language and treat GADT as out-of-scope for V1+V2 unless re-opened. **ASK user.** |
| `ARCHITECTURE.md:1424` | "WASM ... lands as `WasmBackend: Backend` in V2 alongside Lock 11 publication carry" | (a) | KEEP |
| `ARCHITECTURE.md:1425` | "TS ... lands as `TsBackend: Backend` in V2 alongside the principled TS-native parse+runtime fork" | (a) | KEEP |
| `ARCHITECTURE.md:1592` | "They land in V2 alongside `WasmBackend: Backend` and..." | (a) | KEEP |
| `prompts/HARDENING.md:53` | **"SPECULATIVE (Lens K) — V2+; cite the V2 amendment receiver"** | **(b) amendment-receiver — load-bearing** | **DELETE the SPECULATIVE verdict-class** OR rephrase as "V2-confirmed (cite scope-partition commitment + Lock + ARCH-trait receiver) — NO open-ended V2 amendment receivers permitted". V8 hardener cohort applies this lens; the verdict class shapes how V8 SIMPLIFY-AVAILABLE items get classified. The class should name "FOLD-V1" or "DELETE" as the only options for items not falling under (a) scope-partition. |
| `prompts/HARDENING.md:186` | "V1 boundary. Verdict: LOAD-BEARING (V1; cannot defer), ASPIRATIONAL (V1 surface; tranche-deferrable for body), or SPECULATIVE (V2+)" | **(b) amendment-receiver — load-bearing** | **EDIT** — drop "SPECULATIVE (V2+)"; replace with "FOLD-V1 (the surface lands V1 with reduced apparatus) or DELETE (the surface does not land at all)". |
| `locks/14-LOCKS.md:40` (Lock 4) | "user-facing GADT surface defers to V2 amendment via `BBNF-LOCAL-EQUALITY-ANNOTATION`" | **(b) amendment-receiver** | **DECISION REQUIRED — same as ARCHITECTURE.md:1293**; the lock + ARCH duplicate this language. **ASK user**. |
| `locks/14-LOCKS.md:46` (Lock 7) | "`crates/path-ts/` defers post-V1 alongside the TS-native parse+runtime fork" | (a) | KEEP |
| `locks/14-LOCKS.md:48` (Lock 8) | "the WASM lower-and-bench programme awaits the V2 `WasmBackend: Backend` impl" | (a) | KEEP |

### §7.3 — Synthesis

Of ~30 V2 occurrences across the audited corpus, **24 are class (a) user-adjudicated V1/V2 scope partition** (TS + WASM backends; KEEP). **6 carry the (b) amendment-receiver pattern**:

- `MIGRATION.md:803` — "V2 amendment" generic phrase
- `MASTER-PLAN.md:781`, `:787`, `:788`, `:790` — "V2 amendment" used as receiver in carry/friction ledger; rephrase as "V2 release: {Backend impl}"
- `prompts/HARDENING.md:53`, `:186` — SPECULATIVE verdict class + V2-amendment-receiver instruction
- `ARCHITECTURE.md:1293` + `locks/14-LOCKS.md:40` — GADT/BBNF-LOCAL-EQUALITY-ANNOTATION user-facing surface deferral; class (b); requires user adjudication (FOLD V1 vs DELETE)

**Surgery**: rephrase the 4 MASTER-PLAN ledger rows; rephrase MIGRATION:803; restructure HARDENING.md SPECULATIVE verdict class; ASK user on the GADT row.

---

## §8 — Recommended actions (sorted)

### §8.1 — PRUNE (delete)

None at top-level / prompt level. Phase 8.0 already pruned the retired prompts (PASS-1-SUBSTRATE, PASS-2-CODEGEN, PASS-3-RUNTIME, SYNTHESIS) per `HANDOFF.md:78-79`.

### §8.2 — MERGE (consolidate)

None proposed. The 5-doc top-level + 5-prompt structure partitions cleanly.

### §8.3 — UPDATE (currency / V2-deferral / phase-8.1 currency)

| Target | Surgery | Owner |
|---|---|---|
| `restart/HANDOFF.md` | §3 current-state: V7.1 → V8 SIMPLIFY-AVAILABLE; §4 prompt-structure: Phase 8.1 IN-PROGRESS → DONE; §6 verification rituals: add 3 Phase-8.1 commands per §6.5 above; §7 next move: 8.0/8.1/8.2/8.3 → 8.4 PENDING. | synthesis |
| `restart/README.md` | §12 prompt-suite shape: replace 6-prompt PASS/SYNTHESIS table with current 5-prompt structure (or remove §12 prompt-suite section entirely; the canonical prompt-suite description belongs in HANDOFF + ORCHESTRATOR, not in the gestalt anchor). | synthesis |
| `restart/prompts/ORCHESTRATOR.md` | §4 phase-8 dispatch table: 8.0/8.1/8.2/8.3 DONE; 8.4 PENDING (conditional on V8 SIMPLIFY-AVAILABLE). | synthesis |
| `restart/prompts/HARDENING-ORCHESTRATOR.md` | Prune Phase 1/2 sections; renumber (Phase 3 → 1, Phase 6 → 2); refresh required-reading; rename role to "hardening-cycle sub-orchestrator". | synthesis |
| `restart/prompts/AMENDMENT-DISPATCH.md` | Hoist §1-§2 + §4-§7 (the discipline) above §3; rewrite §3 as schematic ("each cycle's wave-tables live in dispatching commit's research notes"). | synthesis |
| `restart/prompts/HARDENING.md` | Drop SPECULATIVE verdict class at `:53` + `:186`; replace with FOLD-V1 / DELETE / V2-confirmed (the latter requires class-(a) scope partition + Lock + ARCH-trait receiver). | synthesis |
| `restart/MIGRATION.md:803` + `restart/MASTER-PLAN.md:{781,787,788,790}` | Rephrase "V2 amendment" → "V2 release: {Backend impl}" per §7.2 ledger. | synthesis |
| `restart/locks/14-LOCKS.md:40` + `restart/ARCHITECTURE.md:1293` | **ASK user** on the GADT/BBNF-LOCAL-EQUALITY-ANNOTATION deferral: FOLD V1 (rank-1 HM is settled; the user-facing surface drops; the diagnostic-code retires) or DELETE (out-of-scope V1+V2 unless re-opened). | synthesis (post-decision) |

### §8.4 — EXPLICATE (authoritative; no edits)

| Target | Why authoritative |
|---|---|
| `restart/ARCHITECTURE.md` | Phase-2 spec; bound by §0 authority ledger; load-bearing across A-J tranches |
| `restart/MASTER-PLAN.md` | Phase-2 plan; sequences ARCHITECTURE into A-J; carry/friction/risk ledgers |
| `restart/MIGRATION.md` | Phase-2 disposition; per-file fates for tranche authors |
| `restart/inheritance/INDEX.md` | Legacy BA-BD survival map; per-tranche inheritance |
| `restart/corpora/{CENSUS,MODULES,RESTART-SKETCH,SOTA}.md` | Frozen 2026-05-03 snapshots; cited from MIGRATION + ARCHITECTURE |
| `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md` | Re-runnable research-fold pipeline template |

---

## §9 — Open questions for synthesis

1. **GADT user-facing surface deferral (Lock 4 + ARCH §8.2)**: FOLD V1 (rank-1 HM is the settled position; the GADT-hidden machinery already exists in CSP solver + branch-local equality plumbing; the user-facing surface drops as YAGNI) or DELETE (out-of-scope; if a future amendment opens GADT, the lock reopens at that point)? **The user retires the V2-amendment-receiver pattern; this is the single hardest case, because the surface is technically ready to land but adds apparatus without V1 use.**

2. **HARDENING.md SPECULATIVE verdict class**: drop entirely (FOLD-V1 / DELETE only) or retain narrowly (V2-confirmed = scope-partition commitment per class (a))? V8 hardener cohort already returned with SPECULATIVE-class verdicts in `28987de4`; if the class drops, the V8 verdicts re-classify as FOLD-V1 or DELETE. The synthesis pass needs to walk the V8 consolidation and re-route any SPECULATIVE entries.

3. **README §12 prompt-suite section**: replace with current 5-prompt table, or excise entirely (delegating to HANDOFF + ORCHESTRATOR)? The latter reduces gestalt-anchor surface; the former preserves "where do I find the prompts?" pointer for the reader who arrives at README first.

4. **HANDOFF as cold-start anchor — explicit signaling**: should `restart/README.md:1` carry a one-line "Cold-start agent: see `HANDOFF.md` first" lead-in, or is HANDOFF's "Read this end-to-end before reading anything else" sentence (`HANDOFF.md:7`) sufficient? Argument for: README is the largest doc; an agent that lands on README cover-to-cover wastes time. Argument against: HANDOFF + ORCHESTRATOR + README is the explicit 3-doc anchor; cold-start protocol is documented inside HANDOFF; the user does not want metalanguage in the gestalt anchor.

5. **AMENDMENT-DISPATCH.md §3 wave-table — preserve historically or rewrite as schematic?** The V1-historical wave-1/2/3/4 plan remains useful as a worked example for future cycles; rewriting as schematic loses the example. The synthesis can fold the schematic above + retain the historical wave-table below as "Wave-1 worked example (historical)".

6. **Phase 8.4 (simplification fold) trigger**: V8 returned SIMPLIFY-AVAILABLE per `28987de4`. Is Phase 8.4 the next move, or is the synthesis pass (cross-cutting amendments per the corpus audit cohort) the next move? The two are not exclusive — Phase 8.4 is a narrow-amendment cycle; the corpus-audit synthesis is a structural fold. The synthesis cohort returns first; Phase 8.4 absorbs the cohort's surgeries (where V8-SIMPLIFY items overlap with corpus-audit UPDATE items, the dispatch is unified).

---

## §10 — Closing posture

The corpus presents one main orchestrator + three sub-orchestrators + one audit-spec contract; the structure is correct post-Phase-8.1; the surgical work is V1-historical bloat removal in HARDENING-ORCHESTRATOR + AMENDMENT-DISPATCH + the V2-amendment-receiver retirement across HARDENING.md + 6 ledger rows + 1 user-decision row (GADT).

The five top-level docs partition cleanly. HANDOFF + README sit above the executable trio (ARCHITECTURE + MASTER-PLAN + MIGRATION); no merge is required. UPDATE on HANDOFF (V7.1→V8 currency) + UPDATE on README §12 (prompt-suite obsolete). The trio is EXPLICATE pending V8 SIMPLIFY-AVAILABLE absorption.

The independent-Codex-hardening protocol is documented implicitly across HANDOFF + ORCHESTRATOR + sub-orchestrators; no new doc is required. The verification block at HANDOFF:122-138 needs three Phase-8.1 currency additions (per §6.5).

The V2-amendment-receiver pattern retires via 6 surgical edits + 1 user-decision case (GADT/BBNF-LOCAL-EQUALITY-ANNOTATION at Lock 4 + ARCH §8.2). Class (a) user-adjudicated V1/V2 scope partitions (TS + WASM via Backend trait at ARCH §7.5) are commitments not deferrals; they remain.

Hereupon the synthesis cohort consolidates this audit (#1) with the parallel #2-#N audits (other agents), producing the post-corpus-audit fold + the unified UPDATE batch.
