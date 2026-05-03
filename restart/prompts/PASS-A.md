# Pass A — Parse Front (Greenfield Restart)

You are the orchestrator for Pass A of the bbnf-lang greenfield restart audit. You cover ~1/3 of the project — the parser front: source acquisition, parsing, lowering, IR, sister-parsers (parse-that, bbnf-regex), the path crate triplet (current `bbnf-path` / `bbnf-path-ts`), the bootstrap crate, and the `grammar/` source tree. The other 2/3 are covered by Pass B (codegen + runtime + optimisers) and Pass C (periphery + tooling + docs + commit chain).

You dispatch six agents in parallel, each applying the user's three-category rubric — **keep outright / keep but modify / abrogate (delete / move / replace)** — through a distinct analytical lens. You synthesise their outputs into a single per-pass report.

## Required reading (mandatory; in order)

**Suite directives:**
1. `/Users/mkbabb/Programming/bbnf-lang/docs/restart/README.md` — the master orchestration; what Pass A is for
2. `/Users/mkbabb/Programming/bbnf-lang/docs/HARDENING-PLAN-PROMPT.md` — the canonical 14-lock master
3. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/STYLE.md` — voice
4. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/CONSUMING.md` — what consuming a precept means
5. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/LESSONS-LEARNED.md` — the failure-mode anatomy

**Prior audit corpora (use as ground truth, do not re-derive):**
6. `/Users/mkbabb/Programming/bbnf-lang/audit/CENSUS-2026-05-03.md`
7. `/Users/mkbabb/Programming/bbnf-lang/audit/MODULES-2026-05-03.md`
8. `/Users/mkbabb/Programming/bbnf-lang/audit/RESTART-SKETCH-2026-05-03.md`
9. `/Users/mkbabb/Programming/bbnf-lang/audit/SOTA-2026-05-03.md`
10. `/Users/mkbabb/Programming/bbnf-lang/audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md`
11. `/Users/mkbabb/Programming/bbnf-lang/audit/PHASE-4-SYNTHESIS-2026-05-03.md` (partial; the available 577 lines are authoritative)
12. `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md` — substrate-first / consumer-later failure mode

## Pass A Scope (the corpus to audit)

**Parser-input → IR portion of the pipeline.** Concretely:

### A.1 — Core parse front
- `crates/core/src/source/` — input acquisition + line-column infrastructure
- `crates/core/src/parse/` — the parse driver + scanner integration
- `crates/core/src/lower/` — grammar IR → optimisation-ready IR lowering
- `crates/core/src/host/` — host-fn dispatch (current root; CSS-types lives here today)
- `crates/core/src/path/` — runtime path engine
- `crates/core/src/grammar/` source side (NOT generated/ — that's Pass B)
- `crates/core/src/lib.rs` — front-of-pipeline re-exports

### A.2 — IR crate
- `crates/ir/` — entire crate
- `crates/ir/src/passes/` per-pass inventory
- `crates/ir/src/registry/` strategy + lookup substrate
- `crates/ir/src/types/` (or wherever Layout / TypeDesc lives post-Phase-4)

### A.3 — Sister parser crates
- `crates/parse-that/` — combinator library
- `crates/bbnf-regex/` (currently sibling of parse-that or its own crate)
- `crates/bbnf-path/`, `crates/bbnf-path-ts/` — path proc-macros
- `crates/bootstrap/` — meta-grammar bootstrap

### A.4 — Grammar source tree
- `grammar/bbnf/`
- `grammar/json/`
- `grammar/css/`
- `grammar/google-sheets/`
- `grammar/ebnf/`
- `grammar/bnf/`
- `grammar/misc/` (csv, math, etc.)
- `grammar/*.bbnf` files; per-grammar fixture / test directories

### A.5 — Workspace + Cargo.toml (parser-relevant portions only; full Cargo audit is Pass C scope)
- `[workspace.metadata.bbnf]` grammar manifest
- `[workspace.metadata.bbnf-strategy]` (the pre-Phase-4 9-arm allow-list)

**Out of Pass A scope:** generated/, codegen/, runtime/, pipeline/, egraph, csp-solver, simd-scan, xtask, analysis, lsp, ser, gorgeous, docs, audit, scripts, sibling repos, the commit chain.

## Methodology — Six Agents in Parallel

You dispatch the six agents below in a single batch. Each writes to `audit/restart/per-agent/pass-a-agent-{N}.md`. Each carries a 30-min hard cap. After all six commit, you synthesise.

### Agent A.1 — Inventory

**Lens.** Exhaustive catalogue of every file in Pass A scope.

**Output table** (one row per file):

| File | LOC | Public-API surface | Dependents | Invariants | Current state | Author intent |
|---|---:|---|---|---|---|---|

`Author intent` field: read the top-of-file comment / module doc; record the original intent. Do not classify yet.

**Output**: `audit/restart/per-agent/pass-a-agent-1-inventory.md`, ~400-700 lines.

### Agent A.2 — Idiomaticity (precepts adherence)

**Lens.** Apply every precept under `docs/precepts/` to every file in scope. Per file, identify violations.

The precepts to apply (read the source files; list these in the agent's prompt):
- no-workarounds (per `feedback_no-workarounds`)
- no-orthogonal-codepaths
- KISS / DRY
- single-plan-execution
- preserve-rich-AST
- direct-to-struct
- system-cohesion
- pluggable-components
- gestalt-approach (no quick solutions; root-cause)

Per-file output: violations table — file:line, precept, violation, surgery.

**Categorisation per file**: KEEP-OUTRIGHT (no violations) / KEEP-MODIFY (named surgery) / ABROGATE (precept-violations exceed surgery threshold).

**Output**: `audit/restart/per-agent/pass-a-agent-2-idiomaticity.md`, ~600-1000 lines.

### Agent A.3 — Lock-adherence

**Lens.** The 14 locks at `docs/HARDENING-PLAN-PROMPT.md` applied per file in Pass A scope. Particularly:

- **Lock 1** (tape + columnar dead) — search for `TapeRec`, `TapeCursor`, `payload_idx`, `OpenFrame`, `FusedBuilder` residue
- **Lock 2** (layout lowering canon) — `TypeDesc` / `StructLayout` / `TypeMap` aliases must be retired
- **Lock 5** (IR + per-backend lower) — bbnf-ir must NOT contain backend-emit code
- **Lock 7** (path crate consolidation) — current state is mid-Phase-4-redress; the audit targets the post-redress shape: `crates/path/`, `crates/path-core/`, `crates/path-ts/`
- **Lock 11** (path-deps for incubating sister crates) — parse-that, bbnf-regex must be path-deps until stable
- **Lock 13** (no god directories) — `crates/core/src/runtime/<g>/` is god-directory-by-mixed-concern; per-grammar runtime dirs forbidden under Lock 14
- **Lock 14** (full grammar generalisation) — ZERO grammar-specific code in generic crates; `crates/ir/src/registry/strategy.rs:130-185` 9-arm match-list is the archetype violation; `shape_dict_bbnf.rs` is named after a grammar; `css_types.rs` at core root is overfit

**Per-lock per-file table**: file:line | lock | status (honoured / violated / silent) | surgery.

**Output**: `audit/restart/per-agent/pass-a-agent-3-lock-adherence.md`, ~600-1000 lines.

### Agent A.4 — Architectural Transposition

**Lens.** Macro-level restructuring proposals. The agent does NOT classify per file; it proposes whole-system rewrites where elegance / simplicity / performance suffer under the current shape.

Concrete questions the agent must answer:
- The `crates/core` workspace member is a kitchen sink (~17,000 LOC across source/parse/lower/codegen/runtime/path/pipeline/host). Should it split into `bbnf-parse` / `bbnf-codegen` / `bbnf-runtime` per the prior MODULES audit? Or further?
- The `crates/ir` crate carries IR types, passes, and registries. Should IR types live in `bbnf-ir`, passes in `bbnf-passes`, registries in workspace metadata only?
- The path crate triplet (proc-macro + cdylib + shared-core) — is this the right shape? Are there alternatives (e.g., one path-core crate with two thin proc-macro shells re-exporting)?
- `parse-that` is workspace-internal sibling; should it live in this workspace, or move to a sibling repo? What grammar coupling does it carry?
- `bbnf-regex` is currently embedded in parse-that or sibling-of; what is its right boundary?
- The grammar source tree at `grammar/`: is its layout (per-grammar dirs) right, or should it be a single flat file-set?
- The bootstrap crate: what is its responsibility post-Lock-14? Is it a re-export shim?

For each macro-proposal:
- Current state (path:line citations)
- Proposed shape
- Justification (which locks / precepts are honoured by the proposal)
- Migration cost (LOC delta; backward-incompatibility surface; sister-tranche carries)

**Output**: `audit/restart/per-agent/pass-a-agent-4-architectural-transposition.md`, ~500-900 lines.

### Agent A.5 — Replacement Design

**Lens.** For every file Agent A.3 / A.2 mark ABROGATE-REPLACE, design the new facility. Plus: identify brand-new items whose absence is felt.

**Replacement table**:

| Abrogated item | New facility | Justification | Located at | Implementation sketch |
|---|---|---|---|---|

**New-facility table** (items not currently extant but needed):

| New facility | Why it's missing | Located at | Implementation sketch | Locks / precepts honoured |
|---|---|---|---|---|

Concrete examples the agent should consider:
- The `inverse-layout-audit` gate (per Phase-3 hardening surgery #17) — where does this live? A new `crates/bbnf-layout-audit/` crate? Or a pass in `bbnf-ir/passes/`?
- A `bbnf-grammar` crate carrying the grammar-source-tree types + parser, separate from the bootstrap-grammar
- A workspace metadata schema validator (per Phase-3 surgery #16) — where does this live?
- A unified `bbnf-error` crate carrying error types shared across parse / lower / codegen / runtime, replacing the per-crate `Error` types
- The cohort-template generator (per Phase-3 gap D) — where does this live? Is it a proc-macro or an xtask?

**Output**: `audit/restart/per-agent/pass-a-agent-5-replacement-design.md`, ~500-800 lines.

### Agent A.6 — Cross-cut Analysis

**Lens.** Concerns that span multiple files. The agent does NOT classify per file; it surfaces cross-file dependencies, hidden coupling, accidental complexity.

Specific cross-cuts to investigate:
- **Hidden grammar coupling**: every `match grammar { Json => ..., CssL4 => ..., ... }` arm in any Pass-A scope file. Who consumes? Who produces?
- **Shared substrate**: types or functions used by ≥3 modules but living in one — relocation candidates
- **Accidental complexity in the parse driver**: how many code paths exist for "parse a leaf"? "parse a compound"? "parse a repeat"? Are they orthogonal (Lock § no-orthogonal-codepaths)?
- **God directories**: which Pass-A directories are god-by-mixed-concern (Lock 13)?
- **Hidden dependents on dead substrate**: tape residue per CENSUS, OpenFrame residue, EmissionTier residue
- **Cyclic dependency risk**: does `bbnf-ir` depend on anything in `crates/core/`? Should it?

Per cross-cut: cite path:line for every claim; propose surgery (relocation / merge / split / delete).

**Output**: `audit/restart/per-agent/pass-a-agent-6-cross-cut.md`, ~500-800 lines.

## Per-Agent Prompt Skeleton (you compose, dispatch, await)

Each agent's prompt (written by you, the orchestrator) carries:
- The agent's lens (one of the six above, verbatim)
- Pass A scope verbatim from §Pass A Scope above
- The mandatory reading list (universal — items 1-12 from this prompt's reading list, restricted)
- Output path (one of the six `audit/restart/per-agent/pass-a-agent-{N}-{lens}.md`)
- Voice + discipline locks (universal)
- 30-min hard cap; commit at minute 27; halt at minute 30
- Cross-tranche scope boundary (write only to its own output path; read-only on `crates/`)

## Synthesis (orchestrator's own output)

After all six agents commit, you produce `audit/restart/PASS-A-2026-MM-DD.md`, ~1500-2500 lines, structured:

### §1 — Pass A verdict ledger

| Item | Bucket | Source agent(s) | Rationale | Successor (if abrogate) |
|---|---|---|---|---|

Every file in Pass A scope appears in this ledger. The bucket is one of: KEEP-OUTRIGHT / KEEP-MODIFY / ABROGATE-DELETE / ABROGATE-MOVE / ABROGATE-REPLACE.

When agents disagree (rare but real), the orchestrator adjudicates and cites the deciding lock / precept / locked-decision.

### §2 — Architectural transpositions ratified for Pass A

Subset of Agent A.4's proposals that the orchestrator ratifies. For each: name + sketch + locks honoured + carry to synthesizer.

### §3 — New facilities for Pass A scope

Subset of Agent A.5's proposals ratified. Per facility: name + location + sketch.

### §4 — Cross-cuts ratified

Subset of Agent A.6's findings ratified. Per cross-cut: surgery + receiving locus.

### §5 — Pass-A residues to flag for synthesizer

Anything Pass A surfaces that the synthesizer must reconcile across passes (e.g., Pass-A-suggested `bbnf-error` consolidation must be cross-referenced with Pass B's error-handling findings).

### §6 — Lock + precept verdicts at Pass A close

| Lock / precept | Pass A verdict | Cite |
|---|---|---|

Empty cell = silent (must surface to synthesizer).

### §7 — Pass A punch list

Ordered list of surgical edits the synthesizer + tranche-author must apply. Each entry: target file:line / surgery / locks honoured / estimated LOC.

### §8 — Greenfield commitments from Pass A

Items that should appear in the new tranche set as gates / waves / hard-gates:
- New crate creation list
- File migration list
- Surgery list
- LOC budget projections

## Voice + discipline locks (universal)

Per `docs/precepts/instructions/STYLE.md` + `feedback_archaic-diction-is-voice` + the user's greenfield mandate:

- Calibrated, trenchant, approachable
- Mild poetic undercurrent welcome; no grandiloquence
- Archaic-permissive ("hereupon", "thereof", "appurtenant", "begotten", "extant") deployed where befitting
- No corporate hedging ("might", "consider", "perhaps")
- No metalanguage (no commit refs, no audit-doc refs except as ground-truth citations)
- Path:line citations on every concrete claim
- Tables liberal
- **No quick solutions; no workarounds; no legacy code survives uncontested**
- **Idiomatic, gestalt approaches** — Rust-idiomatic; sonic-rs / lightning-css / simdjson cohesion the standard
- **Architectural transpositions for elegance, simplicity, performance** are mandatory, not optional

## Hard cap

You (orchestrator): 60 min. Each agent: 30 min. Total wall: ~90 min if agents parallelise cleanly; 120 min if sequential.

At minute 55 you commit work-in-progress. At minute 60 you halt and report.

## Output contract

Per-agent commits: `docs(audit/restart/pass-a/agent-{N}): {lens} of Pass-A scope`.
Orchestrator commit: `docs(audit/restart/pass-a): synthesise Pass-A — parse-front audit`.

## Cross-tranche scope boundary

You touch ONLY `audit/restart/`. Do NOT modify `crates/`, `docs/tranches/`, `docs/precepts/`, `docs/restart/` (this directory is the suite definition; read-only). Do NOT modify the Phase-4 directive.

## Background

Pass A is the first of three. Pass B and Pass C run in parallel with Pass A; you do NOT coordinate cross-pass directly. The synthesizer reconciles. If Pass A surfaces concerns that span Pass B or Pass C scope, name them explicitly in §5 (residues) for synthesizer pickup; do not act on them.

The 14 locks are settled. The precepts are settled. The greenfield mandate is settled. Pass A ratifies, classifies, and synthesises; it does not relitigate.
