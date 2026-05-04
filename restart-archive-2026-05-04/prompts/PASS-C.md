# Pass C — Periphery + Tooling + Docs + Commit Chain (Greenfield Restart)

You are the orchestrator for Pass C. You cover the remaining ~1/3 of the project: analysis, lsp, archived crates (ser, gorgeous), docs (excluding the precepts/ submodule), audit, scripts, tools, server, extension, playground, archive, the workspace top-level files (Cargo.toml, README, Makefile, rust-toolchain.toml), the sibling repos that share governance with bbnf-lang, and **the commit chain itself**.

The commit-chain audit is unique to Pass C and is the most consequential novel deliverable: the user has named the question of whether to rewrite, squash, or keep the last ~2,619 commits. That decision flows from Pass C, ratifies in the synthesizer, and executes outside this suite.

You dispatch six agents in parallel. You synthesise.

## Required reading (mandatory; in order)

1. `/Users/mkbabb/Programming/bbnf-lang/docs/restart/README.md`
2. `/Users/mkbabb/Programming/bbnf-lang/docs/HARDENING-PLAN-PROMPT.md`
3. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/STYLE.md`
4. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/CONSUMING.md`
5. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/LESSONS-LEARNED.md`
6. `/Users/mkbabb/Programming/bbnf-lang/audit/CENSUS-2026-05-03.md`
7. `/Users/mkbabb/Programming/bbnf-lang/audit/MODULES-2026-05-03.md`
8. `/Users/mkbabb/Programming/bbnf-lang/audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md`
9. `/Users/mkbabb/Programming/bbnf-lang/audit/PHASE-4-SYNTHESIS-2026-05-03.md` (partial)
10. `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/meta-audit/06-commit-archaeology.md` (if extant)
11. `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md`

## Pass C Scope

### C.1 — Analysis + LSP
- `crates/analysis/` — entire crate
- `crates/lsp/` — entire crate
- Public API surfaces; what consumes? what does LSP expose?

### C.2 — Archived crates
- `crates/ser/` — moved to archive in Phase-2 prelude; verify
- `crates/gorgeous/` — moved to archive
- `archive/` — what's in it; status; can content be deleted entirely vs preserved as historical record?

### C.3 — Docs (excluding precepts submodule)
- `docs/` — everything EXCEPT `docs/precepts/` (submodule; read-only by Pass C)
- `docs/tranches/` — the entire tranche directory tree (Y, Z, AA … BD; meta-audit; archive)
- `docs/audit/` (or `audit/` at workspace root)
- `docs/migration/`, `docs/cookbook/`, `docs/optimizer/` — Phase-4 artefacts
- `docs/HARDENING-PLAN-PROMPT.md`, `docs/HARDENING-AUDIT-PROMPT.md`, `docs/PHASE-4-DIRECTIVE-2026-05-03.md`
- `docs/codegen-IR-CONTRACT.md` (if extant per BC.W0)
- `docs/SPEC.md` if extant; `docs/README.md`

### C.4 — Audit corpora
- `audit/SOTA-2026-05-03.md`, `CENSUS-2026-05-03.md`, `MODULES-2026-05-03.md`, `RESTART-SKETCH-2026-05-03.md`
- `audit/HARDENING-PLAN-2026-05-03-{01..08}-*.md` — the 8-lane Phase-3 audit
- `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md`
- `audit/PHASE-4-SYNTHESIS-2026-05-03.md`
- `audit/archives/` — older audit sets

### C.5 — Workspace top-level files
- `Cargo.toml` (workspace `[members]`, metadata blocks)
- `Cargo.lock`
- `README.md`
- `Makefile`
- `rust-toolchain.toml`
- `.gitignore`, `.gitmodules`
- `package.json`, `package-lock.json`, `node_modules/` — JS-side residue from playground / server / extension

### C.6 — Tooling + scripts
- `scripts/`
- `tools/` (if extant)
- `server/` — what is this?
- `extension/` — VS Code extension or similar
- `playground/` — interactive playground
- `wasm/` — excluded from workspace per `Cargo.toml` `exclude = ["wasm"]`; audit anyway
- `data/` — fixture data?

### C.7 — Sibling repos (read-only audit)
- `parse-that/ts/` if separately tracked
- The csc411 repo (mentioned in memory; the CSP solver lineage)
- The bbnf-buddy repo (the procedural SVG mascot per memory)
- Any other named-in-memory sibling

For each sibling: location (workspace path-dep / git submodule / external repo); coupling to bbnf-lang; should it stay sibling or merge / split?

### C.8 — The commit chain
- Total commits: ~2,619 per `git log --oneline | wc -l`
- Per-era breakdown: count commits per tranche letter (Y, Z, AA, AB, …, BD)
- Per-era assessment: what's load-bearing for execution? what's archaeology? what's noise?
- Decision matrix: rewrite (squash to era-boundaries) / squash-all (one greenfield commit) / keep verbatim (provenance) / hybrid (squash legacy + keep recent)

## Methodology — Six Agents in Parallel

### Agent C.1 — Inventory

**Lens.** Catalogue every file in Pass C scope.

Critical sub-tables:
- Per-crate inventory (analysis, lsp, ser, gorgeous, archive contents)
- Per-doc inventory: every .md under `docs/`, with summary + last-modified era
- Workspace-file inventory: every top-level config file + its purpose
- Script / tool / server / extension / playground inventory
- Sibling-repo inventory (with coupling notes)
- Commit-chain inventory: per-era commit count + era summary

**Output**: `audit/restart/per-agent/pass-c-agent-1-inventory.md`, ~700-1100 lines.

### Agent C.2 — Idiomaticity (precepts adherence)

**Lens.** Apply precepts to Pass-C files.

Particular foci:
- **archaic-diction-as-voice** for docs — every `docs/**/*.md` should honour `docs/precepts/instructions/STYLE.md`
- **no-metalanguage-docs** — every doc that references commits / conversation history / "the user said" is fault
- **clean-regen-discipline** — generated docs (if any) must be regenerable; hand-patches are fault
- **system-cohesion** for tooling — scripts / Makefile / xtask must not duplicate logic
- **no-workarounds** for archived code — verify ser / gorgeous archive is clean (no backward-compat shims pointing at archive)

**Output**: `audit/restart/per-agent/pass-c-agent-2-idiomaticity.md`, ~600-1000 lines.

### Agent C.3 — Lock-adherence

**Lens.** Apply 14 locks to Pass-C scope.

Particular foci:
- **Lock 12** (ser + gorgeous archive ceremony before BA.W0) — verify the archive is clean; verify workspace `Cargo.toml` removes them
- **Lock 13** (no god directories) — the `docs/` tree itself: how many top-level children? how many in `docs/tranches/`? Is there a Lock-13 violation in the docs structure?
- **Lock 14** (full grammar generalisation) — does any Pass-C file (analysis, lsp, scripts) hardcode a grammar ident? `crates/analysis/` is grammar-coupled per CENSUS; verify how
- **Lock 8** (surpass SOTA, not AU) — every doc that references AU bench numbers as targets is fault per Lock 8; the audit corpora are exempt (they cite history)

**Output**: `audit/restart/per-agent/pass-c-agent-3-lock-adherence.md`, ~600-1000 lines.

### Agent C.4 — Architectural Transposition

**Lens.** Macro-level restructuring.

Concrete questions:
- The `crates/analysis/` crate is grammar-coupled per CENSUS. Should it become a per-grammar crate (one per grammar that needs analysis) or a generic `bbnf-analysis` consuming workspace metadata?
- The `crates/lsp/` crate exposes editor integration. Should it be merged with analysis (single `bbnf-language-server`)? Or kept separate?
- The `docs/` tree carries ~50+ subdirectories (per `docs/tranches/`). Is its layout right? Should the tranche tree be flatter? Should the meta-audit + archaeology directory move to `audit/`?
- The audit corpora (CENSUS, MODULES, RESTART-SKETCH, SOTA, the 8-lane hardening lanes, the synthesis) — should these move from root `audit/` to `docs/audit/`? Or vice versa?
- The sibling repos: should parse-that's TS subtree be its own repo? Or stay workspace-internal?
- The commit chain: rewrite, squash, or keep — propose the disposition with concrete operational sequence (e.g., `git filter-branch` / `git replace --graft` / fresh-rewrite / orphan branch)
- The `docs/precepts/` submodule — is its separation right (it's a submodule for cross-project sharing)? Should the workspace pin it differently?

For each: current state | proposed shape | locks honoured | migration cost.

**Output**: `audit/restart/per-agent/pass-c-agent-4-architectural-transposition.md`, ~700-1100 lines.

### Agent C.5 — Replacement Design

**Lens.** New facilities. Critical items:

- A **`docs/SPEC.md`** master specification — replaces the disorganised tranche-tree / archaeology / audit residue
- A **`docs/architecture.md`** rendering of the post-restart workspace shape
- A **`docs/migration/2026-restart.md`** describing the migration from current state
- A **`bbnf-test-fixtures/`** crate (if not extant) — workspace-internal; carries the per-grammar test fixtures (per the worktree-fixture carry from Phase-3)
- A **`bbnf-cli/`** crate (if not extant) — replaces ad hoc xtask invocations with a stable user-facing CLI
- A **`bbnf-py/`** crate or `bbnf` PyPI package wiring (if any Python binding planned, per memory: `csp-solver` co-located with Python)
- A **`docs/`** restart: every doc not currently honoured by precepts is rewritten
- The **commit-chain rewrite plan** — the operational artefact: branch names, sequence of `git` operations, cutover protocol

**Output**: `audit/restart/per-agent/pass-c-agent-5-replacement-design.md`, ~700-1100 lines.

### Agent C.6 — Cross-cut Analysis + Commit Chain

**Lens.** Cross-cuts spanning Pass-C scope, with the commit chain as the major deliverable.

#### §C.6.A — Pass-C cross-cuts
- Docs ↔ tranches: how do tranche docs cross-reference?
- Audit ↔ tranches: which audits cite tranche files?
- Archive ↔ active: does any active code reference archive contents (it must not)?
- Sibling ↔ workspace: what's the dependency surface from sibling repos into this workspace?

#### §C.6.B — Commit chain analysis
This is the major deliverable. The agent reads `git log --oneline | head -3000` (or `wc -l` to verify count = 2619) and produces:

- **Per-era commit table**: era boundary commits + commit count + era summary
- **Load-bearing test**: which commits' artefacts are still in the working tree at HEAD? Which are dead substrate?
- **Provenance preservation analysis**: if commits are squashed, what is lost? (Author intent; bug-fix archaeology; performance-improvement attribution)
- **Decision matrix**: 4 options
  1. **Rewrite to era boundaries** — squash each era (Y, Z, AA-AT, AU, AV-AX, AY-I/II, AZ-I/II/III/IV, B0/B1, BA-BD) into one commit per era. Result: ~25-30 commits replacing 2,619.
  2. **Squash all** — one greenfield commit replaces all 2,619.
  3. **Keep verbatim** — preserve all 2,619 with a single annotation commit at HEAD declaring greenfield restart.
  4. **Hybrid** — squash legacy (pre-Era-VI: Y through AT, ~700 commits) into one commit; keep Era V + Era VI verbatim.
- **Recommendation** with justification: which option honours the locks (no metalanguage in commit logs once squashed; no legacy substrate lurking in old commits) and the precepts (gestalt; idiomatic; clean slate)?

**Output**: `audit/restart/per-agent/pass-c-agent-6-cross-cut.md`, ~800-1300 lines.

## Synthesis (orchestrator's output)

`audit/restart/PASS-C-2026-MM-DD.md`, ~1500-2500 lines. Same §1-§8 skeleton as Pass A.

Key Pass-C-specific items:
- Commit-chain decision: ratified disposition + operational sequence
- Docs re-do plan: which docs survive, which are rewritten, the sequence
- Archive disposition: keep / delete / extract-as-historical
- Sibling-repo dispositions
- Workspace top-level-files dispositions

## Voice + discipline locks

(Per `docs/restart/README.md` §Voice.)

## Hard cap

You: 60 min. Each agent: 30 min. Wall: ~90 min.

## Output contract

Per-agent: `docs(audit/restart/pass-c/agent-{N}): {lens} of Pass-C scope`.
Orchestrator: `docs(audit/restart/pass-c): synthesise Pass-C — periphery + tooling + docs + commit chain`.

## Cross-tranche scope boundary

Touch ONLY `audit/restart/`. Do NOT modify `crates/`, `docs/tranches/`, `docs/precepts/`, `docs/restart/`. Do NOT execute git operations on the commit chain (just propose them).

## Background

Pass C is the most heterogeneous of the three. Its outputs include the most consequential governance decision of the suite (commit-chain disposition) and the largest operational migration (docs re-do). The synthesizer composes Pass C's verdicts with Pass A and Pass B; the master plan ratifies all three.
