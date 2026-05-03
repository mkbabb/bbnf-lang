# Pass C Agent 1 — Inventory

Date: 2026-05-03. Scope: every file under Pass-C jurisdiction — analysis, lsp, archived crates, docs (excl. `precepts/`), audit, scripts, tools, server, extension, playground, wasm, data, workspace top-level files, sibling repos, and the commit chain.

The lens is exhaustive catalogue. Buckets, dispositions, and lock-adherence land in Agents 2-5; this agent enumerates.

---

## §1 — Per-crate inventory (Pass-C-scoped crates)

### §1.1 — `crates/analysis/`

LSP analysis engine. BBNF-grammar-specific by construction (it implements LSP for BBNF source files); the grammar-coupling is present-but-arguably-correct per CENSUS §2.3. Surface:

| Path | Purpose | LOC class |
|---|---|---|
| `crates/analysis/src/lib.rs` | Crate root; exports `analysis`, `directives`, `features`, `state` | small |
| `crates/analysis/src/analysis.rs` | Top-level `Analysis` orchestrator | medium |
| `crates/analysis/src/state/parsing.rs` | Document state + parse cache | medium |
| `crates/analysis/src/state/*.rs` | Other state surfaces | small |
| `crates/analysis/src/directives/hints.rs` | `@pretty`/`@recover`/`@inline`/`@token` hint catalogue (BBNF-specific) | medium |
| `crates/analysis/src/features/formatting.rs` | LSP formatting feature; consumes `bbnf::runtime::bbnf::{BbnfCompoundKind, BbnfView}` | medium |
| `crates/analysis/src/features/selection_range.rs` | LSP selection range | small |
| `crates/analysis/src/features/...` (7+ feature files) | LSP features | medium each |

Public-API: `Analysis::new`, `Analysis::analyse_document`, `Analysis::format`, `DocumentState`, `Diagnostic`, `Hint`, `SelectionRange`, etc.

Dependents: `crates/lsp` (the LSP server consumes analysis); the playground also reaches via wasm shim.

Coupling: ties direct to BBNF runtime (`BbnfCompoundKind`, `BbnfView`); ties to `bbnf-ir` for grammar-tier facts; touches `crate::types::Ast` for parse representation.

### §1.2 — `crates/lsp/`

LSP server binary + DAP debug adaptor.

| Path | Purpose |
|---|---|
| `crates/lsp/src/lib.rs` | Server entry surface |
| `crates/lsp/src/main.rs` | Binary entry point |
| `crates/lsp/src/server/...` | LSP server impls — handlers per LSP message |
| `crates/lsp/src/dap/...` | DAP (debug adaptor) impls |
| `crates/lsp/benches/bench_lsp.rs` | LSP bench harness |

Public-API: server-binary; not consumed by other crates.

Dependents: VS Code extension (`extension/src/extension.ts` spawns the LSP binary).

Coupling: depends on `crates/analysis` (the LSP wraps Analysis surface).

### §1.3 — `crates/ser/` (archived candidate per Lock 12)

| Path | Purpose |
|---|---|
| `crates/ser/Cargo.toml` | Crate manifest |
| `crates/ser/src/...` | Serialisation traits + impls |

Status at 2026-05-03: STILL listed in `Cargo.toml` workspace `[members]` line 2. Lock 12 (ser + gorgeous archive BEFORE BA.W0) — the ceremony has NOT been executed. Crate must be moved to `archive/ser/` and dropped from workspace before the restart begins.

### §1.4 — `crates/gorgeous/` (archived candidate per Lock 12)

| Path | Purpose |
|---|---|
| `crates/gorgeous/Cargo.toml` | Crate manifest |
| `crates/gorgeous/grammar/` | Grammar definitions for prettify |
| `crates/gorgeous/src/{json,bbnf,bnf,ebnf,css,google_sheets}.rs` | Per-grammar prettify wrappers (~10-15 LOC each per CENSUS §2.5) |
| `crates/gorgeous/src/builtin.rs` | Match-on-grammar-name dispatch (lines 9-22 per CENSUS) |
| `crates/gorgeous/src/vm.rs` | Bytecode prettify VM; line 217 has "shouldn't happen in practice" fallback per CENSUS |
| `crates/gorgeous/tests/...` | Tests |

Status at 2026-05-03: STILL listed in `Cargo.toml` workspace `[members]` line 2. Lock 12 ceremony NOT executed.

### §1.5 — `archive/`

The directory does not yet exist at the workspace root (`ls /Users/mkbabb/Programming/bbnf-lang/archive` returned non-zero). Lock 12's archive destination is therefore unprovisioned; the archive ceremony lands the directory or the lock's path varies.

---

## §2 — Per-doc inventory

### §2.1 — `docs/` top-level (non-tranche, non-precepts, non-restart)

| Path | Purpose | Last-modified era |
|---|---|---|
| `docs/GESTALT.md` | Architectural gestalt narrative | Era VI |
| `docs/HARDENING-AUDIT-PROMPT.md` | Codebase audit prompt — generalised template | Era VI (recent) |
| `docs/HARDENING-PLAN-PROMPT.md` | Plan-set audit prompt | Era VI |
| `docs/PHASE-4-DIRECTIVE-2026-05-03.md` | Phase-4 specification-depth re-draft directive | Era VI (recent) |
| `docs/codegen-paths.md` | Codegen path narrative | Era V or earlier |
| `docs/restart/{README,PASS-A,PASS-B,PASS-C,SYNTHESIZER,HARDENING}.md` | This suite | Era VI (most recent) |

### §2.2 — `docs/bbnf/` (BBNF language docs)

7 files: `api-reference.md`, `getting-started.md`, `grammar-syntax.md`, `lsp.md`, `operators.md`, `pretty-directives.md`, `recover-directives.md`. Era V or earlier; no Lock-14 awareness.

### §2.3 — `docs/parse-that/`

5 files: `combinators.md`, `leaf-parsers.md`, `overview.md`, `regex-engine.md`, `span-combinators.md`. Documents the sister parse-that TS/JS lineage.

### §2.4 — `docs/performance/`

11 files: `automaton-theory-memo.md`, `benchmarks.md`, `formatting.md`, `ir-pipeline.md`, `lsp.md`, `overview.md`, `parsing.md`, `pprint.md`, `regex-codegen.md`, `timeline.md`, `wasm.md`.

### §2.5 — `docs/pprint/`, `docs/gorgeous/`, `docs/cookbook/`, `docs/optimizer/`, `docs/migration/`

| Subdir | Files | Era |
|---|---|---|
| `docs/pprint/` | `doc-api.md`, `overview.md` | older |
| `docs/gorgeous/` | `overview.md` | older |
| `docs/cookbook/` | `lifetime-surfaces.md`, `path-macro.md`, `visitors.md` | Phase-4 (recent) |
| `docs/optimizer/` | `pratt-simd-detection.md` | Phase-4 (recent) |
| `docs/migration/` | `bc-core-split.md` | Phase-4 (recent) |
| `docs/instructions/` | `CHANGELOG.md`, `PROFILING.md`, `README.md` | Era V/VI (active) |
| `docs/benchmarks/SPEC.md` | bench harness spec | Era V/VI |

### §2.6 — `docs/tranches/` tree

49 sub-directories at top level. Letter inventory (alphabetic):
- `Y, Z, AA, AB, AC, AE, AF, AG, AH, AI, AJ, AK, AL, AM, AN, AO, AP, AQ, AR, AS, AT, AU, AV, AW, AX` — Era IV/V landed
- `AY-I, AY-II-I, AY-III` — Era VI restart waves
- `AZ-I, AZ-II, AZ-III, AZ-IV` — Era VI continuation
- `B0, B1, B2, B3, B4, B5, B6, B7` — prelude annexes
- `BA, BB, BC, BD` — current restart-target tranches (newly drafted)
- `archive/{pre-restart-BA, pre-restart-BB, pre-restart-BC}` — first-pass BA/BB/BC archived 2026-05-03
- `meta-audit/` — eight-axis meta-audit + archaeology subdir
- `W, X` — Era IV holdover dirs (mostly empty)
- `PLAN-INPUT-2026-05-03.md` — plan-input doc

The tranche-tree's branching factor (49 immediate children) violates Lock 13's "directories with >10 immediate children mixing concerns are forbidden". The tranches are a single concern (tranche records), so the 10-child cap applies subjectively; the archaeology + meta-audit + W/X holdover bloat the directory. Disposition: relocate per-tranche under `archive/legacy-Y-BD/` per the restart README.

### §2.7 — `docs/precepts/` (submodule; READ-ONLY for Pass C)

Submodule per `.gitmodules`. Path-pinned per `docs/precepts/instructions/CONSUMING.md`. Out of Pass-C surgery scope.

---

## §3 — Workspace top-level file inventory

| Path | Purpose | Notes |
|---|---|---|
| `Cargo.toml` | Workspace manifest + `[workspace.metadata.bbnf]` grammar table + `[workspace.metadata.bbnf-strategy]` strategy table | 14 workspace members; ser + gorgeous still listed |
| `Cargo.lock` | Lock file | Tracked in repo |
| `LICENSE` | License | Standard |
| `Makefile` | High-level convenience targets (~420 lines) | Heavy on AY-W5-W7 gate commands |
| `README.md` | Top-level project overview | Stale framing — claims `rust/` directory layout that no longer matches actual `crates/` layout |
| `rust-toolchain.toml` | Nightly pin (`nightly-2026-04-11`) | Era VI; bisected against ICE reproducer |
| `.gitignore` | Standard |
| `.gitmodules` | `docs/precepts` submodule pin |
| `package.json`, `package-lock.json`, `node_modules/` | JS-side residue from extension/playground/server | Workspace top-level (not extension-scoped) |

### §3.1 — Stale README assertion

`README.md` line 12-33 documents directory structure as `rust/`, `wasm/`, `typescript/`, `prettier-plugin-bbnf/`, `playground/`, `extension/`, `grammar/`, `docs/`, `scripts/`, `data/`, `server/`, `.github/workflows/`, `.vscode/`. The actual workspace has `crates/` (not `rust/`), `xtask/` (not enumerated), and no top-level `typescript/` or `prettier-plugin-bbnf/`. The README is stale by at least one architectural reorganisation (the `architectural-consolidation` memory item: `rust/→crates/`).

---

## §4 — Tooling, scripts, server, extension, playground inventory

### §4.1 — `scripts/`

13 files:

| Path | Purpose |
|---|---|
| `scripts/bisect-fastpath.sh` | Bisect harness for fastpath regressions |
| `scripts/deploy.sh` | Deploy script (playground or extension) |
| `scripts/doctor.sh` | Host-readiness probe (sccache, nextest, samply, lld, cargo-expand) |
| `scripts/extract_hotspots.py` | Python: extract hotspot data from samply outputs |
| `scripts/hooks/pre-commit` | Pre-commit hook |
| `scripts/iai-compare.sh` | iai bench compare harness |
| `scripts/install-hooks.sh` | Install git hooks |
| `scripts/kill-all-rust.sh` | Kill all rust processes (orchestration aid) |
| `scripts/prebuild-benches.sh` | Pre-build bench binaries |
| `scripts/prepare-profile-wave.sh` | Profile-wave preparation (referenced by Makefile `ay-prepare-profile-wave`) |
| `scripts/profile-bench-headless.sh` | Headless samply per-bench-entry harness |
| `scripts/seed-worktree.sh` | Worktree seeding |
| `scripts/sync-external-docs.sh` | Sync docs from external sources |
| `scripts/test-tier.sh` | Tiered test runner |
| `scripts/worktree-status.sh` | Worktree status reporter |

The mix is heterogeneous: `.sh`, `.py`, `hooks/`. Concerns mixed: profiling, testing, deployment, orchestration. Lock 13 candidate restructure: separate `scripts/profile/`, `scripts/test/`, `scripts/orchestrate/`, `scripts/deploy/`.

### §4.2 — `xtask/`

Cargo workspace member; the regen entrypoint per Lock 6.

| Path | Purpose |
|---|---|
| `xtask/Cargo.toml` | Manifest |
| `xtask/src/main.rs` | CLI binary entry point |
| `xtask/src/lib.rs` | Lib surface |
| `xtask/src/regen.rs` | Regen command |
| `xtask/tests/` | Tests |

### §4.3 — `server/`

| Path | Purpose |
|---|---|
| `server/bbnf-lsp` | Compiled LSP binary (committed to repo per Makefile `build-lsp`) |

This is a binary artefact in the repo, copied by `make build-lsp`. Per `feedback_clean-regen-discipline`, generated/built artefacts checked into source are fault-shaped. The binary should live in the build output, NOT in the repo. Disposition: ABROGATE-DELETE (or move to `target/release/` consumed by extension build).

### §4.4 — `extension/`

VS Code extension.

| Path | Purpose |
|---|---|
| `extension/package.json` | Extension manifest |
| `extension/src/extension.ts` | TypeScript entry — spawns the LSP binary |
| `extension/syntaxes/` | TextMate grammar |
| `extension/icons/` | Icons |
| `extension/server/` | Stub for LSP server |
| `extension/dist/` | Build output |
| `extension/bbnf-language-support-1.0.{3,5}.vsix` | Pre-packaged VSIX files (at workspace top-level of extension) |
| `extension/esbuild.mjs` | Build script |
| `extension/language-configuration.json` | Standard VS Code config |
| `extension/node_modules/` | npm dependencies |

The two `.vsix` files at `extension/` top level are committed binary releases; per repo hygiene, releases should live in CI artefacts, not in source. Disposition: ABROGATE-DELETE (or .gitignore + leave-but-untrack).

### §4.5 — `playground/`

Vue 3 + Monaco playground.

| Path | Purpose |
|---|---|
| `playground/package.json`, `playground/vite.config.ts` | Vite config |
| `playground/playwright.config.ts` | Playwright e2e config |
| `playground/index.html` | Entry HTML |
| `playground/src/{App.vue, main.ts, components/, composables/, demos/, lib/, router/, views/, wasm/, assets/}` | Vue source tree |
| `playground/e2e/`, `playground/dist/`, `playground/public/`, `playground/node_modules/` | E2E + build output + npm |
| `playground/DESIGN.md` | Design notes |
| `playground/tsconfig.json`, `playground/tsconfig.node.json`, `playground/env.d.ts` | TS config |

### §4.6 — `wasm/`

Excluded from Cargo workspace per `Cargo.toml` line 3 `exclude = ["wasm"]`.

| Path | Purpose |
|---|---|
| `wasm/Cargo.toml`, `wasm/Cargo.lock` | Standalone crate |
| `wasm/src/lib.rs` | wasm-pack entry |
| `wasm/src/{analysis,gorgeous,lsp,vm}.rs` | WASM bindings to analysis, gorgeous, lsp, IR vm |
| `wasm/bench/` | Standalone bench harness |
| `wasm/pkg/`, `wasm/pkg-node/`, `wasm/pkg-node-debug/` | wasm-pack outputs (committed) |

The committed `pkg*/` outputs are build artefacts; per `feedback_clean-regen-discipline`, they shouldn't live in source. Disposition: ABROGATE-DELETE (move to `playground/src/wasm/` consumption point with `.gitignore` for outputs).

### §4.7 — `data/`

Benchmark fixture data.

| Path | Files |
|---|---|
| `data/{canada,citm_catalog,data,data_supermaxx,data_xl,twitter}.json` | JSON benchmark inputs |
| `data/json/` | JSON sub-fixtures |

These are bench fixtures; KEEP. Lock 13 does not forbid data dirs.

### §4.8 — `grammar/`

Grammar source files.

| Path | Files |
|---|---|
| `grammar/bbnf/`, `grammar/bnf/`, `grammar/ebnf/`, `grammar/json/`, `grammar/google-sheets/`, `grammar/css/{l4,pretty.bbnf}`, `grammar/misc/{csv,math}.bbnf`, `grammar/tests/` | Per-grammar `.bbnf` source files |

Aligned with `[workspace.metadata.bbnf.grammars]` table. KEEP.

---

## §5 — Sibling-repo inventory (read-only audit)

The user's memory + project notes name several sibling repos:

### §5.1 — `docs/precepts/` (submodule)

URL: `git@github.com:mkbabb/precepts.git` per `.gitmodules`. Houses cross-project process/style/voice guidance. KEEP-AS-IS-AND-PIN.

### §5.2 — `parse-that` (TS sister)

External GitHub repo `mkbabb/parse-that` per README. The TS combinator library that BBNF compiles down to. Coupling: BBNF's TS backend (planned BD tranches) emits parse-that-shaped combinators. DEFER — no workspace-internal coupling at present; sibling-repo-status.

### §5.3 — `csc411` (CSP solver lineage)

Per memory item `csp-solver-crate`: "Generalized CSP solver in csc411 repo; Rust+Python co-located; patches into bbnf-lang". The current `crates/csp-solver/` is the workspace path-dep; its upstream lives in a separate repo. Coupling: present-but-stable (workspace path-dep until promoted to registry).

### §5.4 — `bbnf-buddy` (procedural SVG mascot)

Per memory item `bbnf-buddy`: "Procedural SVG mascot: continuous-stem b with tail expression, morphing B↔b planned". External repo. Coupling: zero technical coupling; brand only.

### §5.5 — `gorgeous` (sister; per README)

Per README line 162 `Formatting uses [gorgeous](https://github.com/mkbabb/gorgeous) (WASM)`. Sister repo. Coupling: workspace member (`crates/gorgeous/`). Lock 12 archives the workspace copy.

### §5.6 — `pprint` (sister; per README)

Per README line 166. Sister repo. Coupling: documentation reference only.

---

## §6 — Audit corpora inventory

`audit/` directory at workspace root holds the standing audit corpus:

| Group | Files | Purpose |
|---|---|---|
| Codebase audit (8 lanes + synth) | `HARDENING-2026-05-03-{01..08}-*.md` + `HARDENING-SYNTHESIS-2026-05-03.md` | First-pass codebase audit |
| Plan-hardening audit (8 lanes + synth) | `HARDENING-PLAN-2026-05-03-{01..08}-*.md` + `HARDENING-PLAN-SYNTHESIS-2026-05-03.md` | Audit of the plan-set drafted from the codebase audit |
| Census/modules/sketch | `CENSUS-2026-05-03.md`, `MODULES-2026-05-03.md`, `RESTART-SKETCH-2026-05-03.md`, `SOTA-2026-05-03.md` | Mechanical census + module-by-module + restart sketch + SOTA survey |
| Phase-4 synthesis | `PHASE-4-SYNTHESIS-2026-05-03.md` | Phase-4 cross-tranche verification + Lock-14 enforcement |
| Restart suite | `audit/restart/PASS-A,PASS-B,PASS-C-2026-MM-DD.md` (in flight) + `audit/restart/per-agent/...` | This restart suite |

### §6.1 — `audit/archives/`

Probe: directory does not appear to exist. Older audit sets must be elsewhere or absent. (The PASS-C directive lists `audit/archives/` but inventory shows no such dir.)

---

## §7 — Commit-chain inventory (per-era)

Total commits at HEAD = `2,621` (per `git log --oneline | wc -l`). Origin is `1,724` commits ahead of master. Per the archaeology doc Era VI is in flight at 2026-04-22; commits since have continued to land (Phase-4 directive, Lock 14, BA-restart, this suite — visible in `git log --oneline | head -20`). The total has grown ~700+ commits past the archaeology snapshot.

Per-era estimates (from commit-prefix grep + archaeology numbers + post-archaeology drift):

| Era | Letters | Approx commits | Pattern visible |
|---|---|---:|---|
| I (TextMate prelude) | none | ~25 | Earliest ~2023-03 |
| II (monorepo scaffold) | none | ~264 | Pre-tranche — 2026-02 → 03 |
| III (optimiser substrate) | F-W | ~280 | Single-commit tranches |
| IV (tape-first) | X, Y, Z, AA-AU | ~185 tranche-tagged | First tranche-tagged commit `a3fadf56` Tranche F |
| V (DTA/PSI rut) | AV, AW-I/II/III/IV/V, AX | ~572 tranche-tagged | The 1000-commit DTA arc |
| VI (restart) | AY-I/II/III, AZ-I/II/III/IV, B0-B7, BA-BD | ~1095 tranche-tagged | Active tranches; ~700 added post-archaeology snapshot |

Era VI sub-letter pattern (per plan-doc inventory):
- AY-I, AY-II-I, AY-III — restart waves
- AZ-I (CLASSIFIER-UNIFICATION + RESEARCH + audit/), AZ-II, AZ-III, AZ-IV — continuation
- B0-B7 — prelude annexes (B0+B1 documented in archaeology; B2-B7 added post-snapshot)
- BA-BD — restart-target tranches (BA/BB/BC archived as `pre-restart-*`; new BA/BB/BC/BD just drafted)

Era VI alone has ~38 tranche-letter directories, 3-5× the entire prior letter count; this is the inflation visible in `docs/tranches/` (49 subdirs).

### §7.1 — Per-era summary tags

| Era | Headline | Disposition for restart |
|---|---|---|
| I | LSP + TextMate prelude | Pre-restart provenance; archaeological |
| II | Monorepo scaffold + IR crate | Pre-tranche; archaeological |
| III | Optimiser substrate (CSP, e-graph, regex HIR, NodeId) | Era III is the foundational, per archaeology Part D techniques 1, 2, 4 |
| IV | Tape-first + AU baseline | Tape now severed (Lock 1); AU is dead per Lock 8 |
| V | DTA/PSI rut + interpreter deletion + view layer | The "1000-commit fault" arc; substrate severed at AX.W0b |
| VI | Restart waves + prelude annexes + Phase-4 + greenfield | In-flight; this suite governs the next pivot |

### §7.2 — Load-bearing-vs-archaeology test (preliminary)

Era III's surviving code (CSP, e-graph, regex HIR, NodeId, IndexMap determinism) is at HEAD per archaeology Part D. Era IV's tape substrate is GONE per Lock 1 + AY-I.W1 column revert. Era V's DTA interpreter is GONE per AX.W0b. Era VI is in flight.

Of 2,621 commits, the working-tree-load-bearing fraction is bounded above by Era VI + Era III + remnant Era II scaffold ≈ ~1,400 commits. The other ~1,200+ are archaeology — specifically all of Era IV's tape-first arc + Era V's DTA arc which were both removed during their respective close ceremonies.

This bears directly on the C.6 commit-chain decision: more than half the chain is dead substrate by the project's own attestation.

---

## §8 — Per-grammar fixture / data inventory

Per workspace metadata `[workspace.metadata.bbnf.grammars]` lines 19-29: 9 grammars (bbnf, json, css_l4, css_pretty, google_sheets, ebnf, bnf, csv, math). Each has:
- A `.bbnf` source file under `grammar/`
- A generated runtime under `crates/core/src/runtime/<g>/`
- A bench fixture under `data/`
- A test fixture under `crates/core/tests/`

The bench fixtures (`data/canada.json`, `data/citm_catalog.json`, `data/twitter.json`, etc.) are fixture-data; KEEP.

---

## §9 — Top-line counts

| Surface | Count |
|---|---:|
| Workspace crates listed in `Cargo.toml` | 14 |
| Pass-C-scoped crates (analysis, lsp, ser, gorgeous) | 4 |
| `docs/` top-level subdirectories (excl. `precepts/`) | 14 |
| `docs/tranches/` immediate children | 49 |
| `audit/` top-level files | 22 |
| `scripts/` top-level files | 15 |
| `extension/` top-level (excl. node_modules) | ~12 |
| `playground/` top-level (excl. node_modules) | ~14 |
| `wasm/` top-level (excl. node_modules) | ~8 |
| Sibling repos named in memory | 5 (parse-that, csc411, bbnf-buddy, gorgeous, pprint) |
| Total commits at HEAD | 2,621 |
| Unpushed commits ahead of origin/master | 1,724 |

---

## Closing

The Pass-C scope spans heterogeneous surfaces: archived crates, LSP infrastructure, ~50 doc subdirs, ~15 scripts, three frontends (extension, playground, wasm), and the 2,621-commit chain. The unifying thread is *peripheral to the parse-codegen-runtime axis* — but each surface participates in the workspace's gestalt. The next agents apply lenses; this agent enumerates the substrate.
