# Pass C Agent 5 — Replacement Design

Date: 2026-05-03. Lens: design new facilities to replace abrogated items + propose brand-new items the absence of which is felt across Pass-C surfaces.

For each: name, purpose, location, public surface, replaces-which-abrogated-item or names-the-felt-absence.

---

## §1 — `docs/spec/SPEC.md` — master specification

### §1.1 — Felt absence

Today's "specification" is distributed:
- `docs/bbnf/grammar-syntax.md` documents the BBNF grammar
- `docs/bbnf/operators.md` documents operators
- `grammar/bbnf/bbnf.bbnf` IS the bootstrap source (the grammar that defines the grammar)
- The 14 locks live in `docs/HARDENING-PLAN-PROMPT.md` § Gestalt
- The precepts live in `docs/precepts/instructions/{STYLE,CONSUMING,LESSONS-LEARNED}.md`
- The pipeline lives implicitly in `audit/MODULES-2026-05-03.md` Synthesis §3
- The IR contract lives implicitly in `audit/HARDENING-SYNTHESIS-2026-05-03.md`
- The architecture lives in `docs/GESTALT.md`

A reader looking for *the BBNF specification* has no canonical artefact. The closest is `audit/MODULES-2026-05-03.md` — but that's an audit, not a spec.

### §1.2 — Proposed shape

`docs/spec/SPEC.md` — single authoritative master spec. Sections:

1. **Language** — BBNF grammar surface (operators, directives, regex, imports). Cite `grammar/bbnf/bbnf.bbnf` as the bootstrap; cite `docs/lang/bbnf/grammar-syntax.md` for tutorial.
2. **Pipeline** — 17-step pipeline ordering (per MODULES.md Synthesis §3). Source bytes → typed value.
3. **IR** — IR shape; the contract that codegen consumes. Layout vocabulary (Lock 2).
4. **Locks** — the 14 architectural commitments. Settled, immutable until change-of-spec.
5. **Backends** — per-backend lower (Rust at 1.0; TS + WASM at later release).
6. **API surface** — `parse(input)`, `parse_in(input, &bump)`, `parse_owned(input)`, `Document::get<T>(path)`, `pointer![…]` macro (Lock 9).
7. **Per-grammar facilities** — host-fns, recogniser plug-ins, declarative grammar metadata.

### §1.3 — Replaces

- Distributed knowledge across `docs/HARDENING-PLAN-PROMPT.md`, `audit/MODULES-2026-05-03.md`, `docs/GESTALT.md`, etc.
- Tranche-tree-as-truth pattern (current state: each tranche claims its own truth).

### §1.4 — Locks honoured

- Lock 13 (cohesive single source of truth)
- Lock 14 (the spec is grammar-agnostic; per-grammar facts live in per-grammar metadata blocks)

### §1.5 — Estimated size

~1,000-1,500 lines. Five-day surgery; lands as the first execution-tranche deliverable.

---

## §2 — `docs/spec/architecture.md` — post-restart workspace shape

### §2.1 — Felt absence

Same problem: workspace shape is documented across `Cargo.toml` comments, `audit/MODULES-2026-05-03.md`, `docs/migration/bc-core-split.md`, README.md (stale), and `docs/GESTALT.md`. No single artefact says "here is the workspace; here is what each crate does".

### §2.2 — Proposed shape

`docs/spec/architecture.md`:

1. **Workspace layout** — every crate, with a one-paragraph mission. Cite per-crate `lib.rs` doc-comment as source of truth.
2. **Crate dependency DAG** — visual + tabular.
3. **Pipeline phases** — which crate owns which phase.
4. **Cross-crate contracts** — IR (`bbnf-ir`), regex HIR (`bbnf-regex`), CSP solver (`csp-solver`), e-graph (`egraph`), path (`crates/path/`).
5. **Per-grammar declaration crates** — `bbnf-bootstrap`, `bbnf-grammar`, future `css-l4-grammar` etc.
6. **Frontend integrations** — `crates/bbnf-language-server/` + WASM bridge + extension + playground.

~600-800 lines.

---

## §3 — `docs/howto/migration/2026-restart.md` — migration record

### §3.1 — Felt absence

The user's migration history is distributed across tranche records + audit corpus. A new contributor asking "how did we get here?" has no entry point.

### §3.2 — Proposed shape

`docs/howto/migration/2026-restart.md` — narrative:

1. **The restart context** — Phase-1 audit, Phase-2 plan-set, Phase-3 plan hardening, Phase-4 specification depth, Phase-5 restart suite (this).
2. **What was abrogated** — Era IV's tape arc; Era V's DTA arc; the legacy AU bench corpus; the substrate-first/consumer-later anti-pattern.
3. **What survives** — Era III's optimiser (CSP, e-graph, regex HIR); Era V's view layer + parity harnesses; Era VI's prelude (B0+B1).
4. **What's new** — fresh tranche set (A-J); SPEC.md; architecture.md; cohesive `docs/` tree; consolidated `crates/bbnf-language-server/`.
5. **Operational** — the commit chain disposition (verbatim + branch tag); the workspace `Cargo.toml` updates; `.gitignore` additions.

This document is metalanguage-by-design (it IS the migration record); per Agent 2 §2.2, lives under `audit/` if rigorous-archive, OR under `docs/howto/migration/` if user-facing-onboarding.

Recommended location: **`docs/howto/migration/2026-restart.md`** — user-facing context for new contributors. Audit-side detail stays in `audit/restart/`.

~500 lines.

---

## §4 — `crates/bbnf-test-fixtures/` — workspace-internal test fixtures

### §4.1 — Felt absence

Per `docs/HARDENING-PLAN-PROMPT.md` and the worktree-fixture-carry mechanism in Phase-3, test fixtures are scattered:
- `crates/core/tests/*.rs` (per-crate)
- `data/*.json` (workspace-root)
- `grammar/tests/` (grammar-source-side)
- `docs/tranches/<letter>/audit/` (per-tranche bench captures)

Per `feedback_test-output-to-file`, large outputs route to files, not re-invocation. Per `feedback_no-inline-tests`, all tests in `tests/` not inline. The implicit shape is: a workspace-internal crate carrying the fixture surface that every test crate path-deps.

### §4.2 — Proposed shape

`crates/bbnf-test-fixtures/`:

```
crates/bbnf-test-fixtures/
  Cargo.toml                            ← path-dep'd by every test crate
  src/
    lib.rs                              ← public surface
    json.rs                             ← JSON fixtures (twitter, citm, canada, ...)
    css.rs                              ← CSS fixtures (bootstrap, normalize, tailwind)
    sheets.rs                           ← Sheets fixtures
    bbnf.rs                             ← BBNF self-host fixtures
    snapshots/                          ← shared golden snapshots
      json/{twitter, citm, canada}.snap
      css/{bootstrap, normalize, tailwind}.snap
      ...
```

Public surface: `pub fn load_fixture(name: &str) -> &'static [u8]`, `pub fn snapshot_path(name: &str) -> &'static str`, etc. Single source of truth; consumers `path-dep` via `Cargo.toml [dev-dependencies]`.

### §4.3 — Replaces

- `data/` workspace-root JSON files (relocate to `crates/bbnf-test-fixtures/data/`).
- Per-grammar fixture functions in `crates/ir/tests/structural_alphabet_extended.rs` (per CENSUS §2.2 row 6) — relocate fixture builders.
- Implicit "every test crate hand-rolls its own fixture loader" pattern.

### §4.4 — Locks honoured

- Lock 13 (cohesive fixture concern, single crate).
- Lock 14 (fixtures are per-grammar but live in a single crate organised by grammar — config + per-grammar source, not per-grammar code in generic crates).

### §4.5 — Estimated size

~600 LOC source + ~5-10 MB fixtures. Migration: 2 days mechanical.

---

## §5 — `crates/bbnf-cli/` — stable user-facing CLI

### §5.1 — Felt absence

Today, command-line entry points are:
- `xtask` (workspace-internal regen + dev tasks)
- `cargo bench`, `cargo test`, `cargo nextest run` (cargo native)
- `bbnf-lsp` (the LSP server binary)

There is no user-facing `bbnf` CLI. A user wanting to "parse my grammar; emit Rust; check correctness" runs `cargo xtask regen` — workspace-internal command. The xtask shape is right for workspace dev; wrong for end-user.

### §5.2 — Proposed shape

`crates/bbnf-cli/`:

```
crates/bbnf-cli/
  Cargo.toml                            ← bin = ["bbnf"]
  src/
    main.rs                             ← clap-based dispatcher
    parse.rs                            ← bbnf parse <grammar.bbnf> <input.json>
    emit.rs                             ← bbnf emit <grammar.bbnf> --target rust|ts|wasm
    check.rs                            ← bbnf check <grammar.bbnf> (lint + analyse)
    lsp.rs                              ← bbnf lsp (delegates to bbnf-language-server)
    fmt.rs                              ← bbnf fmt <grammar.bbnf> (gorgeous-style)
```

Public surface: `bbnf <subcommand> <args>`. Distribution: `cargo install bbnf-cli`.

### §5.3 — Replaces

- ad hoc xtask invocations from outside the workspace.
- The current shape where users "run the LSP via VS Code extension only".

### §5.4 — Locks honoured

- Lock 6 (xtask remains workspace-internal regen; CLI is end-user).
- Lock 13 (cohesive CLI concern; clap subcommand split honours per-concern).
- Lock 14 (CLI dispatches by grammar metadata, not match-on-name).

### §5.5 — Estimated size

~800-1200 LOC. 3-4 day surgery. Lands at user-facing 1.0 release.

---

## §6 — `crates/bbnf-py/` (or `bbnf` PyPI package) — Python binding

### §6.1 — Felt absence

Per memory `csp-solver-crate`: "Generalized CSP solver in csc411 repo; Rust+Python co-located". The CSP solver lineage carries Python bindings; BBNF grammar tooling could honour the same shape.

### §6.2 — Proposed shape (deferred)

`crates/bbnf-py/` PyO3 cdylib OR external `bbnf` PyPI package consuming `bbnf-cli` via subprocess. Either is valid; PyO3 gives in-process performance; subprocess gives loose coupling.

Recommendation: **DEFER to post-1.0**. Until a Python consumer materialises (the user has CSP-solver Python bindings; BBNF doesn't yet have a Python use-case), this is speculative work. The shape is right; the timing isn't.

---

## §7 — `docs/` re-do operational sequence

### §7.1 — Felt absence

Per Agent 4 §3, the docs tree restructures. Per Agent 2 §1-§3, the docs need substantive rewriting (banned-words sweep, AI-writing-sign cleanup, Lock 1/2/7/8 violation removal). The combined surgery is the largest operational artefact in the restart.

### §7.2 — Proposed shape

A migration sequence:

1. **Wave 1 — Restructure (mechanical).** Create new directory layout per Agent 4 §3.2. `git mv` files into place. Update internal cross-references. ~4 hours.

2. **Wave 2 — Rewrite older user-facing docs (substantive).** `docs/lang/{bbnf, parse-that, pprint, gorgeous}/`, `docs/perf/*` — banned-words sweep, AI-writing-sign cleanup, Lock 1/8 violation removal (replace AU references with sonic-rs/lightning-css/simdjson; replace tape vocabulary with current substrate). ~3-5 days.

3. **Wave 3 — Write new spec docs.** `docs/spec/SPEC.md`, `docs/spec/architecture.md`. ~3-5 days.

4. **Wave 4 — Write new migration record.** `docs/howto/migration/2026-restart.md`. ~1 day.

5. **Wave 5 — Tranche archive relocation.** Move every letter-tranche to `docs/process/tranches/archive/legacy-Y-BD/`. ~30 minutes.

6. **Wave 6 — Validation.** STYLE.md compliance scan; metalanguage scan; lock-adherence scan. ~1 day.

Total: 8-13 days for the full docs re-do. The mechanical wave 1 lands first; substantive waves 2-4 parallelise; wave 5 trivial; wave 6 closes.

### §7.3 — What docs survive unchanged

| Doc | Reason |
|---|---|
| `docs/precepts/` (submodule) | Read-only by Pass C |
| `audit/*` | Audit corpora are archaeological by design |
| `docs/process/tranches/archive/legacy-Y-BD/*` | Archived tranches are historical; preserve verbatim |
| `docs/process/restart/*` | This suite; preserve |
| `docs/process/tranches/{A,B,...,J}/*` | New tranche set; written fresh |

### §7.4 — What docs rewrite

| Doc | Surgery |
|---|---|
| `docs/bbnf/*` → `docs/lang/bbnf/*` | banned-words sweep; metalanguage strip; Lock 8 (AU references); voice calibration |
| `docs/parse-that/*` → `docs/lang/parse-that/*` | same |
| `docs/pprint/*` → `docs/lang/pprint/*` | same |
| `docs/gorgeous/*` → `docs/lang/gorgeous/*` | same; or DELETE if Lock 12 archive deletes gorgeous-as-active |
| `docs/performance/*` → `docs/perf/*` | full rewrite — Lock 8 (AU silent; SOTA-only); Lock 1 (tape silent); Lock 2 (Layout canon) |
| `docs/cookbook/*` → `docs/howto/cookbook/*` | metalanguage strip; align with new vocabulary |
| `docs/optimizer/pratt-simd-detection.md` → `docs/howto/optimizer/pratt-simd-detection.md` | Lock 2 vocabulary check |
| `docs/migration/bc-core-split.md` → `docs/howto/migration/bc-core-split.md` | full rewrite OR DELETE (the BC core split was pre-restart; the new migration record absorbs it) |
| `docs/codegen-paths.md` | DELETE or absorb into `docs/spec/architecture.md` §pipeline |
| `docs/HARDENING-AUDIT-PROMPT.md` | KEEP (recently relocated; honours STYLE) |
| `docs/HARDENING-PLAN-PROMPT.md` | KEEP (Phase-3 prompt; archaeological) |
| `docs/PHASE-4-DIRECTIVE-2026-05-03.md` | RELOCATE to `audit/` (it's an audit prompt) |
| `docs/GESTALT.md` | UPDATE — rewrite to reflect post-restart shape; voice-calibrated |
| `README.md` (top-level) | full rewrite per Agent 4 §6.2 |

### §7.5 — What docs delete

| Doc | Reason |
|---|---|
| `docs/codegen-paths.md` | Absorbed into `docs/spec/architecture.md` |
| `docs/migration/bc-core-split.md` | Pre-restart artefact; absorbed into the new migration record |

---

## §8 — Commit-chain rewrite plan (the operational artefact)

### §8.1 — Per Agent 4 §8

Recommendation: **Option 3 (keep verbatim) + branch reset, NOT history rewrite**.

### §8.2 — Operational sequence (replicates Agent 4 §8.4 with finer detail)

```bash
# Pre-flight: verify clean state.
git status           # clean working tree
cd docs/precepts && git status && git rev-parse HEAD  # submodule clean + pinned
cd ../..

# 1. Tag current HEAD as the pre-restart provenance anchor.
git tag pre-restart-2026-05-03 master

# 2. Push everything (closes the 1,724-unpushed gap; preserves provenance on remote).
git push origin master
git push origin pre-restart-2026-05-03

# 3. Open a new branch starting at current master.
#    (Optional: rename master itself; either way the tag preserves the ancestry.)
git checkout -b master-greenfield-2026-05-03 master

# 4. Land the restart prelude as a sequence of small, focused commits:

#    Commit 1 — Lock 12 archive ceremony.
git mv crates/ser archive/ser
git mv crates/gorgeous archive/gorgeous
# Edit Cargo.toml (remove from members); update any internal references.
git add -A && git commit -m "chore(workspace): archive ser + gorgeous per Lock 12"

#    Commit 2 — Crates/{analysis, lsp} consolidation.
# (per Agent 4 §1; merge into crates/bbnf-language-server/)
git mv crates/analysis crates/bbnf-language-server
git mv crates/lsp/* crates/bbnf-language-server/   # merge tree
# Update Cargo.toml + every internal import.
git add -A && git commit -m "refactor(workspace): consolidate analysis + lsp into bbnf-language-server per Lock 14"

#    Commit 3 — docs/ tree restructure (mechanical).
# (per Agent 4 §3; git mv to new layout)
git mv docs/bbnf docs/lang/bbnf
# ... (every directory move)
git add -A && git commit -m "docs(restructure): relocate to lang/perf/howto/process/audit/spec layout per Lock 13"

#    Commit 4 — docs/tranches/ archive.
git mv docs/process/tranches/{Y,Z,AA,AB,AC,AE,AF,AG,AH,AI,AJ,AK,AL,AM,AN,AO,AP,AQ,AR,AS,AT,AU,AV,AW,AX,AY-I,AY-II-I,AY-III,AZ-I,AZ-II,AZ-III,AZ-IV,B0,B1,B2,B3,B4,B5,B6,B7,BA,BB,BC,BD,W,X} docs/process/tranches/archive/legacy-Y-BD/
git add -A && git commit -m "docs(tranches): archive legacy Y-BD letter set under restart"

#    Commit 5 — README rewrite + GESTALT update.
# (full rewrites per Agent 4 §6.2 + Agent 5 §7.4)
git add README.md docs/GESTALT.md
git commit -m "docs(README, GESTALT): rewrite per restart vocabulary"

#    Commit 6 — .gitignore additions; delete committed artefacts.
git rm -f server/bbnf-lsp extension/bbnf-language-support-1.0.{3,5}.vsix
git rm -rf wasm/pkg wasm/pkg-node wasm/pkg-node-debug
# Edit .gitignore.
git add -A && git commit -m "chore(.gitignore): exclude server/, wasm/pkg*/, *.vsix; remove committed artefacts"

#    Commit 7 — SPEC.md + architecture.md + migration record + new tranche stubs.
# (per Agent 5 §1, §2, §3 + restart README §Outputs Aggregated)
git add docs/spec/ docs/howto/migration/ docs/process/tranches/{A,B,...,J}/
git commit -m "docs(spec, tranches): land master spec, architecture, migration record, fresh tranche A-J set"

#    Commit 8 — Master Plan + per-pass restart audits.
# (this suite's outputs get ratified)
git add audit/restart/MASTER-PLAN-2026-05-03.md
git commit -m "audit(restart): land master plan synthesis"

# 5. Push the new branch.
git push -u origin master-greenfield-2026-05-03

# 6. Hardening pass per docs/restart/HARDENING.md.

# 7. Cutover decision (USER):
#    - Option A: master-greenfield-2026-05-03 becomes new master.
#      git checkout master-greenfield-2026-05-03
#      git branch -m master-greenfield-2026-05-03 master
#      git push --force-with-lease origin master
#      (pre-restart-2026-05-03 tag preserves the prior chain.)
#    - Option B: keep both; master continues; greenfield evolves separately.
```

### §8.3 — Provenance preservation guarantees

- Tag `pre-restart-2026-05-03` survives every cutover; the old chain is reachable via `git checkout pre-restart-2026-05-03`.
- The new chain has a small, focused commit set (~8 commits) covering the restart prelude.
- Future tranche execution lands per-tranche commits on top of the new chain.
- `accurate-perf-narrative` is honoured because the tag preserves the old chain.

### §8.4 — What the alternative (Option 1: rewrite to era boundaries) would cost

If the user prefers Option 1, the operational sequence is significantly heavier:

1. Identify era-boundary commits per archaeology (Era II → III → IV → V → VI; sub-eras within VI).
2. `git rebase --interactive --onto <root> <era-start>~..<era-end>` — squash each era to one commit.
3. Per-era commit message embeds the era's archaeology summary.
4. Force-push the rewritten chain.

Cost: ~1-2 days operational work + provenance loss (per-commit SHA references in memory items break).

Verdict: **NOT recommended**. Stick with Option 3 + branch reset.

---

## §9 — Top-line replacement design summary

| Replacement | Replaces | Locks honoured | Cost |
|---|---|---|---|
| `docs/spec/SPEC.md` | distributed spec across HARDENING-PLAN-PROMPT, MODULES, GESTALT | 13, 14 | ~5d |
| `docs/spec/architecture.md` | Cargo.toml comments + GESTALT.md + audit/MODULES | 13 | ~3d |
| `docs/howto/migration/2026-restart.md` | distributed migration history | STYLE | ~1d |
| `crates/bbnf-test-fixtures/` | data/, per-test fixture builders | 13, 14 | ~2d |
| `crates/bbnf-cli/` | ad hoc xtask use | 6, 13, 14 | ~3-4d (DEFER to 1.0) |
| `crates/bbnf-py/` | (none; speculative) | 14 | DEFER post-1.0 |
| `docs/` re-do (waves 1-6) | older docs with violations | 1, 2, 7, 8, 13, STYLE | 8-13d |
| Commit-chain disposition (Option 3) | the 2,621-commit chain | accurate-perf-narrative, 8 (AU silent) | tag + branch ops + ~8 prelude commits |

The largest substantive deliverables are `docs/spec/SPEC.md` (the new master spec) and the `docs/` re-do (the largest migration). The most consequential governance decision is the commit-chain disposition — Option 3 is recommended.
