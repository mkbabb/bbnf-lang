# Pass C Agent 4 — Architectural Transposition

Date: 2026-05-03. Lens: macro-level restructuring proposals for elegance, simplicity, performance — at the Pass-C surface (analysis, lsp, archived crates, docs, audit, scripts, tools, server, extension, playground, wasm, sibling repos, commit chain).

For each: current state | proposed shape | locks honoured | migration cost.

---

## §1 — `crates/analysis/` and `crates/lsp/` consolidation

### §1.1 — Current state

Two crates: `crates/analysis/` (LSP-shared analysis engine, BBNF-grammar-specific by import per CENSUS §2.3) and `crates/lsp/` (LSP server binary + DAP debug adaptor; depends on `crates/analysis/`).

The split was useful when the LSP binary needed analysis to be standalone-importable (e.g., for tests, for the playground via WASM). The current consumption sites:

- `crates/lsp/` consumes `crates/analysis/` (inevitable)
- `wasm/src/analysis.rs` consumes `crates/analysis/` (WASM bridge)
- `wasm/src/lsp.rs` consumes `crates/lsp/` (WASM bridge — full LSP exposure)

### §1.2 — Lock 14 + CENSUS verdict

`crates/analysis/` imports `bbnf::runtime::bbnf::{BbnfCompoundKind, BbnfView}` — BBNF-grammar-specific. Per Lock 14 verbatim list, `analysis` is a "generic crate" — violation. CENSUS DEFER: rename to `bbnf-analysis` (per-grammar declaration crate).

### §1.3 — Proposed shape

**Option A — rename + co-locate.** `crates/analysis/` → `crates/bbnf-analysis/` (per-grammar declaration crate; Lock 14 honoured). `crates/lsp/` → `crates/bbnf-lsp/` (matching binary name; per-grammar). Both stay separate; both rename.

**Option B — merge into `crates/bbnf-language-server/`.** Single crate under per-grammar naming; carries analysis + LSP + DAP + WASM bindings. Removes the analysis↔lsp boundary that no consumer uses meaningfully (only WASM imports both; the playground could import the merged crate).

**Option C — multi-grammar future.** If other grammars (CSS L4, JSON, etc.) acquire analysis surfaces, separate them: `crates/bbnf-analysis-{bbnf, css_l4, json}/` + `crates/bbnf-language-server/` (generic; consumes per-grammar analyses via dispatch).

### §1.4 — Recommendation

**Option B** (merge) is the elegance choice for the current state — the boundary is internal-only; merging eliminates the cross-crate coupling overhead and the duplicate `Cargo.toml`. **Option C** is the right shape for the future-multi-grammar case but is premature; the current LSP surfaces only BBNF.

Refined recommendation: **Option B — `crates/bbnf-language-server/` consolidates analysis + lsp + dap into a single crate under per-grammar naming.** Provides:
- Lock 14 honoured (per-grammar declaration crate)
- Lock 13 honoured (cohesive single concern)
- Eliminates the `crates/analysis/` ↔ `crates/lsp/` ↔ `wasm/src/{analysis,lsp}.rs` 4-way coupling

Migration cost: ~2 hours mechanical (consolidate crates + update imports). Source preserved verbatim in repo history; Pass-C synthesis ratifies; Master-Plan tranche executes.

---

## §2 — Archived crates: `crates/ser/`, `crates/gorgeous/`, `archive/`

### §2.1 — Current state

`Cargo.toml` line 2 still lists `crates/ser` and `crates/gorgeous`. `archive/` directory does not exist. Lock 12 archive ceremony is unexecuted.

### §2.2 — Proposed shape

Lock 12 verbatim: "ser + gorgeous archive at `archive/<crate>/`, removed from workspace, source preserved". The proposal IS the lock. Migration sequence:

1. Create `archive/` directory at workspace root.
2. `git mv crates/ser archive/ser`; `git mv crates/gorgeous archive/gorgeous`.
3. Edit `Cargo.toml`: remove from `members`.
4. Add `archive/README.md` declaring archive policy: "These crates are preserved verbatim as historical record; they are NOT compiled, NOT tested, and NOT referenced by any active workspace member. Source is preserved per the user's `feedback_no-backward-compat` mandate — historical provenance, not runtime artefact."
5. `cargo check --workspace` passes; `rg 'bbnf-ser|bbnf-gorgeous' crates/ -l` returns zero.

### §2.3 — Disposition: keep-as-historical or delete

Two reasonable answers. The user's `feedback_no-backward-compat` says "no backward compat, always migrate fully". Read narrowly, archive-as-historical is *backward compat* of a sort (it preserves the source). Read broadly, archive-as-historical is *provenance* (it lets future readers see what existed).

The HARDENING-PLAN-PROMPT Lock 11 says "source preserved". The README §What This Suite is NOT says "Not amendment. The prior plan (BA → BD) is superseded, not patched." But superseded preserves; patched mutates.

Recommendation: **keep-as-historical archive**. Archive is provenance, not active code. The user's no-backward-compat applies to *implementation* (no shims, no fallbacks, no legacy code in *active* crates), not to historical preservation. `archive/` is read-only; future readers see what was; no consumer in `crates/` references archive.

A complementary recommendation: **delete `archive/`'s contents at the next major version cut** (e.g., 1.0). At that point the project is mature; the historical record is the git history; the archive directory adds noise. For 0.x development, archive serves as live provenance.

---

## §3 — `docs/` tree restructuring

### §3.1 — Current state

```
docs/
  bbnf/                 (7 files; user-facing language docs)
  parse-that/           (5 files; sister combinator docs)
  performance/          (11 files; perf corpus)
  pprint/               (2 files; pprint sister docs)
  gorgeous/             (1 file; gorgeous sister docs)
  cookbook/             (3 files; recent Phase-4 user docs)
  optimizer/            (1 file; recent Phase-4 user doc)
  migration/            (1 file; recent Phase-4 user doc)
  instructions/         (3 files; project process)
  benchmarks/           (1 file; bench harness spec)
  precepts/             (submodule; READ-ONLY)
  restart/              (6 files; this suite)
  tranches/             (49 children; god directory)
  audit/                (deferred per CENSUS heading)
  GESTALT.md            (top-level)
  HARDENING-AUDIT-PROMPT.md
  HARDENING-PLAN-PROMPT.md
  PHASE-4-DIRECTIVE-2026-05-03.md
  codegen-paths.md
```

14 subdirs + 4 prompt files + 1 GESTALT.md + 1 codegen-paths.md. Concerns mixed.

### §3.2 — Proposed shape

```
docs/
  GESTALT.md                 ← architecture gestalt; standalone
  README.md                  ← project navigation hub (replaces stale top README)
  lang/                      ← language documentation
    bbnf/                    (former docs/bbnf/)
    parse-that/              (former docs/parse-that/)
    pprint/                  (former docs/pprint/)
    gorgeous/                (former docs/gorgeous/)
  perf/                      (former docs/performance/)
    benchmarks.md
    parsing.md
    formatting.md
    overview.md
    timeline.md
    ir-pipeline.md
    automaton-theory-memo.md
    pprint.md
    lsp.md
    regex-codegen.md
    wasm.md
  howto/                     ← user-facing guides
    cookbook/                (former docs/cookbook/)
    optimizer/               (former docs/optimizer/)
    migration/               (former docs/migration/)
  process/                   ← project process artefacts
    precepts/                (submodule unchanged)
    restart/                 (this suite)
    instructions/            (former docs/instructions/)
    tranches/
      A/  B/  C/  D/  E/  F/  G/  H/  I/  J/  ...  ← restart tranche set
      meta-audit/            (project-wide retrospectives)
      archive/
        legacy-Y-BD/         ← all prior letters relocated here
  audit/                     (workspace audit corpus)
    codebase-2026-05-03/
    plan-2026-05-03/
    restart-2026-05-03/
  spec/                      ← (NEW) authoritative specifications
    SPEC.md                  ← master language spec (replaces tranche-tree archaeology as the truth)
    architecture.md          ← post-restart workspace shape
    codegen.md               ← consolidates docs/codegen-paths.md
```

5 immediate children + GESTALT + README. Concerns clean: `lang` (per-language), `perf` (perf corpus), `howto` (user guides), `process` (project process), `audit` (audit corpus), `spec` (master spec).

### §3.3 — Locks honoured

- Lock 13 (no god directories): every level cohesive.
- Lock 8 (surpass SOTA, AU silent): `perf/` re-write removes AU references.
- Lock 1 (tape dead): `lang/`, `perf/`, `howto/`, `spec/` rewrites strip tape.
- Lock 2 (Layout canon): same.

### §3.4 — Migration cost

~4 hours mechanical relocation + ~2-3 days for the docs re-do (most older docs need substantive rewrite). Per Replacement Design (Agent 5), the substantive rewrite is the larger surgery.

---

## §4 — Audit corpora: workspace-root `audit/` vs `docs/audit/`

### §4.1 — Current state

The audit corpus lives at `audit/` (workspace root, sibling of `docs/`). The Pass-C directive C.4 enumerates it. Probe shows no `docs/audit/` exists (despite the directive listing both).

### §4.2 — Proposed shape

Two reasonable shapes:

**A — workspace-root `audit/` (status quo).** Audit is a top-level concern, sibling to `crates/`, `docs/`, `xtask/`. It governs project metabolism; not user-facing.

**B — `docs/audit/`.** Audit corpus is documentation; lives under `docs/`. Cohesive with the rest of the docs tree.

### §4.3 — Recommendation

**A — keep `audit/` at workspace root.** Audits are NOT user-facing; they're project-process artefacts the orchestrator consumes. Co-located with `crates/` is right because the audit *acts on* `crates/`. Locating under `docs/` confuses provenance — readers expect `docs/` to be human-facing.

But: per Lock 13 §13.4 `audit/` itself needs internal restructuring (current 22-file flat layout → grouped by audit wave). The internal structure proposed in §3.2 (`audit/{codebase-2026-05-03, plan-2026-05-03, restart-2026-05-03}/`) is the surgery.

---

## §5 — Sibling repos: parse-that, csc411, bbnf-buddy, gorgeous, pprint

### §5.1 — `parse-that` (TS combinator)

Current: External `mkbabb/parse-that` repo. Couples to BBNF via the TS backend (planned BD tranches).

Proposed: **Stay external until the TS backend lands**. Move to workspace path-dep when BBNF emits parse-that combinators in production. Rename to `parse-that-ts` / `parse-that-rs` if both Rust + TS variants live (parse-that's Rust port already in the workspace).

### §5.2 — `csc411` (CSP solver)

Current: External; per memory `csp-solver-crate`, "Generalized CSP solver in csc411 repo; Rust+Python co-located; patches into bbnf-lang". Workspace has `crates/csp-solver/` as path-dep mirror.

Proposed: **Promote `crates/csp-solver/` to its own repo when API stabilises** (per Lock 11). Currently path-dep is right; future migration is registry promotion.

### §5.3 — `bbnf-buddy` (procedural SVG mascot)

Current: External; brand artefact only. Zero technical coupling.

Proposed: **Stay external**. No coupling to drive merger. Reference from project README brand section.

### §5.4 — `gorgeous`, `pprint` (sister repos)

Current: External; gorgeous mirrored in `crates/gorgeous/` (Lock 12 archives this). pprint referenced in docs only.

Proposed: **External-as-source**. Documentation references the canonical external repos. `crates/gorgeous/` archive renders the workspace copy historical; gorgeous's continued evolution lives in its own repo.

---

## §6 — Workspace top-level files

### §6.1 — `Cargo.toml`

Current: 14 workspace members + `[workspace.metadata.bbnf]` grammar table + `[workspace.metadata.bbnf-strategy]` strategy table + 4 profiles. Lock 12 unexecuted (ser + gorgeous still listed). 

Proposed:
- Remove `crates/ser`, `crates/gorgeous` from `members` (Lock 12).
- Rename `crates/analysis` → `crates/bbnf-analysis` (Lock 14); `crates/lsp` → `crates/bbnf-lsp` or merge into `crates/bbnf-language-server`.
- Confirm `crates/parse-that` listed (currently absent from `members` per inspection).
- Add `[workspace.metadata.bbnf-test-fixtures]` if a test-fixtures crate lands (per Replacement Design Agent 5).

### §6.2 — `README.md`

Current: Stale (claims `rust/` directory; pre-`crates/` reorganisation). Era V or earlier.

Proposed: **Full rewrite**. Use restart-suite vocabulary. Honour STYLE.md. Cover:
- Project framing (BBNF; sonic-rs / lightning-css / simdjson cohesion)
- Workspace layout (current crates/ tree)
- Quick start (build, test, bench)
- Documentation hierarchy (lang/perf/howto/process/audit/spec)
- Sibling repos (parse-that, csp-solver, gorgeous, pprint, bbnf-buddy)
- License + acknowledgements

### §6.3 — `Makefile`

Current: ~420 lines; AY-W5-W7 gate commands couple wave letters into the build system.

Proposed: **Simplify + decouple**. The Makefile should be tranche-letter-free. Lift AY-* targets to `docs/tranches/AY-*/Makefile.gates` (per-tranche orchestration recipes). Top-level Makefile contains: `build, test, bench, profile, doctor, expand, asm, regen, regen-check, install, package, clean, deploy, watch`. ~150 lines.

### §6.4 — `rust-toolchain.toml`

Current: nightly pin `nightly-2026-04-11`; comments self-document.

Proposed: **Honour as-is + move to nightly bisect-on-update procedure** (already documented). KEEP.

### §6.5 — `.gitignore`, `.gitmodules`

Current: standard.

Proposed: **Add `server/`, `wasm/pkg*/`, `extension/*.vsix`, `extension/dist/`, `playground/dist/` to `.gitignore`**. Eliminates committed-build-artefact violations.

### §6.6 — `package.json`, `package-lock.json`, `node_modules/`

Current: workspace top-level — stale or required-by-?

The top-level `package.json` likely came from playground/extension migration history. Verify: is anything at workspace root requiring it?

Proposed: **DELETE if nothing at workspace root requires it**. `extension/`, `playground/` carry their own. If something at workspace root genuinely needs npm (eg release scripts), keep + .gitignore `node_modules/`.

---

## §7 — Tooling: scripts, server, extension, playground, wasm

### §7.1 — `scripts/`

Per Lock 13 §13.3: restructure into 5 subdirs (`profile/`, `test/`, `orchestrate/`, `deploy/`, `hooks/`) + 1 top-level `doctor.sh`.

### §7.2 — `server/bbnf-lsp` (committed binary)

Per Agent 2 §3.1: **DELETE**; .gitignore `server/`; built fresh by extension `make install`.

### §7.3 — `extension/`

Stays. Internal cleanup:
- Delete `bbnf-language-support-1.0.{3,5}.vsix` (release artefacts).
- .gitignore `*.vsix`, `dist/`, `node_modules/`.
- Verify `extension/server/` isn't a stale stub.

### §7.4 — `playground/`

Stays. Internal cleanup:
- .gitignore `dist/`, `node_modules/`.
- Verify `playground/src/wasm/` is wasm-pack output (cleaned during build).
- Honour Vue 3 + Vite + Playwright setup (it's right-shape for an interactive playground).

### §7.5 — `wasm/`

Excluded from workspace per `Cargo.toml`. Stays separate.

Internal cleanup:
- Delete `wasm/pkg/`, `wasm/pkg-node/`, `wasm/pkg-node-debug/` (build outputs).
- .gitignore `pkg*/`.
- Confirm `wasm/src/{analysis,gorgeous,lsp,vm}.rs` reflects post-Lock-14 + post-archive state (gorgeous bridge becomes archive bridge or deletes).

---

## §8 — Commit chain disposition

### §8.1 — Current state

2,621 commits at HEAD. 1,724 unpushed. Five eras (II-VI), with Era IV's tape arc + Era V's DTA arc both deleted from working tree per Lock 1 + AX.W0b.

### §8.2 — Decision matrix

Four options per the Pass-C directive §C.6.B:

**Option 1 — Rewrite to era boundaries.** Squash each era into 1 commit. Result: ~25-30 commits total.
- Pros: clean log; provenance preserved at era granularity; Lock 8 honoured (no AU references survive); commit-ledger bloat removed.
- Cons: per-tranche debugging archaeology lost (e.g., why did `2f7c1bd4` delete EmissionTier? — known only via squashed log entry); cherry-pick from squashed commits non-trivial; force-push cost.

**Option 2 — Squash all.** One greenfield commit replaces all 2,621.
- Pros: maximum cleanliness; Lock 14 implicit (no historical grammar-coupling lurks); fresh start.
- Cons: ALL provenance erased; the user's investment in 945 commits of unpushed engineering becomes invisible; debugging by archaeology impossible; future readers see only restart point.

**Option 3 — Keep verbatim.** All 2,621 preserved + one annotation commit at HEAD.
- Pros: zero data loss; debugging archaeology fully accessible; user attribution preserved per memory `accurate-perf-narrative` and `perf-breakthrough-accuracy`.
- Cons: 2,621 commits is operationally heavy (clone time, log readability, GitHub-side rendering); commit-prefix noise (745 `docs:`, 521 `feat:` per `git log --oneline | awk '{print $2}' | sort | uniq -c`); the 4869 unique commit prefixes inflate the surface.

**Option 4 — Hybrid.** Squash legacy (pre-Era-VI: Y through AT, ~700 commits) into one commit; keep Era V + Era VI verbatim.
- Pros: Era V's DTA archaeology survives (the user's most consequential lessons live there); pre-tranche scaffolding gone; mid-balance.
- Cons: era-boundary squash is judgement-call; "what is Era V's first commit" requires citation.

### §8.3 — Recommendation

**Option 3 (keep verbatim) + branch reset, NOT history rewrite.**

Reasoning:

1. **Provenance preservation.** The user's prior memory items (`accurate-perf-narrative`, `perf-breakthrough-accuracy`) explicitly cite specific commit SHAs (`2f7c1bd4` AQ.5, `a206b962` AX.W0b, `c1e86ab3` AW-V W3 close, `bd563c1d` AY-II.W0', etc.). These are load-bearing references. Squashing them away breaks the project's own attribution.

2. **The commits ARE the archaeology.** The 1,000-commit DTA arc is the project's primary lessons-learned corpus. Era V's "novel levers compound only when they share a substrate AND a demonstrable floor" lesson is *in the commits*. Deleting the commits deletes the lesson.

3. **Operational cost is bearable.** 2,621 commits is large but not unmanageable. GitHub renders it. `git log --oneline | tail -...` works. Clone time is one-time.

4. **Branch reset, not history rewrite.** Better than rewriting the chain: open a new branch (`master-greenfield-2026-05-03` or just keep `master` and tag `pre-restart-master-2026-05-03`). The greenfield restart's first commit becomes the parent-of-all-future-work. Provenance is preserved as ancestry; new history is clean.

5. **Honour `feedback_accurate-perf-narrative`.** "Performance docs must reconstruct actual timeline from commits; don't fabricate or embellish." Squash erases the timeline.

### §8.4 — Operational sequence

```bash
# 1. Tag the current state for provenance.
git tag pre-restart-2026-05-03 master
git push origin pre-restart-2026-05-03

# 2. Push current master to origin (closes the 1,724-unpushed gap).
git push origin master

# 3. Open a new branch starting at the current master HEAD.
git checkout -b master-greenfield-2026-05-03

# 4. Land the restart prelude:
#    a. Lock 12 archive ceremony (ser + gorgeous → archive/).
#    b. Master Plan + Tranche A through J stubs.
#    c. SPEC.md + architecture.md + this restart-suite output committed.
#    d. README.md rewrite.
#    e. .gitignore additions.
#    f. Architectural transposition (analysis/lsp consolidation, docs/ restructure, scripts/ restructure).

# 5. After hardening pass + ratification, the user decides: 
#    - Option A: master-greenfield-2026-05-03 becomes new master 
#      (force-push origin master; pre-restart-2026-05-03 tag preserves ancestry).
#    - Option B: keep both branches; master continues; greenfield evolves separately.
```

### §8.5 — Post-restart commit discipline

Per `feedback_no-metalanguage-docs`, `bg-then-monitor`, `single-cargo-per-target`, `dispatch-hard-cap`, `triumvirate-discipline`, etc. — the lessons are absorbed. Future commits:

- Subject prefix `tranche(<letter>): <surgery>` for plan work; `feat(<crate>): <surgery>` for implementation; `docs(<area>): <surgery>` for doc work.
- Subject body cites runtime evidence (per `clean-instrumentation`).
- No metalanguage in commit subjects (no "AY-II.W0'.a" formats post-restart).
- HARD CAPs honoured per memory.

---

## §9 — `docs/precepts/` submodule disposition

### §9.1 — Current state

Submodule per `.gitmodules`: `git@github.com:mkbabb/precepts.git`. Pinned by SHA. Contains `instructions/STYLE.md`, `instructions/CONSUMING.md`, `instructions/LESSONS-LEARNED.md`.

### §9.2 — Proposed shape

**Honour the existing pin.** Submodule is the right separation (cross-project sharing per `feedback_general-infra-crates` + `feedback_wasm-subcrate-pattern`). Don't change.

Possible refinement: **promote `instructions/LESSONS-LEARNED.md` to submodule root or split** if lessons grow project-specific (they're cross-project per definition). Current state honours separation.

Verdict: **KEEP as-is**.

---

## §10 — Top-line transposition recommendations

| Item | Current | Proposed | Locks honoured | Cost |
|---|---|---|---|---|
| `crates/{analysis, lsp}` | 2 crates | merge → `crates/bbnf-language-server/` | 13, 14 | ~2h |
| `crates/{ser, gorgeous}` | workspace members | move → `archive/` | 11, 12 | ~1h |
| `archive/` | unprovisioned | created on Lock 12 ceremony | 12 | trivial |
| `docs/` tree | 14 subdirs + flat top | `lang/`, `perf/`, `howto/`, `process/`, `audit/`, `spec/` + GESTALT + README | 13 | ~4h relocate + 2-3d rewrite |
| `audit/` | workspace-root, 22 files flat | restructure into 3 wave-subdirs | 13 | ~30min |
| `docs/tranches/` | 49 children flat | letter-tranches under `archive/legacy-Y-BD/`; new A-J at top | 13 | ~30min |
| `Makefile` | 420 lines, AY-coupled | ~150 lines, tranche-free; AY gates relocate | 13, KISS | ~2h |
| `README.md` | stale (`rust/` dir) | full rewrite per restart vocabulary | STYLE | ~3h |
| `server/bbnf-lsp` | committed binary | DELETE; .gitignore | clean-regen | trivial |
| `extension/*.vsix` | committed releases | DELETE; .gitignore | clean-regen | trivial |
| `wasm/pkg*/` | committed wasm-pack outputs | DELETE; .gitignore | clean-regen | trivial |
| `package.json` (top-level) | unclear purpose | DELETE if no consumer | KISS | trivial |
| `scripts/` | 15 files flat | 5 subdirs + `doctor.sh` top | 13 | ~1h |
| Sibling repos | mostly external | confirm parse-that disposition; promote csp-solver to registry when API stabilises | 11 | ongoing |
| `docs/precepts/` | submodule | KEEP | 12 | none |
| Commit chain (2,621) | linear | tag + branch reset; preserve verbatim | accurate-perf-narrative | tag + branch ops |

The single most consequential transposition is the **commit-chain disposition** — Option 3 (keep verbatim + branch reset) is recommended; the user ratifies. Second most consequential is the **`docs/` tree restructure** (most surface area; requires subsequent docs re-do). Third is **`crates/{analysis, lsp}` → `crates/bbnf-language-server/`** (Lock 14 redress).
