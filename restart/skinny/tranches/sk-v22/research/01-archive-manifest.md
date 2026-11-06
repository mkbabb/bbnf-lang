# SK-V22 R1 — THE ARCHIVE MANIFEST

Date: 2026-07-09. Method: every surface opened; sizes `du`-measured; git reachability verified.
This report dispositions the **entire** four-generation midden — in-repo surfaces, the root npm
layer, the sibling shelf of stale checkouts, and the ~22 worktrees — and proposes the graduated
tree's clean top-level shape, its durable documentation home, and the disk-reclaim total.

Governing correction (PASS-ALPHA): skinny graduates to the **tree of record**; the legacy
`crates/` split-in-place (SK-V21 TW1a) is superseded. Therefore the graduation is not a surgical
prune of master — it is a **cutover**: master's entire git-tracked state is preserved behind ONE
tag, and the working tree is re-founded on skinny's contents. Everything below is framed to that.

> **Load-bearing finding — the SK-V21 dispositions were never executed.** `git tag` returns a
> single tag: `v1.0.5`. None of the SK-V21-planned archival refs exist (`pre-restart-corpus-final`,
> `sk-v19-frozen`, the frozen-oracle tags are all absent). The ADDENDUM's prune manifest is a
> **plan on paper**; SK-V22 must actually create the tags and remove the trees. Treat every
> "already dispositioned" row below as *designed, not done*.

---

## §0 — The four archive mechanics (per-surface vocabulary)

| Mechanism | When to use | Cost | .git growth |
|---|---|---|---|
| **(A) Plain deletion** | Build artifacts, gitignored dirs, prebuilt binaries — zero history value | free | none |
| **(B) Tag + `git rm`** (git-history-only removal) | Git-tracked source that is superseded but worth retaining. Tag pins it; `git rm` clears the working tree; `git log`/`git checkout <tag>` retains forever | cheap | none (commits already exist) |
| **(C) `archive/` subtree** | Only if a surface must stay *visibly browsable* in the working tree post-graduation. **Recommended against** here — tags do the same for free without polluting the clean tree | working-tree weight | none |
| **(D) Separate archive branch** | A coherent body of work you want reachable by name without a tag-per-file — e.g. the whole legacy `crates/` totality as `archive/legacy-crates` | cheap | none |

**No `git filter-repo` / history rewrite.** The `.git` dir is 1.7G; rewriting to reclaim it would
break every tag's promise ("history retains the corpus") and every reflogged worktree. The 1.7G is
the price of the retained record and is paid gladly. Reclaim comes from build artifacts, not history.

**The graduation cutover, mechanically:** tag master `pre-graduation-2026-07-09` (captures *every*
git-tracked surface below in one ref), plus the named sub-tags for navigability, then re-found the
working tree on skinny. Every "(B)" and "(D)" row is subsumed by that single umbrella tag; the
named tags below are the *navigational* refs so a future reader finds a surface without spelunking.

---

## §1 — In-repo surfaces (git-tracked source)

| Surface | Size | What it actually is | SK-V21 status | Disposition |
|---|---|---|---|---|
| `crates/` (13 members) | 18M src | **Legacy totality**: `core`, `ir`, `gorgeous`, `ser`, `simd-scan`, `bootstrap`, `analysis`, `lsp`, `bbnf-path`(+`-ts`), `csp-solver`, `egraph`(+`-derive`). The ~40K-line hand seam skinny ports *from* | Partially owned (TW1a split, per-crate rows in ADDENDUM §1) — **superseded**: no split-in-place | **(D) branch `archive/legacy-crates`** + tag `legacy-totality-final`. Selected pieces (bbnf-path, lsp/vm constellation, egraph/csp, analysis passes) port INTO skinny per R-later; the husk archives whole. `csp-solver/data/` is git-tracked vendored source (`.gitignore` exception) — rides the branch |
| `restart/` | 54M | The 600K-line doc layer: `ARCHITECTURE/MIGRATION/MASTER-PLAN/HANDOFF/README`, `research/`, `locks/`, `corpora/`, `inheritance/`, and **`restart/skinny/`** (the skinny campaign's doc home: BENCH/COMPILER/SUBSTRATE/WORKSPACE/HARDENING/INDEX + `tranches/sk-v3.5..sk-v22`) | README/HANDOFF fold into W0.1 supersession (ADDENDUM §1) | **(B) tag `restart-doclayer-final` + `git rm`** — BUT first **distill** (see §7): `restart/skinny/*` and the SK-V21 41-report corpus become the graduated `docs/`. The raw corpus is tag-retained; the distilled durable docs graduate |
| `restart-archive-2026-05-04/` | 1.6M | A *prior* archival generation (INTERROGATION, legacy-source, tranches) — already dead | newly enumerated | **(B) tag `restart-archive-gen1` + `git rm`** (folds under umbrella tag; no separate ref strictly needed) |
| `docs/` (49 dirs) | 36M | Pre-restart corpus: `tranches/` (49 dirs, 743 files, 14M), `benchmarks/` (220 files), `bbnf/`, `gorgeous/`, `pprint/`, `optimizer/`, `parse-that/`, `cookbook/`, `migration/`, `audit/`, plus **`docs/precepts` submodule** | `docs/tranches`+`docs/benchmarks` → tag `pre-restart-corpus-final` + remove (ADDENDUM §1) | **(B) tag `pre-restart-corpus-final` + `git rm`** the whole dir. **`docs/precepts` submodule stays live** but re-homes: it is a cross-repo shared submodule, keep it wired at the graduated `docs/precepts` path |
| `audit/` (23 files) | 512K | SK-V21-era hardening/census bundle: `CENSUS-2026-05-03`, 8 `HARDENING-*` audits, 8 `HARDENING-PLAN-*`, `SOTA-2026-05-03`, `MODULES`, `RESTART-SKETCH`. Point-in-time restart-planning intel | **newly enumerated** (unowned) | **(B) tag `restart-audit-2026-05-03` + `git rm`**. Lessons already distilled into SPEC; history retains |
| `wasm/` | 33M | Legacy WASM bindings crate — workspace-**excluded**, deps entirely the legacy tree, prebuilt binary from 2026-03-30. Skinny has **zero** wasm bindings | ADDENDUM §2: TW-P rebuild against surviving tree | **(B) tag `legacy-wasm-final` + `git rm`.** Dies with the legacy tree. The greenfield shopwindow rebuilds `bbnf-wasm` against skinny from scratch (concept survives; code does not) |
| `extension/` | 185M | VS Code extension: `src/extension.ts` (thin client), `syntaxes/`, `language-configuration.json`, two `.vsix` (1.0.3, 1.0.5), `esbuild.mjs`, **prebuilt `server/bbnf-lsp` binary**, `dist/`, `node_modules` | newly enumerated | **Split.** `.vsix`/`dist`/`node_modules`/`server/bbnf-lsp` → **(A) delete** (artifacts). `src/`, `syntaxes/`, `language-configuration.json`, `esbuild.mjs`, `package.json` → **(B) tag `vscode-ext-final` + `git rm`** (reusable TextMate grammar + client; re-homes if/when the graduated tree ships an extension) |
| `grammar/` | 184K | **The `.bbnf` grammar fleet** — canonical source: `bbnf/`, `bnf/`, `ebnf/`, `css/l4/` (15 files), `google-sheets/`, `json/`, `misc/`, `tests/`. This is genuine source of record | newly enumerated | **KEEP** — reconcile against skinny's in-tree grammars; one canonical copy graduates to the root `grammar/`. This is the ten-grammar fleet the fleet-oracle consumes |
| `server/` | 7.3M | `.gitkeep` + `bbnf-lsp` (7.6MB Mach-O arm64 **prebuilt binary**) | newly enumerated | **(A) delete** the binary; the dir is a build drop, not source. LSP source lives in `crates/lsp` (archived with legacy, ported later) |
| `scripts/` (16 files) | 120K | Dev tooling: `deploy.sh`, `doctor.sh`, `profile-bench-headless.sh`, `prepare-profile-wave.sh`, `seed-worktree.sh`, `worktree-status.sh`, `iai-compare.sh`, `sync-csp-solver-vendor.sh`, `sync-external-docs.sh`, `kill-all-rust.sh`, `bisect-fastpath.sh`, `test-tier.sh`, `hooks/`, `install-hooks.sh`, `extract_hotspots.py`, `prebuild-benches.sh` | newly enumerated | **Triage.** Profiling/worktree/hooks scripts are load-bearing to SK-V22's profiling leg → **port** the survivors into graduated `scripts/`. Legacy-only (`sync-csp-solver-vendor`, `deploy` for old wasm) archive with their subsystems via **(B)** umbrella tag |
| `data/` | **1.0G** | **Gitignored** test corpora (`bbnf/`, `css/` incl. bootstrap/tailwind/normalize.css, `json/`, `sheets/`). Local-only, not in history | newly enumerated | **(A) trim.** Keep the canonical corpora skinny's oracles/benches actually consume (a few MB); delete the multi-hundred-MB generated stress corpora. Regenerable |

---

## §2 — Root scaffolding surfaces (the non-crate root)

| Surface | Size | What it is | Disposition |
|---|---|---|---|
| `Makefile` | 19K | ~45 targets: `build-lsp/ext/wasm`, `test`, `bench-{json,css,bbnf,sheets,compile}`, `profile`, `regen`, `expand`, `asm`, `release`, plus **campaign-specific `ay-*` (12 targets) and `az-ii-*` legacy tranche targets** wired to `docs/benchmarks/post-AZ-II.json` | **(B) tag + `git rm`.** The `ay-*`/`az-*` targets are dead tranche scaffolding. The graduated tree gets a fresh, minimal `Makefile`/`justfile` for skinny (build/test/bench/regen/profile) — no legacy target sprawl |
| root `package.json` + `package-lock.json` (108K) | — | npm workspace whose sole member is `playground`; the `.gitmodules`-adjacent root npm surface exists **only** to hoist the playground | **(A) delete** both. Dies with the playground (PASS-ALPHA correction #1). Graduated tree has no root npm surface |
| root `node_modules/` | 283M | Hoisted playground deps | **(A) delete** (gitignored, regenerable, orphaned by playground removal) |
| `.profiles/` | 22M | Gitignored profiling output | **(A) delete** |
| `.profile-target/` | 5.2G | Gitignored profiling `CARGO_TARGET_DIR` | **(A) delete** |
| `.playwright-mcp/` | 1.8M | Gitignored Playwright MCP scratch | **(A) delete** |
| `target/` | 33G | Root workspace `CARGO_TARGET_DIR` | **(A) delete** (regenerable) |
| `Cargo.toml`/`Cargo.lock` | 8K/115K | Legacy root workspace manifest (13 crates) | Superseded — skinny's own `[workspace]` manifest (verified: `resolver=2`) becomes root |
| `README.md`, `LICENSE`, `rust-toolchain.toml`, `.rustfmt.toml`, `.github/`, `.vscode/`, `.cargo/`, `.gitignore` | small | Root config | **Rebase** on skinny's equivalents; keep `LICENSE`, a fresh `README`, edition-2024 `rust-toolchain.toml` |

---

## §3 — `playground/` (charter override)

`playground/` (69M, ~10.2K-LOC Vue3/Vite SPA: Monaco → live parse → AST + gorgeous + telemetry;
bound to the workspace-excluded `wasm/` crate with a prebuilt 2026-03-30 binary).

**PASS-ALPHA correction #1 supersedes ADDENDUM §2's TW-P "rebuild the existing SPA."** The
playground **archives outright** — docs stale, to be re-invented greenfield. Only the
"skinny-backed wasm prerequisite" survives *as a concept*, not as code.

Disposition: **(B) tag `playground-spa-final` + `git rm`.** Delete `test-results/` and
`node_modules` (artifacts). The uncommitted `vite.config.ts` alias cleanup (the one dirty tracked
file in the repo, `M playground/vite.config.ts`) is **moot** — it dies with the tree; no need to
adjudicate commit-vs-revert. The greenfield shopwindow is a future, separate concern.

---

## §4 — The sibling shelf (`~/Programming/bbnf-lang-*`, outside the repo)

Verified via `git worktree list`, per-dir `.git` inspection, and reachability checks against master.

| Sibling | Size | What it is | git state | Rescue? | Disposition |
|---|---|---|---|---|---|
| `bbnf-lang-json-threaded-fork` | 3.4G | **Standalone clone** (own `.git` dir), `origin` → local main repo. HEAD `b102aa92e` (sk-v14-w11d threaded-context rejection) | Committed history **is in master** (verified `b102aa92e` reachable). **One uncommitted change**: `+102 lines` in `skinny/crates/parse-that-regex/src/lib.rs` | **Glance-then-drop.** The dirty change is an abandoned threaded-context experiment on a file SK-V21 already marks DELETE (parse-that-regex fork). Skim once; near-certainly discard | **`git worktree remove` N/A** (standalone) → **rm -rf** after skim. ~3.4G reclaimed |
| `bbnf-lang-skv15-profile-279a60646` | 146M | **Registered worktree**, detached `279a60646` | Reachable from master | No | `git worktree remove --force` |
| `bbnf-lang-skv6-candidate7/8/9/10-base` | 4×131M = 524M | **Registered worktrees**, detached (`cff17e385` etc.) | All reachable from master (verified `cff17e385 IN master`) | No | `git worktree remove --force` each |
| `bbnf-skv16-p1` | 629M | **Registered worktree**, detached `5ed43f8e1` | Reachable | No | `git worktree remove --force` |
| `bbnf-lang-skv5-index-validate` | 130M | **Orphan copy — NO `.git`** (plain directory, full source tree) | Not a worktree, not a clone; a raw `cp` | No (source is in master history) | **rm -rf** |
| `bbnf-wt-b5-w3-target` | 3.9G | **NOT a checkout** — a `CARGO_TARGET_DIR` (`ax-iter/debug/release/tmp`). No `.git` | build artifacts | No | **rm -rf** |
| `bbnf-wt-b5-w4-target` | 6.3G | Same — orphan `CARGO_TARGET_DIR` | build artifacts | No | **rm -rf** |
| `bbnf-buddy-wt` | 0B | Empty orphan dir (bbnf-buddy is its own live repo at `~/Programming/bbnf-buddy` — **out of scope**, PASS-ALPHA #2) | empty | No | **rmdir** |

Sibling shelf reclaim: 3.4 + 0.146 + 0.524 + 0.629 + 0.130 + 3.9 + 6.3 = **~15.0G**. Of this, ~10.2G
is the two orphan target dirs (pure artifacts); ~1.4G is registered worktrees; 3.4G is the
standalone fork (skim first). NOT to be confused with the in-repo worktrees below.

> **Out of scope, live siblings (leave untouched):** `~/Programming/bbnf-buddy` (active mascot repo,
> PASS-ALPHA #2), `~/Programming/parse-that` (constellation backbone, PASS-ALPHA #3/#4 — uplift is a
> *separate* track, not archival), `~/Programming/value.js` (backbone consumer), `~/Programming/pprint`.

---

## §5 — The in-repo worktrees (`.claude/worktrees/` + registered)

**`.claude/worktrees/` is the single largest reclaim target: 90G.** 17 entries (6 `agent-*`, 11
`wf_*`), all **locked**, all stamped June 3 (registration) / June 20 (last build), each a **full
checkout with its own build artifacts**. They are stale agent scratch from prior workflow runs.

Reachability audit (`git rev-list --count master..<branch>`):
- 15 of 16 worktree branches are **0 commits ahead of master** — pure scratch, nothing to rescue.
- `sk-v16-W6-o1-checkpoint` (worktree `wf_cda43cd1`): 0 ahead — a named checkpoint already in history.
- **`worktree-wf_bb9dbdf6-c5b-5`: 1 commit ahead** — `f2fe49bbc perf(sk-v16-W6): hoist flat
  StructLayout to LazyLock + wire lightningcss W6 comparator`. A June-3 sk-v16 wave-6 experiment
  **not** on master. **Rescue: tag it `sk-v16-w6-lazylock-experiment` before removal** (cheap
  insurance; near-certainly superseded by SK-V20's re-founding, but the one non-reachable commit
  in the whole worktree fleet).

Disposition: tag the one ahead-commit, then `git worktree remove --force` all 17 + `git worktree
prune` + delete the 16 `worktree-*` branches (of 109 total branches — the bulk are these
scratch refs). Reclaim: **~90G**. This alone dwarfs every other surface.

---

## §6 — The proposed clean top-level shape of the graduated repo

Skinny (its own `[workspace]`, `resolver=2`) becomes root. Target end-state:

```
bbnf-lang/                    # graduated tree of record
├── Cargo.toml / Cargo.lock   # skinny's workspace, edition 2024
├── crates/                   # the graduated implementation (skinny's crates + ported seams)
├── grammar/                  # the .bbnf fleet (canonical source, reconciled to one copy)
├── data/                     # canonical corpora only (trimmed from 1.0G → a few MB)
├── docs/                     # NEW durable doc home (see §7)
│   ├── ARCHITECTURE.md
│   ├── PERFORMANCE-THESIS.md
│   ├── NEGATIVE-RESULTS.md
│   ├── BENCH-PROTOCOL.md
│   ├── EVIDENCE-INDEX.md     # navigational index into the tag-retained corpus
│   ├── precepts/             # the live submodule, re-homed
│   └── tranches/             # ongoing tranche docs (sk-v22 forward)
├── xtask/                    # regen-host (B2's cure, rebuilt against the one generator)
├── scripts/                  # surviving dev/profiling tooling only
├── Makefile / justfile       # minimal: build/test/bench/regen/profile — no ay-*/az-* sprawl
├── README.md / LICENSE / rust-toolchain.toml / .rustfmt.toml / .gitignore / .github/
```

Gone from root: `wasm/`, `extension/`, `server/`, `playground/`, `audit/`, `restart/`,
`restart-archive-2026-05-04/`, the legacy `crates/` (→ `archive/legacy-crates` branch), root
`package.json`/`node_modules`, `.profiles`/`.profile-target`/`.playwright-mcp`, all worktrees.
The greenfield playground/wasm shopwindow, when built, is a *later, separate* surface (likely its
own `apps/` dir or repo) — not carried in the graduation.

---

## §7 — The durable documentation home (restart/ archives too)

The problem: `restart/` (the 600K-line doc layer, incl. `restart/skinny/tranches/sk-v3.5..sk-v22`,
where sk-v22 itself lives) archives — yet tranche docs need a *live* home. Resolution:

- **Distill, don't just delete.** The SK-V21 evidence corpus (research/01-41, prototypes/,
  critiques/, ledgers, GESTALT-SYNTHESIS) and `restart/skinny/*` (BENCH/COMPILER/SUBSTRATE/
  WORKSPACE/HARDENING) are the cached intelligence PASS-ALPHA names. They collapse into **four
  durable root docs** — `docs/ARCHITECTURE.md`, `docs/PERFORMANCE-THESIS.md`,
  `docs/NEGATIVE-RESULTS.md`, `docs/BENCH-PROTOCOL.md` — plus a **`docs/EVIDENCE-INDEX.md`** that
  maps each distilled claim back to its tag-retained source report (so no dead end is re-run).
- **The raw corpus is tag-retained, not carried.** `restart-doclayer-final` + `pre-restart-corpus-final`
  pin the full 600K lines in history; the working tree carries only the distillate.
- **Tranche docs go forward at `docs/tranches/`.** sk-v22 and successors author there. The
  historical `sk-v3.5..sk-v21` tranche dirs are tag-retained (not copied into the clean tree);
  their lessons live in the distilled docs + evidence index.

---

## §8 — Disk-reclaim total

| Bucket | Reclaim | Mechanism |
|---|---|---|
| `.claude/worktrees/` (17 stale locked worktrees) | **90G** | worktree remove + prune |
| skinny `target/` | 57G | delete (regenerable) |
| root `target/` | 33G | delete |
| sibling orphan target dirs (`b5-w3`+`b5-w4`) | 10.2G | rm -rf |
| `.profile-target/` | 5.2G | delete |
| `bbnf-lang-json-threaded-fork` (skim first) | 3.4G | rm -rf |
| registered sibling worktrees (skv15/skv6×4/skv16) | 1.4G | worktree remove |
| `data/` (trim gitignored corpora) | ~1.0G | trim |
| root `node_modules/` | 283M | delete |
| `bbnf-lang-skv5-index-validate` (orphan copy) | 130M | rm -rf |
| `.profiles/` + `.playwright-mcp/` + `server/bbnf-lsp` + ext artifacts | ~215M | delete |

**Total working-tree reclaim ≈ 202G**, of which **~186G is pure regenerable build churn** (worktree
targets, `target/` dirs, node_modules, profiling drops) — safe, immediate, no history impact. The
`.git` dir (1.7G) is **retained deliberately** — it is the archive; no history rewrite.

---

## §9 — What SK-V22 must actually execute (the tag set)

Because the SK-V21 dispositions were never run, SK-V22 creates the full navigational tag set in one
graduation commit sequence:

1. `pre-graduation-2026-07-09` — umbrella: the entire master state, one ref.
2. Navigational sub-tags: `legacy-totality-final` (+ branch `archive/legacy-crates`),
   `restart-doclayer-final`, `pre-restart-corpus-final`, `restart-audit-2026-05-03`,
   `legacy-wasm-final`, `vscode-ext-final`, `playground-spa-final`, `restart-archive-gen1`.
3. `sk-v16-w6-lazylock-experiment` — the one non-reachable worktree commit (`f2fe49bbc`).
4. Then: `git rm` the archived source trees, `git worktree remove --force` + `prune` all 17,
   delete the 16 `worktree-*` branches, `rm -rf` the sibling shelf, delete all build artifacts,
   re-found the working tree on skinny, wire `docs/precepts` submodule at the new path.

## Open questions for synthesis

- **`archive/legacy-crates` branch vs pure tag** for `crates/`: a branch is friendlier for the
  selective port-in work (checkout, cherry-pick a crate) than a tag. Recommend the branch; the
  synthesis should confirm the porting workflow wants it.
- **`grammar/` reconciliation**: does skinny already carry the full `.bbnf` fleet, making the root
  `grammar/` a legacy duplicate, or is root `grammar/` the canonical copy skinny reads? One more
  check needed before "KEEP" is final (R-later, the skinny-lack census, should resolve).
- **`data/` corpus provenance**: which specific corpora the skinny oracles/benches import must be
  enumerated before the 1.0G trim, so the profiling leg's inputs are not deleted.
