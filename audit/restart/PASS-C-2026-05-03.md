# Pass C — Periphery + Tooling + Docs + Commit Chain (Synthesis)

Date: 2026-05-03. Synthesiser orchestrator: Pass C.

This synthesis composes the six lenses (Inventory, Idiomaticity, Lock-adherence, Architectural Transposition, Replacement Design, Cross-cut + Commit Chain) into a single ratified verdict ledger covering ~1/3 of the project: analysis, lsp, archived crates (ser, gorgeous), docs (excluding `precepts/`), audit corpora, scripts, tools, server, extension, playground, wasm, workspace top-level files, sibling repos, and **the commit chain itself**.

The commit-chain disposition is the most consequential governance output of the suite. The docs re-do plan is the largest operational artefact. The greenfield mandate applies across everything.

Per-agent reports anchor every claim:
- `audit/restart/per-agent/pass-c-agent-1-inventory.md`
- `audit/restart/per-agent/pass-c-agent-2-idiomaticity.md`
- `audit/restart/per-agent/pass-c-agent-3-lock-adherence.md`
- `audit/restart/per-agent/pass-c-agent-4-architectural-transposition.md`
- `audit/restart/per-agent/pass-c-agent-5-replacement-design.md`
- `audit/restart/per-agent/pass-c-agent-6-cross-cut.md`

---

## §1 — Pass C scope verdict (top-line)

| Pass-C surface | Bucket | Surgery |
|---|---|---|
| `crates/analysis/` | KEEP-MODIFY | rename to `crates/bbnf-analysis/` OR merge into `crates/bbnf-language-server/` per Lock 14 |
| `crates/lsp/` | KEEP-MODIFY | rename `crates/bbnf-lsp/` OR merge per above |
| `crates/ser/` | ABROGATE-MOVE | move to `archive/ser/`; remove from workspace `members` per Lock 12 |
| `crates/gorgeous/` | ABROGATE-MOVE | move to `archive/gorgeous/`; remove from workspace `members` per Lock 12 |
| `archive/` (provisioned by Lock 12) | NEW | created on first archive ceremony |
| `docs/precepts/` (submodule) | KEEP | unchanged; submodule pin |
| `docs/tranches/{Y..BD}/` | ABROGATE-MOVE | relocate every letter to `docs/tranches/archive/legacy-Y-BD/`; new A-J letter set lands at top |
| `docs/tranches/archive/pre-restart-{BA,BB,BC}/` | KEEP | already archived by `9dde66ab chore(tranches): archive pre-restart BA/BB/BC` |
| `docs/tranches/meta-audit/` | KEEP-MODIFY | maintain as project retrospectives; absorb post-restart meta-audit waves |
| `docs/bbnf/`, `docs/parse-that/`, `docs/pprint/`, `docs/gorgeous/` | KEEP-MODIFY | relocate to `docs/lang/{bbnf, parse-that, pprint, gorgeous}/`; mechanical sweep + rewrite per STYLE.md + Lock 1, 2, 7, 8 |
| `docs/performance/` | KEEP-MODIFY | relocate to `docs/perf/`; full rewrite (Lock 8 — AU silent; Lock 1 — tape silent; Lock 2 — Layout canon) |
| `docs/cookbook/`, `docs/optimizer/`, `docs/migration/` | KEEP-MODIFY | relocate under `docs/howto/{cookbook, optimizer, migration}/`; Phase-4 origin; mostly-honoured |
| `docs/instructions/` | KEEP | relocate under `docs/process/instructions/`; Era V/VI active; honoured |
| `docs/benchmarks/` | KEEP | bench harness spec |
| `docs/restart/` | KEEP | this suite; lives at `docs/process/restart/` post-restructure |
| `docs/HARDENING-AUDIT-PROMPT.md`, `docs/HARDENING-PLAN-PROMPT.md` | KEEP | recently relocated; honour STYLE; cite `docs/process/` post-restructure |
| `docs/PHASE-4-DIRECTIVE-2026-05-03.md` | ABROGATE-MOVE | relocate to `audit/plan-2026-05-03/` (it's an audit prompt, not user-facing doc) |
| `docs/codegen-paths.md` | ABROGATE-DELETE | absorbed into `docs/spec/architecture.md` |
| `docs/GESTALT.md` | KEEP-MODIFY | rewrite to reflect post-restart shape |
| `audit/` (workspace root) | KEEP-MODIFY | restructure into `audit/{codebase-2026-05-03, plan-2026-05-03, restart-2026-05-03}/` per Lock 13 |
| `audit/restart/` | KEEP | this suite; preserve |
| `Cargo.toml` | KEEP-MODIFY | execute Lock 12 + crate consolidation; verify `[workspace.metadata.bbnf]` complete |
| `Cargo.lock` | KEEP | regen after Cargo.toml edits |
| `README.md` | KEEP-MODIFY | full rewrite per Pass-C Agent 4 §6.2 |
| `Makefile` | KEEP-MODIFY | simplify to ~150 lines; relocate AY-* gate targets to per-tranche files |
| `rust-toolchain.toml` | KEEP | nightly pin honoured |
| `.gitignore` | KEEP-MODIFY | add `server/`, `wasm/pkg*/`, `*.vsix`, `extension/dist/`, `playground/dist/` |
| `.gitmodules` | KEEP | precepts pin |
| `package.json`, `package-lock.json`, `node_modules/` (top-level) | ABROGATE-DELETE if no consumer | per Agent 4 §6.6; verify before delete |
| `scripts/` | KEEP-MODIFY | restructure into `profile/`, `test/`, `orchestrate/`, `deploy/`, `hooks/` + top-level `doctor.sh` |
| `xtask/` | KEEP | regen entrypoint per Lock 6 |
| `server/bbnf-lsp` (committed binary) | ABROGATE-DELETE | .gitignore `server/`; built fresh per `make install` |
| `extension/` | KEEP-MODIFY | delete `*.vsix` releases; .gitignore `dist/`; verify `extension/server/` not stale |
| `playground/` | KEEP-MODIFY | .gitignore `dist/`; honour Vue 3 + Vite + Playwright; build outputs cleaned |
| `wasm/` | KEEP-MODIFY | delete `pkg*/` outputs; .gitignore; reflect post-Lock-14 + post-archive state |
| `data/` | KEEP-MODIFY | relocate to `crates/bbnf-test-fixtures/data/` per Replacement Design Agent 5 §4 |
| `grammar/` | KEEP | grammar source files honour `[workspace.metadata.bbnf]` |
| Sibling repos (parse-that, csc411, bbnf-buddy, gorgeous-external, pprint-external) | KEEP | external; brand and architectural separation |
| Commit chain (2,621 commits) | KEEP-VERBATIM + BRANCH-RESET | tag `pre-restart-2026-05-03`; new branch `master-greenfield-2026-05-03`; greenfield prelude (~8 commits); cutover after hardening |

---

## §2 — Locks honoured / violated table (Pass-C surfaces only)

| Lock | Pass-C verdict | Surgery sequence |
|---|---|---|
| 1 (tape dead) | violated in user-facing docs | sweep `rg -ni 'TapeRec\|TapeBuilder\|TapeCursor\|tape-?first\|columnar' docs/{bbnf,performance,parse-that,pprint,gorgeous,cookbook,optimizer,migration}/`; rewrite each match |
| 2 (Layout canon) | violated in user-facing docs | sweep `rg -wn 'TypeDesc\|StructLayout\|TypeMap\|type-projection\|type-collapsing\|schema synthesis\|LayoutDesc' docs/`; rewrite |
| 3 (cursor + byte-skip) | silent-must-add | confirm `docs/cookbook/path-macro.md` states empty-path elision invariant |
| 4 (per-domain optim) | honoured | — |
| 5 (IR + per-backend) | silent-must-add | `docs/cookbook/`, `docs/optimizer/`, `docs/migration/` should reference IR-as-contract |
| 6 (xtask source emit) | honoured | — |
| 7 (consolidated path crate) | violated in docs | sweep `bbnf-path` references in docs; replace with `crates/path/`, `crates/path-core/`, `crates/path-ts/` |
| 8 (surpass SOTA, AU silent) | violated in docs/performance/* + docs/migration/bc-core-split.md + various legacy | full rewrite of `docs/perf/*`; replace AU references with sonic-rs / lightning-css / simdjson |
| 9 (slice-borrow primary) | silent-must-add | confirm `docs/cookbook/lifetime-surfaces.md` reflects three-way split |
| 10 (Pratt + SIMD auto-detect) | silent-must-add | audit `crates/analysis/src/directives/hints.rs`; ZERO `@pratt`/`@simd` entries |
| 11 (path-deps incubating) | honoured-mostly | confirm parse-that disposition |
| 12 (ser + gorgeous archive) | **violated-with-blocking-rec** | execute archive ceremony as first restart step |
| 13 (no god directories) | violated for `docs/`, `docs/tranches/`, `scripts/`, `audit/` | restructure per §3 below |
| 14 (full grammar generalisation) | violated for `crates/analysis/` | rename to `crates/bbnf-analysis/` OR merge into `crates/bbnf-language-server/` |

The most consequential lock failures in Pass-C: Lock 12 (blocking; archive ceremony unexecuted), Lock 13 (god directories), Lock 14 (analysis/lsp grammar-coupling in generic-named crates).

---

## §3 — Architectural transposition ratified

### §3.1 — `crates/{analysis, lsp}` consolidation → `crates/bbnf-language-server/`

Per Pass-C Agent 4 §1: merge analysis + lsp + dap into single per-grammar crate `crates/bbnf-language-server/`. Honours Lock 13 + Lock 14.

Migration: ~2 hours mechanical (consolidate crates + update imports). Source preserved; dispatch packet lands in Master-Plan tranche.

### §3.2 — `crates/{ser, gorgeous}` archive ceremony (Lock 12)

Per Pass-C Agent 4 §2 + Pass-C Agent 3 §Lock 12. Execute as the FIRST restart-suite ratification step.

Sequence:
```
git mv crates/ser archive/ser
git mv crates/gorgeous archive/gorgeous
# Edit Cargo.toml: remove from members; update internal references
git add -A && git commit -m "chore(workspace): archive ser + gorgeous per Lock 12"
```

Disposition: keep-as-historical archive (per Agent 4 §2.3); future maintainers may delete at 1.0 cut.

### §3.3 — `docs/` tree restructure

Per Pass-C Agent 4 §3.2. Target shape:

```
docs/
  GESTALT.md
  README.md (project navigation)
  lang/{bbnf, parse-that, pprint, gorgeous}/
  perf/
  howto/{cookbook, optimizer, migration}/
  process/{precepts, restart, instructions, tranches}/
  tranches → process/tranches  (alias or directly under process)
  audit → workspace-root /audit/
  spec/{SPEC.md, architecture.md, codegen.md}
```

5 immediate children + GESTALT + README. Concerns clean.

### §3.4 — `docs/tranches/` archive

Per Pass-C Agent 4 §3 + Agent 5 §7.4. Move all letter-tranches (Y, Z, AA-AT, AU, AV, AW, AX, AY-I, AY-II-I, AY-III, AZ-I/II/III/IV, B0-B7, BA, BB, BC, BD, W, X) under `docs/process/tranches/archive/legacy-Y-BD/`. Resulting tree: `archive/, A/, B/, ..., J/, meta-audit/`.

### §3.5 — `audit/` restructure

Per Pass-C Agent 3 §Lock 13.4. Restructure into `audit/{codebase-2026-05-03, plan-2026-05-03, restart-2026-05-03}/`.

### §3.6 — `Makefile` simplification

Per Pass-C Agent 2 §7. Lift AY-W5-W7 gate commands out of Makefile; relocate per-tranche orchestration recipes to `docs/process/tranches/AY-{I,II,III}/Makefile.gates` (or wave-spec embedded shell snippets). Top-level Makefile remains generic, ~150 lines, tranche-letter-free.

### §3.7 — `scripts/` restructure (Lock 13)

Per Pass-C Agent 3 §Lock 13.3:

```
scripts/
  doctor.sh                   ← top-level (host probe)
  profile/{prepare-wave.sh, bench-headless.sh, extract-hotspots.py, iai-compare.sh}
  test/{tier.sh, prebuild-benches.sh, bisect-fastpath.sh}
  orchestrate/{seed-worktree.sh, worktree-status.sh, kill-all-rust.sh}
  deploy/{deploy.sh, sync-external-docs.sh}
  hooks/{pre-commit, install-hooks.sh}
```

### §3.8 — Committed-artefact deletions

Per Pass-C Agent 2 §3:
- `server/bbnf-lsp` — DELETE; .gitignore `server/`
- `extension/bbnf-language-support-1.0.{3,5}.vsix` — DELETE; .gitignore `*.vsix`
- `wasm/{pkg, pkg-node, pkg-node-debug}/` — DELETE; .gitignore `wasm/pkg*/`

### §3.9 — Workspace top-level cleanup

Per Pass-C Agent 4 §6:
- `Cargo.toml` — execute Lock 12 + crate consolidation
- `README.md` — full rewrite per restart vocabulary
- `package.json`/`package-lock.json`/`node_modules/` (workspace top-level) — DELETE if no consumer; verify before delete

---

## §4 — Replacement design ratified

### §4.1 — `docs/spec/SPEC.md` — master specification

Per Pass-C Agent 5 §1. Single authoritative master spec replacing distributed knowledge across `docs/HARDENING-PLAN-PROMPT.md`, `audit/MODULES-2026-05-03.md`, `docs/GESTALT.md`. ~1,000-1,500 lines. 5-day surgery; lands as the first execution-tranche deliverable.

Sections: Language | Pipeline | IR | Locks | Backends | API surface | Per-grammar facilities.

### §4.2 — `docs/spec/architecture.md` — post-restart workspace shape

Per Pass-C Agent 5 §2. ~600-800 lines. 3-day surgery. Cite per-crate `lib.rs` doc-comment as source of truth.

### §4.3 — `docs/howto/migration/2026-restart.md` — migration record

Per Pass-C Agent 5 §3. ~500 lines. 1-day surgery. User-facing context for new contributors.

### §4.4 — `crates/bbnf-test-fixtures/` — workspace-internal test fixtures

Per Pass-C Agent 5 §4. New crate carrying fixture surface. ~600 LOC source + ~5-10 MB fixtures. 2-day migration mechanical. Path-dep'd by every test crate via `[dev-dependencies]`.

### §4.5 — `crates/bbnf-cli/` — stable user-facing CLI

Per Pass-C Agent 5 §5. **DEFER to 1.0 release.** Until then, `cargo xtask regen` + LSP via extension cover the dev-loop and end-user-via-extension cases. Estimated 800-1200 LOC; 3-4 day surgery when the time comes.

### §4.6 — `crates/bbnf-py/` — Python binding

Per Pass-C Agent 5 §6. **DEFER to post-1.0**. No Python consumer materialised; speculative work.

### §4.7 — `docs/` re-do (waves 1-6)

Per Pass-C Agent 5 §7.2. Six waves; total 8-13 days for the full docs re-do.

| Wave | Surgery | Cost |
|---|---|---|
| 1 — Restructure (mechanical) | git mv files into new layout per Agent 4 §3.2 | ~4 hours |
| 2 — Rewrite older user-facing docs | `docs/lang/*`, `docs/perf/*` — banned-words sweep, AI-writing-sign cleanup, Lock 1/8 violation removal | 3-5 days |
| 3 — Write new spec docs | `docs/spec/SPEC.md`, `docs/spec/architecture.md` | 3-5 days |
| 4 — Write new migration record | `docs/howto/migration/2026-restart.md` | 1 day |
| 5 — Tranche archive relocation | `git mv` letter-tranches under `archive/legacy-Y-BD/` | ~30 minutes |
| 6 — Validation | STYLE.md compliance scan; metalanguage scan; lock-adherence scan | 1 day |

---

## §5 — Idiomaticity ratified

### §5.1 — STYLE.md compliance

Per Pass-C Agent 2 §1: full sweep across `docs/{bbnf, parse-that, pprint, gorgeous, performance, cookbook, optimizer, migration}/`:

- Banned words (`delve, tapestry, testament, underscore, pivotal, robust, leverage, navigate, unleash, foster, align with, ever-evolving, bustling, showcase, landscape, intricate, in conclusion, in the realm of, it's worth noting`) — replace per STYLE guidance.
- Em-dash discipline — replace " — " with "—" in older docs.
- Epanorthosis — sweep `rg -n 'not just .* but\b\|not .* but,' docs/`; rewrite.
- Outline-shaped AI closers, vague attribution, promotional warmth — visual scan + rewrite.

CI gate: `scripts/style-check.sh` greps the banned list; exits non-zero on any hit in tracked `.md` outside `docs/precepts/` and `audit/`.

### §5.2 — `no-metalanguage-docs` distinction

- `docs/tranches/**/*.md` — exempt by design (tranche provenance).
- `audit/*.md` — exempt by design (archaeological).
- `docs/lang/*`, `docs/perf/*`, `docs/howto/*`, `docs/spec/*`, `README.md`, `docs/GESTALT.md` — fault if metalanguage. Sweep: `rg -n 'AY-?[A-Z]?\.W\d\|post-AU\|Era [IVX]+\|"the user"\|earlier this tranche\|BA\.W\d' docs/{lang, perf, howto, spec}/`. Rewrite each match to standalone prose.

### §5.3 — `clean-regen-discipline` violations resolved

Per §3.8 above. Build-artefact deletions land as one commit; `.gitignore` updates land alongside.

### §5.4 — `archaic-diction-as-voice` calibration

Per Pass-C Agent 2 §6. User-facing `docs/lang/*` re-calibrated to register-appropriate voice; tranche/audit docs preserve mild-lilt; this restart suite deploys archaic-permissive register per HARDENING-AUDIT-PROMPT §V1.

---

## §6 — Cross-cut summary

Per Pass-C Agent 6 §A:

| Cross-cut | Disposition |
|---|---|
| Docs ↔ tranches | honoured-by-design |
| Audit ↔ tranches | honoured-by-design |
| Archive ↔ active | violated for `Cargo.toml` (Lock 12 unexecuted); honoured otherwise |
| Sibling ↔ workspace | honoured-mostly; gorgeous coupling resolves with Lock 12 archive |

The single archive-coupling fault is resolved by the Lock 12 ceremony.

---

## §7 — The commit chain — disposition decision

### §7.1 — Headline numbers

```
git log --oneline | wc -l            → 2621
git log origin/master..HEAD | wc -l  → 1724  (unpushed)
```

Per the archaeology snapshot at 2026-04-22 the chain was 1,842 commits on master + 945 unpushed. Since then, ~700+ commits have landed. The current state is 2,621.

### §7.2 — Per-era commit table (Pass-C Agent 6 §B.2)

| Era | Letters | Commits | Era summary | Substrate at HEAD |
|---|---|---:|---|---|
| I (TextMate prelude) | none | ~25 | LSP + TextMate grammar; 3-year hiatus | dead — replaced |
| II (monorepo scaffold) | none formal | ~264 | monorepo structure + IR crate bootstrap | partial — `crates/ir/` survives |
| III (optimiser substrate) | F-W | ~280 | CSP, e-graph, regex HIR, NodeId, IndexMap determinism | LIVE — every Era III pivot survives |
| IV (tape-first) | X, Y, Z, AA-AU | ~185 | tape-first codegen; AU baseline | DEAD — tape severed (Lock 1); columns reverted (AY-I.W1); AU silent (Lock 8) |
| V (DTA/PSI rut) | AV, AW-I/II/III/IV/V, AX | ~572 | The 1,000-commit DTA arc; substrate-first/consumer-later | DEAD — DTA interpreter deleted at AX.W0b (~78K LOC reclaim); shape emitter substrate retained as view layer |
| VI (restart) | AY-I/II/III, AZ-I/II/III/IV, B0-B7, BA-BD | ~1,095 (incl. ~700 post-archaeology) | restart waves + prelude annexes + Phase-4 + greenfield restart | LIVE — current state |

### §7.3 — Load-bearing test result

Estimated load-bearing: ~1,395 of 2,621 commits (53%).
Estimated archaeology: ~1,225 of 2,621 (47%).

### §7.4 — Decision matrix outcome

Per Pass-C Agent 6 §B.5:

| Option | Recommendation |
|---|---|
| 1 — Rewrite to era boundaries | NOT recommended (per-tranche archaeology lost; commit-SHA references in memory items break) |
| 2 — Squash all to one greenfield commit | NOT recommended (ALL provenance erased; user's investment invisible) |
| 3 — Keep verbatim + branch reset | **RECOMMENDED** (every memory-cited SHA preserved; archaeology accessible; `accurate-perf-narrative` honoured) |
| 4 — Hybrid (squash legacy + keep recent) | NOT recommended (boundary judgement-call) |

### §7.5 — Recommended disposition: Option 3 (keep verbatim + branch reset)

#### Justification

1. **Provenance preservation is non-negotiable.** Memory items `accurate-perf-narrative`, `perf-breakthrough-accuracy`, the entire archaeology document — these cite specific commit SHAs. Squashing them away breaks the project's own attribution.

2. **The commits ARE the lessons.** Era V's failure mode is *commitable* — each commit carries the per-substrate-build reasoning. Future readers can trace via `git log -- crates/tape/src/dta.rs` to see when DTA was added, when deactivated, when deleted. Squashing erases the trace.

3. **Operational cost is manageable.** 2,621 commits clones in seconds; renders in GitHub; `git log --oneline | head/tail` operates instantly. The "noise" framing is misperception — every commit is data.

4. **Branch reset is cheap.** Tag the current state; open a new branch; land the 8-commit greenfield prelude; cutover when ratified. Provenance is in the tag.

5. **Future commit discipline absorbs failure modes.** Per memory items: HARD CAPs, single-cargo-per-target, bg-then-monitor, dispatch-hard-cap, triumvirate-discipline, no-metalanguage-docs in commit subjects, templated-bodies-rejected. The post-restart commits will be cleaner; the legacy noise stays as legacy noise.

#### Operational sequence

```bash
# Pre-flight
git status                                               # clean working tree
cd docs/precepts && git status && git rev-parse HEAD     # submodule clean + pinned
cd ../..

# 1. Tag current HEAD as the pre-restart provenance anchor.
git tag pre-restart-2026-05-03 master

# 2. Push everything — closes the 1,724-unpushed gap; preserves provenance.
git push origin master
git push origin pre-restart-2026-05-03

# 3. Open a new branch starting at current master.
git checkout -b master-greenfield-2026-05-03 master

# 4. Land the restart prelude as ~8 focused commits:
#    Commit 1 — Lock 12 archive ceremony (ser + gorgeous → archive/).
#    Commit 2 — crates/{analysis, lsp} consolidation → crates/bbnf-language-server/.
#    Commit 3 — docs/ tree restructure (mechanical relocation).
#    Commit 4 — docs/tranches/ archive (legacy Y-BD relocate).
#    Commit 5 — README + GESTALT rewrite per restart vocabulary.
#    Commit 6 — .gitignore additions; delete committed build artefacts.
#    Commit 7 — SPEC.md + architecture.md + migration record + tranche A-J stubs.
#    Commit 8 — Master Plan + per-pass restart audit synthesis.

# 5. Push the new branch.
git push -u origin master-greenfield-2026-05-03

# 6. Hardening pass per docs/restart/HARDENING.md.

# 7. Cutover decision (USER):
#    - Option A: master-greenfield-2026-05-03 → master.
#      git branch -m master-greenfield-2026-05-03 master
#      git push --force-with-lease origin master
#      (pre-restart-2026-05-03 tag preserves the prior chain.)
#    - Option B: keep both; master continues; greenfield evolves separately.
```

### §7.6 — Provenance preservation guarantees

- Tag `pre-restart-2026-05-03` survives every cutover; the old chain is reachable via `git checkout pre-restart-2026-05-03`.
- New chain has small, focused commit set (~8 commits) covering the restart prelude.
- Future tranche execution lands per-tranche commits on top of new chain.
- `accurate-perf-narrative` honoured because the tag preserves the old chain.

### §7.7 — Post-restart commit discipline

1. Subject prefixes: `tranche(<letter>):`, `feat(<crate>):`, `refactor(<area>):`, `docs(<dir>):`, `chore(workspace):`, `audit(<area>):`.
2. No metalanguage in subjects (no "AY-II.W0'.a" formats post-restart). Tranche letters yes; sub-wave depth no.
3. Bodies cite runtime evidence per `feedback_clean-instrumentation`. Templated bodies rejected.
4. HARD CAPs honoured per `dispatch-hard-cap`.
5. Worktree integration via cherry-pick per LESSONS-LEARNED 2026-04-30.
6. Empty returns are failed dispatches per LESSONS-LEARNED.

---

## §8 — Punch list (operational; ratifiable)

Ordered. Each item names target + edit + verification.

### §8.1 — Pre-restart blocking (must land before any other tranche)

1. **Lock 12 archive ceremony.**
   - Edit: `git mv crates/ser archive/ser; git mv crates/gorgeous archive/gorgeous`. Edit `Cargo.toml` to remove from `[workspace] members`. Update internal references via `rg -l 'bbnf-ser\|bbnf-gorgeous' crates/` and remove or migrate to dev-dep.
   - Verify: `cargo check --workspace` passes; `rg 'crates/ser\|crates/gorgeous\|bbnf-ser\|bbnf-gorgeous' crates/ -l` returns zero.
   - Commit: `chore(workspace): archive ser + gorgeous per Lock 12`.

2. **Commit-chain provenance anchor.**
   - Edit: `git tag pre-restart-2026-05-03 master`; `git push origin master`; `git push origin pre-restart-2026-05-03`.
   - Verify: `git tag --list pre-restart-2026-05-03`; `git log origin/master..HEAD --oneline | wc -l` returns 0.

### §8.2 — Restart prelude (commits 2-8)

3. **`crates/{analysis, lsp}` consolidation → `crates/bbnf-language-server/`.**
   - Edit: merge crates; update `Cargo.toml` `[workspace] members`; update every `use bbnf_analysis::` / `use bbnf_lsp::` import.
   - Verify: `cargo check --workspace` passes; `rg 'crates/analysis\|crates/lsp\|bbnf_analysis\|bbnf_lsp' crates/ -l` returns zero outside the merged crate.
   - Commit: `refactor(workspace): consolidate analysis + lsp into bbnf-language-server per Lock 14`.

4. **`docs/` tree restructure (mechanical).**
   - Edit: 
     - `git mv docs/bbnf docs/lang/bbnf`
     - `git mv docs/parse-that docs/lang/parse-that`
     - `git mv docs/pprint docs/lang/pprint`
     - `git mv docs/gorgeous docs/lang/gorgeous`
     - `git mv docs/performance docs/perf`
     - `git mv docs/cookbook docs/howto/cookbook`
     - `git mv docs/optimizer docs/howto/optimizer`
     - `git mv docs/migration docs/howto/migration`
     - `git mv docs/instructions docs/process/instructions`
     - `git mv docs/restart docs/process/restart`
     - `git mv docs/tranches docs/process/tranches`
     - `git mv docs/PHASE-4-DIRECTIVE-2026-05-03.md audit/plan-2026-05-03/`
   - Update internal cross-references via grep + edit.
   - Verify: `find docs -name '*.md' | xargs rg -l '\(.*docs/' | xargs rg -L '\(.*docs/(lang\|perf\|howto\|process\|spec\|audit)/' ` returns zero.
   - Commit: `docs(restructure): relocate to lang/perf/howto/process/audit/spec layout per Lock 13`.

5. **`docs/tranches/` archive (legacy Y-BD relocate).**
   - Edit: `git mv docs/process/tranches/{Y,Z,AA,AB,AC,AE,AF,AG,AH,AI,AJ,AK,AL,AM,AN,AO,AP,AQ,AR,AS,AT,AU,AV,AW,AX,AY-I,AY-II-I,AY-III,AZ-I,AZ-II,AZ-III,AZ-IV,B0,B1,B2,B3,B4,B5,B6,B7,BA,BB,BC,BD,W,X} docs/process/tranches/archive/legacy-Y-BD/`.
   - Verify: `find docs/process/tranches -maxdepth 1 -mindepth 1 -type d` returns only `archive`, `meta-audit`, plus new restart letters as they land.
   - Commit: `docs(tranches): archive legacy Y-BD letter set under restart`.

6. **`audit/` restructure.**
   - Edit: 
     - `mkdir -p audit/{codebase-2026-05-03,plan-2026-05-03,restart-2026-05-03}`
     - `git mv audit/HARDENING-2026-05-03-{01..08}-*.md audit/codebase-2026-05-03/`
     - `git mv audit/HARDENING-SYNTHESIS-2026-05-03.md audit/codebase-2026-05-03/`
     - `git mv audit/CENSUS-2026-05-03.md audit/codebase-2026-05-03/`
     - `git mv audit/MODULES-2026-05-03.md audit/codebase-2026-05-03/`
     - `git mv audit/RESTART-SKETCH-2026-05-03.md audit/codebase-2026-05-03/`
     - `git mv audit/SOTA-2026-05-03.md audit/codebase-2026-05-03/`
     - `git mv audit/HARDENING-PLAN-2026-05-03-{01..08}-*.md audit/plan-2026-05-03/`
     - `git mv audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md audit/plan-2026-05-03/`
     - `git mv audit/PHASE-4-SYNTHESIS-2026-05-03.md audit/plan-2026-05-03/`
     - `git mv audit/restart audit/restart-2026-05-03`
   - Update cross-references in synthesis docs.
   - Commit: `audit(restructure): partition into codebase/plan/restart wave subdirs per Lock 13`.

7. **README.md + GESTALT.md rewrite.**
   - Edit: full rewrite per Pass-C Agent 4 §6.2 + Agent 5 §7.4. Cite `crates/`, `xtask/` (not stale `rust/`); cite restart vocabulary.
   - Verify: `rg 'rust/\|typescript/\|prettier-plugin-bbnf/' README.md` returns zero (stale terms removed).
   - Commit: `docs(README, GESTALT): rewrite per restart vocabulary`.

8. **`.gitignore` + committed-artefact deletions.**
   - Edit:
     - `git rm -f server/bbnf-lsp`
     - `git rm -f extension/bbnf-language-support-1.0.{3,5}.vsix`
     - `git rm -rf wasm/pkg wasm/pkg-node wasm/pkg-node-debug`
     - Append to `.gitignore`: `server/`, `wasm/pkg*/`, `*.vsix`, `extension/dist/`, `playground/dist/`.
   - Verify: `git status` shows clean working tree post-commit.
   - Commit: `chore(.gitignore): exclude server/, wasm/pkg*/, *.vsix; remove committed artefacts`.

9. **SPEC.md + architecture.md + migration record + tranche A-J stubs.**
   - Edit: write `docs/spec/SPEC.md` (5d), `docs/spec/architecture.md` (3d), `docs/howto/migration/2026-restart.md` (1d). Stub `docs/process/tranches/{A,B,C,D,E,F,G,H,I,J}/{A,B,...,J}.md` with placeholder thesis + waves placeholder.
   - Verify: `find docs/spec -name '*.md'` lists the three new docs; `find docs/process/tranches -maxdepth 1 -mindepth 1 -type d -name '[A-J]'` returns 10 directories.
   - Commit: `docs(spec, tranches): land master spec, architecture, migration record, fresh tranche A-J set`.

10. **Master Plan + per-pass restart audits.**
    - Edit: `docs/restart/SYNTHESIZER.md` ratifies the master plan; `audit/restart-2026-05-03/MASTER-PLAN-2026-05-03.md` lands; the per-pass audits already landed via this suite.
    - Verify: `find audit/restart-2026-05-03 -name 'PASS-*.md' -o -name 'MASTER-PLAN-*.md'` lists all 4 pass docs + master.
    - Commit: `audit(restart): land master plan synthesis`.

### §8.3 — Substantive docs re-do (post-prelude)

11. **Wave 2 — `docs/lang/*` rewrite.** Per Pass-C Agent 5 §7.2. ~3-5 days. Banned-words sweep + AI-writing-sign cleanup + Lock 1/8 violation removal.
12. **Wave 2b — `docs/perf/*` full rewrite.** Per Pass-C Agent 5 §7.4. Lock 8 (AU silent; SOTA-only).
13. **Wave 2c — `docs/howto/*` polish.** Mostly-honoured; minor metalanguage strip.
14. **Wave 6 — Validation gate.** STYLE compliance scan; metalanguage scan; lock-adherence scan; CI-gate the validation.

### §8.4 — Post-prelude tranche execution (out of restart-suite scope)

The 10 tranche stubs (A-J) execute per Master Plan; out of Pass-C scope.

### §8.5 — Cutover

15. **Hardening pass.** `docs/restart/HARDENING.md`-driven double-back audit.

16. **Cutover decision (USER ratifies).**
    - Option A: `master-greenfield-2026-05-03` → `master`. Force-push. Pre-restart tag preserves prior chain.
    - Option B: keep both; master continues; greenfield evolves separately.

---

## §Closing

The Pass-C scope spans heterogeneous surfaces. The most consequential decisions:

1. **Lock 12 archive ceremony** — must land first; blocks everything else.
2. **Commit-chain disposition** — Option 3 (keep verbatim + branch reset); preserve provenance; tag + new branch.
3. **`docs/` tree restructure + re-do** — largest operational artefact; restructure mechanical (~4h) + substantive rewrite (8-13d).
4. **`crates/{analysis, lsp}` consolidation** — Lock 14 + 13 redress; merge into `crates/bbnf-language-server/`.

The greenfield mandate applies. Every doc not honoured by precepts is candidate ABROGATE-DELETE or ABROGATE-REPLACE. Every script / tool / sibling-repo coupling not idiomatic is fault. The post-restart workspace is cleaner, smaller, more cohesive — and the 2,621 commits remain accessible as archaeology, anchored by the `pre-restart-2026-05-03` tag.

Hereupon Pass C closes its synthesis. Pass A + Pass B compose alongside. The synthesizer ratifies all three; the master plan is the deliverable; hardening is the gate; tranche execution begins thereafter.
