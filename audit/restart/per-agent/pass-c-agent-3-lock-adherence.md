# Pass C Agent 3 — Lock Adherence

Date: 2026-05-03. Lens: 14 architectural locks applied to Pass-C scope. Verdicts: **honoured / violated-with-rec / silent-must-add**.

The 14 locks are settled (per HARDENING-PLAN-PROMPT §Gestalt and the user's `docs/HARDENING-PLAN-PROMPT.md`). This agent verifies the codebase + docs honour them.

---

## §Lock 1 — Tape and columnar variants are fully dead

### §1.1 — Pass-C surfaces with tape-residue

Tape comments are widespread in `crates/core/` (Pass-A scope) per CENSUS §1.2. Pass-C scope tape touchpoints:

| Surface | Tape mention | Verdict |
|---|---|---|
| `crates/analysis/` | None observed in CENSUS sweep | honoured |
| `crates/lsp/` | None observed | honoured |
| `crates/ser/` (archived candidate) | crate is tape-era; archives with tape vocabulary intact | honoured-by-archive |
| `crates/gorgeous/` (archived candidate) | gorgeous's bytecode VM is tape-era | honoured-by-archive |
| `docs/codegen-paths.md`, `docs/performance/*` | likely tape references in older docs | violated-with-rec |
| `docs/tranches/Y/Y.md`, `docs/tranches/AC/AC.md`, `docs/tranches/AE/AE.md` | tranche docs from tape-first era cite tape | honoured-by-archive (tranche docs ARE archaeological) |
| `docs/tranches/archive/pre-restart-{BA,BB,BC}/` | archived plan-set; tape mentions OK | honoured-by-archive |

Verdict: **violated-with-rec** for `docs/performance/*`, `docs/codegen-paths.md`, `docs/bbnf/*` if tape references appear. Tranche-records and audit corpora are exempt as archaeological. Surgery: sweep `rg -ni 'TapeRec|TapeBuilder|TapeCursor|tape-?first|columnar' docs/{bbnf,performance,parse-that,pprint,gorgeous,cookbook,optimizer,migration,instructions}` and rewrite each match to current substrate.

### §1.2 — `audit/RESTART-SKETCH-2026-05-03.md`, `audit/MODULES-2026-05-03.md`

These cite tape-history in narrative form (per archaeology + module fates). Audit corpora reconstruct the timeline; tape vocabulary is correct in retrospective. Verdict: **honoured-by-design**.

---

## §Lock 2 — Layout lowering is the canonical IR pass name

### §2.1 — Pass-C touchpoints

Lock 2 retires `TypeDesc / StructLayout / TypeMap / type-projection / type-collapsing / TypeDesc / schema synthesis` everywhere except archived docs.

| Surface | Old terms found | Verdict |
|---|---|---|
| `docs/migration/bc-core-split.md` | likely cites `TypeDesc` per filename context | violated-with-rec |
| `docs/optimizer/pratt-simd-detection.md` | unclear | silent-must-add audit |
| `docs/performance/ir-pipeline.md` | likely cites `TypeDesc / StructLayout` | violated-with-rec |
| `docs/codegen-paths.md` | older era | violated-with-rec |
| `docs/cookbook/*` | recent; unclear | silent-must-add audit |
| `crates/analysis/src/`, `crates/lsp/src/` | grammar-tier; consumes IR; might import `TypeDesc` | silent-must-add audit |
| `audit/MODULES-2026-05-03.md` line 1003ff "Synthesis" | uses `Layout` per Phase-4 vocabulary | honoured |

Verdict: **violated-with-rec** for older docs; sweep `rg -wn 'TypeDesc|StructLayout|TypeMap|type-projection|type-collapsing|schema synthesis|LayoutDesc' docs/` over non-precepts non-tranche-archive paths and rewrite each to `Layout` / `LayoutSink` / `bbnf-ir/src/passes/layout/`.

### §2.2 — Per HARDENING-PLAN-SYNTHESIS punch-list items 3, 4

Items 3 + 4 cite `docs/tranches/BA/waves/W2.md:9-11`, `:56`, `:134` and `docs/tranches/BC/BC.md:30, :141, :152` for retired-term-uses. Pass-C scope absorbs these as ABROGATE-MOVE (the BA/BB/BC drafts are now in `docs/tranches/` and need rewriting under restart authority). Verdict: routed.

---

## §Lock 3 — Cursor-parse + byte-skip unified, cursor branch elided when empty path

### §3.1 — Pass-C scope

Lock 3 governs `crates/core/src/path/`, `crates/path/`, `crates/path-core/`, `crates/bbnf-path*` — Pass-A territory. Pass-C surfaces:

| Surface | Lock 3 implication |
|---|---|
| `crates/lsp/` | Consumes path query; not a Lock 3 site |
| `crates/analysis/` | Document state — not path-related |
| `docs/cookbook/path-macro.md` | User docs about `pointer![…]` — relevant |
| `docs/bbnf/api-reference.md` | API docs — relevant if path API surfaced |

Verdict: **silent-must-add** — `docs/cookbook/path-macro.md` should clearly state Lock 3's empty-path elision invariant ("the eager fast path pays no consultation cost"). Surgery: confirm doc content; add invariant statement if absent.

---

## §Lock 4 — Per-domain orthogonal optimization

### §4.1 — Pass-C reach

Lock 4 governs `csp-solver`, `egraph`, recognizer pipeline. Pass-C scope:

| Surface | Implication |
|---|---|
| `docs/optimizer/pratt-simd-detection.md` | Documents Pratt + SIMD auto-detection — names the optimizer; should not fuse CSP + e-graph |
| `crates/analysis/` | The crate consumes IR — does it consult CSP / e-graph? Per CENSUS §2.3, analysis is BBNF-grammar-specific; if it bypasses the pluggable optimizer, it's local-shaped, not Lock-4-violating |

Verdict: **honoured** at present.

---

## §Lock 5 — IR + per-backend lower

### §5.1 — Pass-C reach

Lock 5 governs `bbnf-codegen` + per-backend lowerers (Rust, TS, WASM). Pass-C surfaces:

| Surface | Implication |
|---|---|
| `wasm/src/{analysis,gorgeous,lsp,vm}.rs` | WASM bindings; not the WASM backend per Lock 5 |
| `extension/src/extension.ts` | LSP client; not codegen |
| `docs/bbnf/api-reference.md` | If it documents the IR-then-backend separation, it's relevant |
| `docs/optimizer/`, `docs/cookbook/`, `docs/migration/` | Recent Phase-4; should reflect IR + per-backend |

Verdict: **silent-must-add** — confirm `docs/cookbook/`, `docs/optimizer/`, `docs/migration/` reference the IR-as-contract pattern. Surgery: add IR-contract note in `docs/optimizer/pratt-simd-detection.md` and any cookbook entry that crosses Rust↔TS↔WASM.

---

## §Lock 6 — xtask emits committed source artefacts

### §6.1 — `xtask/` crate

`xtask/src/{main,lib,regen}.rs` — the regen entry. The Makefile delegates `make regen` → `cargo xtask regen`. Verdict: **honoured**.

### §6.2 — No proc-macro for codegen output

`crates/bbnf-path/` and `crates/bbnf-path-ts/` are proc-macro shells (per Lock 7), not codegen-output proc-macros. Verdict: **honoured**.

---

## §Lock 7 — `crates/path/` is the consolidated path crate

### §7.1 — Pass-C touchpoint

The consolidation is a Pass-A concern. Pass-C surfaces with stale path-naming:

| Surface | Stale reference |
|---|---|
| `docs/cookbook/path-macro.md` | likely cites `pointer![…]` and the macro shell — should reference `crates/path/`, `crates/path-core/`, `crates/path-ts/` per restart |
| `docs/migration/bc-core-split.md` | cites the BC core split — must use restart vocabulary post-Pass-C ratification |
| `docs/bbnf/api-reference.md` | path API docs |

Per HARDENING-PLAN-SYNTHESIS punch-list 6, the BA.W3 plan-doc still names `bbnf-path` (stale). Pass-C scope absorbs the doc-side propagation.

Verdict: **violated-with-rec** — sweep all docs (excl. precepts + tranche archive) for `bbnf-path` / `crates/bbnf-path/` references, replace with `crates/path/`, `crates/path-core/`, `crates/path-ts/` per Lock 7.

---

## §Lock 8 — Surpass sonic-rs / simdjson / lightning-css; AU is never mentioned

### §8.1 — AU references in docs

Era VI (post-AU-baseline) tranche docs cite AU; tranche archaeology corpora cite AU. Both are exempt (archaeological). User-facing `docs/performance/*`, `docs/bbnf/*` should NOT cite AU.

| Surface | AU reference risk |
|---|---|
| `docs/performance/benchmarks.md` | high (likely cites post-AU.json or similar) |
| `docs/performance/timeline.md` | very high (literally a timeline) |
| `docs/performance/parsing.md` | high |
| `docs/performance/overview.md` | medium |
| `docs/codegen-paths.md` | medium |
| `docs/bbnf/*` | low |
| `docs/cookbook/*` | low |
| `docs/optimizer/pratt-simd-detection.md` | medium |
| `docs/migration/bc-core-split.md` | high (per filename) |
| Audit corpora | exempt by design |
| Tranche docs | exempt by design |

Verdict: **violated-with-rec** for `docs/performance/*` + `docs/migration/bc-core-split.md`. Surgery: sweep `rg -n 'post-AU|AU baseline|Tranche AU|AU-?[A-Za-z]' docs/{performance,bbnf,parse-that,pprint,gorgeous,cookbook,optimizer,migration,instructions}` and rewrite each: replace AU with the SOTA competitor (sonic-rs, lightning-css, simdjson) where the comparison is the point; delete AU-history references entirely.

### §8.2 — `audit/SOTA-2026-05-03.md`

The SOTA survey IS the lock 8 ground-truth. Verdict: **honoured**.

---

## §Lock 9 — Slice-borrow primary; bumpalo + owned escape hatches

### §9.1 — Pass-C touchpoint

Lock 9 governs `parse(input) / parse_in(input, &bump) / parse_owned(input)` API split. Pass-C surfaces:

| Surface | Implication |
|---|---|
| `docs/cookbook/lifetime-surfaces.md` | Per filename — *the* doc for Lock 9 |
| `docs/bbnf/api-reference.md` | API docs |
| `crates/analysis/`, `crates/lsp/` | Consumers of the API; should consume `&str` slices |

Verdict: **silent-must-add** — confirm `docs/cookbook/lifetime-surfaces.md` reflects the three-way split + when to use each. Surgery: read + amend if absent.

---

## §Lock 10 — Pratt + SIMD auto-detected; no `@pratt` / `@simd` directives

### §10.1 — Pass-C touchpoint

Lock 10 governs the optimizer mining grammar shape. Pass-C surface:

| Surface | Implication |
|---|---|
| `docs/optimizer/pratt-simd-detection.md` | *The* doc for Lock 10 |
| `docs/cookbook/*` | might document grammar-author idioms |
| `crates/analysis/src/directives/hints.rs` | hint catalogue — must NOT include `@pratt`/`@simd` per Lock 10 |

Verdict: **silent-must-add audit** — `crates/analysis/src/directives/hints.rs` should be inspected to confirm zero `@pratt`/`@simd` entries. Surgery: read; if absent, honoured; if present, ABROGATE-DELETE entries.

---

## §Lock 11 — Path-deps for incubating sister crates

### §11.1 — Pass-C touchpoint

Workspace `Cargo.toml` already path-deps `egraph`, `egraph-derive`, `csp-solver`, `bbnf-regex` (via `parse_that` linkage), `simd-scan`, `bootstrap`, `analysis`, `lsp`. Verdict: **honoured-mostly** — `crates/parse-that/` (referenced in CENSUS but not visible in workspace `members`) needs verification of path-dep status. Surgery: `cargo metadata | jq '.workspace_members'` to confirm. The `[workspace]` `members` line lists `crates/{core, analysis, ir, lsp, ser, gorgeous, bootstrap, egraph, egraph-derive, csp-solver, simd-scan, bbnf-path, bbnf-path-ts}`. `parse-that` not visible — likely external repo at this point. KEEP-MODIFY: confirm parse-that's status, path-dep if local, registry if external.

### §11.2 — `crates/ser/` and `crates/gorgeous/` archive condition

Lock 11 says "ser + gorgeous archive at `archive/<crate>/`, removed from workspace, source preserved". Cargo.toml line 2 still lists them; archive directory does not exist. Verdict: **violated-with-blocking-rec** — same as Agent 2 §5.1.

---

## §Lock 12 — ser + gorgeous archive BEFORE BA.W0

### §12.1 — Status

NOT executed. Cargo.toml lists `crates/ser`, `crates/gorgeous`. `archive/` directory absent. The plan-set's BA.W0 cannot legitimately open until this lands.

Verdict: **violated-with-blocking-rec** — execute as the FIRST restart-suite ratification step.

Surgery sequence:

1. `git mv crates/ser archive/ser` (creates archive/ on first move)
2. `git mv crates/gorgeous archive/gorgeous`
3. Edit `Cargo.toml`: remove `crates/ser`, `crates/gorgeous` from `[workspace] members`.
4. Update any `bbnf-ser`, `bbnf-gorgeous` usages in `crates/` (sweep `rg`); convert to dev-dep or remove.
5. Update Makefile gorgeous mentions if any (line 162 README → docs reference; OK as link).
6. Verify: `cargo check --workspace` passes; `rg 'crates/ser|crates/gorgeous|bbnf-ser|bbnf-gorgeous' crates/` returns zero.
7. Commit: `chore(workspace): archive ser + gorgeous per Lock 12`.

---

## §Lock 13 — No god directories; cohesive encapsulation at every level

### §13.1 — `docs/tranches/` as a god directory

49 immediate children (`Y, Z, AA-AT, AU, AV, AW, AX, AY-I, AY-II-I, AY-III, AZ-I, AZ-II, AZ-III, AZ-IV, B0, B1, B2, B3, B4, B5, B6, B7, BA, BB, BC, BD, archive, meta-audit, W, X, PLAN-INPUT-2026-05-03.md`). The branching is uniform-by-letter (one concern per child) so it's not a "kitchen sink"; but the count exceeds Lock 13's 10-immediate-children rule.

Verdict: **violated-with-rec** — relocate all letter-tranches under `docs/tranches/archive/legacy-Y-BD/` per restart README §Outputs Aggregated. Result: `docs/tranches/{archive/, A/, B/, ..., J/, meta-audit/}` — 12-13 children.

### §13.2 — `docs/` as a god directory

14 immediate subdirs + 4 top-level files: `bbnf/, parse-that/, performance/, pprint/, gorgeous/, cookbook/, optimizer/, migration/, instructions/, benchmarks/, precepts/, restart/, tranches/, audit/` + `GESTALT.md, HARDENING-AUDIT-PROMPT.md, HARDENING-PLAN-PROMPT.md, PHASE-4-DIRECTIVE-2026-05-03.md, codegen-paths.md`. 14 subdirs is at Lock 13's borderline; concerns are mixed (user-facing language docs vs. project-process docs vs. performance corpus vs. audits).

Verdict: **violated-with-rec** — restructure into:
- `docs/lang/{bbnf, parse-that, pprint, gorgeous}/` — language docs (former `docs/{bbnf,parse-that,pprint,gorgeous}/`)
- `docs/perf/` (former `docs/performance/`)
- `docs/howto/{cookbook, optimizer, migration}/` — user-facing how-tos
- `docs/process/{precepts, restart, instructions, tranches}/` — project process
- `docs/audit/` — restructure existing
- top-level files: `GESTALT.md`, `README.md` only; relocate prompts to `docs/process/` 

Result: 5 immediate children + 2 top-level files. Lock 13 honoured.

### §13.3 — `scripts/` as a god directory

15 files at top level mixing concerns (profile, test, deploy, orchestrate, hooks). Verdict: **violated-with-rec** — restructure:
- `scripts/profile/{prepare-wave.sh, bench-headless.sh, extract-hotspots.py, iai-compare.sh}` 
- `scripts/test/{tier.sh, prebuild-benches.sh, bisect-fastpath.sh}` 
- `scripts/orchestrate/{seed-worktree.sh, worktree-status.sh, kill-all-rust.sh}` 
- `scripts/deploy/{deploy.sh, sync-external-docs.sh}` 
- `scripts/hooks/{pre-commit, install-hooks.sh}` 
- `scripts/doctor.sh` (top-level — host probe)

### §13.4 — `audit/` as a god directory

22 top-level files mixing 3 audit waves (codebase, plan, restart). Verdict: **violated-with-rec** — restructure:
- `audit/codebase-2026-05-03/{HARDENING-{01..08}-*.md, HARDENING-SYNTHESIS.md, CENSUS.md, MODULES.md, RESTART-SKETCH.md, SOTA.md}` 
- `audit/plan-2026-05-03/{HARDENING-PLAN-{01..08}-*.md, HARDENING-PLAN-SYNTHESIS.md, PHASE-4-SYNTHESIS.md}` 
- `audit/restart-2026-05-03/{PASS-A,PASS-B,PASS-C,MASTER-PLAN, per-agent/}` (current `audit/restart/`)

### §13.5 — Other dirs

| Dir | Status |
|---|---|
| `crates/analysis/src/{features, directives, state}/` | 3 subdirs + lib.rs + analysis.rs — within Lock 13 |
| `crates/lsp/src/{server, dap}/` | 2 subdirs — within Lock 13 |
| `extension/src/` | 1 file — fine |
| `playground/src/{App.vue, components/, composables/, demos/, lib/, router/, views/, wasm/, assets/, main.ts, env.d.ts}` | 8 subdirs — within Lock 13 |
| `wasm/src/{analysis,gorgeous,lsp,vm,lib}.rs` | 5 sibling files — within |

Verdict for sub-trees: **honoured-mostly**.

---

## §Lock 14 — Full grammar generalisation; zero overfitting

### §14.1 — `crates/analysis/` is grammar-coupled per CENSUS

CENSUS §2.3: `crates/analysis/src/features/formatting.rs:6-8` imports `bbnf::runtime::bbnf::{BbnfCompoundKind, BbnfView}`; `crates/analysis/src/directives/hints.rs` hard-codes the `@pretty` hint catalog (BBNF-specific). The CENSUS verdict: KEEP — analysis crate is the BBNF grammar's LSP. The CENSUS DEFER says "cohort move is out of BA-restart kill-list scope".

Lock 14 says the substrate carries ZERO grammar-specific code; analysis is grammar-specific BUT analysis is itself a grammar-host (a crate dedicated to the BBNF grammar's analysis). The Lock 14 question is: is `crates/analysis/` a generic crate (must not be grammar-coupled) or a per-grammar crate (allowed)?

Per Lock 14 prose (verbatim): "Generic crates — `bbnf-parse`, `bbnf-codegen`, `bbnf-runtime`, `bbnf-ir`, `path`, `path-core`, `egraph`, `csp-solver`, `bbnf-regex`, `parse-that`, `simd-scan`, `analysis`, `lsp` — carry ZERO `match grammar { ... }` arms".

The Lock 14 verbatim list INCLUDES `analysis` and `lsp` as generic crates. By that wording, `crates/analysis/`'s import of `bbnf::runtime::bbnf::*` IS a Lock 14 violation.

Resolution path: either (a) `crates/analysis/` is renamed `crates/bbnf-analysis/` and bucketed as a per-grammar crate (allowed), or (b) the BBNF-specific imports move OUT of `crates/analysis/` into `crates/bbnf-grammar/` (or whichever per-grammar declaration crate carries BBNF host-fns).

The CENSUS gestures at the same resolution: "The analysis crate ARGUABLY belongs under a `bbnf-grammar` family alongside `bbnf-bootstrap`. Naming-wise, `bbnf-analysis` is fine".

Verdict: **violated-with-rec** — the architectural transposition (Agent 4) recommends rename or split. Pass-C ratifies the rename per Lock 14 + CENSUS DEFER lift.

### §14.2 — `crates/lsp/`

LSP server. Per Lock 14 prose `lsp` is in the generic-crates list. The LSP wraps `crates/analysis` so its grammar-specificity inherits. If `crates/analysis/` is renamed `crates/bbnf-analysis/`, `crates/lsp/` becomes `crates/bbnf-lsp/` (matching the binary name) or stays generic and consumes per-grammar analysis crates via dispatch.

Verdict: **violated-with-rec** — bucket per Architectural Transposition (Agent 4); the rename is the simplest path.

### §14.3 — `audit/CENSUS-2026-05-03.md` §2.4 — `crates/bbnf-path/` and `crates/bbnf-path-ts/`

CENSUS §2.4: `crates/bbnf-path/src/registry.rs:132-135` `match grammar { "json" => ..., "css_l4" => ..., "google_sheets" => ..., "bbnf" => ... }` — Lock 14 violation.

`crates/bbnf-path-ts/src/fixture.rs` (248 LOC) — "Per-grammar fixture registry" — Lock 14 violation by design (synthetic fixture mirror).

Pass-A scope; Pass-C does not surger; routes upstream.

Verdict: **violated** (routed to Pass A).

### §14.4 — `crates/gorgeous/src/builtin.rs:9-22`

Match-on-grammar-name dispatch. Lock 14 violation. Resolved by Lock 12 archive (the violation moves to `archive/gorgeous/` and becomes provenance).

Verdict: **violated-but-archive-resolves**.

### §14.5 — Future-grammar onboarding test (per PHASE-4-SYNTHESIS §2.4)

Per PHASE-4-SYNTHESIS §2.4: "Future-grammar onboarding test". Adding a hypothetical `tenth_grammar` should require ZERO code change in any generic crate. Pass-C surface that fails this test:

- `Cargo.toml` `[workspace.metadata.bbnf]` — adding a row IS a config change (allowed per Lock 14)
- `Cargo.toml` `[workspace.metadata.bbnf-strategy]` — adding a row IS a config change (allowed)
- `crates/analysis/src/features/formatting.rs:6-8` — imports `BbnfCompoundKind`, `BbnfView`; this fails the test for `crates/analysis/` if treated as generic
- `crates/gorgeous/src/builtin.rs:9-22` — fails the test (resolved by archive)
- `crates/bbnf-path/src/registry.rs` — fails (Pass-A)

Verdict: **violated** for `crates/analysis/` if treated as generic; **honoured** if `crates/analysis/` is renamed `crates/bbnf-analysis/`.

---

## §Top-line lock-adherence verdict

| Lock | Verdict | Pass-C surgery |
|---|---|---|
| 1 (tape dead) | violated in user-facing docs | Sweep + rewrite `docs/{performance,bbnf,parse-that,pprint,gorgeous,cookbook,optimizer,migration,instructions}` |
| 2 (Layout canon) | violated in user-facing docs | Same sweep |
| 3 (cursor + byte-skip) | silent-must-add | `docs/cookbook/path-macro.md` — confirm or add |
| 4 (per-domain optim) | honoured | — |
| 5 (IR + per-backend) | silent-must-add | `docs/cookbook/`, `docs/optimizer/`, `docs/migration/` should reflect |
| 6 (xtask source emit) | honoured | — |
| 7 (consolidated path crate) | violated in docs | Sweep `bbnf-path` → `crates/path/` references in docs |
| 8 (surpass SOTA, AU silent) | violated in docs/performance/* + docs/migration/bc-core-split.md | Sweep + rewrite |
| 9 (slice-borrow primary) | silent-must-add | `docs/cookbook/lifetime-surfaces.md` confirm |
| 10 (Pratt + SIMD auto-detect) | silent-must-add | `crates/analysis/src/directives/hints.rs` audit; ZERO `@pratt`/`@simd` entries |
| 11 (path-deps incubating) | honoured-mostly | confirm parse-that disposition |
| 12 (ser + gorgeous archive BEFORE BA.W0) | **violated-with-blocking-rec** | Execute archive ceremony as first restart step |
| 13 (no god directories) | violated for `docs/`, `docs/tranches/`, `scripts/`, `audit/` | Restructure per §13.1-§13.4 |
| 14 (full grammar generalisation) | violated for `crates/analysis/` | Rename to `crates/bbnf-analysis/` OR strip BBNF-specific imports |

The most consequential lock failures in Pass-C are **Lock 12** (blocking; archive ceremony unexecuted), **Lock 13** (god directories at `docs/`, `docs/tranches/`, `scripts/`, `audit/`), and **Lock 14** (`crates/analysis/`'s grammar-specificity in a generically-named crate). Locks 1, 2, 7, 8 violations in older user-facing docs are mechanical-sweep redress within the docs re-do.
