# B2.W4 — Close

W4 lands the CI + pre-commit gate that prevents the workspace from
drifting between checked-in `crates/core/src/grammar/generated/<ident>.rs`
and the xtask's regenerated output, authors `docs/tranches/B2/FINAL.md`,
refreshes the AY-II planning docs to cite B2 as predecessor on the
post-B2 substrate, amends `docs/tranches/AZ-I/{AZ-I.md,waves/W0.md}`
to drop the derive-cache + Watt items as T3-superseded, updates
`docs/tranches/REMAINING-TRAJECTORY.md` and `docs/RISK-PERF-MATRIX.md`
to reflect B2's close, and authors `docs/benchmarks/post-B2.json` as
the aggregate proof matrix. B2 closes.

## Pre-state

- Master HEAD: `23b51df8` (B2.W3 close ceremony — docs).
- Worktree HEAD: same; clean.
- W3 retired the bash + Python regen substrate by reformulation,
  fixed the `include_str!` emitter for portable paths, added
  `make regen` + `make regen-check` Makefile targets, and authored
  the §Grammar regen section in PROFILING.md. The xtask is the
  canonical regen entrypoint; `cargo xtask regen --check` is ready
  to wire as a CI + pre-commit gate.

## Phase 1 — CI gate

`.github/workflows/ci.yml` `rust` job: the pre-existing
`preflight — bootstrap regen clean` step (which invoked the now-deleted
`scripts/check-bootstrap-clean.sh`) replaces with a
`preflight — regen check (xtask)` step that invokes `cargo xtask regen
--check` with `timeout-minutes: 5`. The step lands before
`preflight — clippy` and `preflight — iter-check`, catching drift
early without paying a full workspace check. The replaced narrative
comment also rewrote: "bootstrap regen cleanliness" → "regen drift
gate (xtask regenerates per-grammar `crates/core/src/grammar/
generated/` to a tempdir + diffs against checked-in; non-zero exit
blocks PR)" + "heavy proc-macro sites" → "heavy link-time crates"
(post-B2 the workspace has no proc-macros).

YAML validity verified: `python3 -c "import yaml;
yaml.safe_load(open('.github/workflows/ci.yml'))"` exits 0.

## Phase 2 — pre-commit hook

The repo carries no husky/lefthook/pre-commit framework; the active
mechanism is plain `.git/hooks/pre-commit`. Hooks are not versioned by
git, so the in-tree template lives at `scripts/hooks/pre-commit` (a
new directory under `scripts/`) + `scripts/install-hooks.sh` is the
fresh-checkout installer. The hook:

- Skips during `MERGE_HEAD` / `REBASE_HEAD` / `rebase-merge` /
  `rebase-apply` so half-applied trees do not block conflict
  resolution.
- Runs `cargo xtask regen --check` only when the staged change touches
  a grammar source (`grammar/`), the per-grammar generated tree
  (`crates/core/src/grammar/generated/`), or the regen entrypoint
  (`xtask/src/regen`). CI carries the canonical enforcement; the hook
  is the local fast-fail.
- Exits non-zero with a "run 'cargo xtask regen' and stage the result"
  diagnostic on drift.

The installer pulls the in-tree template, copies it to
`$(git rev-parse --git-path hooks)/pre-commit`, and marks it
executable. Mode `0755` on both the template and the destination.

Validation: `bash scripts/install-hooks.sh` from the worktree resolves
the hooks dir as `/Users/mkbabb/Programming/bbnf-lang/.git/worktrees/
bbnf-wt-b2-w4/hooks/pre-commit` (worktree-scoped); `git rev-parse
--git-path hooks` returns the worktree's git-dir hooks subdir, so the
installer Just Works inside any worktree.

## Phase 3 — FINAL.md

`docs/tranches/B2/FINAL.md` authored. Sections (no meta-language; no
references to plans, commits, conversation history; standalone prose):

- **Headline** — `cargo xtask regen` is the canonical regen
  entrypoint; per-grammar source on disk; `pub use` re-export; CI +
  pre-commit gate on regen-check.
- **Architectural narrative** — five mechanisms compose: workspace
  manifest, per-grammar source on disk, marker-struct re-export,
  portable `include_str!` paths, drift detection via git history.
- **Performance** — wall table; pre-B2 80-min wall retired.
- **Test results** — 1 160 / 1 490 pass; 327 / 3 / 27 routed to B4.W1
  (release-mode parity green).
- **Cross-tranche effects** — AY-II.W0' compressed-honest ~15 min;
  AZ-I.W0 derive-cache + Watt items retire; AZ-II byte-equal cycles
  cost seconds; BA / BB anchor on the on-disk codegen.
- **Forward-routed work** — four-row table.
- **Invariant table** — 14 rows, all green.
- **Hard-gate table** — per-wave gates with closing artefacts.
- **Wave commit ledger** — every B2 commit with one-line headline.
- **AY-II handoff block** — explicit unblock on the post-B2 substrate.

## Phase 4 — post-B2.json aggregate

`docs/benchmarks/post-B2.json` authored. Top-level keys: `provenance`,
`walls`, `tests`, `structural`, `ci_gate`, `pre_commit_hook`,
`cross_tranche_effects`, `forward_routed_work`. Cycle-1 cold regen
wall captured at ~12:43 (full sweep) and ~5 min (single-grammar);
IR-pipeline-only timings at ~73 ms per grammar; `iter-check` warm
at 0.21 s post-W4 close; `iter-check-full` warm at 0.13 s; pre-B2
`cargo expand -p bbnf-bootstrap --lib` cold > 80 min recorded as
retired. Test pass-rate matches the W3 baseline. Structural facts
(crates_derive_deleted, BBNF_SCHEMA_VERSION_retired, bootstrap
scripts retired, ay_prime_makefile_target_retired,
include_str_paths_portable, per-grammar source files with line
counts) all true. JSON validity verified: `python3 -c "import json;
json.load(open('docs/benchmarks/post-B2.json'))"` exits 0.

## Phase 5 — cross-tranche doc updates

| File | Change |
|---|---|
| `docs/tranches/AY-II/AY-II.md` | Header narrative cites B1 + B3 + B2 predecessor sequence; W0' Wave-summary row reflects post-B2 substrate + compressed-honest ceremony per AUDIT-B |
| `docs/tranches/AY-II/PATH-FORWARD.md` | Header date amended to "amended 2026-04-25 post-B2 close"; ordered work expands to five steps (B1, B3, B4.W0, B2 closed; W0' compressed-honest unblocked); §Current truth updates the regen-state narrative; §2 ceremony spec replaces the pre-B2 form with the compressed-honest form |
| `docs/tranches/AY-II/PROGRESS.md` | "## 2026-04-25 — B2 close unblocks W0' compressed-honest ceremony" entry appended noting the substrate shift; status header refreshed |
| `docs/tranches/AY-II/waves/W0p.md` | Status line refreshed to cite B1 + B3 + B4.W0 + B2 close sequence; §Orchestrator-owned close ceremony rewrites to the compressed-honest 5-step form (cycle-1 regen + invariant verification + projection-totality + Unknown retirement + close-status); §Hard gate rewrites with cycle-2/expand/bench/samply/nm deferred to wave-specific gates; §Verification artefacts rewrites accordingly |
| `docs/tranches/AZ-I/AZ-I.md` | §W0 narrative cites the post-B2 amendment (derive-cache + Watt items T3-superseded); §Critical files drops `crates/derive/` + `crates/derive/tests/cache_invalidation/` rows; §Open questions absorbed Q7 dissolves with named rationale; §Handoff contract to AZ-II §4 cites `cargo xtask regen --grammar bbnf` instead of `crates/bbnf_derive/` |
| `docs/tranches/AZ-I/waves/W0.md` | Wholesale rewrite to the post-B2 amendment: 2 sub-agents instead of 3; classifier + IR audit + baseline bench scope retained; `crates/derive/` file-bounds dropped; hard-gate items 5 → 4; Archaeology section records the supersession |
| `docs/tranches/REMAINING-TRAJECTORY.md` | Status line updates to "B1 -> B3 -> B4 -> B2 -> AY-II ..."; §1 Path Change rewrites to cite the post-B1 predecessor sequence; §3 probability tables update with B1/B3/B4.W0/B2 closed rows + AY-II/AZ-I/AZ-II floor lifts; §4 ledger gets B4.W0 + B2 rows; §5 ledger gets B2 row; AZ-I.W0 row notes the post-B2 amendment |
| `docs/RISK-PERF-MATRIX.md` | Title + opening prose update to cite the post-B1 predecessor sequence + post-B2 substrate; §AY-II rewrites scope narrative + W0' close row scope (compressed-honest spec) + tranche-close numbers (0.20 → 0.30 declared, 0.55 → 0.65 floor); §AZ-I W0 row scope rewrites (post-B2 amendment) + tranche-close numbers (0.070 → 0.080 declared, 0.29 → 0.36 floor) + post-preflight numbers; §AZ-II W2/tranche-close rewrites with byte-equal-at-seconds-cost narrative + tranche-close numbers (0.17 → 0.20 declared, 0.45 → 0.50 floor) |
| `docs/tranches/B2/B2.md` | Wave summary table flips W3 + W4 status from "planned" to "complete" |
| `docs/tranches/B2/PROGRESS.md` | Append "## 2026-04-25 — W4 closed; B2 closed" entry; status header → `complete` |
| `docs/tranches/B2/waves/W4.md` | Status line `planned` → `complete` |

## Phase 6 — verify gates

| Gate | Wall | Exit |
|---|---|---|
| `cargo check --workspace --profile ax-iter` (warm post-edits) | 13.24 s | 0 |
| `cargo iter-check` (warm post-edits, parallel cycle) | 13.27 s | 0 |
| `cargo iter-check` (warm second run, sequential) | 0.11 s | 0 |
| `cargo xtask regen --check` (idempotent diff) | 1.11 s | 0 |
| `python3 -c "import yaml; yaml.safe_load(open('.github/workflows/ci.yml'))"` | < 1 s | 0 (YAML OK) |
| `python3 -c "import json; json.load(open('docs/benchmarks/post-B2.json'))"` | < 1 s | 0 (JSON OK) |

The first parallel `iter-check` invocation paid the cold lock cost
(target was already populated but iter-check + check + regen-check
ran simultaneously, serializing on the target lock); the sequential
warm run at 0.11 s confirms the post-W3 baseline holds at W4 close.
The `regen --check` exit 0 + "regen --check: clean (9 grammars
matched)" confirms zero drift between the checked-in tree and the
xtask's regenerator.

## W4 hard-gate verdict

| Gate | Status |
|---|---|
| (1) `.github/workflows/ci.yml` invokes `cargo xtask regen --check` | met |
| (2) Pre-commit hook in place (in-tree template + installer) | met |
| (3) `docs/tranches/B2/FINAL.md` authored | met (standalone prose; no meta-language; ~250 lines) |
| (4) AY-II planning docs reflect B2 as predecessor | met (4 files updated) |
| (5) AZ-I planning docs amended (derive-cache + Watt dropped) | met (2 files updated) |
| (6) `docs/tranches/REMAINING-TRAJECTORY.md` insert B2 in sequence | met |
| (7) `docs/RISK-PERF-MATRIX.md` revised post-B2 | met |
| (8) `docs/benchmarks/post-B2.json` authored + valid JSON | met |
| (9) `cargo xtask regen --check` exits 0 (no drift) | met (1.11 s wall) |
| (10) `cargo check --workspace --profile ax-iter` exits 0 | met (13.24 s warm) |
| (11) `cargo iter-check` warm exits 0 in ≤ 0.5 s | met (0.11 s sequential) |
| (12) CI YAML valid | met |
| (13) B2 close audit + status complete | met (this file + PROGRESS update + B2.md status flip) |

## W4 close verdict

CLOSED. The CI gate + pre-commit hook prevent drift between the
checked-in per-grammar source tree and the xtask's regenerated
output. FINAL.md captures the architectural narrative, performance
numbers, test results, cross-tranche effects, and forward-routed
work in standalone prose. The AY-II.W0' close ceremony is unblocked
in compressed-honest form on the post-B2 substrate; AZ-I.W0
simplifies to its load-bearing core; the runway sequence
`B1 → B3 → B4 → B2 → AY-II → AZ-I → AZ-II → BA → BB` lands in the
planning canon. B2 closes.

## B2 close verdict

CLOSED. The `bbnf_derive` proc-macro IR-pipeline contract retires.
`cargo xtask regen` is the canonical regen entrypoint. Per-grammar
source emerges on disk under
`crates/core/src/grammar/generated/<ident>.rs`. Consumer crates
`pub use ::bbnf::grammar::generated::<ident>::*` in place of
`#[derive(Parser)]`. `crates/derive/` deletes outright (3 files /
457 lines). `BBNF_SCHEMA_VERSION` retires. The `bootstrap-bbnf.sh` +
`check-bootstrap-clean.sh` scripts retire (substrate reformulation,
not transformation reproduction). The `make ay-prime` +
`clean-cache` Makefile targets retire. CI + pre-commit gate on
`cargo xtask regen --check`. The pre-B2 80-min cold rustc-side
IR-pipeline wall ceases to exist.

The substrate's role retires by reformulation: a single file on
disk per grammar, refreshed by one xtask invocation, gated by one
diff at CI. No proc-macro contract, no schema-version protocol, no
content-keyed cache, no `cargo expand` regex post-processor.

## Hand-off

Master post-B2 carries:

- `xtask/` workspace member as the canonical regen entrypoint;
  `cargo xtask regen [--grammar <ident>] [--check]` surface stable.
- 9 per-grammar source files under
  `crates/core/src/grammar/generated/<ident>.rs` with portable
  `concat!(env!("CARGO_MANIFEST_DIR"), "/../../", <rel>)` paths.
- `pub use ::bbnf::grammar::generated::<ident>::*` consumer pattern
  across 43 files / 62 sites.
- `make regen` + `make regen-check` Makefile aliases.
- `.github/workflows/ci.yml` regen-check preflight step before
  iter-check.
- `scripts/hooks/pre-commit` in-tree template + `scripts/
  install-hooks.sh` installer.
- AY-II planning docs reflecting compressed-honest W0' ceremony.
- AZ-I planning docs amended (derive-cache + Watt items dropped).
- REMAINING-TRAJECTORY.md + RISK-PERF-MATRIX.md updated with the
  post-B2 substrate shift.

AY-II.W0' close ceremony resumes immediately on this substrate.
