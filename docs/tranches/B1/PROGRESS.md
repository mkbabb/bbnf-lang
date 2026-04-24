# B1 — Progress Log

Dated execution log for tranche B1, the bounded prelude annex that
refreshes the development/proof surface before AY-II resumes.

- `Status`: planned (waves W0→W3 sequenced per `TOOLCHAIN-MIGRATION.md`
  12-step plan)
- `Current wave`: W0 (planned)
- `Next wave`: W1 (opens on W0 + ICE-clean gate)

---

## 2026-04-24 — W0 close (with triumvirate redress on the iter-check-full ceiling)

Master at `d44585bb`. W0 landed as four commits + the triumvirate redress:

- `41b9c4fb` — W0.a substrate pin (`rust-toolchain.toml` → `nightly-2026-04-11`). Smoke
  `cargo check -p bbnf --profile ax-iter` = 6.32 s; ICE count 0 post-smoke.
- `416dcf76` — W0.b `.cargo/config.toml` rewrite. 4-exclude `iter-check` satisfies
  invariant 10 (gorgeous, bbnf-bootstrap, bbnf-analysis, bbnf-lsp each have a named
  fast-path alias). Cost-model comment block expanded. `cargo build -p bbnf` 44.91 s
  (cold rebuild under new `-Zthreads=8 -Zshare-generics=y` rustflags).
- `6d800162` — W0.c `.config/nextest.toml` rewrite. 4 profiles; `[[profile.close.overrides]]`
  (array-of-tables form) draft-typo discovered + fixed in-place. `2b6e50bf` propagates
  the fix to `patches/nextest.toml.draft`.
- `06d3db65` — W0.d Makefile rewrite (470 → 210 lines). GNU-timeout cascade
  deleted; `ay-prime` target added; `scripts/test-tier.sh` aligned to
  nextest surface + `--profile` passthrough.
- Triumvirate (`eeca61e1` research, `fd2cf6fb` plan, `22013145` redress Change 1,
  `1926aed1` invariant 11 rewording, `1c8c1282` measurements) on the `iter-check-full`
  ceiling after an initial cold run exceeded 25 min wall-clock. Research attributed
  the wall to gorgeous's `default = ["bbnf-grammar", "json-grammar", ...]` which activated
  6 `#[derive(Parser)]` sites in one rustc. Fix: `default = []` + `[[bin]] required-features
  = ["bin-full"]`. Validated by `iter-check-prettify` warm dropping from ~500 s (pre-fix)
  to 33.36 s (post-fix). bbnf-bootstrap's > 600 s single-derive wall emerged as the
  new critical-path ceiling and is routed to AZ-I.W0 (derive-cache relocation + Watt)
  per `B1.md` §Cross-tranche debt. Invariant 11 reworded to the measured truth: B1
  records the ceiling it holds pre-AZ-I; the ≤ 5 min target opens at AZ-I close.
- `d44585bb` — W0 close commit: `scripts/test-tier.sh` bash-3.2 empty-array expansion fix
  (`${ARR[@]+"${ARR[@]}"}` guard); final invariant 11 wording; leaf test tier 582/582
  pass in 45.89 s; `ay-prime` cold + nextest dry-runs routed to AZ-I.W0 with rationale.

Routine surface measured walls (`docs/benchmarks/post-B1-W0-routine.txt`):

| alias | wall | status |
|---|---|---|
| `iter-check` | 3.88 s warm | pass |
| `iter-check-lsp` | 3.27 s warm | pass |
| `iter-check-prettify` | 33.36 s warm | pass (was ~500 s pre-fix) |
| `iter-check-bootstrap` | > 660 s (killed-at-cap) | AZ-I.W0-routed |
| `scripts/test-tier.sh leaf --profile ax-iter` | 45.89 s / 582 pass | pass |

W0 closes on the pinned substrate + full alias surface + nextest 4-profile set +
Makefile rewrite + triumvirate fix + honest invariant 11 ledger. W1 opens on this
baseline.

---

## 2026-04-22 — Concrete rewrite absorbing the 12-step migration

B1's wave plan is rewritten from the original "repair + close" 2-wave
shape into a 4-wave sequence that absorbs the full toolchain migration
plan. The rewrite is informed by these artefacts:

- `docs/tranches/B1/TOOLCHAIN-SOTA.md` (Validation Agent 2 research)
- `docs/tranches/B1/TOOLCHAIN-MIGRATION.md` (12-step sequence + patch
  drafts + risk register)
- `docs/tranches/B1/patches/*` (7 draft files — rust-toolchain.toml,
  config.toml, nextest.toml, Makefile, divan migration, cross-repo
  propagation, derive cache design)
- `docs/tranches/meta-audit/04-toolchain-pain.md` (93-ICE cluster
  evidence + ≥130s bootstrap wall)
- `docs/tranches/meta-audit/07-appurtenant-assay.md` (cross-repo
  propagation matrix)
- `docs/tranches/meta-audit/08-abrogation-catalog.md` (per-script
  DELETE/REWRITE/KEEP verdicts)

### Wave structure

| Wave | Scope | Migration steps | Dispatch |
|---|---|---|---|
| W0 | Substrate pin + alias surface + nextest config + Makefile | 1-4 | 3 parallel + 1 closer |
| W1 | Exemplar divan port + 18 bench ports + bencher removal + iai-callgrind feature | 5-8 | 1 + 4 parallel + 1 |
| W2 | CI rewire + script abrogation + cross-repo pin propagation | 9-11 | 3 parallel |
| W3 | PROFILING.md doc pass + FINAL.md + post-B1.json aggregate | 12 + close | 1 |

### Key sequencing reconciliations

- **Divan lives in B1.W1**, not a successor tranche. The bench harness
  is dev-loop truth; the cold-per-parse invariant (`no-warm-benches`)
  cannot be satisfied by bencher. Bench-framework-dependent script
  abrogations (bench_regression.sh parser rewrite, profile.sh deletion,
  cost-grid-sweep.sh deletion) live in B1.W2.a — they open once divan
  JSON is the committed shape.
- **ICE-clean gate**: W0.d's close measurement clears the ambient-
  nightly ICE corruption by `cargo clean` + pinned toolchain; W1.a
  MUST NOT open until `ls target/rustc-ice-*.txt | wc -l == 0`.
- **Cross-repo scope**: B1.W2.c propagates the pin to bbnf-lang +
  parse-that + pprint only. Wider fleet (gorgeous sibling, csp-solver,
  csc411, crates/ai, etc.) is named in W2.c ledger as
  `DEFERRED to post-B1 appurtenant tranche`.

### Agent dispatch count

- W0: 4 agents (3 parallel on substrate pin / cargo config / nextest
  config, 1 closer on Makefile + ICE-clean verification).
- W1: 6 agents (1 exemplar, 4 parallel bench ports by grammar family,
  1 closer on bencher removal + iai-callgrind).
- W2: 3 agents (CI rewire, script abrogation, cross-repo propagation).
- W3: 1 agent (doc pass + FINAL + AY-II handoff).

Total: 14 agent-slots across the tranche.

### What is NOT in B1

- Derive cache relocation to `$XDG_CACHE_HOME` → BA.
- Watt / WASM-precompiled proc-macros → BA/BB.
- Bench architecture restructure (`crates/bench-*`) → BA.
- Parametric bench collapsing → post-B1 polish.
- sccache (conditional on measured CI cache-hit ≥80%) → TBD.
- Fleet-wide modernization of gorgeous sibling, csp-solver, csc411,
  crates/ai → post-B1 appurtenant tranche.

No implementation wave has been dispatched yet. No runtime work is in
scope for B1.

---

## 2026-04-22 — Annex authored, research waived, AY-II blocked on B1

B1 is promoted from scaffold to authoritative prelude annex. The
separate research wave is waived: AY-II's infra audits plus Wave 1's
TOOLCHAIN-SOTA and TOOLCHAIN-MIGRATION artefacts provide the needed
design-space reduction and evidence.

Authored in the initial state:

- `B1.md` rewritten as the authoritative annex plan.
- `AGENT_DISPATCH.md` added as the B1 dispatch surface.
- `waves/W0.md` and `waves/W1.md` authored (later superseded by the
  4-wave rewrite above).
- AY-II docs redressed so B1 closes before AY-II.W0' close ceremony
  resumes.
