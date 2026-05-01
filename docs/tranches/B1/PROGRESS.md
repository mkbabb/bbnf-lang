# B1 — Progress Log

Dated execution log for tranche B1, the bounded prelude annex that
refreshes the development/proof surface before AY-II resumes.

- `Status`: closed (W4 amendment 2026-04-24)
- `Current wave`: W4 (amendment closed)
- `Successor`: AY-II.W0' close ceremony (unblocked on this baseline)

---

## 2026-04-24 — W4 amendment (B0-adherence restoration)

Post-W3 close review surfaced an adherence gap: the W0.d Makefile
simplification deleted the AY W5-W7 gate-command surface while
`PROFILING.md` still referenced those targets, and `.cargo/config.toml`
lacked B0's three-tier `prep-bench` / `final-bench` aliases and the
`iter-test-leaf` / `iter-test-grammar` aliases named in B1.md
hard-gate item 5. `waves/W0p.md` §Orchestrator close ceremony step 4
explicitly invokes `make ay-bench-close WAVE=W0p-close`; without this
amendment the ceremony would be undispatchable.

Amendment: `b3c50581` — restore the 160-line AY W5-W7 Gate Commands
block in `Makefile` (preserved from pre-W0.d state, invariant-12
corrected — cache-wipe line removed from `ay-bench-close`), add
`prep-bench`, `final-bench`, `iter-test-leaf`, `iter-test-grammar`,
`expand-{json,css,bbnf,sheets}`, `asm-parse` aliases to
`.cargo/config.toml`. Profile definitions (`profiling-prep`, `bench`)
already live in workspace `Cargo.toml`; no additional profile stanzas
needed.

The amendment is scoped strictly to infra restoration — no new runtime
architecture, no behavior beyond reinstating the B0-established public
surface PROFILING.md describes. B1 remains a bounded prelude annex.

---

## 2026-04-24 — B1 closed (W3 close ceremony)

W3 lands as one commit: `PROFILING.md` refresh + `post-B1.json`
aggregate + `FINAL.md` + AY-II handoff updates + this close entry.

Wave close commits:

| Wave | Closing commit | Headline |
|---|---|---|
| W0 | `1d0815dc` | substrate pin + alias surface + nextest config + Makefile rewrite + triumvirate Change 1 |
| W1 | `f9c3db38` | divan exemplar port + 19 file ports + bencher removal + iai-callgrind feature gate |
| W2 | `d276934a` | CI rewire + 5 ABROGATEs + bootstrap cache-guard + cross-repo pin triad |
| W3 | (this commit) | PROFILING refresh + post-B1.json + FINAL + AY-II handoff |

Net landings: substrate pinned (`nightly-2026-04-11`); alias surface
rewritten in `.cargo/config.toml` (4-exclude `iter-check` + per-exclude
fast-paths + 7 `bench-*` aliases + 3 `expand`/`asm` aliases + 6
`iter-test*` aliases); Makefile reduced 470 → 210 lines; nextest
4-profile config (default / ax-iter / ci / close); divan harness across
all 19 bench files plus cross-crate `json_value`; `bencher` purged
from both `Cargo.toml` files plus `Cargo.lock`; iai-callgrind feature
gated and Linux-target-only; `ci.yml` rewired to nextest with junit
upload; `bench-iai.yml` promoted with valgrind setup; 5 scripts
deleted (`profile.sh`, `cost-grid-sweep.sh`,
`check-cst-invariants.sh`, `verify-w2-asm.sh`, `verify-w2-symbols.sh`)
in same commit as their replacements; `bootstrap-bbnf.sh` rewritten
with cache-guard preserving `target/.bbnf-cache/` (invariant 12);
sibling repos (`../parse-that`, `../pprint`) synced to the same pin.

AY-II handoff: B1 close is the predecessor for AY-II.W0' close
ceremony per `docs/tranches/AY-II/PATH-FORWARD.md` 2026-04-24 entry
and per `docs/tranches/AY-II/AY-II.md` Wave summary W0' row. The
ceremony (regen + double-regen + retire compose aliases + fresh
expands + fat-LTO 5-bench matrix + samply per primary grammar + nm)
opens immediately on this baseline.

Cross-tranche debt forwarded: derive cache relocation to
`$XDG_CACHE_HOME/bbnf-derive/` + Watt proc-macro precompilation routed
to AZ-I.W0; per-bench divan JSON emissions + nextest dry-run wall
captures routed to AZ-I.W0 (both gated on the bootstrap > 600 s wall
that AZ-I closes); fleet-wide modernization (gorgeous sibling +
csp-solver + csc411 + crates/ai) routed to a post-B1 appurtenant
tranche; deferred REPLACE / KEEP-MODERNIZE / FOLD-INTO-TOOLING
abrogation entries routed to post-B1 polish per
`docs/tranches/B1/FINAL.md` §Cross-tranche debt ledger.

The defensible floor holds: truthful routine command docs under a
pinned substrate; measured bootstrap/expand/profile/bench proof
surfaces on divan; abrogation catalog executed or explicitly deferred
with owner; cross-repo pin triad in sync; AY-II handoff that no
longer depends on ambiguous infrastructure claims.

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

Routine surface measured walls (`docs/benchmarks/archive/post-B1-W0-routine.txt`):

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
