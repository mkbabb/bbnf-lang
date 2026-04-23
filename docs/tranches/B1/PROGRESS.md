# B1 — Progress Log

Dated execution log for tranche B1, the bounded prelude annex that
refreshes the development/proof surface before AY-II resumes.

- `Status`: planned (waves W0→W3 sequenced per `TOOLCHAIN-MIGRATION.md`
  12-step plan)
- `Current wave`: W0 (planned)
- `Next wave`: W1 (opens on W0 + ICE-clean gate)

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
