# B1 — Agent Dispatch

Dispatch B1 before any further AY-II runtime work.

This is not a research brief. B1's design space is already constrained
by AY-II's infra audits. The job now is execution: harden the command
surface, delete stale workflow cruft, capture proof artefacts, and
unblock AY-II.W0' close.

## Read first

1. `docs/instructions/README.md`
2. `docs/instructions/PROFILING.md`
3. `docs/instructions/tranche/SPEC.md`
4. `docs/instructions/tranche/WAVE_SPEC.md`
5. `docs/tranches/B0/FINAL.md`
6. `docs/tranches/AY-II/audit/W0p-infra-root-cause.md`
7. `docs/tranches/AY-II/audit/W0p-infra-fix-plan.md`
8. `docs/tranches/AY-II/audit/W0-iter-surface-verification.md`
9. `docs/tranches/B1/B1.md`
10. `docs/tranches/B1/waves/W0.md` or `W1.md` as appropriate

## Program order

1. B1.W0 — command-surface truth + stale-surface deletion +
   profiling/bench/regen hardening.
2. B1.W1 — close-proof capture + FINAL + AY-II handoff.
3. AY-II.W0' close ceremony resumes only after B1 closes.
4. AY-II.W1-W5 then execute sequentially.

## Non-negotiables

- No quick solutions.
- No workarounds.
- No runtime-architecture edits in B1.
- No stale workflow prose left behind.
- No parallel B1/AY-II execution.
- No dead command aliases, comments, or docs preserved for sentiment.

## W0 dispatch decomposition

Use 3 parallel agents on disjoint file bounds. The cold-path +
cache-honesty items from meta-audit-04 fold into the three sub-agents
below per existing file ownership — no separate fourth sub-agent.

Each file is owned by exactly one sub-agent. The shared artefact
`docs/benchmarks/post-B1-W0-proof.txt` is append-only with disjoint
row prefixes per sub-agent — W0.a rows `ay-prime-fresh`, W0.b rows
`bootstrap-*` + `ay-samply-*`, W0.c rows `iter-check-full-*` +
`ay-bench-close-*`. W0.a is the declared consolidator for
`docs/instructions/PROFILING.md` per SPEC §N-agent shared-file
consolidation.

- **W0.a — Routine surface truth + cache-alias separation (PROFILING consolidator)**
  Files (owner-only): `.cargo/config.toml`, `Makefile`,
  `scripts/test-tier.sh`, `docs/instructions/PROFILING.md`,
  `docs/benchmarks/post-B1-W0-routine.txt`.
  Job: align the documented routine surface with the live aliases and
  test tiers; delete stale comments and stale command claims; downgrade
  `iter-check-full` from routine-alias wording to close-ceremony
  wording; add ONE named fast-path cargo alias per excluded crate
  (invariant §10 end-to-end): `iter-check-lsp` (bbnf-analysis +
  bbnf-lsp), `iter-check-prettify` (gorgeous), `iter-check-bootstrap`
  (bbnf-bootstrap); add a new `make ay-prime` target that seeds
  `target/.bbnf-cache/` from a single cold run of the bootstrap +
  gorgeous checks. `.cargo/config.toml` carries a comment-block
  naming the full cost model (routine / per-exclude fast-paths /
  close-gate). Commits PROFILING.md after W0.b + W0.c rows are
  visible in the shared artefact; references those rows from the
  prose.

- **W0.b — Bootstrap/profiling/expand truth + cache preservation**
  Files (owner-only): `scripts/bootstrap-bbnf.sh`,
  `scripts/prepare-profile-wave.sh`,
  `scripts/profile-bench-headless.sh`.
  Shared append-only rows: `bootstrap-*`, `ay-samply-*` in
  `docs/benchmarks/post-B1-W0-proof.txt`.
  Job: make the bootstrap/profile/expand scripts truthful, measured,
  and symbol-resolution-correct. Stop `scripts/bootstrap-bbnf.sh`
  from unconditionally `rm -rf target/.bbnf-cache/` (the content-keyed
  cache at `crates/derive/src/lib.rs:300-358` is the source of truth;
  the nuke defeats it). Record cycle-1 and cycle-2 wall-clock rows in
  `docs/benchmarks/post-B1-W0-proof.txt` (keys `bootstrap-cycle-1`,
  `bootstrap-cycle-2`). Emit samply artefact path rows `ay-samply-*`
  for W0.a to reference from PROFILING.md.

- **W0.c — Proof-surface measurement + CI truth**
  Files (owner-only): `.github/workflows/ci.yml` (only if present),
  `docs/tranches/B1/PROGRESS.md`.
  Shared append-only rows: `iter-check-full-*`, `ay-bench-close-*`
  in `docs/benchmarks/post-B1-W0-proof.txt`.
  Job: measure `cargo iter-check-full` cold wall on a fresh
  `rm -rf target/ax-iter/incremental` and commit the number as the
  `iter-check-full-cold` row; measure `make ay-bench-close
  WAVE=close` outputs and commit as `ay-bench-close-*` rows. The
  recorded numbers ARE the ceilings; any regression beyond them
  re-opens B1.W0.c. W0.c does NOT edit Makefile or PROFILING.md —
  those are W0.a's consolidator surface.

## W1 dispatch decomposition

Use 2 serial agents:

- **W1.a — Close evidence capture**
  Files: `docs/benchmarks/post-B1.json`,
  `docs/tranches/B1/PROGRESS.md`.
  Job: collect the refreshed timing/proof matrix and record exact
  command paths + artefacts.

- **W1.b — FINAL + AY-II handoff**
  Files: `docs/tranches/B1/FINAL.md`,
  `docs/tranches/AY-II/PATH-FORWARD.md`,
  `docs/tranches/AY-II/PROGRESS.md`,
  `docs/tranches/AY-II/AY-II.md`,
  active AY-II wave specs as needed.
  Job: mark B1 closed and AY-II unblocked on the refreshed surface.

## Return discipline

Every sub-agent returns:

1. Commit SHAs in order.
2. Exact artefact paths.
3. Hard-gate status per item.
4. Any stale surface deleted.
5. Empty `git status --short` except expected generated artefacts or
   target symlinks.
