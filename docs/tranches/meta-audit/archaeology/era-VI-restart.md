# Era VI — The Restart (2026-04-20 → 2026-04-22)

Era VI is where the project relearns tranche discipline from the wreckage
of Era V. In three calendar days, eight planning branches, two prelude
annexes, two AY passes, and a meta-audit land. At 2026-04-22's close,
master sits 945 commits ahead of `origin/master` — every Era IV / V / VI
commit the user named when framing the archaeology is *still unpushed*.

Commit density: AY-I 1 tagged (but AY-I ran 28 commits pre-split, per
`docs/tranches/AY-I/FINAL.md`), AY-II 77, AZ 14, B0 17, B1 7 so far;
plus BA / BB / BC scaffolds with plan docs and wave directories but no
tranche-tagged commits yet. The meta-audit itself (this document)
accounts for 6 tranche-tagged commits ending at `48e6eaa9`.

## Architectural thesis

*Stop executing until the dev loop is truthful.*

Per `docs/tranches/B1/B1.md`:

> B1 is the bounded prelude annex that restores truthful development
> infrastructure before AY-II resumes. It owns no runtime architecture.
> It closes only when the public command surface, bootstrap/expand/
> bench/profile surfaces, and their documentation agree on measured
> reality. AY-II.W0' stays paused until B1 closes; AY-II resumes
> immediately afterward on the refreshed proof surface.

The thesis inverts Era V's execution-first posture. Era VI refuses
to open the next runtime wave until the infrastructure that measures
it is trustable. The feedback memories `build-infra-first`,
`iter-profile-always`, `single-cargo-per-target`, `test-output-to-
file`, `bg-then-monitor`, `no-polling-loops`, and `status-tick-
cadence` all land permanently in memory during Era VI.

## Tranche ledger

| Tranche | Date | Commits | Headline | Verdict |
|---|---|---:|---|---|
| AY-I | 2026-04-19 → 2026-04-20 | 28 (pre-split) | Pass I of AY — write-time substrate experiment + direct-to-struct broadening. Parity close gates not met. (`docs/tranches/AY-I/FINAL.md`) | Partial — honest relinquish. |
| AY-II | 2026-04-21 → 2026-04-22 | 77 | Gestalt re-ordered remainder against audit-triumvirate prescriptions. W0' substrate refactors (FusedBuilder collapse, finish→finish_fused rename, STRUCTURAL_SCAN_POLICY at codegen). Currently paused for B1. | Partial — paused. |
| AZ | 2026-04-20 | 14 | Planning phase — 6 research branches (`az-a1` through `az-a6`) across JSON / CSS / Sheets / BBNF parse + compile fresh profiles, Value API, named-preservation fix design. | Planning — no runtime commits; fed AY-II plan. |
| B0 | 2026-04-20 | 17 | Bounded prelude annex — public fast-path defaults, scoped AY command repair, profile-tier split (`ax-iter` / `profiling-prep` / `bench`), Makefile `ay-*` targets. `docs/tranches/B0/FINAL.md` close at HEAD `7b223cf6`. | Worked — 45× iter-check warm speedup. |
| B1 | 2026-04-22 | 7 so far | Dev-loop truth + proof-surface hardening. Successor to B0. Blocks AY-II.W0' resume. Seven waves in directory; W0 scaffolded. | In flight. |
| BA | 2026-04-21 | 0 | Scaffold only — `BA.md`, `PROGRESS.md`, `waves/`. No tranche-tagged commits. | Not started. |
| BB | 2026-04-21 | 0 | Scaffold only. | Not started. |
| BC | 2026-04-21 | 0 | Scaffold only. | Not started. |

## AY-I — Pass I close (per `docs/tranches/AY-I/FINAL.md`)

AY-I dispatched against master HEAD `6516086f` (AX W1r close) and
executed W0 through W6 plus a superseded W7. B0 closed cleanly
before W5 opened.

- **W0** — legacy prune + EBNF + AX FINAL + housekeeping. 7 stale
  test files retired; `crates/tape/src/dta.rs` carved 550 → 80 LOC;
  `shape_dict.rs` deleted; dead `GrammarProfile` fields retired.
  AX FINAL captured. 1491 workspace tests passed. Bootstrap regen
  cycle-1 = cycle-2 byte-identical.
- **W1** — AU AoS substrate revert + Pratt Option C + structural-
  scan. **Columns reverted from 7 structural Vecs to 1
  `Vec<TapeRec>` + parallel `sib_skip`.** This is the direct revert
  of Era IV / Tranche Y's column split. Finaliser stack-buffer
  scratch. Structural-scan substrate + consumer probe landed; eager
  scan retired post-bench regression. Pratt Option C:
  `[LocalOpEntry; 16]` op_stack hoist. Twitter recovered to 688 MB/s
  after W1-fix.
- **W2** — named preservation + G1-G4 canonicalisation + EBNF
  reactivation.
- **W3-W6** — executed per plan.
- **W7** — superseded.

Pass I lands 28 commits across the tranche plus 4 audit-triumvirate
artefacts cherry-picked at relinquish. AY-I tranche HEAD: `321d7418`
(post-audit cherry-pick, pre-split).

**The W1 column revert is the most important architectural reversal
of Era VI.** Tranche Y split the tape into 7 columns; AU baked the
split into its measurement floor; AY-I.W1 reverts to a single
`Vec<TapeRec>` + `sib_skip` because the 7-column AoS lost to
cache-locality of a single AoS record. This is direct evidence that
an Era IV "durable" decision (columnar split) was not durable.

## AY-II — Path forward (per `docs/tranches/AY-II/PATH-FORWARD.md`)

The execution order is:

1. **B1 closes first** as the bounded prelude annex over the dev-loop,
   bootstrap, expand, bench, and profiling surfaces.
2. **AY-II.W0' closes next** on the refreshed surface: regen, alias-
   shim retirement, fresh expand, fat-LTO bench matrix, samply, and
   nm.
3. **AY-II W1-W5 execute sequentially** on that post-B1, post-W0'
   truth.

W0' sub-phases already landed in source:

- W0'.a — `FusedBuilder` collapse (`bd563c1d`), `finish` →
  `finish_fused` rename (`4edfac88`), retire standalone
  `ValueBuilder` (`9c9906c8`), single FusedBuilder parse-entry
  (`0beda457`).
- W0'.b — `materialize_projection_*_<G>` routing, raw name for
  materializer lookup, runtime tape path.
- W0'.c — `scan_policy` match arms through raw rule names, retire
  W0-era `#[allow(dead_code)]`, `STRUCTURAL_SCAN_POLICY` splice into
  `emit_path_walk` at codegen.
- W0'.d1 — migrate `push_compound` / `mark_children` tests →
  `FusedBuilder` API.
- W0'.d3 — `O(1) direct_child_count` in `value_end_compound`
  (`f768f50d`).
- W0'.d4-d7 — gate `gorgeous` derive sites, drop `gorgeous` mandatory
  dev-dep, narrow build.rs fingerprint, exclude heavy proc-macro
  crates from iter-check.

`generated.rs` is still pre-regen with the bridge-era parse entry,
so AY-II.W0' is **not** formally closed. The W0'.a compose-boundary
aliases + shim surfaces are still present *by design* until the post-
B1 regen replaces them — per `no-backward-compat` memory's extension
to "shims may exist but only across a known-transient boundary."

## B0 + B1 — The infra prelude (per feedback memory `build-infra-first`)

B0 closed at commit `7b223cf6` with three intentional profile tiers:

- `ax-iter` — fast iteration (stripped symbols, incremental).
- `profiling-prep` — full symbols (debug=true) for samply.
- `bench` — release LTO for authoritative numbers.

Ten `ay-*` Makefile targets implementing the exact commands the AY
hard gates cite (`ay-iter-check`, `ay-expand-rust`, `ay-bench-
parse`, etc.). The user feedback memory `iter-profile-always`
requires every dev `cargo check / test` carry `--profile ax-iter`
explicitly; B0's Makefile surfaces enforce it.

B1 extends B0 with the seven-wave schedule in `docs/tranches/B1/
waves/W0.md` through `W6.md`. B1.W0 through W5 are file-bound
disjoint agent dispatches across `cargo aliases`, `Makefile
targets`, `bootstrap commands`, `cargo expand surface`, `bench
orchestration`, `profiling scripts`, `CI defaults`, and `docs/
instructions/*`. W6 is the close wave.

## AZ — The planning-only tranche

AZ is unusual: 14 commits, all `docs(next-tranche):` — zero runtime
code. `az-a1` through `az-a6` are planning branches each providing
one research artefact:

- `az-a1` — JSON parse fresh profile.
- `az-a2-css-l4-parse-fresh` — CSS L4 parse fresh profile.
- `az-a3-sheets-bbnf` — Sheets + BBNF parse fresh profile.
- `az-a3-value-api` — Value API + JSON apples-to-apples.
- `az-a4-compile-fresh` — compile-time fresh audit.
- `az-a5` — Value API design audit.
- `az-a6-named-preservation` — named-type preservation fix design.

These are the fresh-profile inputs that AY-II W1-W5 will consume
once B1 closes. AZ is the first pure-planning tranche the project
has produced; every prior tranche mixed planning with execution.

## What landed durably

- **Column revert** (AY-I.W1) — 7-Vec → 1-Vec + `sib_skip`.
- **FusedBuilder unification** (AY-II.W0'.a).
- **Profile-tier split** (B0.W0) — three intentional profiles.
- **`ay-*` Makefile targets** — 10 public fast-path commands.
- **Meta-audit protocol** — four agents, four axes of investigation,
  disjoint file bounds. Artefacts: `01-session-friction.md`,
  `02-instruction-adherence.md`, `03-tranche-drift.md`, `04-toolchain-
  pain.md`, and the commit archaeology this document represents.
- **User-led normalization commit `fef4416c`** — across tranches/ +
  instructions/ + B1 rename. User directly edited to normalise
  vocabulary the planning-heavy Era-VI work had fragmented.
- **Triumvirate discipline** (memory `triumvirate-discipline` +
  `triumvirate-auto-trigger`) — research-commit → plan-commit →
  redress-dispatch pattern.
- **Six-era era taxonomy** — this archaeology itself makes the
  frame explicit.

## What was reverted or superseded

- **Y's 7-column split** (column revert at AY-I.W1).
- **AW-V thesis** — "auto-derive the sonic-rs-class inner loop from
  any BBNF grammar" — absorbed into AX.W1r's view-layer + canonical-
  parity surface, which does not require the auto-derive.
- **Seven stale wire-contract + emitter-shape tests** retired at
  AY.W0.1 (`69303e10`).
- **Two additional stale shape-emit tests** retired at `d427d282`.
- **AY-II.W0 "parallel-infra path"** — explicitly abandoned in
  `PATH-FORWARD.md`: "AY-II is not on a parallel-infra path any
  more."

## Salvageable artefacts (still present at close)

- `docs/tranches/meta-audit/` — the retrospective surface.
- `docs/tranches/B1/` — the dev-loop prelude.
- `docs/tranches/B0/FINAL.md` — the first `FINAL.md` for a bounded
  annex.
- `.cargo/config.toml` `[alias]` block — iter-check etc.
- `Makefile` "AY Iteration Surface" — the public command surface.
- `docs/instructions/PROFILING.md` — profiling command canon.
- `post-B0-W0-baseline.txt`, `post-B0-W0-mid.json` — infra
  measurement anchors.
- `docs/tranches/AY-I/FINAL.md` + `docs/tranches/AY-I/audit/` —
  honest relinquish record.
- `docs/tranches/AY-II/PATH-FORWARD.md` — current ordered plan.

## The unpushed story

`git log --oneline origin/master..HEAD | wc -l` = **945**.

Every commit from Era IV's AU close (2026-04-15) through Era VI's
meta-audit (2026-04-22) is unpushed. This is by choice — the
orchestrator elected to keep upstream at the pre-AU baseline while
the DTA/PSI experiment resolved. AX's W0b interpreter deletion and
W1r parity-harness surface land on master but not upstream.

The 945 commits break down roughly:

- AU (14-22 local on master) + pre-AU overlap ~30.
- AV ~53 + AW cluster ~250 (AW + AW-II + AW-III + AW-IV + AW-V).
- AX ~169.
- AY-I + AY-II + AZ + B0 + B1 ~130.
- Infill (bench regens, doc commits, orchestrator housekeeping)
  ~300.

The 24 non-worktree feature branches (`au4-bootstrap-regen`,
`aw5-w1-1`, `ax-w1r-*`, `ay-*`, `az-*`) carry a further ~35 commits
not on master. Those are per-agent worktree cherry-pick sources that
were folded back during wave close; they represent little
incremental work beyond master.

## Transition out of Era VI

Era VI is in flight at 2026-04-22. The close condition is:

1. B1 closes with the dev-loop truthful.
2. AY-II.W0' regens `generated.rs`, retires W0'.a shims, publishes
   fat-LTO bench matrix + samply + nm.
3. AY-II W1–W5 execute sequentially on the refreshed surface.
4. Master pushes to origin (`git push`) once AY-II's close matches
   or exceeds post-AU on the bench matrix.

The next era — provisionally Era VII — opens when AY-II's bench
matrix exceeds post-AU. Until then, Era VI continues.
