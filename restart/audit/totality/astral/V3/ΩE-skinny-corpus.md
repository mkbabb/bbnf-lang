# Omega-E Skinny Corpus Alignment - Pass Omega V3 W2R

Pass: Pass Omega V3.
Date: 2026-05-25.
Scope: `restart/skinny/{INDEX,SUBSTRATE,COMPILER,BENCH,HARDENING,WORKSPACE}.md`
alignment for W2R.
Status: proposed CRUD-5 routing only; no skinny corpus surface is edited here.

## Verdict

ACCEPT-WITH-LIMITED-CRUD-5.

Only skinny corpus surfaces that summarize dispatch posture or hardening
refusals require text updates. Technical substrate/compiler/bench rules remain
valid because W2R does not alter locks, BackendShape, substrate union,
benchmark semantics, or row admission criteria.

## Surface Disposition

| Surface | Current evidence | Disposition |
|---|---|---|
| `restart/skinny/INDEX.md` | Lines `5`-`15` and `19`-`28` summarize active W0..W11 and name W2 regen-css plus W6 9 sub-waves. | CRUD-5 update required: W2 is skinny-side `regen-css`; W6 is W6.0 CSS L4 root-runtime collapse plus W6.1..W6.8 remaining dirs; dispatch blocked until G-Omega and W2 re-admit. |
| `restart/skinny/WORKSPACE.md` | Lines `43`-`52` record Pattern H = 67; lines `55`-`65` describe SK-V14 W0..W11 execution after V2. | CRUD-5 update required: add W2R receiver note near the SK-V14 execution paragraph. Existing Pattern H count remains correct. |
| `restart/skinny/HARDENING.md` | Lines `29`-`43` cover hardening refusal posture but do not yet name W2R. | CRUD-5 update required: add refusal for any W3-or-later plan before G-Omega accepts W2R and amended W2 admits; reject any W2 plan that touches or claims closure over `crates/core/src/runtime/css_l4/`. |
| `restart/skinny/BENCH.md` | Lines `29`-`61` govern telemetry/comparators and already forbid RESULTS/gate mutation from the doc. | No-op. W2R changes wave ownership, not bench comparator or telemetry semantics. |
| `restart/skinny/COMPILER.md` | Lines `41`-`52` record SK-V14 compiler receiver and Pattern H = 67. | No-op. Compiler architecture unchanged. |
| `restart/skinny/SUBSTRATE.md` | Lines `33`-`62` record substrate receiver and close posture. | No-op. W2R does not alter substrate union or retained-state rules. |

## Proposed CRUD-5 Text

Recommended `INDEX.md` receiver note:

```markdown
Pass Omega V3 W2R correction (pending / after G-Omega if authorized):
REDRESS-183 rejected W2's dual-tree `regen-css` gate because root
`crates/core/src/runtime/css_l4/` remains W6 Pattern H work. W2 is
skinny-side only (`skinny/crates/runtime/src/grammars/css_l4_*`); W6.0 owns
CSS L4 root-runtime collapse before W6.1-W6.8. Until G-Omega authorizes the
patch and W2 re-admits under the amended gate, W3+ and all new-admit waves
remain blocked.
```

Recommended `WORKSPACE.md` receiver note:

```markdown
REDRESS-183 blocks W3+ from the current state. After G-Omega accepts W2R,
W2 reruns as skinny-side `regen-css` only; `crates/core/src/runtime/css_l4/`
remains Pattern H until W6.0.
```

Recommended `HARDENING.md` refusal:

```markdown
Reject any W3-or-later plan before G-Omega accepts W2R and W2 reruns/admit
under the amended skinny-only gate. Reject any W2 plan that touches or claims
closure over `crates/core/src/runtime/css_l4/`; W6.0 owns that tree.
```

Recommended no-op confirmation for BENCH/COMPILER/SUBSTRATE:

- Preserve `Pattern H = 67`.
- Preserve the <=90 min per-sub-wave / <=810 min aggregate W6 cap.
- Preserve benchmark comparator / telemetry semantics.

## Tranche-Local Amendment Receivers

These are not CRUD-5 skinny-corpus edits, but G-Omega must carry them with the
W2R packet before any rerun:

- `restart/skinny/tranches/sk-v14/SPEC.md`: remove
  `crates/core/src/runtime/css_l4/` from W2 owner paths/tasks/round-trip gate;
  make W2 destructive check skinny-only; keep root bypass-header detector as a
  fail-unless-W6-owned guard; replace the stale downstream note; move CSS L4
  root-runtime collapse to W6.0.
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md`: update R4/C-3 dual-tree
  wording so W2 owns skinny-side emission and W6.0 owns
  `crates/core/src/runtime/css_l4/`.
- `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md`: update PRUNE-4/R4
  wording from stale 64/8 and ambiguous regen-css to W6.0 plus eight remaining
  sub-waves and skinny-only W2.
- `restart/skinny/tranches/sk-v14/HANDOFF.md`: replace stale post-PRUNE /
  xtask-built posture with the W2R state: W2 is rejected pending amended rerun
  and W3+ is blocked.
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md`: strengthen pre-dispatch
  verification to require amended W2 admit before W3+.

## Explicit Non-Changes

- No `skinny/RESULTS.md` movement.
- No `skinny/REDRESS.md` movement beyond REDRESS-183 already landed.
- No source, generated output, benchmark, gate, or report edit.
- No substrate / BackendShape / Lock 14 semantic change.
