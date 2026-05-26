# Omega-A Coherence Audit - Pass Omega V4 W4R

Pass: Pass Omega V4.
Date: 2026-05-26.
Scope: REDRESS-184 / W4R amended SK-V14 wave graph.
Boundary: audit only; no V1 or SK-V14 dispatch surface is edited by this
artifact.

## Verdict

ACCEPT-WITH-REQUIRED-SURFACE-AMENDMENTS.

W4R removes the current W4 graph cycle by moving CSS L4 provider/template
deletion out of W4 and into W5, where the replacement grammar-agnostic provider
is already owned. W4 becomes the ledger-only CSS L4 admit-prune wave; W5
absorbs the provider/template deletion and `regen_css.rs` migration in the same
replacement commit.

## Pre-Amendment Cycle

Current W4 requires:

- deleting seven `skinny/crates/codegen/src/css_l4_*_provider.rs` modules and
  seven `skinny/crates/codegen/src/css_l4_*_templates/` directories
  (`SPEC.md:572`-`:574`, `:593`-`:596`);
- then running `cargo xtask regen-css` with an empty diff
  (`SPEC.md:603`-`:605`).

Current `regen-css` still calls `codegen::emit_runtime_profile(...)`
(`skinny/xtask/src/regen.rs:18`), and `codegen` still imports and dispatches
through those seven CSS providers (`skinny/crates/codegen/src/lib.rs:1`-`:7`,
`:166`-`:208`; `skinny/crates/codegen/src/grammar_profile.rs:100`-`:110`).
The generic provider replacement is W5 work (`SPEC.md:633`-`:658`), but W5
requires W4 admitted (`SPEC.md:646`-`:648`).

That creates this cycle:

`W5 replacement -> W4 deletion/regen -> W5 entry`

REDRESS-184 records the executable throwaway-worktree proof: after deleting
the W4 provider/template/runtime surface, `cargo xtask regen-css` fails with
`error[E0583]` for all seven `css_l4_*_provider` modules before regeneration.

## Amended Graph

W4R removes the back-edge by aligning deletion with replacement:

- W4 owns only the CSS L4 admit-ledger prune:
  `restart/skinny/ROLLING-SOTA-DELTA.md`, `skinny/RESULTS.md` if needed, and
  `skinny/REDRESS.md`.
- W5 owns the generic provider replacement and deletes the seven CSS provider
  modules plus template directories in the same commit as the replacement.
- W6 remains unchanged from V3: W6.0 CSS L4 root-runtime collapse, then
  W6.1-W6.8 remaining Pattern H dirs.

The resulting order is acyclic:

`W0 -> W1 -> W2 -> W3 -> W4-ledger -> W5-provider-collapse+delete -> W6.0 -> W6.1..W6.8 -> W7 -> W8/W9/W10 -> W11`

The global PRUNE-before-new-admit rule still controls W8/W9/W10.

## Required Surface Amendments

| Surface | Disposition |
|---|---|
| `restart/skinny/tranches/sk-v14/SPEC.md` | Required. Change W4 to ledger-only CSS admit prune; move provider/template deletion and runtime re-emission proof into W5; fix rolling delta path to `restart/skinny/ROLLING-SOTA-DELTA.md`; preserve W6.0. |
| `restart/skinny/tranches/sk-v14/SYNTHESIS.md` | Required. R3/C-5 must say PRUNE-2 reverts CSS admits while PRUNE-3 deletes CSS providers/templates with the generic replacement. |
| `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` | Required. R3/PRUNE wording must match the amended W4/W5 split. |
| `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` | Required. Add W4R gate: do not dispatch W5/provider deletion until G-Omega V4 and amended W4 admission. |
| `restart/MASTER-PLAN.md` | Required. Update §13.3 W4/W5 rows. |
| `restart/HANDOFF.md` | Required. Record W2/W3 admission, REDRESS-184, W4R gate, and refresh the stale SPEC line-count/cycle-status wording. |
| `restart/MIGRATION.md` | Required. Add W4R receiver under the V3 W2R receiver and mark W2 rejection wording as superseded by amended W2 admission. |
| `restart/skinny/{INDEX,WORKSPACE,HARDENING}.md` | Required limited wording alignment. BENCH/COMPILER/SUBSTRATE read no-op; no W4R-specific drift was found. |
| `skinny/REDRESS.md` | Required narrow supersession note for REDRESS-183: W2R admitted at `45568e669`; current blocker is REDRESS-184. |
| `restart/ARCHITECTURE.md` | No-op. W4R changes wave ownership, not architecture. |
| `restart/locks/LOCKS.md` | No-op. W4R changes wave ownership, not lock semantics. |

## Citation Corrections

V4 CRUD must correct active dispatch-surface references to the rolling delta
path. `skinny/ROLLING-SOTA-DELTA.md` does not exist; the live file is
`restart/skinny/ROLLING-SOTA-DELTA.md`. Required corrections include SK-V14
`SPEC.md` references in the setup, W4 owner paths, W8/W9/W10 entry text, and
W11 close text, plus `DISPATCH-PROMPT.md` authority and post-redress update
lists.

CSS L4 row wording should normalize to **24 operational CSS L4 row keys** when
describing W4/W8 work. Historical "25 CSS" wording may survive only when it
explicitly refers to the broader historical source-row narrative.

## Gate Binding

Until G-Omega V4 authorizes W4R, do not patch dispatch surfaces, do not delete
CSS provider/template directories, and do not dispatch W5.
