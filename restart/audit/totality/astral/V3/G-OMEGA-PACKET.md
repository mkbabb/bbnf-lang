# G-Omega Packet - Pass Omega V3 W2R

Status: mandatory user gate packet.
Date: 2026-05-25.
Do not apply CRUD until the user authorizes G-Omega.

## Cohort Lock Declaration

Pass Omega V3 consumes:

- T-P3 V4 LOCK packet already reflected by Pass Omega V2.
- REDRESS-183 W2 rejection.
- `restart/skinny/tranches/sk-v14/research/skv14-W2R-corrective-packet.md`.
- Omega-A through Omega-F V3 artifacts in this directory.

Omega V3 disposition before CRUD: W2R is coherent, zero-lock-change, and
requires SPEC / SYNTHESIS / MASTER / HANDOFF / MIGRATION / limited skinny-corpus
surface updates before W2 can rerun.

## Proposed Locks Diff

`restart/audit/totality/astral/V3/locks-diff.md`: zero delta. CRUD-3 is read/no-op.

## Proposed Master Plan / SPEC Diff

`restart/audit/totality/astral/V3/master-plan-diff.md`:

- W2 becomes skinny-side `regen-css` only.
- W6 becomes W6.0 CSS L4 root-runtime collapse plus W6.1-W6.8 remaining dirs.
- W6 remains nine sub-waves with <=90 min per-sub-wave and <=810 min aggregate caps.
- W8/W9/W10 remain globally blocked until PRUNE-1..PRUNE-5 close.

## Proposed CRUD Operations

| CRUD | Surface | Operation after G-Omega |
|---|---|---|
| CRUD-1 | `restart/ARCHITECTURE.md` | Read/no-op. Existing Pattern H facts already match W2R. |
| CRUD-2 | `restart/MASTER-PLAN.md` | Update §13.3 W2/W6 rows and W2R receiver note. |
| CRUD-3 | `restart/locks/LOCKS.md` | Read/no-op; preserve 16 locks. |
| CRUD-4 | `restart/HANDOFF.md`, `restart/MIGRATION.md` | Record W2 rejection, W2R block, W6.0 ownership, and next dispatch directive. |
| CRUD-5 | `restart/skinny/{INDEX,WORKSPACE,HARDENING}.md` plus tranche-local W2R pointers | Limited text alignment; BENCH/COMPILER/SUBSTRATE no-op; no RESULTS/REDRESS/source/generated movement. |
| CRUD-6 | audit packet | Write post-authorization CRUD log and signoff; no legacy nuke needed for W2R. |
| SPEC patch | `restart/skinny/tranches/sk-v14/SPEC.md`, `SYNTHESIS.md` | Apply amended dispatch authority under the same G-Omega authorization. |
| Tranche dispatch patch | `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` | Update R4/PRUNE-4 wording to W2 skinny-only, W6.0 CSS L4 root-runtime collapse, and W6.1-W6.8 remaining Pattern H dirs. |
| Tranche handoff patch | `restart/skinny/tranches/sk-v14/HANDOFF.md` | Record REDRESS-183, W2 rejected pending amended rerun, and W3+ blocked until amended W2 admits. |
| Tranche dispatch-prompt patch | `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` | Add pre-dispatch verification that W2R G-Omega + CRUD landed and W2 admitted under the amended skinny-only gate before W3+ dispatch. |

## Amended W2 Executable Companion Roster

Post-CRUD W2 must author these exact companion commands and run them in the W2
gate:

```sh
cargo xtask regen-css
cargo xtask check-css-l4-at-rules-and-media
cargo xtask check-css-l4-declaration-values
cargo xtask check-css-l4-declaration-values-extended
cargo xtask check-css-l4-nested-layout
cargo xtask check-css-l4-stylesheet-selectors
cargo xtask check-css-l4-vendor-and-custom-atrules
cargo xtask check-css-l4-visual-functions
rm -rf skinny/crates/runtime/src/grammars/css_l4_* &&
  cargo xtask regen-css &&
  git diff --exit-code -- skinny/crates/runtime/src/grammars
```

The roster derives from the seven current skinny CSS L4 runtime directories:
`css_l4_at_rules_and_media`, `css_l4_declaration_values`,
`css_l4_declaration_values_extended`, `css_l4_nested_layout`,
`css_l4_stylesheet_selectors`, `css_l4_vendor_and_custom_atrules`, and
`css_l4_visual_functions`. W2 may not touch or claim closure over
`crates/core/src/runtime/css_l4/`.

## Gate Question

Choose one:

1. Authorise: close G-Omega V3 and apply the proposed CRUD / SPEC patches.
2. Hold for review: stop before applying any patch.
3. V5 extra confirming wave: run another challenge/fold cycle before CRUD.
