# CH6 Next-Tranche Impact - Pass Omega V3 W2R

Verdict: ACCEPT after fold.

Initial CH6 returned REVISE on two defects:

- D1: `G-OMEGA-PACKET.md` did not explicitly route
  `ORCHESTRATOR-PROMPT.md`, SK-V14 `HANDOFF.md`, and `DISPATCH-PROMPT.md`.
- D2: W2 companion checks were named as placeholders rather than executable
  commands.

The fold commit `f2c0e6034` fixes both defects. CH6 rerun accepts the revised
packet.

## Prior Defect Verification

| Prior defect | Result | Evidence |
|---|---|---|
| D1 missing tranche-local routing | Fixed. `G-OMEGA-PACKET.md` now lists tranche dispatch, handoff, and dispatch-prompt patches for `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md`, `restart/skinny/tranches/sk-v14/HANDOFF.md`, and `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md`. `master-plan-diff.md` includes proposed patch hunks for all three. `Omega-E` also routes all three as tranche-local amendment receivers. |
| D2 non-executable companion checks | Fixed. `G-OMEGA-PACKET.md` provides an executable shell roster, and `Omega-F` repeats it in the next dispatch directive. The seven companion checks are exact `cargo xtask` invocations, followed by the skinny-only destructive round-trip. |

Exact W2 companion roster verified in the revised packet:

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

## CH6 Checks

| Check | Result |
|---|---|
| G-Omega items concretely measurable | PASS. `G-OMEGA-PACKET.md` presents consumed inputs, zero-delta locks diff, master/SPEC diff pointer, CRUD list, exact W2 command roster, and three explicit gate choices. |
| CRUD list complete | PASS. CRUD-1 through CRUD-6 are listed, with SPEC/SYNTHESIS and tranche-local dispatch surfaces carried under the same G-Omega authorization. |
| Blocked state clear | PASS. No W2 rerun and no W3+ dispatch until G-Omega authorizes W2R and amended W2 re-admits. W8/W9/W10 remain blocked until PRUNE-1..PRUNE-5 close. |
| W2 rerun conditions clear | PASS. Rerun requires G-Omega + CRUD, skinny-only `regen-css`, seven exact companion checks, skinny-only destructive round-trip, bypass-header detector preservation, no CSS row movement, no `crates/core/src/runtime/css_l4/` touch, and no Pattern H closure claim. |
| SK-V14/SK-V15 path clear | PASS. `Omega-F` directs W2 rerun, then W3, W4, W5, W6.0..W6.8, W7, then W8/W9/W10 only after PRUNE closure; W11 closes SK-V14 or brackets SK-V15 through Pass Alpha. |
| PASS-OMEGA compliance | PASS. The revised packet preserves G-Omega sign-off before CRUD/spec-surface changes, keeps locks amendment as zero delta, and supplies measurable next-dispatch entry conditions. |

## Conclusion

The revised committed packet fixes both prior CH6 defects. The next-tranche
impact instructions are concrete enough to dispatch safely after G-Omega.
