# SK-V14 W4R Plan: CSS L4 Ledger PRUNE

Date: 2026-05-26.
Wave: W4R.
Phase: plan.
Disposition: PROCEED.

## Decision

Close amended W4 as a ledger-only PRUNE wave. The implementation changes only
the CSS L4 admission ledger and REDRESS ledger:

1. restore the 24 CSS L4 row keys in `restart/skinny/ROLLING-SOTA-DELTA.md`
   from `ADMITTED` to `OPEN`;
2. preserve `skinny/RESULTS.md` because all 24 CSS L4 rows already retain
   `AUDIT-FALSIFIED`;
3. add REDRESS-185 through REDRESS-208, one row-keyed entry per CSS L4 row;
4. prove no CSS source, generator, provider, template, runtime-twin, or
   `regen_css` deletion occurred in W4.

## Owner Paths

- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W4R-close.md`
- `restart/skinny/tranches/sk-v14/HANDOFF.md`

`skinny/RESULTS.md` is a read/no-op owner surface unless the overlay has
drifted before redress execution.

## Explicit Non-Owner Paths

- `grammar/css/l4/`
- `skinny/crates/codegen/src/css_l4_*_provider.rs`
- `skinny/crates/codegen/src/css_l4_*_templates/`
- `skinny/crates/runtime/src/grammars/css_l4_*/`
- `skinny/xtask/src/regen_css.rs`
- `crates/core/src/runtime/css_l4/`
- `crates/core/src/grammar/generated/css_l4.rs`
- `crates/core/src/grammar/generated/css_l4.registry.json`

W5 inherits provider/template deletion and `regen_css.rs` migration after W4
closes. W6.0 retains the root-runtime CSS L4 collapse.

## Falsifiability Gates

W4R is rejected if any of these fail:

- CSS L4 rolling-delta row count is 24 and admitted count is 0.
- JSON rolling-delta admitted counts remain 0/17, 0/17, and 0/17 for
  parse-only, direct-to-struct, and real-typed-struct.
- `skinny/RESULTS.md` reports `AUDIT-FALSIFIED` on all 24 CSS L4 rows.
- REDRESS-185 through REDRESS-208 exist and are row-keyed.
- Deletion-filter diff over CSS source/generator/provider/template/runtime
  paths is empty.
- Preserved file counts are 15 CSS sources, 7 provider modules, 35 template
  files, and 35 skinny runtime CSS files.
- Lock-count and Pattern H invariants remain unchanged.

## Pre-Blocked Routes

- deleting CSS provider/template/runtime paths in W4;
- hand-patching generated CSS runtime output;
- admitting any CSS L4 row before W8;
- moving W5 provider replacement into W4;
- touching `crates/core/src/runtime/css_l4/` before W6.0.

## Same-Wave Consumer

The same-wave consumer is the ledger itself:

- `restart/skinny/ROLLING-SOTA-DELTA.md` must show CSS L4 at 0/24 admitted;
- `skinny/REDRESS.md` must carry the 24 row-keyed reclassification entries;
- no source-generation path changes are made or consumed in W4.

## Revert Protocol

If the ledger or REDRESS row map cannot be made exact, revert the W4R ledger
slice and add a new REDRESS entry naming the missing row key, missing
validation citation, or failed no-deletion proof. Do not patch source
generation to force a W4 admit.

## Downstream Route

After W4 closes, dispatch W5 PRUNE-3. W5 owns the generic provider path,
CSS provider/template deletion, and `regen_css.rs` migration in one
replacement slice. W6.0 then owns CSS L4 root-runtime collapse.
