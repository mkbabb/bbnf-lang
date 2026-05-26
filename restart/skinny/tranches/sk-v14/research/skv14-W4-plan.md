# SK-V14 W4 Plan: Reject Unsafe PRUNE-2 Deletion

Date: 2026-05-26.
Wave: W4.
Phase: plan.
Disposition: REJECT current SPEC shape.

## Decision

Do not delete the CSS L4 template/provider/runtime mesh under the current W4
section. The W4 exit gate requires `cargo xtask regen-css` after deleting the
seven provider modules, but the current `regen-css` path compiles through
those same providers.

## Falsifiability Gate

The current W4 shape is rejected if deleting:

- `skinny/crates/codegen/src/css_l4_*_templates/`
- `skinny/crates/codegen/src/css_l4_*_provider.rs`
- `skinny/crates/runtime/src/grammars/css_l4_*/`

causes `cargo xtask regen-css` to fail before regenerating the skinny runtime
twins. The throwaway-worktree probe in `skv14-W4-B-provider-cycle.md` proves
that failure.

## Forbidden Routes

- Do not preserve the seven provider modules while claiming W4 provider
  deletion.
- Do not copy checked-in runtime files or templates as a replacement for
  generation.
- Do not hand-patch generated runtime output.
- Do not move any CSS L4 row to ADMITTED or claim W4 row closure.
- Do not touch `crates/core/src/runtime/css_l4/`; W6.0 still owns that root.

## Corrective Route

Trigger Pass Omega V4 with a W4R corrective packet. The local amendment should
move the CSS provider/template deletion into W5, where the generic
grammar-provider replacement is already owned, and leave W4 as a ledger-only
PRUNE wave after W3:

1. W4R/W4: revert rolling delta CSS L4 status to 0/24 and add 24
   row-keyed REDRESS entries against `v1 §1-6`; do not delete providers yet.
2. W5: collapse provider/template infrastructure into the generic provider
   and delete the seven CSS provider modules plus seven template directories
   in the same commit as their replacement.
3. W6: continue with the already-amended W6.0 CSS L4 root-runtime collapse
   and W6.1-W6.8 remaining Pattern H dirs.

This preserves PRUNE-before-rebuild while moving deletion to the wave that
owns the replacement generator.
