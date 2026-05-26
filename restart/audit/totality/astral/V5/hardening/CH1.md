# Pass Omega V5 CH1 Correctness

Date: 2026-05-26.
Scope: V5 W5R packet correctness.
Verdict: ACCEPT.

## Finding

No CH1 correctness blocker remains.

REDRESS-209 exists and matches the W5R claim: the current
`G-SK-V14-W5-PRUNE-3` gate is rejected because `regen-css` still emits through
static provider dispatch, CSS source inputs are freshness-only, CSS grammar
syntax is unsupported by the skinny parser, and W6/W7/W8-W10 remain blocked.

## Citation Checks

- W5 SPEC provider-collapse and stale W8-W10 independence surfaces resolve at
  `restart/skinny/tranches/sk-v14/SPEC.md` §8.
- `skinny/xtask/src/regen.rs` still calls
  `codegen::emit_runtime_profile(target.profile)` and hashes inputs without
  passing them to codegen.
- `skinny/crates/codegen/src/lib.rs` still matches static `RuntimeProvider`
  variants.
- `skinny/crates/grammar/src/lib.rs` accepts only `@import` / `@token` and has
  no value-projection or span-capture atom.
- `grammar/css/l4/values.bbnf` uses both `->` and `@{...}`.
- Lock count remains 16; Pattern H count remains 67; five-shape BackendShape
  canon holds.

## Disposition

ACCEPT. No fold required by CH1.
