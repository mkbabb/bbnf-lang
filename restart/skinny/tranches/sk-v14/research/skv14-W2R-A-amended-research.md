# SK-V14 W2R-A: Amended Regen-CSS Research

Date: 2026-05-26.
Status: read-only research after Pass Omega V3 W2R CRUD close.

## Findings

- Pass Omega V3 closed the original W2 dependency cycle. Active W2 is now
  skinny-side only: `cargo xtask regen-css` may emit only
  `skinny/crates/runtime/src/grammars/css_l4_*`; root
  `crates/core/src/runtime/css_l4/` is W6.0 work.
- Skinny `xtask` currently exposes JSON regen/check commands but no
  `regen-css` or exact `check-css-l4-*` companions. `skinny/xtask/src/main.rs`
  still declares only `mod real_typed_schema;`.
- The skinny CSS L4 runtime exports seven runtime profile directories:
  declaration values, declaration values extended, stylesheet selectors,
  visual functions, at-rules/media, vendor/custom at-rules, and nested layout.
  Those seven profiles are the W2 skinny output roster.
- The 15 W2 CSS L4 grammar inputs are the files under `grammar/css/l4/*.bbnf`.
  They must be part of the command input contract even though the current
  provider mesh still renders the seven fact-stream profiles.
- Current `xtask` depends on `runtime` and `test-fixtures` for
  `check-conformance`. If the destructive W2 gate deletes
  `skinny/crates/runtime/src/grammars/css_l4_*`, normal `cargo xtask
  regen-css` cannot compile until `xtask` is decoupled from the runtime
  dependency in the default feature set.
- `skinny/crates/codegen/src/css_l4_declaration_values_provider.rs` omits
  `sink.rs` from its generated file roster even though the checked-in runtime
  module requires `pub mod sink`. After a destructive delete, that provider
  cannot restore the declaration-values runtime directory.

## Amended Gates

- `G-W2-SKINNY-ONLY`: W2 may not touch, delete, emit, or claim closure over
  `crates/core/src/runtime/css_l4/`.
- `G-W2-BOOTSTRAP`: `cargo xtask regen-css` must compile after
  `skinny/crates/runtime/src/grammars/css_l4_*` is removed.
- `G-W2-SEVEN-COMPANIONS`: the seven exact companion commands must exist and
  pass from the `skinny/` cwd.
- `G-W2-ROUNDTRIP`: from repo root,
  `rm -rf skinny/crates/runtime/src/grammars/css_l4_* && (cd skinny && cargo xtask regen-css) && git diff --exit-code -- skinny/crates/runtime/src/grammars`
  must return clean.
- `G-W2-NON-ADMIT`: W2 records generator correctness only. It does not move
  CSS SOTA rows or claim throughput admission.

## Sources

- `restart/audit/totality/astral/V3/CRUD-LOG.md`
- `restart/audit/totality/astral/V3/G-OMEGA-SIGNOFF.md`
- `restart/skinny/tranches/sk-v14/SPEC.md` Section 5
- `restart/skinny/tranches/sk-v14/research/skv14-W2R-corrective-packet.md`
- Sidecar notes from Franklin, Tesla, and Pasteur.
