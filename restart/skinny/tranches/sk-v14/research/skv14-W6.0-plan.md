# SK-V14 Wave W6.0 Plan: CSS L4 Root Runtime Regen Collapse

Inputs: `skv14-W6.0-A1-css-l4-root-runtime-inventory.md`, `skv14-W6.0-A2-lock14-route-gap.md`, `skv14-W6.0-A3-generator-feasibility.md`, `skv14-W6.0-A4-css-l4-consumer-tests.md`, `skv14-W6.0-A5-root-xtask-regeneration.md`, `skv14-W6.0-A6-path-shim-removal.md`; `SPEC.md:932-1018`.

Intervention: Add a narrow W6.0 owner route and a root `cargo xtask regen-css` path that regenerates the CSS L4 root runtime from grammar/registry inputs, then remove the CSS `LegacyPath` / `LegacySegment` alias shim and verify destructive regen plus focused CSS consumers.

Owner paths:

- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `xtask/src/main.rs`
- `xtask/src/lib.rs`
- `xtask/src/regen.rs`
- `xtask/src/regen_css.rs`
- `xtask/tests/` if the command shape needs a focused non-destructive test
- `crates/core/src/runtime/css_l4/`
- `crates/core/tests/parse_with_css_l4.rs` only for the alias-name cleanup needed by W6.0
- `skinny/REDRESS.md` if rejected
- `restart/skinny/tranches/sk-v14/research/skv14-W6.0-redress.md`

Falsifiability gate:

1. Lock 14 route gate:
   - Add `SK_V14_W6_0_OWNER_PATHS` with exact CSS root runtime and root xtask paths.
   - Add `../crates/core/src/runtime/css_l4` and exact root xtask paths to `FROZEN_ROOTS`.
   - Tests prove W6.0 subjects admit CSS L4 root runtime edits, W5D/W6.1/generic W6 subjects reject them, and sibling runtime paths such as `../crates/core/src/runtime/json/...` are rejected.
2. Root regen gate:
   - `cargo xtask regen-css`
   - `git diff --exit-code -- crates/core/src/runtime/css_l4`
   - `rm -rf crates/core/src/runtime/css_l4 && cargo xtask regen-css && git diff --exit-code -- crates/core/src/runtime/css_l4`
3. Parser-regeneration separation gate:
   - `cargo xtask regen --grammar css_l4 --output /tmp/w6-css-parser`
   - `diff -q crates/core/src/grammar/generated/css_l4.rs /tmp/w6-css-parser/css_l4.rs`
   - `diff -q crates/core/src/grammar/generated/css_l4.registry.json /tmp/w6-css-parser/css_l4.registry.json`
4. Focused CSS consumer gate:
   - `cargo test -p bbnf --profile ax-iter --test css_l4_substrate`
   - `cargo test -p bbnf --profile ax-iter --test parse_with_css_l4`
   - `cargo test -p bbnf --profile ax-iter --test runtime_root`
   - `cargo test -p bbnf --profile ax-iter --test typed_accessor_surface`
5. Lock 14 baseline gate:
   - `CARGO_TARGET_DIR=/tmp/bbnf-lang-skinny-gate-target cargo run --manifest-path skinny/Cargo.toml -p xtask -- gate-json --skv14-existing-results-capture --check-results`

Hard cap: 90 minutes for redress. Commit at 81 minutes if a complete ADMIT or honest REJECT packet exists; halt at 90.

Revert protocol: If root runtime generation cannot produce the CSS public API from grammar/registry inputs without copying or hiding handwritten runtime bodies as templates, revert all source/test edits, save the attempted patch to `/tmp/skv14-waveW6.0-rejected.patch`, and commit `docs(sk-v14-waveW6.0-redress): reject css root runtime regen collapse` with a REDRESS entry naming the intrinsic generator gap.

Same-wave consumer: `CssL4Parser::parse`, `runtime::css_l4::parse_with`, direct `CssStructBuilder` substrate tests, and the Lock 14 baseline all consume the W6.0 changes before ADMIT.

Pre-blocked routes:

- Static centralization of the seven CSS runtime files under a fake generated header.
- Shelling root `cargo xtask regen-css` into skinny `cargo xtask regen-css`.
- Replacing CSS L4 rich typed runtime with a generic `SimpleStructBuilder` tree.
- Broad `../crates/core/src/runtime/` owner authorization.
- Preserving `LegacyPath` / `LegacySegment` as production or test vocabulary.
