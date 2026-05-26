# SK-V14 W5B-GEN-B: Xtask Routing

Date: 2026-05-26.
Scope: Read-only xtask request routing and W5B-GEN exit-gate audit.
Output: this file.

## §1 — Findings

The xtask front door is already shaped for W5B-GEN. `skinny/xtask/src/main.rs:19`
routes `regen-json`, `check-json`, `regen-css`, and the seven exact
`check-css-l4-*` companions. JSON builds a `RuntimeGenerationRequest` at
`main.rs:166`; `check-json` builds the same request at `main.rs:172`.

CSS L4 request coverage is complete at the file level. `skinny/xtask/src/regen_css.rs:5`
lists the 15 `grammar/css/l4/*.bbnf` sources, `regen_css.rs:23` names the
metadata inputs, and `regen_css.rs:26` through `regen_css.rs:93` defines seven
runtime targets with the shared grammar name `css_l4` and entry rule
`stylesheet`.

The shared request builder reads source bytes and metadata before codegen.
`skinny/xtask/src/regen.rs:48` constructs `RuntimeGenerationRequest`,
`regen.rs:103` reads workspace metadata from the root and skinny manifests,
and `regen.rs:34` diffs generated output for each check command.

The routing gap is below xtask, not inside xtask. `skinny/crates/codegen/src/grammar_provider.rs:77`
still calls `render_runtime_profile` after validating non-JSON materiality,
so a CSS source mutation can affect the W5A source hash proof without affecting
the emitted CSS runtime bytes.

## §2 — Recommendations

Keep the xtask boundary largely intact. W5B-GEN should replace only the
codegen internals below `emit_runtime_from_request`, then prove that source or
metadata changes either alter the emitted/provenance bytes or fail closed before
emission.

The verification bundle must include:

```sh
cd skinny && cargo xtask regen-css
cd skinny && cargo xtask check-css-l4-at-rules-and-media
cd skinny && cargo xtask check-css-l4-declaration-values
cd skinny && cargo xtask check-css-l4-declaration-values-extended
cd skinny && cargo xtask check-css-l4-nested-layout
cd skinny && cargo xtask check-css-l4-stylesheet-selectors
cd skinny && cargo xtask check-css-l4-vendor-and-custom-atrules
cd skinny && cargo xtask check-css-l4-visual-functions
cd skinny && cargo xtask check-json
cd skinny && cargo xtask gate-json --check-results --skv14-existing-results-capture
```

## §3 — Risks

Passing the seven companion commands is insufficient if they still compare
provider/template-backed output. W5B-GEN must pair the companion checks with
the provider-reachability grep and a request-source dependence proof.

## §4 — Sources

- `restart/skinny/tranches/sk-v14/SPEC.md:723` through `SPEC.md:746`
- `skinny/xtask/src/main.rs:19`
- `skinny/xtask/src/main.rs:166`
- `skinny/xtask/src/main.rs:172`
- `skinny/xtask/src/regen.rs:34`
- `skinny/xtask/src/regen.rs:48`
- `skinny/xtask/src/regen.rs:103`
- `skinny/xtask/src/regen_css.rs:5`
- `skinny/xtask/src/regen_css.rs:23`
- `skinny/xtask/src/regen_css.rs:26`
- `skinny/crates/codegen/src/grammar_provider.rs:77`
