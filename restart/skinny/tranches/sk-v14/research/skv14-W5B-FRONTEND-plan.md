# SK-V14 Wave W5B-FRONTEND Plan: Request-Owned Frontend Compatibility IR

Inputs:
- `restart/skinny/tranches/sk-v14/SPEC.md:708`-`770`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-A1-frontend-construct-gap.md:9`-`79`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-A2-codegen-request-boundary.md:9`-`60`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-A3-xtask-css-runtime-dispatch.md:9`-`76`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-A4-lock14-owner-routing.md:9`-`78`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-A5-proof-carry.md:9`-`92`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-A6-provider-template-topology.md:9`-`70`.

Intervention: add request-owned frontend/import/IR lowering for the CSS L4
compatibility dialect, consumed by `emit_runtime_from_request` before provider
rendering, while preserving W5A provider-backed runtime bytes.

## Owner Paths

Redress is authorized to touch only:

- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/crates/grammar/src/lib.rs`
- `skinny/crates/codegen/src/grammar_provider.rs`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/xtask/src/main.rs`
- `skinny/xtask/src/regen.rs`
- `skinny/xtask/src/regen_css.rs`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-redress.md`
- `skinny/REDRESS.md` only if the redress attempt rejects.

No new neutral module path is selected for W5B-FRONTEND. Any later need for a
new module returns REVISE because V7 requires the plan to name it before the
Lock 14 owner-path gate can admit it.

## Redress Ordering

1. Patch `lock14_baseline.rs` first:
   - Add `SK_V14_W5B_FRONTEND_OWNER_PATHS` with exactly:
     `crates/grammar/src/lib.rs`, `crates/codegen/src/lib.rs`,
     `crates/codegen/src/grammar_provider.rs`, `xtask/src/main.rs`,
     `xtask/src/regen.rs`, `xtask/src/regen_css.rs`, and
     `crates/bbnf-bench/src/lock14_baseline.rs`.
   - Add the roster to `current_lock14_owner_paths()`.
   - Add parent-diff routing for `sk-v14-waveW5B-FRONTEND`,
     `sk-v14-waveW5B-FRONTEND-redress`, and lowercase `sk-v14-w5b-frontend`.
   - Add unit tests that admit only the W5B roster under those subjects and
     reject provider/template paths or W5C/W5D subjects.
2. Only after the Lock 14 patch compiles locally, add frontend lowering in
   `skinny/crates/grammar/src/lib.rs`:
   - Introduce a request-scoped frontend IR/facts artifact that resolves
     `@import` from the request source map, fails closed on missing imports and
     cycles, and records canonical lowering of `@ws`, `@pretty`, `?w`, `>>`,
     `<<`, span capture `@{...}`, and typed projections.
   - Keep `@ws` retired as public syntax: `parse_grammar` must still reject
     standalone public `@ws` outside the request-owned compatibility lowering.
3. Consume the frontend artifact in `skinny/crates/codegen/src/grammar_provider.rs`:
   - `emit_runtime_from_request` must lower frontend/import/IR before
     `validate_non_json_materiality`.
   - Non-JSON emission may still call `render_runtime_profile(profile, None)`;
     W5C-GEN owns provider-free generation.
   - JSON must stay on the W5A request equality path.
4. Touch `skinny/crates/codegen/src/lib.rs` and `skinny/xtask/src/*` only for
   tests or proof plumbing needed to consume the frontend artifact through
   `regen-css` and the seven CSS companion checks.

## Falsifiability Gate

W5B-FRONTEND admits only if all gates pass at HEAD:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo test -p bbnf-bench lock14_baseline -- --nocapture
cargo test -p grammar w5b_frontend_css_l4_compatibility_lowers_to_ir -- --exact --nocapture
cargo test -p grammar w5b_frontend_import_graph_resolves_request_sources -- --exact --nocapture
cargo test -p grammar w5b_frontend_public_ws_remains_retired -- --exact --nocapture
cargo test -p codegen w5b_frontend_request_consumes_lowered_ir -- --exact --nocapture
cargo test -p codegen w5a_json_request_matches_emit_from_source -- --exact --nocapture
cargo test -p codegen w5a_sheets_bbnf_fail_closed_through_runtime_contract -- --exact --nocapture
cargo test -p grammar w5a_named_unsupported_constructs_are_source_located -- --exact --nocapture
cargo xtask check-json
cargo xtask gate-json --check-results --skv14-existing-results-capture
cargo xtask regen-css
cargo xtask check-css-l4-at-rules-and-media
cargo xtask check-css-l4-declaration-values
cargo xtask check-css-l4-declaration-values-extended
cargo xtask check-css-l4-nested-layout
cargo xtask check-css-l4-stylesheet-selectors
cargo xtask check-css-l4-vendor-and-custom-atrules
cargo xtask check-css-l4-visual-functions
```

Topology and no-row-movement gates:

```sh
test "$(find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l | tr -d ' ')" = 8
test "$(find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l | tr -d ' ')" = 7
git diff --name-status -- skinny/crates/codegen/src
git diff --cached --name-status -- skinny/crates/codegen/src
git diff --exit-code HEAD -- skinny/RESULTS.md restart/skinny/ROLLING-SOTA-DELTA.md
git diff --exit-code -- crates/core/src/runtime/css_l4 grammar/css/l4
```

The `git diff --name-status` outputs must contain no provider/template add,
delete, rename, or untracked protected path. W5B-FRONTEND is a capability wave,
not an admit-row wave; `skinny/RESULTS.md` and
`restart/skinny/ROLLING-SOTA-DELTA.md` remain byte-identical.

## Hard Cap

Redress cap: 30 minutes implementation, with the 0.9N checkpoint at 27 minutes.
If the Lock 14 patch plus frontend lowering cannot satisfy the exact gates
inside the cap, revert the source slice and record REDRESS rather than borrowing
from W5C-GEN, W5D-DELETE, or W6.

W5B-FRONTEND source/test LOC cap: <=1.0k C-1 part-A. Generated output is
uncounted, but W5B must not edit committed generated CSS/root runtime output.

## Revert Protocol

On gate failure:

1. Save the rejected patch at `/tmp/skv14-waveW5B-FRONTEND-rejected.patch`.
2. Revert `lock14_baseline.rs`, `grammar/src/lib.rs`,
   `codegen/src/grammar_provider.rs`, `codegen/src/lib.rs`, and any
   `xtask/src/*` W5B edits as one slice.
3. Preserve W5A's request-boundary work and the existing provider/template mesh.
4. Append a REDRESS entry naming the failing construct, import graph,
   canonical-lowering proof, Lock 14 owner routing, JSON/non-JSON proof carry,
   or provider topology gate.

## Same-Wave Consumer

The same redress commit must include:

- Lock 14 W5B owner-routing unit tests.
- Frontend compatibility-lowering tests in the grammar crate.
- A codegen request-boundary test proving `emit_runtime_from_request` consumes
  the lowered frontend artifact before provider rendering.
- `cargo xtask regen-css` and all seven `check-css-l4-*` companions through the
  request-owned frontend closure.
- JSON unchanged-output proof and Sheets/BBNF-self fail-closed proof carry.

## Pre-Blocked Routes

- No provider/template deletion, rename, or add.
- No provider-free generator body replacement; W5C-GEN owns that.
- No `RuntimeProvider`/`GrammarProfile`/`render_runtime_profile` retirement.
- No public `@ws` directive; it is compatibility lowering into canonical IR.
- No grammar-name branch in generic crates.
- No static centralization of hand-written CSS runtime bodies.
- No committed-generated-output mining.
- No `skinny/RESULTS.md` or `restart/skinny/ROLLING-SOTA-DELTA.md` row movement.
- No new neutral module path unless a revised plan names it and Lock 14 admits
  it before source redress.

## Mandatory Challenge

Dispatch seven-lens W5B-FRONTEND CHALLENGE before redress:

- CH1: verify every path/line claim and the exact gate commands.
- CH2: verify Lock 14 generality and non-JSON proof carry.
- CH3: verify REDRESS-209/210/211 are not reopened and that W5B does not borrow
  W5C/W5D deletion/replacement work.
- CH4: verify <=1.0k LOC and 30-minute cap are realistic.
- CH5: verify no hidden provider/template deletion, sidecar substrate, or
  `render_runtime_profile` replacement.
- CH6: verify same-wave consumers and revert protocol.
- CH7: verify no P-1..P-7 recurrence, fake generated header, fixture lookup, or
  gate relabeling as admit.
