# SK-V14 Wave W5B-FRONTEND Plan: Request-Owned Frontend Compatibility IR

Inputs:
- `restart/skinny/tranches/sk-v14/SPEC.md:708`-`770`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-A1-frontend-construct-gap.md:9`-`79`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-A2-codegen-request-boundary.md:9`-`60`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-A3-xtask-css-runtime-dispatch.md:9`-`76`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-A4-lock14-owner-routing.md:9`-`78`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-A5-proof-carry.md:9`-`92`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-A6-provider-template-topology.md:9`-`70`.
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V1/HARDENING-SKV14-W5B-FRONTEND-V1-CONSOLIDATED.md:1`-`73`.

Intervention: add request-owned frontend/import/IR lowering for CSS L4
compatibility constructs, consumed by `emit_runtime_from_request` before
provider rendering, while preserving W5A provider-backed runtime bytes.

V1 challenge returned REVISE. This V2 fold changes execution shape, not the
wave goal: W5B-FRONTEND is executed as serial W5B-internal sub-slices under the
same owner set. W5B does not close, and W5C-GEN remains blocked, until every
sub-slice and the final gate pass. These are not future-tranche deferrals.

## Owner Paths

Redress is authorized to touch only:

- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/crates/grammar/src/lib.rs`
- `skinny/crates/codegen/src/grammar_provider.rs`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/xtask/src/main.rs`
- `skinny/xtask/src/regen.rs`
- `skinny/xtask/src/regen_css.rs`
- `skinny/RESULTS.md` as SPEC row-attribution surface only; W5B admits no row
  movement and the file must remain byte-identical to `HEAD`.
- `skinny/REDRESS.md` only if the redress attempt rejects.

Redress report output path:

- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-redress.md`

No new neutral module path is selected for W5B-FRONTEND. Any need for a new
module returns REVISE because V7 requires the plan to name it before the Lock 14
owner-path gate can admit it.

## W5B Sub-Slice Order

Each sub-slice is capped at 30 minutes with a 27-minute checkpoint. A sub-slice
that cannot satisfy its exact tests under cap reverts its source slice and
records REDRESS rather than borrowing from W5C-GEN, W5D-DELETE, or W6.

1. **W5B.0 LOCK14-IMPORT-WS**:
   - Add `SK_V14_W5B_FRONTEND_OWNER_PATHS` in `lock14_baseline.rs` with exactly
     `crates/grammar/src/lib.rs`, `crates/codegen/src/lib.rs`,
     `crates/codegen/src/grammar_provider.rs`, `xtask/src/main.rs`,
     `xtask/src/regen.rs`, `xtask/src/regen_css.rs`,
     `crates/bbnf-bench/src/lock14_baseline.rs`, and `RESULTS.md`.
   - Add the roster to `current_lock14_owner_paths()`.
   - Add parent-diff routing for `sk-v14-waveW5B-FRONTEND`,
     `sk-v14-waveW5B-FRONTEND-redress`, and lowercase `sk-v14-w5b-frontend`.
   - Add tests that admit only the W5B roster and reject W5C/W5D subjects.
   - Add request-local import DAG resolution from the request source map,
     fail-closed missing-import and import-cycle errors, and public `@ws`
     rejection outside compatibility lowering.
2. **W5B.1 LAYOUT-DISCARD**:
   - Lower `@ws`, `?w`, `>>`, and `<<` into request-local frontend facts.
   - `@ws` remains retired public syntax: `parse_grammar` rejects standalone
     public `@ws`; only request-owned compatibility closure may record it.
3. **W5B.2 PRETTY-SPAN-PROJECTION**:
   - Lower `@pretty`, `@{...}` span capture, `->` projection metadata, and typed
     projections into request-local frontend facts.
   - No `ir::ExprKind` variant is added. The lowering target is a request-local
     `FrontendClosure`/facts structure in `grammar/src/lib.rs`, consumed by the
     codegen request path and not emitted as public syntax or runtime state.
4. **W5B.3 REQUEST-CONSUMER-GATE**:
   - `emit_runtime_from_request` consumes the frontend closure before
     `validate_non_json_materiality`.
   - Non-JSON emission may still call `render_runtime_profile(profile, None)`;
     W5C-GEN owns provider-free generation.
   - JSON remains on the W5A request equality path.
   - `regen-css` and all seven CSS companions pass through the request-owned
     frontend closure while provider/template topology remains unchanged.

## Lowering Contract

The request-owned closure is not a new public directive set, a public substrate,
a parser-owned sidecar, or runtime-queryable state. It is local to the runtime
generation request and may be dropped after codegen validation/emission.

| Construct | Target representation | Required positive gate | Required fail-closed gate |
|---|---|---|---|
| `@import` | `FrontendClosure.imports` DAG keyed by request source path and stable source hash | `w5b_frontend_import_graph_resolves_request_sources` | `w5b_frontend_missing_import_fails_closed`, `w5b_frontend_import_cycle_fails_closed` |
| `@ws` | `FrontendClosure.layout.whitespace_directive` fact | `w5b_frontend_layout_contract_lowers_to_request_facts` | `w5b_frontend_public_ws_remains_retired` |
| `?w` | `FrontendClosure.layout.whitespace_modifier` fact attached to source span | `w5b_frontend_layout_contract_lowers_to_request_facts` | malformed placement rejects with source offset |
| `>>` / `<<` | `FrontendClosure.discard_operator` facts attached to source spans | `w5b_frontend_discard_operators_lower_to_request_facts` | malformed operator placement rejects with source offset |
| `@pretty` | `FrontendClosure.pretty_directive` fact | `w5b_frontend_pretty_span_projection_lower_to_request_facts` | unknown public directive still rejects |
| `@{...}` | `FrontendClosure.host_capture` span fact | `w5b_frontend_pretty_span_projection_lower_to_request_facts` | unterminated capture rejects with source offset |
| `->` projection | `FrontendClosure.projection` fact preserving raw target text | `w5b_frontend_pretty_span_projection_lower_to_request_facts` | malformed projection rejects with source offset |
| typed projection | `FrontendClosure.typed_projection` fact preserving raw type text | `w5b_frontend_pretty_span_projection_lower_to_request_facts` | malformed typed projection rejects with source offset |

## LOC Budget

W5B-FRONTEND source/test LOC cap: <=1.0k C-1 part-A. Generated output is
uncounted only if produced by `cargo xtask regen-css`, named in the redress log,
byte-diff audited, and included in the revert slice.

| File set | Cap |
|---|---:|
| `lock14_baseline.rs` roster, routing, and provider/template-modification tests | <=170 LOC |
| `grammar/src/lib.rs` frontend closure, import DAG, construct lowering, and tests | <=430 LOC |
| `grammar_provider.rs` / `codegen/src/lib.rs` request-consumer proof and tests | <=190 LOC |
| `xtask/src/main.rs`, `regen.rs`, `regen_css.rs` proof plumbing only | <=120 LOC |
| Redress report and reject-only REDRESS entry | <=90 LOC |

## Falsifiability Gates

Every command is cwd-explicit. `gate-json --skv14-existing-results-capture` is
only shape/freshness evidence; full-table maintain for this capability wave is
the exact no-diff proof on `RESULTS.md`, `ROLLING-SOTA-DELTA.md`, generated
runtime outputs, and protected source/runtime inputs. Because W5B is not an
admit or benchmark-refresh wave, byte-identical row and generated-runtime
surfaces are stricter than the +/-1.0% table-maintain allowance.

Required tests. In redress, run each command with `2>&1 | tee
/tmp/skv14-w5b-<test-name>.log` before applying the nonzero-pass assertion.

```sh
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo test -p bbnf-bench lock14_baseline -- --nocapture )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo test -p grammar w5b_frontend_import_graph_resolves_request_sources -- --exact --nocapture )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo test -p grammar w5b_frontend_missing_import_fails_closed -- --exact --nocapture )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo test -p grammar w5b_frontend_import_cycle_fails_closed -- --exact --nocapture )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo test -p grammar w5b_frontend_public_ws_remains_retired -- --exact --nocapture )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo test -p grammar w5b_frontend_layout_contract_lowers_to_request_facts -- --exact --nocapture )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo test -p grammar w5b_frontend_discard_operators_lower_to_request_facts -- --exact --nocapture )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo test -p grammar w5b_frontend_pretty_span_projection_lower_to_request_facts -- --exact --nocapture )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo test -p codegen w5b_frontend_request_consumes_lowered_ir_before_provider_rendering -- --exact --nocapture )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo test -p codegen w5a_json_request_matches_emit_from_source -- --exact --nocapture )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo test -p codegen w5a_sheets_bbnf_fail_closed_through_runtime_contract -- --exact --nocapture )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo test -p grammar w5a_named_unsupported_constructs_are_source_located -- --exact --nocapture )
```

Every exact test command must be paired with a nonzero-pass assertion in the
redress log:

```sh
rg "test result: ok\\. [1-9][0-9]* passed" /tmp/skv14-w5b-*.log
```

Xtask and companion gates:

```sh
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo xtask check-json )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo xtask gate-json --check-results --skv14-existing-results-capture )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo xtask regen-css )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo xtask check-css-l4-at-rules-and-media )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo xtask check-css-l4-declaration-values )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo xtask check-css-l4-declaration-values-extended )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo xtask check-css-l4-nested-layout )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo xtask check-css-l4-stylesheet-selectors )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo xtask check-css-l4-vendor-and-custom-atrules )
( cd /Users/mkbabb/Programming/bbnf-lang/skinny && cargo xtask check-css-l4-visual-functions )
```

Topology, maintain, and hidden-coupling gates:

```sh
test "$(find /Users/mkbabb/Programming/bbnf-lang/skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l | tr -d ' ')" = 8
test "$(find /Users/mkbabb/Programming/bbnf-lang/skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l | tr -d ' ')" = 7
git -C /Users/mkbabb/Programming/bbnf-lang diff --exit-code HEAD -- skinny/RESULTS.md restart/skinny/ROLLING-SOTA-DELTA.md
git -C /Users/mkbabb/Programming/bbnf-lang diff --exit-code HEAD -- skinny/crates/runtime/src/grammars crates/core/src/runtime/css_l4 grammar/css/l4
git -C /Users/mkbabb/Programming/bbnf-lang diff --name-status HEAD -- skinny/crates/codegen/src | rg '(_provider\\.rs|css_l4_.*_templates)' | rg -v 'grammar_provider\\.rs' && exit 1 || true
git -C /Users/mkbabb/Programming/bbnf-lang diff --cached --name-status -- skinny/crates/codegen/src | rg '(_provider\\.rs|css_l4_.*_templates)' | rg -v 'grammar_provider\\.rs' && exit 1 || true
git -C /Users/mkbabb/Programming/bbnf-lang status --short -- skinny/crates/codegen/src | rg '(_provider\\.rs|css_l4_.*_templates)' | rg -v 'grammar_provider\\.rs' && exit 1 || true
rg -n "RuntimeProvider" /Users/mkbabb/Programming/bbnf-lang/skinny/crates/codegen/src/grammar_profile.rs /Users/mkbabb/Programming/bbnf-lang/skinny/crates/codegen/src/grammar_provider.rs /Users/mkbabb/Programming/bbnf-lang/skinny/crates/codegen/src/lib.rs
rg -n "GrammarProfile" /Users/mkbabb/Programming/bbnf-lang/skinny/crates/codegen/src
rg -n "render_runtime_profile\\(profile, None\\)" /Users/mkbabb/Programming/bbnf-lang/skinny/crates/codegen/src/grammar_provider.rs
```

The provider/template path guards intentionally reject `M`, `A`, `D`, `R`, and
`??` statuses for existing providers/templates. `grammar_provider.rs` is the
only provider-adjacent file W5B may edit.

LOC accounting:

```sh
git -C /Users/mkbabb/Programming/bbnf-lang diff --numstat HEAD -- \
  skinny/crates/bbnf-bench/src/lock14_baseline.rs \
  skinny/crates/grammar/src/lib.rs \
  skinny/crates/codegen/src/grammar_provider.rs \
  skinny/crates/codegen/src/lib.rs \
  skinny/xtask/src/main.rs \
  skinny/xtask/src/regen.rs \
  skinny/xtask/src/regen_css.rs
```

The redress log must sum added+deleted source/test lines by file set and fail
the slice if any per-file-set cap or the <=1.0k aggregate cap is exceeded.

## Revert Protocol

On gate failure:

1. Save the rejected patch at `/tmp/skv14-waveW5B-FRONTEND-rejected.patch`.
2. Revert `lock14_baseline.rs`, `grammar/src/lib.rs`,
   `codegen/src/grammar_provider.rs`, `codegen/src/lib.rs`, and any
   `xtask/src/*` W5B edits as one slice.
3. Preserve W5A's request-boundary work and the existing provider/template mesh.
4. Preserve `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md`
   byte-identical to pre-redress `HEAD` unless recording a rejection entry in
   `skinny/REDRESS.md`.
5. Append a REDRESS entry naming the failing construct, import graph,
   canonical-lowering proof, Lock 14 owner routing, JSON/non-JSON proof carry,
   full-table maintain proof, provider-modification guard, or provider
   reachability gate.

## Same-Wave Consumer

W5B closes only when the same redress commit set includes:

- Lock 14 W5B owner-routing unit tests, including modified-provider/template
  rejection under W5B subjects.
- Import DAG positive and negative tests for missing import and import cycles.
- Frontend compatibility-lowering tests in the grammar crate for every construct
  in the lowering table.
- A codegen request-boundary test proving `emit_runtime_from_request` consumes
  the lowered frontend closure before provider rendering.
- `cargo xtask regen-css` and all seven `check-css-l4-*` companions through the
  request-owned frontend closure.
- JSON unchanged-output proof and Sheets/BBNF-self fail-closed proof carry.
- Provider-dispatch reachability proof for `RuntimeProvider`, `GrammarProfile`,
  and `render_runtime_profile(profile, None)`.

## Pre-Blocked Routes

- No provider/template deletion, rename, add, or modification, except edits to
  `grammar_provider.rs`.
- No provider-free generator body replacement; W5C-GEN owns that.
- No `RuntimeProvider`/`GrammarProfile`/`render_runtime_profile` retirement.
- No public `@ws` directive; it is compatibility lowering into request-local
  facts.
- No grammar-name branch in generic crates.
- No static centralization of hand-written CSS runtime bodies.
- No committed-generated-output mining.
- No `skinny/RESULTS.md` or `restart/skinny/ROLLING-SOTA-DELTA.md` row movement.
- No parser-owned sidecar, emitted frontend-fact table, retained runtime query
  surface, or public substrate API.
- No new neutral module path unless a revised plan names it and Lock 14 admits
  it before source redress.

## Mandatory Challenge

Dispatch seven-lens W5B-FRONTEND CHALLENGE V2 before redress:

- CH1: verify owner-path reconciliation, cwd-explicit gates, exact test names,
  nonzero-pass assertions, and lowering-table falsifiability.
- CH2: verify Lock 14 generality and non-JSON proof carry.
- CH3: verify REDRESS-209/210/211 remain closed and the maintain gate is
  executable for a non-admit capability wave.
- CH4: verify sub-slice caps, per-file LOC budget, and no W5C/W5D/W6 borrowing.
- CH5: verify modified provider/template paths are blocked, provider dispatch
  stays reachable, and frontend facts are request-local only.
- CH6: verify same-wave consumers, revert protocol, negative import tests, and
  no paper frontend-IR.
- CH7: verify no P-1..P-7 recurrence, fake generated header, fixture lookup,
  public `@ws`, committed-output mining, or gate relabeling as admit.
