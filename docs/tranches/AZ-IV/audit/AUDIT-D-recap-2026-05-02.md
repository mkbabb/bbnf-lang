# AZ-IV — AUDIT-D Comprehensive Recap (2026-05-02)

**Lane**: AUDIT-D Comprehensive Recap (read-only research).
**Date**: 2026-05-02.
**Base**: `10ac5448` (post-mid-tranche audit landing on master).
**Span audited**: tranche open `2678ed44` through HEAD `10ac5448`; W0+W1+W2 closed; W3+ planned.
**Sources**: `docs/precepts/instructions/{README,STYLE,ORCHESTRATION,LESSONS-LEARNED}.md`,
`docs/precepts/instructions/tranche/{SPEC,WAVE_SPEC}.md`, `docs/GESTALT.md`,
`docs/tranches/AZ-IV/{AZ-IV.md,PROGRESS.md,waves/W0..W6.md}`,
`docs/tranches/AZ-IV/audit/**`, `docs/precepts/audits/overfitting-audit.md`,
`docs/precepts/audits/REAUDIT-2026-04-30/SYNTHESIS.md`,
`git log --oneline 2678ed44..HEAD` (92 commits).

---

## §1 Original Mandate

### Thesis (paraphrase + quote)

AZ-IV is the union tranche that consumes or deletes every existing substrate the
prior tranches accumulated, eliminates every overfit the third hardening pass
surfaced, lands the typed compile-time `path!` macro and the path-driven lazy
recognizer, executes the TS template-literal-tag binding to parity, and
redresses every failing test — closing inside this tranche or not closing at
all. Direct quote from `AZ-IV.md` §Thesis:

> The system already chose the right architecture: grammar-derived Rust struct
> graphs are the materialized parse form, `cargo xtask regen` is the canonical
> Rust generation path, and CSP/egraph/shape/Pratt/regex/SIMD/view substrate
> exist to make that path general and fast. AZ-IV closes the remaining gap by
> consuming or deleting every existing substrate, eliminating every overfit,
> landing the typed compile-time `path!` macro and the path-driven lazy
> recognizer, executing the TS binding to parity, and redressing every failing
> test. No parallel parser, no shadow path system, no unconsumed substrate, no
> chronic deferral — every named carry closes inside this tranche or AZ-IV
> does not close.

### Four grammar-derived invariants (GESTALT §2)

Quoted verbatim from `docs/GESTALT.md` §2 "Four interlocking invariants":

1. **Typed materialisation.** "Every `->` in every grammar reaches
   `push_leaf_with_*`, `begin_compound`, or `end_compound`, and an IR audit
   pass enforces 100 % coverage and fails the build otherwise. The emitter
   never re-derives what the grammar already declares. The `project_types` IR
   pass writes into a `StructRegistry`; the emitter reads the registry; there
   is no third party that opines on shape."
2. **No orthogonal codepaths.** "Arena allocation is a singular collection
   strategy; no conditional Vec-vs-scratch branching; no combinator fallback
   alongside the monolithic codegen; one regex system (HIR); KISS DRY."
3. **Direct-to-struct.** "Generalize regex-to-value conversion; no hard-coded
   pattern lists; every `->` in the grammar projects directly to a typed
   record at emission time, without an intermediate untyped phase.
   `project_types` is the single projection pass; there is no parallel
   shape-derivation pipeline."
4. **Grammar-authoritative.** "The grammar owns leaf semantics through `->`;
   host functions cover context-dependent and recursive computations that the
   grammar cannot express. Hybrid-grammar-host is the current migration
   posture (Phase 1+2 done, Phase 3 host-fns pending). Backends see
   `TypeDesc::Named` as abstract names; each backend resolves to native types
   via its own registry. The CSP and e-graph do not know which language they
   target; the emitter does."

The invariants are interlocking: typed materialisation requires direct-to-struct
because anything else re-derives shape after inference has already composed it;
direct-to-struct requires no-orthogonal-codepaths because a second projection
surface would inevitably drift from the first; no-orthogonal-codepaths requires
grammar-authoritative because only a single source of truth can be canonical;
grammar-authoritative requires typed materialisation because without `->`
reaching the emitter the grammar's authority ends at the parse boundary.

### 23 hard gates (AZ-IV.md §Hard Gates)

| # | One-line summary |
|---:|---|
| 1 | `cargo xtask regen --check` green live for all manifest grammars; archived at `audit/W0-regen.txt`. |
| 2 | Parser strategy binding is manifest/registry driven; synthetic-grammar test fails on a new literal parser-name arm. |
| 3 | Regenerated tempdir outputs run the parity matrix; checked-in freshness cannot be proven by separate stale artefacts. |
| 4 | JSON, CSS, Sheets, BBNF, TS parity gates current, green, regenerated-output based, type-inference driven. |
| 5 | Egraph extraction preserves semantic wrappers such as `Map { fn_id }`; named test red-then-green. |
| 6 | Workspace nextest 100% pass; every `#[ignore]` carries owner + deadline + reason + ticket. |
| 7 | `path!(Json, "statuses", 0, "text")` expands to a typed accessor at `cargo build` time; invalid path = compile error with grammar-aware diagnostic. |
| 8 | `path_check` IR pass + inline-trace sidecar resolves source rule names. |
| 9 | Lazy bail-out `parse_with(input, &path)` works on JSON, CSS L4, Sheets, BBNF; same generated code; entry-point dispatch only. |
| 10 | Variant-selection step: `path!(CssL4, ..., "color")` returns `Option<&CssColor>` from sum type. |
| 11 | Wildcard returns lazy `Iter<Item = T>` zero-allocation default; `.with_anchors()`/`.collect()` adapters. |
| 12 | CSP decisions reach emitted consumers or the dead decision surface deletes; sidecars carry payload, not strategy. |
| 13 | Permanent `crates/ir/src/passes/tests/substrate_audit.rs` enumerates every `pub` substrate; CI fails on zero-caller. |
| 14 | Legacy audit closes: `emit_dfa_inline_body`, DTA walker/tape wording, color compatibility, fallback-to-JSON, `RuleSet`+`egraph::ruler::*` (deleted; BA recreates), discarded per-rule compile, derive/bootstrap residue, host shims, stale locks, sidecar authority. |
| 15 | Fat-LTO `post-AZ-IV.json` carries row-by-row post-AU floor in `floors` block; no watchdog-routed row. |
| 16 | Same-harness JSON direct-struct projection rows close parity-or-better against sonic-rs; lazy lane ≤ 5x sonic on `bbnf_get_twitter`. |
| 17 | Grammar-overfit static scan green: `crates/core/tests/no_grammar_name_branch.rs` zero literal-rule-name match arms in production. |
| 18 | Manifest-driven strategy binding: `EmitStrategy::for_grammar` reads parsed manifest metadata, not Rust arm-list. |
| 19 | Substrate path hard-fail: `shapes/substrate.rs` no longer falls back to `JsonStructBuilder`; panics with binding string. |
| 20 | TS binding executable: `crates/bbnf-path-ts/` cdylib + wasm-bindgen template-literal tag; isomorphic error taxonomy with Rust frontend. |
| 21 | Per-grammar value-enum dedup (skeleton): one `Arena<G>` + `Builder<G>` template; typed `*Value` enums survive untouched. |
| 22 | AscentStrategy hybrid sidecar: trait + reversal seam landed in W2; default chosen by W2 micro-bench. |
| 23 | Non-routable carry blockers: every row in §Non-Routable Carries closes inside AZ-IV with cited evidence; no successor letter route. |

### 33 non-routable carries (AZ-IV.md §Non-Routable Carries)

| # | Item | Owner | Closure proof |
|---:|---|---|---|
| 1 | Strict regen drift (7/9 grammars red) | W0 | `cargo xtask regen --check` green 9/9 |
| 2 | Egraph `Map { fn_id }` preservation | W0 | named test fails-before/passes-after |
| 3 | Sheets parity 133/133 | W1 | regenerated-tempdir parity green |
| 4 | TS backend executable (Node-execute) | W5 | tempdir TS typechecks + Node executes representative grammars |
| 5 | Tailwind regex_scan perf timeout | W4 | profile.json.gz + named hot regex op + non-watchdog measured row |
| 6 | Cross-profile watchdog rows | W6 | fat-LTO + bench-iter matrices have zero watchdog rows |
| 7 | JSON value/path vs sonic-rs perf | W6 | `bbnf_value_*` parity-or-better; `bbnf_get_*` ≤ 5x sonic same-harness |
| 8 | CSS named_color runtime activation | W1+W4 | named_color payload parity vs lightningcss |
| 9 | PatternAnnotations migration | W4 | every consumer migrated or PatternAnnotations deleted |
| 10 | Bootstrap/derive residue (sibling) | W0 | `cargo metadata --locked` + `cargo deny` rule rejects `bbnf_derive` |
| 11 | DTA/dfa naming + cleanup | W4 | every DTA reference enumerated; non-consumed deleted, consumed renamed |
| 12 | `backend/rust/view/color` shim | W1 | shim deleted; CSS uses `runtime::css_l4::CssColor`; legacy decoder test-support only |
| 13 | Substrate denominator (permanent test) | W5 | `crates/ir/src/passes/tests/substrate_audit.rs` CI-gated; zero unconsumed substrate |
| 14 | Unconsumed `RuleSet` deletion | W4 | `pipeline.rs` `CompileOptions::rewrites` field deleted; `egraph::ruler::*` deleted |
| 15 | WASM/sibling derive residue | W0 | locks clean root + wasm/ + parse-that |
| 16 | csp-solver canonical-source split | W0 | diff-clean between bbnf-lang and csc411 sibling |
| 17 | bbnf-bootstrap cache nuke | W0 | cycle-2 wall ≤ 10 % of cycle-1 wall |
| 18 | Dev-iteration baseline gate | W0 | `W0-dev-baseline.txt` row-by-row deltas vs AZ-III |
| 19 | Generated-size budget | W0 | per-grammar LOC ±5 % of pre-W0 baseline |
| 20 | 7 `from_rule_name(&str)` impls eliminated | W1 | static AST scan zero match arms keyed on literal rule names |
| 21 | `(layout.kind, rule_name)` builder dispatches eliminated | W1 | `OpenFrame::from_layout(layout, &registry)` projects discriminator |
| 22 | `EmitStrategy::for_grammar` 9-arm allowlist eliminated | W1 | manifest-driven binding registry; synthetic-grammar test passes |
| 23 | `substrate_path` JSON-builder fallback retired | W1 | `panic!` on invalid binding |
| 24 | `recover_modifier`/`recover_binary_op` deleted | W1 | alt_dispatch typed-leaf push activated; `rg` zero hits |
| 25 | Per-grammar arena/builder dedup (skeleton) | W5 | one `Arena<G>` + `Builder<G>` template; typed `*Value` preserved |
| 26 | All failing tests redressed (1527/1527 pass) | W1 | nextest workspace pass-count = total-count; ignores justified |
| 27 | Path IR + compile-time `path!` macro | W2 | `path!(Json, ...)` compile-time typed; invalid path = compile error |
| 28 | `path_check` IR pass after `project_types` | W2 | inline-trace sidecar; source rule names always resolve |
| 29 | AscentStrategy hybrid sidecar | W2 | trait + reversal seam; default impl picked by W2 micro-bench |
| 30 | Lazy bail-out parse on 4 production grammars | W3 | path-driven recognizer skips unvisited subtrees on JSON/CSS/Sheets/BBNF |
| 31 | TS template-literal tag binding | W5 | `crates/bbnf-path-ts/` cdylib + wasm-bindgen |
| 32 | Variant-selection path step (typed-enum) | W2 | `path!(CssL4, ..., "color")` returns `Option<&CssColor>` from sum type |
| 33 | Wildcard yields `Iter<Item = T>` (default) | W2 | zero-allocation default lane; `.with_anchors()` and `.collect()` adapters |

### User-facing precepts

Direct quotes from `docs/precepts/instructions/README.md` §Edicts:

- **KISS, DRY.** "Use the simplest complete mechanism. Remove duplication
  before adding policy."
- **No quick fixes.** "Workarounds, stubs, disabled gates, and compatibility
  shims are debt unless the plan explicitly declares a bounded brittleness
  window and restoration wave."
- **Architectural transposition wins.** "Elegance, simplicity, and performance
  through structural change are mandatory and desirable in development
  products. Reject sprawling multi-PR sweeps when a single transposition
  lands cleaner."
- **Abrogate before patch.** "For intrinsically failing subsystems, ask 'can
  we delete?' before 'can we patch?'."
- **One path.** "Two orthogonal codepaths for the same logic is a code smell,
  not a feature flag. Collapse to one with the consumer that survives."
- **No legacy code.** "Delete dead code. Do not rename it, hide it behind a
  feature flag, or leave commented remnants."
- **No silent deferrals.** "Planned work lands, is formally retired with
  rationale, or moves to a same-tranche named destination. Cross-tranche
  deferral is a scope-reveal trigger, not a routine close path."
- **Substrate with consumer.** "New abstractions land with a runtime caller,
  test, benchmark, or other proof that the abstraction is consumed."
- **No overfitting.** "A helper, component, parser branch, token, prompt,
  process rule, or public export must have a current consumer and evidence.
  Single-use private helpers inline. Unused public surfaces delete."
- **Every wave is named.** Canonical form `W<N> - <Title>`; bare `W<N>`
  references invalid for any wave that has a name.
- **Evidence beats claims.** "Agent reports are checked against artefacts."
- **Indefatigability belongs to the orchestrator.** "A stuck sub-agent halts
  and reports; the orchestrator replans or redispatches."

GESTALT §7 expands on the load-bearing instruction-layer items: hard-cap on
every dispatch (research 20, plan 15, redress 30, audit 25), triumvirate
auto-trigger (JSONL quiet >15 min OR first-pass no-commit OR three diagnostic
loops OR scope-reveal), worktree isolation, empty-return redispatch, six-agent
ceiling, cherry-pick preserves wave provenance, single cargo per
`CARGO_TARGET_DIR`, iter-profile always, read-size preflight, generated-size
budget, bodyless-large-commits prohibition, close-honesty checklist, hardening
pass, no ledger-only close, substrate-with-consumer, no grammar overfitting,
no silent fallback, failing-test discipline, non-routable carries (chronic
≥3 tranches end at AZ-IV; thesis-review on irresolvability — no successor
letter).

The `typed-materialization-invariant` (`feedback_typed-materialization-invariant`)
binds the four grammar-derived invariants: every `->` in the grammar must reach
the tape emitter; inference composes types, never loses them; parity = full
typed-AST equivalence.

---

## §2 Original 7-wave plan (W0..W6)

Reference table per `AZ-IV.md` §Wave Table + `waves/W<N>.md` per-wave specs.

### W0 - Truth And Canonical Regen

- **Thesis**: get the regen pipeline green and the workspace topology honest
  before any wave attempts parity or substrate work.
- **Sub-units**:
  - W0.1 Truth Ledger (active doc reconciliation; BA/BB historical-frozen).
  - W0.2 Topology Repair (derive residue, csp-solver canonical-source, locks).
    - W0.2.b csp-solver Canonicalisation Enforcement.
    - W0.2.c bbnf_derive Eradication.
  - W0.3 Regen Totality (lowering triad → quartet repair; 9/9 byte-identical).
  - W0.4 Map Preservation (egraph cost/extraction/writeback pin).
  - W0.5 Metadata Gate (manifest fail-closed; staged regen trigger).
- **Hard gates**: regen 9/9 green; cargo metadata --locked at root + wasm/;
  Map preservation test; BA/BB ledger lossless; dev-iteration baseline;
  bbnf-bootstrap cache cycle-2 ≤ 10 %; generated LOC ±5 %; bbnf_derive zero-hit;
  csp-solver canonical-diff.
- **Verification artefacts**: `W0-regen.txt`, `W0-metadata.txt`,
  `W0-dev-baseline.txt`, `W0-bootstrap-cache-honesty.txt`,
  `W0-generated-size.txt`, `W0-derive-eradication.txt`,
  `W0-csp-solver-canonical.txt`, `W0-ba-bb-coverage.md`, `W0-close-ledger.md`.

### W1 - Grammar Generality + Test Redress

- **Thesis**: activate every existing substrate that the OVERFIT-HARD class
  depends on, delete the parallel hand-coded paths, and redress every failing
  test in one wave so runtime-API breakage and test fixes land together.
- **Sub-units**:
  - W1.1 Runtime Overfit Elimination + Test Redress (runtime API).
  - W1.2 Sheets Parity 133/133 + Test Redress (sheets).
  - W1.3 CSS Parity + view/color Deletion + Test Redress (css).
  - W1.4 TS Backend Build-Time + Test Redress (backend-ts).
  - W1.5 Shape Generality + Audit-Tag Fix + Substrate Panic + Manifest
    Strategy + Cross-Cutting Test Redress.
  - W1.6 BBNF Alt-Dispatch Typed-Leaf Push (in W1.5).
  - W1.7 Per-Grammar Discriminator Projection (in W1.1).
  - W1.8 Substrate Panic + Manifest Strategy (in W1.5).
  - W1.9 Failing-Test Redress (per-area, in W1.1-W1.5 worktrees).
- **Hard gates**: regenerated tempdir outputs for parity; workspace nextest
  zero failures; every `#[ignore]` triplet; per-test deletion bodies; JSON
  5/5, BBNF 95/95, Sheets 133/133, CSS lightningcss field-level GREEN; TS
  build+typecheck; shape generality on JSON+non-JSON; no-grammar-name-branch
  CI scan green; manifest-driven `EmitStrategy::for_grammar`; substrate panic;
  7 `from_rule_name` deleted + `(layout.kind, rule_name)` eliminated +
  `leak_static_str` deleted; `view/color` deleted; `recover_*`/wrap byte-recovery
  deleted.
- **Verification artefacts**: `W1-parity-matrix.txt`, `W1-nextest-pass.txt`,
  `W1-ignore-justifications.txt`, `W1-deleted-test-ledger.md`,
  `W1-no-overfit-scan.txt`, `W1-synthetic-strategy.txt`,
  `W1-view-color-deletion.txt`, `W1-recover-deletion.txt`, `W1-ts.txt`,
  `W1-shape-generality.txt`.

### W2 - Path IR + Typed Path<G,T> + AscentStrategy

- **Thesis**: land the typed compile-time Path IR + `path!` proc-macro + IR
  `path_check` pass + bespoke path lexer + hybrid sidecar AscentStrategy. No
  path execution yet (W3 owns lazy bail-out, W5 owns TS binding).
- **Sub-units**:
  - W2.1 Path IR + Type Checker + Error.
  - W2.2 `path_check` IR Pass + Inline-Trace Sidecar.
  - W2.3 bbnf-regex Path Lexer (custom HIR API).
  - W2.4 `path!` Proc-Macro.
  - W2.5 AscentStrategy + Micro-Bench + Wildcard + Variant-Select.
- **Hard gates**: `path!(Json, ..., "text")` resolves at compile time;
  `path!(CssL4, ..., "color")` resolves via variant-selection; wildcard returns
  `Iter<Item = T>`; invalid paths fail compilation with grammar-aware
  diagnostics; `path_check` runs after `project_types`; bbnf-regex path lexer
  ≤ 200 LOC; AscentStrategy three impls + micro-bench artefact; wildcard
  depth-cap diagnostic; path egraph rewrites at `crates/ir/src/rewrites/path_seed.rs`;
  full-chain compile-time error messages; workspace pass-count ≥ W1 close.
- **Verification artefacts**: `W2-path-ir-types.txt`, `W2-path-check-pass.txt`,
  `W2-path-lexer.txt`, `W2-path-macro.txt`, `W2-ascent-microbench.json`,
  `W2-wildcard-iter.txt`, `W2-variant-select.txt`, `W2-path-egraph-seed.txt`.

### W3 - Lazy Bail-Out Parse

- **Thesis**: make the parser path-aware so `parse_with(input, &path)` skips
  unvisited subtrees while sharing generated code with eager mode; close the
  2953x sonic-rs `bbnf_get_twitter` gap to ≤ 5x same-harness.
- **Sub-units**:
  - W3.1 Path Executor + PathCursor + PathSchema.
  - W3.2 Per-Grammar `parse_with` Entry Points.
  - W3.3 Codegen Path-Plan Emitter.
  - W3.4 parse-with Test Suite (4-grammar floor).
  - W3.5 Bench Harness + Profiling.
- **Hard gates**: `JsonParser::parse_with` works on twitter/citm/canada/data_s;
  `CssL4Parser::parse_with` works on bootstrap/normalize incl. variant-select;
  `GoogleSheetsParser::parse_with` works on sheets fixtures;
  `BbnfParser::parse_with` works on self-host fixtures; lazy+eager share
  generated parse functions; deterministic path plan; lazy mode silently
  elides errors past path reach; `bbnf_get_twitter` ≤ 5x sonic-rs same-harness;
  wildcard zero-allocation default lane; variant-selection end-to-end; seven
  required artefacts per profiled entry.
- **Verification artefacts**: `W3-executor-types.txt`,
  `W3-parse-with-coverage.md`, `W3-path-plan-regen-diff.txt`,
  `W3-error-elision-contract.txt`, `docs/benchmarks/profiles/post-AZ-IV/W3/**`,
  `W3-sonic-comparison.json`.

### W4 - Optimization Substrate Activation

- **Thesis**: every existing optimization substrate changes
  emitted/runtime behavior or is deleted; no WIRED-NOT-CONSUMED tolerated.
- **Sub-units**:
  - W4.1 Rewrite Ruler Chain.
  - W4.2 CSP Regex Authority.
  - W4.3 Shape SIMD Consumption.
  - W4.4 DTA DFA Cleanup.
  - W4.5 Pratt View Generality.
- **Hard gates**: every loaded ruleset proves the full production chain; BB
  numeric floors held or block close; CSP consumer-authoritative or removed;
  regex emits chosen scanner class; shape_dict/SIMD/structural-scan runtime
  evidence per active selected fact or retired; DTA/dfa stale claims deleted;
  Pratt/view proven grammar-general or explicitly typed; full
  `W4-substrate-denominator.md` covering active mined fact/sidecar/rule/
  template/shape/scan/Pratt/view/regex/CSP/egraph; each WIRED-NOT-CONSUMED
  substrate from Babbage matrix has a consumer or deletion.
- **Verification artefacts**: `W4-rewrite-chain.txt`, `W4-csp-regex.txt`,
  `W4-shape-simd.txt`, `W4-legacy-deletion-ledger.md`,
  `W4-substrate-denominator.md`, `W4-ruler-oracle-ranker.json`.

### W5 - TS Binding + Value-API + Substrate Audit

- **Thesis**: land the TS binding for the `path!` macro at parity with Rust,
  finish per-grammar value-API consolidation (skeleton dedup; typed enums
  preserved), land the permanent substrate-audit test that prevents future
  "substrate without consumer" regressions.
- **Sub-units**:
  - W5.1 bbnf-path-ts cdylib + Template Tag + Isomorphism Tests.
  - W5.2 TS Node Execution Proof.
  - W5.3 Per-Grammar Value-API Dedup (Structural Skeleton).
  - W5.4 Permanent Substrate-Audit Test.
  - W5.5 Profiling Pass + Wasm-Crossing Attribution.
- **Hard gates**: `crates/bbnf-path-ts/` cdylib builds with `wasm-pack`; TS
  template-literal tag compiles in Vite/esbuild + resolves to same TypedPath
  shape as Rust `path!`; isomorphic `PathError` taxonomy; Node executes
  TS parser end-to-end on twitter.json; per-grammar arena ≤ 30 LOC + builder
  ≤ 50 LOC; typed `*Value` enums byte-identical to W4 close;
  `crates/ir/src/passes/tests/substrate_audit.rs` enumerates every `pub`
  substrate, runs in < 60s, CI-gated; seven required artefacts per profiled
  entry; wasm-crossing samply ≤ 100 µs per cross-call; per-grammar dedup
  ≤ 5 % regression on twitter parse vs W4 close; AZ-IV.md §Hard Gates 13/20/21
  close here.
- **Verification artefacts**: `W5-bbnf-path-ts-build.txt`,
  `W5-isomorphic-error.txt`, `W5-node-execute.txt`,
  `W5-arena-builder-dedup.md`, `W5-substrate-audit-pass.txt`,
  `W5-substrate-denominator.md`, `W5-profiling-pass.md`,
  `docs/benchmarks/profiles/post-AZ-IV/W5/**`.

### W6 - Measurement And Close

- **Thesis**: refresh `post-AZ-IV.json` under fat-LTO close profile + bench-iter
  comparison profile; close every AZ-III carry; run the close-honesty
  checklist; produce `FINAL.md` only at close.
- **Sub-units**:
  - W6.1 Benchmark Matrix.
  - W6.2 Workspace Gates.
  - W6.3 Close Docs.
- **Hard gates**: regen --check passes; fmt + clippy + nextest workspace pass;
  fat-LTO matrix lands at `docs/benchmarks/post-AZ-IV.json` per SPEC.md;
  row-by-row AU floor + post-AZ same-profile + AZ-III bench-iter deltas;
  `bbnf_value_*` parity-or-better with `sonic_value_*`; data_xl, tailwind,
  compile_css_l4 active measured Divan rows; samply seven-artefact contract;
  IAI thresholds; substrate denominator no unowned rows; non-routable carries
  every row resolved with cited evidence (or thesis-review triumvirate, no
  successor letter); close-honesty checklist passes.
- **Verification artefacts**: `docs/benchmarks/post-AZ-IV.json`,
  `W6-fat-lto.txt`, `W6-bench-iter.txt`, `W6-sonic-projection.txt`,
  `W6-iai.txt`, `W6-workspace-gates.txt`, `W6-profiling-pass.md`,
  `docs/benchmarks/profiles/post-AZ-IV/**`, `FINAL.md`.

---

## §3 What landed (W0..W2)

92 commits between `2678ed44` (tranche open) and `10ac5448` (HEAD); 245 files
changed, +33490 / −3161 LOC per the mid-tranche audit's diff stat.

### W0 - Truth And Canonical Regen — complete

| Status | Commit | Subject + scope summary |
|---|---|---|
| ✅ INTENDED | `01c15564` | docs(az-iv): record pre-W0 generated-LOC baseline anchor (orchestrator-owned) |
| ✅ INTENDED | `bd2769f3` | docs(codegen): excise stale TapeDirect/Parsed wording from crate map (W0.1) |
| ✅ INTENDED | `d4fb8835` | docs(az-iv): refresh W0-ba-bb-coverage ledger with landed evidence (W0.1) |
| ✅ INTENDED | `cbcff434` | docs(az-iv): record W0 failing-test census (1527 tests / 1445 pass / 78 fail / 4 timeout / 25 skip) |
| ✅ INTENDED | `138bd1ab` | feat(registry/manifest-strategy-reader): manifest-driven binding scaffold + synthetic test + W0.3 halt |
| ✅ INTENDED | `89fbada8` | fix(xtask-metadata): fail-closed metadata gate + Cargo.toml staged trigger (W0.5) |
| ✅ INTENDED | `92ce2cb1` | chore(workspace-topology): repair wasm patches + scrub xtask doc-link references (W0.2) |
| ✅ INTENDED | `3aab34e8` | chore(csp-solver/canonical-diff): re-vendor src/ from csc411@b70098676 (W0.2.b) |
| ✅ INTENDED | `d36055aa` | docs(az-iv/audit): land W0.2 verification artefacts (derive eradication + csp-solver diff + cache honesty) |
| ✅ INTENDED | `4373a49d` | fix(egraph-cost): preserve `Map { fn_id }` wrapper through cost-guided extraction (W0.4; named test red-then-green) |
| 🔄 SCOPE-ABSORBED | `a975844b` | docs(az-iv/audit): land REGEN triumvirate research lane |
| 🔄 SCOPE-ABSORBED | `2246a87b` | docs(az-iv/audit): land REGEN triumvirate plan lane |
| 🔄 SCOPE-ABSORBED | `70f7f1df` | docs(az-iv): apply REGEN triumvirate plan-lane amendments to W0 + parent |
| ✅ INTENDED | `27592f4e` | fix(lower/expression): structural detection across wrap+repeat+alt for canonical parser tree (W0.3 R1) |
| 🆕 SCOPE-REVEAL | `3c00fb88` | docs(az-iv/audit): W0.3 redress halt — fourth surface in expression/mod.rs |
| 🆕 SCOPE-REVEAL | `8ead0d29` | docs(az-iv): absorb fourth lowering surface into W0.3 carve |
| 🆕 SCOPE-REVEAL | `7fdcd803` | fix(lower/expression/mod): replace is_single_token_span predicate with structural BbnfKind::Span gate (W0.3 R2 quartet) |
| ✅ INTENDED | `cb3a40d5` | chore(grammar/generated): regen 9/9 against fixed lowering quartet |
| ✅ INTENDED | `1e0a738b` | docs(az-iv/audit): land W0.3 REGEN-redress evidence; retire halt reports |
| ✅ INTENDED | `57ca2cb2` | docs(az-iv/audit): record W0.3 post-regen test census + new-failure provenance |
| ✅ INTENDED | `7959e6cb` | docs(az-iv/audit): land W0 generated-size budget verification (-2.10 % aggregate) |
| ✅ INTENDED | `581cb568` | docs(az-iv/progress): close W0 ledger; mark wave complete with REGEN triumvirate trail |
| ✅ INTENDED | `f3143c13` | docs(az-iv/audit): land W0 dev-iteration baseline (cold + warm walls) |

**Triumvirate fired**: REGEN (research → plan → 2-pass redress; fourth surface
absorbed mid-redress per ORCHESTRATION.md §Stalls). Audit trail:
`audit/REGEN-{research,plan,redress}.md`. The triad-now-quartet is codified in
AZ-IV.md §Orchestration Rule 14: "the W0.3 lowering triad
(`crates/core/src/lower/expression/{wrap,repeat,alt}.rs`) is one unit of
repair landed in one commit. Predicate-driven structural detection in lowering
is the underlying defect class".

**Halt + recovery**: `audit/W0.3-redress-HALT.md` (replaced by
`REGEN-redress.md` at landing) — the redress lane discovered the fourth
surface (`expression/mod.rs::dispatch_expression`) during the triad fix
and absorbed it inline rather than re-triumvirate. Hard cap was extended from
60 → 75 min wall per the dispatch's "may extend if scope reveal demands"
clause.

**Hard-gate posture (per mid-tranche audit §2)**: all 11 W0 gates ✅; aggregate
generated LOC -2.10 % (within -5/+5); cycle-2 cache 1.88 % of cycle-1
(threshold ≤10 %); regen --check 9/9 byte-identical; csp-solver 22/22 shared
files byte-identical to csc411@b70098676; bbnf_derive zero hits across active
trees.

### W1 - Grammar Generality + Test Redress — complete

| Status | Commit | Subject + scope summary |
|---|---|---|
| ✅ INTENDED | `bf0cd2fe` | docs(az-iv/progress): record W1.1+W1.7 scope reveal HALT pending triumvirate |
| ✅ INTENDED | `8d514576` | docs(az-iv/audit): land W1.2 sheets halt report; route codegen regression to W1.5 |
| ✅ INTENDED | `07369e95` | refactor(runtime/view-color-delete): excise color shim, fold peel, retire reexport (W1.3 + W1) |
| ✅ INTENDED | `a397d882` | fix(backend/ts-build-time): wrap rule body in discriminated union, enforce EOF, declare host fns (W1.4) |
| ✅ INTENDED | `fbbee8c8` | fix(emitter+registry): substrate panic + manifest-driven strategy (W1.8) |
| ✅ INTENDED | `d30910aa` | fix(pipeline+grammar): audit-tag aliasing + host wildcard (W1.5 / F6 + F10) |
| ✅ INTENDED | `c56bda3f` | test(no-grammar-name-branch): CI-enforced static AST scan |
| ✅ INTENDED | `2b2b221e` | fix(lower/expression): delete recover_modifier + recover_binary_op + byte-recovery (W1.6 / F8) |
| ✅ INTENDED | `f4f40add` | fix(lower/value_expr): route numeric atom Spans + reinstate operator recovery (W1.5) |
| ✅ INTENDED | `49a99cfe` | fix(grammar/host): split @token/@debug decoders by grammar admittance (W1.5 / F10) |
| ✅ INTENDED | `b68d0e4d`+`2d270daf` | W1.5 cross-cutting: `decode_token_name` / `decode_debug_name` split |
| 🔄 SCOPE-ABSORBED | `c06a8802` | docs(az-iv/audit): land W1.5 cross-cutting halt report + post-redress nextest |
| 🔄 SCOPE-ABSORBED | `4b794eba` | docs(az-iv/progress): record W1.5 cross-cutting redress + halt routing |
| ✅ INTENDED | `0ffbd754` | refactor(runtime/discriminator-from-rule-id): rule-id-keyed kind dispatch + `leak_static_str` delete (W1.1+W1.7; **Path B** chosen via triumvirate) |
| ✅ INTENDED | `099993e5` | test(redress-runtime-api): thread real rule-ids through synth_layout fixtures (W1.9) |
| 🔄 SCOPE-ABSORBED | `4f741543` | fix(lower/mapped-factor+value-atom): admit empty-span value-expr compounds (W1.2) |
| 🔄 SCOPE-ABSORBED | `758e69d6` | fix(emitter/shapes-keyword-flat-typed-leaf+hregex): admit constant-fold Map payload + strip int-suffix (W1.2 reroute through W1.5) |
| 🔄 SCOPE-ABSORBED | `0e670141` | fix(grammar/sheets-parity-133): collapse sheet_prefix to single Span regex (W1.2) |
| 🔄 SCOPE-ABSORBED | `b892fe96`+`bc9996c3` | docs(az-iv/audit): W1.2 retry halt + regen sync after typed-leaf codegen lift |
| 🆕 SCOPE-REVEAL | `21efc4cc` | fix(emitter/keyword/null-marker): single-literal IntLit → push_leaf_with_unit (W1.9) |
| 🆕 SCOPE-REVEAL | `47c5f0b1` | fix(lower/wrap-named-annotation-propagate): TypeAnnotation kind + parent-source recovery (W1.9) |
| 🆕 SCOPE-REVEAL | `75eba5e9` | fix(runtime/bbnf-builder-start-offset): record compound bounds at begin/end_compound (W1.9) |
| 🆕 SCOPE-REVEAL | `2f0bd126` | fix(analysis/term-branch-tag): correct branch indexes for 9-branch term alternation (W1.9) |
| ✅ INTENDED | `9807343a` | chore(grammar/generated): regen 9/9 against W1.9 lower + emitter lifts |
| ✅ INTENDED | `069f08db`+`fbf79c5a` | fmt sweep + W1.9 final halt report |
| 🆕 SCOPE-REVEAL | `9ad51fc3` | fix(runtime/sheets+css_l4-rule-id-realign): realign begin_compound dispatch to current grammar regen (W1-zero) |
| 🆕 SCOPE-REVEAL | `0f2be133` | fix(runtime/css_l4-hex-color-frame): decode hex digits via parse_hex_color shim (W1-zero) |
| 🆕 SCOPE-REVEAL | `d891f52c` | fix(analysis/grouped-term-references-collect): recurse into all compound children (W1-zero) |
| 🆕 SCOPE-REVEAL | `700d6170` | fix(tests/json-parity-fast-float-tolerance): admit 1-ULP divergence (W1-zero) |
| 🆕 SCOPE-REVEAL | `bcf68bda` | fix(backend/ts-host-fn-name+input-bind): strip Rust path qualifiers + bind __input (W1-zero) |
| ✅ INTENDED | `f4ab9e90` | docs(az-iv/audit): land W1-zero halt report + post-redress nextest evidence |
| ✅ INTENDED | `4c28c2a8` | docs(az-iv/audit): land W1-CLOSE triumvirate research lane (3 classes: Sheets / CSS / TS) |
| ✅ INTENDED | `1be64115` | docs(az-iv/audit): land W1-CLOSE plan lane (Path B for runtime; `unknown` for TS Color) |
| ✅ INTENDED | `65cdec67` | fix(backend/ts-named-type-preamble): emit `type <Name> = unknown` (W1-CLOSE.C) |
| ✅ INTENDED | `5db5f30d` | fix(emitter/alt-dispatch-per-arm-map-payload): descend into nested Alt for prefix-factored Map (W1-CLOSE.B) |
| ✅ INTENDED | `1e4f25cc` | fix(runtime/css-l4-dir-pseudo-frame): route dirPseudo to DirPseudo OpenFrame (W1-CLOSE.B) |
| ✅ INTENDED | `6619d2dc` | fix(runtime/css-l4-push-branch-tag-narrow): drop GlobalKeyword/MathOperator trial-cast (W1-CLOSE.B) |
| ✅ INTENDED | `1c75f55c` | regen(grammar/css-l4): post-W1-CLOSE.B alt-dispatch payload + DirPseudo |
| ✅ INTENDED | `61100f9d` | fix(ir/operator-chain-per-rule-lut): scope Pratt LUT entries to owning rung (W1-CLOSE.A) |
| ✅ INTENDED | `bce11f73` | fix(emitter/wrap-string-span-capture): include surrounding quotes in -> Span emission (W1-CLOSE.A) |
| ✅ INTENDED | `e109fb4d` | fix(emitter/wrap-regex-span-capture): push matched bytes for regex-led alt branches (W1-CLOSE.A) |
| ✅ INTENDED | `2e669b58`+`d89a9dd9` | regen(grammar/google-sheets+bbnf+css_l4+ebnf): cross-grammar drift from W1-CLOSE.A |
| ✅ INTENDED | `68969b43`+`7edc8b7b` | docs(az-iv/audit): refresh W1-nextest-pass + record W1-CLOSE triumvirate trail |

**Triumvirates fired**:
- W1.1+W1.7 HALT (Path A/B/C decision; PROGRESS rows record triumvirate
  research → plan-lane Path B chosen → redress).
- W1.2 sheets halt + retry halt (`audit/W1.2-sheets-halt-report.md` +
  `W1.2-sheets-retry-halt.md` route codegen regression to W1.5; absorbed).
- W1.5 cross-cutting halt (`W1.5-cross-cutting-halt-report.md`: 8 residual
  failures route to triumvirate per LOCKED-territory class; W1.9 owns
  follow-on lift for classes a/b/c).
- W1-zero final close push (`W1-zero-halt.md`: 5 distinct failure classes
  closed in one extended-cap redress; -129 net failures 142→13).
- W1-CLOSE (research → plan → 3 parallel redress lanes A/B/C):
  `W1-CLOSE-research.md`, `W1-CLOSE-plan.md`. Drove residual 13→0.

**Halt + recovery trail**: W1.1+W1.7 scope-reveal recorded a clean halt with
3 viable Paths A/B/C; the plan lane chose Path B (runtime arena/builder owns
enum, codegen owns rule-id mapping via `from_rule_id(u32)`). W1.5 + W1.9 +
W1-zero + W1-CLOSE chain absorbed multiple cross-cutting defect classes that
no single sub-unit could close in isolation: typed-Nu8 codegen regression
seeded by W0.3 quartet; wrap.rs Named-annotation propagation; runtime/bbnf
compound start-offset bounds recording; rule-id realignment after grammar
regen; CSS hex-color frame; Sheets array_rows Pratt over-share.

**Final W1 close evidence**: `W1-nextest-pass.txt` shows 1538 run / 1536
passed / 0 failed / 2 timed out (W4 carry: tailwind perf) / 26 skipped.
Net W1 redress: 142 → 0 failing tests.

### W2 - Path IR + Typed Path<G,T> + AscentStrategy — complete

| Status | Commit | Subject + scope summary |
|---|---|---|
| ✅ INTENDED | `de807600` | docs(az-iv/audit): land W2-path-lexer evidence (186 LOC ≤ 200; W2.3) |
| ✅ INTENDED | `916182dd` | feat(path-ir+type-check): typed compile-time Path IR + type-checker + grammar markers (W2.1) |
| ✅ INTENDED | `2c0ec381` | chore(backend/ts/emitter): cargo fmt sweep on collect_named_types signature |
| ✅ INTENDED | `a85e28d3` | feat(path-ascent): AscentStrategy trait + three impls + HybridSidecar default (W2.5) |
| ✅ INTENDED | `d87d7cc0` | feat(path-wildcard): lazy WildcardIter + with_anchors + depth cap (W2.5) |
| ✅ INTENDED | `3947c269` | feat(path-variant-select): typed-enum variant resolver (W2.5) |
| ✅ INTENDED | `a8a878f8` | feat(bench/path-ascent): parent-pointer micro-bench harness (W2.5) |
| ✅ INTENDED | `4df2e4c6` | test(path-wildcard-iter): fixture coverage for lazy-iter surface (W2.5) |
| ✅ INTENDED | `e8b749d8` | test(path-variant-select): fixture coverage for variant resolver (W2.5) |
| ✅ INTENDED | `5b74aab2` | docs(az-iv/audit): land W2-ascent-microbench evidence + default pick (HybridSidecar; -1.5/-3.0/-3.8% vs InStruct) |
| ✅ INTENDED | `8ad209f0` | feat(bbnf-path/macro): `path!` proc-macro for compile-time-typed paths (W2.4) |
| ✅ INTENDED | `6b9e069e` | test(tests/path-macro-compile): positive fixtures for `path!` (W2.4) |
| ✅ INTENDED | `490e373d` | test(tests/path-macro-errors): negative compile fixtures + audit (W2.4) |
| ✅ INTENDED | `79a00fa7` | feat(ir-passes/inline-trace): record Ref(source)→body substitutions for path-check resolver (W2.2) |
| ✅ INTENDED | `08b95c75` | feat(ir-passes/path-check): source-rule-name resolver from inline trace + StructRegistry (W2.2) |
| ✅ INTENDED | `d1b753e3` | wire(pipeline/compile-path-check): route inline trace + run path_check after project_types (W2.2) |
| ✅ INTENDED | `b7ce6a28` | test(ir-passes/inline-trace): golden tests for InlineTrace recording determinism (W2.2) |
| ✅ INTENDED | `ac1fa837` | test(ir-passes/path-check): fixture grammar fused-rule resolves through inline trace (W2.2) |
| ✅ INTENDED | `0dc835da` | style(w2.2): apply cargo fmt to inline-trace + path-check files |
| ✅ INTENDED | `66c8b4c9` | docs(az-iv/audit): land W2-path-check-pass evidence (W2.2) |
| ✅ INTENDED | `8c79a3ff` | docs(az-iv/audit): land W2 close nextest evidence (1582/0; +46 over W1) |
| ✅ INTENDED | `058510b6` | docs(az-iv/progress): mark W2 complete; flag path-egraph-seed W3 carry |
| ✅ INTENDED | `10ac5448` | docs(az-iv/audit): land W0+W1+W2 mid-tranche audit |

**Triumvirates fired**: none. Round 1 (W2.1 + W2.3) clean; round 2
(W2.2 + W2.4 + W2.5) clean. Per the mid-tranche audit §3: "only minor scope
deviation: W2.4 carries synthetic registry fixture pending W4/W5 production
wiring".

**Hard-gate posture (per mid-tranche audit §2)**: 8/9 of W2's hard gates met;
gate 9 (path egraph rewrites at `crates/ir/src/rewrites/path_seed.rs`) ⚠
**DEVIATION**: not landed; absorbed into W3 opening per PROGRESS.md
`Carry to W3` annotation. Listed as small-scope absorption, not silent
deferral.

**AscentStrategy default pick**: HybridSidecar — sheets 237.2 µs vs
in-struct 246.7 µs (−3.8%); citm 1.847 ms vs 1.876 ms (−1.5%); tailwind
4.519 ms vs 4.658 ms (−3.0%). RootTraversal 13–14× slower across the matrix.
Type alias `pub type DefaultAscent = HybridSidecar` per
`audit/W2-ascent-microbench.json`.

**Path lexer**: 186 LOC at `crates/bbnf-regex/src/path_lexer.rs` (sibling
parse-that path-patched via `[patch.crates-io]`); no `regex-syntax` dep;
deterministic Span attribution; round-trip on adversarial path strings.

**Path macro**: positive fixtures pass; negative fixtures (unknown_field,
unknown_marker, index_into_struct, empty_path) emit grammar-aware
diagnostics. Verbatim diagnostic example:

```
error: path[2] `nope` not found on `Status`; valid fields: text, retweets, sensitive
```

### Integration-gap flag (none surfaced)

The audit reveals no substantive deviation NOT yet documented. The two
deviations (W2.4 synthetic-registry fixture; W2.9 path-egraph-seed) are
explicitly tracked in PROGRESS.md and the mid-tranche audit. No genuine
integration gap requires a path-forward synthesis flag.

---

## §4 Triumvirate trail

Two triumvirates fired in W0..W2; both landed all three lanes
(research → plan augment/synthesis → redress) with the mandatory `## Exact
Wave-Amendment Text` section in plan artefacts (per ORCHESTRATION.md
§Triumvirate). No silent-skip lanes.

### REGEN triumvirate (W0.3)

- **Trigger**: W0.3 halt — regen drift root cause exceeded the original
  W0.3 budget of "xtask + strategy registry + one lowering/emitter surface".
  Two regressions (R1 HRegex int/float typed-leaf collapse; R2 BBNF self-host
  parse error at byte 36) traced to one shared defect: predicate-driven
  detection silently dropping structural information across three lowering
  surfaces (`wrap.rs::lower_mapped_factor`,
  `repeat.rs::lower_factor::recover_modifier`,
  `alt.rs::lower_concatenation`/`lower_alternation`).
- **Research lane** (`audit/REGEN-research.md`): proved R1 and R2 are
  manifestations of one shared defect class; pinned commit `954d166b` (the
  `bootstrap_parser` deletion that removed the compensating wrap); enumerated
  the 3-surface change footprint; named the W3a.4 audit prescription as
  unfinished routes A and B.
- **Plan lane** (`audit/REGEN-plan.md`): synthesised one redress agent
  landing all three lowering surfaces in one commit + mechanical regen as a
  follow-on commit; produced 4 paste-ready amendment blocks (W0.md
  §File Bounds, §Disjointness, §AZ-IV.W0.3 Regen Totality, AZ-IV.md
  §Orchestration Rules rule 14); recommended HARD CAP 60 min (doubled from
  default 30) for the joint surface load.
- **Redress lane** (`audit/REGEN-redress.md`): landed structural detection
  across the triad (commit `27592f4e`) → triad-redress halt revealed a
  fourth surface (`expression/mod.rs::dispatch_expression` leaf-fastpath
  predicate) → fourth-surface fix landed (`7fdcd803`) → mechanical regen 9/9
  byte-identical (`cb3a40d5`). Hard cap was extended from 60 to 75 min wall
  per the dispatch's "may extend if scope reveal demands" clause.
- **Plan amendments**: AZ-IV.md §Orchestration Rule 14 codified the
  lowering-quartet-as-one-unit-of-repair invariant; W0.md §File Bounds
  added the four lowering surfaces as modify-carve rows; §Disjointness
  rewrote ownership; §AZ-IV.W0.3 mechanism extended to cover the
  canonical-parser-tree divergence.
- **Residual carry**: dispatched-quartet absorbed the fourth surface within
  W0; PROGRESS rows record the absorption per ORCHESTRATION.md §Stalls (file-
  bound expansion at most 2 paths AND hard gate unchanged → absorbed).
  Subsequent integration into W1.5+W1.9 because the codegen regression that
  the triad seeded (typed-Nu8 emission loss across keyword/flat shapes)
  surfaced post-regen as a Sheets parity gap — routed to W1.5's slice and
  closed by `758e69d6`.

### W1-CLOSE triumvirate (W1 final close)

- **Trigger**: residual 13 failures gating W1 close after W1-zero (final
  redress push that drove 142→13). Three independent defect classes
  partitioned the 13: A=Sheets (7), B=CSS L4 (5), C=TS (1).
- **Research lane** (`audit/W1-CLOSE-research.md`): enumerated each defect
  with file:line + commit-of-introduction; named 7 source files + 2
  regenerated grammars across the three classes; partition rationale: classes
  share no IR/runtime/codegen paths.
- **Plan lane** (`audit/W1-CLOSE-plan.md`): chose **Decision 1 (Pratt LUT
  scope)**: per-rule LUT scoping at `operator_chain.rs:227-266` (per-rule
  scoping is the architectural fix; reclassification of array_row/array_rows
  out of `ShapeTag::Pratt` is escape-hatching). Chose **Decision 2 (TS Color
  type)**: emit `type Color = unknown` (lightest declaration that satisfies
  `tsc --noEmit` typecheck; W5 owns the structural binding when it lands the
  runtime). Three disjoint redress packets W1-CLOSE.A/B/C with caps 30/30/15
  min.
- **Redress lanes** (parallel A/B/C):
  - A (Sheets): 5 commits closing 7 sheets failures via per-rule Pratt LUT
    + Wrap-string Span capture + Wrap-regex Span capture + 4-grammar regen.
  - B (CSS L4): 4 commits closing 5 CSS failures via alt-dispatch nested-Alt
    Map projection + DirPseudo OpenFrame + GlobalKeyword/MathOperator narrow
    + CSS L4 regen.
  - C (TS): 1 commit closing 1 TS failure via `type Color = unknown`
    preamble.
- **Outcome**: residual 13 → 0; final workspace nextest 1538 / 1536 / 0 / 2
  timed out (W4 carry) / 26 skipped.
- **Residual carry**: TS `Color = unknown` is a documented W5 carry — the
  cdylib + wasm-bindgen binding (W5.1) replaces `unknown` with executable
  types. Mid-tranche audit §7 R3 records this risk explicitly.

---

## §5 What was deferred / chronically deferred

Cross-reference of `AZ-IV.md` §Non-Routable Carries against landed evidence
through W2 close.

### LANDED (commit hashes)

| # | Item | Owner | Landing commit |
|---:|---|---|---|
| 1 | Strict regen drift (7/9 grammars red) | W0 | `cb3a40d5` (regen 9/9) + `27592f4e`+`7fdcd803` (lowering quartet) |
| 2 | Egraph `Map { fn_id }` preservation | W0 | `4373a49d` |
| 10 | Bootstrap/derive residue (sibling) | W0 | `92ce2cb1` (wasm patches + xtask doc-link refs) |
| 15 | WASM/sibling derive residue | W0 | same as above |
| 16 | csp-solver canonical-source split | W0 | `3aab34e8`+`d36055aa` (re-vendor src/ from csc411@b70098676; 22 shared files byte-identical) |
| 17 | bbnf-bootstrap cache nuke | W0 | cycle-2 = 1.88 % of cycle-1 (`audit/W0-bootstrap-cache-honesty.txt`) |
| 18 | Dev-iteration baseline gate | W0 | `f3143c13` (`audit/W0-dev-baseline.txt`) |
| 19 | Generated-size budget (per-grammar ±5 %) | W0 | `7959e6cb` (-2.10 % aggregate) |
| 20 | 7 `from_rule_name(&str)` impls eliminated | W1 | `0ffbd754` (Path B: rule-id-keyed dispatch) |
| 21 | `(layout.kind, rule_name)` builder dispatches eliminated | W1 | `0ffbd754` |
| 22 | `EmitStrategy::for_grammar` 9-arm allowlist eliminated | W1 | `fbbee8c8` (manifest-driven) + `138bd1ab` (scaffold W0) |
| 23 | `substrate_path` JSON-builder fallback retired (panic!) | W1 | `fbbee8c8` |
| 24 | `recover_modifier`/`recover_binary_op` deleted | W1 | `2b2b221e` |
| 26 | All failing tests redressed (1527/1527 pass, 1538 with new tests) | W1 | W1-zero (`9ad51fc3` + 4 follow-ups) + W1-CLOSE (A/B/C lanes) |
| 27 | Path IR + compile-time `path!` macro | W2 | `916182dd`+`8ad209f0` |
| 28 | `path_check` IR pass after `project_types` | W2 | `79a00fa7`+`08b95c75`+`d1b753e3` |
| 29 | AscentStrategy hybrid sidecar | W2 | `a85e28d3`+`a8a878f8` (micro-bench `audit/W2-ascent-microbench.json`) |
| 32 | Variant-selection path step (typed-enum) | W2 | `3947c269`+`e8b749d8` |
| 33 | Wildcard yields `Iter<Item = T>` | W2 | `d87d7cc0`+`4df2e4c6` |
| 12 | `backend/rust/view/color` shim | W1 | `07369e95` |
| 3 | Sheets parity 133/133 | W1 | W1-CLOSE.A (`61100f9d`+`bce11f73`+`e109fb4d`+`2e669b58`) |

### ROUTED-CARRY (forward to which wave/letter)

| # | Item | Routed to | Reason |
|---:|---|---|---|
| 4 | TS backend executable (Node-execute) | W5 | W1 closed at build-time correctness only; cdylib + wasm-bindgen + Node-execute proof is W5.1+W5.2 territory |
| 5 | Tailwind regex_scan perf timeout | W4 | W1 nextest carries 2 timeouts (W4 perf carry per W1-CLOSE evidence) |
| 6 | Cross-profile watchdog rows | W6 | fat-LTO matrix close is W6.1 |
| 7 | JSON value/path vs sonic-rs perf | W6 | post-AZ-IV.json same-harness comparator |
| 8 | CSS named_color runtime activation | W1+W4 | W1 closed parity (W1-CLOSE.B `5db5f30d` per-arm Map payload); W4 owns extractor binding |
| 9 | PatternAnnotations migration | W4 | W4 owns Pratt/view generality |
| 11 | DTA/dfa naming + cleanup | W4 | W4.4 DTA DFA Cleanup |
| 13 | Substrate denominator (permanent test) | W5 | `crates/ir/src/passes/tests/substrate_audit.rs` is W5.4 |
| 14 | Unconsumed `RuleSet` + `egraph::ruler::*` deletion | W4 | W4.1 BA-recreates-clean precondition |
| 25 | Per-grammar arena/builder dedup (skeleton) | W5 | W5.3 |
| 30 | Lazy bail-out parse on 4 production grammars | W3 | core W3 thesis |
| 31 | TS template-literal tag binding | W5 | W5.1 |

### ABSORBED (folded into a different wave's scope)

| Item | Absorbed by | Why |
|---|---|---|
| W0.3 fourth lowering surface (`expression/mod.rs::dispatch_expression`) | W0 | scope reveal: file-bound expansion 1 path AND hard gate unchanged → absorbed per ORCHESTRATION.md §Stalls |
| W1.2 typed-Nu8 codegen regression (post-W0.3 emission loss in keyword/flat shapes) | W1.5 | sheets parity halt routed codegen restoration to W1.5 (the `758e69d6` lift); W1.2 stayed open until codegen restoration landed, then re-ran the parity sweep |
| W1.9 numeric-literal-with-suffix carry from `wrap.rs::lower_map_arrow` | W1.9 + W1-zero | post-W0.3 `null = "null" -> 0u8` regression surfaced post-regen (88 fail post-W0.3 vs 78 pre-baseline; +10 net); closed in W1.9 + W1-zero |
| BBNF source-byte recovery (`recover_modifier`/`recover_binary_op`) and W1.6 alt_dispatch typed-leaf push | W1.5 (`2b2b221e`) | scoping note: W1.6 sequenced inside W1.5 worktree per Disjointness |
| W2.9 path egraph rewrites seed (`crates/ir/src/rewrites/path_seed.rs`) | W3 opening | small absorption per mid-tranche audit §6 (3 hand-authored rules: duplicate-prefix elimination + redundant downcast removal + adjacent-accessor fusion) |

### UNRESOLVED

None. All non-routable items 1..33 either LANDED, ROUTED-CARRY (with named
forward wave), or ABSORBED (with named absorbing wave). Per
ORCHESTRATION.md §Stalls and AZ-IV.md §Non-Routable Carries: a non-routable
carry that survives close is a process failure; AZ-IV close runs the
close-honesty checklist before declaring close. Through W2 close, no carry
is "later" or ownerless.

---

## §6 Documentation accuracy

Audit of `PROGRESS.md` Wave Status table vs reality.

### Status word accuracy

| Wave | PROGRESS.md status | Reality | Match? |
|---|---|---|---|
| W0 - Truth And Canonical Regen | complete | mid-tranche audit confirms 11/11 W0 hard gates ✅; REGEN triumvirate fired; quartet landed; regen 9/9 byte-identical | ✅ |
| W1 - Grammar Generality + Test Redress | complete | mid-tranche audit confirms 13/13 W1 hard gates ✅ (gate 3 ⚠ "every #[ignore] triplet — NOT YET VERIFIED EXPLICITLY"); 1538/1536 final | ✅ (modulo gate 3) |
| W2 - Path IR + Typed Path<G,T> + AscentStrategy | complete | mid-tranche audit confirms 8/9 W2 hard gates ✅; gate 9 (path egraph rewrites seed) ⚠ absorbed into W3 | ✅ (with annotated W3 absorption) |
| W3..W6 | planned | unopened | ✅ |

The W1 gate 3 (`#[ignore]` triplet enumeration) is flagged as ⚠ in the
mid-tranche audit §2 W1 row. This is documented as W6 close-honesty work,
not silent debt. PROGRESS.md `Close-Honesty Parking Lot` row "Every `#[ignore]`
carries owner + deadline-commit + reason + ticket" is W1-owned (planned,
non-routable) and currently un-evidenced; the 26 skipped tests in
`W1-nextest-pass.txt` need the per-ignore audit before W6 close can pass the
checklist.

### Cited commit hash resolution

Spot-checked the 23 cited hashes in `PROGRESS.md` Running Evidence Ledger:
`bd2769f3`, `d4fb8835`, `cbcff434`, `89fbada8`, `92ce2cb1`, `3aab34e8`,
`d36055aa`, `4373a49d`, `27592f4e`, `7959e6cb`, `b68d0e4d`, `2d270daf`,
`0ffbd754`, `fbbee8c8`, `c56bda3f`, `058510b6`, `10ac5448`, plus the REGEN
triumvirate commits `a975844b`+`2246a87b`+`7fdcd803`+`cb3a40d5`+`1e0a738b`+
`57ca2cb2`. Every cited hash resolves on master. No drift.

### Cited audit-doc paths

Spot-checked `PROGRESS.md` Wave Status `Evidence` column: every audit doc
referenced (`audit/W0-*.{txt,md}`, `audit/REGEN-{research,plan,redress}.md`,
`audit/W1-*.{txt,md}`, `audit/W1-CLOSE-{research,plan}.md`,
`audit/W2-*.{txt,md,json}`) exists in the working tree. No broken paths.

The audit dir holds 41 artefacts (excluding `W0-failing-test-census.txt` at
5203 lines). Every artefact is referenced by either AZ-IV.md, a wave spec,
PROGRESS.md, or another audit doc.

### Drift notes

- W0 close ledger artefact `W0-close-ledger.md` listed in W0.md
  §Verification Artefacts is **not present** in the audit dir; the W0 close
  evidence is distributed across `W0-*.txt` artefacts + `W0-pre-baseline.md`
  + `audit/REGEN-*.md` + commit `7959e6cb`'s body. Not a process failure
  (the artefact is fact-equivalent), but the W0.md spec lists a path that
  doesn't materialise as a single file. Suggest renaming the spec row to
  `W0-pre-baseline.md` + the distributed `W0-*.txt` set, or producing a
  consolidated close ledger before W6.

No other documentation drift surfaces.

---

## §7 Memory feedback compliance

Memory feedback entries most relevant to AZ-IV W0..W2 work, with compliance
evidence.

| Feedback | What it says | Compliance |
|---|---|---|
| `feedback_typed-materialization-invariant` | Every `->` in the grammar must reach the tape emitter; inference composes types, never loses them | W0.3 quartet redress (`27592f4e`+`7fdcd803`): replaced predicate-driven detection with structural detection in lowering, with loud panic on unmatched annotations. Quoted verbatim in REGEN-research.md §5 and REGEN-plan.md §1. |
| `feedback_no-grammar-overfitting` | Production runtime/builder/dispatch derives discriminants from `StructRegistry`, `TypeDesc`, `FactAuthority`, manifest metadata, generated projection tables; no literal rule-name match arms outside `#[cfg(test)]` | `c56bda3f` (CI-enforced static AST scan `no_grammar_name_branch.rs`) + `0ffbd754` (rule-id-keyed kind dispatch + `leak_static_str` delete). 7 `from_rule_name(&str)` impls eliminated; `EmitStrategy::for_grammar` is manifest-driven via `fbbee8c8`. |
| `feedback_no-silent-fallback` | No production code path swallows a malformed substrate path, missing rule, unrecognised parser ident, unknown grammar by routing into a default builder/discriminant/allowlist; failure is `panic!` with named binding string | `fbbee8c8` (substrate.rs panics on invalid binding) + `27592f4e`+`7fdcd803` (lowering panics on unmatched annotations). |
| `feedback_no-orthogonal-codepaths` | Arena allocation singular collection strategy; no conditional Vec-vs-scratch; no combinator fallback; one regex system (HIR); KISS DRY | W2.5 AscentStrategy is a single trait with a default impl chosen by micro-bench (`audit/W2-ascent-microbench.json`); not three parallel ascent paths in production. Path execution is `parse_with` only (W3 future); no shadow path system. |
| `feedback_no-workarounds` | Zero tolerance for workarounds, fallbacks, stubs, or legacy code | `07369e95` (`view/color.rs` 290 LOC + `peel.rs` 42 LOC + `runtime/view.rs:35` re-export deleted; no migration shim). `2b2b221e` (`recover_modifier`/`recover_binary_op` byte-recovery deleted; alt_dispatch typed-leaf push activated). |
| `feedback_no-deferrals` | Never defer optimizations to future tranches; integrate everything into the current pass | AZ-IV.md §Non-Routable Carries declares 13 chronic deferrals are non-routable in AZ-IV; thesis-review triumvirate fires if a carry cannot land without changing the AZ-IV thesis. No successor letter is allowed for these carries. |
| `feedback_substrate-with-consumer` | Every substrate change lands with a same-wave consumer or explicitly declared brittleness window | `138bd1ab` (manifest-strategy scaffold) consumed by `fbbee8c8`. `0ffbd754` (rule-id discriminator) consumed by every per-grammar arena. W2.5 AscentStrategy + micro-bench consumer in same wave. |
| `feedback_directory-modules` | Splits use directory modules (`hir/`), not flat siblings | `crates/core/src/path/{ir,type_check,error,ascent,wildcard,variant_select,mod}.rs` (W2.1+W2.5 layout) is a directory module per W2.md File Bounds. |
| `feedback_clean-regen-discipline` | Generated files always output of fresh regen; never hand-patch | `cb3a40d5` (regen 9/9 mechanical) + `9807343a` (regen 9/9 against W1.9 lifts) + `2e669b58`+`d89a9dd9` (regen against W1-CLOSE.A): every generated commit is mechanical post-source-edit; no hand-patches. |
| `feedback_generated-size-budget` | Generated code has per-tranche line-count budget; overflow blocks wave until O(N) generator regression traced | W0.md §Verification Artefacts §Generated-size budget table; `7959e6cb` records all 9 grammars within ±5 % per file (aggregate -2.10 %). |
| `feedback_aesthetics-critical` | Formatting aesthetics are the purpose of gorgeous/pprint; never use heuristic thresholds over actual configurable values | (out of W0..W2 scope; pprint not touched.) |
| `feedback_inspect-generated-output` | Always inspect expanded/compiled output when working on codegen | W0.3 redress used `dump_ir grammar/bbnf/bbnf.bbnf int_lit --structural` to verify Map wrapper restoration; debug_parse round-trip; regen --check 9/9 byte-identical (per `audit/REGEN-redress.md` §Verification). |
| `feedback_no-warm-benches` | Cold per-parse only | `audit/W2-ascent-microbench.json` uses divan with explicit cold per-iteration; no warm-cache leakage. |
| `feedback_bg-then-monitor` | Any Bash invocation expected to take >60s must set `run_in_background=true` and be followed by Monitor call | (orchestration-only; not a source-code feedback.) |
| `feedback_dispatch_hard_cap` | Every dispatch carries `HARD CAP: N min. At 0.9N commit, at N halt`. Defaults research 20, plan 15, redress 30 | REGEN triumvirate dispatch packets recorded HARD CAP 60 min (doubled from default for joint surface load); W1.9 redress recorded 30→90 min extension under "scope reveal demands" clause; W1-zero redress 60→70 min extension. Each extension logged in halt reports. |
| `feedback_triumvirate-discipline` | Research commits attribution; plan commits plan; only then redress dispatches; never merge roles | REGEN + W1-CLOSE both landed all three lanes with mandatory `## Exact Wave-Amendment Text` section; no role-merging. |
| `feedback_redispatch-empty-return` | Empty sub-agent return is not scope-reveal; redispatch verbatim with prior-worktree pointer | Per W1-CLOSE-plan.md §4: empty-return-rule embedded in every redress dispatch packet. No empty-return triggered redispatch in W0..W2 (per absence of audit doc named `*-empty-return-redispatch.md`). |
| `feedback_no-inline-tests` | All tests in tests/ directory; never inline `#[cfg(test)]` in src/ | `crates/core/tests/no_grammar_name_branch.rs`, `crates/core/tests/synthetic_grammar_strategy.rs`, `crates/core/tests/path_*.rs` — all in `tests/`, not inline. |
| `feedback_pluggable-components` | Decision points must be pluggable (cost model, pattern registry, rewrite rules), not hardcoded branches | `EmitStrategy::for_grammar` post-W1.8 reads `[package.metadata.bbnf-grammars.<ident>]` from manifest, not Rust arm-list. AscentStrategy trait + 3 impls + reversal seam (W2.5). |

All listed feedback entries are honoured in W0..W2 work. No silent
non-compliance surfaces.

---

## Audit verdict

W0+W1+W2 close cleanly with the residual carries enumerated in §5. Triumvirate
process discipline held under three scope-reveal events (REGEN, W1-CLOSE,
W0.3 fourth-surface absorb). The path subsystem (Path IR + lexer + macro + IR
pass + AscentStrategy + Wildcard + Variant-Select) lands as the typed
compile-time substrate the BA pre-recycle scope demanded. The only unfinished
W2 piece is the hand-authored egraph rewrite seed (Hard Gate 9), absorbed into
W3 opening. PROGRESS.md status words match landed reality; cited commit hashes
all resolve; cited audit-doc paths all exist (modulo a missing
`W0-close-ledger.md` whose evidence is distributed across the W0-*.txt set).
Memory-feedback compliance is consistent across the 18 most-relevant entries.

W3 (Lazy Bail-Out Parse) opens against this base. Carries are tractable;
thesis is intact.
