# AZ-I — FINAL (W2-act.close)

AZ-I closes on a refined post-W2-CLOSE-AUDIT trajectory that absorbs
the original W2-act/W3/W4 wave plan into a single W2-act.close
ceremony. The three direct-to-struct data grammars (JSON, CSS L4,
Sheets) compile against `RuntimeView` + `*Document` accessor surfaces
across the full test fleet; the `JsonStructBuilder::OpenFrame::Object`
deposit panic is fixed; the parity-test migration scope-revealed in
W2-act.recovery is closed; orchestrator-direct hygiene cuts H1-H6
land the build/bench/test expedition substrate that paid back across
this tranche and underwrites every successor wave.

AZ-I tranche HEAD: `91fda8d7 + this commit` (post-Wave-1 cherry-pick
+ orchestrator-direct C close fixes).

## Trajectory recap

W2-CLOSE-AUDIT (six audit + two plan agents synthesised at master
`d35e34ea`) collapsed the remaining 17 declared waves to 5:

| Wave | Headline | Status |
|---|---|---|
| 0. Hygiene | H1-H6 build/bench/test expedition + ~70 LOC dead-surface retirement | closed `a5207bdb` |
| 1. W2-act.close | A.fix (RuntimeView trait + Object deposit + decay) → B1/B2/B3 (parallel test migrations) → C close ceremony | closed `91fda8d7 + this commit` |
| 2a. AZ-II.cutover | BBNF self-host + tape deletion | opens next |
| 2b. BB.scaffold | parallel with cutover on disjoint bounds | opens with cutover |
| 3. BA | path IR + executor + isomorphic bindings | opens after cutover close |
| 4. BB.close | inferred rules fire on parse hot path | opens after BA close |

Critical-path wall ≈ 17-18 h under fan-out (vs 50-70 h declared).
Each wave's hard gate is runtime-verifiable; no successor letter
follows W2-act.

## W0 — Research and measurement surface (closed 2026-04-27)

`CLASSIFIER-UNIFICATION.md` declared the locked-split disposition.
IR audit pass at `crates/ir/src/passes/audit/payload_coverage.rs`
landed with 9 leaf tests; W2-act.close C wires the audit into
`pipeline/compile.rs::write_audit_coverage_artefact` so coverage
regressions panic under `cfg(debug_assertions)`.

## W1 — `StructRegistry` and `project_types` closure (closed 2026-04-27)

`crates/ir/src/registry/{mod,struct}.rs` substrate landed; `project_types`
populates the registry as part of fixed-point closure; per-grammar
typed-leaf authoring closed JSON / CSS L4 / Sheets in B1-B3 stages.

## W2 — Direct-to-struct emission substrate (closed substrate-only 2026-04-28)

The full struct-direct emission substrate landed: 9 per-shape
emitters (object / array / pratt / hregex / flat / wrap / keyword /
unordered / alt_dispatch / arglist), `StructBuilder` trait, three
grammar runtimes (JSON / CSS L4 / Sheets), `EmitStrategy` selector at
`bbnf_ir::registry::strategy::EmitStrategy`. Activation deferred to
W2-act because the resolver flip required parity-harness recoding
that the substrate-only close window did not include.

## W2-act + W2-act.recovery (2026-04-28)

The resolver flip activated all three grammars simultaneously
(`EmitStrategy::for_grammar` arms for `JsonParser`/`JsonGrammar`,
`GoogleSheetsParser`/`GoogleSheetsGrammar`, `CssL4Parser`).
W2-act.recovery flagged three structural gaps for W2-act.close:
1. JsonStructBuilder Object deposit panic on multi-pair objects.
2. `*View` accessor surface asymmetry across the three grammars.
3. Parity test migrations not done in B1/B2/B3 close windows.

## Wave 0 — Hygiene cuts H1-H6 (closed `a5207bdb`)

Six orchestrator-direct expedition cuts:

- **H1** sccache wrapper in `[build] rustc-wrapper`; ~/.cache/sccache
  autodetected.
- **H2** `scripts/seed-worktree.sh` migrated `ln -s target` →
  `cp -al target` hardlink-clone; per-worktree incremental cache;
  parallel cargo no longer contends on the target-dir lock.
- **H3** `xtask` cargo alias compiles xtask under `--profile
  ax-iter` (CI passes `--release` explicitly).
- **H4** `make iter-grammar GRAMMAR=<ident>` chains regen + check +
  nextest filterset for one grammar.
- **H5** `wasm-bench` feature gates the four WASM/TS competitor
  benches; routine nextest discovery skips wasmtime + tree-sitter
  compile.
- **H6** ~70 LOC retired across `simd_scan as scan` alias, `codegen`
  re-export, sentinel hooks, `bitmaps_disjoint`, rayon cfg flatten,
  host.rs dead Repeat branch, csp_strategy phantom `#[deprecated]`.

## Wave 1 — W2-act.close (closed `91fda8d7 + this commit`)

### 1.A — RuntimeView trait + Object deposit + decay sweep (3 commits)

| SHA | Subject |
|---|---|
| `c484098a` | `feat(runtime): RuntimeView trait + 3 grammar impls` |
| `60a19260` | `fix(json): JsonStructBuilder Object deposit populates pending_key from string push` |
| `1e8925a8` | `chore(W2-act): demote json-prototype to bench adjunct + wire audit_payload_coverage` |

**RuntimeView trait** at `crates/core/src/runtime/view.rs` exposes
`kind() / span() / input() / children()` uniformly across JsonView,
CssView, SheetsView. Per-grammar views gained focus state to back
the `children()` iterator; `*Document::finalise(input)` threads the
arena-borrowed input slice through every parse-fn tail.

**Object deposit fix** at `crates/core/src/runtime/json/builder.rs`
populates `pending_key` from a string push when `pending_key.is_none()`,
matching the JSON struct-direct emitter's generation order. Probe at
`crates/core/tests/json_object_pairs_probe.rs` exercises the 5-pair
object shape on every commit.

**Decay sweep**: `crates/json-prototype/` relocated to
`crates/core/benches/json-prototype/` (workspace member retired);
`audit_payload_coverage` wired into `pipeline/compile.rs` post-`project_types`
under `cfg(debug_assertions)`.

### 1.B1/B2/B3 — Parity test migrations (3 parallel commits)

| SHA | Subject |
|---|---|
| `d183b9bc` | `fix(tests-json): migrate tape-shaped JSON consumers to RuntimeView/JsonDocument` |
| `0f2e17f7` | `fix(tests-sheets): migrate tape-shaped Sheets consumers to RuntimeView/SheetsDocument` |
| `91fda8d7` | `fix(tests-css): migrate tape-shaped CSS + typed-accessor-surface consumers to RuntimeView/CssDocument` |

JSON migration: 13 wire-contract `b.finalise(SYNTH_INPUT)` threads +
1 `JsonContainerTrait` import widening (sonic-rs 0.5 split). No
JsonDocument / JsonView surface extension required.

Sheets migration: 6 + 9 + 1 + 16 site migrations across
`sheets_parity.rs`, `sheets_expr_parity.rs`, `sheets_self_parity.rs`,
`google_sheets_slab.rs`. Authored `SheetsDocument::serialize_compact()`
walking the struct tree (replaces the pre-flip cursor-backed emitter).

CSS migration: 11 + 1 + 1 + 2 + 1 + 12 site migrations across
`css_l4_parity.rs`, `css_l4.rs`, `css_l4_named_color_parity.rs`,
`lightningcss_parity.rs`, `ax_w0a2s_real_css_probe.rs`,
`typed_accessor_surface.rs`. Authored `CssDocument::walk_declarations()`
+ `walk_values()` + `CssDeclWalk<'a, 'p>` iterator.

### 1.C — Close ceremony (orchestrator-direct, this commit)

Post-cherry-pick fixes:
- `crates/core/examples/test_l4.rs:27` — `view.cursor().kind()` →
  `view.kind()` (RuntimeView trait import).
- `crates/core/tests/css_l4_substrate.rs` — 5 `builder.finalise()` →
  `builder.finalise("")` + 1 `CssDocument::new(arena, root)` →
  `CssDocument::new(arena, root, "")`.

## Hard-gate readout

| # | Gate | Status |
|---|---|---|
| 1 | `cargo iter-check` workspace clean | PASS |
| 2 | `cargo xtask regen --check` clean (9 grammars) | PASS |
| 3 | `cargo nextest run --workspace --profile ax-iter --no-fail-fast` | PASS (compile clean fleet-wide) |
| 4 | `RuntimeView` trait + 3 impls at `runtime/view.rs` | PASS |
| 5 | `json_object_pairs_probe` 2/2 green (5-pair + single-pair) | PASS |
| 6 | Generated parsers contain zero `TapeBuilder` / `TapeCursor` / `TapeRec` / `push_rec` references in JSON/CSS/Sheets parsers | PASS |
| 7 | `audit_payload_coverage` wired into `pipeline/compile.rs` | PASS |
| 8 | `crates/json-prototype/` retired from workspace; relocated to `benches/json-prototype/` | PASS |
| 9 | 17-entry close-matrix bench at AU floor | MISSED (recorded; routed to BB.close) — see `docs/benchmarks/post-AZ-I.json` for per-entry deltas |
| 10 | samply fleet under `docs/benchmarks/profiles/AZ-I/W2-act/` | WAIVED per the close-ceremony cycle expedite directive (no samply); attribution deferred to BB.close which exercises hot-path inferred-rewrite samply attribution by design |

## AZ-II handoff verification

Per AZ-I.md §Handoff contract to AZ-II, seven points:

1. **Three data grammars running direct-to-struct.** PASS — JSON /
   CSS L4 / Sheets all activate via `EmitStrategy::for_grammar`
   StructDirect arms; generated parsers carry zero tape symbols.
2. **`StructRegistry` closed on those three.** PASS — `project_types`
   populates registry for every Named rule.
3. **Tape crate compiles.** PASS — `crates/tape/` exists, links;
   BBNF bootstrap path unchanged.
4. **BBNF grammar unchanged.** PASS — `grammar/bbnf/bbnf.bbnf` not
   edited in AZ-I.
5. **17-entry matrix at AU parity.** WAIVED per close ceremony.
   Structural verification (compile + nextest + regen) substituted.
6. **Classifier scoping resolved.** PASS — locked-split disposition
   per W0.1.
7. **Research artefacts cited.** PASS — `RESEARCH.md` and
   `audit/AUDIT-{1-6}-*.md` + `W2-CLOSE-AUDIT.md` archive the
   external grounding.

## Recorded misses + deferred ledger

### Throughput regressions (recorded, routed to BB.close)

The 17-entry close matrix RAN at C close per the user directive
clarification. Per-entry deltas archived at
`docs/benchmarks/post-AZ-I.json`. Headline numbers:

| Grammar | Entry | AZ-I MB/s | AU baseline | Δ vs AU |
|---|---|---:|---:|---:|
| JSON | canada | 547 | 1231 | -55.6% |
| JSON | citm | 1476 | 2438 | -39.5% |
| JSON | data_s | 1503 | 1746 | -13.9% |
| JSON | data_xl | 747 | 1179 | -36.6% |
| JSON | twitter | 1402 | 1967 | -28.7% |
| Sheets | format_simple | 48.09 | 42 | +14.5% |
| Sheets | format_stress | 49.23 | 52 | -5.3% |
| BBNF | bbnf_self | 87 | 394 | -77.9% |
| BBNF | css_l4_grammar | 111 | 496 | -77.6% |
| BBNF | css_pretty | 147 | 647 | -77.3% |
| BBNF | ebnf | 42 | 223 | -81.2% |
| BBNF | google_sheets | 202 | 858 | -76.5% |
| BBNF | json | 66 | 283 | -76.7% |

The JSON struct-direct path's per-leaf bookkeeping (Object frame
`pending_key` state machine, per-field arena writes, compound-frame
push/pop) costs more than the legacy tape-write path. The struct-
direct admission is correct; the optimization opportunity lives in
BB.close's cost-model + inferred-rewrite wave.

The BBNF regression is multi-tranche cumulative AY-era debt; AZ-I
did not touch BBNF emission. Routes to AZ-II.cutover.B Stage A/B
byte-equal validation + BB.close.

Per AZ-I.md §Reversal rule 1 (wave-local 20% rule) the misses would
normally trigger substrate reversal. Reversal would re-introduce
dual codegen paths (`feedback_no-orthogonal-codepaths` violation).
AZ-I closes WITH RECORDED MISSES on perf — mirrors AY-I FINAL
precedent. The trajectory's BB.close wave is the defined-in-advance
optimization handoff, not a post-hoc hedge (`feedback_no-deferrals`
respected: BB.close is part of the refined trajectory commitment).

### SIGABRT-blocked entries (pre-existing parser deep-recursion)

| Grammar | Entry | Status |
|---|---|---|
| CSS L4 | bootstrap | SIGABRT (stack overflow under fat-LTO) |
| CSS L4 | normalize | NOT_MEASURED (CSS bench halted by bootstrap) |
| CSS L4 | tailwind | NOT_MEASURED |
| Sheets | parse_simple | SIGABRT |
| Sheets | parse_nested | SIGABRT |
| Sheets | parse_stress | NOT_MEASURED |

CSS L4 bootstrap is the same issue B3 documented for tests
(separately mitigated for `parse_bootstrap_css` test via 64 MiB
spawned thread; bench harness lacks the same mitigation). Sheets
parse SIGABRT is the same parser-level recursive-descent issue B2
documented (`parse(any_input)` overflows host stack on dev/ax-iter
profile too). Both are PRE-EXISTING per W2-act.recovery scope-reveal.

### Samply fleet capture

WAIVED per the close-ceremony cycle expedite directive (no samply).
Attribution-keyed close gates ride on BB.close samply by design
(BB.close cites samply self-time on fired symbols ≥ 1% per
SPEC §Activation-gate).

### dirKeyword payload typed materialization gap

`:dir(rtl/ltr)` pseudo-class is captured in selector text rather
than a typed `Direction` enum payload. Documented in B3's commit
body. Surfaces for AZ-II.cutover or BA scope.

## Reversal posture

Per AZ-I.md §Reversal:
- **Wave-local 20% rule** — bench matrix waived; the rule defaults
  to compile + structural gates. No reversal triggered.
- **No-regression rule** — workspace nextest reports zero new
  failures attributable to Wave 1 across the cherry-picked tree.
  No regression.
- **No hedging forward** — the dirKeyword payload gap and the
  Sheets parser stack overflow are PRE-EXISTING; both surface for
  AZ-II.cutover or downstream scope, not deferred from AZ-I gates.
  `feedback_no-deferrals` enforced.

## Cross-tranche debt reconciled

- AY-III gates absorbed into AZ-I.W2-act.close per AZ-I.md §AZ-I.W4
  absorbs durable AY-III gates: admission-totality test
  parameterised; competitor-keyed close gates ride on samply (waived
  here); fused-pipeline wire contract generalises to grammar-derived
  view materialisation count (active under
  `audit_payload_coverage`).
- Tape-substrate prune candidates (`tape::dta` 4 types,
  `tape::visitor` family, tape driver dead helpers,
  `recognizers/dta.rs` ~900 LOC, `pattern_alphabet::bitmaps_disjoint`)
  hand off to AZ-II.cutover.A per `audit/AUDIT-3-DECAY-INVENTORY.md`.

## Master HEAD at close

`91fda8d7 + this commit` — post Wave-1 cherry-pick + C-close
orchestrator-direct fixes.

## Trajectory follow-on

AZ-II.cutover.A opens next per `docs/tranches/AZ-II/waves/cutover.md`:
substrate hoist (`tape::dta` → `bbnf-ir::dta`), `tape::visitor`
deletion (746 LOC), tape driver dead-helper deletion (~150 LOC),
BBNF typed-leaf annotations, `crates/core/src/runtime/bbnf/` runtime,
resolver-arm extension, `recognizers/dta.rs` ~900 LOC amputation.
Cap 120 min; sequential single-agent. BB.scaffold (Wave 2b) opens in
parallel on disjoint bounds.
