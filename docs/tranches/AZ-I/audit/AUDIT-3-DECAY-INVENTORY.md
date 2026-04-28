# AZ-I Close Audit 3 — Decay Inventory

Read-only sweep of dead, deprecated, contrived, shim-like, and legacy
surfaces across `crates/`, `../parse-that/rust/`, and `../pprint/`,
post-AZ-I.W2-substrate close (HEAD `0321c53a`). Every claim cites grep
output. Severity tags: `delete-now` (no live consumer), `migrate-now`
(shim with one consumer to inline), `migrate-soon` (substrate on
sunset path per declared trajectory), `keep-justified` (intentional
defensive code).

The synthesis at `docs/tranches/AZ-I/audit/W2-CLOSE-AUDIT.md` consumes
this ledger.

## 1. Dead-code markers

`#[allow(dead_code)]` and `#[allow(unused...)]` attributes outside
generated code are concentrated, not diffuse — 104 attribute hits
total across `crates/`, of which the bulk live in `crates/core/src/grammar/generated/`
(~78 hits across the eight grammars; the largest is `css_l4.rs` at 33).
The non-generated decay surface is small and surgical:

- **IR-side DTA sentinels** (`crates/ir/src/passes/recognizers/dta.rs:1618`,
  `:1621`, `:1624`) — `_push_fp_sentinel`, `_type_desc_sentinel`,
  `_string_id_sentinel` exist solely to suppress unused-import warnings
  on `PushFingerprint`, `TypeDesc`, `StringId`. Comment concedes "unused-but-
  reserved for AV.4 bridging"; AV closed. Severity: `delete-now`. ~10 LOC.

- **Pattern-alphabet** (`crates/ir/src/passes/recognizers/pattern_alphabet.rs:369`,
  `:383`) — `bitmaps_disjoint` zero non-test consumers; `make_alphabet`'s
  walker-side runtime guard retired with AY.W0 per `tape/src/dta.rs:6`.
  Severity: `delete-now` for `bitmaps_disjoint` (~10), `migrate-soon` for
  `make_alphabet` (~10).

- **Regex codegen LUTs** (`generate/regex/byte_class.rs:133`,
  `last_byte_set.rs:103`, `phf.rs:146 :149 :158`) — emitted as
  `pub(crate) const` into per-grammar code. The `#[allow]` covers
  the proc-macro emission gap. Severity: `keep-justified`.

- **AltDispatch self-test** (`shape_dispatch/alt_dispatch.rs:135`) —
  `const _: fn(ShapeTag) -> bool = ShapeTag::is_classified` static
  assert. Severity: `keep-justified`.

- **Test/bench helpers** — 11 hits sit on shared `parse_hex_color`
  shims and fixtures (`tests/common/mod.rs:4 :7 :10`, parity tests).
  Severity: `keep-justified`. The 1524-line
  `tests/common/css_normalize.rs` is itself a god-module concern
  (see §4).

## 2. Shim / fallback patterns

- **`pub use simd_scan as scan`** (`crates/core/src/runtime/mod.rs:71`) —
  re-export of the entire `simd-scan` crate under the `scan` alias.
  Single consumer: any `runtime::scan::*` reference. Severity:
  `migrate-now` (rewire consumers to `simd_scan::*` or `runtime::simd_scan::*`,
  delete the alias). Reclaim 1 LOC + reduces re-export confusion.

- **`pub use crate::backend::rust as codegen`**
  (`crates/core/src/generate/mod.rs:20`) — alias re-exports `backend::rust`
  module under historical `codegen` name; `mod.rs:21-23` then
  re-export sub-items. Three consumer sites (`rg codegen::` returns
  `mod.rs:21,22,23` only — i.e. all consumers are the alias's own
  re-exports). Severity: `delete-now`. Reclaim ~4 LOC, eliminates a
  dead alias level.

- **`pub use ::bbnf::grammar::generated::css_pretty::CssPrettyParser as CssParser`**
  (`crates/gorgeous/src/css.rs:8`) and `BbnfBootstrap as BbnfParser`
  (`crates/gorgeous/src/bbnf.rs:6`) — uniform `*Parser` surface
  across grammar adapters. Severity: `keep-justified`.

- **TypeVarId alias** (`ir/src/passes/types/constraint/mod.rs:35`) —
  semantic discriminator. Severity: `keep-justified`.

- **`#[cfg(feature = "rayon")]` gates** (`tape/src/psi/stream.rs:10
  :12 :251`) — rayon is `default = ["rayon"]` per
  `tape/Cargo.toml:8`. The disabled branch never triggers in any
  bench/test. Severity: `migrate-now` — flip to non-optional. ~4 LOC.

- **`parser-trace` cfg** (`backend/rust/trace.rs:3 :19 :29 :45`) —
  real diagnostic feature; gate-off correct because trace overhead
  bench-relevant. **`iai` cfg** (`benches/json_callgrind.rs:1`) —
  single rarely-activated bench. Severity: `keep-justified`.

## 3. Stub / placeholder arms

- **`unimplemented!("prettify not supported")` × 15**
  (`crates/core/src/backend/emitter.rs:456 :459 :462 :465 :468 :471
  :474 :477 :480 :483 :486 :497 :509 :512 :515`) — the abstract
  `Emitter` trait declares 15 prettify-specific default methods that
  panic. The `RustEmitter` overrides all of them; non-Rust backends
  (TS, WASM stub) inherit the `unimplemented!`. Severity:
  `migrate-now` — split the trait into `Emitter` + `PrettifyEmitter`
  (per §1 backend-agnostic types and `feedback_no_workarounds`).
  Reclaim ~60 LOC, eliminates 15 dead-default impls.

- **W2.RE codegen-time `panic!("AZ-I.W2.RE: HRegex shape does not
  support StructDirect; …")` × 4**
  (`crates/core/src/backend/rust/emitter/shapes/hregex.rs:285 :446
  :579 :718`) — the AZ-I.W2 plan declares this an intentional
  unworkability gate ("W3 / W2.B expected to extend …"). Per
  `feedback_execute_planned_architecture`, the post-W2-substrate
  redress folded W2-act + W2.B + W3 into one gestalt activation pass
  (per `REMAINING-TRAJECTORY.md:13-18`). The four panics expire when
  that gestalt pass lands. Severity: `migrate-now` (block AZ-I close
  on activation). Reclaim ~25 LOC of panic + repeat ceremony bodies.

- **Other stub macros** — zero `todo!()` / non-prettify
  `unimplemented!()` hits in `crates/`. Stub burden contained.

- **`unreachable!()`** — 18 hits, mostly defensible exhaustive arms
  in generated code (`generated/json.rs:1364 :2545`) and benches.
  `backend/driver/alt.rs:146 :150` are dispatchable-shape-asserted
  upstream. Severity: `keep-justified`.

## 4. God modules / kitchen sinks

The `helpers.rs` / `common.rs` / `misc.rs` taxonomy is mostly
absent — `find -name "{helpers,utils,common,misc}.rs"` returns only
`crates/core/tests/common/` and `crates/core/benches/common/`,
both standard test-fixture directories.

The size-floor candidates (non-generated > 1000 lines):

- **`crates/ir/src/passes/recognizers/dta.rs`** — 1625 lines. Mixes
  `DtaState` (167–311), `DtaTable` (376–435), `lift_dta` (444–1556),
  `summarise` (1557–1591), `DtaProfile` (1592–1612), and three
  sentinel hooks. Per `tape/src/dta.rs:23-26`'s dictum the runtime
  consumers were retired at AY.W0; only `lift_dta` survives as a
  build-time pass producing the DfaCodegen pattern set
  (`grammar.rs:931`). Severity: `migrate-soon` — file is
  >50% kernel-dead per its own historical comment. Reclaim ~900 LOC
  by amputating `DtaState` walking + `DtaProfile` + `summarise`.

- **`crates/csp-solver/tests/solver.rs`** — 1559 lines. One test file,
  ~40 fixtures. Severity: `keep-justified`.

- **`crates/core/tests/common/css_normalize.rs`** — 1524 lines, single
  shared helper module loaded as `mod common;` from CSS L4 parity
  tests (`crates/core/tests/css_l4_parity.rs`, etc.). Severity:
  `migrate-soon` — split by normalization concern (color, dimension,
  selector) per `feedback_no-god-modules`.

- **`crates/lsp/tests/integration.rs`** — 1468 lines, single
  integration suite; well-factored fixtures. Severity:
  `keep-justified`.

- **`crates/core/src/backend/rust/emitter/grammar.rs`** — 1432 lines.
  The orchestrator file for `RustEmitter::emit_grammar` (line 1078+)
  + DTA-walker substrate + keyword-PHF emission. Severity:
  `migrate-soon` — the `emit_grammar` body and the `dta_walker_table`
  prep are independent concerns; split per `feedback_module-structure-codegen`.

- **`crates/ir/tests/shape_dispatch.rs`** (1404 lines) and
  **`crates/ir/tests/structural_alphabet_extended.rs`** (1378 lines) —
  test catalogues. Severity: `keep-justified`.

- **`crates/ir/src/passes/csp_strategy/mod.rs`** (1273 lines) —
  CSP-strategy entry, well-factored docstrings + sub-modules.
  Severity: `keep-justified`.

- **`crates/core/src/backend/rust/view/value.rs`** (1272 lines) —
  Value-API view emitter. Severity: `migrate-soon`, splitting by
  shape (Object, Array, String, Number, Bool, Null) restores the
  per-shape boundary used in sibling `emitter/shapes/`.

## 5. Multi-pass tranche residue

The tranche directory layout under `docs/tranches/`:

- **AY-I/** + **AY-II-I/** + **AY-III/** are the AY-pass cluster.
  The naming is irregular: `AY-II-I/` reads as "AY pass II sub-pass
  I", but no `AY-II/` plain directory exists. Cross-tranche references
  cite `docs/tranches/AY-II/AY-II.md` (60 file:line citations across
  `docs/tranches/B*` and `docs/tranches/meta-audit/`; e.g.
  `B4/FINAL.md:137-139`, `B5/AGENT_DISPATCH.md:318`,
  `AY-I/FINAL.md:8`). Severity: `migrate-now` — either rename
  `AY-II-I/` → `AY-II/` (cleaner) or update the 60 cross-references.
  Audit synthesis target.

- **AZ-I/** + **AZ-II/** are the active pass cluster. `AZ-II/`
  exists with plan + RESEARCH + waves. No mismatch.

- **REMAINING-TRAJECTORY.md** (509 lines) at the tranche-dir root
  records the path forward through `B5 → B6 → B7 → AZ-I → AZ-II →
  BA → BB`. Severity: `keep-justified`, this is the canonical
  scheduling artefact.

- **next-tranche-research/**, **meta-audit/**, **W/X/Y/Z/** — the
  single-letter dirs (`W`, `X`, `Y`, `Z`) live alongside lettered
  tranches; treat as legacy naming. Severity: `migrate-soon` —
  rename or absorb into the AY pass cluster they predate.

## 6. Disused public surface

Tape driver helpers (`crates/tape/src/driver.rs:79 :95 :116 :130
:147 :181 :208 :238 :270`) — nine `pub fn` items
(`trim_ascii_ws`, `trim_with_pattern`, `first_ws_pattern`,
`saturating_u16`, `emit_leaf`, `emit_leaf_with_payload`,
`emit_reducer_compound`, `lookup_precedence`, `close_compound`)
re-exported via `tape/src/lib.rs:90-92`. Verification:
`rg -n '\btape::emit_reducer_compound\(|\btape::lookup_precedence\(|\btape::first_ws_pattern\(' crates/`
returns zero non-docstring hits across the workspace. The functions
are mentioned only in `crates/core/src/grammar/generated/css_l4.rs`
docstrings ("`emit_reducer_compound` — see crate::runtime::tape::emit_reducer_compound`")
and in the lib.rs re-export.
Severity: `delete-now`. Reclaim ~150 LOC + simplifies tape's public
contract. Per `tape/src/lib.rs:90-92` the re-export comment claims
"shape emitters are the sole consumers" — they are not.

`json-prototype` crate (`crates/json-prototype/`, 2217 LOC) — one
consumer outside its own bench: `crates/core/benches/json/value.rs:57`
(`use json_prototype::{self as proto, parse_json}`). The crate
exists per its own docstring as the AW-V.W2 "speed-ceiling validation
against sonic-rs"; AW closed long ago. Severity: `migrate-soon` —
fold into `crates/core/benches/json/` as a bench-private module
or retire entirely. Reclaim 2217 LOC.

`AbsentRegistryProbe` (`crates/ir/src/passes/audit/payload_coverage.rs`,
referenced in `crates/ir/tests/payload_coverage_audit.rs`,
`crates/ir/tests/struct_registry.rs`,
`crates/core/tests/project_types_{json,sheets,css_l4}.rs`) — the
two-implementation `StructRegistryProbe` design (`AbsentRegistryProbe`
for W0 baseline, `PayloadLayoutsProbe` for present-day) survives in
the trait surface because the W1 closure has not landed. Severity:
`keep-justified` until W1.

## 7. Deprecated / contrived patterns

- **Phantom `#[deprecated]` alias** —
  `crates/ir/src/passes/csp_strategy/mod.rs:86` claims
  `solve_strategy_and_materialization` is "kept as a `#[deprecated]`
  alias for one migration window". `rg '#\[deprecated' crates/`
  returns ONE hit — that very docstring. The alias function does not
  exist. Severity: `delete-now` for the docstring (3 LOC) — pure
  meta-comment without running code.

- **`audit_payload_coverage<P: StructRegistryProbe>` signature**
  (`crates/ir/src/passes/audit/payload_coverage.rs:394`) — three
  args (`ir`, `tag`, `probe`); the prompt's "4-arg borderline" note
  is over-counted. The signature is clean; trait-bounds are minimal.
  Severity: `keep-justified`.

- **TODO-postAW marker** (`crates/core/src/grammar/host.rs:194`) —
  "TODO-postAW: collapse these structural-invisible Seqs at the
  lifter (or mark them transparent) so consumers don't re-peel." AW
  closed; the TODO is obsolete. The branch the TODO targets
  describes a "structurally dead" `TapeKind::Repeat` arm at line
  204+: "this branch is effectively dead, but we keep it so the
  walker tolerates shape shifts after regen." Severity: `migrate-now`
  — delete the dead branch + retire the TODO. Reclaim ~25 LOC.

- **`#![allow(dead_code)]` on `crates/core/src/css_types.rs:18`,
  `crates/core/src/grammar/schema/emit/rust/shared.rs:9`,
  `crates/core/tests/common/css_types.rs:11`,
  `crates/core/benches/common/timeout.rs:46`,
  `crates/core/benches/common/validate.rs:7`** — module-level
  blanket attributes. `css_types.rs` is used by emitted generated
  code (`generated/css_l4.rs:9017`) where the warn-by-test
  compilation cannot see the cross-module use. `shared.rs` and
  `tests/common/css_types.rs` are similar shared-helper modules.
  Severity: `keep-justified`, but worth re-classifying as
  `pub(crate)` to avoid the attribute.

- **`_`-prefixed probe fns** — `_force_imports`
  (`egraph/tests/csp_scheduler.rs:255`), `_fixtures_type_touch`
  (`tests/shape_dispatch_emission.rs:493`), six `_require_view_types`
  (`tests/typed_accessor_surface.rs:511 :534 :552 :571 :601 :622`) —
  type-system probes. Severity: `keep-justified`.

## 8. Legacy substrate

- **`crates/tape/`** — the AZ-II.W2 trajectory absorbs the substrate.
  `crates/tape/src/dta.rs:1-26` self-documents that "post-AY.W0 the
  runtime DTA driver is retired; the per-grammar emitter inlines its
  dispatch directly." Five remaining structs (`DtaStateId`,
  `DtaRuleId`, `DtaAssociativity`, `DtaPrecedenceEntry`,
  `DtaError`) are emitter-consumed (`grammar.rs:931`,
  `crates/core/src/grammar/generated/css_l4.rs:1151+`). The carry-
  forward is justified until AZ-II.W2 retires the precedence emission
  surface. Severity: `migrate-soon`.

- **IR `dta.rs` carry-forward** — the 1625-line file (see §4) is
  itself a sunset substrate. The ~900 LOC of `DtaState`, `DtaTable`,
  `DtaBuilder`, `DtaProfile`, `summarise` survive only to provide
  `lift_dta(ir) → DtaTable` to two consumers (`grammar.rs:931`
  + `cost_grid_sweep.rs:90`). Both consume only the pattern-set
  enumeration that walks the IR — a far smaller surface than
  `DtaTable` exports. Severity: `migrate-soon` — replace `lift_dta →
  DtaTable` with a `collect_pattern_set(ir) → Vec<PatternRef>`
  helper and amputate the rest.

- **`crates/json-prototype/`** — see §6. Severity: `migrate-soon`.

- **`crates/csp-solver/tests/optimize.rs`** — flagged by the
  `_legacy|_v1|_v2|_old|_deprecated` filename grep. Re-inspection:
  the file's contents do not carry a legacy concern; the filename
  hit was on the underscore-bearing `optimize` token. Severity:
  `keep-justified` — false positive.

- **`crates/ir/src/passes/recognizers/node_facts.rs`** — same
  filename-grep false positive. Severity: `keep-justified`.

## Prioritized cleanup ledger

Top-15 items by `LOC × architectural-clarity-gain`:

| # | Item | File:line | Severity | LOC |
|---|------|-----------|----------|-----|
| 1 | IR `dta.rs` amputation: retire `DtaState`/`DtaTable`/`DtaBuilder`/`DtaProfile`/`summarise`; expose `collect_pattern_set` | `crates/ir/src/passes/recognizers/dta.rs:95-1612` | migrate-soon | ~900 |
| 2 | `json-prototype/` crate retirement; fold into `crates/core/benches/json/` | `crates/json-prototype/` | migrate-soon | ~2200 |
| 3 | `crates/core/tests/common/css_normalize.rs` split by concern | `crates/core/tests/common/css_normalize.rs` | migrate-soon | ~1500 |
| 4 | `crates/core/src/backend/rust/emitter/grammar.rs` split: `emit_grammar` orchestrator vs DTA-walker prep | `crates/core/src/backend/rust/emitter/grammar.rs` | migrate-soon | ~600 |
| 5 | `crates/core/src/backend/rust/view/value.rs` split by Value shape | `crates/core/src/backend/rust/view/value.rs` | migrate-soon | ~600 |
| 6 | Tape driver dead helpers (`emit_leaf`, `emit_reducer_compound`, `lookup_precedence`, etc.) deletion | `crates/tape/src/driver.rs:79-281` | delete-now | ~150 |
| 7 | Prettify-trait-default split: separate `Emitter` from `PrettifyEmitter` | `crates/core/src/backend/emitter.rs:455-516` | migrate-now | ~60 |
| 8 | W2.RE `panic!` retirement (gestalt activation pass) | `crates/core/src/backend/rust/emitter/shapes/hregex.rs:285 :446 :579 :718` | migrate-now | ~25 |
| 9 | `host.rs` TODO-postAW + dead `TapeKind::Repeat` branch retire | `crates/core/src/grammar/host.rs:194-220` | migrate-now | ~25 |
| 10 | IR `dta.rs` sentinel hooks (3 sentinel fns + their imports) | `crates/ir/src/passes/recognizers/dta.rs:1618-1626` | delete-now | ~10 |
| 11 | `pattern_alphabet.rs` `bitmaps_disjoint` + `make_alphabet` audit | `crates/ir/src/passes/recognizers/pattern_alphabet.rs:369 :383` | migrate-soon | ~20 |
| 12 | `tape/src/psi/stream.rs` rayon cfg-gate flatten (default-on) | `crates/tape/src/psi/stream.rs:10-12 :251` | migrate-now | ~5 |
| 13 | `pub use ::backend::rust as codegen` alias + 3 children retire | `crates/core/src/generate/mod.rs:20-23` | delete-now | ~4 |
| 14 | `pub use simd_scan as scan` alias retire | `crates/core/src/runtime/mod.rs:71` | migrate-now | ~1 |
| 15 | Phantom `#[deprecated]` alias docstring retire | `crates/ir/src/passes/csp_strategy/mod.rs:86` | delete-now | ~3 |

Aggregate reclaim: ~6100 LOC across 15 items. Items 1 and 2 alone
represent ~3100 LOC of substrate sunset already declared in
`tape/src/dta.rs:23-26` and `crates/json-prototype/Cargo.toml:6`.
Items 7 and 8 unblock the AZ-I close gestalt-activation pass. Items
3–5 and 11 dissolve god-module residue per the `feedback_no-god-modules`
edict. Items 6, 10, 13–15 are pure orphan-surface deletes verified
zero-consumer by grep.

The decay surface is moderate, not catastrophic. Items 1+6 sit at
the architectural-clarity centre: amputating the IR-side `dta.rs`
substrate (item 1) and tape-side dead drivers (item 6) closes the
post-AY.W0 lift-DTA-table sunset declared in
`crates/tape/src/dta.rs:1-26`. Item 2 retires a peer crate whose
mission ("AW-V.W2 speed-ceiling validation against sonic-rs") closed
multiple tranches ago. The rest are mechanical sweeps that compose
naturally with the AZ-II.W2 plan.
