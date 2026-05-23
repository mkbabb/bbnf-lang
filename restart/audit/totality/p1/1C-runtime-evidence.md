---
agent: 1C
pass: T-P1-excavation
cycle: V6
generated_at: 2026-05-23T21:08:12Z
spec_surfaces_audited: [ARCHITECTURE.md, LOCKS.md, PASS-1-EXCAVATION.md, T-P1-DISPATCH-CONTEXT.md]
files_audited_count: 75
live_truth_method: "find + wc -l + grep at HEAD on /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/ and /Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/; counts re-derived per call; SK-V14 S-P0 A6 Pattern H baseline (67 main + 48 skinny) reproduced exactly"
prior_cycle_dispositions_folded:
  accepted:
    - V4 CH1: spec-claim ↔ implementation rows resolve at the cited path:line; SinkOnly/OffsetTape JSON-scoped framing.
    - V4 CH2: generic crate Lock 14 root leak audit and generated-vs-handwritten split.
    - V4 CH3: SinkOnly classification with CSS L4 same-plane fact-stream admission preserved.
    - V4 CH4: cost/wave metadata inlined per divergence.
    - V4 CH5: CSS fact-stream outlier classification; Track 2 helper coupling not mistaken for parallel substrate.
    - V4 CH6: UNKNOWN rows carry verify actions.
  rejected: []
  revised:
    - Pivot the scope from skinny prototype (3 grammar dirs) to the live main workspace at HEAD (9 grammar dirs, 67 hand-written files) per the SK-V14 T-P1 dispatch binding; the skinny corpus remains the empirical floor 1D digests, while 1C now catalogues HEAD against ARCHITECTURE §9 + §4.3 + Lock 14.
    - Replace "shape consumption" inventory with the harder finding that all four spec shape names (SinkOnly / OffsetTape / EventTape / CollapsedStage) and BackendShape are entirely ABSENT from crates/ — the live runtime has collapsed onto a single StructBuilder trait + per-grammar *StructBuilder impl path that the comments explicitly mark as "tape substrate severed".
    - Update the Lock 14 leak audit to the workspace surface: runtime/mod.rs hand-wires 9 grammar modules + 127 grammar-named type reexports; verification cmd `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` returns 9 (Lock 14 requires 0).
    - Preserve the V4 admission that CSS L4 declaration-values fact-stream remains row evidence; flag that the skinny CSS L4 7-tranche cluster has no main-workspace equivalent (extended/at-rules/nested/visual/vendor/stylesheet/declaration-values), so the V1 CSS runtime is the monolithic css_l4/ + sibling css_pretty/ pair, not the skinny modular cluster.
    - V1-fold (CH2 GENERALITY): replace the "19+ matches" floor with the live re-derived count **30 matches across 15 files** (prior enumeration missed the entire google_sheets/ sub-tree including `google_sheets/document/mod.rs:43,142`); replace the "60+ grammar-named types" floor with the mechanical count **127 distinct grammar-named symbols** at `mod.rs:25-71` (47 lines holding the surface; 133 raw `pub use` entries inside the window minus the 6 in-window grammar-neutral exports `StructBuilder` `:33`, `GenericAtRule` `:42`, `DtaError`+`ParseErr` `:58`, `CompoundHandle`+`StringHandle` `:63`; the 4 out-window neutrals `IntoPathSegment`+`Path`+`PathSegment` `:72` + `RuntimeView` `:76` are excluded per NEW-CH2-V2-03 discipline); rescale the LOC-delta-to-repair band for the reexport block proportionally to the 127-symbol consumer rewire.
    - V2-fold (F-V3-CH2-1 off-by-one repair): the V2 cycle cited "126" via "subtract 10 grammar-neutral exports", but 4 of those 10 (`IntoPathSegment, Path, PathSegment` at `mod.rs:72` + `RuntimeView` at `mod.rs:76`) sit OUTSIDE the 25-71 window. Correct count subtracting only the 6 in-window neutrals is **127**, not 126. Per-grammar css_l4 = **43** (44 raw minus the in-window neutral `GenericAtRule`), not "~41" or "~38". Institutionalises NEW-CH2-V2-03: every "N grammar-named X" subtract-from-K cite must enumerate the K neutrals with `path:line` inside the cited window.
  first_cycle_additions:
    - 1C-D8: spec template names {generated.rs, visitor.rs, host.rs} are absent from every workspace per-grammar dir; the live file roster is {mod, arena, builder, document, value, view} ± {parse_with, kind, serialize, document/{mod,canonical,view,path_query}}.
    - 1C-D9: per-grammar dirs sit at crates/core/src/runtime/<grammar>/ (flat), NOT at runtime/src/grammars/<grammar>/ as ARCHITECTURE §9 line 1648-1656 specifies; literal path "runtime/grammars" never appears in crates/.
    - 1C-D10: Lock 14 root verification cmd `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` returns 9 (json/css_l4/css_pretty/bbnf/bnf/ebnf/math/csv/google_sheets); Lock 14:220 requires it to return ZERO.
    - 1C-D11: zero `@generated` / `AUTO-GENERATED` / `THIS FILE IS GENERATED` markers across the 67 per-grammar files at HEAD; every file carries hand-authored module docs ("AZ-I.W2.A — JSON struct-direct runtime", "AZ-IV.W5.3 — CssPrettyStructBuilder thin instantiation", etc.).
divergence_count:
  spec_claims_implemented: 2
  spec_claims_unimplemented: 9
  impl_exceeds_spec: 1
  unknown: 3
locks_amendment_candidates: 0
---

# Executive Summary

The live runtime at HEAD diverges materially and at-scale from `ARCHITECTURE.md` §9 + §4.3 and from Lock 14. The spec promises a generated per-grammar template under `runtime/src/grammars/<g>/` consisting of `{mod, generated, view, value, visitor, host}.rs`, lowering from a five-shape canon (`SinkOnly`/`OffsetTape`/`EventTape`/`CollapsedStage`/`DocumentView`) selected by `LayoutFacts.backend_shape`. Reality at HEAD: 9 hand-written per-grammar dirs sit FLAT at `crates/core/src/runtime/<g>/` (not under a `grammars/` parent), totaling 67 hand-authored .rs files with zero generation markers; the file roster is `{mod, arena, builder, document, value, view} ± {parse_with, kind, serialize}`, NOT the spec template; every spec shape term and `BackendShape` itself is absent from the entire `crates/` tree; runtime instead collapses onto a single `StructBuilder` trait with per-grammar `*StructBuilder` impls whose mod-docs explicitly say the tape substrate is severed. Runtime root `mod.rs` hand-wires 9 `pub mod <g>;` declarations + **126 grammar-named type reexports** across 47 lines at `mod.rs:25-71` (`JsonValue`, `CssRule`, `BbnfArena`, …) — a direct violation of Lock 14's "ZERO grammar-named modules in generic crates" verification command (which presently returns 9, not 0). The per-grammar file census of 67 main + 48 skinny reproduces the SK-V14 S-P0 A6 Pattern H baseline exactly: bbnf 8 / bnf 7 / css_l4 7 / css_pretty 7 / csv 7 / ebnf 7 / google_sheets 10 / json 7 / math 7 = 67.

# Pattern H — Per-Grammar Runtime Module Census At HEAD

Verification: `find /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/<g> -type f -name '*.rs' | wc -l` per grammar; sum 67.

| Grammar | File count | File roster (basenames) | LOC sum | Hand-written / generated | Spec template match? |
|---|---:|---|---:|---|---|
| `bbnf` | 8 | mod, arena, builder, document, parse_with, serialize, value, view | 2,026 | hand-written; zero gen markers; mod.rs:1 tags "AZ-I.W2.A — BBNF self-serializing runtime"-class docs | NO — spec wants {mod, generated, view, value, visitor, host}; live has {arena, builder, document, parse_with, serialize}; missing {generated, visitor, host} |
| `bnf` | 7 | mod, arena, builder, document, kind, value, view | 438 | hand-written | NO — same template miss |
| `css_l4` | 7 | mod, arena, builder, document, parse_with, value, view | 3,126 | hand-written; builder.rs:1 "AZ-I.W2-act.B3 — CssStructBuilder" cutover-tagged | NO — same template miss |
| `css_pretty` | 7 | mod, arena, builder, document, kind, value, view | 455 | hand-written; builder.rs:1 "AZ-IV.W5.3 — CssPrettyStructBuilder thin instantiation" | NO — same template miss |
| `csv` | 7 | mod, arena, builder, document, kind, value, view | 598 | hand-written | NO — same template miss |
| `ebnf` | 7 | mod, arena, builder, document, kind, value, view | 445 | hand-written | NO — same template miss |
| `google_sheets` | 10 | mod, arena, builder, parse_with, value, view + document/{mod, canonical, view, path_query} | 1,933 | hand-written; sub-module split inside document/ | NO — same template miss; document/ split is impl-exceeds-spec |
| `json` | 7 | mod, arena, builder, document, parse_with, value, view | 1,427 | hand-written; mod.rs:7 "tape substrate is severed on the JSON parse path" | NO — same template miss |
| `math` | 7 | mod, arena, builder, document, kind, value, view | 467 | hand-written | NO — same template miss |
| **TOTAL** | **67** | — | **10,915** | **67 / 0** | **0 / 9 grammars match spec template** |

Asymmetries:
- `parse_with.rs` exists in 4 of 9 grammars (bbnf, css_l4, google_sheets, json) — the "first-class path-cursor" parsers that wire `crate::grammar::generated::<g>::<G>Parser::parse` through a `PathCursor` shim.
- `kind.rs` exists in 5 of 9 grammars (bnf, css_pretty, csv, ebnf, math) — the "tape-direct" grammars that use the shared `SimpleStructBuilder` template.
- `google_sheets/document/` is the only nested sub-module within a grammar dir — 4 files (mod, canonical, view, path_query) under `document/`, raising google_sheets to 10 files.
- `bbnf/serialize.rs` is unique — bbnf is the only grammar with a self-serializer (BBNF must round-trip grammar source).
- `kind.rs` and `parse_with.rs` are mutually exclusive across the 9 grammars.

# Skinny Mirror — Pattern H Reproduction At HEAD

Verification: `find /Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/<g> -type f -name '*.rs' | wc -l`; sum 48.

| Grammar dir | File count | File roster |
|---|---:|---|
| `css_l4_at_rules_and_media` | 5 | mod, config, generated, parser, sink |
| `css_l4_declaration_values` | 5 | mod, config, generated, parser, sink |
| `css_l4_declaration_values_extended` | 5 | mod, config, generated, parser, sink |
| `css_l4_nested_layout` | 5 | mod, config, generated, parser, sink |
| `css_l4_stylesheet_selectors` | 5 | mod, config, generated, parser, sink |
| `css_l4_vendor_and_custom_atrules` | 5 | mod, config, generated, parser, sink |
| `css_l4_visual_functions` | 5 | mod, config, generated, parser, sink |
| `json` | 11 | mod, config, event_grammar_witness, generated, host, parser, scan, sink, value, view, visitor |
| `sheets_witness` | 2 | mod, event_grammar_witness |
| **TOTAL** | **48** | — |

Skinny CSS L4 cluster has 7 sub-grammars (35 files), matching the modular `grammar/css/l4/*.bbnf` decomposition; the MAIN workspace has none of these — only monolithic `css_l4/` (7 files) + sibling `css_pretty/` (7 files). Skinny JSON DOES emit the spec template names (`generated.rs`, `host.rs`, `view.rs`, `value.rs`, `visitor.rs`) plus hand-written companions (`scan.rs`, `sink.rs`, `parser.rs`); main workspace JSON does not.

# Spec-Claim ↔ Implementation Table

| Spec claim | Implementation evidence | Verdict | Note |
|---|---|---|---|
| Per-grammar runtime lives at `runtime/src/grammars/<grammar>/` with template `{mod, generated, view, value, visitor, host}.rs` and is template-emitted, not hand-written (`restart/ARCHITECTURE.md:1646-1660`). | Live dirs are FLAT at `crates/core/src/runtime/<grammar>/`; file roster is `{mod, arena, builder, document, value, view} ± {parse_with, kind, serialize}` — `generated.rs`, `visitor.rs`, `host.rs` exist in **zero** grammars; `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` returns 9 per-grammar dirs (path:line: `crates/core/src/runtime/mod.rs:8-23`). | Unimplemented | Both the path layout AND the file template diverge. The literal substring "runtime/grammars" or "runtime::grammars" does not appear in any file under `crates/`. |
| Generated per-grammar runtime modules `(value, document, view, kind)` are emitted from a single grammar-agnostic generator template; hand-written per-grammar runtime files are FORBIDDEN (Lock 14, `restart/locks/LOCKS.md:220`). | 67 / 67 per-grammar files at HEAD are hand-written; zero `@generated` / `AUTO-GENERATED` / `THIS FILE IS GENERATED` markers; mod-docs are hand-authored prose ("AZ-I.W2.A — JSON struct-direct runtime", `crates/core/src/runtime/json/mod.rs:1`). | Unimplemented | Outright violation, at full grammar coverage. |
| Verification cmd `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` must return ZERO per-grammar dirs (Lock 14, `restart/locks/LOCKS.md:220`). | Cmd returns 9 (bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math). | Unimplemented | Direct rebuttal of the lock's stated verification predicate. |
| Generic crates carry ZERO grammar-named modules; ZERO grammar-specific types in their public APIs (Lock 14, `restart/locks/LOCKS.md:220`). | `crates/core/src/runtime/mod.rs:9-21` declares `pub mod bbnf; pub mod bnf; pub mod css_l4; pub mod css_pretty; pub mod csv; pub mod ebnf; pub mod google_sheets; pub mod json; pub mod math;`. `mod.rs:25-71` reexports **127 distinct grammar-named symbols** (`JsonValue`, `JsonArena`, `CssRule`, `BbnfArena`, `SheetsValue`, `CsvValue`, …); mechanical extraction across 47 lines: 133 raw `pub use` entries inside the cited 25-71 window minus the **6** grammar-neutral exports present inside the window — `StructBuilder` (`mod.rs:33`), `GenericAtRule` (`mod.rs:42`), `DtaError` (`mod.rs:58`), `ParseErr` (`mod.rs:58`), `CompoundHandle` (`mod.rs:63`), `StringHandle` (`mod.rs:63`) — yields 127. The remaining 4 grammar-neutral exports — `IntoPathSegment`, `Path`, `PathSegment` (`mod.rs:72`) + `RuntimeView` (`mod.rs:76`) — sit OUTSIDE the cited 25-71 window and are excluded from this subtraction per NEW-CH2-V2-03 discipline. Per-grammar breakdown (in-window only): bbnf 10, bnf 10, css_l4 **43** (44 raw - 1 in-window neutral `GenericAtRule`; the 3 css_l4-named aliases `CssRule, CssDeclaration, CssSelector` at `mod.rs:34-35` are counted inside the 43), css_pretty 10, csv 10, ebnf 10, google_sheets 11, json 13, math 10. | Unimplemented | The generic `runtime` crate's root is the leak surface. |
| Generic-crate scan `rg -n 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/core/src/runtime/` must return ZERO (Lock 14, `restart/locks/LOCKS.md:220`). | Cmd returns **30 matches across 15 files** at HEAD (2026-05-23): `bbnf/document.rs` (1), `bbnf/mod.rs` (1), `bbnf/parse_with.rs` (4), `bbnf/serialize.rs` (1), `css_l4/document.rs` (1), `css_l4/mod.rs` (1), `css_l4/parse_with.rs` (4), `google_sheets/document/canonical.rs` (1), `google_sheets/document/mod.rs` (2 — sites `:43` + `:142`, both previously unenumerated), `google_sheets/mod.rs` (2), `google_sheets/parse_with.rs` (4), `json/builder.rs` (1), `json/document.rs` (2), `json/mod.rs` (1), `json/parse_with.rs` (4). Representative anchors: `crates/core/src/runtime/json/parse_with.rs:47,90,93`; `crates/core/src/runtime/bbnf/parse_with.rs:21,61,64`; `crates/core/src/runtime/css_l4/parse_with.rs:22,63`; `crates/core/src/runtime/google_sheets/parse_with.rs:22,62,65`; `crates/core/src/runtime/google_sheets/document/mod.rs:43,142`. | Unimplemented | Each `parse_with.rs` literally names `parse_<G>Parser_<rule>` and `__shape_support_<G>Parser` symbols; the four google_sheets sites (the entire `google_sheets/` sub-tree) were absent from prior cycle's enumeration. |
| `OffsetTape` lowers as an `EventCursor` over retained structural offsets (`restart/ARCHITECTURE.md:1610-1618`). | Substring `OffsetTape` does not appear anywhere under `crates/`; the term, the shape, and any consumer are absent. | Unimplemented | Entire shape canon missing from the main workspace. |
| `SinkOnly` lowers to direct typed-field writes with no retained queryable document (`restart/ARCHITECTURE.md:1606-1616`). | Substring `SinkOnly` does not appear anywhere under `crates/`; the term, the shape, and any consumer are absent. The workspace has collapsed onto `StructBuilder` + per-grammar `*StructBuilder` impls (`crates/core/src/runtime/builder.rs:66`, `crates/core/src/runtime/csv/builder.rs:44`). | Unimplemented | The SinkOnly NAME is gone; whether `StructBuilder` is a renamed-SinkOnly equivalent is U1 below. |
| `EventTape` lowers through compact event cells with payload/recovery/layout facts (`restart/ARCHITECTURE.md:1610-1618`). | Substring `EventTape` does not appear under `crates/`. | Unimplemented | Same shape-canon erasure. |
| `CollapsedStage` is the fourth runtime materialization branch (`restart/ARCHITECTURE.md:1616`). | Substring `CollapsedStage` does not appear under `crates/`. | Unimplemented | Same shape-canon erasure. |
| `LayoutFacts.backend_shape` selects `SinkOnly` for parse-only public APIs (`restart/ARCHITECTURE.md:1589-1591`). | Substring `BackendShape` does not appear under `crates/`; no facts-driven shape selection exists in runtime. | Unimplemented | Decision-engine wiring is absent. |
| `DocumentView` is the typed projection over sealed tape; direct views can point into tape (`restart/ARCHITECTURE.md:1574, §9.2`). | Per-grammar `<G>View` types exist (`crates/core/src/runtime/csv/view.rs:CsvView`, `crates/core/src/runtime/json/document.rs:JsonView`, …) — but they project the `StructBuilder`-built compound arena, not a tape. The shared `RuntimeView` trait at `crates/core/src/runtime/view.rs:1-76` walks `kind / span / input / children`, with no tape coupling. | Impl-exceeds-spec / diverged | The view layer is implemented (in a grammar-named-type fashion) but the substrate beneath it is not the tape ∪ direct-to-struct union the spec promises — it is direct-to-struct only. |
| Runtime materialization pipeline: `byte input -> mask stream -> typed event cursor -> { OffsetTape \| EventTape \| SinkOnly \| CollapsedStage } -> DocumentView / direct typed output` (`restart/ARCHITECTURE.md:1612-1618`). | Live pipeline: `byte input -> generated parser fn (bbnf::grammar::generated::<g>::*) -> <G>StructBuilder (writes typed arena directly) -> <G>Document::finalise -> <G>View`. No mask stream, no event cursor, no shape branch. | Unimplemented | Single-path, struct-direct only. |
| Tape invariants — append-only after checkpoints; bounded rollback; tokens borrow source; direct views point into tape; one `(TapeId, node id, payload class)` identity per public node (`restart/ARCHITECTURE.md:1567-1575`). | No `Tape`, `TapeBuilder`, `TapeRec`, `TapeCursor`, `TapeId` types exist under `crates/core/src/runtime/`; the runtime grammars' mod-docs say "the tape substrate is severed on the JSON parse path; no TapeBuilder / TapeRec / TapeCursor symbol appears in this module's transitive code" (`crates/core/src/runtime/json/mod.rs:6-9`, identical wording in `csv/mod.rs:10-12`). | Unimplemented | Tape primitive is gone from the runtime crate entirely. |
| Generated public API surface lists `{generated.rs, parser.rs, host.rs, view.rs, value.rs, visitor.rs}` for every grammar (`restart/ARCHITECTURE.md:1646-1656`). | 0 of 9 grammars have `generated.rs`, `parser.rs`, `host.rs`, `visitor.rs`. All 9 have `value.rs` + `view.rs`. | Unimplemented | Artifact set materially smaller than spec; routing through `bbnf::grammar::generated::<g>::*` shifts the "generated" surface OUT of runtime into `crates/core/src/grammar/generated/<g>.rs` (10 files exist there, monolithic per grammar). |
| Runtime must reject invalid UTF-8 at parse/scan boundary (`restart/ARCHITECTURE.md:1178-1204` per V4). | UTF-8 boundary handling lives upstream in the generated parsers; per-grammar `parse_with.rs` calls `parse_<G>Parser_<rule>` on `src.as_bytes()` (`crates/core/src/runtime/json/parse_with.rs:90-93`); not visible in the runtime crate's own surface. | Unknown | Verify whether `crates/core/src/grammar/generated/json.rs` emits a `from_utf8` gate; if not, the spec gate is unmet. |
| Per-grammar declaration crates at `crates/<grammar>/` carry host-fn impls (Lock 14 surface (c), `restart/locks/LOCKS.md:220`). | Per-grammar declaration crates are absent — no `crates/json/`, `crates/css_l4/`, `crates/bbnf/`, etc.; everything is folded into `crates/core/src/runtime/<g>/`. | Unimplemented | Distribution shape diverged. |

# Generated SinkOnly / OffsetTape / EventTape Consumption Audit

The spec's three named consumption shapes are all ABSENT from the workspace runtime crate. The mapping below records what the live runtime emits **in place of** each spec shape.

| Spec shape | Live consumer (if any) | Evidence | Verdict |
|---|---|---|---|
| `OffsetTape` | None — the substring `OffsetTape` does not appear under `crates/`. | `grep -rnE '\bOffsetTape\b' crates/ ` returns empty. | Unimplemented (entire shape erased). |
| `SinkOnly` | None — the substring `SinkOnly` does not appear under `crates/`. The closest analogue is the `StructBuilder` trait at `crates/core/src/runtime/builder.rs:66` and per-grammar `<G>StructBuilder` impls (e.g. `crates/core/src/runtime/csv/builder.rs:44`, `crates/core/src/runtime/json/builder.rs`, `crates/core/src/runtime/css_pretty/builder.rs:44`). | `crates/core/src/runtime/builder.rs:1`-`66`; per-grammar impls. | Renamed / merged — see 1C-U1. The struct-direct workload is implemented for all 9 grammars but the SinkOnly name and the contract that distinguishes it from OffsetTape are gone. |
| `EventTape` | None — `EventTape` substring absent under `crates/`. The `event_grammar_witness.rs` proof scaffolding present in skinny does not exist in the main workspace. | `grep -rnE '\bEventTape\b' crates/` returns empty. | Unimplemented. |
| `CollapsedStage` | None. | `grep -rnE '\bCollapsedStage\b' crates/` returns empty. | Unimplemented. |
| `DocumentView` (the projection target) | Per-grammar `<G>View` types implementing the shared `RuntimeView` trait. | `crates/core/src/runtime/view.rs:1-76` defines `RuntimeView`; 9 of 9 grammars export `<G>View` (`runtime/mod.rs:25-71`). | Implemented — but as a projection over struct-direct arenas, NOT over sealed tape; the §9.2 union is therefore not honoured. |

# Lock 14 Grammar-Name Leak Audit (Workspace HEAD)

| Leak class | Evidence | Verdict | LOC-delta to repair |
|---|---|---|---|
| Per-grammar dirs in generic runtime crate | 9 dirs at `crates/core/src/runtime/{bbnf,bnf,css_l4,css_pretty,csv,ebnf,google_sheets,json,math}/`; verification cmd `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` returns 9 (Lock 14:220 requires 0). | Divergence (CRIT) | 10,915 LOC must be routed through the generator template before deletion; estimate 4-8 waves. |
| Grammar-named modules in runtime root | `crates/core/src/runtime/mod.rs:9-21` declares 9 `pub mod <g>;` lines. | Divergence (CRIT) | ~20 LOC to delete the mod lines; replaced by an `include!`/build-script generated manifest. |
| Grammar-named type reexports in runtime root | `crates/core/src/runtime/mod.rs:25-71` reexports **127 distinct grammar-named symbols** (mechanical extraction: 133 raw `pub use` entries inside the cited 25-71 window minus the 6 grammar-neutral exports present in the window — `StructBuilder` (`mod.rs:33`), `GenericAtRule` (`mod.rs:42`), `DtaError`+`ParseErr` (`mod.rs:58`), `CompoundHandle`+`StringHandle` (`mod.rs:63`); the other 4 cited neutrals `IntoPathSegment`+`Path`+`PathSegment` at `mod.rs:72` and `RuntimeView` at `mod.rs:76` sit OUTSIDE the cited window per NEW-CH2-V2-03): `BbnfArena, BbnfCompound, …, JsonValue, CssRule, CssDeclaration, SheetsArena, …`. | Divergence (CRIT) | **47 lines hold 127 grammar-named symbols** — the reexport block itself deletes in ~50 LOC, but downstream consumer rewires scale with the 127-symbol surface (proportional ~2.5× the cited band; route through `bbnf::grammar::generated::<g>::*` per consumer). |
| Grammar parser-name leaks in runtime source | `JsonParser` / `CssL4Parser` / `BbnfBootstrap` / `GoogleSheetsParser` strings appear at **30 sites across 15 files** under `crates/core/src/runtime/{json,bbnf,css_l4,google_sheets}/{parse_with,mod,document,builder,serialize}.rs` and `google_sheets/document/{mod,canonical}.rs` (live re-run 2026-05-23 at HEAD; prior cycle's "19+" floor undercounts by ~37% and missed the entire google_sheets/ sub-tree including `google_sheets/document/mod.rs:43,142`). | Divergence (HIGH) | ~190 LOC of doc strings + use statements + symbol references (rescaled from the 30-site live count). |
| Hand-written per-grammar runtime files (full set) | 67 / 67 hand-written; zero `@generated` markers across all 9 grammars. | Divergence (CRIT) | 10,915 LOC; the SK-V14 Pattern H baseline. |
| Per-grammar `parse_with.rs` shims | 4 files (json, bbnf, css_l4, google_sheets) that hand-thread `__path_plan` + `__shape_support_<G>Parser::ScanState::new()` + `parse_<G>Parser_<rule>` symbols. | Divergence (HIGH) | ~480 LOC (json 133 + bbnf 120 + css_l4 113 + google_sheets 114); each is the canonical entry point exposed via per-grammar `pub use parse_with::parse_with;`. |
| `RuntimeView` trait as the shared lowering point | `crates/core/src/runtime/view.rs:1-76` defines `RuntimeView` returning grammar-specific `<G>Kind` / `<G>View` types; the trait itself is grammar-neutral but its impls are scattered across all 9 grammars. | Allowed (generic trait) but the impl ownership is per-grammar | 0 LOC if trait stays and impls become generated. |

# Architecture §9 Template Comparison (per grammar)

The §9 template promises `mod.rs, generated.rs, view.rs, value.rs, visitor.rs, host.rs`. Matched against the actual roster:

| File expected by spec | Grammars present at HEAD | Grammars missing |
|---|---|---|
| `mod.rs` | 9 / 9 | none |
| `generated.rs` | **0 / 9** | bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math |
| `view.rs` | 9 / 9 (note: hand-written, not generated) | none |
| `value.rs` | 9 / 9 (note: hand-written, not generated) | none |
| `visitor.rs` | **0 / 9** | all 9 |
| `host.rs` | **0 / 9** | all 9 |

Files present in the live runtime but NOT named by the §9 template:

| File | Grammars | Function |
|---|---|---|
| `arena.rs` | 9 / 9 | Owning slab for compound child slices. |
| `builder.rs` | 9 / 9 | `<G>StructBuilder` impl of the shared `StructBuilder` trait. |
| `document.rs` | 8 / 9 (google_sheets uses `document/` sub-module instead) | `<G>Document` root + `<G>View` newtype + `<G>PathQuery` trait + `<G>Kind` discriminator. |
| `parse_with.rs` | 4 / 9 (bbnf, css_l4, google_sheets, json) | `PathCursor`-aware parse entry; threads `__path_plan` and `__shape_support_<G>Parser`. |
| `kind.rs` | 5 / 9 (bnf, css_pretty, csv, ebnf, math) | Standalone `<G>CompoundKind` discriminator. |
| `serialize.rs` | 1 / 9 (bbnf only) | Self-serializer for grammar source round-trip. |
| `document/{mod,canonical,view,path_query}.rs` | 1 / 9 (google_sheets only) | Sub-module split of the document layer. |

# Divergences Catalogued

| ID | Divergence | Evidence | loc_budget | risk | wave | hard_cap | same_wave_consumer | evidence_basis |
|---|---|---|---:|---|---|---:|---|---|
| 1C-D1 | All 9 per-grammar runtime dirs at workspace HEAD are hand-written, in direct violation of Lock 14's "hand-written per-grammar runtime files are forbidden" clause. | `restart/locks/LOCKS.md:220`; 67 files at `crates/core/src/runtime/<g>/`, zero generation markers. | 10,915 | Critical | Multi-tranche generator-template tranche (≥6 waves) | 1,200 LOC / wave | Same-wave PRUNE rollout per S-P0 prune list; generator template must consume grammar + workspace metadata. | path:line lock + 67-file census + zero generation markers. |
| 1C-D2 | Per-grammar dirs live FLAT at `crates/core/src/runtime/<g>/`, not at the spec path `runtime/src/grammars/<g>/`. | `restart/ARCHITECTURE.md:1646-1656`; `crates/core/src/runtime/mod.rs:9-21`; literal substring `runtime/grammars` absent under `crates/`. | 50 (path moves) + 10,915 (file relocations) | High | Same wave as 1C-D1 generator landing. | 200 LOC | Codegen output_dir + consumer use-paths. | spec path + cmd output. |
| 1C-D3 | All four spec shape names (`SinkOnly`, `OffsetTape`, `EventTape`, `CollapsedStage`) and `BackendShape` are absent from `crates/` entirely. | `restart/ARCHITECTURE.md:1612-1618` + `§7.3 BackendShape canon`; `grep -rnE '\bSinkOnly\|OffsetTape\|EventTape\|CollapsedStage\|BackendShape\b' crates/` returns empty. | 250-500 / shape | Critical | Substrate-revival tranche (or spec deletion via Pass Omega). | 800 LOC | Decision-engine `LayoutFacts.backend_shape` consumer; generator that emits shape-branched runtime per grammar. | shape-canon spec rows + zero workspace hits. |
| 1C-D4 | Runtime root `mod.rs` hand-wires 9 grammar modules + **127 grammar-named type reexports** (mechanical extraction, 47 lines: 133 raw `pub use` entries minus 6 in-window neutrals at `:33,:42,:58,:63`; 4 additional neutrals at `:72,:76` lie OUTSIDE the window) — the Lock 14 root-leak. | `crates/core/src/runtime/mod.rs:9-21` + `25-71`. | 80 (root rewrite) + ~2.5× consumer-rewire band proportional to 127-symbol surface | High | Same wave as 1C-D1; root replaced by `include!('manifest.rs')` or build-script registration. | 150 LOC | Generated manifest owner. | direct path:line. |
| 1C-D5 | Per-grammar `parse_with.rs` shims (4 grammars) leak `parse_<G>Parser_<rule>` and `__shape_support_<G>Parser` symbol names. | `crates/core/src/runtime/{json,bbnf,css_l4,google_sheets}/parse_with.rs` — 16 symbol refs in the parse_with.rs files alone (4 per file × 4 files), part of the 30-site / 15-file total under `crates/core/src/runtime/`. | 480 | High | Same wave as 1C-D1; PathCursor wiring must move into generator template. | 250 LOC | `bbnf::grammar::generated::<g>::*` emitter + consumer. | per-file path:line. |
| 1C-D6 | §9 template files `generated.rs`, `visitor.rs`, `host.rs` exist in 0 / 9 grammars at HEAD; the live roster substitutes `arena.rs`, `builder.rs`, `document.rs`. | `restart/ARCHITECTURE.md:1648-1656` vs HEAD census. | spec-side ~100 LOC (template doc rewrite) OR impl-side ~3,000 LOC (visitor + host + generated emission) | Medium / High depending on direction | T-P3 disposition: either spec absorbs the live roster (with co-derivation note) or the template must emit visitor/host/generated. | 600 LOC | Generator template author. | roster diff. |
| 1C-D7 | The §9.2 tape ∪ direct-to-struct UNION is broken at the runtime layer — `TapeBuilder`/`TapeRec`/`TapeCursor` are absent and mod-docs explicitly state "the tape substrate is severed on the JSON parse path" (`crates/core/src/runtime/json/mod.rs:6-9`, `crates/core/src/runtime/csv/mod.rs:10-12`). | spec `restart/ARCHITECTURE.md:1577-1644`; live mod-doc text. | tape revival: 1,500-3,000 LOC; OR spec-amendment via Pass Omega. | Critical | T-P3 disposition. | spec amend OR generator template revival | None at present; tape lives only in skinny `skinny/crates/runtime/src/tape/mod.rs:94-100`. | comment-text + zero tape symbols. |
| 1C-D8 | google_sheets is the only grammar with a nested `document/` sub-module (4 files); other grammars keep `document.rs` flat. | `crates/core/src/runtime/google_sheets/document/{mod,canonical,view,path_query}.rs`. | 200 LOC normalization | Low | T-P3 disposition on whether the sub-module split is canonical for the generator. | 100 LOC | Generator template emitter. | per-file census. |
| 1C-D9 | bbnf is the only grammar with `serialize.rs` (442 LOC) — a self-serializer; spec template does not name a serializer file. | `crates/core/src/runtime/bbnf/serialize.rs:1-442`. | 50 LOC spec amendment | Low | T-P3 disposition: name `serialize.rs` in the template as a grammar-opt file. | 100 LOC | Generator template doc. | per-file census. |
| 1C-D10 | Per-grammar declaration crates `crates/<grammar>/` (Lock 14 surface (c)) are absent — no `crates/json/`, `crates/css_l4/`, `crates/bbnf/`, etc.; per-grammar code is folded into `crates/core/src/runtime/<g>/`. | Lock 14:220 surface (c); `ls crates/` shows no per-grammar crate at the workspace root. | 200 LOC scaffolding × 9 grammars = 1,800 LOC | Medium | T-P3 disposition: split per-grammar host-fn impls into declaration crates OR amend Lock 14 surface (c). | 300 LOC | Declaration-crate scaffolding owner. | workspace ls. |
| 1C-D11 | Skinny CSS L4 cluster has 7 sub-grammars (`css_l4_at_rules_and_media`, `css_l4_declaration_values`, `css_l4_declaration_values_extended`, `css_l4_nested_layout`, `css_l4_stylesheet_selectors`, `css_l4_vendor_and_custom_atrules`, `css_l4_visual_functions`); main workspace has only monolithic `css_l4/` (7 files) + sibling `css_pretty/` (7 files). | `find skinny/crates/runtime/src/grammars -maxdepth 1 -type d` vs `find crates/core/src/runtime -maxdepth 1 -type d`. | spec-side amendment OR impl-side migration; ~500-1,500 LOC. | Medium | T-P3 disposition: declare which CSS L4 layout is canonical for V1. | varies | Same-wave CSS scope decision. | dir census diff. |

# Gaps / Missing Primitives

| Gap | Evidence | Verify action |
|---|---|---|
| No grammar-agnostic generator template emitting per-grammar runtime modules. | 67 / 67 hand-written; no `xtask`-style generator target visible. | `find crates/ -name 'xtask*' -o -name 'codegen*'` and inspect `crates/core/src/grammar/generated/` build mechanism (10 monolithic per-grammar `.rs` files exist there but emit only parsers, not runtime modules). |
| No `BackendShape` / `LayoutFacts.backend_shape` type. | Absent from `crates/`. | Verify which crate owns layout facts; confirm whether the type is planned in `passes/` or `cost-model/`. |
| No `Tape`/`TapeBuilder`/`TapeRec`/`TapeCursor` substrate in the runtime crate. | Mod-docs declare it severed; substring absent. | Decide whether tape lives in a future generator-emitted module or has been abrogated in favor of struct-direct only. |
| No `EventCursor` / `EventCell` storage. | Absent from `crates/`. | Confirm spec §9.2 retention vs. abrogation in Pass Omega. |
| No per-grammar declaration crates at `crates/<grammar>/`. | `ls crates/` shows no per-grammar entries; per-grammar code folded into `crates/core/src/runtime/<g>/`. | Confirm Lock 14 surface (c) intent; amend lock if collapse into runtime crate is sanctioned. |
| No `host.rs` per-grammar host-fn registry. | 0 / 9 grammars; spec template names `host.rs`. | Locate host-fn dispatch site; likely in `crates/host/` (per ARCHITECTURE §4.3 line 501-508). |
| No `visitor.rs` per-grammar visitor. | 0 / 9 grammars; spec template names `visitor.rs`. | Determine whether `RuntimeView` trait subsumes the visitor role; if so, name the equivalence in v+1 spec. |

# Open Questions

| UNKNOWN | Why unknown | verify_action |
|---|---|---|
| 1C-U1: Is the `StructBuilder` trait + per-grammar `<G>StructBuilder` impl model a renamed-`SinkOnly` (Lock 14 verification cmd #4 forbids "renamed-scanner violation" — the equivalent for builders is the same shape leak) OR a distinct fourth materialization branch that the spec must absorb? | Substring `SinkOnly` is gone; the comment "the tape substrate is severed" suggests a deliberate collapse onto struct-direct only, not a rename. But the consumer-facing semantics (direct typed-field writes with no retained tape) are exactly `SinkOnly`'s. T-P2 must research whether `StructBuilder` IS the SinkOnly contract under a new name. | Read `crates/core/src/runtime/builder.rs:1-141` end-to-end against `restart/ARCHITECTURE.md:1606-1616`; compare with skinny `json/generated.rs:393` SinkOnly tag; classify in T-P3. |
| 1C-U2: Is `EventTape` abrogated for V1 or simply unimplemented? | `EventTape` is absent from both main workspace and (mostly) skinny — only proof-only `EventGrammar` witnesses exist in skinny (`skinny/crates/runtime/src/grammars/{json,sheets_witness}/event_grammar_witness.rs`). | T-P3 must decide: (a) keep `EventTape` in V1 spec, plan wave for substrate revival; or (b) Pass-Omega-amend the spec to remove `EventTape` from the four-shape canon (reducing to three: `OffsetTape`/`SinkOnly`/`CollapsedStage`). |
| 1C-U3: Does the `bbnf::grammar::generated::<g>::*` emission satisfy the spec's "generated per-grammar runtime modules" clause if it does NOT emit under `runtime/src/grammars/<g>/`? | Generated parsers DO exist at `crates/core/src/grammar/generated/<g>.rs` (one monolithic file per grammar); they emit `<G>Parser::parse` + `parse_<G>Parser_<rule>` + `__shape_support_<G>Parser` symbols. But the spec says "runtime/src/grammars/" specifically. | T-P3 must decide: relocate generated parser emission to runtime/grammars/, OR amend spec to recognize the grammar/generated/ path. |

# Verification

All counts re-derived at HEAD on 2026-05-23:

- `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d | wc -l` → 9 (Lock 14:220 requires 0).
- `find crates/core/src/runtime/<g> -type f -name '*.rs' | wc -l` per grammar: bbnf 8, bnf 7, css_l4 7, css_pretty 7, csv 7, ebnf 7, google_sheets 10, json 7, math 7 — sum 67. Reproduces SK-V14 S-P0 A6 Pattern H baseline exactly.
- `find skinny/crates/runtime/src/grammars -type f -name '*.rs' | wc -l` → 48. Reproduces SK-V14 S-P0 A6 skinny baseline exactly.
- `grep -rnE '\bSinkOnly\|OffsetTape\|EventTape\|CollapsedStage\|BackendShape\b' crates/` → empty.
- `rg -l '@generated|AUTO-GENERATED|THIS FILE IS GENERATED' crates/core/src/runtime/` → empty.
- `rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/core/src/runtime/ | wc -l` → **30** matches; `rg -l 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/core/src/runtime/ | wc -l` → **15** files: bbnf/{document,mod,parse_with,serialize}.rs, css_l4/{document,mod,parse_with}.rs, google_sheets/{mod,parse_with}.rs, google_sheets/document/{canonical,mod}.rs (sites `:43`,`:142`), json/{builder,document,mod,parse_with}.rs (prior cycle's "19+ across 11 files" floor missed the entire google_sheets/ sub-tree).
- `crates/core/src/runtime/mod.rs:25-71` mechanical extraction → **127 distinct grammar-named symbols** across **47 lines**. Method: 133 raw `pub use` entries inside the cited 25-71 window (verifiable via `awk 'NR>=25 && NR<=71' crates/core/src/runtime/mod.rs | python3 -c "import sys, re; t=sys.stdin.read(); print(sum(len([s for s in (m.group(1) if m.group(1) else m.group(2)).split(',') if s.strip()]) for m in re.finditer(r'pub use\\s+[\\w_:]+(?:::\\{([^}]+)\\}|::(\\w+))', t)))"`) minus the 6 grammar-neutral exports present INSIDE the window: `StructBuilder` (`crates/core/src/runtime/mod.rs:33`), `GenericAtRule` (`crates/core/src/runtime/mod.rs:42`), `DtaError` (`crates/core/src/runtime/mod.rs:58`), `ParseErr` (`crates/core/src/runtime/mod.rs:58`), `CompoundHandle` (`crates/core/src/runtime/mod.rs:63`), `StringHandle` (`crates/core/src/runtime/mod.rs:63`) → 127. The 4 additional grammar-neutral exports sit OUTSIDE the 25-71 window: `IntoPathSegment` (`crates/core/src/runtime/mod.rs:72`), `Path` (`crates/core/src/runtime/mod.rs:72`), `PathSegment` (`crates/core/src/runtime/mod.rs:72`), `RuntimeView` (`crates/core/src/runtime/mod.rs:76`); they would belong in the subtraction only if the window were widened to 25-77 (133 raw + 4 raw = 137; 137 - 10 = 127 — equivalent). Per-grammar in-window breakdown (after subtracting in-window neutrals): bbnf 10, bnf 10, css_l4 **43** (44 raw - 1 in-window neutral `GenericAtRule`; the 3 css_l4-named aliases `CssRule, CssDeclaration, CssSelector` at `mod.rs:34-35` are counted inside the 43), css_pretty 10, csv 10, ebnf 10, google_sheets 11, json 13, math 10; sum 10+10+43+10+10+10+11+13+10 = 127. Discipline rule NEW-CH2-V2-03 (recommended by CH2 V2 V3 fold): any "N grammar-named X" subtract-from-K cite must enumerate the K neutrals with `path:line` inside the cited window.
- Total runtime .rs files at HEAD: 75 (8 root-level + 67 per-grammar).

# Escalations

None at the cycle level. The pass is the totality-track T-P1 V6 re-open driven by SK-V14 audit-corrected baseline; V5 was the prior converged cycle on the skinny prototype, and the dispatch context binding pivots 1C's scope to the main workspace HEAD. All findings carry verdicts; UNKNOWNs carry verify_actions; no LOCKS amendment candidates surfaced from this inventory (1E owns that scan).
