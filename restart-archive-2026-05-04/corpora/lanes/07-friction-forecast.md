# Hardening Plan Audit 07 — Friction Forecast

Date: 2026-05-03
Question: where will users and grammar authors misunderstand the new API?

## Forecast Ledger

| Surface | Planned site | Mental model required | Point of confusion | Required artefact | Verdict |
|---|---|---|---|---|---|
| `pointer!["a","b",1]` | BB-G7 at `docs/tranches/BB/BB.md:21`; BB.W5 at `docs/tranches/BB/BB.md:36`; SOTA analogue at `audit/SOTA-2026-05-03.md:32-40`; BB.W5 names `docs/cookbook/path-macro.md` at `docs/tranches/BB/waves/W5.md:103`. | The macro builds a compile-time typed path AST, validates it against a generated `StructRegistry`, and returns a path whose terminal type is known. | The syntax shown has no grammar marker. Users will ask how the macro knows whether `"a"` belongs to JSON, CSS, Sheets, or BBNF. | `docs/cookbook/path-macro.md`; macro docs with grammar inference rules and examples for explicit marker vs inferred value type. | partial |
| `parse(input)` vs `parse_in(input, bump)` vs `parse_owned(input)` | BB-G8 at `docs/tranches/BB/BB.md:22`; BB.W4 at `docs/tranches/BB/BB.md:35`; risk note at `docs/tranches/BB/BB.md:92`. | Default parse borrows `&'i str`; bumpalo is opt-in lifetime extension; owned copies are for storage after input drops. | Users will reach for `parse_owned` because lifetimes are frightening, or pass a bump to default parse expecting ownership. | `docs/cookbook/lifetime-surfaces.md` with a decision table and one per-grammar docstring template. | partial |
| Layout lowering errors | BA.W2 canon at `docs/tranches/BA/waves/W2.md:52-57`; BC friction row at `docs/tranches/BC/BC.md:183-186`. | Layout lowering is the deterministic grammar-shape-to-memory-layout pass. It is not type inference and not backend codegen. | Authors will not know whether a failure belongs to grammar syntax, layout lowering, or Rust emission. | `docs/errors/layout-lowering.md`; one diagnostic page per error code. | partial |
| Pratt auto-detection misfire | BB-G6 at `docs/tranches/BB/BB.md:20`; BB.W3 at `docs/tranches/BB/BB.md:34`; risk at `docs/tranches/BB/BB.md:88`. | Pratt is mined from left-recursive operator-chain shape, then accepted or rejected by cost/shape checks. | Authors cannot annotate `@pratt`; when a rule is or is not classified, they need to see why. | `docs/optimizer/pratt-simd-detection.md`; compiler warning with rule name, mined shape, rejection reason, and fallback. | silent-must-add |
| SIMD auto-detection threshold | BB-G6 at `docs/tranches/BB/BB.md:20`; risk at `docs/tranches/BB/BB.md:89`. | SIMD scans pay a dispatch/setup cost; the optimiser chooses it only when input/leaf pattern density clears a threshold. | Users will think SIMD absence is a bug if a charclass "looks SIMD-able." | Add threshold explanation to `docs/optimizer/pratt-simd-detection.md`; expose `--explain-optimizer <rule>`. | silent-must-add |
| Visitor bitflags | BB-G9 at `docs/tranches/BB/BB.md:23`; BC friction row at `docs/tranches/BC/BC.md:186`. | `VisitTypes` prunes subtrees; `visit_<Name>` is not called unless the visitor declares the matching bits. | Visitors compile yet appear not to run. | `docs/cookbook/visitors.md`; warning already sketched at `docs/tranches/BC/BC.md:186`. | partial |
| Core split migration | BC.W3 split at `docs/tranches/BC/waves/W3.md:49-87`; BC friction row at `docs/tranches/BC/BC.md:187`. | Users must know which crate now owns parse, codegen, runtime, and path APIs. | Imports that once came from `bbnf::runtime::<g>` move or re-export; without migration docs, downstream fixes become guesswork. | `docs/migration/bc-core-split.md`; one import-before/import-after table per public surface. | silent-must-add |

## Required Diagnostic Text

These messages should be committed verbatim, then adjusted only for local type names.

| ID | Surface | Message |
|---|---|---|
| F07-E1 | pointer macro | `error: pointer segment 2 indexes into rule 'object', but the resolved Layout is not an array; expected a field name from {<fields>}` |
| F07-E2 | pointer macro | `error: pointer path has no grammar context; call pointer!(Json, ["a", "b", 1]) or pass it to a typed parser method so the registry can be inferred` |
| F07-E3 | lifetime API | `error: parse(input) returns values borrowing from 'input'; use parse_owned(input) only when the input buffer cannot outlive the parsed value, or parse_in(input, bump) when arena ownership is intentional` |
| F07-E4 | layout lowering | `error: rule '<rule>' has no resolvable Layout because <reason>; layout lowering requires every compound branch to resolve to a struct, enum, repeat, or borrowed slice` |
| F07-E5 | Pratt detection | `warning: rule '<rule>' was not lowered with Pratt because branch '<branch>' is not an operator-chain segment; emitted recursive descent instead` |
| F07-E6 | SIMD detection | `note: SIMD scanner not emitted for rule '<rule>'; estimated input length < <N> bytes and scalar dispatch is cheaper for this leaf pattern` |
| F07-E7 | visitor | `warning: visitor method visit_<Name> is implemented, but visit_types() does not include <Name>::CHILD_TYPES; this subtree will be skipped` |

## Plan Faults

| ID | Site | Fault | Surgery |
|---|---|---|---|
| F07-1 | `docs/tranches/BB/BB.md:21`, `docs/tranches/BB/BB.md:36` | The pointer syntax lacks grammar context. | Add a grammar-inference paragraph and explicit syntax alternatives to BB.W5: `pointer!(Json, ["a","b",1])` and typed-context shorthand `pointer!["a","b",1]`. |
| F07-2 | `docs/tranches/BB/waves/W4.md:28`, `docs/tranches/BB/waves/W4.md:64`, `docs/tranches/BB/waves/W4.md:147`, `docs/tranches/BB/BB.md:92` | BB.W4 wave docs gate the lifetime cookbook, but the tranche roll-up lists it only as mitigation. The top-level execution plan can lose the gate. | Add BB.W4 tranche gate: `docs/cookbook/lifetime-surfaces.md` exists and every generated parser docstring links it. |
| F07-3 | `docs/tranches/BA/waves/W2.md:52-57`, `docs/tranches/BC/BC.md:185` | Layout diagnostics are sketched in BC, but BA.W2 is where authors first see the term. | Add BA.W2 artefact `docs/errors/layout-lowering.md` and require at least three explicit `LayoutError` variants. |
| F07-4 | `docs/tranches/BB/BB.md:88-89` | Pratt/SIMD misfire mitigation is test-oriented, not author-oriented. | Add `--explain-optimizer <grammar>::<rule>` output and the warnings F07-E5/F07-E6 to BB.W3 gates. |
| F07-5 | `docs/tranches/BB/waves/W5.md:48`, `docs/tranches/BB/waves/W5.md:103`, `docs/tranches/BB/waves/W5.md:177` | The plan's path cookbook name is `path-macro.md`; any audit or tranche text naming `pointers.md` creates duplicate docs and stale links. | Canonicalise on `docs/cookbook/path-macro.md`; add a gate that `rg -n 'cookbook/pointers' docs/` returns zero. |
| F07-6 | `docs/tranches/BB/waves/W5.md:22`, `docs/tranches/BC/BC.md:186` | Visitor education is only a warning sketch. Users need a cookbook because visitor bitflags cause silent-looking non-calls. | Add `docs/cookbook/visitors.md` to BB.W5 or BC.W4 gates, with `VisitTypes` failure examples and F07-E7. |
| F07-7 | `docs/tranches/BC/waves/W3.md:205`, `docs/tranches/BC/BC.md:187` | BC.W3 promises an audit artefact for the core split, but not a user-facing migration page. That leaves API break repair ad hoc. | Add `docs/migration/bc-core-split.md` to BC.W3 gates, including crate ownership, import migration, and re-export sunset rules. |

## Lane Verdict

| Status | Count |
|---|---:|
| partial | 4 |
| silent-must-add | 3 |
| violated | 7 |

The plan knows these surfaces are hard, but the documentation contract is still too thin. BA.W2, BB.W3, BB.W5, and BC.W3 must carry author-facing pages and diagnostics before execution.
