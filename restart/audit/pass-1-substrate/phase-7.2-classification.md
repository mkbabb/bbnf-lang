# Phase 7.2 Classification — PASS-1 type-system + function-value fold

This file records the verify-then-patch classification for the Phase 7.2 PASS-1
fold dispatched after Phase 7.1's lock + ARCHITECTURE amendments landed
(`adbaaaa0`, `9cb92284`). Source is `restart/research/V1-FOLD-CANDIDATES.md`
Tier 1+2+3 routed to PASS-1, distilled through audit-1 (type system) and
audit-2 (function/value). The amendment lands in the next commit on this
branch.

## Classification table

| Item | Surgery directive | Current PASS-1.md state | Classification | Surgery to land |
|---|---|---|---|---|
| A1 — DK13 higher-rank | Update §3 type algorithm: name DK13 algorithmic completeness; cite Dunfield-Krishnaswami 2013/2019; describe synthesis + check + subsumption modes; rank-1 generic rules `Object<V>` remain principal-scheme; higher-rank arrives via DK13 for explicit `forall` annotations. | `:73` carries "HM inference … expected-type check/synth … first-order unification". DK13, Dunfield-Krishnaswami, synthesis/check/subsumption mode names, rank-1-as-default-with-DK13-extension framing absent. | patch-delta | Replace the `:73` algorithm sentence with the HM + Pierce-Turner + DK13 stack: name HM equality + Algorithm-W principal schemes (Damas-Milner 1982; Pierce 2002 ch.22), Pierce-Turner local check/synth, DK13 algorithmic completeness (Dunfield-Krishnaswami 2013; ordered existential contexts; principality tracking; decidability; soundness; completeness). Rank-1 is default; rank-N arrives via DK13 with explicit `forall` annotation rules. |
| A2 — GADT hidden substrate | §3: name internal CSP machinery (branch-local-equality plumbing) without user-facing GADT syntax; reserve `BBNF-LOCAL-EQUALITY-ANNOTATION` diagnostic. | `:75` says "V1 has no GADT branch-local equality surface. A later indexed/local-equality feature must arrive by amendment with annotation rules and `BBNF-LOCAL-EQUALITY-ANNOTATION`." Substrate machinery (branch-local-equality plumbing inside CSP solver feeding `LayoutFacts`) absent. | patch-delta | Append after `:75` GADT sentence: GADT/branch-local-equality machinery is internal substrate (CSP solver branch-equality propagation feeding `LayoutFacts`); the user-facing GADT surface defers to V2 amendment via `BBNF-LOCAL-EQUALITY-ANNOTATION`. |
| A3 — Internal row polymorphism | §3: describe internal row-poly for record-narrowing collapse (no user surface). | `:75` says "row-polymorphism and open structural record subtyping are out of V1." This is the surface fence; the internal-row-poly fold is unstated. | patch-delta | Reframe: V1 record narrowing is finite generated-shape coercion at the surface; the internal row-polymorphism collapse (Leijen-style scoped labels) is a `passes::layout` subroutine and never a public artefact. The user-facing row-poly surface defers to a later type-system research gate. |
| A4 — Schema-mining miner | Add §3 sub-section for telemetry-driven schema inference; cite user mandate. | PASS-1 has no schema-miner prose. ARCH §8.2 carries the paragraph; PASS-1 §3 must mirror. | patch-delta | Add a paragraph after §3 type algorithm: schema-mining miner runs as peer of recogniser miners; observes `(rule_shape, layout_decision, value_shape)` triples; proposes named-record/named-enum/sum-type identities through the HM/CSP/DK13 chain; rejects candidates that fail principality or finite-CSP legality. The user's "type algebra + telemetry to generate semantic schemas without explicit annotations in most cases" lands here. |
| A5 — CHR-improvement layer | Mention CHR-improvement layer for host overloads. | `:73` says "Host overloads with determining arguments emit explicit improvement constraints, CHR-shaped where applicable, before finite CSP selection." | verify-only-stub | The CHR-improvement layer is named at `:73`. No further PASS-1 surgery; the specification of the layer lives at `host/signature/`. |
| B1 — F1+F2 function types HM/DK13 | Update §3 to describe HM equality + DK13 over function types; verify §6 carries `FnType`. | §6 `:226` carries `Type = ... | FnType` and `:227` carries `FnType = "fn" "(" TypeList? ")" "->" Type`. §3 type algorithm does not describe arrow types as a first-order constructor. | patch-delta | Add to §3 after the HM/DK13 stack: function arrow is the canonical first-order type constructor (Milner 1978); `FnType` decomposes through Pottier-Rémy first-order unification; DK13 application judgment handles function values without further extension. |
| B2 — F3 function-typed `@host fn` params | Show `@host fn map<T, U>(f: fn(T) -> U, xs: [T]) -> [U]` typing in §3 — the transducer apotheosis. | §6 `:235` already names the transducer apotheosis at the grammar surface. §3 type algorithm prose does not name function-typed parameters. | patch-delta | Add to §3 function-value paragraph: `@host fn map<T, U>(f: fn(T) -> U, xs: [T]) -> [U]` types under DK13 with `f`'s arrow type concrete at the monomorphisation site; the transducer apotheosis follows without a `@transducer` directive. |
| B3 — F4 lambda literal | §3 describes lambda typing (DK13 synthesis from body or check from expected). | §6 `:222` carries `LambdaExpr = "|" Params? "|" ( Expr | Block )` and `:235` carries the surface form. §3 algorithm does not describe lambda typing. | patch-delta | Add to §3 function-value paragraph: lambda expressions synthesise an arrow type from body (DK13 synthesis mode) when no expected type flows in; check against expected arrow type (Pierce-Turner check mode) when one does — e.g., from a `fn`-typed parameter. |
| B4 — F5 closure capture by `&'i` only | §3: every captured binding is borrowed by `&'i Tape<'i>`-bounded reference; no capture-by-move; no `Fn`/`FnMut`/`FnOnce` discrimination. | §6 `:235` carries the closure-by-reference rule at the grammar surface. §3 type algorithm prose does not. | patch-delta | Add to §3 function-value paragraph: every captured binding is borrowed by `&'i Tape<'i>`-bounded reference; capture-by-move is forbidden in V1; the `Fn`/`FnMut`/`FnOnce` discrimination Rust exposes is collapsed at the BBNF surface — the lifetime-bounded reference closure is the only V1 form. |
| B5 — F8/F9 match + tuples | §3: match-expression typing + tuple typing (product-type rules). | PASS-1 has no match or tuple typing prose. `TupleType` is in `:226` `Type` production. `Match` and `Tuple` Primary forms are not yet in §6. | patch-delta + grammar amendment | Add `Match` and `Tuple` to §6 `Primary` production with corresponding `Arm` / `Pattern` productions. Add a §3 paragraph: match expressions check each arm against the scrutinee's variant set (grammar-derived enums); arm-result types unify across arms; exhaustiveness is checked at compile time and emits `BBNF-PATTERN-NONEXHAUSTIVE` on failure. Tuples synthesise the product type from component types and pattern-destructure through `Pattern`. |
| C1 — parse-that-regex | Update PASS-1 §3 to reference `parse-that-regex` as the regex engine. Confirm no `regex-automata` references remain. | `:175` already carries `parse-that-regex/` in the e-graph rewrite plug-in row. `:138` and `:157` per-crate trees still reference `parse-that` (the parent crate, which is correct — `parse-that-regex` is the regex sub-crate of `parse-that`). | verify-only-stub | Confirm `rg -n 'regex-automata' restart/audit/pass-1-substrate/PASS-1.md` returns zero. The §3 per-crate tree retains `parse-that` (the parser combinator + regex family parent crate); `parse-that-regex` is its regex sub-crate per Lock 11. No surgery. |
| D1 — §6 grammar amendments | Verify §6 carries 6-directive `Directive` and `FnType` and `LambdaExpr` — verify-only-stub if Phase-7.1 already landed all three. | §6 `:196` carries the 6-directive `Directive = ImportDecl | HostFn | ErrorDecl | LayoutDecl | PrettyDecl | TokenDecl ;`. `:226-228` carry `FnType` and `TypeList`. `:222` carries `LambdaExpr`. All three Phase-7.1 amendments present. | verify-only-stub | No surgery; Phase-7.1 already landed the §6 grammar amendments. |

## Routing summary

| Surgery class | Items |
|---|---|
| verify-only-stub | A5, C1, D1 |
| patch-delta | A1, A2, A3, A4, B1, B2, B3, B4 |
| patch-delta + grammar amendment | B5 |

## Acceptance gates carried into the amendment commit

- §3 type-system algorithm: HM + Pierce-Turner + DK13 stack named with citations; CHR-improvement layer confirmed in place.
- §3 GADT substrate: branch-local-equality plumbing named as internal CSP machinery; surface defers via `BBNF-LOCAL-EQUALITY-ANNOTATION`.
- §3 row polymorphism: internal collapse named; surface fence preserved.
- §3 schema-mining miner: peer-of-recogniser-miners paragraph mirrors ARCH §8.2.
- §3 function values: arrow type as first-order constructor; DK13 application judgment; transducer apotheosis worked through; lambda synthesis/check; closure-capture-by-`&'i` rule.
- §6 grammar: `Match`, `Tuple`, `Arm`, `Pattern` productions added; `Primary` extended.
- §3 match + tuple typing: arm-unification + exhaustiveness + product-type synthesis paragraph.
- C1 parse-that-regex: no `regex-automata` references in PASS-1; `parse-that-regex` named only where the regex engine is the subject; the parent `parse-that` crate retains its per-crate-tree row.

The amendment commit lands these surgeries verbatim against PASS-1.md; this
classification file is preserved as evidence that Phase-7.1 baseline was
inspected before the Phase-7.2 fold.
