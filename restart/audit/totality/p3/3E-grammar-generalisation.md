---
agent: 3E
pass: T-P3-synthesis
cycle: V1
generated_at: 2026-05-21T19:12:50Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
t_p2_dossiers_consumed: [2A, 2B, 2C, 2D, 2E, 2F]
v1_surface_targeted: n/a
proposed_deltas_count: 8
delta_summary:
  carried_from_prior_cycle: []
  removed: []
  answered: []
  newly_added: [3E-D01, 3E-D02, 3E-D03, 3E-D04, 3E-D05, 3E-D06, 3E-D07, 3E-D08]
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: []
---

## Executive Summary

The non-JSON generality story is mechanical: keep the five `BackendShape`
variants, but make every selection, primitive policy, sink, flag, and provider
surface generated from grammar source plus workspace metadata. CSS L4 is the
positive proof lane because SK-V12 admitted one strict same-plane
declaration-values fact row, while SK-V13 says that row is evidence, not full
CSS parity (`skinny/RESULTS.md:94`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:38-57`).
Sheets and BBNF-self are negative controls because JSON object/member/value role
mining cannot model formulas, references, directives, or Pratt/operator chains
(`restart/audit/totality/p2/2C-grammar-neutrality.md:72-79`,
`restart/audit/totality/p1/1D-skinny-lessons.md:80-81`). T-P2 converged on the
same rule: primitives are grammar-neutral byte-stream operations, but their
alphabets, quote/escape/control policy, number policy, output facts, and costs
come from generated data (`restart/audit/totality/p2/2C-grammar-neutrality.md:50-56`,
`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:53-75`). T-P3 therefore
proposes Lock 14 hardening clauses, a CSS/Sheets/BBNF-self `BackendShape`
matrix, and a future-grammar onboarding test. It does not edit V1 spec surfaces.

## V1 Delta Summary

| bucket | disposition |
|---|---|
| carried from prior cycle | None. This is T-P3 V1 for agent 3E. |
| removed | None. |
| answered | None from a prior T-P3 cycle. The artifact answers T-P2 2C's future-grammar onboarding requirement and Lock 14 transfer contract (`restart/audit/totality/p2/2C-grammar-neutrality.md:157-168`). |
| newly added | 3E-D01 through 3E-D08 below. |

## Non-JSON Generality Story

The invariant is finite-shape, data-driven selection. `ARCHITECTURE.md` already
defines `LayoutFacts.backend_shape` as a public side table and the five shapes as
`EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, and `CollapsedStage`
(`restart/ARCHITECTURE.md:1045-1056`, `restart/ARCHITECTURE.md:1060-1088`).
T-P1 proved those symbols are live but partial in skinny, especially for cost
strength and non-`SinkOnly` lowerers (`restart/audit/totality/p1/1B-codegen-evidence.md:34-47`).
T-P2 refuted treating the fixed P1-P8 order as the optimizer and requires
candidate generation, feasibility filtering, active cost extraction, and
generated grammar facts (`restart/audit/totality/p2/2D-cost-model.md:155-168`).

### BackendShape Matrix

| grammar | rule or product | proposed `BackendShape` | generated facts required | evidence |
|---|---|---|---|---|
| CSS L4 | `ruleItem` / stylesheet dispatch hub | `OffsetTape` when FIRST sets are byte-disjoint and output remains retained facts | token alphabet for `@`, ident-start, `.`, `#`, `*`, block delimiters, and layout/comment policy | Existing totality matrix gives CSS `ruleItem` as `OffsetTape` (`restart/HANDOFF.md:220-229`); CSS token families are the positive non-JSON falsifier (`restart/audit/totality/p2/2C-grammar-neutrality.md:69-71`). |
| CSS L4 | selector families, pseudo-classes, attributes, combinators | `EagerTape` unless generated selector FIRST/follow facts prove a cheaper retained route | selector grammar facts, recovery policy, combinator and pseudo payload facts | SK-V13 names selectors as missing and high priority (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:96-120`); 2C says selector rows cannot be JSON object/member/value projections (`restart/audit/totality/p2/2C-grammar-neutrality.md:70`). |
| CSS L4 | declaration/property fact stream | `EventTape` for retained visitor traversal; `SinkOnly` for admitted direct fact-stream output | generated fact schema, property-name payload enum, important flag, strict comparator provenance | 2C names CSS `declaration` as event payload retention and current CSS fact stream as a legitimate output-plane gap (`restart/audit/totality/p2/2C-grammar-neutrality.md:78-80`); T-P1 preserves the admitted CSS fact-stream row while naming the category gap (`restart/audit/totality/p1/1A-substrate-evidence.md:45-46`). |
| CSS L4 | values, dimensions, percentages, `calc()`, `var()`, color functions | `EagerTape` until generated value-policy facts and cost evidence admit event or direct lowering | number/dimension suffix policy, function family facts, custom-property and substitution policy | 2C grounds CSS `calc`/custom-property policy as non-JSON (`restart/audit/totality/p2/2C-grammar-neutrality.md:71`); SK-V13 matrix marks `calc`, `var`, and color functions missing or partial (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:104-108`). |
| Sheets | `formula`, `cellRef`, `primary`, reference/range atoms | `OffsetTape` for byte-disjoint A1/reference dispatch and lazy spans | cell/range grammar, reference operator facts, separator and quote policy | 2C cites OpenFormula function/reference operators as a JSON-role falsifier (`restart/audit/totality/p2/2C-grammar-neutrality.md:72`); the prior matrix gives Sheets `formula` / `cellRef` / `primary` as `OffsetTape` (`restart/HANDOFF.md:234-236`). |
| Sheets | function calls, `LET`, `LAMBDA`, array literals | `EventTape` when function-name or array payloads must reach a typed AST; `SinkOnly` only for direct formula-fact products | function-name DFA/payload facts, semicolon parameter policy, array-literal facts, oracle schema | 2C transfer requires Sheets function/reference/operator roles from generated metadata, not generic branches (`restart/audit/totality/p2/2C-grammar-neutrality.md:72`, `restart/audit/totality/p2/2C-grammar-neutrality.md:125`). |
| Sheets | infix expressions | `EagerTape` or Pratt-selected retained shape from resolver | operator precedence/associativity facts, Pratt eligibility, strict formula oracle | T-P1 says Sheets formulas cannot rely on JSON roles (`restart/audit/totality/p1/1B-codegen-evidence.md:64-69`); the prior matrix gives Sheets `expression` as `EagerTape` for precedence (`restart/HANDOFF.md:234-236`). |
| BBNF-self | grammar/declaration/term dispatch | `OffsetTape` for byte-disjoint declaration/token dispatch | directive starts, identifier/literal policy, alternation and repetition facts | 2C marks BBNF-self as a falsifier requiring directive/operator facts (`restart/audit/totality/p2/2C-grammar-neutrality.md:73`); prior matrix gives BBNF `grammar` / `declaration` / `term` as `OffsetTape` (`restart/HANDOFF.md:231-233`). |
| BBNF-self | expression/operator chain | `EagerTape` with Pratt recognized by generated Grammar IR facts | precedence and associativity facts, recursion bounds, operator token facts | T-P1 requires BBNF-self proof beyond JSON role mining (`restart/audit/totality/p1/1D-skinny-lessons.md:80-81`); Lock 10 forbids `@pratt` and requires auto-detection (`restart/locks/LOCKS.md:70`). |
| BBNF-self | directives and generated grammar facts | `EventTape` for retained directive payloads; `SinkOnly` for direct fact-output probes | directive-kind enum, argument schema, layout/error/pretty/token directive facts | 2C says directive payloads must carry through to `LayoutFacts` consumers (`restart/audit/totality/p2/2C-grammar-neutrality.md:73`); the transfer table requires grammar/directive facts for direct/fact sinks (`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:68-75`). |
| Any of the three | hot byte-disjoint hub on admitted hardware | `CollapsedStage` only as a generated transient emitted strategy, never a retained sidecar | feature gate, scalar oracle, checkasm/parity, local temporary lifetime, same-wave measured consumer | 2D limits `CollapsedStage` to concrete transient emitted strategies with local temporary state (`restart/audit/totality/p2/2D-cost-model.md:92-108`); SK-V13 blocks sidecars and old cascade fallback (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:223-235`). |

### Primitive Vocabulary Transfer

| primitive family | CSS L4 transfer | Sheets transfer | BBNF-self transfer | hard gate |
|---|---|---|---|---|
| Byte-set classify / run-skip | delimiters, comments, identifiers, at-rule starts | separators, operators, references, quotes | punctuation, directive starts, identifiers | Caller or generated grammar supplies alphabet and quote/comment policy; JSON structural bytes in shared code are a Lock 14 leak (`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:68-75`). |
| String / escape scan | CSS strings, URLs, escaped identifiers, custom properties | doubled quotes or grammar-specific no-backslash policy | literals and escapes | Requires generated quote, escape, control, and terminator policy; JSON `\uXXXX` is not universal (`restart/audit/totality/p2/2C-grammar-neutrality.md:104-111`). |
| Digit / number scan | numbers, dimensions, percentages, `calc()` | numeric literals and reference-adjacent digits | numeric literals | Requires number grammar, sign/exponent/suffix/unit policy, scalar reference, and same-wave consumer if SIMD is claimed (`restart/audit/totality/p2/2B-primitive-vocabulary.md:151-160`). |
| Direct/fact sink | declaration, selector, stylesheet, and visual-function facts | formula/function/reference facts | grammar/directive facts | Sink callbacks and fact schema are generated per grammar; `JsonSink` is not a generic contract (`restart/audit/totality/p2/2C-grammar-neutrality.md:151-154`). |
| Regex/HIR facts | selector/value recognizers where legal | formula token recognizers | grammar token/literal recognizers | Compile-time HIR, nullability, first-set, and char-class facts feed resolver; opaque JSON pattern strings do not (`restart/audit/totality/p2/2F-parse-that-gaps.md:162-187`). |
| BackendShape resolver | selector/value/declaration shapes | formula/reference shapes | rule/expression/directive shapes | Resolver consumes generated FIRST/follow, layout, host, recovery, output mode, and cost facts; the P1-P8 cascade cannot silently admit (`restart/audit/totality/p2/2C-grammar-neutrality.md:104-111`, `restart/skinny/tranches/sk-v13/HANDOFF.md:166-168`). |
| SIMD / ASM primitives | CSS scan-block or value-row consumers | numeric/reference consumers | token/literal consumers | Every primitive row records scalar reference, strict checkasm/parity, hardware gate, policy owner, same-wave consumer, and row movement or measured rejection (`restart/audit/totality/p2/2E-host-arch-esoterica.md:115-139`). |

## Future-Grammar Onboarding Test

Every future grammar must pass this test before any fleet-wide generality claim:

1. Add only `<name>.bbnf`, one `[workspace.metadata.bbnf.grammars.<name>]`
   block, and optional declaration-crate host functions only when the Lock 14
   exception form is satisfied. Lock 14's current text permits those three
   declarative surfaces and forbids generic-crate branches (`restart/locks/LOCKS.md:78`);
   the architecture YAML probe shows the metadata/build gate shape (`restart/ARCHITECTURE.md:1754-1762`).
2. Regenerate provider manifest, config/fact tables, sink/value/view surfaces,
   path schema, diagnostics, and tests. The generic-crate diff must be empty
   except generated runtime output under `runtime/src/grammars/<name>`
   (`restart/ARCHITECTURE.md:1756-1761`).
3. Run the Lock 14 leak scan over names and shapes: grammar names, JSON byte
   alphabets, object/array/pair roles, `JsonSink` callback names, and flag
   meanings (`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:53-65`).
4. Emit a five-shape eligibility report: one fixture per reachable shape, or a
   generated reason a shape is unreachable for this grammar (`restart/audit/totality/p2/2C-grammar-neutrality.md:157-168`).
5. For any shared primitive, attach a primitive-policy manifest with alphabet,
   quote/escape/control, string/no-string, number/no-number, scalar oracle,
   parity/checkasm, same-wave consumer, and row gate (`restart/audit/totality/p2/2B-primitive-vocabulary.md:196-220`).
6. If the grammar is used as a generality proof, pair a positive row with a
   negative-control witness. CSS L4 is the mandatory positive lane for the
   current cycle; Sheets or BBNF-self must fail closed under JSON-shaped mining
   and pass only after generated facts replace it (`restart/audit/totality/p2/2C-grammar-neutrality.md:168`).
7. Fail closed if onboarding requires a new directive, BIR variant,
   `BackendShape`, public substrate API, retained sidecar, or hand-coded generic
   behavior. SK-V13 carries the same refusal rule for downstream planning
   (`restart/skinny/tranches/sk-v13/HANDOFF.md:146-168`).

## Lock 14 Hardening Clauses For 3C

| clause | proposed Lock 14 hardening text | evidence chain |
|---|---|---|
| L14-HC-01 generated provider manifest | Generic crates may consume a generated provider manifest, but may not hand-code provider enums, provider arrays, root aliases, grammar-name branches, or per-grammar features. | 2C LAC-2C-01 (`restart/audit/totality/p2/2C-grammar-neutrality.md:184`); T-P1 codegen provider leak (`restart/audit/totality/p1/1B-codegen-evidence.md:47-58`); T-P1 runtime root leak (`restart/audit/totality/p1/1C-runtime-evidence.md:75-85`). |
| L14-HC-02 generated sink/fact/value/flag surface | Direct sinks, fact streams, value views, and flag meanings are generated grammar-owned surfaces. Generic tape may store compact bits, but it must not name grammar semantics such as `HAS_ESC` as universal. | 2C LAC-2C-03 (`restart/audit/totality/p2/2C-grammar-neutrality.md:186`); addendum transfer contract (`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:59-65`); flag refutation (`restart/audit/totality/p2/2C-grammar-neutrality.md:151-154`). |
| L14-HC-03 grammar-shape census | Lock 14 verification must scan for grammar-shaped policy, not only literal names: JSON structural alphabets, object/array/pair/string/number/bool/null roles, hardcoded sink callbacks, and flag meanings. | 2C LAC-2C-02 (`restart/audit/totality/p2/2C-grammar-neutrality.md:185`); T-P1 name-vs-shape distinction (`restart/audit/totality/p1/1E-locks-evidence.md:112-118`); pass role-mining divergence (`restart/audit/totality/p1/1B-codegen-evidence.md:49-50`). |
| L14-HC-04 primitive policy ownership | Shared primitives receive alphabets, delimiters, quote/escape/control, string, number, and no-string/no-number policy from generated grammar data or caller data. Shared primitive crates do not own grammar policy. | V2 addendum transfer table (`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:68-75`); 2B Lock 14 fold (`restart/audit/totality/p2/2B-primitive-vocabulary.md:53-60`); 2E transfer note (`restart/audit/totality/p2/2E-host-arch-esoterica.md:243-253`). |
| L14-HC-05 CSS plus negative-control closure | A fleet-wide grammar-neutrality claim requires at least one strict CSS L4 row plus Sheets or BBNF-self witness/negative-control. The SK-V12 declaration-values row is admitted evidence, not full CSS parity or universal closure. | 2C contract (`restart/audit/totality/p2/2C-grammar-neutrality.md:95-96`, `restart/audit/totality/p2/2C-grammar-neutrality.md:168`); SK-V13 CSS goal (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:38-57`); CSS parity matrix (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:127-132`). |
| L14-HC-06 resolver-generated shape facts | Backend-shape rewrites, CSP constraints, and cost guards consume generated grammar facts. A hardcoded cascade or JSON role miner is Lock 14 drift even when JSON equality passes. | 2C LAC-2C-04 (`restart/audit/totality/p2/2C-grammar-neutrality.md:187`); 2D LAC-2D-03 (`restart/audit/totality/p2/2D-cost-model.md:188-191`); SK-V13 cascade fail-closed rule (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:232-235`). |
| L14-HC-07 fact streams are output planes | Fact streams are valid admitted output planes only with strict comparator/oracle provenance and gate-consumed telemetry. They are not hidden retained sidecars and do not create a sixth `BackendShape`. | V2 addendum substrate contract (`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:96-97`); T-P1 CSS fact-stream category gap (`restart/audit/totality/p1/1C-runtime-evidence.md:102-113`); CSS row evidence (`skinny/RESULTS.md:94`). |
| L14-HC-08 generated-output allowance fence | Generated files under `runtime/src/grammars/<name>/` may contain grammar names only if produced by the rostered generator and guarded by Lock 14 validation; handwritten per-grammar runtime files remain forbidden unless G-Omega amends the lock. | 1E LAC-1E-08 (`restart/audit/totality/p1/1E-locks-evidence.md:107`); architecture generated surface rule (`restart/ARCHITECTURE.md:417-419`, `restart/ARCHITECTURE.md:1791-1801`); current hand-written runtime drift (`restart/audit/totality/p1/1C-runtime-evidence.md:58-60`). |

## Proposed Delta Table

| proposed delta | source T-P1/T-P2 finding-id cited | affected V1-surface section | rationale |
|---|---|---|---|
| 3E-D01: Add the CSS/Sheets/BBNF-self `BackendShape` matrix above as the canonical non-JSON companion to `ARCHITECTURE.md` section 7.3. | 2C-BACKENDSHAPE-FIVE, 2C-BACKENDSHAPE-SELECTION, P1-1B-D3/D4/D5, SKINNY-GEN-009/010. | `restart/ARCHITECTURE.md:1033-1108`; `restart/locks/LOCKS.md:70`, `restart/locks/LOCKS.md:78`. | The five-shape vocabulary is live, but selection must be proved by generated facts across non-JSON grammars, not JSON-only tests (`restart/audit/totality/p1/1B-codegen-evidence.md:36-47`, `restart/audit/totality/p2/2C-grammar-neutrality.md:137-145`). |
| 3E-D02: Reword the fixed eight-step `derive_backend_shape` order as a resolver pipeline: generate candidates, saturate/normalize legal alternatives, filter feasibility, extract by active cost, then emit diagnostics. | LAC-2D-01, LAC-2D-02, 2C-BACKENDSHAPE-SELECTION, COH-004. | `restart/ARCHITECTURE.md:1090-1098`; Lock 10 at `restart/locks/LOCKS.md:70`. | 2D refutes the fixed order as universal while preserving finite shape selection (`restart/audit/totality/p2/2D-cost-model.md:57-68`, `restart/audit/totality/p2/2D-cost-model.md:155-168`). |
| 3E-D03: Add generated provider manifest and generated sink/fact/value/flag ownership to the Lock 14 amendment queue. | LAC-2C-01, LAC-2C-03, 1C-D1/D2, D-1E-09. | Lock 14 at `restart/locks/LOCKS.md:78`; generated runtime rule at `restart/ARCHITECTURE.md:417-419`. | Current codegen/runtime still hardcodes provider/root grammar names, while 2C identifies generated manifests and grammar-owned sink/flag surfaces as the repair (`restart/audit/totality/p2/2C-grammar-neutrality.md:128-135`). |
| 3E-D04: Add primitive vocabulary transfer manifest fields to the Lock 14/Lock 16 bridge: `abstract_primitive`, generated policy, scalar oracle, parity/checkasm, same-wave consumer, output plane, row movement or measured rejection. | LAC-2C-05, T2A-LAC-03, LAC-2F-02, LAC-1E-10. | Lock 14 and Lock 16 at `restart/locks/LOCKS.md:78`, `restart/locks/LOCKS.md:87-112`. | T-P2 converged that primitive parity alone is not admission and that shared primitives need generated grammar policy plus row consumers (`restart/audit/totality/p2/2B-primitive-vocabulary.md:38-46`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:115-139`). |
| 3E-D05: Classify CSS fact streams as admitted output planes, not retained sidecars and not a sixth `BackendShape`. | 1A-DIV-006, 1C-D5, 2C-CSS-FACT-STREAM, T2A-LAC-04. | Runtime substrate taxonomy in `restart/ARCHITECTURE.md:1025-1031`; Lock 1/14 in `restart/locks/LOCKS.md:52`, `restart/locks/LOCKS.md:78`. | CSS declaration-values is admitted evidence with strict same-plane telemetry, but V1 lacks a formal category for that product row (`skinny/RESULTS.md:94`, `restart/audit/totality/p1/1A-substrate-evidence.md:45-46`). |
| 3E-D06: Adopt the future-grammar onboarding test as the Lock 14 close gate for arbitrary grammars. | 2C future-grammar onboarding test, 1F COH-002, LAC-1E-08. | `restart/ARCHITECTURE.md:1754-1762`; Lock 14 at `restart/locks/LOCKS.md:78`; future HANDOFF wording. | T-P2 requires source/metadata/declaration-crate-only onboarding, generated surfaces, leak scans, five-shape eligibility, primitive consumer proof, and telemetry before generality claims (`restart/audit/totality/p2/2C-grammar-neutrality.md:157-168`). |
| 3E-D07: Make CSS L4 plus Sheets/BBNF-self negative controls mandatory before "fleet-wide grammar-neutral" wording may be used. | 2C-CSS-SELECTOR-SCOPE, 2C-SHEETS-FORMULA-FALSIFIER, 2C-BBNF-SELF-FALSIFIER, SKINNY-GEN-009/010. | Lock 14; SK-V13/S-P3 gate language; `MASTER-PLAN.md` future wave acceptance criteria. | SK-V13 requires full CSS parity or architectural block, and T-P2 says Sheets/BBNF-self expose JSON role-mining overfit (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:38-57`, `restart/audit/totality/p2/2C-grammar-neutrality.md:72-79`). |
| 3E-D08: Add the grammar-shape leak census to the generic-crate validation command set. | LAC-2C-02, AP-003/AP-006, D-1E-09. | Lock 14 verification commands at `restart/locks/LOCKS.md:78`; diagnostic catalogue around `restart/ARCHITECTURE.md:1135-1144`. | Existing verification catches names but misses shape policy; P1 found JSON punctuation recognizers and object/array/pair role mining without relying on literal grammar names (`restart/audit/totality/p1/1F-anti-pattern.md:47-58`, `restart/audit/totality/p1/1E-locks-evidence.md:112-118`). |

## Consequences

| delta | positive consequence | cost / risk / wave | propagation |
|---|---|---|---|
| 3E-D01 | Makes Lock 14 concrete for CSS L4, Sheets, and BBNF-self while preserving the five-shape canon. | 180-320 doc LOC, medium risk, T-P3/Omega architecture fold. | 3 surfaces: `ARCHITECTURE.md`, `LOCKS.md`, S-P3 gates. |
| 3E-D02 | Reconciles T-P1's live priority drift with T-P2's decision-engine research. | 80-180 doc LOC plus later resolver code, medium-high risk, decision-engine wave. | 3 surfaces: `ARCHITECTURE.md`, `LOCKS.md`, SK-V13 G2. |
| 3E-D03 | Turns provider/sink/flag ownership from prose into a Lock 14 validator target. | 350-900 implementation LOC later, high risk, Lock 14 registry/runtime wave. | 4 surfaces: `LOCKS.md`, `ARCHITECTURE.md`, codegen/runtime plan, tests. |
| 3E-D04 | Prevents support-only SIMD/primitive paper-close. | 200-600 manifest/tooling LOC later, medium-high risk, Lock 16 primitive-admission wave. | 4 surfaces: `LOCKS.md`, `BENCH`, `bbnf-simd` gates, S-P3 gates. |
| 3E-D05 | Preserves the SK-V12 CSS win without inventing a parallel substrate. | 100-300 doc/report LOC, medium risk, substrate taxonomy fold. | 4 surfaces: `ARCHITECTURE.md`, `LOCKS.md`, `BENCH`, `RESULTS` schema. |
| 3E-D06 | Gives arbitrary grammar onboarding a falsifiable gate. | 120-260 doc/test LOC now; higher generated-fixture cost later, medium risk. | 3 surfaces: `ARCHITECTURE.md`, `HANDOFF.md`, `LOCKS.md`. |
| 3E-D07 | Blocks JSON-only closure language and makes non-JSON witnesses required. | 80-180 doc LOC, medium risk, S-P3/MASTER wave criteria. | 4 surfaces: `LOCKS.md`, `MASTER-PLAN.md`, SK-V13 plan, challenge gates. |
| 3E-D08 | Makes grammar-shaped overfit visible to CH2 even when `rg Json` passes. | 180-420 lint/report LOC later, high risk, Lock 14 census wave. | 4 surfaces: `LOCKS.md`, diagnostics, CI/gates, S-P3 validation. |

## Open Questions

| lens | question | receiver / blocker / gate |
|---|---|---|
| CH1 correctness | `ARCHITECTURE.md` section 7.4 still contains stale prose about `shapes_for_json()` and `nominate_json`, while T-P1 says live skinny has moved past symbol absence but still carries provider and role leaks. | Receiver: 3A/Pass Omega. Blocker: 3E cannot edit `ARCHITECTURE.md`. Gate: Omega CRUD must reconcile `restart/ARCHITECTURE.md:1129-1131` with `restart/audit/totality/p1/1B-codegen-evidence.md:48-50`. |
| CH2 generality | Should the formal spec name CSS fact streams as `SinkOnly` products, or as a distinct output-plane taxonomy that does not expand `BackendShape`? | Receiver: 3A + 3C. Blocker: current T-P1 evidence calls it a category gap. Gate: accepted wording must preserve five shapes and cite `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:96-97`. |
| CH4 cost | What exact generated provider-manifest layout replaces `RuntimeProvider` with the smallest public API change? | Receiver: S-P3/Lock 14 registry wave. Blocker: T-P2 left manifest layout open. Gate: add CSS plus Sheets or BBNF-self provider without editing generic code (`restart/audit/totality/p2/2C-grammar-neutrality.md:172-176`). |
| CH5 hidden coupling | How will the CSS lightningcss source sidecar stay comparator-only once more CSS rows are generated? | Receiver: S-P3 CSS rows + BENCH/Omega. Blocker: current same-plane sidecar is valid evidence but not runtime substrate. Gate: every CSS row emits output-plane provenance and no runtime dependency on comparator sidecars (`restart/audit/totality/p1/1F-anti-pattern.md:38-40`). |
| CH6 anti-paper-close | Which first concrete negative-control row should carry Sheets/BBNF-self: generated role-fact proof, formula/directive fact stream, or full parser row? | Receiver: S-P3 or next Omega-approved planning wave. Blocker: T-P2 requires a negative-control witness but does not choose fixture scope. Gate: fail-closed row with no generic code edits and explicit JSON-role-mining rejection (`restart/audit/totality/p2/2C-grammar-neutrality.md:157-168`). |
