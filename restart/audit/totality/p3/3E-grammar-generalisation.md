---
agent: 3E
pass: T-P3-synthesis
cycle: V4
generated_at: 2026-05-23T00:00:00Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
t_p2_dossiers_consumed: [2A, 2B, 2C, 2D, 2E, 2F]
v1_surface_targeted: n/a
proposed_deltas_count: 12
delta_summary:
  carried_from_prior_cycle: [3E-D01, 3E-D02, 3E-D03, 3E-D04, 3E-D05, 3E-D06, 3E-D07, 3E-D08]
  removed: []
  answered: []
  newly_added: [3E-D09, 3E-D10, 3E-D11, 3E-D12]
prior_cycle_dispositions_folded:
  accepted:
    - V3 CH1 stable section/path:line provenance
    - V3 CH3 5-shape canon preserved across 3A/3B/3E
    - V3 CH4 receiver/wave-aligned cost ledger
    - V3 CH5 substrate-union non-violation across 3E-D01..D08
    - V3 CH6 receiver/blocker/gate on D06 generated-fixture tail
  rejected: []
  revised:
    - V3 CH2: 2C V4 expanded CSS L4 to 15 sub-grammars (not 9); V3 matrix is 5×~9 and must broaden to 5×15 sub-grammar rows plus Sheets/BBNF-self/EBNF/BNF/CSV/math
    - V3 CH2: T-P1 1B-D8/D10 + 1C 30/127 leaks surface explicit Lock 14 hardening clauses (V3 L14-HC-01..08 do not enumerate the V3→V4 RuntimeProvider 2→8 enum drift, the 30-parser-name 15-file census, the 127 grammar-named reexport census, the LAC-2C-02 `passes/src/lib.rs:1059/1079/1102` JSON-canonical label sites, the LAC-2C-03 FlagSchema generated table, the LAC-2B-03 `policy_owner` field, or the LAC-2F-V5-03 `byte_class_from_range_64` abstract-primitive sibling)
---

## Executive Summary

The non-JSON generality story is mechanical: keep the five `BackendShape`
variants, but make every selection, primitive policy, sink, flag, and provider
surface generated from grammar source plus workspace metadata. CSS L4 is the
positive proof lane (15 sub-grammars live at `grammar/css/l4/*.bbnf` per
2C V4 §Executive Summary; the SK-V12 declaration-values row is evidence, not
parity per `skinny/RESULTS.md:94` + `restart/skinny/tranches/sk-v13/SYNTHESIS.md:38`-`57`).
Sheets and BBNF-self are negative controls because JSON object/member/value role
mining cannot model formulas, references, directives, or Pratt/operator chains
(`restart/audit/totality/p2/2C-grammar-neutrality.md:114`-`115`,
`restart/audit/totality/p1/1B-codegen-evidence.md:71`-`72`). T-P2 V4 converged on
the same rule: primitives are grammar-neutral byte-stream operations, but their
alphabets, quote/escape/control policy, number policy, output facts, and costs
come from generated data (`restart/audit/totality/p2/2C-grammar-neutrality.md:122`-`125`,
`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:53`-`75`). V4 broadens the
matrix to 5 shapes × 15 CSS L4 sub-grammars + 6 other-grammar rows, and surfaces
four additional Lock 14 hardening clauses (L14-HC-09 through L14-HC-12) covering
the V3→V4 `RuntimeProvider` 2→8-variant enum drift, the pass-layer JSON-byte/
literal mining sites (1B-D8 byte whitelist + 1B-D10 role mining), the 30
parser-name leaks across 15 files in `crates/core/src/runtime/`, the 127
grammar-named reexports at `crates/core/src/runtime/mod.rs:25-71`, the
LAC-2C-02 JSON-canonical label leak at `passes/src/lib.rs:1059/1079/1102`, the
LAC-2C-03 `FlagSchema` generated table, the LAC-2B-03 `policy_owner` field, the
LAC-2F-V5-03 `byte_class_from_range_64` sibling primitive, and the LAC-2B-07
atomic close-state vocabulary. V4 does not edit V1 spec surfaces.

## V4 Delta Summary

| bucket | disposition |
|---|---|
| carried from prior cycle | 3E-D01..D08 carry unchanged with V3 cost-and-routing ledger preserved. |
| removed | None. |
| answered | None from a prior T-P3 cycle. V4 answers V3 CH2 by enumerating 5×15 CSS L4 + 6 other-grammar shape rows and by surfacing four additional Lock 14 hardening clauses warranted by T-P1 1B-D8/D10 + 1C 30/127 + 2C V4 (`restart/audit/totality/p2/2C-grammar-neutrality.md:100`-`106`, `restart/audit/totality/p1/1C-runtime-evidence.md:23`-`24`, `restart/audit/totality/p1/1B-codegen-evidence.md:63`-`64`). |
| newly added | 3E-D09 through 3E-D12 below. |

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

T-P1 1B and 1C also recorded two binding upstream leaks the V3 matrix did not
expose row-locally: the pass-layer JSON-byte recognizer whitelist `{ } [ ] , : "`
at `skinny/crates/passes/src/lib.rs:331` inside `derive_recognizers` (P1-1B-D8;
upstream blocker for Sheets/BBNF-self for the recognizer plane) and the
materialization-role JSON-literal census at `skinny/crates/passes/src/lib.rs:1300`-`1391`
inside `derive_materialization_roles` (P1-1B-D10; upstream blocker for the
role plane). Both are distinct from the codegen-layer `RuntimeProvider` enum
leak; both must be repaired before any non-JSON grammar can ride the existing
materialization plan without generic-crate edits
(`restart/audit/totality/p1/1B-codegen-evidence.md:71`-`72`,
`restart/audit/totality/p1/1B-codegen-evidence.md:86`-`87`). LAC-2C-02 names the
sibling JSON-canonical label leak inside `derive_materialization_roles`
output at `passes/src/lib.rs:1059`, `:1079`, `:1102` even though `TapeKind` is
grammar-neutral (`restart/audit/totality/p2/2C-grammar-neutrality.md:101`).

### BackendShape Matrix — 5 Shapes × 15 CSS L4 Sub-Grammars

The CSS L4 workspace lives at `grammar/css/l4/` and enumerates 15 sub-grammars
per 2C V4 §Executive Summary (`restart/audit/totality/p2/2C-grammar-neutrality.md:50`-`56`):
`color`, `easing`, `filters`, `func-body`, `gradients`, `keyframes`, `keywords`,
`media`, `properties`, `selectors`, `stylesheet`, `tokens`, `transforms`,
`value-unit`, `values`. The HEAD `RuntimeProvider` enum carries 8 variants
covering only a subset (declaration_values + declaration_values_extended +
stylesheet_selectors + visual_functions + at_rules_and_media +
vendor_and_custom_at_rules + nested_layout, plus `Json`) per
`skinny/crates/codegen/src/grammar_profile.rs:17`-`26`; the remaining sub-grammar
rosters are not yet emitted, which is itself a generator-stub gap per
2C V4 §Technique Grounding Table (`restart/audit/totality/p2/2C-grammar-neutrality.md:112`).

Each row gives the dominant `BackendShape` the resolver should select per the
2D pipeline (candidate generation → equality saturation → CSP feasibility →
active cost extraction); generated facts required for selection; and a single
T-P1/T-P2/V1-surface evidence anchor. `CollapsedStage` is the rare admitted
transient emitted strategy; per 2D `CollapsedStage` is x86-only and forbids
retained sidecars (`restart/audit/totality/p2/2D-cost-model.md:92`-`108`).

| css_l4 sub-grammar | dominant `BackendShape` | secondary shapes admitted | generated facts required | evidence |
|---|---|---|---|---|
| `tokens.bbnf` (token alphabet, hash, ident, function, string, URL, number, percentage, dimension, delimiter, at-keyword) | `OffsetTape` | `EventTape` for retained token visitor | byte alphabet (CSS Syntax §4.3), comment/whitespace policy, string-quote/escape policy | 2C-CSS-TOKEN-ALPHABET grounded with generator stub (`restart/audit/totality/p2/2C-grammar-neutrality.md:111`); CSS Syntax §4.3 cited. |
| `stylesheet.bbnf` (root, at-rule, qualified-rule dispatch) | `OffsetTape` when FIRST sets disjoint | `EventTape` for retained facts | dispatch-hub FIRST/follow, layout policy, at-rule starts | HEAD provider `CssL4StylesheetSelectors` at `grammar_profile.rs:21`; SK-V13 missing-row note (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:96`-`120`). |
| `selectors.bbnf` (compound, complex, combinator, pseudo-class, pseudo-element, attribute) | `EagerTape` | `EventTape` if retained selector tree required | selector FIRST/follow, combinator policy, pseudo payload facts, recovery | 2C-CSS-SELECTOR-SCOPE refuted as JSON role-mining target (`restart/audit/totality/p2/2C-grammar-neutrality.md:112`); SK-V13 selectors missing. |
| `properties.bbnf` (longhand declaration, shorthand expansion) | `EventTape` | `SinkOnly` for admitted CSS fact-stream | property-name payload enum, important flag, strict comparator provenance | 2C-CSS-FACT-STREAM grounded (`restart/audit/totality/p2/2C-grammar-neutrality.md:122`); SK-V12 admitted row (`skinny/RESULTS.md:94`). |
| `values.bbnf` (general value grammar, comma list, slash list) | `EagerTape` | `EventTape` when retained value tree feeds visitor | number/dimension policy, function-family facts, custom-property and substitution policy | 2C-CSS-CALC-VAR grounded with generator stub (`restart/audit/totality/p2/2C-grammar-neutrality.md:113`). |
| `value-unit.bbnf` (number+unit, percentage, dimension classification) | `EagerTape` | n/a | unit suffix policy, percentage policy, CSS Values L4 dimensional rules | 2C V4 cites CSS Values L4 (`restart/audit/totality/p2/2C-grammar-neutrality.md:113`). |
| `keywords.bbnf` (keyword tokens by class) | `OffsetTape` | n/a | keyword set per class, case sensitivity | 2C V4 token-alphabet grounding extends (`restart/audit/totality/p2/2C-grammar-neutrality.md:111`). |
| `color.bbnf` (named, hex, rgb/rgba, hsl/hsla, hwb, lab, lch, color()) | `EagerTape` | `EventTape` for retained color tree | color-function family facts, hex/rgb/hsl alphabet, numeric/percentage policy | 2C V4 visual-function provider `CssL4VisualFunctions` at `grammar_profile.rs:22` (`restart/audit/totality/p2/2C-grammar-neutrality.md:118`); SK-V13 color partial. |
| `gradients.bbnf` (linear, radial, conic gradients + repeating variants) | `EagerTape` | `EventTape` for retained gradient tree | gradient-family facts, color-stop policy, angle/length policy | SK-V13 missing/partial (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:104`-`108`). |
| `transforms.bbnf` (translate, rotate, scale, matrix, perspective) | `EagerTape` | `EventTape` for retained transform tree | transform-function family facts, length/angle policy | SK-V13 missing/partial; CSS Transforms reference. |
| `filters.bbnf` (blur, brightness, contrast, drop-shadow, etc.) | `EagerTape` | `EventTape` | filter-function family facts, length/percentage policy | SK-V13 missing/partial. |
| `easing.bbnf` (cubic-bezier, steps, linear) | `EagerTape` | n/a | easing-function family facts, numeric/keyword policy | SK-V13 missing/partial. |
| `func-body.bbnf` (generic function body, `var()`, `calc()`, `env()`, `attr()`) | `EagerTape` | `EventTape` when retained calc tree feeds visitor | substitution-function facts, math notation, custom-property policy | 2C-CSS-CALC-VAR grounded with stub (`restart/audit/totality/p2/2C-grammar-neutrality.md:113`); CSS Custom Properties Level 1 cited. |
| `keyframes.bbnf` (@keyframes block, percentage/from/to selector) | `OffsetTape` | `EventTape` | at-rule dispatch, percentage/keyword selector facts | HEAD provider `CssL4AtRulesAndMedia` at `grammar_profile.rs:23` (`restart/audit/totality/p2/2C-grammar-neutrality.md:118`). |
| `media.bbnf` (@media + feature query) | `OffsetTape` | `EventTape` | media-query feature facts, range syntax | HEAD provider `CssL4AtRulesAndMedia` at `grammar_profile.rs:23`. |

`CollapsedStage` per 2D is x86-only and is not admitted for any current CSS L4
sub-grammar without a same-wave consumer that deletes the prior scalar cost
source (`restart/audit/totality/p2/2D-cost-model.md:92`-`108`,
`restart/audit/totality/p2/2C-grammar-neutrality.md:75`).

### BackendShape Matrix — Other Grammars

| grammar | rule or product | proposed `BackendShape` | generated facts required | evidence |
|---|---|---|---|---|
| Sheets | `formula` / `cellRef` / `primary` / reference + range atoms | `OffsetTape` | cell/range grammar, reference operator facts, separator and quote policy (doubled-`""`) | 2C-SHEETS-FORMULA-FALSIFIER refuted (`restart/audit/totality/p2/2C-grammar-neutrality.md:114`); prior matrix gives `OffsetTape` (`restart/HANDOFF.md:234`-`236`). |
| Sheets | function calls, `LET`, `LAMBDA`, array literals | `EventTape` | function-name DFA/payload facts, semicolon parameter policy, array-literal facts, oracle schema | 2C V4 transfer requires generated function/reference/operator role facts (`restart/audit/totality/p2/2C-grammar-neutrality.md:114`). |
| Sheets | infix expression | `EagerTape` (Pratt) | operator precedence/associativity facts, Pratt eligibility, strict formula oracle | T-P1 Sheets cannot rely on JSON roles (`restart/audit/totality/p1/1B-codegen-evidence.md:71`); Lock 10 auto-detect Pratt (`restart/locks/LOCKS.md:70`). |
| BBNF-self | grammar / declaration / term dispatch | `OffsetTape` | directive starts, identifier/literal policy, alternation/repetition facts | 2C-BBNF-SELF-FALSIFIER (`restart/audit/totality/p2/2C-grammar-neutrality.md:115`); prior matrix gives `OffsetTape` (`restart/HANDOFF.md:231`-`233`). |
| BBNF-self | expression / operator chain | `EagerTape` (Pratt) | precedence/associativity facts, recursion bounds, operator-token facts | T-P1 BBNF-self proof beyond JSON role mining (`restart/audit/totality/p1/1B-codegen-evidence.md:72`); Lock 10 forbids `@pratt` (`restart/locks/LOCKS.md:70`). |
| BBNF-self | directives and generated grammar facts | `EventTape` (retained) + `SinkOnly` (direct fact output) | directive-kind enum, argument schema, layout/error/pretty/token directive facts | 2C V4 directive payloads must reach `LayoutFacts` consumers (`restart/audit/totality/p2/2C-grammar-neutrality.md:115`). |
| BBNF-self | literal `\u`+4-nibble | `EagerTape` (shape-identical to JSON `\uXXXX`) | escape policy (fixed-width); admitted via C4 shape-identical | 2C V4 §C4 worked example (`restart/audit/totality/p2/2C-grammar-neutrality.md:220`-`248`); ADMITTED-VIA-C4-W10 binding. |
| EBNF / BNF | rule / alternation / repetition | `OffsetTape` | dispatch-hub FIRST, terminal/non-terminal policy | 2C V4 per-grammar selection table DEFER (`restart/audit/totality/p2/2C-grammar-neutrality.md:183`-`184`). |
| CSV | record / field / quoted-field | `OffsetTape` | delimiter policy (locale-permissive), quote-doubling policy | 2C V4 per-grammar selection table DEFER (`restart/audit/totality/p2/2C-grammar-neutrality.md:185`); CSV grammar workspace. |
| math | expression / operator | `EagerTape` (Pratt) | precedence/associativity facts | 2C V4 per-grammar selection table DEFER (`restart/audit/totality/p2/2C-grammar-neutrality.md:186`). |
| Any of CSS/Sheets/BBNF-self | byte-disjoint hub on admitted hardware | `CollapsedStage` only as transient emitted strategy, never a retained sidecar | feature gate, scalar oracle, checkasm/parity, local temporary lifetime, same-wave measured consumer | 2D `CollapsedStage` x86-only with local-temp lifetime (`restart/audit/totality/p2/2D-cost-model.md:92`-`108`); SK-V13 blocks sidecars + cascade fallback (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:223`-`235`). |

### Primitive Vocabulary Transfer (per 2C V4 Per-Technique Transfer Coverage)

| primitive family | CSS L4 transfer | Sheets transfer | BBNF-self transfer | hard gate |
|---|---|---|---|---|
| Byte-set classify / run-skip (`byte_class_from_eq_set_64`) | delimiters, comments, identifiers, at-rule starts, hash, function | separators, operators, references, quote policy | punctuation, directive starts, identifiers | Caller or generated grammar supplies alphabet and quote/comment policy; JSON structural bytes inlined at `skinny/crates/runtime/src/grammars/json/config.rs:4` are a Lock 14 leak until generated CSS/Sheets/BBNF-self alphabet siblings exist (`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:68`-`75`). |
| Byte-range classify (`byte_class_from_range_64`, LAC-2F-V5-03 sibling) | CSS hex `[0-9a-fA-F]`, identifier ranges | numeric-literal ranges | identifier ranges | Sibling of admitted `_eq_set_64`; range primitive is the generalization vehicle for digit-run / UTF-8 continuation / CSS hex / BBNF identifier classification per LAC-2F-V5-03 (`restart/audit/totality/p2/2F-parse-that-gaps.md:520`). |
| String / escape scan (`escape_mask_64`, `string_context_64`) | CSS strings, URLs, escaped identifiers (variable-width `\HEXHEX`) | doubled-`""` policy (no backslash) | shape-identical `\\.` + `\u`+4-nibble | Requires generated quote, escape, control, and terminator policy; JSON `\uXXXX` is not universal — CSS variable-width is shape-orthogonal carve-out per 2C V4 §C4 (`restart/audit/totality/p2/2C-grammar-neutrality.md:235`-`248`). |
| Digit / number scan (`digit_run_accumulate_udot`, C3) | numbers, dimensions, percentages, `calc()` | numeric literals; scientific notation | rare numeric leaf (DEFER per 2C V4 per-grammar selection table) | Requires number grammar, sign/exponent/suffix/unit policy, scalar reference, and same-wave consumer if SIMD is claimed (`restart/audit/totality/p2/2B-primitive-vocabulary.md:151`-`160`); UDOT primitive non-shortlist until strict parity test exists per V4 ledger (`restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:51`). |
| Direct/fact sink | CSS declaration/selector/stylesheet/visual-function facts | formula/function/reference facts | grammar/directive facts | Sink callbacks and fact schema are generated per grammar; `JsonSink` is not a generic contract (`restart/audit/totality/p2/2C-grammar-neutrality.md:119`, `:331`). |
| Regex/HIR facts | selector/value recognizers where legal | formula token recognizers | grammar token/literal recognizers | Compile-time HIR, nullability, first-set, char-class facts feed resolver; opaque JSON pattern strings do not (`restart/audit/totality/p2/2F-parse-that-gaps.md:162-187`, LAC-2F-V5-04). |
| BackendShape resolver | selector/value/declaration shapes | formula/reference shapes | rule/expression/directive shapes | Resolver consumes generated FIRST/follow, layout, host, recovery, output mode, and cost facts; the P1-P8 cascade cannot silently admit (`restart/audit/totality/p2/2C-grammar-neutrality.md:117`, `restart/audit/totality/p2/2D-cost-model.md:155`-`168`). |
| Cross-chunk byte-context (`vextq_u8`) | CSS escaped-identifier boundary | Sheets quoted-sheet-name boundary | BBNF-self literal boundary | Lock 16 abstract-primitive declaration applies "to ANY grammar with chunk-spanning tokens"; admission requires same-wave consumer + measured row movement (`restart/audit/totality/p2/2C-grammar-neutrality.md:140`, `restart/locks/LOCKS.md:282`-`288`). |
| SIMD / ASM primitives | CSS scan-block or value-row consumers | numeric/reference consumers | token/literal consumers | Every primitive row records scalar reference, strict checkasm/parity, hardware gate, `policy_owner` (LAC-2B-03), same-wave consumer, and row movement or measured rejection (`restart/audit/totality/p2/2E-host-arch-esoterica.md:115`-`139`, `restart/audit/totality/p2/2B-primitive-vocabulary.md:529`). |

## Future-Grammar Onboarding Test (per 2C V4 7-step protocol)

Every future grammar must pass this test before any fleet-wide generality
claim. The protocol is canonised at
`restart/audit/totality/p2/2C-grammar-neutrality.md:344`-`405`:

1. **Grammar source + metadata only.** Add only `grammar/<name>/<name>.bbnf`,
   one `[workspace.metadata.bbnf.grammars.<name>]` block, and optionally a
   per-grammar declaration crate carrying host-fn implementations. NO edits to
   any generic crate. Lock 14 v+1 permits exactly those three declarative
   surfaces and forbids generic-crate branches (`restart/locks/LOCKS.md:222`-`238`);
   the YAML probe shape is at `restart/ARCHITECTURE.md:1754-1762`.
2. **Regenerate rostered surfaces.** Provider manifest, config/fact tables,
   sink/value/view surfaces, path schema, diagnostics, and tests are emitted
   without editing generic crates. The generic-crate diff must be empty except
   generated output under `runtime/src/grammars/<name>/` (`restart/ARCHITECTURE.md:1756-1761`).
3. **Grammar-name leak scan.** Run `rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser|<NewName>Parser' crates/{ir,parse,codegen,runtime,path,path-core,egraph,csp-solver,parse-that-regex,parse-that,bbnf-simd,analysis,lsp}/src/` per Lock 14 verification command (`restart/locks/LOCKS.md:220`); MUST return ZERO. HEAD currently returns **30 sites across 15 files** in `crates/core/src/runtime/{json,bbnf,css_l4,google_sheets}/` per `restart/audit/totality/p1/1C-runtime-evidence.md:125`.
4. **Grammar-shape leak scan.** Run `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>|<NewName>\w*\s*=>' crates/` per Lock 14 v+1 grammar-shape leak rule; MUST return ZERO new matches. Plus the LAC-2C-02 grammar-shape role census: JSON byte alphabets at `runtime/src/grammars/json/config.rs:4`; object/array/pair role mining at `skinny/crates/passes/src/lib.rs:1053`-`1110` (with `label: "object"/"array"/"pair"` at `:1059/:1079/:1102`); `JsonSink` callbacks at `skinny/crates/runtime/src/grammars/json/sink.rs:4`-`16`; JSON flag meanings (`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:53`-`65`).
5. **Five-shape eligibility fixture.** At least one rule per reachable `BackendShape` OR explicit generated reason a shape is unreachable for this grammar (`restart/audit/totality/p2/2C-grammar-neutrality.md:364`-`370`).
6. **Primitive same-wave non-JSON consumer.** Exercise at least one Layer-1 primitive through a same-wave consumer OR record a measured architectural-block per Lock 14 v+1 strict read. C3 + C4 are the worked examples (`restart/audit/totality/p2/2C-grammar-neutrality.md:194`-`268`).
7. **Telemetry/provenance consumed by gate.** Emit telemetry consumed by the gate in the same wave; row movement, equality verdict, substrate-kind classification must be gate-consumable (`restart/audit/totality/p2/2C-grammar-neutrality.md:379`-`382`).

Fail closed if onboarding requires a new directive, BIR variant, `BackendShape`,
public substrate API, retained sidecar, or hand-coded generic behavior. SK-V13
carries the same refusal rule for downstream planning
(`restart/skinny/tranches/sk-v13/HANDOFF.md:146`-`168`).

For SK-V14, CSS L4 (15 sub-grammars, full step 1-7 cycle) is mandatory per
2C V4 §CSS L4 mandatory; Sheets and BBNF-self are negative controls
(`restart/audit/totality/p2/2C-grammar-neutrality.md:426`-`431`).

## Lock 14 Hardening Clauses For 3C

V4 retains V3 L14-HC-01 through L14-HC-08 and adds L14-HC-09 through L14-HC-12.

| clause | proposed Lock 14 hardening text | evidence chain |
|---|---|---|
| L14-HC-01 generated provider manifest | Generic crates may consume a generated provider manifest, but may not hand-code provider enums, provider arrays, root aliases, grammar-name branches, or per-grammar features. | 2C LAC-2C-01 (`restart/audit/totality/p2/2C-grammar-neutrality.md:448`); T-P1 codegen provider leak (`restart/audit/totality/p1/1B-codegen-evidence.md:47`-`58`); T-P1 runtime root leak (`restart/audit/totality/p1/1C-runtime-evidence.md:75`-`85`). |
| L14-HC-02 generated sink/fact/value/flag surface | Direct sinks, fact streams, value views, and flag meanings are generated grammar-owned surfaces. Generic tape may store compact bits, but it must not name grammar semantics such as `HAS_ESC` as universal. | 2C LAC-2C-03 (`restart/audit/totality/p2/2C-grammar-neutrality.md:450`); addendum transfer contract (`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:59`-`65`); flag refutation (`restart/audit/totality/p2/2C-grammar-neutrality.md:332`). |
| L14-HC-03 grammar-shape census | Lock 14 verification must scan for grammar-shaped policy, not only literal names: JSON structural alphabets, object/array/pair/string/number/bool/null roles, hardcoded sink callback names, and flag meanings. | 2C LAC-2C-02 (`restart/audit/totality/p2/2C-grammar-neutrality.md:449`); T-P1 name-vs-shape distinction (`restart/audit/totality/p1/1E-locks-evidence.md:112`-`118`); pass role-mining divergence (`restart/audit/totality/p1/1B-codegen-evidence.md:49`-`50`). |
| L14-HC-04 primitive policy ownership | Shared primitives receive alphabets, delimiters, quote/escape/control, string, number, and no-string/no-number policy from generated grammar data or caller data. Shared primitive crates do not own grammar policy. | V2 addendum transfer table (`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:68`-`75`); 2B Lock 14 fold (`restart/audit/totality/p2/2B-primitive-vocabulary.md:53`-`60`); 2E transfer note (`restart/audit/totality/p2/2E-host-arch-esoterica.md:243`-`253`). |
| L14-HC-05 CSS plus negative-control closure | A fleet-wide grammar-neutrality claim requires at least one strict CSS L4 row plus Sheets or BBNF-self witness/negative-control. The SK-V12 declaration-values row is admitted evidence, not full CSS parity or universal closure. With only one of Sheets or BBNF-self, the claim is scoped to the witnessed grammars and may not use fleet-wide grammar-neutral wording. | 2C contract (`restart/audit/totality/p2/2C-grammar-neutrality.md:104`); per-wave gate enforcement (`restart/locks/LOCKS.md:246`-`250`); SK-V13 CSS goal (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:38`-`57`). |
| L14-HC-06 resolver-generated shape facts | Backend-shape rewrites, CSP constraints, and cost guards consume generated grammar facts. A hardcoded cascade or JSON role miner is Lock 14 drift even when JSON equality passes. | 2C LAC-2C-04 (`restart/audit/totality/p2/2C-grammar-neutrality.md:451`); 2D LAC-2D-03 (`restart/audit/totality/p2/2D-cost-model.md:188`-`191`); SK-V13 cascade fail-closed rule (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:232`-`235`). |
| L14-HC-07 fact streams are output planes | Fact streams are valid admitted output planes only with strict comparator/oracle provenance and gate-consumed telemetry. They are not hidden retained sidecars and do not create a sixth `BackendShape`. | V2 addendum substrate contract (`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:96`-`97`); T-P1 CSS fact-stream category gap (`restart/audit/totality/p1/1C-runtime-evidence.md:102`-`113`); CSS row evidence (`skinny/RESULTS.md:94`). |
| L14-HC-08 generated-output allowance fence | Generated files under `runtime/src/grammars/<name>/` may contain grammar names only if produced by the rostered generator and guarded by Lock 14 validation; handwritten per-grammar runtime files remain forbidden unless G-Omega amends the lock. | 1E LAC-1E-08 (`restart/audit/totality/p1/1E-locks-evidence.md:107`); architecture generated surface rule (`restart/ARCHITECTURE.md:417-419`, `restart/ARCHITECTURE.md:1791-1801`); current hand-written runtime drift (`restart/audit/totality/p1/1C-runtime-evidence.md:58`-`60`). |
| **L14-HC-09 RuntimeProvider 2→8 enum-drift fault** | The Lock 14 v+1 generated-output allowance forbids expanding a hand-coded provider enum in lieu of generating a provider manifest. The V3→V4 drift expanded `RuntimeProvider` from 2 variants (V2-fold cite) to 8 variants at HEAD (`skinny/crates/codegen/src/grammar_profile.rs:17`-`26`) plus a `runtime_profiles() -> [&'static GrammarProfile; 8]` static array at `:100`-`:110`. Any future Sheets/BBNF-self/EBNF/BNF/CSV/math/css_pretty addition by editing this enum is a Lock 14 fault regardless of other merit. The Lock 14 v+1 manifest must enumerate the generator-input source (workspace metadata or grammar-manifest equivalent) plus the generator output target; a candidate provider added without that pair is non-admittable. | 2C V4 LAC-2C-01 expanded (`restart/audit/totality/p2/2C-grammar-neutrality.md:100`); HEAD enum at `skinny/crates/codegen/src/grammar_profile.rs:17`-`26`; static roster at `:100`-`:110`; 2C V4 V3-V4 drift note (`restart/audit/totality/p2/2C-grammar-neutrality.md:309`, `:333`). |
| **L14-HC-10 pass-layer recognizer + materialization-role JSON-byte/literal leaks** | The Lock 14 grammar-shape census MUST cover BOTH the recognizer plane (1B-D8 byte whitelist `{ } [ ] , : "` at `skinny/crates/passes/src/lib.rs:331` inside `derive_recognizers`) AND the role plane (1B-D10 JSON-literal census `{`, `}`, `[`, `]`, `:`, `true`, `false`, `null` at `skinny/crates/passes/src/lib.rs:1300`-`1391` inside `derive_materialization_roles`) PLUS the LAC-2C-02 JSON-canonical label sites at `:1059`, `:1079`, `:1102` (where `label: "object"/"array"/"pair"` leaks even though `TapeKind` is grammar-neutral). Non-JSON grammar fixture (Sheets formula or BBNF-self directive payload) must derive recognizer + role facts from grammar metadata without pass-crate edits. Both planes are upstream of the codegen-layer Lock 14 PRUNE-4 work; neither alone unblocks Sheets/BBNF-self. | T-P1 1B-D8 (`restart/audit/totality/p1/1B-codegen-evidence.md:50`, `:63`, `:86`); T-P1 1B-D10 (`:51`, `:64`, `:87`); LAC-2C-02 (`restart/audit/totality/p2/2C-grammar-neutrality.md:101`, `:449`); Sheets/BBNF-self upstream-blocker rows (`restart/audit/totality/p1/1B-codegen-evidence.md:71`-`72`). |
| **L14-HC-11 runtime root reexport + parser-name census** | The Lock 14 verification command set MUST surface the runtime-root reexport count and the parser-name leak census as gate-consumed numbers: HEAD `crates/core/src/runtime/mod.rs:25-71` carries **127 distinct grammar-named symbols** across 47 lines (133 raw `pub use` entries minus 6 in-window grammar-neutral exports per NEW-CH2-V2-03 subtraction discipline); HEAD `crates/core/src/runtime/{json,bbnf,css_l4,google_sheets}/{parse_with,mod,document,builder,serialize}.rs` plus `google_sheets/document/{mod,canonical}.rs` carries **30 parser-name leak sites across 15 files** (live re-run 2026-05-23). Lock 14 v+1 must require both numbers to monotonically decrease per wave; baseline 127 reexports and 30 sites at HEAD; closure requires routing consumers through `bbnf::grammar::generated::<g>::*` per 1C-D4/D5 receivers. | T-P1 1C runtime-root leak (`restart/audit/totality/p1/1C-runtime-evidence.md:21`-`24`, `:92`, `:124`-`125`); 1C-D4 receiver (`:162`); 1C-D5 receiver (`:163`); NEW-CH2-V2-03 subtraction discipline (`restart/audit/totality/p1/1C-runtime-evidence.md:23`-`24`). |
| **L14-HC-12 primitive policy_owner + FlagSchema + abstract-primitive sibling** | The Lock 14/Lock 16 bridge manifest MUST require three additional per-primitive/per-grammar fields: (a) `policy_owner` on every Layer 1 consumer call site — one of `generated_grammar` (codegen emits LUT/constants), `caller_data` (consumer supplies at runtime), or `none` (truly grammar-neutral, e.g. `BITMAP_PREFIX_XOR_64`); reject shared call sites with hardcoded JSON constants (LAC-2B-03); (b) a `FlagSchema` generated table replacing ad-hoc `pub(crate) const X: u8 = OffsetFlags::GRAMMAR_BIT0` aliases, with grammar-owned bit assignments — HEAD has only 2 grammar bits at `skinny/crates/runtime/src/tape/mod.rs:22`-`23`, and CSS/Sheets/BBNF-self may need more (LAC-2C-03); (c) `byte_class_from_range_64` pinned as a sibling of `byte_class_from_eq_set_64` in the abstract-primitive declaration list — the two-primitive split (set ≤8 vs inclusive range) is the load-bearing grammar-neutral generalization vehicle for digit-run / UTF-8 continuation / CSS hex / BBNF identifier classification (LAC-2F-V5-03). Lock 16 v+1 atomic close-state vocabulary (LAC-2B-07) gates the same surface. | LAC-2B-03 `policy_owner` field (`restart/audit/totality/p2/2B-primitive-vocabulary.md:383`, `:529`); LAC-2C-03 `FlagSchema` table (`restart/audit/totality/p2/2C-grammar-neutrality.md:120`, `:311`, `:450`); LAC-2F-V5-03 sibling abstract-primitive (`restart/audit/totality/p2/2F-parse-that-gaps.md:156`, `:520`); LAC-2B-07 Lock 16 atomic close-state vocabulary (`restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:234`). |

## Proposed Delta Table

| proposed delta | source T-P1/T-P2 finding-id cited | affected V1-surface section | rationale |
|---|---|---|---|
| 3E-D01: Add the CSS/Sheets/BBNF-self `BackendShape` matrix above as the canonical non-JSON companion to `ARCHITECTURE.md` section 7.3. V4 expands the matrix to 5 shapes × 15 CSS L4 sub-grammars plus 6 other-grammar rows. | 2C-BACKENDSHAPE-FIVE, 2C-BACKENDSHAPE-SELECTION, P1-1B-D3/D4/D5, SKINNY-GEN-009/010; 2C V4 §Executive Summary 15 sub-grammars. | `restart/ARCHITECTURE.md:1033-1108`; `restart/locks/LOCKS.md:70`, `restart/locks/LOCKS.md:78`. | The five-shape vocabulary is live, but selection must be proved by generated facts across non-JSON grammars, not JSON-only tests (`restart/audit/totality/p1/1B-codegen-evidence.md:36-47`, `restart/audit/totality/p2/2C-grammar-neutrality.md:137-145`). 15 CSS L4 sub-grammars per 2C V4. |
| 3E-D02: Reword the fixed eight-step `derive_backend_shape` order as a resolver pipeline: generate candidates, saturate/normalize legal alternatives, filter feasibility, extract by active cost, then emit diagnostics. | LAC-2D-01, LAC-2D-02, 2C-BACKENDSHAPE-SELECTION, COH-004. | `restart/ARCHITECTURE.md:1090-1098`; Lock 10 at `restart/locks/LOCKS.md:70`. | 2D refutes the fixed order as universal while preserving finite shape selection (`restart/audit/totality/p2/2D-cost-model.md:57-68`, `restart/audit/totality/p2/2D-cost-model.md:155-168`). |
| 3E-D03: Add generated provider manifest and generated sink/fact/value/flag ownership to the Lock 14 amendment queue. | LAC-2C-01, LAC-2C-03, 1C-D1/D2, D-1E-09. | Lock 14 at `restart/locks/LOCKS.md:78`; generated runtime rule at `restart/ARCHITECTURE.md:417-419`. | Current codegen/runtime still hardcodes provider/root grammar names, while 2C identifies generated manifests and grammar-owned sink/flag surfaces as the repair (`restart/audit/totality/p2/2C-grammar-neutrality.md:128-135`). |
| 3E-D04: Add primitive vocabulary transfer manifest fields to the Lock 14/Lock 16 bridge: `abstract_primitive`, generated policy, scalar oracle, parity/checkasm, same-wave consumer, output plane, row movement or measured rejection. | LAC-2C-05, T2A-LAC-03, LAC-2F-02, LAC-1E-10. | Lock 14 and Lock 16 at `restart/locks/LOCKS.md:78`, `restart/locks/LOCKS.md:87-112`. | T-P2 converged that primitive parity alone is not admission and that shared primitives need generated grammar policy plus row consumers (`restart/audit/totality/p2/2B-primitive-vocabulary.md:38-46`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:115-139`). |
| 3E-D05: Classify CSS fact streams as admitted output planes, not retained sidecars and not a sixth `BackendShape`. | 1A-DIV-006, 1C-D5, 2C-CSS-FACT-STREAM, T2A-LAC-04. | Runtime substrate taxonomy in `restart/ARCHITECTURE.md:1025-1031`; Lock 1/14 in `restart/locks/LOCKS.md:52`, `restart/locks/LOCKS.md:78`. | CSS declaration-values is admitted evidence with strict same-plane telemetry, but V1 lacks a formal category for that product row (`skinny/RESULTS.md:94`, `restart/audit/totality/p1/1A-substrate-evidence.md:45-46`). |
| 3E-D06: Adopt the future-grammar onboarding test as the Lock 14 close gate for arbitrary grammars. | 2C future-grammar onboarding test, 1F COH-002, LAC-1E-08. | `restart/ARCHITECTURE.md:1754-1762`; Lock 14 at `restart/locks/LOCKS.md:78`; future HANDOFF wording. | T-P2 requires source/metadata/declaration-crate-only onboarding, generated surfaces, leak scans, five-shape eligibility, primitive consumer proof, and telemetry before generality claims (`restart/audit/totality/p2/2C-grammar-neutrality.md:344`-`405`). |
| 3E-D07: Make CSS L4 plus Sheets/BBNF-self negative controls mandatory before "fleet-wide grammar-neutral" wording may be used. | 2C-CSS-SELECTOR-SCOPE, 2C-SHEETS-FORMULA-FALSIFIER, 2C-BBNF-SELF-FALSIFIER, SKINNY-GEN-009/010. | Lock 14; SK-V13/S-P3 gate language; `MASTER-PLAN.md` future wave acceptance criteria. | SK-V13 requires full CSS parity or architectural block, and T-P2 says Sheets/BBNF-self expose JSON role-mining overfit (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:38-57`, `restart/audit/totality/p2/2C-grammar-neutrality.md:72-79`). |
| 3E-D08: Add the grammar-shape leak census to the generic-crate validation command set. | LAC-2C-02, AP-003/AP-006, D-1E-09. | Lock 14 verification commands at `restart/locks/LOCKS.md:78`; diagnostic catalogue around `restart/ARCHITECTURE.md:1135-1144`. | Existing verification catches names but misses shape policy; P1 found JSON punctuation recognizers and object/array/pair role mining without relying on literal grammar names (`restart/audit/totality/p1/1F-anti-pattern.md:47-58`, `restart/audit/totality/p1/1E-locks-evidence.md:112-118`). |
| **3E-D09: Lock 14 v+1 manifest must enumerate the V3→V4 RuntimeProvider 2→8 enum drift as a fault baseline.** Generated provider manifest landing wave deletes the 8-variant hand-coded enum and the `runtime_profiles() -> [&'static GrammarProfile; 8]` static array; any future grammar onboarding adds a workspace-metadata block + grammar source without editing `grammar_profile.rs`. | 2C V4 LAC-2C-01 expanded; HEAD enum/array drift (V3-V4 2→8 variants); 2C V4 §Architectural Assertions Refuted row 4. | `restart/locks/LOCKS.md:222`-`238` (Lock 14 v+1 generated-output allowance); `skinny/crates/codegen/src/grammar_profile.rs:17`-`26`, `:100`-`:110`. | The drift itself is evidence Lock 14 v+1 needs an enumerated fault baseline, not just a generated-output allowance; absent that, future grammar additions will continue to expand the enum (`restart/audit/totality/p2/2C-grammar-neutrality.md:309`, `:333`). |
| **3E-D10: Lock 14 v+1 grammar-shape census must explicitly cover the pass-layer JSON-byte/literal leaks at `passes/src/lib.rs:331`, `:1059`, `:1079`, `:1102`, `:1300`-`:1391`.** Sheets/BBNF-self onboarding requires both 1B-D8 recognizer-byte plane AND 1B-D10 materialization-role plane sourced from generated grammar metadata; neither alone unblocks. | T-P1 1B-D8 + 1B-D10 (`restart/audit/totality/p1/1B-codegen-evidence.md:86`-`87`); LAC-2C-02 JSON-canonical labels at `passes/src/lib.rs:1059/1079/1102`. | Lock 14 verification commands at `restart/locks/LOCKS.md:220`; diagnostic catalogue. | Sheets and BBNF-self fail closed today not from missing codegen surfaces but from pass-layer JSON-shape coding; the Lock 14 census must catch this without grammar names (`restart/audit/totality/p1/1B-codegen-evidence.md:71`-`72`). |
| **3E-D11: Lock 14 v+1 verification must publish the runtime-root reexport count and the parser-name leak count as gate-consumed numbers (HEAD: 127 reexports across 47 lines + 30 parser-name sites across 15 files).** Each wave touching runtime root or per-grammar `parse_with.rs` shims must record both numbers monotonically decreasing; closure requires routing consumers through `bbnf::grammar::generated::<g>::*` per 1C-D4 + 1C-D5 receivers. | T-P1 1C-D4 (127 reexport census, `restart/audit/totality/p1/1C-runtime-evidence.md:162`); 1C-D5 (30 parser-name 15-file census, `:163`); NEW-CH2-V2-03 subtraction discipline (`:23`-`:24`). | Lock 14 verification commands; `restart/HANDOFF.md` Lock 14 leak audit pin. | The Lock 14 verification today calls the command but does not pin the number; without a baseline + monotonic-decrease rule, a partial decrease can register as "fixed" even when 100+ symbols remain (`restart/audit/totality/p1/1C-runtime-evidence.md:21`-`24`). |
| **3E-D12: Lock 14/Lock 16 bridge manifest must require `policy_owner` (LAC-2B-03), `FlagSchema` generated table (LAC-2C-03), `byte_class_from_range_64` sibling abstract-primitive (LAC-2F-V5-03), and atomic close-state vocabulary (LAC-2B-07).** Every Layer 1 consumer call site declares one of `generated_grammar` / `caller_data` / `none`; `OffsetFlags` grows from 2-bit `GRAMMAR_BIT0/1` to a `FlagSchema` table with grammar-owned bit assignments; the range primitive is pinned as the load-bearing grammar-neutral generalization vehicle. | LAC-2B-03 (`restart/audit/totality/p2/2B-primitive-vocabulary.md:529`); LAC-2C-03 (`restart/audit/totality/p2/2C-grammar-neutrality.md:120`, `:311`, `:450`); LAC-2F-V5-03 (`restart/audit/totality/p2/2F-parse-that-gaps.md:156`, `:520`); LAC-2B-07 (`restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:234`). | Lock 14 and Lock 16 manifest at `restart/locks/LOCKS.md:78`, `restart/locks/LOCKS.md:87`-`112`. | LAC-2B-03 + LAC-2C-03 + LAC-2F-V5-03 + LAC-2B-07 are not folded into V3 L14-HC clauses; without the explicit fold, the bridge manifest under-specifies per-primitive policy and bit-schema obligations for CSS/Sheets/BBNF-self consumers. |

## Consequences

V3 consequence rows for D01..D08 preserved verbatim. V4 adds D09..D12.

| delta | positive consequence | cost / risk / wave | propagation |
|---|---|---|---|
| 3E-D01 | Makes Lock 14 concrete for CSS L4, Sheets, and BBNF-self while preserving the five-shape canon; V4 broadens to 5×15+6 rows. | 240-400 doc LOC, medium risk, T-P3/Omega architecture fold. | 3 surfaces: `ARCHITECTURE.md`, `LOCKS.md`, S-P3 gates. |
| 3E-D02 | Reconciles T-P1's live priority drift with T-P2's decision-engine research. | 80-180 doc LOC plus named resolver receiver wave, medium-high risk, decision-engine wave. | 3 surfaces: `ARCHITECTURE.md`, `LOCKS.md`, SK-V13 G2. |
| 3E-D03 | Turns provider/sink/flag ownership from prose into a Lock 14 validator target. | 350-900 implementation LOC in the Lock 14 registry/runtime receiver wave, high risk. | 4 surfaces: `LOCKS.md`, `ARCHITECTURE.md`, codegen/runtime plan, tests. |
| 3E-D04 | Prevents support-only SIMD/primitive paper-close. | 200-600 manifest/tooling LOC in the Lock 16 primitive-admission receiver wave, medium-high risk. | 4 surfaces: `LOCKS.md`, `BENCH`, `bbnf-simd` gates, S-P3 gates. |
| 3E-D05 | Preserves the SK-V12 CSS win without inventing a parallel substrate. | 100-300 doc/report LOC, medium risk, substrate taxonomy fold. | 4 surfaces: `ARCHITECTURE.md`, `LOCKS.md`, `BENCH`, `RESULTS` schema. |
| 3E-D06 | Gives arbitrary grammar onboarding a falsifiable gate. | 120-260 doc/test LOC now; generated-fixture receiver must be capped in S-P3 or routed to an explicit non-budgeted handoff gate, medium risk. | 3 surfaces: `ARCHITECTURE.md`, `HANDOFF.md`, `LOCKS.md`. |
| 3E-D07 | Blocks JSON-only closure language and makes non-JSON witnesses required. | 80-180 doc LOC, medium risk, S-P3/MASTER wave criteria. | 4 surfaces: `LOCKS.md`, `MASTER-PLAN.md`, SK-V13 plan, challenge gates. |
| 3E-D08 | Makes grammar-shaped overfit visible to CH2 even when `rg Json` passes. | 180-420 lint/report LOC in the Lock 14 census receiver wave, high risk. | 4 surfaces: `LOCKS.md`, diagnostics, CI/gates, S-P3 validation. |
| **3E-D09** | Encodes the V3→V4 enum drift as a non-recurrence fault baseline; future grammar additions cannot ride enum expansion. | 80-160 doc LOC in Lock 14 v+1 manifest amendment; 350-700 impl LOC in the generated-provider-manifest receiver wave; medium-high risk. | 3 surfaces: `LOCKS.md`, `ARCHITECTURE.md`, codegen-receiver plan. |
| **3E-D10** | Surfaces the upstream Sheets/BBNF-self generalization blocker (pass layer) that codegen-layer Lock 14 work alone cannot unblock. | 60-140 doc LOC in Lock 14 v+1 census amendment; 250-500 impl LOC per plane (1B-D8 + 1B-D10) in pass-layer receiver wave; medium-high risk. | 3 surfaces: `LOCKS.md`, diagnostics, pass-crate plan. |
| **3E-D11** | Turns the 127 + 30 census numbers from prose into gate-consumed monotonic-decrease budgets; eliminates "partial fix" false closure. | 40-100 doc LOC in Lock 14 v+1 verification amendment; runtime-root receiver wave already in 3E-D03 budget; low-medium risk. | 3 surfaces: `LOCKS.md`, `HANDOFF.md`, CI/gates. |
| **3E-D12** | Adds the four per-primitive/per-grammar fields (`policy_owner`, `FlagSchema`, range-sibling, atomic close-state) that the Lock 14/Lock 16 bridge currently under-specifies. | 120-280 doc/manifest LOC in Lock 14/Lock 16 bridge amendment; 200-500 impl LOC at Layer 1 call sites + `OffsetFlags` expansion; medium-high risk. | 4 surfaces: `LOCKS.md`, `bbnf-simd` Layer 1 boundary, runtime `OffsetFlags`, primitive manifest. |

## V4 Cost And Routing Ledger

V3 ledger preserved verbatim; V4 appends D09..D12 rows.

| delta | LOC budget | propagation surfaces | risk class | wave alignment | same-wave consumer / receiver | hard cap or abrogate gate |
|---|---:|---:|---|---|---|---|
| 3E-D01 | 240-400 docs | 3 | Medium | Omega CRUD-1 architecture shape matrix | Receiver: 5×15+6 BackendShape matrix. | Block if any matrix row expands five-shape canon. |
| 3E-D02 | 80-180 docs plus named resolver receiver wave | 3 | Medium-high | Decision-engine fold | Receiver: resolver pipeline wording and Lock 10 gate. | Abrogate if fixed P1-P8 order remains admitting. |
| 3E-D03 | 350-900 impl/docs in receiver wave | 4 | High | Lock 14 registry/runtime wave | Receiver: generated provider/sink/fact/value/flag manifest. | Block if generic crates hand-code provider arrays, root aliases, or grammar branches. |
| 3E-D04 | 200-600 manifest/tooling in receiver wave | 4 | Medium-high | Lock 16 primitive-admission wave | Receiver: primitive policy manifest and same-wave consumer gate. | Block primitive-only imports without scalar oracle, checkasm, and row movement/rejection. |
| 3E-D05 | 100-300 docs/report | 4 | Medium | Substrate taxonomy + BENCH feed | Receiver: CSS fact-output as output-plane evidence. | Block if fact streams become retained sidecars or a sixth shape. |
| 3E-D06 | 120-260 docs/test now; receiver wave capped by S-P3 | 3 | Medium | Future-grammar onboarding gate | Receiver: S-P3 generated witness wave or explicit Omega handoff gate. | Abrogate prose-only generality; fixture must fail closed without generated facts. |
| 3E-D07 | 80-180 docs | 4 | Medium | Lock 14 and MASTER wave criteria | Receiver: CSS plus Sheets/BBNF-self negative-control rule. | Block "fleet-wide grammar-neutral" wording until positive CSS row plus negative control are gate-consumed. |
| 3E-D08 | 180-420 lint/report in receiver wave | 4 | High | Lock 14 census wave | Receiver: grammar-name and grammar-shape leak command set. | Block if validation only searches literal names and misses JSON-shaped policy. |
| **3E-D09** | 80-160 docs + 350-700 impl | 3 | Medium-high | Lock 14 v+1 manifest amendment + generated-provider receiver wave | Receiver: generated provider manifest replacing hand-coded enum + static roster. | Block if any wave grows `RuntimeProvider` variant count without an emitter-source pair recorded in the manifest. |
| **3E-D10** | 60-140 docs + 250-500 impl per plane | 3 | Medium-high | Lock 14 v+1 census amendment + pass-layer receiver wave | Receiver: 1B-D8 byte-whitelist deletion + 1B-D10 role-mining deletion + LAC-2C-02 label deletion at `passes/src/lib.rs:1059/1079/1102`. | Abrogate Sheets/BBNF-self onboarding claim that asserts codegen-layer-only repair; both planes mandatory. |
| **3E-D11** | 40-100 docs | 3 | Low-medium | Lock 14 v+1 verification amendment | Receiver: Lock 14 verification-command output that publishes baseline 127/30 + monotonic-decrease rule. | Block per-wave gate closure when reexport or parser-name count fails to decrease. |
| **3E-D12** | 120-280 docs + 200-500 impl | 4 | Medium-high | Lock 14/Lock 16 bridge manifest amendment | Receiver: per-primitive `policy_owner` field; `FlagSchema` generated table at `runtime/src/tape/`; `byte_class_from_range_64` pinned in Lock 16 abstract-primitive list; atomic close-state vocabulary surface. | Block primitive admission whose `policy_owner` is missing or whose flag bit alias is not in `FlagSchema`. |

## Open Questions

V3 open questions preserved; V4 adds two.

| lens | question | receiver / blocker / gate |
|---|---|---|
| CH1 correctness | `ARCHITECTURE.md` section 7.4 still contains stale prose about `shapes_for_json()` and `nominate_json`, while T-P1 says live skinny has moved past symbol absence but still carries provider and role leaks. | Receiver: 3A/Pass Omega. Blocker: 3E cannot edit `ARCHITECTURE.md`. Gate: Omega CRUD must reconcile `restart/ARCHITECTURE.md:1129-1131` with `restart/audit/totality/p1/1B-codegen-evidence.md:48-50`. |
| CH2 generality | Should the formal spec name CSS fact streams as `SinkOnly` products, or as a distinct output-plane taxonomy that does not expand `BackendShape`? | Receiver: 3A + 3C. Blocker: current T-P1 evidence calls it a category gap. Gate: accepted wording must preserve five shapes and cite `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:96-97`. |
| CH4 cost | What exact generated provider-manifest layout replaces `RuntimeProvider` with the smallest public API change, given the V3→V4 drift expanded the enum to 8 variants? | Receiver: S-P3/Lock 14 registry wave. Blocker: T-P2 left manifest layout open. Gate: add CSS plus Sheets or BBNF-self provider without editing generic code (`restart/audit/totality/p2/2C-grammar-neutrality.md:437`). |
| CH5 hidden coupling | How will the CSS lightningcss source sidecar stay comparator-only once more CSS rows are generated? | Receiver: S-P3 CSS rows + BENCH/Omega. Blocker: current same-plane sidecar is valid evidence but not runtime substrate. Gate: every CSS row emits output-plane provenance and no runtime dependency on comparator sidecars (`restart/audit/totality/p1/1F-anti-pattern.md:38-40`). |
| CH6 anti-paper-close | Which first concrete negative-control row should carry Sheets/BBNF-self: generated role-fact proof, formula/directive fact stream, or full parser row? | Receiver: S-P3 or next Omega-approved planning wave. Blocker: T-P2 requires a negative-control witness but does not choose fixture scope. Gate: fail-closed row with no generic code edits and explicit JSON-role-mining rejection (`restart/audit/totality/p2/2C-grammar-neutrality.md:157-168`). |
| **CH2 generality (V4)** | Does HEAD's 2-bit `GRAMMAR_BIT0`/`GRAMMAR_BIT1` slot count suffice for CSS L4 (escape + URL + comment policies), Sheets (doubled-quote + reference-type policies), and BBNF-self (multi-quote-set + Pratt policies), or must `OffsetFlags` storage expand alongside the `FlagSchema` generated table? | Receiver: 3A + S-P3 `FlagSchema` wave. Blocker: 2-bit slot may force inline aliasing that recreates the `HAS_ESC` leak under a new name. Gate: prove JSON parity unaffected when `OffsetFlags` widens and `FlagSchema` lands (`restart/audit/totality/p2/2C-grammar-neutrality.md:439`). |
| **CH4 cost (V4)** | The 127 + 30 census numbers at HEAD are large enough that the runtime-root reexport block plus per-grammar `parse_with.rs` shim deletion needs an explicit wave-by-wave staging plan (not a single landing). What is the minimum number of waves to reach 0/0 without breaking JSON consumers? | Receiver: S-P3 1C-D4 + 1C-D5 receivers. Blocker: T-P1 cites 80 LOC root rewrite + ~2.5× consumer-rewire band (proportional to 127 symbols), and 480 LOC across the 4 parse_with.rs shims. Gate: staging plan with monotonically decreasing reexport count and parser-name site count per wave, JSON consumers regress-free at each step (`restart/audit/totality/p1/1C-runtime-evidence.md:162`-`163`). |
