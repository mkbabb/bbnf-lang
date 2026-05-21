---
agent: 2C
pass: T-P2-research
cycle: V2
generated_at: 2026-05-21T08:37:12Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 15
techniques_grounded: 12
techniques_refuted: 5
prior_cycle_dispositions_folded:
  accepted:
    - CH3 primitive-admission posture preserved
    - CH4 generated-policy consumer posture preserved
  rejected: []
  revised:
    - CH1 stable section/page provenance
    - CH2 canonical Lock-14 transfer contract
    - CH5 fact-stream output-plane boundary
    - CH6 feature-to-transfer ledger and NOT-VALIDATED wording
  first_cycle_additions:
    - 2C-css-token-alphabet
    - 2C-css-selector-row-production
    - 2C-css-custom-property-and-calc
    - 2C-sheets-formula-falsifier
    - 2C-backendshape-data-driven-selection
    - 2C-lock14-generated-registry
    - 2C-grammar-sink-and-flag-surface
    - 2C-primitive-onboarding-test
locks_amendment_candidates: 5
v2_fold_authority:
  - restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md
---

## Executive Summary

The primitive vocabulary and five-shape `BackendShape` surface generalize only
when every choice is derived from grammar metadata and Grammar IR facts, not
from JSON role names or a hand-coded provider table. CSS L4 and Sheets are the
load-bearing falsifiers. CSS Syntax tokenizes identifiers, functions, hashes,
strings, URLs, numbers, percentages, dimensions, delimiters, brackets, and
at-keywords; Selectors L4 then layers selectors, pseudo-classes, attributes,
and combinators over that token stream. Sheets formulas instead center
case-insensitive function calls, semicolon-separated parameters, operators, and
cell/range references. Neither grammar can be soundly routed through
`JsonSink`, JSON object/array/pair role mining, or `OffsetFlags::HAS_ESC` /
`HAS_CONTROL` semantics.

The defensible totality rule is: primitives are grammar-neutral byte-stream
operations, but their admission and selection are grammar-specific data. A
future grammar onboards by providing grammar source, workspace metadata, and
optional declaration-crate host functions; generic crates must consume generated
registry/config/fact tables and must not add grammar branches. `BackendShape`
stays five variants; the decision engine chooses among them from generated
facts, cost evidence, and same-wave row consumers.

V2 folds the V1 CH1/CH2/CH5/CH6 revise set. External standards now ground only
candidate grammars through stable section/page anchors; they do not admit
routes. The closure contract is mechanical: generated provider registry,
grammar-shape leak scan, generated sink/fact/value/flag surfaces, primitive
policy manifests, CSS plus negative-control transfer, and decision-engine facts
must all pass before T-P3 can claim Lock 14 closure.

## Technique Grounding Table

| spec-claim or T-P1 divergence id | published source cited | grounded / refuted / partial | bbnf-specific note |
|---|---|---|---|
| 2C-CSS-TOKEN-ALPHABET: byte-class primitives generalize beyond JSON if they consume a grammar-provided token alphabet. | CSS Syntax Module Level 3 tokenization at `https://www.w3.org/TR/css-syntax-3/#tokenization` defines tokenization as the input-to-token-stream transform, with token definitions under `https://www.w3.org/TR/css-syntax-3/#typedef-token-stream`. Local drift: JSON structural alphabet and config are still visible at `skinny/crates/runtime/src/grammars/json/config.rs:4` and `skinny/crates/runtime/src/grammars/json/generated.rs:13`; CSS row exists at `skinny/RESULTS.md:94`. | grounded | `byte_class_from_eq_set_64`, TBL/TBX classifiers, run-skip, and whitespace/comment scanners are admissible as abstract byte-class primitives. Their alphabet must come from generated grammar config, not from `b"{}[],:\""`. |
| 2C-CSS-SELECTOR-SCOPE: full CSS parity is row production, not declaration-token reuse. | Selectors Level 4 overview at `https://www.w3.org/TR/selectors-4/#overview` names selector families and matching model; grammar sections under `https://www.w3.org/TR/selectors-4/#grammar` define the parse surface. Local parity gap: `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md` section 3 marks selectors, stylesheet root, pseudo-classes, pseudo-elements, and attribute selectors as missing. | grounded | A selector row can use the same primitive vocabulary, but it needs generated selector facts and strict lightningcss equality. It cannot be represented as a JSON-style object/member/value projection. |
| 2C-CSS-CALC-VAR: CSS value functions require generated policy, not JSON number/string policy. | CSS Values and Units Level 4 defines calculation notation at `https://www.w3.org/TR/css-values-4/#calc-notation`; CSS Custom Properties Level 1 defines custom property declaration and substitution at `https://www.w3.org/TR/css-variables-1/#defining-variables` and `https://www.w3.org/TR/css-variables-1/#using-variables`. Local residual leak: JSON string and number paths call `match_tiny_plain_string` and `match_number_span_from_first` in `skinny/crates/runtime/src/grammars/json/generated.rs:93`, `:159`, and `:214`. | grounded | GrammarConfig must grow into generated per-grammar dispatch/string/number/whitespace/sink facts or equivalent generated functions. JSON number parsing may stay a primitive, but CSS numbers and dimensions need grammar-selected policies. |
| 2C-SHEETS-FORMULA-FALSIFIER: Sheets cannot pass through JSON-shaped materialization roles. | ODF 1.3 Part 4 Formula, Section 5.6 page 40, defines function calls by name, semicolon-separated parameters, and case-insensitive function names; Section 4.8 page 32 defines references; Section 5.8 page 41 defines constant-reference syntax; Section 5.5 page 38 defines reference operators `!`, `~`, and `:`. Local drift: `skinny/crates/passes/src/lib.rs:1243-1306` mines object/array/string/number/bool/null/pair roles through JSON-shaped literals. | refuted | The JSON role miner is not grammar-neutral. A Sheets proof row must derive function/reference/operator roles from generated grammar metadata and must not add `Sheets` branches to generic crates. |
| 2C-BBNF-SELF-FALSIFIER: BBNF-self needs operator/directive grammar facts, not JSON object-member analogues. | Local primary source: totality scope includes BBNF-self and arbitrary user grammars in `restart/prompts/ORCHESTRATOR.md:48-55`; live role mining remains JSON-shaped at `skinny/crates/passes/src/lib.rs:1243-1306`; SK-V13 synthesis requires BBNF-self fail-closed unless explicitly dispositioned at `restart/skinny/tranches/sk-v13/SYNTHESIS.md:220-235`. | refuted | BBNF-self onboarding is the grammar-shape audit: directives, alternatives, repetitions, and Pratt/operator chains must be facts from Grammar IR or metadata, not recovered by generic code recognizing JSON-like punctuation. |
| 2C-BACKENDSHAPE-FIVE: the five-shape surface is a grammar-neutral target vocabulary. | Local primary source: `BackendShape` has `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, `CollapsedStage` at `skinny/crates/ir/src/lib.rs:402-408`; `all_backend_shapes()` returns the same five at `skinny/crates/ir/src/cost.rs:127-135`; Lock 10 names the same materialization-plan side table at `restart/locks/LOCKS.md:70`. | grounded | Do not add a shape for CSS, Sheets, or BBNF-self. Add generated facts that let the resolver choose an existing shape per rule. |
| 2C-BACKENDSHAPE-SELECTION: current selection exists but is not grammar-general enough. | Local primary source: `LayoutFacts.backend_shape` and `cost_facts` are populated in `skinny/crates/passes/src/lib.rs:44-55` and `:84-92`; the current priority/cost flow sits at `skinny/crates/passes/src/lib.rs:387-505`; `CostFacts` is thin at `skinny/crates/ir/src/cost.rs:5-13`. T-P1 1B records the same gap in `restart/audit/totality/p1/1B-codegen-evidence.md`. | partial | The five-shape vocabulary is defensible; the hardcoded P1-P8 cascade is not the long-term generality mechanism. Selection must be data-driven through the SK-V13 resolver and cost evidence. |
| 2C-RUNTIME-PROVIDER-REGISTRY: grammar providers are still hardcoded. | Local primary source: `RuntimeProvider::{Json, CssL4DeclarationValues}` is hardcoded at `skinny/crates/codegen/src/grammar_profile.rs:11-15`, and `runtime_profiles()` returns only those two at `skinny/crates/codegen/src/grammar_profile.rs:89-93`. Lock 14 forbids grammar-name modules and branches in generic crates at `restart/locks/LOCKS.md:78`. | refuted | The registry must be generated from workspace metadata or grammar manifests. Adding CSS, Sheets, or BBNF-self by editing this enum is a Lock 14 failure. |
| 2C-GENERATED-SINK: `JsonSink` is a row-specific generated/direct surface, not a generic sink contract. | Local primary source: `JsonSink` is declared in `skinny/crates/runtime/src/grammars/json/sink.rs:4`; generated direct parse is bound to `S: JsonSink` in `skinny/crates/runtime/src/grammars/json/generated.rs:407`; CSS fact sink is separate at `skinny/crates/runtime/src/grammars/css_l4_declaration_values/sink.rs:18-99`. | refuted | The totality sink concept should be `GrammarSink` as a generated per-grammar trait or generated associated surface. The generic runtime must not know JSON callback names. |
| 2C-FLAG-SEMANTICS: tape flags need grammar-owned interpretation. | Local primary source: `OffsetFlags::HAS_ESC` and `HAS_CONTROL` are named in `skinny/crates/runtime/src/tape/mod.rs:20-36`; JSON config maps `STRING_NEEDS_DECODE` to `HAS_ESC` at `skinny/crates/runtime/src/grammars/json/config.rs:5`. CSS custom property identity and escape concerns are specified by CSS Custom Properties Level 1 `#defining-variables` and `#using-variables`. | partial | Bit storage may stay generic, but bit names/meaning must be generated per grammar or interpreted through grammar-owned flag tables. |
| 2C-PRIMITIVE-ADMISSION: grammar-neutral primitive parity alone is not admission. | Local primary source: SK-V12 escape-mask correctness was prerequisite-only at `skinny/REDRESS.md:3603-3632`; `a64_ascii_set_run_skip` microbench was routed as production split at `skinny/REDRESS.md:3766-3820`; SK-V13 bans producer-only SIMD/union/resolver/codegen artifacts at `restart/skinny/tranches/sk-v13/SYNTHESIS.md:246-247`. | refuted | A primitive generalizes only after scalar reference, checkasm/parity, generated grammar policy proof, and same-wave CSS/JSON/Sheets/BBNF-self consumer measurement. |
| 2C-CSS-FACT-STREAM: fact streams are valid product rows, but they need a formal substrate/telemetry category. | Local primary source: CSS admission row is recorded in `skinny/RESULTS.md:94`; W1b scaffold and comparator gates are recorded at `skinny/REDRESS.md:3634-3764`; close promotes PASS-ADMIT at `skinny/REDRESS.md:3824-3840`. T-P1 1A flags the missing category in `restart/audit/totality/p1/1A-substrate-evidence.md`. | grounded | CSS fact-stream products are legitimate non-JSON evidence. Totality must classify them without pretending they are `OffsetTape`/`EventTape` retained documents. |

## Canonical Lock 14 Transfer Contract

This dossier adopts `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md` as
the shared mechanical contract. A standards citation or one admitted CSS row
grounds a candidate only; closure still requires generated policy, row
consumer, strict equality, telemetry consumption, and no generic grammar branch.

| gate | 2C closure rule | owner surface |
|---|---|---|
| generated provider registry | `RuntimeProvider::{Json, CssL4DeclarationValues}` is a live Lock 14 leak until replaced by a generated provider manifest. | `skinny/crates/codegen/src/grammar_profile.rs` generator input and generated roster output |
| grammar-shape leak scan | The scan must catch grammar names and grammar-shaped policy: JSON byte alphabets, object/array/pair role mining, `JsonSink` callback names, and JSON flag meanings. | `skinny/crates/{ir,passes,codegen,runtime,bbnf-simd,parse-that-regex,parse-that}/src` |
| generated sink/fact/value/flag surface | `JsonSink`, CSS fact sinks, value views, and flag names are generated grammar-owned surfaces; generic tape may store bits but not define grammar semantics. | `skinny/crates/runtime/src/grammars/<name>/` generated modules |
| primitive policy manifest | Shared primitives receive alphabets, delimiters, quote/escape/control policy, number policy, string policy, and no-string/no-number policy from generated grammar data or caller data. | `bbnf-simd` call boundaries and generated parser consumers |
| CSS plus negative-control transfer | Fleet-wide grammar neutrality requires a CSS L4 row plus a Sheets or BBNF-self witness/negative-control. SK-V12 declaration-values alone is evidence, not closure. | CSS, Sheets, and BBNF-self generated rows or explicit fail-closed witnesses |
| decision-engine facts | e-graph rewrites, CSP constraints, and costs consume generated grammar facts; mining JSON roles remains Lock 14 drift even if JSON equality passes. | `skinny/crates/ir`, `skinny/crates/passes`, decision-engine resolver output |

## Per-Technique Transfer Coverage

Entries below are transfer requirements, not admissions. A technique without
the named generated facts and a same-wave row consumer is `NOT-VALIDATED` for
fleet-wide grammar-neutral claims.

| technique | CSS L4 transfer | Sheets transfer | BBNF-self transfer | required generated facts | failure mode if absent |
|---|---|---|---|---|---|
| byte-set classify / run-skip | delimiters, trivia, identifiers, at-rules | separators, quotes, operators | punctuation, directive starts | byte alphabet, quote/comment policy | generic JSON alphabet leak |
| string / escape scan | CSS strings, URLs, escaped identifiers | doubled quotes or no-backslash policy | literals and escapes | quote, escape, control, terminator policy | JSON `\uXXXX` overfit |
| digit / number scan | numbers, dimensions, percentages, calc | numeric literals and references | numeric literals | number grammar, suffix/unit policy | JSON exponent/sign policy leak |
| direct/fact sink | CSS fact-stream rows | formula facts | grammar/directive facts | generated sink callbacks and fact schema | `JsonSink` as generic API |
| `BackendShape` resolver | selector/value/declaration shapes | formula/reference shapes | rule/expression/directive shapes | FIRST/follow, layout, host, recovery, output mode, cost facts | hardcoded P1-P8 cascade |
| regex/HIR facts | CSS selector/value recognizers where legal | formula token recognizers | grammar token recognizers | HIR, nullability, first-set, char-class facts | opaque JSON regex strings |

## Feature / Witness Transfer Ledger

The ledger makes CH6's anti-paper-close requirement explicit. Rows without an
existing production consumer remain `NOT-VALIDATED`; they can be shortlisted
only with a same-wave row consumer and gate-consumed telemetry.

| feature or witness | generated facts required | primitive families | code owners | equality oracle | row consumer / telemetry | state |
|---|---|---|---|---|---|---|
| CSS declaration values, SK-V12 admitted row | declaration/value token facts, sink schema, string/number policy | byte-set classify, run-skip, string scan, number scan, fact sink | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`, CSS codegen templates | lightningcss + cssparser strict equality | `css_l4/declaration_values/direct_to_struct/main`, `skinny/RESULTS.md` CSS telemetry | ADMITTED-EVIDENCE |
| CSS stylesheet and selectors | stylesheet root, selector families, combinator, pseudo, attribute, nesting facts | byte-set classify, identifier scan, selector dispatch, fact sink | future `css_l4_stylesheet_and_selectors` generated runtime/codegen paths | lightningcss plus independent golden/cssparser coverage where applicable | future generated CSS row; non-JSON telemetry schema | NOT-VALIDATED |
| CSS declaration values extended, including `var()` and `calc()` | custom-property, substitution, function, math, dimension/unit facts | string/escape scan, number/dimension scan, function dispatch, fact sink | CSS declaration extension generated paths | lightningcss plus cssparser/golden table coverage | future generated CSS row; feature-level telemetry | NOT-VALIDATED |
| CSS visual functions: gradients, transforms, filters, easing | function families, nested value grammar, unit/domain facts | byte-set classify, number scan, function dispatch, fact sink | future CSS visual-function generated paths | lightningcss plus hand-checked golden table where cssparser lacks parity | future generated CSS row; feature-level telemetry | NOT-VALIDATED |
| Sheets formula witness | function names, separators, operators, references/ranges, string quote policy | byte-set classify, digit scan, string scan, reference recognizer, fact sink | future `sheets` generated runtime/codegen paths | ODF 1.3 Part 4-derived golden table | future formula fact row or fail-closed witness telemetry | NOT-VALIDATED |
| BBNF-self witness | directives, alternatives, repetitions, token/literal policy, operator/directive facts | regex/HIR facts, byte-set classify, string scan, direct/fact sink | future BBNF-self generated runtime/codegen paths | current grammar parser plus checked golden grammar fixtures | future grammar/directive fact row or fail-closed witness telemetry | NOT-VALIDATED |

## Closure Criteria For Live Grammar Leaks

| surface | closure criterion | not closure |
|---|---|---|
| `RuntimeProvider` | provider roster generated from workspace metadata or grammar manifests, with JSON/CSS/Sheets or BBNF-self onboarding without generic-code edits | adding enum variants or grammar-name branches in `codegen` |
| `JsonSink` | generated per-grammar sink/fact/direct builder surface; generic crates see generated artifacts, not JSON callback names | renaming `JsonSink` while retaining JSON callback semantics as the shared API |
| `OffsetFlags` | bit storage may remain generic, but names, decode policy, and semantic interpretation are generated grammar-owned flag schemas | `HAS_ESC` / `HAS_CONTROL` remaining public grammar-neutral meanings |
| fact streams | admitted output-plane contracts with strict comparator/oracle provenance and gate-consumed telemetry | hidden retained sidecars consumed by later internal waves |

## Architectural Assertions Defended

| assertion | defense | consequence |
|---|---|---|
| The primitive vocabulary is grammar-neutral at the operation layer. | CSS tokenization, selectors, CSS value math/custom properties, and OpenFormula references all reduce to byte-stream recognizers, delimiter handling, number/function/string/reference scanners, and output-fact builders. The grammar-specific piece is the metadata feeding those primitives. | Keep Layer 1 primitives named by abstract operation: byte classify, set-membership, run-skip, digit-block decode, cross-chunk context, prefix/bitmap operations, and fact emission. Do not name primitives after JSON. |
| `BackendShape` remains exactly the five-shape enum. | The live enum and Lock 10 already provide a grammar-neutral target vocabulary. The defect is selection strength and lowerer completeness, not missing CSS/Sheets shapes. | SK-V13/SK-V14 should add resolver facts and lowerers, not enum variants. |
| CSS L4 is the right non-JSON proof grammar. | The admitted CSS declaration-values row supplies strict equality and lightningcss comparator evidence; the parity gap matrix names missing production families. | CSS rows are the first-class generality test. Sheets and BBNF-self remain required falsifiers for role mining, but not substitutes for the pinned CSS bar. |
| Generated per-grammar code may carry grammar names; generic crates may not. | Lock 14 permits grammar source/metadata/declaration-crate inputs, while generated outputs necessarily live under `runtime/src/grammars/<name>/`. The failure is hand-coded generic dispatch/registry policy. | T-P3 should codify a generated-output allowance and a stricter generic-crate scan for name and shape leaks. |
| Same-wave consumer measurement is part of grammar generality. | REDRESS shows primitive-only and microbench-only routes become orphans when they do not move a row. | Every grammar-neutral primitive needs a row consumer in at least one CSS/JSON/Sheets/BBNF-self path during the admitting wave. |

## Architectural Assertions Refuted

| refuted assertion | evidence | replacement rule |
|---|---|---|
| JSON object/array/pair/string/number/bool/null role mining is a generic materialization strategy. | The live pass role mining is JSON-shaped at `skinny/crates/passes/src/lib.rs:1243-1306`; CSS Selectors and OpenFormula references/functions do not map to those roles. | Role facts must be generated from Grammar IR and metadata, then consumed by the resolver. |
| `JsonSink` can stand in for a generic direct-output sink. | `JsonSink` callbacks are JSON-named at `skinny/crates/runtime/src/grammars/json/sink.rs:4-15`; CSS fact sink has a different event/fact shape at `skinny/crates/runtime/src/grammars/css_l4_declaration_values/sink.rs:18-99`. | Generate a per-grammar sink/fact trait or per-grammar direct builder; the generic runtime sees only generated artifacts. |
| `OffsetFlags::HAS_ESC` and `HAS_CONTROL` are grammar-neutral public semantics. | The flag names live in the generic tape at `skinny/crates/runtime/src/tape/mod.rs:20-36`; CSS custom properties and escapes have different identity and context rules. | Keep packed bit storage if useful, but move flag meanings behind generated grammar-owned interpretations. |
| Adding a grammar by editing `RuntimeProvider` is acceptable. | `RuntimeProvider` is a grammar-name enum in generic codegen at `skinny/crates/codegen/src/grammar_profile.rs:11-15`; Lock 14 forbids this class. | Provider registration is generated from workspace metadata; adding a grammar changes metadata/source/generator output, not generic code. |
| CSS declaration-values token parity proves full CSS grammar generality. | SK-V12 admitted one row; SK-V13 synthesis names full CSS parity as open at `restart/skinny/tranches/sk-v13/SYNTHESIS.md:38-57`; the scoping matrix counts one admitted family and many missing/partial families. | Treat the SK-V12 row as valid evidence and as the first CSS fixture, not as close authority. |

## Future-Grammar Onboarding Test

Every future grammar must pass this test before a generality claim is allowed:

1. Add only grammar source, workspace metadata, and optional declaration-crate host functions.
2. Regenerate provider registry, config/fact tables, sink/value/view surfaces, and tests without editing generic crates.
3. Run a Lock 14 scan over `skinny/crates/{ir,passes,codegen,runtime,bbnf-simd,parse-that-regex,parse-that}/src` that detects both grammar-name leaks and grammar-shape leaks.
4. Run a five-shape eligibility fixture: at least one rule per reachable shape or an explicit generated reason that the shape is unreachable for this grammar.
5. Exercise at least one primitive through a same-wave consumer row or record a measured architectural block.
6. Emit telemetry/provenance consumed by the gate in the same wave.

For SK-V13, CSS L4 is mandatory for steps 1-6. Sheets and BBNF-self are the negative controls: they must fail closed under JSON-shaped role mining and pass only after generated role facts replace it.

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| What generated manifest layout replaces `RuntimeProvider` with the smallest public API change? | Prototype a generated provider manifest that recreates the current JSON and CSS outputs, then add a Sheets or BBNF-self witness provider without editing `codegen/src/grammar_profile.rs`. Closure is not optional: a hand-coded provider enum remains Lock 14 drift. |
| Which generated sink/fact trait template covers JSON direct, CSS fact-stream, Sheets formula facts, and BBNF-self parse facts? | Design generated sink/fact traits for the four proof surfaces and verify all four render from the same template family. Closure is not optional: `JsonSink` cannot remain a generic sink contract. |
| Which `FlagSchema` representation preserves packed storage while moving meaning into generated grammar policy? | Add a generated table with flag names, bit assignments, and decode policy; prove JSON parity and one CSS escape/custom-property fixture. Closure is not optional: `HAS_ESC` / `HAS_CONTROL` cannot remain grammar-neutral semantics. |
| Which primitive families are universally grammar-neutral and which are grammar-family-specific? | Cross-tab primitives against grammar facts: alphabet size, quoted-string model, comment model, number/dimension model, function-call syntax, reference syntax, and chunk-spanning tokens. |
| Does the decision-engine fold produce row-moving routes for all three proof grammars? | After W5-W9, run CSS, Sheets, and BBNF-self eligibility fixtures through the resolver and require emitted cost facts plus no generic grammar branches. |

## LOCKS-AMENDMENTS-CANDIDATE

| candidate | target lock(s) | proposed amendment | supporting evidence | risk / same-wave consumer |
|---|---|---|---|---|
| LAC-2C-01 generated grammar registry | Lock 14, Lock 6 | Generic crates may consume a generated provider manifest, but may not hand-code grammar provider enums, grammar-name branches, or grammar-name root aliases. Generated files under `runtime/src/grammars/<name>/` are allowed only if regenerated from the rostered generator. | Current hardcoded providers at `skinny/crates/codegen/src/grammar_profile.rs:11-15` and `:89-93`; Lock 14 at `restart/locks/LOCKS.md:78`; CSS admitted generated row at `skinny/RESULTS.md:94`. | High. Consumer: regenerated JSON and CSS rows plus a new Sheets/BBNF-self witness provider with no generic code edit. |
| LAC-2C-02 grammar-shape leak census | Lock 14, Lock 10 | Lock 14 verification must scan not only literal grammar names but grammar-shaped role policy: JSON punctuation alphabets, object/array/pair/string/number/bool/null roles, and hardcoded sink callback names. | Role mining at `skinny/crates/passes/src/lib.rs:1243-1306`; recognizer derivation at `skinny/crates/passes/src/lib.rs:324-350`; T-P1 1B/1F both classify this as live drift. | High. Consumer: CSS/Sheets/BBNF-self fixtures that derive roles from generated facts. |
| LAC-2C-03 generated sink and flag surface | Lock 1, Lock 9, Lock 14 | Add a generated grammar-owned sink/fact/value/flag interpretation surface. Generic tape may store compact flags, but bit names and direct-output callbacks are grammar-owned generated artifacts. | `OffsetFlags` at `skinny/crates/runtime/src/tape/mod.rs:20-36`; `JsonSink` at `skinny/crates/runtime/src/grammars/json/sink.rs:4`; CSS fact sink at `skinny/crates/runtime/src/grammars/css_l4_declaration_values/sink.rs:18-99`. | Medium-high. Consumer: JSON direct parity, CSS fact-stream parity, and one Sheets formula fact fixture. |
| LAC-2C-04 BackendShape onboarding proof | Lock 10, Lock 14 | A new grammar cannot claim generality until the resolver emits per-rule `BackendShape` and `CostFacts` from grammar facts, with zero generic grammar edits and a five-shape eligibility or explicit unreachable-shape report. | Five-shape enum at `skinny/crates/ir/src/lib.rs:402-408`; current thin cost facts at `skinny/crates/ir/src/cost.rs:5-13`; decision-engine fold requirement in `restart/skinny/tranches/sk-v13/SYNTHESIS.md:59-71`. | High. Consumer: resolver snapshot and generated-code equality tests for CSS, Sheets, BBNF-self. |
| LAC-2C-05 primitive generality admission gate | Lock 16, Lock 14 | Every primitive row records `abstract_primitive`, grammar feature vector, scalar reference, checkasm/parity status, same-wave consumer, and at least one non-JSON exercise when the primitive is claimed grammar-neutral. Primitive parity alone is not closure. | REDRESS prerequisite-only escape mask at `skinny/REDRESS.md:3603-3632`; `a64_ascii_set_run_skip` production split at `skinny/REDRESS.md:3766-3820`; SK-V13 producer-only pre-block at `restart/skinny/tranches/sk-v13/SYNTHESIS.md:246-247`. | High. Consumer: CSS scan-block production row or measured deletion; JSON/Sheets/BBNF-self consumers when claimed by those grammars. |

## Sources

- W3C CSS Syntax Module Level 3: `https://www.w3.org/TR/css-syntax-3/`.
- W3C Selectors Level 4: `https://www.w3.org/TR/selectors-4/`.
- W3C CSS Values and Units Module Level 4: `https://www.w3.org/TR/css-values-4/`.
- W3C CSS Custom Properties for Cascading Variables Module Level 1: `https://www.w3.org/TR/css-variables-1/`.
- OASIS OpenDocument v1.3 Part 4, OpenFormula: `https://docs.oasis-open.org/office/OpenDocument/v1.3/OpenDocument-v1.3-part4-formula.pdf`.
- T-P2 V2 fold authority: `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`.
- Local primary evidence: `restart/audit/totality/p1/*.md`, `restart/locks/LOCKS.md`, `restart/skinny/tranches/sk-v13/scoping/*.md`, `skinny/RESULTS.md`, `skinny/REDRESS.md`, and cited `skinny/crates/*/src` paths above.
