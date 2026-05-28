---
agent: 2C
pass: T-P2-research
cycle: V1
generated_at: 2026-05-28T06:35:30Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 24
techniques_grounded: 12
techniques_refuted: 7
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: []
  first_cycle_additions:
    - SK-V15-2C-GENERATOR-INPUT-SURFACE
    - SK-V15-2C-METADATA-GRAMMAR-OWNERSHIP
    - SK-V15-2C-PATTERN-H-COLLAPSE
    - SK-V15-2C-CSS-TYPED-VALUE-SURFACE
    - SK-V15-2C-FUTURE-GRAMMAR-ONBOARDING
    - SK-V15-2C-CSS-BROADCAST-REFUTATION
    - SK-V15-2C-GENERIC-GRAMMAR-SWITCH-REFUTATION
locks_amendment_candidates: 5
sk_cycle: SK-V15
t_p1_entry_state: CLEAN-FINAL-G1-AUTO-PINNED-NOT-NORMAL-3Z
implementation_floor: PASS-IMPL-V1-CSS-CONTRIVANCE-JSON-HONEST
host_close_route: Apple-M5-Max-aarch64
stale_sk_v14_material_reused: none
---

## Executive Summary

Lock 14 transfer is defensible only if grammar-specific behavior enters the
fleet through grammar source, workspace metadata, and generated per-grammar
runtime surfaces. SK-V15 starts from a split floor: JSON has honest same-plane
rows, while CSS L4 admission is contrived by a 24-row measurement broadcast,
`CSS_GENERATED_RS` string-literal generation, and `CssFullParseSummary`
brace-counter output. Current root runtime code does contain a typed CSS
document/value surface, but that is only partial evidence because Pattern H
still has 67 root runtime files with no line-1 generated provenance, CSS legacy
path shims remain, and the skinny admission path still returns fact-stream or
full-parse strings. Therefore JSON-only, CSS-only, generator-sidecar, and
generic-crate grammar-switch routes are refuted as fleet-wide generalisation.

The admissible transfer route is narrower: generate typed values, sinks,
metadata facts, grammar policy, and provider manifests from source plus
metadata; prove CSS L4 and at least one Sheets or BBNF-self receiver without
editing generic code; demote broadcast CSS rows before retiming; and require
future grammar onboarding tests that fail on any generic crate grammar branch.

## Technique Grounding Table

| spec-claim or T-P1 divergence id | published source cited | grounded / refuted / partial | bbnf-specific note |
|---|---|---|---|
| SK-V15-2C-GENERATOR-INPUT-SURFACE | Lock 14 defines the only legal fleet inputs as grammar source, workspace metadata, and optional per-grammar declaration crate at `restart/locks/LOCKS.md:349`; the grammar parser records source hashes and frontend closure facts at `skinny/crates/grammar/src/lib.rs:29`-`35`, `skinny/crates/grammar/src/lib.rs:60`-`64`, and `skinny/crates/grammar/src/lib.rs:87`-`112`. | grounded | The generator may consume `RuntimeSourceFacts`, `RuntimeFrontendSource`, imports, layout, host captures, and projection facts. It may not invent a CSS tokeniser in a Rust string literal and call the result grammar-derived. |
| SK-V15-2C-METADATA-GRAMMAR-OWNERSHIP | CSS regen declares fifteen CSS source inputs and workspace metadata at `skinny/xtask/src/regen_css.rs:5`-`24`; every CSS target repeats `source_inputs` and `metadata_inputs` at `skinny/xtask/src/regen_css.rs:26`-`97`; tests only prove seven CSS companions and fifteen sources at `skinny/xtask/src/regen_css.rs:147`-`169`. | partial | This is a useful ownership sketch, but it is CSS-only. The SK-V15 non-JSON proof matrix requires CSS plus Sheets or BBNF-self for generic surfaces at `restart/skinny/tranches/sk-v15/SPEC.md:206`-`217`. |
| SK-V15-2C-CSS-SYNTAX-TOKEN-SURFACE | W3C CSS Syntax Module Level 3, "Tokenization" and "Consume a token", `https://www.w3.org/TR/css-syntax-3/#tokenization`; local CSS token/value sources at `grammar/css/l4/stylesheet.bbnf:12`-`14`, `grammar/css/l4/selectors.bbnf:9`-`39`, and `grammar/css/l4/values.bbnf:23`-`63`. | grounded | CSS tokenization is not JSON punctuation. Byte-class and scanner primitives transfer only when the alphabet, escape policy, comments, identifiers, number starts, and block structure are generated facts. |
| SK-V15-2C-CSS-VALUE-API-SURFACE | CSS Typed OM Level 1 defines `CSSStyleValue.parse()` and value objects at `https://www.w3.org/TR/css-typed-om-1/#cssstylevalue-objects`; CSS Values and Units Level 4 defines value syntax and `calc()` at `https://www.w3.org/TR/css-values-4/#value-defs` and `https://www.w3.org/TR/css-values-4/#calc-notation`; current root runtime has `CssDocument`, walk/view methods, and typed values at `crates/core/src/runtime/css_l4/document.rs:10`-`79`, `crates/core/src/runtime/css_l4/document.rs:138`-`207`, and `crates/core/src/runtime/css_l4/value.rs:326`-`424`. | partial | The root runtime proves the desired value shape exists as code. It does not close CSS admission: the skinny live CSS path still emits strings from `CSS_GENERATED_RS`, and Pattern H provenance is absent. Closure requires the typed CSS value/document/view/visitor surface to be generator-derived and measured against same-workload `cssparser`/CSSOM comparators per `restart/skinny/tranches/sk-v15/SPEC.md:54`-`63`. |
| SK-V15-2C-CSS-CUSTOM-PROPERTIES-AND-CALC | CSS Custom Properties Level 1 defines `--*` custom properties and `var()` at `https://www.w3.org/TR/css-variables-1/#defining-variables`; local CSS grammar encodes `calc`, `min`, `max`, `clamp`, `var`, and `env` at `grammar/css/l4/values.bbnf:31`-`63`. | grounded | CSS Value API generation must preserve property/value semantics, not reduce values to generic strings or JSON number/string rules. |
| SK-V15-2C-SELECTOR-GENERALISATION | Selectors Level 4 defines selector families at `https://www.w3.org/TR/selectors-4/`; local selector grammar covers namespaces, attributes, pseudo-classes, pseudo-elements, combinators, and selector lists at `grammar/css/l4/selectors.bbnf:12`-`106`. | grounded | Selector rows need generated selector AST/value facts. Reusing a declaration-value token row or JSON object/member projection is a workload mismatch. |
| SK-V15-2C-CSSOM-COMPARATOR-BOUNDARY | CSSOM specifies the object model at `https://www.w3.org/TR/cssom-1/`; SK-V15 requires cssparser as near-term same-workload comparator and lightningcss only after comparable CSSOM/value output at `restart/skinny/tranches/sk-v15/SPEC.md:59`-`63`. | grounded | A brace-counter `CssFullParseSummary` is not CSSOM. CSS retime must compare typed document/value output, not rule-count summaries. |
| SK-V15-2C-CSS-BROADCAST-ADMISSION | The overfit audit records one W8R CSS measurement broadcast across 24 rows at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21`-`31`; current `RESULTS.md` repeats the same tuple beginning at `skinny/RESULTS.md:112`; W8 measurement loops one aggregate profile at `skinny/crates/bbnf-bench/src/css_l4_w8.rs:206`-`228`. | refuted | The 24-row CSS route is not row-local evidence. W1 must demote/collapse it per `DEP-W1-CSS-BROADCAST` at `restart/skinny/tranches/sk-v15/SPEC.md:194` and Section 4 at `restart/skinny/tranches/sk-v15/SPEC.md:264`-`281`. |
| SK-V15-2C-CSS-GENERATOR-SIDECAR | `emit_frontend_facts` writes `generated.rs` from `CSS_GENERATED_RS` at `skinny/crates/codegen/src/runtime_generator.rs:81`-`104`; the string literal starts at `skinny/crates/codegen/src/runtime_generator.rs:713`, emits fact streams at `skinny/crates/codegen/src/runtime_generator.rs:717`-`760`, defines `CssFullParseSummary` at `skinny/crates/codegen/src/runtime_generator.rs:762`-`771`, and produces a full-parse string at `skinny/crates/codegen/src/runtime_generator.rs:773`-`813`. | refuted | This is a generator-sidecar: CSS source facts become metadata in a hand-written runtime body, not grammar-derived parser/value generation. SK-V15 already routes it to W6 deletion only after W5 typed provider proof at `restart/skinny/tranches/sk-v15/SPEC.md:195`-`196`. |
| SK-V15-2C-GENERIC-GRAMMAR-SWITCH | `RuntimeGenerationMode::{PassCompiled, FrontendFacts}` at `skinny/crates/codegen/src/grammar_profile.rs:11`-`15`; `runtime_profiles() -> [&GrammarProfile; 8]` hand-lists JSON plus seven CSS profiles at `skinny/crates/codegen/src/grammar_profile.rs:89`-`99`; runtime emission dispatches on the mode at `skinny/crates/codegen/src/runtime_generator.rs:19`-`29`; CSS profile IDs are matched at `skinny/crates/codegen/src/runtime_generator.rs:114`-`153`. | refuted | This is the generic-crate grammar switch named in the task. It is JSON/CSS routing under different names and is not a future-grammar onboarding mechanism. |
| SK-V15-2C-PATTERN-H-COLLAPSE | Pattern H census is 67 current files by `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`; overfit audit records 5/9 true template, 4/9 bespoke, and 0/67 line-1 generated headers at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:37`-`43`; simple-template scope is explicit at `crates/core/src/runtime/builder_template.rs:17`-`25` and `crates/core/src/runtime/builder_template.rs:43`-`63`; CSS remains bespoke with `OpenFrame` at `crates/core/src/runtime/css_l4/builder.rs:15` and legacy path shim at `crates/core/src/runtime/css_l4/parse_with.rs:1`-`10`. | partial | The simple cohort is the success pattern; root CSS/JSON/Sheets/BBNF bespoke files are not collapsed. W4 may add provenance and checks, but destructive deletion is blocked until generated replacement exists per `DEP-W4-PATTERN-H-PROVENANCE` and `DEP-W4-W6-CSS-LEGACY-RUNTIME-SHIM` at `restart/skinny/tranches/sk-v15/SPEC.md:198`-`199`. |
| SK-V15-2C-SHEETS-FUTURE-GRAMMAR | Sheets grammar defines numbers, doubled-quote strings, error literals, sheet prefixes, cell/range references, operators, function calls, LET, LAMBDA, and array rows at `grammar/google-sheets/google-sheets.bbnf:6`-`18`, `grammar/google-sheets/google-sheets.bbnf:34`-`84`, and `grammar/google-sheets/google-sheets.bbnf:97`-`160`; current typed value shape is `SheetsValue` at `crates/core/src/runtime/google_sheets/value.rs:3`-`13`. | grounded | Sheets is the anti-JSON falsifier. It needs function/reference/operator facts and typed values from source, not object/array/pair mining or JSON sink callbacks. |
| SK-V15-2C-BBNF-SELF-FUTURE-GRAMMAR | BBNF-self grammar imports, literals, regexes, closures, alternation, directives, and grammar items at `grammar/bbnf/bbnf.bbnf:4`-`15`, `grammar/bbnf/bbnf.bbnf:41`-`52`, and `grammar/bbnf/bbnf.bbnf:54`-`85`. | grounded | BBNF-self exercises grammar-source ownership recursively. It must onboard with the same generator contract, without special generic code for `Bbnf`. |
| SK-V15-2C-GATE-EXCLUSION | Lock 14 scan roots include `crates/codegen/src/lib.rs`, `lower`, `grammar_profile`, passes, runtime lib/tape, and IR at `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2370`-`2379`, but exclude `runtime_generator.rs`, `grammar_provider.rs`, `json_sink_direct.rs`, `json_typed_direct.rs`, and `json_templates`; overfit audit names this omission at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:45`-`47`. | refuted | A gate that excludes the leak roots cannot evidence Lock 14. SK-V15 requires inclusion and exclusion reporting at `restart/skinny/tranches/sk-v15/SPEC.md:64`-`68` and `restart/skinny/tranches/sk-v15/SPEC.md:219`-`236`. |
| SK-V15-2C-FUTURE-GRAMMAR-ONBOARDING-TEST | Lock 14 says adding a grammar is a config plus grammar-source change with no generic-crate code change at `restart/locks/LOCKS.md:349`; SK-V15 proof receiver matrix requires CSS plus Sheets or BBNF-self for generic surfaces at `restart/skinny/tranches/sk-v15/SPEC.md:206`-`217`. | grounded | The onboarding test is not "add enum arm and pass." It is: add grammar source/metadata, run regen/check, observe generated files or gate-consumed rejected alternative, and fail if any generic owner path changes except generated manifests. |

## Architectural Assertions Defended

1. Grammar-neutrality is an input-surface property, not a prose label. The
   admissible inputs are grammar source, workspace metadata, and optional
   per-grammar host/declaration crates. The local grammar parser already
   exposes source hashes, imported source closure, layout directives, host
   captures, and projections (`skinny/crates/grammar/src/lib.rs:60`-`112`);
   those are the facts a generator can legally consume.

2. CSS L4 requires a grammar-derived typed value/document surface. W3C CSS
   Syntax, Values, Variables, Selectors, Typed OM, and CSSOM together define a
   token stream, value grammar, selector model, typed value API, and object
   model. The current root runtime has `CssDocument`, `CssView`, `CssFocus`,
   `CssDocumentKind`, and `CssTypedValue` (`crates/core/src/runtime/css_l4/document.rs:10`-`79`,
   `crates/core/src/runtime/css_l4/document.rs:138`-`207`; `crates/core/src/runtime/css_l4/value.rs:413`-`424`).
   That is the correct target shape for SK-V15 W5/W6, provided it becomes
   generated and row-measured rather than hand-owned Pattern H code.

3. Pattern H collapse is structural, not file-count deletion. SK-V15's own
   contract says the root runtime file count remains exactly 67 while line-1
   provenance and non-writing regen/check evidence are added
   (`restart/skinny/tranches/sk-v15/SPEC.md:69`-`70`, `restart/skinny/tranches/sk-v15/SPEC.md:198`). The simple
   runtime template documents the right abstraction boundary: it covers only
   grammars whose values are span/unit/compound and says richer grammars emit
   their own projection shape from metadata (`crates/core/src/runtime/builder_template.rs:17`-`25`).

4. Future grammar onboarding must use negative controls. CSS L4 alone is not
   enough because CSS-specific generator tables can still overfit. Sheets
   (`grammar/google-sheets/google-sheets.bbnf:141`-`160`) and BBNF-self
   (`grammar/bbnf/bbnf.bbnf:54`-`85`) force function calls, references,
   arrays, directives, alternations, closures, and import ownership that JSON
   and CSS cannot fake.

5. Fact streams may remain diagnostic or narrow product rows, but not as a
   hidden substrate. CSS fact streams are valid if classified as output rows
   with gate-consumed telemetry; they cannot serve as a retained EventTape,
   CSSOM, or typed Value API proof. SK-V15 requires `CSS_GENERATED_RS`,
   `CssFullParseSummary`, fact-stream-only `parse()`, and brace-counter output
   to retire from live CSS admission (`restart/skinny/tranches/sk-v15/SPEC.md:56`-`58`).

## Architectural Assertions Refuted

| assertion | disposition | reason |
|---|---|---|
| JSON-only routes can be promoted to fleet-wide grammar-neutrality. | refuted | JSON honest rows do not prove CSS, Sheets, or BBNF-self. JSON `JsonValue`/object-array-pair shape at `crates/core/src/runtime/json/value.rs:3`-`39` is a grammar-specific value model, not a generic grammar model. |
| CSS-only provider tables are enough to prove generalisation. | refuted | `regen_css.rs` hard-lists seven CSS targets and fifteen CSS files (`skinny/xtask/src/regen_css.rs:5`-`97`). It proves a CSS cohort table, not arbitrary grammar onboarding. |
| `CSS_GENERATED_RS` is grammar-derived generation. | refuted | The parser body is a string literal in `skinny/crates/codegen/src/runtime_generator.rs:713`-`1359`; source facts are emitted as metadata lines, while parser/tokeniser logic lives in the sidecar body. |
| `CssFullParseSummary` is same-workload CSSOM or Value API evidence. | refuted | It records counts for rules, at-rules, qualified rules, and declarations at `skinny/crates/codegen/src/runtime_generator.rs:765`-`771`; CSSOM and Typed OM require object/value representations, not brace-counter summaries. |
| The 24 CSS L4 rows are 24 independent admits. | refuted | One W8 aggregate measurement is multiplied across feature rows (`skinny/crates/bbnf-bench/src/css_l4_w8.rs:206`-`228`; `skinny/RESULTS.md:112`-`135`). SK-V15 W1 must demote or collapse it. |
| `RuntimeGenerationMode` plus `runtime_profiles()` is a future grammar switch. | refuted | The mode split and eight-profile array are generic-crate grammar switches (`skinny/crates/codegen/src/grammar_profile.rs:11`-`15`, `skinny/crates/codegen/src/grammar_profile.rs:89`-`99`) and violate the no-code-change onboarding rule. |
| Current Lock 14 scan output proves grammar-neutrality. | refuted | The scan root list omits `runtime_generator.rs`, `grammar_provider.rs`, JSON direct generators, and JSON templates (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:2370`-`2379`). Excluding the leak roots is a contrivance. |
| Current CSS root runtime Value API closes SK-V15 CSS. | partial only | Typed CSS code exists in `crates/core/src/runtime/css_l4/`, but lacks line-1 generator provenance and is not the skinny live admission path. It becomes close evidence only after generator-derived provenance, semantic equality, and same-workload retiming. |

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| What is the smallest generated manifest that can replace `runtime_profiles()` without changing public codegen callers? | Prototype a generated provider manifest for JSON plus seven CSS profiles, then add Sheets or BBNF-self without editing `skinny/crates/codegen/src/grammar_profile.rs`; gate on diff and Lock 14 scan. |
| Can the root CSS `CssTypedValue` surface be emitted from existing grammar facts without hand-coded CSS branches? | Run a non-writing generator proof over `grammar/css/l4/*.bbnf` plus metadata and compare emitted `value.rs`, `document.rs`, `view.rs`, and builder projection against the current root runtime. |
| Which root runtime files in the 67-file Pattern H census are true generated-output candidates versus intentional hand-owned generic templates? | Produce a line-1 provenance scan, group by generator owner, and require non-writing regen/check for every generated candidate; record intrinsic blocks for any hand-owned generic helper. |
| Which CSS comparator plane is near-term closeable: typed declaration/value tree versus fuller CSSOM stylesheet tree? | Build row-local equality against `cssparser` first, then only admit lightningcss when Track 1 emits comparable CSSOM/value output, per `restart/skinny/tranches/sk-v15/SPEC.md:61`-`63`. |
| What future grammar should be the first non-CSS negative-control receiver? | Prefer Sheets if validating function/reference/value materialization; prefer BBNF-self if validating import/directive/grammar-source recursion. Either must fail if a generic crate changes. |

## LOCKS-AMENDMENTS-CANDIDATE

| id | lock(s) | amendment candidate | evidence | close test |
|---|---|---|---|---|
| LAC-2C-SK15-01 generated provider manifest | Lock 14, Lock 6 | Generic codegen may consume a generated provider manifest, but may not hand-code `RuntimeGenerationMode`, profile arrays, CSS profile matches, or JSON/CSS runtime families in non-generated source. | `skinny/crates/codegen/src/grammar_profile.rs:11`-`15`, `skinny/crates/codegen/src/grammar_profile.rs:89`-`99`; `skinny/crates/codegen/src/runtime_generator.rs:19`-`29`, `skinny/crates/codegen/src/runtime_generator.rs:114`-`153`. | Add a Sheets or BBNF-self provider with grammar source/metadata only; no generic owner-path edit except generated manifest output. |
| LAC-2C-SK15-02 CSS value API close boundary | Lock 1, Lock 8, Lock 14 | CSS typed value/document/view/visitor surfaces count as close evidence only when generated from grammar source and same-workload measured; fact-stream and brace-counter outputs are diagnostic. | Root typed surface at `crates/core/src/runtime/css_l4/document.rs:10`-`79` and `crates/core/src/runtime/css_l4/value.rs:413`-`424`; contrived live path at `skinny/crates/codegen/src/runtime_generator.rs:713`-`813`; SK-V15 close condition at `restart/skinny/tranches/sk-v15/SPEC.md:54`-`63`. | CSS rows emit typed values, row-local equality, and cssparser comparator evidence; `CSS_GENERATED_RS` and `CssFullParseSummary` absent from live admission paths. |
| LAC-2C-SK15-03 broadcast duplicate gate | Lock 8, Lock 14 | Any repeated throughput tuple across distinct row IDs is diagnostic unless each row has independent command, input, equality, timing, and `measurement_row_id`. | Overfit audit at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21`-`31`; repeated `RESULTS.md` rows at `skinny/RESULTS.md:112`-`135`. | Gate rejects duplicate CSS tuple signatures unless a broadcast group is explicitly non-admit. |
| LAC-2C-SK15-04 Pattern H provenance before deletion | Lock 6, Lock 14 | Pattern H collapse means 67/67 line-1 generated provenance plus regen/check proof; deletion is blocked unless replacement provider lands no later than the deletion consumer. | SK-V15 dependency row at `restart/skinny/tranches/sk-v15/SPEC.md:198`; simple-template boundary at `crates/core/src/runtime/builder_template.rs:17`-`25`; CSS bespoke builder at `crates/core/src/runtime/css_l4/builder.rs:15`. | `find` census remains 67, line-1 header scan returns 67, and non-writing regen/check covers every root runtime file claimed generated. |
| LAC-2C-SK15-05 full-surface Lock 14 scan | Lock 14, Lock 16 | Lock 14 gates must report included roots, excluded roots, owner, reason, self-scan status, primitive status, gate consumer, affected rows, and disposition. Excluding a same-change leak path is a failing finding. | Current scan roots at `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2370`-`2379`; SK-V15 gate table at `restart/skinny/tranches/sk-v15/SPEC.md:219`-`236`. | Gate output lists exclusions and fails while `runtime_generator.rs`, `grammar_provider.rs`, JSON direct generators, or templates are omitted from relevant generic scans. |
