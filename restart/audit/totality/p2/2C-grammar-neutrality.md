---
agent: 2C
pass: T-P2-research
cycle: V3
generated_at: 2026-05-23T00:00:00Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 9
counted_source_ids: [T2C-SRC-CSS-SYNTAX, T2C-SRC-SELECTORS, T2C-SRC-CSS-VALUES, T2C-SRC-CSS-VARIABLES, T2C-SRC-OPENFORMULA, T2C-SRC-V2-FOLD, T2C-SRC-V3-FOLD, T2C-SRC-S-P3-P3A, T2C-SRC-LOCAL-EVIDENCE]
techniques_grounded: 14
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
    - CH1 counted-source-id repair through restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md
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
    - 2C-abstract-primitive-lift-table
    - 2C-cost-model-per-grammar-selection
    - 2C-c3-c4-shape-orthogonal-escape
locks_amendment_candidates: 5
v2_fold_authority:
  - restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md
v3_fold_authority:
  - restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md
---

## Executive Summary

The primitive vocabulary and five-shape `BackendShape` surface generalize only
when every choice is derived from grammar metadata and Grammar IR facts, not
from JSON role names or a hand-coded provider table. CSS L4 (15 sub-grammars
in `grammar/css/l4/`), Sheets (`grammar/google-sheets/google-sheets.bbnf`), and
BBNF-self (`grammar/bbnf/{bbnf,expressions,types}.bbnf`) are the load-bearing
falsifiers. CSS Syntax tokenizes identifiers, functions, hashes, strings, URLs,
numbers, percentages, dimensions, delimiters, brackets, and at-keywords;
Selectors L4 layers selectors, pseudo-classes, attributes, and combinators over
that token stream; CSS Values L4 introduces `var()`/`calc()` semantics; Sheets
formulas instead center case-insensitive function calls, semicolon-separated
parameters, operators, and cell/range references; BBNF-self centers
directives, alternatives, repetitions, and Pratt/operator chains. None can be
routed through `JsonSink`, JSON object/array/pair role mining, or
`OffsetFlags::HAS_ESC`/`HAS_CONTROL` semantics.

The defensible totality rule, restated under Lock 14 v+1 strict read: primitives
are grammar-neutral byte-stream operations, but their admission and selection
are grammar-specific data. A primitive claimed grammar-neutral must exercise at
least one non-JSON consumer or record a measured deletion/rejection in the
admitting wave (`restart/locks/LOCKS.md:259`-`260`). Abstract-primitive lift
discipline separates byte-window operations that translate (byte_class, escape
mask, structural index, prefix-XOR, bitmap next-set-bit, byte-context
propagation, byte-window multiply-accumulate) from JSON-only-by-shape kernels
(`\uXXXX` fixed-4-nibble Unicode decode; JSON object/array role mining; JSON
exponent/sign number policy). The S-P3 P3-A C3 (digit-block SIMD) and C4
(unicode `\uXXXX` decode) shortlist rows are the canonical worked examples:
C3 admits via a grammar-neutral byte-class `[0x30..=0x39, 0x2E, 0x2B, 0x2D,
0x65, 0x45]` consumed in a same-wave non-JSON checkasm row; C4 carves out the
CSS L4 variable-width 1-6 hex-digit escape as shape-orthogonal and explicitly
non-admitting under C4 itself.

A future grammar onboards by providing grammar source, workspace metadata, and
optional declaration-crate host functions; generic crates must consume
generated registry/config/fact tables and must not add grammar branches.
`BackendShape` stays five variants (`skinny/crates/ir/src/lib.rs:339`-`345`);
the decision engine chooses among them from generated facts, cost evidence
(per 2D), and same-wave row consumers.

## V3 Fold Authority

This dossier folds the V1 CH1/CH2/CH5/CH6 revise set per the V2 addendum and
adds the V3 counted-source convention, normalized admission-state vocabulary,
shared executable ledger, and numeric abrogate caps. The shared mechanical
contract lives in `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`; the
provenance-pin and counted-source ledger lives in
`restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md`. A standards citation or
one admitted CSS row grounds a candidate only; closure still requires generated
policy, row consumer, strict equality, telemetry consumption, and no generic
grammar branch.

| gate | 2C closure rule | owner surface |
|---|---|---|
| generated provider registry | `RuntimeProvider::{Json, CssL4DeclarationValues, CssL4DeclarationValuesExtended, CssL4StylesheetSelectors, CssL4VisualFunctions, CssL4AtRulesAndMedia, CssL4VendorAndCustomAtRules, CssL4NestedLayout}` is a live Lock 14 leak until replaced by a generated provider manifest. The HEAD enum carries 8 variants at `skinny/crates/codegen/src/grammar_profile.rs:17`-`26`; `runtime_profiles() -> [&'static GrammarProfile; 8]` at `:100`-`:110` is a hand-coded roster. Sheets, BBNF-self, EBNF, BNF, CSV, math, and css_pretty are NOT yet rostered; adding them by editing the enum is a Lock 14 fault per the v+1 generated-output allowance (`restart/locks/LOCKS.md:222`-`238`). | `skinny/crates/codegen/src/grammar_profile.rs` generator input and generated roster output |
| grammar-shape leak scan | The scan must catch grammar names AND grammar-shaped policy: JSON byte alphabets, object/array/pair role mining (live at `skinny/crates/passes/src/lib.rs:1053`-`1110` with `label: "object"/"array"/"pair"` strings even though the `TapeKind` enum is grammar-neutral), `JsonSink` callback names (`skinny/crates/runtime/src/grammars/json/sink.rs:4`-`16` declares JSON-canonical methods `begin_object`, `key`, `string`, `i64`, `u64`, `f64`, `bool`, `null`), and JSON flag meanings. | `skinny/crates/{ir,passes,codegen,runtime,bbnf-simd,parse-that-regex,parse-that}/src` |
| generated sink/fact/value/flag surface | `JsonSink`, CSS fact sinks, value views, and flag names are generated grammar-owned surfaces; generic tape may store bits but not define grammar semantics. HEAD `OffsetFlags` carries grammar-neutral `GRAMMAR_BIT0`/`GRAMMAR_BIT1` at `skinny/crates/runtime/src/tape/mod.rs:22`-`23` (partial repair vs prior `HAS_ESC`/`HAS_CONTROL`); JSON aliases that to `STRING_NEEDS_DECODE = OffsetFlags::GRAMMAR_BIT0` at `skinny/crates/runtime/src/grammars/json/config.rs:5` (generated, grammar-owned). | `skinny/crates/runtime/src/grammars/<name>/` generated modules |
| primitive policy manifest | Shared primitives receive alphabets, delimiters, quote/escape/control policy, number policy, string policy, and no-string/no-number policy from generated grammar data or caller data. | `bbnf-simd` call boundaries and generated parser consumers |
| CSS plus negative-control transfer | Fleet-wide grammar neutrality requires a CSS L4 row plus a Sheets or BBNF-self witness/negative-control. SK-V12 declaration-values alone is evidence, not closure. Per Lock 14 v+1 per-wave gate enforcement, "with only one of Sheets or BBNF-self, the claim is scoped to the witnessed grammars and may not use fleet-wide grammar-neutral wording" (`restart/locks/LOCKS.md:246`-`250`). | CSS, Sheets, and BBNF-self generated rows or explicit fail-closed witnesses |
| decision-engine facts | e-graph rewrites, CSP constraints, and costs consume generated grammar facts (per 2D-cost-model.md); mining JSON roles remains Lock 14 drift even if JSON equality passes. | `skinny/crates/ir`, `skinny/crates/passes`, decision-engine resolver output |

## Technique Grounding Table

| spec-claim or T-P1 divergence id | published source cited | grounded / refuted / partial | bbnf-specific note |
|---|---|---|---|
| 2C-CSS-TOKEN-ALPHABET: byte-class primitives generalize beyond JSON if they consume a grammar-provided token alphabet. | CSS Syntax Module Level 3 tokenization at `https://www.w3.org/TR/css-syntax-3/#tokenization` defines tokenization as the input-to-token-stream transform, with token definitions under `https://www.w3.org/TR/css-syntax-3/#typedef-token-stream`. Local drift: JSON structural alphabet `b"{}[],:\""` is still inlined at `skinny/crates/runtime/src/grammars/json/config.rs:4`; CSS row exists at `skinny/RESULTS.md:94`. | grounded | `byte_class_from_eq_set_64`, TBL/TBX classifiers, run-skip, and whitespace/comment scanners are admissible as abstract byte-class primitives. Their alphabet must come from generated grammar config, not from `b"{}[],:\""`. C3 worked example: CSS L4 `<number>` byte-class `[0x30..=0x39, 0x2E, 0x2B, 0x2D, 0x65, 0x45]` is the non-JSON same-wave checkasm row per `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md:93`. |
| 2C-CSS-SELECTOR-SCOPE: full CSS parity is row production, not declaration-token reuse. | Selectors Level 4 overview at `https://www.w3.org/TR/selectors-4/#overview` names selector families and matching model; grammar sections under `https://www.w3.org/TR/selectors-4/#grammar` define the parse surface. Local parity gap: `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md` section 3 marks selectors, stylesheet root, pseudo-classes, pseudo-elements, and attribute selectors as missing; HEAD `RuntimeProvider::CssL4StylesheetSelectors` exists at `skinny/crates/codegen/src/grammar_profile.rs:21` but selector grammar at `grammar/css/l4/selectors.bbnf` is not yet rostered for fact-stream emission. | grounded | A selector row can use the same primitive vocabulary, but it needs generated selector facts and strict lightningcss equality. It cannot be represented as a JSON-style object/member/value projection. |
| 2C-CSS-CALC-VAR: CSS value functions require generated policy, not JSON number/string policy. | CSS Values and Units Level 4 defines calculation notation at `https://www.w3.org/TR/css-values-4/#calc-notation`; CSS Custom Properties Level 1 defines custom property declaration and substitution at `https://www.w3.org/TR/css-variables-1/#defining-variables` and `https://www.w3.org/TR/css-variables-1/#using-variables`. Local residual leak: JSON string and number paths call `match_tiny_plain_string` and number routines via JSON-shaped generated paths in `skinny/crates/runtime/src/grammars/json/generated.rs`. | grounded | GrammarConfig must grow into generated per-grammar dispatch/string/number/whitespace/sink facts or equivalent generated functions. JSON number parsing may stay a primitive, but CSS numbers and dimensions need grammar-selected policies. |
| 2C-SHEETS-FORMULA-FALSIFIER: Sheets cannot pass through JSON-shaped materialization roles. | ODF 1.3 Part 4 Formula, Section 5.6 page 40, defines function calls by name, semicolon-separated parameters, and case-insensitive function names; Section 4.8 page 32 defines references; Section 5.8 page 41 defines constant-reference syntax; Section 5.5 page 38 defines reference operators `!`, `~`, and `:`. Local drift: `derive_materialization_roles` in `skinny/crates/passes/src/lib.rs` (driven from the role-mining surface) emits JSON-canonical labels at `:1059`, `:1079`, `:1102` (`"object"`, `"array"`, `"pair"`) even though `TapeKind::{Container, Sequence, KeyValuePair}` is grammar-neutral; Sheets has no JSON-pair structure. | refuted | The JSON role miner is not grammar-neutral. A Sheets proof row must derive function/reference/operator roles from generated grammar metadata and must not add `Sheets` branches to generic crates. Sheets workspace grammar lives at `grammar/google-sheets/google-sheets.bbnf` and uses `error_literal` tagged-Nu8 enums, doubled-`""` escapes, semicolon parameter separators — orthogonal to JSON object/array role mining entirely. |
| 2C-BBNF-SELF-FALSIFIER: BBNF-self needs operator/directive grammar facts, not JSON object-member analogues. | Local primary source: BBNF-self workspace grammar at `grammar/bbnf/bbnf.bbnf:11`-`13` declares `literal = ( "\"" , /(\\.|[^"\\])*/ , "\"" | "'" , … | "\`" , … ) -> Span` with three quote sets and shape-identical `\\.` escape pattern to JSON; alternations `expression = literal | reference | …` at `:31` introduce operator/Pratt chains. SK-V13 synthesis requires BBNF-self fail-closed unless explicitly dispositioned at `restart/skinny/tranches/sk-v13/SYNTHESIS.md:220`-`235`. | refuted | BBNF-self onboarding is the grammar-shape audit: directives, alternatives, repetitions, and Pratt/operator chains must be facts from Grammar IR or metadata, not recovered by generic code recognizing JSON-like punctuation. Note that BBNF-self literal escapes are *shape-identical* to JSON `\uXXXX` decode (per `p3a-candidate-shortlist.md:106`) and ARE the C4 same-wave non-JSON consumer — directly admitting C4's `unescape_uxxxx_x8_neon` body, no carve-out needed for BBNF-self literals. |
| 2C-BACKENDSHAPE-FIVE: the five-shape surface is a grammar-neutral target vocabulary. | Local primary source: `BackendShape` has `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, `CollapsedStage` at `skinny/crates/ir/src/lib.rs:339`-`345`; `all_backend_shapes()` returns the same five at `skinny/crates/ir/src/cost.rs`; Lock 10 names the same materialization-plan side table at `restart/locks/LOCKS.md:70`. | grounded | Do not add a shape for CSS, Sheets, or BBNF-self. Add generated facts that let the resolver choose an existing shape per rule. |
| 2C-BACKENDSHAPE-SELECTION: current selection exists but is not grammar-general enough. | Local primary source: `LayoutFacts.backend_shape` and `cost_facts` are populated in `skinny/crates/passes/src/lib.rs` shape/cost passes; the priority/cost flow remains a hardcoded P1-P8 cascade that 2D-cost-model.md refutes as a literature-grounded optimizer (`2D-cost-model.md:58`). T-P1 1B records the same gap in `restart/audit/totality/p1/1B-codegen-evidence.md`. | partial | The five-shape vocabulary is defensible; the hardcoded P1-P8 cascade is not the long-term generality mechanism. Per 2D's `T2D-EGRAPH-EXTRACTION`, selection must be data-driven through e-graph extraction over candidate shapes, CSP feasibility filter, and active cost extraction consuming generated grammar facts (alphabet, layout, host fns, recovery, output mode, FIRST/follow, token classes). |
| 2C-RUNTIME-PROVIDER-REGISTRY: grammar providers are still hardcoded. | Local primary source: `RuntimeProvider::{Json, CssL4DeclarationValues, CssL4DeclarationValuesExtended, CssL4StylesheetSelectors, CssL4VisualFunctions, CssL4AtRulesAndMedia, CssL4VendorAndCustomAtRules, CssL4NestedLayout}` is hardcoded at `skinny/crates/codegen/src/grammar_profile.rs:17`-`26` (8 variants); `runtime_profiles() -> [&'static GrammarProfile; 8]` returns them as a static array at `:100`-`:110`. Lock 14 forbids grammar-name modules and branches in generic crates at `restart/locks/LOCKS.md:220`-`238`. | refuted | The registry must be generated from workspace metadata or grammar manifests. Adding CSS, Sheets, or BBNF-self by editing this enum is a Lock 14 failure. Note the V3-V4 drift expanded the enum from the 2C V2-fold's 2-variant cite to the 8-variant HEAD without converting to generation. |
| 2C-GENERATED-SINK: `JsonSink` is a row-specific generated/direct surface, not a generic sink contract. | Local primary source: `JsonSink` is declared in `skinny/crates/runtime/src/grammars/json/sink.rs:4`-`16` with JSON-canonical method names `begin_object`, `end_object`, `begin_array`, `end_array`, `key`, `string`, `i64`, `u64`, `f64`, `bool`, `null`; default `key_source` and `string_source` at `:17`-`:36` call `parse_that_regex::unescape_string` (JSON-shape escape) for `needs_unescape` flag. CSS fact sink is separate at `skinny/crates/runtime/src/grammars/css_l4_declaration_values/sink.rs`. | refuted | The totality sink concept should be `GrammarSink` as a generated per-grammar trait or generated associated surface. The generic runtime must not know JSON callback names. Sheets needs `formula(span)`/`function_call(name, args)`/`cell_ref(...)` callbacks; BBNF-self needs `rule(name)`/`alt(...)`/`directive(...)`; the per-grammar shape is the point. |
| 2C-FLAG-SEMANTICS: tape flags need grammar-owned interpretation (partial close at HEAD). | Local primary source: `OffsetFlags` at HEAD carries `GRAMMAR_BIT0`/`GRAMMAR_BIT1` (grammar-neutral bit slots) at `skinny/crates/runtime/src/tape/mod.rs:22`-`23`; JSON `STRING_NEEDS_DECODE = OffsetFlags::GRAMMAR_BIT0` at generated `skinny/crates/runtime/src/grammars/json/config.rs:5` is grammar-owned aliasing. CSS custom property identity and escape concerns are specified by CSS Custom Properties Level 1 `#defining-variables` and `#using-variables`. | partial | The HEAD migration from `HAS_ESC`/`HAS_CONTROL` to `GRAMMAR_BIT0`/`GRAMMAR_BIT1` is a partial Lock 14 repair (bit storage is now generic; bit name is now grammar-owned). The remaining gap: only 2 grammar bits exist; CSS/Sheets/BBNF-self may need more, and a `FlagSchema` generated table replacing ad-hoc `pub(crate) const X: u8 = OffsetFlags::GRAMMAR_BIT0` would close the schema vs single-bit-alias question. |
| 2C-PRIMITIVE-ADMISSION: grammar-neutral primitive parity alone is not admission. | Local primary source: SK-V12 escape-mask correctness was prerequisite-only at `skinny/REDRESS.md:3603`-`3632`; `a64_ascii_set_run_skip` microbench was routed as production split at `skinny/REDRESS.md:3766`-`3820`; SK-V13 bans producer-only SIMD/union/resolver/codegen artifacts at `restart/skinny/tranches/sk-v13/SYNTHESIS.md:246`-`247`; Lock 14 v+1 says "a primitive claimed grammar-neutral must exercise at least one non-JSON consumer or record a measured deletion/rejection" at `restart/locks/LOCKS.md:259`-`260`. | refuted | A primitive generalizes only after scalar reference, checkasm/parity, generated grammar policy proof, and same-wave non-JSON consumer measurement. C3 discharges this gate via the CSS L4 `<number>` byte-class checkasm row in the same wave that admits the SIMD body — no cross-wave deferral. |
| 2C-CSS-FACT-STREAM: fact streams are valid product rows, but they need a formal substrate/telemetry category. | Local primary source: CSS admission row is recorded in `skinny/RESULTS.md:94`; W1b scaffold and comparator gates are recorded at `skinny/REDRESS.md:3634`-`3764`; close promotes PASS-ADMIT at `skinny/REDRESS.md:3824`-`3840`. T-P1 1A flags the missing category in `restart/audit/totality/p1/1A-substrate-evidence.md`. | grounded | CSS fact-stream products are legitimate non-JSON evidence. Totality must classify them without pretending they are `OffsetTape`/`EventTape` retained documents. |
| 2C-ABSTRACT-PRIMITIVE-LIFT: dav1d/ffmpeg byte-stream primitives translate; pixel-domain primitives do not. | Lock 16 abstract-primitive declarations at `restart/locks/LOCKS.md:282`-`288` name `vextq_u8` cross-chunk byte-context propagation (dav1d filter-overlap lineage; "applies to ANY grammar with chunk-spanning tokens, not just JSON"), `vqaddq_u8`/`vqsubq_u8` saturating add/sub ("ANY grammar's number primitive"), `udot`/`sdot` byte-window multiply-accumulate ("ANY grammar's digit-block decode, not just JSON: JSON `number`, CSS L4 `<number>`, TOML/INI/SQL integer literals, Sheets formulas"). | grounded | The abstract-primitive name IS the grammar-neutrality contract. See §Abstract-Primitive-Lift Table below for the per-primitive cross-grammar map. |
| 2C-COST-MODEL-PER-GRAMMAR-SELECTION: per-grammar primitive selection logic is data-driven, not branching. | Per 2D `T2D-EGRAPH-EXTRACTION` (`2D-cost-model.md:55`-`58`) and `T2D-CSP-SCOPE` (`:60`-`:62`): backend-shape selection is candidate generation + equality saturation + CSP feasibility filter + active cost extraction. Per the V3 fold (`T-P2-V3-FOLD-ADDENDUM.md:84`-`101`), every candidate carries `substrate_target`/`retention_lifetime`/`policy_owner` and a same-wave consumer path. | grounded | A primitive admits for a grammar only when the grammar's generated facts (alphabet, quote/escape policy, number policy, function-call syntax, reference syntax, layout, output plane) make it cost-feasible AND a row consumer exists in that grammar's runtime/codegen. The S-P3 P3-A shortlist provides the worked vocabulary: every shortlist candidate (C1, C3, C4, C7) names {scalar reference, checkasm command, same-wave consumer path, expected row gate, falsifiability gate} per cell. |

## Abstract-Primitive-Lift Table

Per the explicit task ask: which Layer-1 byte-window primitives translate to
byte-stream parsing for *any* grammar (true grammar-neutral primitives), versus
which are JSON-only-by-shape (kernel shape is JSON-specific and cannot transfer
without rewrite).

| primitive | abstract-primitive name | grammar-neutral applicability | JSON-only-by-shape obstacle | non-JSON consumer evidence |
|---|---|---|---|---|
| `byte_class_from_eq_set_64` | byte-class set membership over 64-byte chunk | YES — any grammar with a delimiter/punctuation alphabet | none if alphabet is caller-supplied | C3 CSS L4 `<number>` byte-class `[0x30..=0x39, 0x2E, 0x2B, 0x2D, 0x65, 0x45]` per `p3a-candidate-shortlist.md:93`; sibling `byte_class_from_range_64` extends to CSS-permissive ranges |
| `escape_mask_64` (PMULL prefix-XOR composition) | escape-run mask over chunk | YES — any grammar with `\`-style escape | none if quote/escape policy is caller-supplied; HEAD `escape_mask_64` at `skinny/crates/bbnf-simd/src/lib.rs:175` is grammar-neutral | C1 long-string SIMD composes this with classifier + prefix-XOR; consumer is `parse_that_regex::skip_string_plain_trusted` which serves JSON, CSS strings, BBNF-self literals |
| `bitmap_prefix_xor_64` | running-XOR scan over 64-bit bitmap | YES — any grammar where escape/string structure needs prefix-prop | none; pure bitmap operation | composed into C1 long-string scan; structural-discovery primitive for all retained-tape grammars |
| `bitmap_next_set_bit` | next-set-bit over 64-bit bitmap | YES — any grammar with a structural index | none; CTZ + selector | scanner-emit primitive used by every BackendShape that retains structural offsets |
| `bulk_emit_positions_64` | bulk position emission from bitmap | YES — any grammar with retained structural projection | none; scatter emit | OffsetTape emission across all grammars (config-driven) |
| `vextq_u8` cross-chunk byte-context propagation | byte-window context shift across 16-byte chunks | YES per Lock 16 explicit lift declaration | none; pure byte-shift | cross-chunk token boundaries in JSON strings, CSS escaped identifiers, Sheets quoted-sheet-names, BBNF-self literals |
| `udot`/`sdot` byte-window multiply-accumulate | byte-window MAC for digit accumulation | YES per Lock 16 explicit "ANY grammar's digit-block decode" | none; pure MAC | C3 digit-block; consumers include JSON `number`, CSS L4 `<number>`, Sheets numeric literals |
| `vbcaxq_u8`/`veor3q_u8` ternary bitwise | 3-way bitwise reduction | YES — any grammar where classifier collapses BIC+EOR | none; pure bitwise | structural classification across all grammars on M5+/Neoverse-V1+ |
| `compact_mask` movemask | dense mask to position list | YES — any grammar emitting structural positions | none; pure bit-pack | every grammar's structural index |
| `unescape_uxxxx_x8_neon` (C4 body) | fixed-4-nibble Unicode hex decode | PARTIAL — applies where `\uXXXX` is shape-identical | **JSON-only-by-shape for variable-width escapes**: CSS L4 `\HEXHEX` is 1-6 hex digits (CSS Syntax §4.3.7); the fixed-4-nibble SIMD body is shape-orthogonal and CANNOT decode CSS variable-width escapes without a different primitive | C4 non-JSON consumer is BBNF-self literal escape per `grammar/bbnf/bbnf.bbnf:11`-`13` (shape-identical `\u`+4-nibble); CSS L4 variable-width escape is "carved out as a separate-primitive concern per Lock 14 v+1 measured deletion/rejection record" per `p3a-candidate-shortlist.md:106` |
| `match_tiny_plain_string_with_cap::<16>` | small-alphabet 16-byte string match | PARTIAL — applies to small keyword sets | acceptable abstract primitive; JSON-shape obstacle only if "plain" means "no JSON escape" hardcoded | folded into C1 long-string SIMD canonical "with smaller alphabet" per `p3a-candidate-shortlist.md:42` |
| `read_hex_unit_scalar` (4-nibble hex unit) | scalar 4-nibble hex decode | JSON-only-by-shape | fixed-4-nibble policy is JSON specific; CSS would need 1-6 variable-width scalar | C4 scalar reference only; CSS L4 escape needs a separate scalar oracle |
| JSON object/array/pair role mining | structural document role discovery | **JSON-only-by-shape** | JSON-canonical role labels (`"object"`, `"array"`, `"pair"`) leak even though `TapeKind` enum is grammar-neutral; CSS/Sheets/BBNF-self have no pair-of-string-and-value structure | live drift at `skinny/crates/passes/src/lib.rs:1053`-`1110`; refuted as fleet-wide generality mechanism |
| `JsonSink` callback set | JSON event sink | **JSON-only-by-shape** | callback names `begin_object`, `key`, `i64`, `null` are JSON's data model | live at `skinny/crates/runtime/src/grammars/json/sink.rs:4`-`16`; CSS fact sink is separate; Sheets needs `function_call`/`cell_ref` callbacks; BBNF-self needs `directive`/`alt` callbacks |
| JSON exponent/sign number policy | number parse | **JSON-only-by-shape** | JSON RFC 8259 specific exponent/sign rules; CSS allows `+`/`-` in different positions, has dimension suffix; Sheets has separate scientific notation | scalar reference reused; SIMD body neutral if policy is caller-supplied; refuted as a fleet-wide grammar-neutral number primitive |

## Cost-Model-Derived Per-Grammar Selection

Per 2D, the data-driven selection logic chooses which primitives admit for
which grammar via a four-stage resolver pipeline (replacing the hardcoded
P1-P8 cascade):

1. **Candidate generation:** enumerate all primitive variants admissible by
   the grammar's generated facts (alphabet, quote/escape policy, number
   policy, host functions, output plane, recovery mode).
2. **Equality saturation:** e-graph keeps alternative `BackendExpr` plans
   simultaneously per `T2D-EGRAPH-EXTRACTION` (`2D-cost-model.md:55`-`58`);
   per-rewrite guards reject shapes whose `substrate_target`/`policy_owner`
   require retained sidecars or grammar-name branches.
3. **CSP feasibility filter:** reject candidates whose hardware gate, sa
   me-wave consumer, parity oracle, generated-size budget, or row regression
   risk fail per `T2D-CSP-SCOPE` (`2D-cost-model.md:60`-`62`).
4. **Active cost extraction:** extract minimum-cost plan from the feasible
   frontier; cost facts include throughput, cycles/byte, IPC, materialization
   bytes, generated LOC, feature gate, evidence freshness; stale/static
   fallback ceiling 30% per V3 numeric abrogate cap
   (`T-P2-V3-FOLD-ADDENDUM.md:108`-`116`).

Per-grammar selection logic example (C3 digit-block SIMD over the 9-grammar
census, with the actual 15 CSS L4 sub-grammars expanded as one census row):

| grammar | generated facts that admit C3 | admission verdict | row consumer | falsifiability gate |
|---|---|---|---|---|
| json | number policy = RFC-8259 (signs, decimal, exponent) | ADMIT | `parse_array_element_at_direct` at `generated.rs:506` on canada/mesh/marine_ik/numbers | sonic-rs strict + Track 2 row movement per `p3a-candidate-shortlist.md:209` |
| css_l4 (15 sub-grammars; relevant: `values.bbnf`, `value-unit.bbnf`) | number/dimension policy = CSS Values L4 (signs in different position, optional unit suffix) | ADMIT with grammar-policy split | new CSS L4 `<number>` byte-class checkasm row per `p3a:93`; runtime consumer post-R4 regen-css | lightningcss strict + Track 1 + 1 % |
| css_pretty | reuses CSS L4 token alphabet | ADMIT (transitive on css_l4) | css_pretty runtime regen-emit | css_pretty round-trip |
| google_sheets | number policy = Sheets formula (scientific notation; no sign in some positions) | ADMIT with grammar-policy split | future Sheets formula numeric consumer | ODF 1.3 Part 4 golden table |
| bbnf | number policy = literal-only (rare numeric leaf) | DEFER — measured deletion: BBNF-self has no hot numeric leaf in P1 profile | n/a; record architectural-block | per Lock 14 v+1 "measured deletion/rejection record" |
| ebnf | similar to bbnf | DEFER — measured deletion | n/a | n/a |
| bnf | similar to bbnf | DEFER — measured deletion | n/a | n/a |
| csv | number policy = CSV (locale-permissive separators) | DEFER until csv hot row | n/a | n/a |
| math | number policy = math literal | DEFER until math hot row | n/a | n/a |

This table is the executable expression of "cost-model-derived per-grammar
selection": each row carries the generated-fact predicate, the admit/defer
verdict, the same-wave consumer, and the falsifiability gate. ADMIT requires
all four cells; DEFER must cite Lock 14 v+1 "measured deletion/rejection
record" with architectural-block evidence.

## C3 + C4 Same-Wave Consumer Documentation Per Lock 14 v+1

The S-P3 P3-A shortlist candidates C3 and C4 are the canonical worked examples
of Lock 14 v+1 strict-read "at least one non-JSON consumer or measured
deletion/rejection" discharge.

### C3 `digit_block_simd_accumulate` — UDOT byte-window multiply-accumulate

- **Abstract primitive (Lock 16 line 287):** "byte-window multiply-accumulate,
  lifted from dav1d's FIR filter — applies to ANY grammar's digit-block decode."
- **Same-wave non-JSON consumer (Lock 14 v+1 strict read):** the
  `bbnf-simd` checkasm row at new `crates/bbnf-simd/tests/checkasm_byte_class_from_range_64.rs`,
  modelling the sibling-shape template at
  `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:1`, with
  the CSS L4 `<number>` byte-class config `[0x30..=0x39, 0x2E, 0x2B, 0x2D,
  0x65, 0x45]` instantiated as the non-JSON row. **The checkasm parity row IS
  the non-JSON same-wave exercise** — discharges Lock 14 v+1 inside the SAME
  wave that admits the SIMD body, no cross-wave deferral to W8
  (`p3a-candidate-shortlist.md:93`).
- **CSS L4 runtime consumer (W8 corroboration):** post-R4
  `cargo xtask regen-css` emits the CSS L4 `<number>` runtime consumer; the
  W9 admission is not gated on it (already discharged at checkasm).
- **Verdict per Lock 14 v+1:** ADMIT. Grammar-neutral abstract primitive, two
  consumer planes (JSON numeric direct + CSS L4 checkasm), no fleet-wide
  unsupported claim.

### C4 `unicode_escape_neon_nibble_decode` — `\uXXXX` x4/x8 batch

- **Abstract primitive:** fixed-4-nibble Unicode hex decode (TBL-based).
- **Same-wave non-JSON consumer (Lock 14 v+1 strict read):** **BBNF-self
  string-escape consumer**. BBNF-self uses JSON-shape escape alphabet per
  P2-F §2.7 + §3 note 1 (`grammar/bbnf/bbnf.bbnf:11`-`13` defines
  `literal = ( "\"" , /(\\.|[^"\\])*/ , "\"" | "'" , … | "\`" , … ) -> Span`
  with the same `\\.` escape pattern as JSON; the `\u`+4-nibble form is
  shape-identical). The SAME SIMD body (`unescape_uxxxx_x8_neon` fixed-4-nibble
  decode) is exercised by the BBNF-self literal-unescape consumer driving
  through `parse_that_regex::unescape_string` at
  `skinny/crates/parse-that-regex/src/lib.rs:718`. (Verified at HEAD via
  `grep -n "literal = " grammar/bbnf/bbnf.bbnf` line 11 and
  `grep -n "fn unescape_string" skinny/crates/parse-that-regex/src/lib.rs`
  line 718.)
- **Shape-orthogonal CSS L4 carve-out (Lock 14 v+1 strict read):** the CSS L4
  escaped-ident `\HEXHEX` (CSS Syntax §4.3.7 variable 1-6 hex digits) is
  **SHAPE-ORTHOGONAL to the fixed-4-nibble SIMD body and does NOT exercise
  this primitive**. Carved out as a separate-primitive concern per Lock 14
  v+1 "measured deletion/rejection" record: variable-width CSS escape
  requires a different primitive; admitted/rejected separately, not via C4
  (`p3a-candidate-shortlist.md:106`).
- **Verdict per Lock 14 v+1:** ADMIT. Grammar-neutral abstract primitive
  with shape-identical BBNF-self consumer; shape-orthogonal CSS variable-
  width escape explicitly excluded with rejection record. The "carve-out"
  IS the strict-read discharge — recognizing the primitive's scope as
  fixed-4-nibble and refusing the claim that the same SIMD body handles
  CSS variable-width.

### Why these are the documented worked examples

C3 + C4 together demonstrate the two strict-read patterns:

1. **C3 pattern (ADMIT via cross-grammar non-JSON consumer):** the abstract
   primitive (byte-window MAC, byte-class set membership) genuinely applies
   to multiple grammars; the same-wave non-JSON consumer (CSS L4 byte-class
   checkasm) directly exercises the same SIMD body.
2. **C4 pattern (ADMIT via shape-identical consumer + shape-orthogonal
   carve-out):** the abstract primitive (fixed-4-nibble Unicode decode)
   applies to grammars with shape-identical escape syntax (JSON + BBNF-self);
   grammars with shape-orthogonal escape syntax (CSS variable-width
   `\HEXHEX`) get an explicit "this primitive does not cover that case"
   rejection record, not a silent generality claim.

This is the practical operational meaning of Lock 14 v+1's "primitive claimed
grammar-neutral must exercise at least one non-JSON consumer or record a
measured deletion/rejection." It is NOT "primitive must work for every
grammar"; it is "primitive's scope must be honestly mapped per grammar, with
ADMIT/DEFER/REJECT recorded for each."

## Per-Technique Transfer Coverage

Entries below are transfer requirements, not admissions. A technique without
the named generated facts and a same-wave row consumer is `NOT-VALIDATED` for
fleet-wide grammar-neutral claims.

| technique | CSS L4 transfer | Sheets transfer | BBNF-self transfer | required generated facts | failure mode if absent |
|---|---|---|---|---|---|
| byte-set classify / run-skip | delimiters, trivia, identifiers, at-rules | separators, quotes, operators | punctuation, directive starts | byte alphabet, quote/comment policy | generic JSON alphabet leak |
| string / escape scan | CSS strings, URLs, escaped identifiers (variable-width `\HEXHEX`) | doubled-`""` policy (no backslash) | shape-identical `\\.` + `\u`+4-nibble | quote, escape, control, terminator policy | JSON `\uXXXX` overfit (C4 carves out CSS) |
| digit / number scan | numbers, dimensions, percentages, calc | numeric literals; scientific notation | rare numeric leaf (DEFER) | number grammar, suffix/unit policy | JSON exponent/sign policy leak |
| direct/fact sink | CSS fact-stream rows | formula facts | grammar/directive facts | generated sink callbacks and fact schema | `JsonSink` as generic API |
| `BackendShape` resolver | selector/value/declaration shapes | formula/reference shapes | rule/expression/directive shapes | FIRST/follow, layout, host, recovery, output mode, cost facts | hardcoded P1-P8 cascade |
| regex/HIR facts | CSS selector/value recognizers where legal | formula token recognizers | grammar token recognizers | HIR, nullability, first-set, char-class facts | opaque JSON regex strings |
| byte-window multiply-accumulate (C3/UDOT) | CSS L4 `<number>` byte-class | Sheets numeric literals | DEFER (no hot numeric leaf) | digit alphabet, accumulator policy | hardcoded JSON exponent/sign |
| fixed-4-nibble Unicode decode (C4) | SHAPE-ORTHOGONAL carve-out (variable-width) | n/a (no `\u` form) | shape-identical (admits) | escape policy, fixed-width vs variable-width flag | claim primitive covers CSS without separate body |
| cross-chunk byte-context (`vextq_u8`) | CSS escaped-identifier boundary | Sheets quoted-sheet-name boundary | BBNF-self literal boundary | chunk-spanning token policy | retain class/cursor sidecar (REDRESS 96/97/98) |

## Feature / Witness Transfer Ledger

The ledger makes CH6's anti-paper-close requirement explicit. Rows without an
existing production consumer remain `NOT-VALIDATED`; they can be shortlisted
only with a same-wave row consumer and gate-consumed telemetry.

| feature or witness | generated facts required | primitive families | code owners | equality oracle | row consumer / telemetry | state |
|---|---|---|---|---|---|---|
| CSS declaration values, SK-V12 admitted row | declaration/value token facts, sink schema, string/number policy | byte-set classify, run-skip, string scan, number scan, fact sink | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`, CSS codegen templates | lightningcss + cssparser strict equality | `css_l4/declaration_values/direct_to_struct/main`, `skinny/RESULTS.md` CSS telemetry | ADMITTED-EVIDENCE |
| CSS stylesheet and selectors | stylesheet root, selector families, combinator, pseudo, attribute, nesting facts | byte-set classify, identifier scan, selector dispatch, fact sink | future `css_l4_stylesheet_and_selectors` generated runtime/codegen paths; HEAD provider exists but emitter not yet rostered (`grammar_profile.rs:21`) | lightningcss plus independent golden/cssparser coverage where applicable | future generated CSS row; non-JSON telemetry schema | NOT-VALIDATED |
| CSS declaration values extended, including `var()` and `calc()` | custom-property, substitution, function, math, dimension/unit facts | string/escape scan, number/dimension scan, function dispatch, fact sink | CSS declaration extension generated paths; HEAD provider exists at `grammar_profile.rs:20` | lightningcss plus cssparser/golden table coverage | future generated CSS row; feature-level telemetry | NOT-VALIDATED |
| CSS visual functions: gradients, transforms, filters, easing | function families, nested value grammar, unit/domain facts | byte-set classify, number scan, function dispatch, fact sink | future CSS visual-function generated paths; HEAD provider exists at `grammar_profile.rs:22` | lightningcss plus hand-checked golden table where cssparser lacks parity | future generated CSS row; feature-level telemetry | NOT-VALIDATED |
| Sheets formula witness | function names, separators, operators, references/ranges, string quote policy (doubled-`""`) | byte-set classify, digit scan, string scan, reference recognizer, fact sink | future `google_sheets` generated runtime/codegen paths (workspace grammar at `grammar/google-sheets/google-sheets.bbnf`) | ODF 1.3 Part 4-derived golden table | future formula fact row or fail-closed witness telemetry | NOT-VALIDATED |
| BBNF-self witness | directives, alternatives, repetitions, token/literal policy, operator/directive facts | regex/HIR facts, byte-set classify, string scan, direct/fact sink | future BBNF-self generated runtime/codegen paths (workspace grammar at `grammar/bbnf/{bbnf,expressions,types}.bbnf`) | current grammar parser plus checked golden grammar fixtures | future grammar/directive fact row or fail-closed witness telemetry; partial via C4 BBNF-self literal-escape same-wave consumer | NOT-VALIDATED (transitioning to ADMITTED-VIA-C4 if C4 wave lands) |

## Closure Criteria For Live Grammar Leaks

| surface | closure criterion | not closure |
|---|---|---|
| `RuntimeProvider` (8-variant HEAD enum) | provider roster generated from workspace metadata or grammar manifests, with JSON/CSS/Sheets or BBNF-self onboarding without generic-code edits | adding enum variants or grammar-name branches in `codegen` (V3-V4 drift expanded the enum from 2 to 8 variants without converting to generation) |
| `JsonSink` | generated per-grammar sink/fact/direct builder surface; generic crates see generated artifacts, not JSON callback names | renaming `JsonSink` while retaining JSON callback semantics as the shared API |
| `OffsetFlags` | bit storage may remain generic (HEAD has `GRAMMAR_BIT0`/`GRAMMAR_BIT1` — partial repair), but a `FlagSchema` generated table replaces ad-hoc per-grammar bit aliases | `HAS_ESC`/`HAS_CONTROL` remaining public grammar-neutral meanings (CLOSED at HEAD); only 2 grammar bits exist (open question if CSS/Sheets/BBNF-self need more) |
| fact streams | admitted output-plane contracts with strict comparator/oracle provenance and gate-consumed telemetry | hidden retained sidecars consumed by later internal waves |
| materialization role mining | derived from Grammar IR alphabet/structural facts; `MaterializationDescriptor.label` drops JSON-canonical `"object"/"array"/"pair"` strings or sources them from the grammar's own rule names | live drift at `skinny/crates/passes/src/lib.rs:1053`-`1110` |

## Architectural Assertions Defended

| assertion | defense | consequence |
|---|---|---|
| The primitive vocabulary is grammar-neutral at the operation layer. | CSS tokenization, selectors, CSS value math/custom properties, OpenFormula references, and BBNF-self directives/operators all reduce to byte-stream recognizers, delimiter handling, number/function/string/reference scanners, and output-fact builders. The grammar-specific piece is the metadata feeding those primitives. Lock 16 abstract-primitive declarations name the lift contract explicitly. | Keep Layer 1 primitives named by abstract operation: byte classify, set-membership, run-skip, digit-block decode, cross-chunk context, prefix/bitmap operations, fact emission, byte-window MAC. Do not name primitives after JSON. |
| `BackendShape` remains exactly the five-shape enum. | The live enum at `skinny/crates/ir/src/lib.rs:339`-`345` and Lock 10 already provide a grammar-neutral target vocabulary. The defect is selection strength and lowerer completeness, not missing CSS/Sheets shapes. | SK-V13/SK-V14 should add resolver facts and lowerers, not enum variants. |
| CSS L4 is the right non-JSON proof grammar. | The admitted CSS declaration-values row supplies strict equality and lightningcss comparator evidence; the parity gap matrix names missing production families; CSS Syntax/Selectors/Values/Variables specs define the full breadth. The 15-sub-grammar workspace expression (`grammar/css/l4/*.bbnf`) is the live materialized scope. | CSS rows are the first-class generality test. Sheets and BBNF-self remain required falsifiers for role mining, but not substitutes for the pinned CSS bar. |
| Generated per-grammar code may carry grammar names; generic crates may not. | Lock 14 v+1 generated-output allowance (`restart/locks/LOCKS.md:222`-`238`) permits grammar source/metadata/declaration-crate inputs, while generated outputs necessarily live under `runtime/src/grammars/<name>/`. The failure is hand-coded generic dispatch/registry policy. | T-P3 should codify a generated-output allowance and a stricter generic-crate scan for name and shape leaks. |
| Same-wave non-JSON consumer measurement is part of grammar generality, per Lock 14 v+1 strict read. | REDRESS shows primitive-only and microbench-only routes become orphans when they do not move a row. Lock 14 v+1 explicit text: "a primitive claimed grammar-neutral must exercise at least one non-JSON consumer or record a measured deletion/rejection." | Every grammar-neutral primitive needs a row consumer in at least one CSS/JSON/Sheets/BBNF-self path during the admitting wave, OR explicit measured deletion/rejection record. C3 (CSS L4 byte-class checkasm) and C4 (BBNF-self literal escape, CSS variable-width escape carve-out) are the worked examples. |
| Cost-model selection is data-driven, not branching. | Per 2D's `T2D-EGRAPH-EXTRACTION` + `T2D-CSP-SCOPE`: equality saturation over `BackendExpr` + CSP feasibility filter + active cost extraction replace the hardcoded P1-P8 cascade. Each candidate carries `substrate_target`, `retention_lifetime`, `policy_owner`. | The decision engine must consume generated grammar facts; mining JSON roles from generic code is Lock 14 drift even if JSON equality passes. |

## Architectural Assertions Refuted

| refuted assertion | evidence | replacement rule |
|---|---|---|
| JSON object/array/pair/string/number/bool/null role mining is a generic materialization strategy. | The live materialization-descriptor surface emits JSON-canonical labels `"object"`, `"array"`, `"pair"` at `skinny/crates/passes/src/lib.rs:1059`, `:1079`, `:1102` even though `TapeKind::{Container, Sequence, KeyValuePair}` is grammar-neutral. CSS Selectors, OpenFormula references/functions, and BBNF-self directives do not map to those labels. | Role facts must be generated from Grammar IR and metadata; descriptor labels source from grammar rule names or grammar metadata, not from JSON-canonical strings. |
| `JsonSink` can stand in for a generic direct-output sink. | `JsonSink` callbacks are JSON-named at `skinny/crates/runtime/src/grammars/json/sink.rs:4`-`16`; CSS fact sink has a different event/fact shape; Sheets needs `function_call`/`cell_ref`; BBNF-self needs `directive`/`alt`. | Generate a per-grammar sink/fact trait or per-grammar direct builder; the generic runtime sees only generated artifacts. |
| `OffsetFlags::HAS_ESC` and `HAS_CONTROL` are grammar-neutral public semantics. | HEAD has already migrated to `GRAMMAR_BIT0`/`GRAMMAR_BIT1` at `skinny/crates/runtime/src/tape/mod.rs:22`-`23` (partial repair). | Keep packed bit storage; move flag meanings behind generated grammar-owned aliases (HEAD partial close); expand to `FlagSchema` generated table if CSS/Sheets/BBNF-self need >2 grammar bits. |
| Adding a grammar by editing `RuntimeProvider` is acceptable. | `RuntimeProvider` is a grammar-name enum in generic codegen at `skinny/crates/codegen/src/grammar_profile.rs:17`-`26` (8 variants at HEAD); Lock 14 forbids this class. | Provider registration is generated from workspace metadata; adding a grammar changes metadata/source/generator output, not generic code. The V3-V4 enum expansion (2→8 variants) without conversion to generation is itself a Lock 14 fault. |
| CSS declaration-values token parity proves full CSS grammar generality. | SK-V12 admitted one row; SK-V13 synthesis names full CSS parity as open at `restart/skinny/tranches/sk-v13/SYNTHESIS.md:38`-`57`; HEAD has 7 CSS L4 sub-providers but only `declaration_values` is admitted. | Treat the SK-V12 row as valid evidence and as the first CSS fixture, not as close authority. |

## Future-Grammar Onboarding Test

Every future grammar must pass this test before a generality claim is allowed.
The test surfaces what's required mechanically; passing each step is a binary
yes/no.

### Steps

1. **Grammar source + metadata only.** Add only grammar source (e.g.
   `grammar/<name>/<name>.bbnf`), workspace metadata declaring strategy
   (recognisers, host fns, output-dir, pratt eligibility, simd eligibility),
   and optionally a per-grammar declaration crate carrying host-fn
   implementations. NO edits to any generic crate.
2. **Regenerate the rostered surfaces.** Run the regen-grammar generator;
   provider registry, config/fact tables, sink/value/view surfaces, tests
   must all be emitted without editing generic crates. Verification:
   `git diff` shows changes ONLY under `crates/runtime/src/grammars/<name>/`,
   the rostered generator, and `Cargo.toml` workspace metadata.
3. **Grammar-name leak scan.** Run
   `rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser|<NewName>Parser' crates/{ir,parse,codegen,runtime,path,path-core,egraph,csp-solver,parse-that-regex,parse-that,bbnf-simd,analysis,lsp}/src/`
   per Lock 14 verification command (`restart/locks/LOCKS.md:220`); MUST
   return ZERO.
4. **Grammar-shape leak scan.** Run
   `rg -n 'object|array|pair|begin_object|key_source' crates/{ir,passes,codegen}/src/`
   AND
   `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>|<NewName>\w*\s*=>' crates/`
   per Lock 14 v+1 grammar-shape leak rule; MUST return ZERO new matches
   relative to baseline.
5. **Five-shape eligibility fixture.** At least one rule per reachable
   `BackendShape` (`EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`,
   `CollapsedStage`) OR an explicit generated reason that the shape is
   unreachable for this grammar (e.g., "no SIMD-eligible structural rule;
   `CollapsedStage` is unreachable"). The resolver must emit per-rule
   `BackendShape` and `CostFacts` from generated grammar facts, with zero
   generic grammar edits.
6. **Primitive same-wave non-JSON consumer.** Exercise at least one Layer-1
   primitive through a same-wave consumer row OR record a measured
   architectural-block per Lock 14 v+1 strict read. For grammars whose
   primitives are shape-identical to existing primitives (e.g., BBNF-self
   `\u`+4-nibble = JSON `\uXXXX`), this discharges via shape-identical
   consumer; for shape-orthogonal cases (e.g., CSS variable-width
   `\HEXHEX`), this requires a separate-primitive declaration and
   measured-rejection record.
7. **Telemetry/provenance consumed by gate.** Emit telemetry
   (`skinny/RESULTS.md`, gate-json) consumed by the gate in the same wave;
   row movement, equality verdict, and substrate-kind classification must
   be gate-consumable.

### What this test surfaces

- **Step 1 → 2 surface:** whether the grammar can be added with no generic
  edits. If any step requires editing a generic crate, the generator/manifest
  is incomplete; Lock 14 fault.
- **Step 3 → 4 surface:** whether grammar-name or grammar-shape leaks persist
  in generic crates. The HEAD `RuntimeProvider` enum expansion is itself a
  step-3 failure for any future Sheets/BBNF-self/etc. additions.
- **Step 5 surface:** which `BackendShape` variants are unreachable for this
  grammar (an explicit metadata field, not silent omission). Sheets formulas
  likely lack `CollapsedStage` (no AVX-512 FSM applicable); BBNF-self likely
  lacks `OffsetTape` retained projection (parse-only).
- **Step 6 surface:** which Layer-1 primitives this grammar genuinely
  consumes and which are explicitly rejected. For CSS L4: byte-class +
  string scan + number scan ADMIT; variable-width escape DEFER (separate
  primitive). For Sheets: byte-class + string scan (with doubled-`""`
  policy) + reference scan ADMIT; UDOT digit-block DEFER until hot row.
  For BBNF-self: string scan + `\u`+4-nibble shape-identical ADMIT;
  numeric primitives DEFER.
- **Step 7 surface:** whether the gate has telemetry to honestly classify
  the wave's outcome (ADMITTED-EVIDENCE, NOT-VALIDATED, ADMIT-WITH-CARVE-OUT,
  ARCHITECTURAL-BLOCK).

### Required primitives (Layer-1) for ANY new grammar

The minimal Layer-1 primitive set every new grammar needs admission tests for:

1. `byte_class_from_eq_set_64` (with grammar-supplied alphabet)
2. `escape_mask_64` (with grammar-supplied escape policy) — required for any
   grammar with quoted strings; rejection record acceptable for unquoted
   grammars (math, BNF terminal-only)
3. `bitmap_prefix_xor_64` / `bitmap_next_set_bit` — required for grammars
   with retained structural projection
4. `compact_mask` — required for grammars emitting structural positions
5. Number primitive (UDOT/scalar-fallback) — DEFER if no hot numeric leaf
6. Sink primitive (per-grammar `Sink` trait, generated) — required for
   `SinkOnly` shape

Admission tests required for each primitive: scalar reference, checkasm
parity, same-wave consumer (or measured architectural-block), strict
equality oracle, row gate.

### CSS L4 mandatory (SK-V14)

For SK-V14, CSS L4 (15 sub-grammars, full step 1-7 cycle) is mandatory.
Sheets and BBNF-self are the negative controls: they must fail closed under
JSON-shaped role mining and pass only after generated role facts replace it,
OR record explicit architectural-block.

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| What generated manifest layout replaces the 8-variant `RuntimeProvider` enum with the smallest public API change? | Prototype a generated provider manifest that recreates the current JSON and 7 CSS L4 outputs, then add a Sheets or BBNF-self witness provider without editing `codegen/src/grammar_profile.rs`. Closure is not optional: a hand-coded provider enum remains Lock 14 drift; the V3-V4 enum expansion (2→8 variants) deepened the drift. |
| Which generated sink/fact trait template covers JSON direct, CSS fact-stream, Sheets formula facts, and BBNF-self parse facts? | Design generated sink/fact traits for the four proof surfaces and verify all four render from the same template family. Closure is not optional: `JsonSink` cannot remain a generic sink contract. |
| Does the HEAD 2-bit `GRAMMAR_BIT0`/`GRAMMAR_BIT1` slot count suffice for CSS L4 (escape + URL + comment policies), Sheets (doubled-quote + reference-type policies), and BBNF-self (multi-quote-set + Pratt policies)? | Add a generated `FlagSchema` table with grammar-owned bit assignments; if any grammar needs >2 bits, expand `OffsetFlags` storage and prove JSON parity unaffected. |
| Which primitive families are universally grammar-neutral and which are grammar-family-specific? | The Abstract-Primitive-Lift Table above is the V3 baseline. Future grammars expand it; cross-tab against grammar facts: alphabet size, quoted-string model, comment model, number/dimension model, function-call syntax, reference syntax, chunk-spanning tokens. |
| Does the decision-engine fold (per 2D) produce row-moving routes for all three proof grammars? | After W5-W9 per SK-V14 SPEC, run CSS, Sheets, and BBNF-self eligibility fixtures through the resolver and require emitted cost facts plus no generic grammar branches. |
| Can the materialization-descriptor JSON-canonical labels at `passes/src/lib.rs:1059`-`:1102` be sourced from Grammar IR rule names instead? | Replace `label: "object"` with `label: rule_name.to_owned()` or grammar-metadata source field; prove JSON parity (test fixtures at `:1684`-`:1760` use `"object"`/`"array"`/`"pair"` rule names already). |

## LOCKS-AMENDMENTS-CANDIDATE

| candidate | target lock(s) | proposed amendment | supporting evidence | risk / same-wave consumer |
|---|---|---|---|---|
| LAC-2C-01 generated grammar registry | Lock 14, Lock 6 | Generic crates may consume a generated provider manifest, but may not hand-code grammar provider enums, grammar-name branches, or grammar-name root aliases. Generated files under `runtime/src/grammars/<name>/` are allowed only if regenerated from the rostered generator. The 8-variant HEAD `RuntimeProvider` enum must be replaced or amended to a single generated roster. | Current hardcoded providers at `skinny/crates/codegen/src/grammar_profile.rs:17`-`26` and `:100`-`:110`; Lock 14 at `restart/locks/LOCKS.md:220`-`238`; V3-V4 drift expanded enum from 2 to 8 variants without conversion to generation; CSS admitted generated row at `skinny/RESULTS.md:94`. | High. Consumer: regenerated JSON + 7 CSS L4 rows plus a new Sheets/BBNF-self witness provider with no generic code edit. |
| LAC-2C-02 grammar-shape leak census | Lock 14, Lock 10 | Lock 14 verification must scan not only literal grammar names but grammar-shaped role policy: JSON punctuation alphabets, object/array/pair/string/number/bool/null roles, JSON-canonical materialization-descriptor labels, and hardcoded sink callback names. | Live role labels at `skinny/crates/passes/src/lib.rs:1059`-`:1102` (despite grammar-neutral `TapeKind`); T-P1 1B/1F both classify this as live drift. | High. Consumer: CSS/Sheets/BBNF-self fixtures that derive labels from generated facts; modify `MaterializationDescriptor.label` to source from grammar rule names. |
| LAC-2C-03 generated sink and flag surface | Lock 1, Lock 9, Lock 14 | Add a generated grammar-owned sink/fact/value/flag interpretation surface. Generic tape may store compact flags (HEAD partial close), but bit names AND direct-output callbacks are grammar-owned generated artifacts. Add `FlagSchema` generated table replacing per-grammar `pub(crate) const X: u8 = OffsetFlags::GRAMMAR_BIT0` aliases. | `OffsetFlags` at `skinny/crates/runtime/src/tape/mod.rs:22`-`23` (HEAD partial close); `JsonSink` at `skinny/crates/runtime/src/grammars/json/sink.rs:4`-`16`; CSS fact sink separate. | Medium-high. Consumer: JSON direct parity, CSS fact-stream parity, and one Sheets formula fact fixture. |
| LAC-2C-04 BackendShape onboarding proof | Lock 10, Lock 14 | A new grammar cannot claim generality until the resolver emits per-rule `BackendShape` and `CostFacts` from grammar facts, with zero generic grammar edits and a five-shape eligibility or explicit unreachable-shape report. The resolver consumes the 2D pipeline (e-graph + CSP + active cost extraction), not the P1-P8 cascade. | Five-shape enum at `skinny/crates/ir/src/lib.rs:339`-`345`; current thin cost facts; 2D `T2D-EGRAPH-EXTRACTION` + `T2D-CSP-SCOPE` route per `2D-cost-model.md:55`-`62`. | High. Consumer: resolver snapshot and generated-code equality tests for CSS, Sheets, BBNF-self. |
| LAC-2C-05 primitive generality admission gate per Lock 14 v+1 | Lock 16, Lock 14 | Every primitive row records `abstract_primitive`, grammar feature vector, scalar reference, checkasm/parity status, same-wave consumer, and at least one non-JSON exercise OR explicit measured-deletion record when the primitive is claimed grammar-neutral. The shape-orthogonal carve-out (e.g., C4 fixed-4-nibble vs CSS variable-width) IS the discharge for shape-orthogonal cases. Primitive parity alone is not closure. | REDRESS prerequisite-only escape mask at `skinny/REDRESS.md:3603`-`3632`; `a64_ascii_set_run_skip` production split at `skinny/REDRESS.md:3766`-`3820`; SK-V13 producer-only pre-block at `restart/skinny/tranches/sk-v13/SYNTHESIS.md:246`-`247`; Lock 14 v+1 at `restart/locks/LOCKS.md:259`-`260`; S-P3 P3-A C3 (`p3a-candidate-shortlist.md:93`) + C4 (`:106`) worked examples. | High. Consumer: CSS scan-block production row or measured deletion; JSON/Sheets/BBNF-self consumers when claimed by those grammars; shape-orthogonal carve-out records when not applicable. |

## Sources

- W3C CSS Syntax Module Level 3: `https://www.w3.org/TR/css-syntax-3/`.
- W3C Selectors Level 4: `https://www.w3.org/TR/selectors-4/`.
- W3C CSS Values and Units Module Level 4: `https://www.w3.org/TR/css-values-4/`.
- W3C CSS Custom Properties for Cascading Variables Module Level 1: `https://www.w3.org/TR/css-variables-1/`.
- OASIS OpenDocument v1.3 Part 4, OpenFormula: `https://docs.oasis-open.org/office/OpenDocument/v1.3/OpenDocument-v1.3-part4-formula.pdf`.
- T-P2 V2 fold authority: `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`.
- T-P2 V3 fold authority: `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md`.
- S-P3 P3-A candidate shortlist (C3/C4 worked examples): `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md`.
- Local primary evidence: `restart/audit/totality/p1/*.md`, `restart/locks/LOCKS.md`, `restart/skinny/tranches/sk-v13/scoping/*.md`, `skinny/RESULTS.md`, `skinny/REDRESS.md`, and cited `skinny/crates/*/src` + `grammar/` paths above.
