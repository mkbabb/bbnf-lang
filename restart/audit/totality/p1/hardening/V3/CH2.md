# T-P1 CHALLENGE V3 — CH2 GENERALITY (SK-V18 cycle, lens V3)

Verdict: REVISE-mixed. 7 ACCEPT, 3 REVISE, 0 REJECT.

LENS: CH2 GENERALITY. Lock 14 holds only when (i) no grammar-neutral substrate
fact is catalogued JSON/CSS-only; (ii) 1C flags every grammar-named module in a
generic crate; (iii) 1D separates JSON/CSS-empirical from grammar-neutral; (iv)
no grammar-name leak goes uncited. Reviewed against `restart/ARCHITECTURE.md`,
`restart/MASTER-PLAN.md`, `restart/locks/LOCKS.md:349` (Lock 14 body + v+1
clauses) and the live code at the dirty HEAD.

NOTE ON SCOPE: the prior `V3/CH2.md` in git belonged to the SK-V14/SK-V15
campaign (`P1-1B-D9`/`P1-1B-D10`, `G-10`-as-recognizer-mining) and returned
all-ACCEPT. That packet is superseded — the six live inventories carry
`cycle: V5-SKV18-totality` frontmatter and an entirely different finding set.
This CH2 reviews the LIVE SK-V18 inventories from first principles; it does not
inherit the prior all-ACCEPT.

## Evidence (commands run, all at dirty HEAD)

```sh
# strategy.rs — the central generic-crate grammar-name leak
rg -n 'PRODUCTION_MANIFEST_TABLE|idents:' crates/ir/src/registry/strategy.rs
rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/ir/src/        # = 11 sites
sed -n '134,190p' crates/ir/src/registry/strategy.rs | rg -n 'idents:'                # 9 idents rows
# Lock 14 leak command across ALL generic crates
rg -ln 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' \
  crates/{ir,codegen,parse,path,path-core,egraph,csp-solver,parse-that-regex,parse-that,bbnf-simd,analysis,lsp}/src
# css_types.rs (lock-named mess)
wc -l crates/core/src/css_types.rs ; head -2 crates/core/src/css_types.rs
# Pattern H census
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l                # 71
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' -not -path '*tape*' | wc -l  # 67
# find_component_delim neutrality
rg -ln 'find_component_delim' skinny/crates/runtime/src | grep -v css                 # empty (CSS-only)
# phantom <G> vs K axis
sed -n '175,181p' skinny/crates/runtime/src/tape/mod.rs
```

Material live truths:

- `crates/ir/src/registry/strategy.rs` carries **9** grammar-named `idents` rows
  at exactly `:137,:143,:149,:155,:161,:167,:173,:179,:185`
  (Json/GoogleSheets/CssL4/Bbnf/Csv/Math/Bnf/Ebnf/CssPretty). `crates/ir` is in
  Lock 14:349's generic-crate list. 9-grammar-wide leak — CONFIRMED.
- The narrow 4-name regex (`JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser`)
  catches **4** of those idents rows (137/143/149/155); the other **5** rows
  (161/167/173/179/185) escape it. Total narrow-regex hits in `crates/ir/src/`
  = 11 (4 idents rows + 5 doc-comment lines in strategy.rs + 2 doc-comments in
  `grammar_facts.rs:799`, `shape_dispatch/scalar.rs:17`).
- `crates/core/src/css_types.rs` = 66 LOC, header "Host shims for the CSS L4
  grammar's `-> parse_hex_color(...)` map"; named VERBATIM in Lock 14:349 as the
  mess. `crates/core` pkg = `bbnf` (the host/core crate, NOT a `crates/<grammar>/`
  declaration crate) — Lock 14 (c) admissibility does not apply.
- Pattern H = 71 full / 67 per-grammar; +4 = generic `tape/{mod,cursor,arena,record}.rs`.
- `find_component_delim` lives ONLY in the 7 css_l4 replicas — zero non-CSS caller.
- `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>` with `_kind:
  PhantomData<fn()->K>` (`:178`) and `_grammar: PhantomData<fn()->G>` (`:179`).
  EventGrammar instantiates with a real type only under `#[cfg(test)]`/witness.

## Findings

| id | inventory | disposition | finding | evidence |
|---|---|---|---|---|
| CH2-V3-001 | 1D | ACCEPT | 1D separates JSON/CSS-empirical (J-1..J-3, C-1..C-4) from grammar-neutral (G-1..G-13) into two explicit tables per Lock 14's separation obligation. No grammar-neutral fact is mis-bucketed as JSON-only; no empirical fact claims fleet-wide neutrality. | `1D-skinny-lessons.md:176-204` |
| CH2-V3-002 | 1D | ACCEPT | The `css_balanced_component_scan` gap imposes the exact Lock 14 neutrality obligation: a non-CSS invoker (JSON `{}/[]` or Sheets `paren_expr`) "else demote to `css_`-scoped name," with U-3 as the UNKNOWN+verify_action. Matches LOCKS:396-397 ("must exercise ≥1 non-JSON consumer or record a measured deletion/rejection") verbatim in intent. | `1D-skinny-lessons.md:127-134,229-233`; `LOCKS.md:396-397` |
| CH2-V3-003 | 1F | ACCEPT (core finding) | COH18-005/012 CORE claim is load-bearing and correct: `strategy.rs` is a 9-grammar-WIDE relocated-seam leak in generic `crates/ir`, and the narrow 4-name Lock 14 regex understates the breadth — spot-verified, all 9 line numbers `:137..:185` confirmed live; total 11 narrow-regex sites confirmed. This is the CH2 generality finding done RIGHT (breadth surfaced, not JSON/CSS-narrowed). | `1F-coherence-scan.md:75,82`; live `strategy.rs:137-185`, `rg crates/ir/src` = 11 |
| CH2-V3-004 | 1F/1C | ACCEPT | `css_types.rs` is treated as the Lock-14-NAMED mess (not a generic JSON/CSS lesson); Lock 14 (c)'s per-grammar-declaration-crate admissibility correctly ruled inapplicable because the file lives in `crates/core/src/`, not `crates/<grammar>/`. Disposition (relocate-to-`crates/css/` vs delete) routed to SK-V19. | `1F-coherence-scan.md:76,117`; live `crates/core/src/css_types.rs:1` (66 LOC); `LOCKS.md:349` |
| CH2-V3-005 | 1C | ACCEPT | 1C flags the per-grammar `runtime/<g>/` dirs in the host crate as the Pattern H Lock 14 surface (C1/C7/D6) and the +4 generic `tape/` census exactly; the genuinely-generated `grammar/generated/<g>.rs` recognizer plane is correctly distinguished as the one place grammar names are allowed (rostered generator output). | `1C-runtime-evidence.md:30,36,60-61`; live census 71/67 |
| CH2-V3-006 | 1C/1A/1D | ACCEPT | The phantom `<G:EventGrammar>` is correctly identified as the DECORATIVE generality-vehicle (zero non-test instantiation) while the `K`=Kind axis is the REAL one preserved on DELETE — the generality-vehicle reasoning (LOCKS:620 leans on `<G>`; SK-V18 deletes it) is grammar-neutral and sound. | `1C-runtime-evidence.md:34,57-58`; `1A-substrate-evidence.md:95,148-169`; live `tape/mod.rs:175-180` |
| CH2-V3-007 | 1B | ACCEPT | `RuntimeEmitterKind{CompiledLowering,RequestFacts}` is catalogued as a grammar-FAMILY (JSON-vs-CSS) Lock 5/14 fork — i.e. a grammar-neutral architectural divergence, NOT a JSON-only or CSS-only empirical lesson. Correctly framed as "pure impl-side divergence the spec absorbs by deletion." | `1B-codegen-evidence.md:58,66-79`; live `grammar_provider.rs:40-42,110` |
| CH2-V3-008 | 1F | REVISE | COH18-005 ("the strict 4-name leak regex catches only **5 ident sites**") and COH18-012 ("**5 are `idents` rows**; the other **4 grammar names** — Csv/Math/Bnf/Ebnf/CssPretty — escape") TRANSPOSE the count. Ground truth: the narrow regex catches **4** idents rows (137/143/149/155); **5** idents rows escape (161/167/173/179/185 — and COH18-012 itself LISTS 5 names while writing "4"). Correction: replace both "catches only 5 ident sites" → "catches only **4** idents rows" and "5 are `idents` rows; the other **4** grammar names" → "**4** are `idents` rows; the other **5** grammar names." The CH2-V2-009 tag means this transposition was carried from the prior cycle uncorrected. | live `sed -n '134,190p' strategy.rs \| rg idents:` = JsonParser/GoogleSheets/CssL4/Bbnf caught (4), Csv/Math/Bnf/Ebnf/CssPretty escaped (5) |
| CH2-V3-009 | 1D | REVISE | G-10 ("`find_component_delim` is the 94.1% CSS hot leaf") sits in the **Grammar-Neutral Findings** table, but the 94.1%/79.5% figures are a CSS-empirical profile measurement (the leaf has zero non-CSS caller on disk). The grammar-NEUTRAL part is only the WIRE-vs-honest-retire DECISION RULE, not the number. Correction: re-label G-10 to scope the ratio as CSS-empirical (or cross-route it to the JSON/CSS-Empirical table) while keeping the decision-rule as the neutral lesson — else a CSS-only profile fact reads as fleet-neutral, the exact Lock 14 conflation the lens guards. | `1D-skinny-lessons.md:201`; live `rg find_component_delim ... \| grep -v css` = empty |
| CH2-V3-010 | 1F | REVISE | Leak-census COMPLETENESS gap: 1F's grammar-name leak census counts the `ir` doc-comment hits (grammar_facts.rs:799, scalar.rs:17) toward its 11-site total but does NOT extend the Lock 14 leak command to the `analysis` generic crate, which carries `BbnfBootstrap`-named doc-comment occurrences at `crates/analysis/src/state/ast_utils/mod.rs:4,11` (`BbnfBootstrapNodeView`/`BbnfBootstrapRuleKind`). These are caught by the same Lock 14 verification regex and live in a Lock-14-listed generic crate, yet are uncited across all six inventories. Correction: add an `analysis`-crate row to COH18-005/012's leak census (or explicitly scope the census to code-vs-doc-comment, applied uniformly to `ir` AND `analysis`). | live `rg -ln 'JsonParser\|...\|BbnfBootstrap\|...' crates/analysis/src` = `ast_utils/mod.rs`; uncited in `1*.md` |

## Spot-Verification of Load-Bearing Rows (path:line)

- `strategy.rs:137,143,149,155,161,167,173,179,185` — 9 grammar-named idents rows: CONFIRMED live.
- `crates/ir/src/` narrow-regex total = 11: CONFIRMED (4 idents rows + 7 doc-comment/sibling).
- `css_types.rs` = 66 LOC, `crates/core` pkg `bbnf`: CONFIRMED.
- Pattern H 71/67 split, +4 = `tape/{mod,cursor,arena,record}.rs`: CONFIRMED.
- `find_component_delim` CSS-only (7 css_l4 replicas, no non-CSS caller): CONFIRMED.
- `ValueRef<…K…G:EventGrammar>` with `_kind` (`:178`) real / `_grammar` (`:179`)
  phantom; EventGrammar real-instantiated only under cfg(test): CONFIRMED.
- `RuntimeEmitterKind` enum `grammar_provider.rs:40-42`, gate `:110`;
  `CSS_GENERATED_RS` const `runtime_generator.rs:701`: CONFIRMED.

## Required Fold

1. **1F-coherence-scan.md COH18-005 + COH18-012**: untranspose the idents-row
   arithmetic — the narrow regex catches **4** idents rows, **5** escape (drop
   the carried CH2-V2-009 "5 caught / 4 escape" wording).
2. **1D-skinny-lessons.md G-10**: re-scope the 94.1%/79.5% ratio as a CSS-empirical
   profile measurement; keep only the WIRE-vs-retire decision rule as the
   grammar-neutral lesson.
3. **1F-coherence-scan.md leak census**: extend the Lock 14 leak census to the
   `analysis` generic crate (`ast_utils/mod.rs:4,11` `BbnfBootstrap*` doc-comments),
   or scope the census code-vs-doc uniformly across `ir` AND `analysis`.

Preserve the 9-grammar-wide strategy.rs breadth finding (CH2-V3-003), the
`css_balanced_component_scan` neutrality obligation (CH2-V3-002), the phantom-`<G>`
generality-vehicle reasoning (CH2-V3-006), and the JSON/CSS-empirical-vs-neutral
two-table split (CH2-V3-001) in the next cycle.

TALLY accept=7 revise=3 reject=0
