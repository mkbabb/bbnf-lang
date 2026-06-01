# T-P1 CH2 GENERALITY — SK-V18 Totality Excavation (cycle V4)

Verdict: REVISE (3 of 7 findings REVISE; 4 ACCEPT; 0 REJECT).

Lens: CH2 GENERALITY. Lock 14 must hold across the six live SK-V18 (V5-self-labelled)
inventories — no divergence catalogued JSON/CSS-only when it is a grammar-neutral
substrate fact; 1C's runtime census flags every grammar-named module in a generic
crate; 1D separates JSON/CSS-empirical from grammar-neutral; no grammar-name leak
passes uncited (`restart/prompts/totality/PASS-1-EXCAVATION.md:110-114`,`:210`).

The prior `V4/CH2.md` on disk was the SK-V15 campaign's verdict; per the SK-V18
cycle protocol it is superseded in place. The current inventories carry frontmatter
`cycle: V5-SKV18-totality`; this CH2 reviews them as the live V4 challenge pass and
spot-verified the most load-bearing cited path:line rows against live code.

## What holds (live-verified)

The grammar-neutral / JSON-empirical separation is genuinely strong and survives
close reading against the tree:

- **1D's two-table split is exemplary.** "JSON / CSS-Empirical Findings" (J-1..C-4,
  `1D:181-191`) are held distinct from "Grammar-Neutral Findings" (G-1..G-13,
  `1D:193-209`). G-10 (`1D:206`) is the single most CH2-load-bearing row and it is
  correct: it re-scopes the 94.1%/79.5% figures as CSS-EMPIRICAL (per CH2-V3-009)
  and keeps ONLY the G6=WIRE decision-RULE as grammar-neutral. I verified the
  falsifier live: `rg find_component_delim skinny/crates/runtime/src | grep -v css`
  is EMPTY (the leaf has zero non-CSS caller; all 7 hits are `css_l4_*/generated.rs`).
- **1F is the only inventory citing the totality-tree leak, and it is exact.**
  `crates/ir/src/registry/strategy.rs` carries NINE grammar-named `idents` rows
  (`:137` Json, `:143` GoogleSheets, `:149` CssL4, `:155` Bbnf, `:161` Csv, `:167`
  Math, `:173` Bnf, `:179` Ebnf, `:185` CssPretty) — confirmed verbatim. The
  COH18-005/012 precision claim "the strict 4-name regex catches only 4 of 9 idents
  rows; the other 5 escape" is live-accurate: the narrow regex matches exactly
  `:137,:143,:149,:155` among idents rows; Csv/Math/Bnf/Ebnf/CssPretty escape. Total
  `crates/ir/src/` narrow-regex sites = 11 (9 strategy.rs + 1 grammar_facts.rs + 1
  scalar.rs). The consumer anchor `for_grammar_with_manifest(grammar_ident, registry,
  PRODUCTION_MANIFEST_TABLE)` is real at `strategy.rs:216`.
- **The analysis-crate completeness add (COH18-012, per CH2-V3-010) is real.**
  `crates/analysis/src/state/ast_utils/mod.rs:4` (`BbnfBootstrapNodeView`) and `:11`
  (`BbnfBootstrapRuleKind`) are `//!` doc-comments, exactly as classed. The 5
  escaping names do not leak beyond `ir/strategy.rs` into other generic crates.
- **The neutral discriminator is genuinely grammar-neutral.** `select_lowering`
  (`skinny/crates/codegen/src/lower/mod.rs:18-26`) matches on `BackendShape` with
  ZERO grammar names; the `RuntimeEmitterKind{CompiledLowering,RequestFacts}` fork
  (`grammar_provider.rs:40-42`) is the leak and is catalogued AS the leak, not
  laundered. The phantom `<G>` (`tape/mod.rs:175,179`, `_grammar: PhantomData<fn() ->
  G>`) is flagged decorative across 1A/1C/1D/1F.
- **Pattern-H census numbers reconcile exactly.** Live `find crates/core/src/runtime
  -mindepth 2 -type f -name '*.rs'` = 71; per-grammar (excl. `tape/`) = 67;
  `@generated` markers = 67. The inventories correctly distinguish 71-with-substrate
  (1F COH18-007) from 67 per-grammar Pattern-H (1C C1, 1D U-1) and trace the +4 to
  the generic `tape/` dir. The 6867-LOC carry figure (1C D6, 1D U-1) is exact.

No instance was found of a grammar-neutral substrate fact mis-catalogued as
JSON/CSS-only. The two `JSON-only`/`CSS-only` tokens that appear (`1D:206` G-10,
`1F:81` COH18-011) are both CORRECT usage — G-10 scoping a CSS measurement away from
a neutral rule, COH18-011 quoting the spec's own REDRESS-prohibition language.

## Where it fails (the REVISE band)

The defect is concentrated and structural, not scattered: the totality-tree
grammar-name leak that falsifies Lock 14's OWN verification command lives only in
1F, and the two CH2-charged status inventories (1E for lock-status, 1C for the
runtime grammar-named-module census) omit it.

I ran the EXACT Lock 14 verification command from `restart/locks/LOCKS.md:349`
(`rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser'
crates/{ir,parse,codegen,runtime,...,analysis,lsp}/src/`). LOCKS:349 asserts it
"returns ZERO". Live result: **13 sites** (11 `crates/ir/src/` + 2
`crates/analysis/src/`). The lock's own gate is RED.

`crates/core/src/css_types.rs` is live (66 LOC, `:1` "Host shims for the CSS L4
grammar's `-> parse_hex_color(...)` map") — the file LOCKS:349 names VERBATIM as "the
failure mode this lock prevents from recurring." Cited only by 1F (COH18-006).

## Findings

| id | disposition | finding | evidence |
|---|---|---|---|
| CH2-V4-001 | ACCEPT | 1D's JSON/CSS-empirical vs grammar-neutral split is exemplary; G-10 re-scopes the 94.1%/79.5% as CSS-empirical and keeps only the G6=WIRE rule neutral. `grep -v css` falsifier verified empty live. | `1D:181-191`, `1D:193-209`, `1D:206`; live `rg find_component_delim skinny/crates/runtime/src \| grep -v css` = empty |
| CH2-V4-002 | ACCEPT | 1F's totality `strategy.rs` 9-grammar idents-table leak and the "4 caught / 5 escape" narrow-regex precision are live-exact; consumer anchor `:216` real; 11-site `crates/ir/src/` total confirmed. | `crates/ir/src/registry/strategy.rs:137,143,149,155,161,167,173,179,185,216`; `1F:75`,`1F:99` |
| CH2-V4-003 | ACCEPT | The grammar-neutral discriminator holds: `select_lowering` is Lock-14-clean (zero grammar names, matches on `BackendShape`); the `RuntimeEmitterKind` fork and phantom `<G>` are catalogued AS leaks, not laundered. | `skinny/crates/codegen/src/lower/mod.rs:18-26`; `grammar_provider.rs:40-42`; `tape/mod.rs:175,179`; `1B:51,59`; `1C:34,37` |
| CH2-V4-004 | ACCEPT | Pattern-H census (71 total / 67 per-grammar / 67 `@generated` / 6867 LOC) reconciles exactly across 1C/1D/1F; the +4 traced to generic `tape/`. No neutral-vs-grammar miscount. | live `find`/`rg`/`wc`; `1C` C1/C2/D6, `1D` U-1, `1F` COH18-007 |
| CH2-V4-005 | REVISE | **1E L14 omits the totality-tree leak that falsifies Lock 14's own gate.** The authoritative lock-status row catalogues L14 `drifted (HIGH)` but cites ONLY skinny-tree leaks (`grammar_provider.rs`, `runtime_generator.rs`, `tape/mod.rs`, `lock14_baseline.rs`, `json/generated.rs`). It never cites `crates/ir/src/registry/strategy.rs:137-185` nor `crates/core/src/css_types.rs:1`, and never states that the LOCKS:349 verification command itself returns 13 (not 0). CORRECTION: add to `1E-locks-evidence.md` L14 row (and a new D-1E-V5 divergence row) the totality idents-table leak `crates/ir/src/registry/strategy.rs:137-185` and the lock-NAMED `crates/core/src/css_types.rs:1`, and record the live LOCKS:349 gate result = 13 sites (not ZERO) as the falsifier. | `1E:94`, `1E:105-113` (no `strategy.rs`/`css_types`/`crates/ir` cite — confirmed via `rg`); live LOCKS:349 command = 13 sites |
| CH2-V4-006 | REVISE | **1C's runtime grammar-named-module census omits `css_types.rs`.** CH2 charges 1C to flag every grammar-named module in a generic crate; `crates/core/src/css_types.rs` is a CSS-L4-named host-shim module IN the generic `core` crate, in 1C's audited tree (`crates/core/src/...`), and LOCKS:349 names it verbatim. 1C cites the Pattern-H runtime leaks (C2/C3 `__shape_support_CssL4Parser` etc.) but not `css_types.rs`. CORRECTION: add a `1C-runtime-evidence.md` divergence/table row citing `crates/core/src/css_types.rs:1` as a grammar-named module in the generic core crate (Lock 14 (c) does NOT apply — it is not a `crates/<grammar>/` declaration crate). | `1C` C1-C12 + D1-D8 (no `css_types` cite — confirmed via `rg`); `crates/core/src/css_types.rs:1`; `LOCKS.md:349` names it |
| CH2-V4-007 | REVISE | **1F COH18-012's "gate DOES catch it" framing softens the live RED gate.** COH18-012 reclassifies OFF impl-exceeds-spec with "the §9 gate `:2215` and `LOCKS.md:349` BOTH scan `crates/{ir,...}/src/` and DO catch `strategy.rs` (11 sites)." The sharper CH2 fact is that LOCKS:349 asserts its command "returns ZERO" and the live command returns 13 (11 ir + 2 analysis) — the lock's own verification gate is FALSIFIED, not merely "catching" a leak benignly. CORRECTION: in `1F-coherence-scan.md` COH18-012 (and the COH18-005 note), state that the LOCKS:349 verification command returns 13 sites live versus the asserted ZERO, so Lock 14's own gate is RED — distinct from "the wide command catches it." | `1F:82` (COH18-012 "DO catch it"); live LOCKS:349 = 13 (11 `crates/ir/src/` + 2 `crates/analysis/src/`) |

## Required Fold (V5)

1. **1E** — fold CH2-V4-005: add `crates/ir/src/registry/strategy.rs:137-185` (9-grammar
   idents table) and `crates/core/src/css_types.rs:1` to the L14 drift evidence and a
   new `D-1E-V5-10` row; record the live LOCKS:349 verification result = 13 sites (not
   ZERO) as the lock-self-falsifier. The skinny-tree leaks already cited remain.
2. **1C** — fold CH2-V4-006: add a runtime-census row for `crates/core/src/css_types.rs:1`
   as a CSS-named module in the generic core crate (Lock 14 (c) inapplicable).
3. **1F** — fold CH2-V4-007: re-word COH18-012 to state the LOCKS:349 command returns 13
   live (asserted ZERO) — the gate is RED, not benignly "catching."

Preserve verbatim in V5: `1D` G-10 CSS-empirical re-scoping and the J/G two-table
split; `1F` COH18-005/012 "4 caught / 5 escape" precision and the analysis
doc-comment completeness add; the Pattern-H 71-vs-67 trace. Do NOT broaden any
CSS-empirical scoped row (G-10, J-3) into fleet-neutral generality, and do not demote
the grammar-neutral G-rows into JSON/CSS-only lessons.

TALLY accept=4 revise=3 reject=0
