# SK-V14 W5B-FRONTEND A1: Generic Frontend / CSS L4 Construct Gap

Date: 2026-05-26.
Scope: read-only inspection of `skinny/crates/grammar/src/lib.rs` and `grammar/css/l4/*.bbnf`.
Output: this file.

## §1 — Findings (concrete, file:line cited)

W5B-FRONTEND is scoped as frontend/import/IR closure only. SPEC §8B requires
W5A admitted, V7 CRUD applied, Lock 14 W5B owner routing before source redress,
and explicit lowering of `@ws`, `@pretty`, `?w`, `>>`, `<<`, span capture,
typed host projections, and import graph through W5A's request without provider
deletion or generator replacement (`restart/skinny/tranches/sk-v14/SPEC.md:724`).

Current support is W5A fact-scanning, not frontend lowering.
`parse_runtime_source_facts` scans requested sources and records constructs at
`skinny/crates/grammar/src/lib.rs:141`; it detects `@import`, `@ws`,
`@pretty`, `@{`, `?w`, `>>`, `<<`, and `->` at
`skinny/crates/grammar/src/lib.rs:188`. The W5A test at
`skinny/crates/grammar/src/lib.rs:660` proves detection only.

The actual generic parser still lacks the compatibility constructs.
`parse_grammar` parses rules, resolves refs, and validates at
`skinny/crates/grammar/src/lib.rs:29`. Directives admit only `@import` and
`@token`; all other directives reject as `BBNF-DIRECTIVE-NOT-IN-SKINNY` at
`skinny/crates/grammar/src/lib.rs:309`. Atoms admit literals, regexes, groups,
and refs at `skinny/crates/grammar/src/lib.rs:425`. `ir::ExprKind` has no
import graph, whitespace/layout, projection, host capture, or fence node today
(`skinny/crates/ir/src/lib.rs:209`).

Construct gap inventory:

| Construct | Current support | Gap |
|---|---|---|
| `@ws` | Fact-scanned at `skinny/crates/grammar/src/lib.rs:192`; CSS uses it in `grammar/css/l4/stylesheet.bbnf:12`. | Parser rejects it as non-skinny directive; must lower request-scoped into layout, not public syntax. |
| `@pretty` | Fact-scanned at `skinny/crates/grammar/src/lib.rs:194`; CSS uses it in `grammar/css/l4/stylesheet.bbnf:53`. | No IR/layout representation; keep as compatibility metadata or discard through canonical lowering. |
| `?w` | Fact-scanned at `skinny/crates/grammar/src/lib.rs:198`; used in `grammar/css/l4/properties.bbnf:161`. | Parser sees leading `?` as unexpected except postfix optional; needs compatibility whitespace modifier lowering. |
| `>>` / `<<` | Fact-scanned at `skinny/crates/grammar/src/lib.rs:200`; used in `grammar/css/l4/values.bbnf:47`. | Scanner does not mark them unsupported, but parser cannot consume them; must lower as canonical sequencing/fence semantics. |
| Span capture `@{...}` | Fact-scanned as host capture at `skinny/crates/grammar/src/lib.rs:196`; CSS uses it in `grammar/css/l4/values.bbnf:67`. | Marked unsupported today; no canonical IR lowering for captured span. |
| Typed host projections | `->` is classified as typed if suffix matches primitive types at `skinny/crates/grammar/src/lib.rs:204`; CSS uses host/typed projections in `grammar/css/l4/color.bbnf:189` and `grammar/css/l4/color.bbnf:220`. | `Projection` and `TypedProjection` are unsupported constructs at `skinny/crates/grammar/src/lib.rs:101`. |
| Import graph | Request carries roots/sources at `skinny/crates/codegen/src/grammar_provider.rs:4`; CSS imports are real, e.g. `grammar/css/l4/values.bbnf:1`. | Current request validation only checks each root is present in the source map at `skinny/crates/codegen/src/grammar_provider.rs:99`; `@import` is consumed but not resolved. |

Lock 14 is not ready for W5B source redress. Current gate has
`SK_V14_W5A_OWNER_PATHS` only at
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1105`. V7 requires
`SK_V14_W5B_FRONTEND_OWNER_PATHS` and parent-diff routing before touching
frontend paths (`restart/audit/totality/astral/V7/ΩF-migration-handoff.md:41`).

## §2 — Recommendations (named falsifiability gates)

- `W5B-A1-LOCK14-FIRST`: before any frontend source edit, add
  `SK_V14_W5B_FRONTEND_OWNER_PATHS`, subject routing for
  `sk-v14-waveW5B-FRONTEND` and `sk-v14-waveW5B-FRONTEND-redress`, plus a unit
  test proving only those paths admit.
- `W5B-A1-COMPAT-LOWER`: add executable grammar tests proving the CSS
  compatibility dialect lowers `@ws`, `@pretty`, `?w`, `>>`, `<<`, span
  capture, typed projections, and imports into canonical IR. Also prove
  standalone public `@ws` remains rejected or request-scoped.
- `W5B-A1-IMPORT-DAG`: resolve `@import` from the W5A request source map, fail
  closed on missing imports/cycles, and cover at least `stylesheet.bbnf ->
  properties/selectors/media` plus `values.bbnf -> tokens/value-unit/color/...`.
- `W5B-A1-TOPOLOGY-UNCHANGED`: provider/template counts unchanged; no provider
  deletion or generator replacement (`restart/skinny/tranches/sk-v14/SPEC.md:746`).
- `W5B-A1-REQUEST-CONSUMER`: `cargo xtask regen-css`, seven `check-css-l4-*`
  companions, JSON unchanged-output proof, Sheets/BBNF fail-closed proof, and
  Lock 14 parent-diff test consume the frontend closure in the same commit
  (`restart/skinny/tranches/sk-v14/SPEC.md:742`).

## §3 — Risks (REDRESS entries to pre-block)

- REDRESS if implementation only extends W5A fact scanning while non-JSON still
  routes through `render_runtime_profile`: current request path still renders
  the provider profile after materiality checks (`skinny/crates/codegen/src/grammar_provider.rs:77`).
- REDRESS if `@ws` becomes public syntax. SPEC §8B requires compatibility
  lowering, not a new public directive (`restart/skinny/tranches/sk-v14/SPEC.md:728`).
- REDRESS if the wave deletes providers/templates, replaces the generator body,
  statically centralizes hand-written CSS runtime bodies, mines committed
  generated output, or borrows W5C/W5D/W6 budget
  (`restart/skinny/tranches/sk-v14/SPEC.md:758`).

## §4 — Sources (every external citation)

Local repository files only; no external sources used.
