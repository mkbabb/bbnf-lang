# PROVE Wave Ledger — the negative control PASSES: a 3rd grammar emits via the un-forked generator

Date: 2026-06-03. Wave: SK-V18 PROVE (§9). Predecessors: G1–G4, G6 closed. Outcome: **A (generator-only,
generalization REAL)**. The Google Sheets precedence tower emits a working recursive-descent parser
through the ONE generator with ZERO hand-authored Sheets runtime Rust — the negative control the whole
SK-V18 thesis rests on.

## 1 — What was built (two slices, both committed)
**Frontend** (commit `9eb2aca1f`): the skinny grammar frontend extended at the ROOT so a precedence-tower
grammar parses + lowers through the EXISTING pipeline — `ExprKind::Project` (`->`) + `ExprKind::Discard`
(`<<`/`>>`), `?w` as a standalone ws element, `,` as an explicit separator, `@pretty`/`@ws` accepted; the
nullable-repeat validator relaxed (loop-progress moved to the emitter). Purely additive — JSON/CSS
byte-identical. Result: Sheets lowers to a 39-rule `SinkOnlyProgram` (the 7-level tower +
cyclic `paren_expr` intact, `entry=formula`).

**Emitter** (this slice): a GENERAL recursive-descent emitter `codegen/src/general_descent/{mod,emit,
view,regex_first}.rs` over the `SinkOnlyProgram`, dispatched by SHAPE — `runtime_generator.rs`
`lower_for_runtime` tries `css_scan::derive_program` first (Ok→Scan/CSS byte-identical; Err→SinkOnly), and
`emit_sink_only` routes a `value`-rule program to `json_sink_direct` (byte-identical) ELSE to
`general_descent::render`. The emitter walks `SinkOnlyExpr` → one `parse_<rule>` fn per rule
(Seq / first-byte-dispatch Alt / ordered-try Alt / **progress-guarded** RepeatLoop / Optional / CallRule /
ByteLiteral / leaf-scan RegexProgram / tape marks) + a grammar-derived narrow G4 view
(`{Prefix}Document` impl `DocumentView`, `{Prefix}Node` impl `Cursor`, `{Prefix}Kind` enum). The type
prefix is DERIVED from the grammar name (`google_sheets` → `GoogleSheets`) — the path is TRULY general,
not Sheets-specialized.

## 2 — §9 exit gate (MEASURED, all GREEN)
```text
sheets_emission_path            = generator-only   (no SHEETS_GENERATED_RS blob; grep 'const.*_RS.*r#' for a Sheets blob == 0)
generator_grammar_branch_count  = 0                (rg '(GoogleSheets|Sheets)\w*=>|grammar_name=="google_sheets"' codegen/src == 0 — shape dispatch only)
generator_grammar_type_count    = 0                (no GoogleSheetsParser/EventGrammar literal in the emitter)
sheets_grammar_shape            = pratt-operator   (40 parse_ fns; the 8 chained level fns comparison→concat→add→mul→exp→unary→postfix→primary + parse_paren_expr/parse_expression cyclic back-edge)
generated_md5_distinct          = true             (sheets 289aa641… ≠ json 6518d673… ≠ css f7460fde…)
sheets_value_instantiates_g4    = true             (rg 'impl.*\b(Cursor|DocumentView)\b.*\bfor\b' grammars/sheets/ → 2: GoogleSheetsNode:Cursor, GoogleSheetsDocument:DocumentView; compiles)
generator_grammar_count         = 3                (json + css_l4 + google_sheets RuntimeTarget rows)
general_path_truly_general      = true             (rg '\bSheets[A-Z]' codegen/src/general_descent + runtime_generator == 0; prefix derived from grammar_name)
json_css_preservation_held      = true             (check-json + check-css-l4 byte-identical; runtime json suite green)
tower_core_recognizes           = true             (sheets_conformance.rs: =1+2*3, =(1+2)*3, =2^3, =1<>2, =-1+2, =TRUE, =FALSE, ="hello", =1, =SUM(1,2), =1&2 ALL Ok)
regen_clean                     = true             (check-sheets idempotent)
```

## 3 — Honest residual (sanctioned deferral, NOT a contrivance)
`import_closure_relaxation`: MOOT. The §9 plan assumed Sheets would route via the request-facts/Scan path
and need an import-closure data-relaxation. Because the frontend extension made Sheets FULLY lowerable, it
routes to the SinkOnly path with a `none()` frontend contract (like JSON) — a stronger result: Sheets is a
first-class SinkOnly grammar, not a request-facts special case. The relaxation is unnecessary.

`leaf precision`: the runtime has NO general regex matcher (JSON uses span-kind scanners, CSS uses
byte-scans), and the `bbnf-regex` HIR is a coarse 4-variant classifier with no walkable AST. So leaves use
a grammar-sanctioned Span delimiter-scan (the grammar's own AU.6.7 TODO defers `cell_ref`/`range`/`LET`/
`LAMBDA` to raw Span). The TOWER CORE — operators, precedence, paren grouping, literals, and `func_call`
with simple args (`=SUM(1,2)`) — recognizes EXACTLY. The deferred `cell_ref`/`range` leaves (`=A1`,
`=A1:B2`, `=SUM(A1:B2,3)`) are imprecise and `sheets_conformance.rs` characterizes them honestly (pinned to
recognize/parse-agreement, never to false precision). A general HIR-driven leaf matcher is the principled
follow-on; it does not change the PROVE verdict (the negative control is the tower SHAPE + generator-only
emission, both met).

## 4 — Lock-14
Owner paths authorize the PROVE surfaces (`crates/grammar/src`, `crates/ir/src`, `crates/passes/src`,
`crates/runtime/src/grammars/sheets/`, the codegen emitter, xtask). Generic-crate neutrality holds (the
new `general_descent` module is grammar-neutral — prefix DERIVED, never a hard-coded grammar token; the
emitted `GoogleSheets*` types live in the generated `grammars/sheets/`, not the generic crates). No
verbatim blob, no `GoogleSheets =>` arm, no parser-type literal.

## 5 — Verdict
The negative control PASSES: generalization is REAL. The ONE generator emits JSON (SinkOnly), CSS (Scan),
and Google Sheets (general recursive-descent) from `.bbnf`, dispatched by SHAPE, with JSON/CSS preserved
byte-identical and Sheets' tower recognized end-to-end. The throwaway feasibility probe is retired;
`sheets_conformance.rs` is the permanent characterization. Next: H1 (the honesty close — CSS framing +
corpus-in-timer + the rigorous G6 magnitude delta + regen-clean), then SK-V18 close.
