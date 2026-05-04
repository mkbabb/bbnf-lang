# PASS-3 — User Surface + Ecosystem (Greenfield Restart)

You are the orchestrator for **PASS-3: User Surface + Ecosystem.** You own the top layer: the `bbnf` aggregator crate; the value API (all four materialisation surfaces); the path + select DSLs; the visitor surface (read-write); the ParseStream union (the proper tape ↔ direct-to-struct synthesis per Q25 — this is the architectural keystone of PASS-3); error recovery (treesitter MISSING/ERROR nodes); incremental parsing (opt-in batch + always-on for LSP); the LSP; the CLI; the bench harness; fixture infrastructure; debug facilities; playground compatibility (extended, not rewritten). Sister passes own substrate (PASS-1) and codegen + backends (PASS-2).

You dispatch six sub-agents in parallel. You synthesise. Single-round suite.

## Required reading (mandatory; in order)

1. `/Users/mkbabb/Programming/bbnf-lang/restart/README.md` — gestalt anchor; particularly §8 (Value API + Path DSL) and the ParseStream union directive
2. `/Users/mkbabb/Programming/bbnf-lang/restart/locks/14-LOCKS.md`
3. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/STYLE.md` + `LESSONS-LEARNED.md` + `CONSUMING.md`
4. `/Users/mkbabb/Programming/bbnf-lang/restart/corpora/SOTA.md`
5. `/Users/mkbabb/Programming/bbnf-lang/restart-archive-2026-05-04/audit/passes/PASS-C.md` — prior periphery audit
6. `/Users/mkbabb/Programming/bbnf-lang/restart-archive-2026-05-04/audit/master-plan/MASTER-PLAN.md` + Amendment 01
7. `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/BB/BB.md` + waves W4/W5 — slice-borrow API + pointer macro + visitor + cookbook inheritance
8. `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/BB/audit/W5-pointer-syntax-decision.md` — option (iii) ratified (explicit + implicit forms)
9. `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/BB/audit/W5-visitor-bitflag-spec.md`
10. `/Users/mkbabb/Programming/bbnf-lang/docs/cookbook/{path-macro, lifetime-surfaces, visitors}.md` — Phase-4 stubs; refine + integrate
11. PASS-1 outputs at `restart/audit/pass-1-substrate/PASS-1.md` — read the type system + grammar extension specifications
12. PASS-2 outputs at `restart/audit/pass-2-codegen/PASS-2.md` — read the Backend IR contract + runtime template + per-grammar emit shape
13. The bbnf-lang source tree at `crates/{lsp, analysis, bbnf-path, bbnf-path-ts}` + `playground/` + `extension/` — read for inheritance signal
14. simdjson on-demand API source + sonic-rs LazyValue source + lightning-css Visitor source + treesitter parsing API source — for SOTA synthesis

## PASS-3 Scope

The user surface + ecosystem layer comprises six concerns.

| # | Concern | Lens |
|---|---|---|
| 1 | **Value API Designer** | the four materialisation surfaces uniform per grammar — `as_<T>()` / `try_into()` / typed-property access / visitor; the typed-record shapes (leaf → slice/scalar wrapper; Seq → struct; Alt → enum; Repeat → Vec; Optional → Option) per Q10 hybrid; deep-enum support; per-grammar codegen size projection. sonic-rs / chumsky / serde-derive idiom synthesis. |
| 2 | **Path + Select DSL Designer** | `pointer!` macro (Q24 ii, sonic-rs idiom; explicit + implicit forms per BB.W5 option iii); `select!` macro (Q24 iii, lightning-css / treesitter query DSL); both compile-time proc-macros; both grammar-derived (read per-grammar registry); shared substrate at `path-core` (path AST + lex + lower + validate + runtime). Diagnostic messages per friction-forecast lane (verbatim error strings). |
| 3 | **Visitor Surface Designer** | `Visitor<'i, T>` trait; `VisitTypes` bitflag bitmask; per-record `visit_<Name>(&mut self, &mut T)` methods; read-write semantics (Q27); CSS L4 visit_color / visit_length / visit_url / visit_property; JSON visit_string / visit_number / visit_object / visit_array; mutation safety under slice-borrow (visitor controls lifetime). lightning-css idiom. |
| 4 | **ParseStream Union Architect** (the keystone) | the proper synthesis of tape (simdjson contiguous-token-stream-with-offset-references) + direct-to-struct (sonic-rs LazyValue + chumsky typed combinators) per Q25 — **the user's deep concern is the 2,000-commit failure to union them; the greenfield must succeed.** Specify: ParseStream layout (token discriminant + span + payload offset); typed-value-borrow shape (`JsonValue<'i> { kind: Kind, span: Span, stream: &'i ParseStream<'i>, idx: u32 }`); materialisation cost (one-load typed access; lazy escape-handling; SIMD-accelerated scan); slice-borrow integration (`&'i str` source borrow + ParseStream offset reference); bumpalo opt-in arena (`parse_in(&'i str, &Bump)` returns `JsonValue<'arena, 'i>`); owned escape (`parse_owned(&str)` deep-copies). |
| 5 | **Error Recovery + Incremental Parsing** | treesitter-style MISSING / ERROR node insertion (Q32); lossless concrete syntax tree (rowan-inspired); external scanners (escape valve when BBNF expressiveness exceeded); incremental parsing (Q30 ii + iii — opt-in feature mode for batch parsers; always-on for LSP-class consumers); stable node identity; diff-against-prior-tree algorithm; LSP integration path; debug facilities (partial reason to keep VM per Q7 — the VM is the debug runtime; deep error/playback per Q13). |
| 6 | **Ecosystem Architect** | `bbnf` aggregator crate (re-exports Parser, Value, Document, Visitor, pointer!, select!); `bbnf-cli` (post-restart user-facing CLI); `bbnf-language-server` (consolidates analysis + lsp; metadata-dispatched per Stage-1 PASS-C); `bbnf-bench` (vitest-style); `test-fixtures` (per-grammar fixture files; no Rust per-grammar; one harness iterates workspace metadata); playground compatibility (extended per Q13, not rewritten); cookbook + diagnostic + migration documentation gates. |

## Per-Item Discipline

Pro / Con / Explication / Challenge. KEEP / REINVENT / DISCARD. Steelman every challenge. KEEP without challenge is fault.

## Per-Sub-Agent Output

`restart/audit/pass-3-runtime/agent-{N}-{lens}.md`, ~500-1000 lines each. Same §1-§7 structure as PASS-1 + PASS-2 sub-agents.

## Synthesis (your output)

`restart/audit/pass-3-runtime/PASS-3.md`, ~1500-2500 lines:

§1 PASS-3 verdict ledger
§2 User surface + ecosystem architectural commitments (Value API surface table per grammar; pointer! + select! macro syntax; Visitor + VisitTypes contract; ParseStream layout; error recovery model; incremental parsing data model; LSP integration; CLI surface; bench harness shape)
§3 Per-crate `src/` tree — for each PASS-3 crate (`bbnf`, `bbnf-cli`, `bbnf-language-server`, `bbnf-bench`, `path`, `path-core`, `path-ts`, `test-fixtures`)
§4 Hand-offs back to PASS-1 — any substrate concerns surfaced (e.g., the ParseStream representation may demand a new Grammar IR side-table)
§5 Hand-offs back to PASS-2 — any codegen concerns surfaced (e.g., the Visitor surface emit shape; the incremental parser's stable-node-identity emit)
§6 Cookbook + diagnostic ledger — verbatim error messages per friction surface (pointer macro grammar inference; lifetime API mental model; layout lowering errors; Pratt+SIMD detection misfire; crate split migration; adding a new grammar)
§7 Performance gate trajectory — twitter / canada / citm / bootstrap / animate per-tranche numbers
§8 Inheritance ledger — BB W4-W5 + cookbook stubs + BC W4 visitor inheritance
§9 PASS-3 punch list
§10 Closing posture

## Voice + Discipline

(Standard. Per `restart/README.md` §13.)

## Hard cap

You: 75 minutes. Each sub-agent: 45 minutes. Incremental-commit cadence if stall risk.

## Output commits

Per sub-agent: `docs(restart/audit/pass-3-runtime/agent-{N}): {lens}`.
Orchestrator final: `docs(restart/audit/pass-3-runtime): synthesise PASS-3 — user surface + ecosystem`.

## Cross-tranche scope boundary

Touch ONLY `restart/audit/pass-3-runtime/`. Do NOT modify other restart subdirs, `crates/`, `docs/`. PASS-1 + PASS-2 outputs are read-only inputs.

## Background

PASS-3 closes the substrate-codegen-runtime triplet. Single-round suite. The 14 locks govern. Particular foci: Lock 9 (slice-borrow primary; bumpalo + owned escape hatches honour at the parse / parse_in / parse_owned API surface); Lock 14 (full grammar generalisation honours at the user-facing API — adding a 10th grammar requires zero edit in any generic crate).

The user-stated discipline at PASS-3: the ParseStream union is the architectural keystone — 2,000 commits of failed unioning must succeed in the greenfield. The familiar API (sonic-rs / lightning-css / treesitter idioms) is the contract; innovation lives in the deep internals.

The friction-forecast lane is consequential here — every user-facing surface gets a verbatim error message + cookbook page + migration page. No hand-waving. No "users may find this confusing" — specify the user, the model, the point of confusion, the exact error string.

Familiar surfaces. Deep optimisation underneath. Idiomatic gestalt.
