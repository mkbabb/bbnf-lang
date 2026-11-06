# 17 — parse-that Fold-In / Fold-Out Decision

## Verdict

The bespoke regex engine in `parse-that/rust/regex` *is* the canonical home, and the
totality tree already folds it in correctly: its package name is literally `bbnf-regex`
(`parse-that/rust/regex/Cargo.toml:2`, lib `bbnf_regex`), and bbnf-lang patches it in by
path at `.cargo/config.toml:48-52` (`[patch.crates-io]` → `bbnf-regex = { path =
"../parse-that/rust/regex" }`, `parse_that = { path = "../parse-that/rust/parse_that" }`).
Totality `crates/core` and `crates/ir` consume that engine and build only *emitters* and
*grammar-tier rules* on top. That boundary is the idiomatic shape and must be preserved.

The damage is entirely in the **skinny tree**, which carries two crates that collide by
name and intent with the canonical engine, plus a third re-implementation buried in skinny
codegen. All three are hand-rolled regex-string scanners overfit to the JSON / CSS / Sheets
corpus — Lock-14 and no-overfit violations. They must be deleted in favor of the canonical
`bbnf_regex`, not folded alongside it.

## The codebases at a glance

| Tree | Crate / path | What it is | Lines |
|------|-------------|------------|------:|
| parse-that | `rust/regex` → `bbnf_regex` | Real engine: hand-written HIR parser, NFA, DFA, accel, classify, first, info, sets, algebra, HIR e-graph | 6742 |
| totality | `crates/core/src/generate/regex/` | DFA/HIR/scanner *emitter* consuming `parse_that::regex::*` | 4378 |
| totality | `crates/ir/.../egraph/rules/regex.rs` + `passes/regex_info.rs` | grammar-tier rewrite rules delegating to `bbnf_regex::algebra`; regex-info cache pass | ~870 |
| skinny | `crates/bbnf-regex/src/lib.rs` | hardcoded literal-pattern classifier (overfit) | 322 |
| skinny | `crates/parse-that-regex/src/` | runtime string/number decode helpers (mis-named) | ~2400 |
| skinny | `crates/codegen/.../general_descent/regex_first.rs` | third ad-hoc FIRST/nullable scanner | 230 |

## Canonical engine — KEEP-OUT of any rewrite, already FOLDED-IN as dependency

`rust/regex/src/lib.rs:1-39` declares the full surface: `algebra automata classify egraph
first hir info path_lexer sets unicode utf8`. The HIR (`hir/mod.rs:44-58`) carries the
explicit `negated: bool` flag on `CharClass::{Bytes,Unicode}` exactly as the precept
demands, with `negated()` (`:62`) replacing heuristic byte-count detection — the doc at
`:42-43` states this is the whole point. NFA→DFA lives at `automata/{nfa,dfa,accel}.rs`
(449/438/131 LOC). The hand-written parser is split into proper sub-modules
`hir/parser/{atom,class,cursor,escape,group,mod,repetition}.rs` — no god module.
`classify.rs` (1148 LOC) exposes `RegexClass` + `ClassRangeInfo` (`:55,36,157,177`), the
dispatch surface the totality emitter keys on. The crate has zero combinator or grammar
dependencies (`lib.rs:3-4`), so it is already a clean general-infra crate
(`general-infra-crates`, `regex-generalized`).

Totality consumes it correctly: `crates/core/.../emit/dfa/mod.rs:26-28` imports
`parse_that::regex::{dfa::Dfa, nfa::DEAD, accel::detect_accel}`; `emit/hir/mod.rs:16-17`
imports `parse_that::regex::hir::{CharClass, Hir}` and `info::is_nullable`;
`last_byte_set.rs:28` consumes `RegexInfo`. The grammar-tier e-graph rules at
`crates/ir/src/egraph/rules/regex.rs:108,154,387` delegate superset/union to
`bbnf_regex::algebra::{pattern_is_superset, try_union_patterns}` — one algebra home reused
by both the grammar tier and the HIR tier (module doc `:11-15`). This is the gestalt shape:
**engine in parse-that, emitters in totality, ONE algebra.**

## DUPLICATE-RISK — skinny `crates/bbnf-regex` collides with the canonical name

The sharpest finding. The skinny path crate `skinny/crates/bbnf-regex` (`Cargo.toml:2`,
package `bbnf-regex`, wired at `skinny/Cargo.toml:32`) shares the **exact crate name** with
the canonical engine that totality pulls via the `.cargo/config.toml:52` path patch. Two
distinct crates named `bbnf-regex` — one a 6742-line engine, one a 322-line toy. Any future
skinny→totality convergence collides on this name; the canonical home is unambiguously the
parse-that engine.

Worse, the skinny crate is **overfit to the test corpus** — a direct Lock-14 and no-overfit
violation. `lib.rs:92-105` classifies by literal string match: `pattern == r"[ \t\n\r]*"` →
`Whitespace`; `pattern.starts_with("-?(0|[1-9]")` *and* `contains("[eE]")` → `Numeric`;
`starts_with('"')` → `QuotedString`. These are the JSON number / JSON string / whitespace
patterns hardcoded as recognizer keys. The enum `RegexKind {Whitespace, QuotedString,
Numeric, Unknown}` (`:16-21`) *is* the JSON/CSS grammar baked into a "generic" crate. The
test at `:296` asserts the literal JSON string regex maps to `QuotedString` — the overfit is
encoded into the test surface. The canonical engine answers the same questions structurally
via `classify_regex_from_hir` (`classify.rs:177`) and `regex_first_chars` (`first.rs:12`),
from a parsed HIR, with zero per-grammar literals.

**Decision: DELETE skinny `crates/bbnf-regex`. Canonical home: parse-that `bbnf_regex`.**
The skinny `ir` and `passes` crates depending on it (`skinny/crates/ir/Cargo.toml:10`,
`skinny/crates/passes/Cargo.toml:10`) switch to `bbnf_regex::{classify_regex,
regex_first_chars, RegexInfo}` — exactly the surface totality already consumes.

## DUPLICATE-RISK — skinny codegen re-implements FIRST/nullable a third time

`skinny/crates/codegen/.../general_descent/regex_first.rs` (230 LOC) is a *third* hand-rolled
regex-string scanner. Its header (`:1-10`) admits the motive: "the `bbnf-regex` HIR is a
coarse classifier, so the general descent emitter derives each leaf's first-byte set HERE."
It then lists the corpus leaf shapes verbatim — `[A-Za-z_]\w*`, `\$?[A-Za-z]{1,3}\$?\d+`,
the Sheets sheet-ref form, `[lL][eE][tT]\(`, the JSON `"..."` and numeric forms (`:6-8`).
That is the Sheets+JSON corpus written into a "pattern-general" comment. `is_nullable`
(`:18`) and `is_single_atom` (`:36`) duplicate the canonical `bbnf_regex::first` +
`info::is_nullable` (the latter already imported by totality at `emit/hir/mod.rs:17`).

The root cause is that skinny vendored the *toy* classifier instead of the real engine, then
had to hand-derive FIRST sets the toy could not provide. The fix is to delete both skinny
scanners and depend on the canonical engine, whose `regex_first_chars`/`RegexInfo` already
cover every shape listed.

**Decision: DELETE `regex_first.rs`. Canonical home: `bbnf_regex::{first, info}`.**

## skinny `crates/parse-that-regex` is mis-named, not an engine duplicate

Despite the name, `skinny/crates/parse-that-regex` (`lib.rs`, `number/`, `unicode/`,
`integration/`) is **not** the regex engine — it is a runtime string/number *decode* library:
`StringMode`, `StringFlags`, `match_number_span`, Eisel-Lemire float parsing
(`number/eisel_lemire/`), Höhrmann UTF-8 validation (`unicode/utf8_hoehrmann.rs`). Wired into
skinny `runtime` and `bbnf-bench` (`runtime/Cargo.toml:11`, `bbnf-bench/Cargo.toml:17`). This
is legitimate runtime scaffolding, but its name falsely implies it is the parse-that regex
engine, and its number/string decode overlaps parse-that's own runtime
(`parse_that/src/parsers/`, `parse_that/src/regex/`).

**Decision: KEEP-OUT (runtime, stays in skinny) but RENAME** to disclose what it is. Audit
its number/string decode against `parse_that` runtime — a separate fold front from the
regex engine.

## TS side — precept honored, FOLD-OUT (stays in parse-that)

The TypeScript tree (`typescript/src/parse/`) has **no IR layer** — a grep for
`intermediate.represent | \bIR\b | lower` over `src` returns nothing, and there is no
`*regex*` or standalone `*combinator*` file. The combinator *is* the AST: `parser.ts`,
`leaf.ts`, `state.ts`, `span.ts`, and the grammar parsers (`parsers/json.ts`,
`parsers/css/*.ts`) are direct combinator definitions. This matches the `no-ts-ir` precept
exactly. Nothing to fold into totality; the TS side stays in parse-that as the reference
combinator implementation and cross-language conformance oracle. **FOLD-OUT.**

## Per-primitive fold map

| Primitive | Canonical source | Decision | Canonical home |
|-----------|-----------------|----------|----------------|
| HIR + hand-written parser (negated flag) | `parse-that/regex/src/hir/` | KEEP-OUT (patched dep) | `bbnf_regex::hir` |
| NFA construction | `regex/src/automata/nfa.rs` | KEEP-OUT (dep) | `bbnf_regex::automata::nfa` |
| DFA compilation | `regex/src/automata/dfa.rs` | KEEP-OUT (dep) | `bbnf_regex::automata::dfa` |
| DFA *codegen* (emit Rust tables) | `crates/core/.../emit/dfa/` | FOLD-IN (done) | `bbnf-core` emitter over `bbnf_regex::Dfa` |
| Acceleration detection | `regex/src/automata/accel.rs` | KEEP-OUT (dep) | `bbnf_regex::automata::accel` |
| Char-class / byteset kernels | `regex/src/sets/` | KEEP-OUT (dep) | `bbnf_regex::sets` |
| classify (`RegexClass`/`ClassRangeInfo`) | `regex/src/classify.rs` | KEEP-OUT (dep) | `bbnf_regex::classify` |
| FIRST-set machinery | `regex/src/first.rs` | KEEP-OUT (dep) | `bbnf_regex::first` |
| HIR-analysis info (nullable, width, one-pass, literal-prefix) | `regex/src/info/` | KEEP-OUT (dep) | `bbnf_regex::info` |
| Regex algebra (superset, union) | `regex/src/algebra/` | KEEP-OUT (dep) | `bbnf_regex::algebra` |
| HIR e-graph rules | `regex/src/egraph/rules/` | KEEP-OUT (dep) | `bbnf_regex::egraph` |
| Grammar-tier regex e-graph rules | `crates/ir/.../egraph/rules/regex.rs` | FOLD-IN (done) | `bbnf-ir`, delegates to `bbnf_regex::algebra` |
| Regex-info cache pass | `crates/ir/src/passes/regex_info.rs` | FOLD-IN (done) | `bbnf-ir` |
| path_lexer (path-string literals) | `regex/src/path_lexer.rs` | KEEP-OUT (dep) | `bbnf_regex::path_lexer` |
| skinny toy classifier | `skinny/crates/bbnf-regex` | **DELETE — DUPLICATE** | replace with `bbnf_regex::{classify,first}` |
| skinny codegen FIRST scanner | `skinny/.../regex_first.rs` | **DELETE — DUPLICATE** | replace with `bbnf_regex::{first,info}` |
| skinny runtime decode (mis-named) | `skinny/crates/parse-that-regex` | KEEP-OUT + RENAME | skinny runtime; audit vs `parse_that` runtime |
| TS combinators (AST = combinator) | `typescript/src/parse/` | FOLD-OUT | stays in parse-that |

## Recommendations (gestalt, not patch)

1. **One regex engine, one name.** Delete `skinny/crates/bbnf-regex` and point skinny's
   `ir`/`passes` at the canonical `bbnf_regex` via the same path patch totality already uses
   (`.cargo/config.toml:52`). The name collision is non-negotiable to resolve before any
   skinny→totality convergence; two crates named `bbnf-regex` cannot coexist in one workspace.

2. **Delete the third FIRST scanner.** `regex_first.rs` exists only because skinny vendored
   the toy. Once skinny depends on the real engine, `regex_first_chars` + `info::is_nullable`
   subsume it, and the corpus-shape list in its doc comment disappears with it.

3. **Rename `parse-that-regex`** to stop it impersonating the engine, and run a separate
   number/string-decode unify check against `parse_that`'s runtime parsers — a second
   duplication front the regex fold does not cover.

4. **Lock the boundary in ARCHITECTURE.md:** parse-that owns the regex *engine* and the TS
   combinator reference; totality owns the *emitters* (DFA codegen, scanner plans) and the
   grammar-tier e-graph rules; skinny owns runtime decode only. No grammar-specific literal
   may live in `bbnf_regex` — the skinny toy proves that line was already crossed once.
