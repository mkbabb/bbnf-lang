# LOCK-7 · Family 7B — TOTAL-SELF-HOST (pass-1 research)

**Thesis.** Every syntactic plane of the generator is a bbnf grammar carried as *data* — the
bbnf grammar parses itself, the regex grammar parses itself, the middle IR is grammar-defined,
and the Rust-emission templates are grammar-specified. Hand-written Rust contracts to (a) the
combinator/SIMD/tape *primitives* and (b) a fixed *meta-emitter* that walks a grammar-derived
IR under a template grammar. The generator "becomes mostly data"; the correctness proof is that
the machine reproduces every generated organ bit-for-bit, and the honesty proof is that
build-time and debuggability are *measured*, not assumed away.

This family is distinguished from its siblings by ambition of *coverage*: not merely a staged
bootstrap of the bbnf parser (7A's minimal surface), nor a totality *bounded* by a typed
host-fn floor (7C), nor self-host-as-verification-first (7D). 7B asserts the maximal claim —
four planes as data — and lives or dies on whether the maximal claim survives the two budgets
the charter names.

---

## 1. The baseline: what self-hosting is real, what is aspirational, what is a hack

### 1.1 The regex grammar is ALREADY self-hosted (living proof, in-repo)

`parse-that/rust/parse_that/src/regex/generated.rs` (2293 LOC, 184 KB) is a **generated typed
parser** produced from `parse-that/rust/bootstrap/regex.bbnf`. It is the existence proof for
7B's central claim: a grammar can generate its own front-end. The artifact is not an
interpretive table — it is a typed sum-of-products AST *derived from grammar shape*:
`RegexParserEnum<'a>` with variants `char_class((Span, &[RegexParserEnum]))`,
`alternation((&RegexParserEnum, &[RegexParserEnum]))`, `quantified((&RegexParserEnum,
Option<&RegexParserEnum>))`, plus synthetic `class_atom_0`/`group_0`/`atom_0` variants for
inlined sub-alternations, and a `GRAMMAR_RegexParser: [&str; 1]` const carrying the source. The
recursive spine (`alternation → concat → quantified → atom → group`) survives the IR optimizer;
leaf rules inline. This is precisely "the generator becomes mostly data" realized for one plane.

**But the bootstrap MECHANISM is a hack to be replaced.** The regen path
(`parse-that/rust/scripts/bootstrap-regex.sh`) is: `#[derive(bbnf_derive::Parser)]` on a marker
struct → `cargo expand -p regex-bootstrap` → a **Python `re.sub` post-processor** that rewrites
`::core::panicking::panic_fmt(format_args!(…))` back to `panic!`, munges `struct
__RegexParserEnumCtx` to `pub(crate)`, and strips a duplicate `use crate::*`. This out-of-band
`cargo expand`+sed pipeline is fragile by construction and is exactly the workaround the standing
law forbids (`no-workarounds-self-hosting`: augment BBNF, never work around). The `bbnf_derive`
proc-macro crate was subsequently **retired** (bbnf-lang commit `6142387f`), and
`bootstrap/src/lib.rs` is now a one-line re-export of `parse_that::regex::generated`. So the
regex plane is self-hosted *in artifact* but its *generation path is orphaned* — it no longer
regenerates through a principled engine. 7B's first concrete deliverable re-homes it onto
`cargo xtask regen`.

### 1.2 The bbnf grammar is NOT self-hosted in skinny — a hand parser holds the seam

The self-hosting bbnf grammar exists as a **three-module** data set: `grammar/bbnf/bbnf.bbnf`
(`@import { value_expr, type_annotation } from "expressions"`, `@import { type_name } from
"types"`), `grammar/bbnf/expressions.bbnf` (the `->` value-DSL: precedence-climbing `value_or →
value_and → … → value_atom`, closures, `value_fn_call`), `grammar/bbnf/types.bbnf` (primitive-
width + identifier type names). The regex grammar `grammar/misc/regex.bbnf` likewise exists as
data.

Yet the operative skinny front-end is a **hand-written recursive-descent parser**:
`skinny/crates/grammar/src/lib.rs:1259` (`struct Parser`), entry `parse_grammar` at `:29`, ~883
LOC of hand parsing (`parse_alt` `:1336`, `parse_seq` `:1361`, `parse_directive` `:1292`). It
accepts only a *subset* of what `bbnf.bbnf` describes: `parse_directive` (`:1319-1329`)
explicitly **rejects** `@recover`/`@debug`/`@host` with `BBNF-DIRECTIVE-NOT-IN-SKINNY`, and
records only `@import`/`@token`/`@pretty`/`@ws` as no-op facts. An earlier extension taught the
hand parser `->` (`ExprKind::Project`), `?w` standalone, `,` separators, and `<<`/`>>`
(`ExprKind::Discard`) so the Sheets tower would build (previously it failed at byte 329 on
`-> f64`). The result is a **structural drift**: `bbnf.bbnf` (the aspirational total grammar) and
the hand parser (the operative subset) can silently disagree. Total-self-host closes that drift
*by construction* — the parser is generated FROM `bbnf.bbnf`, so grammar and parser cannot
diverge.

### 1.3 The emitter is 10.2 k LOC of string templates — the largest hand-code mass

`skinny/crates/codegen` is 10,201 hand-written LOC. Emission is **stringly-typed**:
`format!`/`writeln!`/`push_str` over const `&str` blobs. `runtime_generator.rs` (522 LOC)
concatenates `GENERATED_HEADER`, `JSON_PARSE_ONLY_PARSER_RS` (`:109`), and `CSS_GENERATED_RS`;
`json_sink_direct/skeleton.rs` (330) and `general_descent/emit.rs` (452) build Rust source by
push. There is **zero `quote!`/`syn`** usage. This blob-templating is the "hand code" 7B seeks
to convert to data: its per-grammar specialization (`json_sink_direct`, `css_scan_direct`,
`general_descent`) is the very surface a template grammar would unify.

### 1.4 The IR is a hand Rust enum — no grammar defines it

`GrammarIr`/`ExprKind`/`BackendShape` live as hand enums in `ir/src/lib.rs` (1020 LOC;
`BackendShape` at `:361`, `Recognizer`/`SimdScan` at `:416`), cost model in `ir/src/cost.rs`. No
bbnf grammar specifies the IR. 7B's third plane is net-new and aligns directly with LOCK-3C
(GRAMMAR-HOMOIOUSIA): a bbnf-defined IR grammar whose node types are *derived* the way
`RegexParserEnum` is derived, and whose lowerings are grammar-to-grammar maps.

### 1.5 Regen infra exists per-target but has no self-host fixpoint

`xtask/src/main.rs:39-52` dispatches `regen-json`/`check-json`, `regen-sheets`/`check-sheets`,
`regen-css`, `regen-real-typed`; targets `JSON_TARGET` (`:177`), `SHEETS_TARGET` (`:200`);
`regen.rs:19` `write_targets`, `:37` `check_target` (the bit-for-bit staleness gate). There is
**no** `regen-bbnf`/`regen-regex`/`regen-ir`, and no `regen∘regen = regen` fixpoint gate for the
self-hosting planes — because those planes are off the generator path. The infra is the right
*shape* (write + check-stale-bit-for-bit) but must be extended with self-host targets.

---

## 2. The mechanism made concrete — four data planes over a fixed meta-emitter

**Plane 1 — bbnf grammar (self-parse).** Generate the skinny grammar front-end FROM
`grammar/bbnf/{bbnf,expressions,types}.bbnf`; the generated parser replaces the 883-LOC hand
parser at `grammar/src/lib.rs:1259`. Consumer (Y.13): the generator itself — every `.bbnf`
compiled travels the generated parser. This plane *requires the generator to implement
`@import`* (the three-module split is not decoration — `bbnf.bbnf` cannot self-parse without
module resolution) and the value-DSL of `expressions.bbnf`: net-new generator capability, not
mere re-homing.

**Plane 2 — regex grammar (self-parse).** Re-home the orphaned parse-that regex bootstrap (§1.1)
onto `cargo xtask regen-regex`, deleting `bootstrap-regex.sh`'s `cargo expand`+Python pipeline.
The generated regex parser is emitted directly by the same engine that emits Plane 1, unified
under LOCK-5's ONE-HIR verdict (regex HIR as a sub-language of bbnf HIR). Consumer: the regex
front-end / `bbnf-regex` HIR construction.

**Plane 3 — IR grammar (grammar-defined middle).** Author a bbnf grammar for `GrammarIr`/
`ExprKind`/`BackendShape`; IR node types are derived (as `RegexParserEnum` is). Transpositions
AST→HIR→mid-IR→BackendShape become grammar-to-grammar maps expressed through `->` projections
plus typed host-fn escapes where context demands. Consumer: the lowering passes
(`passes/src/lib.rs` `derive_recognizers` `:338`, `derive_backend_shape` `:398`). Alignment:
LOCK-3C.

**Plane 4 — emitter templates (grammar-specified).** The Rust-emission blobs (§1.3) become a
*template grammar*: a grammar whose leaves are Rust-syntax fragments and whose `->` projections
drive substitution over the IR-grammar's derived nodes. The fixed **meta-emitter** — the one
surviving hand-written codegen kernel — walks the IR under the template grammar. The three
per-grammar emitters collapse into one template-driven lowering keyed on IR *shape*, never
grammar name (the zero-name-gate law).

**The surviving hand-code floor** (the "kernels and primitives"): the combinator primitives
(LOCK-4), the aarch64 SIMD kernels (`bbnf-simd`), the offset-tape runtime, and the meta-emitter.
Everything else is `.bbnf` data. This is the measurable claim: hand-LOC before vs after.

**The bootstrap ordering** (7B rides 7A as substrate): a *committed stage-0* generated parser
(the hand parser's output, frozen as data) is the metagrammar anchor; the hand parser is retired
only after stage-0 + the fixpoint gate (`regen-bbnf` then `check-bbnf` byte-identical) are green.
7D (META-CIRCULAR-ORACLE) is the safety net: the hand parser survives as a *differential oracle*
during promotion, plane by plane, gated on parity + build-time + debuggability — never a
big-bang cutover.

---

## 3. Prior art (with citations) — the totality question is old and answered in fragments

- **META-II (Schorre, 1964)** — the canonical meta-circular bootstrap: a compiler-writing
  language that compiles *itself* from a grammar. The root of "the parser is its own grammar";
  establishes that a fixpoint stage-0 is the whole game.
- **BNFC — the BNF Converter (Forsberg & Ranta)** — the closest prior art to 7B's *thesis*: from
  ONE labeled-BNF grammar it generates a lexer, parser, AST datatypes, pretty-printer, AND a
  skeleton transformer, in multiple backends (Haskell/C/Java/OCaml). "The generator becomes
  mostly data" already shipped. Its known limits are *exactly* 7B's budgets: generated parsers
  are large and hard to debug, and context-sensitive constructs escape to hand code. Cite BNFC as
  both encouragement and cautionary bound.
- **MLIR ODS / LLVM TableGen** — the industrial "IR + emitter as data" reference. TableGen `.td`
  files declaratively specify operations; a fixed generator emits C++ op classes, verifiers,
  printers. MLIR ODS (Operation Definition Specification) is Planes 3+4 in production. Lesson: the
  declarative plane is data, but the *semantics* (folding, canonicalization) stay hand-written —
  the totality is bounded, which pressures 7B toward the 7C floor.
- **Spoofax / SDF3 + Stratego (Visser)** — the SOTA *language workbench*: syntax (SDF3),
  transformation (Stratego), and static semantics declaratively specified; the closest academic
  realization of "all planes grammar-specified." Its JSGLR generator and origin-tracking
  debuggability story are directly relevant to 7B's debug budget.
- **tree-sitter** — `grammar.js` → generated C tables. The metagrammar is NOT self-hosted (a
  fixed JS harness parses it); generated parsers are giant state tables. Debuggability lesson:
  table-driven generated parsers are near-unsteppable — argues for 7B's *typed-AST* generated
  form (as `RegexParserEnum` already is) over tables.
- **GHC** — parser is Happy-generated from `Parser.y` (grammar → LALR table); Template Haskell is
  the "code as data" analog. **Racket `#lang`** — meta-circular language definition via grammars
  + macros.
- **Rust `syn`/`quote!`** — the disciplined alternative to string-blob emission (§1.3); a stepping
  stone (templates as token trees before templates as grammar). `proc_macro`+`cargo expand` is
  precisely the fragile path the parse-that regex bootstrap already proved hazardous (§1.1).
- **egg / egglog** — rules-as-data; the rewrite plane Plane-3's transpositions consume (shared
  with LOCK-2's e-graph home).

---

## 4. Integration seams (path:line)

| Concern | Seam |
|---|---|
| Hand parser to replace (Plane 1) | `skinny/crates/grammar/src/lib.rs:1259` (`struct Parser`), entry `:29`, directive reject `:1319-1329` |
| Self-host grammar data | `grammar/bbnf/{bbnf,expressions,types}.bbnf`; `grammar/misc/regex.bbnf` |
| Living regex bootstrap (Plane 2 prior art in-repo) | `parse-that/rust/parse_that/src/regex/generated.rs` (2293), `parse-that/rust/bootstrap/regex.bbnf`, `parse-that/rust/scripts/bootstrap-regex.sh` (the hack to delete), `bootstrap/src/lib.rs` (re-export) |
| Emitter blobs to templatize (Plane 4) | `codegen/src/runtime_generator.rs` (`JSON_PARSE_ONLY_PARSER_RS:109`, `CSS_GENERATED_RS`, `GENERATED_HEADER`, `format!`/`writeln!`/`push_str`); `json_sink_direct/skeleton.rs:330`; `general_descent/emit.rs:452`; `css_scan_direct/*` |
| IR to grammar-define (Plane 3) | `ir/src/lib.rs` (`BackendShape:361`, `Recognizer/SimdScan:416`), `ir/src/cost.rs`; lowerings `passes/src/lib.rs:338,398` |
| Codegen entry (the meta-emitter's host) | `codegen/src/lib.rs:101` `emit_from_source`, `:130` `emit_typed_from_source` |
| Regen infra to extend (fixpoint gate) | `xtask/src/main.rs:39-52` dispatch, `JSON_TARGET:177`, `SHEETS_TARGET:200`; `xtask/src/regen.rs:19` `write_targets`, `:37` `check_target` |
| Debug source-map infra (debuggability budget) | `@debug` directive (`grammar/bbnf/bbnf.bbnf:72`); interpreter hooks / compiled-trace debug tower |

---

## 5. Named risks and counterexamples

- **R1 — Build-time blowup (charter budget).** The `cargo expand`+Python route (§1.1) proves
  self-host-via-proc-macro is a build-time hazard; the principled engine must emit *directly*
  (no `cargo expand`), or regen wall-time regresses. *Counterexample:* measure regen wall-time
  for `regen-bbnf`/`regen-regex`/`regen-ir`; if bbnf self-parse regen exceeds the hand-parser's
  zero cost beyond a named budget, that plane stays 7D oracle-only, not promoted.
- **R2 — Debuggability regression (charter budget).** The generated regex parser is
  `#![allow(unused, non_snake_case, clippy::all)]` with synthetic `class_atom_0`/`group_0`
  variants — unsteppable without origin tracking. *Counterexample:* the `@debug`/source-map tower
  must map generated frames → grammar spans before promotion; absent that, the hand parser
  survives as the debug build and the totality claim is demoted. tree-sitter's table parsers are
  the cautionary extreme; the typed-AST form (`RegexParserEnum`) is the mitigation.
- **R3 — The emitter-template-as-grammar is a mirage.** Rust emission carries context (hygiene,
  lifetimes, type inference, borrow structure) a template grammar cannot express; the template
  grammar degenerates into host-fn escapes that ARE the hand code, merely relocated — violating
  "mostly data." *Counterexample:* measure the fraction of Plane-4 template expressed as grammar
  vs escaped to host fns; if < ~50% is data, the maximal claim is refuted and 7C
  (HOST-FUNCTION-LATTICE, totality as a measured percentage with a named floor) is the honest
  form. This is the likeliest failure mode — MLIR/BNFC both bound their totality here.
- **R4 — Metagrammar chicken-and-egg.** Parsing `bbnf.bbnf` needs a bbnf parser. If the hand
  parser is deleted before a *committed stage-0* generated parser is frozen as data, the
  bootstrap cannot restart. *Counterexample:* stage-0 is the anchor (7A substrate); hand-parser
  retirement is gated on stage-0 + fixpoint green.
- **R5 — bbnf.bbnf uses features the generator lacks.** `bbnf.bbnf` depends on `@import` module
  resolution (three modules) and declares `@host`/`@recover`/`@debug` the skinny parser *rejects*
  (`:1323`). Total-self-host requires the generator to *implement* `@import` (and the value-DSL of
  `expressions.bbnf`), per `no-workarounds-self-hosting`. *Counterexample:* absent `@import`
  support, `bbnf.bbnf` cannot self-parse and Plane 1 stalls — so `@import` support is a named
  prerequisite wave, not an assumption.
- **R6 — Consumer-less substrate (constitution viii / Y.13).** Each plane must name a same-wave
  consumer: Plane 1 → the generator; Plane 2 → `bbnf-regex` HIR; Plane 3 → the lowering passes;
  Plane 4 → the meta-emitter. A plane authored without its consumer is forbidden — the dead-
  substrate master pathology.

---

## 6. What a prototype must test to move family 7B

1. **Plane-1 fixpoint + oracle.** Add `regen-bbnf`; regenerate the skinny bbnf parser from
   `grammar/bbnf/bbnf.bbnf`; prove it yields `GrammarIr` **byte-identical to the hand parser** on
   the json/css/sheets/csv corpus (differential oracle) AND `regen∘regen` byte-identical
   (fixpoint). Report generated-LOC and regen wall-time vs the 883-LOC hand parser. Gate the
   `@import`-resolution prerequisite explicitly.
2. **Plane-2 re-home.** Move the parse-that regex bootstrap off `bootstrap-regex.sh`
   (`cargo expand`+Python) onto `cargo xtask regen-regex`; prove the generated parser is
   byte-identical to (or a principled improvement over) the current `generated.rs`, and that the
   post-process script is deleted. Measure build-time delta.
3. **Plane-4 single-template conversion.** Convert ONE emitter blob (the CSS `CSS_GENERATED_RS`
   or JSON `skeleton.rs`) from a Rust string template to a grammar-specified/`quote!`-structured
   template; prove regen bit-for-bit identical; report codegen hand-LOC delta, build-time delta,
   and the **data-vs-host-escape ratio** (the R3 metric).
4. **Plane-3 minimal transposition.** Author an IR grammar covering `ExprKind`; prove one
   grammar-to-grammar map (AST→HIR) matches the hand lowering on the JSON grammar.
5. **The budget verdict.** Aggregate regen wall-time across all self-host planes against a named
   build-time budget; run a debuggability probe (source-mapped panic / breakpoint in a generated
   parser) against a named debuggability budget. The maximal four-plane claim is *earned* only if
   both budgets hold; otherwise the honest disposition is a bounded-totality (7C) fallback with
   the exact plane that failed named.

**The move that decides the family:** whether Plane 4 (emitter templates as data) survives the
R3 data-vs-escape ratio. Planes 1–3 have prior art proving feasibility (regex self-host in-repo,
BNFC, MLIR ODS); Plane 4 is where "mostly data" is won or degraded to "bounded totality."
