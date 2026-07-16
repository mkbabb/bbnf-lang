# LOCK-7 · Family 7B — TOTAL-SELF-HOST (research, pass 1)

## The thesis, made literal

Constitution clause (i) — "everything is grammar-driven, here and there, save primitives and
combinators" — is not an aspiration for 7B; it is the whole architecture. 7B drives every
declarative plane through one grammar substrate: the **bbnf grammar** describes bbnf, the
**regex grammar** describes regex (projecting to the shared HIR), an **IR grammar** describes
the IR node algebra, and **emitter templates** describe target emission. The generator's Rust
shrinks to two irreducible cores: the **kernels** (aarch64 NEON in `bbnf-simd`, x86-purged) and
the **combinator primitives** (parse-that). Everything between — front-end, regex HIR
construction, IR type algebra, structural emission — becomes data the generator interprets. The
charter's two ceilings, **build-time** and **debuggability**, are the falsifiers: totality that
blows either budget is demoted, plane by plane, to an oracle role.

The decisive fact is that 7B is **not greenfield**. A working self-hosting bootstrap already
exists across both repos; 7B's move is to promote it from *recognizer/oracle* to *production*,
and to extend the same mechanism up the tower from front-end into IR and emitter.

## What already exists (the proven substrate)

1. **The bbnf grammar is self-described, with projections.** `grammar/bbnf/bbnf.bbnf` (90 lines)
   specifies the full `.bbnf` surface — `identifier`/`literal`/`regex`/`comment` terminals, the
   `term`→`factor`→`mapped_factor`→`binary_factor`→`concatenation`→`alternation`→`rhs` expression
   tower, grammar-function `closure`s, and every directive (`@import`/`@recover`/`@pretty`/`@ws`/
   `@token`/`@debug`/`@host`). It carries `-> Span` projections and `@import`s two modules:
   `grammar/bbnf/expressions.bbnf` (the `->` value-expression sub-language — precedence-climbing
   `value_or`…`value_atom`, `value_fn_call`, `value_closure`, `type_annotation`) and
   `grammar/bbnf/types.bbnf` (`type_name`). The value plane the type system consumes is itself a
   grammar.

2. **bbnf already self-parses through generated code.** The outer bbnf-lang `crates/bootstrap`
   emits `BbnfBootstrap`, generated from `bbnf.bbnf` by `cargo xtask regen` into
   `crates/core/src/grammar/generated/bbnf.rs`; `BbnfBootstrap::parse` is "the single self-host
   parse entry." The pre-B2 route (a `bbnf_derive` proc-macro `cargo expand`-ed and
   post-processed by `scripts/bootstrap-bbnf.sh`) was **retired**: B2 purged the proc-macro
   crate, and the IR pipeline now runs through `xtask regen` directly.

3. **The regex grammar is bootstrapped and live.** parse-that's `rust/bootstrap/regex.bbnf` (a
   full L4 regex spec — `char_class`, `escape`, `quantifier`, `group`, the
   `alternation`→`concat`→`quantified`→`atom` spine) regenerates `rust/parse_that/src/regex/
   generated.rs` (2293 LOC: `RegexParser`, `RegexParserEnum`, `parse_generated`). The bootstrap
   crate's tests exercise `parse_generated` over a comprehensive pattern corpus — self-hosting
   used as a *maintained truth artifact*, exactly the extant baseline (sk-v23 PT-R3.W3's
   "regex.bbnf as a live gate").

4. **The fixpoint mechanism is already implemented.** B6.W0 made `cargo xtask regen` **skip the
   on-disk write when emitted bytes equal the existing file's bytes** — a byte-identical no-op
   regen that preserves mtime and lets cargo reuse cached rmeta, collapsing cold-wall regen from
   **~88s to ~0.5s** in the steady state. This is `regen∘regen = regen` in force; it is the
   safety harness 7B inherits for the multi-plane case.

5. **The regen host exists.** `skinny/xtask/src/main.rs:39-52` dispatches `regen-*`/`check-*`
   per grammar; `xtask/src/regen.rs:19` `write_targets` → `codegen::emit_runtime_from_request`
   → `emitted.write_to_dir`; `check_target` fails on stale output. Source paths resolve against
   repo-root `grammar/` (the path trap). No `regen-bbnf`/`regen-regex`/`regen-ir` target exists
   in the *skinny* xtask today — those are 7B's additions.

## The three gaps 7B closes (what "TOTAL" means beyond the extant)

**Gap A — the front-end is still hand-written in production.** Skinny's live `.bbnf` front-end
is the 2142-LOC hand recursive-descent parser `skinny/crates/grammar/src/lib.rs:29`
(`parse_grammar` → `GrammarIr`). The self-parsing `BbnfBootstrap` is a recognizer/oracle beside
it, and skinny even *pruned* the bootstrap crate (a prior "husk delete"). 7B **resurrects the
bootstrap crate as the production front-end** — a named supersession of that prune — routing
`parse_grammar` through the generated parser and retiring the hand RD parser to, at most, a
differential oracle. This is the load-bearing move that separates 7B (promote to production)
from an oracle-only posture (keep the hand parser, generated stays a checker).

**Gap B — no IR grammar exists.** The IR is hand-written Rust: `skinny/crates/ir/src/lib.rs:234`
`ExprKind` (Seq/Alt/Repeat/Optional/Literal/Regex/Ref/Annotation/Project/Discard), `:376`
`BackendExpr`, `:416` `Recognizer`, `:466` `DirectBuild*`, `:572` `Shape` — ~1020 LOC of enum
algebra. 7B authors an **`ir.bbnf`** that specifies this node algebra and generates the Rust
enums + serde + a visitor scaffold, the way MLIR's ODS/IRDL and LLVM TableGen generate op
classes, verifiers, and printers from declarative `.td`/dialect definitions. Crucially it
generates **only the data definitions**, not the pass logic: `passes/` (2750 LOC) and `codegen/`
stay hand-written against the *generated* types. "Mostly data" = the type algebra is data; the
transformations are kernels. This aligns with LOCK-3's `GRAMMAR-HOMOIOUSIA` route (a
bbnf-defined IR) without asserting LOCK-3's verdict.

**Gap C — emitter templates are Rust source *strings*.** `codegen` (10201 LOC) emits by
splicing raw strings: `runtime_generator.rs:101` `include_str!("json_templates/generated.rs")`
et al., plus inline `r#"…"#` consts (`:339` `JSON_PARSE_ONLY_PARSER_RS`, `:387` `CSS_MOD_RS`,
`:505` `CSS_SINK_RS`) stitched with `format!`. There is **no `quote!`/`syn`** — it is untyped
string codegen. 7B specifies the *structural* emission (parser skeleton, dispatch, tape
assembly, control flow) as an **emitter-template grammar** the generator interprets, keeping the
SIMD kernels as opaque hand-Rust primitives invoked through a typed call interface. The 80%
structural surface becomes data; the 20% kernel surface stays kernel. This is the boldest and
riskiest plane — see R3.

## Prior art (SOTA, with the lesson each carries)

- **tree-sitter** — `grammar.js` (pure data) → a generated table-driven C parser. The strongest
  existence proof for "the generator becomes mostly data." Lesson *and* warning: its data-driven
  path emits an *interpretive* GLR automaton; 7B's specialized-SIMD thesis forbids paying that
  interpretive tax, so 7B's emitter-grammar must lower to *specialized* code, not a table.
- **MLIR ODS + IRDL / LLVM TableGen** — IR ops, verifiers, printers, and rewrite patterns
  declared in `.td`/IRDL and generated to C++. IRDL is itself a self-hosted MLIR dialect
  (a dialect for defining dialects). Direct precedent for Gap B: generate the IR *definitions*,
  hand-write the passes. The verified boundary — data for structure, code for logic — is exactly
  7B's "mostly data."
- **Racket** (`#lang` + the meta-circular macro expander) — the reference for total self-host
  feasibility and for the primitive boundary: the expander is Racket, the runtime kernel is C.
  7B's kernel/combinator carve-out mirrors this precisely.
- **GHC / happy, rustc's hand parser** — the counter-lesson. GHC's parser is `happy`-generated
  but `happy` is a *separately bootstrapped* tool (two-stage, not in-language totality). rustc
  deliberately keeps a **hand-written** parser for diagnostic quality despite self-hosting the
  language. Both are named evidence for R1 below.
- **egg / egglog** — optimizer-as-data (rules, not code); precedent that decision logic can be
  declarative, relevant if 7B's emitter-grammar reaches into rewrite/dispatch selection.
- **Compiler bootstrapping theory** — Wirth's T-diagrams and Thompson's "Reflections on Trusting
  Trust": the committed stage-0 provenance concern that R5 addresses.

## Named risks and counterexamples

- **R1 — diagnostics regression (the rustc lesson).** A generated front-end may give worse
  errors than the 2142-LOC hand parser, whose size is largely diagnostic machinery.
  *Guard:* before promoting Gap A, measure error-message parity on a malformed-grammar fixture
  corpus; promotion requires diagnostics ≥ hand parser. If unmet, Gap A demotes to oracle and
  totality is claimed only for regex + IR + emitter, with the front-end named as the measured
  exception. `bbnf.bbnf` already carries `-> Span` on every terminal, so span-precise errors are
  *expressible*; the test is whether the generated parser *emits* them as richly.
- **R2 — the build-time budget (already a live hazard).** Cold regen was ~88s before the
  byte-identical-skip dropped it to ~0.5s steady-state. Total self-host multiplies grammar
  sources (bbnf + regex + IR + emitter) through the generator and risks a bootstrap-of-bootstrap
  build explosion. *Guard:* the B6.W0 no-op-skip must generalize to all four planes, and cold
  from-scratch regen time is a hard gate with a stated ceiling (the 88s datum is the reference
  ceiling, not a target).
- **R3 — emitter-as-grammar is a category error.** Emission targets Rust+NEON whose correctness
  needs a real type/borrow checker a template DSL cannot provide. *Counterexample that saves it:*
  the SIMD kernels are exactly the *primitives* 7B carves out; the emitter grammar covers
  control flow, dispatch, and tape structure only, and kernels remain hand-Rust behind a typed
  call interface. If the emitter grammar cannot express a construct, that construct is a
  *primitive*, not a workaround — honoring the standing law "augment BBNF if features are
  missing, never work around." A kernel escape-hatch leaking into control flow is the failure
  signature to gate against.
- **R4 — IR-grammar drift from the hand IR.** If `ir.bbnf` generates the enums, every pass that
  matches on `ExprKind` (passes 2750 LOC, codegen 10201 LOC) must compile against the generated
  types. *Counterexample:* generate only data definitions + serde + visitor scaffold (the ODS
  discipline), never pass logic; a diff-gate asserts the generated type surface is byte-identical
  to a frozen snapshot and that `passes`/`codegen` compile unchanged.
- **R5 — stage-0 provenance (trusting-trust).** A committed generated stage-0 could silently
  diverge from its grammar source. *Counterexample:* 7B borrows the staged-bootstrap safety
  harness as a *sub-mechanism* (it is not exclusive with totality) — a committed stage-0 plus the
  fixpoint gate (`regen∘regen` byte-identical, already implemented) plus a periodic from-seed
  rebuild bound divergence. Totality is the ambition; committed-stage-0 + fixpoint is the harness
  underneath it.
- **R6 — regex.bbnf is untyped.** `grammar/misc/regex.bbnf` and parse-that's `bootstrap/regex.bbnf`
  produce *recognizers* (`parse_generated -> bool`), not HIR. Total self-host needs the regex
  grammar to *project to the HIR* via `->`, consuming LOCK-5's lawful HIR form (constitution ii).
  Until LOCK-5 fixes that form, 7B specifies the projection *shape* against `ExprKind`-isomorphic
  nodes and marks the binding as LOCK-5-dependent — not a workaround, a declared consumer edge.

## Integration seams (path:line)

- Front-end promotion target: `skinny/crates/grammar/src/lib.rs:29` `parse_grammar`; source
  `grammar/bbnf/bbnf.bbnf` (+ `expressions.bbnf`, `types.bbnf`). Existing self-parse entry to
  resurrect into skinny: outer `crates/bootstrap/src/lib.rs` `BbnfBootstrap::parse` →
  `crates/core/src/grammar/generated/bbnf.rs`.
- Regex bootstrap (reference implementation): parse-that `rust/bootstrap/regex.bbnf` →
  `rust/parse_that/src/regex/generated.rs` (`parse_generated`); the retired fixpoint script
  `rust/scripts/bootstrap-regex.sh` documents the pre-xtask route.
- IR to grammar-specify: `skinny/crates/ir/src/lib.rs:234` `ExprKind`, `:376` `BackendExpr`,
  `:416` `Recognizer`, `:425` `SimdMode`, `:466` `DirectBuild*`, `:572` `Shape`.
- Emitter templates to grammar-specify: `skinny/crates/codegen/src/runtime_generator.rs:101`
  (`include_str!` splicing), `:339`/`:387`/`:505` (inline `r#"…"#` consts), and the
  `json_templates/*.rs` string set. No `quote!`/`syn` present — the string codegen surface.
- Regen host + fixpoint: `skinny/xtask/src/main.rs:39-52` dispatch, `xtask/src/regen.rs:19`
  `write_targets`; the B6.W0 byte-identical-skip is the fixpoint gate to generalize.

## What a prototype must test to move the family

1. **Promote-the-front-end.** Add a `regen-bbnf` target; route skinny `parse_grammar` through the
   generated parser. Measure (a) differential parse-parity over the full extant grammar corpus
   (json/css/sheets/csv/regex/bbnf), (b) diagnostic-quality delta on a malformed-grammar fixture
   set, (c) cold build-time delta. **Pass = parity + diagnostics ≥ hand + build within budget;
   else Gap A demotes to oracle.**
2. **IR grammar.** Author `ir.bbnf` for `ExprKind`/`BackendExpr`; generate the enums + serde +
   visitor; **assert the generated type surface is byte-identical to a frozen `ir/src/lib.rs`
   snapshot and that `passes` + `codegen` compile unchanged** against generated types.
3. **Emitter-template DSL.** Express ONE grammar's *structural* emission (the JSON parser
   skeleton, kernels excluded) as an emitter-template grammar the generator interprets; regen
   JSON **bit-for-bit** against the current `json_templates/*.rs`. **Pass = bit-for-bit AND no
   kernel escape-hatch leaks into control flow** (R3 signature absent).
4. **Fixpoint + build-budget gate across all four planes.** Prove `regen∘regen` byte-identical
   for bbnf + regex + IR + emitter, and total cold from-scratch regen within a stated ceiling
   (reference: the ~88s→~0.5s B6.W0 datum). **Pass = fixpoint holds AND cold regen ≤ ceiling.**

The family advances when tests 2–4 pass and test 1 either passes or produces a *measured*
front-end demotion with the exact diagnostic/build numbers — because 7B's honest form is
"totality wherever the build-time and debuggability budgets permit, with every exception named
and measured," not totality asserted.
