# LOCK-7 · Family 7B — TOTAL-SELF-HOST (pass-2 research)

The thesis under test: bbnf grammar, regex grammar, IR grammar, and emitter templates all
grammar-specified, so the generator becomes mostly data and hand code shrinks to kernels and
primitives — measured against build-time and debuggability budgets. This report develops 7B to
its strongest concrete form, answers its two open scope questions with measurement, and returns
the shared empirical question as a RED-unknown with its cascade construct named. Every static
count is over `skinny@HEAD` as the measurement substrate (XL-A).

---

## 1. The substrate, made concrete: the emitter is a RECOGNIZER, not a builder

The one grammar-neutral descent emitter is `crates/codegen/src/general_descent/` (emit.rs 452,
view.rs 431, regex_first.rs 230, mod.rs 93 — 1206 LOC). It walks a `SinkOnlyExpr` tower
(`lower/sink_only.rs:69`, 13 variants) and emits exactly one `parse_<rule>` function per rule
(`emit.rs:260 render_rule`). Two facts fix the whole 7B analysis:

- **It emits recognizers.** Each `parse_<rule>` threads `cursor: &mut usize` + `tape: &mut
  TapeBuilder` and records flat offsets (`emit.rs` `tape.push_offset` / `mark`). Its output
  substrate is the sparse offset+flag tape decoded lazily by the emitted view
  (`view.rs:255 render_view` → `{Prefix}Document`/`{Prefix}Node`/`{Prefix}Kind`). This is
  structurally a *different artifact* from a `GrammarIr` (`ir/src/lib.rs:34`), which is a linked
  arena of `Expr`/`Rule` addressed by `ExprId`/`RuleId` and cross-linked by `resolve_refs`.
- **`bbnf.bbnf` shape-dispatches to this branch.** It carries no `value` rule, so
  `runtime_generator.rs:98 !has_rule("value")` routes it to `emit_general_descent`
  (`:99`, `:147`), never `json_sink_direct` (`:103`). The `->` projections it carries reach only
  the four view categories (`view.rs:20-29`: Float/Bool/Tag/Span) — flat per-node accessors, no
  node construction.

**The self-host loop does not exist in the tree.** The skinny regen host is `skinny/xtask/`
(not `crates/xtask/`, which is empty). Its `main.rs:10` USAGE string enumerates the TOTALITY of
targets and its dispatch (`main.rs:39-48`) confirms them: `regen-json`/`check-json`,
`regen-sheets`/`check-sheets`, `regen-css`/`check-css-l4-declaration-values`,
`regen-real-typed`/`check-real-typed`, `check-conformance`, `lint-loc`, benches. There is **no**
`regen-bbnf`, `regen-regex`, `regen-ir`, or `fixpoint` target (`grep -i
'regen-bbnf|regen-regex|regen-ir|fixpoint|self_host' skinny/xtask/` → NONE). `regen-sheets`
(`SHEETS_TARGET` `main.rs:201`, `grammar_name:"google_sheets"`) is the *only* consumer of the
general emitter, and its check gate (`regen.rs:37 check_target`) proves emitter-determinism, not
self-host. Therefore every "the generator eats itself" claim is SPEC-only until a prototype
drives the `!has_rule("value")` branch on `bbnf.bbnf` *itself* — 7B proposes to build that loop.

---

## 2. Structure-scoping (Q2): the data-vs-escape ratio, and the named kernel-escape signature

**The measurement.** Partition `emit.rs`'s emitted-code production into template-substitution
(grammar-plane data: a code string with grammar-derived holes) versus imperative escape (Rust
operational semantics no `->` projection can carry).

Pure-template arms — each is `writeln!` of a fixed string with grammar-derived holes:
`ByteLiteral` → `match_literal(bytes, cursor, {lit})?` (1 hole);
`CallRule` → `parse_{callee}(bytes, cursor, tape)?` (1 hole);
`RegexProgram` → `match_leaf(…, {first_lit}, {node_kind}, {nullable})?` (3 holes, all facts from
`regex_first`); `Seq` interleaves a fixed `skip_ws` between children (`:300`);
`DirectBuild`/`SpanMark`/`TapeEmit`/`Return`/`ValueProject`/`Entry` are marks or passthrough
(`:376-390`). Plus the entire fixed prelude (`render_header:28` + `render_prelude:82`, ~230 LOC)
is a two-hole template (`{prefix}`, `{stop_array}`). Call this the DATA surface.

Escape arms — machinery over the mutable `&mut usize` cursor that encodes **backtracking and
termination**, confirmed at exact lines:

- **The speculative-probe IIFE**, emitted THREE times (RepeatLoop `emit.rs:321`, Optional `:348`,
  Alt `:415`): `let _r: Result<(),ParseError<'i>> = (|| { … })();` then commit-on-success
  — Optional `:354 if _r.is_ok() { *cursor = _probe; }`, Alt `:421-423 if _r.is_ok() { … _matched
  = true; }`. Inside each probe the body **rebinds the mutable borrow** `let cursor = &mut _probe;`
  (`:450 emit_expr_probe`) so a `?` early-return aborts only the speculation and a failed branch
  leaves the real cursor untouched. This is a genuine borrow-checker escape: it manufactures a
  fresh closure precisely to localize `?` and to swap which `&mut usize` the walk mutates.
- **The repeat progress-guard** (`RepeatLoop:311-343`): `let _before = *cursor;` (`:318`) …
  `if _probe == _before { break; }` (`:329`), plus `Err(_) => break` (`:333`) and `_count`/`min`
  enforcement. A termination invariant over runtime cursor state — nullable repeats (`func_args`,
  closures) infinite-loop without it.

**Ratio.** Of the fixed prelude + per-node emission, the DATA surface is ~80% of lines
(~230 prelude + ~6 node-templates ≈ 236) and the ESCAPE machinery ~20% (RepeatLoop ~28, Alt ~22,
Optional ~10, probe-rebind ~2 ≈ 62). **But the ratio is deceptive and must be reported as such:**
the 80% data is trivial boilerplate (already `include_str!`-shaped), while the 20% escape is
precisely *what makes the artifact a parser*. Restricted to control-flow emission (the
non-boilerplate core), escape is ~90% (62 of ~68 lines). The escape is **irreducible as
grammar-plane data** because backtracking (commit-on-success over a shadow cursor) and
termination (break-on-non-advance) are properties of the generated runtime's mutable state, not
of the grammar's structure.

**The named kernel-escape failure signature** (what a grammar-plane emitter cannot emit without a
primitive): `let _r: Result<(),ParseError<'i>> = (|| { let cursor = &mut _probe; … })();` (the
speculative-borrow IIFE) and `if _probe == _before { break; }` (the progress guard). Any attempt
to express these as a `->` projection fails because the projection value would have to *own and
mutate the cursor borrow* — the type of a `->` target is a value category (Span/scalar, or at
most a constructed node), never a `&mut`-threading control operator. The escape is a *statement
in the emitted program's control flow*, whereas `->` is an *expression in the emitted program's
value flow*; these are disjoint syntactic categories in Rust, and no amount of grammar data
crosses that boundary.

**The load-bearing consequence for 7B.** Total self-host does **not** mean zero Rust; it means the
escape surface collapses onto the constitution's own carve-out (clause i: "save primitives and
combinators"). The measured escape names *exactly three* primitives that must remain
hand-written: `probe/commit` (speculative alt + optional), `many-with-progress-guard` (repeat),
and `mutable-cursor threading`. These are precisely the parse-that combinator core (LOCK-4). So
the strongest honest 7B is: **the generator becomes data over a combinator runtime** — every
emitted `parse_<rule>` body reduces to a composition of `.alt() / .opt() / .many() / .seq()`
primitives, and the emitter's escape *relocates into* those primitives rather than vanishing. The
grammar-plane-as-data claim is achievable only atop that runtime, never over raw `&mut usize`
string emission. This is a research answer, not a route: the ratio (~90% escape in the
control-flow core) refutes the maximal "emitter templates as pure data" reading and reduces 7B to
"emitter templates as data over a named 3-primitive kernel."

---

## 3. Scope axis (Q1): does LOCK-3 adopt grammar-homoiousia, and what consumes `ir.bbnf`?

The IR tower today is Rust data: `GrammarIr {Rule, Expr(ExprKind)}` (`ir/src/lib.rs:34,219,234`,
10 `ExprKind` variants) → `BackendIr {BackendRule, BackendExpr}` (`:352,376`, 13 variants) →
`SinkOnlyProgram/SinkOnlyExpr` (`sink_only.rs:20,69`). The transposition maps are hand-written
Rust functions (`sink_only.rs:126 lower_program`, `:111 lower_expr`). Homoiousia (LOCK-3C) asks
that this IR be *itself grammar-specified* by an `ir.bbnf`.

**The gating dependency to name.** For node values to be `->`-expressible, a projection must
*construct* an IR node — e.g. `term = … -> Expr::Ref(...)`. But `->` today yields only value
categories (`view.rs:20 ProjectionKind = Float|Bool|Tag|Span`); there is no SUM/PRODUCT
projection producing a structured node. Building one is exactly GN-3 Wall 1 (document-shape
projections) + Wall 2 (sum types in the direct type kind), owned by LOCK-1's type plane (SPEC §0
row 08 — HM core 221 LOC, the three walls are the GN-3 spec). So: **grammar-homoiousia is GATED
on the LOCK-1 sum/product `->` projection landing.** Absent it, an `ir.bbnf` can only be a
*concrete-syntax grammar for a serialized IR text*, not a constructor of live IR nodes.

**`ir.bbnf`'s lawful consumer — named, per constitution clause viii (no consumer-less
substrate).** Two candidates, only one lawful today:

- *Vacuous / forbidden:* "the emitter reads `ir.bbnf` directly." This requires code-as-data — the
  §2 escape must already be grammar-expressible, which it is not. Reject.
- *Lawful:* `ir.bbnf` defines the concrete syntax of a **textual IR dump**, and its consumer is a
  **round-trip parity oracle** — parse an emitted `BackendIr` text back through the
  `ir.bbnf`-generated recognizer and assert structural equality against the in-memory `BackendIr`
  (the `ir/src/lib.rs` types already `Serialize/Deserialize`). Same oracle discipline as the
  JSON/CSS/sheets `check_target` parity gates (`regen.rs:37`), needing no new type machinery. It
  gives `ir.bbnf` a real consumer *now* and makes the IR self-describing, without pretending the
  transposition maps are data.

**The joint LOCK-3⊗LOCK-7 IR-plane family (drafted, conditional on LOCK-3 adopting).** If the IR
tower owner takes the homoiousia route: (a) `ir.bbnf` specifies `BackendExpr`'s concrete syntax;
(b) its recognizer, generated through the same `!has_rule("value")` branch, consumes an emitted
IR dump; (c) the transposition `lower_program` (`sink_only.rs:126`) is recast as a *rewrite-rule
table* (grammar-to-grammar map) — LOCK-2 e-graph territory (rules are data, the rewrite engine is
a primitive, the MLIR PDL analogue). The consumer of (a) is the parity oracle (b); the consumer
of (c) is the extraction pass. **What remains if LOCK-3 does NOT adopt:** the IR stays Rust
`enum`s; 7B's totality is bounded at the *grammar and regex* planes only, and the IR/emitter
planes are declared kernel — the honest `ir.bbnf`-less floor. Either way, the transposition
*logic* is never data; only the IR *shape* can be. (This mirrors §2's verdict exactly: shape is
data-expressible, control/transposition is kernel.)

---

## 4. The shared empirical question (RED-unknown): builder projections over the recognizer emitter

Question: authoring `-> Span`/`-> T` builder projections on bbnf's structural rules over the
recognizer emitter — does the emitted parser reparse the corpus into GrammarIr-equivalent
structure (within the `add_expr`+`resolve_refs` host budget) while `regen∘regen` holds
byte-identical, or does it cascade, and at which construct?

**Precondition (born-RED gate).** It cannot even be driven yet: three bounded front-parser gaps
block routing `bbnf.bbnf` through the hand parser at all, upstream of the emitter — confirmed
against the live grammar (`grammar/bbnf/bbnf.bbnf`):

1. `@{}` atom (`bbnf.bbnf:33` `term`'s 5th branch `"@{" , rhs ?w , "}"`): `parse_atom`
   (`grammar/src/lib.rs:1495`) handles only `"`, `/`, `(`, ident, never `@{`.
2. `|params|` closures (`bbnf.bbnf:51` `closure = "|" , identifier , … , "|" ?w , rhs`, reached
   via `rhs = closure | alternation` `:52`): `parse_seq`/`parse_alt` break on `|`
   (`:1361`, `:1341`), so the leading-`|` closure form is unparseable.
3. `?w`→`->` attachment (`bbnf.bbnf:42` `mapped_factor = factor , ( "->" ?w , … ) ?`): in
   `parse_seq` (`:1361`), `?w` is consumed standalone as an `Annotation`, so a following `->` is
   orphaned from its `mapped_factor`.

**Predicted answer (to be MEASURED, not seeded).** The recognizer emitter's output is a flat
offset tape, *categorically distinct* from GrammarIr's linked arena — so a `-> Span` build over it
does **not** round-trip to GrammarIr equivalence; it produces a well-formed but *different*
artifact (a `{Prefix}Document` view). It does not cascade to an error; it silently loses
structure. **The first structure-loss construct is `term`** (`bbnf.bbnf:29-36`): an 8-branch
alternation (`ε`/`epsilon` | ident-call | literal | regex | `@{…}` | `(…)` | `[…]` | `{…}`). The
emitter marks one uniform node kind per rule (`emit.rs:388 mark(tape, _node_start, {node_kind})`),
so all eight branches collapse to a single `term` tag — the variant distinction GrammarIr requires
(`ExprKind::Ref` vs `Literal` vs `Regex` vs the three bracket forms) is gone. GrammarIr
equivalence needs the emitter to emit a *builder* (construct `Expr` nodes, call `add_expr`, then
`resolve_refs`), which is a different, **unseeded** emitter needing the §3 sum/product projection.
`regen∘regen` byte-identity holds trivially for the *recognizer* path (the emitter is a
deterministic function of `SinkOnlyProgram`) but certifies only recognizer-determinism — it is
non-vacuous evidence of self-host only if the emitted parser's *output* is fed back as the
generator's *input*, which the recognizer's tape cannot be. Do not seed the verdict from the
emitter's mere existence: that it round-trips a whitespace-insensitive operator tower (Sheets,
`regen-sheets`) says nothing about reconstructing a recursive, variant-rich arena.

---

## 5. Prior art (with citations)

- **rustc / GHC self-hosting** — the classic staged bootstrap: a prior compiler builds the new
  one, then the new one rebuilds itself and the two outputs must match (Rust's `stage1==stage2`;
  GHC's `./validate`). The lesson: the fixpoint gate is over the *generated compiler's behavior*,
  and the seed stage must be small and committed. bbnf has no such gate today (§1).
- **PEG / `pest` / packrat bootstrap** — `pest`'s `.pest` meta-grammar parses `.pest`; Bryan
  Ford's PEG paper (POPL 2004) bootstraps PEG in PEG. These are *recognizer* self-hosts that then
  hand-build the AST in host code — precisely the recognizer/builder split §1/§4 names. None
  claims the AST-construction logic is data.
- **BNF Converter (Forsberg & Ranta)** — the closest prior art to 7B's *thesis*: from one
  labeled-BNF grammar it generates lexer, parser, AST datatypes, pretty-printer, and a skeleton
  transformer across backends. "The generator becomes mostly data" already shipped — and its known
  limits are exactly 7B's budgets: generated parsers are large/hard-to-debug and context-sensitive
  constructs escape to hand code. Encouragement and cautionary bound.
- **tree-sitter** — `grammar.js` generates a C parser table + a generic tree; the *tree shape* is
  data, but the C `external scanners` (`scanner.c`) are hand-written — the exact "escape collapses
  to a primitive set" pattern of §2. Table parsers are near-unsteppable, arguing for the typed-AST
  generated form over tables (the debuggability budget).
- **MLIR ODS / LLVM TableGen / PDL** — operation *definitions* are declarative TableGen/ODS
  records; the *lowering/rewrite logic* is C++ or PDL(L) rewrite patterns. The homoiousia analogue
  for §3: IR *shape* is data (ODS), IR *transposition* is a rewrite-rule table (PDL), rewrite
  *engine* is a primitive. Validates §3's joint-family draft and its "logic is never data" bound.
- **egg / egglog** — rewrite rules as data over a fixed saturation engine; the model for §3(c)'s
  transposition-as-rule-table (shared with LOCK-2's e-graph home).
- **Racket `#lang` / Rust `macro_rules!`** — macro-by-example shows structural template
  substitution (§2's DATA surface) is declarative, but hygiene/recursion need procedural escapes —
  again the primitive carve-out.
- **Spoofax / SDF3 + Stratego (Visser)** — the SOTA language workbench: syntax, transformation,
  and static semantics declaratively specified; the academic realization of "all planes
  grammar-specified," with an origin-tracking debuggability story directly relevant to 7B's debug
  budget.

---

## 6. Named risks and counterexamples

- **R1 — "80% data" is a mirage of boilerplate.** The favorable ratio (§2) collapses to ~10% once
  the fixed prelude is excluded; the load-bearing ~90% control-flow core is irreducible. *Guard:*
  report the control-flow-only ratio, treat the escape as the true kernel floor.
- **R2 — regen∘regen byte-identity certifies nothing.** A deterministic emitter stamps identical
  bytes for any fixed input; triple-identity of a recognizer's `generated.rs` proves emitter
  determinism, not self-host (a fixed-const emit path stamps identical bytes for any grammar).
  *Guard:* the fixpoint SUBJECT must be the *generated parser's output fed back as generator
  input* (a builder), never the recognizer's bytes.
- **R3 — homoiousia hidden dependency.** Node-value `->` projections presuppose LOCK-1 sum/product
  projections (GN-3 Walls 1–2). Claiming `ir.bbnf` constructs live nodes without that is a latent
  kernel-escape. *Guard:* name the dependency; ship `ir.bbnf` as a serialized-IR concrete grammar
  with a round-trip oracle until the type plane lands.
- **R4 — host-function shim as name-gate in disguise.** If a construct the recognizer cannot
  express routes to a hand host-fn that hard-codes grammar-specific parsing, totality is faked —
  and GN-5's grep-gate (the zero-name-gate CI proof, `!has_rule("value")` was PROVE-13's last
  residue) would trip. *Guard:* a host-fn must carry a declared typed signature (hybrid-grammar-
  host law), not a grammar-name predicate.
- **R5 — the front-parser gaps understate cost.** The three gaps (§4) are bounded, but closing the
  closure/`@{}` gaps means the *emitter* must also gain a construction path for them, not just the
  parser — a builder, unseeded. *Guard:* cost the builder emitter separately from the
  parser-augment; do not assume the parser fix suffices.
- **R6 — the combinator-runtime relocation double-counts.** §2's "escape collapses into LOCK-4
  primitives" is only a win if those primitives already exist and are shared; if 7B must *author*
  probe/commit/many-guard, the escape has moved, not shrunk. *Guard:* the 3-primitive kernel must
  be LOCK-4's existing combinator core, consumed — not a new bbnf-side runtime.

---

## 7. What a prototype must test to move 7B

1. **Escape-collapse proof.** Re-emit one `parse_<rule>` (e.g. `term`) as a composition of
   combinator primitives (`.alt/.opt/.many/.seq`) and show the §2 IIFE + progress-guard vanish
   from the *generated* code into the *primitive library*. Measure: generated-LOC delta and
   whether the escape count (`grep '(|| {{' emit-output` → currently 3) drops to zero in generated
   output while the primitive count is fixed and small (≤3). The concrete test of "generator
   becomes data over a combinator runtime" (R6's falsifier).
2. **Builder-over-recognizer cascade.** Close the three front gaps (§4), author `-> Span` on
   `bbnf.bbnf`'s structural rules, drive the `!has_rule("value")` branch, and diff the emitted
   `{Prefix}Document` node stream against the hand parser's `GrammarIr`. Confirm the predicted
   first-loss at `term`'s 8-branch collapse (§4). The decisive RED-unknown.
3. **`ir.bbnf` round-trip oracle.** Author `ir.bbnf` for `BackendExpr` concrete syntax, generate
   its recognizer, and assert an emitted IR dump reparses to structural equality with the
   in-memory `BackendIr` — a lawful, consumer-bearing IR-plane grammar independent of the
   homoiousia dependency (R3), wired as a new `check-ir` sibling of `check_target` (`regen.rs:37`).
4. **Regex self-host parity.** `grammar/misc/regex.bbnf` exists; generate its recognizer through
   the same branch and diff against `parse-that/.../regex/mod.rs:34 parse_generated` (a PUBLIC
   producer: parse then `host::lower` to `Hir`, NOT a recognizer). Confirm the same
   builder-vs-recognizer wall on the regex plane — the `Hir` is a builder; the recognizer is not.

---

## 8. Integration seams (path:line at `skinny@HEAD`)

| Concern | Seam |
|---|---|
| Recognizer emitter (the §2 blob) | `crates/codegen/src/general_descent/emit.rs:260,300,311,344,400,438` |
| Speculative-borrow IIFE (×3) | `emit.rs:321` (repeat), `:348,354` (optional), `:415,421` (alt); rebind `:450` |
| Progress guard | `emit.rs:318 _before`, `:329 if _probe == _before {{ break; }}`, `:333 Err(_) => break` |
| Template arms (DATA surface) | `emit.rs:357,361,365,376,388`; prelude `:28 render_header`, `:82 render_prelude` |
| Lazy view + `->` projection categories | `general_descent/view.rs:20,35,118,255` |
| Shape-dispatch to general descent | `runtime_generator.rs:98 !has_rule("value")` → `:99,:147 emit_general_descent`; JSON at `:103` |
| IR tower (homoiousia target) | `ir/src/lib.rs:34,219,234,352,376`; `sink_only.rs:69,111,126` |
| Front-parser gaps (§4 precondition) | `grammar/src/lib.rs:1341 parse_alt`, `:1361 parse_seq`, `:1404 parse_postfix`, `:1481 parse_projection`, `:1495 parse_atom` |
| bbnf self-host source | `grammar/bbnf/bbnf.bbnf` (`term:29-36`, `@{}:33`, `closure:51`, `mapped_factor:42`) + `expressions.bbnf`, `types.bbnf` |
| Regen host (NO bbnf/regex/ir/fixpoint target) | `skinny/xtask/src/main.rs:10 USAGE`, `:39-48 dispatch`, `SHEETS_TARGET:201`; gate `regen.rs:37 check_target` |
| Regex plane | `grammar/misc/regex.bbnf`; parse-that `…/regex/mod.rs:34 parse_generated` (READ-ONLY producer, not recognizer) |
| Name-gate CI proof (R4) | GN-5 grep-gate (`docs/tranches/sk-v24/GN-5.md`, W3 born-RED falsification-proven) |

**Bottom line for 7B.** Total self-host is real but its floor is not zero Rust — it is the
combinator primitive set the §2 escape measurement names exactly (probe/commit, many-with-guard,
cursor-threading), which must be LOCK-4's existing core, consumed. The generator becomes data
*over that runtime*; the IR *shape* (never its transposition logic) can be `ir.bbnf` with a
round-trip-oracle consumer, and only if LOCK-1's sum/product `->` lands (else the IR/emitter
planes are declared kernel). The decisive open fact is the builder-over-recognizer cascade at
`term` — RED-unknown, drivable only after the three front-parser gaps close.
