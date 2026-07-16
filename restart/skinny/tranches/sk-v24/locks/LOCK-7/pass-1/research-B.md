# LOCK-7 / Family 7B — TOTAL-SELF-HOST (pass 1, research-B)

Thesis: drive constitution clause (i) to its limit — bbnf grammar, regex grammar, IR grammar,
and emitter templates all grammar-specified, the generator collapsing to mostly data, hand
code retreating to kernels and primitives. The family's honesty is that it is *measured
against a build-time and debuggability budget*: totality that inflates compile walls or
blinds the debugger is a defect, not a triumph. This report makes the mechanism concrete,
maps the substrate, and names the counterexamples a prototype must defeat.

## 1. Substrate reality (where the four planes stand today)

The four planes sit at four different depths of self-hosting. Precise state:

**Plane A — the bbnf grammar.** The self-hosted grammar SOURCE already exists:
`grammar/bbnf/bbnf.bbnf` (90 lines, imports `grammar/bbnf/expressions.bbnf` and
`grammar/bbnf/types.bbnf`). It fully describes bbnf: `term`/`factor`/`mapped_factor`/
`binary_factor`/`concatenation`/`alternation`/`rhs`/`closure`/`rule`, every directive
(`@import`/`@recover`/`@pretty`/`@ws`/`@token`/`@debug`/`@host`), and `->` map syntax with
`value_expr` + `type_annotation`. BUT the skinny front-end does NOT consume it: bbnf is parsed
by a **hand-written recursive-descent parser** — `parse_grammar` at
`skinny/crates/grammar/src/lib.rs:29`, `struct Parser<'a>` at `:1259`, the descent tower
`parse_expr`→`parse_alt`→`parse_seq`→`parse_postfix`→`parse_projection`→`parse_atom`→
`parse_literal`/`parse_regex`/`parse_ident` at `:1332-1582`, roughly 900 LOC of the 2142-LOC
crate. Self-hosting of bbnf was ACHIEVED ONCE in the legacy tree: the outer repo carries a
generated parser at `crates/core/src/grammar/generated/bbnf.rs` — **21,557 LOC** produced from
that 90-line grammar. That single number is the family's central tension (§4): the extant
generator inflates ~900 hand LOC of parser into ~21.5k generated LOC, a ~24× blow-up that pays
the B2 build-wall tax. Skinny deliberately kept the hand parser and never re-self-hosted bbnf.

**Plane B — the regex grammar — ALREADY BOOTSTRAPPED.** parse-that's regex parser is
grammar-generated today: `parse-that/rust/bootstrap/regex.bbnf` (72 lines, full L4:
`char_class`/`class_escape`/`quantifier` with bounded `{n,m}`, `group` with inline-flags
`(?simux:…)`, the natural-recursion spine `alternation`→`concat`→`quantified`→`atom`) lowers
through bbnf-lang's `cargo xtask regen` into `parse_that::regex::generated::{RegexParser,
parse_generated}`; the `regex-bootstrap` crate (`parse-that/rust/bootstrap/src/lib.rs:22`) is
now one re-export line after the B2 commit `6142387f` retired the `bbnf_derive` proc-macro.
This is the family's proof-of-existence: a real production regex parser generated from a bbnf
grammar, with a divan bench (`benches/regex_parse.rs`) and a conformance suite (`src/lib.rs:28-61`).
**Drift hazard:** bbnf-lang carries its OWN, DIVERGENT regex grammar at `grammar/misc/regex.bbnf`
(28 lines, demonstrably stale/buggy — `decimal = /d+/` and `hex = /x[0-9a-fA-F]+/` are missing
their backslash escapes, `lookbehind` malformed). Two regex grammars — one canonical (parse-that
bootstrap), one rotting (bbnf-lang misc) — is a constitution clause (ii) isomorphism violation
this family must collapse to one home (LOCK-5's continuum decides the lawful HIR; 7B consumes
that verdict and ensures the *grammar source* is singular).

**Plane C — the IR grammar — DOES NOT EXIST.** There is no `.bbnf` describing `GrammarIr`/
`BackendIr`. `crates/ir/src/lib.rs` (1020 LOC) hand-defines the tower. This is the plane where
7B is purely aspirational and where it interlocks with LOCK-3C (GRAMMAR-HOMOIOUSIA): if the IR
is itself bbnf-specified, transpositions become grammar-to-grammar maps and the generator eats
itself. Highest-risk, highest-reward plane (§4, §6).

**Plane D — emitter templates — MIXED, half-data already.** The generator is `crates/codegen`
(10,201 hand LOC). Part is programmatic Rust construction, but a real fraction is already DATA:
raw-string templates in `codegen/src/runtime_generator.rs` — `JSON_PARSE_ONLY_PARSER_RS`
(`:339`), `JSON_MOD_RS` (`:361`), `JSON_HOST_RS` (`:383`), `CSS_MOD_RS`/`CSS_PARSER_RS`/
`CSS_SINK_RS` (`:387/:401/:505`), plus `r#"…"#` templates through `json_sink_direct/skeleton.rs`,
`general_descent/emit.rs`, `css_scan_direct/{drivers,projection}.rs`. The "generator becomes
mostly data" thesis is a *continuation of an existing gradient*, not a green-field invention —
the seam already leans on string templates; 7B's job is to make them grammar-addressable and
shrink the ~10k of programmatic glue.

## 2. The mechanism made concrete

7B is not "regen everything." It is a **staged, gated totality** with a fixpoint law and a
budget law, executed plane by plane in dependency order.

**(a) The bootstrap engine already exists: xtask regen + the bit-for-bit check IS the fixpoint
substrate.** `skinny/xtask/src/regen.rs:19` `write_targets` → `codegen::emit_runtime_from_request`
→ `emitted.write_to_dir`; `check_target` (`:36`) calls `emitted.check_dir` and FAILS if the
committed tree differs from a fresh emit. That check is exactly the rustc stage-N == stage-(N+1)
fixpoint gate (§5) at the granularity of one generated grammar. The committed generated tree
under `crates/runtime/src/grammars/` is the *stage-0 artifact*; `cargo xtask check-*` is the
*regen∘regen = regen* proof. 7B's mechanism is to *widen the target set* to include bbnf itself,
the unified regex grammar, and — if Plane C proves out — the IR grammar, each with a committed
stage-0 tree and a `check-*` fixpoint gate. No new engine; a wider mouth on the existing one.
Dispatch roster to extend: `skinny/xtask/src/main.rs:39-52` (mirror `SHEETS_TARGET`).

**(b) The self-application closure — the meta-circular step.** Once `grammar/bbnf/bbnf.bbnf`
regenerates a parser P through the generator, feed a corpus of the OTHER `.bbnf` sources
(json, css/l4/*, sheets, csv, regex, and bbnf.bbnf itself) through P and assert P produces the
same `GrammarIr` the hand parser produces. That differential is the promotion gate: P replaces
`parse_grammar` only when it is byte-parity on every grammar the repo ships INCLUDING its own
source. This is the "generator eats itself" closure — the generator's front-end becomes an
output of the generator.

**(c) Emitter templates as grammar-addressed data.** End-state: `runtime_generator.rs`'s const
templates become *entries selected by grammar-shape facts* (backend shape, stop-set, projection
kind), and the programmatic glue in `codegen` shrinks toward a template-selection + hole-fill
kernel — the MLIR/TableGen model (§5): the emitter's decision surface is declarative data; only
the fill kernel is code.

**(d) The budget law is a first-class gate, not a footnote.** Every plane promoted to self-host
carries TWO born-RED gates beyond parity: (1) *build-wall* — `cargo build -p runtime` cold wall
must not regress beyond a named ceiling versus the hand-parser baseline; (2) *generated-LOC* —
`cargo xtask lint-loc` (the existing LOC-budget gate, `main.rs:49`) binds a per-grammar ceiling.
The 21,557-LOC legacy bbnf.rs is the *anti-target*: a self-host that reproduces it fails by
construction.

## 3. Integration seams (path:line where extant code serves)

- **Bootstrap fixpoint gate:** `skinny/xtask/src/regen.rs:19` (`write_targets`), `:36`
  (`check_target` / `check_dir` bit-for-bit). Dispatch roster `skinny/xtask/src/main.rs:39-52`
  — add `regen-bbnf`/`check-bbnf`, `regen-regex`/`check-regex` arms.
- **The hand parser to demote:** `skinny/crates/grammar/src/lib.rs:29` `parse_grammar`, `:1259`
  `struct Parser<'a>`, descent `:1332-1582` — the ~900 LOC self-hosting retires.
- **Self-host grammar sources (ready):** `grammar/bbnf/bbnf.bbnf`, `grammar/bbnf/expressions.bbnf`,
  `grammar/bbnf/types.bbnf`. Regex: canonical `parse-that/rust/bootstrap/regex.bbnf`; stale
  duplicate to reconcile `grammar/misc/regex.bbnf`.
- **Proof-of-existence (legacy self-host):** `crates/core/src/grammar/generated/bbnf.rs`
  (21,557 LOC — the anti-budget witness), `crates/core/src/grammar/generated/mod.rs`.
- **Emitter templates (Plane D data seed):** `skinny/crates/codegen/src/runtime_generator.rs:339,
  361, 383, 387, 401, 505`; also `json_sink_direct/skeleton.rs`, `general_descent/emit.rs`,
  `css_scan_direct/{drivers,projection}.rs`.
- **Regex-bootstrap consumer (already self-hosted):** `parse-that/rust/bootstrap/src/lib.rs:22`,
  conformance `:28-61`, bench `benches/regex_parse.rs`.
- **LOC budget gate:** `cargo xtask lint-loc` (`skinny/xtask/src/main.rs:49`).
- **IR to be grammar-specified (Plane C):** `crates/ir/src/lib.rs` (`GrammarIr`, `BackendIr`,
  `Recognizer`, `BackendShape` at `:361`) — the hand tower LOCK-3C would re-express in bbnf.

## 4. The build-time & debuggability budget — the family's crucible

The single most important prior fact for this family is the B2 pathology, documented at
`restart/skinny/tranches/sk-v21/research/12-dig-b0-b2.md`: the derive-proc-macro self-hosting
route produced a `bbnf-bootstrap` compile wall >600 s, a cold `cargo expand -p bbnf-bootstrap`
exceeding **80 minutes**, and multi-megabyte per-grammar generated files. B2's build-time xtask
transposition *relocated* the cost (proc-macro → committed `.rs` regen) but *did not cure the
size*: the 21,557-LOC bbnf.rs is the relocated-not-cured artifact. TOTAL-SELF-HOST's credibility
rests entirely on defeating this. Two levers make it defeatable rather than fatal:

1. **Rule survival / inlining** — the IR optimizer inlines leaf rules and keeps only the
   recursive spine (`parse-that/rust/bootstrap/regex.bbnf:1-6`). The generated regex parser is
   small BECAUSE the grammar is small and the optimizer collapses leaves; bbnf is larger but the
   same discipline applies — the 24× blow-up is a *generator-quality* defect, not an intrinsic
   cost of self-hosting.
2. **Template-as-data (Plane D)** — moving emitter output from per-node programmatic Rust to
   selected templates + hole-fill shrinks generated volume super-linearly, because shared
   skeletons stop being re-materialized per rule.

Debuggability budget: a generated 21k-LOC parser is un-steppable; the hand parser is legible.
The family's honest floor is that any plane whose self-host makes debugging strictly worse
without a build-wall win is a NAMED negative result, not shipped. Existing debug hook: the
`@debug` directive (`bbnf.bbnf:72`) — the generated parser must surface the SAME grammar spans.

## 5. Prior art (with citations)

- **rustc stage fixpoint.** stage0 (downloaded) builds stage1 builds stage2; stage3 == stage2 is
  the *fixpoint check that self-compilation converges to a stable image* (Rust Compiler
  Development Guide, "What Bootstrapping does"). Exactly 7B's `check-*` law: committed stage-0
  tree, regen∘regen byte-identical. Legitimizes the staged spine 7B rides while pushing totality.
- **tree-sitter grammar DSL → JSON → C.** `grammar.js` (a JS DSL: `seq`/`choice`/`repeat`/`prec`)
  is run by `tree-sitter generate` to emit grammar JSON then C parser tables (Tree-sitter docs,
  "The Grammar DSL"). Lesson for Plane D: the *grammar is data* (JSON) and the emitter is a table
  generator — "generator becomes mostly data" is proven at ecosystem scale. Divergence:
  tree-sitter targets GLR tables, bbnf targets typed recursive-descent + tape; the data-first
  emitter is the transferable idea.
- **MLIR TableGen / ODS.** Op/type/rewrite records declared as data; C++ generated. The archetype
  of "hand code shrinks to kernels, everything else is data" — the target shape for Plane C
  (IR-as-grammar) and Plane D.
- **PEG self-hosting (Ford's PEG / OMeta / Ohm).** PEG metagrammars routinely parse themselves;
  the meta-circular closure (§2b) is standard practice, de-risking the bbnf self-parse step.
- **GHC's `happy`/`alex`.** Generator-produced parsers coexisting with hand-written lexer kernels
  is the mainstream compiler shape — validates 7B's "kernels stay hand-written, front-end is
  generated" split.

## 6. Named risks & counterexamples (what could sink the family)

- **R1 — the generated-size explosion recurs (the 21,557-LOC ghost).** If a self-hosted bbnf
  parser regenerates at legacy scale, the build-wall/LOC gates fail and the bbnf plane cannot
  ship. *Counterexample to defeat:* regenerate `bbnf.bbnf` under the CURRENT skinny generator and
  MEASURE LOC + cold build wall. If not within a named multiple of the ~900-LOC hand parser
  (target ≤ 3–4×, per csv/regex precedent), Plane A demotes to SPEC-ONLY and only regex + Plane D
  proceed.
- **R2 — the IR-grammar (Plane C) is a category error.** `GrammarIr` carries interned arenas,
  `ExprId` graph edges, and Rust-typed payloads a context-free grammar cannot express.
  *Counterexample:* if the IR needs host functions for its own recursion (hybrid-grammar-host
  law), Plane C is "IR schema in bbnf + typed host kernels" — which is 7C's lattice, not 7B's
  totality. 7B must prove the IR IS bbnf-expressible end-to-end or concede Plane C as a named
  boundary to a sibling family / the LOCK-3C verdict.
- **R3 — meta-circular non-termination / two-regex drift.** Self-parsing bbnf while
  `grammar/misc/regex.bbnf` diverges from the canonical parse-that regex grammar means the regex
  leaves in bbnf.bbnf (`identifier`/`literal`/`regex` at `bbnf.bbnf:9-15`) are parsed by a
  DIFFERENT regex semantics than the one being bootstrapped. *Counterexample:* reconcile to ONE
  regex grammar home BEFORE the bbnf self-parse, or the fixpoint gate proves parity against the
  wrong regex.
- **R4 — debuggability regression un-priced.** A generated front-end no one can step through is a
  cost the parity gate does not see. *Counterexample:* a span-fidelity check — generated parser
  must surface the same grammar spans in diagnostics the hand parser does (`@debug`, `bbnf.bbnf:72`).
- **R5 — template-as-data just relocates complexity into an unreadable template DSL.** If the
  emitter templates grow their own untyped mini-language, the god-module pathology reappears as a
  god-template. *Counterexample:* templates must be typed and grammar-addressed (selected by shape
  facts already in `CostFacts`/`BackendShape`), not free-form string soup.

## 7. What a prototype must test to move this family

Four measurements, in dependency order:

1. **Regex-plane reconciliation (cheapest, highest-certainty).** Unify `grammar/misc/regex.bbnf`
   into the canonical `parse-that/rust/bootstrap/regex.bbnf`, wire a `regen-regex`/`check-regex`
   xtask target in skinny, prove the fixpoint gate green. Near-free (regex already bootstrapped
   there) and establishes the singular-source law.
2. **The bbnf self-parse budget probe (make-or-break).** Regenerate a parser from
   `grammar/bbnf/bbnf.bbnf` through the current skinny generator; report generated LOC and cold
   `cargo build` wall. Pass = within ≤3–4× the ~900-LOC hand parser AND no build-wall regression
   beyond a named ceiling. This single number decides whether Plane A is live or SPEC-ONLY.
3. **The meta-circular parity oracle.** Feed all repo `.bbnf` sources (json, css/l4/*, sheets,
   csv, regex, bbnf itself) through the generated bbnf parser and diff `GrammarIr` against the
   hand parser's output. Pass = byte-identical IR on every input including bbnf.bbnf.
4. **Plane D shrink demonstration.** Convert ONE emitter (e.g. `CSS_PARSER_RS` at
   `runtime_generator.rs:401`) from programmatic glue to a grammar-shape-selected template +
   hole-fill; measure codegen LOC delta and generated-output parity (`check-css` bit-for-bit).
   Pass = codegen hand-LOC drops, generated output unchanged.

Plane C (IR-as-grammar) is explicitly NOT a pass-1 prototype target — it is gated behind R2's
adjudication and the LOCK-3C verdict; 7B carries it as a named aspirational boundary, not an
attempt before bbnf and regex planes are proven. The family advances the moment probes 1–3 are
green and probe 4 shows a real shrink; it is falsified the moment probe 2 reproduces the
21,557-LOC blow-up with no build-wall win.
