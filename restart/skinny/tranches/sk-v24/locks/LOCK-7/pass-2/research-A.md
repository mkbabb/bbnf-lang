# LOCK-7 pass-2 — research-A — STAGED-BOOTSTRAP (7A)

The classic two-stage bootstrap made concrete on the skinny substrate: a committed
stage-0 artifact, a fixpoint gate, minimal surface, maximal auditability. This report
re-derives the seed budget on skinny@HEAD, verifies the provenance of the `88s→0.5s`
datum, chooses the fixpoint SUBJECT and proves its non-vacuity, and costs the legacy
gate as rescue-and-port versus re-author with a named born-RED gate.

## 1. Substrate census (skinny@HEAD — the measurement floor XL-A)

Skinny does **not self-host bbnf today.** The skinny xtask dispatch
(`skinny/xtask/src/main.rs:39-52`) exposes exactly four regen targets — `regen-json`,
`regen-sheets`, `regen-css`, `regen-real-typed` — and their `check-*` twins. There is
**no `regen-bbnf` target, no `bbnf.bbnf` regen source wired, no fixpoint gate.** The
grammar source exists as unconsumed data: `grammar/bbnf/bbnf.bbnf` (89 LOC) +
`grammar/bbnf/expressions.bbnf` (37) + `grammar/bbnf/types.bbnf` (7) = **133 LOC** of
bbnf-in-bbnf, plus `grammar/misc/regex.bbnf` (27 LOC) for the regex sub-grammar. This
source has no consumer in skinny — a Y.13 violation the moment 7A proposes to keep it.

The bbnf front-end is a **hand recursive-descent parser**, one file,
`skinny/crates/grammar/src/lib.rs` (2142 LOC). It builds `GrammarIr` directly:
`add_expr(ExprKind::…, SourceSpan)` at `:1351,1371,1393,1400,1423,1439,1456,1463,1485`
(node construction) and `resolve_refs()` at `:33` (cross-rule link, the TreeFold). This
2142-LOC hand parser is the surface a self-host would replace, and the LOC anchor every
seed trade measures against.

The one general, grammar-NEUTRAL emitter is `skinny/crates/codegen/src/general_descent/`
(mod 93, emit 452, view 431, regex_first 230 = ~1206 LOC). It is reached by shape
dispatch: `runtime_generator.rs:98 if !sink_only.has_rule("value")` →
`emit_general_descent`. `bbnf.bbnf` has no `value` rule, so a bbnf regen would route
here. **What it emits is a RECOGNIZER, not a GrammarIr builder** — this is the crux of
7A and is documented in the emitter itself: `general_descent/emit.rs:1` "The
recursive-descent **recognizer** emitter"; the emitted entry `parse_into_tape`
(`emit.rs:105`) writes an **offset tape** (`TapeBuilder`, `tape.push_offset(start,
OffsetFlags(kind))` at `emit.rs:218`) and returns a lazy `{Prefix}Document` view
(`emit.rs:114`), never an `add_expr`/`resolve_refs` GrammarIr. Regex leaves degrade to a
Span delimiter-scan (`mod.rs` docstring; `bbnf-regex` HIR exposes only a coarse 4-variant
classifier, no walkable AST).

## 2. The seed_LOC headline, re-derived — and the "building seed" inflation

Two seeds must be distinguished, because the directive's inflation question turns on it:

- **Recognizer seed (cheap, weak).** Route `bbnf.bbnf` through `!has_rule("value")` →
  general_descent as-is. Output is ~5 files sized like the sheets seed
  (`skinny/crates/runtime/src/grammars/sheets/` = generated.rs 1591 + view.rs 279 +
  parser 25 + mod 9 + config 4 = **1908 gen LOC** for a 39-branch tower). bbnf's ~20
  rules would emit a **comparable ~1.2–1.6k gen LOC recognizer**. This seed *recognizes*
  bbnf and materializes an offset tape + lazy view. It does **not** produce `GrammarIr`,
  so it cannot replace `grammar/src/lib.rs` on the compile path — it is a recognizer, not
  a front-end. Committing it as stage-0 buys auditability of *emission determinism* only.

- **Building seed (the real self-host, expensive).** To replace the hand parser, the
  emitted parser must yield GrammarIr-equivalent structure. On the recognizer emitter
  that means authoring `-> Span`/`-> T` **builder projections** on every structural rule
  of `bbnf.bbnf` (term/factor/mapped_factor/concatenation/alternation/rule/directive)
  such that the tape (or a projection over it) reconstructs `add_expr` node identities +
  `resolve_refs` links. **This is precisely the inflation the directive names:** the
  construction census is host-bound — `add_expr` (append) and `resolve_refs` (cross-rule
  TreeFold) are host semantics, not `->` values (LOCK-1 inherited-attribute verdict is
  the floor). Encoding them as grammar `->` projections re-expresses, construct by
  construct, what the 2142-LOC hand parser does imperatively. The building seed's *source
  delta* is small (add `->` to 133 LOC of grammar) but its *semantic obligation* inflates
  toward the hand-parser's 2142 LOC of build logic, relocated from Rust into `->` +
  host-fn signatures. **The trade is honest only if stated as: recognizer seed is small
  and provable but consumer-less as a front-end; building seed is a front-end but its
  host floor is the LOCK-1/7C verdict, not a codegen line-count.**

Headline to carry (skinny@HEAD, XL-H = skinny master): **grammar source 133 LOC; hand
parser 2142 LOC; recognizer seed ~1.9k gen LOC (sheets proxy); building-seed host floor
UNMEASURED (RED, LOCK-1-bound).**

## 3. The 88s→0.5s datum — VERIFIED a legacy transplant, and mis-scoped as parse cost

`grep 'fixpoint|regen-bbnf'` over `skinny/` returns **nothing** in code. The datum lives
only in the **legacy tree** at `crates/bootstrap/src/lib.rs` (a 28-LOC thin re-export
whose doc-comment records the B6.W0 optimization). The exact provenance:
`cargo xtask regen` "skips the on-disk write when the IR pipeline's emitted bytes match
the existing file's bytes … cold-wall regen drops from ~88 s to ~0.5 s when the regen
produces identical output, which is the steady-state condition." The same comment
states plainly: **"W0 made the build wrapping it cheap, not the parse itself."** The
mechanism (legacy `xtask/src/regen.rs:~455`, "Tranche B6.W0 — content-equality skip") is
a **cargo-fingerprint/mtime-preservation trick**: the 1.6 MB generated `bbnf` source is
`include!`d, so an unconditional write advanced mtime and forced an ~85 s release rebuild
of the `bbnf` crate on every regen — a self-invalidation cycle. Skipping the write when
bytes are unchanged preserves mtime and lets cargo reuse cached rmeta.

**Therefore: the 88s→0.5s is a BUILD-CACHE datum, not a parse-speed or fixpoint datum,
and it lives in the crate that archives at GN-0 (XL-A).** Quoting it as evidence for a
staged bootstrap in skinny would be a double error — it measures cargo's rebuild
avoidance of a monolithic `include!`, and skinny's generated grammars are per-file
directory-modules (`runtime/src/grammars/<g>/*.rs`), a different build shape where the
1.6 MB monolith does not exist. The datum is retired; skinny must re-derive any regen-cost
number on its own substrate if 7A wants one.

## 4. The fixpoint SUBJECT — and proving the byte-determinism non-vacuous

Skinny's existing `check-*` gates (`regen.rs:36 check_target`) assert **regen(source) ==
committed bytes** — a *staleness* check, ONE emitter application, not a fixpoint. A true
fixpoint gate asserts `regen∘regen = regen`. Two candidate SUBJECTS:

**(a) Generated `.rs` bytes.** Cheap: run the emitter twice (legacy did exactly this via
the `--output <tempdir>` flag feeding the `bbnf_bootstrap_reproducibility` CI gate that
"capture[s] two successive regen outputs into a tempdir", `xtask/src/main.rs:56-60`) and
`diff`. **The directive's non-vacuity trap applies here in full force.** The
general_descent emitter's *scaffolding* is a **fixed const emit path**: `render_prelude`
(`emit.rs:82` "The fixed recognizer scaffolding"), the header (`emit.rs:37`), the
`ParseError`/`is_text_byte` boilerplate — these stamp identical bytes **regardless of the
input grammar.** A triple-identical byte diff over an output dominated by fixed
scaffolding certifies the emitter is *deterministic*, which it trivially is (it is a pure
function of the program AST). It certifies **nothing** about the self-host claim. To make
byte-determinism non-vacuous you must (i) show the emitted bytes are a genuine function
of `bbnf.bbnf`'s content — the emitter IS input-dependent for the rule bodies
(`emit.rs:249 render node boundary`, one `parse_<rule>` per rule, first-sets derived from
the program's own `ByteLiteral`s per `mod.rs`), so mutate a rule and assert the bytes
move; and (ii) close the **loop**: parse `bbnf.bbnf` *with the stage-0 generated parser*,
re-emit, and assert byte-identity — not parse it with the hand parser both times. Only
(ii) is a fixpoint; (i)+(single-emit diff) is a determinism check masquerading as one.

**(b) GrammarIr equivalence.** The strong subject: the stage-0 parser, run over
`bbnf.bbnf`, produces a `GrammarIr` that is `Empty ≡ Concat([])`-normalized equal to the
hand parser's `GrammarIr` for the same source (the 7D equivalence relation). This is the
subject that actually certifies self-host — but it is **only reachable with the building
seed (§2b), not the recognizer seed**, because the recognizer produces a tape/Document,
not a `GrammarIr`. **Recommendation: the fixpoint subject is GrammarIr equivalence;
byte-determinism is a supplementary artifact (precept 19), non-vacuous only under the
mutation test + closed loop above.**

## 5. Rescue-and-port vs re-author; the born-RED gate

The legacy carried the whole mechanism: `RegenBbnf` target (legacy `xtask/src/main.rs`,
`regen_simple_runtime.rs:376 "bbnf" => "regen-bbnf"`), a byte-equivalent header for
meaningful diffing (`regen.rs:432`), the retired `scripts/bootstrap-bbnf.sh`
(`regen.rs:548`), and the `bbnf_bootstrap_reproducibility` two-capture gate. **All of it
archives at GN-0.** Cost trade:

- **Rescue-and-port** the legacy `RegenBbnf` + reproducibility gate: the porting surface
  is the target roster (mirror `SHEETS_TARGET` at `main.rs:200`, +~15 LOC for a
  `BBNF_TARGET` + `regen-bbnf`/`check-bbnf` arms), a `--output tempdir` double-capture
  option, and the diff assertion. **But the ported gate certifies the WRONG subject** —
  legacy's gate diffed generated *bytes* of a monolithic `include!` file (§3/§4a), the
  vacuous subject. Porting it gives a green gate that proves emitter determinism, not
  self-host. Rescue-and-port is cheap (~50 LOC) and honestly labeled BANKABLE-but-weak.
- **Re-author** for GrammarIr equivalence (§4b): requires the building seed (§2b) — the
  three parser-augment gaps below AND the `->`-builder projections AND the host floor
  (LOCK-1/7C). This is the real self-host and is UNSEEDED.

**The born-RED gate for the parser-augment.** Before any seed can route through the
emitter, three bounded hand-parser gaps block routing `bbnf.bbnf` (the emitter parses via
`skinny/crates/grammar/src/lib.rs`, which must first accept the meta-grammar's own
syntax). Located at skinny@HEAD:
1. **`?w`→`->` attachment** — `parse_seq` (`grammar/src/lib.rs:1361`, `?w` handled at
   `:1366`) and `parse_postfix` (`:1404`, `?w` at `:1449`) must let a `?w`-suffixed factor
   still attach a `->` projection (`parse_projection:1481`). bbnf.bbnf exercises this at
   `big_comment/comment` (lines 17-18: `?w -> Span`).
2. **`@{}` atom** — `parse_atom` (`:1495`); `@{` is scanned at `:420` but the atom form
   `"@{" , rhs , "}"` (bbnf.bbnf:33) must lower. LOCK-7C names `grammar/src/lib.rs:222` as
   the `@{}` lowering site.
3. **`|params|` closures** — the rule-level `closure = "|" identifier … "|" rhs`
   (bbnf.bbnf:51-52) has no parse path.

Gate: **`check-bbnf-parse` is born-RED** — it asserts the hand parser round-trips
`bbnf.bbnf` (all three constructs) into `GrammarIr` before any emitter routing is
attempted; RED until the three augments land, and it is the precondition for §6's
prototype. This gate is real (it parses the actual meta-grammar), not a byte diff.

## 6. What a prototype must test (the unseeded empirical question)

Author `-> Span`/`-> T` builder projections on `bbnf.bbnf`'s structural rules, drive the
`!has_rule("value")` general_descent branch (**never `json_sink_direct`**), and measure:
**does the emitted parser reparse the bbnf corpus into GrammarIr-equivalent structure —
within the measured `add_expr` + `resolve_refs` host budget — while `regen∘regen` holds
byte-identical, or does it cascade, and at which construct?** Concretely: (1) land the §5
born-RED parser augment; (2) add a `regen-bbnf` target; (3) emit the recognizer seed; (4)
attempt the building-seed projection and record the FIRST construct where the tape cannot
reconstruct a GrammarIr node identity or a `resolve_refs` link (predicted cascade points:
`resolve_refs` cross-rule linkage has no tape analog; `@{}` and `|params|` closures are
host-bound not `->`-expressible); (5) run the §4 mutation test + closed-loop fixpoint. **Do
not seed the verdict from the legacy emitter's existence** — legacy emitted a monolithic
`include!` builder via a retired proc-macro path (`crates/bootstrap` doc), architecturally
unlike skinny's tape-recognizer, so its round-trip tells you nothing about skinny's.

## 7. Prior art (citations)

- **rustc / rustc_parse**: the canonical hand-written front-end kept *alongside* a grammar
  spec (the reference grammar is documentation, not the parser) — the pattern where the
  recognizer and the "self-description" are deliberately NOT unified because build
  semantics (spans, recovery, `resolve_refs`-analogs) resist grammar expression. Validates
  the §2 "building seed inflates toward the hand parser" thesis.
- **GHC bootstrapping / Guile's reduced bootstrap seed (Bootstrappable Builds)**: the
  minimal committed seed + fixpoint discipline; the seed is measured in bytes and audited,
  exactly the §4 subject question. Guile's seed-minimization is the discipline 7A imitates;
  its lesson is that a *recognizer* seed and a *self-compiling* seed are different artifacts
  with different trust.
- **tree-sitter** (`grammar.js` → generated C parser): a grammar-driven **recognizer**
  emitter producing a parse tree (CST), NOT a typed IR — the exact shape of skinny's
  general_descent output. tree-sitter never claims to replace a compiler front-end's
  semantic build; it stops at the CST. Strongest external evidence that the recognizer seed
  is real but consumer-limited (Y.13).
- **Lisp/Scheme meta-circular evaluators & PyPy's RPython bootstrap**: the
  `regen∘regen = regen` fixpoint as certification of self-application; PyPy's "translate the
  translator" is the closed-loop of §4(ii), and its literature is explicit that
  byte-stability of the *first* emission is necessary-not-sufficient — precisely the
  non-vacuity trap.
- **`prettyplease` / `syn`** (in the legacy regen, `regen.rs:422`): the normalizer that
  makes byte-diffing meaningful; any skinny fixpoint gate over `.rs` bytes must route
  through an equivalent canonical unparse or byte-diff noise swamps the signal.

## 8. Named risks / counterexamples

- **R1 — vacuous fixpoint.** A byte-diff over general_descent output is dominated by fixed
  scaffolding (`emit.rs:82 render_prelude`); a green triple-diff certifies determinism,
  not self-host. *Guard:* the §4 mutation test (perturb a rule body, assert bytes move) +
  closed-loop (§4ii) are mandatory before any parity% is quoted.
- **R2 — recognizer seed is consumer-less as a front-end.** It produces a tape/Document,
  not `GrammarIr` (`emit.rs:114`), so it cannot replace `grammar/src/lib.rs`. Committing it
  as "the bbnf self-host" is a Y.13 breach (consumer-less substrate). *Guard:* the seed must
  be labeled recognizer-only, or the building-seed projection (§6) must succeed.
- **R3 — building-seed cascade at `resolve_refs`.** Cross-rule linkage (`:33`) is a
  post-parse TreeFold with no offset-tape analog; `@{}` and `|params|` are host-bound. The
  projection likely cascades here. *Guard:* the prototype records the first cascade construct
  as a named negative result, not a silent fallback.
- **R4 — porting the legacy gate imports the wrong subject.** Legacy's reproducibility gate
  diffed a monolithic `include!` file's bytes; ported verbatim it green-lights the vacuous
  subject. *Guard:* re-author the gate on GrammarIr equivalence, port only the
  `--output tempdir` double-capture mechanism.
- **R5 — the 88s→0.5s number leaks into a skinny claim.** It is a legacy cargo-rebuild
  datum (§3), archives at GN-0, and does not exist on skinny's per-file directory-module
  build shape. *Guard:* any regen-cost figure is re-derived on skinny@HEAD or omitted.

## 9. Integration seams (path:line)

- Route point: `skinny/crates/codegen/src/runtime_generator.rs:98` (`!has_rule("value")`).
- Emitter: `skinny/crates/codegen/src/general_descent/{mod.rs:37 render, emit.rs:105
  parse_into_tape, emit.rs:218 push_offset, emit.rs:249 node boundary}`.
- Hand parser gaps: `skinny/crates/grammar/src/lib.rs:1361 parse_seq`, `:1404
  parse_postfix`, `:1481 parse_projection`, `:1495 parse_atom`, `:420 @{ scan`, `:222`
  (@{} lowering, per 7C); build sites `:1351-1485 add_expr`, `:33 resolve_refs`.
- Regen wiring to mirror: `skinny/xtask/src/main.rs:200 SHEETS_TARGET`, `:42-43
  regen/check-sheets`; `skinny/xtask/src/regen.rs:19 write_targets`, `:36 check_target`.
- Legacy (archives GN-0, XL-A): `crates/bootstrap/src/lib.rs` (88s→0.5s, B6.W0),
  `xtask/src/main.rs:56 bbnf_bootstrap_reproducibility`, `regen.rs:432 byte-equiv header`,
  `regen_simple_runtime.rs:376 regen-bbnf`.
- Consumer-less source awaiting a plan: `grammar/bbnf/{bbnf,expressions,types}.bbnf` (133
  LOC), `grammar/misc/regex.bbnf` (27 LOC).
