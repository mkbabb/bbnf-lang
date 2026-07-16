# LOCK-7 · Family 7A — STAGED-BOOTSTRAP · research (pass 1)

The two-stage discipline: a committed, minimal, auditable stage-0; a generator that reads its
own grammar to emit stage-1; a fixpoint gate that proves `regen∘regen = regen` byte-for-byte.
The family's thesis is that grammar totality (constitution clause i) is earned not by making
the most planes grammar-driven, nor by the widest oracle, but by the smallest, provably-idempotent
trust root — the least hand-written code you must believe, gated by a mechanical fixpoint that a
wrong stage-0 cannot silently satisfy.

---

## 1 · The mechanism, made concrete

### 1.1 What "two-stage" means here

A parser generator self-hosts when the grammar of its own input language is expressed in that
language and fed through the generator to produce the front-end that reads it. bbnf's own
syntax is already a self-describing grammar triple:

- `grammar/bbnf/bbnf.bbnf` — rules, directives, terms, modifiers, the `->` map syntax, the
  closure/grammar-function form (`closure = "|" , identifier , … , "|" ?w , rhs`), and
  `@import`/`@pretty`/`@ws`/`@token`/`@debug`/`@host`/`@recover` directives.
- `grammar/bbnf/expressions.bbnf` — the `value_expr` DSL that is the right side of `->`
  (precedence-climbing atoms → mul → add → cmp → and → or, plus `value_closure`).
- `grammar/bbnf/types.bbnf` — `type_name` (primitive widths + arbitrary idents).

These three modules ARE the object language. Two-stage bootstrap makes the front-end that
parses them a *product* of feeding them through the generator, not a hand-maintained artifact
that merely happens to agree.

### 1.2 The three parsers in play (the state of the art in-tree)

There are today two independent parsers of bbnf, plus a retired third:

1. **The hand-written production front-end.** `crates/grammar/src/lib.rs:29`
   `pub fn parse_grammar(name, source) -> Result<GrammarIr, GrammarError>` — ~2142 hand LOC.
   This is what `cargo xtask regen-*` actually calls to parse every `.bbnf` (via
   `codegen::emit_from_source`, `codegen/src/lib.rs:101-102`). It is the *de facto* stage-0.
2. **The generated bbnf parser.** `crates/bootstrap/src/lib.rs` is a one-line diagnostic
   re-export `pub use ::bbnf::grammar::generated::BbnfBootstrap;` — a parser emitted from
   bbnf.bbnf THROUGH the generator and committed. It exists but serves as a bench/oracle
   surface, not the production front-end.
3. **The retired proc-macro path.** `crates/derive` (`bbnf_derive`) was purged (commit
   `6142387f`); it formerly expanded a `#[derive(Parser)]` marker by running the whole IR
   pipeline at compile time. Its parse-that twin (`rust/bootstrap`, crate `regex-bootstrap`)
   drove `RegexParser` from `regex.bbnf` via `cargo expand` + `scripts/bootstrap-regex.sh`
   into `parse_that/src/regex/generated.rs`. Both derive hosts are gone; the generated
   parsers now flow through `cargo xtask regen` and are committed source.

The strongest 7A reading: the generator ALREADY emits a bbnf-parses-bbnf artifact, but the
bootstrap is *cosmetic* — the hand parser remains authoritative and never shrinks, and no
fixpoint gate proves the generated parser is a true fixed point of the generator. 7A's work is
to (a) make stage-0 minimal and auditable, (b) promote the generated parser to production, and
(c) install the fixpoint gate that makes self-hosting mechanical rather than asserted.

### 1.3 The staging tower (the concrete construction)

```
stage-0  S0 = minimal committed bbnf parser (hand-written seed, enough to parse bbnf.bbnf)
stage-1  P1 = generator(S0.parse(bbnf.bbnf))     — the emitted front-end
stage-2  P2 = generator(P1.parse(bbnf.bbnf))     — re-emit through the emitted front-end
FIXPOINT GATE:   bytes(P1) ≡ bytes(P2)           — regen∘regen = regen, byte-identical
PROMOTION:       production front-end := P1  (S0 retired to the bootstrap-only path)
```

This is the GCC `make bootstrap` shape: stage-0 seeds, stage-1 is compiled by stage-0, stage-2
by stage-1, and stage-1 ≡ stage-2 bit-for-bit is the correctness proof. The difference from a
compiler is that our "compilation" is grammar→Rust emission and the artifact compared is
generated `.rs` source, so the comparison is textual/byte, cheaper and more auditable than
object-file comparison.

### 1.4 The nested-grammar ordering (bbnf needs regex needs bbnf)

bbnf.bbnf contains regex literals (`identifier = /[_a-zA-Z][_a-zA-Z0-9-]*/`), so parsing
bbnf.bbnf requires a regex-literal sub-parser, and the regex language is itself bbnf-defined
(`parse-that/rust/bootstrap/regex.bbnf` — full L4: classes, escapes, quantifiers, groups,
the recursive alternation→concat→quantified→atom spine). The bootstrap is therefore a
*mutual* two-grammar fixpoint with a forced order. The order that terminates: stage-0 treats a
regex literal as an opaque `Span` (bbnf.bbnf line 15: `regex = ( "/" , /(\\.|[^\/])+/ , "/" )
-> Span`), so S0 needs only span-lexing, not a regex engine, to bootstrap bbnf. The regex
grammar is then emitted as an ordinary grammar target once the bbnf front-end exists. This
breaks the chicken-egg: regex is a *leaf span* at bootstrap time, promoted to a full parser
afterward — a clean DAG, not a cycle.

### 1.5 The fixpoint gate, mechanized

`crates/xtask/src/regen.rs:37` `check_target` today re-emits from source and calls
`emitted.check_dir(output_dir)` — it asserts `committed ≡ emit(source)` (single application:
staleness). This is the stage-0 *pin*, not the fixpoint. 7A extends it two ways:

1. **Second application (the fixpoint).** A new `bootstrap-fixpoint` arm: emit P1 from
   bbnf.bbnf through S0; parse bbnf.bbnf through P1; emit P2; assert `bytes(P1) ≡ bytes(P2)`.
   One-step convergence is the gate; if it takes two steps, the generator is non-idempotent
   and that is a named defect (§6, R2).
2. **Input digest as a committed lock.** `regen.rs:153-167` `validate_inputs` already
   blake3-hashes every source + metadata input (`hash_input`, incremental over `rel`+bytes)
   and *prints* the digest (`regen.rs:165`). Today it is provenance theater — nothing asserts
   it. 7A commits the digest beside the generated dir and asserts it in `check_target`, so a
   stage-0 grammar edit without regen trips a digest mismatch (a cheap early gate) BEFORE the
   full byte-diff, and the stage-0 trust root is a named, versioned quantity.

The determinism substrate is partly in place: `regen.rs` uses `BTreeSet` for target validation
(`:88-104`); the generator must emit under total orders (BTreeMap, sorted iteration) so
`regen∘regen` is deterministic — the changed-flag-convergence and clean-regen laws bind here.

### 1.6 Minimal stage-0 (the auditability lever)

The 2142-LOC hand parser is far larger than a bootstrap seed needs. bbnf.bbnf is a small
grammar (≈35 rules across the triple). The 7A minimal seed S0 parses only the bbnf
metalanguage — rules, terms, modifiers, `->`, closures, `@import` — enough to read the triple,
with regex literals as opaque spans. Everything the full production parser does beyond that
(rich diagnostics, recovery, the `value_expr` precedence tower's semantic checks) belongs to
P1, the emitted parser, not S0. Target: S0 ≤ ~400–600 LOC (the "least code you must trust"),
with the full front-end regenerated as P1. This is the family's distinguishing claim —
minimal bootstrap surface, maximal auditability — and it is measurable (§7 P2).

---

## 2 · The substrate (what already exists to build on)

| Organ | Path | Role in 7A |
|-------|------|-----------|
| Hand front-end | `crates/grammar/src/lib.rs:29` `parse_grammar` | the *de facto* stage-0; shrink to minimal S0 + promote P1 |
| Generated bbnf parser | `crates/bootstrap/src/lib.rs` (`BbnfBootstrap`) | today's stage-1 oracle; promotion target |
| Generator entry | `codegen/src/lib.rs:101` `emit_from_source` | the emission the bootstrap drives |
| Regen mechanics | `xtask/src/regen.rs:20` `write_targets`, `:37` `check_target` | staleness pin → extend to fixpoint + digest gate |
| Input digest | `xtask/src/regen.rs:153-167` `validate_inputs`/`hash_input` (blake3) | printed-only → commit + assert (stage-0 lock) |
| Dispatch | `xtask/src/main.rs:39-52` (`regen-json`/`check-json`/…) | add `regen-bbnf`/`check-bbnf`/`bootstrap-fixpoint` arms |
| Target descriptor | `xtask/src/regen.rs:5-18` `RuntimeTarget` | a `BBNF_TARGET` const declaring the self-host target |
| Workspace metadata | `skinny/Cargo.toml` `[workspace.metadata.bbnf]` (only `json` declared) | declare the bbnf-self + regex targets |
| Regex bootstrap (twin) | `parse-that/rust/bootstrap` (`regex.bbnf`, `regex-bootstrap`), `parse_that::regex::generated` | the dependent regex target; retired proc-macro → xtask flow |
| bbnf object language | `grammar/bbnf/{bbnf,expressions,types}.bbnf` | the three modules S0 must parse and the generator must self-emit |

The regex bootstrap crate's own comment states the model verbatim: "Mirrors the bbnf-lang
`crates/bootstrap` crate, whose lib.rs `pub use ::bbnf::grammar::generated::BbnfBootstrap` is
the analogous diagnostic re-export of the workspace's xtask-emitted parser." The two repos
already share the shape; 7A formalizes it into a gated tower on both sides.

---

## 3 · Prior art (with citations)

- **GCC three-stage bootstrap** — `make bootstrap` builds stage1 with the host compiler,
  stage2 with stage1, stage3 with stage2, then **compares stage2 and stage3 object files
  bit-for-bit** (`compare` target). The canonical `regen∘regen = regen` fixpoint and the direct
  archetype for 7A's gate; a mismatch means non-determinism or a miscompile. 7A's advantage: it
  compares generated *source*, not objects, so the diff is human-auditable.
- **rustc staged bootstrap** — stage0 (a downloaded beta), stage1 = stage0-built, stage2 =
  stage1-built; `x.py build --stage 2`; `--enable-full-bootstrap` adds the stage2==stage3
  identity check. Establishes committed/pinned stage-0 as normal auditable practice and the
  reproducibility discipline (deterministic codegen) the fixpoint gate depends on.
- **GHC** — stage0/1/2 with a `validate` triple-build; the compiler compiles itself; the
  "stage restriction" discipline (what stage-N may use) parallels 7A's rule that S0 uses only
  the metalanguage subset.
- **ANTLR self-hosting** — ANTLR's own grammar is written in ANTLR (`ANTLRv4Parser.g4`,
  `ANTLRv4Lexer.g4`); the tool parses its own input language and ships the *generated* parser,
  not a hand one — the model for 7A's promotion step. Contrast: **tree-sitter** does NOT
  self-host — its grammar DSL is `grammar.js` (JavaScript), parsed by a hand-written generator;
  bbnf's self-describing `.bbnf` is the stronger position.
- **Reflections on Trusting Trust** (Thompson, 1984) — the committed stage-0 is a trust root; a
  self-perpetuating trojan in the seed survives all regeneration. **Diverse Double-Compiling**
  (D. A. Wheeler, 2009) is the formal defense: build the self-hoster with a *second,
  independent* implementation and compare outputs; agreement refutes a stage-0 trojan. 7A's
  "maximal auditability" cashes out as (i) a minimal S0 a human can read, (ii) the parse-that
  regex parser as an independent second emission path for a DDC-style cross-check.
- **TeX/METAFONT trip/trap torture tests** (Knuth) and the **reproducible-builds** movement
  (`SOURCE_DATE_EPOCH`, deterministic output) — generated artifacts must be bit-reproducible for
  a byte-identical gate to be meaningful; directly constrains the generator's map-ordering and
  address-stability.
- **Regex determinization prior art** (`regex-automata`, RE2) is out of 7A's scope (LOCK-5); 7A
  consumes the regex grammar only as a bootstrap *target*, not an engine.

---

## 4 · Integration seams (path:line where extant code serves)

- **Fixpoint gate**: `xtask/src/regen.rs:37` `check_target` (single-application byte compare via
  `emitted.check_dir`) → add a second-application `bootstrap_fixpoint(root)` beside it, reusing
  `runtime_request` (`:51`) and `emit_runtime_from_request`.
- **Digest lock**: `xtask/src/regen.rs:155-165` `validate_inputs` — return + persist the blake3
  digest instead of only printing; `check_target` asserts committed digest == recomputed.
- **Self-host target**: a `BBNF_TARGET: regen::RuntimeTarget` const mirroring `JSON_TARGET`
  (`xtask/src/main.rs:177`) / `SHEETS_TARGET` (`:200`), `source_inputs =
  ["grammar/bbnf/bbnf.bbnf", "…/expressions.bbnf", "…/types.bbnf"]`, `output_dir` the
  `BbnfBootstrap` home, `entry_rule = "grammar"`.
- **Dispatch**: `xtask/src/main.rs:39-52` — `regen-bbnf` / `check-bbnf` / `bootstrap-fixpoint`
  arms next to the existing `regen-json`/`check-json`.
- **Generator drive**: `codegen/src/lib.rs:101` `emit_from_source(name, source)` is already the
  correct entry (not the fail-closed `emit(backend)` at `:114`); the bootstrap calls it.
- **Promotion**: `crates/grammar/src/lib.rs:29` `parse_grammar` is the production seam whose
  body becomes the emitted parser once P1 passes the differential oracle (§7 P2/P4).
- **Regex dependent target**: `parse-that/rust/bootstrap/regex.bbnf` +
  `parse_that::regex::generated`; the `regex-bootstrap` crate's re-export is the parity/bench
  surface; the fixpoint gate applies symmetrically there.
- **Metadata**: `skinny/Cargo.toml` `[workspace.metadata.bbnf].grammars` (only `json`) — add the
  bbnf-self entry so `root_grammar_path` (`regen.rs:139`) resolves it.

---

## 5 · Falsification harness (the family's own gate can fail)

Following the discipline of proving a gate can fail before trusting it to pass:
`xtask/tests/bootstrap_fixpoint_falsification.rs` plants (i) a deliberately non-deterministic
emission (unordered map in a throwaway generator source) and asserts `bootstrap-fixpoint`
reports P1≠P2; (ii) a stage-0 edit without regen and asserts the digest lock trips; (iii)
confirms the real tree passes clean. A fixpoint gate that cannot detect a non-fixed point is a
defect.

---

## 6 · Named risks and counterexamples

- **R1 — Cosmetic self-hosting (the two-parser trap).** If the generated parser stays a
  diagnostic re-export and `parse_grammar` remains authoritative forever, "self-hosting" is a
  slogan. *Guard*: promotion is a gate, not a hope — P1 must pass a differential oracle over the
  entire `grammar/` corpus (json/css-l4×16/sheets/misc×7/bbnf-triple) parsing byte-identical
  `GrammarIr` to `parse_grammar`, then `parse_grammar`'s body is replaced by P1. If P1 cannot
  reach parity, the exact divergent rule is a named BBNF-augmentation task (self-hosting uses
  language features; augment, never shim).
- **R2 — Non-idempotent emission (the fixpoint flaps).** If generator output depends on HashMap
  iteration order, address-of, or timestamps, `bytes(P1) ≠ bytes(P2)` nondeterministically and
  the gate is untrustworthy. *Guard*: audit every ordered structure in codegen for total order
  (BTreeMap/sorted); reproducible-builds discipline. `BTreeSet` already appears in the regen
  path — extend the invariant, gate it with a 3×-run identity check.
- **R3 — Feature incompleteness (the generator can't self-emit).** bbnf.bbnf uses `@import`,
  closures/grammar-functions (`call_arg`, the `closure` rule), `->` with the full `value_expr`
  DSL, the `<<`/`>>`/`-` binary operators, and `?w`. If the generator cannot emit a parser that
  BUILDS `GrammarIr` for these (recognizing ≠ building), the bbnf self-host is partial. *Guard*:
  enumerate the feature set (§7 P4) and prove each survives emission; any gap is an augment-BBNF
  task with a named floor, not a deferral.
- **R4 — Trusting-trust in the seed.** A minimal S0 is auditable but still a trust root; a
  compromised or subtly-wrong S0 propagates into every P1. *Guard*: the DDC cross-check — the
  parse-that regex bootstrap is an independent second emission of the regex grammar; agreement
  between the two paths (and a human-readable ≤600-LOC S0) bounds the trust.
- **R5 — The nested regex ordering deadlocks.** If S0 tries to fully parse regex literals to
  bootstrap bbnf, it needs the regex engine, which needs the bbnf front-end — a cycle. *Guard*:
  S0 treats regex literals as opaque spans (bbnf.bbnf declares `regex … -> Span`); regex becomes
  a dependent target, not a co-equal (§1.4). This must be *verified* — a prototype that emits S0
  and confirms it never invokes regex-structural parsing.
- **R6 — Stage-0 bloat defeats the family's thesis.** If the "minimal" seed drifts back toward
  2142 LOC, 7A collapses into "keep the hand parser." *Guard*: a LOC budget on S0 (`xtask
  lint-loc` already gates generated-LOC; add an S0-hand-LOC ceiling), born-RED until S0 ≤ target
  with P1 at parity.

---

## 7 · What a prototype must test to move this family

- **P1 — The fixpoint gate converges in one step and is deterministic.** Implement
  `bootstrap-fixpoint`: emit P1 from the bbnf triple through the current generator, re-parse and
  emit P2, assert `bytes(P1) ≡ bytes(P2)`. Run the whole emission 3× and assert identical output.
  *Moves the family* by proving/refuting `regen∘regen = regen` at HEAD — the gate is the spine;
  if it does not converge, R2 is the blocking defect.
- **P2 — Minimal stage-0 exists and P1 replaces it.** Extract the smallest S0 (target ≤600 LOC)
  that parses the bbnf triple with regex-as-span, emit P1 from it, and run the differential
  oracle: every `.bbnf` in `grammar/` parses to byte-identical `GrammarIr` under `parse_grammar`
  vs P1. *Moves the family* by measuring the actual trust-root size and proving promotion is
  sound — the auditability claim becomes a number.
- **P3 — The digest lock and falsification.** Commit the blake3 input digest + the generated
  digest; assert both in `check-bbnf`; plant a stage-0 edit-without-regen and a non-deterministic
  emission and confirm the gates trip (§5). *Moves the family* by proving the stage-0 pin can
  fail — a gate unable to fail is worthless.
- **P4 — Feature-completeness census.** Enumerate the bbnf.bbnf construct set (@import,
  closures/GrammarCall, `->`/`value_expr`, `<<`/`>>`/`-`, `?w`, `@host`) and, for each, confirm
  the generated P1 BUILDS the correct `GrammarIr` node. *Moves the family* by converting
  "self-hosting depth" from a vibe into a checklist with a named augment-BBNF task per gap (R3).
- **P5 — DDC cross-check (auditability, stretch).** Emit the regex grammar through both the bbnf
  front-end and the parse-that path; assert the two `parse_that::regex::generated`-shaped outputs
  agree. *Moves the family* by giving "maximal auditability" a concrete trusting-trust defense.

The convergence question for 7A is narrow and answerable: does the current generator emit a
byte-identical fixed point of its own grammar (P1), and is the seed small enough (S0) that the
self-hosting is auditable rather than cosmetic? P1 and P2 answer it; everything else hardens it.
