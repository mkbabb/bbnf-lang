# LOCK-7 family 7D — META-CIRCULAR-ORACLE (research, pass 1)

Self-hosting recast as **verification first, production later**. The bbnf-defined bbnf grammar
and the bbnf-defined regex grammar are compiled through the one generator into parsers that do
not immediately replace anything — they run *beside* the incumbent hand parsers as differential
**oracles**. Two independent implementations of the same language must agree, node-for-node,
over a live corpus; disagreement is a bug in one of them, surfaced mechanically. Promotion of a
generated parser to the production seat is gated, subsystem by subsystem, on **measured parity**
(AST-equivalence over the corpus, adjudicated against a hand-audited golden anchor) plus
**measured performance** (cyc/B non-regression, or the named perf floor where the parse is hot).
The hand parser is never dead weight: until its subsystem is promoted it is the epistemic
anchor; after promotion it survives as the CI-only differential reference. This is the family's
distinctive center — it turns constitution clause (i) from a big-bang cutover into a measured
trajectory, and it satisfies clause (viii) from day one, because the generated parser has a
consumer (the oracle) the instant it is emitted.

## 1. Why this family, not staged-bootstrap or total-self-host

A self-hosted parser that parses its own grammar and agrees with itself proves *nothing* — the
trusting-trust circularity (Thompson 1984, *Reflections on Trusting Trust*). The fixpoint gate
of the classic staged bootstrap (regen∘regen byte-identical) certifies that the *generator is
deterministic*, not that either parser is *correct*. 7D breaks the circle by demanding agreement
with a **second, independently authored implementation** — the 2142-LOC hand recursive-descent
parser — and with **externally trusted references** (serde/lightningcss/cssparser already anchor
the json/css corpora). Correctness is then a measured property of *disagreement rate against an
independent oracle*, not an asserted property of self-application. This is exactly the epistemics
of compiler differential testing (Csmith; Yang–Chen–Eide–Regehr, PLDI 2011) and of translation
validation (Pnueli–Siegel–Singerman 1998; Necula, PLDI 2000): do not prove the translator once
and trust it forever — *validate each run's output against an oracle, and promote only what
survives*.

## 2. The mechanism made concrete

**2.1 The two implementations per language.**
- bbnf, incumbent (oracle-of-record initially): `skinny/crates/grammar/src/lib.rs:29`
  `parse_grammar(name, source) -> Result<GrammarIr, GrammarError>` — hand recursive descent,
  2142 LOC.
- bbnf, challenger (generated): the self-hosted parser emitted from `grammar/bbnf/bbnf.bbnf`
  (+ `grammar/bbnf/expressions.bbnf`, `grammar/bbnf/types.bbnf` via `@import`) through the one
  emit spine `codegen/src/lib.rs:101` `emit_from_source`. The *type* to resurrect is
  `BbnfBootstrap::parse` (legacy home `crates/bootstrap/src/lib.rs:28`,
  `pub use ::bbnf::grammar::generated::BbnfBootstrap`; entry drivable per
  `crates/bootstrap/src/bin/debug_parse.rs:22`).
- regex, incumbent: parse-that's hand regex parser (`~/Programming/parse-that/rust/regex/src/hir`)
  and its committed self-host `parse_that::regex::generated` re-exported by the `regex-bootstrap`
  crate.
- regex, challenger (generated): emitted from `grammar/misc/regex.bbnf` through the same spine.

**2.2 The differential.** For each corpus document *d*: run both implementations, normalize each
result to a shared **NormalAst** (§2.3), and assert equality. This is the `css_scan_differential`
pattern (`skinny/crates/runtime/tests/css_scan_differential.rs`) lifted from *scan decisions* to
*parse trees*: that harness already sweeps every start position over the real corpora and asserts
a production kernel's `(byte, pos)`/`None`/error-offset sequence is bit-identical to a clean-room
scalar reference. 7D is the same shape with the hand parser as the clean-room reference and the
AST as the compared artifact. The external-reference variant already exists too:
`xtask/src/main.rs:62` `check_conformance` runs `runtime::generated_json::parse_bytes` against
`serde_json` over valid+invalid fixtures, canonicalizing floats to bits (`check_float_bits`) — the
exact "canonicalize, then compare, valid-accept/invalid-reject" template, against an external
oracle rather than the hand parser.

**2.3 The NormalAst (the crux).** The two implementations produce different in-memory types
(hand → `GrammarIr`; generated → tape/typed nodes). Comparison needs a canonical normal form.
Choose the **least-normalizing** form that both can project to: a structural S-expression that
preserves rule identity, operator nesting/precedence, modifiers (`?w`/`?`/`*`/`+`), `->`
projection expressions, and — critically — **source spans** (`start,end` byte offsets). Over-
canonicalization is a named risk (§5): dropping spans would mask a wrong-span or wrong-precedence
bug, precisely the class differential testing exists to catch. tree-sitter's `corpus/` tests
(parse → S-expr → diff golden) are the precedent for the S-expr form; `check_float_bits` is the
in-tree precedent for "canonicalize only where representation is incidental, compare exactly
everywhere else." Normalizing *both* sides to `GrammarIr` is the alternative, but that couples the
oracle to one side's type and is rejected — the S-expr is implementation-neutral.

**2.4 The golden anchor (breaks "differ, but who is right?").** A differential tells you the two
disagree, not which is correct — and a generated parser that faithfully reproduces a *hand-parser
bug* would pass a pure hand-vs-generated differential. Anchor truth with a small, hand-audited
golden set: `crates/test-fixtures/corpus/bbnf/golden/*.sexp` (and `.../regex/golden/`), each a
NormalAst blessed by human audit against the SPEC. Adjudication rule: every corpus divergence is
resolved against the golden set or, for json/css, against the external reference already trusted
(`check_conformance`'s serde, the css corpora's cssparser 9-field parity cited at
`css_scan_differential.rs`). No divergence is ever resolved by majority vote of two parsers.

**2.5 The corpus is the grammar tree itself.** The natural, zero-fabrication bbnf differential
corpus is `grammar/**/*.bbnf` — `bbnf.bbnf` (self-description), all 16 `css/l4/*.bbnf`,
`json/json.bbnf`, `google-sheets/google-sheets.bbnf`, `bnf/bnf.bbnf`, `ebnf/ebnf.bbnf`, and
`misc/{csv,math,emoji,g4,regex,json-commented}.bbnf`. The regex differential corpus is *every
regex literal extracted from every `.bbnf`* in that tree (mechanically harvestable — regex leaves
are `/…/` spans), plus a crafted adversarial suite (backrefs, lookaround, nested classes per
`grammar/misc/regex.bbnf`). Self-description is included but is not the *proof* — it is one row.

**2.6 The promotion gate (subsystem by subsystem).** Partition each language into promotable
subsystems and gate each independently:
- bbnf: (1) terminal/lexical layer (identifier, literal, regex, comment), (2) expression/factor
  layer (term…alternation, modifiers, `->` mapped_factor), (3) directive layer (`@import`,
  `@pretty`, `@ws`, `@host`, …), (4) whole-grammar assembly, (5) closures/grammar-functions.
- regex: (a) char-class/range, (b) quantifier/group, (c) anchors/backrefs/lookaround, (d) flags.

A subsystem is **promoted** — its generated parser becomes the production path for that construct,
the hand branch retires to CI-differential — only when: **parity** = zero unadjudicated
divergences over the whole corpus for that subsystem, sustained two consecutive regen cycles; and
**perf** = generated cyc/B ≤ hand cyc/B (grammar-source parse is compile-time, so this is a
non-regression floor, and the *win* is deleting hand LOC) — except the regex sub-language, whose
leaves are hot at runtime, where the floor is PROVE-10's certified `Dfa::find` numbers (8.8–29.5×
on prefilterable patterns, SPEC §0 row 10), not mere non-regression. This is dav1d/checkasm
generalized from kernels to parsers: `skinny/crates/bbnf-simd/tests/checkasm_parity.rs` already
runs scalar-reference vs NEON-candidate, `memcmp`s halves, sweeps misalignments, and has a
deliberate-bug injection (`BBNF_SIMD_INJECT_BUG=1`) proving the gate *can* fail — the strict/loose
promotion switch (`BBNF_SIMD_STRICT`) is the exact "record divergences until promoted, then assert
strictly" mechanism 7D needs, verbatim.

## 3. The substrate (what gets built)

- **Oracle home — extend, do not add a subsystem.** GN-5.W1 already *births* a `crates/bbnf-oracle`
  directory-module of per-grammar submodules (GN-5.md §4). 7D adds a `self_host/` sibling submodule
  there: `self_host/bbnf.rs` (generated-vs-`parse_grammar` differential over `grammar/**`),
  `self_host/regex.rs` (generated-vs-parse-that differential over harvested literals),
  `self_host/normal_ast.rs` (the NormalAst S-expr + both projections). System cohesion (memory:
  `system-cohesion`); no orthogonal harness.
- **The bootstrap re-export crate, resurrected as oracle citizen.** Mirror the extant pattern:
  parse-that's `rust/bootstrap` (`regex-bootstrap`) is a thin re-export of
  `parse_that::regex::generated`; bbnf's `crates/bootstrap` was the analogous
  `pub use ::bbnf::grammar::generated::BbnfBootstrap`. Reinstate the bbnf side in the greenfield as
  a thin re-export of the xtask-emitted self-host parser — its consumer is the oracle, not
  production, until promotion.
- **xtask command.** `check-self-host` (and `regen-self-host`) alongside `check-conformance` at
  `xtask/src/main.rs:48`, driven by the same `regen.rs:19` `write_targets`/`check_target`
  mechanics. `check-self-host` runs the differential and fails on any unadjudicated divergence;
  its *artifact* is the NormalAst diff (precept 19: a hard gate needs a runtime artifact, not a
  grep).
- **The golden anchor fixtures** under `crates/test-fixtures/corpus/{bbnf,regex}/golden/`.
- **The falsification proof.** A planted-divergence test (mirror `checkasm_parity`'s
  `INJECT_BUG`): mutate one NormalAst projection and assert `check-self-host` trips — the gate is
  proven able to fail before it is trusted to pass.

## 4. Prior art (citations)

- **rustc bootstrap** (stage0→stage1→stage2, stage1==stage2 build-and-compare): the *fixpoint*
  bootstrap = family 7A. 7D deliberately diverges — a fixpoint certifies determinism, not
  correctness; 7D adds the independent-oracle differential rustc's bootstrap does not have.
- **Csmith / differential compiler testing** (Yang, Chen, Eide, Regehr, PLDI 2011): disagreement
  across independent implementations finds bugs; the known limitation (differ ≠ who's-right) is
  answered here by the golden anchor + trusted external references (§2.4).
- **Translation validation** (Pnueli–Siegel–Singerman 1998; Necula, "Translation Validation for
  an Optimizing Compiler," PLDI 2000): validate each run's *output* rather than prove the
  translator — the direct model for "promote on measured per-instance parity, subsystem by
  subsystem."
- **dav1d / FFmpeg checkasm**: scalar-reference-vs-candidate bit-identical parity with injected-bug
  self-test — the in-tree template (`checkasm_parity.rs`) 7D lifts from kernels to parsers.
- **tree-sitter `corpus/`**: parse→S-expr→golden-diff — the NormalAst comparison form.
- **GHC / CompCert golden-file validation**: golden-AST anchoring precedent (§2.4).
- **Thompson, *Reflections on Trusting Trust* (1984)**: the reason self-parity alone is not
  verification — the motivation for the whole family.

## 5. Named risks and counterexamples

- **R1 — the self-hosted grammar cannot admit its own dialect (coverage gap, likely to fire
  first).** The skinny IR parser has historically rejected dialect constructs the hand parser
  accepts — `->` projections, `?w`, `,`/`;` terminators. `bbnf.bbnf` itself uses `@import`,
  closures (`|params| rhs`), `?w`, `->` chains, `@host`/`@pretty` directives. *Counterexample to
  expect:* the generated parser fails to parse `css/l4/*.bbnf` or `bbnf.bbnf` at all. This is not a
  failure of the family — it is the oracle's *first product*: the differential names exactly which
  construct the self-hosted grammar under-specifies, turning self-hosting into a coverage-driven
  grammar-completion loop (memory: `no-workarounds-self-hosting` — augment `bbnf.bbnf`, never shim
  the generated parser).
- **R2 — over-canonicalized NormalAst masks real divergence.** If the S-expr drops spans or
  reorders alternation arms, a wrong-span or wrong-precedence bug passes. *Guard:* NormalAst
  carries spans and preserves arm order; the golden set includes a precedence-sensitive and a
  span-sensitive document whose blessed form would break under any lossy normalization.
- **R3 — differential resolves to the hand parser's bug.** If both are wrong the same way (e.g.
  both mis-handle a `\` escape edge), the differential is silent. *Guard:* the golden anchor is
  hand-audited against the SPEC independently of both parsers; json/css rows cross-check against
  serde/cssparser (already trusted). Sheets has no external oracle — its golden set must be larger.
- **R4 — promotion perf gate is vacuous for compile-time parsing.** Grammar-source parse is cold
  and rare; a cyc/B non-regression floor is nearly free to pass, so "promotion earns performance"
  is hollow *for bbnf*. *Answer:* the honest value for bbnf is **LOC deletion** (retire the 2142-LOC
  hand parser), stated as such; the perf gate has teeth only for the **regex** sub-language, whose
  leaves are runtime-hot — there the floor is PROVE-10's certified numbers, not non-regression.
- **R5 — the oracle becomes shelfware if never promoted.** A perpetual CI-only differential that
  never flips is dead substrate in spirit (Y.13 / clause viii). *Guard:* every subsystem carries a
  named promotion deadline and a floor; a subsystem that cannot be promoted after two passes is a
  named negative result (its blocking divergence documented), not an indefinite oracle.

## 6. What a prototype must test to move this family

1. **Emit + coverage.** Regenerate the bbnf-in-bbnf parser from `grammar/bbnf/bbnf.bbnf` through
   `emit_from_source`; does it parse `grammar/**/*.bbnf` at all? Report the exact dialect
   constructs it rejects (R1). This single number — corpus documents admitted / total — is the
   family's first falsifiable output.
2. **Differential + NormalAst.** Build `normal_ast.rs`, run generated-vs-`parse_grammar` over the
   admitted corpus, report the divergence count, each divergence adjudicated against the golden
   anchor. Parity rate is the gate metric.
3. **Perf.** cyc/B of generated vs hand on grammar-source parse (bbnf: expect non-regression, cite
   LOC-deletion as the real prize); regex-leaf parse against PROVE-10's `Dfa::find` floor.
4. **Falsification.** Plant a NormalAst mutation and confirm `check-self-host` trips (gate can
   fail), then confirm it passes clean on HEAD.

The prototype's verdict is a table: *per subsystem — admitted %, parity %, cyc/B ratio,
promote/hold/negative-result*. That table is the whole family, made concrete.
