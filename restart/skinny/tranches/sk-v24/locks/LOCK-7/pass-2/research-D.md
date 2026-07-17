# LOCK-7 pass-2 — Family 7D: THE META-CIRCULAR ORACLE

Self-hosting recast as *verification first*. The bbnf-defined bbnf grammar and the bbnf-defined
regex grammar are not first asked to *replace* the hand parsers — they are first asked to
*agree* with them, structure-for-structure, over a corpus, under a pinned equivalence relation.
Promotion of a self-hosted parser to the production path is a per-subsystem verdict gated on
measured parity plus measured performance, never on the mere existence of a generated parser.
This family owns the relation, the ledger, and the falsification that keeps both honest.

The decisive asymmetry this family must state up front: **the regex oracle is realizable at HEAD;
the bbnf oracle is not.** One subsystem already has a self-hosted *producer* that emits a
diffable AST (`regex::parse_generated` → `host::lower` → `Hir`); the other has only a
self-hosted *recognizer* emitter that yields no structure to diff. 7D is therefore two rows in
one ledger — a GREEN-capable regex row and a RED bbnf row with a named gate — not one homogeneous
"self-hosting %".

---

## 1. The equivalence relation — pinned before any parity% is quoted

A parity number is meaningless until the relation it measures is fixed and versioned. Two facts
of the substrate force the relation's shape.

**Fact A — the two sides normalize the empty/singleton cases differently, by construction.**
- The bbnf grammar IR has **no** `Empty`/`Epsilon` node. The empty match IS
  `ExprKind::Seq(Vec::new())` — the hand parser emits exactly this when a concatenation has zero
  children (`grammar/src/lib.rs:1394` `Seq(Vec::new())`; single-child collapses at `:1393`).
  `ExprKind` (`ir/src/lib.rs:234`) enumerates `Seq, Alt, Repeat, Optional, Literal, Regex, Ref,
  Annotation, Project, Discard` — **there is no `Empty` variant**. (The `Empty` at
  `ir/src/lib.rs:479` is `DirectBuildSource::Empty`, a schema-direct leaf — unrelated; do not
  conflate.)
- The regex engine HIR *does* carry a distinct `Empty`: `Hir::{Empty, Literal, Class, Look,
  Repetition, Group, Concat, Alternation}` (`parse-that rust/regex/src/hir/mod.rs:216`). Here
  `Hir::Empty` and `Hir::Concat(vec![])` are **two different nodes** denoting the same language,
  and `Hir::Group(Box<Hir>)` is documented "transparent — only the sub-expression matters"
  (`hir/mod.rs:227`), i.e. `Group(x)` denotes the same as `x`.

So `Empty ≡ Concat([])` is not a nicety — it is the *minimum* rewrite the relation must include,
and on the regex side it must be joined by `Group(x) ≡ x`. The relation is therefore a
**canonicalizing rewrite set applied to both sides before comparison**, not raw structural `==`:

```
CANON (relation-id v1):
  Seq([x])          ≡ x            Alt([x])           ≡ x
  Seq([])           ≡ Empty        Concat([])         ≡ Empty      (regex tier)
  Group(x)          ≡ x            Alternation([x])   ≡ x          (regex tier)
  Concat([x])       ≡ x
```

`host::lower` **already performs** the singleton collapses on the regex side (`regex/host.rs:37`
`if rest.is_empty() return lower_node(first)`, `:49` `if items.len()==1 return
lower_node(&items[0])`) — so v1 CANON is exactly "make the *hand* side do what `host::lower`
already does, on both sides." This is an e-graph canonicalization in miniature (egg/egglog: the
relation-id IS the rewrite-rule set; run both trees to normal form, compare representatives). Cite
egglog congruence-closure as the lawful engine if the rule set outgrows hand-application.

**Fact B — the shared-semantic-pass pivot is a hard precondition, not a comparison detail.**
`resolve_refs` (`ir/src/lib.rs:84`) *mutates* every `Ref{name, target}`, filling `target: None →
Some(rule_id)` from the rule-name table; `validate` (`ir/src/lib.rs:110`) runs
`validate_resolved_refs`. If the oracle compares one side *before* `resolve_refs` and the other
*after*, **every `Ref` node diverges on the `target` field** and parity collapses spuriously. The
pivot law: `resolve_refs`+`validate` run on **both sides or neither**, at one pinned pipeline
stage. The regex analogue is `host::lower` (fold from `RegexParserEnum` AST → `Hir`): both the
self-hosted and hand paths must fold through the *same* `host::lower`, else a fold bug (e.g. an
`(?si)` inline-flag not threaded through `LowerCtx`, `host.rs:11-16`) masquerades as a grammar
divergence.

**Relation-id, pinned.** `relation-id = (CANON-rule-set-hash, pipeline-stage, shared-pass-set)`.
For bbnf: `(v1, post-resolve_refs+validate, {resolve_refs, validate})`. For regex:
`(v1, post-host::lower, {host::lower})`. No parity% is quotable that does not name its relation-id;
two runs quoting different numbers under the same claimed % is the first falsification signal.

---

## 2. Regex divergences — host.rs obligations vs oracle verdicts (record corrected)

On the settled regex substrate (grammar tier `ExprKind::Regex{pattern}` opaque, `ir/src/lib.rs:253`;
engine tier rich `Hir`, `regex/src/hir/mod.rs:216`; bridged per XL-C), a divergence between the
self-hosted generated parser and the hand `parse()` falls into two disjoint buckets, and
mis-bucketing them is risk R3 (§6).

**Bucket 1 — engine-tier `host.rs` obligations (fix the fold, NOT an oracle verdict).** The
generated parser (`regex/generated.rs`, from `grammar/misc/regex.bbnf`) produces only the AST
*shape*; all `Hir` *semantics* live in `host::lower` (`regex/host.rs:19`). Divergences rooted in the
fold are host obligations:
- inline flag groups `(?si)` tracked across the fold in `LowerCtx` (`host.rs:11-16` dotall /
  case_insensitive / unicode);
- quantifier lowering (`host.rs:174` `lower_quantifier`) — greedy/lazy, `{n,m}` bounds;
- escape and class-escape folding (`host.rs:248` `lower_escape`, `:303` `lower_class_escape`);
- char-class construction and negation (`host.rs:357` `lower_char_class`);
- unicode case-folding / class expansion (`host.rs` → `bbnf_regex::unicode`).
A mismatch here is a `host.rs` bug to redress; the grammar is not "failing" the hand parser.

**Bucket 2 — oracle verdicts (the grammar genuinely parses differently).** Pure syntactic
parse-structure divergences the shared fold then propagates. `grammar/misc/regex.bbnf` already
carries live suspects the oracle should *catch*, proving it non-vacuous:
- `decimal = /d+/` (`regex.bbnf:18`) — almost certainly meant `\d+`; as written it matches literal
  `d`, a real divergence from any sane hand parser's `\d`;
- `back_reference = "\\" , number` (`:23`) vs the hand parser's back-ref handling;
- `lookbehind = "(?<=(" , expression , "))"` (`:26`) — the doubled paren is structurally odd;
- `hex = /x[0-9a-fA-F]+/`, `octal = /0[0-7]+/` (`:19-20`) — escape-prefix handling.
These are what a differential oracle certifies subsystem-by-subsystem.

**Record correction.** `regex::parse_generated` (`parse-that rust/parse_that/src/regex/mod.rs:34`)
is a **PUBLIC PRODUCER**: `pub fn parse_generated(pattern) -> Result<Hir, ParseError>` — it parses
*then* `host::lower`s to a full `Hir` (`mod.rs:60` `host::lower(&ast, pattern, opts)`). It is NOT a
recognizer. The confusion is a genuine namesake: there is *also* a `pub fn parse_generated(pattern)
-> bool` recognizer at `regex/generated.rs:2288`. The oracle consumes the **`mod.rs:34` producer**
(returns `Hir`), diffed against `regex::parse` (the hand `Hir`) under the §1 relation. Any note
calling `parse_generated` a recognizer is corrected here: the diffable-AST producer is what makes
the regex oracle realizable at HEAD.

---

## 3. The bbnf side has no producer — the RED row and its gate

The bbnf oracle cannot exist yet, for two independent reasons, both grounded in the emitter's own
prose.

**(a) `bbnf.bbnf` does not route through the emitter.** Three bounded parser gaps block it:
`?w`→`->` attachment in `parse_seq`, the `@{}` atom in `parse_atom`, and `|params|` closures
(`bbnf.bbnf:33` `"@{" , rhs`, `:51` closure, `:42` `mapped_factor` with `->`). The hand builder
already *rejects* these on the runtime-construct path — `grammar/src/lib.rs:222` maps `@{}` to
`"BBNF-UNSUPPORTED-HOST-CAPTURE"` and `Projection`/`TypedProjection` to
`"BBNF-UNSUPPORTED-PROJECTION"`.

**(b) Even routed, the emitter yields no AST to diff.** The general shape-dispatched emitter
(`codegen/src/general_descent/`, entered via `runtime_generator.rs:98` `!has_rule("value")`) emits,
in its own words, "one `parse_<rule>` **recognizer** per rule … a recursive-descent recognizer + a
lazy tape-backed view" (`general_descent/mod.rs:1-11`). A recognizer returns accept/offsets, **not
a `GrammarIr`**. There is nothing to compare to the hand parser's `GrammarIr`. Worse for a naive
oracle: the emitter's regex leaves are matched by a "grammar-sanctioned DELIMITER-SCAN fallback …
Span-projecting the leaf … the grammar's own TODO (typed leaves deferred to `Span`)"
(`general_descent/mod.rs:16-25`) — so even the leaf semantics are lossy relative to the hand
parser's typed leaves.

**The gate (born-RED).** The bbnf oracle is blocked on the LOCK-7 *shared open empirical question*:
does authoring `-> Span`/`-> T` builder projections on bbnf's structural rules over the recognizer
emitter cause it to emit a `GrammarIr`-**producing** parser (not a recognizer), reparsing the
corpus into `GrammarIr`-equivalent structure within the `add_expr`+`resolve_refs` host budget,
while `regen∘regen` holds byte-identical — or does it cascade, and at which construct? Until that
producer augment lands and is driven through the `!has_rule("value")` branch (never
`json_sink_direct`), the bbnf ledger row is RED / SPEC-only. **Regex proves the pattern is
achievable** (a producer exists, folds to a rich IR, is diffable); bbnf must reach the same
producer status or stay RED. Do not seed the verdict from the legacy emitter or from the emitter's
mere existence — the recognizer-vs-builder wall is the unseeded question.

---

## 4. The promotion-ledger schema

Subsystem-by-subsystem, golden-anchor (the hand parser is THE reference — never a majority vote
among parsers, which can agree on a shared bug). Columns:

| col | source | meaning |
|-----|--------|---------|
| `subsystem` | {bbnf-parser, regex-parser}, later per-rule-group | the promotable unit |
| `relation-id` | §1 | `(CANON-hash, stage, shared-pass-set)` — no % without it |
| `corpus-id` | committed fixtures | the diff population; includes planted adversaries |
| `parity%` | oracle | fraction of corpus where `CANON(self) ≡ CANON(hand)` |
| `totality%` | **7C's leaves×branches counter** | share of surface grammar-expressed vs behind host fns |
| `regen-fixpoint` | xtask | `regen∘regen` byte-identical (bool; non-vacuous per 7A) |
| `perf-ratio` | cyc/B, paired cold | self-hosted ÷ hand (constitution's gate; parity alone never promotes) |
| `verdict` | derived | PROMOTE iff parity=100% ∧ perf-ratio ≤ threshold ∧ regen-fixpoint ∧ totality gate met |

Row at HEAD:
- **regex-parser**: relation-id `(v1, post-host::lower, {host::lower})`; corpus = the 44-test regex
  suite (`parse-that rust/parse_that/src/regex/mod.rs:1322` prefilter-test lineage) + §2 adversaries;
  parity% MEASURABLE now; verdict gate-ready.
- **bbnf-parser**: RED — no producer (§3); parity% unquotable; verdict BLOCKED on the shared open
  question.

**Golden-anchor, not majority-vote.** The hand parser's `GrammarIr` / `Hir` is the oracle; the
self-hosted parser is the *candidate*. A third parser is never a tiebreak vote — three parsers
sharing `decimal=/d+/` would vote a bug into "truth". This mirrors tree-sitter's `test/corpus`
S-expression golden trees (expected tree committed, generated parser judged against it) and
regex-automata's `regex-test` TOML reference corpus — golden anchors, not consensus.

**Planted-mutation falsification (the gate must be able to fail).** Modeled on GN-5.W3's name-gate
falsification (`xtask/tests/name_gate_falsification.rs` plants three gate forms, asserts the gate
trips, then asserts HEAD passes clean — GN-5.md §4 W3): plant a grammar mutation into a throwaway
copy of `regex.bbnf` (e.g. flip a quantifier arm, or *correct* `decimal=/d+/`→`\d+` and assert the
parity delta moves), regenerate, and assert the oracle's parity% **drops and names the first
divergent node**. An oracle whose parity% cannot be moved by a planted grammar mutation is vacuous
(the 7A "triple-identical certifies nothing" trap in oracle clothing) and must be rejected before
its passing number is trusted.

---

## 5. Prior art (citations)

- **Differential testing of parsers/compilers** — McKeeman (1998, *Differential Testing for
  Software*); Csmith (Yang et al., PLDI 2011). The golden-anchor + adversarial-corpus method is
  theirs; 7D is differential testing with the hand parser as reference.
- **Translation validation** — Pnueli, Siegel, Singerman (TACAS 1998); CompCert's per-instance
  validation. Justifies *per-subsystem, per-input* promotion over "trust the whole generator".
- **tree-sitter** — `grammar.js` → generated parser judged against committed `test/corpus/*.txt`
  S-expression golden trees. The exact promotion-ledger model: golden tree, not vote.
- **regex-automata** — `regex-test` TOML corpus; PikeVM/backtrack/DFA differentially tested but all
  anchored to the crate's reference semantics. Directly the "many engines, one golden anchor" law.
- **GHC bootstrap** — stage1/stage2 with the byte-identical stage-fixpoint check; the `regen∘regen`
  column's ancestor (and the caution that a fixed-output path makes the fixpoint vacuous).
- **egg / egglog** (Willsey et al., POPL 2021; Zhang et al. 2023) — congruence closure as the lawful
  home for the CANON relation once it outgrows hand-application; the relation-id = rewrite-set.
- **rustc / `syn`** — parser output validated by re-emission + reparse round-trips; the round-trip
  discipline behind `regen∘regen`.

---

## 6. Named risks and counterexamples

- **R1 — Vacuous parity (the empty oracle).** If the corpus only holds inputs both parsers accept
  identically, parity% is trivially ~100% and certifies nothing (the 7A "fixed-const emit stamps
  identical bytes" trap, re-dressed). *Guard:* the corpus MUST include planted adversaries and known
  live divergences (`decimal=/d+/`, the doubled-paren lookbehind); §4's planted mutation must move
  the number.
- **R2 — Relation-id drift.** An under-pinned relation (no CANON hash, no stage, no shared-pass-set)
  makes parity% uninterpretable — two honest runs quote different numbers. *Guard:* commit the CANON
  rule set + stage as a versioned artifact; refuse any % without a relation-id; two runs disagreeing
  under one claimed id is itself the falsification.
- **R3 — Host-tier divergence miscategorized as oracle verdict.** A `host::lower` fold bug (`(?si)`
  flag not threaded, `host.rs:11-16`) diverges every flag-group pattern; reading that as "the grammar
  fails" is wrong and would block a correct grammar. *Guard:* the §1-B pivot — one shared
  `host::lower` / `resolve_refs` on both sides; §2's Bucket-1/Bucket-2 split is the adjudication rule.
- **R4 — The bbnf oracle has no producer (structural, not a bug).** The general_descent emitter emits
  recognizers (`general_descent/mod.rs:1-11`); there is no `GrammarIr` to diff, and leaf semantics
  are Span-lossy (`:16-25`). *Guard:* bbnf row stays RED until the §3 producer augment lands; regex is
  the existence proof it is reachable, not a promise it is done.
- **R5 — Promotion on parity without performance.** Parity 100% but self-hosted parser materially
  slower than hand regresses the product on promotion. *Guard:* the ledger's `perf-ratio` column +
  the constitution's cold-paired cyc/B gate; PROMOTE requires parity ∧ perf ∧ fixpoint ∧ totality,
  conjunctively.
- **R6 — Corpus-fixpoint confound.** `regen-fixpoint` green can be vacuous if the emit path is
  constant w.r.t. the grammar (7A's warning). *Guard:* the fixpoint column is only meaningful
  alongside a parity% that *moves* under mutation — a green fixpoint on a mutation-insensitive oracle
  is a defect, not a pass.

---

## 7. What a prototype must test to move 7D

1. **Regex oracle harness (GREEN-capable).** Build the differential driver:
   `regex::parse_generated(p)` (producer, `mod.rs:34`) vs `regex::parse(p)`; canonicalize both `Hir`
   under CANON v1 (singleton-collapse + `Empty≡Concat([])` + `Group(x)≡x`, via the same `host::lower`
   fold); run over the 44-test suite + adversaries; emit parity% + first divergent node path. Confirm
   whether `decimal=/d+/` (`regex.bbnf:18`) and the doubled-paren lookbehind (`:26`) are live
   divergences — a non-vacuity smoke test.
2. **Relation-id artifact.** Commit CANON + stage + shared-pass-set; show two independent runs quote
   *identical* parity% (R2 falsification-proof).
3. **Planted-mutation falsification.** Mutate a throwaway `regex.bbnf` leaf, regenerate, assert
   parity% drops and names the divergence; assert HEAD passes clean (the GN-5.W3 pattern).
4. **bbnf producer probe (RED verdict, driven honestly).** Attempt the `-> Span`/`-> T` builder
   augment over the recognizer emitter, route `bbnf.bbnf` through `!has_rule("value")` (never
   `json_sink_direct`), and record the exact construct at which it cascades — the 3 parser gaps
   (`?w`→`->`, `@{}`, `|params|`) then the recognizer-not-builder wall (`general_descent/mod.rs:1`).
   The output is the shared-open-question verdict, not a presumption.
5. **Promotion-ledger dry-run.** Fill the schema (§4) for the regex subsystem with real parity%,
   totality% (from 7C's counter), regen-fixpoint, perf-ratio; leave the bbnf row RED with the §3 gate
   named. This dry-run IS the family's concrete deliverable and its consumer.
