# LOCK-7 · Family 7D — THE META-CIRCULAR-ORACLE (pass-2)

Self-hosting as VERIFICATION first. The bbnf-defined bbnf/regex grammars are not (yet) the
production parsers; they are DIFFERENTIAL ORACLES run beside the hand parsers, and promotion of
any subsystem to production is gated on measured parity against a golden anchor plus measured
performance. This family owns the equivalence relation, the divergence taxonomy, and the
promotion ledger. It does not require the builder-augment to exist to begin producing value —
the regex oracle already exists in embryo.

---

## 1 · The relation-id (pin it before any parity% is quoted)

A parity% is meaningless until the equivalence relation `≡` it measures is a named, frozen
artifact. Two parsers can emit structurally different but semantically identical trees; without a
fixed normal form the oracle reports false divergences and a "97% parity" number is noise. The
relation is codified as three canonicalization laws over `ExprKind`
(`skinny/crates/ir/src/lib.rs:234`), NOT invented — both extant parsers already obey them, so the
relation DESCRIBES settled behaviour rather than imposing a new convention:

- **L1 (empty = empty concat).** `ExprKind` has NO distinct `Empty`/`Epsilon` variant. The hand
  parser's `parse_seq` (`skinny/crates/grammar/src/lib.rs:1393`) emits `ExprKind::Seq(Vec::new())`
  for zero children — so the grammar-plane normal form is literally `Empty ≡ Seq([])`; the relation
  is DEFINED at `Seq([])`, there is no other Empty to reconcile. The `Empty` at `ir/src/lib.rs:479`
  is `DirectBuildSource::Empty`, a type-plane payload, NOT a grammar node — do not conflate; the
  oracle never sees it.
- **L2 (unit collapse).** `parse_seq` returns `children[0]` unwrapped for a single child
  (`grammar/src/lib.rs:1395`); the regex host does the identical thing — `concat(items)` with
  `items.len()==1` returns the child (`parse-that/rust/parse_that/src/regex/host.rs:52`), and
  `alternation` with `rest.is_empty()` returns the single branch (host.rs:41). So `Seq([x]) ≡ x`
  and `Alt([x]) ≡ x` are already the parsers' behaviour on BOTH planes. Bake it in or every
  builder-emitted single-element wrapper reads as a divergence.
- **L3 (separators/trivia are not nodes).** `parse_seq` treats `,` and `?w`/inline-trivia as
  sequence structure, not AST children (`grammar/src/lib.rs:1381` comment "not an AST node";
  `?w`→`Annotation{name:"ws"}` at :1371). A builder-side parser must normalize identically or the
  relation must exclude trivia positions from the compared shape.

**The shared-semantic-pass pivot.** `resolve_refs` (`ir/src/lib.rs:84`) MUTATES every
`ExprKind::Ref{ target }` from `None` to `Some(RuleId)` (:103). A pre-resolve tree compared to a
post-resolve tree differs on that field for every reference — a guaranteed false negative.
`validate` (:110) is structurally inert (returns a `ValidationReport` of counts; :118) — it is a
gate, not a normalizer. Therefore the relation-id states: **`resolve_refs` runs on BOTH sides or
NEITHER**; the canonical choice is BOTH (post-resolve, targets bound), the state every downstream
pass consumes. `validate` is asserted GREEN on both sides as an admission gate but is NOT part of
the compared value.

**Relation-id = `GRAMMAR-IR-EQ/v1 = {L1,L2,L3} over resolve_refs'd ExprKind trees; references
compared by resolved rule NAME, not RuleId index}`.** RuleId indices are allocation-order
artifacts (`add_expr`/`add_rule` push-order, `ir/src/lib.rs:51,57`); comparing by name lets two
parsers that visit rules in different order still match. Pin this string in the ledger header; a
parity% without it is void (Constitution vii: measured, named).

---

## 2 · The regex oracle already exists — correct the record

`parse_generated` (`parse-that/rust/parse_that/src/regex/mod.rs:34`) is a PUBLIC PRODUCER, not a
recognizer: it runs the self-hosted generated combinator parser (`generated::RegexParser::regex`,
:41) over the pattern, then `host::lower` (:57) folds the typed `RegexParserEnum` AST into a real
`Hir` and RETURNS it (`Result<Hir, ParseError>`). Its own doc states the purpose — "available for
benchmarking and verification against the primary `parse()`" (mod.rs:33). **The meta-circular
regex oracle is therefore already wired in embryo**: `parse_generated(p)` vs the primary
`bbnf_regex::parse(p)`, both yielding `Hir`. 7D's regex deliverable is not to build this oracle but
to (a) pin its relation-id and (b) fix the divergence taxonomy below. The directives' correction
stands: `parse_generated` produces, it does not recognize.

**Regex relation-id.** The settled substrate (XL-C) gives the pivot free: `simplify_hir`
(`parse-that/rust/regex/src/egraph/mod.rs:143`) is a cost-guided equality-saturation canonicalizer
over `Hir` — the regex analog of `resolve_refs`+normal-form. It already carries the
`Empty ≡ Concat([])` law: its comment (egraph/mod.rs:159) records it "only handled
`Empty/Literal/Class/Look`" — `Hir::Empty` is a first-class node here and saturation folds
`Concat([])`/`Alternation([x])` to canonical form. **Regex relation-id =
`HIR-EQ/v1 = structural Hir equality after simplify_hir on BOTH sides`** — run it on both or
neither, exactly as `resolve_refs` on the grammar plane. Same pivot law, two planes.

**Divergence taxonomy — engine-tier `host.rs` OBLIGATION vs ORACLE VERDICT.** Not every
`parse_generated ≠ parse` is a parser-parity failure. `host::lower` (host.rs:19) performs a
semantic fold — `lower_quantifier` (:174), `lower_escape` (:248), `lower_class_escape` (:303),
`lower_char_class` (:357), `lower_class_atom_byte` (:415), unicode expansion under `ctx.unicode`.
Split:

- **ORACLE VERDICT (parser structural parity — a promotion blocker).** Divergences in
  *grouping / precedence / quantifier attachment / concat-alt shape* — the tree the generated
  combinator parser produces vs the hand parser's, BEFORE the fold. Genuine parser bugs; they gate
  promotion.
- **ENGINE-TIER `host.rs` OBLIGATION (not a parser verdict).** Divergences in *what a class/escape
  expands to*, *case-folding*, *unicode property sets*, *`\d`/`\w` byte membership*. These live in
  the host fold and are fixed IN `host.rs`; they never count against the generated parser's
  parity%. A `\p{L}` expansion mismatch is a host obligation to reconcile, not evidence the
  self-hosted parser is wrong. The ledger tags each divergence with its class; only VERDICT-class
  divergences decrement the parity% that gates promotion.

Constitution clause (ii) — regex HIR isomorphic to bbnf HIR — makes this the seam LOCK-5 owns: the
host-fold obligations are LOCK-5's engine tier; 7D consumes LOCK-5's verdict on the lawful `Hir`
form and asserts parity over it, it does not relitigate the HIR shape.

---

## 3 · The grammar oracle — the recognizer-vs-builder gap, and the tape route around it

The regex plane has a builder (`host::lower` → `Hir`). The grammar plane does NOT: the general
emitter emits `parse_<rule>` RECOGNIZERS with signature
`fn parse_ident<'i>(bytes, cursor: &mut usize, tape: &mut TapeBuilder) -> Result<(), ParseError>`
(`skinny/crates/codegen/src/general_descent/emit.rs:269`). `emit_expr` (:288) writes tape-node
stamps and cursor advances — `Seq`/`Alt`/`RepeatLoop`/`ByteLiteral` arms (:301,:309,:361) — with NO
`add_expr`/GrammarIr-builder arm. `bbnf.bbnf` shape-dispatches here because it has no `value` rule
(`runtime_generator.rs:98 !has_rule("value")` → `emit_general_descent`:99). So a naive "bbnf-defined
bbnf parser vs hand parser" oracle has a type mismatch: hand → `GrammarIr`, generated → offset TAPE.

Two routes; 7D carries BOTH with a measured tie-break, presuming neither:

- **Route T (tape-canonical, no builder augment) — LOW RISK.** Define `≡` over a canonical
  serialization, reusing the EXISTING oracle idiom: `bbnf-bench/src/parity.rs:23 assert_parity`
  already serializes a canonical token stream from a parsed root and asserts equality
  (`serialize_canonical` :64, `token_stream_from_root` :68). Generalize it — decode the generated
  parser's TAPE to the same canonical stream that the hand parser's `GrammarIr.pretty`
  (`ir/src/lib.rs:124`) or a canonical walk yields, and compare under `GRAMMAR-IR-EQ/v1`. The
  emitter stays a recognizer (its fast, measured product); the oracle burden is a cheap
  tape→canonical decoder that never perturbs the hot path.
- **Route B (builder augment) — the shared open question.** Author `-> Span`/`-> T` projections on
  bbnf's structural rules so the emitted parser calls `add_expr`/`resolve_refs` and produces a
  `GrammarIr` directly, then compare structurally. UNSEEDED — do not presume it round-trips from the
  emitter's mere existence; it may cascade at `Discard`/`Repeat`/closure constructs. Host budget:
  `add_expr` (append) + `resolve_refs` (link). Whether the augment avoids a borrow-checker escape is
  what 7B/the shared question measures.

7D's position: the oracle is deliverable via Route T TODAY (tape decode + canonical compare) with
zero emitter change; Route B is a promotion accelerant the prototype must prove. Each parity% the
ledger quotes names which route produced it.

---

## 4 · The promotion ledger (schema)

Promotion is per-subsystem, gated, audit-first. One row per (subsystem, plane):

| column | meaning | source |
|---|---|---|
| `subsystem` | `regex/parser`, `bbnf/structural`, `bbnf/projection`, … | — |
| `relation_id` | `HIR-EQ/v1` or `GRAMMAR-IR-EQ/v1` (frozen string) | §1/§2 |
| `route` | `T` (tape-canonical) or `B` (builder) | §3 |
| `corpus_id + N` | golden corpus hash + document count | committed fixture |
| `parity_verdict%` | VERDICT-class agreement (host-obligation divergences EXCLUDED) | §2 taxonomy |
| `totality%` | share of surface behind grammar, NOT host fns | **7C's counter (leaves×branches)** |
| `perf_ratio` | generated cyc/B ÷ hand cyc/B, cold paired N≥200 | standing-law bench |
| `falsification` | planted-mutation id the oracle CAUGHT | §5 |
| `disposition` | ORACLE-ONLY / PROMOTION-CANDIDATE / PROMOTED | gate below |

**`totality%` is CONSUMED from 7C's counter, not recomputed here** — it is a promotion input: a
subsystem mostly behind host functions (low totality%) is not a self-hosting win even at 100%
parity, because the grammar isn't doing the work. **Promotion gate:** `disposition = PROMOTED` iff
`parity_verdict% == 100 (over golden) AND perf_ratio ≤ 1.0× certified AND falsification caught AND
totality% ≥ subsystem floor`. Anything less stays `ORACLE-ONLY` — it runs beside the hand parser as
a differential test, never replaces it (born-RED per subsystem).

**Golden anchor, NOT majority vote.** The reference is a COMMITTED golden — the hand parser's output
frozen on a versioned corpus (the `check-json`/`check-sheets` byte-identical idiom,
`xtask/src/main.rs:41,43`, generalized to a committed expected-tree fixture). Majority vote (three
derived parsers agreeing) is REJECTED by construction: two parsers sharing one lowering bug (both
consume the same `host::lower`, the same `simplify_hir`) out-vote the correct one — a shared-mode
failure the vote cannot see. Agreement is measured against the golden, never against sibling parsers.

**Planted-mutation falsification (the oracle must be PROVEN able to fail).** Mirror GN-5.W3's
name-gate falsification exactly (`GN-5.md` §W3, `name_gate_falsification.rs`): before the oracle is
trusted to certify parity, plant a KNOWN divergence and assert the oracle reports it. Plants:
(i) flip `<<`/`>>` keep-side in one bbnf rule (`ExprKind::Discard{keep_left}`, `ir/src/lib.rs:266`);
(ii) drop an `Alt` branch; (iii) change one regex quantifier's greediness in the generated grammar.
Assert each flips `parity_verdict%` below 100 and NAMES the divergent rule/path; then assert the
un-mutated tree passes clean. An oracle that cannot fail is a defect (SPEC §0 failure-mode
checklist) — this test is the proof it can, and it is the `falsification` column's content. A row
with an empty `falsification` cannot exceed ORACLE-ONLY.

---

## 5 · Staging & fixpoint facts 7D must CARRY (not re-derive)

- **No bbnf self-regen exists in skinny.** `xtask/src/main.rs:40-52` dispatches
  `regen-json`/`check-json`, `regen-sheets`/`check-sheets`, `regen-css`, `regen-real-typed`,
  `check-conformance`, `lint-loc` — NO `regen-bbnf`, NO fixpoint gate, NO bbnf-in-bbnf target. A
  repo-wide grep for `88s|0.5s|fixpoint|regen∘regen` in skinny finds nothing. **The ~88s→0.5s datum
  and the regen∘regen fixpoint are legacy transplants, unseeded at skinny HEAD** — 7D quotes neither
  as an existing property. The extant byte-identical gates are `check-json`/`check-sheets` (regen
  output == committed `.rs`), the honest fixpoint SUBJECT candidate: *generated `.rs` bytes*, NOT
  GrammarIr equivalence.
- **Byte-determinism is non-vacuous only if the emit path is input-sensitive.** A fixed-const emit
  arm stamps identical bytes for any grammar; triple-identical certifies nothing. `general_descent`
  IS input-sensitive (it walks `SinkOnlyExpr` structure, emit.rs:288), but the fixpoint claim must
  be paired with a MUTATION test: change the grammar, and the bytes must CHANGE — else determinism
  is trivially true and certifies nothing.

---

## 6 · Prior art (citations)

- **Differential parser testing as the gate** — CSmith→compiler and `sqllogictest`/SQLancer: a fixed
  reference oracle + generated inputs, divergence = bug. 7D's golden-anchor-not-vote is the CSmith
  discipline (a fixed reference, not a jury).
- **rustc `-Zunpretty` AST round-trip / `rustfmt` idempotency** — parse→print→parse fixpoint as a
  structural-equivalence oracle; the relation is over the AST modulo pretty-print, exactly L1–L3's
  "modulo trivia/unit-collapse".
- **GHC `-ddump-simpl` + Core lint** — a canonicalizing pass (`simplify_hir`'s analog) run on both
  sides before comparison; a divergence in the fold (Core opt) is engine-tier, a divergence in parse
  is a front-end verdict — the §2 taxonomy.
- **tree-sitter's corpus `test` harness** — committed golden trees in S-expression normal form as
  the anchor; a parser is promoted against the frozen corpus, never against a sibling grammar.
  Route T's tape→canonical serialization is tree-sitter's S-expr idiom.
- **egg / egglog equality saturation as canonicalizer** — `simplify_hir`
  (`regex/src/egraph/mod.rs:143`) IS this; "equal after saturation" is the egg extraction-to-canon
  contract, the settled substrate XL-C cites.
- **Mutation testing (`cargo-mutants`, PIT)** — the "oracle must fail" proof; §4's planted-mutation
  falsification is a mutation-adequacy test on the oracle itself, mirroring GN-5.W3.

---

## 7 · Named risks & counterexamples

- **R1 — parity% quoted before the relation-id is pinned.** Any number without the frozen
  `{HIR-EQ,GRAMMAR-IR-EQ}/v1` string and the resolve_refs/simplify_hir pivot is noise; L2 unit-
  collapse alone can swing a naive comparator by every single-child wrapper. *Guard:* the ledger
  header carries the relation string; a row without it is void.
- **R2 — host-fold divergences miscounted as parser failures.** A `\d`/unicode-class mismatch in
  `host.rs` decrements the parser's parity% and blocks a correct parser. *Counterexample:* plant a
  `lower_class_escape` (host.rs:303) bug and confirm the ledger tags it ENGINE-TIER, not VERDICT —
  the taxonomy is exercised, not asserted.
- **R3 — vacuous byte-determinism.** A const-stamped emit arm passes regen∘regen trivially. *Guard:*
  §5's mutation test — generated bytes MUST change under a grammar mutation or the fixpoint is
  disqualified as evidence.
- **R4 — Route B presumed from the emitter's existence.** The emitter emits recognizers
  (emit.rs:269, no builder arm); assuming a `->`-projection augment round-trips into
  GrammarIr-equivalent structure is the unseeded question — it may cascade at
  `Discard`/`Repeat`/closure. *Guard:* the oracle ships via Route T independent of Route B; Route B
  is a gated candidate.
- **R5 — golden drift.** If the golden is regenerated from the hand parser at test time, a
  hand-parser regression silently rebaselines the anchor and the oracle certifies the regression.
  *Guard:* the golden is a COMMITTED fixture with a hash in the ledger (`corpus_id`), updated only by
  an explicit reviewed regen — never auto-derived at test time.
- **R6 — oracle that cannot fail.** *Guard:* §4 planted-mutation falsification is a hard gate; an
  empty `falsification` column caps disposition at ORACLE-ONLY.

---

## 8 · What the PROTOTYPE must test to move 7D

1. **Freeze both relation-ids as code.** Implement `GRAMMAR-IR-EQ/v1` (L1–L3 + resolve_refs pivot,
   RuleId-by-name) as a comparator over two `GrammarIr` (or tape-canonical streams), and `HIR-EQ/v1`
   (structural `Hir` eq after `simplify_hir` both-sides). Prove L1 `Seq([])` and L2 unit-collapse by
   running one input through both extant parsers and asserting eq.
2. **Regex oracle, live.** Run `parse_generated` (mod.rs:34) vs `bbnf_regex::parse` over a committed
   regex corpus under `HIR-EQ/v1`; classify every divergence VERDICT vs ENGINE-TIER; emit the first
   ledger rows with a real `parity_verdict%` and the host-obligation exclusions itemized.
3. **Grammar oracle via Route T (no emitter change).** Decode the general-descent tape for `bbnf.bbnf`
   (driven through `!has_rule("value")`→`emit_general_descent`, runtime_generator.rs:98 — NEVER
   `json_sink_direct`) to the `parity.rs` canonical stream (parity.rs:64) and compare to the hand
   parser's canonical walk under `GRAMMAR-IR-EQ/v1`; report where it diverges and whether it is
   L1–L3 normalization or a genuine verdict.
4. **Falsification.** Plant the three §4 mutations (Discard keep-side flip, dropped Alt branch, regex
   greediness) and assert each is CAUGHT and named; assert clean passes clean — the gate that lets a
   row exceed ORACLE-ONLY.
5. **Non-vacuous determinism.** Mutate `bbnf.bbnf`, regen, assert the generated `.rs` bytes CHANGE
   (disqualify const-stamp), then assert regen∘regen on the fixed input is byte-identical — the
   honest fixpoint subject (generated bytes), distinct from GrammarIr equivalence.
6. **Populate the ledger** with `totality%` consumed from 7C's counter (do not recompute) and
   `perf_ratio` from a cold-paired N≥200 bench of `parse_generated`/general-descent vs the hand path
   — so the first promotion candidacy is a measured row, not a claim.
