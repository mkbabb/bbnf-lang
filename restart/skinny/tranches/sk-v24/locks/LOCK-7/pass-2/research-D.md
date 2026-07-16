# LOCK-7 pass-2 — Family 7D: THE META-CIRCULAR ORACLE

Self-hosting recast as *verification first*. The bbnf-defined bbnf grammar and the
bbnf-defined regex grammar do not begin life as production parsers; they begin as
**differential oracles** run beside the hand parsers. Promotion to a production role is a
per-subsystem event gated on a pinned equivalence relation, a golden-anchor parity number, a
totality percentage, and a cold-per-parse performance ratio — never on the mere existence of
a generated parser, never on majority vote. The family's whole discipline lives in one law:
*pin the relation-id before quoting any parity%*.

---

## 1. The equivalence relation (pin it before any parity%)

An oracle compares two productions of one input. The comparison is meaningless until the
relation it uses is fixed, versioned, and its quotient audited. Two facts of the substrate
force this.

**(a) There is no `Empty` node — the empty witness is `Seq([])`.** `ExprKind`
(`ir/src/lib.rs:234`) enumerates `Seq / Alt / Repeat / Optional / Literal / Regex / Ref /
Annotation / Project / Discard` — *no* `Empty` variant. The hand parser mints the empty
sequence as `Seq(vec![])` (`grammar/src/lib.rs:1392-1396`) and already collapses the
singleton `Seq([x]) → x` at parse time (`:1397`). So the constitution-level identity is
concretely **`Empty ≡ Seq([])`**, and the relation must be closed under a named rewrite set:

- `Seq([]) ≡ Empty` (the identity element);
- `Seq([x]) ≡ x`, `Alt{[x]} ≡ x` (singleton collapse — the hand parser does the Seq case at
  build time, but a *builder projection* or an alternate construction path need not, so the
  relation must, not the parser);
- `Seq` associative-flatten (`Seq([a, Seq([b,c])]) ≡ Seq([a,b,c])`);
- `Optional(x) ≡ Repeat{x, 0, 1}` — **conditionally**: only when the two carry the same
  projection semantics; over-quotienting here hides a real divergence (see §6), so this rule is
  *off by default* and enabled only for the acceptance-parity rung.

Name it **`OracleRel-bbnf-v1`** and enumerate exactly this rewrite set in the ledger. A parity
number quoted without a relation-id is void, because two "equal" trees may differ only by
Seq-arity or a target-resolution flag.

**(b) The shared-semantic-pass pivot: both sides or neither.** `resolve_refs`
(`ir/src/lib.rs:84`) *mutates* `Ref.target` in place from `None` to `Some(rule_id)`;
`validate` (`:110` → `validate_resolved_refs` `:300`) checks the resolved refs and rejects
duplicate rules. If the hand side has run `resolve_refs` (every `Ref` carries `target:Some`)
and the oracle side has not (`target:None`), **every `Ref` node diverges spuriously**. The
relation is therefore defined over a *fixed shared prefix* of passes applied identically to
both productions:

```
OracleRel-bbnf-v1(P_hand, P_oracle) :=
    normalize( validate ∘ resolve_refs (P_hand) )  ≡_struct
    normalize( validate ∘ resolve_refs (P_oracle) )
```

where `normalize` is the `OracleRel-bbnf-v1` rewrite set above and `≡_struct` is structural
equality of the quotiented `GrammarIr`. Same passes, same order, on both sides — or neither.

**Pluggable formulation (ties LOCK-2).** The regex side already implements its quotient as an
**e-graph**: `simplify_hir(hir, &RegexExtractionCost)` at
`parse-that/rust/regex/src/egraph/mod.rs:143`. The bbnf relation should mirror it — an e-graph
over `ExprKind` seeded with the rewrite set above, parity defined as *both trees extract to the
same e-class representative under a fixed cost*. This makes the quotient a pluggable rule-set
(per the pluggable-components edict), not a hand-written comparator, and homes it in LOCK-2's
e-graph rather than a bespoke subsystem.

---

## 2. The regex oracle — a *producer*, structure-level from day one

**Correct the record (directive item):** `parse-that/.../regex/mod.rs:34 parse_generated` is a
**PUBLIC PRODUCER, not a recognizer**. It runs the generated regex parser then `host::lower`
and returns a rich `Hir` (`mod.rs:34-58`). This is the decisive asymmetry of 7D: the regex
subsystem can be diff-tested *at the structure level* — `Hir` against `Hir` — whereas the bbnf
general-descent emitter yields only recognizers (§3). The regex subsystem is the
**furthest-along and promotes first**.

The regex oracle relation:

```
HirRel-v1(p) := simplify_hir( parse(p) )  ≡_struct  simplify_hir( parse_generated(p) )
```

over a conformance corpus. `simplify_hir` (egraph, `:143`) is the shared canonicalizer applied
to both — the exact analogue of §1's shared-pass pivot.

**Which divergences are engine-tier `host.rs` obligations, which are oracle verdicts:**

- **Host obligations (NOT parser bugs — expected, must be normalized away before comparison):**
  `parse-that/.../regex/host.rs` (524 LOC) carries the semantic fold. `LowerCtx`
  (`host.rs:11-15`, `lower` `:18`) tracks inline flag groups `(?si)` — dotall,
  case-insensitive, unicode — across the walk; unicode property/class expansion; and the
  `Empty ≡ Concat([])` collapse is *visible in host.rs* (`alternation` with empty `rest`
  returns the single branch; a one-element concat lowers to its child). Flag-scope semantics,
  unicode expansion depth, and `simplify_hir` canonicalization depth are **deliberate lowering
  choices** — two `Hir`s equal *after* `simplify_hir` but different before are NOT divergences.
  The oracle must compare after `simplify_hir`, else these read as false positives.
- **Oracle verdicts (the true promotion blockers):** a construct the generated parser rejects
  that hand `parse()` accepts (or vice-versa); or an `Hir` shape difference that *survives*
  `simplify_hir`. These, and only these, drop `HirRel-v1` parity.

**Named perf caveat for the regex ledger row:** `parse_generated` runs on the parse-that
combinator runtime (`generated::RegexParser`, `mod.rs:40-42`), so its cold-per-parse ratio vs
hand `parse()` folds in combinator overhead, not just grammar overhead. The perf gate must
isolate the parse stage from `host::lower`, or the regex parser will look slower than the
grammar plane actually is.

---

## 3. The bbnf oracle — recognizer-level today, builder-level RED-unknown

Routing `bbnf.bbnf` through the one generator lands in the **general-descent** path:
`runtime_generator.rs:98 !sink_only.has_rule("value") → emit_general_descent`. That emitter
(`general_descent/emit.rs render` → one `parse_<rule>` per rule) emits **recognizers** — they
accept/reject and drive a lazy tape/view (`view.rs`); they do **not** build a `GrammarIr`. Its
own module header states the leaf story is a delimiter-scan `Span` deferral
(`general_descent/mod.rs:16-24`). So the bbnf oracle has two rungs, and they are not the same
oracle:

**Rung 1 — acceptance parity (achievable now).** Compare the hand bbnf parser's accept/reject
verdict against the generated bbnf recognizer's verdict over the `grammar/` corpus (json, css
modules, ebnf, bnf, math, sheets, bbnf.bbnf itself). This is a *weaker* relation
(language-equality, not tree-equality) but it is honest and buildable once the **three bounded
parser gaps** close — `?w`→`->` attachment in `parse_seq` (`grammar/src/lib.rs:1361`;
standalone-`?w` at `:1366` and `:1449`), the `@{}` atom in `parse_atom` (`:1495`; the runtime
side is `resolve_runtime_import_closure`, `@{` scanned at `:420`), and `|params|` closures.
These gaps are what block the emitter's generated recognizer from reading bbnf's own surface.

**Rung 2 — structure parity (the RED-unknown, per the shared empirical question).** Authoring
`-> Span` / `-> T` builder projections on bbnf's structural rules over the *recognizer*
emitter, so it produces a tree comparable to `GrammarIr` under `OracleRel-bbnf-v1`. Whether the
emitted builder reparses the corpus into `GrammarIr`-equivalent structure within the measured
`add_expr`+`resolve_refs` host budget **while `regen∘regen` holds byte-identical** is
**unseeded and must be reported RED-unknown** — do NOT presume round-trip from the legacy
emitter or from the emitter's mere existence. Candidate cascade points to test first: the
recursive `expression` self-reference resolved through `resolve_refs` (a builder that
constructs `Ref{target:None}` before the shared pass runs), and the `@{}` host-capture atom
(which the recognizer emitter has no projection for — `general_descent/mod.rs` names no host
tier). The bbnf ledger row therefore carries a *recognizer* verdict until rung 2 is proven, and
promoting a *producer* role on rung-1 acceptance parity alone would be unsound (acceptance
parity ≠ structure parity).

**Fixpoint subject for the oracle.** The single-stage staleness gate already exists —
`xtask/src/regen.rs:37 check_target` fails if committed generated bytes differ from a fresh
regen (`main.rs:40-47` targets json/css/sheets/real-typed; **there is no `regen-bbnf` target**,
so the oracle needs a new one). Note the hazard from a 7D seat: comparing generated `.rs`
**bytes** is vacuous if the emit path is fixed-const (identical bytes for any input). The
non-vacuous oracle subject is **`GrammarIr`/`Hir` equivalence under the pinned relation**, not
bytes.

---

## 4. The promotion ledger schema

One row per subsystem, cut once the relation-id is pinned. Columns:

| col | source / meaning |
|-----|------------------|
| subsystem | `regex-parser` \| `bbnf-parser` \| … |
| relation-id | `HirRel-v1` / `OracleRel-bbnf-v1` — versioned, rewrite-set enumerated |
| role | `recognizer` \| `producer` (what is being promoted; bbnf sits at `recognizer` until rung 2) |
| corpus | differential fixture set + size (regex: `parse-that/rust/regex` tests + a conformance TOML mirroring `regex-automata`; bbnf: `grammar/` tree) |
| parity% | fraction of corpus where oracle ≡ hand under the relation, measured against a **GOLDEN ANCHOR** (below), never majority vote |
| totality% | **from 7C's leaves×branches counter** — fraction of surface behind grammars vs host fns; a subsystem promoting at low totality is more host than grammar and is flagged |
| perf ratio | oracle vs hand parser, cold per-parse, PMU cyc/B (parse stage isolated from `host::lower`) |
| verdict | `BLOCKED` / `SHADOW` (runs beside, not promoted) / `PROMOTED` |

**Golden anchor, not majority vote.** With only two parsers (hand + oracle) a majority is
undefined; and three parsers agreeing on a *wrong* answer certifies nothing. The anchor is a
frozen, hand-audited expectation set — the direct analogue of tree-sitter's `test/corpus/*.txt`
S-expression fixtures and `regex-automata`'s conformance TOML. The oracle must match the
**anchor**, and for regex the anchor `Hir` must be audited *independently of* `simplify_hir`
(else a `simplify_hir` bug makes both sides "agree" on the wrong shape).

**Planted-mutation falsification (GN-5 §1 Precept 19; mirrors GN-5 W3's planted name-gate).**
Before the parity column is trusted to *pass*, it must be proven able to *fail*: plant a
mutation in the oracle grammar/parser — e.g. drop the `+` `Repeat{min:1}` branch
(`grammar/src/lib.rs:1461`) or the regex `alternation` empty-rest collapse — and assert parity
drops below 100% on the corpus that exercises that construct. If parity stays 100%, the
**corpus is inadequate**, not the parser — this makes the corpus itself falsifiable and yields
a coverage gate (every `ExprKind` variant and every host-fn exercised, or a promoted parser can
hide an unexercised divergence: the vacuous-parity failure mode).

---

## 5. Prior art (citations)

- **Differential testing / oracles:** McKeeman, *Differential Testing for Software* (1998);
  Yang, Chen, Eide, Regehr, **CSmith** (PLDI 2011) — the canonical grammar-driven differential
  oracle for a compiler front end; "two implementations, one relation" as the sound substrate.
- **Metamorphic testing** (Chen et al.) — the equivalence-relation-as-oracle discipline; §1's
  quotient (`Empty ≡ Seq([])`, associativity) is a metamorphic relation set.
- **tree-sitter** — generates parsers from `grammar.js`; `tree-sitter test` diff-tests the
  generated parser against golden `test/corpus/*.txt` S-expressions. Direct analogue: golden
  anchor, not majority vote; the corpus IS the specification.
- **rustc / GHC bootstrap** — stage1 (built by stage0) vs stage2 must be byte/behaviorally
  identical; the classic `regen∘regen` fixpoint. 7D takes the *behavioral* (structure-equiv)
  reading over the byte reading, because the byte reading is vacuous under a fixed-const emit.
- **Coq/Lean kernel & the de Bruijn criterion** — a small trusted checker validates a larger
  generated artifact. The hand parser is the trusted anchor; the grammar-generated parser is the
  *checked* artifact until measured parity promotes it.
- **egg / egglog** (Willsey et al., POPL 2021) — equivalence via e-graph congruence;
  `simplify_hir` (`parse-that/rust/regex/src/egraph/mod.rs:143`) already uses this. §1 proposes
  the bbnf relation adopt the same engine (LOCK-2 home).
- **regex-automata** — its TOML conformance suite is the model for the regex oracle's
  golden-anchor corpus.

---

## 6. Named risks and counterexamples

1. **Vacuous parity / coverage hole.** Corpus doesn't exercise a construct → planted mutation
   won't drop parity → false `PROMOTED`. Mitigation: the §4 coverage gate.
2. **Relation over-/under-quotient.** Too aggressive (collapse `Optional`↔`Repeat{0,1}` when
   projection semantics differ) hides a real divergence; too weak (no Seq-flatten) floods
   spurious ones. The relation-id must be versioned and its rewrite set audited; `Optional↔Repeat`
   is off by default.
3. **Recognizer/builder gap (the central bbnf hazard).** Acceptance parity ≠ structure parity. A
   generated recognizer can accept the same language while a hypothetical builder emits a
   different tree. The ledger `role` column exists precisely to forbid promoting a `producer`
   role on `recognizer` evidence.
4. **Shared-canonicalizer bug.** Comparing after `simplify_hir`/`normalize` means a bug in the
   canonicalizer makes both sides agree on a wrong answer. The anchor must be audited
   independently of the canonicalizer.
5. **Perf attribution.** `parse_generated` includes combinator-runtime and `host::lower` cost;
   an un-isolated ratio slanders the grammar plane.

---

## 7. What a prototype must test (to move 7D)

1. **Regex Hir oracle (do first — it's a producer today).** Implement `HirRel-v1` =
   `simplify_hir(parse(p)) ≡ simplify_hir(parse_generated(p))` over the `parse-that/rust/regex`
   tests + a small conformance TOML; report parity%. Plant a mutation in the generated regex
   grammar; confirm parity drops. Isolate parse-stage cyc/B from `host::lower`.
2. **Pin `OracleRel-bbnf-v1`.** Implement the §1 rewrite set as an e-graph normal-form (LOCK-2
   engine) or a normal-form fn; prove confluence on the corpus (normalize twice → fixed point).
3. **bbnf acceptance oracle (rung 1).** Close the three parser gaps (`?w`, `@{}`, `|params|`);
   run hand parser vs generated recognizer over `grammar/`; report accept/reject agreement; plant
   a `Repeat` drop and confirm a fall.
4. **The RED-unknown (rung 2).** Author `-> Span` on ONE structural bbnf rule
   (e.g. `rule = ident "=" expression ";"` → a `RuleNode`), regen, and check (a) does the emitted
   builder reparse that rule into a `GrammarIr`-equivalent node under `OracleRel-bbnf-v1` within
   the `add_expr`+`resolve_refs` budget, and (b) does `regen∘regen` stay byte-identical.
   **Report the exact construct where it cascades** — first suspects: the recursive `expression`
   self-ref through `resolve_refs`, and the `@{}` host-capture atom.
5. **Totality% wiring.** Feed 7C's leaves×branches counter into the ledger's `totality%` column
   for whichever subsystem promotes first; flag any promotion below a named floor.

**Integration seams:** `ir/src/lib.rs:84` `resolve_refs`, `:110` `validate`, `:234` `ExprKind`
(no `Empty`; `Seq([])` witness), `:300` `validate_resolved_refs`;
`grammar/src/lib.rs:1361/1392-1397` (`parse_seq`, the `Seq([])`/singleton witnesses),
`:1404/1481` (`parse_postfix`/`parse_projection`), `:1495` (`parse_atom`), `:420` (`@{`
capture); `parse-that/.../regex/mod.rs:34` `parse_generated` (PRODUCER), `.../regex/host.rs:18`
`lower` + `LowerCtx`, `.../regex/src/egraph/mod.rs:143` `simplify_hir`;
`skinny/crates/codegen/src/runtime_generator.rs:98` (`!has_rule("value")` → general descent),
`general_descent/emit.rs`/`view.rs` (recognizer + lazy view); `xtask/src/regen.rs:37`
`check_target` (single-stage staleness; no `regen-bbnf` target yet), `main.rs:40-47`;
`passes/src/lib.rs:338` `derive_recognizers` (7C counter home); GN-5 §1 Precept 19
(planted-mutation falsification model).
