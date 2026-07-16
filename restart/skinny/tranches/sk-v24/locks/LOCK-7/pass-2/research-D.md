# LOCK-7 pass-2 — Family 7D: THE META-CIRCULAR ORACLE

Self-hosting recast as *verification first*: the bbnf-defined bbnf/regex parsers serve as
differential ORACLES against the hand parsers; promotion to the production parser is gated,
subsystem by subsystem, on measured structural parity plus performance. This family designs the
oracle — the equivalence relation it decides, the pass pivot both sides must share, the host-tier
obligations it must *exclude* from its verdict, and the promotion ledger that reads its output —
and reports the one empirical unknown honestly RED.

The decisive asymmetry, established up front: **the regex tier already has a runnable
meta-circular oracle; the bbnf grammar tier does not.** `parse-that/rust/parse_that/src/regex/
mod.rs:34 parse_generated` is a PUBLIC PRODUCER — `pub fn parse_generated(pattern: &str) ->
Result<Hir, ParseError>` — that runs the self-hosted generated combinator parser
(`regex/generated.rs`, emitted from `bootstrap/regex.bbnf`) and folds its typed AST into `Hir`
via `host::lower` (`regex/mod.rs:65` → `regex/host.rs:19`). A differential oracle
`parse_generated(p) ≟ parse(p)` over a regex corpus is therefore standable TODAY. The bbnf
grammar tier, by contrast, has no producer: skinny's `general_descent` emitter emits
*recognizers* (`general_descent/emit.rs:1` "The recursive-descent recognizer emitter";
`render_rule` at `:259` emits `fn parse_<rule>(...) -> Result<(), ParseError>` that tape-marks
via `mark(tape, offset, kind)` at `:251`), never GrammarIr builders. The bbnf oracle is blocked
on the builder-augment — which is the family's shared empirical unknown (§5).

---

## §1 The equivalence relation — pin the relation-id before any parity%

An oracle that quotes "97% parity" without a stated relation is uninterpretable: the number is a
count of a congruence that must be fixed first. Two subjects, one relation schema.

**Subject A — GrammarIr (bbnf tier).** `crates/ir/src/lib.rs:234 ExprKind` = { `Seq(Vec<ExprId>)`,
`Alt{branches,mode}`, `Repeat{body,min,max}`, `Optional`, `Literal{bytes,case}`, `Regex{pattern}`,
`Ref{name,target}`, `Annotation`, `Project{body,target,ty}`, `Discard{left,right,keep_left}` }.
**Subject B — regex Hir** (`parse-that/rust/regex/src/hir/mod.rs:216`) = { `Empty`, `Literal`,
`Class`, `Look`, `Repetition`, `Concat(Vec<Hir>)`, `Alternation(Vec<Hir>)`, `Capture` }.

The relation is **structural congruence under a fixed normal form** — call it `≡_R`, and the
pinned artifact is `(normal-form spec, hash function) = relation_id`. Its clauses:

1. **Monoid identity — `Empty ≡ Concat([])`.** The directive's named clause. It holds at BOTH
   tiers and is not cosmetic: `regex/host.rs:90` already emits `Hir::Empty` for a bare flag group
   `(?si)`, while a concat that lowered to zero items would be `Hir::Concat([])` — the same
   meaning, two constructors. At the bbnf tier the counterpart is `Seq(vec![]) ≡ ε`; note
   `grammar/bbnf/bbnf.bbnf` writes epsilon three ways (`term = "ε" | "epsilon" | …`), all of which
   the front-end must fold to the empty-Seq identity. Normal form: collapse every zero-arity
   Seq/Concat and every explicit epsilon to a single canonical `Empty` node.
2. **Singleton flattening — `Seq([x]) ≡ x`, `Concat([x]) ≡ x`, `Alt([x]) ≡ x`.** `host.rs`
   already normalizes this on the regex side (`lower_node:36` returns the inner directly when
   `rest.is_empty()`; `:50` returns `items[0]` when `len()==1`). The bbnf-tier front-end must do
   the same, or the two parsers diverge on associativity padding that carries no meaning.
3. **Reference resolution — `Ref{name, target:None}` is NOT `≡_R` `Ref{name, target:Some(id)}`
   until the shared pass runs (§2).** `ExprId`/`RuleId` are arena indices (`ir/src/lib.rs:51
   add_expr` pushes positionally); structural comparison must be a *tree walk that dereferences
   Refs by resolved target*, never a Vec-index or arena-order equality — two parsers that build
   the same grammar in different rule orders produce different `RuleId`s for identical structure.
4. **Span erasure.** `SourceSpan` is construction-path-dependent (every `add_expr` stamps
   `SourceSpan::new(start, cursor)`); it is erased before hashing. Spans are diagnostic payload,
   not structural identity.
5. **Projection/host-annotation identity.** `Project{target, ty}` and `Annotation` compare by
   their string payload after trivia normalization; these are the `->`-carried semantics the type
   plane consumes, and dropping them from `≡_R` would let a builder that discards projections pass
   as "equivalent" — forbidden (a projection-lossy parser is a wrong parser, not an equal one).

**relation_id = a content hash over the post-normalization, span-free, ref-dereferenced tree.**
Pin it (spec + hash fn, committed) BEFORE the first parity table. Parity% is then well-defined:
the fraction of a corpus for which `hash(normalize(generated)) == hash(normalize(golden))`. Two
distinct relations must be tracked and never conflated: **`≡_R` (IR-structural)** is the oracle's
relation; **byte-parity** (regen∘regen identical `.rs`) is a *different, strictly finer* relation
belonging to the fixpoint gate — a parser can be `≡_R`-correct while its emitted bytes differ
(formatting), and byte-identical while `≡_R`-wrong (a fixed-const emit path stamps identical bytes
for any input). Any parity number must name which relation it counts.

---

## §2 The shared-semantic-pass pivot — both sides or neither

The load-bearing design constraint. The hand parser's output and the bbnf-in-bbnf parser's output
must pass through the *same* semantic passes before `≡_R` is evaluated — **`resolve_refs` and
`validate` run on both, or on neither** — because those passes mutate the very fields `≡_R`
inspects. `crates/ir/src/lib.rs:84 resolve_refs` walks every `ExprKind::Ref` and writes
`*target = Some(rule_id)` (`:103`); `:110 validate` calls `validate_resolved_refs` (`:300`) and
asserts every ref resolved. If the oracle compares one side pre-resolve and the other
post-resolve, clause 3 above fails on `target:None` vs `target:Some` for structurally identical
grammars — a false negative that has nothing to do with parser correctness.

The clean rule: **the oracle compares the POST-`resolve_refs`, POST-`validate` normal form of
both sides.** This is also the honest place — resolution is where duplicate-rule and unresolved-
ref errors surface (`ValidationError::{DuplicateRule, UnresolvedRef}`), so a bbnf-in-bbnf parser
that produces a malformed grammar is caught by the *shared* pass, not by a bespoke oracle check.
The pivot generalizes: any future semantic pass (type inference, shape derivation) that both
production and oracle paths would run must run on both oracle inputs or the parity number is
measured under mismatched preprocessing. State the pass set beside the relation_id.

At the regex tier the analogue is already in force by construction: `parse_generated` and `parse`
both terminate in `Hir`, and both feed the *same* downstream `simplify_hir`
(`regex/egraph/mod.rs:143`, cost-guided equality saturation) — so the natural regex oracle can
compare either the raw `Hir` (parser parity) OR the post-`simplify_hir` canonical `Hir` (semantic
parity). Choosing the canonical form is the stronger oracle: it absorbs benign representational
differences (`a{1,1}` vs `a`) the way `resolve_refs` absorbs ref-order differences, and it is the
regex-tier embodiment of "shared pass on both sides or neither." The relation_id must record which
form is counted.

---

## §3 Regex divergences — engine-tier host.rs obligations vs oracle verdicts

On the settled regex substrate (two domain-fit types, grammar-tier `Regex(StringId)` opaque and
engine-tier rich `Hir`, bridged by `simplify_hir`), divergences between the self-hosted parser and
the hand parser fall into two disjoint classes, and the oracle must *verdict the second while
excluding the first*.

**Class 1 — ENGINE-TIER host.rs OBLIGATIONS (excluded from the parity verdict; they are the host
floor, not parser disagreements).** These are context-dependent semantic lowerings the grammar
plane *cannot* express and the recognizer only tokenizes; `regex/host.rs` computes their meaning
from a `LowerCtx { dotall, case_insensitive, unicode }` (`:12`). Enumerated:

- **Inline flag propagation** — `(?si:…)` / `(?si)`. `host.rs:74` saves and restores
  `(dotall, case_insensitive, unicode)` around scoped groups; `apply_flags:472` mutates the ctx.
  The grammar recognizes the flag token `/[simux]+/` (`bootstrap/regex.bbnf` group rule); the
  *effect on downstream nodes* is host-computed. A bare flag group even lowers to `Hir::Empty`
  (`:90`) — pure semantics, no surface.
- **Case folding** — `maybe_case_fold:459` expands `i`-flagged alpha bytes to two-range classes.
- **Dot expansion** — `make_dot:438` is dotall/unicode-dependent (`.` → byte-range 0..=255, or
  Unicode `\0..\u{10FFFF}`, or "not newline").
- **Unicode category ranges** — `\p{Name}` → `unicode::unicode_category_ranges` table
  (`host.rs:293`); a data-table lowering with no grammar representation.
- **Braced-quantifier numeric parse** and **class-escape shorthand expansion** —
  `parse_braced_quantifier:212`, `lower_class_escape:303`, negated shorthand expansion inside
  classes (`lower_char_class:384`).

The oracle CANNOT verdict Class 1 against a "grammar-pure" baseline, because there is no such
baseline: these are exactly the typed host functions the hybrid-grammar-host law admits. They are
the named host floor of the regex subsystem — the totality% denominator's *non*-grammar term
(§4). Correct handling: the oracle compares full `Hir` (which INCLUDES host-lowered results, since
both parsers run the same `host::lower`), so Class 1 is *identical on both sides by construction*
and contributes no divergence — it is excluded not by masking but because both oracle inputs share
the one host implementation. A divergence that appears to be Class 1 is therefore a bug in the fold
wiring, not a legitimate host obligation, and the oracle SHOULD flag it.

**Class 2 — ORACLE VERDICTS (the parity surface).** Structural disagreements the differential
must catch and adjudicate: concat/alternation nesting and order, quantifier `{min,max,greedy}`
(`lower_quantifier:174`), literal byte content, character-class membership and negation, group/
capture structure, anchor placement (`^`/`$` → `Hir::Look`). Here `parse_generated ≟ parse`
disagreement means one parser is wrong; the golden anchor (§4) decides which.

**Record correction (directive-mandated).** `regex/mod.rs:34 parse_generated` is a PUBLIC PRODUCER
returning `Result<Hir>` — parse-then-`host::lower` — NOT a boolean recognizer. A *same-named*
bool-returning `parse_generated` exists in the `generated` module and is re-exported by the
`bootstrap` crate for smoke `assert!`s (`bootstrap/src/lib.rs:22,30`); the two must not be
conflated. The oracle rides the PRODUCER at `regex/mod.rs`, whose output is a comparable `Hir`.
The recognizer variant certifies only acceptance, not structure, and is useless as an oracle.

---

## §4 The promotion ledger — schema, golden anchor, falsification

Promotion of a bbnf-defined parser from *oracle* to *production* is per-subsystem and gated on the
ledger below. It is a committed table (one row per subsystem: `regex`, `bbnf-grammar`, and any
future grammar-defined parser), each column an artifact, not a slogan.

| column | source / definition | promotion condition |
|---|---|---|
| `subsystem` | regex \| bbnf-grammar \| … | — |
| `relation_id` | §1 pinned normal-form + hash fn (committed) | must be non-null; a null relation_id ⇒ the parity% below is uninterpretable and the row is BLOCKED |
| `pass_pivot` | §2 pass set run on both sides (e.g. `resolve_refs;validate` or `simplify_hir`) | identical on both oracle inputs |
| `parity%` | fraction of corpus with `hash(normalize(generated)) == hash(normalize(golden))` under `relation_id` | **= 100%** (structural parity is binary for promotion; <100% names the divergent construct) |
| `totality%` | **sourced from 7C's counter** (leaves × branches behind grammar ÷ total), NOT invented here | reported, with the host floor named (regex floor = the Class-1 host.rs obligations of §3) |
| `perf_ratio` | generated cyc/B ÷ hand cyc/B, cold paired, N≥200, per-block loadavg | ≤ a stated threshold (SPEC profiling standing law) |
| `golden_anchor` | committed reference output = the CURRENT hand parser on a frozen corpus | present and frozen |
| `planted_mutation` | §4b falsification status | the oracle TRIPS on every planted divergence |
| `verdict` | BLOCKED until (parity=100% ∧ perf≤threshold ∧ planted_mutation trips ∧ relation_id non-null) | promotion only on all-green |

**4a — Golden anchor, NOT majority vote.** Parity is measured against a single committed golden
reference — the current hand parser's `≡_R`-normal output on a frozen corpus — never a vote among
≥3 parsers. Correlated bugs (a shared misconception across implementations) would let a majority
ratify a wrong answer; three parsers agreeing certifies nothing about truth. The hand parser is the
oracle's *definition of correct* until promotion; after promotion the roles do not swap silently —
a new golden is re-frozen by explicit owner act. This mirrors the constitution's rejection of
consensus-as-truth and the fixpoint-gate warning that triple-identical bytes certify nothing.

**4b — Planted-mutation falsification (the oracle must be PROVEN able to fail).** Before the ledger
is trusted to pass, inject known divergences and assert the oracle catches each — the pattern the
name-gate gate already uses (`xtask/tests/name_gate_falsification.rs` plants three gate forms and
asserts the check trips, then asserts HEAD passes clean). For 7D: plant, into the *generated* side
only, (i) a swapped `Concat`/`Seq` child order, (ii) a dropped quantifier `greedy` flag, (iii) a
`Empty`↔`Concat([x])` corruption that a naive comparator would miss but `≡_R` normalization must
NOT mask (it masks `Concat([])`, never `Concat([x])`), (iv) a projection dropped from a `Project`
node. Assert parity% falls below 100% for each, then assert the clean tree scores 100%. An oracle
that cannot distinguish a planted divergence certifies nothing — the direct analogue of the
byte-determinism vacuity warning (a fixed-const emit path stamps identical bytes for any input).
The falsification is the ledger's admission gate: no `verdict:PROMOTE` until planted-mutation is
green.

---

## §5 The shared empirical unknown — reported RED

**Question.** Authoring `-> Span`/`-> T` builder projections on bbnf.bbnf's structural rules over
the recognizer emitter: does the emitted parser reparse the bbnf corpus into GrammarIr-`≡_R`-
equivalent structure (within the `add_expr` + `resolve_refs` host budget) while `regen∘regen`
holds byte-identical — or does it cascade, and at which construct?

**Status: RED-unknown, and unseeded by extant evidence.** The reason is structural, not
incidental: skinny's `general_descent` emitter produces **recognizers, not producers.**
`emit.rs:259 render_rule` emits `fn parse_<rule>(bytes, cursor, tape) -> Result<(), ParseError>`;
the leaf path `mark(tape, offset, kind)` (`:251`) tape-marks but constructs no `GrammarIr`. There
is thus **no bbnf-tier analogue of `parse_generated`** — the regex tier's producer exists in
parse-that and is runnable; the bbnf tier's producer does not exist and its round-trip behaviour is
un-probed. Do NOT seed the verdict from the regex tier's success (a different emitter, a different
target type) nor from the recognizer emitter's mere existence. The three known parser gaps that
block routing `bbnf.bbnf` through the emitter at all — `?w`→`->` attachment in `parse_seq`, the
`@{}` atom in `parse_atom` (`grammar/src/lib.rs:1495`), and `|params|` closures — are visible
directly in `bbnf.bbnf` (`term = … | "@{" , rhs ?w , "}"`; `?w` throughout; rule-level closures)
and are the leading cascade candidates.

Corroborating facts (do not extrapolate beyond): skinny's `xtask` has **no `regen-bbnf`,
`fixpoint`, or `bbnf.bbnf` target** (grep of `crates/xtask/src/` returns nothing); `bbnf.bbnf`
lives in the OUTER `grammar/bbnf/` tree, unwired to any skinny regen. The bbnf meta-circular oracle
is therefore UNBUILT in skinny — it is a design, not a running artifact — whereas the regex
meta-circular oracle is a running artifact in the *other* repo.

**What a prototype MUST test to move this family.**

1. **Regex tier (immediately runnable, low risk).** Stand up `parse_generated(p) ≟ parse(p)` over
   the regex corpus. Pin the `Hir` relation_id (Empty≡Concat([]), singleton-flatten, span-free;
   §1). Decide raw-`Hir` vs post-`simplify_hir` as the counted form (§2). Confirm Class-1 host
   obligations (§3) are identical on both sides by shared `host::lower`. Run the four planted
   mutations (§4b) and prove the oracle fails-loud. Emit the first ledger row with a real parity%
   and the 7C-sourced totality% (host floor = the `LowerCtx` obligation set).

2. **bbnf tier (BLOCKED / the RED experiment).** Prototype a builder-augment over
   `general_descent` that emits a GrammarIr *producer* for `bbnf.bbnf` — driving the
   `!has_rule("value")` branch (`runtime_generator.rs:98`), NEVER `json_sink_direct`. Then test:
   (a) does `reparse(bbnf.bbnf) ≡_R hand_parse(bbnf.bbnf)` under the pinned relation_id, after the
   shared `resolve_refs`/`validate` pivot on both sides; (b) does it stay within the measured
   `add_expr`+`resolve_refs` host budget; (c) is `regen∘regen` byte-identical AND **non-vacuous** —
   the emit path must be shown input-sensitive (perturb the source, observe the bytes change),
   because a fixed-const path stamps identical bytes for any input and certifies nothing. If it
   cascades, NAME the construct — the priors are `@{}`, `|params|` closures, `?w` attachment. Until
   this prototype runs, the bbnf ledger row is `verdict:BLOCKED, parity%:RED`.

---

## §6 Named risks and counterexamples

- **R1 — parity% quoted under an unpinned relation.** The failure mode the whole family guards:
  a number reported before relation_id is committed is meaningless. Counter: the ledger row is
  BLOCKED while relation_id is null (§4). Any parity% in prose must cite its relation_id.
- **R2 — the oracle masks a real bug via over-eager normalization.** If `≡_R` folds too
  aggressively (e.g. treats `Concat([x])` as `Empty`), it hides divergences. Counter: the planted
  `Empty`↔`Concat([x])` mutation (§4b-iii) MUST trip; a normalization that masks it is over-eager
  and rejected. Normalization folds only the monoid identity `Concat([])`, never non-empty concats.
- **R3 — majority-vote creep.** Under schedule pressure the temptation is "N parsers agree ⇒
  correct." Counter: golden_anchor is a single frozen reference (§4a); correlated bugs make
  consensus worthless. The ledger has no vote column by design.
- **R4 — the regex oracle's success is transplanted onto the bbnf claim.** The regex producer
  exists; the bbnf producer does not. Counter: §5 forbids seeding the bbnf verdict from the regex
  tier — different emitter (recognizer vs the parse-that combinator generator), different target
  (`GrammarIr` vs `Hir`), un-probed round-trip. The bbnf row stays RED until its own prototype
  runs.
- **R5 — Class-1/Class-2 misclassification.** A parser bug in the fold wiring can masquerade as a
  legitimate host obligation and be wrongly excluded from the verdict. Counter: because both oracle
  inputs share ONE `host::lower`, any true Class-1 divergence is impossible by construction; an
  apparent one is a Class-2 bug and the oracle flags it (§3). A host floor that diverges between
  the two sides is a wiring defect, not an obligation.
- **R6 — byte-parity conflated with IR-parity.** Reporting regen∘regen byte-identity as if it were
  structural correctness (or vice versa). Counter: §1 names them as two distinct relations; the
  oracle's relation is `≡_R` (IR-structural); byte-parity is the separate, finer fixpoint relation.
  A row must state which it reports; both are legitimate, neither substitutes for the other.

---

## §7 Prior art

- **Differential / metamorphic parser testing** — the meta-circular oracle is a differential
  oracle in the Csmith/EMI lineage: a second independent implementation (here, self-hosted) as the
  test oracle for the first, with structural (not byte) equivalence as the relation. The golden-
  anchor-not-vote discipline is the standard defense against correlated-implementation bias.
- **rustc bootstrap staging** — stage-0 (prebuilt) vs stage-1 (self-compiled) with a fixpoint that
  the two agree; the analogue here is IR-equivalence of hand vs self-hosted parse, and the
  cautionary lesson that byte-identity of generated output can be vacuous if the emit path is
  input-insensitive.
- **egg / egglog equality saturation** — `simplify_hir` (`regex/egraph/mod.rs:143`) is the regex
  tier's canonicalizer; running it as the shared pass on both oracle inputs (§2) makes the oracle
  compare canonical forms, absorbing benign representational churn exactly as e-graph extraction
  canonicalizes before comparison. `Empty ≡ Concat([])` is a monoid-identity rewrite rule of the
  same character the e-graph already applies.
- **tree-sitter corpus tests** — S-expression structural comparison with span-erased, order-
  significant tree equality is precisely `≡_R` clause 4 (span erasure) + clause 1–2 (monoid /
  singleton) applied to concrete syntax; the frozen-corpus golden-file discipline is the ledger's
  `golden_anchor`.
- **GHC's `-ddump-simpl` / core-lint** — a shared semantic pass (Core Lint) run on both sides
  before comparison is the pivot pattern of §2: never compare pre-pass output on one side to
  post-pass on the other.
