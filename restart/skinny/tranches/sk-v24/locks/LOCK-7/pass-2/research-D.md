# LOCK-7 research-D — the META-CIRCULAR-ORACLE (family 7D)

Self-hosting as verification first: the bbnf-defined bbnf grammar and the bbnf-defined regex
grammar are run as differential ORACLES against the hand parsers, and promotion of any
generated parser to production status is gated on a *pinned* equivalence relation plus a
measured performance floor, subsystem by subsystem. The family's whole load-bearing act is to
make "the generated parser agrees with the hand parser" a claim that can be *quoted as a
percentage* — impossible until the relation under which agreement is judged is fixed, named,
and shown able to reject a wrong parse. This report codifies that relation, partitions the regex
divergences into engine-tier obligations versus oracle verdicts, drafts the promotion ledger,
and states what a prototype must measure to move the family.

## 1. The mechanism: a two-tier differential oracle over a pinned relation

Differential (pseudo-oracle) testing — feed identical input to two independent implementations,
flag any output divergence as a fault — is the classical technique (McKeeman 1998; Csmith/GCC;
Solsmith; the Cranelift CLIR fuzzer; the Kotlin evolutionary differential harness). The
meta-circular twist is that the two implementations are *the same grammar in two forms*: the hand
parser (`grammar/src/lib.rs`, the production path) and the generated parser (emitted by the one
generator from `bbnf.bbnf` / `regex.bbnf`). Divergence is therefore either a generator bug or a
grammar-authoring gap — never a spec ambiguity, because both sides claim to realize the *same*
grammar.

Two oracle TIERS exist and must be named separately because only one is available today:

- **Tier-R (recognizer-acceptance parity).** For every corpus document `d`, the generated
  recognizer's accept/reject verdict equals the hand parser's. Available *now*: the general-
  descent emitter emits exactly one `parse_<rule>` recognizer per rule (`general_descent/mod.rs`),
  and the regex side exposes a bool recognizer (`generated.rs:2288 pub fn parse_generated(pattern)
  -> bool`). No builder augment required. Tier-R catches every set-of-accepted-strings divergence
  and is the correct first gate.

- **Tier-S (structure-equivalence parity).** For every `d`, the structure the generated parser
  BUILDS equals — under relation R (§2) — the structure the hand parser builds (`GrammarIr` for
  bbnf; `Hir` for regex). Tier-S catches divergences invisible to Tier-R: mis-associated
  alternation arms, dropped projections, wrong quantifier bounds. On the bbnf side Tier-S needs
  the builder augment (the shared open empirical question, §6); on the regex side it is available
  today, because `parse_generated` is a *producer*, not a recognizer (§3).

The family's deliverable is neither tier alone but the LEDGER (§4) recording, per subsystem,
which tier is green, at what parity%, at what totality% (sourced from 7C's counter), and at what
performance ratio — the promotion gate.

## 2. The equivalence relation R, codified and pinned

A parity% is meaningless until R is fixed, because the two sides *already normalize differently
in different places* and a naive `==` on the built structures reports spurious divergence. R is
the smallest normal-form projection `N(·)` such that two structures are equivalent iff
`N(a) == N(b)` under the derived structural equality (`ir/src/lib.rs:233` derives `PartialEq, Eq`
on `Expr`; the regex `Hir` derives structural equality). R has three clauses, each grounded in
code that already performs the normalization on ONE side — which is precisely why it must be
applied to BOTH:

- **R1 — the `Empty ≡ Concat([])` clause (unit/empty canonicalization).** The bbnf `ExprKind`
  (`ir/src/lib.rs:234`) has NO `Empty` variant; the empty match is `Seq(vec![])`
  (`grammar/src/lib.rs:1394`, the `children.len()==0` arm), and a singleton sequence is collapsed
  to its child (`grammar/src/lib.rs:1400`, the `len()==1` arm returns `children[0]`). The regex
  `Hir` DOES have `Hir::Empty`, and `host.rs` collapses singletons inline (`host.rs:44`
  `Alternation([x]) -> x`, `host.rs:54` `Concat([x]) -> x`) and lowers a bare flag group to
  `Hir::Empty` (`host.rs:89`). R1 fixes the normal form: `Seq([]) ≡ Empty ≡ Concat([])`,
  `Seq([x]) ≡ x`, `Alt([x]) ≡ x`, recursively, on BOTH sides before comparison. Without R1 the
  hand parser's already-collapsed `x` and a builder-augment's un-collapsed `Seq([x])` diverge for
  a reason that is not a bug. Pin: R applies the collapse as a bottom-up rewrite to a fixpoint
  (idempotent — the collapse cannot re-introduce a singleton), so N is total and deterministic.

- **R2 — the shared-semantic-pass pivot (`resolve_refs`/`validate` on both sides or neither).**
  The hand path calls `grammar.resolve_refs()` (`grammar/src/lib.rs:33`), which MUTATES every
  `ExprKind::Ref { target: None }` into `target: Some(rule_id)` (`ir/src/lib.rs:84`). A generated
  builder that has not run `resolve_refs` carries `target: None` on every reference — a total
  divergence on every `Ref` node that is again not a bug. R2 fixes: R compares structures at the
  SAME semantic stage — either both post-`resolve_refs`+`validate` (`ir/src/lib.rs:110`) or both
  pre. The pivot is normative because `resolve_refs` is the only host-bound cross-rule build
  mechanism the census names (alongside `add_expr` append, `ir/src/lib.rs:51`); making it shared
  means the oracle tests the *parser*, not the *linker*. The stage is pinned in the relation-id.

- **R3 — projection/`->` preservation.** `bbnf.bbnf`'s structural rules carry `-> Span`
  projections (`identifier = /…/ -> Span`, `literal … -> Span`, `regex … -> Span`,
  `comment … -> Span`), lowered to `ExprKind::Project { body, target, ty }`
  (`grammar/src/lib.rs:1481`). R3 forbids `N` from erasing `Project` wrappers (the standing
  no-value-discard law): `Project` is structural, compared field-for-field. This clause makes
  Tier-S strictly stronger than Tier-R — a generated parser that recognizes but drops a projection
  passes Tier-R and fails Tier-S at R3.

**The relation-id.** `R = (R1-collapse ∘ R2-stage{post-resolve_refs, post-validate} ∘
R3-projection-preserving)`, stamped as a versioned constant `ORACLE_REL_V1` committed beside the
oracle. Every quoted parity% cites `ORACLE_REL_V1`; changing R mints `V2` and invalidates prior
percentages. This discipline stops "97% parity" from silently meaning three different things
across passes.

The extant JSON parity oracle (`bbnf-bench/src/parity.rs:23 assert_parity`) is the working
template for R at the tape layer: it compares (a) the offset stream (`tape().offsets()`,
`flag_cursors()`, `flag_values()`), (b) a payload-zero invariant (`write_count()==0`,
`allocation_count()==0` — the constitution's zero-runtime-cost clause, testable), and (c) a
CANONICAL string with object-key sorting (`to_canonical_string`). The key-sorting *is* an R1-style
normal form applied to both sides — evidence the pattern is already load-bearing and generalizes.

## 3. Regex divergences: engine-tier `host.rs` obligations vs oracle verdicts

On the SETTLED regex substrate (grammar tier `Regex(StringId)` opaque; engine tier rich `Hir`;
bridged by `simplify_hir`), a divergence between `parse_generated` and the primary `parse()` is
NOT automatically an oracle verdict. Partition:

- **Engine-tier `host.rs` OBLIGATIONS (deterministic folds — fix the host, do not bank the gap).**
  `host::lower` (`regex/mod.rs:57`, `host.rs:19`) is a pure recursive fold whose every clause is a
  *total function of the AST*, carrying no verdict authority: `lower_escape` (`host.rs:248`,
  `\d`→class), `lower_class_escape` (`host.rs:303`), `lower_char_class` (`host.rs:357`),
  `lower_quantifier` (`host.rs:174`, greedy/lazy/bounds), `apply_flags` (inline `(?si)` scoping,
  `host.rs:89` `group_1 -> Hir::Empty`). If the generated-parser path and the primary path
  disagree on how `\d` classifies, on a class-range endpoint, or on a quantifier bound, that is a
  `host.rs` bug — an ENGINE-tier obligation to make the fold agree — never a parity gap the ledger
  may bank. These clauses are the regex analog of bbnf's `resolve_refs`: shared semantics R2 must
  run on both sides or neither (here both sides already terminate in `host::lower`, so the pivot
  is satisfied by construction and the oracle compares `Hir` post-lower).

- **Oracle VERDICTS (genuine parser divergence — the ledger's domain).** What the oracle
  legitimately adjudicates is *structure the parser recovered before the fold*: whether an
  alternation arm attached to the right operand, whether a group's extent is correct, whether a
  quantifier bound the intended atom. These are `Hir`-shape divergences that survive R1/R3
  canonicalization and are attributable to the PARSER, not the shared fold. This is the verdict
  domain quoted as regex parity%.

**Record correction — `parse_generated` is a PRODUCER, and the namespace hazard.** The
module-level `regex::parse_generated` (`regex/mod.rs:34` → `parse_generated_with:39`) parses via
`generated::RegexParser::regex()` and then `host::lower(&ast, …)` to return a full `Hir` — it is a
public PRODUCER of the engine's rich type, not a recognizer. Any record calling it a recognizer is
corrected here. The hazard: a SECOND, distinct `generated::parse_generated` (`generated.rs:2288`)
returns `bool` — a pure recognizer. The two share a name across modules. The Tier-S oracle MUST
bind to the producer (`regex/mod.rs:34`); the Tier-R oracle binds to the bool
(`generated.rs:2288`). A prototype that imports the wrong `parse_generated` silently degrades
Tier-S to Tier-R and reports a vacuous 100% structure parity. Name this in the ledger's binding
column.

## 4. The promotion-ledger schema

One row per (subsystem × oracle tier). Subsystems: `bbnf` (the self-host), `regex` (the
regex-grammar bootstrap), and each production grammar the oracle covers (json/css/sheets/…).

| column | source | semantics |
|---|---|---|
| `subsystem` | — | e.g. `bbnf`, `regex`, `json` |
| `tier` | §1 | `R` (recognizer-accept) or `S` (structure-equiv) |
| `rel_id` | §2 | `ORACLE_REL_V1` — the relation the parity% is quoted under |
| `binding` | §3 | exact producer symbol (`regex/mod.rs:34` vs the `bool` twin) — the anti-vacuity pin |
| `parity_pct` | oracle run | `#agree / #corpus` under `rel_id`, golden-anchored (§5) |
| `totality_pct` | **7C counter** | fraction of (leaves × branches) behind the grammar vs host fns |
| `perf_ratio` | cold paired PMU | generated-parser cyc/B ÷ hand-parser cyc/B (ratio certified, absolute directional) |
| `fixpoint` | regen∘regen | byte-identical stamp of the generated `.rs` (7A's subject) |
| `state` | derived gate | `BLOCKED / ORACLE-GREEN / PROMOTED` |

`state = PROMOTED` requires: `parity_pct == 100` at Tier-S under a stated `rel_id`; `perf_ratio ≤
1.0` (the generated parser is not slower than the hand parser it replaces — else it is an oracle,
never a production parser); `fixpoint` holding; and `totality_pct` at or above the subsystem's 7C
floor. `ORACLE-GREEN` is Tier-S 100% with `perf_ratio > 1.0` — a trusted oracle, not yet the
production parser. `BLOCKED` is anything short of Tier-S 100%. `totality_pct` is *not* a promotion
gate (a subsystem may promote its parser while host fns still cover context-dependent leaves) but
IS the 7C-sourced measure the gestalt reads to see how much of each subsystem is genuinely
grammar-driven.

## 5. Golden-anchor, not majority-vote; planted-mutation falsification

**Golden-anchor.** Majority-vote across three parser instances is refused for the reason Diverse
Double-Compiling (Wheeler, 2009, *Fully Countering Trusting Trust through Diverse Double-
Compiling*) makes precise: instances sharing a generator share its bugs and vote together for the
wrong answer — the trusting-trust fixpoint. The directive's own warning is the same shape: a
fixed-const emit path stamps identical bytes for any input, so triple-identical certifies nothing.
The oracle's truth is anchored to (a) the HAND parser as the independent reference implementation
(it does not share the generator's code path — DDC's diverse second compiler realized as the hand
parser), and (b) a committed GOLDEN snapshot of `N(GrammarIr)` / `N(Hir)` for each corpus
document, minted once and regenerated only under the clean-regen discipline. Parity% is measured
against the golden, never against a peer generator instance.

**Planted-mutation falsification (the oracle must be shown able to fail).** A gate that cannot
fail is a defect. Before any green is trusted, plant known divergences and assert the oracle FLAGS
each: (i) swap a quantifier greedy→lazy in a copy of the generated regex output and assert Tier-S
at R3-preservation flags it; (ii) drop one `-> Span` projection from a copy of the bbnf builder
output and assert R3 flags it; (iii) mis-associate an alternation arm and assert R1
canonicalization does NOT mask it (proving R1 collapses only units, never re-associates); (iv)
plant the §3 namespace hazard — bind the `bool` recognizer where the producer was expected — and
assert the ledger's `binding` check rejects the vacuous 100%. Only after all four planted forms
trip is a subsequent clean run trusted. This mirrors the name-gate falsification discipline
(`docs/tranches/sk-v24/GN-5.md §4 W3`): prove the gate trips on planted breaches, then trust its
pass on HEAD.

## 6. The shared empirical question — where the family stands or falls

Tier-S on the bbnf subsystem requires the generated parser to BUILD `GrammarIr`, but the general-
descent emitter emits RECOGNIZERS (`general_descent/mod.rs`), and its regex leaves fall back to a
delimiter-scan Span projection ("typed leaves deferred to `Span`", the emitter's own TODO in
`general_descent/mod.rs`). Whether authoring `-> Span`/`-> T` builder projections over
`bbnf.bbnf`'s structural rules (which already carry `-> Span` at
`identifier`/`literal`/`regex`/`comment`) makes the emitted parser reparse the corpus into
`ORACLE_REL_V1`-equivalent `GrammarIr` — within the `add_expr`+`resolve_refs` host budget — while
`regen∘regen` holds byte-identical is UNSEEDED and must be prototyped, driving the
`runtime_generator.rs:98 !has_rule("value")` general-descent branch, never `json_sink_direct`. If
it round-trips, Tier-S promotes bbnf; if it cascades, the cascade construct is the named negative
result, Tier-R remains the bbnf oracle, and the builder gap is a 7C host-fn line. The regex
subsystem is NOT so blocked: `parse_generated` (`regex/mod.rs:34`) already produces `Hir`, so
regex Tier-S is available immediately and is the family's first promotable subsystem.

## 7. Integration seams (path:line)

- Relation R normal form: `grammar/src/lib.rs:1394,1400` (bbnf singleton/empty collapse),
  `regex/host.rs:44,54,89` (regex singleton collapse + `Hir::Empty`), `ir/src/lib.rs:233`
  (`PartialEq` on `Expr`).
- R2 pivot: `ir/src/lib.rs:84 resolve_refs`, `:110 validate`, `:300 validate_resolved_refs`,
  `:51 add_expr`; hand-path invocation `grammar/src/lib.rs:33`.
- R3 projection: `grammar/src/lib.rs:1481 parse_projection` → `ExprKind::Project`
  (`ir/src/lib.rs:263`); `bbnf.bbnf` `-> Span` sites.
- Regex producer vs recognizer: `regex/mod.rs:34` (producer→`Hir`), `regex/mod.rs:57 host::lower`,
  `regex/host.rs:{174,248,303,357}` (engine-tier obligations), `generated.rs:2288` (bool twin).
- Emitter branch to drive: `codegen/src/runtime_generator.rs:98 !has_rule("value")` →
  `emit_general_descent` → `general_descent/mod.rs` (recognizers today).
- Extant oracle template: `bbnf-bench/src/parity.rs:23 assert_parity` (3-layer relation, the R
  prototype); `bbnf-bench/src/gate.rs`, `bbnf-bench/src/bin/gate.rs`.
- Ledger falsification kin: `docs/tranches/sk-v24/GN-5.md §4 W3` (the name-gate falsification
  pattern).

## 8. Prior art

- **Differential/pseudo-oracle testing** (McKeeman 1998; Csmith/GCC; Solsmith for Solidity; the
  Cranelift CLIR liveness-driven fuzzer; the Kotlin evolutionary differential harness): N
  independent implementations, identical input, flag divergence — the oracle's core method.
- **Diverse Double-Compiling** (Wheeler, 2009): why a self-referential fixpoint certifies nothing
  without an independent second path — the reason the golden anchor is the HAND parser, not a peer
  generator instance.
- **tree-sitter corpus tests** (S-expression golden per input) and **rustc `ui` stderr goldens**:
  the golden-snapshot discipline the ledger's anchor column formalizes.
- **`regex-automata` differential fuzzing** (the regex crate cross-checks PikeVM vs backtracker vs
  lazy DFA vs dense DFA on shared haystacks): the exact Tier-S/Hir-parity shape, cross-engine —
  the model for the regex subsystem's oracle.
- **GHC `-ddump-parsed-ast`, rustc `-Zunpretty=…`**: dumping parsed structure for differential
  comparison — the Tier-S build-and-compare surface.

## 9. Risks and counterexamples

- **Vacuous parity via wrong binding.** Importing `generated.rs:2288`'s `bool` twin where the
  `regex/mod.rs:34` producer was meant reports a meaningless 100% structure parity. Guarded by the
  ledger `binding` column + planted form (iv).
- **R over-normalizes and masks a bug.** If R1 ever re-associated (not just collapsed units) it
  would hide a real mis-association. Guarded by planted form (iii): R1 must be provably a
  unit-collapse fixpoint, never a re-association.
- **Stage skew (R2 breach).** Comparing a post-`resolve_refs` hand structure against a
  pre-`resolve_refs` generated structure yields a total `Ref`-node divergence mistaken for a
  parser bug. Guarded by pinning the stage in `rel_id`.
- **Builder augment cascades (§6).** If `-> T` builder projections do not round-trip on
  `bbnf.bbnf`, bbnf Tier-S is unreachable and the family delivers Tier-R for bbnf + Tier-S for
  regex only — smaller but honest; the cascade construct is the named negative result, not a
  silent shortfall.
- **Fixpoint vacuity (7A boundary).** The ledger's `fixpoint` column must guard against a
  fixed-const emit path stamping identical bytes for any input; the oracle's parity% is the
  non-vacuous certificate the byte stamp alone is not.

## 10. What a prototype must test to move family 7D

1. **Pin and implement `ORACLE_REL_V1`** as `N(GrammarIr)` and `N(Hir)` normal forms (R1+R2+R3),
   with the four §5 planted mutations proven to trip.
2. **Regex Tier-S now:** run `regex/mod.rs:34 parse_generated` vs primary `parse()` over the regex
   corpus, report `parity_pct` under `ORACLE_REL_V1`, golden-anchored — the first promotable
   ledger row; confirm every divergence is either a `host.rs` obligation (fix) or a parser verdict
   (bank).
3. **bbnf Tier-R now:** general-descent recognizer accept/reject vs hand parser over the bbnf
   corpus — the immediately-available bbnf row.
4. **bbnf Tier-S probe (the unseeded question):** author `-> Span`/`-> T` builders over
   `bbnf.bbnf`'s structural rules, drive the `runtime_generator.rs:98` branch, and measure whether
   the emitted parser reparses the corpus into `ORACLE_REL_V1`-equivalent `GrammarIr` within the
   `add_expr`+`resolve_refs` budget WHILE `regen∘regen` stays byte-identical — or name the
   construct at which it cascades.
5. **Emit the ledger** for these rows (parity%, 7C totality%, perf ratio, fixpoint, state) and
   confirm `state` transitions (`BLOCKED`→`ORACLE-GREEN`→`PROMOTED`) are gate-derived, not
   asserted.
