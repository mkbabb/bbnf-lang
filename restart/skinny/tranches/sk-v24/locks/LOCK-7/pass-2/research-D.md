# LOCK-7 pass-2 — Family 7D: META-CIRCULAR-ORACLE

Self-hosting as verification first. The bbnf-defined bbnf grammar and the bbnf-defined regex
grammar are not (yet) the production parsers — they are **differential oracles** run against the
hand parsers over a corpus, subsystem by subsystem, and promotion to production is gated on
measured parity plus performance. Three parsers are in play, and the family's whole discipline is
keeping their equalities distinct and their comparison relations pinned.

## The three parsers and the two equalities

- **Golden hand parser** — `parse_grammar` (`skinny/crates/grammar/src/lib.rs:29`) folds source →
  `GrammarIr`. This is the production reference and the oracle's *golden anchor*.
- **Self-hosted bbnf parser** — the recognizer emitted from `grammar/bbnf/bbnf.bbnf` through the
  shape-dispatched `general_descent` emitter (`skinny/crates/codegen/src/general_descent/`,
  1206 LOC: `emit.rs:452`, `regex_first.rs:230`, `view.rs`, `mod.rs`). `bbnf.bbnf` has no `value`
  rule, so it routes through the `!has_rule("value")` branch (`runtime_generator.rs:98`) — the
  branch the prototype MUST drive, never `json_sink_direct`. Today this branch emits ONE
  `parse_<rule>` **recognizer** per rule, not a `GrammarIr` builder.
- **Self-hosted regex parser** — `parse_generated` (parse-that `rust/parse_that/src/regex/mod.rs:34`).
  **Correct the record:** this is a *producer*, not a recognizer — it parses via the generated
  combinator parser then folds to `Hir` via `host::lower` (`regex/host.rs:19`). The regex oracle is
  therefore producer-vs-producer.

Two equalities must never be conflated, because each answers a different question:

1. **Byte equality** — `EmittedSource::check_dir` (`skinny/crates/codegen/src/lib.rs:68`) compares
   emitted `.rs` against the committed tree byte-for-byte (`actual != *source`) and rejects stray
   `GENERATED_HEADER` files. This is the **fixpoint** subject for `regen∘regen`, the STAGE-0 gate.
2. **GrammarIr-equivalence** — a structural relation over parsed IR. This is the **oracle** subject;
   it is what any parity% quotes.

Byte-identity of generated code does **not** prove parser parity (two structurally different
`GrammarIr`s can emit byte-different, both-correct code; spans and rule order alone perturb bytes),
and parser parity does **not** prove byte-identity. The ledger keeps them in orthogonal columns.

## Q1 — The equivalence relation, and pinning the relation-id

The comparison subject cannot be raw `PartialEq` on `Expr`: `Expr` carries `SourceSpan`, which
differs by construction, and `Ref{target}` carries a `RuleId` that is an **index assigned by parse
order** (`resolve_refs`, `ir/lib.rs:84`, builds a `name → RuleId` map at `:85`). Two parsers that
emit rules in a different order produce different `target` ids for identical grammars. Naive
equality fails on both axes.

Codify a canonical relation `≡_G` (call it **OREL-1**) over `GrammarIr`, pinned before any parity%
is quoted:

- **Empty is already normal.** Skinny's `ExprKind` (`ir/lib.rs:234`) has **no `Empty` variant** —
  the variants are `Seq, Alt, Repeat, Optional, Literal, Regex, Ref, Annotation, Project, Discard`.
  An empty concatenation is `Seq(vec![])`. The directive's `Empty ≡ Concat([])` is thus satisfied
  at **zero cost**: there is one representation, no dual node to reconcile. (The only `Empty` in the
  IR is `DirectBuildSource::Empty` at `ir/lib.rs:479`, a payload-source tag, unrelated.) OREL-1's
  base case is the normal form by construction.
- **Structural units collapse:** `Seq([x]) ≡ x`, `Alt{[x]} ≡ x`; flatten nested same-mode
  `Seq`/`Alt`.
- **Identity axes projected out:** ignore `SourceSpan`; ignore `ExprId` identity (compare by
  structural fold, not by id); resolve `Ref` by **name**, never by `RuleId` index.
- **The shared-semantic-pass pivot.** `AltMode` (`Dispatch` vs `Speculative`) is *derived* by a
  pass, not authored; `Discard{left,right,keep_left}` is a *pass rewrite* of the `<<`/`>>`/`-`
  operators (`bbnf.bbnf:44` `binary_operators`). `resolve_refs` rewrites `Ref{target:None}` →
  `Ref{target:Some}`. So the relation MUST run `resolve_refs` + `validate` (`ir/lib.rs:84`, `:110`)
  on **both sides or neither** — never one — and compare **pre-`derive_altmode`/pre-`Discard`** (or
  run identical passes both sides). Comparing post-pass conflates parser divergence with pass
  divergence.

**Two relation strengths, staged:**

- **OREL-1-ACCEPT (weak, testable NOW).** Language equivalence: both parsers accept/reject the same
  corpus strings, ignoring built structure. Cheap; testable against today's *recognizer* emitter as
  it stands; catches the dominant divergence class (a parse succeeding on one side, failing on the
  other). This is the promotable oracle for the recognizer emitter.
- **OREL-1-STRUCT (strong, RED-unknown).** Full `GrammarIr` structural equivalence mod the
  normalization above. Requires the emitter to *build* `GrammarIr` (the `->`-projection
  builder-augment). This is the shared empirical question below.

`Discard` is where recognizer-vs-builder asymmetry bites: a recognizer builds no `Discard` node at
all, so OREL-1-STRUCT is undefined against the current emitter — only ACCEPT is. Stage the oracle:
ACCEPT gates recognizer promotion now; STRUCT gates the builder-augment when it lands.

An **alternative to hand-normalization** (Constitution's pluggable-components, ties LOCK-2): put
both `GrammarIr`s into an e-graph under the structural axioms `Seq([x])=x`, `Alt([x])=x`,
`Seq([])=Empty`, `flatten`; equality is same-e-class after saturation. `≡_G` becomes rewrite data,
not hand code. The `egraph`/`egraph-derive` crates are in-tree.

## Q2 — Regex divergences: engine-tier host.rs obligations vs oracle verdicts

The settled regex substrate (two domain-fit types: grammar-tier opaque `Regex(pattern:String)` —
skinny stores the raw pattern in `ExprKind::Regex` — and engine-tier rich `Hir`, bridged by
`simplify_hir`) fixes the oracle subject: the regex oracle compares canonical **`Hir`**, and the
grammar-tier opaque form never enters the comparison.

`host::lower` (`regex/host.rs`, 524 LOC) is the fold; its lowering choices are **engine-tier
obligations**, not oracle verdicts, and the oracle must normalize past them:

- inline flag folding `(?si)` / scoped `(?si:...)` (`host.rs:472` `apply_flags`, `:69` group_0 /
  `:87` group_1),
- unicode category expansion `\p{...}` → range set (`host.rs:293` `unicode_category_ranges`),
- class-escape desugaring `\d \w \s` → ranges (`host.rs:303` `lower_class_escape`),
- quantifier normalization (`host.rs:174` `lower_quantifier`).

Because **both** `parse()` and `parse_generated()` fold through `host::lower`, these obligations are
self-cancelling and can never surface as a verdict — provided the comparison is taken on the
`simplify_hir`-canonical `Hir`, not the raw pre-simplify tree (LOCK-5 owns the canonical form).

**Oracle verdicts** (genuine, promotion-blocking) are: a pattern one side accepts and the other
rejects — backreference `\1` (`regex.bbnf` `back_reference`), lookbehind `(?<=...)`, class
subtraction — and any `Hir` that differs **after** `simplify_hir`. The regex oracle subject is thus
`simplify_hir(parse(p)) ≡_Hir simplify_hir(parse_generated(p))` over a pattern corpus, with the
host obligations folded into both sides by construction.

## Q3 — The promotion-ledger schema

Promotion is subsystem-by-subsystem; the ledger is a table, one row per subsystem
(`bbnf-grammar-parser`, `regex-parser`, each of json/css/csv/toml/sheets recognizers):

| subsystem | oracle-relation-id | corpus-id / N | parity% | totality% (7C) | perf cyc/B (cold paired, vs hand) | golden-anchor | status |

- **oracle-relation-id** — `OREL-1-ACCEPT` | `OREL-1-STRUCT` | `OREL-Hir`. Parity% is meaningless
  without it; pin the id in the same cell.
- **parity%** — fraction of corpus where B `≡_relation` A. Promotion requires **100%**: a
  differential parser divergence is a correctness bug, not a percentage to optimize; any divergence
  blocks and the row names the first divergent corpus item + construct.
- **totality%** — **imported from 7C's leaves×branches counter**, not recomputed here, so 7C and 7D
  quote one number. It scores how much surface sits behind grammars vs host.
- **perf cyc/B** — promotion gated on parity=100% **and** perf ≤ threshold, cold per-parse paired
  PMU per the SPEC §0 standing law (refutation-compliant: no warm benches, no fold-over-cursor
  claims).
- **golden-anchor — NOT majority-vote.** The hand parser (`parse_grammar`) is the single golden
  reference; B is compared against A, never against a vote of parsers. A three-way vote would let
  two buggy generated parsers outvote the correct reference. The anchor is the frozen hand output on
  the committed corpus, stored as a golden fixture (mirrors the `bbnf-oracle` `round_trip` fixtures
  born GN-5.W1).
- **status** — `RED` (parity<100 or perf-fail) | `GREEN` (parity=100 ∧ perf-pass ∧ two consecutive
  clean passes) | `PROMOTED` (self-host replaces the hand parser for that subsystem).

**Planted-mutation falsification (the gate must be able to fail).** Inject a known divergence into
the emitted parser (drop `?` optional handling; flip a class range) and assert the oracle reports
parity<100 and names the first divergent item + construct. This mirrors GN-5.W3's
`name_gate_falsification.rs` pattern — a gate that cannot fail is a defect (Constitution vii,
SPEC §0). Home: `bbnf-oracle/tests/oracle_falsification.rs`, inside the oracle fleet crate.

## The shared empirical question — RED-UNKNOWN

Authoring `-> Span`/`-> T` builder projections on bbnf's structural rules over the recognizer
emitter: does the emitted parser reparse the corpus into `GrammarIr`-equivalent (OREL-1-STRUCT)
structure within the `add_expr`+`resolve_refs` host budget while `regen∘regen` holds byte-identical
(`check_dir`, `codegen/lib.rs:68`) — or does it cascade, and at which construct? **RED-UNKNOWN,
unseeded.** The recognizer emitter emits recognizers, not builders; whether a `->`-augment
round-trips into `GrammarIr` builders is untested and must NOT be presumed from the legacy emitter
or from the emitter's mere existence. Suspected cascade points are exactly the three bounded parser
gaps: `?w`→`->` attachment (`parse_seq`, `grammar/lib.rs:1361`), the `@{}` atom and `|params|`
closure (both absent from `parse_atom`, `grammar/lib.rs:1495`, which handles only
literal/regex/paren/ident). Drive the `!has_rule("value")` branch (`runtime_generator.rs:98`), never
`json_sink_direct`.

## The fixpoint subject choice (7D's stake in it)

The oracle needs a *defined equality* on both axes, so 7D constrains the fixpoint choice: the
STAGE-0 fixpoint is **byte equality** (`check_dir`), non-vacuous only if the emitter is a genuine
function of the parsed `GrammarIr` — a fixed-const emit path stamps identical bytes for any input,
so triple-identical certifies nothing. The falsification guard: a *planted mutation on the source
grammar* must change the generated bytes (proves the emitter reads its input). The oracle axis is
**GrammarIr-equivalence** (OREL-1). Keep them orthogonal — do not let byte-identity masquerade as
parser parity.

## Prior art (cited)

- **rustc / GHC bootstrap invariant** — stage1-built-compiler == stage2-built-compiler is the exact
  `regen∘regen` shape; rustc's `-Zunpretty=expanded` self-consistency round-trip is the ACCEPT-level
  oracle in miniature.
- **tree-sitter** — a grammar's generated parser is tested against a golden `.txt` corpus of
  `(input, expected S-expression)` pairs; the S-expression IS OREL-1-STRUCT's normal form, and
  `tree-sitter parse --check` against golden expectations is precisely the golden-anchor ledger.
- **regex-automata** — `hir::Hir` equivalence after `translate`, and property-tested match-set
  equivalence between engines — the ACCEPT-level regex oracle, and the normalize-then-compare
  pattern for `Hir`.
- **egg / egglog** — canonicalization by e-class as the mechanized alternative to hand-normalizing
  `≡_G`; makes the relation rewrite data.
- **CompCert / CakeML** — self-hosting where semantics-preservation is a *named, gated relation*;
  too heavy to prove here, but the discipline (equivalence is a pinned relation, never an assertion)
  is the model.

## Named risks and counterexamples

- **R1 — RuleId-index divergence.** Different rule-emit order breaks naive equality. *Guard:* OREL-1
  resolves `Ref` by name; compares bodies keyed by name (the `names` map shape at `ir/lib.rs:85`).
- **R2 — pass-derived nodes.** `AltMode`/`Discard` are pass outputs; post-pass comparison conflates
  parser and pass divergence. *Guard:* compare pre-`derive_*`, or run identical passes both sides.
- **R3 — recognizer builds no structure.** OREL-1-STRUCT is undefined against today's emitter; only
  ACCEPT is. *Named:* the builder-augment is the born-RED gate for STRUCT.
- **R4 — majority-vote temptation.** *Foreclosed:* golden-anchor is the single hand parser.
- **R5 — byte-fixpoint vacuity.** A const emit path stamps identical bytes. *Guard:* a planted
  source mutation must change generated bytes.
- **R6 — host obligations leaking as verdicts.** Comparing pre-`simplify_hir` `Hir` shows flag
  folding as false divergence. *Guard:* compare post-`simplify_hir` canonical `Hir` only.

## What a prototype must test to move 7D

1. **OREL-1-ACCEPT harness** — run the emitted bbnf recognizer + `parse_grammar` over the `grammar/`
   corpus (json/css/ebnf/bnf/google-sheets/regex `.bbnf` + `bbnf.bbnf` on itself); assert same
   accept/reject; report parity% + first divergence.
2. **OREL-Hir for regex** — `simplify_hir(parse(p)) ≡ simplify_hir(parse_generated(p))` over a
   pattern corpus; classify each divergence as host-obligation (normalized away) or verdict.
3. **Planted-mutation falsification** for both — prove each oracle FAILS on an injected divergence
   and names the construct.
4. **The RED-unknown** — attempt the `-> Span` builder-augment on 2–3 bbnf structural rules through
   the `!has_rule("value")` branch; measure OREL-1-STRUCT parity and `regen∘regen` byte-identity;
   name the exact cascade construct (`?w`→`->`, `@{}`, or `|params|`).

## Seams (path:line)

- `skinny/crates/grammar/src/lib.rs:29` `parse_grammar` (golden anchor); `:1361` `parse_seq` (`?w`
  gap); `:1481` `parse_projection`; `:1495` `parse_atom` (no `@{}`/`|params|`).
- `skinny/crates/ir/src/lib.rs:234` `ExprKind` (no `Empty` variant → `Seq([])` IS the normal form);
  `:51` `add_expr`; `:84` `resolve_refs` (name→RuleId, the index-vs-name pivot); `:110` `validate`.
- `skinny/crates/codegen/src/runtime_generator.rs:98` `!has_rule("value")` shape-dispatch branch —
  drive the emitter here.
- `skinny/crates/codegen/src/general_descent/{emit.rs:452, regex_first.rs:230, view.rs, mod.rs}` —
  1206-LOC recognizer emitter.
- `skinny/crates/codegen/src/lib.rs:58` `write_to_dir` / `:68` `check_dir` — byte-identity fixpoint
  gate + `GENERATED_HEADER` stray-file guard.
- parse-that `rust/parse_that/src/regex/mod.rs:34` `parse_generated` (PRODUCER — record corrected);
  `regex/host.rs:19` `lower`, `:472` `apply_flags`, `:293` unicode ranges, `:303` class escapes —
  engine-tier obligations.
- GN-5.md W3 `name_gate_falsification` (the falsification-proof pattern); the `bbnf-oracle` fleet
  crate (born GN-5.W1) is the oracle's home.
