# LOCK-5 pass-3 — Family 5B: THE LAWFUL FUNCTOR (hardening the two-type bridge)

Settled ground (XL-C): grammar tier keeps the regex leaf opaque; the engine tier keeps a rich
`Hir` grown with `Class`/`Look`; a cheap string bridge reusing the engine's `simplify_hir` joins
them. This pass HARDENS that substrate with run-read evidence, not re-argument. The headline is a
**substrate/settled-ground divergence**: the MEASUREMENT SUBSTRATE's bridge does NOT reuse
`simplify_hir` or the rich `Hir` at all — it is a 322-LOC nominal string-matcher on three
hard-coded JSON patterns. The lawful functor's job here is to delete that nominal duplicate and
route the substrate through the engine's structural passes. Two concrete counterexamples (a phantom
`Look` leaf with no automata consumer; the nominal classifier that breaks off-JSON) are proved from
source below.

## The three regex homes, base-pinned to HEAD

- **Canonical engine** — `parse-that/rust/regex` (READ-ONLY). Rich `Hir` already carries
  `Class(CharClass)` + `Look(Look)` (`hir/mod.rs:216-233`, `Look` 6 variants `:172-185`). Pass files
  and Class/Look arm census (Q1 evidence, exact `wc -l` + `grep -c`):
  | Pass file | LOC | `Class` arms | `Look` arms |
  |---|---|---|---|
  | `classify.rs` | 1148 | 64 | 0 |
  | `first.rs` (FIRST-sets) | 158 | 6 | 2 |
  | `info/classify.rs` | 153 | 10 | 3 |
  | `info/mod.rs` / `one_pass.rs` / `width.rs` | 412 / 72 / 133 | — | — |
  | `automata/nfa.rs` (`from_hir`,`build_class`,`build_look`) | 449 | 5 | 11 |
  | `automata/dfa.rs` | 438 | — | — |
  | `egraph/translate.rs` (`insert_hir`/`extract_hir`) | 132 | 2 | 2 |
  | `egraph/cost.rs` / `node.rs` | 102 / 73 | 1 / 1 | 1 / 1 |
  | `egraph/mod.rs` (`simplify_hir`, `needs_saturation`) | 200 | 1 | 1 |
- **Grammar tier (top-level tree)** — `bbnf-lang/crates/ir`: `GrammarENode::Regex(StringId)` opaque
  + the 434-LOC `egraph/rules/regex.rs` string-fusion rules (pass-2 rule-death target).
- **MEASUREMENT SUBSTRATE** — `skinny/crates/bbnf-regex` (322 LOC) + its consumers `skinny/crates/ir`
  (`use bbnf_regex::analyze;` `ir/src/lib.rs:1`, called at `:331` for nullability) and
  `skinny/crates/passes` (`use bbnf_regex::{analyze, FirstSet, RegexKind};` `passes/src/lib.rs:1`).
  The skinny grammar leaf is `ExprKind::Regex { pattern: String }` (`ir/src/lib.rs:250`), an opaque
  string scanned by `parse_regex` (`grammar/src/lib.rs:1561`), lowered to
  `BackendExpr::RegexProgram { pattern, span_kind }` (`ir/src/lib.rs:389`).

## Q1 — per-pass dispatch census: SHRINK vs RELOCATE

The substrate's bridge is `analyze(&str) -> RegexFacts` (`skinny/crates/bbnf-regex/src/lib.rs:45`).
`RegexFacts` (`:1-8`) carries `nullable`, `first: FirstSet`, `byte_classes`, `hir: RegexHir`, and
`string: Option<StringFacts>`. But `RegexHir` (`:11-13`) is a lie: it wraps a **4-way nominal tag**
`RegexKind { Whitespace, QuotedString, Numeric, Unknown }` (`:16-21`), and `classify` (`:92-99`) is
`if pattern == r"[ \t\n\r]*"` -> `Whitespace`; `pattern.starts_with('"')` -> `QuotedString`;
`pattern.starts_with("-?(0|[1-9]") && …` -> `Numeric`; else `Unknown`. `StringFacts` hard-codes
`delimiter: b'"', escape: b'\\'` (`:53-56`). This crate has **no dependency on the engine `Hir`** and
never calls `simplify_hir` — it is precisely the nominal fast-path the Constitution (vi, no
name-gates) and the direct-to-struct edict forbid; it breaks on any grammar but JSON.

The census verdict: **the substrate side SHRINKS, the engine side RELOCATES.** The engine's
`classify.rs` (1148 LOC, 64 `Class` arms) is irreducible *structural recognition* — it walks the
`Class`-bearing `Hir` to recover sign / fraction / exponent / leading-zero-rejection / `\uXXXX`
flags (`classify.rs:1-20` doc; sample arms `:287,477,506,538,640,795,966,1022,1041,1069,1075`).
Re-homing those 64 arms into a shared IR RELOCATES them arm-for-arm (LOC-neutral); the Class-walk is
the work, not dispatch boilerplate. What is genuinely DELETED is the 322-LOC nominal duplicate:
`classify` (`:92`), `regex_is_nullable` (`:107`), `first_set` (`:120`), `closing_class_index`
(`:164`) all re-implement, string-first, what `classify.rs` + `first.rs` already do structurally over
`Hir`. Prototype must MEASURE: substrate `~322 -> ~60` (a thin structural derivation:
`parse -> simplify_hir -> {nullable via structural walk, first via first.rs, byte_classes via Class
leaves}`), engine `classify.rs` `1148 -> 1148` (unchanged). Net = **delete the nominal duplicate,
point the substrate at the real classify** — a re-home that shrinks the *bridge*, not the engine.

## Q2 — Class/Look typing through LOCK-1's real HM; byte-expansion confinement

The grammar leaf is `Regex(StringId)`/`Regex{pattern:String}` opaque, so in LOCK-1's HM it inhabits a
**single scalar type** — the leaf's materialized value type: a byte-span-producing type by default,
refined ONLY by an enclosing `Project{ty}` (Q11). LOCK-1 never sees `Class` or `Look`; there is no
`BuiltinTy` stub because there is no per-leaf regex type to stub — the type is `String`-ish and
composes through the ordinary `Seq`/`Alt`/`Repeat` rules. The `[α-ω]*` byte-expansion (Class ->
`ByteSet` -> NFA transitions) lives **confined to `automata/nfa.rs::build_class` + `sets/byteset.rs`**;
it never leaks into a shared type because the ONLY thing that crosses the bridge is `RegexFacts` —
first-set bytes, nullable, string delimiters — a *facts record*, not a type. Confinement boundary =
`RegexFacts` at `bbnf-regex/src/lib.rs:1-8`. `Look` assertions are zero-width -> they contribute unit
/ `Empty` to the value type, never widening it (nullability treats them as ε, `first.rs:79,156`
returns empty/transparent). Derivation to hand the prototype: `Regex(sid) : τ_span`, `τ_span`
unifies with the outer `Project`'s `ty` when present; byte cardinality is an automata fact, invisible
to HM. This discharges the Constitution-v invariant (type plane costs zero at runtime) by
construction — the bytes are compiled, not typed.

## Q3 — `from_expr` NFA byte-identical to `from_hir`

`from_hir` exists (`nfa.rs:71`, `build_hir` dispatch `:125-152`). `from_expr` does NOT exist — it is
the target: an NFA builder that consumes the grammar's own `ExprKind` (`Seq/Alt/Repeat/Optional/
Literal/Regex`, `ir/src/lib.rs:234-271`) so grammar structure and regex fragments share ONE Thompson
builder. The two normalization laws the byte-identity harness must pin:
- **`Optional <-> Repeat{0,1}`** — `from_expr(Optional(x))` must equal `from_hir(Repetition{min:0,
  max:Some(1),greedy:true})`; the engine has no `Optional`, so the grammar `Optional` node must lower
  to the same ε-structure `build_repetition` emits.
- **Group elision** — `from_hir` treats `Hir::Group` as transparent (`nfa.rs:143-146`, `build_hir(sub)`),
  but skinny `ExprKind` has NO capture-group node (capture is `Project`, Q11). So
  `from_expr(Regex("(?:ab)"))` routes through the engine parse (which yields `Group`) then elides —
  the harness must assert `from_expr` over a Group-bearing pattern equals `from_hir(parse(pattern))`
  after both elide. Harness: emit both NFAs, canonically renumber states, compare
  `(transitions, epsilon-edges, accept)` vectors for byte-identity; run fresh throughput on both,
  carry NO inherited band. Named risk: `from_expr` sees `Alt{mode: Dispatch|Speculative}`
  (`ir/src/lib.rs:236-239`) — `mode` has no `Hir` analogue; the harness must confirm `mode` does not
  perturb the NFA (it is a parse-strategy annotation, transition-neutral) or the byte-identity fails.

## Q4 — a `Look`-consumer test that CAN fail (it does, today)

**The `Look` leaf is a phantom — it has no automata consumer** (Constitution viii, consumer-less
substrate forbidden). `build_look` (`nfa.rs:254-270`) matches ALL six `Look` variants (`Start`,
`StartLF`, `StartCRLF`, `End`, `EndLF`, `EndCRLF`) into ONE arm that emits a single state with
`start == accept` — a zero-width ε that **always matches**; position is never consulted, `Look::End`
is discarded. Therefore `/foo$/ -> from_hir -> NFA` does NOT reject `"foobar"`: the `$` is a no-op ε
and a search accepts the `"foo"` prefix. The failing/passing pair the directive demands, exercised
against the real builder:
- `assert!(nfa_matches("/foo/", "foobar"))` — **passes** (prefix match).
- `assert!(!nfa_matches("/foo$/", "foobar"))` — **FAILS today** (the `$`/`Look::End` no-op accepts).
This is the concrete counterexample: the grown `Look` leaf is unconsumed. To make the second assert
pass the matcher must gain anchor semantics — either an `anchored`/`WhichCaptures`-style search mode
(regex-automata's model) or explicit boundary transitions consulting input position at
`build_look`. The prototype must EITHER add the consumer (anchor-aware DFA search) OR narrow the
`Look` enum to the variants an actual consumer exists for — banking the rest as a named negative.
Until then `Look::{End,EndLF,EndCRLF,StartLF,StartCRLF}` are decorative.

## Q11 — the ratified representation of `->` (binding on bridge soundness)

`->` is **`ExprKind::Project { body: ExprId, target: String, ty: Option<String> }`**
(`ir/src/lib.rs:261-265`), an OUTER wrapper node — NOT an inner-class `Map` barrier. Projection lives
strictly *outside* the opaque regex leaf. `parse_regex` (`grammar/src/lib.rs:1561`) returns a plain
`String`, so an inner `/[a-c] -> A/` is **not parseable, not expressible** — the inner-class-barrier
question is moot on the substrate. Consequence, and the pass-2 soundness fix made structural:
`/[a-c]/ -> A | /[d-f]/ -> B` is `Alt{branches:[Project{body:Regex("[a-c]"),target:A},
Project{body:Regex("[d-f]"),target:B}]}` — the `Project` wrappers sit ABOVE the regex leaves, so
regex-internal `union`/`superset` fusion (which operates only on bare `Class`-bearing `Hir`) can never
see or drop the projections. The pass-2 latent bug (`[a-c]->A | [d-f]->B` merging to `[a-f]` and
dropping A,B) is **structurally prevented**: the grammar-tier e-graph owns the `Alt`-branch fusion and
must match `Project`-equality before merging `body`s; the regex engine only ever receives already-
stripped classes. The bridge is projection-sound BY the ratified `Project`-wrapper representation.
LOCK-1 must only confirm the `ty` on `Project` is the sole type carrier for the wrapped leaf (Q2).

## Named risks / counterexamples

1. **Phantom `Look` (Q4)** — grown leaf, no automata consumer; `/foo$/` accepts `"foobar"`. Must add
   consumer or narrow the enum.
2. **Nominal substrate classifier (Q1)** — `bbnf-regex::classify` (`lib.rs:92-99`) hard-codes three
   JSON patterns; ANY off-JSON grammar collapses to `Unknown` -> wrong `nullable`/`first`. The
   direct-to-struct / no-name-gate violation the functor must delete, not preserve.
3. **`Alt{mode}` has no `Hir` image (Q3)** — `from_expr` byte-identity can silently fail if `mode`
   perturbs NFA shape; assert transition-neutrality.
4. **Group asymmetry (Q3)** — `from_hir` elides `Group`; `from_expr` has no group node. Harness must
   pair a Group-bearing pattern through the engine parse.
5. **Re-home is LOC-neutral on the engine (Q1)** — do not claim `classify.rs` shrinks; the 64 `Class`
   arms relocate. The only real deletion is the 322-LOC substrate duplicate. Measure both, report the
   honest delta.

## Prior art (citations)

- **Trees That Grow** (Najd & Peyton Jones, IFL 2016 / GHC Core) — one AST family, phase-indexed
  domain-fit decorations; the strongest anchor for the two-type (opaque grammar leaf vs rich `Hir`)
  split kept lawful by a functor rather than a god-enum.
- **Lenses / bidirectional transformations** (Foster, Greenwald, Moore, Pierce, Schmitt, POPL 2005) —
  `GetPut`/`PutGet` are the round-trip oracle law; here the law is DFA-equivalence
  (`extract∘insert ≅ id`), not syntactic identity, since `simplify_hir` canonicalizes.
- **regex-automata `Look` / anchored search** (Rust `regex` crate) — the reference consumer for Q4:
  `Look` assertions are honored via anchored NFA search + `WhichCaptures`, not zero-width ε no-ops;
  the model for giving the grown `Look` leaf a real automata consumer.
- **Thompson NFA construction** (McNaughton–Yamada / Thompson 1968) — the shared `from_hir`/`from_expr`
  builder; `Optional<->Repeat{0,1}` and Group-transparency are the normalization laws under test.
- **egg / egglog** (Willsey POPL 2021; Zhang PLDI 2023) — `insert_hir`/`extract_hir`
  (`egraph/translate.rs:22`) + `simplify_hir` (`egraph/mod.rs:143`) are the functor's regex end; the
  shared rewrite engine is the charter's "shared part".

## What the prototype MUST test to move family 5B

1. **Delete-the-nominal-classifier (Q1):** replace `bbnf-regex::analyze`'s
   `classify/regex_is_nullable/first_set` with a structural derivation over the engine `Hir`
   (`parse -> simplify_hir -> RegexFacts`); assert byte-identical `RegexFacts` on JSON's
   ws/string/number AND correct facts on ≥1 off-JSON grammar the nominal path gets wrong; MEASURE
   substrate LOC (`~322 -> ?`) and engine LOC (expect unchanged).
2. **`from_expr` byte-identity harness (Q3):** build `from_expr(ExprKind)`; assert NFA byte-identity vs
   `from_hir` on `Optional<->Repeat{0,1}`, Group-elision, and a `Class`-bearing pattern; confirm
   `Alt{mode}` transition-neutral; report fresh throughput both sides.
3. **Failing `Look` test (Q4):** exercise `assert!(!nfa_matches("/foo$/","foobar"))` — show it FAILS on
   `build_look` today; add anchored-search consumer OR narrow the enum; show it then PASSES.
4. **Projection-soundness (Q11):** construct `/[a-c]/->A | /[d-f]/->B` as `Alt[Project,Project]`; assert
   grammar-tier fusion refuses to merge `body`s while `target`s differ (projection-preserving), and
   confirm inner `->` is un-parseable (`parse_regex` returns `String`).
5. **Typing confinement (Q2):** show `RegexFacts` (not `Hir`) is the sole bridge crossing; assert
   `[α-ω]*` byte-expansion appears only under `automata/`+`sets/`, never in a LOCK-1-visible type.
