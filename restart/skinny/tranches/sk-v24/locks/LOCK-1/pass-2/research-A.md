# LOCK-1 · FAMILY 1A — UNIFICATION-FIRST · pass-2 research (A)

## Thesis

The type plane is a single directed unifier over one union-find store. The Hindley–Milner
core grows three principled axes — nominal-recursive constructors, row polymorphism for
document shapes, let-generalization over rules — and stays a *decidable* equality engine.
CSP is not the spine; it is a bounded post-solve invoked only where unification is genuinely
undecidable (numeric widths, decode/overload classes). Equality is carried by the store, not
by a monotone fold and not by a saturating e-graph: two rule types are equal iff their
variables union; disagreement raises an honest clash obligation rather than overwriting. This
is the "equality carrier via union-find re-seed" of Thread A, generalized from PROVE-08's
leaf-only prototype to the full document-shape lattice.

## 1. The mechanism, made concrete

### 1.1 The store and its monotypes

The proven substrate is PROVE-08's `Unifier` (`prototypes/PROVE-08/patch.diff:288-372`):
`parent: Vec<u32>` union-find with path-halving `find` (`:300`), `unify` that lets variables
absorb constructors and clashes two distinct constructors as `UnifyError` (`:311`), `bind`
(`:347`), and `resolve`/zonk (`:359`). Today its monotype is only `Ty::{Var, Con(TyCon)}`
with a closed five-leaf `TyCon` (`:254-265`). Family 1A grows `Ty` to a full term algebra
while keeping the store and its `find`/`unify`/`bind` verbatim:

```
Ty ::= Var(u32)                    -- unification variable (union-find node)
     | Con(TyCon)                  -- scalar leaf: Bool F64 U8Tag StringDecoded StringSpan …
     | App(Nom, Vec<Ty>)          -- NOMINAL constructor: one Nom per RuleId (recursion boundary)
     | Row(RowVar)                 -- record shape: fields + open/closed tail (§1.3)
     | Sum(Vec<(Tag, Ty)>)        -- tagged alternation (§1.4)
     | Width(WVar)                 -- residual numeric-width var, deferred to the CSP post-solve (§1.5)
```

Unification extends by structural recursion over `App`/`Sum` and by row unification over
`Row`; `Con`/`Con` stays the honest clash. This is Robinson unification with an occurs-check
relaxed by nominal folding (§1.2).

### 1.2 Nominal-fold recursion (the termination mechanism — Thread A #1)

Grammars are mutually recursive (`value → object → pair → value`). Structural unification of
such a system either loops or fails occurs-check. The cure is nominal folding: each rule
`RuleId` is its own constructor `Nom(RuleId)`; a `Ref{target}` unifies against
`App(Nom(target), args)` — the rule's *name*, not its expansion. Recursion is folded at the
nominal boundary (iso-recursive discipline: rules are the `data` boundary of ML/Haskell).
Termination is guaranteed because descent never crosses a `Nom` node — the store has one
variable per rule, seeded up front. This generalizes skinny's `seed_rule_types`
(`crates/passes/src/lib.rs:172`) from an opaque `Type::Rule(id)` leaf into a real union-find
node that can *carry* a resolved row/sum/scalar. "Re-seed" = seed nominals, then run arrows
and refs as unification constraints and resolve by `find`; this displaces the monotone fold
(§2). An equirecursive fallback (Cardelli's Amber rule / OCaml `-rectypes`; Huet cyclic-term
unification) is available where two differently-named rules must be structurally equal, but
nominal is the correct default — a grammar rule *is* a nominal type.

### 1.3 Row polymorphism for document shapes (the `List ≠ Row` mechanism — Open R)

A record type is `Row { fields: BTreeMap<Label, Ty>, tail: RowTail }` where
`RowTail ::= Closed | Open(RowVar)` (Rémy/Leijen scoped labels). The classifier Open R and
Thread D #1 demand — *record vs list without a `{ }`/`[ ]` hint* — falls straight out of
unification structure, no e-graph, no per-grammar predicate:

- `object = pair*`, each `pair` unifying to `(label, value)` with a **static literal label**
  → the repeat accumulates a **closed `Row`** (one field per distinct literal). This is the
  per-document typed-grammar case (`twitter_search.bbnf`: `statuses -> Vec<Tweet>` becomes a
  closed row field).
- `pair*` with **runtime string keys** (generic `json.bbnf`, keys are `/"…"/` spans) → no
  static labels → `Row` with `Open` tail + a single value-typed field: a **Map**, distinct
  from struct and list. This is the shape the running pass masks as the `json_templates`
  fallback; row polymorphism represents it as a first-class `Row{Open}`.
- `element*` whose repeat body is a **single value type** (no label) → **`List`**.

`List ≠ Row ≠ Map` is thus a consequence of *what the repeat body unifies to*, decided by
inference. Mixed grammars (known fields + arbitrary extras) unify to a partial row with an
open tail — the case ad-hoc struct-detection cannot express and Rémy rows can.

### 1.4 Sums and let-generalization

The six-arm `value = object|array|string|number|bool|null` unifies to one canonical `Sum` by
set-equal variant unification (GN-3.W1's requirement, order-insensitive; canonical variant
ordering). Each grammar rule is a `let`-bound scheme; generalize its inferred type to
`∀ᾱ. τ` at the rule boundary and instantiate fresh at every `Ref`, so a reused helper
(`list<T> = T (',' T)*`) is polymorphic, not monomorphized-to-first-use. Generalization runs
per **SCC of the rule dependency graph** (OCaml `let rec` groups): fix the recursive group's
point *before* generalizing, or sharing is lost. Level/rank-based generalization (Rémy 1992;
Kiselyov "Efficient generalization with levels") avoids the whole-environment free-variable
scan — O(1) amortized per binding — which the derive-wall-time gate needs. This SCC-generalize
discipline is 1A's answer to Open S's cross-rule seam: the synthesized attribute is the rule's
scheme, the termination lattice is the SCC fixpoint — no fifth engine.

### 1.5 The bounded CSP post-solve (widths & overloads — Thread A #3)

After HM fixes the structural skeleton, residual `Width(WVar)` and decode-class variables
form a small finite-domain problem: an integer-literal magnitude under-determined between
`u8/u16/u32` (the A3 fixture: `"px" -> 0u8 | … -> 300`), or a `->` target that could decode
multiple ways. Two settlements, measured head-to-head:

- **Ordered-lattice defaulting** — `u8 ⊑ u16 ⊑ u32 ⊑ u64`; assign the least upper bound of
  observed literal magnitudes. Deterministic, O(constraints), mirrors Rust's integer-literal
  fallback and Haskell's `Num` defaulting. Zero CSP invocation for the common case.
- **Bounded search** — hand the residual variables + domains to the vendored csp-solver
  (`crates/csp-solver/`) as a finite-domain CSP with a cost objective. Needed only when width
  choice interacts with a legality/cost constraint the lattice cannot express.

A3's prototype authors the genuinely under-determined fixture and measures lattice-defaulting
vs search on it; the expectation is defaulting wins on the fixture, search reserved for the
interaction case. This is the pluggable residue seam Open X asks for: one interface, two
swappable implementations (lattice / search) behind it.

## 2. Substrate & integration seams (path:line)

**Measurement substrate to displace (skinny @HEAD):**
- `skinny/crates/passes/src/lib.rs:149` `infer` — entry; `:178-214` `infer_expr` is the
  monotone structural fold (`Type::Seq/Alt/List/Option/Rule`) — no unification anywhere.
- `:204` `ExprKind::Project { body, .. } => self.infer_expr(*body)?` — the equality carrier's
  target: the projected `target`/`ty` is discarded here. Re-seed replaces this arm with
  `classify_arrow` (`PROVE-08/patch.diff:374`) + `unify` into the rule's nominal var.
- `:133-136` `Substitution { vars: HashMap<TypeVarId, Type> }` — vestigial today (the fold
  never substitutes); becomes the store's zonk map.
- `:234-243` `enum Type`, `:245-254` `BuiltinTy` — grow to the `Ty` algebra of §1.1.

**Proven core to seed from:** `PROVE-08/patch.diff:288-372` (`Unifier`, `find`/`unify`/`bind`/
`resolve`), `:374-406` (`classify_arrow`, the bidirectional leaf lexer), `:254-265` (`TyCon`).

**Greenfield carrier & LOCK-3 sink (XL-F):**
- `crates/ir/src/types/type_desc.rs:13` `enum TypeDesc` — already rich: `Named(StringId)`,
  `Enum`, `BoxedEnum`, and a **heterogeneous-alt join obligation** variant (AZ-III.W3a.3)
  "produced by the type projection CSP when an Alt's branch types disagree." 1A adds
  `Row`/`Sum` here (Open R's `DirectTypeKind::{Sum,Row}` / `LayoutKind::Row`,
  `crates/ir/src/registry/struct.rs:58`) so unification's output has an IR home. The Join
  obligation is exactly what row/sum unification should *subsume*: an Alt of records unifies
  to a `Sum` or a widened `Row`, not a deferred CSP obligation.
- `crates/core/src/backend/driver/analysis.rs:22` `TypeAnalysis::from_ir` reads
  `ir.types: Vec<(RuleId, TypeDesc)>` + `ir.type_map` — the record 1A must populate and lower
  through. Second sink: `crates/core/src/backend/rust/view/named_types.rs:100`
  `RustNamedTypes::from_ir`.
- `crates/ir/src/passes/types/mod.rs` `project_types` (called `analysis.rs:136`) is the
  running greenfield type pass a re-seed reconciles-with or displaces.
- **Existing greenfield union-find (do not conflate):**
  `crates/ir/src/passes/csp_strategy/components.rs:75-293` `UnionFind` +
  `partition_by_call_graph` + `collect_ref_unions` unions **`RuleId`s for SCC/call-graph
  partitioning** — the SCC substrate §1.4 needs is *already here*. The type unifier is a
  distinct union-find over *type variables*; the re-seed reuses the SCC partitioner to order
  let-generalization and adds a type-variable store beside it.
- Type-projection CSP already present: `crates/ir/src/passes/csp_strategy/constraints/engine.rs:18`,
  `crates/ir/src/passes/materialization/lattice.rs:46` (the `Map` class). 1A's claim: most of
  this CSP surface is subsumed by unification; the CSP shrinks to the width/overload
  post-solve of §1.5.

**TS mirror:** `crates/core/src/backend/ts/projection.rs:11` `type_desc_to_ts` — every new
`TypeDesc` arm (`Row`/`Sum`) needs its TS projection here (Row → object type / `Record<…>`,
Sum → discriminated union). Every record design names this mirror per the evidence bar.

## 3. Prior art (citations)

- **Row polymorphism:** Leijen, *Extensible records with scoped labels* (TFP 2005); Rémy,
  *Type inference for records in a natural extension of ML* (1994). The principled
  record-shape mechanism; scoped labels handle partial/open rows.
- **Level-based generalization:** Rémy, *Extension of ML type system with a sorted equational
  theory on types* (INRIA RR-1766, 1992); Kiselyov, *Efficient generalization with levels*.
  O(1)-amortized `let`-generalization — the derive-wall-time enabler.
- **Recursive-type unification:** Cardelli's *Amber* rule for recursive equality; Huet's
  cyclic-term unification; OCaml `-rectypes`. Nominal folding = iso-recursive `data` boundary
  (SML/Haskell). Grounds the termination claim of §1.2.
- **Integer-literal defaulting:** Rust's `i32` fallback for unconstrained integer literals;
  Haskell Report §4.3.4 `default` for `Num`. Grounds the ordered-lattice width settlement.
- **HM Algorithm W:** Damas–Milner (1982); Pierce *TAPL* ch. 22. The spine. rustc's
  level/rank inference and its integer-literal fallback are the closest production analogue.
- **CSP post-solve engine:** vendored `crates/csp-solver/` (csc411) — finite-domain solver for
  the width/overload residue only; the inversion of LOCK-1B's framing (unification *first*,
  CSP bounded).

## 4. Named risks & counterexamples

- **Nominal under-unification.** Two structurally-equal, differently-named rules (`point=x y`
  vs `pair=x y`) will not unify nominally. Counterexample: a grammar inlining one shape under
  two names diverges from a structural oracle. *Resolution:* nominal is correct for grammars
  (rules are the nominal boundary); equirecursive structural equality is opt-in via the Amber
  fallback, measured only if a corpus needs it.
- **Infinite type without folding.** `value = object | …`, `object` contains `value`: pure
  structural unification loops / fails occurs-check. This is the A1 *termination* gate — the
  prototype must prove the nominal unifier terminates on mutually-recursive JSON/Sheets/CSS L4.
- **Runtime-key row mis-derivation.** Generic JSON's open row must NOT collapse to a closed
  struct (that reintroduces the masked `json_templates` fallback). *Resolution:* static-literal
  keys → `Closed`; any runtime-key path → `Open(RowVar)` = Map. The classifier is the label's
  *staticness*, decided by unifying the key's type to a literal vs a span.
- **Premature generalization across a `let rec` group.** Generalizing a mutually-recursive
  rule before its SCC fixpoint loses sharing / is unsound. *Resolution:* generalize per SCC
  using the existing `partition_by_call_graph` (`components.rs:256`).
- **Width non-confluence.** Two arrows constraining one width var to different magnitudes: the
  lattice LUB is deterministic; search must be deterministic-ordered or derived schemas differ
  run-to-run (breaks GN-3's byte-identity gate). A3 measures both for determinism.
- **Join-obligation regression.** If `Row`/`Sum` unification does *not* subsume the existing
  AZ-III Join obligation, the heterogeneous-alt corpus could still emit a deferred CSP
  obligation — two carriers for one fact. The port must show the Join variant becomes
  unreachable (or is retired) once Alt-of-records unifies to Sum/widened-Row.

## 5. What a prototype must test to move family 1A

1. **Termination + parity (A1).** Nominal-recursive union-find unifier terminates on
   mutually-recursive JSON, Sheets, CSS L4; produces types matching the skinny monotone fold
   where the fold is defined; derive wall-time ≤ the running pass (baseline = the running
   fold, not a strawman). Gate: no occurs-loop, structural match, ≤ wall-time.
2. **Row-without-hint (Open R / D#1 by unification).** twitter typed grammar → **closed Row**;
   generic `json.bbnf` object → **Map (Open row)**; array → **List** — all by inference, zero
   `{ }`/`[ ]` predicate. Assert `List ≠ Row ≠ Map` on one run.
3. **let-generalization sharing (Open S).** A shared `sepBy`-style helper reused at two
   element types instantiates to two distinct monotypes; SCC-grouped generalization; measured
   against monomorphize-to-first-use.
4. **Width post-solve (A3 / Open X).** Author the u8/u16/u32-under-determined `->` fixture;
   resolve via ordered-lattice defaulting AND via csp-solver bounded search; report
   determinism + wall-time; the residue seam carries two swappable impls.
5. **Lower through the sink (XL-F).** Resolved `Row`/`Sum` populate `ir.types` and lower
   through `TypeAnalysis::from_ir` + `named_types.rs` with the TS mirror `type_desc_to_ts`
   populated — proving the carrier reaches LOCK-3 and closing the `json_templates` mask.
6. **Port cost (A2).** Seed equality from the PROVE-08 leaf core into greenfield, then
   reconcile-with / displace `project_types`; report derive wall-time + LOC-deleted +
   runtime-diff (must be empty — the type plane costs zero at runtime, Constitution v).
