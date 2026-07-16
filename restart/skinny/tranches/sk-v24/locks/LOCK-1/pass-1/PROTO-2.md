# PROTO-2 — 1D existence proof: injectivity propagator = unification, recursive-sum extraction terminates

**Lock:** LOCK-1 (the solver spine — type inference & solving) · pass 1 · probe 2
**Family:** 1D (native-cyclic e-graph type solver), tested against 1A (nominal fold) on the recursion axis
**Engine:** shipped `crates/egraph` (equality saturation + `Analysis` lattice + cost extraction)
**Command:** `cargo test -p egraph --test type_injectivity`
**Verdict line:** **RAN_MEASURED — (a)–(d) all hold AND (e) converges in 2 passes ≤ bound 10. 1D does NOT retire.**

---

## Question

1D's *only* net-new primitive over the shared e-graph substrate is an **injectivity
propagator**: an e-class holding two nodes of the same constructor and arity forces
`C(x₁..xₙ) = C(y₁..yₙ) ⟹ xᵢ = yᵢ` — first-order unification expressed as e-class union.
The probe asks whether that primitive, plus native cyclic (μ-)types, **beats 1A's nominal
recursion fold or merely duplicates it**. Concretely:

1. Is the injectivity hook genuine unification, driven by the *existing* rebuild fixpoint?
2. Does a cyclic `value = object|array|string|number|bool|null` extract to a **finite**
   representative with a back-edge (visited-set terminates the cycle)?
3. Is extraction deterministic — order-independent and idempotent?
4. Does a `WidthBound` lattice on `Int` leaves join monotonically, and does the combined
   injectivity + width fixpoint **converge without oscillation** in ≤ 2×(class count) passes?

**Retirement condition (1D's own):** if the injectivity fixpoint fails to converge
deterministically or oscillates against the width join, 1D's advantage collapses to "free
sum + recursion," which 1A's nominal fold reaches more cheaply → 1D retires toward 1A/1B.

## Method

New test `crates/egraph/tests/type_injectivity.rs` on the shipped engine. It defines:

- **`TypeENode`** = `Leaf(Base) | Sum(Box<[Id]>) | List(Id) | Arrow([Id;2]) | Row(Box<[Id]>) | Var`,
  `#[derive(Language)]` over the whole enum. `Base = Null|Bool|Str|Int(i64)`.
- **`TypeAnalysis`** with `Data = Option<WidthBound{min,max}>` — bottom `None`, join =
  `[min↓, max↑]`; `make` seeds `Int(v) → {v,v}`; `width_ty` maps a bound to `U8/U16/U32/U64`.
- **The injectivity hook in `Analysis::modify` (~22 LOC):** snapshot the class's nodes,
  and for every same-discriminant / same-arity pair, `egraph.union` their positional
  children. Each union re-enqueues the merged class through the *existing*
  `pending`/`analysis_pending` worklist, so the propagator runs to a fixpoint with **no
  bespoke driver**.
- **Cyclic encoding:** `value = Sum([object, array, string, number, bool, null])`, with
  `array = List(value)` and `object = Row([value])` (tail = `value`) referencing `value`'s
  class. The knot is tied with a `Var` placeholder unioned into `value`; the cost model
  prices `Var` out and charges `List`/`Row` a constant (pointer-like) cost so extraction
  prefers the unrolled sum and the cyclic cost stays bounded.
- **Three AC sum rewrites** `sum-flatten / sum-sort / sum-absorb`, all emitting the *same*
  fully-normalized (flattened + deduped + key-sorted) arm list — confluent by construction.

### One core-engine amendment (`crates/egraph/src/egraph.rs`, +30/−4)

`union` previously scheduled the analysis hook only when the lattice `data` *changed*
(`if changed { analysis_pending.push(dst) }`). Structural merges (two `Sum`/`Arrow` nodes
landing in one class) do not move the width lattice, so `modify` — and therefore injectivity
— would never fire. The amendment schedules `modify` on **every** real merge (egg's
semantics): `modify` reacts to the merged *node set*, independent of the lattice value. For
analyses whose `modify` is the default no-op (`NoAnalysis`, `GrammarAnalysis`) the class is
popped and ignored — free. A `rebuild_pass_count` counter was added to measure true fixpoint
iterations for (e).

## What ran

`cargo test -p egraph --test type_injectivity` — **4 tests, 4 passed, 0 failed** (0.00s).
Full `cargo test -p egraph` (7 targets, 33 tests) green — no regression from the `union`
amendment. `cargo test -p bbnf-ir --test egraph` (the sole cross-crate `GrammarAnalysis`
consumer of the amended path) — **37 passed, 0 failed**.

Numbers captured via one instrumented run (`-- --nocapture`), then instrumentation reverted:

| Probe | Assertion | Measured |
|---|---|---|
| **(a)** injectivity = unification | `Sum([a,b])≡Sum([c,d]) ⟹ a≡c, b≡d`; `Arrow` domain+range likewise | `find(a)==find(c)`, `find(b)==find(d)`, `find(p)==find(r)`, `find(q)==find(s)` — **hold** |
| **(b)** recursive extraction | exactly 6 arms; finite; back-edge; no `Var` | Sum arity **6**; `tree_nodes=7 ≤ total_classes=10`; `best_cost=9`; `Var` absent; back-edge present — **hold** |
| **(c)** order-independence | 2 source orderings `[0..5]` vs `[4,0,5,2,3,1]` extract byte-identical | `sexpr_len=63`, **identical** |
| **(d)** idempotence | re-extract same graph → byte-identical | **identical** |
| **(e)** width + injectivity fixpoint | join `{200}⊔{70000}→u32`; converge ≤ 2×classes; no oscillation | `passes=2 ≤ bound=10` (`total_classes=5`); `joined={200,70000}→U32`; 4 extra rebuilds: `union_count` + width **stable** |

The canonical S-expression extracted in (b)/(c)/(d) — the whole proof in one line:

```
@0:Sum(@1:Null|@2:Bool|@3:Int(0)|@4:Str|@5:List(#0)|@6:Row(#0))
```

Exactly six arms; `List` and `Row` both close back onto `#0` (the `value` class itself) —
a **finite μ-representative** whose back-edges are resolved by the extractor's visited set.
Two graphs with different source arm orderings produce this byte-for-byte.

`loadavg` at run: `9.00 / 11.50 / 11.78` (correctness probe — no cold timing claim is made).

## Findings (what the proof surfaced)

1. **The primitive is genuine unification, and it's cheap.** ~22 LOC in `modify` +
   3 lines of scheduling in `union`. It converges through the engine's own worklist — no
   separate solver loop. Confirmed on both `Sum` (as a 2-ary tuple) and `Arrow`.

2. **Convergence is deterministic and monotone.** Union-find only merges and the width
   join only grows `max` — two monotone lattices, so no oscillation is *possible*, and the
   empirical `passes=2` (bound 10) plus 4 stable follow-up rebuilds confirm it. **1D does
   not hit its retirement condition.**

3. **Native cyclicity beats a nominal fold — it needs no recursion machinery at all.** The
   μ-type is just an e-graph cycle; the *already-shipped* `extract_recurse` visited set
   yields the finite representative for free. 1A would need an explicit nominal binder +
   fold to achieve the same finite unrolling; 1D gets it structurally. This is the axis on
   which 1D is strictly ahead, not a duplicate.

4. **Load-bearing soundness constraint (must be carried into any real 1D):** positional
   injectivity is **unsound for associative-commutative constructors**. Applied blindly to
   `Sum`, two differently-*ordered* same-arity sums in one class would falsely unify (e.g.
   `null` with `str`). The probe keeps it sound by making the AC-sum rewrites **confluent**
   to a single canonical representative, so the class never holds two rival same-arity sums
   — injectivity then lies dormant on `Sum` in the cyclic test and only bites the injective
   constructors (`Arrow`/`List`/`Row`/tuple). Test (a) exercises `Sum`-as-tuple injectivity
   deliberately, in an e-graph with no AC rewrites running. **A production 1D must tag
   constructors injective-vs-AC and gate the hook on the flag.**

5. **`Row{fields, tail}` is stored as one contiguous `Box<[Id]>` (tail = last element)**
   because `Language::children()` must return a single `&[Id]`; the derive rejects a
   variant with two separate `Id`-bearing fields. All five recursive constructors are thus
   single-tagged-field variants and the derive handles the whole enum — consistent with the
   "derive verified for `Sum/List/Arrow`" note in the brief.

## Verdict

**RAN_MEASURED.** (a)–(d) all hold and (e) converges in **2 passes ≤ bound 10** with no
oscillation and a monotone `u32` width join. 1D's net-new primitive is proven on the shipped
engine, and native cyclicity is shown to **beat** 1A's nominal fold on the recursion axis
(finite μ-extraction for free via the existing visited set) rather than duplicate it. 1D's
retirement condition is **not** triggered. 1D advances — carrying finding (4) (constructor
injectivity flags) as the one non-negotiable design obligation.
