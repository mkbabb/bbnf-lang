# PROTO-1 — LOCK-1 pass-2, probe 1: let-generalization graft vs. union-find re-seed

**Family fork:** 1A ⊥ 1B carrier fork (§0 X10 — the banked HM(X) void).
**BASE-PIN:** measured on HEAD `ce51c36fcbdf5108eb028005586672d9a6d2ca1b`
(= base-pin ce51c36fc itself; `git merge-base --is-ancestor ce51c36fc HEAD` → true; NOT stale, no wrong-artifact-risk).
**loadavg at measure:** 8.09 / 6.44 / 5.91. **Verdict:** RAN_MEASURED (real `project_types` pass, real types, green goldens, 0-line runtime diff).

---

## QUESTION

The banked HM(X) floor (1A) presumes per-rule **let-generalization** — quantify a
rule's type over its free type vars into a scheme `∀ᾱ.τ`, then **fresh-instantiate**
that scheme at each use site — as the price of *false-fusion soundness* (two uses of a
polymorphic helper must not collapse to one monotype).

The running carrier (1B, `crates/ir/src/passes/types/`) has ZERO generalization
machinery — confirmed:
`grep -rniE 'generali|instantiat|polymorph|scheme' crates/ir/src/passes/types/` = **1 hit**,
a lone doc-comment (`constraint/mod.rs:1`, "generalized `csp-solver` crate" — unrelated).

Does let-generalization graft **additively** into 1B's running CSP, or does it force
the **union-find re-seed** that would re-inflate 1B's build-cost to 1A's and re-open INC-1?
And is "1A and 1B are one architecture" real?

---

## METHOD

1. **Fixture** (`crates/core/tests/proto1_false_fusion.rs`, `structural: true` so helper
   rules are not inlined away): a shared helper `pair` referenced from two sites.
   ```
   top = a b ;   a = pair pair ;   b = pair pair ;
   pair = k v ;  k = /[a-z]+/ -> f64 ;  v = /[0-9]+/ -> u8 ;
   ```
2. **Run** the real `project_types` via `compile_grammar`; read `ir.types` keyed by rule
   name; count `pair` entries; compare `a` vs `b`.
3. **Anatomy read** of the solver to decide additive-vs-reseed:
   `constraint/reference.rs` (RefConstraint), `constraint/domain.rs` (TypeDomain),
   `generate.rs` (var allocation), `mod.rs` (propagate driver), `types/node.rs` (IrNode).

Single cargo invocation, `--profile ax-iter`, `-p bbnf`, four test binaries:
`proto1_false_fusion` + `project_types_json` + `project_types_sheets` + `project_types_css_l4`.

---

## WHAT RAN — THE NUMBERS

`cargo test -p bbnf --profile ax-iter` (exit 0):

```
=== PROTO-1 ir.types (rule -> TypeDesc), structural mode ===
  a        -> Tuple([BoxedEnum, BoxedEnum])
  b        -> Tuple([BoxedEnum, BoxedEnum])
  k        -> F64
  pair     -> Tuple([F64, U8])
  top      -> Tuple([BoxedEnum, BoxedEnum])
  v        -> U8
pair entry count in ir.types = 1
a = Some(Tuple([BoxedEnum, BoxedEnum]))
b = Some(Tuple([BoxedEnum, BoxedEnum]))

proto1_false_fusion   : 1 passed; 0 failed
project_types_json    : 10 passed; 0 failed
project_types_sheets  : 6 passed; 0 failed
project_types_css_l4  : 4 passed; 0 failed
```

- **`pair` has exactly ONE type entry** (one `RuleId` → one `TypeVarId`, measured count = 1).
- **Both use-sites fuse to that one monotype**: `a == b == Tuple([BoxedEnum, BoxedEnum])`
  (each Ref to the compound `pair` collapses to `BoxedEnum` per RefConstraint, identically
  at every site).
- **Goldens byte-identical**: json/sheets/css_l4 `project_types` all green, unchanged.
- **Runtime diff**: `git diff ce51c36fc -- crates/core/src/runtime/` = **0 lines**.

## THE ANATOMY (why the number is what it is)

- `generate.rs:29,176` — `rule_vars: HashMap<RuleId, TypeVarId>`; every `Ref(rid)` bridges
  to `self.rule_vars[rid]`, the **single** shared var for that rule. There is no per-use var.
- `constraint/domain.rs` — `TypeDomain { solved: Option<TypeDesc> }`; the solver is a
  **monotone lattice-join** fixed-point (`join`: `None→Some`, or refine). No unification,
  no substitution, no occurs-check, no union-find, no scheme.
- `constraint/reference.rs` — `RefConstraint::revise` reads the target rule's solved slot
  and propagates it (scalar) or collapses to `BoxedEnum` (compound). It **reads one shared
  rule var**; it never instantiates a fresh copy.
- `types/node.rs` — `IrNode` inter-rule linkage is **`Ref(RuleId)` only**. There is NO
  `Call` / `Apply` / `Lambda` / `Closure` / type-parametric construct. (`grep GrammarCall|
  type_param|TypeParam|forall` over `crates/core/src` + `crates/ir/src` = 0 type-level hits;
  the only "closure" facts are host-`Map` env-liveness in the e-graph, not type polymorphism.)

## VERDICT

**The probe REFUTES its own premise. False-fusion is VACUOUS in the carrier; let-generalization
is INAPPLICABLE — neither "shipped free" nor "a missing floor."**

1. **1B fuses both uses to one monotype** (measured: `pair` count = 1, `a == b`). There is
   **no** per-Ref fresh instantiation.
2. **This is NOT false fusion.** BBNF grammar rules are **monomorphic by construction**:
   `pair = k v` has exactly one body, hence exactly one type, with **zero free type variables**.
   `Ref(RuleId)` is a monomorphic reference — no call site can fill a rule's body with a
   different type, because there is no type-parametric application in `IrNode`. Two uses of a
   helper therefore *genuinely* share one type; fusing them is **correct**, not unsound.
   The literal PASS condition ("two uses resolve to two DISTINCT monotypes") is **unreachable —
   and would be a soundness *bug* if reached.**
3. **Generalization has nothing to quantify.** `∀ᾱ.τ` over `ᾱ = ∅` is `τ`; instantiating a
   0-ary scheme is the identity. A graft added on top would be a **provable no-op** on every
   existing grammar — trivially additive, trivially 0-diff on the goldens, and worth **nothing**:
   it builds no false-fusion floor because there is no false fusion to prevent. (Per no-workarounds /
   parsimony, a decorative no-op that manufactures a hollow "additive PASS" was declined.)
4. **A *meaningful* graft is NON-additive.** To make instantiation observable you must first
   introduce polymorphism (grammar functions with type params / a `GrammarCall`-style
   applicative construct — a **language feature absent from the carrier**), then give each use
   site its **own** type var unifiable against a **fresh copy** of the target's scheme. That
   requires a substitution/instantiation layer — i.e. exactly the **union-find re-seed** the
   probe feared. It does **not** compose with 1B's monotone lattice-join over per-*rule* ground
   vars.

**Fork adjudication (1A ⊥ 1B).** At the polymorphism seam, 1A (banked HM(X): generalize +
fresh-instantiate + substitution) and 1B (monotone lattice-join, monomorphic rules) are **NOT
one architecture** — the graft between them is a re-seed, not a diff. **But the seam is EMPTY**:
the grammar type system has no polymorphic rules, so 1B **owes no generalization layer** and pays
nothing for the divergence. INC-1 does **not** re-open on build-cost: 1B stays **LEAN precisely
because it correctly omits a floor the carrier has no use for.** 1B's cheapest-additive-diff claim
**survives** — not by grafting HM(X), but by the carrier's monomorphism making HM(X)
let-generalization inapplicable.

**Mapping to the brief's binary:** neither clean PASS nor the FAIL-as-stated. PASS is unreachable
(and unsound); FAIL's consequence ("1A's re-seed re-inflates 1B → INC-1 reopens") does **not**
fire, because the re-seed is **unowed**. The honest disposition is **REFUTATION-AS-LAW**:

> *In a monomorphic grammar type system, per-rule let-generalization is not a soundness floor —
> it is a category import from HM(X) with no referent. 1B ships false-fusion soundness vacuously;
> 1A's generalize/instantiate layer is non-additive to 1B but unowed; build-cost stays LEAN-1B.*

---

## LEDGER

| item | value |
|---|---|
| HEAD measured | `ce51c36fcbdf5108eb028005586672d9a6d2ca1b` (base-pin, not stale) |
| loadavg | 8.09 / 6.44 / 5.91 |
| `pair` type entries | **1** (one rule → one var) |
| `a` vs `b` | identical `Tuple([BoxedEnum, BoxedEnum])` (fused — correctly, monomorphic) |
| project_types goldens | json 10 / sheets 6 / css_l4 4 — **all pass, byte-identical** |
| runtime diff vs base | **0 lines** (`crates/core/src/runtime/`) |
| graft written | **none** — meaningful graft is non-additive (re-seed) AND unowed; vacuous graft declined |
| polymorphic IrNode construct | **none** (`Ref(RuleId)` only; no Call/Apply/Lambda) |
| code delivered | `crates/core/tests/proto1_false_fusion.rs` (test fixture only, +76 lines, non-runtime) |

**PASS/FAIL line:** REFUTATION — false-fusion vacuous; let-generalization inapplicable to the
monomorphic carrier; meaningful graft is a non-additive union-find re-seed but **unowed**;
1B's LEAN build-cost claim **survives**; INC-1 stays closed.
