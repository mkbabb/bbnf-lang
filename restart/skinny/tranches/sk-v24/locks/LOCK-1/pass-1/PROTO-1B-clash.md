# PROTO-1B — CSP-equality soundness: does `TypeDomain::join` swallow a genuine clash?

**LOCK-1 (the solver spine · type inference & solving) · pass-1 · probe 1 (family 1B)**
**Family:** 1B — the surviving equality carrier (adjudicates 1A vs 1B).
**Status:** RAN — MEASURED (not SPEC_ONLY).

> Naming note: the pass-1 directory already holds a *different* `PROTO-1.md`
> ("Judgment-mix census", cross-family residue axis, delivered 04:02). To avoid
> clobbering that deliverable this probe ships as `PROTO-1B-clash.md`.

---

## Question

The brief asserts the shipping root-tree equality carrier is
`TypeDomain::join` (`crates/ir/src/passes/types/constraint/domain.rs:72-77`),
whose disagreement arm — `(Some(existing), Some(ty)) if existing != ty` —
re-assigns to a *different* type ("the solver may refine"): non-monotone
last-writer-wins, i.e. Wand's principal-type-losing shape.

Does the LIVE pass raise a clash on two incompatible `->` arrows, or silently
pick one leaf? This decides whether the running CSP can be the spine's equality
carrier as-is, or must be repaired to raise-on-disagreement.

## Method

Two tests in `crates/ir/tests/lattices/clash.rs`, registered via
`mod clash;` in `lattices/main.rs`, modelled on the hand-built fixture
`heterogeneous_css_alt_surfaces_named_obligation` (lattices/types.rs:521):

1. **`heterogeneous_scalar_alt_bool_f64_raises_obligation`** — the LIVE pass.
   ONE rule whose two `->` projections ground to incompatible leaves
   (`Bool` and `F64`) unifying into one type var:

   ```
   rule 0 = Alt(Ref(1), Ref(2))          -> two leaves unify into rule 0's var
   rule 1 = Map(Lit, f0)  f0 = Expr{ return Bool }
   rule 2 = Map(Lit, f1)  f1 = Expr{ return F64  }
   ```

   Runs `project_types` (types/mod.rs:51) then inspects `ir.types[0]` and
   `ir.type_obligations`.

2. **`type_domain_join_is_last_writer_wins_on_clash`** — the cited carrier in
   isolation: call `TypeDomain::join` directly on `Bool` vs `F64`.

**Command:** `cargo test -p bbnf-ir --test lattices clash -- --nocapture`
(isolated `CARGO_TARGET_DIR` in scratchpad; loadavg recorded).

## What actually ran — the numbers

- loadavg at launch: `6.97 7.36 7.96` (1/5/15-min).
- `cargo test -p bbnf-ir --test lattices clash` (exit 0):
  `test result: ok. 2 passed; 0 failed; 0 ignored; 0 measured; 81 filtered out; finished in 0.00s`
  - `clash::type_domain_join_is_last_writer_wins_on_clash ... ok`
  - `clash::heterogeneous_scalar_alt_bool_f64_raises_obligation ... ok`
- Live-pass probe output (`--nocapture`):
  - `ir.types[rule 0]` = `HeterogeneousAltJoin([Bool, F64])`
  - `ir.type_obligations` = `[HeterogeneousAltJoin { rule: Some(0), node: Some(n2), branches: [Bool, F64] }]`

## Structural findings (static, load-bearing)

Three facts about the shipping code that reframe the brief's premise:

1. **`TypeDomain::join` is dead code on the live path.** `grep -rn '\.join('`
   over `crates/ir/src` shows the only `.join` calls are `FactWidth::join`
   (egraph/analysis/facts.rs:223) and a `str` `.join("|")` (egraph/rules/
   regex.rs:300). The `LatticeDomain::join` impl at domain.rs:66-80 has **zero
   call sites** in the type pass. The monotonic solver
   (`csp-solver/src/solver/monotonic.rs:29-44`) drives propagation purely
   through `constraint.revise(...)`; it never calls `domain.join`.

2. **The real equality carrier is `assign` (revise.rs:27-35)** — invoked by
   `GroundConstraint`, `EqualConstraint`, `RefConstraint`, the operator
   constraints. It is *also* last-writer-wins: `*slot = Some(ty)` with no clash
   detection, returning `true` whenever the value changed. So the brief's
   "non-monotone last-writer-wins" characterisation is **correct in substance**
   but **mis-attributed** to `join`; the live carrier `assign` has the identical
   non-monotone shape.

3. **The monotonic loop has no iteration cap** (monotonic.rs:29-44): it sweeps
   all constraints until a pass reports no `Changed`. Two incompatible grounds
   on one variable would therefore **flip-flop forever** (each `assign` reports
   `Changed`), i.e. non-termination — strictly worse than "silently pick one
   leaf". Test 2 demonstrates the per-step oscillation directly.

## The load-bearing distinction

The tree-structured constraint generator (`generate.rs`) never places two
incompatible *ground* constraints on a single scalar variable: every node gets
its own fresh var, and equality only *propagates* one defining ground. The ONE
site where two genuinely different types converge on one var is the **Alt
join** — and that path does **not** go through `assign`/`join`. It routes
through `join_types` (revise.rs:116-142), which on disagreement lifts the
distinct branch leaves into `TypeDesc::HeterogeneousAltJoin(...)` and
`project_types` surfaces a matching `TypeObligation::HeterogeneousAltJoin`
naming the rule (mod.rs:501-551, 480-486). That is **raise-on-disagreement**,
per AZ-III invariant 7 (no silent fallback) — the opposite of last-writer-wins.

So the live clash site is sound; the last-writer-wins carriers (`join` dead,
`assign` live) are a **latent hazard** that is unreachable for clashes under
well-formed tree IR but is un-guarded in principle.

## Verdict

**RAN — MEASURED. Split verdict, decisive both ways.** On the LIVE pass the only
reachable clash site (Alt join) yields `ir.types[0] = HeterogeneousAltJoin([Bool,
F64])` (not a single leaf) plus one `HeterogeneousAltJoin { rule: Some(0) }`
obligation → **PASS** by the brief's criterion (the pass does NOT silently pick a
leaf). BUT the brief's cited carrier `TypeDomain::join` is **dead code**, and the
live carrier `assign` is last-writer-wins under an uncapped loop → the carrier
*as cited* is unsound-in-principle and its 1B-equality claim must be hardened to
raise-on-disagreement, ceding distinctiveness to 1A as the brief anticipated.

For the probe's literal PASS/FAIL on the LIVE pass: the run yields
`ir.types[0] = HeterogeneousAltJoin([Bool, F64])` (NOT a single ground leaf)
AND `>=1` `TypeObligation::HeterogeneousAltJoin { rule: Some(0) }` naming the
rule → **PASS** by the brief's stated criterion. The shipping pass does **not**
silently pick one leaf on two incompatible `->` arrows at its only reachable
clash site.

BUT the brief's named artefact `TypeDomain::join` is refuted as "the equality
carrier": it is dead code. The genuine equality carrier `assign` (and the dead
`join`) are last-writer-wins with an uncapped loop — sound only because the
generator never manufactures a scalar two-ground clash. This is decisive for
the pass-2 carrier choice: **1B's equality can ship for the reachable surface,
but the carrier must be hardened to raise-on-disagreement (union-find /
clash-obligation) to be sound in principle** — ceding the distinctiveness point
to 1A exactly as the brief anticipated. Soundness of the carrier *as literally
cited* is disproven (dead code); soundness of the *live* clash path is proven
(Alt join raises).

## Repair (the minimal hardening, if 1B is chosen)

`assign` (revise.rs:27) and `TypeDomain::join` (domain.rs:66-80) should, on
`Some(existing) != Some(ty)`, record a `TypeObligation::TypeClash { var, a, b }`
and refuse the overwrite (keep `existing`, return `Unchanged`) instead of
flipping. That converts the latent non-termination/last-writer hazard into an
explicit named obligation, matching the Alt-join discipline already shipping.
