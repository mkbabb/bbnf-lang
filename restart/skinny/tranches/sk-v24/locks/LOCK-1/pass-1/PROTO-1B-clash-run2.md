# PROTO-1B (run 2) — CSP-equality soundness: does `TypeDomain::join` swallow a genuine clash?

**LOCK-1 (the solver spine · type inference & solving) · pass-1 · probe 1 (family 1B)**
**Family:** 1B — the surviving equality carrier (adjudicates 1A vs 1B).
**Status:** RAN — MEASURED (not SPEC_ONLY).

> Naming note. The pass-1 directory already holds a *different* `PROTO-1.md`
> ("Judgment-mix census", 04:02) and a prior `PROTO-1B-clash.md` (04:48) that ran
> this same probe. To clobber neither, this **independent replication** ships as
> `PROTO-1B-clash-run2.*`. It reaches the identical verdict via an independently
> authored fixture and adds one strictly stronger assertion (order-dependence /
> non-commutativity of `join`) that the earlier run did not make.

---

## Question

The brief asserts the shipping root-tree equality carrier is `TypeDomain::join`
(`crates/ir/src/passes/types/constraint/domain.rs:66-80`), whose disagreement arm
— `(Some(existing), Some(ty)) if existing != ty` — re-assigns to a *different*
type ("the solver may refine"): non-monotone last-writer-wins, i.e. Wand's
principal-type-losing shape.

Does the LIVE `project_types` pass raise a clash on two incompatible `->` arrows,
or silently pick one leaf? This gates whether the running CSP can be the spine's
equality carrier as-is (1B), or must be repaired to raise-on-disagreement
(union-find behaviour, ceding distinctiveness to 1A).

## Method

Two RUNNING tests in a new `crates/ir/tests/lattices/clash.rs`, registered via
`mod clash;` in `lattices/main.rs`, modelled on the hand-built fixture
`heterogeneous_css_alt_surfaces_named_obligation` (lattices/types.rs:521). One
dev-dependency added (`csp-solver` under `[dev-dependencies]`) so the
integration-test crate can name `csp_solver::domain::LatticeDomain` and call
`TypeDomain::join` directly — a *regular* dep is not nameable from a `tests/`
crate.

1. **`type_domain_join_is_silent_last_writer_wins`** — the cited carrier in
   isolation. Ground a `TypeDomain` to `Bool`, then `join` a `F64` ground into
   it. Assert on: the return signal, the resulting leaf, and (new this run) the
   *reverse* write order — a sound commutative LUB / union-find carrier is
   order-independent.

2. **`heterogeneous_arrows_into_one_var_via_live_pass`** — the brief's prescribed
   pass-level method. ONE rule whose two `->` projections ground to incompatible
   leaves unifying into one type var:

   ```
   clash = a | b ;                     # rule 0 = Alt(Ref(1), Ref(2))
   a = Map(Lit, f0)   f0 = -> Bool     # rule 1
   b = Map(Lit, f1)   f1 = -> F64      # rule 2
   ```

   Run `project_types` (types/mod.rs:51); inspect `ir.types[0]` and
   `ir.type_obligations`.

**Command:** `cargo test -p bbnf-ir --test lattices clash -- --nocapture`

## What actually ran — the numbers

- loadavg at launch: `13.53 15.74 12.07` (1/5/15-min); build `1m 07s`.
- Exit 0. `test result: ok. 2 passed; 0 failed; 0 ignored; 0 measured; 81 filtered out; finished in 0.00s`
  - `clash::type_domain_join_is_silent_last_writer_wins ... ok`
  - `clash::heterogeneous_arrows_into_one_var_via_live_pass ... ok`
- Live-pass `--nocapture` output:
  - `[clash-probe] solved TypeDesc for rule 0 (\`clash\`): HeterogeneousAltJoin([Bool, F64])`
  - `[clash-probe] total type_obligations: 1`
  - `[clash-probe] clash obligations naming rule 0: 1`
  - `[clash-probe] is_single_silently_chosen_ground_leaf: false`
- Unit-probe assertions that passed (so are true of the shipping code):
  - `TypeDomain::ground(Bool).join(&ground(F64))` returns `true` and leaves `Some(F64)`.
  - `TypeDomain::ground(F64).join(&ground(Bool))` returns `Some(Bool)`.
  - The two orderings **disagree** → `join` is non-commutative last-writer-wins.

## Structural findings (static, load-bearing)

1. **`TypeDomain::join` is dead on the live path.** Every type constraint's
   `revise` (Ground, Equal, Ref, Optional, Repeat, Project, Map, Alt) writes
   through `revise::assign` (`revise.rs:27-35`), a bare `*slot = Some(ty)`.
   The monotonic solver (`csp-solver/src/solver/monotonic.rs:29-44`) and AC-3
   both drive propagation purely through `constraint.revise(...)`; neither ever
   calls `domain.join`. The `LatticeDomain::join` impl at domain.rs:66-80 has
   **zero call sites** in the type pass.

2. **The genuine equality carrier is `assign`, and it is *also*
   last-writer-wins** — `*slot = Some(ty)` with no clash channel, returning
   `true` on any change. So the brief's "non-monotone last-writer-wins"
   characterisation is **correct in substance but mis-attributed**: the live
   carrier `assign` has the identical shape; `join` merely mirrors it while
   dead.

3. **The only site where two genuinely different types converge on one variable
   is the Alt join**, and it does **not** route through `assign`/`join`. It runs
   `join_types` (`revise.rs:116-142`): on disagreement it lifts the distinct
   branch leaves into `TypeDesc::HeterogeneousAltJoin(...)` and `project_types`
   surfaces a matching `TypeObligation::HeterogeneousAltJoin` naming the rule.
   That is **raise-on-disagreement** (AZ-III invariant 7, no silent fallback) —
   the opposite of last-writer-wins. The tree constraint generator never places
   two conflicting *ground* constraints on one scalar var, so the `assign`/`join`
   last-writer hazard is **unreachable for clashes under well-formed tree IR**.

## Verdict

**RAN — MEASURED. Split, decisive both ways.**

- **Literal probe PASS/FAIL on the LIVE pass → PASS.** The run yields
  `ir.types[0] = HeterogeneousAltJoin([Bool, F64])` (NOT a single ground leaf)
  **and** `1 ≥ 1` `TypeObligation::HeterogeneousAltJoin { rule: Some(0) }` naming
  the rule. The shipping pass does **not** silently pick one leaf on two
  incompatible `->` arrows at its only reachable clash site.

- **The brief's named artefact `TypeDomain::join` is refuted as "the equality
  carrier": it is dead code.** In isolation it *does* swallow a genuine clash —
  proven directly: it returns the ordinary `true` "changed" signal (no clash
  channel), overwrites `Bool` with `F64`, and is order-dependent
  (`Bool→F64` ⇒ `F64`; `F64→Bool` ⇒ `Bool`). It is not a commutative LUB and
  cannot be an equality carrier as shipped — but it never runs, so no production
  bug flows through it.

**Gate for the pass-2 carrier choice:** 1B's equality **can ship for the
reachable surface** (Alt-join raises correctly), **but the carrier must be
hardened to raise-on-disagreement** in `assign`/`join` to be sound in principle,
which cedes the distinctiveness point to 1A exactly as the brief anticipated.
Soundness of the carrier *as literally cited* (`join`) is disproven (dead +
unsound-in-isolation); soundness of the *live* clash path (Alt join) is proven.

This run **independently confirms** `PROTO-1B-clash.md` (04:48): same live-pass
type/obligation, same dead-`join` / live-`assign` attribution, with the added
non-commutativity proof.

## Repair (minimal hardening, if 1B is chosen)

In `assign` (revise.rs:27) and `TypeDomain::join` (domain.rs:66-80): on
`Some(existing) != Some(ty)`, record a `TypeObligation::TypeClash { var, a, b }`
and refuse the overwrite (keep `existing`, return `Unchanged`) rather than flip.
That converts the latent non-monotone / order-dependent hazard into an explicit
named obligation, matching the Alt-join `HeterogeneousAltJoin` discipline already
shipping.

## Artifacts

- `crates/ir/tests/lattices/clash.rs` (new, +2 tests) — see
  `PROTO-1B-clash-run2.patch.diff`.
- `crates/ir/tests/lattices/main.rs` — `mod clash;`.
- `crates/ir/Cargo.toml` — `csp-solver` dev-dependency (probe-only).
- Full console: `PROTO-1B-clash-run2.log`.
