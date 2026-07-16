# PROTO-2 (iso) — The carrier's iso floor: does the winning record cross to TypeScript?

LOCK-1 (THE SOLVER SPINE — type inference & solving), pass-2, probe 2 —
**the carrier iso axis** (family carrier axis 1A ⊥ 1B / Constitution-iii
isomorphism). Discharges CRIT-2 directive 5 and closes G-XL-2, the lock's
largest un-probed surface.

> Delivered as `PROTO-2-iso.md` (not `PROTO-2.md`) deliberately: the slot
> `PROTO-2.md` is already held by a **distinct** sibling probe —
> "Row-inference vs hint" (Family 1D / Open R), the probe that authored the
> `TypeShapeAnalysis` classifier and landed the `TypeDesc::Row` vocabulary.
> That vocabulary is exactly "Brief 1's vocabulary" this probe builds on.
> Overwriting `PROTO-2.md` would destroy the Open-R deliverable, so this
> report and its patch take a distinct name.

- **HEAD measured on:** `ce51c36fcbdf5108eb028005586672d9a6d2ca1b`
- **Base-pin:** `ce51c36fc`. `git merge-base --is-ancestor ce51c36fc HEAD` =
  **YES**; `HEAD == ce51c36fc`. **No wrong-artifact-risk on HEAD.**
- **Working-tree artifact:** HEAD **+** Brief-1's *uncommitted* Row vocabulary
  (the prior Open-R PROTO-2's working tree: `TypeDesc::Row` variant +
  `type_desc_to_ts`/`type_desc_to_syn` Row arms + `TypeShapeAnalysis`
  `shape.rs`, all uncommitted, 51 insertions across 12 files + 1 untracked
  classifier). This is the intended "share Brief 1's vocabulary" artifact.
  The `HeterogeneousAltJoin` (Sum) vocabulary is **pristine HEAD** — already
  present and projected at `ce51c36fc`.
- **Result:** **RAN, measured.** `cargo test -p bbnf --test carrier_iso_ts` →
  `test result: ok. 4 passed; 0 failed`.
- **Loadavg** at run ≈ 4.5 (categorical string/shape assertions, not timing —
  loadavg irrelevant to the numbers).

## Question

Does the winning carrier's **type-of-record** — a Row-bearing Sum (a two-arm
tagged alternation, each arm a keyed record) — cross to TypeScript with a
**well-formed parallel type**? This is the Constitution-iii iso floor that
decides the family carrier axis (1A ⊥ 1B): carrier **1B** is the `TypeDesc`
widening carrier, and it seats on iso **iff** the record vocabulary
(`TypeDesc::Row`, `TypeDesc::HeterogeneousAltJoin`) has a `type_desc_to_ts`
path that names the **same** record shape the Rust sink names. Build-cost
(Option A's race) alone cannot settle the fork; this closes it on a fresh axis.

## Method (what ran)

**The Row-bearing Sum vocabulary, in the real IR type language:**
- `Row` (Brief-1) — `crates/ir/src/types/type_desc.rs:48`, produced by
  `passes::types::constraint::shape::TypeShapeAnalysis` (Fact 3: a `->`-declared
  key role on the repeated element → `ShapeClass::Row`).
- `HeterogeneousAltJoin(Vec<TypeDesc>)` (pristine HEAD) —
  `type_desc.rs:72`, the Sum obligation produced by
  `passes::types::constraint::revise::join_types` when an `Alt`'s branch types
  disagree. `HeterogeneousAltJoin([Row, Row])` **is** the Row-bearing Sum in
  the existing vocabulary; no new `Sum` variant is needed.

Both are written into `ir.types` by `project_types`
(`crates/ir/src/passes/types/mod.rs`) — the exact `Vec<(RuleId, TypeDesc)>`
that the two LOCK-3 sinks clone verbatim: `TypeAnalysis::from_ir`
(`driver/analysis.rs:22`) and `RustNamedTypes::from_ir`
(`rust/view/named_types.rs:100`).

**Test authored:** `crates/core/tests/carrier_iso_ts.rs` (4 tests, all RAN),
projecting the vocabulary through **both** production sinks and asserting the
shapes are parallel:

| `TypeDesc` | TS sink `type_desc_to_ts` | Rust sink `type_desc_to_syn` | Parallel? |
|---|---|---|---|
| `Row(F64)` | `Record<string, number>` | `Vec<(::parse_that::Span<'a>, f64)>` | ✅ key→value, same value leg |
| `Row(Span)` | `Record<string, Span>` | `Vec<(Span<'a>, Span<'a>)>` | ✅ |
| `HeterogeneousAltJoin([Row(Span), Row(F64)])` | `CarrierNode` (union name) | `CarrierNodeView<'a>` (boxed-enum view) | ✅ iso-**symmetric** name reference |
| `Vec<Row(F64)>` | `Record<string, number>[]` | `&'a [Vec<(Span<'a>, f64)>]` | ✅ composes under list ctor |

A 4th test (`carrier_type_surface_reaches_both_backends`) drives the **full
pipeline** end-to-end: a two-arm alternation grammar compiles via
`compile_grammar_request` to **both** backends; the TS surface emits the
discriminated union (`tag: "item"` / `tag: "list"`) and the Rust backend
realises the same grammar's type surface without error — the Sum crossing
end-to-end, not just at the projection function.

## The numbers

```
running 4 tests
test carrier_row_composes_under_vec ... ok
test carrier_row_crosses_to_ts_parallel ... ok
test carrier_sum_of_rows_crosses_to_ts_parallel ... ok
test carrier_type_surface_reaches_both_backends ... ok
test result: ok. 4 passed; 0 failed; 0 ignored
```

- **Iso pairing (the brief's named target):** Rust `Vec<(Span,T)>` ↔ TS
  `Record<string, T>` for the Row arm; Rust boxed-enum view ↔ TS union name for
  the Sum arm — the discriminated-union / keyed-record duality the brief names,
  **byte-for-byte the two production projections emit it.**
- **Iso-safety (runtime untouched):** `git diff -- crates/core/src/runtime/`
  = **0 lines** (the runtime crate is `crates/core/src/runtime/`; there is no
  `crates/runtime/`). Brief-1's Row landing touches **no** runtime file
  either. The Row/Sum vocabulary crosses to TS **without any runtime change.**
- **Negative (the discriminating fact):** `grep -rc 'TypeENode|monotype' over
  crates/core/src/backend/` = **ZERO**. The **sole** `TypeDesc → TS` sink is
  `type_desc_to_ts`, dispatching on `TypeDesc` variants only. A **fused 1A
  monotype** (a single collapsed representation) and a **1D `TypeENode`**
  (an e-graph node) are **not** `TypeDesc` values — **neither has any
  `type_desc_to_ts` path.** Choosing 1A or 1D would require authoring a
  brand-new `*_to_ts` sink from scratch before any TS mirror exists. **1B is
  the only carrier that already crosses.**

## Verdict

**PASS — 1B's iso-safety is MEASURED.** The Row/Sum-bearing `TypeDesc` crosses
to a well-formed **parallel** TS type on both projection sinks (`Row →
Record<string,T>` ↔ `Vec<(Span,T)>`; `HeterogeneousAltJoin([Row,Row]) →`
union-name ↔ boxed-enum-view, iso-symmetric), **and** `git diff
crates/…/runtime/` is empty. **The carrier seats on iso independently of
build-cost.** Concretely: a fused **1A monotype** and a **1D `TypeENode`** have
**NO** `type_desc_to_ts` path — 1B (the `TypeDesc` widening carrier) is the
only family member with a TS mirror today. This discharges CRIT-2 directive 5
(a TS mirror shown for the record before the carrier is chosen).

The record **vocabulary** needs **no** XL-C / LOCK-6 amendment — it is already
iso-complete for the Row-bearing Sum.

## Deliverables & provenance

- `PROTO-2-iso.md` — this report.
- `PROTO-2-iso.patch` — the sole file I authored: `crates/core/tests/carrier_iso_ts.rs`.
- **Dependency:** the test requires Brief-1's Row working-tree changes (the prior
  Open-R `PROTO-2.patch`, already in this delivery dir) to be applied; the Sum
  half runs on pristine `ce51c36fc`.
