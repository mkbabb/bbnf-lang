# PROTO-PB-4 — Type-plane greenfield skeleton (ANALYSIS INSTRUMENT, never landed)

**Tranche:** SK-V23 / TB4.W1 (the type-plane greenfield). **Date:** 2026-07-09.
**Target repo:** bbnf-lang. **Disposition:** instrument only — nothing lands.
**Build:** single-file `rustc --edition 2021 -O`, zero deps, zero workspace
contact (no cargo, no lock contention). `rustc` exit = 0.

## Question

Does a `TypeDesc` (Named/Aggregate/Scalar) checker over a REAL `->`
type-expression AST compile as scaffolding in the sk-v21 §6 order, with the
arrow-count invariant holding on `json.bbnf` (every `->` reaches exactly one
derived typed projection) and the frozen text-threaded projections a strict
subset — never a loss? Fold over json.bbnf + one more grammar; count arrows vs
derived projections; diff against the frozen `RuntimeTypedProjection` set; name
the two open-ended HM-core surfaces.

## Verdict — PASS on all four success criteria

| Criterion | Result |
|---|---|
| Compiles as sk-v21 §6-order scaffolding | **YES** — `rustc` exit 0; only warnings are dead-code on the deliberately-deferred frontier variants (Tuple/Record/Sum/Var), i.e. exactly the surfaces §6 defers |
| arrow-count == projection-count on json.bbnf | **YES** — 5 arrows == 5 derived projections == step-5 worklist fixed point (5) |
| frozen projections a strict subset, zero lost | **YES** — frozen {4 untyped + 1 typed = 5} ⊆ derived {5 typed}; derived is strictly richer (4 arrows the frozen scan left opaque now carry a `TypeDesc`) |
| two open-ended surfaces named + HM-core seam | **YES** — Surface A `@host`-signature unify; Surface B rank-1 HM instantiate; both dormant (0 holes) on this corpus, exactly as §6 predicts |

## Instrument output (verbatim, cold run)

```
=== grammar/json/json.bbnf ===
arrows(->)            = 5
derived projections   = 5  (step5 fixed point = 5)
frozen: untyped=4 typed=1 total=5
INVARIANT arrow==proj = true
frozen ⊆ derived      = true  (strictly richer: true)
open surf A (host)    = 0 hole(s)
open surf B (HM var)  = 0 var(s)
type histogram        = U8×1 Bool×2 F64×1 Named×1
annotations CHECKED   = 1 (never blind-replaced)

=== grammar/css/l4/easing.bbnf ===
arrows(->)            = 13
derived projections   = 13  (step5 fixed point = 13)
frozen: untyped=13 typed=0 total=13
INVARIANT arrow==proj = true
frozen ⊆ derived      = true  (strictly richer: true)
open surf A (host)    = 0 hole(s)
open surf B (HM var)  = 0 var(s)
type histogram        = U8×13
annotations CHECKED   = 0 (never blind-replaced)
```

Independent cross-checks: `grep -o '\->' json.bbnf | wc -l` = **5** (matches the
scanner). The frozen split model is confirmed against the live skinny fixtures —
`skinny/crates/grammar/src/lib.rs:1754-1755` asserts `Projection=1,
TypedProjection=1`, and `:2043-2060` pins a `RuntimeTypedProjection{ target:
"crate::css_types::parse_hex_color(input)", type_text: "u32" }`, i.e. a
`::`-bearing target with a `: u32` ascription — a case the prototype's
`parse_projection` handles via the prev/next-colon guard.

## What the derived plane types on json.bbnf (the "never lose a type" oracle)

The frozen scanner (`RuntimeProjection`/`RuntimeTypedProjection`,
`grammar/src/lib.rs:181-203`) threads `type_text` **only** when a `: Type`
ascription is present. On json.bbnf that is exactly **one** arrow
(`string … : String`); the other four (`null -> 0u8`, `true`, `false`,
`number -> f64`) are untyped `RuntimeProjection` — their type is opaque to the
frozen plane. The §6-step-1 `MapExpr::synth` recovers all four:

| arrow | frozen record | derived `TypeDesc` |
|---|---|---|
| `null -> 0u8` | Projection (opaque) | **U8** (int-literal suffix synth) |
| `bool -> true` / `-> false` | Projection ×2 (opaque) | **Bool** ×2 |
| `number -> f64` | Projection (opaque) | **F64** (scalar-name shorthand) |
| `string -> decode_json_string_to_arena(input) : String` | **TypedProjection** `type_text="String"` | **Named("String")**, annotation CHECKED |

So `frozen ⊂ derived` strictly: the one frozen typed projection is preserved
verbatim; the four frozen untyped projections gain a real `TypeDesc`. This is the
concrete meaning of "the frozen text-threaded projections a strict subset, never
a loss" — the plane is a superset that recovers the widths the frozen scan drops
(the same width-destruction §6 Step-0/B3 root-causes at
`runtime/bbnf/view.rs:141`).

## The sk-v21 §6 build order, compiled in sequence (load-bearing signature)

Each step is a named function invoked in §6 order; steps 1/3/5 (structural
projection — "for the rest") are implemented, steps 2/4 (the open surfaces) are
explicit holes, step 6 is a deferred stub:

```
step1_synth          — MapExpr::synth, bottom-up over the closed operator set;
                       annotations CHECK, never replace (kills unwrap_or(Span))
step2_host_unify     — HostSigTable + first-order unifier   [OPEN SURFACE A]
step3_resolve_rows   — TypeDesc::Record(Row)/Sum resolution (identity on corpus)
step4_hm_instantiate — rank-1 HM, Algorithm J instantiate    [OPEN SURFACE B]
step5_worklist_fold  — plain Changed-bool worklist (NOT csp; "AC-3" a miscall)
step6_incremental    — Salsa-style shell (deferred)
```

`TypeDesc` mirrors `crates/ir/src/types/type_desc.rs` (same scalar set +
`from_scalar_name` table), extended with two hole variants that keep the open
surfaces non-silent:

```rust
enum TypeDesc {
    Span, F64, Bool, I8,U8,I16,U16,I32,U32,I64,U64,      // scalar (closed)
    Option(Box<_>), Vec(Box<_>), Tuple(Vec<_>),
    Record(Vec<(String,_)>), Sum(Vec<_>),                // §6 step-3 aggregate
    Named(String),                                       // custom map result
    HostSig(String),  // SURFACE A: @host edge — first-order unifier hole
    Var(u32),         // SURFACE B: rank-1 HM var — Algorithm J hole
}
```

## The two open-ended HM-core surfaces (named, with the seam)

Both were 0-hole on json+easing — precisely §6/CRIT-P6's prediction that the
corpus exercises neither. They are the *only* two places a unification variable
is introduced; everything else (steps 1/3/5) is first-order and monotone, so the
HM core is quarantined to exactly these two seams:

- **SURFACE A — `@host` signature edges (§6 step 2).** `HostSigTable` from
  `@host` declarations + a first-order unifier. **Seam:**
  `unify(param_edge_type, arg_type)`. Dormant because the corpus has no
  `@host fn` and lowering drops `FnCall` args (the param edge is unreachable
  today — §6 CRIT-P6, "validated synthetically only"). In the instrument a
  bare host-call target with no ascription becomes `HostSig(target)` — an
  explicit hole, never a silent `Span` default.

- **SURFACE B — rank-1 HM for generic rules (§6 step 4).** Algorithm J
  (union-find + Rémy levels). **Seam:** `instantiate(scheme)` per call-site,
  then `unify`. Dormant because no corpus rule is generic
  (`List : ∀a. Elem(a) -> Vec<a>`), so no `Var` is ever introduced.

**HM-core seam identified:** a single shared union-find substitution serves both
A and B; it is the entire HM core, and steps 1/3/5 never touch it. This is the
empirical basis for §6's disposition that steps 4-6 are DEFERRED until a fleet
grammar demands a generic rule or a host edge — the greenfield can land steps
1-3-5 (structural + honesty) now and gate the unifier behind first real demand.

## Caveats / honest edges

- The instrument scans the `->` boundaries with a re-implementation of
  `parse_runtime_projection`'s delimiter logic (skip quotes/regex/`//`, break on
  `| ; \n ,` at depth 0, single-`:` ascription split) rather than consuming a
  full grammar AST — sufficient to make the arrow set *identical* to the frozen
  scanner's, which is what the subset claim rests on. A landed TB4.W1 checker
  would fold over the real parsed `MapExpr`, not text.
- `parse_type_expr` covers scalars + `Vec<>`/`Option<>` + `Named`; it does not
  parse `Record`/`Sum` literals (absent from the corpus). Those variants compile
  but are unconstructed here — the honest signal that §6 step 3 has no corpus
  witness.
- Numbers are directional (this is a compile+count instrument, not a perf
  probe); no PMU applies.

## Bottom line for TB4.W1

The greenfield type plane is well-founded: a `TypeDesc` checker in the §6 order
compiles today, the arrow-count invariant is an executable falsifier that PASSES
on both grammars (5==5, 13==13), the frozen projection set is a strict
subset the plane never loses (and enriches — 4/5 json arrows gain a width the
frozen scan drops), and the whole HM core reduces to two named, currently-dormant
seams. TB4.W1 can land steps 1/3/5 as the structural plane and gate steps 2/4/6
behind first real demand, exactly as sk-v21 §6 dispositions.
