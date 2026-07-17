# PROTO-2 — LOCK-3 (THE IR TRANSPOSITION SPINE), probe 2

**3A contract non-vacuity: does the per-edge Project contract have MEANING, or is it vacuously-true-until-LOCK-1?**

- **BASE-PIN:** HEAD = `ce51c36fcbdf5108eb028005586672d9a6d2ca1b`. This equals the base pin
  `ce51c36fc` exactly; `git merge-base --is-ancestor ce51c36fc HEAD` → true (DESCENDS-OK).
  No wrong-artifact-risk. Numbers below measured on this HEAD.
- **loadavg (record only; no perf claim made):** start 6.71 / 8.04 / 8.75; end 6.96 / 6.96 / 8.02.
- **RAN.** Semantic/structural probe, executed end-to-end. Not SPEC_ONLY.

---

## QUESTION

`verify_project_types` (the LOCK-1 organ — **not yet in the tree**; `grep verify_project_types`
returns nothing) is imagined to assert, per `Project{ty:Some}`:

1. `node_types[project] == resolved(ty)`, and
2. `node_types[project] != node_types[body]`.

The worry: a `name -> Type` wrapper is **nominal**, while `infer_expr(body)` is **structural**,
so clause (2) is vacuously true whenever `ty = Some` — the organ then verifies threading
**presence**, not declared-vs-inferred **consistency**.

**The contract is non-vacuous only if some projection's declared target is genuinely NOT
recoverable from what the body's structural inference recovers.**

## THE ORGAN UNDER TEST

`skinny/crates/passes/src/lib.rs:204`:

```rust
ExprKind::Project { body, .. } => self.infer_expr(*body)?,
```

Inference **discards** the declared `target`/`ty` and returns the body's structural `Type`.
The structural universe collapses hard:
- `ExprKind::Regex{..}` → `Builtin(Span)` (or `Unit` for whitespace) — `lib.rs:194,224`
- `ExprKind::Literal{..}` → `Builtin(Bytes)` — `lib.rs:193`

So every `->` projection body in the fleet lands in exactly one of two structural buckets:
`Span` (regex body) or `Bytes` (literal body). The declared target is what the emitter's
`classify_projection` (`codegen/general_descent/view.rs:35-46`) folds into
`ProjectionKind{Float,Bool,Tag,Span}` — **purely from the (target, ty) strings, never the body.**

## METHOD

`skinny/crates/passes/tests/proj_witness.rs` (new; patch attached):
1. `grammar::parse_grammar("google_sheets", <google-sheets.bbnf>)` — the only fleet grammar
   with `->` (JSON/CSS have zero, so the predicate is vacuously GREEN there and proves nothing).
2. `passes::layout::types::infer(&grammar)` — runs the **real** inference organ; `node_types`
   is populated for every visited expr, including each `Project` body.
3. Walk `grammar.exprs`; for each `ExprKind::Project{body,target,ty}` print
   `(byte-span, target, ty, infer_expr(body), classify(target,ty))`.
4. Bucket by structural body type; decide non-vacuity.

`cd skinny && cargo test -p passes --profile ax-iter --test proj_witness -- --nocapture` → **ok**
(1 passed; build+run 4.66s + 0.00s).

## THE NUMBERS

28 `Project` nodes. Two structural body buckets, from the real `infer_expr`:

| structural body `infer_expr(body)` | declared kinds present | # projections |
|---|---|---|
| `Builtin(Span)` (regex bodies) | **{Float, Bool, Span}** | 7 |
| `Builtin(Bytes)` (literal bodies) | {Tag} | 21 |

- **declared kind NOT structurally recovered from body: 24 / 28.**
- **some structural body maps to >1 declared kind: TRUE.**

The decisive witnesses — three projections with **identical** structural inference `Builtin(Span)`
but **three divergent** declared meanings:

| rule | byte-span | `infer_expr(body)` | declared target | classify |
|---|---|---|---|---|
| `number`  (`/…/ -> f64`)          | 294..335  | `Builtin(Span)`  | `f64`   | Float |
| `string`  (`/…/ -> input : Span`) | 628..658  | `Builtin(Span)`  | `input`/`Span` | Span |
| `boolean` (`/true/ -> true`)      | 844..870  | `Builtin(Span)`  | `true`  | Bool |

`infer_expr(body)` returns `Span` for all three; it cannot tell whether the projection means an
`f64`, a bool literal, or a raw span. The declared target carries the discriminating information
the structural body erases. Identically, all 21 `Bytes`-bodied literal projections
(`error_literal` #N/A…#SPILL! → `0u8`..`8u8`, and the `compare_op`/`add_op`/`mul_op`/`unary_prefix`
tags) carry a `u8` discriminant that `Builtin(Bytes)` completely flattens.

## VERDICT

**PASS — the 3A per-edge contract is NON-VACUOUS in principle.** ≥1 (in fact 24/28) Sheets
projections exhibit a declared target NOT determined by `infer_expr(body)`; a single structural
body type (`Span`) fans out to three declared kinds. Carry **G-3A-a `name->Type`** as a
**born-RED LOCK-1 edge**.

### Load-bearing caveat for the LOCK-1 organ (scope matters, or the fear comes true)

The brief scopes the imagined check to `Project{ty:Some}`. In the Sheets fleet **only 4**
projections carry `: ty`, and **all four are `-> input : Span` over regex bodies** — so within
that narrow scope `infer_expr(body) == Span == resolved(ty)` for every one, and clause (2)
(`!= node_types[body]`) survives **only** on the nominal-name-vs-`Builtin(Span)` type-identity
gap. Scoped to `ty:Some` alone, `verify_project_types` over today's fleet is **presence-only** —
exactly the vacuity worried about.

The non-vacuity is **earned by the target-only projections** (`Project{ty:None}`: `f64`, `true`,
`false`, the `Nu8` tags — 24 of 28). Therefore G-3A-a must assert consistency over the declared
**value category** (`classify_projection`'s `ProjectionKind`), i.e. the full `->` target surface,
**not** the `: ty` nominal wrapper. Written that way the edge is born RED and genuinely fires:
`number`'s `Float`, `boolean`'s `Bool`, and every literal's `Tag` all differ from the structural
`Span`/`Bytes` the body recovers. Written as `ty:Some` + `!=body` only, it verifies threading and
nothing more.

## FILES

- probe (new): `skinny/crates/passes/tests/proj_witness.rs` (patch: `proj_witness.patch`)
- organ under test: `skinny/crates/passes/src/lib.rs:204` (`Project => infer_expr(body)`), `:193-194,224` (Regex→Span, Literal→Bytes)
- classifier cross-check: `skinny/crates/codegen/src/general_descent/view.rs:20-46`
- grammar: `grammar/google-sheets/google-sheets.bbnf` (projections at :6,12,17-18,34-42,52,63,90,97,107,111,117)
