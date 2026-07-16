# PROTO-1 — LOCK-3 pass-2 probe 1: the single-owner `:204` thread + CONSUME/parity gate

**Family:** 3A (CONTRACT-TOWER, the CONSUME contract) + 3B (SINGLE-IR, the `Project.ty@204`
quadruple-home discharge), unified to one owner.
**Unifies:** research-A T3 = research-B step3 = research-C P2 = research-D step3.
**HEAD measured on:** `c93fc43143743ffaf54855c526f6aeab79227352` (+ dirty working tree: the probe patch).
**Base pin:** `ce51c36fc` — verified ancestor of HEAD after `git merge --ff-only c93fc4314`
(HEAD was stale at `d2b32f6e3`, an ancestor of the pin; fast-forwarded cleanly; c93fc4314 itself
descends from ce51c36fc). **No wrong-artifact-risk.**
**loadavg during runs:** ~10.6–11.8 (1-min), machine up 3 days; other agents concurrent.

---

## QUESTION

Does threading the dropped `->` type at `passes/src/lib.rs:204` into `node_types` and populating
`DirectBuildField.target` from it let the **DERIVED** typed schema reproduce the hand
`xtask/src/real_typed_schema.rs` baseline **byte-identically** (CONSUME, not re-derive) — and does a
per-pass predicate read **RED on HEAD / GREEN after**?

Two separable claims live inside that one sentence:

- **(A) The `:204` thread + parity gate** — thread `Project.ty` into `node_types`; a
  `verify_project_types` predicate reads RED before / GREEN after; zero emit footprint.
- **(B) The CONSUME reproduction** — populate `DirectBuildField.target` from the threaded type and
  have that make the DERIVED typed-direct schema reproduce the hand `real_typed_schema.rs`
  byte-for-byte, as the **SOLE** producer on the typed path (Q-3A-2: no vacuous relocation).

---

## METHOD (what ran)

Smallest probe that answers the question, executed on the real tree:

1. Added `Type::Declared(String)` to `passes::Type` (the one variant that can *carry* a declared
   `->` type verbatim; no external exhaustive match on `passes::Type` exists in the workspace, so the
   addition is closed).
2. Added `pub fn verify_project_types(grammar, facts) -> Result<(), Vec<SourceSpan>>` in
   `passes::layout::types`: for every `Project` with `ty == Some(declared)`, `Err` unless
   `node_types[project] == Type::Declared(declared)` **and** `node_types[project] != node_types[body]`.
3. Added `crates/passes/tests/verify_project_types_probe.rs` — parses the **sole `->` witness**
   (`grammar/google-sheets/google-sheets.bbnf`), runs `infer`, prints the RED/GREEN verdict + offending
   spans; asserts `json.bbnf` (zero arrows) is vacuously GREEN.
4. **RED measurement:** ran the probe with `:204` UNCHANGED (`Project { body, .. } => infer_expr(body)`).
5. **STEP-1 thread:** changed `:204` to consume `ty` — `Some(declared) => Type::Declared(declared)`,
   `None => body_ty` (body still inferred so its own `node_types` entry is populated).
6. **GREEN measurement:** re-ran the probe.
7. **Zero-footprint:** ran `check-real-typed`, `check-json`, `check-sheets` on the HEAD baseline
   (predicate-out) and again with the thread in place (predicate-in).
8. Traced the real-typed emit path end-to-end to test claim (B).

`cargo` ran inside the isolated worktree's own target dir (no shared-target contention). parse-that
untouched (READ-ONLY, not needed).

---

## THE NUMBERS

### (A) — RED→GREEN, EXECUTED

| stage | `:204` | `verify_project_types` on Sheets |
|---|---|---|
| HEAD baseline | drops `ty` (`.. => infer_expr(body)`) | **RED** — `Err`, 4 of 4 declared projections drop their type; spans `[628..658, 2388..2435, 3039..3079, 4278..4319]` |
| after thread | consumes `ty` | **GREEN** — `Ok(())`, all 4 declared projections carry their type |

The 4 spans are exactly the `-> input : Span` projections (`string`, `sheet_prefix`, `cell_ref`,
`identifier`). `json.bbnf` has **zero** declared projections → vacuously GREEN (asserted). This is
**G-3A-1 EXECUTED** (Sheets witness), not merely specified.

### (A) — zero emit footprint, EXECUTED (G-3A-2)

| command | HEAD (predicate-out) | after thread (predicate-in) |
|---|---|---|
| `check-real-typed` | EXIT 0 | EXIT 0 |
| `check-json` | EXIT 0 | EXIT 0 |
| `check-sheets` | EXIT 0 | EXIT 0 |

All three byte-identical with and without the thread. Threading `ty` into `node_types` changes the
attribute table only; the emitter output is untouched. `git diff --stat` code footprint: **51 net
lines in `passes/src/lib.rs`** + a 61-line probe test; **0 generated files touched.**

### derive-path arrow-lex: 0 (satisfied)

`grep -c classify_projection crates/passes/src/**` = **0**. `classify_projection` lives only in
`crates/codegen/src/general_descent/view.rs` (the Sheets *codegen* path), never the passes *derive*
path. No arrow re-lex on the derive path — already clean.

### (B) — CONSUME reproduction: **REFUTED (measured)**

`DirectBuildField.target` is `Option<DirectBuildTarget>` and is **`None` at all 11 construction
sites** in `derive_materialization_plan`; **`Some(..)` appears nowhere** in the workspace;
`DirectBuildTarget { .. }` is **never constructed** outside its definition. The field is dead.

The real-typed emit path proves the field is not merely unpopulated but **unread**:

```
emit_typed_from_source(json, source, schema:&DirectSchemaSet)
  -> passes::compile(grammar)                 // produces backend_ir w/ DirectBuildField.target = None
  -> emit_typed_with_layout(backend_ir, ..., schema)
       -> lower::schema_direct::lower_program(sink_only, schema)
            = TypedDirectProgram { schema: schema.clone(),           // <- HAND schema, cloned whole
                                   direct_shape_roster: <shape NAMES only> }
       -> json_typed_direct::render(&typed)
            reads schema.roots (:50), schema.types (:70);            // <- ALL types from HAND schema
            direct_shape_roster used once, as a COMMENT (:19)        // <- derived side = one comment
```

So the DERIVED side contributes exactly **one comment line** (a comma-joined shape-name roster); every
byte of typed structure comes from the cloned hand `DirectSchemaSet`. `check-real-typed` is GREEN
**because the hand schema still drives everything** — its GREEN is evidence *against* CONSUME, not for
it (this is precisely Q-3A-2's *vacuous CONSUME*, in its strongest form: not relocation-via-fallback
but a field that is never on the wire).

Three independent walls make (B) unreachable by "set `target = Some(threaded_ty)`":
1. **Type wall.** threaded `ty` is a `String`; `DirectBuildField.target` is
   `Option<DirectBuildTarget>` — a 6-field struct (`rust_field`, `type_ref`, `presence`,
   `cardinality`, `representation`, `decode`). `Some(threaded_ty)` does not typecheck.
2. **Wiring wall.** even a fully-populated `target` is never read by `json_typed_direct::render`;
   the render consumes the `schema` parameter, so `target` cannot be the SOLE producer until
   `emit_typed_from_source`'s hand-`schema` parameter is *deleted* and `lower_program` is taught to
   *synthesize* `DirectSchemaSet` from `sink_only` — the 3B substrate fold (G-3B-1), a
   crate-spanning retirement, not this probe.
3. **Corpus wall.** the real-typed corpus is `json.bbnf`, which has **zero arrows** and none of the
   hand schema's per-benchmark detail (root struct names `TwitterSearch`/`CitmCatalog`/…, capacities
   `1_264`/`30`/`10_001`, string-enum variant tables). A bare-grammar derive cannot reproduce the
   1014-line hand schema byte-for-byte for the STRUCT subset regardless of `target`; the information
   is not in the grammar.

---

## VERDICT

**RAN_MEASURED.** The unified single-owner `:204` thread + parity gate — the 3A organ that pass-1
discharged in *none* — is now **DISCHARGED and EXECUTED**: `verify_project_types` reads RED (4 spans)
on HEAD and GREEN after the thread, with byte-identical `check-json`/`check-sheets`/`check-real-typed`
predicate-in vs predicate-out (zero footprint) and 0 `classify_projection` on the derive path.

The CONSUME half of the question — that populating `DirectBuildField.target` from the threaded type
makes the DERIVED schema reproduce the hand `real_typed_schema.rs` byte-identically — is **REFUTED as
stated**. `DirectBuildField.target` is a dead field on the typed path (never populated, never read;
the render clones the hand `DirectSchemaSet` wholesale), and three walls (type / wiring / corpus)
place true CONSUME behind the 3B substrate fold (G-3B-1, delete `emit_typed_from_source`'s schema
param + synthesize `DirectSchemaSet` from `sink_only` + resolve JSON's zero-arrow corpus), which this
parsimonious probe deliberately did not attempt. **The gate is real and green; the quadruple-home
`target` discharge is not reachable by a one-line `target = Some(ty)` and must be carried to LOCK-3
pass-3 / the 3B fold.**

The JSON sum subset (needs `DirectTypeKind::Sum`) was out of scope, carried born-RED on GN-3.W1 — not
touched.

---

## PATCH

`crates/passes/src/lib.rs` (+51/−1) and `crates/passes/tests/verify_project_types_probe.rs` (+61).
Full diff in `PROTO-1.patch` (co-delivered). Core change at `:204`:

```rust
ExprKind::Project { body, ty, .. } => {
    // CONSUME the declared `->` type. Body still inferred so its own
    // node_types entry is populated, but the projected node's type is the
    // author's declaration when present — the arrow is threaded, not dropped.
    let body_ty = self.infer_expr(*body)?;
    match ty {
        Some(declared) => Type::Declared(declared.clone()),
        None => body_ty,
    }
}
```

plus `Type::Declared(String)` and `verify_project_types` (the parity predicate).
