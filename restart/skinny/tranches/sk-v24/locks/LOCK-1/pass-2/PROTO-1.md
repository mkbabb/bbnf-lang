# PROTO-1 — LOCK-1 pass-2, probe 1 (family 1D / Open R, the structural axis)

**Question.** Does the engine-agnostic record widening (**Open R** = `TypeDesc::Row`
+ `LayoutKind::Row` + a pure-IR structural classifier) actually **close the masked
List≠Row fallback** on the SHIPPING grammar `grammar/json/json.bbnf` — or is it
**blocked** behind a LOCK-7 totality decision + the GN-3.W3 wrap gate?

**Verdict — BLOCKED (1D BLOCKED-candidate → routes to GESTALT-B).**
The structural axis is a **LOCK-7 ⊕ LOCK-3 blocker.** Open R lands in `crates/ir`
(`bbnf-ir`); the mask lives in a **separate cargo workspace** (`skinny/`) whose
codegen never consumes `bbnf-ir`'s type projection. The classifier cannot reach
the mask without first unifying the two divorced codepaths — a totality decision.
The GN-3.W3 wrap gate, notably, is **NOT** the blocker (see §4).

- **BASE-PIN.** HEAD `ce51c36fcbdf5108eb028005586672d9a6d2ca1b` — exactly the pin
  `ce51c36fc`. `git merge-base --is-ancestor ce51c36fc HEAD` ⇒ true. Not stale.
  No wrong-artifact-risk. loadavg at launch 3.89; XL-H single-cargo discipline held.
- **Numbers are RAN_MEASURED** on that HEAD (main-path type projection), except
  the full Open-R landing, which was deliberately **not** attempted — see §5 (parsimony).

---

## 1. The decisive structural fact: two divorced `ir` crates, two workspaces

| | package | home of | consumed by the mask? |
|---|---|---|---|
| `crates/ir` | **`bbnf-ir`** | `TypeDesc`, `LayoutKind`, `StructRegistry`, the type-projection CSP, `passes/types/constraint/` (Open R's target + where `shape.rs` would live) | **no** |
| `skinny/crates/ir` | **`ir`** | `BackendIr`, `BackendExpr`, `BackendShape`, `SinkOnlyProgram` (the mask's substrate) | yes |

Measured facts (HEAD ce51c36fc):

- `skinny/Cargo.toml` declares its **own** `[workspace]`; the main `Cargo.toml`
  `members` list does **not** include `skinny`. Two independent workspaces.
- `skinny/crates/codegen` depends on `ir.workspace = true`, which resolves to
  `skinny/crates/ir` (package `ir`), **not** `bbnf-ir`.
- `grep -rn "TypeDesc\|StructRegistry\|LayoutKind\|struct_registry" skinny/crates/`
  ⇒ **zero hits.** The skinny codegen references none of Open R's types.
- The mask is a plain **rule-NAME predicate**: `runtime_generator.rs:98`
  `if !sink_only.has_rule("value")` where `has_rule` (sink_only.rs:31) is
  `self.rules.iter().any(|rule| rule.name == name)` — a `String` compare on
  `BackendRule.name`. It reads no projected type, no layout, no key role.
- `crates/runtime/` **does not exist** (the DISSOLVE clause's `git diff crates/runtime/`
  targets a path absent at this base); the runtime lives at `skinny/crates/runtime`.

**Consequence.** Even a perfectly-landed `TypeDesc::Row` + `LayoutKind::Row` +
`shape.rs` in `bbnf-ir`, yielding `class(object)==Row ∧ class(array)==List`, would
be produced in a crate the skinny mask never links against. The classification
**cannot cross the workspace boundary** to inform `has_rule("value")`. Closing the
mask therefore requires first **unifying skinny's sink-only lowering with bbnf-ir's
type-projection classifier** (or teaching `emit_general_descent` to consume a
Row/List classification) — a LOCK-7 totality decision, entangled with LOCK-3
(the codegen-path unification). This is exactly the BLOCKED disposition.

---

## 2. The refusal, reproduced (class(object) vs class(array))

On the **unchanged** shipping grammar the type projection already runs
(`crates/core/tests/project_types_json.rs`, `compile_paths_request(Vm)` →
`project_types` → `ir.struct_registry`). Its recorded invariants:

- `array = "[" >> (( value << comma ? ) *)?w << "]"` → layout carries a
  **`FieldSource::RepeatElement`** field ⇒ **List-shaped** (`Vec<value>`).
- `object = "{" >> (( pair << comma ? ) *)?w << "}"` → structurally **also a
  `Repeat`** (`Vec<pair>`); `pair` → `LayoutKind::Struct` (key + value).

The registry's discriminator alphabet is `{Struct, TaggedEnum, UntaggedEnum,
NewtypeWrapper}` — **there is no `Row`.** So `object` and `array` both collapse to
the same repeat/list shape; `object` is **indistinguishable** from a List. That is
the **clause-i defect** / "masked List≠Row fallback": the projection has no category
in which a repeat-of-key-value-pairs is a *record*. `class(object) ≈ Unknown`
(no Row category; masquerades as List), `class(array) = List`. **Refusal reproduced.**

---

## 3. Where the List/Row distinction is actually *baked* (the hand-authored mask)

Because bbnf-ir cannot name a Row, JSON is special-cased in skinny: the
`has_rule("value")` branch routes the JSON family to a **hand-authored template**
(`json_templates/value.rs`) whose `enum JsonValue { Object(..), Array(..), .. }`
hard-codes the Object(Row)/Array(List) split. A grammar *without* a `value` rule
(Sheets) falls to `emit_general_descent`, which has **no Object/Array (Row/List)
notion at all** (`grep Row\|List\|is_object\|is_array` over `general_descent/*.rs`
⇒ nothing structural). So the List≠Row knowledge lives *only* in the hand-written
JSON template — the mask — and nowhere in a derived classifier.

---

## 4. GN-3.W3 wrap gate — CLEARED (not the blocker) — RAN_MEASURED

The brief's honest key-role form was authored on the real grammar and driven
through the **main** lowering path (where `wrap.rs:242` fires):

```diff
 string = /".../"  -> decode_json_string_to_arena(input) : String ;
+keystr = /"[^"]*"/ -> input : key ;
 array  = ... ;
-pair = string, colon >> value ;
+pair = keystr, colon >> value ;
```

`cargo test -p bbnf --profile ax-iter --test project_types_json` (loadavg 3.89):

```
test result: ok. 10 passed; 0 failed; 0 ignored; finished in 0.01s   (exit 0)
```

The annotation `-> input : key` **lowered cleanly** — `wrap.rs:242`
(`mapped_factor: … contains a -> arrow but structural detection found no value_expr
head — typed-materialization invariant violated`) did **not** fire, and the full
registry-closure gate (all 10 assertions incl. audit `Mapped`, zero Pending/Missing)
still held with `pair` rebuilt on `keystr`. Static reading agrees: `input` surfaces
as the `value_expr_head` (identical detection to the shipping `decode_json_string_to_arena(input)`),
so `value_expr_head.is_none()` is false and the panic guard is skipped.

**⇒ The second BLOCKED clause via GN-3.W3 does NOT hold. The annotation is
admissible.** This narrows the blocker to the workspace divorce alone (§1) and
discharges CRIT-2 directive 3 (the pass's largest blind spot) with a real run:
the wrap trap is not what stops Open R.

---

## 5. Why the mask cannot delete without regressing (the operative BLOCKED clause)

Deleting `has_rule("value")` routes JSON to `emit_general_descent`, which emits a
**different 5-file roster** (general recursive descent + lazy tape view) in place of
the 8-file hand-authored JSON template — a **regression** (JSON loses its
Object/Array template and its specialized `value.rs`/`view.rs`/`visitor.rs`), and
the general path has **no Row/List classifier to substitute**. Open R cannot supply
the missing classification because it lives across the workspace boundary (§1).
So: *the mask cannot delete without regressing* — the operative BLOCKED condition —
**and** Open R cannot repair the regression from where it sits. Not attempted
destructively; deletion's outcome is determined by structure, not worth a wrong-tree regen.

**Parsimony note (why Open R was not fully landed).** `TypeDesc::` appears across
**102 files** and `LayoutKind::` across ~30 source files + generated grammars + tests;
adding a variant to each exhaustive match, then `cargo xtask regen` over all 9
corpora, is a multi-hour landing. Crucially it would **not change the verdict**: the
classification it produces lands in `bbnf-ir`, unreachable by the skinny mask. The
smallest probe that answers the question is the divorce proof (§1) + the GN-3.W3
run (§4). SPEC_ONLY was avoided — the dispositive claim (§1) and the gate (§4) both RAN.

---

## 6. Verdict line

**BLOCKED — 1D BLOCKED-candidate → GESTALT-B.** Open R does **not** close the masked
List≠Row fallback on the shipping grammar by itself. It is blocked behind a **LOCK-7
totality decision** (unify skinny's sink-only/general-descent codegen — package `ir`,
its own workspace — with bbnf-ir's `StructRegistry`/`TypeDesc` classifier), entangled
with **LOCK-3** (codegen-path unification). The GN-3.W3 wrap gate is **cleared**, not
the blocker: the honest `-> input : key` key role is admissible and closes projection
on the real grammar. The structural axis moves decisively to a LOCK-7⊕LOCK-3 blocker.

## Appendix — artifacts / cleanliness

- Intentional change: `grammar/json/json.bbnf` (+`keystr`, `pair` rewired) — patch: `PROTO-1.patch`.
- `git diff` also lists `crates/core/src/grammar/generated/{bnf,css_l4,google_sheets}.rs`
  + `crates/ir/src/passes/types/constraint/revise.rs` — **pre-existing worktree
  churn / build-manifest side-effects, NOT this probe** (no Row/json-annotation
  content; revise.rs is `TypeObligation::TypeClash` comment churn).
- `git diff skinny/` ⇒ **empty.** The mask path is pristine — a bbnf-ir-side
  annotation touched nothing in the runtime, corroborating the divorce.
