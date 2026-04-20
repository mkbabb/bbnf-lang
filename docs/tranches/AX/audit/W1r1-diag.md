# AX.W1r.1 — Scope-reveal diag

## Summary

W1r.1 refactor executed cleanly (static `BINDINGS` + `COLOR_FIELDS` +
`resolve_named_type` + `NamedTypeBinding` deleted; `RustNamedTypes`
rebuilt as an IR-walker populating `FxHashMap<StringId,
Vec<TypeDesc>>` at `from_ir` time). All 7 parity harnesses, plus
`payload_layouts` (13), `css_color_parity` (4), and
`css_l4_color_view` (23), remain green. Workspace build clean.

Hard-gate #1 (`cargo expand -p bbnf --lib | grep -c "fn
__named_type_shim" ≥ 1`) **cannot be satisfied** with the current
pipeline. The underlying cause is upstream of this sub-wave's file
bounds and is documented here.

## Finding — `ir.types` carries no `TypeDesc::Named` in the Rust
## backend path

Empirical probe (instrumented `emit_direct_to_struct_projection`
with a panic forcing any `TypeDesc::Named(_)` entry in `ir.types` to
surface):

| Grammar | `types_entries` | `named_rules` |
| --- | ---: | --- |
| JsonParser | 8 | `[]` |
| BbnfParser | 17 | `[]` |
| EbnfParser | 12 | `[]` |
| BnfParser | 5 | `[]` |
| CssParser | 15 | `[]` |
| GoogleSheetsParser | 31 | `[]` |

Zero grammars produce any `Named(sid)` in `ir.types` when the Rust
emitter's `analyze_grammar → project_types` pipeline runs.

Cross-check via the VM-target pipeline (`CompileTarget::Vm`) shows
a different picture:

- **JSON**: `string = /regex/ -> decode_json_string_to_arena(input)
  : String` → `rule 3 (string) type=Named(12)` (StringId 12 =
  "String"). Activates the `universal_named_shape` fallback at
  `crates/ir/src/passes/payload/layout.rs:436`, which projects
  `"String" | "str" | "Bytes"` to `(U32, U32)` arena-handle shape.
- **CSS L4**: No Named survives. The three `-> input : Color`
  rules (`colorFunction`, `colorFn`, `colorMix` in
  `grammar/css/l4/color.bbnf`) behave as follows:
  - `colorFunction` and `colorMix` are eliminated entirely by some
    earlier pass — they do not appear in `ir.rules` nor `ir.types`.
  - `colorFn` survives as rule 3 but its type is projected to
    `Tuple([Span, U8, BoxedEnum, BoxedEnum, BoxedEnum,
    Option(BoxedEnum)])` — the structural tuple of the Seq body,
    not `Named("Color")`. The `-> input : Color` projection is
    lost.

The Rust backend's `prepare_grammar → analyze_grammar → project_types`
path evidently runs additional rewrites between IR construction and
type projection that the VM path does not. Neither path currently
produces `Named(Color)` / `Named(ColorMix)`, but the VM path at
least preserves `Named(String)` for JSON; the Rust path does not
surface even that.

## Consequence for W1r.1

1. The static `BINDINGS` table the sub-wave was tasked with removing
   was **entirely dead code** in every grammar. No rule reached the
   Rust-side `resolve_named_type` callsite in
   `emit_direct_to_struct_projection`; no `PROJECTION_DIRECT_TO_STRUCT`
   const was ever emitted; no `.as_<name>()` shim ever fired on the
   per-leaf view path. Deleting the table is a correct code hygiene
   improvement with zero behavioural impact.
2. The IR-derived builder that replaces the static table also admits
   zero bindings for every grammar in the current tree — same reason.
   The resolver's `bindings: FxHashMap<StringId, Vec<TypeDesc>>` is
   empty on every compile. `resolve_named` returns `None` for every
   sid. The layout pass's `universal_named_shape` fallback continues
   to carry JSON's `"String"` projection on the VM path.
3. Hard-gate #1 (`grep -c "fn __named_type_shim" ≥ 1`) assumes the
   builder populates at least one binding per grammar; on the
   current pipeline this is false. The gate is unsatisfiable
   without pipeline changes outside W1r.1's file bounds (IR passes
   that strip `TypeDesc::Named` from rule types, either in
   `project_types` or in the Rust-specific `prepare_grammar`
   pre-passes, must be adjusted to preserve the annotation through
   to `analyze_grammar`).

## Hard-gate status (post-refactor)

| Gate | Status | Notes |
| --- | --- | --- |
| #1 `grep -c "fn __named_type_shim" ≥ 1` | **FAIL (0)** | No grammar reaches the admission path; no shims emitted. |
| #2 `cargo test -p bbnf --test payload_layouts` | PASS (13/0) | Layouts unchanged from pre-W1r.1 baseline. |
| #3 7 parity harnesses | PASS (3/9/2/16/9/13/25) | Matches spec'd counts exactly. |
| #4 `cargo build --workspace` | PASS | Clean modulo pre-existing gorgeous unreachable warns. |
| #5 `grep 'pub static BINDINGS\|const COLOR_FIELDS' named_types.rs` | PASS (0) | Static tables deleted. |

## Scope-reveal recommendation

Treat this diag as informational, not blocking. The refactor's code
merit (dead-code elimination, resolver now IR-driven and ready to
populate when upstream produces Named) stands on its own. Gate #1
surfaces an invariant-violation upstream of W1r.1:

- **Option A** (minimal): Accept the empty-binding steady state as
  the current reality and strike gate #1 from W1r.1. The rest of
  W1r's waves (canonical-form parity, typed-accessor surface audit)
  operate on `NodeView` + tape accessors, not on named-type
  projections; W1r.1's value is code hygiene, not enabling new
  shape routing.
- **Option B** (deeper): Open a follow-up sub-wave to preserve the
  `-> input : <Name>` annotation through the Rust backend's
  `prepare_grammar`. This would restore the pipeline the spec
  implicitly assumes, but the investigation touches `bbnf_ir::passes`
  + `crates/core/src/backend/driver` — well outside W1r.1's file
  bounds.

## Artefacts

- `crates/core/src/backend/rust/view/named_types.rs` rewritten
  (148 LOC; down from 178).
- `crates/core/src/backend/rust/emitter/grammar.rs`
  `emit_direct_to_struct_projection` rewritten to consult the new
  resolver + emit `__named_type_shim_<name>` markers (inert on
  current grammars).
- `crates/core/tests/css_color_parity.rs` rewritten to exercise the
  new builder contract (4 tests, all passing).
- `crates/core/tests/css_l4_color_view.rs`
  `rust_named_types_resolves_color_but_not_foreign_names` rewritten
  to `rust_named_types_resolver_is_ir_derived` (asserts empty
  bindings on a string-only IR — the inverse of the pre-W1r.1
  static-table assumption).

## Commit decision

Refactor committed as `feat(view): IR-derived named-type resolver
replaces static BINDINGS (AX.W1r.1)` with this diag cited in the
commit body. Orchestrator may cherry-pick freely; the dead-code
removal is independent of the upstream `TypeDesc::Named` preservation
question.
