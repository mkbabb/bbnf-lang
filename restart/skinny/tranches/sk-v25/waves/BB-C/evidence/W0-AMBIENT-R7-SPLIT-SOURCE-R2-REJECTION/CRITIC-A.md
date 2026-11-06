# BB-C.W0 ambient R7 split-source R2 — critic A

Date: 2026-07-22 EDT  
Role: fresh hostile type/lowering/backend/generator critic  
Model: UNKNOWN  
Reasoning effort: UNKNOWN

The runtime exposed no independently verifiable model or reasoning-effort
metadata. Neither value is inferred.

## Verdict

**REJECT. Isolated regeneration is not authorized.** No file was edited,
staged, regenerated, or otherwise mutated by this critic.

## Custody

- Subject SHA-256:
  `070f8ca84d80f95e8502d53b44c6df96973d6fd9e116356dd670f9a2ad4d0cdb`.
- Predecessor SHA-256:
  `46c299064f28947663955da158ae69e4c90b28e114e85033690f5f181bdcec43`.
- All 97 ledger members exist and match.
- Total bytes: `1093436`.
- Ordered path digest:
  `5879c5761fa83bd898eab414471c8620e0907aa0f76d864bf8163ca05f692f96`.
- Ordered ledger digest:
  `6907807ebbd4ede5f58df3603676efc990506629ade7149513b57f7b36a9d195`.
- The subject is explicitly born RED and grants no regeneration authority.

## Decisive blockers

### 1. The ledger omits controlling TypeScript source — INCORRECT custody boundary

`crates/core/src/backend/ts/projection.rs:92` changes
`compile_map_expr_to_js` to require `return_type`, but the required integration
edit in `crates/core/src/backend/ts/emitter/value.rs:63` is dirty and absent
from the ledger. Its assayed SHA-256 is
`fc09ef13de386dbe9499aef5daaed49b0b64ae54dd96deab4a3b6c0a2d11f382`;
it threads the new argument into the call at line 87.

Include this exact authority in the next receipt and test through
`TsEmitter::emit_map_expr`, not only the projection helper.

### 2. Signed grammar literals are not exact — INCORRECT

- `grammar/bbnf/expressions.bbnf:6` recognizes an unsigned magnitude; unary
  minus is separate at line 25.
- `crates/core/src/lower/value_expr/literals.rs:6` range-checks the positive
  magnitude before negation, rejecting valid signed minima.
- `crates/core/src/lower/value_expr/atom.rs:22` represents ordinary negatives
  as `UnaryOp(Neg, IntLit(...))`.
- `crates/core/src/lower/expression/wrap.rs:290` derives suffix widths only
  from direct numeric literal nodes, so `-1i8` loses its width.
- `scalar_payload_from_fn` accepts only direct constants
  (`crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs:38`).
  Some downstream paths then produce Unit or no payload.
- Wasm passes every non-direct expression through unchanged
  (`crates/core/src/backend/wasm/emitter/value.rs:82`), so grammar-lowered
  `-1` bypasses the advertised sentinel rejection.
- Tests construct `IntLit(i64::MIN)` directly and omit the corresponding
  grammar-to-backend path.

Lower syntactic negative constants with sign-aware checked magnitude, retain
suffix type, fold constant unary/composite expressions before shared scalar
decoding, and cover every signed minimum plus adjacent overflow through
grammar→IR→Rust/TypeScript/Wasm.

### 3. Rust does not preserve exactly one semantic payload — INCORRECT

1. Untyped nonnegative `IntLit` becomes U64 in
   `alt_dispatch/branches.rs:62`, contradicting its signed spelling.
2. Flat emission counts `StringLit` as emitted at
   `flat/struct_direct.rs:106`, but shared scalar decoding has no string case
   and the path emits no value at lines 204–215.
3. Inline `node_contains_map` is existential
   (`inline/structural_branch.rs:342`). A map under zero repetition, negation,
   subtraction, or only one alternative can suppress the fallback although a
   successful path commits no payload.

Keep untyped `IntLit` signed unless an explicit return type says otherwise;
emit exact strings or reject them; replace existential detection with a
successful-path committed-payload fact.

### 4. Tape is not closed — INCORRECT overall

The 16-byte/alignment-4 layout, full checkpoint restoration, private record
fields/constructors, U64 sentinel handling, and typed leaf checks are correct.
The release boundary is not:

- public `TapeRef` fields and `TapeCursor::new` permit cross-wired arena/record
  references and out-of-range offsets (`runtime/tape/cursor.rs:18`);
- safe `bind_input` lifetime-extends an arbitrary slice with transmute
  (`runtime/tape/mod.rs:158`); and
- `begin_compound` masks `layout.rule_id` to five bits
  (`runtime/tape/mod.rs:184`), aliasing rules 32 apart while tests stop at 31.

Seal cursor/reference construction behind the builder, use real input
ownership/lifetimes, checked arena access, and exact layout identity.

### 5. Semantic Pratt is source-aligned but fresh serialization is wrong

The Pratt emitter and lowerer correctly use alternating operands and semantic
U8 tags and exclude single operands. However:

- `xtask/src/regen_simple_runtime.rs:3662` serializes BBNF `Tag` as decimal,
  while lines 3722–3728 insert `" - "` between every BinaryFactor child.
  `[lhs, Tag(0), rhs]` becomes `lhs - 0 - rhs`, not `lhs << rhs`.
- Sheets owns `=`→3, `<`→4, `>`→5 in
  `grammar/google-sheets/google-sheets.bbnf:97`, while
  `regen_simple_runtime.rs:1438` decodes tags 3/4/5 as `<`/`>`/`=`.
  The relevant self-parity tests are outside the receipt.

Serializer lexemes must derive from the same precedence authority. Govern BBNF
`<<`, `>>`, `-`, and mixed chains plus all Sheets comparison round trips.

### 6. Generator values are not universally exact or fail-closed — INCORRECT

BBNF and Sheets exact I64/U64/U8 carriers and JSON's zero-only assertion are
correct. The shared Simple builder nevertheless silently maps F64, I64, U64,
and Bool to Unit at `crates/core/src/runtime/builder_template.rs:265`, while
`regen_simple_runtime.rs:237` does not reject an incompatible registry.

Add exact variants or reject unsupported scalar domains during generation and
prove the rejection with an adversarial wide-scalar fixture.

### 7. CSS closure is materially incomplete — INCORRECT

Functional/predefined frames, checked tags/components, checked HexColor U32,
complete span-stack rollback, and the 49 configured Length L4 units are correct
as narrow facts. The total claim is false:

1. `colorMix` has no projection route; only `colorFunction` and `colorFn` are
   accepted (`css_l4.toml:616`, `regen_css.rs:761`).
2. `deposit_value` never fills ColorMix left/right colors or percentages
   (`regen_css.rs:1001`).
3. `namedColor` maps packed U32 but has no color frame; fallback Wrap produces
   generic `CssTypedValue::Unsigned`.
4. hex uses illegal `{3,8}` rather than exact 3/4/6/8 lengths, and its host
   fabricates zero on unsupported input (`runtime/css_l4/host.rs:17`).
5. percentage color components become Dimension values while ColorFunction
   only collects direct F64 pushes, so close may fail.

Use exact hex alternatives and a fallible host; add canonical named-color
ownership; either correctly route nested colors/percentages in source order or
remove out-of-scope machinery; prove typed behavior and rollback.

## Numbered receipt classification

1. Structural/semantic identity plus the body/recovery 65,536 gates:
   **CORRECT**.
2. Tape footprint, rollback, sealed record fields, typed discriminators:
   **CORRECT narrowly**; complete safe Tape closure: **INCORRECT**.
3. Exactly one Rust payload through every shape: **INCORRECT**. The HRegex
   integer decoder itself is **CORRECT**.
4. Semantic Pratt producer/reducer and single-operand exclusion:
   **CORRECT narrowly**; generated serializer compatibility: **INCORRECT**.
5. Grammar→IR→Rust/TypeScript/Wasm integer exactness: **INCORRECT**. UIntLit
   transport/folding and direct literal helpers are **CORRECT narrowly**.
6. Generator values exact or fail closed: **INCORRECT**.
7. Complete CSS closure: **INCORRECT**.
8. Zero `push_branch_tag` inside governed non-generated source/tests:
   **CORRECT as scoped**; product clean break remains unproven.

## Verification and wave fit

- `git diff --check` and the scoped no-`push_branch_tag` census are CORRECT.
- Cargo/rustfmt claims lack immutable logs tied to the receipt and were not
  independently rerun: UNPROVEN by this critic.
- `runtime_projection_semantics.rs` mostly asserts source substrings or directly
  constructs values; it does not prove parsers, serializers, or CSS routes.
- Runtime rendering consumes generated registry sidecars outside the ledger,
  and grammar/runtime regeneration are separate commands; a clean ordered
  transaction is UNPROVEN.
- `scalar_routing.rs` advertises an emitter consumer for a predicate with no
  production callers, and its expression range rule is not conservative.
- R2 earns **no BB-C.W0 credit**. W0 requires G3 deletion, typed optimizer
  failures, ambient fallback removal, call-site inventory, and mutation
  witnesses. This broad incident repair proves none of those completion gates.
- The 1.09-MB incident slice is not proved wave-optimal or the smallest
  necessary precondition.

Repair the exact blockers, include every controlling integration file, and
seal a new source receipt. Only an accepted source may authorize one ordered
grammar→fresh-sidecar→runtime regeneration, followed by a byte-identical second
pass, full compile, and behavioral parity.
