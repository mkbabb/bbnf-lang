# BB-C.W0 ambient R7 split-source R2 — critic B

Date: 2026-07-22 EDT  
Role: fresh hostile structural/Tape/CSS critic  
Model: GPT-5  
Reasoning effort: UNKNOWN

The model value is the metadata exposed to the critic. No reasoning-effort
metadata was exposed, so none is inferred.

## Custody

- Subject: `W0-AMBIENT-R7-SPLIT-SOURCE-R2-SUBJECT/AUTHOR.md`
- Subject SHA-256:
  `070f8ca84d80f95e8502d53b44c6df96973d6fd9e116356dd670f9a2ad4d0cdb`
- Reproduced twice: `97` files / `1093436` bytes.
- Reproduced ordered path digest:
  `5879c5761fa83bd898eab414471c8620e0907aa0f76d864bf8163ca05f692f96`.
- Reproduced ordered ledger digest:
  `6907807ebbd4ede5f58df3603676efc990506629ade7149513b57f7b36a9d195`.
- Host: ARM64.
- No file was edited, staged, regenerated, or otherwise mutated by this critic.

## Verdict

**REJECT. Isolated regeneration is not authorized.** R2 earns no wave, lock,
tranche, or movement credit.

## Fact classification

### 1. Structural/semantic identity and recovery bounds — CORRECT in source formation

`AltBranchId(u16)` represents the exact 65,536-branch domain. Both rule bodies
and recovery directives are validated at the early and post-compile gates.
Runtime integration is still unproven because the generated fleet is stale.

### 2. Tape invariants — INCORRECT overall

Checkpoint restoration, typed leaf checks, record-field encapsulation, and the
U64 sentinel repair are correct. Four independent release blockers remain:

1. `TapeRef` exposes forgeable fields and `TapeCursor::new` accepts an unchecked
   offset; `rec()` then indexes without validation
   (`crates/core/src/runtime/tape/cursor.rs:18`).
2. `begin_compound` masks `StructLayout::rule_id` to five bits, aliasing every
   layout beyond 32 (`crates/core/src/runtime/tape/mod.rs:184`).
3. safe `StructBuilder::bind_input` extends an arbitrary input lifetime with
   `transmute` (`crates/core/src/runtime/tape/mod.rs:158`).
4. public unchecked arena reads amplify the forged-reference boundary.

The minimal repair is one sealed cursor/reference construction path, input
ownership or lifetime binding at construction rather than transmute, and exact
layout identity without masking.

### 3. Exactly one semantic payload — INCORRECT

The centralized checked scalar decoder, success-only mapped continuations,
HRegex integer decoder, and semantic Pratt U8 tag emission are sound in the
governed source. Two graph holes remain:

1. Flat emission classifies `MapExpr::StringLit` as payload-emitting, suppressing
   the span fallback, but its scalar path emits no string payload
   (`crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs:109`).
2. Inline `node_contains_map` is existential, not a proof that every successful
   path commits a payload. Maps under zero repetitions, negation, subtraction,
   or only some alternatives can suppress the unit fallback while committing no
   value (`crates/core/src/backend/rust/emitter/shapes/inline/structural_branch.rs:342`).

Implement actual string projection and replace existential map detection with a
committed-payload fact over each successful path.

### 4. Semantic Pratt lowering — CORRECT in source; UNPROVEN end to end

The emitter deposits repeatable U8 operator leaves and the reducer accepts the
alternating semantic-tag chain. The stale generated BBNF view lacks the
corresponding `tag_value`, so no integration acceptance is available.

### 5. Exact integer literals — INCORRECT overall

`UIntLit(u64)`, checked folding, TypeScript bigint projection, Wasm fail-closed
handling, and positive `u64::MAX` lowering are correct. Grammar lowering cannot
represent `i64::MIN`: the positive magnitude overflows before unary negation.
The repair must parse sign and magnitude jointly, allowing `i64::MAX + 1` only
under a negative signed literal.

### 6. Generated-authority exactness — MIXED

- BBNF UInt/u64 generator source: CORRECT.
- Sheets exact signed/unsigned generator source and focused test: CORRECT.
- JSON zero-only assertion: CORRECT.
- CSS claim: overstated. Generic CSS preserves U64 as `Unsigned`; only the
  HexColor frame rejects values outside U32.
- Generated-fleet integration and the absence of generated hand edits:
  UNPROVEN by this source receipt.

### 7. CSS roles and exactness — INCORRECT overall

Functional/predefined frame separation, duplicate checks, full span-stack
checkpointing, and the configured Values L4 length-unit set are correct. The
current grammar and builder still contain material semantic defects:

1. hex accepts illegal five- and seven-digit forms through `{3,8}`
   (`grammar/css/l4/color.bbnf:189`);
2. `currentcolor` is frozen to opaque black instead of a context-dependent
   semantic variant (`grammar/css/l4/color.bbnf:56`);
3. ColorMix has no effective route and its required left/right operands are
   never populated by value deposition;
4. `namedColor` has no route to `CssColor::Named`; and
5. percentage components in color functions have no behavioral proof and are
   structurally suspect under current deposition.

Use exact 3/4/6/8 hex alternatives, a distinct CurrentColor value, one canonical
named-color owner, one explicit nested-color ownership path, and behavioral
percentage/nesting tests. Do not add a scanner or fallback default.

### 8. Clean break — CORRECT only inside the governed non-generated slice

The 97-file source authority contains no `push_branch_tag`. The live generated
fleet still contains the old surface and fails compilation, so product-level
clean-break status is UNPROVEN.

## Verification disposition

- Targeted UInt/registry tests: 17/17 pass.
- Runtime-projection semantics: 3/3 pass.
- Core integration compile: approximately 886 stale-fleet errors.
- Full `bbnf-ir`: 391 tests pass before totality rejects 29 public zero-caller
  substrates. It is not globally green.

## Smallest coherent next slice

1. Repair Tape construction, lifetime, and layout identity.
2. Repair string payload and committed-payload analysis.
3. Repair signed-minimum literal lowering.
4. Repair CSS color grammar and ownership routes.
5. Seal a new exact subject and repeat both hostile reviews plus adjudication.

Regeneration now would materialize known semantic errors and supply no reliable
integration proof.
