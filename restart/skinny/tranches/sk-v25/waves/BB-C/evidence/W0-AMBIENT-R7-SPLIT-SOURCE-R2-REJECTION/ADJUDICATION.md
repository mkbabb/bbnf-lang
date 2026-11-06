# BB-C.W0 ambient R7 split-source R2 — adjudication

Date: 2026-07-22 EDT  
Role: fresh third adjudicator  
Model: UNKNOWN  
Reasoning effort: UNKNOWN

The runtime exposed no independently verifiable model or reasoning-effort
metadata. Neither value is inferred.

## Terminal verdict

**REJECT. R2 remains RED.** Isolated regeneration is not authorized. The
subject earns zero BB-C.W0, wave, lock, tranche, or movement credit.

No file was edited, staged, regenerated, built, or tested during the read-only
adjudication.

## Custody

- Subject SHA-256:
  `070f8ca84d80f95e8502d53b44c6df96973d6fd9e116356dd670f9a2ad4d0cdb`.
- Predecessor SHA-256:
  `46c299064f28947663955da158ae69e4c90b28e114e85033690f5f181bdcec43`.
- Critic A SHA-256:
  `de9fab399a0af5fabd792e9485b60224d60e0b9228d890bf605850043bae9d65`.
- Critic B SHA-256:
  `99b56cf9da0b6c441087927e0b5f3fcdd7a2f7597d6e552b971345dcc5c61d40`.
- The 97-member ledger independently reproduces `1093436` bytes, path
  digest
  `5879c5761fa83bd898eab414471c8620e0907aa0f76d864bf8163ca05f692f96`,
  ledger digest
  `6907807ebbd4ede5f58df3603676efc990506629ade7149513b57f7b36a9d195`,
  zero member mismatches, on `arm64`.

Critic A's custody objection is correct: controlling dirty source
`crates/core/src/backend/ts/emitter/value.rs`, SHA-256
`fc09ef13de386dbe9499aef5daaed49b0b64ae54dd96deab4a3b6c0a2d11f382`,
is absent. Its line 87 provides the new `return_type` argument required by
`backend/ts/projection.rs:92-117`.

## Claim adjudication

### 1. Structural/semantic identity and branch bounds

**CORRECT in source formation; generated integration UNPROVEN.**

`AltBranchId(u16)` and the 65,536 count are exact at
`crates/ir/src/registry/mod.rs:38-52`. Body and recovery traverse the same
validator at `pipeline/validate.rs:7-40`, called before and after passes at
`pipeline/compile/pipeline.rs:80,349`. Boundary tests exist at
`tests/pipeline/compile/pipeline.rs:78-150`.

### 2. Tape

**CORRECT as narrowly enumerated; complete release closure INCORRECT.**

Record size/alignment and private fields/constructors are correct at
`runtime/tape/record.rs:99-171`; checkpoint restoration is complete at
`runtime/tape/mod.rs:70-81,136-156`; U64 sentinel handling is correct at
`:240-252`; typed readers check leaf type. But:

- `TapeRef` and `TapeCursor::new` are publicly forgeable and lead to unchecked
  record/arena indexing (`runtime/tape/cursor.rs:18-50,88-143`,
  `runtime/tape/arena.rs:86-107`);
- safe `bind_input` transmutes the caller's lifetime (`tape/mod.rs:158-161`);
- `rule_id & 0x1F` aliases rules 32 apart (`tape/mod.rs:184-195`).

### 3. Exactly one semantic payload

**INCORRECT.**

Flat declares `StringLit` payload-emitting at
`flat/struct_direct.rs:109-136`, emits nothing at `:204-215`, and suppresses
span synthesis at `:333`. Inline suppresses fallback using existential
`node_contains_map` at `inline/structural_branch.rs:235-239,342-365`, including
zero-repeat, negation, subtraction, and partial-alternative paths. Untyped
positive `IntLit` is also retyped as U64 at
`alt_dispatch/branches.rs:62-64`. HRegex's checked integer decoder, including
U64, is correct narrowly at `hregex.rs:196-325,395-407`.

### 4. Semantic Pratt

**CORRECT in the source producer/reducer; generated authority
INCORRECT/UNPROVEN.**

Pratt emits semantic U8 tags and lowering requires a genuine alternating chain
at `pratt/struct_direct.rs:140-150` and
`lower/expression/pratt.rs:24-105`. BBNF serialization corrupts the chain:
`regen_simple_runtime.rs:3662-3670` prints tags as decimal, while
`:3722-3728` joins every BinaryFactor child with `" - "`. Sheets also carries
conflicting duplicated operator authority: the grammar assigns `=`/`<`/`>` to
3/4/5, while serializer/Pratt metadata assigns 3/4/5 to `</>/=`. A blanket
claim that every Sheets round trip fails is unproven, but the authority conflict
is proved.

### 5. Integer exactness

**INCORRECT overall.**

The grammar separates unsigned magnitude from unary minus
(`grammar/bbnf/expressions.bbnf:6,25`). The magnitude is range-checked as i64
before negation (`lower/value_expr/literals.rs:6-45`), so `i64::MIN` cannot
lower. Ordinary negatives become UnaryOp (`atom.rs:22-44`); direct suffix
inference loses `-1i8`'s width (`lower/expression/wrap.rs:290-352,375-381`);
Wasm passes non-direct expressions through (`backend/wasm/emitter/value.rs:82-87`).
UIntLit transport/folding, TypeScript bigint projection, and direct Wasm
rejection remain correct narrow facts.

### 6. Generator exactness

**INCORRECT.**

BBNF/Sheets exact carriers and JSON's zero assertion are correct. CSS deposits
generic U64 as `CssTypedValue::Unsigned`; only HexColor checks U32. The shared
Simple builder silently converts F64/I64/U64/Bool to Unit at
`runtime/builder_template.rs:265-287`, and `regen_simple_runtime.rs:237` has no
payload-domain rejection.

### 7. CSS roles and exactness

**INCORRECT overall.**

Functional/predefined frames, required-field checks, rollback restoration,
listed discriminants, and 49 configured length units are correct narrow facts.
The total closure is false:

- hex uses illegal `{3,8}` and the host fabricates zero on unsupported lengths;
- `currentcolor` is fabricated as black;
- neither `colorMix` nor `namedColor` has a projection route;
- `deposit_value` cannot populate ColorMix operands; and
- percentage dimensions do not enter color `pending_components`.

### 8. Clean break

**CORRECT only for the exact source ledger; product clean break UNPROVEN.**

The 97 members contain no `push_branch_tag`. The stale generated/runtime fleet
still has 382 occurrences, including generated CSS grammar and builders.

## Normative CSS ruling

R3 must delete ColorMix from the CSS L4 module, not repair Level-5 machinery
inside it. CSS Color 4 defines `<color-base>` without `color-mix`, treats
`currentColor` separately, and requires exactly 3/4/6/8 hex digits.
`color-mix()` belongs to CSS Color 5.

Primary authorities:

- <https://drafts.csswg.org/css-color-4/#color-syntax>
- <https://drafts.csswg.org/css-color-4/#hex-notation>
- <https://drafts.csswg.org/css-color-5/#color-mix>

## Verification disposition

- `git diff --check`: CORRECT.
- exact-ledger no-`push_branch_tag` census: CORRECT.
- Cargo, rustfmt, and reported stale-fleet error counts: UNPROVEN because no
  immutable logs are bound to the subject and this adjudication did not run
  builds.
- `scalar_range_includes_sentinel` has no production caller and is
  non-conservative for function calls/composite arithmetic: Critic A is
  CORRECT.

## Wave fit

BB-C.W0 requires G3 deletion, typed optimizer failures, ambient egraph/CSP
authority removal, a complete call-site inventory, and inverting mutations.
R2 proves none of those completion gates. It is a broad generated-fleet incident
repair prerequisite, not W0 progress. Its 97-file/1.09-MB scope is not proved
minimal or wave-optimal.

## Binding R3 order

1. Repair sign-aware literal lowering as one grammar-to-IR operation,
   preserving suffix and signed minima; exercise grammar→Rust/TypeScript/Wasm.
2. Replace Flat/Inline payload guesses with one path-sensitive exactly-once
   payload authority, including real StringLit emission.
3. Seal Tape cursor/reference construction, remove safe lifetime transmute,
   add checked arena access, and store exact rule identity without masking.
4. Make Simple generation reject unsupported scalar alphabets before output;
   remove Unit coercions. Unify Pratt tags and serialization under one operator
   metadata authority.
5. Restrict CSS L4 to L4: delete ColorMix machinery, add distinct CurrentColor
   ownership, exact hex alternatives with fallible conversion, canonical Named
   ownership, and proven percentage deposition.
6. Seal a new source receipt containing every controlling integration file,
   especially the TypeScript emitter, plus adversarial tests.
7. Repeat two hostile reviews and third adjudication.
8. Only after acceptance: perform one ordered grammar→sidecar→runtime
   regeneration, require a byte-identical second pass, then full ARM64 compile,
   parity/no-throw tests, and a zero-occurrence product census.
