# SK-V7 W8 R4 - IR/Class D and Lock 14 Verification Gate

## Inputs Read

- `restart/skinny/tranches/sk-v7/SPEC.md` §10: W8 owns Lock 14 Phase C + D across codegen and IR; byte-identical generated output is the gating invariant; exit target is Lock 14 HIGH count `-38` / at least `-83%`.
- `restart/skinny/tranches/sk-v7/research/skv7-A5-lock-audit.md` §2.9: original IR leak inventory was `StructuralAlphabet::json()`, JSON-named `TapeKind`, JSON-named `DirectBuildDecode`, and `regex_is_nullable(pattern: &str)` string-equality against JSON whitespace.
- `restart/skinny/tranches/sk-v7/research/skv7-B3-lock14-sequence.md`: Class D/E sequence requires grammar-neutral IR names, deleting `StructuralAlphabet::json()`, replacing `regex_is_nullable` with typed/nullability facts, and verifying byte-identical generated output.
- Current `skinny/crates/ir/src/lib.rs`.
- Current xtask availability in both workspace roots.

## Current IR Leak Inventory After W1/W7

Current `skinny/crates/ir/src/lib.rs` has partially closed the A5 §2.9 Class D list:

| A5 item | Current state | Evidence | W8 status |
|---|---|---|---|
| `TapeKind::{Object, Array, Pair, String, Number, Bool, Null, Member, Element}` | Already renamed to grammar-neutral variants: `Container`, `Sequence`, `KeyValuePair`, `StringValue`, `NumberValue`, `BoolValue`, `NullValue`, `Member`, `Element`. | `skinny/crates/ir/src/lib.rs:433-442`; passes consumers use the new variants. | Closed before W8 R4; do not rework. |
| `DirectBuildDecode::{JsonString, JsonNumber}` | Already renamed to `EscapedString` and `NumberScalar`. | `skinny/crates/ir/src/lib.rs:510-514`. | Closed before W8 R4; do not rework. |
| `StructuralAlphabet::json()` | Still present as an IR convenience constructor returning `b"{}[],:\""`. | `skinny/crates/ir/src/lib.rs:411-417`. | Open W8 IR/Class E residue. |
| `regex_is_nullable(pattern: &str)` | Still present and still special-cases only `r"[ \t\n\r]*"`. | `skinny/crates/ir/src/lib.rs:303`, `skinny/crates/ir/src/lib.rs:321-323`. | Open W8 IR/Class D residue. |

Exact answer: yes, both `StructuralAlphabet::json()` and `regex_is_nullable(pattern: &str)` remain in the current IR crate.

The current direct grep surface also shows no remaining IR definitions or production consumers of `DirectBuildDecode::JsonString`, `DirectBuildDecode::JsonNumber`, `TapeKind::Object`, `TapeKind::Array`, or `TapeKind::Pair`. Some `JsonString` / `JsonNumber` strings remain in per-grammar generated/runtime/codegen surfaces, but those are Phase C/codegen or JSON runtime outputs, not the IR Class D enum names.

## Verifier Availability

The B3 document references `cargo xtask lint-no-hardcoded-grammars`, but that command is not currently available in either xtask binary:

- Root workspace `cargo xtask --help` exposes only `regen`.
- Skinny workspace `cargo xtask --help` exposes `regen-json`, `check-json`, `regen-real-typed`, `check-real-typed`, `check-conformance`, `lint-loc`, `bench-json`, `gate-json`, and `primitive-checkasm`.

SPEC §10 says "`xtask gen --check` must succeed"; the current root spelling appears to be `cargo xtask regen --check`. For skinny-generated JSON, the currently available byte-identical checks are `cargo xtask check-json` and `cargo xtask check-real-typed` from `/Users/mkbabb/Programming/bbnf-lang/skinny`.

## Proposed W8 Intervention

Treat IR work as a small post-W1/W7 cleanup, sequenced after or alongside the Phase C codegen rebrand:

1. Delete `impl StructuralAlphabet { pub fn json() -> Self { ... } }` from `skinny/crates/ir/src/lib.rs`.
2. Ensure no production caller remains. Current grep found no `StructuralAlphabet::json()` call outside the definition, so this should be an IR-local deletion.
3. Replace `regex_is_nullable(pattern: &str)` with grammar-neutral nullable-regex handling. The safest W8-local form is not to introduce a new JSON-shaped helper; either:
   - consume typed regex/nullability facts if W8 has the typed regex IR available at that point, or
   - move the nullable decision to a generic regex parser/classifier API that can answer "matches empty" without comparing against a literal JSON whitespace pattern.
4. Keep `TapeKind` and `DirectBuildDecode` unchanged, because the intended neutral names have already landed.
5. Do not rename JSON runtime types or generated JSON view/value names as part of the IR subtask; those are per-grammar output/API surfaces and are governed by Phase C byte-identical output constraints.

If typed regex nullability is not ready during W8, the intervention should stop at deleting `StructuralAlphabet::json()` and explicitly route `regex_is_nullable` to the typed-regex substrate owner. A cosmetic rename of `regex_is_nullable` that preserves `pattern == r"[ \t\n\r]*"` would not close the Lock 14 violation.

## Grep, Lint, and Test Gates

Run these grep gates after the W8 IR patch:

```sh
rg -n 'StructuralAlphabet::json\(\)|pub fn json\(\)' skinny/crates/ir/src/lib.rs skinny/crates
rg -n 'fn regex_is_nullable|pattern == r"\[ \\t\\n\\r\]\*"' skinny/crates/ir/src/lib.rs skinny/crates
rg -n 'TapeKind::(Object|Array|Pair|String|Number|Bool|Null)\b' skinny/crates/ir/src/lib.rs skinny/crates
rg -n 'DirectBuildDecode::(JsonString|JsonNumber)|\bJsonString\b|\bJsonNumber\b' skinny/crates/ir/src/lib.rs
```

Expected results:

- No hits for `StructuralAlphabet::json()` or `pub fn json()` in IR.
- No hits for the JSON whitespace string-equality nullable helper.
- No hits for old JSON-named `TapeKind` variants.
- No hits for JSON-named `DirectBuildDecode` variants in IR.

Run these available build/generated gates:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo test --workspace
cargo xtask check-json
cargo xtask check-real-typed
cargo xtask check-conformance
cargo xtask lint-loc
```

For the root workspace, use the currently available spelling of SPEC's generated-output check:

```sh
cd /Users/mkbabb/Programming/bbnf-lang
cargo xtask regen --check
```

If W8 adds the missing hardcoded-grammar verifier, add it to the required gate as:

```sh
cargo xtask lint-no-hardcoded-grammars
```

Until that command exists, approximate the Lock 14 verifier with the targeted greps above plus the broader B3 spot checks for parse-that-regex, passes, codegen, template relocation, and JSON entrypoint names.

## Byte-Identical Generated Output Risk

IR-only risk is low but not zero:

- Deleting `StructuralAlphabet::json()` should be byte-output-neutral if no caller exists. Current tree shows no production call sites, so the main risk is hidden external/public API use rather than generated Rust drift.
- `TapeKind` and `DirectBuildDecode` should not be touched in W8 R4. Renaming them again would churn serde-visible IR spellings and could perturb tests or serialized fixtures without helping Lock 14.
- Replacing `regex_is_nullable` can affect validation. Today only `r"[ \t\n\r]*"` is considered nullable. A real generic regex nullability implementation may newly reject nullable repeat bodies for other nullable regexes, which is semantically more correct but can change accepted grammars and therefore generation outcomes for non-JSON grammars. Gate with existing JSON byte checks and add focused IR nullability tests for nullable and non-nullable regex patterns.
- Phase C codegen rebrand is the larger byte-identical risk. Any change to shape ordering, emitted module text, comments, template include order, or typed DirectBuild lowering can alter `runtime/src/grammars/json/generated.rs` or `bbnf-bench/src/generated_real_typed.rs`. Keep IR cleanup in a separate sub-commit from codegen renderer changes if Phase C exceeds the 200 LOC sub-split trigger.

Recommended W8 acceptance statement for the IR slice: IR has no JSON-named Class D enum residue; `StructuralAlphabet::json()` is gone; nullable-regex validation is no longer JSON-pattern string equality; `cargo test --workspace`, `cargo xtask check-json`, `cargo xtask check-real-typed`, and root `cargo xtask regen --check` all pass with no generated-output drift.
