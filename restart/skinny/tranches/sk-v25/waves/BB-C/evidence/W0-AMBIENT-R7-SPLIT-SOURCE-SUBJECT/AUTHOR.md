# BB-C.W0 ambient R7 structural/semantic split — source subject

Date: 2026-07-22  
Machine: Apple ARM64 (`uname -m = arm64`)  
Status: `HOSTILE_REVIEW_REQUIRED`; regeneration and fleet execution forbidden

This subject supersedes, rather than edits, the rejected full-`u8` design in
`W0-AMBIENT-R7-FULL-U8-REJECTION/ADJUDICATION.md` (SHA-256
`f58781ad6867b75aaef4c9febb19e85d2086bac078f53dc75611b2568b9741cb`).
It is source formation only. No generated grammar or runtime product belongs to
this subject.

## Claim

The overloaded branch-tag channel is split into two non-interchangeable
operations:

- `select_alt_branch(AltBranchId)` selects one structural branch. The exact
  structural domain is `u16` (`0..=65535`, at most 65,536 branches), and early
  plus final compiler gates reject the first unsupported Alt count.
- `push_leaf_with_u8(u8)` emits one repeatable semantic value. Pratt operators
  and typed `Nu8`/`u8` maps use this channel; structural dispatch does not.

`TapeRec` remains 16 bytes/aligned 4. Open records store an optional full-width
`AltBranchId`; leaves add an exact inline `U8` type. Tape checkpoints restore
the complete surviving scope and bounds stacks, not only their lengths.
Generated authorities now describe the split for BBNF, Sheets, JSON, the
simple-runtime cohort, and CSS. JSON's sole `0u8` grammar projection remains
`Null`; CSS retains existing typed frame slots and adds a generic
`CssTypedValue::U8` fallback. Wide integer payloads remain wide.

## Born-RED conditions

Reject this subject before regeneration if any of the following is true:

1. any emitter can narrow a structural ordinal without the compiler width gate;
2. any semantic `u8` travels through structural selection, or any `U32`/`U64`
   payload travels through the `u8` channel;
3. more than one Pratt operator cannot survive in source order;
4. rollback retains a mutated branch, bound, span stack, or semantic leaf;
5. a safe public branch API can mutate leaf metadata;
6. JSON `null` becomes numeric zero, CSS drops an otherwise unowned `u8`, or a
   simple runtime drops the byte;
7. generated authorities require a hand-edited generated product;
8. the existing 16-byte Tape record grows or loses its four-byte alignment.

## Exact subject ledger

Members: 36  
Bytes: 697,249  
Path-set SHA-256: `2bfe7c279e970b259c10d8791d21383afc1660882ca3668a5ff9c3d99ee12a1f`  
Ordered `shasum -a 256` ledger SHA-256:
`6940c6c098161575ce30188cfc071cc8fce041bbb2b6842bac9bc301d68a6ddb`

```text
c3010aea9c7f48bcd716f5ce3a601aa918c1268162287d0e7803b9f4a81dd26c  crates/ir/src/lib.rs
838ed1d2715edb59d59b14bb31f851e74da187de0314675e554c219c83441a86  crates/ir/src/registry/mod.rs
e63701b08f036c72fef256c7436281bfb283ca88bc481db4366578dd86d9b543  crates/ir/src/registry/struct.rs
e833719147c296b053608da66087a8dad47815dca7c3d084ad9b2328f0088d7a  crates/ir/src/passes/recognizers/grammar_facts.rs
e9a520d1ca95a5e97ad74390fcc1f18d0839c895cc7f037b68e001d66a723366  crates/ir/src/passes/types/registry.rs
52555a13d5cad12c978f151b034767e9d91c8d7b7bec9da050b138c09fd4301c  crates/ir/src/passes/types/subvariants.rs
2bf296f8123dbeab3f05821904f0875b81f425ed7cff20681d2b68e02a01adbc  crates/ir/src/types/rule.rs
4017c12c3ee92c7e4be79758171a129647e5d1ff8996cf36475877e09e1a82bc  crates/ir/tests/struct_registry.rs
3e0f92dcf9d212d296cd2d223ec8709db6bcbf8ab938d0e4eb78b946bbbc393b  crates/core/src/pipeline.rs
acf01cf1057be34178740e836e6c5ca437cf3e704afc1c31503281f4aa954920  crates/core/src/pipeline/validate.rs
3bd5673fd70f0707365532aa0b8824d7a21ac87d06feb56f82c80917bc95ae63  crates/core/src/pipeline/compile/pipeline.rs
a2ac40d163b42cece57284401c72f62665986b9cf8ea7f0f8656c2395ebebca8  crates/core/tests/pipeline/compile/pipeline.rs
edc900d08ef800f9b0e2095adf0176c6c8ff2a5975cfdb1ac4bd3a0b18ac41e6  crates/core/src/runtime/builder.rs
cffae985f23288220f6017e0313591811fe6cc785b6212010bba33a2eeba71ee  crates/core/src/runtime/builder_template.rs
13be66cb4f8e1f80f08b583be96cfe081b1cf97a627aef311be820af0a9418cb  crates/core/src/runtime/tape/record.rs
f36c360ea0c9ad09fe9d13e30691d8b358e9fdabd56adc2d07f86e9e9547de1d  crates/core/src/runtime/tape/mod.rs
5ce001b52120637d861c8272a735532bff27713e17c465ba9ca6a7ef2aea816a  crates/core/src/runtime/tape/cursor.rs
575c0b6d8b6256f92597b5b3096680ef3df0adc9672c6d412b149f12ba3198ba  crates/core/tests/tape_substrate.rs
5f6b02440d1d772867ad516dcb5ad4e8e36bee36c9d432646254dd733f47b242  crates/core/src/backend/rust/emitter/path_plan.rs
f5b931b97e58f12b56c5f20b56e3f93bdb8de96233e9f524a7191236503fcee2  crates/core/src/backend/rust/emitter/registry_emit.rs
c469a1fe62d509953850a4615a43cb23ca4f09bf1f18fffe26b247e7b3e19a6a  crates/core/src/backend/rust/emitter/shapes/alt_dispatch/mod.rs
2a2c64521d9c7aa4cf4cd1f09075ec1f11e9b1b863e4fb7b893eaf1ec1a0f60b  crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs
a64dc6911c5c467b16c5668fd14f5a3f8815e4fef5774a0827a6109f7bce35f2  crates/core/src/backend/rust/emitter/shapes/keyword/payload.rs
e8ce0f8acea3a7b26c8e769956c48fede234ae27a33f5aa9dfa875b6827ddcd4  crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs
c1931c7be0882f793f57f4dd21e766f99b4138153412b09b80f98b60b2740f93  crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs
f67d93935c7b0085b1c0d8735547429928e21d771dc8a296ded90fe703fc5a37  crates/core/src/backend/rust/emitter/shapes/wrap/struct_direct.rs
b1f3fd0ccaede772c4b4263fc6a695bed1b0653b0ddc62181e32a996053d6e40  crates/core/src/backend/rust/emitter/shapes/pratt/struct_direct.rs
d5b1dd6564217fc275ec751aa4ebf9d88f95b13a65e937da3065c90799a206e1  crates/core/src/backend/rust/emitter/shapes/hregex.rs
c1fd3f03bc89a5d23653c518144bfbc98e08b12dc2d79cbe7b4350bc9e0b7ea6  crates/core/src/lower/expression/mod.rs
26d8d97075a9b4b468ca3288d12a6a8be3f6c6740fd3790b5e8f9ad34528fdca  crates/core/src/lower/expression/pratt.rs
0e3ec24462d61321213375adbf6e3fe3284bc321bd11b1c35d50e61f0b270970  crates/core/tests/project_types_json.rs
95a186e5cb3d9b531a5a267242e037f0de4ba763ff70aad2bc83a484ca989334  crates/core/tests/path_variant_select.rs
470d7bd8da163c0f76ef6d64898f98ace08820b2c626c27e9703afda6c4e60b0  crates/core/tests/path_typed_compile.rs
55f6eb207d1b6ab6a5ed10f0a84a412b616a43b9c7e45a307aa8f129a7ab6f99  crates/core/tests/path_type_errors.rs
4190d26b6810e37fc748872b1766083d5a8ae32cd5bee3c9dc7814f07b7b2fbb  xtask/src/regen_simple_runtime.rs
1b1b73624c081b3a52253bd1e343003da530c9576434debf5b6c528f81085ef7  xtask/src/regen_css.rs
```

## Verification completed before review

- `cargo test -p bbnf-ir --test struct_registry`: 14/14 PASS on ARM64.
- `cargo test -p bbnf-ir --tests`: every test before the known global
  substrate audit passed; the run remains RED only at the pre-existing 29-item
  zero-caller substrate gate. This is not a green full-suite claim.
- `cargo check -p xtask`: PASS on ARM64.
- `cargo test -p xtask --lib`: PASS on ARM64 (zero tests).
- targeted `rustfmt` and repository `git diff --check`: PASS.

The core crate is intentionally not claimed green while generated products
still implement the deposed trait. A hostile source verdict is required before
isolated regeneration, generated fixed-point comparison, semantic fleet tests,
or profiling.
