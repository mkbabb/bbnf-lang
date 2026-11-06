# BB-C.W0 ambient R7 split-source R2 subject

Date: 2026-07-22 EDT  
Host: Apple ARM64 (`uname -m` = `arm64`)  
Status: source-only hostile-review subject; born RED; no regeneration authority

## Custody

This receipt supersedes the rejected R7 split-source subject. Its controlling
predecessor is
`W0-AMBIENT-R7-SPLIT-SOURCE-REJECTION/ADJUDICATION.md`, SHA-256
`46c299064f28947663955da158ae69e4c90b28e114e85033690f5f181bdcec43`.

The receipt governs only the 97 files in the ledger below. Generated grammar
and runtime files, locks, packages, unrelated dirty files, and concurrent work
are outside its authority. No generated file was hand-edited for this repair,
and this receipt does not authorize an isolated regeneration.

## Claimed closure

1. Structural alternation identity and semantic payload are separate:
   `AltBranchId` selects Open alternatives, while typed leaves carry mapped
   values. Both ordinary rule bodies and recovery expressions are checked
   against the exact 65,536-branch representability bound at canonical compile
   entry.
2. Tape records remain 16 bytes. Checkpoint rollback restores records, arena,
   the complete open-scope stack, span state, root, and handle. Raw record
   construction is sealed; cursor leaf readers verify both `Leaf` kind and the
   exact leaf discriminator before decoding.
3. Rust emission preserves one semantic payload through direct and delegated
   alternation, wrapping, flat, keyword, Pratt, inline, and HRegex shapes.
   HRegex typed integers share one checked decoder, including `U64`; no
   structural branch tag substitutes for a semantic value.
4. Pratt-generated operator tags lower through semantic `Tag` leaves. The
   reducer recognizes only a genuine tagged chain, so an anonymous
   single-operand group is not misclassified as Pratt input.
5. Integer literals remain exact across grammar spelling, IR, lowering, Rust,
   TypeScript, and Wasm. `MapExpr::UIntLit(u64)` is transported and folded with
   checked arithmetic. TypeScript uses `bigint` for I64/U64 and validates
   declared ranges. The current Wasm i32 ABI accepts only exactly representable
   widths and rejects I64/U64 and the `-1` sentinel collision rather than
   narrowing.
6. Generator authorities preserve exact runtime values or fail closed. BBNF
   and Sheets have exact unsigned storage; CSS rejects unsupported wide
   integer transport. JSON's U8 projection asserts its grammar-owned value is
   exactly zero before constructing Null.
7. CSS color frames distinguish functional and predefined roles, reject
   missing or duplicated required fields, use canonical color-space
   discriminants, check HexColor conversion, and carry the complete configured
   CSS Length L4 unit set without invented defaults.
8. The clean break is complete within non-generated core source and tests:
   there is no `push_branch_tag` call, alias, or compatibility shim.

## Rooted verification before review

- `cargo test -p bbnf-ir --test map_expr_uint`: 3/3 pass.
- `cargo test -p bbnf-ir --test struct_registry`: 14/14 pass.
- `cargo test -p xtask --test runtime_projection_semantics`: 3/3 pass.
- `cargo check -p xtask`: pass; pre-existing `LIVE_TOOL_NAMES` warning only.
- Targeted `rustfmt --check`: pass.
- `git diff --check`: pass.
- Non-generated `push_branch_tag` scan: zero matches.
- Core compile/test remains intentionally RED against the stale generated
  fleet (approximately 880 errors). That is not accepted evidence and is the
  reason regeneration remains gated behind this source adjudication.

## Born-RED acceptance conditions

This subject remains RED unless two hostile critics independently inspect the
exact bytes and the whole wave fit, and a third adjudicator proves every
material claim CORRECT, INCORRECT, or UNPROVEN. Any changed ledger member
invalidates the receipt. Any missing exactness, rollback, generated-authority,
CSS-role, integer-domain, or clean-break proof rejects isolated regeneration.

## Exact source authority

- Members: `97`
- Bytes: `1093436`
- Ordered newline-terminated path-list SHA-256:
  `5879c5761fa83bd898eab414471c8620e0907aa0f76d864bf8163ca05f692f96`
- Ordered `shasum -a 256` ledger SHA-256:
  `6907807ebbd4ede5f58df3603676efc990506629ade7149513b57f7b36a9d195`

```text
c3010aea9c7f48bcd716f5ce3a601aa918c1268162287d0e7803b9f4a81dd26c  crates/ir/src/lib.rs
838ed1d2715edb59d59b14bb31f851e74da187de0314675e554c219c83441a86  crates/ir/src/registry/mod.rs
e63701b08f036c72fef256c7436281bfb283ca88bc481db4366578dd86d9b543  crates/ir/src/registry/struct.rs
4b47824eab4a74f3cbe83acd5b71435450d977957549a93807d280c6ef854ab9  crates/ir/src/types/map_expr.rs
2bf296f8123dbeab3f05821904f0875b81f425ed7cff20681d2b68e02a01adbc  crates/ir/src/types/rule.rs
85e270ee415d47423317c74e12152dfba2affb1ef036a061ff184def02bd5e83  crates/ir/src/types/type_desc.rs
6c0d8f7a5a4740f445aea0e64826bd16f9e7349a51180911b5a892f88c7e7430  crates/ir/src/passes/payload/scalar_routing.rs
c0c5edeac7959269557fcab3af10733a44ca915decf419fd4aba00048acce9ce  crates/ir/src/passes/recognizers/grammar_facts.rs
e9a520d1ca95a5e97ad74390fcc1f18d0839c895cc7f037b68e001d66a723366  crates/ir/src/passes/types/registry.rs
52555a13d5cad12c978f151b034767e9d91c8d7b7bec9da050b138c09fd4301c  crates/ir/src/passes/types/subvariants.rs
a29f5e481c649e79519738bbe5d36300d22029632c0e5fb0e37f08ab35062169  crates/ir/tests/map_expr_uint.rs
4017c12c3ee92c7e4be79758171a129647e5d1ff8996cf36475877e09e1a82bc  crates/ir/tests/struct_registry.rs
3e0f92dcf9d212d296cd2d223ec8709db6bcbf8ab938d0e4eb78b946bbbc393b  crates/core/src/pipeline.rs
d9488da9915672756623c1f6995e14d6b7752a94b33e44f6ca1237cbfd2b5d4a  crates/core/src/pipeline/validate.rs
3bd5673fd70f0707365532aa0b8824d7a21ac87d06feb56f82c80917bc95ae63  crates/core/src/pipeline/compile/pipeline.rs
3745b1f73f645b12fa94942cc5e9d8ece6c6aff30c2da0b30aed4ecfd405c4fb  crates/core/tests/pipeline/compile/pipeline.rs
903437ee813187ff3d1549ed71b9a7aac88f685bdbf5204073adfc10b682f5c4  crates/core/src/runtime/builder.rs
cffae985f23288220f6017e0313591811fe6cc785b6212010bba33a2eeba71ee  crates/core/src/runtime/builder_template.rs
e0198cdf27f2548a2c39895062f311b1ab22b6abec47a6b5d8d417fd5497f0db  crates/core/src/runtime/tape/arena.rs
7701d87ebd8b7610df7a3533b966bceae19a9351873d06d94321dc469cd50aae  crates/core/src/runtime/tape/record.rs
a554e7f0a06729a0a36759aee285f377a5c1bea625ea29cec75bcc05b627c344  crates/core/src/runtime/tape/mod.rs
b04b1c6ef1732b2acc1b0ec0f117179e24bac5eaac8dd930f4053b67f05f6034  crates/core/src/runtime/tape/cursor.rs
b190ae39a428a379860437834deb0a84db87d05b0479b1b21c63d9efa3fce412  crates/core/tests/tape_substrate.rs
5f6b02440d1d772867ad516dcb5ad4e8e36bee36c9d432646254dd733f47b242  crates/core/src/backend/rust/emitter/path_plan.rs
f5b931b97e58f12b56c5f20b56e3f93bdb8de96233e9f524a7191236503fcee2  crates/core/src/backend/rust/emitter/registry_emit.rs
c469a1fe62d509953850a4615a43cb23ca4f09bf1f18fffe26b247e7b3e19a6a  crates/core/src/backend/rust/emitter/shapes/alt_dispatch/mod.rs
435f26fe85735f3f60085e379002d0c9d0495b1c21c958c0e53b32c31573e99f  crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs
497b0df35643d0bc1d76ed43ecfa7b56ee4e5eee7802ddbb2084b3eea26ae126  crates/core/src/backend/rust/emitter/shapes/inline/structural_branch.rs
693b5eb151e432f54ca31b18841236938ee4c8d19967a3b2790391bd28f068c1  crates/core/src/backend/rust/emitter/shapes/keyword/payload.rs
a4ac6b05a2ee99f83f5f0b0e6eaaad8ef9fa3a7760de29fa264f335478f0f5ea  crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs
c0a60009ae7ffda05ee86449f1b251a1b57ce0e4d27bd065b841bb2317b1f8b9  crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs
09582c5b5d324cc727992e3b27f05d55fb679890b5587a255a7112dbeefd0d18  crates/core/src/backend/rust/emitter/shapes/wrap/struct_direct.rs
b1f3fd0ccaede772c4b4263fc6a695bed1b0653b0ddc62181e32a996053d6e40  crates/core/src/backend/rust/emitter/shapes/pratt/struct_direct.rs
a6c67a724e73a0ebb61dbde30e2da7851464d9e1dee3a4727f01758a05e66d27  crates/core/src/backend/rust/emitter/shapes/hregex.rs
0a683091905a5541e9d09fbda2b95ff92bd5b62ff5f9c856bb3082aca809fa2d  crates/core/src/backend/ts/projection.rs
ada925b6f84b6552804d6cde1069f9f832974102054de0a5f3a4f226024de3a0  crates/core/src/backend/wasm/emitter/value.rs
c1fd3f03bc89a5d23653c518144bfbc98e08b12dc2d79cbe7b4350bc9e0b7ea6  crates/core/src/lower/expression/mod.rs
a18163e5dedb2b560a396ebac6e7087a2cf854517b97ebb1e570ad0e56bbdcb7  crates/core/src/lower/expression/pratt.rs
ee101b8c894bbe93d6fa0e4ec17ff54f253fe1bafaa12a7247baceda56568b90  crates/core/src/lower/expression/wrap.rs
5373c72a6c67342dfbd49a97fbe3c83b21d9e6e38435254b3fb85d6d030e319b  crates/core/src/lower/value_expr/atom.rs
5c369b67ce28ed47aecb36bfda93befd107ec93c5b5cdec10ccd20a709f8cb4b  crates/core/src/lower/value_expr/literals.rs
6c0978901e8611d0e654b21195d2a95fdc1cbb2ea9359e0adcc1e692cd9796cf  crates/core/src/lower/value_expr/mod.rs
cd95e709ff0885bacf3ddfe3d5f118cfcdc2df6ddce4977d4ef12dd6cb436f18  crates/core/src/lower/value_expr/unwrap.rs
434dc2dc6f468cd586acb537753ee7a766ba551b9d74db4596b6b58be02f5c6e  crates/core/tests/lower.rs
331e58a6e4b74ff4fc8d0636ce99e508336c7d6d7362e80e06f9ddb5cb0a23f2  crates/core/tests/lower/expression/wrap.rs
b5c8d842258b158b61695b57395f792a61e5dc5d0d56d684851a0a5e37d0304d  crates/core/tests/emitter_typed_payloads.rs
2e914fa32918720601af51f934dec51d94c57364ccfbae9eb9ab5d800fec4eb3  crates/core/tests/uint_literal_backends.rs
bfee80dded530801421ed578965df10bfffd19a449c0fe011e482f4dc653b67b  crates/core/tests/css_l4_color_roles.rs
a5cd5dee8ded6a0293fcf2f0bb5b02d9b373cc34489c36f25c19f95a531e2038  crates/core/tests/bbnf_struct_builder_substrate.rs
a636963d6a8564f0cbad94d7b7800be92cec2c1e5a00bc21e73836912d11dd67  crates/core/tests/css_l4_substrate.rs
067cacc3de3c842f1aab7328f311d73c0f7282234efc7cca57fd80300a26489a  crates/core/tests/google_sheets_slab.rs
70898b5e9ea0f6599d18b2d5c2c16ce523bddef11beb21f11902458e28500936  crates/core/tests/json_parity_struct.rs
8b6fb4304f5c297b3b082c793eb07a567f5bcb8c6f8e91d4cbd8ed9f943e52a6  crates/core/tests/sheets_expr_parity.rs
9e7581c69d83fb877a9c662943b494f8f20bf13ba07bab72876936a6cd42c966  crates/core/tests/sheets_parity.rs
0aed67c648bd3a6d6cfe23280f28633d1a13f8f13fa7b416aaf4c90dd0154fff  crates/core/tests/projection_totality.rs
0e3ec24462d61321213375adbf6e3fe3284bc321bd11b1c35d50e61f0b270970  crates/core/tests/project_types_json.rs
95a186e5cb3d9b531a5a267242e037f0de4ba763ff70aad2bc83a484ca989334  crates/core/tests/path_variant_select.rs
470d7bd8da163c0f76ef6d64898f98ace08820b2c626c27e9703afda6c4e60b0  crates/core/tests/path_typed_compile.rs
55f6eb207d1b6ab6a5ed10f0a84a412b616a43b9c7e45a307aa8f129a7ab6f99  crates/core/tests/path_type_errors.rs
eef788e29119b7eb88444e1dd0675bc744a1802468914e9cb402d78c9c267a57  grammar/bbnf/bbnf.bbnf
471edce8f51ee5a12d8950e4230d1d6e938679059ef795d8f4cfb0dea41353a4  grammar/bbnf/expressions.bbnf
db3503921bec484807d231095be8306eb4d9e231d5771eff2da8ad3caca4f008  grammar/bbnf/types.bbnf
8d4de97d9a54a1fef6f7ccccdeabd185d66c8614f167fc3ae28225b7994e4a1b  grammar/bnf/bnf.bbnf
e998dd95ad31f97dc873dd65038b8b4b36156f83fd8faf9cbe61b5be6865fa47  grammar/css/l4/color.bbnf
f5550fa89d67644d72ead3b6e30b33e28ff2a41219f91a98ec15d4424cde50bd  grammar/css/l4/easing.bbnf
8321e5436797fe1bf5a614cc46d529ff56164e21d07be49ef3cf7f4dfa622537  grammar/css/l4/filters.bbnf
5b804cb75611645889babab14783cd052321fbf46694eac5b604ba8bd5e854d3  grammar/css/l4/func-body.bbnf
f0f2495d39dc0ee85cf203b622bb4507d7f3cf860817f1544dd1659ccbe4143e  grammar/css/l4/gradients.bbnf
889dec7c994c3379c069524f6ed36ddc24c96de9663b4f6dab8ce762f43dffd8  grammar/css/l4/keyframes.bbnf
bac8e98937bed7e3c4ea6fc2bc5dea9bd8ca046299a23879a8ff81685540fb0f  grammar/css/l4/keywords.bbnf
e0cf30d97ba7861dc7fadac3264e542c07317b32fd6f6df07ee4f71413aa005e  grammar/css/l4/media.bbnf
94451adff5a62ca401f935d696cbd9ee427feefc87660a4538cd20b2d768a8a0  grammar/css/l4/properties.bbnf
455678a59a1a77c868326d988dd185a83a3c84c3a1a006b10fcbcea85de6c966  grammar/css/l4/selectors.bbnf
2c2e62e71cdbd76be78906ad5741763c3ba12c3d7418bc6b39620330027fda07  grammar/css/l4/stylesheet.bbnf
23e535851ce43d7c5e6b243e3b766a4616c5181bf20cc6017405e4406f954054  grammar/css/l4/tokens.bbnf
650cc388d652f43a166a893ea8ce0538d04d2b733d63f2feefcce02127617cf5  grammar/css/l4/transforms.bbnf
cb57f79050a6304d79e3a7d8c7b690c902f243880aab4c896a63899290dd727b  grammar/css/l4/value-unit.bbnf
ecbc3ad7997485abe8964f2f19df4129526ebc367270939e0a8242d8b7215396  grammar/css/l4/values.bbnf
38682d3ff80bc66910dfe4dcc2f962fa4be5cc85cb360f18df00ebe92d98930b  grammar/css/pretty.bbnf
42ddc265529c5662cd667e44b051ba129bbe36bc3b8abc743c26b8826d58de52  grammar/ebnf/ebnf.bbnf
78372003a65bade3baab67ea60e070f9bbbdfe112ed143cb5fc6b0d9ae8704a0  grammar/google-sheets/google-sheets.bbnf
ab8d21218b2a5f49d9231af6f71503a9949782fdc0a50d81a845fc69054f2604  grammar/json/json.bbnf
e9622489b2ffc31850559bd51f33222127493e14cab0f3cacb85e0537e8837f3  grammar/misc/csv.bbnf
04c7013a9ae394ae007242e51fc27533563fe795381f331893fa188e3bd67e31  grammar/misc/math.bbnf
4a455c5f615a1b7b86bdc76cdf7b57ec013f546cb6af2590563675851ad61766  xtask/Cargo.toml
7f4756a9e865b3fb0be6e752084c40e1ee5f2f3a18c017e4baf84b0a244c889c  xtask/src/regen_simple_runtime.rs
970058ff9c57827246a839c381b2f9f1dff83b7102f61b15ec13d0eb137363c6  xtask/src/regen_css.rs
2386b95b6af21d8e474ca791ab8e86b545931271fc99e9d4adeeff30c2bb703e  xtask/runtime-projections/bbnf.toml
7fdba2d4f7c7846184c40c920b2d9d8fcd429721b2b83d93cef75817764cf922  xtask/runtime-projections/bnf.toml
992a9ee276a62ce4baee94ff6be198e74905c4f9948e069fa95828e7a65d3d86  xtask/runtime-projections/css_l4.toml
f06b1097d1c657571ac674ca7aee6c3925144214bb4dc34c509efa2486e95eb1  xtask/runtime-projections/css_pretty.toml
23606a592405b4b17b5d1c280bd59ccb4f7ca9a9df7dc6dada6aa3ac7ab93854  xtask/runtime-projections/csv.toml
a4761322adafb6c940126f903f2b01d4c9bfb08a9c6c5a4832daa4d3ac99b730  xtask/runtime-projections/ebnf.toml
4facfd43428f74d4809e00c84a8bf71b3755c5bf5724f97b5fdee109f8aea3d6  xtask/runtime-projections/google_sheets.toml
035931eec75498a47a690d06028e55a8e3a7f496c39f27c8cb576b15ae26fa62  xtask/runtime-projections/json.toml
827e0cf43458528bc3b5a6a02aeb2fbda5a694348594a62e261ef77e57efd0f2  xtask/runtime-projections/math.toml
21240b78a2674b226e8a3e39a542d3407b798e99795cceb153cba4c828093acf  xtask/tests/runtime_projection_semantics.rs
```
