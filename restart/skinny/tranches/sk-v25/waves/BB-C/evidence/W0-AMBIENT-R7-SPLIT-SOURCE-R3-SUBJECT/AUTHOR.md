# BB-C.W0 ambient R7 split-source R3 subject

Date: 2026-07-22 EDT  
Host: Apple ARM64 (`uname -m` = `arm64`)  
Status: source-only hostile-review subject; born RED; no regeneration authority

## Model-truth gate

The repair was assembled by several agents. One Tape author declared
`gpt-5.6-sol` at `ultra`; the literal/payload author reported
`MODEL UNKNOWN; EFFORT UNKNOWN`; the CSS/Simple author reported a GPT-5-family
model with reasoning metadata unavailable. No independently visible runtime
metadata verified any implementation author's declaration. Their work is
therefore proposal evidence only and earns no judgment credit.

This receipt likewise does not infer its author's model or effort. Formal
acceptance requires two fresh declared-model hostile critics and a third fresh
adjudicator, each recording declaration separately from independently visible
metadata. Unverified metadata keeps the model-truth gate RED.

## Custody and supersession

This subject supersedes, but does not edit, the rejected R2 subject. Its binding
predecessor is
`W0-AMBIENT-R7-SPLIT-SOURCE-R2-REJECTION/ADJUDICATION.md`, SHA-256
`a4c51fd431913c63fa6a30e02f7a110653f6bf5c648f007c144f93acaad53ea7`.

The receipt governs only the 103 files in the exact ledger below. It adds the
previously omitted TypeScript emitter, Flat collector, CSS host, and external
literal/serializer parity tests to R2's 97 members. Generated grammar/runtime
files, locks, packages, unrelated dirty files, and concurrent work remain
outside its authority. No generated file was hand-edited or regenerated for
R3.

## Narrow source claims submitted for hostile proof

1. **Literal exactness.** Integer spelling, sign, suffix, signed minima, exact
   unsigned magnitude, and overflow rejection now lower as one
   grammar-to-IR operation. Rust and TypeScript preserve I64/U64 domains; the
   current Wasm ABI fails closed for unrepresentable widths. Float suffixes are
   restricted to `f32|f64`; `f32` is explicitly source-level rounding into
   the sole `f64` IR carrier, not an invented F32 transport. Constant folding
   commits only checked integer and finite floating results.
2. **Path-sensitive payload formation.** One shared cardinality solver governs
   the owned Flat/Inline/Wrap/Alt paths. Classified references are
   caller-visible one-payload boundaries except Scalar, whose literal/null/bool
   and alias behavior is evaluated exactly; unclassified recursive references
   use a fixed point. Real maps replace inner builder effects transactionally,
   StringLit deposits its semantic string, and Skip/Next parse but roll back the
   discarded side's builder effects. HRegex recognizes an optionally spaced
   direct regex map without accepting nested maps.
3. **Tape safety and identity.** Tape records remain 16-byte, align-4 records.
   Cursor/reference construction is sealed; reads and arena access are checked;
   the Tape owns its source and string arena without a safe lifetime transmute;
   rule IDs retain the exact `u32` domain in a side lane; alternative IDs
   retain the exact `u16` domain; the `u64::MAX` sentinel remains
   distinguishable; rollback restores scope, records, arena, span, root, and
   handle state.
4. **Fail-closed generators and serializer authority.** Simple runtime
   formation rejects unsupported scalar alphabets before output rather than
   coercing to Unit. BBNF and Sheets serializer/Pratt spelling derives from one
   operator metadata authority. External parity tests bind the intended
   round-trip behavior.
5. **CSS Level 4 boundary.** The L4 grammar contains no ColorMix/HueMethod
   ownership. CurrentColor is distinct; Named, functional, and predefined
   roles are separate; percentage deposition and the configured L4 unit set
   are explicit. Hex accepts exactly 3/4/6/8 digits and rejects continuation in
   the same CSS hash token, including valid escapes; conversion is total and
   fallible rather than zero-fabricating. The normative boundary is the frozen
   10 June 2026 CSS Syntax draft snapshot SHA-256
   `3f129d17407f9bd027291bcfb8015beb86fc229bf5efe505b49ec2dc30be4390`.
6. **Integration custody.** The TypeScript emitter that consumes the revised
   projection API is included. The source ledger has no compatibility alias for
   `push_branch_tag`; stale generated occurrences are not misreported as
   repaired product bytes.

## Rooted Apple-ARM verification before review

- `cargo test -p bbnf-ir --test map_expr_uint`: PASS, 4/4.
- `cargo test -p bbnf-ir --test struct_registry`: PASS, 14/14.
- `cargo test -p xtask --test runtime_projection_semantics`: PASS, 7/7.
- `cargo check -p xtask`: PASS; pre-existing `LIVE_TOOL_NAMES` warning only.
- Ledger-scoped `rustfmt --edition 2024 --config skip_children=true --check`:
  PASS.
- `git diff --check`: PASS.
- `cargo test -p bbnf --test literal_exactness --no-run`: expected RED,
  exactly 886 stale generated/runtime errors and one warning.

## Explicit routed REDs

- The generated grammar/runtime fleet is stale. Its 886 compile errors include
  old `push_branch_tag`, old `FieldSource` forms, missing exact unsigned
  runtime variants, and other pre-regeneration interfaces. No core or product
  closure is claimed.
- `xtask check-runtime` remains RED because the source-derived CSS sidecar
  does not yet know `currentColor`.
- Keyword map replacement, Pratt map replacement, and TokenDispatch mapping are
  not proved by the shared Flat/Inline authority. They remain conservative
  follow-on obligations and must not be inferred from claim 2.
- Tape checkpoints clone open scope/bounds, forward-child iteration allocates
  an offset vector, `end_compound` does not yet prove handle/order discipline,
  and exact rule identity costs an additional four-byte side-lane entry.
  Safety formation is claimed; performance or optimality is not.
- This is incident-repair prerequisite evidence only. It earns zero BB-C.W0,
  lock, tranche, prototype, parity, performance, or convergence credit unless
  hostile adjudication accepts it.

## Born-RED acceptance conditions

Two fresh critics must each assume the entire tranche/wave/feature result is
wrong and independently inspect the exact ledger. They must challenge literal
domains, payload paths, Scalar/Ref/Skip/Next behavior, Tape safety and cost,
Simple/serializer authority, CSS normative exactness, custody, parsimony, and
wave fit. A third fresh adjudicator must prove every material claim CORRECT,
INCORRECT, or UNPROVEN. Any ledger mutation invalidates all reviews.

Even an accepted source receipt authorizes only the ordered
grammar-to-sidecar-to-runtime regeneration followed by a byte-identical second
pass. Product acceptance still requires full Apple-ARM compile, parity,
no-throw, serializer, corpus, and zero-legacy-occurrence gates.

## Exact source authority

- Members: `103`
- Bytes: `1204031`
- Ordered newline-terminated path-list SHA-256:
  `56a897ab80df83d10f4583db3008ddd726d389abd28a0b39e2a8f40e6adcde76`
- Ordered `shasum -a 256` ledger SHA-256:
  `7525d13a91522ee4dc8deb602965f1c02ca4168983d7cf6b09fca7a4cc4ff090`

```text
5f6b02440d1d772867ad516dcb5ad4e8e36bee36c9d432646254dd733f47b242  crates/core/src/backend/rust/emitter/path_plan.rs
f5b931b97e58f12b56c5f20b56e3f93bdb8de96233e9f524a7191236503fcee2  crates/core/src/backend/rust/emitter/registry_emit.rs
3caef3aace69d576f38a1860217ef38981c229f4b01c81ce07a299052ce18181  crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs
c469a1fe62d509953850a4615a43cb23ca4f09bf1f18fffe26b247e7b3e19a6a  crates/core/src/backend/rust/emitter/shapes/alt_dispatch/mod.rs
6060656977b061b43087f25740fa5031e35266777ec1e9716e6279c28d306f41  crates/core/src/backend/rust/emitter/shapes/flat/mod.rs
fd7d0cd3ae70b3f5995fa70f61f4571751bf9d256c232775eabc844029c3f56c  crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs
8fbd1cb633b53f12a9209967eacdb1b77411577c1772b1734899347ba3d1bee2  crates/core/src/backend/rust/emitter/shapes/hregex.rs
55b7e4d8bba0e896414d629d683a2e45ca2ae75e3d0aaeaaae554ae75daf3fa4  crates/core/src/backend/rust/emitter/shapes/inline/structural_branch.rs
693b5eb151e432f54ca31b18841236938ee4c8d19967a3b2790391bd28f068c1  crates/core/src/backend/rust/emitter/shapes/keyword/payload.rs
a4ac6b05a2ee99f83f5f0b0e6eaaad8ef9fa3a7760de29fa264f335478f0f5ea  crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs
b1f3fd0ccaede772c4b4263fc6a695bed1b0653b0ddc62181e32a996053d6e40  crates/core/src/backend/rust/emitter/shapes/pratt/struct_direct.rs
5cf153ece8436d9b0868eca7ca05f1a583b515f0f6bcb8f83d2d3ae6da2dfe90  crates/core/src/backend/rust/emitter/shapes/wrap/struct_direct.rs
fc09ef13de386dbe9499aef5daaed49b0b64ae54dd96deab4a3b6c0a2d11f382  crates/core/src/backend/ts/emitter/value.rs
b4480a2ba9e41219d780484fc6884d9e1b516ab5000cb7bda391ef49bf684ec3  crates/core/src/backend/ts/projection.rs
408893e25387ce28115d2950f731290a3b491c2f0fc7d44ee7149d2f47bfe202  crates/core/src/backend/wasm/emitter/value.rs
c1fd3f03bc89a5d23653c518144bfbc98e08b12dc2d79cbe7b4350bc9e0b7ea6  crates/core/src/lower/expression/mod.rs
a18163e5dedb2b560a396ebac6e7087a2cf854517b97ebb1e570ad0e56bbdcb7  crates/core/src/lower/expression/pratt.rs
d7b499c4ff317b4fc172f9549ae8f87d0091809f568fe344e9a4337defa16f81  crates/core/src/lower/expression/wrap.rs
867422a27ecd703f9482f056f9dbe50b6fe79ebe12a027e0ac94e82cdbe62623  crates/core/src/lower/value_expr/atom.rs
ec843ebe5f8cc94744f0002b655a444c7846221f1564382346ddf9b5164cdbd8  crates/core/src/lower/value_expr/literals.rs
6c0978901e8611d0e654b21195d2a95fdc1cbb2ea9359e0adcc1e692cd9796cf  crates/core/src/lower/value_expr/mod.rs
cd95e709ff0885bacf3ddfe3d5f118cfcdc2df6ddce4977d4ef12dd6cb436f18  crates/core/src/lower/value_expr/unwrap.rs
3e0f92dcf9d212d296cd2d223ec8709db6bcbf8ab938d0e4eb78b946bbbc393b  crates/core/src/pipeline.rs
3bd5673fd70f0707365532aa0b8824d7a21ac87d06feb56f82c80917bc95ae63  crates/core/src/pipeline/compile/pipeline.rs
d9488da9915672756623c1f6995e14d6b7752a94b33e44f6ca1237cbfd2b5d4a  crates/core/src/pipeline/validate.rs
903437ee813187ff3d1549ed71b9a7aac88f685bdbf5204073adfc10b682f5c4  crates/core/src/runtime/builder.rs
ccec584a1ca1f9afa57574332270c97070c0d436ef1a23338842ca46e664a9f9  crates/core/src/runtime/builder_template.rs
567396cd70b99cf3750f30bbf79eff79bd144d4f9d31fdb49798809190769a91  crates/core/src/runtime/css_l4/host.rs
def8c5383329a0a77c6bb854f377401c782642bad86fe82e405e07312eb9cfb9  crates/core/src/runtime/tape/arena.rs
99e817435e4e85b495bc37495cd50b30e6d740f5e3e2bfa10ed21703a1c6d9bd  crates/core/src/runtime/tape/cursor.rs
5b28f4df1b92e9134084fc3040845462e9a3e1366432019e04892a5b99138435  crates/core/src/runtime/tape/mod.rs
6e97cd265ad5188bfc032c87c2b4e1f88de74445c1be750794e67ef9879f8e23  crates/core/src/runtime/tape/record.rs
a5cd5dee8ded6a0293fcf2f0bb5b02d9b373cc34489c36f25c19f95a531e2038  crates/core/tests/bbnf_struct_builder_substrate.rs
bfee80dded530801421ed578965df10bfffd19a449c0fe011e482f4dc653b67b  crates/core/tests/css_l4_color_roles.rs
a636963d6a8564f0cbad94d7b7800be92cec2c1e5a00bc21e73836912d11dd67  crates/core/tests/css_l4_substrate.rs
7dd113f214d86ba721014207f47adf73933d69021f97750d827c3a216d5d108e  crates/core/tests/emitter_typed_payloads.rs
067cacc3de3c842f1aab7328f311d73c0f7282234efc7cca57fd80300a26489a  crates/core/tests/google_sheets_slab.rs
70898b5e9ea0f6599d18b2d5c2c16ce523bddef11beb21f11902458e28500936  crates/core/tests/json_parity_struct.rs
dc6e2431c725ea87c515c0ac941603629c28be4141f12444b1d95180eaa7ffee  crates/core/tests/literal_exactness.rs
434dc2dc6f468cd586acb537753ee7a766ba551b9d74db4596b6b58be02f5c6e  crates/core/tests/lower.rs
df0ad3c1e0e03475615b90e4809ca773212a8245b09ec1447f9a3594fe87478c  crates/core/tests/lower/expression/wrap.rs
55f6eb207d1b6ab6a5ed10f0a84a412b616a43b9c7e45a307aa8f129a7ab6f99  crates/core/tests/path_type_errors.rs
470d7bd8da163c0f76ef6d64898f98ace08820b2c626c27e9703afda6c4e60b0  crates/core/tests/path_typed_compile.rs
95a186e5cb3d9b531a5a267242e037f0de4ba763ff70aad2bc83a484ca989334  crates/core/tests/path_variant_select.rs
3745b1f73f645b12fa94942cc5e9d8ece6c6aff30c2da0b30aed4ecfd405c4fb  crates/core/tests/pipeline/compile/pipeline.rs
0e3ec24462d61321213375adbf6e3fe3284bc321bd11b1c35d50e61f0b270970  crates/core/tests/project_types_json.rs
0aed67c648bd3a6d6cfe23280f28633d1a13f8f13fa7b416aaf4c90dd0154fff  crates/core/tests/projection_totality.rs
f095def5fd25588e494037ec25f6e6f48a862e0f5416b7e5093bd17711143c85  crates/core/tests/serialize_roundtrip.rs
8b6fb4304f5c297b3b082c793eb07a567f5bcb8c6f8e91d4cbd8ed9f943e52a6  crates/core/tests/sheets_expr_parity.rs
9e7581c69d83fb877a9c662943b494f8f20bf13ba07bab72876936a6cd42c966  crates/core/tests/sheets_parity.rs
79f0921c4e4d3e520d6c9a218cb07972978e62b3f96a05656e2bb43132498019  crates/core/tests/sheets_self_parity.rs
61cc3a15042bb5e2ae227f1fa517f97f2c3b4f5d9c8e99430f430cc3a8dacea6  crates/core/tests/tape_substrate.rs
f5ee763c54419d7a7531c2af276c75ea60d66219d6a835055c58f8349c322c59  crates/core/tests/uint_literal_backends.rs
c3010aea9c7f48bcd716f5ce3a601aa918c1268162287d0e7803b9f4a81dd26c  crates/ir/src/lib.rs
6c0d8f7a5a4740f445aea0e64826bd16f9e7349a51180911b5a892f88c7e7430  crates/ir/src/passes/payload/scalar_routing.rs
c0c5edeac7959269557fcab3af10733a44ca915decf419fd4aba00048acce9ce  crates/ir/src/passes/recognizers/grammar_facts.rs
e9a520d1ca95a5e97ad74390fcc1f18d0839c895cc7f037b68e001d66a723366  crates/ir/src/passes/types/registry.rs
52555a13d5cad12c978f151b034767e9d91c8d7b7bec9da050b138c09fd4301c  crates/ir/src/passes/types/subvariants.rs
838ed1d2715edb59d59b14bb31f851e74da187de0314675e554c219c83441a86  crates/ir/src/registry/mod.rs
e63701b08f036c72fef256c7436281bfb283ca88bc481db4366578dd86d9b543  crates/ir/src/registry/struct.rs
e129958726e2a9ff4af1131081c94716ef4a7499a21d83588f8a92a74be860c6  crates/ir/src/types/map_expr.rs
2bf296f8123dbeab3f05821904f0875b81f425ed7cff20681d2b68e02a01adbc  crates/ir/src/types/rule.rs
85e270ee415d47423317c74e12152dfba2affb1ef036a061ff184def02bd5e83  crates/ir/src/types/type_desc.rs
fd94189ead2f9530a76bc91af27f428ac45b59d52c54a15d208adf598b43e632  crates/ir/tests/map_expr_uint.rs
4017c12c3ee92c7e4be79758171a129647e5d1ff8996cf36475877e09e1a82bc  crates/ir/tests/struct_registry.rs
eef788e29119b7eb88444e1dd0675bc744a1802468914e9cb402d78c9c267a57  grammar/bbnf/bbnf.bbnf
6651d74ab49c5a0c1d84b73a65ec28ea488ff6b38e178b0b56c1f1eeed1e5553  grammar/bbnf/expressions.bbnf
db3503921bec484807d231095be8306eb4d9e231d5771eff2da8ad3caca4f008  grammar/bbnf/types.bbnf
8d4de97d9a54a1fef6f7ccccdeabd185d66c8614f167fc3ae28225b7994e4a1b  grammar/bnf/bnf.bbnf
24b25dfb36518e485271af995e2f6012e1807a3d57152e3649700017e5532a52  grammar/css/l4/color.bbnf
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
2386b95b6af21d8e474ca791ab8e86b545931271fc99e9d4adeeff30c2bb703e  xtask/runtime-projections/bbnf.toml
7fdba2d4f7c7846184c40c920b2d9d8fcd429721b2b83d93cef75817764cf922  xtask/runtime-projections/bnf.toml
931e7d854b6c3fdb63ba8fe0fff7c6d1ba33e3eab3068c3081d704851d2ade50  xtask/runtime-projections/css_l4.toml
f06b1097d1c657571ac674ca7aee6c3925144214bb4dc34c509efa2486e95eb1  xtask/runtime-projections/css_pretty.toml
23606a592405b4b17b5d1c280bd59ccb4f7ca9a9df7dc6dada6aa3ac7ab93854  xtask/runtime-projections/csv.toml
a4761322adafb6c940126f903f2b01d4c9bfb08a9c6c5a4832daa4d3ac99b730  xtask/runtime-projections/ebnf.toml
4facfd43428f74d4809e00c84a8bf71b3755c5bf5724f97b5fdee109f8aea3d6  xtask/runtime-projections/google_sheets.toml
035931eec75498a47a690d06028e55a8e3a7f496c39f27c8cb576b15ae26fa62  xtask/runtime-projections/json.toml
827e0cf43458528bc3b5a6a02aeb2fbda5a694348594a62e261ef77e57efd0f2  xtask/runtime-projections/math.toml
d1b68c40ac94c5e165642ead5fc46a4f87f03da87735b2cf46805ccda91f9f8c  xtask/src/regen_css.rs
014bb7106af240744a83afdf6e6bbeefce577888ed4a623e293c79d77d654069  xtask/src/regen_simple_runtime.rs
66de0eb36aaec3e4af8a0bc34c43abf9b29040e1a433c6821034f7fe86ca0779  xtask/tests/runtime_projection_semantics.rs
```

