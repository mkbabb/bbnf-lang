MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: R7 four-cluster reconciliation analyst.

# BB-C.W0 R7 four-cluster reconciliation

- Date: 2026-07-22.
- Posture: the sealed fleet is wrong until each byte class and consumer is
  proved independently.
- Authority: read-only reconciliation. I ran no build, test, generator,
  runtime-projection check, profile, or mutant. I changed no product,
  generated, runtime, projection, package, lock, or stage byte.
- Binding predecessor:
  `W0-AMBIENT-R6-GENERATOR-ADJUDICATION.md`, independently verified as
  SHA-256 `4ab6aea4d4449b24e14a98dbe28d20ca972446f90bebd05b29f6e7cc52ee60f1`
  (294 lines / 16,072 bytes).
- Sealed input: durable custody manifest
  `W0-AMBIENT-R7-CUSTODY/SEALED-FLEET.tsv`, SHA-256
  `089bc000aaca7120190fa0724e0e080379adacb1bca8e9db86d1a5db919ff227`
  (19 lines / 1,669 bytes). Its 18 members total 9,082,868 bytes; its
  normalized path set is
  `b24a36930159074c94aadd44779a9c607786ab7078f7fcb749ec2104fc6b5315`.
  The custody copy is byte/mode/SHA-equal to the R6 source fleet.

## Verdict

**RED. Do not land the four parser/sidecar pairs.** The drift is causal, not
random: it is the projection cone of the repaired map lowerer and direct-IR
pipeline, plus scanner specialization, path-plan emission, and the already
known 7/7/2 Pratt bounds hooks. But causal provenance is not semantic proof.

The smallest complete byte envelope is now identifiable: eight sealed
parser/sidecar files plus 31 generated runtime-projection outputs, 39 files
and 7,478,630 bytes. It is not an accepted candidate. Exact type/field/path
contracts are missing in all four clusters; production recovery coverage is
missing for CSS Pretty; affected scanner helpers have no differential
acceptance/offset/no-throw proof; Sheets has an ignored lazy-path row; and CSS
L4's 86 changed layouts and 140 new path rows are covered only by permissive
floor tests. No execution in this receipt can cure those gaps.

The runtime projection delta is statically **zero bytes** for all four
families. `regen_simple_runtime.rs` and `regen_css.rs` read registry sidecars
only to resolve configured rule names to `rule_id`; every `(rule_name,
rule_id)` mapping is identical checked versus sealed. This proves substitution
of the sealed sidecar cannot alter a rendered runtime source. It does not
prove the current dirty runtime trees equal a fresh render; that check was
prohibited and was not run.

## Exact generation and consumer owners

The following are the byte identities used for ownership, not names inferred
from a generated diff.

| Responsibility | Exact owner SHA-256 |
|---|---|
| structural expression dispatch | `crates/core/src/lower/expression/mod.rs` = `3cb889d8d0f3ae03ce64abeb4a5c5513774939ad9db9575c851f33147aa7a16a` |
| mapped-factor/value/type lowering | `crates/core/src/lower/expression/wrap.rs` = `7edbb5bca06a63304615afb4b0560f73a0bb3d6df3439b7aa8c633f6cd323f17` |
| Pratt lowering | `crates/core/src/lower/expression/pratt.rs` = `0d3a50709f40db54569a47cb80e62e5ed076a3148108fb4e33aa8e3a301b79f9` |
| direct-IR pass order | `crates/core/src/pipeline/compile/pipeline.rs` = `4573d5a8d6ff55b01dfe5fd092f25e666c107be38eaebc665d7343ca17e27278` |
| scanner specialization | `crates/core/src/generate/regex/emit/scanner_plan.rs` = `89ae8c7e04ab7f8fce8748205070a39f4ac7c6bb4b761a537b90f8e55adb3808` |
| quote/digraph mining | `crates/ir/src/passes/sets/structural_alphabet.rs` = `32dfd8cc0a19f4f52e8b2e3f945a64988f5ab714d684d5e48108f9f2c24a5801` |
| generated profile | `crates/core/src/backend/rust/emitter/profile.rs` = `70c8e510ff4f1b8b4e4efd0ce2124ef6f280ff678baa02794bfab77a44788d73` |
| type map / registry population | `crates/ir/src/passes/types/type_map.rs` = `797e89d340515a63b5961392a5b5bd3d871e96861ec99a36cc91eb0e7497b2dc`; `types/mod.rs` = `48bdab1ac30f15320214038318eeaf1800a6d4eb0c10008397643d2b3b325912`; `types/registry.rs` = `baf448dbfe941c584343080f391b0d9cb2a6708db3725313624b183d34e32312` |
| registry and path-plan emission | `registry_emit.rs` = `90f9451b5fdb066fd36b27bf7b223bf826e6ad5daa42f63f9477e1a9a51532c7`; `path_plan.rs` = `31988f0b3e52dd3be68d4a45fcfa76e9b27629df32cf5d9deb1639031df7753d` |
| Pratt struct-direct bounds consumer | `shapes/pratt/struct_direct.rs` = `bfc2dff4727d11dcf62bbb3f423cd0c0f3f52e7c888c9e15112cdf6d43f288d2` |
| parser/sidecar writer | `xtask/src/regen.rs` = `2fab26453b6a7a7b256208642b723081affeb10e493d60a78d91bff576268f9c` |
| simple runtime projection | `xtask/src/regen_simple_runtime.rs` = `82a0e360b64bcd762dd90e584f32f95911bb6fa05305168ee104f257a3621a8f` |
| CSS L4 runtime projection | `xtask/src/regen_css.rs` = `16cf68e5b37cd5bfff642ec064682012bd5214ee02e9daf438fba8f0230ae335` |
| sidecar compile-time consumer | `crates/bbnf-path/src/registry.rs` = `aa80cacb18a5c77636a49cfdfb7214e454d90a918e4e5d3638a6c6b22e63b347`; `path_macro.rs` = `17023fecc4a17dbf8d169d7f1fd6e02f14c2d7c4abfe608c3d8bc056f0de8eda` |

The source grammars are clean. JSON is
`ab8d21218b2a5f49d9231af6f71503a9949782fdc0a50d81a845fc69054f2604`;
CSS Pretty is
`38682d3ff80bc66910dfe4dcc2f962fa4be5cc85cb360f18df00ebe92d98930b`;
Sheets is
`78372003a65bade3baab67ea60e070f9bbbdfe112ed143cb5fc6b0d9ae8704a0`.
The 15 CSS L4 module hashes, in filename order, are: color
`e923f52ceb0649cd7817191347aa24105f3c73b42749d9528b6c8cf7ad81123b`,
easing `f5550fa89d67644d72ead3b6e30b33e28ff2a41219f91a98ec15d4424cde50bd`,
filters `8321e5436797fe1bf5a614cc46d529ff56164e21d07be49ef3cf7f4dfa622537`,
func-body `5b804cb75611645889babab14783cd052321fbf46694eac5b604ba8bd5e854d3`,
gradients `f0f2495d39dc0ee85cf203b622bb4507d7f3cf860817f1544dd1659ccbe4143e`,
keyframes `889dec7c994c3379c069524f6ed36ddc24c96de9663b4f6dab8ce762f43dffd8`,
keywords `bac8e98937bed7e3c4ea6fc2bc5dea9bd8ca046299a23879a8ff81685540fb0f`,
media `e0cf30d97ba7861dc7fadac3264e542c07317b32fd6f6df07ee4f71413aa005e`,
properties `94451adff5a62ca401f935d696cbd9ee427feefc87660a4538cd20b2d768a8a0`,
selectors `455678a59a1a77c868326d988dd185a83a3c84c3a1a006b10fcbcea85de6c966`,
stylesheet `2c2e62e71cdbd76be78906ad5741763c3ba12c3d7418bc6b39620330027fda07`,
tokens `23e535851ce43d7c5e6b243e3b766a4616c5181bf20cc6017405e4406f954054`,
transforms `650cc388d652f43a166a893ea8ce0538d04d2b733d63f2feefcce02127617cf5`,
value-unit `cb57f79050a6304d79e3a7d8c7b690c902f243880aab4c896a63899290dd727b`,
and values `ecbc3ad7997485abe8964f2f19df4129526ebc367270939e0a8242d8b7215396`.

## Checked to sealed artifacts

`Diff SHA` is SHA-256 of GNU `diff -U3` with stable
`checked/<name>` / `sealed/<name>` labels. Hunk and +/- counts use that
diff's alignment; they intentionally need not equal Git's histogram.

| Artifact | Checked bytes / SHA-256 | Sealed bytes / SHA-256 | Hunks; +/- | Diff SHA-256 |
|---|---|---|---:|---|
| JSON parser | 163,767 / `82e49add58942e9214995cc1665bc952da3da9d8d563fc25235296b310e413ef` | 119,642 / `36da388fe4c1042a2fd3ff7812e9741ffb54025261ef7c56333673f8344bc95f` | 8; +9/-655 | `9acc3f35755577b1dd2b4d7467f22d047df82ac1d66b53f82f5adf587bbc40cf` |
| JSON sidecar | 4,415 / `8a9500a99b722aa89c44703f1eb9ce6fc9bfefd02780b2d2539d91ecda62ec7e` | 4,407 / `33229d745e334610cfa94f0bf3d997a1eee049ea4b27cb8378ec9166e954b89f` | 3; +4/-4 | `d6916c778cef7a91e0d2808c372bc1a06f299675265570691d24292502412b1e` |
| CSS Pretty parser | 529,141 / `e44d2f8c1a7d4bd649a9226b7aeb708735d28f626cb90ee06e8b9ceff7301299` | 305,493 / `0ef4b9e3153d52d9f3eeba9e0780ba57ce193c90b6adfa0f4e68566a6d698288` | 46; +545/-2,974 | `46b4053c3d733e422a9486e5a9a04389e01c78d3cb7f02dba0728f65029a33c2` |
| CSS Pretty sidecar | 11,944 / `bc50b5f3efce9f0a252ebb8674768161f8ccc68246ac935ab60777ca8f0780a5` | 11,944 / `2b93434e5acd54000c1cb9d235df5b7ef5778de7bb7682a790c86b7b0e64fc7a` | 1; +1/-1 | `e5bb5ce043cf08fbd2db86b22dd3725c1da092d30442c69f6128f6ab9881dd22` |
| Sheets parser | 677,373 / `345397a562d102fefcf85908dadb9784737db0c14904c7e98e97fa1d6c9aac79` | 674,254 / `e200eb9867fadd1ced8148144de5af45cb0357a2e61c2b0829c7ac0a35732b84` | 39; +168/-149 | `d7bec3bac496c64fe596cb9e10359b341f3290d2b31b3c6606c46f0c0c250fbe` |
| Sheets sidecar | 27,211 / `37a2ee49d2c41b952be8d671baa33b5cc612fbe6c5db25845041bc2bd1f1cf1c` | 27,312 / `f916a1ee9fe5a34493b6ac1c6ed7ed093ac013ce5c39a1079979d703d9927451` | 20; +40/-30 | `7919250b66fad8f24002cafbf1c392f63d4749ed963e98ac11f28fe1d8b3cf21` |
| CSS L4 parser | 6,105,898 / `ac08b34d26badb675ce8357b28d2f7ab596eabc74c5ffe4f9d965e3549c2182d` | 5,983,578 / `d98882ad48baad4a3898091aa53c903399929919804c61fad61fe636e10b40fe` | 743; +5,733/-6,618 | `d5f641667a270e35074ee575a05a63e3d8d085548fd84b50b038d6e221b79eef` |
| CSS L4 sidecar | 193,459 / `77e0ecad76b34f4c5233ea76a5b90b946e7f07c411c83e05a0ca1b8d20c97289` | 218,620 / `5477673e1abb12104488cb941ec84446a419dc04f2b9866cb13bfe41bbb54617` | 282; +1,491/-522 | `8f48be7be164cced04ec00db0cac5ec1439e61e16a223224168715cd0a8be165` |

## Exhaustive parser-hunk partition

For each parser, I split the stable GNU unified diff into complete hunks,
assigned each hunk by the nearest old/new generated declaration, then
concatenated complete hunk text in file order inside each class. `Class SHA`
is SHA-256 of that concatenation. Counts sum exactly to 8, 46, 39, and 743;
there is no residual hunk.

| Cluster | Exact hunk class | Hunks; +/- | Class SHA-256 |
|---|---|---:|---|
| JSON | embedded registry | 3; +4/-4 | `b15164e13267b5365b23462d6e7502b75109220ae16e516c0e46103eeb2465cf` |
| JSON | prettify scanner projection | 5; +5/-651 | `464f67568e2798e0279867e36a854146f513fec3946d81028b1b774662c77d6f` |
| CSS Pretty | embedded registry | 1; +1/-1 | `f1453f93a758ec0931f517c2595c8abeddc3ebcb1508669169f2421ac11c62a9` |
| CSS Pretty | scanner tables/dispatcher | 10; +180/-35 | `e12c8778adefc459e04d166970bf7532a725435f5e514d6ef7bc14ba68a650bc` |
| CSS Pretty | parse shape/typed leaf | 2; +140/-12 | `1abac2553db6a76fe46f4e84792037a286bbb70f0dd10e8b66ec8bcefa531a09` |
| CSS Pretty | prettify projection | 33; +224/-2,926 | `59408c1f05aa9ab1b22e6794a40b64c78c2ae8e9f5dbd305c4827820299253ad` |
| Sheets | path plan | 2; +32/-2 | `bece5d02f6e603a0a31b546dbfb92ee5f44f2dd3f807913dfdc24358f3dec483` |
| Sheets | embedded registry | 13; +33/-30 | `e6515fd9c615979d652dda6c5209fca421f6bb247222efa0eb1b90309e4fe28b` |
| Sheets | seven Pratt bounds sites | 21; +91/-0 | `fa2a6dc2f1ec0e4d54a813dc7c88514f6290904fd447c160e0e6828c60f76cf0` |
| Sheets | prettify scanner projection | 3; +12/-117 | `1e16e3bc19da014aa2c5400405a3c0afc95f6e57fc3566c413984c9ca96fcd6d` |
| CSS L4 | structural quote profile | 1; +2/-1 | `e751530260f7498f0d726f8ea3d7f2a64852ad627c4ca95cf05b1c371d7b97ea` |
| CSS L4 | path plan | 12; +842/-2 | `42e3c75d7c9658c38fcd5cafaa6401eb8fc22818db248ee604b63614bab03968` |
| CSS L4 | embedded registry | 115; +726/-516 | `ace95fa09085f82b0d5ad9f25702cfa7d1977906e82b99e74455adc98fe0af83` |
| CSS L4 | scanner tables/dispatcher | 46; +227/-175 | `547fa32d273292df1e573dd50103eb2fa2955ad33008dfcb40fd43ff83799229` |
| CSS L4 | parse shape/typed leaf | 27; +569/-201 | `f658aceb533438e0f71f5635fe1ae37e7aed4aa78e89582be024413cdad1e825` |
| CSS L4 | two Pratt bounds sites | 6; +26/-0 | `4405b6e80d0bbc75565258727178eaff61f80022208aca6e87b2ccbe40276b66` |
| CSS L4 | prettify projection | 536; +3,341/-5,723 | `bfbc227806422f4b5b064bc6ebd263c4662c99148a861279a8e9ea424b5ad901` |

The ownership/consumer mapping is exact:

| Hunk class | Source decision owner | Semantic/runtime consumer |
|---|---|---|
| structural quote profile | structural-alphabet pass -> profile emitter | parser scanner profile and quote-aware recognizers |
| path plan | projected `StructRegistry` -> `path_plan.rs` | generated `__path_plan`; JSON/Sheets/CSS L4 `parse_with`; typed path execution |
| embedded registry | mapped-factor lowering -> type CSP/registry population -> `registry_emit.rs` | struct-direct `begin_compound`, audits, generated paths; same data serialized to sidecar |
| scanner tables/dispatcher | direct-IR pass order, regex recognizer decision, `scanner_plan.rs` | generated parse scanner/offset transitions |
| parse shape/typed leaf | structural/map lowerer and projected layout | `StructBuilder` callbacks and family document materialization |
| Pratt bounds | Pratt structural emitter | runtime builder `record_compound_bounds_start/end` hooks |
| prettify projection | projected rule shape plus scanner plan | generated `*_prettify` parser and serialization/canonicalization callers |
| sidecar JSON hunks | `xtask::regen::write_registry_sidecar` from the same registry | `bbnf-path` macro/type checker and runtime projection route resolution |

## Cluster findings

### JSON

The grammar explicitly says `null = "null" -> 0u8`. The fleet changes the
`null` layout and `value` branch 3 from `Span` to `U8` (two changed layouts;
no kind change), while the generated null parse path still deposits unit and
the runtime builder materializes `JsonValue::Null`. Thus runtime JSON meaning
can remain correct while the registry claims a different wire type. That
registry/event concordance is unproved.

The other five parser hunks replace four large prettify scanners with two
`scan_number_strict_span` calls and three `scan_quoted_string_strict` calls.
The scanner-plan owner explains the bytes; no affected-pattern differential
test proves accepted length, rejection boundary, offset restoration, and
no-throw equivalence.

Runtime consumer: JSON projection routes object/array/pair by unchanged IDs.
The checked and sealed rule-name/ID stream hashes are both
`7008f3cdf3270a3d92d29535e769becc9d12b5d3d928c56460dd979bfa466858`;
runtime output delta is zero.

### CSS Pretty

The sidecar's sole semantic change is `atRuleBody` branch-0 field name
`"43" -> "42"`. Neither is a grammar field name; both expose an interner-ID
fallback as public registry text. No test establishes that the new number is
stable or usable. The parser additionally splits/reclassifies the three
`propertyName` regex alternatives, changes declaration parse layout, changes
the regex dispatcher, and replaces prettify scanning with 15
`scan_ws_block_comments`, one `scan_ident`, and three generated regex-scan
calls. Those changes account for every one of the 46 hunks.

CSS Pretty declares three production `@recover` directives, but its only
family integration tests parse three valid corpora. There is no malformed
declaration/qualified-rule/at-rule recovery assertion and no generated
prettify output/idempotence assertion.

Runtime consumer: the simple projection uses sidecar IDs only for kind
routes. Checked and sealed rule-name/ID stream hashes are both
`926a4e9d559f137a3f7c35ed1697d7d491bf108b6be1766a2199694c5f918fe3`;
runtime output delta is zero. CSS Pretty has no supported `path!` marker and
no `parse_with` runtime, so its numeric sidecar field currently reaches
registry/audit consumers, not a family path executor.

### Google Sheets

Nine of 37 layouts change. `compare_op` becomes `TaggedEnum`; eight rule
types change (`error_literal`, `compare_op`, `unary_prefix`, `mul_op`,
`add_op`, `comparison_expr`, `mul_expr`, `add_expr`); all nine change fields;
`primary` branch 8 becomes `BoxedEnum`. The new `compare_op` field names are
`"96", "97", "97", "97", "97"`; duplicate public variant names make the
path surface ambiguous until proved or repaired. PATH_PLAN grows 148 -> 153
with five `VariantName` rows.

The seven Pratt functions each receive exactly three bounds hunks (21 total,
the already adjudicated 7/7 direct consequence). Three prettify hunks replace
identifier scanners with `scan_ident`. No parse typed-push class changes,
which makes exact registry/event tests mandatory rather than optional.

Runtime consumer: the typed-formula projection's sidecar dependence is only
kind-route rule IDs. Checked and sealed rule-name/ID stream hashes are both
`6b184c457497a81dd12791f93de18b0f918f28ac91bd64b10d7a111422c4f8f0`;
runtime output delta is zero. Generated `parse_with` nevertheless consumes
the five new path-plan rows at runtime.

### CSS L4

Eighty-six of 188 layouts change: 25 kind changes, 71 rule-type changes, and
86 field-vector changes. PATH_PLAN grows 1,054 -> 1,194 (140 rows). The
structural quote-class profile changes from empty to `[34, 39]`. Added helper
calls include 13 `scan_number_span`, seven `scan_ident`, six
`scan_string_quoted`, 11 `scan_ws_block_comments`, and 15 generated
regex-dispatch calls. Two Pratt rules receive six bounds hunks.

The 86 changed layouts trace to grammar owners exactly as follows:

| Grammar module | Changed layouts | Rule owners |
|---|---:|---|
| color | 3 | `namedColor`, `colorSpace`, `colorFn` |
| func-body | 2 | `mathProductOp`, `mathSumOp` |
| keywords | 13 | keyword rules from `displayKeyword` through `cursorKeyword` |
| media | 4 | `mediaType`, `mediaQualifier`, `mediaInParens`, `mediaTypedQuery` |
| properties | 18 | `cssString`, nine property groups/declarations, and `declaration` |
| selectors | 19 | matcher/combinator/pseudo/selector rules |
| stylesheet | 3 | `keyframeStop`, `keyframeSel`, `atRuleBody` |
| tokens | 1 | `string` |
| value-unit | 19 | unit, dimension, `length`, and `valueUnit` rules |
| values | 3 | `globalKeyword`, `mathValue`, `value` |
| compiler-synthesized | 1 | `__pattern_90b9f441ae92fa6c` |

This is a genuine transitive projection cone, not 743 unrelated edits. It is
also too broad for existing permissive tests: several public shapes move
between untagged/tagged/newtype forms; typed dimensions replace Span units;
selector tuples are restructured; and `value` gains `U32`/`U8` alternatives.
No exact golden pins the 86 layouts, 140 new path rows, or their builder event
streams.

Runtime consumer: `regen_css.rs` resolves configured names to IDs and emits
seven files; it does not inspect layout kind, fields, or type. Checked and
sealed rule-name/ID stream hashes are both
`14d16c86af2ccc860d85f0b6f66d9583c2a17e44128e0d91df4ed7df86f39ee3`;
runtime output delta is zero. At parse time the unchanged CSS builder still
consumes the changed generated event stream and rule IDs, while `parse_with`
consumes the enlarged path plan.

## Smallest atomic byte envelope

Projection sources are bound as JSON
`035931eec75498a47a690d06028e55a8e3a7f496c39f27c8cb576b15ae26fa62`,
CSS Pretty
`f06b1097d1c657571ac674ca7aee6c3925144214bb4dc34c509efa2486e95eb1`,
Sheets
`4facfd43428f74d4809e00c84a8bf71b3755c5bf5724f97b5fdee109f8aea3d6`,
and CSS L4
`9b3c5752d9940a89ea838e104cdbdf58b019cc4afd51ef0b96eb2da9c3fb96f6`.

The envelope uses destination paths, mode `0644`, byte count, and SHA-256,
sorted bytewise and newline-terminated. Its 39-path set SHA-256 is
`0c35a5ed00203bcbd69013cab135dbc070a2fcea3a1cb13a169c9887365e69b1`;
the complete manifest SHA-256 is
`0b1a61400aedd6aca11e69daa68f51aebf505471e46a97ceaac86a16c47f3e83`.

| Cluster | Sealed generated bytes | Runtime projected files / bytes | Runtime path-set SHA | Runtime manifest SHA | Cluster bytes |
|---|---:|---:|---|---|---:|
| JSON | 2 / 124,049 | 7 / 22,451 | `27556624b2c071c2e9fde737a2f5bcde066dddc15dfa58953f4fbe7ccbed22e6` | `0f288d760c3cd6bbdf1375993cc234a6adad38afa70b1e7a5805fac496a1476e` | 146,500 |
| CSS Pretty | 2 / 317,437 | 7 / 11,781 | `121794d5dab36c3504ef9385afe3d97294056662339aa5e4f52a31a002256a9b` | `80c939c4720e258124aa4e079247b47a892f07db0c3123566df83f0682db7b53` | 329,218 |
| Sheets | 2 / 701,566 | 10 / 31,944 | `564e0d8bf9380d212ce6cdef290ef5b3fcebd42d0b009204844c4eea3b068f8c` | `c8aed13d23775a7e9fd3f63e4cc49bb55b004edafdb97e778e35e24f4f21007e` | 733,510 |
| CSS L4 | 2 / 6,202,198 | 7 / 67,204 | `f72181dd994b6acdd2e024881bfa00d0b0f7fd7b253b56ab15c9997ae5de4a82` | `46433e0d93d651dcab134519250bf9b86ae2e2a8fd56efc846fc0b9a119d73f8` | 6,269,402 |

Exact runtime file hashes in that envelope:

| Family | Path -> SHA-256 |
|---|---|
| JSON | `arena.rs` -> `df7c50fa6afb8dee2db39f9329897c51fb4a588b5217043a534451eec0953610`; `builder.rs` -> `e633e468ccaa5f0dd1bf560d3b1d73c63afdf14a8c04867f510299474d2d1619`; `document.rs` -> `94142b66a92c152c95dcabe0afb25d2a369e5930ec7be678fd39d96efd75fe67`; `mod.rs` -> `1d507c933b1c071c73f485ef1e1613a3b307a755a0bb5ddbc3d6584f93095628`; `parse_with.rs` -> `8e44509759dea0f43c83aa7b9a268298d3e8476ee7c27aae722d856c0e52b7cf`; `value.rs` -> `85b96ca1763dabcaabe870989fa71c918e7f77d5980c907c92b505107528f9b1`; `view.rs` -> `ac0c3d9d23c8f4dd3537f2460756ab4b1823e741905e21b8c14cd2c49a7a1cff` |
| CSS Pretty | `arena.rs` -> `d58908be69d7b2eff69c611562fd47ed5e3df11216ca91618090a41aa69c7a08`; `builder.rs` -> `29ad050ec8c23392dbcd60432aea03d0febd32e086734ec89ece1aa5dca25290`; `document.rs` -> `734bfc29991ef015cc08af10cd25dbcf0b161d30cfd1fe5b54e29cf45088a86a`; `kind.rs` -> `73d74cc361de1036d19d0839c3029099811031cf01b557bb1b815454355d9363`; `mod.rs` -> `2187b8a323f1b028e37e46f53f9e7c673968b6b03913f557bd4d6906399f8086`; `value.rs` -> `14154d3b9e558e648cba009eef49da595816ec70927af79e06afe589a5089c04`; `view.rs` -> `c3ad11cdefe21edb0554f6c431bff1537cc40cfc9c910f31e4034a527d515ade` |
| Sheets | `arena.rs` -> `d979f107835f929da45d6cbaedb51da3d93b6fd5ed5d21261ca34a829ab3ef3a`; `builder.rs` -> `4256949fa7727b2d4ff415f5054230c393e0e8c1ecccc7156af4598f9174f2d6`; `document/canonical.rs` -> `8ea1c0f8dde5bd099fbaba49ec1303445d52a2788f4bfadd870e2975359eb826`; `document/mod.rs` -> `a3c6fcb9afc870d259e90908b229fb925c06258058f9ae6825de1fcdc8df9388`; `document/path_query.rs` -> `a50dd0494224ed45ed2dd42d1af687718b6d37a2ab40b35a92e3b75acdeec36f`; `document/view.rs` -> `0e25ee295c4248b69203a634300d2eabc3d86d61ef666dbc88ab1273adcb6e46`; `mod.rs` -> `ecb447040fe21bce4297a3100c27631ca0ca013e7562349026e77c6a04b9aa28`; `parse_with.rs` -> `c67b54c3888c27211ae2a1341572f2aa2f401b59a4638ca5d951f40ad0bf21d9`; `value.rs` -> `93d4fcab37850e29ba89402ced53bc93c7c8c8ba2de92547d1c15e133d544fee`; `view.rs` -> `73b91167354b049199a3c1473ddb89672def1e94476e3d2779deb382d29f5671` |
| CSS L4 | `arena.rs` -> `43a63c536ed4ca6a5fb67ee9d42853c7a26f8e38060da5a801c27ff1946467af`; `builder.rs` -> `364008b2bbfca20e41421fb21c73e0bfce37df52d9ec6f3560a650fdd75c1519`; `document.rs` -> `f906c3636c9e40899c3d27fdf74c2a9d27c5c804cbcfc5c7fac1f8bec648db7d`; `mod.rs` -> `d9470ae9809e20f9e92bc914043dd75a623f8ef2da97ba7c04b8b0a82b13a70d`; `parse_with.rs` -> `068703436916d466c2a44fc625917b1032e60053f2c03b019ab5a33339fa0916`; `value.rs` -> `90878deddcf2575b97f27ef0c0361e07dc823d80cbd26b3c604ed97247d47dde`; `view.rs` -> `a3f267bbfeb1da6bae481ef663542d9f36afa6c47f0ac1e8c4fd06e8f12be615` |

`runtime/css_l4/host.rs` is hand-authored, not a projection output, and is
therefore outside the 39-file envelope. No projection TOML changes, so those
inputs are bound but are not output bytes to land.

## Existing and missing proof matrix

No listed test was run here. “Existing” means an inspected active assertion;
it does not confer a pass on the sealed bytes.

| Cluster | Semantic | No-throw / recovery | Path | Serialization / pretty | Exact type/registry |
|---|---|---|---|---|---|
| JSON | Strong active parity/materialization: `json_parity.rs` `a76332a5f10258b3765cdd03a9987a7adb7d07df0294b42e6b7b82279abd3122`, `json_parity_struct.rs` `1838b3dd4481be135d61980e806627f05d0cf255fbd50f29c608820b48307ac2`, `json_decode.rs` `8e439fe1ad51d765a30687a2d66655d65819041160254a63904b96778d10baa9` | Three malformed rejects exist in `structural.rs`; **missing** affected number/string helper boundary, restoration, and catch-unwind corpus | Three active rows in `parse_with_json.rs` `e14cb052495a4a18d1ed33e2fe68aa9bf3ccd308ae3524e7319eb929797a0125` | Active round trips in `serialize_roundtrip.rs` `b1d4b1c0403e137f3e8d0cef5b05927591d329b1645b1c121df7e6513db3aaa7` and canonical parity `e13f2fe87c2d66cbc3f3732378e48d17eef908916bf2d54a15bb18e5eae0ed42` | `project_types_json.rs` `136d5c4e4db6923e2844d315ec60a004e60d61793d17ae91b9fad72a189912cf` checks TypedLeaf but **does not assert U8 or registry/event/runtime concordance** |
| CSS Pretty | Three valid real-corpus parses in `css_pretty.rs` `14212b62888c901bd15e17a702a7dc99e8b812409c0dd2d4bd38514b92099027` | **Missing** tests for all three production `@recover` directives and changed scanner helpers | No family marker/`parse_with`; **missing** any stable assertion for `atRuleBody` numeric field | Only `doc.input()==input` smoke in `serialize_roundtrip.rs`; **missing** generated prettify output and idempotence | Only permissive payload-layout count probes; **missing** exact `atRuleBody` field-name/source golden |
| Sheets | Strong operator/error/AST tests in `sheets_parity.rs` `e35cd4ab7aacb959c50bef9c00056d872a601464d96bbe1b1fe5ac26133593bb` and `sheets_expr_parity.rs` `ed4b8fdcf35e892a73bf2b151235d412bcfe1fb5ea670fc420d25cba24de4776` | Invalid-input smoke exists; **missing** affected identifier no-throw/differential corpus | Happy/invalid rows active, but lazy-error row is ignored in `parse_with_google_sheets.rs` `9dd6ec2f6ba1f28b208649514ba98a67efb5e3beac942556682518e6ca02ec50` and no new operator VariantName row is tested | Extensive active serialize/prettify fixed points in `sheets_self_parity.rs` `48d4dd13d11cf2febed3b149e80649c3b3f00f16a9872351ac38ce0c57ad95a6` | `project_types_sheets.rs` `94a8739dabd979a433c067d4eafecdd8bfda0c6472d3e20bb45f849d151e154b` is permissive; **missing** exact nine-layout golden, unique variant names, and event concordance |
| CSS L4 | Broad dimensions/selector/color/parity tests, including `css_l4_parity.rs` `7ac57ef88c454a938c3ab5fdf2a3a0d98330824b250281af985ea3bcdcf00f39` and `css_l4_substrate.rs` `e17c6322b58a3bd463ca80022358dd5a1c11e7e241dfb1864b7cfd1ed87d34f1` | Real corpora parse, but **missing** malformed affected-pattern no-throw/differential corpus | Three active rows in `parse_with_css_l4.rs` `c748cf3f9af893b937e7383c345b440f86cef6c09d54949a6800b6e545ebda54`; **missing** assertions for 140 new rows | Active canonical/interoperability pretty tests in `css_l4_canonical_parity.rs` `d91f18535bd367c42d0de17ee82ac4d94bed32dfb98ffe604445a13998fb5a1d` | `project_types_css_l4.rs` `56ded7c62f45509283eba036593b75c482003c06626c3f01c923524b5f01a136` accepts broad kinds/floors; **missing** exact 86-layout/field/type and builder-event golden |

Generic regex tests (`regex_fast_path_consistency.rs`
`ec2586155c0bcbb0a15b71462a9b37e6abcf3a77ab5ed8db5038f3571b7c1896`
and `regex_classify.rs`
`416d36157af30b412e1b27aaa7bee2f48506b02ebcc368a05cdea384c40d5f4e`)
prove classification/emit coverage, not helper-versus-previous-engine language and
offset equivalence on these production call sites.

## Required successor closure

1. Treat the 39-file manifest as an envelope only. Re-render the four runtime
   trees in the authorized isolated generator transaction and require the 31
   output hashes above before calling it a candidate.
2. Add exact registry goldens and event-stream/materialized-value concordance:
   JSON `null`; CSS Pretty `atRuleBody`; Sheets nine layouts plus unique
   compare variants and five path rows; CSS L4 all 86 layouts and 140 rows.
3. Add differential scanner tests for every affected production pattern,
   including success length, rejection boundary, cursor restoration, and
   no-throw behavior. Add the three CSS Pretty recovery cases.
4. Exercise Sheets' ignored lazy-path contract or explicitly remove it from
   acceptance; exercise newly emitted VariantName rows and representative CSS
   L4 typed fields through both eager and `parse_with` lanes.
5. Run semantic, recovery/no-throw, path, serialization/prettify, and type
   gates against the complete envelope. Only then rebuild the generator
   against those bytes and require exact nine-pair fixed point.

Until those steps close, all four clusters and the proposed atomic fleet are
**RED**, and profiling, inverse-control formation, generated landing, and W0
acceptance remain barred.
