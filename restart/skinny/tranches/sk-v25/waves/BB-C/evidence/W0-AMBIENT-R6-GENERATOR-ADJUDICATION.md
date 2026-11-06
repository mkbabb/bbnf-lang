MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: fresh third R6 generator adjudicator.

# BB-C.W0 R6 generator/fleet adjudication

- Date: 2026-07-22.
- Posture: assume every author and critic claim is wrong until independently
  reproduced from immutable bytes.
- Authority: adjudication only. This receipt changes no source, generated
  product, sidecar, runtime projection, manifest, lockfile, package, or stage.
- Execution boundary: read-only inspection only. I ran no build, test,
  generator, profile, or mutant.
- Binding predecessor: `W0-AMBIENT-R5-ADJUDICATION.md`, SHA-256
  `df6c714c8f7c0a487b3f019a8c328b144e119fd661ce6f548e863d209a377da1`.

## Subjects read in full

| Subject | SHA-256 | Lines / bytes |
|---|---|---:|
| R6 repair author | `61463059d8873e999d2bc82a260e133ecd50e2ef95c88913e9a0070fff41b39a` | 175 / 9,692 |
| R6 profile design | `c10b4cbcf5efb6099240d75a6f9fb538078bf886cec18f644e689e2be987d66b` | 93 / 4,602 |
| R6 generator author | `93c97aa3f0375f3c2c053ebe5318aa2663184ee3825a221868c65bce0ccf0751` | 170 / 8,162 |
| generator `CENSUS.md` | `6340b1459c5a5b9b2c653375ab576d7eb934ae3288e557dbbb92b85a04c086fa` | 158 / 9,082 |
| generator Critic A | `c000271b73068db2b6e37be3a13914e9e85213317c9cf37d685c7769396fc637` | 424 / 30,430 |
| generator Critic B | `4e9088d37a3cba0e99d165071e5b81efa168ad703efc258798deab997b081fae` | 453 / 30,232 |
| epoch-2 build log | `0fd7d56c4d2f944862b7e6c6ee7f63ddb74035b7a4c90f7dcbdc97c240bc2820` | 1,215 / 45,691 |
| epoch-3 build log | `0ad6b636687ea114ac5a81ee8a87265295a873cbfd244fa329759997c00dcdc7` | 1,215 / 45,691 |
| epoch-2 generation log | `7bd84f1b9a99014cb735d9a24375bec285b1b8f561626bc295751718368a5194` | 11 / 710 |
| epoch-3 generation log | `2ddfc9bd3275c6413cff1b4e59ec058d04d2c041e2d8d34dd4e1851882467e5f` | 11 / 711 |
| fleet generation log | `0fb4dd812d400472633ba8e5a624f8664dc10ed02d1d20ef9f9ad4c1b82c6125` | 134 / 19,164 |
| raw-evidence ledger | `c72b3d04ca04ffaa9835325d1e3e843a97fc8f557c407608fe897707bf87c918` | 6 / 949 |

I also read all 18 checked parser/sidecar artifacts, all 18 sealed fleet
artifacts, the eight mutation patches and their ledgers, the generator,
registry/path-plan, pipeline, scanner/profile, runtime-projection, benchmark,
and timeout owners relevant to the disputed claims.

## Executive verdict

**R6 is RED overall and must not land generated output or begin its profile.**
The narrow BBNF self-host fixed point is correct. The complete fleet is not a
fixed point: five family pairs are exact, four family pairs drift, and no
generator was rebuilt against the proposed 18-file fleet. The hard stop and
zero generated landing were correct.

This is not evidence that the repair is wrong. Much of the broad drift is in
the repair's real grammar-to-IR dependency cone. It is evidence that the
candidate is larger than the predeclared Pratt-only experiment and that its
semantic and runtime-projection consequences are not yet proved.

## ARM build, overlay, and restoration

Every retained build/generation command uses `/usr/bin/arch -arm64`; both
builds target `aarch64-apple-darwin` and exit zero. The retained executables
are independently readable as Mach-O 64-bit `arm64`, and `lipo -archs`
returns only `arm64`:

| Binary | SHA-256 | Bytes |
|---|---|---:|
| epoch 2 | `32ce7e1aa8c97d61d771c7692dd26cccb954cd81257a58b89bd7bbf755d3dbfd` | 45,150,584 |
| epoch 3 | `a6056194eb00ab2007ad684f9294ad44e644bc42f5d03af05be3544576b4d874` | 45,150,584 |

Therefore the bounded recorded route is native ARM: **CORRECT**. The broader
claim that no unrecorded x86 action existed anywhere is **UNPROVEN**.

The compatibility patch is exactly
`56020487aed0701320fc19ab5b6ecd9eeb51dbb460be9d9a087ee644e9622eda`
(57 lines / 3,288 bytes), applies cleanly to current `apple.rs`, and only
replaces const-invalid `Path::new` expressions and the slice
`split_terminator` use. Its streamed result is exactly
`54827145fe52f1cb0391a802031b3a7e137911fb4eab1b5bbfa304ca1c0ad0eb`.
Current restored `apple.rs` is exactly
`ce3ae9af7d93e6066fad2ff7e0ea404405bdff29df64bd803f82fe095067fc5f`;
current `Cargo.lock` is exactly
`480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9`.
Final restoration is **CORRECT**. Restoration before each binary execution is
**UNPROVEN** because the logs do not retain an apply/restore transaction.

Critic B's blanket no-overlay objection is **INCORRECT**. R5 explicitly
authorized an isolated Apple-ARM generator “using only the already frozen
compatibility overlay and restoring it before execution.” The overlay is a
temporary proof-build accommodation, not landed product compatibility. The
author's phrase “source-identical/shim-free build chain” is nevertheless
**INCORRECT**: the executables were compiled from overlaid bytes. Future
receipts must say that exactly. No-shim law still forbids landing the overlay,
an alias, a dual path, or a fallback as product architecture.

## Missing 867-member ledger

The census states this identical pre/post tuple:

```text
members: 867
bytes: 15,297,240
path-set: 5b07b6edcd85671b9df9bec44c0bdaa48fe6975230e8cff79583357c35d5502e
content: 25a6cb85639d1827c9913ed5d25ca25195dc5ff8489a8ba9eb08b28f73d5bcd1
```

No evidence file contains the 867 normalized paths, their sizes and hashes,
or the exact inclusion/exclusion and digest algorithm. The aggregate cannot
be independently recomputed. Named-owner hashes and the final restored bytes
are **CORRECT**; whole-build-input byte identity is **UNPROVEN**. The claim
that `CENSUS.md` itself binds all 867 member identities is **INCORRECT**.

## Fixed point and complete fleet

Epoch-2, epoch-3, and checked BBNF parser bytes are all exactly
`c8f5e6d3dc6b050c508fb1e24a08d698caf5a9b276317e8c1df8b21270efcbe2`;
their registry bytes are all exactly
`3e7494c6664a9352e3512c691bae8bd8a446e1d9e17c88b51a2e36ae7b788021`.
The bounded BBNF parser/sidecar self-host fixed point is **CORRECT**.

The sealed fleet contains exactly nine `.rs` parsers and nine matching
`.registry.json` sidecars. Independent pair comparison reproduces:

| Family | Parser | Sidecar | Pair disposition |
|---|---|---|---|
| BBNF | exact | exact | exact |
| JSON | drift | drift | RED |
| CSS L4 | drift | drift | RED |
| CSS Pretty | drift | drift | RED |
| Google Sheets | drift | drift | RED |
| EBNF | exact | exact | exact |
| BNF | exact | exact | exact |
| CSV | exact | exact | exact |
| Math | exact | exact | exact |

Thus 10 of 18 artifacts are exact and 8 drift. No xtask was rebuilt against
all 18 fleet bytes, so “complete-fleet fixed point” is **INCORRECT**.

The full-directory diff SHA `841cca95…` is not a pure digest of the 18
artifacts because the checked-only `mod.rs` deletion is also present.
Per-file hashes bind the actual 18-file comparison.

## Parser/sidecar isomorphism

I independently normalized every Rust parser's embedded `StructLayout`,
recursive `TypeDesc`, `FieldSource`, and `REGISTRY_ENTRY_RULE` constructors
against its JSON sidecar. Results are identical in both trees:

| Family | Layouts per tree | Checked | Fleet |
|---|---:|---|---|
| BBNF | 53 | 53/53 | 53/53 |
| JSON | 8 | 8/8 | 8/8 |
| CSS L4 | 188 | 188/188 | 188/188 |
| CSS Pretty | 15 | 15/15 | 15/15 |
| Google Sheets | 37 | 37/37 | 37/37 |
| EBNF | 13 | 13/13 | 13/13 |
| BNF | 5 | 5/5 | 5/5 |
| CSV | 4 | 4/4 | 4/4 |
| Math | 1 | 1/1 | 1/1 |
| **Total** | **324** | **324/324** | **324/324** |

All entry rules also match. Internal parser/sidecar isomorphism is
**CORRECT**. It proves coherent projection, not semantic correctness.

## Direct Pratt consequence

Function-body extraction reproduces exactly one
`record_compound_bounds_start` and one `record_compound_bounds_end` in:

- checked and fleet BBNF: `value_path`, `value_mul`, `value_or`, `value_add`,
  `value_cmp`, `value_and`, `binary_factor` — 7/7;
- fleet-only Google Sheets: `comparison_expr`, `mul_expr`, `array_row`,
  `array_rows`, `concat_expr`, `add_expr`, `exp_expr` — 7/7;
- fleet-only CSS L4: `mathExpr`, `mathProduct` — 2/2.

The direct generic Pratt consequence is **CORRECT**. The claim that this is
the whole fleet diff is **INCORRECT**.

## Causal classification of residual drift

| Class | Adjudication |
|---|---|
| repaired mapped-factor/type projection | **CORRECT dependency class.** JSON `null -> 0u8`, Sheets mapped errors/operators, and many CSS typed maps place changed projected types inside the repair's direct grammar/lowerer cone. Calling all non-Pratt drift “unrelated” is incorrect. |
| direct Pratt span hooks | **CORRECT and exact** at 7/7/2. |
| current direct-IR/egraph posture | **Plausible but exclusive causation UNPROVEN.** Current pipeline omits the old ambient saturate/writeback route, and CSS Pretty's combined-to-split regex shape is consistent with current direct IR, but no pass-fingerprint or counterfactual receipt assigns every hunk. |
| scanner/profile selection | **Immediate ownership partly CORRECT; completeness UNPROVEN.** `scanner_plan` and structural/regex profiles own the emitted strict scanners and quote classes. No proof attributes every JSON/CSS delta or proves its semantics. |
| registry/path plan | **Mechanical downstream projection CORRECT.** They derive from the projected registry; correctness remains conditional on upstream IR/type correctness. |
| StringId/layout renumbering and remaining CSS/Sheets shapes | **UNPROVEN.** They require semantic and projection-consumer validation. |

There is no safe non-empty generated landing subset. Filtering only Pratt
hunks would hand-edit generated products; copying parsers without sidecars
would split authority; copying all eight drifting artifacts would cross an
unproved runtime-projection cone; and the five exact pairs need no landing.
The safe subset is exactly **none**.

## Runtime-projection cone

The sidecars are not inert receipts. `xtask/src/regen_simple_runtime.rs`
consumes current grammar registry/rule identities for the simple, typed
formula, typed BBNF, and typed JSON runtime trees; `xtask/src/regen_css.rs`
does so for `crates/core/src/runtime/css_l4`. The four changed sidecars
therefore widen the atomic candidate to their declared projection sources,
generated runtime outputs, builders/documents/views/path plans, and existing
semantic consumers. No such regenerated runtime candidate or downstream
semantic proof was produced. Landing the eight files alone is **INCORRECT**.

## Profile design

The proposed ARM `ay-final` comparison, isolated targets, alternating fresh
process order, 20 matched pairs, deterministic 100,000-resample bootstrap,
row-level reporting, and exact inverse of only the 7/7/2 calls are reasonable.
The design remains born RED for four independent reasons:

1. It has no model/effort/role header. The claim that model-truth metadata is
   present is **INCORRECT**.
2. Sheets `simple.txt`, `nested.txt`, and `stress.txt` are exactly
   `120aead6…` (505 bytes), `4d0ab1…` (1,456 bytes), and `d5951066…`
   (1,838 bytes); each has zero `{` and `}` bytes. `array_row` and
   `array_rows` are not reached. Seven-of-seven Sheets exposure is
   **INCORRECT**; per-function reachability for the other hooks is
   **UNPROVEN**.
3. `bench_with_timeout` measures after `body` returns. It cannot interrupt an
   infinite body; Divan `max_time` is not an external hung-process watchdog.
   The claimed enforceable timeout is **INCORRECT**.
4. There is no accepted atomic fleet/runtime candidate and hence no valid
   inverse preimage. Forming `PROFILE-CONTROL.patch` now remains barred.

With 20 pairs and a hard upper ratio of 1.0, the inference rule is a valid
conservative non-regression gate. An interval spanning 1.0 is inconclusive
and may be RED for closure, but it is not evidence of a measured regression.
No favorable family aggregate may hide a hard-row regression or uncertainty.

## Mutants

Exactly eight one-hunk specimens exist: M01-M05 and M07-M09; M06 is absent.
Their exact manifest is
`30129d456bd4912ccd558b64fd38dcd06ce06afe8723ce61fb7ac120aca6bcc2`.
All statically apply to the final owners, and their scopes cover RHS-boundary,
transaction/cancellation, current-token, solution-shape, and foreign-`VarId`
faults. R5 expressly required this repaired formation, so calling it wholly
irrelevant is **INCORRECT**. No mutant was executed. Kill credit is exactly
**zero**, and no mutation claim is closed.

## Claim ledger

| Claim | Verdict |
|---|---|
| recorded generator route and retained binaries are Apple ARM only | **CORRECT** |
| no x86 action existed anywhere | **UNPROVEN** |
| final `apple.rs` and `Cargo.lock` were restored | **CORRECT** |
| restoration happened before each executable ran | **UNPROVEN** |
| compatibility overlay violated binding R5 authority | **INCORRECT** |
| build chain was literally source-identical/shim-free | **INCORRECT** |
| the 867-member whole-input identity is reproducible | **UNPROVEN** |
| bounded BBNF parser/sidecar fixed point closed | **CORRECT** |
| complete 18-file fleet fixed point closed | **INCORRECT** |
| sealed output has exactly nine parsers and nine sidecars | **CORRECT** |
| checked and fleet parser/sidecar pairs are internally isomorphic | **CORRECT** |
| direct Pratt consequence is exactly BBNF 7, Sheets 7, CSS 2 | **CORRECT** |
| all remaining drift is unrelated to the repair | **INCORRECT** |
| all remaining drift is thereby safe and semantically correct | **UNPROVEN** |
| a Pratt-only generated subset may be landed | **INCORRECT** |
| author correctly landed no generated product | **CORRECT** |
| profile metadata, reachability, and watchdog are complete | **INCORRECT** |
| eight mutants were validly formed | **CORRECT** |
| any mutant was killed | **INCORRECT** |
| R6 closes W0 item 3 or the ambient campaign | **INCORRECT** |

## Binding disposition and successor

1. Preserve the four final repair owners at their R6 identities. Preserve the
   sealed fleet as RED evidence only. Land no parser, sidecar, runtime output,
   profile control, overlay, package, lockfile, or stage from this receipt.
2. Start **R7 atomic fleet reconciliation**, not a Pratt-only refresh. Before
   any execution, freeze and retain the exact normalized path/size/SHA-256
   ledger and census algorithm for every build input and every candidate
   parser/sidecar/runtime projection byte.
3. The already-authorized compatibility overlay may be reused only as the
   exact temporary proof-build accommodation. Retain explicit apply, applied
   hash, build, restore, restored hash, and pre-execution verification events.
   Never describe this as a no-overlay build and never land it as a shim.
4. Treat JSON, CSS Pretty, Sheets, and CSS L4 parser+sidecar drift as four
   causal/semantic clusters. For each, bind changed grammar maps, lowerer and
   pass decisions, registry/path-plan deltas, scanner/profile decisions, and
   runtime-projection consumers. Two hostile critics and a fresh adjudicator
   must decide every unexplained class before acceptance.
5. Form one atomic candidate containing every accepted parser, sidecar, and
   affected runtime projection. Run its semantic, no-throw/recovery, path,
   serialization, and typed-value proofs. Then rebuild the generator against
   that complete candidate and regenerate all nine pairs again; only exact
   18-file equality closes the fleet fixed point.
6. Supersede the profile design before profiling: add model-truth metadata,
   an array-bearing Sheets hard row, an untimed per-hook reachability proof,
   and an external process watchdog. Only after an accepted atomic candidate
   exists may its exact 7/7/2 inverse control be formed and measured.
7. Execute the eight mutants one at a time only after the semantic candidate
   is sealed. Retain per-mutant apply, expected failing assertion, exit,
   restoration, and post-restoration hash. Any survivor or contaminated
   restoration is RED.

Until all seven steps close, R6 remains **RED**, `PROFILE-CONTROL.patch`
remains forbidden, and W0 receives no item-3, ambient, lock, tranche,
prototype, or performance acceptance credit.
