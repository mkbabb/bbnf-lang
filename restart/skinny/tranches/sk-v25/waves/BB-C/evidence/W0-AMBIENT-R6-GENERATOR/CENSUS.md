MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: R6 generator author.

# BB-C.W0 R6 generator fixed-point and fleet census

- Date: 2026-07-22
- Binding adjudication: `W0-AMBIENT-R5-ADJUDICATION.md`, SHA-256
  `df6c714c8f7c0a487b3f019a8c328b144e119fd661ce6f548e863d209a377da1`.
- Architecture: Apple ARM only. Host `uname -m` was `arm64`; both retained
  generator executables are Mach-O 64-bit `arm64`, and every execution used
  `/usr/bin/arch -arm64`.
- Formal posture: zero-credit author evidence. Fixed-point generation is
  GREEN; complete checked-fleet closure is RED because four grammar families
  contain drift outside the adjudicated 7/7/2 Pratt dependency cone.
- No checked generated output was changed.

## Exact source census

The twice-identical R6 repair census immediately before the first build was:

| Governed path | SHA-256 |
|---|---|
| `crates/core/src/lower/expression/wrap.rs` | `7edbb5bca06a63304615afb4b0560f73a0bb3d6df3439b7aa8c633f6cd323f17` |
| `crates/core/tests/lower/expression/wrap.rs` | `31ec57a3bc469d22f4d3ba805a12505365b2aa167ba01ebb74722b817967426c` |
| `crates/ir/src/passes/csp_strategy/solve.rs` | `aac939e7a68261d50dcbcf4961c638a206e71d780d95edf26dd71e1017ccf08e` |
| `crates/ir/tests/passes/csp_strategy/mod.rs` | `1e32d3b4ba2aae1db58cb2ee81db236c17357bb048caf256c3080006da27c9e3` |
| Pratt struct-direct emitter | `bfc2dff4727d11dcf62bbb3f423cd0c0f3f52e7c888c9e15112cdf6d43f288d2` |
| `Cargo.lock` | `480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9` |
| restored `xtask/src/architecture/evidence/binding/apple.rs` | `ce3ae9af7d93e6066fad2ff7e0ea404405bdff29df64bd803f82fe095067fc5f` |

The build-input census contains root Cargo/config inputs; every file under
workspace crate and xtask `src/` trees plus their Cargo manifests; every
grammar source; and the sibling parse-that regex/runtime and pprint Rust
source/manifests selected by the workspace patches. Evidence files under
`restart/` and test-only R6 receipt motion are excluded. The exact pre- and
post-campaign tuples are byte-identical:

```text
members: 867
bytes: 15,297,240
path-set SHA-256: 5b07b6edcd85671b9df9bec44c0bdaa48fe6975230e8cff79583357c35d5502e
content-ledger SHA-256: 25a6cb85639d1827c9913ed5d25ca25195dc5ff8489a8ba9eb08b28f73d5bcd1
```

The frozen compatibility patch remains the R5 artifact at SHA-256
`56020487aed0701320fc19ab5b6ecd9eeb51dbb460be9d9a087ee644e9622eda`.
It produced the exact temporary `apple.rs` SHA-256
`54827145fe52f1cb0391a802031b3a7e137911fb4eab1b5bbfa304ca1c0ad0eb`
for each build. The source was restored and verified at `ce3ae9af…` before
either built binary executed. The final source and `Cargo.lock` hashes remain
the restored values above.

## Isolated rebuild fixed point

| Subject | Identity/result |
|---|---|
| epoch-2 target | `/tmp/bbcw0-r6-xtask-epoch2.MZVMIY` |
| epoch-2 ARM binary | `32ce7e1aa8c97d61d771c7692dd26cccb954cd81257a58b89bd7bbf755d3dbfd` |
| epoch-2 output | `/tmp/bbcw0-r6-output-epoch2.jBv2AI` |
| epoch-2 BBNF parser | `c8f5e6d3dc6b050c508fb1e24a08d698caf5a9b276317e8c1df8b21270efcbe2` |
| epoch-2 BBNF registry | `3e7494c6664a9352e3512c691bae8bd8a446e1d9e17c88b51a2e36ae7b788021` |
| epoch-3 target | `/tmp/bbcw0-r6-xtask-epoch3.sBMQ7V` |
| epoch-3 ARM binary | `a6056194eb00ab2007ad684f9294ad44e644bc42f5d03af05be3544576b4d874` |
| epoch-3 output | `/tmp/bbcw0-r6-output-epoch3.YNiqt3` |
| epoch-3 BBNF parser | `c8f5e6d3dc6b050c508fb1e24a08d698caf5a9b276317e8c1df8b21270efcbe2` |
| epoch-3 BBNF registry | `3e7494c6664a9352e3512c691bae8bd8a446e1d9e17c88b51a2e36ae7b788021` |

Epoch 2 equals the checked parser/registry. The second isolated xtask was
built against those checked epoch-2-equivalent bytes. Epoch 3 equals epoch 2
and the checked parser/registry byte-for-byte. Both build commands used
`CARGO_BUILD_JOBS=1`, `--target aarch64-apple-darwin`, `--profile ax-iter`,
`--features grammar-regen`, `--bin xtask`, and `--locked`.

## Complete isolated nine-entry manifest

The fixed-point epoch-3 binary ran the non-check, no-grammar-filter output
route once into `/tmp/bbcw0-r6-output-fleet.TwgDag`. `P/R` means parser and
registry. Counts are physical lines/bytes.

| Family | Checked P SHA | Fleet P SHA | Fleet P lines/bytes | Checked R SHA | Fleet R SHA | Fleet R lines/bytes | Equality |
|---|---|---|---:|---|---|---:|---|
| BBNF | `c8f5e6d3…` | `c8f5e6d3…` | 21,211 / 1,062,265 | `3e7494c6…` | `3e7494c6…` | 2,232 / 49,354 | exact |
| JSON | `82e49add…` | `36da388f…` | 2,674 / 119,642 | `8a9500a9…` | `33229d74…` | 205 / 4,407 | **DRIFT** |
| CSS L4 | `ac08b34d…` | `d98882ad…` | 107,432 / 5,983,578 | `77e0ecad…` | `5477673e…` | 9,711 / 218,620 | **DRIFT** |
| CSS Pretty | `e44d2f8c…` | `0ef4b9e3…` | 6,438 / 305,493 | `bc50b5f3…` | `2b93434e…` | 545 / 11,944 | **DRIFT** |
| Google Sheets | `345397a5…` | `e200eb98…` | 13,800 / 674,254 | `37a2ee49…` | `f916a1ee…` | 1,260 / 27,312 | **DRIFT** |
| EBNF | `eddd1aa5…` | `eddd1aa5…` | 7,474 / 349,043 | `7938d084…` | `7938d084…` | 1,261 / 28,458 | exact |
| BNF | `14bf5255…` | `14bf5255…` | 2,979 / 144,917 | `00c633bd…` | `00c633bd…` | 177 / 3,808 | exact |
| CSV | `7afa902d…` | `7afa902d…` | 1,517 / 68,977 | `c22b9d5c…` | `c22b9d5c…` | 102 / 2,084 | exact |
| Math | `e084b2c1…` | `e084b2c1…` | 690 / 28,353 | `931c5a94…` | `931c5a94…` | 19 / 359 | exact |

Full fleet identities for the four drifting families are:

```text
JSON parser      36da388fe4c1042a2fd3ff7812e9741ffb54025261ef7c56333673f8344bc95f
JSON registry    33229d745e334610cfa94f0bf3d997a1eee049ea4b27cb8378ec9166e954b89f
CSS L4 parser    d98882ad48baad4a3898091aa53c903399929919804c61fad61fe636e10b40fe
CSS L4 registry  5477673e1abb12104488cb941ec84446a419dc04f2b9866cb13bfe41bbb54617
CSS Pretty       0ef4b9e3153d52d9f3eeba9e0780ba57ce193c90b6adfa0f4e68566a6d698288
CSS Pretty reg   2b93434e5acd54000c1cb9d235df5b7ef5778de7bb7682a790c86b7b0e64fc7a
Sheets parser    e200eb9867fadd1ced8148144de5af45cb0357a2e61c2b0829c7ac0a35732b84
Sheets registry  f916a1ee9fe5a34493b6ac1c6ed7ed093ac013ce5c39a1079979d703d9927451
```

## Diff census and contradiction

`git diff --no-index --numstat`, checked to isolated output:

| Family | Parser + / - | Registry + / - | Checked Pratt/start/end | Fleet Pratt/start/end |
|---|---:|---:|---:|---:|
| BBNF | 0 / 0 | 0 / 0 | 7 / 31 / 31 | 7 / 31 / 31 |
| JSON | 9 / 655 | 4 / 4 | 0 / 1 / 1 | 0 / 1 / 1 |
| CSS L4 | 5,610 / 6,495 | 1,491 / 522 | 2 / 103 / 103 | 2 / 105 / 105 |
| CSS Pretty | 626 / 3,055 | 1 / 1 | 0 / 12 / 12 | 0 / 12 / 12 |
| Google Sheets | 168 / 149 | 40 / 30 | 7 / 14 / 14 | 7 / 21 / 21 |
| EBNF / BNF / CSV / Math | 0 / 0 | 0 / 0 | unchanged | unchanged |

The expected direct Pratt additions are visible exactly as +7 start/end calls
in Sheets and +2 start/end calls in CSS L4. They are not the full diff. JSON
changes projected types (`Span` to `U8`) and replaces large inline scanner
emission with strict scanner intrinsics. CSS Pretty splits/renumbers regex/DFA
owners and deletes thousands of generated lines. Sheets changes registry
layout/type authority in addition to the seven Pratt calls. CSS L4 changes the
structural quote-class table, path-plan cardinality, registry layout/type
authority, and thousands of non-Pratt lines. These are generator-derived but
outside this lane's authorized and expected Pratt dependency cone.

Exact diff-stream SHA-256 identities (the identities bind the literal checked
and temporary paths used by `git diff --no-index`) are:

```text
JSON parser      e2e1b920ae9faf3088e2f6fa642ac56c96a7f7b4bdf4d0e74f9d0107869dee1e
JSON registry    72646c9646be5cfcfd1b62045f2cf3bf3271923e9695478b6d3c7a39a81cd4c5
CSS L4 parser    b733396a2b326422c134bc7ae9679ca11ddad95aa684f63710046cbc944dc0eb
CSS L4 registry  a2282eb5d380eebc79a46b39de25eed5bb342fda5d0740531f3a090e69155381
CSS Pretty       12392205cf328c171d59646a2eee0c55bb63b07b1915db7931d0c350a45eb80c
CSS Pretty reg   50cf37942985078e44da1843212ee1aa125d0cac24ed598f51e6b02a092bc8bc
Sheets parser    97be5406013d714f4ccfdf38ece4ca8b366001893b149d47f3f37073bb2b7385
Sheets registry  77c25824f4059cd0198eddd0edfe58bba0b8d7468a7c9998a7816b9bfba65ba5
full directory   841cca95fa61b179f57b86b0c2c5cfa55ffc45b4e4e99b09f4c2ca4997a21ec5
```

## Stop disposition

The binding instruction says to stop on any unrelated drift. Therefore:

- no checked parser or registry was refreshed;
- BBNF remains exact and unchanged;
- no Google Sheets or CSS L4 byte was copied from the isolated tree;
- the selective-check CLI defect was not touched;
- no semantic test, benchmark, package, lockfile, Value, stage, or commit
  action occurred; and
- no `PROFILE-CONTROL.patch` was formed, because there is no sealed post-R6
  checked fleet candidate against which an exact inverse can be bound.

The successful epoch fixed point may be retained as narrow evidence. Fleet
closure remains RED and must be reconciled under fresh authority that owns the
non-Pratt JSON/CSS Pretty/type/regex/path-plan drift; this lane may not silently
drop or land those changes.
