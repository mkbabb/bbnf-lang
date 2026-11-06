MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: BB-C.W0 R6 generator author.

# BB-C.W0 R6 generator fixed-point/fleet author receipt

- Date: `2026-07-22`
- Governing adjudication: `W0-AMBIENT-R5-ADJUDICATION.md`, SHA-256
  `df6c714c8f7c0a487b3f019a8c328b144e119fd661ce6f548e863d209a377da1`.
- Authority: only the isolated Apple-ARM generator fixed point, complete
  nine-entry generated fleet assay, and its evidence. The Pratt emitter was a
  frozen input. The only permitted production candidates were the checked
  BBNF, Google Sheets, and CSS L4 parser/registry pairs if the full isolated
  diff proved to be the exact expected dependency cone.
- Formal posture: **zero-credit author receipt**.
- Terminal result: **fixed point GREEN; checked-fleet closure RED; no landing**.

## Model-truth and machine gates

This lane explicitly declares `gpt-5.6-sol`, effort `ultra`; no inherited or
recorded model label was accepted as authority. Every build and generator
execution was explicitly launched through `/usr/bin/arch -arm64`; the host
reported `arm64`. Both retained executables report Mach-O 64-bit `arm64` via
`file` and `arm64` via `lipo -archs`. No x86 command or artifact was used.

The exact R6 repair subject was stable across a twice-run census before the
first build:

```text
wrap.rs       7edbb5bca06a63304615afb4b0560f73a0bb3d6df3439b7aa8c633f6cd323f17
wrap test     31ec57a3bc469d22f4d3ba805a12505365b2aa167ba01ebb74722b817967426c
solve.rs      aac939e7a68261d50dcbcf4961c638a206e71d780d95edf26dd71e1017ccf08e
solve test    1e32d3b4ba2aae1db58cb2ee81db236c17357bb048caf256c3080006da27c9e3
Pratt emitter bfc2dff4727d11dcf62bbb3f423cd0c0f3f52e7c888c9e15112cdf6d43f288d2
Cargo.lock    480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9
```

The exact 867-member, 15,297,240-byte build-input census is identical before
and after the campaign: path-set `5b07b6ed…`, content ledger `25a6cb85…`.
`CENSUS.md` defines its membership and binds the full identities.

## Temporary compatibility transaction

Both isolated builds used only the frozen R5 compatibility patch:

```text
patch SHA-256:    56020487aed0701320fc19ab5b6ecd9eeb51dbb460be9d9a087ee644e9622eda
temporary source: 54827145fe52f1cb0391a802031b3a7e137911fb4eab1b5bbfa304ca1c0ad0eb
restored source:  ce3ae9af7d93e6066fad2ff7e0ea404405bdff29df64bd803f82fe095067fc5f
```

The overlay repairs only the current-toolchain const-`Path::new` and byte-slice
`split_terminator` compilation seams already frozen by R5. It was restored and
rehash-verified before either built binary executed. Final `apple.rs` and
`Cargo.lock` identities are the restored preimages. No compatibility byte
landed.

## Rebuild fixed point

Epoch 2:

```text
/usr/bin/arch -arm64 env CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/tmp/bbcw0-r6-xtask-epoch2.MZVMIY \
  cargo build --target aarch64-apple-darwin -p xtask --profile ax-iter \
  --features grammar-regen --bin xtask --locked

/usr/bin/arch -arm64 \
  /tmp/bbcw0-r6-xtask-epoch2.MZVMIY/aarch64-apple-darwin/ax-iter/xtask \
  regen --grammar bbnf --output /tmp/bbcw0-r6-output-epoch2.jBv2AI
```

The binary is `32ce7e1a…`. Its BBNF parser/registry are `c8f5e6d3…` and
`3e7494c6…`, byte-identical to the checked R5 candidate.

Epoch 3 repeated the build in the distinct isolated target
`/tmp/bbcw0-r6-xtask-epoch3.sBMQ7V` against those checked
epoch-2-equivalent bytes, then generated into
`/tmp/bbcw0-r6-output-epoch3.YNiqt3`. The second binary is `a6056194…`.
Its BBNF parser and registry are again `c8f5e6d3…` and `3e7494c6…`.

Therefore epoch 2 equals epoch 3 and both equal the checked BBNF parser and
registry. This closes the specific R5 rebuild-and-regenerate fixed-point
defect. It does not by itself close the full generated fleet.

Both build logs contain the same inherited warning surface: 185 `warning:`
records, including Cargo's `bbnf` summary of 182 warnings and xtask's summary
of one warning. This lane did not claim or attempt warning-bank cleanup.

## Full fleet assay and hard stop

The fixed-point epoch-3 binary executed the non-check, no-filter route once:

```text
/usr/bin/arch -arm64 \
  /tmp/bbcw0-r6-xtask-epoch3.sBMQ7V/aarch64-apple-darwin/ax-iter/xtask \
  regen --output /tmp/bbcw0-r6-output-fleet.TwgDag
```

All nine parser/sidecar pairs were produced. BBNF, EBNF, BNF, CSV, and Math
are byte-identical to checked output. Google Sheets contains the expected +7
start/+7 end calls, and CSS L4 contains the expected +2/+2 calls. The complete
diff is not confined to those calls:

| Family | Parser diff + / - | Registry diff + / - | Disposition |
|---|---:|---:|---|
| BBNF | 0 / 0 | 0 / 0 | exact |
| JSON | 9 / 655 | 4 / 4 | unrelated drift |
| CSS L4 | 5,610 / 6,495 | 1,491 / 522 | expected 2/2 buried in unrelated drift |
| CSS Pretty | 626 / 3,055 | 1 / 1 | unrelated drift |
| Google Sheets | 168 / 149 | 40 / 30 | expected 7/7 buried in unrelated drift |
| EBNF / BNF / CSV / Math | 0 / 0 | 0 / 0 | exact |

The unexpected output includes non-Pratt type/registry projection changes,
strict scanner substitution, regex/DFA splitting and renumbering, CSS
structural quote classes, and path-plan expansion. It is generator-derived,
but it is neither the expected direct Pratt cone nor authorized for silent
landing by this lane. The binding adjudication required an immediate stop on
such drift.

Consequently no checked generated parser or sidecar was changed. In
particular, the isolated Google Sheets and CSS L4 bytes were not copied, and
JSON/CSS Pretty were not touched. The full 18-file isolated census, physical
counts, exact SHA-256 values, diff-stream hashes, and semantic diff examples
are frozen in `W0-AMBIENT-R6-GENERATOR/CENSUS.md`.

## Profile control disposition

Root requested an inverse `PROFILE-CONTROL.patch` only after a post-R6 checked
fleet candidate was sealed. This fleet failed before landing, so no such
candidate exists. Forming an “inverse” against unlanded or partially selected
bytes would violate the exact-preimage requirement and could mask the
unrelated drift. Therefore `PROFILE-CONTROL.patch` is correctly **UNFORMED /
BARRED**, not missing execution work. It can be created only after a successor
reconciles and seals the complete generated fleet under authority that owns
the non-Pratt changes.

No benchmark or broad semantic test was run; root owns those gates after a
valid candidate exists.

## Evidence identities

| Artifact | SHA-256 | Lines / bytes |
|---|---|---:|
| `BUILD-EPOCH2.log` | `0fd7d56c4d2f944862b7e6c6ee7f63ddb74035b7a4c90f7dcbdc97c240bc2820` | 1,215 / 45,691 |
| `BUILD-EPOCH3.log` | `0ad6b636687ea114ac5a81ee8a87265295a873cbfd244fa329759997c00dcdc7` | 1,215 / 45,691 |
| `GENERATE-EPOCH2.log` | `7bd84f1b9a99014cb735d9a24375bec285b1b8f561626bc295751718368a5194` | 11 / 710 |
| `GENERATE-EPOCH3.log` | `2ddfc9bd3275c6413cff1b4e59ec058d04d2c041e2d8d34dd4e1851882467e5f` | 11 / 711 |
| `GENERATE-FLEET.log` | `0fb4dd812d400472633ba8e5a624f8664dc10ed02d1d20ef9f9ad4c1b82c6125` | 134 / 19,164 |
| `CENSUS.md` | `6340b1459c5a5b9b2c653375ab576d7eb934ae3288e557dbbb92b85a04c086fa` | 158 / 9,082 |
| `EVIDENCE.sha256` | `c72b3d04ca04ffaa9835325d1e3e843a97fc8f557c407608fe897707bf87c918` | 6 / 949 |

`shasum -a 256 -c EVIDENCE.sha256` reports six of six `OK`.

## Exact disposition and remainder

| Question | Result |
|---|---|
| Current Apple-ARM generator builds twice | GREEN |
| BBNF epoch 2 == epoch 3 | GREEN |
| BBNF epochs == checked parser/registry | GREEN |
| Complete nine-family isolated generation | GREEN as execution |
| Complete fleet differs only by expected 7/7/2 calls | **FALSE / RED** |
| Checked generated landing | **NONE** |
| PROFILE-CONTROL specimen | **BARRED; no sealed candidate** |
| Package/lock/Value/stage/commit actions | **NONE** |
| Formal item/wave/lock/tranche/performance/movement credit | **0 / 0 / 0 / 0 / 0 / 0** |

The next technically correct action is hostile drift adjudication, followed by
a separately authorized fleet-reconciliation successor if the non-Pratt
generator output is accepted. This author may not reinterpret unexpected
type/regex/path-plan output as the adjudicated Pratt refresh.
