# PT-0 common harness R6 — author record

MODEL: `gpt-5.6-sol`; REASONING: `ultra`; WORKFLOW: `v2`

Status: **R6 COMMON-HARNESS REPAIR IMPLEMENTED; PARSE-ONLY CAPTURE
GREEN; PT-0 GATE RED; ZERO FULL PT-0 CREDIT.**

This is the smallest cohesive R6 ordered by `R5-ADJUDICATION.md` at
`084d334340e83d89e3eebb14933ad87fa950ff6b3e6c62ef48252a73f9526476`.
It preserves the two-script common harness, the parse-only profile, and every
R5/older evidence record and bundle append-only. It adds no parser, lock,
generated output, sibling production edit, Value read, or Value authority.

## Frozen R6 subject

| Path below `parse-that/docs/tranches/T/prototypes/major/common` | SHA-256 |
|---|---|
| `scripts/hash.mjs` | `50013064ec7aa47549b4d3bbd9900cb687f16eba0ddd8727d44e31caa79e08f0` |
| `scripts/pin.mjs` | `7515b67afd0f129a30c9b518e7057f077c9d67936b91dcf0cb1769f9fa07f00a` |
| `test/scripts/hash.test.mjs` | `5c66e18122e6a9ea5f2beea322c8558c67d27f27445a75c9affaa53787eb1691` |
| `test/scripts/pin.test.mjs` | `afb6d06e4fa725bb1d540817debd8736fca924b9e2c9cd80c61069fad68378d6` |
| `test/scripts/publish-child.mjs` | `050320c4cf2f267e0505ca410df92370090b9899e6d1cbdd4c6e79b453c5ce40` |
| `README.md` | `36be6d6226a0204568069bfea2ea792df8b62d60c2768f1d80d92fbadb52a7c4` |
| `pins.config.json` | `1c693805eda94d5605a49ae9f0af4ee8c48e2d20aa6907659c7989206de83e5e` |
| `profiles/pt0-parse-only.json` | `c2c1a781f2748e262fc08e7598b114801b02f96566f3ec81b10bd5c7f26327b1` |
| `package.json` | `b0406306174b694e3d2c2ec906bc626c623c57694f8c489f239d76bf3bb380fd` |

No parse source/build-input byte changed after the terminal capture. The sole
permitted parse write under the lease was the unique R6 bundle itself.

## Repairs implemented

1. Child commands receive only owned `LANG`, `LC_ALL`, `TZ`, and Darwin
   text-encoding variables. Ambient selectors are not spread. Git redirection
   authorities are rejected; no PATH lookup is evidence authority.
2. `xcode-select` resolves the developer root through `arch -arm64`. The
   resolver and `/usr/bin/git` dispatcher have separately executed, recursively
   closed identities. The selected Xcode version/plist is bound. Every census
   Git call invokes the resolved Xcode Git file directly through `arch -arm64`.
3. rustup is resolved directly under owned `RUSTUP_HOME`; repository
   `rust-toolchain.toml` bytes bind the selected toolchain. Direct rustc and
   Cargo paths, sysroot components/manifests, and versions are recorded.
4. One recursive Mach-O policy records each effective entry and non-system
   dependency with bytes, SHA-256, Apple-ARM-capable slices, dependencies, and
   code-signing identity. The system tail uses arm64/arm64e shared-cache UUIDs.
   Node's `libnode.147.dylib` and rustc's `librustc_driver` are present. Every
   execution selected native Apple ARM; no x86 slice executed.
5. Git uses `GIT_OPTIONAL_LOCKS=0`, disabled system/global config, and bound
   local config. Raw index/config/HEAD bytes and metadata are invariant around
   repository and complete two-census transactions. A stale-stat regression
   proves status inspection leaves them exact.
6. Receipt, run, seal, member, repository, tool, runtime, machine, Rust,
   ledger, and nested bindings have closed key/type schemas. Unknown and
   missing fields fail closed. The seal is compared to its canonical
   constructor.
7. The external cooperative lease spans final census through seal fsync and a
   full post-seal recensus. Capture rechecks the unchanged lease before return.
8. Value is absent from declared operands and no R6 runtime code accesses a
   Value path. No broader ambient-process claim is made.

## Test receipts

The born-RED test bank was written before implementation. Its explicit ARM run
reported 2 failures and 0 passes because `rawGitAuthority` and `validateLease`
did not yet exist.

Final command:

```text
/usr/bin/arch -arm64 /opt/homebrew/bin/node --test \
  docs/tranches/T/prototypes/major/common/test/scripts/*.test.mjs
```

| Replay | Pass | Fail | Skip | Duration |
|---|---:|---:|---:|---:|
| A | 39 | 0 | 0 | `3673.537ms` |
| B | 39 | 0 | 0 | `3783.800041ms` |

Both passes include real SIGKILL receipt/run interruption witnesses, exact
environment and native-tool authority, recursive Node/Rust closure checks,
stale-stat Git invariance, R6-prefix rejection, closed-schema deletion and
unknown-key probes, lease validation, canonical sealing, and no-replace tests.

`node --check` through explicit `/usr/bin/arch -arm64` returned zero for all
five JavaScript files. `git diff --check` returned zero for the R6 subject.

## Lease and terminal bundle

The owner ACK text is bound at
`256303390b8fd8ea568583b161458d182bac3b1708fcf05059c61e98274f6afb`.
The immutable external lease is
`8689e853cd36d9975167dfa5565d1c91f938d98708753d5849ec85e7ae04c313`;
its window was `2026-07-22T17:10:00.000Z` through
`2026-07-22T17:40:00.000Z`. Release occurred after the final matching recensus
at `2026-07-22T17:10:50.000Z`; the release record is
`da6cf5444f875ef7a6d29a632337453c3e0aa448f81c52346e3540f7ac008234`.

Capture command:

```text
/usr/bin/arch -arm64 /opt/homebrew/bin/node scripts/pin.mjs capture \
  --profile pt0-parse-only \
  --bundle runs/pt0-parse-only-r6-20260722T171000Z
```

| Member | Bytes | Mode | SHA-256 |
|---|---:|---:|---|
| `receipt.json` | 195593 | `0600` | `bed5b66cbd861e980f5b01a27b234fce2dcf81298fd3bc48a0ad373a202e7ee1` |
| `run.json` | 841 | `0600` | `777ab051f87541adabc93c3a957299b3ab7f89004407932e45f863baa549567e` |
| `complete.json` | 934 | `0600` | `e5239f7bb076439a565892e560fa5a826b892f650f0dc2e3c20aa1942b1634bb` |

The directory is mode `0700`. Capture's post-seal recensus matched the receipt
at `bed5b66c…`. The explicit ARM verifier passed twice with seal `e5239f7b…`.
The same-path capture replay failed with `EEXIST`, exit 1. A subsequent release
recensus verifier again matched the same seal and receipt.

## Captured parse-that identity

- HEAD: `ef10d5b78236c4a30a7bb22a6113b60bdc4bdf42`
- HEAD tree: `d320357165f3d34ae2cde93872273cc4a33b924a`
- index: 270 entries / 23053 bytes /
  `1d3f112a283dd28b08fb8c05f774fcc6f0540921b253a24a29b13dca7fc6f478`
- dirty status: 181 records / 11992 bytes /
  `b77d73ec0879be6094ab37c8ce8f787f2ce44670c202813fe2209f0feffddc4e`
- dirty content: 214 entries /
  `efeeafe40da3be96329c2906aad46af879f16e907e476b9f11adbe79543655a1`
- lockfiles:
  `38ae11346efce5dea15779f05efc2f227713b2e5942f76137bb3e41feebeb72f`
- raw Git index: 29055 bytes /
  `63a781f72bd64b801a20f769b79fbeb833eac5cd9f4df8f90e0f63d82d22b8d5`

| Operand | Entries | SHA-256 |
|---|---:|---|
| `typescript-production` | 129 | `1497c9ff339640722e468aa9c50ddc7bdf5a2aba0ae25fe259d336ab09f33b57` |
| `rust-production` | 183 | `037c5466bab11f4b2625aba6237f16c2dce1a596da2ec33be7ca13a14ead1657` |
| `tranche-t` | 166 | `0b586dbf14352e719f25cff4f626f2d135c3b5d09d7447abd38279a7865afbb1` |
| `tranche-u` | 13 | `39bee35f64ef51c27def9beaa9eff9081c355b36d0813ce1fef3f68ccd3cade1` |
| `invocation-v4` | 68 | `71545234e9526dce62ac3ac853179b6fa324d1f3cd3798b523594f18d4cf8ae4` |
| `binding-authority` | 42 | `ce7668a1682338ffffebc3127be6ac86b5a010af89fdd30f7e873abffe607dbb` |
| `toolchain-selection` | 2 | `4fec9a1569f12060fc1a23e896f555045c5d2c1691984bb6e1b82d8d7de4f286` |

## Effective runtime identities

| Authority | Entry SHA-256 | Runtime SHA-256 |
|---|---|---|
| Xcode resolver | `46db1db75076c31d7e13960c3640abcf9f960e566304ffba44b1b81f47e58a3e` | `4eb8438640d40b2b17606d65fb8d97eb20327c1c1e3394afe004560c0fd8d280` |
| Git dispatcher | `12bed4523661307059b879b9b54e77a73176e9d27d27a0e40363271d8f0668ba` | `2ad88c86517e141853fc822403194f44c7719dfc3a1ad9c16710366157ab8c52` |
| Effective Xcode Git | `472013f51db0f4bd21b630b791423329e2c56a3d2a410834b04cb7cc072078b0` | `872b6479a9d01062bcd978d7fddcd48ddf50e3ba04877319e319f277b25adcfc` |
| Node | `08dad0581f00a0cabf4d49ec92ca1f25fdfd01c2c18fa8e92b35f04d4c24c164` | `c58646e8a95796220e01b099d020c9f2f02b385581b041f426595bb1424033e3` |
| rustup | `aeb4105778ca1bd3c6b0e75768f581c656633cd51368fa61289b6a71696ac7e1` | `a70479728074ba005604dd2fa33eb31e82623ddc1bd6017efd5b28e6ef06d699` |
| rustc | `95d199b3cc0a2e43a904e3ed6120738ce768b7300214023a56d60cbeacf76d36` | `ebb8e7e4d6a717a1592f03627862d7c300a3ba91f96ef6915bb715a1b7ccf137` |
| Cargo | `c6170b6af6c6205953bff3119c3c003744f5b85e9f76489d12e3e45094f1b296` | `329f3331a920d1dd96ea630e2ea7b570585ff614c02ab624b17b4c0d80c77413` |

Selected Rust toolchain:
`nightly-2026-04-11-aarch64-apple-darwin`; selection hash
`b9c6c4c8609f213d11646792b8f172d31ebb257b5f024aab8f74648b526304cc`;
sysroot/component-ledger hash
`92de5c5a46f9f7ec218e652bc430f756f94a47f1c0fb72b5127e7f10a120957c`.
The captured Apple identity is macOS `26.4.1` build `25E253`, Darwin
`25.4.0`, native untranslated ARM64.

## Historical preservation and remaining RED

The R5 bundle remains byte-identical:

| Member | SHA-256 |
|---|---|
| `receipt.json` | `060580f815d9de5d3527a65a76b3f7a6bb4bdbb141245e5862c09aad6e7095c4` |
| `run.json` | `c4da1ff8a7f84e41fc9a8c5b3171096ee1dcb8d328070a1a37124a513e5812fc` |
| `complete.json` | `9c05f0d4c6ccf5d049ca41a99965bae2d0c3f4724a481cadaced2f1c4683f919` |

R6 begins hostile review at zero clean passes. PT0-ART remains 0. The complete
PT-0 denominator, W1 semantic/coordinate/effect/package manifests, W2 oracle
and born-RED bank, W3's five lock adjudications, T.W0 graphs/baselines, and
U.W0 preseal witnesses remain ordered work. No movement is authorized.
