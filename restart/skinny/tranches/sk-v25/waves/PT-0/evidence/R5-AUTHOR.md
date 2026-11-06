# PT-0 common harness R5 — author record

MODEL: `gpt-5.6-sol`; REASONING: `ultra`; WORKFLOW: `v2`

Status: **R5 COMMON-HARNESS REPAIR IMPLEMENTED; PARSE-ONLY CAPTURE
GREEN; PT-0 GATE RED; ZERO FULL PT-0 CREDIT.**

This record implements only the smallest R5 ordered by
`R4-ADJUDICATION.md` at
`6a2c1444ed327309997c043050bb7c3c90a18b9d64b2ccdec3b3ef89f09440a6`.
It is outside the captured parse-that repository. No parse-that byte was written
after the successful terminal capture. No Value repository path was accessed or
mutated. The campaign remains active; this record grants no product, package,
performance, wave, movement, or full PT-0 credit.

## Frozen R5 subject

| Path below `parse-that/docs/tranches/T/prototypes/major/common` | SHA-256 |
|---|---|
| `scripts/hash.mjs` | `6c76d1a0bfc4efdabb98332f169d0ab4a028738248b1e42f81f0b6292fb2f037` |
| `scripts/pin.mjs` | `1a9aa4f5f3b014f5dbb8c390872dc89176a3b3a1b9b248c82a2bc1491552b926` |
| `test/scripts/hash.test.mjs` | `812037a685896c3d1846627337bdc2d46e63e46c7b33d3b3871b1a2c83bec7f7` |
| `test/scripts/pin.test.mjs` | `5235bffdb1e3d24f060fc5f47dcd55fdba7ff17b49ec60bedf4846e23c94ce6a` |
| `test/scripts/publish-child.mjs` | `050320c4cf2f267e0505ca410df92370090b9899e6d1cbdd4c6e79b453c5ce40` |
| `profiles/pt0-parse-only.json` | `c2c1a781f2748e262fc08e7598b114801b02f96566f3ec81b10bd5c7f26327b1` |
| `pins.config.json` | `1c693805eda94d5605a49ae9f0af4ee8c48e2d20aa6907659c7989206de83e5e` |
| `README.md` | `7cea8b683e8ed83190ab0d1597089b47b6e3014f371a4c86d3c94f8377d10c88` |
| `package.json` | `b0406306174b694e3d2c2ec906bc626c623c57694f8c489f239d76bf3bb380fd` |

No production source, repository configuration, or lockfile outside this
private common harness changed.

## Adjudicated repairs

- The success run record contains no duplicated `census`; its sole content
  binding is `receipt_sha256`, with independent config/profile hashes.
- Git is resolved once per Node process. Every harness Git census and version
  command invokes that exact payload through the exact
  `/usr/bin/arch -arm64` launcher. Tests use the same route.
- The receipt binds the launcher and Git payload hashes, `process_arch=arm64`,
  `selected_arch=arm64`, and `translated=false`. Node reported
  `darwin/arm64`; `/usr/sbin/sysctl -n sysctl.proc_translated` returned `0`.
- Configuration, profile, and package-manifest inputs share one guard requiring
  a contained regular file with no symlink in any path component.
- The declared profile contains only parse-that. The R5 runtime code contains
  no Value operand or path access. This is deliberately not a universal claim
  about ambient child-process behavior.
- Two real child processes were killed with `SIGKILL`, respectively after
  durable receipt publication and after durable run publication. The first
  directory contained only `receipt.json`; the second contained only
  `receipt.json` and `run.json`; both were rejected for a missing completion
  seal.
- R5 claims process-interruption fail-closure only. It makes no claim of
  power-loss durability or resistance to malicious same-user filesystem races.
- Parse evidence was not excluded from the tranche/source census. This external
  author record therefore does not invalidate the terminal parse receipt.

## Test and syntax receipts

The final suite command was `npm test`, which expands to
`node --test test/scripts/*.test.mjs`.

| Replay | Pass | Fail | Skip | Total duration |
|---|---:|---:|---:|---:|
| A | 29 | 0 | 0 | `1365.169542ms` |
| B | 29 | 0 | 0 | `1428.353208ms` |

Both replays included the two independent SIGKILL witnesses, exact Apple-ARM
Git facts, contained-input rejections, duplicate-census rejection, pair/seal
cross-hashes, no-replace behavior, and all retained ledger tests.

`node --check` returned zero for all five JavaScript files:

- `scripts/hash.mjs`
- `scripts/pin.mjs`
- `test/scripts/hash.test.mjs`
- `test/scripts/pin.test.mjs`
- `test/scripts/publish-child.mjs`

## Terminal bundle

Capture command:

```text
node scripts/pin.mjs capture --profile pt0-parse-only \
  --bundle runs/pt0-parse-only-r5-20260722T162040Z
```

The bundle completed at `2026-07-22T16:21:03.288Z`.

| Member | Bytes | Mode | SHA-256 |
|---|---:|---:|---|
| `receipt.json` | 165488 | `0600` | `060580f815d9de5d3527a65a76b3f7a6bb4bdbb141245e5862c09aad6e7095c4` |
| `run.json` | 542 | `0600` | `c4da1ff8a7f84e41fc9a8c5b3171096ee1dcb8d328070a1a37124a513e5812fc` |
| `complete.json` | 635 | `0600` | `9c05f0d4c6ccf5d049ca41a99965bae2d0c3f4724a481cadaced2f1c4683f919` |

The bundle directory is mode `0700`. Capture status is `GREEN`; gate status is
`RED`; scope is `PT-0.W0.parse-only`; profile is `pt0-parse-only`.
`run.json` has no `census` member.

The exact verify command ran twice after capture. Both executions returned:

```text
PT-0 parse-only bundle verified: 9c05f0d4c6ccf5d049ca41a99965bae2d0c3f4724a481cadaced2f1c4683f919
```

Two same-path capture replays returned exit `1` with `EEXIST` before any census
or publication. The terminal member and governed-source hashes remained exact.

## Captured parse-that identity

- HEAD: `ef10d5b78236c4a30a7bb22a6113b60bdc4bdf42`
- HEAD tree: `d320357165f3d34ae2cde93872273cc4a33b924a`
- index: 270 entries / 23053 bytes /
  `1d3f112a283dd28b08fb8c05f774fcc6f0540921b253a24a29b13dca7fc6f478`
- dirty status: 181 records / 11992 bytes /
  `b77d73ec0879be6094ab37c8ce8f787f2ce44670c202813fe2209f0feffddc4e`
- dirty content: 214 entries /
  `e924560056517b5f922278ca23aca81cff379cb02e141c352a2ae666875ffb2b`
- lockfiles: `38ae11346efce5dea15779f05efc2f227713b2e5942f76137bb3e41feebeb72f`

| Operand | Entries | SHA-256 |
|---|---:|---|
| `typescript-production` | 129 | `1497c9ff339640722e468aa9c50ddc7bdf5a2aba0ae25fe259d336ab09f33b57` |
| `rust-production` | 183 | `037c5466bab11f4b2625aba6237f16c2dce1a596da2ec33be7ca13a14ead1657` |
| `tranche-t` | 166 | `46de3f6baad18d0f53fd5e57576db41a26e399cfe1b623b8b71c63f0947fd1ba` |
| `tranche-u` | 13 | `39bee35f64ef51c27def9beaa9eff9081c355b36d0813ce1fef3f68ccd3cade1` |
| `invocation-v4` | 68 | `71545234e9526dce62ac3ac853179b6fa324d1f3cd3798b523594f18d4cf8ae4` |
| `binding-authority` | 42 | `ce7668a1682338ffffebc3127be6ac86b5a010af89fdd30f7e873abffe607dbb` |
| `toolchain-selection` | 2 | `4fec9a1569f12060fc1a23e896f555045c5d2c1691984bb6e1b82d8d7de4f286` |

## Tool identities

| Tool | Payload SHA-256 | Version SHA-256 |
|---|---|---|
| Git | `12bed4523661307059b879b9b54e77a73176e9d27d27a0e40363271d8f0668ba` | `b6d9afe6e5be5d40f0e18a09a0708100902e19a4d0e11fb5d3930d59785cb1aa` |
| Node | `08dad0581f00a0cabf4d49ec92ca1f25fdfd01c2c18fa8e92b35f04d4c24c164` | `4ba81b51ceb2a5121a3018c8d23adc26a59dd3e470a2a6c7e75eb7c31327adfd` |
| rustc | `95d199b3cc0a2e43a904e3ed6120738ce768b7300214023a56d60cbeacf76d36` | `d5f79f5406781b8544d4eeb2e4d29a85352b8348d3c2fd0056c76e46eda02ac2` |
| Cargo | `c6170b6af6c6205953bff3119c3c003744f5b85e9f76489d12e3e45094f1b296` | `81def4d843d4c9dc8af475715cb356caef3d673603434c57e5ea75d522a6a97e` |

The Apple ARM launcher is 121216 bytes at
`3f73f8ed8edc1d6bb991681c51f76e18f8c30436a5d0bed5a2f0ded8c3a67ac2`.
The selected Git invocation was exactly launcher argument `-arm64`, selected
architecture `arm64`, process architecture `arm64`, and nontranslated.

## Historical preservation

Every listed hash matched both before R5 edits and after terminal capture,
verification, and no-replace replay:

| Historical path below `common` | SHA-256 |
|---|---|
| `evidence/PT0-UW0-RECONCILIATION.md` | `cc4c293cf876d1505a68d5206673d43dadb8b26b2b100f144de95a28414a2c19` |
| `evidence/PT0-W0-AUTHOR.md` | `2a436943e844a094a5a867584fa055ddc9aec1a2e36dad1fd24151d3d8e748a2` |
| `evidence/PT0-W0-R3-ADJUDICATION.md` | `a32aa235c84c66d342e06e297e27363dda71a0ce16601d9db2304ebf9c868da5` |
| `evidence/PT0-W0-R3-CRITIC-A.md` | `36a6e315903a4bd2cc2eba365e9ab9786d4efa441e11d5b645ae7d3d688e59e9` |
| `evidence/PT0-W0-R3-CRITIC-B.md` | `63ba8cb32e9e26a3972be2480915964c2cb7d7d4bba4b9de005d709d8f79a5e4` |
| `evidence/PT0-W0-R4-AUTHOR.md` | `fcb9b81ccc478d6eb7ef95021cb8b9c182c3594c80c6d5b744996e08a224e611` |
| `evidence/PT0-W0-RECEIPT-OVERWRITE-INCIDENT-2026-07-22.md` | `db6db896482317f06d5c310f57968c257a9fe9de83bfd3028a861278c540a938` |
| `receipts/pins.json` | `74a0c99e01c3de91c599f0c76617a8746067d26ef9425225e39cfd83ddc4e2eb` |
| `runs/pins.run.json` | `844475a73917d106823ad567760aef10e994d98b6b69c6a5f123f6b318283c2e` |
| `runs/pt0-parse-only-r2.receipt.json` | `9ef888aa6aad8d807f9352a0acd28bbc2472636cc4aeb2fcb270fc026aed96b7` |
| `runs/pt0-parse-only-r2.run.json` | `71bf52c33a88b9382dd101904b0cde8af6e6d9929ddb4a1a749b7d8a9db64e77` |
| `runs/pt0-parse-only-r3.receipt.json` | `eb08c60c3fe61758874ecc10ad574e1247a481aad6bef88a056b3936295ba581` |
| `runs/pt0-parse-only-r3.run.json` | `6a3037cac9461c4dc057cf206a4a0d3f4edf0778597aad3c3bf1889ad5ec8bb7` |
| `runs/pt0-parse-only.config.json` | `cd82e57181cddb95231a61c3bf761630a58c933ff722f37f3f1421483816c543` |
| `runs/pt0-parse-only.receipt.json` | `66355aa0accf7bfd1cf7f81d56bcc33b422bc5bb387fd7fe1c7d2165e0656e3b` |
| `runs/pt0-parse-only.run.json` | `a44137808f2d87446c553cb47f14bd8b35bcc08e38d0f8fc266a1889319e3b75` |
| `runs/pt0-parse-only-r4-20260722T154643Z/receipt.json` | `b4ed372b55b980a27defe334de4e49d2be15cacfb7e92478c2bc2503a64c3fd5` |
| `runs/pt0-parse-only-r4-20260722T154643Z/run.json` | `79bc17cdd46bb790c44cb7665c8eeeb4cfa5d9f26002cf55381d6fda9fade354` |
| `runs/pt0-parse-only-r4-20260722T154643Z/complete.json` | `c072160f736f5325caa0beba773bd36ae29c98490c1c453db28135f23dbdd62c` |

## Remaining RED

The machine gate remains exactly `RED`. R5 is still parse-only, hostile R5
review has not begun, and the complete PT-0 denominator is not bound. This
author record authorizes no movement.
