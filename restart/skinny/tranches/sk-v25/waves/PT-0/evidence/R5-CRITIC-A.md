# PT-0 common harness R5 — hostile Critic A

MODEL: `gpt-5.6-sol`; REASONING: `ultra`; WORKFLOW: `v2`

Role: fresh hostile Critic A. I did not author or review this R5 subject. I
assumed it was wrong, inspected only the frozen R5 subject and governing
specifications, and made no parse-that edit, capture, repair, movement, staging,
or commit.

Status: **FOUR FINDINGS PRESENT.** This is critic evidence, not an acceptance or
movement decision. The intentionally RED planes of the full PT-0 denominator are
separated from defects below.

## Frozen review identity

- R4 adjudication:
  `6a2c1444ed327309997c043050bb7c3c90a18b9d64b2ccdec3b3ef89f09440a6`
- R5 author record:
  `fe7d4309f203589fe5375c895478a4fbccd5bebf51425006bc35c2b1ecebe3a7`
- exact bundle:
  `parse-that/docs/tranches/T/prototypes/major/common/runs/pt0-parse-only-r5-20260722T162040Z`

I independently rehashed all nine frozen subject files. They match the R5 author
table exactly:

| Subject path below `common` | SHA-256 |
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

## Findings

### A-1 — P1 — main executable hashes are not exact runtime-payload closure

`hash.mjs:405-422` reads and hashes only the selected executable file, runs its
version command, rereads that one file, and records no executable path or loaded
dependency set. This establishes exact identity for the launcher file, not for
the implementation that executed.

The defect is concrete on the frozen host:

- the receipt's Node payload is a 68,384-byte executable at
  `08dad0581f00a0cabf4d49ec92ca1f25fdfd01c2c18fa8e92b35f04d4c24c164`;
  `otool -L` shows it loads `@rpath/libnode.147.dylib` and many Homebrew
  libraries. The current `libnode.147.dylib` is
  `0afe378c904a35194671ca1b19c7e1df8947fd1da01a7ce2efa1576d9ce21b10`,
  and that digest does not occur in the receipt.
- the receipt's rustc payload is a 412,504-byte executable at
  `95d199b3cc0a2e43a904e3ed6120738ce768b7300214023a56d60cbeacf76d36`;
  `otool -L` shows the matching selected binary loads
  `librustc_driver-0abf9245e2d34e96.dylib`. Its current digest is
  `16cd7da56e561db76e509b93edb419e10e90c645014e13a57e7e0aac0ea1f850`,
  also absent from the receipt.

The same structural omission applies to system libraries, dyld/OS state, and
the Rust sysroot. The observed versions and main-file hashes are true, but the
R5 evidence does not content-address the Node or Rust execution closure. The
author's unqualified `Tool identities` / `Payload SHA-256` presentation therefore
overstates what the artifact proves. This matters to PT-0.W0 artifact truth and
the required Rust pin even though no present replay mismatch was observed.

### A-2 — P1 — ambient environment can select census authority without entering the receipt

Every child process inherits the full environment at `hash.mjs:20-25`; only
`LC_ALL` and `TZ` are overwritten. Git is resolved once and then routed exactly,
but neither the resolution path, the inherited Git-selection variables, nor the
Git configuration inputs are recorded. Rust selection likewise starts with bare
ambient `rustup` (`pin.mjs:259`).

This is not merely a speculative hook allegation. A read-only hostile replay of
the exact R5 Git route showed:

```text
GIT_INDEX_FILE=/tmp/pt0-r5-critic-a-index \
  /usr/bin/arch -arm64 /usr/bin/git rev-parse --git-path index
/tmp/pt0-r5-critic-a-index
```

`GIT_INDEX_FILE` therefore redirects the object called the repository `index`
by the harness, while `GIT_CONFIG_*`, `GIT_WORK_TREE`, and related inherited
inputs can also alter Git interpretation. The current shell exposed no
`GIT_INDEX_FILE`, and no current corruption is alleged; the defect is that the
frozen bundle cannot prove which ambient selection state produced its ledger.
Two equal censuses and later verification under the same ambient state do not
close that ambiguity.

R4 order 3's narrower direct-routing requirement is satisfied: every inspected
harness Git census/version call uses the one resolved `/usr/bin/git` file through
`/usr/bin/arch -arm64`. Exact route identity does not cure the unbound authority
inputs consumed by that route.

### A-3 — P1 — source currentness is not atomic with completion-seal publication

`pin.mjs:619-653` completes `stableCapture`, then serializes the receipt and run,
then publishes `receipt.json`, `run.json`, and finally `complete.json`. There is
no governed-source lock, filesystem snapshot, or post-publication source census.
Consequently an ordinary governed write after the second census but before
`complete.json` can leave a well-cross-hashed, complete GREEN bundle whose
receipt was already stale when its seal appeared. The exact-member and seal-last
protocol detects interrupted publication, but it does not make source snapshot
and completion seal one atomic event.

The author assertion that governed bytes were frozen is external prose, not a
fact bound by the bundle. My two current verification replays both passed, so
this finding does **not** assert that the frozen R5 bundle is currently stale.
It establishes that capture cannot itself prove the R4 order 7 currentness
condition at its publication boundary. This is an ordinary quiescence gap, not
the malicious same-user ancestry race or power-loss claim that R5 correctly
disclaims.

### A-4 — P2 — the author record repeats the prohibited universal Value non-read claim

`R5-AUTHOR.md:11-13` states categorically: `No Value repository path was
accessed or mutated.` That is broader than R4 order 5, which permits only the
declared-operand fact and the static fact that no R5 code accesses Value. It also
contradicts the author's own correct qualification at lines 45-47 that the code
fact is not a universal claim about ambient child-process behavior.

The frozen profile contains only parse-that, the runtime source contains no
Value operand/path access, and I found no evidence that Value was actually read
or mutated. The finding is the unsupported evidence wording, not an allegation
of Value contact.

## Independently retained facts

- I ran the exact R5 verify command twice without capture. Both returned seal
  `9c05f0d4c6ccf5d049ca41a99965bae2d0c3f4724a481cadaced2f1c4683f919`.
  Governed source and bundle hashes remained unchanged.
- I ran `node --test test/scripts/*.test.mjs` twice: 29/29 passed on each replay.
  `node --check` exited zero for all five JavaScript files.
- The exact bundle has only canonical regular `receipt.json`, `run.json`, and
  `complete.json`. Their respective byte counts and hashes are
  `165488` / `060580f815d9de5d3527a65a76b3f7a6bb4bdbb141245e5862c09aad6e7095c4`,
  `542` / `c4da1ff8a7f84e41fc9a8c5b3171096ee1dcb8d328070a1a37124a513e5812fc`,
  and `635` / `9c05f0d4c6ccf5d049ca41a99965bae2d0c3f4724a481cadaced2f1c4683f919`.
  The run has no duplicated `census`; receipt/run/config/profile cross-hashes
  verify.
- The selected process was native untranslated Darwin/arm64, and all direct Git
  calls inspected use the one resolved payload through explicit `arch -arm64`.
  This closes R4's selected-slice routing defect; it does not imply A-1/A-2
  runtime/environment closure.
- Config, profile, and package-manifest loads use the shared contained-regular-file
  guard with non-symlink ancestry. The exact profile allows only `parse-that` and
  `git`, `node`, `rustc`, and `cargo`.
- Exclusive bundle-directory creation gives same-path `EEXIST` before census or
  publication. Members use exclusive temporary creation, file fsync, hard-link
  no-replace publication, directory fsync, and seal-last ordering.
- The two child witnesses wait until the receipt or run has been durably published,
  receive real `SIGKILL`, and leave respectively exactly one or two members. Both
  partial bundles are rejected for missing `complete.json`. These support the
  stated process-interruption boundary only; R5 makes no power-loss or hostile
  same-user-race claim.
- I found no additional concrete parsimony defect. The 1,233 runtime-source lines
  and 537 test/helper lines are substantial, but the retained ledger,
  containment, no-replace, seal, interruption, and guarded-crater mechanisms map
  to explicit PT-0/T.W0 obligations. Line count alone is not a defect.

## Intentionally RED and unclaimed — not defects in this R5 slice

The machine gate is correctly RED and this bundle is explicitly parse-only. It
does not bind the complete PT-0.W0 denominator: bbnf-lang, Value, Keyframes,
Glass, the complete Value tree, and the full multi-repository dirty/content
baseline remain absent. PT-0.W1 semantic/coordinate manifests, PT-0.W2 controls,
independent oracles and born-RED bank, and PT-0.W3 lock reviews also remain open.
U.W0 witnesses remain preseal research and no T-before-U or Value authority is
claimed. Those omissions keep full PT-0, package, product, performance, wave,
and movement credit at zero; they are not misrepresented as complete by the R5
artifact.

The narrow R5 repair location is tranche/wave-consistent: it is quarantined
common PT-0/T.W0 evidence, leaves production and protected Value untouched, and
does not authorize U admission or graph-directed movement. No movement ruling is
made here.
