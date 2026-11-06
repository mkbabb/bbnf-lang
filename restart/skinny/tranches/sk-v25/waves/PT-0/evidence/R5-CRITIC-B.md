# PT-0 common harness R5 — hostile critic B

MODEL: `gpt-5.6-sol`; REASONING: `ultra`; WORKFLOW: `v2`

Role: fresh hostile critic B. I did not author R5, did not read Critic A, did
not edit or recapture parse-that, and treated the subject as wrong until its
frozen bytes and independent Apple-ARM replays compelled narrower facts.

## Disposition

**REJECT R5 AS CLOSURE OF THE R4 REPAIR OR PT0-ART. RETAIN ITS CURRENT
PARSE-ONLY CENSUS AND NARROW PUBLICATION/INTERRUPTION FACTS. FULL PT-0 CREDIT,
WAVE CREDIT, MOVEMENT CREDIT, AND CLEAN-PASS CREDIT REMAIN ZERO.**

R5 is current at this assay boundary: the exact frozen bundle verifies twice
against the live governed parse-that bytes. It nevertheless leaves the central
effective-tool-payload obligation open, can write Git index metadata while
performing a nominally read-only census, and accepts machine records outside its
declared bundle schema. Those are demonstrated implementation defects, not
complaints that R5 has not yet implemented the honestly RED PT-0.W1–W3 or U.W0
planes.

The campaign remains active. These findings are technical RED evidence, never an
ownership blocker or a reason to wait for another tool or agent.

## Frozen subject

- controlling R4 adjudication:
  `6a2c1444ed327309997c043050bb7c3c90a18b9d64b2ccdec3b3ef89f09440a6`
- external R5 author record:
  `fe7d4309f203589fe5375c895478a4fbccd5bebf51425006bc35c2b1ecebe3a7`
- `scripts/hash.mjs`:
  `6c76d1a0bfc4efdabb98332f169d0ab4a028738248b1e42f81f0b6292fb2f037`
- `scripts/pin.mjs`:
  `1a9aa4f5f3b014f5dbb8c390872dc89176a3b3a1b9b248c82a2bc1491552b926`
- hash tests:
  `812037a685896c3d1846627337bdc2d46e63e46c7b33d3b3871b1a2c83bec7f7`
- pin tests:
  `5235bffdb1e3d24f060fc5f47dcd55fdba7ff17b49ec60bedf4846e23c94ce6a`
- child witness:
  `050320c4cf2f267e0505ca410df92370090b9899e6d1cbdd4c6e79b453c5ce40`
- profile/config/README/package:
  `c2c1a781…`, `1c693805…`, `7cea8b68…`, `b0406306…`
- R5 bundle:
  `pt0-parse-only-r5-20260722T162040Z`
- receipt/run/seal:
  `060580f815d9de5d3527a65a76b3f7a6bb4bdbb141245e5862c09aad6e7095c4`,
  `c4da1ff8a7f84e41fc9a8c5b3171096ee1dcb8d328070a1a37124a513e5812fc`,
  `9c05f0d4c6ccf5d049ca41a99965bae2d0c3f4724a481cadaced2f1c4683f919`

All subject hashes still matched after this review.

## Independent replay

Every executable replay was explicitly routed through Apple arm64. The host
reported `arm64`, the Node process reported `darwin/arm64`, and
`sysctl.proc_translated` reported `0`.

```text
/usr/bin/arch -arm64 /opt/homebrew/Cellar/node/26.0.0/bin/node \
  --test test/scripts/*.test.mjs
A: 29 pass / 0 fail / 1486.17225 ms
B: 29 pass / 0 fail / 1475.501 ms

/usr/bin/arch -arm64 /opt/homebrew/Cellar/node/26.0.0/bin/node \
  scripts/pin.mjs verify --profile pt0-parse-only \
  --bundle runs/pt0-parse-only-r5-20260722T162040Z
A: PT-0 parse-only bundle verified: 9c05f0d4…
B: PT-0 parse-only bundle verified: 9c05f0d4…
```

These runs retain currentness and the tested narrow facts. They do not cure the
findings below.

## Findings

### F1 — HIGH — the bound “Git payload” is still a dispatcher, not the Git implementation

`hash.mjs:37-49` resolves `which git` once, realpaths that result, hashes it,
and routes it through `/usr/bin/arch -arm64`. On this host that result is
`/usr/bin/git`, a 118,928-byte developer-tool dispatcher. Its R5 receipt hash is
`12bed4523661307059b879b9b54e77a73176e9d27d27a0e40363271d8f0668ba`.
The effective default implementation selected by the dispatcher is instead:

```text
/Applications/Xcode.app/Contents/Developer/usr/bin/git
bytes 7058640
sha256 472013f51db0f4bd21b630b791423329e2c56a3d2a410834b04cb7cc072078b0
version git version 2.39.5 (Apple Git-154)
```

The separation is executable, not nominal. Calling R5's `gitTool()` with
`DEVELOPER_DIR` selecting Xcode and then CommandLineTools produced the same
recorded executable hash, launcher hash, `-arm64`, `selected_arch=arm64`, and
`translated=false`, while the effective versions changed:

| Selection | recorded executable SHA-256 | version SHA-256 | version |
|---|---|---|---|
| Xcode | `12bed452…` | `b6d9afe6…` | `2.39.5 (Apple Git-154)` |
| CommandLineTools | `12bed452…` | `0a72a762…` | `2.50.1 (Apple Git-155)` |

The version digest therefore detects this particular selection drift, but it is
not a byte identity for the implementation that performs `status`, `ls-files`,
or `rev-parse`; nor does the receipt bind `DEVELOPER_DIR`, the selected developer
root, or the selected implementation bytes. The explicit Apple-arm64 route ends
at the dispatcher. R4 ruling 3 required the exact Git payload to be resolved once
and used for every census and version call. That ruling remains unmet.

Repair minimally: resolve the effective developer Git once, realpath and hash
that implementation, then invoke that exact payload through the bound Apple-arm64
launcher for every Git operation. Bind the effective selection inputs needed to
reproduce it; do not substitute a version string for executable bytes.

### F2 — HIGH — a “read-only” census can mutate the active Git index

`hash.mjs:20-25` inherits the complete ambient environment, and
`gitCommand()` does not set `GIT_OPTIONAL_LOCKS=0`. `dirtyLedger()` then runs
`git status --porcelain=v2` at `hash.mjs:374-380`. Git is permitted to refresh
and write index stat data during that read-shaped command.

A disposable repository counterexample changed only a tracked file's timestamp
before the census. One invocation of the exact R5 status shape changed `.git/index`
bytes:

```text
before 539778d0d257861e58e9a22964b044d00500237b3368b2e87f4d83c5735e6aa6
after  46454eebfcebb45a2e031fedae594ead0e98f68478d72acc673f02197d9a488a
```

The same counterexample with `GIT_OPTIONAL_LOCKS=0` retained one identical index
hash, `b72b68b214047454c4a4a5a93717ea2dd295136f7d42510080a8d3895b56771c`,
before and after status.

Two censuses may report drift after the first census has performed the write, but
detecting that write does not satisfy PT-0.W0's requirement to establish operands
without modifying an active checkout. This is a demonstrated general-input defect;
there is no evidence that the already-quiescent R5 terminal capture happened to
refresh its live index.

Repair minimally: disable optional Git locks/refresh writes for every census
invocation and add a disposable stale-stat regression proving HEAD, index bytes,
index metadata, worktree bytes, and locks remain unchanged.

### F3 — HIGH — Node and rustc records bind launch files, not their effective code closures

`executable()` at `hash.mjs:405-423` hashes one realpathed executable and runs
that file for version output. That is useful launcher identity, but the author and
README call these the exact Node and Rust payloads. On the frozen host:

- recorded Node is a 68,384-byte launcher at `08dad058…`; its effective non-system
  `libnode.147.dylib` is 70,269,024 bytes at
  `0afe378c904a35194671ca1b19c7e1df8947fd1da01a7ce2efa1576d9ce21b10`,
  absent from the receipt;
- recorded rustc is a 412,504-byte launcher at `95d199b3…`; its effective
  204,375,512-byte `librustc_driver-0abf9245e2d34e96.dylib` is
  `16cd7da56e561db76e509b93edb419e10e90c645014e13a57e7e0aac0ea1f850`,
  absent from the receipt.

Cargo and the platform launchers likewise depend on external runtime code. The
Rust version text does prove the selected host triple and commit string, and the
Node process proves native Apple-arm64 execution; neither proves the omitted code
bytes. Retain the recorded executable-file hashes, but do not call them effective
runtime closure. PT0-ART needs a bounded policy that hashes non-system payload
closure and binds the platform/code-signing identity used for system code.

### F4 — MEDIUM — the bundle validator is open-schema and accepts contradictory machine records

`assertBundleContract()` at `pin.mjs:539-591` checks selected fields and specially
rejects only a `run.census` key. It does not require the exact run or seal key set,
and it does not require the volatile run fields created by `runRecord()`.
`verify()` later compares the recaptured receipt only (`pin.mjs:663-666`), so that
comparison cannot close the run or seal schema.

In a disposable copy of the exact R5 bundle, I:

1. removed `captured_at`, `host`, and `workspace` from `run.json`;
2. added `"authority":"GREEN"` to `run.json`;
3. added `"movement_authorized":true` to `complete.json`;
4. canonically re-encoded the records and updated the seal's run member hash.

`readCompletedBundle()` and `assertBundleContract()` accepted the result. The
receipt was unchanged, so the final live-receipt comparison would also agree at a
valid output path. This does not mutate the frozen R5 artifact and does not allege
malicious same-user resistance; it proves that “valid R5 bundle” is not a closed
machine format. It is the generalized residue of R4's falsifiable duplicated-run
summary defect.

Repair minimally: specify exact object key sets and types for receipt, run, seal,
members, and nested bindings; reject every unknown or missing field; add one
unknown-key and one required-field deletion mutation per record.

## Retained facts

- The frozen R5 receipt is current against the governed parse-that bytes at this
  review boundary; two independent verifications returned the exact seal
  `9c05f0d4…`.
- The exact 29-test suite passed twice under a native Apple-arm64 Node process.
- The profile/config bytes are cross-bound, select only parse-that, keep capture
  GREEN separate from the full gate RED, and contain no declared Value operand.
- No R5 runtime path access to Value was found. Test prose and rejection fixtures
  mention Value but do not read it. The broader ambient-child-process claim remains
  deliberately unmade.
- `run.json` no longer duplicates the repository census; the canonical frozen run
  binds its receipt once by `receipt_sha256`.
- The frozen bundle has exactly three regular mode-`0600` members in a mode-`0700`
  directory. Member hashes, lengths, canonical encoding, and seal cross-hashes
  agree.
- Exclusive directory creation, exclusive member publication, file/directory
  synchronization, and same-path `EEXIST` behavior are exercised. The two real
  SIGKILL witnesses prove rejection after receipt-only and receipt-plus-run
  publication. They prove process-interruption fail-closure only; no power-loss or
  hostile-filesystem claim is admitted.
- No new demonstrated defect was found in the narrow hard-link/no-replace ordering.
  Previously adjudicated ancestry-swap and malicious-replacement allegations remain
  unproven and are not recycled here as findings.

## Total tranche, wave, and feature fit

R5 is one parse-only artifact-truth repair, not PT-0.W0 completion. The binding
PT-0.W0 denominator includes parse-that, bbnf-lang, Value, Keyframes, relevant
Glass, complete dirty/untracked operands, disposable control craters, and the
canonical evidence receipt. R5 selects one repository; its catalog does not yet
contain Value. That is accurately labeled RED rather than concealed, so it is
remaining work, not an additional R5 misrepresentation.

PT-0.W1 semantic/coordinate/effect/package manifests, PT-0.W2 independent oracle
and born-RED bank, and PT-0.W3's five lock adjudications remain wholly open. T.W0
still needs immutable clean/disposable TypeScript and Rust baselines, all named
external defect probes, and byte-identical four-graph extraction. U.W0 remains
preseal research until T.W0–W7 and sealed GESTALT-T; none of its witness-bank
status is altered by R5.

The harness totals 1,233 implementation lines and 537 test/witness lines before
README/config data. I found no sound basis to demand a broad rewrite: NUL-safe Git
records, submodules, filesystem publication, and adversarial tests account for
real complexity. Parsimony now means three narrow corrections—effective payload
resolution/closure, non-mutating Git execution, and closed schemas—followed by one
new capture. It does not mean adding another orchestration layer or prematurely
building the full PT-0 denominator into this repair.

## Required next gate

1. Preserve the R5 bundle and this report; never edit or relabel them.
2. Repair F1–F4 with the smallest cohesive R6 subject.
3. Add the dispatcher-selection, stale-index, omitted-runtime-closure, and
   open-schema counterexamples to the regression bank.
4. Capture one new unique parse-only bundle on quiescent bytes and verify it twice
   with an explicitly selected native Apple-arm64 Node payload.
5. Dispatch two fresh critics and a fresh adjudicator. R5's hostile count does not
   transfer to R6.
6. Only after that narrow repair closes should the campaign extend the profile to
   the complete PT-0.W0 denominator; PT-0, T, U, product, package, performance,
   Value, or movement authority remains forbidden meanwhile.
