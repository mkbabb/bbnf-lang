MODEL: gpt-5.6-sol; REASONING: ultra; WORKFLOW: v2

# PT-0 common harness R6 — terminal adjudication

## Declaration

MODEL: gpt-5.6-sol; REASONING: ultra; WORKFLOW: v2

Role: fresh terminal adjudicator. I did not author R6 or either hostile report.
I assumed every author and critic claim was wrong, read the frozen R5 order,
R6 subject, bundle, lease/release, configuration, scripts, and tests, and used
only read-only Apple-ARM probes. I made no parse-that or Value mutation,
capture, recapture, movement, staging, or commit.

## Verdict

**REJECT R6 AS CLOSURE OF R5 AND AS PT0-ART CLOSURE. RETAIN ITS EXACT
CURRENT PARSE-ONLY BUNDLE AND ITS NARROW CLOSED-CHILD, EFFECTIVE-GIT,
RUNTIME-OBSERVATION, NON-MUTATING-GIT, PUBLICATION, AND NATIVE-ARM FACTS.
TWO HOSTILE REVIEWS RAN; ZERO OF TWO IS CLEAN. PT-0, PRODUCT, PACKAGE,
PERFORMANCE, VALUE, T/U, AND MOVEMENT CREDIT REMAIN ZERO. THE CAMPAIGN
REMAINS ACTIVE AND UNBLOCKED.**

The two critics are materially correct. Their fourteen numbered findings reduce
to ten repair obligations because cache/publication/release and Git-authority
findings overlap. No critic finding is disproved. One historical proposition—
that the exact terminal R6 bundle is current—is independently proved here; that
does not repair the reusable harness.

## Frozen authority and subject

| Artifact | SHA-256 |
|---|---|
| R5 adjudication | `084d334340e83d89e3eebb14933ad87fa950ff6b3e6c62ef48252a73f9526476` |
| R6 author | `89020a9f7e147235af4e0d424d5e922d1f20eff1c6029fa470d8553afe654afa` |
| Critic A | `cf33c3746598b81cd0857f71013eb1827644933482291ff8a4a9b9fbc876c12a` |
| Critic B | `6b599e410e53940d73bbd80c70b4cce356912c416d5832a56d11fab6cad2551d` |
| source lease | `8689e853cd36d9975167dfa5565d1c91f938d98708753d5849ec85e7ae04c313` |
| release record | `da6cf5444f875ef7a6d29a632337453c3e0aa448f81c52346e3540f7ac008234` |

All nine author-listed subject hashes matched:

| Subject | SHA-256 |
|---|---|
| `README.md` | `36be6d6226a0204568069bfea2ea792df8b62d60c2768f1d80d92fbadb52a7c4` |
| `package.json` | `b0406306174b694e3d2c2ec906bc626c623c57694f8c489f239d76bf3bb380fd` |
| `pins.config.json` | `1c693805eda94d5605a49ae9f0af4ee8c48e2d20aa6907659c7989206de83e5e` |
| `profiles/pt0-parse-only.json` | `c2c1a781f2748e262fc08e7598b114801b02f96566f3ec81b10bd5c7f26327b1` |
| `scripts/hash.mjs` | `50013064ec7aa47549b4d3bbd9900cb687f16eba0ddd8727d44e31caa79e08f0` |
| `scripts/pin.mjs` | `7515b67afd0f129a30c9b518e7057f077c9d67936b91dcf0cb1769f9fa07f00a` |
| `test/scripts/hash.test.mjs` | `5c66e18122e6a9ea5f2beea322c8558c67d27f27445a75c9affaa53787eb1691` |
| `test/scripts/pin.test.mjs` | `afb6d06e4fa725bb1d540817debd8736fca924b9e2c9cd80c61069fad68378d6` |
| `test/scripts/publish-child.mjs` | `050320c4cf2f267e0505ca410df92370090b9899e6d1cbdd4c6e79b453c5ce40` |

The unique bundle remains exactly three regular mode-`0600` members beneath a
mode-`0700` directory:

| Member | Bytes | SHA-256 |
|---|---:|---|
| `receipt.json` | 195593 | `bed5b66cbd861e980f5b01a27b234fce2dcf81298fd3bc48a0ad373a202e7ee1` |
| `run.json` | 841 | `777ab051f87541adabc93c3a957299b3ab7f89004407932e45f863baa549567e` |
| `complete.json` | 934 | `e5239f7bb076439a565892e560fa5a826b892f650f0dc2e3c20aa1942b1634bb` |

An independent native invocation,
`/usr/bin/arch -arm64 /opt/homebrew/bin/node scripts/pin.mjs verify`, returned
the same completion-seal hash. Thus the declared parse-only receipt is current
at this adjudication boundary under the current harness. The replay cannot prove
uncached independence because it exercises the same implementation defects.

## Finding-by-finding rulings

### 1. Cached recensus — correct

`runtimeClosureCache` returns the same promise for an entry file, and
`rustAuthorityPromise` returns the same entire Rust authority. Both consecutive
censuses and the post-seal census share those module-global values. Runtime,
system-image, Rust payload, component, or manifest drift after the first lookup
can therefore be invisible. A new verifier process proves present currentness,
not transaction-level independent recensus.

### 2. Parent tool selection — correct

The child environment is closed, but `rustAuthority()` first derives
`~/.rustup` and `~/.cargo/bin/rustup` from parent `os.homedir()`. An independent
native-ARM process with only parent `HOME` changed attempted
`/tmp/pt0-r6-parent-home-proof/.rustup` and failed there. Parent selection is
therefore effective, ambient, and absent from the receipt. The critic's core is
proved; the defect precedes child launch.

### 3. Effective analyzer dispatch — correct

`lipo`, `otool`, and `dyld_info` are invoked through `/usr/bin` developer-tool
shims while `applePlatform.analyzers` binds only those shim files. All three
shims hash to `12bed4523661307059b879b9b54e77a73176e9d27d27a0e40363271d8f0668ba`.
The effective Xcode implementations are distinct and unbound:

| Analyzer | Effective SHA-256 |
|---|---|
| `lipo` | `ff35e1c7e734048925405f295de62010a174328ea3dfe14aeca7ad06c30da516` |
| `otool` | `7b5fac0d61c4c8d57c578bd9e10cfb87b2ff743aec5b24688ffc9c33856c8b58` |
| `dyld_info` | `fb7bb6780c53d9cbb86f84a5466e7c381dec27c60fe513b83ed4e67f67037fbe` |

Those implementations produce the architecture, dependency, rpath, and shared-
cache UUID evidence. The recorded shim hashes remain true dispatcher facts, not
effective analyzer identities.

### 4. Git authority closure — correct

R6 correctly binds the raw index, local config, HEAD, and optional worktree
config and rejects the named ambient redirects. It does not bind or reject the
common-dir channel, object alternates, local exclude authority, symbolic-HEAD
target ref, or packed refs. This is concrete, not merely hypothetical:
`.git/info/exclude` is a live 240-byte regular file, `.git/packed-refs` is a live
1,438-byte regular file, and HEAD names `refs/heads/master`; none is in
`rawGitAuthority()`. No current `commondir` or alternates file exists, so current
redirection contamination is unproved. The general complete-Git-authority claim
is false.

### 5. Operand completeness — correct

The parse profile roots `typescript`, `rust`, selected tranche trees, and
binding/toolchain material, but omits root `grammar/` and `data/`. Production
tests consume both: Rust CSS recovery and parse tests use files from
`grammar/tests/css` and `data/css`, while the TypeScript JSON-vector test reads
`grammar/tests/json`. Consequently the receipt is exact for its declared
operands, but neither the lease scope nor the author's “all source and
build-input bytes” statement is proved.

### 6. Configuration and schema closure — correct

Receipt/run/seal and many nested receipt records have exact-key validators. The
configuration authority does not: `validateConfig()` does not apply
`exactRecord()` at the top, repository, package, or operand layers. Independent
in-memory probes accepted unknown keys at all four layers, a missing parse
package coordinate, and a numeric operand ID. Thus canonical bytes close the
chosen configuration, not its allowed meaning.

The tests establish some unknown/deletion rejection, but do not cover every
governed nested class or the open configuration layers. Claims of total schema
test closure are therefore incorrect.

### 7. Lease schema, acquisition, and cooperation — correct

`validateLease()` requires only that time values survive `Date.parse`; it does
not first require canonical UTC strings. An independent probe accepted numeric
`starts_at` and `expires_at`. `requiredString(holder)` also admits an empty
holder. The harness reads a pre-existing file; it does not exclusively acquire
the lease, enumerate governed writers, or validate the acknowledgment preimage.

Repository-wide search found the acknowledgment digest
`256303390b8fd8ea568583b161458d182bac3b1708fcf05059c61e98274f6afb`
only in the lease and author/critic evidence. Neither the exact preimage nor a
writer-by-writer acknowledgment is durable. Cooperation during the historical
window is therefore **unproven**, not disproved.

### 8. Release consumption and publication ordering — correct

Capture loads only the lease. Verification loads neither the external lease nor
the release. Internal lease bindings can therefore prove only that the three
bundle records agree. The separate release is not part of verifier authority,
and any unique R6-prefixed bundle can reuse the same lease bytes while valid.

More decisively, `complete.json` is durably published before the post-seal
`stableCapture()` and lease recheck. A failure in either later operation leaves
a three-member bundle accepted by `verify`. The historical release record is
content-consistent with the terminal bundle, but its `RELEASED` event is
semantically unconsumed while the lease file remains `ACTIVE`. The author claim
that post-seal recensus governs successful publication is false.

### 9. Mode enforcement — correct in its deciding core

The terminal modes match the author's report. `readCompletedBundle()` checks
directory/file kinds and names but not the promised `0700`/`0600` modes. The
external lease and release are ordinary mode-`0644` files. Filesystem
immutability is not intrinsically required by the cooperative same-user model,
but exact mode enforcement and an authoritative no-ABA transition are required
if they are claimed. Neither exists.

### 10. Test and claim closure — correct

The recorded 39/39 native-ARM runs are useful historical execution evidence.
The inspected bank lacks deciding regressions for cache independence, parent
home selection, effective analyzer resolution, local Git authority, full
configuration/nested schemas, lease contention and preimage/writer closure,
release consumption, post-seal failure, and bundle modes. Passing that bank
twice cannot close behavior it never assays.

## Retained R6 facts and exact credit

The defects above do not erase measured facts:

- child commands receive the small owned environment; ambient child selectors
  are not spread;
- every census Git operation directly invokes the selected Xcode Git payload
  through explicit Apple ARM with `GIT_OPTIONAL_LOCKS=0`, disabled
  system/global config, and raw index/config/HEAD invariance checks;
- the receipt records exact current non-system Mach-O observations, including
  Node `libnode.147.dylib` at `0afe378c…` and rustc's driver at `16cd7da5…`;
- Node runtime `c58646e8…`, rustc runtime `ebb8e7e4…`, Cargo runtime
  `329f3331…`, and effective Git runtime `872b6479…` remain exact captured
  observations, subject to the analyzer/cache limitations above;
- the selected execution facts are macOS `26.4.1` build `25E253`, Darwin
  `25.4.0`, native untranslated arm64; the inspected and replayed route used
  `/usr/bin/arch -arm64`, and no x86 execution path was observed;
- receipt/run/seal canonical bytes, cross-hashes, no-replace publication, unique
  bundle naming, and receipt/run interruption rejection remain narrow facts;
- Value is absent from declared operands and no inspected R6 runtime path reads
  Value; this grants no Value authority;
- the current bounded parse-only bundle verifies exactly, but does not cover
  omitted grammar/data inputs or prove the reusable transaction.

Exact counters:

- hostile reviews executed: **2/2**;
- consecutive clean hostile reviews: **0/2**;
- R6 repair closure: **REJECTED**;
- PT0-ART lock closure: **0**;
- PT-0.W0 and full PT-0 credit: **0**;
- T/U admission or movement: **none**;
- package, product, performance, and Value authority: **none**.

This RED is technical evidence, never an ownership blocker and never a reason to
wait for Claude or Value.

## Smallest cohesive R7

Keep the two-script parse-only harness. Do not add a new orchestration framework,
full PT-0 denominator, parser feature, or sibling-repository read.

1. Remove cross-census runtime/Rust memoization. Each deciding census must
   independently resolve and hash its runtime and Rust authority.
2. Make parent selection explicit and bound: the profile owns the canonical
   Rust authority root/payload, and the harness rejects ambient alternatives.
   Resolve `lipo`, `otool`, and `dyld_info` to exact Xcode payloads, record them,
   and invoke those payloads directly.
3. Close Git authority actually read by the census: bind or fail closed on
   common-dir/object redirects, `info/exclude`, the symbolic-HEAD target ref,
   `packed-refs`, and the remaining object/ref authority; disable replace
   objects. Add direct regressions for each channel.
4. Add `grammar/` and `data/` as governed parse operands. Describe the lease
   scope only by the exact declared roots.
5. Apply exact-key/type validation to every configuration layer. Require
   nonempty identifiers/holders and canonical round-tripping UTC timestamps.
   Add one unknown-key and one missing/invalid-type probe for every governed
   record class.
6. Make lease authority a closed protocol: durably bind the acknowledgment
   preimage and governed-writer set; acquire a unique lease with no-replace;
   reject contention and ABA; keep it held through final independent recensus
   and completion-seal fsync.
7. Publish `receipt.json` and `run.json`, perform the deciding recensus while the
   lease is held, then publish `complete.json` last. Only afterward publish a
   release binding the lease, final recensus, and completion hash. Verification
   must require and validate that exact terminal release, so any late failure
   remains RED.
8. Enforce mode `0700` on the bundle and `0600` on members. Add cache, parent
   selection, analyzer, Git, operand, schema, lease/release, post-recensus
   failure, and mode regressions; run them twice via explicit Apple ARM, capture
   one unique R7 bundle, verify it twice, then dispatch two fresh hostile critics
   and a fresh adjudicator.

R7 starts at zero clean passes. PT-0.W1–W3, the complete PT-0 denominator,
T.W0, and U.W0 remain ordered downstream work and are neither imported into R7
nor waived.
