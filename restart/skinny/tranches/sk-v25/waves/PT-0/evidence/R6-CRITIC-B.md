# PT-0 common harness R6 — hostile critic B

MODEL: `gpt-5.6-sol`; REASONING: `ultra`; WORKFLOW: `v2`

Verdict: **R6 is not clean; reject it as closure of R5.** Full PT-0,
movement, package, performance, and Value credit remain zero.

## Findings

1. **Runtime closure is cached, not recensused.** `runtimeClosureCache` and
   `rustAuthorityPromise` in `scripts/hash.mjs` persist across `twoCensus` and
   the post-seal census. Runtime dependency, system-image, or Rust authority
   drift/ABA can therefore be masked. Current `libnode`/`librustc_driver`
   hashes are real observations, not proof of independent repetition.
2. **Publication ordering is incorrect.** `publishBundleMembers` publishes and
   fsyncs `complete.json` before the deciding post-seal recensus and lease
   recheck. A late failure leaves a three-member GREEN-looking bundle.
   `verify` requires neither successful capture completion nor the external
   release; no source consumes `R6-SOURCE-LEASE-RELEASE.json`.
3. **Parent `HOME` remains ambient authority.** Rustup home and executable are
   derived from `os.homedir()`. Closing child environments does not bind the
   parent selector used before child launch.
4. **Measurement analyzers remain dispatchers.** `/usr/bin/lipo`, `otool`, and
   `dyld_info` resolve through the same Apple tool shim recorded as
   `12bed452…`; the effective Xcode binaries (`ff35e1c…`, `7b5fac0d…`,
   `fb7bb678…`) are absent. These unbound implementations produce architecture,
   dependency, rpath, and UUID evidence.
5. **Lease acquisition/release is not authoritative.** The harness trusts any
   nonempty holder and syntactically valid ACK hash, performs no exclusive
   acquisition/writer enumeration/preimage validation/release check, and uses
   content comparison that permits ABA. The lease remains `ACTIVE` while the
   separate unconsumed record says `RELEASED`; the ACK preimage is not durable.
6. **Git common/object redirects are unbound.** `rawGitAuthority` omits
   `.git/commondir` and `.git/objects/info/alternates`. Both are absent in the
   current checkout, so this is a harness defect rather than evidence of
   terminal contamination.
7. **Config/schema mutations are incomplete.** `validateConfig` is open at the
   top, repository, package, and operand layers; it accepts unknown keys,
   missing parse package coordinates, and merely truthy operand IDs. Unknown
   and deletion tests omit repository, runtime, machine, Rust, ledger, config,
   and nested binding classes. No cache, ambient-HOME, local redirect, lease
   contention/release, or post-seal failure regression exists.

## Retained facts

- Value is absent from the parse-only profile and R6 runtime paths.
- Effective Xcode Git is directly invoked and separately bound from the shim;
  `GIT_OPTIONAL_LOCKS=0` and raw index/config/HEAD checks are present.
- Receipt/run/seal decoding and canonical seal construction are closed in their
  current bounded form.
- Native untranslated Apple ARM routing is supported; no x86 path executed.
- Root replayed 39/39 tests twice and verification twice, but those tests do
  not cover the defects above.
- The three R6 bundle members, modes, sizes, hashes, all nine subject hashes,
  and R5 preservation hashes match the author record.
- The author correctly leaves the gate RED, PT0-ART at zero, and grants no
  movement.

The critic performed read-only analysis and wrote no source or evidence bytes.
