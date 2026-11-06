# PT-0 common harness R6 — hostile critic A

MODEL: `gpt-5.6-sol`; REASONING: `ultra`; WORKFLOW: `v2`

Verdict: **R6 is not clean and does not close R5.** The terminal bundle
retains useful static evidence, but lease/release authority, complete Git
identity, operand closure, and independent recensus remain RED. Clean credit:
**0**.

## Findings

1. **Lease/release verification is incorrect.** `scripts/pin.mjs` names and
   loads only the lease during capture; verification loads neither the external
   lease nor release. `assertBundleContract` reconstructs the seal from the
   run's own lease binding, which proves internal consistency rather than
   external authority. The release records `17:10:50Z`, but the lease remains
   `ACTIVE` until `17:40Z`, and any unique R6-prefixed bundle could reuse it.
   `complete.json` is also published before the post-seal recensus and lease
   recheck, so a late failure leaves a verifier-acceptable three-member bundle.
2. **Lease types are incorrect.** `validateLease` passes `starts_at` and
   `expires_at` to `Date.parse` without first requiring canonical UTC strings.
   The lease tests cover unknown keys and expiry, not timestamp types.
3. **Owner cooperation is unproven durably.** The acknowledgement preimage and
   exact governed-writer acknowledgements are absent from both repositories;
   only digest `256303390…` remains. Current lease/release bytes are mutually
   consistent, but cannot prove who cooperated.
4. **Git authority is incomplete.** Environment redirects and raw
   index/config/HEAD/config.worktree are correctly handled, but `.git/commondir`,
   `.git/objects/info/alternates`, `.git/info/exclude`, the symbolic-HEAD target
   ref, and `packed-refs` are neither rejected nor bound. No current alternates
   file exists, so terminal contamination is not proved; README's complete
   redirected-object claim is nonetheless false.
5. **Parse operands are incomplete.** `pins.config.json` omits root `grammar/`
   and `data/`, although Rust CSS tests use `include_str!` from both and the
   TypeScript JSON-vector test reads `grammar/tests/json`. Therefore the lease
   and author record overstate “all source and build-input bytes.”
6. **Tool censuses are memoized.** `runtimeClosureCache` and the singleton
   `rustAuthorityPromise` are reused by both consecutive censuses and the
   post-seal census in one process. Runtime dependency, Rust payload, manifest,
   or sysroot movement after first measurement can remain invisible. Separate
   verifier processes improve terminal currentness but do not repair the
   harness transaction.
7. **Bundle/test enforcement is incomplete.** Verification does not enforce
   documented `0600` member or `0700` directory modes. Unknown/deletion tests
   cover receipt, run, seal, member, and tool records, but not repository,
   runtime, machine, Rust, ledger, raw-authority, or lease-binding record
   classes. The external lease/release are content-addressed ordinary `0644`
   files, not filesystem-immutable objects.

## Retained facts

- All author-listed R6 subject and bundle hashes/modes match. The R5 bundle is
  byte-identical.
- Closed child environment, explicit effective Xcode Git,
  `GIT_OPTIONAL_LOCKS=0`, direct Rust tools, native ARM routing, recursive
  Mach-O traversal, `libnode`, and `librustc_driver` are present in the current
  receipt.
- Value is absent from profile operands and runtime paths.
- Root's two 39/39 ARM test replays are valid narrow execution evidence; the
  missing regressions prevent closure.
- PT-0 remains RED with zero product, package, performance, T/U, or movement
  credit.

The critic performed read-only analysis and wrote no source or evidence bytes.
