# PT-0 common harness R4 — hostile critic B

MODEL: `gpt-5.6-sol`; REASONING: `ultra`; WORKFLOW: `v2`

Canonical agent: `/root/pt0_r4_critic_b`. This critic did not inspect Critic A.
Frozen author record:
`fcb9b81ccc478d6eb7ef95021cb8b9c182c3594c80c6d5b744996e08a224e611`.

Verdict: **REJECT R4.** Static R3 repairs stand; the frozen bundle is not durable,
fully cross-bound evidence.

## Findings

1. The receipt self-invalidates because `tranche-t` and dirty planes include
   `common/evidence`; the post-capture author record and all later reviews create
   drift.
2. `assertBundleContract` does not require `run.census` to equal
   `summary(receipt)`. An adversarially gutted, resealed run was accepted.
3. Exact lexical profiles exclude Value, but config/profile/package reads follow
   symlinks and Git inherits ambient configuration/hooks; categorical no-read is
   unproved. No actual R4 Value access was demonstrated.
4. Tool identity is not bound to every census command: repository operations use
   bare ambient `git`, while the recorded Git payload is captured later.
5. The ARM claim is incomplete. Node/rustc/Cargo are arm64 and Node reports no
   translation, but the recorded universal Git binary has no selected-slice or
   child translation receipt. This proves no x86 execution, but does not close the
   no-x86 requirement.
6. Path hardening is preflight-only; ancestor replacement between check and
   path-based publish/read is untested and undetected.
7. Crash testing omits write, file-sync, link, unlink, directory-sync, and
   completion-seal faults. An early write/sync failure may leave a temporary file,
   though absence of the seal remains fail-closed.

## Accepted facts

- Frozen hashes match and the suite passes 25/25.
- Bundle structure, modes, byte counts, and hashes match.
- GREEN capture and RED gate are correctly distinct; exact named profiles are
  enforced; version commands execute their hashed payload; replay is no-replace;
  historical hashes remain stable.
- No actual R4 Value mutation or demonstrated read occurred.

Credit remains zero. A non-circular fresh bundle, repaired semantic binding, exact
census-tool/ARM receipts, and hostile replay are required.

No file was edited by the critic.
