# SK-V13 S-P1 V5 CH5 Hidden Coupling

Verdict: ACCEPT.

## Evidence

- V4 already accepted CH5 and explicitly scoped V5 as a confirmation challenge
  against the same V4 profile packet, not a new fold cycle.
- The former temp-only harness coupling is resolved: CSS and mode-III profilers
  have checked-in source snapshots, repo-relative rebuild commands, and
  verified rebuilt binary hashes.
- Sidecar coverage is explicit rather than assumed: direct has 34 profiles, 34
  sidecars, and zero bad return codes; mode III has 85 profiles/rows, zero bad
  return codes, and explicit unsupported probes (`p1b-samply-mode-2.md`,
  `p1c-samply-mode-3.md`).
- Remaining sidecar limitations are disclosed, not hidden: parse save-only and
  offline symbolication are called out, mode-III function-only rows remain
  bounded, and line-poor NEON/ASM rows do not create orphan SIMD authority.
- Generated summaries remain `/tmp` measurement artefacts, but V4 adds
  checked-in reproducers and output hashes for hot-leaf, direct, and mode-III
  TSVs (`p1e-hot-leaf-attribution.md`, `support/profile-provenance-v3.md`).
- Cross-surface coupling is bounded: retained V1 parse/typed captures have no
  `skinny/crates/` source delta, typed remains 7/17 with 10 missing product
  surfaces, and CSS V2 throughput is method-mismatched rather than treated as
  demotion/admission.
- The packet keeps profile facts separate from gate authority: all rows are
  `profile_signal_not_gate_admission`, and later REDRESS/gate waves must
  provide admission evidence.

## Blockers

None. CH5 still accepts without another profile fold.
