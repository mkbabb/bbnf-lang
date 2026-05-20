# SK-V11 W7 CH3 - REDRESS Regression Adjudication

Date: 2026-05-20.

Challenge: CH3 redress/regression.

Inputs:

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 11.
- `restart/skinny/tranches/sk-v11/research/w7/w7-plan-output-digest-entry-block.md`.
- `restart/skinny/tranches/sk-v11/research/w7/w7-R3-redress-preblocks.md`.
- `skinny/REDRESS.md` entries 34/35/48, 54/55/66/69, 64, 82, 93,
  100/101/109, 113, 116, and 117.

## Verdict

ACCEPT BLOCK.

The W7 plan's no-source block is required by prior REDRESS. The packet does
not name an admissible C8 output digest/hash host-sink intervention that is
materially different from the rejected decoded-string/source-hook family, and
it does not provide the SPEC Section 11 entry evidence needed to reach behavior
redress.

## Load-Bearing Findings

1. SPEC Section 11 narrows W7 to C8 output digest/hash oracle or per-product
   host sink only. It requires W3-W6 dispositions plus CHALLENGE acceptance
   that digest/hash is an observed limiting hot leaf for a bounded selected
   product-row subset, with exact scalar fold/mix source, output plane, and
   independent oracle named. The plan names no fresh post-W6 limiting profile
   and no selected source function that is both inside the allowed host-sink
   surface and clear of the pre-blocked decoded-string seam.

2. REDRESS 117 is directly controlling. W6's proposed
   `JsonDigestSink::*_source` decoded-byte fold was blocked because it reopens
   REDRESS 54: same sink seam, same current `JsonDirectDigest`
   length/fingerprint output contract, and same allocation-removal claim.
   W7 may dispatch only with REDRESS 54/55/66/69, 64, 82, 107, 108, 113, 116,
   and 117 carried forward. The W7 plan correctly refuses to relabel the W6
   decoded-source fold as host-sink work.

3. REDRESS 54 is not a generic warning; it measured and rejected exact decoded
   length plus `hash_bytes` in `JsonDigestSink`. REDRESS 55 then rejected the
   one-pass quote-source streaming hash version. REDRESS 66 rejected direct
   source-hook field-layout folding, and REDRESS 69 rejected semantic string
   fact hashing for the current direct digest workload. Together they block
   another decoded-byte length/fingerprint fold unless the plan proves a new
   consumer representation, not just a new placement of the same hash work.

4. REDRESS 34/35/48 remain binding on proof shape. Direct evidence must be
   generated Track 1 from lowered BIR with a structurally independent Track 2
   or oracle. The plan contains no candidate row with fresh generated Track 1,
   independent Track 2/oracle, same-run strict comparator, and same-wave gate
   consumption that could satisfy REDRESS 100/101/109's direct movement
   contract.

5. REDRESS 113 blocks the non-JSON escape hatch. W1b did not admit a generated
   non-JSON baseline, so W7 cannot create the first measurable non-JSON host
   sink under C8. A non-JSON digest-output path would need prior generated
   Track 1 plus independent same-plane oracle authority.

6. REDRESS 116 and 117 also block proof inheritance. W5 admits no span API or
   reusable scalar proof; W6 admits no escaped-segment primitive, x4 production
   consumer, source-method digest fold, non-JSON proof, or reusable scalar
   oracle. W7 must start from fresh output-sink evidence.

## Row-Floor Assessment

The plan's triage table is consistent with the block. Even if the old S-P1
profile is treated as planning evidence, the visible digest bucket does not
make `distinct_values`, `update_center`, `random`, or `github_events` clear
the Section 0.4 direct floors on both tracks. `apache_builds` is already a
guard row, not a residual reclamation target. The unicode residual rows remain
string/escape limited under the legal W7 seam, and REDRESS 117 blocks moving
that seam back to `JsonDigestSink::*_source`.

## Regression Decision

This is not a premature block. It is the fail-closed result required by the
existing ledger:

- no fresh post-W6 hot-leaf profile naming `output_digest_hash` as limiting;
- no exact legal scalar fold/mix source below a materially new host-sink
  representation;
- no independent Track 2/oracle plan capable of row movement;
- no non-JSON generated baseline authority;
- no row-floor math showing both tracks can close.

## Material Differential Required To Revise

Current verdict is ACCEPT BLOCK, not REVISE. The exact differential that would
be required for a future REVISE is:

- a fresh post-W6 same-host profile for a bounded selected product-row subset
  naming `output_digest_hash` or a concrete Section 11 host-sink function as a
  limiting hot leaf;
- a source delta limited to the Section 11 owner paths that changes the
  product host-sink output representation or field-specific access pattern,
  not another decoded-byte length/fingerprint fold in
  `JsonDigestSink::*_source`;
- bit-exact raw and decoded boundary equality against an independent
  Track 2/oracle that does not share generated parser control or generated
  SinkOnly helpers;
- same-run strict direct comparator evidence and Section 0.4 floor math showing
  both generated Track 1 and independent Track 2/oracle clear for each selected
  residual row, with guard floors preserved;
- same-wave `report.rs` / `gate.rs` consumption using W7 provenance.

Without that material differential, any W7 source redress would replay
REDRESS 54 through the route REDRESS 117 already blocked.
