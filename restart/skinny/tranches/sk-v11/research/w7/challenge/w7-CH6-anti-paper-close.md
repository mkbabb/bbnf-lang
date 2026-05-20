# SK-V11 W7 CH6 - Anti-Paper-Close

Pass: W7 Phase 2.5 CHALLENGE.
Lens: CH6 anti-paper-close / next-wave impact.
Date: 2026-05-20.
Output: this file only.
Source edits: none.

## Authorities Read

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 11.
- `restart/skinny/tranches/sk-v11/research/w7/w7-plan-output-digest-entry-block.md`.
- `restart/skinny/tranches/sk-v11/research/w7/w7-R1-direct-digest-surface.md`.
- `restart/skinny/tranches/sk-v11/research/w7/w7-R2-hot-leaf-eligibility.md`.
- `restart/skinny/tranches/sk-v11/research/w7/w7-R3-redress-preblocks.md`.
- `restart/skinny/tranches/sk-v11/research/w7/w7-R4-nonjson-host-sink.md`.
- `restart/skinny/tranches/sk-v11/research/w7/w7-R5-oracle-independence.md`.
- `restart/skinny/tranches/sk-v11/research/w7/w7-R6-gate-report-schema.md`.
- `restart/skinny/tranches/sk-v11/HANDOFF.md`.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md` entries 113-117.

## Disposition

ACCEPT.

Accepting the W7 block before source redress is honest REDRESS discipline, not
paper-closing a viable wave. The packet does not claim admission, does not move
`skinny/RESULTS.md`, does not convert a guard row into a residual win, and does
not use prose to close the non-JSON axis. It records that the W7 entry gate is
not measurable under SPEC Section 11 with the available evidence.

## Findings

### 1. The W7 entry gate is not satisfied

SPEC Section 11 requires CHALLENGE acceptance that output digest/hash is an
observed limiting hot leaf for a bounded selected product-row subset, with the
exact scalar fold/mix source, output plane, and independent oracle named. The
exit gate then requires selected direct rows whose fresh post-W6 profile still
names `output_digest_hash` as limiting to meet the Section 0.4 floors on both
generated Track 1 and independent Track 2/oracle.

The W7 packet has W3-W6 dispositions, but it does not have the required fresh
post-W6 profile. R2 makes the narrower honesty claim: the S-P1 profile is
behavior-equivalent triage because W3/W4 patches were rejected and W5/W6 blocked
before source redress. That is useful for deciding whether to spend the wave,
but it is not the literal post-W6 profile required for W7 admission.

### 2. No residual row has credible W7 floor math

Even granting S-P1 as triage evidence, no bounded residual subset is viable:

- `distinct_values/direct_to_struct` has the strongest residual digest signal,
  but 1750 / 1625 Mbps is far below the 2658 Mbps floor, and visible
  digest/support cost is not enough to close both tracks.
- `random/direct_to_struct` is near on Track 1, but Track 2 is 6949 Mbps
  against the 7878 Mbps floor and is not digest-limited enough for W7.
- `update_center/direct_to_struct` and `github_events/direct_to_struct` have
  floor gaps larger than the visible digest bucket.
- `unicode_mixed`, `unicode_escapes`, and `y_string_unicode` are string/escape
  limited. W6 already blocked the decoded source-method digest fold route.
- `apache_builds/direct_to_struct` has the clearest digest bucket, but it is
  already a direct guard `A / GO`, not a residual admission target.

That means CH6 cannot name an exact measurable W7 source wave without inventing
row movement unsupported by the research packet.

### 3. The obvious source seam is pre-blocked

The source route most likely to affect escaped/string-heavy rows,
`JsonDigestSink::*_source` decoded-byte folding, is not a fresh W7 host-sink
idea. REDRESS 117 records it as a REDRESS 54 replay with REDRESS 55/66/69
adjacency: same sink seam, same current `JsonDirectDigest` length/fingerprint
contract, and same allocation-removal claim.

The remaining legal JSON seam would be below the decoded `&str` boundary around
`hash_bytes` / `mix`. The packet does not show a residual row where that seam is
the limiting hot leaf and large enough to close both tracks. Dispatching source
redress anyway would be the paper close.

### 4. Non-JSON cannot rescue W7

W7 may use selected non-JSON oracle/report files only if W1b supplies digest
output baseline authority. REDRESS 113 says W1b admitted no generated non-JSON
Track 1 baseline, and W2 then blocked because the generated non-JSON threshold
was undefined. W7 cannot create the first generated non-JSON row under C8
without bypassing the W1b/W2 failure ledger.

The non-JSON route should be carried forward as blocked context, not treated as
an alternative W7 admission path.

### 5. The proposed block is fail-closed

The plan's block leaves all residual rows residual, opens no owner path, and
requires a REDRESS entry plus an empty `/tmp/skv11-waveW7-rejected.patch`
marker if accepted. That is the correct fail-closed posture. It is materially
different from a paper close because it does not:

- move `skinny/RESULTS.md`;
- claim `A / GO` for any direct row;
- count direct digest evidence as typed proof;
- claim grammar generalization;
- use cache hints, profile-only evidence, or gate-only metadata as admission;
- route around REDRESS 54/55/66/69, 113, 116, or 117.

## Anti-Paper-Close Decision

The challenge accepts the block because no exact W7 source redress can be named
that satisfies all of:

1. fresh post-W6 profile naming `output_digest_hash` as limiting;
2. bounded residual row subset;
3. legal scalar fold/mix source outside the REDRESS 54 decoded-source family;
4. independent Track 2/oracle;
5. plausible floor closure on both tracks;
6. same-wave gate/report consumption.

If a later challenge wants REVISE instead, it must name the exact row, exact
source function, independent oracle, fresh-profile command, and floor math. This
CH6 review cannot do so honestly from the W7 packet.

DISPOSITION: ACCEPT
