# SK-V11 W8 R2: String/Control Direct Residual Fixpoint

Date: 2026-05-20.
Scope: W8 research R2 for structural/string/control direct residual rows:
`twitter`, `github_events`, `update_center`, and `random`.
Output: this read-only artifact.

## Authorities Read

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 12.
- W4 artifacts:
  `research/w4/w4-R5-row-floors.md`,
  `research/w4/w4-plan-container-tail-direct-v2.md`,
  `research/w4/challenge-v2/w4-CH4-cost-v2.md`, and
  `research/w4/redress/w4-redress-rejection.md`.
- W5 artifacts:
  `research/w5/w5-R1-parse-that-string-span.md`,
  `research/w5/w5-R4-row-gates-measurement.md`,
  `research/w5/challenge-v2/w5-CH1-correctness-v2.md`,
  `research/w5/challenge-v2/w5-CH4-cost-v2.md`, and
  `research/w5/redress/w5-redress-entry-blocked.md`.
- W7 artifacts:
  `research/w7/w7-R2-hot-leaf-eligibility.md`,
  `research/w7/w7-R3-redress-preblocks.md`,
  `research/w7/challenge/w7-CH1-correctness.md`,
  `research/w7/challenge/w7-CH4-cost.md`, and
  `research/w7/redress/w7-redress-entry-blocked.md`.
- `skinny/REDRESS.md` entries 115, 116, and 118.
- `skinny/RESULTS.md`.
- S-P1 hot-leaf attribution:
  `research/p1/p1e-hot-leaf-attribution.md`.

## Section 12 Fixpoint Rule

SPEC Section 12 makes W8 a direct residual fixpoint. It may admit only rows
that meet Section 0.4 on both generated Track 1 and independent Track 2/oracle.
Rows that miss must record the attempted candidate, measured tracks,
comparator, floor, and guard status. Source work is outside W8 unless split as
W8a with exactly one accepted candidate and one row subset.

For the four rows in this packet, W3-W7 have produced no admitted source route
and no reusable scalar proof. The only source implementation attempted in this
row set was W4's `random` container-tail helper, and it failed before
Criterion. W5 and W7 blocked before implementation dispatch.

## Row Fixpoint Table

Floors are Section 0.4 direct floors, `ceil(sonic-rs strict direct Mbps / 1.10)`.

| Row | Current Track 1 | Current Track 2 | sonic-rs direct | Floor | Current state |
|---|---:|---:|---:|---:|---|
| `twitter/direct_to_struct` | 11613 | 10816 | 15113 | 13740 | `N-direct / NO-GO`; T1 miss 2127, T2 miss 2924 |
| `github_events/direct_to_struct` | 11918 | 10596 | 14743 | 13403 | `N-direct / NO-GO`; T1 miss 1485, T2 miss 2807 |
| `update_center/direct_to_struct` | 8187 | 7474 | 11064 | 10059 | `N-direct / NO-GO`; T1 miss 1872, T2 miss 2585 |
| `random/direct_to_struct` | 7693 | 6949 | 8665 | 7878 | `N-direct / NO-GO`; T1 miss 185, T2 miss 929 |

## `twitter`

Current row evidence: `skinny/RESULTS.md` reports Track 1 11613 Mbps, Track 2
10816 Mbps, sonic-rs direct 15113 Mbps, floor 13740 Mbps. The S-P1 direct
profile attributes the miss to tiny8 string, whitespace, and movemask work:
Track 1 tiny8 20.0%, whitespace 17.6%, movemask 14.5%; Track 2 hand tiny
18.7%, whitespace 14.7%, movemask 10.5%.

Attempted candidates:

- W4 container-tail dispatch: not selected. W4 R5 treated `twitter` as high
  signal but too far from floor for the W4 control slice.
- W5 bounded string span: scout-only, not selected. W5 R4 listed `twitter` as
  W5-eligible but rejected it as a first redress row because of the large
  absolute Track 2 miss.
- W7 output digest/hash host sink: not eligible. W7 CH1 grouped `twitter` with
  rows whose limiting leaves are tiny/string/whitespace/movemask rather than
  output digest/hash.

Measured rejection/block evidence: no `twitter` source patch was attempted in
W4-W7. W5 blocked before implementation dispatch under REDRESS 116, so no span
API or reusable scalar proof exists. W7 blocked before source redress under
REDRESS 118 because no legal output-digest row/source/consumer/oracle candidate
exists.

Guard status: the same-corpus typed guard holds at
`twitter/real_typed_struct` Track 1 17740 and Track 2 15912 against maintain
floors 17385 and 15593. This is guard evidence only; it does not move the
direct digest row.

Legal W8a source candidate remaining: none. The only plausible family is the
W5 string-span family, but REDRESS 116 admits no span API, no rejected-but-
reusable proof, and no accepted Track 2 cost mechanism. A W8a source split
would need a new CHALLENGE-accepted candidate; none remains from W4/W5/W7.

## `github_events`

Current row evidence: `skinny/RESULTS.md` reports Track 1 11918 Mbps, Track 2
10596 Mbps, sonic-rs direct 14743 Mbps, floor 13403 Mbps. The S-P1 direct
profile attributes the miss to tiny/movemask/whitespace work: Track 1 tiny8
24.4%, movemask 15.2%, whitespace 13.6%; Track 2 hand tiny 19.9%, movemask
14.3%, whitespace 9.7%.

Attempted candidates:

- W4 container-tail dispatch: not selected. W4 R5 found the required Track 2
  lift too large for the first W4 slice.
- W5 bounded string span: scout-only, not selected. W5 R4 listed
  `github_events` as a profile fit but rejected it as a selected row because
  the Track 2 miss was 2807 Mbps.
- W7 output digest/hash host sink: explicitly rejected as a legal source route.
  W7 CH4's optimistic visible-bucket math estimated best-case Track 1 12980
  Mbps and Track 2 11336 Mbps, still below the 13403 Mbps floor.

Measured rejection/block evidence: no `github_events` source patch was
attempted in W4-W7. W5 blocked before implementation under REDRESS 116. W7
blocked under REDRESS 118; the row has visible digest support at most, not a
limiting digest/hash route that can close both tracks.

Guard status: the same-corpus typed guard holds at
`github_events/real_typed_struct` Track 1 11871 and Track 2 12275 against
maintain floors 11633 and 12029. This typed admission is independent guard
evidence and does not admit the direct digest row.

Legal W8a source candidate remaining: none. W5 left no accepted string-span
source candidate, and W7's legal host-sink surface cannot close either track
on the recorded floor math.

## `update_center`

Current row evidence: `skinny/RESULTS.md` reports Track 1 8187 Mbps, Track 2
7474 Mbps, sonic-rs direct 11064 Mbps, floor 10059 Mbps. The S-P1 direct
profile attributes the miss to tiny string, movemask, and digest support:
Track 1 tiny8 26.3%, movemask 10.0%, wrapping-add digest 7.9%; Track 2 hand
tiny 22.3%, skip string 12.3%, movemask 10.9%.

Attempted candidates:

- W4 container-tail dispatch: not selected. W4 R5 treated `update_center` as
  dispatch-relevant but too far from the floor for a one-shape control route.
- W5 bounded string span: scout-only, not selected. W5 R4 called it the best
  profile fit but not a good first row gate because it needed a large Track 2
  lift.
- W7 output digest/hash host sink: explicitly rejected as a legal source route.
  W7 CH4's optimistic visible-bucket math estimated best-case Track 1 9329
  Mbps and Track 2 8249 Mbps, still below the 10059 Mbps floor.

Measured rejection/block evidence: no `update_center` source patch was
attempted in W4-W7. W5 blocked before implementation under REDRESS 116. W7
blocked under REDRESS 118 because digest/hash is not limiting across both
tracks and the floor gap is larger than the visible bucket.

Guard status: the same-corpus typed guard holds at
`update_center/real_typed_struct` Track 1 11851 and Track 2 10358 against
maintain floors 11613 and 10150. This keeps the typed guard green but does not
authorize direct-row movement.

Legal W8a source candidate remaining: none. The string-span route is blocked
without a reusable proof, and the digest/hash route lacks both fresh limiting
profile and floor closure.

## `random`

Current row evidence: `skinny/RESULTS.md` reports Track 1 7693 Mbps, Track 2
6949 Mbps, sonic-rs direct 8665 Mbps, floor 7878 Mbps. The S-P1 direct profile
attributes the miss to string, whitespace, and digest/support work: Track 1
tiny8 23.8%, whitespace 17.9%, option-copied support 6.6%; Track 2 hand tiny
20.2%, whitespace 16.9%, wrapping-add digest 8.5%.

Attempted candidates:

- W4 container-tail dispatch: selected and implemented as a probe-first
  redress slice. Generated Track 1 factored a JSON-local post-value
  `container_tail_next` helper, hand Track 2 mirrored it independently, and
  gate/report learned W4-only `random/direct_to_struct` provenance at the
  7878 Mbps floor.
- W5 bounded string span: selected in research and planning as the primary
  string row, but blocked before source dispatch. CH1 kept malformed-input
  parity at REVISE and CH4 kept cost at REVISE because the plan had no concrete
  independent Track 2 mechanism to lift Track 2 from 6949 to 7878 Mbps.
- W7 output digest/hash host sink: considered and rejected. W7 CH4's optimistic
  visible-bucket math estimated best-case Track 1 8711 Mbps but Track 2 only
  7590 Mbps, below the 7878 Mbps floor.

Measured rejection/block evidence: W4 REDRESS 115 is the only measured source
attempt in this packet. The selected row failed before Criterion:

| Probe | Mbps | Floor | Outcome |
|---|---:|---:|---|
| `random` Track 1 | 3518 | 7878 | FAIL |
| `random` Track 2 | 3498 | 7878 | FAIL |

W4 therefore moved no `RESULTS.md` row and did not authorize a future tail
helper or stale-floor admission. W5 REDRESS 116 blocked without a source patch
or reusable scalar proof. W7 REDRESS 118 blocked without source, row movement,
or a rejected-but-reusable scalar oracle.

Guard status: `random` has no same-corpus typed guard in the Section 0.5 typed
guard set. The global direct and typed guard set remains the binding guard
surface for any future `random` attempt. W4 failed before Criterion and guard
admission, while W7's advisory gate ran on the unchanged surface with no
`RESULTS.md` movement.

Legal W8a source candidate remaining: none. The measured control route failed
badly, the string-span route was blocked before source with no Track 2 cost
mechanism, and the digest/hash route cannot close Track 2 even under perfect
visible-bucket removal.

## Fixpoint Conclusion

No legal W8a source candidate remains for `twitter`, `github_events`,
`update_center`, or `random` on the current evidence. All four rows remain
direct residual `N-direct / NO-GO` rows unless a future governance action
supplies a new CHALLENGE-accepted W8a candidate with one row subset, fresh
same-run Track 1 and independent Track 2/oracle measurement, strict
sonic-rs/serde comparator evidence, and Section 0.5 guard preservation.

For W8 accounting, the honest state is candidate exhaustion:

- W4 control candidate: attempted only on `random`; measured FAIL.
- W5 string-span candidate: blocked before implementation; no reusable proof.
- W7 digest/host-sink candidate: blocked before implementation; no legal
  residual row can clear both direct floors.
- Current guards: typed guards for `twitter`, `github_events`, and
  `update_center` hold; `random` depends on the global Section 0.5 guard set
  for any future attempt.
