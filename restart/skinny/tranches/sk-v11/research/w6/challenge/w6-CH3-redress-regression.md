# SK-V11 W6 CH3 - REDRESS Regression Review

Pass: W6 mandatory CHALLENGE.
Lens: CH3 regression / REDRESS.
Date: 2026-05-20.
Disposition: REVISE.

## Verdict

REVISE.

The W6 plan's material differential is not sufficient. The selected
implementation reopens the rejected direct string/hash family, especially
REDRESS 54, and it is adjacent to the REDRESS 55 / 66 / 69 conclusions in the
same way the ledger explicitly warns against.

The blocking sentence in the plan is the implementation candidate:

> override `JsonDigestSink::{key_source,string_source,array_string_source,object_string_source}`
> so generated Track 1 routes escaped raw string slices to a local decoded-byte
> fold rather than allocating a decoded `String`/`Cow<str>` and then hashing it.

That is not a new REDRESS-differentiated route. REDRESS 54 already kept the
`JsonSink::*_source` seam and replaced escaped-string allocation in
`JsonDigestSink` with exact decoded length plus exact `hash_bytes` computation.
It was correctness-green and rejected by measurement on escaped direct rows.
The W6 plan names the same seam, the same output-plane digest fields, and the
same allocation-removal claim. "Folds decoded bytes only into the
already-existing `JsonDirectDigest` fields" is a description of the REDRESS 54
failure shape, not a material differential from it.

## REDRESS Assessment

REDRESS 54: REOPENED.

The plan selects sink-local decoded byte folding inside `JsonDigestSink`
source-method overrides. REDRESS 54 rejected sink-local exact decoded
stats/hash under the same direct digest workload. The plan does not change the
consumer representation, the output contract, or the cost model enough to avoid
that prior result.

REDRESS 55: not an exact quote-source replay, but still implicated.

The plan does not move the parser to quote-source hooks, so it is not literally
the REDRESS 55 patch. But REDRESS 55's conclusion is broader: another
sink-local decoded hash path is non-canonical unless a before/after row
overturns the allocate-then-contiguous-hash baseline. W6 currently proposes
that same sink-local decoded hash path without a new consumer representation.

REDRESS 66: partially implicated.

The plan does not add new generated direct source hooks, which avoids the exact
REDRESS 66 receiver shortcut. It still uses the direct source-hook boundary as
the intervention point and tries to win by changing what the digest sink does
after receiving raw escaped source. REDRESS 66 says the next admissible direct
route must materially change escaped-string materialization without repeating
REDRESS 54/55. This plan repeats REDRESS 54.

REDRESS 67 and REDRESS 68: avoided.

The plan does not thread parser-owned decoded scratch through generated parser
control and does not rewrite `unescape_string` / `Cow<str>` as a byte-output
materializer. Those two specific replay risks are controlled.

REDRESS 69: adjacent, insufficiently bounded.

The plan does not add DirectBuild semantic string facts, so it is not the exact
REDRESS 69 patch. But it still turns escaped source into decoded length /
fingerprint facts for the current direct digest workload. REDRESS 69 rejected
that cost class and says the next admissible move must change the consumer
representation, not merely stream semantic hash facts for the same digest.

REDRESS 64, 82, 107, and 108: controlled.

The plan correctly does not claim fixed-width Unicode validation, single-quartet
materializer production, x4 proof admission, or existing `unescape_string`
production reuse. That part of the material differential is sufficient.

REDRESS 83: controlled.

The plan does not rely on retained string-block widening or retained tiny-string
evidence.

REDRESS 113 and REDRESS 116: carried, not resolved.

The plan honestly carries the non-JSON block and W5 span-block forward. It does
not claim to close either axis. That is acceptable for CH3, but it leaves W6 as
a JSON direct-only attempt.

## Required Changes

Before CH3 can accept W6 redress, revise the plan to remove the REDRESS 54
replay as the selected implementation.

Required plan changes:

1. Remove `JsonDigestSink::*_source` decoded-byte folding into the current
   `JsonDirectDigest` length/fingerprint fields as the W6 implementation
   candidate.
2. Explicitly cite REDRESS 54 as the exact antecedent and state that sink-local
   decoded stats/hash over the current direct digest workload is pre-blocked.
3. If W6 remains a JSON direct wave, select a true new escaped-segment source
   delta plus a same-wave product consumer whose representation is not the
   current direct digest length/fingerprint contract. The revised plan must
   name the representation change and explain why it is not REDRESS 54, 55, 66,
   or 69.
4. If no such product consumer exists inside SPEC Section 10 authority, record
   W6 as BLOCKED / REJECT before behavior source redress rather than running a
   known REDRESS replay.
5. Keep the existing controls for REDRESS 64, 82, 107, 108, 113, and 116 in the
   revised plan: no x4 proof-to-production claim, no `unescape_string` relabel,
   no non-JSON proof without generated Track 1 authority, and no W5 span reuse.

CH3 does not require a different row target. `unicode_mixed/direct_to_struct`
is the only plausible W6 row under the current floors. The blocker is not row
selection; it is that the selected implementation route is already falsified.
