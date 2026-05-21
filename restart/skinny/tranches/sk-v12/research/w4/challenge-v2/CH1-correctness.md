# SK-V12 W4 CHALLENGE V2 - CH1 Correctness

Verdict: REVISE.

The exact `find_ascii_set_member64` / CSS `scan_block` route is semantically
sound. Current `scan_block` treats `{`, `;`, and `}` as structural delimiters,
so replacing byte-by-byte non-delimiter advance with "jump to next member of
`b\"{};\"`" is equivalent if cursor/end/tail parity is proven.

Blocking revisions:

1. The W4 report/gate contract is still too weak for strict fact-stream
   equality. SPEC requires the equality proof itself to be gate-consumed with
   provenance, checksums, artifacts, and run id. PLAN-V3 must add W1b-style
   post-W4 Track 1, cssparser, and lightningcss fact artifacts/digests.

2. Caller parity is required in prose, but the command block must name the new
   caller-level checkasm/parity test explicitly for both reject and production
   branches.

3. A5's `skip_ws_and_comments` / layout run-skip framing must be marked
   superseded for W4 redress; PLAN-V3 selects delimiter member-find.

W2 prerequisite is satisfied by REDRESS-122 and correctly rerun-gated for W4.
