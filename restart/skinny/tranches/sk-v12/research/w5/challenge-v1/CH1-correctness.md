# SK-V12 W5 CHALLENGE CH1: Correctness

VERDICT: ACCEPT

PLAN.md correctly selects `PASS-ADMIT`. SPEC Section 0.1 requires generated CSS
L4 Track 1 to be strictly greater than `lightningcss_mbps + 1` with same-plane
strict equality and gate-consumed provenance. The W1b-2b report row has Track 1
`429.34420791225705 Mbps`, lightningcss `168.92962215656692 Mbps`, threshold
`169.92962215656692 Mbps`, margin `259.41458575569015 Mbps`, and equality
`pass:track1=cssparser=lightningcss`.

SPEC Section 10 says W3 is required only for FIXPOINT or when no prior CSS row
satisfies ADMIT. REDRESS-125 records the W1b-2b measured ADMIT candidate, and
REDRESS-126 records W4 orphan count `0`.

Required changes: none.
