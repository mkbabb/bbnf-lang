# SK-V12 W5 CHALLENGE CH6: Anti-Paper-Close

VERDICT: ACCEPT

W5 points final close at the consumed W1b-2b CSS report gate
`sk-v12-css-l4-sota-v1`, not prose. The report row is `gate_status=pass`,
`admission_status=PASS-ADMIT-CANDIDATE`, Track 1
`429.34420791225705 Mbps`, lightningcss `168.92962215656692 Mbps`, threshold
`169.92962215656692 Mbps`, margin `259.41458575569015 Mbps`, strict
three-way equality `pass:track1=cssparser=lightningcss`, and fact SHA
`caf97bee6e413157e6114985bc1108bc3a8fbf597a1e519b3ccff905d2e5236c`.

REDRESS-125 confirms the gate recomputes throughput from live Criterion lanes,
consumes retained Track 1/cssparser/lightningcss artifacts, rejects stale or
report-only evidence, and leaves final campaign close to W5.

REDRESS-126 honestly routes the ASM remainder: W4 is
`ROUTE-PRODUCTION-SPLIT`, not CSS ADMIT or production SIMD/ASM admission; the
passing delimiter microbench is routed to a future production/gate split. The
orphan remainder is explicit: five rows are dispositioned as
`inventory_demoted_with_evidence`, selected candidate accounted separately,
final orphan count `0`.

Required changes: none.
