# CH4 - Rejected/Routed Behavior-Wave Review

Verdict: ACCEPT
Confidence: 97%

Evidence:

- Target `e500ad00` adds the W6 close packet and V1 accept cycle. V1 consolidated already accepted CH4's rejected/routed posture for W2 row-table rejection, W3 pre-redress fit-gate rejection, and W4 selected-row falsification plus rejected patch handling (`restart/skinny/tranches/sk-v8/research/wave-6-hardening/V1/HARDENING-W6-V1-CONSOLIDATED.md:36-38`).
- W2 rejected behavior is not treated as admitted. The close packet says W2 admitted source/product parity only, not measured row-table expansion, and that `RESULTS.md` is unchanged with Apache/CITM as source/product rows only (`restart/skinny/tranches/sk-v8/research/skv8-W6-close-and-alpha-feedback.md:15`, `restart/skinny/tranches/sk-v8/research/skv8-W6-close-and-alpha-feedback.md:29`). REDRESS 91 states Apache/CITM are not measured rows, rejects `canada/real_typed_struct`, and rejects W2 benchmark row-table admission (`skinny/REDRESS.md:2622-2652`).
- W3 remains rejected/routed, not admitted source work. The close packet records W3 as rejected/routed with no source, patch artifact, or row-table admission (`restart/skinny/tranches/sk-v8/research/skv8-W6-close-and-alpha-feedback.md:16-17`, `restart/skinny/tranches/sk-v8/research/skv8-W6-close-and-alpha-feedback.md:30`). REDRESS 92 records pre-redress fit-gate rejection, no source or rejected patch artifact, no W3 row-table admission, and unchanged `skinny/RESULTS.md` (`skinny/REDRESS.md:2663-2686`).
- W4 row gates and rejected patch handling are accurate. The W4 plan names selected-row floors Apache 8048 Mbps, numbers 7230 Mbps, and random 7401 Mbps, and requires revert plus REDRESS if any selected row misses (`restart/skinny/tranches/sk-v8/research/skv8-W4-plan.md:48-53`, `restart/skinny/tranches/sk-v8/research/skv8-W4-plan.md:84-89`). REDRESS 93 records the same floors, Criterion falsification, no source admission, unchanged `RESULTS.md`, and rejected patch path `/tmp/skv8-wave4-track2-scalar-fold-rejected.patch` (`skinny/REDRESS.md:2694-2729`).
- No new V1 drift found: `git diff --exit-code HEAD -- skinny/RESULTS.md skinny/REDRESS.md` passed, manifest count remains `manifest_rows=38` and `real_typed_rows=4`, and `/tmp/skv8-wave4-track2-scalar-fold-rejected.patch` exists.

Required fold: None.
