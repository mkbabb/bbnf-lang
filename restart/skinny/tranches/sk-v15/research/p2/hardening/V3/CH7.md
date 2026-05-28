# SK-V15 S-P2 V3 CH7 - OVERFIT-PRUNE / GATE-EXCLUSION CONFIRMATION

Verdict: ACCEPT.

## Evidence Checked

- `restart/skinny/tranches/sk-v15/SYNTHESIS.md:100`-`110`
- `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:20`-`22`, `:104`-`117`
- `restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:50`-`58`
- `restart/skinny/tranches/sk-v15/research/p2/hardening/V2/CH7.md`

## Findings

1. The active CH7 addendum requires Lock 14 / Lock 16 gates to scan and report their own exclusion lists (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:100`-`110`). The S-P2 packet does not author an executable grep gate, but it does require S-P3 to preserve reported-exclusion discipline for any such gate.

2. Diagnostic surfaces are visible and quarantined. P2-F lists rejected numeric/digit surfaces, EOB inventory, PMULL/CSSC promotions, retained string/cursor replays, schema builders, harness hashes, and x86 routes with explicit reasons (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:104`-`115`).

3. CSS overfit is not hidden as an admission path. P2-F says CSS L4 can serve only as a required generalisation target until the provider and comparator plane are repaired (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:20`-`22`, `:117`).

4. Hidden exclusion-like substrate families remain non-candidates. P2-D names retained structural-position vectors, streaming cursors, class columns, whitespace bitmaps, density/projection tables, decoded-byte sidecars, and public `UnionTape` shapes as non-candidates (`restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:50`-`58`).

## V2 Confirmation

V2 CH7 accepted the packet with a watch point for S-P3. V3 confirms the watch point remains routed and no hidden overfit exemption has appeared.
