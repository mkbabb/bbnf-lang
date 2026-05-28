# SK-V15 S-P2 V3 CH5 - HIDDEN COUPLING CONFIRMATION

Verdict: ACCEPT.

## Evidence Checked

- `restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:10`-`58`
- `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:83`-`115`
- `restart/skinny/tranches/sk-v15/research/p2/hardening/V2/CH5.md`
- Lock 1 substrate-union commitments via `restart/locks/LOCKS.md`.

## Findings

1. P2-D states the structural projection and tape are one substrate and forbids a parallel retained structural index, cursor list, class lane, whitespace bitmap, aux density table, or second document projection (`restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:10`-`12`).

2. The capacity candidate is same-substrate only. Its allowed signals are input length, generated metadata, emitted-count feedback, or same-loop accounting, while second scans, pre-scan capacity oracles, retained capacity sidecars, and parallel source passes are rejected (`restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:36`-`41`).

3. P2-D's explicit non-candidate list still catches the hidden-coupling families that caused V1 concern: retained structural-position vectors, streaming cursors, class columns, whitespace bitmaps, density/projection tables, decoded-byte sidecars, and public `UnionTape` shapes (`restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:50`-`58`).

4. P2-F keeps accepted same-tape operations bounded to existing tape, direct sink, or admitted fact output and rejects retained sidecars (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:87`-`91`, `:104`-`115`).

## V2 Confirmation

V2 CH5 accepted the folded no-second-scan wording. No new hidden substrate, sidecar, or Track 1 / Track 2 conflation appears in V3.
