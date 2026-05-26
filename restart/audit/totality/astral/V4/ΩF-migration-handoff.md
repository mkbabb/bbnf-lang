# Omega-F Migration + Handoff - Pass Omega V4 W4R

Pass: Pass Omega V4.
Date: 2026-05-26.
Scope: HANDOFF and MIGRATION updates implied by REDRESS-184.

## Verdict

ACCEPT-WITH-PATCH.

W4R must be recorded as the next dispatch authority after the already-landed W2
and W3 closes. Current implementation dispatch is blocked at W4 until
G-Omega V4 closes and CRUD applies the amended wave graph.

## Handoff Patch Intent

`restart/HANDOFF.md` should change from the V3 W2R state to:

- Pass Omega V3 W2R CRUD closed.
- W2 amended skinny-side `regen-css` admitted at `45568e669`.
- W3 production CSS corpus admitted at `b0a864f0b`.
- W4 rejected at `4a32db45c` / REDRESS-184 because provider deletion removes
  the emitter before W5 replacement exists.
- Next move is Pass Omega V4 G-Omega; after authorization, apply W4R CRUD and
  rerun W4 as ledger-only PRUNE.
- Replace stale "blocked until amended W2 re-admits" wording. That state is
  historical after `45568e669`.
- Normalize the SK-V14 SPEC line count if the handoff cites it, and normalize
  W4/W8 CSS wording to 24 operational CSS L4 row keys.

`restart/skinny/tranches/sk-v14/HANDOFF.md` already points to W4R after
commit `4a32db45c`; CRUD should only align it with the final G-Omega packet.

## Migration Patch Intent

Append a Pass Omega V4 receiver after MIGRATION §0.2:

- W4 current shape rejected by REDRESS-184.
- W4 after amendment owns rolling delta / REDRESS ledger prune only.
- W5 owns CSS provider/template deletion in the same wave as the generic
  provider replacement and `regen_css.rs` migration.
- W6.0 remains CSS L4 root-runtime collapse.
- The V3 W2 rejection block is historical/superseded: W2R admitted at
  `45568e669`; W3 admitted at `b0a864f0b`; current block is W4R.

## Next Dispatch Directive

After G-Omega V4 closes and CRUD applies:

1. Dispatch amended W4 as ledger-only PRUNE-2.
2. Dispatch W5 only after W4 ledger close; W5 owns provider/template deletion
   and replacement in one source slice.
3. Continue W6/W7 then new-admit waves only after PRUNE-1..PRUNE-5 close.

Until G-Omega V4 closes, W5 and all later waves remain blocked.
