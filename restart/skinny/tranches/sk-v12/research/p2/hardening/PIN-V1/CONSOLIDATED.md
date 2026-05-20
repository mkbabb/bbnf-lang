# SK-V12 S-P2 PIN-V1 Consolidated Challenge

Pass: S-P2 Research. Cycle: PIN-V1.
Date: 2026-05-20.
Status: REVISE; folded into P2 Cycle V2.

PIN-V1 reviewed the pin-aware S-P2 research cohort committed at `8017a90b`
against `PASS-2-RESEARCH.md` and the SK-V12 user pin. Four lenses accepted and
two lenses required revision, so this cycle does not count toward §3Z
convergence.

| Lens | Verdict | Score | Blocking result |
|---|---:|---:|---|
| CH1 correctness | REVISE | 86 | P2-C mixed inventory-only ARM rows into the candidate pool; P2-A/P2-B external anchors were stale; P2-C ISA citations needed concrete section labels. |
| CH2 generality / Lock 14 | ACCEPT | 96 | Grammar-neutrality boundaries were coherent. |
| CH3 regression / REDRESS | ACCEPT | 96 | Union and ASM-gen category unblocks preserved material-differential and REDRESS history. |
| CH4 cost / scalar-reference / checkasm | REVISE | 78 | P2-A and P2-E lacked uniform per-row parity, consumer, micro-proof, and orphan disposition; pre-pin convergence marker was stale. |
| CH5 hidden coupling / substrate | ACCEPT | 96 | Lock 1 substrate boundary and same-tape rule were preserved. |
| CH6 anti-paper-close | ACCEPT | 96 | CSS L4 absence was routed, not paper-closed. |

Fold applied:

- Updated all six P2 artifacts to Cycle V2.
- Expanded P2-A candidate accounting with per-row class, scalar reference,
  checkasm/parity, micro-proof, same-wave consumer, Lock 16 prerequisite, orphan
  disposition, and P1 antecedent.
- Replaced stale yyjson and dav1d anchors with resolving current upstream
  anchors.
- Added a P2-C candidate/inventory split. Only C1/C3/C4/C5/C6 are selectable
  from PIN-V1; C2/C9/C11 are inventory/drop; C7/C8/C10/C12 are support-only
  until a folded pass supplies a same-wave consumer and required prerequisites.
- Added concrete Arm section labels for TBL/TBX, UDOT/dotprod, LD4, PMULL,
  CSSC, and SHA3 EOR3/BCAX claims.
- Replaced P2-E's table with explicit parser, support, output-plane, and
  oracle/accounting classes plus checkasm/parity and orphan/Lock 16 disposition.
- Replaced the stale `HARDENING-S-P2-CONVERGED.md` marker with a pin-aware
  in-progress status that routes to PIN-V2.

Advancement: dispatch PIN-V2 challenge against the folded V2 packet.
