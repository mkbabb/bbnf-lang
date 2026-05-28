# SK-V15 S-P3 V2 CH7 OVERFIT-PRUNE / GATE-EXCLUSION

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V2. Lens: CH7.
Date: 2026-05-28.
HEAD: `39e186ee3`.
Scope: audit the V2 S-P3 packet for overfit routes, hidden broadcasts,
self-exempting gates, substrate sidecars, FNV migration, and stale diagnostic
evidence promoted to admission.

## Verdict

ACCEPT.

The V2 packet incorporates the overfit-prune addenda from V1. It makes the
W8R CSS tuple a diagnostic negative fixture, requires Lock 14/16 exclusion
reports to be consumed by gates, keeps EventTape inside the five-shape
BackendShape canon, and quarantines FNV to bench-only proof. No CH7 blocker
remains in the active P3-A..P3-F/SPEC/DISPATCH packet.

## Findings

| id | status | evidence | disposition |
|---|---|---|---|
| CH7-V2-01 | ACCEPT | Hidden broadcast admission is rejected at schema and wave gates. P3-C marks the W8R tuple diagnostic-only (`p3c-falsifiability-gates.md:27-38`), W1 consumes `broadcast_group_id` and rejects clone rows (`p3c-falsifiability-gates.md:111-126`), and DISPATCH rejects hidden broadcast measurements (`DISPATCH-PROMPT.md:248-250`). | Preserve as W0/W1 gate requirement. |
| CH7-V2-02 | ACCEPT | Gate exclusion cannot self-exempt. P3-C defines included roots, excluded roots, reason, owner, self-scan status, primitive status, gate consumer, affected rows, and disposition (`p3c-falsifiability-gates.md:71-89`). SPEC makes missing/self-exempting exclusions close-rejecting (`SPEC.md:119-122`, `SPEC.md:202-209`). | No revision. |
| CH7-V2-03 | ACCEPT | EventTape cannot reopen retained sidecars: SPEC bans sidecar vector, class column, sixth shape, public `UnionTape`, retained stream, public substrate API, and alternate projection (`SPEC.md:202-209`), then repeats the W9 five-shape gate (`SPEC.md:363-378`). P3-E repeats the same pre-block (`p3e-preblocked-ledger.md:213-225`). | No revision. |
| CH7-V2-04 | ACCEPT | FNV closed-enum migration is blocked. SPEC assigns quarantine to W10 and blocks production role (`SPEC.md:380-394`); P3-C requires production FNV scan plus adversarial semantic fixtures (`p3c-falsifiability-gates.md:275-291`); P3-E records production migration as pre-blocked (`p3e-preblocked-ledger.md:227-243`). | No revision. |
| CH7-V2-05 | ACCEPT | Pre-block coverage is normalized across active dispatch surfaces. The shared list includes `28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, and FNV closed-enum production migration` in P3-C, P3-E, SPEC, and DISPATCH (`p3c-falsifiability-gates.md:313-328`, `p3e-preblocked-ledger.md:32-37`, `SPEC.md:414-431`, `DISPATCH-PROMPT.md:238-240`). | No revision. |
| CH7-V2-06 | ACCEPT | x86/AVX-512, PMULL hot-body, CSSC CTZ promotion, retained sidecars, public `UnionTape`, density tables, second tapes, numeric/digit old framing, and W8R positive proof are all rejected in P3-C/P3-E/SPEC (`p3c-falsifiability-gates.md:313-323`, `p3e-preblocked-ledger.md:45-55`, `SPEC.md:414-431`). | No revision. |

## Verification

Commands run:

```sh
rg -n "broadcast|self-exempt|gate_exclusion|EventTape|FNV|Track 1|Track 2|W8R|sidecar|UnionTape|x86|PMULL|CSSC" restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
git rev-parse --short HEAD
```

Result: matches are rejection, quarantine, or gate-consumer clauses; no
positive overfit route was found.
