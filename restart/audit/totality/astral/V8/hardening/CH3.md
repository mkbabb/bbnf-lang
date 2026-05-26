# Pass Omega V8 CH3 Regression

Date: 2026-05-26.
Lens: CH3 regression.
Scope: Omega-A through Omega-F, `locks-diff.md`, `master-plan-diff.md`,
REDRESS-209/210/211/212, and the W5B-FRONTEND V2 challenge archive.
Disposition: ACCEPT.

## Verdict

ACCEPT. V8 does not reopen the CH3 regression routes. It preserves the
REDRESS-209/210/211 sequence, classifies REDRESS-212 as a SPEC wave-graph and
cap-accounting correction, and blocks W5C-GEN, W5D-DELETE, W6, W7, and
W8/W9/W10 until their amended predecessors close.

No additional CH3 fold is required.

## Regression Matrix

| Regression route | CH3 result | Exact fold checked |
|---|---:|---|
| Static centralization | ACCEPT | REDRESS-209 remains the historical rejection of static provider/template centralization: `skinny/REDRESS.md:5171`-`5193`. Omega-B carries the lesson that a moved or centralized static body is not a replacement generator, and Omega-E says to keep the no-static-centralization constraints: `restart/audit/totality/astral/V8/ΩB-skinny-lessons.md:25`, `restart/audit/totality/astral/V8/ΩE-skinny-corpus.md:161`-`165`. |
| Premature provider deletion | ACCEPT | REDRESS-210 remains closed: deletion before a provider-free generator body exists is rejected at `skinny/REDRESS.md:5195`-`5215`. V8 keeps W5D-DELETE behind W5C-GEN and forbids provider/template deletion in W5B: `restart/audit/totality/astral/V8/master-plan-diff.md:40`-`42`, `restart/audit/totality/astral/V8/master-plan-diff.md:77`, `restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:112`. |
| Provider-free generation before frontend closure | ACCEPT | REDRESS-211 remains closed: provider-free generation is split behind generic frontend/import/IR closure at `skinny/REDRESS.md:5217`-`5247`. V8 keeps W5C-GEN blocked until aggregate W5B-FRONTEND close and assigns W5C-GEN, not W5B, to provider-free runtime generation: `restart/audit/totality/astral/V8/master-plan-diff.md:25`-`26`, `restart/audit/totality/astral/V8/master-plan-diff.md:78`, `restart/audit/totality/astral/V8/ΩB-skinny-lessons.md:47`-`52`. |
| W5B one-wave cap violation | ACCEPT | REDRESS-212 rejects the V7 one-wave W5B-FRONTEND shape at `skinny/REDRESS.md:5249`-`5275`. V8 folds the fix by replacing the one-slot cap with formal W5B.0 through W5B.4 sub-waves, <=30 min each and <=150 min aggregate, with W5B closing only after W5B.4: `restart/audit/totality/astral/V8/master-plan-diff.md:30`-`39`, `restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:60`-`73`, `restart/audit/totality/astral/V8/ΩF-migration-handoff.md:55`-`63`. |
| New-admit PRUNE bypass | ACCEPT | REDRESS-209, 210, 211, and 212 all keep W8/W9/W10 blocked by the PRUNE chain: `skinny/REDRESS.md:5186`-`5187`, `skinny/REDRESS.md:5212`-`5213`, `skinny/REDRESS.md:5245`-`5247`, `skinny/REDRESS.md:5274`-`5275`. V8 repeats that W7 and W8/W9/W10 remain blocked by the full PRUNE chain: `restart/audit/totality/astral/V8/master-plan-diff.md:43`, `restart/audit/totality/astral/V8/ΩF-migration-handoff.md:17`-`18`. |

## Challenge Fold Check

The W5B-FRONTEND V2 challenge left CH1, CH2, CH4, CH5, and CH6 as REVISE, while
CH3 and CH7 accepted. V8 addresses only the CH3 regression question here:

- CH3 V2 already accepted the REDRESS-209/210/211 ordering and found no
  W5C/W5D borrowing: `restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH3.md:13`-`28`.
- CH4's cap contradiction is routed to SPEC/Omega sub-waves rather than hidden
  as informal internal slices: `restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH4.md:9`-`13`,
  `restart/audit/totality/astral/V8/master-plan-diff.md:48`-`52`.
- CH5's Lock14-only first checkpoint is folded as W5B.0 LOCK14-GATE before any
  W5B frontend/codegen/xtask source owner-path changes:
  `restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH5.md:36`-`38`,
  `restart/audit/totality/astral/V8/ΩC-locks-amendments.md:75`-`82`.
- CH6's maintain conflict is not papered over: V8 requires either
  SPEC-authorized exact no-diff for the non-admit sequence or fresh full-table
  maintain evidence in W5B.4:
  `restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH6.md:33`-`37`,
  `restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:78`-`91`.

## Required CH3 Folds

None. Carry the V8 folds exactly:

1. Replace one-shot W5B-FRONTEND authority with W5B.0 LOCK14-GATE, W5B.1
   IMPORT-CLOSURE, W5B.2 LAYOUT-DISCARD, W5B.3 PRETTY-SPAN-PROJECTION, and
   W5B.4 REQUEST-CONSUMER.
2. Dispatch W5B.0 first; W5B.0 is Lock14-only, does not edit frontend source,
   does not close W5B-FRONTEND, and does not unblock W5C-GEN.
3. Keep W5C-GEN blocked until aggregate W5B-FRONTEND close at W5B.4.
4. Keep W5D-DELETE blocked until W5C-GEN admits; keep W6 blocked until
   W5D-DELETE admits.
5. Keep W7 and W8/W9/W10 blocked by the full PRUNE chain.
6. Preserve the zero-delta locks posture: no new public syntax, BIR,
   BackendShape, substrate surface, or lock.
