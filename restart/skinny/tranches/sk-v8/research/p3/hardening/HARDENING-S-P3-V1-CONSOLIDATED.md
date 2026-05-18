# SK-V8 S-P3 Hardening V1 Consolidated

Date: 2026-05-18.
Pass: S-P3 Synthesis-Plan.
Cycle: V1.

## Verdict

REVISE.

Acceptance: 4/6 verdict-level ACCEPT; 2/6 qualifying ACCEPT at or above 95%
confidence.

V1 does not qualify for S-P3 convergence. CH1 and CH4 returned REVISE. CH2
and CH6 returned ACCEPT below the 95% confidence floor. CH3 and CH5 returned
qualifying ACCEPT.

## Lens Results

| Lens | Verdict | Confidence | Disposition |
|---|---|---:|---|
| CH1 correctness | REVISE | 93% | Fold required. |
| CH2 generality | ACCEPT | 94% | Non-qualifying ACCEPT; no blocking fold. |
| CH3 regression | ACCEPT | 95% | Qualifying ACCEPT. |
| CH4 cost | REVISE | 91% | Fold required. |
| CH5 hidden coupling | ACCEPT | 96% | Qualifying ACCEPT. |
| CH6 anti-paper-close | ACCEPT | 94% | Non-qualifying ACCEPT; no blocking fold. |

## Required V2 Folds

1. W2 typed-row gate: fold P3-C's candidate typed seed table into the live
   SPEC and dispatch surface. W2 may select only from that seed table unless a
   later accepted S-P3 revision expands it. The gate must state
   `Track 1 Mbps >= ceil(sonic-rs strict Mbps / 1.10)`, recomputed from
   `SK-V8-open` if W0 refreshes a same-run strict anchor.
2. W2 maintain gates: preserve the four current real-typed GO rows with both
   sonic GO floors and no-regression floors; preserve existing direct GO rows
   as guard rows.
3. Cost governance: add explicit source/edit LOC budgets for W0-W6 to the live
   SPEC and mirror them in `DISPATCH-PROMPT.md`. Budgets may distinguish
   production source, report/gate/schema/test, generated-output, RESULTS, and
   REDRESS edits, but every wave needs an objective budget.
4. W3 feasibility: add a pre-redress split gate requiring the exact W3 plan to
   estimate touched source LOC, generated LOC, test LOC, gate/report LOC, and
   revert-slice size. If the estimate exceeds the W3 LOC budget or the
   90-minute implementation/redress cap, W3 must split before redress or return
   REVISE.
5. Citation hygiene: refresh volatile P3 path:line citations against the folded
   SPEC/DISPATCH or replace them with stable section references. The V2 packet
   must not cite a line range that points to the wrong section.
6. Future-artifact links: keep future wave artifact names as naming patterns in
   prose, not unresolved local markdown links or globs.

## Accepted Boundaries Preserved

- S-P2 V6/V7 convergence authorizes S-P3 only, not implementation, G-Alpha,
  or W3 redress.
- G-Alpha remains required before implementation; `G-Alpha closed` initially
  dispatches W0 only.
- Strict-vs-strict comparator discipline remains intact.
- Lock 14 grammar neutrality and non-JSON proof obligations remain intact.
- W3 remains Tier A tape plus structural-projection union only, with one
  retained `Tape`, no Tier B string-boundary/parity/CostFacts-template work,
  no `tape_vs_tape` production consumer, and no sidecar/parser-owned cursor.
- No new directive, BIR variant, substrate surface, `BackendShape`,
  `UnionTape`, public substrate API, or parallel substrate is authorized.
- Pre-blocked REDRESS routes remain closed unless a future accepted plan supplies
  fresh W0 evidence, same-wave consumer, no-regression gate, REDRESS citation,
  scalar/checkasm where relevant, and challenge acceptance.

## V2 Entry

V2 must fold the required items above before re-challenge. V1 hardening alone
does not authorize S-P3 convergence, PASS-ALPHA, G-Alpha, W0, or any SK-V8
implementation wave.
