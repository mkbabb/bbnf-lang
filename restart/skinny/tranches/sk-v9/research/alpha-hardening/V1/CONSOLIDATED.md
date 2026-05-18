# SK-V9 Alpha Hardening V1 Consolidated

Date: 2026-05-18.

Scope: six-lens challenge over the SK-V9 Pass Alpha packet after commit
`ba1bb23d` (`docs(sk-v9-alpha): materialize pass alpha contract from SK-V8
close`).

## Verdict

V1 outcome: REVISE.

| Lens | Verdict | Confidence | Disposition |
|---|---|---:|---|
| CH1 Correctness | REVISE | 94% | Folded. Threshold arithmetic, citation anchors, candidate scope, and Alpha-D head wording required correction. |
| CH2 Generality | REVISE | 88% | Folded. Lock 14 proof, grammar-aware telemetry, sidecar evidence-only scope, and candidate-local strictness boundaries required correction. |
| CH3 Regression | REVISE | 92% | Folded. Alpha-C historical ledger and REDRESS 73 had to become G-Alpha-facing pre-blocks. |
| CH4 Cost | REVISE | 86% | Folded. Alpha cost matrix, candidate status, hard caps, and proof-only retained route were required. |
| CH5 Hidden Coupling | ACCEPT | 96% | No fold required. |
| CH6 Next-Tranche Impact | ACCEPT | 94% | Procedurally not converged because confidence is below 95%, but no unique fold required. |

Nominal ACCEPT rate: 2/6. Minimum confidence: 86%. This fails the Alpha
convergence threshold. V2 re-challenge is required after the folds below.

## Required Folds Applied

1. Correctness folds:
   - `alpha-E` now uses `apache_builds/parse_only >=15368` for the optional
     retained implementation guard.
   - `alpha-E` typed maintain floors now match the master maintain floors:
     `twitter >=15027`, `update_center >=11719`, `mesh >=9431`, and
     `marine_ik >=11548`.
   - Alpha-B through Alpha-F now carry tighter source anchors for row authority,
     W6 close, REDRESS 91-93, and strict comparator boundaries.
   - Alpha-D now says `SK-V8 close head` instead of `HEAD for this ledger`.

2. Scope and cost folds:
   - `SYNTHESIS.md`, `HANDOFF.md`, and Alpha-F now distinguish three W6
     behavior candidates from two non-behavior gate prerequisites.
   - `SYNTHESIS.md` and `HANDOFF.md` now carry an Alpha cost matrix with LOC
     budgets, risk, downstream alignment, same-wave consumer, <=90 min hard cap,
     split-before-dispatch rule, and expected row effect.
   - The retained class/event route is proof-only at Alpha depth. It cannot move
     parse rows unless a later capped S-P3 wave includes a same-wave generated
     retained Track 1 consumer and passes challenge.

3. Lock 14 and comparator folds:
   - `SYNTHESIS.md` now includes an Alpha Generality and Lock 14 gate covering
     public API scan, grammar branch scan, primitive/table scan, role/fact
     boundary, template/provider boundary, and CSS L4 / Sheets / BBNF-self
     non-JSON proof.
   - `SYNTHESIS.md` telemetry now includes grammar-aware registry fields:
     `grammar_id`, `domain`, `comparator_id`, `comparator_plane`,
     `comparator_strictness`, `comparator_freshness`, and
     `measured_validation_path`.
   - Alpha-E now states candidate-local strictness, `parse_utf8`, and
     `escape_complete` boundaries.
   - The comparator manifest candidate is now evidence ingestion only. It cannot
     produce parser data, retained tape data, row output, substrate, or strict
     admission by itself.

4. Regression folds:
   - Alpha-C is now listed in the SK-V9 authority/read-first surfaces.
   - Alpha-C's prior pre-block ledger is binding by reference in
     `SYNTHESIS.md` and `HANDOFF.md`.
   - REDRESS 73 is now carried in Alpha-C, Alpha-E, `SYNTHESIS.md`,
     `HANDOFF.md`, and Alpha-F as a pre-block against generated-helper-shape
     transfer to hand Track 2 or control paths.

## V2 Re-Challenge Target

V2 must review the folded packet, not the original `ba1bb23d` text. The V2
challenge target is acceptable only if:

- all six lenses return ACCEPT;
- minimum confidence is >=95%;
- no open critical defect or orphan REVISE remains;
- the G-Alpha boundary remains intact; and
- no `SPEC.md`, `DISPATCH-PROMPT.md`, or SK-V9 implementation wave is created
  before G-Alpha.
