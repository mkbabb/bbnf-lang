# SK-V12 Pass Alpha CHALLENGE V1 - CH2 Generality / Lock 14

Date: 2026-05-20.
Lens: CH2 generality / Lock 14.
Scope: Pass Alpha SK-V11 -> SK-V12 alpha A-F, SK-V12 SYNTHESIS/HANDOFF,
SK-V11 close-redress, and `skinny/REDRESS.md` through REDRESS 120.

## Disposition

REVISE.

The alpha packet correctly makes generated non-JSON work the first material
SK-V12 problem, blocks JSON-only direct retries, rejects Lock 14 prose proof,
and forbids JSON policy in generic crates. The blocking CH2 defect is narrower:
the fallback baseline path is not matched by a fallback intervention path, and
the known JSON-profiled codegen blocker is not yet turned into an executable
baseline pre-gate for the selected non-JSON grammar.

## Sources Read

- `restart/prompts/pass-contracts/PASS-ALPHA.md` Section 3 and bbnf-lang axes.
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`.
- `restart/skinny/tranches/sk-v12/HANDOFF.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-A-results-extraction.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-B-competitor-deltas.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-C-redress-digest.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-D-validated-invalidated.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md`.
- `restart/skinny/tranches/sk-v11/research/close/close-redress.md`.
- `skinny/REDRESS.md` through REDRESS 120.

## Disposition Matrix

| Surface | Disposition | CH2 rationale |
|---|---|---|
| Alpha-A results extraction | ACCEPT | It preserves the SK-V11 close state as unchanged `N-direct / NoGo`, treats parse-only as diagnostic, and carries the generated non-JSON baseline block forward rather than converting JSON rows into generality proof (`alpha-A-results-extraction.md:15-40`, `:143-157`). |
| Alpha-B competitor deltas | ACCEPT | It keeps comparator evidence plane-correct and explicitly states no non-JSON grammar-domain comparator rows exist in the SK-V11 close authority (`alpha-B-competitor-deltas.md:30-58`, `:164-178`). |
| Alpha-C REDRESS digest | ACCEPT | It correctly makes REDRESS 112/113 the generated non-JSON block, REDRESS 119 the direct residual fixpoint authority, and pre-blocks JSON-only micro-waves before the generated non-JSON baseline (`alpha-C-redress-digest.md:25-32`, `:100-157`). |
| Alpha-D validated/invalidated ledger | ACCEPT | It demotes W1a to an evidence lane only, keeps generated non-JSON baseline as the primary unresolved axis, and forbids generated Track 1 through `json_provider::ensure_runtime_profile` (`alpha-D-validated-invalidated.md:98-107`, `:121-141`, `:253-271`). |
| Alpha-E candidate shortlist | REVISE | E1/E2/E3 name CSS, Sheets, and BBNF-self baseline candidates, but only E4 names a measured intervention and it is CSS-only. If CSS remains blocked and E2 or E3 becomes the admitted baseline, Alpha-E has no corresponding grammar-neutral intervention candidate to satisfy the SK-V12 close condition. |
| Alpha-F contract draft and SK-V12 synthesis/handoff | REVISE | The contract states the right order and refusal posture, but it must fold the Alpha-E fallback-intervention gap and require an executable selected-grammar baseline pre-gate for the known JSON-profiled emission blocker before S-P3 can dispatch behavior work. |

## Critical Findings

### CH2-1 - REVISE: Sheets and BBNF-self baseline fallbacks lack a matching intervention path

SK-V12 closes only after one generated non-JSON baseline and one measured
grammar-generalized intervention consume the same output plane. `SYNTHESIS.md`
requires the baseline first, then an intervention clearing
`ceil(baseline_mbps * 1.01)` (`SYNTHESIS.md:35-48`), and `HANDOFF.md` repeats
that binding priority (`HANDOFF.md:46-57`).

Alpha-E has baseline fallbacks for Sheets and BBNF-self (`alpha-E-candidate-shortlist.md:90-184`),
but its only concrete intervention is E4, the CSS L4 FIRST/prefix intervention
(`alpha-E-candidate-shortlist.md:186-233`). That is insufficient under Lock 14:
REDRESS 112 already proved CSS L4 generated Track 1 absent because runtime
emission remained JSON-profiled and no generated CSS L4 runtime existed
(`skinny/REDRESS.md:3311-3338`), and REDRESS 113 proved an intervention wave may
not create the first measurable non-JSON row and then claim the intervention
win (`skinny/REDRESS.md:3340-3355`).

Fold required: generalize E4 into a "selected generated baseline intervention"
candidate with concrete CSS, Sheets, and BBNF-self variants, or add explicit E4a
and E4b fallbacks. Each variant needs owner paths, generated Track 1 source,
independent oracle/Track 2, strict equality, baseline row id, threshold,
same-wave gate consumer, and JSON-policy leak checks. If S-P1/S-P2 select E2 or
E3 as the first feasible baseline, S-P3 must not have to invent the second-stage
intervention outside the Alpha shortlist.

### CH2-2 - REVISE: the known `json_provider` blocker needs an executable baseline pre-gate

The packet correctly names the blocker: W1b failed because codegen/runtime
emission still goes through `json_provider::ensure_runtime_profile`, which only
accepts `backend.grammar_name == "json"` (`skinny/REDRESS.md:3316-3324`;
`skinny/crates/codegen/src/json_provider.rs:4-12`). The current code still calls
that guard from direct and typed emission (`skinny/crates/codegen/src/lib.rs:102-146`),
and the runtime grammar tree contains generated JSON plus `sheets_witness`, not
generated CSS L4, Sheets formula, or BBNF-self modules.

Alpha-E says the baseline must add generated Track 1 and no JSON-provider proof,
and Alpha-F says prose Lock 14 proof cannot admit (`alpha-E-candidate-shortlist.md:55-88`,
`:107-136`, `:156-184`; `alpha-F-contract-draft.md:169-173`). That is directionally
right, but CH2 needs an executable admission pre-gate before behavior dispatch:
for exactly one selected grammar, S-P3 must prove that non-JSON runtime emission
or an explicitly generated per-grammar runtime path exists, the current
`json_provider` guard is replaced or bypassed without generic JSON policy, the
generated module is built, benchmarked, oracle-compared, and consumed by the
non-JSON gate. A report-only W1a lane, `sheets_witness`, old hand runtime, or
fixture parse must fail closed.

Fold required: add this to Alpha-F/SYNTHESIS dispatch requirements as a named
baseline pre-gate, not just S-P1 inventory. The gate can be phrased as "selected
grammar generated emission smoke + runtime module build + same-plane oracle +
non-JSON gate consumption passes" and must fail if the selected path still
routes through JSON-only `ensure_runtime_profile` or records producer-only
telemetry.

## Accepted CH2 Findings

| Check | Result | Evidence |
|---|---|---|
| Non-JSON first is binding | ACCEPT | `SYNTHESIS.md` requires exactly one generated non-JSON baseline before any JSON-only micro-wave and then a measured grammar-generalized intervention (`SYNTHESIS.md:35-48`, `:72-76`). `HANDOFF.md` repeats the same priority (`HANDOFF.md:46-57`). |
| JSON direct residuals are not the first target | ACCEPT | REDRESS 119 records the direct residual fixpoint and REDRESS 120 routes SK-V12 to generated non-JSON first (`skinny/REDRESS.md:3495-3553`). Alpha-C, Alpha-D, and Alpha-F carry that route into the SK-V12 packet. |
| JSON policy leakage is recognized as a hard failure | ACCEPT WITH FOLD | `SYNTHESIS.md` rejects JSON policy in generic crates or runtime outside generated per-grammar modules (`SYNTHESIS.md:190-194`, `:217-238`), and Alpha-F refuses generic-crate JSON policy (`alpha-F-contract-draft.md:175-197`). The fold is to make the selected-baseline pre-gate executable, per CH2-2. |
| Lock 14 proof by execution is the stated admission path | ACCEPT WITH FOLD | Alpha-F requires generated Track 1, independent Track 2/oracle, strict equality, finite same-run throughput, telemetry, and gate consumption, and explicitly rejects prose Lock 14 proof (`alpha-F-contract-draft.md:71-82`, `:169-173`, `:192-193`). The fold is to cover Sheets/BBNF intervention fallback and codegen smoke execution. |
| W1a report lane is not overclaimed | ACCEPT | REDRESS 111 admitted only a non-admitting companion report lane with `S / NO-GO` semantics; it did not create generated non-JSON baseline authority or move rows (`skinny/REDRESS.md:3282-3310`). Alpha-C/D preserve that demotion. |

## Required Fold

1. Add a selected-grammar baseline pre-gate to Alpha-F/SYNTHESIS: generated
   emission or generated per-grammar runtime path exists for exactly one of CSS
   L4, Sheets, or BBNF-self; it builds; it produces generated Track 1; it is
   oracle-compared on the same output plane; and the non-JSON gate consumes it.
2. Generalize the intervention candidate after the baseline: CSS L4 remains the
   preferred E4 target, but Sheets and BBNF-self need equivalent concrete
   fallback intervention candidates before S-P3 dispatch can close SK-V12 under
   Lock 14.
3. Keep the existing refusals: no JSON-only direct retry before the non-JSON
   priority is satisfied or explicitly blocked, no W1a report-only admission, no
   `sheets_witness` or hand-only parser as generated Track 1, no JSON policy in
   generic crates, and no prose-only Lock 14 proof.

## CH2 Verdict

REVISE. The Pass Alpha packet is pointed at the right generality target, but it
does not yet fully close the Lock 14 execution shape for the fallback grammars.
After the two folds above, CH2 should converge to ACCEPT.

Changed path:

- `restart/skinny/tranches/sk-v12/research/alpha-hardening/V1/CH2.md`
