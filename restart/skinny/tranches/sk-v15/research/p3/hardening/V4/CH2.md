# SK-V15 S-P3 V4 CH2 GENERALITY

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V4. Lens: CH2.
Date: 2026-05-28.
HEAD: `21ae60663`.
Owned output: `restart/skinny/tranches/sk-v15/research/p3/hardening/V4/CH2.md`.

## Verdict

ACCEPT.

The active S-P3 packet satisfies the CH2 generality lens. The V3 CH2
acceptance surface remains intact at HEAD `21ae60663`: the shortlist is bound
to S-P2 grammar-neutral survivors, the non-JSON receiver matrix is concrete in
P3-C and SPEC and dispatch-visible through DISPATCH, Lock 14 / Lock 16
exclusion fields are required before dispatch, generic surfaces reject
grammar-family bespoke behavior, CSS typed admission cannot reuse W8R floors,
EventTape remains one of the canonical five BackendShape lowerers, and active
P3/SPEC/DISPATCH surfaces have no stale V1/W0-W9/PRUNE labels.

## Evidence Table

| id | status | evidence | disposition |
|---|---|---|---|
| CH2-V4-01 | ACCEPT | P3-A limits the shortlist to S-P2 V3 locked survivors and excludes S-P2 REJECT routes (`p3a-candidate-shortlist.md:8`, `:12`). Candidates 1-8 carry non-JSON consumers or grammar-neutral surfaces, including CSS/Sheets/BBNF receivers for byte-set, classifier, literal, escape, tape, and fact-projection work (`p3a-candidate-shortlist.md:22`-`:29`). | Preserve the S-P2 survivor boundary. |
| CH2-V4-02 | ACCEPT | P3-C defines a surface-specific Non-JSON Proof Receiver Matrix with `grammar_provider.rs`, `runtime_generator.rs`, Backend lowerers, `backend_egraph.rs`, `decision_csp.rs`, CostFacts / `cost.rs`, `xtask`, and gate/report code, each with minimum receivers, proof shape, and intrinsic-block handling (`p3c-falsifiability-gates.md:109`-`:124`). SPEC mirrors the same matrix (`SPEC.md:206`-`:217`). DISPATCH requires generic edits to cite the SPEC matrix by those surfaces (`DISPATCH-PROMPT.md:98`-`:102`). | No receiver-matrix edit required. |
| CH2-V4-03 | ACCEPT | Lock 14 / Lock 16 exclusion schema is dispatch-visible. P3-C requires included roots, excluded roots, reason, owner, self-scan status, primitive status, gate consumer, affected rows, and disposition, and rejects silent allowlists/self-exempting rules (`p3c-falsifiability-gates.md:89`-`:107`). SPEC repeats the field requirement (`SPEC.md:233`-`:235`). DISPATCH refuses plans missing those fields (`DISPATCH-PROMPT.md:92`-`:96`). | No exclusion-schema edit required. |
| CH2-V4-04 | ACCEPT | Generic crates and generators reject grammar-family bespoke surfaces. SPEC requires forbidden-token scans over `Json`, `CssL4`, Sheets/corpus names, JSON structural roles, CSS profile names, `json_`, `css_`, `RuntimeProvider`, and aliases (`SPEC.md:221`-`:231`), and P3-C requires surface-specific receivers rather than a generic non-JSON claim (`p3c-falsifiability-gates.md:111`-`:113`). | Preserve as dispatch gate, not advisory prose. |
| CH2-V4-05 | ACCEPT | W8R is not reused as a CSS admission floor. P3-A excludes the W8R tuple from live floors (`p3a-candidate-shortlist.md:16`) and marks CSS W8R as diagnostic-only (`p3a-candidate-shortlist.md:57`). P3-C requires W6 fresh same-run `cssparser` typed-value/document comparison and says the floor is derived from that run, not W8R (`p3c-falsifiability-gates.md:238`-`:247`). SPEC and DISPATCH preserve the W5 typed-provider / W6 retime split and forbid broadcast-measurement floor reuse (`SPEC.md:336`-`:376`; `DISPATCH-PROMPT.md:186`-`:216`). | Preserve W5/W6 split. |
| CH2-V4-06 | ACCEPT | EventTape remains an existing BackendShape lowerer under the five-shape canon. P3-C requires the all-five gate over `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` and forbids EventTape as a sidecar vector, sixth shape, retained stream, public substrate API, or alternate projection (`p3c-falsifiability-gates.md:289`-`:308`). SPEC repeats the all-five and anti-sidecar gate (`SPEC.md:412`-`:428`). DISPATCH requires W9 plans to preserve the five-shape canon and reject EventTape sidecar/sixth-shape interpretations (`DISPATCH-PROMPT.md:257`-`:280`). | Preserve all-five gate. |
| CH2-V4-07 | ACCEPT | Active-surface stale-label grep returned no matches for `P3-B does not exist`, `PRUNE-WAVE`, `REBUILD-WAVE`, `W0-W9`, `W1-W9`, `Cycle: V1`, `S-P3 V1`, `930.281`, `96/97/98`, or `209..213` across P3-A through P3-F, SPEC, and DISPATCH. | No stale-label edit required. |

## Commands

```sh
git rev-parse --short=9 HEAD
git status --short
sed -n '1,260p' restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md
sed -n '1,260p' restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md
sed -n '1,260p' restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md
sed -n '1,620p' restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md
sed -n '1,260p' restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md
sed -n '1,260p' restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md
sed -n '1,260p' restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md
sed -n '1,620p' restart/skinny/tranches/sk-v15/SPEC.md
sed -n '1,620p' restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
sed -n '1,220p' restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH1.md
sed -n '1,220p' restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH2.md
sed -n '1,220p' restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH3.md
sed -n '1,220p' restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH4.md
sed -n '1,220p' restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH5.md
sed -n '1,220p' restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH6.md
sed -n '1,220p' restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH7.md
rg -n "S-P2|grammar-neutral|Non-JSON Proof Receiver Matrix|Lock 14|Lock 16|included roots|excluded roots|grammar_provider\\.rs|runtime_generator\\.rs|Backend lowerers|backend_egraph|decision_csp|CostFacts|cost\\.rs|xtask|gate\\.rs|report\\.rs|W8R|diagnostic negative|cssparser|typed-admission floor|EventTape|five BackendShape|sixth|sidecar vector|new/sixth|W0-W9|W1-W9|Cycle: V1|S-P3 V1|PRUNE-WAVE|REBUILD-WAVE" restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "P3-B does not exist|PRUNE-WAVE|REBUILD-WAVE|W0-W9|W1-W9|Cycle: V1|S-P3 V1|930\\.281|96/97/98|209\\.\\.213" restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md || true
git diff --check -- restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
```

Observed:

- HEAD was `21ae60663`.
- The unrelated dirty implementation files were present and untouched.
- The stale-label grep returned no active-surface matches.
- `git diff --check` returned clean for active S-P3 packet surfaces.

## Required Edits

None.
