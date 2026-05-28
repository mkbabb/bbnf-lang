# SK-V15 S-P3 V3 CH2 GENERALITY

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V3. Lens: CH2.
Date: 2026-05-28.
HEAD: `efe1e4b01`.
Owned output: `restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH2.md`.

## Verdict

ACCEPT.

The V3 packet closes the two V2 CH2 blockers. The non-JSON proof receiver
matrix is now dispatch-visible through P3-C, SPEC, and the DISPATCH pointer to
the SPEC matrix by surface name. The Lock 14 / Lock 16 exclusion report schema
is present in DISPATCH and W2 dispatch. The remaining CH2 axes also hold:
generic surfaces reject grammar-family bespoke behavior, CSS typed admission
does not reuse W8R floors, EventTape remains one of the five BackendShape
lowerers, and the active S-P3 surfaces contain no stale V1/W0-W9/PRUNE labels.

## Evidence Table

| id | status | evidence | disposition |
|---|---|---|---|
| CH2-V3-01 | ACCEPT | P3-A carries only S-P2 survivor families, not rejected routes, and every shortlisted candidate points back to a grammar-neutral S-P2 survivor citation. The shortlist boundary is explicit at `p3a-candidate-shortlist.md:12`-`:16`; candidates 1-8 cite P2 survivor material and non-JSON consumers at `p3a-candidate-shortlist.md:22`-`:29`. | Preserve the S-P2 survivor boundary. |
| CH2-V3-02 | ACCEPT | The non-JSON receiver matrix is concrete in the gate source and SPEC: P3-C lists `grammar_provider.rs`, `runtime_generator.rs`, Backend lowerers, `backend_egraph.rs`, `decision_csp.rs`, CostFacts / `cost.rs`, `xtask`, and gate/report code with minimum receivers, proof shape, and intrinsic-block handling (`p3c-falsifiability-gates.md:109`-`:124`). SPEC repeats the same matrix at `SPEC.md:206`-`:217`. DISPATCH makes the matrix dispatch-visible by requiring generic edits to cite the SPEC matrix and naming the same surfaces (`DISPATCH-PROMPT.md:92`-`:102`). | V2 CH2-V2-01 is closed. No edit required. |
| CH2-V3-03 | ACCEPT | The Lock 14 / Lock 16 exclusion schema is now dispatch-visible. P3-C defines included roots, excluded roots, reason, owner, self-scan status, primitive status, gate consumer, affected rows, and disposition (`p3c-falsifiability-gates.md:89`-`:107`). SPEC requires the same fields at `SPEC.md:233`-`:235`. DISPATCH requires those fields before dispatch (`DISPATCH-PROMPT.md:92`-`:96`) and repeats them in W2 (`DISPATCH-PROMPT.md:153`-`:155`). | V2 CH2-V2-02 is closed. No edit required. |
| CH2-V3-04 | ACCEPT | Generic surfaces are not grammar-family bespoke. SPEC requires forbidden-token scans over `Json`, `CssL4`, Sheets/corpus names, JSON structural roles, CSS profile names, `json_`, `css_`, `RuntimeProvider`, and aliases (`SPEC.md:224`-`:231`), removes grammar-family runtime modes and hardcoded CSS profile rosters in W3 (`SPEC.md:300`-`:316`), and forbids `json_*`/`css_*` generic decision facts (`SPEC.md:387`-`:392`). P3-C carries the same generic no-branch rule at `p3c-falsifiability-gates.md:181`-`:196` and `:253`-`:260`. | Preserve the scan as a wave-plan requirement, not advisory prose. |
| CH2-V3-05 | ACCEPT | CSS typed provider admission cannot reuse W8R floors. P3-A makes the W8R tuple diagnostic-only (`p3a-candidate-shortlist.md:16`, `:29`, `:57`). P3-C says the tuple is never a typed-admission floor and binds CSS candidate 8 to fresh W6 cssparser typed comparison only (`p3c-falsifiability-gates.md:27`-`:38`, `:67`, `:214`-`:247`). SPEC and DISPATCH preserve the W5 provider / W6 retime split (`SPEC.md:336`-`:376`; `DISPATCH-PROMPT.md:186`-`:216`). | Preserve W5/W6 split. |
| CH2-V3-06 | ACCEPT | EventTape is bound as an existing BackendShape lowerer, not a sidecar or sixth shape. P3-C requires the all-five gate over `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` and rejects sidecar/public-substrate interpretations (`p3c-falsifiability-gates.md:289`-`:308`). SPEC repeats the W9 all-five gate and EventTape bans (`SPEC.md:412`-`:428`), while DISPATCH requires the same W9 discipline and consumers (`DISPATCH-PROMPT.md:257`-`:280`). | Preserve the all-five gate. |
| CH2-V3-07 | ACCEPT | Active S-P3 surfaces have no stale V1/W0-W9/PRUNE labels. The stale-token grep over P3-A through P3-F, SPEC, and DISPATCH returned no matches. | No edit required. |

## Commands

```sh
git rev-parse --short=9 HEAD
rg -n "P3-B does not exist|PRUNE-WAVE|REBUILD-WAVE|W0-W9|W1-W9|Cycle: V1|S-P3 V1" \
  restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md \
  restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md \
  restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md \
  restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md \
  restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md \
  restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md \
  restart/skinny/tranches/sk-v15/SPEC.md \
  restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "grammar_provider\\.rs|runtime_generator\\.rs|backend_egraph\\.rs|decision_csp\\.rs|CostFacts|cost\\.rs|xtask|gate\\.rs|report\\.rs|Minimum non-JSON receivers|Intrinsic-block handling" \
  restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md \
  restart/skinny/tranches/sk-v15/SPEC.md \
  restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "included roots|excluded roots|reason, owner|self-scan status|primitive status|gate consumer|affected rows|disposition|Dispatch schema required" \
  restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md \
  restart/skinny/tranches/sk-v15/SPEC.md \
  restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "CSS_GENERATED_RS|CssFullParseSummary|fact-stream|fact_stream|brace-counter|cssparser|typed|measurement_row_id|broadcast_group_id" \
  restart/skinny/tranches/sk-v15/research/p3 \
  restart/skinny/tranches/sk-v15/SPEC.md \
  restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "EventTape is only|sidecar vector|sixth shape|public substrate API|alternate document projection|all-five BackendShape|exactly five BackendShape" \
  restart/skinny/tranches/sk-v15/research/p3 \
  restart/skinny/tranches/sk-v15/SPEC.md \
  restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
```

Observed:

- HEAD was `efe1e4b01`.
- The stale-label grep returned no matches for active S-P3 packet surfaces.
- Receiver-matrix, exclusion-schema, CSS typed-retime, and EventTape searches
  returned only active gate/dispatch obligations or explicit rejection clauses.

## Required Edits

None.
