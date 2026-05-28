# SK-V15 S-P3 V2 CH1 Correctness

Cycle: S-P3 Synthesis-Plan V2.
Date: 2026-05-28.
Input commit: `39e186ee3`.
Lens: CH1 correctness.

## Verdict

REVISE.

The V2 packet correctly reindexes SK-V15 to W0-W11, treats the W8R CSS
tuple as diagnostic-only, splits W5/W6 CSS provider versus retime work, and
does not contradict the 16-lock / five-BackendShape canon. It is not yet
CH1-correct because two promised gate surfaces remain under-specified in the
dispatch contract: P3-C lacks the required per-candidate SK-V15-open rebinding
table, and SPEC/DISPATCH do not promote the concrete dependency rows that
delete/retire/demotion gates are supposed to consume.

## Findings

| id | verdict | finding | citations | required follow-up |
|---|---|---|---|---|
| CH1-V2-01 | REVISE | P3-A requires P3-C to rebind non-CSS candidate floors to `SK-V15-open`, but P3-C only has global `M0`/`M1`/`M-css-prune` budgets. The V1 hardening roster explicitly required a candidate rebinding table for candidates 1-8 with formulas, consumers, scalar/oracle/parity requirements, and reject/demotion actions. Without that table, row-gate correctness depends on P3-A prose instead of the canonical gate source. | `restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md:16`, `restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:43-49`, `restart/skinny/tranches/sk-v15/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:40-43`, `restart/skinny/tranches/sk-v15/research/p3/hardening/V1/redeploy/GATE-TABLE-FOLD-NOTES.md:95-98` | Add a P3-C candidate gate table for candidates 1-8 with row universe, target rows/scans, guard rows, final threshold formula, CSS treatment, same-wave consumer, proof command, and REDRESS/revert action. Preserve W8R numbers as diagnostic-only fixtures. |
| CH1-V2-02 | REVISE | The concrete NEW-CH3 dependency rows exist in P3-B, and P3-C says every delete/retire/demotion must have a visible dependency row with required columns, but SPEC only carries a high-level dependency summary and DISPATCH tells the orchestrator to verify rows that are not present in the final dispatch surface. This weakens dependency gating for CSS old-proof retirement, provider/template deletion, Decision Engine scaffold retirement, lowerer scaffold retirement, and FNV quarantine. | `restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md:87-98`, `restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:325-328`, `restart/skinny/tranches/sk-v15/SPEC.md:176-186`, `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:51-52`, `restart/skinny/tranches/sk-v15/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:44` | Promote the P3-B dependency rows, or an explicit normative pointer plus the required schema, into SPEC and DISPATCH. Each row must name retired/deleted artifact, delete/retire wave, rebuild provider wave, proof command, provider-no-later-than-delete status, disposition, and gate consumer. |

## Accepted Correctness Checks

| check | evidence |
|---|---|
| W0-W11 topology | P3-B declares `W0 -> ... -> W11` and the 12-wave ceiling; P3-C, SPEC, and DISPATCH enumerate W0 through W11 directly (`p3b-wave-sequencing.md:15-26`, `p3c-falsifiability-gates.md:12-15`, `SPEC.md:161-174`, `DISPATCH-PROMPT.md:71-217`). |
| CSS W8R diagnostic-only treatment | P3-A excludes W8R metrics from live floors, P3-C labels the tuple a diagnostic negative fixture only, P3-D binds the legal outcomes, and SPEC/DISPATCH reject W8R floors (`p3a-candidate-shortlist.md:16`, `p3c-falsifiability-gates.md:27-38`, `p3d-telemetry-schema.md:74-81`, `SPEC.md:307-310`, `DISPATCH-PROMPT.md:130-147`). |
| W5/W6 CSS split | W5 owns typed CSS value/document/view/visitor provider output; W6 owns fresh same-workload cssparser retime and old-proof retirement (`SPEC.md:297-329`, `DISPATCH-PROMPT.md:125-147`, `p3c-falsifiability-gates.md:179-212`). |
| Lock compatibility | `restart/locks/LOCKS.md` has 16 locks, and Lock 1/10 preserve the exact five BackendShape variants `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` with FactStream explicitly not a sixth shape (`restart/locks/LOCKS.md:100-109`, `restart/locks/LOCKS.md:269-273`). SPEC W9 repeats the five-shape set without adding a sixth variant (`SPEC.md:363-378`). |

## Verification

Commands run:

```sh
git rev-parse --short=9 HEAD
git status --short -- restart/skinny/tranches/sk-v15/research/p3/hardening/V2 restart/skinny/tranches/sk-v15/research/p3/*.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md restart/skinny/tranches/sk-v15/SYNTHESIS.md
rg -n "W0-W9|W0 through W9|P3-B does not exist|PRUNE-WAVE|REBUILD-WAVE|2362\\.037 Mbps|930\\.281" restart/skinny/tranches/sk-v15/research/p3/*.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "2319\\.041|2362\\.037|929\\.281" restart/skinny/tranches/sk-v15/research/p3/*.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "^### W([0-9]|1[01])|^\\| W([0-9]|1[01]) \\|" restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
grep -cE "^[0-9]+\\. \\*\\*" restart/locks/LOCKS.md
rg -n "EagerTape|OffsetTape|EventTape|SinkOnly|CollapsedStage|BackendShape|FactStream" restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
```

Observed:

- HEAD is `39e186ee3`.
- Target S-P3 files were clean before this CH1 write.
- Stale-topology grep returned no hits for P3-A through P3-F, SPEC, and DISPATCH.
- W8R numeric tuple appears only in diagnostic-negative-fixture contexts.
- Lock count is `16`.
- Pattern H count is `67`.

Recommended after follow-up:

```sh
rg -n "candidate|final_floor|SK-V15-open|same_wave_consumer|REDRESS/revert" restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md
rg -n "CSS_GENERATED_RS|CssFullParseSummary|Decision Engine scaffold|Label-string lowerers|FNV closed-enum|provider-no-later|gate consumer" restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
```
