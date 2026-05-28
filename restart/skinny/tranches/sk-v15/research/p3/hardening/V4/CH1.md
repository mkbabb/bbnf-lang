# SK-V15 S-P3 V4 CH1 Correctness

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V4. Lens: CH1.
Date: 2026-05-28.
HEAD: `21ae60663`.
Owned output: `restart/skinny/tranches/sk-v15/research/p3/hardening/V4/CH1.md`.

## Verdict

ACCEPT.

The unchanged post-V3 S-P3 packet satisfies CH1 correctness. The active packet
keeps the W0-W11 topology, traces the shortlist to the S-P2 survivor pool,
rebinds candidate floors to `SK-V15-open`, makes row gates measurable, enforces
delete/retire/demotion dependency rows, names same-wave consumers, blocks
SK-V16-as-proof deferral, and has no stale V1/W0-W9/PRUNE-WAVE labels in active
P3/SPEC/DISPATCH surfaces.

## Evidence Table

| check | verdict | evidence | exact edits if REVISE |
|---|---|---|---|
| Worktree and input stability | ACCEPT | HEAD is `21ae60663`. `git diff --name-only --` over active P3-A..P3-F, `SPEC.md`, `DISPATCH-PROMPT.md`, `PASS-3-SYNTHESIS-PLAN.md`, and V3 CH1-CH7 returned no paths, so the V4 review is against the unchanged post-V3 packet. | None. |
| W0-W11 topology | ACCEPT | P3-B and P3-C declare `W0 -> W1 -> W2 -> W3 -> W4 -> W5 -> W6 -> W7 -> W8 -> W9 -> W10 -> W11` (`p3b-wave-sequencing.md:17`, `p3c-falsifiability-gates.md:15`). SPEC dispatch lock orders W1-W11, including W5 after W1-W4, W6 after W5, and W11 after W1-W10 are resolved (`SPEC.md:38`-`:43`). SPEC and DISPATCH enumerate the W0-W11 contract (`SPEC.md:246`-`:491`, `DISPATCH-PROMPT.md:118`-`:316`). | None. |
| Candidate trace and rebinding | ACCEPT | P3-A limits the shortlist to S-P2 survivor families and excludes S-P2 REJECT routes (`p3a-candidate-shortlist.md:8`, `:12`, `:20`-`:29`). P3-A requires P3-C to rebind non-CSS floors to `SK-V15-open` and excludes W8R CSS as a live floor (`p3a-candidate-shortlist.md:16`). P3-C carries the candidate rebinding table with `row universe`, `final threshold formula`, `same-wave consumer`, `proof command shape`, and `fail action` for candidates 1-8 (`p3c-falsifiability-gates.md:51`-`:68`). | None. |
| Measurable row gates and strict planes | ACCEPT | P3-C says behavior waves close only on same-run measurements plus strict equality/comparator proof, while ledger/gate waves close on explicit no-behavior proof and gate-consumed reports (`p3c-falsifiability-gates.md:17`-`:20`). JSON remains the 51 strict same-plane rows (`p3c-falsifiability-gates.md:24`-`:26`, `SPEC.md:51`-`:53`). Behavior maintain is `>=98.0%` of `SK-V15-open` (`p3c-falsifiability-gates.md:48`). CSS floors come only from fresh W6 same-run `cssparser` typed comparison, never the W8R tuple (`p3c-falsifiability-gates.md:32`-`:38`, `:238`-`:247`; `SPEC.md:361`-`:372`). | None. |
| DEP row enforceability | ACCEPT | SPEC Section 2.1 requires every delete, retirement, diagnostic demotion, or neutralization to match a dependency row before redress and defines `row_id`, `retired_or_deleted_artifact`, `delete_or_retire_wave`, `rebuild_provider_wave`, `proof_command`, `provider_lands_no_later`, `conditional_status`, `consuming_exit_gates`, and `preblock_cluster` (`SPEC.md:187`-`:204`). DISPATCH binds compact plans back to those fields and mirrors all eleven `DEP-*` rows (`DISPATCH-PROMPT.md:68`-`:90`). SPEC and DISPATCH name per-wave DEP consumption for W1, W3, W4, W5, W6, W7, W8, W9, W10, and W11 (`SPEC.md:281`, `:316`, `:333`-`:334`, `:352`-`:355`, `:373`-`:376`, `:392`, `:410`, `:428`, `:445`, `:464`-`:465`; `DISPATCH-PROMPT.md:141`, `:168`, `:183`-`:184`, `:198`-`:200`, `:214`-`:216`, `:228`, `:249`, `:270`, `:297`, `:315`-`:316`). | None. |
| Same-wave consumers and anti-deferral | ACCEPT | P3-C names same-wave consumers for W0-W11 (`p3c-falsifiability-gates.md:143`, `:160`, `:178`, `:195`, `:211`, `:229`, `:246`, `:262`-`:267`, `:283`-`:287`, `:303`-`:308`, `:325`-`:326`, `:345`-`:346`). SPEC forbids implementation-limited and documentation-only close (`SPEC.md:82`-`:84`, `:146`). P3-E and DISPATCH both say SK-V16 routing is not SK-V15 close evidence, and W11 aborts on unresolved fixes, measurements, or dependency rows (`p3e-preblocked-ledger.md:255`-`:260`, `DISPATCH-PROMPT.md:309`-`:313`). | None. |
| Stale-label sweep | ACCEPT | `rg -n "Cycle: V1|S-P3 V1|W0-W9|W1-W9|W0 through W9|W1 through W9|P3-B does not exist|PRUNE-WAVE|REBUILD-WAVE|2362\\.037 Mbps|930\\.281 Mbps"` over active P3-A..P3-F, `SPEC.md`, and `DISPATCH-PROMPT.md` returned no matches. | None. |

## Verification

Commands run:

```sh
git rev-parse --short=9 HEAD
git status --short
git diff --name-only -- restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH1.md restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH2.md restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH3.md restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH4.md restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH5.md restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH6.md restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH7.md
rg -n "W0 -> W1 -> W2 -> W3 -> W4 -> W5 -> W6 -> W7 -> W8 -> W9 -> W10 -> W11|W1 dispatches after W0|W5 dispatches after W1-W4|W6 dispatches after W5|W11 dispatches after W1-W10|W0-W11" restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "Candidate Rebinding Table|SK-V15-open|final threshold formula|same-wave consumer|proof command shape|fail action|max\\(P3-A floor|fresh W6 same-run cssparser|never W8R|S-P2 survivor|S-P2 REJECT" restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md
rg -n "DEP-W1-CSS-BROADCAST|DEP-W6-CSS-GENERATED-RS|DEP-W6-CSS-SUMMARY-FACT-STREAM|DEP-W3-W6-CSS-PROVIDER-TEMPLATE|DEP-W4-PATTERN-H-PROVENANCE|DEP-W4-W6-CSS-LEGACY-RUNTIME-SHIM|DEP-W7-DECISION-SPINE|DEP-W8-LOWERERS-A|DEP-W9-LOWERERS-B|DEP-W10-FNV-QUARANTINE|DEP-W11-CLOSE-NO-ORPHANS|row_id|retired_or_deleted_artifact|provider_lands_no_later|consuming_exit_gates" restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "same-wave consumer|Same-wave consumer|Dependency rows consumed|No implementation-limited miss closes|SK-V16 routing is routed remainder|documentation-only close|future-phase promises|cannot substitute|PASS-IMPL V2|gate consumes|same-run measurements|strict equality" restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md
rg -n "Cycle: V1|S-P3 V1|W0-W9|W1-W9|W0 through W9|W1 through W9|P3-B does not exist|PRUNE-WAVE|REBUILD-WAVE|2362\\.037 Mbps|930\\.281 Mbps" restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
```

The stale-label grep returned no matches. No CH1 REVISE edits are required.
