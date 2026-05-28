# SK-V15 S-P3 V3 CH1 Correctness

Cycle: S-P3 Synthesis-Plan V3.
Date: 2026-05-28.
Input commit: `efe1e4b01`.
Lens: CH1 correctness.
Owned output: `restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH1.md`.

## Verdict

ACCEPT.

The S-P3 packet at `efe1e4b01` satisfies CH1 correctness for the current
planning surface. The V2 CH1 blockers are folded: candidate floors now rebind
to `SK-V15-open` in P3-C, and the dependency-row ledger is enforceable from
SPEC plus DISPATCH. The active packet uses W0-W11 wave ids, keeps W8R CSS
numbers diagnostic-only, binds behavior close to measured strict rows or
same-workload typed CSS proof, and contains no stale V1/W0-W9/PRUNE-WAVE labels
in active P3/SPEC/DISPATCH surfaces.

## Evidence

| check | disposition | evidence |
|---|---|---|
| W0-W11 topology | ACCEPT | P3-B and P3-C declare `W0 -> W1 -> W2 -> W3 -> W4 -> W5 -> W6 -> W7 -> W8 -> W9 -> W10 -> W11` (`p3b-wave-sequencing.md:17`, `p3c-falsifiability-gates.md:15`). SPEC dispatch lock orders W1 through W11 (`SPEC.md:36`-`:43`), and SPEC/DISPATCH enumerate W0 through W11 sections (`SPEC.md:246`-`:447`, `DISPATCH-PROMPT.md:118`-`:301`). |
| Candidate trace and rebinding | ACCEPT | P3-A keeps eight S-P2 survivor families and excludes S-P2 REJECT routes (`p3a-candidate-shortlist.md:8`, `:12`, `:20`-`:29`). P3-C adds the required candidate rebinding table with `row universe`, `final threshold formula`, `same-wave consumer`, `proof command shape`, and `fail action` for candidates 1-8 (`p3c-falsifiability-gates.md:51`-`:68`). |
| Measurable row gates | ACCEPT | P3-C requires behavior waves to close on same-run measurements and strict equality/comparator proof, with ledger/gate-only waves closing on explicit no-behavior proof (`p3c-falsifiability-gates.md:17`-`:20`). JSON remains strict same-plane over the 51 rows (`p3c-falsifiability-gates.md:24`-`:26`, `SPEC.md:52`-`:53`); behavior maintain is `>=98.0%` of `SK-V15-open` (`p3c-falsifiability-gates.md:48`); W6 CSS floors come from fresh same-run `cssparser` typed comparison, not W8R (`p3c-falsifiability-gates.md:238`-`:247`, `SPEC.md:361`-`:372`). |
| Dependency-row enforceability | ACCEPT | SPEC promotes the normative dependency schema with `row_id`, `retired_or_deleted_artifact`, `delete_or_retire_wave`, `rebuild_provider_wave`, `proof_command`, `provider_lands_no_later`, `conditional_status`, `consuming_exit_gates`, and `preblock_cluster` (`SPEC.md:187`-`:205`). DISPATCH binds compact plans back to those fields and mirrors all eleven `DEP-*` rows (`DISPATCH-PROMPT.md:68`-`:90`). SPEC and DISPATCH name dependency rows consumed in W1, W3, W4, W5, W6, W7, W8, W9, W10, and W11 (`SPEC.md:281`, `:316`, `:333`-`:334`, `:352`-`:355`, `:373`-`:376`, `:392`, `:410`, `:428`, `:445`, `:464`-`:465`; `DISPATCH-PROMPT.md:141`, `:168`, `:183`-`:184`, `:198`-`:200`, `:214`-`:216`, `:228`, `:249`, `:270`, `:297`, `:315`-`:316`). |
| Same-wave consumers and anti-deferral | ACCEPT | P3-C names same-wave consumers for every W0-W11 gate (`p3c-falsifiability-gates.md:143`, `:160`, `:178`, `:195`, `:211`, `:229`, `:246`, `:262`-`:267`, `:283`-`:287`, `:303`-`:308`, `:325`-`:326`, `:345`-`:346`). SPEC forbids implementation-limited close and documentation-only close (`SPEC.md:82`-`:84`, `:146`), while DISPATCH makes unresolved W11 implementation fixes or dependency rows abort close instead of deferring to SK-V16 (`DISPATCH-PROMPT.md:309`-`:313`). |
| CSS W8R treatment | ACCEPT | W8R numbers appear only as diagnostic-negative or pre-block evidence: P3-C marks the tuple diagnostic-only (`p3c-falsifiability-gates.md:32`-`:34`), P3-D allows only aggregate diagnostic or independent typed retiming (`p3d-telemetry-schema.md:74`-`:81`), and P3-E explicitly pre-blocks reusing `2319.041`, `2362.037`, or `929.281` as live CSS floors (`p3e-preblocked-ledger.md:169`). |
| Stale-label sweep | ACCEPT | Executed `rg -n "Cycle: V1|S-P3 V1|W0-W9|W1-W9|W0 through W9|W1 through W9|P3-B does not exist|PRUNE-WAVE|REBUILD-WAVE|2362\\.037 Mbps|930\\.281 Mbps" restart/skinny/tranches/sk-v15/research/p3/p3{a,b,c,d,e,f}-*.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md || true`; it returned no matches. Intentional prose such as "prune-before-rebuild" remains goalset language, not a stale dispatch id. |

## Verification

Commands run:

```sh
git rev-parse --short HEAD
git status --short
rg -n "W0 -> W1 -> W2 -> W3 -> W4 -> W5 -> W6 -> W7 -> W8 -> W9 -> W10 -> W11|W0-W11|W1-W11|W0 through W11" restart/skinny/tranches/sk-v15/research/p3/*.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "Candidate Rebinding Table|SK-V15-open|final threshold formula|same-wave consumer|proof command shape|fail action|max\\(P3-A floor|fresh W6 same-run cssparser|never W8R" restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md
rg -n "DEP-W1-CSS-BROADCAST|DEP-W6-CSS-GENERATED-RS|DEP-W6-CSS-SUMMARY-FACT-STREAM|DEP-W3-W6-CSS-PROVIDER-TEMPLATE|DEP-W4-PATTERN-H-PROVENANCE|DEP-W4-W6-CSS-LEGACY-RUNTIME-SHIM|DEP-W7-DECISION-SPINE|DEP-W8-LOWERERS-A|DEP-W9-LOWERERS-B|DEP-W10-FNV-QUARANTINE|DEP-W11-CLOSE-NO-ORPHANS|row_id|retired_or_deleted_artifact|provider_lands_no_later|consuming_exit_gates" restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "same-wave consumer|Same-wave consumer|Dependency rows consumed|No implementation-limited miss closes|SK-V16 routing is routed remainder|documentation-only close|future-phase promises|cannot substitute|PASS-IMPL V2" restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "Cycle: V1|S-P3 V1|W0-W9|W1-W9|W0 through W9|W1 through W9|P3-B does not exist|PRUNE-WAVE|REBUILD-WAVE|2362\\.037 Mbps|930\\.281 Mbps" restart/skinny/tranches/sk-v15/research/p3/p3{a,b,c,d,e,f}-*.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md || true
git diff --check -- restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH1.md restart/skinny/tranches/sk-v15/research/p3/*.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
```

Observed:

- HEAD is `efe1e4b01`.
- `git diff --check` returned clean.
- The stale-label grep returned no matches in active P3-A through P3-F, SPEC,
  and DISPATCH.
- No CH1-required file edits remain.
