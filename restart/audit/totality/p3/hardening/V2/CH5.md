# CH5 Hidden Coupling - T-P3 V2

Verdict: ACCEPT

No CH5 defects found. Repair directive / owner / severity fields are not
applicable because this lens returns ACCEPT.

## Required Local Checks

- `git show --stat --oneline 7885b29ab -- restart/audit/totality/p3`: target
  packet `7885b29ab docs(sk-v15-t-p3): fold V1 hardening into V2 synthesis`
  touches the seven T-P3 artifacts with 287 insertions and 206 deletions.
- `git diff --check 7885b29ab^ 7885b29ab -- restart/audit/totality/p3`: no
  output.
- Extracted `restart/audit/totality/p3/3C-locks-v+1-diff.md` hunk and ran
  `git apply --check /tmp/tp3-locks-v2.diff`: no output.
- `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md`: `16`.
- `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`:
  `67`.
- Required stale-pattern scan over 3A-3F returned no matches for the stale
  `ORCHESTRATOR-PROMPT`, stale `2F-parse-that-gaps.md:518`,
  active-`bbnf-regex` owner, peer `bbnf-regex`/`bbnf-simd` owner list,
  follow-up docs cleanup, and old open-question wording patterns.

## Coupling Audit

- Parallel substrate / sidecar producer / Track 1 == Track 2: ACCEPT. 3C's
  proposed addendum adds no substrate, public substrate API, retained sidecar,
  or sixth shape (`restart/audit/totality/p3/3C-locks-v+1-diff.md:39`), and its
  Lock 1 clause rejects retained cursor/list/class-column/sidecar, public
  `UnionTape`, second tape, runtime regex/DFA substrate, and cross-call
  classifier state unless later G-Omega amends Lock 1
  (`restart/audit/totality/p3/3C-locks-v+1-diff.md:41`). 3B keeps runtime
  substrate work pending while rejecting hidden sidecar/parallel substrate
  routes and explicitly blocks Track 1 == Track 2 sidecar close
  (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:72`,
  `restart/audit/totality/p3/3B-master-plan-reconciliation.md:90`).
- FactStream as BackendShape: ACCEPT. 3A states `admitted_fact_output` is an
  output-plane / `SubstrateTarget` classification, never a `BackendShape` or CSS
  Value API proof (`restart/audit/totality/p3/3A-architecture-synthesis.md:62`).
  3C keeps `FactStream` out of `BackendShape`
  (`restart/audit/totality/p3/3C-locks-crystallisation.md:81`), and 3E keeps
  the per-grammar matrix on exactly five variants without adding FactStream as a
  shape (`restart/audit/totality/p3/3E-grammar-generalisation.md:70`).
- Renamed-scanner Lock 1 and V1 regex owner defects: ACCEPT. V1 required 3A/3C
  to replace active `bbnf-regex` owner wording and prevent peer-owner Lock 16
  wording (`restart/audit/totality/p3/hardening/HARDENING-T-P3-V1-CONSOLIDATED.md:46`).
  V2 3A names `parse-that-regex`, `bbnf-simd`, and generated providers, while
  marking `skinny/crates/bbnf-regex` only as a temporary legacy path and not an
  admissible future owner (`restart/audit/totality/p3/3A-architecture-synthesis.md:69`).
  3C mirrors that closure for Lock 16
  (`restart/audit/totality/p3/3C-locks-crystallisation.md:56`;
  `restart/audit/totality/p3/3C-locks-v+1-diff.md:63`), consistent with Lock
  11 canonical naming (`restart/locks/LOCKS.md:319`).
- Runtime regex/DFA substrate and V1 Lock 1 gate defect: ACCEPT. V1 required
  all runtime regex/DFA appearances to say manifest/consumer proof is necessary
  but never sufficient, and any runtime substrate requires prior G-Omega Lock 1
  amendment
  (`restart/audit/totality/p3/hardening/HARDENING-T-P3-V1-CONSOLIDATED.md:47`).
  V2 3A says runtime regex/DFA remains rejected unless prior G-Omega changes
  Lock 1, with manifest plus consumer proof necessary but never sufficient
  (`restart/audit/totality/p3/3A-architecture-synthesis.md:69`). 3C's Lock 1
  and Lock 16 clauses state the same gate
  (`restart/audit/totality/p3/3C-locks-v+1-diff.md:41`;
  `restart/audit/totality/p3/3C-locks-v+1-diff.md:63`).
- x86 diagnostic evidence as aarch64 close evidence: ACCEPT. 3A requires Apple
  M5 Max/aarch64 admission and treats x86/AVX-512 as diagnostic only
  (`restart/audit/totality/p3/3A-architecture-synthesis.md:68`). 3B keeps
  CollapsedStage diagnostic unless aarch64 proof exists
  (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:132`), and 3E
  says current SK-V15 grammar rows have no CollapsedStage admission while x86
  remains diagnostic unless a future aarch64 strategy lands
  (`restart/audit/totality/p3/3E-grammar-generalisation.md:97`). This matches
  the close-host invariant that Apple M5 Max/aarch64 is the only admission host
  and x86/AVX-512 are diagnostic only
  (`restart/skinny/tranches/sk-v15/SPEC.md:135`-`136`).

Residual risk: low. The only prior CH5 REVISE items are folded explicitly, and
the required checks preserve the target-packet invariants: 16 locks, 67 Pattern
H runtime files, clean proposed lock diff application, and no stale CH5 pattern
matches.
