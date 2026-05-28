# CH4 COST - T-P3 V1

Verdict: REVISE.

The packet preserves the proposal-only boundary and the required mechanical
invariants, but V1 is not cost-clean. The main failures are cap realism and
field completeness: 3B's W4/W7/W8/W9 rows understate costs relative to the
T-P1/T-P2 cost carriers, 3E leaves a possible CSSOM/projection expansion
unbounded, and 3C's 12 lock-addendum deltas do not state LOC/risk/wave/gate
fields per delta.

## Evidence Commands And Outputs

```sh
$ git status --short
 M crates/core/src/runtime/bbnf/arena.rs
 M crates/core/src/runtime/bbnf/builder.rs
 M crates/core/src/runtime/bbnf/document.rs
 M crates/core/src/runtime/bbnf/parse_with.rs
 M crates/core/src/runtime/bbnf/serialize.rs
 M crates/core/src/runtime/bbnf/view.rs
 M crates/core/src/runtime/bnf/builder.rs
 M crates/core/src/runtime/bnf/document.rs
 M crates/core/src/runtime/bnf/kind.rs
 M crates/core/src/runtime/css_pretty/builder.rs
 M crates/core/src/runtime/css_pretty/document.rs
 M crates/core/src/runtime/css_pretty/kind.rs
 M crates/core/src/runtime/css_pretty/view.rs
 M crates/core/src/runtime/csv/builder.rs
 M crates/core/src/runtime/csv/document.rs
 M crates/core/src/runtime/csv/kind.rs
 M crates/core/src/runtime/ebnf/builder.rs
 M crates/core/src/runtime/ebnf/document.rs
 M crates/core/src/runtime/ebnf/kind.rs
 M crates/core/src/runtime/google_sheets/arena.rs
 M crates/core/src/runtime/google_sheets/builder.rs
 M crates/core/src/runtime/google_sheets/document/canonical.rs
 M crates/core/src/runtime/google_sheets/document/mod.rs
 M crates/core/src/runtime/google_sheets/document/path_query.rs
 M crates/core/src/runtime/google_sheets/document/view.rs
 M crates/core/src/runtime/google_sheets/parse_with.rs
 M crates/core/src/runtime/math/builder.rs
 M crates/core/src/runtime/math/document.rs
 M crates/core/src/runtime/math/kind.rs
 M docs/precepts
 M restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-1-css-l4-oracle.json
 M restart/skinny/tranches/sk-v13/research/w10.1/skv13-W10.1-css-l4-at-rules-media.json
 M restart/skinny/tranches/sk-v13/research/w10.2/skv13-W10.2-css-l4-vendor-custom.json
 M restart/skinny/tranches/sk-v13/research/w10.3/skv13-W10.3-css-l4-nested-layout.json
 M restart/skinny/tranches/sk-v13/research/w2/skv13-W2-css-l4-stylesheet-selectors.json
 M restart/skinny/tranches/sk-v13/research/w3/skv13-W3-css-l4-declaration-values-extended.json
 M restart/skinny/tranches/sk-v13/research/w4/skv13-W4-css-l4-visual-functions.json
 M skinny/crates/bbnf-bench/src/css_l4_w8.rs
 M skinny/crates/bbnf-bench/src/generated_real_typed.rs
 M skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/generated.rs
 M skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs
 M skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs
 M skinny/crates/runtime/src/grammars/css_l4_nested_layout/generated.rs
 M skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs
 M skinny/crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/generated.rs
 M skinny/crates/runtime/src/grammars/css_l4_visual_functions/generated.rs
 M xtask/src/main.rs
 M xtask/src/regen_simple_runtime.rs
```

Dirty state predates this CH4 output and is outside this agent's owned file.

```sh
$ git show --stat --oneline 0a0508acd -- restart/audit/totality/p3
0a0508acd docs(sk-v15-t-p3): add V1 synthesis packet
 .../audit/totality/p3/3A-architecture-synthesis.md |  92 ++++++++++++
 .../totality/p3/3B-master-plan-reconciliation.md   | 167 +++++++++++++++++++++
 .../audit/totality/p3/3C-locks-crystallisation.md  | 114 ++++++++++++++
 restart/audit/totality/p3/3C-locks-v+1-diff.md     |  76 ++++++++++
 restart/audit/totality/p3/3D-skinny-fold.md        |  93 ++++++++++++
 .../audit/totality/p3/3E-grammar-generalisation.md | 145 ++++++++++++++++++
 restart/audit/totality/p3/3F-migration-handoff.md  | 120 +++++++++++++++
 7 files changed, 807 insertions(+)

$ git diff --check 0a0508acd^ 0a0508acd -- restart/audit/totality/p3
# no output; exit 0

$ awk '/^```diff$/{in_diff=1; next} in_diff && /^```$/{exit} in_diff {print}' \
  restart/audit/totality/p3/3C-locks-v+1-diff.md > /tmp/tp3-locks-v1.diff
$ git apply --check /tmp/tp3-locks-v1.diff
# no output; exit 0

$ grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
16

$ find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
      67
```

Cost cross-check excerpts:

```sh
$ nl -ba restart/audit/totality/p3/3B-master-plan-reconciliation.md | sed -n '118,123p'
118 | MP.SK15.W4 Pattern H generated discipline | new pending | 120-280 / runtime generated checks / 80-180 | high | ...
121 | MP.SK15.W7 Decision Engine spine | new pending | 140-300 / selection fixtures / 80-180 | high | ...
122 | MP.SK15.W8 BackendShape harness plus EagerTape/OffsetTape | new pending | 140-300 / 180-360 / 80-180 | high | ...
123 | MP.SK15.W9 EventTape/SinkOnly/CollapsedStage plus all-five gate | new pending | 160-340 / 220-420 / 100-220 | high | ...

$ nl -ba restart/audit/totality/p2/2D-cost-model.md | sed -n '72,76p'
74 | W7 | Decision Engine spine... | 900-1400 | high | W7 | Fits only if lowerer output is out of scope. | ...
75 | W8 | BackendShape lowerers A... | 700-1100 | high | W8 | Fits if shared tape helpers are minimal and EventTape is deferred to W9. | ...
76 | W9 | BackendShape lowerers B... | 850-1300 | high | W9 | Fits if CollapsedStage remains diagnostic-only unless 2E provides an aarch64 route. | ...

$ nl -ba restart/audit/totality/p1/1D-skinny-lessons.md | sed -n '174,180p'
178 | RC-05 Pattern H generated ownership | ... | 1,500-3,000 generator/provenance gate; 700-1,200 for any named runtime projection; 600-1,200 close transcript | high | W4 | 3,600 | ...
179 | RC-06 Decision Engine emission | ... | 600-2,200 | high | W7-W9 | 2,200 | ...
```

## Findings

| id | severity | owner | target reference | conflicting evidence | finding | repair directive |
|---|---|---|---|---|---|---|
| CH4-COST-01 | Critical | 3B with 3D/3E alignment | `restart/audit/totality/p3/3B-master-plan-reconciliation.md:121` and `restart/audit/totality/p3/3B-master-plan-reconciliation.md:154` budget W7/W8/W9 at 440-940 manual LOC total. | `restart/audit/totality/p2/2D-cost-model.md:74`, `restart/audit/totality/p2/2D-cost-model.md:75`, `restart/audit/totality/p2/2D-cost-model.md:76`; `restart/audit/totality/p3/3D-skinny-fold.md:77`; `restart/audit/totality/p3/3A-architecture-synthesis.md:58`, `restart/audit/totality/p3/3A-architecture-synthesis.md:59`. | W7-W9 scope is unrealistic as written. The receiver rows copy the skinny manifest's narrow manual bands while the cost dossier says W7 alone is 900-1400 LOC, W8 is 700-1100 LOC, and W9 is 850-1300 LOC. This creates hidden overflow against the 30-minute redress cap and risks a silent W12-equivalent spill. | Replace the 3B W7/W8/W9 budget rows with the 2D costed bands, or explicitly narrow each row to a gate-only/intrinsic-block slice. Each row must state the same-wave consumer/gate, hard-cap fit, fail action, and no-W12 route. If the skinny `SPEC.md` manifest remains lower, route a wave-graph amendment through G-Omega instead of treating the low bands as dispatch-realistic. |
| CH4-COST-02 | Critical | 3B with 3D/3E alignment | `restart/audit/totality/p3/3B-master-plan-reconciliation.md:118`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:137`, and `restart/audit/totality/p3/3B-master-plan-reconciliation.md:153` budget W4 Pattern H at 120-280 manual LOC and fold W2-W4 into 390-880 manual LOC. | `restart/audit/totality/p1/1D-skinny-lessons.md:178`; `restart/audit/totality/p3/3A-architecture-synthesis.md:55`; `restart/audit/totality/p3/3D-skinny-fold.md:76`; `restart/audit/totality/p3/3E-grammar-generalisation.md:74`, `restart/audit/totality/p3/3E-grammar-generalisation.md:133`. | Pattern H is hidden under a small gate/doc band. The deeper cost carrier makes W4 a 1,500-3,000 LOC generator/provenance effort, plus 600-1,200 close transcript and 700-1,200 per named projection. 3E also leaves "larger projection waves" unbounded. | Split W4 into explicit sub-rows: provenance gate, generator/check proof, any runtime projection, and close transcript. Give each a bounded LOC range, consumer/gate, fail action, and cap-fit statement. If only a 120-280 LOC gate is intended, mark runtime projection/provenance generation as intrinsic-blocked rather than implied closed. |
| CH4-COST-03 | High | 3C | `restart/audit/totality/p3/3C-locks-crystallisation.md:36`, `restart/audit/totality/p3/3C-locks-crystallisation.md:100`, `restart/audit/totality/p3/3C-locks-crystallisation.md:104`, and `restart/audit/totality/p3/3C-locks-v+1-diff.md:38`. | CH4 contract at `restart/prompts/totality/PASS-3-SYNTHESIS.md:118`-`120`; lens assignment at `restart/audit/totality/p3/hardening/V1/CHALLENGE-CONTEXT.md:68`-`70`. | The 12 lock-addendum deltas have no per-delta LOC budget, risk class, wave alignment, or same-wave consumer/gate. 3C instead labels the whole change "documentation-only" and says propagation touches `LOCKS.md` only, even though the clauses drive W1-W11 gates and require 3A/3B/3E/3F cross-references. | Add a CH4-cost column or companion table for every `D-L*` clause: doc LOC, affected waves, consuming gate or same-wave consumer, propagation count across surfaces, and risk. Retain proposal-only status, but do not hide multi-lock operational work behind a single doc-only addendum. |
| CH4-COST-04 | High | 3E with 3B/3D | `restart/audit/totality/p3/3E-grammar-generalisation.md:65`, `restart/audit/totality/p3/3E-grammar-generalisation.md:124`, and `restart/audit/totality/p3/3E-grammar-generalisation.md:142`. | `restart/skinny/tranches/sk-v15/SPEC.md:179`-`180`; `restart/audit/totality/p3/3B-master-plan-reconciliation.md:119`-`120`; `restart/audit/totality/p3/3D-skinny-fold.md:74`; `restart/audit/totality/p1/1D-skinny-lessons.md:175`-`176`. | CSS typed provider and retime are costed inconsistently, and 3E leaves the broad CSSOM rewrite risk as an open question. 3B allocates W5/W6 as 180-360 manual plus generated output and 160-340 retime; 3D/T-P1 carry W5 as 300-900 plus W6 160-340; 3E carries 450-700 provider plus 120-220 retime. None says what CSSOM/value scope is explicitly out of bounds. | Define the W5 deliverable as a scoped typed provider, not full CSSOM, with exact owner paths, generated-output limit, comparator/gate consumer, and fail action. If CSSOM parity is required for close, mark W5/W6 as not cap-fit and route intrinsic block or wave-graph amendment; do not let "CSSOM/value parity" become unbounded hidden work. |
| CH4-COST-05 | Medium | 3F | `restart/audit/totality/p3/3F-migration-handoff.md:120` and `restart/audit/totality/p3/3F-migration-handoff.md:96`-`102`. | CRUD caps at `restart/skinny/tranches/sk-v15/SPEC.md:159`-`163`; 3F open question at `restart/audit/totality/p3/3F-migration-handoff.md:120`. | 3F identifies possible HANDOFF CRUD cap overflow but leaves the minimal patch shape as an open CH4 question. This is less severe because the deltas have LOC/risk/propagation and receiver/gate columns, but the repair route is not executable yet. | Pre-split CRUD-4 into a minimal current-state/next-directive patch and optional historical cleanup. The minimal patch must preserve the implementation block and name its exact lines, doc LOC, and G-Omega gate. |

## Delta Field Coverage

3A mostly satisfies the CH4 field contract: each architecture delta carries a LOC
band, risk, propagation count, and wave alignment in the proposed table or
consequences. The high-risk rows remain bounded only if 3B repairs the W4 and
W7-W9 budget conflicts.

3B has the right receiver-table shape for W0-W11, including LOC, generated
status, risk, MASTER alignment, close consumer, and evidence. It fails cost
realism for W4 and W7-W9 against the T-P1/T-P2 cost carriers.

3C fails the per-delta CH4 field contract. The mechanical diff applies and
preserves 16 locks, but the dense addendum needs a cost/propagation/wave/gate
matrix before V1 can be accepted by this lens.

3D and 3E contain most required cost fields, but they expose the same budget
contradictions rather than resolving them. 3E must bound CSS provider/CSSOM and
Pattern H projection scope instead of leaving those as open cost questions.

3F is acceptable on field coverage except for its unresolved CRUD-cap split
question.

## Repair Directives

1. Fold a V2 cost reconciliation into 3B first. It is the MASTER receiver map and
   must be the single source for cap-real wave rows.
2. Update 3D and 3E to cite the reconciled 3B bands, not parallel or conflicting
   bands. Where a row cannot fit the SK-V15 redress cap, say `intrinsic-block`,
   `revert/REDRESS`, or `G-Omega wave-graph amendment`; do not imply W12.
3. Add a 3C per-clause cost matrix for `D-L01` through `D-L16` folded clauses,
   with doc LOC, risk, wave alignment, consumer/gate, and propagation count.
4. For CSS, state explicitly that W5 is not a broad CSSOM rewrite unless the
   wave graph is amended. CSSOM/value parity may be a comparator precondition,
   not hidden implementation scope.
5. For Pattern H, distinguish provenance gate, generator/check proof, runtime
   projection, destructive deletion, and close transcript. Each needs its own
   cap-fit/fail-action statement.

## Residual Risk

Even after repair, W4, W5/W6, and W7-W9 remain the high-risk rows. They touch
runtime generation, CSS typed output, same-workload measurement, Decision Engine
activation, and lowerer output. The only acceptable remaining risk posture is
explicit intrinsic-block or gate-consumed rejection when a row cannot fit its
wave cap. No W12 overflow, challenge-time implementation overflow, or broad
doc-only close should be allowed.
